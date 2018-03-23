000100000921     H DECEDIT('0,') DATEDIT(*YMD.)
000200000921
000300000921      *------------------------------------------------------------------------*
000400000921      *                                                                        *
000500000921      *          CREAZIONE AUTOMATICA ANAGRAFICA CLIENTI RITIRI                *
000600000921      *                                                                        *
000700000921      *------------------------------------------------------------------------*
000800000921
000900010709     F**!!!TABEL00F  IF   E           K DISK
001000001009     FAZORG01L  IF   E           K DISK
001100000921     FCNACO00F  IF   E           K DISK
001200000921     FCNIND00F  IF   E           K DISK
001300010409     F*CNCLP00F  IF   E           K DISK
001400010409     fsisdc01l  if   e           k disk
001500010709     F**!!!FNSPE01L  IF   E           K DISK
001600001009     FFNACR03L  IF   E           K DISK    RENAME(FNACR000:FNACR3)
001700000921     FFNACR01L  UF A E           K DISK
001800010709     F**!!!TNVRORM1P O    E             PRINTER OFLIND(*IN55)
001900010709     F**!!!TNVRORM1Q O    E             PRINTER OFLIND(*IN65)
002000010709     F**!!!TNVRORM1R O    E             PRINTER OFLIND(*IN75)
002100001219
002200010709     D**!!! W3D             S             10  0 DIM(3000)
002300001219     D L6              S              3  0 DIM(30)                              P.O. Gestiti
002400001219     D L1              S              3  0 DIM(30)                              P.O. Gestiti
002500001009     D kkccc           s                   like(acrccc)
002600001009     D kkpob           s                   like(acrpob)
002700001220     D OG148         E DS
002800010709     D**!!! DS3D          E DS
002900001009     D KPJBA         E DS
003000020503     D* rem                    1      3
003100020503     D* remfil                 4      6
003200000921
003300000921     D CNCR80        E DS
003400001009     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003500001009     D  LIN                    1     90  0
003600001009     D                                     DIM(30)
003700000921     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800000921     D  TCU                  398    697
003900000921     D                                     DIM(50)
004000000921     D  KCU                  698    847P 0
004100000921     D                                     DIM(50)
004200000921     D                                     PACKEVEN
004300010205
004400010205     D Parm01          DS
004500010205     D  pocheparte                    3  0
004600000921
004700000921     D TCUDS           DS
004800000921     D  F1                     1      1
004900000921     D  F3                     3      3
005000000921     D  F2                     2      2
005100000921     D  F4                     4      4
005200000921     D  F56                    5      6
005300000921
005400000921      *------------------------------------------------------------------------*
005500000921      *                                                                        *
005600000921      *          OPERAZIONI INIZIALI                                           *
005700000921      *                                                                        *
005800000921      *------------------------------------------------------------------------*
005900000921
006000001009     C     *ENTRY        PLIST
006100001009     C                   PARM                    KPJBA
006200010205      *
006300010205     C                   movel     kpjbu         parm01
006400001009
006500000921      *          Definizioni di Accesso al Data Base
006600000921
006700001009     C     kfnacr3       klist
006800001009     C                   kfld                    kkpob
006900001009     C                   kfld                    kkccc
007000001009
007100010709     C**!!!ktabel        klist
007200010709     C**!!!              kfld                    codut             1 0
007300010709     C**!!!              kfld                    kcod              2
007400000921
007500010709     C**!!!              move      '3D'          kcod
007600000921
007700000921     C     kcnaco        KLIST
007800000921     C                   KFLD                    codut
007900000921     C                   KFLD                    KCI
008000000921     C                   KFLD                    kksc              7 0
008100000921
008200000922     C     kcnaco2       KLIST
008300000922     C                   KFLD                    codut
008400000922     C                   KFLD                    KCI
008500010709
008600010709     C                   z-add     1             codut
008700000922
008800010709     C**!!!kfnspe        KLIST
008900010709     C**!!!              KFLD                    kkkfls            1
009000010709     C**!!!              KFLD                    kkkcli            7 0
009100010709     C**!!!              KFLD                    kkkcod            3
009200010709
009300010709     C**!!!              eval      kkkfls = 'L'
009400010409
009500010409     C     ksisdc        KLIST
009600010409     C                   KFLD                    kksc
009700010409     C                   KFLD                    kann              4 0
009800000921
009900000921      *          Parametri Utente
010000000921
010100000921     C                   CALL      'X§PARUT'
010200000921     C                   PARM                    UT§DSE
010300000921     C                   MOVEL     RAGUT         RSUT             20
010400000921     C                   MOVEL     REC80         CNCR80
010500001219
010600001219      * IMPOSTO IL P.O. DI GESTIONE
010700020503     C*                  if        rem = 'REM'  AND  remfil > *ZEROS
010800020503     C                   if        simtpp = '2' or simtpp = *blanks
010900010205     C************       movel     REMFIL        pocheparte
011000001219     C                   eval      *in06 = *on
011100001219     C                   else
011200010205     C************       movel     SIMFEL        pocheparte
011300001219     C                   endif
011400001220
011500001220      *          Controllo se il P.O. che parte è flaggato sull'Organigramma
011600001220
011700001220     C     pocheparte    chain     azorg01l
011800001220     C                   if        %found
011900001220     C                   movel     orgde8        og148
012000001220     C                   if        §ogorm <> 'S'
012100001220     C                   goto      fine
012200001220     C                   endif
012300001220     C                   endif
012400001009
012500001219      *          Lira 6
012600001009
012700001009     C                   CLEAR                   DSUL06
012800001219     C                   MOVE      '£6'          D06COD
012900001219     C                   MOVEL     pocheparte    D06KEY
013000001009     C                   MOVEL     DSUL06        KPJBU
013100001009     C                   CALL      'TRUL06R'
013200001009     C                   PARM                    KPJBA
013300001009     C                   MOVEL     KPJBU         DSUL06
013400001219     C                   MOVEA     LIN           L6
013500001219
013600001219      *          Lira 1
013700001219
013800001219     C                   CLEAR                   DSUL06
013900001219     C                   MOVE      '£1'          D06COD
014000001219     C                   MOVEL     pocheparte    D06KEY
014100001219     C                   MOVEL     DSUL06        KPJBU
014200001219     C                   CALL      'TRUL06R'
014300001219     C                   PARM                    KPJBA
014400001219     C                   MOVEL     KPJBU         DSUL06
014500001219     C                   MOVEA     LIN           L1
014600000921
014700000921      *          Ricerca Capoconti
014800000921
014900000921     C                   DO        50            X                 2 0
015000000921     C                   MOVE      TCU(X)        TCUDS
015100000921     C     F56           CABNE     'CG'          END1
015200000921     C     F4            COMP      '1'                                    21
015300000921     C     F4            COMP      '2'                                    22
015400000921     C     F4            COMP      '3'                                    23
015500000921     C     F4            COMP      '6'                                    27
015600000921      ** 1 CLIENTI   21
015700000921      ** 2 FORNITORI 22
015800000921      ** 3 AGENTI    23
015900000921     C     F3            COMP      '0'                                242425
016000000921     C     F3            COMP      'I'                                    26
016100000921      ** 0 ITALIA   25
016200000921      ** 1 ESTERO   24
016300000921      ** I CAPO CONTO IVA
016400000921     C   21
016500000921     CAN 24              Z-ADD     KCU(X)        KCE               4 0
016600000921     C   21
016700000921     CAN 25              Z-ADD     KCU(X)        KCI               4 0
016800000921     C   22
016900000921     CAN 24              Z-ADD     KCU(X)        KFE               4 0
017000000921     C   22
017100000921     CAN 25              Z-ADD     KCU(X)        KFI               4 0
017200000921     C   23
017300000921     CAN 24              Z-ADD     KCU(X)        KAE               4 0
017400000921     C   23
017500000921     CAN 25              Z-ADD     KCU(X)        KAI               4 0
017600000921     C   26              Z-ADD     KCU(X)        KIVA              4 0
017700000921     C   27              Z-ADD     KCU(X)        KBNA              4 0
017800000921     C     END1          TAG
017900000921     C                   END
018000000921     C                   SETOFF                                       212223
018100000921     C                   SETOFF                                       242526
018200000921     C                   SETOFF                                       27
018300000921
018400000922     C                   seton                                        55        overflow
018500001009     C                   seton                                        65        overflow
018600001009     C                   seton                                        75        overflow
018700000926     C                   setoff                                       81
018800001009     C                   setoff                                       82
018900001009     C                   setoff                                       83
019000010709     C**!!!              z-add     0             wc                4 0          contatore tab 3d
019100000922
019200000921      *------------------------------------------------------------------------*
019300000921      *                                                                        *
019400000921      *          ROUTINE PRINCIPALE                                            *
019500000921      *                                                                        *
019600000921      *------------------------------------------------------------------------*
019700000921
019800000921      *          =====================================
019900000921      *          FASE 1 - Ripresa da Anagrafica Luoghi
020000000921      *          =====================================
020100000921
020200010709     C**!!!              exsr      DaFNSPE
020300000921
020400000921      *          ==============================
020500000921      *          FASE 2 - Ripresa da Tabella 3D
020600000921      *          ==============================
020700000921
020800010709     C**!!!              exsr      DaTABELLA3D
020900000921
021000000921      *          ======================================
021100000921      *          FASE 3 - Ripresa da Anagrafica Clienti
021200000921      *          ======================================
021300000921
021400000921     C                   exsr      DaCNACO
021500000921
021600000921      *          Fine Programma
021700000921      *          --------------
021800000921
021900010709     C**!!!   81              write     finest
022000010709     C**!!!   82              write     finestq
022100010709     C**!!!   83              write     finestr
022200001220
022300001220     C     fine          tag
022400000921     C                   eval      *inlr = *on
022500000921
022600000921      *------------------------------------------------------------------------*
022700000921      *                                                                        *
022800000921      *          SUBROUTINES                                                   *
022900000921      *                                                                        *
023000000921      *------------------------------------------------------------------------*
023100000921
023200000921      *------------------------------------------------------------------------*
023300000921      *          Subroutine Ripresa da Anagrafica Luoghi                       *
023400000921      *------------------------------------------------------------------------*
023500000921
023600010709     C**!!!DaFNSPE       begsr
023700000921
023800000921      *          Lettura FNSPE (solo Luoghi)
023900000921
024000010709     C**!!!'L'           setll     fnspe01l
024100010709     C**!!!              do        *hival
024200010709     C**!!!'L'           reade     fnspe01l
024300000921
024400010709     C**!!!              if        %eof
024500010709     C**!!!              leave
024600010709     C**!!!              endif
024700001219
024800001219      *          Converto solo la mia £6
024900001219
025000010709     C**!!!              movel     specli        wwpo              3 0
025100010709     C**!!!wwpo          lookup    l6                                     40
025200010709     C**!!!*in40         ifeq      *off
025300010709     C**!!!              iter
025400010709     C**!!!              endif
025500000921
025600000921      *          Considero solo i luoghi dal 101 al 105 non Annullati
025700000921
025800010709     C**!!!specod        ifge      '101'
025900010709     C**!!!specod        andle     '105'
026000010709     C**!!!speflg        andeq     ' '
026100000921
026200000921      *          Scrivo Anagrafica Clienti Ritiri
026300000921
026400010709     C**!!!              clear                   fnacr000
026500000921
026600010709     C**!!!              eval      ACRTAC = 'R'
026700010709     C**!!!              movel     specli        ACRCRO
026800010709     C**!!!              move      specod        ACRCRO
026900010709     C**!!!              eval      ACRRSR = sperag
027000010709     C**!!!              eval      ACRINR = speind
027100010709     C**!!!              eval      ACRCAR = specap
027200010709     C**!!!              eval      ACRLOR = speloc
027300010709     C**!!!              eval      ACRPRR = spepro
027400010709     C**!!!              eval      ACRNAR = spenaz
027500010709     C**!!!**!!!         eval      ACRKSC = specli
027600010709     C**!!!              eval      ACRPER = speper
027700010709     C**!!!              eval      ACRTEL = spetel
027800010709     C**!!!              movel     specli        ACRPOA
027900010709     C**!!!              eval      ACRTCR = 'R'
028000000921
028100010709     C**!!!              write     fnacr000
028200000921      *                  ------------------
028300000921
028400010709     C**!!!              endif
028500000921
028600010709     C**!!!              enddo
028700000921
028800010709     C**!!!              endsr
028900000921
029000000921      *------------------------------------------------------------------------*
029100000921      *          Subroutine Ripresa da Tabella 3D                              *
029200000921      *------------------------------------------------------------------------*
029300000921
029400010709     C**!!!DaTABELLA3D   begsr
029500000921
029600000921      *          Lettura TABEL (solo codice 3D)
029700000921
029800010709     C**!!!ktabel        setll     tabel00f
029900010709     C**!!!              do        *hival
030000010709     C**!!!ktabel        reade     tabel00f
030100000921
030200010709     C**!!!              if        %eof
030300010709     C**!!!              leave
030400010709     C**!!!              endif
030500000921
030600000921      *          Considero solo i Records non Annullati
030700000921
030800010709     C**!!!tblflg        ifeq      ' '
030900010709     C**!!!              movel     tbluni        ds3d
031000001219
031100001219      *          Converto solo la mia £6
031200001219
031300010709     C**!!!              movel     §3dpor        wwpo              3 0
031400010709     C**!!!wwpo          lookup    L6                                     40
031500010709     C**!!!*in40         ifeq      *off
031600010709     C**!!!              iter
031700010709     C**!!!              endif
031800001009
031900001009      *          Se Cod. Non presente in £1 Stampo e Riciclo
032000001009
032100010709     C**!!!wwpo          lookup    L1                                     40
032200010709     C**!!!*in40         ifeq      *off
032300010709     C**!!!              movel     tblkey        qcallc
032400010709     C**!!!              z-add     §3dksc        kksc
032500010709     C**!!!kcnaco        chain     cnaco00f                           56
032600010709     C**!!!   56              clear                   cnaco000
032700010709     C**!!!              movel     acorag        qragso
032800010709     C**!!!              movel     wwpo          qpoapp
032900010709     C**!!!wwpo          chain     azorg01l
033000010709     C**!!!              if        %found
033100010709     C**!!!              movel     orgdes        qdesp1
033200010709     C**!!!              else
033300010709     C**!!!              clear                   qdesp1
033400010709     C**!!!              clear                   orgfel
033500010709     C**!!!              endif
033600010709     C**!!!              movel     orgfel        qpoela
033700010709     C**!!!orgfel        chain     azorg01l
033800010709     C**!!!              if        %found
033900010709     C**!!!              movel     orgdes        qdesp2
034000010709     C**!!!              else
034100010709     C**!!!              clear                   qdesp2
034200010709     C**!!!              endif
034300010709     C**!!!§3drfs        ifeq      ' '
034400010709     C**!!!§3dgr1        andeq     ' '
034500010709     C**!!!§3dgr2        andeq     ' '
034600010709     C**!!!§3dgr3        andeq     ' '
034700010709     C**!!!§3dgr4        andeq     ' '
034800010709     C**!!!§3dgr5        andeq     ' '
034900010709     C**!!!              eval      qflgcc = 'S'
035000010709     C**!!!              else
035100010709     C**!!!              eval      qflgcc = ' '
035200010709     C**!!!              endif
035300010709     C**!!!              seton                                        82
035400010709     C**!!!   65              write     testaq
035500010709     C**!!!   65              setoff                                       65
035600010709     C**!!!              write     dettq
035700010709     C**!!!              iter
035800010709     C**!!!              endif
035900001009
036000001009      *          Se Call Center Duplicato Stampo e Riciclo
036100001009
036200010709     C**!!!              movel     tblkey        kkccc
036300010709     C**!!!              movel     §3dksc        wwpo              3 0
036400010709     C**!!!wwpo          chain     azorg01l
036500010709     C**!!!              if        %found
036600010709     C**!!!              move      orgfel        kkpob
036700010709     C**!!!              else
036800010709     C**!!!              clear                   kkpob
036900010709     C**!!!§3drfs        ifeq      ' '
037000010709     C**!!!§3dgr1        andeq     ' '
037100010709     C**!!!§3dgr2        andeq     ' '
037200010709     C**!!!§3dgr3        andeq     ' '
037300010709     C**!!!§3dgr4        andeq     ' '
037400010709     C**!!!§3dgr5        andeq     ' '
037500010709     C**!!!kfnacr3       chain     fnacr03l
037600010709     C**!!!              if        %found
037700010709     C**!!!              movel     tblkey        qcallc
037800010709     C**!!!              z-add     §3dksc        kksc
037900010709     C**!!!kcnaco        chain     cnaco00f                           56
038000010709     C**!!!   56              clear                   cnaco000
038100010709     C**!!!              movel     acorag        qragso
038200010709     C**!!!              movel     kkpob         qpoela
038300010709     C**!!!kkpob         chain     azorg01l
038400010709     C**!!!              if        %found
038500010709     C**!!!              movel     orgdes        qdesp2
038600010709     C**!!!              else
038700010709     C**!!!              clear                   qdesp2
038800010709     C**!!!              endif
038900010709     C**!!!              seton                                        83
039000010709     C**!!!   75              write     testar
039100010709     C**!!!   75              setoff                                       75
039200010709     C**!!!              write     dettr
039300010709     C**!!!              iter
039400010709     C**!!!              endif
039500010709     C**!!!              endif
039600010709     C**!!!              endif
039700000921
039800000921      *          Verifico se devo Aggiornare o Scrivere
039900000921
040000010709     C**!!!              movel     §3dksc        Wcliente         10 0
040100010709     C**!!!§3dlrt        ifeq      *blanks
040200010709     C**!!!              move      '000'         Wcliente
040300010709     C**!!!              else
040400010709     C**!!!              move      §3dlrt        Wcliente
040500010709     C**!!!              endif
040600010709     C**!!!              z-add     899           w3num             3 0
040700000926
040800000926      *          Controllo duplicati
040900000926
041000010709 +-> C**!!!su            tag
041100010709 |   C**!!!Wcliente      chain     fnacr01l                           58
041200010709 |   C**!!!  N58Wcliente      lookup    w3d                                    68
041300010709 |   C**!!!  N58*in68         ifeq      *on
041400010709 |   C**!!!              add       1             w3num
041500010709 |   C**!!!              move      w3num         Wcliente
041600010709 +-- C**!!!              goto      su
041700010709     C**!!!              endif
041800010709     C**!!!              add       1             wc
041900010709     C**!!!              z-add     Wcliente      w3d(wc)
042000000921
042100010709     C**!!!*in58         ifeq      *on
042200000921
042300000921      *          SCRITTURA
042400000921
042500000921      *          Recupero i dati da TABEL + CNACO + CNIND
042600000921
042700010709     C**!!!              z-add     §3dksc        kksc
042800010709     C**!!!kcnaco        chain     cnaco00f                           56
042900010709     C**!!!   56              clear                   cnaco000
043000010709     C**!!!kcnaco        chain     cnind00f                           56
043100010709     C**!!!   56              clear                   cnind000
043200000921
043300000921      *          Scrivo Anagrafica Clienti Ritiri
043400000921
043500010709     C**!!!              clear                   fnacr000
043600000921
043700010709     C**!!!              eval      ACRTAC = 'R'
043800010709     C**!!!              movel     §3dksc        ACRCRO
043900010709     C**!!!§3dlrt        ifeq      *blanks
044000010709     C**!!!              move      '000'         ACRCRO
044100010709     C**!!!              eval      ACRRSR = acorag
044200010709     C**!!!              eval      ACRINR = indvia
044300010709     C**!!!              eval      ACRCAR = indcae
044400010709     C**!!!              eval      ACRLOR = indcit
044500010709     C**!!!              eval      ACRPRR = indprv
044600010709     C**!!!              eval      ACRNAR = indsta
044700010709     C**!!!              eval      ACRTEL = indtel
044800010709     C**!!!              else
044900010709     C**!!!              move      §3dlrt        ACRCRO
045000010709     C**!!!              move      acoksc        kkkcli
045100010709     C**!!!              move      §3dlrt        kkkcod
045200010709     C**!!!kfnspe        chain     fnspe01l
045300010709     C**!!!              if        %found
045400010709     C**!!!              eval      ACRRSR = sperag
045500010709     C**!!!              eval      ACRINR = speind
045600010709     C**!!!              eval      ACRCAR = specap
045700010709     C**!!!              eval      ACRLOR = speloc
045800010709     C**!!!              eval      ACRPRR = spepro
045900010709     C**!!!              eval      ACRNAR = spenaz
046000010709     C**!!!              eval      ACRPER = speper
046100010709     C**!!!              eval      ACRTEL = spetel
046200010709     C**!!!              else
046300010709     C**!!!              eval      ACRRSR = acorag
046400010709     C**!!!              eval      ACRINR = indvia
046500010709     C**!!!              eval      ACRCAR = indcae
046600010709     C**!!!              eval      ACRLOR = indcit
046700010709     C**!!!              eval      ACRPRR = indprv
046800010709     C**!!!              eval      ACRNAR = indsta
046900010709     C**!!!              eval      ACRTEL = indtel
047000010709     C**!!!              endif
047100010709     C**!!!              endif
047200010709     C**!!!**!!!         eval      ACRKSC = acoksc
047300010709     C**!!!              movel     acoksc        ACRPOA
047400010709     C**!!!§3drfs        ifeq      ' '
047500010709     C**!!!§3dgr1        andeq     ' '
047600010709     C**!!!§3dgr2        andeq     ' '
047700010709     C**!!!§3dgr3        andeq     ' '
047800010709     C**!!!§3dgr4        andeq     ' '
047900010709     C**!!!§3dgr5        andeq     ' '
048000010709     C**!!!              eval      ACRTCR = 'R'
048100010709     C**!!!              eval      ACRFCC = 'S'
048200010709     C**!!!              movel     tblkey        ACRCCC
048300010709     C**!!!              movel     kkpob         ACRPOB
048400010709     C**!!!              else
048500010709     C**!!!              eval      ACRTCR = 'F'
048600010709     C**!!!              endif
048700010709     C**!!!w3num         ifne      899
048800010709     C**!!!              seton                                        81
048900000926
049000000926      *          Stampa duplicati
049100000926
049200010709     C**!!!              move      ACRCRO        WCRO
049300010709     C**!!!              move      wcliente      ACRCRO
049400010709     C**!!!   55              write     testa
049500010709     C**!!!   55              setoff                                       55
049600010709     C**!!!              write     dett
049700010709     C**!!!              endif
049800000921
049900010709     C**!!!              write     fnacr000
050000000921      *                  ------------------
050100000921
050200010709     C**!!!              else
050300000921
050400001009      *          AGGIORNAMENTO (solo dati call center / fisso)
050500000921
050600010709     C**!!!§3drfs        ifeq      ' '
050700010709     C**!!!§3dgr1        andeq     ' '
050800010709     C**!!!§3dgr2        andeq     ' '
050900010709     C**!!!§3dgr3        andeq     ' '
051000010709     C**!!!§3dgr4        andeq     ' '
051100010709     C**!!!§3dgr5        andeq     ' '
051200010709     C**!!!              eval      ACRTCR = 'R'
051300010709     C**!!!              eval      ACRFCC = 'S'
051400010709     C**!!!              movel     tblkey        ACRCCC
051500010709     C**!!!              movel     kkpob         ACRPOB
051600010709     C**!!!              else
051700010709     C**!!!              eval      ACRTCR = 'F'
051800010709     C**!!!              endif
051900000921
052000010709     C**!!!              update    fnacr000
052100000921      *                  ------------------
052200000921
052300010709     C**!!!              endif
052400000921
052500010709     C**!!!              endif
052600000921
052700010709     C**!!!              enddo
052800000921
052900010709     C**!!!              endsr
053000000921
053100000921      *------------------------------------------------------------------------*
053200000921      *          Subroutine Ripresa da Anagrafica Clienti                      *
053300000921      *------------------------------------------------------------------------*
053400000921
053500000921     C     DaCNACO       begsr
053600000922
053700000922      *          Lettura CNACO
053800000922
053900000922     C     kcnaco2       setll     cnaco00f
054000000922     C                   do        *hival
054100000922     C     kcnaco2       reade     cnaco00f
054200000922
054300000922     C                   if        %eof
054400000922     C                   leave
054500000922     C                   endif
054600001219
054700001219      *          Converto solo la mia £6
054800001219
054900001219     C                   movel     acoksc        wwpo              3 0
055000001219     C     wwpo          lookup    l6                                     40
055100001219     C     *in40         ifeq      *off
055200001219     C                   iter
055300001219     C                   endif
055400000922
055500000922      *          Escludo Records Annullati e Bloccati
055600000922
055700000922     C     acoflg        ifeq      ' '
055800000922     C     acoabl        andeq     ' '
055900000922
056000000926      *          Escludo 8888 e 9999
056100000926
056200000922     C                   move      acoksc        walfa4            4
056300000922     C     walfa4        ifne      '8888'
056400000922     C     walfa4        andne     '9999'
056500000926
056600000926      *          Escludo Records con data ult. fattura inferiore a un anno
056700000926      *          (a meno che non siano stati variati da meno di 3 mesi)
056800000926
056900000926     C                   z-add     acoksc        kksc
057000010409     c                   z-add     2000          kann
057100010409     c                   clear                   clpdus
057200010409     c     ksisdc        setll     sisdc01l
057300010409     c                   do        *hival
057400010411     c     kksc          reade     sisdc01l
057500010409     c                   if        %eof(sisdc01l)
057600010409     c                   leave
057700010409     c                   endif
057800010411     c                   if        sdcann < kann
057900010411     c                   leave
058000010411     c                   endif
058100010409     c                   move      *date         clpdus
058200010409     c                   leave
058300010409     c                   enddo
058400000922
058500010409     C     clpdus        ifeq      0
058600000926     C     acoduv        andne     0
058700000926     C                   z-add     acoduv        wdatacomodo       8 0
058800000926     C     acoduv        ifgt      600000
058900000926     C                   movel     '19'          wdatacomodo
059000000926     C                   else
059100000926     C                   movel     '20'          wdatacomodo
059200000926     C                   endif
059300000926     C     1             do        3                                            3 mesi
059400000926     C                   movel     wdatacomodo   wseinum           6 0
059500000926     C                   movel     wdatacomodo   wquattronum       4 0
059600000926     C                   move      wseinum       wduenum           2 0
059700000926     C                   add       1             wduenum
059800000926     C     wduenum       ifeq      13
059900000926     C                   z-add     1             wduenum
060000000926     C                   add       1             wquattronum
060100000926     C                   endif
060200000926     C                   movel     wquattronum   wseinum
060300000926     C                   move      wduenum       wseinum
060400000926     C                   movel     wseinum       wdatacomodo
060500000926     C                   enddo
060600000926     C     wdatacomodo   ifge      *date
060700010409     C                   move      *date         clpdus            6 0
060800000926     C                   endif
060900010409     C                   endif
061000000926
061100010409     C                   z-add     clpdus        w8dataus          8 0
061200010409     C     clpdus        ifeq      0
061300000926     C     clpdus        orgt      600000
061400000926     C                   movel     '19'          w8dataus
061500000926     C                   else
061600000926     C                   movel     '20'          w8dataus
061700000926     C                   endif
061800000926     C     w8dataus      add       10000         w8dataus                       + 1 anno
061900000922
062000000926     C     w8dataus      ifge      *date
062100000926
062200000926      *          Escludo Records già presenti (scrivo solo)
062300000926      *          ... solo primi 7 bytes
062400000926
062500010709     C                   movel     acoksc        Wcliente         10 0
062600000926     C                   move      '000'         Wcliente
062700000926     C     Wcliente      setll     fnacr01l
062800000926     C                   read(N)   fnacr01l
062900000926     C                   if        %eof
063000000926     C                   else
063100000926     C                   movel     ACRCRO        w7numerico        7 0
063200000926     C     w7numerico    ifeq      acoksc
063300000926     C                   iter
063400000926     C                   endif
063500000926     C                   endif
063600000926
063700000926      *          OK
063800000926      *          Scrivo Anagrafica Clienti Ritiri
063900000922
064000000926     C     kcnaco        chain     cnind00f                           56
064100000926     C   56              clear                   cnind000
064200000926
064300000926     C                   clear                   fnacr000
064400000926
064500000926     C                   eval      ACRTAC = 'R'
064600000926     C                   movel     acoksc        ACRCRO
064700000926     C                   move      '000'         ACRCRO
064800000926     C                   eval      ACRRSR = acorag
064900000926     C                   eval      ACRINR = indvia
065000000926     C                   eval      ACRCAR = indcae
065100000926     C                   eval      ACRLOR = indcit
065200000926     C                   eval      ACRPRR = indprv
065300000926     C                   eval      ACRNAR = indsta
065400000926     C                   eval      ACRTEL = indtel
065500000926     C                   movel     acoksc        ACRPOA
065600010215     C                   eval      ACRTCR = 'R'
065700000926
065800000926     C                   write     fnacr000
065900000926      *                  ------------------
066000000922
066100000922     C                   endif
066200000926
066300000926     C                   endif
066400000922
066500000922     C                   endif
066600000922
066700000922     C                   enddo
066800000922
066900000921     C                   endsr
