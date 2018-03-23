000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950104      * FNLSA3R *----------------------------------------------------*
000300950104      *   INVIO FILE DI TRASMISSIONE:
000400950112      *   -  IN SEDE   : BOLLE / FOGLI VIAGGIO /STATISTICHE /LEGAMI BOL
000500990903      *                  BOLLE ANNULLATE / VOLUME CML / PESO CML
000600950113      *
000700950114      *   -  LANCIO LA STATISTICA ATTIVITA' COMMERCIALE
000800900515      *--------------------------------------------------------------*
000900910912     FAZORG01L  IF   E           K DISK
001000920305     FTABEL00F  IF   E           K DISK
001100050617     FTNTBE01L  IF   E           K DISK
001200950104     F*?  T R A S M I S S I O N E    A    S E D E  ?
001300991026     FFIBLPA0T  O  A E             DISK    USROPN
001400060220     FFiar400T  O  A E             DISK    USROPN
001500060220     F                                     RENAME(Fiar4000:Fiar40TT)
001600991026     FFIAR600T  O  A E             DISK    USROPN
001700991026     F                                     RENAME(FIAR6000:FIAR60TT)
001800991026     FFIAR700T  O  A E             DISK    USROPN
001900991026     F                                     RENAME(FIAR7000:FIAR70TT)
002000000905     FTITAH00T  O  A E             DISK    USROPN
002100030110     fFiar500t  o  a e             Disk    Usropn
002200030110     f                                     Rename(Fiar5000:Fiar50tt)
002300950104     F*?  B O L L E    D I    A P P O G G I O  ?
002400070510     FFNBLP47l  UF   E           K DISK
002500930716     F*
002600970214     FFNBLP01L  IF   E           K DISK
002700970214     F                                     RENAME(FNBLP000:FNBLP001)
002800090304     FFiART01L  IF   E           K DISK
002900051013     FFiarS01L  IF   E           K DISK
003000991026     FFIAR601L  IF   E           K DISK
003100991026     FFIAR701L  IF   E           K DISK
003200051116     FFiar901L  IF   E           K DISK
003300060220     FFiar401L  IF   E           K DISK
003400030110     fFiar501l  uf   e           k Disk
003500950112     D*
003600930929     D FE2             S              3  0 DIM(99)
003700041122     D*NBL             S              3  0 DIM(99)
003800980116     D*                   SPP        99  3 0             SPUNTA NEW
003900950113     D ARV             S              3  0 DIM(99)
004000990903     D ARP             S              3  0 DIM(99)
004100991026     D C7K             S              1    DIM(99)
004200050616     D C7KK            S              1    DIM(99)
004300050617     D CKB4            S              8    DIM(99)
004400021010     D CMD1            S             42    DIM(1) CTDATA PERRCD(1)
004500021010     D CMD2            S             42    DIM(1) CTDATA PERRCD(1)
004600021010     D CMD3            S             42    DIM(1) CTDATA PERRCD(1)
004700021010     D CMD4            S             42    DIM(1) CTDATA PERRCD(1)
004800021010     D C17             S              1    DIM(42) CTDATA PERRCD(42)
004900021010     D CM33            S             42    DIM(1) CTDATA PERRCD(1)
005000021010     D CM34            S             42    DIM(1) CTDATA PERRCD(1)
005100021010     D C56             S              1    DIM(42) CTDATA PERRCD(42)
005200021010     D C57             S              1    DIM(42) CTDATA PERRCD(42)
005300021009      * Comandi x FNARBV0T e FNARBP0T
005400021009     d Cvp             s             42    Dim(2) Ctdata Perrcd(1)
005500950104     D*
005600021009
005700021009     d                 ds
005800021009     d  Emme1                  1      1    Inz('M')
005900021009     d  $Simfel                2      4  0
006000021009     d  alf3                   5      7
006100021009     d  Var10                  1     10
006200021009
006300950112     D***
006400950112     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
006500950112     D***
006600950112     D WLBDAT          DS                  INZ
006700950112     D  G02DAT                 1      8  0
006800950112     D  G02INV                 9     16  0
006900950112     D  G02ERR                17     17
007000950112     D  G02TGI                18     22  0
007100951207     D***
007200960129     D* PARAMETRI PER TRASMISS. FNFGV00T-FVD-FV4          - FNLSA4R -
007300951207     D***
007400951207     D PARAM2          DS
007500951207     D  PA2MBD                 1     10
007600951207     D  PA2FEL                11     13  0
007700960129     D* IL CAMPO PA2ALL SE VIENE RESTITUITO PIENO SIGNIFICA CHE NON
007800960129     D*   SONO AVVENUTE LE ALLOCAZIONI DI TUTTI I FILES
007900951207     D  PA2ALL                14     14
008000960129     D* TIPO LANCIO PROGRAMMA ("C" / "L" / " ")
008100951207     D  PA2TLA                15     15
008200960129     D* PA2FLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
008300960129     D  PA2FLG                16     16    INZ
008400960129     D* I CAMPI PA2LNP E PA2NFV DEVONO ESSERE PASSATI SOLO SE PGM
008500960129     D*   RICHIAMATO DALLA GESTIONE F.VIAGGIO
008600960129     D  PA2LNP                17     19  0 INZ
008700960129     D  PA2NFV                20     24  0 INZ
008800001107     D* PSSAGGIO DATI DA CHIUSURA FOGLI VIAGGIO
008900001107     D PARAM3          DS
009000001107     D  PA3NFV                 1      5
009100070423     D* pa3tfp deve contenere il terminal di partenza per poter elaborare e
009200070423     d* trasmettere le bolle per terminal di partenza
009300070423     d* se non passato un valore > '000' il programma elabora e trasmette
009400070423     d* TUTTE le bolle presenti nel file fnblp46f (come faceva prima)
009500070423     D  PA3tfp                 6      8
009600960129     D*
009700991026     D DS7K          E DS
009800950112     D DS3A          E DS
009900000911     D DS7H          E DS
010000920305     D KPJBA         E DS
010100900601     D DSCMZ         E DS
010200900517     D CNCR80        E DS
010300900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010400020927     D DSBS50        E DS                  EXTNAME(TIBS55DS)
010500021017
010600021017     d Tibs02Ds      e ds
010700021017      * Ds date statistiche
010800050301     d*dSdf          e ds
010900030110      * Ds Tabella AR5
011000030110     d dAr5          e ds
011100021009
011200021009     d $mbr            s              6  0
011300041122     d VAR8            s              8
011400070511     d command         S             80    INZ('DLYJOB DLY(7)')
011500070511     d lenght          S             15  5 INZ(45)
011600900601      *---------------------------------------------------------------*
011700900601      * INDICATORI USATI
011800900601      *
011900910913      * 31 = DI COMODO
012000970214      * 33 = DI COMODO
012100910912      * 40 = NON ALLOCO LINEA--> RIPROVO UNA SECONDA VOLTA
012200910912      * 42 = INVIO MESSAGGIO DI NON ALLOCATO MEMBRI (1 VOLTA SOLA)
012300970214      * 47/54 = ERRORI IN ALLOCAZIONE
012400900601      *--------------------------------------------------------------*
012500971209     C     *ENTRY        PLIST
012600000000     C                   PARM                    KPJBA
012700040505     C                   PARM                    TRASPESVOL        1
012800040505     C
012900001107     C                   MOVEL     KPJBU         PARAM3
013000040505     C* VERIFICO SE PASSATI 1 O 2 PARAMETRI
013100040505     C                   IF        %PARMS<2
013200040505     C                   CLEAR                   WSOLOTRASM        1
013300040505     C                   ELSE
013400040505     C                   EVAL      WSOLOTRASM=TRASPESVOL
013500040505     C                   ENDIF
013600070511     c*
013700070511     c                   if        wsolotrasm=' '
013800070511      * attende qualche secondo
013900070511     c                   call      'QCMDEXC'
014000070511     c                   parm                    command
014100070511     c                   parm                    lenght
014200070511     c                   endif
014300040505     C
014400900516      *
014500000000     C                   Z-ADD     1             CODUT
014600900517     C                   CALL      'X§PARUT'
014700900517     C                   PARM                    UT§DSE
014800900517     C                   MOVEL     REC80         CNCR80
014900041115     c*
015000021009     c                   movel     simfel        $mbr
015100021009     c                   eval      $simfel = simfel
015200040505     c*
015300040505     c                   if        knsif=*blanks
015400040505     c                   eval      knsif='*LIBL'
015500040505     c                   endif
015600040510     c                   if        wsolotrasm<>' '
015700040510     c     simfel        chain     azorg01l
015800040510     c                   if        %found(azorg01l)
015900040510     c                   movel     orgdit        emme1
016000040510     c                   endif
016100040510     c                   endif
016200021009
016300910912     C*----------------------------------------------------------------
016400000911     C* ACCESSO TABEL00F
016500920305     C     KTAB          KLIST
016600920305     C                   KFLD                    CODUT
016700950104     C                   KFLD                    COD
016800000911     C* ACCESSO TABEL00F
016900000911     C     KTABEL        KLIST
017000000911     C                   KFLD                    CODUT
017100000911     C                   KFLD                    COD$              2
017200000911     C                   KFLD                    KEY$              8
017300970214     C* ACCESSO FNBLP01L
017400970214     C     KBLP          KLIST
017500970214     C                   KFLD                    BLPAAS
017600970214     C                   KFLD                    BLPLNP
017700970214     C                   KFLD                    BLPNRS
017800970214     C                   KFLD                    BLPNSP
017900000906     C* ACCESSO A ESTENSIONE SEGNACOLLI PER SCRITTURA TITAH
018000000906     C     KSEGNA        KLIST
018100090304     C                   KFLD                    ARTFLS
018200090304     C                   KFLD                    ARTLNA
018300090304     C                   KFLD                    ARTNRS
018400090304     C                   KFLD                    ARTNSC
018500970214     C*
018600970214     C* DEFINIZIONE CAMPI
018700950104     C     *LIKE         DEFINE    TBLCOD        COD
018800950104     C     *LIKE         DEFINE    TBLKEY        KEY
018900971209     C     *LIKE         DEFINE    CMZLBD        SAVLBD
019000971209     C     *LIKE         DEFINE    CMZLBA        SAVLBA
019100971209     C     *LIKE         DEFINE    CMZLBD        WLBLBD
019200971209     C     *LIKE         DEFINE    CMZLBA        WLBLBA
019300971209     C     *LIKE         DEFINE    ORGFEL        SAVFEL
019400950112     C*----------------------------------------------------------------
019500971113     C*
019600920115     C                   Z-ADD     0             F                 3 0
019700930203     C                   Z-ADD     0             G                 3 0
019800950113     C                   Z-ADD     0             I                 3 0
019900990903     C                   Z-ADD     0             P                 3 0
020000930716     C                   Z-ADD     0             H                 3 0
020100970924     C                   Z-ADD     0             M                 3 0
020200920309     C*
020300950104     C* NUMERO TRASMISSIONE = DATA DEL GIORNO (AAAA/MM/GG)
020400950104     C                   TIME                    WTIME            14 0
020500950104     C                   MOVE      WTIME         WDATE             8 0
020600950104     C                   Z-ADD     WDATE         G02DAT
020700950104     C                   Z-ADD     *ZERO         G02INV
020800950104     C                   MOVEL     *BLANKS       G02ERR
020900950104     C                   CALL      'XSRDA8'
021000950104     C                   PARM                    WLBDAT
021100951122     C                   Z-ADD     G02INV        NUMTRA            8 0
021200930716     C*
021300910926     C* IMPOSTO ALCUNI VALORI FISSI X CREARE FILE TRASMISSIONE
021400910926     C                   EXSR      IMPO
021500971209     C*
021600971209     C     SIMFEL        CHAIN     AZORG01L                           31
021700971209     C  N31              MOVEL     ORGFEL        SAVFEL
021800971209     C   31              MOVEL     SIMFEL        SAVFEL
021900971111     C***********************
022000971111     C* T R A S M I S S I O N I   P E R   S E D E
022100041115     C***********************
022200041115     C                   MOVEL     046           CAETFE            3 0
022300971113     C                   MOVEL     046           ORGFEL
022400971111     C                   MOVEL     046           ALF3              3
022500021009     c                   Move      046           $mbr
022600021010     C                   MOVEL     VAR10         CMZMBD
022700971113     C*
022800971113     C* APRO MEMBRI DEL FILE PER LA TRASMISSIONE A SEDE
022900040505     C                   IF        WSOLOTRASM=' '
023000971113     C                   EXSR      CAEX
023100040505     C                   ENDIF
023200971113     C*
023300971111     C*  ESEGUO CALL PER REPERIRE LIBRERIA E S.I. DI TRAMSISSIONE
023400971111     C                   CLEAR                   DSBS50
023500971111     C                   MOVEL     KNSIF         I50PSI
023600021009     c                   Movel     Simfel        I50Ppo
023700971111     C                   MOVEL     046           I50APO
023800020927     C                   CALL      'TIBS55R'
023900971111     C                   PARM                    DSBS50
024000971111     C**
024100971209     C                   MOVEL     O50PLA        SAVLBD
024200971209     C                   MOVEL     O50ALA        SAVLBA
024300971209     C                   MOVEL     O50PLG        WLBLBD
024400971209     C                   MOVEL     O50ALG        WLBLBA
024500971111     C                   MOVEL     O50ASI        CMZSYF
024600971111     C** TRASMISSIONE    B O L L E   P A R T I T E
024700971111     C**
024800040505     C                   IF        WSOLOTRASM=' '
024900971111     C                   EXSR      LCKSD
025000971111     C   40              Z-ADD     1             WSED              1 0
025100971111     C**
025200971111     C** TRASMISSIONE    F O G L I   V I A G G I O
025300971111     C**
025400001107     C     PA3NFV        IFNE      '99999'
025500021009     c                   movel     var10         pa2mbd
025600971113     C                   MOVEL     SIMFEL        PA2FEL
025700971111     C                   CLEAR                   PA2ALL
025800971111     C                   CLEAR                   PA2TLA
025900971111     C                   MOVEL     PARAM2        KPJBU
026000971111     C                   CALL      'FNLSA4R'
026100971111     C                   PARM                    KPJBA
026200971111     C                   MOVEL     KPJBU         PARAM2
026300971111     C     PA2ALL        IFEQ      '1'
026400971111     C                   SETON                                        40
026500971111     C                   ADD       1             H
026600971111     C                   MOVEL     046           FE2(H)
026700971111     C                   ENDIF
026800001107     C                   ENDIF
026900001107     C**
027000041115     C* RICHIAMO ROUTINE TRAMSISS. legami bolla
027100041122     C*****              EXSR      TUTTI
027200040505     C                   ELSE
027300040505     C**
027400040505     C** TRASMISSIONE    V O L U M E   C M L
027500040505     C**
027600040505     C                   EXSR      LCKARV
027700040505     C   40              ADD       1             I
027800040505     C   40              MOVEL     CAETFE        ARV(I)
027900040505     C**
028000040505     C** TRASMISSIONE    P E S O   C M L
028100040505     C                   EXSR      LCKARP
028200040505     C   40              ADD       1             P
028300040505     C   40              MOVEL     CAETFE        ARP(P)
028400040505     C                   ENDIF
028500930929     C*
028600930929     C* RITENTO TRASMISSIONE IN SEDE SE ERA ALLOCATA
028700930929     C     WSED          IFEQ      1
028800021010     C                   MOVEL     VAR10         CMZMBD
028900930929     C                   EXSR      LCKSD
029000930929     C                   ENDIF
029100950113     C**
029200930716     C* RITENTO PER QUELLI CHE NON SONO RIUSCITA DA ALLOCARE
029300950113     C**
029400950113     C* DA TRASMETTERE SOLO IN SEDE   FGV E FVD
029500001107     C     PA3NFV        IFNE      '99999'
029600001107     C***
029700001107M    C                   Z-ADD     1             H
029800001107     C     FE2(H)        DOWNE     0
029900971111     C                   MOVEL     FE2(H)        CAETFE
030000021010     C**                 MOVEL     VAR2          CMZMBD
030100021010     C                   MOVEL     VAR10         CMZMBD
030200971111     C     CAETFE        IFEQ      046
030300021009     c                   movel     var10         pa2mbd
030400971113     C                   MOVEL     SIMFEL        PA2FEL
030500951207     C                   CLEAR                   PA2ALL
030600951207     C                   CLEAR                   PA2TLA
030700951207     C                   MOVEL     PARAM2        KPJBU
030800951207     C                   CALL      'FNLSA4R'
030900951207     C                   PARM                    KPJBA
031000930716     C                   ENDIF
031100930716     C                   ADD       1             H
031200930716     C                   ENDDO
031300950113      *
031400950113     C                   Z-ADD     1             I
031500950113     C     ARV(I)        DOWNE     0
031600971111     C                   MOVEL     ARV(I)        CAETFE
031700971210     C                   MOVEL     CAETFE        ALF3              3
031800021009     c                   Movel     Var10         CmzMbd
031900971209     C                   EXSR      CALTBS
032000950113     C                   EXSR      LCKARV
032100950113     C                   ADD       1             I
032200950113     C                   END
032300990903      *
032400990903     C                   Z-ADD     1             P
032500990903     C     ARP(P)        DOWNE     0
032600990903     C                   MOVEL     ARP(P)        CAETFE
032700990903     C                   MOVEL     CAETFE        ALF3              3
032800021009     c                   Movel     Var10         CmzMbd
032900990903     C                   EXSR      CALTBS
033000990903     C                   EXSR      LCKARP
033100990903     C                   ADD       1             P
033200990903     C                   END
033300950114     C**
033400950114     C** S A L D I   S T A T I S T I C H E    ATTIVITA' COMMERCIALE
033500950114     C* LA DATA DEL GIORNO LA PRENDO DA TIME
033600050301     C**                 IF        WSOLOTRASM=' '
033700050301      **
033800050301     C**                 TIME                    W0140            14 0
033900050301     C**                 MOVE      W0140         DATA
034000050301     C**ELABORO AL 2 GIORNO DEL MESE
034100050301    2C**   GG            IFGE      2
034200050301     C**LEGGO  LA DATA statistica
034300050301     c**                 Clear                   Tibs02Ds
034400050301     c**                 Clear                   dSdf
034500050301     c**                 Eval      T02Mod = 'C'
034600050301     c**                 Eval      T02Sif = Knsif
034700050301     c**                 Eval      T02Cod = 'SDF'
034800050301     c**                 Movel(p)  Savfel        T02Ke1
034900050301     c**                 Call      'TIBS02R'
035000050301     c**                 Parm                    Kpjba
035100050301     c**                 Parm                    Tibs02Ds
035200050301    3c**                 If        T02Err = *Blanks
035300050301     c**                 Movel     T02Uni        dSdf
035400050301     C**
035500050301    3C**!!!*IN46         IFEQ      *OFF
035600050301     C**                 SUB       1             MM
035700050301    4C**   MM            IFEQ      0
035800050301     C**                 Z-ADD     12            MM
035900050301     C**                 SUB       1             AA
036000050301    4C**                 ENDIF
036100050301     C**
036200050301     C**                 MOVEL     AA            WAAMM             6 0
036300050301     C**                 MOVE      MM            WAAMM
036400050301     C**
036500050301     C*SE ULTIMA ELABORAZIONE < MESE DA ELABORARE --> SOTTOMETTO PGM
036600050301     C**!!!WAAMM         SUB       TNTA66        DIFF              6 0
036700050301     c**   WAAMM         SUB       §SdfStcU      DIFF              6 0
036800050301    4C**   DIFF          IFEQ      1
036900050301     C**                 MOVEL     'TA66'        KCOAZ
037000050301     C**                 CLEAR                   KPJBU
037100050301     C**                 CALL      'BCH10'
037200050301     C**                 PARM                    KPJBA
037300950114     C*
037400050301   X4C**                 ELSE
037500950114     C*
037600950114     C* ELABORO TUTTI I MESI MANCANTI
037700021017    5C**!!!TNTA66        DOWLT     WAAMM
037800050301    5C**   §SdfStcU      DOWLT     WAAMM
037900021017     C**!!!              ADD       1             TNTA66
038000050301     C**                 ADD       1             §SdfStcU
038100021017     C**!!!              MOVEL     TNTA66        PARAM
038200050301     C**                 MOVEL     §SdfStcU      PARAM
038300950114     C*
038400050301    6C**   PARMM         IFGT      12
038500050301     C**                 Z-ADD     1             PARMM
038600050301     C**                 ADD       1             PARAA
038700950114     C*
038800021017     C**!!!              MOVEL     PARAM         TNTA66
038900050301     C**                 MOVEL     PARAM         §SdfStcU
039000050301    6C**                 ENDIF
039100950114     C*
039200050301     C**                 CLEAR                   KPJBU
039300050301     C**                 MOVEL     PARAM         KPJBU
039400050301     C**                 MOVEL     'TA66'        KCOAZ
039500050301     C**                 CALL      'BCH10'
039600050301     C**                 PARM                    KPJBA
039700050301    4C**
039800950114     C*
039900050301    4C**                 END
040000050301    3C**                 END
040100050301    2C**                 END
040200050301     C**                 ENDIF
040300001205      ***
040400001205     C* RICHIAMO PGM SCRITTURA MEMBRI DI TRASMISSIONE STOP PADRONCINI
040500001205     C* NON LO FACCIO PER L'AUTOGENERAZIONE BOLLE POSTE
040600010312     C* RICHIAMO ANCHE PGM TRASMI O.R.M.
040700020917      * ELIMINATA TRASM. ORM PERCHE' SCHEDULATA
040800001205     C     PA3NFV        IFNE      '99999'
040900010320     C* RICHIAMO ANCHE PGM CHIUSURA FOGLI VARI CON SOLI O.R.M.
041000010320     C                   MOVE      'LRA9'        KCOAZ
041100010320     C                   CALL      'BCH10'
041200010320     C                   PARM                    KPJBA
041300010312     C*
041400001205     C                   MOVE      'LRC1'        KCOAZ
041500001205     C                   CALL      'BCH10'
041600001205     C                   PARM                    KPJBA
041700010312     C*
041800001205     C                   ENDIF
041900040505     C                   ENDIF
042000001205      ***
042100950104      *
042200971113     C* CHIUDO I PGM
042300971113     C                   CLEAR                   DSBS50
042400971113     C                   MOVEL     'C'           I50TLA
042500020927     C                   CALL      'TIBS55R'
042600971113     C                   PARM                    DSBS50
042700050616     c*
042800050616     C                   CLEAR                   tibs02ds
042900050616     C                   MOVEL     'C'           T02TLA
043000050616     C                   CALL      'TIBS02R'
043100050616     C                   PARM                    KPJBA
043200050616     C                   PARM                    tibs02ds
043300930512     C*
043400000000     C                   SETON                                        LR
043500950104     C**************************************************************************
043600930929     C** IMPOSTO ALCUNI DATI FISSI PER CREARE I FILE DI TRASMISSIONE
043700950104     C**************************************************************************
043800930929     C     IMPO          BEGSR
043900950104     C*
044000021010     C***                MOVEL     'M'           EMME
044100930929     C                   MOVEL     '3A'          COD
044200930929     C*
044300930929     C                   MOVE      '0000'        CMZORI
044400930929     C                   MOVE      '0000000'     CMZDTI
044500000112     C                   MOVE      'R'           CMZURG
044600930929     C                   MOVE      'A'           CMZADD
044700930929     C                   MOVE      'S'           CMZCLR
044800930929     C                   MOVE      'S'           CMZFTR
044900930929     C                   MOVE      *BLANK        CMZERR
045000021010     C**                 MOVEL     'M'           ALF4              4
045100021010     C**                 MOVE      SIMFEL        ALF4
045200021010     C**                 MOVEL     ALF4          CMZMBA
045300950104     C*
045400930929     C                   ENDSR
045500991026     C**************************************************************************
045600060220     C** TABELLA DEI TIPI RECORD FILE Fiar4 A INVIARE A SEDE
045700991026     C**************************************************************************
045800991026     C     CAR7K         BEGSR
045900991026     C                   CLEAR                   X
046000050616     C                   CLEAR                   Y
046100991026     C                   MOVEL     '7K'          COD
046200991026     C     KTAB          CHAIN     TABEL                              30
046300991026     C     *IN30         DOWEQ     *OFF
046400991026     C     TBLFLG        IFEQ      ' '
046500991026     C                   MOVEL     TBLUNI        DS7K
046600991026     C     §7KSED        IFEQ      'S'
046700991026     C                   ADD       1             X                 3 0
046800991026     C                   MOVEL     TBLKEY        C7K(X)
046900050616     c                   else
047000050616     C     §7KSED        IFEQ      'K'
047100050616     C                   ADD       1             Y                 3 0
047200050616     C                   MOVEL     TBLKEY        C7KK(Y)
047300050616     c                   endif
047400991026     C                   ENDIF
047500991026     C                   ENDIF
047600991026     C**
047700991026     C     KTAB          READE     TABEL                                  30
047800991026     C                   ENDDO
047900050616     c*
048000050616     C                   CLEAR                   Y
048100050616     c     'KB4'         setll     tntbe01l
048200050616     c                   do        *hival
048300050616     c     'KB4'         reade     tntbe01l
048400050617     c                   if        %eof(tntbe01l)
048500050616     c                   leave
048600050616     c                   endif
048700050617     c                   if        tbeatb =  *blanks
048800050616     c                   add       1             y
048900050616     c                   movel     tbeke1        CKB4(Y)
049000050617     c                   eval      %subst(ckb4(y):8:1) = %subst(tbeke2:1:1)
049100050616     c                   endif
049200050616     c                   enddo
049300991026     C                   ENDSR
049400950104     C**************************************************************************
049500950104     C*  ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE
049600950104     C**************************************************************************
049700930929     C     CAEX          BEGSR
049800930929     C*
049900080908     C                   MOVEL     'FIBLPA0T'    FILE             10
050000930929     C                   CALL      'TRUL50C'
050100930929     C                   PARM                    FILE
050200021010     C                   PARM                    VAR10
050300950112     C                   PARM                    KNSIF
050400930929     C                   PARM                    LOGICO           10
050500930929     C                   PARM                    LOGMBR           10
050600930929     C                   PARM                    FLG               1
050700930929     C*
050800991026     C                   MOVEL     'FIAR600T'    FILE
050900930929     C                   CALL      'TRUL50C'
051000930929     C                   PARM                    FILE
051100021010     C                   PARM                    VAR10
051200950112     C                   PARM                    KNSIF
051300930929     C                   PARM                    LOGICO           10
051400930929     C                   PARM                    LOGMBR           10
051500930929     C                   PARM                    FLG               1
051600930929     C*
051700991026     C                   MOVEL     'FIAR700T'    FILE
051800930929     C                   CALL      'TRUL50C'
051900930929     C                   PARM                    FILE
052000021010     C                   PARM                    VAR10
052100950112     C                   PARM                    KNSIF
052200930929     C                   PARM                    LOGICO           10
052300930929     C                   PARM                    LOGMBR           10
052400930929     C                   PARM                    FLG               1
052500951120     C*
052600060220     C                   MOVEL     'FIAR400T'    FILE
052700970214     C                   CALL      'TRUL50C'
052800970214     C                   PARM                    FILE
052900021010     C                   PARM                    VAR10
053000970214     C                   PARM                    KNSIF
053100970214     C                   PARM                    LOGICO           10
053200970214     C                   PARM                    LOGMBR           10
053300970214     C                   PARM                    FLG               1
053400000905     C*
053500000905     C                   MOVEL     'TITAH00T'    FILE
053600000905     C                   CALL      'TRUL50C'
053700000905     C                   PARM                    FILE
053800021010     C                   PARM                    VAR10
053900000905     C                   PARM                    KNSIF
054000000905     C                   PARM                    LOGICO           10
054100000905     C                   PARM                    LOGMBR           10
054200000905     C                   PARM                    FLG               1
054300030110
054400030110     c                   Movel     'FIAR500T'    File
054500030110     c                   Call      'TRUL50C'
054600030110     c                   Parm                    File
054700030110     c                   Parm                    Var10
054800030110     c                   Parm                    Knsif
054900030110     c                   Parm                    Logico           10
055000030110     c                   Parm                    Logmbr           10
055100030110     c                   Parm                    Flg               1
055200970214     C*
055300930929     C                   ENDSR
055400950104     C**************************************************************************
055500950104     C*  ALLOCAZIONE MEMBRI PER LA SEDE
055600950104     C**************************************************************************
055700930929     C     LCKSD         BEGSR
055800950104     C*
055900930929     C                   SETOFF                                       404748
056000930929     C                   SETOFF                                       495253
056100030110     C                   SETOFF                                       545556
056200930929     C*
056300021010     C                   MOVE      '))'          VAR               8
056400021010     C                   Z-ADD     42            LUNG             15 5
056500021010     C                   MOVE      046           $mbr
056600021010     C                   MOVEL     $mbr          VAR
056700930929     C                   MOVE      VAR           CMD1
056800930929     C                   MOVEL     *BLANKS       COMMAN           80
056900930929     C                   MOVEA     CMD1(1)       COMMAN
057000930929     C                   CALL      'QCMDEXC'                            47
057100930929     C                   PARM                    COMMAN
057200930929     C                   PARM                    LUNG
057300930929     C*
057400930929     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
057500930929     C   47              SETON                                          40
057600930929     C*
057700991026    1C  N47              DO
057800991026     C                   MOVE      VAR           CM33
057900930929     C                   MOVEL     *BLANKS       COMMAN           80
058000930929     C                   MOVEA     CM33(1)       COMMAN
058100930929     C                   CALL      'QCMDEXC'                            53
058200930929     C                   PARM                    COMMAN
058300930929     C                   PARM                    LUNG
058400930929     C* GIA' ALLOCATO RITENTO PIU' TARDI
058500930929     C   53              SETON                                        40
058600930929     C*
058700991026    2C  N53              DO
058800991026     C                   MOVE      VAR           CMD3
058900930929     C                   MOVEL     *BLANKS       COMMAN           80
059000930929     C                   MOVEA     CMD3(1)       COMMAN
059100930929     C                   CALL      'QCMDEXC'                            48
059200930929     C                   PARM                    COMMAN
059300930929     C                   PARM                    LUNG
059400930929     C* GIA' ALLOCATO RITENTO PIU' TARDI
059500930929     C   48              SETON                                        40
059600930929     C*
059700991026    3C  N48              DO
059800021010     C                   MOVEL     simfel        W003AA            3
059900021010     C                   MOVEL     046           W003A             3
060000060220     C                   MOVEA     'FIAR400T'    C56(13)
060100021010     C                   MOVEA     W003AA        C56(35)
060200021010     C                   MOVEA     W003A         C56(38)
060300970214     C                   MOVEL     *BLANKS       COMMAN           80
060400970214     C                   MOVEA     C56           COMMAN
060500970214     C                   CALL      'QCMDEXC'                            54
060600970214     C                   PARM                    COMMAN
060700970214     C                   PARM                    LUNG
060800970214     C* GIA' ALLOCATO RITENTO PIU' TARDI
060900970214     C   54              SETON                                        40
061000970214     C*
061100991026    5C  N54              DO
061200000905     C                   MOVEA     'TITAH00T'    C56(13)
061300021010     C                   MOVEA     W003AA        C56(35)
061400021010     C                   MOVEA     W003A         C56(38)
061500000905     C                   MOVEL     *BLANKS       COMMAN           80
061600000905     C                   MOVEA     C56           COMMAN
061700000905     C                   CALL      'QCMDEXC'                            55
061800000905     C                   PARM                    COMMAN
061900000905     C                   PARM                    LUNG
062000000905     C* GIA' ALLOCATO RITENTO PIU' TARDI
062100000905     C   55              SETON                                        40
062200030110
062300030110   5bc  n55              Do
062400030110     c                   Movel     Simfel        w003aa
062500030110     c                   Movel     046           w003a
062600030110     c                   Movea     'FIAR500T'    c56(13)
062700030110     c                   Movea     w003aa        c56(35)
062800030110     c                   Movea     w003a         c56(38)
062900030110     c                   Movel     *Blanks       Comman
063000030110     c                   Movea     c56           Comman
063100030110     c                   Call      'QCMDEXC'                            56
063200030110     c                   Parm                    Comman
063300030110     c                   Parm                    Lung
063400030110      * Già allocato ritento più tardi
063500030110     c   56              Seton                                        40
063600991026     C*
063700030110    6C  N56              DO
063800930929     C*
063900930929     C* S C R I V O   M E M B R I  D I   S E D E
064000930929     C                   EXSR      WTRSED
064100021009
064200021010     c                   Movel     Var10         VarMbr
064300930929     C*
064400930929     C* VEDO SE CONTIENE RECORD FLBLP
064500991026     C                   MOVEL     'FIBLPA0T'    CMZFLD
064600930929     C                   EXSR      NUMREC
064700930929     C                   Z-ADD     NRR           RECBPA           10 0
064800930929     C* I N V I O
064900991026    7C     RECBPA        IFGT      0
065000930929     C*
065100991026     C                   MOVEL     'FIAR600T'    CMZFLD
065200930929     C                   EXSR      NUMREC
065300950112     C                   Z-ADD     NRR           RECB6            10 0
065400930929     C*
065500991026     C                   MOVEL     'FIAR700T'    CMZFLD
065600930929     C                   EXSR      NUMREC
065700950112     C                   Z-ADD     NRR           RECB7            10 0
065800970217     C*
065900060220     C                   MOVEL     'FIAR400T'    CMZFLD
066000970217     C                   EXSR      NUMREC
066100970217     C                   Z-ADD     NRR           RECB4            10 0
066200000905     C*
066300000905     C                   MOVEL     'TITAH00T'    CMZFLD
066400000905     C                   EXSR      NUMREC
066500000905     C                   Z-ADD     NRR           RECTH            10 0
066600030110
066700030110     c                   Movel     'FIAR500T'    CmzFld
066800030110     c                   ExSr      Numrec
066900030110     c                   Z-Add     Nrr           RecFiar          10 0
067000930929     C*
067100930929     C                   EXSR      X25SD
067200930929     C*
067300991026    7C                   END
067400930929     C**
067500021010     C                   MOVEL     simfel        W003AA            3
067600021010     C                   MOVEL     046           W003A
067700030110
067800030110      * Disalloco FIAR5
067900030110     c                   Movea     'FIAR500T'    c57(13)
068000030110     c                   Movea     w003aa        c57(35)
068100030110     c                   Movea     w003a         c57(38)
068200030110     c                   Movel     *Blanks       Comman
068300030110     c                   Movea     c57           Comman
068400030110     c                   Call      'QCMDEXC'                            47
068500030110     c                   Parm                    Comman
068600030110     c                   Parm                    Lung
068700030110    bC                   EndDo
068800021010     c*
068900030110     C* DISALLOCO TITAH
069000000905     C                   MOVEA     'TITAH00T'    C57(13)
069100021010     C                   MOVEA     W003AA        C57(35)
069200021010     C                   MOVEA     W003A         C57(38)
069300000905     C                   MOVEL     *BLANKS       COMMAN           80
069400000905     C                   MOVEA     C57           COMMAN
069500000905     C                   CALL      'QCMDEXC'                            47
069600000905     C                   PARM                    COMMAN
069700000905     C                   PARM                    LUNG
069800991026    6C                   END
069900060220     C* DISALLOCO Fiar4
070000060220     C                   MOVEA     'FIAR400T'    C57(13)
070100021010     C                   MOVEA     W003AA        C57(35)
070200021010     C                   MOVEA     W003A         C57(38)
070300991026     C                   MOVEL     *BLANKS       COMMAN           80
070400991026     C                   MOVEA     C57           COMMAN
070500991026     C                   CALL      'QCMDEXC'                            47
070600991026     C                   PARM                    COMMAN
070700991026     C                   PARM                    LUNG
070800991026    5C                   END
070900930929     C**
071000991026     C* DISALLOCO FIAR7
071100991026     C                   MOVE      VAR           CMD4
071200930929     C                   MOVEL     *BLANKS       COMMAN           80
071300930929     C                   MOVEA     CMD4(1)       COMMAN
071400930929     C                   CALL      'QCMDEXC'                            47
071500930929     C                   PARM                    COMMAN
071600930929     C                   PARM                    LUNG
071700991026    3C                   END
071800930929     C**
071900991026     C* DISALLOCO FNBLP
072000991026     C                   MOVE      VAR           CM34
072100930929     C                   MOVEL     *BLANKS       COMMAN           80
072200930929     C                   MOVEA     CM34(1)       COMMAN
072300930929     C                   CALL      'QCMDEXC'                            47
072400930929     C                   PARM                    COMMAN
072500930929     C                   PARM                    LUNG
072600991026    2C                   END
072700930929     C*
072800991026     C                   MOVE      VAR           CMD2
072900930929     C                   MOVEL     *BLANKS       COMMAN           80
073000930929     C                   MOVEA     CMD2(1)       COMMAN
073100930929     C                   CALL      'QCMDEXC'                            47
073200930929     C                   PARM                    COMMAN
073300930929     C                   PARM                    LUNG
073400991026    1C                   END
073500930929     C*
073600930929     C                   ENDSR
073700950104     C**************************************************************************
073800950104     C*  SCRIVO MEMBRO PER LA SEDE
073900950104     C**************************************************************************
074000910913     C     WTRSED        BEGSR
074100950104     C*
074200910926     C* APERTURA MEMBRI
074300910926     C                   EXSR      CAEXOV
074400060220     C* CARICO TIPI RECORD Fiar4 DA INVARE A SEDE + tabella dei clienti
074500050616     c* abilitati all'invio di tipi record di regola non trasmessi
074600991026     C                   EXSR      CAR7K
074700070423     c                   clear                   wtfp              3 0
074800070423     c                   if        pa3tfp>'000'
074900070423     c                   move      pa3tfp        wtfp
075000070423     c                   endif
075100910913     C*
075200070423     c                   if        wtfp>0
075300070423     c     wtfp          setll     fnblp000
075400070423     c     wtfp          reade     fnblp000                               31
075500070423     c                   else
075600070423     C                   READ      FNBLP000                               31
075700070423     c                   endif
075800910913     C*
075900950112     C     *IN31         DOWEQ     *OFF
076000991026     C     KBLP          CHAIN     FNBLP001                           33
076100991026     C*
076200991026     C     KBLP          CHAIN     FIAR6000                           33
076300991026     C     *IN33         DOWEQ     *OFF
076400991026     C                   WRITE     FIAR60TT
076500991026     C     KBLP          READE     FIAR6000                               33
076600991026     C                   ENDDO
076700991026     C**
076800991026     C* ESTENSIONE TASSAZIONE
076900991026     C**
077000991026     C     KBLP          CHAIN     FIAR7000                           33
077100991026     C     *IN33         DOWEQ     *OFF
077200991026     C                   WRITE     FIAR70TT
077300991026     C*
077400991026     C     KBLP          READE     FIAR7000                               33
077500991026     C                   ENDDO
077600991026     C**
077700060220     C* ESTENSIONE BOLLA Fiar4
077800991026     C**
077900060220     C     KBLP          CHAIN     Fiar4000                           33
078000991026     C     *IN33         DOWEQ     *OFF
078100991026     C*SOLO I TIPI RECORD CHE MI SERVONO IN SEDE
078200060220     C     ar4TRC        LOOKUP    C7K                                    32
078300060220     C   32              WRITE     Fiar40TT
078400050616     c* Vedo se tipo record da inviare solo su abilitazione del cliente
078500050616    3c                   if        *in32 = *off
078600060220     C     ar4TRC        LOOKUP    C7KK                                   32
078700050616     c* vedo se cliente presente in tabella "KB4"
078800050616    4c                   if        *in32
078900050616    5c                   if        blpccm > 0
079000050617     c                   Movel(p)  blpccm        wcodmit           8
079100050616   x5c                   else
079200050616     c                   move      blpksc        w0040             4 0
079300050616    6c                   if        w0040 <> 8888 and w0040 <> 9999
079400050617     c                   Movel(p)  blpksc        wcodmit
079500050616   x6c                   else
079600050617     c                   clear                   wcodmit
079700050616    6c                   endif
079800050616    5c                   endif
079900050617    5c                   if        wcodmit > *zeros
080000060220     c                   move      ar4trc        wcodmit
080100050617     c     wcodmit       lookup    CKB4                                   32
080200050617    6c                   If        *in32
080300060220     C                   WRITE     Fiar40TT
080400050616    6c                   endif
080500050616    5c                   endif
080600050616    4c                   endif
080700050616    3c                   endif
080800991026     C*
080900060220     C     KBLP          READE     Fiar4000                               33
081000991026     C                   ENDDO
081100991026     C**
081200051116     C* C/ASSEGNO        Fiar9
081300991026     C**
081400051116     C     KBLP          CHAIN     Fiar9000                           33
081500991026     C     *IN33         IFEQ      *ON
081600991026     C                   CLEAR                   BL9TIC
081700991026     C                   CLEAR                   BL9CAS
081800991026     C                   CLEAR                   BL9VCA
081900991026     C                   CLEAR                   BL9CMB
082000991026     C                   CLEAR                   BL9GCA
082100051116     c                   else
082200051116     c                   eval      bl9tic=ar9tic
082300051116     c                   eval      bl9cas=ar9cas
082400051116     c                   eval      bl9vca=ar9vca
082500051116     c                   eval      bl9cmb=ar9cmb
082600051116     c                   eval      bl9gca=ar9gca
082700991026     C                   ENDIF
082800991026     C**
082900090304     C* DETTAGLIO SEGNACOLLI FNART - per titah
083000991026     C**
083100090304     C     KBLP          CHAIN     FiART000                           33
083200000906     C     *IN33         DOWEQ     *OFF
083300000906     C                   EXSR      WTRTAH
083400000906     C*                    - - - - - -
083500090304     C     KBLP          READE     FiART000                               33
083600000906     C                   ENDDO
083700991026     C**
083800000127     C                   MOVE      BLPKSC        KSC4              4 0
083900000127     C     KSC4          IFEQ      9999
084000000127     C                   MOVEL     BLPLNA        BLPKSC
084100000127     C                   ENDIF
084200030113
084300030113      *  Estensione Testata Bolla    F I A R 5 0 0 F
084400030113      * Trasmetto solo se lnp e lna non solo nello stesso S.I.
084500030113     c     Kblp          Setll     Fiar501l
084600030113Do  2c                   Do        *Hival
084700030113     c     Kblp          Reade     Fiar501l
084800030113      * Fine File esco dal ciclo
084900030113     c                   If        %Eof(Fiar501l)
085000030113     c                   Leave
085100030113     c                   EndIf
085200030113      * Controllo se già inviato
085300030113     c                   If        Ar5Dt2 <> *All'9' and Ar5Dt2 > *zeros
085400030113     c                   Iter
085500030113     c                   EndIf
085600030113      * Invio alla sede solo se il flag in tabella lo prevede
085700030113     c                   Clear                   Tibs02Ds
085800030113     c                   Clear                   dAr5
085900030113     c                   Eval      T02Mod = 'C'
086000030113     c                   Eval      T02Sif = Knsif
086100030113     c                   Eval      T02Cod = 'AR5'
086200030113     c                   Movel(p)  Ar5Trd        T02Ke1
086300030113     c                   Call      'TIBS02R'
086400030113     c                   Parm                    Kpjba
086500030113     c                   Parm                    Tibs02Ds
086600030113     c                   If        T02Err = *Blanks
086700030113     c                   Movel     T02Uni        dAr5
086800030113     c                   EndIf
086900030113     c                   If        §Ar5Trse = 'S'
087000030113     c                   Write     Fiar50tt
087100030113     c                   EndIf
087200030113      * Flaggo per la trasmissione anche se il record non viene trasmesso
087300030113     c                   Eval      Ar5Ft2 = 'T'
087400030113     c                   Z-Add     Numtra        Ar5Dt2
087500030113     c                   Update    Fiar5000
087600030113    2c                   EndDo
087700000127     C*
087800991026     C                   WRITE     FIBLPA00
087900910926     C*
088000070423     C                   DELETE    FNBLP000
088100910913     C*
088200070423     c                   if        wtfp>0
088300070423     c     wtfp          reade     fnblp000                               31
088400070423     c                   else
088500070423     C                   READ      FnBLP000                               31
088600070423     c                   endif
088700950112     C                   ENDDO
088800910913     C* CHIUDO MEMBRI
088900991026     C                   CLOSE     FIBLPA0T
089000991026     C                   CLOSE     FIAR600T
089100991026     C                   CLOSE     FIAR700T
089200060220     C                   CLOSE     Fiar400T
089300000906     C                   CLOSE     TITAH00T
089400030110     c                   Close     Fiar500t
089500910913     C*
089600910913     C                   ENDSR
089700000906     C**************************************************************************
089800000906     C*  SCRIVO TITAH00T
089900000906     C**************************************************************************
090000000906     C     WTRTAH        BEGSR
090100000906     C*
090200051013     C     KSEGNA        SETLL     FiarS01L
090300000906     C                   DO        *HIVAL
090400051013     C     KSEGNA        READE     FiarS01L                               37
090500000906     C   37              LEAVE
090600000906     C*
090700000911     C*  CONTROLLO IN BASE AL TIPO RECORD SE E' DA INVIARE IN SEDE
090800000911     C*
090900000911     C                   MOVEL     '7H'          COD$
091000051013     C                   MOVEL(P)  arSTRC        KEY$
091100000911     C                   CLEAR                   DS7H
091200000911     C     KTABEL        CHAIN     TABEL00F                           39
091300000911     C  N39              MOVEL     TBLUNI        DS7H
091400000911     C*
091500000911     C     §7HFLT        IFNE      'S'
091600000911     C                   ITER
091700000911     C                   ENDIF
091800000911     C*
091900000906     C                   MOVE      BLPAAS        TAHAAS
092000000906     C                   MOVE      BLPLNP        TAHLNP
092100000906     C                   MOVE      BLPNRS        TAHNRS
092200000906     C                   MOVE      BLPNSP        TAHNSP
092300051013     C                   MOVE      arSTRC        TAHTRC
092400051013     C                   MOVE      arSNOT        TAHNOT
092500090304     C                   Z-ADD     ARTNSC        TAHNSC
092600000906     C*
092700000906     C                   WRITE     TITAH000
092800000906     C*
092900000906     C                   ENDDO
093000000906     C*
093100000906     C                   ENDSR
093200950104     C**************************************************************************
093300950104     C*  ESEGUE CALL QCAEXEC PER OVRDBF SUI FILE
093400950104     C**************************************************************************
093500910926     C     CAEXOV        BEGSR
093600950104     C*
093700911108     C                   MOVE      ')'           VAR4              4
093800971111     C                   MOVEL     046           VAR4
093900021010     C                   MOVEL     simfel        w003aa
094000021010     C                   Z-ADD     42            LUNG
094100910913     C*
094200910926     C** OVRDBF
094300021010     C                   MOVEA     W003aa        C17(36)
094400021010     C                   MOVEA     VAR4          C17(39)
094500991026     C                   MOVEA     'FIBLPA0T'    C17(13)
094600910926     C                   MOVEL     *BLANKS       COMMAN
094700920221     C                   MOVEA     C17           COMMAN
094800910926     C                   CALL      'QCMDEXC'
094900910926     C                   PARM                    COMMAN
095000910926     C                   PARM                    LUNG
095100910926     C*
095200991026     C                   MOVEA     'FIAR600T'    C17(13)
095300910913     C                   MOVEL     *BLANKS       COMMAN
095400920221     C                   MOVEA     C17           COMMAN
095500910913     C                   CALL      'QCMDEXC'
095600910913     C                   PARM                    COMMAN
095700910913     C                   PARM                    LUNG
095800910913     C*
095900991026     C                   MOVEA     'FIAR700T'    C17(13)
096000910913     C                   MOVEL     *BLANKS       COMMAN
096100920221     C                   MOVEA     C17           COMMAN
096200910913     C                   CALL      'QCMDEXC'
096300910913     C                   PARM                    COMMAN
096400910913     C                   PARM                    LUNG
096500970214     C*
096600060220     C                   MOVEA     'FIAR400T'    C17(13)
096700970214     C                   MOVEL     *BLANKS       COMMAN
096800970214     C                   MOVEA     C17           COMMAN
096900970214     C                   CALL      'QCMDEXC'
097000970214     C                   PARM                    COMMAN
097100970214     C                   PARM                    LUNG
097200000905     C*
097300000905     C                   MOVEA     'TITAH00T'    C17(13)
097400000905     C                   MOVEL     *BLANKS       COMMAN
097500000905     C                   MOVEA     C17           COMMAN
097600000905     C                   CALL      'QCMDEXC'
097700000905     C                   PARM                    COMMAN
097800000905     C                   PARM                    LUNG
097900030110
098000030110     c                   Movea     'FIAR500T'    c17(13)
098100030110     c                   Movel     *Blanks       Comman
098200030110     c                   Movea     c17           Comman
098300030110     c                   Call      'QCMDEXC'
098400030110     c                   Parm                    Comman
098500030110     c                   Parm                    Lung
098600910913     C*
098700910913     C* APERTURA FILE
098800991026     C                   OPEN      FIBLPA0T
098900991026     C                   OPEN      FIAR700T
099000991026     C                   OPEN      FIAR600T
099100060220     C                   OPEN      Fiar400T
099200000905     C                   OPEN      TITAH00T
099300030110     c                   Open      Fiar500t
099400950104     C*
099500910913     C                   ENDSR
099600950104     C**************************************************************************
099700950104     C*  PREPARA SOTTOMISSIONE TRASMISSIONE PER LA SEDE
099800950104     C**************************************************************************
099900910926     C     X25SD         BEGSR
100000910926     C*
100100930203     C                   MOVE      '046'         CMZDST
100200971209     C                   MOVEL     SAVLBD        CMZLBD
100300971209     C                   MOVEL     SAVLBA        CMZLBA
100400021010     c                   Movel     Var10         CmzMba
100500910926     C* SE CI SONO RECORD TRASMETTO
100600910926     C     RECBPA        IFGT      0
100700911217     C                   MOVE      *BLANK        CMZPRR
100800930203     C                   MOVE      *BLANK        CMZAZN
100900970217     C                   EXSR      X25B4
101000991026     C                   EXSR      X25B7
101100991026     C                   EXSR      X25B6
101200000906     C                   EXSR      X25TH
101300030110     c                   ExSr      x25Fiar
101400991026     C                   EXSR      X25BPA
101500910926     C                   END
101600950104     C*
101700910926     C                   ENDSR
101800950113     C**************************************************************************
101900991026     C*  ALLOCAZIONE MEMBRI FILE VOLUME CML
102000950113     C**************************************************************************
102100950113     C     LCKARV        BEGSR
102200950113     C*
102300950113     C                   SETOFF                                       40
102400950113     C* VEDO SE CONTIENE RECORD
102500950113     C                   MOVEL     'FNARBV0T'    CMZFLD
102600021009     c                   Movel     Var10         VarMbr
102700950113     C                   EXSR      NUMREC
102800950113     C                   Z-ADD     NRR           RECARV           10 0
102900021009     c                   Movel     Var10         CmzMba
103000021009     c                   Movel     Var10         CmzMbd
103100950113     C* ALLOCO MEMBRO
103200950113     C     RECARV        IFGT      0
103300950113     C     RECARV        IFNE      *ALL'9'
103400021009     C                   MOVE      '))'          VAR8
103500041122     C                   Z-ADD     42            LUNG1            15 5
103600021009     C**!!!              MOVEA     'FNARBV0T'    C56(13)
103700021009     C**!!!              MOVEA     ALF3          C56(35)
103800021009     c                   Eval      %Subst(Cvp(1):13:8) = 'FNARBV0T'
103900040510     c                   Eval      %Subst(Cvp(1):34:1) = emme1
104000021009     C                   MOVEL     $mbr          VAR8
104100021009     C                   MOVE      VAR8          Cvp(1)
104200950113     C                   MOVEL     *BLANKS       COMMAN           80
104300021009     C**!!!              MOVEA     C56           COMMAN
104400021009     c                   Movel     Cvp(1)        Comman
104500950113     C                   CALL      'QCMDEXC'                            40
104600950113     C                   PARM                    COMMAN
104700021009     C                   PARM                    LUNG1
104800950113     C*
104900950113     C                   ELSE
105000950113     C                   SETON                                          40
105100950113     C                   END
105200950113     C*
105300950113     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
105400950113     C*
105500950113     C  N40              DO
105600950113     C* INVIO
105700950113     C                   MOVE      ORGFEL        CMZDST
105800971113     C                   MOVEL     'TR47'        CMZAZN
105900950113     C                   MOVEL     *BLANKS       CMZPRR
106000950113     C                   MOVEL     'FNARBV0R'    CMZFLA
106100950113     C                   MOVE      RECARV        CMZNRR
106200971209     C                   MOVEL     SAVLBD        CMZLBD
106300971209     C                   MOVEL     SAVLBA        CMZLBA
106400950113     C*
106500950113     C                   EXSR      CALCMZ
106600950113     C*
106700950113     C* DISALLOCO
106800021009     c                   Eval      %Subst(Cvp(2):13:8) = 'FNARBV0T'
106900040510     c                   Eval      %Subst(Cvp(2):34:1) = emme1
107000021009     C                   MOVE      VAR8          Cvp(2)
107100950113     C                   MOVEL     *BLANKS       COMMAN           80
107200021009     C**!!!              MOVEA     C57           COMMAN
107300021009     c                   Movel     Cvp(2)        Comman
107400950113     C                   CALL      'QCMDEXC'
107500950113     C                   PARM                    COMMAN
107600021009     C                   PARM                    LUNG1
107700950113     C                   END
107800950113     C                   END
107900950113     C*
108000950113     C                   ENDSR
108100990903     C**************************************************************************
108200990903     C*  ALLOCAZIONE MEMBRI FILE PESO CML
108300990903     C**************************************************************************
108400990903     C     LCKARP        BEGSR
108500990903     C*
108600990903     C                   SETOFF                                       40
108700990903     C* VEDO SE CONTIENE RECORD
108800990903     C                   MOVEL     'FNARBP0T'    CMZFLD
108900021009     c                   Movel     Var10         VarMbr
109000990903     C                   EXSR      NUMREC
109100990903     C                   Z-ADD     NRR           RECARV           10 0
109200021009     c                   Movel     Var10         CmzMba
109300021009     c                   Movel     Var10         CmzMbd
109400990903     C* ALLOCO MEMBRO
109500990903     C     RECARV        IFGT      0
109600990903     C     RECARV        IFNE      *ALL'9'
109700021009     C                   MOVE      '))'          VAR8
109800021009     C                   Z-ADD     42            LUNG1
109900021009     C**!!!              MOVEA     'FNARBP0T'    C56(13)
110000021009     C**!!!              MOVEA     ALF3          C56(35)
110100021009     c                   Eval      %Subst(Cvp(1):13:8) = 'FNARBP0T'
110200040510     c                   Eval      %Subst(Cvp(1):34:1) = emme1
110300021009     C                   MOVEL     $mbr          VAR8
110400021009     C                   MOVE      VAR8          Cvp(1)
110500990903     C                   MOVEL     *BLANKS       COMMAN           80
110600021009     C**!!!              MOVEA     C56           COMMAN
110700021009     c                   Movel     Cvp(1)        Comman
110800990903     C                   CALL      'QCMDEXC'                            40
110900990903     C                   PARM                    COMMAN
111000021009     C                   PARM                    LUNG1
111100990903     C*
111200990903     C                   ELSE
111300990903     C                   SETON                                          40
111400990903     C                   END
111500990903     C*
111600990903     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
111700990903     C*
111800990903     C  N40              DO
111900990903     C* INVIO
112000990903     C                   MOVE      ORGFEL        CMZDST
112100990903     C                   MOVEL     'TR51'        CMZAZN
112200990903     C                   MOVEL     *BLANKS       CMZPRR
112300990903     C                   MOVEL     'FNARBP0R'    CMZFLA
112400990903     C                   MOVE      RECARV        CMZNRR
112500990903     C                   MOVEL     SAVLBD        CMZLBD
112600990903     C                   MOVEL     SAVLBA        CMZLBA
112700990903     C*
112800990903     C                   EXSR      CALCMZ
112900990903     C*
113000990903     C* DISALLOCO
113100021009     c                   Eval      %Subst(Cvp(2):13:8) = 'FNARBP0T'
113200040510     c                   Eval      %Subst(Cvp(2):34:1) = emme1
113300021009     C                   MOVE      VAR8          Cvp(2)
113400990903     C                   MOVEL     *BLANKS       COMMAN           80
113500021009     c                   Movel     Cvp(2)        Comman
113600990903     C                   CALL      'QCMDEXC'
113700990903     C                   PARM                    COMMAN
113800021009     C                   PARM                    LUNG1
113900990903     C                   END
114000990903     C                   END
114100990903     C*
114200990903     C                   ENDSR
114300030110      **************************************************************************
114400030110      *  SOTTOMETTE TRASMISSIONE F I A R 5 0 0 T
114500030110      **************************************************************************
114600030110     c     x25Fiar       BegSr
114700030110
114800030110     c                   If        RecFiar > *Zeros
114900030110     c                   Movel     WlbLba        CMZLBA
115000030110     c                   Movel     'FIAR500T'    CmzFld
115100030110     c                   Movel     'FIAR500R'    CmzFla
115200030110     c                   Move      RecFiar       CmzNrr
115300030110
115400030110     c                   ExSr      CalCmz
115500030110     c                   EndIf
115600030110
115700030110     c                   EndSr
115800950112     C**************************************************************************
115900991026     C*  SOTTOMETTE TRASMISSIONE F I B L P A 0 T
116000950104     C**************************************************************************
116100910926     C     X25BPA        BEGSR
116200950104     C*
116300030115     C                   MOVEL     SAVLBA        CMZLBA
116400991026     C                   MOVEL     'FIBLPA0T'    CMZFLD
116500991026     C                   MOVEL     'FIBLPA0R'    CMZFLA
116600991117     C                   MOVE      'TR36'        CMZAZN
116700910926     C                   MOVE      RECBPA        CMZNRR
116800910927     C*
116900910927     C                   EXSR      CALCMZ
117000950104     C*
117100910926     C                   ENDSR
117200000906     C**************************************************************************
117300000906     C*  SOTTOMETTE TRASMISSIONE T I T A H 0 0 T
117400000906     C*************************************+************************************
117500000906     C     X25TH         BEGSR
117600000906     C*
117700000906     C                   MOVEL     'TITAH00T'    CMZFLD
117800000906     C                   MOVEL     'TITAH00R'    CMZFLA
117900000906     C                   MOVE      RECTH         CMZNRR
118000000906     C*
118100000906     C                   EXSR      CALCMZ
118200000906     C*
118300000906     C                   ENDSR
118400950104     C**************************************************************************
118500991026     C*  SOTTOMETTE TRASMISSIONE F I A R 6 0 0 T
118600950104     C**************************************************************************
118700950112     C     X25B6         BEGSR
118800950104     C*
118900950112     C     RECB6         IFGT      0
119000991026     C                   MOVEL     'FIAR600T'    CMZFLD
119100991026     C                   MOVEL     'FIAR600R'    CMZFLA
119200950112     C                   MOVE      RECB6         CMZNRR
119300910927     C*
119400910927     C                   EXSR      CALCMZ
119500910926     C                   END
119600950104     C*
119700910926     C                   ENDSR
119800950104     C**************************************************************************
119900991026     C*  SOTTOMETTE TRASMISSIONE F I A R 7 0 0 T
120000950104     C**************************************************************************
120100950112     C     X25B7         BEGSR
120200950104     C*
120300950112     C     RECB7         IFGT      *ZERO
120400991026     C                   MOVEL     'FIAR700T'    CMZFLD
120500991026     C                   MOVEL     'FIAR700R'    CMZFLA
120600950112     C                   MOVE      RECB7         CMZNRR
120700910927     C*
120800910927     C                   EXSR      CALCMZ
120900900601     C                   END
121000950104     C*
121100900601     C                   ENDSR
121200970217     C**************************************************************************
121300970217     C*  SOTTOMETTE TRASMISSIONE F N B L 4 0 0 T
121400970217     C**************************************************************************
121500970217     C     X25B4         BEGSR
121600970217     C*
121700970217     C     RECB4         IFGT      *ZERO
121800060220     C                   MOVEL     'FIAR400T'    CMZFLD
121900060220     C                   MOVEL     'FIAR400R'    CMZFLA
122000970217     C                   MOVE      RECB4         CMZNRR
122100970217     C*
122200970217     C                   EXSR      CALCMZ
122300970217     C                   END
122400970217     C*
122500970217     C                   ENDSR
122600950104     C**************************************************************************
122700930203     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
122800950104     C**************************************************************************
122900910912     C     NUMREC        BEGSR
123000950104     C*
123100930120     C                   Z-ADD     0             NRR
123200930120     C                   CALL      'TRUL51C'
123300950112     C                   PARM                    KNSIF
123400930203     C                   PARM                    CMZFLD           10
123500021009     C                   PARM                    VARmbr           10
123600930120     C                   PARM                    NRR              10 0
123700950104     C*
123800910912     C                   ENDSR
123900950104     C**************************************************************************
124000950104     C*  CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
124100950104     C**************************************************************************
124200910927     C     CALCMZ        BEGSR
124300950104     C*
124400971113     C                   CLEAR                   W236
124500971113     C* IMPOSTO SE I 2 P.O. SONO LOCALI
124600971113     C                   MOVEL     O50LCL        CMZLCL            1
124700020927     C                   CALL      'TIBS53C'
124800971114     C                   PARM                    CMZLCL            1
124900910927     C                   PARM                    CMZDST
125000910927     C                   PARM                    CMZFLD
125100910927     C                   PARM                    CMZMBD
125200910927     C                   PARM                    CMZLBD
125300910927     C                   PARM                    CMZFLA
125400910927     C                   PARM                    CMZMBA
125500910927     C                   PARM                    CMZLBA
125600910927     C                   PARM                    CMZORI
125700910927     C                   PARM                    CMZDTI
125800910927     C                   PARM                    CMZURG
125900910927     C                   PARM                    CMZADD
126000910927     C                   PARM                    CMZCLR
126100000726     C                   PARM                    CMZFTR
126200910927     C                   PARM                    CMZERR
126300910927     C                   PARM                    CMZAZN
126400910927     C                   PARM                    CMZSYF
126500910927     C                   PARM                    CMZNRR
126600910927     C                   PARM                    CMZPRR
126700971113     C                   PARM                    W236            236
126800950104     C*
126900910927     C                   ENDSR
127000971209     C**************************************************************************
127100971209     C*  CALL AL PROGRAMMA CHE REPERISCE LIBRERIA E S.I DI TRAMISSIONE
127200971209     C**************************************************************************
127300971209     C     CALTBS        BEGSR
127400971209     C*
127500971209     C** PRELEVO LO STESSO LA FILIALE ELABORATORE COME DESTINATARIO
127600971209     C     CAETFE        CHAIN     AZORG01L                           31
127700971209     C   31              MOVEL     CAETFE        ORGFEL
127800971209     C*
127900971209     C                   CLEAR                   DSBS50
128000971209     C                   MOVEL     KNSIF         I50PSI
128100971209     C                   MOVEL     SIMFEL        I50PPO
128200971209     C                   MOVEL     CAETFE        I50APO
128300020927     C                   CALL      'TIBS55R'
128400971209     C                   PARM                    DSBS50
128500971209     C**
128600971209     C                   MOVEL     O50PLA        SAVLBD
128700971209     C                   MOVEL     O50ALA        SAVLBA
128800971209     C                   MOVEL     O50ASI        CMZSYF
128900971209     C                   MOVEL     O50PLG        WLBLBD
129000971209     C                   MOVEL     O50ALG        WLBLBA
129100971209     C                   ENDSR
129200950104     C**************************************************************************
129300930203**         CMD1
129400021010ALCOBJ OBJ((FIBLPA0T *FILE *EXCL M000xxx))
129500910912**         CMD2
129600021010DLCOBJ OBJ((FIBLPA0T *FILE *EXCL M000xxx))
129700910912**         CMD3
129800021010ALCOBJ OBJ((FIAR700T *FILE *EXCL M000xxx))
129900910912**         CMD4
130000021010DLCOBJ OBJ((FIAR700T *FILE *EXCL M000xxx))
130100920221**         C17
130200021010OVRDBF FILE(XXXXXXXX)         MBR(M000xxx)
130300910926**         CM33
130400021010ALCOBJ OBJ((FIAR600T *FILE *EXCL M000xxx))
130500910926**         CM34
130600021010DLCOBJ OBJ((FIAR600T *FILE *EXCL M000xxx))
130700930203**         C56
130800021010ALCOBJ OBJ((XXXXXXXX *FILE *EXCL MXXXxxx))
130900930203**         C57
131000021010DLCOBJ OBJ((XXXXXXXX *FILE *EXCL M000xxx))
131100021009**         Cvp
131200021009ALCOBJ OBJ((FNARBXXX *FILE *EXCL MXXXXXX))
131300021009DLCOBJ OBJ((FNARBXXX *FILE *EXCL MXXXXXX))
