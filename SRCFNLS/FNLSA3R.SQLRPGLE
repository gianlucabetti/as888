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
001200110318     Ffnorg01l  IF   E           K DISK    prefix(R_)
001300110318     Ffidst01l  IF   E           K DISK
001400110318     Ffidst02l  IF   E           K DISK    RENAME(Fidst000:Fidst002)
001500950104     F*?  T R A S M I S S I O N E    A    S E D E  ?
001600991026     FFIBLPA0T  O  A E             DISK    USROPN
001700060220     FFiar400T  O  A E             DISK    USROPN
001800060220     F                                     RENAME(Fiar4000:Fiar40TT)
001900991026     FFIAR600T  O  A E             DISK    USROPN
002000991026     F                                     RENAME(FIAR6000:FIAR60TT)
002100991026     FFIAR700T  O  A E             DISK    USROPN
002200991026     F                                     RENAME(FIAR7000:FIAR70TT)
002300150915     FTITAH00T  O  A E             DISK    USROPN
002400030110     fFiar500t  o  a e             Disk    Usropn
002500030110     f                                     Rename(Fiar5000:Fiar50tt)
002600950104     F*?  B O L L E    D I    A P P O G G I O  ?
002700070510     FFNBLP47l  UF   E           K DISK
002800930716     F*
002900970214     FFNBLP01L  IF   E           K DISK
003000970214     F                                     RENAME(FNBLP000:FNBLP001)
003100991026     FFNBLT01L  IF   E           K DISK
003200051013     FFiarS01L  IF   E           K DISK
003300991026     FFIAR601L  IF   E           K DISK
003400991026     FFIAR701L  IF   E           K DISK
003500051116     FFiar901L  IF   E           K DISK
003600060220     FFiar401L  IF   E           K DISK
003700100317     fFiar501l  uf a e           k Disk
003800950112     D*
003900110318     D tipod           S              1    DIM(2)
004000110321     D PDR             S              7  0 DIM(5500)
004100120730     d c_Elem          c                   const(500)
004200120730     D ARV             S              3  0 DIM(c_Elem)
004300120730     D ARP             S              3  0 DIM(c_Elem)
004400120730     D C7K             S              1    DIM(c_Elem)
004500120730     D C7KK            S              1    DIM(c_Elem)
004600120730     D CKB4            S              8    DIM(c_Elem)
004700021010     D CMD1            S             42    DIM(1) CTDATA PERRCD(1)
004800021010     D CMD2            S             42    DIM(1) CTDATA PERRCD(1)
004900021010     D CMD3            S             42    DIM(1) CTDATA PERRCD(1)
005000021010     D CMD4            S             42    DIM(1) CTDATA PERRCD(1)
005100021010     D C17             S              1    DIM(42) CTDATA PERRCD(42)
005200021010     D CM33            S             42    DIM(1) CTDATA PERRCD(1)
005300021010     D CM34            S             42    DIM(1) CTDATA PERRCD(1)
005400021010     D C56             S              1    DIM(42) CTDATA PERRCD(42)
005500021010     D C57             S              1    DIM(42) CTDATA PERRCD(42)
005600021009      * Comandi x FNARBV0T e FNARBP0T
005700021009     d Cvp             s             42    Dim(2) Ctdata Perrcd(1)
005800950104     D*
005900100914     D CMjob           S             42    DIM(1) CTDATA PERRCD(1)
006000111123     D CMdly           S             42    DIM(1) CTDATA PERRCD(1)
006100111123     D CMovrp          S             60    DIM(1) CTDATA PERRCD(1)
006200111123     D CMwrka          S             72    DIM(1) CTDATA PERRCD(1)
006300021009
006400021009     d                 ds
006500021009     d  Emme1                  1      1    Inz('M')
006600021009     d  $Simfel                2      4  0
006700021009     d  alf3                   5      7
006800021009     d  Var10                  1     10
006900021009
007000950112     D***
007100950112     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
007200950112     D***
007300950112     D WLBDAT          DS                  INZ
007400950112     D  G02DAT                 1      8  0
007500950112     D  G02INV                 9     16  0
007600950112     D  G02ERR                17     17
007700950112     D  G02TGI                18     22  0
007800951207     D***
007900960129     D* PARAMETRI PER TRASMISS. FNFGV00T-FVD-FV4          - FNLSA4R -
008000951207     D***
008100951207     D PARAM2          DS
008200951207     D  PA2MBD                 1     10
008300951207     D  PA2FEL                11     13  0
008400960129     D* IL CAMPO PA2ALL SE VIENE RESTITUITO PIENO SIGNIFICA CHE NON
008500960129     D*   SONO AVVENUTE LE ALLOCAZIONI DI TUTTI I FILES
008600951207     D  PA2ALL                14     14
008700960129     D* TIPO LANCIO PROGRAMMA ("C" / "L" / " ")
008800951207     D  PA2TLA                15     15
008900960129     D* PA2FLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
009000960129     D  PA2FLG                16     16    INZ
009100960129     D* I CAMPI PA2LNP E PA2NFV DEVONO ESSERE PASSATI SOLO SE PGM
009200960129     D*   RICHIAMATO DALLA GESTIONE F.VIAGGIO
009300960129     D  PA2LNP                17     19  0 INZ
009400960129     D  PA2NFV                20     24  0 INZ
009500111123     d
009600111123     D* PASSAGGIO DATI DA CHIUSURA FOGLI VIAGGIO
009700001107     D PARAM3          DS
009800001107     D  PA3NFV                 1      5
009900070423     D* pa3tfp deve contenere il terminal di partenza per poter elaborare e
010000070423     d* trasmettere le bolle per terminal di partenza
010100070423     d* se non passato un valore > '000' il programma elabora e trasmette
010200070423     d* TUTTE le bolle presenti nel file fnblp46f (come faceva prima)
010300070423     D  PA3tfp                 6      8
010400111123     D  PA3num                 9      9
010500960129     D*
010600991026     D DS7K          E DS
010700950112     D DS3A          E DS
010800000911     D DS7H          E DS
010900110805     D DS7R          E DS
011000920305     D KPJBA         E DS
011100900601     D DSCMZ         E DS
011200900517     D CNCR80        E DS
011300900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
011400020927     D DSBS50        E DS                  EXTNAME(TIBS55DS)
011500021017
011600021017     d Tibs02Ds      e ds
011700030110      * Ds Tabella AR5
011800030110     d dAr5          e ds
011900100317     d dAr5GEN       e ds
011901150916     d dAr5doc       e ds
012000110805     d** dCli          e ds
012100110318     d dsbl4M        e ds
012200110318     d dDSTFLR       e ds
012300021009
012400100914     d Wfile           s             10
012500100914     d $mbr            s              6  0
012600041122     d VAR8            s              8
012700070511     d command         S             80    INZ('DLYJOB DLY(7)')
012800070511     d lenght          S             15  5 INZ(45)
012900100317
013000110805     d** SavCCM          S              7  0 INZ(0)
013100110805     d savgma          S                   like(blpgma) inz
013200100317     d kTRD            s                   Like(ar5trd) inz('GEN')
013300110318     d kTRC            s                   Like(ar4trc) inz('M')
013400110318     d kfgs            s                   Like(dstfgs)
013500110318     d knfv            s                   Like(dstnfv)
013600110318     d kdfv            s                   Like(dstdfv)
013700110318     d knpg            s                   Like(dstnpg) inz(4)
013800110321     d $Endtab         s               n   inz(*off)
013900110321     d xx              s              4  0 inz
014000110321     d Indx            s              4  0 inz
014100110321     d FE2             s              3  0 inz
014200110321     d apdpdr          s              7  0 inz
014300100317     d
014400150122     D CoggB           C                   CONST('FNLSA3R: all-
014500100914     D                                     ocato file bolle')
014600150122     D CoggFI          C                   CONST('FNLSA3R: all-
014700150122     D                                     ocato file bolle NON TRASMETTE')
014800100914     D Cmsg            C                   CONST('Impossibile la trasmissione a-
014900150122     D                                      sede. Risottomessa -
015000111123     d                                     azione di trasmissione')
015100150122     D CmsgFI          C                   CONST('Impossibile la trasmissione a-
015200150122     D                                      sede. L''ELABORAZIONE TERMINA !!')
015300111123
015400120302     D Ceml            C                   CONST('elisa.sanfelici@brt.it')
015500900601      *---------------------------------------------------------------*
015600900601      * INDICATORI USATI
015700900601      *
015800910913      * 31 = DI COMODO
015900970214      * 33 = DI COMODO
016000910912      * 40 = NON ALLOCO LINEA--> RIPROVO UNA SECONDA VOLTA
016100910912      * 42 = INVIO MESSAGGIO DI NON ALLOCATO MEMBRI (1 VOLTA SOLA)
016200970214      * 47/54 = ERRORI IN ALLOCAZIONE
016300900601      *--------------------------------------------------------------*
016400971209     C     *ENTRY        PLIST
016500000000     C                   PARM                    KPJBA
016600040505     C                   PARM                    TRASPESVOL        1
016700040505     C
016800001107     C                   MOVEL     KPJBU         PARAM3
016900040505     C* VERIFICO SE PASSATI 1 O 2 PARAMETRI
017000040505     C                   IF        %PARMS<2
017100040505     C                   CLEAR                   WSOLOTRASM        1
017200040505     C                   ELSE
017300040505     C                   EVAL      WSOLOTRASM=TRASPESVOL
017400040505     C                   ENDIF
017500070511     c*
017600070511     c                   if        wsolotrasm=' '
017700070511      * attende qualche secondo
017800070511     c                   call      'QCMDEXC'
017900070511     c                   parm                    command
018000070511     c                   parm                    lenght
018100070511     c                   endif
018200040505     C
018300900516      *
018400000000     C                   Z-ADD     1             CODUT
018500900517     C                   CALL      'X§PARUT'
018600900517     C                   PARM                    UT§DSE
018700900517     C                   MOVEL     REC80         CNCR80
018800041115     c*
018900021009     c                   movel     simfel        $mbr
019000021009     c                   eval      $simfel = simfel
019100040505     c*
019200040505     c                   if        knsif=*blanks
019300040505     c                   eval      knsif='*LIBL'
019400040505     c                   endif
019500110322     c
019600040510     c     simfel        chain     azorg01l
019700110324     c                   if        %found(azorg01l) and wsolotrasm<>' '
019800040510     c                   movel     orgdit        emme1
019900110324     c                   endif
020000950112     C*----------------------------------------------------------------
020100971113     C*
020200920115     C                   Z-ADD     0             F                 3 0
020300950113     C                   Z-ADD     0             I                 3 0
020400990903     C                   Z-ADD     0             P                 3 0
020500920309     C*
020600930716     C*
020700910926     C* IMPOSTO ALCUNI VALORI FISSI X CREARE FILE TRASMISSIONE
020800910926     C                   EXSR      IMPO
020900110322     C* carico codici AUT da non considerare per PDA
021000110321     C                   EXSR      CarAUT
021100971209     C*
021200971111     C***********************
021300971111     C* T R A S M I S S I O N I   P E R   S E D E
021400041115     C***********************
021500041115     C                   MOVEL     046           CAETFE            3 0
021600971113     C                   MOVEL     046           ORGFEL
021700971111     C                   MOVEL     046           ALF3              3
021800021009     c                   Move      046           $mbr
021900021010     C                   MOVEL     VAR10         CMZMBD
022000971113     C*
022100971113     C* APRO MEMBRI DEL FILE PER LA TRASMISSIONE A SEDE
022200040505     C                   IF        WSOLOTRASM=' '
022300971113     C                   EXSR      CAEX
022400040505     C                   ENDIF
022500971113     C*
022600971111     C*  ESEGUO CALL PER REPERIRE LIBRERIA E S.I. DI TRAMSISSIONE
022700971111     C                   CLEAR                   DSBS50
022800971111     C                   MOVEL     KNSIF         I50PSI
022900021009     c                   Movel     Simfel        I50Ppo
023000971111     C                   MOVEL     046           I50APO
023100020927     C                   CALL      'TIBS55R'
023200971111     C                   PARM                    DSBS50
023300971111     C**
023400971209     C                   MOVEL     O50PLA        SAVLBD
023500971209     C                   MOVEL     O50ALA        SAVLBA
023600971209     C                   MOVEL     O50ALG        WLBLBA
023700971111     C                   MOVEL     O50ASI        CMZSYF
023800971111     C** TRASMISSIONE    B O L L E   P A R T I T E
023900971111     C**
024000040505     C                   IF        WSOLOTRASM=' '
024100971111     C                   EXSR      LCKSD
024200971111     C   40              Z-ADD     1             WSED              1 0
024300971111     C**
024400971111     C** TRASMISSIONE    F O G L I   V I A G G I O
024500971111     C**
024600001107     C     PA3NFV        IFNE      '99999'
024700021009     c                   movel     var10         pa2mbd
024800971113     C                   MOVEL     SIMFEL        PA2FEL
024900971111     C                   CLEAR                   PA2ALL
025000971111     C                   CLEAR                   PA2TLA
025100971111     C                   MOVEL     PARAM2        KPJBU
025200971111     C                   CALL      'FNLSA4R'
025300971111     C                   PARM                    KPJBA
025400971111     C                   MOVEL     KPJBU         PARAM2
025500971111     C     PA2ALL        IFEQ      '1'
025600971111     C                   SETON                                        40
025700110321     C                   z-add     046           FE2
025800971111     C                   ENDIF
025900001107     C                   ENDIF
026000001107     C**
026100040505     C                   ELSE
026200040505     C**
026300040505     C** TRASMISSIONE    V O L U M E   C M L
026400040505     C**
026500040505     C                   EXSR      LCKARV
026600040505     C   40              ADD       1             I
026700040505     C   40              MOVEL     CAETFE        ARV(I)
026800040505     C**
026900040505     C** TRASMISSIONE    P E S O   C M L
027000040505     C                   EXSR      LCKARP
027100040505     C   40              ADD       1             P
027200040505     C   40              MOVEL     CAETFE        ARP(P)
027300040505     C                   ENDIF
027400930929     C*
027500930929     C* RITENTO TRASMISSIONE IN SEDE SE ERA ALLOCATA
027600930929     C     WSED          IFEQ      1
027700111123     c* eseguo dly di attesa
027800111123     c                   clear                   comman
027900111123     C                   Z-ADD     42            LUNG             15 5
028000111123     C                   MOVEA     CMdly(1)      COMMAN
028100111123     C                   CALL      'QCMDEXC'
028200111123     C                   PARM                    COMMAN
028300111123     C                   PARM                    LUNG
028400021010     C                   MOVEL     VAR10         CMZMBD
028500111123     c
028600930929     C                   EXSR      LCKSD
028700100914     c
028800100914     c* Se ancora non riesco ad inviare a sede joblog e e-mail
028900100914     c                   if        *in40
029000111123     c**                 clear                   comman
029100111123     C**                 Z-ADD     60            LUNG             15 5
029200111123     C**                 MOVEA     CMovrp(1)     COMMAN
029300111123     C**                 CALL      'QCMDEXC'
029400111123     C**                 PARM                    COMMAN
029500111123     C**                 PARM                    LUNG
029600111123     c
029700111123     c**                 clear                   comman
029800111123     C**                 Z-ADD     42            LUNG             15 5
029900111123     C**                 MOVEA     CMjob(1)      COMMAN
030000111123     C**                 CALL      'QCMDEXC'
030100111123     C**                 PARM                    COMMAN
030200111123     C**                 PARM                    LUNG
030300100914     c
030400111123     c
030500150122     c                   if        pa3num>*zeros
030600100914     c                   clear                   peml            253
030700100914     c                   clear                   pogg             44
030800100914     c                   clear                   pmsg           5000
030900100914     c                   eval      peml=cEML
031000150122     C                   IF        pa3num<>'4'
031100150122     c                   eval      pogg=coggB
031200100914     c                   eval      pmsg=cmsg
031300150122     c                   else
031400150122     c                   eval      pmsg=cmsgFI
031500150122     c                   eval      pogg=coggFI
031600150122     c                   endif
031700150122     c
031800100914     c                   call      'TIS701C'
031900100914     c                   parm                    peml
032000100914     c                   parm                    pogg
032100100914     c                   parm                    pmsg
032200150122     c                   endif
032300100914     c*
032400150122     c* Risottometto azione 4 volte
032500111123     c                   select
032600111123     c                   when      pa3num<=*zeros
032700111123     c                   eval      pa3num='1'
032800111123     c
032900111123     c                   when      pa3num='1'
033000111123     c                   eval      pa3num='2'
033100111123     c                   when      pa3num='2'
033200111123     c                   eval      pa3num='3'
033300150122     c                   when      pa3num='3'
033400150122     c                   eval      pa3num='4'
033500150122     c                   when      pa3num='4'
033600150122     c                   eval      pa3num='5'
033700111123     c                   endsl
033800111123     c
033900111123     c                   if        pa3num='1' or pa3num='2'
034000150122     c                             or pa3num='3' or pa3num='4'
034100111123     c                   clear                   kpjbu
034200111123     c                   movel     param3        kpjbu
034300111123     C                   MOVEL     'LSA3'        KCOAZ
034400111123     C                   CALL      'BCH10'
034500111123     C                   PARM                    KPJBA
034600111123     c                   endif
034700100914     c
034800111123     c                   endif
034900930929     C                   ENDIF
035000950113     C**
035100930716     C* RITENTO PER QUELLI CHE NON SONO RIUSCITA DA ALLOCARE
035200950113     C**
035300950113     C* DA TRASMETTERE SOLO IN SEDE   FGV E FVD
035400001107     C     PA3NFV        IFNE      '99999'
035500001107     C***
035600110321     C     FE2           ifgt      0
035700110321     C                   MOVEL     FE2           CAETFE
035800021010     C**                 MOVEL     VAR2          CMZMBD
035900021010     C                   MOVEL     VAR10         CMZMBD
036000971111     C     CAETFE        IFEQ      046
036100021009     c                   movel     var10         pa2mbd
036200971113     C                   MOVEL     SIMFEL        PA2FEL
036300951207     C                   CLEAR                   PA2ALL
036400951207     C                   CLEAR                   PA2TLA
036500951207     C                   MOVEL     PARAM2        KPJBU
036600951207     C                   CALL      'FNLSA4R'
036700951207     C                   PARM                    KPJBA
036800930716     C                   ENDIF
036900110321     C                   endif
037000950113      *
037100950113     C                   Z-ADD     1             I
037200950113     C     ARV(I)        DOWNE     0
037300971111     C                   MOVEL     ARV(I)        CAETFE
037400971210     C                   MOVEL     CAETFE        ALF3              3
037500021009     c                   Movel     Var10         CmzMbd
037600971209     C                   EXSR      CALTBS
037700950113     C                   EXSR      LCKARV
037800950113     C                   ADD       1             I
037900950113     C                   END
038000990903      *
038100990903     C                   Z-ADD     1             P
038200990903     C     ARP(P)        DOWNE     0
038300990903     C                   MOVEL     ARP(P)        CAETFE
038400990903     C                   MOVEL     CAETFE        ALF3              3
038500021009     c                   Movel     Var10         CmzMbd
038600990903     C                   EXSR      CALTBS
038700990903     C                   EXSR      LCKARP
038800990903     C                   ADD       1             P
038900990903     C                   END
039000001205      ***
039100001205     C* RICHIAMO PGM SCRITTURA MEMBRI DI TRASMISSIONE STOP PADRONCINI
039200001205     C* NON LO FACCIO PER L'AUTOGENERAZIONE BOLLE POSTE
039300010312     C* RICHIAMO ANCHE PGM TRASMI O.R.M.
039400020917      * ELIMINATA TRASM. ORM PERCHE' SCHEDULATA
039500001205     C     PA3NFV        IFNE      '99999'
039600010320     C* RICHIAMO ANCHE PGM CHIUSURA FOGLI VARI CON SOLI O.R.M.
039700010320     C                   MOVE      'LRA9'        KCOAZ
039800010320     C                   CALL      'BCH10'
039900010320     C                   PARM                    KPJBA
040000010312     C*
040100001205     C                   MOVE      'LRC1'        KCOAZ
040200001205     C                   CALL      'BCH10'
040300001205     C                   PARM                    KPJBA
040400010312     C*
040500001205     C                   ENDIF
040600040505     C                   ENDIF
040700001205      ***
040800950104      *
040900971113     C* CHIUDO I PGM
041000971113     C                   CLEAR                   DSBS50
041100971113     C                   MOVEL     'C'           I50TLA
041200020927     C                   CALL      'TIBS55R'
041300971113     C                   PARM                    DSBS50
041400050616     c*
041500050616     C                   CLEAR                   tibs02ds
041600050616     C                   MOVEL     'C'           T02TLA
041700050616     C                   CALL      'TIBS02R'
041800050616     C                   PARM                    KPJBA
041900050616     C                   PARM                    tibs02ds
042000930512     C*
042100000000     C                   SETON                                        LR
042200950104     C**************************************************************************
042300930929     C** IMPOSTO ALCUNI DATI FISSI PER CREARE I FILE DI TRASMISSIONE
042400950104     C**************************************************************************
042500930929     C     IMPO          BEGSR
042600930929     C*
042700930929     C                   MOVE      '0000'        CMZORI
042800930929     C                   MOVE      '0000000'     CMZDTI
042900000112     C                   MOVE      'R'           CMZURG
043000930929     C                   MOVE      'A'           CMZADD
043100930929     C                   MOVE      'S'           CMZCLR
043200930929     C                   MOVE      'S'           CMZFTR
043300930929     C                   MOVE      *BLANK        CMZERR
043400950104     C*
043500930929     C                   ENDSR
043600110321     C**************************************************************************
043700110321     C** Carico codici AUT da non consdierare per PDA
043800110321     C**************************************************************************
043900110321     C     carAUT        BEGSR
044000110321      /free
044100110321         clear xx;
044200110321         $EndTab = *off;
044300110321
044400110321         exec sql
044500110321          declare aut cursor for
044600110321          select apdpdr
044700110321          from FIAPD00f where  apdtip= 'A' and
044800110321          apdfcm='S' and apdatb=' '        ;
044900110321
045000110321         exec sql
045100110321           open AUT;
045200110321           IF sqlcode < 0;
045300110321             $Endtab = *on;
045400110321           ENDIF;
045500110321
045600110321         DOW not $Endtab;
045700110321           exec sql
045800110321             fetch next from AUT into :apdpdr    ;
045900110321
046000110321             IF sqlcod = 100 or sqlcod < 0;
046100110321               $EndTab = *on;
046200110321               leave;
046300110321             ENDIF;
046400110321
046500110321               xx += 1;
046600110321               pdr(xx) = apdpdr            ;
046700110321         ENDDO;
046800110321
046900110321         exec sql close AUT;
047000110321
047100110321      /end-free
047200110321     C                   ENDSR
047300991026     C**************************************************************************
047400060220     C** TABELLA DEI TIPI RECORD FILE Fiar4 A INVIARE A SEDE
047500991026     C**************************************************************************
047600991026     C     CAR7K         BEGSR
047700991026     C                   CLEAR                   X
047800050616     C                   CLEAR                   Y
047900991026     C                   MOVEL     '7K'          COD
048000991026     C     KTAB          CHAIN     TABEL                              30
048100991026     C     *IN30         DOWEQ     *OFF
048200991026     C     TBLFLG        IFEQ      ' '
048300991026     C                   MOVEL     TBLUNI        DS7K
048400991026     C     §7KSED        IFEQ      'S'
048500991026     C                   ADD       1             X                 3 0
048600991026     C                   MOVEL     TBLKEY        C7K(X)
048700050616     c                   else
048800050616     C     §7KSED        IFEQ      'K'
048900050616     C                   ADD       1             Y                 3 0
049000050616     C                   MOVEL     TBLKEY        C7KK(Y)
049100050616     c                   endif
049200991026     C                   ENDIF
049300991026     C                   ENDIF
049400991026     C**
049500991026     C     KTAB          READE     TABEL                                  30
049600991026     C                   ENDDO
049700050616     c*
049800050616     C                   CLEAR                   Y
049900050616     c     'KB4'         setll     tntbe01l
050000050616     c                   do        *hival
050100050616     c     'KB4'         reade     tntbe01l
050200050617     c                   if        %eof(tntbe01l)
050300050616     c                   leave
050400050616     c                   endif
050500050617     c                   if        tbeatb =  *blanks
050600050616     c                   add       1             y
050700050616     c                   movel     tbeke1        CKB4(Y)
050800050617     c                   eval      %subst(ckb4(y):8:1) = %subst(tbeke2:1:1)
050900050616     c                   endif
051000050616     c                   enddo
051100991026     C                   ENDSR
051200950104     C**************************************************************************
051300950104     C*  ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE
051400950104     C**************************************************************************
051500930929     C     CAEX          BEGSR
051600930929     C*
051700080908     C                   MOVEL     'FIBLPA0T'    FILE             10
051800930929     C                   CALL      'TRUL50C'
051900930929     C                   PARM                    FILE
052000021010     C                   PARM                    VAR10
052100950112     C                   PARM                    KNSIF
052200930929     C                   PARM                    LOGICO           10
052300930929     C                   PARM                    LOGMBR           10
052400930929     C                   PARM                    FLG               1
052500930929     C*
052600991026     C                   MOVEL     'FIAR600T'    FILE
052700930929     C                   CALL      'TRUL50C'
052800930929     C                   PARM                    FILE
052900021010     C                   PARM                    VAR10
053000950112     C                   PARM                    KNSIF
053100930929     C                   PARM                    LOGICO           10
053200930929     C                   PARM                    LOGMBR           10
053300930929     C                   PARM                    FLG               1
053400930929     C*
053500991026     C                   MOVEL     'FIAR700T'    FILE
053600930929     C                   CALL      'TRUL50C'
053700930929     C                   PARM                    FILE
053800021010     C                   PARM                    VAR10
053900950112     C                   PARM                    KNSIF
054000930929     C                   PARM                    LOGICO           10
054100930929     C                   PARM                    LOGMBR           10
054200930929     C                   PARM                    FLG               1
054300951120     C*
054400060220     C                   MOVEL     'FIAR400T'    FILE
054500970214     C                   CALL      'TRUL50C'
054600970214     C                   PARM                    FILE
054700021010     C                   PARM                    VAR10
054800970214     C                   PARM                    KNSIF
054900970214     C                   PARM                    LOGICO           10
055000970214     C                   PARM                    LOGMBR           10
055100970214     C                   PARM                    FLG               1
055200000905     C*
055300000905     C                   MOVEL     'TITAH00T'    FILE
055400000905     C                   CALL      'TRUL50C'
055500000905     C                   PARM                    FILE
055600021010     C                   PARM                    VAR10
055700000905     C                   PARM                    KNSIF
055800000905     C                   PARM                    LOGICO           10
055900000905     C                   PARM                    LOGMBR           10
056000000905     C                   PARM                    FLG               1
056100030110
056200030110     c                   Movel     'FIAR500T'    File
056300030110     c                   Call      'TRUL50C'
056400030110     c                   Parm                    File
056500030110     c                   Parm                    Var10
056600030110     c                   Parm                    Knsif
056700030110     c                   Parm                    Logico           10
056800030110     c                   Parm                    Logmbr           10
056900030110     c                   Parm                    Flg               1
057000970214     C*
057100930929     C                   ENDSR
057200950104     C**************************************************************************
057300950104     C*  ALLOCAZIONE MEMBRI PER LA SEDE
057400950104     C**************************************************************************
057500930929     C     LCKSD         BEGSR
057600950104     C*
057700930929     C                   SETOFF                                       404748
057800930929     C                   SETOFF                                       495253
057900030110     C                   SETOFF                                       545556
058000930929     C*
058100021010     C                   MOVE      '))'          VAR               8
058200021010     C                   Z-ADD     42            LUNG             15 5
058300021010     C                   MOVE      046           $mbr
058400021010     C                   MOVEL     $mbr          VAR
058500930929     C                   MOVE      VAR           CMD1
058600930929     C                   MOVEL     *BLANKS       COMMAN           80
058700930929     C                   MOVEA     CMD1(1)       COMMAN
058800930929     C                   CALL      'QCMDEXC'                            47
058900930929     C                   PARM                    COMMAN
059000930929     C                   PARM                    LUNG
059100930929     C*
059200930929     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
059300100914     c                   if        *in47
059400100914     C                   SETON                                          40
059500100914     c                   eval      wfile='FIBLPA0T'
059600100914     c                   exsr      stampaalloc
059700100914     c                   endif
059800100914     c
059900930929     C*
060000991026    1C  N47              DO
060100991026     C                   MOVE      VAR           CM33
060200930929     C                   MOVEL     *BLANKS       COMMAN           80
060300930929     C                   MOVEA     CM33(1)       COMMAN
060400930929     C                   CALL      'QCMDEXC'                            53
060500930929     C                   PARM                    COMMAN
060600930929     C                   PARM                    LUNG
060700930929     C* GIA' ALLOCATO RITENTO PIU' TARDI
060800100914     c                   if        *in53
060900100914     C                   SETON                                        40
061000100914     c                   eval      wfile='FIAR600T'
061100100914     c                   exsr      stampaalloc
061200100914     c                   endif
061300930929     C*
061400991026    2C  N53              DO
061500991026     C                   MOVE      VAR           CMD3
061600930929     C                   MOVEL     *BLANKS       COMMAN           80
061700930929     C                   MOVEA     CMD3(1)       COMMAN
061800930929     C                   CALL      'QCMDEXC'                            48
061900930929     C                   PARM                    COMMAN
062000930929     C                   PARM                    LUNG
062100930929     C* GIA' ALLOCATO RITENTO PIU' TARDI
062200100914     c                   if        *in48
062300100914     C                   SETON                                        40
062400100914     c                   eval      wfile='FIAR700T'
062500100914     c                   exsr      stampaalloc
062600100914     c                   endif
062700930929     C*
062800991026    3C  N48              DO
062900021010     C                   MOVEL     simfel        W003AA            3
063000021010     C                   MOVEL     046           W003A             3
063100060220     C                   MOVEA     'FIAR400T'    C56(13)
063200021010     C                   MOVEA     W003AA        C56(35)
063300021010     C                   MOVEA     W003A         C56(38)
063400970214     C                   MOVEL     *BLANKS       COMMAN           80
063500970214     C                   MOVEA     C56           COMMAN
063600970214     C                   CALL      'QCMDEXC'                            54
063700970214     C                   PARM                    COMMAN
063800970214     C                   PARM                    LUNG
063900100914     C* GIA' ALLOCATO RITENTO PIU' TARDI
064000100914     c                   if        *in54
064100100914     C                   SETON                                        40
064200100914     c                   eval      wfile='FIAR400T'
064300100914     c                   exsr      stampaalloc
064400100914     c                   endif
064500970214     C*
064600991026    5C  N54              DO
064700000905     C                   MOVEA     'TITAH00T'    C56(13)
064800021010     C                   MOVEA     W003AA        C56(35)
064900021010     C                   MOVEA     W003A         C56(38)
065000000905     C                   MOVEL     *BLANKS       COMMAN           80
065100000905     C                   MOVEA     C56           COMMAN
065200000905     C                   CALL      'QCMDEXC'                            55
065300000905     C                   PARM                    COMMAN
065400000905     C                   PARM                    LUNG
065500000905     C* GIA' ALLOCATO RITENTO PIU' TARDI
065600100914     c                   if        *in55
065700100914     C                   SETON                                        40
065800100914     c                   eval      wfile='TITAH00T'
065900100914     c                   exsr      stampaalloc
066000100914     c                   endif
066100030110
066200030110   5bc  n55              Do
066300030110     c                   Movel     Simfel        w003aa
066400030110     c                   Movel     046           w003a
066500030110     c                   Movea     'FIAR500T'    c56(13)
066600030110     c                   Movea     w003aa        c56(35)
066700030110     c                   Movea     w003a         c56(38)
066800030110     c                   Movel     *Blanks       Comman
066900030110     c                   Movea     c56           Comman
067000030110     c                   Call      'QCMDEXC'                            56
067100030110     c                   Parm                    Comman
067200030110     c                   Parm                    Lung
067300030110      * Già allocato ritento più tardi
067400100914     c                   if        *in56
067500100914     C                   SETON                                        40
067600100914     c                   eval      wfile='FIAR500T'
067700100914     c                   exsr      stampaalloc
067800100914     c                   endif
067900991026     C*
068000030110    6C  N56              DO
068100930929     C*
068200930929     C* S C R I V O   M E M B R I  D I   S E D E
068300930929     C                   EXSR      WTRSED
068400021009
068500021010     c                   Movel     Var10         VarMbr
068600930929     C*
068700930929     C* VEDO SE CONTIENE RECORD FLBLP
068800991026     C                   MOVEL     'FIBLPA0T'    CMZFLD
068900930929     C                   EXSR      NUMREC
069000930929     C                   Z-ADD     NRR           RECBPA           10 0
069100930929     C* I N V I O
069200991026    7C     RECBPA        IFGT      0
069300930929     C*
069400991026     C                   MOVEL     'FIAR600T'    CMZFLD
069500930929     C                   EXSR      NUMREC
069600950112     C                   Z-ADD     NRR           RECB6            10 0
069700930929     C*
069800991026     C                   MOVEL     'FIAR700T'    CMZFLD
069900930929     C                   EXSR      NUMREC
070000950112     C                   Z-ADD     NRR           RECB7            10 0
070100970217     C*
070200060220     C                   MOVEL     'FIAR400T'    CMZFLD
070300970217     C                   EXSR      NUMREC
070400970217     C                   Z-ADD     NRR           RECB4            10 0
070500000905     C*
070600000905     C                   MOVEL     'TITAH00T'    CMZFLD
070700000905     C                   EXSR      NUMREC
070800000905     C                   Z-ADD     NRR           RECTH            10 0
070900030110
071000030110     c                   Movel     'FIAR500T'    CmzFld
071100030110     c                   ExSr      Numrec
071200030110     c                   Z-Add     Nrr           RecFiar          10 0
071300930929     C*
071400930929     C                   EXSR      X25SD
071500930929     C*
071600991026    7C                   END
071700930929     C**
071800021010     C                   MOVEL     simfel        W003AA            3
071900021010     C                   MOVEL     046           W003A
072000030110
072100030110      * Disalloco FIAR5
072200030110     c                   Movea     'FIAR500T'    c57(13)
072300030110     c                   Movea     w003aa        c57(35)
072400030110     c                   Movea     w003a         c57(38)
072500030110     c                   Movel     *Blanks       Comman
072600030110     c                   Movea     c57           Comman
072700030110     c                   Call      'QCMDEXC'                            47
072800030110     c                   Parm                    Comman
072900030110     c                   Parm                    Lung
073000030110    bC                   EndDo
073100021010     c*
073200030110     C* DISALLOCO TITAH
073300000905     C                   MOVEA     'TITAH00T'    C57(13)
073400021010     C                   MOVEA     W003AA        C57(35)
073500021010     C                   MOVEA     W003A         C57(38)
073600000905     C                   MOVEL     *BLANKS       COMMAN           80
073700000905     C                   MOVEA     C57           COMMAN
073800000905     C                   CALL      'QCMDEXC'                            47
073900000905     C                   PARM                    COMMAN
074000000905     C                   PARM                    LUNG
074100991026    6C                   END
074200060220     C* DISALLOCO Fiar4
074300060220     C                   MOVEA     'FIAR400T'    C57(13)
074400021010     C                   MOVEA     W003AA        C57(35)
074500021010     C                   MOVEA     W003A         C57(38)
074600991026     C                   MOVEL     *BLANKS       COMMAN           80
074700991026     C                   MOVEA     C57           COMMAN
074800991026     C                   CALL      'QCMDEXC'                            47
074900991026     C                   PARM                    COMMAN
075000991026     C                   PARM                    LUNG
075100991026    5C                   END
075200930929     C**
075300991026     C* DISALLOCO FIAR7
075400991026     C                   MOVE      VAR           CMD4
075500930929     C                   MOVEL     *BLANKS       COMMAN           80
075600930929     C                   MOVEA     CMD4(1)       COMMAN
075700930929     C                   CALL      'QCMDEXC'                            47
075800930929     C                   PARM                    COMMAN
075900930929     C                   PARM                    LUNG
076000991026    3C                   END
076100930929     C**
076200991026     C* DISALLOCO FNBLP
076300991026     C                   MOVE      VAR           CM34
076400930929     C                   MOVEL     *BLANKS       COMMAN           80
076500930929     C                   MOVEA     CM34(1)       COMMAN
076600930929     C                   CALL      'QCMDEXC'                            47
076700930929     C                   PARM                    COMMAN
076800930929     C                   PARM                    LUNG
076900991026    2C                   END
077000110322     C*
077100991026     C                   MOVE      VAR           CMD2
077200930929     C                   MOVEL     *BLANKS       COMMAN           80
077300930929     C                   MOVEA     CMD2(1)       COMMAN
077400930929     C                   CALL      'QCMDEXC'                            47
077500930929     C                   PARM                    COMMAN
077600930929     C                   PARM                    LUNG
077700991026    1C                   END
077800930929     C*
077900930929     C                   ENDSR
078000100914     C**************************************************************************
078100100914     c* Stampa pagina di allocazione
078200100914     C**************************************************************************
078300100914     c     Stampaalloc   BEGSR
078400100914     c                   eval      lung=70
078500100914     C                   MOVEL     *BLANKS       COMMAN           80
078600100914     C                   MOVEA     CMwrka(1)     COMMAN
078700100914     c                   eval      %subst(comman:15:10)=wfile
078800100914     c                   eval      %subst(comman:47:06)=%editc($mbr:'X')
078900100914     C                   CALL      'QCMDEXC'                            53
079000100914     C                   PARM                    COMMAN
079100100914     C                   PARM                    LUNG
079200100914     c                   eval      lung=42
079300100914     c                   ENDSR
079400950104     C**************************************************************************
079500950104     C*  SCRIVO MEMBRO PER LA SEDE
079600950104     C**************************************************************************
079700910913     C     WTRSED        BEGSR
079800950104     C*
079900910926     C* APERTURA MEMBRI
080000910926     C                   EXSR      CAEXOV
080100060220     C* CARICO TIPI RECORD Fiar4 DA INVARE A SEDE + tabella dei clienti
080200050616     c* abilitati all'invio di tipi record di regola non trasmessi
080300991026     C                   EXSR      CAR7K
080400070423     c                   clear                   wtfp              3 0
080500070423     c                   if        pa3tfp>'000'
080600070423     c                   move      pa3tfp        wtfp
080700070423     c                   endif
080800910913     C*
080900070423     c                   if        wtfp>0
081000070423     c     wtfp          setll     fnblp000
081100070423     c     wtfp          reade     fnblp000                               31
081200070423     c                   else
081300070423     C                   READ      FNBLP000                               31
081400070423     c                   endif
081500910913     C*
081600950112     C     *IN31         DOWEQ     *OFF
081700991026     C     KBLP          CHAIN     FNBLP001                           33
081800991026     C*
081900991026     C     KBLP          CHAIN     FIAR6000                           33
082000991026     C     *IN33         DOWEQ     *OFF
082100991026     C                   WRITE     FIAR60TT
082200991026     C     KBLP          READE     FIAR6000                               33
082300991026     C                   ENDDO
082400991026     C**
082500991026     C* ESTENSIONE TASSAZIONE
082600991026     C**
082700991026     C     KBLP          CHAIN     FIAR7000                           33
082800991026     C     *IN33         DOWEQ     *OFF
082900991026     C                   WRITE     FIAR70TT
083000991026     C*
083100991026     C     KBLP          READE     FIAR7000                               33
083200991026     C                   ENDDO
083300991026     C**
083400060220     C* ESTENSIONE BOLLA Fiar4
083500991026     C**
083600060220     C     KBLP          CHAIN     Fiar4000                           33
083700991026     C     *IN33         DOWEQ     *OFF
083800991026     C*SOLO I TIPI RECORD CHE MI SERVONO IN SEDE
083900060220     C     ar4TRC        LOOKUP    C7K                                    32
084000060220     C   32              WRITE     Fiar40TT
084100050616     c* Vedo se tipo record da inviare solo su abilitazione del cliente
084200050616    3c                   if        *in32 = *off
084300060220     C     ar4TRC        LOOKUP    C7KK                                   32
084400050616     c* vedo se cliente presente in tabella "KB4"
084500050616    4c                   if        *in32
084600050616    5c                   if        blpccm > 0
084700050617     c                   Movel(p)  blpccm        wcodmit           8
084800050616   x5c                   else
084900050616     c                   move      blpksc        w0040             4 0
085000050616    6c                   if        w0040 <> 8888 and w0040 <> 9999
085100050617     c                   Movel(p)  blpksc        wcodmit
085200050616   x6c                   else
085300050617     c                   clear                   wcodmit
085400050616    6c                   endif
085500050616    5c                   endif
085600050617    5c                   if        wcodmit > *zeros
085700060220     c                   move      ar4trc        wcodmit
085800050617     c     wcodmit       lookup    CKB4                                   32
085900050617    6c                   If        *in32
086000060220     C                   WRITE     Fiar40TT
086100050616    6c                   endif
086200050616    5c                   endif
086300050616    4c                   endif
086400050616    3c                   endif
086500991026     C*
086600060220     C     KBLP          READE     Fiar4000                               33
086700991026     C                   ENDDO
086800991026     C**
086900051116     C* C/ASSEGNO        Fiar9
087000991026     C**
087100051116     C     KBLP          CHAIN     Fiar9000                           33
087200991026     C     *IN33         IFEQ      *ON
087300991026     C                   CLEAR                   BL9TIC
087400991026     C                   CLEAR                   BL9CAS
087500991026     C                   CLEAR                   BL9VCA
087600991026     C                   CLEAR                   BL9CMB
087700991026     C                   CLEAR                   BL9GCA
087800051116     c                   else
087900051116     c                   eval      bl9tic=ar9tic
088000051116     c                   eval      bl9cas=ar9cas
088100051116     c                   eval      bl9vca=ar9vca
088200051116     c                   eval      bl9cmb=ar9cmb
088300051116     c                   eval      bl9gca=ar9gca
088400991026     C                   ENDIF
088500991026     C**
088600080424     C* DETTAGLIO SEGNACOLLI FNBLT - per titah
088700991026     C**
088800000906     C     KBLP          CHAIN     FNBLT000                           33
088900000906     C     *IN33         DOWEQ     *OFF
089000000906     C                   EXSR      WTRTAH
089100000906     C*                    - - - - - -
089200000906     C     KBLP          READE     FNBLT000                               33
089300000906     C                   ENDDO
089400991026     C**
089500000127     C                   MOVE      BLPKSC        KSC4              4 0
089600000127     C     KSC4          IFEQ      9999
089700000127     C                   MOVEL     BLPLNA        BLPKSC
089800000127     C                   ENDIF
089900030113
090000030113      *  Estensione Testata Bolla    F I A R 5 0 0 F
090100100317
090200100317     c* Prima di scrivere i record per le SEDe, verifico se devo creare
090300100317     c*  il FIAR5 GEN per la restituzione del DDT - imposto la data §ar5DRDT
090400100317     c                   exsr      ContrDCLI
090500110318     c* Verifico anche se il ritiro è stato fatto con PDA
090600110321     c*  non elaboro se padroncino dii merce portata a magazzino
090700110321     c                   eval      Indx=%lookup(blppdr:pdr)
090800110321     c                   if        Indx=0
090900110318     c                   exsr      ContrPDA
091000110321     c                   endif
091100100317
091200030113     c     Kblp          Setll     Fiar501l
091300030113Do  2c                   Do        *Hival
091400030113     c     Kblp          Reade     Fiar501l
091500030113      * Fine File esco dal ciclo
091600030113     c                   If        %Eof(Fiar501l)
091700030113     c                   Leave
091800030113     c                   EndIf
091900100317     c
092000030113      * Controllo se già inviato
092100030113     c                   If        Ar5Dt2 <> *All'9' and Ar5Dt2 > *zeros
092200030113     c                   Iter
092300030113     c                   EndIf
092400030113      * Invio alla sede solo se il flag in tabella lo prevede
092500030113     c                   Clear                   Tibs02Ds
092600030113     c                   Clear                   dAr5
092700030113     c                   Eval      T02Mod = 'C'
092800030113     c                   Eval      T02Sif = Knsif
092900030113     c                   Eval      T02Cod = 'AR5'
093000030113     c                   Movel(p)  Ar5Trd        T02Ke1
093100030113     c                   Call      'TIBS02R'
093200030113     c                   Parm                    Kpjba
093300030113     c                   Parm                    Tibs02Ds
093400030113     c                   If        T02Err = *Blanks
093500030113     c                   Movel     T02Uni        dAr5
093600030113     c                   EndIf
093700030113     c                   If        §Ar5Trse = 'S'
093800030113     c                   Write     Fiar50tt
093900030113     c                   EndIf
094000030113      * Flaggo per la trasmissione anche se il record non viene trasmesso
094100030113     c                   Eval      Ar5Ft2 = 'T'
094200030113     c                   Z-Add     Numtra        Ar5Dt2
094300030113     c                   Update    Fiar5000
094400030113    2c                   EndDo
094500000127     C*
094600991026     C                   WRITE     FIBLPA00
094700910926     C*
094800070423     C                   DELETE    FNBLP000
094900910913     C*
095000070423     c                   if        wtfp>0
095100070423     c     wtfp          reade     fnblp000                               31
095200070423     c                   else
095300070423     C                   READ      FnBLP000                               31
095400070423     c                   endif
095500950112     C                   ENDDO
095600910913     C* CHIUDO MEMBRI
095700991026     C                   CLOSE     FIBLPA0T
095800991026     C                   CLOSE     FIAR600T
095900991026     C                   CLOSE     FIAR700T
096000060220     C                   CLOSE     Fiar400T
096100000906     C                   CLOSE     TITAH00T
096200030110     c                   Close     Fiar500t
096300910913     C*
096400910913     C                   ENDSR
096500000906     C**************************************************************************
096600000906     C*  SCRIVO TITAH00T
096700000906     C**************************************************************************
096800000906     C     WTRTAH        BEGSR
096900000906     C*
097000051013     C     KSEGNA        SETLL     FiarS01L
097100000906     C                   DO        *HIVAL
097200051013     C     KSEGNA        READE     FiarS01L                               37
097300000906     C   37              LEAVE
097400000906     C*
097500000911     C*  CONTROLLO IN BASE AL TIPO RECORD SE E' DA INVIARE IN SEDE
097600000911     C*
097700000911     C                   MOVEL     '7H'          COD$
097800051013     C                   MOVEL(P)  arSTRC        KEY$
097900000911     C                   CLEAR                   DS7H
098000000911     C     KTABEL        CHAIN     TABEL00F                           39
098100000911     C  N39              MOVEL     TBLUNI        DS7H
098200000911     C*
098300000911     C     §7HFLT        IFNE      'S'
098400000911     C                   ITER
098500000911     C                   ENDIF
098600000911     C*
098700000906     C                   MOVE      BLPAAS        TAHAAS
098800000906     C                   MOVE      BLPLNP        TAHLNP
098900000906     C                   MOVE      BLPNRS        TAHNRS
099000000906     C                   MOVE      BLPNSP        TAHNSP
099100051013     C                   MOVE      arSTRC        TAHTRC
099200051013     C                   MOVE      arSNOT        TAHNOT
099300010525     C                   Z-ADD     BLTNSC        TAHNSC
099400000906     C*
099500000906     C                   WRITE     TITAH000
099600000906     C*
099700000906     C                   ENDDO
099800000906     C*
099900000906     C                   ENDSR
100000950104     C**************************************************************************
100100950104     C*  ESEGUE CALL QCAEXEC PER OVRDBF SUI FILE
100200950104     C**************************************************************************
100300910926     C     CAEXOV        BEGSR
100400950104     C*
100500911108     C                   MOVE      ')'           VAR4              4
100600971111     C                   MOVEL     046           VAR4
100700021010     C                   MOVEL     simfel        w003aa
100800021010     C                   Z-ADD     42            LUNG
100900910913     C*
101000910926     C** OVRDBF
101100021010     C                   MOVEA     W003aa        C17(36)
101200021010     C                   MOVEA     VAR4          C17(39)
101300991026     C                   MOVEA     'FIBLPA0T'    C17(13)
101400910926     C                   MOVEL     *BLANKS       COMMAN
101500920221     C                   MOVEA     C17           COMMAN
101600910926     C                   CALL      'QCMDEXC'
101700910926     C                   PARM                    COMMAN
101800910926     C                   PARM                    LUNG
101900910926     C*
102000991026     C                   MOVEA     'FIAR600T'    C17(13)
102100910913     C                   MOVEL     *BLANKS       COMMAN
102200920221     C                   MOVEA     C17           COMMAN
102300910913     C                   CALL      'QCMDEXC'
102400910913     C                   PARM                    COMMAN
102500910913     C                   PARM                    LUNG
102600910913     C*
102700991026     C                   MOVEA     'FIAR700T'    C17(13)
102800910913     C                   MOVEL     *BLANKS       COMMAN
102900920221     C                   MOVEA     C17           COMMAN
103000910913     C                   CALL      'QCMDEXC'
103100910913     C                   PARM                    COMMAN
103200910913     C                   PARM                    LUNG
103300970214     C*
103400060220     C                   MOVEA     'FIAR400T'    C17(13)
103500970214     C                   MOVEL     *BLANKS       COMMAN
103600970214     C                   MOVEA     C17           COMMAN
103700970214     C                   CALL      'QCMDEXC'
103800970214     C                   PARM                    COMMAN
103900970214     C                   PARM                    LUNG
104000000905     C*
104100000905     C                   MOVEA     'TITAH00T'    C17(13)
104200000905     C                   MOVEL     *BLANKS       COMMAN
104300000905     C                   MOVEA     C17           COMMAN
104400000905     C                   CALL      'QCMDEXC'
104500000905     C                   PARM                    COMMAN
104600000905     C                   PARM                    LUNG
104700030110
104800030110     c                   Movea     'FIAR500T'    c17(13)
104900030110     c                   Movel     *Blanks       Comman
105000030110     c                   Movea     c17           Comman
105100030110     c                   Call      'QCMDEXC'
105200030110     c                   Parm                    Comman
105300030110     c                   Parm                    Lung
105400910913     C*
105500910913     C* APERTURA FILE
105600991026     C                   OPEN      FIBLPA0T
105700991026     C                   OPEN      FIAR700T
105800991026     C                   OPEN      FIAR600T
105900060220     C                   OPEN      Fiar400T
106000000905     C                   OPEN      TITAH00T
106100030110     c                   Open      Fiar500t
106200950104     C*
106300910913     C                   ENDSR
106400950104     C**************************************************************************
106500950104     C*  PREPARA SOTTOMISSIONE TRASMISSIONE PER LA SEDE
106600950104     C**************************************************************************
106700910926     C     X25SD         BEGSR
106800910926     C*
106900930203     C                   MOVE      '046'         CMZDST
107000971209     C                   MOVEL     SAVLBD        CMZLBD
107100971209     C                   MOVEL     SAVLBA        CMZLBA
107200021010     c                   Movel     Var10         CmzMba
107300910926     C* SE CI SONO RECORD TRASMETTO
107400910926     C     RECBPA        IFGT      0
107500911217     C                   MOVE      *BLANK        CMZPRR
107600930203     C                   MOVE      *BLANK        CMZAZN
107700970217     C                   EXSR      X25B4
107800991026     C                   EXSR      X25B7
107900991026     C                   EXSR      X25B6
108000000906     C                   EXSR      X25TH
108100030110     c                   ExSr      x25Fiar
108200991026     C                   EXSR      X25BPA
108300910926     C                   END
108400950104     C*
108500910926     C                   ENDSR
108600950113     C**************************************************************************
108700991026     C*  ALLOCAZIONE MEMBRI FILE VOLUME CML
108800950113     C**************************************************************************
108900950113     C     LCKARV        BEGSR
109000950113     C*
109100950113     C                   SETOFF                                       40
109200950113     C* VEDO SE CONTIENE RECORD
109300950113     C                   MOVEL     'FNARBV0T'    CMZFLD
109400021009     c                   Movel     Var10         VarMbr
109500950113     C                   EXSR      NUMREC
109600950113     C                   Z-ADD     NRR           RECARV           10 0
109700021009     c                   Movel     Var10         CmzMba
109800021009     c                   Movel     Var10         CmzMbd
109900950113     C* ALLOCO MEMBRO
110000950113     C     RECARV        IFGT      0
110100950113     C     RECARV        IFNE      *ALL'9'
110200021009     C                   MOVE      '))'          VAR8
110300041122     C                   Z-ADD     42            LUNG1            15 5
110400021009     c                   Eval      %Subst(Cvp(1):13:8) = 'FNARBV0T'
110500040510     c                   Eval      %Subst(Cvp(1):34:1) = emme1
110600021009     C                   MOVEL     $mbr          VAR8
110700021009     C                   MOVE      VAR8          Cvp(1)
110800950113     C                   MOVEL     *BLANKS       COMMAN           80
110900021009     c                   Movel     Cvp(1)        Comman
111000950113     C                   CALL      'QCMDEXC'                            40
111100950113     C                   PARM                    COMMAN
111200021009     C                   PARM                    LUNG1
111300950113     C*
111400950113     C                   ELSE
111500950113     C                   SETON                                          40
111600950113     C                   END
111700950113     C*
111800950113     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
111900950113     C*
112000950113     C  N40              DO
112100950113     C* INVIO
112200950113     C                   MOVE      ORGFEL        CMZDST
112300971113     C                   MOVEL     'TR47'        CMZAZN
112400950113     C                   MOVEL     *BLANKS       CMZPRR
112500950113     C                   MOVEL     'FNARBV0R'    CMZFLA
112600950113     C                   MOVE      RECARV        CMZNRR
112700971209     C                   MOVEL     SAVLBD        CMZLBD
112800971209     C                   MOVEL     SAVLBA        CMZLBA
112900950113     C*
113000950113     C                   EXSR      CALCMZ
113100950113     C*
113200950113     C* DISALLOCO
113300021009     c                   Eval      %Subst(Cvp(2):13:8) = 'FNARBV0T'
113400040510     c                   Eval      %Subst(Cvp(2):34:1) = emme1
113500021009     C                   MOVE      VAR8          Cvp(2)
113600950113     C                   MOVEL     *BLANKS       COMMAN           80
113700021009     c                   Movel     Cvp(2)        Comman
113800950113     C                   CALL      'QCMDEXC'
113900950113     C                   PARM                    COMMAN
114000021009     C                   PARM                    LUNG1
114100950113     C                   END
114200950113     C                   END
114300950113     C*
114400950113     C                   ENDSR
114500990903     C**************************************************************************
114600990903     C*  ALLOCAZIONE MEMBRI FILE PESO CML
114700990903     C**************************************************************************
114800990903     C     LCKARP        BEGSR
114900990903     C*
115000990903     C                   SETOFF                                       40
115100990903     C* VEDO SE CONTIENE RECORD
115200990903     C                   MOVEL     'FNARBP0T'    CMZFLD
115300021009     c                   Movel     Var10         VarMbr
115400990903     C                   EXSR      NUMREC
115500990903     C                   Z-ADD     NRR           RECARV           10 0
115600021009     c                   Movel     Var10         CmzMba
115700021009     c                   Movel     Var10         CmzMbd
115800990903     C* ALLOCO MEMBRO
115900990903     C     RECARV        IFGT      0
116000990903     C     RECARV        IFNE      *ALL'9'
116100021009     C                   MOVE      '))'          VAR8
116200021009     C                   Z-ADD     42            LUNG1
116300021009     c                   Eval      %Subst(Cvp(1):13:8) = 'FNARBP0T'
116400040510     c                   Eval      %Subst(Cvp(1):34:1) = emme1
116500021009     C                   MOVEL     $mbr          VAR8
116600021009     C                   MOVE      VAR8          Cvp(1)
116700990903     C                   MOVEL     *BLANKS       COMMAN           80
116800021009     c                   Movel     Cvp(1)        Comman
116900990903     C                   CALL      'QCMDEXC'                            40
117000990903     C                   PARM                    COMMAN
117100021009     C                   PARM                    LUNG1
117200990903     C*
117300990903     C                   ELSE
117400990903     C                   SETON                                          40
117500990903     C                   END
117600990903     C*
117700990903     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
117800990903     C*
117900990903     C  N40              DO
118000990903     C* INVIO
118100990903     C                   MOVE      ORGFEL        CMZDST
118200990903     C                   MOVEL     'TR51'        CMZAZN
118300990903     C                   MOVEL     *BLANKS       CMZPRR
118400990903     C                   MOVEL     'FNARBP0R'    CMZFLA
118500990903     C                   MOVE      RECARV        CMZNRR
118600990903     C                   MOVEL     SAVLBD        CMZLBD
118700990903     C                   MOVEL     SAVLBA        CMZLBA
118800990903     C*
118900990903     C                   EXSR      CALCMZ
119000990903     C*
119100990903     C* DISALLOCO
119200021009     c                   Eval      %Subst(Cvp(2):13:8) = 'FNARBP0T'
119300040510     c                   Eval      %Subst(Cvp(2):34:1) = emme1
119400021009     C                   MOVE      VAR8          Cvp(2)
119500990903     C                   MOVEL     *BLANKS       COMMAN           80
119600021009     c                   Movel     Cvp(2)        Comman
119700990903     C                   CALL      'QCMDEXC'
119800990903     C                   PARM                    COMMAN
119900021009     C                   PARM                    LUNG1
120000990903     C                   END
120100990903     C                   END
120200990903     C*
120300990903     C                   ENDSR
120400030110      **************************************************************************
120500030110      *  SOTTOMETTE TRASMISSIONE F I A R 5 0 0 T
120600030110      **************************************************************************
120700030110     c     x25Fiar       BegSr
120800030110
120900030110     c                   If        RecFiar > *Zeros
121000030110     c                   Movel     WlbLba        CMZLBA
121100030110     c                   Movel     'FIAR500T'    CmzFld
121200030110     c                   Movel     'FIAR500R'    CmzFla
121300030110     c                   Move      RecFiar       CmzNrr
121400030110
121500030110     c                   ExSr      CalCmz
121600030110     c                   EndIf
121700030110
121800030110     c                   EndSr
121900950112     C**************************************************************************
122000991026     C*  SOTTOMETTE TRASMISSIONE F I B L P A 0 T
122100950104     C**************************************************************************
122200910926     C     X25BPA        BEGSR
122300950104     C*
122400030115     C                   MOVEL     SAVLBA        CMZLBA
122500991026     C                   MOVEL     'FIBLPA0T'    CMZFLD
122600991026     C                   MOVEL     'FIBLPA0R'    CMZFLA
122700991117     C                   MOVE      'TR36'        CMZAZN
122800910926     C                   MOVE      RECBPA        CMZNRR
122900910927     C*
123000910927     C                   EXSR      CALCMZ
123100950104     C*
123200910926     C                   ENDSR
123300000906     C**************************************************************************
123400000906     C*  SOTTOMETTE TRASMISSIONE T I T A H 0 0 T
123500000906     C*************************************+************************************
123600000906     C     X25TH         BEGSR
123700000906     C*
123800000906     C                   MOVEL     'TITAH00T'    CMZFLD
123900000906     C                   MOVEL     'TITAH00R'    CMZFLA
124000000906     C                   MOVE      RECTH         CMZNRR
124100000906     C*
124200000906     C                   EXSR      CALCMZ
124300000906     C*
124400000906     C                   ENDSR
124500950104     C**************************************************************************
124600991026     C*  SOTTOMETTE TRASMISSIONE F I A R 6 0 0 T
124700950104     C**************************************************************************
124800950112     C     X25B6         BEGSR
124900950104     C*
125000950112     C     RECB6         IFGT      0
125100991026     C                   MOVEL     'FIAR600T'    CMZFLD
125200991026     C                   MOVEL     'FIAR600R'    CMZFLA
125300950112     C                   MOVE      RECB6         CMZNRR
125400910927     C*
125500910927     C                   EXSR      CALCMZ
125600910926     C                   END
125700950104     C*
125800910926     C                   ENDSR
125900950104     C**************************************************************************
126000991026     C*  SOTTOMETTE TRASMISSIONE F I A R 7 0 0 T
126100950104     C**************************************************************************
126200950112     C     X25B7         BEGSR
126300950104     C*
126400950112     C     RECB7         IFGT      *ZERO
126500991026     C                   MOVEL     'FIAR700T'    CMZFLD
126600991026     C                   MOVEL     'FIAR700R'    CMZFLA
126700950112     C                   MOVE      RECB7         CMZNRR
126800910927     C*
126900910927     C                   EXSR      CALCMZ
127000900601     C                   END
127100950104     C*
127200900601     C                   ENDSR
127300970217     C**************************************************************************
127400970217     C*  SOTTOMETTE TRASMISSIONE F N B L 4 0 0 T
127500970217     C**************************************************************************
127600970217     C     X25B4         BEGSR
127700970217     C*
127800970217     C     RECB4         IFGT      *ZERO
127900060220     C                   MOVEL     'FIAR400T'    CMZFLD
128000060220     C                   MOVEL     'FIAR400R'    CMZFLA
128100970217     C                   MOVE      RECB4         CMZNRR
128200970217     C*
128300970217     C                   EXSR      CALCMZ
128400970217     C                   END
128500970217     C*
128600970217     C                   ENDSR
128700950104     C**************************************************************************
128800930203     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
128900950104     C**************************************************************************
129000910912     C     NUMREC        BEGSR
129100950104     C*
129200930120     C                   Z-ADD     0             NRR
129300930120     C                   CALL      'TRUL51C'
129400950112     C                   PARM                    KNSIF
129500930203     C                   PARM                    CMZFLD           10
129600021009     C                   PARM                    VARmbr           10
129700930120     C                   PARM                    NRR              10 0
129800950104     C*
129900910912     C                   ENDSR
130000950104     C**************************************************************************
130100950104     C*  CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
130200950104     C**************************************************************************
130300910927     C     CALCMZ        BEGSR
130400950104     C*
130500971113     C                   CLEAR                   W236
130600971113     C* IMPOSTO SE I 2 P.O. SONO LOCALI
130700971113     C                   MOVEL     O50LCL        CMZLCL            1
130800020927     C                   CALL      'TIBS53C'
130900971114     C                   PARM                    CMZLCL            1
131000910927     C                   PARM                    CMZDST
131100910927     C                   PARM                    CMZFLD
131200910927     C                   PARM                    CMZMBD
131300910927     C                   PARM                    CMZLBD
131400910927     C                   PARM                    CMZFLA
131500910927     C                   PARM                    CMZMBA
131600910927     C                   PARM                    CMZLBA
131700910927     C                   PARM                    CMZORI
131800910927     C                   PARM                    CMZDTI
131900910927     C                   PARM                    CMZURG
132000910927     C                   PARM                    CMZADD
132100910927     C                   PARM                    CMZCLR
132200000726     C                   PARM                    CMZFTR
132300910927     C                   PARM                    CMZERR
132400910927     C                   PARM                    CMZAZN
132500910927     C                   PARM                    CMZSYF
132600910927     C                   PARM                    CMZNRR
132700910927     C                   PARM                    CMZPRR
132800971113     C                   PARM                    W236            236
132900950104     C*
133000910927     C                   ENDSR
133100971209     C**************************************************************************
133200971209     C*  CALL AL PROGRAMMA CHE REPERISCE LIBRERIA E S.I DI TRAMISSIONE
133300971209     C**************************************************************************
133400971209     C     CALTBS        BEGSR
133500971209     C*
133600971209     C** PRELEVO LO STESSO LA FILIALE ELABORATORE COME DESTINATARIO
133700971209     C     CAETFE        CHAIN     AZORG01L                           31
133800971209     C   31              MOVEL     CAETFE        ORGFEL
133900971209     C*
134000971209     C                   CLEAR                   DSBS50
134100971209     C                   MOVEL     KNSIF         I50PSI
134200971209     C                   MOVEL     SIMFEL        I50PPO
134300971209     C                   MOVEL     CAETFE        I50APO
134400020927     C                   CALL      'TIBS55R'
134500971209     C                   PARM                    DSBS50
134600971209     C**
134700971209     C                   MOVEL     O50PLA        SAVLBD
134800971209     C                   MOVEL     O50ALA        SAVLBA
134900971209     C                   MOVEL     O50ASI        CMZSYF
135000971209     C                   MOVEL     O50ALG        WLBLBA
135100971209     C                   ENDSR
135200100317      **-------------------------------------------------------------**
135300110805      * controllo da particolarità consegna per data inventario ricez DDT
135400100317      **-------------------------------------------------------------**
135500100317     c     ContrDCLI     BEGSR
135600110805     c* Non lo devo fare più in base al flag della DCLI
135700110805     c* ma solo se presente la particolarità
135800110805     c*
135900110805     c***                if        blpccm>0
136000110805     c***                movel     blpccm        wccm
136100110805     c***+               else
136200110805     c***                movel     blpksc        wccm              7 0
136300110805     c***                endif
136400110805     c***
136500110805    1c***                if        wccm<>savccm
136600110805     c***                Clear                   tibs02DS
136700110805     c***                Clear                   dcli
136800110805     c***                Movel     'C'           T02Mod
136900110805     c***                Movel     KNSIF         T02Sif
137000110805     c***                Movel     'CLI'         T02Cod
137100110805     c***                Movel     wccm          T02Ke1
137200110805     c***                Call      'TIBS02R'
137300110805     c***                Parm                    Kpjba
137400110805     c***                Parm                    tibs02DS
137500110805    2c***                If        T02Err = *Blanks
137600110805     c***                Movel     T02Uni        Dcli
137700110805    2c***                endif
137800110805     c***                movel     wccm          savccm
137900110805    1c***                endif
138000100317     C
138100100317     c* Se da trasmettere
138200110805    1c***                if        §cliidt='S'
138300110805
138400110805
138500110805     c                   if        blpgma<>savgma and blpgma<>*blanks
138600110805     C                   MOVEL     '7R'          COD$
138700110805     C                   MOVEL(P)  blpgma        KEY$
138800110805     C                   CLEAR                   DS3A
138900110805     C     KTABEL        CHAIN     TABEL00F                           39
139000110805     C  N39              MOVEL     TBLUNI        DS7R
139100110805     c   39              clear                   ds7R
139200110805     c                   eval      savgma=blpgma
139300110805     c                   endif
139400110805     c
139500110805     c                   if        §7r1BF='B'  and blpgma<>*blanks
139600110805     c
139700100317     c* Escludo i recuperi
139800100317     C* CODICE BOLLA
139900100317     C                   MOVEL     '3A'          COD$
140000100317     C                   MOVEL(P)  blpcbo        KEY$
140100100317     C                   CLEAR                   DS3A
140200100317     C     KTABEL        CHAIN     TABEL00F                           39
140300100317     C  N39              MOVEL     TBLUNI        DS3a
140400100317     c
140500100317    2c                   if        §3arbl<>'R'
140600100317     c*
140700150916     c***  kar5          chain     fiar501l
140800150916    3c***                if        %found(fiar501l)
140900150916     c***                movel     ar5uni        dar5gen
141000150916     c***                eval      §ar5drdt='99999999'
141100150916     c***                movel     dar5gen       ar5uni
141200150916     c***                clear                   ar5dt2
141300150916     c***                update    fiar5000
141400150916   x3c***                else
141500100317     c                   clear                   fiar5000
141600100317     c                   eval      ar5aas=blpaas
141700100317     c                   eval      ar5lnp=blplnp
141800100317     c                   eval      ar5nrs=blpnrs
141900100317     c                   eval      ar5nsp=blpnsp
142000150916     c****               eval      ar5trd='GEN'
142001150916     c                   eval      ar5trd='DOC'
142100100317     c                   movel     numtra        ar5dac
142200100317     c                   time                    ar5orc
142300150916     c***                clear                   dar5gen
142301150916     c                   clear                   dar5doc
142400100317     c                   eval      §ar5drdt='99999999'
142500150916     c                   movel     dar5doc       ar5uni
142501150916     c***                movel     dar5gen       ar5uni
142600100317      *
142700150916     C                   WRITE     FIAR50tt
142701150916     C****               WRITE     FIAR5000                             38
142800150916e   3c***                ENDIF
142900100317e   2c                   ENDIF
143000100317    1c                   ENDIF
143100100317     c
143200100317     c                   ENDSR
143300110318      **-------------------------------------------------------------**
143400110318      * controllo sse ritiro effettuato con PDA
143500110318      **-------------------------------------------------------------**
143600110318     c     ContrPDA      BEGSR
143700110318     c                   clear                   RitiroPDA         1
143800110318     c* Se c'e' numero  ORM --> reperisco subito la distinta
143900110318     c     kar4          chain     fiar401l
144000110318    1c                   if        %found(fiar401l)
144100110318     c                   movel     ar4not        dsbl4m
144200110318     c
144300110318     c     korg          chain     fnorg01l
144400110321    2c                   if        %found(fnorg01l) and R_orgatb=' '
144500110318     c                   eval      kfgs=r_orgfgs
144600110321     c                   eval      knfv=r_orgndc
144700110318     c
144800110318     c     kdst          chain     fidst01l
144900110321    3c                   if        %found(fidst01l) and dstatb=' '
145000110318
145100110318     c                   eval      RitiroPDA='N'
145200110318    4c                   if        dstpda='O' or dstpda='E'
145300110318     c                   eval      DDSTFLR =  DstFLR
145400110318    5c                   if        §dstTSTPDA=' ' or §dstTSTPDA='C'
145500110318     c                   eval      RitiroPDA='S'
145600110318    5c                   endif
145700110318    4c                   endif
145800110321     c
145900110318    3c                   endif
146000110318    2c                   endif
146100110318    1c                   endif
146200110318     c
146300110318     c* se non ho trovato l'ORM sulla bolla, lo devo desumere con data e padr
146400110321    1c                   if        RitiroPDA= ' '
146500110318     c* Pulisco i campi ce mi servono per contare le distinte
146600110318     c                   clear                   disPADR           2 0
146700110318     c* Imposto il tipo distinta
146800110318     c                   eval      tipod(1)='O'
146900110318     c                   eval      tipod(2)='E'
147000110318     c* Data distinta = data ritiro
147100110318     c                   eval      kdfv=blpdrt
147200110318     c* PO ritiro    = linea partenza bolla
147300110318     c                   eval      kfgs=blplnp
147400110318     c                   exsr      CercaDST
147500110318     c
147600110318     c* Se non trovata cerco distinta solo CONSEGNE di quel padroncino
147700110321     c* ES --> per ora non lo faccio...è opinabile il fatto che vada bene
147800110321    2c***                if        RitiroPDA=' '
147900110321     c***                eval      tipod(1)='C'
148000110321     c***                eval      tipod(2)=' '
148100110321     c***                exsr      CercaDST
148200110321    2c***                endif
148300110321e   1c                   ENDIF
148400110318     c
148500110321    1c                   if        RitiroPDA='S'
148600110318     c     kar5          chain     fiar501l
148700110321    2c                   if        %found(fiar501l)
148800110318     c                   movel     ar5uni        dar5gen
148900110318     c                   eval      §ar5PDArt='S'
149000110318     c                   movel     dar5gen       ar5uni
149100110318     c                   clear                   ar5dt2
149200110318     c                   update    fiar5000
149300110321   x2c                   else
149400110318     c                   clear                   fiar5000
149500110318     c                   eval      ar5aas=blpaas
149600110318     c                   eval      ar5lnp=blplnp
149700110318     c                   eval      ar5nrs=blpnrs
149800110318     c                   eval      ar5nsp=blpnsp
149900110318     c                   eval      ar5trd='GEN'
150000110318     c                   movel     numtra        ar5dac
150100110318     c                   time                    ar5orc
150200110318     c                   clear                   dar5gen
150300110318     c                   eval      §ar5PDArt='S'
150400110318     c                   movel     dar5gen       ar5uni
150500110318      *
150600110318     C                   WRITE     FIAR5000                             38
150700110321e   2c                   ENDIF
150800110318     c
150900110321e   1c                   ENDIF
151000110318
151100110318     c                   ENDSR
151200110318      **-------------------------------------------------------------**
151300110318      * cerco distinta ORM per data
151400110318      **-------------------------------------------------------------**
151500110318     c     CercaDST      BEGSR
151600110318     c*
151700110318     c     kdst2         setll     fidst02l
151800110318     c     kdst2         reade     fidst02l
151900110318     c
152000110318    1c                   dow       not %eof(fidst02l)
152100110318     c* Padroncino diverso--> rileggo
152200110321   1ac                   if        dstatb=' ' and dstpdr= blppdr
152300110321
152400110318     c* Trovata la distinta --> conteggio
152500110318     c                   add       1             disPADR
152600110318     c
152700110318     c* non corrisponde ai tipi indicati --> rileggo
152800110318     c     dstpda        lookup    tipod                                  30
152900110321   1bc                   if        *in30
153000110318     c
153100110318    2c                   if        RitiroPDA= ' '   or
153200110318     c                             (ritiroPDA='N' and tipod(1)<>'C')
153300110318     c* verifico se PDA in TEST
153400110318     c                   eval      DDSTFLR =  DstFLR
153500110318    3c                   if        §dstTSTPDA=' ' or (§dstTSTPDA='C'
153600110321     c                             and dstpda<>'C')
153700110318     c                   eval      RitiroPDA='S'
153800110318   x3c                   else
153900110321    4c                   if        dstpda <>'C'
154000110318     c                   eval      RitiroPDA='N'
154100110318    4c                   endif
154200110318    3c                   endif
154300110318    2c                   endif
154400110321   1bc                   endif
154500110321   1ac                   endif
154600110318     c
154700110318     c     kdst2         reade     fidst02l
154800110318     c                   enddo
154900110318     c
155000110318     c                   ENDSR
155100110322      **-------------------------------------------------------------**
155200110322     c     *inzsr        BEGSR
155300110322     C* ACCESSO TABEL00F
155400110322     C     KTAB          KLIST
155500110322     C                   KFLD                    CODUT
155600110322     C                   KFLD                    COD
155700110322     C* ACCESSO TABEL00F
155800110322     C     KTABEL        KLIST
155900110322     C                   KFLD                    CODUT
156000110322     C                   KFLD                    COD$
156100110322     C                   KFLD                    KEY$
156200110322     C* ACCESSO FNBLP01L
156300110322     C     KBLP          KLIST
156400110322     C                   KFLD                    BLPAAS
156500110322     C                   KFLD                    BLPLNP
156600110322     C                   KFLD                    BLPNRS
156700110322     C                   KFLD                    BLPNSP
156800110322     C     Kar5          KLIST
156900110322     C                   KFLD                    BLPAAS
157000110322     C                   KFLD                    BLPLNP
157100110322     C                   KFLD                    BLPNRS
157200110322     C                   KFLD                    BLPNSP
157300110322     C                   KFLD                    KTRD
157400110322     C     Kar4          KLIST
157500110322     C                   KFLD                    BLPAAS
157600110322     C                   KFLD                    BLPLNP
157700110322     C                   KFLD                    BLPNRS
157800110322     C                   KFLD                    BLPNSP
157900110322     C                   KFLD                    KTRc
158000110322     C     KORG          KLIST
158100110322     C                   KFLD                    §b4poe
158200110322     C                   KFLD                    §b4nsr
158300110322     C                   KFLD                    §b4nor
158400110322     C                   KFLD                    §b4nrv
158500110322     C     KDST          KLIST
158600110322     C                   KFLD                    knpg
158700110322     C                   KFLD                    knfv
158800110322     C                   KFLD                    kfgs
158900110322     C     KDST2         KLIST
159000110322     C                   KFLD                    knpg
159100110322     C                   KFLD                    kdfv
159200110322     C                   KFLD                    kfgs
159300110322     c
159400110322     C* ACCESSO A ESTENSIONE SEGNACOLLI PER SCRITTURA TITAH
159500110322     C     KSEGNA        KLIST
159600110322     C                   KFLD                    BLTFLS
159700110322     C                   KFLD                    BLTLNA
159800110322     C                   KFLD                    BLTNRS
159900110322     C                   KFLD                    BLTNSC
160000110322     C*
160100110322     C* DEFINIZIONE CAMPI
160200110322     C     *LIKE         DEFINE    TBLCOD        COD
160300110322     C     *LIKE         DEFINE    TBLCOD        COD$
160400110322     C     *LIKE         DEFINE    TBLKEY        KEY$
160500110322     C     *LIKE         DEFINE    CMZLBD        SAVLBD
160600110322     C     *LIKE         DEFINE    CMZLBA        SAVLBA
160700110322     C     *LIKE         DEFINE    CMZLBA        WLBLBA
160800110322     c
160900110322     C* NUMERO TRASMISSIONE = DATA DEL GIORNO (AAAA/MM/GG)
161000110322     C                   TIME                    WTIME            14 0
161100110322     C                   MOVE      WTIME         WDATE             8 0
161200110322     C                   Z-ADD     WDATE         G02DAT
161300110322     C                   Z-ADD     *ZERO         G02INV
161400110322     C                   MOVEL     *BLANKS       G02ERR
161500110322     C                   CALL      'XSRDA8'
161600110322     C                   PARM                    WLBDAT
161700110322     C                   Z-ADD     G02INV        NUMTRA            8 0
161800110322     c                   ENDSR
161900950104     C**************************************************************************
162000930203**         CMD1
162100021010ALCOBJ OBJ((FIBLPA0T *FILE *EXCL M000xxx))
162200910912**         CMD2
162300021010DLCOBJ OBJ((FIBLPA0T *FILE *EXCL M000xxx))
162400910912**         CMD3
162500021010ALCOBJ OBJ((FIAR700T *FILE *EXCL M000xxx))
162600910912**         CMD4
162700021010DLCOBJ OBJ((FIAR700T *FILE *EXCL M000xxx))
162800920221**         C17
162900021010OVRDBF FILE(XXXXXXXX)         MBR(M000xxx)
163000910926**         CM33
163100021010ALCOBJ OBJ((FIAR600T *FILE *EXCL M000xxx))
163200910926**         CM34
163300021010DLCOBJ OBJ((FIAR600T *FILE *EXCL M000xxx))
163400930203**         C56
163500021010ALCOBJ OBJ((XXXXXXXX *FILE *EXCL MXXXxxx))
163600930203**         C57
163700021010DLCOBJ OBJ((XXXXXXXX *FILE *EXCL M000xxx))
163800021009**         Cvp
163900021009ALCOBJ OBJ((FNARBXXX *FILE *EXCL MXXXXXX))
164000021009DLCOBJ OBJ((FNARBXXX *FILE *EXCL MXXXXXX))
164100100914**
164200100914DSPJOBLOG OUTPUT(*PRINT)
164300111123**
164400111123DLYJOB DLY(120)
164500111123**
164600111123OVRPRTF FILE(QPDSPOLK) OUTQ(PRT01) HOLD(*YES) SHARE(*YES)
164700100914**
164800100914WRKOBJLCK OBJ(xxxxxxxxxx) OBJTYPE(*FILE) MBR(Myyyyyy) OUTPUT(*PRINT)
