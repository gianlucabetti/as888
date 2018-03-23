000100900410     H DECEDIT('0,') DATEDIT(*DMY.)
000200941020     H* TNTA66R *----------------------------------------------------*
000300940328     H*  STATISTICA ATTIVITA' COMMERCIALE
000400900410     H*--------------------------------------------------------------*
000500101019     FTIVIS05L  IF   E           K DISK
000600940328     FCNCLP00F  IF   E           K DISK
000700940328     FTFCLP00F  IF   E           K DISK
000800940328     F                                     RENAME(CNCLP000:TFCLP000)
000900940328     FTFSAC02L  UF A E           K DISK
001000981230     FTFSAC01L  UF A E           K DISK
001100981230     F                                     RENAME(TFSAC000:TFSAC001)
001200101222     F**!!!TABEL00F  IF   E           K DISK
001300981203     FTNCPO08L  IF   E           K DISK
001400100528     FTNCPO01L  IF   E           K DISK    RENAME(Tncpo000:Tncpo001)
001500981117     FAZORG01L  IF   E           K DISK
001600021017     fTntbe01l  uf   e           k Disk
001700981230     FQSYSPRT   O    F  132        PRINTER
001800021017
001900021017     d kTbeCod         s                   Like(TbeCod)
002000021017     d kTbeKe1         s                   Like(TbeKe1)
002100021029     d wtime1          s               t   timfmt(*hms) inz
002200021029     d wtime2          s               t   timfmt(*hms) inz
002300021017
002400980831     D* VISITE VALIDE (FLAG FITTIZIO = ' ')
002500020218     D TO1             S              7  0 DIM(5)
002600020218     D TO2             S              7  0 DIM(5)
002700020218     D TO3             S              7  0 DIM(5)
002800020218     D OFE             S              7  0 DIM(5)
002900020218     D COE             S              7  0 DIM(5)
003000980831     D* VISITE FITTIZIE (FLAG FITTIZIO ='S')
003100020218     D TO1F            S              7  0 DIM(5)
003200020218     D TO2F            S              7  0 DIM(5)
003300020218     D TO3F            S              7  0 DIM(5)
003400020218     D OFEF            S              7  0 DIM(5)
003500020218     D COEF            S              7  0 DIM(5)
003600980831     D* POTENZIALI INVIO MAIL
003700940328     D IML             S              7  0 DIM(3)
003800940328     D*
003900010703     D NUM             S              7  0 DIM(900)
004000010703     D NUM2            S              7  0 DIM(900)
004100900410     D TCUDS           DS
004200900410     D  F1                     1      1
004300900410     D  F3                     3      3
004400900410     D  F2                     2      2
004500900410     D  F4                     4      4
004600900410     D  F56                    5      6
004700100528     D DSATC         E DS                  EXTNAME(Tiatc00F)
004800981230     D* PASSO ANNO/MESE DI ELABORAZIONE
004900940328     D PARAM           DS
005000941021     D  PARAMM                 1      6
005100950407     D* PARCTR = ' ' CONTROLLO SE L'HO GIA' ELABORATO--> NON ELABORO
005200950407     D* PARCTR = 'N' ELABORO A PRESCINDERE SE L'HO GIA' FATTO O NO
005300950407     D  PARCTR                 7      7
005400941021     D WLBDAT          DS
005500941021     D  G02DAT                 1      8  0
005600941021     D  G02INV                 9     16  0
005700941021     D  G02ERR                17     17
005800941021     D  G02TGI                18     22  0
005900940328     D                 DS
006000941021     D  DATA                   1      8  0
006100940328     D  GG                     1      2  0
006200940328     D  MM                     3      4  0
006300941021     D  AA                     5      8  0
006400020528     D*                                    DIM(30)
006500101222     D**!!! DS1D          E DS
006600101222     d Tibs02ds      e ds
006700101222     d dttr          e ds
006800940328     D KPJBA         E DS
006900970113     D CNCR80        E DS
007000940323     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007100900410     D  TCU                  398    697
007200900411     D                                     DIM(50)
007300900410     D  KCU                  698    847P 0
007400900411     D                                     DIM(50)
007500900411     D                                     PACKEVEN
007600021017
007700021017      * Ds date statistiche
007800021017     d dSdf          e ds
007900120917      * Dati in CPORST
008000120917     d dcpo01        e ds                  inz
008100120917
008200100528     d wrkgetlista     s           4096    varying
008300100528     d PrimaOFF        s              8  0
008400100531     d PrimaCON        s              8  0
008500100528     d vofnrv          s              7  0
008600100531     d Aggio           s              1
008700101019     d wksc            s                   like(visksc)
008800101019     d wcpo            s                   like(viscpo)
008900101019     d wnrv            s                   like(visnrv)
009000920521     C*****************************************************************
009100920521     C* RIEPILOGO INDICATORI
009200920521     C*****************************************************************
009300940328     C* 01    - TIPO VISITA VALEVOLE SIA PER CLIENTI NEW CHE OLD
009400041007     C* 15    -
009500940328     C* 30    - LETTURA FILES
009600940328     C* 31    - DI COMODO
009700940328     C*****************************************************************
009800900410     C     *ENTRY        PLIST
009900900410     C                   PARM                    KPJBA
010000900410     C                   Z-ADD     1             CODUT
010100900410     C                   CALL      'X§PARUT'
010200900410     C                   PARM                    UT§DSE
010300970113     C                   MOVEL     REC80         CNCR80
010400981117     C     SIMFEL        CHAIN     AZORG01L                           30
010500020528     C                   MOVEL     ORGFEL        WSFEL             3 0
010600900410     C*--- RICERCA CAPOCONTI------------------------------------------*
010700900410     C                   DO        50            X                 2 0
010800900410     C                   MOVE      TCU(X)        TCUDS
010900900410     C     F56           CABNE     'CG'          END1
011000900410     C     F4            COMP      '1'                                    21
011100900410     C     F4            COMP      '2'                                    22
011200900410     C     F4            COMP      '3'                                    23
011300900410     C     F4            COMP      '6'                                    27
011400900410     C** 1 CLIENTI   21
011500900410     C** 2 FORNITORI 22
011600900410     C** 3 AGENTI    23
011700900410     C     F3            COMP      '0'                                242425
011800900410     C     F3            COMP      'I'                                    26
011900900410     C** 0 ITALIA   25
012000900410     C** 1 ESTERO   24
012100900410     ** I CAPO CONTO IVA
012200900410     C   21
012300900410     CAN 24              Z-ADD     KCU(X)        KCE               4 0
012400900410     C   21
012500900410     CAN 25              Z-ADD     KCU(X)        KCI               4 0
012600900410     C   22
012700900410     CAN 24              Z-ADD     KCU(X)        KFE               4 0
012800900410     C   22
012900900410     CAN 25              Z-ADD     KCU(X)        KFI               4 0
013000900410     C   23
013100900410     CAN 24              Z-ADD     KCU(X)        KAE               4 0
013200900410     C   23
013300900410     CAN 25              Z-ADD     KCU(X)        KAI               4 0
013400900410     C   26              Z-ADD     KCU(X)        KIVA              4 0
013500900410     C   27              Z-ADD     KCU(X)        KBNA              4 0
013600900410     C     END1          TAG
013700900410     C                   END
013800940324     C*---------------------------------------------------------------*
013900940328     C*--- FILE TFSAC00F
014000940328     C     KSAC          KLIST
014100980831     C                   KFLD                    AA
014200940328     C                   KFLD                    MM
014300981230     C     KSAC1         KLIST
014400981230     C                   KFLD                    KCMM
014500981230     C                   KFLD                    AA
014600981230     C                   KFLD                    MM
014700981230     C                   KFLD                    WVCN
014800100531     C     KSAC3         KLIST
014900100531     C                   KFLD                    sacCMM
015000100531     C                   KFLD                    sacAAA
015100100531     C                   KFLD                    sacMMM
015200100531     C                   KFLD                    sacAPL
015300100531     C                   KFLD                    sacTPV
015400100531     C                   KFLD                    sacCLV
015500100531     C                   KFLD                    sacFIE
015600100531     C                   KFLD                    sacFFZ
015700920521     C*--- FILE TABEL00F
015800101222     C**!!!KTAB          KLIST
015900101222     C**!!!              KFLD                    CODUT
016000101222     C**!!!              KFLD                    COD               2
016100101222     C**!!!              KFLD                    KEY               8
016200940328     C** ACCESSO CNCL
016300940328     C     KCLP          KLIST
016400900410     C                   KFLD                    CODUT
016500940328     C                   KFLD                    KCI
016600900410     C                   KFLD                    KSC               7 0
016700981230     C     KCPO          KLIST
016800981230     C                   KFLD                    WCMM
016900981230     C                   KFLD                    CPOFLT
017000021017
017100021017     c     Ktbe          Klist
017200021017     c                   Kfld                    kTbeCod
017300021017     c                   Kfld                    kTbeKe1
017400940324     C*---------------------------------------------------------------*
017500940324     C                   MOVEL     KPJBU         PARAM
017600940328     C* OPERAZIONI INIZIALI
017700940328     C                   EXSR      INIZIO
017800101019     c
017900940328     C     *IN99         IFEQ      *OFF
018000100531     c                   eval      Aggio='N'
018100940328     C***
018200940328     C***   P O T E N Z I A L I
018300940328     C***
018400940328     C                   MOVEL     'P'           WVCN
018500981203     C                   CLEAR                   WCMM
018600981203     C* LEGGO TUTTI I POTENZIALI DA CONTATTARE :
018700981230     C*  HANNO IL COMMERCIALE = 0 E SONO SENZA AZIONI
018800981230     C     WCMM          SETLL     TNCPO000
018900981230     C     WCMM          READE     TNCPO000                               30
019000940328     C*
019100940328    1C     *IN30         DOWEQ     *OFF
019200970113     C* ESCLUDO GLI ANNULLATI
019300940531    2C     CPOATB        IFEQ      ' '
019400040503      * escluto i potenziali senza cpoflt .... altrimenti il pgm si spacca
019500040503     c     cpoflt        andgt     *Zeros
019600100527      *
019700120917     C                   EVAL      DCPO01 = CPORST
019800120917     C                   IF        §CPODTPCON = *BLANKS OR
019900120917     C                             §CPODTPCON =  *ZEROS
020000981230     C**
020100981230    6C     SAVFLT        IFNE      CPOFLT
020200981230    7C     SAVFLT        IFGT      0
020300981230     C                   EXSR      ROTFLT
020400981230    7C                   ENDIF
020500981230     C**
020600981230     C                   Z-ADD     CPOFLT        SAVFLT
020700981230    6C                   ENDIF
020800981230     C**
020900981230     C                   ADD       1             TO1(1)
021000981230     C**
021100981230     C                   Z-ADD     SAVFLT        A                 3 0
021200981230     C                   ADD       1             NUM(A)
021300981230    5C                   ENDIF
021400101019     C**
021500981230    2C                   ENDIF
021600981230     C**
021700981230     C     WCMM          READE     TNCPO000                               30
021800981203    1C                   ENDDO
021900981203     C**
022000981230     C* ULTIMA SCRITTURA
022100981230     C                   EXSR      ROTFLT
022200981230     C***
022300981230     C*  LEGGO LA 1 AZIONE: SE > DEL MESE DI ELABORAZIONE, NEL MESE
022400981230     C*  ERA UN CLIENTE DA CONTATTARE
022500981230     C                   CLEAR                   SAVFLT
022600981230     C                   Z-ADD     1             WCMM
022700981230     C     WCMM          SETLL     TNCPO08L
022800981230     C                   READ      TNCPO08L                               30
022900981230     C**
023000981230    1C     *IN30         DOWEQ     *OFF
023100981230   1AC     CPOATB        IFEQ      ' '
023200981230     C*
023300120917     C                   EVAL      DCPO01 = CPORST
023400120917     C                   MOVEL     §CPODTPCON    W0060             6 0
023500120917     C                   IF        W0060 > 0
023600981230     C**
023700981230     C** NEL MESE DI ELABORAZIONE ERA UN CLIENTE DA CONTATTARE
023800981230    3C     W0060         IFGT      WDATA
023900981230     C*
024000981230    4C     SAVFLT        IFNE      CPOFLT
024100981230    5C     SAVFLT        IFGT      0
024200981230     C                   EXSR      ROTFLT
024300981230    5C                   ENDIF
024400981230     C                   Z-ADD     CPOFLT        SAVFLT
024500981230    4C                   ENDIF
024600981230    C**
024700981230     C                   ADD       1             TO1(1)
024800981230     C                   Z-ADD     SAVFLT        A                 3 0
024900981230     C                   ADD       1             NUM2(A)
025000981230     C**
025100981230    3C                   ENDIF
025200981230    2C                   ENDIF
025300981230   1AC                   ENDIF
025400940531     C*
025500981230     C                   READ      TNCPO000                               30
025600981230    1C                   ENDDO
025700940328     C*
025800940328     C* ULTIMA ROTTURA COMMERCIALE
025900981230     C                   EXSR      ROTFLT
026000981230     C                   EXSR      ROTCMM
026100100527     c* Lettura file nuovi
026200100531     c                   eval      Aggio='S'
026300100528     c*  Visite clienti file CRM
026400100528     c                   exsr      NewVIS
026500100531     c*  Offerte ITALIA clienti file CRM
026600100528     c                   exsr      NewOFF
026700100531     c*  Offerte ESTERE clienti file CRM
026800100531     c                   exsr      NewEstOFF
026900100531     c*  Offerte ITALIA convalidate file CRM
027000100531     c                   exsr      NewConval
027100100531     c*  Offerte Estere convalidate file CRM
027200100531     c                   exsr      NewEstCon
027300940328     C*
027400021017     C* AGGIORNO Tabella CON DATA ULTIMA ELABORAZIONE
027500021017     C     WDATA         IFGT      §SdfStcU
027600021017     C                   Z-ADD     WDATA         §SdfStcU
027700940328     C                   ENDIF
027800021017     c                   Movel     dSdf          TbeUni
027900021017     c                   Update    Tntbe000
028000940520     C*
028100020528     C                   ENDIF
028200940328     C*
028300981230     C**
028400900410     C                   SETON                                        LR
028500940328     C*
028600940328     C*  OPERAZIONI INIZIALI ----------------------------------------*
028700940328     C     INIZIO        BEGSR
028800041007     c*
028900041007     C                   TIME                    W0140            14 0
029000041007     C                   CLEAR                   WLBDAT
029100041007     C                   MOVE      W0140         G02DAT
029200041007     C                   CALL      'XSRDA8'
029300041007     C                   PARM                    WLBDAT
029400041007     C                   Z-ADD     G02INV        dateu             8 0
029500041007     c*
029600970113     C**
029700940328     C* E' STATO PASSATO ANNOMESE
029800940328     C                   TESTN                   PARAMM               31
029900940328    1C     *IN31         IFEQ      *ON
030000940328     C     PARAMM        ANDGT     *ZEROS
030100940328     C                   MOVEL     PARAMM        WDATA
030200940328     C                   MOVEL     WDATA         AA
030300940328     C                   MOVE      WDATA         MM
030400940328     C*
030500940328   X1C                   ELSE
030600940328     C*
030700940328     C* SE NON E' STATO PASSATO
030800940328     C* DETERMINO IL MESE DI ELABORAZIONE --> MESE DI UDATE - 1
030900941021     C* LA DATA DEL GIORNO LA PRENDO DA TIME
031000941021     C                   MOVE      W0140         DATA
031100940328     C*
031200940328     C                   SUB       1             MM
031300940328    2C     MM            IFEQ      0
031400940328     C                   Z-ADD     12            MM
031500940328     C                   SUB       1             AA
031600940328    2C                   ENDIF
031700941021     C                   MOVEL     AA            WDATA             6 0
031800940328     C                   MOVE      MM            WDATA
031900940328    1C                   ENDIF
032000021029      * Aggancio la tabella SDF e la tengo allocata fino alla fine
032100021029      * del PGM così non posso avere 2 elaborazioni contemporanee
032200021017     c                   Clear                   dSdf
032300021029     c                   Eval      kTbeCod = 'SDF'
032400021029     c                   Movel(p)  Wsfel         kTbeKe1
032500021029     c                   Do        3
032600021029     c     Ktbe          Chain(e)  Tntbe01l
032700021029      * Ok ho agganciato il record esco dal do
032800021029     c                   If        Not %Error
032900021029     c                   Leave
033000021029     c                   EndIf
033100021029      * Il record è allocato aspetto un minuto poi riprovo
033200021029     c                   time                    wtime1
033300021029     c     wtime1        adddur    60:*s         wtime2
033400021029     c                   dou       wtime1 >= wtime2
033500021029     c                   time                    wtime1
033600021029     c                   enddo
033700021029 2e  c                   enddo
033800021029      * dopo 3 tentativi andati male esco dal programma
033900021029     c                   If        %Error
034000021029     c                   eval      *in99 = *on
034100021029     c                   endif
034200021029      * Trovo la tabella
034300021029     c                   If        %Found(Tntbe01l) and not *in99
034400021029     c                   Movel     TbeUni        dSdf
034500021017     c                   EndIf
034600021029      * Non trovo la tabella esco dal programma
034700021029     c                   If        Not %Found(Tntbe01l)
034800021029     c                   Eval      *In99 = *On
034900021029     c                   EndIf
035000950407     C*
035100950407     C* SE DEVO RIFARE IL CONTROLLO CON LA DATA AREA E NEL FRATTEMPO
035200950407     C*  L'HO GIA' ELABORATA --> NON FACCIO NULLA
035300950407     C  N99PARCTR        IFEQ      ' '
035400021017     C**!!!WDATA         ANDLE     TNTA66
035500021017     C     WDATA         ANDLE     §SdfStcU
035600950407     C                   SETON                                        99
035700950407     C                   ENDIF
035800940328     C*
035900940328     C* 99 ON - VADO A FINE PGM
036000940328    1C     *IN99         IFEQ      *OFF
036100981117     C                   CLEAR                   SAV003
036200940328     C* SE PER CASO HO GIA' ELABORATO IL MESE CANCELLO E RISCRIVO
036300940328     C     KSAC          CHAIN     TFSAC000                           30
036400940328     C*
036500940328    2C     *IN30         DOWEQ     *OFF
036600020528     C* SE E' DEL MIO AS CANCELLO
036700981117     C                   MOVEL     SACCMM        W0030             3 0
036800981117     C     W0030         IFNE      SAV003
036900981117     C     W0030         CHAIN     AZORG01L                           31
037000981117     C                   MOVEL     W0030         SAV003            3 0
037100981117     C                   ENDIF
037200020528     C     ORGFEL        IFEQ      WSFEL
037300940328     C                   DELETE    TFSAC000
037400981117     C                   ENDIF
037500940328     C     KSAC          READE     TFSAC000                               30
037600940328    2C                   ENDDO
037700940328     C*
037800940328    1C                   ENDIF
037900940328     C* DEFINIZIONE CAMPI
038000981230     C     *LIKE         DEFINE    CPOFLT        SAVFLT
038100981230     C     *LIKE         DEFINE    CPOCMM        WCMM
038200981230     C     *LIKE         DEFINE    CPOCMM        KCMM
038300101019     C     *LIKE         DEFINE    vISTPV        WTPV
038400101222     C**!!!*LIKE         DEFINE    §1DVCN        WVCN
038500990105     C*
038600900410     C                   ENDSR
038700981230     C**
038800981230     C* ROTTURA PER PO APPARTENENZA ----------------------------------*
038900981230     C     ROTFLT        BEGSR
039000981230     C*
039100981230    1C     TO1(1)        IFGT      0
039200981230     C* IMPOSTO CAMPI FISSI DA SCRIVERE
039300990105     C                   CLEAR                   KCMM
039400981230     C                   MOVEL     SAVFLT        KCMM
039500981230     C*
039600981230     C     KSAC1         CHAIN     TFSAC01L                           32
039700981230    2C     *IN32         IFEQ      *ON
039800981230     C                   CLEAR                   TFSAC000
039900981230     C                   MOVEL     SAVFLT        SACCMM
040000981230     C                   MOVEL     AA            SACAAA
040100981230     C                   MOVEL     MM            SACMMM
040200981230     C                   MOVEL     WVCN          SACAPL
040300981230    2C                   ENDIF
040400041007     C                   clear                   SACFTR
040500041007     C                   z-add     dateu         SACDTR
040600981230     C                   ADD       TO1(1)        SACTO1
040700981230     C   32              WRITE     TFSAC001
040800981230     C  N32              UPDATE    TFSAC001
040900981230    1C                   ENDIF
041000981230     C* PULIZIA SCHIERE
041100981230     C                   CLEAR                   TO1
041200981230     C*
041300981230     C                   ENDSR
041400940324     C*
041500940328     C* ROTTURA CODICE COMMERCIALE -----------------------------------*
041600940328     C     ROTCMM        BEGSR
041700940328     C*
041800981230    1C     IML(1)        IFGT      0
041900981230     C                   Z-ADD     WCMM          KCMM
042000940328     C* IMPOSTO CAMPI FISSI DA SCRIVERE
042100981230     C     KSAC1         CHAIN     TFSAC01L                           32
042200981230    2C     *IN32         IFEQ      *ON
042300940328     C                   CLEAR                   TFSAC000
042400940328     C                   MOVEL     WCMM          SACCMM
042500980831     C                   MOVEL     AA            SACAAA
042600940328     C                   MOVEL     MM            SACMMM
042700940328     C                   MOVEL     WVCN          SACAPL
042800981230    2C                   ENDIF
042900981230     C**
043000981230     C                   ADD       IML(1)        SACTO4
043100041007     C                   clear                   SACFTR
043200041007     C                   z-add     dateu         SACDTR
043300981230     C**
043400981230     C   32              WRITE     TFSAC001
043500981230     C  N32              UPDATE    TFSAC001
043600981230    1C                   ENDIF
043700940328     C*
043800940328     C* PULIZIA SCHIERE
043900940328     C                   CLEAR                   IML
044000940328     C*
044100940328     C                   ENDSR
044200940328     C*
044300940328     C* SCRIVO RECORD IN TFSAC00F ------------------------------------*
044400940328     C     SCRIVI        BEGSR
044500980831     C                   CLEAR                   SACTO1
044600980831     C                   CLEAR                   SACTO2
044700980831     C                   CLEAR                   SACTO3
044800980831     C                   CLEAR                   SACTO4
044900980831     C                   CLEAR                   SACFIE
045000980831     C                   CLEAR                   SACFFZ
045100940328     C* TIPOLOGIA CLIENTE
045200020218     c                   SELECT
045300020218     C                   WHEN      YY = 1
045400020218     c                   eval      sacclv = 'A'
045500020218     c                   WHEN      YY = 2
045600020218     c                   eval      sacclv = 'B'
045700020218     c                   WHEN      YY = 3
045800020218     c                   eval      sacclv = 'C'
045900020218     c                   WHEN      YY = 4
046000020218     c                   eval      sacclv = 'T'
046100020218     c                   WHEN      YY = 5
046200020218     c                   eval      sacclv = 'D'
046300020218     c                   ENDSL
046400020218      *
046500020218      * Memorizzo dati
046600980831     C                   SELECT
046700980831     C* VISITE TARIFFE BUONE
046800981002     C     WWRITE        WHENEQ    '1 '
046900100531     c                   if        Aggio='S'
047000100531     c     ksac3         chain     tfsac01l
047100100531     c                   endif
047200100531     c                   if        Aggio<>'S' or not %found(TFSAC01l)
047300020218     C                   Z-ADD     TO1(yy)       SACTO1
047400100531     c                   else
047500100531     C                   add       TO1(yy)       SACTO1
047600100531     c                   endif
047700980831     C* VISITE TARIFFE FITTIZIE
047800981002     C     WWRITE        WHENEQ    '3 '
047900980831     C                   MOVEL     'S'           SACFFZ
048000100531     c                   if        Aggio='S'
048100100531     c     ksac3         chain     tfsac01l
048200100531     c                   endif
048300100531     c                   if        Aggio<>'S' or not %found(TFSAC01l)
048400100531     C                   Z-ADD     TO1F(yy)      SACTO1
048500100531     c                   else
048600100531     C                   add       TO1F(yy)      SACTO1
048700100531     c                   endif
048800980831     C* OFFERTE E CONVALIDE BUONE ITALIA
048900981002     C     WWRITE        WHENEQ    '2 '
049000980831     C                   MOVEL     'I'           SACFIE
049100100531     c                   if        Aggio='S'
049200100531     c     ksac3         chain     tfsac01l
049300100531     c                   endif
049400100531     c                   if        Aggio<>'S' or not %found(TFSAC01l)
049500100531     C                   Z-ADD     TO2(yy)       SACTO2
049600100531     C                   Z-ADD     TO3(yy)       SACTO3
049700100531     c                   else
049800100531     C                   add       TO2(yy)       SACTO2
049900100531     C                   add       TO3(yy)       SACTO3
050000100531     c                   endif
050100980831     C* OFFERTE E CONVALIDE BUONE ESTERE
050200981002     C     WWRITE        WHENEQ    '2E'
050300980831     C                   MOVEL     'E'           SACFIE
050400100531     c                   if        Aggio='S'
050500100531     c     ksac3         chain     tfsac01l
050600100531     c                   endif
050700100531     c                   if        Aggio<>'S' or not %found(TFSAC01l)
050800100531     C                   Z-ADD     OFE(yy)       SACTO2
050900100531     C                   Z-ADD     COE(yy)       SACTO3
051000100531     c                   else
051100100531     C                   add       OFE(yy)       SACTO2
051200100531     C                   add       COE(yy)       SACTO3
051300100531     c                   endif
051400980831     C* OFFERTE E CONVALIDE FITTIZIE ITALIA
051500981002     C     WWRITE        WHENEQ    '4 '
051600980831     C                   MOVEL     'I'           SACFIE
051700980831     C                   MOVEL     'S'           SACFFZ
051800100531     c                   if        Aggio='S'
051900100531     c     ksac3         chain     tfsac01l
052000100531     c                   endif
052100100531     c                   if        Aggio<>'S' or not %found(TFSAC01l)
052200100531     C                   Z-ADD     TO2F(yy)      SACTO2
052300100531     C                   Z-ADD     TO2F(yy)      SACTO3
052400100531     c                   else
052500100531     C                   add       TO2F(yy)      SACTO2
052600100531     C                   add       TO2F(yy)      SACTO3
052700100531     c                   endif
052800980831     C* OFFERTE E CONVALIDE FITTIZIE ESTERE
052900981002     C     WWRITE        WHENEQ    '4E'
053000980831     C                   MOVEL     'E'           SACFIE
053100980831     C                   MOVEL     'S'           SACFFZ
053200100531     c                   if        Aggio='S'
053300100531     c     ksac3         chain     tfsac01l
053400100531     c                   endif
053500100531     c                   if        Aggio<>'S' or not %found(TFSAC01l)
053600100531     C                   Z-ADD     OFEF(yy)      SACTO2
053700100531     C                   Z-ADD     COEF(yy)      SACTO3
053800100531     c                   else
053900100531     C                   add       OFEF(yy)      SACTO2
054000100531     C                   add       COEF(yy)      SACTO3
054100100531     c                   endif
054200980831     C                   ENDSL
054300020218      **
054400100531     c                   if        Aggio<>'S' or not %found(TFSAC01l)
054500940328     C                   WRITE     TFSAC000
054600100531     c                   else
054700100531     C                   update    TFSAC001
054800100531     c                   endif
054900940328     C                   ENDSR
055000940328     C*
055100940328     C* ROTTURA TIPO VISITA ------------------------------------------*
055200940328     C     ROTTPV        BEGSR
055300940328     C*
055400940328    1C     WTPV          IFNE      *BLANKS
055500940328     C* IMPOSTO CAMPI FISSI DA SCRIVERE
055600940328     C                   CLEAR                   TFSAC000
055700940328     C                   MOVEL     WCMM          SACCMM
055800980831     C                   MOVEL     AA            SACAAA
055900940328     C                   MOVEL     MM            SACMMM
056000940328    2C     WVCN          IFEQ      'N'
056100940328     C                   MOVEL     'N'           SACAPL
056200940328     C                   ELSE
056300940328     C                   MOVEL     'C'           SACAPL
056400940328    2C                   ENDIF
056500940328     C                   MOVEL     WTPV          SACTPV
056600041007     C                   clear                   SACFTR
056700041007     C                   z-add     dateu         SACDTR
056800940328     C*
056900020218    2C                   DO        5             yy                2 0
057000980831     C* LE VISITE IN UN RECORD A PARTEN
057100020218    3C     TO1(yy)       IFGT      0
057200981002     C                   MOVEL     '1 '          WWRITE            2
057300980831     C                   EXSR      SCRIVI
057400980831    3C                   ENDIF
057500020218     C     TO2(yy)       IFGT      0
057600020218     C     TO3(yy)       ORGT      0
057700981002     C                   MOVEL     '2 '          WWRITE
057800981002     C                   EXSR      SCRIVI
057900981002     C                   ENDIF
058000020218     C     OFE(yy)       IFGT      0
058100020218     C     COE(yy)       ORGT      0
058200981002     C                   MOVEL     '2E'          WWRITE
058300940328     C                   EXSR      SCRIVI
058400940328    3C                   ENDIF
058500020218    3C     TO1F(yy)      IFGT      0
058600981002     C                   MOVEL     '3 '          WWRITE
058700980831     C                   EXSR      SCRIVI
058800980831    3C                   ENDIF
058900020218     C     TO2F(yy)      IFGT      0
059000020218     C     TO3F(yy)      ORGT      0
059100981002     C                   MOVEL     '4 '          WWRITE
059200981002     C                   EXSR      SCRIVI
059300981002    3C                   ENDIF
059400020218     C     OFEF(yy)      IFGT      0
059500020218     C     COEF(yy)      ORGT      0
059600981002     C                   MOVEL     '4E'          WWRITE
059700980831     C                   EXSR      SCRIVI
059800980831    3C                   ENDIF
059900940328    2C                   ENDDO
060000940328     C*
060100940328    1C                   ENDIF
060200940328     C*
060300940328     C* SE TIPO VISITA PER ENTRAMBI MA CAMBIA SUBITO IL TIPO VISITA
060400940328     C*  NON ESEGUO IL 2 GIRO
060500940328    1C   01VISTPV        IFNE      WTPV
060600940328     C                   SETOFF                                       01
060700940328    1C                   ENDIF
060800940328     C*
060900940328     C* CHAIN NUOVO TIPO VISITA
061000940328    1C     *IN01         IFEQ      *OFF
061100101222     c                   clear                   tibs02ds
061200101222     c                   clear                   dttr
061300101222     c                   eval      t02mod = 'C'
061400101222     c                   eval      t02sif = knsif
061500101222     c                   eval      t02cod = 'TTR'
061600101222     c                   movel     vistpv        t02ke1
061700101222     c                   call      'TIBS02R'
061800101222     c                   parm                    kpjba
061900101222     c                   parm                    tibs02ds
062000101222     c                   if        t02err = *blanks
062100101222     c                   eval      dttr = t02uni
062200101222     c                   endif
062300101222     C**!!!              MOVEL     '1D'          COD
062400101222     C**!!!              MOVEL     *BLANKS       KEY
062500101222     C**!!!              MOVEL     VISTPV        KEY
062600101222     C**!! KTAB          CHAIN     TABEL                              31
062700101222     C*!N31              MOVEL     TBLUNI        DS1D
062800940328     C* SE VALE PER CLIENTI NUOVI E NON DEVO FARE 2 GIRI DI ELABORAZ
062900101222    2C**!!!§1DVCN        IFEQ      'E'
063000101222    2c                   IF        §ttrvcn = 'E'
063100940328     C* PRIMA ELABORO SEMPRE I CLIENTI NUOVI
063200101222     C                   MOVEL     'N'           WVCN              1
063300940328     C                   SETON                                        01
063400940328   X2C                   ELSE
063500101222     C**!!!              MOVEL     §1DVCN        WVCN
063600101222     c                   eval      wvcn = §ttrvcn
063700940328    2C                   ENDIF
063800940328     C*
063900940328   X1C                   ELSE
064000940328     C                   MOVEL     'O'           WVCN
064100940328     C                   SETOFF                                       01
064200940328    1C                   ENDIF
064300940328     C* PULIZIA SCHIERE
064400940328     C                   CLEAR                   TO1
064500940328     C                   CLEAR                   TO2
064600940328     C                   CLEAR                   TO3
064700980831     C                   CLEAR                   OFE
064800980831     C                   CLEAR                   COE
064900980831     C                   CLEAR                   TO1F
065000980831     C                   CLEAR                   TO2F
065100980831     C                   CLEAR                   TO3F
065200980831     C                   CLEAR                   OFEF
065300980831     C                   CLEAR                   COEF
065400940328     C*
065500940328     C                   MOVEL     VISTPV        WTPV
065600940328     C                   ENDSR
065700100527     C*
065800100528     C*  Lettura visite clienti file CRM ----------------------------*
065900100527     C     NewVis        BEGSR
066000100528     C                   CLEAR                   WTPV
066100100531     c                   clear                   dataIni
066200100531     c                   clear                   dataFin
066300100531     c                   movel     wdata         dataIni
066400100527     c                   move      01            dataIni           8 0
066500100531     c                   movel     wdata         dataFin
066600100527     c                   move      31            datafin           8 0
066700100527     c
066800100528     c* CLIENTI NUOVI
066900100527     c                   eval      wrkgetlista =
067000100531     c                             'select atccmm, atcnrv, atccpo, atcksc +
067100100528     c                             FROM tiatc00f  +
067200100527     c                             where atctat =''A'' and atcdad between ' +
067300100528     c                             %editc(Dataini:'X') + ' and ' +
067400100528     c                             %editc(dataFin:'X') + ' and   +
067500100528     c                             atcest =''S'' and atcksc = 0 ORDER BY atccmm'
067600100528     C/EXEC SQL
067700100528     C+ PREPARE s1 FROM :wrkgetlista
067800100528     C/END-EXEC
067900100528     C
068000100528     C/EXEC SQL
068100100528     C+ DECLARE A1 CURSOR FOR S1
068200100528     C/END-EXEC
068300100528     C
068400100528     C/EXEC SQL
068500100528     C+ OPEN a1
068600100528     C/END-EXEC
068700100528
068800100528      *?Leggo il file
068900100528     c                   do        *hival
069000100528     C/EXEC SQL
069100100531     C+ FETCH NEXT FROM a1 INTO :atccmm, :atcnrv, :atccpo, :atcksc
069200100528     C/END-EXEC
069300100528     c                   select
069400100528
069500100528     c                   when      sqlcod = 100
069600100528     c                   leave
069700100528     c                   when      sqlcod < 0
069800100528     c                   seton                                        H1
069900100528     c                   other
070000100528     c                   exsr      contaApp
070100100528     c                   endsl
070200100528
070300100528     c                   enddo
070400100528     C
070500100528     C* ULTIMA ROTTURA TIPO VISITA
070600100528     C                   EXSR      ROTTPV
070700100528
070800100528     C/EXEC SQL
070900100528     C+ CLOSE a1
071000100528     C/END-EXEC
071100100528
071200100528     c* CLIENTI ESISTENTI
071300100528     c                   eval      wrkgetlista =
071400100531     c                             'select atccmm, atcnrv, atccpo, atcksc +
071500100528     c                             FROM tiatc00f  +
071600100528     c                             where atctat =''A'' and atcdad between ' +
071700100528     c                             %editc(DataIni:'X') + ' and ' +
071800100528     c                             %editc(DataFin:'X') + ' and   +
071900100528     c                             atcest =''S'' and atcksc > 0 ORDER BY atccmm'
072000100528     C/EXEC SQL
072100100528     C+ PREPARE s2 FROM :wrkgetlista
072200100528     C/END-EXEC
072300100528     C
072400100528     C/EXEC SQL
072500100528     C+ DECLARE A2 CURSOR FOR S2
072600100528     C/END-EXEC
072700100528     C
072800100528     C/EXEC SQL
072900100528     C+ OPEN a2
073000100528     C/END-EXEC
073100100528
073200100528      *?Leggo il file
073300100528     C                   CLEAR                   WTPV
073400100528     c                   do        *hival
073500100528     C/EXEC SQL
073600100531     C+ FETCH NEXT FROM a2 INTO :atccmm, :atcnrv, :atccpo, :atcksc
073700100528     C/END-EXEC
073800100528     c                   select
073900100528
074000100528     c                   when      sqlcod = 100
074100100528     c                   leave
074200100528     c                   when      sqlcod < 0
074300100528     c                   seton                                        H1
074400100528     c                   other
074500100528     c                   exsr      contaApp
074600100528     c                   endsl
074700100528
074800100528     c                   enddo
074900100528     C
075000100528     C* ULTIMA ROTTURA TIPO VISITA
075100100528     C                   EXSR      ROTTPV
075200100528
075300100528     C/EXEC SQL
075400100528     C+ CLOSE a2
075500100528     C/END-EXEC
075600100527     C                   ENDSR
075700100528     c
075800100528     C* Conto gli appuntamenti su TIATC ------------------------------*
075900100528     c     contaApp      BEGSR
076000100528
076100100528
076200100528     c     atcnrv        chain     tivis05l
076300100528     c                   if        not %found(tivis05l)
076400100528
076500100528     c                   if        atcksc=0
076600100528     c                   eval      vistpv='N'
076700100528     c                   else
076800100528     c                   eval      vistpv='A'
076900100528     c                   endif
077000100528     c
077100101019     c***                else
077200101019     c***                eval      vistpv=Wistpv
077300100528     c                   endif
077400100528     C* ROTTURA COMMERCIALE
077500100528    2C     atcCMM        IFNE      WCMM
077600100528     C     WCMM          IFGT      0
077700100528     C                   EXSR      ROTTPV
077800100528     C                   ENDIF
077900100528
078000100528     C                   MOVEL     atcCMM        WCMM
078100100528    2C                   ENDIF
078200100528     c
078300100528     C* ROTTURA TIPO VISITA
078400100528     C* O PER CLI > 0  SE TIPO VISITA VALE SIA PER CLI NEW CHE OLD
078500100528    2C     VISTPV        IFNE      WTPV
078600100528     C     *IN01         OREQ      *ON
078700100528     C     atcKSC        ANDGT     0
078800100528     C                   EXSR      ROTTPV
078900100528    2C                   ENDIF
079000100528     c
079100100531     c                   eval      Wksc=atcksc
079200100531     c                   eval      Wcpo=atccpo
079300100531     c                   eval      Wnrv=atcnrv
079400100528     c                   exsr      IMPCli
079500100528
079600100528     C                   ADD       1             TO1(yy)
079700100528     c
079800100528     c                   ENDSR
079900100528     C*
080000100531     C*  Lettura Offerte ITALIA clienti file CRM -----------------------*
080100100528     C     NewOff        BEGSR
080200100528     C                   CLEAR                   WTPV
080300100531     C                   CLEAR                   WCMM
080400100528     c* Offerte italia
080500100528     c                   eval      wrkgetlista =
080600100528     c                             'select +
080700100528     c                             distinct min(vofdpo),  vofnrv +
080800101222     c                             from TIVOF11L where voftpt = ''I'' +
080900101222     c                             group by vofnrv +
081000101222     c                             order by vofnrv'
081100100528     C/EXEC SQL
081200100528     C+ PREPARE s3 FROM :wrkgetlista
081300100528     C/END-EXEC
081400100528     C
081500100528     C/EXEC SQL
081600100528     C+ DECLARE A3 CURSOR FOR S3
081700100528     C/END-EXEC
081800100528     C
081900100528     C/EXEC SQL
082000100528     C+ OPEN a3
082100100528     C/END-EXEC
082200100528
082300100528      *?Leggo il file
082400100531    1c                   do        *hival
082500100528     C/EXEC SQL
082600100531     C+ FETCH NEXT FROM a3 INTO :primaoff, :vofnrv
082700100528     C/END-EXEC
082800100531    2c                   select
082900100528
083000100528     c                   when      sqlcod = 100
083100100528     c                   leave
083200100528     c                   when      sqlcod < 0
083300100528     c                   seton                                        H1
083400100528     c                   other
083500100528
083600100528     c* Solo prima visita nella data elaborazione
083700100528     C                   MOVEL     primaOFF      vofOI             6 0
083800100531     C                   MOVEL     primaOFF      alfaOFF           8
083900100531    3c                   if        vofOI=WDATA
084000100528     c* chain TIVIS
084100100528     c     vofnrv        chain     tivis05l
084200101019    4c                   if        %found(tivis05l) and visffz=' '
084300100531     c
084400101019     c***                eval      vistpv=Wistpv
084500100531     c* CONTEGGIO: rottura codice
084600101019    6C     visCMM        IFNE      WCMM
084700100531     C     WCMM          IFGT      0
084800100531     C                   EXSR      ROTTPV
084900100531     C                   ENDIF
085000100531
085100101019     C                   MOVEL     visCMM        WCMM
085200100531    6C                   ENDIF
085300100531     c
085400100531     C* ROTTURA TIPO VISITA
085500100531     C* O PER CLI > 0  SE TIPO VISITA VALE SIA PER CLI NEW CHE OLD
085600100531    6C     VISTPV        IFNE      WTPV
085700100531     C     *IN01         OREQ      *ON
085800101019     C     vISKSC        ANDGT     0
085900100531     C                   EXSR      ROTTPV
086000100531    6C                   ENDIF
086100100531     c
086200101019     c                   eval      Wksc=visksc
086300101019     c                   eval      Wcpo=viscpo
086400101019     c                   eval      Wnrv=visnrv
086500100531     c                   exsr      IMPCli
086600100531
086700100531     C                   ADD       1             TO2(yy)
086800100531     c
086900100531    4c                   endif
087000100531    3c                   endif
087100100528     c
087200100531    2c                   endsl
087300100528
087400100531    1c                   enddo
087500100528     C
087600100528     C* ULTIMA ROTTURA TIPO VISITA
087700100528     C                   EXSR      ROTTPV
087800100528
087900100528     C/EXEC SQL
088000100528     C+ CLOSE a3
088100100528     C/END-EXEC
088200100531     c                   ENDSR
088300100531     C*
088400100531     C*  Lettura Offerte ESTERE cclienti file CRM ---------------------------*
088500100531     C     NewEstOFF     BEGSR
088600100531     C                   CLEAR                   WTPV
088700100531     C                   CLEAR                   WCMM
088800100531     c* Offerte Estere
088900100531     c                   eval      wrkgetlista =
089000100531     c                             'select +
089100100531     c                             distinct min(vofdpo),  vofnrv +
089200101222     c                             from TIVOF11L where voftpt <> ''I'' +
089300101222     c                             group by vofnrv +
089400101222     c                             order by vofnrv'
089500100531     C/EXEC SQL
089600100531     C+ PREPARE s4 FROM :wrkgetlista
089700100531     C/END-EXEC
089800100531     C
089900100531     C/EXEC SQL
090000100531     C+ DECLARE A4 CURSOR FOR S4
090100100531     C/END-EXEC
090200100531     C
090300100531     C/EXEC SQL
090400100531     C+ OPEN a4
090500100531     C/END-EXEC
090600100531
090700100531      *?Leggo il file
090800100531     c                   do        *hival
090900100531     C/EXEC SQL
091000100531     C+ FETCH NEXT FROM a4 INTO :primaoff, :vofnrv
091100100531     C/END-EXEC
091200100531     c                   select
091300100531
091400100531     c                   when      sqlcod = 100
091500100531     c                   leave
091600100531     c                   when      sqlcod < 0
091700100531     c                   seton                                        H1
091800100531     c                   other
091900100531
092000100531     c* Solo prima visita nella data elaborazione
092100100531     C                   MOVEL     primaOFF      vofOI             6 0
092200100531     C                   MOVEL     primaOFF      alfaOFF           8
092300100531     c                   if        vofOI=WDATA
092400100531     c* chain TIVIS
092500100531     c     vofnrv        chain     tivis05l
092600101019     c                   if        %found(tivis05l) and visffz=' '
092700100531     c
092800101019     c***                eval      vistpv=Wistpv
092900100531     c* CONTEGGIO: rottura codice
093000101019    2C     visCMM        IFNE      WCMM
093100100531     C     WCMM          IFGT      0
093200100531     C                   EXSR      ROTTPV
093300100531     C                   ENDIF
093400100531
093500101019     C                   MOVEL     visCMM        WCMM
093600100531    2C                   ENDIF
093700100531     c
093800100531     C* ROTTURA TIPO VISITA
093900100531     C* O PER CLI > 0  SE TIPO VISITA VALE SIA PER CLI NEW CHE OLD
094000100531    2C     VISTPV        IFNE      WTPV
094100100531     C     *IN01         OREQ      *ON
094200101019     C     VISKSC        ANDGT     0
094300100531     C                   EXSR      ROTTPV
094400100531    2C                   ENDIF
094500100531     c
094600101019     c                   eval      Wksc=Visksc
094700101019     c                   eval      Wcpo=Viscpo
094800101019     c                   eval      Wnrv=Visnrv
094900100531     c                   exsr      IMPCli
095000100531
095100100531     C                   ADD       1             OFE(yy)
095200100531     c
095300100531     c                   endif
095400100531     c                   endif
095500100531     c
095600100531     c                   endsl
095700100531
095800100531     c                   enddo
095900100531     C
096000100531     C* ULTIMA ROTTURA TIPO VISITA
096100100531     C                   EXSR      ROTTPV
096200100531
096300100531     C/EXEC SQL
096400100531     C+ CLOSE a4
096500100531     C/END-EXEC
096600100531     c                   ENDSR
096700100531     C*
096800100531     C*  Lettura Offerte ITALIA CONVALIDATE clienti file CRM -----------------*
096900100531     C     NewConval     BEGSR
097000100531     C                   CLEAR                   WTPV
097100100531     C                   CLEAR                   WCMM
097200100531     c* Offerte italia convalidate
097300100531     c                   eval      wrkgetlista =
097400100531     c                             'select +
097500100531     c                             distinct min(vofdCo),  vofnrv +
097600101222     c                             from TIVOF11L where voftpt = ''I'' +
097700100531     c                             and vofeso=''C''  +
097800101222     c                             group by vofnrv +
097900101222     c                             order by vofnrv'
098000100531     C/EXEC SQL
098100100531     C+ PREPARE s5 FROM :wrkgetlista
098200100531     C/END-EXEC
098300100531     C
098400100531     C/EXEC SQL
098500100531     C+ DECLARE A5 CURSOR FOR S5
098600100531     C/END-EXEC
098700100531     C
098800100531     C/EXEC SQL
098900100531     C+ OPEN a5
099000100531     C/END-EXEC
099100100531
099200100531      *?Leggo il file
099300100531     c                   do        *hival
099400100531     C/EXEC SQL
099500100531     C+ FETCH NEXT FROM a5 INTO :primaCon, :vofnrv
099600100531     C/END-EXEC
099700100531     c                   select
099800100531
099900100531     c                   when      sqlcod = 100
100000100531     c                   leave
100100100531     c                   when      sqlcod < 0
100200100531     c                   seton                                        H1
100300100531     c                   other
100400100531
100500100531     c* Solo prima visita nella data elaborazione
100600100531     C                   MOVEL     primaCON      vofOI             6 0
100700100531     C                   MOVEL     primaCON      alfaCON           8
100800100531     c                   if        vofOI=WDATA
100900100531     c* chain TIVIS
101000100531     c     vofnrv        chain     tivis05l
101100101019     c                   if        %found(tivis05l) and Visffz=' '
101200100531     c
101300101019     c***                eval      vistpv=Wistpv
101400100531     c* CONTEGGIO: rottura codice
101500101019    2C     VisCMM        IFNE      WCMM
101600100531     C     WCMM          IFGT      0
101700100531     C                   EXSR      ROTTPV
101800100531     C                   ENDIF
101900100531
102000101019     C                   MOVEL     VisCMM        WCMM
102100100531    2C                   ENDIF
102200100531     c
102300100531     C* ROTTURA TIPO VISITA
102400100531     C* O PER CLI > 0  SE TIPO VISITA VALE SIA PER CLI NEW CHE OLD
102500100531    2C     VISTPV        IFNE      WTPV
102600100531     C     *IN01         OREQ      *ON
102700101019     C     VISKSC        ANDGT     0
102800100531     C                   EXSR      ROTTPV
102900100531    2C                   ENDIF
103000100531     c
103100101019     c                   eval      Wksc=Visksc
103200101019     c                   eval      Wcpo=Viscpo
103300101019     c                   eval      Wnrv=Visnrv
103400100531     c                   exsr      IMPCli
103500100531
103600100531     C                   ADD       1             TO3(yy)
103700100531     c
103800100531     c                   endif
103900100531     c                   endif
104000100531     c
104100100531     c                   endsl
104200100531
104300100531     c                   enddo
104400100531     C
104500100531     C* ULTIMA ROTTURA TIPO VISITA
104600100531     C                   EXSR      ROTTPV
104700100531
104800100531     C/EXEC SQL
104900100531     C+ CLOSE a5
105000100531     C/END-EXEC
105100100531     c                   ENDSR
105200100531     C*
105300100531     C*  Lettura Offerte Estere CONVALIDATE clienti file CRM -----------------*
105400100531     C     NewEstCon     BEGSR
105500100531     C                   CLEAR                   WTPV
105600100531     C                   CLEAR                   WCMM
105700100531     c* Offerte Estere convalidate
105800100531     c                   eval      wrkgetlista =
105900100531     c                             'select +
106000100531     c                             distinct min(vofdCo),  vofnrv +
106100101222     c                             from TIVOF11L where voftpt <> ''I'' +
106200100531     c                             and vofeso=''C''  +
106300101222     c                             group by vofnrv +
106400101222     c                             order by vofnrv'
106500100531     C/EXEC SQL
106600100531     C+ PREPARE s6 FROM :wrkgetlista
106700100531     C/END-EXEC
106800100531     C
106900100531     C/EXEC SQL
107000100531     C+ DECLARE A6 CURSOR FOR S6
107100100531     C/END-EXEC
107200100531     C
107300100531     C/EXEC SQL
107400100531     C+ OPEN a6
107500100531     C/END-EXEC
107600100531
107700100531      *?Leggo il file
107800100531     c                   do        *hival
107900100531     C/EXEC SQL
108000100531     C+ FETCH NEXT FROM a6 INTO :primaCon, :vofnrv
108100100531     C/END-EXEC
108200100531     c                   select
108300100531
108400100531     c                   when      sqlcod = 100
108500100531     c                   leave
108600100531     c                   when      sqlcod < 0
108700100531     c                   seton                                        H1
108800100531     c                   other
108900100531
109000100531     c* Solo prima visita nella data elaborazione
109100100531     C                   MOVEL     primaCON      vofOI             6 0
109200100531     C                   MOVEL     primaCON      alfaCON           8
109300101019    1c                   if        vofOI=WDATA
109400100531     c* chain TIVIS
109500100531     c     vofnrv        chain     tivis05l
109600101019    2c                   if        %found(tivis05l) and Visffz=' '
109700100531     c
109800101019     c****               eval      vistpv=Vistpv
109900100531     c* CONTEGGIO: rottura codice
110000101019    2C     VisCMM        IFNE      WCMM
110100100531     C     WCMM          IFGT      0
110200100531     C                   EXSR      ROTTPV
110300100531     C                   ENDIF
110400100531
110500101019     C                   MOVEL     VisCMM        WCMM
110600100531    2C                   ENDIF
110700100531     c
110800100531     C* ROTTURA TIPO VISITA
110900100531     C* O PER CLI > 0  SE TIPO VISITA VALE SIA PER CLI NEW CHE OLD
111000100531    2C     VISTPV        IFNE      WTPV
111100100531     C     *IN01         OREQ      *ON
111200101019     C     VISKSC        ANDGT     0
111300100531     C                   EXSR      ROTTPV
111400100531    2C                   ENDIF
111500100531     c
111600101019     c                   eval      Wksc=Visksc
111700101019     c                   eval      Wcpo=Viscpo
111800101019     c                   eval      Wnrv=Visnrv
111900100531     c                   exsr      IMPCli
112000100531
112100100531     C                   ADD       1             COE(yy)
112200100531     c
112300101019    2c                   endif
112400101019    1c                   endif
112500100531     c
112600101019   0ac                   endsl
112700100531
112800101019    0c                   enddo
112900100531     C
113000100531     C* ULTIMA ROTTURA TIPO VISITA
113100100531     C                   EXSR      ROTTPV
113200100531
113300100531     C/EXEC SQL
113400100531     C+ CLOSE a6
113500100531     C/END-EXEC
113600100531     c                   ENDSR
113700100528     c*
113800100528     C* cerco importanza ciente --------------------------------------*
113900100528     C     IMPCli        BEGSR
114000100528     c                   clear                   wclv              1
114100100528     c
114200100528     C* anagrafica Provvisoria
114300100531     C                   MOVEL     WNRV          KSC
114400100528     C     KCLP          CHAIN     TFCLP000
114500100528     c                   if        %found(tfclp00f) and clpclv<>*blanks
114600100528     c                   eval      WCLV= clpclv
114700100528     c                   endif
114800100528     c
114900100528     C* anagrafica Buona
115000100531     c                   if        wclv=' ' and Wksc >0
115100100531     C                   MOVEL     Wksc          KSC
115200100528     C     KCLP          CHAIN     CNCLP000                           31
115300100528     c                   if        %found(cnclp00f) and clpclv<>*blanks
115400100528     c                   eval      WCLV= clpclv
115500100528     c                   endif
115600100528     c                   endif
115700100528     c* anagrafica potenziale
115800100531     c                   if        wclv=' ' and Wcpo >0
115900100528     c     atccpo        chain     tncpo01l
116000100528     c                   if        %found(tncpo01l)
116100100528     c                   eval      WCLv= cpoftr
116200100528     c                   endif
116300100528     c                   endif
116400100528     c
116500100528     c                   if        wclv=' '
116600100528     c                   eval      WCLV= 'C'
116700100528     c                   endif
116800100528      *
116900100528      *
117000100528     c                   SELECT
117100100528     c                   WHEN      wCLV = 'A'
117200100528     c                   z-add     1             yy
117300100528     c                   WHEN      wCLV = 'B'
117400100528     c                   z-add     2             yy
117500100528     c                   WHEN      wCLV = 'C'
117600100528     c                   z-add     3             yy
117700100528     c                   WHEN      wCLV = 'T'
117800100528     c                   z-add     4             yy
117900100528     c                   WHEN      wCLV = 'D'
118000100528     c                   z-add     5             yy
118100100528     c                   ENDSL
118200100528      *
118300100528     C*
118400100528     C                   ENDSR
118500020321********
118600020321*******ELABORAZIONE SALDI ATTIVITA' COMMERCIALE
118700020321*******Eseguita nel terminal di partenza di : xxx
118800020321*******il giorno xx/xx/xx    alle ore xx:xx
