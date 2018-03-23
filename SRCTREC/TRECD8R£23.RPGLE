000100991116     H*------------------------------------------------------------------------*
000200950303     H*                                                                        *
000300940906     H*   PARTENDO DALLA DS ESTERNA PASSATA DAL PGM DI LETTURA                 *
000400091007     H*   DELLA DATAQ, CALCOLA i saldi di quantità x tempi uff                 *
000500940906     H*                                                                        *
000600940906     H*   PARAMETRI UTILIZZATI DAL PGM:                                        *
000700940906     H*   INPUT: - DS ESTERNA 'DS$ECO' :                                       *
000800940906     H*            - PARAMETRI COMUNI                                          *
000900940906     H*            - IMMAGINE PRIMA (NON USATO)                                *
001000940906     H*            - IMMAGINE DOPO                                             *
001100941201     H*          - SCHIERA CODICI RICAVI DOPO                                  *
001200941006     H*          - SCHIERA IMPORTI RICAVI DOPO                                 *
001300940906     H*   OUTUT: - SCHIERA CODICI COMPETENZE DOPO                              *
001400940906     H*          - SCHIERA IMPORTI COMPETENZE DOPO                             *
001500940906     H*                                                                        *
001600940906     H*------------------------------------------------------------------------*
001700930413     H DECEDIT('0,') DATEDIT(*DMY.)
001800940906     F*------------------------------------------------------------------------*
001900940906     F* DATA BASE
002000940906     F*------------------------------------------------------------------------*
002100121120     FEsvoc01L  UF A E           K DISK    usropn
002200121120     FEsvoc11L  UF A E           K DISK    usropn
002300091007     FECECT01L  IF   E           K DISK
002400091007     FECEVT01L  IF   E           K DISK
002500131002     Fecevd06l  IF   E           K DISK
002600940831     FECEVD02L  IF   E           K DISK
002700010807     F                                     RENAME(ECEVD000:ECEVD2)
002800940902     FECEVD03L  IF   E           K DISK
002900010807     F                                     RENAME(ECEVD000:ECEVD3)
003000940905     FECEVD04L  IF   E           K DISK
003100010807     F                                     RENAME(ECEVD000:ECEVD4)
003200940524     FECEVM01L  IF   E           K DISK
003300950123     FECERR01L  UF A E           K DISK
003400941024     FTABEL00F  IF   E           K DISK    USROPN
003500011221     FTNTBE01L  IF   E           K DISK    USROPN
003600020805     FAZORG01L  IF   E           K DISK    USROPN
003700941229     F*PRTF198 O   F     198     OF    LPRINTER                        UC
003800940906     D*------------------------------------------------------------------------*
003900941229     D*PRTF198  66FL 60OL
004000940906     D*------------------------------------------------------------------------*
004100940906     D* SCHIERE
004200940906     D*------------------------------------------------------------------------*
004300940906     D*-------------------
004400930304     D* MESSAGGI
004500940906     D*-------------------
004600020805     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
004700940906     D*-------------------
004800940704     D* SCHIERE RICAVI IN INPUT AL PGM
004900940906     D*-------------------
005000030506     D RCD             S              3  0 DIM(75)
005100090914     D RIM             S             13  5 DIM(75)
005200940906     D*-------------------
005300940704     D* SCHIERA COMPETENZE IN OUTPUT AL PGM
005400940906     D*-------------------
005500030506     D CCD             S              3  0 DIM(75)
005600090914     D CIM             S             13  5 DIM(75)
005700940906     D*-------------------
005800940704     D* SCHIERA COMPETENZE (INDICE=CODICE COMPETENZA)
005900940906     D*-------------------
006000090914     D CIMP            S             13  5 DIM(999)
006100940906     D*-------------------
006200940525     D* SCHIERA VOCI (INDICE=CODICE VOCE)
006300940906     D*-------------------
006400940704     D VPRG            S              3  0 DIM(999)
006500940704     D VTVO            S              1    DIM(999)
006600940721     D VDDV            S              8  0 DIM(999)
006700940721     D VDSV            S              8  0 DIM(999)
006800940707     D VVRV            S              2    DIM(999)
006900940714     D VTIV            S              2    DIM(999)
007000940603     D VSPT            S              1    DIM(999)
007100940603     D VCMP            S              3  0 DIM(999)
007200940607     D VRFI            S              2    DIM(999)
007300940607     D VRFE            S              2    DIM(999)
007400940607     D VRTI            S              2    DIM(999)
007500940607     D VRTE            S              2    DIM(999)
007600940607     D VRPI            S              2    DIM(999)
007700940607     D VRPE            S              2    DIM(999)
007800940607     D VRKI            S              2    DIM(999)
007900940607     D VRKE            S              2    DIM(999)
008000940704     D VARF            S             11  3 DIM(999)
008100940704     D VARL            S             11  3 DIM(999)
008200940704     D VARO            S             11  3 DIM(999)
008300090914     D VIMP            S             13  5 DIM(999)
008400030113     D VCOP            S              1    DIM(999)
008500030113     D VDAT            S              1    DIM(999)
008600030113     D VAID            S              1    DIM(999)
008700030422     D VRPV            S              3  1 DIM(999)
008800030422     D VSPS            S              1    DIM(999)
008900940603     D VC              S              1    DIM(999)
009000940607     D VV              S              1    DIM(999)
009100991027     D VS              S              1    DIM(999)
009200091009     D vric            S                   DIM(999) like(ectric)
009300091009     D vfil            S              3  0 DIM(999)
009400091009     D vqta            S             13  5 DIM(999)
009500091009     D vsgi            S             13  3 DIM(999)
009600091009     D vsgf            S             13  3 DIM(999)
009700940829     D*-------------------
009800940829     D* SCHIERE TARIFFE DI CARTELLO
009900940829     D*-------------------
010000021111     D CVOC            S              3  0 DIM(300)
010100021111     D CPRG            S              3  0 DIM(300)
010200021111     D CFT1            S              3  0 DIM(300)
010300021111     D CFT2            S              3  0 DIM(300)
010400021111     D CCLI            S              7  0 DIM(300)
010500021111     D CCTR            S              3  0 DIM(300)
010600021111     D CCTS            S              2    DIM(300)
010700131002     D Ccad            S              9    DIM(300)
010800131002     D Cnaz            S              3    DIM(300)
010900021111     D CDDT            S              8  0 DIM(300)
011000021111     D CDST            S              8  0 DIM(300)
011100021111     D CSGI            S             13  3 DIM(300)
011200021111     D CSGF            S             13  3 DIM(300)
011300090914     D CITR            S             13  5 DIM(300)
011400090914     D CIMI            S             13  5 DIM(300)
011500090914     D CIMA            S             13  5 DIM(300)
011600021111     D CSEG            S              1    DIM(300)
011700021111     D CPER            S              5  2 DIM(300)
011800021111     D CRPV            S              3  1 DIM(300)
011900060313     D DSVN            S              1    DIM(20)
012000940902     D*-------------------
012100940902     D* SCHIERE TARIFFE DELL'ULTIMA FILAILE DEL CLIENTE
012200940902     D*-------------------
012300021111     D FVOC            S              3  0 DIM(600)
012400021111     D FPRG            S              3  0 DIM(600)
012500021111     D FFT1            S              3  0 DIM(600)
012600021111     D FFT2            S              3  0 DIM(600)
012700021111     D FCLI            S              7  0 DIM(600)
012800021111     D FCTR            S              3  0 DIM(600)
012900021111     D FCTS            S              2    DIM(600)
013000131002     D Fcad            S              9    DIM(600)
013100131002     D Fnaz            S              3    DIM(600)
013200021111     D FDDT            S              8  0 DIM(600)
013300021111     D FDST            S              8  0 DIM(600)
013400021111     D FSGI            S             13  3 DIM(600)
013500021111     D FSGF            S             13  3 DIM(600)
013600090914     D FITR            S             13  5 DIM(600)
013700090914     D FIMI            S             13  5 DIM(600)
013800090914     D FIMA            S             13  5 DIM(600)
013900021111     D FSEG            S              1    DIM(600)
014000021111     D FPER            S              5  2 DIM(600)
014100021111     D FRPV            S              3  1 DIM(600)
014200940831     D*-------------------
014300940831     D* SCHIERE TARIFFE DELL'ULTIMO CLIENTE
014400940831     D*-------------------
014500021111     D KVOC            S              3  0 DIM(300)
014600021111     D KPRG            S              3  0 DIM(300)
014700021111     D KFT1            S              3  0 DIM(300)
014800021111     D KFT2            S              3  0 DIM(300)
014900021111     D KCLI            S              7  0 DIM(300)
015000021111     D KCTR            S              3  0 DIM(300)
015100021111     D KCTS            S              2    DIM(300)
015200131002     D Kcad            S              9    DIM(300)
015300131002     D Knaz            S              3    DIM(300)
015400021111     D KDDT            S              8  0 DIM(300)
015500021111     D KDST            S              8  0 DIM(300)
015600021111     D KSGI            S             13  3 DIM(300)
015700021111     D KSGF            S             13  3 DIM(300)
015800090914     D KITR            S             13  5 DIM(300)
015900090914     D KIMI            S             13  5 DIM(300)
016000090914     D KIMA            S             13  5 DIM(300)
016100021111     D KSEG            S              1    DIM(300)
016200021111     D KPER            S              5  2 DIM(300)
016300021111     D KRPV            S              3  1 DIM(300)
016400940906     D*-------------------
016500940906     D* SCHIERE MAGGIORAZIONI
016600940906     D*-------------------
016700041118     D MVOM            S              3  0 DIM(600)
016800041118     D MPRM            S              3  0 DIM(600)
016900041118     D MVOC            S              3  0 DIM(600)
017000041118     D MPRG            S              3  0 DIM(600)
017100100408     D Mviso           S              3  0 DIM(600)
017200100408     D Mddti           S              8  0 DIM(600)
017300100408     D Mdsti           S              8  0 DIM(600)
017400100408     D Mapp            S              1    DIM(600)
017500940906     D*-------------------
017600940906     D* CODICI CLIENTI CHE HANNO UNA TARIFFA PARTICOLARE (PER VELOCIZZARE)
017700940906     D*-------------------
017800070626     D KSC             S              7  0 DIM(5000)
017900940906     D*-------------------
018000940707     D* CODICI E QUANTITA TAB. 2R (TIPO TARIFFA)
018100940906     D*-------------------
018200940707     D C2R             S              2    DIM(100)
018300940707     D T2R             S              1    DIM(100)
018400010202     D Q2R             S             15  5 DIM(100)
018500940906     D*-------------------
018600940704     D* SCHIERE CODICI E IMPORTI IN INPUT E OUTPUT AL PGM
018700940906     D*-------------------
018800030506     D CAN             S              2    DIM(75)
018900030506     D PAN             S              2    DIM(75)
019000030506     D IAN             S              7    DIM(75)
019100030506     D COP             S              1    DIM(75)
019200030506     D DAT             S              1    DIM(75)
019300030506     D AID             S              1    DIM(75)
019400030506     D CMP             S              2    DIM(75)
019500940906     D*-------------------
019600940720     D* CODICI E GIORNI PAGAMENTO (TAB.FA)
019700940906     D*-------------------
019800940720     D CFA             S              3    DIM(100)
019900940720     D GFA             S              3  0 DIM(100)
020000020805     D*-------------------
020100020805     D* NETWORK
020200020805     D*-------------------
020300020805     D SNTW            S              3    DIM(20)
020400020805     D SNFIE           S              1    DIM(20)
020500971211     D*-------------------
020600971211     D* INDICE=COD.FILIALE ,   FL1 => BLANK=ITALIA,'E'=ESTERO
020700000623     D*-------------------
020800020503     D NTW             S              3    DIM(999)
020900020805     D FIE             S              1    DIM(999)
021000010116     D CTS             S              2    DIM(999)
021100000623     D*-------------------
021200000623     D* CONSEGNA ANOMALE
021300000623     D*-------------------
021400000623     D CCA             S              1    DIM(50)
021500000623     D CFPT            S              1    DIM(50)
021600000623     D CRMS            S              1    DIM(50)
021700010420     D*-------------------
021800010420     D* TRATTAMENTO MERCE
021900010420     D*-------------------
022000010420     D CTM             S              2    DIM(100)
022100010420     D CF12            S              1    DIM(100)
022200050801     D CF16            S              1    DIM(100)
022300030530     D CDKC            S              1    DIM(100)
022400131002     D*-------------------
022500131002     D* nazione
022600131002     D*-------------------
022700131002     D d15             S              3    DIM(1000)
022800131002     D ecf             S              1  0 DIM(1000)
022900131002     D pcf             S              1  0 DIM(1000)
023000131002     D*-------------------
023100020213     D* IMPORTANZA CLIENTE
023200020213     D*-------------------
023300020213     D CLV             S              1    DIM(10)
023400940906     D*------------------------------------------------------------------------*
023500940906     D* INPUT
023600940906     D*------------------------------------------------------------------------*
023700111020     D*-------------------
023800111020     D* DS x pgm £6
023900111020     D*-------------------
024000111020     D fnlv55ds      E DS
024100030422     D*-------------------
024200040903     D* DS DFE tabella tariffe fedex
024300030422     D*-------------------
024400040903     D Ddfe          E DS
024500121120     D*-------------------
024600121120     D* DS dsd$g informazioni x simulazione
024700121120     D*-------------------
024800121120     D Dsd$g         E DS
024900040903     D*-------------------
025000040903     D* DS LETTURA CAMPO EVTXXX
025100040903     D*-------------------
025200040903     D DEVTXXX       E DS
025300021028     D*-------------------
025400021028     D* DS LETTURA CAMPO TASFLO (D$DFLO)
025500021028     D*-------------------
025600021028     D DTASFLO       E DS
025700940906     D*-------------------
025800930323     D* DS ESTERNA IN INPUT AL PGM
025900940906     D*-------------------
026000940822     D DS$ECO        E DS
026100971222     D*-------------------
026200971222     D* DS PER LANCIO TIBS69R (RICERCA SU CNIND/CNCLP)
026300971222     D*-------------------
026400971222     D TIBS69        E DS                  EXTNAME(TIBS69DS)
026500971222     D ACODS         E DS                  EXTNAME(CNACO00F)
026600971222     D INDDS         E DS                  EXTNAME(CNIND00F)
026700971222     D CLPDS         E DS                  EXTNAME(CNCLP00F)
026800971222     D CLSDS         E DS                  EXTNAME(FNCLS00F)
026900940906     D*-------------------
027000991119     D* DS ESTERNA PER RIDEFINIRE IMMAGINE PRIMA E DOPO DI TITAS10F
027100940906     D*-------------------
027200941201     D TRECDS        E DS
027300940405     D  D$PDS1                 1    250
027400940405     D  D$PDS2               251    500
027500940405     D  D$DDS1               501    750
027600940405     D  D$DDS2               751   1000
027700941201     D  D$ODS1              1001   1250
027800941201     D  D$ODS2              1251   1500
027900940906     D*-------------------
028000940721     D* DS ESTERNA TABELLA 2R (TIPI RIFERIM. VOCE)
028100940906     D*-------------------
028200940721     D DS2R          E DS
028300940906     D*-------------------
028400940720     D* DS ESTERNA TABELLA FA (CONDIZIONI DI PAGAMENTO)
028500940906     D*-------------------
028600940720     D DSFA          E DS
028700000202     D*-------------------
028800000202     D* DS ESTERNA DESCRIZIONE 43 ORGANIGRAMMA
028900000202     D*-------------------
029000000202     D OG143         E DS
029100000623     D*-------------------
029200000623     D* DS ESTERNA TABELLA 7O (CONSEGNE ANOMALE)
029300000623     D*-------------------
029400000623     D DS7O          E DS
029500010420     D*-------------------
029600021029     D* DS ESTERNA TABELLA 1B (CODICI TRATTAMENTO MERCE)
029700010420     D*-------------------
029800010420     D DS1B          E DS
029900131002     D*-------------------
030000131002     D* DS ESTERNA TABELLA 15 (CODICI nazioni)
030100131002     D*-------------------
030200131002     D DS15          E DS
030300091007     D*-------------------
030400091007     D* DS ESTERNA TABELLA 2K (blocco competenza)
030500091007     D*-------------------
030600091007     D DS2k          E DS
030700011221     D*-------------------
030800011221     D* DS DIVISE PER CONVERSIONI
030900011221     D*-------------------
031000011221     D DSGEDCE       E DS
031100020102     D DGEDAC        E DS
031200020805     D*-------------------
031300020805     D* DS NETWORK
031400020805     D*-------------------
031500020805     D DNTW          E DS
031600011221     D*-------------------
031700011221     D* CONVERSIONI IMPORTI
031800011221     D*-------------------
031900011221     D YEUR          E DS                  EXTNAME(YEURCODS)
032000940906     D*-------------------
032100930325     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
032200940906     D*-------------------
032300940831     D DS§DAT          DS
032400940831     D  DSSEC                  1      2  0
032500940831     D  DSANN                  3      4  0
032600941201     D  DSAAS                  1      4  0
032700940831     D  DSMES                  5      6  0
032800940831     D  DSGIO                  7      8  0
032900941201     D  DSMGS                  5      8  0
033000940831     D  DSDAT6                 3      8  0
033100940831     D  DSDAT8                 1      8  0
033200940831     D DS§TIM          DS
033300940831     D  DTORA                  1      2  0
033400940831     D  DTMIN                  3      4  0
033500940901     D  DTSCN                  5      6  0
033600940831     D  DTGIO                  7      8  0
033700940831     D  DTMES                  9     10  0
033800940901     D  DTSEC                 11     12  0
033900940901     D  DTANN                 13     14  0
034000940831     D  DTORA6                 1      6  0
034100940901     D  DTTIME                 1     14  0
034200940906     D*-------------------
034300940906     D* DS PER PASSAGGIO CAMPI PACKED -CODICE-
034400940906     D*-------------------
034500940825     D                 DS
034600940825     D  DSWCN                  1      3P 0
034700940825     D  DSWCA                  1      3
034800090904     D  DSWIN                  4     13P 5
034900011221     D  DSWIA                  4     13
035000940906     D*-------------------
035100940729     D* DS PER MESSAGGIO MANCA TARIFFA
035200940906     D*-------------------
035300940729     D                 DS
035400940729     D  DSMSG                  1     80
035500941006     D  DSVOCE                29     31
035600941006     D  DSTT1                 33     35
035700941006     D  DSTT2                 37     39
035800950519     D  DSCTS                 41     42
035900941007     D  DSDER                  1     74
036000941007     D  DSCER                 75     78
036100011221     D*-------------------
036200011221     D* DS  CONTROLLO DATA (8)
036300011221     D*-------------------
036400011221     D WLBDA8          DS                  INZ
036500011221     D  G08DAT                 1      8  0
036600011221     D  G08INV                 9     16  0
036700011221     D  G08ERR                17     17
036800011221     D  G08TGI                18     22  0
036900091012     d* campi di comodo
037000091012     d pkfvoc          s                   like(vocpkf)
037100091012     d vlfvoc          s                   like(vocvlf)
037200091012     d banvoc          s                   like(vocban)
037300940803     C*------------------------------------------------------------------------*
037400940906     C*?MAIN LINE
037500940803     C*------------------------------------------------------------------------*
037600931105     C*
037700931105     C* ESEGUE ROUTINE INIZIALE
037800931105     C                   EXSR      RUTINZ
037900930602     C*
038000940405     C* SE NEL CAMPO STATUS TROVO 'C' (CLOSE)
038100931104     C* CHIUDO I FILE E FINISCO PGM
038200991214     C     D$GSTS        IFEQ      'C'
038300991214     C     D$GSTS        OREQ      'S'
038400991214     C     $ERR          ORNE      'N'
038500991214     C                   ELSE
038600940621     C*
038700940621     C* LANCIA ROUTINE CHE CALCOLA LE COMPETENZE
038800940621     C                   EXSR      GESCOM
038900931104     C*
039000940217     C                   END
039100930323     C*
039200940405     C* SE PGM LANCIATO CON 'C' (CLOSE) CHIUDE CON LR (CHIUDE FILE)
039300930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
039400940405     C     D$GSTS        IFEQ      'C'
039500991214     C     D$GSTS        OREQ      'S'
039600941229     C*****++++++++++++++++EXSR STAMPA
039700971223     C                   CLEAR                   TIBS69
039800971222     C                   MOVEL     'C'           I69TLA
039900971222     C                   CALL      'TIBS69R'                            98
040000971222     C                   PARM                    TIBS69
040100971222     C                   PARM                    ACODS
040200971222     C                   PARM                    INDDS
040300971222     C                   PARM                    CLPDS
040400971222     C                   PARM                    CLSDS
040500930323     C                   SETON                                        LR
040600930324     C                   ELSE
040700930324     C                   SETON                                        RT
040800930323     C                   END
040900940803     C*------------------------------------------------------------------------*
041000940906     C*?GESCOM - GESTIONE CALCOLO VOCI E COMPETENZE
041100940803     C*------------------------------------------------------------------------*
041200940608     C     GESCOM        BEGSR
041300940803     C*
041400940803     C* DETERMINA QUALI SONO LE VOCI DA CALCOLARE PER QUESTA BOLLA
041500940913     C     $ERR          IFEQ      'N'
041600940803     C                   EXSR      DETVOC
041700940803     C                   END
041800940608     C*
041900940707     C* CALCOLA VALORI TABELLA '2R' (QTA TIPO TARIFFA) DI QUESTA BOLLA
042000940913     C     $ERR          IFEQ      'N'
042100940608     C                   EXSR      CAL2R
042200940608     C                   END
042300940608     C*
042400940608     C* CONTROLLA SE TUTTE LE VOCI IN MEMORIA SONO VALIDE PER LA BOLLA (DEC/SCA)
042500940608     C* ED EVENTUALMENTE LE RICARICA
042600940913     C     $ERR          IFEQ      'N'
042700940608     C                   EXSR      CHKVOC
042800940608     C                   END
042900940608     C*
043000940608     C* VALORIZZA LE VOCI DA CALCOLARE
043100940913     C     $ERR          IFEQ      'N'
043200940608     C                   EXSR      VALVOC
043300940608     C                   END
043400940608     C*
043500940913     C     $ERR          IFEQ      'N'
043600940803     C*
043700940701     C* PORTA GLI IMPORTI DELLE VOCI NELLE COMPETENZE
043800940701     C                   DO        999           V
043900940701     C     VIMP(V)       IFNE      *ZEROS
044000940721     C                   Z-ADD     VCMP(V)       C                 9 0
044100940701     C                   ADD       VIMP(V)       CIMP(C)
044200940701     C                   ENDIF
044300940701     C                   ENDDO
044400940621     C*
044500940630     C* PORTA COD. E IMP. COMPETENZE NELLA SCHIERA OUTPUT
044600940621     C                   Z-ADD     *ZEROS        I
044700940621     C                   DO        999           C
044800940621     C     CIMP(C)       IFNE      *ZEROS
044900940621     C                   ADD       1             I
045000940630     C                   Z-ADD     C             CCD(I)
045100940630     C                   Z-ADD     CIMP(C)       CIM(I)
045200940621     C                   ENDIF
045300940621     C                   ENDDO
045400940704     C*
045500940704     C* PORTA COD. E IMP. COMPETENZE NELLA DS DI OUTPUT
045600030506     C     1             DO        75            I
045700950124     C                   Z-ADD     CCD(I)        DSWCN
045800950124     C                   MOVE      DSWCA         CAN(I)
045900950124     C                   Z-ADD     CIM(I)        DSWIN
046000950124     C                   MOVE      DSWIA         IAN(I)
046100940728     C                   ENDDO
046200950125     C                   MOVEA     CAN           D$DCCD
046300950125     C                   MOVEA     IAN           D$DICD
046400940803     C*
046500940803     C                   ENDIF
046600940729     C                   Z-ADD     0             II
046700030506     C     1             DO        75            I
046800950124     C                   Z-ADD     *ZEROS        DSWCN
046900950124     C                   MOVE      DSWCA         CAN(I)
047000950124     C                   MOVE      DSWCA         PAN(I)
047100950124     C                   Z-ADD     *ZEROS        DSWIN
047200950124     C                   MOVE      DSWIA         IAN(I)
047300940729     C                   ENDDO
047400940729     C     1             DO        999           I
047500940729     C     VC(I)         IFEQ      'S'
047600991104     C     VIMP(I)       ANDGT     *ZEROS
047700940729     C                   ADD       1             II
047800030506     c     II            IFGT      75
047900030205     C                   MOVEL     MSG(34)       $MSG
048000030205     C                   EXSR      SCRERR
048100030205     C                   ELSE
048200950124     C                   Z-ADD     I             DSWCN
048300950124     C                   MOVE      DSWCA         CAN(II)
048400950124     C                   Z-ADD     VPRG(I)       DSWCN
048500950124     C                   MOVE      DSWCA         PAN(II)
048600950124     C                   Z-ADD     VIMP(I)       DSWIN
048700950125     C                   MOVE      DSWIA         IAN(II)
048800030114     C                   MOVEL     VCOP(I)       COP(II)
048900030114     C                   MOVEL     VDAT(I)       DAT(II)
049000030114     C                   MOVEL     VAID(I)       AID(II)
049100030123     C                   Z-ADD     VCMP(I)       DSWCN
049200030123     C                   MOVE      DSWCA         CMP(II)
049300030205     C                   ENDIF
049400940729     C                   ENDIF
049500100518     c* se importo = 0 devo sottrarre dai saldi la qta e l'importo
049600121120     c* se richiesto  dal chiamante
049700091012     C     VC(I)         IFEQ      'S'
049800121120     c     D$GFSF        andeq     'S'
049900110309     c                   exsr      sresvocs
050000110309     C     VIMP(I)       ifne      *ZEROS
050100100518     c                   exsr      sresvoc10
050200100518     c                   end
050300091012     c                   end
050400940729     C                   ENDDO
050500950125     C                   MOVEA     CAN           D$DCVD
050600950125     C                   MOVEA     PAN           D$DPVD
050700030114     C                   MOVEA     IAN           D$DIVD
050800030114     C                   MOVEA     COP           D$DCOP
050900030114     C                   MOVEA     DAT           D$DDAT
051000030114     C                   MOVEA     AID           D$DAID
051100030123     C                   MOVEA     CMP           D$DCMP
051200940608     C*
051300940608     C                   ENDSR
051400940803     C*------------------------------------------------------------------------*
051500940906     C*?DETVOC - DETERMINA LE VOCI DA CALCOLARE QUINDI VALIDE PER LA BOLLA
051600940803     C*------------------------------------------------------------------------*
051700940603     C     DETVOC        BEGSR
051800040526     c                   MOVEL     D$DFLO        DTASFLO
051900060313     c* controllo se ho la varia "d" per l'assicurazione AC BASE
052000060313     c                   MOVEa     D$Dsvn        Dsvn
052100060313     c                   z-add     1             jj                2 0
052200060313     c     'd'           lookup    dsvn                                   44
052300040427     C* ----- PESO -----
052400040427     C* SE I COLLI=COLLI CML PRENDO PESO CML
052500040427IF  2C     D$DPKC        IFGT      *ZEROS
052600040427     C     D$DNCL        ANDEQ     D$DNCP
052700040427     C                   Z-ADD     D$DPKC        WDPES
052800040427X   2C                   ELSE
052900040427     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
053000040427IF  3C     D$DPKF        IFGT      D$DPKC
053100040427     C                   Z-ADD     D$DPKF        WDPES
053200040427X   3C                   ELSE
053300040427     C                   Z-ADD     D$DPKC        WDPES
053400040427E   3C                   ENDIF
053500040427E   1C                   ENDIF
053600940526     C*
053700940603     C* IMPOSTA A NO TUTTI I FLAG 'DA CALCOLARE' DELLE VOCI
053800940721     C                   DO        999           V                 9 0
053900940603     C                   MOVEL     'N'           VC(V)
054000991027     C                   MOVEL     'S'           VS(V)
054100940526     C                   ENDDO
054200000623     C*====================
054300000623     C* SE TIPO SERVIZIO NO POSTE
054400000623     C*====================
054500000623IF  0C     D$DTSP        IFNE      'P'
054600020621     C*
054700030630     C* =============> VOCI CHE VANNO APPLICATE SOLO SE LA BOLLA
054800030630     C*    NON E' UNA BOLLA DI RECUPERO (A3) OPPURE
054900030630     C*    E'     UNA BOLLA DI RECUPERO (A3) MA PER I CASI DI:
055000030630     C*    - RICERCA DOCUMENTO (FO)
055100030630     C*    - RITIRO ANNULLATO  (FY)
055200030630     C*    - ORM COMMISSIONATO (FT)
055300040520     C*    - BOLLE FIRMATE     (FF)
055400050801     C*    - POD IMAGE         (FI)
055500020621IF 1aC                   IF        D$DTBL <> 'A3' OR
055600020621     C                             (D$DTBL = 'A3' AND D$DCBO = 'FO') OR
055700030630     C                             (D$DTBL = 'A3' AND D$DCBO = 'FY') OR
055800040520     C                             (D$DTBL = 'A3' AND D$DCBO = 'FF') OR
055900050801     C                             (D$DTBL = 'A3' AND D$DCBO = 'FI') OR
056000030630     C                             (D$DTBL = 'A3' AND D$DCBO = 'FT')
056100020621     C*
056200020621     C* VOCE COMETENZA DELTA ======>
056300020621     C*    SEMPRE DA ATTIVARE SE LA BOLLA PASSA DA QUI
056400020621     C                   MOVEL     'S'           VC(900)
056500021029     C*
056600040520     C* CONTROLLO SE ESISTE =======>
056700040520     C*    RICERCA DOCUMENTO (fotocopia)
056800130930if  1c                   if        D$DTBL = 'A3' AND
056900040520     c                             D$DCBO = 'FF'
057000040520     C                   MOVEL     'S'           VC(82)
057100040520E   1C                   ENDIF
057200050801     C*    POD IMAGE
057300130930if  1c                   if        D$DTBL = 'A3' AND
057400050801     c                             D$DCBO = 'FI'
057500050801     C                   MOVEL     'S'           VC(199)
057600050801E   1C                   ENDIF
057700020621     c*
057800021029     C* CONTROLLO SE ESISTE =======>
057900021029     C*    RICERCA DOCUMENTO (fotocopia)
058000021029     C*    RESTITUZIONE NS BOLLA FIRMATA AL CLIENTE (originale)
058100130930IF  0C     WDDSP         ifle      20181231
058200020621if  1c     D$DTBL        IFEQ      'A3'
058300020621     c     D$DCBO        ANDEQ     'FO'
058400020621     C                   MOVEL     'S'           VC(75)
058500020621     C                   MOVEL     'S'           VC(185)
058600020621E   1C                   ENDIF
058700110321E   1C                   ENDIF
058800020621     c*
058900020621     C* CONTROLLO SE ESISTE =======> RITIRO ANNULLATO
059000020621if  1c     D$DTBL        IFEQ      'A3'
059100020621     c     D$DCBO        ANDEQ     'FY'
059200020621     C                   MOVEL     'S'           VC(16)
059300020621E   1C                   ENDIF
059400020621     C*
059500020621     C* CONTROLLO SE ESISTE =======> RICARICO SEDE EXPORT DPD
059600020621     C*    FRANCHI EXPORT O ASSEGNATI IMPORT
059700020621     C                   Z-ADD     W$LNP         F
059800111020     C                   Z-ADD     W$lna         F1
059900020805IF  1C     NTW(F)        IFEQ      'DPD'
060000020621     C     D$DTPO        ANDEQ     'A'
060100020805     C     NTW(F1)       OREQ      'DPD'
060200020621     C     D$DTPO        ANDEQ     'F'
060300100408     C                   MOVEL     'S'           VC(823)
060400130930     c* FINANZIAM.TARIFFE IMPORT DPD
060500040607     C                   MOVEL     'S'           VC(305)
060600020621E   1C                   ENDIF
060700030604     c*
060800030604     C* CONTROLLO SE ESISTE =======> RECAPITO C/A MITTENTE
060900030604IF  1C     D$DCBO        IFEQ      'FW'
061000030604     C                   MOVEL     'S'           VC(164)
061100030604     C                   MOVEL     'S'           VC(187)
061200030604E   1C                   ENDIF
061300030630     c*
061400030630     C* CONTROLLO SE ESISTE =======> ORM COMMISSIONATO
061500030630if  1c     D$DTBL        IFEQ      'A3'
061600030630     c     D$DCBO        ANDEQ     'FT'
061700030630     C                   MOVEL     'S'           VC(76)
061800030630E   1C                   ENDIF
061900020620     C*
062000020621     C* CONTROLLO SE ESISTE =============> RICARICO SEDE NON DPD
062100020620     C*    SE LINEA PARTENZA E LINEA ARRIVO <> DPD
062200020620     C                   Z-ADD     W$LNP         F
062300111020     C                   Z-ADD     W$lna         F1
062400020805IF  1C     NTW(F)        IFNE      'DPD'
062500020805     C     NTW(F1)       ANDNE     'DPD'
062600020620     C*
062700020620     C*    SE CLIENTE 8888 O 9999 -CLIENTE NON CODIFICATO-
062800020620     C                   MOVE      W$KSC         KSCIS             4 0
062900020620IF  2C     KSCIS         IFEQ      8888
063000020620     C     KSCIS         OREQ      9999
063100100408     C                   MOVEL     'S'           VC(822)
063200020620X   2C                   ELSE
063300020620     C*
063400020620     C*    SE NON E' CLIENTE 8888 O 9999  -CLIENTE CODIFICATO-
063500020620IF  3C     D$DTSP        IFEQ      'D'
063600020620     C     WCLV          IFEQ      'A'
063700100408     C                   MOVEL     'S'           VC(816)
063800020620     C                   ENDIF
063900020620     C     WCLV          IFEQ      'B'
064000100408     C                   MOVEL     'S'           VC(817)
064100020620     C                   ENDIF
064200020620     C     WCLV          IFEQ      'C'
064300100408     C                   MOVEL     'S'           VC(818)
064400020620     C                   ENDIF
064500020620     C     WCLV          IFEQ      'T'
064600020620     C     WCLV          OREQ      'D'
064700100408     C                   MOVEL     'S'           VC(821)
064800020620     C                   ENDIF
064900020620E   3C                   ENDIF
065000020620IF  3C     D$DTSP        IFEQ      'C'
065100020620     C     WCLV          IFEQ      'A'
065200100408     C                   MOVEL     'S'           VC(810)
065300020620     C                   ENDIF
065400020620     C     WCLV          IFEQ      'B'
065500100408     C                   MOVEL     'S'           VC(811)
065600020620     C                   ENDIF
065700020620     C     WCLV          IFEQ      'C'
065800100408     C                   MOVEL     'S'           VC(812)
065900020620     C                   ENDIF
066000020620     C     WCLV          IFEQ      'T'
066100020620     C     WCLV          OREQ      'D'
066200100408     C                   MOVEL     'S'           VC(819)
066300020620     C                   ENDIF
066400020620E   3C                   ENDIF
066500020620IF  3C     D$DTSP        IFEQ      'E'
066600100408IF  3C     D$DTSP        OREQ      'H'
066700020620     C     WCLV          IFEQ      'A'
066800100408     C                   MOVEL     'S'           VC(813)
066900020620     C                   ENDIF
067000020620     C     WCLV          IFEQ      'B'
067100100408     C                   MOVEL     'S'           VC(814)
067200020620     C                   ENDIF
067300020620     C     WCLV          IFEQ      'C'
067400100408     C                   MOVEL     'S'           VC(815)
067500020620     C                   ENDIF
067600020620     C     WCLV          IFEQ      'T'
067700020620     C     WCLV          OREQ      'D'
067800100408     C                   MOVEL     'S'           VC(820)
067900020620     C                   ENDIF
068000020620E   3C                   ENDIF
068100020620E   2C                   ENDIF
068200020620E   1C                   ENDIF
068300020621     C*
068400020621E  1ac                   ENDIF
068500030630     C*
068600030630     C* =============> VOCI CHE VANNO APPLICATE SOLO SE LA BOLLA
068700030630     C*    NON E' UNA BOLLA DI RECUPERO (A3) E
068800030630     C*    NON E' UN  RECAPITO C/A MITTENTE (FW)
068900030604IF 1aC                   IF        D$DTBL <> 'A3' AND
069000030604     C                             D$DCBO <> 'FW'
069100030630     C*
069200020620     C* =======>
069300940526     C* IMPOSTA A SI TUTTE LE VOCI SEMPRE PRESENTI
069400940603     C                   MOVEL     'S'           VC(10)
069500940603     C                   MOVEL     'S'           VC(50)
069600940603     C                   MOVEL     'S'           VC(60)
069700940727     C                   MOVEL     'S'           VC(70)
069800940727     C                   MOVEL     'S'           VC(80)
069900940727     C                   MOVEL     'S'           VC(81)
070000940603     C                   MOVEL     'S'           VC(110)
070100940603     C                   MOVEL     'S'           VC(120)
070200940603     C                   MOVEL     'S'           VC(130)
070300940727     C                   MOVEL     'S'           VC(180)
070400940727     C                   MOVEL     'S'           VC(190)
070500940727     C                   MOVEL     'S'           VC(191)
070600960131     C                   MOVEL     'S'           VC(192)
070700061207     C                   MOVEL     'S'           VC(193)
070800011203     C                   MOVEL     'S'           VC(194)
070900940728     C                   MOVEL     'S'           VC(300)
071000980716     C                   MOVEL     'S'           VC(320)
071100991116     C                   MOVEL     'S'           VC(321)
071200991116     C                   MOVEL     'S'           VC(322)
071300090923     C                   MOVEL     'S'           VC(112)
071400090923     C                   MOVEL     'S'           VC(122)
071500090923     C                   MOVEL     'S'           VC(137)
071600130930IF  0C     WDDSP         ifgt      20181231
071700110321     C                   MOVEL     'S'           VC(85)
071800110321     C                   MOVEL     'S'           VC(200)
071900110321     c                   end
072000050926     c* merce di valore
072100050926IF  1C     §FLOTBA       IFEQ      'V'
072200050926     C                   MOVEL     'S'           VC(19)
072300050926     C                   MOVEL     'S'           VC(21)
072400110321     c                   end
072500021028     c*
072600010524     C* =============> COMPETENZA SEDE -CLIENTE CODIFICATO-
072700010524     C*    SE NON E' CLIENTE 8888 O 9999
072800991116     C                   MOVE      W$KSC         KSCIS             4 0
072900010524IF  1C     KSCIS         IFNE      8888
073000010524     C     KSCIS         ANDNE     9999
073100000301IF  2C     D$DTSP        IFEQ      'E'
073200100408IF  2C     D$DTSP        OREQ      'H'
073300001201     C                   MOVEL     'S'           VC(533)
073400001206E   2C                   ENDIF
073500010524E   1C                   ENDIF
073600111003     C*
073700111003     C                   Z-ADD     W$LNP         F
073800111020     C                   Z-ADD     W$lna         F1
073900111003      * ADEGUAMENTO "MOMENTANEO" TARIFFE EEX
074000111003      * partenza euroexpress, FRANCO
074100111003     c     NTW(F1)       IFEQ      'EEX'
074200111003     C     D$DTPO        andeq     'F'
074300111003     C                   MOVEL     'S'           VC(549)
074400111003     C                   ENDIF
074500111003      * partenza euroexpress, ASSEGNATO
074600111003     c     NTW(F)        IFEQ      'EEX'
074700111003     C     D$DTPO        andeq     'A'
074800111003     C                   MOVEL     'S'           VC(550)
074900111003     C                   ENDIF
075000020503     C*
075100020503     C* CONTROLLO SE ESISTE =======> ASSISTENZA CLIENTI FEDEX
075200020503     C*    IMPORT o EXPORT FEDEX
075300020503IF  1C     NTW(F)        IFEQ      'FED'
075400020503     C     NTW(F1)       OREQ      'FED'
075500020503     C                   MOVEL     'S'           VC(284)
075600020503E   1C                   ENDIF
075700050329     C* CONTROLLO SE ESISTE =======> ETICHETTATURA EXPORT DPD
075800130930     C                   IF        NTW(F1) = 'DPD'
075900050329     C                   MOVEL     'S'           VC(45)
076000050329E   1C                   ENDIF
076100081029      * arrivo euroexpress, franco, consegna loc.disagiate
076200081027     c     NTW(F1)       IFEQ      'EEX'
076300081027     C     D$DTPO        andeq     'F'
076400081027     C     D$DFIN        ANDEQ     'D'
076500081027     C                   MOVEL     'S'           VC(340)
076600081027     C                   ENDIF
076700081029      * partenza euroexpress, assegnato, ritiro loc.disagiate
076800081027     c     NTW(F)        IFEQ      'EEX'
076900081027     C     D$DTPO        andeq     'A'
077000081027     C     D$DFAP        ANDEQ     'D'
077100081027     C                   MOVEL     'S'           VC(341)
077200081027     C                   ENDIF
077300000301     C*
077400000301     C* CONTROLLO SE ESISTE =======> ESTERO - IMPORT
077500971211     C                   Z-ADD     W$LNP         F
077600020805     C     FIE(F)        IFEQ      'E'
077700020805     C     NTW(F)        IFEQ      'DPD'
077800000202     C                   MOVEL     'S'           VC(183)
077900010726     C                   MOVEL     'S'           VC(283)
078000000202     C                   ELSE
078100000202     C                   MOVEL     'S'           VC(181)
078200020503     C     NTW(F)        IFNE      'FED'
078300010726     C                   MOVEL     'S'           VC(282)
078400000202     C                   ENDIF
078500020503     C                   ENDIF
078600010116     C                   MOVEL     'S'           VC(542)
078700000202     C                   MOVEL     'S'           VC(41)
078800950208     C     D$DTPO        IFEQ      'A'
078900020805     C     NTW(F)        IFEQ      'DPD'
079000000202     C                   MOVEL     'S'           VC(13)
079100000202     C                   MOVEL     'S'           VC(222)
079200000202     C                   ELSE
079300020503     C     NTW(F)        IFNE      'FED'
079400000202     C                   MOVEL     'S'           VC(11)
079500950207     C                   MOVEL     'S'           VC(12)
079600130529     C                   IF        WDDSP > 20130600
079700130529     C                   MOVEL     'S'           VC(14)
079800130529     c                   end
079900090122     C                   MOVEL     'S'           VC(48)
080000051111     C                   MOVEL     'S'           VC(35)
080100051111     C                   MOVEL     'S'           VC(37)
080200051111     C                   MOVEL     'S'           VC(38)
080300070521     C                   MOVEL     'S'           VC(47)
080400070521     C                   MOVEL     'S'           VC(552)
080500040526     c* 546 MAGG.AUTOSTRADA AUSTRIA PARTENZA
080600040526     C                   MOVEL     'S'           VC(546)
080700040526     c* 544 MAGGIORAZ.RITIRO ISOLE GERM E RITIRO ISOLA
080800130930     C                   if        D$DFAP = 'I'
080900040526     C                   MOVEL     'S'           VC(544)
081000040526     C                   ENDIF
081100020503     C                   ENDIF
081200000202     C                   MOVEL     'S'           VC(221)
081300000202     C                   ENDIF
081400000202     C                   ENDIF
081500000202     C                   ENDIF
081600950207     C*
081700950207     C* CONTROLLO SE ESISTE =======> ESTERO - EXPORT
081800111020     C                   Z-ADD     W$lna         F
081900020805     C     FIE(F)        IFEQ      'E'
082000040429      *
082100130930      * per euroexpress e solo franco e con giacenza
082200040429     c     NTW(F)        IFEQ      'EEX'
082300040429     C     D$DFGC        andeq     'S'
082400040429     C     D$DTPO        andeq     'F'
082500040429     C                   MOVEL     'S'           VC(590)
082600070521     C                   MOVEL     'S'           VC(268)
082700070521     C                   MOVEL     'S'           VC(553)
082800070521     C                   ENDIF
082900040429      *
083000020805     C     NTW(F)        IFEQ      'DPD'
083100000202     C                   MOVEL     'S'           VC(74)
083200000202     C                   MOVEL     'S'           VC(276)
083300000202     C                   ELSE
083400000202     C                   MOVEL     'S'           VC(71)
083500020503     C     NTW(F)        IFNE      'FED'
083600000202     C                   MOVEL     'S'           VC(275)
083700020503     C                   MOVEL     'S'           VC(555)
083800030527     C                   MOVEL     'S'           VC(556)
083900040429     C                   ENDIF
084000000202     C                   ENDIF
084100010116     C                   MOVEL     'S'           VC(543)
084200020503     C     NTW(F)        IFEQ      'FED'
084300040427     C                   MOVEL     'S'           VC(591)
084400040903     c* le devo attivare comunque tutte e 3 x reperire le loro informazioni
084500040903     c* se per la voce 167 ho manca tariffa xchè peso tassabile > 2,5 kg
084600040903     c* disattivo quest'ultima e lascio la 168 e 169, viceversa, se non ho
084700040903     c* il manca tariffa sulla voce 167 disattivo la 168 e 169
084800130930     C                   MOVEL     'S'           VC(168)
084900130930     C                   MOVEL     'S'           VC(169)
085000130930     C                   if        D$DCTR >= 350 AND D$DCTR <=399
085100130930     C                   MOVEL     'S'           VC(167)
085200030204E   2C                   ENDIF
085300020503     C                   ENDIF
085400130930     C* ASSEGNATO
085500980317     C     D$DTPO        IFEQ      'A'
085600980317     C                   MOVEL     'S'           VC(562)
085700020805     C     NTW(F)        IFEQ      'DPD'
085800000202     C                   MOVEL     'S'           VC(73)
085900000202     C                   ENDIF
086000000202     C                   ENDIF
086100130930     c* FRANCO
086200950207     C     D$DTPO        IFEQ      'F'
086300020805     C     NTW(F)        IFEQ      'DPD'
086400000202     C                   MOVEL     'S'           VC(175)
086500000202     C                   ELSE
086600020503     C     NTW(F)        IFNE      'FED'
086700090122     C                   MOVEL     'S'           VC(332)
086800050504     c                   movel     d$dcts        wdcts             1
086900130930     c* MONOCOLLO GERM FINO 30 KG
087000030429IF  6C     D$DNCL        IFEQ      1
087100040427     C     wdpes         ANDLE     30
087200140124     C     wdcts         ANDEQ     'D'
087300030429     C     D$DCAS        ANDEQ     *ZEROS
087400070523IF  6C     D$DNCL        orEQ      1
087500070523     C     wdpes         ANDLE     30
087600140124     C     d$dcts        ANDEQ     'W4'
087700070523     C     D$DCAS        ANDEQ     *ZEROS
087800030429     C                   MOVEL     'S'           VC(166)
087900130930     c* CONSEGNE ISOLE E CONSEGNA ISOLE
088000130930     C                   if        D$DFIN = 'I'
088100040526     C                   MOVEL     'S'           VC(170)
088200040526     C                   ENDIF
088300130930     c* NO MONOCOLLO GERM FINO 30 KG
088400030429X   6C                   ELSE
088500030429     C                   MOVEL     'S'           VC(171)
088600030429     C                   MOVEL     'S'           VC(172)
088700130529IF  5C                   IF        WDDSP > 20130600
088800130529     C                   MOVEL     'S'           VC(173)
088900130529     c                   end
089000051104     C                   MOVEL     'S'           VC(323)
089100051104     C                   MOVEL     'S'           VC(327)
089200051104     C                   MOVEL     'S'           VC(328)
089300070521     C                   MOVEL     'S'           VC(331)
089400070521     C                   MOVEL     'S'           VC(552)
089500130930     c* MAGG.AUTOSTRADA AUSTRIA ARRIVO
089600040526     C                   MOVEL     'S'           VC(547)
089700130930     c* MAGGIORAZ.CONSEGNE ISOLE E CONSEGNA ISOLE
089800130930     C                   if        D$DFIN = 'I'
089900040526     C                   MOVEL     'S'           VC(545)
090000040526     C                   ENDIF
090100130930     c*
090200030429E   6C                   ENDIF
090300030429E   4C                   ENDIF
090400030429E   3C                   ENDIF
090500030429E   2C                   ENDIF
090600130930     C*
090700030429E   1C                   ENDIF
090800940526     C*
090900940526     C* CONTROLLO SE ESISTE =======> AFFLUENZA
091000940526     C*    LINEA PARTENZA DIVERSA DAL TERMINAL PARTENZA
091100940829     C     W$LNP         IFNE      W$TFP
091200971211     C                   Z-ADD     W$LNP         F
091300020805     C     FIE(F)        IFEQ      'E'
091400950207     C                   MOVEL     'S'           VC(32)
091500950207     C                   MOVEL     'S'           VC(52)
091600950207     C                   MOVEL     'S'           VC(64)
091700950207     C                   ELSE
091800980119     C                   MOVEL     'S'           VC(220)
091900940727     C                   MOVEL     'S'           VC(31)
092000940727     C                   MOVEL     'S'           VC(51)
092100940727     C                   MOVEL     'S'           VC(61)
092200130930IF  0C     WDDSP         ifgt      20181231
092300110321     C                   MOVEL     'S'           VC(86)
092400110321     c                   end
092500050926     c* merce di valore
092600050926IF  1C     §FLOTBA       IFEQ      'V'
092700050926     C                   MOVEL     'S'           VC(20)
092800050926     c                   end
092900050926     c*
093000950207     C                   ENDIF
093100940729     C                   ENDIF
093200940526     C*
093300940526     C* CONTROLLO SE ESISTE =======> DEFLUENZA
093400940526     C*    LINEA ARRIVO DIVERSA DAL TERMINAL ARRIVO
093500111020IF  1C     W$lna         IFNE      D$DTFA
093600111020     C                   Z-ADD     W$lna         F
093700020805IF  2C     FIE(F)        IFEQ      'E'
093800980401     C                   MOVEL     'S'           VC(114)
093900980401     C                   MOVEL     'S'           VC(124)
094000980401     C                   MOVEL     'S'           VC(152)
094100020805IF  3C     NTW(F)        IFEQ      'DPD'
094200000204IF  4C     D$DTPO        IFEQ      'F'
094300000202     C                   MOVEL     'S'           VC(233)
094400000204E   4C                   ENDIF
094500000204X   3C                   ELSE
094600001201     C                   MOVEL     'S'           VC(231)
094700001201E   3C                   ENDIF
094800980402X   2C                   ELSE
094900950703///  C                   MOVEL     'S'           VC(230)
095000940727     C                   MOVEL     'S'           VC(111)
095100940727     C                   MOVEL     'S'           VC(121)
095200940727     C                   MOVEL     'S'           VC(151)
095300110905     C                   MOVEL     'S'           VC(548)
095400090923     C                   MOVEL     'S'           VC(113)
095500090923     C                   MOVEL     'S'           VC(125)
095600090923     C                   MOVEL     'S'           VC(154)
095700130930IF  0C     WDDSP         ifgt      20181231
095800110321     C                   MOVEL     'S'           VC(201)
095900110321     c                   end
096000050926     c* merce di valore
096100050926IF  1C     §FLOTBA       IFEQ      'V'
096200050926     C                   MOVEL     'S'           VC(22)
096300050926     c                   end
096400980402E   2C                   ENDIF
096500980402E   1C                   ENDIF
096600940526     C*
096700940727     C* CONTROLLO SE ESISTE =======> TRANSITO 1
096800940727     C*    TRANSITO 1 DIVERSO DA ZERO
096900040520IF  1C*    D$DFLP        IFNE      *ZEROS
097000040520IF  1C     D$Dar5flp     IFNE      *ZEROS
097100001201     C                   MOVEL     'S'           VC(67)
097200001201     C                   MOVEL     'S'           VC(117)
097300001201     C                   MOVEL     'S'           VC(127)
097400001201     C                   MOVEL     'N'           VS(67)
097500001201     C                   MOVEL     'N'           VS(117)
097600001201     C                   MOVEL     'N'           VS(127)
097700050926     c* merce di valore
097800050926IF  1C     §FLOTBA       IFEQ      'V'
097900050926     C                   MOVEL     'S'           VC(24)
098000050926     c                   end
098100001201E   1C                   ENDIF
098200940727     C*
098300940727     C* CONTROLLO SE ESISTE =======> TRANSITO 2
098400940727     C*    TRANSITO 2 DIVERSO DA ZERO
098500040520     C*    D$DFL2        IFNE      *ZEROS
098600040520     C     D$Dar5fl2     IFNE      *ZEROS
098700001201     C                   MOVEL     'S'           VC(68)
098800001201     C                   MOVEL     'S'           VC(118)
098900001201     C                   MOVEL     'S'           VC(128)
099000001201     C                   MOVEL     'N'           VS(68)
099100001201     C                   MOVEL     'N'           VS(118)
099200001201     C                   MOVEL     'N'           VS(128)
099300050926     c* merce di valore
099400050926IF  1C     §FLOTBA       IFEQ      'V'
099500050926     C                   MOVEL     'S'           VC(25)
099600050926     c                   end
099700940727     C                   ENDIF
099800040520     C*
099900040520     C* CONTROLLO SE ESISTE =======> TRANSITO 3
100000040520     C*    TRANSITO 3 DIVERSO DA ZERO
100100040520     C     D$Dar5fl3     IFNE      *ZEROS
100200040520     C                   MOVEL     'S'           VC(77)
100300040520     C                   MOVEL     'S'           VC(131)
100400040520     C                   MOVEL     'S'           VC(134)
100500040520     C                   MOVEL     'N'           VS(77)
100600040520     C                   MOVEL     'N'           VS(131)
100700040520     C                   MOVEL     'N'           VS(134)
100800050926     c* merce di valore
100900050926IF  1C     §FLOTBA       IFEQ      'V'
101000130930IF  0C     WDDSP         ANDle     20181231
101100050926     C                   MOVEL     'S'           VC(26)
101200050926     c                   end
101300040520     C                   ENDIF
101400040520     C*
101500040520     C* CONTROLLO SE ESISTE =======> TRANSITO 4
101600040520     C*    TRANSITO 4 DIVERSO DA ZERO
101700040520     C     D$Dar5fl4     IFNE      *ZEROS
101800040520     C                   MOVEL     'S'           VC(78)
101900040520     C                   MOVEL     'S'           VC(132)
102000040520     C                   MOVEL     'S'           VC(135)
102100040520     C                   MOVEL     'N'           VS(78)
102200040520     C                   MOVEL     'N'           VS(132)
102300040520     C                   MOVEL     'N'           VS(135)
102400050926     c* merce di valore
102500050926IF  1C     §FLOTBA       IFEQ      'V'
102600130930IF  0C     WDDSP         ANDle     20181231
102700050926     C                   MOVEL     'S'           VC(27)
102800050926     c                   end
102900040520     C                   ENDIF
103000040520     C*
103100040520     C* CONTROLLO SE ESISTE =======> TRANSITO 5
103200040520     C*    TRANSITO 5 DIVERSO DA ZERO
103300040520     C     D$Dar5fl5     IFNE      *ZEROS
103400040520     C                   MOVEL     'S'           VC(79)
103500040520     C                   MOVEL     'S'           VC(133)
103600040520     C                   MOVEL     'S'           VC(136)
103700040520     C                   MOVEL     'N'           VS(79)
103800040520     C                   MOVEL     'N'           VS(133)
103900040520     C                   MOVEL     'N'           VS(136)
104000050926     c* merce di valore
104100050926IF  1C     §FLOTBA       IFEQ      'V'
104200130930IF  0C     WDDSP         ANDle     20181231
104300050926     C                   MOVEL     'S'           VC(28)
104400050926     c                   end
104500040520     C                   ENDIF
104600940722     C*
104700940722     C* CONTROLLO SE ESISTE =======> LOCALI
104800980317     C*    LINEA PART. = LINEA ARR. = TERM. PART. = TERM. ARR.
104900111020     C     W$LNP         IFEQ      W$lna
105000980317     C     W$LNP         ANDEQ     D$DTFA
105100980318     C     W$LNP         ANDEQ     W$TFP
105200940728     C                   MOVEL     'S'           VC(500)
105300940722     C                   ENDIF
105400940728     C*
105500950626     C* CONTROLLO SE ESISTE =======> DISK 'A' E DISK 'B'
105600950626     C*    SE SIGLA OPERATORE = '§' O '%' E' UN DISK 'A' O DISK 'B'
105700950626     C     D$DSOP        IFEQ      '§'
105800950626     C     D$DSOP        OREQ      '%'
105900000210     C     D$DSOP        OREQ      ':'
106000950626     C*    SE N.SERIE > ZERO E' UN DISK 'B' ALTRIMENTI E' UN DISK 'A'
106100950626     C     D$DNRS        IFGT      *ZEROS
106200950626     C                   MOVEL     'S'           VC(510)
106300950626     C                   MOVEL     'S'           VC(511)
106400990223     C                   MOVEL     'S'           VC(55)
106500950626     C                   ELSE
106600950626     C                   MOVEL     'S'           VC(511)
106700940728     C                   ENDIF
106800121128     c* sbinata la 511: ora la 511 sarà per la lnp e la 512 per lna
106900130930IF  2C     WDDSP         ifgt      20181231
107000121128     C                   MOVEL     'S'           VC(512)
107100950626     C                   ENDIF
107200121128     C                   ENDIF
107300021029     C*
107400021029     C* REPERISCE I DATI DEL CODICE TRATTAMENTO MERCE =======>
107500021029     C                   CLEAR                   WCF12
107600050801     C                   CLEAR                   WCF16
107700030530     C                   CLEAR                   WCDKC
107800021029IF  1C     D$DCTM        IFNE      *BLANKS
107900021029     C                   Z-ADD     1             I
108000021029     C     D$DCTM        LOOKUP    CTM(I)                                 98
108100021029IF  2C     *IN98         IFEQ      *OFF
108200021029     C                   MOVEL     MSG(27)       $MSG
108300021029     C                   EXSR      SCRERR
108400021029X   2C                   ELSE
108500021029     C                   MOVEL     CF12(I)       WCF12
108600050801     C                   MOVEL     CF16(I)       WCF16
108700030530     C                   MOVEL     CDKC(I)       WCDKC
108800021029E   2C                   ENDIF
108900021029E   1C                   ENDIF
109000010420     C*
109100021029     C* CONTROLLA SE ESISTE =======> DISCK 'C'
109200030530IF  1C     WCDKC         IFNE      *BLANKS
109300010420     C                   MOVEL     'S'           VC(56)
109400030530E   1C                   ENDIF
109500050801     C*
109600050801     C* CONTROLLA SE ESISTE =======> packing list
109700050801IF  1C     D$DCTM        IFNE      *BLANKS
109800050801IF  2C     WCF16         andeq     'S'
109900050801     C                   MOVEL     'S'           VC(189)
110000050801E   1C                   ENDIF
110100130930     c*
110200130930     C* =============> ETICHETTATURA , SCARICO , PICKING e CARICO
110300021029     C*    DELL' AUTOTRASPORTATORE (flag =" ") OPPURE
110400021029     C*    DELLA COOPERATIVA       (flag ="C")
110500021029IF  1C     §FLOSCA       IFEQ      *BLANKS
110600021029     C                   MOVEL     'S'           VC(30)
110700021212     C                   MOVEL     'S'           VC(57)
110800021029X   1C                   ELSE
110900021029     C                   MOVEL     'S'           VC(33)
111000021029E   1C                   ENDIF
111100040520     C* BOLLA CON IMMAGINE APPLICO LA VOCE 188
111200040520IF  1C     §FLOIML       IFNE      *BLANKS
111300040520     C                   MOVEL     'S'           VC(188)
111400040520E   1C                   ENDIF
111500021029     C* NO disck C
111600021029IF  1C     WCF12         IFEQ      'N'
111700021029IF  2C     §FLOETI       IFEQ      *BLANKS
111800021029     C                   MOVEL     'S'           VC(40)
111900021029X   2C                   ELSE
112000021029     C                   MOVEL     'S'           VC(42)
112100021029E   2C                   ENDIF
112200021029E   1C                   ENDIF
112300030918     C* disck C NON gestito dal cliente
112400030530IF  1C     WCDKC         IFEQ      'B'
112500030919IF  3C     D$DFDN        IFEQ      'S'
112600030918     C                   MOVEL     'S'           VC(44)
112700030919X   3c                   ELSE
112800021029     C                   MOVEL     'S'           VC(43)
112900030919E   2C                   ENDIF
113000030530E   1C                   ENDIF
113100021029     c*
113200021029IF  1C     §FLOPIC       IFEQ      *BLANKS
113300021029     C                   MOVEL     'S'           VC(140)
113400021029X   1C                   ELSE
113500021029     C                   MOVEL     'S'           VC(141)
113600021029E   1C                   ENDIF
113700021029IF  1C     §FLOCAR       IFEQ      *BLANKS
113800021029     C                   MOVEL     'S'           VC(150)
113900021029X   1C                   ELSE
114000021029     C                   MOVEL     'S'           VC(153)
114100021029E   1C                   ENDIF
114200940526     C*
114300940526     C* CONTROLLO SE ESISTE =======> GIACENZA
114400940526     C*    FLAG GIACENZA UGUALE A SI
114500940526     C     D$DFGC        IFEQ      'S'
114600940728     C                   MOVEL     'S'           VC(260)
114700940728     C                   MOVEL     'S'           VC(261)
114800940728     C                   MOVEL     'S'           VC(262)
114900040427     C     wdpes         IFLE      100
115000000201     C                   MOVEL     'S'           VC(267)
115100000201     C                   ELSE
115200000201     C                   MOVEL     'S'           VC(265)
115300110321     C                   ENDIF
115400940728     C*    SE NON E' DIROTTATA E RESO MITT.
115500940728     C     D$DCCA        IFNE      '1'
115600940728     C     D$DCCA        ANDNE     '2'
115700960329     C     D$DCCA        ANDNE     '6'
115800940728     C                   MOVEL     'S'           VC(263)
115900940526     C                   ENDIF
116000940728     C                   ENDIF
116100940526     C*
116200940728     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
116300130930     C*    FLAG FERMO DEPOSITO = "S"
116400130930     c*    OPPURE N.STOP CONSEGNA = 99999 (consegna a magazzino)
116500990420     C*    OPPURE INOLTRO = I (CONSEGNA ISOLA)
116600940728     C     D$DFFD        IFEQ      'S'
116700000411     C     D$DSTP        OREQ      99999
116800990420     C     D$DFIN        OREQ      'I'
116900010116     C                   MOVEL     'S'           VC(532)
117000001206     C                   ENDIF
117100011203     C*
117200011203     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
117300011203IF  1C     D$DFFD        IFEQ      'S'
117400011203     C                   MOVEL     'S'           VC(184)
117500011203E   2C                   ENDIF
117600940714     C*
117700940714     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE PER CONTO
117800941201     C*    SE HA IMPORTO VALORE ASSICURAZIONE X CONTO > ZERO
117900970225     C                   MOVE      W$KSC         KSCIS             4 0
1180009702251    C     KSCIS         IFEQ      8888
118100970225     C     KSCIS         OREQ      9999
1182000705212    C     D$DVAE        ifgt      *ZEROS
118300970225     C                   MOVEL     'S'           VC(291)
118400070521     c                   end
1185009702251    C                   ELSE
1186000112272    C     D$DVAE        IFGT      *ZEROS
118700940728     C                   MOVEL     'S'           VC(290)
118800051017     C                   MOVEL     'S'           VC(292)
1189009702252    C                   ELSE
119000130930     C* FRANCHI EXPORT O ASSEGNATI IMPORT,
119100100408     C                   Z-ADD     W$LNP         F
119200111020     C                   Z-ADD     W$lna         F1
119300130930IF  1C                   if        ((fie(f) = 'E' and D$DTPO ='A') or
119400100408IF  1C                             (fie(f1) = 'E' and D$DTPO ='F'))
119500100408     C                   MOVEL     'S'           VC(286)
119600130930     C* SOLO x Italia
119700100408     c                   else
119800960606     C                   MOVEL     'S'           VC(281)
119900100408     c                   end
120000130930     c* AC BASE  (varia "d" )
120100130930     c                   if        *in44
120200060313     c* cliente top o direzionale
120300060313     c                   select
120400060313     c                   when      wclv = 'T' or wclv = 'D'
120500060313     C                   MOVEL     'S'           VC(293)
120600060313     c* cliente A
120700060313     c                   when      wclv = 'A'
120800060313     C                   MOVEL     'S'           VC(294)
120900060313     c* cliente B o C
121000060313     c                   when      wclv = 'B' or wclv = 'C'
121100060313     C                   MOVEL     'S'           VC(295)
121200060313     C                   endsl
121300060313     C                   endif
1214009702252    C                   ENDIF
1215009702251    C                   ENDIF
121600940722     C*
121700940722     C* CONTROLLO SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
121800970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "A"
121900130930IF  0C     WDDSP         ifle      20181231
122000940722     C     D$DFTC        IFEQ      'A'
122100970228     C     D$DTC2        OREQ      'A'
122200011203IF  3C     D$DFTC        IFEQ      'S'
122300011203     C     D$DTC2        OREQ      'S'
122400011203X   3C                   ELSE
122500011203     C                   MOVEL     'S'           VC(240)
122600011203E   3C                   ENDIF
122700011203     C                   ENDIF
122800110321     c                   else
122900110321     C     D$DFTC        IFEQ      'A'
123000110321     C     D$DTC2        OREQ      'A'
123100110321IF  3C     D$DFTC        OREQ      'S'
123200110321     C     D$DTC2        OREQ      'S'
123300110321     C                   MOVEL     'S'           VC(240)
123400110321     c                   end
123500110321     c                   end
123600940722     C*
123700940722     C* CONTROLLO SE ESISTE =======> CONSEGNA AI SUPERMERCATI
123800970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "S"
123900940722     C     D$DFTC        IFEQ      'S'
124000970228     C     D$DTC2        OREQ      'S'
124100980119     C                   MOVEL     'S'           VC(242)
124200980119     C                   MOVEL     'S'           VC(247)
124300110321     C                   ENDIF
124400940722     C*
124500940722     C* CONTROLLO SE ESISTE =======> CONSEGNA AI PIANI
124600970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "P"
124700940722     C     D$DFTC        IFEQ      'P'
124800970228     C     D$DTC2        OREQ      'P'
124900940727     C                   MOVEL     'S'           VC(241)
125000940722     C                   ENDIF
125100020620     C*
125200020620     C* CONTROLLO SE ESISTE =======> CONSEGNA DISAGIATA
125300020620     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "X"
125400020620     C     D$DFTC        IFEQ      'X'
125500020620     C     D$DTC2        OREQ      'X'
125600020620     C                   MOVEL     'S'           VC(534)
125700020620     C                   MOVEL     'S'           VC(535)
125800020620     C                   ENDIF
125900940722     C*
126000940722     C* CONTROLLO SE ESISTE =======> CONTRASSEGNO
126100940722     C*    IMPORTO CONTRASSEGNO > ZERO
126200941201     C     D$DCAS        IFGT      *ZEROS
126300111020     C                   Z-ADD     W$lna         F
126400060223     c* arrivi
126500020805     C     FIE(F)        IFEQ      'E'
126600000203     C                   MOVEL     'S'           VC(256)
126700000203     C                   MOVEL     'S'           VC(257)
126800000203     C                   MOVEL     'S'           VC(280)
126900000203     C                   ELSE
127000940727     C                   MOVEL     'S'           VC(251)
127100940727     C                   MOVEL     'S'           VC(252)
127200110321     c* abilitazione c/assegno
127300130930IF  0C     WDDSP         ifgt      20181231
127400110321IF  1C     §FLOfca       andeq     'S'
127500110321     C                   MOVEL     'S'           VC(259)
127600000203     C                   ENDIF
127700110321     C                   ENDIF
127800060223     c**** mittente
127900941201     C     D$DTPI        IFEQ      'M'
128000060223     C                   MOVEL     'S'           VC(254)
128100941021     C                   ELSE
128200060223     c**** non mittente
128300941021     C                   MOVEL     'S'           VC(253)
128400941021     C                   ENDIF
128500130930     c*
128600090417     C                   IF        (d$dsop <> '§' and
128700090417     C                             d$dsop <> '%' and
128800130930     C                             d$dsop <> ':' )
128900090417     C                   MOVEL     'S'           VC(250)
129000090417     c                   end
129100940722     C                   ENDIF
129200940728     C*
129300110321     C* CONTROLLO SE ESISTE =======> PORTO ASSEGNATO UFF.ARR.
129400110321     C     D$DTPO        IFEQ      'A'
129500130930IF  2C     wddsp         andgt     20181231
129600110321     C                   MOVEL     'S'           VC(312)
129700110321     C                   ENDIF
129800110321     C* CONTROLLO SE ESISTE =======> PORTO ASSEGNATO UFF.ARR. E PADRONCINO
129900110321     C*    ASSEGNATO CON FATTURA IMMEDIATA
130000941201     C     D$DTPO        IFEQ      'A'
130100030529     C     D$DFIV        ANDGT     0
130200030529     C     D$DFIV        ANDLT     900
130300940728     C                   MOVEL     'S'           VC(310)
130400940728     C                   MOVEL     'S'           VC(311)
130500110321     C                   ENDIF
130600941028     C*
130700990420     C* CONTROLLO SE ESISTE =======> CONSEGNA A MAGAZZINO (RIDUZIONE)
130800941028     C*    FLAG MERCE PORTATA A MAGAZZINO DAL CLIENTE = 'SI'
130900990420     C*    OPPURE ANTEPORTO = I (RITIRO ISOLA)
131000941209     C     D$DFDN        IFEQ      'S'
131100990420     C     D$DFAP        OREQ      'I'
131200010116     C                   MOVEL     'S'           VC(521)
131300001206     C                   ENDIF
131400960214     C*
131500960214     C* CONTROLLO SE ESISTE =======> CONSEGNA A PRIVATI (DAL FLAG BOLLA)
131600960214     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'P'
131700960214     C                   MOVEL     D$DGVA        WTM01             1
131800030422     C*
131900030422     C* CONTROLLO SE ESISTE =======> GESTIONE BANCALI
132000030422     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'B'
132100030422     C                   MOVEL     D$DGVA        WTM01
132200030422     C     WTM01         IFEQ      'B'
132300030422     C                   MOVEL     'S'           VC(17)
132400030422     C                   MOVEL     'S'           VC(119)
132500030422     C                   MOVEL     'S'           VC(142)
132600030422     C                   MOVEL     'S'           VC(186)
132700030430     C                   MOVEL     'S'           VC(213)
132800030422     C                   ENDIF
132900030725     C*
133000030801     C* CONTROLLO SE ESISTE =======> Spedizioni oltre 20 colli bancalatura Coop
133100030801     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'O'
133200030801     C                   MOVEL     D$DGVA        WTM01             1
133300030801     C     WTM01         IFEQ      'O'
133400030801     C                   MOVEL     'S'           VC(34)
133500030801     C                   ENDIF
133600960214     C*
133700960214     C* CONTROLLO SE ESISTE =======> SECONDA LINEA DISTRIBUZIONE
133800030430     C*    SECONDO CARATTERE FLAG PARTICOLARITA' VARIE = 'D' e
133900010116     C*                LINEA ARRIVO = TERMINAL ARRIVO  e
134000010116     C*                C.TASSAZIONE DESTINO BOLLA <> C.TASSAZIONE LINEA ARRIVO
134100001204     C                   MOVE      D$DGVA        WTM01             1
134200001204IF  1C     WTM01         IFEQ      'D'
134300111020IF  3C     W$lna         IFEQ      D$DTFA
134400111020     C                   Z-ADD     W$lna         F
134500010116IF  4C     D$DCTS        IFNE      CTS(F)
134600001201     C                   MOVEL     'S'           VC(165)
134700001201     C                   MOVEL     'S'           VC(178)
134800110321E   4C                   ENDIF
134900010116E   3C                   ENDIF
135000001204E   1C                   ENDIF
135100970429     C*
135200970429     C* CONTROLLO SE ESISTE =======> DDT (DOCUM. DI TRASP. DA CONS. AL DEST.)
135300970429     C*    FLAG LIBERO 1  = 'S' O 'C'
135400970429     C     D$DLL1        IFEQ      'S'
135500970429     C     D$DLL1        OREQ      'C'
135600970429     C                   MOVEL     'S'           VC(72)
135700970429     C                   MOVEL     'S'           VC(182)
135800970429     C                   ENDIF
135900990222     C*
136000990222     C* CONTROLLO SE ESISTE =======> CONSEGNA LOCALITA DISAGIATE
136100990222     C*    SE FLAG INOLTRO 'D' E NON C'E' FERMO DEPOSITO
136200030430     C     D$DFFD        IFNE      'S'
136300030430     C     D$DFIN        ANDEQ     'D'
136400030430     C                   MOVEL     'S'           VC(246)
136500030430     C                   ENDIF
136600980202     C*
136700030430     C* CONTROLLO SE ESISTE =======>
136800030430     C                   MOVEL     'S'           VC(212)
136900980204     C                   MOVEL     'S'           VC(270)
137000010116     C                   MOVEL     'S'           VC(523)
137100010116     C                   MOVEL     'S'           VC(594)
137200021031     C                   MOVEL     'S'           VC(595)
137300000721     C                   MOVEL     'S'           VC(592)
137400000721     C                   MOVEL     'S'           VC(274)
137500001201     C                   MOVEL     'S'           VC(23)
137600001201     C                   MOVEL     'S'           VC(161)
137700001201     C                   MOVEL     'S'           VC(177)
137800980317     C*
137900980318     C* CONTROLLO SE ESISTE =======> AFFLUENZA/DEFLUENZA SU STESSO TERMINAL
138000980317     C*    SE TERM.PART. = TERM.ARR. E LINEA ARR. <> LINEA PART.
138100980318     C     W$TFP         IFEQ      D$DTFA
138200980318     C     W$LNP         ANDNE     W$TFP
138300111020     C     W$lna         ANDNE     D$DTFA
138400980319     C                   MOVEL     'S'           VC(560)
138500980317     C                   ENDIF
138600980317     C*
138700980319     C* CONTROLLO SE ESISTE =======> LINEA PART. = TERM. EXPORT
138800980317     C*    SE TERM.PART. = TERM.ARR. = LINEA PART. E EXPORT
138900111020     C                   Z-ADD     W$lna         F
139000980318     C     W$TFP         IFEQ      D$DTFA
139100980318     C     W$LNP         ANDEQ     W$TFP
139200020805     C     FIE(F)        ANDEQ     'E'
139300980319     C                   MOVEL     'S'           VC(561)
139400980317     C                   ENDIF
139500980717     C*
139600980717     C* CONTROLLO SE ESISTE =======> LINEA PART. E' 2°LIV. TERM. EXPORT
139700980717     C*    SE TERM.PART.=TERM.ARR. E LINEA PART.<>TERM.PART. E EXPORT
139800111020     C                   Z-ADD     W$lna         F
139900980717     C     W$TFP         IFEQ      D$DTFA
140000980717     C     W$LNP         ANDNE     W$TFP
140100020805     C     FIE(F)        ANDEQ     'E'
140200980717     C                   MOVEL     'S'           VC(570)
140300980717     C                   ENDIF
140400980717     C*
140500980717     C* CONTROLLO SE ESISTE =======> LINEA ARR. E' 2°LIV. TERM. IMPORT
140600980717     C*    SE TERM.PART.=TERM.ARR. E LINEA ARR.<>TERM.ARR. E IMPORT
140700980717     C                   Z-ADD     W$LNP         F
140800980717     C     W$TFP         IFEQ      D$DTFA
140900111020     C     W$lna         ANDNE     D$DTFA
141000020805     C     FIE(F)        ANDEQ     'E'
141100980717     C                   MOVEL     'S'           VC(571)
141200980717     C                   ENDIF
141300990217     C*
141400990420     C* CONTROLLO SE ESISTE =======> CONSEGNA ISOLA
141500990219     C*    SE FLAG INOLTRO 'I' E NON C'E' FERMO DEPOSITO
141600111020     C                   Z-ADD     W$lna         F
141700000302IF  1C     D$DFFD        IFNE      'S'
141800990219     C     D$DFIN        ANDEQ     'I'
141900020805IF  2C     FIE(F)        IFEQ      'E'
142000020805IF  3C     NTW(F)        IFEQ      'DPD'
142100000302     C                   MOVEL     'S'           VC(288)
142200000302E   3C                   ENDIF
142300000302X   2C                   ELSE
142400000302     C                   MOVEL     'S'           VC(278)
142500000302E   2C                   ENDIF
142600000302E   1C                   ENDIF
142700990217     C*
142800990420     C* CONTROLLO SE ESISTE =======> RITIRO ISOLA
142900990217     C*    SE FLAG ANTEPORTO 'I' E NON C'E' CONSEGNA A MAGAZZINO
143000000302     C                   Z-ADD     W$LNP         F
143100000302IF  1C     D$DFDN        IFNE      'S'
143200990219     C     D$DFAP        ANDEQ     'I'
143300020805IF  2C     FIE(F)        IFEQ      'E'
143400020805IF  3C     NTW(F)        IFEQ      'DPD'
143500000302     C                   MOVEL     'S'           VC(289)
143600000302E   3C                   ENDIF
143700000302X   2C                   ELSE
143800990420     C                   MOVEL     'S'           VC(279)
143900130930     c* COMP.RITIRI ISOLE VENEZIA
144000040617     C                   MOVEL     'S'           VC(285)
144100000302E   2C                   ENDIF
144200000302E   1C                   ENDIF
144300990217     C*
144400990217     C* CONTROLLO SE ESISTE =======> PREPAGATI
144500990217     C*    SE CLIENTE 8888
144600990217     C                   MOVE      W$KSC         KSCIS             4 0
1447009902171    C     KSCIS         IFEQ      8888
144800990217     C                   MOVEL     'S'           VC(266)
1449009902171    C                   ENDIF
145000020621     C*
145100020621E  1aC                   ENDIF
145200000623E   0C                   ENDIF
145300000630     C*
145400940526     C                   ENDSR
145500940908     C*------------------------------------------------------------------------*
145600940908     C*?CAL2R  - CALCOLA LE QTA DELLA TABELLA 2R (TIPO TARIFFA)
145700940908     C*------------------------------------------------------------------------*
145800940908     C     CAL2R         BEGSR
145900940908     C* TIPO -01- COLLI -------------------------------------------------------*
146000940908     C                   Z-ADD     1             I
146100940908     C     '01'          LOOKUP    C2R(I)                                 98
146200940908     C     *IN98         IFEQ      '0'
146300940908     C                   MOVEL     MSG(5)        $MSG
146400940908     C                   EXSR      SCRERR
146500940908     C                   ELSE
146600940908     C                   Z-ADD     D$DNCL        Q2R(I)
146700940908     C                   ENDIF
146800940908     C*
146900940908     C* TIPO -02- SPEDIZIONI --------------------------------------------------*
147000940908     C                   Z-ADD     1             I
147100940908     C     '02'          LOOKUP    C2R(I)                                 98
147200940908     C     *IN98         IFEQ      '0'
147300940908     C                   MOVEL     MSG(6)        $MSG
147400940908     C                   EXSR      SCRERR
147500940908     C                   ELSE
147600940908     C                   Z-ADD     1             Q2R(I)
147700940908     C                   ENDIF
147800940908     C*
147900940908     C* TIPO -03- CHILOGRAMMI -------------------------------------------------*
148000040428     C* SE I COLLI=COLLI CML PRENDO PESO CML
148100040428IF  2C     D$DPKC        IFGT      *ZEROS
148200040428     C     D$DNCL        ANDEQ     D$DNCP
148300040428     C                   Z-ADD     D$DPKC        WPES
148400040428X   2C                   ELSE
148500040428     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
148600040428IF  3C     D$DPKF        IFGT      D$DPKC
148700040428     C                   Z-ADD     D$DPKF        WPES
148800040428X   3C                   ELSE
148900040428     C                   Z-ADD     D$DPKC        WPES
149000040428E   3C                   ENDIF
149100040428E   2C                   ENDIF
149200130930     c*
149300940908     C                   Z-ADD     1             I
149400940908     C     '03'          LOOKUP    C2R(I)                                 98
149500940908     C     *IN98         IFEQ      '0'
149600940908     C                   MOVEL     MSG(7)        $MSG
149700940908     C                   EXSR      SCRERR
149800940908     C                   ELSE
149900991008     C                   Z-ADD     WPES          Q2R(I)
150000940908     C                   ENDIF
150100940908     C*
150200940908     C* TIPO -04- VOLUME ------------------------------------------------------*
150300990422     C* SE VOLUME FATTURATO E' 'T' (TOTALE COLLI) PRENDO QUELLO
150400990421IF  1C     D$DFVF        IFEQ      'T'
150500990421     C                   Z-ADD     D$DVLF        WVOL
150600990421X   1C                   ELSE
150700990422     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
150800990421IF  2C     D$DVLC        IFGT      *ZEROS
150900990421     C     D$DNCL        ANDEQ     D$DNCR
151000990422     C                   Z-ADD     D$DVLC        WVOL
151100990421X   2C                   ELSE
151200990422     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
151300990422IF  3C     D$DVLF        IFGT      D$DVLC
151400990422     C                   Z-ADD     D$DVLF        WVOL
151500990421X   3C                   ELSE
151600990422     C                   Z-ADD     D$DVLC        WVOL
151700990421E   3C                   ENDIF
151800990421E   2C                   ENDIF
151900990421E   1C                   ENDIF
152000990421     C*
152100940908     C                   Z-ADD     1             I
152200940908     C     '04'          LOOKUP    C2R(I)                                 98
152300940908     C     *IN98         IFEQ      '0'
152400940908     C                   MOVEL     MSG(8)        $MSG
152500940908     C                   EXSR      SCRERR
152600940908     C                   ELSE
152700940908     C                   Z-ADD     WVOL          Q2R(I)
152800940908     C                   ENDIF
152900030409     C*
153000030409     C* TIPO -06- BANCALI -----------------------------------------------------*
153100030422     C                   CLEAR                   WBAN              5 0
153200030422     C                   EVAL      WBAN = D$DAR5NBA + D$DAR5NB2
153300030422     C                   Z-ADD     1             I
153400030422     C     '06'          LOOKUP    C2R(I)                                 98
153500030422     C     *IN98         IFEQ      '0'
153600030422     C                   MOVEL     MSG(35)       $MSG
153700030422     C                   EXSR      SCRERR
153800030422     C                   ELSE
153900030422     C                   Z-ADD     WBAN          Q2R(I)
154000030422     C                   ENDIF
154100940908     C*
154200951213     C* TIPO -09- STOP RITIRO -------------------------------------------------*
154300940908     C                   Z-ADD     1             I
154400940908     C     '09'          LOOKUP    C2R(I)                                 98
154500940908IF  1C     *IN98         IFEQ      '0'
154600940908     C                   MOVEL     MSG(9)        $MSG
154700940908     C                   EXSR      SCRERR
154800940908X   1C                   ELSE
154900941209     C                   Z-ADD     D$DNSR        Q2R(I)
155000940908E   1C                   ENDIF
155100951213     C*
155200951213     C* TIPO -10- STOP CONSEGNA -----------------------------------------------*
155300951213     C                   Z-ADD     1             I
155400951213     C     '10'          LOOKUP    C2R(I)                                 98
155500951213IF  1C     *IN98         IFEQ      '0'
155600951213     C                   MOVEL     MSG(16)       $MSG
155700951213     C                   EXSR      SCRERR
155800951213X   1C                   ELSE
155900951213     C                   Z-ADD     D$DNSC        Q2R(I)
156000951213E   1C                   ENDIF
156100991029     C*
156200991029     C* TIPO -11- PESO X GIORNI GIACENZA --------------------------------------*
156300991029     C                   Z-ADD     1             I
156400991029     C     '11'          LOOKUP    C2R(I)                                 98
156500991029IF  1C     *IN98         IFEQ      '0'
156600991029     C                   MOVEL     MSG(17)       $MSG
156700991029     C                   EXSR      SCRERR
156800991029X   1C                   ELSE
156900991029     C     D$DGGS        IFEQ      *ZEROS
157000991029     C                   Z-ADD     1             WT0155           15 5
157100991029     C                   ELSE
157200991029     C                   Z-ADD     D$DGGS        WT0155
157300991029     C                   ENDIF
157400991029     C     WPES          MULT      WT0155        Q2R(I)
157500991029E   1C                   ENDIF
157600000201     C*
157700000201     C* TIPO -12- GIORNI GIACENZA ---------------------------------------------*
157800000201     C                   Z-ADD     1             I
157900000201     C     '12'          LOOKUP    C2R(I)                                 98
158000000201IF  1C     *IN98         IFEQ      '0'
158100000201     C                   MOVEL     MSG(25)       $MSG
158200000201     C                   EXSR      SCRERR
158300000201X   1C                   ELSE
158400000201IF  2C     D$DGGS        IFEQ      *ZEROS
158500000201     C                   Z-ADD     1             Q2R(I)
158600000201X   2C                   ELSE
158700000201     C                   Z-ADD     D$DGGS        Q2R(I)
158800000201E   2C                   ENDIF
158900000201E   1C                   ENDIF
159000940908     C*
159100940908     C* TIPO -21- IMPONIBILE REALE --------------------------------------------*
159200940908     C                   Z-ADD     1             I
159300940908     C     '21'          LOOKUP    C2R(I)                                 98
159400940908     C     *IN98         IFEQ      '0'
159500940908     C                   MOVEL     MSG(11)       $MSG
159600940908     C                   EXSR      SCRERR
159700940908     C                   ELSE
159800940908     C* CERCO NELLA SCHIERA IL INPUT AL PGM L'IMPONIBILE REALE
159900940908     C                   Z-ADD     1             II                9 0
160000940908     C     556           LOOKUP    RCD(II)                                98
160100940908     C     *IN98         IFEQ      '1'
160200940908     C                   Z-ADD     RIM(II)       Q2R(I)
160300940908     C                   ENDIF
160400940908     C                   ENDIF
160500940908     C*
160600940908     C* TIPO -22- CONTRASEGNO -------------------------------------------------*
160700940908     C                   Z-ADD     1             I
160800940908     C     '22'          LOOKUP    C2R(I)                                 98
160900940908     C     *IN98         IFEQ      '0'
161000940908     C                   MOVEL     MSG(12)       $MSG
161100940908     C                   EXSR      SCRERR
161200940908     C                   ELSE
161300941201     C                   Z-ADD     D$DCAS        Q2R(I)
161400940908     C                   ENDIF
161500940908     C*
161600940908     C* TIPO -23- VALORE MERCE ASSICURATA -------------------------------------*
161700940908     C                   Z-ADD     1             I
161800940908     C     '23'          LOOKUP    C2R(I)                                 98
161900940908     C     *IN98         IFEQ      '0'
162000940908     C                   MOVEL     MSG(13)       $MSG
162100940908     C                   EXSR      SCRERR
162200940908     C                   ELSE
162300011227     C                   Z-ADD     D$DVAE        Q2R(I)
162400940908     C                   ENDIF
162500940908     C*
162600940908     C* TIPO -24- NUMERO DI GIORNI PAGAMENTO ----------------------------------*
162700940908     C                   Z-ADD     1             I
162800940908     C     '24'          LOOKUP    C2R(I)                                 98
162900940908     C     *IN98         IFEQ      '0'
163000940908     C                   MOVEL     MSG(14)       $MSG
163100940908     C                   EXSR      SCRERR
163200940908     C                   ELSE
163300940908     C                   Z-ADD     WGGP          Q2R(I)
163400940908     C                   ENDIF
163500940908     C*
163600940908     C                   ENDSR
163700940803     C*------------------------------------------------------------------------*
163800940803     C*? CHKVOC - CONTROLLA CHE TUTTE LE VOCI DA CALCOLARE SIANO VALIDE
163900940803     C*            ALLA DATA DELLA BOLLA ED EVENTUALMENTE CARICA LE GIUSTE
164000940803     C*            DECORRENZE
164100940803     C*            INPUT: - WDDSP (DATA SPEDIZIONE BOLLA)
164200940803     C*            OUTPUT:- SCHIERE VOCI COMPLETE ALLA DATA IN INPUT
164300940803     C*------------------------------------------------------------------------*
164400940603     C     CHKVOC        BEGSR
164500940603     C*
164600940607     C* CICLO PER TUTTE LE VOCI DA CALCOLARE
1647009406071    C                   DO        999           V
1648009406072    C     VC(V)         IFEQ      'S'
1649009407293    C     WDDSP         IFLT      VDDV(V)
165000940729     C     WDDSP         ORGT      VDSV(V)
165100940630     C                   Z-ADD     *ZEROS        VPRG(V)
165200940603     C                   MOVEL     *BLANKS       VTVO(V)
165300940704     C                   Z-ADD     *ZEROS        VDDV(V)
165400940704     C                   Z-ADD     *ZEROS        VDSV(V)
165500940603     C                   MOVEL     *BLANKS       VVRV(V)
165600940603     C                   MOVEL     *BLANKS       VTIV(V)
165700940603     C                   MOVEL     *BLANKS       VSPT(V)
165800940603     C                   Z-ADD     *ZEROS        VCMP(V)
165900940607     C                   MOVEL     *BLANKS       VRFI(V)
166000940607     C                   MOVEL     *BLANKS       VRFE(V)
166100940607     C                   MOVEL     *BLANKS       VRTI(V)
166200940607     C                   MOVEL     *BLANKS       VRTE(V)
166300940607     C                   MOVEL     *BLANKS       VRPI(V)
166400940607     C                   MOVEL     *BLANKS       VRPE(V)
166500940607     C                   MOVEL     *BLANKS       VRKI(V)
166600940607     C                   MOVEL     *BLANKS       VRKE(V)
166700940704     C                   Z-ADD     *ZEROS        VARF(V)
166800940704     C                   Z-ADD     *ZEROS        VARL(V)
166900940704     C                   Z-ADD     *ZEROS        VARO(V)
167000030113     C                   MOVEL     *BLANKS       VCOP(V)
167100030113     C                   MOVEL     *BLANKS       VDAT(V)
167200030113     C                   MOVEL     *BLANKS       VAID(V)
167300030422     C                   Z-ADD     *ZEROS        VRPV(V)
167400091009     C                   MOVEL     *BLANKS       Vric(V)
167500091009     C                   Z-ADD     *ZEROS        Vfil(V)
167600091009     C                   Z-ADD     *ZEROS        Vqta(V)
167700091009     C                   Z-ADD     *ZEROS        Vsgi(V)
167800091009     C                   Z-ADD     *ZEROS        Vsgf(V)
167900030422     C                   MOVEL     *BLANKS       VSPS(V)
168000940607     C                   MOVEL     'N'           VC(V)
168100940607     C* CERCO IL RECORD DELLA VOCE CON DEC/SCAD PER LA DATA BOLLA
168200940607     C                   Z-ADD     V             EVTVOC
168300940630     C                   Z-ADD     *ZEROS        EVTPRG
168400940603     C     KEYEVT        SETLL     ECEVT01L                           99
168500940603     C  N99              READ      ECEVT01L                               99
168600940607     C     *IN99         IFEQ      '1'
168700940829     C     EVTVOC        ORNE      V
168800991119     C                   MOVEL     MSG(18)       DSMSG
168900991119     C                   MOVE      V             DSVOCE
169000991119     C                   MOVEL     DSMSG         $MSG
169100940907     C                   EXSR      SCRERR
169200940607     C                   END
1693009407044    C     *IN99         DOWEQ     '0'
169400940607     C     EVTVOC        ANDEQ     V
169500940704     C     VC(V)         ANDEQ     'N'
1696009407045    C     EVTATB        IFEQ      *BLANKS
169700940729     C     WDDSP         ANDGE     EVTDDV
169800940729     C     WDDSP         ANDLE     EVTDSV
169900940603     C                   Z-ADD     EVTVOC        V
170000940630     C                   Z-ADD     EVTPRG        VPRG(V)
170100940607     C                   MOVEL     EVTTVO        VTVO(V)
170200940704     C                   Z-ADD     EVTDDV        VDDV(V)
170300940704     C                   Z-ADD     EVTDSV        VDSV(V)
170400090304     c* per pod immage forzo colli invece di spedizione, perchè non si
170500090304     C* vuole cambiare il tipo tariffa, la spedizione sarà una e i
170600090304     c* colli sono le immagini
170700090304     c                   if        v = 199
170800090304     C                   MOVEL     '01'          VVRV(V)
170900090304     c                   if        evttiv <> ' '
171000090304     C                   MOVEL     '01'          VTIV(V)
171100090304     c                   end
171200090304     c                   else
171300940607     C                   MOVEL     EVTVRV        VVRV(V)
171400940607     C                   MOVEL     EVTTIV        VTIV(V)
171500090304     c                   end
171600940607     C                   MOVEL     EVTSPT        VSPT(V)
171700940607     C                   Z-ADD     EVTCMP        VCMP(V)
171800940607     C                   MOVEL     EVTRFI        VRFI(V)
171900940607     C                   MOVEL     EVTRFE        VRFE(V)
172000940607     C                   MOVEL     EVTRTI        VRTI(V)
172100940607     C                   MOVEL     EVTRTE        VRTE(V)
172200940607     C                   MOVEL     EVTRPI        VRPI(V)
172300940607     C                   MOVEL     EVTRPE        VRPE(V)
172400940607     C                   MOVEL     EVTRKI        VRKI(V)
172500940607     C                   MOVEL     EVTRKE        VRKE(V)
172600940704     C                   Z-ADD     EVTARF        VARF(V)
172700940704     C                   Z-ADD     EVTARL        VARL(V)
172800940704     C                   Z-ADD     EVTARO        VARO(V)
172900030113     C                   MOVEL     EVTCOP        VCOP(V)
173000030113     C                   MOVEL     EVTDAT        VDAT(V)
173100030113     C                   MOVEL     EVTAID        VAID(V)
173200030422     C                   MOVEL     EVTXXX        DEVTXXX
173300030422     C                   Z-ADD     EVTRPV        VRPV(V)
173400030422     C                   MOVEL     EVTSPS        VSPS(V)
173500940607     C                   MOVEL     'S'           VC(V)
173600091009     c* recupero la filiale di competenza voce
173700091009     C     KEYECT        SETLL     ECECT01L
173800091009     c                   do        *hival
173900091009     C     KEYECT        READE     ECECT01L
174000091009     c                   if        %eof(ecect01l)
174100091009     c                   leave
174200091009     c                   end
174300091009IF  4C     ECTATB        IFne      *BLANKS
174400091009     C     WDDSP         orlt      EcTDDc
174500091009     C     WDDSP         orgt      EcTDSc
174600091009     c                   iter
174700091009     c                   end
174800091009     c                   eval      vric(v) = ectric
174900091009     c                   leave
175000091009     c                   enddo
1751009407045    C                   ENDIF
175200940603     C                   READ      ECEVT01L                               99
1753009407044    C                   ENDDO
1754009407043    C                   ENDIF
1755009407042    C                   ENDIF
1756009406071    C                   ENDDO
175700940603     C*
175800940603     C                   ENDSR
175900940803     C*------------------------------------------------------------------------*
176000940803     C*? VALVOC - VALORIZZA LE VOCI VALIDE
176100940803     C*------------------------------------------------------------------------*
176200940526     C     VALVOC        BEGSR
176300940526     C*
176400940603     C* CICLO PER TUTTE LE VOCI 'DA CALCOLARE'
176500940603     C                   DO        999           V
176600940607     C     VC(V)         IFEQ      'S'
176700940630     C                   Z-ADD     V             WVOC
176800940630     C                   Z-ADD     VPRG(V)       WPRG
176900000306     C                   MOVEL     VTIV(V)       WTIV
177000940607     C* CERCA TARIFFA
177100940607     C                   EXSR      CERTAR
177200940729     C*
177300940908     C* SE NON C'E' ERRORE (ES. MANCA TARIFFA)
177400940908     C* E IL MANCA TARIFFA ERA DA SEGNALARE
177500940913     C     $ERR          IFEQ      'N'
177600940908     C     WMSGMT        ANDEQ     'S'
177700940720     C*
177800940720     C* CALCOLA QTA
177900941107     C*
178000941107     C* SE TIPO VALORIZZAZIONE = Q=QUANTITA  OPPURE P=PERCENTUALE
178100941107     C* RECUPERO LA QTA O IL VALORE DI RIFERIMENTO
178200020109     C                   Z-ADD     *ZEROS        WQTA             21 9
178300940701     C     VVRV(V)       IFNE      *BLANKS
178400940721     C                   Z-ADD     1             I
178500940701     C     VVRV(V)       LOOKUP    C2R(I)                                 98
178600940701     C     *IN98         IFEQ      '0'
178700940720     C                   MOVEL     MSG(19)       $MSG
178800940907     C                   EXSR      SCRERR
178900940729     C                   ELSE
179000941107     C* SE E' UNA TARIFFA DOVE E' STATO CALCOLATO IL PESO TASSABILE
179100941107     C* (GIA' CALCOLATO NELLA RICERCA DELLO SCAGLIONE) PRENDO QTA
179200941107     C* UTILIZZATA PER LA RICERCA DELLO SCAGLIONE
179300941107     C* ALTRIMENTI PRENDO QTA DELLA SCHIERA
179400941107     C     VSPT(V)       IFNE      *BLANKS
179500961202     C     WQTAPT        ANDGT     *ZEROS
179600941107     C                   Z-ADD     WQTAPT        WQTA
179700941107     C                   ELSE
179800100422     c* per tipo voce = importo voce x isolamento
179900100423     c                   if        vvrv(v) <> '26'
180000100423     C                   Z-ADD     Q2R(I)        WQTA
180100100423     c                   else
180200100422     C                   Z-ADD     vimp(v)       WQTA
180300100422     c                   end
180400941107     C                   ENDIF
180500940729     C                   MOVEL     T2R(I)        WTTQ              1
180600940720     C                   ENDIF
180700940721     C                   ENDIF
180800940803     C*
180900940803     C* CONTROLLO L'ARROTONDAMENTO
181000940803     C                   EXSR      ARROTO
181100940720     C*
181200020109     C                   Z-ADD     *ZEROS        WVVOCE           21 9
181300090914     C                   Z-ADD     *ZEROS        WVVO25           15 5
181400100506     c*
181500121120     c                   if        D$GFSF = 'S'
181600091009     c                   exsr      sresvoca
181700121120     c                   end
181800940802     C*
181900940802     C* SE TIPO VOCE V=VOCE NORMALE
182000940802     C* IN BASE AL TIPO TARIFFA CALCOLA IMPORTO A QTA, FISSO, PERCENTUALE
182100940802     C     VTVO(V)       IFEQ      'V'
182200940802     C                   EXSR      CALVOC
182300940802     C                   ENDIF
182400940720     C*
182500940802     C* SE TIPO VOCE M=MAGGIORAZIONE
182600940802     C* RECUPERO TUTTE LE VOCI DA MAGGIORARE E APPLICO LA PERCENTUALE
182700940802     C     VTVO(V)       IFEQ      'M'
182800940906     C                   EXSR      CERMAG
182900940802     C                   ENDIF
183000940802     C*
183100940802     C* CARICO TIPO TARIFFA -> IMPONIBILE PER COMPETENZE SEDE (SOMMA DI TUTTE
183200991027     C* LE VOCI PRECEDENTI TRANNE I TRANSITI)
183300991027     C     VS(V)         IFEQ      'S'
183400940802     C                   Z-ADD     1             I
183500940802     C     '25'          LOOKUP    C2R(I)                                 98
183600940802     C     *IN98         IFEQ      '0'
183700940802     C                   MOVEL     MSG(15)       $MSG
183800940907     C                   EXSR      SCRERR
183900940802     C                   ELSE
184000940824     C                   ADD       WVVO25        Q2R(I)
184100940802     C                   ENDIF
184200991027     C                   ENDIF
184300940707     C*
184400940721     C                   ENDIF
184500940729     C                   ENDIF
184600940721     C                   ENDDO
184700940603     C*
184800091007     C                   ENDSR
184900091007     C*------------------------------------------------------------------------*
185000100518     C*? sresvoc10 saldi x tempi uffici
185100091007     C*------------------------------------------------------------------------*
185200100518     C     sresvoc10     BEGSR
185300091009     c* memorizzo in schiera qta scaglioni e filiale  nel caso dovessi
185400091009     c* sottrarre la qta
185500091009     c                   select
185600100518     C     vric(i)       WHENEQ    'LA'
185700111020     C                   Z-ADD     W$lna         v10fil
185800100518     C     vric(i)       WHENEQ    'LP'
185900100518     C                   Z-ADD     W$LNP         v10fil
186000100518     C     vric(i)       WHENEQ    'TA'
186100100518     C                   Z-ADD     D$DTFA        v10fil
186200100518     C     vric(i)       WHENEQ    'TP'
186300100518     C                   Z-ADD     W$TFP         v10fil
186400100518     C     vric(i)       WHENEQ    'T1'
186500100518     C                   Z-ADD     D$DAR5flp     v10fil
186600100518     C     vric(i)       WHENEQ    'T2'
186700100518     C                   Z-ADD     D$DAR5fl2     v10fil
186800100518     C     vric(i)       WHENEQ    'T3'
186900100518     C                   Z-ADD     D$DAR5fl3     v10fil
187000100518     C     vric(i)       WHENEQ    'T4'
187100100518     C                   Z-ADD     D$DAR5fl4     v10fil
187200100518     C     vric(i)       WHENEQ    'T5'
187300100518     C                   Z-ADD     D$DAR5fl5     v10fil
187400100506     c* se blanks forzo sede
187500100506     c                   other
187600100518     C                   Z-ADD     46            v10fil
187700091009     c                   endsl
187800100518     c* aggancio il file saldi esvoc10f, se trovo sommo altrimenti scrivo
187900100518     c                   eval      v10dat = wdsp
188000100518     c                   eval      v10voc = i
188100100518     c     keyes10       chain     esvoc11l
188200091012     c*
188300100518     c                   if        not %found(esvoc11l)
188400100518     c                   eval      v10imp = vimp(i)
188500100518     c                   write     esvoc010
188600091007     c                   else
188700100518     c                   add       vimp(i)       v10imp
188800100518     c                   update    esvoc010
188900091007     c                   end
189000091007     c*
189100940526     C                   ENDSR
189200091009     C*------------------------------------------------------------------------*
189300110309     C*? sresvocs- sottraggo la qta dai saldi perchè importo voce = 0 oppure
189400110309     C*? aggiungo l'importo se importo > 0
189500091009     C*------------------------------------------------------------------------*
189600091009     C     sresvocs      BEGSR
189700110309     c* aggancio il file saldi esvoc00F x sottrarre qta o per aggiungere
189800110309     c* importo
189900091012     c                   eval      vocfil = vfil(i)
190000091009     c                   eval      vocdat = wdsp
190100091012     c                   eval      vocvoc = i
190200091012     c                   eval      vocsgi = vsgi(i)
190300091012     c                   eval      vocsgf = vsgf(i)
190400091009     c     keyes         chain     esvoc01l
190500091009     c                   if        %found(esvoc01l)
190600110309     c                   if        vimp(i) = 0
190700091012     c                   sub       vqta(i)       vocqta
190800091012     c                   sub       1             vocnsp
190900091012     C                   sub       D$DNCL        vocncl
191000091016     c* non aggiorna il numero dei bancali BNB se  voce 34 e DISK B
191100091016     c* (equivalente a serie > 0) e particolarità consegne bancali
191200091016if  1c                   IF        i = 34 and
191300091016     c                             d$dnrs > 0 and
191400091016     c                              %subst(d$dgva: 1: 1) = 'O'
191500091016     c                   else
191600091016     c                   sub       banvoc        vocban
191700091016e   1c                   endif
191800091012     c                   sub       pkfvoc        vocpkf
191900091012     c                   sub       vlfvoc        vocvlf
192000110309     c                   else
192100110309     c* imposto l'importo
192200110309     c                   add       vimp(i)       vocimp
192300110309     c                   end
192400091009     c                   update    esvoc000
192500091009     c                   end
192600091009     c*
192700091009     C                   ENDSR
192800100518     C*------------------------------------------------------------------------*
192900100518     C*? sresvoca  - saldi x tempi uffici
193000100518     C*------------------------------------------------------------------------*
193100100518     C     sresvoca      BEGSR
193200100518     c* memorizzo in schiera importo voce per filiale
193300100518     c                   eval      vqta(v)= wqta
193400100518     c                   eval      vsgi(v) = evdsgi
193500100518     c                   eval      vsgf(v) = evdsgf
193600100518     c                   select
193700100518     C     vric(v)       WHENEQ    'LA'
193800111020     C                   Z-ADD     W$lna         vfil(v)
193900100518     C     vric(v)       WHENEQ    'LP'
194000100518     C                   Z-ADD     W$LNP         vfil(v)
194100100518     C     vric(v)       WHENEQ    'TA'
194200100518     C                   Z-ADD     D$DTFA        vfil(v)
194300100518     C     vric(v)       WHENEQ    'TP'
194400100518     C                   Z-ADD     W$TFP         vfil(v)
194500100518     C     vric(v)       WHENEQ    'T1'
194600100518     C                   Z-ADD     D$DAR5flp     vfil(v)
194700100518     C     vric(v)       WHENEQ    'T2'
194800100518     C                   Z-ADD     D$DAR5fl2     vfil(v)
194900100518     C     vric(v)       WHENEQ    'T3'
195000100518     C                   Z-ADD     D$DAR5fl3     vfil(v)
195100100518     C     vric(v)       WHENEQ    'T4'
195200100518     C                   Z-ADD     D$DAR5fl4     vfil(v)
195300100518     C     vric(v)       WHENEQ    'T5'
195400100518     C                   Z-ADD     D$DAR5fl5     vfil(v)
195500100518     c* se blanks forzo sede
195600100518     c                   other
195700100518     C                   Z-ADD     46            vfil(v)
195800100518     c                   endsl
195900100518     c* aggancio il file saldi esvoc00f, se trovo sommo altrimenti scrivo
196000100518     c                   eval      vocfil = vfil(v)
196100100518     c                   eval      vocdat = wdsp
196200100518     c                   eval      vocvoc = v
196300100518     c                   eval      vocsgi = evdsgi
196400100518     c                   eval      vocsgf = evdsgf
196500100518     c                   eval      vocvrv = vvrv(v)
196600100518     c                   clear                   vocqta
196700100518     c                   clear                   vocnsp
196800100518     c                   clear                   vocncl
196900100518     c                   clear                   vocpkf
197000100518     c                   clear                   vocvlf
197100100518     c                   clear                   vocban
197200110311     c                   clear                   vocimp
197300100518     c     keyes         chain     esvoc01l
197400100518     C     WTTQ          IFEQ      'F'
197500100518     c                   add       1             vocqta
197600100518     c                   else
197700100518     c                   add       wqta          vocqta
197800100518     c                   end
197900100518     c                   add       1             vocnsp
198000100518     C                   add       D$DNCL        vocncl
198100100518     c* non aggiorna il numero dei bancali BNB se  voce 34 e DISK B
198200100518     c* (equivalente a serie > 0) e particolarità consegne bancali
198300100518if  1c                   IF        v = 34 and
198400100518     c                             d$dnrs > 0 and
198500100518     c                              %subst(d$dgva: 1: 1) = 'O'
198600100518     c                   else
198700100518     c                   add       banvoc        vocban
198800100518e   1c                   endif
198900100518     c                   add       pkfvoc        vocpkf
199000100518     c                   add       vlfvoc        vocvlf
199100100518     c*
199200100518     c                   if        not %found(esvoc01l)
199300100518     c                   write     esvoc000
199400100518     c                   else
199500100518     c                   update    esvoc000
199600100518     c                   end
199700100518     c*
199800100518     C                   ENDSR
199900940803     C*------------------------------------------------------------------------*
200000091012     C*? srqta  - CALCOLA LE qta della spedizione
200100940803     C*------------------------------------------------------------------------*
200200091012     C     srqta         BEGSR
200300091012     c* peso
200400091012IF  2C     D$DPKC        IFGT      *ZEROS
200500091012     C     D$DNCL        ANDEQ     D$DNCP
200600091012     C                   z-add     D$DPKC        Pkfvoc
200700091012X   2C                   ELSE
200800091012IF  3C     D$DPKF        IFGT      D$DPKC
200900091012     C                   z-add     D$DPKF        Pkfvoc
201000091012X   3C                   ELSE
201100091012     C                   z-add     D$DPKC        Pkfvoc
201200091012E   3C                   ENDIF
201300091012E   2C                   ENDIF
201400091012     c* volume
201500091012IF  1C     D$DFVF        IFEQ      'T'
201600091012     C                   z-add     D$DVLF        vlfvoc
201700091012X   1C                   ELSE
201800091012IF  2C     D$DVLC        IFGT      *ZEROS
201900091012     C     D$DNCL        ANDEQ     D$DNCR
202000091012     C                   z-add     D$DVLC        vlfvoc
202100091012X   2C                   ELSE
202200091012IF  3C     D$DVLF        IFGT      D$DVLC
202300091012     C                   z-add     D$DVLF        vlfvoc
202400091012X   3C                   ELSE
202500091012     C                   z-add     D$DVLC        vlfvoc
202600091012E   3C                   ENDIF
202700091012E   2C                   ENDIF
202800091012E   1C                   ENDIF
202900091012     C                   eval      banvoc= D$DAR5NBA + D$DAR5NB2
203000091012     C                   ENDSR
203100091012     C*------------------------------------------------------------------------*
203200091012     C*? CALVOC - CALCOLA LE VOCI NORMALI
203300091012     C*------------------------------------------------------------------------*
203400091012     C     CALVOC        BEGSR
203500011227     C*
203600011227     C* CONVERTE GLI IMPORTI DELLE TARIFFE SE LA DIVISA DELLE ANAGRAFICHE C/E E'
203700011227     C* DIVERSA DALLA DIVISA DI OUTPUT DEL C/E
203800011227     C                   Z-ADD     EVDITR        W$ITR
203900011227     C                   Z-ADD     EVDIMI        W$IMI
204000011227     C                   Z-ADD     EVDIMA        W$IMA
204100011227IF  1C     WDIVAC        IFNE      WDIVCE
204200011227IF  2C     EVDITR        IFNE      *ZEROS
204300011227     C                   Z-ADD     EVDITR        §IMI
204400011227     C                   MOVEL     WDIVAC        §DVI
204500011227     C                   EXSR      CNVEUR
204600011227     C                   Z-ADD     §IMO          W$ITR
204700011227E   2C                   ENDIF
204800011227IF  2C     EVDIMI        IFNE      *ZEROS
204900011227     C                   Z-ADD     EVDIMI        §IMI
205000011227     C                   MOVEL     WDIVAC        §DVI
205100011227     C                   EXSR      CNVEUR
205200011227     C                   Z-ADD     §IMO          W$IMI
205300011227E   2C                   ENDIF
205400011227IF  2C     EVDIMA        IFNE      *ZEROS
205500011227     C                   Z-ADD     EVDIMA        §IMI
205600011227     C                   MOVEL     WDIVAC        §DVI
205700011227     C                   EXSR      CNVEUR
205800011227     C                   Z-ADD     §IMO          W$IMA
205900011227E   2C                   ENDIF
206000011227E   1C                   ENDIF
206100940802     C*
206200940802     C* CALCOLO IN BASE AL TIPO TARIFFA
206300940802     C*
206400940802     C     WTTQ          IFEQ      'Q'
206500940802     C     WQTA          ANDNE     *ZEROS
206600011227     C     W$ITR         ANDNE     *ZEROS
206700011227     C     WQTA          MULT      W$ITR         WVVOCE
206800940802     C                   ENDIF
206900940802     C*
207000940802     C     WTTQ          IFEQ      'F'
207100011227     C                   Z-ADD     W$ITR         WVVOCE
207200940802     C                   ENDIF
207300940802     C*
207400940802     C     WTTQ          IFEQ      'P'
207500940802     C     WQTA          ANDNE     *ZEROS
207600940802     C     EVDPER        ANDNE     *ZEROS
207700940802     C     WQTA          DIV       100           WVVOCE
207800940802     C     WVVOCE        MULT      EVDPER        WVVOCE
207900940802     C                   ENDIF
208000940802     C*
208100940802     C* CONTROLLO IMPORTO MINIMO
208200011227     C     W$IMI         IFNE      *ZEROS
208300011227     C     WVVOCE        ANDLT     W$IMI
208400011227     C                   Z-ADD     W$IMI         WVVOCE
208500940802     C                   ENDIF
208600011227     C     W$IMA         IFNE      *ZEROS
208700011227     C     WVVOCE        ANDGT     W$IMA
208800011227     C                   Z-ADD     W$IMA         WVVOCE
208900940802     C                   ENDIF
209000940802     C*
209100940802     C* CONTROLLO SEGNO E CARICO IMPORTO NELLA SCHIERA
209200090914     C                   Z-ADD(H)  WVVOCE        N133             13 5
209300940802     C     EVDSEG        IFEQ      '+'
209400020130     C                   Z-ADD     N133          VIMP(V)
209500020130     C                   ADD       N133          WVVO25
209600940802     C                   ELSE
209700020130     C                   Z-SUB     N133          VIMP(V)
209800020130     C                   SUB       N133          WVVO25
209900940802     C                   ENDIF
210000940802     C*
210100940802     C                   ENDSR
210200940907     C*------------------------------------------------------------------------*
210300940907     C*?ARROTO - ARROTONDA LA QUANTITA'
210400940907     C*          INPUT  : WQTA (DA ARROTONDARE)
210500940907     C*          OUTPUT : WQTA (ARROTONDATA)
210600940907     C*------------------------------------------------------------------------*
210700940907     C     ARROTO        BEGSR
210800940907     C*
210900940907     C* CONTROLLO SE ESISTE L'ARROTONDAMENTO
211000940907     C     WQTA          IFNE      *ZEROS
211100940907     C     VARL(V)       ANDNE     *ZEROS
211200940907     C     WQTA          IFLE      VARL(V)
211300010202     C                   Z-ADD     VARF(V)       WARR             17 5
211400940907     C                   ELSE
211500940907     C                   Z-ADD     VARO(V)       WARR
211600940907     C                   ENDIF
211700940907     C* CONTROLLO SE DATO GIA' ARRORONDATO
211800940907     C     WQTA          DIV       WARR          WINT             15 0
211900970730     C                   MVR                     WT0155           15 5    99
212000940907     C     *IN99         IFEQ      *OFF
212100940907     C     WINT          MULT      WARR          WQTA
212200940907     C                   ADD       WARR          WQTA
212300940907     C                   ENDIF
212400940907     C                   ENDIF
212500940907     C*
212600940907     C                   ENDSR
212700940907     C*------------------------------------------------------------------------*
212800940907     C*? CALPT  - CALCOLA IL PESO TASSABILE
212900940907     C*------------------------------------------------------------------------*
213000940907     C     CALPT         BEGSR
213100940907     C*
213200940907     C* CALCOLO PESO TASSABILE DAL VOLUME
213300941019     C     WVOL          MULT      EVDRPV        WPTAS
213400940907     C     WPTAS         MULT      100           WPTAS
213500940907     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
213600940907     C     VSPT(V)       IFEQ      '1'
213700940907     C                   Z-ADD     WPTAS         WQTA
213800940907     C                   ENDIF
213900940907     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
214000940907     C     VSPT(V)       IFEQ      '2'
214100991008     C     WPTAS         IFGT      WPES
214200940907     C                   Z-ADD     WPTAS         WQTA
214300940907     C                   ENDIF
214400940907     C                   ENDIF
214500940907     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
214600940907     C     VSPT(V)       IFEQ      '3'
214700991008     C     WPTAS         IFLT      WPES
214800940907     C                   Z-ADD     WPTAS         WQTA
214900940907     C                   ENDIF
215000940907     C                   ENDIF
215100940907     C*
215200940907     C                   ENDSR
215300030422     C*------------------------------------------------------------------------*
215400030422     C*? CALPS  - CALCOLA IL PESO TASSABILE PER RICERCA SCAGLIONE
215500030422     C*------------------------------------------------------------------------*
215600030422     C     CALPS         BEGSR
215700040209     C                   CLEAR                   WPTASR2
215800030422     C*
215900030422     C* CALCOLO PESO TASSABILE DAL VOLUME
216000030423     C     WVOL          MULT      VRPV(V)       WPTASR
216100030423     C     WPTASR        MULT      100           WPTASR
216200030422     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
216300030423     C     VSPS(V)       IFEQ      '1'
216400040209     C                   Z-ADD     WPTASR        WPTASR2
216500030422     C                   ENDIF
216600030422     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
216700030423     C     VSPS(V)       IFEQ      '2'
216800030423     C     WPTASR        IFGT      WPES
216900040209     C                   Z-ADD     WPTASR        WPTASR2
217000040209     c                   ELSE
217100040209     C                   Z-ADD     WPES          WPTASR2
217200030422     C                   ENDIF
217300030422     C                   ENDIF
217400030422     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
217500030423     C     VSPS(V)       IFEQ      '3'
217600030423     C     WPTASR        IFLT      WPES
217700040209     C                   Z-ADD     WPTASR        WPTASR2
217800040209     C                   ELSE
217900040209     C                   Z-ADD     WPES          WPTASR2
218000030422     C                   ENDIF
218100030422     C                   ENDIF
218200030422     C*
218300030422     C                   ENDSR
218400940803     C*------------------------------------------------------------------------*
218500940906     C*? CERMAG - CERCA LE MAGGIORAZIONI
218600940803     C*------------------------------------------------------------------------*
218700940906     C     CERMAG        BEGSR
218800940908     C*
218900940906     C* SE PERCENTUALE DIVERSA DA ZERO NON CERCO NEMMENO LA MAGGIORAZIONE
219000940906     C     EVDPER        IFNE      *ZEROS
219100940908     C                   MOVEL     'N'           WMAGOK            1
219200940906     C*
219300941024     C* CONTROLLA SE MAGGIORAZIONI IN MEMORIA
219400940906     C                   Z-ADD     1             M                 6 0
219500940906     C     M             DOWLE     WULTMA
219600940906     C     WVOC          IFEQ      MVOM(M)
219700940906     C     WPRG          ANDEQ     MPRM(M)
219800940908     C                   MOVEL     'S'           WMAGOK
219900940906     C                   Z-ADD     MVOM(M)       EVMVOM
220000940906     C                   Z-ADD     MPRM(M)       EVMPRM
220100940906     C                   Z-ADD     MVOC(M)       EVMVOC
220200940906     C                   Z-ADD     MPRG(M)       EVMPRG
220300100408     C                   Z-ADD     Mviso(M)      EVMviso
220400100408     C                   Z-ADD     Mddti(M)      EVMddt
220500100408     C                   Z-ADD     Mdsti(M)      EVMdst
220600100408     C                   movel     Mapp(M)       EVMapp
220700940906     C                   EXSR      CALMAG
220800940906     C                   ENDIF
220900940906     C                   ADD       1             M
221000940906     C                   ENDDO
221100940908     C*
221200940906     C* SE NON TROVATA IN MEMORIA E FINE SCHIERA LEGGO SUL FILE
221300940906     C     WMAGOK        IFEQ      'N'
221400041118     C     WULTMA        ANDGE     600
221500940906     C*
221600030331     C                   Z-ADD     WVOC          EVMVOM
221700940802     C                   Z-ADD     WPRG          EVMPRM
221800940802     C                   Z-ADD     *ZEROS        EVMVOC
221900940802     C                   Z-ADD     *ZEROS        EVMPRG
222000940802     C     KEYEVM        SETLL     ECEVM01L                           99
222100940802     C  N99              READ      ECEVM01L                               99
222200940802     C     *IN99         IFEQ      '1'
222300940802     C                   MOVEL     MSG(22)       $MSG
222400940907     C                   EXSR      SCRERR
222500940802     C                   END
222600940906     C     *IN99         DOWEQ     '0'
222700940802     C     EVMVOM        ANDEQ     WVOC
222800940802     C     EVMPRM        ANDEQ     WPRG
222900940906     C     EVMATB        IFEQ      *BLANKS
223000940906     C                   EXSR      CALMAG
223100940906     C                   ENDIF
223200940802     C                   READ      ECEVM01L                               99
223300940906     C                   ENDDO
223400940802     C*
223500940906     C                   ENDIF
223600940906     C                   ENDIF
223700940906     C*
223800940802     C                   ENDSR
223900940906     C*------------------------------------------------------------------------*
224000940906     C*? CALMAG - CALCOLA LA MAGGIORAZIONE
224100940906     C*------------------------------------------------------------------------*
224200940906     C     CALMAG        BEGSR
224300011227     C*
224400011227     C* CONVERTE GLI IMPORTI DELLE TARIFFE SE LA DIVISA DELLE ANAGRAFICHE C/E E'
224500011227     C* DIVERSA DALLA DIVISA DI OUTPUT DEL C/E
224600011227     C                   Z-ADD     EVDIMI        W$IMI
224700011227     C                   Z-ADD     EVDIMA        W$IMA
224800011227IF  1C     WDIVAC        IFNE      WDIVCE
224900011227IF  2C     EVDIMI        IFNE      *ZEROS
225000011227     C                   Z-ADD     EVDIMI        §IMI
225100011227     C                   MOVEL     WDIVAC        §DVI
225200011227     C                   EXSR      CNVEUR
225300011227     C                   Z-ADD     §IMO          W$IMI
225400011227E   2C                   ENDIF
225500011227IF  2C     EVDIMA        IFNE      *ZEROS
225600011227     C                   Z-ADD     EVDIMA        §IMI
225700011227     C                   MOVEL     WDIVAC        §DVI
225800011227     C                   EXSR      CNVEUR
225900011227     C                   Z-ADD     §IMO          W$IMA
226000011227E   2C                   ENDIF
226100011227E   1C                   ENDIF
226200940906     C*
226300940906     C                   Z-ADD     EVMVOC        I
226400000407     C* CALCOLA SOLO SE LA VOCE DA MAGGIORARE E' DA CALCOLARE
226500000407     C     VC(I)         IFEQ      'S'
226600940906     C* CALCOLA LA PERCENTUALE
226700940906     C     VIMP(I)       DIV       100           WVVOCE
226800940906     C     WVVOCE        MULT      EVDPER        WVVOCE
226900940906     C* CONTROLLO IMPORTO MINIMO
227000011227     C     W$IMI         IFNE      *ZEROS
227100011227     C     WVVOCE        ANDLT     W$IMI
227200011227     C                   Z-ADD     W$IMI         WVVOCE
227300940906     C                   ENDIF
227400940906     C* CONTROLLO IMPORTO MASSIMO
227500011227     C     W$IMA         IFNE      *ZEROS
227600011227     C     WVVOCE        ANDGT     W$IMA
227700011227     C                   Z-ADD     W$IMA         WVVOCE
227800940906     C                   ENDIF
227900940906     C* CONTROLLO SEGNO E AGGIUNGO O TOLGO IMPORTO NELLA SCHIERA
228000090914     C                   Z-ADD(H)  WVVOCE        N133
228100940906     C     EVDSEG        IFEQ      '+'
228200100408     c* ISOLO la maggiorazione e la applico solo se richiesto
228300100408     c                   if        evmapp = 'N' and
228400100408     C                             WDDSP >= evmddt and
228500100408     C                             WDDSP <= evmdst and
228600100422     c                             evmviso <> 0 and vc(evmviso)='S'
228700100408     c                   add       n133          vimp(evmviso)
228800100420     c                   if        vs(evmviso) = 'S'
228900100420     C                   ADD       N133          WVVO25
229000100420     c                   end
229100100408     c                   else
229200020130     C     VIMP(I)       ADD       N133          VIMP(I)
229300050503     c                   if        vs(i) = 'S'
229400020130     C                   ADD       N133          WVVO25
229500050503     c                   end
229600100420     c                   end
229700940906     C                   ELSE
229800100408     C* SEGNO NEGATIVO
229900100420     c* se il segno è negativo isolo la voce con segno positivo e la applico
230000100422     c* sempre
230100020130     C     VIMP(I)       SUB       N133          VIMP(I)
230200050503     c                   if        vs(i) = 'S'
230300020130     C                   SUB       N133          WVVO25
230400050503     c                   end
230500100420     c                   if        evmapp = 'S' and
230600100420     C                             WDDSP >= evmddt and
230700100420     C                             WDDSP <= evmdst and
230800100422     c                             evmviso <> 0 and vc(evmviso)='S'
230900100420     c                   add       n133          vimp(evmviso)
231000100420     c                   if        vs(evmviso) = 'S'
231100100420     C                   ADD       N133          WVVO25
231200100420     c                   end
231300100420     c                   end
231400940906     C                   ENDIF
231500000407     C                   ENDIF
231600940906     C*
231700940906     C                   ENDSR
231800940908     C*------------------------------------------------------------------------*
231900030429     C*?CERTAR - CERCA LA TARIFFA
232000940908     C*------------------------------------------------------------------------*
232100940908     C     CERTAR        BEGSR
232200940908     C*
232300940908     C* IMPOSTO CAMPI LAVORO
232400940908     C                   MOVEL     'N'           WFLTT             1
232500940908     C                   MOVEL     *BLANKS       WRIF
232600940908     C                   Z-ADD     WDDSP         WDSP
232700940908     C                   MOVEL     *BLANKS       WTT1              3
232800940908     C                   MOVEL     *BLANKS       WTT2              3
232900950519     C                   MOVEL     *BLANKS       WCTSM             2
233000940908     C                   MOVEL     'S'           WMSGMT            1
233100941107     C                   Z-ADD     *ZEROS        WPTAS            15 3
233200030423     C                   Z-ADD     *ZEROS        WPTASR           15 3
233300040209     C                   Z-ADD     *ZEROS        WPTASR2          15 3
233400030423     C*
233500030423     C* calcola il peso tassabile da ricercare nello scaglione
233600030423     C     VSPS(V)       IFGT      *ZEROS
233700030423     C                   EXSR      CALPS
233800040209     C                   Z-ADD     WPTASR2       WQTA
233900030423     C                   ELSE
234000940908     C*
234100940908     C* CERCA LA QTA DA RICERCARE NELLO SCAGLIONE
234200940908     C                   Z-ADD     *ZEROS        WQTA
234300940908     C                   Z-ADD     1             I
234400940908     C     VTIV(V)       LOOKUP    C2R(I)                                 98
234500940908     C     *IN98         IFEQ      '0'
234600940908     C                   MOVEL     MSG(20)       $MSG
234700940908     C                   EXSR      SCRERR
234800940908     C                   ELSE
234900940908     C                   Z-ADD     Q2R(I)        WQTA
235000940908     C                   END
235100030423     C                   ENDIF
235200940908     C*
235300940908     C* IMPOSTO IL FLAG ITALIA/ESTERO
235400971211     C                   Z-ADD     W$LNP         F
235500111020     C                   Z-ADD     W$lna         F1                4 0
235600020805     C     FIE(F)        IFEQ      'E'
235700020805     C     FIE(F1)       OREQ      'E'
235800940908     C                   MOVEL     'E'           WIE               1
235900940908     C                   ELSE
236000940908     C                   MOVEL     'I'           WIE
236100940908     C                   ENDIF
236200940908     C*
236300940908     C* SE RICHIESTO, TENTATIVO PER CLIENTE
236400940908     C     WFLTT         IFEQ      'N'
236500940908     C     WIE           ANDEQ     'I'
236600940908     C     VRKI(V)       ANDNE     *BLANKS
236700940908     C                   MOVEL     VRKI(V)       WRIF
236800940908     C                   EXSR      CERCLI
236900940908     C                   ENDIF
237000940908     C     WFLTT         IFEQ      'N'
237100940908     C     WIE           ANDEQ     'E'
237200940908     C     VRKE(V)       ANDNE     *BLANKS
237300940908     C                   MOVEL     VRKE(V)       WRIF
237400940908     C                   EXSR      CERCLI
237500940908     C                   ENDIF
237600940908     C*
237700940908     C* SE RICHIESTO, TENTATIVO PER CAP
237800940908     C     WFLTT         IFEQ      'N'
237900940908     C     WIE           ANDEQ     'I'
238000940908     C     VRPI(V)       ANDNE     *BLANKS
238100940908     C                   MOVEL     VRPI(V)       WRIF
238200940908     C                   EXSR      CERCAP
238300940908     C                   ENDIF
238400940908     C     WFLTT         IFEQ      'N'
238500940908     C     WIE           ANDEQ     'E'
238600940908     C     VRPE(V)       ANDNE     *BLANKS
238700940908     C                   MOVEL     VRPE(V)       WRIF
238800940908     C                   EXSR      CERCAP
238900940908     C                   ENDIF
239000940908     C*
239100940908     C* SE RICHIESTO, TENTATIVO PER CODICE TASSAZIONE
239200940908     C     WFLTT         IFEQ      'N'
239300940908     C     WIE           ANDEQ     'I'
239400940908     C     VRTI(V)       ANDNE     *BLANKS
239500940908     C                   MOVEL     VRTI(V)       WRIF
239600940908     C                   EXSR      CERTAS
239700940908     C                   ENDIF
239800940908     C     WFLTT         IFEQ      'N'
239900940908     C     WIE           ANDEQ     'E'
240000940908     C     VRTE(V)       ANDNE     *BLANKS
240100940908     C                   MOVEL     VRTE(V)       WRIF
240200940908     C                   EXSR      CERTAS
240300940908     C                   ENDIF
240400940908     C*
240500940908     C* SE RICHIESTO, TENTATIVO PER CODICE FILIALE
240600940908     C     WFLTT         IFEQ      'N'
240700940908     C     WIE           ANDEQ     'I'
240800940908     C     VRFI(V)       ANDNE     *BLANKS
240900940908     C                   MOVEL     VRFI(V)       WRIF
241000940908     C                   EXSR      CERFIL
241100940908     C                   ENDIF
241200940908     C     WFLTT         IFEQ      'N'
241300940908     C     WIE           ANDEQ     'E'
241400940908     C     VRFE(V)       ANDNE     *BLANKS
241500940908     C                   MOVEL     VRFE(V)       WRIF
241600940908     C                   EXSR      CERFIL
241700940908     C                   ENDIF
241800940908     C*
241900940908     C* SE TARIFFA NON ANCORA TROVATA CERCA TARIFFA DI CARTELLO
242000940908     C     WFLTT         IFEQ      'N'
242100940908     C                   EXSR      CERCAR
242200940908     C                   ENDIF
242300040903     c* se ho manca tariffa per la voce 167 xchè peso tassabile > 2,5 kg
242400040903     c* disattivo tale voce e lascio la 168 e 169 e viceversa
242500040903     c                   if        wvoc = 167
242600040903     c                   if        wfltt = 'N' and WQTA > §DFEPKGD
242700040903     c                   movel     'N'           wmsgmt
242800040903     c                   movel     'N'           vc(167)
242900040903     c                   else
243000040903     c                   movel     'N'           vc(168)
243100040903     c                   movel     'N'           vc(169)
243200040903     c                   end
243300040903     c                   end
243400940908     C*
243500940908     C* CONTROLLO MANCA TARIFFA
243600940908     C     WFLTT         IFEQ      'N'
243700940908     C     WMSGMT        IFEQ      'S'
243800940908     C                   MOVEL     MSG(21)       DSMSG
243900940908     C                   MOVE      WVOC          DSVOCE
244000940908     C                   MOVE      WTT1          DSTT1
244100940908     C                   MOVE      WTT2          DSTT2
244200950519     C                   MOVE      WCTSM         DSCTS
244300940908     C                   MOVEL     DSMSG         $MSG
244400940908     C                   EXSR      SCRERR
244500940908     C                   ENDIF
244600940908     C                   ELSE
244700940908     C                   MOVEL     'S'           WFLTT
244800940908     C                   END
244900940908     C*
245000940908     C                   ENDSR
245100940803     C*------------------------------------------------------------------------*
245200940803     C*? CERCLI - CERCA LA TARIFFA PER CLIENTE
245300940803     C*------------------------------------------------------------------------*
245400940608     C     CERCLI        BEGSR
245500940608     C*
245600940630     C                   MOVEL     'K'           WTTV
245700940630     C                   Z-ADD     *ZEROS        WFT1
245800940630     C                   Z-ADD     *ZEROS        WFT2
245900940630     C                   Z-ADD     *ZEROS        WKSC
246000940701     C                   Z-ADD     *ZEROS        WCTR
246100940701     C                   MOVEL     *BLANKS       WCTS
246200131002     C                   MOVEL     *BLANKS       Wnaz
246300131002     C                   MOVEL     *BLANKS       wcad
246400940826     C     WRIF          IFEQ      'KK'
246500940829     C                   Z-ADD     W$KSC         WKSC
246600940701     C                   Z-ADD     D$DCTR        WCTR
246700940826     C                   ENDIF
246800940826     C     WRIF          IFEQ      'KM'
246900941201     C     D$DTPO        IFEQ      'F'
247000940829     C     W$CCM         IFNE      *ZEROS
247100940829     C                   Z-ADD     W$CCM         WKSC
247200940826     C                   ELSE
247300940829     C                   Z-ADD     W$KSC         WKSC
247400940826     C                   ENDIF
247500940826     C                   ENDIF
247600941201     C     D$DTPO        IFEQ      'A'
247700940829     C                   Z-ADD     W$CCM         WKSC
247800940826     C                   ENDIF
247900940701     C                   Z-ADD     D$DCTR        WCTR
248000940826     C                   ENDIF
248100940701     C*
248200940701     C* LANCIA GESTIONE TARIFFE
248300940902     C     WKSC          IFNE      DEPKSC
248400940902     C                   Z-ADD     WKSC          DEPKSC            7 0
248500940830     C                   EXSR      CHKKSC
248600940902     C                   ENDIF
248700940908     C     WKSCOK        IFEQ      'S'
248800941107     C                   EXSR      GESEVD
248900940908     C                   ENDIF
249000940608     C*
249100940719     C* SE TARIFFA BOLLA NON E' STATA TROVATA PROVO CON TARIFFA = 999 (TUTTE)
249200940719     C     WFLTT         IFEQ      'N'
249300940829     C                   Z-ADD     W$KSC         WKSC
249400940719     C                   Z-ADD     999           WCTR
249500940908     C     WKSCOK        IFEQ      'S'
249600940722     C                   EXSR      GESEVD
249700940908     C                   ENDIF
249800940830     C                   ENDIF
249900940719     C*
250000940608     C                   ENDSR
250100940803     C*------------------------------------------------------------------------*
250200940803     C*? CERCAP - CERCA LA TARIFFA PER CAP
250300940803     C*------------------------------------------------------------------------*
250400940701     C     CERCAP        BEGSR
250500940701     C*
250600940701     C                   MOVEL     'P'           WTTV
250700940701     C                   Z-ADD     *ZEROS        WFT1
250800940701     C                   Z-ADD     *ZEROS        WFT2
250900940701     C                   Z-ADD     *ZEROS        WKSC
251000940701     C                   Z-ADD     *ZEROS        WCTR
251100940701     C                   MOVEL     *BLANKS       WCTS
251200131002     C                   MOVEL     *BLANKS       wcad
251300131002     C                   MOVEL     *BLANKS       Wnaz
251400131002     c* cap completo
251500940701     C     WRIF          IFEQ      'PA'
251600131002     C                   MOVEl     D$Dnzd        Wnaz
251700131002     C                   MOVEl     D$DCAD        wcad
251800940701     C* LANCIA GESTIONE TARIFFE
251900940722     C                   EXSR      GESEVD
252000131002     C                   ENDIF
252100131002     c* cap fittizio
252200131002     C                   if        wfltt = 'N' and
252300131002     c                             (wrif = 'PA' or wrif = 'PP')
252400131002     C                   MOVEl     D$Dnzd        Wnaz
252500131002     c     wnaz          lookup    d15(i)                                 88
252600131002     c                   if        *in88
252700131002     C                   eval      wcad = %subst(D$DCAD: pcf(i): ecf(i))
252800131002     C                   ENDIF
252900131002     C                   EXSR      GESEVD
253000131002     C                   ENDIF
253100940701     C*
253200940701     C                   ENDSR
253300940803     C*------------------------------------------------------------------------*
253400940803     C*? CERTAS - CERCA LA TARIFFA PER CODICE TASSAZIONE
253500940803     C*------------------------------------------------------------------------*
253600940701     C     CERTAS        BEGSR
253700940701     C*
253800940701     C                   MOVEL     'T'           WTTV
253900940701     C                   Z-ADD     *ZEROS        WFT1
254000940701     C                   Z-ADD     *ZEROS        WFT2
254100940701     C                   Z-ADD     *ZEROS        WKSC
254200940701     C                   Z-ADD     *ZEROS        WCTR
254300940701     C                   MOVEL     *BLANKS       WCTS
254400131002     C                   MOVEL     *BLANKS       wcad
254500131002     C                   MOVEL     *BLANKS       Wnaz
254600940701     C     WRIF          IFEQ      'SP'
254700940829     C                   MOVEL     W$MCT         WCTS
254800940701     C                   ENDIF
254900940701     C     WRIF          IFEQ      'SA'
255000940721     C                   MOVEL     D$DCTS        WCTS
255100940701     C                   ENDIF
255200940701     C*
255300940701     C* LANCIA GESTIONE TARIFFE
255400940722     C                   EXSR      GESEVD
255500950519     C*
255600950519     C* CAMPI PER MESSAGGI
255700950519     C                   MOVE      WCTS          WCTSM
255800940701     C*
255900940701     C                   ENDSR
256000940803     C*------------------------------------------------------------------------*
256100940803     C*? CERFIL - CERCA LA TARIFFA PER FILIALE/TERMINAL
256200940803     C*------------------------------------------------------------------------*
256300940701     C     CERFIL        BEGSR
256400940701     C*
256500940701     C                   MOVEL     'F'           WTTV
256600940701     C                   Z-ADD     *ZEROS        WFT1
256700940701     C                   Z-ADD     *ZEROS        WFT2
256800940701     C                   Z-ADD     *ZEROS        WKSC
256900940701     C                   Z-ADD     *ZEROS        WCTR
257000940701     C                   MOVEL     *BLANKS       WCTS
257100131002     C                   MOVEL     *BLANKS       wcad
257200131002     C                   MOVEL     *BLANKS       Wnaz
257300940701     C     WRIF          IFEQ      'LP'
257400940829     C                   Z-ADD     W$LNP         WFT1
257500940701     C                   ENDIF
257600940701     C     WRIF          IFEQ      'LT'
257700940829     C                   Z-ADD     W$LNP         WFT1
257800940829     C                   Z-ADD     W$TFP         WFT2
257900940701     C                   ENDIF
258000940701     C     WRIF          IFEQ      'TP'
258100940829     C                   Z-ADD     W$TFP         WFT1
258200940701     C                   ENDIF
258300940701     C     WRIF          IFEQ      'TT'
258400940829     C                   Z-ADD     W$TFP         WFT1
258500940826     C                   Z-ADD     D$DTFA        WFT2
258600130930     c* per bolle legate: conti servizi (F7 e FC) impostiamo il terminal
258700130122     c* della bolla figlia e non dell'originale
258800130122     C     D$DFIG        IFEQ      'S'
258900130122     c     D$DRBL        andeq     'C'
259000130122     c     D$DDBO        andeq     'S'
259100130930     C     WDDSP         andgt     20181231
259200130122     c                   z-add     d$DTFP        WFT1
259300130122     c                   ENDIF
259400940701     C                   ENDIF
259500940701     C     WRIF          IFEQ      'T1'
259600040524     C*                  Z-ADD     D$DFLP        WFT1
259700040524     C                   Z-ADD     D$Dar5flp     WFT1
259800940701     C                   ENDIF
259900940701     C     WRIF          IFEQ      'T2'
260000040524     C*                  Z-ADD     D$DFL2        WFT1
260100040524     C                   Z-ADD     D$Dar5fl2     WFT1
260200940701     C                   ENDIF
260300040524     C     WRIF          IFEQ      'T3'
260400040524     C                   Z-ADD     D$Dar5fl3     WFT1
260500040524     C                   ENDIF
260600040524     C     WRIF          IFEQ      'T4'
260700040524     C                   Z-ADD     D$Dar5fl4     WFT1
260800040524     C                   ENDIF
260900040524     C     WRIF          IFEQ      'T5'
261000040524     C                   Z-ADD     D$Dar5fl5     WFT1
261100040524     C                   ENDIF
261200940701     C     WRIF          IFEQ      'TA'
261300940701     C                   Z-ADD     D$DTFA        WFT1
261400940701     C                   ENDIF
261500940701     C     WRIF          IFEQ      'TL'
261600940701     C                   Z-ADD     D$DTFA        WFT1
261700111020     C                   Z-ADD     W$lna         WFT2
261800940701     C                   ENDIF
261900940701     C     WRIF          IFEQ      'LA'
262000111020     C                   Z-ADD     W$lna         WFT1
262100940701     C                   ENDIF
262200940902     C*
262300940902     C* LANCIA GESTIONE TARIFFE
262400940902     C                   MOVEL     D$DKSC        WFILX             3
262500940902     C     WFILX         IFNE      DEPFIL
262600940902     C                   MOVE      WFILX         DEPFIL            3
262700940902     C                   MOVE      WFILX         WFIL              3 0
262800940902     C                   EXSR      CARFIL
262900940902     C                   ENDIF
263000940722     C                   EXSR      GESEVD
263100940701     C*
263200940908     C* SE BOLLA FIGLIA E TARIFFA TERMINAL/TERMINAL E TARIFFA NON TROVATA
263300940908     C* ----> NON EMETTO MESSAGGIO 'TARIFFA NON TROVATA'
263400950905     C     D$DFIG        IFEQ      'S'
263500940908     C     WRIF          ANDEQ     'TT'
263600940908     C     WFLTT         ANDEQ     'N'
263700940908     C                   MOVEL     'N'           WMSGMT
263800940908     C                   ENDIF
263900940908     C*
264000940908     C* CAMPI PER MESSAGGI
264100940830     C                   MOVE      WFT1          WTT1
264200940830     C                   MOVE      WFT2          WTT2
264300940830     C*
264400940701     C                   ENDSR
264500940803     C*------------------------------------------------------------------------*
264600940803     C*? CERCAR - CERCA LA TARIFFA DI CARTELLO
264700940803     C*------------------------------------------------------------------------*
264800940701     C     CERCAR        BEGSR
264900940701     C*
265000940701     C                   MOVEL     'C'           WTTV
265100940701     C                   Z-ADD     *ZEROS        WFT1
265200940701     C                   Z-ADD     *ZEROS        WFT2
265300940701     C                   Z-ADD     *ZEROS        WKSC
265400940701     C                   Z-ADD     *ZEROS        WCTR
265500940701     C                   MOVEL     *BLANKS       WCTS
265600131002     C                   MOVEL     *BLANKS       wcad
265700131002     C                   MOVEL     *BLANKS       Wnaz
265800940701     C*
265900940701     C* LANCIA GESTIONE TARIFFE
266000940722     C                   EXSR      GESEVD
266100940701     C*
266200940701     C                   ENDSR
266300940704     C*------------------------------------------------------------------------*
266400940704     C*? GESEVD - GESTIONE FILE ECEVD (TARIFFE)
266500940704     C*------------------------------------------------------------------------*
266600940704     C     GESEVD        BEGSR
266700940801     C*
266800940801     C* SE TIPO SVILUPPO PESO TASSABILE 2=MIGLIORE REALE/TASS. O 3=MINORE REA/TA
266900940801     C* CERCO IL RAPP PESO/VOL PRIMA DI ARROTONDARE
267000961202     C     VSPT(V)       IFNE      *BLANKS
267100961202     C*
267200961202     C                   Z-ADD     *ZEROS        WQTAPT
267300940704     C*
267400940704     C* CERCA LO SCAGLIONE CON LA QTA DELLA BOLLA
267500940704     C                   EXSR      SETEVD
267600940704     C*
267700940707     C* SE IL RAPP. PESO/VOL MAGGIORE DI ZERO E VOLUME MAGGIORE DI ZERO
267800940707     C* CERCO SCAGLIONE CON PESO TASSABILE
267900940801     C     EVDRPV        IFNE      *ZEROS
268000941201     C     D$DVLF        ANDNE     *ZEROS
268100940707     C* CALCOLO PESO TASSABILE
268200940801     C                   EXSR      CALPT
268300020102     C                   Z-ADD     WQTA          WQTAPT           19 7
268400940801     C                   ENDIF
268500940801     C*
268600940801     C                   ENDIF
268700940707     C*
268800940803     C* CONTROLLA ARROTONDAMENTO
268900940803     C                   EXSR      ARROTO
269000940729     C*
269100940801     C* CERCA LO SCAGLIONE GIUSTO IN TARIFFA
269200940707     C                   EXSR      SETEVD
269300940704     C*
269400940704     C                   ENDSR
269500940701     C*------------------------------------------------------------------------*
269600940701     C*? SETEVD - POSIZIONAMENTO E PRIMA LETTURA FILE TARIFFE
269700940701     C*------------------------------------------------------------------------*
269800940701     C     SETEVD        BEGSR
269900940830     C*
270000940908     C                   MOVEL     'N'           WSETLL            1
270100940701     C                   MOVEL     '0'           FINEVD            1
270200940829     C*
270300940829     C* SE TARIFFA DI CARTELLO CONTROLLO PRIMA SE CARICATA IN MEMORIA
270400940829IF  1C     WTTV          IFEQ      'C'
270500940829     C                   EXSR      CHKCAR
270600940830E   1C                   ENDIF
270700940902     C*
270800940902     C* SE TARIFFA DI FILIALE CONTROLLO PRIMA SE CARICATA IN MEMORIA
270900940902IF  1C     WTTV          IFEQ      'F'
271000940902     C                   EXSR      CHKFIL
271100940902E   1C                   ENDIF
271200940831     C*
271300940831     C* SE TARIFFA CLIENTE CONTROLLO PRIMA SE CARICATA IN MEMORIA
271400940831IF  1C     WTTV          IFEQ      'K'
271500940831     C                   EXSR      CHKCLI
271600940831E   1C                   ENDIF
271700940829     C*
271800940907     C* SE TARIFFA NON TROVATA IN MEMORIA LEGGO DAL FILE
271900940908IF  1C     WSETLL        IFEQ      'N'
272000940829     C*
272100940701     C* POSIZIONAMENTO E PRIMA LETTURA
272200940701     C                   Z-ADD     WVOC          EVDVOC
272300940701     C                   Z-ADD     WPRG          EVDPRG
272400940701     C                   MOVEL     WTTV          EVDTTV
272500940701     C                   Z-ADD     WFT1          EVDFT1
272600940701     C                   Z-ADD     WFT2          EVDFT2
272700940701     C                   Z-ADD     WKSC          EVDCLI
272800940701     C                   Z-ADD     WCTR          EVDCTR
272900940701     C                   MOVEL     WCTS          EVDCTS
273000131002     C                   movel     Wnaz          EVDnaz
273100131002     C                   movel(p)  wcad          EVDcad
273200940802     C                   Z-ADD     *ZEROS        EVDDDT
273300940802     C                   Z-ADD     *ZEROS        EVDSGI
273400131002     C     KEYEVD        SETLL     ecevd06l                           99
273500940701     C*
273600940830IF  2C     *IN99         IFEQ      *ON
273700940701     C                   MOVEL     '1'           FINEVD
273800940830X   2C                   ELSE
273900940701     C                   EXSR      LETEVD
274000940830E   2C                   ENDIF
274100940701     C*
274200940720     C* CONTROLLO MANCA TARIFFA
274300940830IF  2C     WRECOK        IFEQ      'N'
274400991112     C                   MOVEL     'N'           WFLTT
274500940830X   2C                   ELSE
274600940701     C                   MOVEL     'S'           WFLTT
274700940830E   2C                   ENDIF
274800940829     C*
274900940830E   1C                   ENDIF
275000940701     C*
275100940701     C                   ENDSR
275200940701     C*------------------------------------------------------------------------*
275300940701     C*? LETEVD - LETTURA PROSSIMO RECORD TARIFFE
275400940830     C*------------------------------------------------------------------------*
275500940701     C     LETEVD        BEGSR
275600940701     C*
275700940802     C* LEGGO FINO A:
275800940701     C*     - FINE FILE O SUPERATO LIMITE FINALE
275900940701     C*     - TROVATO RECORD VALIDO
276000940701     C                   MOVEL     'N'           WRECOK
276100940701     C     FINEVD        DOWEQ     '0'
276200940701     C     WRECOK        ANDEQ     'N'
276300940701     C*
276400940701     C* LETTURE SUCCESSIVE ALLA PRIMA
276500131002     C                   READ      ecevd06l                               99
276600940701     C*
276700940701     C     *IN99         IFEQ      '1'
276800940701     C     EVDVOC        ORNE      WVOC
276900940701     C     EVDPRG        ORNE      WPRG
277000940701     C     EVDTTV        ORNE      WTTV
277100940701     C     EVDFT1        ORNE      WFT1
277200940701     C     EVDFT2        ORNE      WFT2
277300940721     C     EVDCLI        ORNE      WKSC
277400940701     C     EVDCTR        ORNE      WCTR
277500940701     C     EVDCTS        ORNE      WCTS
277600131002     C     EVDCAd        ORNE      wcad
277700131002     C     EVDnaz        ORNE      Wnaz
277800940720     C                   MOVEL     '1'           FINEVD
277900940701     C                   ELSE
278000940701     C                   EXSR      CHKEVD
278100940701     C                   END
278200940701     C*
278300940701     C                   END
278400940701     C*
278500940701     C                   ENDSR
278600940830     C*------------------------------------------------------------------------*
278700940701     C*? CHKEVD - CONTROLLA VALIDITA' DELLA TARIFFA
278800940830     C*------------------------------------------------------------------------*
278900940701     C     CHKEVD        BEGSR
279000940701     C*
279100940701     C                   MOVEL     'S'           WRECOK            1
279200940701     C* ESCLUDO SE ANNULLATO
279300940701     C     EVDATB        IFNE      *BLANKS
279400940701     C                   MOVEL     'N'           WRECOK
279500940701     C                   END
279600940701     C* ESCLUDO SE DATA SPEDIZIONE FUORI DALLA DECORR/SCAD.
279700940701     C     WDSP          IFLT      EVDDDT
279800940701     C     WDSP          ORGT      EVDDST
279900940701     C                   MOVEL     'N'           WRECOK
280000940701     C                   END
280100011227     C*
280200011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
280300011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
280400011227     C                   Z-ADD     EVDSGI        W$SGI
280500011227     C                   Z-ADD     EVDSGF        W$SGF
280600011227IF  1C     WRECOK        IFEQ      'S'
280700011227IF  2C                   IF        WTIV = '21' OR
280800011227     C                             WTIV = '22' OR
280900011227     C                             WTIV = '23' OR
281000011227     C                             WTIV = '25'
281100011227IF  3C     WDIVAC        IFNE      WDIVCE
281200011227IF  4C     EVDSGI        IFNE      *ZEROS
281300011227     C                   Z-ADD     EVDSGI        §IMI
281400011227     C                   MOVEL     WDIVAC        §DVI
281500011227     C                   EXSR      CNVEUR
281600011227     C                   Z-ADD     §IMO          W$SGI
281700011227E   4C                   ENDIF
281800011227IF  4C     EVDSGF        IFNE      *ZEROS
281900011227     C                   Z-ADD     EVDSGF        §IMI
282000011227     C                   MOVEL     WDIVAC        §DVI
282100011227     C                   EXSR      CNVEUR
282200011227     C                   Z-ADD     §IMO          W$SGF
282300011227E   4C                   ENDIF
282400011227E   3C                   ENDIF
282500011227E   2C                   ENDIF
282600011227E   1C                   ENDIF
282700011227     C*
282800940701     C* ESCLUDO SE QTA NON COMPRESA NELLO SCAGLIONE
282900000306IF  1C     WTIV          IFNE      *BLANKS
283000011227     C     WQTA          IFLT      W$SGI
283100011227     C     WQTA          ORGT      W$SGF
283200940701     C                   MOVEL     'N'           WRECOK
283300940701     C                   END
283400000306E   1C                   ENDIF
283500940701     C*
283600940701     C                   ENDSR
283700940830     C*------------------------------------------------------------------------*
283800940830     C*? CHKCAR - CONTROLLA SE TARIFFA DI CARTELLO CARICATA IN MEMORIA
283900940830     C*------------------------------------------------------------------------*
284000940830     C     CHKCAR        BEGSR
284100940830     C*
284200940830     C* CONTROLLA ESISTENZA TARIFFA DI CARTELLO IN LINEA FRA QUELLE CARICATE
284300940830     C                   Z-ADD     1             I
284400940830DO  1C     I             DOWLE     WULTCA
284500940908     C     WSETLL        ANDEQ     'N'
284600940830     C*
284700940830     C* CONTROLLI DI VALIDITA'
284800941213IF  2C     WVOC          IFEQ      CVOC(I)
284900941213     C*
285000941213IF  3C     WPRG          IFEQ      CPRG(I)
285100940830     C     WFT1          ANDEQ     CFT1(I)
285200940830     C     WFT2          ANDEQ     CFT2(I)
285300940830     C     WKSC          ANDEQ     CCLI(I)
285400940830     C     WCTR          ANDEQ     CCTR(I)
285500940830     C     WCTS          ANDEQ     CCTS(I)
285600131002     C     Wnaz          ANDEQ     Cnaz(I)
285700131002     C     wcad          ANDEQ     Ccad(I)
285800940830     C*
285900941213IF  4C     WDSP          IFGE      CDDT(I)
286000940830     C     WDSP          ANDLE     CDST(I)
286100000306     C*
286200011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
286300011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
286400011227     C                   Z-ADD     CSGI(I)       W$SGI
286500011227     C                   Z-ADD     CSGF(I)       W$SGF
286600011227IF  5C                   IF        WTIV = '21' OR
286700011227     C                             WTIV = '22' OR
286800011227     C                             WTIV = '23' OR
286900011227     C                             WTIV = '25'
287000011227IF  6C     WDIVAC        IFNE      WDIVCE
287100011227IF  7C     CSGI(I)       IFNE      *ZEROS
287200011227     C                   Z-ADD     CSGI(I)       §IMI
287300011227     C                   MOVEL     WDIVAC        §DVI
287400011227     C                   EXSR      CNVEUR
287500011227     C                   Z-ADD     §IMO          W$SGI
287600011227E   7C                   ENDIF
287700011227IF  7C     CSGF(I)       IFNE      *ZEROS
287800011227     C                   Z-ADD     CSGF(I)       §IMI
287900011227     C                   MOVEL     WDIVAC        §DVI
288000011227     C                   EXSR      CNVEUR
288100011227     C                   Z-ADD     §IMO          W$SGF
288200011227E   7C                   ENDIF
288300011227E   6C                   ENDIF
288400011227E   5C                   ENDIF
288500011227     c*
288600000306IF  5C     WTIV          IFEQ      *BLANKS
288700000306     C     WTIV          ORNE      *BLANKS
288800011227     C     WQTA          ANDGE     W$SGI
288900011227     C     WQTA          ANDLE     W$SGF
289000940830     C*
289100940830     C* CARTELLO TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
289200940908     C                   MOVEL     'S'           WSETLL
289300940830     C                   MOVEL     'S'           WFLTT
289400940830     C*
289500940830     C                   Z-ADD     CVOC(I)       EVDVOC
289600940830     C                   Z-ADD     CPRG(I)       EVDPRG
289700940830     C                   MOVEL     'C'           EVDTTV
289800940830     C                   Z-ADD     CFT1(I)       EVDFT1
289900940830     C                   Z-ADD     CFT2(I)       EVDFT2
290000940830     C                   Z-ADD     CCLI(I)       EVDCLI
290100940830     C                   Z-ADD     CCTR(I)       EVDCTR
290200940830     C                   MOVEL     CCTS(I)       EVDCTS
290300131002     C                   movel     Ccad(I)       EVDcad
290400131002     C                   movel     Cnaz(I)       EVDnaz
290500940830     C                   Z-ADD     CDDT(I)       EVDDDT
290600940830     C                   Z-ADD     CDST(I)       EVDDST
290700940830     C                   Z-ADD     CSGI(I)       EVDSGI
290800940830     C                   Z-ADD     CSGF(I)       EVDSGF
290900940830     C                   Z-ADD     CITR(I)       EVDITR
291000940830     C                   Z-ADD     CIMI(I)       EVDIMI
291100940830     C                   Z-ADD     CIMA(I)       EVDIMA
291200940830     C                   MOVEL     CSEG(I)       EVDSEG
291300940830     C                   Z-ADD     CPER(I)       EVDPER
291400940830     C                   Z-ADD     CRPV(I)       EVDRPV
291500940830     C*
291600941213E   5C                   ENDIF
291700940830E   4C                   ENDIF
291800940830E   3C                   ENDIF
291900940830E   2C                   ENDIF
292000940830     C*
292100940830     C                   ADD       1             I
292200940830E   1C                   ENDDO
292300940907     C*
292400940908     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
292500940908     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
292600940908IF  1C     WSETLL        IFEQ      'N'
292700940908     C     WULTCA        ANDLT     200
292800940908     C                   MOVEL     'S'           WSETLL
292900940908E   1C                   ENDIF
293000940907     C*
293100940907     C                   ENDSR
293200940902     C*------------------------------------------------------------------------*
293300940902     C*? CARFIL - CARICA TUTTE LE TARIFFE DELLA FILIALE
293400940902     C*------------------------------------------------------------------------*
293500940902     C     CARFIL        BEGSR
293600940902     C*
293700940902     C* POSIZIONAMENTO E PRIMA LETTURA
293800940902     C     WFIL          SETLL     ECEVD03L
293900940902     C                   READ      ECEVD03L                               99
294000940902     C*
294100940902     C* FINO A FINE FILE
294200940902     C                   Z-ADD     *ZEROS        I
294300940902DO  1C     *IN99         DOWEQ     *OFF
294400940902     C     EVDFT1        ANDEQ     WFIL
294500940902IF  3C     EVDATB        IFEQ      *BLANKS
294600940902     C                   ADD       1             I
294700940902     C                   Z-ADD     EVDVOC        FVOC(I)
294800940902     C                   Z-ADD     EVDPRG        FPRG(I)
294900940902     C                   Z-ADD     EVDFT1        FFT1(I)
295000940902     C                   Z-ADD     EVDFT2        FFT2(I)
295100940902     C                   Z-ADD     EVDCLI        FCLI(I)
295200940902     C                   Z-ADD     EVDCTR        FCTR(I)
295300940902     C                   MOVEL     EVDCTS        FCTS(I)
295400131002     C                   movel     EVDcad        Fcad(I)
295500131002     C                   movel     EVDnaz        Fnaz(I)
295600940902     C                   Z-ADD     EVDDDT        FDDT(I)
295700940902     C                   Z-ADD     EVDDST        FDST(I)
295800940902     C                   Z-ADD     EVDSGI        FSGI(I)
295900940902     C                   Z-ADD     EVDSGF        FSGF(I)
296000940902     C                   Z-ADD     EVDITR        FITR(I)
296100940902     C                   Z-ADD     EVDIMI        FIMI(I)
296200940902     C                   Z-ADD     EVDIMA        FIMA(I)
296300940902     C                   MOVEL     EVDSEG        FSEG(I)
296400940902     C                   Z-ADD     EVDPER        FPER(I)
296500940902     C                   Z-ADD     EVDRPV        FRPV(I)
296600940902E   2C                   ENDIF
296700940902     C*
296800940902     C                   READ      ECEVD03L                               99
296900940902E   1C                   ENDDO
297000940902     C*
297100940902     C                   Z-ADD     I             WULTTF            6 0
297200940902     C*
297300940902     C*
297400940902     C                   ENDSR
297500940902     C*------------------------------------------------------------------------*
297600941213     C*? CHKFIL - CONTROLLA SE TARIFFA FILIALE GIA' IN MEMORIA
297700940902     C*------------------------------------------------------------------------*
297800940902     C     CHKFIL        BEGSR
297900940902     C*
298000941213     C* CONTROLLA ESISTENZA TARIFFA FILIALE FRA QUELLE CARICATE
298100940902     C                   Z-ADD     1             I
298200940908DO  1C     I             DOWLE     WULTTF
298300940908     C     WSETLL        ANDEQ     'N'
298400940902     C*
298500940902     C* CONTROLLI DI VALIDITA'
298600941213IF  2C     WVOC          IFEQ      FVOC(I)
298700941213     C*
298800941213IF  3C     WPRG          IFEQ      FPRG(I)
298900940902     C     WFT1          ANDEQ     FFT1(I)
299000940902     C     WFT2          ANDEQ     FFT2(I)
299100940902     C     WKSC          ANDEQ     FCLI(I)
299200940902     C     WCTR          ANDEQ     FCTR(I)
299300940902     C     WCTS          ANDEQ     FCTS(I)
299400131002     C     Wnaz          ANDEQ     Fnaz(I)
299500131002     C     wcad          ANDEQ     Fcad(I)
299600940902     C*
299700941213IF  4C     WDSP          IFGE      FDDT(I)
299800940902     C     WDSP          ANDLE     FDST(I)
299900011227     C*
300000011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
300100011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
300200011227     C                   Z-ADD     FSGI(I)       W$SGI
300300011227     C                   Z-ADD     FSGF(I)       W$SGF
300400011227IF  5C                   IF        WTIV = '21' OR
300500011227     C                             WTIV = '22' OR
300600011227     C                             WTIV = '23' OR
300700011227     C                             WTIV = '25'
300800011227IF  6C     WDIVAC        IFNE      WDIVCE
300900011227IF  7C     FSGI(I)       IFNE      *ZEROS
301000011227     C                   Z-ADD     FSGI(I)       §IMI
301100011227     C                   MOVEL     WDIVAC        §DVI
301200011227     C                   EXSR      CNVEUR
301300011227     C                   Z-ADD     §IMO          W$SGI
301400011227E   7C                   ENDIF
301500011227IF  7C     FSGF(I)       IFNE      *ZEROS
301600011227     C                   Z-ADD     FSGF(I)       §IMI
301700011227     C                   MOVEL     WDIVAC        §DVI
301800011227     C                   EXSR      CNVEUR
301900011227     C                   Z-ADD     §IMO          W$SGF
302000011227E   7C                   ENDIF
302100011227E   6C                   ENDIF
302200011227E   5C                   ENDIF
302300000306     C*
302400000306IF  5C     WTIV          IFEQ      *BLANKS
302500000306     C     WTIV          ORNE      *BLANKS
302600011227     C     WQTA          ANDGE     W$SGI
302700011227     C     WQTA          ANDLE     W$SGF
302800940902     C*
302900940902     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
303000940908     C                   MOVEL     'S'           WSETLL
303100940902     C                   MOVEL     'S'           WFLTT
303200940902     C*
303300940902     C                   Z-ADD     FVOC(I)       EVDVOC
303400940902     C                   Z-ADD     FPRG(I)       EVDPRG
303500940902     C                   MOVEL     'F'           EVDTTV
303600940902     C                   Z-ADD     FFT1(I)       EVDFT1
303700940902     C                   Z-ADD     FFT2(I)       EVDFT2
303800940902     C                   Z-ADD     FCLI(I)       EVDCLI
303900940902     C                   Z-ADD     FCTR(I)       EVDCTR
304000940902     C                   MOVEL     FCTS(I)       EVDCTS
304100131002     C                   movel     Fcad(I)       EVDcad
304200131002     C                   movel     Fnaz(I)       EVDnaz
304300940902     C                   Z-ADD     FDDT(I)       EVDDDT
304400940902     C                   Z-ADD     FDST(I)       EVDDST
304500940902     C                   Z-ADD     FSGI(I)       EVDSGI
304600940902     C                   Z-ADD     FSGF(I)       EVDSGF
304700940902     C                   Z-ADD     FITR(I)       EVDITR
304800940902     C                   Z-ADD     FIMI(I)       EVDIMI
304900940902     C                   Z-ADD     FIMA(I)       EVDIMA
305000940902     C                   MOVEL     FSEG(I)       EVDSEG
305100940902     C                   Z-ADD     FPER(I)       EVDPER
305200940902     C                   Z-ADD     FRPV(I)       EVDRPV
305300940902     C*
305400941213E   5C                   ENDIF
305500940902E   4C                   ENDIF
305600940902E   3C                   ENDIF
305700940902E   2C                   ENDIF
305800940902     C*
305900940902     C                   ADD       1             I
306000940902E   1C                   ENDDO
306100940902     C*
306200940902     C                   ENDSR
306300940907     C*------------------------------------------------------------------------*
306400940907     C*? CARCLI - CARICA LE TARIFFE DI UN CLIENTE
306500940907     C*------------------------------------------------------------------------*
306600940907     C     CARCLI        BEGSR
306700940907     C*
306800940907     C* POSIZIONAMENTO E PRIMA LETTURA
306900940907     C     WKSC          SETLL     ECEVD02L
307000940907     C                   READ      ECEVD02L                               99
307100940907     C*
307200940907     C* FINO A FINE FILE
307300940907     C                   Z-ADD     *ZEROS        I
307400940907DO  1C     *IN99         DOWEQ     *OFF
307500940907IF  2C     EVDCLI        ANDEQ     WKSC
307600940907     C     I             ANDLT     200
307700940907     C     EVDATB        IFEQ      *BLANKS
307800940907     C                   ADD       1             I
307900940907     C                   Z-ADD     EVDVOC        KVOC(I)
308000940907     C                   Z-ADD     EVDPRG        KPRG(I)
308100940907     C                   Z-ADD     EVDFT1        KFT1(I)
308200940907     C                   Z-ADD     EVDFT2        KFT2(I)
308300940907     C                   Z-ADD     EVDCLI        KCLI(I)
308400940907     C                   Z-ADD     EVDCTR        KCTR(I)
308500940907     C                   MOVEL     EVDCTS        KCTS(I)
308600131002     C                   movel     EVDcad        Kcad(I)
308700131002     C                   movel     EVDnaz        Knaz(I)
308800940907     C                   Z-ADD     EVDDDT        KDDT(I)
308900940907     C                   Z-ADD     EVDDST        KDST(I)
309000940907     C                   Z-ADD     EVDSGI        KSGI(I)
309100940907     C                   Z-ADD     EVDSGF        KSGF(I)
309200940907     C                   Z-ADD     EVDITR        KITR(I)
309300940907     C                   Z-ADD     EVDIMI        KIMI(I)
309400940907     C                   Z-ADD     EVDIMA        KIMA(I)
309500940907     C                   MOVEL     EVDSEG        KSEG(I)
309600940907     C                   Z-ADD     EVDPER        KPER(I)
309700940907     C                   Z-ADD     EVDRPV        KRPV(I)
309800940907E   2C                   ENDIF
309900940907     C*
310000940907     C                   READ      ECEVD02L                               99
310100940907E   1C                   ENDDO
310200940907     C*
310300940907     C                   Z-ADD     I             WULTTK            6 0
310400940907     C*
310500940907     C                   ENDSR
310600940831     C*------------------------------------------------------------------------*
310700940902     C*? CHKCLI - CONTROLLA SE TARIFFA CLIENTE GIA' IN MEMORIA
310800940831     C*------------------------------------------------------------------------*
310900940831     C     CHKCLI        BEGSR
311000940831     C*
311100940831     C* CONTROLLA ESISTENZA TARIFFA CLIENTE FRA QUELLE CARICATE
311200940831     C                   Z-ADD     1             I
311300940831DO  1C     I             DOWLE     WULTTK
311400940908     C     WSETLL        ANDEQ     'N'
311500940831     C*
311600940831     C* CONTROLLI DI VALIDITA'
311700940831IF  2C     WVOC          IFEQ      KVOC(I)
311800941213     C*
311900941213IF  3C     WPRG          IFEQ      KPRG(I)
312000940831     C     WFT1          ANDEQ     KFT1(I)
312100940831     C     WFT2          ANDEQ     KFT2(I)
312200940831     C     WKSC          ANDEQ     KCLI(I)
312300940831     C     WCTR          ANDEQ     KCTR(I)
312400940831     C     WCTS          ANDEQ     KCTS(I)
312500131002     C     Wnaz          ANDEQ     Knaz(I)
312600131002     C     wcad          ANDEQ     Kcad(I)
312700940831     C*
312800941213IF  4C     WDSP          IFGE      KDDT(I)
312900940831     C     WDSP          ANDLE     KDST(I)
313000011227     C*
313100011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
313200011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
313300011227     C                   Z-ADD     KSGI(I)       W$SGI
313400011227     C                   Z-ADD     KSGF(I)       W$SGF
313500011227IF  5C                   IF        WTIV = '21' OR
313600011227     C                             WTIV = '22' OR
313700011227     C                             WTIV = '23' OR
313800011227     C                             WTIV = '25'
313900011227IF  6C     WDIVAC        IFNE      WDIVCE
314000011227IF  7C     KSGI(I)       IFNE      *ZEROS
314100011227     C                   Z-ADD     KSGI(I)       §IMI
314200011227     C                   MOVEL     WDIVAC        §DVI
314300011227     C                   EXSR      CNVEUR
314400011227     C                   Z-ADD     §IMO          W$SGI
314500011227E   7C                   ENDIF
314600011227IF  7C     KSGF(I)       IFNE      *ZEROS
314700011227     C                   Z-ADD     KSGF(I)       §IMI
314800011227     C                   MOVEL     WDIVAC        §DVI
314900011227     C                   EXSR      CNVEUR
315000011227     C                   Z-ADD     §IMO          W$SGF
315100011227E   7C                   ENDIF
315200011227E   6C                   ENDIF
315300011227E   5C                   ENDIF
315400940831     C*
315500000306IF  5C     WTIV          IFEQ      *BLANKS
315600000306     C     WTIV          ORNE      *BLANKS
315700011227     C     WQTA          ANDGE     W$SGI
315800011227     C     WQTA          ANDLE     W$SGF
315900940831     C*
316000940902     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
316100940908     C                   MOVEL     'S'           WSETLL
316200940831     C                   MOVEL     'S'           WFLTT
316300940831     C*
316400940831     C                   Z-ADD     KVOC(I)       EVDVOC
316500940831     C                   Z-ADD     KPRG(I)       EVDPRG
316600940831     C                   MOVEL     'K'           EVDTTV
316700940831     C                   Z-ADD     KFT1(I)       EVDFT1
316800940831     C                   Z-ADD     KFT2(I)       EVDFT2
316900940831     C                   Z-ADD     KCLI(I)       EVDCLI
317000940831     C                   Z-ADD     KCTR(I)       EVDCTR
317100940831     C                   MOVEL     KCTS(I)       EVDCTS
317200131002     C                   movel     Kcad(I)       EVDcad
317300131002     C                   movel     Knaz(I)       EVDnaz
317400940831     C                   Z-ADD     KDDT(I)       EVDDDT
317500940831     C                   Z-ADD     KDST(I)       EVDDST
317600940831     C                   Z-ADD     KSGI(I)       EVDSGI
317700940831     C                   Z-ADD     KSGF(I)       EVDSGF
317800940831     C                   Z-ADD     KITR(I)       EVDITR
317900940831     C                   Z-ADD     KIMI(I)       EVDIMI
318000940831     C                   Z-ADD     KIMA(I)       EVDIMA
318100940831     C                   MOVEL     KSEG(I)       EVDSEG
318200940831     C                   Z-ADD     KPER(I)       EVDPER
318300940831     C                   Z-ADD     KRPV(I)       EVDRPV
318400940831     C*
318500941213E   5C                   ENDIF
318600941213E   4C                   ENDIF
318700940831E   3C                   ENDIF
318800940831E   2C                   ENDIF
318900940831     C*
319000940831     C                   ADD       1             I
319100940908E   1C                   ENDDO
319200940908     C*
319300940908     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
319400940908     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
319500940908IF  1C     WSETLL        IFEQ      'N'
319600940908     C     WULTTK        ANDLT     200
319700940908     C                   MOVEL     'S'           WSETLL
319800940908E   1C                   ENDIF
319900940831     C*
320000940831     C                   ENDSR
320100940830     C*------------------------------------------------------------------------*
320200940830     C*? CHKKSC - CONTROLLA SE IL CLIENTE E' TRA QUELLI CON TARIFFA PARTICOLARE
320300940830     C*------------------------------------------------------------------------*
320400940830     C     CHKKSC        BEGSR
320500940830     C*
320600940831     C* CONTROLLA ESISTENZA CLIENTE TRA QUELLI CON TARIFFA PARTICOLARE
320700940830     C                   MOVEL     'N'           WKSCOK            1
320800940902     C                   Z-ADD     1             IK                9 0
320900940902DO  1C     IK            DOWLE     WULTKS
321000940907     C     WKSCOK        ANDEQ     'N'
321100940830     C*
321200940907     C* CONTROLLI DI VALIDITA' (CLIENTE FRA QUELLI CARICATI)
321300940902IF  2C     WKSC          IFEQ      KSC(IK)
321400940830     C                   MOVEL     'S'           WKSCOK
321500940831     C* CARICA TUTTE LE TARIFFE DEL CLIENTE
321600940831     C                   EXSR      CARCLI
321700940830E   2C                   ENDIF
321800940830     C*
321900940902     C                   ADD       1             IK
322000940830E   1C                   ENDDO
322100940908     C*
322200940830     C                   ENDSR
322300011221     C*------------------------------------------------------------------------*
322400011221     C*?CNVEUR - CONVERTE I CAMPI DA DIVISA ESTERA A LIRE
322500011221     C*------------------------------------------------------------------------*
322600011221     C     CNVEUR        BEGSR
322700011221     C*
322800011221     C* AZZERA VARIABILI DI LAVORO
322900011221     C                   Z-ADD     *ZEROS        §IMO
323000011221     C*
323100011221     C                   CLEAR                   YEUR
323200011221     C                   MOVEL     §DVI          YECDVI
323300011221     C                   Z-ADD     §IMI          YECIMI
323400011221     C                   MOVEL     WDIVCE        YECDVO
323500011221     C                   MOVEL     '3'           YECDCO
323600011221     C                   MOVEL     'H'           YECTAR
323700011221     C                   CALL      'YEURCO'
323800011221     C                   PARM                    YEUR
323900011221IF  1C     YECESI        IFEQ      ' '
324000011221     C                   Z-ADD     YECIMO        §IMO
324100011221X   1C                   ELSE
324200011221     C                   Z-ADD     §IMI          §IMO
324300011221E   1C                   ENDIF
324400011221     C*
324500011221     C                   ENDSR
324600940907     C*------------------------------------------------------------------------*
324700940907     C*? CARCAR - CARICA LE TARIFFE DI CARTELLO
324800940907     C*------------------------------------------------------------------------*
324900940907     C     CARCAR        BEGSR
325000940907     C*
325100940907     C* POSIZIONAMENTO E PRIMA LETTURA
325200940907     C                   MOVEL     'C'           EVDTTV
325300940907     C     EVDTTV        SETLL     ECEVD04L
325400940907     C                   READ      ECEVD04L                               99
325500940907     C*
325600940907     C* FINO A FINE FILE E TARIFFE DI CARTELLO
325700940907     C                   Z-ADD     *ZEROS        I
325800940907DO  1C     *IN99         DOWEQ     *OFF
325900940907     C     EVDTTV        ANDEQ     'C'
326000940907     C     I             ANDLT     200
326100940907IF  2C     EVDATB        IFEQ      *BLANKS
326200940907     C                   ADD       1             I
326300940907     C                   Z-ADD     EVDVOC        CVOC(I)
326400940907     C                   Z-ADD     EVDPRG        CPRG(I)
326500940907     C                   Z-ADD     EVDFT1        CFT1(I)
326600940907     C                   Z-ADD     EVDFT2        CFT2(I)
326700940907     C                   Z-ADD     EVDCLI        CCLI(I)
326800940907     C                   Z-ADD     EVDCTR        CCTR(I)
326900940907     C                   MOVEL     EVDCTS        CCTS(I)
327000131002     C                   movel     EVDcad        Ccad(I)
327100131002     C                   movel     EVDnaz        Cnaz(I)
327200940907     C                   Z-ADD     EVDDDT        CDDT(I)
327300940907     C                   Z-ADD     EVDDST        CDST(I)
327400940907     C                   Z-ADD     EVDSGI        CSGI(I)
327500940907     C                   Z-ADD     EVDSGF        CSGF(I)
327600940907     C                   Z-ADD     EVDITR        CITR(I)
327700940907     C                   Z-ADD     EVDIMI        CIMI(I)
327800940907     C                   Z-ADD     EVDIMA        CIMA(I)
327900940907     C                   MOVEL     EVDSEG        CSEG(I)
328000940907     C                   Z-ADD     EVDPER        CPER(I)
328100940907     C                   Z-ADD     EVDRPV        CRPV(I)
328200940907E   2C                   ENDIF
328300940907     C*
328400940907     C                   READ      ECEVD04L                               99
328500940907E   1C                   ENDDO
328600940907     C*
328700940907     C                   Z-ADD     I             WULTCA            6 0
328800940907     C*
328900940907     C                   ENDSR
329000940907     C*------------------------------------------------------------------------*
329100940907     C*? CARKSC - CARICA I CLIENTI CON TARIFFE PARTICOLARI
329200940907     C*------------------------------------------------------------------------*
329300940907     C     CARKSC        BEGSR
329400940907     C*
329500940907     C* POSIZIONAMENTO E PRIMA LETTURA
329600940907     C                   MOVEL     'K'           EVDTTV
329700940907     C     EVDTTV        SETLL     ECEVD04L
329800940907     C                   READ      ECEVD04L                               99
329900940907     C*
330000940907     C* FINO A FINE FILE E TARIFFE DI CLIENTE
330100940907     C                   Z-ADD     *ZEROS        Y                 6 0
330200940907DO  1C     *IN99         DOWEQ     *OFF
330300940907     C     EVDTTV        ANDEQ     'K'
330400070626     C     Y             ANDLT     5000
330500940907IF  2C     EVDATB        IFEQ      *BLANKS
330600940907     C                   MOVEL     'S'           WFKSC             1
330700940907IF  3C     Y             IFGT      *ZEROS
330800940907     C                   Z-ADD     *ZEROS        YY                6 0
330900940907DO  4C                   DO        Y             YY
331000940907IF  5C     EVDCLI        IFEQ      KSC(YY)
331100940907     C                   MOVEL     'N'           WFKSC
331200940907E   5C                   ENDIF
331300940907E   4C                   ENDDO
331400940907E   3C                   ENDIF
331500940907IF  3C     WFKSC         IFEQ      'S'
331600940907     C                   ADD       1             Y
331700940907     C                   Z-ADD     EVDCLI        KSC(Y)
331800940907E   3C                   ENDIF
331900940907E   2C                   ENDIF
332000940907     C*
332100940907     C                   READ      ECEVD04L                               99
332200940907E   1C                   ENDDO
332300940907     C*
332400940907     C                   Z-ADD     Y             WULTKS            6 0
332500940907     C*
332600940907     C                   ENDSR
332700940907     C*------------------------------------------------------------------------*
332800940907     C*? CARMAG - CARICA LE MAGGIORAZIONI
332900940907     C*------------------------------------------------------------------------*
333000940907     C     CARMAG        BEGSR
333100940907     C*
333200940907     C* POSIZIONAMENTO E PRIMA LETTURA
333300940907     C     *LOVAL        SETLL     ECEVM01L
333400940907     C                   READ      ECEVM01L                               99
333500940907     C*
333600940907     C* FINO A FINE FILE E MAGGIORAZIONI
333700940907     C                   Z-ADD     *ZEROS        I
333800940907DO  1C     *IN99         DOWEQ     *OFF
333900041118     C     I             ANDLT     600
334000940907IF  2C     EVMATB        IFEQ      *BLANKS
334100940907     C                   ADD       1             I
334200940907     C                   Z-ADD     EVMVOM        MVOM(I)
334300940907     C                   Z-ADD     EVMPRM        MPRM(I)
334400940907     C                   Z-ADD     EVMVOC        MVOC(I)
334500940907     C                   Z-ADD     EVMPRG        MPRG(I)
334600100408     C                   Z-ADD     EVMviso       Mviso(I)
334700100408     C                   Z-ADD     EVMddt        Mddti(I)
334800100408     C                   Z-ADD     EVMdst        Mdsti(I)
334900100408     C                   movel     EVMapp        Mapp(I)
335000940907E   2C                   ENDIF
335100940907     C*
335200940907     C                   READ      ECEVM01L                               99
335300940907E   1C                   ENDDO
335400940907     C*
335500940907     C                   Z-ADD     I             WULTMA            6 0
335600940907     C*
335700940907     C                   ENDSR
335800940803     C*------------------------------------------------------------------------*
335900940803     C*? CARTAB - CARICA LE TABELLE
336000940803     C*------------------------------------------------------------------------*
336100930414     C     CARTAB        BEGSR
336200011221     C*
336300011221     C* REPERISCE LA DATA CORRENTE
336400011221     C                   TIME                    N14              14 0
336500011221     C                   MOVE      N14           N8                8 0
336600011221     C                   Z-ADD     N8            G08DAT
336700011221     C                   Z-ADD     *ZEROS        G08INV
336800011221     C                   MOVEL     '0'           G08ERR
336900011221     C                   CALL      'XSRDA8'
337000011221     C                   PARM                    WLBDA8
337100011221     C                   Z-ADD     G08INV        DATCOR            8 0
337200941024     C*
337300941024     C                   OPEN      TABEL00F
337400011221     C                   OPEN      TNTBE01L
337500020805     C                   OPEN      AZORG01L
337600940704     C*
337700940704     C* CARICA TABELLA -2R- RIFERIMENTO VOCE
337800940913     C     $ERR          IFEQ      'N'
337900940704     C                   Z-ADD     0             I
338000940704     C                   Z-ADD     CODUT         KTBUTE
338100940704     C                   MOVEL     '2R'          KTBCOD
338200940704     C                   MOVEL     *BLANKS       KTBKEY
338300940704     C     KEYTAB        SETLL     TABEL00F                           99
338400940704     C  N99              READ      TABEL00F                               99
338500940704     C     *IN99         IFEQ      '1'
338600940720     C                   MOVEL     MSG(3)        $MSG
338700940907     C                   EXSR      SCRERR
338800940704     C                   END
338900940704     C     *IN99         DOWEQ     '0'
339000940704     C     TBLCOD        ANDEQ     '2R'
339100940704     C     TBLFLG        IFEQ      *BLANKS
339200940704     C                   ADD       1             I
339300940704     C                   MOVEL     TBLKEY        C2R(I)
339400940707     C                   MOVEL     TBLUNI        DS2R
339500940707     C                   MOVEL     §2RTVA        T2R(I)
339600940704     C                   END
339700940704     C                   READ      TABEL00F                               99
339800940704     C                   END
339900940704     C                   END
340000940720     C*
340100940720     C* CARICA TABELLA -FA- CONDIZIONI DI PAGAMENTO
340200940913     C     $ERR          IFEQ      'N'
340300940720     C                   Z-ADD     0             I
340400940720     C                   Z-ADD     CODUT         KTBUTE
340500940720     C                   MOVEL     'FA'          KTBCOD
340600940720     C                   MOVEL     *BLANKS       KTBKEY
340700940720     C     KEYTAB        SETLL     TABEL00F                           99
340800940720     C  N99              READ      TABEL00F                               99
340900940720     C     *IN99         IFEQ      '1'
341000940720     C                   MOVEL     MSG(2)        $MSG
341100940907     C                   EXSR      SCRERR
341200940720     C                   END
341300940720     C     *IN99         DOWEQ     '0'
341400940720     C     TBLCOD        ANDEQ     'FA'
341500940720     C     TBLFLG        IFEQ      *BLANKS
341600940720     C                   ADD       1             I
341700940720     C                   MOVEL     TBLKEY        CFA(I)
341800940721     C                   MOVEL     TBLUNI        DSFA
341900940720     C                   MOVEL     §FARAT        WTM03             3
342000940720     C                   MOVE      WTM03         WTM030            3 0
342100940720     C     WTM030        IFLT      15
342200940720     C     WTM030        MULT      30            GFA(I)
342300940720     C                   ELSE
342400940720     C                   Z-ADD     WTM030        GFA(I)
342500940720     C                   END
342600940720     C                   END
342700940720     C                   READ      TABEL00F                               99
342800940720     C                   END
342900940720     C                   END
343000000626     C*
343100000626     C* CARICA TABELLA -7O-  CONSEGNE ANOMALE
343200000626IF  1C     $ERR          IFEQ      'N'
343300000626     C                   Z-ADD     0             I
343400000626     C                   Z-ADD     CODUT         KTBUTE
343500000626     C                   MOVEL     '7O'          KTBCOD
343600000626     C                   MOVEL     *BLANKS       KTBKEY
343700000626     C     KEYTAB        SETLL     TABEL00F                           99
343800000626     C  N99              READ      TABEL00F                               99
343900000626IF  2C     *IN99         IFEQ      '1'
344000000626     C                   MOVEL     MSG(1)        $MSG
344100000626     C                   EXSR      SCRERR
344200000626E   2C                   ENDIF
344300000626DO  2C     *IN99         DOWEQ     '0'
344400000626     C     TBLCOD        ANDEQ     '7O'
344500000626IF  3C     TBLFLG        IFEQ      *BLANKS
344600000626     C                   ADD       1             I
344700000626     C                   MOVEL     TBLKEY        CCA(I)
344800000626     C                   MOVEL     TBLUNI        DS7O
344900000626     C                   MOVEL     §7OFPT        CFPT(I)
345000000626     C                   MOVEL     §7ORMS        CRMS(I)
345100000626E   3C                   ENDIF
345200000626     C                   READ      TABEL00F                               99
345300000626E   2C                   ENDDO
345400000626E   1C                   ENDIF
345500010420     C*
345600010420     C* CARICA TABELLA -1B-  CODICI TRATTAMENTO MERCE
345700010420IF  1C     $ERR          IFEQ      'N'
345800010420     C                   Z-ADD     0             I
345900010420     C                   Z-ADD     CODUT         KTBUTE
346000010420     C                   MOVEL     '1B'          KTBCOD
346100010420     C                   MOVEL     *BLANKS       KTBKEY
346200010420     C     KEYTAB        SETLL     TABEL00F                           99
346300010420     C  N99              READ      TABEL00F                               99
346400010420IF  2C     *IN99         IFEQ      '1'
346500010801     C                   MOVEL     MSG(28)       $MSG
346600010420     C                   EXSR      SCRERR
346700010420E   2C                   ENDIF
346800010420DO  2C     *IN99         DOWEQ     '0'
346900010420     C     TBLCOD        ANDEQ     '1B'
347000010420IF  3C     TBLFLG        IFEQ      *BLANKS
347100010420     C                   ADD       1             I
347200010420     C                   MOVEL     TBLKEY        CTM(I)
347300010420     C                   MOVEL     TBLUNI        DS1B
347400010420     C                   MOVEL     §1BF12        CF12(I)
347500050801     C                   MOVEL     §1BF16        CF16(I)
347600030530     C                   MOVEL     §1BDKC        CDKC(I)
347700010420E   3C                   ENDIF
347800010420     C                   READ      TABEL00F                               99
347900010420E   2C                   ENDDO
348000010420E   1C                   ENDIF
348100131002     C*
348200131002     C* CARICA TABELLA -15-  nazioni per reperire cap fittizio
348300131002IF  1C     $ERR          IFEQ      'N'
348400131002     C                   Z-ADD     0             I
348500131002     C                   Z-ADD     CODUT         KTBUTE
348600131002     C                   MOVEL     '15'          KTBCOD
348700131002     C                   MOVEL     *BLANKS       KTBKEY
348800131002     C     KEYTAB        SETLL     TABEL00F                           99
348900131002     C  N99              READ      TABEL00F                               99
349000131002IF  2C     *IN99         IFEQ      '1'
349100131002     C                   MOVEL     MSG(28)       $MSG
349200131002     C                   EXSR      SCRERR
349300131002E   2C                   ENDIF
349400131002DO  2C     *IN99         DOWEQ     '0'
349500131002     C     TBLCOD        ANDEQ     '15'
349600131002IF  3C     TBLFLG        IFEQ      *BLANKS
349700131002     C                   ADD       1             I
349800131002     C                   MOVEL     TBLKEY        d15(I)
349900131002     C                   MOVEL     TBLUNI        DS15
350000131002     C                   MOVEL     §15pcf        pcf(I)
350100131002     C                   MOVEL     §15ecf        ecf(I)
350200131002E   3C                   ENDIF
350300131002     C                   READ      TABEL00F                               99
350400131002E   2C                   ENDDO
350500131002E   1C                   ENDIF
350600091007     C* CARICA TABELLA -2K-  blocco competenza
350700091007IF  1C     $ERR          IFEQ      'N'
350800091007     C                   Z-ADD     0             I
350900091007     C                   Z-ADD     CODUT         KTBUTE
351000091007     C                   MOVEL     '2K'          KTBCOD
351100091007     C                   MOVEL     *BLANKS       KTBKEY
351200091007     C     KEYTAB        SETLL     TABEL00F                           99
351300091007     C  N99              READ      TABEL00F                               99
351400091007IF  2C     *IN99         IFEQ      '1'
351500091007     C                   MOVEL     MSG(28)       $MSG
351600091007     C                   EXSR      SCRERR
351700091007E   2C                   ENDIF
351800091007DO  2C     *IN99         DOWEQ     '0'
351900091007     C     TBLCOD        ANDEQ     '2K'
352000091007IF  3C     TBLFLG        IFEQ      *BLANKS
352100091007     C                   MOVEL     TBLUNI        DS2K
352200091009     C                   Z-ADD     §2KNBC        WNBCD             5 0
352300091007E   3C                   ENDIF
352400091007     C                   READ      TABEL00F                               99
352500091007E   2C                   ENDDO
352600091007E   1C                   ENDIF
352700011221     c*
352800011221     C* DIVISE PER CONVERSIONE -OUTPUT C/ECONOMICO-
352900011221     C                   CLEAR                   DSGEDCE
353000011221     C                   MOVEL     *blanks       WDIVCE            3
353100011221     C                   MOVEL(P)  'GED'         KTECOD
353200011221     C                   MOVEL(P)  'CE'          KTEKE1
353300011221     C     KEYTBE        CHAIN     TNTBE01L
353400011221IF  1C                   IF        %found(TNTBE01L)
353500011221     C                   MOVEL     TBEUNI        DSGEDCE
353600011221E   1C                   ENDIF
353700011221IF  1C                   IF        §GEDCEDAT > DATCOR
353800011221     C                   EVAL      WDIVCE = §GEDCEDIP
353900011221X   1C                   ELSE
354000011221     C                   EVAL      WDIVCE = §GEDCEDIV
354100011221E   1C                   ENDIF
354200011221IF  1C                   IF        WDIVCE = *blanks
354300011221     C                   EVAL      WDIVCE = 'ITL'
354400011221E   1C                   ENDIF
354500011221     c*
354600011221     C* DIVISE PER CONVERSIONE -INPUT ANAGRAFICHE C/ECONOMICO-
354700020102     C                   CLEAR                   DGEDAC
354800011221     C                   MOVEL     *blanks       WDIVAC            3
354900011221     C                   MOVEL(P)  'GED'         KTECOD
355000011221     C                   MOVEL(P)  'AC'          KTEKE1
355100011221     C     KEYTBE        CHAIN     TNTBE01L
355200011221IF  1C                   IF        %found(TNTBE01L)
355300020102     C                   MOVEL     TBEUNI        DGEDAC
355400011221E   1C                   ENDIF
355500011221IF  1C                   IF        §GEDACDAT > DATCOR
355600011221     C                   EVAL      WDIVAC = §GEDACDIP
355700011221X   1C                   ELSE
355800020102     C                   EVAL      WDIVAC = §GEDACDID
355900011221E   1C                   ENDIF
356000011221IF  1C                   IF        WDIVAC = *blanks
356100011221     C                   EVAL      WDIVAC = 'ITL'
356200011221E   1C                   ENDIF
356300941024     C*
356400020213     C* CARICA TABELLA -IC-  CODICI QUALITÀ CLEINTE
356500020213IF  1C     $ERR          IFEQ      'N'
356600020213     C                   Z-ADD     0             I
356700020213     C                   Z-ADD     CODUT         KTBUTE
356800020213     C                   MOVEL     'IC'          KTBCOD
356900020213     C                   MOVEL     *BLANKS       KTBKEY
357000020213     C     KEYTAB        SETLL     TABEL00F                           99
357100020213     C  N99              READ      TABEL00F                               99
357200020213IF  2C     *IN99         IFEQ      '1'
357300020213     C                   MOVEL     MSG(29)       $MSG
357400020213     C                   EXSR      SCRERR
357500020213E   2C                   ENDIF
357600020213DO  2C     *IN99         DOWEQ     '0'
357700020213     C     TBLCOD        ANDEQ     'IC'
357800020213IF  3C     TBLFLG        IFEQ      *BLANKS
357900020213     C                   ADD       1             I
358000020213     C                   MOVEL     TBLKEY        CLV(I)
358100020213E   3C                   ENDIF
358200020213     C                   READ      TABEL00F                               99
358300020213E   2C                   ENDDO
358400020213E   1C                   ENDIF
358500020805     C*
358600020805     C* CARICA TABELLA -NTW- NETWORK
358700020805IF  1C     $ERR          IFEQ      'N'
358800020805     C                   Z-ADD     *ZEROS        I
358900020805     C                   MOVEL(P)  'NTW'         KTECOD
359000020805     C     KEYTB2        SETLL     TNTBE01L                           99
359100020805     C     KEYTB2        READE     TNTBE01L                               99
359200020805IF  2C     *IN99         IFEQ      *ON
359300020805     C                   MOVEL     MSG(30)       $MSG
359400020805     C                   EXSR      SCRERR
359500020805E   2C                   ENDIF
359600020805DO  2C     *IN99         DOWEQ     *OFF
359700020805     C                   MOVEL     TBEUNI        DNTW
359800020805     C                   ADD       1             I
359900020805     C                   MOVEL     TBEKE1        SNTW(I)
360000020805     C                   MOVEL     §NTWFIE       SNFIE(I)
360100020805     C     KEYTB2        READE     TNTBE01L                               99
360200020805E   2C                   ENDDO
360300020805E   1C                   ENDIF
360400040903     C* CARICA TABELLA -DFE- per peso limite tariffa pak fedex
360500040903     c                   clear                   ddfe
360600040903IF  1C     $ERR          IFEQ      'N'
360700040903     C                   MOVEL(P)  'DFE'         KTECOD
360800040903     C                   MOVEL(P)  'FED'         KTEKE1
360900040903     C     KEYTBE        chain     TNTBE01L
361000040903     c                   if        %found(tntbe01l)
361100040903     c                   movel(p)  tbeuni        ddfe
361200040903     c                   end
361300040903E   1C                   ENDIF
361400020805     C*
361500020805     C* CARICA FLAG ITALIA/ESTERO DALL'ORGANIGRAMMA
361600020805if  1C     $ERR          IFEQ      'N'
361700020805     C     *LOVAL        SETLL     AZORG01L
361800020805     C                   READ      AZORG01L                               98
361900020805DO  2C     *IN98         DOWEQ     *OFF
362000020805IF  3C     ORGFVA        IFEQ      *BLANKS
362100020805     C     ORGFAG        ANDNE     'V'
362200020805     C                   Z-ADD     ORGFIL        F                 4 0
362300020805     C                   MOVEL     ORGDE3        OG143
362400020805     C                   MOVEL     ORGCTS        CTS(F)
362500020805     C                   Z-ADD     1             II
362600020805     C     §OGNTW        LOOKUP    SNTW(II)                               97
362700020805IF  4C     *IN97         IFEQ      *OFF
362800020805     C                   MOVEL     MSG(31)       $MSG
362900020805     C                   EXSR      SCRERR
363000020805X   4C                   ELSE
363100020805     C                   MOVEL     SNTW(II)      NTW(F)
363200020805     C                   MOVEL     SNFIE(II)     FIE(F)
363300020805E   4C                   ENDIF
363400020805E   3C                   ENDIF
363500020805     C                   READ      AZORG01L                               98
363600020805E   2C                   ENDDO
363700020805e   1C                   ENDIF
363800020213     C*
363900941024     C                   CLOSE     TABEL00F
364000011221     C                   CLOSE     TNTBE01L
364100020805     C                   CLOSE     AZORG01L
364200940704     C*
364300940704     C                   ENDSR
364400940803     C*------------------------------------------------------------------------*
364500940803     C*? DATCLI - RECUPERA DATI CLIENTE
364600940902     C*           INPUT: - D$DKSC (CODICE DEL CLIENTE)
364700940803     C*           OUTPUT: - WGGP (GIORNI PAGAMENTO)
364800940803     C*                   - WCLV (CLASSE CLIENTE)
364900940803     C*------------------------------------------------------------------------*
365000940722     C     DATCLI        BEGSR
365100971222     C*
365200971222     C* LANCIA PGM PER RECUPERARE DATI CLIENTE
365300971222     C                   CLEAR                   TIBS69
365400971222     C                   Z-ADD     WCLI          I69KIN
365500971222     C                   Z-ADD     WCLI          I69KCP
365600971222     C                   CALL      'TIBS69R'
365700971222     C                   PARM                    TIBS69
365800971222     C                   PARM                    ACODS
365900971222     C                   PARM                    INDDS
366000971222     C                   PARM                    CLPDS
366100971222     C                   PARM                    CLSDS
366200971222IF  1C     O69ERR        IFEQ      '1'
366300020701     C                   MOVE      'BS69'        O69MSG
366400020520     C                   MOVEL     O69MSG        DSMSG
366500971222     C                   MOVEL     O69MSG        $MSG
366600971222     C                   EXSR      SCRERR
366700971222X   1C                   ELSE
366800940722     C*
366900940722     C* RECUPERA GIORNI PAGAMENTO DALL'ANAGRAFICO INDIRIZZI (CNIND)
367000940722     C                   Z-ADD     *ZEROS        WGGP              3 0
367100940802     C                   MOVE      INDCDP        WTM03
367200940722     C                   Z-ADD     1             I
367300940722     C     WTM03         LOOKUP    CFA(I)                                 98
367400971222IF  2C     *IN98         IFEQ      '0'
367500940722     C                   MOVEL     MSG(23)       $MSG
367600940907     C                   EXSR      SCRERR
367700971222X   2C                   ELSE
367800940722     C                   Z-ADD     GFA(I)        WGGP
367900971222E   2C                   ENDIF
368000940722     C*
368100940722     C* RECUPERA LA CLASSE DEL CLIENTE DALL'ANAGRAFICO PARTICOLARITA' (CNCLP)
368200020213     C                   Z-ADD     1             I
368300020213     C     CLPCLV        LOOKUP    CLV(I)                                 98
368400020213IF  2C     *IN98         IFEQ      *ON
368500971229     C                   MOVEL     CLPCLV        WCLV              1
368600971222X   2C                   ELSE
368700940722     C                   MOVEL     MSG(24)       $MSG
368800940907     C                   EXSR      SCRERR
368900971222E   2C                   ENDIF
369000971222     C*
369100971222E   1C                   ENDIF
369200940722     C*
369300940722     C                   ENDSR
369400940831     C*------------------------------------------------------------------------*
369500940831     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
369600940831     C*------------------------------------------------------------------------*
369700940831     C     SUBDAT        BEGSR
369800940831     C*
369900940831     C                   TIME                    DTTIME
370000940831     C                   Z-ADD     DTANN         DSANN
370100940831     C                   Z-ADD     DTMES         DSMES
370200940831     C                   Z-ADD     DTGIO         DSGIO
370300940901     C                   Z-ADD     DTSEC         DSSEC
370400940831     C*
370500940831     C                   ENDSR
370600940907     C*------------------------------------------------------------------------*
370700940907     C*?SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SLSDE (ERRORI SALDI)
370800940907     C*------------------------------------------------------------------------*
370900940907     C     SCRERR        BEGSR
371000940907     C*
371100940913     C                   MOVEL     'S'           $ERR
371200970407     C*
371300970407     C* SE LANCIATO DAL PGM TNTE50R (DELTA IN FILIALE) NON SCRIVO ERRORI
371400970407     C     D$GPGF        IFEQ      'TNTE50R'
371500970407     C                   MOVEL     $MSG          D$GERR
371600970407     C                   MOVEL     $MSG          D$GMSG
371700970407     C                   ELSE
371800941026     C*
371900941026     C* SCRIVE SOLO SE NON VI E' GIA' STATO ALTRO ERRORE (IN QUESTO O ALTRO PGM)
372000941026     C     D$GERR        IFEQ      *BLANKS
372100941026     C*
372200940907     C                   EXSR      SUBDAT
372300940907     C                   Z-ADD     DSDAT8        ERRDAT
372400940907     C                   Z-ADD     DTORA6        ERRTIM
372500941026     C                   ADD       1             D$GPRE
372600941026     C                   Z-ADD     D$GPRE        ERRPRG
372700941213     C                   Z-ADD     D$DAAS        ERRAAS
372800940907     C                   MOVEL     D$DTBL        ERRTBL
372900940907     C                   Z-ADD     D$DLNP        ERRLNP
373000940907     C                   Z-ADD     D$DNRS        ERRNRS
373100940907     C                   Z-ADD     D$DNSP        ERRNSP
373200941201     C                   Z-ADD     D$DAAS        DSAAS
373300941201     C                   Z-ADD     D$DMGS        DSMGS
373400941201     C                   Z-ADD     DSDAT8        ERRDSP
373500941007     C                   MOVEL     $MSG          DSMSG
373600941007     C                   MOVEL     DSDER         ERRDER
373700941007     C                   MOVEL     DSCER         ERRCER
373800950123     C                   MOVEL     D$GSTO        ERRSTS
373900941026     C                   Z-ADD     D$GDAT        ERRDAM
374000941026     C                   Z-ADD     D$GTIM        ERRORM
374100941026     C                   MOVEL     D$GUSR        ERRUSR
374200941026     C                   MOVEL     D$GPGF        ERRPGF
374300950118     C                   MOVEL     D$GFIL        ERRFIL
374400950118     C                   MOVEL     D$GMBR        ERRMBR
374500950118     C                   MOVEL     D$GLIB        ERRLIB
374600991119     C     D$GFIL        IFEQ      'TITAS10F'
374700020703     C     D$GFIL        OREQ      'TITAS00F'
374800950118     C                   MOVEL     D$PRE1        ERRRP1
374900950118     C                   MOVEL     D$PRE2        ERRRP2
375000950118     C                   MOVEL     D$DRE1        ERRRD1
375100950118     C                   MOVEL     D$DRE2        ERRRD2
375200950123     C                   ENDIF
375300950123     C     D$GFIL        IFEQ      'TNCSB00F'
375400950123     C                   MOVEL     D$PCO1        ERRRP1
375500950123     C                   MOVEL     D$PCO2        ERRRP2
375600950123     C                   MOVEL     D$DCO1        ERRRD1
375700950123     C                   MOVEL     D$DCO2        ERRRD2
375800950123     C                   ENDIF
375900030205     C     D$GFIL        IFEQ      'FIAR500F'
376000030205     C     D$GTRD        IFEQ      'TRS'
376100030205     C                   MOVEL     D$PAR5T       ERRRP1
376200030205     C                   MOVEL     D$DAR5T       ERRRD1
376300030205     C                   ENDIF
376400030801     C     D$GTRD        IFEQ      'BAN'
376500030801     C     D$GTRD        OREQ      'BNB'
376600030801     C                   MOVEL     D$PAR5B       ERRRP1
376700030801     C                   MOVEL     D$DAR5B       ERRRD1
376800030801     C                   ENDIF
376900030801     C                   ENDIF
377000030801     C                   WRITE     ECERR
377100941026     C*
377200941026     C                   MOVEL     DSCER         D$GERR
377300941026     C                   MOVEL     DSDER         D$GMSG
377400941026     C*
377500941026     C                   ENDIF
377600970407     C                   ENDIF
377700940907     C*
377800940907     C                   ENDSR
377900940907     C*------------------------------------------------------------------------*
378000940907     C*? RUTINZ - ROUTINE INIZIALE
378100940907     C*------------------------------------------------------------------------*
378200940907     C     RUTINZ        BEGSR
378300940907     C*
378400940907     C* PARAMETRI IN INPUT ALLA ROUTINE
378500940907     C     *ENTRY        PLIST
378600940907     C                   PARM                    DS$ECO
378700121120     c                   movel     d$gxxx        dsd$g
378800121120     c                   if        d$gfsf = 'S' and
378900121120     c                             not %open(esvoc01l)
379000121120     c                   open      esvoc01l
379100121120     c                   open      esvoc11l
379200121120     c                   end
379300940907     C*
379400940907     C* CHIAVE LETTURA TABEL00F - COMPLETA
379500940907     C     KEYTAB        KLIST
379600940907     C                   KFLD                    KTBUTE
379700940907     C                   KFLD                    KTBCOD
379800940907     C                   KFLD                    KTBKEY
379900011221     C*
380000011221     C* CHIAVE LETTURA TNTBE01L - PARZIALE
380100011221     C     KEYTBE        KLIST
380200011221     C                   KFLD                    KTECOD
380300011221     C                   KFLD                    KTEKE1
380400020805     C*
380500020805     C* CHIAVE LETTURA TNTBE01L - PARZIALE
380600020805     C     KEYTB2        KLIST
380700020805     C                   KFLD                    KTECOD
380800091007     C* CHIAVE LETTURA ECECT01L - PARZIALE
380900091007     C     KEYECT        KLIST
381000091007     C                   KFLD                    WNBCD
381100091007     C                   KFLD                    vcmp(v)
381200940907     C*
381300940907     C* CHIAVE LETTURA ECEVT01L - COMPLETA
381400940907     C     KEYEVT        KLIST
381500940907     C                   KFLD                    EVTVOC
381600940907     C                   KFLD                    EVTPRG
381700091009     C* CHIAVE LETTURA ESVOC01L - COMPLETA
381800091009     C     KEYEs         KLIST
381900091009     C                   KFLD                    vocfil
382000091009     C                   KFLD                    vocdat
382100091009     C                   KFLD                    vocvoc
382200091009     C                   KFLD                    vocsgi
382300091009     C                   KFLD                    vocsgf
382400100518     C     KEYEs10       KLIST
382500100518     C                   KFLD                    v10fil
382600100518     C                   KFLD                    v10dat
382700100518     C                   KFLD                    v10voc
382800940907     C*
382900131002     C* CHIAVE LETTURA ecevd06l - COMPLETA
383000940907     C     KEYEVD        KLIST
383100940907     C                   KFLD                    EVDVOC
383200940907     C                   KFLD                    EVDPRG
383300940907     C                   KFLD                    EVDTTV
383400940907     C                   KFLD                    EVDFT1
383500940907     C                   KFLD                    EVDFT2
383600940907     C                   KFLD                    EVDCLI
383700940907     C                   KFLD                    EVDCTR
383800940907     C                   KFLD                    EVDCTS
383900131002     C                   KFLD                    EVDnaz
384000131002     C                   KFLD                    EVDCAd
384100940907     C                   KFLD                    EVDDDT
384200940907     C                   KFLD                    EVDSGI
384300940907     C*
384400940907     C* CHIAVE LETTURA ECEVD02L - COMPLETA
384500940907     C     KEYEV2        KLIST
384600940907     C                   KFLD                    EVDCLI
384700940907     C*
384800940907     C* CHIAVE LETTURA ECEVM01L - COMPLETA
384900940907     C     KEYEVM        KLIST
385000940907     C                   KFLD                    EVMVOM
385100940907     C                   KFLD                    EVMPRM
385200940907     C                   KFLD                    EVMVOC
385300940907     C                   KFLD                    EVMPRG
385400940907     C*
385500940907     C* CHIAVE LETTURA ECERR01L - COMPLETA
385600940907     C     KEYERR        KLIST
385700940907     C                   KFLD                    ERRDAT
385800940907     C                   KFLD                    ERRTIM
385900940907     C                   KFLD                    ERRPRG
386000940907     C*
386100940907     C* DEFINISCO CAMPI
386200940907     C     *LIKE         DEFINE    TBLKUT        KTBUTE
386300940907     C     *LIKE         DEFINE    TBLCOD        KTBCOD
386400940907     C     *LIKE         DEFINE    TBLKEY        KTBKEY
386500011221     C     *LIKE         DEFINE    TBECOD        KTECOD
386600011221     C     *LIKE         DEFINE    TBEKE1        KTEKE1
386700940907     C* CAMPI VARI
386800940907     C     *LIKE         DEFINE    EVTARF        WVOL
386900991008     C     *LIKE         DEFINE    D$DPKF        WPES
387000040427     C     *LIKE         DEFINE    D$DPKF        WDPES
387100940907     C     *LIKE         DEFINE    EVTVOC        WVOC
387200940907     C     *LIKE         DEFINE    EVTPRG        WPRG
387300000306     C     *LIKE         DEFINE    EVTTIV        WTIV
387400940907     C     *LIKE         DEFINE    EVTRFI        WRIF
387500940907     C     *LIKE         DEFINE    EVDTTV        WTTV
387600940907     C     *LIKE         DEFINE    EVDFT1        WFT1
387700940907     C     *LIKE         DEFINE    EVDFT2        WFT2
387800940907     C     *LIKE         DEFINE    EVDCLI        WKSC
387900940907     C     *LIKE         DEFINE    EVDCTR        WCTR
388000940907     C     *LIKE         DEFINE    EVDCTS        WCTS
388100131002     C     *LIKE         DEFINE    EVDnaz        Wnaz
388200131002     C     *LIKE         DEFINE    EVDCAd        wcad
388300940907     C     *LIKE         DEFINE    EVTDDV        WDSP
388400940907     C     *LIKE         DEFINE    EVTDDV        WDDSP
388500940908     C     *LIKE         DEFINE    EVTDDV        WSPDAT
388600940907     C     *LIKE         DEFINE    D$DKSC        WCLI
388700940907     C     *LIKE         DEFINE    D$DLNP        W$LNP
388800111020     C     *LIKE         DEFINE    D$Dlna        W$LNA
388900940907     C     *LIKE         DEFINE    D$DTFP        W$TFP
389000940907     C     *LIKE         DEFINE    D$DKSC        W$KSC
389100940908     C     *LIKE         DEFINE    D$DKSC        WSPKSC
389200940907     C     *LIKE         DEFINE    D$DCCM        W$CCM
389300940907     C     *LIKE         DEFINE    D$DMCT        W$MCT
389400011227     C     *LIKE         DEFINE    EVDSGI        W$SGI
389500011227     C     *LIKE         DEFINE    EVDSGF        W$SGF
389600011227     C     *LIKE         DEFINE    EVDITR        W$ITR
389700011227     C     *LIKE         DEFINE    EVDIMI        W$IMI
389800011227     C     *LIKE         DEFINE    EVDIMA        W$IMA
389900020102     C     *LIKE         DEFINE    YECIMI        §IMI
390000020102     C     *LIKE         DEFINE    YECDVI        §DVI
390100020102     C     *LIKE         DEFINE    YECIMO        §IMO
390200021029     C     *LIKE         DEFINE    §1BF12        WCF12
390300050801     C     *LIKE         DEFINE    §1BF16        WCF16
390400030530     C     *LIKE         DEFINE    §1BDKC        WCDKC
390500950123     C*
390600950123     C* FACCIO LETTURA DI ECERR FASULLA PER NON FARE VENIRE MESSAGGIO
390700950123     C* AD OGNI APERTURA DI DEL FILE
390800950123     C   99
390900950123     CANN99KEYERR        CHAIN     ECERR01L                           99
391000940907     C*
391100940907     C* IMPOSTO CAMPI DI GESTIONE PGM
391200940913     C                   MOVEL     'N'           $ERR              1
391300941006     C                   MOVEL     *BLANKS       $MSG             80
391400940907     C*
391500940907     C* RECUPERO IL CODICE DELL'AZIENDA
391600940907     C                   Z-ADD     1             CODUT             1 0
391700940907     C*
391800940907     C* SE PGM CHIAMATO PER CHIUSURA ESCE
391900991214     C     D$GSTS        IFEQ      'C'
392000991214     C     D$GSTS        OREQ      'S'
392100991214     C                   ELSE
392200940907     C*
392300940907     C* PORTA LE IMMAGINI BOLLA PRIMA E DOPO DALLA DS INPUT ALLA DS PGM
392400940907     C                   MOVEL     D$PRE1        D$PDS1
392500940907     C                   MOVEL     D$PRE2        D$PDS2
392600940907     C                   MOVEL     D$DRE1        D$DDS1
392700940907     C                   MOVEL     D$DRE2        D$DDS2
392800941201     C                   MOVEL     D$ORE1        D$ODS1
392900941201     C                   MOVEL     D$ORE2        D$ODS2
393000940907     C*
393100940907     C* PORTA I RICAVI IN INPUT NELLA SCHIERA DEL PGM
393200950125     C                   MOVEA     D$DCCD        CAN
393300950125     C                   MOVEA     D$DICD        IAN
393400030506DO  2C     1             DO        75            I                 6 0
393500940907     C                   MOVE      CAN(I)        DSWCA
393600940907     C                   Z-ADD     DSWCN         RCD(I)
393700011221     C                   MOVE      IAN(I)        DSWIA
393800940907     C                   Z-ADD     DSWIN         RIM(I)
393900940907E   2C                   ENDDO
394000940907     C*
394100940907     C* IMPOSTA A ZEROS LE SCHIERE IN OUTPUT
394200940907     C                   CLEAR                   CCD
394300940907     C                   CLEAR                   CIM
394400940907     C                   CLEAR                   CIMP
394500940907     C                   CLEAR                   VIMP
394600940907     C                   CLEAR                   Q2R
394700940907     C*
394800940907     C* RECUPERO DATI VARI
394900941201     C                   Z-ADD     D$DAAS        DSAAS
395000941201     C                   Z-ADD     D$DMGS        DSMGS
395100941201     C                   Z-ADD     DSDAT8        WDDSP
395200940907     C*
395300940907     C* CARICA LE TABELLE NECESSARIE AL PGM
395400940907     C* CARICA LE TARIFFE DI CARTELLO
395500940907     C* SOLO NEL CASO IN CUI NON SIANO ANCORA STATE CARICATE
395600940913     C     $ERR          IFEQ      'N'
395700940907     C     CTABSN        IFNE      'S'
395800940907     C     D$GSTS        ANDNE     'C'
395900940907     C                   MOVEL     'S'           CTABSN            1
396000940907     C                   EXSR      CARTAB
396100940907     C                   EXSR      CARCAR
396200940907     C                   EXSR      CARKSC
396300940907     C                   EXSR      CARMAG
396400940907     C                   ENDIF
396500940907     C                   ENDIF
396600940907     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI AL POSTO DEI DATI DOPO
396700940907     C     D$OLNP        IFNE      *ZEROS
396800950913     C     D$DFIG        ANDEQ     'S'
396900940907     C                   Z-ADD     D$OLNP        W$LNP
397000940907     C                   ELSE
397100940907     C                   Z-ADD     D$DLNP        W$LNP
397200940907     C                   ENDIF
397300940907     C     D$OTFP        IFNE      *ZEROS
397400950913     C     D$DFIG        ANDEQ     'S'
397500940907     C                   Z-ADD     D$OTFP        W$TFP
397600940907     C                   ELSE
397700940907     C                   Z-ADD     D$DTFP        W$TFP
397800940907     C                   ENDIF
397900940907     C     D$OKSC        IFNE      *ZEROS
398000950913     C     D$DFIG        ANDEQ     'S'
398100940907     C                   Z-ADD     D$OKSC        W$KSC
398200940907     C                   ELSE
398300940907     C                   Z-ADD     D$DKSC        W$KSC
398400940907     C                   ENDIF
398500940907     C     D$OCCM        IFNE      *ZEROS
398600950913     C     D$DFIG        ANDEQ     'S'
398700940907     C                   Z-ADD     D$OCCM        W$CCM
398800940907     C                   ELSE
398900940907     C                   Z-ADD     D$DCCM        W$CCM
399000940907     C                   ENDIF
399100940907     C     D$OMCT        IFNE      *BLANKS
399200950913     C     D$DFIG        ANDEQ     'S'
399300940907     C                   MOVEL     D$OMCT        W$MCT
399400940907     C                   ELSE
399500940907     C                   MOVEL     D$DMCT        W$MCT
399600940907     C                   ENDIF
399700111020     c* verifico se la linea di arrivo è in £6
399800111020     c                   z-add     d$dlna        w$lna
399900111020     c                   clear                   fnlv55ds
400000111020     c                   z-add     d$dlna        d55lin
400100111020     c                   movel     '6'           d55tpt
400200111020     C                   eval      d55drf = WDDSP
400300111020     c                   if        d$ddti <> 0
400400111020     C                   eval      d55drf = d$ddti
400500111020     c                   end
400600111020     c                   call      'FNLV55R'
400700111020     c                   parm                    fnlv55ds
400800111020     c                   if        d55tfa <> 0
400900111020     c                   eval      w$lna = d55tfa
401000111020     c                   end
401100021029     C*
401200021029     C* CARICA I DATI DEL CLIENTE di fatturazione, SE DIVERSO DALL'ULTIMO LETTO
401300021029     C     $ERR          IFEQ      'N'
401400021029     C     W$KSC         IFNE      WCLI
401500021029     C                   Z-ADD     W$KSC         WCLI
401600021029     C                   EXSR      DATCLI
401700021029     C                   ENDIF
401800021029     C                   ENDIF
401900021029     C*
402000140203     c* calcolo le qta della spedizione per saldi tempi uffici
402100140203     c                   if        D$GFSF = 'S'
402200140203     c                   exsr      srqta
402300140203     c                   end
402400940907     C*
402500940907     C                   ENDIF
402600940907     C*
402700940907     C                   ENDSR
402800940907     C*------------------------------------------------------------------------*
402900940907     C*? STAMPA - STAMPA RIGA
403000940907     C*------------------------------------------------------------------------*
403100941229     C*****+++   STAMPA    BEGSR
403200940902     C*
403300941024     C* APRE IL FILE DI STAMPA
403400941229     C****++++             OPEN PRTF198
403500941024     C*
403600940902     C* RECUPERO LA DATA E L'ORA
403700941229     C*****++++            Z-ADDUDATE     DATA    60
403800941229     C****+++++            TIME           ORA     60
403900940902     C*
404000940902     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
404100941229     C****+++++            EXCPTTESTA
404200940902     C*
404300940902     C* STAMPA RIGA DI DETTAGLIO TEMPI
404400941229     C***++++++            EXCPTRIGA
404500940902     C*
404600940902     C* STAMPA RIGA FINE
404700941229     C****+++++            EXCPTSTAFIN
404800941024     C*
404900941024     C* CHIUDE IL FILE DI STAMPA
405000941229     C****+++++            CLOSEPRTF198
405100941229     C****++++
405200941229     C***++++++            ENDSR
405300940907     O*------------------------------------------------------------------------*
405400941229     O*PRTF198 E  102           TESTA
405500941229     O****++++                            8 'TRECD8R '
405600941229     O****++++                           82 'PRESTAZIONI PGM'
405700941229     O****++++                          150 'Data:'
405800941229     O****++++                 DATA     159 '  /  /  '
405900941229     O****++++                          167 'Ora:'
406000941229     O****++++                 ORA      178 '  .  .  '
406100941229     O****++++                          190 'Pagina:'
406200941229     O****++++                 PAGE  Z  198
406300941229     O*------------------------
406400941229     O****++++E  1             RIGA
406500941229     O****++++                           30 'NUMERO DI BOLLE.....:'
406600941229     O*****++++++++++++++++++++WBOLLE2   40
406700941229     O****++++E  1             RIGA
406800941229     O****++++                           30 'TENT.RICERCA TARIFFA:'
406900941229     O******+++++++++++++++++++WTEEVD2   40
407000941229     O****++++E  1             RIGA
407100941229     O****++++                           30 'TENT.SETLL   TARIFFA:'
407200941229     O******+++++++++++++++++++WSEEVD2   40
407300941229     O****++++E  1             RIGA
407400941229     O****++++                           30 'TENT.LETTURE TARIFFA:'
407500941229     O*****++++++++++++++++++++WLEEVD2   40
407600941229     O****++++E  1             RIGA
407700941229     O****++++                           30 'TENT.CARTELLO.......:'
407800941229     O*****++++++++++++++++++++WTECAR2   40
407900941229     O****++++E  1             RIGA
408000941229     O****++++                           30 'CARTELLO IN MEMORIA.:'
408100941229     O****+++++++++++++++++++++WMECAR2   40
408200941229     O****++++E  1             RIGA
408300941229     O****++++                           30 'CARTELLO LETTE......:'
408400941229     O*****++++++++++++++++++++WLECAR2   40
408500941229     O****++++E  1             RIGA
408600941229     O****++++                           30 'TENT.FILIALE........:'
408700941229     O*****++++++++++++++++++++WTEFIL2   40
408800941229     O****++++E  1             RIGA
408900941229     O****++++                           30 'FILIALE IN MEMORIA..:'
409000941229     O*****++++++++++++++++++++WMEFIL2   40
409100941229     O****++++E  1             RIGA
409200941229     O****++++                           30 'FILIALE LETTE.......:'
409300941229     O*****++++++++++++++++++++WLEFIL2   40
409400941229     O****++++E  1             RIGA
409500941229     O****++++                           30 'TENT.CLIENTE........:'
409600941229     O*****++++++++++++++++++++WTEKSC2   40
409700941229     O****++++E  1             RIGA
409800941229     O****++++                           30 'CLIENTE IN MEMORIA..:'
409900941229     O*****++++++++++++++++++++WMEKSC2   40
410000941229     O****++++E  1             RIGA
410100941229     O****++++                           30 'CLIENTE LETTE.......:'
410200941229     O******+++++++++++++++++++WLEKSC2   40
410300941229     O****++++E  1             RIGA
410400941229     O****++++                           30 'TENT.MAGGIORAZIONI..:'
410500941229     O******+++++++++++++++++++WTEMAG2   40
410600941229     O****++++E  1             RIGA
410700941229     O****++++                           30 'MAGGIOR. IN MEMORIA.:'
410800941229     O******+++++++++++++++++++WMEMAG2   40
410900941229     O****++++E  1             RIGA
411000941229     O****++++                           30 'MAGGIORAZIONI LETTE.:'
411100941229     O*****++++++++++++++++++++WLEMAG2   40
411200940907     O*------------------------
411300941229     O****++++E   60           STAFIN
411400941229     O*****+++                           76 '*** FINE STAMPA ***'
411500940907     O*------------------------------------------------------------------------*
411600941201**   MSG
411700000623TRECD8R- Tabella '7O' non trovata. Avvertire CED.                         TB7O 1
411800941201TRECD8R- Tabella 'FA' non trovata. Avvertire CED.                         TBFA 2
411900941201TRECD8R- Tabella '2R' non trovata. Avvertire CED.                         TB2R 3
412000941201TRECD8R- Tipo tariffa '00' (quintali) non trovata.                        TT00 4
412100941201TRECD8R- Tipo tariffa '01' (colli) non trovata.                           TT01 5
412200941201TRECD8R- Tipo tariffa '02' (n.spedizioni) non trovata.                    TT02 6
412300941201TRECD8R- Tipo tariffa '03' (chilogrammi) non trovata.                     TT03 7
412400941201TRECD8R- Tipo tariffa '04' (volume) non trovata.                          TT04 8
412500951213TRECD8R- Tipo tariffa '09' (stop ritiro) non trovata.                     TT09 9
412600941201TRECD8R- Tipo tariffa '20' (val.limite risarcibile) non trovata.          TT2010
412700941201TRECD8R- Tipo tariffa '21' (imponibile reale) non trovata.                TT2111
412800941201TRECD8R- Tipo tariffa '22' (contrassegno) non trovata.                    TT2212
412900941201TRECD8R- Tipo tariffa '23' (val merce assicur.) non trovata.              TT2313
413000941201TRECD8R- Tipo tariffa '24' (n.giorni pagamento) non trovata.              TT2414
413100941201TRECD8R- Tipo tariffa '25' (imp.comp.sede) non trovata.                   TT2515
413200951213TRECD8R- Tipo tariffa '10' (stop consegna) non trovata.                   TT1016
413300991029TRECD8R- Tipo tariffa '11' (peso x giorni giacenza) non trovata.          TT1117
413400991119TRECD8R- Voce non trovata.  XXX                                           VNTR18
413500941201TRECD8R- Tipo tariffa della voce non trovata in schiera.                  TTNT19
413600941201TRECD8R- Qta x ricerca scaglione non trovata.                             QRSN20
413700941201TRECD8R- Manca tariffa.Voce xxx.999 999                                   MATA21
413800941201TRECD8R- Voce di maggiorazione senza voci da maggiorare.                  VMSV22
413900941201TRECD8R- Condizione di pagamento non trovata.                             CPNT23
414000020213TRECD8R- Codice importanza cliente non trovato.                           APNT24
414100000201TRECD8R- Tipo tariffa '12' (giorni giacenza) non trovata.                 TT1125
414200000623TRECD8R- Codice consegna anomala non trovata                              CANT26
414300010420TRECD8R- Codice trattamento merce non trovata                             TMNT27
414400010801TRECD8R- Tabella '1B' non trovata. Avvertire CED.                         TB1B28
414500020213TRECD8R- Tabella 'IC' non trovata. Avvertire CED.                         TBIC29
414600020805TRECD8R- Tabella 'NTW' non trovata. Avvertire CED.                        TNTW30
414700020805TRECD8R- Network filiale non trovato in tab. 'NTW'. Avvertire CED.        NWNT31
414800021029TRECD8R- Tabella '7R' non trovata. Avvertire CED.                         TB7R32
414900021029TRECD8R- Codice particolarità consegna non trovato                        GMNT33
415000030506TRECD8R- Più di 75 voci calcolate. Avvertire CED.                         P75V34
415100030409TRECD8R- Tipo tariffa '06' (bancali) non trovata.                         TT0635
415200020805TRECD8R-                                                                      36
415300020805TRECD8R-                                                                      37
415400020805TRECD8R-                                                                      38
415500020805TRECD8R-                                                                      39
415600020805TRECD8R-                                                                      40
