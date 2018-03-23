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
101000050926     C                   MOVEL     'S'           VC(26)
101100050926     c                   end
101200040520     C                   ENDIF
101300040520     C*
101400040520     C* CONTROLLO SE ESISTE =======> TRANSITO 4
101500040520     C*    TRANSITO 4 DIVERSO DA ZERO
101600040520     C     D$Dar5fl4     IFNE      *ZEROS
101700040520     C                   MOVEL     'S'           VC(78)
101800040520     C                   MOVEL     'S'           VC(132)
101900040520     C                   MOVEL     'S'           VC(135)
102000040520     C                   MOVEL     'N'           VS(78)
102100040520     C                   MOVEL     'N'           VS(132)
102200040520     C                   MOVEL     'N'           VS(135)
102300050926     c* merce di valore
102400050926IF  1C     §FLOTBA       IFEQ      'V'
102500050926     C                   MOVEL     'S'           VC(27)
102600050926     c                   end
102700040520     C                   ENDIF
102800040520     C*
102900040520     C* CONTROLLO SE ESISTE =======> TRANSITO 5
103000040520     C*    TRANSITO 5 DIVERSO DA ZERO
103100040520     C     D$Dar5fl5     IFNE      *ZEROS
103200040520     C                   MOVEL     'S'           VC(79)
103300040520     C                   MOVEL     'S'           VC(133)
103400040520     C                   MOVEL     'S'           VC(136)
103500040520     C                   MOVEL     'N'           VS(79)
103600040520     C                   MOVEL     'N'           VS(133)
103700040520     C                   MOVEL     'N'           VS(136)
103800050926     c* merce di valore
103900050926IF  1C     §FLOTBA       IFEQ      'V'
104000050926     C                   MOVEL     'S'           VC(28)
104100050926     c                   end
104200040520     C                   ENDIF
104300940722     C*
104400940722     C* CONTROLLO SE ESISTE =======> LOCALI
104500980317     C*    LINEA PART. = LINEA ARR. = TERM. PART. = TERM. ARR.
104600111020     C     W$LNP         IFEQ      W$lna
104700980317     C     W$LNP         ANDEQ     D$DTFA
104800980318     C     W$LNP         ANDEQ     W$TFP
104900940728     C                   MOVEL     'S'           VC(500)
105000940722     C                   ENDIF
105100940728     C*
105200950626     C* CONTROLLO SE ESISTE =======> DISK 'A' E DISK 'B'
105300950626     C*    SE SIGLA OPERATORE = '§' O '%' E' UN DISK 'A' O DISK 'B'
105400950626     C     D$DSOP        IFEQ      '§'
105500950626     C     D$DSOP        OREQ      '%'
105600000210     C     D$DSOP        OREQ      ':'
105700950626     C*    SE N.SERIE > ZERO E' UN DISK 'B' ALTRIMENTI E' UN DISK 'A'
105800950626     C     D$DNRS        IFGT      *ZEROS
105900950626     C                   MOVEL     'S'           VC(510)
106000950626     C                   MOVEL     'S'           VC(511)
106100990223     C                   MOVEL     'S'           VC(55)
106200950626     C                   ELSE
106300950626     C                   MOVEL     'S'           VC(511)
106400940728     C                   ENDIF
106500121128     c* sbinata la 511: ora la 511 sarà per la lnp e la 512 per lna
106600130930IF  2C     WDDSP         ifgt      20181231
106700121128     C                   MOVEL     'S'           VC(512)
106800950626     C                   ENDIF
106900121128     C                   ENDIF
107000021029     C*
107100021029     C* REPERISCE I DATI DEL CODICE TRATTAMENTO MERCE =======>
107200021029     C                   CLEAR                   WCF12
107300050801     C                   CLEAR                   WCF16
107400030530     C                   CLEAR                   WCDKC
107500021029IF  1C     D$DCTM        IFNE      *BLANKS
107600021029     C                   Z-ADD     1             I
107700021029     C     D$DCTM        LOOKUP    CTM(I)                                 98
107800021029IF  2C     *IN98         IFEQ      *OFF
107900021029     C                   MOVEL     MSG(27)       $MSG
108000021029     C                   EXSR      SCRERR
108100021029X   2C                   ELSE
108200021029     C                   MOVEL     CF12(I)       WCF12
108300050801     C                   MOVEL     CF16(I)       WCF16
108400030530     C                   MOVEL     CDKC(I)       WCDKC
108500021029E   2C                   ENDIF
108600021029E   1C                   ENDIF
108700010420     C*
108800021029     C* CONTROLLA SE ESISTE =======> DISCK 'C'
108900030530IF  1C     WCDKC         IFNE      *BLANKS
109000010420     C                   MOVEL     'S'           VC(56)
109100030530E   1C                   ENDIF
109200050801     C*
109300050801     C* CONTROLLA SE ESISTE =======> packing list
109400050801IF  1C     D$DCTM        IFNE      *BLANKS
109500050801IF  2C     WCF16         andeq     'S'
109600050801     C                   MOVEL     'S'           VC(189)
109700050801E   1C                   ENDIF
109800130930     c*
109900130930     C* =============> ETICHETTATURA , SCARICO , PICKING e CARICO
110000021029     C*    DELL' AUTOTRASPORTATORE (flag =" ") OPPURE
110100021029     C*    DELLA COOPERATIVA       (flag ="C")
110200021029IF  1C     §FLOSCA       IFEQ      *BLANKS
110300021029     C                   MOVEL     'S'           VC(30)
110400021212     C                   MOVEL     'S'           VC(57)
110500021029X   1C                   ELSE
110600021029     C                   MOVEL     'S'           VC(33)
110700021029E   1C                   ENDIF
110800040520     C* BOLLA CON IMMAGINE APPLICO LA VOCE 188
110900040520IF  1C     §FLOIML       IFNE      *BLANKS
111000040520     C                   MOVEL     'S'           VC(188)
111100040520E   1C                   ENDIF
111200021029     C* NO disck C
111300021029IF  1C     WCF12         IFEQ      'N'
111400021029IF  2C     §FLOETI       IFEQ      *BLANKS
111500021029     C                   MOVEL     'S'           VC(40)
111600021029X   2C                   ELSE
111700021029     C                   MOVEL     'S'           VC(42)
111800021029E   2C                   ENDIF
111900021029E   1C                   ENDIF
112000030918     C* disck C NON gestito dal cliente
112100030530IF  1C     WCDKC         IFEQ      'B'
112200030919IF  3C     D$DFDN        IFEQ      'S'
112300030918     C                   MOVEL     'S'           VC(44)
112400030919X   3c                   ELSE
112500021029     C                   MOVEL     'S'           VC(43)
112600030919E   2C                   ENDIF
112700030530E   1C                   ENDIF
112800021029     c*
112900021029IF  1C     §FLOPIC       IFEQ      *BLANKS
113000021029     C                   MOVEL     'S'           VC(140)
113100021029X   1C                   ELSE
113200021029     C                   MOVEL     'S'           VC(141)
113300021029E   1C                   ENDIF
113400021029IF  1C     §FLOCAR       IFEQ      *BLANKS
113500021029     C                   MOVEL     'S'           VC(150)
113600021029X   1C                   ELSE
113700021029     C                   MOVEL     'S'           VC(153)
113800021029E   1C                   ENDIF
113900940526     C*
114000940526     C* CONTROLLO SE ESISTE =======> GIACENZA
114100940526     C*    FLAG GIACENZA UGUALE A SI
114200940526     C     D$DFGC        IFEQ      'S'
114300940728     C                   MOVEL     'S'           VC(260)
114400940728     C                   MOVEL     'S'           VC(261)
114500940728     C                   MOVEL     'S'           VC(262)
114600040427     C     wdpes         IFLE      100
114700000201     C                   MOVEL     'S'           VC(267)
114800000201     C                   ELSE
114900000201     C                   MOVEL     'S'           VC(265)
115000110321     C                   ENDIF
115100940728     C*    SE NON E' DIROTTATA E RESO MITT.
115200940728     C     D$DCCA        IFNE      '1'
115300940728     C     D$DCCA        ANDNE     '2'
115400960329     C     D$DCCA        ANDNE     '6'
115500940728     C                   MOVEL     'S'           VC(263)
115600940526     C                   ENDIF
115700940728     C                   ENDIF
115800940526     C*
115900940728     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
116000130930     C*    FLAG FERMO DEPOSITO = "S"
116100130930     c*    OPPURE N.STOP CONSEGNA = 99999 (consegna a magazzino)
116200990420     C*    OPPURE INOLTRO = I (CONSEGNA ISOLA)
116300940728     C     D$DFFD        IFEQ      'S'
116400000411     C     D$DSTP        OREQ      99999
116500990420     C     D$DFIN        OREQ      'I'
116600010116     C                   MOVEL     'S'           VC(532)
116700001206     C                   ENDIF
116800011203     C*
116900011203     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
117000011203IF  1C     D$DFFD        IFEQ      'S'
117100011203     C                   MOVEL     'S'           VC(184)
117200011203E   2C                   ENDIF
117300940714     C*
117400940714     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE PER CONTO
117500941201     C*    SE HA IMPORTO VALORE ASSICURAZIONE X CONTO > ZERO
117600970225     C                   MOVE      W$KSC         KSCIS             4 0
1177009702251    C     KSCIS         IFEQ      8888
117800970225     C     KSCIS         OREQ      9999
1179000705212    C     D$DVAE        ifgt      *ZEROS
118000970225     C                   MOVEL     'S'           VC(291)
118100070521     c                   end
1182009702251    C                   ELSE
1183000112272    C     D$DVAE        IFGT      *ZEROS
118400940728     C                   MOVEL     'S'           VC(290)
118500051017     C                   MOVEL     'S'           VC(292)
1186009702252    C                   ELSE
118700130930     C* FRANCHI EXPORT O ASSEGNATI IMPORT,
118800100408     C                   Z-ADD     W$LNP         F
118900111020     C                   Z-ADD     W$lna         F1
119000130930IF  1C                   if        ((fie(f) = 'E' and D$DTPO ='A') or
119100100408IF  1C                             (fie(f1) = 'E' and D$DTPO ='F'))
119200100408     C                   MOVEL     'S'           VC(286)
119300130930     C* SOLO x Italia
119400100408     c                   else
119500960606     C                   MOVEL     'S'           VC(281)
119600100408     c                   end
119700130930     c* AC BASE  (varia "d" )
119800130930     c                   if        *in44
119900060313     c* cliente top o direzionale
120000060313     c                   select
120100060313     c                   when      wclv = 'T' or wclv = 'D'
120200060313     C                   MOVEL     'S'           VC(293)
120300060313     c* cliente A
120400060313     c                   when      wclv = 'A'
120500060313     C                   MOVEL     'S'           VC(294)
120600060313     c* cliente B o C
120700060313     c                   when      wclv = 'B' or wclv = 'C'
120800060313     C                   MOVEL     'S'           VC(295)
120900060313     C                   endsl
121000060313     C                   endif
1211009702252    C                   ENDIF
1212009702251    C                   ENDIF
121300940722     C*
121400940722     C* CONTROLLO SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
121500970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "A"
121600130930IF  0C     WDDSP         ifle      20181231
121700940722     C     D$DFTC        IFEQ      'A'
121800970228     C     D$DTC2        OREQ      'A'
121900011203IF  3C     D$DFTC        IFEQ      'S'
122000011203     C     D$DTC2        OREQ      'S'
122100011203X   3C                   ELSE
122200011203     C                   MOVEL     'S'           VC(240)
122300011203E   3C                   ENDIF
122400011203     C                   ENDIF
122500110321     c                   else
122600110321     C     D$DFTC        IFEQ      'A'
122700110321     C     D$DTC2        OREQ      'A'
122800110321IF  3C     D$DFTC        OREQ      'S'
122900110321     C     D$DTC2        OREQ      'S'
123000110321     C                   MOVEL     'S'           VC(240)
123100110321     c                   end
123200110321     c                   end
123300940722     C*
123400940722     C* CONTROLLO SE ESISTE =======> CONSEGNA AI SUPERMERCATI
123500970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "S"
123600940722     C     D$DFTC        IFEQ      'S'
123700970228     C     D$DTC2        OREQ      'S'
123800980119     C                   MOVEL     'S'           VC(242)
123900980119     C                   MOVEL     'S'           VC(247)
124000110321     C                   ENDIF
124100940722     C*
124200940722     C* CONTROLLO SE ESISTE =======> CONSEGNA AI PIANI
124300970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "P"
124400940722     C     D$DFTC        IFEQ      'P'
124500970228     C     D$DTC2        OREQ      'P'
124600940727     C                   MOVEL     'S'           VC(241)
124700940722     C                   ENDIF
124800020620     C*
124900020620     C* CONTROLLO SE ESISTE =======> CONSEGNA DISAGIATA
125000020620     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "X"
125100020620     C     D$DFTC        IFEQ      'X'
125200020620     C     D$DTC2        OREQ      'X'
125300020620     C                   MOVEL     'S'           VC(534)
125400020620     C                   MOVEL     'S'           VC(535)
125500020620     C                   ENDIF
125600940722     C*
125700940722     C* CONTROLLO SE ESISTE =======> CONTRASSEGNO
125800940722     C*    IMPORTO CONTRASSEGNO > ZERO
125900941201     C     D$DCAS        IFGT      *ZEROS
126000111020     C                   Z-ADD     W$lna         F
126100060223     c* arrivi
126200020805     C     FIE(F)        IFEQ      'E'
126300000203     C                   MOVEL     'S'           VC(256)
126400000203     C                   MOVEL     'S'           VC(257)
126500000203     C                   MOVEL     'S'           VC(280)
126600000203     C                   ELSE
126700940727     C                   MOVEL     'S'           VC(251)
126800940727     C                   MOVEL     'S'           VC(252)
126900110321     c* abilitazione c/assegno
127000130930IF  0C     WDDSP         ifgt      20181231
127100110321IF  1C     §FLOfca       andeq     'S'
127200110321     C                   MOVEL     'S'           VC(259)
127300000203     C                   ENDIF
127400110321     C                   ENDIF
127500060223     c**** mittente
127600941201     C     D$DTPI        IFEQ      'M'
127700060223     C                   MOVEL     'S'           VC(254)
127800941021     C                   ELSE
127900060223     c**** non mittente
128000941021     C                   MOVEL     'S'           VC(253)
128100941021     C                   ENDIF
128200130930     c*
128300090417     C                   IF        (d$dsop <> '§' and
128400090417     C                             d$dsop <> '%' and
128500130930     C                             d$dsop <> ':' )
128600090417     C                   MOVEL     'S'           VC(250)
128700090417     c                   end
128800940722     C                   ENDIF
128900940728     C*
129000110321     C* CONTROLLO SE ESISTE =======> PORTO ASSEGNATO UFF.ARR.
129100110321     C     D$DTPO        IFEQ      'A'
129200130930IF  2C     wddsp         andgt     20181231
129300110321     C                   MOVEL     'S'           VC(312)
129400110321     C                   ENDIF
129500110321     C* CONTROLLO SE ESISTE =======> PORTO ASSEGNATO UFF.ARR. E PADRONCINO
129600110321     C*    ASSEGNATO CON FATTURA IMMEDIATA
129700941201     C     D$DTPO        IFEQ      'A'
129800030529     C     D$DFIV        ANDGT     0
129900030529     C     D$DFIV        ANDLT     900
130000940728     C                   MOVEL     'S'           VC(310)
130100940728     C                   MOVEL     'S'           VC(311)
130200110321     C                   ENDIF
130300941028     C*
130400990420     C* CONTROLLO SE ESISTE =======> CONSEGNA A MAGAZZINO (RIDUZIONE)
130500941028     C*    FLAG MERCE PORTATA A MAGAZZINO DAL CLIENTE = 'SI'
130600990420     C*    OPPURE ANTEPORTO = I (RITIRO ISOLA)
130700941209     C     D$DFDN        IFEQ      'S'
130800990420     C     D$DFAP        OREQ      'I'
130900010116     C                   MOVEL     'S'           VC(521)
131000001206     C                   ENDIF
131100960214     C*
131200960214     C* CONTROLLO SE ESISTE =======> CONSEGNA A PRIVATI (DAL FLAG BOLLA)
131300960214     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'P'
131400960214     C                   MOVEL     D$DGVA        WTM01             1
131500030422     C*
131600030422     C* CONTROLLO SE ESISTE =======> GESTIONE BANCALI
131700030422     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'B'
131800030422     C                   MOVEL     D$DGVA        WTM01
131900030422     C     WTM01         IFEQ      'B'
132000030422     C                   MOVEL     'S'           VC(17)
132100030422     C                   MOVEL     'S'           VC(119)
132200030422     C                   MOVEL     'S'           VC(142)
132300030422     C                   MOVEL     'S'           VC(186)
132400030430     C                   MOVEL     'S'           VC(213)
132500030422     C                   ENDIF
132600030725     C*
132700030801     C* CONTROLLO SE ESISTE =======> Spedizioni oltre 20 colli bancalatura Coop
132800030801     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'O'
132900030801     C                   MOVEL     D$DGVA        WTM01             1
133000030801     C     WTM01         IFEQ      'O'
133100030801     C                   MOVEL     'S'           VC(34)
133200030801     C                   ENDIF
133300960214     C*
133400960214     C* CONTROLLO SE ESISTE =======> SECONDA LINEA DISTRIBUZIONE
133500030430     C*    SECONDO CARATTERE FLAG PARTICOLARITA' VARIE = 'D' e
133600010116     C*                LINEA ARRIVO = TERMINAL ARRIVO  e
133700010116     C*                C.TASSAZIONE DESTINO BOLLA <> C.TASSAZIONE LINEA ARRIVO
133800001204     C                   MOVE      D$DGVA        WTM01             1
133900001204IF  1C     WTM01         IFEQ      'D'
134000111020IF  3C     W$lna         IFEQ      D$DTFA
134100111020     C                   Z-ADD     W$lna         F
134200010116IF  4C     D$DCTS        IFNE      CTS(F)
134300001201     C                   MOVEL     'S'           VC(165)
134400001201     C                   MOVEL     'S'           VC(178)
134500110321E   4C                   ENDIF
134600010116E   3C                   ENDIF
134700001204E   1C                   ENDIF
134800970429     C*
134900970429     C* CONTROLLO SE ESISTE =======> DDT (DOCUM. DI TRASP. DA CONS. AL DEST.)
135000970429     C*    FLAG LIBERO 1  = 'S' O 'C'
135100970429     C     D$DLL1        IFEQ      'S'
135200970429     C     D$DLL1        OREQ      'C'
135300970429     C                   MOVEL     'S'           VC(72)
135400970429     C                   MOVEL     'S'           VC(182)
135500970429     C                   ENDIF
135600990222     C*
135700990222     C* CONTROLLO SE ESISTE =======> CONSEGNA LOCALITA DISAGIATE
135800990222     C*    SE FLAG INOLTRO 'D' E NON C'E' FERMO DEPOSITO
135900030430     C     D$DFFD        IFNE      'S'
136000030430     C     D$DFIN        ANDEQ     'D'
136100030430     C                   MOVEL     'S'           VC(246)
136200030430     C                   ENDIF
136300980202     C*
136400030430     C* CONTROLLO SE ESISTE =======>
136500030430     C                   MOVEL     'S'           VC(212)
136600980204     C                   MOVEL     'S'           VC(270)
136700010116     C                   MOVEL     'S'           VC(523)
136800010116     C                   MOVEL     'S'           VC(594)
136900021031     C                   MOVEL     'S'           VC(595)
137000000721     C                   MOVEL     'S'           VC(592)
137100000721     C                   MOVEL     'S'           VC(274)
137200001201     C                   MOVEL     'S'           VC(23)
137300001201     C                   MOVEL     'S'           VC(161)
137400001201     C                   MOVEL     'S'           VC(177)
137500980317     C*
137600980318     C* CONTROLLO SE ESISTE =======> AFFLUENZA/DEFLUENZA SU STESSO TERMINAL
137700980317     C*    SE TERM.PART. = TERM.ARR. E LINEA ARR. <> LINEA PART.
137800980318     C     W$TFP         IFEQ      D$DTFA
137900980318     C     W$LNP         ANDNE     W$TFP
138000111020     C     W$lna         ANDNE     D$DTFA
138100980319     C                   MOVEL     'S'           VC(560)
138200980317     C                   ENDIF
138300980317     C*
138400980319     C* CONTROLLO SE ESISTE =======> LINEA PART. = TERM. EXPORT
138500980317     C*    SE TERM.PART. = TERM.ARR. = LINEA PART. E EXPORT
138600111020     C                   Z-ADD     W$lna         F
138700980318     C     W$TFP         IFEQ      D$DTFA
138800980318     C     W$LNP         ANDEQ     W$TFP
138900020805     C     FIE(F)        ANDEQ     'E'
139000980319     C                   MOVEL     'S'           VC(561)
139100980317     C                   ENDIF
139200980717     C*
139300980717     C* CONTROLLO SE ESISTE =======> LINEA PART. E' 2°LIV. TERM. EXPORT
139400980717     C*    SE TERM.PART.=TERM.ARR. E LINEA PART.<>TERM.PART. E EXPORT
139500111020     C                   Z-ADD     W$lna         F
139600980717     C     W$TFP         IFEQ      D$DTFA
139700980717     C     W$LNP         ANDNE     W$TFP
139800020805     C     FIE(F)        ANDEQ     'E'
139900980717     C                   MOVEL     'S'           VC(570)
140000980717     C                   ENDIF
140100980717     C*
140200980717     C* CONTROLLO SE ESISTE =======> LINEA ARR. E' 2°LIV. TERM. IMPORT
140300980717     C*    SE TERM.PART.=TERM.ARR. E LINEA ARR.<>TERM.ARR. E IMPORT
140400980717     C                   Z-ADD     W$LNP         F
140500980717     C     W$TFP         IFEQ      D$DTFA
140600111020     C     W$lna         ANDNE     D$DTFA
140700020805     C     FIE(F)        ANDEQ     'E'
140800980717     C                   MOVEL     'S'           VC(571)
140900980717     C                   ENDIF
141000990217     C*
141100990420     C* CONTROLLO SE ESISTE =======> CONSEGNA ISOLA
141200990219     C*    SE FLAG INOLTRO 'I' E NON C'E' FERMO DEPOSITO
141300111020     C                   Z-ADD     W$lna         F
141400000302IF  1C     D$DFFD        IFNE      'S'
141500990219     C     D$DFIN        ANDEQ     'I'
141600020805IF  2C     FIE(F)        IFEQ      'E'
141700020805IF  3C     NTW(F)        IFEQ      'DPD'
141800000302     C                   MOVEL     'S'           VC(288)
141900000302E   3C                   ENDIF
142000000302X   2C                   ELSE
142100000302     C                   MOVEL     'S'           VC(278)
142200000302E   2C                   ENDIF
142300000302E   1C                   ENDIF
142400990217     C*
142500990420     C* CONTROLLO SE ESISTE =======> RITIRO ISOLA
142600990217     C*    SE FLAG ANTEPORTO 'I' E NON C'E' CONSEGNA A MAGAZZINO
142700000302     C                   Z-ADD     W$LNP         F
142800000302IF  1C     D$DFDN        IFNE      'S'
142900990219     C     D$DFAP        ANDEQ     'I'
143000020805IF  2C     FIE(F)        IFEQ      'E'
143100020805IF  3C     NTW(F)        IFEQ      'DPD'
143200000302     C                   MOVEL     'S'           VC(289)
143300000302E   3C                   ENDIF
143400000302X   2C                   ELSE
143500990420     C                   MOVEL     'S'           VC(279)
143600130930     c* COMP.RITIRI ISOLE VENEZIA
143700040617     C                   MOVEL     'S'           VC(285)
143800000302E   2C                   ENDIF
143900000302E   1C                   ENDIF
144000990217     C*
144100990217     C* CONTROLLO SE ESISTE =======> PREPAGATI
144200990217     C*    SE CLIENTE 8888
144300990217     C                   MOVE      W$KSC         KSCIS             4 0
1444009902171    C     KSCIS         IFEQ      8888
144500990217     C                   MOVEL     'S'           VC(266)
1446009902171    C                   ENDIF
144700020621     C*
144800020621E  1aC                   ENDIF
144900000623E   0C                   ENDIF
145000000630     C*
145100940526     C                   ENDSR
145200940908     C*------------------------------------------------------------------------*
145300940908     C*?CAL2R  - CALCOLA LE QTA DELLA TABELLA 2R (TIPO TARIFFA)
145400940908     C*------------------------------------------------------------------------*
145500940908     C     CAL2R         BEGSR
145600940908     C* TIPO -01- COLLI -------------------------------------------------------*
145700940908     C                   Z-ADD     1             I
145800940908     C     '01'          LOOKUP    C2R(I)                                 98
145900940908     C     *IN98         IFEQ      '0'
146000940908     C                   MOVEL     MSG(5)        $MSG
146100940908     C                   EXSR      SCRERR
146200940908     C                   ELSE
146300940908     C                   Z-ADD     D$DNCL        Q2R(I)
146400940908     C                   ENDIF
146500940908     C*
146600940908     C* TIPO -02- SPEDIZIONI --------------------------------------------------*
146700940908     C                   Z-ADD     1             I
146800940908     C     '02'          LOOKUP    C2R(I)                                 98
146900940908     C     *IN98         IFEQ      '0'
147000940908     C                   MOVEL     MSG(6)        $MSG
147100940908     C                   EXSR      SCRERR
147200940908     C                   ELSE
147300940908     C                   Z-ADD     1             Q2R(I)
147400940908     C                   ENDIF
147500940908     C*
147600940908     C* TIPO -03- CHILOGRAMMI -------------------------------------------------*
147700040428     C* SE I COLLI=COLLI CML PRENDO PESO CML
147800040428IF  2C     D$DPKC        IFGT      *ZEROS
147900040428     C     D$DNCL        ANDEQ     D$DNCP
148000040428     C                   Z-ADD     D$DPKC        WPES
148100040428X   2C                   ELSE
148200040428     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
148300040428IF  3C     D$DPKF        IFGT      D$DPKC
148400040428     C                   Z-ADD     D$DPKF        WPES
148500040428X   3C                   ELSE
148600040428     C                   Z-ADD     D$DPKC        WPES
148700040428E   3C                   ENDIF
148800040428E   2C                   ENDIF
148900130930     c*
149000940908     C                   Z-ADD     1             I
149100940908     C     '03'          LOOKUP    C2R(I)                                 98
149200940908     C     *IN98         IFEQ      '0'
149300940908     C                   MOVEL     MSG(7)        $MSG
149400940908     C                   EXSR      SCRERR
149500940908     C                   ELSE
149600991008     C                   Z-ADD     WPES          Q2R(I)
149700940908     C                   ENDIF
149800940908     C*
149900940908     C* TIPO -04- VOLUME ------------------------------------------------------*
150000990422     C* SE VOLUME FATTURATO E' 'T' (TOTALE COLLI) PRENDO QUELLO
150100990421IF  1C     D$DFVF        IFEQ      'T'
150200990421     C                   Z-ADD     D$DVLF        WVOL
150300990421X   1C                   ELSE
150400990422     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
150500990421IF  2C     D$DVLC        IFGT      *ZEROS
150600990421     C     D$DNCL        ANDEQ     D$DNCR
150700990422     C                   Z-ADD     D$DVLC        WVOL
150800990421X   2C                   ELSE
150900990422     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
151000990422IF  3C     D$DVLF        IFGT      D$DVLC
151100990422     C                   Z-ADD     D$DVLF        WVOL
151200990421X   3C                   ELSE
151300990422     C                   Z-ADD     D$DVLC        WVOL
151400990421E   3C                   ENDIF
151500990421E   2C                   ENDIF
151600990421E   1C                   ENDIF
151700990421     C*
151800940908     C                   Z-ADD     1             I
151900940908     C     '04'          LOOKUP    C2R(I)                                 98
152000940908     C     *IN98         IFEQ      '0'
152100940908     C                   MOVEL     MSG(8)        $MSG
152200940908     C                   EXSR      SCRERR
152300940908     C                   ELSE
152400940908     C                   Z-ADD     WVOL          Q2R(I)
152500940908     C                   ENDIF
152600030409     C*
152700030409     C* TIPO -06- BANCALI -----------------------------------------------------*
152800030422     C                   CLEAR                   WBAN              5 0
152900030422     C                   EVAL      WBAN = D$DAR5NBA + D$DAR5NB2
153000030422     C                   Z-ADD     1             I
153100030422     C     '06'          LOOKUP    C2R(I)                                 98
153200030422     C     *IN98         IFEQ      '0'
153300030422     C                   MOVEL     MSG(35)       $MSG
153400030422     C                   EXSR      SCRERR
153500030422     C                   ELSE
153600030422     C                   Z-ADD     WBAN          Q2R(I)
153700030422     C                   ENDIF
153800940908     C*
153900951213     C* TIPO -09- STOP RITIRO -------------------------------------------------*
154000940908     C                   Z-ADD     1             I
154100940908     C     '09'          LOOKUP    C2R(I)                                 98
154200940908IF  1C     *IN98         IFEQ      '0'
154300940908     C                   MOVEL     MSG(9)        $MSG
154400940908     C                   EXSR      SCRERR
154500940908X   1C                   ELSE
154600941209     C                   Z-ADD     D$DNSR        Q2R(I)
154700940908E   1C                   ENDIF
154800951213     C*
154900951213     C* TIPO -10- STOP CONSEGNA -----------------------------------------------*
155000951213     C                   Z-ADD     1             I
155100951213     C     '10'          LOOKUP    C2R(I)                                 98
155200951213IF  1C     *IN98         IFEQ      '0'
155300951213     C                   MOVEL     MSG(16)       $MSG
155400951213     C                   EXSR      SCRERR
155500951213X   1C                   ELSE
155600951213     C                   Z-ADD     D$DNSC        Q2R(I)
155700951213E   1C                   ENDIF
155800991029     C*
155900991029     C* TIPO -11- PESO X GIORNI GIACENZA --------------------------------------*
156000991029     C                   Z-ADD     1             I
156100991029     C     '11'          LOOKUP    C2R(I)                                 98
156200991029IF  1C     *IN98         IFEQ      '0'
156300991029     C                   MOVEL     MSG(17)       $MSG
156400991029     C                   EXSR      SCRERR
156500991029X   1C                   ELSE
156600991029     C     D$DGGS        IFEQ      *ZEROS
156700991029     C                   Z-ADD     1             WT0155           15 5
156800991029     C                   ELSE
156900991029     C                   Z-ADD     D$DGGS        WT0155
157000991029     C                   ENDIF
157100991029     C     WPES          MULT      WT0155        Q2R(I)
157200991029E   1C                   ENDIF
157300000201     C*
157400000201     C* TIPO -12- GIORNI GIACENZA ---------------------------------------------*
157500000201     C                   Z-ADD     1             I
157600000201     C     '12'          LOOKUP    C2R(I)                                 98
157700000201IF  1C     *IN98         IFEQ      '0'
157800000201     C                   MOVEL     MSG(25)       $MSG
157900000201     C                   EXSR      SCRERR
158000000201X   1C                   ELSE
158100000201IF  2C     D$DGGS        IFEQ      *ZEROS
158200000201     C                   Z-ADD     1             Q2R(I)
158300000201X   2C                   ELSE
158400000201     C                   Z-ADD     D$DGGS        Q2R(I)
158500000201E   2C                   ENDIF
158600000201E   1C                   ENDIF
158700940908     C*
158800940908     C* TIPO -21- IMPONIBILE REALE --------------------------------------------*
158900940908     C                   Z-ADD     1             I
159000940908     C     '21'          LOOKUP    C2R(I)                                 98
159100940908     C     *IN98         IFEQ      '0'
159200940908     C                   MOVEL     MSG(11)       $MSG
159300940908     C                   EXSR      SCRERR
159400940908     C                   ELSE
159500940908     C* CERCO NELLA SCHIERA IL INPUT AL PGM L'IMPONIBILE REALE
159600940908     C                   Z-ADD     1             II                9 0
159700940908     C     556           LOOKUP    RCD(II)                                98
159800940908     C     *IN98         IFEQ      '1'
159900940908     C                   Z-ADD     RIM(II)       Q2R(I)
160000940908     C                   ENDIF
160100940908     C                   ENDIF
160200940908     C*
160300940908     C* TIPO -22- CONTRASEGNO -------------------------------------------------*
160400940908     C                   Z-ADD     1             I
160500940908     C     '22'          LOOKUP    C2R(I)                                 98
160600940908     C     *IN98         IFEQ      '0'
160700940908     C                   MOVEL     MSG(12)       $MSG
160800940908     C                   EXSR      SCRERR
160900940908     C                   ELSE
161000941201     C                   Z-ADD     D$DCAS        Q2R(I)
161100940908     C                   ENDIF
161200940908     C*
161300940908     C* TIPO -23- VALORE MERCE ASSICURATA -------------------------------------*
161400940908     C                   Z-ADD     1             I
161500940908     C     '23'          LOOKUP    C2R(I)                                 98
161600940908     C     *IN98         IFEQ      '0'
161700940908     C                   MOVEL     MSG(13)       $MSG
161800940908     C                   EXSR      SCRERR
161900940908     C                   ELSE
162000011227     C                   Z-ADD     D$DVAE        Q2R(I)
162100940908     C                   ENDIF
162200940908     C*
162300940908     C* TIPO -24- NUMERO DI GIORNI PAGAMENTO ----------------------------------*
162400940908     C                   Z-ADD     1             I
162500940908     C     '24'          LOOKUP    C2R(I)                                 98
162600940908     C     *IN98         IFEQ      '0'
162700940908     C                   MOVEL     MSG(14)       $MSG
162800940908     C                   EXSR      SCRERR
162900940908     C                   ELSE
163000940908     C                   Z-ADD     WGGP          Q2R(I)
163100940908     C                   ENDIF
163200940908     C*
163300940908     C                   ENDSR
163400940803     C*------------------------------------------------------------------------*
163500940803     C*? CHKVOC - CONTROLLA CHE TUTTE LE VOCI DA CALCOLARE SIANO VALIDE
163600940803     C*            ALLA DATA DELLA BOLLA ED EVENTUALMENTE CARICA LE GIUSTE
163700940803     C*            DECORRENZE
163800940803     C*            INPUT: - WDDSP (DATA SPEDIZIONE BOLLA)
163900940803     C*            OUTPUT:- SCHIERE VOCI COMPLETE ALLA DATA IN INPUT
164000940803     C*------------------------------------------------------------------------*
164100940603     C     CHKVOC        BEGSR
164200940603     C*
164300940607     C* CICLO PER TUTTE LE VOCI DA CALCOLARE
1644009406071    C                   DO        999           V
1645009406072    C     VC(V)         IFEQ      'S'
1646009407293    C     WDDSP         IFLT      VDDV(V)
164700940729     C     WDDSP         ORGT      VDSV(V)
164800940630     C                   Z-ADD     *ZEROS        VPRG(V)
164900940603     C                   MOVEL     *BLANKS       VTVO(V)
165000940704     C                   Z-ADD     *ZEROS        VDDV(V)
165100940704     C                   Z-ADD     *ZEROS        VDSV(V)
165200940603     C                   MOVEL     *BLANKS       VVRV(V)
165300940603     C                   MOVEL     *BLANKS       VTIV(V)
165400940603     C                   MOVEL     *BLANKS       VSPT(V)
165500940603     C                   Z-ADD     *ZEROS        VCMP(V)
165600940607     C                   MOVEL     *BLANKS       VRFI(V)
165700940607     C                   MOVEL     *BLANKS       VRFE(V)
165800940607     C                   MOVEL     *BLANKS       VRTI(V)
165900940607     C                   MOVEL     *BLANKS       VRTE(V)
166000940607     C                   MOVEL     *BLANKS       VRPI(V)
166100940607     C                   MOVEL     *BLANKS       VRPE(V)
166200940607     C                   MOVEL     *BLANKS       VRKI(V)
166300940607     C                   MOVEL     *BLANKS       VRKE(V)
166400940704     C                   Z-ADD     *ZEROS        VARF(V)
166500940704     C                   Z-ADD     *ZEROS        VARL(V)
166600940704     C                   Z-ADD     *ZEROS        VARO(V)
166700030113     C                   MOVEL     *BLANKS       VCOP(V)
166800030113     C                   MOVEL     *BLANKS       VDAT(V)
166900030113     C                   MOVEL     *BLANKS       VAID(V)
167000030422     C                   Z-ADD     *ZEROS        VRPV(V)
167100091009     C                   MOVEL     *BLANKS       Vric(V)
167200091009     C                   Z-ADD     *ZEROS        Vfil(V)
167300091009     C                   Z-ADD     *ZEROS        Vqta(V)
167400091009     C                   Z-ADD     *ZEROS        Vsgi(V)
167500091009     C                   Z-ADD     *ZEROS        Vsgf(V)
167600030422     C                   MOVEL     *BLANKS       VSPS(V)
167700940607     C                   MOVEL     'N'           VC(V)
167800940607     C* CERCO IL RECORD DELLA VOCE CON DEC/SCAD PER LA DATA BOLLA
167900940607     C                   Z-ADD     V             EVTVOC
168000940630     C                   Z-ADD     *ZEROS        EVTPRG
168100940603     C     KEYEVT        SETLL     ECEVT01L                           99
168200940603     C  N99              READ      ECEVT01L                               99
168300940607     C     *IN99         IFEQ      '1'
168400940829     C     EVTVOC        ORNE      V
168500991119     C                   MOVEL     MSG(18)       DSMSG
168600991119     C                   MOVE      V             DSVOCE
168700991119     C                   MOVEL     DSMSG         $MSG
168800940907     C                   EXSR      SCRERR
168900940607     C                   END
1690009407044    C     *IN99         DOWEQ     '0'
169100940607     C     EVTVOC        ANDEQ     V
169200940704     C     VC(V)         ANDEQ     'N'
1693009407045    C     EVTATB        IFEQ      *BLANKS
169400940729     C     WDDSP         ANDGE     EVTDDV
169500940729     C     WDDSP         ANDLE     EVTDSV
169600940603     C                   Z-ADD     EVTVOC        V
169700940630     C                   Z-ADD     EVTPRG        VPRG(V)
169800940607     C                   MOVEL     EVTTVO        VTVO(V)
169900940704     C                   Z-ADD     EVTDDV        VDDV(V)
170000940704     C                   Z-ADD     EVTDSV        VDSV(V)
170100090304     c* per pod immage forzo colli invece di spedizione, perchè non si
170200090304     C* vuole cambiare il tipo tariffa, la spedizione sarà una e i
170300090304     c* colli sono le immagini
170400090304     c                   if        v = 199
170500090304     C                   MOVEL     '01'          VVRV(V)
170600090304     c                   if        evttiv <> ' '
170700090304     C                   MOVEL     '01'          VTIV(V)
170800090304     c                   end
170900090304     c                   else
171000940607     C                   MOVEL     EVTVRV        VVRV(V)
171100940607     C                   MOVEL     EVTTIV        VTIV(V)
171200090304     c                   end
171300940607     C                   MOVEL     EVTSPT        VSPT(V)
171400940607     C                   Z-ADD     EVTCMP        VCMP(V)
171500940607     C                   MOVEL     EVTRFI        VRFI(V)
171600940607     C                   MOVEL     EVTRFE        VRFE(V)
171700940607     C                   MOVEL     EVTRTI        VRTI(V)
171800940607     C                   MOVEL     EVTRTE        VRTE(V)
171900940607     C                   MOVEL     EVTRPI        VRPI(V)
172000940607     C                   MOVEL     EVTRPE        VRPE(V)
172100940607     C                   MOVEL     EVTRKI        VRKI(V)
172200940607     C                   MOVEL     EVTRKE        VRKE(V)
172300940704     C                   Z-ADD     EVTARF        VARF(V)
172400940704     C                   Z-ADD     EVTARL        VARL(V)
172500940704     C                   Z-ADD     EVTARO        VARO(V)
172600030113     C                   MOVEL     EVTCOP        VCOP(V)
172700030113     C                   MOVEL     EVTDAT        VDAT(V)
172800030113     C                   MOVEL     EVTAID        VAID(V)
172900030422     C                   MOVEL     EVTXXX        DEVTXXX
173000030422     C                   Z-ADD     EVTRPV        VRPV(V)
173100030422     C                   MOVEL     EVTSPS        VSPS(V)
173200940607     C                   MOVEL     'S'           VC(V)
173300091009     c* recupero la filiale di competenza voce
173400091009     C     KEYECT        SETLL     ECECT01L
173500091009     c                   do        *hival
173600091009     C     KEYECT        READE     ECECT01L
173700091009     c                   if        %eof(ecect01l)
173800091009     c                   leave
173900091009     c                   end
174000091009IF  4C     ECTATB        IFne      *BLANKS
174100091009     C     WDDSP         orlt      EcTDDc
174200091009     C     WDDSP         orgt      EcTDSc
174300091009     c                   iter
174400091009     c                   end
174500091009     c                   eval      vric(v) = ectric
174600091009     c                   leave
174700091009     c                   enddo
1748009407045    C                   ENDIF
174900940603     C                   READ      ECEVT01L                               99
1750009407044    C                   ENDDO
1751009407043    C                   ENDIF
1752009407042    C                   ENDIF
1753009406071    C                   ENDDO
175400940603     C*
175500940603     C                   ENDSR
175600940803     C*------------------------------------------------------------------------*
175700940803     C*? VALVOC - VALORIZZA LE VOCI VALIDE
175800940803     C*------------------------------------------------------------------------*
175900940526     C     VALVOC        BEGSR
176000940526     C*
176100940603     C* CICLO PER TUTTE LE VOCI 'DA CALCOLARE'
176200940603     C                   DO        999           V
176300940607     C     VC(V)         IFEQ      'S'
176400940630     C                   Z-ADD     V             WVOC
176500940630     C                   Z-ADD     VPRG(V)       WPRG
176600000306     C                   MOVEL     VTIV(V)       WTIV
176700940607     C* CERCA TARIFFA
176800940607     C                   EXSR      CERTAR
176900940729     C*
177000940908     C* SE NON C'E' ERRORE (ES. MANCA TARIFFA)
177100940908     C* E IL MANCA TARIFFA ERA DA SEGNALARE
177200940913     C     $ERR          IFEQ      'N'
177300940908     C     WMSGMT        ANDEQ     'S'
177400940720     C*
177500940720     C* CALCOLA QTA
177600941107     C*
177700941107     C* SE TIPO VALORIZZAZIONE = Q=QUANTITA  OPPURE P=PERCENTUALE
177800941107     C* RECUPERO LA QTA O IL VALORE DI RIFERIMENTO
177900020109     C                   Z-ADD     *ZEROS        WQTA             21 9
178000940701     C     VVRV(V)       IFNE      *BLANKS
178100940721     C                   Z-ADD     1             I
178200940701     C     VVRV(V)       LOOKUP    C2R(I)                                 98
178300940701     C     *IN98         IFEQ      '0'
178400940720     C                   MOVEL     MSG(19)       $MSG
178500940907     C                   EXSR      SCRERR
178600940729     C                   ELSE
178700941107     C* SE E' UNA TARIFFA DOVE E' STATO CALCOLATO IL PESO TASSABILE
178800941107     C* (GIA' CALCOLATO NELLA RICERCA DELLO SCAGLIONE) PRENDO QTA
178900941107     C* UTILIZZATA PER LA RICERCA DELLO SCAGLIONE
179000941107     C* ALTRIMENTI PRENDO QTA DELLA SCHIERA
179100941107     C     VSPT(V)       IFNE      *BLANKS
179200961202     C     WQTAPT        ANDGT     *ZEROS
179300941107     C                   Z-ADD     WQTAPT        WQTA
179400941107     C                   ELSE
179500100422     c* per tipo voce = importo voce x isolamento
179600100423     c                   if        vvrv(v) <> '26'
179700100423     C                   Z-ADD     Q2R(I)        WQTA
179800100423     c                   else
179900100422     C                   Z-ADD     vimp(v)       WQTA
180000100422     c                   end
180100941107     C                   ENDIF
180200940729     C                   MOVEL     T2R(I)        WTTQ              1
180300940720     C                   ENDIF
180400940721     C                   ENDIF
180500940803     C*
180600940803     C* CONTROLLO L'ARROTONDAMENTO
180700940803     C                   EXSR      ARROTO
180800940720     C*
180900020109     C                   Z-ADD     *ZEROS        WVVOCE           21 9
181000090914     C                   Z-ADD     *ZEROS        WVVO25           15 5
181100100506     c*
181200121120     c                   if        D$GFSF = 'S'
181300091009     c                   exsr      sresvoca
181400121120     c                   end
181500940802     C*
181600940802     C* SE TIPO VOCE V=VOCE NORMALE
181700940802     C* IN BASE AL TIPO TARIFFA CALCOLA IMPORTO A QTA, FISSO, PERCENTUALE
181800940802     C     VTVO(V)       IFEQ      'V'
181900940802     C                   EXSR      CALVOC
182000940802     C                   ENDIF
182100940720     C*
182200940802     C* SE TIPO VOCE M=MAGGIORAZIONE
182300940802     C* RECUPERO TUTTE LE VOCI DA MAGGIORARE E APPLICO LA PERCENTUALE
182400940802     C     VTVO(V)       IFEQ      'M'
182500940906     C                   EXSR      CERMAG
182600940802     C                   ENDIF
182700940802     C*
182800940802     C* CARICO TIPO TARIFFA -> IMPONIBILE PER COMPETENZE SEDE (SOMMA DI TUTTE
182900991027     C* LE VOCI PRECEDENTI TRANNE I TRANSITI)
183000991027     C     VS(V)         IFEQ      'S'
183100940802     C                   Z-ADD     1             I
183200940802     C     '25'          LOOKUP    C2R(I)                                 98
183300940802     C     *IN98         IFEQ      '0'
183400940802     C                   MOVEL     MSG(15)       $MSG
183500940907     C                   EXSR      SCRERR
183600940802     C                   ELSE
183700940824     C                   ADD       WVVO25        Q2R(I)
183800940802     C                   ENDIF
183900991027     C                   ENDIF
184000940707     C*
184100940721     C                   ENDIF
184200940729     C                   ENDIF
184300940721     C                   ENDDO
184400940603     C*
184500091007     C                   ENDSR
184600091007     C*------------------------------------------------------------------------*
184700100518     C*? sresvoc10 saldi x tempi uffici
184800091007     C*------------------------------------------------------------------------*
184900100518     C     sresvoc10     BEGSR
185000091009     c* memorizzo in schiera qta scaglioni e filiale  nel caso dovessi
185100091009     c* sottrarre la qta
185200091009     c                   select
185300100518     C     vric(i)       WHENEQ    'LA'
185400111020     C                   Z-ADD     W$lna         v10fil
185500100518     C     vric(i)       WHENEQ    'LP'
185600100518     C                   Z-ADD     W$LNP         v10fil
185700100518     C     vric(i)       WHENEQ    'TA'
185800100518     C                   Z-ADD     D$DTFA        v10fil
185900100518     C     vric(i)       WHENEQ    'TP'
186000100518     C                   Z-ADD     W$TFP         v10fil
186100100518     C     vric(i)       WHENEQ    'T1'
186200100518     C                   Z-ADD     D$DAR5flp     v10fil
186300100518     C     vric(i)       WHENEQ    'T2'
186400100518     C                   Z-ADD     D$DAR5fl2     v10fil
186500100518     C     vric(i)       WHENEQ    'T3'
186600100518     C                   Z-ADD     D$DAR5fl3     v10fil
186700100518     C     vric(i)       WHENEQ    'T4'
186800100518     C                   Z-ADD     D$DAR5fl4     v10fil
186900100518     C     vric(i)       WHENEQ    'T5'
187000100518     C                   Z-ADD     D$DAR5fl5     v10fil
187100100506     c* se blanks forzo sede
187200100506     c                   other
187300100518     C                   Z-ADD     46            v10fil
187400091009     c                   endsl
187500100518     c* aggancio il file saldi esvoc10f, se trovo sommo altrimenti scrivo
187600100518     c                   eval      v10dat = wdsp
187700100518     c                   eval      v10voc = i
187800100518     c     keyes10       chain     esvoc11l
187900091012     c*
188000100518     c                   if        not %found(esvoc11l)
188100100518     c                   eval      v10imp = vimp(i)
188200100518     c                   write     esvoc010
188300091007     c                   else
188400100518     c                   add       vimp(i)       v10imp
188500100518     c                   update    esvoc010
188600091007     c                   end
188700091007     c*
188800940526     C                   ENDSR
188900091009     C*------------------------------------------------------------------------*
189000110309     C*? sresvocs- sottraggo la qta dai saldi perchè importo voce = 0 oppure
189100110309     C*? aggiungo l'importo se importo > 0
189200091009     C*------------------------------------------------------------------------*
189300091009     C     sresvocs      BEGSR
189400110309     c* aggancio il file saldi esvoc00F x sottrarre qta o per aggiungere
189500110309     c* importo
189600091012     c                   eval      vocfil = vfil(i)
189700091009     c                   eval      vocdat = wdsp
189800091012     c                   eval      vocvoc = i
189900091012     c                   eval      vocsgi = vsgi(i)
190000091012     c                   eval      vocsgf = vsgf(i)
190100091009     c     keyes         chain     esvoc01l
190200091009     c                   if        %found(esvoc01l)
190300110309     c                   if        vimp(i) = 0
190400091012     c                   sub       vqta(i)       vocqta
190500091012     c                   sub       1             vocnsp
190600091012     C                   sub       D$DNCL        vocncl
190700091016     c* non aggiorna il numero dei bancali BNB se  voce 34 e DISK B
190800091016     c* (equivalente a serie > 0) e particolarità consegne bancali
190900091016if  1c                   IF        i = 34 and
191000091016     c                             d$dnrs > 0 and
191100091016     c                              %subst(d$dgva: 1: 1) = 'O'
191200091016     c                   else
191300091016     c                   sub       banvoc        vocban
191400091016e   1c                   endif
191500091012     c                   sub       pkfvoc        vocpkf
191600091012     c                   sub       vlfvoc        vocvlf
191700110309     c                   else
191800110309     c* imposto l'importo
191900110309     c                   add       vimp(i)       vocimp
192000110309     c                   end
192100091009     c                   update    esvoc000
192200091009     c                   end
192300091009     c*
192400091009     C                   ENDSR
192500100518     C*------------------------------------------------------------------------*
192600100518     C*? sresvoca  - saldi x tempi uffici
192700100518     C*------------------------------------------------------------------------*
192800100518     C     sresvoca      BEGSR
192900100518     c* memorizzo in schiera importo voce per filiale
193000100518     c                   eval      vqta(v)= wqta
193100100518     c                   eval      vsgi(v) = evdsgi
193200100518     c                   eval      vsgf(v) = evdsgf
193300100518     c                   select
193400100518     C     vric(v)       WHENEQ    'LA'
193500111020     C                   Z-ADD     W$lna         vfil(v)
193600100518     C     vric(v)       WHENEQ    'LP'
193700100518     C                   Z-ADD     W$LNP         vfil(v)
193800100518     C     vric(v)       WHENEQ    'TA'
193900100518     C                   Z-ADD     D$DTFA        vfil(v)
194000100518     C     vric(v)       WHENEQ    'TP'
194100100518     C                   Z-ADD     W$TFP         vfil(v)
194200100518     C     vric(v)       WHENEQ    'T1'
194300100518     C                   Z-ADD     D$DAR5flp     vfil(v)
194400100518     C     vric(v)       WHENEQ    'T2'
194500100518     C                   Z-ADD     D$DAR5fl2     vfil(v)
194600100518     C     vric(v)       WHENEQ    'T3'
194700100518     C                   Z-ADD     D$DAR5fl3     vfil(v)
194800100518     C     vric(v)       WHENEQ    'T4'
194900100518     C                   Z-ADD     D$DAR5fl4     vfil(v)
195000100518     C     vric(v)       WHENEQ    'T5'
195100100518     C                   Z-ADD     D$DAR5fl5     vfil(v)
195200100518     c* se blanks forzo sede
195300100518     c                   other
195400100518     C                   Z-ADD     46            vfil(v)
195500100518     c                   endsl
195600100518     c* aggancio il file saldi esvoc00f, se trovo sommo altrimenti scrivo
195700100518     c                   eval      vocfil = vfil(v)
195800100518     c                   eval      vocdat = wdsp
195900100518     c                   eval      vocvoc = v
196000100518     c                   eval      vocsgi = evdsgi
196100100518     c                   eval      vocsgf = evdsgf
196200100518     c                   eval      vocvrv = vvrv(v)
196300100518     c                   clear                   vocqta
196400100518     c                   clear                   vocnsp
196500100518     c                   clear                   vocncl
196600100518     c                   clear                   vocpkf
196700100518     c                   clear                   vocvlf
196800100518     c                   clear                   vocban
196900110311     c                   clear                   vocimp
197000100518     c     keyes         chain     esvoc01l
197100100518     C     WTTQ          IFEQ      'F'
197200100518     c                   add       1             vocqta
197300100518     c                   else
197400100518     c                   add       wqta          vocqta
197500100518     c                   end
197600100518     c                   add       1             vocnsp
197700100518     C                   add       D$DNCL        vocncl
197800100518     c* non aggiorna il numero dei bancali BNB se  voce 34 e DISK B
197900100518     c* (equivalente a serie > 0) e particolarità consegne bancali
198000100518if  1c                   IF        v = 34 and
198100100518     c                             d$dnrs > 0 and
198200100518     c                              %subst(d$dgva: 1: 1) = 'O'
198300100518     c                   else
198400100518     c                   add       banvoc        vocban
198500100518e   1c                   endif
198600100518     c                   add       pkfvoc        vocpkf
198700100518     c                   add       vlfvoc        vocvlf
198800100518     c*
198900100518     c                   if        not %found(esvoc01l)
199000100518     c                   write     esvoc000
199100100518     c                   else
199200100518     c                   update    esvoc000
199300100518     c                   end
199400100518     c*
199500100518     C                   ENDSR
199600940803     C*------------------------------------------------------------------------*
199700091012     C*? srqta  - CALCOLA LE qta della spedizione
199800940803     C*------------------------------------------------------------------------*
199900091012     C     srqta         BEGSR
200000091012     c* peso
200100091012IF  2C     D$DPKC        IFGT      *ZEROS
200200091012     C     D$DNCL        ANDEQ     D$DNCP
200300091012     C                   z-add     D$DPKC        Pkfvoc
200400091012X   2C                   ELSE
200500091012IF  3C     D$DPKF        IFGT      D$DPKC
200600091012     C                   z-add     D$DPKF        Pkfvoc
200700091012X   3C                   ELSE
200800091012     C                   z-add     D$DPKC        Pkfvoc
200900091012E   3C                   ENDIF
201000091012E   2C                   ENDIF
201100091012     c* volume
201200091012IF  1C     D$DFVF        IFEQ      'T'
201300091012     C                   z-add     D$DVLF        vlfvoc
201400091012X   1C                   ELSE
201500091012IF  2C     D$DVLC        IFGT      *ZEROS
201600091012     C     D$DNCL        ANDEQ     D$DNCR
201700091012     C                   z-add     D$DVLC        vlfvoc
201800091012X   2C                   ELSE
201900091012IF  3C     D$DVLF        IFGT      D$DVLC
202000091012     C                   z-add     D$DVLF        vlfvoc
202100091012X   3C                   ELSE
202200091012     C                   z-add     D$DVLC        vlfvoc
202300091012E   3C                   ENDIF
202400091012E   2C                   ENDIF
202500091012E   1C                   ENDIF
202600091012     C                   eval      banvoc= D$DAR5NBA + D$DAR5NB2
202700091012     C                   ENDSR
202800091012     C*------------------------------------------------------------------------*
202900091012     C*? CALVOC - CALCOLA LE VOCI NORMALI
203000091012     C*------------------------------------------------------------------------*
203100091012     C     CALVOC        BEGSR
203200011227     C*
203300011227     C* CONVERTE GLI IMPORTI DELLE TARIFFE SE LA DIVISA DELLE ANAGRAFICHE C/E E'
203400011227     C* DIVERSA DALLA DIVISA DI OUTPUT DEL C/E
203500011227     C                   Z-ADD     EVDITR        W$ITR
203600011227     C                   Z-ADD     EVDIMI        W$IMI
203700011227     C                   Z-ADD     EVDIMA        W$IMA
203800011227IF  1C     WDIVAC        IFNE      WDIVCE
203900011227IF  2C     EVDITR        IFNE      *ZEROS
204000011227     C                   Z-ADD     EVDITR        §IMI
204100011227     C                   MOVEL     WDIVAC        §DVI
204200011227     C                   EXSR      CNVEUR
204300011227     C                   Z-ADD     §IMO          W$ITR
204400011227E   2C                   ENDIF
204500011227IF  2C     EVDIMI        IFNE      *ZEROS
204600011227     C                   Z-ADD     EVDIMI        §IMI
204700011227     C                   MOVEL     WDIVAC        §DVI
204800011227     C                   EXSR      CNVEUR
204900011227     C                   Z-ADD     §IMO          W$IMI
205000011227E   2C                   ENDIF
205100011227IF  2C     EVDIMA        IFNE      *ZEROS
205200011227     C                   Z-ADD     EVDIMA        §IMI
205300011227     C                   MOVEL     WDIVAC        §DVI
205400011227     C                   EXSR      CNVEUR
205500011227     C                   Z-ADD     §IMO          W$IMA
205600011227E   2C                   ENDIF
205700011227E   1C                   ENDIF
205800940802     C*
205900940802     C* CALCOLO IN BASE AL TIPO TARIFFA
206000940802     C*
206100940802     C     WTTQ          IFEQ      'Q'
206200940802     C     WQTA          ANDNE     *ZEROS
206300011227     C     W$ITR         ANDNE     *ZEROS
206400011227     C     WQTA          MULT      W$ITR         WVVOCE
206500940802     C                   ENDIF
206600940802     C*
206700940802     C     WTTQ          IFEQ      'F'
206800011227     C                   Z-ADD     W$ITR         WVVOCE
206900940802     C                   ENDIF
207000940802     C*
207100940802     C     WTTQ          IFEQ      'P'
207200940802     C     WQTA          ANDNE     *ZEROS
207300940802     C     EVDPER        ANDNE     *ZEROS
207400940802     C     WQTA          DIV       100           WVVOCE
207500940802     C     WVVOCE        MULT      EVDPER        WVVOCE
207600940802     C                   ENDIF
207700940802     C*
207800940802     C* CONTROLLO IMPORTO MINIMO
207900011227     C     W$IMI         IFNE      *ZEROS
208000011227     C     WVVOCE        ANDLT     W$IMI
208100011227     C                   Z-ADD     W$IMI         WVVOCE
208200940802     C                   ENDIF
208300011227     C     W$IMA         IFNE      *ZEROS
208400011227     C     WVVOCE        ANDGT     W$IMA
208500011227     C                   Z-ADD     W$IMA         WVVOCE
208600940802     C                   ENDIF
208700940802     C*
208800940802     C* CONTROLLO SEGNO E CARICO IMPORTO NELLA SCHIERA
208900090914     C                   Z-ADD(H)  WVVOCE        N133             13 5
209000940802     C     EVDSEG        IFEQ      '+'
209100020130     C                   Z-ADD     N133          VIMP(V)
209200020130     C                   ADD       N133          WVVO25
209300940802     C                   ELSE
209400020130     C                   Z-SUB     N133          VIMP(V)
209500020130     C                   SUB       N133          WVVO25
209600940802     C                   ENDIF
209700940802     C*
209800940802     C                   ENDSR
209900940907     C*------------------------------------------------------------------------*
210000940907     C*?ARROTO - ARROTONDA LA QUANTITA'
210100940907     C*          INPUT  : WQTA (DA ARROTONDARE)
210200940907     C*          OUTPUT : WQTA (ARROTONDATA)
210300940907     C*------------------------------------------------------------------------*
210400940907     C     ARROTO        BEGSR
210500940907     C*
210600940907     C* CONTROLLO SE ESISTE L'ARROTONDAMENTO
210700940907     C     WQTA          IFNE      *ZEROS
210800940907     C     VARL(V)       ANDNE     *ZEROS
210900940907     C     WQTA          IFLE      VARL(V)
211000010202     C                   Z-ADD     VARF(V)       WARR             17 5
211100940907     C                   ELSE
211200940907     C                   Z-ADD     VARO(V)       WARR
211300940907     C                   ENDIF
211400940907     C* CONTROLLO SE DATO GIA' ARRORONDATO
211500940907     C     WQTA          DIV       WARR          WINT             15 0
211600970730     C                   MVR                     WT0155           15 5    99
211700940907     C     *IN99         IFEQ      *OFF
211800940907     C     WINT          MULT      WARR          WQTA
211900940907     C                   ADD       WARR          WQTA
212000940907     C                   ENDIF
212100940907     C                   ENDIF
212200940907     C*
212300940907     C                   ENDSR
212400940907     C*------------------------------------------------------------------------*
212500940907     C*? CALPT  - CALCOLA IL PESO TASSABILE
212600940907     C*------------------------------------------------------------------------*
212700940907     C     CALPT         BEGSR
212800940907     C*
212900940907     C* CALCOLO PESO TASSABILE DAL VOLUME
213000941019     C     WVOL          MULT      EVDRPV        WPTAS
213100940907     C     WPTAS         MULT      100           WPTAS
213200940907     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
213300940907     C     VSPT(V)       IFEQ      '1'
213400940907     C                   Z-ADD     WPTAS         WQTA
213500940907     C                   ENDIF
213600940907     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
213700940907     C     VSPT(V)       IFEQ      '2'
213800991008     C     WPTAS         IFGT      WPES
213900940907     C                   Z-ADD     WPTAS         WQTA
214000940907     C                   ENDIF
214100940907     C                   ENDIF
214200940907     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
214300940907     C     VSPT(V)       IFEQ      '3'
214400991008     C     WPTAS         IFLT      WPES
214500940907     C                   Z-ADD     WPTAS         WQTA
214600940907     C                   ENDIF
214700940907     C                   ENDIF
214800940907     C*
214900940907     C                   ENDSR
215000030422     C*------------------------------------------------------------------------*
215100030422     C*? CALPS  - CALCOLA IL PESO TASSABILE PER RICERCA SCAGLIONE
215200030422     C*------------------------------------------------------------------------*
215300030422     C     CALPS         BEGSR
215400040209     C                   CLEAR                   WPTASR2
215500030422     C*
215600030422     C* CALCOLO PESO TASSABILE DAL VOLUME
215700030423     C     WVOL          MULT      VRPV(V)       WPTASR
215800030423     C     WPTASR        MULT      100           WPTASR
215900030422     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
216000030423     C     VSPS(V)       IFEQ      '1'
216100040209     C                   Z-ADD     WPTASR        WPTASR2
216200030422     C                   ENDIF
216300030422     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
216400030423     C     VSPS(V)       IFEQ      '2'
216500030423     C     WPTASR        IFGT      WPES
216600040209     C                   Z-ADD     WPTASR        WPTASR2
216700040209     c                   ELSE
216800040209     C                   Z-ADD     WPES          WPTASR2
216900030422     C                   ENDIF
217000030422     C                   ENDIF
217100030422     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
217200030423     C     VSPS(V)       IFEQ      '3'
217300030423     C     WPTASR        IFLT      WPES
217400040209     C                   Z-ADD     WPTASR        WPTASR2
217500040209     C                   ELSE
217600040209     C                   Z-ADD     WPES          WPTASR2
217700030422     C                   ENDIF
217800030422     C                   ENDIF
217900030422     C*
218000030422     C                   ENDSR
218100940803     C*------------------------------------------------------------------------*
218200940906     C*? CERMAG - CERCA LE MAGGIORAZIONI
218300940803     C*------------------------------------------------------------------------*
218400940906     C     CERMAG        BEGSR
218500940908     C*
218600940906     C* SE PERCENTUALE DIVERSA DA ZERO NON CERCO NEMMENO LA MAGGIORAZIONE
218700940906     C     EVDPER        IFNE      *ZEROS
218800940908     C                   MOVEL     'N'           WMAGOK            1
218900940906     C*
219000941024     C* CONTROLLA SE MAGGIORAZIONI IN MEMORIA
219100940906     C                   Z-ADD     1             M                 6 0
219200940906     C     M             DOWLE     WULTMA
219300940906     C     WVOC          IFEQ      MVOM(M)
219400940906     C     WPRG          ANDEQ     MPRM(M)
219500940908     C                   MOVEL     'S'           WMAGOK
219600940906     C                   Z-ADD     MVOM(M)       EVMVOM
219700940906     C                   Z-ADD     MPRM(M)       EVMPRM
219800940906     C                   Z-ADD     MVOC(M)       EVMVOC
219900940906     C                   Z-ADD     MPRG(M)       EVMPRG
220000100408     C                   Z-ADD     Mviso(M)      EVMviso
220100100408     C                   Z-ADD     Mddti(M)      EVMddt
220200100408     C                   Z-ADD     Mdsti(M)      EVMdst
220300100408     C                   movel     Mapp(M)       EVMapp
220400940906     C                   EXSR      CALMAG
220500940906     C                   ENDIF
220600940906     C                   ADD       1             M
220700940906     C                   ENDDO
220800940908     C*
220900940906     C* SE NON TROVATA IN MEMORIA E FINE SCHIERA LEGGO SUL FILE
221000940906     C     WMAGOK        IFEQ      'N'
221100041118     C     WULTMA        ANDGE     600
221200940906     C*
221300030331     C                   Z-ADD     WVOC          EVMVOM
221400940802     C                   Z-ADD     WPRG          EVMPRM
221500940802     C                   Z-ADD     *ZEROS        EVMVOC
221600940802     C                   Z-ADD     *ZEROS        EVMPRG
221700940802     C     KEYEVM        SETLL     ECEVM01L                           99
221800940802     C  N99              READ      ECEVM01L                               99
221900940802     C     *IN99         IFEQ      '1'
222000940802     C                   MOVEL     MSG(22)       $MSG
222100940907     C                   EXSR      SCRERR
222200940802     C                   END
222300940906     C     *IN99         DOWEQ     '0'
222400940802     C     EVMVOM        ANDEQ     WVOC
222500940802     C     EVMPRM        ANDEQ     WPRG
222600940906     C     EVMATB        IFEQ      *BLANKS
222700940906     C                   EXSR      CALMAG
222800940906     C                   ENDIF
222900940802     C                   READ      ECEVM01L                               99
223000940906     C                   ENDDO
223100940802     C*
223200940906     C                   ENDIF
223300940906     C                   ENDIF
223400940906     C*
223500940802     C                   ENDSR
223600940906     C*------------------------------------------------------------------------*
223700940906     C*? CALMAG - CALCOLA LA MAGGIORAZIONE
223800940906     C*------------------------------------------------------------------------*
223900940906     C     CALMAG        BEGSR
224000011227     C*
224100011227     C* CONVERTE GLI IMPORTI DELLE TARIFFE SE LA DIVISA DELLE ANAGRAFICHE C/E E'
224200011227     C* DIVERSA DALLA DIVISA DI OUTPUT DEL C/E
224300011227     C                   Z-ADD     EVDIMI        W$IMI
224400011227     C                   Z-ADD     EVDIMA        W$IMA
224500011227IF  1C     WDIVAC        IFNE      WDIVCE
224600011227IF  2C     EVDIMI        IFNE      *ZEROS
224700011227     C                   Z-ADD     EVDIMI        §IMI
224800011227     C                   MOVEL     WDIVAC        §DVI
224900011227     C                   EXSR      CNVEUR
225000011227     C                   Z-ADD     §IMO          W$IMI
225100011227E   2C                   ENDIF
225200011227IF  2C     EVDIMA        IFNE      *ZEROS
225300011227     C                   Z-ADD     EVDIMA        §IMI
225400011227     C                   MOVEL     WDIVAC        §DVI
225500011227     C                   EXSR      CNVEUR
225600011227     C                   Z-ADD     §IMO          W$IMA
225700011227E   2C                   ENDIF
225800011227E   1C                   ENDIF
225900940906     C*
226000940906     C                   Z-ADD     EVMVOC        I
226100000407     C* CALCOLA SOLO SE LA VOCE DA MAGGIORARE E' DA CALCOLARE
226200000407     C     VC(I)         IFEQ      'S'
226300940906     C* CALCOLA LA PERCENTUALE
226400940906     C     VIMP(I)       DIV       100           WVVOCE
226500940906     C     WVVOCE        MULT      EVDPER        WVVOCE
226600940906     C* CONTROLLO IMPORTO MINIMO
226700011227     C     W$IMI         IFNE      *ZEROS
226800011227     C     WVVOCE        ANDLT     W$IMI
226900011227     C                   Z-ADD     W$IMI         WVVOCE
227000940906     C                   ENDIF
227100940906     C* CONTROLLO IMPORTO MASSIMO
227200011227     C     W$IMA         IFNE      *ZEROS
227300011227     C     WVVOCE        ANDGT     W$IMA
227400011227     C                   Z-ADD     W$IMA         WVVOCE
227500940906     C                   ENDIF
227600940906     C* CONTROLLO SEGNO E AGGIUNGO O TOLGO IMPORTO NELLA SCHIERA
227700090914     C                   Z-ADD(H)  WVVOCE        N133
227800940906     C     EVDSEG        IFEQ      '+'
227900100408     c* ISOLO la maggiorazione e la applico solo se richiesto
228000100408     c                   if        evmapp = 'N' and
228100100408     C                             WDDSP >= evmddt and
228200100408     C                             WDDSP <= evmdst and
228300100422     c                             evmviso <> 0 and vc(evmviso)='S'
228400100408     c                   add       n133          vimp(evmviso)
228500100420     c                   if        vs(evmviso) = 'S'
228600100420     C                   ADD       N133          WVVO25
228700100420     c                   end
228800100408     c                   else
228900020130     C     VIMP(I)       ADD       N133          VIMP(I)
229000050503     c                   if        vs(i) = 'S'
229100020130     C                   ADD       N133          WVVO25
229200050503     c                   end
229300100420     c                   end
229400940906     C                   ELSE
229500100408     C* SEGNO NEGATIVO
229600100420     c* se il segno è negativo isolo la voce con segno positivo e la applico
229700100422     c* sempre
229800020130     C     VIMP(I)       SUB       N133          VIMP(I)
229900050503     c                   if        vs(i) = 'S'
230000020130     C                   SUB       N133          WVVO25
230100050503     c                   end
230200100420     c                   if        evmapp = 'S' and
230300100420     C                             WDDSP >= evmddt and
230400100420     C                             WDDSP <= evmdst and
230500100422     c                             evmviso <> 0 and vc(evmviso)='S'
230600100420     c                   add       n133          vimp(evmviso)
230700100420     c                   if        vs(evmviso) = 'S'
230800100420     C                   ADD       N133          WVVO25
230900100420     c                   end
231000100420     c                   end
231100940906     C                   ENDIF
231200000407     C                   ENDIF
231300940906     C*
231400940906     C                   ENDSR
231500940908     C*------------------------------------------------------------------------*
231600030429     C*?CERTAR - CERCA LA TARIFFA
231700940908     C*------------------------------------------------------------------------*
231800940908     C     CERTAR        BEGSR
231900940908     C*
232000940908     C* IMPOSTO CAMPI LAVORO
232100940908     C                   MOVEL     'N'           WFLTT             1
232200940908     C                   MOVEL     *BLANKS       WRIF
232300940908     C                   Z-ADD     WDDSP         WDSP
232400940908     C                   MOVEL     *BLANKS       WTT1              3
232500940908     C                   MOVEL     *BLANKS       WTT2              3
232600950519     C                   MOVEL     *BLANKS       WCTSM             2
232700940908     C                   MOVEL     'S'           WMSGMT            1
232800941107     C                   Z-ADD     *ZEROS        WPTAS            15 3
232900030423     C                   Z-ADD     *ZEROS        WPTASR           15 3
233000040209     C                   Z-ADD     *ZEROS        WPTASR2          15 3
233100030423     C*
233200030423     C* calcola il peso tassabile da ricercare nello scaglione
233300030423     C     VSPS(V)       IFGT      *ZEROS
233400030423     C                   EXSR      CALPS
233500040209     C                   Z-ADD     WPTASR2       WQTA
233600030423     C                   ELSE
233700940908     C*
233800940908     C* CERCA LA QTA DA RICERCARE NELLO SCAGLIONE
233900940908     C                   Z-ADD     *ZEROS        WQTA
234000940908     C                   Z-ADD     1             I
234100940908     C     VTIV(V)       LOOKUP    C2R(I)                                 98
234200940908     C     *IN98         IFEQ      '0'
234300940908     C                   MOVEL     MSG(20)       $MSG
234400940908     C                   EXSR      SCRERR
234500940908     C                   ELSE
234600940908     C                   Z-ADD     Q2R(I)        WQTA
234700940908     C                   END
234800030423     C                   ENDIF
234900940908     C*
235000940908     C* IMPOSTO IL FLAG ITALIA/ESTERO
235100971211     C                   Z-ADD     W$LNP         F
235200111020     C                   Z-ADD     W$lna         F1                4 0
235300020805     C     FIE(F)        IFEQ      'E'
235400020805     C     FIE(F1)       OREQ      'E'
235500940908     C                   MOVEL     'E'           WIE               1
235600940908     C                   ELSE
235700940908     C                   MOVEL     'I'           WIE
235800940908     C                   ENDIF
235900940908     C*
236000940908     C* SE RICHIESTO, TENTATIVO PER CLIENTE
236100940908     C     WFLTT         IFEQ      'N'
236200940908     C     WIE           ANDEQ     'I'
236300940908     C     VRKI(V)       ANDNE     *BLANKS
236400940908     C                   MOVEL     VRKI(V)       WRIF
236500940908     C                   EXSR      CERCLI
236600940908     C                   ENDIF
236700940908     C     WFLTT         IFEQ      'N'
236800940908     C     WIE           ANDEQ     'E'
236900940908     C     VRKE(V)       ANDNE     *BLANKS
237000940908     C                   MOVEL     VRKE(V)       WRIF
237100940908     C                   EXSR      CERCLI
237200940908     C                   ENDIF
237300940908     C*
237400940908     C* SE RICHIESTO, TENTATIVO PER CAP
237500940908     C     WFLTT         IFEQ      'N'
237600940908     C     WIE           ANDEQ     'I'
237700940908     C     VRPI(V)       ANDNE     *BLANKS
237800940908     C                   MOVEL     VRPI(V)       WRIF
237900940908     C                   EXSR      CERCAP
238000940908     C                   ENDIF
238100940908     C     WFLTT         IFEQ      'N'
238200940908     C     WIE           ANDEQ     'E'
238300940908     C     VRPE(V)       ANDNE     *BLANKS
238400940908     C                   MOVEL     VRPE(V)       WRIF
238500940908     C                   EXSR      CERCAP
238600940908     C                   ENDIF
238700940908     C*
238800940908     C* SE RICHIESTO, TENTATIVO PER CODICE TASSAZIONE
238900940908     C     WFLTT         IFEQ      'N'
239000940908     C     WIE           ANDEQ     'I'
239100940908     C     VRTI(V)       ANDNE     *BLANKS
239200940908     C                   MOVEL     VRTI(V)       WRIF
239300940908     C                   EXSR      CERTAS
239400940908     C                   ENDIF
239500940908     C     WFLTT         IFEQ      'N'
239600940908     C     WIE           ANDEQ     'E'
239700940908     C     VRTE(V)       ANDNE     *BLANKS
239800940908     C                   MOVEL     VRTE(V)       WRIF
239900940908     C                   EXSR      CERTAS
240000940908     C                   ENDIF
240100940908     C*
240200940908     C* SE RICHIESTO, TENTATIVO PER CODICE FILIALE
240300940908     C     WFLTT         IFEQ      'N'
240400940908     C     WIE           ANDEQ     'I'
240500940908     C     VRFI(V)       ANDNE     *BLANKS
240600940908     C                   MOVEL     VRFI(V)       WRIF
240700940908     C                   EXSR      CERFIL
240800940908     C                   ENDIF
240900940908     C     WFLTT         IFEQ      'N'
241000940908     C     WIE           ANDEQ     'E'
241100940908     C     VRFE(V)       ANDNE     *BLANKS
241200940908     C                   MOVEL     VRFE(V)       WRIF
241300940908     C                   EXSR      CERFIL
241400940908     C                   ENDIF
241500940908     C*
241600940908     C* SE TARIFFA NON ANCORA TROVATA CERCA TARIFFA DI CARTELLO
241700940908     C     WFLTT         IFEQ      'N'
241800940908     C                   EXSR      CERCAR
241900940908     C                   ENDIF
242000040903     c* se ho manca tariffa per la voce 167 xchè peso tassabile > 2,5 kg
242100040903     c* disattivo tale voce e lascio la 168 e 169 e viceversa
242200040903     c                   if        wvoc = 167
242300040903     c                   if        wfltt = 'N' and WQTA > §DFEPKGD
242400040903     c                   movel     'N'           wmsgmt
242500040903     c                   movel     'N'           vc(167)
242600040903     c                   else
242700040903     c                   movel     'N'           vc(168)
242800040903     c                   movel     'N'           vc(169)
242900040903     c                   end
243000040903     c                   end
243100940908     C*
243200940908     C* CONTROLLO MANCA TARIFFA
243300940908     C     WFLTT         IFEQ      'N'
243400940908     C     WMSGMT        IFEQ      'S'
243500940908     C                   MOVEL     MSG(21)       DSMSG
243600940908     C                   MOVE      WVOC          DSVOCE
243700940908     C                   MOVE      WTT1          DSTT1
243800940908     C                   MOVE      WTT2          DSTT2
243900950519     C                   MOVE      WCTSM         DSCTS
244000940908     C                   MOVEL     DSMSG         $MSG
244100940908     C                   EXSR      SCRERR
244200940908     C                   ENDIF
244300940908     C                   ELSE
244400940908     C                   MOVEL     'S'           WFLTT
244500940908     C                   END
244600940908     C*
244700940908     C                   ENDSR
244800940803     C*------------------------------------------------------------------------*
244900940803     C*? CERCLI - CERCA LA TARIFFA PER CLIENTE
245000940803     C*------------------------------------------------------------------------*
245100940608     C     CERCLI        BEGSR
245200940608     C*
245300940630     C                   MOVEL     'K'           WTTV
245400940630     C                   Z-ADD     *ZEROS        WFT1
245500940630     C                   Z-ADD     *ZEROS        WFT2
245600940630     C                   Z-ADD     *ZEROS        WKSC
245700940701     C                   Z-ADD     *ZEROS        WCTR
245800940701     C                   MOVEL     *BLANKS       WCTS
245900131002     C                   MOVEL     *BLANKS       Wnaz
246000131002     C                   MOVEL     *BLANKS       wcad
246100940826     C     WRIF          IFEQ      'KK'
246200940829     C                   Z-ADD     W$KSC         WKSC
246300940701     C                   Z-ADD     D$DCTR        WCTR
246400940826     C                   ENDIF
246500940826     C     WRIF          IFEQ      'KM'
246600941201     C     D$DTPO        IFEQ      'F'
246700940829     C     W$CCM         IFNE      *ZEROS
246800940829     C                   Z-ADD     W$CCM         WKSC
246900940826     C                   ELSE
247000940829     C                   Z-ADD     W$KSC         WKSC
247100940826     C                   ENDIF
247200940826     C                   ENDIF
247300941201     C     D$DTPO        IFEQ      'A'
247400940829     C                   Z-ADD     W$CCM         WKSC
247500940826     C                   ENDIF
247600940701     C                   Z-ADD     D$DCTR        WCTR
247700940826     C                   ENDIF
247800940701     C*
247900940701     C* LANCIA GESTIONE TARIFFE
248000940902     C     WKSC          IFNE      DEPKSC
248100940902     C                   Z-ADD     WKSC          DEPKSC            7 0
248200940830     C                   EXSR      CHKKSC
248300940902     C                   ENDIF
248400940908     C     WKSCOK        IFEQ      'S'
248500941107     C                   EXSR      GESEVD
248600940908     C                   ENDIF
248700940608     C*
248800940719     C* SE TARIFFA BOLLA NON E' STATA TROVATA PROVO CON TARIFFA = 999 (TUTTE)
248900940719     C     WFLTT         IFEQ      'N'
249000940829     C                   Z-ADD     W$KSC         WKSC
249100940719     C                   Z-ADD     999           WCTR
249200940908     C     WKSCOK        IFEQ      'S'
249300940722     C                   EXSR      GESEVD
249400940908     C                   ENDIF
249500940830     C                   ENDIF
249600940719     C*
249700940608     C                   ENDSR
249800940803     C*------------------------------------------------------------------------*
249900940803     C*? CERCAP - CERCA LA TARIFFA PER CAP
250000940803     C*------------------------------------------------------------------------*
250100940701     C     CERCAP        BEGSR
250200940701     C*
250300940701     C                   MOVEL     'P'           WTTV
250400940701     C                   Z-ADD     *ZEROS        WFT1
250500940701     C                   Z-ADD     *ZEROS        WFT2
250600940701     C                   Z-ADD     *ZEROS        WKSC
250700940701     C                   Z-ADD     *ZEROS        WCTR
250800940701     C                   MOVEL     *BLANKS       WCTS
250900131002     C                   MOVEL     *BLANKS       wcad
251000131002     C                   MOVEL     *BLANKS       Wnaz
251100131002     c* cap completo
251200940701     C     WRIF          IFEQ      'PA'
251300131002     C                   MOVEl     D$Dnzd        Wnaz
251400131002     C                   MOVEl     D$DCAD        wcad
251500940701     C* LANCIA GESTIONE TARIFFE
251600940722     C                   EXSR      GESEVD
251700131002     C                   ENDIF
251800131002     c* cap fittizio
251900131002     C                   if        wfltt = 'N' and
252000131002     c                             (wrif = 'PA' or wrif = 'PP')
252100131002     C                   MOVEl     D$Dnzd        Wnaz
252200131002     c     wnaz          lookup    d15(i)                                 88
252300131002     c                   if        *in88
252400131002     C                   eval      wcad = %subst(D$DCAD: pcf(i): ecf(i))
252500131002     C                   ENDIF
252600131002     C                   EXSR      GESEVD
252700131002     C                   ENDIF
252800940701     C*
252900940701     C                   ENDSR
253000940803     C*------------------------------------------------------------------------*
253100940803     C*? CERTAS - CERCA LA TARIFFA PER CODICE TASSAZIONE
253200940803     C*------------------------------------------------------------------------*
253300940701     C     CERTAS        BEGSR
253400940701     C*
253500940701     C                   MOVEL     'T'           WTTV
253600940701     C                   Z-ADD     *ZEROS        WFT1
253700940701     C                   Z-ADD     *ZEROS        WFT2
253800940701     C                   Z-ADD     *ZEROS        WKSC
253900940701     C                   Z-ADD     *ZEROS        WCTR
254000940701     C                   MOVEL     *BLANKS       WCTS
254100131002     C                   MOVEL     *BLANKS       wcad
254200131002     C                   MOVEL     *BLANKS       Wnaz
254300940701     C     WRIF          IFEQ      'SP'
254400940829     C                   MOVEL     W$MCT         WCTS
254500940701     C                   ENDIF
254600940701     C     WRIF          IFEQ      'SA'
254700940721     C                   MOVEL     D$DCTS        WCTS
254800940701     C                   ENDIF
254900940701     C*
255000940701     C* LANCIA GESTIONE TARIFFE
255100940722     C                   EXSR      GESEVD
255200950519     C*
255300950519     C* CAMPI PER MESSAGGI
255400950519     C                   MOVE      WCTS          WCTSM
255500940701     C*
255600940701     C                   ENDSR
255700940803     C*------------------------------------------------------------------------*
255800940803     C*? CERFIL - CERCA LA TARIFFA PER FILIALE/TERMINAL
255900940803     C*------------------------------------------------------------------------*
256000940701     C     CERFIL        BEGSR
256100940701     C*
256200940701     C                   MOVEL     'F'           WTTV
256300940701     C                   Z-ADD     *ZEROS        WFT1
256400940701     C                   Z-ADD     *ZEROS        WFT2
256500940701     C                   Z-ADD     *ZEROS        WKSC
256600940701     C                   Z-ADD     *ZEROS        WCTR
256700940701     C                   MOVEL     *BLANKS       WCTS
256800131002     C                   MOVEL     *BLANKS       wcad
256900131002     C                   MOVEL     *BLANKS       Wnaz
257000940701     C     WRIF          IFEQ      'LP'
257100940829     C                   Z-ADD     W$LNP         WFT1
257200940701     C                   ENDIF
257300940701     C     WRIF          IFEQ      'LT'
257400940829     C                   Z-ADD     W$LNP         WFT1
257500940829     C                   Z-ADD     W$TFP         WFT2
257600940701     C                   ENDIF
257700940701     C     WRIF          IFEQ      'TP'
257800940829     C                   Z-ADD     W$TFP         WFT1
257900940701     C                   ENDIF
258000940701     C     WRIF          IFEQ      'TT'
258100940829     C                   Z-ADD     W$TFP         WFT1
258200940826     C                   Z-ADD     D$DTFA        WFT2
258300130930     c* per bolle legate: conti servizi (F7 e FC) impostiamo il terminal
258400130122     c* della bolla figlia e non dell'originale
258500130122     C     D$DFIG        IFEQ      'S'
258600130122     c     D$DRBL        andeq     'C'
258700130122     c     D$DDBO        andeq     'S'
258800130930     C     WDDSP         andgt     20181231
258900130122     c                   z-add     d$DTFP        WFT1
259000130122     c                   ENDIF
259100940701     C                   ENDIF
259200940701     C     WRIF          IFEQ      'T1'
259300040524     C*                  Z-ADD     D$DFLP        WFT1
259400040524     C                   Z-ADD     D$Dar5flp     WFT1
259500940701     C                   ENDIF
259600940701     C     WRIF          IFEQ      'T2'
259700040524     C*                  Z-ADD     D$DFL2        WFT1
259800040524     C                   Z-ADD     D$Dar5fl2     WFT1
259900940701     C                   ENDIF
260000040524     C     WRIF          IFEQ      'T3'
260100040524     C                   Z-ADD     D$Dar5fl3     WFT1
260200040524     C                   ENDIF
260300040524     C     WRIF          IFEQ      'T4'
260400040524     C                   Z-ADD     D$Dar5fl4     WFT1
260500040524     C                   ENDIF
260600040524     C     WRIF          IFEQ      'T5'
260700040524     C                   Z-ADD     D$Dar5fl5     WFT1
260800040524     C                   ENDIF
260900940701     C     WRIF          IFEQ      'TA'
261000940701     C                   Z-ADD     D$DTFA        WFT1
261100940701     C                   ENDIF
261200940701     C     WRIF          IFEQ      'TL'
261300940701     C                   Z-ADD     D$DTFA        WFT1
261400111020     C                   Z-ADD     W$lna         WFT2
261500940701     C                   ENDIF
261600940701     C     WRIF          IFEQ      'LA'
261700111020     C                   Z-ADD     W$lna         WFT1
261800940701     C                   ENDIF
261900940902     C*
262000940902     C* LANCIA GESTIONE TARIFFE
262100940902     C                   MOVEL     D$DKSC        WFILX             3
262200940902     C     WFILX         IFNE      DEPFIL
262300940902     C                   MOVE      WFILX         DEPFIL            3
262400940902     C                   MOVE      WFILX         WFIL              3 0
262500940902     C                   EXSR      CARFIL
262600940902     C                   ENDIF
262700940722     C                   EXSR      GESEVD
262800940701     C*
262900940908     C* SE BOLLA FIGLIA E TARIFFA TERMINAL/TERMINAL E TARIFFA NON TROVATA
263000940908     C* ----> NON EMETTO MESSAGGIO 'TARIFFA NON TROVATA'
263100950905     C     D$DFIG        IFEQ      'S'
263200940908     C     WRIF          ANDEQ     'TT'
263300940908     C     WFLTT         ANDEQ     'N'
263400940908     C                   MOVEL     'N'           WMSGMT
263500940908     C                   ENDIF
263600940908     C*
263700940908     C* CAMPI PER MESSAGGI
263800940830     C                   MOVE      WFT1          WTT1
263900940830     C                   MOVE      WFT2          WTT2
264000940830     C*
264100940701     C                   ENDSR
264200940803     C*------------------------------------------------------------------------*
264300940803     C*? CERCAR - CERCA LA TARIFFA DI CARTELLO
264400940803     C*------------------------------------------------------------------------*
264500940701     C     CERCAR        BEGSR
264600940701     C*
264700940701     C                   MOVEL     'C'           WTTV
264800940701     C                   Z-ADD     *ZEROS        WFT1
264900940701     C                   Z-ADD     *ZEROS        WFT2
265000940701     C                   Z-ADD     *ZEROS        WKSC
265100940701     C                   Z-ADD     *ZEROS        WCTR
265200940701     C                   MOVEL     *BLANKS       WCTS
265300131002     C                   MOVEL     *BLANKS       wcad
265400131002     C                   MOVEL     *BLANKS       Wnaz
265500940701     C*
265600940701     C* LANCIA GESTIONE TARIFFE
265700940722     C                   EXSR      GESEVD
265800940701     C*
265900940701     C                   ENDSR
266000940704     C*------------------------------------------------------------------------*
266100940704     C*? GESEVD - GESTIONE FILE ECEVD (TARIFFE)
266200940704     C*------------------------------------------------------------------------*
266300940704     C     GESEVD        BEGSR
266400940801     C*
266500940801     C* SE TIPO SVILUPPO PESO TASSABILE 2=MIGLIORE REALE/TASS. O 3=MINORE REA/TA
266600940801     C* CERCO IL RAPP PESO/VOL PRIMA DI ARROTONDARE
266700961202     C     VSPT(V)       IFNE      *BLANKS
266800961202     C*
266900961202     C                   Z-ADD     *ZEROS        WQTAPT
267000940704     C*
267100940704     C* CERCA LO SCAGLIONE CON LA QTA DELLA BOLLA
267200940704     C                   EXSR      SETEVD
267300940704     C*
267400940707     C* SE IL RAPP. PESO/VOL MAGGIORE DI ZERO E VOLUME MAGGIORE DI ZERO
267500940707     C* CERCO SCAGLIONE CON PESO TASSABILE
267600940801     C     EVDRPV        IFNE      *ZEROS
267700941201     C     D$DVLF        ANDNE     *ZEROS
267800940707     C* CALCOLO PESO TASSABILE
267900940801     C                   EXSR      CALPT
268000020102     C                   Z-ADD     WQTA          WQTAPT           19 7
268100940801     C                   ENDIF
268200940801     C*
268300940801     C                   ENDIF
268400940707     C*
268500940803     C* CONTROLLA ARROTONDAMENTO
268600940803     C                   EXSR      ARROTO
268700940729     C*
268800940801     C* CERCA LO SCAGLIONE GIUSTO IN TARIFFA
268900940707     C                   EXSR      SETEVD
269000940704     C*
269100940704     C                   ENDSR
269200940701     C*------------------------------------------------------------------------*
269300940701     C*? SETEVD - POSIZIONAMENTO E PRIMA LETTURA FILE TARIFFE
269400940701     C*------------------------------------------------------------------------*
269500940701     C     SETEVD        BEGSR
269600940830     C*
269700940908     C                   MOVEL     'N'           WSETLL            1
269800940701     C                   MOVEL     '0'           FINEVD            1
269900940829     C*
270000940829     C* SE TARIFFA DI CARTELLO CONTROLLO PRIMA SE CARICATA IN MEMORIA
270100940829IF  1C     WTTV          IFEQ      'C'
270200940829     C                   EXSR      CHKCAR
270300940830E   1C                   ENDIF
270400940902     C*
270500940902     C* SE TARIFFA DI FILIALE CONTROLLO PRIMA SE CARICATA IN MEMORIA
270600940902IF  1C     WTTV          IFEQ      'F'
270700940902     C                   EXSR      CHKFIL
270800940902E   1C                   ENDIF
270900940831     C*
271000940831     C* SE TARIFFA CLIENTE CONTROLLO PRIMA SE CARICATA IN MEMORIA
271100940831IF  1C     WTTV          IFEQ      'K'
271200940831     C                   EXSR      CHKCLI
271300940831E   1C                   ENDIF
271400940829     C*
271500940907     C* SE TARIFFA NON TROVATA IN MEMORIA LEGGO DAL FILE
271600940908IF  1C     WSETLL        IFEQ      'N'
271700940829     C*
271800940701     C* POSIZIONAMENTO E PRIMA LETTURA
271900940701     C                   Z-ADD     WVOC          EVDVOC
272000940701     C                   Z-ADD     WPRG          EVDPRG
272100940701     C                   MOVEL     WTTV          EVDTTV
272200940701     C                   Z-ADD     WFT1          EVDFT1
272300940701     C                   Z-ADD     WFT2          EVDFT2
272400940701     C                   Z-ADD     WKSC          EVDCLI
272500940701     C                   Z-ADD     WCTR          EVDCTR
272600940701     C                   MOVEL     WCTS          EVDCTS
272700131002     C                   movel     Wnaz          EVDnaz
272800131002     C                   movel(p)  wcad          EVDcad
272900940802     C                   Z-ADD     *ZEROS        EVDDDT
273000940802     C                   Z-ADD     *ZEROS        EVDSGI
273100131002     C     KEYEVD        SETLL     ecevd06l                           99
273200940701     C*
273300940830IF  2C     *IN99         IFEQ      *ON
273400940701     C                   MOVEL     '1'           FINEVD
273500940830X   2C                   ELSE
273600940701     C                   EXSR      LETEVD
273700940830E   2C                   ENDIF
273800940701     C*
273900940720     C* CONTROLLO MANCA TARIFFA
274000940830IF  2C     WRECOK        IFEQ      'N'
274100991112     C                   MOVEL     'N'           WFLTT
274200940830X   2C                   ELSE
274300940701     C                   MOVEL     'S'           WFLTT
274400940830E   2C                   ENDIF
274500940829     C*
274600940830E   1C                   ENDIF
274700940701     C*
274800940701     C                   ENDSR
274900940701     C*------------------------------------------------------------------------*
275000940701     C*? LETEVD - LETTURA PROSSIMO RECORD TARIFFE
275100940830     C*------------------------------------------------------------------------*
275200940701     C     LETEVD        BEGSR
275300940701     C*
275400940802     C* LEGGO FINO A:
275500940701     C*     - FINE FILE O SUPERATO LIMITE FINALE
275600940701     C*     - TROVATO RECORD VALIDO
275700940701     C                   MOVEL     'N'           WRECOK
275800940701     C     FINEVD        DOWEQ     '0'
275900940701     C     WRECOK        ANDEQ     'N'
276000940701     C*
276100940701     C* LETTURE SUCCESSIVE ALLA PRIMA
276200131002     C                   READ      ecevd06l                               99
276300940701     C*
276400940701     C     *IN99         IFEQ      '1'
276500940701     C     EVDVOC        ORNE      WVOC
276600940701     C     EVDPRG        ORNE      WPRG
276700940701     C     EVDTTV        ORNE      WTTV
276800940701     C     EVDFT1        ORNE      WFT1
276900940701     C     EVDFT2        ORNE      WFT2
277000940721     C     EVDCLI        ORNE      WKSC
277100940701     C     EVDCTR        ORNE      WCTR
277200940701     C     EVDCTS        ORNE      WCTS
277300131002     C     EVDCAd        ORNE      wcad
277400131002     C     EVDnaz        ORNE      Wnaz
277500940720     C                   MOVEL     '1'           FINEVD
277600940701     C                   ELSE
277700940701     C                   EXSR      CHKEVD
277800940701     C                   END
277900940701     C*
278000940701     C                   END
278100940701     C*
278200940701     C                   ENDSR
278300940830     C*------------------------------------------------------------------------*
278400940701     C*? CHKEVD - CONTROLLA VALIDITA' DELLA TARIFFA
278500940830     C*------------------------------------------------------------------------*
278600940701     C     CHKEVD        BEGSR
278700940701     C*
278800940701     C                   MOVEL     'S'           WRECOK            1
278900940701     C* ESCLUDO SE ANNULLATO
279000940701     C     EVDATB        IFNE      *BLANKS
279100940701     C                   MOVEL     'N'           WRECOK
279200940701     C                   END
279300940701     C* ESCLUDO SE DATA SPEDIZIONE FUORI DALLA DECORR/SCAD.
279400940701     C     WDSP          IFLT      EVDDDT
279500940701     C     WDSP          ORGT      EVDDST
279600940701     C                   MOVEL     'N'           WRECOK
279700940701     C                   END
279800011227     C*
279900011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
280000011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
280100011227     C                   Z-ADD     EVDSGI        W$SGI
280200011227     C                   Z-ADD     EVDSGF        W$SGF
280300011227IF  1C     WRECOK        IFEQ      'S'
280400011227IF  2C                   IF        WTIV = '21' OR
280500011227     C                             WTIV = '22' OR
280600011227     C                             WTIV = '23' OR
280700011227     C                             WTIV = '25'
280800011227IF  3C     WDIVAC        IFNE      WDIVCE
280900011227IF  4C     EVDSGI        IFNE      *ZEROS
281000011227     C                   Z-ADD     EVDSGI        §IMI
281100011227     C                   MOVEL     WDIVAC        §DVI
281200011227     C                   EXSR      CNVEUR
281300011227     C                   Z-ADD     §IMO          W$SGI
281400011227E   4C                   ENDIF
281500011227IF  4C     EVDSGF        IFNE      *ZEROS
281600011227     C                   Z-ADD     EVDSGF        §IMI
281700011227     C                   MOVEL     WDIVAC        §DVI
281800011227     C                   EXSR      CNVEUR
281900011227     C                   Z-ADD     §IMO          W$SGF
282000011227E   4C                   ENDIF
282100011227E   3C                   ENDIF
282200011227E   2C                   ENDIF
282300011227E   1C                   ENDIF
282400011227     C*
282500940701     C* ESCLUDO SE QTA NON COMPRESA NELLO SCAGLIONE
282600000306IF  1C     WTIV          IFNE      *BLANKS
282700011227     C     WQTA          IFLT      W$SGI
282800011227     C     WQTA          ORGT      W$SGF
282900940701     C                   MOVEL     'N'           WRECOK
283000940701     C                   END
283100000306E   1C                   ENDIF
283200940701     C*
283300940701     C                   ENDSR
283400940830     C*------------------------------------------------------------------------*
283500940830     C*? CHKCAR - CONTROLLA SE TARIFFA DI CARTELLO CARICATA IN MEMORIA
283600940830     C*------------------------------------------------------------------------*
283700940830     C     CHKCAR        BEGSR
283800940830     C*
283900940830     C* CONTROLLA ESISTENZA TARIFFA DI CARTELLO IN LINEA FRA QUELLE CARICATE
284000940830     C                   Z-ADD     1             I
284100940830DO  1C     I             DOWLE     WULTCA
284200940908     C     WSETLL        ANDEQ     'N'
284300940830     C*
284400940830     C* CONTROLLI DI VALIDITA'
284500941213IF  2C     WVOC          IFEQ      CVOC(I)
284600941213     C*
284700941213IF  3C     WPRG          IFEQ      CPRG(I)
284800940830     C     WFT1          ANDEQ     CFT1(I)
284900940830     C     WFT2          ANDEQ     CFT2(I)
285000940830     C     WKSC          ANDEQ     CCLI(I)
285100940830     C     WCTR          ANDEQ     CCTR(I)
285200940830     C     WCTS          ANDEQ     CCTS(I)
285300131002     C     Wnaz          ANDEQ     Cnaz(I)
285400131002     C     wcad          ANDEQ     Ccad(I)
285500940830     C*
285600941213IF  4C     WDSP          IFGE      CDDT(I)
285700940830     C     WDSP          ANDLE     CDST(I)
285800000306     C*
285900011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
286000011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
286100011227     C                   Z-ADD     CSGI(I)       W$SGI
286200011227     C                   Z-ADD     CSGF(I)       W$SGF
286300011227IF  5C                   IF        WTIV = '21' OR
286400011227     C                             WTIV = '22' OR
286500011227     C                             WTIV = '23' OR
286600011227     C                             WTIV = '25'
286700011227IF  6C     WDIVAC        IFNE      WDIVCE
286800011227IF  7C     CSGI(I)       IFNE      *ZEROS
286900011227     C                   Z-ADD     CSGI(I)       §IMI
287000011227     C                   MOVEL     WDIVAC        §DVI
287100011227     C                   EXSR      CNVEUR
287200011227     C                   Z-ADD     §IMO          W$SGI
287300011227E   7C                   ENDIF
287400011227IF  7C     CSGF(I)       IFNE      *ZEROS
287500011227     C                   Z-ADD     CSGF(I)       §IMI
287600011227     C                   MOVEL     WDIVAC        §DVI
287700011227     C                   EXSR      CNVEUR
287800011227     C                   Z-ADD     §IMO          W$SGF
287900011227E   7C                   ENDIF
288000011227E   6C                   ENDIF
288100011227E   5C                   ENDIF
288200011227     c*
288300000306IF  5C     WTIV          IFEQ      *BLANKS
288400000306     C     WTIV          ORNE      *BLANKS
288500011227     C     WQTA          ANDGE     W$SGI
288600011227     C     WQTA          ANDLE     W$SGF
288700940830     C*
288800940830     C* CARTELLO TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
288900940908     C                   MOVEL     'S'           WSETLL
289000940830     C                   MOVEL     'S'           WFLTT
289100940830     C*
289200940830     C                   Z-ADD     CVOC(I)       EVDVOC
289300940830     C                   Z-ADD     CPRG(I)       EVDPRG
289400940830     C                   MOVEL     'C'           EVDTTV
289500940830     C                   Z-ADD     CFT1(I)       EVDFT1
289600940830     C                   Z-ADD     CFT2(I)       EVDFT2
289700940830     C                   Z-ADD     CCLI(I)       EVDCLI
289800940830     C                   Z-ADD     CCTR(I)       EVDCTR
289900940830     C                   MOVEL     CCTS(I)       EVDCTS
290000131002     C                   movel     Ccad(I)       EVDcad
290100131002     C                   movel     Cnaz(I)       EVDnaz
290200940830     C                   Z-ADD     CDDT(I)       EVDDDT
290300940830     C                   Z-ADD     CDST(I)       EVDDST
290400940830     C                   Z-ADD     CSGI(I)       EVDSGI
290500940830     C                   Z-ADD     CSGF(I)       EVDSGF
290600940830     C                   Z-ADD     CITR(I)       EVDITR
290700940830     C                   Z-ADD     CIMI(I)       EVDIMI
290800940830     C                   Z-ADD     CIMA(I)       EVDIMA
290900940830     C                   MOVEL     CSEG(I)       EVDSEG
291000940830     C                   Z-ADD     CPER(I)       EVDPER
291100940830     C                   Z-ADD     CRPV(I)       EVDRPV
291200940830     C*
291300941213E   5C                   ENDIF
291400940830E   4C                   ENDIF
291500940830E   3C                   ENDIF
291600940830E   2C                   ENDIF
291700940830     C*
291800940830     C                   ADD       1             I
291900940830E   1C                   ENDDO
292000940907     C*
292100940908     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
292200940908     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
292300940908IF  1C     WSETLL        IFEQ      'N'
292400940908     C     WULTCA        ANDLT     200
292500940908     C                   MOVEL     'S'           WSETLL
292600940908E   1C                   ENDIF
292700940907     C*
292800940907     C                   ENDSR
292900940902     C*------------------------------------------------------------------------*
293000940902     C*? CARFIL - CARICA TUTTE LE TARIFFE DELLA FILIALE
293100940902     C*------------------------------------------------------------------------*
293200940902     C     CARFIL        BEGSR
293300940902     C*
293400940902     C* POSIZIONAMENTO E PRIMA LETTURA
293500940902     C     WFIL          SETLL     ECEVD03L
293600940902     C                   READ      ECEVD03L                               99
293700940902     C*
293800940902     C* FINO A FINE FILE
293900940902     C                   Z-ADD     *ZEROS        I
294000940902DO  1C     *IN99         DOWEQ     *OFF
294100940902     C     EVDFT1        ANDEQ     WFIL
294200940902IF  3C     EVDATB        IFEQ      *BLANKS
294300940902     C                   ADD       1             I
294400940902     C                   Z-ADD     EVDVOC        FVOC(I)
294500940902     C                   Z-ADD     EVDPRG        FPRG(I)
294600940902     C                   Z-ADD     EVDFT1        FFT1(I)
294700940902     C                   Z-ADD     EVDFT2        FFT2(I)
294800940902     C                   Z-ADD     EVDCLI        FCLI(I)
294900940902     C                   Z-ADD     EVDCTR        FCTR(I)
295000940902     C                   MOVEL     EVDCTS        FCTS(I)
295100131002     C                   movel     EVDcad        Fcad(I)
295200131002     C                   movel     EVDnaz        Fnaz(I)
295300940902     C                   Z-ADD     EVDDDT        FDDT(I)
295400940902     C                   Z-ADD     EVDDST        FDST(I)
295500940902     C                   Z-ADD     EVDSGI        FSGI(I)
295600940902     C                   Z-ADD     EVDSGF        FSGF(I)
295700940902     C                   Z-ADD     EVDITR        FITR(I)
295800940902     C                   Z-ADD     EVDIMI        FIMI(I)
295900940902     C                   Z-ADD     EVDIMA        FIMA(I)
296000940902     C                   MOVEL     EVDSEG        FSEG(I)
296100940902     C                   Z-ADD     EVDPER        FPER(I)
296200940902     C                   Z-ADD     EVDRPV        FRPV(I)
296300940902E   2C                   ENDIF
296400940902     C*
296500940902     C                   READ      ECEVD03L                               99
296600940902E   1C                   ENDDO
296700940902     C*
296800940902     C                   Z-ADD     I             WULTTF            6 0
296900940902     C*
297000940902     C*
297100940902     C                   ENDSR
297200940902     C*------------------------------------------------------------------------*
297300941213     C*? CHKFIL - CONTROLLA SE TARIFFA FILIALE GIA' IN MEMORIA
297400940902     C*------------------------------------------------------------------------*
297500940902     C     CHKFIL        BEGSR
297600940902     C*
297700941213     C* CONTROLLA ESISTENZA TARIFFA FILIALE FRA QUELLE CARICATE
297800940902     C                   Z-ADD     1             I
297900940908DO  1C     I             DOWLE     WULTTF
298000940908     C     WSETLL        ANDEQ     'N'
298100940902     C*
298200940902     C* CONTROLLI DI VALIDITA'
298300941213IF  2C     WVOC          IFEQ      FVOC(I)
298400941213     C*
298500941213IF  3C     WPRG          IFEQ      FPRG(I)
298600940902     C     WFT1          ANDEQ     FFT1(I)
298700940902     C     WFT2          ANDEQ     FFT2(I)
298800940902     C     WKSC          ANDEQ     FCLI(I)
298900940902     C     WCTR          ANDEQ     FCTR(I)
299000940902     C     WCTS          ANDEQ     FCTS(I)
299100131002     C     Wnaz          ANDEQ     Fnaz(I)
299200131002     C     wcad          ANDEQ     Fcad(I)
299300940902     C*
299400941213IF  4C     WDSP          IFGE      FDDT(I)
299500940902     C     WDSP          ANDLE     FDST(I)
299600011227     C*
299700011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
299800011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
299900011227     C                   Z-ADD     FSGI(I)       W$SGI
300000011227     C                   Z-ADD     FSGF(I)       W$SGF
300100011227IF  5C                   IF        WTIV = '21' OR
300200011227     C                             WTIV = '22' OR
300300011227     C                             WTIV = '23' OR
300400011227     C                             WTIV = '25'
300500011227IF  6C     WDIVAC        IFNE      WDIVCE
300600011227IF  7C     FSGI(I)       IFNE      *ZEROS
300700011227     C                   Z-ADD     FSGI(I)       §IMI
300800011227     C                   MOVEL     WDIVAC        §DVI
300900011227     C                   EXSR      CNVEUR
301000011227     C                   Z-ADD     §IMO          W$SGI
301100011227E   7C                   ENDIF
301200011227IF  7C     FSGF(I)       IFNE      *ZEROS
301300011227     C                   Z-ADD     FSGF(I)       §IMI
301400011227     C                   MOVEL     WDIVAC        §DVI
301500011227     C                   EXSR      CNVEUR
301600011227     C                   Z-ADD     §IMO          W$SGF
301700011227E   7C                   ENDIF
301800011227E   6C                   ENDIF
301900011227E   5C                   ENDIF
302000000306     C*
302100000306IF  5C     WTIV          IFEQ      *BLANKS
302200000306     C     WTIV          ORNE      *BLANKS
302300011227     C     WQTA          ANDGE     W$SGI
302400011227     C     WQTA          ANDLE     W$SGF
302500940902     C*
302600940902     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
302700940908     C                   MOVEL     'S'           WSETLL
302800940902     C                   MOVEL     'S'           WFLTT
302900940902     C*
303000940902     C                   Z-ADD     FVOC(I)       EVDVOC
303100940902     C                   Z-ADD     FPRG(I)       EVDPRG
303200940902     C                   MOVEL     'F'           EVDTTV
303300940902     C                   Z-ADD     FFT1(I)       EVDFT1
303400940902     C                   Z-ADD     FFT2(I)       EVDFT2
303500940902     C                   Z-ADD     FCLI(I)       EVDCLI
303600940902     C                   Z-ADD     FCTR(I)       EVDCTR
303700940902     C                   MOVEL     FCTS(I)       EVDCTS
303800131002     C                   movel     Fcad(I)       EVDcad
303900131002     C                   movel     Fnaz(I)       EVDnaz
304000940902     C                   Z-ADD     FDDT(I)       EVDDDT
304100940902     C                   Z-ADD     FDST(I)       EVDDST
304200940902     C                   Z-ADD     FSGI(I)       EVDSGI
304300940902     C                   Z-ADD     FSGF(I)       EVDSGF
304400940902     C                   Z-ADD     FITR(I)       EVDITR
304500940902     C                   Z-ADD     FIMI(I)       EVDIMI
304600940902     C                   Z-ADD     FIMA(I)       EVDIMA
304700940902     C                   MOVEL     FSEG(I)       EVDSEG
304800940902     C                   Z-ADD     FPER(I)       EVDPER
304900940902     C                   Z-ADD     FRPV(I)       EVDRPV
305000940902     C*
305100941213E   5C                   ENDIF
305200940902E   4C                   ENDIF
305300940902E   3C                   ENDIF
305400940902E   2C                   ENDIF
305500940902     C*
305600940902     C                   ADD       1             I
305700940902E   1C                   ENDDO
305800940902     C*
305900940902     C                   ENDSR
306000940907     C*------------------------------------------------------------------------*
306100940907     C*? CARCLI - CARICA LE TARIFFE DI UN CLIENTE
306200940907     C*------------------------------------------------------------------------*
306300940907     C     CARCLI        BEGSR
306400940907     C*
306500940907     C* POSIZIONAMENTO E PRIMA LETTURA
306600940907     C     WKSC          SETLL     ECEVD02L
306700940907     C                   READ      ECEVD02L                               99
306800940907     C*
306900940907     C* FINO A FINE FILE
307000940907     C                   Z-ADD     *ZEROS        I
307100940907DO  1C     *IN99         DOWEQ     *OFF
307200940907IF  2C     EVDCLI        ANDEQ     WKSC
307300940907     C     I             ANDLT     200
307400940907     C     EVDATB        IFEQ      *BLANKS
307500940907     C                   ADD       1             I
307600940907     C                   Z-ADD     EVDVOC        KVOC(I)
307700940907     C                   Z-ADD     EVDPRG        KPRG(I)
307800940907     C                   Z-ADD     EVDFT1        KFT1(I)
307900940907     C                   Z-ADD     EVDFT2        KFT2(I)
308000940907     C                   Z-ADD     EVDCLI        KCLI(I)
308100940907     C                   Z-ADD     EVDCTR        KCTR(I)
308200940907     C                   MOVEL     EVDCTS        KCTS(I)
308300131002     C                   movel     EVDcad        Kcad(I)
308400131002     C                   movel     EVDnaz        Knaz(I)
308500940907     C                   Z-ADD     EVDDDT        KDDT(I)
308600940907     C                   Z-ADD     EVDDST        KDST(I)
308700940907     C                   Z-ADD     EVDSGI        KSGI(I)
308800940907     C                   Z-ADD     EVDSGF        KSGF(I)
308900940907     C                   Z-ADD     EVDITR        KITR(I)
309000940907     C                   Z-ADD     EVDIMI        KIMI(I)
309100940907     C                   Z-ADD     EVDIMA        KIMA(I)
309200940907     C                   MOVEL     EVDSEG        KSEG(I)
309300940907     C                   Z-ADD     EVDPER        KPER(I)
309400940907     C                   Z-ADD     EVDRPV        KRPV(I)
309500940907E   2C                   ENDIF
309600940907     C*
309700940907     C                   READ      ECEVD02L                               99
309800940907E   1C                   ENDDO
309900940907     C*
310000940907     C                   Z-ADD     I             WULTTK            6 0
310100940907     C*
310200940907     C                   ENDSR
310300940831     C*------------------------------------------------------------------------*
310400940902     C*? CHKCLI - CONTROLLA SE TARIFFA CLIENTE GIA' IN MEMORIA
310500940831     C*------------------------------------------------------------------------*
310600940831     C     CHKCLI        BEGSR
310700940831     C*
310800940831     C* CONTROLLA ESISTENZA TARIFFA CLIENTE FRA QUELLE CARICATE
310900940831     C                   Z-ADD     1             I
311000940831DO  1C     I             DOWLE     WULTTK
311100940908     C     WSETLL        ANDEQ     'N'
311200940831     C*
311300940831     C* CONTROLLI DI VALIDITA'
311400940831IF  2C     WVOC          IFEQ      KVOC(I)
311500941213     C*
311600941213IF  3C     WPRG          IFEQ      KPRG(I)
311700940831     C     WFT1          ANDEQ     KFT1(I)
311800940831     C     WFT2          ANDEQ     KFT2(I)
311900940831     C     WKSC          ANDEQ     KCLI(I)
312000940831     C     WCTR          ANDEQ     KCTR(I)
312100940831     C     WCTS          ANDEQ     KCTS(I)
312200131002     C     Wnaz          ANDEQ     Knaz(I)
312300131002     C     wcad          ANDEQ     Kcad(I)
312400940831     C*
312500941213IF  4C     WDSP          IFGE      KDDT(I)
312600940831     C     WDSP          ANDLE     KDST(I)
312700011227     C*
312800011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
312900011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
313000011227     C                   Z-ADD     KSGI(I)       W$SGI
313100011227     C                   Z-ADD     KSGF(I)       W$SGF
313200011227IF  5C                   IF        WTIV = '21' OR
313300011227     C                             WTIV = '22' OR
313400011227     C                             WTIV = '23' OR
313500011227     C                             WTIV = '25'
313600011227IF  6C     WDIVAC        IFNE      WDIVCE
313700011227IF  7C     KSGI(I)       IFNE      *ZEROS
313800011227     C                   Z-ADD     KSGI(I)       §IMI
313900011227     C                   MOVEL     WDIVAC        §DVI
314000011227     C                   EXSR      CNVEUR
314100011227     C                   Z-ADD     §IMO          W$SGI
314200011227E   7C                   ENDIF
314300011227IF  7C     KSGF(I)       IFNE      *ZEROS
314400011227     C                   Z-ADD     KSGF(I)       §IMI
314500011227     C                   MOVEL     WDIVAC        §DVI
314600011227     C                   EXSR      CNVEUR
314700011227     C                   Z-ADD     §IMO          W$SGF
314800011227E   7C                   ENDIF
314900011227E   6C                   ENDIF
315000011227E   5C                   ENDIF
315100940831     C*
315200000306IF  5C     WTIV          IFEQ      *BLANKS
315300000306     C     WTIV          ORNE      *BLANKS
315400011227     C     WQTA          ANDGE     W$SGI
315500011227     C     WQTA          ANDLE     W$SGF
315600940831     C*
315700940902     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
315800940908     C                   MOVEL     'S'           WSETLL
315900940831     C                   MOVEL     'S'           WFLTT
316000940831     C*
316100940831     C                   Z-ADD     KVOC(I)       EVDVOC
316200940831     C                   Z-ADD     KPRG(I)       EVDPRG
316300940831     C                   MOVEL     'K'           EVDTTV
316400940831     C                   Z-ADD     KFT1(I)       EVDFT1
316500940831     C                   Z-ADD     KFT2(I)       EVDFT2
316600940831     C                   Z-ADD     KCLI(I)       EVDCLI
316700940831     C                   Z-ADD     KCTR(I)       EVDCTR
316800940831     C                   MOVEL     KCTS(I)       EVDCTS
316900131002     C                   movel     Kcad(I)       EVDcad
317000131002     C                   movel     Knaz(I)       EVDnaz
317100940831     C                   Z-ADD     KDDT(I)       EVDDDT
317200940831     C                   Z-ADD     KDST(I)       EVDDST
317300940831     C                   Z-ADD     KSGI(I)       EVDSGI
317400940831     C                   Z-ADD     KSGF(I)       EVDSGF
317500940831     C                   Z-ADD     KITR(I)       EVDITR
317600940831     C                   Z-ADD     KIMI(I)       EVDIMI
317700940831     C                   Z-ADD     KIMA(I)       EVDIMA
317800940831     C                   MOVEL     KSEG(I)       EVDSEG
317900940831     C                   Z-ADD     KPER(I)       EVDPER
318000940831     C                   Z-ADD     KRPV(I)       EVDRPV
318100940831     C*
318200941213E   5C                   ENDIF
318300941213E   4C                   ENDIF
318400940831E   3C                   ENDIF
318500940831E   2C                   ENDIF
318600940831     C*
318700940831     C                   ADD       1             I
318800940908E   1C                   ENDDO
318900940908     C*
319000940908     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
319100940908     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
319200940908IF  1C     WSETLL        IFEQ      'N'
319300940908     C     WULTTK        ANDLT     200
319400940908     C                   MOVEL     'S'           WSETLL
319500940908E   1C                   ENDIF
319600940831     C*
319700940831     C                   ENDSR
319800940830     C*------------------------------------------------------------------------*
319900940830     C*? CHKKSC - CONTROLLA SE IL CLIENTE E' TRA QUELLI CON TARIFFA PARTICOLARE
320000940830     C*------------------------------------------------------------------------*
320100940830     C     CHKKSC        BEGSR
320200940830     C*
320300940831     C* CONTROLLA ESISTENZA CLIENTE TRA QUELLI CON TARIFFA PARTICOLARE
320400940830     C                   MOVEL     'N'           WKSCOK            1
320500940902     C                   Z-ADD     1             IK                9 0
320600940902DO  1C     IK            DOWLE     WULTKS
320700940907     C     WKSCOK        ANDEQ     'N'
320800940830     C*
320900940907     C* CONTROLLI DI VALIDITA' (CLIENTE FRA QUELLI CARICATI)
321000940902IF  2C     WKSC          IFEQ      KSC(IK)
321100940830     C                   MOVEL     'S'           WKSCOK
321200940831     C* CARICA TUTTE LE TARIFFE DEL CLIENTE
321300940831     C                   EXSR      CARCLI
321400940830E   2C                   ENDIF
321500940830     C*
321600940902     C                   ADD       1             IK
321700940830E   1C                   ENDDO
321800940908     C*
321900940830     C                   ENDSR
322000011221     C*------------------------------------------------------------------------*
322100011221     C*?CNVEUR - CONVERTE I CAMPI DA DIVISA ESTERA A LIRE
322200011221     C*------------------------------------------------------------------------*
322300011221     C     CNVEUR        BEGSR
322400011221     C*
322500011221     C* AZZERA VARIABILI DI LAVORO
322600011221     C                   Z-ADD     *ZEROS        §IMO
322700011221     C*
322800011221     C                   CLEAR                   YEUR
322900011221     C                   MOVEL     §DVI          YECDVI
323000011221     C                   Z-ADD     §IMI          YECIMI
323100011221     C                   MOVEL     WDIVCE        YECDVO
323200011221     C                   MOVEL     '3'           YECDCO
323300011221     C                   MOVEL     'H'           YECTAR
323400011221     C                   CALL      'YEURCO'
323500011221     C                   PARM                    YEUR
323600011221IF  1C     YECESI        IFEQ      ' '
323700011221     C                   Z-ADD     YECIMO        §IMO
323800011221X   1C                   ELSE
323900011221     C                   Z-ADD     §IMI          §IMO
324000011221E   1C                   ENDIF
324100011221     C*
324200011221     C                   ENDSR
324300940907     C*------------------------------------------------------------------------*
324400940907     C*? CARCAR - CARICA LE TARIFFE DI CARTELLO
324500940907     C*------------------------------------------------------------------------*
324600940907     C     CARCAR        BEGSR
324700940907     C*
324800940907     C* POSIZIONAMENTO E PRIMA LETTURA
324900940907     C                   MOVEL     'C'           EVDTTV
325000940907     C     EVDTTV        SETLL     ECEVD04L
325100940907     C                   READ      ECEVD04L                               99
325200940907     C*
325300940907     C* FINO A FINE FILE E TARIFFE DI CARTELLO
325400940907     C                   Z-ADD     *ZEROS        I
325500940907DO  1C     *IN99         DOWEQ     *OFF
325600940907     C     EVDTTV        ANDEQ     'C'
325700940907     C     I             ANDLT     200
325800940907IF  2C     EVDATB        IFEQ      *BLANKS
325900940907     C                   ADD       1             I
326000940907     C                   Z-ADD     EVDVOC        CVOC(I)
326100940907     C                   Z-ADD     EVDPRG        CPRG(I)
326200940907     C                   Z-ADD     EVDFT1        CFT1(I)
326300940907     C                   Z-ADD     EVDFT2        CFT2(I)
326400940907     C                   Z-ADD     EVDCLI        CCLI(I)
326500940907     C                   Z-ADD     EVDCTR        CCTR(I)
326600940907     C                   MOVEL     EVDCTS        CCTS(I)
326700131002     C                   movel     EVDcad        Ccad(I)
326800131002     C                   movel     EVDnaz        Cnaz(I)
326900940907     C                   Z-ADD     EVDDDT        CDDT(I)
327000940907     C                   Z-ADD     EVDDST        CDST(I)
327100940907     C                   Z-ADD     EVDSGI        CSGI(I)
327200940907     C                   Z-ADD     EVDSGF        CSGF(I)
327300940907     C                   Z-ADD     EVDITR        CITR(I)
327400940907     C                   Z-ADD     EVDIMI        CIMI(I)
327500940907     C                   Z-ADD     EVDIMA        CIMA(I)
327600940907     C                   MOVEL     EVDSEG        CSEG(I)
327700940907     C                   Z-ADD     EVDPER        CPER(I)
327800940907     C                   Z-ADD     EVDRPV        CRPV(I)
327900940907E   2C                   ENDIF
328000940907     C*
328100940907     C                   READ      ECEVD04L                               99
328200940907E   1C                   ENDDO
328300940907     C*
328400940907     C                   Z-ADD     I             WULTCA            6 0
328500940907     C*
328600940907     C                   ENDSR
328700940907     C*------------------------------------------------------------------------*
328800940907     C*? CARKSC - CARICA I CLIENTI CON TARIFFE PARTICOLARI
328900940907     C*------------------------------------------------------------------------*
329000940907     C     CARKSC        BEGSR
329100940907     C*
329200940907     C* POSIZIONAMENTO E PRIMA LETTURA
329300940907     C                   MOVEL     'K'           EVDTTV
329400940907     C     EVDTTV        SETLL     ECEVD04L
329500940907     C                   READ      ECEVD04L                               99
329600940907     C*
329700940907     C* FINO A FINE FILE E TARIFFE DI CLIENTE
329800940907     C                   Z-ADD     *ZEROS        Y                 6 0
329900940907DO  1C     *IN99         DOWEQ     *OFF
330000940907     C     EVDTTV        ANDEQ     'K'
330100070626     C     Y             ANDLT     5000
330200940907IF  2C     EVDATB        IFEQ      *BLANKS
330300940907     C                   MOVEL     'S'           WFKSC             1
330400940907IF  3C     Y             IFGT      *ZEROS
330500940907     C                   Z-ADD     *ZEROS        YY                6 0
330600940907DO  4C                   DO        Y             YY
330700940907IF  5C     EVDCLI        IFEQ      KSC(YY)
330800940907     C                   MOVEL     'N'           WFKSC
330900940907E   5C                   ENDIF
331000940907E   4C                   ENDDO
331100940907E   3C                   ENDIF
331200940907IF  3C     WFKSC         IFEQ      'S'
331300940907     C                   ADD       1             Y
331400940907     C                   Z-ADD     EVDCLI        KSC(Y)
331500940907E   3C                   ENDIF
331600940907E   2C                   ENDIF
331700940907     C*
331800940907     C                   READ      ECEVD04L                               99
331900940907E   1C                   ENDDO
332000940907     C*
332100940907     C                   Z-ADD     Y             WULTKS            6 0
332200940907     C*
332300940907     C                   ENDSR
332400940907     C*------------------------------------------------------------------------*
332500940907     C*? CARMAG - CARICA LE MAGGIORAZIONI
332600940907     C*------------------------------------------------------------------------*
332700940907     C     CARMAG        BEGSR
332800940907     C*
332900940907     C* POSIZIONAMENTO E PRIMA LETTURA
333000940907     C     *LOVAL        SETLL     ECEVM01L
333100940907     C                   READ      ECEVM01L                               99
333200940907     C*
333300940907     C* FINO A FINE FILE E MAGGIORAZIONI
333400940907     C                   Z-ADD     *ZEROS        I
333500940907DO  1C     *IN99         DOWEQ     *OFF
333600041118     C     I             ANDLT     600
333700940907IF  2C     EVMATB        IFEQ      *BLANKS
333800940907     C                   ADD       1             I
333900940907     C                   Z-ADD     EVMVOM        MVOM(I)
334000940907     C                   Z-ADD     EVMPRM        MPRM(I)
334100940907     C                   Z-ADD     EVMVOC        MVOC(I)
334200940907     C                   Z-ADD     EVMPRG        MPRG(I)
334300100408     C                   Z-ADD     EVMviso       Mviso(I)
334400100408     C                   Z-ADD     EVMddt        Mddti(I)
334500100408     C                   Z-ADD     EVMdst        Mdsti(I)
334600100408     C                   movel     EVMapp        Mapp(I)
334700940907E   2C                   ENDIF
334800940907     C*
334900940907     C                   READ      ECEVM01L                               99
335000940907E   1C                   ENDDO
335100940907     C*
335200940907     C                   Z-ADD     I             WULTMA            6 0
335300940907     C*
335400940907     C                   ENDSR
335500940803     C*------------------------------------------------------------------------*
335600940803     C*? CARTAB - CARICA LE TABELLE
335700940803     C*------------------------------------------------------------------------*
335800930414     C     CARTAB        BEGSR
335900011221     C*
336000011221     C* REPERISCE LA DATA CORRENTE
336100011221     C                   TIME                    N14              14 0
336200011221     C                   MOVE      N14           N8                8 0
336300011221     C                   Z-ADD     N8            G08DAT
336400011221     C                   Z-ADD     *ZEROS        G08INV
336500011221     C                   MOVEL     '0'           G08ERR
336600011221     C                   CALL      'XSRDA8'
336700011221     C                   PARM                    WLBDA8
336800011221     C                   Z-ADD     G08INV        DATCOR            8 0
336900941024     C*
337000941024     C                   OPEN      TABEL00F
337100011221     C                   OPEN      TNTBE01L
337200020805     C                   OPEN      AZORG01L
337300940704     C*
337400940704     C* CARICA TABELLA -2R- RIFERIMENTO VOCE
337500940913     C     $ERR          IFEQ      'N'
337600940704     C                   Z-ADD     0             I
337700940704     C                   Z-ADD     CODUT         KTBUTE
337800940704     C                   MOVEL     '2R'          KTBCOD
337900940704     C                   MOVEL     *BLANKS       KTBKEY
338000940704     C     KEYTAB        SETLL     TABEL00F                           99
338100940704     C  N99              READ      TABEL00F                               99
338200940704     C     *IN99         IFEQ      '1'
338300940720     C                   MOVEL     MSG(3)        $MSG
338400940907     C                   EXSR      SCRERR
338500940704     C                   END
338600940704     C     *IN99         DOWEQ     '0'
338700940704     C     TBLCOD        ANDEQ     '2R'
338800940704     C     TBLFLG        IFEQ      *BLANKS
338900940704     C                   ADD       1             I
339000940704     C                   MOVEL     TBLKEY        C2R(I)
339100940707     C                   MOVEL     TBLUNI        DS2R
339200940707     C                   MOVEL     §2RTVA        T2R(I)
339300940704     C                   END
339400940704     C                   READ      TABEL00F                               99
339500940704     C                   END
339600940704     C                   END
339700940720     C*
339800940720     C* CARICA TABELLA -FA- CONDIZIONI DI PAGAMENTO
339900940913     C     $ERR          IFEQ      'N'
340000940720     C                   Z-ADD     0             I
340100940720     C                   Z-ADD     CODUT         KTBUTE
340200940720     C                   MOVEL     'FA'          KTBCOD
340300940720     C                   MOVEL     *BLANKS       KTBKEY
340400940720     C     KEYTAB        SETLL     TABEL00F                           99
340500940720     C  N99              READ      TABEL00F                               99
340600940720     C     *IN99         IFEQ      '1'
340700940720     C                   MOVEL     MSG(2)        $MSG
340800940907     C                   EXSR      SCRERR
340900940720     C                   END
341000940720     C     *IN99         DOWEQ     '0'
341100940720     C     TBLCOD        ANDEQ     'FA'
341200940720     C     TBLFLG        IFEQ      *BLANKS
341300940720     C                   ADD       1             I
341400940720     C                   MOVEL     TBLKEY        CFA(I)
341500940721     C                   MOVEL     TBLUNI        DSFA
341600940720     C                   MOVEL     §FARAT        WTM03             3
341700940720     C                   MOVE      WTM03         WTM030            3 0
341800940720     C     WTM030        IFLT      15
341900940720     C     WTM030        MULT      30            GFA(I)
342000940720     C                   ELSE
342100940720     C                   Z-ADD     WTM030        GFA(I)
342200940720     C                   END
342300940720     C                   END
342400940720     C                   READ      TABEL00F                               99
342500940720     C                   END
342600940720     C                   END
342700000626     C*
342800000626     C* CARICA TABELLA -7O-  CONSEGNE ANOMALE
342900000626IF  1C     $ERR          IFEQ      'N'
343000000626     C                   Z-ADD     0             I
343100000626     C                   Z-ADD     CODUT         KTBUTE
343200000626     C                   MOVEL     '7O'          KTBCOD
343300000626     C                   MOVEL     *BLANKS       KTBKEY
343400000626     C     KEYTAB        SETLL     TABEL00F                           99
343500000626     C  N99              READ      TABEL00F                               99
343600000626IF  2C     *IN99         IFEQ      '1'
343700000626     C                   MOVEL     MSG(1)        $MSG
343800000626     C                   EXSR      SCRERR
343900000626E   2C                   ENDIF
344000000626DO  2C     *IN99         DOWEQ     '0'
344100000626     C     TBLCOD        ANDEQ     '7O'
344200000626IF  3C     TBLFLG        IFEQ      *BLANKS
344300000626     C                   ADD       1             I
344400000626     C                   MOVEL     TBLKEY        CCA(I)
344500000626     C                   MOVEL     TBLUNI        DS7O
344600000626     C                   MOVEL     §7OFPT        CFPT(I)
344700000626     C                   MOVEL     §7ORMS        CRMS(I)
344800000626E   3C                   ENDIF
344900000626     C                   READ      TABEL00F                               99
345000000626E   2C                   ENDDO
345100000626E   1C                   ENDIF
345200010420     C*
345300010420     C* CARICA TABELLA -1B-  CODICI TRATTAMENTO MERCE
345400010420IF  1C     $ERR          IFEQ      'N'
345500010420     C                   Z-ADD     0             I
345600010420     C                   Z-ADD     CODUT         KTBUTE
345700010420     C                   MOVEL     '1B'          KTBCOD
345800010420     C                   MOVEL     *BLANKS       KTBKEY
345900010420     C     KEYTAB        SETLL     TABEL00F                           99
346000010420     C  N99              READ      TABEL00F                               99
346100010420IF  2C     *IN99         IFEQ      '1'
346200010801     C                   MOVEL     MSG(28)       $MSG
346300010420     C                   EXSR      SCRERR
346400010420E   2C                   ENDIF
346500010420DO  2C     *IN99         DOWEQ     '0'
346600010420     C     TBLCOD        ANDEQ     '1B'
346700010420IF  3C     TBLFLG        IFEQ      *BLANKS
346800010420     C                   ADD       1             I
346900010420     C                   MOVEL     TBLKEY        CTM(I)
347000010420     C                   MOVEL     TBLUNI        DS1B
347100010420     C                   MOVEL     §1BF12        CF12(I)
347200050801     C                   MOVEL     §1BF16        CF16(I)
347300030530     C                   MOVEL     §1BDKC        CDKC(I)
347400010420E   3C                   ENDIF
347500010420     C                   READ      TABEL00F                               99
347600010420E   2C                   ENDDO
347700010420E   1C                   ENDIF
347800131002     C*
347900131002     C* CARICA TABELLA -15-  nazioni per reperire cap fittizio
348000131002IF  1C     $ERR          IFEQ      'N'
348100131002     C                   Z-ADD     0             I
348200131002     C                   Z-ADD     CODUT         KTBUTE
348300131002     C                   MOVEL     '15'          KTBCOD
348400131002     C                   MOVEL     *BLANKS       KTBKEY
348500131002     C     KEYTAB        SETLL     TABEL00F                           99
348600131002     C  N99              READ      TABEL00F                               99
348700131002IF  2C     *IN99         IFEQ      '1'
348800131002     C                   MOVEL     MSG(28)       $MSG
348900131002     C                   EXSR      SCRERR
349000131002E   2C                   ENDIF
349100131002DO  2C     *IN99         DOWEQ     '0'
349200131002     C     TBLCOD        ANDEQ     '15'
349300131002IF  3C     TBLFLG        IFEQ      *BLANKS
349400131002     C                   ADD       1             I
349500131002     C                   MOVEL     TBLKEY        d15(I)
349600131002     C                   MOVEL     TBLUNI        DS15
349700131002     C                   MOVEL     §15pcf        pcf(I)
349800131002     C                   MOVEL     §15ecf        ecf(I)
349900131002E   3C                   ENDIF
350000131002     C                   READ      TABEL00F                               99
350100131002E   2C                   ENDDO
350200131002E   1C                   ENDIF
350300091007     C* CARICA TABELLA -2K-  blocco competenza
350400091007IF  1C     $ERR          IFEQ      'N'
350500091007     C                   Z-ADD     0             I
350600091007     C                   Z-ADD     CODUT         KTBUTE
350700091007     C                   MOVEL     '2K'          KTBCOD
350800091007     C                   MOVEL     *BLANKS       KTBKEY
350900091007     C     KEYTAB        SETLL     TABEL00F                           99
351000091007     C  N99              READ      TABEL00F                               99
351100091007IF  2C     *IN99         IFEQ      '1'
351200091007     C                   MOVEL     MSG(28)       $MSG
351300091007     C                   EXSR      SCRERR
351400091007E   2C                   ENDIF
351500091007DO  2C     *IN99         DOWEQ     '0'
351600091007     C     TBLCOD        ANDEQ     '2K'
351700091007IF  3C     TBLFLG        IFEQ      *BLANKS
351800091007     C                   MOVEL     TBLUNI        DS2K
351900091009     C                   Z-ADD     §2KNBC        WNBCD             5 0
352000091007E   3C                   ENDIF
352100091007     C                   READ      TABEL00F                               99
352200091007E   2C                   ENDDO
352300091007E   1C                   ENDIF
352400011221     c*
352500011221     C* DIVISE PER CONVERSIONE -OUTPUT C/ECONOMICO-
352600011221     C                   CLEAR                   DSGEDCE
352700011221     C                   MOVEL     *blanks       WDIVCE            3
352800011221     C                   MOVEL(P)  'GED'         KTECOD
352900011221     C                   MOVEL(P)  'CE'          KTEKE1
353000011221     C     KEYTBE        CHAIN     TNTBE01L
353100011221IF  1C                   IF        %found(TNTBE01L)
353200011221     C                   MOVEL     TBEUNI        DSGEDCE
353300011221E   1C                   ENDIF
353400011221IF  1C                   IF        §GEDCEDAT > DATCOR
353500011221     C                   EVAL      WDIVCE = §GEDCEDIP
353600011221X   1C                   ELSE
353700011221     C                   EVAL      WDIVCE = §GEDCEDIV
353800011221E   1C                   ENDIF
353900011221IF  1C                   IF        WDIVCE = *blanks
354000011221     C                   EVAL      WDIVCE = 'ITL'
354100011221E   1C                   ENDIF
354200011221     c*
354300011221     C* DIVISE PER CONVERSIONE -INPUT ANAGRAFICHE C/ECONOMICO-
354400020102     C                   CLEAR                   DGEDAC
354500011221     C                   MOVEL     *blanks       WDIVAC            3
354600011221     C                   MOVEL(P)  'GED'         KTECOD
354700011221     C                   MOVEL(P)  'AC'          KTEKE1
354800011221     C     KEYTBE        CHAIN     TNTBE01L
354900011221IF  1C                   IF        %found(TNTBE01L)
355000020102     C                   MOVEL     TBEUNI        DGEDAC
355100011221E   1C                   ENDIF
355200011221IF  1C                   IF        §GEDACDAT > DATCOR
355300011221     C                   EVAL      WDIVAC = §GEDACDIP
355400011221X   1C                   ELSE
355500020102     C                   EVAL      WDIVAC = §GEDACDID
355600011221E   1C                   ENDIF
355700011221IF  1C                   IF        WDIVAC = *blanks
355800011221     C                   EVAL      WDIVAC = 'ITL'
355900011221E   1C                   ENDIF
356000941024     C*
356100020213     C* CARICA TABELLA -IC-  CODICI QUALITÀ CLEINTE
356200020213IF  1C     $ERR          IFEQ      'N'
356300020213     C                   Z-ADD     0             I
356400020213     C                   Z-ADD     CODUT         KTBUTE
356500020213     C                   MOVEL     'IC'          KTBCOD
356600020213     C                   MOVEL     *BLANKS       KTBKEY
356700020213     C     KEYTAB        SETLL     TABEL00F                           99
356800020213     C  N99              READ      TABEL00F                               99
356900020213IF  2C     *IN99         IFEQ      '1'
357000020213     C                   MOVEL     MSG(29)       $MSG
357100020213     C                   EXSR      SCRERR
357200020213E   2C                   ENDIF
357300020213DO  2C     *IN99         DOWEQ     '0'
357400020213     C     TBLCOD        ANDEQ     'IC'
357500020213IF  3C     TBLFLG        IFEQ      *BLANKS
357600020213     C                   ADD       1             I
357700020213     C                   MOVEL     TBLKEY        CLV(I)
357800020213E   3C                   ENDIF
357900020213     C                   READ      TABEL00F                               99
358000020213E   2C                   ENDDO
358100020213E   1C                   ENDIF
358200020805     C*
358300020805     C* CARICA TABELLA -NTW- NETWORK
358400020805IF  1C     $ERR          IFEQ      'N'
358500020805     C                   Z-ADD     *ZEROS        I
358600020805     C                   MOVEL(P)  'NTW'         KTECOD
358700020805     C     KEYTB2        SETLL     TNTBE01L                           99
358800020805     C     KEYTB2        READE     TNTBE01L                               99
358900020805IF  2C     *IN99         IFEQ      *ON
359000020805     C                   MOVEL     MSG(30)       $MSG
359100020805     C                   EXSR      SCRERR
359200020805E   2C                   ENDIF
359300020805DO  2C     *IN99         DOWEQ     *OFF
359400020805     C                   MOVEL     TBEUNI        DNTW
359500020805     C                   ADD       1             I
359600020805     C                   MOVEL     TBEKE1        SNTW(I)
359700020805     C                   MOVEL     §NTWFIE       SNFIE(I)
359800020805     C     KEYTB2        READE     TNTBE01L                               99
359900020805E   2C                   ENDDO
360000020805E   1C                   ENDIF
360100040903     C* CARICA TABELLA -DFE- per peso limite tariffa pak fedex
360200040903     c                   clear                   ddfe
360300040903IF  1C     $ERR          IFEQ      'N'
360400040903     C                   MOVEL(P)  'DFE'         KTECOD
360500040903     C                   MOVEL(P)  'FED'         KTEKE1
360600040903     C     KEYTBE        chain     TNTBE01L
360700040903     c                   if        %found(tntbe01l)
360800040903     c                   movel(p)  tbeuni        ddfe
360900040903     c                   end
361000040903E   1C                   ENDIF
361100020805     C*
361200020805     C* CARICA FLAG ITALIA/ESTERO DALL'ORGANIGRAMMA
361300020805if  1C     $ERR          IFEQ      'N'
361400020805     C     *LOVAL        SETLL     AZORG01L
361500020805     C                   READ      AZORG01L                               98
361600020805DO  2C     *IN98         DOWEQ     *OFF
361700020805IF  3C     ORGFVA        IFEQ      *BLANKS
361800020805     C     ORGFAG        ANDNE     'V'
361900020805     C                   Z-ADD     ORGFIL        F                 4 0
362000020805     C                   MOVEL     ORGDE3        OG143
362100020805     C                   MOVEL     ORGCTS        CTS(F)
362200020805     C                   Z-ADD     1             II
362300020805     C     §OGNTW        LOOKUP    SNTW(II)                               97
362400020805IF  4C     *IN97         IFEQ      *OFF
362500020805     C                   MOVEL     MSG(31)       $MSG
362600020805     C                   EXSR      SCRERR
362700020805X   4C                   ELSE
362800020805     C                   MOVEL     SNTW(II)      NTW(F)
362900020805     C                   MOVEL     SNFIE(II)     FIE(F)
363000020805E   4C                   ENDIF
363100020805E   3C                   ENDIF
363200020805     C                   READ      AZORG01L                               98
363300020805E   2C                   ENDDO
363400020805e   1C                   ENDIF
363500020213     C*
363600941024     C                   CLOSE     TABEL00F
363700011221     C                   CLOSE     TNTBE01L
363800020805     C                   CLOSE     AZORG01L
363900940704     C*
364000940704     C                   ENDSR
364100940803     C*------------------------------------------------------------------------*
364200940803     C*? DATCLI - RECUPERA DATI CLIENTE
364300940902     C*           INPUT: - D$DKSC (CODICE DEL CLIENTE)
364400940803     C*           OUTPUT: - WGGP (GIORNI PAGAMENTO)
364500940803     C*                   - WCLV (CLASSE CLIENTE)
364600940803     C*------------------------------------------------------------------------*
364700940722     C     DATCLI        BEGSR
364800971222     C*
364900971222     C* LANCIA PGM PER RECUPERARE DATI CLIENTE
365000971222     C                   CLEAR                   TIBS69
365100971222     C                   Z-ADD     WCLI          I69KIN
365200971222     C                   Z-ADD     WCLI          I69KCP
365300971222     C                   CALL      'TIBS69R'
365400971222     C                   PARM                    TIBS69
365500971222     C                   PARM                    ACODS
365600971222     C                   PARM                    INDDS
365700971222     C                   PARM                    CLPDS
365800971222     C                   PARM                    CLSDS
365900971222IF  1C     O69ERR        IFEQ      '1'
366000020701     C                   MOVE      'BS69'        O69MSG
366100020520     C                   MOVEL     O69MSG        DSMSG
366200971222     C                   MOVEL     O69MSG        $MSG
366300971222     C                   EXSR      SCRERR
366400971222X   1C                   ELSE
366500940722     C*
366600940722     C* RECUPERA GIORNI PAGAMENTO DALL'ANAGRAFICO INDIRIZZI (CNIND)
366700940722     C                   Z-ADD     *ZEROS        WGGP              3 0
366800940802     C                   MOVE      INDCDP        WTM03
366900940722     C                   Z-ADD     1             I
367000940722     C     WTM03         LOOKUP    CFA(I)                                 98
367100971222IF  2C     *IN98         IFEQ      '0'
367200940722     C                   MOVEL     MSG(23)       $MSG
367300940907     C                   EXSR      SCRERR
367400971222X   2C                   ELSE
367500940722     C                   Z-ADD     GFA(I)        WGGP
367600971222E   2C                   ENDIF
367700940722     C*
367800940722     C* RECUPERA LA CLASSE DEL CLIENTE DALL'ANAGRAFICO PARTICOLARITA' (CNCLP)
367900020213     C                   Z-ADD     1             I
368000020213     C     CLPCLV        LOOKUP    CLV(I)                                 98
368100020213IF  2C     *IN98         IFEQ      *ON
368200971229     C                   MOVEL     CLPCLV        WCLV              1
368300971222X   2C                   ELSE
368400940722     C                   MOVEL     MSG(24)       $MSG
368500940907     C                   EXSR      SCRERR
368600971222E   2C                   ENDIF
368700971222     C*
368800971222E   1C                   ENDIF
368900940722     C*
369000940722     C                   ENDSR
369100940831     C*------------------------------------------------------------------------*
369200940831     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
369300940831     C*------------------------------------------------------------------------*
369400940831     C     SUBDAT        BEGSR
369500940831     C*
369600940831     C                   TIME                    DTTIME
369700940831     C                   Z-ADD     DTANN         DSANN
369800940831     C                   Z-ADD     DTMES         DSMES
369900940831     C                   Z-ADD     DTGIO         DSGIO
370000940901     C                   Z-ADD     DTSEC         DSSEC
370100940831     C*
370200940831     C                   ENDSR
370300940907     C*------------------------------------------------------------------------*
370400940907     C*?SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SLSDE (ERRORI SALDI)
370500940907     C*------------------------------------------------------------------------*
370600940907     C     SCRERR        BEGSR
370700940907     C*
370800940913     C                   MOVEL     'S'           $ERR
370900970407     C*
371000970407     C* SE LANCIATO DAL PGM TNTE50R (DELTA IN FILIALE) NON SCRIVO ERRORI
371100970407     C     D$GPGF        IFEQ      'TNTE50R'
371200970407     C                   MOVEL     $MSG          D$GERR
371300970407     C                   MOVEL     $MSG          D$GMSG
371400970407     C                   ELSE
371500941026     C*
371600941026     C* SCRIVE SOLO SE NON VI E' GIA' STATO ALTRO ERRORE (IN QUESTO O ALTRO PGM)
371700941026     C     D$GERR        IFEQ      *BLANKS
371800941026     C*
371900940907     C                   EXSR      SUBDAT
372000940907     C                   Z-ADD     DSDAT8        ERRDAT
372100940907     C                   Z-ADD     DTORA6        ERRTIM
372200941026     C                   ADD       1             D$GPRE
372300941026     C                   Z-ADD     D$GPRE        ERRPRG
372400941213     C                   Z-ADD     D$DAAS        ERRAAS
372500940907     C                   MOVEL     D$DTBL        ERRTBL
372600940907     C                   Z-ADD     D$DLNP        ERRLNP
372700940907     C                   Z-ADD     D$DNRS        ERRNRS
372800940907     C                   Z-ADD     D$DNSP        ERRNSP
372900941201     C                   Z-ADD     D$DAAS        DSAAS
373000941201     C                   Z-ADD     D$DMGS        DSMGS
373100941201     C                   Z-ADD     DSDAT8        ERRDSP
373200941007     C                   MOVEL     $MSG          DSMSG
373300941007     C                   MOVEL     DSDER         ERRDER
373400941007     C                   MOVEL     DSCER         ERRCER
373500950123     C                   MOVEL     D$GSTO        ERRSTS
373600941026     C                   Z-ADD     D$GDAT        ERRDAM
373700941026     C                   Z-ADD     D$GTIM        ERRORM
373800941026     C                   MOVEL     D$GUSR        ERRUSR
373900941026     C                   MOVEL     D$GPGF        ERRPGF
374000950118     C                   MOVEL     D$GFIL        ERRFIL
374100950118     C                   MOVEL     D$GMBR        ERRMBR
374200950118     C                   MOVEL     D$GLIB        ERRLIB
374300991119     C     D$GFIL        IFEQ      'TITAS10F'
374400020703     C     D$GFIL        OREQ      'TITAS00F'
374500950118     C                   MOVEL     D$PRE1        ERRRP1
374600950118     C                   MOVEL     D$PRE2        ERRRP2
374700950118     C                   MOVEL     D$DRE1        ERRRD1
374800950118     C                   MOVEL     D$DRE2        ERRRD2
374900950123     C                   ENDIF
375000950123     C     D$GFIL        IFEQ      'TNCSB00F'
375100950123     C                   MOVEL     D$PCO1        ERRRP1
375200950123     C                   MOVEL     D$PCO2        ERRRP2
375300950123     C                   MOVEL     D$DCO1        ERRRD1
375400950123     C                   MOVEL     D$DCO2        ERRRD2
375500950123     C                   ENDIF
375600030205     C     D$GFIL        IFEQ      'FIAR500F'
375700030205     C     D$GTRD        IFEQ      'TRS'
375800030205     C                   MOVEL     D$PAR5T       ERRRP1
375900030205     C                   MOVEL     D$DAR5T       ERRRD1
376000030205     C                   ENDIF
376100030801     C     D$GTRD        IFEQ      'BAN'
376200030801     C     D$GTRD        OREQ      'BNB'
376300030801     C                   MOVEL     D$PAR5B       ERRRP1
376400030801     C                   MOVEL     D$DAR5B       ERRRD1
376500030801     C                   ENDIF
376600030801     C                   ENDIF
376700030801     C                   WRITE     ECERR
376800941026     C*
376900941026     C                   MOVEL     DSCER         D$GERR
377000941026     C                   MOVEL     DSDER         D$GMSG
377100941026     C*
377200941026     C                   ENDIF
377300970407     C                   ENDIF
377400940907     C*
377500940907     C                   ENDSR
377600940907     C*------------------------------------------------------------------------*
377700940907     C*? RUTINZ - ROUTINE INIZIALE
377800940907     C*------------------------------------------------------------------------*
377900940907     C     RUTINZ        BEGSR
378000940907     C*
378100940907     C* PARAMETRI IN INPUT ALLA ROUTINE
378200940907     C     *ENTRY        PLIST
378300940907     C                   PARM                    DS$ECO
378400121120     c                   movel     d$gxxx        dsd$g
378500121120     c                   if        d$gfsf = 'S' and
378600121120     c                             not %open(esvoc01l)
378700121120     c                   open      esvoc01l
378800121120     c                   open      esvoc11l
378900121120     c                   end
379000940907     C*
379100940907     C* CHIAVE LETTURA TABEL00F - COMPLETA
379200940907     C     KEYTAB        KLIST
379300940907     C                   KFLD                    KTBUTE
379400940907     C                   KFLD                    KTBCOD
379500940907     C                   KFLD                    KTBKEY
379600011221     C*
379700011221     C* CHIAVE LETTURA TNTBE01L - PARZIALE
379800011221     C     KEYTBE        KLIST
379900011221     C                   KFLD                    KTECOD
380000011221     C                   KFLD                    KTEKE1
380100020805     C*
380200020805     C* CHIAVE LETTURA TNTBE01L - PARZIALE
380300020805     C     KEYTB2        KLIST
380400020805     C                   KFLD                    KTECOD
380500091007     C* CHIAVE LETTURA ECECT01L - PARZIALE
380600091007     C     KEYECT        KLIST
380700091007     C                   KFLD                    WNBCD
380800091007     C                   KFLD                    vcmp(v)
380900940907     C*
381000940907     C* CHIAVE LETTURA ECEVT01L - COMPLETA
381100940907     C     KEYEVT        KLIST
381200940907     C                   KFLD                    EVTVOC
381300940907     C                   KFLD                    EVTPRG
381400091009     C* CHIAVE LETTURA ESVOC01L - COMPLETA
381500091009     C     KEYEs         KLIST
381600091009     C                   KFLD                    vocfil
381700091009     C                   KFLD                    vocdat
381800091009     C                   KFLD                    vocvoc
381900091009     C                   KFLD                    vocsgi
382000091009     C                   KFLD                    vocsgf
382100100518     C     KEYEs10       KLIST
382200100518     C                   KFLD                    v10fil
382300100518     C                   KFLD                    v10dat
382400100518     C                   KFLD                    v10voc
382500940907     C*
382600131002     C* CHIAVE LETTURA ecevd06l - COMPLETA
382700940907     C     KEYEVD        KLIST
382800940907     C                   KFLD                    EVDVOC
382900940907     C                   KFLD                    EVDPRG
383000940907     C                   KFLD                    EVDTTV
383100940907     C                   KFLD                    EVDFT1
383200940907     C                   KFLD                    EVDFT2
383300940907     C                   KFLD                    EVDCLI
383400940907     C                   KFLD                    EVDCTR
383500940907     C                   KFLD                    EVDCTS
383600131002     C                   KFLD                    EVDnaz
383700131002     C                   KFLD                    EVDCAd
383800940907     C                   KFLD                    EVDDDT
383900940907     C                   KFLD                    EVDSGI
384000940907     C*
384100940907     C* CHIAVE LETTURA ECEVD02L - COMPLETA
384200940907     C     KEYEV2        KLIST
384300940907     C                   KFLD                    EVDCLI
384400940907     C*
384500940907     C* CHIAVE LETTURA ECEVM01L - COMPLETA
384600940907     C     KEYEVM        KLIST
384700940907     C                   KFLD                    EVMVOM
384800940907     C                   KFLD                    EVMPRM
384900940907     C                   KFLD                    EVMVOC
385000940907     C                   KFLD                    EVMPRG
385100940907     C*
385200940907     C* CHIAVE LETTURA ECERR01L - COMPLETA
385300940907     C     KEYERR        KLIST
385400940907     C                   KFLD                    ERRDAT
385500940907     C                   KFLD                    ERRTIM
385600940907     C                   KFLD                    ERRPRG
385700940907     C*
385800940907     C* DEFINISCO CAMPI
385900940907     C     *LIKE         DEFINE    TBLKUT        KTBUTE
386000940907     C     *LIKE         DEFINE    TBLCOD        KTBCOD
386100940907     C     *LIKE         DEFINE    TBLKEY        KTBKEY
386200011221     C     *LIKE         DEFINE    TBECOD        KTECOD
386300011221     C     *LIKE         DEFINE    TBEKE1        KTEKE1
386400940907     C* CAMPI VARI
386500940907     C     *LIKE         DEFINE    EVTARF        WVOL
386600991008     C     *LIKE         DEFINE    D$DPKF        WPES
386700040427     C     *LIKE         DEFINE    D$DPKF        WDPES
386800940907     C     *LIKE         DEFINE    EVTVOC        WVOC
386900940907     C     *LIKE         DEFINE    EVTPRG        WPRG
387000000306     C     *LIKE         DEFINE    EVTTIV        WTIV
387100940907     C     *LIKE         DEFINE    EVTRFI        WRIF
387200940907     C     *LIKE         DEFINE    EVDTTV        WTTV
387300940907     C     *LIKE         DEFINE    EVDFT1        WFT1
387400940907     C     *LIKE         DEFINE    EVDFT2        WFT2
387500940907     C     *LIKE         DEFINE    EVDCLI        WKSC
387600940907     C     *LIKE         DEFINE    EVDCTR        WCTR
387700940907     C     *LIKE         DEFINE    EVDCTS        WCTS
387800131002     C     *LIKE         DEFINE    EVDnaz        Wnaz
387900131002     C     *LIKE         DEFINE    EVDCAd        wcad
388000940907     C     *LIKE         DEFINE    EVTDDV        WDSP
388100940907     C     *LIKE         DEFINE    EVTDDV        WDDSP
388200940908     C     *LIKE         DEFINE    EVTDDV        WSPDAT
388300940907     C     *LIKE         DEFINE    D$DKSC        WCLI
388400940907     C     *LIKE         DEFINE    D$DLNP        W$LNP
388500111020     C     *LIKE         DEFINE    D$Dlna        W$LNA
388600940907     C     *LIKE         DEFINE    D$DTFP        W$TFP
388700940907     C     *LIKE         DEFINE    D$DKSC        W$KSC
388800940908     C     *LIKE         DEFINE    D$DKSC        WSPKSC
388900940907     C     *LIKE         DEFINE    D$DCCM        W$CCM
389000940907     C     *LIKE         DEFINE    D$DMCT        W$MCT
389100011227     C     *LIKE         DEFINE    EVDSGI        W$SGI
389200011227     C     *LIKE         DEFINE    EVDSGF        W$SGF
389300011227     C     *LIKE         DEFINE    EVDITR        W$ITR
389400011227     C     *LIKE         DEFINE    EVDIMI        W$IMI
389500011227     C     *LIKE         DEFINE    EVDIMA        W$IMA
389600020102     C     *LIKE         DEFINE    YECIMI        §IMI
389700020102     C     *LIKE         DEFINE    YECDVI        §DVI
389800020102     C     *LIKE         DEFINE    YECIMO        §IMO
389900021029     C     *LIKE         DEFINE    §1BF12        WCF12
390000050801     C     *LIKE         DEFINE    §1BF16        WCF16
390100030530     C     *LIKE         DEFINE    §1BDKC        WCDKC
390200950123     C*
390300950123     C* FACCIO LETTURA DI ECERR FASULLA PER NON FARE VENIRE MESSAGGIO
390400950123     C* AD OGNI APERTURA DI DEL FILE
390500950123     C   99
390600950123     CANN99KEYERR        CHAIN     ECERR01L                           99
390700940907     C*
390800940907     C* IMPOSTO CAMPI DI GESTIONE PGM
390900940913     C                   MOVEL     'N'           $ERR              1
391000941006     C                   MOVEL     *BLANKS       $MSG             80
391100940907     C*
391200940907     C* RECUPERO IL CODICE DELL'AZIENDA
391300940907     C                   Z-ADD     1             CODUT             1 0
391400940907     C*
391500940907     C* SE PGM CHIAMATO PER CHIUSURA ESCE
391600991214     C     D$GSTS        IFEQ      'C'
391700991214     C     D$GSTS        OREQ      'S'
391800991214     C                   ELSE
391900940907     C*
392000940907     C* PORTA LE IMMAGINI BOLLA PRIMA E DOPO DALLA DS INPUT ALLA DS PGM
392100940907     C                   MOVEL     D$PRE1        D$PDS1
392200940907     C                   MOVEL     D$PRE2        D$PDS2
392300940907     C                   MOVEL     D$DRE1        D$DDS1
392400940907     C                   MOVEL     D$DRE2        D$DDS2
392500941201     C                   MOVEL     D$ORE1        D$ODS1
392600941201     C                   MOVEL     D$ORE2        D$ODS2
392700940907     C*
392800940907     C* PORTA I RICAVI IN INPUT NELLA SCHIERA DEL PGM
392900950125     C                   MOVEA     D$DCCD        CAN
393000950125     C                   MOVEA     D$DICD        IAN
393100030506DO  2C     1             DO        75            I                 6 0
393200940907     C                   MOVE      CAN(I)        DSWCA
393300940907     C                   Z-ADD     DSWCN         RCD(I)
393400011221     C                   MOVE      IAN(I)        DSWIA
393500940907     C                   Z-ADD     DSWIN         RIM(I)
393600940907E   2C                   ENDDO
393700940907     C*
393800940907     C* IMPOSTA A ZEROS LE SCHIERE IN OUTPUT
393900940907     C                   CLEAR                   CCD
394000940907     C                   CLEAR                   CIM
394100940907     C                   CLEAR                   CIMP
394200940907     C                   CLEAR                   VIMP
394300940907     C                   CLEAR                   Q2R
394400940907     C*
394500940907     C* RECUPERO DATI VARI
394600941201     C                   Z-ADD     D$DAAS        DSAAS
394700941201     C                   Z-ADD     D$DMGS        DSMGS
394800941201     C                   Z-ADD     DSDAT8        WDDSP
394900940907     C*
395000940907     C* CARICA LE TABELLE NECESSARIE AL PGM
395100940907     C* CARICA LE TARIFFE DI CARTELLO
395200940907     C* SOLO NEL CASO IN CUI NON SIANO ANCORA STATE CARICATE
395300940913     C     $ERR          IFEQ      'N'
395400940907     C     CTABSN        IFNE      'S'
395500940907     C     D$GSTS        ANDNE     'C'
395600940907     C                   MOVEL     'S'           CTABSN            1
395700940907     C                   EXSR      CARTAB
395800940907     C                   EXSR      CARCAR
395900940907     C                   EXSR      CARKSC
396000940907     C                   EXSR      CARMAG
396100940907     C                   ENDIF
396200940907     C                   ENDIF
396300940907     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI AL POSTO DEI DATI DOPO
396400940907     C     D$OLNP        IFNE      *ZEROS
396500950913     C     D$DFIG        ANDEQ     'S'
396600940907     C                   Z-ADD     D$OLNP        W$LNP
396700940907     C                   ELSE
396800940907     C                   Z-ADD     D$DLNP        W$LNP
396900940907     C                   ENDIF
397000940907     C     D$OTFP        IFNE      *ZEROS
397100950913     C     D$DFIG        ANDEQ     'S'
397200940907     C                   Z-ADD     D$OTFP        W$TFP
397300940907     C                   ELSE
397400940907     C                   Z-ADD     D$DTFP        W$TFP
397500940907     C                   ENDIF
397600940907     C     D$OKSC        IFNE      *ZEROS
397700950913     C     D$DFIG        ANDEQ     'S'
397800940907     C                   Z-ADD     D$OKSC        W$KSC
397900940907     C                   ELSE
398000940907     C                   Z-ADD     D$DKSC        W$KSC
398100940907     C                   ENDIF
398200940907     C     D$OCCM        IFNE      *ZEROS
398300950913     C     D$DFIG        ANDEQ     'S'
398400940907     C                   Z-ADD     D$OCCM        W$CCM
398500940907     C                   ELSE
398600940907     C                   Z-ADD     D$DCCM        W$CCM
398700940907     C                   ENDIF
398800940907     C     D$OMCT        IFNE      *BLANKS
398900950913     C     D$DFIG        ANDEQ     'S'
399000940907     C                   MOVEL     D$OMCT        W$MCT
399100940907     C                   ELSE
399200940907     C                   MOVEL     D$DMCT        W$MCT
399300940907     C                   ENDIF
399400111020     c* verifico se la linea di arrivo è in £6
399500111020     c                   z-add     d$dlna        w$lna
399600111020     c                   clear                   fnlv55ds
399700111020     c                   z-add     d$dlna        d55lin
399800111020     c                   movel     '6'           d55tpt
399900111020     C                   eval      d55drf = WDDSP
400000111020     c                   if        d$ddti <> 0
400100111020     C                   eval      d55drf = d$ddti
400200111020     c                   end
400300111020     c                   call      'FNLV55R'
400400111020     c                   parm                    fnlv55ds
400500111020     c                   if        d55tfa <> 0
400600111020     c                   eval      w$lna = d55tfa
400700111020     c                   end
400800021029     C*
400900021029     C* CARICA I DATI DEL CLIENTE di fatturazione, SE DIVERSO DALL'ULTIMO LETTO
401000021029     C     $ERR          IFEQ      'N'
401100021029     C     W$KSC         IFNE      WCLI
401200021029     C                   Z-ADD     W$KSC         WCLI
401300021029     C                   EXSR      DATCLI
401400021029     C                   ENDIF
401500021029     C                   ENDIF
401600021029     C*
401700140203     c* calcolo le qta della spedizione per saldi tempi uffici
401800140203     c                   if        D$GFSF = 'S'
401900140203     c                   exsr      srqta
402000140203     c                   end
402100940907     C*
402200940907     C                   ENDIF
402300940907     C*
402400940907     C                   ENDSR
402500940907     C*------------------------------------------------------------------------*
402600940907     C*? STAMPA - STAMPA RIGA
402700940907     C*------------------------------------------------------------------------*
402800941229     C*****+++   STAMPA    BEGSR
402900940902     C*
403000941024     C* APRE IL FILE DI STAMPA
403100941229     C****++++             OPEN PRTF198
403200941024     C*
403300940902     C* RECUPERO LA DATA E L'ORA
403400941229     C*****++++            Z-ADDUDATE     DATA    60
403500941229     C****+++++            TIME           ORA     60
403600940902     C*
403700940902     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
403800941229     C****+++++            EXCPTTESTA
403900940902     C*
404000940902     C* STAMPA RIGA DI DETTAGLIO TEMPI
404100941229     C***++++++            EXCPTRIGA
404200940902     C*
404300940902     C* STAMPA RIGA FINE
404400941229     C****+++++            EXCPTSTAFIN
404500941024     C*
404600941024     C* CHIUDE IL FILE DI STAMPA
404700941229     C****+++++            CLOSEPRTF198
404800941229     C****++++
404900941229     C***++++++            ENDSR
405000940907     O*------------------------------------------------------------------------*
405100941229     O*PRTF198 E  102           TESTA
405200941229     O****++++                            8 'TRECD8R '
405300941229     O****++++                           82 'PRESTAZIONI PGM'
405400941229     O****++++                          150 'Data:'
405500941229     O****++++                 DATA     159 '  /  /  '
405600941229     O****++++                          167 'Ora:'
405700941229     O****++++                 ORA      178 '  .  .  '
405800941229     O****++++                          190 'Pagina:'
405900941229     O****++++                 PAGE  Z  198
406000941229     O*------------------------
406100941229     O****++++E  1             RIGA
406200941229     O****++++                           30 'NUMERO DI BOLLE.....:'
406300941229     O*****++++++++++++++++++++WBOLLE2   40
406400941229     O****++++E  1             RIGA
406500941229     O****++++                           30 'TENT.RICERCA TARIFFA:'
406600941229     O******+++++++++++++++++++WTEEVD2   40
406700941229     O****++++E  1             RIGA
406800941229     O****++++                           30 'TENT.SETLL   TARIFFA:'
406900941229     O******+++++++++++++++++++WSEEVD2   40
407000941229     O****++++E  1             RIGA
407100941229     O****++++                           30 'TENT.LETTURE TARIFFA:'
407200941229     O*****++++++++++++++++++++WLEEVD2   40
407300941229     O****++++E  1             RIGA
407400941229     O****++++                           30 'TENT.CARTELLO.......:'
407500941229     O*****++++++++++++++++++++WTECAR2   40
407600941229     O****++++E  1             RIGA
407700941229     O****++++                           30 'CARTELLO IN MEMORIA.:'
407800941229     O****+++++++++++++++++++++WMECAR2   40
407900941229     O****++++E  1             RIGA
408000941229     O****++++                           30 'CARTELLO LETTE......:'
408100941229     O*****++++++++++++++++++++WLECAR2   40
408200941229     O****++++E  1             RIGA
408300941229     O****++++                           30 'TENT.FILIALE........:'
408400941229     O*****++++++++++++++++++++WTEFIL2   40
408500941229     O****++++E  1             RIGA
408600941229     O****++++                           30 'FILIALE IN MEMORIA..:'
408700941229     O*****++++++++++++++++++++WMEFIL2   40
408800941229     O****++++E  1             RIGA
408900941229     O****++++                           30 'FILIALE LETTE.......:'
409000941229     O*****++++++++++++++++++++WLEFIL2   40
409100941229     O****++++E  1             RIGA
409200941229     O****++++                           30 'TENT.CLIENTE........:'
409300941229     O*****++++++++++++++++++++WTEKSC2   40
409400941229     O****++++E  1             RIGA
409500941229     O****++++                           30 'CLIENTE IN MEMORIA..:'
409600941229     O*****++++++++++++++++++++WMEKSC2   40
409700941229     O****++++E  1             RIGA
409800941229     O****++++                           30 'CLIENTE LETTE.......:'
409900941229     O******+++++++++++++++++++WLEKSC2   40
410000941229     O****++++E  1             RIGA
410100941229     O****++++                           30 'TENT.MAGGIORAZIONI..:'
410200941229     O******+++++++++++++++++++WTEMAG2   40
410300941229     O****++++E  1             RIGA
410400941229     O****++++                           30 'MAGGIOR. IN MEMORIA.:'
410500941229     O******+++++++++++++++++++WMEMAG2   40
410600941229     O****++++E  1             RIGA
410700941229     O****++++                           30 'MAGGIORAZIONI LETTE.:'
410800941229     O*****++++++++++++++++++++WLEMAG2   40
410900940907     O*------------------------
411000941229     O****++++E   60           STAFIN
411100941229     O*****+++                           76 '*** FINE STAMPA ***'
411200940907     O*------------------------------------------------------------------------*
411300941201**   MSG
411400000623TRECD8R- Tabella '7O' non trovata. Avvertire CED.                         TB7O 1
411500941201TRECD8R- Tabella 'FA' non trovata. Avvertire CED.                         TBFA 2
411600941201TRECD8R- Tabella '2R' non trovata. Avvertire CED.                         TB2R 3
411700941201TRECD8R- Tipo tariffa '00' (quintali) non trovata.                        TT00 4
411800941201TRECD8R- Tipo tariffa '01' (colli) non trovata.                           TT01 5
411900941201TRECD8R- Tipo tariffa '02' (n.spedizioni) non trovata.                    TT02 6
412000941201TRECD8R- Tipo tariffa '03' (chilogrammi) non trovata.                     TT03 7
412100941201TRECD8R- Tipo tariffa '04' (volume) non trovata.                          TT04 8
412200951213TRECD8R- Tipo tariffa '09' (stop ritiro) non trovata.                     TT09 9
412300941201TRECD8R- Tipo tariffa '20' (val.limite risarcibile) non trovata.          TT2010
412400941201TRECD8R- Tipo tariffa '21' (imponibile reale) non trovata.                TT2111
412500941201TRECD8R- Tipo tariffa '22' (contrassegno) non trovata.                    TT2212
412600941201TRECD8R- Tipo tariffa '23' (val merce assicur.) non trovata.              TT2313
412700941201TRECD8R- Tipo tariffa '24' (n.giorni pagamento) non trovata.              TT2414
412800941201TRECD8R- Tipo tariffa '25' (imp.comp.sede) non trovata.                   TT2515
412900951213TRECD8R- Tipo tariffa '10' (stop consegna) non trovata.                   TT1016
413000991029TRECD8R- Tipo tariffa '11' (peso x giorni giacenza) non trovata.          TT1117
413100991119TRECD8R- Voce non trovata.  XXX                                           VNTR18
413200941201TRECD8R- Tipo tariffa della voce non trovata in schiera.                  TTNT19
413300941201TRECD8R- Qta x ricerca scaglione non trovata.                             QRSN20
413400941201TRECD8R- Manca tariffa.Voce xxx.999 999                                   MATA21
413500941201TRECD8R- Voce di maggiorazione senza voci da maggiorare.                  VMSV22
413600941201TRECD8R- Condizione di pagamento non trovata.                             CPNT23
413700020213TRECD8R- Codice importanza cliente non trovato.                           APNT24
413800000201TRECD8R- Tipo tariffa '12' (giorni giacenza) non trovata.                 TT1125
413900000623TRECD8R- Codice consegna anomala non trovata                              CANT26
414000010420TRECD8R- Codice trattamento merce non trovata                             TMNT27
414100010801TRECD8R- Tabella '1B' non trovata. Avvertire CED.                         TB1B28
414200020213TRECD8R- Tabella 'IC' non trovata. Avvertire CED.                         TBIC29
414300020805TRECD8R- Tabella 'NTW' non trovata. Avvertire CED.                        TNTW30
414400020805TRECD8R- Network filiale non trovato in tab. 'NTW'. Avvertire CED.        NWNT31
414500021029TRECD8R- Tabella '7R' non trovata. Avvertire CED.                         TB7R32
414600021029TRECD8R- Codice particolarità consegna non trovato                        GMNT33
414700030506TRECD8R- Più di 75 voci calcolate. Avvertire CED.                         P75V34
414800030409TRECD8R- Tipo tariffa '06' (bancali) non trovata.                         TT0635
414900020805TRECD8R-                                                                      36
415000020805TRECD8R-                                                                      37
415100020805TRECD8R-                                                                      38
415200020805TRECD8R-                                                                      39
415300020805TRECD8R-                                                                      40
