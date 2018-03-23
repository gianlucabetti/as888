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
003601151113     FFIAR531C  IF   E           K DISK
003602170317     Ftigcp51l  IF   E           K DISK
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
021400141205     D c7r             S              2    DIM(100)
021500141204     D pin             S              1    DIM(100)
021600010420     D*-------------------
021700010420     D* TRATTAMENTO MERCE
021800010420     D*-------------------
021900010420     D CTM             S              2    DIM(100)
022000010420     D CF12            S              1    DIM(100)
022100050801     D CF16            S              1    DIM(100)
022200030530     D CDKC            S              1    DIM(100)
022300131002     D*-------------------
022400131002     D* nazione
022500131002     D*-------------------
022600131002     D d15             S              3    DIM(1000)
022700131002     D ecf             S              1  0 DIM(1000)
022800131002     D pcf             S              1  0 DIM(1000)
022900131002     D*-------------------
023000020213     D* IMPORTANZA CLIENTE
023100020213     D*-------------------
023200020213     D CLV             S              1    DIM(10)
023300940906     D*------------------------------------------------------------------------*
023400940906     D* INPUT
023500940906     D*------------------------------------------------------------------------*
023600111020     D*-------------------
023700111020     D* DS x pgm £6
023800111020     D*-------------------
023900111020     D fnlv55ds      E DS
024000030422     D*-------------------
024100040903     D* DS DFE tabella tariffe fedex
024200030422     D*-------------------
024300040903     D Ddfe          E DS
024400121120     D*-------------------
024500121120     D* DS dsd$g informazioni x simulazione
024600121120     D*-------------------
024700121120     D Dsd$g         E DS
024800040903     D*-------------------
024900040903     D* DS LETTURA CAMPO EVTXXX
025000040903     D*-------------------
025100040903     D DEVTXXX       E DS
025200021028     D*-------------------
025300021028     D* DS LETTURA CAMPO TASFLO (D$DFLO)
025400021028     D*-------------------
025500021028     D DTASFLO       E DS
025501151116     D*-------------------
025502151116     D* DS ESTERNA IN INPUT record GEN FIAR5
025503151116     D*-------------------
025504151116     D Dar5gen       E DS
025600940906     D*-------------------
025700930323     D* DS ESTERNA IN INPUT AL PGM
025800940906     D*-------------------
025900940822     D DS$ECO        E DS
026000971222     D*-------------------
026100971222     D* DS PER LANCIO TIBS69R (RICERCA SU CNIND/CNCLP)
026200971222     D*-------------------
026300971222     D TIBS69        E DS                  EXTNAME(TIBS69DS)
026400971222     D ACODS         E DS                  EXTNAME(CNACO00F)
026500971222     D INDDS         E DS                  EXTNAME(CNIND00F)
026600971222     D CLPDS         E DS                  EXTNAME(CNCLP00F)
026700971222     D CLSDS         E DS                  EXTNAME(FNCLS00F)
026800940906     D*-------------------
026900991119     D* DS ESTERNA PER RIDEFINIRE IMMAGINE PRIMA E DOPO DI TITAS10F
027000940906     D*-------------------
027100941201     D TRECDS        E DS
027200940405     D  D$PDS1                 1    250
027300940405     D  D$PDS2               251    500
027400940405     D  D$DDS1               501    750
027500940405     D  D$DDS2               751   1000
027600941201     D  D$ODS1              1001   1250
027700941201     D  D$ODS2              1251   1500
027800940906     D*-------------------
027900940721     D* DS ESTERNA TABELLA 2R (TIPI RIFERIM. VOCE)
028000940906     D*-------------------
028100940721     D DS2R          E DS
028200940906     D*-------------------
028300940720     D* DS ESTERNA TABELLA FA (CONDIZIONI DI PAGAMENTO)
028400940906     D*-------------------
028500940720     D DSFA          E DS
028600000202     D*-------------------
028700000202     D* DS ESTERNA DESCRIZIONE 43 ORGANIGRAMMA
028800000202     D*-------------------
028900000202     D OG143         E DS
029000000623     D*-------------------
029100141204     D* DS ESTERNA TABELLA 7R (PARTICOLARITÀ CONSEGNA)
029200000623     D*-------------------
029300141204     D DS7R          E DS
029400010420     D*-------------------
029500021029     D* DS ESTERNA TABELLA 1B (CODICI TRATTAMENTO MERCE)
029600010420     D*-------------------
029700010420     D DS1B          E DS
029800131002     D*-------------------
029900131002     D* DS ESTERNA TABELLA 15 (CODICI nazioni)
030000131002     D*-------------------
030100131002     D DS15          E DS
030200091007     D*-------------------
030300091007     D* DS ESTERNA TABELLA 2K (blocco competenza)
030400091007     D*-------------------
030500091007     D DS2k          E DS
030600011221     D*-------------------
030700011221     D* DS DIVISE PER CONVERSIONI
030800011221     D*-------------------
030900011221     D DSGEDCE       E DS
031000020102     D DGEDAC        E DS
031100020805     D*-------------------
031200020805     D* DS NETWORK
031300020805     D*-------------------
031400020805     D DNTW          E DS
031500011221     D*-------------------
031600011221     D* CONVERSIONI IMPORTI
031700011221     D*-------------------
031800011221     D YEUR          E DS                  EXTNAME(YEURCODS)
031900940906     D*-------------------
032000930325     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
032100940906     D*-------------------
032200940831     D DS§DAT          DS
032300940831     D  DSSEC                  1      2  0
032400940831     D  DSANN                  3      4  0
032500941201     D  DSAAS                  1      4  0
032600940831     D  DSMES                  5      6  0
032700940831     D  DSGIO                  7      8  0
032800941201     D  DSMGS                  5      8  0
032900940831     D  DSDAT6                 3      8  0
033000940831     D  DSDAT8                 1      8  0
033100940831     D DS§TIM          DS
033200940831     D  DTORA                  1      2  0
033300940831     D  DTMIN                  3      4  0
033400940901     D  DTSCN                  5      6  0
033500940831     D  DTGIO                  7      8  0
033600940831     D  DTMES                  9     10  0
033700940901     D  DTSEC                 11     12  0
033800940901     D  DTANN                 13     14  0
033900940831     D  DTORA6                 1      6  0
034000940901     D  DTTIME                 1     14  0
034100940906     D*-------------------
034200940906     D* DS PER PASSAGGIO CAMPI PACKED -CODICE-
034300940906     D*-------------------
034400940825     D                 DS
034500940825     D  DSWCN                  1      3P 0
034600940825     D  DSWCA                  1      3
034700090904     D  DSWIN                  4     13P 5
034800011221     D  DSWIA                  4     13
034900940906     D*-------------------
035000940729     D* DS PER MESSAGGIO MANCA TARIFFA
035100940906     D*-------------------
035200940729     D                 DS
035300940729     D  DSMSG                  1     80
035400941006     D  DSVOCE                29     31
035500941006     D  DSTT1                 33     35
035600941006     D  DSTT2                 37     39
035700950519     D  DSCTS                 41     42
035800941007     D  DSDER                  1     74
035900941007     D  DSCER                 75     78
036000011221     D*-------------------
036100011221     D* DS  CONTROLLO DATA (8)
036200011221     D*-------------------
036300011221     D WLBDA8          DS                  INZ
036400011221     D  G08DAT                 1      8  0
036500011221     D  G08INV                 9     16  0
036600011221     D  G08ERR                17     17
036700011221     D  G08TGI                18     22  0
036800091012     d* campi di comodo
036900091012     d pkfvoc          s                   like(vocpkf)
037000091012     d vlfvoc          s                   like(vocvlf)
037100091012     d banvoc          s                   like(vocban)
037200141111     d X               s                   like(I)
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
055401170316     C*    - DIROTTAMENTO DPD  (F*)
055402170316     c                   Z-ADD     W$LNP         F
055500020621IF 1aC                   IF        D$DTBL <> 'A3' OR
055600020621     C                             (D$DTBL = 'A3' AND D$DCBO = 'FO') OR
055700030630     C                             (D$DTBL = 'A3' AND D$DCBO = 'FY') OR
055800040520     C                             (D$DTBL = 'A3' AND D$DCBO = 'FF') OR
055900050801     C                             (D$DTBL = 'A3' AND D$DCBO = 'FI') OR
055901170316     C                             (D$DTBL = 'A3' AND D$DCBO = 'F*' and
055902170316     C                              NTW(F) = 'DPD') OR
056000030630     C                             (D$DTBL = 'A3' AND D$DCBO = 'FT')
056100020621     C*
056200020621     C* VOCE COMETENZA DELTA ======>
056300020621     C*    SEMPRE DA ATTIVARE SE LA BOLLA PASSA DA QUI
056400020621     C                   MOVEL     'S'           VC(900)
056402150421IF  0C     WDDSP         ifgt      20181231
056403150421     C                   MOVEL     'S'           VC(700)
056404150421     C                   MOVEL     'S'           VC(701)
056405150421     C                   MOVEL     'S'           VC(702)
056406150421     C                   MOVEL     'S'           VC(703)
056407150421     C                   MOVEL     'S'           VC(400)
056408150421     C                   MOVEL     'S'           VC(401)
056409150421     C                   MOVEL     'S'           VC(402)
056410150421     C                   MOVEL     'S'           VC(403)
056411150421     c                   end
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
059301170316     C*
059302170316     c* CONTROLLO SE ESISTE =======> DIROTTAMENTO DPD
059303170316     c                   Z-ADD     W$LNP         F
059304170316IF  0C     WDDSP         ifgt      20170400
059305170316if  1c     D$DTBL        andeq     'A3'
059306170316     c     D$DCBO        ANDEQ     'F*'
059307170316     C     NTW(F)        andeq     'DPD'
059308170316     C                   MOVEL     'S'           VC(273)
059309170316E   1C                   ENDIF
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
061101150421IF  0C     WDDSP         ifgt      20181231
061102150421     C                   MOVEL     'S'           VC(346)
061200030604E   1C                   ENDIF
061201150421E   1C                   ENDIF
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
064201160726     C     WCLV          OREQ      'E'
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
065701160726     C     WCLV          OREQ      'E'
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
067301160726     C     WCLV          OREQ      'E'
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
071501160401     c* voci personalizzate per logistica
071600160401IF  0C     WDDSP         ifgt      20160100
071700160401     C                   MOVEL     'S'           VC(350)
071701160401     C                   MOVEL     'S'           VC(351)
071702160401     C                   MOVEL     'S'           VC(352)
071703160401     C                   MOVEL     'S'           VC(353)
071704160401     C                   MOVEL     'S'           VC(354)
071705160401     C                   MOVEL     'S'           VC(355)
071706160401     C                   MOVEL     'S'           VC(356)
071707160401     C                   MOVEL     'S'           VC(357)
071708160401     C                   MOVEL     'S'           VC(358)
071709160401     C                   MOVEL     'S'           VC(359)
071710160401     c                   end
071711170303IF  0C     WDDSP         ifgt      20170200
071712170303     C                   MOVEL     'S'           VC(360)
071713170303     c                   end
071714160401     c*
071715160401IF  0C     WDDSP         ifgt      20181231
071716160401     C                   MOVEL     'S'           VC(85)
071800110321     C                   MOVEL     'S'           VC(200)
071801151214     c* SERVIZI DI SEDE (A SPEDIZ.) per tipologia cliente
071802151214     c                   select
071803151214     c                   when      wclv = 'D'
071804151214     C                   MOVEL     'S'           VC(315)
071805151214     c                   when      wclv = 'T'
071806151214     C                   MOVEL     'S'           VC(316)
071807151214     c                   when      wclv = 'A'
071808151214     C                   MOVEL     'S'           VC(317)
071809151214     c                   when      wclv = 'B'
071810151214     C                   MOVEL     'S'           VC(318)
071811160726     c                   when      wclv = 'C' or wclv = 'E'
071812151214     C                   MOVEL     'S'           VC(319)
071813151214     c                   endsl
071900110321     c                   end
072000050926     c* merce di valore
072100050926IF  1C     §FLOTBA       IFEQ      'V'
072200050926     C                   MOVEL     'S'           VC(19)
072300050926     C                   MOVEL     'S'           VC(21)
072400110321     c                   end
072401151113     c* orm telefonico
072402151113IF  1C     §FLOADO       IFEQ      'S'
072403151113IF  0C     WDDSP         andge     20160101
072404151203     C                   MOVEL     'S'           VC(83)
072406151113     c                   end
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
109502151118IF  2C     WDDSP         ifle      20151231
109600050801     C                   MOVEL     'S'           VC(189)
109700151118E   1C                   Else
109701151118     c                   exsr      srpack
109702151116IF  1C     flgpackl      IFEQ      'S'
109704151209     C                   MOVEL     'S'           VC(189)
109705151116     c                   end
109706151118     c                   end
109707151118     c                   end
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
114301170322     C                   MOVEL     'S'           VC(261)
114302170323IF  2C     WDDSP         ifgt      20170500
114303170323     c* da maggio 2017 non vengono date le competenze alla coop per
114304170317     c* la merce portata a magazzino giacenza nel caso in cui la merce
114305170317     c* resti in IMA (le data della "lista merce portata a mag" restan      o
114306170317     c* a 0 oppure 99999999 quando viene chiusa la giacenza)
114307170317     c                   clear                   gcpfrg
114308170317     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI x AGGANCIARE LA   DOPO
114309170317     C* GIACENZA
114310170317     C     D$OLNP        IFNE      *ZEROS
114311170317     c     kgiaco        chain     tigcp51l
114312170317     c                   else
114313170317     c     kgiac         chain     tigcp51l
114314170317     c                   end
114315170317     c                   if        %found(tigcp51l) and gcpdlma <> 0 and
114316170317     c                             gcpdlma <> 99999999
114500940728     C                   MOVEL     'S'           VC(262)
114600040427     C     wdpes         IFLE      100
114700000201     C                   MOVEL     'S'           VC(267)
114800000201     C                   ELSE
114900000201     C                   MOVEL     'S'           VC(265)
115000110321     C                   ENDIF
115001170317     C                   ENDIF
115002170323     C                   ELSE
115003170323     c* prima di maggio 2017
115004170323     C                   MOVEL     'S'           VC(262)
115005170323     C     wdpes         IFLE      100
115006170323     C                   MOVEL     'S'           VC(267)
115007170323     C                   ELSE
115008170323     C                   MOVEL     'S'           VC(265)
115009170323     C                   ENDIF
115010170323     C                   ENDIF
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
120700160726     c                   when      wclv = 'B' or wclv = 'C' or wclv = 'E'
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
122301150421E   3C                   ENDIF
122400011203     C                   ENDIF
122500110321     c                   else
122600110321     C     D$DFTC        IFEQ      'A'
122700110321     C     D$DTC2        OREQ      'A'
122800110321IF  3C     D$DFTC        OREQ      'S'
122900110321     C     D$DTC2        OREQ      'S'
123000110321     C                   MOVEL     'S'           VC(240)
123001150422IF  0C     WDDSP         ifgt      20181231
123002150422     C                   MOVEL     'S'           VC(347)
123003150422E   3C                   ENDIF
123100110321     c                   end
123200110321     c                   end
123201170316     C*
123202170316     C* CONTROLLO SE ESISTE =======> FUORI MISURA X EXPORT DPD
123203170316     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "F"
123204170316     C                   Z-ADD     W$lna         F1
123205170316     c     NTW(F1)       IFEQ      'DPD'
123206170316     C     D$DTPO        andeq     'F'
123207170316IF  0C     WDDSP         ANDGT     20170400
123208170316     C     D$DFTC        IFEQ      'F'
123209170316     C     D$DTC2        OREQ      'F'
123210170316     C                   MOVEL     'S'           VC(277)
123211170316E   3C                   ENDIF
123212170316E   3C                   ENDIF
123300940722     C*
123400940722     C* CONTROLLO SE ESISTE =======> CONSEGNA AI SUPERMERCATI
123500970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "S"
123600940722     C     D$DFTC        IFEQ      'S'
123700970228     C     D$DTC2        OREQ      'S'
123800980119     C                   MOVEL     'S'           VC(242)
123900980119     C                   MOVEL     'S'           VC(247)
124000141110     C                   MOVEL     'S'           VC(609)
124001150421IF  0C     WDDSP         ifgt      20181231
124002150421     C                   MOVEL     'S'           VC(345)
124100110321     C                   ENDIF
124101150421     C                   ENDIF
124200940722     C*
124300940722     C* CONTROLLO SE ESISTE =======> CONSEGNA AI PIANI
124400970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "P"
124500940722     C     D$DFTC        IFEQ      'P'
124600970228     C     D$DTC2        OREQ      'P'
124700940727     C                   MOVEL     'S'           VC(241)
124800940722     C                   ENDIF
124801150422     C* CONTROLLO SE ESISTE =======> CONSEGNA EXPO
124802150422     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "Z"
124803150422IF  0C     WDDSP         ifge      20150501
124804150422     C     D$DFTC        IFEQ      'Z'
124805150422     C     D$DTC2        OREQ      'Z'
124806150422     C                   MOVEL     'S'           VC(348)
124807150422     C                   ENDIF
124808150422     C                   ENDIF
124900020620     C*
125000020620     C* CONTROLLO SE ESISTE =======> CONSEGNA DISAGIATA
125100020620     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "X"
125200020620     C     D$DFTC        IFEQ      'X'
125300020620     C     D$DTC2        OREQ      'X'
125400020620     C                   MOVEL     'S'           VC(534)
125500020620     C                   MOVEL     'S'           VC(535)
125600020620     C                   ENDIF
125700940722     C*
125800940722     C* CONTROLLO SE ESISTE =======> CONTRASSEGNO
125900940722     C*    IMPORTO CONTRASSEGNO > ZERO
126000941201     C     D$DCAS        IFGT      *ZEROS
126100111020     C                   Z-ADD     W$lna         F
126200060223     c* arrivi
126300020805     C     FIE(F)        IFEQ      'E'
126400000203     C                   MOVEL     'S'           VC(256)
126500000203     C                   MOVEL     'S'           VC(257)
126600000203     C                   MOVEL     'S'           VC(280)
126700000203     C                   ELSE
126800940727     C                   MOVEL     'S'           VC(251)
126900940727     C                   MOVEL     'S'           VC(252)
127000110321     c* abilitazione c/assegno
127100130930IF  0C     WDDSP         ifgt      20181231
127200110321IF  1C     §FLOfca       andeq     'S'
127300110321     C                   MOVEL     'S'           VC(259)
127400000203     C                   ENDIF
127500110321     C                   ENDIF
127600060223     c**** mittente
127700941201     C     D$DTPI        IFEQ      'M'
127800060223     C                   MOVEL     'S'           VC(254)
127900941021     C                   ELSE
128000060223     c**** non mittente
128100941021     C                   MOVEL     'S'           VC(253)
128200941021     C                   ENDIF
128300130930     c*
128400090417     C                   IF        (d$dsop <> '§' and
128500090417     C                             d$dsop <> '%' and
128600130930     C                             d$dsop <> ':' )
128700090417     C                   MOVEL     'S'           VC(250)
128800090417     c                   end
128900940722     C                   ENDIF
129000940728     C*
129100110321     C* CONTROLLO SE ESISTE =======> PORTO ASSEGNATO UFF.ARR.
129200110321     C     D$DTPO        IFEQ      'A'
129300130930IF  2C     wddsp         andgt     20181231
129400110321     C                   MOVEL     'S'           VC(312)
129500110321     C                   ENDIF
129600110321     C* CONTROLLO SE ESISTE =======> PORTO ASSEGNATO UFF.ARR. E PADRONCINO
129700110321     C*    ASSEGNATO CON FATTURA IMMEDIATA
129800941201     C     D$DTPO        IFEQ      'A'
129900030529     C     D$DFIV        ANDGT     0
130000030529     C     D$DFIV        ANDLT     900
130100940728     C                   MOVEL     'S'           VC(310)
130200940728     C                   MOVEL     'S'           VC(311)
130300110321     C                   ENDIF
130400941028     C*
130500990420     C* CONTROLLO SE ESISTE =======> CONSEGNA A MAGAZZINO (RIDUZIONE)
130600941028     C*    FLAG MERCE PORTATA A MAGAZZINO DAL CLIENTE = 'SI'
130700990420     C*    OPPURE ANTEPORTO = I (RITIRO ISOLA)
130800941209     C     D$DFDN        IFEQ      'S'
130900990420     C     D$DFAP        OREQ      'I'
131000010116     C                   MOVEL     'S'           VC(521)
131100001206     C                   ENDIF
131200141204     C*
131300141204     C* PARTICOLARITÀ CONSEGNA PINCODE
131400141204     C                   IF        D$DGMA <> *BLANKS
131500141204     C                   Z-ADD     1             I
131600141204     C     D$DGMA        LOOKUP    C7R(I)                                 98
131700141204IF  2C     *IN98         IFEQ      *OFF
131800141205     C                   MOVEL     MSG(32)       $MSG
131900141204     C                   EXSR      SCRERR
132000141204X   2C                   ELSE
132100141204     C                   IF        PIN(I) = 'S'
132200141204     C                   MOVEL     'S'           VC(248)
132300141204     C                   END
132400141204X   2C                   END
132500141204X   2C                   END
132600960214     C*
132700960214     C* CONTROLLO SE ESISTE =======> CONSEGNA A PRIVATI (DAL FLAG BOLLA)
132800960214     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'P'
132900960214     C                   MOVEL     D$DGVA        WTM01             1
133000030422     C*
133100030422     C* CONTROLLO SE ESISTE =======> GESTIONE BANCALI
133200030422     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'B'
133300030422     C                   MOVEL     D$DGVA        WTM01
133400030422     C     WTM01         IFEQ      'B'
133401150227IF  2C     wddsp         andlt     20150323
133500030422     C                   MOVEL     'S'           VC(17)
133600030422     C                   MOVEL     'S'           VC(119)
133700030422     C                   MOVEL     'S'           VC(142)
133800030422     C                   MOVEL     'S'           VC(186)
133900030430     C                   MOVEL     'S'           VC(213)
134000030422     C                   ENDIF
134001150227     c*
134002150227     C     WTM01         IFEQ      'B'
134003150227IF  2C     wddsp         andge     20150323
134004150227     c* eur
134005150227     c                   if        D$DAR5TBa = 1 or
134006150227     c                             D$DAR5TB2 = 1
134007150227     C                   MOVEL     'S'           VC(342)
134008150227cc   c                   end
134009150227     C* epal
134010150227     c                   if        D$DAR5TBa = 2 or
134011150227     c                             D$DAR5TB2 = 2
134012150227     C                   MOVEL     'S'           VC(343)
134013150227     c                   end
134014150227     C* leggeri
134015150227     c                   if        D$DAR5TBa = 3 or
134016150227     c                             D$DAR5TB2 = 3
134019150227     C                   MOVEL     'S'           VC(344)
134020150227     c                   end
134021150227     C                   ENDIF
134100030725     C*
134200030801     C* CONTROLLO SE ESISTE =======> Spedizioni oltre 20 colli bancalatura Coop
134300030801     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'O'
134301150227     c* non attiva se serie perchè è la logiostica che imbancala
134400030801     C                   MOVEL     D$DGVA        WTM01             1
134500030801     C     WTM01         IFEQ      'O'
134501150227     C     wddsp         iflt      20150323
134502150227     C     wddsp         orge      20150323
134503150227     C     d$dnrs        andeq     0
134600030801     C                   MOVEL     'S'           VC(34)
134700030801     C                   ENDIF
134701150227     C                   ENDIF
134800960214     C*
134900960214     C* CONTROLLO SE ESISTE =======> SECONDA LINEA DISTRIBUZIONE
135000030430     C*    SECONDO CARATTERE FLAG PARTICOLARITA' VARIE = 'D' e
135100010116     C*                LINEA ARRIVO = TERMINAL ARRIVO  e
135200010116     C*                C.TASSAZIONE DESTINO BOLLA <> C.TASSAZIONE LINEA ARRIVO
135300001204     C                   MOVE      D$DGVA        WTM01             1
135400001204IF  1C     WTM01         IFEQ      'D'
135500111020IF  3C     W$lna         IFEQ      D$DTFA
135600111020     C                   Z-ADD     W$lna         F
135700010116IF  4C     D$DCTS        IFNE      CTS(F)
135800001201     C                   MOVEL     'S'           VC(165)
135900001201     C                   MOVEL     'S'           VC(178)
136000110321E   4C                   ENDIF
136100010116E   3C                   ENDIF
136200001204E   1C                   ENDIF
136300970429     C*
136400970429     C* CONTROLLO SE ESISTE =======> DDT (DOCUM. DI TRASP. DA CONS. AL DEST.)
136500970429     C*    FLAG LIBERO 1  = 'S' O 'C'
136600970429     C     D$DLL1        IFEQ      'S'
136700970429     C     D$DLL1        OREQ      'C'
136800970429     C                   MOVEL     'S'           VC(72)
136900970429     C                   MOVEL     'S'           VC(182)
137000970429     C                   ENDIF
137100990222     C*
137200990222     C* CONTROLLO SE ESISTE =======> CONSEGNA LOCALITA DISAGIATE
137300990222     C*    SE FLAG INOLTRO 'D' E NON C'E' FERMO DEPOSITO
137400030430     C     D$DFFD        IFNE      'S'
137500030430     C     D$DFIN        ANDEQ     'D'
137600030430     C                   MOVEL     'S'           VC(246)
137700030430     C                   ENDIF
137800980202     C*
137900030430     C* CONTROLLO SE ESISTE =======>
138000030430     C                   MOVEL     'S'           VC(212)
138100980204     C                   MOVEL     'S'           VC(270)
138200010116     C                   MOVEL     'S'           VC(523)
138300010116     C                   MOVEL     'S'           VC(594)
138400021031     C                   MOVEL     'S'           VC(595)
138500000721     C                   MOVEL     'S'           VC(592)
138600000721     C                   MOVEL     'S'           VC(274)
138700001201     C                   MOVEL     'S'           VC(23)
138800001201     C                   MOVEL     'S'           VC(161)
138900001201     C                   MOVEL     'S'           VC(177)
139000980317     C*
139100980318     C* CONTROLLO SE ESISTE =======> AFFLUENZA/DEFLUENZA SU STESSO TERMINAL
139200980317     C*    SE TERM.PART. = TERM.ARR. E LINEA ARR. <> LINEA PART.
139300980318     C     W$TFP         IFEQ      D$DTFA
139400980318     C     W$LNP         ANDNE     W$TFP
139500111020     C     W$lna         ANDNE     D$DTFA
139600980319     C                   MOVEL     'S'           VC(560)
139700980317     C                   ENDIF
139800980317     C*
139900980319     C* CONTROLLO SE ESISTE =======> LINEA PART. = TERM. EXPORT
140000980317     C*    SE TERM.PART. = TERM.ARR. = LINEA PART. E EXPORT
140100111020     C                   Z-ADD     W$lna         F
140200980318     C     W$TFP         IFEQ      D$DTFA
140300980318     C     W$LNP         ANDEQ     W$TFP
140400020805     C     FIE(F)        ANDEQ     'E'
140500980319     C                   MOVEL     'S'           VC(561)
140600980317     C                   ENDIF
140700980717     C*
140800980717     C* CONTROLLO SE ESISTE =======> LINEA PART. E' 2°LIV. TERM. EXPORT
140900980717     C*    SE TERM.PART.=TERM.ARR. E LINEA PART.<>TERM.PART. E EXPORT
141000111020     C                   Z-ADD     W$lna         F
141100980717     C     W$TFP         IFEQ      D$DTFA
141200980717     C     W$LNP         ANDNE     W$TFP
141300020805     C     FIE(F)        ANDEQ     'E'
141400980717     C                   MOVEL     'S'           VC(570)
141500980717     C                   ENDIF
141600980717     C*
141700980717     C* CONTROLLO SE ESISTE =======> LINEA ARR. E' 2°LIV. TERM. IMPORT
141800980717     C*    SE TERM.PART.=TERM.ARR. E LINEA ARR.<>TERM.ARR. E IMPORT
141900980717     C                   Z-ADD     W$LNP         F
142000980717     C     W$TFP         IFEQ      D$DTFA
142100111020     C     W$lna         ANDNE     D$DTFA
142200020805     C     FIE(F)        ANDEQ     'E'
142300980717     C                   MOVEL     'S'           VC(571)
142400980717     C                   ENDIF
142500990217     C*
142600990420     C* CONTROLLO SE ESISTE =======> CONSEGNA ISOLA
142700990219     C*    SE FLAG INOLTRO 'I' E NON C'E' FERMO DEPOSITO
142800111020     C                   Z-ADD     W$lna         F
142900000302IF  1C     D$DFFD        IFNE      'S'
143000990219     C     D$DFIN        ANDEQ     'I'
143100020805IF  2C     FIE(F)        IFEQ      'E'
143200020805IF  3C     NTW(F)        IFEQ      'DPD'
143300000302     C                   MOVEL     'S'           VC(288)
143400000302E   3C                   ENDIF
143500000302X   2C                   ELSE
143600000302     C                   MOVEL     'S'           VC(278)
143700000302E   2C                   ENDIF
143800000302E   1C                   ENDIF
143900990217     C*
144000990420     C* CONTROLLO SE ESISTE =======> RITIRO ISOLA
144100990217     C*    SE FLAG ANTEPORTO 'I' E NON C'E' CONSEGNA A MAGAZZINO
144200000302     C                   Z-ADD     W$LNP         F
144300000302IF  1C     D$DFDN        IFNE      'S'
144400990219     C     D$DFAP        ANDEQ     'I'
144500020805IF  2C     FIE(F)        IFEQ      'E'
144600020805IF  3C     NTW(F)        IFEQ      'DPD'
144700000302     C                   MOVEL     'S'           VC(289)
144800000302E   3C                   ENDIF
144900000302X   2C                   ELSE
145000990420     C                   MOVEL     'S'           VC(279)
145100130930     c* COMP.RITIRI ISOLE VENEZIA
145200040617     C                   MOVEL     'S'           VC(285)
145300000302E   2C                   ENDIF
145400000302E   1C                   ENDIF
145500990217     C*
145600990217     C* CONTROLLO SE ESISTE =======> PREPAGATI
145700990217     C*    SE CLIENTE 8888
145800990217     C                   MOVE      W$KSC         KSCIS             4 0
1459009902171    C     KSCIS         IFEQ      8888
146000990217     C                   MOVEL     'S'           VC(266)
1461009902171    C                   ENDIF
146200020621     C*
146300020621E  1aC                   ENDIF
146400000623E   0C                   ENDIF
146500000630     C*
146600940526     C                   ENDSR
146700940908     C*------------------------------------------------------------------------*
146800940908     C*?CAL2R  - CALCOLA LE QTA DELLA TABELLA 2R (TIPO TARIFFA)
146900940908     C*------------------------------------------------------------------------*
147000940908     C     CAL2R         BEGSR
147100940908     C* TIPO -01- COLLI -------------------------------------------------------*
147200940908     C                   Z-ADD     1             I
147300940908     C     '01'          LOOKUP    C2R(I)                                 98
147400940908     C     *IN98         IFEQ      '0'
147500940908     C                   MOVEL     MSG(5)        $MSG
147600940908     C                   EXSR      SCRERR
147700940908     C                   ELSE
147800940908     C                   Z-ADD     D$DNCL        Q2R(I)
147900940908     C                   ENDIF
148000940908     C*
148100940908     C* TIPO -02- SPEDIZIONI --------------------------------------------------*
148200940908     C                   Z-ADD     1             I
148300940908     C     '02'          LOOKUP    C2R(I)                                 98
148400940908     C     *IN98         IFEQ      '0'
148500940908     C                   MOVEL     MSG(6)        $MSG
148600940908     C                   EXSR      SCRERR
148700940908     C                   ELSE
148800940908     C                   Z-ADD     1             Q2R(I)
148900940908     C                   ENDIF
149000940908     C*
149100940908     C* TIPO -03- CHILOGRAMMI -------------------------------------------------*
149200040428     C* SE I COLLI=COLLI CML PRENDO PESO CML
149300040428IF  2C     D$DPKC        IFGT      *ZEROS
149400040428     C     D$DNCL        ANDEQ     D$DNCP
149500040428     C                   Z-ADD     D$DPKC        WPES
149600040428X   2C                   ELSE
149700040428     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
149800040428IF  3C     D$DPKF        IFGT      D$DPKC
149900040428     C                   Z-ADD     D$DPKF        WPES
150000040428X   3C                   ELSE
150100040428     C                   Z-ADD     D$DPKC        WPES
150200040428E   3C                   ENDIF
150300040428E   2C                   ENDIF
150400130930     c*
150500940908     C                   Z-ADD     1             I
150600940908     C     '03'          LOOKUP    C2R(I)                                 98
150700940908     C     *IN98         IFEQ      '0'
150800940908     C                   MOVEL     MSG(7)        $MSG
150900940908     C                   EXSR      SCRERR
151000940908     C                   ELSE
151100991008     C                   Z-ADD     WPES          Q2R(I)
151200940908     C                   ENDIF
151300940908     C*
151400940908     C* TIPO -04- VOLUME ------------------------------------------------------*
151500990422     C* SE VOLUME FATTURATO E' 'T' (TOTALE COLLI) PRENDO QUELLO
151600990421IF  1C     D$DFVF        IFEQ      'T'
151700990421     C                   Z-ADD     D$DVLF        WVOL
151800990421X   1C                   ELSE
151900990422     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
152000990421IF  2C     D$DVLC        IFGT      *ZEROS
152100990421     C     D$DNCL        ANDEQ     D$DNCR
152200990422     C                   Z-ADD     D$DVLC        WVOL
152300990421X   2C                   ELSE
152400990422     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
152500990422IF  3C     D$DVLF        IFGT      D$DVLC
152600990422     C                   Z-ADD     D$DVLF        WVOL
152700990421X   3C                   ELSE
152800990422     C                   Z-ADD     D$DVLC        WVOL
152900990421E   3C                   ENDIF
153000990421E   2C                   ENDIF
153100990421E   1C                   ENDIF
153200990421     C*
153300940908     C                   Z-ADD     1             I
153400940908     C     '04'          LOOKUP    C2R(I)                                 98
153500940908     C     *IN98         IFEQ      '0'
153600940908     C                   MOVEL     MSG(8)        $MSG
153700940908     C                   EXSR      SCRERR
153800940908     C                   ELSE
153900940908     C                   Z-ADD     WVOL          Q2R(I)
154000940908     C                   ENDIF
154100030409     C*
154200030409     C* TIPO -06- BANCALI -----------------------------------------------------*
154300030422     C                   CLEAR                   WBAN              5 0
154400030422     C                   EVAL      WBAN = D$DAR5NBA + D$DAR5NB2
154500030422     C                   Z-ADD     1             I
154600030422     C     '06'          LOOKUP    C2R(I)                                 98
154700030422     C     *IN98         IFEQ      '0'
154800030422     C                   MOVEL     MSG(35)       $MSG
154900030422     C                   EXSR      SCRERR
155000030422     C                   ELSE
155100030422     C                   Z-ADD     WBAN          Q2R(I)
155200030422     C                   ENDIF
155201150227     C* TIPO -13- BANCALI eur  ------------------------------------------------*
155202150227     C                   CLEAR                   WBAN              5 0
155203150227     c                   if        D$DAR5TBa = 1
155205150227     C                   add       D$DAR5NBA     wban
155207150227     c                   end
155208150227     c                   if        D$DAR5TB2 = 1
155209150227     C                   add       D$DAR5NB2     wban
155211150227     c                   end
155212150227     C                   Z-ADD     1             I
155213150227     C     '13'          LOOKUP    C2R(I)                                 98
155214150227     C     *IN98         IFEQ      '0'
155215150227     C                   MOVEL     MSG(36)       $MSG
155216150227     C                   EXSR      SCRERR
155217150227     C                   ELSE
155218150227     C                   Z-ADD     WBAN          Q2R(I)
155219150227     C                   ENDIF
155220150227     C* TIPO -14- BANCALI epal ------------------------------------------------*
155221150227     C                   CLEAR                   WBAN              5 0
155222150227     c                   if        D$DAR5TBa = 2
155224150227     C                   add       D$DAR5NBA     wban
155225150227     c                   end
155226150227     c                   if        D$DAR5TB2 = 2
155228150227     C                   add       D$DAR5NB2     wban
155229150227     c                   end
155230150227     C                   Z-ADD     1             I
155231150227     C     '14'          LOOKUP    C2R(I)                                 98
155232150227     C     *IN98         IFEQ      '0'
155233150227     C                   MOVEL     MSG(37)       $MSG
155234150227     C                   EXSR      SCRERR
155235150227     C                   ELSE
155236150227     C                   Z-ADD     WBAN          Q2R(I)
155237150227     C                   ENDIF
155238150227     C* TIPO -15- BANCALI leggeri ---------------------------------------------*
155239150227     C                   CLEAR                   WBAN              5 0
155240150227     c                   if        D$DAR5TBa = 3
155242150227     C                   add       D$DAR5NBa     wban
155243150227     c                   end
155244150227     c                   if        D$DAR5TB2 = 3
155246150227     C                   add       D$DAR5NB2     wban
155247150227     c                   end
155248150227     C                   Z-ADD     1             I
155249150227     C     '15'          LOOKUP    C2R(I)                                 98
155250150227     C     *IN98         IFEQ      '0'
155251150227     C                   MOVEL     MSG(38)       $MSG
155252150227     C                   EXSR      SCRERR
155253150227     C                   ELSE
155254150227     C                   Z-ADD     WBAN          Q2R(I)
155255150227     C                   ENDIF
155300940908     C*
155400951213     C* TIPO -09- STOP RITIRO -------------------------------------------------*
155500940908     C                   Z-ADD     1             I
155600940908     C     '09'          LOOKUP    C2R(I)                                 98
155700940908IF  1C     *IN98         IFEQ      '0'
155800940908     C                   MOVEL     MSG(9)        $MSG
155900940908     C                   EXSR      SCRERR
156000940908X   1C                   ELSE
156100941209     C                   Z-ADD     D$DNSR        Q2R(I)
156200940908E   1C                   ENDIF
156300951213     C*
156400951213     C* TIPO -10- STOP CONSEGNA -----------------------------------------------*
156500951213     C                   Z-ADD     1             I
156600951213     C     '10'          LOOKUP    C2R(I)                                 98
156700951213IF  1C     *IN98         IFEQ      '0'
156800951213     C                   MOVEL     MSG(16)       $MSG
156900951213     C                   EXSR      SCRERR
157000951213X   1C                   ELSE
157100951213     C                   Z-ADD     D$DNSC        Q2R(I)
157200951213E   1C                   ENDIF
157300991029     C*
157400991029     C* TIPO -11- PESO X GIORNI GIACENZA --------------------------------------*
157500991029     C                   Z-ADD     1             I
157600991029     C     '11'          LOOKUP    C2R(I)                                 98
157700991029IF  1C     *IN98         IFEQ      '0'
157800991029     C                   MOVEL     MSG(17)       $MSG
157900991029     C                   EXSR      SCRERR
158000991029X   1C                   ELSE
158100991029     C     D$DGGS        IFEQ      *ZEROS
158200991029     C                   Z-ADD     1             WT0155           15 5
158300991029     C                   ELSE
158400991029     C                   Z-ADD     D$DGGS        WT0155
158500991029     C                   ENDIF
158600991029     C     WPES          MULT      WT0155        Q2R(I)
158700991029E   1C                   ENDIF
158800000201     C*
158900000201     C* TIPO -12- GIORNI GIACENZA ---------------------------------------------*
159000000201     C                   Z-ADD     1             I
159100000201     C     '12'          LOOKUP    C2R(I)                                 98
159200000201IF  1C     *IN98         IFEQ      '0'
159300000201     C                   MOVEL     MSG(25)       $MSG
159400000201     C                   EXSR      SCRERR
159500000201X   1C                   ELSE
159600000201IF  2C     D$DGGS        IFEQ      *ZEROS
159700000201     C                   Z-ADD     1             Q2R(I)
159800000201X   2C                   ELSE
159900000201     C                   Z-ADD     D$DGGS        Q2R(I)
160000000201E   2C                   ENDIF
160100000201E   1C                   ENDIF
160200940908     C*
160300940908     C* TIPO -21- IMPONIBILE REALE --------------------------------------------*
160400940908     C                   Z-ADD     1             I
160500940908     C     '21'          LOOKUP    C2R(I)                                 98
160600940908     C     *IN98         IFEQ      '0'
160700940908     C                   MOVEL     MSG(11)       $MSG
160800940908     C                   EXSR      SCRERR
160900940908     C                   ELSE
161000940908     C* CERCO NELLA SCHIERA IL INPUT AL PGM L'IMPONIBILE REALE
161100940908     C                   Z-ADD     1             II                9 0
161200940908     C     556           LOOKUP    RCD(II)                                98
161300940908     C     *IN98         IFEQ      '1'
161400940908     C                   Z-ADD     RIM(II)       Q2R(I)
161500940908     C                   ENDIF
161600940908     C                   ENDIF
161700940908     C*
161800940908     C* TIPO -22- CONTRASEGNO -------------------------------------------------*
161900940908     C                   Z-ADD     1             I
162000940908     C     '22'          LOOKUP    C2R(I)                                 98
162100940908     C     *IN98         IFEQ      '0'
162200940908     C                   MOVEL     MSG(12)       $MSG
162300940908     C                   EXSR      SCRERR
162400940908     C                   ELSE
162500941201     C                   Z-ADD     D$DCAS        Q2R(I)
162600940908     C                   ENDIF
162700940908     C*
162800940908     C* TIPO -23- VALORE MERCE ASSICURATA -------------------------------------*
162900940908     C                   Z-ADD     1             I
163000940908     C     '23'          LOOKUP    C2R(I)                                 98
163100940908     C     *IN98         IFEQ      '0'
163200940908     C                   MOVEL     MSG(13)       $MSG
163300940908     C                   EXSR      SCRERR
163400940908     C                   ELSE
163500011227     C                   Z-ADD     D$DVAE        Q2R(I)
163600940908     C                   ENDIF
163700940908     C*
163800940908     C* TIPO -24- NUMERO DI GIORNI PAGAMENTO ----------------------------------*
163900940908     C                   Z-ADD     1             I
164000940908     C     '24'          LOOKUP    C2R(I)                                 98
164100940908     C     *IN98         IFEQ      '0'
164200940908     C                   MOVEL     MSG(14)       $MSG
164300940908     C                   EXSR      SCRERR
164400940908     C                   ELSE
164500940908     C                   Z-ADD     WGGP          Q2R(I)
164600940908     C                   ENDIF
164700940908     C*
164800940908     C                   ENDSR
164900940803     C*------------------------------------------------------------------------*
165000940803     C*? CHKVOC - CONTROLLA CHE TUTTE LE VOCI DA CALCOLARE SIANO VALIDE
165100940803     C*            ALLA DATA DELLA BOLLA ED EVENTUALMENTE CARICA LE GIUSTE
165200940803     C*            DECORRENZE
165300940803     C*            INPUT: - WDDSP (DATA SPEDIZIONE BOLLA)
165400940803     C*            OUTPUT:- SCHIERE VOCI COMPLETE ALLA DATA IN INPUT
165500940803     C*------------------------------------------------------------------------*
165600940603     C     CHKVOC        BEGSR
165700940603     C*
165800940607     C* CICLO PER TUTTE LE VOCI DA CALCOLARE
1659009406071    C                   DO        999           V
1660009406072    C     VC(V)         IFEQ      'S'
1661009407293    C     WDDSP         IFLT      VDDV(V)
166200940729     C     WDDSP         ORGT      VDSV(V)
166300940630     C                   Z-ADD     *ZEROS        VPRG(V)
166400940603     C                   MOVEL     *BLANKS       VTVO(V)
166500940704     C                   Z-ADD     *ZEROS        VDDV(V)
166600940704     C                   Z-ADD     *ZEROS        VDSV(V)
166700940603     C                   MOVEL     *BLANKS       VVRV(V)
166800940603     C                   MOVEL     *BLANKS       VTIV(V)
166900940603     C                   MOVEL     *BLANKS       VSPT(V)
167000940603     C                   Z-ADD     *ZEROS        VCMP(V)
167100940607     C                   MOVEL     *BLANKS       VRFI(V)
167200940607     C                   MOVEL     *BLANKS       VRFE(V)
167300940607     C                   MOVEL     *BLANKS       VRTI(V)
167400940607     C                   MOVEL     *BLANKS       VRTE(V)
167500940607     C                   MOVEL     *BLANKS       VRPI(V)
167600940607     C                   MOVEL     *BLANKS       VRPE(V)
167700940607     C                   MOVEL     *BLANKS       VRKI(V)
167800940607     C                   MOVEL     *BLANKS       VRKE(V)
167900940704     C                   Z-ADD     *ZEROS        VARF(V)
168000940704     C                   Z-ADD     *ZEROS        VARL(V)
168100940704     C                   Z-ADD     *ZEROS        VARO(V)
168200030113     C                   MOVEL     *BLANKS       VCOP(V)
168300030113     C                   MOVEL     *BLANKS       VDAT(V)
168400030113     C                   MOVEL     *BLANKS       VAID(V)
168500030422     C                   Z-ADD     *ZEROS        VRPV(V)
168600091009     C                   MOVEL     *BLANKS       Vric(V)
169100030422     C                   MOVEL     *BLANKS       VSPS(V)
169200940607     C                   MOVEL     'N'           VC(V)
169300940607     C* CERCO IL RECORD DELLA VOCE CON DEC/SCAD PER LA DATA BOLLA
169400940607     C                   Z-ADD     V             EVTVOC
169500940630     C                   Z-ADD     *ZEROS        EVTPRG
169600940603     C     KEYEVT        SETLL     ECEVT01L                           99
169700940603     C  N99              READ      ECEVT01L                               99
169800940607     C     *IN99         IFEQ      '1'
169900940829     C     EVTVOC        ORNE      V
170000991119     C                   MOVEL     MSG(18)       DSMSG
170100991119     C                   MOVE      V             DSVOCE
170200991119     C                   MOVEL     DSMSG         $MSG
170300940907     C                   EXSR      SCRERR
170400940607     C                   END
1705009407044    C     *IN99         DOWEQ     '0'
170600940607     C     EVTVOC        ANDEQ     V
170700940704     C     VC(V)         ANDEQ     'N'
1708009407045    C     EVTATB        IFEQ      *BLANKS
170900940729     C     WDDSP         ANDGE     EVTDDV
171000940729     C     WDDSP         ANDLE     EVTDSV
171100940603     C                   Z-ADD     EVTVOC        V
171200940630     C                   Z-ADD     EVTPRG        VPRG(V)
171300940607     C                   MOVEL     EVTTVO        VTVO(V)
171400940704     C                   Z-ADD     EVTDDV        VDDV(V)
171500940704     C                   Z-ADD     EVTDSV        VDSV(V)
171600090304     c* per pod immage forzo colli invece di spedizione, perchè non si
171700090304     C* vuole cambiare il tipo tariffa, la spedizione sarà una e i
171800090304     c* colli sono le immagini
171900090304     c                   if        v = 199
172000090304     C                   MOVEL     '01'          VVRV(V)
172100090304     c                   if        evttiv <> ' '
172200090304     C                   MOVEL     '01'          VTIV(V)
172300090304     c                   end
172400090304     c                   else
172500940607     C                   MOVEL     EVTVRV        VVRV(V)
172600940607     C                   MOVEL     EVTTIV        VTIV(V)
172700090304     c                   end
172800940607     C                   MOVEL     EVTSPT        VSPT(V)
172900940607     C                   Z-ADD     EVTCMP        VCMP(V)
173000940607     C                   MOVEL     EVTRFI        VRFI(V)
173100940607     C                   MOVEL     EVTRFE        VRFE(V)
173200940607     C                   MOVEL     EVTRTI        VRTI(V)
173300940607     C                   MOVEL     EVTRTE        VRTE(V)
173400940607     C                   MOVEL     EVTRPI        VRPI(V)
173500940607     C                   MOVEL     EVTRPE        VRPE(V)
173600940607     C                   MOVEL     EVTRKI        VRKI(V)
173700940607     C                   MOVEL     EVTRKE        VRKE(V)
173800940704     C                   Z-ADD     EVTARF        VARF(V)
173900940704     C                   Z-ADD     EVTARL        VARL(V)
174000940704     C                   Z-ADD     EVTARO        VARO(V)
174100030113     C                   MOVEL     EVTCOP        VCOP(V)
174200030113     C                   MOVEL     EVTDAT        VDAT(V)
174300030113     C                   MOVEL     EVTAID        VAID(V)
174400030422     C                   MOVEL     EVTXXX        DEVTXXX
174500030422     C                   Z-ADD     EVTRPV        VRPV(V)
174600030422     C                   MOVEL     EVTSPS        VSPS(V)
174700940607     C                   MOVEL     'S'           VC(V)
174800091009     c* recupero la filiale di competenza voce
174900091009     C     KEYECT        SETLL     ECECT01L
175000091009     c                   do        *hival
175100091009     C     KEYECT        READE     ECECT01L
175200091009     c                   if        %eof(ecect01l)
175300091009     c                   leave
175400091009     c                   end
175500091009IF  4C     ECTATB        IFne      *BLANKS
175600091009     C     WDDSP         orlt      EcTDDc
175700091009     C     WDDSP         orgt      EcTDSc
175800091009     c                   iter
175900091009     c                   end
176000091009     c                   eval      vric(v) = ectric
176100091009     c                   leave
176200091009     c                   enddo
1763009407045    C                   ENDIF
176400940603     C                   READ      ECEVT01L                               99
1765009407044    C                   ENDDO
1766009407043    C                   ENDIF
1767009407042    C                   ENDIF
1768009406071    C                   ENDDO
176900940603     C*
177000940603     C                   ENDSR
177100940803     C*------------------------------------------------------------------------*
177200940803     C*? VALVOC - VALORIZZA LE VOCI VALIDE
177300940803     C*------------------------------------------------------------------------*
177400940526     C     VALVOC        BEGSR
177500940526     C*
177600940603     C* CICLO PER TUTTE LE VOCI 'DA CALCOLARE'
177700940603     C                   DO        999           V
177800940607     C     VC(V)         IFEQ      'S'
177900940630     C                   Z-ADD     V             WVOC
178000940630     C                   Z-ADD     VPRG(V)       WPRG
178100000306     C                   MOVEL     VTIV(V)       WTIV
178200940607     C* CERCA TARIFFA
178300940607     C                   EXSR      CERTAR
178400940729     C*
178500940908     C* SE NON C'E' ERRORE (ES. MANCA TARIFFA)
178600940908     C* E IL MANCA TARIFFA ERA DA SEGNALARE
178700940913     C     $ERR          IFEQ      'N'
178800940908     C     WMSGMT        ANDEQ     'S'
178900940803     C*
179000141113     C* calcola le quantità
179100141113     C                   EXSR      calqta
179200141113     C* CONTROLLO L'ARROTONDAMENTO
179300141113     C                   EXSR      ARROTO
179400940720     C*
179500020109     C                   Z-ADD     *ZEROS        WVVOCE           21 9
179600141114     C                   Z-ADD     *ZEROS        newvoce          21 9
179700090914     C                   Z-ADD     *ZEROS        WVVO25           15 5
179800100506     c*
179900121120     c                   if        D$GFSF = 'S'
180000091009     c                   exsr      sresvoca
180100121120     c                   end
180200940802     C*
180300940802     C* SE TIPO VOCE V=VOCE NORMALE
180400940802     C* IN BASE AL TIPO TARIFFA CALCOLA IMPORTO A QTA, FISSO, PERCENTUALE
180500940802     C     VTVO(V)       IFEQ      'V'
180600940802     C                   EXSR      CALVOC
180700940802     C                   ENDIF
180800940720     C*
180900940802     C* SE TIPO VOCE M=MAGGIORAZIONE
181000940802     C* RECUPERO TUTTE LE VOCI DA MAGGIORARE E APPLICO LA PERCENTUALE
181100940802     C     VTVO(V)       IFEQ      'M'
181200940906     C                   EXSR      CERMAG
181300940802     C                   ENDIF
181400940802     C*
181500940802     C* CARICO TIPO TARIFFA -> IMPONIBILE PER COMPETENZE SEDE (SOMMA DI TUTTE
181600991027     C* LE VOCI PRECEDENTI TRANNE I TRANSITI)
181700991027     C     VS(V)         IFEQ      'S'
181800940802     C                   Z-ADD     1             I
181900940802     C     '25'          LOOKUP    C2R(I)                                 98
182000940802     C     *IN98         IFEQ      '0'
182100940802     C                   MOVEL     MSG(15)       $MSG
182200940907     C                   EXSR      SCRERR
182300940802     C                   ELSE
182400940824     C                   ADD       WVVO25        Q2R(I)
182500940802     C                   ENDIF
182600991027     C                   ENDIF
182700940707     C*
182800940721     C                   ENDIF
182900940729     C                   ENDIF
183000940721     C                   ENDDO
183100940603     C*
183200091007     C                   ENDSR
183300141113     C*------------------------------------------------------------------------*
183400141113     C*? calqta : calcola le quantità
183500141113     C*------------------------------------------------------------------------*
183600141113     C     calqta        BEGSR
183700141113     C*
183800141113     C* CALCOLA QTA
183900141113     C*
184000141113     C* SE TIPO VALORIZZAZIONE = Q=QUANTITA  OPPURE P=PERCENTUALE
184100141113     C* RECUPERO LA QTA O IL VALORE DI RIFERIMENTO
184200141113     C                   Z-ADD     *ZEROS        WQTA             21 9
184300141113     C     VVRV(v)       IFNE      *BLANKS
184400141113     C                   Z-ADD     1             x
184500141113     C     VVRV(V)       LOOKUP    C2R(x)                                 98
184600141113     C     *IN98         IFEQ      '0'
184700141113     C                   MOVEL     MSG(19)       $MSG
184800141113     C                   EXSR      SCRERR
184900141113     C                   ELSE
185000141113     C* SE E' UNA TARIFFA DOVE E' STATO CALCOLATO IL PESO TASSABILE
185100141113     C* (GIA' CALCOLATO NELLA RICERCA DELLO SCAGLIONE) PRENDO QTA
185200141113     C* UTILIZZATA PER LA RICERCA DELLO SCAGLIONE
185300141113     C* ALTRIMENTI PRENDO QTA DELLA SCHIERA
185400141113     C     VSPT(V)       IFNE      *BLANKS
185500141113     C     WQTAPT        ANDGT     *ZEROS
185600141113     C                   Z-ADD     WQTAPT        WQTA
185700141113     C                   ELSE
185800141113     c* per tipo voce = importo voce x isolamento
185900141113     c                   if        vvrv(v) <> '26'
186000141113     C                   Z-ADD     Q2R(x)        WQTA
186100141113     c                   else
186200141113     C                   Z-ADD     vimp(v)       WQTA
186300141113     c                   end
186400141113     C                   ENDIF
186500141113     C                   MOVEL     T2R(x)        WTTQ              1
186600141113     C                   ENDIF
186700141113     C                   ENDIF
186800141113     C                   ENDSR
186900091007     C*------------------------------------------------------------------------*
187000100518     C*? sresvoc10 saldi x tempi uffici
187100091007     C*------------------------------------------------------------------------*
187200100518     C     sresvoc10     BEGSR
187300091009     c* memorizzo in schiera qta scaglioni e filiale  nel caso dovessi
187400091009     c* sottrarre la qta
187500091009     c                   select
187600100518     C     vric(i)       WHENEQ    'LA'
187700111020     C                   Z-ADD     W$lna         v10fil
187800100518     C     vric(i)       WHENEQ    'LP'
187900100518     C                   Z-ADD     W$LNP         v10fil
188000100518     C     vric(i)       WHENEQ    'TA'
188100100518     C                   Z-ADD     D$DTFA        v10fil
188200100518     C     vric(i)       WHENEQ    'TP'
188300100518     C                   Z-ADD     W$TFP         v10fil
188400100518     C     vric(i)       WHENEQ    'T1'
188500100518     C                   Z-ADD     D$DAR5flp     v10fil
188600100518     C     vric(i)       WHENEQ    'T2'
188700100518     C                   Z-ADD     D$DAR5fl2     v10fil
188800100518     C     vric(i)       WHENEQ    'T3'
188900100518     C                   Z-ADD     D$DAR5fl3     v10fil
189000100518     C     vric(i)       WHENEQ    'T4'
189100100518     C                   Z-ADD     D$DAR5fl4     v10fil
189200100518     C     vric(i)       WHENEQ    'T5'
189300100518     C                   Z-ADD     D$DAR5fl5     v10fil
189400100506     c* se blanks forzo sede
189500100506     c                   other
189600100518     C                   Z-ADD     46            v10fil
189700091009     c                   endsl
189800100518     c* aggancio il file saldi esvoc10f, se trovo sommo altrimenti scrivo
189900100518     c                   eval      v10dat = wdsp
190000100518     c                   eval      v10voc = i
190100100518     c     keyes10       chain     esvoc11l
190200091012     c*
190300100518     c                   if        not %found(esvoc11l)
190400100518     c                   eval      v10imp = vimp(i)
190500100518     c                   write     esvoc010
190600091007     c                   else
190700100518     c                   add       vimp(i)       v10imp
190800100518     c                   update    esvoc010
190900091007     c                   end
191000091007     c*
191100940526     C                   ENDSR
191200091009     C*------------------------------------------------------------------------*
191300110309     C*? sresvocs- sottraggo la qta dai saldi perchè importo voce = 0 oppure
191400110309     C*? aggiungo l'importo se importo > 0
191500091009     C*------------------------------------------------------------------------*
191600091009     C     sresvocs      BEGSR
191700110309     c* aggancio il file saldi esvoc00F x sottrarre qta o per aggiungere
191800110309     c* importo
191900091012     c                   eval      vocfil = vfil(i)
192000091009     c                   eval      vocdat = wdsp
192100091012     c                   eval      vocvoc = i
192200091012     c                   eval      vocsgi = vsgi(i)
192300091012     c                   eval      vocsgf = vsgf(i)
192400091009     c     keyes         chain     esvoc01l
192500091009     c                   if        %found(esvoc01l)
192600110309     c                   if        vimp(i) = 0
192700091012     c                   sub       vqta(i)       vocqta
192800091012     c                   sub       1             vocnsp
192900091012     C                   sub       D$DNCL        vocncl
193000091016     c* non aggiorna il numero dei bancali BNB se  voce 34 e DISK B
193100091016     c* (equivalente a serie > 0) e particolarità consegne bancali
193200091016if  1c                   IF        i = 34 and
193300091016     c                             d$dnrs > 0 and
193400091016     c                              %subst(d$dgva: 1: 1) = 'O'
193500091016     c                   else
193600091016     c                   sub       banvoc        vocban
193700091016e   1c                   endif
193800091012     c                   sub       pkfvoc        vocpkf
193900091012     c                   sub       vlfvoc        vocvlf
194000110309     c                   else
194100110309     c* imposto l'importo
194200110309     c                   add       vimp(i)       vocimp
194300110309     c                   end
194400091009     c                   update    esvoc000
194500091009     c                   end
194600091009     c*
194700091009     C                   ENDSR
194800100518     C*------------------------------------------------------------------------*
194900100518     C*? sresvoca  - saldi x tempi uffici
195000100518     C*------------------------------------------------------------------------*
195100100518     C     sresvoca      BEGSR
195200100518     c* memorizzo in schiera importo voce per filiale
195300100518     c                   eval      vqta(v)= wqta
195400100518     c                   eval      vsgi(v) = evdsgi
195500100518     c                   eval      vsgf(v) = evdsgf
195600100518     c                   select
195700100518     C     vric(v)       WHENEQ    'LA'
195800111020     C                   Z-ADD     W$lna         vfil(v)
195900100518     C     vric(v)       WHENEQ    'LP'
196000100518     C                   Z-ADD     W$LNP         vfil(v)
196100100518     C     vric(v)       WHENEQ    'TA'
196200100518     C                   Z-ADD     D$DTFA        vfil(v)
196300100518     C     vric(v)       WHENEQ    'TP'
196400100518     C                   Z-ADD     W$TFP         vfil(v)
196500100518     C     vric(v)       WHENEQ    'T1'
196600100518     C                   Z-ADD     D$DAR5flp     vfil(v)
196700100518     C     vric(v)       WHENEQ    'T2'
196800100518     C                   Z-ADD     D$DAR5fl2     vfil(v)
196900100518     C     vric(v)       WHENEQ    'T3'
197000100518     C                   Z-ADD     D$DAR5fl3     vfil(v)
197100100518     C     vric(v)       WHENEQ    'T4'
197200100518     C                   Z-ADD     D$DAR5fl4     vfil(v)
197300100518     C     vric(v)       WHENEQ    'T5'
197400100518     C                   Z-ADD     D$DAR5fl5     vfil(v)
197500100518     c* se blanks forzo sede
197600100518     c                   other
197700100518     C                   Z-ADD     46            vfil(v)
197800100518     c                   endsl
197900100518     c* aggancio il file saldi esvoc00f, se trovo sommo altrimenti scrivo
198000100518     c                   eval      vocfil = vfil(v)
198100100518     c                   eval      vocdat = wdsp
198200100518     c                   eval      vocvoc = v
198300100518     c                   eval      vocsgi = evdsgi
198400100518     c                   eval      vocsgf = evdsgf
198500100518     c                   eval      vocvrv = vvrv(v)
198600100518     c                   clear                   vocqta
198700100518     c                   clear                   vocnsp
198800100518     c                   clear                   vocncl
198900100518     c                   clear                   vocpkf
199000100518     c                   clear                   vocvlf
199100100518     c                   clear                   vocban
199200110311     c                   clear                   vocimp
199300100518     c     keyes         chain     esvoc01l
199400100518     C     WTTQ          IFEQ      'F'
199500100518     c                   add       1             vocqta
199600100518     c                   else
199700100518     c                   add       wqta          vocqta
199800100518     c                   end
199900100518     c                   add       1             vocnsp
200000100518     C                   add       D$DNCL        vocncl
200100100518     c* non aggiorna il numero dei bancali BNB se  voce 34 e DISK B
200200100518     c* (equivalente a serie > 0) e particolarità consegne bancali
200300100518if  1c                   IF        v = 34 and
200400100518     c                             d$dnrs > 0 and
200500100518     c                              %subst(d$dgva: 1: 1) = 'O'
200600100518     c                   else
200700100518     c                   add       banvoc        vocban
200800100518e   1c                   endif
200900100518     c                   add       pkfvoc        vocpkf
201000100518     c                   add       vlfvoc        vocvlf
201100100518     c*
201200100518     c                   if        not %found(esvoc01l)
201300100518     c                   write     esvoc000
201400100518     c                   else
201500100518     c                   update    esvoc000
201600100518     c                   end
201700100518     c*
201800100518     C                   ENDSR
201900940803     C*------------------------------------------------------------------------*
202000091012     C*? srqta  - CALCOLA LE qta della spedizione
202100940803     C*------------------------------------------------------------------------*
202200091012     C     srqta         BEGSR
202300091012     c* peso
202400091012IF  2C     D$DPKC        IFGT      *ZEROS
202500091012     C     D$DNCL        ANDEQ     D$DNCP
202600091012     C                   z-add     D$DPKC        Pkfvoc
202700091012X   2C                   ELSE
202800091012IF  3C     D$DPKF        IFGT      D$DPKC
202900091012     C                   z-add     D$DPKF        Pkfvoc
203000091012X   3C                   ELSE
203100091012     C                   z-add     D$DPKC        Pkfvoc
203200091012E   3C                   ENDIF
203300091012E   2C                   ENDIF
203400091012     c* volume
203500091012IF  1C     D$DFVF        IFEQ      'T'
203600091012     C                   z-add     D$DVLF        vlfvoc
203700091012X   1C                   ELSE
203800091012IF  2C     D$DVLC        IFGT      *ZEROS
203900091012     C     D$DNCL        ANDEQ     D$DNCR
204000091012     C                   z-add     D$DVLC        vlfvoc
204100091012X   2C                   ELSE
204200091012IF  3C     D$DVLF        IFGT      D$DVLC
204300091012     C                   z-add     D$DVLF        vlfvoc
204400091012X   3C                   ELSE
204500091012     C                   z-add     D$DVLC        vlfvoc
204600091012E   3C                   ENDIF
204700091012E   2C                   ENDIF
204800091012E   1C                   ENDIF
204900091012     C                   eval      banvoc= D$DAR5NBA + D$DAR5NB2
205000091012     C                   ENDSR
205100091012     C*------------------------------------------------------------------------*
205200091012     C*? CALVOC - CALCOLA LE VOCI NORMALI
205300091012     C*------------------------------------------------------------------------*
205400091012     C     CALVOC        BEGSR
205500011227     C*
205600011227     C* CONVERTE GLI IMPORTI DELLE TARIFFE SE LA DIVISA DELLE ANAGRAFICHE C/E E'
205700011227     C* DIVERSA DALLA DIVISA DI OUTPUT DEL C/E
205800011227     C                   Z-ADD     EVDITR        W$ITR
205900011227     C                   Z-ADD     EVDIMI        W$IMI
206000011227     C                   Z-ADD     EVDIMA        W$IMA
206100011227IF  1C     WDIVAC        IFNE      WDIVCE
206200011227IF  2C     EVDITR        IFNE      *ZEROS
206300011227     C                   Z-ADD     EVDITR        §IMI
206400011227     C                   MOVEL     WDIVAC        §DVI
206500011227     C                   EXSR      CNVEUR
206600011227     C                   Z-ADD     §IMO          W$ITR
206700011227E   2C                   ENDIF
206800011227IF  2C     EVDIMI        IFNE      *ZEROS
206900011227     C                   Z-ADD     EVDIMI        §IMI
207000011227     C                   MOVEL     WDIVAC        §DVI
207100011227     C                   EXSR      CNVEUR
207200011227     C                   Z-ADD     §IMO          W$IMI
207300011227E   2C                   ENDIF
207400011227IF  2C     EVDIMA        IFNE      *ZEROS
207500011227     C                   Z-ADD     EVDIMA        §IMI
207600011227     C                   MOVEL     WDIVAC        §DVI
207700011227     C                   EXSR      CNVEUR
207800011227     C                   Z-ADD     §IMO          W$IMA
207900011227E   2C                   ENDIF
208000011227E   1C                   ENDIF
208100940802     C*
208200940802     C* CALCOLO IN BASE AL TIPO TARIFFA
208300940802     C*
208400940802     C     WTTQ          IFEQ      'Q'
208500940802     C     WQTA          ANDNE     *ZEROS
208600011227     C     W$ITR         ANDNE     *ZEROS
208700011227     C     WQTA          MULT      W$ITR         WVVOCE
208800940802     C                   ENDIF
208900940802     C*
209000940802     C     WTTQ          IFEQ      'F'
209100011227     C                   Z-ADD     W$ITR         WVVOCE
209200940802     C                   ENDIF
209300940802     C*
209400940802     C     WTTQ          IFEQ      'P'
209500940802     C     WQTA          ANDNE     *ZEROS
209600940802     C     EVDPER        ANDNE     *ZEROS
209700940802     C     WQTA          DIV       100           WVVOCE
209800940802     C     WVVOCE        MULT      EVDPER        WVVOCE
209900940802     C                   ENDIF
210000940802     C*
210100940802     C* CONTROLLO IMPORTO MINIMO
210200011227     C     W$IMI         IFNE      *ZEROS
210300011227     C     WVVOCE        ANDLT     W$IMI
210400011227     C                   Z-ADD     W$IMI         WVVOCE
210500940802     C                   ENDIF
210600011227     C     W$IMA         IFNE      *ZEROS
210700011227     C     WVVOCE        ANDGT     W$IMA
210800011227     C                   Z-ADD     W$IMA         WVVOCE
210900940802     C                   ENDIF
211000940802     C*
211100940802     C* CONTROLLO SEGNO E CARICO IMPORTO NELLA SCHIERA
211200090914     C                   Z-ADD(H)  WVVOCE        N133             13 5
211300940802     C     EVDSEG        IFEQ      '+'
211400020130     C                   Z-ADD     N133          VIMP(V)
211500020130     C                   ADD       N133          WVVO25
211600940802     C                   ELSE
211700020130     C                   Z-SUB     N133          VIMP(V)
211800020130     C                   SUB       N133          WVVO25
211900940802     C                   ENDIF
212000940802     C*
212100940802     C                   ENDSR
212200940907     C*------------------------------------------------------------------------*
212300940907     C*?ARROTO - ARROTONDA LA QUANTITA'
212400940907     C*          INPUT  : WQTA (DA ARROTONDARE)
212500940907     C*          OUTPUT : WQTA (ARROTONDATA)
212600940907     C*------------------------------------------------------------------------*
212700940907     C     ARROTO        BEGSR
212800940907     C*
212900940907     C* CONTROLLO SE ESISTE L'ARROTONDAMENTO
213000940907     C     WQTA          IFNE      *ZEROS
213100940907     C     VARL(V)       ANDNE     *ZEROS
213200940907     C     WQTA          IFLE      VARL(V)
213300010202     C                   Z-ADD     VARF(V)       WARR             17 5
213400940907     C                   ELSE
213500940907     C                   Z-ADD     VARO(V)       WARR
213600940907     C                   ENDIF
213700940907     C* CONTROLLO SE DATO GIA' ARRORONDATO
213800940907     C     WQTA          DIV       WARR          WINT             15 0
213900970730     C                   MVR                     WT0155           15 5    99
214000940907     C     *IN99         IFEQ      *OFF
214100940907     C     WINT          MULT      WARR          WQTA
214200940907     C                   ADD       WARR          WQTA
214300940907     C                   ENDIF
214400940907     C                   ENDIF
214500940907     C*
214600940907     C                   ENDSR
214700940907     C*------------------------------------------------------------------------*
214800940907     C*? CALPT  - CALCOLA IL PESO TASSABILE
214900940907     C*------------------------------------------------------------------------*
215000940907     C     CALPT         BEGSR
215100940907     C*
215200940907     C* CALCOLO PESO TASSABILE DAL VOLUME
215300941019     C     WVOL          MULT      EVDRPV        WPTAS
215400940907     C     WPTAS         MULT      100           WPTAS
215500940907     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
215600940907     C     VSPT(V)       IFEQ      '1'
215700940907     C                   Z-ADD     WPTAS         WQTA
215800940907     C                   ENDIF
215900940907     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
216000940907     C     VSPT(V)       IFEQ      '2'
216100991008     C     WPTAS         IFGT      WPES
216200940907     C                   Z-ADD     WPTAS         WQTA
216300940907     C                   ENDIF
216400940907     C                   ENDIF
216500940907     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
216600940907     C     VSPT(V)       IFEQ      '3'
216700991008     C     WPTAS         IFLT      WPES
216800940907     C                   Z-ADD     WPTAS         WQTA
216900940907     C                   ENDIF
217000940907     C                   ENDIF
217100940907     C*
217200940907     C                   ENDSR
217300030422     C*------------------------------------------------------------------------*
217400030422     C*? CALPS  - CALCOLA IL PESO TASSABILE PER RICERCA SCAGLIONE
217500030422     C*------------------------------------------------------------------------*
217600030422     C     CALPS         BEGSR
217700040209     C                   CLEAR                   WPTASR2
217800030422     C*
217900030422     C* CALCOLO PESO TASSABILE DAL VOLUME
218000030423     C     WVOL          MULT      VRPV(V)       WPTASR
218100030423     C     WPTASR        MULT      100           WPTASR
218200030422     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
218300030423     C     VSPS(V)       IFEQ      '1'
218400040209     C                   Z-ADD     WPTASR        WPTASR2
218500030422     C                   ENDIF
218600030422     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
218700030423     C     VSPS(V)       IFEQ      '2'
218800030423     C     WPTASR        IFGT      WPES
218900040209     C                   Z-ADD     WPTASR        WPTASR2
219000040209     c                   ELSE
219100040209     C                   Z-ADD     WPES          WPTASR2
219200030422     C                   ENDIF
219300030422     C                   ENDIF
219400030422     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
219500030423     C     VSPS(V)       IFEQ      '3'
219600030423     C     WPTASR        IFLT      WPES
219700040209     C                   Z-ADD     WPTASR        WPTASR2
219800040209     C                   ELSE
219900040209     C                   Z-ADD     WPES          WPTASR2
220000030422     C                   ENDIF
220100030422     C                   ENDIF
220200030422     C*
220300030422     C                   ENDSR
220400940803     C*------------------------------------------------------------------------*
220500940906     C*? CERMAG - CERCA LE MAGGIORAZIONI
220600940803     C*------------------------------------------------------------------------*
220700940906     C     CERMAG        BEGSR
220800940908     C*
220900940906     C* SE PERCENTUALE DIVERSA DA ZERO NON CERCO NEMMENO LA MAGGIORAZIONE
221000940906     C     EVDPER        IFNE      *ZEROS
221100940908     C                   MOVEL     'N'           WMAGOK            1
221200940906     C*
221300941024     C* CONTROLLA SE MAGGIORAZIONI IN MEMORIA
221400940906     C                   Z-ADD     1             M                 6 0
221500940906     C     M             DOWLE     WULTMA
221600940906     C     WVOC          IFEQ      MVOM(M)
221700940906     C     WPRG          ANDEQ     MPRM(M)
221800940908     C                   MOVEL     'S'           WMAGOK
221900940906     C                   Z-ADD     MVOM(M)       EVMVOM
222000940906     C                   Z-ADD     MPRM(M)       EVMPRM
222100940906     C                   Z-ADD     MVOC(M)       EVMVOC
222200940906     C                   Z-ADD     MPRG(M)       EVMPRG
222300100408     C                   Z-ADD     Mviso(M)      EVMviso
222400100408     C                   Z-ADD     Mddti(M)      EVMddt
222500100408     C                   Z-ADD     Mdsti(M)      EVMdst
222600100408     C                   movel     Mapp(M)       EVMapp
222700940906     C                   EXSR      CALMAG
222800940906     C                   ENDIF
222900940906     C                   ADD       1             M
223000940906     C                   ENDDO
223100940908     C*
223200940906     C* SE NON TROVATA IN MEMORIA E FINE SCHIERA LEGGO SUL FILE
223300940906     C     WMAGOK        IFEQ      'N'
223400041118     C     WULTMA        ANDGE     600
223500940906     C*
223600030331     C                   Z-ADD     WVOC          EVMVOM
223700940802     C                   Z-ADD     WPRG          EVMPRM
223800940802     C                   Z-ADD     *ZEROS        EVMVOC
223900940802     C                   Z-ADD     *ZEROS        EVMPRG
224000940802     C     KEYEVM        SETLL     ECEVM01L                           99
224100940802     C  N99              READ      ECEVM01L                               99
224200940802     C     *IN99         IFEQ      '1'
224300940802     C                   MOVEL     MSG(22)       $MSG
224400940907     C                   EXSR      SCRERR
224500940802     C                   END
224600940906     C     *IN99         DOWEQ     '0'
224700940802     C     EVMVOM        ANDEQ     WVOC
224800940802     C     EVMPRM        ANDEQ     WPRG
224900940906     C     EVMATB        IFEQ      *BLANKS
225000940906     C                   EXSR      CALMAG
225100940906     C                   ENDIF
225200940802     C                   READ      ECEVM01L                               99
225300940906     C                   ENDDO
225400940802     C*
225500940906     C                   ENDIF
225600940906     C                   ENDIF
225700940906     C*
225800940802     C                   ENDSR
225900940906     C*------------------------------------------------------------------------*
226000940906     C*? CALMAG - CALCOLA LA MAGGIORAZIONE
226100940906     C*------------------------------------------------------------------------*
226200940906     C     CALMAG        BEGSR
226300011227     C*
226400011227     C* CONVERTE GLI IMPORTI DELLE TARIFFE SE LA DIVISA DELLE ANAGRAFICHE C/E E'
226500011227     C* DIVERSA DALLA DIVISA DI OUTPUT DEL C/E
226600011227     C                   Z-ADD     EVDIMI        W$IMI
226700011227     C                   Z-ADD     EVDIMA        W$IMA
226800011227IF  1C     WDIVAC        IFNE      WDIVCE
226900011227IF  2C     EVDIMI        IFNE      *ZEROS
227000011227     C                   Z-ADD     EVDIMI        §IMI
227100011227     C                   MOVEL     WDIVAC        §DVI
227200011227     C                   EXSR      CNVEUR
227300011227     C                   Z-ADD     §IMO          W$IMI
227400011227E   2C                   ENDIF
227500011227IF  2C     EVDIMA        IFNE      *ZEROS
227600011227     C                   Z-ADD     EVDIMA        §IMI
227700011227     C                   MOVEL     WDIVAC        §DVI
227800011227     C                   EXSR      CNVEUR
227900011227     C                   Z-ADD     §IMO          W$IMA
228000011227E   2C                   ENDIF
228100011227E   1C                   ENDIF
228200940906     C*
228300940906     C                   Z-ADD     EVMVOC        I
228400000407     C* CALCOLA SOLO SE LA VOCE DA MAGGIORARE E' DA CALCOLARE
228500000407     C     VC(I)         IFEQ      'S'
228600940906     C* CALCOLA LA PERCENTUALE
228700940906     C     VIMP(I)       DIV       100           WVVOCE
228800940906     C     WVVOCE        MULT      EVDPER        WVVOCE
228900940906     C* CONTROLLO IMPORTO MINIMO
229000011227     C     W$IMI         IFNE      *ZEROS
229100011227     C     WVVOCE        ANDLT     W$IMI
229200011227     C                   Z-ADD     W$IMI         WVVOCE
229300940906     C                   ENDIF
229400940906     C* CONTROLLO IMPORTO MASSIMO
229500011227     C     W$IMA         IFNE      *ZEROS
229600011227     C     WVVOCE        ANDGT     W$IMA
229700011227     C                   Z-ADD     W$IMA         WVVOCE
229800940906     C                   ENDIF
229900141114     c* se è presente il tipo tariffa anche sulla maggiorazione, lo
230000141114     c* sostituisco all'importo appena calcolato
230100141127     c                   if        vvrv(v)<> ' ' and vimp(i) <> 0
230200141114     c                             and evdper <> 0
230300141114     c                   exsr      calqta
230400141127     c                   eval      wvvoce = VIMP(I) - (vimp(i) * wqta)
230500141128     c                   if        wvvoce > 0
230600141127     c                   eval      evdseg = '-'
230700141127     c                   else
230800141127     c                   eval      evdseg = '+'
230900141128     c                   eval      wvvoce = wvvoce * -1
231000141114     C                   ENDIF
231100141127     C                   ENDIF
231200940906     C* CONTROLLO SEGNO E AGGIUNGO O TOLGO IMPORTO NELLA SCHIERA
231300090914     C                   Z-ADD(H)  WVVOCE        N133
231400940906     C     EVDSEG        IFEQ      '+'
231500100408     c* ISOLO la maggiorazione e la applico solo se richiesto
231600100408     c                   if        evmapp = 'N' and
231700100408     C                             WDDSP >= evmddt and
231800100408     C                             WDDSP <= evmdst and
231900100422     c                             evmviso <> 0 and vc(evmviso)='S'
232000100408     c                   add       n133          vimp(evmviso)
232100100420     c                   if        vs(evmviso) = 'S'
232200100420     C                   ADD       N133          WVVO25
232300100420     c                   end
232400100408     c                   else
232500020130     C     VIMP(I)       ADD       N133          VIMP(I)
232600050503     c                   if        vs(i) = 'S'
232700020130     C                   ADD       N133          WVVO25
232800050503     c                   end
232900100420     c                   end
233000940906     C                   ELSE
233100100408     C* SEGNO NEGATIVO
233200100420     c* se il segno è negativo isolo la voce con segno positivo e la applico
233300100422     c* sempre
233400020130     C     VIMP(I)       SUB       N133          VIMP(I)
233500050503     c                   if        vs(i) = 'S'
233600020130     C                   SUB       N133          WVVO25
233700050503     c                   end
233800100420     c                   if        evmapp = 'S' and
233900100420     C                             WDDSP >= evmddt and
234000100420     C                             WDDSP <= evmdst and
234100100422     c                             evmviso <> 0 and vc(evmviso)='S'
234200100420     c                   add       n133          vimp(evmviso)
234300100420     c                   if        vs(evmviso) = 'S'
234400100420     C                   ADD       N133          WVVO25
234500100420     c                   end
234600100420     c                   end
234700940906     C                   ENDIF
234800000407     C                   ENDIF
234900940906     C*
235000940906     C                   ENDSR
235100940908     C*------------------------------------------------------------------------*
235200030429     C*?CERTAR - CERCA LA TARIFFA
235300940908     C*------------------------------------------------------------------------*
235400940908     C     CERTAR        BEGSR
235500940908     C*
235600940908     C* IMPOSTO CAMPI LAVORO
235700940908     C                   MOVEL     'N'           WFLTT             1
235800940908     C                   MOVEL     *BLANKS       WRIF
235900940908     C                   Z-ADD     WDDSP         WDSP
236000940908     C                   MOVEL     *BLANKS       WTT1              3
236100940908     C                   MOVEL     *BLANKS       WTT2              3
236200950519     C                   MOVEL     *BLANKS       WCTSM             2
236300940908     C                   MOVEL     'S'           WMSGMT            1
236400941107     C                   Z-ADD     *ZEROS        WPTAS            15 3
236500030423     C                   Z-ADD     *ZEROS        WPTASR           15 3
236600040209     C                   Z-ADD     *ZEROS        WPTASR2          15 3
236700030423     C*
236800030423     C* calcola il peso tassabile da ricercare nello scaglione
236900030423     C     VSPS(V)       IFGT      *ZEROS
237000030423     C                   EXSR      CALPS
237100040209     C                   Z-ADD     WPTASR2       WQTA
237200030423     C                   ELSE
237300940908     C*
237400940908     C* CERCA LA QTA DA RICERCARE NELLO SCAGLIONE
237500940908     C                   Z-ADD     *ZEROS        WQTA
237600940908     C                   Z-ADD     1             I
237700940908     C     VTIV(V)       LOOKUP    C2R(I)                                 98
237800940908     C     *IN98         IFEQ      '0'
237900940908     C                   MOVEL     MSG(20)       $MSG
238000940908     C                   EXSR      SCRERR
238100940908     C                   ELSE
238200940908     C                   Z-ADD     Q2R(I)        WQTA
238300940908     C                   END
238400030423     C                   ENDIF
238500940908     C*
238600940908     C* IMPOSTO IL FLAG ITALIA/ESTERO
238700971211     C                   Z-ADD     W$LNP         F
238800111020     C                   Z-ADD     W$lna         F1                4 0
238900020805     C     FIE(F)        IFEQ      'E'
239000020805     C     FIE(F1)       OREQ      'E'
239100940908     C                   MOVEL     'E'           WIE               1
239200940908     C                   ELSE
239300940908     C                   MOVEL     'I'           WIE
239400940908     C                   ENDIF
239500940908     C*
239600940908     C* SE RICHIESTO, TENTATIVO PER CLIENTE
239700940908     C     WFLTT         IFEQ      'N'
239800940908     C     WIE           ANDEQ     'I'
239900940908     C     VRKI(V)       ANDNE     *BLANKS
240000940908     C                   MOVEL     VRKI(V)       WRIF
240100940908     C                   EXSR      CERCLI
240200940908     C                   ENDIF
240300940908     C     WFLTT         IFEQ      'N'
240400940908     C     WIE           ANDEQ     'E'
240500940908     C     VRKE(V)       ANDNE     *BLANKS
240600940908     C                   MOVEL     VRKE(V)       WRIF
240700940908     C                   EXSR      CERCLI
240800940908     C                   ENDIF
240900940908     C*
241000940908     C* SE RICHIESTO, TENTATIVO PER CAP
241100940908     C     WFLTT         IFEQ      'N'
241200940908     C     WIE           ANDEQ     'I'
241300940908     C     VRPI(V)       ANDNE     *BLANKS
241400940908     C                   MOVEL     VRPI(V)       WRIF
241500940908     C                   EXSR      CERCAP
241600940908     C                   ENDIF
241700940908     C     WFLTT         IFEQ      'N'
241800940908     C     WIE           ANDEQ     'E'
241900940908     C     VRPE(V)       ANDNE     *BLANKS
242000940908     C                   MOVEL     VRPE(V)       WRIF
242100940908     C                   EXSR      CERCAP
242200940908     C                   ENDIF
242300940908     C*
242400940908     C* SE RICHIESTO, TENTATIVO PER CODICE TASSAZIONE
242500940908     C     WFLTT         IFEQ      'N'
242600940908     C     WIE           ANDEQ     'I'
242700940908     C     VRTI(V)       ANDNE     *BLANKS
242800940908     C                   MOVEL     VRTI(V)       WRIF
242900940908     C                   EXSR      CERTAS
243000940908     C                   ENDIF
243100940908     C     WFLTT         IFEQ      'N'
243200940908     C     WIE           ANDEQ     'E'
243300940908     C     VRTE(V)       ANDNE     *BLANKS
243400940908     C                   MOVEL     VRTE(V)       WRIF
243500940908     C                   EXSR      CERTAS
243600940908     C                   ENDIF
243700940908     C*
243800940908     C* SE RICHIESTO, TENTATIVO PER CODICE FILIALE
243900940908     C     WFLTT         IFEQ      'N'
244000940908     C     WIE           ANDEQ     'I'
244100940908     C     VRFI(V)       ANDNE     *BLANKS
244200940908     C                   MOVEL     VRFI(V)       WRIF
244300940908     C                   EXSR      CERFIL
244400940908     C                   ENDIF
244500940908     C     WFLTT         IFEQ      'N'
244600940908     C     WIE           ANDEQ     'E'
244700940908     C     VRFE(V)       ANDNE     *BLANKS
244800940908     C                   MOVEL     VRFE(V)       WRIF
244900940908     C                   EXSR      CERFIL
245000940908     C                   ENDIF
245100940908     C*
245200940908     C* SE TARIFFA NON ANCORA TROVATA CERCA TARIFFA DI CARTELLO
245300940908     C     WFLTT         IFEQ      'N'
245400940908     C                   EXSR      CERCAR
245500940908     C                   ENDIF
245600040903     c* se ho manca tariffa per la voce 167 xchè peso tassabile > 2,5 kg
245700040903     c* disattivo tale voce e lascio la 168 e 169 e viceversa
245800040903     c                   if        wvoc = 167
245900040903     c                   if        wfltt = 'N' and WQTA > §DFEPKGD
246000040903     c                   movel     'N'           wmsgmt
246100040903     c                   movel     'N'           vc(167)
246200040903     c                   else
246300040903     c                   movel     'N'           vc(168)
246400040903     c                   movel     'N'           vc(169)
246500040903     c                   end
246600040903     c                   end
246700940908     C*
246800940908     C* CONTROLLO MANCA TARIFFA
246900940908     C     WFLTT         IFEQ      'N'
247000940908     C     WMSGMT        IFEQ      'S'
247100940908     C                   MOVEL     MSG(21)       DSMSG
247200940908     C                   MOVE      WVOC          DSVOCE
247300940908     C                   MOVE      WTT1          DSTT1
247400940908     C                   MOVE      WTT2          DSTT2
247500950519     C                   MOVE      WCTSM         DSCTS
247600940908     C                   MOVEL     DSMSG         $MSG
247700940908     C                   EXSR      SCRERR
247800940908     C                   ENDIF
247900940908     C                   ELSE
248000940908     C                   MOVEL     'S'           WFLTT
248100940908     C                   END
248200940908     C*
248300940908     C                   ENDSR
248400940803     C*------------------------------------------------------------------------*
248500940803     C*? CERCLI - CERCA LA TARIFFA PER CLIENTE
248600940803     C*------------------------------------------------------------------------*
248700940608     C     CERCLI        BEGSR
248800940608     C*
248900940630     C                   MOVEL     'K'           WTTV
249000940630     C                   Z-ADD     *ZEROS        WFT1
249100940630     C                   Z-ADD     *ZEROS        WFT2
249200940630     C                   Z-ADD     *ZEROS        WKSC
249300940701     C                   Z-ADD     *ZEROS        WCTR
249400940701     C                   MOVEL     *BLANKS       WCTS
249500131002     C                   MOVEL     *BLANKS       Wnaz
249600131002     C                   MOVEL     *BLANKS       wcad
249700940826     C     WRIF          IFEQ      'KK'
249800940829     C                   Z-ADD     W$KSC         WKSC
249900940701     C                   Z-ADD     D$DCTR        WCTR
250000940826     C                   ENDIF
250100940826     C     WRIF          IFEQ      'KM'
250200941201     C     D$DTPO        IFEQ      'F'
250300940829     C     W$CCM         IFNE      *ZEROS
250400940829     C                   Z-ADD     W$CCM         WKSC
250500940826     C                   ELSE
250600940829     C                   Z-ADD     W$KSC         WKSC
250700940826     C                   ENDIF
250800940826     C                   ENDIF
250900941201     C     D$DTPO        IFEQ      'A'
251000940829     C                   Z-ADD     W$CCM         WKSC
251100940826     C                   ENDIF
251200940701     C                   Z-ADD     D$DCTR        WCTR
251300940826     C                   ENDIF
251400940701     C*
251500940701     C* LANCIA GESTIONE TARIFFE
251600940902     C     WKSC          IFNE      DEPKSC
251700940902     C                   Z-ADD     WKSC          DEPKSC            7 0
251800940830     C                   EXSR      CHKKSC
251900940902     C                   ENDIF
252000940908     C     WKSCOK        IFEQ      'S'
252100941107     C                   EXSR      GESEVD
252200940908     C                   ENDIF
252300940608     C*
252400940719     C* SE TARIFFA BOLLA NON E' STATA TROVATA PROVO CON TARIFFA = 999 (TUTTE)
252500940719     C     WFLTT         IFEQ      'N'
252600940829     C                   Z-ADD     W$KSC         WKSC
252700940719     C                   Z-ADD     999           WCTR
252800940908     C     WKSCOK        IFEQ      'S'
252900940722     C                   EXSR      GESEVD
253000940908     C                   ENDIF
253100940830     C                   ENDIF
253200940719     C*
253300940608     C                   ENDSR
253400940803     C*------------------------------------------------------------------------*
253500940803     C*? CERCAP - CERCA LA TARIFFA PER CAP
253600940803     C*------------------------------------------------------------------------*
253700940701     C     CERCAP        BEGSR
253800940701     C*
253900940701     C                   MOVEL     'P'           WTTV
254000940701     C                   Z-ADD     *ZEROS        WFT1
254100940701     C                   Z-ADD     *ZEROS        WFT2
254200940701     C                   Z-ADD     *ZEROS        WKSC
254300940701     C                   Z-ADD     *ZEROS        WCTR
254400940701     C                   MOVEL     *BLANKS       WCTS
254500131002     C                   MOVEL     *BLANKS       wcad
254600131002     C                   MOVEL     *BLANKS       Wnaz
254700131002     c* cap completo
254800940701     C     WRIF          IFEQ      'PA'
254900131002     C                   MOVEl     D$Dnzd        Wnaz
255000131002     C                   MOVEl     D$DCAD        wcad
255100940701     C* LANCIA GESTIONE TARIFFE
255200940722     C                   EXSR      GESEVD
255300131002     C                   ENDIF
255400131002     c* cap fittizio
255500131002     C                   if        wfltt = 'N' and
255600131002     c                             (wrif = 'PA' or wrif = 'PP')
255700131002     C                   MOVEl     D$Dnzd        Wnaz
255800131002     c     wnaz          lookup    d15(i)                                 88
255900131002     c                   if        *in88
256000131002     C                   eval      wcad = %subst(D$DCAD: pcf(i): ecf(i))
256100131002     C                   ENDIF
256200131002     C                   EXSR      GESEVD
256300131002     C                   ENDIF
256400940701     C*
256500940701     C                   ENDSR
256600940803     C*------------------------------------------------------------------------*
256700940803     C*? CERTAS - CERCA LA TARIFFA PER CODICE TASSAZIONE
256800940803     C*------------------------------------------------------------------------*
256900940701     C     CERTAS        BEGSR
257000940701     C*
257100940701     C                   MOVEL     'T'           WTTV
257200940701     C                   Z-ADD     *ZEROS        WFT1
257300940701     C                   Z-ADD     *ZEROS        WFT2
257400940701     C                   Z-ADD     *ZEROS        WKSC
257500940701     C                   Z-ADD     *ZEROS        WCTR
257600940701     C                   MOVEL     *BLANKS       WCTS
257700131002     C                   MOVEL     *BLANKS       wcad
257800131002     C                   MOVEL     *BLANKS       Wnaz
257900940701     C     WRIF          IFEQ      'SP'
258000940829     C                   MOVEL     W$MCT         WCTS
258100940701     C                   ENDIF
258200940701     C     WRIF          IFEQ      'SA'
258300940721     C                   MOVEL     D$DCTS        WCTS
258400940701     C                   ENDIF
258500940701     C*
258600940701     C* LANCIA GESTIONE TARIFFE
258700940722     C                   EXSR      GESEVD
258800950519     C*
258900950519     C* CAMPI PER MESSAGGI
259000950519     C                   MOVE      WCTS          WCTSM
259100940701     C*
259200940701     C                   ENDSR
259300940803     C*------------------------------------------------------------------------*
259400940803     C*? CERFIL - CERCA LA TARIFFA PER FILIALE/TERMINAL
259500940803     C*------------------------------------------------------------------------*
259600940701     C     CERFIL        BEGSR
259700940701     C*
259800940701     C                   MOVEL     'F'           WTTV
259900940701     C                   Z-ADD     *ZEROS        WFT1
260000940701     C                   Z-ADD     *ZEROS        WFT2
260100940701     C                   Z-ADD     *ZEROS        WKSC
260200940701     C                   Z-ADD     *ZEROS        WCTR
260300940701     C                   MOVEL     *BLANKS       WCTS
260400131002     C                   MOVEL     *BLANKS       wcad
260500131002     C                   MOVEL     *BLANKS       Wnaz
260600940701     C     WRIF          IFEQ      'LP'
260700940829     C                   Z-ADD     W$LNP         WFT1
260800940701     C                   ENDIF
260900940701     C     WRIF          IFEQ      'LT'
261000940829     C                   Z-ADD     W$LNP         WFT1
261100940829     C                   Z-ADD     W$TFP         WFT2
261200940701     C                   ENDIF
261300940701     C     WRIF          IFEQ      'TP'
261400940829     C                   Z-ADD     W$TFP         WFT1
261500940701     C                   ENDIF
261600940701     C     WRIF          IFEQ      'TT'
261700940829     C                   Z-ADD     W$TFP         WFT1
261800940826     C                   Z-ADD     D$DTFA        WFT2
261900130930     c* per bolle legate: conti servizi (F7 e FC) impostiamo il terminal
262000130122     c* della bolla figlia e non dell'originale
262100130122     C     D$DFIG        IFEQ      'S'
262200130122     c     D$DRBL        andeq     'C'
262300130122     c     D$DDBO        andeq     'S'
262400130930     C     WDDSP         andgt     20181231
262500130122     c                   z-add     d$DTFP        WFT1
262600130122     c                   ENDIF
262700940701     C                   ENDIF
262800940701     C     WRIF          IFEQ      'T1'
262900040524     C*                  Z-ADD     D$DFLP        WFT1
263000040524     C                   Z-ADD     D$Dar5flp     WFT1
263100940701     C                   ENDIF
263200940701     C     WRIF          IFEQ      'T2'
263300040524     C*                  Z-ADD     D$DFL2        WFT1
263400040524     C                   Z-ADD     D$Dar5fl2     WFT1
263500940701     C                   ENDIF
263600040524     C     WRIF          IFEQ      'T3'
263700040524     C                   Z-ADD     D$Dar5fl3     WFT1
263800040524     C                   ENDIF
263900040524     C     WRIF          IFEQ      'T4'
264000040524     C                   Z-ADD     D$Dar5fl4     WFT1
264100040524     C                   ENDIF
264200040524     C     WRIF          IFEQ      'T5'
264300040524     C                   Z-ADD     D$Dar5fl5     WFT1
264400040524     C                   ENDIF
264500940701     C     WRIF          IFEQ      'TA'
264600940701     C                   Z-ADD     D$DTFA        WFT1
264700940701     C                   ENDIF
264800940701     C     WRIF          IFEQ      'TL'
264900940701     C                   Z-ADD     D$DTFA        WFT1
265000111020     C                   Z-ADD     W$lna         WFT2
265100940701     C                   ENDIF
265200940701     C     WRIF          IFEQ      'LA'
265300111020     C                   Z-ADD     W$lna         WFT1
265400940701     C                   ENDIF
265500940902     C*
265600940902     C* LANCIA GESTIONE TARIFFE
265700940902     C                   MOVEL     D$DKSC        WFILX             3
265800940902     C     WFILX         IFNE      DEPFIL
265900940902     C                   MOVE      WFILX         DEPFIL            3
266000940902     C                   MOVE      WFILX         WFIL              3 0
266100940902     C                   EXSR      CARFIL
266200940902     C                   ENDIF
266300940722     C                   EXSR      GESEVD
266400940701     C*
266500940908     C* SE BOLLA FIGLIA E TARIFFA TERMINAL/TERMINAL E TARIFFA NON TROVATA
266600940908     C* ----> NON EMETTO MESSAGGIO 'TARIFFA NON TROVATA'
266700950905     C     D$DFIG        IFEQ      'S'
266800940908     C     WRIF          ANDEQ     'TT'
266900940908     C     WFLTT         ANDEQ     'N'
267000940908     C                   MOVEL     'N'           WMSGMT
267100940908     C                   ENDIF
267200940908     C*
267300940908     C* CAMPI PER MESSAGGI
267400940830     C                   MOVE      WFT1          WTT1
267500940830     C                   MOVE      WFT2          WTT2
267600940830     C*
267700940701     C                   ENDSR
267800940803     C*------------------------------------------------------------------------*
267900940803     C*? CERCAR - CERCA LA TARIFFA DI CARTELLO
268000940803     C*------------------------------------------------------------------------*
268100940701     C     CERCAR        BEGSR
268200940701     C*
268300940701     C                   MOVEL     'C'           WTTV
268400940701     C                   Z-ADD     *ZEROS        WFT1
268500940701     C                   Z-ADD     *ZEROS        WFT2
268600940701     C                   Z-ADD     *ZEROS        WKSC
268700940701     C                   Z-ADD     *ZEROS        WCTR
268800940701     C                   MOVEL     *BLANKS       WCTS
268900131002     C                   MOVEL     *BLANKS       wcad
269000131002     C                   MOVEL     *BLANKS       Wnaz
269100940701     C*
269200940701     C* LANCIA GESTIONE TARIFFE
269300940722     C                   EXSR      GESEVD
269400940701     C*
269500940701     C                   ENDSR
269600940704     C*------------------------------------------------------------------------*
269700940704     C*? GESEVD - GESTIONE FILE ECEVD (TARIFFE)
269800940704     C*------------------------------------------------------------------------*
269900940704     C     GESEVD        BEGSR
270000940801     C*
270100940801     C* SE TIPO SVILUPPO PESO TASSABILE 2=MIGLIORE REALE/TASS. O 3=MINORE REA/TA
270200940801     C* CERCO IL RAPP PESO/VOL PRIMA DI ARROTONDARE
270300961202     C     VSPT(V)       IFNE      *BLANKS
270400961202     C*
270500961202     C                   Z-ADD     *ZEROS        WQTAPT
270600940704     C*
270700940704     C* CERCA LO SCAGLIONE CON LA QTA DELLA BOLLA
270800940704     C                   EXSR      SETEVD
270900940704     C*
271000940707     C* SE IL RAPP. PESO/VOL MAGGIORE DI ZERO E VOLUME MAGGIORE DI ZERO
271100940707     C* CERCO SCAGLIONE CON PESO TASSABILE
271200940801     C     EVDRPV        IFNE      *ZEROS
271300941201     C     D$DVLF        ANDNE     *ZEROS
271400940707     C* CALCOLO PESO TASSABILE
271500940801     C                   EXSR      CALPT
271600020102     C                   Z-ADD     WQTA          WQTAPT           19 7
271700940801     C                   ENDIF
271800940801     C*
271900940801     C                   ENDIF
272000940707     C*
272100940803     C* CONTROLLA ARROTONDAMENTO
272200940803     C                   EXSR      ARROTO
272300940729     C*
272400940801     C* CERCA LO SCAGLIONE GIUSTO IN TARIFFA
272500940707     C                   EXSR      SETEVD
272600940704     C*
272700940704     C                   ENDSR
272800940701     C*------------------------------------------------------------------------*
272900940701     C*? SETEVD - POSIZIONAMENTO E PRIMA LETTURA FILE TARIFFE
273000940701     C*------------------------------------------------------------------------*
273100940701     C     SETEVD        BEGSR
273200940830     C*
273300940908     C                   MOVEL     'N'           WSETLL            1
273400940701     C                   MOVEL     '0'           FINEVD            1
273500940829     C*
273600940829     C* SE TARIFFA DI CARTELLO CONTROLLO PRIMA SE CARICATA IN MEMORIA
273700940829IF  1C     WTTV          IFEQ      'C'
273800940829     C                   EXSR      CHKCAR
273900940830E   1C                   ENDIF
274000940902     C*
274100940902     C* SE TARIFFA DI FILIALE CONTROLLO PRIMA SE CARICATA IN MEMORIA
274200940902IF  1C     WTTV          IFEQ      'F'
274300940902     C                   EXSR      CHKFIL
274400940902E   1C                   ENDIF
274500940831     C*
274600940831     C* SE TARIFFA CLIENTE CONTROLLO PRIMA SE CARICATA IN MEMORIA
274700940831IF  1C     WTTV          IFEQ      'K'
274800940831     C                   EXSR      CHKCLI
274900940831E   1C                   ENDIF
275000940829     C*
275100940907     C* SE TARIFFA NON TROVATA IN MEMORIA LEGGO DAL FILE
275200940908IF  1C     WSETLL        IFEQ      'N'
275300940829     C*
275400940701     C* POSIZIONAMENTO E PRIMA LETTURA
275500940701     C                   Z-ADD     WVOC          EVDVOC
275600940701     C                   Z-ADD     WPRG          EVDPRG
275700940701     C                   MOVEL     WTTV          EVDTTV
275800940701     C                   Z-ADD     WFT1          EVDFT1
275900940701     C                   Z-ADD     WFT2          EVDFT2
276000940701     C                   Z-ADD     WKSC          EVDCLI
276100940701     C                   Z-ADD     WCTR          EVDCTR
276200940701     C                   MOVEL     WCTS          EVDCTS
276300131002     C                   movel     Wnaz          EVDnaz
276400131002     C                   movel(p)  wcad          EVDcad
276500940802     C                   Z-ADD     *ZEROS        EVDDDT
276600940802     C                   Z-ADD     *ZEROS        EVDSGI
276700131002     C     KEYEVD        SETLL     ecevd06l                           99
276800940701     C*
276900940830IF  2C     *IN99         IFEQ      *ON
277000940701     C                   MOVEL     '1'           FINEVD
277100940830X   2C                   ELSE
277200940701     C                   EXSR      LETEVD
277300940830E   2C                   ENDIF
277400940701     C*
277500940720     C* CONTROLLO MANCA TARIFFA
277600940830IF  2C     WRECOK        IFEQ      'N'
277700991112     C                   MOVEL     'N'           WFLTT
277800940830X   2C                   ELSE
277900940701     C                   MOVEL     'S'           WFLTT
278000940830E   2C                   ENDIF
278100940829     C*
278200940830E   1C                   ENDIF
278300940701     C*
278400940701     C                   ENDSR
278500940701     C*------------------------------------------------------------------------*
278600940701     C*? LETEVD - LETTURA PROSSIMO RECORD TARIFFE
278700940830     C*------------------------------------------------------------------------*
278800940701     C     LETEVD        BEGSR
278900940701     C*
279000940802     C* LEGGO FINO A:
279100940701     C*     - FINE FILE O SUPERATO LIMITE FINALE
279200940701     C*     - TROVATO RECORD VALIDO
279300940701     C                   MOVEL     'N'           WRECOK
279400940701     C     FINEVD        DOWEQ     '0'
279500940701     C     WRECOK        ANDEQ     'N'
279600940701     C*
279700940701     C* LETTURE SUCCESSIVE ALLA PRIMA
279800131002     C                   READ      ecevd06l                               99
279900940701     C*
280000940701     C     *IN99         IFEQ      '1'
280100940701     C     EVDVOC        ORNE      WVOC
280200940701     C     EVDPRG        ORNE      WPRG
280300940701     C     EVDTTV        ORNE      WTTV
280400940701     C     EVDFT1        ORNE      WFT1
280500940701     C     EVDFT2        ORNE      WFT2
280600940721     C     EVDCLI        ORNE      WKSC
280700940701     C     EVDCTR        ORNE      WCTR
280800940701     C     EVDCTS        ORNE      WCTS
280900131002     C     EVDCAd        ORNE      wcad
281000131002     C     EVDnaz        ORNE      Wnaz
281100940720     C                   MOVEL     '1'           FINEVD
281200940701     C                   ELSE
281300940701     C                   EXSR      CHKEVD
281400940701     C                   END
281500940701     C*
281600940701     C                   END
281700940701     C*
281800940701     C                   ENDSR
281900940830     C*------------------------------------------------------------------------*
282000940701     C*? CHKEVD - CONTROLLA VALIDITA' DELLA TARIFFA
282100940830     C*------------------------------------------------------------------------*
282200940701     C     CHKEVD        BEGSR
282300940701     C*
282400940701     C                   MOVEL     'S'           WRECOK            1
282500940701     C* ESCLUDO SE ANNULLATO
282600940701     C     EVDATB        IFNE      *BLANKS
282700940701     C                   MOVEL     'N'           WRECOK
282800940701     C                   END
282900940701     C* ESCLUDO SE DATA SPEDIZIONE FUORI DALLA DECORR/SCAD.
283000940701     C     WDSP          IFLT      EVDDDT
283100940701     C     WDSP          ORGT      EVDDST
283200940701     C                   MOVEL     'N'           WRECOK
283300940701     C                   END
283400011227     C*
283500011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
283600011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
283700011227     C                   Z-ADD     EVDSGI        W$SGI
283800011227     C                   Z-ADD     EVDSGF        W$SGF
283900011227IF  1C     WRECOK        IFEQ      'S'
284000011227IF  2C                   IF        WTIV = '21' OR
284100011227     C                             WTIV = '22' OR
284200011227     C                             WTIV = '23' OR
284300011227     C                             WTIV = '25'
284400011227IF  3C     WDIVAC        IFNE      WDIVCE
284500011227IF  4C     EVDSGI        IFNE      *ZEROS
284600011227     C                   Z-ADD     EVDSGI        §IMI
284700011227     C                   MOVEL     WDIVAC        §DVI
284800011227     C                   EXSR      CNVEUR
284900011227     C                   Z-ADD     §IMO          W$SGI
285000011227E   4C                   ENDIF
285100011227IF  4C     EVDSGF        IFNE      *ZEROS
285200011227     C                   Z-ADD     EVDSGF        §IMI
285300011227     C                   MOVEL     WDIVAC        §DVI
285400011227     C                   EXSR      CNVEUR
285500011227     C                   Z-ADD     §IMO          W$SGF
285600011227E   4C                   ENDIF
285700011227E   3C                   ENDIF
285800011227E   2C                   ENDIF
285900011227E   1C                   ENDIF
286000011227     C*
286100940701     C* ESCLUDO SE QTA NON COMPRESA NELLO SCAGLIONE
286200000306IF  1C     WTIV          IFNE      *BLANKS
286300011227     C     WQTA          IFLT      W$SGI
286400011227     C     WQTA          ORGT      W$SGF
286500940701     C                   MOVEL     'N'           WRECOK
286600940701     C                   END
286700000306E   1C                   ENDIF
286800940701     C*
286900940701     C                   ENDSR
287000940830     C*------------------------------------------------------------------------*
287100940830     C*? CHKCAR - CONTROLLA SE TARIFFA DI CARTELLO CARICATA IN MEMORIA
287200940830     C*------------------------------------------------------------------------*
287300940830     C     CHKCAR        BEGSR
287400940830     C*
287500940830     C* CONTROLLA ESISTENZA TARIFFA DI CARTELLO IN LINEA FRA QUELLE CARICATE
287600940830     C                   Z-ADD     1             I
287700940830DO  1C     I             DOWLE     WULTCA
287800940908     C     WSETLL        ANDEQ     'N'
287900940830     C*
288000940830     C* CONTROLLI DI VALIDITA'
288100941213IF  2C     WVOC          IFEQ      CVOC(I)
288200941213     C*
288300941213IF  3C     WPRG          IFEQ      CPRG(I)
288400940830     C     WFT1          ANDEQ     CFT1(I)
288500940830     C     WFT2          ANDEQ     CFT2(I)
288600940830     C     WKSC          ANDEQ     CCLI(I)
288700940830     C     WCTR          ANDEQ     CCTR(I)
288800940830     C     WCTS          ANDEQ     CCTS(I)
288900131002     C     Wnaz          ANDEQ     Cnaz(I)
289000131002     C     wcad          ANDEQ     Ccad(I)
289100940830     C*
289200941213IF  4C     WDSP          IFGE      CDDT(I)
289300940830     C     WDSP          ANDLE     CDST(I)
289400000306     C*
289500011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
289600011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
289700011227     C                   Z-ADD     CSGI(I)       W$SGI
289800011227     C                   Z-ADD     CSGF(I)       W$SGF
289900011227IF  5C                   IF        WTIV = '21' OR
290000011227     C                             WTIV = '22' OR
290100011227     C                             WTIV = '23' OR
290200011227     C                             WTIV = '25'
290300011227IF  6C     WDIVAC        IFNE      WDIVCE
290400011227IF  7C     CSGI(I)       IFNE      *ZEROS
290500011227     C                   Z-ADD     CSGI(I)       §IMI
290600011227     C                   MOVEL     WDIVAC        §DVI
290700011227     C                   EXSR      CNVEUR
290800011227     C                   Z-ADD     §IMO          W$SGI
290900011227E   7C                   ENDIF
291000011227IF  7C     CSGF(I)       IFNE      *ZEROS
291100011227     C                   Z-ADD     CSGF(I)       §IMI
291200011227     C                   MOVEL     WDIVAC        §DVI
291300011227     C                   EXSR      CNVEUR
291400011227     C                   Z-ADD     §IMO          W$SGF
291500011227E   7C                   ENDIF
291600011227E   6C                   ENDIF
291700011227E   5C                   ENDIF
291800011227     c*
291900000306IF  5C     WTIV          IFEQ      *BLANKS
292000000306     C     WTIV          ORNE      *BLANKS
292100011227     C     WQTA          ANDGE     W$SGI
292200011227     C     WQTA          ANDLE     W$SGF
292300940830     C*
292400940830     C* CARTELLO TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
292500940908     C                   MOVEL     'S'           WSETLL
292600940830     C                   MOVEL     'S'           WFLTT
292700940830     C*
292800940830     C                   Z-ADD     CVOC(I)       EVDVOC
292900940830     C                   Z-ADD     CPRG(I)       EVDPRG
293000940830     C                   MOVEL     'C'           EVDTTV
293100940830     C                   Z-ADD     CFT1(I)       EVDFT1
293200940830     C                   Z-ADD     CFT2(I)       EVDFT2
293300940830     C                   Z-ADD     CCLI(I)       EVDCLI
293400940830     C                   Z-ADD     CCTR(I)       EVDCTR
293500940830     C                   MOVEL     CCTS(I)       EVDCTS
293600131002     C                   movel     Ccad(I)       EVDcad
293700131002     C                   movel     Cnaz(I)       EVDnaz
293800940830     C                   Z-ADD     CDDT(I)       EVDDDT
293900940830     C                   Z-ADD     CDST(I)       EVDDST
294000940830     C                   Z-ADD     CSGI(I)       EVDSGI
294100940830     C                   Z-ADD     CSGF(I)       EVDSGF
294200940830     C                   Z-ADD     CITR(I)       EVDITR
294300940830     C                   Z-ADD     CIMI(I)       EVDIMI
294400940830     C                   Z-ADD     CIMA(I)       EVDIMA
294500940830     C                   MOVEL     CSEG(I)       EVDSEG
294600940830     C                   Z-ADD     CPER(I)       EVDPER
294700940830     C                   Z-ADD     CRPV(I)       EVDRPV
294800940830     C*
294900941213E   5C                   ENDIF
295000940830E   4C                   ENDIF
295100940830E   3C                   ENDIF
295200940830E   2C                   ENDIF
295300940830     C*
295400940830     C                   ADD       1             I
295500940830E   1C                   ENDDO
295600940907     C*
295700940908     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
295800940908     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
295900940908IF  1C     WSETLL        IFEQ      'N'
296000940908     C     WULTCA        ANDLT     200
296100940908     C                   MOVEL     'S'           WSETLL
296200940908E   1C                   ENDIF
296300940907     C*
296400940907     C                   ENDSR
296500940902     C*------------------------------------------------------------------------*
296600940902     C*? CARFIL - CARICA TUTTE LE TARIFFE DELLA FILIALE
296700940902     C*------------------------------------------------------------------------*
296800940902     C     CARFIL        BEGSR
296900940902     C*
297000940902     C* POSIZIONAMENTO E PRIMA LETTURA
297100940902     C     WFIL          SETLL     ECEVD03L
297200940902     C                   READ      ECEVD03L                               99
297300940902     C*
297400940902     C* FINO A FINE FILE
297500940902     C                   Z-ADD     *ZEROS        I
297600940902DO  1C     *IN99         DOWEQ     *OFF
297700940902     C     EVDFT1        ANDEQ     WFIL
297800940902IF  3C     EVDATB        IFEQ      *BLANKS
297900940902     C                   ADD       1             I
298000940902     C                   Z-ADD     EVDVOC        FVOC(I)
298100940902     C                   Z-ADD     EVDPRG        FPRG(I)
298200940902     C                   Z-ADD     EVDFT1        FFT1(I)
298300940902     C                   Z-ADD     EVDFT2        FFT2(I)
298400940902     C                   Z-ADD     EVDCLI        FCLI(I)
298500940902     C                   Z-ADD     EVDCTR        FCTR(I)
298600940902     C                   MOVEL     EVDCTS        FCTS(I)
298700131002     C                   movel     EVDcad        Fcad(I)
298800131002     C                   movel     EVDnaz        Fnaz(I)
298900940902     C                   Z-ADD     EVDDDT        FDDT(I)
299000940902     C                   Z-ADD     EVDDST        FDST(I)
299100940902     C                   Z-ADD     EVDSGI        FSGI(I)
299200940902     C                   Z-ADD     EVDSGF        FSGF(I)
299300940902     C                   Z-ADD     EVDITR        FITR(I)
299400940902     C                   Z-ADD     EVDIMI        FIMI(I)
299500940902     C                   Z-ADD     EVDIMA        FIMA(I)
299600940902     C                   MOVEL     EVDSEG        FSEG(I)
299700940902     C                   Z-ADD     EVDPER        FPER(I)
299800940902     C                   Z-ADD     EVDRPV        FRPV(I)
299900940902E   2C                   ENDIF
300000940902     C*
300100940902     C                   READ      ECEVD03L                               99
300200940902E   1C                   ENDDO
300300940902     C*
300400940902     C                   Z-ADD     I             WULTTF            6 0
300500940902     C*
300600940902     C*
300700940902     C                   ENDSR
300800940902     C*------------------------------------------------------------------------*
300900941213     C*? CHKFIL - CONTROLLA SE TARIFFA FILIALE GIA' IN MEMORIA
301000940902     C*------------------------------------------------------------------------*
301100940902     C     CHKFIL        BEGSR
301200940902     C*
301300941213     C* CONTROLLA ESISTENZA TARIFFA FILIALE FRA QUELLE CARICATE
301400940902     C                   Z-ADD     1             I
301500940908DO  1C     I             DOWLE     WULTTF
301600940908     C     WSETLL        ANDEQ     'N'
301700940902     C*
301800940902     C* CONTROLLI DI VALIDITA'
301900941213IF  2C     WVOC          IFEQ      FVOC(I)
302000941213     C*
302100941213IF  3C     WPRG          IFEQ      FPRG(I)
302200940902     C     WFT1          ANDEQ     FFT1(I)
302300940902     C     WFT2          ANDEQ     FFT2(I)
302400940902     C     WKSC          ANDEQ     FCLI(I)
302500940902     C     WCTR          ANDEQ     FCTR(I)
302600940902     C     WCTS          ANDEQ     FCTS(I)
302700131002     C     Wnaz          ANDEQ     Fnaz(I)
302800131002     C     wcad          ANDEQ     Fcad(I)
302900940902     C*
303000941213IF  4C     WDSP          IFGE      FDDT(I)
303100940902     C     WDSP          ANDLE     FDST(I)
303200011227     C*
303300011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
303400011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
303500011227     C                   Z-ADD     FSGI(I)       W$SGI
303600011227     C                   Z-ADD     FSGF(I)       W$SGF
303700011227IF  5C                   IF        WTIV = '21' OR
303800011227     C                             WTIV = '22' OR
303900011227     C                             WTIV = '23' OR
304000011227     C                             WTIV = '25'
304100011227IF  6C     WDIVAC        IFNE      WDIVCE
304200011227IF  7C     FSGI(I)       IFNE      *ZEROS
304300011227     C                   Z-ADD     FSGI(I)       §IMI
304400011227     C                   MOVEL     WDIVAC        §DVI
304500011227     C                   EXSR      CNVEUR
304600011227     C                   Z-ADD     §IMO          W$SGI
304700011227E   7C                   ENDIF
304800011227IF  7C     FSGF(I)       IFNE      *ZEROS
304900011227     C                   Z-ADD     FSGF(I)       §IMI
305000011227     C                   MOVEL     WDIVAC        §DVI
305100011227     C                   EXSR      CNVEUR
305200011227     C                   Z-ADD     §IMO          W$SGF
305300011227E   7C                   ENDIF
305400011227E   6C                   ENDIF
305500011227E   5C                   ENDIF
305600000306     C*
305700000306IF  5C     WTIV          IFEQ      *BLANKS
305800000306     C     WTIV          ORNE      *BLANKS
305900011227     C     WQTA          ANDGE     W$SGI
306000011227     C     WQTA          ANDLE     W$SGF
306100940902     C*
306200940902     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
306300940908     C                   MOVEL     'S'           WSETLL
306400940902     C                   MOVEL     'S'           WFLTT
306500940902     C*
306600940902     C                   Z-ADD     FVOC(I)       EVDVOC
306700940902     C                   Z-ADD     FPRG(I)       EVDPRG
306800940902     C                   MOVEL     'F'           EVDTTV
306900940902     C                   Z-ADD     FFT1(I)       EVDFT1
307000940902     C                   Z-ADD     FFT2(I)       EVDFT2
307100940902     C                   Z-ADD     FCLI(I)       EVDCLI
307200940902     C                   Z-ADD     FCTR(I)       EVDCTR
307300940902     C                   MOVEL     FCTS(I)       EVDCTS
307400131002     C                   movel     Fcad(I)       EVDcad
307500131002     C                   movel     Fnaz(I)       EVDnaz
307600940902     C                   Z-ADD     FDDT(I)       EVDDDT
307700940902     C                   Z-ADD     FDST(I)       EVDDST
307800940902     C                   Z-ADD     FSGI(I)       EVDSGI
307900940902     C                   Z-ADD     FSGF(I)       EVDSGF
308000940902     C                   Z-ADD     FITR(I)       EVDITR
308100940902     C                   Z-ADD     FIMI(I)       EVDIMI
308200940902     C                   Z-ADD     FIMA(I)       EVDIMA
308300940902     C                   MOVEL     FSEG(I)       EVDSEG
308400940902     C                   Z-ADD     FPER(I)       EVDPER
308500940902     C                   Z-ADD     FRPV(I)       EVDRPV
308600940902     C*
308700941213E   5C                   ENDIF
308800940902E   4C                   ENDIF
308900940902E   3C                   ENDIF
309000940902E   2C                   ENDIF
309100940902     C*
309200940902     C                   ADD       1             I
309300940902E   1C                   ENDDO
309400940902     C*
309500940902     C                   ENDSR
309600940907     C*------------------------------------------------------------------------*
309700940907     C*? CARCLI - CARICA LE TARIFFE DI UN CLIENTE
309800940907     C*------------------------------------------------------------------------*
309900940907     C     CARCLI        BEGSR
310000940907     C*
310100940907     C* POSIZIONAMENTO E PRIMA LETTURA
310200940907     C     WKSC          SETLL     ECEVD02L
310300940907     C                   READ      ECEVD02L                               99
310400940907     C*
310500940907     C* FINO A FINE FILE
310600940907     C                   Z-ADD     *ZEROS        I
310700940907DO  1C     *IN99         DOWEQ     *OFF
310800940907IF  2C     EVDCLI        ANDEQ     WKSC
310900940907     C     I             ANDLT     200
311000940907     C     EVDATB        IFEQ      *BLANKS
311100940907     C                   ADD       1             I
311200940907     C                   Z-ADD     EVDVOC        KVOC(I)
311300940907     C                   Z-ADD     EVDPRG        KPRG(I)
311400940907     C                   Z-ADD     EVDFT1        KFT1(I)
311500940907     C                   Z-ADD     EVDFT2        KFT2(I)
311600940907     C                   Z-ADD     EVDCLI        KCLI(I)
311700940907     C                   Z-ADD     EVDCTR        KCTR(I)
311800940907     C                   MOVEL     EVDCTS        KCTS(I)
311900131002     C                   movel     EVDcad        Kcad(I)
312000131002     C                   movel     EVDnaz        Knaz(I)
312100940907     C                   Z-ADD     EVDDDT        KDDT(I)
312200940907     C                   Z-ADD     EVDDST        KDST(I)
312300940907     C                   Z-ADD     EVDSGI        KSGI(I)
312400940907     C                   Z-ADD     EVDSGF        KSGF(I)
312500940907     C                   Z-ADD     EVDITR        KITR(I)
312600940907     C                   Z-ADD     EVDIMI        KIMI(I)
312700940907     C                   Z-ADD     EVDIMA        KIMA(I)
312800940907     C                   MOVEL     EVDSEG        KSEG(I)
312900940907     C                   Z-ADD     EVDPER        KPER(I)
313000940907     C                   Z-ADD     EVDRPV        KRPV(I)
313100940907E   2C                   ENDIF
313200940907     C*
313300940907     C                   READ      ECEVD02L                               99
313400940907E   1C                   ENDDO
313500940907     C*
313600940907     C                   Z-ADD     I             WULTTK            6 0
313700940907     C*
313800940907     C                   ENDSR
313900940831     C*------------------------------------------------------------------------*
314000940902     C*? CHKCLI - CONTROLLA SE TARIFFA CLIENTE GIA' IN MEMORIA
314100940831     C*------------------------------------------------------------------------*
314200940831     C     CHKCLI        BEGSR
314300940831     C*
314400940831     C* CONTROLLA ESISTENZA TARIFFA CLIENTE FRA QUELLE CARICATE
314500940831     C                   Z-ADD     1             I
314600940831DO  1C     I             DOWLE     WULTTK
314700940908     C     WSETLL        ANDEQ     'N'
314800940831     C*
314900940831     C* CONTROLLI DI VALIDITA'
315000940831IF  2C     WVOC          IFEQ      KVOC(I)
315100941213     C*
315200941213IF  3C     WPRG          IFEQ      KPRG(I)
315300940831     C     WFT1          ANDEQ     KFT1(I)
315400940831     C     WFT2          ANDEQ     KFT2(I)
315500940831     C     WKSC          ANDEQ     KCLI(I)
315600940831     C     WCTR          ANDEQ     KCTR(I)
315700940831     C     WCTS          ANDEQ     KCTS(I)
315800131002     C     Wnaz          ANDEQ     Knaz(I)
315900131002     C     wcad          ANDEQ     Kcad(I)
316000940831     C*
316100941213IF  4C     WDSP          IFGE      KDDT(I)
316200940831     C     WDSP          ANDLE     KDST(I)
316300011227     C*
316400011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
316500011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
316600011227     C                   Z-ADD     KSGI(I)       W$SGI
316700011227     C                   Z-ADD     KSGF(I)       W$SGF
316800011227IF  5C                   IF        WTIV = '21' OR
316900011227     C                             WTIV = '22' OR
317000011227     C                             WTIV = '23' OR
317100011227     C                             WTIV = '25'
317200011227IF  6C     WDIVAC        IFNE      WDIVCE
317300011227IF  7C     KSGI(I)       IFNE      *ZEROS
317400011227     C                   Z-ADD     KSGI(I)       §IMI
317500011227     C                   MOVEL     WDIVAC        §DVI
317600011227     C                   EXSR      CNVEUR
317700011227     C                   Z-ADD     §IMO          W$SGI
317800011227E   7C                   ENDIF
317900011227IF  7C     KSGF(I)       IFNE      *ZEROS
318000011227     C                   Z-ADD     KSGF(I)       §IMI
318100011227     C                   MOVEL     WDIVAC        §DVI
318200011227     C                   EXSR      CNVEUR
318300011227     C                   Z-ADD     §IMO          W$SGF
318400011227E   7C                   ENDIF
318500011227E   6C                   ENDIF
318600011227E   5C                   ENDIF
318700940831     C*
318800000306IF  5C     WTIV          IFEQ      *BLANKS
318900000306     C     WTIV          ORNE      *BLANKS
319000011227     C     WQTA          ANDGE     W$SGI
319100011227     C     WQTA          ANDLE     W$SGF
319200940831     C*
319300940902     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
319400940908     C                   MOVEL     'S'           WSETLL
319500940831     C                   MOVEL     'S'           WFLTT
319600940831     C*
319700940831     C                   Z-ADD     KVOC(I)       EVDVOC
319800940831     C                   Z-ADD     KPRG(I)       EVDPRG
319900940831     C                   MOVEL     'K'           EVDTTV
320000940831     C                   Z-ADD     KFT1(I)       EVDFT1
320100940831     C                   Z-ADD     KFT2(I)       EVDFT2
320200940831     C                   Z-ADD     KCLI(I)       EVDCLI
320300940831     C                   Z-ADD     KCTR(I)       EVDCTR
320400940831     C                   MOVEL     KCTS(I)       EVDCTS
320500131002     C                   movel     Kcad(I)       EVDcad
320600131002     C                   movel     Knaz(I)       EVDnaz
320700940831     C                   Z-ADD     KDDT(I)       EVDDDT
320800940831     C                   Z-ADD     KDST(I)       EVDDST
320900940831     C                   Z-ADD     KSGI(I)       EVDSGI
321000940831     C                   Z-ADD     KSGF(I)       EVDSGF
321100940831     C                   Z-ADD     KITR(I)       EVDITR
321200940831     C                   Z-ADD     KIMI(I)       EVDIMI
321300940831     C                   Z-ADD     KIMA(I)       EVDIMA
321400940831     C                   MOVEL     KSEG(I)       EVDSEG
321500940831     C                   Z-ADD     KPER(I)       EVDPER
321600940831     C                   Z-ADD     KRPV(I)       EVDRPV
321700940831     C*
321800941213E   5C                   ENDIF
321900941213E   4C                   ENDIF
322000940831E   3C                   ENDIF
322100940831E   2C                   ENDIF
322200940831     C*
322300940831     C                   ADD       1             I
322400940908E   1C                   ENDDO
322500940908     C*
322600940908     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
322700940908     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
322800940908IF  1C     WSETLL        IFEQ      'N'
322900940908     C     WULTTK        ANDLT     200
323000940908     C                   MOVEL     'S'           WSETLL
323100940908E   1C                   ENDIF
323200940831     C*
323300940831     C                   ENDSR
323400940830     C*------------------------------------------------------------------------*
323500940830     C*? CHKKSC - CONTROLLA SE IL CLIENTE E' TRA QUELLI CON TARIFFA PARTICOLARE
323600940830     C*------------------------------------------------------------------------*
323700940830     C     CHKKSC        BEGSR
323800940830     C*
323900940831     C* CONTROLLA ESISTENZA CLIENTE TRA QUELLI CON TARIFFA PARTICOLARE
324000940830     C                   MOVEL     'N'           WKSCOK            1
324100940902     C                   Z-ADD     1             IK                9 0
324200940902DO  1C     IK            DOWLE     WULTKS
324300940907     C     WKSCOK        ANDEQ     'N'
324400940830     C*
324500940907     C* CONTROLLI DI VALIDITA' (CLIENTE FRA QUELLI CARICATI)
324600940902IF  2C     WKSC          IFEQ      KSC(IK)
324700940830     C                   MOVEL     'S'           WKSCOK
324800940831     C* CARICA TUTTE LE TARIFFE DEL CLIENTE
324900940831     C                   EXSR      CARCLI
325000940830E   2C                   ENDIF
325100940830     C*
325200940902     C                   ADD       1             IK
325300940830E   1C                   ENDDO
325400940908     C*
325500940830     C                   ENDSR
325600011221     C*------------------------------------------------------------------------*
325700011221     C*?CNVEUR - CONVERTE I CAMPI DA DIVISA ESTERA A LIRE
325800011221     C*------------------------------------------------------------------------*
325900011221     C     CNVEUR        BEGSR
326000011221     C*
326100011221     C* AZZERA VARIABILI DI LAVORO
326200011221     C                   Z-ADD     *ZEROS        §IMO
326300011221     C*
326400011221     C                   CLEAR                   YEUR
326500011221     C                   MOVEL     §DVI          YECDVI
326600011221     C                   Z-ADD     §IMI          YECIMI
326700011221     C                   MOVEL     WDIVCE        YECDVO
326800011221     C                   MOVEL     '3'           YECDCO
326900011221     C                   MOVEL     'H'           YECTAR
327000011221     C                   CALL      'YEURCO'
327100011221     C                   PARM                    YEUR
327200011221IF  1C     YECESI        IFEQ      ' '
327300011221     C                   Z-ADD     YECIMO        §IMO
327400011221X   1C                   ELSE
327500011221     C                   Z-ADD     §IMI          §IMO
327600011221E   1C                   ENDIF
327700011221     C*
327800011221     C                   ENDSR
327900940907     C*------------------------------------------------------------------------*
328000940907     C*? CARCAR - CARICA LE TARIFFE DI CARTELLO
328100940907     C*------------------------------------------------------------------------*
328200940907     C     CARCAR        BEGSR
328300940907     C*
328400940907     C* POSIZIONAMENTO E PRIMA LETTURA
328500940907     C                   MOVEL     'C'           EVDTTV
328600940907     C     EVDTTV        SETLL     ECEVD04L
328700940907     C                   READ      ECEVD04L                               99
328800940907     C*
328900940907     C* FINO A FINE FILE E TARIFFE DI CARTELLO
329000940907     C                   Z-ADD     *ZEROS        I
329100940907DO  1C     *IN99         DOWEQ     *OFF
329200940907     C     EVDTTV        ANDEQ     'C'
329300940907     C     I             ANDLT     200
329400940907IF  2C     EVDATB        IFEQ      *BLANKS
329500940907     C                   ADD       1             I
329600940907     C                   Z-ADD     EVDVOC        CVOC(I)
329700940907     C                   Z-ADD     EVDPRG        CPRG(I)
329800940907     C                   Z-ADD     EVDFT1        CFT1(I)
329900940907     C                   Z-ADD     EVDFT2        CFT2(I)
330000940907     C                   Z-ADD     EVDCLI        CCLI(I)
330100940907     C                   Z-ADD     EVDCTR        CCTR(I)
330200940907     C                   MOVEL     EVDCTS        CCTS(I)
330300131002     C                   movel     EVDcad        Ccad(I)
330400131002     C                   movel     EVDnaz        Cnaz(I)
330500940907     C                   Z-ADD     EVDDDT        CDDT(I)
330600940907     C                   Z-ADD     EVDDST        CDST(I)
330700940907     C                   Z-ADD     EVDSGI        CSGI(I)
330800940907     C                   Z-ADD     EVDSGF        CSGF(I)
330900940907     C                   Z-ADD     EVDITR        CITR(I)
331000940907     C                   Z-ADD     EVDIMI        CIMI(I)
331100940907     C                   Z-ADD     EVDIMA        CIMA(I)
331200940907     C                   MOVEL     EVDSEG        CSEG(I)
331300940907     C                   Z-ADD     EVDPER        CPER(I)
331400940907     C                   Z-ADD     EVDRPV        CRPV(I)
331500940907E   2C                   ENDIF
331600940907     C*
331700940907     C                   READ      ECEVD04L                               99
331800940907E   1C                   ENDDO
331900940907     C*
332000940907     C                   Z-ADD     I             WULTCA            6 0
332100940907     C*
332200940907     C                   ENDSR
332300940907     C*------------------------------------------------------------------------*
332400940907     C*? CARKSC - CARICA I CLIENTI CON TARIFFE PARTICOLARI
332500940907     C*------------------------------------------------------------------------*
332600940907     C     CARKSC        BEGSR
332700940907     C*
332800940907     C* POSIZIONAMENTO E PRIMA LETTURA
332900940907     C                   MOVEL     'K'           EVDTTV
333000940907     C     EVDTTV        SETLL     ECEVD04L
333100940907     C                   READ      ECEVD04L                               99
333200940907     C*
333300940907     C* FINO A FINE FILE E TARIFFE DI CLIENTE
333400940907     C                   Z-ADD     *ZEROS        Y                 6 0
333500940907DO  1C     *IN99         DOWEQ     *OFF
333600940907     C     EVDTTV        ANDEQ     'K'
333700070626     C     Y             ANDLT     5000
333800940907IF  2C     EVDATB        IFEQ      *BLANKS
333900940907     C                   MOVEL     'S'           WFKSC             1
334000940907IF  3C     Y             IFGT      *ZEROS
334100940907     C                   Z-ADD     *ZEROS        YY                6 0
334200940907DO  4C                   DO        Y             YY
334300940907IF  5C     EVDCLI        IFEQ      KSC(YY)
334400940907     C                   MOVEL     'N'           WFKSC
334500940907E   5C                   ENDIF
334600940907E   4C                   ENDDO
334700940907E   3C                   ENDIF
334800940907IF  3C     WFKSC         IFEQ      'S'
334900940907     C                   ADD       1             Y
335000940907     C                   Z-ADD     EVDCLI        KSC(Y)
335100940907E   3C                   ENDIF
335200940907E   2C                   ENDIF
335300940907     C*
335400940907     C                   READ      ECEVD04L                               99
335500940907E   1C                   ENDDO
335600940907     C*
335700940907     C                   Z-ADD     Y             WULTKS            6 0
335800940907     C*
335900940907     C                   ENDSR
336000940907     C*------------------------------------------------------------------------*
336100940907     C*? CARMAG - CARICA LE MAGGIORAZIONI
336200940907     C*------------------------------------------------------------------------*
336300940907     C     CARMAG        BEGSR
336400940907     C*
336500940907     C* POSIZIONAMENTO E PRIMA LETTURA
336600940907     C     *LOVAL        SETLL     ECEVM01L
336700940907     C                   READ      ECEVM01L                               99
336800940907     C*
336900940907     C* FINO A FINE FILE E MAGGIORAZIONI
337000940907     C                   Z-ADD     *ZEROS        I
337100940907DO  1C     *IN99         DOWEQ     *OFF
337200041118     C     I             ANDLT     600
337300940907IF  2C     EVMATB        IFEQ      *BLANKS
337400940907     C                   ADD       1             I
337500940907     C                   Z-ADD     EVMVOM        MVOM(I)
337600940907     C                   Z-ADD     EVMPRM        MPRM(I)
337700940907     C                   Z-ADD     EVMVOC        MVOC(I)
337800940907     C                   Z-ADD     EVMPRG        MPRG(I)
337900100408     C                   Z-ADD     EVMviso       Mviso(I)
338000100408     C                   Z-ADD     EVMddt        Mddti(I)
338100100408     C                   Z-ADD     EVMdst        Mdsti(I)
338200100408     C                   movel     EVMapp        Mapp(I)
338300940907E   2C                   ENDIF
338400940907     C*
338500940907     C                   READ      ECEVM01L                               99
338600940907E   1C                   ENDDO
338700940907     C*
338800940907     C                   Z-ADD     I             WULTMA            6 0
338900940907     C*
339000940907     C                   ENDSR
339100940803     C*------------------------------------------------------------------------*
339200940803     C*? CARTAB - CARICA LE TABELLE
339300940803     C*------------------------------------------------------------------------*
339400930414     C     CARTAB        BEGSR
339500011221     C*
339600011221     C* REPERISCE LA DATA CORRENTE
339700011221     C                   TIME                    N14              14 0
339800011221     C                   MOVE      N14           N8                8 0
339900011221     C                   Z-ADD     N8            G08DAT
340000011221     C                   Z-ADD     *ZEROS        G08INV
340100011221     C                   MOVEL     '0'           G08ERR
340200011221     C                   CALL      'XSRDA8'
340300011221     C                   PARM                    WLBDA8
340400011221     C                   Z-ADD     G08INV        DATCOR            8 0
340500941024     C*
340600941024     C                   OPEN      TABEL00F
340700011221     C                   OPEN      TNTBE01L
340800020805     C                   OPEN      AZORG01L
340900940704     C*
341000940704     C* CARICA TABELLA -2R- RIFERIMENTO VOCE
341100940913     C     $ERR          IFEQ      'N'
341200940704     C                   Z-ADD     0             I
341300940704     C                   Z-ADD     CODUT         KTBUTE
341400940704     C                   MOVEL     '2R'          KTBCOD
341500940704     C                   MOVEL     *BLANKS       KTBKEY
341600940704     C     KEYTAB        SETLL     TABEL00F                           99
341700940704     C  N99              READ      TABEL00F                               99
341800940704     C     *IN99         IFEQ      '1'
341900940720     C                   MOVEL     MSG(3)        $MSG
342000940907     C                   EXSR      SCRERR
342100940704     C                   END
342200940704     C     *IN99         DOWEQ     '0'
342300940704     C     TBLCOD        ANDEQ     '2R'
342400940704     C     TBLFLG        IFEQ      *BLANKS
342500940704     C                   ADD       1             I
342600940704     C                   MOVEL     TBLKEY        C2R(I)
342700940707     C                   MOVEL     TBLUNI        DS2R
342800940707     C                   MOVEL     §2RTVA        T2R(I)
342900940704     C                   END
343000940704     C                   READ      TABEL00F                               99
343100940704     C                   END
343200940704     C                   END
343300940720     C*
343400940720     C* CARICA TABELLA -FA- CONDIZIONI DI PAGAMENTO
343500940913     C     $ERR          IFEQ      'N'
343600940720     C                   Z-ADD     0             I
343700940720     C                   Z-ADD     CODUT         KTBUTE
343800940720     C                   MOVEL     'FA'          KTBCOD
343900940720     C                   MOVEL     *BLANKS       KTBKEY
344000940720     C     KEYTAB        SETLL     TABEL00F                           99
344100940720     C  N99              READ      TABEL00F                               99
344200940720     C     *IN99         IFEQ      '1'
344300940720     C                   MOVEL     MSG(2)        $MSG
344400940907     C                   EXSR      SCRERR
344500940720     C                   END
344600940720     C     *IN99         DOWEQ     '0'
344700940720     C     TBLCOD        ANDEQ     'FA'
344800940720     C     TBLFLG        IFEQ      *BLANKS
344900940720     C                   ADD       1             I
345000940720     C                   MOVEL     TBLKEY        CFA(I)
345100940721     C                   MOVEL     TBLUNI        DSFA
345200940720     C                   MOVEL     §FARAT        WTM03             3
345300940720     C                   MOVE      WTM03         WTM030            3 0
345400940720     C     WTM030        IFLT      15
345500940720     C     WTM030        MULT      30            GFA(I)
345600940720     C                   ELSE
345700940720     C                   Z-ADD     WTM030        GFA(I)
345800940720     C                   END
345900940720     C                   END
346000940720     C                   READ      TABEL00F                               99
346100940720     C                   END
346200940720     C                   END
346300000626     C*
346400141204     C* CARICA TABELLA -7R-  PARTICOLARITÀ CONSEGNA
346500000626IF  1C     $ERR          IFEQ      'N'
346600000626     C                   Z-ADD     0             I
346700000626     C                   Z-ADD     CODUT         KTBUTE
346800141204     C                   MOVEL     '7R'          KTBCOD
346900000626     C                   MOVEL     *BLANKS       KTBKEY
347000000626     C     KEYTAB        SETLL     TABEL00F                           99
347100000626     C  N99              READ      TABEL00F                               99
347200000626IF  2C     *IN99         IFEQ      '1'
347300141204     C                   MOVEL     MSG(32)       $MSG
347400000626     C                   EXSR      SCRERR
347500000626E   2C                   ENDIF
347600000626DO  2C     *IN99         DOWEQ     '0'
347700141204     C     TBLCOD        ANDEQ     '7R'
347800000626IF  3C     TBLFLG        IFEQ      *BLANKS
347900000626     C                   ADD       1             I
348000141205     C                   MOVEL     TBLKEY        c7r(I)
348100141205     C                   MOVEL     TBLUNI        DS7R
348200141204     C                   MOVEL     §7RPINCODE    PIN(I)
348300000626E   3C                   ENDIF
348400141205     C                   READ      TABEL00F                               99
348500000626E   2C                   ENDDO
348600000626E   1C                   ENDIF
348700010420     C*
348800010420     C* CARICA TABELLA -1B-  CODICI TRATTAMENTO MERCE
348900010420IF  1C     $ERR          IFEQ      'N'
349000010420     C                   Z-ADD     0             I
349100010420     C                   Z-ADD     CODUT         KTBUTE
349200010420     C                   MOVEL     '1B'          KTBCOD
349300010420     C                   MOVEL     *BLANKS       KTBKEY
349400010420     C     KEYTAB        SETLL     TABEL00F                           99
349500010420     C  N99              READ      TABEL00F                               99
349600010420IF  2C     *IN99         IFEQ      '1'
349700010801     C                   MOVEL     MSG(28)       $MSG
349800010420     C                   EXSR      SCRERR
349900010420E   2C                   ENDIF
350000010420DO  2C     *IN99         DOWEQ     '0'
350100010420     C     TBLCOD        ANDEQ     '1B'
350200010420IF  3C     TBLFLG        IFEQ      *BLANKS
350300010420     C                   ADD       1             I
350400010420     C                   MOVEL     TBLKEY        CTM(I)
350500010420     C                   MOVEL     TBLUNI        DS1B
350600010420     C                   MOVEL     §1BF12        CF12(I)
350700050801     C                   MOVEL     §1BF16        CF16(I)
350800030530     C                   MOVEL     §1BDKC        CDKC(I)
350900010420E   3C                   ENDIF
351000010420     C                   READ      TABEL00F                               99
351100010420E   2C                   ENDDO
351200010420E   1C                   ENDIF
351300131002     C*
351400131002     C* CARICA TABELLA -15-  nazioni per reperire cap fittizio
351500131002IF  1C     $ERR          IFEQ      'N'
351600131002     C                   Z-ADD     0             I
351700131002     C                   Z-ADD     CODUT         KTBUTE
351800131002     C                   MOVEL     '15'          KTBCOD
351900131002     C                   MOVEL     *BLANKS       KTBKEY
352000131002     C     KEYTAB        SETLL     TABEL00F                           99
352100131002     C  N99              READ      TABEL00F                               99
352200131002IF  2C     *IN99         IFEQ      '1'
352300131002     C                   MOVEL     MSG(28)       $MSG
352400131002     C                   EXSR      SCRERR
352500131002E   2C                   ENDIF
352600131002DO  2C     *IN99         DOWEQ     '0'
352700131002     C     TBLCOD        ANDEQ     '15'
352800131002IF  3C     TBLFLG        IFEQ      *BLANKS
352900131002     C                   ADD       1             I
353000131002     C                   MOVEL     TBLKEY        d15(I)
353100131002     C                   MOVEL     TBLUNI        DS15
353200131002     C                   MOVEL     §15pcf        pcf(I)
353300131002     C                   MOVEL     §15ecf        ecf(I)
353400131002E   3C                   ENDIF
353500131002     C                   READ      TABEL00F                               99
353600131002E   2C                   ENDDO
353700131002E   1C                   ENDIF
353800091007     C* CARICA TABELLA -2K-  blocco competenza
353900091007IF  1C     $ERR          IFEQ      'N'
354000091007     C                   Z-ADD     0             I
354100091007     C                   Z-ADD     CODUT         KTBUTE
354200091007     C                   MOVEL     '2K'          KTBCOD
354300091007     C                   MOVEL     *BLANKS       KTBKEY
354400091007     C     KEYTAB        SETLL     TABEL00F                           99
354500091007     C  N99              READ      TABEL00F                               99
354600091007IF  2C     *IN99         IFEQ      '1'
354700091007     C                   MOVEL     MSG(28)       $MSG
354800091007     C                   EXSR      SCRERR
354900091007E   2C                   ENDIF
355000091007DO  2C     *IN99         DOWEQ     '0'
355100091007     C     TBLCOD        ANDEQ     '2K'
355200091007IF  3C     TBLFLG        IFEQ      *BLANKS
355300091007     C                   MOVEL     TBLUNI        DS2K
355400091009     C                   Z-ADD     §2KNBC        WNBCD             5 0
355500091007E   3C                   ENDIF
355600091007     C                   READ      TABEL00F                               99
355700091007E   2C                   ENDDO
355800091007E   1C                   ENDIF
355900011221     c*
356000011221     C* DIVISE PER CONVERSIONE -OUTPUT C/ECONOMICO-
356100011221     C                   CLEAR                   DSGEDCE
356200011221     C                   MOVEL     *blanks       WDIVCE            3
356300011221     C                   MOVEL(P)  'GED'         KTECOD
356400011221     C                   MOVEL(P)  'CE'          KTEKE1
356500011221     C     KEYTBE        CHAIN     TNTBE01L
356600011221IF  1C                   IF        %found(TNTBE01L)
356700011221     C                   MOVEL     TBEUNI        DSGEDCE
356800011221E   1C                   ENDIF
356900011221IF  1C                   IF        §GEDCEDAT > DATCOR
357000011221     C                   EVAL      WDIVCE = §GEDCEDIP
357100011221X   1C                   ELSE
357200011221     C                   EVAL      WDIVCE = §GEDCEDIV
357300011221E   1C                   ENDIF
357400011221IF  1C                   IF        WDIVCE = *blanks
357500011221     C                   EVAL      WDIVCE = 'ITL'
357600011221E   1C                   ENDIF
357700011221     c*
357800011221     C* DIVISE PER CONVERSIONE -INPUT ANAGRAFICHE C/ECONOMICO-
357900020102     C                   CLEAR                   DGEDAC
358000011221     C                   MOVEL     *blanks       WDIVAC            3
358100011221     C                   MOVEL(P)  'GED'         KTECOD
358200011221     C                   MOVEL(P)  'AC'          KTEKE1
358300011221     C     KEYTBE        CHAIN     TNTBE01L
358400011221IF  1C                   IF        %found(TNTBE01L)
358500020102     C                   MOVEL     TBEUNI        DGEDAC
358600011221E   1C                   ENDIF
358700011221IF  1C                   IF        §GEDACDAT > DATCOR
358800011221     C                   EVAL      WDIVAC = §GEDACDIP
358900011221X   1C                   ELSE
359000020102     C                   EVAL      WDIVAC = §GEDACDID
359100011221E   1C                   ENDIF
359200011221IF  1C                   IF        WDIVAC = *blanks
359300011221     C                   EVAL      WDIVAC = 'ITL'
359400011221E   1C                   ENDIF
359500941024     C*
359600020213     C* CARICA TABELLA -IC-  CODICI QUALITÀ CLEINTE
359700020213IF  1C     $ERR          IFEQ      'N'
359800020213     C                   Z-ADD     0             I
359900020213     C                   Z-ADD     CODUT         KTBUTE
360000020213     C                   MOVEL     'IC'          KTBCOD
360100020213     C                   MOVEL     *BLANKS       KTBKEY
360200020213     C     KEYTAB        SETLL     TABEL00F                           99
360300020213     C  N99              READ      TABEL00F                               99
360400020213IF  2C     *IN99         IFEQ      '1'
360500020213     C                   MOVEL     MSG(29)       $MSG
360600020213     C                   EXSR      SCRERR
360700020213E   2C                   ENDIF
360800020213DO  2C     *IN99         DOWEQ     '0'
360900020213     C     TBLCOD        ANDEQ     'IC'
361000020213IF  3C     TBLFLG        IFEQ      *BLANKS
361100020213     C                   ADD       1             I
361200020213     C                   MOVEL     TBLKEY        CLV(I)
361300020213E   3C                   ENDIF
361400020213     C                   READ      TABEL00F                               99
361500020213E   2C                   ENDDO
361600020213E   1C                   ENDIF
361700020805     C*
361800020805     C* CARICA TABELLA -NTW- NETWORK
361900020805IF  1C     $ERR          IFEQ      'N'
362000020805     C                   Z-ADD     *ZEROS        I
362100020805     C                   MOVEL(P)  'NTW'         KTECOD
362200020805     C     KEYTB2        SETLL     TNTBE01L                           99
362300020805     C     KEYTB2        READE     TNTBE01L                               99
362400020805IF  2C     *IN99         IFEQ      *ON
362500020805     C                   MOVEL     MSG(30)       $MSG
362600020805     C                   EXSR      SCRERR
362700020805E   2C                   ENDIF
362800020805DO  2C     *IN99         DOWEQ     *OFF
362900020805     C                   MOVEL     TBEUNI        DNTW
363000020805     C                   ADD       1             I
363100020805     C                   MOVEL     TBEKE1        SNTW(I)
363200020805     C                   MOVEL     §NTWFIE       SNFIE(I)
363300020805     C     KEYTB2        READE     TNTBE01L                               99
363400020805E   2C                   ENDDO
363500020805E   1C                   ENDIF
363600040903     C* CARICA TABELLA -DFE- per peso limite tariffa pak fedex
363700040903     c                   clear                   ddfe
363800040903IF  1C     $ERR          IFEQ      'N'
363900040903     C                   MOVEL(P)  'DFE'         KTECOD
364000040903     C                   MOVEL(P)  'FED'         KTEKE1
364100040903     C     KEYTBE        chain     TNTBE01L
364200040903     c                   if        %found(tntbe01l)
364300040903     c                   movel(p)  tbeuni        ddfe
364400040903     c                   end
364500040903E   1C                   ENDIF
364600020805     C*
364700020805     C* CARICA FLAG ITALIA/ESTERO DALL'ORGANIGRAMMA
364800020805if  1C     $ERR          IFEQ      'N'
364900020805     C     *LOVAL        SETLL     AZORG01L
365000020805     C                   READ      AZORG01L                               98
365100020805DO  2C     *IN98         DOWEQ     *OFF
365200020805IF  3C     ORGFVA        IFEQ      *BLANKS
365300020805     C     ORGFAG        ANDNE     'V'
365400020805     C                   Z-ADD     ORGFIL        F                 4 0
365500020805     C                   MOVEL     ORGDE3        OG143
365600020805     C                   MOVEL     ORGCTS        CTS(F)
365700020805     C                   Z-ADD     1             II
365800020805     C     §OGNTW        LOOKUP    SNTW(II)                               97
365900020805IF  4C     *IN97         IFEQ      *OFF
366000020805     C                   MOVEL     MSG(31)       $MSG
366100020805     C                   EXSR      SCRERR
366200020805X   4C                   ELSE
366300020805     C                   MOVEL     SNTW(II)      NTW(F)
366400020805     C                   MOVEL     SNFIE(II)     FIE(F)
366500020805E   4C                   ENDIF
366600020805E   3C                   ENDIF
366700020805     C                   READ      AZORG01L                               98
366800020805E   2C                   ENDDO
366900020805e   1C                   ENDIF
367000020213     C*
367100941024     C                   CLOSE     TABEL00F
367200011221     C                   CLOSE     TNTBE01L
367300020805     C                   CLOSE     AZORG01L
367400940704     C*
367500940704     C                   ENDSR
367600940803     C*------------------------------------------------------------------------*
367601151113     C     srpack        BEGSR
368100940803     C*------------------------------------------------------------------------*
368101151113     c* l'informazione se packing list non verrà mai cambiata, quindi non
368102151113     c* importa prevederne la variazione
368103151113     c                   clear                   flgpackl          1
368104151116     c                   clear                   dar5gen
368105151113     C                   MOVEL     'GEN'         AR5TRD
368106151116     C     KEYAR5        CHAIN     FIAR531C
368107151116     c                   if        %found(fiar531c)
368110151116     c                   movel     ar5uni        dar5gen
368113151116      * Flag packing list
368114151116     c                   if        §AR5als ='S'
368115151116     c                             or §AR5alx  ='S'
368116151116     c                   eval      flgpackl = 'S'
368119151116     C                   endif
368120151113     c                   end
368201151113     C                   ENDSR
368202151113     C*------------------------------------------------------------------------*
368203151113     C*? DATCLI - RECUPERA DATI CLIENTE
368204151113     C*           INPUT: - D$DKSC (CODICE DEL CLIENTE)
368205151113     C*           OUTPUT: - WGGP (GIORNI PAGAMENTO)
368206151113     C*                   - WCLV (CLASSE CLIENTE)
368207151113     C*------------------------------------------------------------------------*
368208151113     C     DATCLI        BEGSR
368300971222     C*
368400971222     C* LANCIA PGM PER RECUPERARE DATI CLIENTE
368500971222     C                   CLEAR                   TIBS69
368600971222     C                   Z-ADD     WCLI          I69KIN
368700971222     C                   Z-ADD     WCLI          I69KCP
368800971222     C                   CALL      'TIBS69R'
368900971222     C                   PARM                    TIBS69
369000971222     C                   PARM                    ACODS
369100971222     C                   PARM                    INDDS
369200971222     C                   PARM                    CLPDS
369300971222     C                   PARM                    CLSDS
369400971222IF  1C     O69ERR        IFEQ      '1'
369500020701     C                   MOVE      'BS69'        O69MSG
369600020520     C                   MOVEL     O69MSG        DSMSG
369700971222     C                   MOVEL     O69MSG        $MSG
369800971222     C                   EXSR      SCRERR
369900971222X   1C                   ELSE
370000940722     C*
370100940722     C* RECUPERA GIORNI PAGAMENTO DALL'ANAGRAFICO INDIRIZZI (CNIND)
370200940722     C                   Z-ADD     *ZEROS        WGGP              3 0
370300940802     C                   MOVE      INDCDP        WTM03
370400940722     C                   Z-ADD     1             I
370500940722     C     WTM03         LOOKUP    CFA(I)                                 98
370600971222IF  2C     *IN98         IFEQ      '0'
370700940722     C                   MOVEL     MSG(23)       $MSG
370800940907     C                   EXSR      SCRERR
370900971222X   2C                   ELSE
371000940722     C                   Z-ADD     GFA(I)        WGGP
371100971222E   2C                   ENDIF
371200940722     C*
371300940722     C* RECUPERA LA CLASSE DEL CLIENTE DALL'ANAGRAFICO PARTICOLARITA' (CNCLP)
371400020213     C                   Z-ADD     1             I
371500020213     C     CLPCLV        LOOKUP    CLV(I)                                 98
371600020213IF  2C     *IN98         IFEQ      *ON
371700971229     C                   MOVEL     CLPCLV        WCLV              1
371800971222X   2C                   ELSE
371900940722     C                   MOVEL     MSG(24)       $MSG
372000940907     C                   EXSR      SCRERR
372100971222E   2C                   ENDIF
372200971222     C*
372300971222E   1C                   ENDIF
372400940722     C*
372500940722     C                   ENDSR
372600940831     C*------------------------------------------------------------------------*
372700940831     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
372800940831     C*------------------------------------------------------------------------*
372900940831     C     SUBDAT        BEGSR
373000940831     C*
373100940831     C                   TIME                    DTTIME
373200940831     C                   Z-ADD     DTANN         DSANN
373300940831     C                   Z-ADD     DTMES         DSMES
373400940831     C                   Z-ADD     DTGIO         DSGIO
373500940901     C                   Z-ADD     DTSEC         DSSEC
373600940831     C*
373700940831     C                   ENDSR
373800940907     C*------------------------------------------------------------------------*
373900940907     C*?SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SLSDE (ERRORI SALDI)
374000940907     C*------------------------------------------------------------------------*
374100940907     C     SCRERR        BEGSR
374200940907     C*
374300940913     C                   MOVEL     'S'           $ERR
374400970407     C*
374500970407     C* SE LANCIATO DAL PGM TNTE50R (DELTA IN FILIALE) NON SCRIVO ERRORI
374600970407     C     D$GPGF        IFEQ      'TNTE50R'
374700970407     C                   MOVEL     $MSG          D$GERR
374800970407     C                   MOVEL     $MSG          D$GMSG
374900970407     C                   ELSE
375000941026     C*
375100941026     C* SCRIVE SOLO SE NON VI E' GIA' STATO ALTRO ERRORE (IN QUESTO O ALTRO PGM)
375200941026     C     D$GERR        IFEQ      *BLANKS
375300941026     C*
375400940907     C                   EXSR      SUBDAT
375500940907     C                   Z-ADD     DSDAT8        ERRDAT
375600940907     C                   Z-ADD     DTORA6        ERRTIM
375700941026     C                   ADD       1             D$GPRE
375800941026     C                   Z-ADD     D$GPRE        ERRPRG
375900941213     C                   Z-ADD     D$DAAS        ERRAAS
376000940907     C                   MOVEL     D$DTBL        ERRTBL
376100940907     C                   Z-ADD     D$DLNP        ERRLNP
376200940907     C                   Z-ADD     D$DNRS        ERRNRS
376300940907     C                   Z-ADD     D$DNSP        ERRNSP
376400941201     C                   Z-ADD     D$DAAS        DSAAS
376500941201     C                   Z-ADD     D$DMGS        DSMGS
376600941201     C                   Z-ADD     DSDAT8        ERRDSP
376700941007     C                   MOVEL     $MSG          DSMSG
376800941007     C                   MOVEL     DSDER         ERRDER
376900941007     C                   MOVEL     DSCER         ERRCER
377000950123     C                   MOVEL     D$GSTO        ERRSTS
377100941026     C                   Z-ADD     D$GDAT        ERRDAM
377200941026     C                   Z-ADD     D$GTIM        ERRORM
377300941026     C                   MOVEL     D$GUSR        ERRUSR
377400941026     C                   MOVEL     D$GPGF        ERRPGF
377500950118     C                   MOVEL     D$GFIL        ERRFIL
377600950118     C                   MOVEL     D$GMBR        ERRMBR
377700950118     C                   MOVEL     D$GLIB        ERRLIB
377800991119     C     D$GFIL        IFEQ      'TITAS10F'
377900020703     C     D$GFIL        OREQ      'TITAS00F'
378000950118     C                   MOVEL     D$PRE1        ERRRP1
378100950118     C                   MOVEL     D$PRE2        ERRRP2
378200950118     C                   MOVEL     D$DRE1        ERRRD1
378300950118     C                   MOVEL     D$DRE2        ERRRD2
378400950123     C                   ENDIF
378500950123     C     D$GFIL        IFEQ      'TNCSB00F'
378600950123     C                   MOVEL     D$PCO1        ERRRP1
378700950123     C                   MOVEL     D$PCO2        ERRRP2
378800950123     C                   MOVEL     D$DCO1        ERRRD1
378900950123     C                   MOVEL     D$DCO2        ERRRD2
379000950123     C                   ENDIF
379100030205     C     D$GFIL        IFEQ      'FIAR500F'
379200030205     C     D$GTRD        IFEQ      'TRS'
379300030205     C                   MOVEL     D$PAR5T       ERRRP1
379400030205     C                   MOVEL     D$DAR5T       ERRRD1
379500030205     C                   ENDIF
379600030801     C     D$GTRD        IFEQ      'BAN'
379700030801     C     D$GTRD        OREQ      'BNB'
379800030801     C                   MOVEL     D$PAR5B       ERRRP1
379900030801     C                   MOVEL     D$DAR5B       ERRRD1
380000030801     C                   ENDIF
380100030801     C                   ENDIF
380200030801     C                   WRITE     ECERR
380300941026     C*
380400941026     C                   MOVEL     DSCER         D$GERR
380500941026     C                   MOVEL     DSDER         D$GMSG
380600941026     C*
380700941026     C                   ENDIF
380800970407     C                   ENDIF
380900940907     C*
381000940907     C                   ENDSR
381100940907     C*------------------------------------------------------------------------*
381200940907     C*? RUTINZ - ROUTINE INIZIALE
381300940907     C*------------------------------------------------------------------------*
381400940907     C     RUTINZ        BEGSR
381500940907     C*
381600940907     C* PARAMETRI IN INPUT ALLA ROUTINE
381700940907     C     *ENTRY        PLIST
381800940907     C                   PARM                    DS$ECO
381900121120     c                   movel     d$gxxx        dsd$g
382000121120     c                   if        d$gfsf = 'S' and
382100121120     c                             not %open(esvoc01l)
382200121120     c                   open      esvoc01l
382300121120     c                   open      esvoc11l
382400121120     c                   end
382500940907     C*
382600940907     C* CHIAVE LETTURA TABEL00F - COMPLETA
382700940907     C     KEYTAB        KLIST
382800940907     C                   KFLD                    KTBUTE
382900940907     C                   KFLD                    KTBCOD
383000940907     C                   KFLD                    KTBKEY
383100011221     C*
383200011221     C* CHIAVE LETTURA TNTBE01L - PARZIALE
383300011221     C     KEYTBE        KLIST
383400011221     C                   KFLD                    KTECOD
383500011221     C                   KFLD                    KTEKE1
383600020805     C*
383700020805     C* CHIAVE LETTURA TNTBE01L - PARZIALE
383800020805     C     KEYTB2        KLIST
383900020805     C                   KFLD                    KTECOD
384000091007     C* CHIAVE LETTURA ECECT01L - PARZIALE
384100091007     C     KEYECT        KLIST
384200091007     C                   KFLD                    WNBCD
384300091007     C                   KFLD                    vcmp(v)
384400940907     C*
384500940907     C* CHIAVE LETTURA ECEVT01L - COMPLETA
384600940907     C     KEYEVT        KLIST
384700940907     C                   KFLD                    EVTVOC
384800940907     C                   KFLD                    EVTPRG
384900091009     C* CHIAVE LETTURA ESVOC01L - COMPLETA
385000091009     C     KEYEs         KLIST
385100091009     C                   KFLD                    vocfil
385200091009     C                   KFLD                    vocdat
385300091009     C                   KFLD                    vocvoc
385400091009     C                   KFLD                    vocsgi
385500091009     C                   KFLD                    vocsgf
385600100518     C     KEYEs10       KLIST
385700100518     C                   KFLD                    v10fil
385800100518     C                   KFLD                    v10dat
385900100518     C                   KFLD                    v10voc
386000940907     C*
386100131002     C* CHIAVE LETTURA ecevd06l - COMPLETA
386200940907     C     KEYEVD        KLIST
386300940907     C                   KFLD                    EVDVOC
386400940907     C                   KFLD                    EVDPRG
386500940907     C                   KFLD                    EVDTTV
386600940907     C                   KFLD                    EVDFT1
386700940907     C                   KFLD                    EVDFT2
386800940907     C                   KFLD                    EVDCLI
386900940907     C                   KFLD                    EVDCTR
387000940907     C                   KFLD                    EVDCTS
387100131002     C                   KFLD                    EVDnaz
387200131002     C                   KFLD                    EVDCAd
387300940907     C                   KFLD                    EVDDDT
387400940907     C                   KFLD                    EVDSGI
387500940907     C*
387600940907     C* CHIAVE LETTURA ECEVD02L - COMPLETA
387700940907     C     KEYEV2        KLIST
387800940907     C                   KFLD                    EVDCLI
387900940907     C*
388000940907     C* CHIAVE LETTURA ECEVM01L - COMPLETA
388100940907     C     KEYEVM        KLIST
388200940907     C                   KFLD                    EVMVOM
388300940907     C                   KFLD                    EVMPRM
388400940907     C                   KFLD                    EVMVOC
388500940907     C                   KFLD                    EVMPRG
388600940907     C*
388700940907     C* CHIAVE LETTURA ECERR01L - COMPLETA
388800940907     C     KEYERR        KLIST
388900940907     C                   KFLD                    ERRDAT
389000940907     C                   KFLD                    ERRTIM
389100940907     C                   KFLD                    ERRPRG
389101151113     C* CHIAVE LETTURA FIAR531C - PARZIALE
389102151113     C     KEYAR5        KLIST
389103151116     C                   KFLD                    D$DAAS
389104151116     C                   KFLD                    D$DLNP
389105151116     C                   KFLD                    D$DNRS
389106151116     C                   KFLD                    D$DNSP
389107151113     C                   KFLD                    AR5TRD
389108170317     C* CHIAVE LETTURA TIGCP01L
389109170317     C     KGIAC         KLIST
389110170317     C                   KFLD                    D$DAAS
389111170317     C                   KFLD                    D$DLNP
389112170317     C                   KFLD                    D$DNRS
389113170317     C                   KFLD                    D$DNSP
389114170317     C                   KFLD                    GCPFRG
389115170317     C     KGIACO        KLIST
389116170317     C                   KFLD                    D$OAAS
389117170317     C                   KFLD                    D$OLNP
389118170317     C                   KFLD                    D$ONRS
389119170317     C                   KFLD                    D$ONSP
389120170317     C                   KFLD                    GCPFRG
389200940907     C*
389300940907     C* DEFINISCO CAMPI
389400940907     C     *LIKE         DEFINE    TBLKUT        KTBUTE
389500940907     C     *LIKE         DEFINE    TBLCOD        KTBCOD
389600940907     C     *LIKE         DEFINE    TBLKEY        KTBKEY
389700011221     C     *LIKE         DEFINE    TBECOD        KTECOD
389800011221     C     *LIKE         DEFINE    TBEKE1        KTEKE1
389900940907     C* CAMPI VARI
390000940907     C     *LIKE         DEFINE    EVTARF        WVOL
390100991008     C     *LIKE         DEFINE    D$DPKF        WPES
390200040427     C     *LIKE         DEFINE    D$DPKF        WDPES
390300940907     C     *LIKE         DEFINE    EVTVOC        WVOC
390400940907     C     *LIKE         DEFINE    EVTPRG        WPRG
390500000306     C     *LIKE         DEFINE    EVTTIV        WTIV
390600940907     C     *LIKE         DEFINE    EVTRFI        WRIF
390700940907     C     *LIKE         DEFINE    EVDTTV        WTTV
390800940907     C     *LIKE         DEFINE    EVDFT1        WFT1
390900940907     C     *LIKE         DEFINE    EVDFT2        WFT2
391000940907     C     *LIKE         DEFINE    EVDCLI        WKSC
391100940907     C     *LIKE         DEFINE    EVDCTR        WCTR
391200940907     C     *LIKE         DEFINE    EVDCTS        WCTS
391300131002     C     *LIKE         DEFINE    EVDnaz        Wnaz
391400131002     C     *LIKE         DEFINE    EVDCAd        wcad
391500940907     C     *LIKE         DEFINE    EVTDDV        WDSP
391600940907     C     *LIKE         DEFINE    EVTDDV        WDDSP
391700940908     C     *LIKE         DEFINE    EVTDDV        WSPDAT
391800940907     C     *LIKE         DEFINE    D$DKSC        WCLI
391900940907     C     *LIKE         DEFINE    D$DLNP        W$LNP
392000111020     C     *LIKE         DEFINE    D$Dlna        W$LNA
392100940907     C     *LIKE         DEFINE    D$DTFP        W$TFP
392200940907     C     *LIKE         DEFINE    D$DKSC        W$KSC
392300940908     C     *LIKE         DEFINE    D$DKSC        WSPKSC
392400940907     C     *LIKE         DEFINE    D$DCCM        W$CCM
392500940907     C     *LIKE         DEFINE    D$DMCT        W$MCT
392600011227     C     *LIKE         DEFINE    EVDSGI        W$SGI
392700011227     C     *LIKE         DEFINE    EVDSGF        W$SGF
392800011227     C     *LIKE         DEFINE    EVDITR        W$ITR
392900011227     C     *LIKE         DEFINE    EVDIMI        W$IMI
393000011227     C     *LIKE         DEFINE    EVDIMA        W$IMA
393100020102     C     *LIKE         DEFINE    YECIMI        §IMI
393200020102     C     *LIKE         DEFINE    YECDVI        §DVI
393300020102     C     *LIKE         DEFINE    YECIMO        §IMO
393400021029     C     *LIKE         DEFINE    §1BF12        WCF12
393500050801     C     *LIKE         DEFINE    §1BF16        WCF16
393600030530     C     *LIKE         DEFINE    §1BDKC        WCDKC
393700950123     C*
393800950123     C* FACCIO LETTURA DI ECERR FASULLA PER NON FARE VENIRE MESSAGGIO
393900950123     C* AD OGNI APERTURA DI DEL FILE
394000950123     C   99
394100950123     CANN99KEYERR        CHAIN     ECERR01L                           99
394200940907     C*
394300940907     C* IMPOSTO CAMPI DI GESTIONE PGM
394400940913     C                   MOVEL     'N'           $ERR              1
394500941006     C                   MOVEL     *BLANKS       $MSG             80
394600940907     C*
394700940907     C* RECUPERO IL CODICE DELL'AZIENDA
394800940907     C                   Z-ADD     1             CODUT             1 0
394900940907     C*
395000940907     C* SE PGM CHIAMATO PER CHIUSURA ESCE
395100991214     C     D$GSTS        IFEQ      'C'
395200991214     C     D$GSTS        OREQ      'S'
395300991214     C                   ELSE
395400940907     C*
395500940907     C* PORTA LE IMMAGINI BOLLA PRIMA E DOPO DALLA DS INPUT ALLA DS PGM
395600940907     C                   MOVEL     D$PRE1        D$PDS1
395700940907     C                   MOVEL     D$PRE2        D$PDS2
395800940907     C                   MOVEL     D$DRE1        D$DDS1
395900940907     C                   MOVEL     D$DRE2        D$DDS2
396000941201     C                   MOVEL     D$ORE1        D$ODS1
396100941201     C                   MOVEL     D$ORE2        D$ODS2
396200940907     C*
396300940907     C* PORTA I RICAVI IN INPUT NELLA SCHIERA DEL PGM
396400950125     C                   MOVEA     D$DCCD        CAN
396500950125     C                   MOVEA     D$DICD        IAN
396600030506DO  2C     1             DO        75            I                 6 0
396700940907     C                   MOVE      CAN(I)        DSWCA
396800940907     C                   Z-ADD     DSWCN         RCD(I)
396900011221     C                   MOVE      IAN(I)        DSWIA
397000940907     C                   Z-ADD     DSWIN         RIM(I)
397100940907E   2C                   ENDDO
397200940907     C*
397300940907     C* IMPOSTA A ZEROS LE SCHIERE IN OUTPUT
397400940907     C                   CLEAR                   CCD
397500940907     C                   CLEAR                   CIM
397600940907     C                   CLEAR                   CIMP
397700940907     C                   CLEAR                   VIMP
397800940907     C                   CLEAR                   Q2R
397801151229     C                   clear                   Vfil
397802151229     C                   clear                   Vqta
397803151229     C                   clear                   Vsgi
397804151229     C                   clear                   Vsgf
397900940907     C*
398000940907     C* RECUPERO DATI VARI
398100941201     C                   Z-ADD     D$DAAS        DSAAS
398200941201     C                   Z-ADD     D$DMGS        DSMGS
398300941201     C                   Z-ADD     DSDAT8        WDDSP
398400940907     C*
398500940907     C* CARICA LE TABELLE NECESSARIE AL PGM
398600940907     C* CARICA LE TARIFFE DI CARTELLO
398700940907     C* SOLO NEL CASO IN CUI NON SIANO ANCORA STATE CARICATE
398800940913     C     $ERR          IFEQ      'N'
398900940907     C     CTABSN        IFNE      'S'
399000940907     C     D$GSTS        ANDNE     'C'
399100940907     C                   MOVEL     'S'           CTABSN            1
399200940907     C                   EXSR      CARTAB
399300940907     C                   EXSR      CARCAR
399400940907     C                   EXSR      CARKSC
399500940907     C                   EXSR      CARMAG
399600940907     C                   ENDIF
399700940907     C                   ENDIF
399800940907     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI AL POSTO DEI DATI DOPO
399900940907     C     D$OLNP        IFNE      *ZEROS
400000950913     C     D$DFIG        ANDEQ     'S'
400100940907     C                   Z-ADD     D$OLNP        W$LNP
400200940907     C                   ELSE
400300940907     C                   Z-ADD     D$DLNP        W$LNP
400400940907     C                   ENDIF
400500940907     C     D$OTFP        IFNE      *ZEROS
400600950913     C     D$DFIG        ANDEQ     'S'
400700940907     C                   Z-ADD     D$OTFP        W$TFP
400800940907     C                   ELSE
400900940907     C                   Z-ADD     D$DTFP        W$TFP
401000940907     C                   ENDIF
401100940907     C     D$OKSC        IFNE      *ZEROS
401200950913     C     D$DFIG        ANDEQ     'S'
401300940907     C                   Z-ADD     D$OKSC        W$KSC
401400940907     C                   ELSE
401500940907     C                   Z-ADD     D$DKSC        W$KSC
401600940907     C                   ENDIF
401700940907     C     D$OCCM        IFNE      *ZEROS
401800950913     C     D$DFIG        ANDEQ     'S'
401900940907     C                   Z-ADD     D$OCCM        W$CCM
402000940907     C                   ELSE
402100940907     C                   Z-ADD     D$DCCM        W$CCM
402200940907     C                   ENDIF
402300940907     C     D$OMCT        IFNE      *BLANKS
402400950913     C     D$DFIG        ANDEQ     'S'
402500940907     C                   MOVEL     D$OMCT        W$MCT
402600940907     C                   ELSE
402700940907     C                   MOVEL     D$DMCT        W$MCT
402800940907     C                   ENDIF
402900111020     c* verifico se la linea di arrivo è in £6
403000111020     c                   z-add     d$dlna        w$lna
403100111020     c                   clear                   fnlv55ds
403200111020     c                   z-add     d$dlna        d55lin
403300111020     c                   movel     '6'           d55tpt
403400111020     C                   eval      d55drf = WDDSP
403500111020     c                   if        d$ddti <> 0
403600111020     C                   eval      d55drf = d$ddti
403700111020     c                   end
403800111020     c                   call      'FNLV55R'
403900111020     c                   parm                    fnlv55ds
404000111020     c                   if        d55tfa <> 0
404100111020     c                   eval      w$lna = d55tfa
404200111020     c                   end
404300021029     C*
404400021029     C* CARICA I DATI DEL CLIENTE di fatturazione, SE DIVERSO DALL'ULTIMO LETTO
404500021029     C     $ERR          IFEQ      'N'
404600021029     C     W$KSC         IFNE      WCLI
404700021029     C                   Z-ADD     W$KSC         WCLI
404800021029     C                   EXSR      DATCLI
404900021029     C                   ENDIF
405000021029     C                   ENDIF
405100021029     C*
405200140203     c* calcolo le qta della spedizione per saldi tempi uffici
405300140203     c                   if        D$GFSF = 'S'
405400140203     c                   exsr      srqta
405500140203     c                   end
405600940907     C*
405700940907     C                   ENDIF
405800940907     C*
405900940907     C                   ENDSR
406000940907     C*------------------------------------------------------------------------*
406100940907     C*? STAMPA - STAMPA RIGA
406200940907     C*------------------------------------------------------------------------*
406300941229     C*****+++   STAMPA    BEGSR
406400940902     C*
406500941024     C* APRE IL FILE DI STAMPA
406600941229     C****++++             OPEN PRTF198
406700941024     C*
406800940902     C* RECUPERO LA DATA E L'ORA
406900941229     C*****++++            Z-ADDUDATE     DATA    60
407000941229     C****+++++            TIME           ORA     60
407100940902     C*
407200940902     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
407300941229     C****+++++            EXCPTTESTA
407400940902     C*
407500940902     C* STAMPA RIGA DI DETTAGLIO TEMPI
407600941229     C***++++++            EXCPTRIGA
407700940902     C*
407800940902     C* STAMPA RIGA FINE
407900941229     C****+++++            EXCPTSTAFIN
408000941024     C*
408100941024     C* CHIUDE IL FILE DI STAMPA
408200941229     C****+++++            CLOSEPRTF198
408300941229     C****++++
408400941229     C***++++++            ENDSR
408500940907     O*------------------------------------------------------------------------*
408600941229     O*PRTF198 E  102           TESTA
408700941229     O****++++                            8 'TRECD8R '
408800941229     O****++++                           82 'PRESTAZIONI PGM'
408900941229     O****++++                          150 'Data:'
409000941229     O****++++                 DATA     159 '  /  /  '
409100941229     O****++++                          167 'Ora:'
409200941229     O****++++                 ORA      178 '  .  .  '
409300941229     O****++++                          190 'Pagina:'
409400941229     O****++++                 PAGE  Z  198
409500941229     O*------------------------
409600941229     O****++++E  1             RIGA
409700941229     O****++++                           30 'NUMERO DI BOLLE.....:'
409800941229     O*****++++++++++++++++++++WBOLLE2   40
409900941229     O****++++E  1             RIGA
410000941229     O****++++                           30 'TENT.RICERCA TARIFFA:'
410100941229     O******+++++++++++++++++++WTEEVD2   40
410200941229     O****++++E  1             RIGA
410300941229     O****++++                           30 'TENT.SETLL   TARIFFA:'
410400941229     O******+++++++++++++++++++WSEEVD2   40
410500941229     O****++++E  1             RIGA
410600941229     O****++++                           30 'TENT.LETTURE TARIFFA:'
410700941229     O*****++++++++++++++++++++WLEEVD2   40
410800941229     O****++++E  1             RIGA
410900941229     O****++++                           30 'TENT.CARTELLO.......:'
411000941229     O*****++++++++++++++++++++WTECAR2   40
411100941229     O****++++E  1             RIGA
411200941229     O****++++                           30 'CARTELLO IN MEMORIA.:'
411300941229     O****+++++++++++++++++++++WMECAR2   40
411400941229     O****++++E  1             RIGA
411500941229     O****++++                           30 'CARTELLO LETTE......:'
411600941229     O*****++++++++++++++++++++WLECAR2   40
411700941229     O****++++E  1             RIGA
411800941229     O****++++                           30 'TENT.FILIALE........:'
411900941229     O*****++++++++++++++++++++WTEFIL2   40
412000941229     O****++++E  1             RIGA
412100941229     O****++++                           30 'FILIALE IN MEMORIA..:'
412200941229     O*****++++++++++++++++++++WMEFIL2   40
412300941229     O****++++E  1             RIGA
412400941229     O****++++                           30 'FILIALE LETTE.......:'
412500941229     O*****++++++++++++++++++++WLEFIL2   40
412600941229     O****++++E  1             RIGA
412700941229     O****++++                           30 'TENT.CLIENTE........:'
412800941229     O*****++++++++++++++++++++WTEKSC2   40
412900941229     O****++++E  1             RIGA
413000941229     O****++++                           30 'CLIENTE IN MEMORIA..:'
413100941229     O*****++++++++++++++++++++WMEKSC2   40
413200941229     O****++++E  1             RIGA
413300941229     O****++++                           30 'CLIENTE LETTE.......:'
413400941229     O******+++++++++++++++++++WLEKSC2   40
413500941229     O****++++E  1             RIGA
413600941229     O****++++                           30 'TENT.MAGGIORAZIONI..:'
413700941229     O******+++++++++++++++++++WTEMAG2   40
413800941229     O****++++E  1             RIGA
413900941229     O****++++                           30 'MAGGIOR. IN MEMORIA.:'
414000941229     O******+++++++++++++++++++WMEMAG2   40
414100941229     O****++++E  1             RIGA
414200941229     O****++++                           30 'MAGGIORAZIONI LETTE.:'
414300941229     O*****++++++++++++++++++++WLEMAG2   40
414400940907     O*------------------------
414500941229     O****++++E   60           STAFIN
414600941229     O*****+++                           76 '*** FINE STAMPA ***'
414700940907     O*------------------------------------------------------------------------*
414800941201**   MSG
414900141205TRECD8R- Tabella '7O' non trovata. Avvertire CED.                         TB7O 1
415000941201TRECD8R- Tabella 'FA' non trovata. Avvertire CED.                         TBFA 2
415100941201TRECD8R- Tabella '2R' non trovata. Avvertire CED.                         TB2R 3
415200941201TRECD8R- Tipo tariffa '00' (quintali) non trovata.                        TT00 4
415300941201TRECD8R- Tipo tariffa '01' (colli) non trovata.                           TT01 5
415400941201TRECD8R- Tipo tariffa '02' (n.spedizioni) non trovata.                    TT02 6
415500941201TRECD8R- Tipo tariffa '03' (chilogrammi) non trovata.                     TT03 7
415600941201TRECD8R- Tipo tariffa '04' (volume) non trovata.                          TT04 8
415700951213TRECD8R- Tipo tariffa '09' (stop ritiro) non trovata.                     TT09 9
415800941201TRECD8R- Tipo tariffa '20' (val.limite risarcibile) non trovata.          TT2010
415900941201TRECD8R- Tipo tariffa '21' (imponibile reale) non trovata.                TT2111
416000941201TRECD8R- Tipo tariffa '22' (contrassegno) non trovata.                    TT2212
416100941201TRECD8R- Tipo tariffa '23' (val merce assicur.) non trovata.              TT2313
416200941201TRECD8R- Tipo tariffa '24' (n.giorni pagamento) non trovata.              TT2414
416300941201TRECD8R- Tipo tariffa '25' (imp.comp.sede) non trovata.                   TT2515
416400951213TRECD8R- Tipo tariffa '10' (stop consegna) non trovata.                   TT1016
416500991029TRECD8R- Tipo tariffa '11' (peso x giorni giacenza) non trovata.          TT1117
416600141204TRECD8R- Codice consegna non trovato                                      VNTR18
416700941201TRECD8R- Tipo tariffa della voce non trovata in schiera.                  TTNT19
416800941201TRECD8R- Qta x ricerca scaglione non trovata.                             QRSN20
416900941201TRECD8R- Manca tariffa.Voce xxx.999 999                                   MATA21
417000941201TRECD8R- Voce di maggiorazione senza voci da maggiorare.                  VMSV22
417100941201TRECD8R- Condizione di pagamento non trovata.                             CPNT23
417200020213TRECD8R- Codice importanza cliente non trovato.                           APNT24
417300000201TRECD8R- Tipo tariffa '12' (giorni giacenza) non trovata.                 TT1125
417400000623TRECD8R- Codice consegna anomala non trovata                              CANT26
417500010420TRECD8R- Codice trattamento merce non trovata                             TMNT27
417600010801TRECD8R- Tabella '1B' non trovata. Avvertire CED.                         TB1B28
417700020213TRECD8R- Tabella 'IC' non trovata. Avvertire CED.                         TBIC29
417800020805TRECD8R- Tabella 'NTW' non trovata. Avvertire CED.                        TNTW30
417900020805TRECD8R- Network filiale non trovato in tab. 'NTW'. Avvertire CED.        NWNT31
418000021029TRECD8R- Tabella '7R' non trovata. Avvertire CED.                         TB7R32
418100141204TRECD8R- Codice particolarità consegna non trovato                        TB7R33
418200030506TRECD8R- Più di 75 voci calcolate. Avvertire CED.                         P75V34
418300030409TRECD8R- Tipo tariffa '06' (bancali) non trovata.                         TT0635
418301150227TRECD8R- Tipo tariffa '13' (bancali eur) non trovata.                     TT1336
418302150227TRECD8R- Tipo tariffa '14' (bancali epal) non trovata.                    TT1437
418303150227TRECD8R- Tipo tariffa '15' (bancali leggeri) non trovata.                 TT1538
418700020805TRECD8R-                                                                      39
418800020805TRECD8R-                                                                      40
