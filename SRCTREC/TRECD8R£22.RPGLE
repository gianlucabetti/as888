000100991116     H*------------------------------------------------------------------------*
000200950303     H*                                                                        *
000300940906     H*   PARTENDO DALLA DS ESTERNA PASSATA DAL PGM DI LETTURA                 *
000400940906     H*   DELLA DATAQ, CALCOLA LE COMPETENZE DELLA BOLLA                       *
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
002100940524     FECEVT01L  IF   E           K DISK
002200940524     FECEVD01L  IF   E           K DISK
002300940831     FECEVD02L  IF   E           K DISK
002400010807     F                                     RENAME(ECEVD000:ECEVD2)
002500940902     FECEVD03L  IF   E           K DISK
002600010807     F                                     RENAME(ECEVD000:ECEVD3)
002700940905     FECEVD04L  IF   E           K DISK
002800010807     F                                     RENAME(ECEVD000:ECEVD4)
002900940524     FECEVM01L  IF   E           K DISK
003000950123     FECERR01L  UF A E           K DISK
003100941024     FTABEL00F  IF   E           K DISK    USROPN
003200011221     FTNTBE01L  IF   E           K DISK    USROPN
003300020805     FAZORG01L  IF   E           K DISK    USROPN
003400941229     F*PRTF198 O   F     198     OF    LPRINTER                        UC
003500940906     D*------------------------------------------------------------------------*
003600941229     D*PRTF198  66FL 60OL
003700940906     D*------------------------------------------------------------------------*
003800940906     D* SCHIERE
003900940906     D*------------------------------------------------------------------------*
004000940906     D*-------------------
004100930304     D* MESSAGGI
004200940906     D*-------------------
004300020805     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
004400940906     D*-------------------
004500940704     D* SCHIERE RICAVI IN INPUT AL PGM
004600940906     D*-------------------
004700030506     D RCD             S              3  0 DIM(75)
004800090914     D RIM             S             13  5 DIM(75)
004900940906     D*-------------------
005000940704     D* SCHIERA COMPETENZE IN OUTPUT AL PGM
005100940906     D*-------------------
005200030506     D CCD             S              3  0 DIM(75)
005300090914     D CIM             S             13  5 DIM(75)
005400940906     D*-------------------
005500940704     D* SCHIERA COMPETENZE (INDICE=CODICE COMPETENZA)
005600940906     D*-------------------
005700090914     D CIMP            S             13  5 DIM(999)
005800940906     D*-------------------
005900940525     D* SCHIERA VOCI (INDICE=CODICE VOCE)
006000940906     D*-------------------
006100940704     D VPRG            S              3  0 DIM(999)
006200940704     D VTVO            S              1    DIM(999)
006300940721     D VDDV            S              8  0 DIM(999)
006400940721     D VDSV            S              8  0 DIM(999)
006500940707     D VVRV            S              2    DIM(999)
006600940714     D VTIV            S              2    DIM(999)
006700940603     D VSPT            S              1    DIM(999)
006800940603     D VCMP            S              3  0 DIM(999)
006900940607     D VRFI            S              2    DIM(999)
007000940607     D VRFE            S              2    DIM(999)
007100940607     D VRTI            S              2    DIM(999)
007200940607     D VRTE            S              2    DIM(999)
007300940607     D VRPI            S              2    DIM(999)
007400940607     D VRPE            S              2    DIM(999)
007500940607     D VRKI            S              2    DIM(999)
007600940607     D VRKE            S              2    DIM(999)
007700940704     D VARF            S             11  3 DIM(999)
007800940704     D VARL            S             11  3 DIM(999)
007900940704     D VARO            S             11  3 DIM(999)
008000090914     D VIMP            S             13  5 DIM(999)
008100030113     D VCOP            S              1    DIM(999)
008200030113     D VDAT            S              1    DIM(999)
008300030113     D VAID            S              1    DIM(999)
008400030422     D VRPV            S              3  1 DIM(999)
008500030422     D VSPS            S              1    DIM(999)
008600940603     D VC              S              1    DIM(999)
008700940607     D VV              S              1    DIM(999)
008800991027     D VS              S              1    DIM(999)
008900940829     D*-------------------
009000940829     D* SCHIERE TARIFFE DI CARTELLO
009100940829     D*-------------------
009200021111     D CVOC            S              3  0 DIM(300)
009300021111     D CPRG            S              3  0 DIM(300)
009400021111     D CFT1            S              3  0 DIM(300)
009500021111     D CFT2            S              3  0 DIM(300)
009600021111     D CCLI            S              7  0 DIM(300)
009700021111     D CCTR            S              3  0 DIM(300)
009800021111     D CCTS            S              2    DIM(300)
009900021111     D CCAP            S              5  0 DIM(300)
010000021111     D CDDT            S              8  0 DIM(300)
010100021111     D CDST            S              8  0 DIM(300)
010200021111     D CSGI            S             13  3 DIM(300)
010300021111     D CSGF            S             13  3 DIM(300)
010400090914     D CITR            S             13  5 DIM(300)
010500090914     D CIMI            S             13  5 DIM(300)
010600090914     D CIMA            S             13  5 DIM(300)
010700021111     D CSEG            S              1    DIM(300)
010800021111     D CPER            S              5  2 DIM(300)
010900021111     D CRPV            S              3  1 DIM(300)
011000060313     D DSVN            S              1    DIM(20)
011100940902     D*-------------------
011200940902     D* SCHIERE TARIFFE DELL'ULTIMA FILAILE DEL CLIENTE
011300940902     D*-------------------
011400021111     D FVOC            S              3  0 DIM(600)
011500021111     D FPRG            S              3  0 DIM(600)
011600021111     D FFT1            S              3  0 DIM(600)
011700021111     D FFT2            S              3  0 DIM(600)
011800021111     D FCLI            S              7  0 DIM(600)
011900021111     D FCTR            S              3  0 DIM(600)
012000021111     D FCTS            S              2    DIM(600)
012100021111     D FCAP            S              5  0 DIM(600)
012200021111     D FDDT            S              8  0 DIM(600)
012300021111     D FDST            S              8  0 DIM(600)
012400021111     D FSGI            S             13  3 DIM(600)
012500021111     D FSGF            S             13  3 DIM(600)
012600090914     D FITR            S             13  5 DIM(600)
012700090914     D FIMI            S             13  5 DIM(600)
012800090914     D FIMA            S             13  5 DIM(600)
012900021111     D FSEG            S              1    DIM(600)
013000021111     D FPER            S              5  2 DIM(600)
013100021111     D FRPV            S              3  1 DIM(600)
013200940831     D*-------------------
013300940831     D* SCHIERE TARIFFE DELL'ULTIMO CLIENTE
013400940831     D*-------------------
013500021111     D KVOC            S              3  0 DIM(300)
013600021111     D KPRG            S              3  0 DIM(300)
013700021111     D KFT1            S              3  0 DIM(300)
013800021111     D KFT2            S              3  0 DIM(300)
013900021111     D KCLI            S              7  0 DIM(300)
014000021111     D KCTR            S              3  0 DIM(300)
014100021111     D KCTS            S              2    DIM(300)
014200021111     D KCAP            S              5  0 DIM(300)
014300021111     D KDDT            S              8  0 DIM(300)
014400021111     D KDST            S              8  0 DIM(300)
014500021111     D KSGI            S             13  3 DIM(300)
014600021111     D KSGF            S             13  3 DIM(300)
014700090914     D KITR            S             13  5 DIM(300)
014800090914     D KIMI            S             13  5 DIM(300)
014900090914     D KIMA            S             13  5 DIM(300)
015000021111     D KSEG            S              1    DIM(300)
015100021111     D KPER            S              5  2 DIM(300)
015200021111     D KRPV            S              3  1 DIM(300)
015300940906     D*-------------------
015400940906     D* SCHIERE MAGGIORAZIONI
015500940906     D*-------------------
015600041118     D MVOM            S              3  0 DIM(600)
015700041118     D MPRM            S              3  0 DIM(600)
015800041118     D MVOC            S              3  0 DIM(600)
015900041118     D MPRG            S              3  0 DIM(600)
016000940906     D*-------------------
016100940906     D* CODICI CLIENTI CHE HANNO UNA TARIFFA PARTICOLARE (PER VELOCIZZARE)
016200940906     D*-------------------
016300070626     D KSC             S              7  0 DIM(5000)
016400940906     D*-------------------
016500940707     D* CODICI E QUANTITA TAB. 2R (TIPO TARIFFA)
016600940906     D*-------------------
016700940707     D C2R             S              2    DIM(100)
016800940707     D T2R             S              1    DIM(100)
016900010202     D Q2R             S             15  5 DIM(100)
017000940906     D*-------------------
017100940704     D* SCHIERE CODICI E IMPORTI IN INPUT E OUTPUT AL PGM
017200940906     D*-------------------
017300030506     D CAN             S              2    DIM(75)
017400030506     D PAN             S              2    DIM(75)
017500030506     D IAN             S              7    DIM(75)
017600030506     D COP             S              1    DIM(75)
017700030506     D DAT             S              1    DIM(75)
017800030506     D AID             S              1    DIM(75)
017900030506     D CMP             S              2    DIM(75)
018000940906     D*-------------------
018100940720     D* CODICI E GIORNI PAGAMENTO (TAB.FA)
018200940906     D*-------------------
018300940720     D CFA             S              3    DIM(100)
018400940720     D GFA             S              3  0 DIM(100)
018500020805     D*-------------------
018600020805     D* NETWORK
018700020805     D*-------------------
018800020805     D SNTW            S              3    DIM(20)
018900020805     D SNFIE           S              1    DIM(20)
019000971211     D*-------------------
019100971211     D* INDICE=COD.FILIALE ,   FL1 => BLANK=ITALIA,'E'=ESTERO
019200000623     D*-------------------
019300020503     D NTW             S              3    DIM(999)
019400020805     D FIE             S              1    DIM(999)
019500010116     D CTS             S              2    DIM(999)
019600000623     D*-------------------
019700000623     D* CONSEGNA ANOMALE
019800000623     D*-------------------
019900000623     D CCA             S              1    DIM(50)
020000000623     D CFPT            S              1    DIM(50)
020100000623     D CRMS            S              1    DIM(50)
020200021029     D*-------------------
020300021029     D* PARTICOLARITÀ CONSEGNE
020400021029     D*-------------------
020500130529     D*GMA             S              2    DIM(99)
020600130529     D*G1BF            S              1    DIM(99)
020700010420     D*-------------------
020800010420     D* TRATTAMENTO MERCE
020900010420     D*-------------------
021000010420     D CTM             S              2    DIM(100)
021100010420     D CF12            S              1    DIM(100)
021200050801     D CF16            S              1    DIM(100)
021300030530     D CDKC            S              1    DIM(100)
021400020213     D*-------------------
021500020213     D* IMPORTANZA CLIENTE
021600020213     D*-------------------
021700020213     D CLV             S              1    DIM(10)
021800940906     D*------------------------------------------------------------------------*
021900940906     D* INPUT
022000940906     D*------------------------------------------------------------------------*
022100111011     D*-------------------
022200111011     D* DS x pgm £6
022300111011     D*-------------------
022400111011     D fnlv55ds      E DS
022500030422     D*-------------------
022600040903     D* DS DFE tabella tariffe fedex
022700030422     D*-------------------
022800040903     D Ddfe          E DS
022900040903     D*-------------------
023000040903     D* DS LETTURA CAMPO EVTXXX
023100040903     D*-------------------
023200040903     D DEVTXXX       E DS
023300021028     D*-------------------
023400021028     D* DS LETTURA CAMPO TASFLO (D$DFLO)
023500021028     D*-------------------
023600021028     D DTASFLO       E DS
023700940906     D*-------------------
023800930323     D* DS ESTERNA IN INPUT AL PGM
023900940906     D*-------------------
024000940822     D DS$ECO        E DS
024100971222     D*-------------------
024200971222     D* DS PER LANCIO TIBS69R (RICERCA SU CNIND/CNCLP)
024300971222     D*-------------------
024400971222     D TIBS69        E DS                  EXTNAME(TIBS69DS)
024500971222     D ACODS         E DS                  EXTNAME(CNACO00F)
024600971222     D INDDS         E DS                  EXTNAME(CNIND00F)
024700971222     D CLPDS         E DS                  EXTNAME(CNCLP00F)
024800971222     D CLSDS         E DS                  EXTNAME(FNCLS00F)
024900940906     D*-------------------
025000991119     D* DS ESTERNA PER RIDEFINIRE IMMAGINE PRIMA E DOPO DI TITAS10F
025100940906     D*-------------------
025200941201     D TRECDS        E DS
025300940405     D  D$PDS1                 1    250
025400940405     D  D$PDS2               251    500
025500940405     D  D$DDS1               501    750
025600940405     D  D$DDS2               751   1000
025700941201     D  D$ODS1              1001   1250
025800941201     D  D$ODS2              1251   1500
025900940906     D*-------------------
026000940721     D* DS ESTERNA TABELLA 2R (TIPI RIFERIM. VOCE)
026100940906     D*-------------------
026200940721     D DS2R          E DS
026300940906     D*-------------------
026400940720     D* DS ESTERNA TABELLA FA (CONDIZIONI DI PAGAMENTO)
026500940906     D*-------------------
026600940720     D DSFA          E DS
026700000202     D*-------------------
026800000202     D* DS ESTERNA DESCRIZIONE 43 ORGANIGRAMMA
026900000202     D*-------------------
027000000202     D OG143         E DS
027100000623     D*-------------------
027200000623     D* DS ESTERNA TABELLA 7O (CONSEGNE ANOMALE)
027300000623     D*-------------------
027400000623     D DS7O          E DS
027500021029     D*-------------------
027600021029     D* DS ESTERNA TABELLA 7R (PARTICOLARITÀ CONSEGNE)
027700021029     D*-------------------
027800130529     D*DS7R          E DS
027900010420     D*-------------------
028000021029     D* DS ESTERNA TABELLA 1B (CODICI TRATTAMENTO MERCE)
028100010420     D*-------------------
028200010420     D DS1B          E DS
028300011221     D*-------------------
028400011221     D* DS DIVISE PER CONVERSIONI
028500011221     D*-------------------
028600011221     D DSGEDCE       E DS
028700020102     D DGEDAC        E DS
028800020805     D*-------------------
028900020805     D* DS NETWORK
029000020805     D*-------------------
029100020805     D DNTW          E DS
029200011221     D*-------------------
029300011221     D* CONVERSIONI IMPORTI
029400011221     D*-------------------
029500011221     D YEUR          E DS                  EXTNAME(YEURCODS)
029600940906     D*-------------------
029700930325     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
029800940906     D*-------------------
029900940831     D DS§DAT          DS
030000940831     D  DSSEC                  1      2  0
030100940831     D  DSANN                  3      4  0
030200941201     D  DSAAS                  1      4  0
030300940831     D  DSMES                  5      6  0
030400940831     D  DSGIO                  7      8  0
030500941201     D  DSMGS                  5      8  0
030600940831     D  DSDAT6                 3      8  0
030700940831     D  DSDAT8                 1      8  0
030800940831     D DS§TIM          DS
030900940831     D  DTORA                  1      2  0
031000940831     D  DTMIN                  3      4  0
031100940901     D  DTSCN                  5      6  0
031200940831     D  DTGIO                  7      8  0
031300940831     D  DTMES                  9     10  0
031400940901     D  DTSEC                 11     12  0
031500940901     D  DTANN                 13     14  0
031600940831     D  DTORA6                 1      6  0
031700940901     D  DTTIME                 1     14  0
031800940906     D*-------------------
031900940906     D* DS PER PASSAGGIO CAMPI PACKED -CODICE-
032000940906     D*-------------------
032100940825     D                 DS
032200940825     D  DSWCN                  1      3P 0
032300940825     D  DSWCA                  1      3
032400090904     D  DSWIN                  4     13P 5
032500011221     D  DSWIA                  4     13
032600940906     D*-------------------
032700940729     D* DS PER MESSAGGIO MANCA TARIFFA
032800940906     D*-------------------
032900940729     D                 DS
033000940729     D  DSMSG                  1     80
033100941006     D  DSVOCE                29     31
033200941006     D  DSTT1                 33     35
033300941006     D  DSTT2                 37     39
033400950519     D  DSCTS                 41     42
033500941007     D  DSDER                  1     74
033600941007     D  DSCER                 75     78
033700011221     D*-------------------
033800011221     D* DS  CONTROLLO DATA (8)
033900011221     D*-------------------
034000011221     D WLBDA8          DS                  INZ
034100011221     D  G08DAT                 1      8  0
034200011221     D  G08INV                 9     16  0
034300011221     D  G08ERR                17     17
034400011221     D  G08TGI                18     22  0
034500940803     C*------------------------------------------------------------------------*
034600940906     C*?MAIN LINE
034700940803     C*------------------------------------------------------------------------*
034800931105     C*
034900931105     C* ESEGUE ROUTINE INIZIALE
035000931105     C                   EXSR      RUTINZ
035100930602     C*
035200940405     C* SE NEL CAMPO STATUS TROVO 'C' (CLOSE)
035300931104     C* CHIUDO I FILE E FINISCO PGM
035400991214     C     D$GSTS        IFEQ      'C'
035500991214     C     D$GSTS        OREQ      'S'
035600991214     C     $ERR          ORNE      'N'
035700991214     C                   ELSE
035800991214     C*
035900991214     C***********D$GSTS    IFNE 'C'                        --- 1 -->
036000991214     C***********$ERR      ANDEQ'N'
036100940621     C*
036200940621     C* LANCIA ROUTINE CHE CALCOLA LE COMPETENZE
036300940621     C                   EXSR      GESCOM
036400931104     C*
036500940217     C                   END
036600930323     C*
036700940405     C* SE PGM LANCIATO CON 'C' (CLOSE) CHIUDE CON LR (CHIUDE FILE)
036800930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
036900940405     C     D$GSTS        IFEQ      'C'
037000991214     C     D$GSTS        OREQ      'S'
037100941229     C*****++++++++++++++++EXSR STAMPA
037200971223     C                   CLEAR                   TIBS69
037300971222     C                   MOVEL     'C'           I69TLA
037400971222     C                   CALL      'TIBS69R'                            98
037500971222     C                   PARM                    TIBS69
037600971222     C                   PARM                    ACODS
037700971222     C                   PARM                    INDDS
037800971222     C                   PARM                    CLPDS
037900971222     C                   PARM                    CLSDS
038000930323     C                   SETON                                        LR
038100930324     C                   ELSE
038200930324     C                   SETON                                        RT
038300930323     C                   END
038400940803     C*------------------------------------------------------------------------*
038500940906     C*?GESCOM - GESTIONE CALCOLO VOCI E COMPETENZE
038600940803     C*------------------------------------------------------------------------*
038700940608     C     GESCOM        BEGSR
038800940803     C*
038900940803     C* DETERMINA QUALI SONO LE VOCI DA CALCOLARE PER QUESTA BOLLA
039000940913     C     $ERR          IFEQ      'N'
039100940803     C                   EXSR      DETVOC
039200940803     C                   END
039300940608     C*
039400940707     C* CALCOLA VALORI TABELLA '2R' (QTA TIPO TARIFFA) DI QUESTA BOLLA
039500940913     C     $ERR          IFEQ      'N'
039600940608     C                   EXSR      CAL2R
039700940608     C                   END
039800940608     C*
039900940608     C* CONTROLLA SE TUTTE LE VOCI IN MEMORIA SONO VALIDE PER LA BOLLA (DEC/SCA)
040000940608     C* ED EVENTUALMENTE LE RICARICA
040100940913     C     $ERR          IFEQ      'N'
040200940608     C                   EXSR      CHKVOC
040300940608     C                   END
040400940608     C*
040500940608     C* VALORIZZA LE VOCI DA CALCOLARE
040600940913     C     $ERR          IFEQ      'N'
040700940608     C                   EXSR      VALVOC
040800940608     C                   END
040900940608     C*
041000940913     C     $ERR          IFEQ      'N'
041100940803     C*
041200940701     C* PORTA GLI IMPORTI DELLE VOCI NELLE COMPETENZE
041300940701     C                   DO        999           V
041400940701     C     VIMP(V)       IFNE      *ZEROS
041500940721     C                   Z-ADD     VCMP(V)       C                 9 0
041600940701     C                   ADD       VIMP(V)       CIMP(C)
041700940701     C                   ENDIF
041800940701     C                   ENDDO
041900940621     C*
042000940630     C* PORTA COD. E IMP. COMPETENZE NELLA SCHIERA OUTPUT
042100940621     C                   Z-ADD     *ZEROS        I
042200940621     C                   DO        999           C
042300940621     C     CIMP(C)       IFNE      *ZEROS
042400940621     C                   ADD       1             I
042500940630     C                   Z-ADD     C             CCD(I)
042600940630     C                   Z-ADD     CIMP(C)       CIM(I)
042700940621     C                   ENDIF
042800940621     C                   ENDDO
042900940704     C*
043000940704     C* PORTA COD. E IMP. COMPETENZE NELLA DS DI OUTPUT
043100030506     C     1             DO        75            I
043200950124     C                   Z-ADD     CCD(I)        DSWCN
043300950124     C                   MOVE      DSWCA         CAN(I)
043400950124     C                   Z-ADD     CIM(I)        DSWIN
043500950124     C                   MOVE      DSWIA         IAN(I)
043600940728     C                   ENDDO
043700950125     C                   MOVEA     CAN           D$DCCD
043800950125     C                   MOVEA     IAN           D$DICD
043900940803     C*
044000940803     C                   ENDIF
044100940729     C*
044200940729     C* PORTA COD. E IMP. VOCI NELLA DS DI OUTPUT >>>>> CANCELLARE !!!!! <<<<<<<
044300030114     C*--- D$GPGF        IFEQ      'TREC78R'
044400030114     C*--- D$GPGF        OREQ      'TREC79R'
044500940729     C                   Z-ADD     0             II
044600030506     C     1             DO        75            I
044700950124     C                   Z-ADD     *ZEROS        DSWCN
044800950124     C                   MOVE      DSWCA         CAN(I)
044900950124     C                   MOVE      DSWCA         PAN(I)
045000950124     C                   Z-ADD     *ZEROS        DSWIN
045100950124     C                   MOVE      DSWIA         IAN(I)
045200940729     C                   ENDDO
045300940729     C     1             DO        999           I
045400940729     C     VC(I)         IFEQ      'S'
045500991104     C     VIMP(I)       ANDGT     *ZEROS
045600940729     C                   ADD       1             II
045700030506     c     II            IFGT      75
045800030205     C                   MOVEL     MSG(34)       $MSG
045900030205     C                   EXSR      SCRERR
046000030205     C                   ELSE
046100950124     C                   Z-ADD     I             DSWCN
046200950124     C                   MOVE      DSWCA         CAN(II)
046300950124     C                   Z-ADD     VPRG(I)       DSWCN
046400950124     C                   MOVE      DSWCA         PAN(II)
046500950124     C                   Z-ADD     VIMP(I)       DSWIN
046600950125     C                   MOVE      DSWIA         IAN(II)
046700030114     C                   MOVEL     VCOP(I)       COP(II)
046800030114     C                   MOVEL     VDAT(I)       DAT(II)
046900030114     C                   MOVEL     VAID(I)       AID(II)
047000030123     C                   Z-ADD     VCMP(I)       DSWCN
047100030123     C                   MOVE      DSWCA         CMP(II)
047200030205     C                   ENDIF
047300940729     C                   ENDIF
047400940729     C                   ENDDO
047500950125     C                   MOVEA     CAN           D$DCVD
047600950125     C                   MOVEA     PAN           D$DPVD
047700030114     C                   MOVEA     IAN           D$DIVD
047800030114     C                   MOVEA     COP           D$DCOP
047900030114     C                   MOVEA     DAT           D$DDAT
048000030114     C                   MOVEA     AID           D$DAID
048100030123     C                   MOVEA     CMP           D$DCMP
048200030114     C*---               ENDIF
048300940608     C*
048400940608     C                   ENDSR
048500940803     C*------------------------------------------------------------------------*
048600940906     C*?DETVOC - DETERMINA LE VOCI DA CALCOLARE QUINDI VALIDE PER LA BOLLA
048700940803     C*------------------------------------------------------------------------*
048800940603     C     DETVOC        BEGSR
048900040526     c                   MOVEL     D$DFLO        DTASFLO
049000060313     c* controllo se ho la varia "d" per l'assicurazione AC BASE
049100060313     c                   MOVEa     D$Dsvn        Dsvn
049200060313     c                   z-add     1             jj                2 0
049300060313     c     'd'           lookup    dsvn                                   44
049400040427     C* ----- PESO -----
049500040427     c* PRIMA maggio 2004
049600040427     C* SEMPRE PESO DA FATTURARE
049700040428     C                   IF        WDDSP < 20040400
049800040427     C                   Z-ADD     D$DPKF        WDPES
049900040427X   1C                   ELSE
050000040427     c* DOPO maggio 2004
050100040427     C* SE I COLLI=COLLI CML PRENDO PESO CML
050200040427IF  2C     D$DPKC        IFGT      *ZEROS
050300040427     C     D$DNCL        ANDEQ     D$DNCP
050400040427     C                   Z-ADD     D$DPKC        WDPES
050500040427X   2C                   ELSE
050600040427     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
050700040427IF  3C     D$DPKF        IFGT      D$DPKC
050800040427     C                   Z-ADD     D$DPKF        WDPES
050900040427X   3C                   ELSE
051000040427     C                   Z-ADD     D$DPKC        WDPES
051100040427E   3C                   ENDIF
051200040427E   2C                   ENDIF
051300040427E   1C                   ENDIF
051400940526     C*
051500940603     C* IMPOSTA A NO TUTTI I FLAG 'DA CALCOLARE' DELLE VOCI
051600940721     C                   DO        999           V                 9 0
051700940603     C                   MOVEL     'N'           VC(V)
051800991027     C                   MOVEL     'S'           VS(V)
051900940526     C                   ENDDO
052000020621     C*
052100030630     C* =============> VOCI CHE VANNO APPLICATE SOLO SE LA BOLLA
052200030630     C*    NON E' UNA BOLLA DI RECUPERO (A3) OPPURE
052300030630     C*    E'     UNA BOLLA DI RECUPERO (A3) MA PER I CASI DI:
052400030630     C*    - RICERCA DOCUMENTO (FO)
052500030630     C*    - RITIRO ANNULLATO  (FY)
052600030630     C*    - ORM COMMISSIONATO (FT)
052700040520     C*    - BOLLE FIRMATE     (FF)
052800050801     C*    - POD IMAGE         (FI)
052900020621IF 1aC                   IF        D$DTBL <> 'A3' OR
053000020621     C                             (D$DTBL = 'A3' AND D$DCBO = 'FO') OR
053100030630     C                             (D$DTBL = 'A3' AND D$DCBO = 'FY') OR
053200040520     C                             (D$DTBL = 'A3' AND D$DCBO = 'FF') OR
053300050801     C                             (D$DTBL = 'A3' AND D$DCBO = 'FI') OR
053400030630     C                             (D$DTBL = 'A3' AND D$DCBO = 'FT')
053500020621     C*
053600020621     C* VOCE COMETENZA DELTA ======>
053700020621     C*    SEMPRE DA ATTIVARE SE LA BOLLA PASSA DA QUI
053800020621     C                   MOVEL     'S'           VC(900)
053900041119     c* attivo sempre le maggiorazioni di adeguamento tariffario anno 2005
054000091104     C                   IF        WDDSP > 20050100 and wddsp < 20100100
054100041119     C                   MOVEL     'S'           VC(600)
054200041119     C                   MOVEL     'S'           VC(601)
054300041119     C                   MOVEL     'S'           VC(602)
054400041119     C                   MOVEL     'S'           VC(603)
054500041119     C                   MOVEL     'S'           VC(604)
054600041119     c                   end
054700061201     c* attivo sempre le maggiorazioni di adeguamento tariffario anno 2007
054800091104     C                   IF        WDDSP > 20070100 and wddsp < 20100100
054900061201     C                   MOVEL     'S'           VC(605)
055000061201     C                   MOVEL     'S'           VC(606)
055100061201     c                   end
055200091104     C                   IF        WDDSP > 20090400 and wddsp < 20100100
055300090407     C                   MOVEL     'S'           VC(607)
055400090407     c                   end
055500091104     C                   IF        WDDSP > 20090900 and wddsp < 20100100
055600090908     C                   MOVEL     'S'           VC(608)
055700090908     c                   end
055800021029     C*
055900021029     C* REPERISCE I DATI DEL CODICE PARTICOLARITÀ CONSEGNA =======>
056000130529     C*                  CLEAR                   WG1BF
056100130529IF  1C*    D$DGMA        IFNE      *BLANKS
056200130529     C*                  Z-ADD     1             I
056300130529     C*    D$DGMA        LOOKUP    GMA(I)                                 98
056400130529IF  2C*    *IN98         IFEQ      *OFF
056500130529     C*                  MOVEL     MSG(33)       $MSG
056600130529     C*                  EXSR      SCRERR
056700130529X   2C*                  ELSE
056800130529     C*                  MOVEL     G1BF(I)       WG1BF
056900130529E   2C*                  ENDIF
057000130529E   1C*                  ENDIF
057100040520     C* CONTROLLO SE ESISTE =======>
057200040520     C*    RICERCA DOCUMENTO (fotocopia)
057300040520     C                   IF        WDDSP > 20040600 AND
057400040520if  1c                             D$DTBL = 'A3' AND
057500040520     c                             D$DCBO = 'FF'
057600040520     C                   MOVEL     'S'           VC(82)
057700040520E   1C                   ENDIF
057800050801     C*    POD IMAGE
057900050801     C                   IF        WDDSP > 20050900 AND
058000050801if  1c                             D$DTBL = 'A3' AND
058100050801     c                             D$DCBO = 'FI'
058200050801     C                   MOVEL     'S'           VC(199)
058300050801E   1C                   ENDIF
058400020621     c*
058500021029     C* CONTROLLO SE ESISTE =======>
058600021029     C*    RICERCA DOCUMENTO (fotocopia)
058700021029     C*    RESTITUZIONE NS BOLLA FIRMATA AL CLIENTE (originale)
058800020621if  1c     D$DTBL        IFEQ      'A3'
058900020621     c     D$DCBO        ANDEQ     'FO'
059000130529     C*    D$DGMA        ORNE      *BLANKS
059100130529     C*    WG1BF         ANDEQ     'L'
059200020621     C                   MOVEL     'S'           VC(75)
059300020621     C                   MOVEL     'S'           VC(185)
059400020621E   1C                   ENDIF
059500020621     c*
059600020621     C* CONTROLLO SE ESISTE =======> RITIRO ANNULLATO
059700020621if  1c     D$DTBL        IFEQ      'A3'
059800020621     c     D$DCBO        ANDEQ     'FY'
059900020621     C                   MOVEL     'S'           VC(16)
060000020621E   1C                   ENDIF
060100020621     C*
060200020621     C* CONTROLLO SE ESISTE =======> RICARICO SEDE EXPORT DPD
060300020621     C*    FRANCHI EXPORT O ASSEGNATI IMPORT
060400020621     C                   Z-ADD     W$LNP         F
060500111011     C                   Z-ADD     W$lna         F1
060600020805IF  1C     NTW(F)        IFEQ      'DPD'
060700020621     C     D$DTPO        ANDEQ     'A'
060800020805     C     NTW(F1)       OREQ      'DPD'
060900020621     C     D$DTPO        ANDEQ     'F'
061000100422     C                   MOVEL     'S'           VC(823)
061100040607     c* per sped. > 00/07/2004 attivo voce 305 FINANZIAM.TARIFFE IMPORT DPD
061200040607     C                   IF        WDDSP > 20040700
061300040607     C                   MOVEL     'S'           VC(305)
061400040607E   1C                   ENDIF
061500020621E   1C                   ENDIF
061600030604     c*
061700030604     C* CONTROLLO SE ESISTE =======> RECAPITO C/A MITTENTE
061800030604IF  1C     D$DCBO        IFEQ      'FW'
061900030604     C                   MOVEL     'S'           VC(164)
062000030604     C                   MOVEL     'S'           VC(187)
062100030604E   1C                   ENDIF
062200030630     c*
062300030630     C* CONTROLLO SE ESISTE =======> ORM COMMISSIONATO
062400030630if  1c     D$DTBL        IFEQ      'A3'
062500030630     c     D$DCBO        ANDEQ     'FT'
062600030630     C                   MOVEL     'S'           VC(76)
062700030630E   1C                   ENDIF
062800020620     C*
062900020621     C* CONTROLLO SE ESISTE =============> RICARICO SEDE NON DPD
063000020620     C*    SE LINEA PARTENZA E LINEA ARRIVO <> DPD
063100020620     C                   Z-ADD     W$LNP         F
063200111011     C                   Z-ADD     W$lna         F1
063300020805IF  1C     NTW(F)        IFNE      'DPD'
063400020805     C     NTW(F1)       ANDNE     'DPD'
063500020620     C*
063600020620     C*    SE CLIENTE 8888 O 9999 -CLIENTE NON CODIFICATO-
063700020620     C                   MOVE      W$KSC         KSCIS             4 0
063800020620IF  2C     KSCIS         IFEQ      8888
063900020620     C     KSCIS         OREQ      9999
064000100422     C                   MOVEL     'S'           VC(822)
064100020620X   2C                   ELSE
064200020620     C*
064300020620     C*    SE NON E' CLIENTE 8888 O 9999  -CLIENTE CODIFICATO-
064400020620IF  3C     D$DTSP        IFEQ      'D'
064500020620     C     WCLV          IFEQ      'A'
064600100422     C                   MOVEL     'S'           VC(816)
064700020620     C                   ENDIF
064800020620     C     WCLV          IFEQ      'B'
064900100422     C                   MOVEL     'S'           VC(817)
065000020620     C                   ENDIF
065100020620     C     WCLV          IFEQ      'C'
065200100422     C                   MOVEL     'S'           VC(818)
065300020620     C                   ENDIF
065400020620     C     WCLV          IFEQ      'T'
065500020620     C     WCLV          OREQ      'D'
065600100422     C                   MOVEL     'S'           VC(821)
065700020620     C                   ENDIF
065800020620E   3C                   ENDIF
065900020620IF  3C     D$DTSP        IFEQ      'C'
066000020620     C     WCLV          IFEQ      'A'
066100100421     C                   MOVEL     'S'           VC(810)
066200020620     C                   ENDIF
066300020620     C     WCLV          IFEQ      'B'
066400100421     C                   MOVEL     'S'           VC(811)
066500020620     C                   ENDIF
066600020620     C     WCLV          IFEQ      'C'
066700100421     C                   MOVEL     'S'           VC(812)
066800020620     C                   ENDIF
066900020620     C     WCLV          IFEQ      'T'
067000020620     C     WCLV          OREQ      'D'
067100100421     C                   MOVEL     'S'           VC(819)
067200020620     C                   ENDIF
067300020620E   3C                   ENDIF
067400020620IF  3C     D$DTSP        IFEQ      'E'
067500100107IF  3C     D$DTSP        OREQ      'H'
067600020620     C     WCLV          IFEQ      'A'
067700100421     C                   MOVEL     'S'           VC(813)
067800020620     C                   ENDIF
067900020620     C     WCLV          IFEQ      'B'
068000100421     C                   MOVEL     'S'           VC(814)
068100020620     C                   ENDIF
068200020620     C     WCLV          IFEQ      'C'
068300100421     C                   MOVEL     'S'           VC(815)
068400020620     C                   ENDIF
068500020620     C     WCLV          IFEQ      'T'
068600020620     C     WCLV          OREQ      'D'
068700100421     C                   MOVEL     'S'           VC(820)
068800020620     C                   ENDIF
068900020620E   3C                   ENDIF
069000020620E   2C                   ENDIF
069100020620E   1C                   ENDIF
069200020621     C*
069300020621E  1ac                   ENDIF
069400030630     C*
069500030630     C* =============> VOCI CHE VANNO APPLICATE SOLO SE LA BOLLA
069600030630     C*    NON E' UNA BOLLA DI RECUPERO (A3) E
069700030630     C*    NON E' UN  RECAPITO C/A MITTENTE (FW)
069800030604IF 1aC                   IF        D$DTBL <> 'A3' AND
069900030604     C                             D$DCBO <> 'FW'
070000030630     C*
070100020620     C* =======>
070200940526     C* IMPOSTA A SI TUTTE LE VOCI SEMPRE PRESENTI
070300940603     C                   MOVEL     'S'           VC(10)
070400940603     C                   MOVEL     'S'           VC(50)
070500940603     C                   MOVEL     'S'           VC(60)
070600940727     C                   MOVEL     'S'           VC(70)
070700940727     C                   MOVEL     'S'           VC(80)
070800940727     C                   MOVEL     'S'           VC(81)
070900940603     C                   MOVEL     'S'           VC(110)
071000940603     C                   MOVEL     'S'           VC(120)
071100940603     C                   MOVEL     'S'           VC(130)
071200940727     C                   MOVEL     'S'           VC(180)
071300940727     C                   MOVEL     'S'           VC(190)
071400940727     C                   MOVEL     'S'           VC(191)
071500960131     C                   MOVEL     'S'           VC(192)
071600061207     C                   MOVEL     'S'           VC(193)
071700011203     C                   MOVEL     'S'           VC(194)
071800940728     C                   MOVEL     'S'           VC(300)
071900980716     C                   MOVEL     'S'           VC(320)
072000991116     C                   MOVEL     'S'           VC(321)
072100991116     C                   MOVEL     'S'           VC(322)
072200090902IF  0C     WDDSP         ifgt      20090900
072300090923     C                   MOVEL     'S'           VC(112)
072400090923     C                   MOVEL     'S'           VC(122)
072500090923     C                   MOVEL     'S'           VC(137)
072600090902     c                   end
072700050926     c* merce di valore
072800050926IF  1C     §FLOTBA       IFEQ      'V'
072900050926IF  0C     WDDSP         ANDGT     20051000
073000050926     C                   MOVEL     'S'           VC(19)
073100050926     C                   MOVEL     'S'           VC(21)
073200050926     c                   end
073300021028     c*
073400010524     C* =============> COMPETENZA SEDE -CLIENTE CODIFICATO-
073500010524     C*    SE NON E' CLIENTE 8888 O 9999
073600991116     C                   MOVE      W$KSC         KSCIS             4 0
073700010524IF  1C     KSCIS         IFNE      8888
073800010524     C     KSCIS         ANDNE     9999
073900000301IF  2C     D$DTSP        IFEQ      'E'
074000100107IF  2C     D$DTSP        OREQ      'H'
074100001201     C                   MOVEL     'S'           VC(533)
074200001206E   2C                   ENDIF
074300010524E   1C                   ENDIF
074400020503     C*
074500020503     C                   Z-ADD     W$LNP         F
074600111011     C                   Z-ADD     W$lna         F1
074700110930      * ADEGUAMENTO "MOMENTANEO" TARIFFE EEX
074800110930      * partenza euroexpress, FRANCO
074900110930     c     NTW(F1)       IFEQ      'EEX'
075000110930     C     D$DTPO        andeq     'F'
075100110930     C     WDDSP         andgt     20110900
075200110930     C                   MOVEL     'S'           VC(549)
075300110930     C                   ENDIF
075400110930      * partenza euroexpress, ASSEGNATO
075500110930     c     NTW(F)        IFEQ      'EEX'
075600110930     C     D$DTPO        andeq     'A'
075700110930     C     WDDSP         andgt     20110900
075800110930     C                   MOVEL     'S'           VC(550)
075900110930     C                   ENDIF
076000110930     C*
076100110930     C* CONTROLLO SE ESISTE =======> ASSISTENZA CLIENTI FEDEX
076200110930     C*    IMPORT o EXPORT FEDEX
076300020503IF  1C     NTW(F)        IFEQ      'FED'
076400020503     C     NTW(F1)       OREQ      'FED'
076500020503     C                   MOVEL     'S'           VC(284)
076600020503E   1C                   ENDIF
076700050329     C* CONTROLLO SE ESISTE =======> ETICHETTATURA EXPORT DPD
076800050329     C                   IF        NTW(F1) = 'DPD' and
076900050329     C                             WDDSP > 20050400
077000050329     C                   MOVEL     'S'           VC(45)
077100050329E   1C                   ENDIF
077200081029      * arrivo euroexpress, franco, consegna loc.disagiate
077300081027     c     NTW(F1)       IFEQ      'EEX'
077400081027     C     D$DTPO        andeq     'F'
077500081027     C     D$DFIN        ANDEQ     'D'
077600081027     C     WDDSP         andgt     20081026
077700081027     C                   MOVEL     'S'           VC(340)
077800081027     C                   ENDIF
077900081029      * partenza euroexpress, assegnato, ritiro loc.disagiate
078000081027     c     NTW(F)        IFEQ      'EEX'
078100081027     C     D$DTPO        andeq     'A'
078200081027     C     D$DFAP        ANDEQ     'D'
078300081027     C     WDDSP         andgt     20081026
078400081027     C                   MOVEL     'S'           VC(341)
078500081027     C                   ENDIF
078600000301     C*
078700000301     C* CONTROLLO SE ESISTE =======> ESTERO - IMPORT
078800950207     C*    LINEA PARTENZA > 300
078900971211     C                   Z-ADD     W$LNP         F
079000020805     C     FIE(F)        IFEQ      'E'
079100020805     C     NTW(F)        IFEQ      'DPD'
079200000202     C                   MOVEL     'S'           VC(183)
079300010726     C                   MOVEL     'S'           VC(283)
079400000202     C                   ELSE
079500000202     C                   MOVEL     'S'           VC(181)
079600020503     C     NTW(F)        IFNE      'FED'
079700010726     C                   MOVEL     'S'           VC(282)
079800000202     C                   ENDIF
079900020503     C                   ENDIF
080000010116     C                   MOVEL     'S'           VC(542)
080100000202     C                   MOVEL     'S'           VC(41)
080200950208     C     D$DTPO        IFEQ      'A'
080300020805     C     NTW(F)        IFEQ      'DPD'
080400000202     C                   MOVEL     'S'           VC(13)
080500000202     C                   MOVEL     'S'           VC(222)
080600000202     C                   ELSE
080700020503     C     NTW(F)        IFNE      'FED'
080800000202     C                   MOVEL     'S'           VC(11)
080900950207     C                   MOVEL     'S'           VC(12)
081000130529     C                   IF        WDDSP > 20130600
081100130529     C                   MOVEL     'S'           VC(14)
081200090122     c                   end
081201130529     C                   IF        WDDSP > 20090100
081202130529     C                   MOVEL     'S'           VC(48)
081203130529     c                   end
081300051111     C                   IF        WDDSP > 20051120
081400051111     C                   MOVEL     'S'           VC(35)
081500051111     C                   MOVEL     'S'           VC(37)
081600051111     C                   MOVEL     'S'           VC(38)
081700070521     c                   end
081800070521     C                   IF        WDDSP > 20070100
081900070521     C                   MOVEL     'S'           VC(47)
082000070521     C                   MOVEL     'S'           VC(552)
082100070521     c                   end
082200040526     c* 546 MAGG.AUTOSTRADA AUSTRIA PARTENZA
082300040526     C                   IF        WDDSP > 20040100
082400040526     C                   MOVEL     'S'           VC(546)
082500040526     C                   ENDIF
082600040526     c* 544 MAGGIORAZ.RITIRO ISOLE GERM E RITIRO ISOLA
082700040526     C                   IF        WDDSP > 20040100 and
082800040526     C                             D$DFAP = 'I'
082900040526     C                   MOVEL     'S'           VC(544)
083000040526     C                   ENDIF
083100020503     C                   ENDIF
083200000202     C                   MOVEL     'S'           VC(221)
083300000202     C                   ENDIF
083400000202     C                   ENDIF
083500000202     C                   ENDIF
083600950207     C*
083700950207     C* CONTROLLO SE ESISTE =======> ESTERO - EXPORT
083800950207     C*    LINEA ARRIVO   > 300
083900111011     C                   Z-ADD     W$lna         F
084000020805     C     FIE(F)        IFEQ      'E'
084100040429      *
084200040429      * per euroexpress e olo franco e con giacenza
084300040429     c     NTW(F)        IFEQ      'EEX'
084400040429     C     D$DFGC        andeq     'S'
084500040429     C     D$DTPO        andeq     'F'
084600040429     C                   MOVEL     'S'           VC(590)
084700070521     C                   IF        WDDSP > 20070100
084800070521     C                   MOVEL     'S'           VC(268)
084900070521     C                   MOVEL     'S'           VC(553)
085000040429     C                   ENDIF
085100070521     C                   ENDIF
085200040429      *
085300020805     C     NTW(F)        IFEQ      'DPD'
085400000202     C                   MOVEL     'S'           VC(74)
085500000202     C                   MOVEL     'S'           VC(276)
085600000202     C                   ELSE
085700000202     C                   MOVEL     'S'           VC(71)
085800020503     C     NTW(F)        IFNE      'FED'
085900000202     C                   MOVEL     'S'           VC(275)
086000020503     C                   MOVEL     'S'           VC(555)
086100030527     C                   MOVEL     'S'           VC(556)
086200040429     C                   ENDIF
086300000202     C                   ENDIF
086400010116     C                   MOVEL     'S'           VC(543)
086500020503     C     NTW(F)        IFEQ      'FED'
086600040427     C                   MOVEL     'S'           VC(591)
086700050302     C* sdeguamento tarif. 2005 x fedex da febbraio 2005
086800091104IF  2C                   IF        WDDSP > 20050200 and wddsp < 20100100
086900050302     C                   MOVEL     'S'           VC(597)
087000050302     c                   end
087100030204IF  2C                   IF        (WDDSP < 20030000) OR
087200030204     C                             (WDDSP > 20030000 AND D$DCTR >= 300 AND
087300030204     C                              D$DCTR<=349)
087400020503     C                   MOVEL     'S'           VC(168)
087500020503     C                   MOVEL     'S'           VC(169)
087600030204E   2C                   ENDIF
087700030204IF  2C                   IF        WDDSP > 20030000 AND
087800030204     C                             D$DCTR >= 350 AND D$DCTR <=399
087900030204     C                   MOVEL     'S'           VC(167)
088000040903     c* le devo attivare comunque tutte e 3 x reperire le loro informazioni
088100040903     c* se per la voce 167 ho manca tariffa xchè peso tassabile > 2,5 kg
088200040903     c* disattivo quest'ultima e lascio la 168 e 169, viceversa, se non ho
088300040903     c* il manca tariffa sulla voce 167 disattivo la 168 e 169
088400040903     C                   MOVEL     'S'           VC(168)
088500040903     C                   MOVEL     'S'           VC(169)
088600030204E   2C                   ENDIF
088700041103     c* per FED se cod. tariffa è diverso dal range 300 - 399
088800041103     c* applico comunque la voce 168 e 169
088900041103IF  2C                   IF        WDDSP > 20040000 AND
089000041103     C                             (D$DCTR < 300 or D$DCTR >399)
089100041103     C                   MOVEL     'S'           VC(168)
089200041103     C                   MOVEL     'S'           VC(169)
089300041103E   2C                   ENDIF
089400020503     C                   ENDIF
089500020503     C*
089600980317     C     D$DTPO        IFEQ      'A'
089700980317     C                   MOVEL     'S'           VC(562)
089800020805     C     NTW(F)        IFEQ      'DPD'
089900000202     C                   MOVEL     'S'           VC(73)
090000000202     C                   ENDIF
090100000202     C                   ENDIF
090200950207     C     D$DTPO        IFEQ      'F'
090300020805     C     NTW(F)        IFEQ      'DPD'
090400000202     C                   MOVEL     'S'           VC(175)
090500000202     C                   ELSE
090600020503     C     NTW(F)        IFNE      'FED'
090700090122IF  5C                   IF        WDDSP > 20090100
090800090122     C                   MOVEL     'S'           VC(332)
090900090122     c                   end
091000091019IF  5C                   IF        WDDSP > 20030000
091100050504     c                   movel     d$dcts        wdcts             1
091200030429IF  6C     D$DNCL        IFEQ      1
091300040427     C     wdpes         ANDLE     30
091400070523     C     wdcts         ANDEQ     'D'
091500030429     C     D$DCAS        ANDEQ     *ZEROS
091600070523IF  6C     D$DNCL        orEQ      1
091700070523     C     wdpes         ANDLE     30
091800070523     C     d$dcts        ANDEQ     'W4'
091900070523     C     D$DCAS        ANDEQ     *ZEROS
092000030429     C                   MOVEL     'S'           VC(166)
092100040526     c* 170 MONOCOLLO GERM FINO 30 KG ISOLE E CONSEGNA ISOLE
092200040526     C                   IF        WDDSP > 20040100 and
092300040526     C                             D$DFIN = 'I'
092400040526     C                   MOVEL     'S'           VC(170)
092500040526     C                   ENDIF
092600030429X   6C                   ELSE
092700030429     C                   MOVEL     'S'           VC(171)
092800030429     C                   MOVEL     'S'           VC(172)
092801130529IF  5C                   IF        WDDSP > 20130600
092802130529     C                   MOVEL     'S'           VC(173)
092803130529     c                   end
092900051104IF  5C                   IF        WDDSP > 20051106
093000051104     C                   MOVEL     'S'           VC(323)
093100051104     C                   MOVEL     'S'           VC(327)
093200051104     C                   MOVEL     'S'           VC(328)
093300051104     c                   end
093400070521IF  5C                   IF        WDDSP > 20070100
093500070521     C                   MOVEL     'S'           VC(331)
093600070521     C                   MOVEL     'S'           VC(552)
093700070521     c                   end
093800040526     c* 547 MAGG.AUTOSTRADA AUSTRIA ARRIVO
093900040526     C                   IF        WDDSP > 20040100
094000040526     C                   MOVEL     'S'           VC(547)
094100040526     C                   ENDIF
094200040526     c* 545 MAGGIORAZ.CONSEGNE ISOLE GERM E CONSEGNA ISOLE
094300040526     C                   IF        WDDSP > 20040100 and
094400040526     C                             D$DFIN = 'I'
094500040526     C                   MOVEL     'S'           VC(545)
094600040526     C                   ENDIF
094700030429E   6C                   ENDIF
094800030429     C
094900030429E   5C                   ENDIF
095000030429E   4C                   ENDIF
095100030429E   3C                   ENDIF
095200030429E   2C                   ENDIF
095300030429E   1C                   ENDIF
095400940526     C*
095500940526     C* CONTROLLO SE ESISTE =======> AFFLUENZA
095600940526     C*    LINEA PARTENZA DIVERSA DAL TERMINAL PARTENZA
095700940829     C     W$LNP         IFNE      W$TFP
095800971211     C                   Z-ADD     W$LNP         F
095900020805     C     FIE(F)        IFEQ      'E'
096000950207     C                   MOVEL     'S'           VC(32)
096100950207     C                   MOVEL     'S'           VC(52)
096200950207     C                   MOVEL     'S'           VC(64)
096300950207     C                   ELSE
096400980119     C                   MOVEL     'S'           VC(220)
096500940727     C                   MOVEL     'S'           VC(31)
096600940727     C                   MOVEL     'S'           VC(51)
096700940727     C                   MOVEL     'S'           VC(61)
096800050926     c* merce di valore
096900050926IF  1C     §FLOTBA       IFEQ      'V'
097000050926IF  0C     WDDSP         ANDGT     20051000
097100050926     C                   MOVEL     'S'           VC(20)
097200050926     c                   end
097300050926     c*
097400950207     C                   ENDIF
097500940729     C                   ENDIF
097600940526     C*
097700940526     C* CONTROLLO SE ESISTE =======> DEFLUENZA
097800940526     C*    LINEA ARRIVO DIVERSA DAL TERMINAL ARRIVO
097900111011IF  1C     W$lna         IFNE      D$DTFA
098000111011     C                   Z-ADD     W$lna         F
098100020805IF  2C     FIE(F)        IFEQ      'E'
098200980401     C                   MOVEL     'S'           VC(114)
098300980401     C                   MOVEL     'S'           VC(124)
098400980401     C                   MOVEL     'S'           VC(152)
098500020805IF  3C     NTW(F)        IFEQ      'DPD'
098600000204IF  4C     D$DTPO        IFEQ      'F'
098700000202     C                   MOVEL     'S'           VC(233)
098800000204E   4C                   ENDIF
098900000204X   3C                   ELSE
099000001201     C                   MOVEL     'S'           VC(231)
099100001201E   3C                   ENDIF
099200980402X   2C                   ELSE
099300950703///  C                   MOVEL     'S'           VC(230)
099400940727     C                   MOVEL     'S'           VC(111)
099500940727     C                   MOVEL     'S'           VC(121)
099600940727     C                   MOVEL     'S'           VC(151)
099700110905IF  0C     WDDSP         ifge      20110829
099800110905     C                   MOVEL     'S'           VC(548)
099900110905     c                   end
100000090902IF  0C     WDDSP         ifgt      20090900
100100090923     C                   MOVEL     'S'           VC(113)
100200090923     C                   MOVEL     'S'           VC(125)
100300090923     C                   MOVEL     'S'           VC(154)
100400090910     c                   end
100500050926     c* merce di valore
100600050926IF  1C     §FLOTBA       IFEQ      'V'
100700050926IF  0C     WDDSP         ANDGT     20051000
100800050926     C                   MOVEL     'S'           VC(22)
100900050926     c                   end
101000980402E   2C                   ENDIF
101100980402E   1C                   ENDIF
101200940526     C*
101300940727     C* CONTROLLO SE ESISTE =======> TRANSITO 1
101400940727     C*    TRANSITO 1 DIVERSO DA ZERO
101500040520IF  1C     D$Dar5flp     IFNE      *ZEROS
101600001201     C                   MOVEL     'S'           VC(67)
101700001201     C                   MOVEL     'S'           VC(117)
101800001201     C                   MOVEL     'S'           VC(127)
101900001201     C                   MOVEL     'N'           VS(67)
102000001201     C                   MOVEL     'N'           VS(117)
102100001201     C                   MOVEL     'N'           VS(127)
102200050926     c* merce di valore
102300050926IF  1C     §FLOTBA       IFEQ      'V'
102400050926IF  0C     WDDSP         ANDGT     20051000
102500050926     C                   MOVEL     'S'           VC(24)
102600050926     c                   end
102700001201E   1C                   ENDIF
102800940727     C*
102900940727     C* CONTROLLO SE ESISTE =======> TRANSITO 2
103000940727     C*    TRANSITO 2 DIVERSO DA ZERO
103100040520     C     D$Dar5fl2     IFNE      *ZEROS
103200001201     C                   MOVEL     'S'           VC(68)
103300001201     C                   MOVEL     'S'           VC(118)
103400001201     C                   MOVEL     'S'           VC(128)
103500001201     C                   MOVEL     'N'           VS(68)
103600001201     C                   MOVEL     'N'           VS(118)
103700001201     C                   MOVEL     'N'           VS(128)
103800050926     c* merce di valore
103900050926IF  1C     §FLOTBA       IFEQ      'V'
104000050926IF  0C     WDDSP         ANDGT     20051000
104100050926     C                   MOVEL     'S'           VC(25)
104200050926     c                   end
104300940727     C                   ENDIF
104400040520     C*
104500040520     C* CONTROLLO SE ESISTE =======> TRANSITO 3
104600040520     C*    TRANSITO 3 DIVERSO DA ZERO
104700040520     C     D$Dar5fl3     IFNE      *ZEROS
104800040520     C                   MOVEL     'S'           VC(77)
104900040520     C                   MOVEL     'S'           VC(131)
105000040520     C                   MOVEL     'S'           VC(134)
105100040520     C                   MOVEL     'N'           VS(77)
105200040520     C                   MOVEL     'N'           VS(131)
105300040520     C                   MOVEL     'N'           VS(134)
105400050926     c* merce di valore
105500050926IF  1C     §FLOTBA       IFEQ      'V'
105600050926IF  0C     WDDSP         ANDGT     20051000
105700050926     C                   MOVEL     'S'           VC(26)
105800050926     c                   end
105900040520     C                   ENDIF
106000040520     C*
106100040520     C* CONTROLLO SE ESISTE =======> TRANSITO 4
106200040520     C*    TRANSITO 4 DIVERSO DA ZERO
106300040520     C     D$Dar5fl4     IFNE      *ZEROS
106400040520     C                   MOVEL     'S'           VC(78)
106500040520     C                   MOVEL     'S'           VC(132)
106600040520     C                   MOVEL     'S'           VC(135)
106700040520     C                   MOVEL     'N'           VS(78)
106800040520     C                   MOVEL     'N'           VS(132)
106900040520     C                   MOVEL     'N'           VS(135)
107000050926     c* merce di valore
107100050926IF  1C     §FLOTBA       IFEQ      'V'
107200050926IF  0C     WDDSP         ANDGT     20051000
107300050926     C                   MOVEL     'S'           VC(27)
107400050926     c                   end
107500040520     C                   ENDIF
107600040520     C*
107700040520     C* CONTROLLO SE ESISTE =======> TRANSITO 5
107800040520     C*    TRANSITO 5 DIVERSO DA ZERO
107900040520     C     D$Dar5fl5     IFNE      *ZEROS
108000040520     C                   MOVEL     'S'           VC(79)
108100040520     C                   MOVEL     'S'           VC(133)
108200040520     C                   MOVEL     'S'           VC(136)
108300040520     C                   MOVEL     'N'           VS(79)
108400040520     C                   MOVEL     'N'           VS(133)
108500040520     C                   MOVEL     'N'           VS(136)
108600050926     c* merce di valore
108700050926IF  1C     §FLOTBA       IFEQ      'V'
108800050926IF  0C     WDDSP         ANDGT     20051000
108900050926     C                   MOVEL     'S'           VC(28)
109000050926     c                   end
109100040520     C                   ENDIF
109200940722     C*
109300940722     C* CONTROLLO SE ESISTE =======> LOCALI
109400980317     C*    LINEA PART. = LINEA ARR. = TERM. PART. = TERM. ARR.
109500111011     C     W$LNP         IFEQ      W$lna
109600980317     C     W$LNP         ANDEQ     D$DTFA
109700980318     C     W$LNP         ANDEQ     W$TFP
109800940728     C                   MOVEL     'S'           VC(500)
109900940722     C                   ENDIF
110000940728     C*
110100950626     C* CONTROLLO SE ESISTE =======> DISK 'A' E DISK 'B'
110200950626     C*    SE SIGLA OPERATORE = '§' O '%' E' UN DISK 'A' O DISK 'B'
110300950626     C     D$DSOP        IFEQ      '§'
110400950626     C     D$DSOP        OREQ      '%'
110500000210     C     D$DSOP        OREQ      ':'
110600950626     C*    SE N.SERIE > ZERO E' UN DISK 'B' ALTRIMENTI E' UN DISK 'A'
110700950626     C     D$DNRS        IFGT      *ZEROS
110800950626     C                   MOVEL     'S'           VC(510)
110900950626     C                   MOVEL     'S'           VC(511)
111000990223     C                   MOVEL     'S'           VC(55)
111100950626     C                   ELSE
111200950626     C                   MOVEL     'S'           VC(511)
111300940728     C                   ENDIF
111400950626     C                   ENDIF
111500021029     C*
111600021029     C* REPERISCE I DATI DEL CODICE TRATTAMENTO MERCE =======>
111700021029     C                   CLEAR                   WCF12
111800050801     C                   CLEAR                   WCF16
111900030530     C                   CLEAR                   WCDKC
112000021029IF  1C     D$DCTM        IFNE      *BLANKS
112100021029     C                   Z-ADD     1             I
112200021029     C     D$DCTM        LOOKUP    CTM(I)                                 98
112300021029IF  2C     *IN98         IFEQ      *OFF
112400021029     C                   MOVEL     MSG(27)       $MSG
112500021029     C                   EXSR      SCRERR
112600021029X   2C                   ELSE
112700021029     C                   MOVEL     CF12(I)       WCF12
112800050801     C                   MOVEL     CF16(I)       WCF16
112900030530     C                   MOVEL     CDKC(I)       WCDKC
113000021029E   2C                   ENDIF
113100021029E   1C                   ENDIF
113200010420     C*
113300021029     C* CONTROLLA SE ESISTE =======> DISCK 'C'
113400030530IF  1C     WCDKC         IFNE      *BLANKS
113500010420     C                   MOVEL     'S'           VC(56)
113600030530E   1C                   ENDIF
113700050801     C*
113800050801     C* CONTROLLA SE ESISTE =======> packing list
113900050801IF  1C     D$DCTM        IFNE      *BLANKS
114000050801IF  2C     WCF16         andeq     'S'
114100050801IF  2C     WDDSP         andgt     20050900
114200050801     C                   MOVEL     'S'           VC(189)
114300050801E   1C                   ENDIF
114400021104     c*
114500021104     C* =============> ETICHETTATURA, SCARICO, PICKING E CARICO
114600021104     C*    PRIMA del 2003, SEMPRE
114700021104IF  2C     WDDSP         IFLT      20030000
114800021104     C                   MOVEL     'S'           VC(30)
114900021104     C                   MOVEL     'S'           VC(40)
115000021104     C                   MOVEL     'S'           VC(140)
115100021104     C                   MOVEL     'S'           VC(150)
115200021104E   2C                   ENDIF
115300021029     c*
115400021104     C* =============> ETICHETTATURA e SCARICO e PICKING e CARICO, DOPO il 2002
115500021029     C*    DELL' AUTOTRASPORTATORE (flag =" ") OPPURE
115600021029     C*    DELLA COOPERATIVA       (flag ="C")
115700021104IF  0C     WDDSP         IFGT      20030000
115800021029IF  1C     §FLOSCA       IFEQ      *BLANKS
115900021029     C                   MOVEL     'S'           VC(30)
116000021212     C                   MOVEL     'S'           VC(57)
116100021029X   1C                   ELSE
116200021029     C                   MOVEL     'S'           VC(33)
116300021029E   1C                   ENDIF
116400040520     C* BOLLA CON IMMAGINE APPLICO LA VOCE 188
116500040520IF  1C     §FLOIML       IFNE      *BLANKS
116600040520IF  0C     WDDSP         ANDGT     20040700
116700040520     C                   MOVEL     'S'           VC(188)
116800040520E   1C                   ENDIF
116900021029     C* NO disck C
117000021029IF  1C     WCF12         IFEQ      'N'
117100021029IF  2C     §FLOETI       IFEQ      *BLANKS
117200021029     C                   MOVEL     'S'           VC(40)
117300021029X   2C                   ELSE
117400021029     C                   MOVEL     'S'           VC(42)
117500021029E   2C                   ENDIF
117600021029E   1C                   ENDIF
117700030918     C* disck C NON gestito dal cliente
117800030530IF  1C     WCDKC         IFEQ      'B'
117900030919IF  2C     WDDSP         IFLT      20030922
118000030919     C                   MOVEL     'S'           VC(43)
118100030919X   2c                   ELSE
118200030919IF  3C     D$DFDN        IFEQ      'S'
118300030918     C                   MOVEL     'S'           VC(44)
118400030919X   3c                   ELSE
118500021029     C                   MOVEL     'S'           VC(43)
118600030919E   3C                   ENDIF
118700030919E   2C                   ENDIF
118800030530E   1C                   ENDIF
118900021029     c*
119000021029IF  1C     §FLOPIC       IFEQ      *BLANKS
119100021029     C                   MOVEL     'S'           VC(140)
119200021029X   1C                   ELSE
119300021029     C                   MOVEL     'S'           VC(141)
119400021029E   1C                   ENDIF
119500021029IF  1C     §FLOCAR       IFEQ      *BLANKS
119600021029     C                   MOVEL     'S'           VC(150)
119700021029X   1C                   ELSE
119800021029     C                   MOVEL     'S'           VC(153)
119900021029E   1C                   ENDIF
120000021104E   0C                   ENDIF
120100940526     C*
120200940526     C* CONTROLLO SE ESISTE =======> GIACENZA
120300940526     C*    FLAG GIACENZA UGUALE A SI
120400940526     C     D$DFGC        IFEQ      'S'
120500940728     C                   MOVEL     'S'           VC(260)
120600940728     C                   MOVEL     'S'           VC(261)
120700940728     C                   MOVEL     'S'           VC(262)
120800040427     C     wdpes         IFLE      100
120900000201     C                   MOVEL     'S'           VC(267)
121000000201     C                   ELSE
121100000201     C                   MOVEL     'S'           VC(265)
121200000201     C                   ENDIF
121300940728     C*    SE NON E' DIROTTATA E RESO MITT.
121400940728     C     D$DCCA        IFNE      '1'
121500940728     C     D$DCCA        ANDNE     '2'
121600960329     C     D$DCCA        ANDNE     '6'
121700940728     C                   MOVEL     'S'           VC(263)
121800940526     C                   ENDIF
121900940728     C                   ENDIF
122000940526     C*
122100940728     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
122200000411     C*    FLAG FERMO DEPOSITO = "S" OPPURE N.STOP CONSEGNA = 99999
122300990420     C*    OPPURE INOLTRO = I (CONSEGNA ISOLA)
122400940728     C     D$DFFD        IFEQ      'S'
122500000411     C     D$DSTP        OREQ      99999
122600990420     C     D$DFIN        OREQ      'I'
122700010116     C                   MOVEL     'S'           VC(532)
122800001206     C                   ENDIF
122900011203     C*
123000011203     C* CONTROLLO SE ESISTE =======> FERMO DEPOSITO
123100011203IF  1C     D$DFFD        IFEQ      'S'
123200011203     C                   MOVEL     'S'           VC(184)
123300011203E   2C                   ENDIF
123400940714     C*
123500940714     C* CONTROLLO SE ESISTE =======> ASSICURAZIONE PER CONTO
123600941201     C*    SE HA IMPORTO VALORE ASSICURAZIONE X CONTO > ZERO
123700970225     C                   MOVE      W$KSC         KSCIS             4 0
1238009702251    C     KSCIS         IFEQ      8888
123900970225     C     KSCIS         OREQ      9999
1240000705212    C     D$DVAE        ifgt      *ZEROS
124100970225     C                   MOVEL     'S'           VC(291)
124200070521     c                   end
1243009702251    C                   ELSE
1244000112272    C     D$DVAE        IFGT      *ZEROS
124500940728     C                   MOVEL     'S'           VC(290)
124600051017IF  2C     WDDSP         IFGT      20050800
124700051017     C                   MOVEL     'S'           VC(292)
124800051017     c                   end
1249009702252    C                   ELSE
125000100224     C* attivo nuova voce 286 da febbraio 2010 solo per
125100100224     C* FRANCHI EXPORT O ASSEGNATI IMPORT, la voce 281 da febbraio diventa
125200100224     C* solo per Italia
125300100224     C                   Z-ADD     W$LNP         F
125400111011     C                   Z-ADD     W$lna         F1
125500100224     c                   if        WDDSP > 20100200  and
125600100224IF  1C                             ((fie(f) = 'E' and D$DTPO ='A') or
125700100224IF  1C                             (fie(f1) = 'E' and D$DTPO ='F'))
125800100224     C                   MOVEL     'S'           VC(286)
125900100224     c                   else
126000100224     C                   MOVEL     'S'           VC(281)
126100100224     c                   end
126200060313     c* AC BASE
126300060313     c                   if        *in44 and WDDSP > 20060300
126400060313     c* cliente top o direzionale
126500060313     c                   select
126600060313     c                   when      wclv = 'T' or wclv = 'D'
126700060313     C                   MOVEL     'S'           VC(293)
126800060313     c* cliente A
126900060313     c                   when      wclv = 'A'
127000060313     C                   MOVEL     'S'           VC(294)
127100060313     c* cliente B o C
127200060313     c                   when      wclv = 'B' or wclv = 'C'
127300060313     C                   MOVEL     'S'           VC(295)
127400060313     C                   endsl
127500060313     C                   endif
1276009702252    C                   ENDIF
1277009702251    C                   ENDIF
127800940722     C*
127900940722     C* CONTROLLO SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
128000970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "A"
128100940722     C     D$DFTC        IFEQ      'A'
128200970228     C     D$DTC2        OREQ      'A'
128300011203IF  3C     D$DFTC        IFEQ      'S'
128400011203     C     D$DTC2        OREQ      'S'
128500011203X   3C                   ELSE
128600011203     C                   MOVEL     'S'           VC(240)
128700011203E   3C                   ENDIF
128800011203     C                   ENDIF
128900940722     C*
129000940722     C* CONTROLLO SE ESISTE =======> CONSEGNA AI SUPERMERCATI
129100970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "S"
129200940722     C     D$DFTC        IFEQ      'S'
129300970228     C     D$DTC2        OREQ      'S'
129400980119     C                   MOVEL     'S'           VC(242)
129500980119     C                   MOVEL     'S'           VC(247)
129600940722     C                   ENDIF
129700940722     C*
129800940722     C* CONTROLLO SE ESISTE =======> CONSEGNA AI PIANI
129900970228     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "P"
130000940722     C     D$DFTC        IFEQ      'P'
130100970228     C     D$DTC2        OREQ      'P'
130200940727     C                   MOVEL     'S'           VC(241)
130300940722     C                   ENDIF
130400020620     C*
130500020620     C* CONTROLLO SE ESISTE =======> CONSEGNA DISAGIATA
130600020620     C*    CODICE CONSEGNA PARTICOLARE 1 E 2 = "X"
130700020620     C     D$DFTC        IFEQ      'X'
130800020620     C     D$DTC2        OREQ      'X'
130900020620     C                   MOVEL     'S'           VC(534)
131000020620     C                   MOVEL     'S'           VC(535)
131100020620     C                   ENDIF
131200940722     C*
131300940722     C* CONTROLLO SE ESISTE =======> CONTRASSEGNO
131400940722     C*    IMPORTO CONTRASSEGNO > ZERO
131500941201     C     D$DCAS        IFGT      *ZEROS
131600111011     C                   Z-ADD     W$lna         F
131700060223     c* arrivi
131800020805     C     FIE(F)        IFEQ      'E'
131900000203     C                   MOVEL     'S'           VC(256)
132000000203     C                   MOVEL     'S'           VC(257)
132100000203     C                   MOVEL     'S'           VC(280)
132200000203     C                   ELSE
132300940727     C                   MOVEL     'S'           VC(251)
132400940727     C                   MOVEL     'S'           VC(252)
132500000203     C                   ENDIF
132600060223     c* partenza
132700060223     c**** mittente
132800941201     C     D$DTPI        IFEQ      'M'
132900060223     c******* non clienti informatizzati
133000090417     c                   if        wddsp < 20090500
133100060223     C                   IF        (d$dsop <> '§' and
133200060223     C                             d$dsop <> '%' and
133300060223     C                             d$dsop <> ':' and wddsp > 20060200) or
133400060223IF  2C                             wddsp < 20060200
133500970225     C                   MOVEL     'S'           VC(255)
133600060223     c                   end
133700090417     c                   end
133800060223     c******* clienti informatizzati
133900090417     c                   if        wddsp < 20090500
134000060223     C                   IF        (d$dsop = '§' or
134100060223     C                             d$dsop = '%' or
134200060223     C                             d$dsop = ':' ) and wddsp > 20060200
134300060223     C                   MOVEL     'S'           VC(258)
134400060223     c                   end
134500090417     c                   end
134600060223     C                   MOVEL     'S'           VC(254)
134700941021     C                   ELSE
134800060223     c**** non mittente
134900060223     c******* non clienti informatizzati
135000090417     c                   if        wddsp < 20090500
135100060223     C                   IF        (d$dsop <> '§' and
135200060223     C                             d$dsop <> '%' and
135300060223     C                             d$dsop <> ':' and wddsp > 20060200) or
135400060223IF  2C                             wddsp < 20060200
135500060223     C                   MOVEL     'S'           VC(250)
135600060223     c                   end
135700090417     c                   end
135800941021     C                   MOVEL     'S'           VC(253)
135900941021     C                   ENDIF
136000090417     c* cambia criterio di attivazione per la voce 250
136100090417     C                   IF        (d$dsop <> '§' and
136200090417     C                             d$dsop <> '%' and
136300090417     C                             d$dsop <> ':' and wddsp > 20090500)
136400090417     C                   MOVEL     'S'           VC(250)
136500090417     c                   end
136600940722     C                   ENDIF
136700940728     C*
136800940728     C* CONTROLLO SE ESISTE =======> PORTO ASSEGNATO UFF.ARR. E PADRONCINO
136900940728     C*    ASSEGNATO CON FATTURA IMMEDIATA
137000941201     C     D$DTPO        IFEQ      'A'
137100030529     C     D$DFIV        ANDGT     0
137200030529     C     D$DFIV        ANDLT     900
137300940728     C                   MOVEL     'S'           VC(310)
137400940728     C                   MOVEL     'S'           VC(311)
137500940728     C                   ENDIF
137600941028     C*
137700990420     C* CONTROLLO SE ESISTE =======> CONSEGNA A MAGAZZINO (RIDUZIONE)
137800941028     C*    FLAG MERCE PORTATA A MAGAZZINO DAL CLIENTE = 'SI'
137900990420     C*    OPPURE ANTEPORTO = I (RITIRO ISOLA)
138000941209     C     D$DFDN        IFEQ      'S'
138100990420     C     D$DFAP        OREQ      'I'
138200010116     C                   MOVEL     'S'           VC(521)
138300001206     C                   ENDIF
138400030422     C*
138500030422     C* CONTROLLO SE ESISTE =======> GESTIONE BANCALI
138600030422     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'B'
138700091019     C                   MOVEL     D$DGVA        WTM01             1
138800030422     C     WTM01         IFEQ      'B'
138900030422     C                   MOVEL     'S'           VC(17)
139000030422     C                   MOVEL     'S'           VC(119)
139100030422     C                   MOVEL     'S'           VC(142)
139200030422     C                   MOVEL     'S'           VC(186)
139300030430     C                   MOVEL     'S'           VC(213)
139400030422     C                   ENDIF
139500030725     C*
139600030801     C* CONTROLLO SE ESISTE =======> Spedizioni oltre 20 colli bancalatura Coop
139700030801     C*    PRIMO CARATTERE FLAG PARTICOLARITA' VARIE = 'O'
139800030801     C                   MOVEL     D$DGVA        WTM01             1
139900030801     C     WTM01         IFEQ      'O'
140000030801     C                   MOVEL     'S'           VC(34)
140100030801     C                   ENDIF
140200960214     C*
140300960214     C* CONTROLLO SE ESISTE =======> SECONDA LINEA DISTRIBUZIONE
140400030430     C*    SECONDO CARATTERE FLAG PARTICOLARITA' VARIE = 'D' e
140500010116     C*                LINEA ARRIVO = TERMINAL ARRIVO  e
140600010116     C*                C.TASSAZIONE DESTINO BOLLA <> C.TASSAZIONE LINEA ARRIVO
140700001204     C                   MOVE      D$DGVA        WTM01             1
140800001204IF  1C     WTM01         IFEQ      'D'
140900111011IF  3C     W$lna         IFEQ      D$DTFA
141000111011     C                   Z-ADD     W$lna         F
141100010116IF  4C     D$DCTS        IFNE      CTS(F)
141200001201     C                   MOVEL     'S'           VC(165)
141300001201     C                   MOVEL     'S'           VC(178)
141400010116E   4C                   ENDIF
141500010116E   3C                   ENDIF
141600001204E   1C                   ENDIF
141700970429     C*
141800970429     C* CONTROLLO SE ESISTE =======> DDT (DOCUM. DI TRASP. DA CONS. AL DEST.)
141900970429     C*    FLAG LIBERO 1  = 'S' O 'C'
142000970429     C     D$DLL1        IFEQ      'S'
142100970429     C     D$DLL1        OREQ      'C'
142200970429     C                   MOVEL     'S'           VC(72)
142300970429     C                   MOVEL     'S'           VC(182)
142400970429     C                   ENDIF
142500990222     C*
142600990222     C* CONTROLLO SE ESISTE =======> CONSEGNA LOCALITA DISAGIATE
142700990222     C*    SE FLAG INOLTRO 'D' E NON C'E' FERMO DEPOSITO
142800030430     C     D$DFFD        IFNE      'S'
142900030430     C     D$DFIN        ANDEQ     'D'
143000030430     C                   MOVEL     'S'           VC(246)
143100030430     C                   ENDIF
143200980202     C*
143300030430     C* CONTROLLO SE ESISTE =======>
143400030430     C                   MOVEL     'S'           VC(212)
143500980204     C                   MOVEL     'S'           VC(270)
143600010116     C                   MOVEL     'S'           VC(523)
143700010116     C                   MOVEL     'S'           VC(594)
143800021031     C                   MOVEL     'S'           VC(595)
143900000721     C                   MOVEL     'S'           VC(592)
144000000721     C                   MOVEL     'S'           VC(274)
144100001201     C                   MOVEL     'S'           VC(23)
144200001201     C                   MOVEL     'S'           VC(161)
144300001201     C                   MOVEL     'S'           VC(177)
144400980317     C*
144500980318     C* CONTROLLO SE ESISTE =======> AFFLUENZA/DEFLUENZA SU STESSO TERMINAL
144600980317     C*    SE TERM.PART. = TERM.ARR. E LINEA ARR. <> LINEA PART.
144700980318     C     W$TFP         IFEQ      D$DTFA
144800980318     C     W$LNP         ANDNE     W$TFP
144900111011     C     W$lna         ANDNE     D$DTFA
145000980319     C                   MOVEL     'S'           VC(560)
145100980317     C                   ENDIF
145200980317     C*
145300980319     C* CONTROLLO SE ESISTE =======> LINEA PART. = TERM. EXPORT
145400980317     C*    SE TERM.PART. = TERM.ARR. = LINEA PART. E EXPORT
145500111011     C                   Z-ADD     W$lna         F
145600980318     C     W$TFP         IFEQ      D$DTFA
145700980318     C     W$LNP         ANDEQ     W$TFP
145800020805     C     FIE(F)        ANDEQ     'E'
145900980319     C                   MOVEL     'S'           VC(561)
146000980317     C                   ENDIF
146100980717     C*
146200980717     C* CONTROLLO SE ESISTE =======> LINEA PART. E' 2°LIV. TERM. EXPORT
146300980717     C*    SE TERM.PART.=TERM.ARR. E LINEA PART.<>TERM.PART. E EXPORT
146400111011     C                   Z-ADD     W$lna         F
146500980717     C     W$TFP         IFEQ      D$DTFA
146600980717     C     W$LNP         ANDNE     W$TFP
146700020805     C     FIE(F)        ANDEQ     'E'
146800980717     C                   MOVEL     'S'           VC(570)
146900980717     C                   ENDIF
147000980717     C*
147100980717     C* CONTROLLO SE ESISTE =======> LINEA ARR. E' 2°LIV. TERM. IMPORT
147200980717     C*    SE TERM.PART.=TERM.ARR. E LINEA ARR.<>TERM.ARR. E IMPORT
147300980717     C                   Z-ADD     W$LNP         F
147400980717     C     W$TFP         IFEQ      D$DTFA
147500111011     C     W$lna         ANDNE     D$DTFA
147600020805     C     FIE(F)        ANDEQ     'E'
147700980717     C                   MOVEL     'S'           VC(571)
147800980717     C                   ENDIF
147900990217     C*
148000990420     C* CONTROLLO SE ESISTE =======> CONSEGNA ISOLA
148100990219     C*    SE FLAG INOLTRO 'I' E NON C'E' FERMO DEPOSITO
148200111011     C                   Z-ADD     W$lna         F
148300000302IF  1C     D$DFFD        IFNE      'S'
148400990219     C     D$DFIN        ANDEQ     'I'
148500020805IF  2C     FIE(F)        IFEQ      'E'
148600020805IF  3C     NTW(F)        IFEQ      'DPD'
148700000302     C                   MOVEL     'S'           VC(288)
148800000302E   3C                   ENDIF
148900000302X   2C                   ELSE
149000000302     C                   MOVEL     'S'           VC(278)
149100000302E   2C                   ENDIF
149200000302E   1C                   ENDIF
149300990217     C*
149400990420     C* CONTROLLO SE ESISTE =======> RITIRO ISOLA
149500990217     C*    SE FLAG ANTEPORTO 'I' E NON C'E' CONSEGNA A MAGAZZINO
149600000302     C                   Z-ADD     W$LNP         F
149700000302IF  1C     D$DFDN        IFNE      'S'
149800990219     C     D$DFAP        ANDEQ     'I'
149900020805IF  2C     FIE(F)        IFEQ      'E'
150000020805IF  3C     NTW(F)        IFEQ      'DPD'
150100000302     C                   MOVEL     'S'           VC(289)
150200000302E   3C                   ENDIF
150300000302X   2C                   ELSE
150400990420     C                   MOVEL     'S'           VC(279)
150500040617     c* per sped. > 00/07/2004 attivo voce 285 COMP.RITIRI ISOLE VENEZIA
150600040617     C                   IF        WDDSP > 20040700
150700040617     C                   MOVEL     'S'           VC(285)
150800040617E   1C                   ENDIF
150900000302E   2C                   ENDIF
151000000302E   1C                   ENDIF
151100990217     C*
151200990217     C* CONTROLLO SE ESISTE =======> PREPAGATI
151300990217     C*    SE CLIENTE 8888
151400990217     C                   MOVE      W$KSC         KSCIS             4 0
1515009902171    C     KSCIS         IFEQ      8888
151600990217     C                   MOVEL     'S'           VC(266)
1517009902171    C                   ENDIF
151800020621     C*
151900020621E  1aC                   ENDIF
152000000630     C*
152100940526     C                   ENDSR
152200940908     C*------------------------------------------------------------------------*
152300940908     C*?CAL2R  - CALCOLA LE QTA DELLA TABELLA 2R (TIPO TARIFFA)
152400940908     C*------------------------------------------------------------------------*
152500940908     C     CAL2R         BEGSR
152600940908     C* TIPO -01- COLLI -------------------------------------------------------*
152700940908     C                   Z-ADD     1             I
152800940908     C     '01'          LOOKUP    C2R(I)                                 98
152900940908     C     *IN98         IFEQ      '0'
153000940908     C                   MOVEL     MSG(5)        $MSG
153100940908     C                   EXSR      SCRERR
153200940908     C                   ELSE
153300940908     C                   Z-ADD     D$DNCL        Q2R(I)
153400940908     C                   ENDIF
153500940908     C*
153600940908     C* TIPO -02- SPEDIZIONI --------------------------------------------------*
153700940908     C                   Z-ADD     1             I
153800940908     C     '02'          LOOKUP    C2R(I)                                 98
153900940908     C     *IN98         IFEQ      '0'
154000940908     C                   MOVEL     MSG(6)        $MSG
154100940908     C                   EXSR      SCRERR
154200940908     C                   ELSE
154300940908     C                   Z-ADD     1             Q2R(I)
154400940908     C                   ENDIF
154500940908     C*
154600940908     C* TIPO -03- CHILOGRAMMI -------------------------------------------------*
154700040428     c* PRIMA maggio 2004
154800040428     C* SEMPRE PESO DA FATTURARE
154900040428     C                   IF        WDDSP < 20040400
155000040428     C                   Z-ADD     D$DPKF        WPES
155100040428X   1C                   ELSE
155200040428     c* DOPO maggio 2004
155300040428     C* SE I COLLI=COLLI CML PRENDO PESO CML
155400040428IF  2C     D$DPKC        IFGT      *ZEROS
155500040428     C     D$DNCL        ANDEQ     D$DNCP
155600040428     C                   Z-ADD     D$DPKC        WPES
155700040428X   2C                   ELSE
155800040428     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
155900040428IF  3C     D$DPKF        IFGT      D$DPKC
156000040428     C                   Z-ADD     D$DPKF        WPES
156100040428X   3C                   ELSE
156200040428     C                   Z-ADD     D$DPKC        WPES
156300040428E   3C                   ENDIF
156400040428E   2C                   ENDIF
156500040428E   1C                   ENDIF
156600940908     C                   Z-ADD     1             I
156700940908     C     '03'          LOOKUP    C2R(I)                                 98
156800940908     C     *IN98         IFEQ      '0'
156900940908     C                   MOVEL     MSG(7)        $MSG
157000940908     C                   EXSR      SCRERR
157100940908     C                   ELSE
157200991008     C                   Z-ADD     WPES          Q2R(I)
157300940908     C                   ENDIF
157400940908     C*
157500940908     C* TIPO -04- VOLUME ------------------------------------------------------*
157600990422     C* SE VOLUME FATTURATO E' 'T' (TOTALE COLLI) PRENDO QUELLO
157700990421IF  1C     D$DFVF        IFEQ      'T'
157800990421     C                   Z-ADD     D$DVLF        WVOL
157900990421X   1C                   ELSE
158000990422     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
158100990421IF  2C     D$DVLC        IFGT      *ZEROS
158200990421     C     D$DNCL        ANDEQ     D$DNCR
158300990422     C                   Z-ADD     D$DVLC        WVOL
158400990421X   2C                   ELSE
158500990422     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
158600990422IF  3C     D$DVLF        IFGT      D$DVLC
158700990422     C                   Z-ADD     D$DVLF        WVOL
158800990421X   3C                   ELSE
158900990422     C                   Z-ADD     D$DVLC        WVOL
159000990421E   3C                   ENDIF
159100990421E   2C                   ENDIF
159200990421E   1C                   ENDIF
159300990421     C*
159400940908     C                   Z-ADD     1             I
159500940908     C     '04'          LOOKUP    C2R(I)                                 98
159600940908     C     *IN98         IFEQ      '0'
159700940908     C                   MOVEL     MSG(8)        $MSG
159800940908     C                   EXSR      SCRERR
159900940908     C                   ELSE
160000940908     C                   Z-ADD     WVOL          Q2R(I)
160100940908     C                   ENDIF
160200030409     C*
160300030409     C* TIPO -06- BANCALI -----------------------------------------------------*
160400030422     C                   CLEAR                   WBAN              5 0
160500030422     C                   EVAL      WBAN = D$DAR5NBA + D$DAR5NB2
160600030422     C                   Z-ADD     1             I
160700030422     C     '06'          LOOKUP    C2R(I)                                 98
160800030422     C     *IN98         IFEQ      '0'
160900030422     C                   MOVEL     MSG(35)       $MSG
161000030422     C                   EXSR      SCRERR
161100030422     C                   ELSE
161200030422     C                   Z-ADD     WBAN          Q2R(I)
161300030422     C                   ENDIF
161400940908     C*
161500951213     C* TIPO -09- STOP RITIRO -------------------------------------------------*
161600940908     C                   Z-ADD     1             I
161700940908     C     '09'          LOOKUP    C2R(I)                                 98
161800940908IF  1C     *IN98         IFEQ      '0'
161900940908     C                   MOVEL     MSG(9)        $MSG
162000940908     C                   EXSR      SCRERR
162100940908X   1C                   ELSE
162200941209     C                   Z-ADD     D$DNSR        Q2R(I)
162300940908E   1C                   ENDIF
162400951213     C*
162500951213     C* TIPO -10- STOP CONSEGNA -----------------------------------------------*
162600951213     C                   Z-ADD     1             I
162700951213     C     '10'          LOOKUP    C2R(I)                                 98
162800951213IF  1C     *IN98         IFEQ      '0'
162900951213     C                   MOVEL     MSG(16)       $MSG
163000951213     C                   EXSR      SCRERR
163100951213X   1C                   ELSE
163200951213     C                   Z-ADD     D$DNSC        Q2R(I)
163300951213E   1C                   ENDIF
163400991029     C*
163500991029     C* TIPO -11- PESO X GIORNI GIACENZA --------------------------------------*
163600991029     C                   Z-ADD     1             I
163700991029     C     '11'          LOOKUP    C2R(I)                                 98
163800991029IF  1C     *IN98         IFEQ      '0'
163900991029     C                   MOVEL     MSG(17)       $MSG
164000991029     C                   EXSR      SCRERR
164100991029X   1C                   ELSE
164200991029     C     D$DGGS        IFEQ      *ZEROS
164300991029     C                   Z-ADD     1             WT0155           15 5
164400991029     C                   ELSE
164500991029     C                   Z-ADD     D$DGGS        WT0155
164600991029     C                   ENDIF
164700991029     C     WPES          MULT      WT0155        Q2R(I)
164800991029E   1C                   ENDIF
164900000201     C*
165000000201     C* TIPO -12- GIORNI GIACENZA ---------------------------------------------*
165100000201     C                   Z-ADD     1             I
165200000201     C     '12'          LOOKUP    C2R(I)                                 98
165300000201IF  1C     *IN98         IFEQ      '0'
165400000201     C                   MOVEL     MSG(25)       $MSG
165500000201     C                   EXSR      SCRERR
165600000201X   1C                   ELSE
165700000201IF  2C     D$DGGS        IFEQ      *ZEROS
165800000201     C                   Z-ADD     1             Q2R(I)
165900000201X   2C                   ELSE
166000000201     C                   Z-ADD     D$DGGS        Q2R(I)
166100000201E   2C                   ENDIF
166200000201E   1C                   ENDIF
166300940908     C*
166400940908     C* TIPO -21- IMPONIBILE REALE --------------------------------------------*
166500940908     C                   Z-ADD     1             I
166600940908     C     '21'          LOOKUP    C2R(I)                                 98
166700940908     C     *IN98         IFEQ      '0'
166800940908     C                   MOVEL     MSG(11)       $MSG
166900940908     C                   EXSR      SCRERR
167000940908     C                   ELSE
167100940908     C* CERCO NELLA SCHIERA IL INPUT AL PGM L'IMPONIBILE REALE
167200940908     C                   Z-ADD     1             II                9 0
167300940908     C     556           LOOKUP    RCD(II)                                98
167400940908     C     *IN98         IFEQ      '1'
167500940908     C                   Z-ADD     RIM(II)       Q2R(I)
167600940908     C                   ENDIF
167700940908     C                   ENDIF
167800940908     C*
167900940908     C* TIPO -22- CONTRASEGNO -------------------------------------------------*
168000940908     C                   Z-ADD     1             I
168100940908     C     '22'          LOOKUP    C2R(I)                                 98
168200940908     C     *IN98         IFEQ      '0'
168300940908     C                   MOVEL     MSG(12)       $MSG
168400940908     C                   EXSR      SCRERR
168500940908     C                   ELSE
168600941201     C                   Z-ADD     D$DCAS        Q2R(I)
168700940908     C                   ENDIF
168800940908     C*
168900940908     C* TIPO -23- VALORE MERCE ASSICURATA -------------------------------------*
169000940908     C                   Z-ADD     1             I
169100940908     C     '23'          LOOKUP    C2R(I)                                 98
169200940908     C     *IN98         IFEQ      '0'
169300940908     C                   MOVEL     MSG(13)       $MSG
169400940908     C                   EXSR      SCRERR
169500940908     C                   ELSE
169600011227     C                   Z-ADD     D$DVAE        Q2R(I)
169700940908     C                   ENDIF
169800940908     C*
169900940908     C* TIPO -24- NUMERO DI GIORNI PAGAMENTO ----------------------------------*
170000940908     C                   Z-ADD     1             I
170100940908     C     '24'          LOOKUP    C2R(I)                                 98
170200940908     C     *IN98         IFEQ      '0'
170300940908     C                   MOVEL     MSG(14)       $MSG
170400940908     C                   EXSR      SCRERR
170500940908     C                   ELSE
170600940908     C                   Z-ADD     WGGP          Q2R(I)
170700940908     C                   ENDIF
170800940908     C*
170900940908     C                   ENDSR
171000940803     C*------------------------------------------------------------------------*
171100940803     C*? CHKVOC - CONTROLLA CHE TUTTE LE VOCI DA CALCOLARE SIANO VALIDE
171200940803     C*            ALLA DATA DELLA BOLLA ED EVENTUALMENTE CARICA LE GIUSTE
171300940803     C*            DECORRENZE
171400940803     C*            INPUT: - WDDSP (DATA SPEDIZIONE BOLLA)
171500940803     C*            OUTPUT:- SCHIERE VOCI COMPLETE ALLA DATA IN INPUT
171600940803     C*------------------------------------------------------------------------*
171700940603     C     CHKVOC        BEGSR
171800940603     C*
171900940607     C* CICLO PER TUTTE LE VOCI DA CALCOLARE
1720009406071    C                   DO        999           V
1721009406072    C     VC(V)         IFEQ      'S'
1722009407293    C     WDDSP         IFLT      VDDV(V)
172300940729     C     WDDSP         ORGT      VDSV(V)
172400940630     C                   Z-ADD     *ZEROS        VPRG(V)
172500940603     C                   MOVEL     *BLANKS       VTVO(V)
172600940704     C                   Z-ADD     *ZEROS        VDDV(V)
172700940704     C                   Z-ADD     *ZEROS        VDSV(V)
172800940603     C                   MOVEL     *BLANKS       VVRV(V)
172900940603     C                   MOVEL     *BLANKS       VTIV(V)
173000940603     C                   MOVEL     *BLANKS       VSPT(V)
173100940603     C                   Z-ADD     *ZEROS        VCMP(V)
173200940607     C                   MOVEL     *BLANKS       VRFI(V)
173300940607     C                   MOVEL     *BLANKS       VRFE(V)
173400940607     C                   MOVEL     *BLANKS       VRTI(V)
173500940607     C                   MOVEL     *BLANKS       VRTE(V)
173600940607     C                   MOVEL     *BLANKS       VRPI(V)
173700940607     C                   MOVEL     *BLANKS       VRPE(V)
173800940607     C                   MOVEL     *BLANKS       VRKI(V)
173900940607     C                   MOVEL     *BLANKS       VRKE(V)
174000940704     C                   Z-ADD     *ZEROS        VARF(V)
174100940704     C                   Z-ADD     *ZEROS        VARL(V)
174200940704     C                   Z-ADD     *ZEROS        VARO(V)
174300030113     C                   MOVEL     *BLANKS       VCOP(V)
174400030113     C                   MOVEL     *BLANKS       VDAT(V)
174500030113     C                   MOVEL     *BLANKS       VAID(V)
174600030422     C                   Z-ADD     *ZEROS        VRPV(V)
174700030422     C                   MOVEL     *BLANKS       VSPS(V)
174800940607     C                   MOVEL     'N'           VC(V)
174900940607     C* CERCO IL RECORD DELLA VOCE CON DEC/SCAD PER LA DATA BOLLA
175000940607     C                   Z-ADD     V             EVTVOC
175100940630     C                   Z-ADD     *ZEROS        EVTPRG
175200940603     C     KEYEVT        SETLL     ECEVT01L                           99
175300940603     C  N99              READ      ECEVT01L                               99
175400940607     C     *IN99         IFEQ      '1'
175500940829     C     EVTVOC        ORNE      V
175600991119     C                   MOVEL     MSG(18)       DSMSG
175700991119     C                   MOVE      V             DSVOCE
175800991119     C                   MOVEL     DSMSG         $MSG
175900940907     C                   EXSR      SCRERR
176000940607     C                   END
1761009407044    C     *IN99         DOWEQ     '0'
176200940607     C     EVTVOC        ANDEQ     V
176300940704     C     VC(V)         ANDEQ     'N'
1764009407045    C     EVTATB        IFEQ      *BLANKS
176500940729     C     WDDSP         ANDGE     EVTDDV
176600940729     C     WDDSP         ANDLE     EVTDSV
176700940603     C                   Z-ADD     EVTVOC        V
176800940630     C                   Z-ADD     EVTPRG        VPRG(V)
176900940607     C                   MOVEL     EVTTVO        VTVO(V)
177000940704     C                   Z-ADD     EVTDDV        VDDV(V)
177100940704     C                   Z-ADD     EVTDSV        VDSV(V)
177200090304     c* per pod immage forzo colli invece di spedizione, perchè non si
177300090304     C* vuole cambiare il tipo tariffa, la spedizione sarà una e i
177400090304     c* colli sono le immagini
177500090304     c                   if        v = 199
177600090304     C                   MOVEL     '01'          VVRV(V)
177700090304     c                   if        evttiv <> ' '
177800090304     C                   MOVEL     '01'          VTIV(V)
177900090304     c                   end
178000090304     c                   else
178100940607     C                   MOVEL     EVTVRV        VVRV(V)
178200940607     C                   MOVEL     EVTTIV        VTIV(V)
178300090304     c                   end
178400940607     C                   MOVEL     EVTSPT        VSPT(V)
178500940607     C                   Z-ADD     EVTCMP        VCMP(V)
178600940607     C                   MOVEL     EVTRFI        VRFI(V)
178700940607     C                   MOVEL     EVTRFE        VRFE(V)
178800940607     C                   MOVEL     EVTRTI        VRTI(V)
178900940607     C                   MOVEL     EVTRTE        VRTE(V)
179000940607     C                   MOVEL     EVTRPI        VRPI(V)
179100940607     C                   MOVEL     EVTRPE        VRPE(V)
179200940607     C                   MOVEL     EVTRKI        VRKI(V)
179300940607     C                   MOVEL     EVTRKE        VRKE(V)
179400940704     C                   Z-ADD     EVTARF        VARF(V)
179500940704     C                   Z-ADD     EVTARL        VARL(V)
179600940704     C                   Z-ADD     EVTARO        VARO(V)
179700030113     C                   MOVEL     EVTCOP        VCOP(V)
179800030113     C                   MOVEL     EVTDAT        VDAT(V)
179900030113     C                   MOVEL     EVTAID        VAID(V)
180000030422     C                   MOVEL     EVTXXX        DEVTXXX
180100030422     C                   Z-ADD     EVTRPV        VRPV(V)
180200030422     C                   MOVEL     EVTSPS        VSPS(V)
180300940607     C                   MOVEL     'S'           VC(V)
1804009407045    C                   ENDIF
180500940603     C                   READ      ECEVT01L                               99
1806009407044    C                   ENDDO
1807009407043    C                   ENDIF
1808009407042    C                   ENDIF
1809009406071    C                   ENDDO
181000940603     C*
181100940603     C                   ENDSR
181200940803     C*------------------------------------------------------------------------*
181300940803     C*? VALVOC - VALORIZZA LE VOCI VALIDE
181400940803     C*------------------------------------------------------------------------*
181500940526     C     VALVOC        BEGSR
181600940526     C*
181700940603     C* CICLO PER TUTTE LE VOCI 'DA CALCOLARE'
181800940603     C                   DO        999           V
181900940607     C     VC(V)         IFEQ      'S'
182000940630     C                   Z-ADD     V             WVOC
182100940630     C                   Z-ADD     VPRG(V)       WPRG
182200000306     C                   MOVEL     VTIV(V)       WTIV
182300940607     C* CERCA TARIFFA
182400940607     C                   EXSR      CERTAR
182500940729     C*
182600940908     C* SE NON C'E' ERRORE (ES. MANCA TARIFFA)
182700940908     C* E IL MANCA TARIFFA ERA DA SEGNALARE
182800940913     C     $ERR          IFEQ      'N'
182900940908     C     WMSGMT        ANDEQ     'S'
183000940720     C*
183100940720     C* CALCOLA QTA
183200941107     C*
183300941107     C* SE TIPO VALORIZZAZIONE = Q=QUANTITA  OPPURE P=PERCENTUALE
183400941107     C* RECUPERO LA QTA O IL VALORE DI RIFERIMENTO
183500020109     C                   Z-ADD     *ZEROS        WQTA             21 9
183600940701     C     VVRV(V)       IFNE      *BLANKS
183700940721     C                   Z-ADD     1             I
183800940701     C     VVRV(V)       LOOKUP    C2R(I)                                 98
183900940701     C     *IN98         IFEQ      '0'
184000940720     C                   MOVEL     MSG(19)       $MSG
184100940907     C                   EXSR      SCRERR
184200940729     C                   ELSE
184300941107     C* SE E' UNA TARIFFA DOVE E' STATO CALCOLATO IL PESO TASSABILE
184400941107     C* (GIA' CALCOLATO NELLA RICERCA DELLO SCAGLIONE) PRENDO QTA
184500941107     C* UTILIZZATA PER LA RICERCA DELLO SCAGLIONE
184600941107     C* ALTRIMENTI PRENDO QTA DELLA SCHIERA
184700941107     C     VSPT(V)       IFNE      *BLANKS
184800961202     C     WQTAPT        ANDGT     *ZEROS
184900941107     C                   Z-ADD     WQTAPT        WQTA
185000941107     C                   ELSE
185100940729     C                   Z-ADD     Q2R(I)        WQTA
185200941107     C                   ENDIF
185300940729     C                   MOVEL     T2R(I)        WTTQ              1
185400940720     C                   ENDIF
185500940721     C                   ENDIF
185600940803     C*
185700940803     C* CONTROLLO L'ARROTONDAMENTO
185800940803     C                   EXSR      ARROTO
185900940720     C*
186000020109     C                   Z-ADD     *ZEROS        WVVOCE           21 9
186100090914     C                   Z-ADD     *ZEROS        WVVO25           15 5
186200940802     C*
186300940802     C* SE TIPO VOCE V=VOCE NORMALE
186400940802     C* IN BASE AL TIPO TARIFFA CALCOLA IMPORTO A QTA, FISSO, PERCENTUALE
186500940802     C     VTVO(V)       IFEQ      'V'
186600940802     C                   EXSR      CALVOC
186700940802     C                   ENDIF
186800940720     C*
186900940802     C* SE TIPO VOCE M=MAGGIORAZIONE
187000940802     C* RECUPERO TUTTE LE VOCI DA MAGGIORARE E APPLICO LA PERCENTUALE
187100940802     C     VTVO(V)       IFEQ      'M'
187200940906     C                   EXSR      CERMAG
187300940802     C                   ENDIF
187400940802     C*
187500940802     C* CARICO TIPO TARIFFA -> IMPONIBILE PER COMPETENZE SEDE (SOMMA DI TUTTE
187600991027     C* LE VOCI PRECEDENTI TRANNE I TRANSITI)
187700991027     C     VS(V)         IFEQ      'S'
187800940802     C                   Z-ADD     1             I
187900940802     C     '25'          LOOKUP    C2R(I)                                 98
188000940802     C     *IN98         IFEQ      '0'
188100940802     C                   MOVEL     MSG(15)       $MSG
188200940907     C                   EXSR      SCRERR
188300940802     C                   ELSE
188400940824     C                   ADD       WVVO25        Q2R(I)
188500940802     C                   ENDIF
188600991027     C                   ENDIF
188700940707     C*
188800940721     C                   ENDIF
188900940729     C                   ENDIF
189000940721     C                   ENDDO
189100940603     C*
189200940526     C                   ENDSR
189300940803     C*------------------------------------------------------------------------*
189400940803     C*? CALVOC - CALCOLA LE VOCI NORMALI
189500940803     C*------------------------------------------------------------------------*
189600940802     C     CALVOC        BEGSR
189700011227     C*
189800011227     C* CONVERTE GLI IMPORTI DELLE TARIFFE SE LA DIVISA DELLE ANAGRAFICHE C/E E'
189900011227     C* DIVERSA DALLA DIVISA DI OUTPUT DEL C/E
190000011227     C                   Z-ADD     EVDITR        W$ITR
190100011227     C                   Z-ADD     EVDIMI        W$IMI
190200011227     C                   Z-ADD     EVDIMA        W$IMA
190300011227IF  1C     WDIVAC        IFNE      WDIVCE
190400011227IF  2C     EVDITR        IFNE      *ZEROS
190500011227     C                   Z-ADD     EVDITR        §IMI
190600011227     C                   MOVEL     WDIVAC        §DVI
190700011227     C                   EXSR      CNVEUR
190800011227     C                   Z-ADD     §IMO          W$ITR
190900011227E   2C                   ENDIF
191000011227IF  2C     EVDIMI        IFNE      *ZEROS
191100011227     C                   Z-ADD     EVDIMI        §IMI
191200011227     C                   MOVEL     WDIVAC        §DVI
191300011227     C                   EXSR      CNVEUR
191400011227     C                   Z-ADD     §IMO          W$IMI
191500011227E   2C                   ENDIF
191600011227IF  2C     EVDIMA        IFNE      *ZEROS
191700011227     C                   Z-ADD     EVDIMA        §IMI
191800011227     C                   MOVEL     WDIVAC        §DVI
191900011227     C                   EXSR      CNVEUR
192000011227     C                   Z-ADD     §IMO          W$IMA
192100011227E   2C                   ENDIF
192200011227E   1C                   ENDIF
192300940802     C*
192400940802     C* CALCOLO IN BASE AL TIPO TARIFFA
192500940802     C*
192600940802     C     WTTQ          IFEQ      'Q'
192700940802     C     WQTA          ANDNE     *ZEROS
192800011227     C     W$ITR         ANDNE     *ZEROS
192900011227     C     WQTA          MULT      W$ITR         WVVOCE
193000940802     C                   ENDIF
193100940802     C*
193200940802     C     WTTQ          IFEQ      'F'
193300011227     C                   Z-ADD     W$ITR         WVVOCE
193400940802     C                   ENDIF
193500940802     C*
193600940802     C     WTTQ          IFEQ      'P'
193700940802     C     WQTA          ANDNE     *ZEROS
193800940802     C     EVDPER        ANDNE     *ZEROS
193900940802     C     WQTA          DIV       100           WVVOCE
194000940802     C     WVVOCE        MULT      EVDPER        WVVOCE
194100940802     C                   ENDIF
194200940802     C*
194300940802     C* CONTROLLO IMPORTO MINIMO
194400011227     C     W$IMI         IFNE      *ZEROS
194500011227     C     WVVOCE        ANDLT     W$IMI
194600011227     C                   Z-ADD     W$IMI         WVVOCE
194700940802     C                   ENDIF
194800011227     C     W$IMA         IFNE      *ZEROS
194900011227     C     WVVOCE        ANDGT     W$IMA
195000011227     C                   Z-ADD     W$IMA         WVVOCE
195100940802     C                   ENDIF
195200940802     C*
195300940802     C* CONTROLLO SEGNO E CARICO IMPORTO NELLA SCHIERA
195400090914     C                   Z-ADD(H)  WVVOCE        N133             13 5
195500940802     C     EVDSEG        IFEQ      '+'
195600020130     C                   Z-ADD     N133          VIMP(V)
195700020130     C                   ADD       N133          WVVO25
195800940802     C                   ELSE
195900020130     C                   Z-SUB     N133          VIMP(V)
196000020130     C                   SUB       N133          WVVO25
196100940802     C                   ENDIF
196200940802     C*
196300940802     C                   ENDSR
196400940907     C*------------------------------------------------------------------------*
196500940907     C*?ARROTO - ARROTONDA LA QUANTITA'
196600940907     C*          INPUT  : WQTA (DA ARROTONDARE)
196700940907     C*          OUTPUT : WQTA (ARROTONDATA)
196800940907     C*------------------------------------------------------------------------*
196900940907     C     ARROTO        BEGSR
197000940907     C*
197100940907     C* CONTROLLO SE ESISTE L'ARROTONDAMENTO
197200940907     C     WQTA          IFNE      *ZEROS
197300940907     C     VARL(V)       ANDNE     *ZEROS
197400940907     C     WQTA          IFLE      VARL(V)
197500010202     C                   Z-ADD     VARF(V)       WARR             17 5
197600940907     C                   ELSE
197700940907     C                   Z-ADD     VARO(V)       WARR
197800940907     C                   ENDIF
197900940907     C* CONTROLLO SE DATO GIA' ARRORONDATO
198000940907     C     WQTA          DIV       WARR          WINT             15 0
198100970730     C                   MVR                     WT0155           15 5    99
198200940907     C     *IN99         IFEQ      *OFF
198300940907     C     WINT          MULT      WARR          WQTA
198400940907     C                   ADD       WARR          WQTA
198500940907     C                   ENDIF
198600940907     C                   ENDIF
198700940907     C*
198800940907     C                   ENDSR
198900940907     C*------------------------------------------------------------------------*
199000940907     C*? CALPT  - CALCOLA IL PESO TASSABILE
199100940907     C*------------------------------------------------------------------------*
199200940907     C     CALPT         BEGSR
199300940907     C*
199400940907     C* CALCOLO PESO TASSABILE DAL VOLUME
199500941019     C     WVOL          MULT      EVDRPV        WPTAS
199600940907     C     WPTAS         MULT      100           WPTAS
199700940907     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
199800940907     C     VSPT(V)       IFEQ      '1'
199900940907     C                   Z-ADD     WPTAS         WQTA
200000940907     C                   ENDIF
200100940907     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
200200940907     C     VSPT(V)       IFEQ      '2'
200300991008     C     WPTAS         IFGT      WPES
200400940907     C                   Z-ADD     WPTAS         WQTA
200500940907     C                   ENDIF
200600940907     C                   ENDIF
200700940907     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
200800940907     C     VSPT(V)       IFEQ      '3'
200900991008     C     WPTAS         IFLT      WPES
201000940907     C                   Z-ADD     WPTAS         WQTA
201100940907     C                   ENDIF
201200940907     C                   ENDIF
201300940907     C*
201400940907     C                   ENDSR
201500030422     C*------------------------------------------------------------------------*
201600030422     C*? CALPS  - CALCOLA IL PESO TASSABILE PER RICERCA SCAGLIONE
201700030422     C*------------------------------------------------------------------------*
201800030422     C     CALPS         BEGSR
201900040209     C                   CLEAR                   WPTASR2
202000030422     C*
202100030422     C* CALCOLO PESO TASSABILE DAL VOLUME
202200030423     C     WVOL          MULT      VRPV(V)       WPTASR
202300030423     C     WPTASR        MULT      100           WPTASR
202400030422     C* SVILUPPPO PESO TASSABILE 1 (SOLO VOLUME)
202500030423     C     VSPS(V)       IFEQ      '1'
202600040209     C                   Z-ADD     WPTASR        WPTASR2
202700030422     C                   ENDIF
202800030422     C* SVILUPPPO PESO TASSABILE 2 (MAGG. TRA REALE E TASSABILE)
202900030423     C     VSPS(V)       IFEQ      '2'
203000030423     C     WPTASR        IFGT      WPES
203100040209     C                   Z-ADD     WPTASR        WPTASR2
203200040209     c                   ELSE
203300040209     C                   Z-ADD     WPES          WPTASR2
203400030422     C                   ENDIF
203500030422     C                   ENDIF
203600030422     C* SVILUPPPO PESO TASSABILE 3 (MINORE TRA REALE E TASSABILE)
203700030423     C     VSPS(V)       IFEQ      '3'
203800030423     C     WPTASR        IFLT      WPES
203900040209     C                   Z-ADD     WPTASR        WPTASR2
204000040209     C                   ELSE
204100040209     C                   Z-ADD     WPES          WPTASR2
204200030422     C                   ENDIF
204300030422     C                   ENDIF
204400030422     C*
204500030422     C                   ENDSR
204600940803     C*------------------------------------------------------------------------*
204700940906     C*? CERMAG - CERCA LE MAGGIORAZIONI
204800940803     C*------------------------------------------------------------------------*
204900940906     C     CERMAG        BEGSR
205000940908     C*
205100940906     C* SE PERCENTUALE DIVERSA DA ZERO NON CERCO NEMMENO LA MAGGIORAZIONE
205200940906     C     EVDPER        IFNE      *ZEROS
205300940908     C                   MOVEL     'N'           WMAGOK            1
205400941229     C******+++++++++++++++ADD  1         WTEMAG  90       *TENTATIVI LETT MAGG.
205500940906     C*
205600941024     C* CONTROLLA SE MAGGIORAZIONI IN MEMORIA
205700940906     C                   Z-ADD     1             M                 6 0
205800940906     C     M             DOWLE     WULTMA
205900941024     C***********WMAGOK    ANDEQ'N'                        MAGG. NON TROVATA
206000940906     C     WVOC          IFEQ      MVOM(M)
206100940906     C     WPRG          ANDEQ     MPRM(M)
206200940908     C                   MOVEL     'S'           WMAGOK
206300941229     C*****++++++++++++++++ADD  1         WMEMAG  90       *MAGG.IN MEMORIA
206400940906     C                   Z-ADD     MVOM(M)       EVMVOM
206500940906     C                   Z-ADD     MPRM(M)       EVMPRM
206600940906     C                   Z-ADD     MVOC(M)       EVMVOC
206700940906     C                   Z-ADD     MPRG(M)       EVMPRG
206800940906     C                   EXSR      CALMAG
206900940906     C                   ENDIF
207000940906     C                   ADD       1             M
207100940906     C                   ENDDO
207200940908     C*
207300940906     C* SE NON TROVATA IN MEMORIA E FINE SCHIERA LEGGO SUL FILE
207400940906     C     WMAGOK        IFEQ      'N'
207500041118     C     WULTMA        ANDGE     600
207600940906     C*
207700030331     C                   Z-ADD     WVOC          EVMVOM
207800940802     C                   Z-ADD     WPRG          EVMPRM
207900940802     C                   Z-ADD     *ZEROS        EVMVOC
208000940802     C                   Z-ADD     *ZEROS        EVMPRG
208100940802     C     KEYEVM        SETLL     ECEVM01L                           99
208200940802     C  N99              READ      ECEVM01L                               99
208300940802     C     *IN99         IFEQ      '1'
208400940802     C                   MOVEL     MSG(22)       $MSG
208500940907     C                   EXSR      SCRERR
208600940802     C                   END
208700940906     C     *IN99         DOWEQ     '0'
208800940802     C     EVMVOM        ANDEQ     WVOC
208900940802     C     EVMPRM        ANDEQ     WPRG
209000940906     C     EVMATB        IFEQ      *BLANKS
209100940906     C                   EXSR      CALMAG
209200940906     C                   ENDIF
209300940802     C                   READ      ECEVM01L                               99
209400940906     C                   ENDDO
209500940802     C*
209600940906     C                   ENDIF
209700940906     C                   ENDIF
209800940906     C*
209900940802     C                   ENDSR
210000940906     C*------------------------------------------------------------------------*
210100940906     C*? CALMAG - CALCOLA LA MAGGIORAZIONE
210200940906     C*------------------------------------------------------------------------*
210300940906     C     CALMAG        BEGSR
210400011227     C*
210500011227     C* CONVERTE GLI IMPORTI DELLE TARIFFE SE LA DIVISA DELLE ANAGRAFICHE C/E E'
210600011227     C* DIVERSA DALLA DIVISA DI OUTPUT DEL C/E
210700011227     C                   Z-ADD     EVDIMI        W$IMI
210800011227     C                   Z-ADD     EVDIMA        W$IMA
210900011227IF  1C     WDIVAC        IFNE      WDIVCE
211000011227IF  2C     EVDIMI        IFNE      *ZEROS
211100011227     C                   Z-ADD     EVDIMI        §IMI
211200011227     C                   MOVEL     WDIVAC        §DVI
211300011227     C                   EXSR      CNVEUR
211400011227     C                   Z-ADD     §IMO          W$IMI
211500011227E   2C                   ENDIF
211600011227IF  2C     EVDIMA        IFNE      *ZEROS
211700011227     C                   Z-ADD     EVDIMA        §IMI
211800011227     C                   MOVEL     WDIVAC        §DVI
211900011227     C                   EXSR      CNVEUR
212000011227     C                   Z-ADD     §IMO          W$IMA
212100011227E   2C                   ENDIF
212200011227E   1C                   ENDIF
212300940906     C*
212400940906     C                   Z-ADD     EVMVOC        I
212500000407     C* CALCOLA SOLO SE LA VOCE DA MAGGIORARE E' DA CALCOLARE
212600000407     C     VC(I)         IFEQ      'S'
212700940906     C* CALCOLA LA PERCENTUALE
212800940906     C     VIMP(I)       DIV       100           WVVOCE
212900940906     C     WVVOCE        MULT      EVDPER        WVVOCE
213000940906     C* CONTROLLO IMPORTO MINIMO
213100011227     C     W$IMI         IFNE      *ZEROS
213200011227     C     WVVOCE        ANDLT     W$IMI
213300011227     C                   Z-ADD     W$IMI         WVVOCE
213400940906     C                   ENDIF
213500940906     C* CONTROLLO IMPORTO MASSIMO
213600011227     C     W$IMA         IFNE      *ZEROS
213700011227     C     WVVOCE        ANDGT     W$IMA
213800011227     C                   Z-ADD     W$IMA         WVVOCE
213900940906     C                   ENDIF
214000940906     C* CONTROLLO SEGNO E AGGIUNGO O TOLGO IMPORTO NELLA SCHIERA
214100090914     C                   Z-ADD(H)  WVVOCE        N133
214200940906     C     EVDSEG        IFEQ      '+'
214300020130     C     VIMP(I)       ADD       N133          VIMP(I)
214400050503     c                   if        vs(i) = 'S'
214500020130     C                   ADD       N133          WVVO25
214600050503     c                   end
214700940906     C                   ELSE
214800020130     C     VIMP(I)       SUB       N133          VIMP(I)
214900050503     c                   if        vs(i) = 'S'
215000020130     C                   SUB       N133          WVVO25
215100050503     c                   end
215200940906     C                   ENDIF
215300000407     C                   ENDIF
215400940906     C*
215500940906     C                   ENDSR
215600940908     C*------------------------------------------------------------------------*
215700030429     C*?CERTAR - CERCA LA TARIFFA
215800940908     C*------------------------------------------------------------------------*
215900940908     C     CERTAR        BEGSR
216000940908     C*
216100940908     C* IMPOSTO CAMPI LAVORO
216200940908     C                   MOVEL     'N'           WFLTT             1
216300940908     C                   MOVEL     *BLANKS       WRIF
216400940908     C                   Z-ADD     WDDSP         WDSP
216500940908     C                   MOVEL     *BLANKS       WTT1              3
216600940908     C                   MOVEL     *BLANKS       WTT2              3
216700950519     C                   MOVEL     *BLANKS       WCTSM             2
216800940908     C                   MOVEL     'S'           WMSGMT            1
216900941107     C                   Z-ADD     *ZEROS        WPTAS            15 3
217000030423     C                   Z-ADD     *ZEROS        WPTASR           15 3
217100040209     C                   Z-ADD     *ZEROS        WPTASR2          15 3
217200030423     C*
217300030423     C* calcola il peso tassabile da ricercare nello scaglione
217400030423     C     VSPS(V)       IFGT      *ZEROS
217500030423     C                   EXSR      CALPS
217600040209     C                   Z-ADD     WPTASR2       WQTA
217700030423     C                   ELSE
217800940908     C*
217900940908     C* CERCA LA QTA DA RICERCARE NELLO SCAGLIONE
218000940908     C                   Z-ADD     *ZEROS        WQTA
218100940908     C                   Z-ADD     1             I
218200940908     C     VTIV(V)       LOOKUP    C2R(I)                                 98
218300940908     C     *IN98         IFEQ      '0'
218400940908     C                   MOVEL     MSG(20)       $MSG
218500940908     C                   EXSR      SCRERR
218600940908     C                   ELSE
218700940908     C                   Z-ADD     Q2R(I)        WQTA
218800940908     C                   END
218900030423     C                   ENDIF
219000940908     C*
219100940908     C* IMPOSTO IL FLAG ITALIA/ESTERO
219200971211     C                   Z-ADD     W$LNP         F
219300111011     C                   Z-ADD     W$lna         F1                4 0
219400020805     C     FIE(F)        IFEQ      'E'
219500020805     C     FIE(F1)       OREQ      'E'
219600940908     C                   MOVEL     'E'           WIE               1
219700940908     C                   ELSE
219800940908     C                   MOVEL     'I'           WIE
219900940908     C                   ENDIF
220000940908     C*
220100940908     C* SE RICHIESTO, TENTATIVO PER CLIENTE
220200940908     C     WFLTT         IFEQ      'N'
220300940908     C     WIE           ANDEQ     'I'
220400940908     C     VRKI(V)       ANDNE     *BLANKS
220500940908     C                   MOVEL     VRKI(V)       WRIF
220600940908     C                   EXSR      CERCLI
220700940908     C                   ENDIF
220800940908     C     WFLTT         IFEQ      'N'
220900940908     C     WIE           ANDEQ     'E'
221000940908     C     VRKE(V)       ANDNE     *BLANKS
221100940908     C                   MOVEL     VRKE(V)       WRIF
221200940908     C                   EXSR      CERCLI
221300940908     C                   ENDIF
221400940908     C*
221500940908     C* SE RICHIESTO, TENTATIVO PER CAP
221600940908     C     WFLTT         IFEQ      'N'
221700940908     C     WIE           ANDEQ     'I'
221800940908     C     VRPI(V)       ANDNE     *BLANKS
221900940908     C                   MOVEL     VRPI(V)       WRIF
222000940908     C                   EXSR      CERCAP
222100940908     C                   ENDIF
222200940908     C     WFLTT         IFEQ      'N'
222300940908     C     WIE           ANDEQ     'E'
222400940908     C     VRPE(V)       ANDNE     *BLANKS
222500940908     C                   MOVEL     VRPE(V)       WRIF
222600940908     C                   EXSR      CERCAP
222700940908     C                   ENDIF
222800940908     C*
222900940908     C* SE RICHIESTO, TENTATIVO PER CODICE TASSAZIONE
223000940908     C     WFLTT         IFEQ      'N'
223100940908     C     WIE           ANDEQ     'I'
223200940908     C     VRTI(V)       ANDNE     *BLANKS
223300940908     C                   MOVEL     VRTI(V)       WRIF
223400940908     C                   EXSR      CERTAS
223500940908     C                   ENDIF
223600940908     C     WFLTT         IFEQ      'N'
223700940908     C     WIE           ANDEQ     'E'
223800940908     C     VRTE(V)       ANDNE     *BLANKS
223900940908     C                   MOVEL     VRTE(V)       WRIF
224000940908     C                   EXSR      CERTAS
224100940908     C                   ENDIF
224200940908     C*
224300940908     C* SE RICHIESTO, TENTATIVO PER CODICE FILIALE
224400940908     C     WFLTT         IFEQ      'N'
224500940908     C     WIE           ANDEQ     'I'
224600940908     C     VRFI(V)       ANDNE     *BLANKS
224700940908     C                   MOVEL     VRFI(V)       WRIF
224800940908     C                   EXSR      CERFIL
224900940908     C                   ENDIF
225000940908     C     WFLTT         IFEQ      'N'
225100940908     C     WIE           ANDEQ     'E'
225200940908     C     VRFE(V)       ANDNE     *BLANKS
225300940908     C                   MOVEL     VRFE(V)       WRIF
225400940908     C                   EXSR      CERFIL
225500940908     C                   ENDIF
225600940908     C*
225700940908     C* SE TARIFFA NON ANCORA TROVATA CERCA TARIFFA DI CARTELLO
225800940908     C     WFLTT         IFEQ      'N'
225900940908     C                   EXSR      CERCAR
226000940908     C                   ENDIF
226100040903     c* se ho manca tariffa per la voce 167 xchè peso tassabile > 2,5 kg
226200040903     c* disattivo tale voce e lascio la 168 e 169 e viceversa
226300040903     c                   if        wvoc = 167
226400040903     c                   if        wfltt = 'N' and WQTA > §DFEPKGD
226500040903     c                   movel     'N'           wmsgmt
226600040903     c                   movel     'N'           vc(167)
226700040903     c                   else
226800040903     c                   movel     'N'           vc(168)
226900040903     c                   movel     'N'           vc(169)
227000040903     c                   end
227100040903     c                   end
227200940908     C*
227300940908     C* CONTROLLO MANCA TARIFFA
227400940908     C     WFLTT         IFEQ      'N'
227500940908     C     WMSGMT        IFEQ      'S'
227600940908     C                   MOVEL     MSG(21)       DSMSG
227700940908     C                   MOVE      WVOC          DSVOCE
227800940908     C                   MOVE      WTT1          DSTT1
227900940908     C                   MOVE      WTT2          DSTT2
228000950519     C                   MOVE      WCTSM         DSCTS
228100940908     C                   MOVEL     DSMSG         $MSG
228200940908     C                   EXSR      SCRERR
228300940908     C                   ENDIF
228400940908     C                   ELSE
228500940908     C                   MOVEL     'S'           WFLTT
228600940908     C                   END
228700940908     C*
228800940908     C                   ENDSR
228900940803     C*------------------------------------------------------------------------*
229000940803     C*? CERCLI - CERCA LA TARIFFA PER CLIENTE
229100940803     C*------------------------------------------------------------------------*
229200940608     C     CERCLI        BEGSR
229300940608     C*
229400940630     C                   MOVEL     'K'           WTTV
229500940630     C                   Z-ADD     *ZEROS        WFT1
229600940630     C                   Z-ADD     *ZEROS        WFT2
229700940630     C                   Z-ADD     *ZEROS        WKSC
229800940701     C                   Z-ADD     *ZEROS        WCTR
229900940701     C                   MOVEL     *BLANKS       WCTS
230000940701     C                   Z-ADD     *ZEROS        WCAP
230100940826     C     WRIF          IFEQ      'KK'
230200940829     C                   Z-ADD     W$KSC         WKSC
230300940701     C                   Z-ADD     D$DCTR        WCTR
230400940826     C                   ENDIF
230500940826     C     WRIF          IFEQ      'KM'
230600941201     C     D$DTPO        IFEQ      'F'
230700940829     C     W$CCM         IFNE      *ZEROS
230800940829     C                   Z-ADD     W$CCM         WKSC
230900940826     C                   ELSE
231000940829     C                   Z-ADD     W$KSC         WKSC
231100940826     C                   ENDIF
231200940826     C                   ENDIF
231300941201     C     D$DTPO        IFEQ      'A'
231400940829     C                   Z-ADD     W$CCM         WKSC
231500940826     C                   ENDIF
231600940701     C                   Z-ADD     D$DCTR        WCTR
231700940826     C                   ENDIF
231800940701     C*
231900940701     C* LANCIA GESTIONE TARIFFE
232000940902     C     WKSC          IFNE      DEPKSC
232100940902     C                   Z-ADD     WKSC          DEPKSC            7 0
232200940830     C                   EXSR      CHKKSC
232300940902     C                   ENDIF
232400940908     C     WKSCOK        IFEQ      'S'
232500941107     C                   EXSR      GESEVD
232600940908     C                   ENDIF
232700940608     C*
232800940719     C* SE TARIFFA BOLLA NON E' STATA TROVATA PROVO CON TARIFFA = 999 (TUTTE)
232900940719     C     WFLTT         IFEQ      'N'
233000940829     C                   Z-ADD     W$KSC         WKSC
233100940719     C                   Z-ADD     999           WCTR
233200940908     C     WKSCOK        IFEQ      'S'
233300940722     C                   EXSR      GESEVD
233400940908     C                   ENDIF
233500940830     C                   ENDIF
233600940719     C*
233700940608     C                   ENDSR
233800940803     C*------------------------------------------------------------------------*
233900940803     C*? CERCAP - CERCA LA TARIFFA PER CAP
234000940803     C*------------------------------------------------------------------------*
234100940701     C     CERCAP        BEGSR
234200940701     C*
234300940701     C                   MOVEL     'P'           WTTV
234400940701     C                   Z-ADD     *ZEROS        WFT1
234500940701     C                   Z-ADD     *ZEROS        WFT2
234600940701     C                   Z-ADD     *ZEROS        WKSC
234700940701     C                   Z-ADD     *ZEROS        WCTR
234800940701     C                   MOVEL     *BLANKS       WCTS
234900941201     C                   Z-ADD     *ZEROS        WCAP
235000940701     C     WRIF          IFEQ      'PP'
235100941201     C                   MOVE      D$DCAD        WCAP
235200940701     C                   ENDIF
235300940701     C     WRIF          IFEQ      'PA'
235400941201     C                   MOVE      D$DCAD        WCAP
235500940701     C                   ENDIF
235600940701     C*
235700940701     C* LANCIA GESTIONE TARIFFE
235800940722     C                   EXSR      GESEVD
235900940701     C*
236000940701     C                   ENDSR
236100940803     C*------------------------------------------------------------------------*
236200940803     C*? CERTAS - CERCA LA TARIFFA PER CODICE TASSAZIONE
236300940803     C*------------------------------------------------------------------------*
236400940701     C     CERTAS        BEGSR
236500940701     C*
236600940701     C                   MOVEL     'T'           WTTV
236700940701     C                   Z-ADD     *ZEROS        WFT1
236800940701     C                   Z-ADD     *ZEROS        WFT2
236900940701     C                   Z-ADD     *ZEROS        WKSC
237000940701     C                   Z-ADD     *ZEROS        WCTR
237100940701     C                   MOVEL     *BLANKS       WCTS
237200940701     C                   Z-ADD     *ZEROS        WCAP
237300940701     C     WRIF          IFEQ      'SP'
237400940829     C                   MOVEL     W$MCT         WCTS
237500940701     C                   ENDIF
237600940701     C     WRIF          IFEQ      'SA'
237700940721     C                   MOVEL     D$DCTS        WCTS
237800940701     C                   ENDIF
237900940701     C*
238000940701     C* LANCIA GESTIONE TARIFFE
238100940722     C                   EXSR      GESEVD
238200950519     C*
238300950519     C* CAMPI PER MESSAGGI
238400950519     C                   MOVE      WCTS          WCTSM
238500940701     C*
238600940701     C                   ENDSR
238700940803     C*------------------------------------------------------------------------*
238800940803     C*? CERFIL - CERCA LA TARIFFA PER FILIALE/TERMINAL
238900940803     C*------------------------------------------------------------------------*
239000940701     C     CERFIL        BEGSR
239100940701     C*
239200940701     C                   MOVEL     'F'           WTTV
239300940701     C                   Z-ADD     *ZEROS        WFT1
239400940701     C                   Z-ADD     *ZEROS        WFT2
239500940701     C                   Z-ADD     *ZEROS        WKSC
239600940701     C                   Z-ADD     *ZEROS        WCTR
239700940701     C                   MOVEL     *BLANKS       WCTS
239800940701     C                   Z-ADD     *ZEROS        WCAP
239900940701     C     WRIF          IFEQ      'LP'
240000940829     C                   Z-ADD     W$LNP         WFT1
240100940701     C                   ENDIF
240200940701     C     WRIF          IFEQ      'LT'
240300940829     C                   Z-ADD     W$LNP         WFT1
240400940829     C                   Z-ADD     W$TFP         WFT2
240500940701     C                   ENDIF
240600940701     C     WRIF          IFEQ      'TP'
240700940829     C                   Z-ADD     W$TFP         WFT1
240800940701     C                   ENDIF
240900940701     C     WRIF          IFEQ      'TT'
241000940829     C                   Z-ADD     W$TFP         WFT1
241100940826     C                   Z-ADD     D$DTFA        WFT2
241200940701     C                   ENDIF
241300940701     C     WRIF          IFEQ      'T1'
241400040524     C                   Z-ADD     D$Dar5flp     WFT1
241500940701     C                   ENDIF
241600940701     C     WRIF          IFEQ      'T2'
241700040524     C                   Z-ADD     D$Dar5fl2     WFT1
241800940701     C                   ENDIF
241900040524     C     WRIF          IFEQ      'T3'
242000040524     C                   Z-ADD     D$Dar5fl3     WFT1
242100040524     C                   ENDIF
242200040524     C     WRIF          IFEQ      'T4'
242300040524     C                   Z-ADD     D$Dar5fl4     WFT1
242400040524     C                   ENDIF
242500040524     C     WRIF          IFEQ      'T5'
242600040524     C                   Z-ADD     D$Dar5fl5     WFT1
242700040524     C                   ENDIF
242800940701     C     WRIF          IFEQ      'TA'
242900940701     C                   Z-ADD     D$DTFA        WFT1
243000940701     C                   ENDIF
243100940701     C     WRIF          IFEQ      'TL'
243200940701     C                   Z-ADD     D$DTFA        WFT1
243300111011     C                   Z-ADD     W$lna         WFT2
243400940701     C                   ENDIF
243500940701     C     WRIF          IFEQ      'LA'
243600111011     C                   Z-ADD     W$lna         WFT1
243700940701     C                   ENDIF
243800940902     C*
243900940902     C* LANCIA GESTIONE TARIFFE
244000940902     C                   MOVEL     D$DKSC        WFILX             3
244100940902     C     WFILX         IFNE      DEPFIL
244200940902     C                   MOVE      WFILX         DEPFIL            3
244300940902     C                   MOVE      WFILX         WFIL              3 0
244400940902     C                   EXSR      CARFIL
244500940902     C                   ENDIF
244600940722     C                   EXSR      GESEVD
244700940701     C*
244800940908     C* SE BOLLA FIGLIA E TARIFFA TERMINAL/TERMINAL E TARIFFA NON TROVATA
244900940908     C* ----> NON EMETTO MESSAGGIO 'TARIFFA NON TROVATA'
245000950905     C     D$DFIG        IFEQ      'S'
245100940908     C     WRIF          ANDEQ     'TT'
245200940908     C     WFLTT         ANDEQ     'N'
245300940908     C                   MOVEL     'N'           WMSGMT
245400940908     C                   ENDIF
245500940908     C*
245600940908     C* CAMPI PER MESSAGGI
245700940830     C                   MOVE      WFT1          WTT1
245800940830     C                   MOVE      WFT2          WTT2
245900940830     C*
246000940701     C                   ENDSR
246100940803     C*------------------------------------------------------------------------*
246200940803     C*? CERCAR - CERCA LA TARIFFA DI CARTELLO
246300940803     C*------------------------------------------------------------------------*
246400940701     C     CERCAR        BEGSR
246500940701     C*
246600940701     C                   MOVEL     'C'           WTTV
246700940701     C                   Z-ADD     *ZEROS        WFT1
246800940701     C                   Z-ADD     *ZEROS        WFT2
246900940701     C                   Z-ADD     *ZEROS        WKSC
247000940701     C                   Z-ADD     *ZEROS        WCTR
247100940701     C                   MOVEL     *BLANKS       WCTS
247200940701     C                   Z-ADD     *ZEROS        WCAP
247300940701     C*
247400940701     C* LANCIA GESTIONE TARIFFE
247500940722     C                   EXSR      GESEVD
247600940701     C*
247700940701     C                   ENDSR
247800940704     C*------------------------------------------------------------------------*
247900940704     C*? GESEVD - GESTIONE FILE ECEVD (TARIFFE)
248000940704     C*------------------------------------------------------------------------*
248100940704     C     GESEVD        BEGSR
248200940801     C*
248300940801     C* SE TIPO SVILUPPO PESO TASSABILE 2=MIGLIORE REALE/TASS. O 3=MINORE REA/TA
248400940801     C* CERCO IL RAPP PESO/VOL PRIMA DI ARROTONDARE
248500961202     C     VSPT(V)       IFNE      *BLANKS
248600961202     C*
248700961202     C                   Z-ADD     *ZEROS        WQTAPT
248800940704     C*
248900940704     C* CERCA LO SCAGLIONE CON LA QTA DELLA BOLLA
249000940704     C                   EXSR      SETEVD
249100940704     C*
249200940707     C* SE IL RAPP. PESO/VOL MAGGIORE DI ZERO E VOLUME MAGGIORE DI ZERO
249300940707     C* CERCO SCAGLIONE CON PESO TASSABILE
249400940801     C     EVDRPV        IFNE      *ZEROS
249500941201     C     D$DVLF        ANDNE     *ZEROS
249600940707     C* CALCOLO PESO TASSABILE
249700940801     C                   EXSR      CALPT
249800020102     C                   Z-ADD     WQTA          WQTAPT           19 7
249900940801     C                   ENDIF
250000940801     C*
250100940801     C                   ENDIF
250200940707     C*
250300940803     C* CONTROLLA ARROTONDAMENTO
250400940803     C                   EXSR      ARROTO
250500940729     C*
250600940801     C* CERCA LO SCAGLIONE GIUSTO IN TARIFFA
250700940707     C                   EXSR      SETEVD
250800940704     C*
250900940704     C                   ENDSR
251000940701     C*------------------------------------------------------------------------*
251100940701     C*? SETEVD - POSIZIONAMENTO E PRIMA LETTURA FILE TARIFFE
251200940701     C*------------------------------------------------------------------------*
251300940701     C     SETEVD        BEGSR
251400940830     C*
251500940908     C                   MOVEL     'N'           WSETLL            1
251600940701     C                   MOVEL     '0'           FINEVD            1
251700940829     C*
251800940829     C* SE TARIFFA DI CARTELLO CONTROLLO PRIMA SE CARICATA IN MEMORIA
251900940829IF  1C     WTTV          IFEQ      'C'
252000940829     C                   EXSR      CHKCAR
252100940830E   1C                   ENDIF
252200940902     C*
252300940902     C* SE TARIFFA DI FILIALE CONTROLLO PRIMA SE CARICATA IN MEMORIA
252400940902IF  1C     WTTV          IFEQ      'F'
252500940902     C                   EXSR      CHKFIL
252600940902E   1C                   ENDIF
252700940831     C*
252800940831     C* SE TARIFFA CLIENTE CONTROLLO PRIMA SE CARICATA IN MEMORIA
252900940831IF  1C     WTTV          IFEQ      'K'
253000940831     C                   EXSR      CHKCLI
253100940831E   1C                   ENDIF
253200940829     C*
253300940907     C* SE TARIFFA NON TROVATA IN MEMORIA LEGGO DAL FILE
253400940908IF  1C     WSETLL        IFEQ      'N'
253500940829     C*
253600940701     C* POSIZIONAMENTO E PRIMA LETTURA
253700940701     C                   Z-ADD     WVOC          EVDVOC
253800940701     C                   Z-ADD     WPRG          EVDPRG
253900940701     C                   MOVEL     WTTV          EVDTTV
254000940701     C                   Z-ADD     WFT1          EVDFT1
254100940701     C                   Z-ADD     WFT2          EVDFT2
254200940701     C                   Z-ADD     WKSC          EVDCLI
254300940701     C                   Z-ADD     WCTR          EVDCTR
254400940701     C                   MOVEL     WCTS          EVDCTS
254500940701     C                   Z-ADD     WCAP          EVDCAP
254600940802     C                   Z-ADD     *ZEROS        EVDDDT
254700940802     C                   Z-ADD     *ZEROS        EVDSGI
254800940701     C     KEYEVD        SETLL     ECEVD01L                           99
254900940701     C*
255000940830IF  2C     *IN99         IFEQ      *ON
255100940701     C                   MOVEL     '1'           FINEVD
255200940830X   2C                   ELSE
255300940701     C                   EXSR      LETEVD
255400940830E   2C                   ENDIF
255500940701     C*
255600940720     C* CONTROLLO MANCA TARIFFA
255700940830IF  2C     WRECOK        IFEQ      'N'
255800991112     C                   MOVEL     'N'           WFLTT
255900940830X   2C                   ELSE
256000940701     C                   MOVEL     'S'           WFLTT
256100940830E   2C                   ENDIF
256200940829     C*
256300940830E   1C                   ENDIF
256400940701     C*
256500940701     C                   ENDSR
256600940701     C*------------------------------------------------------------------------*
256700940701     C*? LETEVD - LETTURA PROSSIMO RECORD TARIFFE
256800940830     C*------------------------------------------------------------------------*
256900940701     C     LETEVD        BEGSR
257000940701     C*
257100940802     C* LEGGO FINO A:
257200940701     C*     - FINE FILE O SUPERATO LIMITE FINALE
257300940701     C*     - TROVATO RECORD VALIDO
257400940701     C                   MOVEL     'N'           WRECOK
257500940701     C     FINEVD        DOWEQ     '0'
257600940701     C     WRECOK        ANDEQ     'N'
257700940701     C*
257800940701     C* LETTURE SUCCESSIVE ALLA PRIMA
257900940701     C                   READ      ECEVD01L                               99
258000940701     C*
258100940701     C     *IN99         IFEQ      '1'
258200940701     C     EVDVOC        ORNE      WVOC
258300940701     C     EVDPRG        ORNE      WPRG
258400940701     C     EVDTTV        ORNE      WTTV
258500940701     C     EVDFT1        ORNE      WFT1
258600940701     C     EVDFT2        ORNE      WFT2
258700940721     C     EVDCLI        ORNE      WKSC
258800940701     C     EVDCTR        ORNE      WCTR
258900940701     C     EVDCTS        ORNE      WCTS
259000940701     C     EVDCAP        ORNE      WCAP
259100940720     C                   MOVEL     '1'           FINEVD
259200940701     C                   ELSE
259300940701     C                   EXSR      CHKEVD
259400940701     C                   END
259500940701     C*
259600940701     C                   END
259700940701     C*
259800940701     C                   ENDSR
259900940830     C*------------------------------------------------------------------------*
260000940701     C*? CHKEVD - CONTROLLA VALIDITA' DELLA TARIFFA
260100940830     C*------------------------------------------------------------------------*
260200940701     C     CHKEVD        BEGSR
260300940701     C*
260400940701     C                   MOVEL     'S'           WRECOK            1
260500940701     C* ESCLUDO SE ANNULLATO
260600940701     C     EVDATB        IFNE      *BLANKS
260700940701     C                   MOVEL     'N'           WRECOK
260800940701     C                   END
260900940701     C* ESCLUDO SE DATA SPEDIZIONE FUORI DALLA DECORR/SCAD.
261000940701     C     WDSP          IFLT      EVDDDT
261100940701     C     WDSP          ORGT      EVDDST
261200940701     C                   MOVEL     'N'           WRECOK
261300940701     C                   END
261400011227     C*
261500011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
261600011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
261700011227     C                   Z-ADD     EVDSGI        W$SGI
261800011227     C                   Z-ADD     EVDSGF        W$SGF
261900011227IF  1C     WRECOK        IFEQ      'S'
262000011227IF  2C                   IF        WTIV = '21' OR
262100011227     C                             WTIV = '22' OR
262200011227     C                             WTIV = '23' OR
262300011227     C                             WTIV = '25'
262400011227IF  3C     WDIVAC        IFNE      WDIVCE
262500011227IF  4C     EVDSGI        IFNE      *ZEROS
262600011227     C                   Z-ADD     EVDSGI        §IMI
262700011227     C                   MOVEL     WDIVAC        §DVI
262800011227     C                   EXSR      CNVEUR
262900011227     C                   Z-ADD     §IMO          W$SGI
263000011227E   4C                   ENDIF
263100011227IF  4C     EVDSGF        IFNE      *ZEROS
263200011227     C                   Z-ADD     EVDSGF        §IMI
263300011227     C                   MOVEL     WDIVAC        §DVI
263400011227     C                   EXSR      CNVEUR
263500011227     C                   Z-ADD     §IMO          W$SGF
263600011227E   4C                   ENDIF
263700011227E   3C                   ENDIF
263800011227E   2C                   ENDIF
263900011227E   1C                   ENDIF
264000011227     C*
264100940701     C* ESCLUDO SE QTA NON COMPRESA NELLO SCAGLIONE
264200000306IF  1C     WTIV          IFNE      *BLANKS
264300011227     C     WQTA          IFLT      W$SGI
264400011227     C     WQTA          ORGT      W$SGF
264500940701     C                   MOVEL     'N'           WRECOK
264600940701     C                   END
264700000306E   1C                   ENDIF
264800940701     C*
264900940701     C                   ENDSR
265000940830     C*------------------------------------------------------------------------*
265100940830     C*? CHKCAR - CONTROLLA SE TARIFFA DI CARTELLO CARICATA IN MEMORIA
265200940830     C*------------------------------------------------------------------------*
265300940830     C     CHKCAR        BEGSR
265400940830     C*
265500940830     C* CONTROLLA ESISTENZA TARIFFA DI CARTELLO IN LINEA FRA QUELLE CARICATE
265600940830     C                   Z-ADD     1             I
265700940830DO  1C     I             DOWLE     WULTCA
265800940908     C     WSETLL        ANDEQ     'N'
265900940830     C*
266000940830     C* CONTROLLI DI VALIDITA'
266100941213IF  2C     WVOC          IFEQ      CVOC(I)
266200941213     C*
266300941213IF  3C     WPRG          IFEQ      CPRG(I)
266400940830     C     WFT1          ANDEQ     CFT1(I)
266500940830     C     WFT2          ANDEQ     CFT2(I)
266600940830     C     WKSC          ANDEQ     CCLI(I)
266700940830     C     WCTR          ANDEQ     CCTR(I)
266800940830     C     WCTS          ANDEQ     CCTS(I)
266900940830     C     WCAP          ANDEQ     CCAP(I)
267000940830     C*
267100941213IF  4C     WDSP          IFGE      CDDT(I)
267200940830     C     WDSP          ANDLE     CDST(I)
267300000306     C*
267400011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
267500011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
267600011227     C                   Z-ADD     CSGI(I)       W$SGI
267700011227     C                   Z-ADD     CSGF(I)       W$SGF
267800011227IF  5C                   IF        WTIV = '21' OR
267900011227     C                             WTIV = '22' OR
268000011227     C                             WTIV = '23' OR
268100011227     C                             WTIV = '25'
268200011227IF  6C     WDIVAC        IFNE      WDIVCE
268300011227IF  7C     CSGI(I)       IFNE      *ZEROS
268400011227     C                   Z-ADD     CSGI(I)       §IMI
268500011227     C                   MOVEL     WDIVAC        §DVI
268600011227     C                   EXSR      CNVEUR
268700011227     C                   Z-ADD     §IMO          W$SGI
268800011227E   7C                   ENDIF
268900011227IF  7C     CSGF(I)       IFNE      *ZEROS
269000011227     C                   Z-ADD     CSGF(I)       §IMI
269100011227     C                   MOVEL     WDIVAC        §DVI
269200011227     C                   EXSR      CNVEUR
269300011227     C                   Z-ADD     §IMO          W$SGF
269400011227E   7C                   ENDIF
269500011227E   6C                   ENDIF
269600011227E   5C                   ENDIF
269700011227     c*
269800000306IF  5C     WTIV          IFEQ      *BLANKS
269900000306     C     WTIV          ORNE      *BLANKS
270000011227     C     WQTA          ANDGE     W$SGI
270100011227     C     WQTA          ANDLE     W$SGF
270200940830     C*
270300940830     C* CARTELLO TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
270400940908     C                   MOVEL     'S'           WSETLL
270500940830     C                   MOVEL     'S'           WFLTT
270600941229     C****+++++++++++++++++ADD  1         WMECAR  90
270700940830     C*
270800940830     C                   Z-ADD     CVOC(I)       EVDVOC
270900940830     C                   Z-ADD     CPRG(I)       EVDPRG
271000940830     C                   MOVEL     'C'           EVDTTV
271100940830     C                   Z-ADD     CFT1(I)       EVDFT1
271200940830     C                   Z-ADD     CFT2(I)       EVDFT2
271300940830     C                   Z-ADD     CCLI(I)       EVDCLI
271400940830     C                   Z-ADD     CCTR(I)       EVDCTR
271500940830     C                   MOVEL     CCTS(I)       EVDCTS
271600940830     C                   Z-ADD     CCAP(I)       EVDCAP
271700940830     C                   Z-ADD     CDDT(I)       EVDDDT
271800940830     C                   Z-ADD     CDST(I)       EVDDST
271900940830     C                   Z-ADD     CSGI(I)       EVDSGI
272000940830     C                   Z-ADD     CSGF(I)       EVDSGF
272100940830     C                   Z-ADD     CITR(I)       EVDITR
272200940830     C                   Z-ADD     CIMI(I)       EVDIMI
272300940830     C                   Z-ADD     CIMA(I)       EVDIMA
272400940830     C                   MOVEL     CSEG(I)       EVDSEG
272500940830     C                   Z-ADD     CPER(I)       EVDPER
272600940830     C                   Z-ADD     CRPV(I)       EVDRPV
272700940830     C*
272800941213E   5C                   ENDIF
272900940830E   4C                   ENDIF
273000940830E   3C                   ENDIF
273100940830E   2C                   ENDIF
273200940830     C*
273300940830     C                   ADD       1             I
273400940830E   1C                   ENDDO
273500940907     C*
273600940908     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
273700940908     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
273800940908IF  1C     WSETLL        IFEQ      'N'
273900940908     C     WULTCA        ANDLT     200
274000940908     C                   MOVEL     'S'           WSETLL
274100940908E   1C                   ENDIF
274200940907     C*
274300940907     C                   ENDSR
274400940902     C*------------------------------------------------------------------------*
274500940902     C*? CARFIL - CARICA TUTTE LE TARIFFE DELLA FILIALE
274600940902     C*------------------------------------------------------------------------*
274700940902     C     CARFIL        BEGSR
274800940902     C*
274900940902     C* POSIZIONAMENTO E PRIMA LETTURA
275000940902     C     WFIL          SETLL     ECEVD03L
275100940902     C                   READ      ECEVD03L                               99
275200940902     C*
275300940902     C* FINO A FINE FILE
275400940902     C                   Z-ADD     *ZEROS        I
275500940902DO  1C     *IN99         DOWEQ     *OFF
275600940902     C     EVDFT1        ANDEQ     WFIL
275700940902IF  3C     EVDATB        IFEQ      *BLANKS
275800940902     C                   ADD       1             I
275900940902     C                   Z-ADD     EVDVOC        FVOC(I)
276000940902     C                   Z-ADD     EVDPRG        FPRG(I)
276100940902     C                   Z-ADD     EVDFT1        FFT1(I)
276200940902     C                   Z-ADD     EVDFT2        FFT2(I)
276300940902     C                   Z-ADD     EVDCLI        FCLI(I)
276400940902     C                   Z-ADD     EVDCTR        FCTR(I)
276500940902     C                   MOVEL     EVDCTS        FCTS(I)
276600940902     C                   Z-ADD     EVDCAP        FCAP(I)
276700940902     C                   Z-ADD     EVDDDT        FDDT(I)
276800940902     C                   Z-ADD     EVDDST        FDST(I)
276900940902     C                   Z-ADD     EVDSGI        FSGI(I)
277000940902     C                   Z-ADD     EVDSGF        FSGF(I)
277100940902     C                   Z-ADD     EVDITR        FITR(I)
277200940902     C                   Z-ADD     EVDIMI        FIMI(I)
277300940902     C                   Z-ADD     EVDIMA        FIMA(I)
277400940902     C                   MOVEL     EVDSEG        FSEG(I)
277500940902     C                   Z-ADD     EVDPER        FPER(I)
277600940902     C                   Z-ADD     EVDRPV        FRPV(I)
277700940902E   2C                   ENDIF
277800940902     C*
277900940902     C                   READ      ECEVD03L                               99
278000940902E   1C                   ENDDO
278100940902     C*
278200940902     C                   Z-ADD     I             WULTTF            6 0
278300940902     C*
278400940902     C                   ENDSR
278500940902     C*------------------------------------------------------------------------*
278600941213     C*? CHKFIL - CONTROLLA SE TARIFFA FILIALE GIA' IN MEMORIA
278700940902     C*------------------------------------------------------------------------*
278800940902     C     CHKFIL        BEGSR
278900940902     C*
279000941213     C* CONTROLLA ESISTENZA TARIFFA FILIALE FRA QUELLE CARICATE
279100940902     C                   Z-ADD     1             I
279200940908DO  1C     I             DOWLE     WULTTF
279300940908     C     WSETLL        ANDEQ     'N'
279400940902     C*
279500940902     C* CONTROLLI DI VALIDITA'
279600941213IF  2C     WVOC          IFEQ      FVOC(I)
279700941213     C*
279800941213IF  3C     WPRG          IFEQ      FPRG(I)
279900940902     C     WFT1          ANDEQ     FFT1(I)
280000940902     C     WFT2          ANDEQ     FFT2(I)
280100940902     C     WKSC          ANDEQ     FCLI(I)
280200940902     C     WCTR          ANDEQ     FCTR(I)
280300940902     C     WCTS          ANDEQ     FCTS(I)
280400940902     C     WCAP          ANDEQ     FCAP(I)
280500940902     C*
280600941213IF  4C     WDSP          IFGE      FDDT(I)
280700940902     C     WDSP          ANDLE     FDST(I)
280800011227     C*
280900011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
281000011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
281100011227     C                   Z-ADD     FSGI(I)       W$SGI
281200011227     C                   Z-ADD     FSGF(I)       W$SGF
281300011227IF  5C                   IF        WTIV = '21' OR
281400011227     C                             WTIV = '22' OR
281500011227     C                             WTIV = '23' OR
281600011227     C                             WTIV = '25'
281700011227IF  6C     WDIVAC        IFNE      WDIVCE
281800011227IF  7C     FSGI(I)       IFNE      *ZEROS
281900011227     C                   Z-ADD     FSGI(I)       §IMI
282000011227     C                   MOVEL     WDIVAC        §DVI
282100011227     C                   EXSR      CNVEUR
282200011227     C                   Z-ADD     §IMO          W$SGI
282300011227E   7C                   ENDIF
282400011227IF  7C     FSGF(I)       IFNE      *ZEROS
282500011227     C                   Z-ADD     FSGF(I)       §IMI
282600011227     C                   MOVEL     WDIVAC        §DVI
282700011227     C                   EXSR      CNVEUR
282800011227     C                   Z-ADD     §IMO          W$SGF
282900011227E   7C                   ENDIF
283000011227E   6C                   ENDIF
283100011227E   5C                   ENDIF
283200000306     C*
283300000306IF  5C     WTIV          IFEQ      *BLANKS
283400000306     C     WTIV          ORNE      *BLANKS
283500011227     C     WQTA          ANDGE     W$SGI
283600011227     C     WQTA          ANDLE     W$SGF
283700940902     C*
283800940902     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
283900940908     C                   MOVEL     'S'           WSETLL
284000940902     C                   MOVEL     'S'           WFLTT
284100940902     C*
284200940902     C                   Z-ADD     FVOC(I)       EVDVOC
284300940902     C                   Z-ADD     FPRG(I)       EVDPRG
284400940902     C                   MOVEL     'F'           EVDTTV
284500940902     C                   Z-ADD     FFT1(I)       EVDFT1
284600940902     C                   Z-ADD     FFT2(I)       EVDFT2
284700940902     C                   Z-ADD     FCLI(I)       EVDCLI
284800940902     C                   Z-ADD     FCTR(I)       EVDCTR
284900940902     C                   MOVEL     FCTS(I)       EVDCTS
285000940902     C                   Z-ADD     FCAP(I)       EVDCAP
285100940902     C                   Z-ADD     FDDT(I)       EVDDDT
285200940902     C                   Z-ADD     FDST(I)       EVDDST
285300940902     C                   Z-ADD     FSGI(I)       EVDSGI
285400940902     C                   Z-ADD     FSGF(I)       EVDSGF
285500940902     C                   Z-ADD     FITR(I)       EVDITR
285600940902     C                   Z-ADD     FIMI(I)       EVDIMI
285700940902     C                   Z-ADD     FIMA(I)       EVDIMA
285800940902     C                   MOVEL     FSEG(I)       EVDSEG
285900940902     C                   Z-ADD     FPER(I)       EVDPER
286000940902     C                   Z-ADD     FRPV(I)       EVDRPV
286100940902     C*
286200941213E   5C                   ENDIF
286300940902E   4C                   ENDIF
286400940902E   3C                   ENDIF
286500940902E   2C                   ENDIF
286600940902     C*
286700940902     C                   ADD       1             I
286800940902E   1C                   ENDDO
286900940902     C*
287000940902     C                   ENDSR
287100940907     C*------------------------------------------------------------------------*
287200940907     C*? CARCLI - CARICA LE TARIFFE DI UN CLIENTE
287300940907     C*------------------------------------------------------------------------*
287400940907     C     CARCLI        BEGSR
287500940907     C*
287600940907     C* POSIZIONAMENTO E PRIMA LETTURA
287700940907     C     WKSC          SETLL     ECEVD02L
287800940907     C                   READ      ECEVD02L                               99
287900940907     C*
288000940907     C* FINO A FINE FILE
288100940907     C                   Z-ADD     *ZEROS        I
288200940907DO  1C     *IN99         DOWEQ     *OFF
288300940907IF  2C     EVDCLI        ANDEQ     WKSC
288400940907     C     I             ANDLT     200
288500940907     C     EVDATB        IFEQ      *BLANKS
288600940907     C                   ADD       1             I
288700940907     C                   Z-ADD     EVDVOC        KVOC(I)
288800940907     C                   Z-ADD     EVDPRG        KPRG(I)
288900940907     C                   Z-ADD     EVDFT1        KFT1(I)
289000940907     C                   Z-ADD     EVDFT2        KFT2(I)
289100940907     C                   Z-ADD     EVDCLI        KCLI(I)
289200940907     C                   Z-ADD     EVDCTR        KCTR(I)
289300940907     C                   MOVEL     EVDCTS        KCTS(I)
289400940907     C                   Z-ADD     EVDCAP        KCAP(I)
289500940907     C                   Z-ADD     EVDDDT        KDDT(I)
289600940907     C                   Z-ADD     EVDDST        KDST(I)
289700940907     C                   Z-ADD     EVDSGI        KSGI(I)
289800940907     C                   Z-ADD     EVDSGF        KSGF(I)
289900940907     C                   Z-ADD     EVDITR        KITR(I)
290000940907     C                   Z-ADD     EVDIMI        KIMI(I)
290100940907     C                   Z-ADD     EVDIMA        KIMA(I)
290200940907     C                   MOVEL     EVDSEG        KSEG(I)
290300940907     C                   Z-ADD     EVDPER        KPER(I)
290400940907     C                   Z-ADD     EVDRPV        KRPV(I)
290500940907E   2C                   ENDIF
290600940907     C*
290700940907     C                   READ      ECEVD02L                               99
290800940907E   1C                   ENDDO
290900940907     C*
291000940907     C                   Z-ADD     I             WULTTK            6 0
291100940907     C*
291200940907     C                   ENDSR
291300940831     C*------------------------------------------------------------------------*
291400940902     C*? CHKCLI - CONTROLLA SE TARIFFA CLIENTE GIA' IN MEMORIA
291500940831     C*------------------------------------------------------------------------*
291600940831     C     CHKCLI        BEGSR
291700940831     C*
291800940831     C* CONTROLLA ESISTENZA TARIFFA CLIENTE FRA QUELLE CARICATE
291900940831     C                   Z-ADD     1             I
292000940831DO  1C     I             DOWLE     WULTTK
292100940908     C     WSETLL        ANDEQ     'N'
292200940831     C*
292300940831     C* CONTROLLI DI VALIDITA'
292400940831IF  2C     WVOC          IFEQ      KVOC(I)
292500941213     C*
292600941213IF  3C     WPRG          IFEQ      KPRG(I)
292700940831     C     WFT1          ANDEQ     KFT1(I)
292800940831     C     WFT2          ANDEQ     KFT2(I)
292900940831     C     WKSC          ANDEQ     KCLI(I)
293000940831     C     WCTR          ANDEQ     KCTR(I)
293100940831     C     WCTS          ANDEQ     KCTS(I)
293200940831     C     WCAP          ANDEQ     KCAP(I)
293300940831     C*
293400941213IF  4C     WDSP          IFGE      KDDT(I)
293500940831     C     WDSP          ANDLE     KDST(I)
293600011227     C*
293700011227     C* CONVERTE GLI SCAGLIONI -SE SCAGLIONI IMPORTO- QUANDO LA DIVISA DELLE
293800011227     C* ANAGRAFICHE DI C/E E' DIVERSA DALLA DIVISA DI OUTPUT C/E
293900011227     C                   Z-ADD     KSGI(I)       W$SGI
294000011227     C                   Z-ADD     KSGF(I)       W$SGF
294100011227IF  5C                   IF        WTIV = '21' OR
294200011227     C                             WTIV = '22' OR
294300011227     C                             WTIV = '23' OR
294400011227     C                             WTIV = '25'
294500011227IF  6C     WDIVAC        IFNE      WDIVCE
294600011227IF  7C     KSGI(I)       IFNE      *ZEROS
294700011227     C                   Z-ADD     KSGI(I)       §IMI
294800011227     C                   MOVEL     WDIVAC        §DVI
294900011227     C                   EXSR      CNVEUR
295000011227     C                   Z-ADD     §IMO          W$SGI
295100011227E   7C                   ENDIF
295200011227IF  7C     KSGF(I)       IFNE      *ZEROS
295300011227     C                   Z-ADD     KSGF(I)       §IMI
295400011227     C                   MOVEL     WDIVAC        §DVI
295500011227     C                   EXSR      CNVEUR
295600011227     C                   Z-ADD     §IMO          W$SGF
295700011227E   7C                   ENDIF
295800011227E   6C                   ENDIF
295900011227E   5C                   ENDIF
296000940831     C*
296100000306IF  5C     WTIV          IFEQ      *BLANKS
296200000306     C     WTIV          ORNE      *BLANKS
296300011227     C     WQTA          ANDGE     W$SGI
296400011227     C     WQTA          ANDLE     W$SGF
296500940831     C*
296600940902     C* TARIFFA TROVATA --> VALORIZZA I CAMPI DI ECEVD CON QUELLI DELLA SCHIERA
296700940908     C                   MOVEL     'S'           WSETLL
296800940831     C                   MOVEL     'S'           WFLTT
296900940831     C*
297000940831     C                   Z-ADD     KVOC(I)       EVDVOC
297100940831     C                   Z-ADD     KPRG(I)       EVDPRG
297200940831     C                   MOVEL     'K'           EVDTTV
297300940831     C                   Z-ADD     KFT1(I)       EVDFT1
297400940831     C                   Z-ADD     KFT2(I)       EVDFT2
297500940831     C                   Z-ADD     KCLI(I)       EVDCLI
297600940831     C                   Z-ADD     KCTR(I)       EVDCTR
297700940831     C                   MOVEL     KCTS(I)       EVDCTS
297800940831     C                   Z-ADD     KCAP(I)       EVDCAP
297900940831     C                   Z-ADD     KDDT(I)       EVDDDT
298000940831     C                   Z-ADD     KDST(I)       EVDDST
298100940831     C                   Z-ADD     KSGI(I)       EVDSGI
298200940831     C                   Z-ADD     KSGF(I)       EVDSGF
298300940831     C                   Z-ADD     KITR(I)       EVDITR
298400940831     C                   Z-ADD     KIMI(I)       EVDIMI
298500940831     C                   Z-ADD     KIMA(I)       EVDIMA
298600940831     C                   MOVEL     KSEG(I)       EVDSEG
298700940831     C                   Z-ADD     KPER(I)       EVDPER
298800940831     C                   Z-ADD     KRPV(I)       EVDRPV
298900940831     C*
299000941213E   5C                   ENDIF
299100941213E   4C                   ENDIF
299200940831E   3C                   ENDIF
299300940831E   2C                   ENDIF
299400940831     C*
299500940831     C                   ADD       1             I
299600940908E   1C                   ENDDO
299700940908     C*
299800940908     C* SE NON TROVO LA TARIFFA TRA QUELLE CARICATE IN MEMORIA E DA LEGGERE DAL
299900940908     C* FILE NON CE NE SONO PIU' PERCHE' LA SCHIERA NON E' COMPLETAMENTE PIENA
300000940908IF  1C     WSETLL        IFEQ      'N'
300100940908     C     WULTTK        ANDLT     200
300200940908     C                   MOVEL     'S'           WSETLL
300300940908E   1C                   ENDIF
300400940831     C*
300500940831     C                   ENDSR
300600940830     C*------------------------------------------------------------------------*
300700940830     C*? CHKKSC - CONTROLLA SE IL CLIENTE E' TRA QUELLI CON TARIFFA PARTICOLARE
300800940830     C*------------------------------------------------------------------------*
300900940830     C     CHKKSC        BEGSR
301000940830     C*
301100940831     C* CONTROLLA ESISTENZA CLIENTE TRA QUELLI CON TARIFFA PARTICOLARE
301200940830     C                   MOVEL     'N'           WKSCOK            1
301300940902     C                   Z-ADD     1             IK                9 0
301400940902DO  1C     IK            DOWLE     WULTKS
301500940907     C     WKSCOK        ANDEQ     'N'
301600940830     C*
301700940907     C* CONTROLLI DI VALIDITA' (CLIENTE FRA QUELLI CARICATI)
301800940902IF  2C     WKSC          IFEQ      KSC(IK)
301900940830     C                   MOVEL     'S'           WKSCOK
302000940831     C* CARICA TUTTE LE TARIFFE DEL CLIENTE
302100940831     C                   EXSR      CARCLI
302200940830E   2C                   ENDIF
302300940830     C*
302400940902     C                   ADD       1             IK
302500940830E   1C                   ENDDO
302600940908     C*
302700940830     C                   ENDSR
302800011221     C*------------------------------------------------------------------------*
302900011221     C*?CNVEUR - CONVERTE I CAMPI DA DIVISA ESTERA A LIRE
303000011221     C*------------------------------------------------------------------------*
303100011221     C     CNVEUR        BEGSR
303200011221     C*
303300011221     C* AZZERA VARIABILI DI LAVORO
303400011221     C                   Z-ADD     *ZEROS        §IMO
303500011221     C*
303600011221     C                   CLEAR                   YEUR
303700011221     C                   MOVEL     §DVI          YECDVI
303800011221     C                   Z-ADD     §IMI          YECIMI
303900011221     C                   MOVEL     WDIVCE        YECDVO
304000011221     C                   MOVEL     '3'           YECDCO
304100011221     C                   MOVEL     'H'           YECTAR
304200011221     C                   CALL      'YEURCO'
304300011221     C                   PARM                    YEUR
304400011221IF  1C     YECESI        IFEQ      ' '
304500011221     C                   Z-ADD     YECIMO        §IMO
304600011221X   1C                   ELSE
304700011221     C                   Z-ADD     §IMI          §IMO
304800011221E   1C                   ENDIF
304900011221     C*
305000011221     C                   ENDSR
305100940907     C*------------------------------------------------------------------------*
305200940907     C*? CARCAR - CARICA LE TARIFFE DI CARTELLO
305300940907     C*------------------------------------------------------------------------*
305400940907     C     CARCAR        BEGSR
305500940907     C*
305600940907     C* POSIZIONAMENTO E PRIMA LETTURA
305700940907     C                   MOVEL     'C'           EVDTTV
305800940907     C     EVDTTV        SETLL     ECEVD04L
305900940907     C                   READ      ECEVD04L                               99
306000940907     C*
306100940907     C* FINO A FINE FILE E TARIFFE DI CARTELLO
306200940907     C                   Z-ADD     *ZEROS        I
306300940907DO  1C     *IN99         DOWEQ     *OFF
306400940907     C     EVDTTV        ANDEQ     'C'
306500940907     C     I             ANDLT     200
306600940907IF  2C     EVDATB        IFEQ      *BLANKS
306700940907     C                   ADD       1             I
306800940907     C                   Z-ADD     EVDVOC        CVOC(I)
306900940907     C                   Z-ADD     EVDPRG        CPRG(I)
307000940907     C                   Z-ADD     EVDFT1        CFT1(I)
307100940907     C                   Z-ADD     EVDFT2        CFT2(I)
307200940907     C                   Z-ADD     EVDCLI        CCLI(I)
307300940907     C                   Z-ADD     EVDCTR        CCTR(I)
307400940907     C                   MOVEL     EVDCTS        CCTS(I)
307500940907     C                   Z-ADD     EVDCAP        CCAP(I)
307600940907     C                   Z-ADD     EVDDDT        CDDT(I)
307700940907     C                   Z-ADD     EVDDST        CDST(I)
307800940907     C                   Z-ADD     EVDSGI        CSGI(I)
307900940907     C                   Z-ADD     EVDSGF        CSGF(I)
308000940907     C                   Z-ADD     EVDITR        CITR(I)
308100940907     C                   Z-ADD     EVDIMI        CIMI(I)
308200940907     C                   Z-ADD     EVDIMA        CIMA(I)
308300940907     C                   MOVEL     EVDSEG        CSEG(I)
308400940907     C                   Z-ADD     EVDPER        CPER(I)
308500940907     C                   Z-ADD     EVDRPV        CRPV(I)
308600940907E   2C                   ENDIF
308700940907     C*
308800940907     C                   READ      ECEVD04L                               99
308900940907E   1C                   ENDDO
309000940907     C*
309100940907     C                   Z-ADD     I             WULTCA            6 0
309200940907     C*
309300940907     C                   ENDSR
309400940907     C*------------------------------------------------------------------------*
309500940907     C*? CARKSC - CARICA I CLIENTI CON TARIFFE PARTICOLARI
309600940907     C*------------------------------------------------------------------------*
309700940907     C     CARKSC        BEGSR
309800940907     C*
309900940907     C* POSIZIONAMENTO E PRIMA LETTURA
310000940907     C                   MOVEL     'K'           EVDTTV
310100940907     C     EVDTTV        SETLL     ECEVD04L
310200940907     C                   READ      ECEVD04L                               99
310300940907     C*
310400940907     C* FINO A FINE FILE E TARIFFE DI CLIENTE
310500940907     C                   Z-ADD     *ZEROS        Y                 6 0
310600940907DO  1C     *IN99         DOWEQ     *OFF
310700940907     C     EVDTTV        ANDEQ     'K'
310800070626     C     Y             ANDLT     5000
310900940907IF  2C     EVDATB        IFEQ      *BLANKS
311000940907     C                   MOVEL     'S'           WFKSC             1
311100940907IF  3C     Y             IFGT      *ZEROS
311200940907     C                   Z-ADD     *ZEROS        YY                6 0
311300940907DO  4C                   DO        Y             YY
311400940907IF  5C     EVDCLI        IFEQ      KSC(YY)
311500940907     C                   MOVEL     'N'           WFKSC
311600940907E   5C                   ENDIF
311700940907E   4C                   ENDDO
311800940907E   3C                   ENDIF
311900940907IF  3C     WFKSC         IFEQ      'S'
312000940907     C                   ADD       1             Y
312100940907     C                   Z-ADD     EVDCLI        KSC(Y)
312200940907E   3C                   ENDIF
312300940907E   2C                   ENDIF
312400940907     C*
312500940907     C                   READ      ECEVD04L                               99
312600940907E   1C                   ENDDO
312700940907     C*
312800940907     C                   Z-ADD     Y             WULTKS            6 0
312900940907     C*
313000940907     C                   ENDSR
313100940907     C*------------------------------------------------------------------------*
313200940907     C*? CARMAG - CARICA LE MAGGIORAZIONI
313300940907     C*------------------------------------------------------------------------*
313400940907     C     CARMAG        BEGSR
313500940907     C*
313600940907     C* POSIZIONAMENTO E PRIMA LETTURA
313700940907     C     *LOVAL        SETLL     ECEVM01L
313800940907     C                   READ      ECEVM01L                               99
313900940907     C*
314000940907     C* FINO A FINE FILE E MAGGIORAZIONI
314100940907     C                   Z-ADD     *ZEROS        I
314200940907DO  1C     *IN99         DOWEQ     *OFF
314300041118     C     I             ANDLT     600
314400940907IF  2C     EVMATB        IFEQ      *BLANKS
314500940907     C                   ADD       1             I
314600940907     C                   Z-ADD     EVMVOM        MVOM(I)
314700940907     C                   Z-ADD     EVMPRM        MPRM(I)
314800940907     C                   Z-ADD     EVMVOC        MVOC(I)
314900940907     C                   Z-ADD     EVMPRG        MPRG(I)
315000940907E   2C                   ENDIF
315100940907     C*
315200940907     C                   READ      ECEVM01L                               99
315300940907E   1C                   ENDDO
315400940907     C*
315500940907     C                   Z-ADD     I             WULTMA            6 0
315600940907     C*
315700940907     C                   ENDSR
315800940803     C*------------------------------------------------------------------------*
315900940803     C*? CARTAB - CARICA LE TABELLE
316000940803     C*------------------------------------------------------------------------*
316100930414     C     CARTAB        BEGSR
316200011221     C*
316300011221     C* REPERISCE LA DATA CORRENTE
316400011221     C                   TIME                    N14              14 0
316500011221     C                   MOVE      N14           N8                8 0
316600011221     C                   Z-ADD     N8            G08DAT
316700011221     C                   Z-ADD     *ZEROS        G08INV
316800011221     C                   MOVEL     '0'           G08ERR
316900011221     C                   CALL      'XSRDA8'
317000011221     C                   PARM                    WLBDA8
317100011221     C                   Z-ADD     G08INV        DATCOR            8 0
317200941024     C*
317300941024     C                   OPEN      TABEL00F
317400011221     C                   OPEN      TNTBE01L
317500020805     C                   OPEN      AZORG01L
317600940704     C*
317700940704     C* CARICA TABELLA -2R- RIFERIMENTO VOCE
317800940913     C     $ERR          IFEQ      'N'
317900940704     C                   Z-ADD     0             I
318000940704     C                   Z-ADD     CODUT         KTBUTE
318100940704     C                   MOVEL     '2R'          KTBCOD
318200940704     C                   MOVEL     *BLANKS       KTBKEY
318300940704     C     KEYTAB        SETLL     TABEL00F                           99
318400940704     C  N99              READ      TABEL00F                               99
318500940704     C     *IN99         IFEQ      '1'
318600940720     C                   MOVEL     MSG(3)        $MSG
318700940907     C                   EXSR      SCRERR
318800940704     C                   END
318900940704     C     *IN99         DOWEQ     '0'
319000940704     C     TBLCOD        ANDEQ     '2R'
319100940704     C     TBLFLG        IFEQ      *BLANKS
319200940704     C                   ADD       1             I
319300940704     C                   MOVEL     TBLKEY        C2R(I)
319400940707     C                   MOVEL     TBLUNI        DS2R
319500940707     C                   MOVEL     §2RTVA        T2R(I)
319600940704     C                   END
319700940704     C                   READ      TABEL00F                               99
319800940704     C                   END
319900940704     C                   END
320000940720     C*
320100940720     C* CARICA TABELLA -FA- CONDIZIONI DI PAGAMENTO
320200940913     C     $ERR          IFEQ      'N'
320300940720     C                   Z-ADD     0             I
320400940720     C                   Z-ADD     CODUT         KTBUTE
320500940720     C                   MOVEL     'FA'          KTBCOD
320600940720     C                   MOVEL     *BLANKS       KTBKEY
320700940720     C     KEYTAB        SETLL     TABEL00F                           99
320800940720     C  N99              READ      TABEL00F                               99
320900940720     C     *IN99         IFEQ      '1'
321000940720     C                   MOVEL     MSG(2)        $MSG
321100940907     C                   EXSR      SCRERR
321200940720     C                   END
321300940720     C     *IN99         DOWEQ     '0'
321400940720     C     TBLCOD        ANDEQ     'FA'
321500940720     C     TBLFLG        IFEQ      *BLANKS
321600940720     C                   ADD       1             I
321700940720     C                   MOVEL     TBLKEY        CFA(I)
321800940721     C                   MOVEL     TBLUNI        DSFA
321900940720     C                   MOVEL     §FARAT        WTM03             3
322000940720     C                   MOVE      WTM03         WTM030            3 0
322100940720     C     WTM030        IFLT      15
322200940720     C     WTM030        MULT      30            GFA(I)
322300940720     C                   ELSE
322400940720     C                   Z-ADD     WTM030        GFA(I)
322500940720     C                   END
322600940720     C                   END
322700940720     C                   READ      TABEL00F                               99
322800940720     C                   END
322900940720     C                   END
323000000626     C*
323100000626     C* CARICA TABELLA -7O-  CONSEGNE ANOMALE
323200000626IF  1C     $ERR          IFEQ      'N'
323300000626     C                   Z-ADD     0             I
323400000626     C                   Z-ADD     CODUT         KTBUTE
323500000626     C                   MOVEL     '7O'          KTBCOD
323600000626     C                   MOVEL     *BLANKS       KTBKEY
323700000626     C     KEYTAB        SETLL     TABEL00F                           99
323800000626     C  N99              READ      TABEL00F                               99
323900000626IF  2C     *IN99         IFEQ      '1'
324000000626     C                   MOVEL     MSG(1)        $MSG
324100000626     C                   EXSR      SCRERR
324200000626E   2C                   ENDIF
324300000626DO  2C     *IN99         DOWEQ     '0'
324400000626     C     TBLCOD        ANDEQ     '7O'
324500000626IF  3C     TBLFLG        IFEQ      *BLANKS
324600000626     C                   ADD       1             I
324700000626     C                   MOVEL     TBLKEY        CCA(I)
324800000626     C                   MOVEL     TBLUNI        DS7O
324900000626     C                   MOVEL     §7OFPT        CFPT(I)
325000000626     C                   MOVEL     §7ORMS        CRMS(I)
325100000626E   3C                   ENDIF
325200000626     C                   READ      TABEL00F                               99
325300000626E   2C                   ENDDO
325400000626E   1C                   ENDIF
325500021029     C*
325600021029     C* CARICA TABELLA -7R- PARTICOLARITÀ CONSEGNE
325700130529IF  1C*    $ERR          IFEQ      'N'
325800130529     C*                  Z-ADD     0             I
325900130529     C*                  Z-ADD     CODUT         KTBUTE
326000130529     C*                  MOVEL     '7R'          KTBCOD
326100130529     C*                  MOVEL     *BLANKS       KTBKEY
326200130529     C*    KEYTAB        SETLL     TABEL00F                           99
326300130529     C* N99              READ      TABEL00F                               99
326400130529IF  2C*    *IN99         IFEQ      '1'
326500130529     C*                  MOVEL     MSG(32)       $MSG
326600130529     C*                  EXSR      SCRERR
326700130529E   2C*                  ENDIF
326800130529DO  2C*    *IN99         DOWEQ     '0'
326900130529     C*    TBLCOD        ANDEQ     '7R'
327000130529IF  3C*    TBLFLG        IFEQ      *BLANKS
327100130529     C*                  ADD       1             I
327200130529     C*                  MOVEL     TBLKEY        GMA(I)
327300130529     C*                  MOVEL     TBLUNI        DS7R
327400130529     C*                  MOVEL     §7R1BF        G1BF(I)
327500130529E   3C*                  ENDIF
327600130529     C*                  READ      TABEL00F                               99
327700130529E   2C*                  ENDDO
327800130529E   1C*                  ENDIF
327900010420     C*
328000010420     C* CARICA TABELLA -1B-  CODICI TRATTAMENTO MERCE
328100010420IF  1C     $ERR          IFEQ      'N'
328200010420     C                   Z-ADD     0             I
328300010420     C                   Z-ADD     CODUT         KTBUTE
328400010420     C                   MOVEL     '1B'          KTBCOD
328500010420     C                   MOVEL     *BLANKS       KTBKEY
328600010420     C     KEYTAB        SETLL     TABEL00F                           99
328700010420     C  N99              READ      TABEL00F                               99
328800010420IF  2C     *IN99         IFEQ      '1'
328900010801     C                   MOVEL     MSG(28)       $MSG
329000010420     C                   EXSR      SCRERR
329100010420E   2C                   ENDIF
329200010420DO  2C     *IN99         DOWEQ     '0'
329300010420     C     TBLCOD        ANDEQ     '1B'
329400010420IF  3C     TBLFLG        IFEQ      *BLANKS
329500010420     C                   ADD       1             I
329600010420     C                   MOVEL     TBLKEY        CTM(I)
329700010420     C                   MOVEL     TBLUNI        DS1B
329800010420     C                   MOVEL     §1BF12        CF12(I)
329900050801     C                   MOVEL     §1BF16        CF16(I)
330000030530     C                   MOVEL     §1BDKC        CDKC(I)
330100010420E   3C                   ENDIF
330200010420     C                   READ      TABEL00F                               99
330300010420E   2C                   ENDDO
330400010420E   1C                   ENDIF
330500011221     c*
330600011221     C* DIVISE PER CONVERSIONE -OUTPUT C/ECONOMICO-
330700011221     C                   CLEAR                   DSGEDCE
330800011221     C                   MOVEL     *blanks       WDIVCE            3
330900011221     C                   MOVEL(P)  'GED'         KTECOD
331000011221     C                   MOVEL(P)  'CE'          KTEKE1
331100011221     C     KEYTBE        CHAIN     TNTBE01L
331200011221IF  1C                   IF        %found(TNTBE01L)
331300011221     C                   MOVEL     TBEUNI        DSGEDCE
331400011221E   1C                   ENDIF
331500011221IF  1C                   IF        §GEDCEDAT > DATCOR
331600011221     C                   EVAL      WDIVCE = §GEDCEDIP
331700011221X   1C                   ELSE
331800011221     C                   EVAL      WDIVCE = §GEDCEDIV
331900011221E   1C                   ENDIF
332000011221IF  1C                   IF        WDIVCE = *blanks
332100011221     C                   EVAL      WDIVCE = 'ITL'
332200011221E   1C                   ENDIF
332300011221     c*
332400011221     C* DIVISE PER CONVERSIONE -INPUT ANAGRAFICHE C/ECONOMICO-
332500020102     C                   CLEAR                   DGEDAC
332600011221     C                   MOVEL     *blanks       WDIVAC            3
332700011221     C                   MOVEL(P)  'GED'         KTECOD
332800011221     C                   MOVEL(P)  'AC'          KTEKE1
332900011221     C     KEYTBE        CHAIN     TNTBE01L
333000011221IF  1C                   IF        %found(TNTBE01L)
333100020102     C                   MOVEL     TBEUNI        DGEDAC
333200011221E   1C                   ENDIF
333300011221IF  1C                   IF        §GEDACDAT > DATCOR
333400011221     C                   EVAL      WDIVAC = §GEDACDIP
333500011221X   1C                   ELSE
333600020102     C                   EVAL      WDIVAC = §GEDACDID
333700011221E   1C                   ENDIF
333800011221IF  1C                   IF        WDIVAC = *blanks
333900011221     C                   EVAL      WDIVAC = 'ITL'
334000011221E   1C                   ENDIF
334100941024     C*
334200020213     C* CARICA TABELLA -IC-  CODICI QUALITÀ CLEINTE
334300020213IF  1C     $ERR          IFEQ      'N'
334400020213     C                   Z-ADD     0             I
334500020213     C                   Z-ADD     CODUT         KTBUTE
334600020213     C                   MOVEL     'IC'          KTBCOD
334700020213     C                   MOVEL     *BLANKS       KTBKEY
334800020213     C     KEYTAB        SETLL     TABEL00F                           99
334900020213     C  N99              READ      TABEL00F                               99
335000020213IF  2C     *IN99         IFEQ      '1'
335100020213     C                   MOVEL     MSG(29)       $MSG
335200020213     C                   EXSR      SCRERR
335300020213E   2C                   ENDIF
335400020213DO  2C     *IN99         DOWEQ     '0'
335500020213     C     TBLCOD        ANDEQ     'IC'
335600020213IF  3C     TBLFLG        IFEQ      *BLANKS
335700020213     C                   ADD       1             I
335800020213     C                   MOVEL     TBLKEY        CLV(I)
335900020213E   3C                   ENDIF
336000020213     C                   READ      TABEL00F                               99
336100020213E   2C                   ENDDO
336200020213E   1C                   ENDIF
336300020805     C*
336400020805     C* CARICA TABELLA -NTW- NETWORK
336500020805IF  1C     $ERR          IFEQ      'N'
336600020805     C                   Z-ADD     *ZEROS        I
336700020805     C                   MOVEL(P)  'NTW'         KTECOD
336800020805     C     KEYTB2        SETLL     TNTBE01L                           99
336900020805     C     KEYTB2        READE     TNTBE01L                               99
337000020805IF  2C     *IN99         IFEQ      *ON
337100020805     C                   MOVEL     MSG(30)       $MSG
337200020805     C                   EXSR      SCRERR
337300020805E   2C                   ENDIF
337400020805DO  2C     *IN99         DOWEQ     *OFF
337500020805     C                   MOVEL     TBEUNI        DNTW
337600020805     C                   ADD       1             I
337700020805     C                   MOVEL     TBEKE1        SNTW(I)
337800020805     C                   MOVEL     §NTWFIE       SNFIE(I)
337900020805     C     KEYTB2        READE     TNTBE01L                               99
338000020805E   2C                   ENDDO
338100020805E   1C                   ENDIF
338200040903     C* CARICA TABELLA -DFE- per peso limite tariffa pak fedex
338300040903     c                   clear                   ddfe
338400040903IF  1C     $ERR          IFEQ      'N'
338500040903     C                   MOVEL(P)  'DFE'         KTECOD
338600040903     C                   MOVEL(P)  'FED'         KTEKE1
338700040903     C     KEYTBE        chain     TNTBE01L
338800040903     c                   if        %found(tntbe01l)
338900040903     c                   movel(p)  tbeuni        ddfe
339000040903     c                   end
339100040903E   1C                   ENDIF
339200020805     C*
339300020805     C* CARICA FLAG ITALIA/ESTERO DALL'ORGANIGRAMMA
339400020805if  1C     $ERR          IFEQ      'N'
339500020805     C     *LOVAL        SETLL     AZORG01L
339600020805     C                   READ      AZORG01L                               98
339700020805DO  2C     *IN98         DOWEQ     *OFF
339800020805IF  3C     ORGFVA        IFEQ      *BLANKS
339900020805     C     ORGFAG        ANDNE     'V'
340000020805     C                   Z-ADD     ORGFIL        F                 4 0
340100020805     C                   MOVEL     ORGDE3        OG143
340200020805     C                   MOVEL     ORGCTS        CTS(F)
340300020805     C                   Z-ADD     1             II
340400020805     C     §OGNTW        LOOKUP    SNTW(II)                               97
340500020805IF  4C     *IN97         IFEQ      *OFF
340600020805     C                   MOVEL     MSG(31)       $MSG
340700020805     C                   EXSR      SCRERR
340800020805X   4C                   ELSE
340900020805     C                   MOVEL     SNTW(II)      NTW(F)
341000020805     C                   MOVEL     SNFIE(II)     FIE(F)
341100020805IF  5C     NTW(F)        IFEQ      'DPD'
341200020805     C                   MOVEL     'E'           FIE(F)
341300020805E   5C                   ENDIF
341400020805E   4C                   ENDIF
341500020805E   3C                   ENDIF
341600020805     C                   READ      AZORG01L                               98
341700020805E   2C                   ENDDO
341800020805e   1C                   ENDIF
341900020213     C*
342000941024     C                   CLOSE     TABEL00F
342100011221     C                   CLOSE     TNTBE01L
342200020805     C                   CLOSE     AZORG01L
342300940704     C*
342400940704     C                   ENDSR
342500940803     C*------------------------------------------------------------------------*
342600940803     C*? DATCLI - RECUPERA DATI CLIENTE
342700940902     C*           INPUT: - D$DKSC (CODICE DEL CLIENTE)
342800940803     C*           OUTPUT: - WGGP (GIORNI PAGAMENTO)
342900940803     C*                   - WCLV (CLASSE CLIENTE)
343000940803     C*------------------------------------------------------------------------*
343100940722     C     DATCLI        BEGSR
343200971222     C*
343300971222     C* LANCIA PGM PER RECUPERARE DATI CLIENTE
343400971222     C                   CLEAR                   TIBS69
343500971222     C                   Z-ADD     WCLI          I69KIN
343600971222     C                   Z-ADD     WCLI          I69KCP
343700971222     C                   CALL      'TIBS69R'
343800971222     C                   PARM                    TIBS69
343900971222     C                   PARM                    ACODS
344000971222     C                   PARM                    INDDS
344100971222     C                   PARM                    CLPDS
344200971222     C                   PARM                    CLSDS
344300971222IF  1C     O69ERR        IFEQ      '1'
344400020701     C                   MOVE      'BS69'        O69MSG
344500020520     C                   MOVEL     O69MSG        DSMSG
344600971222     C                   MOVEL     O69MSG        $MSG
344700971222     C                   EXSR      SCRERR
344800971222X   1C                   ELSE
344900940722     C*
345000940722     C* RECUPERA GIORNI PAGAMENTO DALL'ANAGRAFICO INDIRIZZI (CNIND)
345100940722     C                   Z-ADD     *ZEROS        WGGP              3 0
345200940802     C                   MOVE      INDCDP        WTM03
345300940722     C                   Z-ADD     1             I
345400940722     C     WTM03         LOOKUP    CFA(I)                                 98
345500971222IF  2C     *IN98         IFEQ      '0'
345600940722     C                   MOVEL     MSG(23)       $MSG
345700940907     C                   EXSR      SCRERR
345800971222X   2C                   ELSE
345900940722     C                   Z-ADD     GFA(I)        WGGP
346000971222E   2C                   ENDIF
346100940722     C*
346200940722     C* RECUPERA LA CLASSE DEL CLIENTE DALL'ANAGRAFICO PARTICOLARITA' (CNCLP)
346300020213     C                   Z-ADD     1             I
346400020213     C     CLPCLV        LOOKUP    CLV(I)                                 98
346500020213IF  2C     *IN98         IFEQ      *ON
346600971229     C                   MOVEL     CLPCLV        WCLV              1
346700971222X   2C                   ELSE
346800940722     C                   MOVEL     MSG(24)       $MSG
346900940907     C                   EXSR      SCRERR
347000971222E   2C                   ENDIF
347100971222     C*
347200971222E   1C                   ENDIF
347300940722     C*
347400940722     C                   ENDSR
347500940831     C*------------------------------------------------------------------------*
347600940831     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
347700940831     C*------------------------------------------------------------------------*
347800940831     C     SUBDAT        BEGSR
347900940831     C*
348000940831     C                   TIME                    DTTIME
348100940831     C                   Z-ADD     DTANN         DSANN
348200940831     C                   Z-ADD     DTMES         DSMES
348300940831     C                   Z-ADD     DTGIO         DSGIO
348400940901     C                   Z-ADD     DTSEC         DSSEC
348500940831     C*
348600940831     C                   ENDSR
348700940907     C*------------------------------------------------------------------------*
348800940907     C*?SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SLSDE (ERRORI SALDI)
348900940907     C*------------------------------------------------------------------------*
349000940907     C     SCRERR        BEGSR
349100940907     C*
349200940913     C                   MOVEL     'S'           $ERR
349300970407     C*
349400970407     C* SE LANCIATO DAL PGM TNTE50R (DELTA IN FILIALE) NON SCRIVO ERRORI
349500970407     C     D$GPGF        IFEQ      'TNTE50R'
349600970407     C                   MOVEL     $MSG          D$GERR
349700970407     C                   MOVEL     $MSG          D$GMSG
349800970407     C                   ELSE
349900941026     C*
350000941026     C* SCRIVE SOLO SE NON VI E' GIA' STATO ALTRO ERRORE (IN QUESTO O ALTRO PGM)
350100941026     C     D$GERR        IFEQ      *BLANKS
350200941026     C*
350300940907     C                   EXSR      SUBDAT
350400940907     C                   Z-ADD     DSDAT8        ERRDAT
350500940907     C                   Z-ADD     DTORA6        ERRTIM
350600941026     C                   ADD       1             D$GPRE
350700941026     C                   Z-ADD     D$GPRE        ERRPRG
350800941213     C                   Z-ADD     D$DAAS        ERRAAS
350900940907     C                   MOVEL     D$DTBL        ERRTBL
351000940907     C                   Z-ADD     D$DLNP        ERRLNP
351100940907     C                   Z-ADD     D$DNRS        ERRNRS
351200940907     C                   Z-ADD     D$DNSP        ERRNSP
351300941201     C                   Z-ADD     D$DAAS        DSAAS
351400941201     C                   Z-ADD     D$DMGS        DSMGS
351500941201     C                   Z-ADD     DSDAT8        ERRDSP
351600941007     C                   MOVEL     $MSG          DSMSG
351700941007     C                   MOVEL     DSDER         ERRDER
351800941007     C                   MOVEL     DSCER         ERRCER
351900950123     C                   MOVEL     D$GSTO        ERRSTS
352000941026     C                   Z-ADD     D$GDAT        ERRDAM
352100941026     C                   Z-ADD     D$GTIM        ERRORM
352200941026     C                   MOVEL     D$GUSR        ERRUSR
352300941026     C                   MOVEL     D$GPGF        ERRPGF
352400950118     C                   MOVEL     D$GFIL        ERRFIL
352500950118     C                   MOVEL     D$GMBR        ERRMBR
352600950118     C                   MOVEL     D$GLIB        ERRLIB
352700991119     C     D$GFIL        IFEQ      'TITAS10F'
352800020703     C     D$GFIL        OREQ      'TITAS00F'
352900950118     C                   MOVEL     D$PRE1        ERRRP1
353000950118     C                   MOVEL     D$PRE2        ERRRP2
353100950118     C                   MOVEL     D$DRE1        ERRRD1
353200950118     C                   MOVEL     D$DRE2        ERRRD2
353300950123     C                   ENDIF
353400950123     C     D$GFIL        IFEQ      'TNCSB00F'
353500950123     C                   MOVEL     D$PCO1        ERRRP1
353600950123     C                   MOVEL     D$PCO2        ERRRP2
353700950123     C                   MOVEL     D$DCO1        ERRRD1
353800950123     C                   MOVEL     D$DCO2        ERRRD2
353900950123     C                   ENDIF
354000030205     C     D$GFIL        IFEQ      'FIAR500F'
354100030205     C     D$GTRD        IFEQ      'TRS'
354200030205     C                   MOVEL     D$PAR5T       ERRRP1
354300030205     C                   MOVEL     D$DAR5T       ERRRD1
354400030205     C                   ENDIF
354500030801     C     D$GTRD        IFEQ      'BAN'
354600030801     C     D$GTRD        OREQ      'BNB'
354700030801     C                   MOVEL     D$PAR5B       ERRRP1
354800030801     C                   MOVEL     D$DAR5B       ERRRD1
354900030801     C                   ENDIF
355000030801     C                   ENDIF
355100030801     C                   WRITE     ECERR
355200941026     C*
355300941026     C                   MOVEL     DSCER         D$GERR
355400941026     C                   MOVEL     DSDER         D$GMSG
355500941026     C*
355600941026     C                   ENDIF
355700970407     C                   ENDIF
355800940907     C*
355900940907     C                   ENDSR
356000940907     C*------------------------------------------------------------------------*
356100940907     C*? RUTINZ - ROUTINE INIZIALE
356200940907     C*------------------------------------------------------------------------*
356300940907     C     RUTINZ        BEGSR
356400940907     C*
356500940907     C* PARAMETRI IN INPUT ALLA ROUTINE
356600940907     C     *ENTRY        PLIST
356700940907     C                   PARM                    DS$ECO
356800940907     C*
356900940907     C* CHIAVE LETTURA TABEL00F - COMPLETA
357000940907     C     KEYTAB        KLIST
357100940907     C                   KFLD                    KTBUTE
357200940907     C                   KFLD                    KTBCOD
357300940907     C                   KFLD                    KTBKEY
357400011221     C*
357500011221     C* CHIAVE LETTURA TNTBE01L - PARZIALE
357600011221     C     KEYTBE        KLIST
357700011221     C                   KFLD                    KTECOD
357800011221     C                   KFLD                    KTEKE1
357900020805     C*
358000020805     C* CHIAVE LETTURA TNTBE01L - PARZIALE
358100020805     C     KEYTB2        KLIST
358200020805     C                   KFLD                    KTECOD
358300940907     C*
358400940907     C* CHIAVE LETTURA ECEVT01L - COMPLETA
358500940907     C     KEYEVT        KLIST
358600940907     C                   KFLD                    EVTVOC
358700940907     C                   KFLD                    EVTPRG
358800940907     C*
358900940907     C* CHIAVE LETTURA ECEVD01L - COMPLETA
359000940907     C     KEYEVD        KLIST
359100940907     C                   KFLD                    EVDVOC
359200940907     C                   KFLD                    EVDPRG
359300940907     C                   KFLD                    EVDTTV
359400940907     C                   KFLD                    EVDFT1
359500940907     C                   KFLD                    EVDFT2
359600940907     C                   KFLD                    EVDCLI
359700940907     C                   KFLD                    EVDCTR
359800940907     C                   KFLD                    EVDCTS
359900940907     C                   KFLD                    EVDCAP
360000940907     C                   KFLD                    EVDDDT
360100940907     C                   KFLD                    EVDSGI
360200940907     C*
360300940907     C* CHIAVE LETTURA ECEVD02L - COMPLETA
360400940907     C     KEYEV2        KLIST
360500940907     C                   KFLD                    EVDCLI
360600940907     C*
360700940907     C* CHIAVE LETTURA ECEVM01L - COMPLETA
360800940907     C     KEYEVM        KLIST
360900940907     C                   KFLD                    EVMVOM
361000940907     C                   KFLD                    EVMPRM
361100940907     C                   KFLD                    EVMVOC
361200940907     C                   KFLD                    EVMPRG
361300940907     C*
361400940907     C* CHIAVE LETTURA ECERR01L - COMPLETA
361500940907     C     KEYERR        KLIST
361600940907     C                   KFLD                    ERRDAT
361700940907     C                   KFLD                    ERRTIM
361800940907     C                   KFLD                    ERRPRG
361900940907     C*
362000940907     C* DEFINISCO CAMPI
362100940907     C     *LIKE         DEFINE    TBLKUT        KTBUTE
362200940907     C     *LIKE         DEFINE    TBLCOD        KTBCOD
362300940907     C     *LIKE         DEFINE    TBLKEY        KTBKEY
362400011221     C     *LIKE         DEFINE    TBECOD        KTECOD
362500011221     C     *LIKE         DEFINE    TBEKE1        KTEKE1
362600940907     C* CAMPI VARI
362700940907     C     *LIKE         DEFINE    EVTARF        WVOL
362800991008     C     *LIKE         DEFINE    D$DPKF        WPES
362900040427     C     *LIKE         DEFINE    D$DPKF        WDPES
363000940907     C     *LIKE         DEFINE    EVTVOC        WVOC
363100940907     C     *LIKE         DEFINE    EVTPRG        WPRG
363200000306     C     *LIKE         DEFINE    EVTTIV        WTIV
363300940907     C     *LIKE         DEFINE    EVTRFI        WRIF
363400940907     C     *LIKE         DEFINE    EVDTTV        WTTV
363500940907     C     *LIKE         DEFINE    EVDFT1        WFT1
363600940907     C     *LIKE         DEFINE    EVDFT2        WFT2
363700940907     C     *LIKE         DEFINE    EVDCLI        WKSC
363800940907     C     *LIKE         DEFINE    EVDCTR        WCTR
363900940907     C     *LIKE         DEFINE    EVDCTS        WCTS
364000940907     C     *LIKE         DEFINE    EVDCAP        WCAP
364100940907     C     *LIKE         DEFINE    EVTDDV        WDSP
364200940907     C     *LIKE         DEFINE    EVTDDV        WDDSP
364300940908     C     *LIKE         DEFINE    EVTDDV        WSPDAT
364400940907     C     *LIKE         DEFINE    D$DKSC        WCLI
364500940907     C     *LIKE         DEFINE    D$DLNP        W$LNP
364600111011     C     *LIKE         DEFINE    D$Dlna        W$LNA
364700940907     C     *LIKE         DEFINE    D$DTFP        W$TFP
364800940907     C     *LIKE         DEFINE    D$DKSC        W$KSC
364900940908     C     *LIKE         DEFINE    D$DKSC        WSPKSC
365000940907     C     *LIKE         DEFINE    D$DCCM        W$CCM
365100940907     C     *LIKE         DEFINE    D$DMCT        W$MCT
365200011227     C     *LIKE         DEFINE    EVDSGI        W$SGI
365300011227     C     *LIKE         DEFINE    EVDSGF        W$SGF
365400011227     C     *LIKE         DEFINE    EVDITR        W$ITR
365500011227     C     *LIKE         DEFINE    EVDIMI        W$IMI
365600011227     C     *LIKE         DEFINE    EVDIMA        W$IMA
365700020102     C     *LIKE         DEFINE    YECIMI        §IMI
365800020102     C     *LIKE         DEFINE    YECDVI        §DVI
365900020102     C     *LIKE         DEFINE    YECIMO        §IMO
366000021029     C     *LIKE         DEFINE    §1BF12        WCF12
366100050801     C     *LIKE         DEFINE    §1BF16        WCF16
366200030530     C     *LIKE         DEFINE    §1BDKC        WCDKC
366300130529     C*    *LIKE         DEFINE    §7R1BF        WG1BF
366400950123     C*
366500950123     C* FACCIO LETTURA DI ECERR FASULLA PER NON FARE VENIRE MESSAGGIO
366600950123     C* AD OGNI APERTURA DI DEL FILE
366700950123     C   99
366800950123     CANN99KEYERR        CHAIN     ECERR01L                           99
366900940907     C*
367000940907     C* IMPOSTO CAMPI DI GESTIONE PGM
367100940913     C                   MOVEL     'N'           $ERR              1
367200941006     C                   MOVEL     *BLANKS       $MSG             80
367300940907     C*
367400940907     C* RECUPERO IL CODICE DELL'AZIENDA
367500940907     C                   Z-ADD     1             CODUT             1 0
367600940907     C*
367700940907     C* SE PGM CHIAMATO PER CHIUSURA ESCE
367800991214     C     D$GSTS        IFEQ      'C'
367900991214     C     D$GSTS        OREQ      'S'
368000991214     C                   ELSE
368100940907     C*
368200940907     C* PORTA LE IMMAGINI BOLLA PRIMA E DOPO DALLA DS INPUT ALLA DS PGM
368300940907     C                   MOVEL     D$PRE1        D$PDS1
368400940907     C                   MOVEL     D$PRE2        D$PDS2
368500940907     C                   MOVEL     D$DRE1        D$DDS1
368600940907     C                   MOVEL     D$DRE2        D$DDS2
368700941201     C                   MOVEL     D$ORE1        D$ODS1
368800941201     C                   MOVEL     D$ORE2        D$ODS2
368900940907     C*
369000940907     C* PORTA I RICAVI IN INPUT NELLA SCHIERA DEL PGM
369100950125     C                   MOVEA     D$DCCD        CAN
369200950125     C                   MOVEA     D$DICD        IAN
369300030506DO  2C     1             DO        75            I                 6 0
369400940907     C                   MOVE      CAN(I)        DSWCA
369500940907     C                   Z-ADD     DSWCN         RCD(I)
369600011221     C                   MOVE      IAN(I)        DSWIA
369700940907     C                   Z-ADD     DSWIN         RIM(I)
369800940907E   2C                   ENDDO
369900940907     C*
370000940907     C* IMPOSTA A ZEROS LE SCHIERE IN OUTPUT
370100940907     C                   CLEAR                   CCD
370200940907     C                   CLEAR                   CIM
370300940907     C                   CLEAR                   CIMP
370400940907     C                   CLEAR                   VIMP
370500940907     C                   CLEAR                   Q2R
370600940907     C*
370700940907     C* RECUPERO DATI VARI
370800941201     C                   Z-ADD     D$DAAS        DSAAS
370900941201     C                   Z-ADD     D$DMGS        DSMGS
371000941201     C                   Z-ADD     DSDAT8        WDDSP
371100940907     C*
371200940907     C* CARICA LE TABELLE NECESSARIE AL PGM
371300940907     C* CARICA LE TARIFFE DI CARTELLO
371400940907     C* SOLO NEL CASO IN CUI NON SIANO ANCORA STATE CARICATE
371500940913     C     $ERR          IFEQ      'N'
371600940907     C     CTABSN        IFNE      'S'
371700940907     C     D$GSTS        ANDNE     'C'
371800940907     C                   MOVEL     'S'           CTABSN            1
371900940907     C                   EXSR      CARTAB
372000940907     C                   EXSR      CARCAR
372100940907     C                   EXSR      CARKSC
372200940907     C                   EXSR      CARMAG
372300940907     C                   ENDIF
372400940907     C                   ENDIF
372500940907     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI AL POSTO DEI DATI DOPO
372600940907     C     D$OLNP        IFNE      *ZEROS
372700950913     C     D$DFIG        ANDEQ     'S'
372800940907     C                   Z-ADD     D$OLNP        W$LNP
372900940907     C                   ELSE
373000940907     C                   Z-ADD     D$DLNP        W$LNP
373100940907     C                   ENDIF
373200940907     C     D$OTFP        IFNE      *ZEROS
373300950913     C     D$DFIG        ANDEQ     'S'
373400940907     C                   Z-ADD     D$OTFP        W$TFP
373500940907     C                   ELSE
373600940907     C                   Z-ADD     D$DTFP        W$TFP
373700940907     C                   ENDIF
373800940907     C     D$OKSC        IFNE      *ZEROS
373900950913     C     D$DFIG        ANDEQ     'S'
374000940907     C                   Z-ADD     D$OKSC        W$KSC
374100940907     C                   ELSE
374200940907     C                   Z-ADD     D$DKSC        W$KSC
374300940907     C                   ENDIF
374400940907     C     D$OCCM        IFNE      *ZEROS
374500950913     C     D$DFIG        ANDEQ     'S'
374600940907     C                   Z-ADD     D$OCCM        W$CCM
374700940907     C                   ELSE
374800940907     C                   Z-ADD     D$DCCM        W$CCM
374900940907     C                   ENDIF
375000940907     C     D$OMCT        IFNE      *BLANKS
375100950913     C     D$DFIG        ANDEQ     'S'
375200940907     C                   MOVEL     D$OMCT        W$MCT
375300940907     C                   ELSE
375400940907     C                   MOVEL     D$DMCT        W$MCT
375500940907     C                   ENDIF
375600111011     c* verifico se la linea di arrivo è in £6
375700111011     c                   z-add     d$dlna        w$lna
375800111011     c                   clear                   fnlv55ds
375900111011     c                   z-add     d$dlna        d55lin
376000111011     c                   movel     '6'           d55tpt
376100111011     C                   eval      d55drf = WDDSP
376200111011     c                   if        d$ddti <> 0
376300111011     C                   eval      d55drf = d$ddti
376400111011     c                   end
376500111011     c                   call      'FNLV55R'
376600111011     c                   parm                    fnlv55ds
376700111011     c                   if        d55tfa <> 0
376800111011     c                   eval      w$lna = d55tfa
376900111011     c                   end
377000021029     C*
377100021029     C* CARICA I DATI DEL CLIENTE di fatturazione, SE DIVERSO DALL'ULTIMO LETTO
377200021029     C     $ERR          IFEQ      'N'
377300021029     C     W$KSC         IFNE      WCLI
377400021029     C                   Z-ADD     W$KSC         WCLI
377500021029     C                   EXSR      DATCLI
377600021029     C                   ENDIF
377700021029     C                   ENDIF
377800021029     C*
377900940907     C*
378000940907     C                   ENDIF
378100940907     C*
378200940907     C                   ENDSR
378300940907     C*------------------------------------------------------------------------*
378400940907     C*? STAMPA - STAMPA RIGA
378500940907     C*------------------------------------------------------------------------*
378600941229     C*****+++   STAMPA    BEGSR
378700940902     C*
378800941024     C* APRE IL FILE DI STAMPA
378900941229     C****++++             OPEN PRTF198
379000941024     C*
379100940902     C* RECUPERO LA DATA E L'ORA
379200941229     C*****++++            Z-ADDUDATE     DATA    60
379300941229     C****+++++            TIME           ORA     60
379400940902     C*
379500940902     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
379600941229     C****+++++            EXCPTTESTA
379700940902     C*
379800940902     C* STAMPA RIGA DI DETTAGLIO TEMPI
379900941229     C***++++++            EXCPTRIGA
380000940902     C*
380100940902     C* STAMPA RIGA FINE
380200941229     C****+++++            EXCPTSTAFIN
380300941024     C*
380400941024     C* CHIUDE IL FILE DI STAMPA
380500941229     C****+++++            CLOSEPRTF198
380600941229     C****++++
380700941229     C***++++++            ENDSR
380800940907     O*------------------------------------------------------------------------*
380900941229     O*PRTF198 E  102           TESTA
381000941229     O****++++                            8 'TRECD8R '
381100941229     O****++++                           82 'PRESTAZIONI PGM'
381200941229     O****++++                          150 'Data:'
381300941229     O****++++                 DATA     159 '  /  /  '
381400941229     O****++++                          167 'Ora:'
381500941229     O****++++                 ORA      178 '  .  .  '
381600941229     O****++++                          190 'Pagina:'
381700941229     O****++++                 PAGE  Z  198
381800941229     O*------------------------
381900941229     O****++++E  1             RIGA
382000941229     O****++++                           30 'NUMERO DI BOLLE.....:'
382100941229     O*****++++++++++++++++++++WBOLLE2   40
382200941229     O****++++E  1             RIGA
382300941229     O****++++                           30 'TENT.RICERCA TARIFFA:'
382400941229     O******+++++++++++++++++++WTEEVD2   40
382500941229     O****++++E  1             RIGA
382600941229     O****++++                           30 'TENT.SETLL   TARIFFA:'
382700941229     O******+++++++++++++++++++WSEEVD2   40
382800941229     O****++++E  1             RIGA
382900941229     O****++++                           30 'TENT.LETTURE TARIFFA:'
383000941229     O*****++++++++++++++++++++WLEEVD2   40
383100941229     O****++++E  1             RIGA
383200941229     O****++++                           30 'TENT.CARTELLO.......:'
383300941229     O*****++++++++++++++++++++WTECAR2   40
383400941229     O****++++E  1             RIGA
383500941229     O****++++                           30 'CARTELLO IN MEMORIA.:'
383600941229     O****+++++++++++++++++++++WMECAR2   40
383700941229     O****++++E  1             RIGA
383800941229     O****++++                           30 'CARTELLO LETTE......:'
383900941229     O*****++++++++++++++++++++WLECAR2   40
384000941229     O****++++E  1             RIGA
384100941229     O****++++                           30 'TENT.FILIALE........:'
384200941229     O*****++++++++++++++++++++WTEFIL2   40
384300941229     O****++++E  1             RIGA
384400941229     O****++++                           30 'FILIALE IN MEMORIA..:'
384500941229     O*****++++++++++++++++++++WMEFIL2   40
384600941229     O****++++E  1             RIGA
384700941229     O****++++                           30 'FILIALE LETTE.......:'
384800941229     O*****++++++++++++++++++++WLEFIL2   40
384900941229     O****++++E  1             RIGA
385000941229     O****++++                           30 'TENT.CLIENTE........:'
385100941229     O*****++++++++++++++++++++WTEKSC2   40
385200941229     O****++++E  1             RIGA
385300941229     O****++++                           30 'CLIENTE IN MEMORIA..:'
385400941229     O*****++++++++++++++++++++WMEKSC2   40
385500941229     O****++++E  1             RIGA
385600941229     O****++++                           30 'CLIENTE LETTE.......:'
385700941229     O******+++++++++++++++++++WLEKSC2   40
385800941229     O****++++E  1             RIGA
385900941229     O****++++                           30 'TENT.MAGGIORAZIONI..:'
386000941229     O******+++++++++++++++++++WTEMAG2   40
386100941229     O****++++E  1             RIGA
386200941229     O****++++                           30 'MAGGIOR. IN MEMORIA.:'
386300941229     O******+++++++++++++++++++WMEMAG2   40
386400941229     O****++++E  1             RIGA
386500941229     O****++++                           30 'MAGGIORAZIONI LETTE.:'
386600941229     O*****++++++++++++++++++++WLEMAG2   40
386700940907     O*------------------------
386800941229     O****++++E   60           STAFIN
386900941229     O*****+++                           76 '*** FINE STAMPA ***'
387000940907     O*------------------------------------------------------------------------*
387100941201**   MSG
387200000623TRECD8R- Tabella '7O' non trovata. Avvertire CED.                         TB7O 1
387300941201TRECD8R- Tabella 'FA' non trovata. Avvertire CED.                         TBFA 2
387400941201TRECD8R- Tabella '2R' non trovata. Avvertire CED.                         TB2R 3
387500941201TRECD8R- Tipo tariffa '00' (quintali) non trovata.                        TT00 4
387600941201TRECD8R- Tipo tariffa '01' (colli) non trovata.                           TT01 5
387700941201TRECD8R- Tipo tariffa '02' (n.spedizioni) non trovata.                    TT02 6
387800941201TRECD8R- Tipo tariffa '03' (chilogrammi) non trovata.                     TT03 7
387900941201TRECD8R- Tipo tariffa '04' (volume) non trovata.                          TT04 8
388000951213TRECD8R- Tipo tariffa '09' (stop ritiro) non trovata.                     TT09 9
388100941201TRECD8R- Tipo tariffa '20' (val.limite risarcibile) non trovata.          TT2010
388200941201TRECD8R- Tipo tariffa '21' (imponibile reale) non trovata.                TT2111
388300941201TRECD8R- Tipo tariffa '22' (contrassegno) non trovata.                    TT2212
388400941201TRECD8R- Tipo tariffa '23' (val merce assicur.) non trovata.              TT2313
388500941201TRECD8R- Tipo tariffa '24' (n.giorni pagamento) non trovata.              TT2414
388600941201TRECD8R- Tipo tariffa '25' (imp.comp.sede) non trovata.                   TT2515
388700951213TRECD8R- Tipo tariffa '10' (stop consegna) non trovata.                   TT1016
388800991029TRECD8R- Tipo tariffa '11' (peso x giorni giacenza) non trovata.          TT1117
388900991119TRECD8R- Voce non trovata.  XXX                                           VNTR18
389000941201TRECD8R- Tipo tariffa della voce non trovata in schiera.                  TTNT19
389100941201TRECD8R- Qta x ricerca scaglione non trovata.                             QRSN20
389200941201TRECD8R- Manca tariffa.Voce xxx.999 999                                   MATA21
389300941201TRECD8R- Voce di maggiorazione senza voci da maggiorare.                  VMSV22
389400941201TRECD8R- Condizione di pagamento non trovata.                             CPNT23
389500020213TRECD8R- Codice importanza cliente non trovato.                           APNT24
389600000201TRECD8R- Tipo tariffa '12' (giorni giacenza) non trovata.                 TT1125
389700000623TRECD8R- Codice consegna anomala non trovata                              CANT26
389800010420TRECD8R- Codice trattamento merce non trovata                             TMNT27
389900010801TRECD8R- Tabella '1B' non trovata. Avvertire CED.                         TB1B28
390000020213TRECD8R- Tabella 'IC' non trovata. Avvertire CED.                         TBIC29
390100020805TRECD8R- Tabella 'NTW' non trovata. Avvertire CED.                        TNTW30
390200020805TRECD8R- Network filiale non trovato in tab. 'NTW'. Avvertire CED.        NWNT31
390300021029TRECD8R- Tabella '7R' non trovata. Avvertire CED.                         TB7R32
390400021029TRECD8R- Codice particolarità consegna non trovato                        GMNT33
390500030506TRECD8R- Più di 75 voci calcolate. Avvertire CED.                         P75V34
390600030409TRECD8R- Tipo tariffa '06' (bancali) non trovata.                         TT0635
390700020805TRECD8R-                                                                      36
390800020805TRECD8R-                                                                      37
390900020805TRECD8R-                                                                      38
391000020805TRECD8R-                                                                      39
391100020805TRECD8R-                                                                      40
