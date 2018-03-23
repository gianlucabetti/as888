000100940912     H*------------------------------------------------------------------------*
000200950310     H* SALVATAGGIO E PULIZIA LEGAMI BOLLA
000300940912     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500950309     F* F N L B L
000600950309     FFNLBL01L  UF   E           K DISK
000700950309     FFNLBL00F  O  A E           K DISK
000800950309     F                                     RENAME(FNLBL000:FNLBLS00)
000900950310     F* T N T A S
001000990803     FTITAS30C  IF   E           K DISK
001100931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001200931105     F                                     FORMLEN(66)
001300931105     F                                     FORMOFL(60)
001400940912     L*------------------------------------------------------------------------*
001500940912     D*------------------------------------------------------------------------*
001600940912     D* SCHIERE
001700940912     D*------------------------------------------------------------------------*
001800931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
001900950309     D***************************************************************
002000950309     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
002100950309     D DS§DAT          DS
002200950309     D  DSSEC                  1      2  0
002300950309     D  DSANN                  3      4  0
002400950309     D  DSMES                  5      6  0
002500950309     D  DSGIO                  7      8  0
002600950309     D  DSAAS                  1      4  0
002700950309     D  DSMGS                  5      8  0
002800950309     D  DSDAT6                 3      8  0
002900950309     D  DSDAT8                 1      8  0
003000940912     D*------------------------------------------------------------------------*
003100940912     D* INPUT
003200940912     D*------------------------------------------------------------------------*
003300941123     D                 DS
003400941123     D  DSAASI                 1      4  0
003500941123     D  DSMGSI                 5      8  0
003600950309     D  DSDATI                 1      8  0
003700941123     D                 DS
003800941123     D  DSAASF                 1      4  0
003900941123     D  DSMGSF                 5      8  0
004000950309     D  DSDATF                 1      8  0
004100940912     D*-------------------
004200931115     D* PARAMETRI LANCIO PGM
004300940912     D*-------------------
004400931115     D PARAM           DS                  INZ
004500950309     D  PARDTI                 1      8  0                                      DT.INI
004600950309     D  PARDTF                 9     16  0                                      DT.FIN
004700940912     D*-------------------
004800940912     D* ARCHITETTURA
004900940912     D*-------------------
005000931115     D KPJBA         E DS
005100940912     C*------------------------------------------------------------------------*
005200940912     C*?MAIN LINE
005300940912     C*------------------------------------------------------------------------*
005400000000     C*
005500930129     C* ROUTINE INIZIALE
005600930129     C                   EXSR      RUTINZ
005700931105     C*
005800931105     C* STAMPA RIGA DI TESTATA
005900931105     C                   EXCEPT    TESTA
006000930601     C*
006100930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
006200930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
006300000000     C*
006400950309     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU FNLBL
006500950309     C                   EXSR      SETLBL
006600930128     C*
006700950309     C* CICLO FINO A FINE FILE FNLBL OPPURE
006800950308     C* RICHIESTA CHIUSURA PGM OPPURE
006900950308     C* FINE PGM PER ERRORE
007000950308     C                   SETOFF                                       98
007100950309     C     FINLBL        DOWEQ     '0'                                          --- 2 -->
007200950308     C     *IN98         ANDEQ     '0'
007300950308     C     FINL00        ANDEQ     '0'
007400950307     C*
007500950309     C                   WRITE     FNLBLS00                             99      *SCRIVO FNLBLS
007600950309     C   99              EXCEPT    ERRLBL
007700950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
007800950310     C                   DELETE    FNLBL000                                     *CANCELLO FNLBL
007900950309     C  N99              ADD       1             NUMLBL                         *REC TRASF FNLBL
008000000000     C*
008100950309     C                   EXSR      LETLBL                                       *SUCCESSIVO FNLBL
008200950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
008300930128     C*
008400950307     C                   ENDDO                                                  <-- 2 ---
008500950307     C                   ENDIF                                                  <-- 1 ---
008600931105     C*
008700931105     C* STAMPA RIGA DI FINE STAMPA
008800931115     C                   Z-ADD     UDATE         DATA              6 0
008900931115     C                   TIME                    ORA               6 0
009000931105     C                   EXCEPT    STAFIN
009100000000     C*
009200000000     C                   SETON                                        LR
009300940912     C*------------------------------------------------------------------------*
009400950309     C*? SETLBL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE FNLBL
009500940912     C*------------------------------------------------------------------------*
009600950309     C     SETLBL        BEGSR
009700931115     C*
009800931115     C* AZZERO CONTATORE RECORD LETTI
009900950309     C                   MOVEL     '0'           FINLBL            1
010000930208     C*
010100930316     C* POSIZIONAMENTO E PRIMA LETTURA
010200941123     C                   Z-ADD     PARDTI        DSDATI
010300941123     C                   Z-ADD     PARDTF        DSDATF
010400950309     C                   Z-ADD     *ZEROS        LBLAAN
010500950309     C     KEYLBL        SETLL     FNLBL01L                           99        *FINE-ARCHIV
010600930625     C*
010700930625     C     *IN99         IFEQ      '1'
010800950309     C                   MOVEL     '1'           FINLBL
010900930625     C                   ELSE
011000950309     C                   EXSR      LETLBL
011100930625     C                   END
011200930208     C*
011300930208     C                   ENDSR
011400940912     C*------------------------------------------------------------------------*
011500950309     C*? LETLBL   LETTURA PROSSIMO RECORD FNLBL
011600940912     C*------------------------------------------------------------------------*
011700950309     C     LETLBL        BEGSR
011800930625     C*
011900930625     C* LEGGO FINO A:
012000930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
012100930625     C*     - TROVATO RECORD VALIDO
012200930625     C                   MOVEL     'N'           WRECOK
012300950309     C     FINLBL        DOWEQ     '0'                                          --- 1 -->
012400930625     C     WRECOK        ANDEQ     'N'
012500930208     C*
012600930316     C* LETTURE SUCCESSIVE ALLA PRIMA
012700950309     C                   READ      FNLBL01L                               99
012800930625     C*
012900930625     C     *IN99         IFEQ      '1'                                          *FINE FILE
013000950309     C     LBLAAN        ORGT      DSAASF                                       *SUPERATO ANNO FINE
013100950309     C                   MOVEL     '1'           FINLBL
013200930625     C                   ELSE
013300950309     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
013400930208     C                   END
013500930625     C*
013600930625     C                   END                                                    <-- 1 ---
013700930208     C*
013800930208     C                   ENDSR
013900950307     C*------------------------------------------------------------------------*
014000950309     C*? CHKREC - CONTROLLA VALIDITA' DEL RECORD
014100950307     C*------------------------------------------------------------------------*
014200950309     C     CHKREC        BEGSR
014300950307     C*
014400950307     C                   MOVEL     'S'           WRECOK            1
014500950309     C*
014600950309     C* TUTTE LE BOLLE DEVONO AVERE LA DATA INFERIORE ALLA DATA DI FINE CANC.
014700950309     C* BOLLA ORIGINALE
014800950309     C     LBLAAO        IFGT      DSAASF
014900950309     C                   MOVEL     'N'           WRECOK
015000950309     C                   ENDIF
015100950309     C     LBLAAO        IFEQ      DSAASF
015200950309     C     LBLMGO        ANDGT     DSMGSF
015300950309     C                   MOVEL     'N'           WRECOK
015400950309     C                   ENDIF
015500950309     C* BOLLA SEGUENTE
015600950309     C     LBLAAN        IFGT      DSAASF
015700950309     C                   MOVEL     'N'           WRECOK
015800950309     C                   ENDIF
015900950309     C     LBLAAN        IFEQ      DSAASF
016000950309     C     LBLMGN        ANDGT     DSMGSF
016100950309     C                   MOVEL     'N'           WRECOK
016200950309     C                   ENDIF
016300950309     C* BOLLA PRECEDENTE
016400950309     C     LBLAAP        IFGT      DSAASF
016500950309     C                   MOVEL     'N'           WRECOK
016600950309     C                   ENDIF
016700950309     C     LBLAAP        IFEQ      DSAASF
016800950309     C     LBLMGP        ANDGT     DSMGSF
016900950309     C                   MOVEL     'N'           WRECOK
017000950309     C                   ENDIF
017100950309     C*
017200990803     C* CONTROLLA SE ESISTE IN TITAS LA BOLLA ORIGINALE
017300950309     C     WRECOK        IFEQ      'S'
017400950309     C                   Z-ADD     LBLAAO        TASAAS
017500950309     C                   Z-ADD     LBLLPO        TASLNP
017600950309     C                   Z-ADD     LBLNRO        TASNRS
017700950309     C                   Z-ADD     LBLNSO        TASNSP
017800950309     C                   MOVEL     *LOVAL        TASTBL
017900990803     C     KEYTAS        SETLL     TITAS30C                           99
018000990803     C  N99              READ      TITAS30C                               99
018100950309     C     *IN99         IFEQ      '0'
018200950309     C     TASAAS        ANDEQ     LBLAAO
018300950309     C     TASLNP        ANDEQ     LBLLPO
018400950309     C     TASNRS        ANDEQ     LBLNRO
018500950309     C     TASNSP        ANDEQ     LBLNSO
018600950309     C                   MOVEL     'N'           WRECOK                         *ESISTE, NON CANCELL
018700950309     C                   ENDIF
018800950309     C                   ENDIF
018900950309     C*
019000990803     C* CONTROLLA SE ESISTE IN TITAS LA BOLLA SEGUENTE
019100950309     C     WRECOK        IFEQ      'S'
019200950309     C                   Z-ADD     LBLAAN        TASAAS
019300950309     C                   Z-ADD     LBLLPN        TASLNP
019400950309     C                   Z-ADD     LBLNRN        TASNRS
019500950309     C                   Z-ADD     LBLNSN        TASNSP
019600950309     C                   MOVEL     *LOVAL        TASTBL
019700990803     C     KEYTAS        SETLL     TITAS30C                           99
019800990803     C  N99              READ      TITAS30C                               99
019900950309     C     *IN99         IFEQ      '0'
020000950309     C     TASAAS        ANDEQ     LBLAAN
020100950309     C     TASLNP        ANDEQ     LBLLPN
020200950309     C     TASNRS        ANDEQ     LBLNRN
020300950309     C     TASNSP        ANDEQ     LBLNSN
020400950309     C                   MOVEL     'N'           WRECOK                         *ESISTE, NON CANCELL
020500950309     C                   ENDIF
020600950309     C                   ENDIF
020700950309     C*
020800990803     C* CONTROLLA SE ESISTE IN TITAS LA BOLLA PRECEDENTE
020900950309     C     WRECOK        IFEQ      'S'
021000950309     C                   Z-ADD     LBLAAP        TASAAS
021100950309     C                   Z-ADD     LBLLPP        TASLNP
021200950309     C                   Z-ADD     LBLNRP        TASNRS
021300950309     C                   Z-ADD     LBLNSP        TASNSP
021400950309     C                   MOVEL     *LOVAL        TASTBL
021500990803     C     KEYTAS        SETLL     TITAS30C                           99
021600990803     C  N99              READ      TITAS30C                               99
021700950309     C     *IN99         IFEQ      '0'
021800950309     C     TASAAS        ANDEQ     LBLAAP
021900950309     C     TASLNP        ANDEQ     LBLLPP
022000950309     C     TASNRS        ANDEQ     LBLNRP
022100950309     C     TASNSP        ANDEQ     LBLNSP
022200950309     C                   MOVEL     'N'           WRECOK                         *ESISTE, NON CANCELL
022300950309     C                   ENDIF
022400950309     C                   ENDIF
022500950307     C*
022600950307     C                   ENDSR
022700940912     C*------------------------------------------------------------------------*
022800940912     C*? RUTINZ - ROUTINE INIZIALE
022900940912     C*------------------------------------------------------------------------*
023000000000     C     RUTINZ        BEGSR
023100931115     C*
023200931115     C* PARAMETRI IN INPUT AL PGM
023300931115     C     *ENTRY        PLIST
023400931115     C                   PARM                    KPJBA
023500931115     C                   MOVEL     KPJBU         PARAM
023600930208     C*
023700990803     C* CHIAVE LETTURA TITAS30C - COMPLETA
023800930621     C     KEYTAS        KLIST
023900941123     C                   KFLD                    TASAAS
024000950309     C                   KFLD                    TASLNP
024100950309     C                   KFLD                    TASNRS
024200950309     C                   KFLD                    TASNSP
024300950309     C                   KFLD                    TASTBL
024400950307     C*
024500950309     C* CHIAVE LETTURA FNLBL01L - COMPLETA
024600950309     C     KEYLBL        KLIST
024700950309     C                   KFLD                    LBLAAN
024800950309     C                   KFLD                    LBLLPN
024900950309     C                   KFLD                    LBLNRN
025000950309     C                   KFLD                    LBLNSN
025100930208     C*
025200930208     C* IMPOSTO A ZERO IL FINE PGM
025300930208     C                   MOVEL     '0'           FINL00            1
025400931105     C                   MOVEL     *BLANKS       RSUT             20
025500950309     C                   Z-ADD     *ZEROS        NUMLBL            9 0
025600931115     C*
025700931115     C* RECUPERO LA DATA E L'ORA
025800931115     C                   Z-ADD     UDATE         DATA              6 0
025900931115     C                   TIME                    ORA               6 0
026000000000     C*
026100000000     C                   ENDSR
026200940912     O*------------------------------------------------------------------------*
026300940905     OPRTF198   E            TESTA            02
026400950309     O                                            8 'TNSB40R '
026500931105     O                       RSUT                45
026600950309     O                                           82 'SALVATAGGIO FNLBL       '
026700931115     O                                           90 'DAL:'
026800950309     O                       PARDTI              99
026900931115     O                                          110 'AL:'
027000950309     O                       PARDTF             119
027100931105     O                                          150 'Data:'
027200931105     O                       DATA               159 '  /  /  '
027300931105     O                                          167 'Ora:'
027400931105     O                       ORA                178 '  .  .  '
027500931105     O                                          190 'Pagina:'
027600931105     O                       PAGE          Z    198
027700931105     O          E            TESTA       1  1
027800931105     O                       TES(2)              66
027900931105     O                       TES(3)             132
028000931105     O                       TES(4)             198
028100931105     O          E            TESTA          1
028200931105     O                       TES(1)              66
028300931105     O                       TES(1)             132
028400931105     O                       TES(1)             198
028500931105     O*------------------------------------
028600950309     O          E            ERRLBL         1
028700950309     O                       LBLAAN            +  5
028800950309     O                       LBLLPN            +  1
028900950309     O                       LBLNRN            +  1
029000950309     O                       LBLNSN            +  1
029100950309     O                                         +  1 'ERR. WRITE FNLBL   '
029200931105     O*------------------------------------
029300950308     O          E            STAFIN         1
029400950309     O                                           30 'N.REC.TRASF. FNLBL'
029500950309     O                       NUMLBL        2     44
029600950308     O          E            STAFIN           60
029700931115     O                                           76 '*** FINE STAMPA ***'
029800931115     O                                          150 'Data:'
029900931115     O                       DATA               159 '  /  /  '
030000931115     O                                          167 'Ora:'
030100940912     O                       ORA                178 '  :  :  '
030200940912     O*------------------------------------------------------------------------*
030300931105**
030400931105------------------------------------------------------------------  1
030500931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
030600950308E                                                                   3
030700931105                                                                    4
030800931105                                                                    5
030900931105                                                                    6
031000931105                                                                    7
031100931105                                                                    8
031200931105                                                                    9
031300931105                                                                   10
031400931105                                                                   11
031500931105                                                                   12
031600931105                                                                   13
031700931105                                                                   14
031800931105                                                                   15
