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
001100041122     Ffnblp01l  IF   E           K DISK    usropn
001200041122     Ffnarb01l  IF   E           K DISK    usropn
001300931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001400931105     F                                     FORMLEN(66)
001500931105     F                                     FORMOFL(60)
001600940912     L*------------------------------------------------------------------------*
001700940912     D*------------------------------------------------------------------------*
001800940912     D* SCHIERE
001900940912     D*------------------------------------------------------------------------*
002000041122     D OVRblp          S              1    DIM(39) CTDATA PERRCD(39)
002100041122     D OVRarb          S              1    DIM(39) CTDATA PERRCD(39)
002200041122     D TES             S             66    DIM(15) CTDATA PERRCD(1)
002300950309     D***************************************************************
002400950309     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
002500950309     D DS§DAT          DS
002600950309     D  DSSEC                  1      2  0
002700950309     D  DSANN                  3      4  0
002800950309     D  DSMES                  5      6  0
002900950309     D  DSGIO                  7      8  0
003000950309     D  DSAAS                  1      4  0
003100950309     D  DSMGS                  5      8  0
003200950309     D  DSDAT6                 3      8  0
003300950309     D  DSDAT8                 1      8  0
003400940912     D*------------------------------------------------------------------------*
003500940912     D* INPUT
003600940912     D*------------------------------------------------------------------------*
003700941123     D                 DS
003800941123     D  DSAASI                 1      4  0
003900941123     D  DSMGSI                 5      8  0
004000950309     D  DSDATI                 1      8  0
004100941123     D                 DS
004200941123     D  DSAASF                 1      4  0
004300941123     D  DSMGSF                 5      8  0
004400950309     D  DSDATF                 1      8  0
004500940912     D*-------------------
004600931115     D* PARAMETRI LANCIO PGM
004700940912     D*-------------------
004800931115     D PARAM           DS                  INZ
004900950309     D  PARDTI                 1      8  0                                      DT.INI
005000950309     D  PARDTF                 9     16  0                                      DT.FIN
005100940912     D*-------------------
005200940912     D* ARCHITETTURA
005300940912     D*-------------------
005400931115     D KPJBA         E DS
005500041122     D DSBS50        E DS                  EXTNAME(TIBS55DS)
005600041122     d KAas            s                   Like(tasaas)
005700041122     d Klnp            s                   Like(taslnp)
005800041122     d Knrs            s                   Like(tasnrs)
005900041122     d Knsp            s                   Like(tasnsp)
006000041122     D Cblp            C                   CONST('/FNBLP01L)')
006100041122     D Carb            C                   CONST('/FNARB01L)')
006200940912     C*------------------------------------------------------------------------*
006300940912     C*?MAIN LINE
006400940912     C*------------------------------------------------------------------------*
006500000000     C*
006600930129     C* ROUTINE INIZIALE
006700930129     C                   EXSR      RUTINZ
006800931105     C*
006900931105     C* STAMPA RIGA DI TESTATA
007000931105     C                   EXCEPT    TESTA
007100930601     C*
007200930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
007300930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
007400000000     C*
007500950309     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU FNLBL
007600950309     C                   EXSR      SETLBL
007700930128     C*
007800950309     C* CICLO FINO A FINE FILE FNLBL OPPURE
007900950308     C* RICHIESTA CHIUSURA PGM OPPURE
008000950308     C* FINE PGM PER ERRORE
008100950308     C                   SETOFF                                       98
008200950309     C     FINLBL        DOWEQ     '0'                                          --- 2 -->
008300950308     C     *IN98         ANDEQ     '0'
008400950308     C     FINL00        ANDEQ     '0'
008500950307     C*
008600950309     C                   WRITE     FNLBLS00                             99      *SCRIVO FNLBLS
008700950309     C   99              EXCEPT    ERRLBL
008800950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
008900950310     C                   DELETE    FNLBL000                                     *CANCELLO FNLBL
009000950309     C  N99              ADD       1             NUMLBL                         *REC TRASF FNLBL
009100000000     C*
009200950309     C                   EXSR      LETLBL                                       *SUCCESSIVO FNLBL
009300950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
009400930128     C*
009500950307     C                   ENDDO                                                  <-- 2 ---
009600950307     C                   ENDIF                                                  <-- 1 ---
009700931105     C*
009800931105     C* STAMPA RIGA DI FINE STAMPA
009900931115     C                   Z-ADD     UDATE         DATA              6 0
010000931115     C                   TIME                    ORA               6 0
010100931105     C                   EXCEPT    STAFIN
010200000000     C*
010300000000     C                   SETON                                        LR
010400940912     C*------------------------------------------------------------------------*
010500950309     C*? SETLBL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE FNLBL
010600940912     C*------------------------------------------------------------------------*
010700950309     C     SETLBL        BEGSR
010800931115     C*
010900931115     C* AZZERO CONTATORE RECORD LETTI
011000950309     C                   MOVEL     '0'           FINLBL            1
011100930208     C*
011200930316     C* POSIZIONAMENTO E PRIMA LETTURA
011300941123     C                   Z-ADD     PARDTI        DSDATI
011400941123     C                   Z-ADD     PARDTF        DSDATF
011500950309     C                   Z-ADD     *ZEROS        LBLAAN
011600950309     C     KEYLBL        SETLL     FNLBL01L                           99        *FINE-ARCHIV
011700930625     C*
011800930625     C     *IN99         IFEQ      '1'
011900950309     C                   MOVEL     '1'           FINLBL
012000930625     C                   ELSE
012100950309     C                   EXSR      LETLBL
012200930625     C                   END
012300930208     C*
012400930208     C                   ENDSR
012500940912     C*------------------------------------------------------------------------*
012600950309     C*? LETLBL   LETTURA PROSSIMO RECORD FNLBL
012700940912     C*------------------------------------------------------------------------*
012800950309     C     LETLBL        BEGSR
012900930625     C*
013000930625     C* LEGGO FINO A:
013100930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
013200930625     C*     - TROVATO RECORD VALIDO
013300930625     C                   MOVEL     'N'           WRECOK
013400950309     C     FINLBL        DOWEQ     '0'                                          --- 1 -->
013500930625     C     WRECOK        ANDEQ     'N'
013600930208     C*
013700930316     C* LETTURE SUCCESSIVE ALLA PRIMA
013800950309     C                   READ      FNLBL01L                               99
013900930625     C*
014000930625     C     *IN99         IFEQ      '1'                                          *FINE FILE
014100950309     C     LBLAAN        ORGT      DSAASF                                       *SUPERATO ANNO FINE
014200950309     C                   MOVEL     '1'           FINLBL
014300930625     C                   ELSE
014400950309     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
014500930208     C                   END
014600930625     C*
014700930625     C                   END                                                    <-- 1 ---
014800930208     C*
014900930208     C                   ENDSR
015000950307     C*------------------------------------------------------------------------*
015100950309     C*? CHKREC - CONTROLLA VALIDITA' DEL RECORD
015200950307     C*------------------------------------------------------------------------*
015300950309     C     CHKREC        BEGSR
015400950307     C*
015500950307     C                   MOVEL     'S'           WRECOK            1
015600950309     C*
015700950309     C* TUTTE LE BOLLE DEVONO AVERE LA DATA INFERIORE ALLA DATA DI FINE CANC.
015800950309     C* BOLLA ORIGINALE
015900950309     C     LBLAAO        IFGT      DSAASF
016000950309     C                   MOVEL     'N'           WRECOK
016100950309     C                   ENDIF
016200950309     C     LBLAAO        IFEQ      DSAASF
016300950309     C     LBLMGO        ANDGT     DSMGSF
016400950309     C                   MOVEL     'N'           WRECOK
016500950309     C                   ENDIF
016600950309     C* BOLLA SEGUENTE
016700950309     C     LBLAAN        IFGT      DSAASF
016800950309     C                   MOVEL     'N'           WRECOK
016900950309     C                   ENDIF
017000950309     C     LBLAAN        IFEQ      DSAASF
017100950309     C     LBLMGN        ANDGT     DSMGSF
017200950309     C                   MOVEL     'N'           WRECOK
017300950309     C                   ENDIF
017400950309     C* BOLLA PRECEDENTE
017500950309     C     LBLAAP        IFGT      DSAASF
017600950309     C                   MOVEL     'N'           WRECOK
017700950309     C                   ENDIF
017800950309     C     LBLAAP        IFEQ      DSAASF
017900950309     C     LBLMGP        ANDGT     DSMGSF
018000950309     C                   MOVEL     'N'           WRECOK
018100950309     C                   ENDIF
018200950309     C*
018300990803     C* CONTROLLA SE ESISTE IN TITAS LA BOLLA ORIGINALE
018400041122    1C     WRECOK        IFEQ      'S'
018500950309     C                   Z-ADD     LBLAAO        TASAAS
018600950309     C                   Z-ADD     LBLLPO        TASLNP
018700950309     C                   Z-ADD     LBLNRO        TASNRS
018800950309     C                   Z-ADD     LBLNSO        TASNSP
018900950309     C                   MOVEL     *LOVAL        TASTBL
019000990803     C     KEYTAS        SETLL     TITAS30C                           99
019100990803     C  N99              READ      TITAS30C                               99
019200041122    2C     *IN99         IFEQ      '0'
019300950309     C     TASAAS        ANDEQ     LBLAAO
019400950309     C     TASLNP        ANDEQ     LBLLPO
019500950309     C     TASNRS        ANDEQ     LBLNRO
019600950309     C     TASNSP        ANDEQ     LBLNSO
019700950309     C                   MOVEL     'N'           WRECOK                         *ESISTE, NON CANCELL
019800041122    2C                   ENDIF
019900041122     c*
020000041122     C                   Z-ADD     LBLAAO        kAAS
020100041122     C                   Z-ADD     LBLLPO        kLNP
020200041122     C                   Z-ADD     LBLNRO        kNRS
020300041122     C                   Z-ADD     LBLNSO        kNSP
020400041122     c     kbolfi        setll     fnblp01l
020500041122    2c                   if        %equal(fnblp01l)
020600041122     c                   eval      wrecok='N'
020700041122    2c                   endif
020800041122     c     kbolfi        setll     fnarb01l
020900041122    2c                   if        %equal(fnarb01l)
021000041122     c                   eval      wrecok='N'
021100041122    2c                   endif
021200041122     c
021300041122    1C                   ENDIF
021400950309     C*
021500990803     C* CONTROLLA SE ESISTE IN TITAS LA BOLLA SEGUENTE
021600950309     C     WRECOK        IFEQ      'S'
021700950309     C                   Z-ADD     LBLAAN        TASAAS
021800950309     C                   Z-ADD     LBLLPN        TASLNP
021900950309     C                   Z-ADD     LBLNRN        TASNRS
022000950309     C                   Z-ADD     LBLNSN        TASNSP
022100950309     C                   MOVEL     *LOVAL        TASTBL
022200990803     C     KEYTAS        SETLL     TITAS30C                           99
022300990803     C  N99              READ      TITAS30C                               99
022400950309     C     *IN99         IFEQ      '0'
022500950309     C     TASAAS        ANDEQ     LBLAAN
022600950309     C     TASLNP        ANDEQ     LBLLPN
022700950309     C     TASNRS        ANDEQ     LBLNRN
022800950309     C     TASNSP        ANDEQ     LBLNSN
022900950309     C                   MOVEL     'N'           WRECOK                         *ESISTE, NON CANCELL
023000950309     C                   ENDIF
023100041122     c*
023200041122     C                   Z-ADD     LBLAAN        kAAS
023300041122     C                   Z-ADD     LBLLPN        kLNP
023400041122     C                   Z-ADD     LBLNRN        kNRS
023500041122     C                   Z-ADD     LBLNSN        kNSP
023600041122     c     kbolfi        setll     fnblp01l
023700041122    2c                   if        %equal(fnblp01l)
023800041122     c                   eval      wrecok='N'
023900041122    2c                   endif
024000041122     c     kbolfi        setll     fnarb01l
024100041122    2c                   if        %equal(fnarb01l)
024200041122     c                   eval      wrecok='N'
024300041122    2c                   endif
024400041122     c
024500950309     C                   ENDIF
024600950309     C*
024700990803     C* CONTROLLA SE ESISTE IN TITAS LA BOLLA PRECEDENTE
024800950309     C     WRECOK        IFEQ      'S'
024900950309     C                   Z-ADD     LBLAAP        TASAAS
025000950309     C                   Z-ADD     LBLLPP        TASLNP
025100950309     C                   Z-ADD     LBLNRP        TASNRS
025200950309     C                   Z-ADD     LBLNSP        TASNSP
025300950309     C                   MOVEL     *LOVAL        TASTBL
025400990803     C     KEYTAS        SETLL     TITAS30C                           99
025500990803     C  N99              READ      TITAS30C                               99
025600950309     C     *IN99         IFEQ      '0'
025700950309     C     TASAAS        ANDEQ     LBLAAP
025800950309     C     TASLNP        ANDEQ     LBLLPP
025900950309     C     TASNRS        ANDEQ     LBLNRP
026000950309     C     TASNSP        ANDEQ     LBLNSP
026100950309     C                   MOVEL     'N'           WRECOK                         *ESISTE, NON CANCELL
026200950309     C                   ENDIF
026300041122     c*
026400041122     C                   Z-ADD     LBLAAP        kAAS
026500041122     C                   Z-ADD     LBLLPP        kLNP
026600041122     C                   Z-ADD     LBLNRP        kNRS
026700041122     C                   Z-ADD     LBLNSP        kNSP
026800041122     c     kbolfi        setll     fnblp01l
026900041122    2c                   if        %equal(fnblp01l)
027000041122     c                   eval      wrecok='N'
027100041122    2c                   endif
027200041122     c     kbolfi        setll     fnarb01l
027300041122    2c                   if        %equal(fnarb01l)
027400041122     c                   eval      wrecok='N'
027500041122    2c                   endif
027600950309     C                   ENDIF
027700950307     C*
027800950307     C                   ENDSR
027900940912     C*------------------------------------------------------------------------*
028000940912     C*? RUTINZ - ROUTINE INIZIALE
028100940912     C*------------------------------------------------------------------------*
028200000000     C     RUTINZ        BEGSR
028300931115     C*
028400931115     C* PARAMETRI IN INPUT AL PGM
028500931115     C     *ENTRY        PLIST
028600931115     C                   PARM                    KPJBA
028700931115     C                   MOVEL     KPJBU         PARAM
028800930208     C*
028900990803     C* CHIAVE LETTURA TITAS30C - COMPLETA
029000930621     C     KEYTAS        KLIST
029100941123     C                   KFLD                    TASAAS
029200950309     C                   KFLD                    TASLNP
029300950309     C                   KFLD                    TASNRS
029400950309     C                   KFLD                    TASNSP
029500950309     C                   KFLD                    TASTBL
029600041122     C     Kbolfi        KLIST
029700041122     C                   KFLD                    kAAS
029800041122     C                   KFLD                    kLNP
029900041122     C                   KFLD                    kNRS
030000041122     C                   KFLD                    kNSP
030100950307     C*
030200950309     C* CHIAVE LETTURA FNLBL01L - COMPLETA
030300950309     C     KEYLBL        KLIST
030400950309     C                   KFLD                    LBLAAN
030500950309     C                   KFLD                    LBLLPN
030600950309     C                   KFLD                    LBLNRN
030700950309     C                   KFLD                    LBLNSN
030800930208     C*
030900930208     C* IMPOSTO A ZERO IL FINE PGM
031000930208     C                   MOVEL     '0'           FINL00            1
031100931105     C                   MOVEL     *BLANKS       RSUT             20
031200950309     C                   Z-ADD     *ZEROS        NUMLBL            9 0
031300931115     C*
031400931115     C* RECUPERO LA DATA E L'ORA
031500931115     C                   Z-ADD     UDATE         DATA              6 0
031600931115     C                   TIME                    ORA               6 0
031700041122     c* apro file bolle part e arr della filiale
031800041122     C* CERCO CON IL TIBS55 LA LIBRERIA DI filiale
031900041122     C                   CLEAR                   DSBS50
032000041122     c                   movel     'L'           i50tla
032100041122     c                   Movel     043           I50apo
032200041122     C                   CALL      'TIBS55R'
032300041122     C                   PARM                    DSBS50
032400041122     C
032500041122     C* overdbf  fnblp01l
032600041122     c                   movea     o50ala        ovrblp(30)
032700041122     C                   SETOFF                                       01
032800041122     C                   CLEAR                   COMMAN           80
032900041122     C                   MOVEA     ovrblp        COMMAN
033000041122     c                   eval      comman=%trim(comman)+cblp
033100041122     C                   CALL      'QCMDEXC'
033200041122     C                   PARM                    COMMAN
033300041122     C                   PARM      50            LUNG             15 5
033400041122     C*
033500041122     C                   OPEN      fnblp01l
033600041122     C* overdbf  fnarb01l
033700041122     c                   movea     o50ala        ovrarb(30)
033800041122     C                   SETOFF                                       01
033900041122     C                   CLEAR                   COMMAN           80
034000041122     C                   MOVEA     ovrarb        COMMAN
034100041122     c                   eval      comman=%trim(comman)+carb
034200041122     C                   CALL      'QCMDEXC'
034300041122     C                   PARM                    COMMAN
034400041122     C                   PARM      50            LUNG             15 5
034500041122     C*
034600041122     C                   OPEN      fnarb01l
034700000000     C*
034800000000     C                   ENDSR
034900940912     O*------------------------------------------------------------------------*
035000940905     OPRTF198   E            TESTA            02
035100950309     O                                            8 'TNSB40R '
035200931105     O                       RSUT                45
035300950309     O                                           82 'SALVATAGGIO FNLBL       '
035400931115     O                                           90 'DAL:'
035500950309     O                       PARDTI              99
035600931115     O                                          110 'AL:'
035700950309     O                       PARDTF             119
035800931105     O                                          150 'Data:'
035900931105     O                       DATA               159 '  /  /  '
036000931105     O                                          167 'Ora:'
036100931105     O                       ORA                178 '  .  .  '
036200931105     O                                          190 'Pagina:'
036300931105     O                       PAGE          Z    198
036400931105     O          E            TESTA       1  1
036500931105     O                       TES(2)              66
036600931105     O                       TES(3)             132
036700931105     O                       TES(4)             198
036800931105     O          E            TESTA          1
036900931105     O                       TES(1)              66
037000931105     O                       TES(1)             132
037100931105     O                       TES(1)             198
037200931105     O*------------------------------------
037300950309     O          E            ERRLBL         1
037400950309     O                       LBLAAN            +  5
037500950309     O                       LBLLPN            +  1
037600950309     O                       LBLNRN            +  1
037700950309     O                       LBLNSN            +  1
037800950309     O                                         +  1 'ERR. WRITE FNLBL   '
037900931105     O*------------------------------------
038000950308     O          E            STAFIN         1
038100950309     O                                           30 'N.REC.TRASF. FNLBL'
038200950309     O                       NUMLBL        2     44
038300950308     O          E            STAFIN           60
038400931115     O                                           76 '*** FINE STAMPA ***'
038500931115     O                                          150 'Data:'
038600931115     O                       DATA               159 '  /  /  '
038700931115     O                                          167 'Ora:'
038800940912     O                       ORA                178 '  :  :  '
038900940912     O*------------------------------------------------------------------------*
039000041122**         OVRBLP
039100041122OVRDBF FILE(FNBLP01L) TOFILE(xxxxxxxxxx
039200041122**         OVRARB
039300041122OVRDBF FILE(FNARB01L) TOFILE(
039400931105**
039500931105------------------------------------------------------------------  1
039600931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
039700950308E                                                                   3
039800931105                                                                    4
039900931105                                                                    5
040000931105                                                                    6
040100931105                                                                    7
040200931105                                                                    8
040300931105                                                                    9
040400931105                                                                   10
040500931105                                                                   11
040600931105                                                                   12
040700931105                                                                   13
040800931105                                                                   14
040900931105                                                                   15
