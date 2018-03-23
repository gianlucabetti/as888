000100940912     H*------------------------------------------------------------------------*
000200990802     H* TRASFERIMENTO BOLLE DA TITAS10F A TITASP0F
000300940912     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500990802     F*---
000600990802     F* BOLLE
000700990802     F*---
000800990802     FTITAS38C  UF   E           K DISK
000900990802     F                                     IGNORE(TITAS000)
001000990802     F                                     IGNORE(TITASP00)
001100040220     F                                     INFDS(£DSTAS)
001200990802     FTITASP0F  O  A E           K DISK
001300990802     F*---
001400990802     F* ESTENSIONE BOLLE
001500990802     F*---
001600990802     FTITA730C  UF   E           K DISK
001700990802     F                                     IGNORE(TITA7P00)
001800040220     F                                     INFDS(£DSTA7)
001900990802     FTITA7P0F  O  A E           K DISK
002000990802     F*---
002100990802     F* ANAGRAFICHE
002200990802     F*---
002300990802     FTITAA30C  UF   E           K DISK
002400990802     F                                     IGNORE(TITAAP00)
002500040220     F                                     INFDS(£DSTAA)
002600990802     FTITAAP0F  O  A E           K DISK
002700990802     F*---
002800990802     F* SEGNACOLLI
002900990802     F*---
003000990802     FTITAT30C  UF   E           K DISK
003100990802     F                                     IGNORE(TITATP00)
003200040220     F                                     INFDS(£DSTAT)
003300990802     FTITATP0F  O  A E           K DISK
003400990802     F*---
003500990802     F* RIFERIMENTI
003600990802     F*---
003700990802     FTITA430C  UF   E           K DISK
003800990802     F                                     IGNORE(TITA4P00)
003900040220     F                                     INFDS(£DSTA4)
004000990802     FTITA4P0F  O  A E           K DISK
004100010903     F*---
004200010903     F* ESTENSIONE DETTAGLI SEGNACOLLI
004300010903     F*---
004400010903     FTITAH30C  UF   E           K DISK
004500010903     F                                     IGNORE(TITAHP00)
004600040220     F                                     INFDS(£DSTAH)
004700010903     FTITAHP0F  O  A E           K DISK
004800990802     F*---
004900990802     F* COMPETENZE TESTATA
005000990802     F*---
005100950307     FECCET30C  UF   E           K DISK
005200010829     F                                     IGNORE(ECCETP00)
005300040220     F                                     INFDS(£DSCET)
005400950307     FECCETP0F  O  A E           K DISK
005500990802     F*---
005600990802     F* COMPETENZE DETTAGLIO
005700990802     F*---
005800950307     FECCED30C  UF   E           K DISK
005900010829     F                                     IGNORE(ECCEDP00)
006000040220     F                                     INFDS(£DSCED)
006100950307     FECCEDP0F  O  A E           K DISK
006200030122     F*---
006300030512     F* VOCI C/E TESTATA
006400030122     F*---
006500030122     FECCEV30C  UF   E           K DISK
006600030122     F                                     IGNORE(ECCEVP00)
006700040220     F                                     INFDS(£DScev)
006800030122     FECCEVP0F  O  A E           K DISK
006900030512     F*---
007000030512     F* VOCI C/E ESTENSIONE
007100030512     F*---
007200030512     FECC2V30C  UF   E           K DISK
007300030512     F                                     IGNORE(ECC2VP00)
007400040220     F                                     INFDS(£DSc2v)
007500030512     FECC2VP0F  O  A E           K DISK
007600040303     F*---
007700040303     F* ESTENSIONE TESTATA BOLLA
007800040303     F*---
007900040531     FFIAR531C  UF   E           K DISK
008000040303     F                                     IGNORE(FIAR5P00)
008100040303     F                                     INFDS(£DSAR5)
008200040303     FFIAR5P0F  O  A E           K DISK
008300030512     F*---
008400030512     F* TABELLE
008500030512     F*---
008600180308     FTABEL00F  UF   E           K DISK
008601180308     Ffiapd01l  IF   E           K DISK
008602180308     Ftilad01l  IF   E           K DISK
008603180308     Ftncsb03l  IF   E           K DISK
008604180308     Fbitass0f  o    E             DISK
008605180308     F*---------
008700931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
008800931105     F                                     FORMLEN(66)
008900931105     F                                     FORMOFL(60)
009000940912     L*------------------------------------------------------------------------*
009100040220     D £DSTAS          DS
009200040220     D  NRRTAS               397    400B 0
009300040220     D £DSTA7          DS
009400040220     D  NRRTA7               397    400B 0
009500040220     D £DSTAA          DS
009600040220     D  NRRTAA               397    400B 0
009700040220     D £DSTAT          DS
009800040220     D  NRRTAT               397    400B 0
009900040220     D £DSTA4          DS
010000040220     D  NRRTA4               397    400B 0
010100040220     D £DSTAH          DS
010200040220     D  NRRTAH               397    400B 0
010300040220     D £DSCET          DS
010400040220     D  NRRCET               397    400B 0
010500040220     D £DSCED          DS
010600040220     D  NRRCED               397    400B 0
010700040220     D £DScev          DS
010800040220     D  NRRcev               397    400B 0
010900040220     D £DSc2v          DS
011000040220     D  NRRc2v               397    400B 0
011100040303     D £DSAR5          DS
011200040303     D  NRRAR5               397    400B 0
011300940912     D*------------------------------------------------------------------------*
011400940912     D* SCHIERE
011500940912     D*------------------------------------------------------------------------*
011600931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
011700950309     D* DS TABELLA $3 (DATE AVANZAMENTO TRASFERIMENTO BOLLE)
011800950309     D DS$3          E DS
011900950717     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
012000950717     D DS§DAT          DS
012100950717     D  DSSEC                  1      2  0
012200950717     D  DSANN                  3      4  0
012300950717     D  DSMES                  5      6  0
012400950717     D  DSGIO                  7      8  0
012500950717     D  DSAAS                  1      4  0
012600950717     D  DSMGS                  5      8  0
012700950717     D  DSDAT6                 3      8  0
012800950717     D  DSDAT8                 1      8  0
012900950717     D DS§TIM          DS
013000950717     D  DTORA                  1      2  0
013100950717     D  DTMIN                  3      4  0
013200950717     D  DTSCN                  5      6  0
013300950717     D  DTGIO                  7      8  0
013400950717     D  DTMES                  9     10  0
013500950717     D  DTSEC                 11     12  0
013600950717     D  DTANN                 13     14  0
013700950717     D  DTORA6                 1      6  0
013800950717     D  DTTIME                 1     14  0
013801180308      * RCD FAT FIAR5
013802180308     d dAR5fat       e ds
013803180308     d DTA4A         e ds
013804180308     d DTA4Q         e ds
013805180308     d Dstb          e ds
013806180308     d DTAsflo       e ds
013807180308     D*-------------------
013808180308     d TIBS69ds      e ds                  inz
013809180308     d DS_cnaco      e ds                  inz  extname(CNACO00F)
013810180308     d DS_cnind      e ds                  inz  extname(CNIND00F)
013811180308     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
013812180308     d DS_fncls      e ds                  inz  extname(FNCLS00F)
013813180308     d com04           s              4  0
013815180308     d tmp             s             17
013816180308     d trcapd          s                   like(apdtip) inz('A')
013900940912     D*------------------------------------------------------------------------*
014000180308     D* INPUT
014100940912     D*------------------------------------------------------------------------*
014200951017     D*-------------------
014300951017     D* SCOMPOSIZIONE DATE PARAMETRO
014400951017     D*-------------------
014500941123     D                 DS
014600941123     D  DSAASI                 1      4  0
014700941123     D  DSMGSI                 5      8  0
014800950309     D  DSDATI                 1      8  0
014900941123     D                 DS
015000941123     D  DSAASF                 1      4  0
015100941123     D  DSMGSF                 5      8  0
015200950309     D  DSDATF                 1      8  0
015300951017     D*-------------------
015400951017     D* DATA SPEDIZIONE
015500951017     D*-------------------
015600951017     D                 DS
015700951017     D  TASAAS                 1      4  0
015800951017     D  TASMGS                 5      8  0
015900951017     D  TASDSP                 1      8  0
016000940912     D*-------------------
016100931115     D* PARAMETRI LANCIO PGM
016200940912     D*-------------------
016300931115     D PARAM           DS                  INZ
016400950309     D  PARDTI                 1      8  0                                      DT.INI
016500950309     D  PARDTF                 9     16  0                                      DT.FIN
016600940912     D*-------------------
016700940912     D* ARCHITETTURA
016800940912     D*-------------------
016900931115     D KPJBA         E DS
017000940912     C*------------------------------------------------------------------------*
017100940912     C*?MAIN LINE
017200940912     C*------------------------------------------------------------------------*
017300000000     C*
017400930129     C* ROUTINE INIZIALE
017500930129     C                   EXSR      RUTINZ
017600931105     C*
017700931105     C* STAMPA RIGA DI TESTATA
017800931105     C                   EXCEPT    TESTA
017900930601     C*
018000930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
018100930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
018200000000     C*
018300990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
018400950307     C                   EXSR      SETTAS
018500930128     C*
018600990802     C* CICLO FINO A FINE FILE TITAS OPPURE
018700950308     C* RICHIESTA CHIUSURA PGM OPPURE
018800950308     C* FINE PGM PER ERRORE
018900950308     C                   SETOFF                                       98
019000950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
019100950308     C     *IN98         ANDEQ     '0'
019200950308     C     FINL00        ANDEQ     '0'
019300010124     C*
019400010124     C* CANCELLA LA BOLLA
019500990802     C                   DELETE    TITAS010                                     *CANCELLO TITAS10F
019600990802     C                   WRITE     TITASP00                             99      *SCRIVO TITASP0F
019700950307     C   99              EXCEPT    ERRTAS
019800950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
019900990802     C  N99              ADD       1             NUMTAS                         *REC TRASF TITAS
019901180308     C  N99              exsr      trasferisci
020000950307     C*
020100990802     C* SE ESISTE TITA7 TRASFERISCO ANCHE QUELLO
020200950307     C     TASSV3        IFNE      *BLANKS                                      --- 3 -->
020300990802     C                   EXSR      SETTA7                                       *PRIMO TITA7
020400950307     C     FINTA7        DOWEQ     '0'                                          --- 4 -->
020500990802     C                   DELETE    TITA7000                                     *CANCELLO TITA710
020600990802     C                   WRITE     TITA7P00                             99      *SCRIVO TITA7P
020700950307     C   99              EXCEPT    ERRTA7
020800950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
020900990802     C  N99              ADD       1             NUMTA7                         *REC TRASF TITA7
021000990802     C                   EXSR      LETTA7                                       *SUCCESSIVO TITA7
021100950307     C                   ENDDO                                                  <-- 4 ---
021200950307     C                   ENDIF                                                  <-- 3 ---
021300990802     C*
021400990802     C* TRASFERISCE TITAA - ANAGRAFICHE
021500990802     C                   EXSR      SETTAA                                       *PRIMO TITAA
021600990802DO  3C     FINTAA        DOWEQ     '0'
021700990802     C                   DELETE    TITAA000                                     *CANCELLA TITAA10
021800990802     C                   WRITE     TITAAP00                             99      *SCRIVE TITAAP
021900990802     C   99              EXCEPT    ERRTAA
022000990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
022100990802     C  N99              ADD       1             NUMTAA                         *REC TRASF TITAA
022200990802     C                   EXSR      LETTAA                                       *SUCCESSIVO TITAA
022300990802E   3C                   ENDDO
022400990802     C*
022500990802     C* TRASFERISCE TITAT - SEGNACOLLI
022600990802     C                   EXSR      SETTAT                                       *PRIMO TITAT
022700990802     C     FINTAT        DOWEQ     '0'                                          --- 3 -->
022800990802     C                   DELETE    TITAT000                                     *CANCELLA TITAT10
022900990802     C                   WRITE     TITATP00                             99      *SCRIVE TITATP
023000990802     C   99              EXCEPT    ERRTAT
023100990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
023200990802     C  N99              ADD       1             NUMTAT                         *REC TRASF TITAT
023300990802     C                   EXSR      LETTAT                                       *SUCCESSIVO TITAT
023400990802     C                   ENDDO                                                  <-- 3 ---
023500990802     C*
023600990802     C* TRASFERISCE TITA4 - RIFERIMENTI
023700990802     C                   EXSR      SETTA4                                       *PRIMO TITA4
023800990802     C     FINTA4        DOWEQ     '0'                                          --- 3 -->
023900990802     C                   DELETE    TITA4000                                     *CANCELLA TITA410
024000990802     C                   WRITE     TITA4P00                             99      *SCRIVE TITA4P
024100990802     C   99              EXCEPT    ERRTA4
024200990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
024300990802     C  N99              ADD       1             NUMTA4                         *REC TRASF TITA4
024400990802     C                   EXSR      LETTA4                                       *SUCCESSIVO TITA4
024500990802     C                   ENDDO                                                  <-- 3 ---
024600010903     C*
024700010903     C* TRASFERISCE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
024800010903     C                   EXSR      SETTAH                                       *PRIMO TITAH
024900010903     C     FINTAH        DOWEQ     '0'                                          --- 3 -->
025000010903     C                   DELETE    TITAH000                                     *CANCELLA TITAH10
025100010903     C                   WRITE     TITAHP00                             99      *SCRIVE TITAHP
025200010903     C   99              EXCEPT    ERRTAH
025300010903     C   99              MOVEL     '1'           FINL00                         *FINE PGM
025400010903     C  N99              ADD       1             NUMTAH                         *REC TRASF TITAH
025500010903     C                   EXSR      LETTAH                                       *SUCCESSIVO TITAH
025600010903     C                   ENDDO                                                  <-- 3 ---
025700010124     C*
025800010124     C* SE ESISTE ECCET LO TRASFERISCE
025900010124     C                   Z-ADD     TASAAS        CETAAS                         ANNO SPEDIZIONE
026000010124     C                   Z-ADD     TASLNP        CETLNP                         LINEA DI PARTENZA
026100010124     C                   Z-ADD     TASNRS        CETNRS                         NUMERO SERIE
026200010124     C                   Z-ADD     TASNSP        CETNSP                         NUMERO SPEDIZIONE
026300010124     C                   MOVEL     TASTBL        CETTBL                         TIPO BOLLA
026400010124     C     KEYCET        CHAIN     ECCET30C                           99
026500010124     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
026600010829     C                   DELETE    ECCET000                                     *CANCELLO
026700010829     C                   WRITE     ECCETP00                             99      *SCRIVO ECCETP
026800010124     C   99              EXCEPT    ERRCET
026900010124     C   99              MOVEL     '1'           FINL00                         *FINE PGM
027000010124     C  N99              ADD       1             NUMCET                         *REC TRASF ECCET
027100010124     C*
027200010316     C* SE ESISTE ECCED TRASFERISCO ANCHE QUELLO
027300010124     C     CETDET        IFEQ      'S'                                          --- 4 -->
027400010124     C                   EXSR      SETCED                                       *PRIMO ECCED
027500010124     C     FINCED        DOWEQ     '0'                                          --- 5 -->
027600010829     C                   DELETE    ECCED000                                     *CANCELLO
027700010829     C                   WRITE     ECCEDP00                             99      *SCRIVO ECCEDP
027800010124     C   99              EXCEPT    ERRCED
027900010124     C   99              MOVEL     '1'           FINL00                         *FINE PGM
028000010124     C  N99              ADD       1             NUMCED                         *REC TRASF ECCED
028100010124     C                   EXSR      LETCED                                       *SUCCESSIVO ECCED
028200010124     C                   ENDDO                                                  <-- 5 ---
028300010124     C                   ENDIF                                                  <-- 4 ---
028400010124     C                   ENDIF                                                  <-- 3 ---
028500030122     C*
028600030122     C* SE ESISTE ECCEV LO TRASFERISCE
028700030122     C                   Z-ADD     TASAAS        CEVAAS                         ANNO SPEDIZIONE
028800030122     C                   Z-ADD     TASLNP        CEVLNP                         LINEA DI PARTENZA
028900030122     C                   Z-ADD     TASNRS        CEVNRS                         NUMERO SERIE
029000030122     C                   Z-ADD     TASNSP        CEVNSP                         NUMERO SPEDIZIONE
029100030122     C                   MOVEL     TASTBL        CEVTBL                         TIPO BOLLA
029200030122     C     KEYCEV        CHAIN     ECCEV30C                           99
029300030122     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
029400030122     C                   DELETE    ECCEV000                                     *CANCELLO
029500030122     C                   WRITE     ECCEVP00                             99      *SCRIVO ECCEVP
029600030122     C   99              EXCEPT    ERRCEV
029700030122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
029800030122     C  N99              ADD       1             NUMCEV                         *REC TRASF ECCEV
029900030122     C                   ENDIF                                                  <-- 3 ---
030000030512     C*
030100030512     C* SE ESISTE ECC2V LO TRASFERISCE
030200030512     C                   Z-ADD     TASAAS        C2VAAS                         ANNO SPEDIZIONE
030300030512     C                   Z-ADD     TASLNP        C2VLNP                         LINEA DI PARTENZA
030400030512     C                   Z-ADD     TASNRS        C2VNRS                         NUMERO SERIE
030500030512     C                   Z-ADD     TASNSP        C2VNSP                         NUMERO SPEDIZIONE
030600030512     C                   MOVEL     TASTBL        C2VTBL                         TIPO BOLLA
030700030512     C     KEYC2V        CHAIN     ECC2V30C                           99
030800030512     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
030900030512     C                   DELETE    ECC2V000                                     *CANCELLO
031000030512     C                   WRITE     ECC2VP00                             99      *SCRIVO ECC2VP
031100030512     C   99              EXCEPT    ERRC2V
031200030512     C   99              MOVEL     '1'           FINL00                         *FINE PGM
031300030512     C  N99              ADD       1             NUMC2V                         *REC TRASF ECC2V
031400030512     C                   ENDIF                                                  <-- 3 ---
031500040303     C*
031600040303     C* SE ESISTE FIAR5 LO TRASFERISCE
031700040303     C                   Z-ADD     TASAAS        AR5AAS                         ANNO SPEDIZIONE
031800040303     C                   Z-ADD     TASLNP        AR5LNP                         LINEA DI PARTENZA
031900040303     C                   Z-ADD     TASNRS        AR5NRS                         NUMERO SERIE
032000040303     C                   Z-ADD     TASNSP        AR5NSP                         NUMERO SPEDIZIONE
032100040531     C     KEYAR5        CHAIN     FIAR531C                           99
032200040310     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
032300040303     C                   DELETE    FIAR5000                                     *CANCELLO
032400040303     C                   WRITE     FIAR5P00                             99      *SCRIVO ECC2VP
032500040303     C   99              EXCEPT    ERRAR5
032600040303     C   99              MOVEL     '1'           FINL00                         *FINE PGM
032700040303     C  N99              ADD       1             NUMAR5                         *REC TRASF ECC2V
032800040531     C     KEYAR5        READE     FIAR531C                               99
032900040310     C                   enddo                                                  <-- 3 ---
033000980325     C*
033100980325     C* LETTURA SUCCESSIVA BOLLE
033200990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
033300950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
033400930128     C*
033500950307     C                   ENDDO                                                  <-- 2 ---
033600950307     C                   ENDIF                                                  <-- 1 ---
033700950309     C*
033800950309     C* AGGIORNA LA TABELLA CON L'ULTIMA DATA DI TRASFERIM. (SOLO SE TUTTO OK)
033900950309     C     *IN98         IFEQ      '0'
034000950309     C     FINL00        ANDEQ     '0'
034100950309     C                   EXSR      AGGTAB
034200950309     C                   ENDIF
034300931105     C*
034400931105     C* STAMPA RIGA DI FINE STAMPA
034500950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
034600931105     C                   EXCEPT    STAFIN
034700000000     C*
034800000000     C                   SETON                                        LR
034900940912     C*------------------------------------------------------------------------*
035000990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
035100940912     C*------------------------------------------------------------------------*
035200950307     C     SETTAS        BEGSR
035300931115     C*
035400931115     C* AZZERO CONTATORE RECORD LETTI
035500950307     C                   MOVEL     '0'           FINTAS            1
035600930208     C*
035700930316     C* POSIZIONAMENTO E PRIMA LETTURA
035800941123     C                   Z-ADD     PARDTI        DSDATI
035900941123     C                   Z-ADD     PARDTF        DSDATF
036000941123     C                   Z-ADD     DSAASI        TASAAS
036100941123     C                   Z-ADD     DSMGSI        TASMGS
036200990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
036300930625     C*
036400930625     C     *IN99         IFEQ      '1'
036500950307     C                   MOVEL     '1'           FINTAS
036600930625     C                   ELSE
036700950307     C                   EXSR      LETTAS
036800930625     C                   END
036900930208     C*
037000930208     C                   ENDSR
037100940912     C*------------------------------------------------------------------------*
037200990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
037300940912     C*------------------------------------------------------------------------*
037400950307     C     LETTAS        BEGSR
037500930625     C*
037600930625     C* LEGGO FINO A:
037700930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
037800930625     C*     - TROVATO RECORD VALIDO
037900930625     C                   MOVEL     'N'           WRECOK
038000950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
038100930625     C     WRECOK        ANDEQ     'N'
038200930208     C*
038300930316     C* LETTURE SUCCESSIVE ALLA PRIMA
038400990802     C                   READ      TITAS38C                               99
038500951017     C*
038600930625     C     *IN99         IFEQ      '1'                                          *FINE FILE
038700951017     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
038800950307     C                   MOVEL     '1'           FINTAS
038900930625     C                   ELSE
039000930625     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
039100930208     C                   END
039200930625     C*
039300930625     C                   END                                                    <-- 1 ---
039400930208     C*
039500930208     C                   ENDSR
039600950307     C*------------------------------------------------------------------------*
039700990802     C*? SETTA7 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA7
039800950307     C*------------------------------------------------------------------------*
039900950307     C     SETTA7        BEGSR
040000950307     C*
040100950307     C* AZZERO CONTATORE RECORD LETTI
040200950307     C                   MOVEL     '0'           FINTA7            1
040300950307     C*
040400950307     C* POSIZIONAMENTO E PRIMA LETTURA
040500950307     C                   Z-ADD     TASAAS        TA7AAS
040600950307     C                   Z-ADD     TASLNP        TA7LNP
040700950307     C                   Z-ADD     TASNRS        TA7NRS
040800950307     C                   Z-ADD     TASNSP        TA7NSP
040900950307     C                   MOVEL     TASTBL        TA7TBL
041000010124     C     KEYTA7        CHAIN     TITA730C                           99        *FINE-ARCHIV
041100950307     C     *IN99         IFEQ      '1'
041200950307     C                   MOVEL     '1'           FINTA7
041300950307     C                   END
041400950307     C*
041500950307     C                   ENDSR
041600950307     C*------------------------------------------------------------------------*
041700990802     C*? LETTA7 - LETTURA PROSSIMO RECORD TITA7
041800950307     C*------------------------------------------------------------------------*
041900950307     C     LETTA7        BEGSR
042000950307     C*
042100990802     C                   READ      TITA730C                               99
042200950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
042300950307     C     TA7AAS        ORNE      TASAAS
042400950307     C     TA7LNP        ORNE      TASLNP
042500950307     C     TA7NRS        ORNE      TASNRS
042600950307     C     TA7NSP        ORNE      TASNSP
042700950307     C     TA7TBL        ORNE      TASTBL
042800950307     C                   MOVEL     '1'           FINTA7
042900010124     C                   ENDIF
043000950307     C*
043100950307     C                   ENDSR
043200990802     C*------------------------------------------------------------------------*
043300990802     C*? SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
043400990802     C*------------------------------------------------------------------------*
043500990802     C     SETTAA        BEGSR
043600990802     C*
043700990802     C* AZZERO CONTATORE RECORD LETTI
043800990802     C                   MOVEL     '0'           FINTAA            1
043900990802     C*
044000990802     C* POSIZIONAMENTO E PRIMA LETTURA
044100990802     C                   Z-ADD     TASAAS        TAAAAS
044200990802     C                   Z-ADD     TASLNP        TAALNP
044300990802     C                   Z-ADD     TASNRS        TAANRS
044400990802     C                   Z-ADD     TASNSP        TAANSP
044500010124     C     KEYTAA        CHAIN     TITAA30C                           99        *FINE FILE
044600990802IF  1C     *IN99         IFEQ      '1'
044700990802     C                   MOVEL     '1'           FINTAA
044800990802E   1C                   ENDIF
044900990802     C*
045000990802     C                   ENDSR
045100990802     C*------------------------------------------------------------------------*
045200990802     C*? LETTAA - LETTURA PROSSIMO RECORD TITAA - ANAGRAFICHE
045300990802     C*------------------------------------------------------------------------*
045400990802     C     LETTAA        BEGSR
045500010124     C*
045600990802     C                   READ      TITAA30C                               99
045700990802IF  2C     *IN99         IFEQ      '1'                                          *FINE FILE
045800990802     C     TAAAAS        ORNE      TASAAS
045900990802     C     TAALNP        ORNE      TASLNP
046000990802     C     TAANRS        ORNE      TASNRS
046100990802     C     TAANSP        ORNE      TASNSP
046200990802     C                   MOVEL     '1'           FINTAA
046300990802E   2C                   ENDIF
046400990802     C*
046500990802     C                   ENDSR
046600990802     C*------------------------------------------------------------------------*
046700990802     C*? SETTAT - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAT - SEGNACOLLI
046800990802     C*------------------------------------------------------------------------*
046900990802     C     SETTAT        BEGSR
047000990802     C*
047100990802     C* AZZERO CONTATORE RECORD LETTI
047200990802     C                   MOVEL     '0'           FINTAT            1
047300990802     C*
047400990802     C* POSIZIONAMENTO E PRIMA LETTURA
047500990802     C                   Z-ADD     TASAAS        TATAAS
047600990802     C                   Z-ADD     TASLNP        TATLNP
047700990802     C                   Z-ADD     TASNRS        TATNRS
047800990802     C                   Z-ADD     TASNSP        TATNSP
047900010124     C     KEYTAT        CHAIN     TITAT30C                           99        *FINE FILE
048000990802IF  1C     *IN99         IFEQ      '1'
048100990802     C                   MOVEL     '1'           FINTAT
048200990802E   1C                   ENDIF
048300990802     C*
048400990802     C                   ENDSR
048500990802     C*------------------------------------------------------------------------*
048600990802     C*? LETTAT - LETTURA PROSSIMO RECORD TITAT - SEGNACOLLI
048700990802     C*------------------------------------------------------------------------*
048800990802     C     LETTAT        BEGSR
048900010124     C*
049000990802     C                   READ      TITAT30C                               99
049100990802IF  2C     *IN99         IFEQ      '1'                                          *FINE FILE
049200990802     C     TATAAS        ORNE      TASAAS
049300990802     C     TATLNP        ORNE      TASLNP
049400990802     C     TATNRS        ORNE      TASNRS
049500990802     C     TATNSP        ORNE      TASNSP
049600990802     C                   MOVEL     '1'           FINTAT
049700010124E   1C                   ENDIF
049800990802     C*
049900990802     C                   ENDSR
050000990802     C*------------------------------------------------------------------------*
050100990802     C*? SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
050200990802     C*------------------------------------------------------------------------*
050300990802     C     SETTA4        BEGSR
050400990802     C*
050500990802     C* AZZERO CONTATORE RECORD LETTI
050600990802     C                   MOVEL     '0'           FINTA4            1
050700990802     C*
050800990802     C* POSIZIONAMENTO E PRIMA LETTURA
050900990802     C                   Z-ADD     TASAAS        TA4AAS
051000990802     C                   Z-ADD     TASLNP        TA4LNP
051100990802     C                   Z-ADD     TASNRS        TA4NRS
051200990802     C                   Z-ADD     TASNSP        TA4NSP
051300010124     C     KEYTA4        CHAIN     TITA430C                           99        *FINE FILE
051400990802IF  1C     *IN99         IFEQ      '1'
051500990802     C                   MOVEL     '1'           FINTA4
051600990802E   1C                   ENDIF
051700990802     C*
051800990802     C                   ENDSR
051900990802     C*------------------------------------------------------------------------*
052000990802     C*? LETTA4 - LETTURA PROSSIMO RECORD TITA4 - RIFERIMENTI
052100990802     C*------------------------------------------------------------------------*
052200990802     C     LETTA4        BEGSR
052300010124     C*
052400990802     C                   READ      TITA430C                               99
052500990802IF  2C     *IN99         IFEQ      '1'                                          *FINE FILE
052600990802     C     TA4AAS        ORNE      TASAAS
052700990802     C     TA4LNP        ORNE      TASLNP
052800990802     C     TA4NRS        ORNE      TASNRS
052900990802     C     TA4NSP        ORNE      TASNSP
053000990802     C                   MOVEL     '1'           FINTA4
053100990802E   2C                   ENDIF
053200990802     C*
053300990802     C                   ENDSR
053400010903     C*------------------------------------------------------------------------*
053500010903     C*? SETTAH - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
053600010903     C*------------------------------------------------------------------------*
053700010903     C     SETTAH        BEGSR
053800010903     C*
053900010903     C* AZZERO CONTATORE RECORD LETTI
054000010903     C                   MOVEL     '0'           FINTAH            1
054100010903     C*
054200010903     C* POSIZIONAMENTO E PRIMA LETTURA
054300010903     C                   Z-ADD     TASAAS        TAHAAS
054400010903     C                   Z-ADD     TASLNP        TAHLNP
054500010903     C                   Z-ADD     TASNRS        TAHNRS
054600010903     C                   Z-ADD     TASNSP        TAHNSP
054700010903     C     KEYTAH        CHAIN     TITAH30C                           99        *FINE FILE
054800010903IF  1C     *IN99         IFEQ      '1'
054900010903     C                   MOVEL     '1'           FINTAH
055000010903E   1C                   ENDIF
055100010903     C*
055200010903     C                   ENDSR
055300010903     C*------------------------------------------------------------------------*
055400010903     C*? LETTAH - LETTURA PROSSIMO RECORD TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
055500010903     C*------------------------------------------------------------------------*
055600010903     C     LETTAH        BEGSR
055700010903     C*
055800010903     C                   READ      TITAH30C                               99
055900010903IF  2C     *IN99         IFEQ      '1'                                          *FINE FILE
056000010903     C     TAHAAS        ORNE      TASAAS
056100010903     C     TAHLNP        ORNE      TASLNP
056200010903     C     TAHNRS        ORNE      TASNRS
056300010903     C     TAHNSP        ORNE      TASNSP
056400010903     C                   MOVEL     '1'           FINTAH
056500010903E   2C                   ENDIF
056600010903     C*
056700010903     C                   ENDSR
056800950307     C*------------------------------------------------------------------------*
056900950307     C*? SETCED - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ECCED
057000950307     C*------------------------------------------------------------------------*
057100950307     C     SETCED        BEGSR
057200950307     C*
057300950307     C* AZZERO CONTATORE RECORD LETTI
057400950307     C                   MOVEL     '0'           FINCED            1
057500950307     C*
057600950307     C* POSIZIONAMENTO E PRIMA LETTURA
057700950307     C                   Z-ADD     TASAAS        CEDAAS
057800950307     C                   Z-ADD     TASLNP        CEDLNP
057900950307     C                   Z-ADD     TASNRS        CEDNRS
058000950307     C                   Z-ADD     TASNSP        CEDNSP
058100950307     C                   MOVEL     TASTBL        CEDTBL
058200010124     C     KEYCED        CHAIN     ECCED30C                           99        *FINE-ARCHIV
058300950307     C     *IN99         IFEQ      '1'
058400950307     C                   MOVEL     '1'           FINCED
058500950307     C                   END
058600950307     C*
058700950307     C                   ENDSR
058800950307     C*------------------------------------------------------------------------*
058900950307     C*? LETCED - LETTURA PROSSIMO RECORD ECCED
059000950307     C*------------------------------------------------------------------------*
059100950307     C     LETCED        BEGSR
059200010124     C*
059300950307     C                   READ      ECCED30C                               99
059400950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
059500950307     C     CEDAAS        ORNE      TASAAS
059600950307     C     CEDLNP        ORNE      TASLNP
059700950307     C     CEDNRS        ORNE      TASNRS
059800950307     C     CEDNSP        ORNE      TASNSP
059900950307     C     CEDTBL        ORNE      TASTBL
060000950307     C                   MOVEL     '1'           FINCED
060100950307     C                   END                                                    <-- 1 ---
060200950307     C*
060300950307     C                   ENDSR
060400950307     C*------------------------------------------------------------------------*
060500950307     C*? CHKREC - CONTROLLA VALIDITA' DEL RECORD
060600950307     C*------------------------------------------------------------------------*
060700950307     C     CHKREC        BEGSR
060800950307     C*
060900950307     C                   MOVEL     'S'           WRECOK            1
061000950307     C*
061100950307     C                   ENDSR
061200950309     C*------------------------------------------------------------------------*
061300950309     C*?AGGTAB - AGGIORNA LA TABELLA '$3' CON ULTIMA DATA TRASF. TAS00 > TAS10
061400950309     C*------------------------------------------------------------------------*
061500950309     C     AGGTAB        BEGSR
061600950309     C*
061900950309     C                   Z-ADD     1             KTBUTE
062000950309     C                   MOVEL     '$3'          KTBCOD
062100950309     C                   MOVEL     '1'           KTBKEY
062200950309     C     KEYTAB        CHAIN     TABEL00F                           99
062300950309     C     *IN99         IFEQ      *ON                                          *NON TROVATA
062400950309     C                   ELSE                                                   *TROVATA
062500950309     C                   MOVEL     TBLUNI        DS$3
062600950309     C                   Z-ADD     PARDTF        §$3D10                         *ULT.DATA TRASF 10>P
062700950309     C                   MOVEL     DS$3          TBLUNI
062800980828     C                   MOVEL     *BLANKS       TBLFTR
062900980828     C                   Z-ADD     *ZEROS        TBLDTR
063000950309     C                   UPDATE    TABEL
063100950309     C                   ENDIF
063400950309     C*
063500950309     C                   ENDSR
063600950717     C*------------------------------------------------------------------------*
063700950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
063800950717     C*------------------------------------------------------------------------*
063900950717     C     SUBDAT        BEGSR
064000950717     C                   TIME                    DTTIME
064100950717     C                   Z-ADD     DTANN         DSANN
064200950717     C                   Z-ADD     DTMES         DSMES
064300950717     C                   Z-ADD     DTGIO         DSGIO
064400950717     C                   Z-ADD     DTSEC         DSSEC
064500180308     C                   ENDSR
064501180308     C*------------------------------------------------------------------------*
064502180308     C*  trasferisci SCRITTURA FILE STORICO TITASS
064503180308     C*------------------------------------------------------------------------*
064504180308     C     trasferisci   BEGSR
064505180308      * verifico se bolla da trasferire se di recupero ignoro
064506180308     c                   movel(p)  tastbl        tblkey
064507180308     c     ktab          chain     tabel00f
064508180308     c                   if        not %found(tabel00f)
064509180308     c                   leavesr
064510180308     c                   end
064511180308     c                   movel(p)  tbluni        dstb
064512180308     c                   if        §TBRBL ='R'
064513180308     c                   leavesr
064514180308     c                   end
064515180308      * cerco dati per completamento campi informazioni bolla
064516180308     c                   exsr      newta4
064517180308     c                   exsr      newtaa
064518180308     c                   exsr      recana
064519180308     c                   exsr      reciva
064520180308      *
064521180308     c                   movel     tasflo        dtasflo
064522180308     c* memorizzo directory laguna ter sapere dove è stata archiviata la spe
064523180308     c                   clear                   tasdtfp
064524180308     c                   if        §floiml <> ' '
064525180308     c                   move      tasaas        ladaas
064526180308     c                   move      taslnp        ladlnp
064527180308     c                   move      tasnrs        ladnrs
064528180308     c                   move      tasnsp        ladnsp
064529180308     c     klad          chain     tilad01l
064530180308     c                   If        %found(tilad01l)
064531180308     c                   movel     laddir        tasdtfp
064532180308     c                   end
064533180308     c                   end
064534180308      * Decodifica padroncino
064535180308     c                   If        taspdc <> *zeros
064536180308     c     Kapd          chain     fiapd01l
064537180308     c                   If        %found(fiapd01l)
064538180308     c                   eval      taspdd = apdrsc
064539180308     c                   endif
064540180308     c                   endif
064541180308
064542180308      * recupero c/assegno
064543180308     c                   if        tasfca = 'S'
064544180308     C     Ktas          CHAIN     TNCSB03L
064545180308     C                   IF        %found(tncsb03l)                             --- 2 -->
064546180313     c* segna importo solo se non annullato
064547180313     C                   IF        csbsta <> 9                                  --- 2 -->
064548180308     c                   z-add     csbcas        Tascas
064549180308     c                   movel     csbvca        tasvca
064550180313     c                   endif
064551180308     c                   endif
064552180308     c                   endif
064553180308
064554180308     c                   move      tasksc        com04
064555180308      * PARTITA IVA / CODICE FISCALE
064556180308     c                   If        %subst(tastbl:1:1) = 'A'  and
064557180308     c                             tascpd <> *blanks
064558180308     c                   eval      tasiva  = tascpd
064559180308     c                   endif
064560180308      * se partita iva non valorizzata
064561180308     c                   If        tasiva = *blanks  or tascdf = *blanks
064562180308      * codificato franco o assegnato
064563180308     c                   if        com04  <>  9999  and com04  <>  8888
064564180308     c                   exsr      reciva
064565180308     c                   if        o69err <> *on
064566180308     c                   if        tascdf = *blanks
064567180308     c                   eval      tasiva = indiva
064568180308     c                   endif
064569180308      * se devo cercare anche il codife fiscale
064570180308     c                   if        tascdf = *blanks
064571180308     c                   eval      tascdf = indcdf
064572180308     c                   endif
064573180308
064574180308     c                   endif
064575180308     c                   endif
064576180308     c                   endif
064577180308      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
064578180308     c                   If        Tasfns <> 'S' and tasncd = *zeros
064579180308     c                   eval      tassss = 'S'
064580180308     c                   endif
064581180308
064582180308      * verifico se la ds dar5fat è valorizzata
064583180308     c                   clear                   dar5fat
064584180308     C     Kar5          CHAIN     FIAR531C
064585180308      *se trovo rekord FAT mi salvo i dati
064586180308     c                   if        %found(fiar531c)
064587180308     c                   movel     ar5uni        dar5fat
064588180308     c                   endif
064589180308     c                   If        §ar5pktas > 0
064590180308     c                   eval      taspkf = §ar5pktas
064591180308     c                   endif
064592180308
064593180308     c                   If        §ar5vltas > 0
064594180308     c                   eval      tasvlf = §ar5vltas
064595180308     c                   endif
064596180308
064597180309     c                   write     bitass00                             99
064598180309     C   99              EXCEPT    ERRTASBI
064599180309     C  N99              ADD       1             NUMTASBI
064600180309     c                   clear                   bitass00
064601180308     C                   ENDSR
064602180308     C*
064603180308     C*------------------------------------------------------------------------*
064604180308     C*  newTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
064605180308     C*------------------------------------------------------------------------*
064606180313     C     newTA4        BEGSR
064607180313     C*
064608180313     C* POSIZIONAMENTO E PRIMA LETTURA
064609180313     C     Ktas          setll     TITA430C
064610180313     c                   do        *hival
064611180313     C     Ktas          reade     TITA430C
064612180313     C                   IF        %eof(tita430c)
064613180313     C                   leave
064614180313      * recupero riferimento mittente alfanumerico e natura merce
064615180313     c                   endif
064616180313     c                   exsr      recta4
064617180313     C                   ENDdo
064618180313     C*
064619180313     C                   ENDSR
064620180308     C*------------------------------------------------------------------------*
064621180308     C*  recta4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
064622180308     C*------------------------------------------------------------------------*
064623180313     C     recta4        BEGSR
064624180313      * riferimento mittente alfanumerico
064625180313     c                   IF        ta4trc = 'A'
064626180313     c                   movel     ta4not        dta4a
064627180313     c                   movel     §ta4aRMA      Tasrma
064628180313     c                   movel     §ta4anas      Tasnas
064629180313     c                   endif
064630180313      * CODICE FISCALE
064631180313     c                   IF        ta4trc = 'Q'
064632180313     c                   movel     ta4not        dta4q
064633180313      * VERIFICO SE FRANCO O ASSEGNATO
064634180313     c                   If        %subst(tastbl:1:1) = 'F'
064635180313     c                   movel     §TA4QCFM      Tascdf
064636180313     c                   endif
064637180313     c                   If        %subst(tastbl:1:1) = 'A'
064638180313     c                   movel     §TA4QCFD      Tascdf
064639180313     c                   endif
064640180313     c                   endif
064641180313
064642180313     c                   endsr
064643180308     C*------------------------------------------------------------------------*
064644180308     C*  newtaa - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
064645180308     C*------------------------------------------------------------------------*
064646180312     C     newtaa        BEGSR
064647180312     C*
064648180312     C* POSIZIONAMENTO E PRIMA LETTURA
064649180312     C     Ktaa          setll     TITAA30C
064650180312     c                   do        *hival
064651180312     C     Ktaa          reade     TITAA30C
064652180312     C                   IF        %eof(titaa30c)
064653180312     C                   leave
064654180312      * recupero dati anagrafici del mittente
064655180312     c                   endif
064656180312     c                   exsr      rectaa
064657180312     C                   ENDdo
064658180312     C*
064659180312     C                   ENDSR
064660180312     C*------------------------------------------------------------------------*
064661180312     C*  RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
064662180312     C*------------------------------------------------------------------------*
064663180312     C     RECTAA        BEGSR
064664180312      * dati anagrafici mittente
064665180312     c                   IF        taatrc = 'M'
064666180312     c                   eval      tasrsm = taarsc
064667180312     c                   eval      tasinm = taaind
064668180312     c                   eval      tascam = taacap
064669180312     c                   eval      taslom = taaloc
064670180312     c                   eval      tasprm = taaprv
064671180312     c                   eval      tasnzm = taanaz
064672180313      * iva  per i franchi non codificati
064673180312     c                   if        %subst(tastbl:1:1) = 'F'
064674180312     c                   eval      tasiva = taacpi
064675180312     c                   endif
064676180312
064677180312     c                   endif
064678180312
064679180312     c                   endsr
064680180308     C*------------------------------------------------------------------------*
064681180308     C*  RECANA - RECUPERO DA ANAGRAFICO CLIENTE
064682180308     C*------------------------------------------------------------------------*
064683180308     C     RECANA        BEGSR
064684180308     C                   clear                   ds_CNACO
064685180308     C                   clear                   ds_CNIND
064686180308     C                   clear                   ds_CNCLP
064687180308     C                   clear                   ds_FNCLS
064688180308     c                   z-add     TASccm        I69kac
064689180308     c                   z-add     TASccm        I69kin
064690180308     c                   call      'TIBS69R'
064691180308     c                   parm                    TIBS69ds
064692180308     c                   parm                    ds_CNACO
064693180308     c                   parm                    ds_CNIND
064694180308     c                   parm                    ds_CNCLP
064695180308     c                   parm                    ds_FNCLS
064696180308if  3c                   if        O69err <> *on
064697180308     c                   movel     ACOrag        TASrsm
064698180308     c                   movel     INDvia        TASinm
064699180308     c                   movel     INDcap        TAScam
064700180308     c                   movel     INDcit        TASlom
064701180308     c                   movel     INDprv        TASprm
064702180308     c                   movel     INDsta        TASnzm
064703180308e   3c                   endif
064704180308
064705180308     c                   endsr
064706180308     C*------------------------------------------------------------------------*
064707180308     C*  RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
064708180308     C*------------------------------------------------------------------------*
064709180308     C     RECIVA        BEGSR
064710180308     C                   clear                   ds_CNACO
064711180308     C                   clear                   ds_CNIND
064712180308     C                   clear                   ds_CNCLP
064713180308     C                   clear                   ds_FNCLS
064714180308     c                   z-add     TASKSC        I69kin
064715180308     c                   call      'TIBS69R'
064716180308     c                   parm                    TIBS69ds
064717180308     c                   parm                    ds_CNACO
064718180308     c                   parm                    ds_CNIND
064719180308     c                   parm                    ds_CNCLP
064720180308     c                   parm                    ds_FNCLS
064721180308
064722180308     c                   endsr
064723940912     C*------------------------------------------------------------------------*
064724940912     C*? RUTINZ - ROUTINE INIZIALE
064800940912     C*------------------------------------------------------------------------*
064900000000     C     RUTINZ        BEGSR
065000931115     C*
065100931115     C* PARAMETRI IN INPUT AL PGM
065200931115     C     *ENTRY        PLIST
065300931115     C                   PARM                    KPJBA
065400931115     C                   MOVEL     KPJBU         PARAM
065500950309     C*
065600950309     C* DEFINISCO CAMPI
065700950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
065800950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
065900950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
066000950309     C*
066100950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
066200950309     C     KEYTAB        KLIST
066300950309     C                   KFLD                    KTBUTE
066400950309     C                   KFLD                    KTBCOD
066500950309     C                   KFLD                    KTBKEY
066600930208     C*
066700990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
066800930621     C     KEYTAS        KLIST
066900941123     C                   KFLD                    TASAAS
067000941123     C                   KFLD                    TASMGS
067100950307     C*
067200990802     C* CHIAVE LETTURA TITA730C - COMPLETA
067300950307     C     KEYTA7        KLIST
067400950307     C                   KFLD                    TA7AAS
067500950307     C                   KFLD                    TA7LNP
067600950307     C                   KFLD                    TA7NRS
067700950307     C                   KFLD                    TA7NSP
067800950307     C                   KFLD                    TA7TBL
067900990802     C*
068000990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
068100990802     C     KEYTAA        KLIST
068200990802     C                   KFLD                    TAAAAS
068300990802     C                   KFLD                    TAALNP
068400990802     C                   KFLD                    TAANRS
068500990802     C                   KFLD                    TAANSP
068600990802     C*
068700990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
068800990802     C     KEYTAT        KLIST
068900990802     C                   KFLD                    TATAAS
069000990802     C                   KFLD                    TATLNP
069100990802     C                   KFLD                    TATNRS
069200990802     C                   KFLD                    TATNSP
069300990802     C*
069400990802     C* CHIAVE LETTURA TITA430C - COMPLETA
069500990802     C     KEYTA4        KLIST
069600990802     C                   KFLD                    TA4AAS
069700990802     C                   KFLD                    TA4LNP
069800990802     C                   KFLD                    TA4NRS
069900990802     C                   KFLD                    TA4NSP
070000010903     C*
070100010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
070200010903     C     KEYTAH        KLIST
070300010903     C                   KFLD                    TAHAAS
070400010903     C                   KFLD                    TAHLNP
070500010903     C                   KFLD                    TAHNRS
070600010903     C                   KFLD                    TAHNSP
070700950209     C*
070800950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
070900950220     C     KEYCET        KLIST
071000950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
071100950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
071200950220     C                   KFLD                    CETNRS                         NUMERO SERIE
071300950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
071400950220     C                   KFLD                    CETTBL                         TIPO BOLLA
071500950307     C*
071600950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
071700950307     C     KEYCED        KLIST
071800950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
071900950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
072000950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
072100950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
072200950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
072300030122     C*
072400030122     C* CHIAVE LETTURA ECCEV30C - COMPLETA
072500030122     C     KEYCEV        KLIST
072600030122     C                   KFLD                    CEVAAS                         ANNO SPEDIZIONE
072700030122     C                   KFLD                    CEVLNP                         LINEA DI PARTENZA
072800030122     C                   KFLD                    CEVNRS                         NUMERO SERIE
072900030122     C                   KFLD                    CEVNSP                         NUMERO SPEDIZIONE
073000030122     C                   KFLD                    CEVTBL                         TIPO BOLLA
073100030512     C*
073200030512     C* CHIAVE LETTURA ECCEV30C - COMPLETA
073300030512     C     KEYC2V        KLIST
073400030512     C                   KFLD                    C2VAAS                         ANNO SPEDIZIONE
073500030512     C                   KFLD                    C2VLNP                         LINEA DI PARTENZA
073600030512     C                   KFLD                    C2VNRS                         NUMERO SERIE
073700030512     C                   KFLD                    C2VNSP                         NUMERO SPEDIZIONE
073800030512     C                   KFLD                    C2VTBL                         TIPO BOLLA
073900040531     C* CHIAVE LETTURA FIAR531C
074000040303     C     KEYAR5        KLIST
074100040303     C                   KFLD                    AR5AAS                         ANNO SPEDIZIONE
074200040303     C                   KFLD                    AR5LNP                         LINEA DI PARTENZA
074300040303     C                   KFLD                    AR5NRS                         NUMERO SERIE
074400040303     C                   KFLD                    AR5NSP                         NUMERO SPEDIZIONE
074401180308     c     ktab          klist
074402180308     c                   kFLD                    TBLKUT
074403180308     c                   kFLD                    TBLCOD
074404180308     c                   kFLD                    TBLKEY
074405180308     c                   z-add     1             TBLKUT
074406180308     c                   move      'TB'          TBLCOD
074407180308     C* CHIAVE LETTURA Fiapd01l
074408180308     C     Kapd          KLIST
074409180308     C                   KFLD                    trcapd
074410180308     C                   KFLD                    taspdc
074411180308     C* CHIAVE LETTURA
074412180308     C     Ktas          KLIST
074413180308     C                   KFLD                    TAsAAS
074414180308     C                   KFLD                    TAsLNP
074415180308     C                   KFLD                    TAsNRS
074416180308     C                   KFLD                    TAsNSP
074417180308     C*
074418180308     C* CHIAVE LETTURA
074419180308     C     Klad          KLIST
074420180308     C                   KFLD                    ladaas
074421180308     C                   KFLD                    ladlnp
074422180308     C                   KFLD                    ladnrs
074423180308     C                   KFLD                    ladnsp
074424180308     C*
074425180308     C* CHIAVE LETTURA
074426180308     C     Ktaa          KLIST
074427180308     C                   KFLD                    TAsAAS
074428180308     C                   KFLD                    TAsLNP
074429180308     C                   KFLD                    TAsNRS
074430180308     C                   KFLD                    TAsNSP
074431180308     C                   KFLD                    atrc
074432180308     C                   move      'M'           atrc              1
074433180308     C* CHIAVE LETTURA
074434180308     C     Kar5          KLIST
074435180308     C                   KFLD                    TAsAAS
074436180308     C                   KFLD                    TAsLNP
074437180308     C                   KFLD                    TAsNRS
074438180308     C                   KFLD                    TAsNSP
074439180308     C                   KFLD                    trcar5
074440180308     C                   move      'FAT'         trcar5            3
074500930208     C*
074600930208     C* IMPOSTO A ZERO IL FINE PGM
074700930208     C                   MOVEL     '0'           FINL00            1
074800931105     C                   MOVEL     *BLANKS       RSUT             20
074900950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
074901180309     C                   z-ADD     *ZEROS        NUMTASbi          9 0
075000950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
075100990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
075200990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
075300990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
075400010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
075500950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
075600950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
075700030122     C                   Z-ADD     *ZEROS        NUMCEV            9 0
075800030512     C                   Z-ADD     *ZEROS        NUMC2V            9 0
075900040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
075901180309     c                   clear                   bitass00
076000931115     C*
076100950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
076200000000     C*
076300000000     C                   ENDSR
076400940912     O*------------------------------------------------------------------------*
076500940905     OPRTF198   E            TESTA            02
076600950308     O                                            8 'TNSB36R '
076700931105     O                       RSUT                45
076800990802     O                                           82 'TRASF. TITAS10->TITASP  '
076900931115     O                                           90 'DAL:'
077000950309     O                       PARDTI              99
077100931115     O                                          110 'AL:'
077200950309     O                       PARDTF             119
077300931105     O                                          150 'Data:'
077400950717     O                       DSDAT6             159 '  /  /  '
077500931105     O                                          167 'Ora:'
077600950717     O                       DTORA6             178 '  .  .  '
077700931105     O                                          190 'Pagina:'
077800931105     O                       PAGE          Z    198
077900931105     O          E            TESTA       1  1
078000931105     O                       TES(2)              66
078100931105     O                       TES(3)             132
078200931105     O                       TES(4)             198
078300931105     O          E            TESTA          1
078400931105     O                       TES(1)              66
078500931105     O                       TES(1)             132
078600931105     O                       TES(1)             198
078700931105     O*------------------------------------
078800950307     O          E            ERRTAS         1
078900950307     O                       TASAAS            +  5
079000950307     O                       TASLNP            +  1
079100950307     O                       TASNRS            +  1
079200950307     O                       TASNSP            +  1
079300950307     O                       TASTBL            +  1
079400990802     O                                         +  1 'ERR. WRITE TITAS   '
079500180309     O                                         +  1 'NR. RELATIVO REC.'
079600040220     O                       NRRTAS            +  1
079601180309     O*------------------------------------
079602180309     O          E            ERRTASbi       1
079603180309     O                       TASAAS            +  5
079604180309     O                       TASLNP            +  1
079605180309     O                       TASNRS            +  1
079606180309     O                       TASNSP            +  1
079607180309     O                       TASTBL            +  1
079608180309     O                                         +  1 'ERR. WRITE BITAS   '
079609180309     O                                         +  1 'NR. RELATIVO REC.'
079610180309     O                       NRRTAS            +  1
079700950307     O*------------------------------------
079800950307     O          E            ERRTA7         1
079900950307     O                       TA7AAS            +  5
080000950307     O                       TA7LNP            +  1
080100950307     O                       TA7NRS            +  1
080200950307     O                       TA7NSP            +  1
080300950307     O                       TA7TBL            +  1
080400950307     O                       TA7SVN            +  1
080500990802     O                                         +  1 'ERR. WRITE TITA7   '
080600180309     O                                         +  1 'NR. RELATIVO REC.'
080700040220     O                       NRRTA7            +  1
080800990802     O*------------------------------------
080900990802     O          E            ERRTAA         1
081000990802     O                       TAAAAS            +  5
081100990802     O                       TAALNP            +  1
081200990802     O                       TAANRS            +  1
081300990802     O                       TAANSP            +  1
081400990802     O                       TAATRC            +  1
081500990802     O                                         +  1 'ERR. WRITE TITAA   '
081600180309     O                                         +  1 'NR. RELATIVO REC.'
081700040220     O                       NRRTAA            +  1
081800990802     O*------------------------------------
081900990802     O          E            ERRTAT         1
082000990802     O                       TATAAS            +  5
082100990802     O                       TATLNP            +  1
082200990802     O                       TATNRS            +  1
082300990802     O                       TATNSP            +  1
082400990802     O                       TATNSC            +  1
082500990802     O                                         +  1 'ERR. WRITE TITAT   '
082600180309     O                                         +  1 'NR. RELATIVO REC.'
082700040220     O                       NRRTAT            +  1
082800990802     O*------------------------------------
082900990802     O          E            ERRTA4         1
083000990802     O                       TA4AAS            +  5
083100990802     O                       TA4LNP            +  1
083200990802     O                       TA4NRS            +  1
083300990802     O                       TA4NSP            +  1
083400990802     O                       TA4TRC            +  1
083500990802     O                                         +  1 'ERR. WRITE TITA4   '
083600180309     O                                         +  1 'NR. RELATIVO REC.'
083700040220     O                       NRRTA4            +  1
083800010903     O*------------------------------------
083900010903     O          E            ERRTAH         1
084000010903     O                       TAHAAS            +  5
084100010903     O                       TAHLNP            +  1
084200010903     O                       TAHNRS            +  1
084300010903     O                       TAHNSP            +  1
084400010903     O                       TAHTRC            +  1
084500010903     O                                         +  1 'ERR. WRITE TITAH   '
084600180309     O                                         +  1 'NR. RELATIVO REC.'
084700040220     O                       NRRTAH            +  1
084800950307     O*------------------------------------
084900950307     O          E            ERRCET         1
085000950307     O                       CETAAS            +  5
085100950307     O                       CETLNP            +  1
085200950307     O                       CETNRS            +  1
085300950307     O                       CETNSP            +  1
085400950307     O                       CETTBL            +  1
085500950307     O                                         +  1 'ERR. WRITE ECCET   '
085600180309     O                                         +  1 'NR. RELATIVO REC.'
085700040220     O                       NRRCET            +  1
085800950307     O*------------------------------------
085900950307     O          E            ERRCED         1
086000950307     O                       CEDAAS            +  5
086100950307     O                       CEDLNP            +  1
086200950307     O                       CEDNRS            +  1
086300950307     O                       CEDNSP            +  1
086400950307     O                       CEDTBL            +  1
086500950307     O                       CEDCMP            +  1
086600950307     O                                         +  1 'ERR. WRITE ECCED   '
086700180309     O                                         +  1 'NR. RELATIVO REC.'
086800040220     O                       NRRCED            +  1
086900980325     O*------------------------------------
087000030122     O          E            ERRCEV         1
087100030122     O                       CEVAAS            +  5
087200030122     O                       CEVLNP            +  1
087300030122     O                       CEVNRS            +  1
087400030122     O                       CEVNSP            +  1
087500030122     O                       CEVTBL            +  1
087600030122     O                                         +  1 'ERR. WRITE ECCEV   '
087700180309     O                                         +  1 'NR. RELATIVO REC.'
087800040220     O                       NRRCEV            +  1
087900030512     O*------------------------------------
088000030512     O          E            ERRC2V         1
088100030512     O                       C2VAAS            +  5
088200030512     O                       C2VLNP            +  1
088300030512     O                       C2VNRS            +  1
088400030512     O                       C2VNSP            +  1
088500030512     O                       C2VTBL            +  1
088600030512     O                                         +  1 'ERR. WRITE ECC2V   '
088700180309     O                                         +  1 'NR. RELATIVO REC.'
088800040220     O                       NRRC2V            +  1
088900040303     O*------------------------------------
089000040303     O          E            ERRAR5         1
089100040303     O                       AR5AAS            +  5
089200040303     O                       AR5LNP            +  1
089300040303     O                       AR5NRS            +  1
089400040303     O                       AR5NSP            +  1
089500040303     O                                         +  1 'ERR. WRITE FIAR5   '
089600180309     O                                         +  1 'NR. RELATIVO REC.'
089700040303     O                       NRRAR5            +  1
089800931105     O*------------------------------------
089900950308     O          E            STAFIN         1
090000990802     O                                           30 'N.REC.TRASF. TITAS'
090100950308     O                       NUMTAS        2     44
090101180309     O          E            STAFIN         1
090102180309     O                                           30 'N.REC.TRASF. BITAS'
090103180309     O                       NUMTASbi      2     44
090200950308     O          E            STAFIN         1
090300990802     O                                           30 'N.REC.TRASF. TITA7'
090400950308     O                       NUMTA7        2     44
090500990802     O          E            STAFIN         1
090600990802     O                                           30 'N.REC.TRASF. TITAA'
090700990802     O                       NUMTAA        2     44
090800990802     O          E            STAFIN         1
090900990802     O                                           30 'N.REC.TRASF. TITAT'
091000990802     O                       NUMTAT        2     44
091100990802     O          E            STAFIN         1
091200990802     O                                           30 'N.REC.TRASF. TITA4'
091300990802     O                       NUMTA4        2     44
091400010904     O          E            STAFIN         1
091500010904     O                                           30 'N.REC.TRASF. TITAH'
091600010904     O                       NUMTAH        2     44
091700950308     O          E            STAFIN         1
091800950308     O                                           30 'N.REC.TRASF. ECCET'
091900950308     O                       NUMCET        2     44
092000950308     O          E            STAFIN         1
092100950308     O                                           30 'N.REC.TRASF. ECCED'
092200950308     O                       NUMCED        2     44
092300030122     O          E            STAFIN         1
092400030122     O                                           30 'N.REC.TRASF. ECCEV'
092500030122     O                       NUMCEV        2     44
092600030512     O          E            STAFIN         1
092700030512     O                                           30 'N.REC.TRASF. ECC2V'
092800030512     O                       NUMC2V        2     44
092900040303     O          E            STAFIN         1
093000040303     O                                           30 'N.REC.TRASF. FIAR5'
093100040303     O                       NUMAR5        2     44
093200950308     O          E            STAFIN           60
093300931115     O                                           76 '*** FINE STAMPA ***'
093400931115     O                                          150 'Data:'
093500950717     O                       DSDAT6             159 '  /  /  '
093600931115     O                                          167 'Ora:'
093700950717     O                       DTORA6             178 '  :  :  '
093800940912     O*------------------------------------------------------------------------*
093900931105**
094000931105------------------------------------------------------------------  1
094100931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
094200950308E                                                                   3
094300931105                                                                    4
094400931105                                                                    5
094500931105                                                                    6
094600931105                                                                    7
094700931105                                                                    8
094800931105                                                                    9
094900931105                                                                   10
095000931105                                                                   11
095100931105                                                                   12
095200931105                                                                   13
095300931105                                                                   14
095400931105                                                                   15
