000100940330     F*------------------------------------------------------------------------*
000200940406     F*                 - AGGIORNA SALDI CLIENTI NUOVI -
000300940406     F* PER OGNI CLIENTE AGGIORNA CON UNA "Y" I SALDI CON DATA UGUALE ALLA DATA
000400940406     F* DI PRIMA SPEDIZIONE DEL CLIENTE, E CON UNA "S" I SALDI DI MESE SUPERIORE
000500940406     F* A QUELLO DELLA DATA DI PRIMA SPEDIZIONE MA DI ANNO UGUALE.
000600990603     H* NB: LANCIATO DALL'AGGIORNAMENTO SALDI CLIENTE DA CONTABILITA' - YCO902C
000700980122     H*     CHE A SUA VOLTA VIENE LANCIATO MENSILMENTE DAL SUPERLANCIO -TNSS01R
000800940330     F*------------------------------------------------------------------------*
000900940330     H DATEDIT(*YMD)
001000940330     F*------------------------------------------------------------------------*
001100940330     F* DATA BASE
001200940330     F*------------------------------------------------------------------------*
001300990609     FSISDO01L  UF   E           K DISK
001400940406     FCNCLP00F  IF   E           K DISK
001500980121     FAZORG01L  IF   E           K DISK
001600940406     D*------------------------------------------------------------------------*
001700940406     D* SCHIERE
001800940406     D*------------------------------------------------------------------------*
001900980121     D*-------------------
002000980121     D* FILIALI IN GESTIONE
002100980121     D*-------------------
002200980121     D FIL             S              3  0 DIM(999)
002300940406     D*-------------------
002400940406     D* TABELLE CAPICONTO
002500940406     D*-------------------
002600940330     D*------------------------------------------------------------------------*
002700940330     D* INPUT
002800940330     D*------------------------------------------------------------------------*
002900940406     D*------------------
003000020830     D* PARAMETRI X§PARUT
003100940406     D*------------------
003200020906     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003300940406     D  TCU                  398    697
003400940406     D                                     DIM(50)                              *TAB FLG 8  TP.CN
003500940406     D  KCU                  698    847P 0
003600940406     D                                     DIM(50)                              *TAB CAPICONTO
003700940406     D                                     PACKEVEN
003800940406     D  DTU                  848    895P 0
003900940406     D                                     DIM(12)                              *DATE PARAM
004000940406     D                                     PACKEVEN
004100940406     D*------------------
004200940406     D* RICERCA CAPICONTO
004300940406     D*------------------
004400940406     D TCUDS           DS
004500940406     D  E1                     1      1
004600940406     D  E3                     3      3
004700940406     D  E2                     2      2
004800940406     D  E4                     4      4
004900940406     D  E56                    5      6
005000940406     D*------------------
005100940406     D* PARAMETRO DATA INIZIALE (AA/MM)
005200940406     D*------------------
005300940406     D                 DS
005400990602     D  PARAAI                 1      4  0
005500990602     D  PARMMI                 5      6  0
005600990602     D  PARAMI                 1      6  0
005700940406     D*------------------
005800940406     D* PARAMETRO DATA FINALE (AA/MM)
005900940406     D*------------------
006000940406     D                 DS
006100990602     D  PARAAF                 1      4  0
006200990602     D  PARMMF                 5      6  0
006300990602     D  PARAMF                 1      6  0
006400940330     D*------------------
006500990609     D* DATA SISDO01L (AA/MM)
006600940330     D*------------------
006700940330     D                 DS
006800990602     D  SDOANN                 1      4  0
006900990602     D  SDOMES                 5      6  0
007000990602     D  SDODAT                 1      6  0
007100940406     D*------------------
007200940406     D* DATA PRIMA SPEDIZIONE DI CNCLP00F
007300940406     D*------------------
007400940406     D                 DS
007500990603     D  CLPSEC                 1      2  0
007600990603     D  CLPANN                 3      4  0
007700990603     D  CLPAN4                 1      4  0
007800990603     D  CLPMES                 5      6  0
007900990603     D  CLPDAT                 1      6  0
008000990603     D  DSDPS                  1      8  0
008100020805     D*------------------
008200020805     D* ARCHITETTURA
008300020805     D*------------------
008400020805     D KPJBA         E DS
008500940330     C*------------------------------------------------------------------------*
008600940330     C*?MAIN LINE
008700010308     C*------------------------------------------------------------------------*
008800010308     C*
008900010308     C* NON FA NIENTE SE ERRONEAMENTE LANCIATO DA 'SDI'
009000020805IF  1C     KNSIF         IFEQ      'GAITRA201'
009100940330     C*
009200940330     C* LEGGO IL PRIMO CLIENTE VALIDO
009300940331     C                   Z-ADD     *ZEROS        DEPKSC                         *DEPOSITO CLIENTE
009400940406     C*
009500940331     C                   Z-ADD     DEPKSC        KSOKSC
009600990609     C     KEYSDO        SETGT     SISDO01L
009700990609     C                   READ      SISDO01L                               99
009800940330     C*
009900940331     C* PER OGNI CLIENTE PRESENTE NEI SALDI
010000940330DO  1C     *IN99         DOWEQ     *OFF
010100940406     C                   Z-ADD     SDOKSC        DEPKSC                         *DEPOSITO CLIENTE
010200980121     C*
010300980121     C* CONTROLLA VALIDITA' RECORD
010400980121     C                   EXSR      CHKSDO
010500980121     C*
010600980121     C* ESEGUE L'AGGIORNAMENTO SOLO SE IL RECORD E' OK
010700980121IF  2C     WRECOK        IFEQ      'S'
010800940406     C*
010900940406     C* REPERISCO DATA PRIMA SPEDIZIONE
011000940406     C                   EXSR      REPDPS
011100940331     C*
011200940406     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
011300940406     C                   EXSR      SETFIL
011400940406     C*
011500940406     C* SE NEL PERIODO CI SONO SALDI DI CLIENTI NUOVI
011600980121DO  3C     $FINE         DOWEQ     *OFF
011700940406     C*
011800940406     C* AGGIORNO IL FLAG "CLIENTE NUOVO" DEI SALDI
011900940406     C                   EXSR      AGGNEW
012000940406     C*
012100940406     C* LETTURA SUCESSIVA SALDI CLIENTE
012200940406     C                   EXSR      LETFIL
012300980121E   3C                   ENDDO
012400980121E   2C                   ENDIF
012500940331     C*
012600940331     C* LEGGO IL SUCESSIVO CLIENTE VALIDO
012700940331     C                   Z-ADD     DEPKSC        KSOKSC
012800990609     C     KEYSDO        SETGT     SISDO01L
012900990609     C                   READ      SISDO01L                               99
013000980121E   1C                   ENDDO
013100010308E   1C                   ENDIF
013200940331     C*
013300000000     C                   SETON                                        LR
013400980121     C*------------------------------------------------------------------------*
013500980121     C*?CHKSDO - CONTROLLA VALIDITA' RECORD
013600980121     C*------------------------------------------------------------------------*
013700980121     C     CHKSDO        BEGSR
013800980121     C*
013900980121     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
014000980121     C*
014100980121     C* CLIENTE NON GESTITO DAL S.INFORMATIVO, ESCLUDE
014200980121     C                   SETOFF                                           28
014300980121     C                   MOVEL     SDOKSC        N3                3 0          *FILIALE
014400980121     C     N3            LOOKUP    FIL                                    28
014500980121IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
014600980121     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
014700980121E   1C                   ENDIF
014800980121     C*
014900980121     C                   ENDSR
015000940406     C*------------------------------------------------------------------------*
015100940406     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
015200940406     C*------------------------------------------------------------------------*
015300940406     C     SETFIL        BEGSR
015400940406     C*
015500940406     C* REIMPOSTO FLAG DI FINE LETTURA SALDI
015600940406     C                   MOVEL     '0'           $FINE
015700940406     C*
015800940406     C                   Z-ADD     DEPKSC        KS2KSC
015900940406     C                   Z-ADD     PARAAI        KS2ANN
016000940406     C                   Z-ADD     PARMMI        KS2MES
016100940406     C*
016200940406     C* POSIZIONAMENTO PER CERCARE IL PRIMO CLIENTE VALIDO (KSC+ANN)
016300990609     C     KE2SDO        SETLL     SISDO01L                           99        *FINE ARCHIVIO
016400940406     C*
016500940406IF  1C     *IN99         IFEQ      '1'
016600940406     C                   MOVEL     '1'           $FINE
016700940406X   1C                   ELSE
016800940406     C                   EXSR      LETFIL                                       *LETTURA RECORD
016900940406E   1C                   ENDIF
017000940406     C*
017100940406     C                   ENDSR
017200940406     C*------------------------------------------------------------------------*
017300940406     C*?LETFIL - LETTURA PROSSIMO RECORD SALDI CLIENTE
017400940406     C*------------------------------------------------------------------------*
017500940406     C     LETFIL        BEGSR
017600940406     C*
017700940406     C* LEGGO FINO A:
017800940406     C*     - FINE FILE
017900940406     C*     - TROVATO RECORD VALIDO
018000940406     C                   MOVEL     'N'           WRECOK            1
018100940406DO  1C     $FINE         DOWEQ     '0'
018200940406     C     WRECOK        ANDEQ     'N'
018300940406     C*
018400990609     C     KS2KSC        READE     SISDO01L                               99
018500940406     C*
018600940406IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
018700940406     C                   MOVEL     '1'           $FINE
018800940406X   2C                   ELSE
018900940406     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
019000940406E   2C                   ENDIF
019100940406     C*
019200940406E   1C                   ENDDO
019300940406     C*
019400940406     C                   ENDSR
019500940406     C*------------------------------------------------------------------------*
019600940406     C*?CHKREC - CONTROLLA VALIDITA' RECORD SALDI CLIENTE
019700940406     C*------------------------------------------------------------------------*
019800940406     C     CHKREC        BEGSR
019900940406     C*
020000940406     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
020100940406     C*
020200940406     C* SE I SALDI HANNO DATA SUPERIORE A QUELLA RICHIESTA O SONO DI ANNI
020300940406     C* SUPERIORI RISPETTO A QUELLO DI PRIMA SPEDIZIONE DEL CLIENTE, ESCO
020400940406     C* (RIMANGONO FUORI I SALDI MINORI ALLA DATA O UGUALI ALL' ANNO)
020500940406IF  1C     SDODAT        IFGT      PARAMF
020600990603     C     SDOANN        ORGT      CLPAN4
020700940406     C                   MOVEL     '1'           $FINE                          *FINE CICLO SALDI
020800940406E   1C                   ENDIF
020900940406     C*
021000940406     C* SE I SALDI SONO DI DATA MINORE ALLA PRIMA SPEDIZIONE, RECORD NON VALIDO
021100990603IF  1C     SDODAT        IFLT      CLPDAT
021200940406     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021300940406E   1C                   ENDIF
021400940406     C*
021500940406     C                   ENDSR
021600940406     C*------------------------------------------------------------------------*
021700940406     C*?REPDPS - REPERISCE LA DATA DI PRIMA SPEDIZIONE DEL CLIENTE
021800940406     C*------------------------------------------------------------------------*
021900940406     C     REPDPS        BEGSR
022000940406     C*
022100940406     C                   Z-ADD     CODUT         KCLKUT                         *CODICE UTENTE
022200940406     C                   Z-ADD     KCI           KCLKCC                         *CAPOCONTO
022300940406     C                   Z-ADD     DEPKSC        KCLKSC                         *CODICE CLIENTE
022400940406     C*
022500940406     C     KEYCLP        SETLL     CNCLP00F
022600940406     C     KEYCLP        READE     CNCLP00F                               99
022700940406IF  1C     *IN99         IFEQ      *ON                                          *CLIENTE NON TROVATO
022800990603     C                   Z-ADD     *ZEROS        DSDPS
022900990603X   1C                   ELSE
023000990603     C                   Z-ADD     CLPDPS        DSDPS                          *X SCOMPOSIZIONE DAT
023100940406     C*
023200990603     C* AGGIUNGE IL SECOLO ALLA DATA PRIMA SPEDIZIONE
023300000211IF  2C     CLPANN        IFGT      60
023400990603     C                   Z-ADD     19            CLPSEC
023500000211X   2C                   ELSE
023600990603     C                   Z-ADD     20            CLPSEC
023700000211E   2C                   ENDIF
023800000211E   1C                   ENDIF
023900990603     C*
024000940406     C                   ENDSR
024100940406     C*------------------------------------------------------------------------*
024200940406     C*?AGGNEW - AGGIORNA IL FLAG "CLIENTE NUOVO" DEI SALDI
024300940406     C*------------------------------------------------------------------------*
024400940406     C     AGGNEW        BEGSR
024500940406     C*
024600940406     C* SE IL SALDO E' DELLA DATA DI PRIMA SPEDIZIONE DEL CLIENTE
024700020522     C                   MOVEL     SDOCLN        DEPCLN
024800990603IF  1C     SDODAT        IFEQ      CLPDAT
024900940406     C                   MOVEL     'Y'           SDOCLN                         *CLIENTE NUOVO MESE
025000940406X   1C                   ELSE
025100940406     C                   MOVEL     'S'           SDOCLN                         *CLIENTE NUOVO ANNO
025200940406E   1C                   ENDIF
025300940406     C*
025400020522IF  1C     SDOCLN        IFNE      DEPCLN
025500020522     C                   Z-ADD     *ZEROS        SDODTR
025600020522     C                   MOVEL     *BLANKS       SDOFTR
025700940406     C                   EXCEPT    AGGIOR
025800020522E   1C                   ENDIF
025900940406     C*
026000940406     C                   ENDSR
026100980121     C*------------------------------------------------------------------------*
026200980121     C*?CARTAB - CARICA LE TABELLE OCCORRENTI
026300980121     C*------------------------------------------------------------------------*
026400980121     C     CARTAB        BEGSR
026500980121     C*
026600980121     C* CARICA LE FILIALI IN GESTIONE AL SISTEMA INFORMATIVO
026700980121     C                   Z-ADD     *ZEROS        I                 4 0
026800980121     C     *LOVAL        SETLL     AZORG01L
026900980121     C                   READ      AZORG01L                               99
027000980121DO  1C     *IN99         DOWEQ     *OFF
027100980121IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDE
027200980121IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
027300980121     C                   ADD       1             I
027400980121     C                   Z-ADD     ORGFIL        FIL(I)
027500980121E   3C                   ENDIF
027600980121E   2C                   ENDIF
027700980121     C                   READ      AZORG01L                               99
027800980121E   1C                   ENDDO
027900980121     C*
028000980121     C                   ENDSR
028100940406     C*------------------------------------------------------------------------*
028200940406     C*?CARCC  - CARICA I CAPICONTO DI CONTABILITA'
028300940406     C*------------------------------------------------------------------------*
028400940406     C     CARCC         BEGSR
028500940406     C*
028600940406     C* RECUPERO DATI DELL'UTENTE
028700940406     C                   Z-ADD     1             CODUT
028800020830     C                   CALL      'X§PARUT'
028900940406     C                   PARM                    UTEDSE
029000940406     C*
029100940406     C* RICERCA CAPOCONTI
029200940406     C                   DO        50            X                 2 0
029300940406     C                   MOVE      TCU(X)        TCUDS
029400940406     C     E56           CABNE     'CG'          END1
029500940406     C     E4            COMP      '1'                                    21
029600940406     C     E4            COMP      '2'                                    22
029700940406     C     E4            COMP      '3'                                    23
029800940406     C     E4            COMP      '6'                                    27
029900940406     C*
030000940406     C* 1 CLIENTI   21
030100940406     C* 2 FORNITORI 22
030200940406     C* 3 AGENTI    23
030300940406     C*
030400940406     C     E3            COMP      '0'                                242425
030500940406     C     E3            COMP      'I'                                    26
030600940406     C*
030700940406     C* 0 ITALIA   25
030800940406     C* 1 ESTERO   24
030900940406     C* I CAPO CSBTO IVA
031000940406     C*
031100940406     C   21
031200940406     CAN 24              Z-ADD     KCU(X)        KCE               4 0
031300940406     C   21
031400940406     CAN 25              Z-ADD     KCU(X)        KCI               4 0
031500940406     C   22
031600940406     CAN 24              Z-ADD     KCU(X)        KFE               4 0
031700940406     C   22
031800940406     CAN 25              Z-ADD     KCU(X)        KFI               4 0
031900940406     C   23
032000940406     CAN 24              Z-ADD     KCU(X)        KAE               4 0
032100940406     C   23
032200940406     CAN 25              Z-ADD     KCU(X)        KAI               4 0
032300940406     C   26              Z-ADD     KCU(X)        KIVA              4 0
032400940406     C   27              Z-ADD     KCU(X)        KBNA              4 0
032500940406     C     END1          TAG
032600940406     C                   END
032700940406     C*
032800940406     C                   ENDSR
032900940330     C*------------------------------------------------------------------------*
033000940330     C*?*INZSR - ROUTINE INIZIALE
033100940330     C*------------------------------------------------------------------------*
033200940330     C     *INZSR        BEGSR
033300940330     C*
033400941121     C* PARAMETRI PASSATI DAL CLP "TNSS72C"
033500940330     C     *ENTRY        PLIST
033600020805     C                   PARM                    KPJBA
033700990602     C                   PARM                    $AAI              4 0          *ANNO INIZIALE
033800940406     C                   PARM                    $MMI              2 0          *MESE INIZIALE
033900990602     C                   PARM                    $AAF              4 0          *ANNO FINALE
034000940406     C                   PARM                    $MMF              2 0          *MESE FINALE
034100940406     C*
034200940406     C                   Z-ADD     $AAI          PARAAI
034300940406     C                   Z-ADD     $MMI          PARMMI
034400940406     C                   Z-ADD     $AAF          PARAAF
034500940406     C                   Z-ADD     $MMF          PARMMF
034600940406     C*
034700020906     C                   Z-ADD     1             CODUT             1 0          *CODICE UTENTE
034800940406     C*
034900940406     C* DEFINIZIONE VARIABILI KLIST
035000990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO00F
035100940406     C     *LIKE         DEFINE    SDOKSC        KS2KSC
035200940406     C     *LIKE         DEFINE    SDOANN        KS2ANN
035300940406     C     *LIKE         DEFINE    SDOMES        KS2MES
035400940406     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
035500940406     C     *LIKE         DEFINE    CLPKCC        KCLKCC
035600940406     C     *LIKE         DEFINE    CLPKSC        KCLKSC
035700940406     C*
035800940406     C* DEFINIZIONE VARIABILI
035900940406     C     *LIKE         DEFINE    SDOKSC        DEPKSC                         *DEPOSITO CLIENTE
036000020522     C     *LIKE         DEFINE    SDOCLN        DEPCLN
036100940330     C*
036200990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
036300940330     C     KEYSDO        KLIST
036400940406     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
036500940406     C*
036600990609     C* CHIAVE LETTURA SISDO01L - COMPLETA
036700940406     C     KE2SDO        KLIST
036800940406     C                   KFLD                    KS2KSC                         *CODICE CLIENTE
036900940406     C                   KFLD                    KS2ANN                         *ANNO
037000940406     C                   KFLD                    KS2MES                         *MESE
037100940406     C*
037200940406     C* CHIAVE LETTURA CNCLP00F - COMPLETA
037300940406     C     KEYCLP        KLIST
037400940406     C                   KFLD                    KCLKUT                         *CODICE UTENTE
037500940406     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
037600940406     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
037700940406     C*
037800940406     C* FINE CICLI DI LETTURA
037900940406     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA SALDI
038000980121     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
038100940406     C*
038200940406     C* RICERCA I CAPICONTI DI CONTABILITA'
038300940406     C                   EXSR      CARCC
038400980121     C*
038500980121     C* CARICA LE TABELLE OCCORRENTI
038600980121     C                   EXSR      CARTAB
038700940406     C*
038800940330     C                   ENDSR
038900940406     C*------------------------------------------------------------------------*
039000940406     C*?AGGIORNAMENTO FLAG "CLIENTE NUOVO"
039100940406     C*------------------------------------------------------------------------*
039200990609     OSISDO000  E            AGGIOR
039300940406     O                       SDOCLN
039400020522     O                       SDOFTR
039500020522     O                       SDODTR
