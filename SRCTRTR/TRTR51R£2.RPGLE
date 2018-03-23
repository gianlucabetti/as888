000100990805     H* TRTR51R *-----------------------------------------------------*
000200930422     H*-----------------*                                             *
000300990805     H*            - RICEZIONE VARIAZIONI PESO   IN SEDE -            *
000400941124     H*---------------------------------------------------------------*
000500930422     H* INDICATORI USATI                                              *
000600930422     H*---------------------------------------------------------------*
000700930806     H* 01 - MANCA SECONDA BOLLA
000800930806     H* 02 - MANCA PRIMA   BOLLA
000900930806     H* 05 - BOLLA TROVATA IN ARCHIVIO CORRENTE
001000930426     H* 06 - BOLLA TROVATA NELLO STORICO
001100930426     H* 07 - BOLLA TROVATA IN ARCHIVIO BOLLE ANNO PRECEDENTE
001200990702     H* 11 - LETTURA DI TITAS000
001300990702     H* 12 - LETTURA DI TITAS010
001400990702     H* 13 - LETTURA DI TITASP00
001500930423     H* 30 - LETTURA TABEL00F
001600930427     H* 31 - RICERCA FILIALE IN TABELLA £1
001700930423     H* 32 - CALL 'QCMDEXC'
001800990805     H* 33 - LETTURA FNARBP0R
001900930423     H* 34 - LETTURA CMRIC01L
002000990702     H* 38 - LETTURA DI TNBLA000
002100930806     H* 39/40 COMOD
002200930422     H*---------------------------------------------------------------*
002300930422     H* DATA BASE                                                     *
002400930422     H*---------------------------------------------------------------*
002500930422     H DECEDIT('0,') DATEDIT(*DMY.)
002600990805     FFNARBP0R  UF   E           K DISK    USROPN
002700941124     FTNBLA01L  IF   E           K DISK
002800990701     FTITAS30C  UF   E           K DISK
002900991028     FFIARBE2C  UF A E           K DISK
003000990805     F                                     RENAME(FNARBP00:FNARBP0E)
003100930423     FTABEL00F  IF   E           K DISK
003200930423     FCMRIC01L  UF   E           K DISK
003300930422     D*---------------------------------------------------------------*
003400930422     D* SCHIERE                                                       *
003500930422     D*---------------------------------------------------------------*
003600930423     D* COMANDI QCMDEXC
003700930423     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
003800930423     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
003900930423     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
004000930426     D* DESCRIZIONE ERRORE
004100930528     D ERR             S             35    DIM(4) CTDATA PERRCD(1)
004200930422     D*---------------------------------------------------------------*
004300930422     D* DS                                                            *
004400930422     D*---------------------------------------------------------------*
004500930422     D KPJBA         E DS
004600930423     D  LEGFIL               483    492
004700930423     D  MBRFIL               493    502
004800960208     D***
004900960208     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005000960208     D***
005100960208     D WLBDAT          DS                  INZ
005200960208     D  G02DAT                 1      8  0
005300960208     D  G02INV                 9     16  0
005400960208     D  G02ERR                17     17
005500960208     D  G02TGI                18     22  0
005600960208     D***
005700960208     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
005800960208     D***
005900960208     D WGIDAT          DS                  INZ
006000960208     D  GIODAT                 1      8  0
006100960208     D  GIOINV                 9     16  0
006200960208     D  GIOTGI                17     21  0
006300930528     D DS3A          E DS
006400950111     D*
006500990805     D* DS PER FNLV39R - DS DEI CAMPI PER CALCOLO PESO   DA FATTURARE
006600990805     D DSLV39        E DS                  EXTNAME(FNLV39DS)
006700990702     D*
006800990702     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006900990701     ITITAS000      11
007000990701     ITITAS010      12
007100990701     ITITASP00      13
007200941129     I*
007300990805     IFNARBP0E
007400950111     I              ARBAAS                      RRBAAS
007500950111     I              ARBLNP                      RRBLNP
007600950111     I              ARBNRS                      RRBNRS
007700950111     I              ARBNSP                      RRBNSP
007800950111     I              ARBCBO                      RRBCBO
007900950111     I              ARBDTV                      RRBDTV
008000950111     I              ARBORV                      RRBORV
008100950111     I              ARBCVB                      RRBCVB
008200990805     I              ARBPKC                      RRBPKC
008300991028     I              ARBFPC                      RRBFPC
008400990805     I              ARBNCP                      RRBNCP
008500950111     I*
008600950111£$   I*DS$SLD    E DS
008700930422     C*---------------------------------------------------------------*
008800930422     C*                    MAIN                                       *
008900990805     C* PROGRAMMA DI RICEZIONE IN SEDE VARIAZIONI PESO                *
009000930422     C*---------------------------------------------------------------*
009100930528     C*
009200930528     C* DEFINIZIONE VARIABILI, KLIST E PLIST
009300930528     C                   EXSR      DEFVAR
009400930528     C*
009500930528     C* APERTURA MEMBRO DI RICEZIONE
009600930528     C                   EXSR      CAEXA
009700930528     C*
009800930528     C* APERTURA RIUSCITA
009900930528IF  1C     *IN32         IFEQ      '0'
010000930423     C*
010100990805     C* LEGGO VARIAZIONE PESO
010200990805     C                   CLEAR                   FNARBP0E
010300990805     C                   READ      FNARBP0R                               33
010400930423DO  2C     *IN33         DOWEQ     '0'
010500930423     C*
010600930423     C* GESTIONE VARIAZIONI DA APPLICARE
010700930423     C                   EXSR      GESMOD
010800990805     C                   CLEAR                   FNARBP0E
010900990805     C                   READ      FNARBP0R                               33
011000930423E   2C                   ENDDO
011100930423     C*
011200930423     C* AGGIORNA ARCHIVIO DI LOG
011300930423     C     LEGFIL        CHAIN     CMRIC01L                           34
011400930423IF  2C     *IN34         IFEQ      '0'
011500930423     C                   Z-ADD     NREC          RICNR1
011600930423     C                   MOVE      '1'           RICFL2
011700930423     C                   UPDATE    CMRIC000
011800930423E   2C                   ENDIF
011900930423     C*
012000930423     C* CHIUSURA MEMBRO DI RICEZIONE
012100990805     C                   CLOSE     FNARBP0R
012200930423E   1C                   ENDIF
012300990702     C*
012400990702     C                   MOVEL     'C'           D55TLA
012500990702     C                   CALL      'FNLV55R'
012600990702     C                   PARM                    DSLV55
012700930422     C*
012800930423     C                   SETON                                        LR
012900930423     C*---------------------------------------------------------------*
013000930423     C*                    GESMOD                                     *
013100990805     C* ROUTINE DI GESTIONE MODIFICHE PESO   DA APPLICARE             *
013200930423     C*---------------------------------------------------------------*
013300930423     C     GESMOD        BEGSR
013400930426     C*
013500930426     C                   SETOFF                                       050607
013600930528     C                   SETOFF                                       3108
013700930806     C                   SETOFF                                       0102
013800930806     C*
013900930806     C* LETTURA TIPO BOLLA
014000930528     C                   MOVEL     '3A'          COD
014100950111     C                   MOVEL(P)  ARBCBO        KEY
014200930528     C     KTAB          CHAIN     TABEL                              30
014300930528     C*
014400930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
014500930806IF  1C     *IN30         IFEQ      '1'
014600930806     C                   Z-ADD     102           FEV
014700930806     C                   MOVEL     ERR(3)        DER
014800950111     C                   EXSR      RIEARB
014900930528     C*
015000930806X   1C                   ELSE
015100930528     C*
015200930528     C                   MOVEL     TBLUNI        DS3A
015300950905     C* SE SI TRATTA DI UNA BOLLA NON TRASMESSA IN SEDE
015400950905     C* CANCELLO SENZA DARE ERRORE
015500950905   1AC     §3ABSD        IFEQ      ' '
015600930806     C*
015700930806     C* 2   B O L L A
015800930806     C*
015900930806IF  2C     §3ATB2        IFNE      '  '
016000930806     C                   MOVEL     §3ATB2        TBL
016100930528     C*
016200930426     C* CERCO BOLLA IN ARCHIVIO CORRENTE
016300941129     C                   SETOFF                                       111213
016400990701     C     KTAS          CHAIN     TITAS30C                           37
016500930426     C*
016600930426     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
016700941129     C   37KTAS2         CHAIN     TNBLA01L                           38
016800941129     C   37
016900941129     CAN 38              SETON                                        01
017000941129    2C                   ENDIF
017100930426     C*
017200930806     C* 1   B O L L A
017300930806     C*
017400930806     C                   MOVEL     §3ATB1        TBL
017500930528     C*
017600930528     C* CERCO BOLLA IN ARCHIVIO CORRENTE
017700941129     C                   SETOFF                                       111213
017800990701     C     KTAS          CHAIN     TITAS30C                           37
017900930528     C*
018000930528     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
018100941129IF  2C     *IN37         IFEQ      '1'
018200941129     C     KTAS2         CHAIN     TNBLA01L                           38
018300941129     C   38              SETON                                        02
018400930528     C*
018500941129X   2C                   ELSE
018600941129     C* BOLLA TROVATA
018700941129     C   11              SETON                                        05
018800941129     C   12              SETON                                        06
018900941129     C   13              SETON                                        07
019000941129E   2C                   ENDIF
019100930806     C*
019200930806     C* CONTROLLO SE MANCANO VERAMENTE O E' CAMBIATO IL TIPO BOLLA
019300930806     C   01
019400930806     COR 02              EXSR      MANBOL
019500930806     C*
019600930806     C* SE PRIMA BOLLA INESISTENTE ESCO E NON AGGIORNO
019700930806IF  2C     *IN08         IFEQ      '0'
019800930806     C*
019900930528     C* 1 BOLLA TROVATA IN ARCHIVIO (CORRENTE, STORICO O PRECEDENTE)
020000930528IF  3C     *IN05         IFEQ      '1'
020100930427     C     *IN06         OREQ      '1'
020200930427     C     *IN07         OREQ      '1'
020300930528     C                   EXSR      AGGIOR
020400930528     C*
020500930806IF  4C     *IN05         IFEQ      *ON
020600930806     C*
020700930706     C* SE ESISTE ERRORE NON STAMPATO CANCELLO
020800950111     C                   EXSR      ANNARB
020900930706     C*
021000990701     C                   UPDATE    TITAS000
021100930806E   4C                   ENDIF
021200930706     C*
021300990701     C   06              UPDATE    TITAS010
021400940208     C*
021500990701     C   07              UPDATE    TITASP00
021600930806E   3C                   ENDIF
021700930806     C*
021800941129     C* 2 BOLLA TROVATA IN ARCHIVIO
021900941129IF  3C     §3ATB2        IFNE      *BLANKS
022000941129     C     *IN01         ANDEQ     *OFF
022100941129     C*
022200930528     C                   MOVEL     §3ATB2        TBL
022300941129     C                   SETOFF                                       111213
022400990701     C     KTAS          CHAIN     TITAS30C                           30
022500930528     C*
022600941129    4C     *IN30         IFEQ      *OFF
022700930528     C                   EXSR      AGGIOR
022800930528     C*
022900990701     C   11              UPDATE    TITAS000
023000990701     C   12              UPDATE    TITAS010
023100990701     C   13              UPDATE    TITASP00
023200941129E   4C                   ENDIF
023300941129E   3C                   ENDIF
023400930528     C*
023500930528     C* CANCELLO SOLO VARIAZIONE TRASMESSA
023600930528     C                   ADD       1             NREC              7 0
023700990805     C                   DELETE    FNARBP0R
023800930806E   2C                   ENDIF
023900950905     C*
024000950905X  1AC                   ELSE
024100950905     C                   ADD       1             NREC              7 0
024200990805     C                   DELETE    FNARBP0R
024300950905E  1AC                   ENDIF
024400950905E   1C                   ENDIF
024500930806     C*
024600930528     C                   ENDSR
024700930528     C*---------------------------------------------------------------*
024800930528     C*                    AGGIOR                                     *
024900990805     C* ROUTINE DI AGGIORNAMENTO PESO   SULLA BOLLA                   *
025000930528     C*---------------------------------------------------------------*
025100930528     C     AGGIOR        BEGSR
025200930426     C*
025300990805     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO PESO   UGUALE A 'T'
025400970625    1C******     TASFVF    IFNE 'T'
025500930528     C*
025600930806     C                   MOVEL     TASTBL        BOL1              1
025700930806     C                   MOVE      TASTBL        BOL2              1
025800930426     C                   MOVE      TASKSC        CODCLI            4 0
025900930806     C*
026000990805     C                   CLEAR                   DSLV39
026100950111     C*
026200950111     C* SE E' LA PRIMA BOLLA
026300950111    2C     TBL           IFEQ      §3ATB1
026400950111     C*
026500950111     C* SE C/SERVIZIO O STORNO O ASSEGNATO 9999
026600950111    3C     BOL2          IFEQ      '7'
026700950111     C     BOL2          OREQ      'C'
026800950111     C     BOL2          OREQ      'S'
026900950111     C     BOL1          OREQ      'A'
027000950111     C     CODCLI        ANDEQ     9999
027100950111     C*
027200950111     C* NON PASSO LA DATA FATTURA
027300990805     C                   CLEAR                   D39DFT
027400950111     C*
027500950111   X3C                   ELSE
027600990805     C                   MOVEL     TASDFT        D39DFT
027700950111    3C                   ENDIF
027800930623     C*
027900950111   X2C                   ELSE
028000950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
028100990805     C                   CLEAR                   D39DFT
028200950111    2C                   ENDIF
028300930623     C*
028400990805     C                   MOVEL     'C'           D39TPP
028500990921     C                   MOVEL     ARBNCP        D39NCP
028600990805     C                   MOVEL     ARBPKC        D39PLU
028700990805     C                   MOVEL     TASFPF        D39FPF
028800990805     C                   MOVEL     TASPKF        D39PLF
028900990805     C                   MOVEL     TASFPB        D39FPB
029000990805     C                   MOVEL     TASPKB        D39PLB
029100990805     C                   MOVEL     TASAAS        D39AAS
029200990805     C                   MOVEL     TASLNP        D39LNP
029300990805     C                   MOVEL     TASNRS        D39NRS
029400990805     C                   MOVEL     TASNSP        D39NSP
029500990805     C                   MOVEL     ARBCBO        D39CBO
029600990805     C                   MOVEL     TASNCL        D39NCL
029700990805     C                   MOVEL     TASLNA        D39LNA
029800990805     C                   MOVEL     NUM3          D39MBR
029900990805     C                   MOVE      TASMGS        D39DTE
030000990805     C                   MOVEL     TASAAS        D39DTE
030100990805     C* SE IL PESO   CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
030200970904     C*  COLLI RILEVATI, LI METTO UGUALI
030300990805     C     ARBFPC        IFEQ      'T'
030400990805     C     ARBNCP        ANDNE     TASNCL
030500990805     C                   Z-ADD     TASNCL        D39NCP
030600970904     C                   ENDIF
030700950111     C*
030800990805     C                   MOVEL     DSLV39        KPJBU
030900950111     C*
031000990805     C                   CALL      'FNLV39R'
031100950111     C                   PARM                    KPJBA
031200990805     C                   MOVEL     KPJBU         DSLV39
031300950111     C*
031400930623     C*
031500990702     C* SE ERRORE SCRIVO FILE ERRORI
031600990805    2C     D39ERR        IFEQ      '2'
031700950111     C*
031800930623     C* FRANCO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNP
031900950111    3C     BOL1          IFEQ      'F'
032000990701     C                   Z-ADD     TASTFP        FEV
032100950111    3C                   ELSE
032200930623     C* ASSEGNATO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNA
032300990702     C                   CLEAR                   DSLV55
032400990702     C                   MOVEL     'P'           D55TPT
032500990702     C                   Z-ADD     TASLNA        D55LIN
032600990702     C                   Z-ADD     DATEU         D55DRF
032700990702     C                   CALL      'FNLV55R'
032800990702     C                   PARM                    DSLV55
032900990702     C                   Z-ADD     D55TFP        FEV
033000950111    3C                   ENDIF
033100950111     C                   MOVEL     ERR(2)        DER
033200930623     C*
033300930623     C* SCRIVO IL FILE DEGLI ERRORI
033400950111     C                   EXSR      RIEARB
033500950111    2C                   ENDIF
033600990702     C* AGGIORNO IN OGNI CASO I CAMPI DI TITAS (ANCHE SE C'E' ERRORE)
033700990805     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV39R
033800990805     C                   MOVEL     D39NCP        TASNCP
033900990805     C                   MOVEL     D39PLU        TASPKC
034000990805     C                   MOVEL     D39FPF        TASFPF
034100990805     C                   MOVEL     D39PLF        TASPKF
034200970625    1C***********          ENDIF
034300930426     C*
034400930423     C                   ENDSR
034500930423     C*---------------------------------------------------------------*
034600930423     C*                    CAEXA                                      *
034700930423     C* ROUTINE DI APERTURA MEMBRO FILE DI RICEZIONE                  *
034800930423     C*---------------------------------------------------------------*
034900930423     C     CAEXA         BEGSR
035000930423     C*
035100930423     C                   MOVEL     *BLANKS       VAR11            11
035200930423     C                   MOVEL     *BLANKS       VAR12            12
035300930423     C                   MOVE      ')'           VAR11
035400930423     C                   MOVEL     MBRFIL        VAR11
035500930423     C                   MOVE      '))'          VAR12
035600930423     C                   MOVEL     MBRFIL        VAR12
035700930423     C                   Z-ADD     55            LUNG             15 5
035800930423     C* CHKOBJ
035900930423     C                   MOVE      VAR11         CMD1
036000930423     C                   MOVEL     *BLANKS       COMMAN
036100930423     C                   MOVEA     CMD1(1)       COMMAN           80
036200930423     C                   CALL      'QCMDEXC'                            32
036300930423     C                   PARM                    COMMAN
036400930423     C                   PARM                    LUNG
036500930423IF  1C     *IN32         IFEQ      '0'
036600930423     C* ALCOBJ
036700930423     C                   MOVE      VAR12         CMD2
036800930423     C                   MOVEL     *BLANKS       COMMAN
036900930423     C                   MOVEA     CMD2(1)       COMMAN
037000930423     C                   CALL      'QCMDEXC'                            32
037100930423     C                   PARM                    COMMAN
037200930423     C                   PARM                    LUNG
037300930423IF  2C     *IN32         IFEQ      '0'
037400930423     C* OVRDBF
037500930423     C                   MOVE      VAR11         CMD3
037600930423     C                   MOVEL     *BLANKS       COMMAN
037700930423     C                   MOVEA     CMD3(1)       COMMAN
037800930423     C                   CALL      'QCMDEXC'                            32
037900930423     C                   PARM                    COMMAN
038000930423     C                   PARM                    LUNG
038100930423IF  3C     *IN32         IFEQ      '0'
038200930423     C* OPEN
038300990805     C                   OPEN      FNARBP0R
038400930423E   3C                   ENDIF
038500930423E   2C                   ENDIF
038600930423E   1C                   ENDIF
038700930423     C*
038800930423     C                   ENDSR
038900930706     C*---------------------------------------------------------------*
039000950111     C*                    RIEARB                                     *
039100930806     C* ROUTINE DI RIEMPIMENTO ARCHIVIO ERRORI                        *
039200930706     C*---------------------------------------------------------------*
039300950111     C     RIEARB        BEGSR
039400930806     C*
039500950111     C                   MOVEL     DER           ARBDER
039600990805     C     KARB          CHAIN     FNARBP0E                           30
039700930806     C*
039800930706     C* SE NON TROVATO RECORD
039900930806IF  1C     *IN30         IFEQ      *ON
040000990805     C* O TROVATO MA NON E' QUELLE VARIAZIONE DI PESO
040100990805     C     RRBPKC        ORNE      ARBPKC
040200930806     C*
040300930806     C* RISCRIVO ERRORE
040400990805     C                   CLEAR                   FNARBP0E
040500950111     C                   Z-ADD     ARBAAS        RRBAAS
040600950111     C                   Z-ADD     ARBLNP        RRBLNP
040700950111     C                   Z-ADD     ARBNRS        RRBNRS
040800950111     C                   Z-ADD     ARBNSP        RRBNSP
040900950111     C                   MOVEL     ARBCBO        RRBCBO
041000950111     C                   MOVEL     ARBDTV        RRBDTV
041100950111     C                   MOVEL     ARBORV        RRBORV
041200950111     C                   MOVEL     ARBCVB        RRBCVB
041300990805     C                   Z-ADD     ARBPKC        RRBPKC
041400991028     C                   MOVEL     ARBFPC        RRBFPC
041500990805     C                   MOVEL     ARBNCP        RRBNCP
041600950111     C                   MOVEL     FEV           ARBFEV
041700950111     C                   MOVEL     DER           ARBDER
041800950111     C                   CLEAR                   ARBDAT
041900990805     C                   WRITE     FNARBP0E
042000930806E   1C                   ENDIF
042100930806     C*
042200930706     C                   ENDSR
042300930706     C*---------------------------------------------------------------*
042400950111     C*                    ANNARB                                     *
042500930806     C* ROUTINE DI ANNULLAMENTO ERRORE PERCHE' VARIAZIONE CARICATA    *
042600930706     C*---------------------------------------------------------------*
042700950111     C     ANNARB        BEGSR
042800930806     C*
042900990805     C     KARB2         SETLL     FNARBP0E
043000990805     C     KARB2         READE     FNARBP0E                               30
043100930706     C*
043200930806DO  1C     *IN30         DOWEQ     *OFF
043300930706     C*
043400930706     C* NON DEVE ESSERE RECORD STAMPATO (DATA = 0)
043500930706     C*  E NON DEVE ESSERE GIA' FATTURATA CHE LA STAMPO COMUNQUE
043600950111IF  2C     ARBDAT        IFEQ      0
043700950111     C     ARBDER        ANDNE     ERR(2)
043800930706     C*
043900941129     C* SE E' "2 SPE INESISTENTE" DEVO AVERE TROVATO LA 2 BOLLA
044000950111IF  3C     ARBDER        IFEQ      ERR(4)
044100950111IF  4C     *IN01         IFEQ      *OFF
044200990805     C                   DELETE    FNARBP0E
044300950111E   4C                   ENDIF
044400930706     C*
044500950111X   3C                   ELSE
044600930706     C*
044700930706     C* SE E' "1 SPE INESISTENTE" DEVO AVERE TROVATO LA 1 BOLLA (05 ON)
044800950111IF  4C     ARBDER        IFEQ      ERR(1)
044900950111IF  5C     *IN05         IFEQ      *ON
045000990805     C                   DELETE    FNARBP0E
045100950111E   5C                   ENDIF
045200930706     C*
045300950111X   4C                   ELSE
045400930706     C*
045500930706     C* SE E' " CODICE BOLLA INESISTE ANNULLO SEMPRE
045600990805     C                   DELETE    FNARBP0E
045700950111E   4C                   ENDIF
045800941129E   3C                   ENDIF
045900941124E   2C                   ENDIF
046000930706     C*
046100990805     C     KARB2         READE     FNARBP0E                               30
046200930806E   1C                   ENDDO
046300930706     C*
046400930706     C                   ENDSR
046500930806     C*---------------------------------------------------------------*
046600930806     C*                    MANBOL                                     *
046700930806     C* ROUTINE DI CONTROLLO SE MANCA LA BOLLA O E' CAMBIATO IL TIPO  *
046800930806     C*---------------------------------------------------------------*
046900930806     C     MANBOL        BEGSR
047000930806     C*
047100930806     C* 01 02 ON - MANCANO ENTRAMBE SCRIVO FILE DEGLI ERRORI
047200930806IF  1C     *IN01         IFEQ      *ON
047300930806     C     *IN02         ANDEQ     *ON
047400960208     C                   SETON                                        08
047500960208     C*
047600960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
047700960208     C     ARBDTV        IFLE      DATAM
047800930806     C                   Z-ADD     102           FEV
047900930806     C                   MOVEL     ERR(4)        DER
048000950111     C                   EXSR      RIEARB
048100930806     C*
048200930806     C                   Z-ADD     102           FEV
048300930806     C                   MOVEL     ERR(1)        DER
048400950111     C                   EXSR      RIEARB
048500960208     C                   ENDIF
048600930806     C*
048700930806X   1C                   ELSE
048800930806     C*
048900930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
049000930806IF  2C     *IN01         IFEQ      *ON
049100930806     C     *IN02         OREQ      *ON
049200930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
049300930806IF  3C     *IN01         IFEQ      *ON
049400930806     C                   MOVEL     §3ATB1        ALF2
049500930806     C                   EXSR      RICBOL
049600930806     C*
049700930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
049800960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
049900930806IF  4C     TIBOL         IFEQ      *BLANKS
050000960208     C     ARBDTV        IFLE      DATAM
050100930806     C                   Z-ADD     102           FEV
050200930806     C                   MOVEL     ERR(4)        DER
050300950111     C                   EXSR      RIEARB
050400960208     C                   ENDIF
050500930806X   4C                   ELSE
050600930806     C                   MOVEL     TIBOL         §3ATB2
050700990702     C                   SETOFF                                       01
050800930806E   4C                   ENDIF
050900990805     C* RICHIAMO LA PRIMA BOLLA
051000990702     C                   MOVEL     §3ATB1        TBL
051100990702     C                   SETOFF                                       111213
051200990702     C     KTAS          CHAIN     TITAS30C                           37
051300930806     C*
051400930806X   3C                   ELSE
051500990805     C* N O N   T R O V A T A   P R I M A   B O L L A
051600930806     C                   MOVEL     §3ATB2        ALF2              2
051700930806     C                   EXSR      RICBOL
051800930806     C*
051900930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
052000930806IF  4C     TIBOL         IFEQ      *BLANKS
052100960208     C                   SETON                                        08
052200960208     C     ARBDTV        IFLE      DATAM
052300930806     C                   Z-ADD     102           FEV
052400930806     C                   MOVEL     ERR(1)        DER
052500950111     C                   EXSR      RIEARB
052600960208     C                   ENDIF
052700930806X   4C                   ELSE
052800941129     C   11              SETON                                        05
052900941129     C   12              SETON                                        06
053000941129     C   13              SETON                                        07
053100930806     C                   MOVEL     TIBOL         §3ATB1
053200930806E   4C                   ENDIF
053300930806E   3C                   ENDIF
053400930806E   2C                   ENDIF
053500930806E   1C                   ENDIF
053600930806     C*
053700930806     C                   ENDSR
053800930806     C*---------------------------------------------------------------*
053900930806     C*                    RICBOL                                     *
054000930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
054100930806     C*---------------------------------------------------------------*
054200930806     C     RICBOL        BEGSR
054300930806     C*
054400930806     C                   SETOFF                                       3940
054500941129     C                   SETOFF                                       111213
054600930806     C                   CLEAR                   TIBOL
054700930806     C*
054800990701     C     KTAS2         SETLL     TITAS30C
054900990701     C     KTAS2         READE     TITAS30C                               39
055000930806     C*
055100930806DO  1C     *IN39         DOWEQ     *OFF
055200930806     C*
055300930806IF  2C     TASTBL        IFNE      ALF2
055400930806     C                   SETON                                        39
055500930806     C                   MOVEL     TASTBL        TIBOL             2
055600930806E   2C                   ENDIF
055700930806     C*
055800990701     C  N39KTAS2         READE     TITAS30C                               39
055900930806E   1C                   ENDDO
056000930806     C*
056100930806     C                   ENDSR
056200930422     C*---------------------------------------------------------------*
056300930422     C*                    DEFVAR                                     *
056400930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
056500930422     C*---------------------------------------------------------------*
056600930422     C     DEFVAR        BEGSR
056700930422     C*
056800930422     C     *ENTRY        PLIST
056900930422     C                   PARM                    KPJBA
057000930422     C*
057100950111     C                   Z-ADD     1             CODUT             1 0
057200930806     C                   Z-ADD     0             DAT
057300930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
057400930423     C                   MOVEL     MBRFIL        ALF4              4
057500930423     C                   MOVE      ALF4          NUM3              3 0
057600960208     C* UDATE - 3
057700960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
057800960208     C                   TIME                    W0140            14 0
057900960208     C* UDATE IN GGMMAAAA
058000960208     C                   MOVE      W0140         WDTGIO            8 0
058100960208     C*
058200960208     C* UDATE IN AAAAMMGG
058300960208     C                   Z-ADD     WDTGIO        G02DAT
058400960208     C                   MOVEL     *BLANK        G02ERR
058500960208     C                   CALL      'XSRDA8'
058600960208     C                   PARM                    WLBDAT
058700960208     C                   MOVEL     G02INV        DATEU             8 0
058800960208     C* SOTTRAGGO  3
058900960208     C     G02TGI        SUB       3             GIOTGI
059000960208     C*
059100960208     C                   CALL      'XSRGI8'
059200960208     C                   PARM                    WGIDAT
059300960208     C*
059400960208     C                   Z-ADD     GIOINV        DATAM             8 0
059500950111     C*
059600950111     C* DEFINIZIONE CAMPI
059700950111     C     *LIKE         DEFINE    ARBFEV        FEV
059800950111     C     *LIKE         DEFINE    ARBDER        DER
059900950111     C     *LIKE         DEFINE    ARBDAT        DAT
060000950111     C     *LIKE         DEFINE    TBLCOD        COD
060100950111     C     *LIKE         DEFINE    TBLKEY        KEY
060200930422     C*
060300950111     C* ACCESSO TABEL00F
060400930423     C     KTAB          KLIST
060500950111     C                   KFLD                    CODUT
060600950111     C                   KFLD                    COD
060700950111     C                   KFLD                    KEY
060800990701     C* ACCESSO TITAS30C
060900930426     C     KTAS          KLIST
061000950111     C                   KFLD                    ARBAAS
061100950111     C                   KFLD                    ARBLNP
061200950111     C                   KFLD                    ARBNRS
061300950111     C                   KFLD                    ARBNSP
061400930528     C                   KFLD                    TBL               2
061500990701     C* ACCESSO TITAS30C/TNBLA01L
061600930528     C     KTAS2         KLIST
061700950111     C                   KFLD                    ARBAAS
061800950111     C                   KFLD                    ARBLNP
061900950111     C                   KFLD                    ARBNRS
062000950111     C                   KFLD                    ARBNSP
062100991028     C* ACCESSO FIARBE2C
062200950111     C     KARB          KLIST
062300950111     C                   KFLD                    ARBAAS
062400950111     C                   KFLD                    ARBLNP
062500950111     C                   KFLD                    ARBNRS
062600950111     C                   KFLD                    ARBNSP
062700950111     C                   KFLD                    ARBDER
062800950111     C                   KFLD                    DAT
062900950111     C     KARB2         KLIST
063000950111     C                   KFLD                    ARBAAS
063100950111     C                   KFLD                    ARBLNP
063200950111     C                   KFLD                    ARBNRS
063300950111     C                   KFLD                    ARBNSP
063400930423     C*
063500930423     C                   ENDSR
063600930423     C*---------------------------------------------------------------*
063700930423     C*                    QCMDEXC                                    *
063800930423     C*---------------------------------------------------------------*
063900940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
064000930528**         CMD1
064100990805CHKOBJ OBJ(FNARBP0R) OBJTYPE(*FILE)     MBR(M234567890)
064200930423**         CMD2
064300990805ALCOBJ OBJ((FNARBP0R *FILE *EXCL           M234567890))
064400930423**         CMD3
064500990805OVRDBF FILE(FNARBP0R)                   MBR(M234567890)
064600930426**         ERR
0647009305281 SPEDIZIONE INESISTENTE
064800950222SPEDIZIONE GIA' TASSATA
064900930528CODICE BOLLA INESISTENTE
0650009305282 SPEDIZIONE INESISTENTE
