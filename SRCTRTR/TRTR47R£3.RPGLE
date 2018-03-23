000100950110     H* TRTR47R *-----------------------------------------------------*
000200930422     H*            - RICEZIONE VARIAZIONI VOLUME IN SEDE -            *
000300941124     H*---------------------------------------------------------------*
000400930422     H* INDICATORI USATI                                              *
000500930422     H*---------------------------------------------------------------*
000600930806     H* 01 - MANCA SECONDA BOLLA
000700930806     H* 02 - MANCA PRIMA   BOLLA
000800930806     H* 05 - BOLLA TROVATA IN ARCHIVIO CORRENTE
000900930426     H* 06 - BOLLA TROVATA NELLO STORICO
001000930426     H* 07 - BOLLA TROVATA IN ARCHIVIO BOLLE ANNO PRECEDENTE
001100990702     H* 11 - LETTURA DI TITAS000
001200990702     H* 12 - LETTURA DI TITAS010
001300990702     H* 13 - LETTURA DI TITASP00
001400930423     H* 30 - LETTURA TABEL00F
001500930427     H* 31 - RICERCA FILIALE IN TABELLA £1
001600930423     H* 32 - CALL 'QCMDEXC'
001700950110     H* 33 - LETTURA FNARBV0R
001800990702     H* 38 - LETTURA DI TNBLA000
001900930806     H* 39/40 COMOD
002000930422     H*---------------------------------------------------------------*
002100930422     H* DATA BASE                                                     *
002200930422     H*---------------------------------------------------------------*
002300930422     H DECEDIT('0,') DATEDIT(*DMY.)
002400950110     FFNARBV0R  UF   E           K DISK    USROPN
002500941124     FTNBLA01L  IF   E           K DISK
002600990701     FTITAS30C  UF   E           K DISK
002700991103     FFIARBE2C  UF A E           K DISK
002800950111     F                                     RENAME(FNARBV00:FNARBV0E)
002900930423     FTABEL00F  IF   E           K DISK
003000070202     Ffnblp01l  IF   E           K DISK    usropn
003100930422     D*---------------------------------------------------------------*
003200930422     D* SCHIERE                                                       *
003300930422     D*---------------------------------------------------------------*
003400930423     D* COMANDI QCMDEXC
003500930423     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
003600930423     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
003700930423     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
003800930426     D* DESCRIZIONE ERRORE
003900930528     D ERR             S             35    DIM(4) CTDATA PERRCD(1)
004000070202     d cmdt            s             60    dim(01) ctdata perrcd(1)
004100930422     D*---------------------------------------------------------------*
004200930422     D* DS                                                            *
004300930422     D*---------------------------------------------------------------*
004400930422     D KPJBA         E DS
004500930423     D  LEGFIL               483    492
004600930423     D  MBRFIL               493    502
004700960208     D***
004800960208     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
004900960208     D***
005000960208     D WLBDAT          DS                  INZ
005100960208     D  G02DAT                 1      8  0
005200960208     D  G02INV                 9     16  0
005300960208     D  G02ERR                17     17
005400960208     D  G02TGI                18     22  0
005500960208     D***
005600960208     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
005700960208     D***
005800960208     D WGIDAT          DS                  INZ
005900960208     D  GIODAT                 1      8  0
006000960208     D  GIOINV                 9     16  0
006100960208     D  GIOTGI                17     21  0
006200930528     D DS3A          E DS
006300070202     D tibs55ds      E DS
006400950111     D*
006500950111     D* DS PER FNLV20R - DS DEI CAMPI PER CALCOLO VOLUME DA FATTURARE
006600950111     D DSLV20        E DS                  EXTNAME(FNLV20DS)
006700990702     D*
006800990702     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006900070202     d
007000070202     d commt           s             80
007100070202     d lenght          s             15  5 inz(80)
007200070202     i
007300990701     ITITAS000      11
007400990701     ITITAS010      12
007500990701     ITITASP00      13
007600941129     I*
007700950111     IFNARBV0E
007800950111     I              ARBAAS                      RRBAAS
007900950111     I              ARBLNP                      RRBLNP
008000950111     I              ARBNRS                      RRBNRS
008100950111     I              ARBNSP                      RRBNSP
008200950111     I              ARBCBO                      RRBCBO
008300950111     I              ARBDTV                      RRBDTV
008400950111     I              ARBORV                      RRBORV
008500950111     I              ARBCVB                      RRBCVB
008600950111     I              ARBVLC                      RRBVLC
008700950111     I              ARBFVC                      RRBFVC
008800950111     I              ARBNCR                      RRBNCR
008900950111     I*
009000950111£$   I*DS$SLD    E DS
009100930422     C*---------------------------------------------------------------*
009200930422     C*                    MAIN                                       *
009300930423     C* PROGRAMMA DI RICEZIONE IN SEDE VARIAZIONI VOLUME              *
009400930422     C*---------------------------------------------------------------*
009500930528     C*
009600930528     C* DEFINIZIONE VARIABILI, KLIST E PLIST
009700930528     C                   EXSR      DEFVAR
009800930528     C*
009900930528     C* APERTURA MEMBRO DI RICEZIONE
010000930528     C                   EXSR      CAEXA
010100930528     C*
010200930528     C* APERTURA RIUSCITA
010300930528IF  1C     *IN32         IFEQ      '0'
010400930423     C*
010500930423     C* LEGGO VARIAZIONE VOLUME
010600950111     C                   CLEAR                   FNARBV0E
010700950110     C                   READ      FNARBV0R                               33
010800930423DO  2C     *IN33         DOWEQ     '0'
010900930423     C*
011000930423     C* GESTIONE VARIAZIONI DA APPLICARE
011100930423     C                   EXSR      GESMOD
011200950111     C                   CLEAR                   FNARBV0E
011300950110     C                   READ      FNARBV0R                               33
011400930423E   2C                   ENDDO
011500930423     C*
011600930423     C* CHIUSURA MEMBRO DI RICEZIONE
011700950110     C                   CLOSE     FNARBV0R
011800930423E   1C                   ENDIF
011900990702     C*
012000990702     C                   MOVEL     'C'           D55TLA
012100990702     C                   CALL      'FNLV55R'
012200990702     C                   PARM                    DSLV55
012300930422     C*
012400930423     C                   SETON                                        LR
012500930423     C*---------------------------------------------------------------*
012600930423     C*                    GESMOD                                     *
012700930423     C* ROUTINE DI GESTIONE MODIFICHE VOLUME DA APPLICARE             *
012800930423     C*---------------------------------------------------------------*
012900930423     C     GESMOD        BEGSR
013000930426     C*
013100930426     C                   SETOFF                                       050607
013200930528     C                   SETOFF                                       3108
013300930806     C                   SETOFF                                       0102
013400930806     C*
013500930806     C* LETTURA TIPO BOLLA
013600930528     C                   MOVEL     '3A'          COD
013700950111     C                   MOVEL(P)  ARBCBO        KEY
013800930528     C     KTAB          CHAIN     TABEL                              30
013900930528     C*
014000930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
014100930806IF  1C     *IN30         IFEQ      '1'
014200930806     C                   Z-ADD     102           FEV
014300930806     C                   MOVEL     ERR(3)        DER
014400950111     C                   EXSR      RIEARB
014500930528     C*
014600930806X   1C                   ELSE
014700930528     C*
014800930528     C                   MOVEL     TBLUNI        DS3A
014900950905     C* SE SI TRATTA DI UNA BOLLA NON TRASMESSA IN SEDE
015000950905     C* CANCELLO SENZA DARE ERRORE
015100950905   1AC     §3ABSD        IFEQ      ' '
015200930806     C*
015300930806     C* 2   B O L L A
015400930806     C*
015500930806IF  2C     §3ATB2        IFNE      '  '
015600930806     C                   MOVEL     §3ATB2        TBL
015700930528     C*
015800930426     C* CERCO BOLLA IN ARCHIVIO CORRENTE
015900941129     C                   SETOFF                                       111213
016000990701     C     KTAS          CHAIN     TITAS30C                           37
016100930426     C*
016200930426     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
016300941129     C   37KTAS2         CHAIN     TNBLA01L                           38
016400941129     C   37
016500941129     CAN 38              SETON                                        01
016600941129    2C                   ENDIF
016700930426     C*
016800930806     C* 1   B O L L A
016900930806     C*
017000930806     C                   MOVEL     §3ATB1        TBL
017100930528     C*
017200930528     C* CERCO BOLLA IN ARCHIVIO CORRENTE
017300941129     C                   SETOFF                                       111213
017400990701     C     KTAS          CHAIN     TITAS30C                           37
017500930528     C*
017600930528     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
017700941129IF  2C     *IN37         IFEQ      '1'
017800941129     C     KTAS2         CHAIN     TNBLA01L                           38
017900941129     C   38              SETON                                        02
018000930528     C*
018100941129X   2C                   ELSE
018200941129     C* BOLLA TROVATA
018300941129     C   11              SETON                                        05
018400941129     C   12              SETON                                        06
018500941129     C   13              SETON                                        07
018600941129E   2C                   ENDIF
018700930806     C*
018800930806     C* CONTROLLO SE MANCANO VERAMENTE O E' CAMBIATO IL TIPO BOLLA
018900930806     C   01
019000930806     COR 02              EXSR      MANBOL
019100930806     C*
019200930806     C* SE PRIMA BOLLA INESISTENTE ESCO E NON AGGIORNO
019300930806IF  2C     *IN08         IFEQ      '0'
019400070202     c
019500070202     c* Se chiusa in blp non stampo ma cancello in fnarbv0r
019600070202   2ac     wChiusainBLP  ifeq      ' '
019700930806     C*
019800930528     C* 1 BOLLA TROVATA IN ARCHIVIO (CORRENTE, STORICO O PRECEDENTE)
019900930528IF  3C     *IN05         IFEQ      '1'
020000930427     C     *IN06         OREQ      '1'
020100930427     C     *IN07         OREQ      '1'
020200930528     C                   EXSR      AGGIOR
020300930528     C*
020400930806IF  4C     *IN05         IFEQ      *ON
020500930806     C*
020600930706     C* SE ESISTE ERRORE NON STAMPATO CANCELLO
020700950111     C                   EXSR      ANNARB
020800930706     C*
020900990701     C                   UPDATE    TITAS000
021000930806E   4C                   ENDIF
021100930706     C*
021200990701     C   06              UPDATE    TITAS010
021300940208     C*
021400990701     C   07              UPDATE    TITASP00
021500930806E   3C                   ENDIF
021600930806     C*
021700941129     C* 2 BOLLA TROVATA IN ARCHIVIO
021800941129IF  3C     §3ATB2        IFNE      *BLANKS
021900941129     C     *IN01         ANDEQ     *OFF
022000941129     C*
022100930528     C                   MOVEL     §3ATB2        TBL
022200941129     C                   SETOFF                                       111213
022300990701     C     KTAS          CHAIN     TITAS30C                           30
022400930528     C*
022500941129    4C     *IN30         IFEQ      *OFF
022600930528     C                   EXSR      AGGIOR
022700930528     C*
022800990701     C   11              UPDATE    TITAS000
022900990701     C   12              UPDATE    TITAS010
023000990701     C   13              UPDATE    TITASP00
023100941129E   4C                   ENDIF
023200941129E   3C                   ENDIF
023300070202E  2aC                   ENDIF
023400930528     C*
023500070202     C*
023600950110     C                   DELETE    FNARBV0R
023700930806E   2C                   ENDIF
023800950905     C*
023900950905X  1AC                   ELSE
024000950905     C                   DELETE    FNARBV0R
024100950905E  1AC                   ENDIF
024200950905E   1C                   ENDIF
024300930806     C*
024400930528     C                   ENDSR
024500930528     C*---------------------------------------------------------------*
024600930528     C*                    AGGIOR                                     *
024700930806     C* ROUTINE DI AGGIORNAMENTO VOLUME SULLA BOLLA                   *
024800930528     C*---------------------------------------------------------------*
024900930528     C     AGGIOR        BEGSR
025000930426     C*
025100930426     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO VOLUME UGUALE A 'T'
025200970625    1C******     TASFVF    IFNE 'T'
025300930528     C*
025400930806     C                   MOVEL     TASTBL        BOL1              1
025500930806     C                   MOVE      TASTBL        BOL2              1
025600930426     C                   MOVE      TASKSC        CODCLI            4 0
025700930806     C*
025800950111     C                   CLEAR                   DSLV20
025900950111     C*
026000950111     C* SE E' LA PRIMA BOLLA
026100950111    2C     TBL           IFEQ      §3ATB1
026200950111     C*
026300950111     C* SE C/SERVIZIO O STORNO O ASSEGNATO 9999
026400950111    3C     BOL2          IFEQ      '7'
026500950111     C     BOL2          OREQ      'C'
026600950111     C     BOL2          OREQ      'S'
026700950111     C     BOL1          OREQ      'A'
026800950111     C     CODCLI        ANDEQ     9999
026900950111     C*
027000950111     C* NON PASSO LA DATA FATTURA
027100950111     C                   CLEAR                   D20DFT
027200950111     C*
027300950111   X3C                   ELSE
027400950111     C                   MOVEL     TASDFT        D20DFT
027500950111    3C                   ENDIF
027600930623     C*
027700950111   X2C                   ELSE
027800950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
027900950111     C                   CLEAR                   D20DFT
028000950111    2C                   ENDIF
028100930623     C*
028200950111     C                   MOVEL     'C'           D20TVL
028300950111     C                   MOVEL     ARBNCR        D20NCR
028400950111     C                   MOVEL     ARBVLC        D20VLU
028500950111     C                   MOVEL     TASFVF        D20FVF
028600950111     C                   MOVEL     TASVLF        D20VLF
028700950111     C                   MOVEL     TASFVB        D20FVB
028800950111     C                   MOVEL     TASVLB        D20VLB
028900950111     C                   MOVEL     TASAAS        D20AAS
029000950111     C                   MOVEL     TASLNP        D20LNP
029100950111     C                   MOVEL     TASNRS        D20NRS
029200950111     C                   MOVEL     TASNSP        D20NSP
029300950111     C                   MOVEL     ARBCBO        D20CBO
029400950111     C                   MOVEL     TASNCL        D20NCL
029500950111     C                   MOVEL     TASLNA        D20LNA
029600950111     C                   MOVEL     NUM3          D20MBR
029700960927     C                   MOVE      TASMGS        D20DTE
029800960927     C                   MOVEL     TASAAS        D20DTE
029900970904     C* SE IL VOLUME CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
030000970904     C*  COLLI RILEVATI, LI METTO UGUALI
030100970904     C     ARBFVC        IFEQ      'T'
030200970904     C     ARBNCR        ANDNE     TASNCL
030300970904     C                   Z-ADD     TASNCL        D20NCR
030400970904     C                   ENDIF
030500950111     C*
030600950111     C                   MOVEL     DSLV20        KPJBU
030700950111     C*
030800950111     C                   CALL      'FNLV20R'
030900950111     C                   PARM                    KPJBA
031000950111     C                   MOVEL     KPJBU         DSLV20
031100950111     C*
031200930623     C*
031300990702     C* SE ERRORE SCRIVO FILE ERRORI
031400950111    2C     D20ERR        IFEQ      '2'
031500950111     C*
031600930623     C* FRANCO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNP
031700950111    3C     BOL1          IFEQ      'F'
031800990701     C                   Z-ADD     TASTFP        FEV
031900950111    3C                   ELSE
032000930623     C* ASSEGNATO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNA
032100990702     C                   CLEAR                   DSLV55
032200990702     C                   MOVEL     'P'           D55TPT
032300990702     C                   Z-ADD     TASLNA        D55LIN
032400990702     C                   Z-ADD     DATEU         D55DRF
032500990702     C                   CALL      'FNLV55R'
032600990702     C                   PARM                    DSLV55
032700990702     C                   Z-ADD     D55TFP        FEV
032800950111    3C                   ENDIF
032900950111     C                   MOVEL     ERR(2)        DER
033000930623     C*
033100930623     C* SCRIVO IL FILE DEGLI ERRORI
033200950111     C                   EXSR      RIEARB
033300950111    2C                   ENDIF
033400990702     C* AGGIORNO IN OGNI CASO I CAMPI DI TITAS (ANCHE SE C'E' ERRORE)
033500990702     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV20R
033600950111     C                   MOVEL     D20NCR        TASNCR
033700950111     C                   MOVEL     D20VLU        TASVLC
033800950111     C                   MOVEL     D20FVF        TASFVF
033900950111     C                   MOVEL     D20VLF        TASVLF
034000970625    1C***********          ENDIF
034100930426     C*
034200930423     C                   ENDSR
034300930423     C*---------------------------------------------------------------*
034400930423     C*                    CAEXA                                      *
034500930423     C* ROUTINE DI APERTURA MEMBRO FILE DI RICEZIONE                  *
034600930423     C*---------------------------------------------------------------*
034700930423     C     CAEXA         BEGSR
034800930423     C*
034900930423     C                   MOVEL     *BLANKS       VAR11            11
035000930423     C                   MOVEL     *BLANKS       VAR12            12
035100930423     C                   MOVE      ')'           VAR11
035200930423     C                   MOVEL     MBRFIL        VAR11
035300930423     C                   MOVE      '))'          VAR12
035400930423     C                   MOVEL     MBRFIL        VAR12
035500930423     C                   Z-ADD     55            LUNG             15 5
035600930423     C* CHKOBJ
035700930423     C                   MOVE      VAR11         CMD1
035800930423     C                   MOVEL     *BLANKS       COMMAN
035900930423     C                   MOVEA     CMD1(1)       COMMAN           80
036000930423     C                   CALL      'QCMDEXC'                            32
036100930423     C                   PARM                    COMMAN
036200930423     C                   PARM                    LUNG
036300930423IF  1C     *IN32         IFEQ      '0'
036400930423     C* ALCOBJ
036500930423     C                   MOVE      VAR12         CMD2
036600930423     C                   MOVEL     *BLANKS       COMMAN
036700930423     C                   MOVEA     CMD2(1)       COMMAN
036800930423     C                   CALL      'QCMDEXC'                            32
036900930423     C                   PARM                    COMMAN
037000930423     C                   PARM                    LUNG
037100930423IF  2C     *IN32         IFEQ      '0'
037200930423     C* OVRDBF
037300930423     C                   MOVE      VAR11         CMD3
037400930423     C                   MOVEL     *BLANKS       COMMAN
037500930423     C                   MOVEA     CMD3(1)       COMMAN
037600930423     C                   CALL      'QCMDEXC'                            32
037700930423     C                   PARM                    COMMAN
037800930423     C                   PARM                    LUNG
037900930423IF  3C     *IN32         IFEQ      '0'
038000930423     C* OPEN
038100950110     C                   OPEN      FNARBV0R
038200930423E   3C                   ENDIF
038300930423E   2C                   ENDIF
038400930423E   1C                   ENDIF
038500930423     C*
038600930423     C                   ENDSR
038700930706     C*---------------------------------------------------------------*
038800950111     C*                    RIEARB                                     *
038900930806     C* ROUTINE DI RIEMPIMENTO ARCHIVIO ERRORI                        *
039000930706     C*---------------------------------------------------------------*
039100950111     C     RIEARB        BEGSR
039200930806     C*
039300950111     C                   MOVEL     DER           ARBDER
039400950111     C     KARB          CHAIN     FNARBV0E                           30
039500930806     C*
039600930706     C* SE NON TROVATO RECORD
039700930806IF  1C     *IN30         IFEQ      *ON
039800930806     C* O TROVATO MA NON E' QUELLE VARIAZIONE DI VOLUME
039900950111     C     RRBVLC        ORNE      ARBVLC
040000930806     C*
040100930806     C* RISCRIVO ERRORE
040200950111     C                   CLEAR                   FNARBV0E
040300950111     C                   Z-ADD     ARBAAS        RRBAAS
040400950111     C                   Z-ADD     ARBLNP        RRBLNP
040500950111     C                   Z-ADD     ARBNRS        RRBNRS
040600950111     C                   Z-ADD     ARBNSP        RRBNSP
040700950111     C                   MOVEL     ARBCBO        RRBCBO
040800950111     C                   MOVEL     ARBDTV        RRBDTV
040900950111     C                   MOVEL     ARBORV        RRBORV
041000950111     C                   MOVEL     ARBCVB        RRBCVB
041100950111     C                   Z-ADD     ARBVLC        RRBVLC
041200950111     C                   MOVEL     ARBFVC        RRBFVC
041300950111     C                   MOVEL     ARBNCR        RRBNCR
041400950111     C                   MOVEL     FEV           ARBFEV
041500950111     C                   MOVEL     DER           ARBDER
041600950111     C                   CLEAR                   ARBDAT
041700950111     C                   WRITE     FNARBV0E
041800930806E   1C                   ENDIF
041900930806     C*
042000930706     C                   ENDSR
042100930706     C*---------------------------------------------------------------*
042200950111     C*                    ANNARB                                     *
042300930806     C* ROUTINE DI ANNULLAMENTO ERRORE PERCHE' VARIAZIONE CARICATA    *
042400930706     C*---------------------------------------------------------------*
042500950111     C     ANNARB        BEGSR
042600930806     C*
042700950111     C     KARB2         SETLL     FNARBV0E
042800950111     C     KARB2         READE     FNARBV0E                               30
042900930706     C*
043000930806DO  1C     *IN30         DOWEQ     *OFF
043100930706     C*
043200930706     C* NON DEVE ESSERE RECORD STAMPATO (DATA = 0)
043300930706     C*  E NON DEVE ESSERE GIA' FATTURATA CHE LA STAMPO COMUNQUE
043400950111IF  2C     ARBDAT        IFEQ      0
043500950111     C     ARBDER        ANDNE     ERR(2)
043600930706     C*
043700941129     C* SE E' "2 SPE INESISTENTE" DEVO AVERE TROVATO LA 2 BOLLA
043800950111IF  3C     ARBDER        IFEQ      ERR(4)
043900950111IF  4C     *IN01         IFEQ      *OFF
044000950111     C                   DELETE    FNARBV0E
044100950111E   4C                   ENDIF
044200930706     C*
044300950111X   3C                   ELSE
044400930706     C*
044500930706     C* SE E' "1 SPE INESISTENTE" DEVO AVERE TROVATO LA 1 BOLLA (05 ON)
044600950111IF  4C     ARBDER        IFEQ      ERR(1)
044700950111IF  5C     *IN05         IFEQ      *ON
044800950111     C                   DELETE    FNARBV0E
044900950111E   5C                   ENDIF
045000930706     C*
045100950111X   4C                   ELSE
045200930706     C*
045300930706     C* SE E' " CODICE BOLLA INESISTE ANNULLO SEMPRE
045400950111     C                   DELETE    FNARBV0E
045500950111E   4C                   ENDIF
045600941129E   3C                   ENDIF
045700941124E   2C                   ENDIF
045800930706     C*
045900950111     C     KARB2         READE     FNARBV0E                               30
046000930806E   1C                   ENDDO
046100930706     C*
046200930706     C                   ENDSR
046300930806     C*---------------------------------------------------------------*
046400930806     C*                    MANBOL                                     *
046500930806     C* ROUTINE DI CONTROLLO SE MANCA LA BOLLA O E' CAMBIATO IL TIPO  *
046600930806     C*---------------------------------------------------------------*
046700930806     C     MANBOL        BEGSR
046800930806     C*
046900930806     C* 01 02 ON - MANCANO ENTRAMBE SCRIVO FILE DEGLI ERRORI
047000930806IF  1C     *IN01         IFEQ      *ON
047100930806     C     *IN02         ANDEQ     *ON
047200070202     c                   exsr      CERBLP
047300070202     c
047400070202     c* Non trovata nemmeno in blp come merce mai affidata
047500070202   1ac                   if        wChiusainBLP=' '
047600960208     C                   SETON                                        08
047700960208     C*
047800960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
047900960208     C     ARBDTV        IFLE      DATAM
048000930806     C                   Z-ADD     102           FEV
048100930806     C                   MOVEL     ERR(4)        DER
048200950111     C                   EXSR      RIEARB
048300930806     C*
048400930806     C                   Z-ADD     102           FEV
048500930806     C                   MOVEL     ERR(1)        DER
048600950111     C                   EXSR      RIEARB
048700960208     C                   ENDIF
048800070202   1aC                   ENDIF
048900930806     C*
049000930806X   1C                   ELSE
049100930806     C*
049200930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
049300930806IF  2C     *IN01         IFEQ      *ON
049400930806     C     *IN02         OREQ      *ON
049500930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
049600930806IF  3C     *IN01         IFEQ      *ON
049700930806     C                   MOVEL     §3ATB1        ALF2
049800930806     C                   EXSR      RICBOL
049900930806     C*
050000930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
050100960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
050200930806IF  4C     TIBOL         IFEQ      *BLANKS
050300960208     C     ARBDTV        IFLE      DATAM
050400930806     C                   Z-ADD     102           FEV
050500930806     C                   MOVEL     ERR(4)        DER
050600950111     C                   EXSR      RIEARB
050700960208     C                   ENDIF
050800930806X   4C                   ELSE
050900930806     C                   MOVEL     TIBOL         §3ATB2
051000990702     C                   SETOFF                                       01
051100930806E   4C                   ENDIF
051200990702     C* RICHAINO LA PRIMA BOLLA
051300990702     C                   MOVEL     §3ATB1        TBL
051400990702     C                   SETOFF                                       111213
051500990702     C     KTAS          CHAIN     TITAS30C                           37
051600930806     C*
051700930806X   3C                   ELSE
051800930806     C* N O N   T R O V AT A   P R I M A   B O L L A
051900930806     C                   MOVEL     §3ATB2        ALF2              2
052000930806     C                   EXSR      RICBOL
052100930806     C*
052200930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
052300930806IF  4C     TIBOL         IFEQ      *BLANKS
052400070202     c* Se la bolla è chiusa in partenza con cca=/-merce mai affidadata
052500070202     c*  cancello senza dare erorre
052600070202     c                   exsr      CERBLP
052700070202     c
052800070202     c* Non trovata nemmeno in blp come merce mai affidata
052900070202    5c                   if        wChiusainBLP=' '
053000070202     c
053100960208     C                   SETON                                        08
053200960208     C     ARBDTV        IFLE      DATAM
053300930806     C                   Z-ADD     102           FEV
053400930806     C                   MOVEL     ERR(1)        DER
053500950111     C                   EXSR      RIEARB
053600960208     C                   ENDIF
053700070202E   5C                   ENDIF
053800070202     c
053900930806X   4C                   ELSE
054000941129     C   11              SETON                                        05
054100941129     C   12              SETON                                        06
054200941129     C   13              SETON                                        07
054300930806     C                   MOVEL     TIBOL         §3ATB1
054400070202E   4C                   ENDIF
054500930806E   3C                   ENDIF
054600930806E   2C                   ENDIF
054700930806E   1C                   ENDIF
054800930806     C*
054900930806     C                   ENDSR
055000070202     C*---------------------------------------------------------------*
055100070202     C*                    CERBLP                                     *
055200070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
055300070202     c* merce mai affdata
055400070202     C*---------------------------------------------------------------*
055500070202     C     CERBLP        BEGSR
055600070202     c                   clear                   wChiusainBLP      1
055700070202     c     ktas2         chain     fnblp01l
055800070202    5c                   if        %found(fnblp01l) and blpft1='N'
055900070202     c                             and blpcca='7'
056000070202     c                   eval      wChiusainBLP='S'
056100070202    5c                   endif
056200070202     c                   ENDSR
056300930806     C*---------------------------------------------------------------*
056400930806     C*                    RICBOL                                     *
056500930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
056600930806     C*---------------------------------------------------------------*
056700930806     C     RICBOL        BEGSR
056800930806     C*
056900930806     C                   SETOFF                                       3940
057000941129     C                   SETOFF                                       111213
057100930806     C                   CLEAR                   TIBOL
057200930806     C*
057300990701     C     KTAS2         SETLL     TITAS30C
057400990701     C     KTAS2         READE     TITAS30C                               39
057500930806     C*
057600930806DO  1C     *IN39         DOWEQ     *OFF
057700930806     C*
057800930806IF  2C     TASTBL        IFNE      ALF2
057900930806     C                   SETON                                        39
058000930806     C                   MOVEL     TASTBL        TIBOL             2
058100930806E   2C                   ENDIF
058200930806     C*
058300990701     C  N39KTAS2         READE     TITAS30C                               39
058400930806E   1C                   ENDDO
058500930806     C*
058600930806     C*
058700930806     C                   ENDSR
058800930422     C*---------------------------------------------------------------*
058900930422     C*                    DEFVAR                                     *
059000930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
059100930422     C*---------------------------------------------------------------*
059200930422     C     DEFVAR        BEGSR
059300930422     C*
059400930422     C     *ENTRY        PLIST
059500930422     C                   PARM                    KPJBA
059600930422     C*
059700950111     C                   Z-ADD     1             CODUT             1 0
059800930806     C                   Z-ADD     0             DAT
059900930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
060000930423     C                   MOVEL     MBRFIL        ALF4              4
060100930423     C                   MOVE      ALF4          NUM3              3 0
060200960208     C* UDATE - 3
060300960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
060400960208     C                   TIME                    W0140            14 0
060500960208     C* UDATE IN GGMMAAAA
060600960208     C                   MOVE      W0140         WDTGIO            8 0
060700960208     C*
060800960208     C* UDATE IN AAAAMMGG
060900960208     C                   Z-ADD     WDTGIO        G02DAT
061000960208     C                   MOVEL     *BLANK        G02ERR
061100960208     C                   CALL      'XSRDA8'
061200960208     C                   PARM                    WLBDAT
061300960208     C                   MOVEL     G02INV        DATEU             8 0
061400960208     C* SOTTRAGGO  3
061500960208     C     G02TGI        SUB       3             GIOTGI
061600960208     C*
061700960208     C                   CALL      'XSRGI8'
061800960208     C                   PARM                    WGIDAT
061900960208     C*
062000960208     C                   Z-ADD     GIOINV        DATAM             8 0
062100070202     c* Apro file bolle di filiale
062200070202     c                   clear                   tibs55ds
062300070202     c                   eval      i50tla='L'
062400070202     c                   eval      i50apo=001
062500070202     c                   call      'TIBS55R'
062600070202     c                   parm                    tibs55ds
062700070202     c*
062800070202     c                   Clear                   commt
062900070202     c                   Movel(p)  cmdt(1)       commt
063000070202     c                   Eval      %Subst(commt:30:10) = o50ala
063100070202     c                   Eval      commt =
063200070202     c                             %trim(commt) + '/FNBLP01L)'
063300070202     c                   Call      'QCMDEXC'
063400070202     c                   Parm                    commt
063500070202     c                   Parm                    lenght
063600070202     c
063700070202     c                   Open      FNBLP01L
063800950111     C*
063900950111     C* DEFINIZIONE CAMPI
064000950111     C     *LIKE         DEFINE    ARBFEV        FEV
064100950111     C     *LIKE         DEFINE    ARBDER        DER
064200950111     C     *LIKE         DEFINE    ARBDAT        DAT
064300950111     C     *LIKE         DEFINE    TBLCOD        COD
064400950111     C     *LIKE         DEFINE    TBLKEY        KEY
064500930422     C*
064600950111     C* ACCESSO TABEL00F
064700930423     C     KTAB          KLIST
064800950111     C                   KFLD                    CODUT
064900950111     C                   KFLD                    COD
065000950111     C                   KFLD                    KEY
065100990701     C* ACCESSO TITAS30C
065200930426     C     KTAS          KLIST
065300950111     C                   KFLD                    ARBAAS
065400950111     C                   KFLD                    ARBLNP
065500950111     C                   KFLD                    ARBNRS
065600950111     C                   KFLD                    ARBNSP
065700930528     C                   KFLD                    TBL               2
065800990701     C* ACCESSO TITAS30C/TNBLA01L
065900930528     C     KTAS2         KLIST
066000950111     C                   KFLD                    ARBAAS
066100950111     C                   KFLD                    ARBLNP
066200950111     C                   KFLD                    ARBNRS
066300950111     C                   KFLD                    ARBNSP
066400991103     C* ACCESSO FIARBE2C
066500950111     C     KARB          KLIST
066600950111     C                   KFLD                    ARBAAS
066700950111     C                   KFLD                    ARBLNP
066800950111     C                   KFLD                    ARBNRS
066900950111     C                   KFLD                    ARBNSP
067000950111     C                   KFLD                    ARBDER
067100950111     C                   KFLD                    DAT
067200950111     C     KARB2         KLIST
067300950111     C                   KFLD                    ARBAAS
067400950111     C                   KFLD                    ARBLNP
067500950111     C                   KFLD                    ARBNRS
067600950111     C                   KFLD                    ARBNSP
067700930423     C*
067800930423     C                   ENDSR
067900930423     C*---------------------------------------------------------------*
068000930423     C*                    QCMDEXC                                    *
068100930423     C*---------------------------------------------------------------*
068200940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
068300930528**         CMD1
068400950110CHKOBJ OBJ(FNARBV0R) OBJTYPE(*FILE)     MBR(M234567890)
068500930423**         CMD2
068600950110ALCOBJ OBJ((FNARBV0R *FILE *EXCL           M234567890))
068700930423**         CMD3
068800950110OVRDBF FILE(FNARBV0R)                   MBR(M234567890)
068900930426**         ERR
0690009305281 SPEDIZIONE INESISTENTE
069100950222SPEDIZIONE GIA' TASSATA
069200930528CODICE BOLLA INESISTENTE
0693009305282 SPEDIZIONE INESISTENTE
069400070202** cmdt
069500070202OVRDBF FILE(FNBLP01L) TOFILE(
