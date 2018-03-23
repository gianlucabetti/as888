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
003000070302     Fazorg01l  IF   E           K DISK
003100070202     Ffnblp01l  IF   E           K DISK    usropn
003200930422     D*---------------------------------------------------------------*
003300930422     D* SCHIERE                                                       *
003400930422     D*---------------------------------------------------------------*
003500930423     D* COMANDI QCMDEXC
003600930423     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
003700930423     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
003800930423     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
003900930426     D* DESCRIZIONE ERRORE
004000930528     D ERR             S             35    DIM(4) CTDATA PERRCD(1)
004100070202     d cmdt            s             60    dim(01) ctdata perrcd(1)
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
006400070302     D og143         E DS
006500070202     D tibs55ds      E DS
006600950111     D*
006700950111     D* DS PER FNLV20R - DS DEI CAMPI PER CALCOLO VOLUME DA FATTURARE
006800950111     D DSLV20        E DS                  EXTNAME(FNLV20DS)
006900990702     D*
007000990702     D DSLV55        E DS                  EXTNAME(FNLV55DS)
007100070202     d
007200070202     d commt           s             80
007300070202     d lenght          s             15  5 inz(80)
007400070202     i
007500990701     ITITAS000      11
007600990701     ITITAS010      12
007700990701     ITITASP00      13
007800941129     I*
007900950111     IFNARBV0E
008000950111     I              ARBAAS                      RRBAAS
008100950111     I              ARBLNP                      RRBLNP
008200950111     I              ARBNRS                      RRBNRS
008300950111     I              ARBNSP                      RRBNSP
008400950111     I              ARBCBO                      RRBCBO
008500950111     I              ARBDTV                      RRBDTV
008600950111     I              ARBORV                      RRBORV
008700950111     I              ARBCVB                      RRBCVB
008800950111     I              ARBVLC                      RRBVLC
008900950111     I              ARBFVC                      RRBFVC
009000950111     I              ARBNCR                      RRBNCR
009100950111     I*
009200950111£$   I*DS$SLD    E DS
009300930422     C*---------------------------------------------------------------*
009400930422     C*                    MAIN                                       *
009500930423     C* PROGRAMMA DI RICEZIONE IN SEDE VARIAZIONI VOLUME              *
009600930422     C*---------------------------------------------------------------*
009700930528     C*
009800930528     C* DEFINIZIONE VARIABILI, KLIST E PLIST
009900930528     C                   EXSR      DEFVAR
010000930528     C*
010100930528     C* APERTURA MEMBRO DI RICEZIONE
010200930528     C                   EXSR      CAEXA
010300930528     C*
010400930528     C* APERTURA RIUSCITA
010500930528IF  1C     *IN32         IFEQ      '0'
010600930423     C*
010700930423     C* LEGGO VARIAZIONE VOLUME
010800950111     C                   CLEAR                   FNARBV0E
010900950110     C                   READ      FNARBV0R                               33
011000930423DO  2C     *IN33         DOWEQ     '0'
011100930423     C*
011200930423     C* GESTIONE VARIAZIONI DA APPLICARE
011300930423     C                   EXSR      GESMOD
011400950111     C                   CLEAR                   FNARBV0E
011500950110     C                   READ      FNARBV0R                               33
011600930423E   2C                   ENDDO
011700930423     C*
011800930423     C* CHIUSURA MEMBRO DI RICEZIONE
011900950110     C                   CLOSE     FNARBV0R
012000930423E   1C                   ENDIF
012100990702     C*
012200990702     C                   MOVEL     'C'           D55TLA
012300990702     C                   CALL      'FNLV55R'
012400990702     C                   PARM                    DSLV55
012500930422     C*
012600930423     C                   SETON                                        LR
012700930423     C*---------------------------------------------------------------*
012800930423     C*                    GESMOD                                     *
012900930423     C* ROUTINE DI GESTIONE MODIFICHE VOLUME DA APPLICARE             *
013000930423     C*---------------------------------------------------------------*
013100930423     C     GESMOD        BEGSR
013200930426     C*
013300930426     C                   SETOFF                                       050607
013400930528     C                   SETOFF                                       3108
013500930806     C                   SETOFF                                       0102
013600930806     C*
013700930806     C* LETTURA TIPO BOLLA
013800930528     C                   MOVEL     '3A'          COD
013900950111     C                   MOVEL(P)  ARBCBO        KEY
014000930528     C     KTAB          CHAIN     TABEL                              30
014100930528     C*
014200930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
014300930806IF  1C     *IN30         IFEQ      '1'
014400930806     C                   Z-ADD     102           FEV
014500930806     C                   MOVEL     ERR(3)        DER
014600950111     C                   EXSR      RIEARB
014700930528     C*
014800930806X   1C                   ELSE
014900930528     C*
015000930528     C                   MOVEL     TBLUNI        DS3A
015100950905     C* SE SI TRATTA DI UNA BOLLA NON TRASMESSA IN SEDE
015200950905     C* CANCELLO SENZA DARE ERRORE
015300950905   1AC     §3ABSD        IFEQ      ' '
015400930806     C*
015500930806     C* 2   B O L L A
015600930806     C*
015700930806IF  2C     §3ATB2        IFNE      '  '
015800930806     C                   MOVEL     §3ATB2        TBL
015900930528     C*
016000930426     C* CERCO BOLLA IN ARCHIVIO CORRENTE
016100941129     C                   SETOFF                                       111213
016200990701     C     KTAS          CHAIN     TITAS30C                           37
016300930426     C*
016400930426     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
016500941129     C   37KTAS2         CHAIN     TNBLA01L                           38
016600941129     C   37
016700941129     CAN 38              SETON                                        01
016800941129    2C                   ENDIF
016900930426     C*
017000930806     C* 1   B O L L A
017100930806     C*
017200930806     C                   MOVEL     §3ATB1        TBL
017300930528     C*
017400930528     C* CERCO BOLLA IN ARCHIVIO CORRENTE
017500941129     C                   SETOFF                                       111213
017600990701     C     KTAS          CHAIN     TITAS30C                           37
017700930528     C*
017800930528     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
017900941129IF  2C     *IN37         IFEQ      '1'
018000941129     C     KTAS2         CHAIN     TNBLA01L                           38
018100941129     C   38              SETON                                        02
018200930528     C*
018300941129X   2C                   ELSE
018400941129     C* BOLLA TROVATA
018500941129     C   11              SETON                                        05
018600941129     C   12              SETON                                        06
018700941129     C   13              SETON                                        07
018800941129E   2C                   ENDIF
018900930806     C*
019000930806     C* CONTROLLO SE MANCANO VERAMENTE O E' CAMBIATO IL TIPO BOLLA
019100930806     C   01
019200930806     COR 02              EXSR      MANBOL
019300930806     C*
019400930806     C* SE PRIMA BOLLA INESISTENTE ESCO E NON AGGIORNO
019500930806IF  2C     *IN08         IFEQ      '0'
019600070202     c
019700070202     c* Se chiusa in blp non stampo ma cancello in fnarbv0r
019800070202   2ac     wChiusainBLP  ifeq      ' '
019900930806     C*
020000930528     C* 1 BOLLA TROVATA IN ARCHIVIO (CORRENTE, STORICO O PRECEDENTE)
020100930528IF  3C     *IN05         IFEQ      '1'
020200930427     C     *IN06         OREQ      '1'
020300930427     C     *IN07         OREQ      '1'
020400930528     C                   EXSR      AGGIOR
020500930528     C*
020600930806IF  4C     *IN05         IFEQ      *ON
020700930806     C*
020800930706     C* SE ESISTE ERRORE NON STAMPATO CANCELLO
020900950111     C                   EXSR      ANNARB
021000930706     C*
021100990701     C                   UPDATE    TITAS000
021200930806E   4C                   ENDIF
021300930706     C*
021400990701     C   06              UPDATE    TITAS010
021500940208     C*
021600990701     C   07              UPDATE    TITASP00
021700930806E   3C                   ENDIF
021800930806     C*
021900941129     C* 2 BOLLA TROVATA IN ARCHIVIO
022000941129IF  3C     §3ATB2        IFNE      *BLANKS
022100941129     C     *IN01         ANDEQ     *OFF
022200941129     C*
022300930528     C                   MOVEL     §3ATB2        TBL
022400941129     C                   SETOFF                                       111213
022500990701     C     KTAS          CHAIN     TITAS30C                           30
022600930528     C*
022700941129    4C     *IN30         IFEQ      *OFF
022800930528     C                   EXSR      AGGIOR
022900930528     C*
023000990701     C   11              UPDATE    TITAS000
023100990701     C   12              UPDATE    TITAS010
023200990701     C   13              UPDATE    TITASP00
023300941129E   4C                   ENDIF
023400941129E   3C                   ENDIF
023500070202E  2aC                   ENDIF
023600930528     C*
023700070202     C*
023800950110     C                   DELETE    FNARBV0R
023900930806E   2C                   ENDIF
024000950905     C*
024100950905X  1AC                   ELSE
024200950905     C                   DELETE    FNARBV0R
024300950905E  1AC                   ENDIF
024400950905E   1C                   ENDIF
024500930806     C*
024600930528     C                   ENDSR
024700930528     C*---------------------------------------------------------------*
024800930528     C*                    AGGIOR                                     *
024900930806     C* ROUTINE DI AGGIORNAMENTO VOLUME SULLA BOLLA                   *
025000930528     C*---------------------------------------------------------------*
025100930528     C     AGGIOR        BEGSR
025200930426     C*
025300930426     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO VOLUME UGUALE A 'T'
025400970625    1C******     TASFVF    IFNE 'T'
025500930528     C*
025600930806     C                   MOVEL     TASTBL        BOL1              1
025700930806     C                   MOVE      TASTBL        BOL2              1
025800930426     C                   MOVE      TASKSC        CODCLI            4 0
025900930806     C*
026000950111     C                   CLEAR                   DSLV20
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
027300950111     C                   CLEAR                   D20DFT
027400950111     C*
027500950111   X3C                   ELSE
027600950111     C                   MOVEL     TASDFT        D20DFT
027700950111    3C                   ENDIF
027800930623     C*
027900950111   X2C                   ELSE
028000950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
028100950111     C                   CLEAR                   D20DFT
028200950111    2C                   ENDIF
028300930623     C*
028400950111     C                   MOVEL     'C'           D20TVL
028500950111     C                   MOVEL     ARBNCR        D20NCR
028600950111     C                   MOVEL     ARBVLC        D20VLU
028700950111     C                   MOVEL     TASFVF        D20FVF
028800950111     C                   MOVEL     TASVLF        D20VLF
028900950111     C                   MOVEL     TASFVB        D20FVB
029000950111     C                   MOVEL     TASVLB        D20VLB
029100950111     C                   MOVEL     TASAAS        D20AAS
029200950111     C                   MOVEL     TASLNP        D20LNP
029300950111     C                   MOVEL     TASNRS        D20NRS
029400950111     C                   MOVEL     TASNSP        D20NSP
029500950111     C                   MOVEL     ARBCBO        D20CBO
029600950111     C                   MOVEL     TASNCL        D20NCL
029700950111     C                   MOVEL     TASLNA        D20LNA
029800950111     C                   MOVEL     NUM3          D20MBR
029900960927     C                   MOVE      TASMGS        D20DTE
030000960927     C                   MOVEL     TASAAS        D20DTE
030100970904     C* SE IL VOLUME CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
030200970904     C*  COLLI RILEVATI, LI METTO UGUALI
030300970904     C     ARBFVC        IFEQ      'T'
030400970904     C     ARBNCR        ANDNE     TASNCL
030500970904     C                   Z-ADD     TASNCL        D20NCR
030600970904     C                   ENDIF
030700950111     C*
030800950111     C                   MOVEL     DSLV20        KPJBU
030900950111     C*
031000950111     C                   CALL      'FNLV20R'
031100950111     C                   PARM                    KPJBA
031200950111     C                   MOVEL     KPJBU         DSLV20
031300950111     C*
031400930623     C*
031500990702     C* SE ERRORE SCRIVO FILE ERRORI
031600950111    2C     D20ERR        IFEQ      '2'
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
033700990702     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV20R
033800950111     C                   MOVEL     D20NCR        TASNCR
033900950111     C                   MOVEL     D20VLU        TASVLC
034000950111     C                   MOVEL     D20FVF        TASFVF
034100950111     C                   MOVEL     D20VLF        TASVLF
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
038300950110     C                   OPEN      FNARBV0R
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
039600950111     C     KARB          CHAIN     FNARBV0E                           30
039700930806     C*
039800930706     C* SE NON TROVATO RECORD
039900930806IF  1C     *IN30         IFEQ      *ON
040000930806     C* O TROVATO MA NON E' QUELLE VARIAZIONE DI VOLUME
040100950111     C     RRBVLC        ORNE      ARBVLC
040200930806     C*
040300930806     C* RISCRIVO ERRORE
040400950111     C                   CLEAR                   FNARBV0E
040500950111     C                   Z-ADD     ARBAAS        RRBAAS
040600950111     C                   Z-ADD     ARBLNP        RRBLNP
040700950111     C                   Z-ADD     ARBNRS        RRBNRS
040800950111     C                   Z-ADD     ARBNSP        RRBNSP
040900950111     C                   MOVEL     ARBCBO        RRBCBO
041000950111     C                   MOVEL     ARBDTV        RRBDTV
041100950111     C                   MOVEL     ARBORV        RRBORV
041200950111     C                   MOVEL     ARBCVB        RRBCVB
041300950111     C                   Z-ADD     ARBVLC        RRBVLC
041400950111     C                   MOVEL     ARBFVC        RRBFVC
041500950111     C                   MOVEL     ARBNCR        RRBNCR
041600950111     C                   MOVEL     FEV           ARBFEV
041700950111     C                   MOVEL     DER           ARBDER
041800950111     C                   CLEAR                   ARBDAT
041900950111     C                   WRITE     FNARBV0E
042000930806E   1C                   ENDIF
042100930806     C*
042200930706     C                   ENDSR
042300930706     C*---------------------------------------------------------------*
042400950111     C*                    ANNARB                                     *
042500930806     C* ROUTINE DI ANNULLAMENTO ERRORE PERCHE' VARIAZIONE CARICATA    *
042600930706     C*---------------------------------------------------------------*
042700950111     C     ANNARB        BEGSR
042800930806     C*
042900950111     C     KARB2         SETLL     FNARBV0E
043000950111     C     KARB2         READE     FNARBV0E                               30
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
044200950111     C                   DELETE    FNARBV0E
044300950111E   4C                   ENDIF
044400930706     C*
044500950111X   3C                   ELSE
044600930706     C*
044700930706     C* SE E' "1 SPE INESISTENTE" DEVO AVERE TROVATO LA 1 BOLLA (05 ON)
044800950111IF  4C     ARBDER        IFEQ      ERR(1)
044900950111IF  5C     *IN05         IFEQ      *ON
045000950111     C                   DELETE    FNARBV0E
045100950111E   5C                   ENDIF
045200930706     C*
045300950111X   4C                   ELSE
045400930706     C*
045500930706     C* SE E' " CODICE BOLLA INESISTE ANNULLO SEMPRE
045600950111     C                   DELETE    FNARBV0E
045700950111E   4C                   ENDIF
045800941129E   3C                   ENDIF
045900941124E   2C                   ENDIF
046000930706     C*
046100950111     C     KARB2         READE     FNARBV0E                               30
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
047400070202     c                   exsr      CERBLP
047500070202     c
047600070202     c* Non trovata nemmeno in blp come merce mai affidata
047700070202   1ac                   if        wChiusainBLP=' '
047800960208     C                   SETON                                        08
047900960208     C*
048000960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
048100960208     C     ARBDTV        IFLE      DATAM
048200930806     C                   Z-ADD     102           FEV
048300930806     C                   MOVEL     ERR(4)        DER
048400950111     C                   EXSR      RIEARB
048500930806     C*
048600930806     C                   Z-ADD     102           FEV
048700930806     C                   MOVEL     ERR(1)        DER
048800950111     C                   EXSR      RIEARB
048900960208     C                   ENDIF
049000070202   1aC                   ENDIF
049100930806     C*
049200930806X   1C                   ELSE
049300930806     C*
049400930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
049500930806IF  2C     *IN01         IFEQ      *ON
049600930806     C     *IN02         OREQ      *ON
049700930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
049800930806IF  3C     *IN01         IFEQ      *ON
049900930806     C                   MOVEL     §3ATB1        ALF2
050000930806     C                   EXSR      RICBOL
050100930806     C*
050200930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
050300960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
050400930806IF  4C     TIBOL         IFEQ      *BLANKS
050500960208     C     ARBDTV        IFLE      DATAM
050600930806     C                   Z-ADD     102           FEV
050700930806     C                   MOVEL     ERR(4)        DER
050800950111     C                   EXSR      RIEARB
050900960208     C                   ENDIF
051000930806X   4C                   ELSE
051100930806     C                   MOVEL     TIBOL         §3ATB2
051200990702     C                   SETOFF                                       01
051300930806E   4C                   ENDIF
051400990702     C* RICHAINO LA PRIMA BOLLA
051500990702     C                   MOVEL     §3ATB1        TBL
051600990702     C                   SETOFF                                       111213
051700990702     C     KTAS          CHAIN     TITAS30C                           37
051800930806     C*
051900930806X   3C                   ELSE
052000930806     C* N O N   T R O V AT A   P R I M A   B O L L A
052100930806     C                   MOVEL     §3ATB2        ALF2              2
052200930806     C                   EXSR      RICBOL
052300930806     C*
052400930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
052500930806IF  4C     TIBOL         IFEQ      *BLANKS
052600070202     c* Se la bolla è chiusa in partenza con cca=/-merce mai affidadata
052700070202     c*  cancello senza dare erorre
052800070202     c                   exsr      CERBLP
052900070202     c
053000070202     c* Non trovata nemmeno in blp come merce mai affidata
053100070202    5c                   if        wChiusainBLP=' '
053200070202     c
053300960208     C                   SETON                                        08
053400960208     C     ARBDTV        IFLE      DATAM
053500930806     C                   Z-ADD     102           FEV
053600930806     C                   MOVEL     ERR(1)        DER
053700950111     C                   EXSR      RIEARB
053800960208     C                   ENDIF
053900070202E   5C                   ENDIF
054000070202     c
054100930806X   4C                   ELSE
054200941129     C   11              SETON                                        05
054300941129     C   12              SETON                                        06
054400941129     C   13              SETON                                        07
054500930806     C                   MOVEL     TIBOL         §3ATB1
054600070202E   4C                   ENDIF
054700930806E   3C                   ENDIF
054800930806E   2C                   ENDIF
054900930806E   1C                   ENDIF
055000930806     C*
055100930806     C                   ENDSR
055200070202     C*---------------------------------------------------------------*
055300070202     C*                    CERBLP                                     *
055400070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
055500070202     c* merce mai affdata
055600070202     C*---------------------------------------------------------------*
055700070202     C     CERBLP        BEGSR
055800070202     c                   clear                   wChiusainBLP      1
055900070202     c     ktas2         chain     fnblp01l
056000070202    5c                   if        %found(fnblp01l) and blpft1='N'
056100070302     c                   if        blpcca='7'
056200070202     c                   eval      wChiusainBLP='S'
056300070302     c                   else
056400070302     c* Verifico se si tratta di import
056500070302     c     blplnp        chain     azorg01l
056600070302If   c                   If        %Found(Azorg01l)
056700070302     c                   Movel     OrgDe3        og143
056800070302     c                   Else
056900070302     c                   Clear                   og143
057000070302     c                   EndIf
057100070302    5c                   if        §ogntw='EEX'  or §ogntw='DPD'
057200070302     c                   eval      wChiusainBLP='S'
057300070302     c                   EndIf
057400070302     C
057500070302     c                   EndIf
057600070302     c                   EndIf
057700070202     c                   ENDSR
057800930806     C*---------------------------------------------------------------*
057900930806     C*                    RICBOL                                     *
058000930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
058100930806     C*---------------------------------------------------------------*
058200930806     C     RICBOL        BEGSR
058300930806     C*
058400930806     C                   SETOFF                                       3940
058500941129     C                   SETOFF                                       111213
058600930806     C                   CLEAR                   TIBOL
058700930806     C*
058800990701     C     KTAS2         SETLL     TITAS30C
058900990701     C     KTAS2         READE     TITAS30C                               39
059000930806     C*
059100930806DO  1C     *IN39         DOWEQ     *OFF
059200930806     C*
059300930806IF  2C     TASTBL        IFNE      ALF2
059400930806     C                   SETON                                        39
059500930806     C                   MOVEL     TASTBL        TIBOL             2
059600930806E   2C                   ENDIF
059700930806     C*
059800990701     C  N39KTAS2         READE     TITAS30C                               39
059900930806E   1C                   ENDDO
060000930806     C*
060100930806     C*
060200930806     C                   ENDSR
060300930422     C*---------------------------------------------------------------*
060400930422     C*                    DEFVAR                                     *
060500930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
060600930422     C*---------------------------------------------------------------*
060700930422     C     DEFVAR        BEGSR
060800930422     C*
060900930422     C     *ENTRY        PLIST
061000930422     C                   PARM                    KPJBA
061100930422     C*
061200950111     C                   Z-ADD     1             CODUT             1 0
061300930806     C                   Z-ADD     0             DAT
061400930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
061500930423     C                   MOVEL     MBRFIL        ALF4              4
061600930423     C                   MOVE      ALF4          NUM3              3 0
061700960208     C* UDATE - 3
061800960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
061900960208     C                   TIME                    W0140            14 0
062000960208     C* UDATE IN GGMMAAAA
062100960208     C                   MOVE      W0140         WDTGIO            8 0
062200960208     C*
062300960208     C* UDATE IN AAAAMMGG
062400960208     C                   Z-ADD     WDTGIO        G02DAT
062500960208     C                   MOVEL     *BLANK        G02ERR
062600960208     C                   CALL      'XSRDA8'
062700960208     C                   PARM                    WLBDAT
062800960208     C                   MOVEL     G02INV        DATEU             8 0
062900960208     C* SOTTRAGGO  3
063000960208     C     G02TGI        SUB       3             GIOTGI
063100960208     C*
063200960208     C                   CALL      'XSRGI8'
063300960208     C                   PARM                    WGIDAT
063400960208     C*
063500960208     C                   Z-ADD     GIOINV        DATAM             8 0
063600070202     c* Apro file bolle di filiale
063700070202     c                   clear                   tibs55ds
063800070202     c                   eval      i50tla='L'
063900070202     c                   eval      i50apo=001
064000070202     c                   call      'TIBS55R'
064100070202     c                   parm                    tibs55ds
064200070202     c*
064300070202     c                   Clear                   commt
064400070202     c                   Movel(p)  cmdt(1)       commt
064500070202     c                   Eval      %Subst(commt:30:10) = o50ala
064600070202     c                   Eval      commt =
064700070202     c                             %trim(commt) + '/FNBLP01L)'
064800070202     c                   Call      'QCMDEXC'
064900070202     c                   Parm                    commt
065000070202     c                   Parm                    lenght
065100070202     c
065200070202     c                   Open      FNBLP01L
065300950111     C*
065400950111     C* DEFINIZIONE CAMPI
065500950111     C     *LIKE         DEFINE    ARBFEV        FEV
065600950111     C     *LIKE         DEFINE    ARBDER        DER
065700950111     C     *LIKE         DEFINE    ARBDAT        DAT
065800950111     C     *LIKE         DEFINE    TBLCOD        COD
065900950111     C     *LIKE         DEFINE    TBLKEY        KEY
066000930422     C*
066100950111     C* ACCESSO TABEL00F
066200930423     C     KTAB          KLIST
066300950111     C                   KFLD                    CODUT
066400950111     C                   KFLD                    COD
066500950111     C                   KFLD                    KEY
066600990701     C* ACCESSO TITAS30C
066700930426     C     KTAS          KLIST
066800950111     C                   KFLD                    ARBAAS
066900950111     C                   KFLD                    ARBLNP
067000950111     C                   KFLD                    ARBNRS
067100950111     C                   KFLD                    ARBNSP
067200930528     C                   KFLD                    TBL               2
067300990701     C* ACCESSO TITAS30C/TNBLA01L
067400930528     C     KTAS2         KLIST
067500950111     C                   KFLD                    ARBAAS
067600950111     C                   KFLD                    ARBLNP
067700950111     C                   KFLD                    ARBNRS
067800950111     C                   KFLD                    ARBNSP
067900991103     C* ACCESSO FIARBE2C
068000950111     C     KARB          KLIST
068100950111     C                   KFLD                    ARBAAS
068200950111     C                   KFLD                    ARBLNP
068300950111     C                   KFLD                    ARBNRS
068400950111     C                   KFLD                    ARBNSP
068500950111     C                   KFLD                    ARBDER
068600950111     C                   KFLD                    DAT
068700950111     C     KARB2         KLIST
068800950111     C                   KFLD                    ARBAAS
068900950111     C                   KFLD                    ARBLNP
069000950111     C                   KFLD                    ARBNRS
069100950111     C                   KFLD                    ARBNSP
069200930423     C*
069300930423     C                   ENDSR
069400930423     C*---------------------------------------------------------------*
069500930423     C*                    QCMDEXC                                    *
069600930423     C*---------------------------------------------------------------*
069700940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
069800930528**         CMD1
069900950110CHKOBJ OBJ(FNARBV0R) OBJTYPE(*FILE)     MBR(M234567890)
070000930423**         CMD2
070100950110ALCOBJ OBJ((FNARBV0R *FILE *EXCL           M234567890))
070200930423**         CMD3
070300950110OVRDBF FILE(FNARBV0R)                   MBR(M234567890)
070400930426**         ERR
0705009305281 SPEDIZIONE INESISTENTE
070600950222SPEDIZIONE GIA' TASSATA
070700930528CODICE BOLLA INESISTENTE
0708009305282 SPEDIZIONE INESISTENTE
070900070202** cmdt
071000070202OVRDBF FILE(FNBLP01L) TOFILE(
