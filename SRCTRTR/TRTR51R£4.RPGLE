000100990805     H* TRTR51R *-----------------------------------------------------*
000200990805     H*            - RICEZIONE VARIAZIONI PESO   IN SEDE -            *
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
001700990805     H* 33 - LETTURA FNARBP0R
001800990702     H* 38 - LETTURA DI TNBLA000
001900930806     H* 39/40 COMOD
002000930422     H*---------------------------------------------------------------*
002100930422     H* DATA BASE                                                     *
002200930422     H*---------------------------------------------------------------*
002300930422     H DECEDIT('0,') DATEDIT(*DMY.)
002400990805     FFNARBP0R  UF   E           K DISK    USROPN
002500941124     FTNBLA01L  IF   E           K DISK
002600990701     FTITAS30C  UF   E           K DISK
002700991028     FFIARBE2C  UF A E           K DISK
002800990805     F                                     RENAME(FNARBP00:FNARBP0E)
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
006700990805     D* DS PER FNLV39R - DS DEI CAMPI PER CALCOLO PESO   DA FATTURARE
006800990805     D DSLV39        E DS                  EXTNAME(FNLV39DS)
006900990702     D*
007000990702     D DSLV55        E DS                  EXTNAME(FNLV55DS)
007100070202     d commt           s             80
007200070202     d lenght          s             15  5 inz(80)
007300070202     i
007400990701     ITITAS000      11
007500990701     ITITAS010      12
007600990701     ITITASP00      13
007700941129     I*
007800990805     IFNARBP0E
007900950111     I              ARBAAS                      RRBAAS
008000950111     I              ARBLNP                      RRBLNP
008100950111     I              ARBNRS                      RRBNRS
008200950111     I              ARBNSP                      RRBNSP
008300950111     I              ARBCBO                      RRBCBO
008400950111     I              ARBDTV                      RRBDTV
008500950111     I              ARBORV                      RRBORV
008600950111     I              ARBCVB                      RRBCVB
008700990805     I              ARBPKC                      RRBPKC
008800991028     I              ARBFPC                      RRBFPC
008900990805     I              ARBNCP                      RRBNCP
009000950111     I*
009100950111£$   I*DS$SLD    E DS
009200930422     C*---------------------------------------------------------------*
009300930422     C*                    MAIN                                       *
009400990805     C* PROGRAMMA DI RICEZIONE IN SEDE VARIAZIONI PESO                *
009500930422     C*---------------------------------------------------------------*
009600930528     C*
009700930528     C* DEFINIZIONE VARIABILI, KLIST E PLIST
009800930528     C                   EXSR      DEFVAR
009900930528     C*
010000930528     C* APERTURA MEMBRO DI RICEZIONE
010100930528     C                   EXSR      CAEXA
010200930528     C*
010300930528     C* APERTURA RIUSCITA
010400930528IF  1C     *IN32         IFEQ      '0'
010500930423     C*
010600990805     C* LEGGO VARIAZIONE PESO
010700990805     C                   CLEAR                   FNARBP0E
010800990805     C                   READ      FNARBP0R                               33
010900930423DO  2C     *IN33         DOWEQ     '0'
011000930423     C*
011100930423     C* GESTIONE VARIAZIONI DA APPLICARE
011200930423     C                   EXSR      GESMOD
011300990805     C                   CLEAR                   FNARBP0E
011400990805     C                   READ      FNARBP0R                               33
011500930423E   2C                   ENDDO
011600930423     C*
011700930423     C* CHIUSURA MEMBRO DI RICEZIONE
011800990805     C                   CLOSE     FNARBP0R
011900930423E   1C                   ENDIF
012000990702     C*
012100990702     C                   MOVEL     'C'           D55TLA
012200990702     C                   CALL      'FNLV55R'
012300990702     C                   PARM                    DSLV55
012400930422     C*
012500930423     C                   SETON                                        LR
012600930423     C*---------------------------------------------------------------*
012700930423     C*                    GESMOD                                     *
012800990805     C* ROUTINE DI GESTIONE MODIFICHE PESO   DA APPLICARE             *
012900930423     C*---------------------------------------------------------------*
013000930423     C     GESMOD        BEGSR
013100930426     C*
013200930426     C                   SETOFF                                       050607
013300930528     C                   SETOFF                                       3108
013400930806     C                   SETOFF                                       0102
013500930806     C*
013600930806     C* LETTURA TIPO BOLLA
013700930528     C                   MOVEL     '3A'          COD
013800950111     C                   MOVEL(P)  ARBCBO        KEY
013900930528     C     KTAB          CHAIN     TABEL                              30
014000930528     C*
014100930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
014200930806IF  1C     *IN30         IFEQ      '1'
014300930806     C                   Z-ADD     102           FEV
014400930806     C                   MOVEL     ERR(3)        DER
014500950111     C                   EXSR      RIEARB
014600930528     C*
014700930806X   1C                   ELSE
014800930528     C*
014900930528     C                   MOVEL     TBLUNI        DS3A
015000950905     C* SE SI TRATTA DI UNA BOLLA NON TRASMESSA IN SEDE
015100950905     C* CANCELLO SENZA DARE ERRORE
015200950905   1AC     §3ABSD        IFEQ      ' '
015300930806     C*
015400930806     C* 2   B O L L A
015500930806     C*
015600930806IF  2C     §3ATB2        IFNE      '  '
015700930806     C                   MOVEL     §3ATB2        TBL
015800930528     C*
015900930426     C* CERCO BOLLA IN ARCHIVIO CORRENTE
016000941129     C                   SETOFF                                       111213
016100990701     C     KTAS          CHAIN     TITAS30C                           37
016200930426     C*
016300930426     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
016400941129     C   37KTAS2         CHAIN     TNBLA01L                           38
016500941129     C   37
016600941129     CAN 38              SETON                                        01
016700941129    2C                   ENDIF
016800930426     C*
016900930806     C* 1   B O L L A
017000930806     C*
017100930806     C                   MOVEL     §3ATB1        TBL
017200930528     C*
017300930528     C* CERCO BOLLA IN ARCHIVIO CORRENTE
017400941129     C                   SETOFF                                       111213
017500990701     C     KTAS          CHAIN     TITAS30C                           37
017600930528     C*
017700930528     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
017800941129IF  2C     *IN37         IFEQ      '1'
017900941129     C     KTAS2         CHAIN     TNBLA01L                           38
018000941129     C   38              SETON                                        02
018100930528     C*
018200941129X   2C                   ELSE
018300941129     C* BOLLA TROVATA
018400941129     C   11              SETON                                        05
018500941129     C   12              SETON                                        06
018600941129     C   13              SETON                                        07
018700941129E   2C                   ENDIF
018800930806     C*
018900930806     C* CONTROLLO SE MANCANO VERAMENTE O E' CAMBIATO IL TIPO BOLLA
019000930806     C   01
019100930806     COR 02              EXSR      MANBOL
019200930806     C*
019300930806     C* SE PRIMA BOLLA INESISTENTE ESCO E NON AGGIORNO
019400930806IF  2C     *IN08         IFEQ      '0'
019500070202     c
019600070202     c* Se chiusa in blp non stampo ma cancello in fnarbv0r
019700070202   2ac     wChiusainBLP  ifeq      ' '
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
023400070202E  2aC                   ENDIF
023500930528     C*
023600930528     C* CANCELLO SOLO VARIAZIONE TRASMESSA
023700990805     C                   DELETE    FNARBP0R
023800930806E   2C                   ENDIF
023900950905     C*
024000950905X  1AC                   ELSE
024100990805     C                   DELETE    FNARBP0R
024200950905E  1AC                   ENDIF
024300950905E   1C                   ENDIF
024400930806     C*
024500930528     C                   ENDSR
024600930528     C*---------------------------------------------------------------*
024700930528     C*                    AGGIOR                                     *
024800990805     C* ROUTINE DI AGGIORNAMENTO PESO   SULLA BOLLA                   *
024900930528     C*---------------------------------------------------------------*
025000930528     C     AGGIOR        BEGSR
025100930426     C*
025200990805     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO PESO   UGUALE A 'T'
025300970625    1C******     TASFVF    IFNE 'T'
025400930528     C*
025500930806     C                   MOVEL     TASTBL        BOL1              1
025600930806     C                   MOVE      TASTBL        BOL2              1
025700930426     C                   MOVE      TASKSC        CODCLI            4 0
025800930806     C*
025900990805     C                   CLEAR                   DSLV39
026000950111     C*
026100950111     C* SE E' LA PRIMA BOLLA
026200950111    2C     TBL           IFEQ      §3ATB1
026300950111     C*
026400950111     C* SE C/SERVIZIO O STORNO O ASSEGNATO 9999
026500950111    3C     BOL2          IFEQ      '7'
026600950111     C     BOL2          OREQ      'C'
026700950111     C     BOL2          OREQ      'S'
026800950111     C     BOL1          OREQ      'A'
026900950111     C     CODCLI        ANDEQ     9999
027000950111     C*
027100950111     C* NON PASSO LA DATA FATTURA
027200990805     C                   CLEAR                   D39DFT
027300950111     C*
027400950111   X3C                   ELSE
027500990805     C                   MOVEL     TASDFT        D39DFT
027600950111    3C                   ENDIF
027700930623     C*
027800950111   X2C                   ELSE
027900950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
028000990805     C                   CLEAR                   D39DFT
028100950111    2C                   ENDIF
028200930623     C*
028300990805     C                   MOVEL     'C'           D39TPP
028400990921     C                   MOVEL     ARBNCP        D39NCP
028500990805     C                   MOVEL     ARBPKC        D39PLU
028600990805     C                   MOVEL     TASFPF        D39FPF
028700990805     C                   MOVEL     TASPKF        D39PLF
028800990805     C                   MOVEL     TASFPB        D39FPB
028900990805     C                   MOVEL     TASPKB        D39PLB
029000990805     C                   MOVEL     TASAAS        D39AAS
029100990805     C                   MOVEL     TASLNP        D39LNP
029200990805     C                   MOVEL     TASNRS        D39NRS
029300990805     C                   MOVEL     TASNSP        D39NSP
029400990805     C                   MOVEL     ARBCBO        D39CBO
029500990805     C                   MOVEL     TASNCL        D39NCL
029600990805     C                   MOVEL     TASLNA        D39LNA
029700990805     C                   MOVEL     NUM3          D39MBR
029800990805     C                   MOVE      TASMGS        D39DTE
029900990805     C                   MOVEL     TASAAS        D39DTE
030000990805     C* SE IL PESO   CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
030100970904     C*  COLLI RILEVATI, LI METTO UGUALI
030200990805     C     ARBFPC        IFEQ      'T'
030300990805     C     ARBNCP        ANDNE     TASNCL
030400990805     C                   Z-ADD     TASNCL        D39NCP
030500970904     C                   ENDIF
030600950111     C*
030700990805     C                   MOVEL     DSLV39        KPJBU
030800950111     C*
030900990805     C                   CALL      'FNLV39R'
031000950111     C                   PARM                    KPJBA
031100990805     C                   MOVEL     KPJBU         DSLV39
031200950111     C*
031300930623     C*
031400990702     C* SE ERRORE SCRIVO FILE ERRORI
031500990805    2C     D39ERR        IFEQ      '2'
031600950111     C*
031700930623     C* FRANCO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNP
031800950111    3C     BOL1          IFEQ      'F'
031900990701     C                   Z-ADD     TASTFP        FEV
032000950111    3C                   ELSE
032100930623     C* ASSEGNATO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNA
032200990702     C                   CLEAR                   DSLV55
032300990702     C                   MOVEL     'P'           D55TPT
032400990702     C                   Z-ADD     TASLNA        D55LIN
032500990702     C                   Z-ADD     DATEU         D55DRF
032600990702     C                   CALL      'FNLV55R'
032700990702     C                   PARM                    DSLV55
032800990702     C                   Z-ADD     D55TFP        FEV
032900950111    3C                   ENDIF
033000950111     C                   MOVEL     ERR(2)        DER
033100930623     C*
033200930623     C* SCRIVO IL FILE DEGLI ERRORI
033300950111     C                   EXSR      RIEARB
033400950111    2C                   ENDIF
033500990702     C* AGGIORNO IN OGNI CASO I CAMPI DI TITAS (ANCHE SE C'E' ERRORE)
033600990805     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV39R
033700990805     C                   MOVEL     D39NCP        TASNCP
033800990805     C                   MOVEL     D39PLU        TASPKC
033900990805     C                   MOVEL     D39FPF        TASFPF
034000990805     C                   MOVEL     D39PLF        TASPKF
034100970625    1C***********          ENDIF
034200930426     C*
034300930423     C                   ENDSR
034400930423     C*---------------------------------------------------------------*
034500930423     C*                    CAEXA                                      *
034600930423     C* ROUTINE DI APERTURA MEMBRO FILE DI RICEZIONE                  *
034700930423     C*---------------------------------------------------------------*
034800930423     C     CAEXA         BEGSR
034900930423     C*
035000930423     C                   MOVEL     *BLANKS       VAR11            11
035100930423     C                   MOVEL     *BLANKS       VAR12            12
035200930423     C                   MOVE      ')'           VAR11
035300930423     C                   MOVEL     MBRFIL        VAR11
035400930423     C                   MOVE      '))'          VAR12
035500930423     C                   MOVEL     MBRFIL        VAR12
035600930423     C                   Z-ADD     55            LUNG             15 5
035700930423     C* CHKOBJ
035800930423     C                   MOVE      VAR11         CMD1
035900930423     C                   MOVEL     *BLANKS       COMMAN
036000930423     C                   MOVEA     CMD1(1)       COMMAN           80
036100930423     C                   CALL      'QCMDEXC'                            32
036200930423     C                   PARM                    COMMAN
036300930423     C                   PARM                    LUNG
036400930423IF  1C     *IN32         IFEQ      '0'
036500930423     C* ALCOBJ
036600930423     C                   MOVE      VAR12         CMD2
036700930423     C                   MOVEL     *BLANKS       COMMAN
036800930423     C                   MOVEA     CMD2(1)       COMMAN
036900930423     C                   CALL      'QCMDEXC'                            32
037000930423     C                   PARM                    COMMAN
037100930423     C                   PARM                    LUNG
037200930423IF  2C     *IN32         IFEQ      '0'
037300930423     C* OVRDBF
037400930423     C                   MOVE      VAR11         CMD3
037500930423     C                   MOVEL     *BLANKS       COMMAN
037600930423     C                   MOVEA     CMD3(1)       COMMAN
037700930423     C                   CALL      'QCMDEXC'                            32
037800930423     C                   PARM                    COMMAN
037900930423     C                   PARM                    LUNG
038000930423IF  3C     *IN32         IFEQ      '0'
038100930423     C* OPEN
038200990805     C                   OPEN      FNARBP0R
038300930423E   3C                   ENDIF
038400930423E   2C                   ENDIF
038500930423E   1C                   ENDIF
038600930423     C*
038700930423     C                   ENDSR
038800930706     C*---------------------------------------------------------------*
038900950111     C*                    RIEARB                                     *
039000930806     C* ROUTINE DI RIEMPIMENTO ARCHIVIO ERRORI                        *
039100930706     C*---------------------------------------------------------------*
039200950111     C     RIEARB        BEGSR
039300930806     C*
039400950111     C                   MOVEL     DER           ARBDER
039500990805     C     KARB          CHAIN     FNARBP0E                           30
039600930806     C*
039700930706     C* SE NON TROVATO RECORD
039800930806IF  1C     *IN30         IFEQ      *ON
039900990805     C* O TROVATO MA NON E' QUELLE VARIAZIONE DI PESO
040000990805     C     RRBPKC        ORNE      ARBPKC
040100930806     C*
040200930806     C* RISCRIVO ERRORE
040300990805     C                   CLEAR                   FNARBP0E
040400950111     C                   Z-ADD     ARBAAS        RRBAAS
040500950111     C                   Z-ADD     ARBLNP        RRBLNP
040600950111     C                   Z-ADD     ARBNRS        RRBNRS
040700950111     C                   Z-ADD     ARBNSP        RRBNSP
040800950111     C                   MOVEL     ARBCBO        RRBCBO
040900950111     C                   MOVEL     ARBDTV        RRBDTV
041000950111     C                   MOVEL     ARBORV        RRBORV
041100950111     C                   MOVEL     ARBCVB        RRBCVB
041200990805     C                   Z-ADD     ARBPKC        RRBPKC
041300991028     C                   MOVEL     ARBFPC        RRBFPC
041400990805     C                   MOVEL     ARBNCP        RRBNCP
041500950111     C                   MOVEL     FEV           ARBFEV
041600950111     C                   MOVEL     DER           ARBDER
041700950111     C                   CLEAR                   ARBDAT
041800990805     C                   WRITE     FNARBP0E
041900930806E   1C                   ENDIF
042000930806     C*
042100930706     C                   ENDSR
042200930706     C*---------------------------------------------------------------*
042300950111     C*                    ANNARB                                     *
042400930806     C* ROUTINE DI ANNULLAMENTO ERRORE PERCHE' VARIAZIONE CARICATA    *
042500930706     C*---------------------------------------------------------------*
042600950111     C     ANNARB        BEGSR
042700930806     C*
042800990805     C     KARB2         SETLL     FNARBP0E
042900990805     C     KARB2         READE     FNARBP0E                               30
043000930706     C*
043100930806DO  1C     *IN30         DOWEQ     *OFF
043200930706     C*
043300930706     C* NON DEVE ESSERE RECORD STAMPATO (DATA = 0)
043400930706     C*  E NON DEVE ESSERE GIA' FATTURATA CHE LA STAMPO COMUNQUE
043500950111IF  2C     ARBDAT        IFEQ      0
043600950111     C     ARBDER        ANDNE     ERR(2)
043700930706     C*
043800941129     C* SE E' "2 SPE INESISTENTE" DEVO AVERE TROVATO LA 2 BOLLA
043900950111IF  3C     ARBDER        IFEQ      ERR(4)
044000950111IF  4C     *IN01         IFEQ      *OFF
044100990805     C                   DELETE    FNARBP0E
044200950111E   4C                   ENDIF
044300930706     C*
044400950111X   3C                   ELSE
044500930706     C*
044600930706     C* SE E' "1 SPE INESISTENTE" DEVO AVERE TROVATO LA 1 BOLLA (05 ON)
044700950111IF  4C     ARBDER        IFEQ      ERR(1)
044800950111IF  5C     *IN05         IFEQ      *ON
044900990805     C                   DELETE    FNARBP0E
045000950111E   5C                   ENDIF
045100930706     C*
045200950111X   4C                   ELSE
045300930706     C*
045400930706     C* SE E' " CODICE BOLLA INESISTE ANNULLO SEMPRE
045500990805     C                   DELETE    FNARBP0E
045600950111E   4C                   ENDIF
045700941129E   3C                   ENDIF
045800941124E   2C                   ENDIF
045900930706     C*
046000990805     C     KARB2         READE     FNARBP0E                               30
046100930806E   1C                   ENDDO
046200930706     C*
046300930706     C                   ENDSR
046400930806     C*---------------------------------------------------------------*
046500930806     C*                    MANBOL                                     *
046600930806     C* ROUTINE DI CONTROLLO SE MANCA LA BOLLA O E' CAMBIATO IL TIPO  *
046700930806     C*---------------------------------------------------------------*
046800930806     C     MANBOL        BEGSR
046900930806     C*
047000930806     C* 01 02 ON - MANCANO ENTRAMBE SCRIVO FILE DEGLI ERRORI
047100930806IF  1C     *IN01         IFEQ      *ON
047200930806     C     *IN02         ANDEQ     *ON
047300070202     c                   exsr      CERBLP
047400070202     c
047500070202     c* Non trovata nemmeno in blp come merce mai affidata
047600070202   1ac                   if        wChiusainBLP=' '
047700960208     C                   SETON                                        08
047800960208     C*
047900960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
048000960208     C     ARBDTV        IFLE      DATAM
048100930806     C                   Z-ADD     102           FEV
048200930806     C                   MOVEL     ERR(4)        DER
048300950111     C                   EXSR      RIEARB
048400930806     C*
048500930806     C                   Z-ADD     102           FEV
048600930806     C                   MOVEL     ERR(1)        DER
048700950111     C                   EXSR      RIEARB
048800960208     C                   ENDIF
048900070202   1aC                   ENDIF
049000930806     C*
049100930806X   1C                   ELSE
049200930806     C*
049300930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
049400930806IF  2C     *IN01         IFEQ      *ON
049500930806     C     *IN02         OREQ      *ON
049600930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
049700930806IF  3C     *IN01         IFEQ      *ON
049800930806     C                   MOVEL     §3ATB1        ALF2
049900930806     C                   EXSR      RICBOL
050000930806     C*
050100930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
050200960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
050300930806IF  4C     TIBOL         IFEQ      *BLANKS
050400960208     C     ARBDTV        IFLE      DATAM
050500930806     C                   Z-ADD     102           FEV
050600930806     C                   MOVEL     ERR(4)        DER
050700950111     C                   EXSR      RIEARB
050800960208     C                   ENDIF
050900930806X   4C                   ELSE
051000930806     C                   MOVEL     TIBOL         §3ATB2
051100990702     C                   SETOFF                                       01
051200930806E   4C                   ENDIF
051300990805     C* RICHIAMO LA PRIMA BOLLA
051400990702     C                   MOVEL     §3ATB1        TBL
051500990702     C                   SETOFF                                       111213
051600990702     C     KTAS          CHAIN     TITAS30C                           37
051700930806     C*
051800930806X   3C                   ELSE
051900990805     C* N O N   T R O V A T A   P R I M A   B O L L A
052000930806     C                   MOVEL     §3ATB2        ALF2              2
052100930806     C                   EXSR      RICBOL
052200930806     C*
052300930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
052400930806IF  4C     TIBOL         IFEQ      *BLANKS
052500070202     c* Se la bolla è chiusa in partenza con cca=/-merce mai affidadata
052600070202     c*  cancello senza dare erorre
052700070202     c                   exsr      CERBLP
052800070202     c
052900070202     c* Non trovata nemmeno in blp come merce mai affidata
053000070202    5c                   if        wChiusainBLP=' '
053100070202     c
053200960208     C                   SETON                                        08
053300960208     C     ARBDTV        IFLE      DATAM
053400930806     C                   Z-ADD     102           FEV
053500930806     C                   MOVEL     ERR(1)        DER
053600950111     C                   EXSR      RIEARB
053700960208     C                   ENDIF
053800070202    5C                   ENDIF
053900070202     c
054000930806X   4C                   ELSE
054100941129     C   11              SETON                                        05
054200941129     C   12              SETON                                        06
054300941129     C   13              SETON                                        07
054400930806     C                   MOVEL     TIBOL         §3ATB1
054500930806E   4C                   ENDIF
054600930806E   3C                   ENDIF
054700930806E   2C                   ENDIF
054800930806E   1C                   ENDIF
054900930806     C*
055000930806     C                   ENDSR
055100930806     C*---------------------------------------------------------------*
055200930806     C*                    RICBOL                                     *
055300930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
055400930806     C*---------------------------------------------------------------*
055500930806     C     RICBOL        BEGSR
055600930806     C*
055700930806     C                   SETOFF                                       3940
055800941129     C                   SETOFF                                       111213
055900930806     C                   CLEAR                   TIBOL
056000930806     C*
056100990701     C     KTAS2         SETLL     TITAS30C
056200990701     C     KTAS2         READE     TITAS30C                               39
056300930806     C*
056400930806DO  1C     *IN39         DOWEQ     *OFF
056500930806     C*
056600930806IF  2C     TASTBL        IFNE      ALF2
056700930806     C                   SETON                                        39
056800930806     C                   MOVEL     TASTBL        TIBOL             2
056900930806E   2C                   ENDIF
057000930806     C*
057100990701     C  N39KTAS2         READE     TITAS30C                               39
057200930806E   1C                   ENDDO
057300930806     C*
057400930806     C                   ENDSR
057500070202     C*---------------------------------------------------------------*
057600070202     C*                    CERBLP                                     *
057700070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
057800070202     c* merce mai affdata
057900070202     C*---------------------------------------------------------------*
058000070202     C     CERBLP        BEGSR
058100070202     c                   clear                   wChiusainBLP      1
058200070202     c     ktas2         chain     fnblp01l
058300070202    5c                   if        %found(fnblp01l) and blpft1='N'
058400070302     c                   if        blpcca='7'
058500070202     c                   eval      wChiusainBLP='S'
058600070302     c                   else
058700070302     c* Verifico se si tratta di import
058800070302     c     blplnp        chain     azorg01l
058900070302If   c                   If        %Found(Azorg01l)
059000070302     c                   Movel     OrgDe3        og143
059100070302     c                   Else
059200070302     c                   Clear                   og143
059300070302     c                   EndIf
059400070302    5c                   if        §ogntw='EEX'  or §ogntw='DPD'
059500070302     c                   eval      wChiusainBLP='S'
059600070302     c                   EndIf
059700070302     C
059800070302     c                   EndIf
059900070302     c                   EndIf
060000070202     c                   ENDSR
060100930422     C*---------------------------------------------------------------*
060200930422     C*                    DEFVAR                                     *
060300930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
060400930422     C*---------------------------------------------------------------*
060500930422     C     DEFVAR        BEGSR
060600930422     C*
060700930422     C     *ENTRY        PLIST
060800930422     C                   PARM                    KPJBA
060900930422     C*
061000950111     C                   Z-ADD     1             CODUT             1 0
061100930806     C                   Z-ADD     0             DAT
061200930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
061300930423     C                   MOVEL     MBRFIL        ALF4              4
061400930423     C                   MOVE      ALF4          NUM3              3 0
061500960208     C* UDATE - 3
061600960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
061700960208     C                   TIME                    W0140            14 0
061800960208     C* UDATE IN GGMMAAAA
061900960208     C                   MOVE      W0140         WDTGIO            8 0
062000960208     C*
062100960208     C* UDATE IN AAAAMMGG
062200960208     C                   Z-ADD     WDTGIO        G02DAT
062300960208     C                   MOVEL     *BLANK        G02ERR
062400960208     C                   CALL      'XSRDA8'
062500960208     C                   PARM                    WLBDAT
062600960208     C                   MOVEL     G02INV        DATEU             8 0
062700960208     C* SOTTRAGGO  3
062800960208     C     G02TGI        SUB       3             GIOTGI
062900960208     C*
063000960208     C                   CALL      'XSRGI8'
063100960208     C                   PARM                    WGIDAT
063200960208     C*
063300960208     C                   Z-ADD     GIOINV        DATAM             8 0
063400070202     c
063500070202     c* Apro file bolle di filiale
063600070202     c                   clear                   tibs55ds
063700070202     c                   eval      i50tla='L'
063800070202     c                   eval      i50apo=001
063900070202     c                   call      'TIBS55R'
064000070202     c                   parm                    tibs55ds
064100070202     c*
064200070202     c                   Clear                   commt
064300070202     c                   Movel(p)  cmdt(1)       commt
064400070202     c                   Eval      %Subst(commt:30:10) = o50ala
064500070202     c                   Eval      commt =
064600070202     c                             %trim(commt) + '/FNBLP01L)'
064700070202     c                   Call      'QCMDEXC'
064800070202     c                   Parm                    commt
064900070202     c                   Parm                    lenght
065000070202     c
065100070202     c                   Open      FNBLP01L
065200950111     C*
065300950111     C* DEFINIZIONE CAMPI
065400950111     C     *LIKE         DEFINE    ARBFEV        FEV
065500950111     C     *LIKE         DEFINE    ARBDER        DER
065600950111     C     *LIKE         DEFINE    ARBDAT        DAT
065700950111     C     *LIKE         DEFINE    TBLCOD        COD
065800950111     C     *LIKE         DEFINE    TBLKEY        KEY
065900930422     C*
066000950111     C* ACCESSO TABEL00F
066100930423     C     KTAB          KLIST
066200950111     C                   KFLD                    CODUT
066300950111     C                   KFLD                    COD
066400950111     C                   KFLD                    KEY
066500990701     C* ACCESSO TITAS30C
066600930426     C     KTAS          KLIST
066700950111     C                   KFLD                    ARBAAS
066800950111     C                   KFLD                    ARBLNP
066900950111     C                   KFLD                    ARBNRS
067000950111     C                   KFLD                    ARBNSP
067100930528     C                   KFLD                    TBL               2
067200990701     C* ACCESSO TITAS30C/TNBLA01L
067300930528     C     KTAS2         KLIST
067400950111     C                   KFLD                    ARBAAS
067500950111     C                   KFLD                    ARBLNP
067600950111     C                   KFLD                    ARBNRS
067700950111     C                   KFLD                    ARBNSP
067800991028     C* ACCESSO FIARBE2C
067900950111     C     KARB          KLIST
068000950111     C                   KFLD                    ARBAAS
068100950111     C                   KFLD                    ARBLNP
068200950111     C                   KFLD                    ARBNRS
068300950111     C                   KFLD                    ARBNSP
068400950111     C                   KFLD                    ARBDER
068500950111     C                   KFLD                    DAT
068600950111     C     KARB2         KLIST
068700950111     C                   KFLD                    ARBAAS
068800950111     C                   KFLD                    ARBLNP
068900950111     C                   KFLD                    ARBNRS
069000950111     C                   KFLD                    ARBNSP
069100930423     C*
069200930423     C                   ENDSR
069300930423     C*---------------------------------------------------------------*
069400930423     C*                    QCMDEXC                                    *
069500930423     C*---------------------------------------------------------------*
069600940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
069700930528**         CMD1
069800990805CHKOBJ OBJ(FNARBP0R) OBJTYPE(*FILE)     MBR(M234567890)
069900930423**         CMD2
070000990805ALCOBJ OBJ((FNARBP0R *FILE *EXCL           M234567890))
070100930423**         CMD3
070200990805OVRDBF FILE(FNARBP0R)                   MBR(M234567890)
070300930426**         ERR
0704009305281 SPEDIZIONE INESISTENTE
070500950222SPEDIZIONE GIA' TASSATA
070600930528CODICE BOLLA INESISTENTE
0707009305282 SPEDIZIONE INESISTENTE
070800070202** cmdt
070900070202OVRDBF FILE(FNBLP01L) TOFILE(
