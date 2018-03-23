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
006500990805     D* DS PER FNLV39R - DS DEI CAMPI PER CALCOLO PESO   DA FATTURARE
006600990805     D DSLV39        E DS                  EXTNAME(FNLV39DS)
006700990702     D*
006800990702     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006900070202     d commt           s             80
007000070202     d lenght          s             15  5 inz(80)
007100070202     i
007200990701     ITITAS000      11
007300990701     ITITAS010      12
007400990701     ITITASP00      13
007500941129     I*
007600990805     IFNARBP0E
007700950111     I              ARBAAS                      RRBAAS
007800950111     I              ARBLNP                      RRBLNP
007900950111     I              ARBNRS                      RRBNRS
008000950111     I              ARBNSP                      RRBNSP
008100950111     I              ARBCBO                      RRBCBO
008200950111     I              ARBDTV                      RRBDTV
008300950111     I              ARBORV                      RRBORV
008400950111     I              ARBCVB                      RRBCVB
008500990805     I              ARBPKC                      RRBPKC
008600991028     I              ARBFPC                      RRBFPC
008700990805     I              ARBNCP                      RRBNCP
008800950111     I*
008900950111£$   I*DS$SLD    E DS
009000930422     C*---------------------------------------------------------------*
009100930422     C*                    MAIN                                       *
009200990805     C* PROGRAMMA DI RICEZIONE IN SEDE VARIAZIONI PESO                *
009300930422     C*---------------------------------------------------------------*
009400930528     C*
009500930528     C* DEFINIZIONE VARIABILI, KLIST E PLIST
009600930528     C                   EXSR      DEFVAR
009700930528     C*
009800930528     C* APERTURA MEMBRO DI RICEZIONE
009900930528     C                   EXSR      CAEXA
010000930528     C*
010100930528     C* APERTURA RIUSCITA
010200930528IF  1C     *IN32         IFEQ      '0'
010300930423     C*
010400990805     C* LEGGO VARIAZIONE PESO
010500990805     C                   CLEAR                   FNARBP0E
010600990805     C                   READ      FNARBP0R                               33
010700930423DO  2C     *IN33         DOWEQ     '0'
010800930423     C*
010900930423     C* GESTIONE VARIAZIONI DA APPLICARE
011000930423     C                   EXSR      GESMOD
011100990805     C                   CLEAR                   FNARBP0E
011200990805     C                   READ      FNARBP0R                               33
011300930423E   2C                   ENDDO
011400930423     C*
011500930423     C* CHIUSURA MEMBRO DI RICEZIONE
011600990805     C                   CLOSE     FNARBP0R
011700930423E   1C                   ENDIF
011800990702     C*
011900990702     C                   MOVEL     'C'           D55TLA
012000990702     C                   CALL      'FNLV55R'
012100990702     C                   PARM                    DSLV55
012200930422     C*
012300930423     C                   SETON                                        LR
012400930423     C*---------------------------------------------------------------*
012500930423     C*                    GESMOD                                     *
012600990805     C* ROUTINE DI GESTIONE MODIFICHE PESO   DA APPLICARE             *
012700930423     C*---------------------------------------------------------------*
012800930423     C     GESMOD        BEGSR
012900930426     C*
013000930426     C                   SETOFF                                       050607
013100930528     C                   SETOFF                                       3108
013200930806     C                   SETOFF                                       0102
013300930806     C*
013400930806     C* LETTURA TIPO BOLLA
013500930528     C                   MOVEL     '3A'          COD
013600950111     C                   MOVEL(P)  ARBCBO        KEY
013700930528     C     KTAB          CHAIN     TABEL                              30
013800930528     C*
013900930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
014000930806IF  1C     *IN30         IFEQ      '1'
014100930806     C                   Z-ADD     102           FEV
014200930806     C                   MOVEL     ERR(3)        DER
014300950111     C                   EXSR      RIEARB
014400930528     C*
014500930806X   1C                   ELSE
014600930528     C*
014700930528     C                   MOVEL     TBLUNI        DS3A
014800950905     C* SE SI TRATTA DI UNA BOLLA NON TRASMESSA IN SEDE
014900950905     C* CANCELLO SENZA DARE ERRORE
015000950905   1AC     §3ABSD        IFEQ      ' '
015100930806     C*
015200930806     C* 2   B O L L A
015300930806     C*
015400930806IF  2C     §3ATB2        IFNE      '  '
015500930806     C                   MOVEL     §3ATB2        TBL
015600930528     C*
015700930426     C* CERCO BOLLA IN ARCHIVIO CORRENTE
015800941129     C                   SETOFF                                       111213
015900990701     C     KTAS          CHAIN     TITAS30C                           37
016000930426     C*
016100930426     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
016200941129     C   37KTAS2         CHAIN     TNBLA01L                           38
016300941129     C   37
016400941129     CAN 38              SETON                                        01
016500941129    2C                   ENDIF
016600930426     C*
016700930806     C* 1   B O L L A
016800930806     C*
016900930806     C                   MOVEL     §3ATB1        TBL
017000930528     C*
017100930528     C* CERCO BOLLA IN ARCHIVIO CORRENTE
017200941129     C                   SETOFF                                       111213
017300990701     C     KTAS          CHAIN     TITAS30C                           37
017400930528     C*
017500930528     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
017600941129IF  2C     *IN37         IFEQ      '1'
017700941129     C     KTAS2         CHAIN     TNBLA01L                           38
017800941129     C   38              SETON                                        02
017900930528     C*
018000941129X   2C                   ELSE
018100941129     C* BOLLA TROVATA
018200941129     C   11              SETON                                        05
018300941129     C   12              SETON                                        06
018400941129     C   13              SETON                                        07
018500941129E   2C                   ENDIF
018600930806     C*
018700930806     C* CONTROLLO SE MANCANO VERAMENTE O E' CAMBIATO IL TIPO BOLLA
018800930806     C   01
018900930806     COR 02              EXSR      MANBOL
019000930806     C*
019100930806     C* SE PRIMA BOLLA INESISTENTE ESCO E NON AGGIORNO
019200930806IF  2C     *IN08         IFEQ      '0'
019300070202     c
019400070202     c* Se chiusa in blp non stampo ma cancello in fnarbv0r
019500070202   2ac     wChiusainBLP  ifeq      ' '
019600930806     C*
019700930528     C* 1 BOLLA TROVATA IN ARCHIVIO (CORRENTE, STORICO O PRECEDENTE)
019800930528IF  3C     *IN05         IFEQ      '1'
019900930427     C     *IN06         OREQ      '1'
020000930427     C     *IN07         OREQ      '1'
020100930528     C                   EXSR      AGGIOR
020200930528     C*
020300930806IF  4C     *IN05         IFEQ      *ON
020400930806     C*
020500930706     C* SE ESISTE ERRORE NON STAMPATO CANCELLO
020600950111     C                   EXSR      ANNARB
020700930706     C*
020800990701     C                   UPDATE    TITAS000
020900930806E   4C                   ENDIF
021000930706     C*
021100990701     C   06              UPDATE    TITAS010
021200940208     C*
021300990701     C   07              UPDATE    TITASP00
021400930806E   3C                   ENDIF
021500930806     C*
021600941129     C* 2 BOLLA TROVATA IN ARCHIVIO
021700941129IF  3C     §3ATB2        IFNE      *BLANKS
021800941129     C     *IN01         ANDEQ     *OFF
021900941129     C*
022000930528     C                   MOVEL     §3ATB2        TBL
022100941129     C                   SETOFF                                       111213
022200990701     C     KTAS          CHAIN     TITAS30C                           30
022300930528     C*
022400941129    4C     *IN30         IFEQ      *OFF
022500930528     C                   EXSR      AGGIOR
022600930528     C*
022700990701     C   11              UPDATE    TITAS000
022800990701     C   12              UPDATE    TITAS010
022900990701     C   13              UPDATE    TITASP00
023000941129E   4C                   ENDIF
023100941129E   3C                   ENDIF
023200070202E  2aC                   ENDIF
023300930528     C*
023400930528     C* CANCELLO SOLO VARIAZIONE TRASMESSA
023500990805     C                   DELETE    FNARBP0R
023600930806E   2C                   ENDIF
023700950905     C*
023800950905X  1AC                   ELSE
023900990805     C                   DELETE    FNARBP0R
024000950905E  1AC                   ENDIF
024100950905E   1C                   ENDIF
024200930806     C*
024300930528     C                   ENDSR
024400930528     C*---------------------------------------------------------------*
024500930528     C*                    AGGIOR                                     *
024600990805     C* ROUTINE DI AGGIORNAMENTO PESO   SULLA BOLLA                   *
024700930528     C*---------------------------------------------------------------*
024800930528     C     AGGIOR        BEGSR
024900930426     C*
025000990805     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO PESO   UGUALE A 'T'
025100970625    1C******     TASFVF    IFNE 'T'
025200930528     C*
025300930806     C                   MOVEL     TASTBL        BOL1              1
025400930806     C                   MOVE      TASTBL        BOL2              1
025500930426     C                   MOVE      TASKSC        CODCLI            4 0
025600930806     C*
025700990805     C                   CLEAR                   DSLV39
025800950111     C*
025900950111     C* SE E' LA PRIMA BOLLA
026000950111    2C     TBL           IFEQ      §3ATB1
026100950111     C*
026200950111     C* SE C/SERVIZIO O STORNO O ASSEGNATO 9999
026300950111    3C     BOL2          IFEQ      '7'
026400950111     C     BOL2          OREQ      'C'
026500950111     C     BOL2          OREQ      'S'
026600950111     C     BOL1          OREQ      'A'
026700950111     C     CODCLI        ANDEQ     9999
026800950111     C*
026900950111     C* NON PASSO LA DATA FATTURA
027000990805     C                   CLEAR                   D39DFT
027100950111     C*
027200950111   X3C                   ELSE
027300990805     C                   MOVEL     TASDFT        D39DFT
027400950111    3C                   ENDIF
027500930623     C*
027600950111   X2C                   ELSE
027700950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
027800990805     C                   CLEAR                   D39DFT
027900950111    2C                   ENDIF
028000930623     C*
028100990805     C                   MOVEL     'C'           D39TPP
028200990921     C                   MOVEL     ARBNCP        D39NCP
028300990805     C                   MOVEL     ARBPKC        D39PLU
028400990805     C                   MOVEL     TASFPF        D39FPF
028500990805     C                   MOVEL     TASPKF        D39PLF
028600990805     C                   MOVEL     TASFPB        D39FPB
028700990805     C                   MOVEL     TASPKB        D39PLB
028800990805     C                   MOVEL     TASAAS        D39AAS
028900990805     C                   MOVEL     TASLNP        D39LNP
029000990805     C                   MOVEL     TASNRS        D39NRS
029100990805     C                   MOVEL     TASNSP        D39NSP
029200990805     C                   MOVEL     ARBCBO        D39CBO
029300990805     C                   MOVEL     TASNCL        D39NCL
029400990805     C                   MOVEL     TASLNA        D39LNA
029500990805     C                   MOVEL     NUM3          D39MBR
029600990805     C                   MOVE      TASMGS        D39DTE
029700990805     C                   MOVEL     TASAAS        D39DTE
029800990805     C* SE IL PESO   CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
029900970904     C*  COLLI RILEVATI, LI METTO UGUALI
030000990805     C     ARBFPC        IFEQ      'T'
030100990805     C     ARBNCP        ANDNE     TASNCL
030200990805     C                   Z-ADD     TASNCL        D39NCP
030300970904     C                   ENDIF
030400950111     C*
030500990805     C                   MOVEL     DSLV39        KPJBU
030600950111     C*
030700990805     C                   CALL      'FNLV39R'
030800950111     C                   PARM                    KPJBA
030900990805     C                   MOVEL     KPJBU         DSLV39
031000950111     C*
031100930623     C*
031200990702     C* SE ERRORE SCRIVO FILE ERRORI
031300990805    2C     D39ERR        IFEQ      '2'
031400950111     C*
031500930623     C* FRANCO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNP
031600950111    3C     BOL1          IFEQ      'F'
031700990701     C                   Z-ADD     TASTFP        FEV
031800950111    3C                   ELSE
031900930623     C* ASSEGNATO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNA
032000990702     C                   CLEAR                   DSLV55
032100990702     C                   MOVEL     'P'           D55TPT
032200990702     C                   Z-ADD     TASLNA        D55LIN
032300990702     C                   Z-ADD     DATEU         D55DRF
032400990702     C                   CALL      'FNLV55R'
032500990702     C                   PARM                    DSLV55
032600990702     C                   Z-ADD     D55TFP        FEV
032700950111    3C                   ENDIF
032800950111     C                   MOVEL     ERR(2)        DER
032900930623     C*
033000930623     C* SCRIVO IL FILE DEGLI ERRORI
033100950111     C                   EXSR      RIEARB
033200950111    2C                   ENDIF
033300990702     C* AGGIORNO IN OGNI CASO I CAMPI DI TITAS (ANCHE SE C'E' ERRORE)
033400990805     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV39R
033500990805     C                   MOVEL     D39NCP        TASNCP
033600990805     C                   MOVEL     D39PLU        TASPKC
033700990805     C                   MOVEL     D39FPF        TASFPF
033800990805     C                   MOVEL     D39PLF        TASPKF
033900970625    1C***********          ENDIF
034000930426     C*
034100930423     C                   ENDSR
034200930423     C*---------------------------------------------------------------*
034300930423     C*                    CAEXA                                      *
034400930423     C* ROUTINE DI APERTURA MEMBRO FILE DI RICEZIONE                  *
034500930423     C*---------------------------------------------------------------*
034600930423     C     CAEXA         BEGSR
034700930423     C*
034800930423     C                   MOVEL     *BLANKS       VAR11            11
034900930423     C                   MOVEL     *BLANKS       VAR12            12
035000930423     C                   MOVE      ')'           VAR11
035100930423     C                   MOVEL     MBRFIL        VAR11
035200930423     C                   MOVE      '))'          VAR12
035300930423     C                   MOVEL     MBRFIL        VAR12
035400930423     C                   Z-ADD     55            LUNG             15 5
035500930423     C* CHKOBJ
035600930423     C                   MOVE      VAR11         CMD1
035700930423     C                   MOVEL     *BLANKS       COMMAN
035800930423     C                   MOVEA     CMD1(1)       COMMAN           80
035900930423     C                   CALL      'QCMDEXC'                            32
036000930423     C                   PARM                    COMMAN
036100930423     C                   PARM                    LUNG
036200930423IF  1C     *IN32         IFEQ      '0'
036300930423     C* ALCOBJ
036400930423     C                   MOVE      VAR12         CMD2
036500930423     C                   MOVEL     *BLANKS       COMMAN
036600930423     C                   MOVEA     CMD2(1)       COMMAN
036700930423     C                   CALL      'QCMDEXC'                            32
036800930423     C                   PARM                    COMMAN
036900930423     C                   PARM                    LUNG
037000930423IF  2C     *IN32         IFEQ      '0'
037100930423     C* OVRDBF
037200930423     C                   MOVE      VAR11         CMD3
037300930423     C                   MOVEL     *BLANKS       COMMAN
037400930423     C                   MOVEA     CMD3(1)       COMMAN
037500930423     C                   CALL      'QCMDEXC'                            32
037600930423     C                   PARM                    COMMAN
037700930423     C                   PARM                    LUNG
037800930423IF  3C     *IN32         IFEQ      '0'
037900930423     C* OPEN
038000990805     C                   OPEN      FNARBP0R
038100930423E   3C                   ENDIF
038200930423E   2C                   ENDIF
038300930423E   1C                   ENDIF
038400930423     C*
038500930423     C                   ENDSR
038600930706     C*---------------------------------------------------------------*
038700950111     C*                    RIEARB                                     *
038800930806     C* ROUTINE DI RIEMPIMENTO ARCHIVIO ERRORI                        *
038900930706     C*---------------------------------------------------------------*
039000950111     C     RIEARB        BEGSR
039100930806     C*
039200950111     C                   MOVEL     DER           ARBDER
039300990805     C     KARB          CHAIN     FNARBP0E                           30
039400930806     C*
039500930706     C* SE NON TROVATO RECORD
039600930806IF  1C     *IN30         IFEQ      *ON
039700990805     C* O TROVATO MA NON E' QUELLE VARIAZIONE DI PESO
039800990805     C     RRBPKC        ORNE      ARBPKC
039900930806     C*
040000930806     C* RISCRIVO ERRORE
040100990805     C                   CLEAR                   FNARBP0E
040200950111     C                   Z-ADD     ARBAAS        RRBAAS
040300950111     C                   Z-ADD     ARBLNP        RRBLNP
040400950111     C                   Z-ADD     ARBNRS        RRBNRS
040500950111     C                   Z-ADD     ARBNSP        RRBNSP
040600950111     C                   MOVEL     ARBCBO        RRBCBO
040700950111     C                   MOVEL     ARBDTV        RRBDTV
040800950111     C                   MOVEL     ARBORV        RRBORV
040900950111     C                   MOVEL     ARBCVB        RRBCVB
041000990805     C                   Z-ADD     ARBPKC        RRBPKC
041100991028     C                   MOVEL     ARBFPC        RRBFPC
041200990805     C                   MOVEL     ARBNCP        RRBNCP
041300950111     C                   MOVEL     FEV           ARBFEV
041400950111     C                   MOVEL     DER           ARBDER
041500950111     C                   CLEAR                   ARBDAT
041600990805     C                   WRITE     FNARBP0E
041700930806E   1C                   ENDIF
041800930806     C*
041900930706     C                   ENDSR
042000930706     C*---------------------------------------------------------------*
042100950111     C*                    ANNARB                                     *
042200930806     C* ROUTINE DI ANNULLAMENTO ERRORE PERCHE' VARIAZIONE CARICATA    *
042300930706     C*---------------------------------------------------------------*
042400950111     C     ANNARB        BEGSR
042500930806     C*
042600990805     C     KARB2         SETLL     FNARBP0E
042700990805     C     KARB2         READE     FNARBP0E                               30
042800930706     C*
042900930806DO  1C     *IN30         DOWEQ     *OFF
043000930706     C*
043100930706     C* NON DEVE ESSERE RECORD STAMPATO (DATA = 0)
043200930706     C*  E NON DEVE ESSERE GIA' FATTURATA CHE LA STAMPO COMUNQUE
043300950111IF  2C     ARBDAT        IFEQ      0
043400950111     C     ARBDER        ANDNE     ERR(2)
043500930706     C*
043600941129     C* SE E' "2 SPE INESISTENTE" DEVO AVERE TROVATO LA 2 BOLLA
043700950111IF  3C     ARBDER        IFEQ      ERR(4)
043800950111IF  4C     *IN01         IFEQ      *OFF
043900990805     C                   DELETE    FNARBP0E
044000950111E   4C                   ENDIF
044100930706     C*
044200950111X   3C                   ELSE
044300930706     C*
044400930706     C* SE E' "1 SPE INESISTENTE" DEVO AVERE TROVATO LA 1 BOLLA (05 ON)
044500950111IF  4C     ARBDER        IFEQ      ERR(1)
044600950111IF  5C     *IN05         IFEQ      *ON
044700990805     C                   DELETE    FNARBP0E
044800950111E   5C                   ENDIF
044900930706     C*
045000950111X   4C                   ELSE
045100930706     C*
045200930706     C* SE E' " CODICE BOLLA INESISTE ANNULLO SEMPRE
045300990805     C                   DELETE    FNARBP0E
045400950111E   4C                   ENDIF
045500941129E   3C                   ENDIF
045600941124E   2C                   ENDIF
045700930706     C*
045800990805     C     KARB2         READE     FNARBP0E                               30
045900930806E   1C                   ENDDO
046000930706     C*
046100930706     C                   ENDSR
046200930806     C*---------------------------------------------------------------*
046300930806     C*                    MANBOL                                     *
046400930806     C* ROUTINE DI CONTROLLO SE MANCA LA BOLLA O E' CAMBIATO IL TIPO  *
046500930806     C*---------------------------------------------------------------*
046600930806     C     MANBOL        BEGSR
046700930806     C*
046800930806     C* 01 02 ON - MANCANO ENTRAMBE SCRIVO FILE DEGLI ERRORI
046900930806IF  1C     *IN01         IFEQ      *ON
047000930806     C     *IN02         ANDEQ     *ON
047100070202     c                   exsr      CERBLP
047200070202     c
047300070202     c* Non trovata nemmeno in blp come merce mai affidata
047400070202   1ac                   if        wChiusainBLP=' '
047500960208     C                   SETON                                        08
047600960208     C*
047700960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
047800960208     C     ARBDTV        IFLE      DATAM
047900930806     C                   Z-ADD     102           FEV
048000930806     C                   MOVEL     ERR(4)        DER
048100950111     C                   EXSR      RIEARB
048200930806     C*
048300930806     C                   Z-ADD     102           FEV
048400930806     C                   MOVEL     ERR(1)        DER
048500950111     C                   EXSR      RIEARB
048600960208     C                   ENDIF
048700070202   1aC                   ENDIF
048800930806     C*
048900930806X   1C                   ELSE
049000930806     C*
049100930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
049200930806IF  2C     *IN01         IFEQ      *ON
049300930806     C     *IN02         OREQ      *ON
049400930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
049500930806IF  3C     *IN01         IFEQ      *ON
049600930806     C                   MOVEL     §3ATB1        ALF2
049700930806     C                   EXSR      RICBOL
049800930806     C*
049900930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
050000960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
050100930806IF  4C     TIBOL         IFEQ      *BLANKS
050200960208     C     ARBDTV        IFLE      DATAM
050300930806     C                   Z-ADD     102           FEV
050400930806     C                   MOVEL     ERR(4)        DER
050500950111     C                   EXSR      RIEARB
050600960208     C                   ENDIF
050700930806X   4C                   ELSE
050800930806     C                   MOVEL     TIBOL         §3ATB2
050900990702     C                   SETOFF                                       01
051000930806E   4C                   ENDIF
051100990805     C* RICHIAMO LA PRIMA BOLLA
051200990702     C                   MOVEL     §3ATB1        TBL
051300990702     C                   SETOFF                                       111213
051400990702     C     KTAS          CHAIN     TITAS30C                           37
051500930806     C*
051600930806X   3C                   ELSE
051700990805     C* N O N   T R O V A T A   P R I M A   B O L L A
051800930806     C                   MOVEL     §3ATB2        ALF2              2
051900930806     C                   EXSR      RICBOL
052000930806     C*
052100930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
052200930806IF  4C     TIBOL         IFEQ      *BLANKS
052300070202     c* Se la bolla è chiusa in partenza con cca=/-merce mai affidadata
052400070202     c*  cancello senza dare erorre
052500070202     c                   exsr      CERBLP
052600070202     c
052700070202     c* Non trovata nemmeno in blp come merce mai affidata
052800070202    5c                   if        wChiusainBLP=' '
052900070202     c
053000960208     C                   SETON                                        08
053100960208     C     ARBDTV        IFLE      DATAM
053200930806     C                   Z-ADD     102           FEV
053300930806     C                   MOVEL     ERR(1)        DER
053400950111     C                   EXSR      RIEARB
053500960208     C                   ENDIF
053600070202    5C                   ENDIF
053700070202     c
053800930806X   4C                   ELSE
053900941129     C   11              SETON                                        05
054000941129     C   12              SETON                                        06
054100941129     C   13              SETON                                        07
054200930806     C                   MOVEL     TIBOL         §3ATB1
054300930806E   4C                   ENDIF
054400930806E   3C                   ENDIF
054500930806E   2C                   ENDIF
054600930806E   1C                   ENDIF
054700930806     C*
054800930806     C                   ENDSR
054900930806     C*---------------------------------------------------------------*
055000930806     C*                    RICBOL                                     *
055100930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
055200930806     C*---------------------------------------------------------------*
055300930806     C     RICBOL        BEGSR
055400930806     C*
055500930806     C                   SETOFF                                       3940
055600941129     C                   SETOFF                                       111213
055700930806     C                   CLEAR                   TIBOL
055800930806     C*
055900990701     C     KTAS2         SETLL     TITAS30C
056000990701     C     KTAS2         READE     TITAS30C                               39
056100930806     C*
056200930806DO  1C     *IN39         DOWEQ     *OFF
056300930806     C*
056400930806IF  2C     TASTBL        IFNE      ALF2
056500930806     C                   SETON                                        39
056600930806     C                   MOVEL     TASTBL        TIBOL             2
056700930806E   2C                   ENDIF
056800930806     C*
056900990701     C  N39KTAS2         READE     TITAS30C                               39
057000930806E   1C                   ENDDO
057100930806     C*
057200930806     C                   ENDSR
057300070202     C*---------------------------------------------------------------*
057400070202     C*                    CERBLP                                     *
057500070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
057600070202     c* merce mai affdata
057700070202     C*---------------------------------------------------------------*
057800070202     C     CERBLP        BEGSR
057900070202     c                   clear                   wChiusainBLP      1
058000070202     c     ktas2         chain     fnblp01l
058100070202    5c                   if        %found(fnblp01l) and blpft1='N'
058200070202     c                             and blpcca='7'
058300070202     c                   eval      wChiusainBLP='S'
058400070202    5c                   endif
058500070202     c                   ENDSR
058600930422     C*---------------------------------------------------------------*
058700930422     C*                    DEFVAR                                     *
058800930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
058900930422     C*---------------------------------------------------------------*
059000930422     C     DEFVAR        BEGSR
059100930422     C*
059200930422     C     *ENTRY        PLIST
059300930422     C                   PARM                    KPJBA
059400930422     C*
059500950111     C                   Z-ADD     1             CODUT             1 0
059600930806     C                   Z-ADD     0             DAT
059700930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
059800930423     C                   MOVEL     MBRFIL        ALF4              4
059900930423     C                   MOVE      ALF4          NUM3              3 0
060000960208     C* UDATE - 3
060100960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
060200960208     C                   TIME                    W0140            14 0
060300960208     C* UDATE IN GGMMAAAA
060400960208     C                   MOVE      W0140         WDTGIO            8 0
060500960208     C*
060600960208     C* UDATE IN AAAAMMGG
060700960208     C                   Z-ADD     WDTGIO        G02DAT
060800960208     C                   MOVEL     *BLANK        G02ERR
060900960208     C                   CALL      'XSRDA8'
061000960208     C                   PARM                    WLBDAT
061100960208     C                   MOVEL     G02INV        DATEU             8 0
061200960208     C* SOTTRAGGO  3
061300960208     C     G02TGI        SUB       3             GIOTGI
061400960208     C*
061500960208     C                   CALL      'XSRGI8'
061600960208     C                   PARM                    WGIDAT
061700960208     C*
061800960208     C                   Z-ADD     GIOINV        DATAM             8 0
061900070202     c
062000070202     c* Apro file bolle di filiale
062100070202     c                   clear                   tibs55ds
062200070202     c                   eval      i50tla='L'
062300070202     c                   eval      i50apo=001
062400070202     c                   call      'TIBS55R'
062500070202     c                   parm                    tibs55ds
062600070202     c*
062700070202     c                   Clear                   commt
062800070202     c                   Movel(p)  cmdt(1)       commt
062900070202     c                   Eval      %Subst(commt:30:10) = o50ala
063000070202     c                   Eval      commt =
063100070202     c                             %trim(commt) + '/FNBLP01L)'
063200070202     c                   Call      'QCMDEXC'
063300070202     c                   Parm                    commt
063400070202     c                   Parm                    lenght
063500070202     c
063600070202     c                   Open      FNBLP01L
063700950111     C*
063800950111     C* DEFINIZIONE CAMPI
063900950111     C     *LIKE         DEFINE    ARBFEV        FEV
064000950111     C     *LIKE         DEFINE    ARBDER        DER
064100950111     C     *LIKE         DEFINE    ARBDAT        DAT
064200950111     C     *LIKE         DEFINE    TBLCOD        COD
064300950111     C     *LIKE         DEFINE    TBLKEY        KEY
064400930422     C*
064500950111     C* ACCESSO TABEL00F
064600930423     C     KTAB          KLIST
064700950111     C                   KFLD                    CODUT
064800950111     C                   KFLD                    COD
064900950111     C                   KFLD                    KEY
065000990701     C* ACCESSO TITAS30C
065100930426     C     KTAS          KLIST
065200950111     C                   KFLD                    ARBAAS
065300950111     C                   KFLD                    ARBLNP
065400950111     C                   KFLD                    ARBNRS
065500950111     C                   KFLD                    ARBNSP
065600930528     C                   KFLD                    TBL               2
065700990701     C* ACCESSO TITAS30C/TNBLA01L
065800930528     C     KTAS2         KLIST
065900950111     C                   KFLD                    ARBAAS
066000950111     C                   KFLD                    ARBLNP
066100950111     C                   KFLD                    ARBNRS
066200950111     C                   KFLD                    ARBNSP
066300991028     C* ACCESSO FIARBE2C
066400950111     C     KARB          KLIST
066500950111     C                   KFLD                    ARBAAS
066600950111     C                   KFLD                    ARBLNP
066700950111     C                   KFLD                    ARBNRS
066800950111     C                   KFLD                    ARBNSP
066900950111     C                   KFLD                    ARBDER
067000950111     C                   KFLD                    DAT
067100950111     C     KARB2         KLIST
067200950111     C                   KFLD                    ARBAAS
067300950111     C                   KFLD                    ARBLNP
067400950111     C                   KFLD                    ARBNRS
067500950111     C                   KFLD                    ARBNSP
067600930423     C*
067700930423     C                   ENDSR
067800930423     C*---------------------------------------------------------------*
067900930423     C*                    QCMDEXC                                    *
068000930423     C*---------------------------------------------------------------*
068100940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
068200930528**         CMD1
068300990805CHKOBJ OBJ(FNARBP0R) OBJTYPE(*FILE)     MBR(M234567890)
068400930423**         CMD2
068500990805ALCOBJ OBJ((FNARBP0R *FILE *EXCL           M234567890))
068600930423**         CMD3
068700990805OVRDBF FILE(FNARBP0R)                   MBR(M234567890)
068800930426**         ERR
0689009305281 SPEDIZIONE INESISTENTE
069000950222SPEDIZIONE GIA' TASSATA
069100930528CODICE BOLLA INESISTENTE
0692009305282 SPEDIZIONE INESISTENTE
069300070202** cmdt
069400070202OVRDBF FILE(FNBLP01L) TOFILE(
