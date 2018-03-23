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
007001130118     D fnlvv1ds      E DS
007002130118     d dsarbv        e ds
007003130124     D                SDS
007004130124     D  NOMPGM                 1     10
007100070202     d
007101130206     d parvar          s            224
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
012401130118      *
012402130118     C                   MOVEL     'C'           iv1tla
012403130121     c                   movel     fnlvv1ds      kpjbu
012404130118     C                   CALL      'FNLVV1R'
012405130121     C                   PARM                    kpjba
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
013600111123     c                   clear                   Wchiusainblp
013700930806     C*
013800930806     C* LETTURA TIPO BOLLA
013900930528     C                   MOVEL     '3A'          COD
014000950111     C                   MOVEL(P)  ARBCBO        KEY
014100930528     C     KTAB          CHAIN     TABEL                              30
014200930528     C*
014300930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
014400930806IF  1C     *IN30         IFEQ      '1'
014500930806     C                   Z-ADD     102           FEV
014600930806     C                   MOVEL     ERR(3)        DER
014700950111     C                   EXSR      RIEARB
014800930528     C*
014900930806X   1C                   ELSE
015000930528     C*
015100930528     C                   MOVEL     TBLUNI        DS3A
015200950905     C* SE SI TRATTA DI UNA BOLLA NON TRASMESSA IN SEDE
015300950905     C* CANCELLO SENZA DARE ERRORE
015400950905   1AC     §3ABSD        IFEQ      ' '
015500930806     C*
015600930806     C* 2   B O L L A
015700930806     C*
015800930806IF  2C     §3ATB2        IFNE      '  '
015900930806     C                   MOVEL     §3ATB2        TBL
016000930528     C*
016100930426     C* CERCO BOLLA IN ARCHIVIO CORRENTE
016200941129     C                   SETOFF                                       111213
016300990701     C     KTAS          CHAIN     TITAS30C                           37
016400930426     C*
016500930426     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
016600941129     C   37KTAS2         CHAIN     TNBLA01L                           38
016700941129     C   37
016800941129     CAN 38              SETON                                        01
016900941129    2C                   ENDIF
017000930426     C*
017100930806     C* 1   B O L L A
017200930806     C*
017300930806     C                   MOVEL     §3ATB1        TBL
017400930528     C*
017500930528     C* CERCO BOLLA IN ARCHIVIO CORRENTE
017600941129     C                   SETOFF                                       111213
017700990701     C     KTAS          CHAIN     TITAS30C                           37
017800930528     C*
017900930528     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
018000941129IF  2C     *IN37         IFEQ      '1'
018100941129     C     KTAS2         CHAIN     TNBLA01L                           38
018200941129     C   38              SETON                                        02
018300930528     C*
018400941129X   2C                   ELSE
018500941129     C* BOLLA TROVATA
018600941129     C   11              SETON                                        05
018700941129     C   12              SETON                                        06
018800941129     C   13              SETON                                        07
018900941129E   2C                   ENDIF
019000930806     C*
019100930806     C* CONTROLLO SE MANCANO VERAMENTE O E' CAMBIATO IL TIPO BOLLA
019200930806     C   01
019300930806     COR 02              EXSR      MANBOL
019400930806     C*
019500930806     C* SE PRIMA BOLLA INESISTENTE ESCO E NON AGGIORNO
019600930806IF  2C     *IN08         IFEQ      '0'
019700070202     c
019800070202     c* Se chiusa in blp non stampo ma cancello in fnarbv0r
019900070202   2ac     wChiusainBLP  ifeq      ' '
020000930806     C*
020100930528     C* 1 BOLLA TROVATA IN ARCHIVIO (CORRENTE, STORICO O PRECEDENTE)
020200930528IF  3C     *IN05         IFEQ      '1'
020300930427     C     *IN06         OREQ      '1'
020400930427     C     *IN07         OREQ      '1'
020500930528     C                   EXSR      AGGIOR
020600930528     C*
020700930806IF  4C     *IN05         IFEQ      *ON
020800930806     C*
020900930706     C* SE ESISTE ERRORE NON STAMPATO CANCELLO
021000950111     C                   EXSR      ANNARB
021100930706     C*
021200990701     C                   UPDATE    TITAS000
021300930806E   4C                   ENDIF
021400930706     C*
021500990701     C   06              UPDATE    TITAS010
021600940208     C*
021700990701     C   07              UPDATE    TITASP00
021800930806E   3C                   ENDIF
021900930806     C*
022000941129     C* 2 BOLLA TROVATA IN ARCHIVIO
022100941129IF  3C     §3ATB2        IFNE      *BLANKS
022200941129     C     *IN01         ANDEQ     *OFF
022300941129     C*
022400930528     C                   MOVEL     §3ATB2        TBL
022500941129     C                   SETOFF                                       111213
022600990701     C     KTAS          CHAIN     TITAS30C                           30
022700930528     C*
022800941129    4C     *IN30         IFEQ      *OFF
022900930528     C                   EXSR      AGGIOR
023000930528     C*
023100990701     C   11              UPDATE    TITAS000
023200990701     C   12              UPDATE    TITAS010
023300990701     C   13              UPDATE    TITASP00
023400941129E   4C                   ENDIF
023500941129E   3C                   ENDIF
023600070202E  2aC                   ENDIF
023700930528     C*
023800070202     C*
023900950110     C                   DELETE    FNARBV0R
024000930806E   2C                   ENDIF
024100950905     C*
024200950905X  1AC                   ELSE
024300950905     C                   DELETE    FNARBV0R
024400950905E  1AC                   ENDIF
024500950905E   1C                   ENDIF
024600930806     C*
024700930528     C                   ENDSR
024800930528     C*---------------------------------------------------------------*
024900930528     C*                    AGGIOR                                     *
025000930806     C* ROUTINE DI AGGIORNAMENTO VOLUME SULLA BOLLA                   *
025100930528     C*---------------------------------------------------------------*
025200930528     C     AGGIOR        BEGSR
025300930426     C*
025400930426     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO VOLUME UGUALE A 'T'
025500970625    1C******     TASFVF    IFNE 'T'
025600930528     C*
025700930806     C                   MOVEL     TASTBL        BOL1              1
025800930806     C                   MOVE      TASTBL        BOL2              1
025900930426     C                   MOVE      TASKSC        CODCLI            4 0
026000930806     C*
026100950111     C                   CLEAR                   DSLV20
026200950111     C*
026300950111     C* SE E' LA PRIMA BOLLA
026400950111    2C     TBL           IFEQ      §3ATB1
026500950111     C*
026600950111     C* SE C/SERVIZIO O STORNO O ASSEGNATO 9999
026700950111    3C     BOL2          IFEQ      '7'
026800950111     C     BOL2          OREQ      'C'
026900950111     C     BOL2          OREQ      'S'
027000950111     C     BOL1          OREQ      'A'
027100950111     C     CODCLI        ANDEQ     9999
027200950111     C*
027300950111     C* NON PASSO LA DATA FATTURA
027400950111     C                   CLEAR                   D20DFT
027500950111     C*
027600950111   X3C                   ELSE
027700950111     C                   MOVEL     TASDFT        D20DFT
027800950111    3C                   ENDIF
027900930623     C*
028000950111   X2C                   ELSE
028100950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
028200950111     C                   CLEAR                   D20DFT
028300950111    2C                   ENDIF
028400930623     C*
028500950111     C                   MOVEL     'C'           D20TVL
028600950111     C                   MOVEL     ARBNCR        D20NCR
028700950111     C                   MOVEL     ARBVLC        D20VLU
028800950111     C                   MOVEL     TASFVF        D20FVF
028900950111     C                   MOVEL     TASVLF        D20VLF
029000950111     C                   MOVEL     TASFVB        D20FVB
029100950111     C                   MOVEL     TASVLB        D20VLB
029200950111     C                   MOVEL     TASAAS        D20AAS
029300950111     C                   MOVEL     TASLNP        D20LNP
029400950111     C                   MOVEL     TASNRS        D20NRS
029500950111     C                   MOVEL     TASNSP        D20NSP
029600950111     C                   MOVEL     ARBCBO        D20CBO
029700950111     C                   MOVEL     TASNCL        D20NCL
029800950111     C                   MOVEL     TASLNA        D20LNA
029900950111     C                   MOVEL     NUM3          D20MBR
030000960927     C                   MOVE      TASMGS        D20DTE
030100960927     C                   MOVEL     TASAAS        D20DTE
030200970904     C* SE IL VOLUME CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
030300970904     C*  COLLI RILEVATI, LI METTO UGUALI
030400970904     C     ARBFVC        IFEQ      'T'
030500970904     C     ARBNCR        ANDNE     TASNCL
030600970904     C                   Z-ADD     TASNCL        D20NCR
030700970904     C                   ENDIF
030800950111     C*
030900950111     C                   MOVEL     DSLV20        KPJBU
031000950111     C*
031100950111     C                   CALL      'FNLV20R'
031200950111     C                   PARM                    KPJBA
031300950111     C                   MOVEL     KPJBU         DSLV20
031400950111     C*
031500930623     C*
031600990702     C* SE ERRORE SCRIVO FILE ERRORI
031700950111    2C     D20ERR        IFEQ      '2'
031800950111     C*
031900930623     C* FRANCO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNP
032000950111    3C     BOL1          IFEQ      'F'
032100990701     C                   Z-ADD     TASTFP        FEV
032200950111    3C                   ELSE
032300930623     C* ASSEGNATO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNA
032400990702     C                   CLEAR                   DSLV55
032500990702     C                   MOVEL     'P'           D55TPT
032600990702     C                   Z-ADD     TASLNA        D55LIN
032700990702     C                   Z-ADD     DATEU         D55DRF
032800990702     C                   CALL      'FNLV55R'
032900990702     C                   PARM                    DSLV55
033000990702     C                   Z-ADD     D55TFP        FEV
033100950111    3C                   ENDIF
033200950111     C                   MOVEL     ERR(2)        DER
033300930623     C*
033400930623     C* SCRIVO IL FILE DEGLI ERRORI
033500950111     C                   EXSR      RIEARB
033600950111    2C                   ENDIF
033700990702     C* AGGIORNO IN OGNI CASO I CAMPI DI TITAS (ANCHE SE C'E' ERRORE)
033800990702     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV20R
033900950111     C                   MOVEL     D20NCR        TASNCR
034000950111     C                   MOVEL     D20VLU        TASVLC
034100950111     C                   MOVEL     D20FVF        TASFVF
034200950111     C                   MOVEL     D20VLF        TASVLF
034201130118     c* se bolla contabilizzata richiamo pgm di gestione file per recupero
034202130118     c* variazioni post-fatturazione
034203130118     c                   if        tasfic<>' '
034204130118     c                   clear                   fnlvv1ds
034205130118     c                   eval      iv1cvb='VV'
034206130124     c                   eval      iv1pgm=nompgm
034207130118     c                   eval      iv1aas=tasaas
034208130118     c                   eval      iv1lnp=taslnp
034209130118     c                   eval      iv1nrs=tasnrs
034210130118     c                   eval      iv1nsp=tasnsp
034211130118     c                   eval      iv1tbl=tastbl
034212130118     c                   clear                   dsarbv
034213130118     c                   eval      §bvncr=TASNCR
034214130118     c                   eval      §bvvlc=TASVLC
034215130118     c                   eval      §bvfvf=TASFVF
034216130118     c                   eval      §bvvlf=TASVLF
034218130118     c                   eval      parvar=dsarbv
034219130121     c                   eval      kpjbu=fnlvv1ds
034220130118     c                   call      'FNLVV1R'
034221130121     c                   parm                    kpjba
034222130118     c                   parm                    parvar
034223130118     c                   endif
034300970625    1C***********          ENDIF
034400930426     C*
034500930423     C                   ENDSR
034600930423     C*---------------------------------------------------------------*
034700930423     C*                    CAEXA                                      *
034800930423     C* ROUTINE DI APERTURA MEMBRO FILE DI RICEZIONE                  *
034900930423     C*---------------------------------------------------------------*
035000930423     C     CAEXA         BEGSR
035100930423     C*
035200930423     C                   MOVEL     *BLANKS       VAR11            11
035300930423     C                   MOVEL     *BLANKS       VAR12            12
035400930423     C                   MOVE      ')'           VAR11
035500930423     C                   MOVEL     MBRFIL        VAR11
035600930423     C                   MOVE      '))'          VAR12
035700930423     C                   MOVEL     MBRFIL        VAR12
035800930423     C                   Z-ADD     55            LUNG             15 5
035900930423     C* CHKOBJ
036000930423     C                   MOVE      VAR11         CMD1
036100930423     C                   MOVEL     *BLANKS       COMMAN
036200930423     C                   MOVEA     CMD1(1)       COMMAN           80
036300930423     C                   CALL      'QCMDEXC'                            32
036400930423     C                   PARM                    COMMAN
036500930423     C                   PARM                    LUNG
036600930423IF  1C     *IN32         IFEQ      '0'
036700930423     C* ALCOBJ
036800930423     C                   MOVE      VAR12         CMD2
036900930423     C                   MOVEL     *BLANKS       COMMAN
037000930423     C                   MOVEA     CMD2(1)       COMMAN
037100930423     C                   CALL      'QCMDEXC'                            32
037200930423     C                   PARM                    COMMAN
037300930423     C                   PARM                    LUNG
037400930423IF  2C     *IN32         IFEQ      '0'
037500930423     C* OVRDBF
037600930423     C                   MOVE      VAR11         CMD3
037700930423     C                   MOVEL     *BLANKS       COMMAN
037800930423     C                   MOVEA     CMD3(1)       COMMAN
037900930423     C                   CALL      'QCMDEXC'                            32
038000930423     C                   PARM                    COMMAN
038100930423     C                   PARM                    LUNG
038200930423IF  3C     *IN32         IFEQ      '0'
038300930423     C* OPEN
038400950110     C                   OPEN      FNARBV0R
038500930423E   3C                   ENDIF
038600930423E   2C                   ENDIF
038700930423E   1C                   ENDIF
038800930423     C*
038900930423     C                   ENDSR
039000930706     C*---------------------------------------------------------------*
039100950111     C*                    RIEARB                                     *
039200930806     C* ROUTINE DI RIEMPIMENTO ARCHIVIO ERRORI                        *
039300930706     C*---------------------------------------------------------------*
039400950111     C     RIEARB        BEGSR
039500930806     C*
039600950111     C                   MOVEL     DER           ARBDER
039700950111     C     KARB          CHAIN     FNARBV0E                           30
039800930806     C*
039900930706     C* SE NON TROVATO RECORD
040000930806IF  1C     *IN30         IFEQ      *ON
040100930806     C* O TROVATO MA NON E' QUELLE VARIAZIONE DI VOLUME
040200950111     C     RRBVLC        ORNE      ARBVLC
040300930806     C*
040400930806     C* RISCRIVO ERRORE
040500950111     C                   CLEAR                   FNARBV0E
040600950111     C                   Z-ADD     ARBAAS        RRBAAS
040700950111     C                   Z-ADD     ARBLNP        RRBLNP
040800950111     C                   Z-ADD     ARBNRS        RRBNRS
040900950111     C                   Z-ADD     ARBNSP        RRBNSP
041000950111     C                   MOVEL     ARBCBO        RRBCBO
041100950111     C                   MOVEL     ARBDTV        RRBDTV
041200950111     C                   MOVEL     ARBORV        RRBORV
041300950111     C                   MOVEL     ARBCVB        RRBCVB
041400950111     C                   Z-ADD     ARBVLC        RRBVLC
041500950111     C                   MOVEL     ARBFVC        RRBFVC
041600950111     C                   MOVEL     ARBNCR        RRBNCR
041700950111     C                   MOVEL     FEV           ARBFEV
041800950111     C                   MOVEL     DER           ARBDER
041900950111     C                   CLEAR                   ARBDAT
042000950111     C                   WRITE     FNARBV0E
042100930806E   1C                   ENDIF
042200930806     C*
042300930706     C                   ENDSR
042400930706     C*---------------------------------------------------------------*
042500950111     C*                    ANNARB                                     *
042600930806     C* ROUTINE DI ANNULLAMENTO ERRORE PERCHE' VARIAZIONE CARICATA    *
042700930706     C*---------------------------------------------------------------*
042800950111     C     ANNARB        BEGSR
042900930806     C*
043000950111     C     KARB2         SETLL     FNARBV0E
043100950111     C     KARB2         READE     FNARBV0E                               30
043200930706     C*
043300930806DO  1C     *IN30         DOWEQ     *OFF
043400930706     C*
043500930706     C* NON DEVE ESSERE RECORD STAMPATO (DATA = 0)
043600930706     C*  E NON DEVE ESSERE GIA' FATTURATA CHE LA STAMPO COMUNQUE
043700950111IF  2C     ARBDAT        IFEQ      0
043800950111     C     ARBDER        ANDNE     ERR(2)
043900930706     C*
044000941129     C* SE E' "2 SPE INESISTENTE" DEVO AVERE TROVATO LA 2 BOLLA
044100950111IF  3C     ARBDER        IFEQ      ERR(4)
044200950111IF  4C     *IN01         IFEQ      *OFF
044300950111     C                   DELETE    FNARBV0E
044400950111E   4C                   ENDIF
044500930706     C*
044600950111X   3C                   ELSE
044700930706     C*
044800930706     C* SE E' "1 SPE INESISTENTE" DEVO AVERE TROVATO LA 1 BOLLA (05 ON)
044900950111IF  4C     ARBDER        IFEQ      ERR(1)
045000950111IF  5C     *IN05         IFEQ      *ON
045100950111     C                   DELETE    FNARBV0E
045200950111E   5C                   ENDIF
045300930706     C*
045400950111X   4C                   ELSE
045500930706     C*
045600930706     C* SE E' " CODICE BOLLA INESISTE ANNULLO SEMPRE
045700950111     C                   DELETE    FNARBV0E
045800950111E   4C                   ENDIF
045900941129E   3C                   ENDIF
046000941124E   2C                   ENDIF
046100930706     C*
046200950111     C     KARB2         READE     FNARBV0E                               30
046300930806E   1C                   ENDDO
046400930706     C*
046500930706     C                   ENDSR
046600930806     C*---------------------------------------------------------------*
046700930806     C*                    MANBOL                                     *
046800930806     C* ROUTINE DI CONTROLLO SE MANCA LA BOLLA O E' CAMBIATO IL TIPO  *
046900930806     C*---------------------------------------------------------------*
047000930806     C     MANBOL        BEGSR
047100930806     C*
047200930806     C* 01 02 ON - MANCANO ENTRAMBE SCRIVO FILE DEGLI ERRORI
047300930806IF  1C     *IN01         IFEQ      *ON
047400930806     C     *IN02         ANDEQ     *ON
047500070202     c                   exsr      CERBLP
047600070202     c
047700070202     c* Non trovata nemmeno in blp come merce mai affidata
047800070202   1ac                   if        wChiusainBLP=' '
047900960208     C                   SETON                                        08
048000960208     C*
048100960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
048200960208     C     ARBDTV        IFLE      DATAM
048300930806     C                   Z-ADD     102           FEV
048400930806     C                   MOVEL     ERR(4)        DER
048500950111     C                   EXSR      RIEARB
048600930806     C*
048700930806     C                   Z-ADD     102           FEV
048800930806     C                   MOVEL     ERR(1)        DER
048900950111     C                   EXSR      RIEARB
049000960208     C                   ENDIF
049100070202   1aC                   ENDIF
049200930806     C*
049300930806X   1C                   ELSE
049400930806     C*
049500930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
049600930806IF  2C     *IN01         IFEQ      *ON
049700930806     C     *IN02         OREQ      *ON
049800930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
049900930806IF  3C     *IN01         IFEQ      *ON
050000930806     C                   MOVEL     §3ATB1        ALF2
050100930806     C                   EXSR      RICBOL
050200930806     C*
050300930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
050400960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
050500930806IF  4C     TIBOL         IFEQ      *BLANKS
050600960208     C     ARBDTV        IFLE      DATAM
050700930806     C                   Z-ADD     102           FEV
050800930806     C                   MOVEL     ERR(4)        DER
050900950111     C                   EXSR      RIEARB
051000960208     C                   ENDIF
051100930806X   4C                   ELSE
051200930806     C                   MOVEL     TIBOL         §3ATB2
051300990702     C                   SETOFF                                       01
051400930806E   4C                   ENDIF
051500990702     C* RICHAINO LA PRIMA BOLLA
051600990702     C                   MOVEL     §3ATB1        TBL
051700990702     C                   SETOFF                                       111213
051800990702     C     KTAS          CHAIN     TITAS30C                           37
051900930806     C*
052000930806X   3C                   ELSE
052100930806     C* N O N   T R O V AT A   P R I M A   B O L L A
052200930806     C                   MOVEL     §3ATB2        ALF2              2
052300930806     C                   EXSR      RICBOL
052400930806     C*
052500930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
052600930806IF  4C     TIBOL         IFEQ      *BLANKS
052700070202     c* Se la bolla è chiusa in partenza con cca=/-merce mai affidadata
052800070202     c*  cancello senza dare erorre
052900070202     c                   exsr      CERBLP
053000070202     c
053100070202     c* Non trovata nemmeno in blp come merce mai affidata
053200070202    5c                   if        wChiusainBLP=' '
053300070202     c
053400960208     C                   SETON                                        08
053500960208     C     ARBDTV        IFLE      DATAM
053600930806     C                   Z-ADD     102           FEV
053700930806     C                   MOVEL     ERR(1)        DER
053800950111     C                   EXSR      RIEARB
053900960208     C                   ENDIF
054000070202E   5C                   ENDIF
054100070202     c
054200930806X   4C                   ELSE
054300941129     C   11              SETON                                        05
054400941129     C   12              SETON                                        06
054500941129     C   13              SETON                                        07
054600930806     C                   MOVEL     TIBOL         §3ATB1
054700070202E   4C                   ENDIF
054800930806E   3C                   ENDIF
054900930806E   2C                   ENDIF
055000930806E   1C                   ENDIF
055100930806     C*
055200930806     C                   ENDSR
055300070202     C*---------------------------------------------------------------*
055400070202     C*                    CERBLP                                     *
055500070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
055600070202     c* merce mai affdata
055700070202     C*---------------------------------------------------------------*
055800070202     C     CERBLP        BEGSR
055900070202     c                   clear                   wChiusainBLP      1
056000070202     c     ktas2         chain     fnblp01l
056100070202    5c                   if        %found(fnblp01l) and blpft1='N'
056200070302     c                   if        blpcca='7'
056300070202     c                   eval      wChiusainBLP='S'
056400070302     c                   else
056500070302     c* Verifico se si tratta di import
056600070302     c     blplnp        chain     azorg01l
056700070302If   c                   If        %Found(Azorg01l)
056800070302     c                   Movel     OrgDe3        og143
056900070302     c                   Else
057000070302     c                   Clear                   og143
057100070302     c                   EndIf
057200070302    5c                   if        §ogntw='EEX'  or §ogntw='DPD'
057300070302     c                   eval      wChiusainBLP='S'
057400070302     c                   EndIf
057500070302     C
057600070302     c                   EndIf
057700070302     c                   EndIf
057800070202     c                   ENDSR
057900930806     C*---------------------------------------------------------------*
058000930806     C*                    RICBOL                                     *
058100930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
058200930806     C*---------------------------------------------------------------*
058300930806     C     RICBOL        BEGSR
058400930806     C*
058500930806     C                   SETOFF                                       3940
058600941129     C                   SETOFF                                       111213
058700930806     C                   CLEAR                   TIBOL
058800930806     C*
058900990701     C     KTAS2         SETLL     TITAS30C
059000990701     C     KTAS2         READE     TITAS30C                               39
059100930806     C*
059200930806DO  1C     *IN39         DOWEQ     *OFF
059300930806     C*
059400930806IF  2C     TASTBL        IFNE      ALF2
059500930806     C                   SETON                                        39
059600930806     C                   MOVEL     TASTBL        TIBOL             2
059700930806E   2C                   ENDIF
059800930806     C*
059900990701     C  N39KTAS2         READE     TITAS30C                               39
060000930806E   1C                   ENDDO
060100930806     C*
060200930806     C*
060300930806     C                   ENDSR
060400930422     C*---------------------------------------------------------------*
060500930422     C*                    DEFVAR                                     *
060600930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
060700930422     C*---------------------------------------------------------------*
060800930422     C     DEFVAR        BEGSR
060900930422     C*
061000930422     C     *ENTRY        PLIST
061100930422     C                   PARM                    KPJBA
061200930422     C*
061300950111     C                   Z-ADD     1             CODUT             1 0
061400930806     C                   Z-ADD     0             DAT
061500930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
061600930423     C                   MOVEL     MBRFIL        ALF4              4
061700930423     C                   MOVE      ALF4          NUM3              3 0
061800960208     C* UDATE - 3
061900960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
062000960208     C                   TIME                    W0140            14 0
062100960208     C* UDATE IN GGMMAAAA
062200960208     C                   MOVE      W0140         WDTGIO            8 0
062300960208     C*
062400960208     C* UDATE IN AAAAMMGG
062500960208     C                   Z-ADD     WDTGIO        G02DAT
062600960208     C                   MOVEL     *BLANK        G02ERR
062700960208     C                   CALL      'XSRDA8'
062800960208     C                   PARM                    WLBDAT
062900960208     C                   MOVEL     G02INV        DATEU             8 0
063000960208     C* SOTTRAGGO  3
063100960208     C     G02TGI        SUB       3             GIOTGI
063200960208     C*
063300960208     C                   CALL      'XSRGI8'
063400960208     C                   PARM                    WGIDAT
063500960208     C*
063600960208     C                   Z-ADD     GIOINV        DATAM             8 0
063700070202     c* Apro file bolle di filiale
063800070202     c                   clear                   tibs55ds
063900070202     c                   eval      i50tla='L'
064000070202     c                   eval      i50apo=001
064100070202     c                   call      'TIBS55R'
064200070202     c                   parm                    tibs55ds
064300070202     c*
064400070202     c                   Clear                   commt
064500070202     c                   Movel(p)  cmdt(1)       commt
064600070202     c                   Eval      %Subst(commt:30:10) = o50ala
064700070202     c                   Eval      commt =
064800070202     c                             %trim(commt) + '/FNBLP01L)'
064900070202     c                   Call      'QCMDEXC'
065000070202     c                   Parm                    commt
065100070202     c                   Parm                    lenght
065200070202     c
065300070202     c                   Open      FNBLP01L
065400950111     C*
065500950111     C* DEFINIZIONE CAMPI
065600950111     C     *LIKE         DEFINE    ARBFEV        FEV
065700950111     C     *LIKE         DEFINE    ARBDER        DER
065800950111     C     *LIKE         DEFINE    ARBDAT        DAT
065900950111     C     *LIKE         DEFINE    TBLCOD        COD
066000950111     C     *LIKE         DEFINE    TBLKEY        KEY
066100930422     C*
066200950111     C* ACCESSO TABEL00F
066300930423     C     KTAB          KLIST
066400950111     C                   KFLD                    CODUT
066500950111     C                   KFLD                    COD
066600950111     C                   KFLD                    KEY
066700990701     C* ACCESSO TITAS30C
066800930426     C     KTAS          KLIST
066900950111     C                   KFLD                    ARBAAS
067000950111     C                   KFLD                    ARBLNP
067100950111     C                   KFLD                    ARBNRS
067200950111     C                   KFLD                    ARBNSP
067300930528     C                   KFLD                    TBL               2
067400990701     C* ACCESSO TITAS30C/TNBLA01L
067500930528     C     KTAS2         KLIST
067600950111     C                   KFLD                    ARBAAS
067700950111     C                   KFLD                    ARBLNP
067800950111     C                   KFLD                    ARBNRS
067900950111     C                   KFLD                    ARBNSP
068000991103     C* ACCESSO FIARBE2C
068100950111     C     KARB          KLIST
068200950111     C                   KFLD                    ARBAAS
068300950111     C                   KFLD                    ARBLNP
068400950111     C                   KFLD                    ARBNRS
068500950111     C                   KFLD                    ARBNSP
068600950111     C                   KFLD                    ARBDER
068700950111     C                   KFLD                    DAT
068800950111     C     KARB2         KLIST
068900950111     C                   KFLD                    ARBAAS
069000950111     C                   KFLD                    ARBLNP
069100950111     C                   KFLD                    ARBNRS
069200950111     C                   KFLD                    ARBNSP
069300930423     C*
069400930423     C                   ENDSR
069500930423     C*---------------------------------------------------------------*
069600930423     C*                    QCMDEXC                                    *
069700930423     C*---------------------------------------------------------------*
069800940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
069900930528**         CMD1
070000950110CHKOBJ OBJ(FNARBV0R) OBJTYPE(*FILE)     MBR(M234567890)
070100930423**         CMD2
070200950110ALCOBJ OBJ((FNARBV0R *FILE *EXCL           M234567890))
070300930423**         CMD3
070400950110OVRDBF FILE(FNARBV0R)                   MBR(M234567890)
070500930426**         ERR
0706009305281 SPEDIZIONE INESISTENTE
070700950222SPEDIZIONE GIA' TASSATA
070800930528CODICE BOLLA INESISTENTE
0709009305282 SPEDIZIONE INESISTENTE
071000070202** cmdt
071100070202OVRDBF FILE(FNBLP01L) TOFILE(
