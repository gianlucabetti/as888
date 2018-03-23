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
007001130122     D fnlvv1ds      E DS
007004130122     d dsarbv        e ds
007005130124     D                SDS
007006130124     D  NOMPGM                 1     10
007007130122     d
007008130206     d parvar          s            224
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
012301130122      *
012302130122     C                   MOVEL     'C'           iv1tla
012303130122     c                   movel     fnlvv1ds      kpjbu
012304130122     C                   CALL      'FNLVV1R'
012305130122     C                   PARM                    kpjba
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
013500111123     c                   clear                   Wchiusainblp
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
023700930528     C* CANCELLO SOLO VARIAZIONE TRASMESSA
023800990805     C                   DELETE    FNARBP0R
023900930806E   2C                   ENDIF
024000950905     C*
024100950905X  1AC                   ELSE
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
034101130122     c* se bolla contabilizzata richiamo pgm di gestione file per recupero
034102130122     c* variazioni post-fatturazione
034103130122     c                   if        tasfic<>' '
034104130122     c                   clear                   fnlvv1ds
034105130122     c                   eval      iv1cvb='PV'
034106130124     c                   eval      iv1pgm=nompgm
034107130122     c                   eval      iv1aas=tasaas
034108130122     c                   eval      iv1lnp=taslnp
034109130122     c                   eval      iv1nrs=tasnrs
034110130122     c                   eval      iv1nsp=tasnsp
034111130122     c                   eval      iv1tbl=tastbl
034112130122     c                   clear                   dsarbv
034113130122     c                   eval      §bvncp=TASNCP
034114130122     c                   eval      §bvpkc=TASPKC
034115130122     c                   eval      §bvfpf=TASFPF
034116130122     c                   eval      §bvpkf=TASPKF
034117130122     c                   eval      parvar=dsarbv
034118130122     c                   eval      kpjbu=fnlvv1ds
034119130122     c                   call      'FNLVV1R'
034120130122     c                   parm                    kpjba
034121130122     c                   parm                    parvar
034122130122     c                   endif
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
051400990805     C* RICHIAMO LA PRIMA BOLLA
051500990702     C                   MOVEL     §3ATB1        TBL
051600990702     C                   SETOFF                                       111213
051700990702     C     KTAS          CHAIN     TITAS30C                           37
051800930806     C*
051900930806X   3C                   ELSE
052000990805     C* N O N   T R O V A T A   P R I M A   B O L L A
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
053900070202    5C                   ENDIF
054000070202     c
054100930806X   4C                   ELSE
054200941129     C   11              SETON                                        05
054300941129     C   12              SETON                                        06
054400941129     C   13              SETON                                        07
054500930806     C                   MOVEL     TIBOL         §3ATB1
054600930806E   4C                   ENDIF
054700930806E   3C                   ENDIF
054800930806E   2C                   ENDIF
054900930806E   1C                   ENDIF
055000930806     C*
055100930806     C                   ENDSR
055200930806     C*---------------------------------------------------------------*
055300930806     C*                    RICBOL                                     *
055400930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
055500930806     C*---------------------------------------------------------------*
055600930806     C     RICBOL        BEGSR
055700930806     C*
055800930806     C                   SETOFF                                       3940
055900941129     C                   SETOFF                                       111213
056000930806     C                   CLEAR                   TIBOL
056100930806     C*
056200990701     C     KTAS2         SETLL     TITAS30C
056300990701     C     KTAS2         READE     TITAS30C                               39
056400930806     C*
056500930806DO  1C     *IN39         DOWEQ     *OFF
056600930806     C*
056700930806IF  2C     TASTBL        IFNE      ALF2
056800930806     C                   SETON                                        39
056900930806     C                   MOVEL     TASTBL        TIBOL             2
057000930806E   2C                   ENDIF
057100930806     C*
057200990701     C  N39KTAS2         READE     TITAS30C                               39
057300930806E   1C                   ENDDO
057400930806     C*
057500930806     C                   ENDSR
057600070202     C*---------------------------------------------------------------*
057700070202     C*                    CERBLP                                     *
057800070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
057900070202     c* merce mai affdata
058000070202     C*---------------------------------------------------------------*
058100070202     C     CERBLP        BEGSR
058200070202     c                   clear                   wChiusainBLP      1
058300070202     c     ktas2         chain     fnblp01l
058400070202    5c                   if        %found(fnblp01l) and blpft1='N'
058500070302     c                   if        blpcca='7'
058600070202     c                   eval      wChiusainBLP='S'
058700070302     c                   else
058800070302     c* Verifico se si tratta di import
058900070302     c     blplnp        chain     azorg01l
059000070302If   c                   If        %Found(Azorg01l)
059100070302     c                   Movel     OrgDe3        og143
059200070302     c                   Else
059300070302     c                   Clear                   og143
059400070302     c                   EndIf
059500070302    5c                   if        §ogntw='EEX'  or §ogntw='DPD'
059600070302     c                   eval      wChiusainBLP='S'
059700070302     c                   EndIf
059800070302     C
059900070302     c                   EndIf
060000070302     c                   EndIf
060100070202     c                   ENDSR
060200930422     C*---------------------------------------------------------------*
060300930422     C*                    DEFVAR                                     *
060400930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
060500930422     C*---------------------------------------------------------------*
060600930422     C     DEFVAR        BEGSR
060700930422     C*
060800930422     C     *ENTRY        PLIST
060900930422     C                   PARM                    KPJBA
061000930422     C*
061100950111     C                   Z-ADD     1             CODUT             1 0
061200930806     C                   Z-ADD     0             DAT
061300930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
061400930423     C                   MOVEL     MBRFIL        ALF4              4
061500930423     C                   MOVE      ALF4          NUM3              3 0
061600960208     C* UDATE - 3
061700960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
061800960208     C                   TIME                    W0140            14 0
061900960208     C* UDATE IN GGMMAAAA
062000960208     C                   MOVE      W0140         WDTGIO            8 0
062100960208     C*
062200960208     C* UDATE IN AAAAMMGG
062300960208     C                   Z-ADD     WDTGIO        G02DAT
062400960208     C                   MOVEL     *BLANK        G02ERR
062500960208     C                   CALL      'XSRDA8'
062600960208     C                   PARM                    WLBDAT
062700960208     C                   MOVEL     G02INV        DATEU             8 0
062800960208     C* SOTTRAGGO  3
062900960208     C     G02TGI        SUB       3             GIOTGI
063000960208     C*
063100960208     C                   CALL      'XSRGI8'
063200960208     C                   PARM                    WGIDAT
063300960208     C*
063400960208     C                   Z-ADD     GIOINV        DATAM             8 0
063500070202     c
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
067900991028     C* ACCESSO FIARBE2C
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
069900990805CHKOBJ OBJ(FNARBP0R) OBJTYPE(*FILE)     MBR(M234567890)
070000930423**         CMD2
070100990805ALCOBJ OBJ((FNARBP0R *FILE *EXCL           M234567890))
070200930423**         CMD3
070300990805OVRDBF FILE(FNARBP0R)                   MBR(M234567890)
070400930426**         ERR
0705009305281 SPEDIZIONE INESISTENTE
070600950222SPEDIZIONE GIA' TASSATA
070700930528CODICE BOLLA INESISTENTE
0708009305282 SPEDIZIONE INESISTENTE
070900070202** cmdt
071000070202OVRDBF FILE(FNBLP01L) TOFILE(
