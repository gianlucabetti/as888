000100921102     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200921106?    F* CMZM04R *----------------------------------------------------*
000300921102?    F*---------*                                                    *
000400921106?    F*               - INVIO SOLLECITO DOCUMENTI -                  *
000500921106?    F*               -----------------------------                  *
000600921102?    F*                                                              *
000700921102?    F*                                                 AZ.          *
000800921102?    F*                                                              *
000900921102?    F*--------------------------------------------------------------*
001000921102?    F* I N D I C A T O R I                                          *
001100921102?    F*--------------------------------------------------------------*
001200921102     F*                                                              *
001300921106     F* -30- LETTURA CMTSM04L                                        *
001400921106     F* -31- SCAN '?'                                                *
001500921112     F* -37- LETTURA AZORG01L                                        *
001600921103     F*                                                              *
001700921106     F* -70- ERR. M004D1 *- FILIALE ERRATA                          -*
001800921111     F* -71- ERR. M004D1 *- LISTA E ERRATA                          -*
001900921106     F* -72- ERR. M004D1 *- SELEZIONI NON POSSIBILI                 -*
002000921106     F* -73- ERR. M004D1 *- UTENTE OBBLIGATORIO                     -*
002100921106     F* -74- ERR. M004D1 *- DATA ERRATA                             -*
002200921106     F* -75- ERR. M004D1 *- ORA ERRATA                              -*
002300010511     F* -76- ERR. M004D1 *- INCONGRUENZA DATI                       -*
002400921103     F*                                                              *
002500921102     F*  99 - ERRORE GENERICO                                        *
002600921102?    F*
002700921102?    F*--------------------------------------------------------------*
002800921102?    F*  DATA BASE                                                   *
002900921102?    F*--------------------------------------------------------------*
003000921102     F*
003100921102?    F*------------
003200921106     FCMTSM04L  IF   E           K DISK
003300921106?    F*------------
003400921112     FCMLST01L  IF   E           K DISK
003500921103?    F*------------
003600921112     FAZORG01L  IF   E           K DISK
003700921102?    F*------------
003800921106     FCMZM04D   CF   E             WORKSTN
003900921106     D*
004000921106?    D*------------------------------------------------------------
004100921106?    D*  SCHIERE
004200921106?    D*------------------------------------------------------------
004300921106     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
004400921102      *---
004500930623     D                SDS
004600930623     D  SYSUTL               254    263
004700930322     D ZM06DS          DS
004800930322     D  M06DCR                 1      7  0
004900930322     D  M06FLG                 8      8
005000930322     D  M06FIL                 9     11  0
005100930322     D  M06LIS                12     21
005200930322     D  M06UTI                22     31
005300930322     D  M06URG                32     32
005400930322     D  M06DAT                33     38  0
005500930322     D  M06ORR                39     44  0
005600930322     D  M06STP                45     45
005700930322     D  M06MSG                46     46
005800930322     D  M06RIS                47     47
005900930322     D  M06URR                48     48
006000930322     D  M06MSR                49     49
006100930623     D  M06UTL                50     59
006200010503?     *
006300010503EDPMOD TBCMUS        E DS                  EXTNAME(TNTBCMUS)
006400010503  |  D  TBCFUN       E                     EXTFLD(TBCFUN)
006500010503  |  D  TBCUTI       E                     EXTFLD(TBCKE1)
006600010503  |  D  TBCDES       E                     EXTFLD(TBCUNI)
006700010511  |  D  TBCUBI       E                     EXTFLD(TBCUBICAZ)
006800010503  |  D  TBCRRN       E                     EXTFLD(TBCRELREC)
006900010503  |  D  TBCCMD       E                     EXTFLD(TBCCOMAND)
007000010503  |  D  TBCOPZ       E                     EXTFLD(TBCOPZIO)
007100010503  |  D  TBCRIC       E                     EXTFLD(TBCRICEZ)
007200010503  |  D  TBCERR       E                     EXTFLD(TBCESITO)
007300010511  |   *
007400010503EDPMOD KPJBA         E DS
007500010503?     *
007600921102?     *------------------------------------------------------------
007700921102?     *  FLUSSO PRINCIPALE
007800921102?     *------------------------------------------------------------
007900921102?     *
008000921102?    C                   EXSR      INIZIO
008100921106?    C                   EXSR      D1INZ
008200010511     C*
0083009211061    C     D1STAI        DOWEQ     'S'
008400010511     C*
008500921106     C* Richiedo i parametri solo per invio.
008600921106     C*
0087009211062    C     M04FLG        IFEQ      'I'
008800010511      * VISUALIZZO LE DECODIFICHE IN PRESENZA DI ERRORI.
008900010511     C     *IN99         IFEQ      *ON
009000010511     C                   SETOFF                                       99
009100010511     C                   WRITE     M004D1
009200010511     C                   SETON                                        99
009300010511     C                   ENDIF
009400010511      *
009500921106     C                   EXFMT     M004D1
009600921102?    C***
009700921106?    C* F12= RITORNO
009800921102?    C***
009900921106     C   KL              MOVE      'N'           D1STAI
010000921106     C*
010100921106     C*
010200921106?    C  NKL              EXSR      D1CTL
0103009211062-   C                   END
010400921102     C*
010500921113     C                   MOVE      'N'           M04SPE
010600921113     C  NKL
010700921113     CANN99              DO
010800921113?    C                   EXSR      GESINV
010900921113     C                   MOVE      'N'           D1STAI
011000921113     C                   MOVE      'S'           M04SPE
0111009211131-   C                   END
011200921102     C*
0113009211021-   C                   END
011400921102     C*
011500921102     C                   SETON                                            LR
011600921102?     *
011700921102?     *------------------------------------------------------------
011800921102?     *  INIZIALIZZAZIONE CAMPI DI LAVORO E DEFINIZIONE CHIAVI
011900921102?     *------------------------------------------------------------
012000921102?     *
012100921102     C*
012200921102?    C     INIZIO        BEGSR
012300921102     C*
012400921103     C     *ENTRY        PLIST
012500921106     C                   PARM                    M04DCR            7 0
012600921106     C                   PARM                    M04FLG            1
012700921106     C                   PARM                    M04FIL            3 0
012800921106     C                   PARM                    M04UTN           10
012900921106     C                   PARM                    M04URG            1
013000921106     C                   PARM                    M04MSG            1
013100921113     C                   PARM                    M04SPE            1
013200921102     C*
013300921110     C     *LIKE         DEFINE    ORGFIL        KFIL
013400921110     C*
013500921102     C                   ENDSR
013600921106?     *
013700921106?     *------------------------------------------------------
013800921106?     * D 1 I N Z   - INIZIALIZZO FORMARO M004D1
013900921106?     *------------------------------------------------------
014000921106?     *
014100921106?    C     D1INZ         BEGSR
014200921106     C*
014300921106     C     M04DCR        CHAIN     CMTSM04L                           30
014400921106     C                   MOVEL     TSMTXT        D1TXT
014500921106     C                   MOVE      M04FIL        D1FIL
0146009211111    C     D1FIL         IFEQ      *ZERO
014700921111     C                   MOVE      *BLANK        D1FIL
0148009211111-   C                   END
014900921106     C                   MOVE      *BLANK        D1LIS
015000921106     C                   MOVEL     M04UTN        D1UTI
015100921106     C                   MOVE      M04URG        D1URG
0152009211101    C     D1URG         IFEQ      *BLANK
015300921110     C                   MOVE      'N'           D1URG
0154009211101-   C                   END
015500941024     C*                    MOVE UDATE     D1DTR
015600921106     C                   TIME                    D1ORR
015700941024     C                   TIME                    W012             12 0
015800941024     C                   MOVE      W012          D1DTR
015900930226     C                   ADD       010000        D1ORR
016000930222     C                   MOVE      0000          D1ORR
016100921106     C                   MOVE      'N'           D1STP
016200930222     C                   MOVE      'N'           D1MSG
016300921106     C                   MOVE      'N'           D1RIS
016400921113     C                   MOVE      'N'           D1URR
0165009211101    C     D1URR         IFEQ      *BLANK
016600921110     C                   MOVE      'N'           D1URR
0167009211101-   C                   END
016800921106     C                   MOVE      'N'           D1MSR
016900921106     C*
017000921106     C                   MOVE      'S'           D1STAI            1
017100921106     C                   MOVE      '0'           *IN
017200010904     C* Proteggo il campo utente.
017300010904     C                   IF        M04Utn = *BLANK
017400010904     C                   EVAL      *IN32 = *OFF
017500010904     C                   ELSE
017600010904     C                   EVAL      *IN32 = *ON
017700010904     C                   ENDIF
017800010904     C*
0179009211131    C     M04FLG        IFEQ      'I'
018000921109     C                   WRITE     M004W1
0181009211131-   C                   END
018200921106     C*
018300921106     C                   ENDSR
018400921103?     *
018500921103?     *------------------------------------------------------
018600921106?     * D 1 C T L   - CONTROLLO FORMATO M004D1
018700921103?     *------------------------------------------------------
018800921103?     *
018900921106?    C     D1CTL         BEGSR
019000921103     C*
019100921103     C                   SETOFF                                       99
019200921103?    C*
019300921106?    C*                **  FILIALE         **
019400921103?    C*
0195009211121    C     D1FIL         IFNE      *ZERO
019600921112     C     D1FIL         ANDNE     *BLANK
019700921112     C                   MOVE      D1FIL         £FIL
019800921112?    C                   EXSR      CHKFIL
019900921112     C     £STS          COMP      'ERR'                                  70
020000921112     C                   MOVE      £FIL          D1FIL
020100921123C    C   99              GOTO      ENDD1
0202009211121-   C                   END
020300921106?    C*
020400921106?    C*                **  LISTA DI DISTR. **
020500921106?    C*
020600921106     C     '?'           SCAN      D1LIS                                  31
0207009211061    C   31              DO
020800921112     C                   SETON                                            99
020900921106     C                   CALL      'CMZM09R'
021000921106     C                   PARM      *BLANK        D1LIS
021100921106     C                   PARM      'R'           $STS              1
021200921123     C                   GOTO      ENDD1
0213009211061-   C                   END
021400921106     C*
0215009211061    C     D1LIS         IFNE      *BLANK
021600921112     C     D1LIS         CHAIN     CMLST01L                           71
0217009211061-   C                   END
021800921106?    C*
021900921106?    C*  CONTROLLI DI CONGRUENZA
022000921106?    C*
0221009211061    C     D1FIL         IFNE      *BLANK
022200921106     C     D1LIS         ANDNE     *BLANK
022300921123     C     D1FIL         ANDNE     '000'
022400921106     C     D1FIL         OREQ      *BLANK
022500921106     C     D1LIS         ANDEQ     *BLANK
022600921106     C                   SETON                                        72
0227009211061-   C                   END
022800921106?    C*
022900921106?    C*                **  UTENTE A CUI INVIARE  **
023000010502?    C*
023100010503EDPMOC     D1UTI         IFEQ      '?'
023200010503  |  C                   CLEAR                   KPJBU
023300010503  |  C                   CLEAR                   TBCMUS
023400010503  |  C                   MOVE      '1'           TBCFUN
023500010503  |  C                   MOVEL     TBCMUS        KPJBU
023600010503  |  C                   CALL      'TNTBCMUR'
023700010503  |  C                   PARM                    KPJBA
023800010503  |  C                   MOVEL     KPJBU         TBCMUS
023900010508  |  C                   SETON                                        99
024000010503  |  C     TBCRIC        IFEQ      'C'
024100010503  |  C                   CLEAR                   D1UTI
024200010503  |  C                   MOVEL     *ON           *INKL
024300010503  |  C                   GOTO      ENDD1
024400010503  |  C                   END
024500010503  |  C     TBCERR        IFEQ      *OFF
024600010503  |  C                   MOVEL     TBCUTI        D1UTI
024700010503  |  C                   END
024800010503EDPMOC                   END
024900010502      *
025000001020     C**         D1UTI     COMP *BLANK                   73
025100001020     C     D1UTI         IFEQ      *BLANK
025200001020     C                   MOVEL     *ON           *IN73
025300001020     C                   ELSE
025400010904     C* Controllo l'utente solo se non l'ho ricevuto come parametro.
025500010904     C* Se l'ho ricevuto come parametro significa che sto rispondendo
025600010904     C* ad un messaggio, quindi il controllo è inutile (anche perchè
025700010904     C* il campo è protetto).
025800010904     C                   IF        M04Utn = *BLANK
025900010904EDPMOC*
026000010511EDPMOC                   MOVE      D1FIL         PRMFIL
026100010511  |  C                   CALL      'CMZM08R'
026200010511  |  C                   PARM      D1UTI         PRMUTI           10
026300010511  |  C                   PARM                    PRMFIL            3 0
026400010511  |  C                   PARM      *BLANK        PRMESI            1
026500010511EDPMOC*
026600010511  |  C                   SELECT
026700010511  |  C     PRMESI        WHENEQ    '1'
026800010511  |  C                   SETON                                        73
026900010511  |  C     PRMESI        WHENEQ    '2'
027000010511  |  C                   SETON                                        76
027100010511  |  C                   ENDSL
027200010511EDPMOC*
027300010904     C                   ENDIF
027400010904EDPMOC*
027500001020     C                   ENDIF
027600921106?    C*
027700921110?    C*                **  DATA ORA INVIO  **
027800921106?    C*
027900921106     C                   Z-ADD     D1DTR         XDTGMA
028000921106     C                   Z-ADD     3             XDTSTA
028100921106?    C                   EXSR      XDATA
028200921106     C     XDTSTA        COMP      0                                    74
028300921106     C  N74              Z-ADD     XDTAMG        SAVDAT            6 0
028400921106     C*
028500921110     C     D1ORR         COMP      235959                             75
028600921106     C*
028700921103     C*
028800921106     C*
028900921106     C   70
029000921106     COR 71
029100921106     COR 72
029200921106     COR 73
029300921106     COR 74
029400010511     COR 75
029500010511     COR 76              SETON                                        99
029600921103     C*
029700921123     C     ENDD1         ENDSR
029800921112?     *
029900921112?     *----------------------------------------------------
030000921112?     * C H K F I L - CONTROLLI FILIALE
030100921112?     *----------------------------------------------------
030200921112?     *
030300921112?    C     CHKFIL        BEGSR
030400921112     C*
030500921112     C                   MOVE      *BLANK        £DES             25
030600921112     C                   MOVE      *BLANK        £STS              3
030700921112     C                   SETOFF                                           91
030800921112     C*
0309009211121    C     £FIL          IFNE      *BLANKS
031000921112     C     £FIL          ANDNE     *ZEROS
031100921112     C*
031200921112     C     '?'           SCAN      £FIL                                   37
0313009211122    C     *IN37         IFEQ      '1'
031400921112     C                   SETON                                            99
031500921112     C                   MOVE      *BLANK        £FIL              3
031600941125     C                   CALL      'TNSD19R'
031700921112     C                   PARM      *BLANK        §COD1             3
031800921112     C                   PARM      *BLANK        §TIP              1
031900921112     C                   PARM      *BLANK        §DES1            25
032000921112     C                   MOVEL     §COD1         £FIL
032100921112     C                   MOVEL     §DES1         £DES
0322009211122E   C                   ELSE
032300921112     C*
0324009211123    C     £FIL          IFGT      *ZEROS
032500921112     C                   MOVEL     £FIL          ORGFIL
032600921112     C     ORGFIL        CHAIN     AZORG01L                           91
0327009211123E   C                   ELSE
032800921112     C                   SETON                                        91
0329009211123-   C                   END
033000921112     C*
0331009211122-   C                   END
033200921112     C*
033300921112     C* CONTROLLO SE ANNULLATO
033400921112     C*
033500921112     C  N91ORGFVA        COMP      *BLANK                             9191
033600011122
033700011122     ** Accetto solo filiali, agenzie e la sede.
033800011122     C                   IF        NOT *IN91
033900011122     C                   EVAL      *IN91 = (OrgFAg = 'V' AND OrgFil <> 46)
034000011122     C                   ENDIF
034100011122
034200921112     C*
034300921112     C   91              MOVE      'ERR'         £STS
034400921112     C  N91              MOVEL     ORGDES        £DES
0345009211121-   C                   END
034600921112     C*
034700921112     C                   ENDSR
034800921106?     *
034900921106?     *------------------------------------------------------
035000921106?     * G E S I N V - GESTIONE INVIO
035100921106?     *------------------------------------------------------
035200921106?     *
035300921106?    C     GESINV        BEGSR
035400921106     C*
035500921110     C                   MOVEL     D1FIL         M06FIL
035600930322     C*                    CALL 'CMZM06R'
035700930322     C*                    PARM           M04DCR           DOC DA INVIARE
035800930322     C*                    PARM           M04FLG           I=INVIO S=SOLLECITO
035900930322     C*                    PARM           M06FIL  30       FILIALE A CUI INVIARE
036000930322     C*                    PARM           D1LIS            LISTA DI DISTRIBUZ.
036100930322     C*                    PARM           D1UTI            UTENTE A CUI INVIARE
036200930322     C*                    PARM           D1URG            URGENZA INVIO
036300930322     C*                    PARM           SAVDAT           DATA INVIO
036400930322     C*                    PARM           D1ORR            ORA  INVIO
036500930322     C*                    PARM           D1STP            OBBLIGO DI SATMPA
036600930322     C*                    PARM           D1MSG            MSG ALL'ARRIVO
036700930322     C*                    PARM           D1RIS            RISPOSTA OBBLIGATORIA
036800930322     C*                    PARM           D1URR            URGENZA RISPOSTA
036900930322     C*                    PARM           D1MSR            MSG ALL'ARRIVO RISP.
037000930322     C*
037100930322     C                   MOVE      M04DCR        M06DCR
037200930322     C                   MOVE      M04FLG        M06FLG
037300930322     C                   MOVE      D1LIS         M06LIS
037400930322     C                   MOVE      D1UTI         M06UTI
037500930322     C                   MOVE      D1URG         M06URG
037600930322     C                   MOVE      SAVDAT        M06DAT
037700930322     C                   MOVE      D1ORR         M06ORR
037800930322     C                   MOVE      D1STP         M06STP
037900930322     C                   MOVE      D1MSG         M06MSG
038000930322     C                   MOVE      D1RIS         M06RIS
038100930322     C                   MOVE      D1URR         M06URR
038200930322     C                   MOVE      D1MSR         M06MSR
038300930623     C                   MOVEL     SYSUTL        M06UTL
038400930322     C                   CALL      'CMZM06C'
038500930322     C                   PARM                    ZM06DS
038600921106     C*
038700921106     C                   ENDSR
038800921106?     *
038900921106?     *----------------------------------------------------*
039000921106?     * XDATA    INVERSIONE E CONTROLLO DATE               *
039100921106?     *----------------------------------------------------*
039200921106?     *
039300921106     C*
039400921106?    C     XDATA         BEGSR
039500921106     C*
039600921106     C     XDTSTA        CABGT     4             XDTERR
039700921106     C     XDTSTA        CABLT     1             XDTERR
039800921106     C     XDTSTA        CABGT     2             XDTEI
039900921106     C*
040000921106     C* -- GIRA DATA INTERNA IN ESTERNA
040100921106     C*
040200921106     C     XDTAMG        DIV       100           XDTMM             2 0
040300921106     C     XDTAMG        DIV       10000         XDTAA             2 0
040400921106     C                   MOVE      XDTAMG        XDTGG             2 0
040500921106     C                   MOVE      XDTAMG        XDTGMA            6 0
040600921106     C                   MOVEL     XDTGG         XDTGMA
040700921106     C                   MOVE      XDTAA         XDTGMA
040800921106     C                   GOTO      XDTCON
040900921106     C*
041000921106     C     XDTEI         TAG
041100921106     C*
041200921106     C* -- GIRA DATA ESTERNA IN INTERNA
041300921106     C*
041400921106     C     XDTGMA        DIV       100           XDTMM
041500921106     C     XDTGMA        DIV       10000         XDTGG
041600921106     C                   MOVE      XDTGMA        XDTAA
041700921106     C                   MOVE      XDTGMA        XDTAMG            6 0
041800921106     C                   MOVEL     XDTAA         XDTAMG
041900921106     C                   MOVE      XDTGG         XDTAMG
042000921106     C                   SUB       2             XDTSTA            1 0
042100921106     C*                                                    * PER TEST CONT
042200921106     C     XDTCON        TAG
042300921106     C     XDTSTA        CABNE     1             XDTOK
042400921106     C*
042500921106     C* -- CONTROLLA LA VALIDITA* DELLA DATA
042600921106     C*
042700921106     C     XDTGG         CABLT     1             XDTERR
042800921106     C     XDTMM         CABLT     1             XDTERR
042900921106     C     XDTMM         CABGT     12            XDTERR
043000921106     C     XDTAA         DIV       4             XDTRES            2 0
043100921106     C                   MVR                     XDTRES
043200921106     C*
043300921106     C     XDTRES        IFEQ      0
043400921106     C                   Z-ADD     29            XDTF(2)
043500921106     C                   ELSE
043600921106     C                   Z-ADD     28            XDTF(2)
043700921106     C                   END
043800921106     C*
043900921106     C     XDTGG         CABGT     XDTF(XDTMM)   XDTERR
044000921106     C*                                                    * FINE MESE
044100921106     C     XDTOK         TAG
044200921106     C*                                                    * FINE MESE
044300921106     C* -- FINE NORMALE
044400921106     C*                                                    * FINE MESE
044500921106     C                   SUB       XDTSTA        XDTSTA
044600921106     C                   GOTO      XDTFIN
044700921106     C*                                                    * FINE MESE
044800921106     C     XDTERR        TAG
044900921106     C*                                                    * FINE MESE
045000921106     C* -- DATA O RICHIESTA ERRATA
045100921106     C*                                                    * FINE MESE
045200921106     C                   Z-SUB     1             XDTSTA
045300921106     C*                                                    * FINE MESE
045400921106     C     XDTFIN        TAG
045500921106     C*                                                    * FINE MESE
045600921106     C* -- FINE ISTRUZIONI ROUTINE
045700921106     C*
045800921106     C                   ENDSR
045900921106     C********************************************************************
046000921106**  XDTF  X XDATA
046100921106312831303130313130313031
