000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300000601     FFNARB05L  IF   E           K DISK
000400000601     FFIGCA01L  IF   E           K DISK
000500000601     FFNDCT02L  IF   E           K DISK
000600000602     FFNEVB01L  IF   E           K DISK
000700000602     FFNAR401L  IF   E           K DISK
000800000602      *
000900000602     FTABEL00F  IF   E           K DISK
001000000601      *
001100000602     FFNCDE02L  UF A E           K DISK
001200000602      *
001300000601     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400000602     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)
001500000601      * -------
001600000601     D ARR             S              3  0 DIM(10)                              LNA ARRIVO
001700000601      * -------
001800000602     D CHIUSO          C                   CONST('CHIUSURA CONSEGNA')
001900000602     D FORZA           C                   CONST('FORZATA')
002000000602     D DELIV           C                   CONST('DELIVERY')
002100000602     D DIGITS          C                   CONST('1234567890')
002200000602      * ------------------------------------------------------
002300000601      *   PARAMETRI DI INPUT PER L'ELABORAZIONE:
002400000601      *    FILIALI CHE GESTISCONO L'EXPORT
002500000601      *     DA DATA SPEDIZIONE.
002600000602      *
002700000601     C                   EXSR      PARINP
002800000602      *--------------------------------------------------
002900000601      *     MAX 10 FILIALI
003000000601     C                   DO        10            Y                 3 0
003100000601      *--
003200000601      *  ESCE SE NON CI SONO PIÙ FILIALI
003300000601     C     ARR(Y)        IFEQ      *ZERO
003400000601     C                   LEAVE
003500000601     C                   ENDIF
003600000601      *
003700000601      *  SPEDIZIONI
003800000601     C                   Z-ADD     ARR(Y)        KLNA              3 0
003900000601     C     KLNA          CHAIN     FNARB05L                           21
004000000601      *
004100000601     C     *IN21         DOWEQ     *OFF
004200000601      *
004300000602      *  PER OGNI SPEDIZIONE
004400000602     C                   EXSR      SPEDIZ
004500000602      *
004600000601      *
004700000601     C     KLNA          READE     FNARB05L                               21
004800000601     C                   ENDDO
004900000601      *
005000000601     C                   ENDDO
005100000601      *------
005200950201     C                   SETON                                        LR
005300000601      *---------------------------------------------------------
005400000601      *   IMPOSTAI PUNTI DI ARRIVO (AD ORA VERONA E TORINO)
005500000601      *---------------------------------------------------------
005600000601     C     PARINP        BEGSR
005700000601      *
005800000601     C     KARB          KLIST
005900000601     C                   KFLD                    ARBAAS
006000000601     C                   KFLD                    ARBLNP
006100000601     C                   KFLD                    ARBNRS
006200000601     C                   KFLD                    ARBNSP
006300000602      *
006400000602     C     KCDE          KLIST
006500000602     C                   KFLD                    CDENDC
006600000602     C                   KFLD                    ARBAAS
006700000602     C                   KFLD                    ARBLNP
006800000602     C                   KFLD                    ARBNRS
006900000602     C                   KFLD                    ARBNSP
007000000602      *
007100000602     C     KAR4          KLIST
007200000602     C                   KFLD                    ARBAAS
007300000602     C                   KFLD                    ARBLNP
007400000602     C                   KFLD                    ARBNRS
007500000602     C                   KFLD                    ARBNSP
007600000602     C                   KFLD                    EFFE
007700000602      *
007800000602     C                   MOVE      'F'           EFFE              1
007900000602      *
008000000602     C     KTBL          KLIST
008100000602     C                   KFLD                    TBLKUT
008200000602     C                   KFLD                    TBLCOD
008300000602     C                   KFLD                    TBLKEY
008400000602      *
008500000602     C                   MOVE      '1'           TBLKUT
008600000602     C                   MOVE      '2A'          TBLCOD
008700000601      *
008800000601      *  IMPOSTA LA SCHIERA DELLE FILIALI ABILITATE ALLA
008900000601      *  SPEDIZIONE
009000000601     C                   CLEAR                   ARR
009100000601      *  OGNI FILIALE IN UNA SPECIFICA POSIZIONE DI SCHIERA
009200000601     C                   Z-ADD     190           ARR(1)                           1
009300000601     C                   Z-ADD     195           ARR(2)                           2
009400000602     C*****              Z-ADD                   ARR(3)                           3
009500000602     C*****              Z-ADD                   ARR(4)                           4
009600000602     C*****              Z-ADD                   ARR(5)                           5
009700000602     C*****              Z-ADD                   ARR(6)                           6
009800000602     C*****              Z-ADD                   ARR(7)                           7
009900000602     C*****              Z-ADD                   ARR(8)                           8
010000000602     C*****              Z-ADD                   ARR(9)                           9
010100000602     C*****              Z-ADD                   ARR(10)                          10
010200000601      *
010300000601      *  DATA DI SPEDIZIONE FINO A
010400000601     C                   Z-ADD     2000          AASPE             4 0
010500000601     C                   Z-ADD     0520          MGSPE             4 0
010600000601      *
010700000601      *    TESTATA DI STAMPA
010800000601     C                   EXCEPT    TESTA
010900000602     C                   EXCEPT    TESSYS
011000000601      *
011100000601     C                   ENDSR
011200000601      *---------------------------------------------------------
011300000602      *  PER OGNI SPEDIZIONE    ESEGUE CONTROLLI / STAMPA E AGGIORNA
011400000602      *---------------------------------------------------------
011500000602     C     SPEDIZ        BEGSR
011600000602      *
011700000602      * CONTROLLA CHE SIANO NON CONSEGNATI
011800000602      *  OSSIA CON DATA CONSEGNA A ZERO
011900000602     C     ARBDCM        IFEQ      *ZERO
012000000602      *
012100000602      * OCCORRE CONTROLLARE SOLO QUELLI SPEDITI OLTRE
012200000602      *   UNA CERTA DATA DI SPEDIZIONE    (PRIMA DEL 20/5/2000)
012300000602     C     ARBAAS        IFEQ      AASPE
012400000602     C     ARBMGS        ANDLE     MGSPE
012500000602      *
012600000602      * CONTROLLA CHE NON CI SIA UNA GIACENZA
012700000602     C     KARB          CHAIN     FIGCA01L                           22
012800000602     C     *IN22         IFEQ      *ON
012900000602      *
013000000602      * CONTROLLA CHE NON CI SIA UNA PRATICA DI DANNO
013100000602     C     KARB          CHAIN     FNDCT02L                           22
013200000602     C     *IN22         IFEQ      *ON
013300000602      *
013400000602      * LA SPEDIZIONE È QUELLA CHE CERCAVAMO
013500000602      *  REPERISCE IL NUMERO DI RIFERIMENTO NUMERICO
013600000602     C     KAR4          CHAIN     FNAR401L                           22
013700000602      *  SE C'E' LO IMPOSTA (ATTENZIONE AL CAMPO NUMERICO)
013800000602     C                   MOVE      *ZEROS        CAMP12           12
013900000602     C  N22              MOVEL     AR4NOT        CAMP12
014000000602     C     DIGITS        CHECK     CAMP12                                 24
014100000602      *  SOLO SE NUMERICO IL RIFERIMENTO
014200000602     C                   CLEAR                   NUM12
014300000602     C  N22
014400000602     CANN24              MOVE      CAMP12        NUM12            12 0
014500000602      *
014600000602      * IMPORTANTE PER LA SCRITTURA DI CERTI CAMPI A SEGUITO
014700000602      *  PRENDE I DATI DELL'ULTIMO EVENTO.
014800000602     C     KARB          SETGT     FNEVB01L
014900000602      *                                                                 *----*
015000000602     C     KARB          READPE    FNEVB01L                               23
015100000602      *                                                                 *----*
015200000602      *  ATTENZIONE ALL'INDICATORE 23 UTILIZZATO PER STAMPA
015300000602      *      E AGGIORNAMENTO FILE.
015400000602      *
015500000602      *
015600000602      *
015700000602      * SE È UN RESO
015800000602     C     ARBFBR        IFEQ      'S'
015900000602      *
016000000602      * STAMPA IL DETTAGLIO
016100000602     C   OF              EXCEPT    TESTA
016200000602     C                   EXCEPT    DETTA
016300000602      *
016400000602     C                   ELSE
016500000602      *
016600000602      *   PER NON RESO
016700000602     C                   MOVE      *BLANK        TBLKEY
016800000602     C                   MOVEL     EVBCEV        TBLKEY
016900000602     C     KTBL          CHAIN     TABEL00F                           22
017000000602     C                   MOVEL     *BLANK        DESCEV           20
017100000602     C  N22              MOVEL     TBLUNI        DESCEV
017200000602      * STAMPA IL DETTAGLIO
017300000602     C   OA              EXCEPT    TESSYS
017400000602     C                   EXCEPT    DETSYS
017500000602      *
017600000602      * SOLO PER GLI EVENTI CON
017700000602      *    CODICE 'MIC' --> MERCE IN CONSEGNA
017800000602      *  SCRIVE/AGGGIORNA IL FILE X CHIUSURA CONSEGNA FORZATA
017900000602     C  N23EVBCEV        IFEQ      'MIC'
018000000602     C                   EXSR      WRICDE
018100000602     C                   END
018200000602      *
018300000602     C                   END
018400000602      *
018500000602     C                   ENDIF
018600000602      *
018700000602     C                   ENDIF
018800000602      *
018900000602     C                   END
019000000602     C                   END
019100000602      *
019200000602     C                   ENDSR
019300000602      *---------------------------------------------------------
019400000602      *  SCRITTURA FILE FNCDE02L
019500000602      *---------------------------------------------------------
019600000602     C     WRICDE        BEGSR
019700000602      *
019800000602      * LA CHIAVE È IMPOSTATA TRAMITE IL FNARB00F
019900000602     C                   MOVE      *ZERO         CDENDC
020000000602      *   CONTROLLA COMUNQUE SE ESISTE
020100000602     C     KCDE          CHAIN     FNCDE02L                           22
020200000602      *
020300000602     C   22              CLEAR                   FNCDE000
020400000602      *  IMPOSTA SEMPRE QUESTI CAMPI SIA IN WRITE/UPDAT
020500000602     C                   Z-ADD     ARBLNA        CDELNA
020600000602     C                   Z-ADD     NUM12         CDERMN
020700000602     C                   Z-ADD     EVBDEV        CDEDCM
020800000602     C                   Z-ADD     EVBHEV        CDEHMC
020900000602     C     CHIUSO        CAT       FORZA:1       CDENO1
021000000602     C                   MOVE      '03C'         CDESTS
021100000602     C                   MOVEL     *BLANK        CDEMOT
021200000602     C                   MOVEL     DELIV         CDEMOT
021300000602      *
021400000602     C     *IN22         IFEQ      *ON
021500000602     C                   Z-ADD     ARBAAS        CDEAAS
021600000602     C                   Z-ADD     ARBLNP        CDELNP
021700000602     C                   Z-ADD     ARBNRS        CDENRS
021800000602     C                   Z-ADD     ARBNSP        CDENSP
021900000602     C                   WRITE     FNCDE000
022000000602     C                   ELSE
022100000602     C                   UPDATE    FNCDE000
022200000602     C                   END
022300000602      *
022400000602     C                   ENDSR
022500000602      *---------------------------------------------------------
022600000601     OQSYSPRT   E            TESTA            01
022700000601     O                                           10 'TNVRABR'
022800000601     O                       *DATE               20 '  /  /    '
022900000601     O                                           60 'ELENCO SPEDIZIONI EXPORT'
023000000601     O                                         +  2 'CHECK X NON CONSEGNATE'
023100000602     O                                          126 'PAG.'
023200000602     O                       PAGE               132
023300000601     O          E            TESTA       2
023400000601     O                                           15 'DATA SPED.'
023500000602     O                                         +  4 'RIFERIMENTO SPEDIZIONE'
023600000602     O                                         +  5 'RESO'
023700000602     O                                         +  3 'RIF.MITT.NR.'
023800000602     O                                           95 'DATA E ORA ULTIMO'
023900000602     O                                         +  1 'EVENTO'
024000000602     O          E            DETTA       1
024100000601     O                       ARBAAS               9
024200000601     O                                           10 '/'
024300000601     O                       ARBMGS              15 '  /  '
024400000601     O                       ARBAAS            +  6
024500000601     O                       ARBLNP            +  1
024600000601     O                       ARBNRS            +  1
024700000602     O                       ARBNSP        Z   +  1
024800000602     O                       ARBFBR            +  7
024900000602     O                       NUM12         Z   +  5
025000000602     O              N23      EVBDEV              90 '    /  /  '
025100000602     O              N23      EVBHEV            +  2 '  :  :  '
025200000602     O              N23      EVBCEV            +  2
025300000602     O              N23      DESCEV            +  2
025400000602      *
025500000602     OSYSPRT    E            TESSYS           01
025600000602     O                                           10 'TNVRABR'
025700000602     O                       *DATE               20 '  /  /    '
025800000602     O                                           60 'ELENCO SPEDIZIONI EXPORT'
025900000602     O                                         +  2 'CHECK X NON CONSEGNATE'
026000000602     O                                          126 'PAG.'
026100000602     O                       PAGE1              132
026200000602     O          E            TESSYS      2
026300000602     O                                           15 'DATA SPED.'
026400000602     O                                         +  4 'RIFERIMENTO SPEDIZIONE'
026500000602     O                                         +  5 'RESO'
026600000602     O                                         +  3 'RIF.MITT.NR.'
026700000602     O                                           95 'DATA E ORA ULTIMO'
026800000602     O                                         +  1 'EVENTO'
026900000602     O          E            DETSYS      1
027000000602     O                       ARBAAS               9
027100000602     O                                           10 '/'
027200000602     O                       ARBMGS              15 '  /  '
027300000602     O                       ARBAAS            +  6
027400000602     O                       ARBLNP            +  1
027500000602     O                       ARBNRS            +  1
027600000602     O                       ARBNSP        Z   +  1
027700000602     O                       ARBFBR            +  7
027800000602     O                       ARBRMN        Z   +  5
027900000602     O              N23      EVBDEV              90 '    /  /  '
028000000602     O              N23      EVBHEV            +  2 '  :  :  '
028100000602     O              N23      EVBCEV            +  2
028200000602     O              N23      DESCEV            +  2
