000100950529     H*------------------------------------------------------------------------*
000200000614     H* STATISTICA MOVIMENTI - ESEGUE                                          *
000300990527     H*------------------------------------------------------------------------*
000400990527     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE                                                              *
000700000000     F*------------------------------------------------------------------------*
000800990609     FSISDR01L  IF   E           K DISK    USROPN
000900990609     FSISDR02L  IF   E           K DISK    USROPN
001000990609     F                                     RENAME(SISDR000:SISDR2)
001100160421     FTEBDG01L  IF   E           K DISK    usropn
001200000000     FAZORG01L  IF   E           K DISK
001300000000     FAZORG02L  IF   E           K DISK
001400000000     F                                     RENAME(AZORG:AZORG2)
001500000000     FTABEL00F  IF   E           K DISK
001600020115     FTISE46P   O    E             PRINTER
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE                                                                *
001900000000     D*------------------------------------------------------------------------*
002000000000     D*------------------
002100000614     D* CODICI/DESCRIZIONI FILIALI
002200000000     D*------------------
002300971124     D FIL             S              3  0 DIM(500)
002400971124     D DFIL            S             20    DIM(500)
002500971204     D FL1             S              1    DIM(500)
002600020325     D NTX             S              3    DIM(500)                             *NETWORK
002700000000     D*------------------
002800000000     D* DECODIFICHE FASCE DI PESO
002900000000     D*------------------
003000020610     D FPE             S             14    DIM(10)
003100000000     D*------------------
003200000000     D* MESI DA ELABORARE
003300000000     D*------------------
003400000000     D MES             S              2  0 DIM(12)
003500000000     D*------------------
003600000000     D* ANNI DEI MESI DA ELABORARE
003700950529     D*------------------
003800990527     D ANN             S              4  0 DIM(12)
003900000000     D*------------------
004000000000     D* ANNO INIZIALE E ANNO FINALE ELABORAZIONE SALDI CORRENTI
004100000000     D*------------------
004200990527     D AIF             S              4  0 DIM(2)
004300000000     D*------------------
004400000000     D* ANNO INIZIALE E ANNO FINALE ELABORAZIONE SALDI PRECEDENTI
004500000000     D*------------------
004600990527     D PIF             S              4  0 DIM(2)
004700000000     D*------------------
004800000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
004900000000     D*------------------
005000000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
005100000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/N)
005200000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
005300000000     D*------------------
005400000000     D* DECODIFICA PARAMETRI FILTRO
005500000000     D*------------------
005600950602     D PAR             S             15    DIM(20) CTDATA PERRCD(1)
005700000000     D*------------------
005800000000     D* DECODIFICA MESI DELL'ANNO
005900000000     D*------------------
006000000000     D DMM             S              9    DIM(12) CTDATA PERRCD(1)
006100000000     D*------------------
006200000000     D* DECODIFICA RIGHE DI STAMPA
006300000000     D*------------------
006400000000     D DRI             S             14    DIM(10) CTDATA PERRCD(1)
006500000000     D*------------------
006600020610     D* SCHIERA MULTIPLA PER TOTALI GENERALI           - 10 FASCE -
006700000000     D*------------------
006800000000     D*------------------
006900020610     D* SCHIERA MULTIPLA PER TOTALI DIVISIONE          - 10 FASCE -
007000000000     D*------------------
007100000000     D*------------------
007200020610     D* SCHIERA MULTIPLA PER TOTALI AREA               - 10 FASCE -
007300000000     D*------------------
007400000000     D*------------------
007500020610     D* SCHIERA MULTIPLA PER TOTALI FILIALE            - 10 FASCE -
007600000000     D*------------------
007700000000     D*------------------
007800020610     D* SCHIERA MULTIPLA PER TOTALI FILIALE DETTAGLIO  - 10 FASCE -
007900000000     D*------------------
008000000000     D*------------------
008100000000     D* SCHIERA MULTIPLA PER STAMPA TOTALI
008200000000     D*------------------
008300000000     D*------------------
008400000000     D* SCHIERE DI LAVORO PER TOTALI A FINE PAGINA
008500000000     D*------------------
008600000000     D SPW             S             13  0 DIM(12)                              SPEDIZIONI
008700000000     D SBW             S             13  0 DIM(12)                              SPEDIZIONI BUDGET
008800000000     D SAW             S             13  0 DIM(12)                              SPEDIZIONI ANNO PREC.
008900000000     D QTW             S             15  1 DIM(12)                              KILOGRAMMI
009000000000     D QBW             S             15  1 DIM(12)                              KILOGRAMMI BUDGET
009100000000     D QAW             S             15  1 DIM(12)                              KILOGRAMMI ANNO PREC.
009200000000     D CLW             S             13  0 DIM(12)                              COLLI
009300000000     D CAW             S             13  0 DIM(12)                              COLLI ANNO PRECEDENTE
009400000000     D VLW             S             15  3 DIM(12)                              VOLUME
009500000000     D VAW             S             15  3 DIM(12)                              VOLUME ANNO PRECEDENTE
009600000000     D CBW             S             11  0 DIM(12)                              COLLI BUDGET
009700000000     D VBW             S             13  3 DIM(12)                              VOLUME BUDGET
009800000000     D*------------------
009900000000     D* SCHIERE ULTIME DA PORTARE IN STAMPA
010000000000     D*------------------
010100000000     D $VA             S             17  3 DIM(12)                              VALORE
010200000000     D $BD             S             17  3 DIM(12)                              VALORE BUDGET
010300000000     D $AP             S             17  3 DIM(12)                              VALORE ANNO PRECEDENTE
010400000000     D $VS             S             17  3 DIM(12)                              VALORE SUPER. RAPPORTO
010500000000     D $VI             S             17  3 DIM(12)                              VALORE INFER. RAPPORTO
010600000000     D $PB             S              4  1 DIM(12)                              % BUDGET
010700000000     D $PA             S              4  1 DIM(12)                              % ANNO PRECEDENTE
010800000000     D $RP             S              9  3 DIM(12)                              RAPPORTO
010900000000     D*------------------
011000000000     D* TABELLE CAPICONTO
011100000000     D*------------------
011200000000     D*------------------------------------------------------------------------*
011300000000     D* INPUT                                                                  *
011400000000     D*------------------------------------------------------------------------*
011500000000     D KPJBA         E DS
011600000000     D  PROFIL                 1     10                                         UTENTE
011700000000     D*------------------
011800000000     D* NUMERO DEI BYTE IN PACKED:
011900000000     D*------------------
012000000000     D* SPEDIZIONI  (11:2) + 1 =  6,5  /  (6 X 12) = 72
012100000000     D* KILOGRAMMI  (13:2) + 1 =  7,5  /  (7 X 12) = 84
012200000000     D* COLLI       (11:2) + 1 =  6,5  /  (6 X 12) = 72
012300000000     D* VOLUME      (13:2) + 1 =  7,5  /  (7 X 12) = 84
012400000000     D*------------------
012500020610     D* DS MULTIPLA FASCE DI PESO PER TOTALI GENERALI     - 10 FASCE X 12 MESI -
012600000000     D*------------------
012700020610     D FPGEN           DS                  OCCURS(10) INZ
012800000000     D  SPG                    1     72P 0                                      SPE*12
012900000000     D                                     DIM(12)                              SPEDIZIONI
013000000000     D  SBG                   73    144P 0                                      %BG*12
013100000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
013200000000     D  SAG                  145    216P 0                                      %AP*12
013300000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
013400000000     D  QTG                  217    300P 1                                      QUI*12
013500000000     D                                     DIM(12)                              KILOGRAMMI
013600000000     D  QBG                  301    384P 1                                      %BG*12
013700000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
013800000000     D  QAG                  385    468P 1                                      %AP*12
013900000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
014000000000     D  CLG                  469    540P 0                                      COL*12
014100000000     D                                     DIM(12)                              COLLI
014200000000     D  CAG                  541    612P 0                                      %AP*12
014300000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
014400000000     D  VLG                  613    696P 3                                      VOL*12
014500000000     D                                     DIM(12)                              VOLUME
014600000000     D  VAG                  697    780P 3                                      %AP*12
014700000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
014800000000     D  CBG                  781    852P 0                                      %BD*12
014900000000     D                                     DIM(12)                              COLLI BUDGET
015000000000     D  VBG                  853    936P 3                                      %BD*12
015100000000     D                                     DIM(12)                              VOLUME BUDGET
015200000000     D*------------------
015300020610     D* DS MULTIPLA FASCE DI PESO PER DIVISIONE           - 10 FASCE X 12 MESI -
015400000000     D*------------------
015500020610     D FPDIV           DS                  OCCURS(10) INZ
015600000000     D  SPD                    1     72P 0                                      SPE*12
015700000000     D                                     DIM(12)                              SPEDIZIONI
015800000000     D  SBD                   73    144P 0                                      %BG*12
015900000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
016000000000     D  SAD                  145    216P 0                                      %AP*12
016100000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
016200000000     D  QTD                  217    300P 1                                      QUI*12
016300000000     D                                     DIM(12)                              KILOGRAMMI
016400000000     D  QBD                  301    384P 1                                      %BG*12
016500000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
016600000000     D  QAD                  385    468P 1                                      %AP*12
016700000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
016800000000     D  CLD                  469    540P 0                                      COL*12
016900000000     D                                     DIM(12)                              COLLI
017000000000     D  CAD                  541    612P 0                                      %AP*12
017100000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
017200000000     D  VLD                  613    696P 3                                      VOL*12
017300000000     D                                     DIM(12)                              VOLUME
017400000000     D  VAD                  697    780P 3                                      %AP*12
017500000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
017600000000     D  CBD                  781    852P 0                                      %BD*12
017700000000     D                                     DIM(12)                              COLLI BUDGET
017800000000     D  VBD                  853    936P 3                                      %BD*12
017900000000     D                                     DIM(12)                              VOLUME BUDGET
018000000000     D*------------------
018100020610     D* DS MULTIPLA FASCE DI PESO PER AREA                - 10 FASCE X 12 MESI -
018200000000     D*------------------
018300020610     D FPARE           DS                  OCCURS(10) INZ
018400000000     D  SPA                    1     72P 0                                      SPE*12
018500000000     D                                     DIM(12)                              SPEDIZIONI
018600000000     D  SBA                   73    144P 0                                      %BG*12
018700000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
018800000000     D  SAA                  145    216P 0                                      %AP*12
018900000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
019000000000     D  QTA                  217    300P 1                                      QUI*12
019100000000     D                                     DIM(12)                              KILOGRAMMI
019200000000     D  QBA                  301    384P 1                                      %BG*12
019300000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
019400000000     D  QAA                  385    468P 1                                      %AP*12
019500000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
019600000000     D  CLA                  469    540P 0                                      COL*12
019700000000     D                                     DIM(12)                              COLLI
019800000000     D  CAA                  541    612P 0                                      %AP*12
019900000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
020000000000     D  VLA                  613    696P 3                                      VOL*12
020100000000     D                                     DIM(12)                              VOLUME
020200000000     D  VAA                  697    780P 3                                      %AP*12
020300000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
020400000000     D  CBA                  781    852P 0                                      %BD*12
020500000000     D                                     DIM(12)                              COLLI BUDGET
020600000000     D  VBA                  853    936P 3                                      %BD*12
020700000000     D                                     DIM(12)                              VOLUME BUDGET
020800000000     D*------------------
020900020610     D* DS MULTIPLA FASCE DI PESO PER FILIALE             - 10 FASCE X 12 MESI -
021000000000     D*------------------
021100020610     D FPFIL           DS                  OCCURS(10) INZ
021200000000     D  SPF                    1     72P 0                                      SPE*12
021300000000     D                                     DIM(12)                              SPEDIZIONI
021400000000     D  SBF                   73    144P 0                                      %BG*12
021500000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
021600000000     D  SAF                  145    216P 0                                      %AP*12
021700000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
021800000000     D  QTF                  217    300P 1                                      QUI*12
021900000000     D                                     DIM(12)                              KILOGRAMMI
022000000000     D  QBF                  301    384P 1                                      %BG*12
022100000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
022200000000     D  QAF                  385    468P 1                                      %AP*12
022300000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
022400000000     D  CLF                  469    540P 0                                      COL*12
022500000000     D                                     DIM(12)                              COLLI
022600000000     D  CAF                  541    612P 0                                      %AP*12
022700000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
022800000000     D  VLF                  613    696P 3                                      VOL*12
022900000000     D                                     DIM(12)                              VOLUME
023000000000     D  VAF                  697    780P 3                                      %AP*12
023100000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
023200000000     D  CBF                  781    852P 0                                      %BD*12
023300000000     D                                     DIM(12)                              COLLI BUDGET
023400000000     D  VBF                  853    936P 3                                      %BD*12
023500000000     D                                     DIM(12)                              VOLUME BUDGET
023600000000     D*------------------
023700020610     D* DS MULTIPLA FASCE DI PESO PER FILIALE DETTAGLIO   - 10 FASCE X 12 MESI -
023800000000     D*------------------
023900020610     D FPDET           DS                  OCCURS(10) INZ
024000000000     D  SPT                    1     72P 0                                      SPE*12
024100000000     D                                     DIM(12)                              SPEDIZIONI
024200000000     D  SAT                   73    144P 0                                      %AP*12
024300000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
024400000000     D  QTT                  145    228P 1                                      QUI*12
024500000000     D                                     DIM(12)                              KILOGRAMMI
024600000000     D  QAT                  229    312P 1                                      %AP*12
024700000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
024800000000     D  CLT                  313    384P 0                                      COL*12
024900000000     D                                     DIM(12)                              COLLI
025000000000     D  CAT                  385    456P 0                                      %AP*12
025100000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
025200000000     D  VLT                  457    540P 3                                      VOL*12
025300000000     D                                     DIM(12)                              VOLUME
025400000000     D  VAT                  541    624P 3                                      %AP*12
025500000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
025600000000     D*------------------
025700020610     D* DS MULTIPLA FASCE DI PESO DA PORTARE IN STAMPA    - 10 FASCE X 12 MESI -
025800000000     D*------------------
025900020610     D FPSTA           DS                  OCCURS(10) INZ
026000000000     D  SPS                    1     72P 0                                      SPE*12
026100000000     D                                     DIM(12)                              SPEDIZIONI
026200000000     D  SBS                   73    144P 0                                      %BG*12
026300000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
026400000000     D  SAS                  145    216P 0                                      %AP*12
026500000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
026600000000     D  QTS                  217    300P 1                                      QUI*12
026700000000     D                                     DIM(12)                              KILOGRAMMI
026800000000     D  QBS                  301    384P 1                                      %BG*12
026900000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
027000000000     D  QAS                  385    468P 1                                      %AP*12
027100000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
027200000000     D  CLS                  469    540P 0                                      COL*12
027300000000     D                                     DIM(12)                              COLLI
027400000000     D  CAS                  541    612P 0                                      %AP*12
027500000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
027600000000     D  VLS                  613    696P 3                                      VOL*12
027700000000     D                                     DIM(12)                              VOLUME
027800000000     D  VAS                  697    780P 3                                      %AP*12
027900000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
028000000000     D  CBS                  781    852P 0                                      %BD*12
028100000000     D                                     DIM(12)                              COLLI BUDGET
028200000000     D  VBS                  853    936P 3                                      %BD*12
028300000000     D                                     DIM(12)                              VOLUME BUDGET
028400000000     D*------------------
028500000000     D* PARAMETRI FILTRO
028600000000     D*------------------
028700990527     D PARAM           DS
028800990527     D  PARDAT                 1      6  0
028900990527     D  ANNINI                 1      4  0
029000990527     D  MESINI                 5      6  0
029100990527     D  PARDA2                 7     12  0
029200990527     D  ANNFIN                 7     10  0
029300990527     D  MESFIN                11     12  0
029400990527     D  PARMPA                13     13
029500990527     D  PARFIL                14     16  0
029600990527     D  PARARE                17     19  0
029700990527     D  PARDIV                20     20
029800990527     D  PARSER                21     21
029900990527     D  PARPOR                22     22
030000990527     D  PARSPE                23     23
030100990527     D  PARPRO                24     24
030200990527     D  PARDET                25     25
030300990527     D  PARIOE                27     27
030400990527     D*------------------
030500020114     D* DS COMPOSIZIONE DATA(6) - TEBDG
030600990527     D*------------------
030700990527     D                 DS
030800990527     D  TBGANN                 1      4  0
030900990527     D  TBGMES                 5      6  0
031000990527     D  TBGDAT                 1      6  0
031100000000     D*------------------
031200000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
031300000000     D*------------------
031400000000     D DS2W          E DS
031500000216     D*---
031600000703     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA
031700000216     D*---
031800000216     D OG143         E DS
031900020325     D*-------------------
032000020325     D* DS DEFINIZIONE TABELLA "NETWORK"
032100020325     D*-------------------
032200020325     D DNTW          E DS
032300020325     D*-------------------
032400020325     D* DS RICHIAMO SCELTA TABELLE
032500020325     D*-------------------
032600020325     D TIBS02        E DS                  EXTNAME(TIBS02DS)
032700090605
032800090605     d ds5e          e ds
032900160421      *_______________________________________________________________
033000160421     D Cmd             S             49                                         QCAEXEC
033100160421     D Cmd1            S             49    dim(1) ctdata perrcd(1)              QCAEXEC
033200160421     D Cmd2            S             49    dim(1) ctdata perrcd(1)              QCAEXEC
033300100413     C*------------------------------------------------------------------------*
033400950529     C*?MAIN LINE
033500000000     C*------------------------------------------------------------------------*
033600000000     C*
033700000000     C* STAMPA PARAMETRI FILTRO
033800000000     C                   EXSR      STAPAR
033900000000     C*
034000000000     C* POSIZIONAMENTO SUL FILE ORGANIGRAMMI E PRIMA LETTURA
034100000000     C                   EXSR      SETFIL
034200000000     C*
034300000000     C* CICLO FINO A FINE FILE ORGANIGRAMMI
034400000000DO  1C     $FINE         DOWEQ     '0'
034500000000     C*
034600000000     C* OPERAZIONI PER NUOVA DIVISIONE
034700000000     C                   EXSR      NEWDIV
034800000000     C*
034900000000     C* CICLO FINO A ROTTURA DI DIVISIONE
035000000000DO  2C     $FINE         DOWEQ     '0'
035100000000     C     ORGFL3        ANDEQ     DEPDIV
035200000000     C*
035300000000     C* OPERAZIONI PER NUOVA AREA
035400000000     C                   EXSR      NEWARE
035500000000     C*
035600000000     C* CICLO FINO A ROTTURA DI AREA
035700000000DO  3C     $FINE         DOWEQ     '0'
035800000000     C     ORGFL3        ANDEQ     DEPDIV
035900000000     C     ORGCAR        ANDEQ     DEPARE
036000000000     C*
036100000000     C* OPERAZIONI PER NUOVA FILIALE
036200000000     C                   EXSR      NEWFIL
036300000000     C*
036400000000     C* CICLO LETTURA FILIALI DETTAGLIO
036500971124DO  4C     1             DO        500           F                 3 0
036600000000IF  5C     FIL(F)        IFGT      *ZEROS
036700000000     C*
036800000000     C* OPERAZIONI PER NUOVA FILIALE DETTAGLIO
036900000000     C                   EXSR      NEWDET
037000000000     C*
037100000000     C* LETTURA SALDI AA INIZIALE, E AA FINALE SOLO SE DIVERSO DALL'INIZIALE
037200000000     C                   Z-ADD     1             J                 1 0
037300000703DO  7C                   DO        2             J
037400000000     C                   MOVEL     '0'           $FIN1                          *REIMPOSTO FINE FILE
037500000000     C*
037600000000     C* SECONDO GIRO SOLO SE ANNO FINALE DIVERSO DALL' INIZIALE (PIENO)
037700000703IF  8C     AIF(J)        IFNE      *ZEROS
037800000000     C*
037900000000     C* POSIZIONAMENTO SUL FILE SALDI ANNO CORRENTE E PRIMA LETTURA
038000000000     C                   EXSR      SETFI1
038100000000     C*
038200000000     C* CICLO FINO A FINE FILE SALDI O ROTTURA FILIALE
038300000703DO  9C     $FIN1         DOWEQ     '0'
038400000000     C*
038500000000     C* MEMORIZZA I SALDI CORRENTI DELLA FILIALE LETTA DALL'ORGANIGRAMMA
038600000000     C                   EXSR      MEMSAL
038700000000     C*
038800000000     C* LETTURA RECORD SALDI SUCCESSIVO
038900000000     C                   EXSR      LETFI1
039000000000     C*
039100000703E   9C                   ENDDO
039200160421     c                   ENDIF
039300000000     C*
039400000703E   7C                   ENDDO
039500000000     C*
039600000000     C* CALCOLA I SALDI DELL' ANNO PRECEDENTE DELLA FILIALE DETTAGLIO
039700000000     C                   EXSR      CALANP
039800000000     C*
039900000000     C* SE RICHIESTO DETTAGLIO STAMPO FILIALE
040000000703IF  7C     PARDET        IFEQ      'S'                                          *SE MEMORIZZATI SALDI
040100000000     C     PASDET        ANDEQ     '1'
040200000000     C*
040300000000     C* IMPOSTA STAMPA TOTALI FILIALE DETTAGLIO
040400000000     C                   EXSR      STADET
040500000703E   7C                   ENDIF
040600000703E   6C                   ENDIF
040700000000     C*
040800000000E   4C                   ENDDO                                                  *FIL/DET SUCESSIVA
040900000000     C*
041000000000     C* CALCOLA I VALORI DEL BUDGET
041100000000IF  4C     PASFIL        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
041200000000     C                   EXSR      CALBDG
041300000000E   4C                   ENDIF
041400000000     C*
041500000000     C* IMPOSTA STAMPA TOTALI FILIALE
041600000000IF  4C     PASFIL        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
041700000000     C                   EXSR      STAFIL
041800000000E   4C                   ENDIF
041900000000     C*
042000000000     C* LETTURA RECORD ORGANIGRAMMI SUCCESSIVO
042100000000     C                   EXSR      LETFIL
042200000000     C*
042300000000E   3C                   ENDDO
042400000000     C*
042500000000     C* IMPOSTA STAMPA TOTALI AREA
042600000000IF  3C     PASARE        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
042700000000     C                   EXSR      STAARE
042800000000E   3C                   ENDIF
042900000000     C*
043000000000E   2C                   ENDDO
043100000000     C*
043200000000     C* IMPOSTA STAMPA TOTALI DIVISIONE
043300000000IF  2C     PASDIV        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
043400000000     C                   EXSR      STADIV
043500000000E   2C                   ENDIF
043600000000     C*
043700000000E   1C                   ENDDO
043800000000     C*
043900000000     C* IMPOSTA STAMPA TOTALI GENERALI
044000000000IF  1C     PASGEN        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
044100000000     C                   EXSR      STAGEN
044200000000E   1C                   ENDIF
044300000000     C*
044400000000     C* STAMPA RIGA DI FINE PROSPETTO
044500020115     C                   WRITE     SE46FI
044600000000     C*
044700000000     C* C H I U S U R A   F I L E
044800000000     C*
044900000000IF  1C     PARMPA        IFEQ      'P'
045000990609     C                   CLOSE     SISDR01L                                     *SALDI PARTENZE
045100000000     C*
045200000000X   1C                   ELSE
045300990609     C                   CLOSE     SISDR02L                                     *SALDI ARRIVI
045400000000     C*
045500000000E   1C                   ENDIF
045600000000     C*
045700000000     C                   SETON                                        LR
045800000000     C*------------------------------------------------------------------------*
045900000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ORGANIGRAMMI
046000000000     C*------------------------------------------------------------------------*
046100000000     C     SETFIL        BEGSR
046200000000     C*
046300000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE ORGANIGRAMMI
046400000000     C                   MOVEL     PARDIV        KORDIV
046500000000     C     KEYORG        SETLL     AZORG02L                           99        *FINE ARCHIVIO
046600000000     C*
046700000000IF  1C     *IN99         IFEQ      '1'
046800000000     C                   MOVEL     '1'           $FINE
046900000000X   1C                   ELSE
047000000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
047100000000E   1C                   ENDIF
047200000000     C*
047300000000     C                   ENDSR
047400000000     C*------------------------------------------------------------------------*
047500000000     C*?LETFIL - LETTURA PROSSIMO RECORD FILE ORGANIGRAMMI
047600000000     C*------------------------------------------------------------------------*
047700000000     C     LETFIL        BEGSR
047800000000     C*
047900000000     C* LEGGO FINO A:
048000000000     C*     - FINE FILE
048100000000     C*     - TROVATO RECORD VALIDO
048200000000     C                   MOVEL     'N'           WRECOK
048300000000DO  1C     $FINE         DOWEQ     '0'
048400000000     C     WRECOK        ANDEQ     'N'
048500000000     C*
048600000000     C* LETTURE SUCCESSIVE ALLA PRIMA
048700000000     C* SE SELEZIONATA LA DIVISIONE LEGGO ANCHE PER UGUALE
048800000000IF  2C     PARDIV        IFGT      *BLANKS
048900000000     C     KEYORG        READE     AZORG02L                               99
049000000000X   2C                   ELSE
049100000000     C                   READ      AZORG02L                               99
049200000000E   2C                   ENDIF
049300000703     C*
049400000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
049500000000     C                   MOVEL     '1'           $FINE
049600000000X   2C                   ELSE
049700000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
049800000000E   2C                   ENDIF
049900000000     C*
050000000000E   1C                   ENDDO
050100000000     C*
050200000000     C                   ENDSR
050300000000     C*------------------------------------------------------------------------*
050400000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD ORGANIGRAMMA
050500000000     C*------------------------------------------------------------------------*
050600000000     C     CHKREC        BEGSR
050700000000     C*
050800000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
050900000703     C*
051000000703     C* IMPOSTAZIONI
051100000703     C                   CLEAR                   OG143
051200000703     C                   MOVEL     ORGDE3        OG143
051300000000     C*
051400000000     C* ESCLUDO LE FILIALI ANNULLATE
051500000000IF  1C     ORGFVA        IFNE      *BLANKS
051600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
051700000000E   1C                   ENDIF
051800000000     C*
051900000000     C* ESCLUDO LE VOCI VARIE
052000000000IF  1C     ORGFAG        IFEQ      'V'
052100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
052200000000E   1C                   ENDIF
052300000000     C*
052400000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
052500000000IF  1C     PARARE        IFNE      *ZEROS
052600000000     C     ORGCAR        ANDNE     PARARE
052700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
052800000000E   1C                   ENDIF
052900000000     C*
053000000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
053100000000IF  1C     PARFIL        IFNE      *ZEROS
053200000000     C     ORGFIL        ANDNE     PARFIL
053300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
053400000000E   1C                   ENDIF
053500000000     C*
053600000000     C                   ENDSR
053700000000     C*------------------------------------------------------------------------*
053800000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
053900000000     C*------------------------------------------------------------------------*
054000000000     C     NEWDIV        BEGSR
054100000000     C*
054200000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
054300000000     C                   MOVEL     ORGFL3        DEPDIV
054400000000     C*
054500000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
054600000000     C                   EXSR      DECDIV
054700000000     C*
054800000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
054900000000     C                   MOVEL     '0'           PASDIV
055000000000     C*
055100000000     C* AZZERA I TOTALI DELLA DIVISIONE
055200000000DO  1C                   DO        10            A                 2 0
055300000000     C     A             OCCUR     FPDIV
055400000000     C                   CLEAR                   SPD
055500000000     C                   CLEAR                   SBD
055600000000     C                   CLEAR                   SAD
055700000000     C                   CLEAR                   QTD
055800000000     C                   CLEAR                   QBD
055900000000     C                   CLEAR                   QAD
056000000000     C                   CLEAR                   CLD
056100000000     C                   CLEAR                   CAD
056200000000     C                   CLEAR                   VLD
056300000000     C                   CLEAR                   VAD
056400000000     C                   CLEAR                   CBD
056500000000     C                   CLEAR                   VBD
056600000000E   1C                   ENDDO
056700000000     C*
056800000000     C                   ENDSR
056900000000     C*------------------------------------------------------------------------*
057000000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
057100000000     C*------------------------------------------------------------------------*
057200000000     C     NEWARE        BEGSR
057300000000     C*
057400000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
057500000000     C                   MOVEL     ORGCAR        DEPARE
057600000000     C*
057700000000     C* DECODIFICA IL CODICE AREA
057800000000     C                   EXSR      DECARE
057900000000     C*
058000000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
058100000000     C                   MOVEL     '0'           PASARE
058200000000     C*
058300000000     C* AZZERA I TOTALI DELL'AREA
058400000000DO  1C                   DO        10            A
058500000000     C     A             OCCUR     FPARE
058600000000     C                   CLEAR                   SPA
058700000000     C                   CLEAR                   SBA
058800000000     C                   CLEAR                   SAA
058900000000     C                   CLEAR                   QTA
059000000000     C                   CLEAR                   QBA
059100000000     C                   CLEAR                   QAA
059200000000     C                   CLEAR                   CLA
059300000000     C                   CLEAR                   CAA
059400000000     C                   CLEAR                   VLA
059500000000     C                   CLEAR                   VAA
059600000000     C                   CLEAR                   CBA
059700000000     C                   CLEAR                   VBA
059800000000E   1C                   ENDDO
059900000000     C*
060000000000     C                   ENDSR
060100000000     C*------------------------------------------------------------------------*
060200000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA DEI SALDI ANNO CORRENTE
060300000000     C*------------------------------------------------------------------------*
060400000000     C     SETFI1        BEGSR
060500000000     C*
060600000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE SALDI
060700000000     C                   Z-ADD     AIF(J)        KSDANN                         *AA INIZIALE O FINALE
060800000000     C                   Z-ADD     ORGFIL        KSDFIL                         *FILIALE
060900000000     C                   Z-ADD     FIL(F)        KSDFIT                         *FILIALE DETTAGLIO
061000000000     C*
061100000000     C* MI POSIZIONO CON IL MESE INIZIALE SE ANNI UGUALI O PRIMO ANNO DIVERSO,
061200000000     C* OPPURE CON IL MESE = 01 SE GLI ANNI SONO DIVERSI E STO ELABORANDO IL 2°
061300000000IF  1C     TIPANN        IFEQ      'U'
061400000000     C     J             OREQ      1
061500000000     C                   Z-ADD     MESINI        KSDMES                         *MESE INIZIALE
061600000000E   1C                   ELSE
061700000000     C                   Z-ADD     01            KSDMES                         *MESE 01
061800000000E   1C                   ENDIF
061900000000     C* PARTENZE
062000000000IF  1C     PARMPA        IFEQ      'P'
062100990609     C     KEYSDR        SETLL     SISDR01L                           99        *FINE ARCHIVIO
062200000000X   1C                   ELSE
062300000000     C* ARRIVI
062400990609     C     KEYSDR        SETLL     SISDR02L                           99        *FINE ARCHIVIO
062500000000E   1C                   ENDIF
062600000000     C*
062700000000IF  1C     *IN99         IFEQ      '1'
062800000000     C                   MOVEL     '1'           $FIN1
062900000000X   1C                   ELSE
063000000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
063100000000E   1C                   ENDIF
063200000000     C*
063300000000     C                   ENDSR
063400000000     C*------------------------------------------------------------------------*
063500000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI ANNO CORRENTE
063600000000     C*------------------------------------------------------------------------*
063700000000     C     LETFI1        BEGSR
063800000000     C*
063900000000     C* LEGGO FINO A:
064000000000     C*     - FINE FILE
064100000000     C*     - TROVATO RECORD VALIDO
064200000000     C                   MOVEL     'N'           WRECOK
064300000000DO  1C     $FIN1         DOWEQ     '0'
064400000000     C     WRECOK        ANDEQ     'N'
064500000000     C*
064600000000     C* LETTURE SUCCESSIVE ALLA PRIMA (ANNO + FILIALE + DETTAGLIO)
064700000000IF  1C     PARMPA        IFEQ      'P'
064800990609     C     KEYSD2        READE     SISDR01L                               99
064900000000X   1C                   ELSE
065000990609     C     KEYSD2        READE     SISDR02L                               99
065100000000E   1C                   ENDIF
065200000000     C*
065300000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
065400000000     C                   MOVEL     '1'           $FIN1
065500000000X   2C                   ELSE
065600000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
065700000000E   2C                   ENDIF
065800000000     C*
065900000000E   1C                   ENDDO
066000000000     C*
066100000000     C                   ENDSR
066200000000     C*------------------------------------------------------------------------*
066300000000     C*?CHKRE1 - CONTROLLA VALIDITA' DEI SALDI ANNO CORRENTE/PRECEDENTE
066400000000     C*------------------------------------------------------------------------*
066500000000     C     CHKRE1        BEGSR
066600000000     C*
066700000000     C* °ANNO INIZIALE = ANNO FINALE: IL MESE LETTO DEVE ESSERE MINORE/UGUALE
066800000000     C*  DEL MESE FINALE RICHIESTO A VIDEO
066900000000     C*  ANNO INIZIALE <> ANNO FINALE: IL MESE DELL'ANNO INIZIALE DEVE ESSERE
067000000000     C*  MIN/UG DI 12; IL MESE DELL'ANNO FINALE MIN/UG DEL MESE FINALE RICHIESTO
067100000000     C*
067200000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
067300000000     C*
067400000000     C* CONTROLLO MESE DEL RECORD SALDI LETTO°
067500000000IF  1C     TIPANN        IFEQ      'U'                                          *ANNI UGUALI
067600000000     C     J             OREQ      2                                            *ANNI DIVERSI MA 2°
067700000000     C*
067800000000IF  2C     SDRMES        IFGT      MESFIN
067900000000     C                   MOVEL     '1'           $FIN1                          *ESCO DAL CICLO CORR.
068000000000     C                   MOVEL     '1'           $FIN2                          *ESCO DAL CICLO PREC.
068100000000E   2C                   ENDIF
068200000000E   1C                   ENDIF
068300000000     C*
068400000000     C* SE TIPO SERVIZIO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
068500000000IF  1C     PARSER        IFNE      *BLANKS
068600000000IF  2C     PARSER        IFNE      SDRTSP
068700000000     C                   MOVEL     'N'           WRECOK
068800000000E   2C                   ENDIF
068900000000E   1C                   ENDIF
069000000000     C*
069100000000     C* SE TIPO PORTO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
069200000000IF  1C     PARPOR        IFNE      *BLANKS
069300000000IF  2C     SDRTPO        IFNE      PARPOR
069400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
069500000000E   2C                   ENDIF
069600000000E   1C                   ENDIF
069700000000     C*
069800000000     C* ESCLUDO LE BOLLE PER CUI NON VALGONO LE SPEDIZIONI
069900000000     C                   Z-ADD     1             G
070000000000     C                   SETOFF                                           99
070100000000     C     SDRRBL        LOOKUP    FLAB(G)                                99
070200000000IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
070300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
070400000000X   1C                   ELSE
070500000000IF  2C     FSP(G)        IFEQ      'N'                                          BOLLA CON SPED.VALIDE
070600000000     C                   MOVEL     'N'           WRECOK
070700000000E   2C                   ENDIF
070800000000E   1C                   ENDIF
070900000000     C*
071000000000     C* SE TIPO SPEDIZIONE DIVERSO DA QUELLO RICHIESTO, ESCLUDO
071100000000IF  1C*          PARSPE    IFNE *BLANKS
071200000000IF  2C*          SDRRBL    IFNE PARSPE
071300000000     C*                    MOVEL'N'       WRECOK           *RECORD NON VALIDO
071400000000E   2C*                    ENDIF
071500000000E   1C*                    ENDIF
071600000000     C*
071700000000     C* SE TIPO PRODOTTO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
071800000000IF  1C     PARPRO        IFNE      *BLANKS
071900000000IF  2C     SDRTPR        IFNE      PARPRO
072000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
072100000000E   2C                   ENDIF
072200000000E   1C                   ENDIF
072300971204     C*
072400971204     C* REPERISCE IL FLAG ITALIA / ESTERO
072500971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
072600971204     C                   MOVEL     *BLANKS       DLPFL1
072700971204     C                   MOVEL     *BLANKS       DLAFL1
072800971204     C                   Z-ADD     1             I
072900971204     C     SDRFLP        LOOKUP    FIL(I)                                 99
073000971204IF  2C     *IN99         IFEQ      *ON                                          *TROVATO
073100971204     C                   MOVEL     FL1(I)        DLPFL1
073200971204E   2C                   ENDIF
073300971204     C                   Z-ADD     1             I
073400971204     C     SDRFLA        LOOKUP    FIL(I)                                 99
073500971204IF  2C     *IN99         IFEQ      *ON                                          *TROVATO
073600971204     C                   MOVEL     FL1(I)        DLAFL1
073700971204E   2C                   ENDIF
073800971204E   1C                   ENDIF
073900000218     C*
074000000614     C* SE RICHIESTA STATISTICA ITALIA, ESCLUDE BOLLE ESTERO (ITA->ITA)
074100971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
074200971204IF  2C     PARIOE        IFEQ      'I'
074300971204IF  3C     DLPFL1        IFNE      *BLANKS
074400971204     C     DLAFL1        ORNE      *BLANKS
074500950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
074600971204E   3C                   ENDIF
074700971204E   2C                   ENDIF
074800971204E   1C                   ENDIF
074900000614     C*
075000950529     C* SE RICHIESTA STATISTICA IMPORT, ESCLUDE BOLLE CON LNP ITALIA (EST->ITA)
075100971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
075200971204IF  2C     PARIOE        IFEQ      'M'
075300000218IF  3C     DLPFL1        IFNE      *BLANKS
075400000218     C     DLAFL1        ANDEQ     *BLANKS
075500000218     C                   ELSE
075600950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
075700971204E   3C                   ENDIF
075800971204E   2C                   ENDIF
075900971204E   1C                   ENDIF
076000950529     C*
076100950529     C* SE RICHIESTA STATISTICA EXPORT, ESCLUDE BOLLE CON LNA ITALIA (ITA->EST)
076200971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
076300971204IF  2C     PARIOE        IFEQ      'X'
076400000228IF  3C     DLAFL1        IFEQ      *BLANKS
076500950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
076600971204E   3C                   ENDIF
076700971204E   2C                   ENDIF
076800971204E   1C                   ENDIF
076900950529     C*
077000000000     C                   ENDSR
077100000000     C*------------------------------------------------------------------------*
077200000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
077300000000     C*------------------------------------------------------------------------*
077400000000     C     NEWFIL        BEGSR
077500000000     C*
077600000000     C* REIMPOSTO FINE FILE SALDI CORRENTI/PRECEDENTI/BUDGET
077700000000     C                   MOVEL     '0'           $FIN1                          FINE SALDI CORRENTI
077800000000     C                   MOVEL     '0'           $FIN2                          FINE SALDI PRECEDENTI
077900000000     C                   MOVEL     '0'           $FIN3                          FINE VALORI BUDGET
078000000000     C*
078100000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI E BUDGET
078200000000     C                   MOVEL     '0'           PASFIL                         *SALDI
078300000000     C                   MOVEL     '0'           NOBDG                          *BUDGET
078400000000     C*
078500000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
078600000000     C                   Z-ADD     ORGFIL        DEPFIL
078700000000     C*
078800000000     C* DECODIFICA IL CODICE FILIALE
078900160302     C                   MOVEL     *BLANKS       LAVNTX            3
079000000000     C                   EXSR      DECFIL
079100000703     C*
079200000703     C* MEMORIZZA IL NETWORK DELLA NUOVA FILIALE
079300160302     C                   MOVEL     LAVNTX        DEPNTF            3            *NETWORK
079400000000     C*
079500000000     C* AZZERA I TOTALI DELLA FILIALE
079600000000DO  1C                   DO        10            A
079700000000     C     A             OCCUR     FPFIL
079800000000     C                   CLEAR                   SPF
079900000000     C                   CLEAR                   SBF
080000000000     C                   CLEAR                   SAF
080100000000     C                   CLEAR                   QTF
080200000000     C                   CLEAR                   QBF
080300000000     C                   CLEAR                   QAF
080400000000     C                   CLEAR                   CLF
080500000000     C                   CLEAR                   CAF
080600000000     C                   CLEAR                   VLF
080700000000     C                   CLEAR                   VAF
080800000000     C                   CLEAR                   CBF
080900000000     C                   CLEAR                   VBF
081000000000E   1C                   ENDDO
081100000000     C*
081200000000     C                   ENDSR
081300000000     C*------------------------------------------------------------------------*
081400000000     C*?NEWDET - OPERAZIONI PER NUOVA FILIALE DETTAGLIO
081500000000     C*------------------------------------------------------------------------*
081600000000     C     NEWDET        BEGSR
081700000000     C*
081800000000     C* REIMPOSTO FINE FILE SALDI CORRENTI/PRECEDENTI
081900000000     C                   MOVEL     '0'           $FIN1                          FINE SALDI CORRENTI
082000000000     C                   MOVEL     '0'           $FIN2                          FINE SALDI PRECEDENTI
082100000000     C*
082200000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI FILIALE DETTAGLIO
082300000000     C                   MOVEL     '0'           PASDET
082400000703     C*
082500000703     C* MEMORIZZA IL NETWORK DELLA NUOVA FILIALE DETTAGLIO
082600160302     C                   MOVEL     NTX(F)        DEPNTT            3            *NETWORK
082700000000     C*
082800000000     C* AZZERA I TOTALI DELLA FILIALE
082900000000DO  1C                   DO        10            A
083000000000     C     A             OCCUR     FPDET
083100000000     C                   CLEAR                   SPT
083200000000     C                   CLEAR                   SAT
083300000000     C                   CLEAR                   QTT
083400000000     C                   CLEAR                   QAT
083500000000     C                   CLEAR                   CLT
083600000000     C                   CLEAR                   CAT
083700000000     C                   CLEAR                   VLT
083800000000     C                   CLEAR                   VAT
083900000000E   1C                   ENDDO
084000000000     C*
084100000000     C                   ENDSR
084200000000     C*------------------------------------------------------------------------*
084300000000     C*?MEMSAL - MEMORIZZA I SALDI CORRENTI
084400000000     C*------------------------------------------------------------------------*
084500000000     C     MEMSAL        BEGSR
084600000000     C*
084700000000     C                   MOVEL     SDRFPE        FASCIA            2 0
084800000000     C                   Z-ADD     1             MM                2 0
084900000000     C*
085000000000     C* RICERCA DELLA POSIZIONE IN SCHIERA DELL'ANNO/MESE CHE STO ELABORANDO
085100000000     C                   SETOFF                                           28
085200000000     C     SDRMES        LOOKUP    MES(MM)                                28
085300000000     C*
085400000000     C* SE MESE TROVATO, ADDIZIONO
085500000000IF  1C     *IN28         IFEQ      *ON
085600000000     C*
085700000000     C* FASCIA DI PESO PER FILIALE DETTAGLIO (SOLO SE RICHIESTO)
085800000000     C     FASCIA        OCCUR     FPDET
085900950529     C                   MOVEL     '1'           PASDET                         *PASSATO MEMORIZZAZ.
086000000000     C                   ADD       SDRNSP        SPT(MM)                        *SPEDIZIONI
086100000000     C                   ADD       SDRPKG        QTT(MM)                        *KILOGRAMMI
086200000000     C                   ADD       SDRNCO        CLT(MM)                        *COLLI
086300000000     C                   ADD       SDRVOL        VLT(MM)                        *VOLUME IN MC
086400000000     C*
086500000000     C* FASCIA DI PESO PER FILIALE
086600000000     C     FASCIA        OCCUR     FPFIL
086700950529     C                   MOVEL     '1'           PASFIL                         *PASSATO MEMORIZZAZ.
086800950529     C                   MOVEL     '1'           PASARE
086900950529     C                   MOVEL     '1'           PASDIV
087000950529     C                   MOVEL     '1'           PASGEN
087100000000     C                   ADD       SDRNSP        SPF(MM)                        *SPEDIZIONI
087200000000     C                   ADD       SDRPKG        QTF(MM)                        *KILOGRAMMI
087300000000     C                   ADD       SDRNCO        CLF(MM)                        *COLLI
087400000000     C                   ADD       SDRVOL        VLF(MM)                        *VOLUME IN MC
087500000000     C*
087600000000     C* FASCIA DI PESO PER AREA
087700000000     C     FASCIA        OCCUR     FPARE
087800000000     C                   ADD       SDRNSP        SPA(MM)
087900000000     C                   ADD       SDRPKG        QTA(MM)
088000000000     C                   ADD       SDRNCO        CLA(MM)
088100000000     C                   ADD       SDRVOL        VLA(MM)
088200000000     C*
088300000000     C* FASCIA DI PESO PER DIVISIONE
088400000000     C     FASCIA        OCCUR     FPDIV
088500000000     C                   ADD       SDRNSP        SPD(MM)
088600000000     C                   ADD       SDRPKG        QTD(MM)
088700000000     C                   ADD       SDRNCO        CLD(MM)
088800000000     C                   ADD       SDRVOL        VLD(MM)
088900000000     C*
089000000000     C* FASCIA DI PESO PER TOTALI GENERALI
089100000000     C     FASCIA        OCCUR     FPGEN
089200000000     C                   ADD       SDRNSP        SPG(MM)
089300000000     C                   ADD       SDRPKG        QTG(MM)
089400000000     C                   ADD       SDRNCO        CLG(MM)
089500000000     C                   ADD       SDRVOL        VLG(MM)
089600000000E   1C                   ENDIF
089700000000     C*
089800000000     C                   ENDSR
089900000000     C*------------------------------------------------------------------------*
090000000000     C*?CALANP - CALCOLA I SALDI DELL' ANNO PRECEDENTE
090100000000     C*------------------------------------------------------------------------*
090200000000     C     CALANP        BEGSR
090300000000     C*
090400000000     C* LETTURA SALDI X AA INIZIALE, E X AA FINALE SOLO SE DIVERSO DALL'INIZIALE
090500000000     C                   Z-ADD     1             J
090600000000DO  1C                   DO        2             J
090700000000     C                   MOVEL     '0'           $FIN2                          *REIMPOSTO FINE FILE
090800000000     C*
090900000000     C* SECONDO GIRO SOLO SE ANNO FINALE DIVERSO DALL' INIZIALE (PIENO)
091000000000IF  2C     PIF(J)        IFNE      *ZEROS
091100000000     C*
091200000000     C* POSIZIONAMENTO SUI SALDI DELL' ANNO PRECEDENTE E PRIMA LETTURA
091300000000     C                   EXSR      SETFI2
091400000000     C*
091500000000     C* FINCHE FINE SALDI DELLA FILIALE
091600000000DO  3C     $FIN2         DOWEQ     '0'
091700000000     C*
091800000000     C* MEMORIZZA SALDI ANNO PRECEDENTE
091900000000     C                   EXSR      MEMANP
092000000000     C*
092100000000     C* LETTURA PROSSIMO RECORD SALDI ANNO PRECEDENTE
092200000000     C                   EXSR      LETFI2
092300000000E   3C                   ENDDO
092400000000E   2C                   ENDIF
092500000000E   1C                   ENDDO
092600000000     C*
092700000000     C                   ENDSR
092800000000     C*------------------------------------------------------------------------*
092900000000     C*?SETFI2 - POSIZIONAMENTO E PRIMA LETTURA SALDI ANNO PRECEDENTE
093000000000     C*------------------------------------------------------------------------*
093100000000     C     SETFI2        BEGSR
093200000000     C*
093300000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI ANNO PRECEDENTE
093400000000     C                   Z-ADD     PIF(J)        KSDANN                         *ANNO
093500000000     C                   Z-ADD     ORGFIL        KSDFIL                         *FILIALE
093600000000     C                   Z-ADD     FIL(F)        KSDFIT                         *FILIALE DETTAGLIO
093700000000     C*
093800000000     C* MI POSIZIONO CON IL MESE INIZIALE SE ANNI UGUALI O PRIMO ANNO DIVERSO,
093900000000     C* OPPURE CON IL MESE = 01 SE GLI ANNI SONO DIVERSI E STO ELABORANDO IL 2°
094000000000IF  1C     TIPANN        IFEQ      'U'
094100000000     C     J             OREQ      1
094200000000     C                   Z-ADD     MESINI        KSDMES                         *MESE INIZIALE
094300000000E   1C                   ELSE
094400000000     C                   Z-ADD     01            KSDMES                         *MESE 01
094500000000E   1C                   ENDIF
094600000000     C*
094700000000     C* POSIZIONAMENTO E PRIMA LETTURA (ANNO INIZIALE - 1)
094800000000IF  1C     PARMPA        IFEQ      'P'
094900000000     C* PARTENZE
095000990609     C     KEYSDR        SETLL     SISDR01L                           99
095100000000X   1C                   ELSE
095200000000     C* ARRIVI
095300990609     C     KEYSDR        SETLL     SISDR02L                           99
095400000000E   1C                   ENDIF
095500000000     C*
095600000000IF  1C     *IN99         IFEQ      '1'
095700000000     C                   MOVEL     '1'           $FIN2
095800000000X   1C                   ELSE
095900000000     C                   EXSR      LETFI2                                       *LETTURA RECORD
096000000000E   1C                   ENDIF
096100000000     C*
096200000000     C                   ENDSR
096300000000     C*------------------------------------------------------------------------*
096400000000     C*?LETFI2 - LETTURA PROSSIMO RECORD SALDI ANNO PRECEDENTE
096500000000     C*------------------------------------------------------------------------*
096600000000     C     LETFI2        BEGSR
096700000000     C*
096800000000     C* LEGGO FINO A:
096900000000     C*     - FINE FILE
097000000000     C*     - TROVATO RECORD VALIDO
097100000000     C                   MOVEL     'N'           WRECOK
097200000000DO  1C     $FIN2         DOWEQ     '0'
097300000000     C     WRECOK        ANDEQ     'N'
097400000000     C*
097500000000     C* LETTURE SUCCESSIVE ALLA PRIMA (ANNO + FILIALE + DETTAGLIO)
097600000000IF  2C     PARMPA        IFEQ      'P'
097700990609     C     KEYSD2        READE     SISDR01L                               99
097800000000X   2C                   ELSE
097900990609     C     KEYSD2        READE     SISDR02L                               99
098000000000E   2C                   ENDIF
098100000000     C*
098200000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
098300000000     C                   MOVEL     '1'           $FIN2
098400000000X   2C                   ELSE
098500000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
098600000000E   2C                   ENDIF
098700000000     C*
098800000000E   1C                   ENDDO
098900000000     C*
099000000000     C                   ENDSR
099100000000     C*------------------------------------------------------------------------*
099200000000     C*?MEMANP - MEMORIZZA I SALDI DELL' ANNO PRECEDENTE
099300000000     C*------------------------------------------------------------------------*
099400000000     C     MEMANP        BEGSR
099500000000     C*
099600000000     C                   MOVEL     SDRFPE        FASCIA
099700000000     C                   Z-ADD     1             MM
099800000000     C*
099900000000     C* RICERCA DELLA POSIZIONE IN SHIERA DELL'ANNO/MESE CHE STO ELABORANDO
100000000000     C                   SETOFF                                           28
100100000000     C     SDRMES        LOOKUP    MES(MM)                                28
100200000000     C*
100300000000     C* SE MESE TROVATO, ADDIZIONO
100400000000IF  1C     *IN28         IFEQ      *ON
100500000000     C*
100600000000     C* FASCIA DI PESO PER FILIALE DETTAGLIO (SOLO SE RICHIESTO)
100700000000     C     FASCIA        OCCUR     FPDET
100800000000     C                   ADD       SDRNSP        SAT(MM)                        *SPEDIZIONI
100900000000     C                   ADD       SDRPKG        QAT(MM)                        *KILOGRAMMI
101000000000     C                   ADD       SDRNCO        CAT(MM)                        *COLLI
101100000000     C                   ADD       SDRVOL        VAT(MM)                        *VOLUME IN MC
101200000000     C*
101300000000     C* FASCIA DI PESO PER FILIALE
101400000000     C     FASCIA        OCCUR     FPFIL
101500000000     C                   ADD       SDRNSP        SAF(MM)                        *SPEDIZIONI
101600000000     C                   ADD       SDRPKG        QAF(MM)                        *KILOGRAMMI
101700000000     C                   ADD       SDRNCO        CAF(MM)                        *COLLI
101800000000     C                   ADD       SDRVOL        VAF(MM)                        *VOLUME IN MC
101900000000     C*
102000000000     C* FASCIA DI PESO PER AREA
102100000000     C     FASCIA        OCCUR     FPARE
102200000000     C                   ADD       SDRNSP        SAA(MM)
102300000000     C                   ADD       SDRPKG        QAA(MM)
102400000000     C                   ADD       SDRNCO        CAA(MM)
102500000000     C                   ADD       SDRVOL        VAA(MM)
102600000000     C*
102700000000     C* FASCIA DI PESO PER DIVISIONE
102800000000     C     FASCIA        OCCUR     FPDIV
102900000000     C                   ADD       SDRNSP        SAD(MM)
103000000000     C                   ADD       SDRPKG        QAD(MM)
103100000000     C                   ADD       SDRNCO        CAD(MM)
103200000000     C                   ADD       SDRVOL        VAD(MM)
103300000000     C*
103400000000     C* FASCIA DI PESO PER TOTALI GENERALI
103500000000     C     FASCIA        OCCUR     FPGEN
103600000000     C                   ADD       SDRNSP        SAG(MM)
103700000000     C                   ADD       SDRPKG        QAG(MM)
103800000000     C                   ADD       SDRNCO        CAG(MM)
103900000000     C                   ADD       SDRVOL        VAG(MM)
104000000000E   1C                   ENDIF
104100000000     C*
104200000000     C                   ENDSR
104300000000     C*------------------------------------------------------------------------*
104400000000     C*?CALBDG - CALCOLA I VALORI DEL BUDGET
104500000000     C*------------------------------------------------------------------------*
104600000000     C     CALBDG        BEGSR
104700000000     C*
104800000000     C* POSIZIONAMENTO SUL FILE BUDGET E PRIMA LETTURA
104900000000     C                   EXSR      SETFI3
105000000000     C*
105100000000     C* FINCHE' FINE FILE BUDGET O ROTTURA FILIALE
105200000000DO  1C     $FIN3         DOWEQ     '0'
105300000000     C*
105400000000     C* MEMORIZZA VALORI BUDGET
105500000000     C                   EXSR      MEMBDG
105600000000     C*
105700000000     C* LETTURA PROSSIMO RECORD BUDGET
105800000000     C                   EXSR      LETFI3
105900000000E   1C                   ENDDO
106000000000     C*
106100000000     C                   ENDSR
106200000000     C*------------------------------------------------------------------------*
106300000000     C*?SETF13 - POSIZIONAMENTO E PRIMA LETTURA FILE BUDGET
106400000000     C*------------------------------------------------------------------------*
106500000000     C     SETFI3        BEGSR
106600000000     C*
106700000000     C* SE SELEZ.TI TIPO SERVIZIO/PORTO/PRODOTTO NON LEGGO I VALORI DEL BUDGET
106800000000     C* PERCHE' QUESTI SONO PREVISTI SOLO PER FILIALE (NON NEL DETTAGLIO)
106900000000IF  1C     PARSER        IFNE      *BLANKS
107000000000     C     PARPOR        ORNE      *BLANKS
107100000000     C     PARPRO        ORNE      *BLANKS
107200000000     C                   MOVEL     '1'           $FIN3                          *USCITA CICLO
107300000000     C                   MOVEL     '1'           NOBDG                          *NO LETTURA BUDGET
107400000000X   1C                   ELSE
107500000000     C                   MOVEL     '0'           NOBDG                          *SI LETTURA BUDGET
107600000000E   1C                   ENDIF
107700000000     C*
107800000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE BUDGET (FILIALE+ANNO+MESE)
107900000000     C* SOLO SE RICHIESTO TUTTI I SERVIZI/SPEDIZIONI/PRODOTTI
108000000000IF  1C     $FIN3         IFEQ      '0'
108100000000     C                   Z-ADD     DEPFIL        KBDFIL
108200000000     C                   Z-ADD     ANNINI        KBDANN
108300000000     C                   Z-ADD     MESINI        KBDMES
108400960115     C                   MOVEL     PARIOE        KBDIOE
108500960108     C*
108600020114     C     KEYBDG        SETLL     TEBDG01L                           99        *FINE ARCHIVIO
108700000000     C*
108800000000IF  2C     *IN99         IFEQ      '1'
108900000000     C                   MOVEL     '1'           $FIN3
109000000000X   2C                   ELSE
109100000000     C                   EXSR      LETFI3                                       *LETTURA RECORD
109200000000E   2C                   ENDIF
109300000000E   1C                   ENDIF
109400000000     C*
109500000000     C                   ENDSR
109600000000     C*------------------------------------------------------------------------*
109700000000     C*?LETF13 - LETTURA PROSSIMO RECORD BUDGET
109800000000     C*------------------------------------------------------------------------*
109900000000     C     LETFI3        BEGSR
110000000000     C*
110100000000     C* LEGGO FINO A:
110200000000     C*     - FINE FILE O ROTTURA FILIALE
110300000000     C*     - TROVATO RECORD VALIDO
110400000000     C                   MOVEL     'N'           WRECOK
110500000000DO  1C     $FIN3         DOWEQ     '0'
110600000000     C     WRECOK        ANDEQ     'N'
110700000000     C*
110800020114     C     KBDFIL        READE     TEBDG01L                               99
110900000000     C*
111000000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
111100000000     C                   MOVEL     '1'           $FIN3
111200000000X   2C                   ELSE
111300000000     C                   EXSR      CHKRE3                                       *CONTROLLO RECORD
111400000000E   2C                   ENDIF
111500000000     C*
111600000000E   1C                   ENDDO
111700000000     C*
111800000000     C                   ENDSR
111900000000     C*------------------------------------------------------------------------*
112000000000     C*?CHKRE3 - CONTROLLO RECORD BUDGET
112100000000     C*------------------------------------------------------------------------*
112200000000     C     CHKRE3        BEGSR
112300000000     C*
112400000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
112500000000     C*
112600000000     C* LA DATA LETTA DEVE ESSERE MAX/UGU ALLA DATA INIZ E MIN/UGU ALLA DATA FIN
112700990527IF  1C     TBGDAT        IFGE      PARDAT
112800990527     C     TBGDAT        ANDLE     PARDA2
112900000000X   1C                   ELSE
113000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
113100000000     C*
113200000000     C* SE POI DATA BUDGET MAGGIORE A DATA FINALE RICHIESTA, ESCO DAL CICLO
113300990527IF  2C     TBGDAT        IFGT      PARDA2
113400000000     C                   MOVEL     '1'           $FIN3
113500000000E   2C                   ENDIF
113600000000E   1C                   ENDIF
113700000000     C*
113800000000     C* VALORE DI BUDGET ANNULLATO, ESCLUDO
113900000000IF  1C     TBGATB        IFNE      *BLANKS
114000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
114100000000E   1C                   ENDIF
114200960105     C*
114300960115     C* DATI IN STATISTICA <> DA QUELLI RICHIESTI
114400960115IF  1C     PARIOE        IFNE      *BLANKS
114500960115IF  2C     PARIOE        IFNE      TBGIOE
114600960105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
114700960105E   2C                   ENDIF
114800960105E   1C                   ENDIF
114900000000     C*
115000000000     C                   ENDSR
115100000000     C*------------------------------------------------------------------------*
115200000000     C*?MEMBDG - MEMORIZZA VALORI BUDGET
115300000000     C*------------------------------------------------------------------------*
115400000000     C     MEMBDG        BEGSR
115500000000     C*                                                    *PARTENZA
115600000000IF  1C     PARMPA        IFEQ      'P'
115700000000     C                   Z-ADD     TBGNSP        LAVSPE
115800000000     C                   Z-ADD     TBGKGP        LAVCHI
115900000000     C                   Z-ADD     TBGNCP        LAVCOL
116000000000     C                   Z-ADD     TBGVOP        LAVVOL
116100000000X   1C                   ELSE                                                   *ARRIVO
116200000000     C                   Z-ADD     TBGNSA        LAVSPE
116300000000     C                   Z-ADD     TBGKGA        LAVCHI
116400000000     C                   Z-ADD     TBGNCA        LAVCOL
116500000000     C                   Z-ADD     TBGVOA        LAVVOL
116600000000E   1C                   ENDIF
116700000000     C*
116800000000     C                   MOVEL     TBGFPE        FASCIA
116900000000     C                   Z-ADD     1             MM
117000000000     C*
117100000000     C* RICERCA DELLA POSIZIONE IN SHIERA DELL'ANNO/MESE CHE STO ELABORANDO
117200000000     C                   SETOFF                                           28
117300000000     C     TBGMES        LOOKUP    MES(MM)                                28
117400000000     C*
117500000000     C* SE MESE TROVATO, ADDIZIONO
117600000000IF  1C     *IN28         IFEQ      *ON
117700000000     C*
117800000000     C* FASCIA DI PESO PER FILIALE
117900000000     C     FASCIA        OCCUR     FPFIL
118000000000     C                   ADD       LAVSPE        SBF(MM)
118100000000     C                   ADD       LAVCHI        QBF(MM)
118200000000     C                   ADD       LAVCOL        CBF(MM)
118300000000     C                   ADD       LAVVOL        VBF(MM)
118400000000     C*
118500000000     C* FASCIA DI PESO PER AREA
118600000000     C     FASCIA        OCCUR     FPARE
118700000000     C                   ADD       LAVSPE        SBA(MM)
118800000000     C                   ADD       LAVCHI        QBA(MM)
118900000000     C                   ADD       LAVCOL        CBA(MM)
119000000000     C                   ADD       LAVVOL        VBA(MM)
119100000000     C*
119200000000     C* FASCIA DI PESO PER DIVISIONE
119300000000     C     FASCIA        OCCUR     FPDIV
119400000000     C                   ADD       LAVSPE        SBD(MM)
119500000000     C                   ADD       LAVCHI        QBD(MM)
119600000000     C                   ADD       LAVCOL        CBD(MM)
119700000000     C                   ADD       LAVVOL        VBD(MM)
119800000000     C*
119900000000     C* FASCIA DI PESO PER TOTALI GENERALI
120000000000     C     FASCIA        OCCUR     FPGEN
120100000000     C                   ADD       LAVSPE        SBG(MM)
120200000000     C                   ADD       LAVCHI        QBG(MM)
120300000000     C                   ADD       LAVCOL        CBG(MM)
120400000000     C                   ADD       LAVVOL        VBG(MM)
120500000000E   1C                   ENDIF
120600000000     C*
120700000000     C                   ENDSR
120800000000     C*------------------------------------------------------------------------*
120900000000     C*?STADET - IMPOSTA STAMPA TOTALI FILIALE DETTAGLIO
121000000000     C*------------------------------------------------------------------------*
121100000000     C     STADET        BEGSR
121200000000     C*
121300000000     C* °LE PERCENTUALI SUL BUDGET IN STAMPA LE IMPOSTO A ZERO PERCHE' I DATI
121400000000     C*  SUL FILE PER FILIALE/FILIALE NON ESISTONO
121500000000     C*
121600000000     C* CODICI INTESTAZIONE
121700980206     C                   MOVEL     DEPFIL        PTCFIL                         *FILIALE
121800000000     C                   MOVEL     DESFIL        PTDFIL
121900000000     C*
122000980206     C                   MOVEL     FIL(F)        PTCFIT                         *FILIALE DETTAGLIO
122100000000     C                   MOVEL     DFIL(F)       PTDFIT
122200000000     C*
122300980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
122400000000     C                   MOVEL     DESARE        PTDARE
122500000000     C*
122600980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
122700980206     C                   MOVEL     DESDIV        PTDDIV
122800000000     C*
122900950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
123000950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
123100950529     C*
123200000000     C* SCHIERE SALDI MENSILI DI FILIALE DETTAGLIO
123300000000DO  1C     1             DO        10            A
123400000000     C     A             OCCUR     FPDET
123500000000     C     A             OCCUR     FPSTA
123600000000     C                   Z-ADD     SPT           SPS                            *SPEDIZIONI
123700000000     C                   Z-ADD     SAT           SAS
123800000000     C                   Z-ADD     QTT           QTS                            *KILOGRAMMI
123900000000     C                   Z-ADD     QAT           QAS
124000000000     C                   Z-ADD     CLT           CLS                            *COLLI
124100000000     C                   Z-ADD     CAT           CAS
124200000000     C                   Z-ADD     VLT           VLS                            *VOLUME
124300000000     C                   Z-ADD     VAT           VAS
124400000000     C*
124500000000     C* PULISCO I CAMPI DI LAVORO PER IL BUDGET
124600000000     C                   CLEAR                   SBS
124700000000     C                   CLEAR                   QBS
124800000000E   1C                   ENDDO
124900000000     C*
125000000000     C* IMPOSTO NO STAMPA DATI BUDGET°
125100000000     C                   MOVEL     '1'           NOBDG
125200000000     C*
125300000000     C* ROUTINE DI STAMPA TOTALI
125400000000     C                   EXSR      STAMPA
125500000000     C*
125600000000     C                   ENDSR
125700000000     C*------------------------------------------------------------------------*
125800000000     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
125900000000     C*------------------------------------------------------------------------*
126000000000     C     STAFIL        BEGSR
126100000000     C*
126200000000     C* CODICI INTESTAZIONE
126300980206     C                   MOVEL     DEPFIL        PTCFIL                         *FILIALE
126400000000     C                   MOVEL     DESFIL        PTDFIL
126500000000     C*
126600000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
126700000000     C                   MOVEL     *BLANKS       PTDFIT
126800000000     C                   MOVEL     PAR(11)       PTDFIT
126900000000     C*
127000980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
127100000000     C                   MOVEL     DESARE        PTDARE
127200000000     C*
127300980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
127400980206     C                   MOVEL     DESDIV        PTDDIV
127500950529     C*
127600950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
127700950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
127800000000     C*
127900000000     C* SCHIERE SALDI MENSILI DI FILIALE
128000000000DO  1C     1             DO        10            A
128100000000     C     A             OCCUR     FPFIL
128200000000     C     A             OCCUR     FPSTA
128300000000     C                   Z-ADD     SPF           SPS                            *SPEDIZIONI
128400000000     C                   Z-ADD     SBF           SBS
128500000000     C                   Z-ADD     SAF           SAS
128600000000     C                   Z-ADD     QTF           QTS                            *KILOGRAMMI
128700000000     C                   Z-ADD     QBF           QBS
128800000000     C                   Z-ADD     QAF           QAS
128900000000     C                   Z-ADD     CLF           CLS                            *COLLI
129000000000     C                   Z-ADD     CAF           CAS
129100000000     C                   Z-ADD     VLF           VLS                            *VOLUME
129200000000     C                   Z-ADD     VAF           VAS
129300000000     C                   Z-ADD     CBF           CBS
129400000000     C                   Z-ADD     VBF           VBS
129500000000E   1C                   ENDDO
129600000000     C*
129700000000     C* ROUTINE DI STAMPA TOTALI
129800000000     C                   EXSR      STAMPA
129900000000     C*
130000000000     C                   ENDSR
130100000000     C*------------------------------------------------------------------------*
130200000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
130300000000     C*------------------------------------------------------------------------*
130400000000     C     STAARE        BEGSR
130500000000     C*
130600000000     C* CODICI INTESTAZIONE
130700000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
130800000000     C                   MOVEL     *BLANKS       PTDFIL
130900000000     C                   MOVEL     PAR(11)       PTDFIL
131000000000     C*
131100000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
131200000000     C                   MOVEL     *BLANKS       PTDFIT
131300000000     C                   MOVEL     PAR(11)       PTDFIT
131400000000     C*
131500980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
131600000000     C                   MOVEL     DESARE        PTDARE
131700980206     C*---                 Z-ADD*ZEROS    PTCARE
131800000000     C*
131900980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
132000980206     C                   MOVEL     DESDIV        PTDDIV
132100950529     C*
132200950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
132300950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
132400000000     C*
132500000000     C* SCHIERE SALDI MENSILI DI AREA
132600000000DO  1C     1             DO        10            A
132700000000     C     A             OCCUR     FPARE
132800000000     C     A             OCCUR     FPSTA
132900000000     C                   Z-ADD     SPA           SPS                            *SPEDIZIONI
133000000000     C                   Z-ADD     SBA           SBS
133100000000     C                   Z-ADD     SAA           SAS
133200000000     C                   Z-ADD     QTA           QTS                            *KILOGRAMMI
133300000000     C                   Z-ADD     QBA           QBS
133400000000     C                   Z-ADD     QAA           QAS
133500000000     C                   Z-ADD     CLA           CLS                            *COLLI
133600000000     C                   Z-ADD     CAA           CAS
133700000000     C                   Z-ADD     VLA           VLS                            *VOLUME
133800000000     C                   Z-ADD     VAA           VAS
133900000000     C                   Z-ADD     CBA           CBS
134000000000     C                   Z-ADD     VBA           VBS
134100000000E   1C                   ENDDO
134200000000     C*
134300000000     C* ROUTINE DI STAMPA TOTALI
134400000000     C                   EXSR      STAMPA
134500000000     C*
134600000000     C                   ENDSR
134700000000     C*------------------------------------------------------------------------*
134800000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
134900000000     C*------------------------------------------------------------------------*
135000000000     C     STADIV        BEGSR
135100000000     C*
135200000000     C* CODICI INTESTAZIONE
135300000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
135400000000     C                   MOVEL     *BLANKS       PTDFIL
135500000000     C                   MOVEL     PAR(11)       PTDFIL
135600000000     C*
135700000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
135800000000     C                   MOVEL     *BLANKS       PTDFIT
135900000000     C                   MOVEL     PAR(11)       PTDFIT
136000000000     C*
136100000000     C                   MOVEL     *BLANKS       PTCARE                         *AREA
136200000000     C                   MOVEL     *BLANKS       PTDARE
136300000000     C                   MOVEL     PAR(11)       PTDARE
136400000000     C*
136500980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
136600980206     C                   MOVEL     DESDIV        PTDDIV
136700950529     C*
136800950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
136900950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
137000000000     C*
137100000000     C* SCHIERE SALDI MENSILI DI DIVISIONE
137200000000DO  1C     1             DO        10            A
137300000000     C     A             OCCUR     FPDIV
137400000000     C     A             OCCUR     FPSTA
137500000000     C                   Z-ADD     SPD           SPS                            *SPEDIZIONI
137600000000     C                   Z-ADD     SBD           SBS
137700000000     C                   Z-ADD     SAD           SAS
137800000000     C                   Z-ADD     QTD           QTS                            *KILOGRAMMI
137900000000     C                   Z-ADD     QBD           QBS
138000000000     C                   Z-ADD     QAD           QAS
138100000000     C                   Z-ADD     CLD           CLS                            *COLLI
138200000000     C                   Z-ADD     CAD           CAS
138300000000     C                   Z-ADD     VLD           VLS                            *VOLUME
138400000000     C                   Z-ADD     VAD           VAS
138500000000     C                   Z-ADD     CBD           CBS
138600000000     C                   Z-ADD     VBD           VBS
138700000000E   1C                   ENDDO
138800000000     C*
138900000000     C* ROUTINE DI STAMPA TOTALI
139000000000     C                   EXSR      STAMPA
139100000000     C*
139200000000     C                   ENDSR
139300000000     C*------------------------------------------------------------------------*
139400000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
139500000000     C*------------------------------------------------------------------------*
139600000000     C     STAGEN        BEGSR
139700000000     C*
139800000000     C* CODICI INTESTAZIONE
139900000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
140000000000     C                   MOVEL     *BLANKS       PTDFIL
140100000000     C                   MOVEL     PAR(11)       PTDFIL
140200000000     C*
140300000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
140400000000     C                   MOVEL     *BLANKS       PTDFIT
140500000000     C                   MOVEL     PAR(11)       PTDFIT
140600000000     C*
140700000000     C                   MOVEL     *BLANKS       PTCARE                         *AREA
140800000000     C                   MOVEL     *BLANKS       PTDARE
140900000000     C                   MOVEL     PAR(11)       PTDARE
141000000000     C*
141100000000     C                   MOVEL     *BLANKS       PTCDIV                         *DIVISIONE
141200000000     C                   MOVEL     *BLANKS       PTDDIV
141300000000     C                   MOVEL     PAR(11)       PTDDIV
141400950529     C*
141500950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
141600950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
141700000000     C*
141800000000     C* SCHIERE SALDI MENSILI GENERALI
141900000000DO  1C     1             DO        10            A
142000000000     C     A             OCCUR     FPGEN
142100000000     C     A             OCCUR     FPSTA
142200000000     C                   Z-ADD     SPG           SPS                            *SPEDIZIONI
142300000000     C                   Z-ADD     SBG           SBS
142400000000     C                   Z-ADD     SAG           SAS
142500000000     C                   Z-ADD     QTG           QTS                            *KILOGRAMMI
142600000000     C                   Z-ADD     QBG           QBS
142700000000     C                   Z-ADD     QAG           QAS
142800000000     C                   Z-ADD     CLG           CLS                            *COLLI
142900000000     C                   Z-ADD     CAG           CAS
143000000000     C                   Z-ADD     VLG           VLS                            *VOLUME
143100000000     C                   Z-ADD     VAG           VAS
143200000000     C                   Z-ADD     CBG           CBS
143300000000     C                   Z-ADD     VBG           VBS
143400000000E   1C                   ENDDO
143500000000     C*
143600000000     C* ROUTINE DI STAMPA TOTALI DI FILIALE
143700000000     C                   EXSR      STAMPA
143800000000     C*
143900000000     C                   ENDSR
144000000000     C*------------------------------------------------------------------------*
144100000000     C*?STAMPA - STAMPA TOTALI GENERALI DI DIVISIONE/AREA/FILIALE
144200000000     C*------------------------------------------------------------------------*
144300000000     C     STAMPA        BEGSR
144400000000     C*
144500000000     C* REIMPOSTO IL CONTATORE DI FASCE STAMPATE PER RISTAMPARE LA TESTATA
144600000000     C                   Z-ADD     *ZEROS        CONFAS            2 0
144700000000     C*
144800000000     C* PULISCO I TOTALI DI LAVORO GENERALI/DIVISIONE/AREA/FILIALE
144900000000     C                   CLEAR                   SPW                            *SPEDIZIONI
145000000000     C                   CLEAR                   SBW
145100000000     C                   CLEAR                   SAW
145200000000     C                   CLEAR                   QTW                            *KILOGRAMMI
145300000000     C                   CLEAR                   QBW
145400000000     C                   CLEAR                   QAW
145500000000     C                   CLEAR                   CLW                            *COLLI
145600000000     C                   CLEAR                   CAW
145700000000     C                   CLEAR                   VLW                            *VOLUME
145800000000     C                   CLEAR                   VAW
145900000000     C                   CLEAR                   CBW
146000000000     C                   CLEAR                   VBW
146100000000     C*
146200000000     C* STAMPA  T E S T A T A
146300020115     C                   WRITE     SE46PT                                       *TESTATA
146400020115     C                   WRITE     SE46PR                                       *RIGA
146500020115     C                   WRITE     SE46PT1                                      *DECOD FIL/AREA/DIV
146600020115     C                   WRITE     SE46PR                                       *RIGA
146700020115     C                   WRITE     SE46PI                                       *DECODIFICA MESI
146800000000     C*
146900000000     C* PER OGNI FASCIA LETTA
147000000000DO  1C     1             DO        10            A
147100000000     C     A             OCCUR     FPSTA
147200000000     C*
147300000000     C                   ADD       1             CONFAS                         *CONT. FASCE STAMPATE
147400000000     C                   MOVEL     FPE(A)        PPDFPE                         *DESCRIZIONE FASCIA
147500000000     C*
147600000000     C* ADDIZIONA I SALDI PER I TOTALI A FINE PAGINA
147700000000     C                   ADD       SPS           SPW                            *SPEDIZIONI
147800000000     C                   ADD       SBS           SBW
147900000000     C                   ADD       SAS           SAW
148000000000     C                   ADD       QTS           QTW                            *KILOGRAMMI
148100000000     C                   ADD       QBS           QBW
148200000000     C                   ADD       QAS           QAW
148300000000     C                   ADD       CLS           CLW                            *COLLI
148400000000     C                   ADD       CAS           CAW
148500000000     C                   ADD       VLS           VLW                            *VOLUME
148600000000     C                   ADD       VAS           VAW
148700000000     C                   ADD       CBS           CBW
148800000000     C                   ADD       VBS           VBW
148900000000     C*
149000000000     C* SE SONO ALLA 6^ FASCIA STAMPO LA TESTATA A SALTO PAGINA
149100000000IF  2C     CONFAS        IFEQ      6
149200020115     C                   WRITE     SE46PR                                       *RIGA
149300020115     C                   WRITE     SE46PT                                       *TESTATA
149400020115     C                   WRITE     SE46PR                                       *RIGA
149500020115     C                   WRITE     SE46PI                                       *DECODIFICA MESI
149600000000E   2C                   ENDIF
149700000000     C*
149800020115     C                   WRITE     SE46PP                                       *STAMPA DECOD. FASCIA
149900000000     C*
150000000000     C* S T A M P A   F A S C I A
150100000000     C*
150200000000     C                   EXSR      STFASC
150300000000     C*
150400000000E   1C                   ENDDO
150500000000     C*
150600000000     C* TRASFERISCE I TOTALI A FINE PAGINA NELLE SCHIERE DI LAVORO
150700000000     C                   Z-ADD     SPW           SPS                            *SPEDIZIONI
150800000000     C                   Z-ADD     SBW           SBS
150900000000     C                   Z-ADD     SAW           SAS
151000000000     C                   Z-ADD     QTW           QTS                            *KILOGRAMMI
151100000000     C                   Z-ADD     QBW           QBS
151200000000     C                   Z-ADD     QAW           QAS
151300000000     C                   Z-ADD     CLW           CLS                            *COLLI
151400000000     C                   Z-ADD     CAW           CAS
151500000000     C                   Z-ADD     VLW           VLS                            *VOLUME
151600000000     C                   Z-ADD     VAW           VAS
151700000000     C                   Z-ADD     CBW           CBS
151800000000     C                   Z-ADD     VBW           VBS
151900000000     C*
152000000000     C* S T A M P A   T O T A L E
152100000000     C*
152200000000     C                   MOVEL     DRI(7)        PPDFPE                         *DESCRIZIONE TOTALE
152300020115     C                   WRITE     SE46PP                                       *STAMPA DESCR. TOTALE
152400000000     C                   EXSR      STFASC
152500000000     C*
152600000000     C* S T A M P A   R A P P O R T I
152700000000     C*
152800000000     C                   EXSR      STRAPP
152900000000     C*
153000000000     C* STAMPA RIGA DI CHIUSURA PROSPETTO
153100020115     C                   WRITE     SE46PR
153200000000     C*
153300000000     C                   ENDSR
153400000000     C*------------------------------------------------------------------------*
153500000000     C*?STFASC - STAMPA I VALORI DELLA FASCIA O DEI TOTALI A FINE PAGINA
153600000000     C*------------------------------------------------------------------------*
153700000000     C     STFASC        BEGSR
153800000000     C*
153900000000     C*---------------------
154000000000     C* S P E D I Z I O N I
154100000000     C*---------------------
154200000000     C* DESCRIZIONE DEL VALORE DA STAMPARE E DELLE SUE PERCENTUALI
154300000000     C                   MOVEL     DRI(1)        P1DFAS
154400000000     C                   MOVEL     DRI(5)        P2DFAS
154500000000     C*
154600000000     C* CALCOLA LE PERCENTUALI, LA 13^ COLONNA E STAMPA LA RIGA
154700000000     C                   Z-ADD     SPS           $VA                            *VALORE ANNO CORRENTE
154800000000     C                   Z-ADD     SBS           $BD                            *VALORE BUDGET
154900000000     C                   Z-ADD     SAS           $AP                            *VALORE AA PRECEDENTE
155000000000     C                   EXSR      STRIGA
155100000000     C*---------------------
155200000000     C* K I L O G R A M M I  (IN STAMPA QLI)
155300000000     C*---------------------
155400000000     C* DESCRIZIONE DEL VALORE DA STAMPARE E DELLE SUE PERCENTUALI
155500000000     C                   MOVEL     DRI(2)        P1DFAS
155600000000     C                   MOVEL     DRI(5)        P2DFAS
155700000000     C*
155800000000     C* CALCOLA LE PERCENTUALI, LA 13^ COLONNA E STAMPA LA RIGA
155900000000     C     QTS           DIV       100           $VA                            *VALORE ANNO CORRENTE
156000000000     C     QBS           DIV       100           $BD                            *VALORE BUDGET
156100000000     C     QAS           DIV       100           $AP                            *VALORE AA PRECEDENTE
156200000000     C                   EXSR      STRIGA
156300000000     C*---------------------
156400000000     C* C O L L I
156500000000     C*---------------------
156600000000     C* DESCRIZIONE DEL VALORE DA STAMPARE E DELLE SUE PERCENTUALI
156700000000     C                   MOVEL     DRI(3)        P1DFAS
156800000000     C                   MOVEL     DRI(5)        P2DFAS
156900000000     C*
157000000000     C* CALCOLA LE PERCENTUALI, LA 13^ COLONNA E STAMPA LA RIGA
157100000000     C                   Z-ADD     CLS           $VA                            *VALORE ANNO CORRENTE
157200000000     C                   Z-ADD     CBS           $BD                            *VALORE BUDGET
157300000000     C                   Z-ADD     CAS           $AP                            *VALORE AA PRECEDENTE
157400000000     C                   EXSR      STRIGA
157500000000     C*---------------------
157600000000     C* V O L U M E
157700000000     C*---------------------
157800000000     C* DESCRIZIONE DEL VALORE DA STAMPARE E DELLE SUE PERCENTUALI
157900000000     C                   MOVEL     DRI(4)        P1DFAS
158000000000     C                   MOVEL     DRI(5)        P2DFAS
158100000000     C*
158200000000     C* CALCOLA LE PERCENTUALI, LA 13^ COLONNA E STAMPA LA RIGA
158300000000     C                   Z-ADD     VLS           $VA                            *VALORE ANNO CORRENTE
158400000000     C                   Z-ADD     VBS           $BD                            *VALORE BUDGET
158500000000     C                   Z-ADD     VAS           $AP                            *VALORE AA PRECEDENTE
158600000000     C                   EXSR      STRIGA
158700000000     C*
158800000000     C                   ENDSR
158900000000     C*------------------------------------------------------------------------*
159000000000     C*?STRIGA - STAMPA RIGA FASCIA O TOTALI FINE PAGINA
159100000000     C*------------------------------------------------------------------------*
159200000000     C     STRIGA        BEGSR
159300000000     C*
159400000000     C* °SE NON HO MEMORIZZATO I VALORI DEL BUDGET (SCHIERE VUOTE);
159500000000     C*  SE STO STAMPANDO I TOTALI PER FIL-DETTAGLIO(PER CUI NON CI SONO I DATI)
159600000000     C*  IMPOSTO LA PERCENTUALE DEL BUDGET A ZERO
159700000000     C*
159800000000     C* PER OGNI MESE
159900000000DO  1C     1             DO        PARJ          X                 2 0
160000000000     C*
160100000000     C* CALCOLA LE PERCENTUALI
160200000000     C                   Z-ADD     $VA(X)        CONSUN           17 3          *VALORE CORRENTE
160300000000     C*--------------------
160400000000     C* SUL  B U D G E T°
160500000000     C*--------------------
160600000000IF  2C     NOBDG         IFEQ      '0'
160700000000     C                   Z-ADD     $BD(X)        RIFERI           17 3          *VALORE BUDGET
160800000000     C                   EXSR      CALPER
160900000000     C                   Z-ADD     PERCEN        $PB(X)                         *VALORE PERCENTUALE
161000000000X   2C                   ELSE
161100000000     C                   Z-ADD     *ZEROS        $PB(X)                         *PERCENTUALE A ZERO
161200000000E   2C                   ENDIF
161300000000     C*--------------------
161400000000     C* SULL' ANNO  P R E C E D E N T E
161500000000     C*--------------------
161600000000     C                   Z-ADD     $AP(X)        RIFERI                         *VALORE ANNO PRECEDEN
161700000000     C                   EXSR      CALPER
161800000000     C                   Z-ADD     PERCEN        $PA(X)                         *VALORE PERCENTUALE
161900000000     C*
162000000000E   1C                   ENDDO
162100000000     C*
162200000000     C* CALCOLA LA 13^ COLONNA
162300000000     C                   XFOOT     $VA           $VA13C           17 3          *ANNO CORRENTE
162400000000     C                   XFOOT     $BD           $BD13C           17 3          *BUDGET
162500000000     C                   XFOOT     $AP           $AP13C           17 3          *ANNO PRECEDENTE
162600000000     C*
162700000000     C* E LE SUE PERCENTUALI
162800000000     C                   Z-ADD     $VA13C        CONSUN                         *VALORE CORRENTE
162900000000     C*--------------------
163000000000     C* SUL  B U D G E T°
163100000000     C*--------------------
163200000000IF  1C     NOBDG         IFEQ      '0'
163300000000     C                   Z-ADD     $BD13C        RIFERI                         *VALORE BUDGET
163400000000     C                   EXSR      CALPER
163500000000     C                   Z-ADD     PERCEN        $PB13C            4 1          *VALORE PERCENTUALE
163600000000X   1C                   ELSE
163700000000     C                   Z-ADD     *ZEROS        $PB13C                         *PERCENTUALE A ZERO
163800000000E   1C                   ENDIF
163900000000     C*--------------------
164000000000     C* SULL' ANNO  P R E C E D E N T E
164100000000     C*--------------------
164200000000     C                   Z-ADD     $AP13C        RIFERI                         *VALORE ANNO PRECEDEN
164300000000     C                   EXSR      CALPER
164400000000     C                   Z-ADD     PERCEN        $PA13C            4 1          *VALORE PERCENTUALE
164500000000     C*
164600000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
164700000000     C                   EXSR      STACAM
164800000000     C*
164900000000     C                   ENDSR
165000000000     C*------------------------------------------------------------------------*
165100000000     C*?CALPER - CALCOLA LE PERCENTUALI A.P./BDG SUI SALDI
165200000000     C*          INPUT  -- CONSUN: VALORE CORRENTE
165300000000     C*                    RIFERI: VALORE DELL' ANNO PRECEDENTE / BUDGET
165400000000     C*          OUTPUT -- PERCEN: PERCENTUALE CALCOLATA
165500000000     C*------------------------------------------------------------------------*
165600000000     C     CALPER        BEGSR
165700000000     C*
165800000000     C*   VALORE CONSUNTIVO  *  100
165900000000     C*  ---------------------------- - 100 =  % SCOSTAMENTO (AP/BDG)
166000000000     C*   VALORE RIFERIMENTO (AP/BDG)
166100000000     C*
166200000000     C* A) SIA IL VALORE A CONSUNTIVO CHE QUELLO DI RIFERIMENTO SONO = ZERO;
166300000000     C*    % DI SCOSTAMENTO = ZERO
166400000000     C* B) IL VALORE A CONSUNTIVO E' = ZERO MENTRE QUELLO DI RIFERIMENTO NO;
166500000000     C*    % DI SCOSTAMENTO = -999,9
166600000000     C* C) IL VALORE A CONSUNTIVO E' > ZERO MENTRE QUELLO DI RIFERIMENTO E' = 0;
166700000000     C*    % DI SCOSTAMENTO = +999,9
166800000000     C* D) SIA IL VALORE A CONSUNTIVO CHE QUELLO DI RIFERIMENTO SONO > ZERO;
166900000000     C*    % DI SCOSTAMENTO = +/-XXX,X
167000000000     C*
167100000000     C                   SETOFF                                           28
167200000000     C                   Z-ADD     *ZEROS        PERCEN            4 1
167300000000     C*
167400000000IF  1C     CONSUN        IFEQ      *ZEROS
167500000000IF  2C     RIFERI        IFEQ      *ZEROS
167600000000     C                   Z-ADD     *ZEROS        PERCEN                         *CASO A
167700000000X   2C                   ELSE
167800000000     C                   Z-ADD     -999,9        PERCEN                         *CASO B
167900000000E   2C                   ENDIF
168000000000X   1C                   ELSE
168100000000IF  2C     RIFERI        IFEQ      *ZEROS
168200000000     C                   Z-ADD     +999,9        PERCEN                         *CASO C
168300000000X   2C                   ELSE
168400000000     C*
168500000000     C     CONSUN        MULT      100           RISUL            19 3
168600000000     C     RISUL         DIV       RIFERI        RISUL
168700000000     C     RISUL         SUB       100           RISUL
168800000000     C*
168900000000     C* SE LA PERCENTUALE DI SCOSTAMENTO E' +/-1000  METTO  +/-999,9%
169000000000     C                   Z-ADD     RISUL         PERCEN
169100000000     C     RISUL         COMP      0                                    28
169200000000     C* POSITIVO
169300000000IF  3C     *IN28         IFEQ      *OFF
169400000000IF  4C     RISUL         IFGE      1000
169500000000     C                   Z-ADD     +999,9        PERCEN
169600000000E   4C                   ENDIF
169700000000     C* NEGATIVO
169800000000X   3C                   ELSE
169900000000IF  4C     RISUL         IFLE      -1000
170000000000     C                   Z-ADD     -999,9        PERCEN
170100000000E   4C                   ENDIF
170200000000E   3C                   ENDIF
170300000000     C*
170400000000E   2C                   ENDIF
170500000000E   1C                   ENDIF
170600000000     C*
170700000000     C                   ENDSR
170800000000     C*------------------------------------------------------------------------*
170900000000     C*?STACAM - MUOVE I CAMPI VALORE E % IN QUELLI DI STAMPA E STAMPA
171000000000     C*------------------------------------------------------------------------*
171100000000     C     STACAM        BEGSR
171200000000     C*
171300000000     C* VALORE ANNO CORRENTE
171400000000     C                   Z-ADD     $VA(1)        P1CQT1                         *MESE UNO
171500000000     C                   Z-ADD     $VA(2)        P1CQT2                         *MESE DUE
171600000000     C                   Z-ADD     $VA(3)        P1CQT3                         *MESE TRE
171700000000     C                   Z-ADD     $VA(4)        P1CQT4                         *MESE QUATTRO
171800000000     C                   Z-ADD     $VA(5)        P1CQT5                         *MESE CINQUE
171900000000     C                   Z-ADD     $VA(6)        P1CQT6                         *MESE SEI
172000000000     C                   Z-ADD     $VA(7)        P1CQT7                         *MESE SETTE
172100000000     C                   Z-ADD     $VA(8)        P1CQT8                         *MESE OTTO
172200000000     C                   Z-ADD     $VA(9)        P1CQT9                         *MESE NOVE
172300000000     C                   Z-ADD     $VA(10)       P1CQT0                         *MESE DIECI
172400000000     C                   Z-ADD     $VA(11)       P1CQTA                         *MESE UNDICI
172500000000     C                   Z-ADD     $VA(12)       P1CQTB                         *MESE DODICI
172600000000     C                   Z-ADD     $VA13C        P1CQTT                         *TOTALE MESI
172700000000     C*
172800020115     C                   WRITE     SE46P1                                       *STAMPA RIGA
172900000000     C*
173000000000     C* PERCENTUALI %BGD E %AP
173100000000     C                   Z-ADD     $PA(1)        P1CPA1
173200000000     C                   Z-ADD     $PA(2)        P1CPA2
173300000000     C                   Z-ADD     $PA(3)        P1CPA3
173400000000     C                   Z-ADD     $PA(4)        P1CPA4
173500120227     C**                 Z-ADD     $PB(5)        P1CPB5
173600000000     C                   Z-ADD     $PA(5)        P1CPA5
173700120227     C**                 Z-ADD     $PB(6)        P1CPB6
173800000000     C                   Z-ADD     $PA(6)        P1CPA6
173900000000     C                   Z-ADD     $PA(7)        P1CPA7
174000000000     C                   Z-ADD     $PA(8)        P1CPA8
174100000000     C                   Z-ADD     $PA(9)        P1CPA9
174200000000     C                   Z-ADD     $PA(10)       P1CPA0
174300000000     C                   Z-ADD     $PA(11)       P1CPAA
174400000000     C                   Z-ADD     $PA(12)       P1CPAB
174500000000     C                   Z-ADD     $PA13C        P1CPAT
174600000000     C*
174700020115     C                   WRITE     SE46P2                                       *STAMPA RIGA
174800000000     C*
174900000000     C                   ENDSR
175000000000     C*------------------------------------------------------------------------*
175100000000     C*?STRAPP - STAMPA I RAPPORTI SUI TOTALI A FINE PAGINA
175200000000     C*------------------------------------------------------------------------*
175300000000     C     STRAPP        BEGSR
175400000000     C*
175500000000     C* KILOGRAMMI/SPEDIZIONI
175600000000     C*
175700000000     C                   MOVEL     DRI(8)        P3DFAS                         *DESCRIZIONE RAPPORTO
175800000000     C                   Z-ADD     QTS           $VS                            *VALORE SUPERIORE
175900000000     C                   Z-ADD     SPS           $VI                            *VALORE INFERIORE
176000000000     C*
176100000000     C* CALCOLA IL RAPPORTO, LA 13^ COLONNA E STAMPA LA RIGA
176200000000     C                   EXSR      STRIGR
176300000000     C*
176400000000     C* COLLI/SPEDIZIONI
176500000000     C*
176600000000     C                   MOVEL     DRI(9)        P3DFAS                         *DESCRIZIONE RAPPORTO
176700000000     C                   Z-ADD     CLS           $VS                            *VALORE SUPERIORE
176800000000     C*
176900000000     C* CALCOLA IL RAPPORTO, LA 13^ COLONNA E STAMPA LA RIGA
177000000000     C                   EXSR      STRIGR
177100000000     C*
177200000000     C* VOLUME/SPEDIZIONI
177300000000     C*
177400000000     C                   MOVEL     DRI(10)       P3DFAS                         *DESCRIZIONE RAPPORTO
177500000000     C                   Z-ADD     VLS           $VS                            *VALORE SUPERIORE
177600000000     C*
177700000000     C* CALCOLA IL RAPPORTO, LA 13^ COLONNA E STAMPA LA RIGA
177800000000     C                   EXSR      STRIGR
177900000000     C*
178000000000     C                   ENDSR
178100000000     C*------------------------------------------------------------------------*
178200000000     C*?STRIGR - STAMPA RIGA RAPPORTI
178300000000     C*------------------------------------------------------------------------*
178400000000     C     STRIGR        BEGSR
178500000000     C*
178600000000     C* PER OGNI MESE CALCOLA IL RAPPORTO
178700000000DO  1C     1             DO        PARJ          X
178800000000     C*
178900000000     C* SE SPEDIZIONI = 0, LE IMPOSTO = 1 PER EVITARE ERRORI NELLE DIVISIONI
179000000000IF  2C     $VI(X)        IFEQ      *ZEROS
179100000000     C                   Z-ADD     1             VALINF           17 3
179200000000X   2C                   ELSE
179300000000     C                   Z-ADD     $VI(X)        VALINF
179400000000E   2C                   ENDIF
179500000000     C*
179600000000     C     $VS(X)        DIV       VALINF        $RP(X)                         *RAPPORTO DEL MESE
179700000000     C*
179800000000E   1C                   ENDDO
179900000000     C*
180000000000     C* CALCOLA IL RAPPORTO PER LA 13^ COLONNA
180100000000     C                   XFOOT     $VS           $VS13C           17 3          *TOTALE VALORE SUPER
180200000000     C                   XFOOT     $VI           $VI13C           17 3          *TOTALE VALORE INFER
180300000000     C*
180400000000     C* SE SPEDIZIONI = 0, LE IMPOSTO = 1 PER EVITARE ERRORI NELLE DIVISIONI
180500000000IF  1C     $VI13C        IFEQ      *ZEROS
180600000000     C                   Z-ADD     1             $VI13C
180700000000E   1C                   ENDIF
180800000000     C*
180900000000     C     $VS13C        DIV       $VI13C        $RP13C            9 3          *RAPPORTO 13^ COLONNA
181000000000     C*
181100000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
181200000000     C                   EXSR      STARAP
181300000000     C*
181400000000     C                   ENDSR
181500000000     C*------------------------------------------------------------------------*
181600000000     C*?STARAP - MUOVE I RAPPORTI CALCOLATI NEI CAMPI DI STAMPA E STAMPA
181700000000     C*------------------------------------------------------------------------*
181800000000     C     STARAP        BEGSR
181900000000     C*
182000000000     C* RAPPORTO
182100000000     C                   Z-ADD     $RP(1)        P3CPE1                         *MESE UNO
182200000000     C                   Z-ADD     $RP(2)        P3CPE2                         *MESE DUE
182300000000     C                   Z-ADD     $RP(3)        P3CPE3                         *MESE TRE
182400000000     C                   Z-ADD     $RP(4)        P3CPE4                         *MESE QUATTRO
182500000000     C                   Z-ADD     $RP(5)        P3CPE5                         *MESE CINQUE
182600000000     C                   Z-ADD     $RP(6)        P3CPE6                         *MESE SEI
182700000000     C                   Z-ADD     $RP(7)        P3CPE7                         *MESE SETTE
182800000000     C                   Z-ADD     $RP(8)        P3CPE8                         *MESE OTTO
182900000000     C                   Z-ADD     $RP(9)        P3CPE9                         *MESE NOVE
183000000000     C                   Z-ADD     $RP(10)       P3CPE0                         *MESE DIECI
183100000000     C                   Z-ADD     $RP(11)       P3CPEA                         *MESE UNDICI
183200000000     C                   Z-ADD     $RP(12)       P3CPEB                         *MESE DODICI
183300000000     C                   Z-ADD     $RP13C        P3CPET                         *TOTALE MESI
183400000000     C*
183500020115     C                   WRITE     SE46P3                                       *STAMPA RIGA RAPPORTI
183600000000     C*
183700000000     C                   ENDSR
183800000000     C*------------------------------------------------------------------------*
183900000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
184000000000     C*------------------------------------------------------------------------*
184100000000     C     DECTES        BEGSR
184200000000     C*
184300950529     C* TIPO MOVIMENTI
184400000000IF  1C     PARMPA        IFEQ      'P'
184500950529     C                   MOVEL     PAR(1)        PTCMPA
184600950529     C                   MOVEL     PAR(1)        PTDMP2
184700951005     C                   MOVEL     PAR(2)        PTDMPA                         *DESCR FIL DET=ARRIVO
184800000000X   1C                   ELSE
184900950529     C                   MOVEL     PAR(2)        PTCMPA
185000950529     C                   MOVEL     PAR(2)        PTDMP2
185100951005     C                   MOVEL     PAR(1)        PTDMPA                         *DESCR FIL DET=PARTEN
185200000000E   1C                   ENDIF
185300950529     C*
185400950529     C* DATI IN STATISTICA
185500950529IF  1C     PARIOE        IFEQ      'I'
185600950602     C                   MOVEL     PAR(12)       PTCIOE
185700950529     C                   MOVEL     PAR(12)       PTDIOE                         *DATI "ITALIA"
185800950529E   1C                   ENDIF
185900950529IF  1C     PARIOE        IFEQ      'M'
186000950602     C                   MOVEL     PAR(13)       PTCIOE
186100950529     C                   MOVEL     PAR(13)       PTDIOE                         *DATI "IMPORT"
186200950529E   1C                   ENDIF
186300950529IF  1C     PARIOE        IFEQ      'X'
186400950602     C                   MOVEL     PAR(14)       PTCIOE
186500950529     C                   MOVEL     PAR(14)       PTDIOE                         *DATI "EXPORT"
186600950529E   1C                   ENDIF
186700950529IF  1C     PARIOE        IFEQ      *BLANKS
186800950602     C                   MOVEL     PAR(16)       PTCIOE
186900950602     C                   MOVEL     PAR(16)       PTDIOE                         *DATI "TUTTI"
187000950529E   1C                   ENDIF
187100950529     C*
187200000000     C* DECODIFICA MESI
187300000000IF  1C     MES(1)        IFGT      *ZEROS
187400000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ.
187500000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
187600990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
187700000000E   1C                   ENDIF
187800000000     C*
187900000000IF  1C     MES(2)        IFGT      *ZEROS
188000000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ.
188100000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
188200990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
188300000000E   1C                   ENDIF
188400000000     C*
188500000000IF  1C     MES(3)        IFGT      *ZEROS
188600000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ.
188700000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
188800990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
188900000000E   1C                   ENDIF
189000000000     C*
189100000000IF  1C     MES(4)        IFGT      *ZEROS
189200000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ.
189300000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
189400990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
189500000000E   1C                   ENDIF
189600000000     C*
189700000000IF  1C     MES(5)        IFGT      *ZEROS
189800000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ.
189900000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
190000990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
190100000000E   1C                   ENDIF
190200000000     C*
190300000000IF  1C     MES(6)        IFGT      *ZEROS
190400000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ.
190500000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
190600990608     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
190700000000E   1C                   ENDIF
190800000000     C*
190900000000IF  1C     MES(7)        IFGT      *ZEROS
191000000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ.
191100000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
191200990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
191300000000E   1C                   ENDIF
191400000000     C*
191500000000IF  1C     MES(8)        IFGT      *ZEROS
191600000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ.
191700000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
191800990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
191900000000E   1C                   ENDIF
192000000000     C*
192100000000IF  1C     MES(9)        IFGT      *ZEROS
192200000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ.
192300000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
192400990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
192500000000E   1C                   ENDIF
192600000000     C*
192700000000IF  1C     MES(10)       IFGT      *ZEROS
192800000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ.
192900000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
193000990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
193100000000E   1C                   ENDIF
193200000000     C*
193300000000IF  1C     MES(11)       IFGT      *ZEROS
193400000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ.
193500000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
193600990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
193700000000E   1C                   ENDIF
193800000000     C*
193900000000IF  1C     MES(12)       IFGT      *ZEROS
194000000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ.
194100000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
194200990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
194300000000E   1C                   ENDIF
194400000000     C*
194500000000     C* DECODIFICA FASCE DI PESO
194600990527IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
194700990217     C                   MOVEL     'SDR'         WSX               3
194800990217X   1C                   ELSE                                                   *FASCE DAL '99
194900020328     C                   MOVEL     'S9R'         WSX
195000020325E   1C                   ENDIF
195100000000     C                   Z-ADD     1             IN
195200020906     C                   Z-ADD     1             KTBUTE
195300000000     C                   MOVEL     '2L'          KTBCOD
195400000000     C     KEYTA2        SETLL     TABEL00F
195500000000     C     KEYTA2        READE     TABEL00F                               99
195600000000DO  1C     *IN99         DOWEQ     *OFF
195700000000     C                   MOVEL     TBLKEY        FILEST            3            *FILE STATISTICA
195800990217IF  2C     FILEST        IFEQ      WSX
195900000000     C                   MOVEL     TBLUNI        FPE(IN)
196000000000     C                   ADD       1             IN
196100000000E   2C                   ENDIF
196200000000     C     KEYTA2        READE     TABEL00F                               99
196300000000E   1C                   ENDDO
196400000000     C*
196500000000     C                   ENDSR
196600000000     C*------------------------------------------------------------------------*
196700000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
196800000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
196900000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
197000000000     C*------------------------------------------------------------------------*
197100000000     C     DECDIV        BEGSR
197200000000     C*
197300020906     C                   MOVEL     1             KTBUTE
197400000000     C                   MOVE      '17'          KTBCOD
197500000000     C                   MOVE      *BLANKS       KTBKEY
197600000000     C                   MOVEL     DEPDIV        KTBKEY
197700000000     C     KEYTAB        CHAIN     TABEL00F                           99
197800000000     C*
197900000000IF  1C     *IN99         IFEQ      '1'
198000000000     C     TBLFLG        OREQ      '*'
198100000000     C                   MOVEL     *ALL'*'       DESDIV           20
198200000000X   1C                   ELSE
198300000000     C                   MOVEL     TBLUNI        DESDIV
198400000000E   1C                   ENDIF
198500000000     C*
198600000000     C                   ENDSR
198700000000     C*------------------------------------------------------------------------*
198800000000     C*? DECARE - DECODIFICA CODICE AREA
198900000000     C*          INPUT  -- DEPARE: CODICE AREA
199000000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
199100000000     C*------------------------------------------------------------------------*
199200000000     C     DECARE        BEGSR
199300000000     C*
199400020906     C                   MOVEL     1             KTBUTE
199500000000     C                   MOVE      '05'          KTBCOD
199600000000     C                   MOVE      *BLANKS       KTBKEY
199700000000     C                   MOVEL     DEPARE        KTBKEY
199800000000     C     KEYTAB        CHAIN     TABEL00F                           99
199900000000     C*
200000000000IF  1C     *IN99         IFEQ      '1'
200100000000     C     TBLFLG        OREQ      '*'
200200000000     C                   MOVEL     *ALL'*'       DESARE           20
200300000000X   1C                   ELSE
200400000000     C                   MOVEL     TBLUNI        DESARE
200500000000E   1C                   ENDIF
200600000000     C*
200700000000     C                   ENDSR
200800000000     C*------------------------------------------------------------------------*
200900000000     C*? DECFIL - DECODIFICA CODICE FILIALE
201000000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
201100000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
201200000000     C*------------------------------------------------------------------------*
201300000000     C     DECFIL        BEGSR
201400000000     C*
201500000000     C                   Z-ADD     1             I
201600000000     C     DEPFIL        LOOKUP    FIL(I)                                 99
201700000000IF  1C     *IN99         IFEQ      '0'
201800000000     C                   MOVEL     *ALL'*'       DESFIL           20
201900020325     C                   MOVEL     *BLANKS       LAVNTX
202000000000X   1C                   ELSE
202100000000     C                   MOVEL     DFIL(I)       DESFIL
202200000703     C                   MOVEL     NTX(I)        LAVNTX                         *NETWORK
202300000000E   1C                   ENDIF
202400000000     C*
202500000000     C                   ENDSR
202600000000     C*------------------------------------------------------------------------*
202700000000     C*?CARORG - CARICA I CODICI DELLE FILIALI E LE LORO DECODIFICHE
202800000000     C*------------------------------------------------------------------------*
202900000000     C     CARORG        BEGSR
203000000000     C*
203100000000     C                   Z-ADD     *ZEROS        KORFIL
203200000000     C     KEYOR2        SETLL     AZORG01L                           99        *RECORD NON TROVATO
203300000000     C  N99              READ      AZORG01L                               99
203400000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
203500000000DO  1C     *IN99         DOWEQ     '0'
203600000000IF  2C     ORGFVA        IFEQ      *BLANKS
203700000000IF  3C     ORGFAG        IFNE      'V'
203800000703     C                   CLEAR                   OG143
203900000703     C                   MOVEL     ORGDE3        OG143
204000000000     C                   ADD       1             I
204100000000     C                   Z-ADD     ORGFIL        FIL(I)
204200000000     C                   MOVEL     ORGDES        DFIL(I)
204300020325     C* CALCOLA NETWORK
204400020325     C                   MOVEL     §OGNTW        NTX(I)                         *NETWORK
204500040323     C* ELIMINATO NETWORK MES CON COR
204600040323     C                   IF        §OGNTW = 'MES'
204700040323     C                   MOVEL     'COR'         NTX(I)
204800040323     C                   END
204900020325     C* VERIFICA SE TRATTASI DI I/E
205000020325     C                   EXSR      CHKNTW
205100020325     C                   MOVEL     §NTWFIE       FL1(I)
205200020325     C*
205300000703E   3C                   ENDIF
205400000000E   2C                   ENDIF
205500000000     C                   READ      AZORG01L                               99
205600000000E   1C                   ENDDO
205700000000     C*
205800000000     C                   ENDSR
205900020325     C*------------------------------------------------------------------------*
206000020325     C* CHKNTW - CONTROLLA IL NETWORK
206100020325     C*------------------------------------------------------------------------*
206200020325     C     CHKNTW        BEGSR
206300020325     C*
206400020325     C* VERIFICA VALORE IN TABELLA
206500020325     C                   CLEAR                   DNTW
206600020325     C                   CLEAR                   TIBS02
206700020325     C                   MOVEL     'NTW'         T02COD
206800020325     C                   MOVEL     KNSIF         T02SIF
206900020325     C                   MOVEL     §OGNTW        T02KE1
207000020325     C                   MOVEL     'C'           T02MOD
207100020325     C                   CALL      'TIBS02R'
207200020325     C                   PARM                    KPJBA
207300020325     C                   PARM                    TIBS02
207400020325     C*
207500020325     C     T02ERR        IFEQ      *BLANKS
207600020325     C                   MOVEL     T02UNI        DNTW
207700020325     C                   ENDIF
207800020325     C*
207900020325     C                   ENDSR
208000000000     C*------------------------------------------------------------------------*
208100000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
208200000000     C*------------------------------------------------------------------------*
208300000000     C     STAPAR        BEGSR
208400000000     C*
208500000000     C* STAMPA INTESTAZIONE
208600020115     C                   WRITE     SE46PT
208700000000     C*
208800000000     C* DECODIFICHE PARAMETRI FILTRO
208900000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
209000000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
209100000000     C*
209200000000IF  1C     PARMPA        IFEQ      'P'
209300950529     C                   MOVEL     PAR(1)        PADMPA                         *PARTENZE
209400000000X   1C                   ELSE
209500950529     C                   MOVEL     PAR(2)        PADMPA                         *ARRIVI
209600000000E   1C                   ENDIF
209700000000     C*
209800000000IF  1C     PARDET        IFEQ      'S'
209900000000     C                   MOVEL     'SI'          PACDET                         *SI DETTAGLIO
210000000000X   1C                   ELSE
210100000000     C                   MOVEL     'NO'          PACDET                         *NO DETTAGLIO
210200000000E   1C                   ENDIF
210300000000     C*
210400000000IF  1C     PARFIL        IFGT      *ZEROS
210500000000     C                   Z-ADD     PARFIL        DEPFIL
210600000000     C                   EXSR      DECFIL
210700000000     C                   Z-ADD     *ZEROS        DEPFIL
210800000000     C                   MOVEL     PARFIL        PACFIL                         *FILIALE
210900000000     C                   MOVEL     DESFIL        PADFIL
211000000000X   1C                   ELSE
211100000000     C                   MOVEL     *BLANKS       PACFIL
211200000000E   1C                   ENDIF
211300000000     C*
211400000000IF  1C     PARARE        IFGT      *ZEROS
211500000000     C                   Z-ADD     PARARE        DEPARE
211600000000     C                   EXSR      DECARE
211700000000     C                   Z-ADD     *ZEROS        DEPARE
211800000000     C                   MOVEL     PARARE        PACARE                         *AREA
211900000000     C                   MOVEL     DESARE        PADARE
212000000000X   1C                   ELSE
212100000000     C                   MOVEL     *BLANKS       PACARE
212200000000E   1C                   ENDIF
212300000000     C*
212400000000IF  1C     PARDIV        IFNE      *BLANKS
212500000000     C                   MOVEL     PARDIV        DEPDIV
212600000000     C                   EXSR      DECDIV
212700000000     C                   MOVEL     *BLANKS       DEPDIV
212800000000     C                   MOVEL     PARDIV        PACDIV                         *DIVISIONE
212900000000     C                   MOVEL     DESDIV        PADDIV
213000000000E   1C                   ENDIF
213100000000     C*
213200090605      * decodifico tipo servizio richiesto
213300090605     c                   if        parser <> *blanks
213400090605     c                   clear                   ds5e
213500090605     c                   eval      ktbcod = '5E'
213600090605     c                   eval      ktbkey = parser
213700090605     c     keytab        chain     tabel00f
213800090605     c                   if        %found(tabel00f)
213900090605     c                   eval      ds5e = tbluni
214000090605     c                   endif
214100090605     c                   eval      padser = §5edes
214200090605     c                   else
214300090605     c                   eval      padser = 'TUTTI'
214400090605     c                   endif
214500000000     C*
214600000000IF  1C     PARPOR        IFEQ      'F'
214700000000     C                   MOVEL     PAR(6)        PADPOR                         *PORTO FRANCO
214800000000E   1C                   ENDIF
214900000000     C*
215000000000IF  1C     PARPOR        IFEQ      'A'
215100000000     C                   MOVEL     PAR(7)        PADPOR                         *PORTO ASSEGNATO
215200000000E   1C                   ENDIF
215300000000     C*
215400000000IF  1C     PARPOR        IFEQ      ' '
215500000000     C                   MOVEL     PAR(10)       PADPOR                         *TUTTI
215600000000E   1C                   ENDIF
215700000000     C*
215800000000IF  1C     PARSPE        IFEQ      'N'
215900950529     C                   MOVEL     PAR(15)       PADSPE                         *SPEDIZIONE NORMALE
216000000000E   1C                   ENDIF
216100000000     C*
216200000000IF  1C     PARSPE        IFEQ      ' '
216300000000     C                   MOVEL     PAR(10)       PADSPE                         *TUTTE
216400000000E   1C                   ENDIF
216500000000     C*
216600000000IF  1C     PARPRO        IFEQ      'B'
216700000000     C                   MOVEL     PAR(8)        PADPRO                         *PRODOTTO BUSTA
216800000000E   1C                   ENDIF
216900000000     C*
217000000000IF  1C     PARPRO        IFEQ      'P'
217100000000     C                   MOVEL     PAR(9)        PADPRO                         *PRODOTTO PARCEL
217200000000E   1C                   ENDIF
217300000000     C*
217400000000IF  1C     PARPRO        IFEQ      ' '
217500000000     C                   MOVEL     PAR(10)       PADPRO                         *TUTTI
217600000000E   1C                   ENDIF
217700950529     C*
217800950529     C* DATI IN STATISTICA
217900950529     C                   MOVEL     PARIOE        PACIOE
218000950529IF  1C     PARIOE        IFEQ      'I'
218100950529     C                   MOVEL     PAR(12)       PADIOE                         *DATI "ITALIA"
218200950529E   1C                   ENDIF
218300950529IF  1C     PARIOE        IFEQ      'M'
218400950529     C                   MOVEL     PAR(13)       PADIOE                         *DATI "IMPORT"
218500950529E   1C                   ENDIF
218600950529IF  1C     PARIOE        IFEQ      'X'
218700950529     C                   MOVEL     PAR(14)       PADIOE                         *DATI "EXPORT"
218800950529E   1C                   ENDIF
218900950529IF  1C     PARIOE        IFEQ      *BLANKS
219000950602     C                   MOVEL     PAR(16)       PADIOE                         *DATI "TUTTI"
219100950529E   1C                   ENDIF
219200000000     C*
219300000000     C* STAMPA PARAMETRI
219400020115     C                   WRITE     SE46PA
219500000000     C*
219600000000     C                   ENDSR
219700000000     C*------------------------------------------------------------------------*
219800000000     C*?CARTAB - CARICA I DATI IN TABELLE
219900000000     C*------------------------------------------------------------------------*
220000000000     C     CARTAB        BEGSR
220100000000     C*-------------------------
220200000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
220300000000     C*-------------------------
220400020906     C                   Z-ADD     1             KTBUTE
220500000000     C                   MOVEL     '2W'          KTBCOD
220600000000     C     KEYTA2        SETLL     TABEL00F
220700000000     C     KEYTA2        READE     TABEL00F                               99
220800000000     C*
220900000000     C                   Z-ADD     *ZEROS        G                 4 0
221000000000DO  1C     *IN99         DOWEQ     *OFF
221100000000IF  2C     TBLFLG        IFEQ      *BLANKS
221200000000     C                   ADD       1             G
221300000000     C                   MOVEL     TBLKEY        FLAB(G)
221400000000     C                   MOVEL     TBLUNI        DS2W
221500000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
221600000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
221700000000E   2C                   ENDIF
221800000000     C*
221900000000     C     KEYTA2        READE     TABEL00F                               99
222000000000E   1C                   ENDDO
222100000000     C*
222200000000     C                   ENDSR
222300950712     C*------------------------------------------------------------------------*
222400950712     C*?AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
222500950712     C*          OUTPUT--> MES  : MESI DA ELABORARE
222600950712     C*                    ANN  : ANNI DEI MESI DA ELABORARE
222700950712     C*                    PARJ : NUMERO DEI MESI DA ELABORARE
222800950712     C*------------------------------------------------------------------------*
222900950712     C     AMELAB        BEGSR
223000950712     C*
223100950712     C                   CLEAR                   ANN                            *ANNI
223200950712     C                   CLEAR                   MES                            *MESI
223300950712     C                   Z-ADD     *ZEROS        PARJ              2 0          *N° MESI DA ELABORARE
223400950712     C*
223500950712     C* ANNO INIZIALE UGUALE AD ANNO FINALE
223600950712IF  1C     WAA2          IFEQ      WAA
223700950712     C*
223800950712     C* CALCOLO I MESI DI DIFFERENZA
223900950712DO  2C     WMM           DOWLE     WMM2
224000950712     C                   ADD       1             JI
224100950712     C                   Z-ADD     WAA           ANN(JI)
224200950712     C                   Z-ADD     WMM           MES(JI)
224300950712     C                   ADD       1             WMM
224400950712E   2C                   ENDDO
224500950712     C*
224600950712     C                   Z-ADD     JI            PARJ                           *PARAMETRO
224700950712     C*
224800950712     C* IL RESTO DELLA SCHIERA A ZERO
224900950712DO  2C     JI            DOWLT     12
225000950712     C                   ADD       1             JI
225100950712     C                   Z-ADD     *ZEROS        ANN(JI)
225200950712     C                   Z-ADD     *ZEROS        MES(JI)
225300950712E   2C                   ENDDO
225400950712     C*
225500950712     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
225600950712X   1C                   ELSE
225700950712     C*
225800950712     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
225900950712DO  2C     WMM           DOWLE     12
226000950712     C                   ADD       1             JI
226100950712     C                   Z-ADD     WAA           ANN(JI)
226200950712     C                   Z-ADD     WMM           MES(JI)
226300950712     C                   ADD       1             WMM
226400950712E   2C                   ENDDO
226500950712     C*
226600950712     C* MEMORIZZO I MESI DELL'ANNO FINALE
226700950712     C                   Z-ADD     01            WMM
226800950712DO  2C     WMM           DOWLE     WMM2
226900950712     C                   ADD       1             JI
227000950712     C                   Z-ADD     WAA2          ANN(JI)
227100950712     C                   Z-ADD     WMM           MES(JI)
227200950712     C                   ADD       1             WMM
227300950712E   2C                   ENDDO
227400950712     C*
227500950712     C                   Z-ADD     JI            PARJ                           *PARAMETRO
227600950712     C*
227700950712     C* IL RESTO DELLA SCHIERA A ZERO
227800950712DO  2C     JI            DOWLT     12
227900950712     C                   ADD       1             JI
228000950712     C                   Z-ADD     *ZEROS        ANN(JI)
228100950712     C                   Z-ADD     *ZEROS        MES(JI)
228200950712E   2C                   ENDDO
228300950712E   1C                   ENDIF
228400950712     C*
228500950712     C                   ENDSR
228600000000     C*------------------------------------------------------------------------*
228700000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
228800000000     C*------------------------------------------------------------------------*
228900000000     C     *INZSR        BEGSR
229000950529     C*--------------------
229100950529     C* RICEVIMENTO PARAMETRI
229200950529     C*--------------------
229300000000     C     *ENTRY        PLIST
229400000000     C                   PARM                    KPJBA
229500000000     C                   MOVEL     KPJBU         PARAM
229600950529     C*--------------------
229700950529     C* DEFINIZIONE VARIABILI RIFERITE AL DATA BASE
229800950529     C*--------------------
229900000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
230000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
230100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
230200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
230300000000     C     *LIKE         DEFINE    ORGFL3        KORDIV
230400971204     C     *LIKE         DEFINE    SDRANN        KSDANN
230500000000     C     *LIKE         DEFINE    SDRMES        KSDMES
230600000000     C     *LIKE         DEFINE    ORGFIL        KSDFIL
230700000000     C     *LIKE         DEFINE    ORGFIL        KSDFIT
230800000000     C     *LIKE         DEFINE    TBGFIL        KBDFIL
230900000000     C     *LIKE         DEFINE    TBGANN        KBDANN
231000000000     C     *LIKE         DEFINE    TBGMES        KBDMES
231100960108     C     *LIKE         DEFINE    TBGIOE        KBDIOE
231200971204     C     *LIKE         DEFINE    ORGFL3        DEPDIV                         *DEPOSITI
231300971204     C     *LIKE         DEFINE    ORGCAR        DEPARE
231400971204     C     *LIKE         DEFINE    ORGFIL        DEPFIL
231500971204     C     *LIKE         DEFINE    ORGFL1        DLPFL1
231600971204     C     *LIKE         DEFINE    ORGFL1        DLAFL1
231700950529     C     *LIKE         DEFINE    TBGNSP        LAVSPE                         *DI LAVORO
231800000000     C     *LIKE         DEFINE    TBGKGP        LAVCHI
231900000000     C     *LIKE         DEFINE    TBGNCP        LAVCOL
232000000000     C     *LIKE         DEFINE    TBGVOP        LAVVOL
232100950529     C*--------------------
232200950529     C* CHIAVI DI LETTURA
232300950529     C*--------------------
232400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
232500000000     C     KEYTAB        KLIST
232600000000     C                   KFLD                    KTBUTE
232700000000     C                   KFLD                    KTBCOD
232800000000     C                   KFLD                    KTBKEY
232900000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
233000000000     C     KEYTA2        KLIST
233100000000     C                   KFLD                    KTBUTE
233200000000     C                   KFLD                    KTBCOD
233300000000     C* CHIAVE LETTURA AZORG02L - PARZIALE
233400000000     C     KEYORG        KLIST
233500000000     C                   KFLD                    KORDIV                         *DIVISIONE
233600000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
233700000000     C     KEYOR2        KLIST
233800000000     C                   KFLD                    KORFIL                         *FILIALE
233900990609     C* CHIAVE LETTURA SISDR01L/SISDR02L - TOTALE
234000000000     C     KEYSDR        KLIST
234100000000     C                   KFLD                    KSDANN                         *ANNO
234200000000     C                   KFLD                    KSDFIL                         *FILIALE
234300000000     C                   KFLD                    KSDFIT                         *FILIALE DETTAGLIO
234400000000     C                   KFLD                    KSDMES                         *MESE
234500990609     C* CHIAVE LETTURA SISDR01L/SISDR02L - PARZIALE
234600000000     C     KEYSD2        KLIST
234700000000     C                   KFLD                    KSDANN                         *ANNO
234800000000     C                   KFLD                    KSDFIL                         *FILIALE
234900000000     C                   KFLD                    KSDFIT                         *FILIALE DETTAGLIO
235000020114     C* CHIAVE LETTURA TEBDG01L - PARZIALE
235100000000     C     KEYBDG        KLIST
235200000000     C                   KFLD                    KBDFIL                         *FILIALE
235300000000     C                   KFLD                    KBDANN                         *ANNO
235400000000     C                   KFLD                    KBDMES                         *MESE
235500960108     C                   KFLD                    KBDIOE                         *DATI BUDGET
235600950529     C*--------------------
235700950529     C* VARIABILI DI CONTROLLO
235800950529     C*--------------------
235900000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
236000000000     C                   MOVEL     '0'           $FIN1             1            *FINE SALDI ANNO CORR
236100000000     C                   MOVEL     '0'           $FIN2             1            *FINE SALDI ANNO PREC
236200000000     C                   MOVEL     '0'           $FIN3             1            *FINE VALORI BUDGET
236300000000     C                   MOVEL     '0'           PASDET            1            *MEMOR SALDI FIL DET
236400000000     C                   MOVEL     '0'           PASFIL            1            *MEMOR SALDI FILIALE
236500000000     C                   MOVEL     '0'           PASARE            1            *MEMOR SALDI AREA
236600000000     C                   MOVEL     '0'           PASDIV            1            *MEMOR SALDI DIVISION
236700000000     C                   MOVEL     '0'           PASGEN            1            *MEMOR SALDI GENERALI
236800000000     C                   MOVEL     '0'           NOBDG             1            *'1'-NO BDG; '0' SI
236900000000     C                   MOVEL     ' '           TIPANN            1            TIPO ANNI  (U-UGUALI)
237000950529     C                   Z-ADD     UDATE         DATA              6 0          *ATA
237100950529     C                   TIME                    ORA               6 0          *RA
237200000000     C*
237300000000     C* MEMORIZZA I CODICI DELLE FILIALI E LE LORO DECODIFICHE
237400000000     C                   EXSR      CARORG
237500950908     C* CALCOLA I MESI DA ELABORARE
237600990527     C                   Z-ADD     ANNFIN        WAA2              4 0          *ANNO FINALE LAVORO
237700990527     C                   Z-ADD     MESFIN        WMM2              2 0          *MESE FINALE LAVORO
237800990527     C                   Z-ADD     ANNINI        WAA               4 0          *ANNO INIZIALE LAVORO
237900990527     C                   Z-ADD     MESINI        WMM               2 0          *MESE INIZIALE LAVORO
238000990527     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORARE
238100990527     C                   EXSR      AMELAB
238200000000     C*
238300000000     C* DECODIFICA INTESTAZIONE
238400000000     C                   EXSR      DECTES
238500000000     C*
238600000000     C* CARICA TABELLE
238700000000     C                   EXSR      CARTAB
238800000000     C*
238900000000     C* RIEMPO SCHIERA CON ANNO INIZIALE E ANNO FINALE PER LETTURA SALDI CORR.
239000000000     C                   Z-ADD     ANNINI        AIF(1)                         *ANNO INIZIALE
239100000000     C                   Z-ADD     ANNFIN        AIF(2)                         *ANNO FINALE
239200000000     C*
239300000000     C* E SCHIERA CON ANNO INIZIALE E ANNO FINALE - UNO, PER LETTURA SALDI PREC.
239400000000     C     ANNINI        SUB       1             PIF(1)                         *ANNO INIZIALE - 1
239500000000     C     ANNFIN        SUB       1             PIF(2)                         *ANNO FINALE - 1
239600000000     C*
239700000000     C* SE AA INIZIALE =  AA FINALE, QUEST'ULTIMO IN SCHIERA LO IMPOSTO A ZERO
239800000000IF  1C     ANNINI        IFEQ      ANNFIN
239900000000     C                   Z-ADD     *ZEROS        AIF(2)                         *ANNO FINALE
240000000000     C                   Z-ADD     *ZEROS        PIF(2)                         *ANNO FINALE - 1
240100000000     C                   MOVEL     'U'           TIPANN                         *ANNI UGUALI
240200000000E   1C                   ENDIF
240300000000     C*
240400000000     C* A P E R T U R A   F I L E
240500000000     C*
240600000000IF  1C     PARMPA        IFEQ      'P'
240700990609     C                   OPEN      SISDR01L                                     *SALDI PARTENZE
240800000000X   1C                   ELSE
240900990609     C                   OPEN      SISDR02L                                     *SALDI ARRIVI
241000000000E   1C                   ENDIF
241100160505      *accende indicatori per inibire stampa campi occulti per filiale
241200160505     c                   if        knsif = 'FILTRA201' OR
241300160505     c                             knsif = 'FILTRAPFP'
241400160505     C                   seton                                        20
241500160505     c                   endif
241600160504      *
241700160421     c                   if        %subst(knsif:7:1) = 'P'
241800160421     C                   movea     cmd1          cmd
241900160421     c                   else
242000160421     C                   movea     cmd2          cmd
242100160421     c                   endif
242200160421     C                   movel(p)  cmd           comman
242300160421     C                   call      'QCMDEXC'                              98
242400160421     C                   parm                    comman           80
242500160421     C                   parm      49            lung             15 5
242600160421     c                   open      tebdg01l
242700000000     C*
242800000000     C                   ENDSR
242900000000     O*------------------------------------------------------------------------*
243000000703**   PAR - DECODIFICHE PARAMETRI
243100950602PARTENZA       1
243200950602ARRIVO         2
243300090605..Libero       3
243400090605..Libero       4
243500090605..Libero       5
243600950602FRANCO         6
243700950602ASSEGNATO      7
243800950602BUSTE          8
243900950602PARCEL         9
244000090605TUTTI          10
244100950602TUTTE          11
244200950602ITALIA         12
244300950602IMPORT         13
244400950602EXPORT         14
244500950602NORMALE        15
244600950712COMPLETA       16
24470016042117
24480016042118
24490095060219
24500095060220
245100000000**   DMM - DECODIFICHE MESI DELL'ANNO
245200000000GENNAIO                        1
245300000000FEBBRAIO                       2
245400000000MARZO                          3
245500000000APRILE                         4
245600000000MAGGIO                         5
245700000000GIUGNO                         6
245800000000LUGLIO                         7
245900000000AGOSTO                         8
246000000000SETTEMBRE                      9
246100000000OTTOBRE                        10
246200000000NOVEMBRE                       11
246300000000DICEMBRE                       12
246400000000**   DRI - DECODIFICHE RIGA DI STAMPA
246500000000 N.SPEDIZIONI                  1
246600000000   PESO (QLI)                  2
246700000000     N. COLLI                  3
246800000000  VOLUME (MC)                  4
246900120227         %A.P.                 5
247000000000                               6
247100000000 TOTALI                        7
247200000000KG PER SPEDIZ.                 8
247300000000COLLI PER SPED                 9
247400000000MC PER SPEDIZ.                 10
247500160421**         CMD1
247600160421OVRDBF FILE(tebdg01L) TOFILE(gaitragrps/tebdg01l)
247700160421**         CMD2
247800160421OVRDBF FILE(tebdg01l)  TOFILE(gaitragru/tebdg01l)
