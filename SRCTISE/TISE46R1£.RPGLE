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
001100000703     FSIPCP01L  UF A E           K DISK    USROPN
001200000703     FSIPCA01L  UF A E           K DISK    USROPN
001300100413     FSIPCP11L  UF A E           K DISK    USROPN prefix(N_)
001400100413     FSIPCA11L  UF A E           K DISK    USROPN prefix(N_)
001500020114     FTEBDG01L  IF   E           K DISK
001600000000     FAZORG01L  IF   E           K DISK
001700000000     FAZORG02L  IF   E           K DISK
001800000000     F                                     RENAME(AZORG:AZORG2)
001900000000     FTABEL00F  IF   E           K DISK
002000020115     FTISE46P   O    E             PRINTER
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE                                                                *
002300000000     D*------------------------------------------------------------------------*
002400000000     D*------------------
002500000614     D* CODICI/DESCRIZIONI FILIALI
002600000000     D*------------------
002700971124     D FIL             S              3  0 DIM(500)
002800971124     D DFIL            S             20    DIM(500)
002900971204     D FL1             S              1    DIM(500)
003000020325     D NTX             S              3    DIM(500)                             *NETWORK
003100000000     D*------------------
003200000000     D* DECODIFICHE FASCE DI PESO
003300000000     D*------------------
003400020610     D FPE             S             14    DIM(10)
003500000000     D*------------------
003600000000     D* MESI DA ELABORARE
003700000000     D*------------------
003800000000     D MES             S              2  0 DIM(12)
003900000000     D*------------------
004000000000     D* ANNI DEI MESI DA ELABORARE
004100950529     D*------------------
004200990527     D ANN             S              4  0 DIM(12)
004300000000     D*------------------
004400000000     D* ANNO INIZIALE E ANNO FINALE ELABORAZIONE SALDI CORRENTI
004500000000     D*------------------
004600990527     D AIF             S              4  0 DIM(2)
004700000000     D*------------------
004800000000     D* ANNO INIZIALE E ANNO FINALE ELABORAZIONE SALDI PRECEDENTI
004900000000     D*------------------
005000990527     D PIF             S              4  0 DIM(2)
005100000000     D*------------------
005200000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
005300000000     D*------------------
005400000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
005500000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/N)
005600000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
005700000000     D*------------------
005800000000     D* DECODIFICA PARAMETRI FILTRO
005900000000     D*------------------
006000950602     D PAR             S             15    DIM(20) CTDATA PERRCD(1)
006100000000     D*------------------
006200000000     D* DECODIFICA MESI DELL'ANNO
006300000000     D*------------------
006400000000     D DMM             S              9    DIM(12) CTDATA PERRCD(1)
006500000000     D*------------------
006600000000     D* DECODIFICA RIGHE DI STAMPA
006700000000     D*------------------
006800000000     D DRI             S             14    DIM(10) CTDATA PERRCD(1)
006900000000     D*------------------
007000000000     D* DECODIFICA CODICE DIVISIONE
007100000000     D*------------------
007200980206     D*---                DIV     1  10 20
007300000000     D*------------------
007400000000     D* DESCRIZIONE COMANDI 'QCMDEXC'
007500000000     D*------------------
007600000000     D CMD1            S             45    DIM(1) CTDATA PERRCD(1)
007700000000     D CMD2            S             45    DIM(1) CTDATA PERRCD(1)
007800000000     D*------------------
007900020610     D* SCHIERA MULTIPLA PER TOTALI GENERALI           - 10 FASCE -
008000000000     D*------------------
008100000000     D*------------------
008200020610     D* SCHIERA MULTIPLA PER TOTALI DIVISIONE          - 10 FASCE -
008300000000     D*------------------
008400000000     D*------------------
008500020610     D* SCHIERA MULTIPLA PER TOTALI AREA               - 10 FASCE -
008600000000     D*------------------
008700000000     D*------------------
008800020610     D* SCHIERA MULTIPLA PER TOTALI FILIALE            - 10 FASCE -
008900000000     D*------------------
009000000000     D*------------------
009100020610     D* SCHIERA MULTIPLA PER TOTALI FILIALE DETTAGLIO  - 10 FASCE -
009200000000     D*------------------
009300000000     D*------------------
009400000000     D* SCHIERA MULTIPLA PER STAMPA TOTALI
009500000000     D*------------------
009600000000     D*------------------
009700000000     D* SCHIERE DI LAVORO PER TOTALI A FINE PAGINA
009800000000     D*------------------
009900000000     D SPW             S             13  0 DIM(12)                              SPEDIZIONI
010000000000     D SBW             S             13  0 DIM(12)                              SPEDIZIONI BUDGET
010100000000     D SAW             S             13  0 DIM(12)                              SPEDIZIONI ANNO PREC.
010200000000     D QTW             S             15  1 DIM(12)                              KILOGRAMMI
010300000000     D QBW             S             15  1 DIM(12)                              KILOGRAMMI BUDGET
010400000000     D QAW             S             15  1 DIM(12)                              KILOGRAMMI ANNO PREC.
010500000000     D CLW             S             13  0 DIM(12)                              COLLI
010600000000     D CAW             S             13  0 DIM(12)                              COLLI ANNO PRECEDENTE
010700000000     D VLW             S             15  3 DIM(12)                              VOLUME
010800000000     D VAW             S             15  3 DIM(12)                              VOLUME ANNO PRECEDENTE
010900000000     D CBW             S             11  0 DIM(12)                              COLLI BUDGET
011000000000     D VBW             S             13  3 DIM(12)                              VOLUME BUDGET
011100000000     D*------------------
011200000000     D* SCHIERE ULTIME DA PORTARE IN STAMPA
011300000000     D*------------------
011400000000     D $VA             S             17  3 DIM(12)                              VALORE
011500000000     D $BD             S             17  3 DIM(12)                              VALORE BUDGET
011600000000     D $AP             S             17  3 DIM(12)                              VALORE ANNO PRECEDENTE
011700000000     D $VS             S             17  3 DIM(12)                              VALORE SUPER. RAPPORTO
011800000000     D $VI             S             17  3 DIM(12)                              VALORE INFER. RAPPORTO
011900000000     D $PB             S              4  1 DIM(12)                              % BUDGET
012000000000     D $PA             S              4  1 DIM(12)                              % ANNO PRECEDENTE
012100000000     D $RP             S              9  3 DIM(12)                              RAPPORTO
012200000000     D*------------------
012300000000     D* TABELLE CAPICONTO
012400000000     D*------------------
012500000000     D*------------------------------------------------------------------------*
012600000000     D* INPUT                                                                  *
012700000000     D*------------------------------------------------------------------------*
012800000000     D KPJBA         E DS
012900000000     D  PROFIL                 1     10                                         UTENTE
013000000000     D*------------------
013100000000     D* NUMERO DEI BYTE IN PACKED:
013200000000     D*------------------
013300000000     D* SPEDIZIONI  (11:2) + 1 =  6,5  /  (6 X 12) = 72
013400000000     D* KILOGRAMMI  (13:2) + 1 =  7,5  /  (7 X 12) = 84
013500000000     D* COLLI       (11:2) + 1 =  6,5  /  (6 X 12) = 72
013600000000     D* VOLUME      (13:2) + 1 =  7,5  /  (7 X 12) = 84
013700000000     D*------------------
013800020610     D* DS MULTIPLA FASCE DI PESO PER TOTALI GENERALI     - 10 FASCE X 12 MESI -
013900000000     D*------------------
014000020610     D FPGEN           DS                  OCCURS(10) INZ
014100000000     D  SPG                    1     72P 0                                      SPE*12
014200000000     D                                     DIM(12)                              SPEDIZIONI
014300000000     D  SBG                   73    144P 0                                      %BG*12
014400000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
014500000000     D  SAG                  145    216P 0                                      %AP*12
014600000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
014700000000     D  QTG                  217    300P 1                                      QUI*12
014800000000     D                                     DIM(12)                              KILOGRAMMI
014900000000     D  QBG                  301    384P 1                                      %BG*12
015000000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
015100000000     D  QAG                  385    468P 1                                      %AP*12
015200000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
015300000000     D  CLG                  469    540P 0                                      COL*12
015400000000     D                                     DIM(12)                              COLLI
015500000000     D  CAG                  541    612P 0                                      %AP*12
015600000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
015700000000     D  VLG                  613    696P 3                                      VOL*12
015800000000     D                                     DIM(12)                              VOLUME
015900000000     D  VAG                  697    780P 3                                      %AP*12
016000000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
016100000000     D  CBG                  781    852P 0                                      %BD*12
016200000000     D                                     DIM(12)                              COLLI BUDGET
016300000000     D  VBG                  853    936P 3                                      %BD*12
016400000000     D                                     DIM(12)                              VOLUME BUDGET
016500000000     D*------------------
016600020610     D* DS MULTIPLA FASCE DI PESO PER DIVISIONE           - 10 FASCE X 12 MESI -
016700000000     D*------------------
016800020610     D FPDIV           DS                  OCCURS(10) INZ
016900000000     D  SPD                    1     72P 0                                      SPE*12
017000000000     D                                     DIM(12)                              SPEDIZIONI
017100000000     D  SBD                   73    144P 0                                      %BG*12
017200000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
017300000000     D  SAD                  145    216P 0                                      %AP*12
017400000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
017500000000     D  QTD                  217    300P 1                                      QUI*12
017600000000     D                                     DIM(12)                              KILOGRAMMI
017700000000     D  QBD                  301    384P 1                                      %BG*12
017800000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
017900000000     D  QAD                  385    468P 1                                      %AP*12
018000000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
018100000000     D  CLD                  469    540P 0                                      COL*12
018200000000     D                                     DIM(12)                              COLLI
018300000000     D  CAD                  541    612P 0                                      %AP*12
018400000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
018500000000     D  VLD                  613    696P 3                                      VOL*12
018600000000     D                                     DIM(12)                              VOLUME
018700000000     D  VAD                  697    780P 3                                      %AP*12
018800000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
018900000000     D  CBD                  781    852P 0                                      %BD*12
019000000000     D                                     DIM(12)                              COLLI BUDGET
019100000000     D  VBD                  853    936P 3                                      %BD*12
019200000000     D                                     DIM(12)                              VOLUME BUDGET
019300000000     D*------------------
019400020610     D* DS MULTIPLA FASCE DI PESO PER AREA                - 10 FASCE X 12 MESI -
019500000000     D*------------------
019600020610     D FPARE           DS                  OCCURS(10) INZ
019700000000     D  SPA                    1     72P 0                                      SPE*12
019800000000     D                                     DIM(12)                              SPEDIZIONI
019900000000     D  SBA                   73    144P 0                                      %BG*12
020000000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
020100000000     D  SAA                  145    216P 0                                      %AP*12
020200000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
020300000000     D  QTA                  217    300P 1                                      QUI*12
020400000000     D                                     DIM(12)                              KILOGRAMMI
020500000000     D  QBA                  301    384P 1                                      %BG*12
020600000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
020700000000     D  QAA                  385    468P 1                                      %AP*12
020800000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
020900000000     D  CLA                  469    540P 0                                      COL*12
021000000000     D                                     DIM(12)                              COLLI
021100000000     D  CAA                  541    612P 0                                      %AP*12
021200000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
021300000000     D  VLA                  613    696P 3                                      VOL*12
021400000000     D                                     DIM(12)                              VOLUME
021500000000     D  VAA                  697    780P 3                                      %AP*12
021600000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
021700000000     D  CBA                  781    852P 0                                      %BD*12
021800000000     D                                     DIM(12)                              COLLI BUDGET
021900000000     D  VBA                  853    936P 3                                      %BD*12
022000000000     D                                     DIM(12)                              VOLUME BUDGET
022100000000     D*------------------
022200020610     D* DS MULTIPLA FASCE DI PESO PER FILIALE             - 10 FASCE X 12 MESI -
022300000000     D*------------------
022400020610     D FPFIL           DS                  OCCURS(10) INZ
022500000000     D  SPF                    1     72P 0                                      SPE*12
022600000000     D                                     DIM(12)                              SPEDIZIONI
022700000000     D  SBF                   73    144P 0                                      %BG*12
022800000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
022900000000     D  SAF                  145    216P 0                                      %AP*12
023000000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
023100000000     D  QTF                  217    300P 1                                      QUI*12
023200000000     D                                     DIM(12)                              KILOGRAMMI
023300000000     D  QBF                  301    384P 1                                      %BG*12
023400000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
023500000000     D  QAF                  385    468P 1                                      %AP*12
023600000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
023700000000     D  CLF                  469    540P 0                                      COL*12
023800000000     D                                     DIM(12)                              COLLI
023900000000     D  CAF                  541    612P 0                                      %AP*12
024000000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
024100000000     D  VLF                  613    696P 3                                      VOL*12
024200000000     D                                     DIM(12)                              VOLUME
024300000000     D  VAF                  697    780P 3                                      %AP*12
024400000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
024500000000     D  CBF                  781    852P 0                                      %BD*12
024600000000     D                                     DIM(12)                              COLLI BUDGET
024700000000     D  VBF                  853    936P 3                                      %BD*12
024800000000     D                                     DIM(12)                              VOLUME BUDGET
024900000000     D*------------------
025000020610     D* DS MULTIPLA FASCE DI PESO PER FILIALE DETTAGLIO   - 10 FASCE X 12 MESI -
025100000000     D*------------------
025200020610     D FPDET           DS                  OCCURS(10) INZ
025300000000     D  SPT                    1     72P 0                                      SPE*12
025400000000     D                                     DIM(12)                              SPEDIZIONI
025500000000     D  SAT                   73    144P 0                                      %AP*12
025600000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
025700000000     D  QTT                  145    228P 1                                      QUI*12
025800000000     D                                     DIM(12)                              KILOGRAMMI
025900000000     D  QAT                  229    312P 1                                      %AP*12
026000000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
026100000000     D  CLT                  313    384P 0                                      COL*12
026200000000     D                                     DIM(12)                              COLLI
026300000000     D  CAT                  385    456P 0                                      %AP*12
026400000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
026500000000     D  VLT                  457    540P 3                                      VOL*12
026600000000     D                                     DIM(12)                              VOLUME
026700000000     D  VAT                  541    624P 3                                      %AP*12
026800000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
026900000000     D*------------------
027000020610     D* DS MULTIPLA FASCE DI PESO DA PORTARE IN STAMPA    - 10 FASCE X 12 MESI -
027100000000     D*------------------
027200020610     D FPSTA           DS                  OCCURS(10) INZ
027300000000     D  SPS                    1     72P 0                                      SPE*12
027400000000     D                                     DIM(12)                              SPEDIZIONI
027500000000     D  SBS                   73    144P 0                                      %BG*12
027600000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
027700000000     D  SAS                  145    216P 0                                      %AP*12
027800000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
027900000000     D  QTS                  217    300P 1                                      QUI*12
028000000000     D                                     DIM(12)                              KILOGRAMMI
028100000000     D  QBS                  301    384P 1                                      %BG*12
028200000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
028300000000     D  QAS                  385    468P 1                                      %AP*12
028400000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
028500000000     D  CLS                  469    540P 0                                      COL*12
028600000000     D                                     DIM(12)                              COLLI
028700000000     D  CAS                  541    612P 0                                      %AP*12
028800000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
028900000000     D  VLS                  613    696P 3                                      VOL*12
029000000000     D                                     DIM(12)                              VOLUME
029100000000     D  VAS                  697    780P 3                                      %AP*12
029200000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
029300000000     D  CBS                  781    852P 0                                      %BD*12
029400000000     D                                     DIM(12)                              COLLI BUDGET
029500000000     D  VBS                  853    936P 3                                      %BD*12
029600000000     D                                     DIM(12)                              VOLUME BUDGET
029700000000     D*------------------
029800000000     D* PARAMETRI FILTRO
029900000000     D*------------------
030000990527     D PARAM           DS
030100990527     D  PARDAT                 1      6  0
030200990527     D  ANNINI                 1      4  0
030300990527     D  MESINI                 5      6  0
030400990527     D  PARDA2                 7     12  0
030500990527     D  ANNFIN                 7     10  0
030600990527     D  MESFIN                11     12  0
030700990527     D  PARMPA                13     13
030800990527     D  PARFIL                14     16  0
030900990527     D  PARARE                17     19  0
031000990527     D  PARDIV                20     20
031100990527     D  PARSER                21     21
031200990527     D  PARPOR                22     22
031300990527     D  PARSPE                23     23
031400990527     D  PARPRO                24     24
031500990527     D  PARDET                25     25
031600990527     D  PARFPC                26     26
031700990527     D  PARIOE                27     27
031800000703     D  PARPOS                28     28
031900990527     D*------------------
032000020114     D* DS COMPOSIZIONE DATA(6) - TEBDG
032100990527     D*------------------
032200990527     D                 DS
032300990527     D  TBGANN                 1      4  0
032400990527     D  TBGMES                 5      6  0
032500990527     D  TBGDAT                 1      6  0
032600000000     D*------------------
032700000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
032800000000     D*------------------
032900000000     D DS2W          E DS
033000000216     D*---
033100000703     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA
033200000216     D*---
033300000216     D OG143         E DS
033400020325     D*-------------------
033500020325     D* DS DEFINIZIONE TABELLA "NETWORK"
033600020325     D*-------------------
033700020325     D DNTW          E DS
033800020325     D*-------------------
033900020325     D* DS RICHIAMO SCELTA TABELLE
034000020325     D*-------------------
034100020325     D TIBS02        E DS                  EXTNAME(TIBS02DS)
034200090605
034300090605     d ds5e          e ds
034400100413     d
034500100413     d kcptsp          s                   like(n_pcptsp)
034600100413     d kcatsp          s                   like(n_pcatsp)
034700100413     C*------------------------------------------------------------------------*
034800950529     C*?MAIN LINE
034900000000     C*------------------------------------------------------------------------*
035000000000     C*
035100000000     C* STAMPA PARAMETRI FILTRO
035200000000     C                   EXSR      STAPAR
035300000000     C*
035400000000     C* POSIZIONAMENTO SUL FILE ORGANIGRAMMI E PRIMA LETTURA
035500000000     C                   EXSR      SETFIL
035600000000     C*
035700000000     C* CICLO FINO A FINE FILE ORGANIGRAMMI
035800000000DO  1C     $FINE         DOWEQ     '0'
035900000000     C*
036000000000     C* OPERAZIONI PER NUOVA DIVISIONE
036100000000     C                   EXSR      NEWDIV
036200000000     C*
036300000000     C* CICLO FINO A ROTTURA DI DIVISIONE
036400000000DO  2C     $FINE         DOWEQ     '0'
036500000000     C     ORGFL3        ANDEQ     DEPDIV
036600000000     C*
036700000000     C* OPERAZIONI PER NUOVA AREA
036800000000     C                   EXSR      NEWARE
036900000000     C*
037000000000     C* CICLO FINO A ROTTURA DI AREA
037100000000DO  3C     $FINE         DOWEQ     '0'
037200000000     C     ORGFL3        ANDEQ     DEPDIV
037300000000     C     ORGCAR        ANDEQ     DEPARE
037400000000     C*
037500000000     C* OPERAZIONI PER NUOVA FILIALE
037600000000     C                   EXSR      NEWFIL
037700000000     C*
037800000000     C* CICLO LETTURA FILIALI DETTAGLIO
037900971124DO  4C     1             DO        500           F                 3 0
038000000000IF  5C     FIL(F)        IFGT      *ZEROS
038100000000     C*
038200000000     C* OPERAZIONI PER NUOVA FILIALE DETTAGLIO
038300000000     C                   EXSR      NEWDET
038400000703     C*
038500020409 xxx C* CONTROLLA VALIDITA' DEL DETTAGLIO
038600000703IF  6C     PARPOS        IFEQ      'S'                                          *DATI POSTE
038700020409     C     DEPNTF        ANDEQ     'PPT'                                         FILIALE POSTE
038800000703     C     PARPOS        OREQ      'S'                                          *DATI POSTE
038900020409     C     DEPNTT        ANDEQ     'PPT'                                         FILIALE DETTAGLIO POSTE
039000000703     C     PARPOS        OREQ      'N'                                          *NO DATI POSTE
039100020409     C     DEPNTF        ANDNE     'PPT'                                         NO FILIALE POSTE
039200020409     C     DEPNTT        ANDNE     'PPT'                                         NO FILIALE DETTAGLIO POSTE
039300000000     C*
039400000000     C* LETTURA SALDI AA INIZIALE, E AA FINALE SOLO SE DIVERSO DALL'INIZIALE
039500000000     C                   Z-ADD     1             J                 1 0
039600000703DO  7C                   DO        2             J
039700000000     C                   MOVEL     '0'           $FIN1                          *REIMPOSTO FINE FILE
039800000000     C*
039900000000     C* SECONDO GIRO SOLO SE ANNO FINALE DIVERSO DALL' INIZIALE (PIENO)
040000000703IF  8C     AIF(J)        IFNE      *ZEROS
040100000000     C*
040200000000     C* POSIZIONAMENTO SUL FILE SALDI ANNO CORRENTE E PRIMA LETTURA
040300000000     C                   EXSR      SETFI1
040400000000     C*
040500000000     C* CICLO FINO A FINE FILE SALDI O ROTTURA FILIALE
040600000703DO  9C     $FIN1         DOWEQ     '0'
040700000000     C*
040800000000     C* MEMORIZZA I SALDI CORRENTI DELLA FILIALE LETTA DALL'ORGANIGRAMMA
040900000000     C                   EXSR      MEMSAL
041000000000     C*
041100000000     C* LETTURA RECORD SALDI SUCCESSIVO
041200000000     C                   EXSR      LETFI1
041300000000     C*
041400000703E   9C                   ENDDO
041500000000     C*
041600000703E   8C                   ENDIF
041700000703E   7C                   ENDDO
041800000000     C*
041900000000     C* CALCOLA I SALDI DELL' ANNO PRECEDENTE DELLA FILIALE DETTAGLIO
042000000000     C                   EXSR      CALANP
042100000704     C*
042200000704     C* SCRIVE I SALDI MOVIMENTI DELLA FILIALE SUL FILE PC -NEW-
042300000704IF  7C     PARFPC        IFEQ      'S'
042400000704     C                   EXSR      SCRSIX                                       *SCRIVE FILE PC -NEW-
042500000704E   7C                   ENDIF
042600000000     C*
042700000000     C* SE RICHIESTO DETTAGLIO STAMPO FILIALE
042800000703IF  7C     PARDET        IFEQ      'S'                                          *SE MEMORIZZATI SALDI
042900000000     C     PASDET        ANDEQ     '1'
043000000000     C*
043100000000     C* IMPOSTA STAMPA TOTALI FILIALE DETTAGLIO
043200000000     C                   EXSR      STADET
043300000703E   7C                   ENDIF
043400000703E   6C                   ENDIF
043500000000     C*
043600000000E   5C                   ENDIF
043700000000E   4C                   ENDDO                                                  *FIL/DET SUCESSIVA
043800000000     C*
043900000000     C* CALCOLA I VALORI DEL BUDGET
044000000000IF  4C     PASFIL        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
044100000000     C                   EXSR      CALBDG
044200000000E   4C                   ENDIF
044300000000     C*
044400000000     C* IMPOSTA STAMPA TOTALI FILIALE
044500000000IF  4C     PASFIL        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
044600000000     C                   EXSR      STAFIL
044700000000E   4C                   ENDIF
044800000000     C*
044900000000     C* LETTURA RECORD ORGANIGRAMMI SUCCESSIVO
045000000000     C                   EXSR      LETFIL
045100000000     C*
045200000000E   3C                   ENDDO
045300000000     C*
045400000000     C* IMPOSTA STAMPA TOTALI AREA
045500000000IF  3C     PASARE        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
045600000000     C                   EXSR      STAARE
045700000000E   3C                   ENDIF
045800000000     C*
045900000000E   2C                   ENDDO
046000000000     C*
046100000000     C* IMPOSTA STAMPA TOTALI DIVISIONE
046200000000IF  2C     PASDIV        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
046300000000     C                   EXSR      STADIV
046400000000E   2C                   ENDIF
046500000000     C*
046600000000E   1C                   ENDDO
046700000000     C*
046800000000     C* IMPOSTA STAMPA TOTALI GENERALI
046900000000IF  1C     PASGEN        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
047000000000     C                   EXSR      STAGEN
047100000000E   1C                   ENDIF
047200000000     C*
047300000000     C* STAMPA RIGA DI FINE PROSPETTO
047400020115     C                   WRITE     SE46FI
047500000000     C*
047600000000     C* C H I U S U R A   F I L E
047700000000     C*
047800000000IF  1C     PARMPA        IFEQ      'P'
047900990609     C                   CLOSE     SISDR01L                                     *SALDI PARTENZE
048000000000IF  2C     PARFPC        IFEQ      'S'
048100000703     C                   CLOSE     SIPCP01L                                     *SALDI PARTENZE PC NEW
048200120820     C                   CLOSE     SIPCP11L                                     *SALDI PARTENZE PC NEW
048300000000E   2C                   ENDIF
048400000000     C*
048500000000X   1C                   ELSE
048600990609     C                   CLOSE     SISDR02L                                     *SALDI ARRIVI
048700000000IF  2C     PARFPC        IFEQ      'S'
048800000703     C                   CLOSE     SIPCA01L                                     *SALDI ARRIVI PC NEW
048900120820     C                   CLOSE     SIPCA11L                                     *SALDI ARRIVI PC NEW
049000000000E   2C                   ENDIF
049100000000     C*
049200000000E   1C                   ENDIF
049300000000     C*
049400000000     C                   SETON                                        LR
049500000000     C*------------------------------------------------------------------------*
049600000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ORGANIGRAMMI
049700000000     C*------------------------------------------------------------------------*
049800000000     C     SETFIL        BEGSR
049900000000     C*
050000000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE ORGANIGRAMMI
050100000000     C                   MOVEL     PARDIV        KORDIV
050200000000     C     KEYORG        SETLL     AZORG02L                           99        *FINE ARCHIVIO
050300000000     C*
050400000000IF  1C     *IN99         IFEQ      '1'
050500000000     C                   MOVEL     '1'           $FINE
050600000000X   1C                   ELSE
050700000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
050800000000E   1C                   ENDIF
050900000000     C*
051000000000     C                   ENDSR
051100000000     C*------------------------------------------------------------------------*
051200000000     C*?LETFIL - LETTURA PROSSIMO RECORD FILE ORGANIGRAMMI
051300000000     C*------------------------------------------------------------------------*
051400000000     C     LETFIL        BEGSR
051500000000     C*
051600000000     C* LEGGO FINO A:
051700000000     C*     - FINE FILE
051800000000     C*     - TROVATO RECORD VALIDO
051900000000     C                   MOVEL     'N'           WRECOK
052000000000DO  1C     $FINE         DOWEQ     '0'
052100000000     C     WRECOK        ANDEQ     'N'
052200000000     C*
052300000000     C* LETTURE SUCCESSIVE ALLA PRIMA
052400000000     C* SE SELEZIONATA LA DIVISIONE LEGGO ANCHE PER UGUALE
052500000000IF  2C     PARDIV        IFGT      *BLANKS
052600000000     C     KEYORG        READE     AZORG02L                               99
052700000000X   2C                   ELSE
052800000000     C                   READ      AZORG02L                               99
052900000000E   2C                   ENDIF
053000000703     C*
053100000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
053200000000     C                   MOVEL     '1'           $FINE
053300000000X   2C                   ELSE
053400000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
053500000000E   2C                   ENDIF
053600000000     C*
053700000000E   1C                   ENDDO
053800000000     C*
053900000000     C                   ENDSR
054000000000     C*------------------------------------------------------------------------*
054100000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD ORGANIGRAMMA
054200000000     C*------------------------------------------------------------------------*
054300000000     C     CHKREC        BEGSR
054400000000     C*
054500000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
054600000703     C*
054700000703     C* IMPOSTAZIONI
054800000703     C                   CLEAR                   OG143
054900000703     C                   MOVEL     ORGDE3        OG143
055000000000     C*
055100000000     C* ESCLUDO LE FILIALI ANNULLATE
055200000000IF  1C     ORGFVA        IFNE      *BLANKS
055300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055400000000E   1C                   ENDIF
055500000000     C*
055600000000     C* ESCLUDO LE VOCI VARIE
055700000000IF  1C     ORGFAG        IFEQ      'V'
055800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055900000000E   1C                   ENDIF
056000000000     C*
056100000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
056200000000IF  1C     PARARE        IFNE      *ZEROS
056300000000     C     ORGCAR        ANDNE     PARARE
056400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056500000000E   1C                   ENDIF
056600000000     C*
056700000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
056800000000IF  1C     PARFIL        IFNE      *ZEROS
056900000000     C     ORGFIL        ANDNE     PARFIL
057000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057100000000E   1C                   ENDIF
057200000000     C*
057300000000     C                   ENDSR
057400000000     C*------------------------------------------------------------------------*
057500000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
057600000000     C*------------------------------------------------------------------------*
057700000000     C     NEWDIV        BEGSR
057800000000     C*
057900000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
058000000000     C                   MOVEL     ORGFL3        DEPDIV
058100000000     C*
058200000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
058300000000     C                   EXSR      DECDIV
058400000000     C*
058500000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
058600000000     C                   MOVEL     '0'           PASDIV
058700000000     C*
058800000000     C* AZZERA I TOTALI DELLA DIVISIONE
058900000000DO  1C                   DO        10            A                 2 0
059000000000     C     A             OCCUR     FPDIV
059100000000     C                   CLEAR                   SPD
059200000000     C                   CLEAR                   SBD
059300000000     C                   CLEAR                   SAD
059400000000     C                   CLEAR                   QTD
059500000000     C                   CLEAR                   QBD
059600000000     C                   CLEAR                   QAD
059700000000     C                   CLEAR                   CLD
059800000000     C                   CLEAR                   CAD
059900000000     C                   CLEAR                   VLD
060000000000     C                   CLEAR                   VAD
060100000000     C                   CLEAR                   CBD
060200000000     C                   CLEAR                   VBD
060300000000E   1C                   ENDDO
060400000000     C*
060500000000     C                   ENDSR
060600000000     C*------------------------------------------------------------------------*
060700000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
060800000000     C*------------------------------------------------------------------------*
060900000000     C     NEWARE        BEGSR
061000000000     C*
061100000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
061200000000     C                   MOVEL     ORGCAR        DEPARE
061300000000     C*
061400000000     C* DECODIFICA IL CODICE AREA
061500000000     C                   EXSR      DECARE
061600000000     C*
061700000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
061800000000     C                   MOVEL     '0'           PASARE
061900000000     C*
062000000000     C* AZZERA I TOTALI DELL'AREA
062100000000DO  1C                   DO        10            A
062200000000     C     A             OCCUR     FPARE
062300000000     C                   CLEAR                   SPA
062400000000     C                   CLEAR                   SBA
062500000000     C                   CLEAR                   SAA
062600000000     C                   CLEAR                   QTA
062700000000     C                   CLEAR                   QBA
062800000000     C                   CLEAR                   QAA
062900000000     C                   CLEAR                   CLA
063000000000     C                   CLEAR                   CAA
063100000000     C                   CLEAR                   VLA
063200000000     C                   CLEAR                   VAA
063300000000     C                   CLEAR                   CBA
063400000000     C                   CLEAR                   VBA
063500000000E   1C                   ENDDO
063600000000     C*
063700000000     C                   ENDSR
063800000000     C*------------------------------------------------------------------------*
063900000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA DEI SALDI ANNO CORRENTE
064000000000     C*------------------------------------------------------------------------*
064100000000     C     SETFI1        BEGSR
064200000000     C*
064300000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE SALDI
064400000000     C                   Z-ADD     AIF(J)        KSDANN                         *AA INIZIALE O FINALE
064500000000     C                   Z-ADD     ORGFIL        KSDFIL                         *FILIALE
064600000000     C                   Z-ADD     FIL(F)        KSDFIT                         *FILIALE DETTAGLIO
064700000000     C*
064800000000     C* MI POSIZIONO CON IL MESE INIZIALE SE ANNI UGUALI O PRIMO ANNO DIVERSO,
064900000000     C* OPPURE CON IL MESE = 01 SE GLI ANNI SONO DIVERSI E STO ELABORANDO IL 2°
065000000000IF  1C     TIPANN        IFEQ      'U'
065100000000     C     J             OREQ      1
065200000000     C                   Z-ADD     MESINI        KSDMES                         *MESE INIZIALE
065300000000E   1C                   ELSE
065400000000     C                   Z-ADD     01            KSDMES                         *MESE 01
065500000000E   1C                   ENDIF
065600000000     C* PARTENZE
065700000000IF  1C     PARMPA        IFEQ      'P'
065800990609     C     KEYSDR        SETLL     SISDR01L                           99        *FINE ARCHIVIO
065900000000X   1C                   ELSE
066000000000     C* ARRIVI
066100990609     C     KEYSDR        SETLL     SISDR02L                           99        *FINE ARCHIVIO
066200000000E   1C                   ENDIF
066300000000     C*
066400000000IF  1C     *IN99         IFEQ      '1'
066500000000     C                   MOVEL     '1'           $FIN1
066600000000X   1C                   ELSE
066700000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
066800000000E   1C                   ENDIF
066900000000     C*
067000000000     C                   ENDSR
067100000000     C*------------------------------------------------------------------------*
067200000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI ANNO CORRENTE
067300000000     C*------------------------------------------------------------------------*
067400000000     C     LETFI1        BEGSR
067500000000     C*
067600000000     C* LEGGO FINO A:
067700000000     C*     - FINE FILE
067800000000     C*     - TROVATO RECORD VALIDO
067900000000     C                   MOVEL     'N'           WRECOK
068000000000DO  1C     $FIN1         DOWEQ     '0'
068100000000     C     WRECOK        ANDEQ     'N'
068200000000     C*
068300000000     C* LETTURE SUCCESSIVE ALLA PRIMA (ANNO + FILIALE + DETTAGLIO)
068400000000IF  1C     PARMPA        IFEQ      'P'
068500990609     C     KEYSD2        READE     SISDR01L                               99
068600000000X   1C                   ELSE
068700990609     C     KEYSD2        READE     SISDR02L                               99
068800000000E   1C                   ENDIF
068900000000     C*
069000000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
069100000000     C                   MOVEL     '1'           $FIN1
069200000000X   2C                   ELSE
069300000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
069400000000E   2C                   ENDIF
069500000000     C*
069600000000E   1C                   ENDDO
069700000000     C*
069800000000     C                   ENDSR
069900000000     C*------------------------------------------------------------------------*
070000000000     C*?CHKRE1 - CONTROLLA VALIDITA' DEI SALDI ANNO CORRENTE/PRECEDENTE
070100000000     C*------------------------------------------------------------------------*
070200000000     C     CHKRE1        BEGSR
070300000000     C*
070400000000     C* °ANNO INIZIALE = ANNO FINALE: IL MESE LETTO DEVE ESSERE MINORE/UGUALE
070500000000     C*  DEL MESE FINALE RICHIESTO A VIDEO
070600000000     C*  ANNO INIZIALE <> ANNO FINALE: IL MESE DELL'ANNO INIZIALE DEVE ESSERE
070700000000     C*  MIN/UG DI 12; IL MESE DELL'ANNO FINALE MIN/UG DEL MESE FINALE RICHIESTO
070800000000     C*
070900000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
071000000000     C*
071100000000     C* CONTROLLO MESE DEL RECORD SALDI LETTO°
071200000000IF  1C     TIPANN        IFEQ      'U'                                          *ANNI UGUALI
071300000000     C     J             OREQ      2                                            *ANNI DIVERSI MA 2°
071400000000     C*
071500000000IF  2C     SDRMES        IFGT      MESFIN
071600000000     C                   MOVEL     '1'           $FIN1                          *ESCO DAL CICLO CORR.
071700000000     C                   MOVEL     '1'           $FIN2                          *ESCO DAL CICLO PREC.
071800000000E   2C                   ENDIF
071900000000E   1C                   ENDIF
072000000000     C*
072100000000     C* SE TIPO SERVIZIO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
072200000000IF  1C     PARSER        IFNE      *BLANKS
072300000000IF  2C     PARSER        IFNE      SDRTSP
072400000000     C                   MOVEL     'N'           WRECOK
072500000000E   2C                   ENDIF
072600000000E   1C                   ENDIF
072700000000     C*
072800000000     C* SE TIPO PORTO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
072900000000IF  1C     PARPOR        IFNE      *BLANKS
073000000000IF  2C     SDRTPO        IFNE      PARPOR
073100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
073200000000E   2C                   ENDIF
073300000000E   1C                   ENDIF
073400000000     C*
073500000000     C* ESCLUDO LE BOLLE PER CUI NON VALGONO LE SPEDIZIONI
073600000000     C                   Z-ADD     1             G
073700000000     C                   SETOFF                                           99
073800000000     C     SDRRBL        LOOKUP    FLAB(G)                                99
073900000000IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
074000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
074100000000X   1C                   ELSE
074200000000IF  2C     FSP(G)        IFEQ      'N'                                          BOLLA CON SPED.VALIDE
074300000000     C                   MOVEL     'N'           WRECOK
074400000000E   2C                   ENDIF
074500000000E   1C                   ENDIF
074600000000     C*
074700000000     C* SE TIPO SPEDIZIONE DIVERSO DA QUELLO RICHIESTO, ESCLUDO
074800000000IF  1C*          PARSPE    IFNE *BLANKS
074900000000IF  2C*          SDRRBL    IFNE PARSPE
075000000000     C*                    MOVEL'N'       WRECOK           *RECORD NON VALIDO
075100000000E   2C*                    ENDIF
075200000000E   1C*                    ENDIF
075300000000     C*
075400000000     C* SE TIPO PRODOTTO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
075500000000IF  1C     PARPRO        IFNE      *BLANKS
075600000000IF  2C     SDRTPR        IFNE      PARPRO
075700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
075800000000E   2C                   ENDIF
075900000000E   1C                   ENDIF
076000971204     C*
076100971204     C* REPERISCE IL FLAG ITALIA / ESTERO
076200971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
076300971204     C                   MOVEL     *BLANKS       DLPFL1
076400971204     C                   MOVEL     *BLANKS       DLAFL1
076500971204     C                   Z-ADD     1             I
076600971204     C     SDRFLP        LOOKUP    FIL(I)                                 99
076700971204IF  2C     *IN99         IFEQ      *ON                                          *TROVATO
076800971204     C                   MOVEL     FL1(I)        DLPFL1
076900971204E   2C                   ENDIF
077000971204     C                   Z-ADD     1             I
077100971204     C     SDRFLA        LOOKUP    FIL(I)                                 99
077200971204IF  2C     *IN99         IFEQ      *ON                                          *TROVATO
077300971204     C                   MOVEL     FL1(I)        DLAFL1
077400971204E   2C                   ENDIF
077500971204E   1C                   ENDIF
077600000218     C*
077700000614     C* SE RICHIESTA STATISTICA ITALIA, ESCLUDE BOLLE ESTERO (ITA->ITA)
077800971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
077900971204IF  2C     PARIOE        IFEQ      'I'
078000971204IF  3C     DLPFL1        IFNE      *BLANKS
078100971204     C     DLAFL1        ORNE      *BLANKS
078200950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
078300971204E   3C                   ENDIF
078400971204E   2C                   ENDIF
078500971204E   1C                   ENDIF
078600000614     C*
078700950529     C* SE RICHIESTA STATISTICA IMPORT, ESCLUDE BOLLE CON LNP ITALIA (EST->ITA)
078800971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
078900971204IF  2C     PARIOE        IFEQ      'M'
079000000218IF  3C     DLPFL1        IFNE      *BLANKS
079100000218     C     DLAFL1        ANDEQ     *BLANKS
079200000218     C                   ELSE
079300950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
079400971204E   3C                   ENDIF
079500971204E   2C                   ENDIF
079600971204E   1C                   ENDIF
079700950529     C*
079800950529     C* SE RICHIESTA STATISTICA EXPORT, ESCLUDE BOLLE CON LNA ITALIA (ITA->EST)
079900971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
080000971204IF  2C     PARIOE        IFEQ      'X'
080100000228IF  3C     DLAFL1        IFEQ      *BLANKS
080200950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
080300971204E   3C                   ENDIF
080400971204E   2C                   ENDIF
080500971204E   1C                   ENDIF
080600950529     C*
080700000000     C                   ENDSR
080800000000     C*------------------------------------------------------------------------*
080900000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
081000000000     C*------------------------------------------------------------------------*
081100000000     C     NEWFIL        BEGSR
081200000000     C*
081300000000     C* REIMPOSTO FINE FILE SALDI CORRENTI/PRECEDENTI/BUDGET
081400000000     C                   MOVEL     '0'           $FIN1                          FINE SALDI CORRENTI
081500000000     C                   MOVEL     '0'           $FIN2                          FINE SALDI PRECEDENTI
081600000000     C                   MOVEL     '0'           $FIN3                          FINE VALORI BUDGET
081700000000     C*
081800000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI E BUDGET
081900000000     C                   MOVEL     '0'           PASFIL                         *SALDI
082000000000     C                   MOVEL     '0'           NOBDG                          *BUDGET
082100000000     C*
082200000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
082300000000     C                   Z-ADD     ORGFIL        DEPFIL
082400000000     C*
082500000000     C* DECODIFICA IL CODICE FILIALE
082600000703     C                   MOVEL     *BLANKS       LAVNTX
082700000000     C                   EXSR      DECFIL
082800000703     C*
082900000703     C* MEMORIZZA IL NETWORK DELLA NUOVA FILIALE
083000000703     C                   MOVEL     LAVNTX        DEPNTF                         *NETWORK
083100000000     C*
083200000000     C* AZZERA I TOTALI DELLA FILIALE
083300000000DO  1C                   DO        10            A
083400000000     C     A             OCCUR     FPFIL
083500000000     C                   CLEAR                   SPF
083600000000     C                   CLEAR                   SBF
083700000000     C                   CLEAR                   SAF
083800000000     C                   CLEAR                   QTF
083900000000     C                   CLEAR                   QBF
084000000000     C                   CLEAR                   QAF
084100000000     C                   CLEAR                   CLF
084200000000     C                   CLEAR                   CAF
084300000000     C                   CLEAR                   VLF
084400000000     C                   CLEAR                   VAF
084500000000     C                   CLEAR                   CBF
084600000000     C                   CLEAR                   VBF
084700000000E   1C                   ENDDO
084800000000     C*
084900000000     C                   ENDSR
085000000000     C*------------------------------------------------------------------------*
085100000000     C*?NEWDET - OPERAZIONI PER NUOVA FILIALE DETTAGLIO
085200000000     C*------------------------------------------------------------------------*
085300000000     C     NEWDET        BEGSR
085400000000     C*
085500000000     C* REIMPOSTO FINE FILE SALDI CORRENTI/PRECEDENTI
085600000000     C                   MOVEL     '0'           $FIN1                          FINE SALDI CORRENTI
085700000000     C                   MOVEL     '0'           $FIN2                          FINE SALDI PRECEDENTI
085800000000     C*
085900000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI FILIALE DETTAGLIO
086000000000     C                   MOVEL     '0'           PASDET
086100000703     C*
086200000703     C* MEMORIZZA IL NETWORK DELLA NUOVA FILIALE DETTAGLIO
086300000703     C                   MOVEL     NTX(F)        DEPNTT                         *NETWORK
086400000000     C*
086500000000     C* AZZERA I TOTALI DELLA FILIALE
086600000000DO  1C                   DO        10            A
086700000000     C     A             OCCUR     FPDET
086800000000     C                   CLEAR                   SPT
086900000000     C                   CLEAR                   SAT
087000000000     C                   CLEAR                   QTT
087100000000     C                   CLEAR                   QAT
087200000000     C                   CLEAR                   CLT
087300000000     C                   CLEAR                   CAT
087400000000     C                   CLEAR                   VLT
087500000000     C                   CLEAR                   VAT
087600000000E   1C                   ENDDO
087700000000     C*
087800000000     C                   ENDSR
087900000000     C*------------------------------------------------------------------------*
088000000000     C*?MEMSAL - MEMORIZZA I SALDI CORRENTI
088100000000     C*------------------------------------------------------------------------*
088200000000     C     MEMSAL        BEGSR
088300000000     C*
088400000000     C                   MOVEL     SDRFPE        FASCIA            2 0
088500000000     C                   Z-ADD     1             MM                2 0
088600000000     C*
088700000000     C* RICERCA DELLA POSIZIONE IN SCHIERA DELL'ANNO/MESE CHE STO ELABORANDO
088800000000     C                   SETOFF                                           28
088900000000     C     SDRMES        LOOKUP    MES(MM)                                28
089000000000     C*
089100000000     C* SE MESE TROVATO, ADDIZIONO
089200000000IF  1C     *IN28         IFEQ      *ON
089300000000     C*
089400000000     C* FASCIA DI PESO PER FILIALE DETTAGLIO (SOLO SE RICHIESTO)
089500000000     C     FASCIA        OCCUR     FPDET
089600950529     C                   MOVEL     '1'           PASDET                         *PASSATO MEMORIZZAZ.
089700000000     C                   ADD       SDRNSP        SPT(MM)                        *SPEDIZIONI
089800000000     C                   ADD       SDRPKG        QTT(MM)                        *KILOGRAMMI
089900000000     C                   ADD       SDRNCO        CLT(MM)                        *COLLI
090000000000     C                   ADD       SDRVOL        VLT(MM)                        *VOLUME IN MC
090100000000     C*
090200000000     C* FASCIA DI PESO PER FILIALE
090300000000     C     FASCIA        OCCUR     FPFIL
090400950529     C                   MOVEL     '1'           PASFIL                         *PASSATO MEMORIZZAZ.
090500950529     C                   MOVEL     '1'           PASARE
090600950529     C                   MOVEL     '1'           PASDIV
090700950529     C                   MOVEL     '1'           PASGEN
090800000000     C                   ADD       SDRNSP        SPF(MM)                        *SPEDIZIONI
090900000000     C                   ADD       SDRPKG        QTF(MM)                        *KILOGRAMMI
091000000000     C                   ADD       SDRNCO        CLF(MM)                        *COLLI
091100000000     C                   ADD       SDRVOL        VLF(MM)                        *VOLUME IN MC
091200000000     C*
091300000000     C* FASCIA DI PESO PER AREA
091400000000     C     FASCIA        OCCUR     FPARE
091500000000     C                   ADD       SDRNSP        SPA(MM)
091600000000     C                   ADD       SDRPKG        QTA(MM)
091700000000     C                   ADD       SDRNCO        CLA(MM)
091800000000     C                   ADD       SDRVOL        VLA(MM)
091900000000     C*
092000000000     C* FASCIA DI PESO PER DIVISIONE
092100000000     C     FASCIA        OCCUR     FPDIV
092200000000     C                   ADD       SDRNSP        SPD(MM)
092300000000     C                   ADD       SDRPKG        QTD(MM)
092400000000     C                   ADD       SDRNCO        CLD(MM)
092500000000     C                   ADD       SDRVOL        VLD(MM)
092600000000     C*
092700000000     C* FASCIA DI PESO PER TOTALI GENERALI
092800000000     C     FASCIA        OCCUR     FPGEN
092900000000     C                   ADD       SDRNSP        SPG(MM)
093000000000     C                   ADD       SDRPKG        QTG(MM)
093100000000     C                   ADD       SDRNCO        CLG(MM)
093200000000     C                   ADD       SDRVOL        VLG(MM)
093300100413     c
093400100413     c* Memorizzo nel file sudddiviso per tipo servizio
093500120820     c                   if        PARFPC='S'
093600100413     c                   exsr      Mempc10
093700120820     c                   endif
093800120820     c
093900000000E   1C                   ENDIF
094000000000     C*
094100000000     C                   ENDSR
094200100413     C*------------------------------------------------------------------------*
094300100413     C*?Memorizzo nel SIPCP10 o SIPCPA10 suddiviso per tipo servizio
094400100413     C*------------------------------------------------------------------------*
094500100413     c     Mempc10       BEGSR
094600100413     C*---
094700100413     C* P A R T E N Z A
094800100413     C*---
094900100413IF  3C     PARMPA        IFEQ      'P'
095000100413     C                   Z-ADD     DEPFIL        KCPFLP                         *FILIALE PARTENZA
095100100413     C                   MOVEL     PARIOE        KCPIOE                         *DATI IN STATISTICA
095200100413     C                   MOVEL     fascia        KCPFPE                         *FASCIA DI PESO
095300100413     C                   MOVEL     sdrtsp        KCPtsp                         *FASCIA DI PESO
095400100413     C                   MOVEL     DEPNTF        KCPNTP                         *NETWORK PARTENZA
095500100413     C                   MOVEL     DEPNTT        KCPNTA                         *NETWORK ARRIVO
095600100413     C     KEYPCP1       CHAIN     SIPCP11L                           98
095700100413IF  4C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
095800100413     C                   CLEAR                   SIPCP010
095900100413     C                   Z-ADD     Ksdann        n_PCPaaa
096000100413     C                   Z-ADD     KCPFLP        n_PCPFLP
096100100413     C                   MOVEL     KCPIOE        n_PCPIOE
096200100413     C                   MOVEL     KCPtsp        n_PCPtsp
096300100413     C                   MOVEL     KCPFPE        n_PCPFPE
096400100413     C                   MOVEL     KCPNTP        n_PCPNTP
096500100413     C                   MOVEL     KCPNTA        n_PCPNTA
096600100413     c                   endif
096700100413     c* Addiziono campi
096800100413     c                   select
096900100413     c
097000100413     c                   when      sdrmes=1
097100100413     C                   ADD       SDRNsp        N_pcpns1
097200100413     C     sdrpkg        DIV       100           N11
097300100413     C                   ADD(H)    N11           n_PCPPQ1                         *QUINTALI 1° MESE
097400100413     C                   ADD       sdrnco        N_PCPNC1                         *COLLI 1° MESE
097500100413     C                   ADD(H)    sdrvol        N_PCPVO1                         *VOLUME 1° MESE
097600100413     c
097700100413     c                   when      sdrmes=2
097800100413     C                   ADD       SDRNsp        n_pcpns2
097900100413     C     sdrpkg        DIV       100           N11
098000100413     C                   ADD(H)    N11           n_PCPPQ2
098100100413     C                   ADD       sdrnco        n_PCPNC2
098200100413     C                   ADD(H)    sdrvol        n_PCPVO2
098300100413     c
098400100413     c                   when      sdrmes=3
098500100413     C                   ADD       SDRNsp        n_pcpns3
098600100413     C     sdrpkg        DIV       100           N11
098700100413     C                   ADD(H)    N11           n_PCPPQ3
098800100413     C                   ADD       sdrnco        n_PCPNC3
098900100413     C                   ADD(H)    sdrvol        n_PCPVO3
099000100413     c
099100100413     c                   when      sdrmes=4
099200100413     C                   ADD       SDRNsp        n_pcpns4
099300100413     C     sdrpkg        DIV       100           N11
099400100413     C                   ADD(H)    N11           n_PCPPQ4
099500100413     C                   ADD       sdrnco        n_PCPNC4
099600100413     C                   ADD(H)    sdrvol        n_PCPVO4
099700100413     c
099800100413     c                   when      sdrmes=5
099900100413     C                   ADD       SDRNsp        n_pcpns5
100000100413     C     sdrpkg        DIV       100           N11
100100100413     C                   ADD(H)    N11           n_PCPPQ5
100200100413     C                   ADD       sdrnco        n_PCPNC5
100300100413     C                   ADD(H)    sdrvol        n_PCPVO5
100400100413     c
100500100413     c                   when      sdrmes=6
100600100413     C                   ADD       SDRNsp        n_pcpns6
100700100413     C     sdrpkg        DIV       100           N11
100800100413     C                   ADD(H)    N11           n_PCPPQ6
100900100413     C                   ADD       sdrnco        n_PCPNC6
101000100413     C                   ADD(H)    sdrvol        n_PCPVO6
101100100413     c
101200100413     c                   when      sdrmes=7
101300100413     C                   ADD       SDRNsp        n_pcpns7
101400100413     C     sdrpkg        DIV       100           N11
101500100413     C                   ADD(H)    N11           n_PCPPQ7
101600100413     C                   ADD       sdrnco        n_PCPNC7
101700100413     C                   ADD(H)    sdrvol        n_PCPVO7
101800100413     c
101900100413     c                   when      sdrmes=8
102000100413     C                   ADD       SDRNsp        n_pcpns8
102100100413     C     sdrpkg        DIV       100           N11
102200100413     C                   ADD(H)    N11           n_PCPPQ8
102300100413     C                   ADD       sdrnco        n_PCPNC8
102400100413     C                   ADD(H)    sdrvol        n_PCPVO8
102500100413     c
102600100413     c                   when      sdrmes=9
102700100413     C                   ADD       SDRNsp        n_pcpns9
102800100413     C     sdrpkg        DIV       100           N11
102900100413     C                   ADD(H)    N11           n_PCPPQ9
103000100413     C                   ADD       sdrnco        n_PCPNC9
103100100413     C                   ADD(H)    sdrvol        n_PCPVO9
103200100413     c
103300100413     c                   when      sdrmes=10
103400100413     C                   ADD       SDRNsp        n_pcpns0
103500100413     C     sdrpkg        DIV       100           N11
103600100413     C                   ADD(H)    N11           n_PCPPQ0
103700100413     C                   ADD       sdrnco        n_PCPNC0
103800100413     C                   ADD(H)    sdrvol        n_PCPVO0
103900100413     c
104000100413     c                   when      sdrmes=11
104100100413     C                   ADD       SDRNsp        n_pcpnsa
104200100413     C     sdrpkg        DIV       100           N11
104300100413     C                   ADD(H)    N11           n_PCPPQa
104400100413     C                   ADD       sdrnco        n_PCPNCa
104500100413     C                   ADD(H)    sdrvol        n_PCPVOa
104600100413     c
104700100413     c                   when      sdrmes=12
104800100413     C                   ADD       SDRNsp        n_pcpnsb
104900100413     C     sdrpkg        DIV       100           N11
105000100413     C                   ADD(H)    N11           n_PCPPQb
105100100413     C                   ADD       sdrnco        n_PCPNCb
105200100413     C                   ADD(H)    sdrvol        n_PCPVOb
105300100413     c                   ENDSL
105400100413     c
105500100413     c                   if        *in98
105600100413     C                   WRITE     SIPCP010
105700100413X   4C                   ELSE
105800100413     C                   UPDATE    SIPCP010
105900100413E   4C                   ENDIF
106000100413     c
106100100413     c                   else
106200100413     C*---
106300100413     C* A R R I V O
106400100413     C*---
106500100413     C                   Z-ADD     DEPFIL        KCAFLA                         *FILIALE ARRIVO
106600100413     C                   MOVEL     PARIOE        KCAIOE                         *DATI IN STATISTICA
106700100415     C                   MOVEL     sdrtsp        KCAtsp
106800100413     C                   MOVEL     fascia        KCAFPE                         *FASCIA DI PESO
106900100413     C                   MOVEL     DEPNTF        KCANTA                         *NETWORK ARRIVO
107000100413     C                   MOVEL     DEPNTT        KCANTP                         *NETWORK PARTENZA
107100100413     C     KEYPCA1       CHAIN     SIPCA11L                           98
107200100413IF  4C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
107300100413     C                   CLEAR                   SIPCA010                       *AZZERA FORMATO RECORD
107400100415     C                   Z-ADD     Ksdann        n_PCaaaa
107500100413     C                   Z-ADD     KCAFLA        n_PCAFLA                         *CHIAVE
107600100413     C                   MOVEL     KCAIOE        n_PCAIOE
107700100413     C                   MOVEL     KCAFPE        n_PCAFPE
107800100413     C                   MOVEL     KCAtsp        n_PCAtsp
107900100413     C                   MOVEL     KCANTA        n_PCANTA
108000100413     C                   MOVEL     KCANTP        n_PCANTP
108100100413    4c                   endif
108200100413     c* Addiziono campi
108300100413     c                   select
108400100413     c
108500100413     c                   when      sdrmes=1
108600100413     C                   ADD       SDRNsp        N_pcans1
108700100413     C     sdrpkg        DIV       100           N11
108800100413     C                   ADD(H)    N11           n_PCaPQ1                         *QUINTALI 1° MESE
108900100413     C                   ADD       sdrnco        N_PCaNC1                         *COLLI 1° MESE
109000100413     C                   ADD(H)    sdrvol        N_PCaVO1                         *VOLUME 1° MESE
109100100413     c
109200100413     c                   when      sdrmes=2
109300100413     C                   ADD       SDRNsp        n_pcans2
109400100413     C     sdrpkg        DIV       100           N11
109500100413     C                   ADD(H)    N11           n_PCaPQ2
109600100413     C                   ADD       sdrnco        n_PCaNC2
109700100413     C                   ADD(H)    sdrvol        n_PCaVO2
109800100413     c
109900100413     c                   when      sdrmes=3
110000100413     C                   ADD       SDRNsp        n_pcans3
110100100413     C     sdrpkg        DIV       100           N11
110200100413     C                   ADD(H)    N11           n_PCaPQ3
110300100413     C                   ADD       sdrnco        n_PCaNC3
110400100413     C                   ADD(H)    sdrvol        n_PCaVO3
110500100413     c
110600100413     c                   when      sdrmes=4
110700100413     C                   ADD       SDRNsp        n_pcans4
110800100413     C     sdrpkg        DIV       100           N11
110900100413     C                   ADD(H)    N11           n_PCaPQ4
111000100413     C                   ADD       sdrnco        n_PCaNC4
111100100413     C                   ADD(H)    sdrvol        n_PCaVO4
111200100413     c
111300100413     c                   when      sdrmes=5
111400100413     C                   ADD       SDRNsp        n_pcans5
111500100413     C     sdrpkg        DIV       100           N11
111600100413     C                   ADD(H)    N11           n_PCaPQ5
111700100413     C                   ADD       sdrnco        n_PCaNC5
111800100413     C                   ADD(H)    sdrvol        n_PCaVO5
111900100413     c
112000100413     c                   when      sdrmes=6
112100100413     C                   ADD       SDRNsp        n_pcans6
112200100413     C     sdrpkg        DIV       100           N11
112300100413     C                   ADD(H)    N11           n_PCaPQ6
112400100413     C                   ADD       sdrnco        n_PCaNC6
112500100413     C                   ADD(H)    sdrvol        n_PCaVO6
112600100413     c
112700100413     c                   when      sdrmes=7
112800100413     C                   ADD       SDRNsp        n_pcans7
112900100413     C     sdrpkg        DIV       100           N11
113000100413     C                   ADD(H)    N11           n_PCaPQ7
113100100413     C                   ADD       sdrnco        n_PCaNC7
113200100413     C                   ADD(H)    sdrvol        n_PCaVO7
113300100413     c
113400100413     c                   when      sdrmes=8
113500100413     C                   ADD       SDRNsp        n_pcans8
113600100413     C     sdrpkg        DIV       100           N11
113700100413     C                   ADD(H)    N11           n_PCaPQ8
113800100413     C                   ADD       sdrnco        n_PCaNC8
113900100413     C                   ADD(H)    sdrvol        n_PCaVO8
114000100413     c
114100100413     c                   when      sdrmes=9
114200100413     C                   ADD       SDRNsp        n_pcans9
114300100413     C     sdrpkg        DIV       100           N11
114400100413     C                   ADD(H)    N11           n_PCaPQ9
114500100413     C                   ADD       sdrnco        n_PCaNC9
114600100413     C                   ADD(H)    sdrvol        n_PCaVO9
114700100413     c
114800100413     c                   when      sdrmes=10
114900100413     C                   ADD       SDRNsp        n_pcans0
115000100413     C     sdrpkg        DIV       100           N11
115100100413     C                   ADD(H)    N11           n_PCaPQ0
115200100413     C                   ADD       sdrnco        n_PCaNC0
115300100413     C                   ADD(H)    sdrvol        n_PCaVO0
115400100413     c
115500100413     c                   when      sdrmes=11
115600100413     C                   ADD       SDRNsp        n_pcansa
115700100413     C     sdrpkg        DIV       100           N11
115800100413     C                   ADD(H)    N11           n_PCaPQa
115900100413     C                   ADD       sdrnco        n_PCaNCa
116000100413     C                   ADD(H)    sdrvol        n_PCaVOa
116100100413     c
116200100413     c                   when      sdrmes=12
116300100413     C                   ADD       SDRNsp        n_pcansb
116400100413     C     sdrpkg        DIV       100           N11
116500100413     C                   ADD(H)    N11           n_PCaPQb
116600100413     C                   ADD       sdrnco        n_PCaNCb
116700100413     C                   ADD(H)    sdrvol        n_PCaVOb
116800100413     c                   ENDSL
116900100413     c
117000100413     c                   if        *in98
117100100413     C                   WRITE     SIPCA010
117200100413X   4C                   ELSE
117300100413     C                   UPDATE    SIPCA010
117400100413E   4C                   ENDIF
117500100413E   3C                   ENDIF
117600100413     c
117700100413     C                   ENDSR
117800000000     C*------------------------------------------------------------------------*
117900000000     C*?CALANP - CALCOLA I SALDI DELL' ANNO PRECEDENTE
118000000000     C*------------------------------------------------------------------------*
118100000000     C     CALANP        BEGSR
118200000000     C*
118300000000     C* LETTURA SALDI X AA INIZIALE, E X AA FINALE SOLO SE DIVERSO DALL'INIZIALE
118400000000     C                   Z-ADD     1             J
118500000000DO  1C                   DO        2             J
118600000000     C                   MOVEL     '0'           $FIN2                          *REIMPOSTO FINE FILE
118700000000     C*
118800000000     C* SECONDO GIRO SOLO SE ANNO FINALE DIVERSO DALL' INIZIALE (PIENO)
118900000000IF  2C     PIF(J)        IFNE      *ZEROS
119000000000     C*
119100000000     C* POSIZIONAMENTO SUI SALDI DELL' ANNO PRECEDENTE E PRIMA LETTURA
119200000000     C                   EXSR      SETFI2
119300000000     C*
119400000000     C* FINCHE FINE SALDI DELLA FILIALE
119500000000DO  3C     $FIN2         DOWEQ     '0'
119600000000     C*
119700000000     C* MEMORIZZA SALDI ANNO PRECEDENTE
119800000000     C                   EXSR      MEMANP
119900000000     C*
120000000000     C* LETTURA PROSSIMO RECORD SALDI ANNO PRECEDENTE
120100000000     C                   EXSR      LETFI2
120200000000E   3C                   ENDDO
120300000000E   2C                   ENDIF
120400000000E   1C                   ENDDO
120500000000     C*
120600000000     C                   ENDSR
120700000000     C*------------------------------------------------------------------------*
120800000000     C*?SETFI2 - POSIZIONAMENTO E PRIMA LETTURA SALDI ANNO PRECEDENTE
120900000000     C*------------------------------------------------------------------------*
121000000000     C     SETFI2        BEGSR
121100000000     C*
121200000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI ANNO PRECEDENTE
121300000000     C                   Z-ADD     PIF(J)        KSDANN                         *ANNO
121400000000     C                   Z-ADD     ORGFIL        KSDFIL                         *FILIALE
121500000000     C                   Z-ADD     FIL(F)        KSDFIT                         *FILIALE DETTAGLIO
121600000000     C*
121700000000     C* MI POSIZIONO CON IL MESE INIZIALE SE ANNI UGUALI O PRIMO ANNO DIVERSO,
121800000000     C* OPPURE CON IL MESE = 01 SE GLI ANNI SONO DIVERSI E STO ELABORANDO IL 2°
121900000000IF  1C     TIPANN        IFEQ      'U'
122000000000     C     J             OREQ      1
122100000000     C                   Z-ADD     MESINI        KSDMES                         *MESE INIZIALE
122200000000E   1C                   ELSE
122300000000     C                   Z-ADD     01            KSDMES                         *MESE 01
122400000000E   1C                   ENDIF
122500000000     C*
122600000000     C* POSIZIONAMENTO E PRIMA LETTURA (ANNO INIZIALE - 1)
122700000000IF  1C     PARMPA        IFEQ      'P'
122800000000     C* PARTENZE
122900990609     C     KEYSDR        SETLL     SISDR01L                           99
123000000000X   1C                   ELSE
123100000000     C* ARRIVI
123200990609     C     KEYSDR        SETLL     SISDR02L                           99
123300000000E   1C                   ENDIF
123400000000     C*
123500000000IF  1C     *IN99         IFEQ      '1'
123600000000     C                   MOVEL     '1'           $FIN2
123700000000X   1C                   ELSE
123800000000     C                   EXSR      LETFI2                                       *LETTURA RECORD
123900000000E   1C                   ENDIF
124000000000     C*
124100000000     C                   ENDSR
124200000000     C*------------------------------------------------------------------------*
124300000000     C*?LETFI2 - LETTURA PROSSIMO RECORD SALDI ANNO PRECEDENTE
124400000000     C*------------------------------------------------------------------------*
124500000000     C     LETFI2        BEGSR
124600000000     C*
124700000000     C* LEGGO FINO A:
124800000000     C*     - FINE FILE
124900000000     C*     - TROVATO RECORD VALIDO
125000000000     C                   MOVEL     'N'           WRECOK
125100000000DO  1C     $FIN2         DOWEQ     '0'
125200000000     C     WRECOK        ANDEQ     'N'
125300000000     C*
125400000000     C* LETTURE SUCCESSIVE ALLA PRIMA (ANNO + FILIALE + DETTAGLIO)
125500000000IF  2C     PARMPA        IFEQ      'P'
125600990609     C     KEYSD2        READE     SISDR01L                               99
125700000000X   2C                   ELSE
125800990609     C     KEYSD2        READE     SISDR02L                               99
125900000000E   2C                   ENDIF
126000000000     C*
126100000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
126200000000     C                   MOVEL     '1'           $FIN2
126300000000X   2C                   ELSE
126400000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
126500000000E   2C                   ENDIF
126600000000     C*
126700000000E   1C                   ENDDO
126800000000     C*
126900000000     C                   ENDSR
127000000000     C*------------------------------------------------------------------------*
127100000000     C*?MEMANP - MEMORIZZA I SALDI DELL' ANNO PRECEDENTE
127200000000     C*------------------------------------------------------------------------*
127300000000     C     MEMANP        BEGSR
127400000000     C*
127500000000     C                   MOVEL     SDRFPE        FASCIA
127600000000     C                   Z-ADD     1             MM
127700000000     C*
127800000000     C* RICERCA DELLA POSIZIONE IN SHIERA DELL'ANNO/MESE CHE STO ELABORANDO
127900000000     C                   SETOFF                                           28
128000000000     C     SDRMES        LOOKUP    MES(MM)                                28
128100000000     C*
128200000000     C* SE MESE TROVATO, ADDIZIONO
128300000000IF  1C     *IN28         IFEQ      *ON
128400000000     C*
128500000000     C* FASCIA DI PESO PER FILIALE DETTAGLIO (SOLO SE RICHIESTO)
128600000000     C     FASCIA        OCCUR     FPDET
128700000000     C                   ADD       SDRNSP        SAT(MM)                        *SPEDIZIONI
128800000000     C                   ADD       SDRPKG        QAT(MM)                        *KILOGRAMMI
128900000000     C                   ADD       SDRNCO        CAT(MM)                        *COLLI
129000000000     C                   ADD       SDRVOL        VAT(MM)                        *VOLUME IN MC
129100000000     C*
129200000000     C* FASCIA DI PESO PER FILIALE
129300000000     C     FASCIA        OCCUR     FPFIL
129400000000     C                   ADD       SDRNSP        SAF(MM)                        *SPEDIZIONI
129500000000     C                   ADD       SDRPKG        QAF(MM)                        *KILOGRAMMI
129600000000     C                   ADD       SDRNCO        CAF(MM)                        *COLLI
129700000000     C                   ADD       SDRVOL        VAF(MM)                        *VOLUME IN MC
129800000000     C*
129900000000     C* FASCIA DI PESO PER AREA
130000000000     C     FASCIA        OCCUR     FPARE
130100000000     C                   ADD       SDRNSP        SAA(MM)
130200000000     C                   ADD       SDRPKG        QAA(MM)
130300000000     C                   ADD       SDRNCO        CAA(MM)
130400000000     C                   ADD       SDRVOL        VAA(MM)
130500000000     C*
130600000000     C* FASCIA DI PESO PER DIVISIONE
130700000000     C     FASCIA        OCCUR     FPDIV
130800000000     C                   ADD       SDRNSP        SAD(MM)
130900000000     C                   ADD       SDRPKG        QAD(MM)
131000000000     C                   ADD       SDRNCO        CAD(MM)
131100000000     C                   ADD       SDRVOL        VAD(MM)
131200000000     C*
131300000000     C* FASCIA DI PESO PER TOTALI GENERALI
131400000000     C     FASCIA        OCCUR     FPGEN
131500000000     C                   ADD       SDRNSP        SAG(MM)
131600000000     C                   ADD       SDRPKG        QAG(MM)
131700000000     C                   ADD       SDRNCO        CAG(MM)
131800000000     C                   ADD       SDRVOL        VAG(MM)
131900000000E   1C                   ENDIF
132000000000     C*
132100000000     C                   ENDSR
132200000000     C*------------------------------------------------------------------------*
132300000000     C*?CALBDG - CALCOLA I VALORI DEL BUDGET
132400000000     C*------------------------------------------------------------------------*
132500000000     C     CALBDG        BEGSR
132600000000     C*
132700000000     C* POSIZIONAMENTO SUL FILE BUDGET E PRIMA LETTURA
132800000000     C                   EXSR      SETFI3
132900000000     C*
133000000000     C* FINCHE' FINE FILE BUDGET O ROTTURA FILIALE
133100000000DO  1C     $FIN3         DOWEQ     '0'
133200000000     C*
133300000000     C* MEMORIZZA VALORI BUDGET
133400000000     C                   EXSR      MEMBDG
133500000000     C*
133600000000     C* LETTURA PROSSIMO RECORD BUDGET
133700000000     C                   EXSR      LETFI3
133800000000E   1C                   ENDDO
133900000000     C*
134000000000     C                   ENDSR
134100000000     C*------------------------------------------------------------------------*
134200000000     C*?SETF13 - POSIZIONAMENTO E PRIMA LETTURA FILE BUDGET
134300000000     C*------------------------------------------------------------------------*
134400000000     C     SETFI3        BEGSR
134500000000     C*
134600000000     C* SE SELEZ.TI TIPO SERVIZIO/PORTO/PRODOTTO NON LEGGO I VALORI DEL BUDGET
134700000000     C* PERCHE' QUESTI SONO PREVISTI SOLO PER FILIALE (NON NEL DETTAGLIO)
134800000000IF  1C     PARSER        IFNE      *BLANKS
134900000000     C     PARPOR        ORNE      *BLANKS
135000000000     C*          PARSPE    ORNE *BLANKS                    *MOMENTANEAM. ESCLUSO
135100000000     C     PARPRO        ORNE      *BLANKS
135200000000     C                   MOVEL     '1'           $FIN3                          *USCITA CICLO
135300000000     C                   MOVEL     '1'           NOBDG                          *NO LETTURA BUDGET
135400000000X   1C                   ELSE
135500000000     C                   MOVEL     '0'           NOBDG                          *SI LETTURA BUDGET
135600000000E   1C                   ENDIF
135700000000     C*
135800000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE BUDGET (FILIALE+ANNO+MESE)
135900000000     C* SOLO SE RICHIESTO TUTTI I SERVIZI/SPEDIZIONI/PRODOTTI
136000000000IF  1C     $FIN3         IFEQ      '0'
136100000000     C                   Z-ADD     DEPFIL        KBDFIL
136200000000     C                   Z-ADD     ANNINI        KBDANN
136300000000     C                   Z-ADD     MESINI        KBDMES
136400960115     C                   MOVEL     PARIOE        KBDIOE
136500960108     C*
136600020114     C     KEYBDG        SETLL     TEBDG01L                           99        *FINE ARCHIVIO
136700000000     C*
136800000000IF  2C     *IN99         IFEQ      '1'
136900000000     C                   MOVEL     '1'           $FIN3
137000000000X   2C                   ELSE
137100000000     C                   EXSR      LETFI3                                       *LETTURA RECORD
137200000000E   2C                   ENDIF
137300000000E   1C                   ENDIF
137400000000     C*
137500000000     C                   ENDSR
137600000000     C*------------------------------------------------------------------------*
137700000000     C*?LETF13 - LETTURA PROSSIMO RECORD BUDGET
137800000000     C*------------------------------------------------------------------------*
137900000000     C     LETFI3        BEGSR
138000000000     C*
138100000000     C* LEGGO FINO A:
138200000000     C*     - FINE FILE O ROTTURA FILIALE
138300000000     C*     - TROVATO RECORD VALIDO
138400000000     C                   MOVEL     'N'           WRECOK
138500000000DO  1C     $FIN3         DOWEQ     '0'
138600000000     C     WRECOK        ANDEQ     'N'
138700000000     C*
138800020114     C     KBDFIL        READE     TEBDG01L                               99
138900000000     C*
139000000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
139100000000     C                   MOVEL     '1'           $FIN3
139200000000X   2C                   ELSE
139300000000     C                   EXSR      CHKRE3                                       *CONTROLLO RECORD
139400000000E   2C                   ENDIF
139500000000     C*
139600000000E   1C                   ENDDO
139700000000     C*
139800000000     C                   ENDSR
139900000000     C*------------------------------------------------------------------------*
140000000000     C*?CHKRE3 - CONTROLLO RECORD BUDGET
140100000000     C*------------------------------------------------------------------------*
140200000000     C     CHKRE3        BEGSR
140300000000     C*
140400000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
140500000000     C*
140600000000     C* LA DATA LETTA DEVE ESSERE MAX/UGU ALLA DATA INIZ E MIN/UGU ALLA DATA FIN
140700990527IF  1C     TBGDAT        IFGE      PARDAT
140800990527     C     TBGDAT        ANDLE     PARDA2
140900000000X   1C                   ELSE
141000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
141100000000     C*
141200000000     C* SE POI DATA BUDGET MAGGIORE A DATA FINALE RICHIESTA, ESCO DAL CICLO
141300990527IF  2C     TBGDAT        IFGT      PARDA2
141400000000     C                   MOVEL     '1'           $FIN3
141500000000E   2C                   ENDIF
141600000000E   1C                   ENDIF
141700000000     C*
141800000000     C* VALORE DI BUDGET ANNULLATO, ESCLUDO
141900000000IF  1C     TBGATB        IFNE      *BLANKS
142000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
142100000000E   1C                   ENDIF
142200960105     C*
142300960115     C* DATI IN STATISTICA <> DA QUELLI RICHIESTI
142400960115IF  1C     PARIOE        IFNE      *BLANKS
142500960115IF  2C     PARIOE        IFNE      TBGIOE
142600960105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
142700960105E   2C                   ENDIF
142800960105E   1C                   ENDIF
142900000000     C*
143000000000     C                   ENDSR
143100000000     C*------------------------------------------------------------------------*
143200000000     C*?MEMBDG - MEMORIZZA VALORI BUDGET
143300000000     C*------------------------------------------------------------------------*
143400000000     C     MEMBDG        BEGSR
143500000000     C*                                                    *PARTENZA
143600000000IF  1C     PARMPA        IFEQ      'P'
143700000000     C                   Z-ADD     TBGNSP        LAVSPE
143800000000     C                   Z-ADD     TBGKGP        LAVCHI
143900000000     C                   Z-ADD     TBGNCP        LAVCOL
144000000000     C                   Z-ADD     TBGVOP        LAVVOL
144100000000X   1C                   ELSE                                                   *ARRIVO
144200000000     C                   Z-ADD     TBGNSA        LAVSPE
144300000000     C                   Z-ADD     TBGKGA        LAVCHI
144400000000     C                   Z-ADD     TBGNCA        LAVCOL
144500000000     C                   Z-ADD     TBGVOA        LAVVOL
144600000000E   1C                   ENDIF
144700000000     C*
144800000000     C                   MOVEL     TBGFPE        FASCIA
144900000000     C                   Z-ADD     1             MM
145000000000     C*
145100000000     C* RICERCA DELLA POSIZIONE IN SHIERA DELL'ANNO/MESE CHE STO ELABORANDO
145200000000     C                   SETOFF                                           28
145300000000     C     TBGMES        LOOKUP    MES(MM)                                28
145400000000     C*
145500000000     C* SE MESE TROVATO, ADDIZIONO
145600000000IF  1C     *IN28         IFEQ      *ON
145700000000     C*
145800000000     C* FASCIA DI PESO PER FILIALE
145900000000     C     FASCIA        OCCUR     FPFIL
146000000000     C                   ADD       LAVSPE        SBF(MM)
146100000000     C                   ADD       LAVCHI        QBF(MM)
146200000000     C                   ADD       LAVCOL        CBF(MM)
146300000000     C                   ADD       LAVVOL        VBF(MM)
146400000000     C*
146500000000     C* FASCIA DI PESO PER AREA
146600000000     C     FASCIA        OCCUR     FPARE
146700000000     C                   ADD       LAVSPE        SBA(MM)
146800000000     C                   ADD       LAVCHI        QBA(MM)
146900000000     C                   ADD       LAVCOL        CBA(MM)
147000000000     C                   ADD       LAVVOL        VBA(MM)
147100000000     C*
147200000000     C* FASCIA DI PESO PER DIVISIONE
147300000000     C     FASCIA        OCCUR     FPDIV
147400000000     C                   ADD       LAVSPE        SBD(MM)
147500000000     C                   ADD       LAVCHI        QBD(MM)
147600000000     C                   ADD       LAVCOL        CBD(MM)
147700000000     C                   ADD       LAVVOL        VBD(MM)
147800000000     C*
147900000000     C* FASCIA DI PESO PER TOTALI GENERALI
148000000000     C     FASCIA        OCCUR     FPGEN
148100000000     C                   ADD       LAVSPE        SBG(MM)
148200000000     C                   ADD       LAVCHI        QBG(MM)
148300000000     C                   ADD       LAVCOL        CBG(MM)
148400000000     C                   ADD       LAVVOL        VBG(MM)
148500000000E   1C                   ENDIF
148600000000     C*
148700000000     C                   ENDSR
148800000000     C*------------------------------------------------------------------------*
148900000000     C*?STADET - IMPOSTA STAMPA TOTALI FILIALE DETTAGLIO
149000000000     C*------------------------------------------------------------------------*
149100000000     C     STADET        BEGSR
149200000000     C*
149300000000     C* °LE PERCENTUALI SUL BUDGET IN STAMPA LE IMPOSTO A ZERO PERCHE' I DATI
149400000000     C*  SUL FILE PER FILIALE/FILIALE NON ESISTONO
149500000000     C*
149600000000     C* CODICI INTESTAZIONE
149700980206     C                   MOVEL     DEPFIL        PTCFIL                         *FILIALE
149800000000     C                   MOVEL     DESFIL        PTDFIL
149900980206     C*---                 Z-ADD*ZEROS    PTCFIL
150000000000     C*
150100980206     C                   MOVEL     FIL(F)        PTCFIT                         *FILIALE DETTAGLIO
150200000000     C                   MOVEL     DFIL(F)       PTDFIT
150300980206     C*---                 Z-ADD*ZEROS    PTCFIT
150400000000     C*
150500980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
150600000000     C                   MOVEL     DESARE        PTDARE
150700980206     C*---                 Z-ADD*ZEROS    PTCARE
150800000000     C*
150900980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
151000980206     C                   MOVEL     DESDIV        PTDDIV
151100980206     C*---                 MOVELDEPDIV    D       10
151200980206     C*---                 MOVELDIV,D     PTDDIV
151300000000     C*
151400950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
151500950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
151600950529     C*
151700000000     C* SCHIERE SALDI MENSILI DI FILIALE DETTAGLIO
151800000000DO  1C     1             DO        10            A
151900000000     C     A             OCCUR     FPDET
152000000000     C     A             OCCUR     FPSTA
152100000000     C                   Z-ADD     SPT           SPS                            *SPEDIZIONI
152200000000     C                   Z-ADD     SAT           SAS
152300000000     C                   Z-ADD     QTT           QTS                            *KILOGRAMMI
152400000000     C                   Z-ADD     QAT           QAS
152500000000     C                   Z-ADD     CLT           CLS                            *COLLI
152600000000     C                   Z-ADD     CAT           CAS
152700000000     C                   Z-ADD     VLT           VLS                            *VOLUME
152800000000     C                   Z-ADD     VAT           VAS
152900000000     C*
153000000000     C* PULISCO I CAMPI DI LAVORO PER IL BUDGET
153100000000     C                   CLEAR                   SBS
153200000000     C                   CLEAR                   QBS
153300000000E   1C                   ENDDO
153400000000     C*
153500000000     C* IMPOSTO NO STAMPA DATI BUDGET°
153600000000     C                   MOVEL     '1'           NOBDG
153700000000     C*
153800000000     C* ROUTINE DI STAMPA TOTALI
153900000000     C                   EXSR      STAMPA
154000000000     C*
154100000000     C                   ENDSR
154200000000     C*------------------------------------------------------------------------*
154300000000     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
154400000000     C*------------------------------------------------------------------------*
154500000000     C     STAFIL        BEGSR
154600000000     C*
154700000000     C* CODICI INTESTAZIONE
154800980206     C                   MOVEL     DEPFIL        PTCFIL                         *FILIALE
154900000000     C                   MOVEL     DESFIL        PTDFIL
155000980206     C*---                 Z-ADD*ZEROS    PTCFIL
155100000000     C*
155200000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
155300000000     C                   MOVEL     *BLANKS       PTDFIT
155400000000     C                   MOVEL     PAR(11)       PTDFIT
155500000000     C*
155600980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
155700000000     C                   MOVEL     DESARE        PTDARE
155800980206     C*---                 Z-ADD*ZEROS    PTCARE
155900000000     C*
156000980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
156100980206     C                   MOVEL     DESDIV        PTDDIV
156200980206     C*---                 MOVELDEPDIV    D       10
156300980206     C*---                 MOVELDIV,D     PTDDIV
156400950529     C*
156500950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
156600950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
156700000000     C*
156800000000     C* SCHIERE SALDI MENSILI DI FILIALE
156900000000DO  1C     1             DO        10            A
157000000000     C     A             OCCUR     FPFIL
157100000000     C     A             OCCUR     FPSTA
157200000000     C                   Z-ADD     SPF           SPS                            *SPEDIZIONI
157300000000     C                   Z-ADD     SBF           SBS
157400000000     C                   Z-ADD     SAF           SAS
157500000000     C                   Z-ADD     QTF           QTS                            *KILOGRAMMI
157600000000     C                   Z-ADD     QBF           QBS
157700000000     C                   Z-ADD     QAF           QAS
157800000000     C                   Z-ADD     CLF           CLS                            *COLLI
157900000000     C                   Z-ADD     CAF           CAS
158000000000     C                   Z-ADD     VLF           VLS                            *VOLUME
158100000000     C                   Z-ADD     VAF           VAS
158200000000     C                   Z-ADD     CBF           CBS
158300000000     C                   Z-ADD     VBF           VBS
158400000000E   1C                   ENDDO
158500000000     C*
158600000000     C* ROUTINE DI STAMPA TOTALI
158700000000     C                   EXSR      STAMPA
158800000000     C*
158900000000     C                   ENDSR
159000000000     C*------------------------------------------------------------------------*
159100000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
159200000000     C*------------------------------------------------------------------------*
159300000000     C     STAARE        BEGSR
159400000000     C*
159500000000     C* CODICI INTESTAZIONE
159600000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
159700000000     C                   MOVEL     *BLANKS       PTDFIL
159800000000     C                   MOVEL     PAR(11)       PTDFIL
159900000000     C*
160000000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
160100000000     C                   MOVEL     *BLANKS       PTDFIT
160200000000     C                   MOVEL     PAR(11)       PTDFIT
160300000000     C*
160400980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
160500000000     C                   MOVEL     DESARE        PTDARE
160600980206     C*---                 Z-ADD*ZEROS    PTCARE
160700000000     C*
160800980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
160900980206     C                   MOVEL     DESDIV        PTDDIV
161000980206     C*---                 MOVELDEPDIV    D       10
161100980206     C*---                 MOVELDIV,D     PTDDIV
161200950529     C*
161300950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
161400950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
161500000000     C*
161600000000     C* SCHIERE SALDI MENSILI DI AREA
161700000000DO  1C     1             DO        10            A
161800000000     C     A             OCCUR     FPARE
161900000000     C     A             OCCUR     FPSTA
162000000000     C                   Z-ADD     SPA           SPS                            *SPEDIZIONI
162100000000     C                   Z-ADD     SBA           SBS
162200000000     C                   Z-ADD     SAA           SAS
162300000000     C                   Z-ADD     QTA           QTS                            *KILOGRAMMI
162400000000     C                   Z-ADD     QBA           QBS
162500000000     C                   Z-ADD     QAA           QAS
162600000000     C                   Z-ADD     CLA           CLS                            *COLLI
162700000000     C                   Z-ADD     CAA           CAS
162800000000     C                   Z-ADD     VLA           VLS                            *VOLUME
162900000000     C                   Z-ADD     VAA           VAS
163000000000     C                   Z-ADD     CBA           CBS
163100000000     C                   Z-ADD     VBA           VBS
163200000000E   1C                   ENDDO
163300000000     C*
163400000000     C* ROUTINE DI STAMPA TOTALI
163500000000     C                   EXSR      STAMPA
163600000000     C*
163700000000     C                   ENDSR
163800000000     C*------------------------------------------------------------------------*
163900000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
164000000000     C*------------------------------------------------------------------------*
164100000000     C     STADIV        BEGSR
164200000000     C*
164300000000     C* CODICI INTESTAZIONE
164400000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
164500000000     C                   MOVEL     *BLANKS       PTDFIL
164600000000     C                   MOVEL     PAR(11)       PTDFIL
164700000000     C*
164800000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
164900000000     C                   MOVEL     *BLANKS       PTDFIT
165000000000     C                   MOVEL     PAR(11)       PTDFIT
165100000000     C*
165200000000     C                   MOVEL     *BLANKS       PTCARE                         *AREA
165300000000     C                   MOVEL     *BLANKS       PTDARE
165400000000     C                   MOVEL     PAR(11)       PTDARE
165500000000     C*
165600980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
165700980206     C                   MOVEL     DESDIV        PTDDIV
165800980206     C*---                 MOVELDEPDIV    D       10
165900980206     C*---                 MOVELDIV,D     PTDDIV
166000950529     C*
166100950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
166200950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
166300000000     C*
166400000000     C* SCHIERE SALDI MENSILI DI DIVISIONE
166500000000DO  1C     1             DO        10            A
166600000000     C     A             OCCUR     FPDIV
166700000000     C     A             OCCUR     FPSTA
166800000000     C                   Z-ADD     SPD           SPS                            *SPEDIZIONI
166900000000     C                   Z-ADD     SBD           SBS
167000000000     C                   Z-ADD     SAD           SAS
167100000000     C                   Z-ADD     QTD           QTS                            *KILOGRAMMI
167200000000     C                   Z-ADD     QBD           QBS
167300000000     C                   Z-ADD     QAD           QAS
167400000000     C                   Z-ADD     CLD           CLS                            *COLLI
167500000000     C                   Z-ADD     CAD           CAS
167600000000     C                   Z-ADD     VLD           VLS                            *VOLUME
167700000000     C                   Z-ADD     VAD           VAS
167800000000     C                   Z-ADD     CBD           CBS
167900000000     C                   Z-ADD     VBD           VBS
168000000000E   1C                   ENDDO
168100000000     C*
168200000000     C* ROUTINE DI STAMPA TOTALI
168300000000     C                   EXSR      STAMPA
168400000000     C*
168500000000     C                   ENDSR
168600000000     C*------------------------------------------------------------------------*
168700000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
168800000000     C*------------------------------------------------------------------------*
168900000000     C     STAGEN        BEGSR
169000000000     C*
169100000000     C* CODICI INTESTAZIONE
169200000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
169300000000     C                   MOVEL     *BLANKS       PTDFIL
169400000000     C                   MOVEL     PAR(11)       PTDFIL
169500000000     C*
169600000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
169700000000     C                   MOVEL     *BLANKS       PTDFIT
169800000000     C                   MOVEL     PAR(11)       PTDFIT
169900000000     C*
170000000000     C                   MOVEL     *BLANKS       PTCARE                         *AREA
170100000000     C                   MOVEL     *BLANKS       PTDARE
170200000000     C                   MOVEL     PAR(11)       PTDARE
170300000000     C*
170400000000     C                   MOVEL     *BLANKS       PTCDIV                         *DIVISIONE
170500000000     C                   MOVEL     *BLANKS       PTDDIV
170600000000     C                   MOVEL     PAR(11)       PTDDIV
170700950529     C*
170800950529     C                   MOVEL     PADMPA        PTDMP2                         *TIPO MOVIMENTO
170900950529     C                   MOVEL     PADIOE        PTDIOE                         *DATI IN STATISTICA
171000000000     C*
171100000000     C* SCHIERE SALDI MENSILI GENERALI
171200000000DO  1C     1             DO        10            A
171300000000     C     A             OCCUR     FPGEN
171400000000     C     A             OCCUR     FPSTA
171500000000     C                   Z-ADD     SPG           SPS                            *SPEDIZIONI
171600000000     C                   Z-ADD     SBG           SBS
171700000000     C                   Z-ADD     SAG           SAS
171800000000     C                   Z-ADD     QTG           QTS                            *KILOGRAMMI
171900000000     C                   Z-ADD     QBG           QBS
172000000000     C                   Z-ADD     QAG           QAS
172100000000     C                   Z-ADD     CLG           CLS                            *COLLI
172200000000     C                   Z-ADD     CAG           CAS
172300000000     C                   Z-ADD     VLG           VLS                            *VOLUME
172400000000     C                   Z-ADD     VAG           VAS
172500000000     C                   Z-ADD     CBG           CBS
172600000000     C                   Z-ADD     VBG           VBS
172700000000E   1C                   ENDDO
172800000000     C*
172900000000     C* ROUTINE DI STAMPA TOTALI DI FILIALE
173000000000     C                   EXSR      STAMPA
173100000000     C*
173200000000     C                   ENDSR
173300000000     C*------------------------------------------------------------------------*
173400000000     C*?STAMPA - STAMPA TOTALI GENERALI DI DIVISIONE/AREA/FILIALE
173500000000     C*------------------------------------------------------------------------*
173600000000     C     STAMPA        BEGSR
173700000000     C*
173800000000     C* REIMPOSTO IL CONTATORE DI FASCE STAMPATE PER RISTAMPARE LA TESTATA
173900000000     C                   Z-ADD     *ZEROS        CONFAS            2 0
174000000000     C*
174100000000     C* PULISCO I TOTALI DI LAVORO GENERALI/DIVISIONE/AREA/FILIALE
174200000000     C                   CLEAR                   SPW                            *SPEDIZIONI
174300000000     C                   CLEAR                   SBW
174400000000     C                   CLEAR                   SAW
174500000000     C                   CLEAR                   QTW                            *KILOGRAMMI
174600000000     C                   CLEAR                   QBW
174700000000     C                   CLEAR                   QAW
174800000000     C                   CLEAR                   CLW                            *COLLI
174900000000     C                   CLEAR                   CAW
175000000000     C                   CLEAR                   VLW                            *VOLUME
175100000000     C                   CLEAR                   VAW
175200000000     C                   CLEAR                   CBW
175300000000     C                   CLEAR                   VBW
175400000000     C*
175500000000     C* STAMPA  T E S T A T A
175600020115     C                   WRITE     SE46PT                                       *TESTATA
175700020115     C                   WRITE     SE46PR                                       *RIGA
175800020115     C                   WRITE     SE46PT1                                      *DECOD FIL/AREA/DIV
175900020115     C                   WRITE     SE46PR                                       *RIGA
176000020115     C                   WRITE     SE46PI                                       *DECODIFICA MESI
176100000000     C*
176200000000     C* PER OGNI FASCIA LETTA
176300000000DO  1C     1             DO        10            A
176400000000     C     A             OCCUR     FPSTA
176500000000     C*
176600000000     C                   ADD       1             CONFAS                         *CONT. FASCE STAMPATE
176700000000     C                   MOVEL     FPE(A)        PPDFPE                         *DESCRIZIONE FASCIA
176800000000     C*
176900000000     C* ADDIZIONA I SALDI PER I TOTALI A FINE PAGINA
177000000000     C                   ADD       SPS           SPW                            *SPEDIZIONI
177100000000     C                   ADD       SBS           SBW
177200000000     C                   ADD       SAS           SAW
177300000000     C                   ADD       QTS           QTW                            *KILOGRAMMI
177400000000     C                   ADD       QBS           QBW
177500000000     C                   ADD       QAS           QAW
177600000000     C                   ADD       CLS           CLW                            *COLLI
177700000000     C                   ADD       CAS           CAW
177800000000     C                   ADD       VLS           VLW                            *VOLUME
177900000000     C                   ADD       VAS           VAW
178000000000     C                   ADD       CBS           CBW
178100000000     C                   ADD       VBS           VBW
178200000000     C*
178300000000     C* SE SONO ALLA 6^ FASCIA STAMPO LA TESTATA A SALTO PAGINA
178400000000IF  2C     CONFAS        IFEQ      6
178500020115     C                   WRITE     SE46PR                                       *RIGA
178600020115     C                   WRITE     SE46PT                                       *TESTATA
178700020115     C                   WRITE     SE46PR                                       *RIGA
178800020115     C                   WRITE     SE46PI                                       *DECODIFICA MESI
178900000000E   2C                   ENDIF
179000000000     C*
179100020115     C                   WRITE     SE46PP                                       *STAMPA DECOD. FASCIA
179200000000     C*
179300000000     C* S T A M P A   F A S C I A
179400000000     C*
179500000000     C                   EXSR      STFASC
179600000000     C*
179700000000E   1C                   ENDDO
179800000000     C*
179900000000     C* TRASFERISCE I TOTALI A FINE PAGINA NELLE SCHIERE DI LAVORO
180000000000     C                   Z-ADD     SPW           SPS                            *SPEDIZIONI
180100000000     C                   Z-ADD     SBW           SBS
180200000000     C                   Z-ADD     SAW           SAS
180300000000     C                   Z-ADD     QTW           QTS                            *KILOGRAMMI
180400000000     C                   Z-ADD     QBW           QBS
180500000000     C                   Z-ADD     QAW           QAS
180600000000     C                   Z-ADD     CLW           CLS                            *COLLI
180700000000     C                   Z-ADD     CAW           CAS
180800000000     C                   Z-ADD     VLW           VLS                            *VOLUME
180900000000     C                   Z-ADD     VAW           VAS
181000000000     C                   Z-ADD     CBW           CBS
181100000000     C                   Z-ADD     VBW           VBS
181200000000     C*
181300000000     C* S T A M P A   T O T A L E
181400000000     C*
181500000000     C                   MOVEL     DRI(7)        PPDFPE                         *DESCRIZIONE TOTALE
181600020115     C                   WRITE     SE46PP                                       *STAMPA DESCR. TOTALE
181700000000     C                   EXSR      STFASC
181800000000     C*
181900000000     C* S T A M P A   R A P P O R T I
182000000000     C*
182100000000     C                   EXSR      STRAPP
182200000000     C*
182300000000     C* STAMPA RIGA DI CHIUSURA PROSPETTO
182400020115     C                   WRITE     SE46PR
182500000000     C*
182600000000     C                   ENDSR
182700000000     C*------------------------------------------------------------------------*
182800000000     C*?STFASC - STAMPA I VALORI DELLA FASCIA O DEI TOTALI A FINE PAGINA
182900000000     C*------------------------------------------------------------------------*
183000000000     C     STFASC        BEGSR
183100000000     C*
183200000000     C*---------------------
183300000000     C* S P E D I Z I O N I
183400000000     C*---------------------
183500000000     C* DESCRIZIONE DEL VALORE DA STAMPARE E DELLE SUE PERCENTUALI
183600000000     C                   MOVEL     DRI(1)        P1DFAS
183700000000     C                   MOVEL     DRI(5)        P2DFAS
183800000000     C*
183900000000     C* CALCOLA LE PERCENTUALI, LA 13^ COLONNA E STAMPA LA RIGA
184000000000     C                   Z-ADD     SPS           $VA                            *VALORE ANNO CORRENTE
184100000000     C                   Z-ADD     SBS           $BD                            *VALORE BUDGET
184200000000     C                   Z-ADD     SAS           $AP                            *VALORE AA PRECEDENTE
184300000000     C                   EXSR      STRIGA
184400000000     C*---------------------
184500000000     C* K I L O G R A M M I  (IN STAMPA QLI)
184600000000     C*---------------------
184700000000     C* DESCRIZIONE DEL VALORE DA STAMPARE E DELLE SUE PERCENTUALI
184800000000     C                   MOVEL     DRI(2)        P1DFAS
184900000000     C                   MOVEL     DRI(5)        P2DFAS
185000000000     C*
185100000000     C* CALCOLA LE PERCENTUALI, LA 13^ COLONNA E STAMPA LA RIGA
185200000000     C     QTS           DIV       100           $VA                            *VALORE ANNO CORRENTE
185300000000     C     QBS           DIV       100           $BD                            *VALORE BUDGET
185400000000     C     QAS           DIV       100           $AP                            *VALORE AA PRECEDENTE
185500000000     C                   EXSR      STRIGA
185600000000     C*---------------------
185700000000     C* C O L L I
185800000000     C*---------------------
185900000000     C* DESCRIZIONE DEL VALORE DA STAMPARE E DELLE SUE PERCENTUALI
186000000000     C                   MOVEL     DRI(3)        P1DFAS
186100000000     C                   MOVEL     DRI(5)        P2DFAS
186200000000     C*
186300000000     C* CALCOLA LE PERCENTUALI, LA 13^ COLONNA E STAMPA LA RIGA
186400000000     C                   Z-ADD     CLS           $VA                            *VALORE ANNO CORRENTE
186500000000     C                   Z-ADD     CBS           $BD                            *VALORE BUDGET
186600000000     C                   Z-ADD     CAS           $AP                            *VALORE AA PRECEDENTE
186700000000     C                   EXSR      STRIGA
186800000000     C*---------------------
186900000000     C* V O L U M E
187000000000     C*---------------------
187100000000     C* DESCRIZIONE DEL VALORE DA STAMPARE E DELLE SUE PERCENTUALI
187200000000     C                   MOVEL     DRI(4)        P1DFAS
187300000000     C                   MOVEL     DRI(5)        P2DFAS
187400000000     C*
187500000000     C* CALCOLA LE PERCENTUALI, LA 13^ COLONNA E STAMPA LA RIGA
187600000000     C                   Z-ADD     VLS           $VA                            *VALORE ANNO CORRENTE
187700000000     C                   Z-ADD     VBS           $BD                            *VALORE BUDGET
187800000000     C                   Z-ADD     VAS           $AP                            *VALORE AA PRECEDENTE
187900000000     C                   EXSR      STRIGA
188000000000     C*
188100000000     C                   ENDSR
188200000000     C*------------------------------------------------------------------------*
188300000000     C*?STRIGA - STAMPA RIGA FASCIA O TOTALI FINE PAGINA
188400000000     C*------------------------------------------------------------------------*
188500000000     C     STRIGA        BEGSR
188600000000     C*
188700000000     C* °SE NON HO MEMORIZZATO I VALORI DEL BUDGET (SCHIERE VUOTE);
188800000000     C*  SE STO STAMPANDO I TOTALI PER FIL-DETTAGLIO(PER CUI NON CI SONO I DATI)
188900000000     C*  IMPOSTO LA PERCENTUALE DEL BUDGET A ZERO
189000000000     C*
189100000000     C* PER OGNI MESE
189200000000DO  1C     1             DO        PARJ          X                 2 0
189300000000     C*
189400000000     C* CALCOLA LE PERCENTUALI
189500000000     C                   Z-ADD     $VA(X)        CONSUN           17 3          *VALORE CORRENTE
189600000000     C*--------------------
189700000000     C* SUL  B U D G E T°
189800000000     C*--------------------
189900000000IF  2C     NOBDG         IFEQ      '0'
190000000000     C                   Z-ADD     $BD(X)        RIFERI           17 3          *VALORE BUDGET
190100000000     C                   EXSR      CALPER
190200000000     C                   Z-ADD     PERCEN        $PB(X)                         *VALORE PERCENTUALE
190300000000X   2C                   ELSE
190400000000     C                   Z-ADD     *ZEROS        $PB(X)                         *PERCENTUALE A ZERO
190500000000E   2C                   ENDIF
190600000000     C*--------------------
190700000000     C* SULL' ANNO  P R E C E D E N T E
190800000000     C*--------------------
190900000000     C                   Z-ADD     $AP(X)        RIFERI                         *VALORE ANNO PRECEDEN
191000000000     C                   EXSR      CALPER
191100000000     C                   Z-ADD     PERCEN        $PA(X)                         *VALORE PERCENTUALE
191200000000     C*
191300000000E   1C                   ENDDO
191400000000     C*
191500000000     C* CALCOLA LA 13^ COLONNA
191600000000     C                   XFOOT     $VA           $VA13C           17 3          *ANNO CORRENTE
191700000000     C                   XFOOT     $BD           $BD13C           17 3          *BUDGET
191800000000     C                   XFOOT     $AP           $AP13C           17 3          *ANNO PRECEDENTE
191900000000     C*
192000000000     C* E LE SUE PERCENTUALI
192100000000     C                   Z-ADD     $VA13C        CONSUN                         *VALORE CORRENTE
192200000000     C*--------------------
192300000000     C* SUL  B U D G E T°
192400000000     C*--------------------
192500000000IF  1C     NOBDG         IFEQ      '0'
192600000000     C                   Z-ADD     $BD13C        RIFERI                         *VALORE BUDGET
192700000000     C                   EXSR      CALPER
192800000000     C                   Z-ADD     PERCEN        $PB13C            4 1          *VALORE PERCENTUALE
192900000000X   1C                   ELSE
193000000000     C                   Z-ADD     *ZEROS        $PB13C                         *PERCENTUALE A ZERO
193100000000E   1C                   ENDIF
193200000000     C*--------------------
193300000000     C* SULL' ANNO  P R E C E D E N T E
193400000000     C*--------------------
193500000000     C                   Z-ADD     $AP13C        RIFERI                         *VALORE ANNO PRECEDEN
193600000000     C                   EXSR      CALPER
193700000000     C                   Z-ADD     PERCEN        $PA13C            4 1          *VALORE PERCENTUALE
193800000000     C*
193900000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
194000000000     C                   EXSR      STACAM
194100000000     C*
194200000000     C                   ENDSR
194300000000     C*------------------------------------------------------------------------*
194400000000     C*?CALPER - CALCOLA LE PERCENTUALI A.P./BDG SUI SALDI
194500000000     C*          INPUT  -- CONSUN: VALORE CORRENTE
194600000000     C*                    RIFERI: VALORE DELL' ANNO PRECEDENTE / BUDGET
194700000000     C*          OUTPUT -- PERCEN: PERCENTUALE CALCOLATA
194800000000     C*------------------------------------------------------------------------*
194900000000     C     CALPER        BEGSR
195000000000     C*
195100000000     C*   VALORE CONSUNTIVO  *  100
195200000000     C*  ---------------------------- - 100 =  % SCOSTAMENTO (AP/BDG)
195300000000     C*   VALORE RIFERIMENTO (AP/BDG)
195400000000     C*
195500000000     C* A) SIA IL VALORE A CONSUNTIVO CHE QUELLO DI RIFERIMENTO SONO = ZERO;
195600000000     C*    % DI SCOSTAMENTO = ZERO
195700000000     C* B) IL VALORE A CONSUNTIVO E' = ZERO MENTRE QUELLO DI RIFERIMENTO NO;
195800000000     C*    % DI SCOSTAMENTO = -999,9
195900000000     C* C) IL VALORE A CONSUNTIVO E' > ZERO MENTRE QUELLO DI RIFERIMENTO E' = 0;
196000000000     C*    % DI SCOSTAMENTO = +999,9
196100000000     C* D) SIA IL VALORE A CONSUNTIVO CHE QUELLO DI RIFERIMENTO SONO > ZERO;
196200000000     C*    % DI SCOSTAMENTO = +/-XXX,X
196300000000     C*
196400000000     C                   SETOFF                                           28
196500000000     C                   Z-ADD     *ZEROS        PERCEN            4 1
196600000000     C*
196700000000IF  1C     CONSUN        IFEQ      *ZEROS
196800000000IF  2C     RIFERI        IFEQ      *ZEROS
196900000000     C                   Z-ADD     *ZEROS        PERCEN                         *CASO A
197000000000X   2C                   ELSE
197100000000     C                   Z-ADD     -999,9        PERCEN                         *CASO B
197200000000E   2C                   ENDIF
197300000000X   1C                   ELSE
197400000000IF  2C     RIFERI        IFEQ      *ZEROS
197500000000     C                   Z-ADD     +999,9        PERCEN                         *CASO C
197600000000X   2C                   ELSE
197700000000     C*
197800000000     C     CONSUN        MULT      100           RISUL            19 3
197900000000     C     RISUL         DIV       RIFERI        RISUL
198000000000     C     RISUL         SUB       100           RISUL
198100000000     C*
198200000000     C* SE LA PERCENTUALE DI SCOSTAMENTO E' +/-1000  METTO  +/-999,9%
198300000000     C                   Z-ADD     RISUL         PERCEN
198400000000     C     RISUL         COMP      0                                    28
198500000000     C* POSITIVO
198600000000IF  3C     *IN28         IFEQ      *OFF
198700000000IF  4C     RISUL         IFGE      1000
198800000000     C                   Z-ADD     +999,9        PERCEN
198900000000E   4C                   ENDIF
199000000000     C* NEGATIVO
199100000000X   3C                   ELSE
199200000000IF  4C     RISUL         IFLE      -1000
199300000000     C                   Z-ADD     -999,9        PERCEN
199400000000E   4C                   ENDIF
199500000000E   3C                   ENDIF
199600000000     C*
199700000000E   2C                   ENDIF
199800000000E   1C                   ENDIF
199900000000     C*
200000000000     C                   ENDSR
200100000000     C*------------------------------------------------------------------------*
200200000000     C*?STACAM - MUOVE I CAMPI VALORE E % IN QUELLI DI STAMPA E STAMPA
200300000000     C*------------------------------------------------------------------------*
200400000000     C     STACAM        BEGSR
200500000000     C*
200600000000     C* VALORE ANNO CORRENTE
200700000000     C                   Z-ADD     $VA(1)        P1CQT1                         *MESE UNO
200800000000     C                   Z-ADD     $VA(2)        P1CQT2                         *MESE DUE
200900000000     C                   Z-ADD     $VA(3)        P1CQT3                         *MESE TRE
201000000000     C                   Z-ADD     $VA(4)        P1CQT4                         *MESE QUATTRO
201100000000     C                   Z-ADD     $VA(5)        P1CQT5                         *MESE CINQUE
201200000000     C                   Z-ADD     $VA(6)        P1CQT6                         *MESE SEI
201300000000     C                   Z-ADD     $VA(7)        P1CQT7                         *MESE SETTE
201400000000     C                   Z-ADD     $VA(8)        P1CQT8                         *MESE OTTO
201500000000     C                   Z-ADD     $VA(9)        P1CQT9                         *MESE NOVE
201600000000     C                   Z-ADD     $VA(10)       P1CQT0                         *MESE DIECI
201700000000     C                   Z-ADD     $VA(11)       P1CQTA                         *MESE UNDICI
201800000000     C                   Z-ADD     $VA(12)       P1CQTB                         *MESE DODICI
201900000000     C                   Z-ADD     $VA13C        P1CQTT                         *TOTALE MESI
202000000000     C*
202100020115     C                   WRITE     SE46P1                                       *STAMPA RIGA
202200000000     C*
202300000000     C* PERCENTUALI %BGD E %AP
202400120227     C**                 Z-ADD     $PB(1)        P1CPB1
202500000000     C                   Z-ADD     $PA(1)        P1CPA1
202600120227     C**                 Z-ADD     $PB(2)        P1CPB2
202700000000     C                   Z-ADD     $PA(2)        P1CPA2
202800120227     C**                 Z-ADD     $PB(3)        P1CPB3
202900000000     C                   Z-ADD     $PA(3)        P1CPA3
203000120227     C**                 Z-ADD     $PB(4)        P1CPB4
203100000000     C                   Z-ADD     $PA(4)        P1CPA4
203200120227     C**                 Z-ADD     $PB(5)        P1CPB5
203300000000     C                   Z-ADD     $PA(5)        P1CPA5
203400120227     C**                 Z-ADD     $PB(6)        P1CPB6
203500000000     C                   Z-ADD     $PA(6)        P1CPA6
203600120227     C**                 Z-ADD     $PB(7)        P1CPB7
203700000000     C                   Z-ADD     $PA(7)        P1CPA7
203800120227     C**                 Z-ADD     $PB(8)        P1CPB8
203900000000     C                   Z-ADD     $PA(8)        P1CPA8
204000120227     C**                 Z-ADD     $PB(9)        P1CPB9
204100000000     C                   Z-ADD     $PA(9)        P1CPA9
204200120227     C**                 Z-ADD     $PB(10)       P1CPB0
204300000000     C                   Z-ADD     $PA(10)       P1CPA0
204400120227     C**                 Z-ADD     $PB(11)       P1CPBA
204500000000     C                   Z-ADD     $PA(11)       P1CPAA
204600120227     C**                 Z-ADD     $PB(12)       P1CPBB
204700000000     C                   Z-ADD     $PA(12)       P1CPAB
204800120227     C**                 Z-ADD     $PB13C        P1CPBT
204900000000     C                   Z-ADD     $PA13C        P1CPAT
205000000000     C*
205100020115     C                   WRITE     SE46P2                                       *STAMPA RIGA
205200000000     C*
205300000000     C                   ENDSR
205400000000     C*------------------------------------------------------------------------*
205500000000     C*?STRAPP - STAMPA I RAPPORTI SUI TOTALI A FINE PAGINA
205600000000     C*------------------------------------------------------------------------*
205700000000     C     STRAPP        BEGSR
205800000000     C*
205900000000     C* KILOGRAMMI/SPEDIZIONI
206000000000     C*
206100000000     C                   MOVEL     DRI(8)        P3DFAS                         *DESCRIZIONE RAPPORTO
206200000000     C                   Z-ADD     QTS           $VS                            *VALORE SUPERIORE
206300000000     C                   Z-ADD     SPS           $VI                            *VALORE INFERIORE
206400000000     C*
206500000000     C* CALCOLA IL RAPPORTO, LA 13^ COLONNA E STAMPA LA RIGA
206600000000     C                   EXSR      STRIGR
206700000000     C*
206800000000     C* COLLI/SPEDIZIONI
206900000000     C*
207000000000     C                   MOVEL     DRI(9)        P3DFAS                         *DESCRIZIONE RAPPORTO
207100000000     C                   Z-ADD     CLS           $VS                            *VALORE SUPERIORE
207200000000     C*
207300000000     C* CALCOLA IL RAPPORTO, LA 13^ COLONNA E STAMPA LA RIGA
207400000000     C                   EXSR      STRIGR
207500000000     C*
207600000000     C* VOLUME/SPEDIZIONI
207700000000     C*
207800000000     C                   MOVEL     DRI(10)       P3DFAS                         *DESCRIZIONE RAPPORTO
207900000000     C                   Z-ADD     VLS           $VS                            *VALORE SUPERIORE
208000000000     C*
208100000000     C* CALCOLA IL RAPPORTO, LA 13^ COLONNA E STAMPA LA RIGA
208200000000     C                   EXSR      STRIGR
208300000000     C*
208400000000     C                   ENDSR
208500000000     C*------------------------------------------------------------------------*
208600000000     C*?STRIGR - STAMPA RIGA RAPPORTI
208700000000     C*------------------------------------------------------------------------*
208800000000     C     STRIGR        BEGSR
208900000000     C*
209000000000     C* PER OGNI MESE CALCOLA IL RAPPORTO
209100000000DO  1C     1             DO        PARJ          X
209200000000     C*
209300000000     C* SE SPEDIZIONI = 0, LE IMPOSTO = 1 PER EVITARE ERRORI NELLE DIVISIONI
209400000000IF  2C     $VI(X)        IFEQ      *ZEROS
209500000000     C                   Z-ADD     1             VALINF           17 3
209600000000X   2C                   ELSE
209700000000     C                   Z-ADD     $VI(X)        VALINF
209800000000E   2C                   ENDIF
209900000000     C*
210000000000     C     $VS(X)        DIV       VALINF        $RP(X)                         *RAPPORTO DEL MESE
210100000000     C*
210200000000E   1C                   ENDDO
210300000000     C*
210400000000     C* CALCOLA IL RAPPORTO PER LA 13^ COLONNA
210500000000     C                   XFOOT     $VS           $VS13C           17 3          *TOTALE VALORE SUPER
210600000000     C                   XFOOT     $VI           $VI13C           17 3          *TOTALE VALORE INFER
210700000000     C*
210800000000     C* SE SPEDIZIONI = 0, LE IMPOSTO = 1 PER EVITARE ERRORI NELLE DIVISIONI
210900000000IF  1C     $VI13C        IFEQ      *ZEROS
211000000000     C                   Z-ADD     1             $VI13C
211100000000E   1C                   ENDIF
211200000000     C*
211300000000     C     $VS13C        DIV       $VI13C        $RP13C            9 3          *RAPPORTO 13^ COLONNA
211400000000     C*
211500000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
211600000000     C                   EXSR      STARAP
211700000000     C*
211800000000     C                   ENDSR
211900000000     C*------------------------------------------------------------------------*
212000000000     C*?STARAP - MUOVE I RAPPORTI CALCOLATI NEI CAMPI DI STAMPA E STAMPA
212100000000     C*------------------------------------------------------------------------*
212200000000     C     STARAP        BEGSR
212300000000     C*
212400000000     C* RAPPORTO
212500000000     C                   Z-ADD     $RP(1)        P3CPE1                         *MESE UNO
212600000000     C                   Z-ADD     $RP(2)        P3CPE2                         *MESE DUE
212700000000     C                   Z-ADD     $RP(3)        P3CPE3                         *MESE TRE
212800000000     C                   Z-ADD     $RP(4)        P3CPE4                         *MESE QUATTRO
212900000000     C                   Z-ADD     $RP(5)        P3CPE5                         *MESE CINQUE
213000000000     C                   Z-ADD     $RP(6)        P3CPE6                         *MESE SEI
213100000000     C                   Z-ADD     $RP(7)        P3CPE7                         *MESE SETTE
213200000000     C                   Z-ADD     $RP(8)        P3CPE8                         *MESE OTTO
213300000000     C                   Z-ADD     $RP(9)        P3CPE9                         *MESE NOVE
213400000000     C                   Z-ADD     $RP(10)       P3CPE0                         *MESE DIECI
213500000000     C                   Z-ADD     $RP(11)       P3CPEA                         *MESE UNDICI
213600000000     C                   Z-ADD     $RP(12)       P3CPEB                         *MESE DODICI
213700000000     C                   Z-ADD     $RP13C        P3CPET                         *TOTALE MESI
213800000000     C*
213900020115     C                   WRITE     SE46P3                                       *STAMPA RIGA RAPPORTI
214000000000     C*
214100000000     C                   ENDSR
214200000000     C*------------------------------------------------------------------------*
214300000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
214400000000     C*------------------------------------------------------------------------*
214500000000     C     DECTES        BEGSR
214600000000     C*
214700950529     C* TIPO MOVIMENTI
214800000000IF  1C     PARMPA        IFEQ      'P'
214900950529     C                   MOVEL     PAR(1)        PTCMPA
215000950529     C                   MOVEL     PAR(1)        PTDMP2
215100951005     C                   MOVEL     PAR(2)        PTDMPA                         *DESCR FIL DET=ARRIVO
215200000000X   1C                   ELSE
215300950529     C                   MOVEL     PAR(2)        PTCMPA
215400950529     C                   MOVEL     PAR(2)        PTDMP2
215500951005     C                   MOVEL     PAR(1)        PTDMPA                         *DESCR FIL DET=PARTEN
215600000000E   1C                   ENDIF
215700950529     C*
215800950529     C* DATI IN STATISTICA
215900950529IF  1C     PARIOE        IFEQ      'I'
216000950602     C                   MOVEL     PAR(12)       PTCIOE
216100950529     C                   MOVEL     PAR(12)       PTDIOE                         *DATI "ITALIA"
216200950529E   1C                   ENDIF
216300950529IF  1C     PARIOE        IFEQ      'M'
216400950602     C                   MOVEL     PAR(13)       PTCIOE
216500950529     C                   MOVEL     PAR(13)       PTDIOE                         *DATI "IMPORT"
216600950529E   1C                   ENDIF
216700950529IF  1C     PARIOE        IFEQ      'X'
216800950602     C                   MOVEL     PAR(14)       PTCIOE
216900950529     C                   MOVEL     PAR(14)       PTDIOE                         *DATI "EXPORT"
217000950529E   1C                   ENDIF
217100950529IF  1C     PARIOE        IFEQ      *BLANKS
217200950602     C                   MOVEL     PAR(16)       PTCIOE
217300950602     C                   MOVEL     PAR(16)       PTDIOE                         *DATI "TUTTI"
217400950529E   1C                   ENDIF
217500950529     C*
217600000000     C* DECODIFICA MESI
217700000000IF  1C     MES(1)        IFGT      *ZEROS
217800000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ.
217900000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
218000990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
218100000000E   1C                   ENDIF
218200000000     C*
218300000000IF  1C     MES(2)        IFGT      *ZEROS
218400000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ.
218500000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
218600990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
218700000000E   1C                   ENDIF
218800000000     C*
218900000000IF  1C     MES(3)        IFGT      *ZEROS
219000000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ.
219100000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
219200990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
219300000000E   1C                   ENDIF
219400000000     C*
219500000000IF  1C     MES(4)        IFGT      *ZEROS
219600000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ.
219700000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
219800990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
219900000000E   1C                   ENDIF
220000000000     C*
220100000000IF  1C     MES(5)        IFGT      *ZEROS
220200000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ.
220300000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
220400990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
220500000000E   1C                   ENDIF
220600000000     C*
220700000000IF  1C     MES(6)        IFGT      *ZEROS
220800000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ.
220900000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
221000990608     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
221100000000E   1C                   ENDIF
221200000000     C*
221300000000IF  1C     MES(7)        IFGT      *ZEROS
221400000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ.
221500000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
221600990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
221700000000E   1C                   ENDIF
221800000000     C*
221900000000IF  1C     MES(8)        IFGT      *ZEROS
222000000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ.
222100000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
222200990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
222300000000E   1C                   ENDIF
222400000000     C*
222500000000IF  1C     MES(9)        IFGT      *ZEROS
222600000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ.
222700000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
222800990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
222900000000E   1C                   ENDIF
223000000000     C*
223100000000IF  1C     MES(10)       IFGT      *ZEROS
223200000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ.
223300000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
223400990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
223500000000E   1C                   ENDIF
223600000000     C*
223700000000IF  1C     MES(11)       IFGT      *ZEROS
223800000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ.
223900000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
224000990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
224100000000E   1C                   ENDIF
224200000000     C*
224300000000IF  1C     MES(12)       IFGT      *ZEROS
224400000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ.
224500000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
224600990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
224700000000E   1C                   ENDIF
224800000000     C*
224900000000     C* DECODIFICA FASCE DI PESO
225000990527IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
225100990217     C                   MOVEL     'SDR'         WSX               3
225200990217X   1C                   ELSE                                                   *FASCE DAL '99
225300020328     C                   MOVEL     'S9R'         WSX
225400020325E   1C                   ENDIF
225500000000     C                   Z-ADD     1             IN
225600020906     C                   Z-ADD     1             KTBUTE
225700000000     C                   MOVEL     '2L'          KTBCOD
225800000000     C     KEYTA2        SETLL     TABEL00F
225900000000     C     KEYTA2        READE     TABEL00F                               99
226000000000DO  1C     *IN99         DOWEQ     *OFF
226100000000     C                   MOVEL     TBLKEY        FILEST            3            *FILE STATISTICA
226200990217IF  2C     FILEST        IFEQ      WSX
226300000000     C                   MOVEL     TBLUNI        FPE(IN)
226400000000     C                   ADD       1             IN
226500000000E   2C                   ENDIF
226600000000     C     KEYTA2        READE     TABEL00F                               99
226700000000E   1C                   ENDDO
226800000000     C*
226900000000     C                   ENDSR
227000000000     C*------------------------------------------------------------------------*
227100000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
227200000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
227300000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
227400000000     C*------------------------------------------------------------------------*
227500000000     C     DECDIV        BEGSR
227600000000     C*
227700020906     C                   MOVEL     1             KTBUTE
227800000000     C                   MOVE      '17'          KTBCOD
227900000000     C                   MOVE      *BLANKS       KTBKEY
228000000000     C                   MOVEL     DEPDIV        KTBKEY
228100000000     C     KEYTAB        CHAIN     TABEL00F                           99
228200000000     C*
228300000000IF  1C     *IN99         IFEQ      '1'
228400000000     C     TBLFLG        OREQ      '*'
228500000000     C                   MOVEL     *ALL'*'       DESDIV           20
228600000000X   1C                   ELSE
228700000000     C                   MOVEL     TBLUNI        DESDIV
228800000000E   1C                   ENDIF
228900000000     C*
229000000000     C                   ENDSR
229100000000     C*------------------------------------------------------------------------*
229200000000     C*? DECARE - DECODIFICA CODICE AREA
229300000000     C*          INPUT  -- DEPARE: CODICE AREA
229400000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
229500000000     C*------------------------------------------------------------------------*
229600000000     C     DECARE        BEGSR
229700000000     C*
229800020906     C                   MOVEL     1             KTBUTE
229900000000     C                   MOVE      '05'          KTBCOD
230000000000     C                   MOVE      *BLANKS       KTBKEY
230100000000     C                   MOVEL     DEPARE        KTBKEY
230200000000     C     KEYTAB        CHAIN     TABEL00F                           99
230300000000     C*
230400000000IF  1C     *IN99         IFEQ      '1'
230500000000     C     TBLFLG        OREQ      '*'
230600000000     C                   MOVEL     *ALL'*'       DESARE           20
230700000000X   1C                   ELSE
230800000000     C                   MOVEL     TBLUNI        DESARE
230900000000E   1C                   ENDIF
231000000000     C*
231100000000     C                   ENDSR
231200000000     C*------------------------------------------------------------------------*
231300000000     C*? DECFIL - DECODIFICA CODICE FILIALE
231400000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
231500000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
231600000000     C*------------------------------------------------------------------------*
231700000000     C     DECFIL        BEGSR
231800000000     C*
231900000000     C                   Z-ADD     1             I
232000000000     C     DEPFIL        LOOKUP    FIL(I)                                 99
232100000000IF  1C     *IN99         IFEQ      '0'
232200000000     C                   MOVEL     *ALL'*'       DESFIL           20
232300020325     C                   MOVEL     *BLANKS       LAVNTX
232400000000X   1C                   ELSE
232500000000     C                   MOVEL     DFIL(I)       DESFIL
232600000703     C                   MOVEL     NTX(I)        LAVNTX                         *NETWORK
232700000000E   1C                   ENDIF
232800000000     C*
232900000000     C                   ENDSR
233000000000     C*------------------------------------------------------------------------*
233100000000     C*?CARORG - CARICA I CODICI DELLE FILIALI E LE LORO DECODIFICHE
233200000000     C*------------------------------------------------------------------------*
233300000000     C     CARORG        BEGSR
233400000000     C*
233500000000     C                   Z-ADD     *ZEROS        KORFIL
233600000000     C     KEYOR2        SETLL     AZORG01L                           99        *RECORD NON TROVATO
233700000000     C  N99              READ      AZORG01L                               99
233800000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
233900000000DO  1C     *IN99         DOWEQ     '0'
234000000000IF  2C     ORGFVA        IFEQ      *BLANKS
234100000000IF  3C     ORGFAG        IFNE      'V'
234200000703     C                   CLEAR                   OG143
234300000703     C                   MOVEL     ORGDE3        OG143
234400000000     C                   ADD       1             I
234500000000     C                   Z-ADD     ORGFIL        FIL(I)
234600000000     C                   MOVEL     ORGDES        DFIL(I)
234700020325     C* CALCOLA NETWORK
234800020325     C                   MOVEL     §OGNTW        NTX(I)                         *NETWORK
234900040323     C* ELIMINATO NETWORK MES CON COR
235000040323     C                   IF        §OGNTW = 'MES'
235100040323     C                   MOVEL     'COR'         NTX(I)
235200040323     C                   END
235300020325     C* VERIFICA SE TRATTASI DI I/E
235700020325     C                   EXSR      CHKNTW
235800020325     C                   MOVEL     §NTWFIE       FL1(I)
236000020325     C*
236100000703E   3C                   ENDIF
236200000000E   2C                   ENDIF
236300000000     C                   READ      AZORG01L                               99
236400000000E   1C                   ENDDO
236500000000     C*
236600000000     C                   ENDSR
236700020325     C*------------------------------------------------------------------------*
236800020325     C* CHKNTW - CONTROLLA IL NETWORK
236900020325     C*------------------------------------------------------------------------*
237000020325     C     CHKNTW        BEGSR
237100020325     C*
237200020325     C* VERIFICA VALORE IN TABELLA
237300020325     C                   CLEAR                   DNTW
237400020325     C                   CLEAR                   TIBS02
237500020325     C                   MOVEL     'NTW'         T02COD
237600020325     C                   MOVEL     KNSIF         T02SIF
237700020325     C                   MOVEL     §OGNTW        T02KE1
237800020325     C                   MOVEL     'C'           T02MOD
237900020325     C                   CALL      'TIBS02R'
238000020325     C                   PARM                    KPJBA
238100020325     C                   PARM                    TIBS02
238200020325     C*
238300020325     C     T02ERR        IFEQ      *BLANKS
238400020325     C                   MOVEL     T02UNI        DNTW
238500020325     C                   ENDIF
238600020325     C*
238700020325     C                   ENDSR
238800000000     C*------------------------------------------------------------------------*
238900000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
239000000000     C*------------------------------------------------------------------------*
239100000000     C     STAPAR        BEGSR
239200000000     C*
239300000000     C* STAMPA INTESTAZIONE
239400020115     C                   WRITE     SE46PT
239500000000     C*
239600000000     C* DECODIFICHE PARAMETRI FILTRO
239700000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
239800000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
239900000000     C*
240000000000IF  1C     PARMPA        IFEQ      'P'
240100950529     C                   MOVEL     PAR(1)        PADMPA                         *PARTENZE
240200000000X   1C                   ELSE
240300950529     C                   MOVEL     PAR(2)        PADMPA                         *ARRIVI
240400000000E   1C                   ENDIF
240500000000     C*
240600000000IF  1C     PARDET        IFEQ      'S'
240700000000     C                   MOVEL     'SI'          PACDET                         *SI DETTAGLIO
240800000000X   1C                   ELSE
240900000000     C                   MOVEL     'NO'          PACDET                         *NO DETTAGLIO
241000000000E   1C                   ENDIF
241100000000     C*
241200000000IF  1C     PARFPC        IFEQ      'S'
241300000000     C                   MOVEL     'SI'          PACFPC                         *SI CREAZIONE FILE PC
241400000000X   1C                   ELSE
241500000000     C                   MOVEL     'NO'          PACFPC                         *NO CREAZIONE FILE PC
241600000000E   1C                   ENDIF
241700000703     C*
241800000703     C                   MOVEL     PARPOS        PACPOS                         *POSTE/NO POSTE
241900000703IF  1C     PARPOS        IFEQ      'S'
242000000703     C                   MOVEL     PAR(17)       PADPOS                         *POSTE
242100000703X   1C                   ELSE
242200000703     C                   MOVEL     PAR(18)       PADPOS                         *NO POSTE
242300000703E   1C                   ENDIF
242400000000     C*
242500000000IF  1C     PARFIL        IFGT      *ZEROS
242600000000     C                   Z-ADD     PARFIL        DEPFIL
242700000000     C                   EXSR      DECFIL
242800000000     C                   Z-ADD     *ZEROS        DEPFIL
242900000000     C                   MOVEL     PARFIL        PACFIL                         *FILIALE
243000000000     C                   MOVEL     DESFIL        PADFIL
243100000000X   1C                   ELSE
243200000000     C                   MOVEL     *BLANKS       PACFIL
243300000000E   1C                   ENDIF
243400000000     C*
243500000000IF  1C     PARARE        IFGT      *ZEROS
243600000000     C                   Z-ADD     PARARE        DEPARE
243700000000     C                   EXSR      DECARE
243800000000     C                   Z-ADD     *ZEROS        DEPARE
243900000000     C                   MOVEL     PARARE        PACARE                         *AREA
244000000000     C                   MOVEL     DESARE        PADARE
244100000000X   1C                   ELSE
244200000000     C                   MOVEL     *BLANKS       PACARE
244300000000E   1C                   ENDIF
244400000000     C*
244500000000IF  1C     PARDIV        IFNE      *BLANKS
244600000000     C                   MOVEL     PARDIV        DEPDIV
244700000000     C                   EXSR      DECDIV
244800000000     C                   MOVEL     *BLANKS       DEPDIV
244900000000     C                   MOVEL     PARDIV        PACDIV                         *DIVISIONE
245000000000     C                   MOVEL     DESDIV        PADDIV
245100000000E   1C                   ENDIF
245200000000     C*
245300090605      * decodifico tipo servizio richiesto
245400090605     c                   if        parser <> *blanks
245500090605     c                   clear                   ds5e
245600090605     c                   eval      ktbcod = '5E'
245700090605     c                   eval      ktbkey = parser
245800090605     c     keytab        chain     tabel00f
245900090605     c                   if        %found(tabel00f)
246000090605     c                   eval      ds5e = tbluni
246100090605     c                   endif
246200090605     c                   eval      padser = §5edes
246300090605     c                   else
246400090605     c                   eval      padser = 'TUTTI'
246500090605     c                   endif
246600000000     C*
246700000000IF  1C     PARPOR        IFEQ      'F'
246800000000     C                   MOVEL     PAR(6)        PADPOR                         *PORTO FRANCO
246900000000E   1C                   ENDIF
247000000000     C*
247100000000IF  1C     PARPOR        IFEQ      'A'
247200000000     C                   MOVEL     PAR(7)        PADPOR                         *PORTO ASSEGNATO
247300000000E   1C                   ENDIF
247400000000     C*
247500000000IF  1C     PARPOR        IFEQ      ' '
247600000000     C                   MOVEL     PAR(10)       PADPOR                         *TUTTI
247700000000E   1C                   ENDIF
247800000000     C*
247900000000IF  1C     PARSPE        IFEQ      'N'
248000950529     C                   MOVEL     PAR(15)       PADSPE                         *SPEDIZIONE NORMALE
248100000000E   1C                   ENDIF
248200000000     C*
248300000000IF  1C     PARSPE        IFEQ      ' '
248400000000     C                   MOVEL     PAR(10)       PADSPE                         *TUTTE
248500000000E   1C                   ENDIF
248600000000     C*
248700000000IF  1C     PARPRO        IFEQ      'B'
248800000000     C                   MOVEL     PAR(8)        PADPRO                         *PRODOTTO BUSTA
248900000000E   1C                   ENDIF
249000000000     C*
249100000000IF  1C     PARPRO        IFEQ      'P'
249200000000     C                   MOVEL     PAR(9)        PADPRO                         *PRODOTTO PARCEL
249300000000E   1C                   ENDIF
249400000000     C*
249500000000IF  1C     PARPRO        IFEQ      ' '
249600000000     C                   MOVEL     PAR(10)       PADPRO                         *TUTTI
249700000000E   1C                   ENDIF
249800950529     C*
249900950529     C* DATI IN STATISTICA
250000950529     C                   MOVEL     PARIOE        PACIOE
250100950529IF  1C     PARIOE        IFEQ      'I'
250200950529     C                   MOVEL     PAR(12)       PADIOE                         *DATI "ITALIA"
250300950529E   1C                   ENDIF
250400950529IF  1C     PARIOE        IFEQ      'M'
250500950529     C                   MOVEL     PAR(13)       PADIOE                         *DATI "IMPORT"
250600950529E   1C                   ENDIF
250700950529IF  1C     PARIOE        IFEQ      'X'
250800950529     C                   MOVEL     PAR(14)       PADIOE                         *DATI "EXPORT"
250900950529E   1C                   ENDIF
251000950529IF  1C     PARIOE        IFEQ      *BLANKS
251100950602     C                   MOVEL     PAR(16)       PADIOE                         *DATI "TUTTI"
251200950529E   1C                   ENDIF
251300000000     C*
251400000000     C* STAMPA PARAMETRI
251500020115     C                   WRITE     SE46PA
251600000000     C*
251700000000     C                   ENDSR
251800000000     C*------------------------------------------------------------------------*
251900000000     C*?CANFPC - CANCELLAZIONE FILE SALDI MOVIMENTO PER PC
252000000000     C*------------------------------------------------------------------------*
252100000000     C     CANFPC        BEGSR
252200950529     C*
252300950529     C* SE E' STATA RICHIESTA LA STATISTICA PER ITALIA, IMPORT O EXPORT CANCELLA
252400950529     C* SOLO I RECORD INTERESSATI ("I"/"M"/"X"/" ");
252500950529     C* SE E'STATA CHIESTA LA STATISTICA CON TUTTI I DATI,CANCELLA TUTTO IL FILE
252600950529IF  1C     PARIOE        IFEQ      *BLANKS
252700950529     C                   Z-ADD     45            LUNG             15 5          *LUNGHEZZA COMANDO
252800000000     C                   MOVEL     *BLANKS       COMMAN           80            *DESCRIZIONE COMANDO
252900950529     C***
253000950529     C* PARTENZA
253100950529     C***
253200950529IF  2C     PARMPA        IFEQ      'P'
253300000703     C                   MOVEA     CMD1(1)       COMMAN
253400000703     C                   CALL      'QCMDEXC'                                    *CLRPFM NEW FILE
253500000614     C                   PARM                    COMMAN
253600000614     C                   PARM                    LUNG
253700950529X   2C                   ELSE
253800950529     C***
253900950529     C* ARRIVO
254000950529     C***
254100000703     C                   MOVEA     CMD2(1)       COMMAN
254200000614     C                   CALL      'QCMDEXC'                                    *CLRPFM
254300000614     C                   PARM                    COMMAN
254400000614     C                   PARM                    LUNG
254500950529E   2C                   ENDIF
254600950529E   1C                   ENDIF
254700950529     C* APERTURA FILE
254800950529IF  1C     PARMPA        IFEQ      'P'
254900000703     C                   OPEN      SIPCP01L
255000100413     C                   OPEN      SIPCP11L
255100950529X   1C                   ELSE
255200000703     C                   OPEN      SIPCA01L
255300100413     C                   OPEN      SIPCA11L
255400950529E   1C                   ENDIF
255500950529     C***
255600950529     C* PARTENZA
255700950529     C***
255800000703IF  1C     PARIOE        IFNE      *BLANKS                                      *ITALIA/IMPORT/EXPORT
255900950529IF  2C     PARMPA        IFEQ      'P'
256000000703     C                   READ      SIPCP01L                               99
256100000703DO  3C     *IN99         DOWEQ     *OFF
256200000703IF  4C     PCPIOE        IFEQ      PARIOE
256300000703IF  5C     PARPOS        IFEQ      'S'                                          *DATI POSTE
256400020409     C     PCPNTP        ANDEQ     'PPT'                                         NW PART POSTE
256500000703     C     PARPOS        OREQ      'S'                                          *DATI POSTE
256600020409     C     PCPNTA        ANDEQ     'PPT'                                         NW ARR POSTE
256700000703     C     PARPOS        OREQ      'N'                                          *DATI NO POSTE
256800020409     C     PCPNTP        ANDNE     'PPT'                                         NW PART NO POSTE
256900020409     C     PCPNTA        ANDNE     'PPT'                                         NW ARR NO POSTE
257000000703     C                   DELETE    SIPCP000
257100000703E   5C                   ENDIF
257200000703E   4C                   ENDIF
257300000703     C                   READ      SIPCP01L                               99
257400000703E   3C                   ENDDO
257500100413     c
257600100413     C                   READ      SIPCP11L                               99
257700100413DO  3C     *IN99         DOWEQ     *OFF
257800100415IF  4C     n_PCPIOE      IFEQ      PARIOE
257900100413     c                   if        n_pcpaaa=annini or n_pcpaaa=annfin
258000100413     C                   DELETE    SIPCP010
258100100413E   4C                   ENDIF
258200100413E   4C                   ENDIF
258300100413     c
258400100415     C                   READ      SIPCP11L                               99
258500100413E   3C                   ENDDO
258600950529     C***
258700950529     C* ARRIVO
258800950529     C***
258900950529X   2C                   ELSE
259000000703     C                   READ      SIPCA01L                               99
259100000614DO  3C     *IN99         DOWEQ     *OFF
259200000614IF  4C     PCAIOE        IFEQ      PARIOE
259300000703IF  5C     PARPOS        IFEQ      'S'                                          *DATI POSTE
259400020409     C     PCANTP        ANDEQ     'PPT'                                         NW PART POSTE
259500000703     C     PARPOS        OREQ      'S'                                          *DATI POSTE
259600020409     C     PCANTA        ANDEQ     'PPT'                                         NW ARR POSTE
259700000703     C     PARPOS        OREQ      'N'                                          *DATI NO POSTE
259800020409     C     PCANTP        ANDNE     'PPT'                                         NW PART NO POSTE
259900020409     C     PCANTA        ANDNE     'PPT'                                         NW ARR NO POSTE
260000000703     C                   DELETE    SIPCA000
260100000703E   5C                   ENDIF
260200000703E   4C                   ENDIF
260300000703     C                   READ      SIPCA01L                               99
260400000614E   3C                   ENDDO
260500100413     c
260600100413     C                   READ      SIPCA11L                               99
260700100413DO  3C     *IN99         DOWEQ     *OFF
260800100415IF  4C     n_PCAIOE      IFEQ      PARIOE
260900100413     c                   if        n_pcaAaa=annini or n_pcaAaa=annfin
261000100413     c                   delete    sipca010
261100100413E   5C                   ENDIF
261200100413E   4C                   ENDIF
261300100413     c
261400100415     C                   READ      SIPCA11L                               99
261500100413E   3C                   ENDDO
261600100413     c
261700950529E   2C                   ENDIF
261800950529E   1C                   ENDIF
261900000000     C*
262000000000     C                   ENDSR
262100000703     C*------------------------------------------------------------------------*
262200000703     C*?SCRSIX - SCRIVE MOVIMENTO FILIALE SUL FILE PC
262300000703     C*------------------------------------------------------------------------*
262400000703     C     SCRSIX        BEGSR
262500000703     C*
262600000703     C* SCHIERE SALDI MENSILI DI FILIALE DETTAGLIO
262700000703IF  1C     PASDET        IFEQ      '1'                                          *MEMORIZZATO QC
262800000703DO  2C     1             DO        10            A
262900000703     C     A             OCCUR     FPDET
263000000703     C*---
263100000703     C* P A R T E N Z A
263200000703     C*---
263300000703IF  3C     PARMPA        IFEQ      'P'
263400000703     C                   Z-ADD     DEPFIL        KCPFLP                         *FILIALE PARTENZA
263500000703     C                   MOVEL     PARIOE        KCPIOE                         *DATI IN STATISTICA
263600000703     C                   MOVEL     A             KCPFPE                         *FASCIA DI PESO
263700000703     C                   MOVEL     DEPNTF        KCPNTP                         *NETWORK PARTENZA
263800000703     C                   MOVEL     DEPNTT        KCPNTA                         *NETWORK ARRIVO
263900000703     C     KEYPCP        CHAIN     SIPCP01L                           98
264000000703IF  4C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
264100000703     C                   CLEAR                   SIPCP000                       *AZZERA FORMATO RECORD
264200000703     C                   Z-ADD     KCPFLP        PCPFLP                         *CHIAVE
264300000703     C                   MOVEL     KCPIOE        PCPIOE
264400000703     C                   MOVEL     KCPFPE        PCPFPE
264500000703     C                   MOVEL     KCPNTP        PCPNTP
264600000703     C                   MOVEL     KCPNTA        PCPNTA
264700000703     C                   EXSR      ADDPCP                                       *AGGIUNGE DATI RECORD
264800000703     C                   WRITE     SIPCP000
264900000703X   4C                   ELSE                                                   M O D I F I C A
265000000703     C                   EXSR      ADDPCP                                       *AGGIUNGE DATI RECORD
265100000703     C                   UPDATE    SIPCP000
265200000703E   4C                   ENDIF
265300000703     C*---
265400000703     C* A R R I V O
265500000703     C*---
265600000703X   3C                   ELSE
265700000703     C                   Z-ADD     DEPFIL        KCAFLA                         *FILIALE ARRIVO
265800000703     C                   MOVEL     PARIOE        KCAIOE                         *DATI IN STATISTICA
265900000703     C                   MOVEL     A             KCAFPE                         *FASCIA DI PESO
266000000703     C                   MOVEL     DEPNTF        KCANTA                         *NETWORK ARRIVO
266100000703     C                   MOVEL     DEPNTT        KCANTP                         *NETWORK PARTENZA
266200000703     C     KEYPCA        CHAIN     SIPCA01L                           98
266300000703IF  4C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
266400000703     C                   CLEAR                   SIPCA000                       *AZZERA FORMATO RECORD
266500000703     C                   Z-ADD     KCAFLA        PCAFLA                         *CHIAVE
266600000703     C                   MOVEL     KCAIOE        PCAIOE
266700000703     C                   MOVEL     KCAFPE        PCAFPE
266800000703     C                   MOVEL     KCANTA        PCANTA
266900000703     C                   MOVEL     KCANTP        PCANTP
267000000703     C                   EXSR      ADDPCA                                       *AGGIUNGE DATI RECORD
267100000703     C                   WRITE     SIPCA000
267200000703X   4C                   ELSE                                                   M O D I F I C A
267300000703     C                   EXSR      ADDPCA                                       *AGGIUNGE DATI RECORD
267400000703     C                   UPDATE    SIPCA000
267500000703E   4C                   ENDIF
267600000703E   3C                   ENDIF
267700000703     C*
267800000703E   2C                   ENDDO
267900000703E   1C                   ENDIF
268000000703     C*
268100000703     C                   ENDSR
268200000703     C*------------------------------------------------------------------------*
268300000703     C*?ADDPCP - AGGIUNGE DATI AL RECORD DI SIPCP00F
268400000703     C*------------------------------------------------------------------------*
268500000703     C     ADDPCP        BEGSR
268600000703     C*
268700010227     C                   Z-ADD     *ZEROS        N11              11 3
268800000703     C*
268900000703     C                   ADD       SPT(1)        PCPNS1                         *SPEDIZIONI 1° MESE
269000000703     C                   ADD       SPT(2)        PCPNS2                         *SPEDIZIONI 2° MESE
269100000703     C                   ADD       SPT(3)        PCPNS3                         *SPEDIZIONI 3° MESE
269200000703     C                   ADD       SPT(4)        PCPNS4                         *SPEDIZIONI 4° MESE
269300000703     C                   ADD       SPT(5)        PCPNS5                         *SPEDIZIONI 5° MESE
269400000703     C                   ADD       SPT(6)        PCPNS6                         *SPEDIZIONI 6° MESE
269500000703     C                   ADD       SPT(7)        PCPNS7                         *SPEDIZIONI 7° MESE
269600000703     C                   ADD       SPT(8)        PCPNS8                         *SPEDIZIONI 8° MESE
269700000703     C                   ADD       SPT(9)        PCPNS9                         *SPEDIZIONI 9° MESE
269800000703     C                   ADD       SPT(10)       PCPNS0                         *SPEDIZIONI 10°MESE
269900000703     C                   ADD       SPT(11)       PCPNSA                         *SPEDIZIONI 11°MESE
270000000703     C                   ADD       SPT(12)       PCPNSB                         *SPEDIZIONI 12°MESE
270100000703     C*
270200010227     C     QTT(1)        DIV       100           N11
270300010227     C                   ADD(H)    N11           PCPPQ1                         *QUINTALI 1° MESE
270400010227     C     QTT(2)        DIV       100           N11
270500010227     C                   ADD(H)    N11           PCPPQ2                         *QUINTALI 2° MESE
270600010227     C     QTT(3)        DIV       100           N11
270700010227     C                   ADD(H)    N11           PCPPQ3                         *QUINTALI 3° MESE
270800010227     C     QTT(4)        DIV       100           N11
270900010227     C                   ADD(H)    N11           PCPPQ4                         *QUINTALI 4° MESE
271000010227     C     QTT(5)        DIV       100           N11
271100010227     C                   ADD(H)    N11           PCPPQ5                         *QUINTALI 5° MESE
271200010227     C     QTT(6)        DIV       100           N11
271300010227     C                   ADD(H)    N11           PCPPQ6                         *QUINTALI 6° MESE
271400010227     C     QTT(7)        DIV       100           N11
271500010227     C                   ADD(H)    N11           PCPPQ7                         *QUINTALI 7° MESE
271600010227     C     QTT(8)        DIV       100           N11
271700010227     C                   ADD(H)    N11           PCPPQ8                         *QUINTALI 8° MESE
271800010227     C     QTT(9)        DIV       100           N11
271900010227     C                   ADD(H)    N11           PCPPQ9                         *QUINTALI 9° MESE
272000010227     C     QTT(10)       DIV       100           N11
272100010227     C                   ADD(H)    N11           PCPPQ0                         *QUINTALI 10°MESE
272200010227     C     QTT(11)       DIV       100           N11
272300010227     C                   ADD(H)    N11           PCPPQA                         *QUINTALI 11°MESE
272400010227     C     QTT(12)       DIV       100           N11
272500010227     C                   ADD(H)    N11           PCPPQB                         *QUINTALI 12°MESE
272600000703     C*
272700000703     C                   ADD       CLT(1)        PCPNC1                         *COLLI 1° MESE
272800000703     C                   ADD       CLT(2)        PCPNC2                         *COLLI 2° MESE
272900000703     C                   ADD       CLT(3)        PCPNC3                         *COLLI 3° MESE
273000000703     C                   ADD       CLT(4)        PCPNC4                         *COLLI 4° MESE
273100000703     C                   ADD       CLT(5)        PCPNC5                         *COLLI 5° MESE
273200000703     C                   ADD       CLT(6)        PCPNC6                         *COLLI 6° MESE
273300000703     C                   ADD       CLT(7)        PCPNC7                         *COLLI 7° MESE
273400000703     C                   ADD       CLT(8)        PCPNC8                         *COLLI 8° MESE
273500000703     C                   ADD       CLT(9)        PCPNC9                         *COLLI 9° MESE
273600000703     C                   ADD       CLT(10)       PCPNC0                         *COLLI 10°MESE
273700000703     C                   ADD       CLT(11)       PCPNCA                         *COLLI 11°MESE
273800000703     C                   ADD       CLT(12)       PCPNCB                         *COLLI 12°MESE
273900000703     C*
274000000703     C                   ADD(H)    VLT(1)        PCPVO1                         *VOLUME 1° MESE
274100000703     C                   ADD(H)    VLT(2)        PCPVO2                         *VOLUME 2° MESE
274200000703     C                   ADD(H)    VLT(3)        PCPVO3                         *VOLUME 3° MESE
274300000703     C                   ADD(H)    VLT(4)        PCPVO4                         *VOLUME 4° MESE
274400000703     C                   ADD(H)    VLT(5)        PCPVO5                         *VOLUME 5° MESE
274500000703     C                   ADD(H)    VLT(6)        PCPVO6                         *VOLUME 6° MESE
274600000703     C                   ADD(H)    VLT(7)        PCPVO7                         *VOLUME 7° MESE
274700000703     C                   ADD(H)    VLT(8)        PCPVO8                         *VOLUME 8° MESE
274800000703     C                   ADD(H)    VLT(9)        PCPVO9                         *VOLUME 9° MESE
274900000703     C                   ADD(H)    VLT(10)       PCPVO0                         *VOLUME 10°MESE
275000000703     C                   ADD(H)    VLT(11)       PCPVOA                         *VOLUME 11°MESE
275100000703     C                   ADD(H)    VLT(12)       PCPVOB                         *VOLUME 12°MESE
275200000703     C*
275300000703     C                   ENDSR
275400000703     C*------------------------------------------------------------------------*
275500000703     C*?ADDPCA - AGGIUNGE DATI AL RECORD DI SIPCA00F
275600000703     C*------------------------------------------------------------------------*
275700000703     C     ADDPCA        BEGSR
275800010227     C*
275900010227     C                   Z-ADD     *ZEROS        N11              11 3
276000000703     C*
276100000703     C                   ADD       SPT(1)        PCANS1                         *SPEDIZIONI 1° MESE
276200000703     C                   ADD       SPT(2)        PCANS2                         *SPEDIZIONI 2° MESE
276300000703     C                   ADD       SPT(3)        PCANS3                         *SPEDIZIONI 3° MESE
276400000703     C                   ADD       SPT(4)        PCANS4                         *SPEDIZIONI 4° MESE
276500000703     C                   ADD       SPT(5)        PCANS5                         *SPEDIZIONI 5° MESE
276600000703     C                   ADD       SPT(6)        PCANS6                         *SPEDIZIONI 6° MESE
276700000703     C                   ADD       SPT(7)        PCANS7                         *SPEDIZIONI 7° MESE
276800000703     C                   ADD       SPT(8)        PCANS8                         *SPEDIZIONI 8° MESE
276900000703     C                   ADD       SPT(9)        PCANS9                         *SPEDIZIONI 9° MESE
277000000703     C                   ADD       SPT(10)       PCANS0                         *SPEDIZIONI 10°MESE
277100000703     C                   ADD       SPT(11)       PCANSA                         *SPEDIZIONI 11°MESE
277200000703     C                   ADD       SPT(12)       PCANSB                         *SPEDIZIONI 12°MESE
277300000703     C*
277400010227     C     QTT(1)        DIV       100           N11
277500010227     C                   ADD(H)    N11           PCAPQ1                         *QUINTALI 1° MESE
277600010227     C     QTT(2)        DIV       100           N11
277700010227     C                   ADD(H)    N11           PCAPQ2                         *QUINTALI 2° MESE
277800010227     C     QTT(3)        DIV       100           N11
277900010227     C                   ADD(H)    N11           PCAPQ3                         *QUINTALI 3° MESE
278000010227     C     QTT(4)        DIV       100           N11
278100010227     C                   ADD(H)    N11           PCAPQ4                         *QUINTALI 4° MESE
278200010227     C     QTT(5)        DIV       100           N11
278300010227     C                   ADD(H)    N11           PCAPQ5                         *QUINTALI 5° MESE
278400010227     C     QTT(6)        DIV       100           N11
278500010227     C                   ADD(H)    N11           PCAPQ6                         *QUINTALI 6° MESE
278600010227     C     QTT(7)        DIV       100           N11
278700010227     C                   ADD(H)    N11           PCAPQ7                         *QUINTALI 7° MESE
278800010227     C     QTT(8)        DIV       100           N11
278900010227     C                   ADD(H)    N11           PCAPQ8                         *QUINTALI 8° MESE
279000010227     C     QTT(9)        DIV       100           N11
279100010227     C                   ADD(H)    N11           PCAPQ9                         *QUINTALI 9° MESE
279200010227     C     QTT(10)       DIV       100           N11
279300010227     C                   ADD(H)    N11           PCAPQ0                         *QUINTALI 10°MESE
279400010227     C     QTT(11)       DIV       100           N11
279500010227     C                   ADD(H)    N11           PCAPQA                         *QUINTALI 11°MESE
279600010227     C     QTT(12)       DIV       100           N11
279700010227     C                   ADD(H)    N11           PCAPQB                         *QUINTALI 12°MESE
279800000703     C*
279900000703     C                   ADD       CLT(1)        PCANC1                         *COLLI 1° MESE
280000000703     C                   ADD       CLT(2)        PCANC2                         *COLLI 2° MESE
280100000703     C                   ADD       CLT(3)        PCANC3                         *COLLI 3° MESE
280200000703     C                   ADD       CLT(4)        PCANC4                         *COLLI 4° MESE
280300000703     C                   ADD       CLT(5)        PCANC5                         *COLLI 5° MESE
280400000703     C                   ADD       CLT(6)        PCANC6                         *COLLI 6° MESE
280500000703     C                   ADD       CLT(7)        PCANC7                         *COLLI 7° MESE
280600000703     C                   ADD       CLT(8)        PCANC8                         *COLLI 8° MESE
280700000703     C                   ADD       CLT(9)        PCANC9                         *COLLI 9° MESE
280800000703     C                   ADD       CLT(10)       PCANC0                         *COLLI 10°MESE
280900000703     C                   ADD       CLT(11)       PCANCA                         *COLLI 11°MESE
281000000703     C                   ADD       CLT(12)       PCANCB                         *COLLI 12°MESE
281100000703     C*
281200000703     C                   ADD(H)    VLT(1)        PCAVO1                         *VOLUME 1° MESE
281300000703     C                   ADD(H)    VLT(2)        PCAVO2                         *VOLUME 2° MESE
281400000703     C                   ADD(H)    VLT(3)        PCAVO3                         *VOLUME 3° MESE
281500000703     C                   ADD(H)    VLT(4)        PCAVO4                         *VOLUME 4° MESE
281600000703     C                   ADD(H)    VLT(5)        PCAVO5                         *VOLUME 5° MESE
281700000703     C                   ADD(H)    VLT(6)        PCAVO6                         *VOLUME 6° MESE
281800000703     C                   ADD(H)    VLT(7)        PCAVO7                         *VOLUME 7° MESE
281900000703     C                   ADD(H)    VLT(8)        PCAVO8                         *VOLUME 8° MESE
282000000703     C                   ADD(H)    VLT(9)        PCAVO9                         *VOLUME 9° MESE
282100000703     C                   ADD(H)    VLT(10)       PCAVO0                         *VOLUME 10°MESE
282200000703     C                   ADD(H)    VLT(11)       PCAVOA                         *VOLUME 11°MESE
282300000703     C                   ADD(H)    VLT(12)       PCAVOB                         *VOLUME 12°MESE
282400000703     C*
282500000703     C                   ENDSR
282600000000     C*------------------------------------------------------------------------*
282700000000     C*?CARTAB - CARICA I DATI IN TABELLE
282800000000     C*------------------------------------------------------------------------*
282900000000     C     CARTAB        BEGSR
283000000000     C*-------------------------
283100000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
283200000000     C*-------------------------
283300020906     C                   Z-ADD     1             KTBUTE
283400000000     C                   MOVEL     '2W'          KTBCOD
283500000000     C     KEYTA2        SETLL     TABEL00F
283600000000     C     KEYTA2        READE     TABEL00F                               99
283700000000     C*
283800000000     C                   Z-ADD     *ZEROS        G                 4 0
283900000000DO  1C     *IN99         DOWEQ     *OFF
284000000000IF  2C     TBLFLG        IFEQ      *BLANKS
284100000000     C                   ADD       1             G
284200000000     C                   MOVEL     TBLKEY        FLAB(G)
284300000000     C                   MOVEL     TBLUNI        DS2W
284400000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
284500000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
284600000000E   2C                   ENDIF
284700000000     C*
284800000000     C     KEYTA2        READE     TABEL00F                               99
284900000000E   1C                   ENDDO
285000000000     C*
285100000000     C                   ENDSR
285200950712     C*------------------------------------------------------------------------*
285300950712     C*?AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
285400950712     C*          OUTPUT--> MES  : MESI DA ELABORARE
285500950712     C*                    ANN  : ANNI DEI MESI DA ELABORARE
285600950712     C*                    PARJ : NUMERO DEI MESI DA ELABORARE
285700950712     C*------------------------------------------------------------------------*
285800950712     C     AMELAB        BEGSR
285900950712     C*
286000950712     C                   CLEAR                   ANN                            *ANNI
286100950712     C                   CLEAR                   MES                            *MESI
286200950712     C                   Z-ADD     *ZEROS        PARJ              2 0          *N° MESI DA ELABORARE
286300950712     C*
286400950712     C* ANNO INIZIALE UGUALE AD ANNO FINALE
286500950712IF  1C     WAA2          IFEQ      WAA
286600950712     C*
286700950712     C* CALCOLO I MESI DI DIFFERENZA
286800950712DO  2C     WMM           DOWLE     WMM2
286900950712     C                   ADD       1             JI
287000950712     C                   Z-ADD     WAA           ANN(JI)
287100950712     C                   Z-ADD     WMM           MES(JI)
287200950712     C                   ADD       1             WMM
287300950712E   2C                   ENDDO
287400950712     C*
287500950712     C                   Z-ADD     JI            PARJ                           *PARAMETRO
287600950712     C*
287700950712     C* IL RESTO DELLA SCHIERA A ZERO
287800950712DO  2C     JI            DOWLT     12
287900950712     C                   ADD       1             JI
288000950712     C                   Z-ADD     *ZEROS        ANN(JI)
288100950712     C                   Z-ADD     *ZEROS        MES(JI)
288200950712E   2C                   ENDDO
288300950712     C*
288400950712     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
288500950712X   1C                   ELSE
288600950712     C*
288700950712     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
288800950712DO  2C     WMM           DOWLE     12
288900950712     C                   ADD       1             JI
289000950712     C                   Z-ADD     WAA           ANN(JI)
289100950712     C                   Z-ADD     WMM           MES(JI)
289200950712     C                   ADD       1             WMM
289300950712E   2C                   ENDDO
289400950712     C*
289500950712     C* MEMORIZZO I MESI DELL'ANNO FINALE
289600950712     C                   Z-ADD     01            WMM
289700950712DO  2C     WMM           DOWLE     WMM2
289800950712     C                   ADD       1             JI
289900950712     C                   Z-ADD     WAA2          ANN(JI)
290000950712     C                   Z-ADD     WMM           MES(JI)
290100950712     C                   ADD       1             WMM
290200950712E   2C                   ENDDO
290300950712     C*
290400950712     C                   Z-ADD     JI            PARJ                           *PARAMETRO
290500950712     C*
290600950712     C* IL RESTO DELLA SCHIERA A ZERO
290700950712DO  2C     JI            DOWLT     12
290800950712     C                   ADD       1             JI
290900950712     C                   Z-ADD     *ZEROS        ANN(JI)
291000950712     C                   Z-ADD     *ZEROS        MES(JI)
291100950712E   2C                   ENDDO
291200950712E   1C                   ENDIF
291300950712     C*
291400950712     C                   ENDSR
291500000000     C*------------------------------------------------------------------------*
291600000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
291700000000     C*------------------------------------------------------------------------*
291800000000     C     *INZSR        BEGSR
291900950529     C*--------------------
292000950529     C* RICEVIMENTO PARAMETRI
292100950529     C*--------------------
292200000000     C     *ENTRY        PLIST
292300000000     C                   PARM                    KPJBA
292400000000     C                   MOVEL     KPJBU         PARAM
292500950529     C*--------------------
292600950529     C* DEFINIZIONE VARIABILI RIFERITE AL DATA BASE
292700950529     C*--------------------
292800000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
292900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
293000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
293100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
293200000000     C     *LIKE         DEFINE    ORGFL3        KORDIV
293300971204     C     *LIKE         DEFINE    SDRANN        KSDANN
293400000000     C     *LIKE         DEFINE    SDRMES        KSDMES
293500000000     C     *LIKE         DEFINE    ORGFIL        KSDFIL
293600000000     C     *LIKE         DEFINE    ORGFIL        KSDFIT
293700000000     C     *LIKE         DEFINE    TBGFIL        KBDFIL
293800000000     C     *LIKE         DEFINE    TBGANN        KBDANN
293900000000     C     *LIKE         DEFINE    TBGMES        KBDMES
294000960108     C     *LIKE         DEFINE    TBGIOE        KBDIOE
294100000703     C     *LIKE         DEFINE    PCAFLA        KCAFLA                         *SIPCA00F
294200000703     C     *LIKE         DEFINE    PCAIOE        KCAIOE
294300000703     C     *LIKE         DEFINE    PCAFPE        KCAFPE
294400000703     C     *LIKE         DEFINE    PCANTP        KCANTP
294500000703     C     *LIKE         DEFINE    PCANTA        KCANTA
294600000703     C     *LIKE         DEFINE    PCPFLP        KCPFLP                         *SIPCA00F
294700000703     C     *LIKE         DEFINE    PCPIOE        KCPIOE
294800000703     C     *LIKE         DEFINE    PCPFPE        KCPFPE
294900000703     C     *LIKE         DEFINE    PCPNTA        KCPNTA
295000000703     C     *LIKE         DEFINE    PCPNTP        KCPNTP
295100971204     C     *LIKE         DEFINE    ORGFL3        DEPDIV                         *DEPOSITI
295200971204     C     *LIKE         DEFINE    ORGCAR        DEPARE
295300971204     C     *LIKE         DEFINE    ORGFIL        DEPFIL
295400971204     C     *LIKE         DEFINE    ORGFL1        DLPFL1
295500971204     C     *LIKE         DEFINE    ORGFL1        DLAFL1
295600000703     C     *LIKE         DEFINE    PCANTP        DEPNTF
295700000703     C     *LIKE         DEFINE    PCANTP        DEPNTT
295800950529     C     *LIKE         DEFINE    TBGNSP        LAVSPE                         *DI LAVORO
295900000000     C     *LIKE         DEFINE    TBGKGP        LAVCHI
296000000000     C     *LIKE         DEFINE    TBGNCP        LAVCOL
296100000000     C     *LIKE         DEFINE    TBGVOP        LAVVOL
296200000703     C     *LIKE         DEFINE    PCANTP        LAVNTX
296300950529     C*--------------------
296400950529     C* CHIAVI DI LETTURA
296500950529     C*--------------------
296600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
296700000000     C     KEYTAB        KLIST
296800000000     C                   KFLD                    KTBUTE
296900000000     C                   KFLD                    KTBCOD
297000000000     C                   KFLD                    KTBKEY
297100000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
297200000000     C     KEYTA2        KLIST
297300000000     C                   KFLD                    KTBUTE
297400000000     C                   KFLD                    KTBCOD
297500000000     C* CHIAVE LETTURA AZORG02L - PARZIALE
297600000000     C     KEYORG        KLIST
297700000000     C                   KFLD                    KORDIV                         *DIVISIONE
297800000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
297900000000     C     KEYOR2        KLIST
298000000000     C                   KFLD                    KORFIL                         *FILIALE
298100990609     C* CHIAVE LETTURA SISDR01L/SISDR02L - TOTALE
298200000000     C     KEYSDR        KLIST
298300000000     C                   KFLD                    KSDANN                         *ANNO
298400000000     C                   KFLD                    KSDFIL                         *FILIALE
298500000000     C                   KFLD                    KSDFIT                         *FILIALE DETTAGLIO
298600000000     C                   KFLD                    KSDMES                         *MESE
298700990609     C* CHIAVE LETTURA SISDR01L/SISDR02L - PARZIALE
298800000000     C     KEYSD2        KLIST
298900000000     C                   KFLD                    KSDANN                         *ANNO
299000000000     C                   KFLD                    KSDFIL                         *FILIALE
299100000000     C                   KFLD                    KSDFIT                         *FILIALE DETTAGLIO
299200020114     C* CHIAVE LETTURA TEBDG01L - PARZIALE
299300000000     C     KEYBDG        KLIST
299400000000     C                   KFLD                    KBDFIL                         *FILIALE
299500000000     C                   KFLD                    KBDANN                         *ANNO
299600000000     C                   KFLD                    KBDMES                         *MESE
299700960108     C                   KFLD                    KBDIOE                         *DATI BUDGET
299800000703     C* CHIAVE LETTURA SIPCA01L - COMPLETA
299900100413     C     KEYPCA1       KLIST
300000100413     C                   KFLD                    Ksdann                         *anno
300100000703     C                   KFLD                    KCAFLA                         *FILIALE ARRIVO
300200000703     C                   KFLD                    KCAIOE                         *DATI STATISTICA
300300100413     C                   KFLD                    KCAtsp                         *tipo servizio
300400000703     C                   KFLD                    KCAFPE                         *FASCIA DI PESO
300500000703     C                   KFLD                    KCANTP                         *NETWORK PARTENZA
300600000703     C                   KFLD                    KCANTA                         *NETWORK ARRIVO
300700100413     C     KEYPCA        KLIST
300800100413     C                   KFLD                    KCAFLA                         *FILIALE ARRIVO
300900100413     C                   KFLD                    KCAIOE                         *DATI STATISTICA
301000100413     C                   KFLD                    KCAFPE                         *FASCIA DI PESO
301100100413     C                   KFLD                    KCANTP                         *NETWORK PARTENZA
301200100413     C                   KFLD                    KCANTA                         *NETWORK ARRIVO
301300000703     C* CHIAVE LETTURA SIPCP01L - COMPLETA
301400000703     C     KEYPCP        KLIST
301500000703     C                   KFLD                    KCPFLP                         *FILIALE PARTENZA
301600000703     C                   KFLD                    KCPIOE                         *DATI STATISTICA
301700000703     C                   KFLD                    KCPFPE                         *FASCIA DI PESO
301800000703     C                   KFLD                    KCPNTA                         *NETWORK ARRIVO
301900000703     C                   KFLD                    KCPNTP                         *NETWORK PARTENZA
302000100413     C     KEYPCP1       KLIST
302100100413     C                   KFLD                    Ksdann                         *anno
302200100413     C                   KFLD                    KCPFLP                         *FILIALE PARTENZA
302300100413     C                   KFLD                    KCPIOE                         *DATI STATISTICA
302400100413     C                   KFLD                    KCPtsp                         *tipo servizio
302500100413     C                   KFLD                    KCPFPE                         *FASCIA DI PESO
302600100413     C                   KFLD                    KCPNTA                         *NETWORK ARRIVO
302700100413     C                   KFLD                    KCPNTP                         *NETWORK PARTENZA
302800950529     C*--------------------
302900950529     C* VARIABILI DI CONTROLLO
303000950529     C*--------------------
303100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
303200000000     C                   MOVEL     '0'           $FIN1             1            *FINE SALDI ANNO CORR
303300000000     C                   MOVEL     '0'           $FIN2             1            *FINE SALDI ANNO PREC
303400000000     C                   MOVEL     '0'           $FIN3             1            *FINE VALORI BUDGET
303500000000     C                   MOVEL     '0'           PASDET            1            *MEMOR SALDI FIL DET
303600000000     C                   MOVEL     '0'           PASFIL            1            *MEMOR SALDI FILIALE
303700000000     C                   MOVEL     '0'           PASARE            1            *MEMOR SALDI AREA
303800000000     C                   MOVEL     '0'           PASDIV            1            *MEMOR SALDI DIVISION
303900000000     C                   MOVEL     '0'           PASGEN            1            *MEMOR SALDI GENERALI
304000000000     C                   MOVEL     '0'           NOBDG             1            *'1'-NO BDG; '0' SI
304100000000     C                   MOVEL     ' '           TIPANN            1            TIPO ANNI  (U-UGUALI)
304200950529     C                   Z-ADD     UDATE         DATA              6 0          *ATA
304300950529     C                   TIME                    ORA               6 0          *RA
304400000000     C*
304500000000     C* MEMORIZZA I CODICI DELLE FILIALI E LE LORO DECODIFICHE
304600000000     C                   EXSR      CARORG
304700000000     C*
304800000000     C* CANCELLA E APRE IL FILE PC
304900000000IF  1C     PARFPC        IFEQ      'S'
305000000000     C                   EXSR      CANFPC
305100000000E   1C                   ENDIF
305200950908     C*
305300950908     C* CALCOLA I MESI DA ELABORARE
305400990527     C                   Z-ADD     ANNFIN        WAA2              4 0          *ANNO FINALE LAVORO
305500990527     C                   Z-ADD     MESFIN        WMM2              2 0          *MESE FINALE LAVORO
305600990527     C                   Z-ADD     ANNINI        WAA               4 0          *ANNO INIZIALE LAVORO
305700990527     C                   Z-ADD     MESINI        WMM               2 0          *MESE INIZIALE LAVORO
305800990527     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORARE
305900990527     C                   EXSR      AMELAB
306000000000     C*
306100000000     C* DECODIFICA INTESTAZIONE
306200000000     C                   EXSR      DECTES
306300000000     C*
306400000000     C* CARICA TABELLE
306500000000     C                   EXSR      CARTAB
306600000000     C*
306700000000     C* RIEMPO SCHIERA CON ANNO INIZIALE E ANNO FINALE PER LETTURA SALDI CORR.
306800000000     C                   Z-ADD     ANNINI        AIF(1)                         *ANNO INIZIALE
306900000000     C                   Z-ADD     ANNFIN        AIF(2)                         *ANNO FINALE
307000000000     C*
307100000000     C* E SCHIERA CON ANNO INIZIALE E ANNO FINALE - UNO, PER LETTURA SALDI PREC.
307200000000     C     ANNINI        SUB       1             PIF(1)                         *ANNO INIZIALE - 1
307300000000     C     ANNFIN        SUB       1             PIF(2)                         *ANNO FINALE - 1
307400000000     C*
307500000000     C* SE AA INIZIALE =  AA FINALE, QUEST'ULTIMO IN SCHIERA LO IMPOSTO A ZERO
307600000000IF  1C     ANNINI        IFEQ      ANNFIN
307700000000     C                   Z-ADD     *ZEROS        AIF(2)                         *ANNO FINALE
307800000000     C                   Z-ADD     *ZEROS        PIF(2)                         *ANNO FINALE - 1
307900000000     C                   MOVEL     'U'           TIPANN                         *ANNI UGUALI
308000000000E   1C                   ENDIF
308100000000     C*
308200000000     C* A P E R T U R A   F I L E
308300000000     C*
308400000000IF  1C     PARMPA        IFEQ      'P'
308500990609     C                   OPEN      SISDR01L                                     *SALDI PARTENZE
308600000000X   1C                   ELSE
308700990609     C                   OPEN      SISDR02L                                     *SALDI ARRIVI
308800000000E   1C                   ENDIF
308900000000     C*
309000000000     C                   ENDSR
309100000000     O*------------------------------------------------------------------------*
309200000703**   PAR - DECODIFICHE PARAMETRI
309300950602PARTENZA       1
309400950602ARRIVO         2
309500090605..Libero       3
309600090605..Libero       4
309700090605..Libero       5
309800950602FRANCO         6
309900950602ASSEGNATO      7
310000950602BUSTE          8
310100950602PARCEL         9
310200090605TUTTI          10
310300950602TUTTE          11
310400950602ITALIA         12
310500950602IMPORT         13
310600950602EXPORT         14
310700950602NORMALE        15
310800950712COMPLETA       16
310900000703POSTE          17
311000000703NO POSTE       18
31110095060219
31120095060220
311300000000**   DMM - DECODIFICHE MESI DELL'ANNO
311400000000GENNAIO                        1
311500000000FEBBRAIO                       2
311600000000MARZO                          3
311700000000APRILE                         4
311800000000MAGGIO                         5
311900000000GIUGNO                         6
312000000000LUGLIO                         7
312100000000AGOSTO                         8
312200000000SETTEMBRE                      9
312300000000OTTOBRE                        10
312400000000NOVEMBRE                       11
312500000000DICEMBRE                       12
312600000000**   DRI - DECODIFICHE RIGA DI STAMPA
312700000000 N.SPEDIZIONI                  1
312800000000   PESO (QLI)                  2
312900000000     N. COLLI                  3
313000000000  VOLUME (MC)                  4
313100120227         %A.P.                 5
313200000000                               6
313300000000 TOTALI                        7
313400000000KG PER SPEDIZ.                 8
313500000000COLLI PER SPED                 9
313600000000MC PER SPEDIZ.                 10
313700000703**   CMD1
313800000703CLRPFM FILE(*LIBL/SIPCP00F)
313900000703**   CMD2
314000000703CLRPFM FILE(*LIBL/SIPCA00F)
