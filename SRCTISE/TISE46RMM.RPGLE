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
002000160401     F*ISE46P   O    E             PRINTER
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
005800000000     D* DESCRIZIONE COMANDI 'QCMDEXC'
005900000000     D*------------------
006000000000     D CMD1            S             45    DIM(1) CTDATA PERRCD(1)
006100000000     D CMD2            S             45    DIM(1) CTDATA PERRCD(1)
006200000000     D*------------------------------------------------------------------------*
006300000000     D* INPUT                                                                  *
006400000000     D*------------------------------------------------------------------------*
006500000000     D KPJBA         E DS
006600000000     D  PROFIL                 1     10                                         UTENTE
006700000000     D*------------------
006800000000     D* NUMERO DEI BYTE IN PACKED:
006900000000     D*------------------
007000000000     D* SPEDIZIONI  (11:2) + 1 =  6,5  /  (6 X 12) = 72
007100000000     D* KILOGRAMMI  (13:2) + 1 =  7,5  /  (7 X 12) = 84
007200000000     D* COLLI       (11:2) + 1 =  6,5  /  (6 X 12) = 72
007300000000     D* VOLUME      (13:2) + 1 =  7,5  /  (7 X 12) = 84
007400000000     D*------------------
007500020610     D* DS MULTIPLA FASCE DI PESO PER TOTALI GENERALI     - 10 FASCE X 12 MESI -
007600000000     D*------------------
007700020610     D FPGEN           DS                  OCCURS(10) INZ
007800000000     D  SPG                    1     72P 0                                      SPE*12
007900000000     D                                     DIM(12)                              SPEDIZIONI
008000000000     D  SBG                   73    144P 0                                      %BG*12
008100000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
008200000000     D  SAG                  145    216P 0                                      %AP*12
008300000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
008400000000     D  QTG                  217    300P 1                                      QUI*12
008500000000     D                                     DIM(12)                              KILOGRAMMI
008600000000     D  QBG                  301    384P 1                                      %BG*12
008700000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
008800000000     D  QAG                  385    468P 1                                      %AP*12
008900000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
009000000000     D  CLG                  469    540P 0                                      COL*12
009100000000     D                                     DIM(12)                              COLLI
009200000000     D  CAG                  541    612P 0                                      %AP*12
009300000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
009400000000     D  VLG                  613    696P 3                                      VOL*12
009500000000     D                                     DIM(12)                              VOLUME
009600000000     D  VAG                  697    780P 3                                      %AP*12
009700000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
009800000000     D  CBG                  781    852P 0                                      %BD*12
009900000000     D                                     DIM(12)                              COLLI BUDGET
010000000000     D  VBG                  853    936P 3                                      %BD*12
010100000000     D                                     DIM(12)                              VOLUME BUDGET
010200000000     D*------------------
010300020610     D* DS MULTIPLA FASCE DI PESO PER DIVISIONE           - 10 FASCE X 12 MESI -
010400000000     D*------------------
010500020610     D FPDIV           DS                  OCCURS(10) INZ
010600000000     D  SPD                    1     72P 0                                      SPE*12
010700000000     D                                     DIM(12)                              SPEDIZIONI
010800000000     D  SBD                   73    144P 0                                      %BG*12
010900000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
011000000000     D  SAD                  145    216P 0                                      %AP*12
011100000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
011200000000     D  QTD                  217    300P 1                                      QUI*12
011300000000     D                                     DIM(12)                              KILOGRAMMI
011400000000     D  QBD                  301    384P 1                                      %BG*12
011500000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
011600000000     D  QAD                  385    468P 1                                      %AP*12
011700000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
011800000000     D  CLD                  469    540P 0                                      COL*12
011900000000     D                                     DIM(12)                              COLLI
012000000000     D  CAD                  541    612P 0                                      %AP*12
012100000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
012200000000     D  VLD                  613    696P 3                                      VOL*12
012300000000     D                                     DIM(12)                              VOLUME
012400000000     D  VAD                  697    780P 3                                      %AP*12
012500000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
012600000000     D  CBD                  781    852P 0                                      %BD*12
012700000000     D                                     DIM(12)                              COLLI BUDGET
012800000000     D  VBD                  853    936P 3                                      %BD*12
012900000000     D                                     DIM(12)                              VOLUME BUDGET
013000000000     D*------------------
013100020610     D* DS MULTIPLA FASCE DI PESO PER AREA                - 10 FASCE X 12 MESI -
013200000000     D*------------------
013300020610     D FPARE           DS                  OCCURS(10) INZ
013400000000     D  SPA                    1     72P 0                                      SPE*12
013500000000     D                                     DIM(12)                              SPEDIZIONI
013600000000     D  SBA                   73    144P 0                                      %BG*12
013700000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
013800000000     D  SAA                  145    216P 0                                      %AP*12
013900000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
014000000000     D  QTA                  217    300P 1                                      QUI*12
014100000000     D                                     DIM(12)                              KILOGRAMMI
014200000000     D  QBA                  301    384P 1                                      %BG*12
014300000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
014400000000     D  QAA                  385    468P 1                                      %AP*12
014500000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
014600000000     D  CLA                  469    540P 0                                      COL*12
014700000000     D                                     DIM(12)                              COLLI
014800000000     D  CAA                  541    612P 0                                      %AP*12
014900000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
015000000000     D  VLA                  613    696P 3                                      VOL*12
015100000000     D                                     DIM(12)                              VOLUME
015200000000     D  VAA                  697    780P 3                                      %AP*12
015300000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
015400000000     D  CBA                  781    852P 0                                      %BD*12
015500000000     D                                     DIM(12)                              COLLI BUDGET
015600000000     D  VBA                  853    936P 3                                      %BD*12
015700000000     D                                     DIM(12)                              VOLUME BUDGET
015800000000     D*------------------
015900020610     D* DS MULTIPLA FASCE DI PESO PER FILIALE             - 10 FASCE X 12 MESI -
016000000000     D*------------------
016100020610     D FPFIL           DS                  OCCURS(10) INZ
016200000000     D  SPF                    1     72P 0                                      SPE*12
016300000000     D                                     DIM(12)                              SPEDIZIONI
016400000000     D  SBF                   73    144P 0                                      %BG*12
016500000000     D                                     DIM(12)                              SPEDIZIONI BUDGET
016600000000     D  SAF                  145    216P 0                                      %AP*12
016700000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
016800000000     D  QTF                  217    300P 1                                      QUI*12
016900000000     D                                     DIM(12)                              KILOGRAMMI
017000000000     D  QBF                  301    384P 1                                      %BG*12
017100000000     D                                     DIM(12)                              KILOGRAMMI BUDGET
017200000000     D  QAF                  385    468P 1                                      %AP*12
017300000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
017400000000     D  CLF                  469    540P 0                                      COL*12
017500000000     D                                     DIM(12)                              COLLI
017600000000     D  CAF                  541    612P 0                                      %AP*12
017700000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
017800000000     D  VLF                  613    696P 3                                      VOL*12
017900000000     D                                     DIM(12)                              VOLUME
018000000000     D  VAF                  697    780P 3                                      %AP*12
018100000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
018200000000     D  CBF                  781    852P 0                                      %BD*12
018300000000     D                                     DIM(12)                              COLLI BUDGET
018400000000     D  VBF                  853    936P 3                                      %BD*12
018500000000     D                                     DIM(12)                              VOLUME BUDGET
018600000000     D*------------------
018700020610     D* DS MULTIPLA FASCE DI PESO PER FILIALE DETTAGLIO   - 10 FASCE X 12 MESI -
018800000000     D*------------------
018900020610     D FPDET           DS                  OCCURS(10) INZ
019000000000     D  SPT                    1     72P 0                                      SPE*12
019100000000     D                                     DIM(12)                              SPEDIZIONI
019200000000     D  SAT                   73    144P 0                                      %AP*12
019300000000     D                                     DIM(12)                              SPEDIZIONI ANNO PREC.
019400000000     D  QTT                  145    228P 1                                      QUI*12
019500000000     D                                     DIM(12)                              KILOGRAMMI
019600000000     D  QAT                  229    312P 1                                      %AP*12
019700000000     D                                     DIM(12)                              KILOGRAMMI ANNO PREC.
019800000000     D  CLT                  313    384P 0                                      COL*12
019900000000     D                                     DIM(12)                              COLLI
020000000000     D  CAT                  385    456P 0                                      %AP*12
020100000000     D                                     DIM(12)                              COLLI ANNO PRECEDENTE
020200000000     D  VLT                  457    540P 3                                      VOL*12
020300000000     D                                     DIM(12)                              VOLUME
020400000000     D  VAT                  541    624P 3                                      %AP*12
020500000000     D                                     DIM(12)                              VOLUME ANNO PRECEDENTE
020600000000     D*------------------
020700000000     D* PARAMETRI FILTRO
020800000000     D*------------------
020900990527     D PARAM           DS
021000990527     D  PARDAT                 1      6  0
021100990527     D  ANNINI                 1      4  0
021200990527     D  MESINI                 5      6  0
021300990527     D  PARDA2                 7     12  0
021400990527     D  ANNFIN                 7     10  0
021500990527     D  MESFIN                11     12  0
021600990527     D  PARMPA                13     13
021700990527     D  PARFIL                14     16  0
021800990527     D  PARARE                17     19  0
021900990527     D  PARDIV                20     20
022000990527     D  PARSER                21     21
022100990527     D  PARPOR                22     22
022200990527     D  PARSPE                23     23
022300990527     D  PARPRO                24     24
022400990527     D  PARDET                25     25
022500990527     D  PARFPC                26     26
022600990527     D  PARIOE                27     27
022700000703     D  PARPOS                28     28
022800990527     D*------------------
022900020114     D* DS COMPOSIZIONE DATA(6) - TEBDG
023000990527     D*------------------
023100990527     D                 DS
023200990527     D  TBGANN                 1      4  0
023300990527     D  TBGMES                 5      6  0
023400990527     D  TBGDAT                 1      6  0
023500000000     D*------------------
023600000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
023700000000     D*------------------
023800000000     D DS2W          E DS
023900000216     D*---
024000000703     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA
024100000216     D*---
024200000216     D OG143         E DS
024300020325     D*-------------------
024400020325     D* DS DEFINIZIONE TABELLA "NETWORK"
024500020325     D*-------------------
024600020325     D DNTW          E DS
024700020325     D*-------------------
024800020325     D* DS RICHIAMO SCELTA TABELLE
024900020325     D*-------------------
025000020325     D TIBS02        E DS                  EXTNAME(TIBS02DS)
025100090605
025200090605     d ds5e          e ds
025300100413     d
025400100413     d kcptsp          s                   like(n_pcptsp)
025500100413     d kcatsp          s                   like(n_pcatsp)
025600100413     C*------------------------------------------------------------------------*
025700950529     C*?MAIN LINE
025800000000     C*------------------------------------------------------------------------*
025900000000     C*
026000000000     C* POSIZIONAMENTO SUL FILE ORGANIGRAMMI E PRIMA LETTURA
026100000000     C                   EXSR      SETFIL
026200000000     C*
026300000000     C* CICLO FINO A FINE FILE ORGANIGRAMMI
026400000000DO  1C     $FINE         DOWEQ     '0'
026500000000     C*
026600000000     C* OPERAZIONI PER NUOVA DIVISIONE
026700000000     C                   EXSR      NEWDIV
026800000000     C*
026900000000     C* CICLO FINO A ROTTURA DI DIVISIONE
027000000000DO  2C     $FINE         DOWEQ     '0'
027100000000     C     ORGFL3        ANDEQ     DEPDIV
027200000000     C*
027300000000     C* OPERAZIONI PER NUOVA AREA
027400000000     C                   EXSR      NEWARE
027500000000     C*
027600000000     C* CICLO FINO A ROTTURA DI AREA
027700000000DO  3C     $FINE         DOWEQ     '0'
027800000000     C     ORGFL3        ANDEQ     DEPDIV
027900000000     C     ORGCAR        ANDEQ     DEPARE
028000000000     C*
028100000000     C* OPERAZIONI PER NUOVA FILIALE
028200000000     C                   EXSR      NEWFIL
028300000000     C*
028400000000     C* CICLO LETTURA FILIALI DETTAGLIO
028500971124DO  4C     1             DO        500           F                 3 0
028600000000IF  5C     FIL(F)        IFGT      *ZEROS
028700000000     C*
028800000000     C* OPERAZIONI PER NUOVA FILIALE DETTAGLIO
028900000000     C                   EXSR      NEWDET
029000000703     C*
029100020409 xxx C* CONTROLLA VALIDITA' DEL DETTAGLIO
029200000703IF  6C     PARPOS        IFEQ      'S'                                          *DATI POSTE
029300020409     C     DEPNTF        ANDEQ     'PPT'                                         FILIALE POSTE
029400000703     C     PARPOS        OREQ      'S'                                          *DATI POSTE
029500020409     C     DEPNTT        ANDEQ     'PPT'                                         FILIALE DETTAGLIO POSTE
029600000703     C     PARPOS        OREQ      'N'                                          *NO DATI POSTE
029700020409     C     DEPNTF        ANDNE     'PPT'                                         NO FILIALE POSTE
029800020409     C     DEPNTT        ANDNE     'PPT'                                         NO FILIALE DETTAGLIO POSTE
029900000000     C*
030000000000     C* LETTURA SALDI AA INIZIALE, E AA FINALE SOLO SE DIVERSO DALL'INIZIALE
030100000000     C                   Z-ADD     1             J                 1 0
030200000703DO  7C                   DO        2             J
030300000000     C                   MOVEL     '0'           $FIN1                          *REIMPOSTO FINE FILE
030400000000     C*
030500000000     C* SECONDO GIRO SOLO SE ANNO FINALE DIVERSO DALL' INIZIALE (PIENO)
030600000703IF  8C     AIF(J)        IFNE      *ZEROS
030700000000     C*
030800000000     C* POSIZIONAMENTO SUL FILE SALDI ANNO CORRENTE E PRIMA LETTURA
030900000000     C                   EXSR      SETFI1
031000000000     C*
031100000000     C* CICLO FINO A FINE FILE SALDI O ROTTURA FILIALE
031200000703DO  9C     $FIN1         DOWEQ     '0'
031300000000     C*
031400000000     C* MEMORIZZA I SALDI CORRENTI DELLA FILIALE LETTA DALL'ORGANIGRAMMA
031500000000     C                   EXSR      MEMSAL
031600000000     C*
031700000000     C* LETTURA RECORD SALDI SUCCESSIVO
031800000000     C                   EXSR      LETFI1
031900000000     C*
032000000703E   9C                   ENDDO
032100000000     C*
032200000703E   8C                   ENDIF
032300000703E   7C                   ENDDO
032400000000     C*
032500000000     C* CALCOLA I SALDI DELL' ANNO PRECEDENTE DELLA FILIALE DETTAGLIO
032600000000     C                   EXSR      CALANP
032700000704     C*
032800000704     C* SCRIVE I SALDI MOVIMENTI DELLA FILIALE SUL FILE PC -NEW-
032900000704IF  7C     PARFPC        IFEQ      'S'
033000000704     C                   EXSR      SCRSIX                                       *SCRIVE FILE PC -NEW-
033100000704E   7C                   ENDIF
033200000000     C*
033300000703E   6C                   ENDIF
033400000000     C*
033500000000E   5C                   ENDIF
033600000000E   4C                   ENDDO                                                  *FIL/DET SUCESSIVA
033700000000     C*
033800000000     C* CALCOLA I VALORI DEL BUDGET
033900000000IF  4C     PASFIL        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
034000000000     C                   EXSR      CALBDG
034100000000E   4C                   ENDIF
034200000000     C*
034300000000     C* LETTURA RECORD ORGANIGRAMMI SUCCESSIVO
034400000000     C                   EXSR      LETFIL
034500000000     C*
034600000000E   3C                   ENDDO
034700000000     C*
034800000000E   2C                   ENDDO
034900000000E   1C                   ENDDO
035000000000     C* C H I U S U R A   F I L E
035100000000     C*
035200000000IF  1C     PARMPA        IFEQ      'P'
035300990609     C                   CLOSE     SISDR01L                                     *SALDI PARTENZE
035400000000IF  2C     PARFPC        IFEQ      'S'
035500000703     C                   CLOSE     SIPCP01L                                     *SALDI PARTENZE PC NEW
035600120820     C                   CLOSE     SIPCP11L                                     *SALDI PARTENZE PC NEW
035700000000E   2C                   ENDIF
035800000000     C*
035900000000X   1C                   ELSE
036000990609     C                   CLOSE     SISDR02L                                     *SALDI ARRIVI
036100000000IF  2C     PARFPC        IFEQ      'S'
036200000703     C                   CLOSE     SIPCA01L                                     *SALDI ARRIVI PC NEW
036300120820     C                   CLOSE     SIPCA11L                                     *SALDI ARRIVI PC NEW
036400000000E   2C                   ENDIF
036500000000     C*
036600000000E   1C                   ENDIF
036700000000     C*
036800000000     C                   SETON                                        LR
036900000000     C*------------------------------------------------------------------------*
037000000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ORGANIGRAMMI
037100000000     C*------------------------------------------------------------------------*
037200000000     C     SETFIL        BEGSR
037300000000     C*
037400000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE ORGANIGRAMMI
037500000000     C                   MOVEL     PARDIV        KORDIV
037600000000     C     KEYORG        SETLL     AZORG02L                           99        *FINE ARCHIVIO
037700000000     C*
037800000000IF  1C     *IN99         IFEQ      '1'
037900000000     C                   MOVEL     '1'           $FINE
038000000000X   1C                   ELSE
038100000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
038200000000E   1C                   ENDIF
038300000000     C*
038400000000     C                   ENDSR
038500000000     C*------------------------------------------------------------------------*
038600000000     C*?LETFIL - LETTURA PROSSIMO RECORD FILE ORGANIGRAMMI
038700000000     C*------------------------------------------------------------------------*
038800000000     C     LETFIL        BEGSR
038900000000     C*
039000000000     C* LEGGO FINO A:
039100000000     C*     - FINE FILE
039200000000     C*     - TROVATO RECORD VALIDO
039300000000     C                   MOVEL     'N'           WRECOK
039400000000DO  1C     $FINE         DOWEQ     '0'
039500000000     C     WRECOK        ANDEQ     'N'
039600000000     C*
039700000000     C* LETTURE SUCCESSIVE ALLA PRIMA
039800000000     C* SE SELEZIONATA LA DIVISIONE LEGGO ANCHE PER UGUALE
039900000000IF  2C     PARDIV        IFGT      *BLANKS
040000000000     C     KEYORG        READE     AZORG02L                               99
040100000000X   2C                   ELSE
040200000000     C                   READ      AZORG02L                               99
040300000000E   2C                   ENDIF
040400000703     C*
040500000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
040600000000     C                   MOVEL     '1'           $FINE
040700000000X   2C                   ELSE
040800000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
040900000000E   2C                   ENDIF
041000000000     C*
041100000000E   1C                   ENDDO
041200000000     C*
041300000000     C                   ENDSR
041400000000     C*------------------------------------------------------------------------*
041500000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD ORGANIGRAMMA
041600000000     C*------------------------------------------------------------------------*
041700000000     C     CHKREC        BEGSR
041800000000     C*
041900000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
042000000703     C*
042100000703     C* IMPOSTAZIONI
042200000703     C                   CLEAR                   OG143
042300000703     C                   MOVEL     ORGDE3        OG143
042400000000     C*
042500000000     C* ESCLUDO LE FILIALI ANNULLATE
042600000000IF  1C     ORGFVA        IFNE      *BLANKS
042700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042800000000E   1C                   ENDIF
042900000000     C*
043000000000     C* ESCLUDO LE VOCI VARIE
043100000000IF  1C     ORGFAG        IFEQ      'V'
043200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043300000000E   1C                   ENDIF
043400000000     C*
043500000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
043600000000IF  1C     PARARE        IFNE      *ZEROS
043700000000     C     ORGCAR        ANDNE     PARARE
043800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043900000000E   1C                   ENDIF
044000000000     C*
044100000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
044200000000IF  1C     PARFIL        IFNE      *ZEROS
044300000000     C     ORGFIL        ANDNE     PARFIL
044400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
044500000000E   1C                   ENDIF
044600000000     C*
044700000000     C                   ENDSR
044800000000     C*------------------------------------------------------------------------*
044900000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
045000000000     C*------------------------------------------------------------------------*
045100000000     C     NEWDIV        BEGSR
045200000000     C*
045300000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
045400000000     C                   MOVEL     ORGFL3        DEPDIV
045500000000     C*
045600000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
045700000000     C                   EXSR      DECDIV
045800000000     C*
045900000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
046000000000     C                   MOVEL     '0'           PASDIV
046100000000     C*
046200000000     C* AZZERA I TOTALI DELLA DIVISIONE
046300000000DO  1C                   DO        10            A                 2 0
046400000000     C     A             OCCUR     FPDIV
046500000000     C                   CLEAR                   SPD
046600000000     C                   CLEAR                   SBD
046700000000     C                   CLEAR                   SAD
046800000000     C                   CLEAR                   QTD
046900000000     C                   CLEAR                   QBD
047000000000     C                   CLEAR                   QAD
047100000000     C                   CLEAR                   CLD
047200000000     C                   CLEAR                   CAD
047300000000     C                   CLEAR                   VLD
047400000000     C                   CLEAR                   VAD
047500000000     C                   CLEAR                   CBD
047600000000     C                   CLEAR                   VBD
047700000000E   1C                   ENDDO
047800000000     C*
047900000000     C                   ENDSR
048000000000     C*------------------------------------------------------------------------*
048100000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
048200000000     C*------------------------------------------------------------------------*
048300000000     C     NEWARE        BEGSR
048400000000     C*
048500000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
048600000000     C                   MOVEL     ORGCAR        DEPARE
048700000000     C*
048800000000     C* DECODIFICA IL CODICE AREA
048900000000     C                   EXSR      DECARE
049000000000     C*
049100000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
049200000000     C                   MOVEL     '0'           PASARE
049300000000     C*
049400000000     C* AZZERA I TOTALI DELL'AREA
049500000000DO  1C                   DO        10            A
049600000000     C     A             OCCUR     FPARE
049700000000     C                   CLEAR                   SPA
049800000000     C                   CLEAR                   SBA
049900000000     C                   CLEAR                   SAA
050000000000     C                   CLEAR                   QTA
050100000000     C                   CLEAR                   QBA
050200000000     C                   CLEAR                   QAA
050300000000     C                   CLEAR                   CLA
050400000000     C                   CLEAR                   CAA
050500000000     C                   CLEAR                   VLA
050600000000     C                   CLEAR                   VAA
050700000000     C                   CLEAR                   CBA
050800000000     C                   CLEAR                   VBA
050900000000E   1C                   ENDDO
051000000000     C*
051100000000     C                   ENDSR
051200000000     C*------------------------------------------------------------------------*
051300000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA DEI SALDI ANNO CORRENTE
051400000000     C*------------------------------------------------------------------------*
051500000000     C     SETFI1        BEGSR
051600000000     C*
051700000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE SALDI
051800000000     C                   Z-ADD     AIF(J)        KSDANN                         *AA INIZIALE O FINALE
051900000000     C                   Z-ADD     ORGFIL        KSDFIL                         *FILIALE
052000000000     C                   Z-ADD     FIL(F)        KSDFIT                         *FILIALE DETTAGLIO
052100000000     C*
052200000000     C* MI POSIZIONO CON IL MESE INIZIALE SE ANNI UGUALI O PRIMO ANNO DIVERSO,
052300000000     C* OPPURE CON IL MESE = 01 SE GLI ANNI SONO DIVERSI E STO ELABORANDO IL 2°
052400000000IF  1C     TIPANN        IFEQ      'U'
052500000000     C     J             OREQ      1
052600000000     C                   Z-ADD     MESINI        KSDMES                         *MESE INIZIALE
052700000000E   1C                   ELSE
052800000000     C                   Z-ADD     01            KSDMES                         *MESE 01
052900000000E   1C                   ENDIF
053000000000     C* PARTENZE
053100000000IF  1C     PARMPA        IFEQ      'P'
053200990609     C     KEYSDR        SETLL     SISDR01L                           99        *FINE ARCHIVIO
053300000000X   1C                   ELSE
053400000000     C* ARRIVI
053500990609     C     KEYSDR        SETLL     SISDR02L                           99        *FINE ARCHIVIO
053600000000E   1C                   ENDIF
053700000000     C*
053800000000IF  1C     *IN99         IFEQ      '1'
053900000000     C                   MOVEL     '1'           $FIN1
054000000000X   1C                   ELSE
054100000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
054200000000E   1C                   ENDIF
054300000000     C*
054400000000     C                   ENDSR
054500000000     C*------------------------------------------------------------------------*
054600000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI ANNO CORRENTE
054700000000     C*------------------------------------------------------------------------*
054800000000     C     LETFI1        BEGSR
054900000000     C*
055000000000     C* LEGGO FINO A:
055100000000     C*     - FINE FILE
055200000000     C*     - TROVATO RECORD VALIDO
055300000000     C                   MOVEL     'N'           WRECOK
055400000000DO  1C     $FIN1         DOWEQ     '0'
055500000000     C     WRECOK        ANDEQ     'N'
055600000000     C*
055700000000     C* LETTURE SUCCESSIVE ALLA PRIMA (ANNO + FILIALE + DETTAGLIO)
055800000000IF  1C     PARMPA        IFEQ      'P'
055900990609     C     KEYSD2        READE     SISDR01L                               99
056000000000X   1C                   ELSE
056100990609     C     KEYSD2        READE     SISDR02L                               99
056200000000E   1C                   ENDIF
056300000000     C*
056400000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
056500000000     C                   MOVEL     '1'           $FIN1
056600000000X   2C                   ELSE
056700000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
056800000000E   2C                   ENDIF
056900000000     C*
057000000000E   1C                   ENDDO
057100000000     C*
057200000000     C                   ENDSR
057300000000     C*------------------------------------------------------------------------*
057400000000     C*?CHKRE1 - CONTROLLA VALIDITA' DEI SALDI ANNO CORRENTE/PRECEDENTE
057500000000     C*------------------------------------------------------------------------*
057600000000     C     CHKRE1        BEGSR
057700000000     C*
057800000000     C* °ANNO INIZIALE = ANNO FINALE: IL MESE LETTO DEVE ESSERE MINORE/UGUALE
057900000000     C*  DEL MESE FINALE RICHIESTO A VIDEO
058000000000     C*  ANNO INIZIALE <> ANNO FINALE: IL MESE DELL'ANNO INIZIALE DEVE ESSERE
058100000000     C*  MIN/UG DI 12; IL MESE DELL'ANNO FINALE MIN/UG DEL MESE FINALE RICHIESTO
058200000000     C*
058300000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
058400000000     C*
058500000000     C* CONTROLLO MESE DEL RECORD SALDI LETTO°
058600000000IF  1C     TIPANN        IFEQ      'U'                                          *ANNI UGUALI
058700000000     C     J             OREQ      2                                            *ANNI DIVERSI MA 2°
058800000000     C*
058900000000IF  2C     SDRMES        IFGT      MESFIN
059000000000     C                   MOVEL     '1'           $FIN1                          *ESCO DAL CICLO CORR.
059100000000     C                   MOVEL     '1'           $FIN2                          *ESCO DAL CICLO PREC.
059200000000E   2C                   ENDIF
059300000000E   1C                   ENDIF
059400000000     C*
059500000000     C* SE TIPO SERVIZIO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
059600000000IF  1C     PARSER        IFNE      *BLANKS
059700000000IF  2C     PARSER        IFNE      SDRTSP
059800000000     C                   MOVEL     'N'           WRECOK
059900000000E   2C                   ENDIF
060000000000E   1C                   ENDIF
060100000000     C*
060200000000     C* SE TIPO PORTO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
060300000000IF  1C     PARPOR        IFNE      *BLANKS
060400000000IF  2C     SDRTPO        IFNE      PARPOR
060500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
060600000000E   2C                   ENDIF
060700000000E   1C                   ENDIF
060800000000     C*
060900000000     C* ESCLUDO LE BOLLE PER CUI NON VALGONO LE SPEDIZIONI
061000000000     C                   Z-ADD     1             G
061100000000     C                   SETOFF                                           99
061200000000     C     SDRRBL        LOOKUP    FLAB(G)                                99
061300000000IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
061400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
061500000000X   1C                   ELSE
061600000000IF  2C     FSP(G)        IFEQ      'N'                                          BOLLA CON SPED.VALIDE
061700000000     C                   MOVEL     'N'           WRECOK
061800000000E   2C                   ENDIF
061900000000E   1C                   ENDIF
062000000000     C*
062100000000     C* SE TIPO SPEDIZIONE DIVERSO DA QUELLO RICHIESTO, ESCLUDO
062200000000IF  1C*          PARSPE    IFNE *BLANKS
062300000000IF  2C*          SDRRBL    IFNE PARSPE
062400000000     C*                    MOVEL'N'       WRECOK           *RECORD NON VALIDO
062500000000E   2C*                    ENDIF
062600000000E   1C*                    ENDIF
062700000000     C*
062800000000     C* SE TIPO PRODOTTO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
062900000000IF  1C     PARPRO        IFNE      *BLANKS
063000000000IF  2C     SDRTPR        IFNE      PARPRO
063100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
063200000000E   2C                   ENDIF
063300000000E   1C                   ENDIF
063400971204     C*
063500971204     C* REPERISCE IL FLAG ITALIA / ESTERO
063600971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
063700971204     C                   MOVEL     *BLANKS       DLPFL1
063800971204     C                   MOVEL     *BLANKS       DLAFL1
063900971204     C                   Z-ADD     1             I
064000971204     C     SDRFLP        LOOKUP    FIL(I)                                 99
064100971204IF  2C     *IN99         IFEQ      *ON                                          *TROVATO
064200971204     C                   MOVEL     FL1(I)        DLPFL1
064300971204E   2C                   ENDIF
064400971204     C                   Z-ADD     1             I
064500971204     C     SDRFLA        LOOKUP    FIL(I)                                 99
064600971204IF  2C     *IN99         IFEQ      *ON                                          *TROVATO
064700971204     C                   MOVEL     FL1(I)        DLAFL1
064800971204E   2C                   ENDIF
064900971204E   1C                   ENDIF
065000000218     C*
065100000614     C* SE RICHIESTA STATISTICA ITALIA, ESCLUDE BOLLE ESTERO (ITA->ITA)
065200971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
065300971204IF  2C     PARIOE        IFEQ      'I'
065400971204IF  3C     DLPFL1        IFNE      *BLANKS
065500971204     C     DLAFL1        ORNE      *BLANKS
065600950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
065700971204E   3C                   ENDIF
065800971204E   2C                   ENDIF
065900971204E   1C                   ENDIF
066000000614     C*
066100950529     C* SE RICHIESTA STATISTICA IMPORT, ESCLUDE BOLLE CON LNP ITALIA (EST->ITA)
066200971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
066300971204IF  2C     PARIOE        IFEQ      'M'
066400000218IF  3C     DLPFL1        IFNE      *BLANKS
066500000218     C     DLAFL1        ANDEQ     *BLANKS
066600000218     C                   ELSE
066700950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
066800971204E   3C                   ENDIF
066900971204E   2C                   ENDIF
067000971204E   1C                   ENDIF
067100950529     C*
067200950529     C* SE RICHIESTA STATISTICA EXPORT, ESCLUDE BOLLE CON LNA ITALIA (ITA->EST)
067300971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
067400971204IF  2C     PARIOE        IFEQ      'X'
067500000228IF  3C     DLAFL1        IFEQ      *BLANKS
067600950529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
067700971204E   3C                   ENDIF
067800971204E   2C                   ENDIF
067900971204E   1C                   ENDIF
068000950529     C*
068100000000     C                   ENDSR
068200000000     C*------------------------------------------------------------------------*
068300000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
068400000000     C*------------------------------------------------------------------------*
068500000000     C     NEWFIL        BEGSR
068600000000     C*
068700000000     C* REIMPOSTO FINE FILE SALDI CORRENTI/PRECEDENTI/BUDGET
068800000000     C                   MOVEL     '0'           $FIN1                          FINE SALDI CORRENTI
068900000000     C                   MOVEL     '0'           $FIN2                          FINE SALDI PRECEDENTI
069000000000     C                   MOVEL     '0'           $FIN3                          FINE VALORI BUDGET
069100000000     C*
069200000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI E BUDGET
069300000000     C                   MOVEL     '0'           PASFIL                         *SALDI
069400000000     C                   MOVEL     '0'           NOBDG                          *BUDGET
069500000000     C*
069600000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
069700000000     C                   Z-ADD     ORGFIL        DEPFIL
069800000000     C*
069900000000     C* DECODIFICA IL CODICE FILIALE
070000000703     C                   MOVEL     *BLANKS       LAVNTX
070100000000     C                   EXSR      DECFIL
070200000703     C*
070300000703     C* MEMORIZZA IL NETWORK DELLA NUOVA FILIALE
070400000703     C                   MOVEL     LAVNTX        DEPNTF                         *NETWORK
070500000000     C*
070600000000     C* AZZERA I TOTALI DELLA FILIALE
070700000000DO  1C                   DO        10            A
070800000000     C     A             OCCUR     FPFIL
070900000000     C                   CLEAR                   SPF
071000000000     C                   CLEAR                   SBF
071100000000     C                   CLEAR                   SAF
071200000000     C                   CLEAR                   QTF
071300000000     C                   CLEAR                   QBF
071400000000     C                   CLEAR                   QAF
071500000000     C                   CLEAR                   CLF
071600000000     C                   CLEAR                   CAF
071700000000     C                   CLEAR                   VLF
071800000000     C                   CLEAR                   VAF
071900000000     C                   CLEAR                   CBF
072000000000     C                   CLEAR                   VBF
072100000000E   1C                   ENDDO
072200000000     C*
072300000000     C                   ENDSR
072400000000     C*------------------------------------------------------------------------*
072500000000     C*?NEWDET - OPERAZIONI PER NUOVA FILIALE DETTAGLIO
072600000000     C*------------------------------------------------------------------------*
072700000000     C     NEWDET        BEGSR
072800000000     C*
072900000000     C* REIMPOSTO FINE FILE SALDI CORRENTI/PRECEDENTI
073000000000     C                   MOVEL     '0'           $FIN1                          FINE SALDI CORRENTI
073100000000     C                   MOVEL     '0'           $FIN2                          FINE SALDI PRECEDENTI
073200000000     C*
073300000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI FILIALE DETTAGLIO
073400000000     C                   MOVEL     '0'           PASDET
073500000703     C*
073600000703     C* MEMORIZZA IL NETWORK DELLA NUOVA FILIALE DETTAGLIO
073700000703     C                   MOVEL     NTX(F)        DEPNTT                         *NETWORK
073800000000     C*
073900000000     C* AZZERA I TOTALI DELLA FILIALE
074000000000DO  1C                   DO        10            A
074100000000     C     A             OCCUR     FPDET
074200000000     C                   CLEAR                   SPT
074300000000     C                   CLEAR                   SAT
074400000000     C                   CLEAR                   QTT
074500000000     C                   CLEAR                   QAT
074600000000     C                   CLEAR                   CLT
074700000000     C                   CLEAR                   CAT
074800000000     C                   CLEAR                   VLT
074900000000     C                   CLEAR                   VAT
075000000000E   1C                   ENDDO
075100000000     C*
075200000000     C                   ENDSR
075300000000     C*------------------------------------------------------------------------*
075400000000     C*?MEMSAL - MEMORIZZA I SALDI CORRENTI
075500000000     C*------------------------------------------------------------------------*
075600000000     C     MEMSAL        BEGSR
075700000000     C*
075800000000     C                   MOVEL     SDRFPE        FASCIA            2 0
075900000000     C                   Z-ADD     1             MM                2 0
076000000000     C*
076100000000     C* RICERCA DELLA POSIZIONE IN SCHIERA DELL'ANNO/MESE CHE STO ELABORANDO
076200000000     C                   SETOFF                                           28
076300000000     C     SDRMES        LOOKUP    MES(MM)                                28
076400000000     C*
076500000000     C* SE MESE TROVATO, ADDIZIONO
076600000000IF  1C     *IN28         IFEQ      *ON
076700000000     C*
076800000000     C* FASCIA DI PESO PER FILIALE DETTAGLIO (SOLO SE RICHIESTO)
076900000000     C     FASCIA        OCCUR     FPDET
077000950529     C                   MOVEL     '1'           PASDET                         *PASSATO MEMORIZZAZ.
077100000000     C                   ADD       SDRNSP        SPT(MM)                        *SPEDIZIONI
077200000000     C                   ADD       SDRPKG        QTT(MM)                        *KILOGRAMMI
077300000000     C                   ADD       SDRNCO        CLT(MM)                        *COLLI
077400000000     C                   ADD       SDRVOL        VLT(MM)                        *VOLUME IN MC
077500000000     C*
077600000000     C* FASCIA DI PESO PER FILIALE
077700000000     C     FASCIA        OCCUR     FPFIL
077800950529     C                   MOVEL     '1'           PASFIL                         *PASSATO MEMORIZZAZ.
077900950529     C                   MOVEL     '1'           PASARE
078000950529     C                   MOVEL     '1'           PASDIV
078100950529     C                   MOVEL     '1'           PASGEN
078200000000     C                   ADD       SDRNSP        SPF(MM)                        *SPEDIZIONI
078300000000     C                   ADD       SDRPKG        QTF(MM)                        *KILOGRAMMI
078400000000     C                   ADD       SDRNCO        CLF(MM)                        *COLLI
078500000000     C                   ADD       SDRVOL        VLF(MM)                        *VOLUME IN MC
078600000000     C*
078700000000     C* FASCIA DI PESO PER AREA
078800000000     C     FASCIA        OCCUR     FPARE
078900000000     C                   ADD       SDRNSP        SPA(MM)
079000000000     C                   ADD       SDRPKG        QTA(MM)
079100000000     C                   ADD       SDRNCO        CLA(MM)
079200000000     C                   ADD       SDRVOL        VLA(MM)
079300000000     C*
079400000000     C* FASCIA DI PESO PER DIVISIONE
079500000000     C     FASCIA        OCCUR     FPDIV
079600000000     C                   ADD       SDRNSP        SPD(MM)
079700000000     C                   ADD       SDRPKG        QTD(MM)
079800000000     C                   ADD       SDRNCO        CLD(MM)
079900000000     C                   ADD       SDRVOL        VLD(MM)
080000000000     C*
080100000000     C* FASCIA DI PESO PER TOTALI GENERALI
080200000000     C     FASCIA        OCCUR     FPGEN
080300000000     C                   ADD       SDRNSP        SPG(MM)
080400000000     C                   ADD       SDRPKG        QTG(MM)
080500000000     C                   ADD       SDRNCO        CLG(MM)
080600000000     C                   ADD       SDRVOL        VLG(MM)
080700100413     c
080800100413     c* Memorizzo nel file sudddiviso per tipo servizio
080900120820     c                   if        PARFPC='S'
081000100413     c                   exsr      Mempc10
081100120820     c                   endif
081200120820     c
081300000000E   1C                   ENDIF
081400000000     C*
081500000000     C                   ENDSR
081600100413     C*------------------------------------------------------------------------*
081700100413     C*?Memorizzo nel SIPCP10 o SIPCPA10 suddiviso per tipo servizio
081800100413     C*------------------------------------------------------------------------*
081900100413     c     Mempc10       BEGSR
082000100413     C*---
082100100413     C* P A R T E N Z A
082200100413     C*---
082300100413IF  3C     PARMPA        IFEQ      'P'
082400100413     C                   Z-ADD     DEPFIL        KCPFLP                         *FILIALE PARTENZA
082500100413     C                   MOVEL     PARIOE        KCPIOE                         *DATI IN STATISTICA
082600100413     C                   MOVEL     fascia        KCPFPE                         *FASCIA DI PESO
082700100413     C                   MOVEL     sdrtsp        KCPtsp                         *FASCIA DI PESO
082800100413     C                   MOVEL     DEPNTF        KCPNTP                         *NETWORK PARTENZA
082900100413     C                   MOVEL     DEPNTT        KCPNTA                         *NETWORK ARRIVO
083000100413     C     KEYPCP1       CHAIN     SIPCP11L                           98
083100100413IF  4C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
083200100413     C                   CLEAR                   SIPCP010
083300100413     C                   Z-ADD     Ksdann        n_PCPaaa
083400100413     C                   Z-ADD     KCPFLP        n_PCPFLP
083500100413     C                   MOVEL     KCPIOE        n_PCPIOE
083600100413     C                   MOVEL     KCPtsp        n_PCPtsp
083700100413     C                   MOVEL     KCPFPE        n_PCPFPE
083800100413     C                   MOVEL     KCPNTP        n_PCPNTP
083900100413     C                   MOVEL     KCPNTA        n_PCPNTA
084000100413     c                   endif
084100100413     c* Addiziono campi
084200100413     c                   select
084300100413     c
084400100413     c                   when      sdrmes=1
084500100413     C                   ADD       SDRNsp        N_pcpns1
084600100413     C     sdrpkg        DIV       100           N11
084700100413     C                   ADD(H)    N11           n_PCPPQ1                         *QUINTALI 1° MESE
084800100413     C                   ADD       sdrnco        N_PCPNC1                         *COLLI 1° MESE
084900100413     C                   ADD(H)    sdrvol        N_PCPVO1                         *VOLUME 1° MESE
085000100413     c
085100100413     c                   when      sdrmes=2
085200100413     C                   ADD       SDRNsp        n_pcpns2
085300100413     C     sdrpkg        DIV       100           N11
085400100413     C                   ADD(H)    N11           n_PCPPQ2
085500100413     C                   ADD       sdrnco        n_PCPNC2
085600100413     C                   ADD(H)    sdrvol        n_PCPVO2
085700100413     c
085800100413     c                   when      sdrmes=3
085900100413     C                   ADD       SDRNsp        n_pcpns3
086000100413     C     sdrpkg        DIV       100           N11
086100100413     C                   ADD(H)    N11           n_PCPPQ3
086200100413     C                   ADD       sdrnco        n_PCPNC3
086300100413     C                   ADD(H)    sdrvol        n_PCPVO3
086400100413     c
086500100413     c                   when      sdrmes=4
086600100413     C                   ADD       SDRNsp        n_pcpns4
086700100413     C     sdrpkg        DIV       100           N11
086800100413     C                   ADD(H)    N11           n_PCPPQ4
086900100413     C                   ADD       sdrnco        n_PCPNC4
087000100413     C                   ADD(H)    sdrvol        n_PCPVO4
087100100413     c
087200100413     c                   when      sdrmes=5
087300100413     C                   ADD       SDRNsp        n_pcpns5
087400100413     C     sdrpkg        DIV       100           N11
087500100413     C                   ADD(H)    N11           n_PCPPQ5
087600100413     C                   ADD       sdrnco        n_PCPNC5
087700100413     C                   ADD(H)    sdrvol        n_PCPVO5
087800100413     c
087900100413     c                   when      sdrmes=6
088000100413     C                   ADD       SDRNsp        n_pcpns6
088100100413     C     sdrpkg        DIV       100           N11
088200100413     C                   ADD(H)    N11           n_PCPPQ6
088300100413     C                   ADD       sdrnco        n_PCPNC6
088400100413     C                   ADD(H)    sdrvol        n_PCPVO6
088500100413     c
088600100413     c                   when      sdrmes=7
088700100413     C                   ADD       SDRNsp        n_pcpns7
088800100413     C     sdrpkg        DIV       100           N11
088900100413     C                   ADD(H)    N11           n_PCPPQ7
089000100413     C                   ADD       sdrnco        n_PCPNC7
089100100413     C                   ADD(H)    sdrvol        n_PCPVO7
089200100413     c
089300100413     c                   when      sdrmes=8
089400100413     C                   ADD       SDRNsp        n_pcpns8
089500100413     C     sdrpkg        DIV       100           N11
089600100413     C                   ADD(H)    N11           n_PCPPQ8
089700100413     C                   ADD       sdrnco        n_PCPNC8
089800100413     C                   ADD(H)    sdrvol        n_PCPVO8
089900100413     c
090000100413     c                   when      sdrmes=9
090100100413     C                   ADD       SDRNsp        n_pcpns9
090200100413     C     sdrpkg        DIV       100           N11
090300100413     C                   ADD(H)    N11           n_PCPPQ9
090400100413     C                   ADD       sdrnco        n_PCPNC9
090500100413     C                   ADD(H)    sdrvol        n_PCPVO9
090600100413     c
090700100413     c                   when      sdrmes=10
090800100413     C                   ADD       SDRNsp        n_pcpns0
090900100413     C     sdrpkg        DIV       100           N11
091000100413     C                   ADD(H)    N11           n_PCPPQ0
091100100413     C                   ADD       sdrnco        n_PCPNC0
091200100413     C                   ADD(H)    sdrvol        n_PCPVO0
091300100413     c
091400100413     c                   when      sdrmes=11
091500100413     C                   ADD       SDRNsp        n_pcpnsa
091600100413     C     sdrpkg        DIV       100           N11
091700100413     C                   ADD(H)    N11           n_PCPPQa
091800100413     C                   ADD       sdrnco        n_PCPNCa
091900100413     C                   ADD(H)    sdrvol        n_PCPVOa
092000100413     c
092100100413     c                   when      sdrmes=12
092200100413     C                   ADD       SDRNsp        n_pcpnsb
092300100413     C     sdrpkg        DIV       100           N11
092400100413     C                   ADD(H)    N11           n_PCPPQb
092500100413     C                   ADD       sdrnco        n_PCPNCb
092600100413     C                   ADD(H)    sdrvol        n_PCPVOb
092700100413     c                   ENDSL
092800100413     c
092900100413     c                   if        *in98
093000100413     C                   WRITE     SIPCP010
093100100413X   4C                   ELSE
093200100413     C                   UPDATE    SIPCP010
093300100413E   4C                   ENDIF
093400100413     c
093500100413     c                   else
093600100413     C*---
093700100413     C* A R R I V O
093800100413     C*---
093900100413     C                   Z-ADD     DEPFIL        KCAFLA                         *FILIALE ARRIVO
094000100413     C                   MOVEL     PARIOE        KCAIOE                         *DATI IN STATISTICA
094100100415     C                   MOVEL     sdrtsp        KCAtsp
094200100413     C                   MOVEL     fascia        KCAFPE                         *FASCIA DI PESO
094300100413     C                   MOVEL     DEPNTF        KCANTA                         *NETWORK ARRIVO
094400100413     C                   MOVEL     DEPNTT        KCANTP                         *NETWORK PARTENZA
094500100413     C     KEYPCA1       CHAIN     SIPCA11L                           98
094600100413IF  4C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
094700100413     C                   CLEAR                   SIPCA010                       *AZZERA FORMATO RECORD
094800100415     C                   Z-ADD     Ksdann        n_PCaaaa
094900100413     C                   Z-ADD     KCAFLA        n_PCAFLA                         *CHIAVE
095000100413     C                   MOVEL     KCAIOE        n_PCAIOE
095100100413     C                   MOVEL     KCAFPE        n_PCAFPE
095200100413     C                   MOVEL     KCAtsp        n_PCAtsp
095300100413     C                   MOVEL     KCANTA        n_PCANTA
095400100413     C                   MOVEL     KCANTP        n_PCANTP
095500100413    4c                   endif
095600100413     c* Addiziono campi
095700100413     c                   select
095800100413     c
095900100413     c                   when      sdrmes=1
096000100413     C                   ADD       SDRNsp        N_pcans1
096100100413     C     sdrpkg        DIV       100           N11
096200100413     C                   ADD(H)    N11           n_PCaPQ1                         *QUINTALI 1° MESE
096300100413     C                   ADD       sdrnco        N_PCaNC1                         *COLLI 1° MESE
096400100413     C                   ADD(H)    sdrvol        N_PCaVO1                         *VOLUME 1° MESE
096500100413     c
096600100413     c                   when      sdrmes=2
096700100413     C                   ADD       SDRNsp        n_pcans2
096800100413     C     sdrpkg        DIV       100           N11
096900100413     C                   ADD(H)    N11           n_PCaPQ2
097000100413     C                   ADD       sdrnco        n_PCaNC2
097100100413     C                   ADD(H)    sdrvol        n_PCaVO2
097200100413     c
097300100413     c                   when      sdrmes=3
097400100413     C                   ADD       SDRNsp        n_pcans3
097500100413     C     sdrpkg        DIV       100           N11
097600100413     C                   ADD(H)    N11           n_PCaPQ3
097700100413     C                   ADD       sdrnco        n_PCaNC3
097800100413     C                   ADD(H)    sdrvol        n_PCaVO3
097900100413     c
098000100413     c                   when      sdrmes=4
098100100413     C                   ADD       SDRNsp        n_pcans4
098200100413     C     sdrpkg        DIV       100           N11
098300100413     C                   ADD(H)    N11           n_PCaPQ4
098400100413     C                   ADD       sdrnco        n_PCaNC4
098500100413     C                   ADD(H)    sdrvol        n_PCaVO4
098600100413     c
098700100413     c                   when      sdrmes=5
098800100413     C                   ADD       SDRNsp        n_pcans5
098900100413     C     sdrpkg        DIV       100           N11
099000100413     C                   ADD(H)    N11           n_PCaPQ5
099100100413     C                   ADD       sdrnco        n_PCaNC5
099200100413     C                   ADD(H)    sdrvol        n_PCaVO5
099300100413     c
099400100413     c                   when      sdrmes=6
099500100413     C                   ADD       SDRNsp        n_pcans6
099600100413     C     sdrpkg        DIV       100           N11
099700100413     C                   ADD(H)    N11           n_PCaPQ6
099800100413     C                   ADD       sdrnco        n_PCaNC6
099900100413     C                   ADD(H)    sdrvol        n_PCaVO6
100000100413     c
100100100413     c                   when      sdrmes=7
100200100413     C                   ADD       SDRNsp        n_pcans7
100300100413     C     sdrpkg        DIV       100           N11
100400100413     C                   ADD(H)    N11           n_PCaPQ7
100500100413     C                   ADD       sdrnco        n_PCaNC7
100600100413     C                   ADD(H)    sdrvol        n_PCaVO7
100700100413     c
100800100413     c                   when      sdrmes=8
100900100413     C                   ADD       SDRNsp        n_pcans8
101000100413     C     sdrpkg        DIV       100           N11
101100100413     C                   ADD(H)    N11           n_PCaPQ8
101200100413     C                   ADD       sdrnco        n_PCaNC8
101300100413     C                   ADD(H)    sdrvol        n_PCaVO8
101400100413     c
101500100413     c                   when      sdrmes=9
101600100413     C                   ADD       SDRNsp        n_pcans9
101700100413     C     sdrpkg        DIV       100           N11
101800100413     C                   ADD(H)    N11           n_PCaPQ9
101900100413     C                   ADD       sdrnco        n_PCaNC9
102000100413     C                   ADD(H)    sdrvol        n_PCaVO9
102100100413     c
102200100413     c                   when      sdrmes=10
102300100413     C                   ADD       SDRNsp        n_pcans0
102400100413     C     sdrpkg        DIV       100           N11
102500100413     C                   ADD(H)    N11           n_PCaPQ0
102600100413     C                   ADD       sdrnco        n_PCaNC0
102700100413     C                   ADD(H)    sdrvol        n_PCaVO0
102800100413     c
102900100413     c                   when      sdrmes=11
103000100413     C                   ADD       SDRNsp        n_pcansa
103100100413     C     sdrpkg        DIV       100           N11
103200100413     C                   ADD(H)    N11           n_PCaPQa
103300100413     C                   ADD       sdrnco        n_PCaNCa
103400100413     C                   ADD(H)    sdrvol        n_PCaVOa
103500100413     c
103600100413     c                   when      sdrmes=12
103700100413     C                   ADD       SDRNsp        n_pcansb
103800100413     C     sdrpkg        DIV       100           N11
103900100413     C                   ADD(H)    N11           n_PCaPQb
104000100413     C                   ADD       sdrnco        n_PCaNCb
104100100413     C                   ADD(H)    sdrvol        n_PCaVOb
104200100413     c                   ENDSL
104300100413     c
104400100413     c                   if        *in98
104500100413     C                   WRITE     SIPCA010
104600100413X   4C                   ELSE
104700100413     C                   UPDATE    SIPCA010
104800100413E   4C                   ENDIF
104900100413E   3C                   ENDIF
105000100413     c
105100100413     C                   ENDSR
105200000000     C*------------------------------------------------------------------------*
105300000000     C*?CALANP - CALCOLA I SALDI DELL' ANNO PRECEDENTE
105400000000     C*------------------------------------------------------------------------*
105500000000     C     CALANP        BEGSR
105600000000     C*
105700000000     C* LETTURA SALDI X AA INIZIALE, E X AA FINALE SOLO SE DIVERSO DALL'INIZIALE
105800000000     C                   Z-ADD     1             J
105900000000DO  1C                   DO        2             J
106000000000     C                   MOVEL     '0'           $FIN2                          *REIMPOSTO FINE FILE
106100000000     C*
106200000000     C* SECONDO GIRO SOLO SE ANNO FINALE DIVERSO DALL' INIZIALE (PIENO)
106300000000IF  2C     PIF(J)        IFNE      *ZEROS
106400000000     C*
106500000000     C* POSIZIONAMENTO SUI SALDI DELL' ANNO PRECEDENTE E PRIMA LETTURA
106600000000     C                   EXSR      SETFI2
106700000000     C*
106800000000     C* FINCHE FINE SALDI DELLA FILIALE
106900000000DO  3C     $FIN2         DOWEQ     '0'
107000000000     C*
107100000000     C* MEMORIZZA SALDI ANNO PRECEDENTE
107200000000     C                   EXSR      MEMANP
107300000000     C*
107400000000     C* LETTURA PROSSIMO RECORD SALDI ANNO PRECEDENTE
107500000000     C                   EXSR      LETFI2
107600000000E   3C                   ENDDO
107700000000E   2C                   ENDIF
107800000000E   1C                   ENDDO
107900000000     C*
108000000000     C                   ENDSR
108100000000     C*------------------------------------------------------------------------*
108200000000     C*?SETFI2 - POSIZIONAMENTO E PRIMA LETTURA SALDI ANNO PRECEDENTE
108300000000     C*------------------------------------------------------------------------*
108400000000     C     SETFI2        BEGSR
108500000000     C*
108600000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI ANNO PRECEDENTE
108700000000     C                   Z-ADD     PIF(J)        KSDANN                         *ANNO
108800000000     C                   Z-ADD     ORGFIL        KSDFIL                         *FILIALE
108900000000     C                   Z-ADD     FIL(F)        KSDFIT                         *FILIALE DETTAGLIO
109000000000     C*
109100000000     C* MI POSIZIONO CON IL MESE INIZIALE SE ANNI UGUALI O PRIMO ANNO DIVERSO,
109200000000     C* OPPURE CON IL MESE = 01 SE GLI ANNI SONO DIVERSI E STO ELABORANDO IL 2°
109300000000IF  1C     TIPANN        IFEQ      'U'
109400000000     C     J             OREQ      1
109500000000     C                   Z-ADD     MESINI        KSDMES                         *MESE INIZIALE
109600000000E   1C                   ELSE
109700000000     C                   Z-ADD     01            KSDMES                         *MESE 01
109800000000E   1C                   ENDIF
109900000000     C*
110000000000     C* POSIZIONAMENTO E PRIMA LETTURA (ANNO INIZIALE - 1)
110100000000IF  1C     PARMPA        IFEQ      'P'
110200000000     C* PARTENZE
110300990609     C     KEYSDR        SETLL     SISDR01L                           99
110400000000X   1C                   ELSE
110500000000     C* ARRIVI
110600990609     C     KEYSDR        SETLL     SISDR02L                           99
110700000000E   1C                   ENDIF
110800000000     C*
110900000000IF  1C     *IN99         IFEQ      '1'
111000000000     C                   MOVEL     '1'           $FIN2
111100000000X   1C                   ELSE
111200000000     C                   EXSR      LETFI2                                       *LETTURA RECORD
111300000000E   1C                   ENDIF
111400000000     C*
111500000000     C                   ENDSR
111600000000     C*------------------------------------------------------------------------*
111700000000     C*?LETFI2 - LETTURA PROSSIMO RECORD SALDI ANNO PRECEDENTE
111800000000     C*------------------------------------------------------------------------*
111900000000     C     LETFI2        BEGSR
112000000000     C*
112100000000     C* LEGGO FINO A:
112200000000     C*     - FINE FILE
112300000000     C*     - TROVATO RECORD VALIDO
112400000000     C                   MOVEL     'N'           WRECOK
112500000000DO  1C     $FIN2         DOWEQ     '0'
112600000000     C     WRECOK        ANDEQ     'N'
112700000000     C*
112800000000     C* LETTURE SUCCESSIVE ALLA PRIMA (ANNO + FILIALE + DETTAGLIO)
112900000000IF  2C     PARMPA        IFEQ      'P'
113000990609     C     KEYSD2        READE     SISDR01L                               99
113100000000X   2C                   ELSE
113200990609     C     KEYSD2        READE     SISDR02L                               99
113300000000E   2C                   ENDIF
113400000000     C*
113500000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
113600000000     C                   MOVEL     '1'           $FIN2
113700000000X   2C                   ELSE
113800000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
113900000000E   2C                   ENDIF
114000000000     C*
114100000000E   1C                   ENDDO
114200000000     C*
114300000000     C                   ENDSR
114400000000     C*------------------------------------------------------------------------*
114500000000     C*?MEMANP - MEMORIZZA I SALDI DELL' ANNO PRECEDENTE
114600000000     C*------------------------------------------------------------------------*
114700000000     C     MEMANP        BEGSR
114800000000     C*
114900000000     C                   MOVEL     SDRFPE        FASCIA
115000000000     C                   Z-ADD     1             MM
115100000000     C*
115200000000     C* RICERCA DELLA POSIZIONE IN SHIERA DELL'ANNO/MESE CHE STO ELABORANDO
115300000000     C                   SETOFF                                           28
115400000000     C     SDRMES        LOOKUP    MES(MM)                                28
115500000000     C*
115600000000     C* SE MESE TROVATO, ADDIZIONO
115700000000IF  1C     *IN28         IFEQ      *ON
115800000000     C*
115900000000     C* FASCIA DI PESO PER FILIALE DETTAGLIO (SOLO SE RICHIESTO)
116000000000     C     FASCIA        OCCUR     FPDET
116100000000     C                   ADD       SDRNSP        SAT(MM)                        *SPEDIZIONI
116200000000     C                   ADD       SDRPKG        QAT(MM)                        *KILOGRAMMI
116300000000     C                   ADD       SDRNCO        CAT(MM)                        *COLLI
116400000000     C                   ADD       SDRVOL        VAT(MM)                        *VOLUME IN MC
116500000000     C*
116600000000     C* FASCIA DI PESO PER FILIALE
116700000000     C     FASCIA        OCCUR     FPFIL
116800000000     C                   ADD       SDRNSP        SAF(MM)                        *SPEDIZIONI
116900000000     C                   ADD       SDRPKG        QAF(MM)                        *KILOGRAMMI
117000000000     C                   ADD       SDRNCO        CAF(MM)                        *COLLI
117100000000     C                   ADD       SDRVOL        VAF(MM)                        *VOLUME IN MC
117200000000     C*
117300000000     C* FASCIA DI PESO PER AREA
117400000000     C     FASCIA        OCCUR     FPARE
117500000000     C                   ADD       SDRNSP        SAA(MM)
117600000000     C                   ADD       SDRPKG        QAA(MM)
117700000000     C                   ADD       SDRNCO        CAA(MM)
117800000000     C                   ADD       SDRVOL        VAA(MM)
117900000000     C*
118000000000     C* FASCIA DI PESO PER DIVISIONE
118100000000     C     FASCIA        OCCUR     FPDIV
118200000000     C                   ADD       SDRNSP        SAD(MM)
118300000000     C                   ADD       SDRPKG        QAD(MM)
118400000000     C                   ADD       SDRNCO        CAD(MM)
118500000000     C                   ADD       SDRVOL        VAD(MM)
118600000000     C*
118700000000     C* FASCIA DI PESO PER TOTALI GENERALI
118800000000     C     FASCIA        OCCUR     FPGEN
118900000000     C                   ADD       SDRNSP        SAG(MM)
119000000000     C                   ADD       SDRPKG        QAG(MM)
119100000000     C                   ADD       SDRNCO        CAG(MM)
119200000000     C                   ADD       SDRVOL        VAG(MM)
119300000000E   1C                   ENDIF
119400000000     C*
119500000000     C                   ENDSR
119600000000     C*------------------------------------------------------------------------*
119700000000     C*?CALBDG - CALCOLA I VALORI DEL BUDGET
119800000000     C*------------------------------------------------------------------------*
119900000000     C     CALBDG        BEGSR
120000000000     C*
120100000000     C* POSIZIONAMENTO SUL FILE BUDGET E PRIMA LETTURA
120200000000     C                   EXSR      SETFI3
120300000000     C*
120400000000     C* FINCHE' FINE FILE BUDGET O ROTTURA FILIALE
120500000000DO  1C     $FIN3         DOWEQ     '0'
120600000000     C*
120700000000     C* MEMORIZZA VALORI BUDGET
120800000000     C                   EXSR      MEMBDG
120900000000     C*
121000000000     C* LETTURA PROSSIMO RECORD BUDGET
121100000000     C                   EXSR      LETFI3
121200000000E   1C                   ENDDO
121300000000     C*
121400000000     C                   ENDSR
121500000000     C*------------------------------------------------------------------------*
121600000000     C*?SETF13 - POSIZIONAMENTO E PRIMA LETTURA FILE BUDGET
121700000000     C*------------------------------------------------------------------------*
121800000000     C     SETFI3        BEGSR
121900000000     C*
122000000000     C* SE SELEZ.TI TIPO SERVIZIO/PORTO/PRODOTTO NON LEGGO I VALORI DEL BUDGET
122100000000     C* PERCHE' QUESTI SONO PREVISTI SOLO PER FILIALE (NON NEL DETTAGLIO)
122200000000IF  1C     PARSER        IFNE      *BLANKS
122300000000     C     PARPOR        ORNE      *BLANKS
122400000000     C*          PARSPE    ORNE *BLANKS                    *MOMENTANEAM. ESCLUSO
122500000000     C     PARPRO        ORNE      *BLANKS
122600000000     C                   MOVEL     '1'           $FIN3                          *USCITA CICLO
122700000000     C                   MOVEL     '1'           NOBDG                          *NO LETTURA BUDGET
122800000000X   1C                   ELSE
122900000000     C                   MOVEL     '0'           NOBDG                          *SI LETTURA BUDGET
123000000000E   1C                   ENDIF
123100000000     C*
123200000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE BUDGET (FILIALE+ANNO+MESE)
123300000000     C* SOLO SE RICHIESTO TUTTI I SERVIZI/SPEDIZIONI/PRODOTTI
123400000000IF  1C     $FIN3         IFEQ      '0'
123500000000     C                   Z-ADD     DEPFIL        KBDFIL
123600000000     C                   Z-ADD     ANNINI        KBDANN
123700000000     C                   Z-ADD     MESINI        KBDMES
123800960115     C                   MOVEL     PARIOE        KBDIOE
123900960108     C*
124000020114     C     KEYBDG        SETLL     TEBDG01L                           99        *FINE ARCHIVIO
124100000000     C*
124200000000IF  2C     *IN99         IFEQ      '1'
124300000000     C                   MOVEL     '1'           $FIN3
124400000000X   2C                   ELSE
124500000000     C                   EXSR      LETFI3                                       *LETTURA RECORD
124600000000E   2C                   ENDIF
124700000000E   1C                   ENDIF
124800000000     C*
124900000000     C                   ENDSR
125000000000     C*------------------------------------------------------------------------*
125100000000     C*?LETF13 - LETTURA PROSSIMO RECORD BUDGET
125200000000     C*------------------------------------------------------------------------*
125300000000     C     LETFI3        BEGSR
125400000000     C*
125500000000     C* LEGGO FINO A:
125600000000     C*     - FINE FILE O ROTTURA FILIALE
125700000000     C*     - TROVATO RECORD VALIDO
125800000000     C                   MOVEL     'N'           WRECOK
125900000000DO  1C     $FIN3         DOWEQ     '0'
126000000000     C     WRECOK        ANDEQ     'N'
126100000000     C*
126200020114     C     KBDFIL        READE     TEBDG01L                               99
126300000000     C*
126400000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
126500000000     C                   MOVEL     '1'           $FIN3
126600000000X   2C                   ELSE
126700000000     C                   EXSR      CHKRE3                                       *CONTROLLO RECORD
126800000000E   2C                   ENDIF
126900000000     C*
127000000000E   1C                   ENDDO
127100000000     C*
127200000000     C                   ENDSR
127300000000     C*------------------------------------------------------------------------*
127400000000     C*?CHKRE3 - CONTROLLO RECORD BUDGET
127500000000     C*------------------------------------------------------------------------*
127600000000     C     CHKRE3        BEGSR
127700000000     C*
127800000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
127900000000     C*
128000000000     C* LA DATA LETTA DEVE ESSERE MAX/UGU ALLA DATA INIZ E MIN/UGU ALLA DATA FIN
128100990527IF  1C     TBGDAT        IFGE      PARDAT
128200990527     C     TBGDAT        ANDLE     PARDA2
128300000000X   1C                   ELSE
128400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
128500000000     C*
128600000000     C* SE POI DATA BUDGET MAGGIORE A DATA FINALE RICHIESTA, ESCO DAL CICLO
128700990527IF  2C     TBGDAT        IFGT      PARDA2
128800000000     C                   MOVEL     '1'           $FIN3
128900000000E   2C                   ENDIF
129000000000E   1C                   ENDIF
129100000000     C*
129200000000     C* VALORE DI BUDGET ANNULLATO, ESCLUDO
129300000000IF  1C     TBGATB        IFNE      *BLANKS
129400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
129500000000E   1C                   ENDIF
129600960105     C*
129700960115     C* DATI IN STATISTICA <> DA QUELLI RICHIESTI
129800960115IF  1C     PARIOE        IFNE      *BLANKS
129900960115IF  2C     PARIOE        IFNE      TBGIOE
130000960105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
130100960105E   2C                   ENDIF
130200960105E   1C                   ENDIF
130300000000     C*
130400000000     C                   ENDSR
130500000000     C*------------------------------------------------------------------------*
130600000000     C*?MEMBDG - MEMORIZZA VALORI BUDGET
130700000000     C*------------------------------------------------------------------------*
130800000000     C     MEMBDG        BEGSR
130900000000     C*                                                    *PARTENZA
131000000000IF  1C     PARMPA        IFEQ      'P'
131100000000     C                   Z-ADD     TBGNSP        LAVSPE
131200000000     C                   Z-ADD     TBGKGP        LAVCHI
131300000000     C                   Z-ADD     TBGNCP        LAVCOL
131400000000     C                   Z-ADD     TBGVOP        LAVVOL
131500000000X   1C                   ELSE                                                   *ARRIVO
131600000000     C                   Z-ADD     TBGNSA        LAVSPE
131700000000     C                   Z-ADD     TBGKGA        LAVCHI
131800000000     C                   Z-ADD     TBGNCA        LAVCOL
131900000000     C                   Z-ADD     TBGVOA        LAVVOL
132000000000E   1C                   ENDIF
132100000000     C*
132200000000     C                   MOVEL     TBGFPE        FASCIA
132300000000     C                   Z-ADD     1             MM
132400000000     C*
132500000000     C* RICERCA DELLA POSIZIONE IN SHIERA DELL'ANNO/MESE CHE STO ELABORANDO
132600000000     C                   SETOFF                                           28
132700000000     C     TBGMES        LOOKUP    MES(MM)                                28
132800000000     C*
132900000000     C* SE MESE TROVATO, ADDIZIONO
133000000000IF  1C     *IN28         IFEQ      *ON
133100000000     C*
133200000000     C* FASCIA DI PESO PER FILIALE
133300000000     C     FASCIA        OCCUR     FPFIL
133400000000     C                   ADD       LAVSPE        SBF(MM)
133500000000     C                   ADD       LAVCHI        QBF(MM)
133600000000     C                   ADD       LAVCOL        CBF(MM)
133700000000     C                   ADD       LAVVOL        VBF(MM)
133800000000     C*
133900000000     C* FASCIA DI PESO PER AREA
134000000000     C     FASCIA        OCCUR     FPARE
134100000000     C                   ADD       LAVSPE        SBA(MM)
134200000000     C                   ADD       LAVCHI        QBA(MM)
134300000000     C                   ADD       LAVCOL        CBA(MM)
134400000000     C                   ADD       LAVVOL        VBA(MM)
134500000000     C*
134600000000     C* FASCIA DI PESO PER DIVISIONE
134700000000     C     FASCIA        OCCUR     FPDIV
134800000000     C                   ADD       LAVSPE        SBD(MM)
134900000000     C                   ADD       LAVCHI        QBD(MM)
135000000000     C                   ADD       LAVCOL        CBD(MM)
135100000000     C                   ADD       LAVVOL        VBD(MM)
135200000000     C*
135300000000     C* FASCIA DI PESO PER TOTALI GENERALI
135400000000     C     FASCIA        OCCUR     FPGEN
135500000000     C                   ADD       LAVSPE        SBG(MM)
135600000000     C                   ADD       LAVCHI        QBG(MM)
135700000000     C                   ADD       LAVCOL        CBG(MM)
135800000000     C                   ADD       LAVVOL        VBG(MM)
135900000000E   1C                   ENDIF
136000000000     C*
136100000000     C                   ENDSR
136200000000     C*------------------------------------------------------------------------*
136300000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
136400000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
136500000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
136600000000     C*------------------------------------------------------------------------*
136700000000     C     DECDIV        BEGSR
136800000000     C*
136900020906     C                   MOVEL     1             KTBUTE
137000000000     C                   MOVE      '17'          KTBCOD
137100000000     C                   MOVE      *BLANKS       KTBKEY
137200000000     C                   MOVEL     DEPDIV        KTBKEY
137300000000     C     KEYTAB        CHAIN     TABEL00F                           99
137400000000     C*
137500000000IF  1C     *IN99         IFEQ      '1'
137600000000     C     TBLFLG        OREQ      '*'
137700000000     C                   MOVEL     *ALL'*'       DESDIV           20
137800000000X   1C                   ELSE
137900000000     C                   MOVEL     TBLUNI        DESDIV
138000000000E   1C                   ENDIF
138100000000     C*
138200000000     C                   ENDSR
138300000000     C*------------------------------------------------------------------------*
138400000000     C*? DECARE - DECODIFICA CODICE AREA
138500000000     C*          INPUT  -- DEPARE: CODICE AREA
138600000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
138700000000     C*------------------------------------------------------------------------*
138800000000     C     DECARE        BEGSR
138900000000     C*
139000020906     C                   MOVEL     1             KTBUTE
139100000000     C                   MOVE      '05'          KTBCOD
139200000000     C                   MOVE      *BLANKS       KTBKEY
139300000000     C                   MOVEL     DEPARE        KTBKEY
139400000000     C     KEYTAB        CHAIN     TABEL00F                           99
139500000000     C*
139600000000IF  1C     *IN99         IFEQ      '1'
139700000000     C     TBLFLG        OREQ      '*'
139800000000     C                   MOVEL     *ALL'*'       DESARE           20
139900000000X   1C                   ELSE
140000000000     C                   MOVEL     TBLUNI        DESARE
140100000000E   1C                   ENDIF
140200000000     C*
140300000000     C                   ENDSR
140400000000     C*------------------------------------------------------------------------*
140500000000     C*? DECFIL - DECODIFICA CODICE FILIALE
140600000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
140700000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
140800000000     C*------------------------------------------------------------------------*
140900000000     C     DECFIL        BEGSR
141000000000     C*
141100000000     C                   Z-ADD     1             I
141200000000     C     DEPFIL        LOOKUP    FIL(I)                                 99
141300000000IF  1C     *IN99         IFEQ      '0'
141400000000     C                   MOVEL     *ALL'*'       DESFIL           20
141500020325     C                   MOVEL     *BLANKS       LAVNTX
141600000000X   1C                   ELSE
141700000000     C                   MOVEL     DFIL(I)       DESFIL
141800000703     C                   MOVEL     NTX(I)        LAVNTX                         *NETWORK
141900000000E   1C                   ENDIF
142000000000     C*
142100000000     C                   ENDSR
142200000000     C*------------------------------------------------------------------------*
142300000000     C*?CARORG - CARICA I CODICI DELLE FILIALI E LE LORO DECODIFICHE
142400000000     C*------------------------------------------------------------------------*
142500000000     C     CARORG        BEGSR
142600000000     C*
142700000000     C                   Z-ADD     *ZEROS        KORFIL
142800000000     C     KEYOR2        SETLL     AZORG01L                           99        *RECORD NON TROVATO
142900000000     C  N99              READ      AZORG01L                               99
143000000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
143100000000DO  1C     *IN99         DOWEQ     '0'
143200000000IF  2C     ORGFVA        IFEQ      *BLANKS
143300000000IF  3C     ORGFAG        IFNE      'V'
143400000703     C                   CLEAR                   OG143
143500000703     C                   MOVEL     ORGDE3        OG143
143600000000     C                   ADD       1             I
143700000000     C                   Z-ADD     ORGFIL        FIL(I)
143800000000     C                   MOVEL     ORGDES        DFIL(I)
143900020325     C* CALCOLA NETWORK
144000020325     C                   MOVEL     §OGNTW        NTX(I)                         *NETWORK
144100040323     C* ELIMINATO NETWORK MES CON COR
144200040323     C                   IF        §OGNTW = 'MES'
144300040323     C                   MOVEL     'COR'         NTX(I)
144400040323     C                   END
144500020325     C* VERIFICA SE TRATTASI DI I/E
144600020325     C                   EXSR      CHKNTW
144700020325     C                   MOVEL     §NTWFIE       FL1(I)
144800020325     C*
144900000703E   3C                   ENDIF
145000000000E   2C                   ENDIF
145100000000     C                   READ      AZORG01L                               99
145200000000E   1C                   ENDDO
145300000000     C*
145400000000     C                   ENDSR
145500020325     C*------------------------------------------------------------------------*
145600020325     C* CHKNTW - CONTROLLA IL NETWORK
145700020325     C*------------------------------------------------------------------------*
145800020325     C     CHKNTW        BEGSR
145900020325     C*
146000020325     C* VERIFICA VALORE IN TABELLA
146100020325     C                   CLEAR                   DNTW
146200020325     C                   CLEAR                   TIBS02
146300020325     C                   MOVEL     'NTW'         T02COD
146400020325     C                   MOVEL     KNSIF         T02SIF
146500020325     C                   MOVEL     §OGNTW        T02KE1
146600020325     C                   MOVEL     'C'           T02MOD
146700020325     C                   CALL      'TIBS02R'
146800020325     C                   PARM                    KPJBA
146900020325     C                   PARM                    TIBS02
147000020325     C*
147100020325     C     T02ERR        IFEQ      *BLANKS
147200020325     C                   MOVEL     T02UNI        DNTW
147300020325     C                   ENDIF
147400020325     C*
147500020325     C                   ENDSR
147600000000     C*------------------------------------------------------------------------*
147700000000     C*?CANFPC - CANCELLAZIONE FILE SALDI MOVIMENTO PER PC
147800000000     C*------------------------------------------------------------------------*
147900000000     C     CANFPC        BEGSR
148000950529     C*
148100950529     C* SE E' STATA RICHIESTA LA STATISTICA PER ITALIA, IMPORT O EXPORT CANCELLA
148200950529     C* SOLO I RECORD INTERESSATI ("I"/"M"/"X"/" ");
148300950529     C* SE E'STATA CHIESTA LA STATISTICA CON TUTTI I DATI,CANCELLA TUTTO IL FILE
148400950529IF  1C     PARIOE        IFEQ      *BLANKS
148500950529     C                   Z-ADD     45            LUNG             15 5          *LUNGHEZZA COMANDO
148600000000     C                   MOVEL     *BLANKS       COMMAN           80            *DESCRIZIONE COMANDO
148700950529     C***
148800950529     C* PARTENZA
148900950529     C***
149000950529IF  2C     PARMPA        IFEQ      'P'
149100000703     C                   MOVEA     CMD1(1)       COMMAN
149200000703     C                   CALL      'QCMDEXC'                                    *CLRPFM NEW FILE
149300000614     C                   PARM                    COMMAN
149400000614     C                   PARM                    LUNG
149500950529X   2C                   ELSE
149600950529     C***
149700950529     C* ARRIVO
149800950529     C***
149900000703     C                   MOVEA     CMD2(1)       COMMAN
150000000614     C                   CALL      'QCMDEXC'                                    *CLRPFM
150100000614     C                   PARM                    COMMAN
150200000614     C                   PARM                    LUNG
150300950529E   2C                   ENDIF
150400950529E   1C                   ENDIF
150500950529     C* APERTURA FILE
150600950529IF  1C     PARMPA        IFEQ      'P'
150700000703     C                   OPEN      SIPCP01L
150800100413     C                   OPEN      SIPCP11L
150900950529X   1C                   ELSE
151000000703     C                   OPEN      SIPCA01L
151100100413     C                   OPEN      SIPCA11L
151200950529E   1C                   ENDIF
151300950529     C***
151400950529     C* PARTENZA
151500950529     C***
151600000703IF  1C     PARIOE        IFNE      *BLANKS                                      *ITALIA/IMPORT/EXPORT
151700950529IF  2C     PARMPA        IFEQ      'P'
151800000703     C                   READ      SIPCP01L                               99
151900000703DO  3C     *IN99         DOWEQ     *OFF
152000000703IF  4C     PCPIOE        IFEQ      PARIOE
152100000703IF  5C     PARPOS        IFEQ      'S'                                          *DATI POSTE
152200020409     C     PCPNTP        ANDEQ     'PPT'                                         NW PART POSTE
152300000703     C     PARPOS        OREQ      'S'                                          *DATI POSTE
152400020409     C     PCPNTA        ANDEQ     'PPT'                                         NW ARR POSTE
152500000703     C     PARPOS        OREQ      'N'                                          *DATI NO POSTE
152600020409     C     PCPNTP        ANDNE     'PPT'                                         NW PART NO POSTE
152700020409     C     PCPNTA        ANDNE     'PPT'                                         NW ARR NO POSTE
152800000703     C                   DELETE    SIPCP000
152900000703E   5C                   ENDIF
153000000703E   4C                   ENDIF
153100000703     C                   READ      SIPCP01L                               99
153200000703E   3C                   ENDDO
153300100413     c
153400100413     C                   READ      SIPCP11L                               99
153500100413DO  3C     *IN99         DOWEQ     *OFF
153600100415IF  4C     n_PCPIOE      IFEQ      PARIOE
153700100413     c                   if        n_pcpaaa=annini or n_pcpaaa=annfin
153800100413     C                   DELETE    SIPCP010
153900100413E   4C                   ENDIF
154000100413E   4C                   ENDIF
154100100413     c
154200100415     C                   READ      SIPCP11L                               99
154300100413E   3C                   ENDDO
154400950529     C***
154500950529     C* ARRIVO
154600950529     C***
154700950529X   2C                   ELSE
154800000703     C                   READ      SIPCA01L                               99
154900000614DO  3C     *IN99         DOWEQ     *OFF
155000000614IF  4C     PCAIOE        IFEQ      PARIOE
155100000703IF  5C     PARPOS        IFEQ      'S'                                          *DATI POSTE
155200020409     C     PCANTP        ANDEQ     'PPT'                                         NW PART POSTE
155300000703     C     PARPOS        OREQ      'S'                                          *DATI POSTE
155400020409     C     PCANTA        ANDEQ     'PPT'                                         NW ARR POSTE
155500000703     C     PARPOS        OREQ      'N'                                          *DATI NO POSTE
155600020409     C     PCANTP        ANDNE     'PPT'                                         NW PART NO POSTE
155700020409     C     PCANTA        ANDNE     'PPT'                                         NW ARR NO POSTE
155800000703     C                   DELETE    SIPCA000
155900000703E   5C                   ENDIF
156000000703E   4C                   ENDIF
156100000703     C                   READ      SIPCA01L                               99
156200000614E   3C                   ENDDO
156300100413     c
156400100413     C                   READ      SIPCA11L                               99
156500100413DO  3C     *IN99         DOWEQ     *OFF
156600100415IF  4C     n_PCAIOE      IFEQ      PARIOE
156700100413     c                   if        n_pcaAaa=annini or n_pcaAaa=annfin
156800100413     c                   delete    sipca010
156900100413E   5C                   ENDIF
157000100413E   4C                   ENDIF
157100100413     c
157200100415     C                   READ      SIPCA11L                               99
157300100413E   3C                   ENDDO
157400100413     c
157500950529E   2C                   ENDIF
157600950529E   1C                   ENDIF
157700000000     C*
157800000000     C                   ENDSR
157900000703     C*------------------------------------------------------------------------*
158000000703     C*?SCRSIX - SCRIVE MOVIMENTO FILIALE SUL FILE PC
158100000703     C*------------------------------------------------------------------------*
158200000703     C     SCRSIX        BEGSR
158300000703     C*
158400000703     C* SCHIERE SALDI MENSILI DI FILIALE DETTAGLIO
158500000703IF  1C     PASDET        IFEQ      '1'                                          *MEMORIZZATO QC
158600000703DO  2C     1             DO        10            A
158700000703     C     A             OCCUR     FPDET
158800000703     C*---
158900000703     C* P A R T E N Z A
159000000703     C*---
159100000703IF  3C     PARMPA        IFEQ      'P'
159200000703     C                   Z-ADD     DEPFIL        KCPFLP                         *FILIALE PARTENZA
159300000703     C                   MOVEL     PARIOE        KCPIOE                         *DATI IN STATISTICA
159400000703     C                   MOVEL     A             KCPFPE                         *FASCIA DI PESO
159500000703     C                   MOVEL     DEPNTF        KCPNTP                         *NETWORK PARTENZA
159600000703     C                   MOVEL     DEPNTT        KCPNTA                         *NETWORK ARRIVO
159700000703     C     KEYPCP        CHAIN     SIPCP01L                           98
159800000703IF  4C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
159900000703     C                   CLEAR                   SIPCP000                       *AZZERA FORMATO RECORD
160000000703     C                   Z-ADD     KCPFLP        PCPFLP                         *CHIAVE
160100000703     C                   MOVEL     KCPIOE        PCPIOE
160200000703     C                   MOVEL     KCPFPE        PCPFPE
160300000703     C                   MOVEL     KCPNTP        PCPNTP
160400000703     C                   MOVEL     KCPNTA        PCPNTA
160500000703     C                   EXSR      ADDPCP                                       *AGGIUNGE DATI RECORD
160600000703     C                   WRITE     SIPCP000
160700000703X   4C                   ELSE                                                   M O D I F I C A
160800000703     C                   EXSR      ADDPCP                                       *AGGIUNGE DATI RECORD
160900000703     C                   UPDATE    SIPCP000
161000000703E   4C                   ENDIF
161100000703     C*---
161200000703     C* A R R I V O
161300000703     C*---
161400000703X   3C                   ELSE
161500000703     C                   Z-ADD     DEPFIL        KCAFLA                         *FILIALE ARRIVO
161600000703     C                   MOVEL     PARIOE        KCAIOE                         *DATI IN STATISTICA
161700000703     C                   MOVEL     A             KCAFPE                         *FASCIA DI PESO
161800000703     C                   MOVEL     DEPNTF        KCANTA                         *NETWORK ARRIVO
161900000703     C                   MOVEL     DEPNTT        KCANTP                         *NETWORK PARTENZA
162000000703     C     KEYPCA        CHAIN     SIPCA01L                           98
162100000703IF  4C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
162200000703     C                   CLEAR                   SIPCA000                       *AZZERA FORMATO RECORD
162300000703     C                   Z-ADD     KCAFLA        PCAFLA                         *CHIAVE
162400000703     C                   MOVEL     KCAIOE        PCAIOE
162500000703     C                   MOVEL     KCAFPE        PCAFPE
162600000703     C                   MOVEL     KCANTA        PCANTA
162700000703     C                   MOVEL     KCANTP        PCANTP
162800000703     C                   EXSR      ADDPCA                                       *AGGIUNGE DATI RECORD
162900000703     C                   WRITE     SIPCA000
163000000703X   4C                   ELSE                                                   M O D I F I C A
163100000703     C                   EXSR      ADDPCA                                       *AGGIUNGE DATI RECORD
163200000703     C                   UPDATE    SIPCA000
163300000703E   4C                   ENDIF
163400000703E   3C                   ENDIF
163500000703     C*
163600000703E   2C                   ENDDO
163700000703E   1C                   ENDIF
163800000703     C*
163900000703     C                   ENDSR
164000000703     C*------------------------------------------------------------------------*
164100000703     C*?ADDPCP - AGGIUNGE DATI AL RECORD DI SIPCP00F
164200000703     C*------------------------------------------------------------------------*
164300000703     C     ADDPCP        BEGSR
164400000703     C*
164500010227     C                   Z-ADD     *ZEROS        N11              11 3
164600000703     C*
164700000703     C                   ADD       SPT(1)        PCPNS1                         *SPEDIZIONI 1° MESE
164800000703     C                   ADD       SPT(2)        PCPNS2                         *SPEDIZIONI 2° MESE
164900000703     C                   ADD       SPT(3)        PCPNS3                         *SPEDIZIONI 3° MESE
165000000703     C                   ADD       SPT(4)        PCPNS4                         *SPEDIZIONI 4° MESE
165100000703     C                   ADD       SPT(5)        PCPNS5                         *SPEDIZIONI 5° MESE
165200000703     C                   ADD       SPT(6)        PCPNS6                         *SPEDIZIONI 6° MESE
165300000703     C                   ADD       SPT(7)        PCPNS7                         *SPEDIZIONI 7° MESE
165400000703     C                   ADD       SPT(8)        PCPNS8                         *SPEDIZIONI 8° MESE
165500000703     C                   ADD       SPT(9)        PCPNS9                         *SPEDIZIONI 9° MESE
165600000703     C                   ADD       SPT(10)       PCPNS0                         *SPEDIZIONI 10°MESE
165700000703     C                   ADD       SPT(11)       PCPNSA                         *SPEDIZIONI 11°MESE
165800000703     C                   ADD       SPT(12)       PCPNSB                         *SPEDIZIONI 12°MESE
165900000703     C*
166000010227     C     QTT(1)        DIV       100           N11
166100010227     C                   ADD(H)    N11           PCPPQ1                         *QUINTALI 1° MESE
166200010227     C     QTT(2)        DIV       100           N11
166300010227     C                   ADD(H)    N11           PCPPQ2                         *QUINTALI 2° MESE
166400010227     C     QTT(3)        DIV       100           N11
166500010227     C                   ADD(H)    N11           PCPPQ3                         *QUINTALI 3° MESE
166600010227     C     QTT(4)        DIV       100           N11
166700010227     C                   ADD(H)    N11           PCPPQ4                         *QUINTALI 4° MESE
166800010227     C     QTT(5)        DIV       100           N11
166900010227     C                   ADD(H)    N11           PCPPQ5                         *QUINTALI 5° MESE
167000010227     C     QTT(6)        DIV       100           N11
167100010227     C                   ADD(H)    N11           PCPPQ6                         *QUINTALI 6° MESE
167200010227     C     QTT(7)        DIV       100           N11
167300010227     C                   ADD(H)    N11           PCPPQ7                         *QUINTALI 7° MESE
167400010227     C     QTT(8)        DIV       100           N11
167500010227     C                   ADD(H)    N11           PCPPQ8                         *QUINTALI 8° MESE
167600010227     C     QTT(9)        DIV       100           N11
167700010227     C                   ADD(H)    N11           PCPPQ9                         *QUINTALI 9° MESE
167800010227     C     QTT(10)       DIV       100           N11
167900010227     C                   ADD(H)    N11           PCPPQ0                         *QUINTALI 10°MESE
168000010227     C     QTT(11)       DIV       100           N11
168100010227     C                   ADD(H)    N11           PCPPQA                         *QUINTALI 11°MESE
168200010227     C     QTT(12)       DIV       100           N11
168300010227     C                   ADD(H)    N11           PCPPQB                         *QUINTALI 12°MESE
168400000703     C*
168500000703     C                   ADD       CLT(1)        PCPNC1                         *COLLI 1° MESE
168600000703     C                   ADD       CLT(2)        PCPNC2                         *COLLI 2° MESE
168700000703     C                   ADD       CLT(3)        PCPNC3                         *COLLI 3° MESE
168800000703     C                   ADD       CLT(4)        PCPNC4                         *COLLI 4° MESE
168900000703     C                   ADD       CLT(5)        PCPNC5                         *COLLI 5° MESE
169000000703     C                   ADD       CLT(6)        PCPNC6                         *COLLI 6° MESE
169100000703     C                   ADD       CLT(7)        PCPNC7                         *COLLI 7° MESE
169200000703     C                   ADD       CLT(8)        PCPNC8                         *COLLI 8° MESE
169300000703     C                   ADD       CLT(9)        PCPNC9                         *COLLI 9° MESE
169400000703     C                   ADD       CLT(10)       PCPNC0                         *COLLI 10°MESE
169500000703     C                   ADD       CLT(11)       PCPNCA                         *COLLI 11°MESE
169600000703     C                   ADD       CLT(12)       PCPNCB                         *COLLI 12°MESE
169700000703     C*
169800000703     C                   ADD(H)    VLT(1)        PCPVO1                         *VOLUME 1° MESE
169900000703     C                   ADD(H)    VLT(2)        PCPVO2                         *VOLUME 2° MESE
170000000703     C                   ADD(H)    VLT(3)        PCPVO3                         *VOLUME 3° MESE
170100000703     C                   ADD(H)    VLT(4)        PCPVO4                         *VOLUME 4° MESE
170200000703     C                   ADD(H)    VLT(5)        PCPVO5                         *VOLUME 5° MESE
170300000703     C                   ADD(H)    VLT(6)        PCPVO6                         *VOLUME 6° MESE
170400000703     C                   ADD(H)    VLT(7)        PCPVO7                         *VOLUME 7° MESE
170500000703     C                   ADD(H)    VLT(8)        PCPVO8                         *VOLUME 8° MESE
170600000703     C                   ADD(H)    VLT(9)        PCPVO9                         *VOLUME 9° MESE
170700000703     C                   ADD(H)    VLT(10)       PCPVO0                         *VOLUME 10°MESE
170800000703     C                   ADD(H)    VLT(11)       PCPVOA                         *VOLUME 11°MESE
170900000703     C                   ADD(H)    VLT(12)       PCPVOB                         *VOLUME 12°MESE
171000000703     C*
171100000703     C                   ENDSR
171200000703     C*------------------------------------------------------------------------*
171300000703     C*?ADDPCA - AGGIUNGE DATI AL RECORD DI SIPCA00F
171400000703     C*------------------------------------------------------------------------*
171500000703     C     ADDPCA        BEGSR
171600010227     C*
171700010227     C                   Z-ADD     *ZEROS        N11              11 3
171800000703     C*
171900000703     C                   ADD       SPT(1)        PCANS1                         *SPEDIZIONI 1° MESE
172000000703     C                   ADD       SPT(2)        PCANS2                         *SPEDIZIONI 2° MESE
172100000703     C                   ADD       SPT(3)        PCANS3                         *SPEDIZIONI 3° MESE
172200000703     C                   ADD       SPT(4)        PCANS4                         *SPEDIZIONI 4° MESE
172300000703     C                   ADD       SPT(5)        PCANS5                         *SPEDIZIONI 5° MESE
172400000703     C                   ADD       SPT(6)        PCANS6                         *SPEDIZIONI 6° MESE
172500000703     C                   ADD       SPT(7)        PCANS7                         *SPEDIZIONI 7° MESE
172600000703     C                   ADD       SPT(8)        PCANS8                         *SPEDIZIONI 8° MESE
172700000703     C                   ADD       SPT(9)        PCANS9                         *SPEDIZIONI 9° MESE
172800000703     C                   ADD       SPT(10)       PCANS0                         *SPEDIZIONI 10°MESE
172900000703     C                   ADD       SPT(11)       PCANSA                         *SPEDIZIONI 11°MESE
173000000703     C                   ADD       SPT(12)       PCANSB                         *SPEDIZIONI 12°MESE
173100000703     C*
173200010227     C     QTT(1)        DIV       100           N11
173300010227     C                   ADD(H)    N11           PCAPQ1                         *QUINTALI 1° MESE
173400010227     C     QTT(2)        DIV       100           N11
173500010227     C                   ADD(H)    N11           PCAPQ2                         *QUINTALI 2° MESE
173600010227     C     QTT(3)        DIV       100           N11
173700010227     C                   ADD(H)    N11           PCAPQ3                         *QUINTALI 3° MESE
173800010227     C     QTT(4)        DIV       100           N11
173900010227     C                   ADD(H)    N11           PCAPQ4                         *QUINTALI 4° MESE
174000010227     C     QTT(5)        DIV       100           N11
174100010227     C                   ADD(H)    N11           PCAPQ5                         *QUINTALI 5° MESE
174200010227     C     QTT(6)        DIV       100           N11
174300010227     C                   ADD(H)    N11           PCAPQ6                         *QUINTALI 6° MESE
174400010227     C     QTT(7)        DIV       100           N11
174500010227     C                   ADD(H)    N11           PCAPQ7                         *QUINTALI 7° MESE
174600010227     C     QTT(8)        DIV       100           N11
174700010227     C                   ADD(H)    N11           PCAPQ8                         *QUINTALI 8° MESE
174800010227     C     QTT(9)        DIV       100           N11
174900010227     C                   ADD(H)    N11           PCAPQ9                         *QUINTALI 9° MESE
175000010227     C     QTT(10)       DIV       100           N11
175100010227     C                   ADD(H)    N11           PCAPQ0                         *QUINTALI 10°MESE
175200010227     C     QTT(11)       DIV       100           N11
175300010227     C                   ADD(H)    N11           PCAPQA                         *QUINTALI 11°MESE
175400010227     C     QTT(12)       DIV       100           N11
175500010227     C                   ADD(H)    N11           PCAPQB                         *QUINTALI 12°MESE
175600000703     C*
175700000703     C                   ADD       CLT(1)        PCANC1                         *COLLI 1° MESE
175800000703     C                   ADD       CLT(2)        PCANC2                         *COLLI 2° MESE
175900000703     C                   ADD       CLT(3)        PCANC3                         *COLLI 3° MESE
176000000703     C                   ADD       CLT(4)        PCANC4                         *COLLI 4° MESE
176100000703     C                   ADD       CLT(5)        PCANC5                         *COLLI 5° MESE
176200000703     C                   ADD       CLT(6)        PCANC6                         *COLLI 6° MESE
176300000703     C                   ADD       CLT(7)        PCANC7                         *COLLI 7° MESE
176400000703     C                   ADD       CLT(8)        PCANC8                         *COLLI 8° MESE
176500000703     C                   ADD       CLT(9)        PCANC9                         *COLLI 9° MESE
176600000703     C                   ADD       CLT(10)       PCANC0                         *COLLI 10°MESE
176700000703     C                   ADD       CLT(11)       PCANCA                         *COLLI 11°MESE
176800000703     C                   ADD       CLT(12)       PCANCB                         *COLLI 12°MESE
176900000703     C*
177000000703     C                   ADD(H)    VLT(1)        PCAVO1                         *VOLUME 1° MESE
177100000703     C                   ADD(H)    VLT(2)        PCAVO2                         *VOLUME 2° MESE
177200000703     C                   ADD(H)    VLT(3)        PCAVO3                         *VOLUME 3° MESE
177300000703     C                   ADD(H)    VLT(4)        PCAVO4                         *VOLUME 4° MESE
177400000703     C                   ADD(H)    VLT(5)        PCAVO5                         *VOLUME 5° MESE
177500000703     C                   ADD(H)    VLT(6)        PCAVO6                         *VOLUME 6° MESE
177600000703     C                   ADD(H)    VLT(7)        PCAVO7                         *VOLUME 7° MESE
177700000703     C                   ADD(H)    VLT(8)        PCAVO8                         *VOLUME 8° MESE
177800000703     C                   ADD(H)    VLT(9)        PCAVO9                         *VOLUME 9° MESE
177900000703     C                   ADD(H)    VLT(10)       PCAVO0                         *VOLUME 10°MESE
178000000703     C                   ADD(H)    VLT(11)       PCAVOA                         *VOLUME 11°MESE
178100000703     C                   ADD(H)    VLT(12)       PCAVOB                         *VOLUME 12°MESE
178200000703     C*
178300000703     C                   ENDSR
178400000000     C*------------------------------------------------------------------------*
178500000000     C*?CARTAB - CARICA I DATI IN TABELLE
178600000000     C*------------------------------------------------------------------------*
178700000000     C     CARTAB        BEGSR
178800000000     C*-------------------------
178900000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
179000000000     C*-------------------------
179100020906     C                   Z-ADD     1             KTBUTE
179200000000     C                   MOVEL     '2W'          KTBCOD
179300000000     C     KEYTA2        SETLL     TABEL00F
179400000000     C     KEYTA2        READE     TABEL00F                               99
179500000000     C*
179600000000     C                   Z-ADD     *ZEROS        G                 4 0
179700000000DO  1C     *IN99         DOWEQ     *OFF
179800000000IF  2C     TBLFLG        IFEQ      *BLANKS
179900000000     C                   ADD       1             G
180000000000     C                   MOVEL     TBLKEY        FLAB(G)
180100000000     C                   MOVEL     TBLUNI        DS2W
180200000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
180300000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
180400000000E   2C                   ENDIF
180500000000     C*
180600000000     C     KEYTA2        READE     TABEL00F                               99
180700000000E   1C                   ENDDO
180800000000     C*
180900000000     C                   ENDSR
181000950712     C*------------------------------------------------------------------------*
181100950712     C*?AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
181200950712     C*          OUTPUT--> MES  : MESI DA ELABORARE
181300950712     C*                    ANN  : ANNI DEI MESI DA ELABORARE
181400950712     C*                    PARJ : NUMERO DEI MESI DA ELABORARE
181500950712     C*------------------------------------------------------------------------*
181600950712     C     AMELAB        BEGSR
181700950712     C*
181800950712     C                   CLEAR                   ANN                            *ANNI
181900950712     C                   CLEAR                   MES                            *MESI
182000950712     C                   Z-ADD     *ZEROS        PARJ              2 0          *N° MESI DA ELABORARE
182100950712     C*
182200950712     C* ANNO INIZIALE UGUALE AD ANNO FINALE
182300950712IF  1C     WAA2          IFEQ      WAA
182400950712     C*
182500950712     C* CALCOLO I MESI DI DIFFERENZA
182600950712DO  2C     WMM           DOWLE     WMM2
182700950712     C                   ADD       1             JI
182800950712     C                   Z-ADD     WAA           ANN(JI)
182900950712     C                   Z-ADD     WMM           MES(JI)
183000950712     C                   ADD       1             WMM
183100950712E   2C                   ENDDO
183200950712     C*
183300950712     C                   Z-ADD     JI            PARJ                           *PARAMETRO
183400950712     C*
183500950712     C* IL RESTO DELLA SCHIERA A ZERO
183600950712DO  2C     JI            DOWLT     12
183700950712     C                   ADD       1             JI
183800950712     C                   Z-ADD     *ZEROS        ANN(JI)
183900950712     C                   Z-ADD     *ZEROS        MES(JI)
184000950712E   2C                   ENDDO
184100950712     C*
184200950712     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
184300950712X   1C                   ELSE
184400950712     C*
184500950712     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
184600950712DO  2C     WMM           DOWLE     12
184700950712     C                   ADD       1             JI
184800950712     C                   Z-ADD     WAA           ANN(JI)
184900950712     C                   Z-ADD     WMM           MES(JI)
185000950712     C                   ADD       1             WMM
185100950712E   2C                   ENDDO
185200950712     C*
185300950712     C* MEMORIZZO I MESI DELL'ANNO FINALE
185400950712     C                   Z-ADD     01            WMM
185500950712DO  2C     WMM           DOWLE     WMM2
185600950712     C                   ADD       1             JI
185700950712     C                   Z-ADD     WAA2          ANN(JI)
185800950712     C                   Z-ADD     WMM           MES(JI)
185900950712     C                   ADD       1             WMM
186000950712E   2C                   ENDDO
186100950712     C*
186200950712     C                   Z-ADD     JI            PARJ                           *PARAMETRO
186300950712     C*
186400950712     C* IL RESTO DELLA SCHIERA A ZERO
186500950712DO  2C     JI            DOWLT     12
186600950712     C                   ADD       1             JI
186700950712     C                   Z-ADD     *ZEROS        ANN(JI)
186800950712     C                   Z-ADD     *ZEROS        MES(JI)
186900950712E   2C                   ENDDO
187000950712E   1C                   ENDIF
187100950712     C*
187200950712     C                   ENDSR
187300000000     C*------------------------------------------------------------------------*
187400000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
187500000000     C*------------------------------------------------------------------------*
187600000000     C     *INZSR        BEGSR
187700950529     C*--------------------
187800950529     C* RICEVIMENTO PARAMETRI
187900950529     C*--------------------
188000000000     C     *ENTRY        PLIST
188100000000     C                   PARM                    KPJBA
188200000000     C                   MOVEL     KPJBU         PARAM
188300950529     C*--------------------
188400950529     C* DEFINIZIONE VARIABILI RIFERITE AL DATA BASE
188500950529     C*--------------------
188600000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
188700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
188800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
188900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
189000000000     C     *LIKE         DEFINE    ORGFL3        KORDIV
189100971204     C     *LIKE         DEFINE    SDRANN        KSDANN
189200000000     C     *LIKE         DEFINE    SDRMES        KSDMES
189300000000     C     *LIKE         DEFINE    ORGFIL        KSDFIL
189400000000     C     *LIKE         DEFINE    ORGFIL        KSDFIT
189500000000     C     *LIKE         DEFINE    TBGFIL        KBDFIL
189600000000     C     *LIKE         DEFINE    TBGANN        KBDANN
189700000000     C     *LIKE         DEFINE    TBGMES        KBDMES
189800960108     C     *LIKE         DEFINE    TBGIOE        KBDIOE
189900000703     C     *LIKE         DEFINE    PCAFLA        KCAFLA                         *SIPCA00F
190000000703     C     *LIKE         DEFINE    PCAIOE        KCAIOE
190100000703     C     *LIKE         DEFINE    PCAFPE        KCAFPE
190200000703     C     *LIKE         DEFINE    PCANTP        KCANTP
190300000703     C     *LIKE         DEFINE    PCANTA        KCANTA
190400000703     C     *LIKE         DEFINE    PCPFLP        KCPFLP                         *SIPCA00F
190500000703     C     *LIKE         DEFINE    PCPIOE        KCPIOE
190600000703     C     *LIKE         DEFINE    PCPFPE        KCPFPE
190700000703     C     *LIKE         DEFINE    PCPNTA        KCPNTA
190800000703     C     *LIKE         DEFINE    PCPNTP        KCPNTP
190900971204     C     *LIKE         DEFINE    ORGFL3        DEPDIV                         *DEPOSITI
191000971204     C     *LIKE         DEFINE    ORGCAR        DEPARE
191100971204     C     *LIKE         DEFINE    ORGFIL        DEPFIL
191200971204     C     *LIKE         DEFINE    ORGFL1        DLPFL1
191300971204     C     *LIKE         DEFINE    ORGFL1        DLAFL1
191400000703     C     *LIKE         DEFINE    PCANTP        DEPNTF
191500000703     C     *LIKE         DEFINE    PCANTP        DEPNTT
191600950529     C     *LIKE         DEFINE    TBGNSP        LAVSPE                         *DI LAVORO
191700000000     C     *LIKE         DEFINE    TBGKGP        LAVCHI
191800000000     C     *LIKE         DEFINE    TBGNCP        LAVCOL
191900000000     C     *LIKE         DEFINE    TBGVOP        LAVVOL
192000000703     C     *LIKE         DEFINE    PCANTP        LAVNTX
192100950529     C*--------------------
192200950529     C* CHIAVI DI LETTURA
192300950529     C*--------------------
192400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
192500000000     C     KEYTAB        KLIST
192600000000     C                   KFLD                    KTBUTE
192700000000     C                   KFLD                    KTBCOD
192800000000     C                   KFLD                    KTBKEY
192900000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
193000000000     C     KEYTA2        KLIST
193100000000     C                   KFLD                    KTBUTE
193200000000     C                   KFLD                    KTBCOD
193300000000     C* CHIAVE LETTURA AZORG02L - PARZIALE
193400000000     C     KEYORG        KLIST
193500000000     C                   KFLD                    KORDIV                         *DIVISIONE
193600000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
193700000000     C     KEYOR2        KLIST
193800000000     C                   KFLD                    KORFIL                         *FILIALE
193900990609     C* CHIAVE LETTURA SISDR01L/SISDR02L - TOTALE
194000000000     C     KEYSDR        KLIST
194100000000     C                   KFLD                    KSDANN                         *ANNO
194200000000     C                   KFLD                    KSDFIL                         *FILIALE
194300000000     C                   KFLD                    KSDFIT                         *FILIALE DETTAGLIO
194400000000     C                   KFLD                    KSDMES                         *MESE
194500990609     C* CHIAVE LETTURA SISDR01L/SISDR02L - PARZIALE
194600000000     C     KEYSD2        KLIST
194700000000     C                   KFLD                    KSDANN                         *ANNO
194800000000     C                   KFLD                    KSDFIL                         *FILIALE
194900000000     C                   KFLD                    KSDFIT                         *FILIALE DETTAGLIO
195000020114     C* CHIAVE LETTURA TEBDG01L - PARZIALE
195100000000     C     KEYBDG        KLIST
195200000000     C                   KFLD                    KBDFIL                         *FILIALE
195300000000     C                   KFLD                    KBDANN                         *ANNO
195400000000     C                   KFLD                    KBDMES                         *MESE
195500960108     C                   KFLD                    KBDIOE                         *DATI BUDGET
195600000703     C* CHIAVE LETTURA SIPCA01L - COMPLETA
195700100413     C     KEYPCA1       KLIST
195800100413     C                   KFLD                    Ksdann                         *anno
195900000703     C                   KFLD                    KCAFLA                         *FILIALE ARRIVO
196000000703     C                   KFLD                    KCAIOE                         *DATI STATISTICA
196100100413     C                   KFLD                    KCAtsp                         *tipo servizio
196200000703     C                   KFLD                    KCAFPE                         *FASCIA DI PESO
196300000703     C                   KFLD                    KCANTP                         *NETWORK PARTENZA
196400000703     C                   KFLD                    KCANTA                         *NETWORK ARRIVO
196500100413     C     KEYPCA        KLIST
196600100413     C                   KFLD                    KCAFLA                         *FILIALE ARRIVO
196700100413     C                   KFLD                    KCAIOE                         *DATI STATISTICA
196800100413     C                   KFLD                    KCAFPE                         *FASCIA DI PESO
196900100413     C                   KFLD                    KCANTP                         *NETWORK PARTENZA
197000100413     C                   KFLD                    KCANTA                         *NETWORK ARRIVO
197100000703     C* CHIAVE LETTURA SIPCP01L - COMPLETA
197200000703     C     KEYPCP        KLIST
197300000703     C                   KFLD                    KCPFLP                         *FILIALE PARTENZA
197400000703     C                   KFLD                    KCPIOE                         *DATI STATISTICA
197500000703     C                   KFLD                    KCPFPE                         *FASCIA DI PESO
197600000703     C                   KFLD                    KCPNTA                         *NETWORK ARRIVO
197700000703     C                   KFLD                    KCPNTP                         *NETWORK PARTENZA
197800100413     C     KEYPCP1       KLIST
197900100413     C                   KFLD                    Ksdann                         *anno
198000100413     C                   KFLD                    KCPFLP                         *FILIALE PARTENZA
198100100413     C                   KFLD                    KCPIOE                         *DATI STATISTICA
198200100413     C                   KFLD                    KCPtsp                         *tipo servizio
198300100413     C                   KFLD                    KCPFPE                         *FASCIA DI PESO
198400100413     C                   KFLD                    KCPNTA                         *NETWORK ARRIVO
198500100413     C                   KFLD                    KCPNTP                         *NETWORK PARTENZA
198600950529     C*--------------------
198700950529     C* VARIABILI DI CONTROLLO
198800950529     C*--------------------
198900000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
199000000000     C                   MOVEL     '0'           $FIN1             1            *FINE SALDI ANNO CORR
199100000000     C                   MOVEL     '0'           $FIN2             1            *FINE SALDI ANNO PREC
199200000000     C                   MOVEL     '0'           $FIN3             1            *FINE VALORI BUDGET
199300000000     C                   MOVEL     '0'           PASDET            1            *MEMOR SALDI FIL DET
199400000000     C                   MOVEL     '0'           PASFIL            1            *MEMOR SALDI FILIALE
199500000000     C                   MOVEL     '0'           PASARE            1            *MEMOR SALDI AREA
199600000000     C                   MOVEL     '0'           PASDIV            1            *MEMOR SALDI DIVISION
199700000000     C                   MOVEL     '0'           PASGEN            1            *MEMOR SALDI GENERALI
199800000000     C                   MOVEL     '0'           NOBDG             1            *'1'-NO BDG; '0' SI
199900000000     C                   MOVEL     ' '           TIPANN            1            TIPO ANNI  (U-UGUALI)
200000950529     C                   Z-ADD     UDATE         DATA              6 0          *ATA
200100950529     C                   TIME                    ORA               6 0          *RA
200200000000     C*
200300000000     C* MEMORIZZA I CODICI DELLE FILIALI E LE LORO DECODIFICHE
200400000000     C                   EXSR      CARORG
200500000000     C*
200600000000     C* CANCELLA E APRE IL FILE PC
200700000000IF  1C     PARFPC        IFEQ      'S'
200800000000     C                   EXSR      CANFPC
200900000000E   1C                   ENDIF
201000950908     C*
201100950908     C* CALCOLA I MESI DA ELABORARE
201200990527     C                   Z-ADD     ANNFIN        WAA2              4 0          *ANNO FINALE LAVORO
201300990527     C                   Z-ADD     MESFIN        WMM2              2 0          *MESE FINALE LAVORO
201400990527     C                   Z-ADD     ANNINI        WAA               4 0          *ANNO INIZIALE LAVORO
201500990527     C                   Z-ADD     MESINI        WMM               2 0          *MESE INIZIALE LAVORO
201600990527     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORARE
201700990527     C                   EXSR      AMELAB
201800000000     C*
201900000000     C* CARICA TABELLE
202000000000     C                   EXSR      CARTAB
202100000000     C*
202200000000     C* RIEMPO SCHIERA CON ANNO INIZIALE E ANNO FINALE PER LETTURA SALDI CORR.
202300000000     C                   Z-ADD     ANNINI        AIF(1)                         *ANNO INIZIALE
202400000000     C                   Z-ADD     ANNFIN        AIF(2)                         *ANNO FINALE
202500000000     C*
202600000000     C* E SCHIERA CON ANNO INIZIALE E ANNO FINALE - UNO, PER LETTURA SALDI PREC.
202700000000     C     ANNINI        SUB       1             PIF(1)                         *ANNO INIZIALE - 1
202800000000     C     ANNFIN        SUB       1             PIF(2)                         *ANNO FINALE - 1
202900000000     C*
203000000000     C* SE AA INIZIALE =  AA FINALE, QUEST'ULTIMO IN SCHIERA LO IMPOSTO A ZERO
203100000000IF  1C     ANNINI        IFEQ      ANNFIN
203200000000     C                   Z-ADD     *ZEROS        AIF(2)                         *ANNO FINALE
203300000000     C                   Z-ADD     *ZEROS        PIF(2)                         *ANNO FINALE - 1
203400000000     C                   MOVEL     'U'           TIPANN                         *ANNI UGUALI
203500000000E   1C                   ENDIF
203600000000     C*
203700000000     C* A P E R T U R A   F I L E
203800000000     C*
203900000000IF  1C     PARMPA        IFEQ      'P'
204000990609     C                   OPEN      SISDR01L                                     *SALDI PARTENZE
204100000000X   1C                   ELSE
204200990609     C                   OPEN      SISDR02L                                     *SALDI ARRIVI
204300000000E   1C                   ENDIF
204400000000     C*
204500000000     C                   ENDSR
204600000000     O*------------------------------------------------------------------------*
204700000703**   CMD1
204800000703CLRPFM FILE(*LIBL/SIPCP00F)
204900000703**   CMD2
205000000703CLRPFM FILE(*LIBL/SIPCA00F)
