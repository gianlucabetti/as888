000100010718     H*PARMS TGTRLS(*CURRENT)
000200000000     H*------------------------------------------------------------------------*
000300950525     H* STATISTICA RICAVI - ESEGUE                                             *
000400000000     H*------------------------------------------------------------------------*
000500990531     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900990609     FSISDR01L  IF   E           K DISK
001000990609     FSISDR02L  IF   E           K DISK
001100990609     F                                     RENAME(SISDR000:SISDR4)
001200010718     FSEPCR01L  UF A E           K DISK    USROPN
001300010718     FTEBDG01L  IF   E           K DISK
001400000000     FAZORG01L  IF   E           K DISK
001500000000     FAZORG02L  IF   E           K DISK
001600000000     F                                     RENAME(AZORG:AZORG2)
001700000000     FTABEL00F  IF   E           K DISK
001800010511     FTISE56P   O    E             PRINTER
001900000000     D*------------------------------------------------------------------------*
002000000000     D* SCHIERE
002100000000     D*------------------------------------------------------------------------*
002200000000     D*------------------
002300000000     D* CODICI/DESCRIZIONI FILIALI
002400000000     D*------------------
002500971124     D FIL             S              3  0 DIM(500)
002600971124     D DFIL            S             20    DIM(500)
002700971204     D FL1             S              1    DIM(500)
002800020325     D NTX             S              3    DIM(500)                             *NETWORK
002900000000     D*------------------
003000000000     D* DECODIFICHE FASCE DI PESO
003100000000     D*------------------
003200020522     D FPE             S             14    DIM(11)
003300000000     D*------------------
003400000000     D* MESI DA ELABORARE
003500000000     D*------------------
003600000000     D MES             S              2  0 DIM(12)
003700000000     D*------------------
003800000000     D* ANNI DEI MESI DA ELABORARE
003900000000     D*------------------
004000990531     D ANN             S              4  0 DIM(12)
004100000000     D*------------------
004200000000     D* ANNO INIZIALE E ANNO FINALE ELABORAZIONE SALDI CORRENTI
004300000000     D*------------------
004400990531     D AIF             S              4  0 DIM(2)
004500000000     D*------------------
004600000000     D* ANNO INIZIALE E ANNO FINALE ELABORAZIONE SALDI PRECEDENTI
004700000000     D*------------------
004800990531     D PIF             S              4  0 DIM(2)
004900000000     D*------------------
005000000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
005100000000     D*------------------
005200000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
005300000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/N)
005400000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
005500000000     D*------------------
005600000000     D* DECODIFICA PARAMETRI FILTRO
005700000000     D*------------------
005800960105     D PAR             S             15    DIM(15) CTDATA PERRCD(1)
005900000000     D*------------------
006000000000     D* DECODIFICA MESI DELL'ANNO
006100000000     D*------------------
006200000000     D DMM             S              9    DIM(12) CTDATA PERRCD(1)
006300000000     D*------------------
006400000000     D* DECODIFICA RIGHE DI STAMPA
006500000000     D*------------------
006600000000     D DRI             S             14    DIM(9) CTDATA PERRCD(1)
006700000000     D*------------------
006800000000     D* DECODIFICA CODICE DIVISIONE
006900000000     D*------------------
007000980206     D*---                DIV     1  10 20
007100000000     D*------------------
007200000000     D* DESCRIZIONE COMANDI 'QCMDEXC'
007300000000     D*------------------
007400000000     D CMD1            S             45    DIM(1) CTDATA PERRCD(1)
007500000000     D*------------------
007600020522     D* SCHIERA MULTIPLA PER TOTALI GENERALI           - 11 FASCE -
007700000000     D*------------------
007800000000     D*------------------
007900020522     D* SCHIERA MULTIPLA PER TOTALI DIVISIONE          - 11 FASCE -
008000000000     D*------------------
008100000000     D*------------------
008200020522     D* SCHIERA MULTIPLA PER TOTALI AREA               - 11 FASCE -
008300000000     D*------------------
008400000000     D*------------------
008500020522     D* SCHIERA MULTIPLA PER TOTALI FILIALE            - 11 FASCE -
008600000000     D*------------------
008700000000     D*------------------
008800020522     D* SCHIERA MULTIPLA PER TOTALI FILIALE DETTAGLIO  - 11 FASCE -
008900000000     D*------------------
009000000000     D*------------------
009100000000     D* SCHIERA MULTIPLA PER STAMPA TOTALI
009200000000     D*------------------
009300000000     D*------------------
009400000000     D* SCHIERE DI LAVORO PER TOTALI A FINE PAGINA
009500000000     D*------------------
009600010511     D RFW             S             19  3 DIM(12)                              RICAVI FRANCO
009700000000     D SFW             S             15  0 DIM(12)                              SPEDIZIONI FRANCO
009800010511     D RAW             S             19  3 DIM(12)                              RICAVI ASSEGNATO
009900000000     D SAW             S             15  0 DIM(12)                              SPEDIZIONI ASSEGNATO
010000010511     D RBW             S             19  3 DIM(12)                              RICAVI BUDGET
010100010511     D RPW             S             19  3 DIM(12)                              RICAVI ANNO PRECEDENTE
010200000000     D SOW             S             15  0 DIM(12)                              SPEDIZ BUDGET PARTENZA
010300000000     D SPW             S             15  0 DIM(12)                              SPEDIZIONI ANNO PRECED
010400000000     D SXW             S             15  0 DIM(12)                              SPED FRANCO ARRIVO
010500000000     D SYW             S             15  0 DIM(12)                              SPED ASSEGNATO PARTENZA
010600000000     D SIW             S             15  0 DIM(12)                              SPEDIZ BUDGET ARRIVO
010700000000     D*------------------
010800000000     D* SCHIERE DI LAVORO
010900000000     D*------------------
011000010515     D $VA             S             19  5 DIM(12)
011100010515     D $VM             S             19  5 DIM(12)
011200010515     D $VR             S             19  5 DIM(12)
011300010515     D $RT             S             19  5 DIM(12)                              RICAVI TOTALI/MEDI
011400000000     D $ST             S             15  0 DIM(12)                              SPEDIZIONI TOTALI
011500000000     D $SB             S             15  0 DIM(12)                              SPEDIZIONI BUDGET
011600000000     D $CP             S              1    DIM(12)                              CONDIZIONI PERCENTUALI
011700000000     D $PR             S              4  1 DIM(12)                              PERCENTUALI
011800000000     D*------------------
011900000000     D* TABELLE CAPICONTO
012000000000     D*------------------
012100000000     D*------------------------------------------------------------------------*
012200000000     D* INPUT
012300000000     D*------------------------------------------------------------------------*
012400000000     D KPJBA         E DS
012500000000     D  PROFIL                 1     10                                         UTENTE
012600000000     D*------------------
012700000000     D* NUMERO DEI BYTE IN PACKED:
012800000000     D*------------------
012900010511     D* RICAVI       (17:2) + 1 =  9,5  /  (9 X 12) = 108
013000000000     D*------------------
013100020522     D* DS MULTIPLA FASCE DI PESO PER TOTALI GENERALI     - 11 FASCE X 12 MESI -
013200000000     D*------------------
013300020522     D FPGEN           DS                  OCCURS(11) INZ
013400010511     D  RFG                          19  3 DIM(12)                              R.F*12
013500010511     D  SFG                          15  0 DIM(12)                              S.F*12
013600010511     D  RAG                          19  3 DIM(12)                              R.A*12
013700010511     D  SAG                          15  0 DIM(12)                              S.A*12
013800010511     D  RBG                          19  3 DIM(12)                              R.B*12
013900010511     D  RPG                          19  3 DIM(12)                              R.P*12
014000010511     D  SOG                          15  0 DIM(12)                              SBP*12
014100010511     D  SPG                          15  0 DIM(12)                              S.P*12
014200010511     D  SXG                          15  0 DIM(12)                              A.P*12
014300010511     D  SYG                          15  0 DIM(12)                              F.A*12
014400010511     D  SIG                          15  0 DIM(12)                              SBA*12
014500000000     D*------------------
014600020522     D* DS MULTIPLA FASCE DI PESO PER DIVISIONE           - 11 FASCE X 12 MESI -
014700000000     D*------------------
014800020522     D FPDIV           DS                  OCCURS(11) INZ
014900010511     D  RFD                          19  3 DIM(12)                              R.F*12
015000010511     D  SFD                          15  0 DIM(12)                              S.F*12
015100010511     D  RAD                          19  3 DIM(12)                              R.A*12
015200010511     D  SAD                          15  0 DIM(12)                              S.A*12
015300010511     D  RBD                          19  3 DIM(12)                              R.B*12
015400010511     D  RPD                          19  3 DIM(12)                              R.P*12
015500010511     D  SOD                          15  0 DIM(12)                              SBP*12
015600010511     D  SPD                          15  0 DIM(12)                              S.P*12
015700010511     D  SXD                          15  0 DIM(12)                              A.P*12
015800010511     D  SYD                          15  0 DIM(12)                              F.A*12
015900010511     D  SID                          15  0 DIM(12)                              SBA*12
016000000000     D*------------------
016100020522     D* DS MULTIPLA FASCE DI PESO PER AREA                - 11 FASCE X 12 MESI -
016200000000     D*------------------
016300020522     D FPARE           DS                  OCCURS(11) INZ
016400010511     D  RFA                          19  3 DIM(12)                              R.F*12
016500010511     D  SFA                          15  0 DIM(12)                              S.F*12
016600010511     D  RAA                          19  3 DIM(12)                              R.A*12
016700010511     D  SAA                          15  0 DIM(12)                              S.A*12
016800010511     D  RBA                          19  3 DIM(12)                              R.B*12
016900010511     D  RPA                          19  3 DIM(12)                              R.P*12
017000010511     D  SOA                          15  0 DIM(12)                              SBP*12
017100010511     D  SPA                          15  0 DIM(12)                              S.P*12
017200010511     D  SXA                          15  0 DIM(12)                              A.P*12
017300010511     D  SYA                          15  0 DIM(12)                              F.A*12
017400010511     D  SIA                          15  0 DIM(12)                              SBA*12
017500000000     D*------------------
017600020522     D* DS MULTIPLA FASCE DI PESO PER FILIALE             - 11 FASCE X 12 MESI -
017700000000     D*------------------
017800020522     D FPFIL           DS                  OCCURS(11) INZ
017900010511     D  RFF                          19  3 DIM(12)                              R.F*12
018000010511     D  SFF                          15  0 DIM(12)                              S.F*12
018100010511     D  RAF                          19  3 DIM(12)                              R.A*12
018200010511     D  SAF                          15  0 DIM(12)                              S.A*12
018300010511     D  RBF                          19  3 DIM(12)                              R.B*12
018400010511     D  RPF                          19  3 DIM(12)                              R.P*12
018500010511     D  SOF                          15  0 DIM(12)                              SBP*12
018600010511     D  SPF                          15  0 DIM(12)                              S.P*12
018700010511     D  SXF                          15  0 DIM(12)                              A.P*12
018800010511     D  SYF                          15  0 DIM(12)                              F.A*12
018900010511     D  SIF                          15  0 DIM(12)                              SBA*12
019000000000     D*------------------
019100020522     D* DS MULTIPLA FASCE DI PESO PER FILIALE DETTAGLIO   - 11 FASCE X 12 MESI -
019200000000     D*------------------
019300020522     D FPDET           DS                  OCCURS(11) INZ
019400010511     D  RFT                          19  3 DIM(12)                              R.F*12
019500010511     D  SFT                          19  3 DIM(12)                              S.F*12
019600010511     D  RAT                          19  3 DIM(12)                              R.A*12
019700010511     D  SAT                          19  3 DIM(12)                              S.A*12
019800010511     D  RPT                          19  3 DIM(12)                              R.P*12
019900010511     D  SPT                          19  3 DIM(12)                              S.P*12
020000000000     D*------------------
020100020522     D* DS MULTIPLA FASCE DI PESO DA PORTARE IN STAMPA    - 11 FASCE X 12 MESI -
020200000000     D*------------------
020300020522     D FPSTA           DS                  OCCURS(11) INZ
020400010511     D  RFS                          19  3 DIM(12)                              R.F*12
020500010511     D  SFS                          15  0 DIM(12)                              S.F*12
020600010511     D  RAS                          19  3 DIM(12)                              R.A*12
020700010511     D  SAS                          15  0 DIM(12)                              S.A*12
020800010511     D  RBS                          19  3 DIM(12)                              R.B*12
020900010511     D  RPS                          19  3 DIM(12)                              R.P*12
021000010511     D  SOS                          15  0 DIM(12)                              SBP*12
021100010511     D  SPS                          15  0 DIM(12)                              S.P*12
021200010511     D  SXS                          15  0 DIM(12)                              A.P*12
021300010511     D  SYS                          15  0 DIM(12)                              F.A*12
021400010511     D  SIS                          15  0 DIM(12)                              SBA*12
021500000000     D*------------------
021600000000     D* PARAMETRI FILTRO
021700000000     D*------------------
021800000000     D PARAM           DS                  INZ
021900990531     D  PARDAT                 1      6  0
022000990531     D  ANNINI                 1      4  0
022100990531     D  MESINI                 5      6  0
022200990531     D  PARDA2                 7     12  0
022300990531     D  ANNFIN                 7     10  0
022400990531     D  MESFIN                11     12  0
022500990531     D  PARDET                13     13
022600990531     D  PARFIL                14     16  0
022700990531     D  PARARE                17     19  0
022800990531     D  PARDIV                20     20
022900990531     D  PARSER                21     21
023000990531     D  PARPRO                22     22
023100990531     D  PARRNC                23     23
023200990531     D  PARFPC                24     24
023300990531     D  PARIOE                25     25
023400000704     D  PARPOS                26     26
023500990531     D*------------------
023600010718     D* DS COMPOSIZIONE DATA(6) - TEBDG
023700990531     D*------------------
023800990531     D                 DS
023900990531     D  TBGANN                 1      4  0
024000990531     D  TBGMES                 5      6  0
024100990531     D  TBGDAT                 1      6  0
024200000000     D*------------------
024300000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
024400000000     D*------------------
024500000000     D DS2W          E DS
024600000217     D*---
024700000217     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
024800000217     D*---
024900000217     D OG143         E DS
025000020325     D*-------------------
025100020325     D* DS DEFINIZIONE TABELLA "NETWORK"
025200020325     D*-------------------
025300020325     D DNTW          E DS
025400020325     D*-------------------
025500020325     D* DS RICHIAMO SCELTA TABELLE
025600020325     D*-------------------
025700020325     D TIBS02        E DS                  EXTNAME(TIBS02DS)
025800090605
025900090605     d ds5e          e ds
026000000000     C*------------------------------------------------------------------------*
026100000000     C*?MAIN   - PROGRAMMA BATCH STAMPA STATISTICA RICAVI
026200000000     C*------------------------------------------------------------------------*
026300000000     C*
026400000000     C* STAMPA PARAMETRI FILTRO
026500000000     C                   EXSR      STAPAR
026600000000     C*
026700000000     C* POSIZIONAMENTO SUL FILE ORGANIGRAMMI E PRIMA LETTURA
026800000000     C                   EXSR      SETFIL
026900000000     C*
027000000000     C* CICLO FINO A FINE FILE ORGANIGRAMMI
027100000000DO  1C     $FINE         DOWEQ     '0'
027200000000     C*
027300000000     C* OPERAZIONI PER NUOVA DIVISIONE
027400000000     C                   EXSR      NEWDIV
027500000000     C*
027600000000     C* CICLO FINO A ROTTURA DI DIVISIONE
027700000000DO  2C     $FINE         DOWEQ     '0'
027800000000     C     ORGFL3        ANDEQ     DEPDIV
027900000000     C*
028000000000     C* OPERAZIONI PER NUOVA AREA
028100000000     C                   EXSR      NEWARE
028200000000     C*
028300000000     C* CICLO FINO A ROTTURA DI AREA
028400000000DO  3C     $FINE         DOWEQ     '0'
028500000000     C     ORGFL3        ANDEQ     DEPDIV
028600000000     C     ORGCAR        ANDEQ     DEPARE
028700000000     C*
028800000000     C* OPERAZIONI PER NUOVA FILIALE
028900000000     C                   EXSR      NEWFIL
029000000000     C*
029100000000     C* CICLO LETTURA FILIALI DETTAGLIO
029200971124DO  4C     1             DO        500           F                 3 0
029300000000IF  5C     FIL(F)        IFGT      *ZEROS
029400000000     C*
029500000000     C* OPERAZIONI PER NUOVA FILIALE DETTAGLIO
029600000000     C                   EXSR      NEWDET
029700000707     C*
029800000707     C* CONTROLLA VALIDITA' DEL DETTAGLIO
029900000707IF  6C     PARPOS        IFEQ      'S'                                          *DATI POSTE
030000020416     C     DEPNTF        ANDEQ     'PPT'                                         FILIALE POSTE
030100000707     C     PARPOS        OREQ      'S'                                          *DATI POSTE
030200020416     C     DEPNTT        ANDEQ     'PPT'                                         FILIALE DETTAGLIO POSTE
030300000707     C     PARPOS        OREQ      'N'                                          *NO DATI POSTE
030400020416     C     DEPNTF        ANDNE     'PPT'                                         NO FILIALE POSTE
030500020416     C     DEPNTT        ANDNE     'PPT'                                         NO FILIALE DETTAGLIO POSTE
030600000000     C*
030700000000     C* LETTURA SALDI AA INIZIALE, E AA FINALE SOLO SE DIVERSO DALL'INIZIALE
030800000000     C                   Z-ADD     1             J                 1 0
030900000707DO  7C                   DO        2             J
031000000000     C                   MOVEL     '0'           $FIN1                          *REIMPOSTO FINE FILE
031100000000     C*
031200000000     C* SECONDO GIRO SOLO SE ANNO FINALE DIVERSO DALL' INIZIALE (PIENO)
031300000707IF  8C     AIF(J)        IFNE      *ZEROS
031400000000     C*
031500000000     C* POSIZIONAMENTO SALDI PARTENZA ANNO CORRENTE E PRIMA LETTURA
031600000000     C                   MOVEL     'P'           CICLO                          *PARTENZA
031700000000     C                   EXSR      SETFI1
031800000000     C*
031900000000     C* CICLO FINO A FINE FILE SALDI O ROTTURA FILIALE
032000000707DO  9C     $FIN1         DOWEQ     '0'
032100000000     C*
032200000000     C* MEMORIZZA I FRANCHI CORRENTI DELLA FILIALE DETTAGLIO
032300000000     C                   EXSR      MEMSAL
032400000000     C*
032500000000     C* LETTURA RECORD SALDI SUCCESSIVO
032600000000     C                   EXSR      LETFI1
032700000000     C*
032800000707E   9C                   ENDDO
032900000000     C*
033000000000     C* POSIZIONAMENTO SALDI ARRIVO ANNO CORRENTE E PRIMA LETTURA
033100000000     C                   MOVEL     '0'           $FIN1
033200000000     C                   MOVEL     'A'           CICLO                          *ARRIVO
033300000000     C                   EXSR      SETFI1
033400000000     C*
033500000000     C* CICLO FINO A FINE FILE SALDI O ROTTURA FILIALE
033600000707DO  9C     $FIN1         DOWEQ     '0'
033700000000     C*
033800000000     C* MEMORIZZA GLI ASSEGNATI CORRENTI DELLA FILIALE DETTAGLIO
033900000000     C                   EXSR      MEMSAL
034000000000     C*
034100000000     C* LETTURA RECORD SALDI SUCCESSIVO
034200000000     C                   EXSR      LETFI1
034300000000     C*
034400000707E   9C                   ENDDO
034500000707E   8C                   ENDIF
034600000707E   7C                   ENDDO
034700000000     C*
034800000000     C* CALCOLA I SALDI DELL' ANNO PRECEDENTE DELLA FILIALE DETTAGLIO
034900000000     C                   EXSR      CALANP
035000000707     C*
035100000707     C* SCRIVE I SALDI MOVIMENTI DELLA FILIALE SUL FILE PC -NEW-
035200000707IF  7C     PARFPC        IFEQ      'S'
035300000707     C                   EXSR      SCRPCR                                       *SCRIVE FILE PC -NEW-
035400000707E   7C                   ENDIF
035500000000     C*
035600000000     C* SE RICHIESTO DETTAGLIO STAMPO FILIALE
035700000707IF  7C     PARDET        IFEQ      'S'                                          *SE MEMORIZZATI SALDI
035800000000     C     PASDET        ANDEQ     '1'
035900000000     C*
036000000000     C* IMPOSTA STAMPA TOTALI FILIALE DETTAGLIO
036100000000     C                   EXSR      STADET
036200000707E   7C                   ENDIF
036300000707E   6C                   ENDIF
036400000000     C*
036500000000E   5C                   ENDIF
036600000000E   4C                   ENDDO                                                  *FIL/DET SUCESSIVA
036700000000     C*
036800000000     C* CALCOLA I VALORI DEL BUDGET
036900000000IF  4C     PASFIL        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
037000000000     C                   EXSR      CALBDG
037100000000E   4C                   ENDIF
037200000000     C*
037300000000     C* IMPOSTA STAMPA TOTALI FILIALE
037400000000IF  4C     PASFIL        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
037500000000     C                   EXSR      STAFIL
037600000000E   4C                   ENDIF
037700000000     C*
037800000000     C* LETTURA RECORD ORGANIGRAMMI SUCCESSIVO
037900000000     C                   EXSR      LETFIL
038000000000     C*
038100000000E   3C                   ENDDO
038200000000     C*
038300000000     C* IMPOSTA STAMPA TOTALI AREA
038400000000IF  3C     PASARE        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
038500000000     C                   EXSR      STAARE
038600000000E   3C                   ENDIF
038700000000     C*
038800000000E   2C                   ENDDO
038900000000     C*
039000000000     C* IMPOSTA STAMPA TOTALI DIVISIONE
039100000000IF  2C     PASDIV        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
039200000000     C                   EXSR      STADIV
039300000000E   2C                   ENDIF
039400000000     C*
039500000000E   1C                   ENDDO
039600000000     C*
039700000000     C* IMPOSTA STAMPA TOTALI GENERALI
039800000000IF  1C     PASGEN        IFEQ      '1'                                          *SOLO SE MEMORIZZATI
039900000000     C                   EXSR      STAGEN
040000000000E   1C                   ENDIF
040100000000     C*
040200000000     C* STAMPA RIGA DI FINE PROSPETTO
040300010511     C                   WRITE     SE56FI
040400000000     C*
040500000000     C* CHIUSURA FILE PC
040600000000IF  1C     PARFPC        IFEQ      'S'
040700010718     C                   CLOSE     SEPCR01L
040800000000E   1C                   ENDIF
040900000000     C*
041000000000     C                   SETON                                        LR
041100000000     C*------------------------------------------------------------------------*
041200000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ORGANIGRAMMI
041300000000     C*------------------------------------------------------------------------*
041400000000     C     SETFIL        BEGSR
041500000000     C*
041600000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE ORGANIGRAMMI
041700000000     C                   MOVEL     PARDIV        KORDIV
041800000000     C     KEYORG        SETLL     AZORG02L                           99        *FINE ARCHIVIO
041900000000     C*
042000000000IF  1C     *IN99         IFEQ      '1'
042100000000     C                   MOVEL     '1'           $FINE
042200000000X   1C                   ELSE
042300000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
042400000000E   1C                   ENDIF
042500000000     C*
042600000000     C                   ENDSR
042700000000     C*------------------------------------------------------------------------*
042800000000     C*?LETFIL - LETTURA PROSSIMO RECORD FILE ORGANIGRAMMI
042900000000     C*------------------------------------------------------------------------*
043000000000     C     LETFIL        BEGSR
043100000000     C*
043200000000     C* LEGGO FINO A:
043300000000     C*     - FINE FILE
043400000000     C*     - TROVATO RECORD VALIDO
043500000000     C                   MOVEL     'N'           WRECOK
043600000000DO  1C     $FINE         DOWEQ     '0'
043700000000     C     WRECOK        ANDEQ     'N'
043800000000     C*
043900000000     C* LETTURE SUCCESSIVE ALLA PRIMA
044000000000     C* SE SELEZIONATA LA DIVISIONE LEGGO ANCHE PER UGUALE
044100000000IF  2C     PARDIV        IFGT      *BLANKS
044200000000     C     KEYORG        READE     AZORG02L                               99
044300000000X   2C                   ELSE
044400000000     C                   READ      AZORG02L                               99
044500000000E   2C                   ENDIF
044600000000     C*
044700000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
044800000000     C                   MOVEL     '1'           $FINE
044900000000X   2C                   ELSE
045000000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
045100000000E   2C                   ENDIF
045200000000E   1C                   ENDDO
045300000000     C*
045400000000     C                   ENDSR
045500000000     C*------------------------------------------------------------------------*
045600000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD ORGANIGRAMMA
045700000000     C*------------------------------------------------------------------------*
045800000000     C     CHKREC        BEGSR
045900000707     C*
046000000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
046100000707     C*
046200000707     C* IMPOSTAZIONI
046300000707     C                   CLEAR                   OG143
046400000707     C                   MOVEL     ORGDE3        OG143
046500000000     C*
046600950525     C* ESCLUDE LE FILIALI ANNULLATE
046700000000IF  1C     ORGFVA        IFNE      *BLANKS
046800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046900000000E   1C                   ENDIF
047000000000     C*
047100950525     C* ESCLUDE LE VOCI VARIE
047200000000IF  1C     ORGFAG        IFEQ      'V'
047300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047400000000E   1C                   ENDIF
047500000000     C*
047600950525     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDE
047700000000IF  1C     PARARE        IFNE      *ZEROS
047800000000     C     ORGCAR        ANDNE     PARARE
047900000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048000000000E   1C                   ENDIF
048100000000     C*
048200950525     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDE
048300000000IF  1C     PARFIL        IFNE      *ZEROS
048400000000     C     ORGFIL        ANDNE     PARFIL
048500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048600000000E   1C                   ENDIF
048700000000     C*
048800000000     C                   ENDSR
048900000000     C*------------------------------------------------------------------------*
049000000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
049100000000     C*------------------------------------------------------------------------*
049200000000     C     NEWDIV        BEGSR
049300000000     C*
049400000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
049500000000     C                   MOVEL     ORGFL3        DEPDIV
049600000000     C*
049700000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
049800000000     C                   EXSR      DECDIV
049900000000     C*
050000000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
050100000000     C                   MOVEL     '0'           PASDIV
050200000000     C*
050300000000     C* AZZERA I TOTALI DELLA DIVISIONE
050400000000DO  1C                   DO        10            A                 2 0
050500000000     C     A             OCCUR     FPDIV
050600000000     C                   CLEAR                   RFD
050700000000     C                   CLEAR                   SFD
050800000000     C                   CLEAR                   RAD
050900000000     C                   CLEAR                   SAD
051000000000     C                   CLEAR                   RBD
051100000000     C                   CLEAR                   RPD
051200000000     C                   CLEAR                   SOD
051300000000     C                   CLEAR                   SPD
051400000000     C                   CLEAR                   SXD
051500000000     C                   CLEAR                   SYD
051600000000     C                   CLEAR                   SID
051700000000E   1C                   ENDDO
051800000000     C*
051900000000     C                   ENDSR
052000000000     C*------------------------------------------------------------------------*
052100000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
052200000000     C*------------------------------------------------------------------------*
052300000000     C     NEWARE        BEGSR
052400000000     C*
052500000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
052600000000     C                   MOVEL     ORGCAR        DEPARE
052700000000     C*
052800000000     C* DECODIFICA IL CODICE AREA
052900000000     C                   EXSR      DECARE
053000000000     C*
053100000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
053200000000     C                   MOVEL     '0'           PASARE
053300000000     C*
053400000000     C* AZZERA I TOTALI DELL'AREA
053500000000DO  1C                   DO        10            A
053600000000     C     A             OCCUR     FPARE
053700000000     C                   CLEAR                   RFA
053800000000     C                   CLEAR                   SFA
053900000000     C                   CLEAR                   RAA
054000000000     C                   CLEAR                   SAA
054100000000     C                   CLEAR                   RBA
054200000000     C                   CLEAR                   RPA
054300000000     C                   CLEAR                   SOA
054400000000     C                   CLEAR                   SPA
054500000000     C                   CLEAR                   SXA
054600000000     C                   CLEAR                   SYA
054700000000     C                   CLEAR                   SIA
054800000000E   1C                   ENDDO
054900000000     C*
055000000000     C                   ENDSR
055100000000     C*------------------------------------------------------------------------*
055200000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA DEI SALDI ANNO CORRENTE
055300000000     C*------------------------------------------------------------------------*
055400000000     C     SETFI1        BEGSR
055500000000     C*
055600990609     C* LEGGO 'SISDR01L' PER ELABORARE I FRANCHI, E 'SISDR02L' PER GLI ASSEGNATI
055700000000     C                   Z-ADD     AIF(J)        KSDANN                         *AA INIZIALE O FINALE
055800000000     C                   Z-ADD     ORGFIL        KSDFIL                         *FILIALE
055900000000     C                   Z-ADD     FIL(F)        KSDFIT                         *FILIALE DETTAGLIO
056000000000     C*
056100000000     C* MI POSIZIONO CON IL MESE INIZIALE SE ANNI UGUALI O PRIMO ANNO DIVERSO,
056200000000     C* OPPURE CON IL MESE = 01 SE GLI ANNI SONO DIVERSI E STO ELABORANDO IL 2°
056300000000IF  1C     TIPANN        IFEQ      'U'
056400000000     C     J             OREQ      1
056500000000     C                   Z-ADD     MESINI        KSDMES                         *MESE INIZIALE
056600000000E   1C                   ELSE
056700000000     C                   Z-ADD     01            KSDMES                         *MESE 01
056800000000E   1C                   ENDIF
056900000000     C*
057000000000     C* POSIZIONAMENTO FILE SALDI
057100000000IF  1C     CICLO         IFEQ      'P'                                          *CICLO PARTENZA
057200990609     C     KEYSDR        SETLL     SISDR01L                           99        *FINE ARCHIVIO
057300000000X   1C                   ELSE                                                   *CICLO ARRIVO
057400990609     C     KEYSDR        SETLL     SISDR02L                           99        *FINE ARCHIVIO
057500000000E   1C                   ENDIF
057600000000     C*
057700000000IF  1C     *IN99         IFEQ      '1'
057800000000     C                   MOVEL     '1'           $FIN1
057900000000X   1C                   ELSE
058000000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
058100000000E   1C                   ENDIF
058200000000     C*
058300000000     C                   ENDSR
058400000000     C*------------------------------------------------------------------------*
058500000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI ANNO CORRENTE
058600000000     C*------------------------------------------------------------------------*
058700000000     C     LETFI1        BEGSR
058800000000     C*
058900000000     C* LEGGO FINO A:
059000000000     C*     - FINE FILE
059100000000     C*     - TROVATO RECORD VALIDO
059200000000     C                   MOVEL     'N'           WRECOK
059300000000DO  1C     $FIN1         DOWEQ     '0'
059400000000     C     WRECOK        ANDEQ     'N'
059500000000     C*
059600000000     C* LEGGO PER: ANNO + FILIALE + FILIALE-DETTAGLIO
059700000000IF  1C     CICLO         IFEQ      'P'                                          *CICLO PARTENZA
059800990609     C     KEYSD2        READE     SISDR01L                               99
059900000000X   1C                   ELSE                                                   *CICLO ARRIVO
060000990609     C     KEYSD2        READE     SISDR02L                               99
060100000000E   1C                   ENDIF
060200000000     C*
060300000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
060400000000     C                   MOVEL     '1'           $FIN1
060500000000X   2C                   ELSE
060600000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
060700000000E   2C                   ENDIF
060800000000     C*
060900000000E   1C                   ENDDO
061000000000     C*
061100000000     C                   ENDSR
061200000000     C*------------------------------------------------------------------------*
061300000000     C*?CHKRE1 - CONTROLLA VALIDITA' DEI SALDI ANNO CORRENTE/PRECEDENTE
061400000000     C*------------------------------------------------------------------------*
061500000000     C     CHKRE1        BEGSR
061600000000     C*
061700000000     C* °ANNO INIZIALE = ANNO FINALE: IL MESE LETTO DEVE ESSERE MINORE/UGUALE
061800000000     C*  DEL MESE FINALE RICHIESTO A VIDEO
061900000000     C*  ANNO INIZIALE <> ANNO FINALE: IL MESE DELL'ANNO INIZIALE DEVE ESSERE
062000000000     C*  MIN/UG DI 12; IL MESE DELL'ANNO FINALE MIN/UG DEL MESE FINALE RICHIESTO
062100000000     C*
062200000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
062300000000     C*
062400010511     C* CONTROLLO MESE DEL RECORD SALDI LETTO
062500000000IF  1C     TIPANN        IFEQ      'U'                                          *ANNI UGUALI
062600000000     C     J             OREQ      2                                            *ANNI DIVERSI MA 2°
062700000000     C*
062800000000IF  2C     SDRMES        IFGT      MESFIN
062900000000     C                   MOVEL     '1'           $FIN1                          *ESCO DAL CICLO CORR.
063000000000     C                   MOVEL     '1'           $FIN2                          *ESCO DAL CICLO PREC.
063100000000E   2C                   ENDIF
063200000000E   1C                   ENDIF
063300000000     C*
063400000000     C* SE TIPO SERVIZIO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
063500000000IF  1C     PARSER        IFNE      *BLANKS
063600000000IF  2C     PARSER        IFNE      SDRTSP
063700000000     C                   MOVEL     'N'           WRECOK
063800000000E   2C                   ENDIF
063900000000E   1C                   ENDIF
064000000000     C*
064100000000     C* ESCLUDO LE BOLLE CHE NON SONO VALIDE SIA COME SPEDIZIONE CHE COME RICAVO
064200000000     C                   Z-ADD     1             G
064300000000     C                   SETOFF                                           99
064400000000     C     SDRRBL        LOOKUP    FLAB(G)                                99
064500000000IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
064600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
064700000000X   1C                   ELSE
064800000000IF  2C     FSP(G)        IFNE      'S'                                          BOLLA CON SPED.VALIDE
064900000000IF  2C     FRI(G)        ANDNE     'S'                                          BOLLA CON RIC .VALIDI
065000000000     C                   MOVEL     'N'           WRECOK
065100000000E   2C                   ENDIF
065200000000E   1C                   ENDIF
065300000000     C*
065400000000     C* SE TIPO PRODOTTO DIVERSO DA QUELLO RICHIESTO, ESCLUDO
065500000000IF  1C     PARPRO        IFNE      *BLANKS
065600000000IF  2C     SDRTPR        IFNE      PARPRO
065700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
065800000000E   2C                   ENDIF
065900000000E   1C                   ENDIF
066000971204     C*
066100971204     C* REPERISCE IL FLAG ITALIA / ESTERO
066200971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
066300971204     C                   MOVEL     *BLANKS       DLPFL1
066400971204     C                   MOVEL     *BLANKS       DLAFL1
066500971204     C                   Z-ADD     1             I
066600971204     C     SDRFLP        LOOKUP    FIL(I)                                 99
066700971204IF  2C     *IN99         IFEQ      *ON                                          *TROVATO
066800971204     C                   MOVEL     FL1(I)        DLPFL1
066900971204E   2C                   ENDIF
067000971204     C                   Z-ADD     1             I
067100971204     C     SDRFLA        LOOKUP    FIL(I)                                 99
067200971204IF  2C     *IN99         IFEQ      *ON                                          *TROVATO
067300971204     C                   MOVEL     FL1(I)        DLAFL1
067400971204E   2C                   ENDIF
067500971204E   1C                   ENDIF
067600950525     C*
067700960105     C* SE RICHIESTA STATISTICA ITALIA, ESCLUDE LE BOLLE ESTERO (ITA->ITA)
067800971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
067900971204IF  2C     PARIOE        IFEQ      'I'
068000971204IF  3C     DLPFL1        IFNE      *BLANKS
068100971204     C     DLAFL1        ORNE      *BLANKS
068200950525     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
068300971204E   3C                   ENDIF
068400971204E   2C                   ENDIF
068500971204E   1C                   ENDIF
068600960105     C*
068700960105     C* SE RICHIESTA STATISTICA IMPORT, ESCLUDE BOLLE CON LNP ITALIA (EST->ITA)
068800971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
068900971204IF  2C     PARIOE        IFEQ      'M'
069000971204IF  3C     DLPFL1        IFEQ      *BLANKS
069100960105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
069200971204E   3C                   ENDIF
069300971204E   2C                   ENDIF
069400971204E   1C                   ENDIF
069500960105     C*
069600960105     C* SE RICHIESTA STATISTICA EXPORT, ESCLUDE BOLLE CON LNA ITALIA (ITA->EST)
069700971204IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
069800971204IF  2C     PARIOE        IFEQ      'X'
069900971204IF  3C     DLAFL1        IFEQ      *BLANKS
070000960105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
070100971204E   3C                   ENDIF
070200971204E   2C                   ENDIF
070300971204E   1C                   ENDIF
070400000000     C*
070500000000     C                   ENDSR
070600000000     C*------------------------------------------------------------------------*
070700000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
070800000000     C*------------------------------------------------------------------------*
070900000000     C     NEWFIL        BEGSR
071000000000     C*
071100000000     C* REIMPOSTO FINE FILE SALDI CORRENTI/PRECEDENTI/BUDGET
071200000000     C                   MOVEL     '0'           $FIN1                          FINE SALDI CORRENTI
071300000000     C                   MOVEL     '0'           $FIN2                          FINE SALDI PRECEDENTI
071400000000     C                   MOVEL     '0'           $FIN3                          FINE VALORI BUDGET
071500000000     C*
071600000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI E BUDGET
071700000000     C                   MOVEL     '0'           PASFIL                         *SALDI
071800000000     C                   MOVEL     '0'           NOBDG                          *BUDGET
071900000000     C*
072000000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
072100000000     C                   Z-ADD     ORGFIL        DEPFIL
072200000000     C*
072300000000     C* DECODIFICA IL CODICE FILIALE
072400000707     C                   MOVEL     *BLANKS       LAVNTX
072500000000     C                   EXSR      DECFIL
072600000707     C*
072700000707     C* MEMORIZZA IL NETWORK DELLA NUOVA FILIALE
072800000707     C                   MOVEL     LAVNTX        DEPNTF                         *NETWORK
072900000000     C*
073000000000     C* AZZERA I TOTALI DELLA FILIALE
073100000000DO  1C                   DO        10            A
073200000000     C     A             OCCUR     FPFIL
073300000000     C                   CLEAR                   RFF
073400000000     C                   CLEAR                   SFF
073500000000     C                   CLEAR                   RAF
073600000000     C                   CLEAR                   SAF
073700000000     C                   CLEAR                   RBF
073800000000     C                   CLEAR                   RPF
073900000000     C                   CLEAR                   SOF
074000000000     C                   CLEAR                   SPF
074100000000     C                   CLEAR                   SXF
074200000000     C                   CLEAR                   SYF
074300000000     C                   CLEAR                   SIF
074400000000E   1C                   ENDDO
074500000000     C*
074600000000     C                   ENDSR
074700000000     C*------------------------------------------------------------------------*
074800000000     C*?NEWDET - OPERAZIONI PER NUOVA FILIALE DETTAGLIO
074900000000     C*------------------------------------------------------------------------*
075000000000     C     NEWDET        BEGSR
075100000000     C*
075200000000     C* REIMPOSTO FINE FILE SALDI CORRENTI/PRECEDENTI
075300000000     C                   MOVEL     '0'           $FIN1                          FINE SALDI CORRENTI
075400000000     C                   MOVEL     '0'           $FIN2                          FINE SALDI PRECEDENTI
075500000000     C*
075600000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI FILIALE DETTAGLIO
075700000000     C                   MOVEL     '0'           PASDET
075800000707     C*
075900000707     C* MEMORIZZA IL NETWORK DELLA NUOVA FILIALE DETTAGLIO
076000000707     C                   MOVEL     NTX(F)        DEPNTT                         *NETWORK
076100000000     C*
076200000000     C* AZZERA I TOTALI DELLA FILIALE
076300000000DO  1C                   DO        10            A
076400000000     C     A             OCCUR     FPDET
076500000000     C                   CLEAR                   RFT
076600000000     C                   CLEAR                   SFT
076700000000     C                   CLEAR                   RAT
076800000000     C                   CLEAR                   SAT
076900000000     C                   CLEAR                   RPT
077000000000     C                   CLEAR                   SPT
077100000000E   1C                   ENDDO
077200000000     C*
077300000000     C                   ENDSR
077400000000     C*------------------------------------------------------------------------*
077500010511     C*?MEMSAL - MEMORIZZA I SALDI CORRENTI
077600000000     C*------------------------------------------------------------------------*
077700000000     C     MEMSAL        BEGSR
077800000000     C*
077900000000     C* ADDIZIONO LE NOTE DI CREDITO, INVECE DI SOTTRARLE, PERCHE' NEL FILE
078000000000     C* SONO MEMORIZZATE CON IL SEGNO MENO
078100000000     C*
078200000000     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
078300000000     C* PER IL CONTEGGIO IN STAMPA DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
078400000000     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
078500000000     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
078600000000     C                   Z-ADD     1             JI                4 0
078700000000     C                   SETOFF                                           28
078800000000     C     SDRRBL        LOOKUP    FLAB(JI)                               28
078900000000     C* BOLLA NON VALIDA COME N°SPEDIZIONE
079000000000IF  1C     FSP(JI)       IFNE      'S'
079100000000     C                   Z-ADD     *ZEROS        SDRNSP                         *NUMERO SPEDIZIONI
079200000000     C                   Z-ADD     *ZEROS        SDRPKG                         *PESO
079300000000     C                   Z-ADD     *ZEROS        SDRNCO                         *NUMERO COLLI
079400000000     C                   Z-ADD     *ZEROS        SDRVOL                         *VOLUME TOTALE
079500000000     C                   Z-ADD     *ZEROS        SDRPTS                         *ECCEDENZA PESO TASS.
079600000000E   1C                   ENDIF
079700000000     C* BOLLA NON VALIDA COME RICAVO
079800000000IF  1C     FRI(JI)       IFNE      'S'
079900000000     C                   Z-ADD     *ZEROS        SDRIRB                         RICAVO REALE BOLLE
080000000000     C                   Z-ADD     *ZEROS        SDRIRR                         RETTIFICHE
080100000000     C                   Z-ADD     *ZEROS        SDRINC                         NOTE ACCREDITO
080200000000E   1C                   ENDIF
080300000000     C*
080400000000     C* R I C A V O = (RICAVI DA BOLLE + RICAVI VARI + NOTE DI CREDITO)°°
080500000000     C*
080600010515     C                   Z-ADD     *ZEROS        RICAVI           19 5
080700000000     C                   ADD       SDRIRB        RICAVI                         *RICAVI DA BOLLE
080800000000     C     RICAVI        ADD       SDRIRR        RICAVI                         *RICAVI VARI
080900000000     C     RICAVI        ADD       SDRINC        RICAVI                         *NOTE DI CREDITO
081000000000     C*--------------------
081100000000     C* F A S C I A
081200000000     C*--------------------
081300000000     C                   MOVEL     SDRFPE        FASCIA            2 0
081400000000     C                   Z-ADD     1             MM                2 0
081500000000     C*
081600000000     C* RICERCA DELLA POSIZIONE IN SCHIERA DELL'ANNO/MESE CHE STO ELABORANDO
081700000000     C                   SETOFF                                           28
081800000000     C     SDRMES        LOOKUP    MES(MM)                                28
081900000000     C*
082000000000     C* SE MESE TROVATO, ADDIZIONO
082100000000IF  1C     *IN28         IFEQ      *ON
082200000000     C*
082300000000     C* FASCIA DI PESO PER FILIALE  D E T T A G L I O  (SOLO SE RICHIESTO)
082400000714IF  2C*---       PARDET    IFEQ 'S'
082500950529     C*
082600000000     C     FASCIA        OCCUR     FPDET
082700000000     C* PARTENZA
082800000000IF  3C     CICLO         IFEQ      'P'
082900000000     C     SDRTPO        ANDEQ     'F'
083000950529IF  4C     FSP(JI)       IFEQ      'S'                                          *VALIDA COME SPEDIZ.
083100950529     C     FRI(JI)       OREQ      'S'                                          *VALIDA COME RICAVO
083200950529     C                   MOVEL     '1'           PASDET                         *PASSATO MEMORIZZAZ.
083300950529E   4C                   ENDIF
083400000000     C                   ADD       RICAVI        RFT(MM)                        *RICAVI
083500000000     C                   ADD       SDRNSP        SFT(MM)                        *SPEDIZIONI°
083600000000E   3C                   ENDIF
083700000000     C* ARRIVO
083800000000IF  3C     CICLO         IFEQ      'A'
083900000000     C     SDRTPO        ANDEQ     'A'
084000950529IF  4C     FSP(JI)       IFEQ      'S'                                          *VALIDA COME SPEDIZ.
084100950529     C     FRI(JI)       OREQ      'S'                                          *VALIDA COME RICAVO
084200950529     C                   MOVEL     '1'           PASDET                         *PASSATO MEMORIZZAZ.
084300950529E   4C                   ENDIF
084400000000     C                   ADD       RICAVI        RAT(MM)                        *RICAVI
084500000000     C                   ADD       SDRNSP        SAT(MM)                        *SPEDIZIONI°
084600000000E   3C                   ENDIF
084700000714E   2C*---                 ENDIF
084800000000     C*
084900000000     C* FASCIA DI PESO PER  F I L I A L E
085000000000     C     FASCIA        OCCUR     FPFIL
085100000000     C* PARTENZA
085200000000IF  2C     CICLO         IFEQ      'P'
085300000000IF  3C     SDRTPO        IFEQ      'F'
085400950529     C*
085500950529     C* PASSATO DALLA MEMORIZZAZIONE
085600950529IF  4C     FSP(JI)       IFEQ      'S'                                          *VALIDA COME SPEDIZ.
085700950529     C     FRI(JI)       OREQ      'S'                                          *VALIDA COME RICAVO
085800950529     C                   MOVEL     '1'           PASFIL                         *PASSATO MEMORIZZAZ.
085900950529     C                   MOVEL     '1'           PASARE
086000950529     C                   MOVEL     '1'           PASDIV
086100950529     C                   MOVEL     '1'           PASGEN
086200950529E   4C                   ENDIF
086300000000     C                   ADD       RICAVI        RFF(MM)                        *RICAVI
086400000000     C                   ADD       SDRNSP        SFF(MM)                        *FRANCO PARTENZA
086500000000X   3C                   ELSE
086600950529     C*
086700950529     C* PASSATO DALLA MEMORIZZAZIONE
086800950529IF  4C     FSP(JI)       IFEQ      'S'                                          *VALIDA COME SPEDIZ.
086900950529     C                   MOVEL     '1'           PASFIL                         *PASSATO MEMORIZZAZ.
087000950529     C                   MOVEL     '1'           PASARE
087100950529     C                   MOVEL     '1'           PASDIV
087200950529     C                   MOVEL     '1'           PASGEN
087300950529E   4C                   ENDIF
087400000000     C                   ADD       SDRNSP        SXF(MM)                        *ASSEGNATO PARTENZA
087500000000E   3C                   ENDIF
087600000000E   2C                   ENDIF
087700000000     C* ARRIVO
087800000000IF  2C     CICLO         IFEQ      'A'
087900000000IF  3C     SDRTPO        IFEQ      'A'
088000950529     C*
088100950529     C* PASSATO DALLA MEMORIZZAZIONE
088200950529IF  4C     FSP(JI)       IFEQ      'S'                                          *VALIDA COME SPEDIZ.
088300950529     C     FRI(JI)       OREQ      'S'                                          *VALIDA COME RICAVO
088400950529     C                   MOVEL     '1'           PASFIL                         *PASSATO MEMORIZZAZ.
088500950529     C                   MOVEL     '1'           PASARE
088600950529     C                   MOVEL     '1'           PASDIV
088700950529     C                   MOVEL     '1'           PASGEN
088800950529E   4C                   ENDIF
088900000000     C                   ADD       RICAVI        RAF(MM)                        *RICAVI
089000000000     C                   ADD       SDRNSP        SAF(MM)                        *ASSEGNATO ARRIVO
089100000000X   3C                   ELSE
089200950529     C*
089300950529     C* PASSATO DALLA MEMORIZZAZIONE
089400950529IF  4C     FSP(JI)       IFEQ      'S'                                          *VALIDA COME SPEDIZ.
089500950529     C                   MOVEL     '1'           PASFIL                         *PASSATO MEMORIZZAZ.
089600950529     C                   MOVEL     '1'           PASARE
089700950529     C                   MOVEL     '1'           PASDIV
089800950529     C                   MOVEL     '1'           PASGEN
089900950529E   4C                   ENDIF
090000000000     C                   ADD       SDRNSP        SYF(MM)                        *FRANCO ARRIVO
090100000000E   3C                   ENDIF
090200000000E   2C                   ENDIF
090300000000     C*
090400000000     C* FASCIA DI PESO PER  A R E A
090500000000     C     FASCIA        OCCUR     FPARE
090600000000     C* PARTENZA
090700000000IF  2C     CICLO         IFEQ      'P'
090800000000IF  3C     SDRTPO        IFEQ      'F'
090900000000     C                   ADD       RICAVI        RFA(MM)                        *RICAVI
091000000000     C                   ADD       SDRNSP        SFA(MM)                        *FRANCO PARTENZA
091100000000X   3C                   ELSE
091200000000     C                   ADD       SDRNSP        SXA(MM)                        *ASSEGNATO PARTENZA
091300000000E   3C                   ENDIF
091400000000E   2C                   ENDIF
091500000000     C* ARRIVO
091600000000IF  2C     CICLO         IFEQ      'A'
091700000000IF  3C     SDRTPO        IFEQ      'A'
091800000000     C                   ADD       RICAVI        RAA(MM)                        *RICAVI
091900000000     C                   ADD       SDRNSP        SAA(MM)                        *ASSEGNATO ARRIVO
092000000000X   3C                   ELSE
092100000000     C                   ADD       SDRNSP        SYA(MM)                        *FRANCO ARRIVO
092200000000E   3C                   ENDIF
092300000000E   2C                   ENDIF
092400000000     C*
092500000000     C* FASCIA DI PESO PER  D I V I S I O N E
092600000000     C     FASCIA        OCCUR     FPDIV
092700000000     C* PARTENZA
092800000000IF  2C     CICLO         IFEQ      'P'
092900000000IF  3C     SDRTPO        IFEQ      'F'
093000000000     C                   ADD       RICAVI        RFD(MM)                        *RICAVI
093100000000     C                   ADD       SDRNSP        SFD(MM)                        *FRANCO PARTENZA
093200000000X   3C                   ELSE
093300000000     C                   ADD       SDRNSP        SXD(MM)                        *ASSEGNATO PARTENZA
093400000000E   3C                   ENDIF
093500000000E   2C                   ENDIF
093600000000     C* ARRIVO
093700000000IF  2C     CICLO         IFEQ      'A'
093800000000IF  3C     SDRTPO        IFEQ      'A'
093900000000     C                   ADD       RICAVI        RAD(MM)                        *RICAVI
094000000000     C                   ADD       SDRNSP        SAD(MM)                        *ASSEGNATO ARRIVO
094100000000X   3C                   ELSE
094200000000     C                   ADD       SDRNSP        SYD(MM)                        *FRANCO ARRIVO
094300000000E   3C                   ENDIF
094400000000E   2C                   ENDIF
094500000000     C*
094600000000     C* FASCIA DI PESO PER TOTALI  G E N E R A L I
094700000000     C     FASCIA        OCCUR     FPGEN
094800000000     C* PARTENZA
094900000000IF  2C     CICLO         IFEQ      'P'
095000000000IF  3C     SDRTPO        IFEQ      'F'
095100000000     C                   ADD       RICAVI        RFG(MM)                        *RICAVI
095200000000     C                   ADD       SDRNSP        SFG(MM)                        *FRANCO PARTENZA
095300000000X   3C                   ELSE
095400000000     C                   ADD       SDRNSP        SXG(MM)                        *ASSEGNATO PARTENZA
095500000000E   3C                   ENDIF
095600000000E   2C                   ENDIF
095700000000     C* ARRIVO
095800000000IF  2C     CICLO         IFEQ      'A'
095900000000IF  3C     SDRTPO        IFEQ      'A'
096000000000     C                   ADD       RICAVI        RAG(MM)                        *RICAVI
096100000000     C                   ADD       SDRNSP        SAG(MM)                        *ASSEGNATO ARRIVO
096200000000X   3C                   ELSE
096300000000     C                   ADD       SDRNSP        SYG(MM)                        *FRANCO ARRIVO
096400000000E   3C                   ENDIF
096500000000E   2C                   ENDIF
096600000000     C*
096700000000E   1C                   ENDIF
096800000000     C*
096900000000     C                   ENDSR
097000000000     C*------------------------------------------------------------------------*
097100000000     C*?CALANP - CALCOLA I SALDI DELL' ANNO PRECEDENTE
097200000000     C*------------------------------------------------------------------------*
097300000000     C     CALANP        BEGSR
097400000000     C*
097500000000     C* LETTURA SALDI X AA INIZIALE, E X AA FINALE SOLO SE DIVERSO DALL'INIZIALE
097600000000     C                   Z-ADD     1             J
097700000000DO  1C                   DO        2             J
097800000000     C                   MOVEL     '0'           $FIN2                          *REIMPOSTO FINE FILE
097900000000     C*
098000000000     C* SECONDO GIRO SOLO SE ANNO FINALE DIVERSO DALL' INIZIALE (PIENO)
098100000000IF  2C     PIF(J)        IFNE      *ZEROS
098200000000     C*
098300000000     C* POSIZIONAMENTO SUI SALDI PARTENZA ANNO PRECEDENTE E PRIMA LETTURA
098400000000     C                   MOVEL     'P'           CICLO                          *PARTENZA
098500000000     C                   EXSR      SETFI2
098600000000     C*
098700000000     C* CICLO FINO A FINE FILE SALDI O ROTTURA FILIALE
098800000000DO  3C     $FIN2         DOWEQ     '0'
098900000000     C*
099000000000     C* MEMORIZZA I FRANCHI CORRENTE DELLA FILIALE DETTAGLIO
099100000000     C                   EXSR      MEMANP
099200000000     C*
099300000000     C* LETTURA RECORD SALDI SUCESSIVO
099400000000     C                   EXSR      LETFI2
099500000000E   3C                   ENDDO
099600000000     C*
099700000000     C* POSIZIONAMENTO SALDI ARRIVO ANNO PRECEDENTE E PRIMA LETTURA
099800000000     C                   MOVEL     '0'           $FIN2
099900000000     C                   MOVEL     'A'           CICLO                          *ARRIVO
100000000000     C                   EXSR      SETFI2
100100000000     C*
100200000000     C* CICLO FINO A FINE FILE SALDI O ROTTURA FILIALE
100300000000DO  3C     $FIN2         DOWEQ     '0'
100400000000     C*
100500000000     C* MEMORIZZA GLI ASSEGNATI PRECEDENTI DELLA FILIALE DETTAGLIO
100600000000     C                   EXSR      MEMANP
100700000000     C*
100800000000     C* LETTURA RECORD SALDI SUCCESSIVO
100900000000     C                   EXSR      LETFI2
101000000000E   3C                   ENDDO
101100000000     C*
101200000000E   2C                   ENDIF
101300000000E   1C                   ENDDO
101400000000     C*
101500000000     C                   ENDSR
101600000000     C*------------------------------------------------------------------------*
101700000000     C*?SETFI2 - POSIZIONAMENTO E PRIMA LETTURA SALDI ANNO PRECEDENTE
101800000000     C*------------------------------------------------------------------------*
101900000000     C     SETFI2        BEGSR
102000000000     C*
102100990609     C* LEGGO 'SISDR01L' PER ELABORARE I FRANCHI, E 'SISDR02L' PER GLI ASSEGNATI
102200000000     C                   Z-ADD     PIF(J)        KSDANN                         *ANNO
102300000000     C                   Z-ADD     ORGFIL        KSDFIL                         *FILIALE
102400000000     C                   Z-ADD     FIL(F)        KSDFIT                         *FILIALE DETTAGLIO
102500000000     C*
102600000000     C* MI POSIZIONO CON IL MESE INIZIALE SE ANNI UGUALI O PRIMO ANNO DIVERSO,
102700000000     C* OPPURE CON IL MESE = 01 SE GLI ANNI SONO DIVERSI E STO ELABORANDO IL 2°
102800000000IF  1C     TIPANN        IFEQ      'U'
102900000000     C     J             OREQ      1
103000000000     C                   Z-ADD     MESINI        KSDMES                         *MESE INIZIALE
103100000000E   1C                   ELSE
103200000000     C                   Z-ADD     01            KSDMES                         *MESE 01
103300000000E   1C                   ENDIF
103400000000     C*
103500000000     C* POSIZIONAMENTO E PRIMA LETTURA (ANNO INIZIALE - 1)
103600000000IF  1C     CICLO         IFEQ      'P'                                          *PARTENZA
103700990609     C     KEYSDR        SETLL     SISDR01L                           99        *FINE ARCHIVIO
103800000000X   1C                   ELSE
103900990609     C     KEYSDR        SETLL     SISDR02L                           99        *FINE ARCHIVIO
104000000000E   1C                   ENDIF
104100000000     C*
104200000000IF  1C     *IN99         IFEQ      '1'
104300000000     C                   MOVEL     '1'           $FIN2
104400000000X   1C                   ELSE
104500000000     C                   EXSR      LETFI2                                       *LETTURA RECORD
104600000000E   1C                   ENDIF
104700000000     C*
104800000000     C                   ENDSR
104900000000     C*------------------------------------------------------------------------*
105000000000     C*?LETFI2 - LETTURA PROSSIMO RECORD SALDI ANNO PRECEDENTE
105100000000     C*------------------------------------------------------------------------*
105200000000     C     LETFI2        BEGSR
105300000000     C*
105400000000     C* LEGGO FINO A:
105500000000     C*     - FINE FILE
105600000000     C*     - TROVATO RECORD VALIDO
105700000000     C                   MOVEL     'N'           WRECOK
105800000000DO  1C     $FIN2         DOWEQ     '0'
105900000000     C     WRECOK        ANDEQ     'N'
106000000000     C*
106100000000     C* LEGGO I SALDI IN PARTENZA PER MEMORIZZARE I FRANCHI E QUELLI IN
106200000000     C* ARRIVO PER MEMORIZZARE GLI ASSEGNATI
106300000000IF  1C     CICLO         IFEQ      'P'                                          *PARTENZA
106400990609     C     KEYSD2        READE     SISDR01L                               99
106500000000X   1C                   ELSE
106600990609     C     KEYSD2        READE     SISDR02L                               99
106700000000E   1C                   ENDIF
106800000000     C*
106900000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
107000000000     C                   MOVEL     '1'           $FIN2
107100000000X   2C                   ELSE
107200000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
107300000000E   2C                   ENDIF
107400000000     C*
107500000000E   1C                   ENDDO
107600000000     C*
107700000000     C                   ENDSR
107800000000     C*------------------------------------------------------------------------*
107900000000     C*?MEMANP - MEMORIZZA I SALDI DELL' ANNO PRECEDENTE
108000000000     C*------------------------------------------------------------------------*
108100000000     C     MEMANP        BEGSR
108200000000     C*
108300000000     C* ADDIZIONO LE NOTE DI CREDITO, INVECE DI SOTTRARLE, PERCHE' NEL FILE
108400000000     C* SONO MEMORIZZATE CON IL SEGNO MENO
108500000000     C*
108600000000     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
108700000000     C* PER IL CONTEGGIO IN STAMPA DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
108800000000     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
108900000000     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
109000000000     C                   Z-ADD     1             JI
109100000000     C                   SETOFF                                           28
109200000000     C     SDRRBL        LOOKUP    FLAB(JI)                               28
109300000000     C* BOLLA NON VALIDA COME N°SPEDIZIONE
109400000000IF  1C     FSP(JI)       IFNE      'S'
109500000000     C                   Z-ADD     *ZEROS        SDRNSP                         *NUMERO SPEDIZIONI
109600000000     C                   Z-ADD     *ZEROS        SDRPKG                         *PESO
109700000000     C                   Z-ADD     *ZEROS        SDRNCO                         *NUMERO COLLI
109800000000     C                   Z-ADD     *ZEROS        SDRVOL                         *VOLUME TOTALE
109900000000     C                   Z-ADD     *ZEROS        SDRPTS                         *ECCEDENZA PESO TASS.
110000000000E   1C                   ENDIF
110100000000     C* BOLLA NON VALIDA COME RICAVO
110200000000IF  1C     FRI(JI)       IFNE      'S'
110300000000     C                   Z-ADD     *ZEROS        SDRIRB                         RICAVO REALE BOLLE
110400000000     C                   Z-ADD     *ZEROS        SDRIRR                         RETTIFICHE
110500000000     C                   Z-ADD     *ZEROS        SDRINC                         NOTE ACCREDITO
110600000000E   1C                   ENDIF
110700000000     C*
110800000000     C* SE SONO NEL CICLO PARTENZA ESCLUDO DALLA MEMORIZZAZIONE GLI ASSEGNATI;
110900000000     C* SE SONO NEL CICLO ARRIVO ESCLUDO I FRANCHI
111000000000IF  1C     CICLO         IFEQ      'P'
111100000000     C     SDRTPO        ANDEQ     'F'
111200000000     C     CICLO         OREQ      'A'
111300000000     C     SDRTPO        ANDEQ     'A'
111400000000     C*
111500000000     C* R I C A V O = (RICAVI DA BOLLE + RICAVI VARI + NOTE DI CREDITO)°°
111600000000     C*
111700000000     C                   Z-ADD     *ZEROS        RICAVI
111800000000     C                   ADD       SDRIRB        RICAVI                         *RICAVI DA BOLLE
111900000000     C     RICAVI        ADD       SDRIRR        RICAVI                         *RICAVI VARI
112000000000     C     RICAVI        ADD       SDRINC        RICAVI                         *NOTE DI CREDITO
112100000000     C*--------------------
112200000000     C* F A S C I A
112300000000     C*--------------------
112400000000     C                   MOVEL     SDRFPE        FASCIA
112500000000     C                   Z-ADD     1             MM
112600000000     C*
112700000000     C* RICERCA DELLA POSIZIONE IN SHIERA DELL'ANNO/MESE CHE STO ELABORANDO
112800000000     C                   SETOFF                                           28
112900000000     C     SDRMES        LOOKUP    MES(MM)                                28
113000000000     C*
113100000000     C* SE MESE TROVATO, ADDIZIONO
113200000000IF  2C     *IN28         IFEQ      *ON
113300000000     C*
113400000000     C* FASCIA DI PESO PER FILIALE  D E T T A G L I O  (SOLO SE RICHIESTO)
113500000714IF  3C*---       PARDET    IFEQ 'S'
113600000000     C     FASCIA        OCCUR     FPDET
113700000000     C                   ADD       RICAVI        RPT(MM)                        *RICAVI
113800000000     C                   ADD       SDRNSP        SPT(MM)                        *SPEDIZIONI°
113900000714E   3C*---                 ENDIF
114000000000     C*
114100000000     C* FASCIA DI PESO PER  F I L I A L E
114200000000     C     FASCIA        OCCUR     FPFIL
114300000000     C                   ADD       RICAVI        RPF(MM)                        *RICAVI
114400000000     C                   ADD       SDRNSP        SPF(MM)                        *SPEDIZIONI°
114500000000     C*
114600000000     C* FASCIA DI PESO PER  A R E A
114700000000     C     FASCIA        OCCUR     FPARE
114800000000     C                   ADD       RICAVI        RPA(MM)                        *RICAVI
114900000000     C                   ADD       SDRNSP        SPA(MM)                        *SPEDIZIONI°
115000000000     C*
115100000000     C* FASCIA DI PESO PER  D I V I S I O N E
115200000000     C     FASCIA        OCCUR     FPDIV
115300000000     C                   ADD       RICAVI        RPD(MM)                        *RICAVI
115400000000     C                   ADD       SDRNSP        SPD(MM)                        *SPEDIZIONI°
115500000000     C*
115600000000     C* FASCIA DI PESO PER TOTALI  G E N E R A L I
115700000000     C     FASCIA        OCCUR     FPGEN
115800000000     C                   ADD       RICAVI        RPG(MM)                        *RICAVI
115900000000     C                   ADD       SDRNSP        SPG(MM)                        *SPEDIZIONI°
116000000000E   2C                   ENDIF
116100000000E   1C                   ENDIF
116200000000     C*
116300000000     C                   ENDSR
116400000000     C*------------------------------------------------------------------------*
116500000000     C*?CALBDG - CALCOLA I VALORI DEL BUDGET
116600000000     C*------------------------------------------------------------------------*
116700000000     C     CALBDG        BEGSR
116800000000     C*
116900000000     C* POSIZIONAMENTO SUL FILE BUDGET E PRIMA LETTURA
117000000000     C                   EXSR      SETFI3
117100000000     C*
117200000000     C* FINCHE' FINE FILE BUDGET O ROTTURA FILIALE
117300000000DO  1C     $FIN3         DOWEQ     '0'
117400000000     C*
117500000000     C* MEMORIZZA VALORI BUDGET
117600000000     C                   EXSR      MEMBDG
117700000000     C*
117800000000     C* LETTURA PROSSIMO RECORD BUDGET
117900000000     C                   EXSR      LETFI3
118000000000E   1C                   ENDDO
118100000000     C*
118200000000     C                   ENDSR
118300000000     C*------------------------------------------------------------------------*
118400000000     C*?SETF13 - POSIZIONAMENTO E PRIMA LETTURA FILE BUDGET
118500000000     C*------------------------------------------------------------------------*
118600000000     C     SETFI3        BEGSR
118700000000     C*
118800000000     C* SE SELEZIONATI TIPO SERVIZIO O TIPO PRODOTTO NON LEGGO I VALORI DEL
118900000000     C* BUDGET PERCHE' QUESTI SONO PREVISTI SOLO PER FILIALE
119000000000IF  1C     PARSER        IFNE      *BLANKS
119100000000     C     PARPRO        ORNE      *BLANKS
119200000000     C                   MOVEL     '1'           $FIN3                          *USCITA CICLO
119300000000     C                   MOVEL     '1'           NOBDG                          *NO LETTURA BUDGET
119400000000     C                   ELSE
119500000000     C                   MOVEL     '0'           NOBDG                          *SI LETTURA BUDGET
119600000000E   1C                   ENDIF
119700000000     C*
119800000000     C* POSIZIONAMENTO E PRIMA LETTURA SUL FILE BUDGET (FILIALE+ANNO+MESE)
119900000000     C* SOLO SE RICHIESTI TUTTI I SERVIZI E TUTTI I PRODOTTI
120000000000IF  1C     $FIN3         IFEQ      '0'
120100000000     C                   Z-ADD     DEPFIL        KBDFIL
120200000000     C                   Z-ADD     ANNINI        KBDANN
120300000000     C                   Z-ADD     MESINI        KBDMES
120400960115     C                   MOVEL     PARIOE        KBDIOE
120500960108     C*
120600010718     C     KEYBDG        SETLL     TEBDG01L                           99        *FINE ARCHIVIO
120700000000     C*
120800000000IF  2C     *IN99         IFEQ      '1'
120900000000     C                   MOVEL     '1'           $FIN3
121000000000X   2C                   ELSE
121100000000     C                   EXSR      LETFI3                                       *LETTURA RECORD
121200000000E   2C                   ENDIF
121300000000E   1C                   ENDIF
121400000000     C*
121500000000     C                   ENDSR
121600000000     C*------------------------------------------------------------------------*
121700000000     C*?LETF13 - LETTURA PROSSIMO RECORD BUDGET
121800000000     C*------------------------------------------------------------------------*
121900000000     C     LETFI3        BEGSR
122000000000     C*
122100000000     C* LEGGO FINO A:
122200000000     C*     - FINE FILE O ROTTURA FILIALE
122300000000     C*     - TROVATO RECORD VALIDO
122400000000     C                   MOVEL     'N'           WRECOK
122500000000DO  1C     $FIN3         DOWEQ     '0'
122600000000     C     WRECOK        ANDEQ     'N'
122700000000     C*
122800010718     C     KBDFIL        READE     TEBDG01L                               99
122900000000     C*
123000000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
123100000000     C                   MOVEL     '1'           $FIN3
123200000000X   2C                   ELSE
123300000000     C                   EXSR      CHKRE3                                       *CONTROLLO RECORD
123400000000E   2C                   ENDIF
123500000000     C*
123600000000E   1C                   ENDDO
123700000000     C*
123800000000     C                   ENDSR
123900000000     C*------------------------------------------------------------------------*
124000000000     C*?CHKRE3 - CONTROLLO RECORD BUDGET
124100000000     C*------------------------------------------------------------------------*
124200000000     C     CHKRE3        BEGSR
124300000000     C*
124400000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
124500990531     C*
124600990531     C* LA DATA LETTA DEVE ESSERE MAX/UGU ALLA DATA INIZ E MIN/UGU ALLA DATA FIN
124700990531IF  1C     TBGDAT        IFGE      PARDAT
124800990531     C     TBGDAT        ANDLE     PARDA2
124900000000X   1C                   ELSE
125000000000     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
125100000000     C*
125200000000     C* SE POI DATA BUDGET MAGGIORE A DATA FINALE RICHIESTA, ESCO DAL CICLO
125300990531IF  2C     TBGDAT        IFGT      PARDA2
125400000000     C                   MOVEL     '1'           $FIN3
125500000000E   2C                   ENDIF
125600000000E   1C                   ENDIF
125700000000     C*
125800000000     C* VALORE DI BUDGET ANNULLATO, ESCLUDO
125900000000IF  1C     TBGATB        IFNE      *BLANKS
126000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
126100000000E   1C                   ENDIF
126200960105     C*
126300960115     C* DATI IN STATISTICA ITALIA <> DA QUELLI RICHIESTI
126400960115IF  1C     PARIOE        IFNE      *BLANKS
126500960115IF  2C     PARIOE        IFNE      TBGIOE
126600960105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
126700960105E   2C                   ENDIF
126800960105E   1C                   ENDIF
126900000000     C*
127000000000     C                   ENDSR
127100000000     C*------------------------------------------------------------------------*
127200000000     C*?MEMBDG - MEMORIZZA VALORI BUDGET
127300000000     C*------------------------------------------------------------------------*
127400000000     C     MEMBDG        BEGSR
127500000000     C*
127600000000     C* °INVECE DI SOTTRARRE LE NOTE DI CREDITO E LE RETTIFICHE, LE ADDIZIONO
127700000000     C*  PERCHE' NEL FILE SONO MEMORIZZATE CON IL SEGNO MENO
127800000000     C*
127900000000     C* R I C A V O = (RICAVI BOLLE + RETTIFICHE AI RICAVI + NOTE DI CREDITO)°
128000000000     C*
128100000000     C                   Z-ADD     *ZEROS        RICAVI
128200000000     C                   ADD       TBGIRB        RICAVI                         *RICAVI DA BOLLE
128300000000     C     RICAVI        ADD       TBGINC        RICAVI                         *NOTE DI CREDITO
128400000000     C     RICAVI        ADD       TBGIRR        RICAVI                         *RETTIFICHE AI RICAVI
128500000000     C*
128600000000     C* RICERCA DELLA POSIZIONE IN SHIERA DELL'ANNO/MESE CHE STO ELABORANDO
128700000000     C                   MOVEL     TBGFPE        FASCIA
128800000000     C                   Z-ADD     1             MM
128900000000     C                   SETOFF                                           28
129000000000     C     TBGMES        LOOKUP    MES(MM)                                28
129100000000     C*
129200000000     C* SE MESE TROVATO, ADDIZIONO
129300000000IF  1C     *IN28         IFEQ      *ON
129400000000     C*
129500000000     C* FASCIA DI PESO PER  F I L I A L E
129600000000     C     FASCIA        OCCUR     FPFIL
129700000000     C                   ADD       RICAVI        RBF(MM)                        *RICAVI
129800000000     C                   ADD       TBGNSP        SOF(MM)                        *SPEDIZIONI PARTENZA
129900000000     C                   ADD       TBGNSA        SIF(MM)                        *SPEDIZIONI ARRIVO
130000000000     C*
130100000000     C* FASCIA DI PESO PER  A R E A
130200000000     C     FASCIA        OCCUR     FPARE
130300000000     C                   ADD       RICAVI        RBA(MM)                        *RICAVI
130400000000     C                   ADD       TBGNSP        SOA(MM)                        *SPEDIZIONI PARTENZA
130500000000     C                   ADD       TBGNSA        SIA(MM)                        *SPEDIZIONI ARRIVO
130600000000     C*
130700000000     C* FASCIA DI PESO PER  D I V I S I O N E
130800000000     C     FASCIA        OCCUR     FPDIV
130900000000     C                   ADD       RICAVI        RBD(MM)                        *RICAVI
131000000000     C                   ADD       TBGNSP        SOD(MM)                        *SPEDIZIONI PARTENZA
131100000000     C                   ADD       TBGNSA        SID(MM)                        *SPEDIZIONI ARRIVO
131200000000     C*
131300000000     C* FASCIA DI PESO PER TOTALI  G E N E R A L I
131400000000     C     FASCIA        OCCUR     FPGEN
131500000000     C                   ADD       RICAVI        RBG(MM)                        *RICAVI
131600000000     C                   ADD       TBGNSP        SOG(MM)                        *SPEDIZIONI PARTENZA
131700000000     C                   ADD       TBGNSA        SIG(MM)                        *SPEDIZIONI ARRIVO
131800000000     C*
131900000000E   1C                   ENDIF
132000000000     C*
132100000000     C                   ENDSR
132200000000     C*------------------------------------------------------------------------*
132300000000     C*?STADET - IMPOSTA STAMPA TOTALI FILIALE DETTAGLIO
132400000000     C*------------------------------------------------------------------------*
132500000000     C     STADET        BEGSR
132600000000     C*
132700000000     C* °LE PERCENTUALI SUL BUDGET IN STAMPA LE IMPOSTO A ZERO PERCHE' I DATI
132800000000     C*  SUL FILE PER FILIALE/FILIALE NON ESISTONO
132900000000     C*
133000000000     C* CODICI INTESTAZIONE
133100980206     C                   MOVEL     DEPFIL        PTCFIL                         *FILIALE
133200000000     C                   MOVEL     DESFIL        PTDFIL
133300980206     C*---                 Z-ADD*ZEROS    PTCFIL
133400000000     C*
133500980206     C                   MOVEL     FIL(F)        PTCFIT                         *FILIALE DETTAGLIO
133600000000     C                   MOVEL     DFIL(F)       PTDFIT
133700980206     C*---                 Z-ADD*ZEROS    PTCFIT
133800000000     C*
133900980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
134000000000     C                   MOVEL     DESARE        PTDARE
134100980206     C*---                 Z-ADD*ZEROS    PTCARE
134200000000     C*
134300980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
134400980206     C                   MOVEL     DESDIV        PTDDIV
134500980206     C*---                 MOVELDEPDIV    D       10
134600980206     C*---                 MOVELDIV,D     PTDDIV
134700950525     C*
134800960105     C                   MOVEL     PADIOE        PTDIOE                         *ITALIA/IMPORT/EXPORT
134900000000     C*
135000000000     C* SCHIERE SALDI MENSILI DI FILIALE DETTAGLIO
135100000000DO  1C     1             DO        10            A
135200000000     C     A             OCCUR     FPDET
135300000000     C     A             OCCUR     FPSTA
135400000000     C                   Z-ADD     RFT           RFS
135500000000     C                   Z-ADD     SFT           SFS
135600000000     C                   Z-ADD     RAT           RAS
135700000000     C                   Z-ADD     SAT           SAS
135800000000     C                   Z-ADD     RPT           RPS
135900000000     C                   Z-ADD     SPT           SPS
136000000000     C*
136100000000     C* PULISCO I CAMPI DI LAVORO PER IL BUDGET (PER DETTAGLIO NON ESISTONO)
136200000000     C                   CLEAR                   RBS
136300000000     C                   CLEAR                   SOS
136400000000     C                   CLEAR                   SXS
136500000000     C                   CLEAR                   SYS
136600000000     C                   CLEAR                   SIS
136700000000E   1C                   ENDDO
136800000000     C*
136900000000     C* IMPOSTO NO STAMPA DATI BUDGET°
137000000000     C                   MOVEL     '1'           NOBDG
137100000000     C*
137200000000     C* ROUTINE DI STAMPA TOTALI
137300000000     C                   EXSR      STAMPA
137400000000     C*
137500000000     C                   ENDSR
137600000000     C*------------------------------------------------------------------------*
137700000000     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
137800000000     C*------------------------------------------------------------------------*
137900000000     C     STAFIL        BEGSR
138000000000     C*
138100000000     C* CODICI INTESTAZIONE
138200980206     C                   MOVEL     DEPFIL        PTCFIL                         *FILIALE
138300000000     C                   MOVEL     DESFIL        PTDFIL
138400980206     C*---                 Z-ADD*ZEROS    PTCFIL
138500000000     C*
138600000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
138700000000     C                   MOVEL     *BLANKS       PTDFIT
138800950602     C                   MOVEL     PAR(10)       PTDFIT
138900000000     C*
139000980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
139100000000     C                   MOVEL     DESARE        PTDARE
139200980206     C*---                 Z-ADD*ZEROS    PTCARE
139300000000     C*
139400980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
139500980206     C                   MOVEL     DESDIV        PTDDIV
139600980206     C*---                 MOVELDEPDIV    D       10
139700980206     C*---                 MOVELDIV,D     PTDDIV
139800950525     C*
139900960105     C                   MOVEL     PADIOE        PTDIOE                         *ITALIA/IMPORT/EXPORT
140000000000     C*
140100000000     C* SCHIERE SALDI MENSILI DI FILIALE
140200000000DO  1C     1             DO        10            A
140300000000     C     A             OCCUR     FPFIL
140400000000     C     A             OCCUR     FPSTA
140500000000     C                   Z-ADD     RFF           RFS
140600000000     C                   Z-ADD     SFF           SFS
140700000000     C                   Z-ADD     RAF           RAS
140800000000     C                   Z-ADD     SAF           SAS
140900000000     C                   Z-ADD     RBF           RBS
141000000000     C                   Z-ADD     RPF           RPS
141100000000     C                   Z-ADD     SOF           SOS
141200000000     C                   Z-ADD     SPF           SPS
141300000000     C                   Z-ADD     SXF           SXS
141400000000     C                   Z-ADD     SYF           SYS
141500000000     C                   Z-ADD     SIF           SIS
141600000000E   1C                   ENDDO
141700000000     C*
141800000000     C* ROUTINE DI STAMPA TOTALI
141900000000     C                   EXSR      STAMPA
142000000000     C*
142100000000     C                   ENDSR
142200000000     C*------------------------------------------------------------------------*
142300000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
142400000000     C*------------------------------------------------------------------------*
142500000000     C     STAARE        BEGSR
142600000000     C*
142700000000     C* CODICI INTESTAZIONE
142800000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
142900000000     C                   MOVEL     *BLANKS       PTDFIL
143000950602     C                   MOVEL     PAR(10)       PTDFIL
143100000000     C*
143200000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
143300000000     C                   MOVEL     *BLANKS       PTDFIT
143400950602     C                   MOVEL     PAR(10)       PTDFIT
143500000000     C*
143600980206     C                   MOVEL     DEPARE        PTCARE                         *AREA
143700000000     C                   MOVEL     DESARE        PTDARE
143800980206     C*---                 Z-ADD*ZEROS    PTCARE
143900000000     C*
144000980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
144100980206     C                   MOVEL     DESDIV        PTDDIV
144200980206     C*---                 MOVELDEPDIV    D       10
144300980206     C*---                 MOVELDIV,D     PTDDIV
144400950525     C*
144500960105     C                   MOVEL     PADIOE        PTDIOE                         *ITALIA/IMPORT/EXPORT
144600000000     C*
144700000000     C* SCHIERE SALDI MENSILI DI AREA
144800000000DO  1C     1             DO        10            A
144900000000     C     A             OCCUR     FPARE
145000000000     C     A             OCCUR     FPSTA
145100000000     C                   Z-ADD     RFA           RFS
145200000000     C                   Z-ADD     SFA           SFS
145300000000     C                   Z-ADD     RAA           RAS
145400000000     C                   Z-ADD     SAA           SAS
145500000000     C                   Z-ADD     RBA           RBS
145600000000     C                   Z-ADD     RPA           RPS
145700000000     C                   Z-ADD     SOA           SOS
145800000000     C                   Z-ADD     SPA           SPS
145900000000     C                   Z-ADD     SXA           SXS
146000000000     C                   Z-ADD     SYA           SYS
146100000000     C                   Z-ADD     SIA           SIS
146200000000E   1C                   ENDDO
146300000000     C*
146400000000     C* ROUTINE DI STAMPA TOTALI
146500000000     C                   EXSR      STAMPA
146600000000     C*
146700000000     C                   ENDSR
146800000000     C*------------------------------------------------------------------------*
146900000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
147000000000     C*------------------------------------------------------------------------*
147100000000     C     STADIV        BEGSR
147200000000     C*
147300000000     C* CODICI INTESTAZIONE
147400000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
147500000000     C                   MOVEL     *BLANKS       PTDFIL
147600950602     C                   MOVEL     PAR(10)       PTDFIL
147700000000     C*
147800000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
147900000000     C                   MOVEL     *BLANKS       PTDFIT
148000950602     C                   MOVEL     PAR(10)       PTDFIT
148100000000     C*
148200000000     C                   MOVEL     *BLANKS       PTCARE                         *AREA
148300000000     C                   MOVEL     *BLANKS       PTDARE
148400950602     C                   MOVEL     PAR(10)       PTDARE
148500000000     C*
148600980206     C                   MOVEL     DEPDIV        PTCDIV                         *DIVISIONE
148700980206     C                   MOVEL     DESDIV        PTDDIV
148800980206     C*---                 MOVELDEPDIV    D       10
148900980206     C*---                 MOVELDIV,D     PTDDIV
149000950525     C*
149100960105     C                   MOVEL     PADIOE        PTDIOE                         *ITALIA/IMPORT/EXPORT
149200000000     C*
149300000000     C* SCHIERE SALDI MENSILI DI DIVISIONE
149400000000DO  1C     1             DO        10            A
149500000000     C     A             OCCUR     FPDIV
149600000000     C     A             OCCUR     FPSTA
149700000000     C                   Z-ADD     RFD           RFS
149800000000     C                   Z-ADD     SFD           SFS
149900000000     C                   Z-ADD     RAD           RAS
150000000000     C                   Z-ADD     SAD           SAS
150100000000     C                   Z-ADD     RBD           RBS
150200000000     C                   Z-ADD     RPD           RPS
150300000000     C                   Z-ADD     SOD           SOS
150400000000     C                   Z-ADD     SPD           SPS
150500000000     C                   Z-ADD     SXD           SXS
150600000000     C                   Z-ADD     SYD           SYS
150700000000     C                   Z-ADD     SID           SIS
150800000000E   1C                   ENDDO
150900000000     C*
151000000000     C* ROUTINE DI STAMPA TOTALI
151100000000     C                   EXSR      STAMPA
151200000000     C*
151300000000     C                   ENDSR
151400000000     C*------------------------------------------------------------------------*
151500000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
151600000000     C*------------------------------------------------------------------------*
151700000000     C     STAGEN        BEGSR
151800000000     C*
151900000000     C* CODICI INTESTAZIONE
152000000000     C                   MOVEL     *BLANKS       PTCFIL                         *FILIALE
152100000000     C                   MOVEL     *BLANKS       PTDFIL
152200950602     C                   MOVEL     PAR(10)       PTDFIL
152300000000     C*
152400000000     C                   MOVEL     *BLANKS       PTCFIT                         *FILIALE DETTAGLIO
152500000000     C                   MOVEL     *BLANKS       PTDFIT
152600950602     C                   MOVEL     PAR(10)       PTDFIT
152700000000     C*
152800000000     C                   MOVEL     *BLANKS       PTCARE                         *AREA
152900000000     C                   MOVEL     *BLANKS       PTDARE
153000950602     C                   MOVEL     PAR(10)       PTDARE
153100000000     C*
153200000000     C                   MOVEL     *BLANKS       PTCDIV                         *DIVISIONE
153300000000     C                   MOVEL     *BLANKS       PTDDIV
153400950602     C                   MOVEL     PAR(10)       PTDDIV
153500950525     C*
153600960105     C                   MOVEL     PADIOE        PTDIOE                         *ITALIA/IMPORT/EXPORT
153700000000     C*
153800000000     C* SCHIERE SALDI MENSILI GENERALI
153900000000DO  1C     1             DO        10            A
154000000000     C     A             OCCUR     FPGEN
154100000000     C     A             OCCUR     FPSTA
154200000000     C                   Z-ADD     RFG           RFS
154300000000     C                   Z-ADD     SFG           SFS
154400000000     C                   Z-ADD     RAG           RAS
154500000000     C                   Z-ADD     SAG           SAS
154600000000     C                   Z-ADD     RBG           RBS
154700000000     C                   Z-ADD     RPG           RPS
154800000000     C                   Z-ADD     SOG           SOS
154900000000     C                   Z-ADD     SPG           SPS
155000000000     C                   Z-ADD     SXG           SXS
155100000000     C                   Z-ADD     SYG           SYS
155200000000     C                   Z-ADD     SIG           SIS
155300000000E   1C                   ENDDO
155400000000     C*
155500000000     C* ROUTINE DI STAMPA TOTALI DI FILIALE
155600000000     C                   EXSR      STAMPA
155700000000     C*
155800000000     C                   ENDSR
155900000000     C*------------------------------------------------------------------------*
156000000000     C*?STAMPA - STAMPA TOTALI GENERALI, DI DIVISIONE/AREA/FILIALE
156100000000     C*------------------------------------------------------------------------*
156200000000     C     STAMPA        BEGSR
156300000000     C*
156400000000     C* REIMPOSTO IL CONTATORE DI FASCE STAMPATE PER RISTAMPARE LA TESTATA
156500000000     C                   Z-ADD     *ZEROS        CONFAS            2 0
156600000000     C*
156700000000     C* PULISCO I TOTALI A FINE PAGINA
156800000000     C                   CLEAR                   RFW
156900000000     C                   CLEAR                   SFW
157000000000     C                   CLEAR                   RAW
157100000000     C                   CLEAR                   SAW
157200000000     C                   CLEAR                   RBW
157300000000     C                   CLEAR                   RPW
157400000000     C                   CLEAR                   SOW
157500000000     C                   CLEAR                   SPW
157600000000     C                   CLEAR                   SXW
157700000000     C                   CLEAR                   SYW
157800000000     C                   CLEAR                   SIW
157900000000     C*--------------------
158000000000     C* T E S T A T A
158100000000     C*--------------------
158200010511     C                   WRITE     SE56PT                                       *TESTATA
158300010511     C                   WRITE     SE56PR                                       *RIGA
158400010511     C                   WRITE     SE56PT1                                      *DECOD FIL/AREA/DIV
158500010511     C                   WRITE     SE56PR                                       *RIGA
158600010511     C                   WRITE     SE56PI                                       *DECODIFICA MESI
158700000000     C*
158800000000     C* PER OGNI FASCIA LETTA
158900000000DO  1C     1             DO        10            A
159000000000     C     A             OCCUR     FPSTA
159100000000     C*
159200000000     C                   ADD       1             CONFAS                         *CONT. FASCE STAMPATE
159300000000     C                   MOVEL     FPE(A)        PPDFPE                         *DESCRIZIONE FASCIA
159400000000     C*
159500000000     C* ADDIZIONA I SALDI PER I TOTALI A FINE PAGINA
159600000000     C                   ADD       RFS           RFW
159700000000     C                   ADD       SFS           SFW
159800000000     C                   ADD       RAS           RAW
159900000000     C                   ADD       SAS           SAW
160000000000     C                   ADD       RBS           RBW
160100000000     C                   ADD       RPS           RPW
160200000000     C                   ADD       SOS           SOW
160300000000     C                   ADD       SPS           SPW
160400000000     C                   ADD       SXS           SXW
160500000000     C                   ADD       SYS           SYW
160600000000     C                   ADD       SIS           SIW
160700000000     C*
160800000000     C* SE SONO ALLA 5^ O ALLA 9^ FASCIA STAMPO LA TESTATA A SALTO PAGINA
160900000000IF  2C     CONFAS        IFEQ      5
161000000000     C     CONFAS        OREQ      9
161100010511     C                   WRITE     SE56PR                                       *RIGA
161200010511     C                   WRITE     SE56PT                                       *TESTATA
161300010511     C                   WRITE     SE56PR                                       *RIGA
161400010511     C                   WRITE     SE56PI                                       *DECODIFICA MESI
161500000000E   2C                   ENDIF
161600000000     C*
161700010511     C                   WRITE     SE56PP                                       *STAMPA DECOD. FASCIA
161800000000     C*--------------------
161900000000     C* F A S C I A
162000000000     C*--------------------
162100000000     C                   EXSR      STFASC
162200000000     C*
162300000000E   1C                   ENDDO
162400000000     C*
162500000000     C* TRASFERISCE I TOTALI A FINE PAGINA NELLE SCHIERE DI LAVORO
162600000000     C                   Z-ADD     RFW           RFS
162700000000     C                   Z-ADD     SFW           SFS
162800000000     C                   Z-ADD     RAW           RAS
162900000000     C                   Z-ADD     SAW           SAS
163000000000     C                   Z-ADD     RBW           RBS
163100000000     C                   Z-ADD     RPW           RPS
163200000000     C                   Z-ADD     SOW           SOS
163300000000     C                   Z-ADD     SPW           SPS
163400000000     C                   Z-ADD     SXW           SXS
163500000000     C                   Z-ADD     SYW           SYS
163600000000     C                   Z-ADD     SIW           SIS
163700000000     C*--------------------
163800000000     C* T O T A L E   P A G I N A
163900000000     C*--------------------
164000000000     C                   MOVEL     DRI(9)        PPDFPE                         *DESCRIZIONE TOTALE
164100010511     C                   WRITE     SE56PP                                       *STAMPA DESCR. TOTALE
164200000000     C                   EXSR      STFASC
164300000000     C*
164400000000     C* STAMPA RIGA DI CHIUSURA PROSPETTO
164500010511     C                   WRITE     SE56PR
164600000000     C*
164700000000     C                   ENDSR
164800000000     C*------------------------------------------------------------------------*
164900000000     C*?STFASC - STAMPA I VALORI DELLA FASCIA O DEI TOTALI A FINE PAGINA
165000000000     C*------------------------------------------------------------------------*
165100000000     C     STFASC        BEGSR
165200000000     C*--------------------
165300000000     C* R I C A V I  F R A N C O
165400000000     C*--------------------
165500000000     C                   MOVEL     '1'           STRRIC                         *STAMPA RIGA RICAVI
165600000000     C                   MOVEL     '0'           STRRIM                         *NON STAMPA RIC MEDIO
165700000000     C                   MOVEL     '0'           STRRIT                         *NON STAMPA RIC TOT
165800000000     C                   MOVEL     DRI(1)        P1DFAS                         *DESCRIZIONE VALORE
165900000000     C*
166000000000     C* STAMPA LA RIGA VALORE
166100000000     C                   Z-ADD     RFS           $VA                            *VALORE
166200000000     C                   EXSR      STRIGV
166300010511 EUR C* --->
166400010515     C*    $VA13C        MULT      1000          TOTRIC           19 5          *TOTALE RICAVI
166500010515     C                   Z-ADD     $VA13C        TOTRIC           19 5          *TOTALE RICAVI
166600010511 EUR C* <---
166700000000     C*--------------------
166800000000     C* S P E D I Z I O N I  F R A N C O
166900000000     C*--------------------
167000000000     C                   MOVEL     '0'           STRRIC                         *NON STAMPA RICAVI
167100000000     C                   MOVEL     '0'           STRRIM                         *NON STAMPA RIC MEDIO
167200000000     C                   MOVEL     '0'           STRRIT                         *NON STAMPA RIC TOT
167300000000     C                   MOVEL     DRI(2)        P1DFAS                         *DESCRIZIONE VALORE
167400000000     C*
167500000000     C* STAMPA LA RIGA VALORE
167600000000     C                   Z-ADD     SFS           $VA                            *VALORE
167700000000     C                   EXSR      STRIGV
167800000000     C                   Z-ADD     $VA13C        TOTSPE           15 0          *TOTALE SPEDIZIONI
167900000000     C*--------------------
168000000000     C* R I C A V O   M E D I O   F R A N C O
168100000000     C*--------------------
168200000000     C                   MOVEL     '1'           STRRIC                         *STAMPA RICAVI
168300000000     C                   MOVEL     '1'           STRRIM                         *STAMPA RICAVO MEDIO
168400000000     C                   MOVEL     '0'           STRRIT                         *NON STAMPA R.TOTALE
168500000000     C                   MOVEL     DRI(3)        P1DFAS                         *DESCRIZIONE VALORE
168600000000     C*
168700000000     C* STAMPA LA RIGA VALORI
168800000000     C                   Z-ADD     RFS           $VA                            *VALORE RICAVI
168900000000     C                   Z-ADD     SFS           $VM                            *VALORE SPEDIZIONI
169000010515     C                   Z-ADD     TOTRIC        RICAV2           19 5          *TOTALE RICAVI
169100000000     C                   Z-ADD     TOTSPE        SPEDIZ           15 0          *TOTALE SPEDIZIONI
169200000000     C                   EXSR      STRIGV
169300000000     C*--------------------
169400000000     C* R I C A V I  A S S E G N A T O
169500000000     C*--------------------
169600000000     C                   MOVEL     '1'           STRRIC                         *STAMPA RIGA RICAVI
169700000000     C                   MOVEL     '0'           STRRIM                         *NON STAMPA RIC MEDIO
169800000000     C                   MOVEL     '0'           STRRIT                         *NON STAMPA RIC TOT
169900000000     C                   MOVEL     DRI(1)        P1DFAS                         *DESCRIZIONE VALORE
170000000000     C*
170100000000     C* STAMPA LA RIGA VALORE
170200000000     C                   Z-ADD     RAS           $VA                            *VALORE
170300000000     C                   EXSR      STRIGV
170400010511 EIR C* --->
170500010511     C*    $VA13C        MULT      1000          TOTRIC                         *13° TOTALE RICAVI
170600010511     C                   Z-ADD     $VA13C        TOTRIC                         *13° TOTALE RICAVI
170700010511 EIR C* <---
170800000000     C*--------------------
170900000000     C* S P E D I Z I O N I  A S S E G N A T O
171000000000     C*--------------------
171100000000     C                   MOVEL     '0'           STRRIC                         *NON STAMPA RICAVI
171200000000     C                   MOVEL     '0'           STRRIM                         *NON STAMPA RIC MEDIO
171300000000     C                   MOVEL     '0'           STRRIT                         *NON STAMPA RIC MEDIO
171400000000     C                   MOVEL     DRI(4)        P1DFAS                         *DESCRIZIONE VALORE
171500000000     C*
171600000000     C* STAMPA LA RIGA VALORE
171700000000     C                   Z-ADD     SAS           $VA                            *VALORE
171800000000     C                   EXSR      STRIGV
171900000000     C                   Z-ADD     $VA13C        TOTSPE                         *13° TOT SPEDIZIONI
172000000000     C*--------------------
172100000000     C* R I C A V O   M E D I O   A S S E G N A T O
172200000000     C*--------------------
172300000000     C                   MOVEL     '1'           STRRIC                         *STAMPA RICAVI
172400000000     C                   MOVEL     '1'           STRRIM                         *STAMPA RICAVO MEDIO
172500000000     C                   MOVEL     '0'           STRRIT                         *NON STAMPA R.TOTALE
172600000000     C                   MOVEL     DRI(3)        P1DFAS                         *DESCRIZIONE VALORE
172700000000     C*
172800000000     C* STAMPA LA RIGA VALORI
172900000000     C                   Z-ADD     RAS           $VA                            *VALORE RICAVI
173000000000     C                   Z-ADD     SAS           $VM                            *VALORE SPEDIZIONI
173100000000     C                   Z-ADD     TOTRIC        RICAV2                         *TOTALE RICAVI
173200000000     C                   Z-ADD     TOTSPE        SPEDIZ                         *TOTALE SPEDIZIONI
173300000000     C                   EXSR      STRIGV
173400000000     C*
173500000000     C* STAMPO UNA RIGA VUOTA
173600010511     C                   WRITE     SE56RV                                       *RIGA VUOTA
173700000000     C*--------------------
173800000000     C* R I C A V I   T O T A L I
173900000000     C*--------------------
174000000000     C                   MOVEL     '1'           STRRIC                         *STAMPA RICAVI
174100000000     C                   MOVEL     '0'           STRRIM                         *NON STAMPA RIC MEDIO
174200000000     C                   MOVEL     '1'           STRRIT                         *STAMPA RICAVO TOTALE
174300000000     C                   MOVEL     DRI(5)        P1DFAS                         *DESCRIZIONE VALORE
174400000000     C*
174500000000     C* STAMPA LA RIGA VALORI
174600000000     C                   Z-ADD     RFS           $VA                            *VALORE RICAVI FRANCO
174700000000     C                   Z-ADD     RAS           $VM                            *VALORE RICAVI ASSEGN
174800010511     C                   EXSR      STRIGV
174900010511 EUR C* --->
175000010511     C*    $VA           MULT      1000          $RT                            *RICAVI TOTALI MESE
175100010511     C                   Z-ADD     $VA           $RT                            *RICAVI TOTALE MESE
175200010511     C*    $VA13C        MULT      1000          TOTRIC                         *13° TOT RICAVI
175300010511     C                   Z-ADD     $VA13C        TOTRIC                         *13° TOT RICAVI
175400010511 EUR C* --->
175500000000     C*--------------------
175600000000     C* P E R C E N T U A L I  RICAVI TOTALI
175700000000     C*--------------------
175800000000DO  1C     1             DO        12            X
175900000000     C                   MOVEL     '0'           $CP(X)                         *IMPOSTO A NO -999,9%
176000000000E   1C                   ENDDO
176100000000     C                   MOVEL     '0'           $CP13C                         *ANCHE LA 13^ %
176200000000     C                   MOVEL     '0'           STRRIM                         *NON STAMPA RIC MEDIO
176300000000     C                   MOVEL     DRI(8)        P2DFAS                         *DESCRIZIONE VALORE
176400010511 EUR C* --->
176500010511     C*    $RT           DIV       1000          $VA                            *RICAVI TOTALI
176600010511     C                   Z-ADD     $RT           $VA                            *RICAVI TOTALI
176700010511 EUR C* <---
176800000000     C* SUL BUDGET
176900010718     C                   MOVEL     '1'           STEBDG                         *CALCOLO % BUDGET
177000010511 EUR C* --->
177100010511     C*    RBS           DIV       1000          $VR                            *RICAVI BUDGET
177200010511     C                   Z-ADD     RBS           $VR                            *RICAVI BUDGET
177300010511 EUR C* <---
177400000000     C                   EXSR      STRIGP
177500010511 EUR C* --->
177600010515     C*    RIFERI        MULT      1000          TOTRBD           19 5          *13° TOT RICAVI BDG
177700010515     C                   Z-ADD     RIFERI        TOTRBD           19 5          *13° TOT RICAVI BDG
177800010511 EUR C* <---
177900000000     C*
178000000000     C* SULL' ANNO PRECEDENTE
178100010718     C                   MOVEL     '0'           STEBDG                         *CALCOLO % A.P.
178200010511 EUR C* --->
178300010511     C*    RPS           DIV       1000          $VR                            *RICAVI ANNO PRECED
178400010511     C                   Z-ADD     RPS           $VR                            *RICAVI ANNO PRECED
178500010511 EUR C* <---
178600000000     C                   EXSR      STRIGP
178700010511 EUR C* --->
178800010515     C*    RIFERI        MULT      1000          TOTRAP           19 5          *13° TOT RICAVI A.P.
178900010515     C                   Z-ADD     RIFERI        TOTRAP           19 5          *13° TOT RICAVI A.P.
179000010511 EUR C* <---
179100000000     C*--------------------
179200000000     C* S P E D I Z I O N I  T O T A L I
179300000000     C*--------------------
179400000000     C                   MOVEL     '0'           STRRIC                         *NON STAMPA RICAVI
179500000000     C                   MOVEL     '0'           STRRIM                         *NON STAMPA RIC MEDIO
179600000000     C                   MOVEL     '1'           STRRIT                         *STAMPA SPEDIZ TOTALI
179700000000     C                   MOVEL     DRI(6)        P1DFAS                         *DESCRIZIONE VALORE
179800000000     C*
179900000000     C* STAMPA LA RIGA VALORI
180000000000     C                   Z-ADD     SFS           $VA                            *VALORE SPEDIZ FRANCO
180100000000     C                   Z-ADD     SAS           $VM                            *VALORE SPEDIZ ASSEGN
180200000000     C                   EXSR      STRIGV
180300000000     C                   Z-ADD     $VA           $ST                            *SPEDIZIONI TOTALI
180400000000     C                   Z-ADD     $VA13C        TOTSPE                         *13° TOT SPEDIZ TOT
180500000000     C*--------------------
180600000000     C* P E R C E N T U A L I  SPEDIZIONI TOTALI
180700000000     C*--------------------
180800000000     C* SUL BUDGET  (CALCOLO LE SPEDIZIONI DEL BUDGET SECONDO LA FORMULA)
180900000000     C                   EXSR      CALSPB
181000000000     C*
181100000000     C                   MOVEL     '0'           STRRIM                         *NON STAMPA RIC MEDIO
181200000000     C                   MOVEL     DRI(8)        P2DFAS                         *DESCRIZIONE VALORE
181300000000     C                   Z-ADD     $ST           $VA                            *SPEDIZIONI TOTALI
181400000000     C*
181500000000     C* SUL BUDGET
181600010718     C                   MOVEL     '1'           STEBDG                         *CALCOLO % BUDGET
181700000000     C                   Z-ADD     $SB           $VR                            SPED BUDGET FORMULATE
181800000000     C                   EXSR      STRIGP
181900000000     C                   Z-ADD     RIFERI        TOTSBD           15 0          *13° TOT SPEDIZ BDG
182000000000     C*
182100000000     C* SULL' ANNO PRECEDENTE
182200010718     C                   MOVEL     '0'           STEBDG                         *CALCOLO % A.P.
182300000000     C                   Z-ADD     SPS           $VR                            *SPEDIZ ANNO PRECED
182400000000     C                   EXSR      STRIGP
182500000000     C                   Z-ADD     RIFERI        TOTSAP           15 0          *13° TOT SPEDIZ A.P.
182600000000     C*--------------------
182700000000     C* R I C A V O  M E D I O  T O T A L E
182800000000     C*--------------------
182900000000     C                   MOVEL     '1'           STRRIC                         *STAMPA RICAVI
183000000000     C                   MOVEL     '1'           STRRIM                         *STAMPA RICAVO MEDIO
183100000000     C                   MOVEL     '0'           STRRIT                         *NON STAMPA RIC TOT
183200000000     C                   MOVEL     DRI(7)        P1DFAS                         *DESCRIZIONE VALORE
183300000000     C*
183400000000     C* STAMPA LA RIGA VALORI
183500000000     C                   Z-ADD     $RT           $VA                            *RICAVI TOTALI
183600000000     C                   Z-ADD     $ST           $VM                            *SPEDIZIONI TOTALI
183700000000     C                   Z-ADD     TOTRIC        RICAV2                         *13° RICAVO
183800000000     C                   Z-ADD     TOTSPE        SPEDIZ                         *13^ SPEDIZIONE
183900000000     C                   EXSR      STRIGV
184000000000     C                   Z-ADD     $VA           $RT                            *TOTALE RICAVI MEDI
184100000000     C                   Z-ADD     $VA13C        TOTRIC                         *13° RICAVO MEDIO
184200000000     C*--------------------
184300000000     C* P E R C E N T U A L I  RICAVO MEDIO
184400000000     C*--------------------
184500000000     C*
184600000000     C                   MOVEL     DRI(8)        P2DFAS                         *DESCRIZIONE VALORE
184700000000     C                   MOVEL     '1'           STRRIM                         *NON STAMPA RIC MEDIO
184800000000     C*
184900000000     C* SUL  B U D G E T
185000000000     C*
185100000000     C* CALCOLO I VALORI MEDI
185200010718     C                   MOVEL     '1'           STEBDG                         *STAMPA % BUDGET
185300000000     C                   Z-ADD     RBS           $VA                            *RICAVI BUDGET
185400000000     C                   Z-ADD     $SB           $VR                            *SPEDIZIONI BUDGET
185500010515     C                   Z-ADD     TOTRBD        VALSUP           19 5          *13° TOT RICAVI BDG
185600000000     C                   Z-ADD     TOTSBD        VALINF           15 0          *13° TOT SPEDIZ BDG
185700000000     C                   EXSR      CALPEM
185800000000     C*
185900000000     C* CALCOLO LA PERCENTUALE
186000000000     C* '$VR' GIA VALORIZZATA DA 'CALPEM'
186100000000     C* '$VA' LA VALORIZZATO SOLO UNA VOLTA PERCHE' NON SI SPORCA
186200000000     C                   Z-ADD     $RT           $VA                            *SCHIERA RICAVI MEDI
186300010515     C                   Z-ADD     TOTRIC        VALCON           19 5          *13° RICAVO MEDIO
186400000000     C                   EXSR      STRIGP
186500000000     C*
186600000000     C* SULL'  A N N O   P R E C E D E N T E
186700000000     C*
186800000000     C* CALCOLO I VALORI MEDI
186900010718     C                   MOVEL     '0'           STEBDG                         *NON STAMPA % BUDGET
187000000000     C                   Z-ADD     RPS           $VA                            *RICAVI ANNO PREC
187100000000     C                   Z-ADD     SPS           $VR                            *SPEDIZIONI ANNO PREC
187200000000     C                   Z-ADD     TOTRAP        VALSUP                         *13° TOT RICAVI A.P.
187300000000     C                   Z-ADD     TOTSAP        VALINF                         *13° TOT SPEDIZ A.P.
187400000000     C                   EXSR      CALPEM
187500000000     C*
187600000000     C* CALCOLO LA PERCENTUALE
187700000000     C                   Z-ADD     $RT           $VA                            *SCHIERA RICAVI MEDI
187800000000     C                   EXSR      STRIGP
187900000000     C*
188000000000     C                   ENDSR
188100000000     C*------------------------------------------------------------------------*
188200010511 xxx C*?STRIGV - STAMPA RIGA VALORE
188300000000     C*          INPUT  -- $VA    : SCHIERA VALORI
188400000000     C*                    $VM    : SCHIERA PER VALORE MEDIO O TOTALE
188500000000     C*                    RICAV2 : TOTALE RICAVI PER CALCOLO RICAVI MEDI
188600000000     C*                    SPEDIZ : TOTALE SPEDIZ PER CALCOLO RICAVI MEDI
188700000000     C*------------------------------------------------------------------------*
188800000000     C     STRIGV        BEGSR
188900000000     C*
189000000000     C* °CALCOLA LA 13^ COLONNA FACENDO LA SOMMA DELLE ALTRE SOLO SE NON STO
189100000000     C*  CALCOLANDO IL RICAVO MEDIO;  PER QUEST' ULTIMO IL VALORE DELLA 13^
189200000000     C*  COLONNA E' DATO DAL RAPPORTO FRA IL 13° RICAVO E LA 13^ SPEDIZIONE
189300000000     C*
189400000000     C*                      RICAVO
189500000000     C*  RICAVO MEDIO°°  =  -----------
189600000000     C*                     SPEDIZIONI
189700000000     C*
189800000000     C*  RICAVI/SPEDIZIONI TOTALI°°°  =  R/S FRANCO + R/S ASSEGNATO
189900000000     C*
190000000000     C* PER OGNI MESE
190100000000DO  1C     1             DO        PARJ          X                 2 0
190200000000     C*
190300000000     C* CALCOLO IL RICAVO TOTALE°°°
190400000000IF  2C     STRRIT        IFEQ      '1'
190500000000     C     $VA(X)        ADD       $VM(X)        $VA(X)
190600010718     C* ACCENDO IL FLAG CHE INDICA DI STAMPARE SENZA DECIMALI
190700010718     C                   EVAL      FLGDEC = '0'
190800000000E   2C                   ENDIF
190900000000     C*
191000000000     C* CALCOLO IL RICAVO MEDIO°°°
191100000000IF  2C     STRRIM        IFEQ      '1'
191200000000     C*
191300000000     C* SE SPEDIZIONI = 0, LE IMPOSTO = 1 PER EVITARE ERRORI NELLA DIVISIONE
191400000000IF  3C     $VM(X)        IFEQ      *ZEROS
191500000000     C                   Z-ADD     1             $VM(X)
191600000000E   3C                   ENDIF
191700000000     C*
191800000000     C     $VA(X)        DIV       $VM(X)        $VA(X)
191900010718     C* ACCENDO IL FLAG CHE INDICA DI STAMPARE CON DECIMALI
192000010718     C                   EVAL      FLGDEC = '1'
192100000000E   2C                   ENDIF
192200010511 EUR C* ---->
192300010511     C* MODIFICA X EURO, NON MILIALIZZO PIU PERCHE' ESPRIMO GIA' IL RICAVO IN EURO CON 3 DECIMALI
192400010511     C*
192500010511     C* CALCOLA LE MIGLIAIA SOLO PER I RICAVI NON MEDI
192600010511IF  2C*    STRRIC        IFEQ      '1'
192700010511     C*    STRRIM        ANDEQ     '0'
192800010515     C*
192900010511     C*    $VA(X)        DIV       1000          $VA(X)
193000010511E   2C*                  ENDIF
193100010511 EUR C* <----
193200010511     C*
193300000000E   1C                   ENDDO
193400000000     C*
193500000000     C* 1 3 ^  C O L O N N A
193600000000     C*
193700000000     C* PER TUTTI I VALORI ESCLUSI I RICAVI MEDI°
193800000000IF  1C     STRRIM        IFEQ      '0'
193900010515     C                   XFOOT     $VA           $VA13C           19 5
194000000000     C*
194100000000     C* PER I RICAVI MEDI°
194200000000X   1C                   ELSE
194300000000     C*
194400000000     C* SE SPEDIZIONI = 0, LE IMPOSTO = 1 PER EVITARE ERRORI NELLA DIVISIONE
194500000000IF  2C     SPEDIZ        IFEQ      *ZEROS
194600000000     C                   Z-ADD     1             SPEDIZ
194700000000E   2C                   ENDIF
194800000000     C*
194900000000     C     RICAV2        DIV       SPEDIZ        $VA13C
195000000000E   1C                   ENDIF
195100000000     C*
195200000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
195300000000     C                   EXSR      STACAV
195400000000     C*
195500000000     C                   ENDSR
195600000000     C*------------------------------------------------------------------------*
195700000000     C*?STRIGP - CALCOLO RIGA PERCENTUALE
195800000000     C*          INPUT  -- $VA    : SCHIERA VALORI CONSUNTIVO
195900000000     C*                    $VR    : SCHIERA VALORI DI RIFERIMENTO
196000000000     C* N.B.               VALCON : VALORE CONSUNTIVO  SE  % RICAVO MEDIO
196100000000     C*                    VALRIF : VALORE RIFERIMENTO SE  % RICAVO MEDIO
196200000000     C*------------------------------------------------------------------------*
196300000000     C     STRIGP        BEGSR
196400000000     C*
196500000000     C* °SE NON HO MEMORIZZATO I VALORI DEL BUDGET PERCHE' HO SELEZIONATO
196600000000     C*  TIPO SERVIZIO O TIPO PRODOTTO O PERCHE' STO STAMPANDO I TOTALI PER
196700000000     C*  FILIALE DETTAGLIO (DATI SUL BDG INESISTENTI) IMPOSTO LA %BDG A ZERO
196800000000     C*
196900000000     C* PER OGNI MESE
197000000000DO  1C     1             DO        PARJ          X                 2 0
197100000000     C*
197200000000     C* CALCOLA LE PERCENTUALI
197300010515     C                   Z-ADD     $VA(X)        CONSUN           19 5          *VALORE CORRENTE
197400010515     C                   Z-ADD     $VR(X)        RIFERI           19 5          *VALORE RIFERIMENTO
197500000000     C*
197600000000     C* SE BUDGET°
197700010718IF  2C     STEBDG        IFEQ      '1'
197800000000IF  3C     NOBDG         IFEQ      '0'
197900000000     C*
198000000000     C* SE NON DEVO IMPOSTARE LA PERCENTUALE A -999,9 LA CALCOLO
198100000000IF  4C     $CP(X)        IFEQ      '0'
198200000000     C                   EXSR      CALPER
198300000000     C                   Z-ADD     PERCEN        $PR(X)                         *VALORE PERCENTUALE
198400000000X   4C                   ELSE
198500000000     C                   MOVEL     -999,9        $PR(X)
198600000000E   4C                   ENDIF
198700000000     C*
198800000000X   3C                   ELSE
198900000000     C                   Z-ADD     *ZEROS        $PR(X)                         *PERCENTUALE A ZERO
199000000000E   3C                   ENDIF
199100000000     C*
199200000000     C* SULL' ANNO PRECEDENTE
199300000000X   2C                   ELSE
199400000000     C                   EXSR      CALPER
199500000000     C                   Z-ADD     PERCEN        $PR(X)                         *VALORE PERCENTUALE
199600000000     C*
199700000000E   2C                   ENDIF
199800000000E   1C                   ENDDO
199900000000     C*
200000000000     C* CALCOLA LE PERCENTUALI DELLA 13^ COLONNA°°
200100000000IF  1C     STRRIM        IFEQ      '0'                                          *NO RICAVO MEDIO
200200000000     C                   XFOOT     $VA           CONSUN
200300000000     C                   XFOOT     $VR           RIFERI
200400000000     C                   ELSE                                                   *RICAVO MEDIO
200500000000     C                   Z-ADD     VALCON        CONSUN
200600000000     C                   Z-ADD     VALRIF        RIFERI
200700000000E   1C                   ENDIF
200800000000     C*
200900000000     C* SUL BUDGET°
201000010718IF  1C     STEBDG        IFEQ      '1'
201100000000IF  2C     NOBDG         IFEQ      '0'
201200000000     C*
201300000000     C* SE NON DEVO IMPOSTARE LA PERCENTUALE A -999,9 LA CALCOLO
201400000000IF  3C     $CP13C        IFEQ      '0'
201500000000     C                   EXSR      CALPER
201600000000     C                   Z-ADD     PERCEN        $PR13C            4 1          *VALORE PERCENTUALE
201700000000X   3C                   ELSE
201800000000     C                   Z-ADD     -999,9        $PR13C
201900000000E   3C                   ENDIF
202000000000     C*
202100000000X   2C                   ELSE
202200000000     C                   Z-ADD     *ZEROS        $PR13C                         *PERCENTUALE A ZERO
202300000000E   2C                   ENDIF
202400000000     C*
202500000000     C* SULL' ANNO PRECEDENTE
202600000000X   1C                   ELSE
202700000000     C                   EXSR      CALPER
202800000000     C                   Z-ADD     PERCEN        $PR13C                         *VALORE PERCENTUALE
202900000000E   1C                   ENDIF
203000000000     C*
203100000000     C* MUOVE I CAMPI DELLE PERCENTUALI IN QUELLI DI STAMPA E STAMPA
203200000000     C                   EXSR      STACAP
203300000000     C*
203400000000     C                   ENDSR
203500000000     C*------------------------------------------------------------------------*
203600000000     C*?CALSPB - CALCOLO LE SPEDIZIONI DEL BUDGET
203700000000     C*          INPUT  -- SFS    : SPEDIZIONI CONSUNTIVO FRANCO PARTENZA
203800000000     C*                    SAS    : SPEDIZIONI CONSUNTIVO ASSEGNATO ARRIVO
203900000000     C*                    SOS    : SPEDIZIONI BUDGET PARTENZA
204000000000     C*                    SIS    : SPEDIZIONI BUDGET ARRIVO
204100000000     C*                    SXS    : SPEDIZIONI CONSUNTIVO FRANCO ARRIVO
204200000000     C*                    SYS    : SPEDIZIONI CONSUNTIVO ASSEGNATO PARTENZA
204300000000     C*          OUTPUT -- $SB    : SPEDIZIONI BUDGET CALCOLATI CON LA FORMULA
204400000000     C*                    $CP    : INDICAZIONE SE DEVO IMPOSTARE LA % A -999,9
204500000000     C*------------------------------------------------------------------------*
204600000000     C     CALSPB        BEGSR
204700000000     C*
204800000000     C*                 (A)                                 (1)
204900000000     C*     SPEDIZIONI BUDGET PARTENZA  x  SPEDIZIONI CONSUNTIVO PARTENZA FRANCO
205000000000     C* X = --------------------------------------------------------------------
205100000000     C*                 CONSUNTIVO PARTENZA (FRANCO + ASSEGNATO)
205200000000     C*
205300000000     C*                 (B)                                 (2)
205400000000     C*     SPEDIZIONI BUDGET ARRIVO  x  SPEDIZIONI CONSUNTIVO ARRIVO ASSEGNATO
205500000000     C* Y = --------------------------------------------------------------------
205600000000     C*                 CONSUNTIVO ARRIVO (ASSEGNATO + FRANCO)
205700000000     C*
205800000000     C* S P E D I Z I O N I = (X + Y)
205900000000     C*
206000000000     C* SE (A) > 0  E  (1) = 0    O P P U R E    SE (B) > 0  E  (2) = 0
206100000000     C* LA PERCENTUALE SUL BUDGET IN STAMPA DEVE ESSERE IMPOSTATA A -999,9
206200000000     C*
206300000000DO  1C     1             DO        12            X
206400000000     C                   MOVEL     '0'           $CP(X)                         *IMPOSTO A NO -999,9%
206500000000E   1C                   ENDDO
206600000000     C                   MOVEL     '0'           $CP13C                         *ANCHE LA 13^ %
206700000000     C* PER OGNI MESE
206800000000DO  1C     1             DO        PARJ          X                 2 0
206900000000     C*
207000000000IF  2C     SOS(X)        IFGT      *ZEROS                                       *(A) > 0
207100000000     C     SFS(X)        ANDEQ     *ZEROS                                       *(1) = 0
207200000000     C     SIS(X)        ORGT      *ZEROS                                       *(B) > 0
207300000000     C     SAS(X)        ANDEQ     *ZEROS                                       *(2) = 0
207400000000     C                   MOVEL     '1'           $CP(X)                         *SI % A -999,9
207500000000E   2C                   ENDIF
207600000000     C*
207700000000     C     SOS(X)        MULT      SFS(X)        COMX1            15 0
207800000000     C     SFS(X)        ADD       SXS(X)        COMX2            15 0
207900000000IF  2C     COMX1         IFEQ      *ZEROS
208000000000     C     COMX2         OREQ      *ZEROS
208100000000     C                   Z-ADD     *ZEROS        COMX
208200000000X   2C                   ELSE
208300000000     C     COMX1         DIV       COMX2         COMX             15 0          *ADDENDO X
208400000000E   2C                   ENDIF
208500000000     C*
208600000000     C     SIS(X)        MULT      SAS(X)        COMY1            15 0
208700000000     C     SAS(X)        ADD       SYS(X)        COMY2            15 0
208800000000IF  2C     COMY1         IFEQ      *ZEROS
208900000000     C     COMY2         OREQ      *ZEROS
209000000000     C                   Z-ADD     *ZEROS        COMY
209100000000X   2C                   ELSE
209200000000     C     COMY1         DIV       COMY2         COMY             15 0          *ADDENDO Y
209300000000E   2C                   ENDIF
209400000000     C*
209500000000     C     COMX          ADD(H)    COMY          $SB(X)                         *SPEDIZIONI BUDGET
209600000000     C*
209700000000E   1C                   ENDDO
209800000000     C*
209900000000     C* 13^ SPEDIZIONE BUDGET
210000000000     C                   XFOOT     SOS           SOS13C           15 0
210100000000     C                   XFOOT     SFS           SFS13C           15 0
210200000000     C                   XFOOT     SIS           SIS13C           15 0
210300000000     C                   XFOOT     SAS           SAS13C           15 0
210400000000IF  1C     SOS13C        IFGT      *ZEROS                                       *(A) > 0
210500000000     C     SFS13C        ANDEQ     *ZEROS                                       *(1) = 0
210600000000     C     SIS13C        ORGT      *ZEROS                                       *(B) > 0
210700000000     C     SAS13C        ANDEQ     *ZEROS                                       *(2) = 0
210800000000     C                   MOVEL     '1'           $CP13C            1            *SI % A -999,9
210900000000E   1C                   ENDIF
211000000000     C*
211100000000     C                   ENDSR
211200000000     C*------------------------------------------------------------------------*
211300000000     C*?CALPEM - CALCOLO VALORI MEDI DEL BUDGET E DELL'ANNO PRECEDENTE
211400000000     C*          INPUT  -- $VA    : SCHIERA VALORI RICAVI BDG/AP
211500000000     C*                    $VR    : SCHIERA VALORI SPEDIZIONI BDG/AP
211600000000     C*                    VALSUP : 13° TOTALE RICAVI BDG/AP
211700000000     C*                    VALINF : 13° TOTALE SPEDIZ BDG/AP
211800000000     C* N.B.     OUTPUT -- $VR    : SCHIERA RAPPORTI BDG O AP
211900000000     C*                    VALRIF : 13° RAPPORTO BDG O AP
212000000000     C*------------------------------------------------------------------------*
212100000000     C     CALPEM        BEGSR
212200000000     C*
212300000000     C*   RICAVO MEDIO  *  1 0 0
212400000000     C* ----------------------------  - 1 0 0   =  PERC BDG/AP RICAVO MEDIO
212500000000     C*       RICAVO BUDGET/AP
212600000000     C*  !!== ----------------
212700000000     C*  !!   SPEDIZ BUDGET/AP
212800000000     C*  !!
212900000000     C*  !!
213000950525     C*  !!=====> *** QUESTA ROUTINE CALCOLA IL RAPPORTO BDG/BDG  O  AP/AP ***
213100000000     C*
213200000000     C* PER OGNI MESE
213300000000DO  1C     1             DO        PARJ          X                 2 0
213400000000     C*
213500000000     C* SE SPEDIZIONI = 0, LE IMPOSTO = 1 PER EVITARE ERRORI NELLA DIVISIONE
213600000000IF  2C     $VR(X)        IFEQ      *ZEROS
213700000000     C                   Z-ADD     1             $VR(X)
213800000000E   2C                   ENDIF
213900000000     C*
214000000000     C     $VA(X)        DIV       $VR(X)        $VR(X)
214100000000E   1C                   ENDDO
214200000000     C*
214300000000     C* CALCOLA LA 13^ COLONNA
214400000000     C*
214500000000     C* SE SPEDIZIONI = 0, LE IMPOSTO = 1 PER EVITARE ERRORI NELLA DIVISIONE
214600000000IF  1C     VALINF        IFEQ      *ZEROS
214700000000     C                   Z-ADD     1             VALINF
214800000000E   1C                   ENDIF
214900000000     C*
215000000000     C* SUL BUDGET°
215100010718IF  1C     STEBDG        IFEQ      '1'
215200000000IF  2C     NOBDG         IFEQ      '0'
215300010515     C     VALSUP        DIV       VALINF        VALRIF           19 5
215400000000X   2C                   ELSE
215500000000     C                   Z-ADD     *ZEROS        VALRIF
215600000000E   2C                   ENDIF
215700000000     C*
215800000000     C* SULL'ANNO PRECEDENTE
215900000000X   1C                   ELSE
216000000000     C     VALSUP        DIV       VALINF        VALRIF
216100000000E   1C                   ENDIF
216200000000     C*
216300000000     C                   ENDSR
216400000000     C*------------------------------------------------------------------------*
216500000000     C*?STACAV - MUOVE I CAMPI VALORE IN QUELLI DI STAMPA E STAMPA
216600000000     C*------------------------------------------------------------------------*
216700000000     C     STACAV        BEGSR
216800000000     C*
216900000000     C* VALORE
217000010515     C*
217100010515     C* Eseguo le valorizzazioni approssimate nei campi di deposito numerici ed applico l'edit-code
217200010718     C                   IF        FLGDEC = '1'
217300020114     C                   Z-ADD(H)  $VA(1)        d_P1CQT1         10 2           *MESE UNO
217400020214     C                   EVALR     P1CQT1 = %editc(d_P1CQT1:'2')
217500020114     C                   Z-ADD(H)  $VA(2)        d_P1CQT2         10 2           *MESE DUE
217600020214     C                   EVALR     P1CQT2 = %editc(d_P1CQT2:'2')
217700020114     C                   Z-ADD(H)  $VA(3)        d_P1CQT3         10 2           *MESE TRE
217800020214     C                   EVALR     P1CQT3 = %editc(d_P1CQT3:'2')
217900020114     C                   Z-ADD(H)  $VA(4)        d_P1CQT4         10 2           *MESE QUATTRO
218000020214     C                   EVALR     P1CQT4 = %editc(d_P1CQT4:'2')
218100020114     C                   Z-ADD(H)  $VA(5)        d_P1CQT5         10 2           *MESE CINQUE
218200020214     C                   EVALR     P1CQT5 = %editc(d_P1CQT5:'2')
218300020114     C                   Z-ADD(H)  $VA(6)        d_P1CQT6         10 2           *MESE SEI
218400020214     C                   EVALR     P1CQT6 = %editc(d_P1CQT6:'2')
218500020114     C                   Z-ADD(H)  $VA(7)        d_P1CQT7         10 2           *MESE SETTE
218600020214     C                   EVALR     P1CQT7 = %editc(d_P1CQT7:'2')
218700020114     C                   Z-ADD(H)  $VA(8)        d_P1CQT8         10 2           *MESE OTTO
218800020214     C                   EVALR     P1CQT8 = %editc(d_P1CQT8:'2')
218900020114     C                   Z-ADD(H)  $VA(9)        d_P1CQT9         10 2           *MESE NOVE
219000020214     C                   EVALR     P1CQT9 = %editc(d_P1CQT9:'2')
219100020114     C                   Z-ADD(H)  $VA(10)       d_P1CQT0         10 2           *MESE DIECI
219200020214     C                   EVALR     P1CQT0 = %editc(d_P1CQT0:'2')
219300020114     C                   Z-ADD(H)  $VA(11)       d_P1CQTA         10 2           *MESE UNDICI
219400020214     C                   EVALR     P1CQTA = %editc(d_P1CQTA:'2')
219500020114     C                   Z-ADD(H)  $VA(12)       d_P1CQTB         10 2           *MESE DODICI
219600020214     C                   EVALR     P1CQTB = %editc(d_P1CQTB:'2')
219700020114     C                   Z-ADD(H)  $VA13C        d_P1CQTT         10 2           *TOTALE MESI
219800020214     C                   EVALR     P1CQTT = %editc(d_P1CQTT:'2')
219900010515     C                   ELSE
220000020114     C                   Z-ADD(H)  $VA(1)        i_P1CQT1         10 0           *MESE UNO
220100020214     C                   EVALR     P1CQT1 = %editc(i_P1CQT1:'2')
220200020114     C                   Z-ADD(H)  $VA(2)        i_P1CQT2         10 0           *MESE DUE
220300020214     C                   EVALR     P1CQT2 = %editc(i_P1CQT2:'2')
220400020114     C                   Z-ADD(H)  $VA(3)        i_P1CQT3         10 0           *MESE TRE
220500020214     C                   EVALR     P1CQT3 = %editc(i_P1CQT3:'2')
220600020114     C                   Z-ADD(H)  $VA(4)        i_P1CQT4         10 0           *MESE QUATTRO
220700020214     C                   EVALR     P1CQT4 = %editc(i_P1CQT4:'2')
220800020114     C                   Z-ADD(H)  $VA(5)        i_P1CQT5         10 0           *MESE CINQUE
220900020214     C                   EVALR     P1CQT5 = %editc(i_P1CQT5:'2')
221000020114     C                   Z-ADD(H)  $VA(6)        i_P1CQT6         10 0           *MESE SEI
221100020214     C                   EVALR     P1CQT6 = %editc(i_P1CQT6:'2')
221200020114     C                   Z-ADD(H)  $VA(7)        i_P1CQT7         10 0           *MESE SETTE
221300020214     C                   EVALR     P1CQT7 = %editc(i_P1CQT7:'2')
221400020114     C                   Z-ADD(H)  $VA(8)        i_P1CQT8         10 0           *MESE OTTO
221500020214     C                   EVALR     P1CQT8 = %editc(i_P1CQT8:'2')
221600020114     C                   Z-ADD(H)  $VA(9)        i_P1CQT9         10 0           *MESE NOVE
221700020214     C                   EVALR     P1CQT9 = %editc(i_P1CQT9:'2')
221800020114     C                   Z-ADD(H)  $VA(10)       i_P1CQT0         10 0           *MESE DIECI
221900020214     C                   EVALR     P1CQT0 = %editc(i_P1CQT0:'2')
222000020114     C                   Z-ADD(H)  $VA(11)       i_P1CQTA         10 0           *MESE UNDICI
222100020214     C                   EVALR     P1CQTA = %editc(i_P1CQTA:'2')
222200020114     C                   Z-ADD(H)  $VA(12)       i_P1CQTB         10 0           *MESE DODICI
222300020214     C                   EVALR     P1CQTB = %editc(i_P1CQTB:'2')
222400020114     C                   Z-ADD(H)  $VA13C        i_P1CQTT         10 0           *TOTALE MESI
222500020214     C                   EVALR     P1CQTT = %editc(i_P1CQTT:'2')
222600010515     C                   ENDIF
222700000000     C*
222800010511     C                   WRITE     SE56P1                                       *STAMPA RIGA VALORE
222900010718     C* REIMPOSTO I ZERO DECIMALI LE ATRE TIPOLOGIE DI VALORI
223000010718     C                   EVAL      FLGDEC = '0'
223100000000     C*
223200000000     C                   ENDSR
223300000000     C*------------------------------------------------------------------------*
223400000000     C*?STACAP - MUOVE I CAMPI PERCENTUALI IN QUELLI DI STAMPA
223500000000     C*------------------------------------------------------------------------*
223600000000     C     STACAP        BEGSR
223700000000     C*
223800000000     C* °STAMPA SOLO QUANDO STO RIEMPENDO I CAMPI DELL'ANNO PRECEDENTE PERCHE'
223900000000     C*  SOLO IN QUESTO MOMENTO TUTTA LA RIGA PERCENTUALI E' VALORIZZATA
224000000000     C*
224100000000     C* PERCENTUALI BUDGET
224200010718IF  1C     STEBDG        IFEQ      '1'
224300000000     C*
224400000000     C                   Z-ADD     $PR(1)        P1CPB1
224500000000     C                   Z-ADD     $PR(2)        P1CPB2
224600000000     C                   Z-ADD     $PR(3)        P1CPB3
224700000000     C                   Z-ADD     $PR(4)        P1CPB4
224800000000     C                   Z-ADD     $PR(5)        P1CPB5
224900000000     C                   Z-ADD     $PR(6)        P1CPB6
225000000000     C                   Z-ADD     $PR(7)        P1CPB7
225100000000     C                   Z-ADD     $PR(8)        P1CPB8
225200000000     C                   Z-ADD     $PR(9)        P1CPB9
225300000000     C                   Z-ADD     $PR(10)       P1CPB0
225400000000     C                   Z-ADD     $PR(11)       P1CPBA
225500000000     C                   Z-ADD     $PR(12)       P1CPBB
225600000000     C                   Z-ADD     $PR13C        P1CPBT
225700000000     C*
225800000000     C* PERCENTUALI ANNO PRECEDENTE°
225900000000X   1C                   ELSE
226000000000     C*
226100000000     C                   Z-ADD     $PR(1)        P1CPA1
226200000000     C                   Z-ADD     $PR(2)        P1CPA2
226300000000     C                   Z-ADD     $PR(3)        P1CPA3
226400000000     C                   Z-ADD     $PR(4)        P1CPA4
226500000000     C                   Z-ADD     $PR(5)        P1CPA5
226600000000     C                   Z-ADD     $PR(6)        P1CPA6
226700000000     C                   Z-ADD     $PR(7)        P1CPA7
226800000000     C                   Z-ADD     $PR(8)        P1CPA8
226900000000     C                   Z-ADD     $PR(9)        P1CPA9
227000000000     C                   Z-ADD     $PR(10)       P1CPA0
227100000000     C                   Z-ADD     $PR(11)       P1CPAA
227200000000     C                   Z-ADD     $PR(12)       P1CPAB
227300000000     C                   Z-ADD     $PR13C        P1CPAT
227400000000     C*
227500010511     C                   WRITE     SE56P2                                       *STAMPA RIGA
227600000000E   1C                   ENDIF
227700000000     C*
227800000000     C                   ENDSR
227900000000     C*------------------------------------------------------------------------*
228000000000     C*?CALPER - CALCOLA LE PERCENTUALI A.P./BDG SUI SALDI
228100000000     C*          INPUT  -- CONSUN: VALORE CORRENTE
228200000000     C*                    RIFERI: VALORE DELL' ANNO PRECEDENTE / BUDGET
228300000000     C*          OUTPUT -- PERCEN: PERCENTUALE CALCOLATA
228400000000     C*------------------------------------------------------------------------*
228500000000     C     CALPER        BEGSR
228600000000     C*
228700000000     C*   VALORE CONSUNTIVO  *  100
228800000000     C*  ---------------------------- - 100 =  % SCOSTAMENTO (AP/BDG)
228900000000     C*   VALORE RIFERIMENTO (AP/BDG)
229000000000     C*
229100000000     C* A) SIA IL VALORE A CONSUNTIVO CHE QUELLO DI RIFERIMENTO SONO = ZERO;
229200000000     C*    % DI SCOSTAMENTO = ZERO
229300000000     C* B) IL VALORE A CONSUNTIVO E' = ZERO MENTRE QUELLO DI RIFERIMENTO NO;
229400000000     C*    % DI SCOSTAMENTO = -999,9
229500000000     C* C) IL VALORE A CONSUNTIVO E' > ZERO MENTRE QUELLO DI RIFERIMENTO E' = 0;
229600000000     C*    % DI SCOSTAMENTO = +999,9
229700000000     C* D) SIA IL VALORE A CONSUNTIVO CHE QUELLO DI RIFERIMENTO SONO > ZERO;
229800000000     C*    % DI SCOSTAMENTO = +/-XXX,X
229900000000     C*
230000000000     C                   SETOFF                                           28
230100000000     C                   Z-ADD     *ZEROS        PERCEN            4 1
230200000000     C*
230300000000IF  1C     CONSUN        IFEQ      *ZEROS
230400000000IF  2C     RIFERI        IFEQ      *ZEROS
230500000000     C                   Z-ADD     *ZEROS        PERCEN                         *CASO A
230600000000X   2C                   ELSE
230700000000     C                   Z-ADD     -999,9        PERCEN                         *CASO B
230800000000E   2C                   ENDIF
230900000000X   1C                   ELSE
231000000000IF  2C     RIFERI        IFEQ      *ZEROS
231100000000     C                   Z-ADD     +999,9        PERCEN                         *CASO C
231200000000X   2C                   ELSE
231300000000     C*
231400010515     C     CONSUN        MULT      100           RISUL            19 5
231500000000     C     RISUL         DIV       RIFERI        RISUL
231600000000     C     RISUL         SUB       100           RISUL
231700000000     C*
231800000000     C* SE LA PERCENTUALE DI SCOSTAMENTO E' +/-1000  METTO  +/-999,9%
231900000000     C                   Z-ADD     RISUL         PERCEN
232000000000     C     RISUL         COMP      0                                    28
232100000000     C* POSITIVO
232200000000IF  3C     *IN28         IFEQ      *OFF
232300000000IF  4C     RISUL         IFGE      1000
232400000000     C                   Z-ADD     +999,9        PERCEN
232500000000E   4C                   ENDIF
232600000000     C* NEGATIVO
232700000000X   3C                   ELSE
232800000000IF  4C     RISUL         IFLE      -1000
232900000000     C                   Z-ADD     -999,9        PERCEN
233000000000E   4C                   ENDIF
233100000000E   3C                   ENDIF
233200000000     C*
233300000000E   2C                   ENDIF
233400000000E   1C                   ENDIF
233500000000     C*
233600000000     C                   ENDSR
233700000000     C*------------------------------------------------------------------------*
233800000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
233900000000     C*------------------------------------------------------------------------*
234000000000     C     DECTES        BEGSR
234100980223     C*
234200950602     C* DATI IN STATISTICA
234300950602IF  1C     PARIOE        IFEQ      'I'
234400950602     C                   MOVEL     PAR(1)        PTCIOE
234500950602E   1C                   ENDIF
234600960105IF  1C     PARIOE        IFEQ      'M'
234700950602     C                   MOVEL     PAR(2)        PTCIOE
234800950602E   1C                   ENDIF
234900960105IF  1C     PARIOE        IFEQ      'X'
235000960105     C                   MOVEL     PAR(11)       PTCIOE
235100960105E   1C                   ENDIF
235200950602IF  1C     PARIOE        IFEQ      *BLANKS
235300950602     C                   MOVEL     PAR(9)        PTCIOE
235400950602E   1C                   ENDIF
235500000000     C* DECODIFICA MESI
235600000000IF  1C     MES(1)        IFGT      *ZEROS
235700000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ.
235800000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
235900990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
236000020207     C                   SETON                                        3151
236100000000E   1C                   ENDIF
236200000000     C*
236300000000IF  1C     MES(2)        IFGT      *ZEROS
236400000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ.
236500000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
236600990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
236700020207     C                   SETON                                        3252
236800000000E   1C                   ENDIF
236900000000     C*
237000000000IF  1C     MES(3)        IFGT      *ZEROS
237100000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ.
237200000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
237300990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
237400020207     C                   SETON                                        3353
237500000000E   1C                   ENDIF
237600000000     C*
237700000000IF  1C     MES(4)        IFGT      *ZEROS
237800000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ.
237900000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
238000990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
238100020207     C                   SETON                                        3454
238200000000E   1C                   ENDIF
238300000000     C*
238400000000IF  1C     MES(5)        IFGT      *ZEROS
238500000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ.
238600000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
238700990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
238800020207     C                   SETON                                        3555
238900000000E   1C                   ENDIF
239000000000     C*
239100000000IF  1C     MES(6)        IFGT      *ZEROS
239200000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ.
239300000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
239400020207     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
239500020207     C                   SETON                                        3656
239600000000E   1C                   ENDIF
239700000000     C*
239800000000IF  1C     MES(7)        IFGT      *ZEROS
239900000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ.
240000000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
240100990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
240200020207     C                   SETON                                        3757
240300000000E   1C                   ENDIF
240400000000     C*
240500000000IF  1C     MES(8)        IFGT      *ZEROS
240600000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ.
240700000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
240800990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
240900020207     C                   SETON                                        3858
241000000000E   1C                   ENDIF
241100000000     C*
241200000000IF  1C     MES(9)        IFGT      *ZEROS
241300000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ.
241400000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
241500990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
241600020207     C                   SETON                                        3959
241700000000E   1C                   ENDIF
241800000000     C*
241900000000IF  1C     MES(10)       IFGT      *ZEROS
242000000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ.
242100000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
242200990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
242300020207     C                   SETON                                        4060
242400000000E   1C                   ENDIF
242500000000     C*
242600000000IF  1C     MES(11)       IFGT      *ZEROS
242700000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ.
242800000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
242900990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
243000020207     C                   SETON                                        4161
243100000000E   1C                   ENDIF
243200000000     C*
243300000000IF  1C     MES(12)       IFGT      *ZEROS
243400000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ.
243500000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
243600990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
243700020207     C                   SETON                                        4262
243800000000E   1C                   ENDIF
243900000000     C*
244000000000     C* DECODIFICA FASCE DI PESO
244100990531IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
244200990217     C                   MOVEL     'SDR'         WSX               3
244300020325X   1C                   ELSE                                                   *FASCE DAL '99 AL 2001
244400020325     C                   MOVEL     'S9R'         WSX
244500020325E   1C                   ENDIF
244600020325     C*
244700000000     C                   Z-ADD     1             IN
244800020906     C                   Z-ADD     1             KTBUTE
244900000000     C                   MOVEL     '2L'          KTBCOD
245000000000     C     KEYTA2        SETLL     TABEL00F
245100000000     C     KEYTA2        READE     TABEL00F                               99
245200000000DO  1C     *IN99         DOWEQ     *OFF
245300000000     C                   MOVEL     TBLKEY        FILEST            3            *FILE STATISTICA
245400990217IF  2C     FILEST        IFEQ      WSX
245500000000     C                   MOVEL     TBLUNI        FPE(IN)
245600000000     C                   ADD       1             IN
245700000000E   2C                   ENDIF
245800000000     C     KEYTA2        READE     TABEL00F                               99
245900000000E   1C                   ENDDO
246000000000     C*
246100000000     C                   ENDSR
246200000000     C*------------------------------------------------------------------------*
246300000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
246400000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
246500000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
246600000000     C*------------------------------------------------------------------------*
246700000000     C     DECDIV        BEGSR
246800000000     C*
246900020906     C                   MOVEL     1             KTBUTE
247000000000     C                   MOVE      '17'          KTBCOD
247100000000     C                   MOVE      *BLANKS       KTBKEY
247200000000     C                   MOVEL     DEPDIV        KTBKEY
247300000000     C     KEYTAB        CHAIN     TABEL00F                           99
247400000000     C*
247500000000IF  1C     *IN99         IFEQ      '1'
247600000000     C     TBLFLG        OREQ      '*'
247700000000     C                   MOVEL     *ALL'*'       DESDIV           20
247800000000X   1C                   ELSE
247900000000     C                   MOVEL     TBLUNI        DESDIV
248000000000E   1C                   ENDIF
248100000000     C*
248200000000     C                   ENDSR
248300000000     C*------------------------------------------------------------------------*
248400000000     C*? DECARE - DECODIFICA CODICE AREA
248500000000     C*          INPUT  -- DEPARE: CODICE AREA
248600000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
248700000000     C*------------------------------------------------------------------------*
248800000000     C     DECARE        BEGSR
248900000000     C*
249000020906     C                   MOVEL     1             KTBUTE
249100000000     C                   MOVE      '05'          KTBCOD
249200000000     C                   MOVE      *BLANKS       KTBKEY
249300000000     C                   MOVEL     DEPARE        KTBKEY
249400000000     C     KEYTAB        CHAIN     TABEL00F                           99
249500000000     C*
249600000000IF  1C     *IN99         IFEQ      '1'
249700000000     C     TBLFLG        OREQ      '*'
249800000000     C                   MOVEL     *ALL'*'       DESARE           20
249900000000X   1C                   ELSE
250000000000     C                   MOVEL     TBLUNI        DESARE
250100000000E   1C                   ENDIF
250200000000     C*
250300000000     C                   ENDSR
250400000000     C*------------------------------------------------------------------------*
250500000000     C*? DECFIL - DECODIFICA CODICE FILIALE
250600000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
250700000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
250800000000     C*------------------------------------------------------------------------*
250900000000     C     DECFIL        BEGSR
251000000000     C*
251100000000     C                   Z-ADD     1             I
251200000000     C     DEPFIL        LOOKUP    FIL(I)                                 99
251300000000IF  1C     *IN99         IFEQ      '0'
251400000000     C                   MOVEL     *ALL'*'       DESFIL           20
251500020325     C                   MOVEL     *BLANKS       LAVNTX
251600000000X   1C                   ELSE
251700000000     C                   MOVEL     DFIL(I)       DESFIL
251800000707     C                   MOVEL     NTX(I)        LAVNTX                         *NETWORK
251900000000E   1C                   ENDIF
252000000000     C*
252100000000     C                   ENDSR
252200000000     C*------------------------------------------------------------------------*
252300000000     C*?CARORG - CARICA I CODICI DELLE FILIALI E LE LORO DECODIFICHE
252400000000     C*------------------------------------------------------------------------*
252500000000     C     CARORG        BEGSR
252600000000     C*
252700000000     C                   Z-ADD     *ZEROS        KORFIL
252800000000     C     KEYOR2        SETLL     AZORG01L                           99        *RECORD NON TROVATO
252900000000     C  N99              READ      AZORG01L                               99
253000000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
253100000000DO  1C     *IN99         DOWEQ     '0'
253200000000IF  2C     ORGFVA        IFEQ      *BLANKS
253300000000IF  3C     ORGFAG        IFNE      'V'
253400000707     C                   CLEAR                   OG143
253500000707     C                   MOVEL     ORGDE3        OG143
253600000000     C                   ADD       1             I
253700000000     C                   Z-ADD     ORGFIL        FIL(I)
253800000000     C                   MOVEL     ORGDES        DFIL(I)
253900020325     C* CALCOLA NETWORK
254000020325     C                   MOVEL     §OGNTW        NTX(I)                         *NETWORK
254100020325     C* VERIFICA SE TRATTASI DI I/E
254200020325     C                   IF        §OGNTW = 'DPD'
254300020325     C                   MOVEL     'E'           FL1(I)
254400020325     C                   ELSE
254500020325     C                   EXSR      CHKNTW
254600020325     C                   MOVEL     §NTWFIE       FL1(I)
254700020325     C                   ENDIF
254800020325     C*
254900000000E   3C                   ENDIF
255000000000E   2C                   ENDIF
255100000000     C                   READ      AZORG01L                               99
255200000000E   1C                   ENDDO
255300000000     C*
255400000000     C                   ENDSR
255500020325     C*------------------------------------------------------------------------*
255600020325     C* CHKNTW - CONTROLLA IL NETWORK
255700020325     C*------------------------------------------------------------------------*
255800020325     C     CHKNTW        BEGSR
255900020325     C*
256000020325     C* VERIFICA VALORE IN TABELLA
256100020325     C                   CLEAR                   DNTW
256200020325     C                   CLEAR                   TIBS02
256300020325     C                   MOVEL     'NTW'         T02COD
256400020325     C                   MOVEL     KNSIF         T02SIF
256500020325     C                   MOVEL     §OGNTW        T02KE1
256600020325     C                   MOVEL     'C'           T02MOD
256700020325     C                   CALL      'TIBS02R'
256800020325     C                   PARM                    KPJBA
256900020325     C                   PARM                    TIBS02
257000020325     C*
257100020325     C     T02ERR        IFEQ      *BLANKS
257200020325     C                   MOVEL     T02UNI        DNTW
257300020325     C                   ENDIF
257400020325     C*
257500020325     C                   ENDSR
257600000000     C*------------------------------------------------------------------------*
257700000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
257800000000     C*------------------------------------------------------------------------*
257900000000     C     STAPAR        BEGSR
258000000000     C*
258100000000     C* STAMPA INTESTAZIONE
258200010511     C                   WRITE     SE56PT
258300000000     C*
258400000000     C* DECODIFICHE PARAMETRI FILTRO
258500000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
258600000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
258700000000     C*
258800000000IF  1C     PARDET        IFEQ      'S'
258900000000     C                   MOVEL     'SI'          PACDET                         *SI DETTAGLIO
259000000000X   1C                   ELSE
259100000000     C                   MOVEL     'NO'          PACDET                         *NO DETTAGLIO
259200000000E   1C                   ENDIF
259300000000     C*
259400000000IF  1C     PARRNC        IFEQ      'S'
259500000000     C                   MOVEL     'SI'          PACRNC                         *SI AGGIORNAMENTO
259600000000X   1C                   ELSE
259700000000     C                   MOVEL     'NO'          PACRNC                         *NO AGGIORNAMENTO
259800000000E   1C                   ENDIF
259900000000     C*
260000000000IF  1C     PARFPC        IFEQ      'S'
260100000000     C                   MOVEL     'SI'          PACFPC                         *SI CREAZIONE FILE PC
260200000000X   1C                   ELSE
260300000000     C                   MOVEL     'NO'          PACFPC                         *NO CREAZIONE FILE PC
260400000000E   1C                   ENDIF
260500000000     C*
260600000000IF  1C     PARFIL        IFGT      *ZEROS
260700000000     C                   Z-ADD     PARFIL        DEPFIL
260800000000     C                   EXSR      DECFIL
260900000000     C                   Z-ADD     *ZEROS        DEPFIL
261000000000     C                   MOVEL     PARFIL        PACFIL                         *FILIALE
261100000000     C                   MOVEL     DESFIL        PADFIL
261200000000X   1C                   ELSE
261300000000     C                   MOVEL     *BLANKS       PACFIL
261400000000E   1C                   ENDIF
261500000000     C*
261600000000IF  1C     PARARE        IFGT      *ZEROS
261700000000     C                   Z-ADD     PARARE        DEPARE
261800000000     C                   EXSR      DECARE
261900000000     C                   Z-ADD     *ZEROS        DEPARE
262000000000     C                   MOVEL     PARARE        PACARE                         *AREA
262100000000     C                   MOVEL     DESARE        PADARE
262200000000X   1C                   ELSE
262300000000     C                   MOVEL     *BLANKS       PACARE
262400000000E   1C                   ENDIF
262500000000     C*
262600000000IF  1C     PARDIV        IFNE      *BLANKS
262700000000     C                   MOVEL     PARDIV        DEPDIV
262800000000     C                   EXSR      DECDIV
262900000000     C                   MOVEL     *BLANKS       DEPDIV
263000000000     C                   MOVEL     PARDIV        PACDIV                         *DIVISIONE
263100000000     C                   MOVEL     DESDIV        PADDIV
263200000000E   1C                   ENDIF
263300000000     C*
263400090605      * decodifico tipo servizio richiesto
263500090605     c                   if        parser <> *blanks
263600090605     c                   clear                   ds5e
263700090605     c                   eval      ktbcod = '5E'
263800090605     c                   eval      ktbkey = parser
263900090605     c     keytab        chain     tabel00f
264000090605     c                   if        %found(tabel00f)
264100090605     c                   eval      ds5e = tbluni
264200090605     c                   endif
264300090605     c                   eval      padser = §5edes
264400090605     c                   else
264500090605     c                   eval      padser = 'TUTTI'
264600090605     c                   endif
264700000000     C*
264800000000IF  1C     PARPRO        IFEQ      'B'
264900000000     C                   MOVEL     PAR(6)        PADPRO                         *PRODOTTO BUSTA
265000000000E   1C                   ENDIF
265100000000     C*
265200000000IF  1C     PARPRO        IFEQ      'P'
265300000000     C                   MOVEL     PAR(7)        PADPRO                         *PRODOTTO PARCEL
265400000000E   1C                   ENDIF
265500000000     C*
265600000000IF  1C     PARPRO        IFEQ      ' '
265700000000     C                   MOVEL     PAR(8)        PADPRO                         *TUTTI
265800000000E   1C                   ENDIF
265900950525     C*
266000960105     C                   MOVEL     PARIOE        PACIOE                         *"I"/"M"/"X "/" "
266100950525IF  1C     PARIOE        IFEQ      'I'
266200950602     C                   MOVEL     PAR(1)        PADIOE                         *DATI ITALIA
266300950525E   1C                   ENDIF
266400960105IF  1C     PARIOE        IFEQ      'M'
266500960105     C                   MOVEL     PAR(2)        PADIOE                         *DATI IMPORT
266600950525E   1C                   ENDIF
266700960105IF  1C     PARIOE        IFEQ      'X'
266800960105     C                   MOVEL     PAR(11)       PADIOE                         *DATI EXPORT
266900960105E   1C                   ENDIF
267000950525IF  1C     PARIOE        IFEQ      *BLANKS
267100950602     C                   MOVEL     PAR(9)        PADIOE                         *TUTTI I DATI
267200950525E   1C                   ENDIF
267300000707     C*
267400000707IF  1C     PARPOS        IFEQ      'S'
267500000707     C                   MOVEL     'SI'          PADPOS                         *DATI POSTE
267600000707X   1C                   ELSE
267700000707     C                   MOVEL     'NO'          PADPOS                         *DATI POSTE
267800000707E   1C                   ENDIF
267900000000     C* STAMPA PARAMETRI
268000010511     C                   WRITE     SE56PA
268100000000     C*
268200000000     C                   ENDSR
268300000000     C*------------------------------------------------------------------------*
268400000000     C*?CANFPC - CANCELLAZIONE FILE SALDI RICAVI PER PC
268500000000     C*------------------------------------------------------------------------*
268600000000     C     CANFPC        BEGSR
268700000000     C*
268800960105     C* SE E' STATA RICHIESTA LA STATISTICA PER ITALIA/IMP/EXP, CANCELLA SOLO I
268900960105     C* RECORD INTERESSATI ("I", "M" O "X");
269000960105     C* SE E'STATA RICHIESTA LA STATISTICA CON TUTTI I DATI NSIEME, CANCELLA
269100950525     C* TUTTO L'ARCHIVIO.
269200960105IF  1C     PARIOE        IFEQ      *BLANKS                                      *TUTTI I DATI INSIEME
269300950525     C                   Z-ADD     45            LUNG             15 5
269400950525     C                   MOVEL     *BLANKS       COMMAN           80
269500000000     C                   MOVEA     CMD1(1)       COMMAN
269600000000     C                   CALL      'QCMDEXC'                                    *CLRPFM
269700000000     C                   PARM                    COMMAN
269800000000     C                   PARM                    LUNG
269900950525E   1C                   ENDIF
270000000000     C* APERTURA FILE
270100010718     C                   OPEN      SEPCR01L
270200950525     C*
270300960105IF  1C     PARIOE        IFNE      *BLANKS                                      *"I"/"M"/"X"
270400010718     C                   READ      SEPCR01L                               99
270500950525DO  2C     *IN99         DOWEQ     *OFF
270600950525IF  3C     PCRIOE        IFEQ      PARIOE
270700000707IF  4C     PARPOS        IFEQ      'S'                                          *DATI POSTE
270800020416     C     PCRNTP        ANDEQ     'PPT'                                         NW PART POSTE
270900000707     C     PARPOS        OREQ      'S'                                          *DATI POSTE
271000020416     C     PCRNTA        ANDEQ     'PPT'                                         NW ARR POSTE
271100000707     C     PARPOS        OREQ      'N'                                          *DATI NO POSTE
271200020416     C     PCRNTP        ANDNE     'PPT'                                         NW PART NO POSTE
271300020416     C     PCRNTA        ANDNE     'PPT'                                         NW ARR NO POSTE
271400010718     C                   DELETE    SEPCR000
271500000707E   4C                   ENDIF
271600000707E   3C                   ENDIF
271700010718     C                   READ      SEPCR01L                               99
271800950525E   2C                   ENDDO
271900950525E   1C                   ENDIF
272000000000     C*
272100000000     C                   ENDSR
272200000707     C*------------------------------------------------------------------------*
272300000707     C*?SCRPCR - SCRIVE FILE RICAVI PER PC
272400000707     C*------------------------------------------------------------------------*
272500000707     C     SCRPCR        BEGSR
272600000707     C*
272700000707     C* SCHIERE SALDI MENSILI DI FILIALE DETTAGLIO
272800000707IF  1C     PASDET        IFEQ      '1'                                          *MEMORIZZATO QC
272900000707DO  2C     1             DO        10            A
273000000707     C     A             OCCUR     FPDET
273100000707     C*
273200000707     C* SCHIERE SALDI MENSILI DI FILIALE DETTAGLIO
273300000707     C                   Z-ADD     DEPFIL        KCRFIL                         *FILIALE PARTENZA
273400000707     C                   MOVEL     PARIOE        KCRIOE                         *DATI IN STATISTICA
273500000707     C                   MOVEL     A             KCRFPE                         *FASCIA DI PESO
273600000707     C                   MOVEL     DEPNTF        KCRNTP                         *NETWORK PARTENZA
273700000707     C                   MOVEL     DEPNTT        KCRNTA                         *NETWORK ARRIVO
273800010718     C     KEYPCR        CHAIN     SEPCR01L                           98
273900000707IF  3C     *IN98         IFEQ      *ON                                          I N S E R I M E N T O
274000010718     C                   CLEAR                   SEPCR000                       *AZZERA FORMATO RECORD
274100000707     C                   Z-ADD     KCRFIL        PCRFIL                         *CHIAVE
274200000707     C                   MOVEL     KCRIOE        PCRIOE
274300000707     C                   MOVEL     KCRFPE        PCRFPE
274400000707     C                   MOVEL     KCRNTP        PCRNTP
274500000707     C                   MOVEL     KCRNTA        PCRNTA
274600000707     C                   EXSR      ADDPCR                                       *AGGIUNGE DATI RECORD
274700010718     C                   WRITE     SEPCR000
274800000707X   3C                   ELSE                                                   M O D I F I C A
274900000707     C                   EXSR      ADDPCR                                       *AGGIUNGE DATI RECORD
275000010718     C                   UPDATE    SEPCR000
275100000707E   3C                   ENDIF
275200000707     C*
275300000707E   2C                   ENDDO
275400000707E   1C                   ENDIF
275500000707     C*
275600000707     C                   ENDSR
275700000707     C*------------------------------------------------------------------------*
275800010718     C*?ADDPCR - AGGIUNGE DATI AL RECORD DI SEPCR00F
275900000707     C*------------------------------------------------------------------------*
276000010511     C*
276100010511     C* MODIFICATA ROUTINE X GESTIONE EURO: NON VENGONO PIU' DIVISI GLI IMPORTI PER 1000
276200010511     C*                                     MA SOMMATI IN CAMPI SEMPLICEMENTE INTERI
276300010511     C*                                     => PER CUI SI AVRANNO NON PIU' CAMPI ALLE 1000 LIRE MA ALLE 2000 LIRE (1EUR)
276400010511     C*
276500000707     C     ADDPCR        BEGSR
276600000707     C*
276700000707     C                   Z-ADD     *ZEROS        N7                7 0
276800000000     C*
276900010511 EUR C* --->
277000010511     C*    RFT(1)        DIV       1000          N7
277100010511     C*                  ADD       N7            PCRRF1                         *RICAVI FRANCO 1° MM
277200010511     C                   ADD       RFT(1)        PCRRF1                         *RICAVI FRANCO 1° MM
277300010511     C*    RFT(2)        DIV       1000          N7
277400010511     C*                  ADD       N7            PCRRF2                         *RICAVI FRANCO 2° MM
277500010511     C                   ADD       RFT(2)        PCRRF2                         *RICAVI FRANCO 2° MM
277600010511     C*    RFT(3)        DIV       1000          N7
277700010511     C*                  ADD       N7            PCRRF3                         *RICAVI FRANCO 3° MM
277800010511     C                   ADD       RFT(3)        PCRRF3                         *RICAVI FRANCO 3° MM
277900010511     C*    RFT(4)        DIV       1000          N7
278000010511     C*                  ADD       N7            PCRRF4                         *RICAVI FRANCO 4° MM
278100010511     C                   ADD       RFT(4)        PCRRF4                         *RICAVI FRANCO 4° MM
278200010511     C*    RFT(5)        DIV       1000          N7
278300010511     C*                  ADD       N7            PCRRF5                         *RICAVI FRANCO 5° MM
278400010511     C                   ADD       RFT(5)        PCRRF5                         *RICAVI FRANCO 5° MM
278500010511     C*    RFT(6)        DIV       1000          N7
278600010511     C*                  ADD       N7            PCRRF6                         *RICAVI FRANCO 6° MM
278700010511     C                   ADD       RFT(6)        PCRRF6                         *RICAVI FRANCO 6° MM
278800010511     C*    RFT(7)        DIV       1000          N7
278900010511     C*                  ADD       N7            PCRRF7                         *RICAVI FRANCO 7° MM
279000010511     C                   ADD       RFT(7)        PCRRF7                         *RICAVI FRANCO 7° MM
279100010511     C*    RFT(8)        DIV       1000          N7
279200010511     C*                  ADD       N7            PCRRF8                         *RICAVI FRANCO 8° MM
279300010511     C                   ADD       RFT(8)        PCRRF8                         *RICAVI FRANCO 8° MM
279400010511     C*    RFT(9)        DIV       1000          N7
279500010511     C*                  ADD       N7            PCRRF9                         *RICAVI FRANCO 9° MM
279600010511     C                   ADD       RFT(9)        PCRRF9                         *RICAVI FRANCO 9° MM
279700010511     C*    RFT(10)       DIV       1000          N7
279800010511     C*                  ADD       N7            PCRRF0                         *RICAVI FRANCO 10°MM
279900010511     C                   ADD       RFT(10)       PCRRF0                         *RICAVI FRANCO 10°MM
280000010511     C*    RFT(11)       DIV       1000          N7
280100010511     C*                  ADD       N7            PCRRFA                         *RICAVI FRANCO 11°MM
280200010511     C                   ADD       RFT(11)       PCRRFA                         *RICAVI FRANCO 11°MM
280300010511     C*    RFT(12)       DIV       1000          N7
280400010511     C*                  ADD       N7            PCRRFB                         *RICAVI FRANCO 12°MM
280500010511     C                   ADD       RFT(12)       PCRRFB                         *RICAVI FRANCO 12°MM
280600010511 EUR C* <---
280700960530     C*
280800000714     C                   ADD       SFT(1)        PCRSF1                         *SPEDIZ FRANCO 1° MM
280900000714     C                   ADD       SFT(2)        PCRSF2                         *SPEDIZ FRANCO 2° MM
281000000714     C                   ADD       SFT(3)        PCRSF3                         *SPEDIZ FRANCO 3° MM
281100000714     C                   ADD       SFT(4)        PCRSF4                         *SPEDIZ FRANCO 4° MM
281200000714     C                   ADD       SFT(5)        PCRSF5                         *SPEDIZ FRANCO 5° MM
281300000714     C                   ADD       SFT(6)        PCRSF6                         *SPEDIZ FRANCO 6° MM
281400000714     C                   ADD       SFT(7)        PCRSF7                         *SPEDIZ FRANCO 7° MM
281500000714     C                   ADD       SFT(8)        PCRSF8                         *SPEDIZ FRANCO 8° MM
281600000714     C                   ADD       SFT(9)        PCRSF9                         *SPEDIZ FRANCO 9° MM
281700000714     C                   ADD       SFT(10)       PCRSF0                         *SPEDIZ FRANCO 10°MM
281800000714     C                   ADD       SFT(11)       PCRSFA                         *SPEDIZ FRANCO 11°MM
281900000714     C                   ADD       SFT(12)       PCRSFB                         *SPEDIZ FRANCO 12°MM
282000000000     C*
282100010511 EUR C* --->
282200010511     C*    RAT(1)        DIV       1000          N7
282300010511     C*                  ADD       N7            PCRRA1                         *RICAVI ASSEGN 1° MM
282400010511     C                   ADD       RAT(1)        PCRRA1                         *RICAVI ASSEGN 1° MM
282500010511     C*    RAT(2)        DIV       1000          N7
282600010511     C*                  ADD       N7            PCRRA2                         *RICAVI ASSEGN 2° MM
282700010511     C                   ADD       RAT(2)        PCRRA2                         *RICAVI ASSEGN 2° MM
282800010511     C*    RAT(3)        DIV       1000          N7
282900010511     C*                  ADD       N7            PCRRA3                         *RICAVI ASSEGN 3° MM
283000010511     C                   ADD       RAT(3)        PCRRA3                         *RICAVI ASSEGN 3° MM
283100010511     C*    RAT(4)        DIV       1000          N7
283200010511     C*                  ADD       N7            PCRRA4                         *RICAVI ASSEGN 4° MM
283300010511     C                   ADD       RAT(4)        PCRRA4                         *RICAVI ASSEGN 4° MM
283400010511     C*    RAT(5)        DIV       1000          N7
283500010511     C*                  ADD       N7            PCRRA5                         *RICAVI ASSEGN 5° MM
283600010511     C                   ADD       RAT(5)        PCRRA5                         *RICAVI ASSEGN 5° MM
283700010511     C*    RAT(6)        DIV       1000          N7
283800010511     C*                  ADD       N7            PCRRA6                         *RICAVI ASSEGN 6° MM
283900010511     C                   ADD       RAT(6)        PCRRA6                         *RICAVI ASSEGN 6° MM
284000010511     C*    RAT(7)        DIV       1000          N7
284100010511     C*                  ADD       N7            PCRRA7                         *RICAVI ASSEGN 7° MM
284200010511     C                   ADD       RAT(7)        PCRRA7                         *RICAVI ASSEGN 7° MM
284300010511     C*    RAT(8)        DIV       1000          N7
284400010511     C*                  ADD       N7            PCRRA8                         *RICAVI ASSEGN 8° MM
284500010511     C                   ADD       RAT(8)        PCRRA8                         *RICAVI ASSEGN 8° MM
284600010511     C*    RAT(9)        DIV       1000          N7
284700010511     C*                  ADD       N7            PCRRA9                         *RICAVI ASSEGN 9° MM
284800010511     C                   ADD       RAT(9)        PCRRA9                         *RICAVI ASSEGN 9° MM
284900010511     C*    RAT(10)       DIV       1000          N7
285000010511     C*                  ADD       N7            PCRRA0                         *RICAVI ASSEGN 10°MM
285100010511     C                   ADD       RAT(10)       PCRRA0                         *RICAVI ASSEGN 10°MM
285200010511     C*    RAT(11)       DIV       1000          N7
285300010511     C*                  ADD       N7            PCRRAA                         *RICAVI ASSEGN 11°MM
285400010511     C                   ADD       RAT(11)       PCRRAA                         *RICAVI ASSEGN 11°MM
285500010511     C*    RAT(12)       DIV       1000          N7
285600010511     C*                  ADD       N7            PCRRAB                         *RICAVI ASSEGN 12°MM
285700010511     C                   ADD       RAT(12)       PCRRAB                         *RICAVI ASSEGN 12°MM
285800010511 EUR C* <---
285900000000     C*
286000000714     C                   ADD       SAT(1)        PCRSA1                         *SPEDIZ ASSEGN 1° MM
286100000714     C                   ADD       SAT(2)        PCRSA2                         *SPEDIZ ASSEGN 2° MM
286200000714     C                   ADD       SAT(3)        PCRSA3                         *SPEDIZ ASSEGN 3° MM
286300000714     C                   ADD       SAT(4)        PCRSA4                         *SPEDIZ ASSEGN 4° MM
286400000714     C                   ADD       SAT(5)        PCRSA5                         *SPEDIZ ASSEGN 5° MM
286500000714     C                   ADD       SAT(6)        PCRSA6                         *SPEDIZ ASSEGN 6° MM
286600000714     C                   ADD       SAT(7)        PCRSA7                         *SPEDIZ ASSEGN 7° MM
286700000714     C                   ADD       SAT(8)        PCRSA8                         *SPEDIZ ASSEGN 8° MM
286800000714     C                   ADD       SAT(9)        PCRSA9                         *SPEDIZ ASSEGN 9° MM
286900000714     C                   ADD       SAT(10)       PCRSA0                         *SPEDIZ ASSEGN 10°MM
287000000714     C                   ADD       SAT(11)       PCRSAA                         *SPEDIZ ASSEGN 11°MM
287100000714     C                   ADD       SAT(12)       PCRSAB                         *SPEDIZ ASSEGN 12°MM
287200000000     C*
287300000000     C                   ENDSR
287400000000     C*------------------------------------------------------------------------*
287500000000     C*?CARTAB - CARICA I DATI IN TABELLE
287600000000     C*------------------------------------------------------------------------*
287700000000     C     CARTAB        BEGSR
287800000000     C*-------------------------
287900000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
288000000000     C*-------------------------
288100020906     C                   Z-ADD     1             KTBUTE
288200000000     C                   MOVEL     '2W'          KTBCOD
288300000000     C     KEYTA2        SETLL     TABEL00F
288400000000     C     KEYTA2        READE     TABEL00F                               99
288500000000     C*
288600000000     C                   Z-ADD     *ZEROS        G                 4 0
288700000000DO  1C     *IN99         DOWEQ     *OFF
288800000000IF  2C     TBLFLG        IFEQ      *BLANKS
288900000000     C                   ADD       1             G
289000000000     C                   MOVEL     TBLKEY        FLAB(G)
289100000000     C                   MOVEL     TBLUNI        DS2W
289200000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
289300000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
289400000000E   2C                   ENDIF
289500000000     C*
289600000000     C     KEYTA2        READE     TABEL00F                               99
289700000000E   1C                   ENDDO
289800000000     C*
289900000000     C                   ENDSR
290000950713     C*------------------------------------------------------------------------*
290100950713     C*?AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
290200950713     C*          OUTPUT--> MES  : MESI DA ELABORARE
290300950713     C*                    ANN  : ANNI DEI MESI DA ELABORARE
290400950713     C*                    PARJ : NUMERO DEI MESI DA ELABORARE
290500950713     C*------------------------------------------------------------------------*
290600950713     C     AMELAB        BEGSR
290700950713     C*
290800950713     C                   CLEAR                   ANN                            *ANNI
290900950713     C                   CLEAR                   MES                            *MESI
291000950713     C                   Z-ADD     *ZEROS        PARJ              2 0          *N° MESI DA ELABORARE
291100950713     C*
291200950713     C* ANNO INIZIALE UGUALE AD ANNO FINALE
291300950713IF  1C     WAA2          IFEQ      WAA
291400950713     C*
291500950713     C* CALCOLO I MESI DI DIFFERENZA
291600950713DO  2C     WMM           DOWLE     WMM2
291700950713     C                   ADD       1             JJ
291800950713     C                   Z-ADD     WAA           ANN(JJ)
291900950713     C                   Z-ADD     WMM           MES(JJ)
292000950713     C                   ADD       1             WMM
292100950713E   2C                   ENDDO
292200950713     C*
292300950713     C                   Z-ADD     JJ            PARJ                           *PARAMETRO
292400950713     C*
292500950713     C* IL RESTO DELLA SCHIERA A ZERO
292600950713DO  2C     JJ            DOWLT     12
292700950713     C                   ADD       1             JJ
292800950713     C                   Z-ADD     *ZEROS        ANN(JJ)
292900950713     C                   Z-ADD     *ZEROS        MES(JJ)
293000950713E   2C                   ENDDO
293100950713     C*
293200950713     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
293300950713X   1C                   ELSE
293400950713     C*
293500950713     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
293600950713DO  2C     WMM           DOWLE     12
293700950713     C                   ADD       1             JJ
293800950713     C                   Z-ADD     WAA           ANN(JJ)
293900950713     C                   Z-ADD     WMM           MES(JJ)
294000950713     C                   ADD       1             WMM
294100950713E   2C                   ENDDO
294200950713     C*
294300950713     C* MEMORIZZO I MESI DELL'ANNO FINALE
294400950713     C                   Z-ADD     01            WMM
294500950713DO  2C     WMM           DOWLE     WMM2
294600950713     C                   ADD       1             JJ
294700950713     C                   Z-ADD     WAA2          ANN(JJ)
294800950713     C                   Z-ADD     WMM           MES(JJ)
294900950713     C                   ADD       1             WMM
295000950713E   2C                   ENDDO
295100950713     C*
295200950713     C                   Z-ADD     JJ            PARJ                           *PARAMETRO
295300950713     C*
295400950713     C* IL RESTO DELLA SCHIERA A ZERO
295500950713DO  2C     JJ            DOWLT     12
295600950713     C                   ADD       1             JJ
295700950713     C                   Z-ADD     *ZEROS        ANN(JJ)
295800950713     C                   Z-ADD     *ZEROS        MES(JJ)
295900950713E   2C                   ENDDO
296000950713E   1C                   ENDIF
296100950713     C*
296200950713     C                   ENDSR
296300000000     C*------------------------------------------------------------------------*
296400000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
296500000000     C*------------------------------------------------------------------------*
296600000000     C     *INZSR        BEGSR
296700950525     C*--------------------
296800950525     C* RICEVIMENTO PARAMETRI
296900950525     C*--------------------
297000000000     C     *ENTRY        PLIST
297100000000     C                   PARM                    KPJBA
297200000000     C                   MOVEL     KPJBU         PARAM
297300950525     C*--------------------
297400950525     C* VARIABILI RIFERITE AL DATA BASE
297500950525     C*--------------------
297600000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
297700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
297800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
297900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
298000000000     C     *LIKE         DEFINE    ORGFL3        KORDIV
298100000000     C     *LIKE         DEFINE    ORGFIL        KSDFIT
298200000000     C     *LIKE         DEFINE    SDRANN        KSDANN
298300000000     C     *LIKE         DEFINE    SDRMES        KSDMES
298400000000     C     *LIKE         DEFINE    ORGFIL        KSDFIL
298500000000     C     *LIKE         DEFINE    TBGFIL        KBDFIL
298600000000     C     *LIKE         DEFINE    TBGANN        KBDANN
298700000000     C     *LIKE         DEFINE    TBGMES        KBDMES
298800960108     C     *LIKE         DEFINE    TBGIOE        KBDIOE
298900010718     C     *LIKE         DEFINE    PCRFIL        KCRFIL                         *SEPCR00F
299000000707     C     *LIKE         DEFINE    PCRIOE        KCRIOE
299100000707     C     *LIKE         DEFINE    PCRFPE        KCRFPE
299200000707     C     *LIKE         DEFINE    PCRNTP        KCRNTP
299300000707     C     *LIKE         DEFINE    PCRNTA        KCRNTA
299400971204     C     *LIKE         DEFINE    ORGFL3        DEPDIV                         *DEPOSITI
299500000707     C     *LIKE         DEFINE    PCRNTP        DEPNTF
299600000707     C     *LIKE         DEFINE    PCRNTP        DEPNTT
299700971204     C     *LIKE         DEFINE    ORGCAR        DEPARE
299800971204     C     *LIKE         DEFINE    ORGFIL        DEPFIL
299900971204     C     *LIKE         DEFINE    ORGFL1        DLPFL1
300000971204     C     *LIKE         DEFINE    ORGFL1        DLAFL1
300100000707     C     *LIKE         DEFINE    PCRNTP        LAVNTX
300200950525     C*--------------------
300300950525     C* VARIABILI DI CONTROLLO
300400950525     C*--------------------
300500950525     C                   MOVEL     '0'           $FINE             1            *FINE PGM
300600950525     C                   MOVEL     '0'           $FIN1             1            *FINE SALDI ANNO CORR
300700950525     C                   MOVEL     '0'           $FIN2             1            *FINE SALDI ANNO PREC
300800950525     C                   MOVEL     '0'           $FIN3             1            *FINE VALORI BUDGET
300900950525     C                   MOVEL     '0'           PASDET            1            *MEMOR SALDI FIL DET
301000950525     C                   MOVEL     '0'           PASFIL            1            *MEMOR SALDI FILIALE
301100950525     C                   MOVEL     '0'           PASARE            1            *MEMOR SALDI AREA
301200950525     C                   MOVEL     '0'           PASDIV            1            *MEMOR SALDI DIVISION
301300950525     C                   MOVEL     '0'           PASGEN            1            *MEMOR SALDI GENERALI
301400950525     C                   MOVEL     '0'           NOBDG             1            *'1'-NO BDG; '0' SI
301500950525     C                   MOVEL     '0'           STRRIC            1            STAMPA RIGA RICAVI
301600950525     C                   MOVEL     '0'           STRRIM            1            STAMPA RICAVO MEDIO
301700950525     C                   MOVEL     '0'           STRRIT            1            STAMPA RICAVO TOTALE
301800010718     C                   MOVEL     '0'           STEBDG            1            STAMPA % BUDGET
301900950525     C                   MOVEL     ' '           CICLO             1            CICLO LETTURA (P/A)
302000950525     C                   MOVEL     ' '           TIPANN            1            TIPO ANNI  (U-UGUALI)
302100010515     C                   MOVEL     '0'           FLGDEC            1            FLAG DI STAMPA CON O SENZA DECIMALI
302200950525     C                   Z-ADD     UDATE         DATA              6 0          DATA CORRENTE
302300950525     C                   TIME                    ORA               6 0          ORA  CORRENTE
302400950525     C*--------------------
302500950525     C* CHIAVI DI LETTURA
302600950525     C*--------------------
302700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
302800000000     C     KEYTAB        KLIST
302900000000     C                   KFLD                    KTBUTE
303000000000     C                   KFLD                    KTBCOD
303100000000     C                   KFLD                    KTBKEY
303200000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
303300000000     C     KEYTA2        KLIST
303400000000     C                   KFLD                    KTBUTE
303500000000     C                   KFLD                    KTBCOD
303600000000     C* CHIAVE LETTURA AZORG02L - PARZIALE
303700000000     C     KEYORG        KLIST
303800000000     C                   KFLD                    KORDIV                         *DIVISIONE
303900000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
304000000000     C     KEYOR2        KLIST
304100000000     C                   KFLD                    KORFIL                         *FILIALE
304200990609     C* CHIAVE LETTURA SISDR01L/SISDR02L - COMPLETA
304300000000     C     KEYSDR        KLIST
304400000000     C                   KFLD                    KSDANN                         *ANNO
304500000000     C                   KFLD                    KSDFIL                         *FILIALE
304600000000     C                   KFLD                    KSDFIT                         *FILIALE DETTAGLIO
304700000000     C                   KFLD                    KSDMES                         *MESE
304800990609     C* CHIAVE LETTURA SISDR01L/SISDR02L - PARZIALE
304900000000     C     KEYSD2        KLIST
305000000000     C                   KFLD                    KSDANN                         *ANNO
305100000000     C                   KFLD                    KSDFIL                         *FILIALE
305200000000     C                   KFLD                    KSDFIT                         *FILIALE DETTAGLIO
305300010718     C* CHIAVE LETTURA TEBDG01L - PARZIALE
305400000000     C     KEYBDG        KLIST
305500000000     C                   KFLD                    KBDFIL                         *FILIALE
305600000000     C                   KFLD                    KBDANN                         *ANNO
305700000000     C                   KFLD                    KBDMES                         *MESE
305800000707     C                   KFLD                    KBDIOE                         *DATI BUDGET
305900010718     C* CHIAVE LETTURA SEPCR01L - COMPLETA
306000000707     C     KEYPCR        KLIST
306100000707     C                   KFLD                    KCRFIL                         *FILIALE
306200000707     C                   KFLD                    KCRIOE                         *DATI STATISTICA
306300000707     C                   KFLD                    KCRFPE                         *FASCIA DI PESO
306400000707     C                   KFLD                    KCRNTP                         *NETWORK PARTENZA
306500000707     C                   KFLD                    KCRNTA                         *NETWORK ARRIVO
306600000000     C*
306700000000     C* MEMORIZZA I CODICI DELLE FILIALI E LE LORO DECODIFICHE
306800000000     C                   EXSR      CARORG
306900000000     C*
307000000000     C* CANCELLAZIONE E APERTURA FILE PC SE RICHIESTA CREAZIONE
307100000000IF  1C     PARFPC        IFEQ      'S'
307200000000     C                   EXSR      CANFPC
307300000000E   1C                   ENDIF
307400960108     C*
307500950911SL   C* CALCOLA I MESI DA ELABORARE
307600990531     C                   Z-ADD     ANNFIN        WAA2              4 0          *ANNO FINALE LAVORO
307700990531     C                   Z-ADD     MESFIN        WMM2              2 0          *MESE FINALE LAVORO
307800990531     C                   Z-ADD     ANNINI        WAA               4 0          *ANNO INIZIALE LAVORO
307900990531     C                   Z-ADD     MESINI        WMM               2 0          *MESE INIZIALE LAVORO
308000990610     C                   Z-ADD     *ZEROS        JJ                4 0          *N° MESI DA ELABORARE
308100990531     C                   EXSR      AMELAB
308200000000     C*
308300000000     C* DECODIFICA INTESTAZIONE
308400000000     C                   EXSR      DECTES
308500000000     C*
308600000000     C* CARICA TABELLE
308700000000     C                   EXSR      CARTAB
308800000000     C*
308900000000     C* RIEMPO SCHIERA CON ANNO INIZIALE E ANNO FINALE PER LETTURA SALDI CORR.
309000000000     C                   Z-ADD     ANNINI        AIF(1)                         *ANNO INIZIALE
309100000000     C                   Z-ADD     ANNFIN        AIF(2)                         *ANNO FINALE
309200000000     C*
309300000000     C* E SCHIERA CON ANNO INIZIALE E ANNO FINALE - UNO, PER LETTURA SALDI PREC.
309400000000     C     ANNINI        SUB       1             PIF(1)                         *ANNO INIZIALE - 1
309500000000     C     ANNFIN        SUB       1             PIF(2)                         *ANNO FINALE - 1
309600000000     C*
309700000000     C* SE AA INIZIALE =  AA FINALE, QUEST'ULTIMO IN SCHIERA LO IMPOSTO A ZERO
309800000000IF  1C     ANNINI        IFEQ      ANNFIN
309900000000     C                   Z-ADD     *ZEROS        AIF(2)                         *ANNO FINALE
310000000000     C                   Z-ADD     *ZEROS        PIF(2)                         *ANNO FINALE - 1
310100000000     C                   MOVEL     'U'           TIPANN                         *ANNI UGUALI
310200000000E   1C                   ENDIF
310300000000     C*
310400000000     C                   ENDSR
310500000000     O*------------------------------------------------------------------------*
310600000707**   PAR - DECODIFICHE PARAMETRI
310700950602ITALIA         1
310800960105IMPORT         2
310900090605..libero       3
311000090605..libero       4
311100090605..libero       5
311200950602BUSTE          6
311300950602PARCEL         7
311400950602TUTTI          8
311500950712COMPLETA       9
311600950602TUTTE          10
311700960105EXPORT         11
31180096010512
31190096010513
31200096010514
31210096010515
312200000000**   DMM - DECODIFICHE MESI DELL'ANNO
312300000000GENNAIO                        1
312400000000FEBBRAIO                       2
312500000000MARZO                          3
312600000000APRILE                         4
312700000000MAGGIO                         5
312800000000GIUGNO                         6
312900000000LUGLIO                         7
313000000000AGOSTO                         8
313100000000SETTEMBRE                      9
313200000000OTTOBRE                        10
313300000000NOVEMBRE                       11
313400000000DICEMBRE                       12
313500000000**   DRI - DECODIFICHE RIGA DI STAMPA
313600000000      / RICAVI                 1
313700000000FRANCO  N.SPED                 2
313800000000      \ MEDIO                  3
313900000000ASSEGN  N.SPED                 4
314000000000RICAVI  TOTALI                 5
314100000000N.SPED  TOTALI                 6
314200000000R.MEDIO TOTALE                 7
314300000000 %BDG    %A.P.                 8
314400000000 TOTALI                        9
314500000000**   CMD1
314600010718CLRPFM FILE(*LIBL/SEPCR00F)
