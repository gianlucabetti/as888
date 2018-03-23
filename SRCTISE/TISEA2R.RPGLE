000100130130     /*END
000200000000     H*------------------------------------------------------------------------*
000300000000     H*                      - STATISTICA DELTA FILIALE -                      *
000400000000     H*------------------------------------------------------------------------*
000500000000     H* INPUT  : -------- - ----------------------------------------------------
000600020117     H*          PARAM    - PARAMETRI PASSATI DALLA GUIDA 'TISE91C'
000700000000     H*                     PARDAT  -  DATA INIZIALE
000800000000     H*                     PARDA2  -  DATA FINALE
000900950208     H*                     PARIOE  -  DELTA ITALIA/ESTERO/TUTTO
001000160831     H*                     PARPT   -  clienti nuovi S/N
001100000000     H* OUTPUT : -------- - ----------------------------------------------------
001200010507     H*          TISEA2P  - STATISTICA DELTA PER FASCE DI PESO
001300000000     F*------------------------------------------------------------------------*
001400000000     F* DATA BASE
001500000000     F*------------------------------------------------------------------------*
001600000000     H DECEDIT('0,') DATEDIT(*DMY.)
001700990609     FSISDC01L  IF   E           K DISK
001800170214     FWADELPCT1 IF   E           k DISK
001900170213     F*****SISDO01L  IF   E           K DISK    usropn
002000950216     FAZORG02L  IF   E           K DISK
002100000000     FTABEL00F  IF   E           K DISK
002200010507     FTISEA2P   O    E             PRINTER OFLIND(*IN01)
002300000000     D*------------------------------------------------------------------------*
002400000000     D* SCHIERE
002500000000     D*------------------------------------------------------------------------*
002600000000     D*------------------
002700000000     D* DECODIFICA MESI DELL'ANNO
002800000000     D*------------------
002900000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
003000950214     D*-------------------
003100950214     D* FASCE DI PESO
003200950214     D*-------------------
003300020522     D FPE             S              2    DIM(11)                              *CODICI FASCE ESISTENTI
003400020522     D DFPE            S             14    DIM(11)                              *DECODIFICA FASCE
003500950214     D*------------------
003600950214     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
003700950214     D*------------------
003800950214     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
003900950214     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/N)
004000950214     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
004100000000     D*------------------
004200000000     D* ORGANIGRAMMA
004300000000     D*------------------
004400971124     D FIL             S              3  0 DIM(500)                             *CODICI
004500971124     D ARE             S              3  0 DIM(500)
004600971124     D DIV             S              1    DIM(500)
004700971124     D DFIL            S             20    DIM(500)                             *DECODIFICHE
004800971124     D DARE            S             20    DIM(500)
004900971124     D DDIV            S             20    DIM(500)
005000020328     D*------------------
005100020328     D* RAGGRUPPAMENTI REGIONI/NETWORK
005200020328     D*------------------
005300020328     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIONE
005400020328     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RAGGR. REGIONE
005500000000     D*------------------
005600000000     D* MESI DA ELABORARE
005700000000     D*------------------
005800000000     D MES             S              2  0 DIM(12)
005900000000     D*------------------
006000000000     D* ANNI DEI MESI DA ELABORARE
006100000000     D*------------------
006200990524     D ANN             S              4  0 DIM(12)
006300000000    ?D*-------------------
006400950214    ?D* S C H I E R E   M U L T I P L E
006500000000    ?D*-------------------
006600950214     D*-------------------
006700950214     D* DELTA FILIALE
006800950214     D*-------------------
006900950214     D*-------------------
007000950214     D* DELTA AREA
007100950214     D*-------------------
007200950214     D*-------------------
007300950214     D* DELTA DIVISIONE
007400950214     D*-------------------
007500950214     D*-------------------
007600950214     D* DELTA GENERALE
007700950214     D*-------------------
007800950214     D*-------------------
007900950214     D* DELTA IN STAMPA
008000950214     D*-------------------
008100950214    ?D*-------------------
008200000000    ?D* S C H I E R E   N O N   M U L T I P L E
008300000000    ?D*-------------------
008400010515     D TRS             S             19  5 DIM(12)                              STAMPA TOT.RIC.REALE
008500010515     D TPS             S             19  5 DIM(12)                              STAMPA TOT.RIC.PRES.
008600010515     D TSS             S             19  0 DIM(12)                              STAMPA TOT.SPED.
008700010515     D SVA             S             19  5 DIM(12)                              STAMPA VALORI
008800950216     D SDE             S              4  1 DIM(12)                              STAMPA PERC. DELTA
008900000000     D*------------------------------------------------------------------------*
009000000000     D* INPUT
009100000000     D*------------------------------------------------------------------------*
009200950214    ?D*-------------------
009300950214    ?D* S C H I E R E   M U L T I P L E      - 11 FASCE X 12 MESI -
009400950214    ?D*-------------------
009500950214     D*------------------
009600950214     D* NUMERO DI BYTE IN PACKED
009700950214     D*------------------
009800010507     D* (17:2) + 1 = 9 ; (9 X 12) = 108
009900950214     D*------------------
010000950214     D* DELTA FILIALE
010100950214     D*------------------
010200020522     D FDEL            DS                  OCCURS(11) INZ
010300010515     D  DRF                          19  5 DIM(12)                              R REAL
010400010515     D  DPF                          19  5 DIM(12)                              R PRES
010500010515     D  DSF                          17  0 DIM(12)                              SPEDIZ
010600950214     D*------------------
010700950214     D* DELTA AREA
010800950214     D*------------------
010900020522     D ADEL            DS                  OCCURS(11) INZ
011000010515     D  DRA                          19  5 DIM(12)                              R REAL
011100010515     D  DPA                          19  5 DIM(12)                              R PRES
011200010515     D  DSA                          17  0 DIM(12)                              SPEDIZ
011300950214     D*------------------
011400950214     D* DELTA DIVISIONE
011500950214     D*------------------
011600020522     D DDEL            DS                  OCCURS(11) INZ
011700010515     D  DRD                          19  5 DIM(12)                              R REAL
011800010515     D  DPD                          19  5 DIM(12)                              R PRES
011900010515     D  DSD                          17  0 DIM(12)                              SPEDIZ
012000950214     D*------------------
012100950214     D* DELTA GENERALE
012200950214     D*------------------
012300020522     D GDEL            DS                  OCCURS(11) INZ
012400010515     D  DRG                          19  5 DIM(12)                              R REAL
012500010515     D  DPG                          19  5 DIM(12)                              R PRES
012600010515     D  DSG                          17  0 DIM(12)                              SPEDIZ
012700950214     D*------------------
012800950214     D* DELTA IN STAMPA
012900950214     D*------------------
013000020522     D SDEL            DS                  OCCURS(11) INZ
013100010515     D  DRS                          19  5 DIM(12)                              R REAL
013200010515     D  DPS                          19  5 DIM(12)                              R PRES
013300010515     D  DSS                          17  0 DIM(12)                              SPEDIZ
013400950214     D*------------------
013500950214     D* ARCHITETTURA
013600950214     D*------------------
013700000000     D KPJBA         E DS
013800950214     D  PROFIL                 1     10                                         UTENTE
013900950214     D*------------------
014000990609     D* DS UNIONE DATA SISDC01L
014100950214     D*------------------
014200950214     D                 DS
014300990524     D  SDCANN                 1      4  0
014400990524     D  SDCMES                 5      6  0
014500990524     D  SDCDAT                 1      6  0
014600950214     D*------------------
014700950214     D* DS FASCE DI PESO
014800950214     D*------------------
014900950214     D DS2L          E DS
015000950214     D*------------------
015100950214     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
015200950214     D*------------------
015300950214     D DS2W          E DS
015400000000     D*------------------
015500000000     D* PARAMETRI FILTRO
015600000000     D*------------------
015700000000     D PARAM           DS
015800990524     D  PARDAT                 1      6  0
015900990524     D  PARAAI                 1      4  0
016000990524     D  PARMMI                 5      6  0
016100990524     D  PARDA2                 7     12  0
016200990524     D  PARAAF                 7     10  0
016300990524     D  PARMMF                11     12  0
016400990524     D  PARIOE                13     13
016500170213     d  PARPT                 14     14
016600130130     d  ParTOT                15     15                                         *Solo Totali
016700170213     D*------------------
016800170213     D* DS CONTROLLO DATA (8)
016900170213     D*------------------
017000170213     D WLBDA8          DS                  INZ
017100170213     D  G08DAT                 1      8  0
017200170213     D  G08INV                 9     16  0
017300170213     D  G08ERR                17     17
017400170213     D  G08TGI                18     22  0
017500170213     D*------------------
017600170213     D Data_iso        s               d   Datfmt(*iso)
017700170213     D*------------------   CLIENTE UNIFICANTE x reperire NUOVO/ACQUISITO
017800170213     D BS10DS        E DS                  EXTNAME(TIBS10DS)
017900000000     D*------------------
018000020916     D* DS REPERIMENTO DATI UTENTE
018100020916     D*-------------------
018200020916     D TIBS34DS      E DS                                                       *Profili utente
018300020916     D DDATIUTE      E DS                                                       *Dati utente
018400020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
018500000614     D*------------------
018600000614     D* DS LETTURA 143° DESCRIZIO ORGANIGRAMMA (X POSTE)
018700000614     D*------------------
018800000614     D OG143         E DS
018900000614     D*------------------
019000000614     D* DS DI SCOMPOSIZIONE CLIENTE
019100000614     D*------------------
019200000614     D                 DS
019300000614     D  KSCFIL                 1      3  0
019400000614     D  KSCCLI                 4      7  0
019500000614     D  KSCCOD                 1      7  0
019600020328     D*------------------
019700020328     D* DS DI LETTURA TABELLA "2M"
019800020328     D*------------------
019900020328     D DS2M          E DS
020000020523     D*------------------
020100020523     D* VARIABILE INDICANTE IL NUMERO DI FASCE DI PESO
020200020523     D*------------------
020300020523     D FPENUM          S              2  0 INZ(10)
020400000000     C*------------------------------------------------------------------------*
020500950214     C* MAIN LINE
020600000000     C*------------------------------------------------------------------------*
020700000000     C*
020800000000     C* STAMPA PARAMETRI FILTRO
020900000000     C                   EXSR      STAPAR
021000950214     C*
021100950216     C* LETTURA PRIMA FILIALE
021200950216     C                   Z-ADD     1             F                 6 0          *PRIMA FILIALE
021300000000     C*
021400950214     C* CICLO FINO A FINE FILE E FINE FILIALI
021500950216DO  2C     F             DOWLE     ULTFIL
021600000000     C*
021700000000     C* OPERAZIONI PER NUOVA DIVISIONE
021800000000     C                   EXSR      NEWDIV
021900000000     C*
022000000000     C* CICLO FINO A ROTTURA DI DIVISIONE
022100950216DO  3C     F             DOWLE     ULTFIL
022200950214     C     DIV(F)        ANDEQ     DEPDIV
022300000000     C*
022400000000     C* OPERAZIONI PER NUOVA AREA
022500000000     C                   EXSR      NEWARE
022600000000     C*
022700000000     C* CICLO FINO A ROTTURA DI AREA
022800950216DO  4C     F             DOWLE     ULTFIL
022900950214     C     DIV(F)        ANDEQ     DEPDIV
023000950214     C     ARE(F)        ANDEQ     DEPARE
023100000000     C*
023200000000     C* OPERAZIONI PER NUOVA FILIALE
023300000000     C                   EXSR      NEWFIL
023400950214     C*
023500950216     C* LETTURA DEI SALDI DELLA FILIALE
023600950216     C                   EXSR      SETSDC
023700000000     C*
023800950216     C* CICLO FINO A FINE SALDI PER QUESTA FILIALE
023900950216DO  5C     $FINE         DOWEQ     '0'
024000000000     C*
024100000000     C* MEMORIZZA I SALDI
024200000000     C                   EXSR      MEMSAL
024300000000     C*
024400950216     C* LETTURA RECORD SALDI SUCCESSIVO
024500950216     C                   EXSR      LETSDC
024600950214E   5C                   ENDDO
024700950214     C*
024800950214     C* STAMPA IL TOTALE FILIALE
024900130130     c                   if        ParTOT <> 'S'
025000000614     C  N45              EXSR      STAFIL
025100130130     c                   endif
025200950216     C*
025300950216     C* SI POSIZIONA SULLA NUOVA FILIALE
025400950216     C                   ADD       1             F
025500950214E   4C                   ENDDO
025600000000     C*
025700000000     C* STAMPA IL TOTALE AREA
025800000614     C     CHKFIL        IFEQ      '1'
025900130130     c     ParTOT        andNE     'S'
026000000614     C                   EXSR      STAARE
026100000614     C                   ENDIF
026200950214E   3C                   ENDDO
026300000000     C*
026400000000     C* STAMPA IL TOTALE DIVISIONE
026500000614     C     CHKARE        IFEQ      '1'
026600130130     c     ParTOT        andNE     'S'
026700000614     C                   EXSR      STADIV
026800000614     C                   ENDIF
026900950214E   2C                   ENDDO
027000000000     C*
027100000000     C* STAMPA IL TOTALE GENERALE
027200000000     C                   EXSR      STAGEN
027300000000     C*
027400000000     C* STAMPA RIGA FINE STAMPA
027500010507     C                   WRITE     SEA2FI
027600000000     C*
027700000000     C                   SETON                                        LR
027800000000     C*------------------------------------------------------------------------*
027900950216     C* SETSDC - POSIZIONAMENTO E PRIMA LETTURA
028000000000     C*------------------------------------------------------------------------*
028100950216     C     SETSDC        BEGSR
028200950214     C*
028300950216     C                   MOVEL     '0'           $FINE             1
028400950214     C*
028500950216     C                   Z-ADD     9999999       KSCFIN            7 0          *CLIENTE
028600000614     C                   MOVEL     FIL(F)        KSCFIN                         *CLIENTE FINALE
028700000614     C                   Z-ADD     *ZEROS        KSCKSC                         *CLIENTE
028800000614     C                   MOVEL     FIL(F)        KSCKSC                         *FIL+0000
028900950216     C*
029000000614     C     KSCKSC        IFEQ      *ZEROS
029100000614     C                   MOVEL     '1'           $FINE
029200000614     C                   SETON                                        45
029300000614     C                   ELSE
029400000614     C                   SETOFF                                       45
029500000614     C     KEYSDC        SETLL     SISDC01L                           99        *FINE FILIALE
029600000000     C*
029700950214IF  1C     *IN99         IFEQ      *ON
029800000000     C                   MOVEL     '1'           $FINE
029900000614     C                   SETON                                        45
030000000000X   1C                   ELSE
030100950216     C                   EXSR      LETSDC                                       *LETTURA RECORD
030200000000E   1C                   ENDIF
030300000614     C                   ENDIF
030400000000     C*
030500000000     C                   ENDSR
030600000000     C*------------------------------------------------------------------------*
030700950216     C* LETSDC - LETTURA PROSSIMO RECORD
030800000000     C*------------------------------------------------------------------------*
030900950216     C     LETSDC        BEGSR
031000000000     C*
031100000000     C* LEGGO FINO A:
031200000000     C*     - FINE FILE
031300000000     C*     - TROVATO RECORD VALIDO
031400000000     C                   MOVEL     'N'           WRECOK
031500000000DO  1C     $FINE         DOWEQ     '0'
031600000000     C     WRECOK        ANDEQ     'N'
031700000000     C*
031800000000     C* LETTURE SUCCESSIVE ALLA PRIMA
031900990609     C                   READ      SISDC01L                               99
032000000000     C*
032100950214IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
032200950216     C     SDCKSC        ORGT      KSCFIN                                       *FINE FILIALE
032300000000     C                   MOVEL     '1'           $FINE
032400000000X   2C                   ELSE
032500000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
032600000000E   2C                   ENDIF
032700000000     C*
032800000000E   1C                   ENDDO
032900000000     C*
033000000000     C                   ENDSR
033100000000     C*------------------------------------------------------------------------*
033200950214     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
033300000000     C*------------------------------------------------------------------------*
033400000000     C     CHKREC        BEGSR
033500000000     C*
033600000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
033700020516     C*
033800020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
033900020516IF  1C     WRECOK        IFEQ      'S'
034000020516IF  2C     SDCNSP        IFEQ      *ZEROS
034100020516     C     SDCPKG        ANDEQ     *ZEROS
034200020516     C     SDCPCC        ANDEQ     *ZEROS
034300020516     C     SDCPLS        ANDEQ     *ZEROS
034400020516     C     SDCNCO        ANDEQ     *ZEROS
034500020516     C     SDCVOL        ANDEQ     *ZEROS
034600020516     C     SDCVCC        ANDEQ     *ZEROS
034700020516     C     SDCVLS        ANDEQ     *ZEROS
034800020516     C     SDCIRB        ANDEQ     *ZEROS
034900020516     C     SDCIRR        ANDEQ     *ZEROS
035000020516     C     SDCINC        ANDEQ     *ZEROS
035100020516     C     SDCIRP        ANDEQ     *ZEROS
035200020516     C     SDCIRC        ANDEQ     *ZEROS
035300020516     C     SDCIRD        ANDEQ     *ZEROS
035400020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
035500170214     c                   leavesr
035600020516E   2C                   ENDIF
035700020516E   1C                   ENDIF
035800160831     C*
035900160831     C* esclusioni per filiale
036000000614     C                   Z-ADD     SDCKSC        KSCCOD
036100000614     C     KSCFIL        LOOKUP    FIL                                    99
036200000614IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
036300000614     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
036400170214     c                   leavesr
036500000614E   1C                   ENDIF
036600950214     C*
036700950214     C* ESCLUSIONI PER DATA NON RICHIESTA
036800950214IF  1C     SDCDAT        IFLT      PARDAT
036900950214     C     SDCDAT        ORGT      PARDA2
037000950214     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
037100170214     c                   leavesr
037200950214E   1C                   ENDIF
037300020328     C*
037400020328     C* DETERMINA IL NETWORK
037500020328     C                   MOVEL     *BLANKS       DEPNTWP           3
037600020328     C                   Z-ADD     1             R
037700020328     C     SDCREP        LOOKUP    RGREG(R)                               55
037800020328     C                   IF        *IN55 = *ON
037900020328     C                   EVAL      DEPNTWP = RGNTW(R)
038000020328     C                   ENDIF
038100020328     C*
038200020328     C                   MOVEL     *BLANKS       DEPNTWA           3
038300020328     C                   Z-ADD     1             R
038400020328     C     SDCREA        LOOKUP    RGREG(R)                               55
038500020328     C                   IF        *IN55 = *ON
038600020328     C                   EVAL      DEPNTWA = RGNTW(R)
038700020328     C                   ENDIF
038800950214     C*
038900950208     C* ESCLUSIONI PER DELTA ITALIA O ESTERO
039000950216     C     PARIOE        IFNE      *BLANKS
039100020328     C     DEPNTWP       IFNE      *BLANKS
039200020328     C     DEPNTWA       ORNE      *BLANKS
039300950216     C                   MOVEL     'E'           WIOE              1            *ESTERO
039400950216     C                   ELSE
039500950216     C                   MOVEL     'I'           WIOE                           *ITALIA
039600950216     C                   ENDIF
039700950216     C     PARIOE        IFNE      WIOE
039800950208     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
039900170214     c                   leavesr
040000950216     C                   ENDIF
040100950216     C                   ENDIF
040200950216     C*
040300950216     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
040400950216     C                   Z-ADD     1             I
040500950216     C                   SETOFF                                           99
040600950216     C     SDCRBL        LOOKUP    FLAB(I)                                99
040700950216IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
040800950216     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
040900170214     c                   leavesr
041000950216     C                   ELSE
041100971204     C*
041200950216     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
041300950216     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
041400950216IF  1C     FSP(I)        IFNE      'S'
041500950216     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
041600950216E   1C                   ENDIF
041700950216E   1C                   ENDIF
041800950216     C*
041900990217     C* ESCLUDO LE FASCE DI PESO OLTRE 5000 KG
042000000309IF  1C     SDCFPE        IFEQ      '07'
042100000309     C     PARDAT        ANDLT     200001
042200000309     C                   MOVEL     'N'           WRECOK
042300170214     c                   leavesr
042400000309E   1C                   ENDIF
042500160831     C*
042600160831     C* controllo se richiesto solo clienti nuovi
042700160906IF  1C     parpt         IFne      ' '
042800170213     C*
042900170213      ****
043000170213      ****  Prima andava per cliente singolo x decidere se nuovo o acquisito
043100170213      ****    deve invece prendere la stessa logica del delta
043200170213      ****
043300170213     C**** ksdo          chain     sisdo01l
043400170213     c****               if        %found(sisdo01l)
043500170213     c****               if        sdocln <>'Y' and sdocln <> 'S'
043600170213     c****                         and parpt = 'N'
043700170213     C****               MOVEL     'N'           WRECOK
043800170213E   1C****               ENDIF
043900170213      ****
044000170213     c****               if        (sdocln = 'Y' or sdocln = 'S')
044100170213     c****                         and parpt = 'V'
044200170213     C****               MOVEL     'N'           WRECOK
044300170213E   1C****               ENDIF
044400170213E   1C****               ENDIF
044500170213      ****
044600170213     c***  DEVE PRENDERE il CODICE PADRE
044700170214     C* a rottura di codice cliente letto
044800170214     c                   if        sdcksc <> savsdcksc
044900170214     c                   eval        savsdcksc = sdcksc
045000170214      *
045100170213     C* IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-
045200170213     C                   CLEAR                   BS10DS
045300170213     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
045400170213     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
045500170213     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
045600170213     C                   MOVE      sdcksc        D10COD
045700170213     C                   CALL      'TIBS10R'
045800170213     C                   PARM                    BS10DS
045900170213     C                   Z-ADD     sdcksc        UNIcli            7 0
046000170213IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
046100170213     C     D10COP        ANDNE     *ZEROS
046200170213     C                   Z-ADD     D10COP        UNIcli                         *UNIFICANTE
046300170213E   2C                   ENDIF
046400170213      *
046500170214     C* flag NUOVO/ACQUISITO
046600170214     c                   clear                   flag_NA           1
046700170214     c     UniCLI        chain     wadelpct1
046800170214     c                   if        %Found(wadelpct1)
046900170214     c                   eval        flag_NA = wapCLN
047000170214E   1C                   endIF
047100170214     c                   end
047200170214      ****
047300170214      ****   DEVE ESCLUDERE IL RECORD  se non è come quello RICHIESTO
047400170214     c******             if        (parpt = 'N' and flag_NA = 'A')
047500170214     c******                         or
047600170214     c******                       (parpt = 'V' and flag_NA = 'N')
047700170214     c                   if        (parpt = 'N' and flag_NA <>'N')
047800170214     c                               or
047900170214     c                             (parpt = 'V' and flag_NA = 'N')
048000170214     c***********                  (parpt = 'V' and flag_NA <>'A')
048100170214     C                   eval         WRECOK = 'N'
048200170214     c                   leavesr
048300170213E   1C                   endIF
048400170213      ****
048500160831E   1C                   ENDIF
048600950208     C*
048700000000     C                   ENDSR
048800000000     C*------------------------------------------------------------------------*
048900950214     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
049000000000     C*------------------------------------------------------------------------*
049100000000     C     NEWDIV        BEGSR
049200000000     C*
049300000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
049400950216     C                   MOVEL     DIV(F)        DEPDIV            1
049500950216     C                   MOVEL     DDIV(F)       DESDIV           20
049600000000     C*
049700000000     C* AZZERA I TOTALI DELLA DIVISIONE
049800020523     C                   DO        FPENUM        FP                             * X OGNI FASCIA DI PESO
049900950216     C     FP            OCCUR     DDEL
050000950215     C                   CLEAR                   DRD
050100950215     C                   CLEAR                   DPD
050200950215     C                   CLEAR                   DSD
050300950216     C                   ENDDO
050400000000     C*
050500000000     C                   ENDSR
050600000000     C*------------------------------------------------------------------------*
050700000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
050800000000     C*------------------------------------------------------------------------*
050900000000     C     NEWARE        BEGSR
051000000614     C*
051100000614     C* INIZIALIZZO FLAG X CONTROLLO STAMPA
051200000614     C                   MOVEL     '0'           CHKARE            1
051300000000     C*
051400000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
051500950216     C                   Z-ADD     ARE(F)        DEPARE            3 0
051600950216     C                   MOVEL     DARE(F)       DESARE           20
051700000000     C*
051800000000     C* AZZERA I TOTALI DELL'AREA
051900020523     C                   DO        FPENUM        FP                             * X OGNI FASCIA DI PESO
052000950216     C     FP            OCCUR     ADEL
052100950215     C                   CLEAR                   DRA
052200950215     C                   CLEAR                   DPA
052300950215     C                   CLEAR                   DSA
052400950216     C                   ENDDO
052500000000     C*
052600000000     C                   ENDSR
052700000000     C*------------------------------------------------------------------------*
052800000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
052900000000     C*------------------------------------------------------------------------*
053000000000     C     NEWFIL        BEGSR
053100000614     C*
053200000614     C* INIZIALIZZO FLAG X CONTROLLO STAMPA
053300000614     C                   MOVEL     '0'           CHKFIL            1
053400000000     C*
053500000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
053600000614     C                   Z-ADD     FIL(F)        DEPFIL            3 0
053700000614     C                   MOVEL     DFIL(F)       DESFIL           20
053800000000     C*
053900000000     C* AZZERA I TOTALI DELLA FILIALE
054000020523     C                   DO        FPENUM        FP                             * X OGNI FASCIA DI PESO
054100950216     C     FP            OCCUR     FDEL
054200950215     C                   CLEAR                   DRF
054300950215     C                   CLEAR                   DPF
054400950215     C                   CLEAR                   DSF
054500950216     C                   ENDDO
054600000000     C*
054700000000     C                   ENDSR
054800000000     C*------------------------------------------------------------------------*
054900000000     C*?MEMSAL - MEMORIZZA I SALDI DEL WORK FILE IN SCHIERE
055000000000     C*------------------------------------------------------------------------*
055100000000     C     MEMSAL        BEGSR
055200950216     C*
055300950216     C* FASCIA DI PESO
055400950216     C                   Z-ADD     1             FP
055500950216     C                   SETOFF                                           99
055600950216     C     SDCFPE        LOOKUP    FPE(FP)                                99
055700000000     C*
055800000000     C* M E S E
055900950216     C                   Z-ADD     1             MM
056000950216     C                   SETOFF                                           99
056100950216     C     SDCMES        LOOKUP    MES(MM)                                99
056200950216     C*
056300950216     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
056400010508     C                   Z-ADD     *ZEROS        TOTRIC           21 5
056500950216     C     SDCIRD        ADD       SDCIRR        TOTRIC
056600950216     C     SDCINC        ADD       TOTRIC        TOTRIC
056700130130      *
056800130130     c                   If        ParTOT <> 'S'                                NON SOLO TOT. GEN.
056900130130      *
057000000000     C*--------------------
057100000000     C* F I L I A L E
057200000000     C*--------------------
057300950216     C     FP            OCCUR     FDEL
057400950215     C                   ADD       SDCNSP        DSF(MM)                        SPEDIZIONI
057500950216     C                   ADD       TOTRIC        DRF(MM)                        RICAVI REALI DELTA
057600950215     C                   ADD       SDCIRP        DPF(MM)                        RICAVI PRESUNTI DELTA
057700000000     C*--------------------
057800000000     C* A R E A
057900000000     C*--------------------
058000950216     C     FP            OCCUR     ADEL
058100950215     C                   ADD       SDCNSP        DSA(MM)                        SPEDIZIONI
058200950216     C                   ADD       TOTRIC        DRA(MM)                        RICAVI REALI DELTA
058300950215     C                   ADD       SDCIRP        DPA(MM)                        RICAVI PRESUNTI DELTA
058400000000     C*--------------------
058500000000     C* D I V I S I O N E
058600000000     C*--------------------
058700950216     C     FP            OCCUR     DDEL
058800950215     C                   ADD       SDCNSP        DSD(MM)                        SPEDIZIONI
058900950216     C                   ADD       TOTRIC        DRD(MM)                        RICAVI REALI DELTA
059000950215     C                   ADD       SDCIRP        DPD(MM)                        RICAVI PRESUNTI DELTA
059100130130      *
059200130130     c                   EndIf
059300130130      *
059400000000     C*--------------------
059500000000     C* G E N E R A L E
059600000000     C*--------------------
059700950216     C     FP            OCCUR     GDEL
059800950215     C                   ADD       SDCNSP        DSG(MM)                        SPEDIZIONI
059900950216     C                   ADD       TOTRIC        DRG(MM)                        RICAVI REALI DELTA
060000950215     C                   ADD       SDCIRP        DPG(MM)                        RICAVI PRESUNTI DELTA
060100000000     C*
060200000000     C                   ENDSR
060300000000     C*------------------------------------------------------------------------*
060400000000     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
060500000000     C*------------------------------------------------------------------------*
060600000000     C     STAFIL        BEGSR
060700000614     C*
060800000614     C* VALIZZO IL FLAG DI "STAMPATO ALMENO UNA FILIALE"
060900000614     C                   MOVEL     '1'           CHKFIL
061000950216     C*
061100000000     C* DESCRIZIONI INTESTAZIONE
061200010507     C                   CLEAR                   SEA2IN
061300000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
061400000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
061500000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
061600000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
061700000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
061800000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISIONE
061900000000     C*
062000000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
062100020523     C                   DO        FPENUM        FP                6 0
062200950216     C     FP            OCCUR     FDEL
062300950216     C     FP            OCCUR     SDEL
062400950216     C                   Z-ADD     DRF           DRS
062500950216     C                   Z-ADD     DPF           DPS
062600950216     C                   Z-ADD     DSF           DSS
062700950216     C                   ENDDO
062800000000     C*
062900000000     C* ROUTINE DI STAMPA TOTALI
063000000000     C                   EXSR      STAMPA
063100000000     C*
063200000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
063300000000     C                   SETON                                        01
063400000000     C*
063500000000     C                   ENDSR
063600000000     C*------------------------------------------------------------------------*
063700000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
063800000000     C*------------------------------------------------------------------------*
063900000000     C     STAARE        BEGSR
064000000614     C*
064100000614     C* VALIZZO IL FLAG DI "STAMPATO ALMENO UNA AREA"
064200000614     C                   MOVEL     '1'           CHKARE
064300950216     C*
064400000000     C* DESCRIZIONI INTESTAZIONE
064500010507     C                   CLEAR                   SEA2IN
064600000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
064700950216     C                   MOVEL     'TUTTE'       INDFIL                         *DESCRIZIONE FILIALE
064800000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
064900000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
065000000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
065100000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISIONE
065200000000     C*
065300000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
065400020523     C                   DO        FPENUM        FP
065500950216     C     FP            OCCUR     ADEL
065600950216     C     FP            OCCUR     SDEL
065700950216     C                   Z-ADD     DRA           DRS
065800950216     C                   Z-ADD     DPA           DPS
065900950216     C                   Z-ADD     DSA           DSS
066000950216     C                   ENDDO
066100000000     C*
066200000000     C* ROUTINE DI STAMPA TOTALI
066300000000     C                   EXSR      STAMPA
066400000000     C*
066500000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
066600000000     C                   SETON                                        01
066700000000     C*
066800000000     C                   ENDSR
066900000000     C*------------------------------------------------------------------------*
067000000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
067100000000     C*------------------------------------------------------------------------*
067200000000     C     STADIV        BEGSR
067300950216     C*
067400000000     C* DESCRIZIONI INTESTAZIONE
067500010507     C                   CLEAR                   SEA2IN
067600000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
067700950216     C                   MOVEL     'TUTTE'       INDFIL                         *DESCRIZIONE FILIALE
067800000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
067900950216     C                   MOVEL     'TUTTE'       INDARE                         *DESCRIZIONE AREA
068000000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
068100000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISIONE
068200000000     C*
068300000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
068400020523     C                   DO        FPENUM        FP
068500950216     C     FP            OCCUR     DDEL
068600950216     C     FP            OCCUR     SDEL
068700950216     C                   Z-ADD     DRD           DRS
068800950216     C                   Z-ADD     DPD           DPS
068900950216     C                   Z-ADD     DSD           DSS
069000950216     C                   ENDDO
069100000000     C*
069200000000     C* ROUTINE DI STAMPA TOTALI
069300000000     C                   EXSR      STAMPA
069400000000     C*
069500000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
069600000000     C                   SETON                                        01
069700000000     C*
069800000000     C                   ENDSR
069900000000     C*------------------------------------------------------------------------*
070000000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
070100000000     C*------------------------------------------------------------------------*
070200000000     C     STAGEN        BEGSR
070300950216     C*
070400000000     C* DESCRIZIONI INTESTAZIONE
070500010507     C                   CLEAR                   SEA2IN
070600000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
070700950216     C                   MOVEL     'TUTTE'       INDFIL                         *DESCRIZIONE FILIALE
070800000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
070900950216     C                   MOVEL     'TUTTE'       INDARE                         *DESCRIZIONE AREA
071000000000     C                   MOVEL     *BLANKS       INCDIV                         *DIVISIONE
071100950216     C                   MOVEL     'TUTTE'       INDDIV                         DESCRIZIONE DIVISIONE
071200000000     C*
071300000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
071400020523     C                   DO        FPENUM        FP
071500950216     C     FP            OCCUR     GDEL
071600950216     C     FP            OCCUR     SDEL
071700950216     C                   Z-ADD     DRG           DRS
071800950216     C                   Z-ADD     DPG           DPS
071900950216     C                   Z-ADD     DSG           DSS
072000950216     C                   ENDDO
072100000000     C*
072200000000     C* ROUTINE DI STAMPA TOTALI
072300000000     C                   EXSR      STAMPA
072400000000     C*
072500000000     C                   ENDSR
072600000000     C*------------------------------------------------------------------------*
072700000000     C*?STAINT - STAMPA INTESTAZIONE
072800000000     C*------------------------------------------------------------------------*
072900000000     C     STAINT        BEGSR
073000000000     C*
073100010507     C                   WRITE     SEA2PT                                       *TESTATA
073200150331     C                   WRITE     SEA2PRM                                      *pro-memoria parametri
073300010507     C                   WRITE     SEA2IN                                       *RIGA INTESTAZIONE
073400010507     C                   WRITE     SEA2R1                                       *RIGA SEPARAZIONE
073500010507     C                   WRITE     SEA2PI                                       *RIGA MESI
073600010507     C                   WRITE     SEA2R2                                       *RIGA SEPARAZIONE
073700000000     C                   SETOFF                                       01
073800000000     C*
073900000000     C                   ENDSR
074000000000     C*------------------------------------------------------------------------*
074100950216     C*?STAMPA - STAMPA I TOTALI NELLA FORMA: SPE / RIC / DEL
074200950216     C*          INPUT  --> DRS    : SCHIERA RICAVO REALE       MULT.
074300950216     C*                     DPS    : SCHIERA RICAVO PRESUNTO    MULT.
074400950216     C*                     DSS    : SCHIERA SPEDIZIONI         MULT.
074500000000     C*------------------------------------------------------------------------*
074600950216     C     STAMPA        BEGSR
074700000000     C*--------------------
074800000000     C* I N T E S T A Z I O N E
074900000000     C*--------------------
075000000000IF  1C     *IN01         IFEQ      *ON
075100000000     C                   EXSR      STAINT
075200000000E   1C                   ENDIF
075300950216     C*--------------------
075400950216     C* CICLO PER TUTTE LE FASCIE DI PESO
075500950216     C*--------------------
075600950216     C                   CLEAR                   TRS
075700950216     C                   CLEAR                   TPS
075800950216     C                   CLEAR                   TSS
075900020523     C                   DO        FPENUM        FP
076000950216     C     FP            OCCUR     SDEL
076100950216     C*
076200000000     C* S P E D I Z I O N I
076300950216     C                   MOVEL     'SPE'         DES03             3
076400000000     C                   MOVEL     *BLANKS       P1DDES
076500950216     C                   MOVEL     DFPE(FP)      P1DDES
076600000000     C                   MOVE      DES03         P1DDES
076700000000     C*
076800950216     C                   Z-ADD     DSS           SVA                            *SCH.VALORI 12 MESI
076900010508     C                   XFOOT     DSS           SVA13C           21 5          *TOTALE ANNO
077000000000     C*
077100950216     C                   EXSR      SCRVAL                                       *STAMPA RIGA VALORI
077200950216     C*
077300000000     C* R I C A V I
077400950216     C                   MOVEL     'RIC'         DES03             3
077500000000     C                   MOVEL     *BLANKS       P1DDES
077600000000     C                   MOVE      DES03         P1DDES
077700000000     C*
077800010507     C***  DRS           DIV       1000          SVA                            *SCH.VALORI 12 MESI
077900010507     C                   Z-ADD     DRS           SVA                            *SCH.VALORI 12 MESI
078000010507     C                   XFOOT     SVA           SVA13C                         *TOTALE ANNO
078100000000     C*
078200950216     C                   EXSR      SCRVAL                                       *SCRIVE RIGA VALORI
078300950216     C*
078400000000     C* D E L T A
078500950216     C                   MOVEL     'DEL'         DES03             3
078600000000     C                   MOVEL     *BLANKS       P2DDES
078700000000     C                   MOVE      DES03         P2DDES
078800950216     C* DELTA 12 MESI
078900950216DO  1C     1             DO        SPARM         J                 6 0
079000010508     C                   Z-ADD     DRS(J)        WRRW             21 5          *RICAVO REALE
079100010508     C                   Z-ADD     DPS(J)        WRPW             21 5          *RICAVO PRESUNTO
079200950216     C                   EXSR      CALDEL
079300950216     C                   Z-ADD     WDPW          SDE(J)                         *SCH.DELTA IN STAMPA
079400950216E   1C                   ENDDO
079500950216     C* DELTA 13^ COLONNA
079600010507     C                   XFOOT     DRS           WRRW                           *RICAVO REALE
079700010507     C                   XFOOT     DPS           WRPW                           *RICAVO PRESUNTO
079800950216     C                   EXSR      CALDEL
079900950216     C                   Z-ADD     WDPW          SDE13C            4 1          *13^ COLONNA DELTA
080000000000     C*
080100950216     C                   EXSR      SCRDEL                                       *SCRIVE PERCENT.DELTA
080200950216     C*
080300010507     C                   WRITE     SEA2R2                                       *STAMPA RIGA SEPARAZI
080400950216     C*
080500950216     C* MEMORIZZO PER TOTALE DI TUTTE LE FASCIE DI PESO
080600950216     C                   DO        12            I                 6 0
080700950216     C                   ADD       DRS(I)        TRS(I)
080800950216     C                   ADD       DPS(I)        TPS(I)
080900950216     C                   ADD       DSS(I)        TSS(I)
081000950216     C                   ENDDO
081100950216     C*
081200950216     C* FINE CICLO FASCIE DI PESO
081300950216     C                   ENDDO
081400950216     C*
081500950216     C*--------------------
081600950216     C* TOTALE DI TUTTE LE FASCIE DI PESO
081700950216     C*--------------------
081800950216     C* S P E D I Z I O N I
081900950216     C                   MOVEL     'SPE'         DES03             3
082000950216     C                   MOVEL     *BLANKS       P1DDES
082100950216     C                   MOVEL     'TOTALE'      P1DDES
082200950216     C                   MOVE      DES03         P1DDES
082300950216     C*
082400950216     C                   Z-ADD     TSS           SVA                            *SCH.VALORI 12 MESI
082500010508     C                   XFOOT     TSS           SVA13C           21 5          *TOTALE ANNO
082600950216     C*
082700950216     C                   EXSR      SCRVAL                                       *STAMPA RIGA VALORI
082800950216     C*
082900950216     C* R I C A V I
083000950216     C                   MOVEL     'RIC'         DES03             3
083100950216     C                   MOVEL     *BLANKS       P1DDES
083200950216     C                   MOVE      DES03         P1DDES
083300950216     C*
083400010507     C***  TRS           DIV       1000          SVA                            *SCH.VALORI 12 MESI
083500010507     C                   Z-ADD     TRS           SVA                            *SCH.VALORI 12 MESI
083600010507     C                   XFOOT     SVA           SVA13C                         *TOTALE ANNO
083700950216     C*
083800950216     C                   EXSR      SCRVAL                                       *SCRIVE RIGA VALORI
083900950216     C*
084000950216     C* D E L T A
084100950216     C                   MOVEL     'DEL'         DES03             3
084200950216     C                   MOVEL     *BLANKS       P2DDES
084300950216     C                   MOVE      DES03         P2DDES
084400950216     C* DELTA 12 MESI
084500950216DO  1C     1             DO        SPARM         J
084600010507     C                   Z-ADD     TRS(J)        WRRW                           *RICAVO REALE
084700010507     C                   Z-ADD     TPS(J)        WRPW                           *RICAVO PRESUNTO
084800950216     C                   EXSR      CALDEL
084900950216     C                   Z-ADD     WDPW          SDE(J)                         *SCH.DELTA IN STAMPA
085000950216E   1C                   ENDDO
085100950216     C* DELTA 13^ COLONNA
085200010507     C                   XFOOT     TRS           WRRW                           *RICAVO REALE
085300010507     C                   XFOOT     TPS           WRPW                           *RICAVO PRESUNTO
085400950216     C                   EXSR      CALDEL
085500950216     C                   Z-ADD     WDPW          SDE13C                         *13^ COLONNA DELTA
085600950216     C*
085700950216     C                   EXSR      SCRDEL                                       *SCRIVE PERCENT.DELTA
085800950216     C*
085900010507     C                   WRITE     SEA2R2                                       *STAMPA RIGA SEPARAZI
086000000000     C*
086100000000     C                   ENDSR
086200000000     C*------------------------------------------------------------------------*
086300000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
086400000000     C*          INPUT  --> WRRW: RICAVO REALE
086500000000     C*                     WRPW: RICAVO PRESUNTO
086600000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
086700000000     C*------------------------------------------------------------------------*
086800000000     C     CALDEL        BEGSR
086900000000     C*--------------------
087000000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
087100000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
087200000000     C*--------------------
087300000000     C*
087400950216     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
087500000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
087600010508     C     WRRW          SUB       WRPW          GAP              21 5          (REALE - PRESUNTO)
087700000000     C     GAP           MULT      100           GAP                            (GAP x 100)
087800000000     C*
087900000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
088000000000IF  1C     WRRW          IFEQ      *ZEROS
088100000000     C                   Z-ADD     1             WRRW
088200000000E   1C                   ENDIF
088300000000     C*
088400000000     C     GAP           DIV       WRRW          GAP
088500000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
088600000000     C*
088700000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
088800000000     C     WDPW          COMP      0                                    28
088900000000     C* POSITIVO
089000000000IF  1C     *IN28         IFEQ      *OFF
089100000000IF  2C     GAP           IFGE      1000
089200000000     C                   Z-ADD     +999,9        WDPW
089300000000E   2C                   ENDIF
089400000000     C* NEGATIVO
089500000000X   1C                   ELSE
089600000000IF  2C     GAP           IFLE      -1000
089700000000     C                   Z-ADD     -999,9        WDPW
089800000000E   2C                   ENDIF
089900000000E   1C                   ENDIF
090000000000     C*
090100000000     C                   ENDSR
090200000000     C*------------------------------------------------------------------------*
090300000000     C*?SCRVAL - SCRIVE I VALORI IN STAMPA
090400000000     C*------------------------------------------------------------------------*
090500000000     C     SCRVAL        BEGSR
090600010515     C*
090700010515     C                   Z-ADD(H)  SVA(1)        P1CVA1                         *MESE UNO
090800010515     C                   Z-ADD(H)  SVA(2)        P1CVA2                         *MESE DUE
090900010515     C                   Z-ADD(H)  SVA(3)        P1CVA3                         *MESE TRE
091000010515     C                   Z-ADD(H)  SVA(4)        P1CVA4                         *MESE QUATTRO
091100010515     C                   Z-ADD(H)  SVA(5)        P1CVA5                         *MESE CINQUE
091200010515     C                   Z-ADD(H)  SVA(6)        P1CVA6                         *MESE SEI
091300010515     C                   Z-ADD(H)  SVA(7)        P1CVA7                         *MESE SETTE
091400010515     C                   Z-ADD(H)  SVA(8)        P1CVA8                         *MESE OTTO
091500010515     C                   Z-ADD(H)  SVA(9)        P1CVA9                         *MESE NOVE
091600010515     C                   Z-ADD(H)  SVA(10)       P1CVA0                         *MESE DIECI
091700010515     C                   Z-ADD(H)  SVA(11)       P1CVAA                         *MESE UNDICI
091800010515     C                   Z-ADD(H)  SVA(12)       P1CVAB                         *MESE DODICI
091900010515     C                   Z-ADD(H)  SVA13C        P1CVAT                         *TOTALE MESI
092000000000     C*
092100010507     C                   WRITE     SEA2P1                                       *STAMPA RIGA VALORE
092200000000     C*
092300000000     C                   ENDSR
092400000000     C*------------------------------------------------------------------------*
092500000000     C*?SCRDEL - SCRIVE I VALORI DELTA IN STAMPA
092600000000     C*------------------------------------------------------------------------*
092700000000     C     SCRDEL        BEGSR
092800000000     C*
092900950216     C                   Z-ADD     SDE(1)        P2CDE1                         *MESE UNO
093000950216     C                   Z-ADD     SDE(2)        P2CDE2                         *MESE DUE
093100950216     C                   Z-ADD     SDE(3)        P2CDE3                         *MESE TRE
093200950216     C                   Z-ADD     SDE(4)        P2CDE4                         *MESE QUATTRO
093300950216     C                   Z-ADD     SDE(5)        P2CDE5                         *MESE CINQUE
093400950216     C                   Z-ADD     SDE(6)        P2CDE6                         *MESE SEI
093500950216     C                   Z-ADD     SDE(7)        P2CDE7                         *MESE SETTE
093600950216     C                   Z-ADD     SDE(8)        P2CDE8                         *MESE OTTO
093700950216     C                   Z-ADD     SDE(9)        P2CDE9                         *MESE NOVE
093800950216     C                   Z-ADD     SDE(10)       P2CDE0                         *MESE DIECI
093900950216     C                   Z-ADD     SDE(11)       P2CDEA                         *MESE UNDICI
094000950216     C                   Z-ADD     SDE(12)       P2CDEB                         *MESE DODICI
094100950216     C                   Z-ADD     SDE13C        P2CDET                         *TOTALE MESI
094200000000     C*
094300010507     C                   WRITE     SEA2P2                                       *STAMPA RIGA VALORE
094400000000     C*
094500000000     C                   ENDSR
094600000000     C*------------------------------------------------------------------------*
094700000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
094800000000     C*------------------------------------------------------------------------*
094900000000     C     CARMES        BEGSR
095000000000     C*
095100000000     C                   CLEAR                   ANN                            *ANNI
095200000000     C                   CLEAR                   MES                            *MESI
095300000000     C                   Z-ADD     *ZEROS        J                              *N° MESI DA ELABORARE
095400000000     C*
095500990524     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
095600000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
095700990524     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVORO
095800000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVORO
095900000000     C*
096000000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
096100000000IF  1C     WAA2          IFEQ      WAA
096200000000     C*
096300000000     C* CALCOLO I MESI DI DIFFERENZA
096400000000DO  2C     WMM           DOWLE     WMM2
096500000000     C                   ADD       1             J
096600000000     C                   Z-ADD     WAA           ANN(J)
096700000000     C                   Z-ADD     WMM           MES(J)
096800000000     C                   ADD       1             WMM
096900000000E   2C                   ENDDO
097000000000     C***
097100000000     C* SPARM = N° MESI SELEZIONATI
097200000000     C***
097300000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
097400000000     C*
097500000000     C* IL RESTO DELLA SCHIERA A ZERO
097600000000DO  2C     J             DOWLT     12
097700000000     C                   ADD       1             J
097800000000     C                   Z-ADD     *ZEROS        ANN(J)
097900000000     C                   Z-ADD     *ZEROS        MES(J)
098000010515E   2C                   ENDDO
098100000000     C*
098200000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
098300000000X   1C                   ELSE
098400000000     C*
098500000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
098600000000DO  2C     WMM           DOWLE     12
098700000000     C                   ADD       1             J
098800000000     C                   Z-ADD     WAA           ANN(J)
098900000000     C                   Z-ADD     WMM           MES(J)
099000000000     C                   ADD       1             WMM
099100000000E   2C                   ENDDO
099200000000     C*
099300000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
099400000000     C                   Z-ADD     01            WMM
099500000000DO  2C     WMM           DOWLE     WMM2
099600000000     C                   ADD       1             J
099700000000     C                   Z-ADD     WAA2          ANN(J)
099800000000     C                   Z-ADD     WMM           MES(J)
099900000000     C                   ADD       1             WMM
100000000000E   2C                   ENDDO
100100000000     C***
100200000000     C* SPARM = N° MESI SELEZIONATI
100300000000     C***
100400000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
100500000000     C*
100600000000     C* IL RESTO DELLA SCHIERA A ZERO
100700000000DO  2C     J             DOWLT     12
100800000000     C                   ADD       1             J
100900000000     C                   Z-ADD     *ZEROS        ANN(J)
101000000000     C                   Z-ADD     *ZEROS        MES(J)
101100000000E   2C                   ENDDO
101200000000E   1C                   ENDIF
101300000000     C*
101400000000     C                   ENDSR
101500000000     C*------------------------------------------------------------------------*
101600000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
101700000000     C*------------------------------------------------------------------------*
101800000000     C     DECTES        BEGSR
101900000000     C*--------------------
102000000000     C* DATA E ORA
102100000000     C*--------------------
102200000000     C                   Z-ADD     UDATE         DATA              6 0
102300000000     C                   TIME                    ORA               6 0
102400000000     C*--------------------
102500000000     C* DECODIFICA MESI
102600000000     C*--------------------
102700000000IF  1C     MES(1)        IFGT      *ZEROS
102800000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ.
102900000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
103000990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
103100000000E   1C                   ENDIF
103200000000     C*
103300000000IF  1C     MES(2)        IFGT      *ZEROS
103400000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ.
103500000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
103600990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
103700000000E   1C                   ENDIF
103800000000     C*
103900000000IF  1C     MES(3)        IFGT      *ZEROS
104000000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ.
104100000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
104200990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
104300000000E   1C                   ENDIF
104400000000     C*
104500000000IF  1C     MES(4)        IFGT      *ZEROS
104600000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ.
104700000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
104800990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
104900000000E   1C                   ENDIF
105000000000     C*
105100000000IF  1C     MES(5)        IFGT      *ZEROS
105200000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ.
105300000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
105400990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
105500000000E   1C                   ENDIF
105600000000     C*
105700000000IF  1C     MES(6)        IFGT      *ZEROS
105800000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ.
105900000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
106000990608     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
106100000000E   1C                   ENDIF
106200000000     C*
106300000000IF  1C     MES(7)        IFGT      *ZEROS
106400000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ.
106500000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
106600990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
106700000000E   1C                   ENDIF
106800000000     C*
106900000000IF  1C     MES(8)        IFGT      *ZEROS
107000000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ.
107100000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
107200990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
107300000000E   1C                   ENDIF
107400000000     C*
107500000000IF  1C     MES(9)        IFGT      *ZEROS
107600000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ.
107700000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
107800990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
107900000000E   1C                   ENDIF
108000000000     C*
108100000000IF  1C     MES(10)       IFGT      *ZEROS
108200000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ.
108300000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
108400990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
108500000000E   1C                   ENDIF
108600000000     C*
108700000000IF  1C     MES(11)       IFGT      *ZEROS
108800000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ.
108900000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
109000990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
109100000000E   1C                   ENDIF
109200000000     C*
109300000000IF  1C     MES(12)       IFGT      *ZEROS
109400000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ.
109500000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
109600990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
109700000000E   1C                   ENDIF
109800000000     C*
109900000000     C                   ENDSR
110000000000     C*------------------------------------------------------------------------*
110100000000     C*?CARTAB - CARICA I DATI IN TABELLE
110200000000     C*------------------------------------------------------------------------*
110300000000     C     CARTAB        BEGSR
110400000000     C*--------------------
110500971204     C* ORGANIGRAMMA
110600000000     C*--------------------
110700950216     C                   MOVEL     *BLANKS       KORDIV
110800950216     C     KEYORG        SETLL     AZORG02L
110900950216     C                   READ      AZORG02L                               99
111000000000     C                   Z-ADD     *ZEROS        F
111100000000DO  1C     *IN99         DOWEQ     '0'
111200000000IF  2C     ORGFVA        IFEQ      *BLANKS
111300950216IF  3C     ORGFAG        IFEQ      'F'
111400950216     C     ORGFAG        OREQ      'A'
111500000000     C                   ADD       1             F
111600000000     C* F I L I A L I
111700000000     C                   Z-ADD     ORGFIL        FIL(F)
111800000000     C                   MOVEL     ORGDES        DFIL(F)
111900000000     C* A R E E
112000020916     C                   Z-ADD     1             KTBKUT
112100000000     C                   MOVEL     '05'          KTBCOD
112200000000     C                   MOVEL     *BLANKS       KTBKEY
112300000000     C                   MOVEL     ORGCAR        KTBKEY
112400000000     C     KEYTAB        CHAIN     TABEL00F                           99
112500950216IF  4C     *IN99         IFEQ      *OFF
112600000000     C                   Z-ADD     ORGCAR        ARE(F)
112700000000     C                   MOVEL     TBLUNI        DARE(F)
112800950216E   4C                   ENDIF
112900000000     C* D I V I S I O N I
113000020916     C                   Z-ADD     1             KTBKUT
113100000000     C                   MOVEL     '17'          KTBCOD
113200000000     C                   MOVEL     *BLANKS       KTBKEY
113300000000     C                   MOVEL     ORGFL3        KTBKEY
113400000000     C     KEYTAB        CHAIN     TABEL00F                           99
113500950216IF  4C     *IN99         IFEQ      *OFF
113600000000     C                   MOVEL     ORGFL3        DIV(F)
113700000000     C                   MOVEL     TBLUNI        DDIV(F)
113800950216E   4C                   ENDIF
113900950216E   3C                   ENDIF
114000000000E   2C                   ENDIF
114100950216     C                   READ      AZORG02L                               99
114200000000E   1C                   ENDDO
114300950216     C                   Z-ADD     F             ULTFIL            6 0
114400990217     C*--------------------
114500950216     C* FASCE DI PESO
114600990217     C*--------------------
114700020325IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
114800990217     C                   MOVEL     'SDC'         WSX               3
114900020325X   1C                   ELSE
115000020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO AL 2001
115100020325     C                   MOVEL     'S9C'         WSX
115200020325X   2C                   ELSE                                                   *FASCE DAL 2002
115300020325     C                   MOVEL     'S2C'         WSX
115400020325E   2C                   ENDIF
115500020325E   1C                   ENDIF
115600950216     C                   Z-ADD     CODUT         KTBKUT                         *CODICE UTENTE
115700950216     C                   MOVEL     '2L'          KTBCOD
115800950216     C     KEYTA2        SETLL     TABEL00F
115900950216     C     KEYTA2        READE     TABEL00F                               99
116000950216     C                   Z-ADD     *ZEROS        J                              *INDICE
116100950216DO  1C     *IN99         DOWEQ     *OFF
116200950216IF  2C     TBLFLG        IFEQ      *BLANKS
116300990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
116400950216     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
116500990217IF  3C     FILEST        IFEQ      WSX
116600950216     C                   ADD       1             J
116700950216     C                   MOVE      STACOD        FPE(J)                         *CODICE STATISTICA
116800950216     C                   MOVEL     TBLUNI        DS2L
116900950216     C                   MOVEL     §2LDFP        DFPE(J)                        *DECODIFICA FASCIA
117000950216E   3C                   ENDIF
117100950216E   2C                   ENDIF
117200950216     C     KEYTA2        READE     TABEL00F                               99
117300950216E   1C                   ENDDO
117400990217     C*--------------------
117500950216     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
117600990217     C*--------------------
117700020916     C                   Z-ADD     1             KTBKUT
117800950216     C                   MOVEL     '2W'          KTBCOD
117900950216     C     KEYTA2        SETLL     TABEL00F
118000950216     C     KEYTA2        READE     TABEL00F                               99
118100950216     C                   Z-ADD     *ZEROS        I
118200950216DO  1C     *IN99         DOWEQ     *OFF
118300950216IF  2C     TBLFLG        IFEQ      *BLANKS
118400950216     C                   ADD       1             I
118500950216     C                   MOVEL     TBLKEY        FLAB(I)
118600950216     C                   MOVEL     TBLUNI        DS2W
118700950216     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONI
118800950216     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVI
118900950216E   2C                   ENDIF
119000950216     C     KEYTA2        READE     TABEL00F                               99
119100950216E   1C                   ENDDO
119200020328     C*-------------------------
119300020328     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
119400020328     C*-------------------------
119500020328     C                   Z-ADD     *ZEROS        R
119600020916     C                   Z-ADD     1             KTBKUT
119700020328     C                   MOVEL     '2M'          KTBCOD
119800020328     C     KEYTA2        SETLL     TABEL00F
119900020328     C     KEYTA2        READE     TABEL00F                               99
120000020328DO  1C     *IN99         DOWEQ     *OFF
120100020328IF  2C     TBLFLG        IFEQ      *BLANKS
120200020328     C                   ADD       1             R
120300020328     C                   MOVEL     TBLKEY        RGREG(R)
120400020328     C                   MOVEL     TBLUNI        DS2M
120500020328     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
120600020328E   2C                   ENDIF
120700020328     C     KEYTA2        READE     TABEL00F                               99
120800020328E   1C                   ENDDO
120900000000     C*
121000000000     C                   ENDSR
121100000000     C*------------------------------------------------------------------------*
121200000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
121300000000     C*------------------------------------------------------------------------*
121400000000     C     STAPAR        BEGSR
121500000000     C*--------------------
121600000000     C* DATE LANCIO
121700000000     C*--------------------
121800000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
121900000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
122000000000     C*--------------------
122100950214     C* PARZIALIZZAZIONI
122200000000     C*--------------------
122300130208     c                   eval      StpTOT = 'NO'
122400160906IF  1C     PARpt         IFEQ      'N'
122500160906     c                   eval      Stppt = 'Nuovi'
122600160831     c                   end
122700160906IF  1C     PARpt         IFEQ      'V'
122800160906     c                   eval      Stppt = 'Vecchi'
122900160906     c                   end
123000160906IF  1C     PARpt         IFEQ      ' '
123100160906     c                   eval      Stppt = 'Tutti'
123200160906     c                   end
123300950214     C*
123400950214     C                   MOVEL     PARIOE        PACIOE
123500950214IF  1C     PARIOE        IFEQ      'I'
123600950216     C                   MOVEL     'ITALIA'      PADIOE
123700950214E   1C                   ENDIF
123800950214IF  1C     PARIOE        IFEQ      'E'
123900950216     C                   MOVEL     'ESTERO'      PADIOE
124000950214E   1C                   ENDIF
124100950214IF  1C     PARIOE        IFEQ      *BLANKS
124200950216     C                   MOVEL     'ITA+EST'     PADIOE
124300950214E   1C                   ENDIF
124400950214     C***
124500000000     C* S T A M P A
124600950214     C***
124700010507     C                   WRITE     SEA2PT                                       *INTESTAZIONE
124800010507     C                   WRITE     SEA2PA                                       *PARAMETRI
124900000000     C*
125000000000     C* ACCENDO INDICATORE DI OF PER STAMPARE LA TESTATA ALLA 1^ ROTTURA CLIENTE
125100000000     C                   SETON                                        01
125200000000     C*
125300000000     C                   ENDSR
125400020916     C*--------------------------------------------------------------------------------------------*
125500020916     C* REPERISCE I DATI UTENTE
125600020916     C*--------------------------------------------------------------------------------------------*
125700020916     C     REPDATIUTE    BEGSR
125800020916     C*
125900020916     C* INIZIALIZZA VARIABILI DI WRK
126000020916     C                   CLEAR                   TIBS34DS
126100020916     C                   CLEAR                   AZUTEDS
126200020916     C                   CLEAR                   DDATIUTE
126300020916     C*
126400020916     C     *DTAARA       DEFINE    §azute        azuteds
126500020916     C     *DTAARA       DEFINE    §datiute      ddatiute
126600020916     C                   IN(E)     *DTAARA
126700020916if  1C                   IF        %Error
126800020916     c                   EVAL      I34Tla = 'L'
126900020916     C                   CALL      'TIBS34R'
127000020916     C                   PARM                    Tibs34Ds
127100020916     C                   IN        *DTAARA
127200020916e   1C                   ENDIF
127300020916     C*
127400020916     C                   ENDSR
127500000000     C*------------------------------------------------------------------------*
127600000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
127700000000     C*------------------------------------------------------------------------*
127800000000     C     *INZSR        BEGSR
127900000000     C*
128000000000     C* PARAMETRI IN INPUT AL PGM
128100000000     C     *ENTRY        PLIST
128200000000     C                   PARM                    KPJBA
128300000000     C                   MOVEL     KPJBU         PARAM
128400170213      **
128500170213     c*****              if        parpt <>' '
128600170213     c******             open      sisdo01l
128700170213     c*****              end
128800170213      **
128900170213     C                   clear                   WDFIN             8 0          *DATA AAAA/MM/GG
129000170213      **
129100170213      **  SOLO COME DEFAULT:  (a mo di paracadute)   perchè dopo normalmente verrà ricoperta.
129200170213     C*     IMPOSTO la data corrente per trovare il mese precedente su cui basare
129300170213      **    il calcolo dell'unificante
129400170213     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
129500170213     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
129600170213     C                   Z-ADD     *ZEROS        G08INV
129700170213     C                   MOVEL     '0'           G08ERR
129800170213     C                   CALL      'XSRDA8'
129900170213     C                   PARM                    WLBDA8
130000170213     C                   Z-ADD     G08INV        wdFIN                            AAAA-MM-GG
130100170213      ****  deve ricavare il mese precedente
130200170213     C                   move      wdfin         data_Iso
130300170213     c                   subdur    1:*m          data_Iso
130400170213     C                   move      data_Iso      wdfin
130500170213      **
130600170213      **  ORA se TROVA correttamente i dati SOSTITUISCE ciò che ho impostato nella data
130700170213      **   solo a mo' di protezione.
130800170213      ***   Leggo un record dei totali per prendere il riferimento della data di
130900170213      **     elaborazione su cui basare il calcolo del UNIFICANTE
131000170213     c                   clear                   data6             6 0               AAAA-MM
131100170213      **  imposto l'estremo al ANNO/MESE
131200170214     c                   read      WADELPCT1
131300170214     c                   if        not %EoF(WADELPCT1)
131400170213     c                   eval      data6 = wapAAA * 100 + wapMMA
131500170213     C                   movel     data6         wdfin                          solo AAAA-MM
131600170213     c                   end
131700170213      *
131800170213     C*------- GIORNO   di   F I N E   M E S E ---------------------*
131900170213     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
132000170213     C                   MOVE      28            WDFIN
132100170213     C*  Ultimo giorno del mese
132200170213     C                   MOVE      WDFIN         MMGG_4            4 0
132300170213     C                   MOVEL     MMGG_4        Mm_2              2 0
132400170213     C                   MOVEl     WDFIN         Anno_4            4 0
132500170213      * se è febbraio
132600170213     c                   If        Mm_2 = 2
132700170213      *  potrebbe essere bisestile 29 gg.
132800170213     c     Anno_4        div       4             risulta           3 0
132900170213     c                   mvr                     resto             3 0
133000170213      *  Se bisestile
133100170213     c                   If        resto = 0
133200170213     C                   MOVE      '29'          WDFIN
133300170213     c                   end
133400170213      *
133500170213     c                   else
133600170213      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
133700170213     c                   If           Mm_2 = 4 or Mm_2 = 6 or
133800170213     c                                Mm_2 = 9 or Mm_2 = 11
133900170213     C                   MOVE      '30'          WDFIN
134000170213     c                   else
134100170213     C                   MOVE      '31'          WDFIN
134200170213     c                   end
134300170213      *
134400170213     c                   endIf
134500170213     C*
134600170213     C*----------- F I N E    M E S E ---------------------------*
134700170213     C*
134800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
134900000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
135000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
135100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
135200950216     C     *LIKE         DEFINE    ORGFL3        KORDIV                         *AZORG01L
135300950216     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *CLIENTE
135400170214     C     *LIKE         DEFINE    SDCKSC        savSDCKSC                      *CLIENTE
135500000000     C*
135600000000     C* DEFINISCE INDICI DI SCHIERA
135700000000     C                   Z-ADD     *ZEROS        X                 2 0          *PER I MESI
135800000000     C                   Z-ADD     *ZEROS        MM                2 0          *PER I MESI
135900000000     C                   Z-ADD     *ZEROS        IN                2 0          *PER I MESI
136000000000     C                   Z-ADD     *ZEROS        SPARM             4 0          *N° MESI SELEZIONATI
136100020328     C                   Z-ADD     *ZEROS        R                 3 0          *PER RAGGRUPPAMENTI REGIONI/NETWORK
136200000000     C*
136300000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
136400000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
136500000000     C                   MOVEL     ' '           TIPANN            1            *TIPO ANNO (P/C)
136600000000     C*
136700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
136800000000     C     KEYTAB        KLIST
136900000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
137000000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
137100000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
137200160831     C*
137300160831     C* CHIAVE LETTURA sisdo01l - COMPLETA
137400170213     C**** Ksdo          KLIST
137500170213     C****               KFLD                    sdcksc                         *CHIAVE TABELLA
137600170213     C****               KFLD                    sdcann                         *CODICE UTENTE
137700170213     C****               KFLD                    sdcmes                         *CODICE TABELLA
137800000000     C*
137900000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
138000000000     C     KEYTA2        KLIST
138100000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
138200000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
138300000000     C*
138400950216     C* CHIAVE LETTURA AZORG02L - PARZIALE
138500000000     C     KEYORG        KLIST
138600950216     C                   KFLD                    KORDIV                         *DIVISIONE
138700000000     C*
138800990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
138900950216     C     KEYSDC        KLIST
139000950216     C                   KFLD                    KSCKSC                         *DIVISIONE
139100000000     C*
139200000000     C* LANCIO ROUTINE PER VERIFICARE SE SONO IN SEDE, IN FILIALE DI 1°LIVELLO O
139300000000     C* IN FILIALE DI 2° LIVELLO E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
139400020916     C                   EXSR      REPDATIUTE
139500020916     C                   Z-ADD     1             CODUT             1 0
139600000000     C*
139700000000     C* MEMORIZZA I DATI IN TABELLE
139800000000     C                   EXSR      CARTAB
139900000000     C*
140000000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
140100000000     C                   EXSR      CARMES
140200000000     C*
140300000000     C* DECODIFICA INTESTAZIONE
140400000000     C                   EXSR      DECTES
140500000000     C*
140600000000     C                   ENDSR
140700000000     O*------------------------------------------------------------------------*
140800000000**   DMM - DECODIFICHE MESI DELL'ANNO
140900000000GEN  1
141000000000FEB  2
141100000000MAR  3
141200000000APR  4
141300000000MAG  5
141400000000GIU  6
141500000000LUG  7
141600000000AGO  8
141700000000SET  9
141800000000OTT  10
141900000000NOV  11
142000000000DIC  12
