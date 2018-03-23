000100011203    ?H* TIBS16R *-----------------------------------------------------*
000200011203    ?H*            PROGRAMMA MANUTENZIONE FILE NUMERATORI             *
000300020212    ?H*---------------------------------------------------------------*
000400011203     H DECEDIT('0,') DATEDIT(*DMY.)
000500020114    ?F*---------------------------------------------------------------*
000600011203     FAZNUM01L  UF A E           K DISK
000700011203     FAZNUM02L  UF   E           K DISK
000800011203     F                                     RENAME(AZNUM000:AZNUM02)
000900011203     FAZORG01L  IF   E           K DISK
001000011203     FTIBS16D   CF   E             WORKSTN
001100020204     F                                     INFDS(BS16DS)
001200011203     F                                     SFILE(BS16SFL:NRR)
001300020212    ?D*---------------------------------------------------------------*
001400011203     D KPJBA         E DS
001500011203     D  PROFIL                 1      3
001600020204     D*------------------
001700011203     D TIBS02DS      E DS                  INZ
001800011203     D* DS TABELLA CNU - CODICI NUMERATORI
001900011203     D DCNU          E DS                  INZ
002000020204     D*------------------
002100020204     D* DATI UTENTE
002200020204     D UT§DSE0F      E DS
002300020204     D CNCR80        E DS
002400020204     D*------------------
002500020204     D WLBDAT          DS                  INZ
002600020204     D  G02DAT                 1      8  0
002700020204     D  G02INV                 9     16  0
002800020204     D  G02ERR                17     17
002900020204     D  G02TGI                18     22  0
003000020204     D*------------------
003100020204     D* DS PAGINA DEL RECORD DI SFILE MODIFICATO
003200020204     D BS16DS          DS
003300020204     D  PRIMA                378    379B 0
003400020114     D*------------------
003500020114     D* VARIABILI:
003600020206     D KSOC            s                   like(NUMsoc) inz
003700020115     D KANN            s                   like(NUMaaa) inz
003800020115     D KNUM            s                   like(NUMcnu) inz
003900020115     D KPO1            s                   like(NUMpo1) inz
004000020115     D KPO2            s                   like(NUMpo2) inz
004100020115     D KDAT            s                   like(NUMdac) inz
004200020115     D ANNOME          s              4  0 inz
004300020115     D ANNOPI          s              4  0 inz
004400020115     D wZero20         s             20    inz(*zeros)
004500020114     D SAVnum          s                   like(NUMcnu) inz
004600020114     D SAVcnu          s              3    inz(*hival)
004700020304     D WPO             s              3    inz(*zeros)
004800020130     D W1Cnum          s                   like(NUMcnu) inz
004900020130     D W1Cdcd          s                   like(NUMdac) inz(*loval)
005000020130     D W1Cdca          s                   like(NUMdac) inz(*hival)
005100020204     D WdataEUR        s               d   datfmt(*eur)
005200020204     D WdataDMY        s               d   datfmt(*dmy)
005300011206    ? *---------------------------------------------------------------*
005400011206    ? * INDICATORI USATI                                              *
005500011206    ? *---------------------------------------------------------------*
005600020114      *  09 - POSIZIONAMENTO CURSORE SU COD.NUMERATORE 1°VIDEATA
005700020114      *  10 <=> 29 - ERRORI VIDEATA SFL:
005800020204      *  10 - ERRORE SFL: CODICE NUMERATORE ERRATO
005900020204      *  11 -   "     "   NUMERATORE IN INSERIMENTO GIA' ESISTENTE
006000020304      *  12 -   "     "   SOCIETA' ERRATA =============== NON GESTITO
006100020204      *  13 -   "     "   COD.NUMERATORE E ANNO NON CONGRUENTI
006200020204      *  14 -   "     "   ANNO NON COMPRESO NEL RANGE CONSENTITO
006300020204      *  15 -   "     "   DATA OBBLIGATORIA
006400020204      *  16 -   "     "   DATA FORMALMENTE ERRATA
006500020204      *  17 -   "     "   P.O.1 ERRATO
006600020304      *  18 -   "     "   P.O.1 DA ELABORARE PREVISTO IN TAB."CNU" DIVERSO
006700020304      *  19 -   "     "   P.O.2 ERRATO
006800020304      *  20 -   "     "   P.O.2 DA ELABORARE PREVISTO IN TAB."CNU" DIVERSO
006900020304      *  21 -   "     "   MANUTENZIONE CONSENTITA SOLO AD UTENTI "EDP"
007000020304      *  22 -   "     "   NON IMMESSSO RANGE
007100020304      *  23 -   "     "   RANGE DA > RANGE A
007200020304      *  24 -   "     "   ULTIMO NUMERO PRELEVATO FUORI RANGE
007300020114      *  30 - DI COMODO
007400020114      *  31 - DI COMODO
007500020204      *  40 <=> 69 - ERRORI VIDEATA D01:
007600020130      *  40 - ERRORE D01: CODICE NUMERATORE ERRATO
007700020130      *  41 -   "     "   ANNO NON COMPRESO NEL RANGE CONSENTITO
007800020130      *  42 -   "     "   NUMERATORE ANNUO, MA ANNO NON IMMESSO
007900020130      *  43 -   "     "   NUMERATORE NON ANNUO, MA ANNO IMMESSO
008000020130      *  44 -   "     "   DATA -DAL- FORMALMENTE ERRATA
008100020130      *  45 -   "     "   DATA -AL-  FORMALMENTE ERRATA
008200020130      *  46 -   "     "   RANGE DATE ERRATO
008300020130      *  47 -   "     "   NUMERATORE GIORNALIERO, MA DATA NON IMMESSA
008400020130      *  48 -   "     "   NUMERATORE NON GIORNALIERO, MA DATA IMMESSA
008500020130      *  49 -   "     "   NUMERATORE SENZA DATA IN CHIAVE, MA RICHIESTI
008600020130      *                    NUMERATORI CON   DATA IN CHIAVE
008700020130      *  50 -   "     "   NUMERATORE  CON  DATA IN CHIAVE, MA RICHIESTI
008800020130      *                    NUMERATORI SENZA DATA IN CHIAVE
008900020130      *  51 -   "     "   P.O.1 ERRATO
009000020130      *  52 -   "     "   P.O.2 ERRATO
009100020114      *  70 - EMETTE RICHIESTA IMMISSIONE NUOVI NUMERATORI
009200020114      *  71 - CONSENTITO INSERIMENTO DI NUOVI RECORD
009300020114      * N71 - PROTEZIONE DEI CAMPI CHIAVE PER I RECORD GIA' INSERITI
009400020114      *  72 - PROTEZIONE DEI CAMPI CHIAVE DI V.L. 01L
009500020114      *  73 - PROTEZIONE DEI CAMPI CHIAVE DI V.L. 02L
009600020114      *  80 - SFLNXTCHG
009700020114      *  80 - PULIZIA ED EMISSIONE SFL
009800020114      *  90 - GENERICO ERRORE O RIEMISSIONE VIDEATA
009900020114      *  98 - MANUTENZIONE SOLO EDP
010000011206    ?C*---------------------------------------------------------------*
010100011203     C*
010200011203     C* DEFINIZIONI VARIABILI E KLIST
010300020114     C                   EXSR      DEFCAM
010400011203     C*
010500020212     C                   CLEAR                   V1CSOC
010600020130     C                   MOVE      *ZEROS        V1CNUM
010700020204     C                   MOVE      *YEAR         V1CANN
010800020204     C                   CLEAR                   V1CDCD
010900020204     C                   CLEAR                   V1CDCA
011000020204     C                   CLEAR                   V1CDAT
011100020204     C                   CLEAR                   V1CPO1
011200020204     C                   CLEAR                   V1CPO2
011300020204     C                   CLEAR                   CONF
011400011203     C*
011500020114     C     INIZIO        TAG
011600011203     C* EMISSIONE I FORMATO -
011700020204     C                   WRITE     BS16T1
011800011203     C*
011900020204     C     FMTD1         TAG
012000011206     C                   MOVEL     *OFF          *IN70
012100020204     C                   EXFMT     BS16D1
012200011203     C*
012300011203     C* F3 - FINE LAVORO
012400020114     C   KC              GOTO      FINE
012500011203     C*
012600011203     C* CONTROLLO I FORMATO VIDEO
012700011203     C                   EXSR      CTRD01
012800011203     C* ERRORE
012900011203     C   90
013000020114     CAN 09              GOTO      INIZIO
013100011203     C   90              GOTO      FMTD1
013200011203     C* CARCO SFL
013300011203     C                   EXSR      CARSFL
013400020114     C*
013500011203     C* NESSUN NUMERATORE TROVATO: CHIEDO SE NE VOGLIONO IMMETTERE
013600020114IF  1C     NRR           IFEQ      0
013700011203     C*
013800011206     C                   MOVEL     *ON           *IN70
013900020204     C                   EXFMT     BS16D1
014000011203     C* F3 - FINE LAVORO
014100020114     C   KC              GOTO      FINE
014200011203     C*
014300020114IF  2C     CONF          IFEQ      'NO'
014400020114     C                   GOTO      INIZIO
014500020114X   2C                   ELSE
014600020114     C                   EXSR      CARVUO
014700020114E   2C                   ENDIF
014800020114     C*
014900020114E   1C                   ENDIF
015000011203     C*
015100011203     C* EMMISIONE SUBFILE
015200011203     C                   Z-ADD     NRR           WNRR
015300011203     C                   Z-ADD     1             NRR
015400011203     C                   Z-ADD     1             HSRN
015500011203     C*
015600020204     C                   WRITE     BS16T1
015700011203     C                   WRITE     BS16D5
015800011203     C*
015900020114     C     VIDEO         TAG
016000011203     C*
016100011203     C                   SETOFF                                           90
016200011203     C                   EXFMT     BS16CTL
016300011203     C*
016400011203     C* F3 - FINE LAVORO
016500020114     C   KC              GOTO      FINE
016600011203     C*
016700011203     C* F12 - RITORNO
016800011203     C   KL              Z-ADD     *ZEROS        NRR
016900020114     C   KL              GOTO      INIZIO
017000011203     C*
017100011203     C* F8 - RICHIESTE RIGHE DI IMMISSIONE (N° 19)
017200011203DO  1C   08              DO
017300011203     C     WNRR          ADD       1             HSRN
017400011203     C*
017500020111     C                   movel     *off          *in70
017600020114     C                   EXSR      CARVUO
017700011203     C*
017800011203     C                   Z-ADD     NRR           WNRR
017900020114     C                   GOTO      VIDEO
018000011203E   1C                   ENDDO
018100011203     C*
018200011203     C* ELABORAZIONE SUBFILE
018300011203     C                   SETOFF                                           90
018400011203     C                   Z-ADD     PRIMA         HSRN
018500011203     C*
018600011203     C                   READC     BS16SFL                                60
018700020111DO  1C     *IN60         DOWEQ     *OFF
018800011203     C*
018900011203     C* CONTROLLO CAMPI SFILE
019000020114     C                   EXSR      CTRSFL
019100011203     C*
019200020114     C     HHHNRR        COMP      0                                  7171
019300020114IF  2C                   if        *in71
019400020114IF  3C                   if        HCNUkda = 'S'
019500020114     C                   movea     '01'          *in(72)
019600020114X   3C                   else
019700020114     C                   movea     '10'          *in(72)
019800020114E   3C                   endif
019900020206X   2C                   else
020000020206     C                   movea     '00'          *in(72)
020100020114E   2C                   endif
020200011203     C*
020300020114     C                   SETON                                        80
020400011203     C                   UPDATE    BS16SFL
020500020114     C                   SETOFF                                       80
020600020114     C*
020700020114     C   90              leave
020800020114     C*
020900011203     C                   READC     BS16SFL                                60
021000011203E   1C                   ENDDO
021100011203     C*
021200020114     C   90              GOTO      VIDEO
021300011203     C*
021400011203     C* AGGIORNAMENTI RECORD
021500020114IF  1C     *IN06         IFEQ      *ON
021600020114     C                   EXSR      AGGIOR
021700011203     C                   Z-ADD     *ZEROS        NRR
021800020114     C                   GOTO      INIZIO
021900011203     C*
022000011203X   1C                   ELSE
022100011203     C*
022200020114     C                   GOTO      VIDEO
022300011203E   1C                   ENDIF
022400011203     C*
022500020114     C     FINE          TAG
022600011203     C                   SETON                                        LR
022700011203    ?C*---------------------------------------------------------------*
022800020114    ?C* CONTROLLO FORMATO 1
022900011203    ?C*---------------------------------------------------------------*
023000011203     C     CTRD01        BEGSR
023100020114     C*
023200020114     C                   SETOFF                                       09  90
023300011203     C                   CLEAR                   SAVNUM
023400020130     C                   CLEAR                   W1Cnum
023500020206     C*
023600020206     C* CONTROLLO CODICE SOCIETA'
023700020213     C* (per ora nessun controllo)
023800020130     C*
023900020130     C* CODICE NUMERATORE
024000020130IF  1C     V1CNUM        IFNE      *BLANKS
024100020130     C     V1CNUM        ANDNE     *ZEROS
024200020130     C*
024300020130     C* CONTROLLO TIPO NUMERATORE
024400020130     C     '?'           SCAN      V1CNUM                                 90
024500020130IF  2C     *IN90         IFEQ      *ON
024600020130     C                   SETON                                        09
024700020130     C                   clear                   TIBS02DS
024800020130     C                   movel     'R'           t02mod
024900020130     C                   movel     knsif         t02sif
025000020130     C                   movel     'CNU'         t02cod
025100020130     C                   movel     *blanks       t02ke1
025200020130     C                   call      'TIBS02R'
025300020130     C                   parm                    KPJBA
025400020130     C                   parm                    TIBS02DS
025500020130     C* CODICE SELEZIONATO
025600020130IF  3C     T02ke1        IFNE      *BLANKS
025700020130     C                   MOVEL     T02ke1        V1CNUM
025800020130X   3C                   ELSE
025900020130     C                   MOVEL     *ZEROS        V1CNUM
026000020130E   3C                   ENDIF
026100020130     C                   GOTO      ENDCT1
026200020130E   2C                   ENDIF
026300020130     C*
026400020130     C* CONTROLLO ESISTENZA CODICE
026500020130     C                   clear                   TIBS02DS
026600020130     C                   movel     'C'           t02mod
026700020130     C                   movel     knsif         t02sif
026800020130     C                   movel     'CNU'         t02cod
026900020130     C                   movel(p)  V1CNUM        t02ke1
027000020130     C                   call      'TIBS02R'
027100020130     C                   parm                    KPJBA
027200020130     C                   parm                    TIBS02DS
027300020130IF  2C                   IF        T02err <> *blanks
027400020130     C                   SETON                                        40  90
027500020130     C                   GOTO      ENDCT1
027600020130E   2C                   ENDIF
027700020130     C                   MOVEL     T02UNI        DCNU
027800020130     C                   MOVE      V1Cnum        W1Cnum
027900020130     C*
028000020130X   1C                   ELSE
028100020130     C*
028200020130     C                   CLEAR                   DCNU
028300020130     C*
028400020130E   1C                   ENDIF
028500020114     C*
028600011203     C* L'ANNO DEVE ESSERE COMPRESO DA -3 E + 3
028700020130IF  1C                   IF        V1Cann > 0
028800020130     C                             and (V1Cann < AnnoME
028900020130     C                              or  V1Cann > AnnoPI)
029000020130     C                   SETON                                        41  90
029100011203     C                   GOTO      ENDCT1
029200020130E   1C                   ENDIF
029300020130     C*
029400020130     C* IL TIPO NUMERATORE DEVE ESSERE CONGRUENTE CON L'ANNO
029500020130IF  1C     V1CNUM        IFNE      *BLANKS
029600020130     C     V1CNUM        ANDNE     *ZEROS
029700011206IF  2C     §CNUFAA       IFEQ      'S'
029800011203     C     V1CANN        ANDEQ     0
029900020130     C                   SETON                                        42  90
030000020114     C                   GOTO      ENDCT1
030100011203E   2C                   ENDIF
030200020130IF  2C     §CNUFAA       IFEQ      *BLANKS
030300011203     C     V1CANN        ANDGT     0
030400020130     C                   SETON                                        43  90
030500020114     C                   GOTO      ENDCT1
030600011203E   2C                   ENDIF
030700020130E   1C                   ENDIF
030800020130     C* CONTROLLO DATA DAL
030900020130     C                   RESET                   W1CDCD
031000020130IF  1C                   IF        V1CDCD <> *ZEROS
031100020130     C                   Z-ADD     V1CDCD        G02DAT
031200020130     C                   CLEAR                   G02ERR
031300020130     C                   CALL      'XSRDA8'
031400020130     C                   PARM                    WLBDAT
031500020130IF  2C     G02ERR        IFEQ      '1'
031600020130     C                   SETON                                        44  90
031700020130     C                   GOTO      ENDCT1
031800020130X   2C                   ELSE
031900020130     C                   Z-ADD     G02DAT        V1CDCD
032000020130     C                   Z-ADD     G02INV        W1CDCD
032100020130E   2C                   ENDIF
032200020130E   1C                   ENDIF
032300020130     C*
032400020130     C* CONTROLLO DATA AL
032500020130     C                   RESET                   W1CDCA
032600020130IF  1C                   IF        V1CDCA <> *ZEROS
032700020130     C                   Z-ADD     V1CDCA        G02DAT
032800020130     C                   CLEAR                   G02ERR
032900020130     C                   CALL      'XSRDA8'
033000020130     C                   PARM                    WLBDAT
033100020130IF  2C     G02ERR        IFEQ      '1'
033200020130     C                   SETON                                        45  90
033300020130     C                   GOTO      ENDCT1
033400020130X   2C                   ELSE
033500020130     C                   Z-ADD     G02DAT        V1CDCA
033600020130     C                   Z-ADD     G02INV        W1CDCA
033700020130E   2C                   ENDIF
033800020130E   1C                   ENDIF
033900020130     C* RANGE DATE CREAZIONE ERRATO
034000020130IF  1C     W1CDCD        IFGT      W1CDCA
034100020130     C                   SETON                                        46  90
034200020130     C                   GOTO      ENDCT1
034300020130E   1C                   ENDIF
034400020130     C* IL TIPO NUMERATORE DEVE ESSERE CONGRUENTE CON IL RANGE DI DATE
034500020130IF  1C     V1CNUM        IFNE      *BLANKS
034600020130     C     V1CNUM        ANDNE     *ZEROS
034700020130IF  2C                   IF            §CNUkda = 'S'
034800020130     C                             and V1Cdcd  = *zeros
034900020130     C                             and V1CdcA  = *zeros
035000020130     C                   SETON                                        47  90
035100020130     C                   GOTO      ENDCT1
035200020130E   2C                   ENDIF
035300020130IF  2C                   IF             §CNUkda =  *blanks
035400020130     C                             and (V1Cdcd  <> *zeros
035500020130     C                              or  V1CdcA  <> *zeros)
035600020130     C                   SETON                                        48  90
035700020130     C                   GOTO      ENDCT1
035800020130E   2C                   ENDIF
035900020130     C* IL TIPO NUMERATORE DEVE ESSERE CONGRUENTE CON LA RICHIESTA PER DATA
036000011206IF  2C     §CNUKDA       IFEQ      'S'
036100011203     C     V1CDAT        ANDEQ     'N'
036200020130     C                   SETON                                        50  90
036300020114     C                   GOTO      ENDCT1
036400011203E   2C                   ENDIF
036500020130IF  2C     §CNUKDA       IFEQ      *BLANKS
036600011203     C     V1CDAT        ANDEQ     'S'
036700020130     C                   SETON                                        50  90
036800020114     C                   GOTO      ENDCT1
036900011203E   2C                   ENDIF
037000020130     C                   MOVE      V1CNUM        SAVNUM
037100020130E   1C                   ENDIF
037200020114     C*
037300011203     C* CONTROLLO I P.O. SE IMMESSI
037400020204IF  1C     V1CPO1        IFGT      0
037500020204     C     V1CPO1        CHAIN     AZORG01L                           30
037600020130IF  2C     *IN30         IFEQ      *ON
037700011203     C     ORGFVA        ORNE      ' '
037800011203     C     ORGFAG        ORNE      'A'
037900011203     C     ORGFAG        ANDNE     'F'
038000011203     C                   SETON                                        4790
038100020114     C                   GOTO      ENDCT1
038200020130E   2C                   ENDIF
038300020130E   1C                   ENDIF
038400020204IF  1C     V1CPO2        IFGT      0
038500020204     C     V1CPO2        CHAIN     AZORG01L                           30
038600020130IF  2C     *IN30         IFEQ      *ON
038700011203     C     ORGFVA        ORNE      ' '
038800011203     C     ORGFAG        ORNE      'A'
038900011203     C     ORGFAG        ANDNE     'F'
039000011203     C                   SETON                                        4990
039100020114     C                   GOTO      ENDCT1
039200020130E   2C                   ENDIF
039300020130E   1C                   ENDIF
039400020114     C*
039500011203     C     ENDCT1        ENDSR
039600011203    ?C*---------------------------------------------------------------*
039700020114    ?C* CARICO SFL
039800011203    ?C*---------------------------------------------------------------*
039900011203     C     CARSFL        BEGSR
040000020114     C*
040100011203     C                   EXSR      PULSFL
040200020206     C                   MOVEL     V1CSOC        KSOC
040300011203     C                   MOVE      V1CANN        KANN
040400020130     C                   MOVE      V1CNUM        KNUM
040500011203     C                   MOVEL     *BLANKS       CONF
040600020118     C                   MOVEL     *ON           *IN71
040700020114     C*
040800020206sel 1C                   select
040900020206     C*
041000020206     C* Posizionamento per DATA APERTURA
041100020206w   1C                   when      V1Cdat =  'S'
041200020306sel 2C                   select
041300020306w   2C                   when          V1Cnum <> *blanks
041400020206     C                             and V1Cnum <> *zeros
041500020206     C     K2NUM2        setll     AZNUM02L
041600020306w   2C                   when      V1Csoc <> *blanks
041700020306     C     V1Csoc        setll     AZNUM02L
041800020306x   2C                   other
041900020206     C     *loval        setll     AZNUM02L
042000020306e   2C                   endsl
042100020306     C                   read(n)   AZNUM02L                               31
042200020130     C*
042300020206     C* Posizionamento per SOCIETA', ANNO, COD.NUM, P.O.1 e P.O.2
042400020206w   1C                   when          V1Cnum <> *blanks
042500020206     C                             and V1Cnum <> *zeros
042600020206w   1C                             and V1Cpo1 >  0
042700020206     C                             and V1Cpo2 >  0
042800020306     C     K5NUM1        setll     AZNUM01L
042900020306     C                   read(n)   AZNUM01L                               31
043000020206     C*
043100020206     C* Posizionamento per SOCIETA', ANNO, COD.NUM e P.O.1
043200020206w   1C                   when          V1Cnum <> *blanks
043300020206     C                             and V1Cnum <> *zeros
043400020206     C                             and V1Cpo1 >  0
043500020206     C     K4NUM1        setll     AZNUM01L
043600020306     C                   read(n)   AZNUM01L                               31
043700020206     C*
043800020206     C* Posizionamento per SOCIETA', ANNO e COD.NUM
043900020206w   1C                   when          V1Cnum <> *blanks
044000020206     C                             and V1Cnum <> *zeros
044100020206     C     K3NUM1        setll     AZNUM01L
044200020306     C                   read(n)   AZNUM01L                               31
044300020206     C*
044400020206     C* Posizionamento per SOCIETA' e ANNO
044500020306w   1C                   when      V1Cann <> *zeros
044600020206     C     K2NUM1        setll     AZNUM01L
044700020306     C                   read(n)   AZNUM01L                               31
044800020306     C*
044900020306     C* Posizionamento per SOCIETA'
045000020306x   1C                   other
045100020306     C     V1Csoc        setll     AZNUM01L
045200020306     C                   read(n)   AZNUM01L                               31
045300020206e   1C                   endsl
045400020114     C*
045500020130     C* SELEZIONE:
045600020118     C*
045700020206DO  1C                   DOW       not *in31
045800020114     C*
045900020206IF  2C                   IF        SAVnum <> NUMcnu
046000011203     C                   clear                   TIBS02DS
046100011203     C                   movel     'C'           t02mod
046200011203     C                   movel     knsif         t02sif
046300011203     C                   movel     'CNU'         t02cod
046400020111     C                   movel(p)  NUMCNU        t02ke1
046500011203     C                   call      'TIBS02R'
046600011203     C                   parm                    KPJBA
046700011203     C                   parm                    TIBS02DS
046800020130IF  3C                   IF        T02err = *blanks
046900011203     C                   movel     T02uni        DCNU
047000011203     C                   else
047100011203     C                   clear                   DCNU
047200020130E   3C                   ENDIF
047300020206     C                   move      NUMCNU        SAVNUM
047400020130E   2C                   ENDIF
047500020114     C*
047600011203     C* SE LE CARATTERISTICHE CORRISPONDO A QUELLE DELLE RICHIESTE
047700011203     C*  CARICO
047800020130SEL 2C                   select
047900020204     C* Codice società non compreso nella scelta
048000020204W   2C                   when      V1CSOC  <> *blanks and
048100020204     C                             V1Csoc  <> NUMsoc
048200020130     C* Anno numeratore non compreso nella scelta
048300020130W   2C                   when      V1CANN  <> *zeros  and
048400020130     C                             NUMaaa  <> V1CANN
048500020130W   2C***                when      V1Cann  <> NUMaaa  and
048600020130     C***                          §CNUfaa =  'S'
048700020130W   2C***                when      V1Cann  <> NUMaaa  and
048800020130     C***                          §CNUfaa =  *blanks
048900020130     C* Codice numeratore non compreso nella scelta
049000020130W   2C                   when      V1CNUM  <> *blanks and
049100020130     C                             V1CNUM  <> *zeros  and
049200020130     C                             NUMcnu  <> W1Cnum
049300020130W   2C                   when      V1Cdat  =  'N'     and
049400020130     C                             §CNUkda =  'S'
049500020130W   2C***                when      §CNUkda =  'S'
049600020130     C***                          and (NUMdac < wDataMIN
049700020130     C***                           or  NUMdac > wDataMAX)
049800020130W   2C                   when      §CNUkda =  *blanks and
049900020118     C                             V1Cdat  =  'S'
050000020130     C* Data creazione numeratore non compreso nella scelta
050100020130W   2C                   when      NUMdac < W1Cdcd  or
050200020130     C                             NUMdac > W1Cdca
050300020206     C* Codice P.O.1 non compreso nella scelta
050400020206W   2C                   when      V1Cpo1  <> *zeros and
050500020206     C                             V1Cpo1  <> NUMpo1
050600020206     C* Codice P.O.2 non compreso nella scelta
050700020206W   2C                   when      V1Cpo2  <> *zeros and
050800020206     C                             V1Cpo2  <> NUMpo2
050900020206     C* OK
051000020130X   2C                   other
051100020206     C                   add       1             NRR
051200020206     C                   exsr      CARICA
051300020206E   2C                   endsl
051400020114     C*
051500020206IF  2C                   if        V1Cdat = 'S'
051600020306     C                   read(n)   AZNUM02L                               31
051700020206X   2C                   else
051800020306     C                   read(n)   AZNUM01L                               31
051900020206E   2C                   endif
052000020118     C*
052100020206E   1C                   enddo
052200020114     C*
052300011203     C                   ENDSR
052400011203    ?C*---------------------------------------------------------------*
052500020114    ?C* PULIZIA SUBFILE
052600011203    ?C*---------------------------------------------------------------*
052700011203     C     PULSFL        BEGSR
052800020114     C*
052900011203     C                   SETON                                        85
053000011203     C                   WRITE     BS16CTL
053100011203     C                   SETOFF                                       85
053200011203     C                   Z-ADD     0             WNRR
053300020114     C*
053400011203     C                   ENDSR
053500011203    ?C*---------------------------------------------------------------*
053600011203    ?C* ROUTINE DI CARICAMENTO SFILE PER ANNO NUMERATORI ESISTENTE    *
053700011203    ?C*---------------------------------------------------------------*
053800011203     C     CARICA        BEGSR
053900011203     C*
054000011206     C                   Z-ADD     NUMUNP        HHHNRR
054100011206     C                   Z-ADD     NUMDAC        HDAT
054200020114     C                   movel     §CNUKDA       HCNUKDA
054300011206     C*
054400011206     C                   MOVEL     *BLANKS       VIDANN
054500020206     C                   MOVEL     NUMSOC        VSOC
054600011206     C                   Z-ADD     NUMAAA        VAAA
054700011206     C***                Z-ADD     NUMCNU        NUMcnu
054800011203     C                   Z-ADD     NUMPO1        VPO1
054900011203     C                   Z-ADD     NUMPO2        VPO2
055000011206     C                   Z-ADD     NUMUNP        VNUM
055100011203     C                   Z-ADD     NUMRIN        VRIN
055200011203     C                   Z-ADD     NUMRFI        VRFI
055300011203     C                   MOVEL     NUMFLO        VFLO
055400011206     C* GIRO LA DATA DOCUMENTO
055500020215IF  1C                   if        NUMdac > *zero
055600011206     C                   CLEAR                   WLBDAT
055700011206     C                   Z-ADD     NUMDAC        G02INV
055800011206     C                   MOVEL     '3'           G02ERR
055900011206     C                   CALL      'XSRDA8'
056000011206     C                   PARM                    WLBDAT
056100020204     C                   MOVE      G02DAT        WdataEUR
056200020204     C                   MOVE      WdataEUR      WdataDMY
056300020206     C                   MOVE      WdataDMY      VDAC
056400020215X   1C                   else
056500020215     C                   clear                   VDAC
056600020215E   1C                   endif
056700011203     C*
056800011203     C* DECODIFICA CODICE NUMERATORE
056900020204     C                   MOVEL     §CNUDES       VIDDNU
057000020206     C*
057100020206     C* DECODIFICA CODICE P.O.1
057200020206IF  1C     VPO1          IFNE      *ZEROS
057300020206     C     VPO1          CHAIN     AZORG01L                           31
057400020206     C                   MOVEL     *BLANKS       VIDDPO
057500020206     C  N31              MOVEL     ORGDES        VIDDPO
057600020206     C   31              MOVEL     *ALL'?'       VIDDPO
057700020206X   1C                   ELSE
057800020206     C                   MOVEL     *BLANKS       VIDDPO
057900020206E   1C                   ENDIF
058000020114     C*
058100020114     C* PROTEZIONE DEI CAMPI CHIAVE (A SECONDA DEL TIPO NUMERATORE)
058200020114IF  1C                   if        §CNUkda = 'S'
058300020114     C                   movea     '01'          *in(72)
058400020114X   1C                   else
058500020114     C                   movea     '10'          *in(72)
058600020114E   1C                   endif
058700011203     C*
058800011203     C* NUMERATORE MANUTENZIONABILE SOLO DA 'EDP'
058900020114IF  1C     §CNUEDP       IFEQ      'S'
059000011203     C     PROFIL        ANDNE     'EDP'
059100011203     C                   SETON                                        98
059200020114X   1C                   ELSE
059300020114     C                   SETOFF                                       98
059400020114E   1C                   ENDIF
059500011206     C*
059600011206     C* HHHNRR: SONO IN VARIAZIONE O IN INSERIMENTO
059700011206IF  1C     HHHNRR        IFEQ      0
059800011206     C                   MOVE      *HIVAL        HHHNRR
059900011206E   1C                   ENDIF
060000011203     C*
060100020114     C* I record già inseriti non vengono impostati come modificati
060200020114     C* (pertanto: non verranno controllati se non "ritoccati" -
060300020206     C*  eventuali errori già esistenti non verranno segnalati !)
060400020114     C                   SETOFF                                       80
060500020114     C***                SETON                                        80
060600011203     C                   WRITE     BS16SFL
060700020114     C***                SETOFF                                       80
060800011203     C*
060900011203     C                   ENDSR
061000011203    ?C*---------------------------------------------------------------*
061100011203    ?C* ROUTINE DI CARICAMENTO SFILE PER ANNO NUMERATORI INESISTENTE  *
061200011203    ?C*---------------------------------------------------------------*
061300011203     C     CARVUO        BEGSR
061400011203     C*
061500020204     C                   SETOFF                                       98  80
061600020204     C                   SETOFF                                       717273
061700011203     C*
061800011203     C                   Z-ADD     WNRR          NRR
061900011203     C*
062000020206     C* calcolo il numero "residuo" di righe al completamento
062100020206     C*  della pagina
062200020204     C     WNRR          div       18            WNRRW
062300020204     C                   mvr                     WNRRW
062400020204     C                   eval      WNRRW = 18 - WNRRW
062500020204DO  1C                   DO        WNRRW
062600020206     C*
062700011203     C                   ADD       1             NRR
062800011206     C                   clear                   HHHNRR
062900011206     C                   clear                   HDAT
063000020114     C                   clear                   HCNUKDA
063100011206     C                   clear                   VIDANN
063200020111IF  2C                   IF        *in70 = *on
063300020111     C                   eval      *in80 = *on
063400020204     C                   movel     V1CSOC        VSOC
063500020111     C                   z-add     V1CANN        VAAA
063600020130     C                   move      V1CNUM        NUMCNU
063700020204     C                   z-add     V1CPO1        VPO1
063800020204     C                   z-add     V1CPO2        VPO2
063900020206     C                   move      Udate         VDAC
064000020111X   2C                   ELSE
064100020204     C                   clear                   VSOC
064200020111     C                   clear                   VAAA
064300020111     C                   clear                   NUMCNU
064400020111     C                   clear                   VPO1
064500020111     C                   clear                   VPO2
064600020206     C                   clear                   VDAC
064700020111E   2C                   ENDIF
064800011206     C                   clear                   VNUM
064900011206     C                   clear                   VRIN
065000011206     C                   clear                   VRFI
065100011206     C                   clear                   VFLO
065200020204     C                   clear                   VIDDNU
065300020204     C                   clear                   VIDDPO
065400020111     C*
065500011203     C                   WRITE     BS16SFL
065600020111     C*
065700020111IF  2C                   IF        *in70 = *on
065800020111     C                   leave
065900020111E   2C                   ENDIF
066000020206     C*
066100011203E   1C                   ENDDO
066200011203     C*
066300011203     C                   ENDSR
066400011203    ?C*---------------------------------------------------------------*
066500011203    ?C* CONTROLLO CAMPI SFILE                                         *
066600011203    ?C*---------------------------------------------------------------*
066700020114     C     CTRSFL        BEGSR
066800020114     C*
066900011203     C                   SETOFF                                       90
067000020204     C                   movea     wZero20       *in(10)
067100011203     C*
067200011203     C* NON CONTROLLO DATI SUBFILE SE ANNULLO LA RIGA
067300020204     C     VIDANN        cabeq     'A'           ENDCNT
067400020204     C*
067500020204     C* e neanche quelli prima immessi e poi "cancellati"
067600020204IF  1C                   if             VAAA   =  0
067700020204     C                             and  NUMCNU =  0
067800020204     C                             and  VPO1   =  0
067900020204     C                             and  VPO2   =  0
068000020206     C                             and  VDAC   =  0
068100020204     C                   clear                   VIDDNU
068200020204     C                   clear                   VIDDPO
068300020204     C                   clear                   HCNUKDA
068400020204     C                   clear                   VNUM
068500020206     C                   clear                   VDAC
068600020204     C                   clear                   VRIN
068700020204     C                   clear                   VRFI
068800020204     C                   clear                   VSOC
068900020204     C                   clear                   VFLO
069000020204     C                   movea     '00'          *in(72)
069100020204     C                   goto      ENDCNT
069200020204E   1C                   endif
069300011203     C*
069400020204     C* REPERIMENTO DATI NUMERATORE DA TABELLA CNU
069500020114     C                   move      NUMcnu        SAVcnu
069600020204IF  1C                   if        SAVcnu <> %subst(T02ke1:1:3)
069700011203     C                   clear                   TIBS02DS
069800011203     C                   movel     'C'           t02mod
069900011203     C                   movel     knsif         t02sif
070000011203     C                   movel     'CNU'         t02cod
070100011203     C                   movel(p)  NUMCNU        t02ke1
070200011203     C                   call      'TIBS02R'
070300011203     C                   parm                    KPJBA
070400011203     C                   parm                    TIBS02DS
070500020204E   1C                   endif
070600011203     C*
070700020204     C* CODICE NUMERATORE INESISTENTE
070800020204IF  1C                   IF        T02err <> *blanks
070900020204     C                   CLEAR                   DCNU
071000020204     C                   CLEAR                   VIDDNU
071100020204     C                   SETON                                        10  90
071200020204     C                   GOTO      ENDCNT
071300020204E   1C                   ENDIF
071400011203     C*
071500011203     C                   MOVEL     T02UNI        DCNU
071600020204     C                   MOVEL     §CNUDES       VIDDNU
071700011203     C*
071800011203     C* IMMESSA UNA NUOVA RIGA
071900020204IF  1C     HHHNRR        IFEQ      0
072000020114     C*
072100020114     C                   eval      HCNUKDA = §CNUKDA
072200011203     C*
072300020204     C* CONTROLLO SE CHIAVE DEL NUMERATORE GIA' ESISTENTE
072400020204IF  2C     §CNUKDA       IFEQ      *BLANKS
072500020206     C                   MOVEL     VSOC          KSOC
072600020206     C                   Z-ADD     VAAA          KANN
072700011203     C                   Z-ADD     VPO1          KPO1
072800011203     C                   Z-ADD     VPO2          KPO2
072900020306     C     KEYNUM        CHAIN(n)  AZNUM01L                           30
073000020204X   2C                   ELSE
073100020206     C                   MOVEL     VSOC          KSOC
073200020204     C                   Z-ADD     G02INV        KDAT
073300020204     C                   Z-ADD     VPO1          KPO1
073400020204     C                   Z-ADD     VPO2          KPO2
073500020306     C     K5NUM2        CHAIN(n)  AZNUM02L                           30
073600020204E   2C                   ENDIF
073700020204IF  2C                   IF        not *IN30
073800020204     C                   SETON                                        11  90
073900020204     C                   GOTO      ENDCNT
074000020204E   2C                   ENDIF
074100020204     C*
074200020204     C* CONTROLLI PER P.O.1
074300020204IF  2C                   IF        VPO1 <> *ZEROS
074400020204     C     VPO1          CHAIN     AZORG01L
074500020204IF  3C                   if        not %found(AZORG01L)
074600020204     C                             or  ORGfva  <> *blanks
074700020204     C                   clear                   ORGdes
074800020204E   3C                   endif
074900020204     C* - ESISTENZA P.O.1
075000020204IF  3C                   IF        (not %found(AZORG01L)
075100020204     C                              or  ORGFVA  <> *blanks
075200020204     C                              or (ORGfag  <> 'A'
075300020204     C                              and ORGfag  <> 'F'))
075400020204     C                             and  §CNUp1n <> 'S'
075500020204     C                   SETON                                        17  90
075600020204     C                   GOTO      ENDCNT
075700020204E   3C                   ENDIF
075800020204     C                   MOVEL     ORGDES        VIDDPO
075900020204X   2C                   ELSE
076000020204     C                   CLEAR                   VIDDPO
076100020204E   2C                   ENDIF
076200020304     C* COMPATIBILITA' P.O.1
076300020304     C                   move      VPO1          wPO
076400020304IF  2C                   if            §CNUpo1 =  *blanks
076500020304     C                             and VPO1    <> *zeros
076600020304     C                             or  §CNUpo1 >= *zeros
076700020304     C                             and §CNUpo1 <> wPO
076800020304     C                   seton                                        18  90
076900020304     C                   goto      ENDCNT
077000020304E   2C                   endif
077100020212     C*
077200020204     C* CONTROLLI PER P.O.2
077300020204IF  2C                   IF        VPO2 <> *ZEROS
077400020204     C     VPO2          CHAIN     AZORG01L
077500020204IF  3C                   if        not %found(AZORG01L)
077600020204     C                             or  ORGfva  <> *blanks
077700020204     C                   clear                   ORGdes
077800020204E   3C                   endif
077900020204     C* - ESISTENZA P.O.2
078000020204IF  3C                   IF        (not %found(AZORG01L)
078100020204     C                              or  ORGFVA  <> *blanks
078200020204     C                              or (ORGfag  <> 'A'
078300020204     C                              and ORGfag  <> 'F'))
078400020208     C                             and  §CNUp2n <> 'S'
078500020304     C                   SETON                                        19  90
078600020204     C                   GOTO      ENDCNT
078700020204E   3C                   ENDIF
078800020204E   2C                   ENDIF
078900020304     C* COMPATIBILITA' P.O.2
079000020304     C                   move      VPO2          wPO
079100020304IF  2C                   if            §CNUpo2 =  *blanks
079200020304     C                             and VPO2    <> *zeros
079300020304     C                             or  §CNUpo2 >= *zeros
079400020304     C                             and §CNUpo2 <> wPO
079500020304     C                   seton                                        20  90
079600020304     C                   goto      ENDCNT
079700020304E   2C                   endif
079800020204     C*
079900020204     C* CONTROLLO SE IL PROFILO UTENTE CONSENTE DI INSERIRE IL CODICE
080000020204IF  2C     §CNUEDP       IFEQ      'S'
080100020204     C     PROFIL        ANDNE     'EDP'
080200020304     C                   SETON                                        21  90
080300020204     C                   GOTO      ENDCNT
080400020204E   2C                   ENDIF
080500020204     C*
080600020204E   1C                   ENDIF
080700020204     C*
080800020204     C* CONTROLLO SE ANNO CONGRUENTE CON COD.NUMERATORE
080900020206     C* - Anno non richiesto
081000020206IF  1C     §CNUFAA       IFEQ      *BLANKS
081100020206     C     VAAA          ANDGT     0
081200020204     C                   SETON                                        13  90
081300020204     C                   GOTO      ENDCNT
081400020204E   1C                   ENDIF
081500020206     C* - Anno obbligatorio
081600020204IF  1C     §CNUFAA       IFEQ      'S'
081700020204IF  1C     VAAA          ANDEQ     0
081800020204     C                   SETON                                        13  90
081900020204     C                   GOTO      ENDCNT
082000020204E   1C                   ENDIF
082100020204IF  1C     §CNUFAA       IFEQ      'S'
082200020204IF  2C     VAAA          IFLT      ANNOME
082300020204     C     VAAA          ORGT      ANNOPI
082400020204     C                   SETON                                        14  90
082500020204     C                   GOTO      ENDCNT
082600020204E   2C                   ENDIF
082700020204E   1C                   ENDIF
082800020204     C*
082900020204     C* DATA OBBLIGATORIA
083000020206IF  1C     VDAC          IFEQ      *ZEROS
083100020204IF  2C                   IF        HHHNRR = 0
083200020206     C                   MOVE      Udate         VDAC
083300020204X   2C                   ELSE
083400020204     C                   SETON                                        15  90
083500020204     C                   GOTO      ENDCNT
083600020204E   2C                   ENDIF
083700020204     C* DATA FORMALMENTE CORRETTA
083800020204X   1C                   ELSE
083900020206     C                   Z-ADD     VDAC          G02DAT
084000020204     C                   MOVEL     *BLANKS       G02ERR
084100020204     C                   CALL      'XSRDA8'
084200020204     C                   PARM                    WLBDAT
084300020204IF  2C     G02ERR        IFEQ      '1'
084400020204     C                   SETON                                        16  90
084500020204     C                   GOTO      ENDCNT
084600020204E   2C                   ENDIF
084700020204E   1C                   ENDIF
084800020204     C*
084900020226     C* IMMETTERE I RANGE
085000020226IF  1C                   IF        VRIN =  0
085100020204     C                   eval      VRIN =  §CNUrin
085200020226E   1C                   ENDIF
085300020226IF  1C                   IF        VRFI =  0
085400020204     C                   eval      VRFI =  §CNUrfi
085500020226E   1C                   ENDIF
085600020226IF  1C                   IF        VRFI =  0
085700020304     C                   SETON                                        22  90
085800020204     C                   GOTO      ENDCNT
085900020226E   1C                   ENDIF
086000011203     C*
086100020114     C* RANGE DA >= RANGE A
086200020226IF  1C                   IF            VRFI <> *ZEROS
086300020204     C                             and VRFI <  (VRIN + §CNUinc)
086400020304     C                   SETON                                        23  90
086500020204     C                   GOTO      ENDCNT
086600020226E   1C                   ENDIF
086700011203     C*
086800011203     C* NON INSERIRE RANGE DI VALORI PER QUESTO CODICE
086900020208     C*
087000020208     C* ULTIMO NUMERO PRELEVATO FUORI RANGE
087100020226IF  1C                   IF            VNUM   <> *zeros
087200020208     C                             and (VNUM   < (VRIN - §CNUinc)
087300020208     C                              or  VNUM   > VRFI)
087400020304     C                   seton                                        24  90
087500020208     C                   goto      ENDCNT
087600020208E   1C                   ENDIF
087700011203     C*
087800011203     C* SE ERRORE MI POSIZIONO SULLA PAGINA DEL RECORD SBAGLIATO
087900011203     C     ENDCNT        TAG
088000011203     C   90              Z-ADD     NRR           HSRN
088100011203     C*
088200011203     C                   ENDSR
088300011203    ?C*---------------------------------------------------------------*
088400011203    ?C* ROUTINE DI GESTIONE AGGIORNAMENTI RECORD AZNUM01L             *
088500011203    ?C*---------------------------------------------------------------*
088600011203     C     AGGIOR        BEGSR
088700011203     C*
088800011203     C                   READC     BS16SFL                                60
088900011203     C*
089000020204DO  1C                   DOW       *IN60 = *OFF
089100011203     C*
089200020204IF  2C                   IF        HHHNRR = 0
089300020114     C*
089400020114IF  3C                   IF             VIDANN =  *BLANKS
089500020213     C                             and (VSOC   <> *BLANKS
089600020213     C                              or  VAAA   <> 0
089700020114     C                              or  NUMCNU <> 0
089800020114     C                              or  VPO1   <> 0
089900020204     C                              or  VPO2   <> 0
090000020213     C                              or  VDAC   <> 0)
090100020114     C                   EXSR      WRTREC
090200020114E   3C                   ENDIF
090300011203     C*
090400020114X   2C                   ELSE
090500011203     C*
090600020204IF  3C                   IF        VIDANN <> 'A'
090700011203     C                   MOVEL     'U'           WREC              1
090800020114     C                   EXSR      UPDREC
090900011203     C                   ELSE
091000011203     C                   MOVEL     'D'           WREC              1
091100020114     C                   EXSR      UPDREC
091200020114E   3C                   ENDIF
091300020114     C*
091400020114E   2C                   ENDIF
091500011203     C*
091600011203     C                   READC     BS16SFL                                60
091700020114E   1C                   ENDDO
091800011203     C*
091900011203     C                   ENDSR
092000011203    ?C*---------------------------------------------------------------*
092100011203    ?C* ROUTINE DI INSERIMENTO RECORD AZNUM01L                        *
092200011203    ?C*---------------------------------------------------------------*
092300011203     C     WRTREC        BEGSR
092400011203     C*
092500020114     C                   EXSR      CARREC
092600011203     C                   WRITE     AZNUM000
092700011203     C*
092800011203     C                   ENDSR
092900011203    ?C*---------------------------------------------------------------*
093000020114    ?C* CARICO DATI DEL SFL NEL RECORD DI AZNUM000                    *
093100011203    ?C*---------------------------------------------------------------*
093200011203     C     CARREC        BEGSR
093300020114     C*
093400011203     C* GIRO DATA IN A/M/G
093500011203     C                   CLEAR                   WLBDAT
093600020206     C                   Z-ADD     VDAC          G02DAT
093700011203     C                   CALL      'XSRDA8'
093800011203     C                   PARM                    WLBDAT
093900011203     C                   MOVE      G02INV        NUMDAC
094000011203     C*
094100020108     C                   MOVEL     VSOC          NUMSOC
094200020108     C                   MOVE      VAAA          NUMAAA
094300020108     C***                Z-ADD     NUMCNU        NUMCNU
094400011203     C                   Z-ADD     VPO1          NUMPO1
094500011203     C                   Z-ADD     VPO2          NUMPO2
094600011203     C                   Z-ADD     VNUM          NUMUNP
094700011203     C                   Z-ADD     VRIN          NUMRIN
094800011203     C                   Z-ADD     VRFI          NUMRFI
094900011203     C                   MOVEL     VFLO          NUMFLO
095000011203     C*
095100011203     C                   ENDSR
095200011203    ?C*---------------------------------------------------------------*
095300011203    ?C* ROUTINE DI AGGIORNAMENTO RECORD AZNUM01L                      *
095400011203    ?C*---------------------------------------------------------------*
095500011203     C     UPDREC        BEGSR
095600020114     C*
095700011203     C     SAVNUM        IFNE      NUMUNP
095800011203     C                   clear                   TIBS02DS
095900011203     C                   movel     'C'           t02mod
096000011203     C                   movel     knsif         t02sif
096100011203     C                   movel     'CNU'         t02cod
096200011203     C                   movel(p)  NUMCNU        t02ke1
096300011203     C                   call      'TIBS02R'
096400011203     C                   parm                    KPJBA
096500011203     C                   parm                    TIBS02DS
096600011203IF  1C                   IF        T02err = *blanks
096700011203     C                   movel     T02uni        DCNU
096800011203     C                   else
096900011203     C                   clear                   DCNU
097000011203E   1C                   endif
097100011203     C*
097200011203     C                   MOVEL     NUMUNP        SAVNUM
097300011203     C                   ENDIF
097400011203     C*
097500020206     C                   MOVEL     VSOC          KSOC
097600020206     C                   Z-ADD     VAAA          KANN
097700011203     C                   Z-ADD     VPO1          KPO1
097800011203     C                   Z-ADD     VPO2          KPO2
097900020206     C     §CNUKDA       IFEQ      *BLANKS
098000011203     C     KEYNUM        CHAIN     AZNUM01L                           95
098100011203     C                   ELSE
098200011203     C                   MOVEL     HDAT          KDAT
098300020206     C     K5NUM2        CHAIN     AZNUM02L                           95
098400011203     C                   ENDIF
098500011203     C*
098600011203     C     WREC          IFEQ      'U'
098700011203     C                   EXSR      CARREC
098800020206     C  N95§CNUKDA       IFEQ      *BLANKS
098900020206     C                   UPDATE    AZNUM000
099000011203     C                   ELSE
099100020206     C                   UPDATE    AZNUM02
099200011203     C                   ENDIF
099300020114     C*
099400011203     C   95              WRITE     AZNUM000
099500011203     C                   ENDIF
099600011203     C*
099700011203     C  N95WREC          IFEQ      'D'
099800020206     C     §CNUKDA       IFEQ      *BLANKS
099900011203     C                   DELETE    AZNUM000
100000011203     C                   ELSE
100100011203     C                   DELETE    AZNUM02
100200011203     C                   ENDIF
100300011203     C                   ENDIF
100400011203     C*
100500011203     C                   ENDSR
100600011203    ?C*---------------------------------------------------------------*
100700011203    ?C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
100800011203    ?C*---------------------------------------------------------------*
100900011203     C     DEFCAM        BEGSR
101000011203     C*
101100011203     C     *ENTRY        PLIST
101200011203     C                   PARM                    KPJBA
101300020204     C*
101400020204     C* Reperimento dati utente
101500020204     C                   eval      CODUT = 1
101600020204     C                   call      'X§PARUT'
101700020204     C                   parm                    UT§DSE0F
101800020204     C                   movel     RAGUT         RSUT
101900020204     C                   movel     REC80         CNCR80
102000020114     C*
102100020114     C                   Z-ADD     0             NRR               4 0
102200020114     C                   Z-ADD     0             WNRR              4 0
102300020204     C                   Z-ADD     0             WNRRW             4 0
102400020114     C*
102500020115     C                   eval      ANNOme = *year - 3
102600020115     C                   eval      ANNOpi = *year + 3
102700011203     C*
102800011203     C* LETTURA AZNUM01L PER L'INTERA CHIAVE
102900011203     C     KEYNUM        KLIST
103000020206     C                   KFLD                    KSOC
103100011203     C                   KFLD                    KANN
103200011203     C                   KFLD                    NUMCNU
103300020115     C                   KFLD                    KPO1
103400020115     C                   KFLD                    KPO2
103500011203     C*
103600020206     C* AZNUM01L per SOCIETA', ANNO, COD.NUMERATORE, P.O.1 e P.O.2
103700020206     C     K5NUM1        KLIST
103800020206     C                   KFLD                    KSOC
103900020206     C                   KFLD                    KANN
104000020206     C                   KFLD                    KNUM
104100020206     C                   KFLD                    V1CPO1
104200020206     C                   KFLD                    V1CPO2
104300020206     C     K4NUM1        KLIST
104400020206     C                   KFLD                    KSOC
104500011203     C                   KFLD                    KANN
104600011203     C                   KFLD                    KNUM
104700020204     C                   KFLD                    V1CPO1
104800020206     C     K3NUM1        KLIST
104900020206     C                   KFLD                    KSOC
105000020206     C                   KFLD                    KANN
105100020206     C                   KFLD                    KNUM
105200020206     C     K2NUM1        KLIST
105300020206     C                   KFLD                    KSOC
105400020206     C                   KFLD                    KANN
105500020114     C*
105600020206     C* AZNUM02L per SOCIETA', COD.NUMERATORE, ANNO, P.O.1 e P.O.2
105700020206     C     K5NUM2        KLIST
105800020206     C                   KFLD                    KSOC
105900011203     C                   KFLD                    NUMCNU
106000011203     C                   KFLD                    KDAT
106100011203     C                   KFLD                    KPO1
106200011203     C                   KFLD                    KPO2
106300020206     C     K2NUM2        KLIST
106400020206     C                   KFLD                    KSOC
106500020206     C                   KFLD                    KNUM
106600011203     C*
106700011203     C                   ENDSR
