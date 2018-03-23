000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940531     H* TNTA02R2 *---------------------------------------------------*
000300940531     H*               INTERROGAZIONE TARIFFE                         *
000400000000     H*--------------------------------------------------------------*
000500940615     FTNTA01D2  CF   E             WORKSTN
000600940811     F                                     SFILE(TA01S03:NRR)
000700940615     FTNTAM01L  IF   E           K DISK
000800990622     FTITAD04L  IF   E           K DISK
000900990622     FTITPT01L  IF   E           K DISK
001000990622     FTITGC01L  IF   E           K DISK
001100000000     FTABEL00F  IF   E           K DISK
001200101013     ftisis01l  if   e           k disk
001300940531     D*
001400940531     D* DEFINIZIONE SCHIERE
001500041220     D CCT             S              2    DIM(600)
001600041220     D DCT             S             10    DIM(600)
001700041220     D ORP             S              5    DIM(600)
001800951009     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
001900940531     D*
002000940531     D TES             S             35    DIM(1) CTDATA PERRCD(1)
002100940622     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
002200051202      *
002300051202     d Status         sds
002400051202     d  SDSpgm           *proc
002500940615     D*
002600990622     D* CHIAVE X CHAIN COMPLETO SU FILE TITAD
002700000000     D KIAVE           DS
002800891005     D  TADLNP                 1      3  0
002900990622     D  TADORP                 4      8
003000990622     D  TADNAZ                 9     11
003100990622     D  TADCAP                12     20
003200990622     D  TADSGL                21     33  3
003300940531     D* CHIAVE INIZIALE SUBFILE AGGIORNAMENTO
003400000000     D KI              DS
003500891005     D  LNPI                   1      3  0
003600990622     D  ORPI                   4      8
003700990622     D  NAZI                   9     11
003800990622     D  PARI                  12     20
003900990622     D  SCGI                  21     33  3
004000940531     D*  CHIAVE FINALE SUBFILE AGGIORNAMENTO
004100000000     D KE              DS
004200891005     D  LNPE                   1      3  0
004300990622     D  ORPE                   4      8
004400990622     D  NAZE                   9     11
004500990622     D  PARE                  12     20
004600990622     D  SCGE                  21     33  3
004700940531     D** CHIAVE X CHAIN SCORRIMENTO ROLLUP E ROLLDOWN
004800000000     D KX              DS
004900891005     D  LNPX                   1      3  0
005000990622     D  ORPX                   4      8
005100990622     D  NAZX                   9     11
005200990622     D  PARX                  12     20
005300990622     D  SCGX                  21     33  3
005400941201     D*
005500941201     D** PARAMETRI PASSATI DAL TNTA02R
005600941201     D PARAMX          DS
005700941201     D  V1CKSC                 2      8  0
005800941201     D  PRG                    9     11  0
005900941201     D  CCC                   12     15  0
006000941201     D  V1DKSC                16     63
006100941201     D  VCTR                  64     66  0
006200990914     D* FLAG DI RITORNO
006300990914     D* F12 = 'R' RITORNO ALLA VIDEATA
006400990914     D* F06/F03 = 'C' FINE PROGRAMMA
006500990914     D  FLGRTN               127    127
006600990914     D*
006700940310     D*
006800941220     D* PASSAGGIO AL FILTRO DELLA STAMPA TESTI       - FNLV43R -
006900900424     D PARAM2          DS
007000940310     D* NUMERO VISITA E PROGRESSIVO
007100940310     D  PA2NRV                 3      9  0 INZ
007200940310     D* CODICE CLIENTE
007300940310     D  PA2CLI                10     16  0
007400940310     D* CODICE CLIENTE POTENZIALE
007500940310     D  PA2CPO                17     27  0 INZ
007600941220     D* TIPO STAMPA TARIFFA -->PER FARE LE GIUSTE OVRDBF  AL TNTA43R
007700941220     D  PA2TST                98     98    INZ
007800940310     D* AMBIENTE STAMPA TESTI ("V"=VISITE/"C"=CLIENTI/"P"=POTENZIALI)
007900941220     D  PA2AST                99     99    INZ('C')
008000940811     D* COD TARIFFA E PROGRESSIVO PER STAMPA TARIFFA   - TNTA43R
008100940310     D  PA2CTR               100    102  0
008200940310     D  PA2PRG               103    105  0
008300940310     D* '2' -- DA OFFERTA / TARIFFA
008400940310     D  PA2FLG               106    106    INZ('2')
008500940310     D* RAGIONE SOCIALE
008600940310     D  PA2RGS               107    152
008700940310     D*
008800941221     D* RICHIAMO PGM INTERROGAZIONE TARIFFE GIACENZA   - TNTA15R -
008900940318     D PARAM3          DS
009000941221     D* CODICE CLIENTE
009100940318     D  PA3CLI                10     16  0
009200941221     D* DECODIFICA CODICE TARIFFA
009300941221     D  PA3DCT                17     26
009400941221     D* DESCRIZIONE TARIFFA
009500941221     D  PA3DCV                27     41
009600990623     D* DESCRIZIONE DELLA DIVISA
009700990824     D  PA3DIV                43     45
009800941221     D* CODICE TARIFFA E PROGRESSIVO
009900941221     D  PA3CTR               125    127  0
010000941221     D  PA3PRG               128    130  0
010100940531     D*
010200950116     D** PARAMETRI PER INTERROG.TARIFFE PARTICOLARI - TNTA68R -
010300920708     D PARAM5          DS
010400950116     D* CODICE CLIENTE/NUMERO VISITA
010500920708     D  CLI5                   2      8  0
010600950116     D* RAGIONE SOCIALE
010700920708     D  RAG5                   9     56
010800950116     D* PROGRESSIVO TARIFFA
010900940617     D  V1CPRG                57     59  0
011000950116     D* CODICE TARIFFA
011100940617     D  V1CCTR                60     62  0
011200950116     D* DECODIFICA CODICE TARIFFA
011300940628     D  V1DCTR                63     72
011400950116     D* DESCRIZIONE TARIFFA
011500940617     D  V1CDCV                73     87
011600950116     D* TESTATA PROGRAMMA
011700940628     D  V1CTES                88    121
011800950116     D* FLG = " " ---> TARIFFE     FLG <> " " ---> OFFERTE
011900950116     D  FLG                  122    122    INZ
012000950116     D* FLG4   --> SE RESTITUITO PIENO INDICA CHE E' STATO PREMUTO CMD3
012100921119     D  FLG4                 123    123
012200970325     D  PARFIE               125    125
012300100806     D* FLAG CLIENTE POSTE - non più impostato
012400000420     D  WRKPT                127    127
012500990623     D* DECODIFICA DELLA DIVISA
012600990824     D  V1CDIV               128    130
012700020201     D* FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
012800020201     D  PA5FDC               131    131
012900020201     D* DATA DECORRENZA TARIFFA
013000020201     D  PA5DDT               132    139  0
013100020201     D* TIPO SERVIZIO BOLLE
013200020201     D  PA5TSP               140    140
013300020201     D* Flag Network
013400020201     D  FLGNET               141    141
013500120319
013600120319       // -?Flag booleani
013700120319     d wFine           s               n   inz(*off)
013800120319     d wPrima          s               n   inz(*off)
013900120319     d wSeconda        s               n   inz(*off)
014000120319     d wTerza          s               n   inz(*off)
014100120319     d wQuarta         s               n   inz(*off)
014200120319
014300120319       // -?Campi x gestione riga mobile tasti funzione
014400120319     d wTfv            s             61    dim(20) ctdata perrcd(1)
014500120319     d wf24            s              2  0
014600120319     d wpos            s              2  0
014700120319     d wposda          s              2  0
014800120319     d wpostf1         s              5  0
014900120319     d wpostf2         s              5  0
015000120319     d wpostf3         s              5  0
015100120319     d wrig            s              2  0
015200120319     d wtf1            s             61
015300120319     d wtf2            s             61
015400120319     d wtf3            s             61
015500120319     d tf              s              3  0
015600120319
015700961212     D*
015800961212     D* DS PER TNTE01R - TARIFFE DA C/ECONOMICO
015900961212     D DSTE00        E DS                  EXTNAME(TNTE00DS)
016000940811     D*
016100940811     D                 DS
016200940811     D  AA                     1      4  0
016300940811     D  MM                     5      6  0
016400940811     D  GG                     7      8  0
016500940811     D  DATA                   1      8  0
016600970130     D                 DS
016700970130     D  ROVGG                  1      2  0
016800970130     D  ROVMM                  3      4  0
016900970130     D  ROVAA                  5      8  0
017000970130     D  ROVDAT                 1      8  0
017100970130     D*
017200020206     D DSCT          E DS
017300960209     D*
017400960209     D* DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT/GEST PARTIC.
017500960209     D D7PQRS        E DS                  EXTNAME(DS7PQRS)
017600941123     D DSTR          E DS
017700101013
017800101013     d TRTB12ds1     e ds                  inz
017900101013     d   D12opz      e                     inz('5')
018000031119      *
018100000000     D KPJBA         E DS
018200900424     D CNCR80        E DS
018300900424     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018400980504     D DSTA01        E DS
018500090608     d ds5e          e ds
018600031124
018700031124     d Tibs02Ds      e ds
018800031124      * Ds data massima elaborazione CAT da passare al TNTE01R
018900031124     d dcat          e ds
019000030718      * ds per visualizzazione variazioni tariffe
019100030718     D TNTA04DS      E DS
019200090709
019300090709     d tnta12ds      e ds
019400100407     d TNTA28DS      e ds
019500100407     d TRMK20DS      e ds
019600101013
019700101013     d wDate_EUR       s               d   datfmt(*eur)
019800101013     d wDate_ISO       s               d   datfmt(*iso)
019900120321
020000120321       // -?ds dati passati da TNSB48R - Manca Tariffa
020100120321     d TNSB48ds      e ds
020200120321     d Parm48          s                   like(tnsb48ds)
020300120713
020400120713       // -?passaggio dati a TRTB09R - Ricerca codice tassazione
020500120713     d TRTB09DS      e ds
020600120319
020700120319      //---------------------------------------------------------------
020800120319      //?Costanti
020900120319      //---------------------------------------------------------------
021000120319     d cf2413          c                   const('F24=AlTasti(1/3)')
021100120319     d cf2423          c                   const('F24=AlTasti(2/3)')
021200120319     d cf2433          c                   const('F24=AlTasti(3/3)')
021300120319     d cf2412          c                   const('F24=AlTasti(1/2)')
021400120319     d cf2422          c                   const('F24=AlTasti(2/2)')
021500120319
021600120319       // -?campo per dspatr NO DISPLAY + protetto
021700120319     d nodisp          C                   CONST(X'A7')
021800120319
021900120319       // -?campo per dspatr protetto
022000120319     d protet          C                   CONST(X'A6')
022100101013
022200101013      //---------------------------------------------------------------
022300101013      //?Definizione procedure esterne.
022400101013      //---------------------------------------------------------------
022500101013      // - Reperimento Scatti ISTAT
022600101013     d trtb12r1        pr                  extpgm('TRTB12R1')
022700101013     d  kpjba                              likeds(KPJBA)
022800120713
022900120713      // - Interrogazione codici tassazione
023000120713     d trtb09r         pr                  extpgm('TRTB09R')
023100120713     d  kpjba                              likeds(KPJBA)
023200100407
023300100407      //---------------------------------------------------------------
023400100407      //?prototipi
023500100407      //---------------------------------------------------------------
023600100407      /copy gaitrasrc/srcprotopr,tnta28r
023700100407      /copy gaitrasrc/srcprotopr,trmk20r
023800100407
023900030718      *
024000940531     C*****************************************************************
024100940531     C* RIEPILOGO INDICATORI
024200940531     C*****************************************************************
024300941019     C* 13    - ABILITO L'INTERROGAZIONE PARTICOLARITA' C/ASSEGNO
024400941126     C*         USATO ANCHE NEL DETTAGLIO PER VISUALIZZARE LA LETTERA
024500941126     C*         "M" CHE INDICA LA PRESENZA DEL MINIMO O DEL MASSIMO
024600940531     C* 15    - INTERROGAZIONE
024700950105     C* 16    - INDICA IL TIPO TARIFFA "4 - A VALORE"
024800130124     C* 63    - ABILITO L'INTERROGAZIONE PARTICOLARITA' GIACENZA
024900941019     C* 19    - ABILITO L'INTERROGAZIONE PARTICOALRITA' CONSEGNA
025000941019     C* 24    - ABILITO L'INTERROGAZIONE PARTICOLARITA' VARIE
025100940607     C* 25    - ROLLUP
025200940622     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
025300940607     C* 30    - DI COMODO
025400940607     C* 35    - USATO PER LOKUP
025500940903     C* 18    - NON VISUALIZZO LE NOTE IN SEDE
025600151016     C* 53    - Visualizzo applicazione calcolo peso desunto
025700941123     C* 88    - SI TRATTA DI TARIFFA A PERCENTUALE
025800950116     C* 95    - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
025900950116     C* 96    - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
026000120321      * 97    - Pgm richiamato x manca tariffa TNSB48
026100940531     C*****************************************************************
026200000000     C     *ENTRY        PLIST
026300000000     C                   PARM                    KPJBA
026400120321     c                   parm                    Parm48
026500120321
026600120321      /free
026700120321       //?controllo se passati tutti i parametri
026800120321         IF  %parms = 1;
026900120321           clear tnsb48ds;
027000120321           *in97 = *off;
027100120321         ELSE;
027200120321           clear tnsb48ds;
027300120321           tnsb48ds = parm48;
027400120321           *in97 = *on;
027500120321         ENDIF;
027600120321
027700120321         clear OSB48err;
027800120321         clear OSB48msg;
027900120321      /end-free
028000100505
028100000000     C                   Z-ADD     1             CODUT
028200900424     C                   CALL      'X§PARUT'
028300900424     C                   PARM                    UT§DSE
028400940811     C                   MOVEL     RAGUT         V1CRSU
028500900424     C                   MOVEL     REC80         CNCR80
028600100505
028700970130     C* LA DATA DEL GIORNO LA PRENDO DA TIME
028800970130     C                   TIME                    W0140            14 0
028900970130     C                   MOVE      W0140         UDATE8            8 0
029000970130     C                   Z-ADD     UDATE8        ROVDAT
029100970130     C                   Z-ADD     ROVGG         MMGG              4 0
029200970130     C                   MOVEL     ROVMM         MMGG
029300970130     C                   Z-ADD     MMGG          DATEU8            8 0
029400970130     C                   MOVEL     ROVAA         DATEU8
029500970404     C                   MOVE      KNSIF         WSIF              4
029600051202      * Nome pgm.
029700051202     c                   movel     SDSpgm        V1Cpgm
029800970130     C*
029900000000     C*---------------------------------------------------------------*
030000000000     C                   SETOFF                                       212223
030100000000     C                   SETOFF                                       242526
030200000000     C                   SETOFF                                       27
030300940622     C*
030400990622     C* ACCESSO TNTAM01L/TITAD04L
030500000000     C     KTAM          KLIST
030600940811     C                   KFLD                    V1CKSC
030700900409     C                   KFLD                    VCTR
030800900409     C                   KFLD                    PRG
030900990622     C* ACCESSO TITAD04L
031000000000     C     KSCOR         KLIST
031100940811     C                   KFLD                    V1CKSC
031200900409     C                   KFLD                    TAMCTR
031300000000     C                   KFLD                    PRG
031400000000     C                   KFLD                    LNPX
031500000000     C                   KFLD                    ORPX
031600990622     C                   KFLD                    NAZX
031700000000     C                   KFLD                    PARX
031800000000     C                   KFLD                    SCGX
031900000000     C     KTTAD         KLIST
032000940811     C                   KFLD                    V1CKSC
032100900409     C                   KFLD                    TAMCTR
032200000000     C                   KFLD                    PRG
032300940811     C                   KFLD                    V1CRDA
032400990622     C                   KFLD                    VIDORP            5
032500940531     C* ACCESSO TABEL00F
032600000000     C     KTAB          KLIST
032700000000     C                   KFLD                    CODUT
032800940531     C                   KFLD                    COD
032900940531     C                   KFLD                    KEY
033000940531     C     KTAB2         KLIST
033100920525     C                   KFLD                    CODUT
033200920525     C                   KFLD                    COD
033300940531     C*
033400940622     C* DEFINIZIONE CAMPI
033500940531     C     *LIKE         DEFINE    TBLCOD        COD
033600940531     C     *LIKE         DEFINE    TBLKEY        KEY
033700920525     C*
033800940531     C*---------------------------------------------------------------*
033900940622     C* 15 ON  - I N T E R R O G A Z I O N E
034000940622     C                   SETON                                        15
034100940531     C*
034200000000     C                   MOVEL     *ZEROS        KI
034300000000     C                   MOVEL     *ZEROS        KE
034400000000     C                   MOVEL     *ZEROS        KX
034500000000     C                   MOVEL     *ZEROS        KIAVE
034600000000     C                   MOVEL     *BLANKS       TADORP
034700940628     C                   MOVEL     *BLANKS       TADCAP
034800000000     C                   MOVEL     *BLANKS       ORPI
034900000000     C                   MOVEL     *BLANKS       PARI
035000000000     C                   MOVEL     *BLANKS       PARE
035100000000     C                   MOVEL     *BLANKS       ORPE
035200000000     C                   MOVEL     *BLANKS       ORPX
035300000000     C                   MOVEL     *BLANKS       PARX
035400990622     C                   MOVEL     *BLANKS       NAZI
035500990622     C                   MOVEL     *BLANKS       NAZE
035600990622     C                   MOVEL     *BLANKS       NAZX
035700940620     C*
035800940531     C* TESTATA PGM
035900940628     C                   MOVE      TES(1)        V1CTES
036000920708     C*
036100940903     C* 18 ON  - SONO IN FILIALE ---> IN SEDE NON VISUALIZZO LE NOTE
036200940903    1C  N18SIMFEL        IFNE      0
036300940903     C                   SETON                                        18
036400940629    1C                   ENDIF
036500940607     C*
036600940811     C                   Z-ADD     0             V1CKSC
036700000000     C                   Z-ADD     0             PRG
036800000000     C                   Z-ADD     0             CCC
036900940811     C                   Z-ADD     0             V1CRDA
037000940811     C                   MOVEL     *BLANKS       V1CRA
037100940811     C                   MOVEL     V1CRDA        CONFRP
037200940811     C                   MOVEL     V1CRDA        CONFR
037300940811     C                   MOVE      V1CRA         CONFRP
037400940811     C                   MOVE      V1CRA         CONFR
037500940811     C                   MOVEL     *BLANKS       V1DKSC
037600000000     C                   MOVEL     KPJBU         PARAMX
037700990914     C* IMPOSTO IL CODICE DI RITORNO AL PROGRAMMA CHIAMANTE
037800990914     C*
037900990914     C                   MOVEL     'R'           FLGRTN
038000990914     C****
038100951009     C                   MOVEL     V1CKSC        W0030             3 0
038200100806
038300940531     C** CARICAMENTO TABELLE
038400920525     C                   EXSR      CARTAB
038500940607     C*
038600000000     C** CAPO CONTO CLIENTI
038700940903     C                   SETOFF                                       011014
038800940615     C     KTAM          CHAIN     TNTAM000                           01
038900000000     C  N01TAMATB        COMP      'A'                                    01
039000900409     C   01              GOTO      FINE
039100000000     C** 10  AGGIORNAMENTO
039200920525     C                   SETON                                        10
039300940811     C                   Z-ADD     V1CKSC        CLI               7 0
039400950116     C* CARICO DATI VIDEATA
039500950116     C                   EXSR      RIED01
039600120319       //?Imposto i tasti funzione nella riga mobile
039700120319     c                   eval      wPrima = *on
039800120319     c                   eval      wSeconda = *off
039900120319     c                   eval      wTerza = *off
040000120319     c                   eval      wQuarta = *off
040100120319     c                   ExSr      Tastifun
040200151016
040300151016      /free
040400151016       //?visualizzo calcolo peso desunto in fatturazione se:
040500151016       //?utente di sede sempre
040600151016       //?utente di filiale solo se valorizzato
040700151016        *in53 = *off;
040800151016        IF  *in18 and V1Ctpr <> *blanks;
040900151016          *in53 = *on;
041000151016        ENDIF;
041100151016        IF  not *in18;
041200151016          *in53 = *on;
041300151016        ENDIF;
041400151016      /end-free
041500940629     C*
041600940603     C** EMISSIONE VIDEO 2
041700950116     C     FOR01         TAG
041800120319     C                   write     TA01Z01
041900940628     C                   EXFMT     TA01D01
042000051202     c                   setoff                                       28  90
042100051202     c                   clear                   V1Cmsg
042200120319       //?F3 - Fine
042300120319     c   kc              goto      fine
042400940622     C*
042500940622     C* CMD12 - FINE LAVORO
042600940531     C   KL              GOTO      FINE
042700030718     C*
042800950116     C* COMANDI
042900950116     C                   EXSR      COMAND
043000950116     C*
043100950116     C* WFLG = '1' ---> E' STATO PREMUTO UN TASTO DI FUNZIONE
043200950116    1C     WFLG          IFEQ      '1'
043300950116     C* SE PREMUTO CMD10 OPPURE SE PREMUTO CMD3 DALLE TARIF.PARTICOLARI
043400950116     C*   VADO A FINE PGM ALTRIMENTI RIEMETTO IL FORMATO
043500950116     C     *INKJ         IFEQ      *ON
043600950117     C     *INKV         OREQ      *ON
043700950117     C     FLG4          ANDNE     ' '
043800950116     C                   GOTO      FINE
043900950116     C                   ENDIF
044000950116     C*
044100950116     C                   GOTO      FOR01
044200950116    1C                   ENDIF
044300120319
044400120319       //?Imposto i tasti funzione nella riga mobile
044500120319     c                   eval      wPrima = *off
044600120319     c                   eval      wSeconda = *on
044700120319     c                   eval      wTerza = *off
044800120319     c                   eval      wQuarta = *off
044900120319     c                   ExSr      Tastifun
045000940629     C*
045100940811     C****
045200940811     C* EMISSIONE VIDEO 3
045300950116     C     FOR02         TAG
045400940811     C                   Z-ADD     0             V1CRDA
045500940811     C                   MOVEL     *BLANKS       V1CRA
045600940811     C*
045700120319     C                   write     TA01Z02
045800940811     C                   EXFMT     TA01D02
045900941201     C*
046000941201     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE
046100941201     C                   CLEAR                   V1CMSG
046200941201     C                   SETOFF                                       28
046300120319       //?F3 - Fine
046400120319     c   kc              goto      fine
046500960209     C** CMD12 - FINE LAVORO
046600960209     C   KL              GOTO      FOR01
046700960209     C**
046800960209     C                   SETOFF                                       31
046900960209     C* VEDO SE RICHIESTO IL '?' SULLE PARTICOLARITA
047000960209     C     V2CGCR        IFEQ      '?'
047100960209     C                   CLEAR                   D7PQRS
047200960209     C                   MOVEL     'S'           DS7RIC
047300960209     C                   MOVEL     'V'           DS7GES
047400960209     C                   MOVEL     V2CGCA        DS7COD
047500960209     C                   MOVEL     D7PQRS        KPJBU
047600960209     C                   CALL      'TRTB72R'                            31
047700960209     C                   PARM                    KPJBA
047800960209     C                   SETON                                          31
047900960209     C                   MOVEL     *BLANKS       V2CGCR
048000960209     C                   ENDIF
048100960209     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
048200960209     C     V2CGGR        IFEQ      '?'
048300960209     C                   CLEAR                   D7PQRS
048400960209     C                   MOVEL     'S'           DS7RIC
048500960209     C                   MOVEL     'V'           DS7GES
048600960209     C                   MOVEL     V2CGGA        DS7COD
048700960209     C                   MOVEL     D7PQRS        KPJBU
048800960209     C                   CALL      'TRTB69R'                            31
048900960209     C                   PARM                    KPJBA
049000960209     C                   SETON                                          31
049100960209     C                   MOVEL     *BLANKS       V2CGGR
049200960209     C                   ENDIF
049300960209     C*
049400960209     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
049500960209     C     V2CGMR        IFEQ      '?'
049600960209     C                   CLEAR                   D7PQRS
049700960209     C                   MOVEL     'S'           DS7RIC
049800960209     C                   MOVEL     'V'           DS7GES
049900960209     C                   MOVEL     V2CGMA        DS7COD
050000960209     C                   MOVEL     D7PQRS        KPJBU
050100960209     C                   CALL      'TRTB70R'                            31
050200960209     C                   PARM                    KPJBA
050300960209     C                   SETON                                          31
050400960209     C                   MOVEL     *BLANKS       V2CGMR
050500960209     C                   ENDIF
050600960209     C*
050700960209     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
050800960209     C     V2CGVR        IFEQ      '?'
050900960209     C                   CLEAR                   D7PQRS
051000960209     C                   MOVEL     'S'           DS7RIC
051100960209     C                   MOVEL     'V'           DS7GES
051200960209     C                   MOVEL     V2CGVA        DS7COD
051300960209     C                   MOVEL     D7PQRS        KPJBU
051400960209     C                   CALL      'TRTB73R'                            31
051500960209     C                   PARM                    KPJBA
051600960209     C                   SETON                                          31
051700960209     C                   MOVEL     *BLANKS       V2CGVR
051800960209     C                   ENDIF
051900960209     C*
052000960209     C   31              GOTO      FOR02
052100941201     C*
052200950116     C* COMANDI
052300950116     C                   EXSR      COMAND
052400950116     C*
052500950116     C* WFLG = '1' ---> E' STATO PREMUTO UN TASTO DI FUNZIONE
052600950116    1C     WFLG          IFEQ      '1'
052700950116     C* SE PREMUTO CMD10 OPPURE SE PREMUTO CMD3 DALLE TARIF.PARTICOLARI
052800950116     C*   VADO A FINE PGM ALTRIMENTI RIEMETTO IL FORMATO
052900950116     C     *INKJ         IFEQ      *ON
053000950117     C     *INKV         OREQ      *ON
053100950117     C     FLG4          ANDNE     ' '
053200950116     C                   GOTO      FINE
053300950116     C                   ENDIF
053400950116     C*
053500950116     C                   GOTO      FOR02
053600950116    1C                   ENDIF
053700900409     C*
053800000000     C     SUSF1         TAG
053900940811     C     V1CRDA        COMP      0                                  2020
054000940811     C   20V1CRA         COMP      *BLANKS                            2020
054100940811     C   20              MOVEL     V1CRDA        CONFRP
054200940811     C   20              MOVE      V1CRA         CONFRP
054300940622     C**  20     E' STATA BATTUTA UNA     CHIAVE DI RICERCA
054400940531     C** N20 NON E' STATA BATTUTA NESSUNA CHIAVE DI RICERCA
054500000000     C**-----------------------------------------------------------
054600940627     C* CHAIN PER CLIENTE
054700900925     C                   SETOFF                                       2221
054800940629     C*
054900940629     C     *IN20         IFEQ      *OFF
055000990622     C     KTAM          SETLL     TITAD000                               21
055100940629     C*
055200900925     C   21              MOVEL     'A'           TADATB
055300940629     C   21TADATB        DOWNE     ' '
055400990622     C     KTAM          READE     TITAD000                               32
055500900925     C  N32              END
055600920525     C* SE TUTTI ANNULLATI COME SE NON CI FOSSE
055700900925     C   21
055800900925     CAN 32              SETOFF                                       3221
055900920525     C*
056000990622     C   21KTAM          SETLL     TITAD000
056100900925     C   21              EXSR      PULSF
056200940629     C** ESEGUO CARICAMENTO SF
056300900925     C   21              EXSR      CARSFA
056400940629     C** ESEGUO SR AGGIORNAMENTO
056500900925     C   21              EXSR      AGGIOR
056600940629     C* SE NON CI SONO RIGHE DI DETTAGLIO ERRORE
056700940622     C  N21              MOVEL     MSG(1)        V1CMSG
056800940622     C  N21              SETON                                        28
056900950116     C  N21              GOTO      FOR02
057000900925     C                   END
057100000000     C**----------------------------------------------------------
057200000000     C     POI20         TAG
057300940903     C   14              SETON                                        2045
057400000000     C   20              DO
057500120713     c                   IF        %scan('?':V1Cra) > *zeros
057600120713     c                   exsr      Ric_CT
057700120713     c                   eval      V1Cra = TB09cod
057800120713     c                   ENDIF
057900000000     C                   Z-ADD     1             A                 3 0
058000940811     C     V1CRA         LOOKUP    CCT(A)                                 35
058100020215     C   35              MOVE      ORP(A)        VIDORP
058200940607     C  N35              MOVEL     *BLANKS       VIDORP
058300900424     C* SE CHIAVE RIC. NON ESISTE NE' PER 0 NE' PER > LEGGO DALL'INIZIO
058400990622     C     KTTAD         SETLL     TITAD000                           30
058500900424     C     SUTAD         TAG
058600990622     C  N30KTAM          READE     TITAD000                               30
058700940607     C  N30TADATB        CABEQ     'A'           SUTAD
058800900424     C*
058900990622     C  N30KTTAD         SETLL     TITAD000
059000990622     C   30KTAM          SETLL     TITAD000
059100940620     C** CARICO
059200900925     C                   EXSR      PULSF
059300900409     C                   EXSR      CARSFA
059400000000     C** ESEGUO CARICAMENTO SF
059500900409     C                   EXSR      AGGIOR
059600000000     C** ESEGUO SR AGGIORNAMENTO
059700900409     C                   END
059800940531     C*                                                                ---
059900940531     C** ELIMINAZIONE SU DISCO SUBFILE SF E S2
060000000000     C                   EXSR      ELISF
060100990915     C* SE DATO ENTER METTO 'C' NEL FLAG DI RITORNO
060200990915     C                   MOVEL     'C'           FLGRTN
060300940531     C*
060400000000     C     FINE          TAG
060500940615     C     KTAM          SETLL     TNTAM000                               01
060600990622     C     KTAM          SETLL     TITAD000                               01
060700990914     C*
060800990914     C* RITORNO PARAMETRI AL TNTA01R
060900990914     C                   MOVEL     PARAMX        KPJBU
061000940620     C*
061100000000     C                   RETURN
061200940531     C*
061300950116     C*--- ROUTINE COMANDI -------------------------------------------*
061400950116     C     COMAND        BEGSR
061500950116     C                   MOVEL     ' '           WFLG              1
061600101013
061700101013      /free
061800120321       //?F1 - dati manca tariffa
061900120321        IF  *inka;
062000120321          exsr gesw01;
062100120321          wflg = '1';
062200120321        ENDIF;
062300120321
062400101013       //?F4 = ISTAT
062500101013        IF  *inkd;
062600101013          reset TRTB12DS1;
062700101013          kpjbu = TRTB12DS1;
062800101013          trtb12r1 (kpjba);
062900101013          TRTB12DS1 = kpjbu;
063000101013          wflg = '1';
063100101013        ENDIF;
063200101013      /end-free
063300051202      *
063400051202      * F7 Visualizzazine storico variazioni
063500051202     c                   if        *inkg
063600051202     c                   clear                   tnta04ds
063700051202     c                   eval      i04ksc = v1cksc
063800051202     c                   eval      i04ctr = v1cctr
063900051202     c                   eval      i04prg = v1cprg
064000051202     c                   eval      i04rag = v1dksc
064100051202     c                   eval      i04dct = v1dctr
064200051202     c                   eval      i04dcv = v1cdcv
064300051202     c                   eval      i04div = v1cdiv
064400051202     c                   eval      i04ndf = v1cnet
064500051202      *
064600051202     c                   call      'TNTA04R'
064700051202     c                   parm                    tnta04ds
064800051202      *
064900051202     c                   movel     '1'           WFLG
065000051202     c                   leavesr
065100051202     c                   endif
065200950116     C*
065300950116     C** CMD14 - INTERROGAZIONE TARIFFE GIACENZE
065400950116    1C     *INKN         IFEQ      *ON
065500950116     C                   Z-ADD     V1CKSC        PA3CLI
065600950116     C                   Z-ADD     V1CCTR        PA3CTR
065700950116     C                   Z-ADD     V1CPRG        PA3PRG
065800950116     C                   MOVEL     V1DCTR        PA3DCT
065900950116     C                   MOVEL     V1CDCV        PA3DCV
066000990824     C                   MOVEL     V1CDIV        PA3DIV
066100090916
066200950116     C                   MOVEL     PARAM3        KPJBU
066300950116     C                   CALL      'TNTA15R'
066400950116     C                   PARM                    KPJBA
066500950116     C*
066600950116     C                   MOVEL     '1'           WFLG
066700950116     C                   GOTO      ENDCOM
066800950116    1C                   ENDIF
066900950116     C*
067000950116     C** CMD21 - INTERROGAZIONE TARIFFE PARTICOLARI
067100950116    1C     *INKV         IFEQ      *ON
067200950116     C                   Z-ADD     V1CKSC        CLI5
067300950116     C                   MOVEL     V1DKSC        RAG5
067400950116     C                   MOVEL     ' '           FLG4
067500970325     C                   MOVEL     V1CFIE        PARFIE
067600020201     C* PASSO IL TIPO SERVIZIO BOLLA
067700020201     C                   MOVE      V1CTSP        PA5TSP
067800020201     C* Flag Network
067900020201     C                   MOVEL     V1CNET        FLGNET
068000020201      *
068100950116     C                   MOVEL     PARAM5        KPJBU
068200120321     c                   IF        not *in97
068300950116     C                   CALL      'TNTA68R'
068400950116     C                   PARM                    KPJBA
068500120321     c                   ELSE
068600120321     C                   CALL      'TNTA68R'
068700120321     C                   PARM                    KPJBA
068800120321     c                   parm                    tnsb48ds
068900120321     c                   ENDIF
069000950116     C*
069100950116     C                   MOVEL     KPJBU         PARAM5
069200950116     C*
069300950116     C                   MOVEL     '1'           WFLG
069400950116     C                   GOTO      ENDCOM
069500950116    1C                   ENDIF
069600950116     C*
069700950116     C** CMD18 - GESTIONE NOTE
069800100407      /free
069900100806       //?Nuovo programma gestione note
070000100806        IF  *inks;
070100100407          clear trmk20ds;
070200110511          IMK20tla = 'L';
070300100407          IMK20flm = 'I';
070400100407          IMK20ksc = v1cksc;
070500100407          IMK20rsc = v1dksc;
070600100407
070700100407          trmk20r (kpjba : TRMK20DS);
070800100407          wflg = '1';
070900100407          leavesr;
071000100407        ENDIF;
071100100407
071200100407       //?F8 - Note tariffa
071300100806        IF  *inkh;
071400100407          clear TNTA28ds;
071500100407
071600100407          ITA28ela = 'I';
071700100407          ITA28tip = 'C';
071800100407          ITA28cod = V1cksc;
071900100407          ITA28rsc = V1dksc;
072000100407          kpjbu    = TNTA28DS;
072100100407
072200100407          tnta28r (kpjba);
072300100407          wflg = '1';
072400100407          leavesr;
072500100407        ENDIF;
072600100407      /end-free
072700090709
072800090709     C** F2=RUBRICA
072900090709    1c                   if        *inkb
073000090709     c                   clear                   tnta12ds
073100090709     c                   eval      ta12ric = 'V'
073200090709     c  n50              Eval      ta12apl = 'C'
073300090709     c  n50              Eval      ta12ksc = v1cksc
073400090709     c   50              Eval      ta12apl = 'V'
073500090709     c   50              Eval      ta12nrv = v1cnrv
073600090709     c                   Eval      ta12rag = v1dksc
073700090709     c                   Call      'TNTA12R'
073800090709     c                   Parm                    kpjba
073900090709     c                   Parm                    tnta12ds
074000090709     C*
074100090709     C                   MOVEL     '1'           WFLG
074200090709     C                   GOTO      ENDCOM
074300090709    1C                   ENDIF
074400950116     C*
074500950116     C** CMD10 - STAMPA TARIFFA
074600950116    1C     *INKJ         IFEQ      *ON
074700950116     C                   EXSR      STAMPA
074800950116     C*
074900950116     C                   MOVEL     '1'           WFLG
075000950116     C                   GOTO      ENDCOM
075100950116    1C                   ENDIF
075200120319      /free
075300120319       //?F24 Altri tasti
075400120319         IF  *inky;
075500120319           exsr Gesf24;
075600120319           wflg = '1';
075700120319           leavesr;
075800120319         ENDIF;
075900120319      /end-free
076000950116     C*
076100950116     C     ENDCOM        ENDSR
076200950116     C*
076300940615     C*--- MUOVO CAMPI DA TNTAM A VIDEO ------------------------------*
076400950116     C     RIED01        BEGSR
076500051202     C                   setoff                                       43
076600950105     C                   SETOFF                                       8816
076700950116     C                   SETOFF                                       9596
076800940616     C*
076900940616     C****  CODICE TARIFFA  ****
077000940616     C                   Z-ADD     1             A                 3 0
077100950105     C                   MOVEL     TAMCTR        W001              1
077200940616     C                   MOVEL     'TR'          COD
077300940616     C                   MOVEL     *BLANKS       KEY
077400950105     C                   MOVEL     W001          KEY
077500940616     C     KTAB          CHAIN     TABEL                              30
077600950105    1C     *IN30         IFEQ      *OFF
077700941123     C                   MOVEL     TBLUNI        DSTR
077800941123     C                   MOVEL     §TRDES        V1DCTR
077900941123     C     §TRTAP        IFEQ      'S'
078000941123     C                   SETON                                        88
078100941123     C                   ENDIF
078200950105    1C                   ENDIF
078300950105     C*
078400950105     C* 16 ON  - INDICA IL TIPO TARIFFA "4 - A VALORE"
078500950105    1C     W001          IFEQ      '4'
078600950105     C                   SETON                                        16
078700950105    1C                   ENDIF
078800940616     C*
078900940616     C****  DATA SCADENZA TARIFFA  ****
079000940629    1C     TAMDDT        IFGT      0
079100940629    2C     TAMDST        IFGT      0
079200940811     C                   Z-ADD     TAMDST        DATA
079300940811     C                   Z-ADD     MM            GGMM              4 0
079400940811     C                   MOVEL     GG            GGMM
079500940811     C                   Z-ADD     AA            V1CDST
079600940811     C                   MOVEL     GGMM          V1CDST
079700940629    2C                   ENDIF
079800940616     C*
079900940616     C****  DATA DECORRENZA TARIFFA  ****
080000940811     C                   Z-ADD     TAMDDT        DATA
080100940811     C                   Z-ADD     MM            GGMM
080200940811     C                   MOVEL     GG            GGMM
080300940811     C                   Z-ADD     AA            V1CDDT
080400940811     C                   MOVEL     GGMM          V1CDDT
080500940629    1C                   ENDIF
080600940616     C*
080700940616     C****  DATA ULTIMA VARIAZIONE  ****
080800940629    1C     TAMDUV        IFGT      0
080900940811     C                   Z-ADD     TAMDUV        DATA
081000940811     C                   Z-ADD     MM            GGMM              4 0
081100940811     C                   MOVEL     GG            GGMM
081200940811     C                   Z-ADD     AA            V1CDUV
081300940811     C                   MOVEL     GGMM          V1CDUV
081400940629    1C                   ENDIF
081500940607     C*
081600940617     C                   Z-ADD     TAMPRG        V1CPRG
081700940617     C                   MOVE      TAMCTR        V1CCTR
081800940617     C                   MOVE      TAMDCV        V1CDCV
081900101013     C                   Z-ADD     TAMrct        V1Crct
082000940629     C                   Z-ADD     TAMPRI        V1CPRI
082100150923     c                   IF        TAMtpr = 'NO'
082200150923     c                   eval      V1Ctpr = 'N'
082300150923     c                   ELSE
082400150923     c                   clear                   V1Ctpr
082500150923     c                   ENDIF
082600101013
082700101013      /free
082800101013       //?Cerco la data decorrenza scatto ISTAT
082900101013         clear V1Dist;
083000101013         IF  V1Crct > 0;
083100101013           SISsca = V1Crct;
083200101013           chain (SISsca) TISIS01L;
083300101013           IF  %Found(TISIS01L);
083400101013             wDate_ISO = %date(SISdata:*iso);
083500101013             wDate_Eur = wDate_ISO;
083600101013             V1Dist    = %dec(wDate_EUR);
083700101013           ENDIF;
083800101013         ENDIF;
083900101013      /end-free
084000101013
084100940617     C*
084200990827     C                   Z-ADD     TAMLRC        V1CMIS
084300990827     C                   Z-ADD     TAMLAS        V1CMAS
084400940629     C                   Z-ADD     TAMPAG        V1CPAG
084500041129     C                   Z-ADD     TAMPpa        V1CPpa
084600940622     C                   MOVEL     TAMTSP        V1CTSP
084700941109     C* DECODIFICA TIPO SPEDIZIONE
084800941109     C                   MOVEL     '5E'          COD
084900941109     C                   MOVEL     *BLANKS       KEY
085000941109     C                   MOVEL     V1CTSP        KEY
085100941109     C     KTAB          CHAIN     TABEL                              32
085200090608     C  N32              MOVEL     TBLUNI        ds5e
085300090608     C  N32              MOVEL     §5ed08        V1DTSP
085400941109     C*
085500940622     C                   MOVEL     TAMBAP        V1CBAP
085600940622     C                   MOVE      TAMFTP        V1CFTP
085700940622     C* CONTROLLO SE KG O MC PER CALCOLO VOLUME AUTOMATICO
085800940622     C                   Z-ADD     0             V1CKPM
085900940622     C                   Z-ADD     0             V1CMPC
086000940629    1C     TAMTIN        IFEQ      'K'
086100940622     C                   Z-ADD     TAMKPM        V1CKPM
086200940629    1C                   ENDIF
086300940629    1C     TAMTIN        IFEQ      'C'
086400940622     C                   MOVE      TAMKPM        V1CMPC
086500940629    1C                   ENDIF
086600940629     C                   Z-ADD     TAMATA        V1CATA
086700940629     C                   Z-ADD     TAMARL        V1CARL
086800940622     C                   Z-ADD     TAMARF        V1CARF
086900940629     C                   Z-ADD     TAMARO        V1CARO
087000940622     C*
087100940811     C                   MOVEL     TAMFTM        V2CFTM
087200940811     C                   MOVEL     TAMFTS        V2CFTS
087300940622     C                   MOVEL     TAMFIE        V1CFIE
087400940811     C                   MOVEL     TAMNAS        V2CNAS
087500940811     C                   MOVEL     TAMNOT        V2CNOT
087600940811     C                   MOVEL     TAMFIS        V2CFIS
087700940811     C                   MOVEL     TAMSIS        V2CSIS
087800940811     C                   MOVEL     TAMFVD        V2CFVD
087900970128     C                   MOVEL     TAMBAM        V2CBAM
088000980507     C* MODO APPLICAZIONE TARIFFA
088100980507     C                   MOVEL     TAMFLO        DSTA01
088200980507     C                   MOVEL     §TAF02        V1CMAT
088300020205      *
088400020205     C                   CLEAR                   V1CNET
088500020201     C     §TADPD        IFEQ      'S'
088600020201     C                   MOVE      'D'           V1CNET
088700020201     C                   ENDIF
088800020201     C     §TAFED        IFEQ      'S'
088900020201     C                   MOVE      'F'           V1CNET
089000020201     C                   ENDIF
089100020109     C*
089200020109     C                   MOVE      §TAPCA        V1CPCA
089300020109     C                   MOVE      §TATAP        V1CTAP
089400050914     c                   Movel     §taovb        v1covb
089500990622     C* DIVISA TARIFFA
089600990622     C                   MOVEL     §TADIV        V1CDIV
089700991119     C                   MOVE      V1CDIV        VICDIV
089800941018     C*
089900941019     C                   MOVEL     TAMGCA        V2CGCA
090000941018     C                   CLEAR                   V2CGCR
090100040601     c                   Movel     §tabva        v2cbva
090200060308     c                   eval      v2cfmp = tamfmp
090300120319
090400120319     c                   IF        TAMeds = 'N'
090500120319     c                   eval      v2Internet = 'NO'
090600120319     c                   ELSE
090700120319     c                   clear                   v2Internet
090800120319     c                   ENDIF
090900120319
091000941019     C* SOLO SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
091100941019     C     V2CGCA        COMP      *BLANKS                            1313
091200941019     C*
091300941019     C                   MOVEL     TAMGGA        V2CGGA
091400941019     C                   CLEAR                   V2CGGR
091500941019     C* SOLO SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
091600130124     C     V2CGGA        COMP      *BLANKS                            6363
091700941019     C*
091800941019     C                   MOVEL     TAMGMA        V2CGMA
091900941019     C                   CLEAR                   V2CGMR
092000941019     C* SOLO SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
092100960209     C     V2CGMA        COMP      *BLANKS                            1919
092200941019     C*
092300941019     C                   MOVEL     TAMGVA        V2CGVA
092400941019     C                   CLEAR                   V2CGVR
092500941019     C* SOLO SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
092600960209     C     V2CGVA        COMP      *BLANKS                            2424
092700941019     C*
092800950116     C*
092900950116     C* 95 ON  - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
093000950116     C                   SETOFF                                       95
093100990622     C     KTAM          SETLL     TITPT000
093200100407     C     KTAM          READE     TITPT000                               32
093300950116    3C     *IN32         DOWEQ     *OFF
093400950116     C     TPTATB        IFEQ      ' '
093500950116     C                   SETON                                        3295
093600950116     C                   ELSE
093700100407     C     KTAM          READE     TITPT000                               32
093800950116     C                   ENDIF
093900950116    3C                   ENDDO
094000950116     C*
094100950116     C* 96 ON  - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
094200100407     C     KTAM          CHAIN     TITGC000                           32
094300950116     C  N32TGCATB        IFEQ      ' '
094400950116     C                   SETON                                        96
094500950116     C                   ENDIF
094600950116     C*
094700900409     C                   ENDSR
094800940531     C*
094900940531     C*--- PULIZIA SUBFILE AGGIORNAMENTO -----------------------------*
095000900409     C     PULSF         BEGSR
095100000000     C                   SETON                                        94
095200940811     C                   WRITE     TA01C03
095300000000     C                   SETOFF                                       94
095400900925     C                   Z-ADD     0             SAVREC
095500000000     C                   ENDSR
095600940531     C*
095700940621     C*--- SR AGGIORNAMENTO ------------------------------------------*
095800920525     C     AGGIOR        BEGSR
095900120319
096000120319       //?Imposto i tasti funzione nella riga mobile
096100120319     c                   eval      wPrima = *off
096200120319     c                   eval      wSeconda = *off
096300120319     c                   eval      wTerza = *on
096400120319     c                   eval      wQuarta = *off
096500120319     c                   ExSr      Tastifun
096600120319
096700000000     C     SUAGG1        TAG
096800940811     C                   Z-ADD     0             V1CRDA
096900940811     C                   MOVEL     *BLANKS       V1CRA
097000940627     C*
097100000000     C     SUAGG         TAG
097200000000     C                   SETOFF                                       45
097300120319     C                   WRITE     TA01Z04
097400940811     C                   EXFMT     TA01C03
097500940622     C*
097600940622     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE
097700940622     C                   CLEAR                   VCCMSG
097800940622     C                   SETOFF                                       28
097900120327       //?F1 - dati manca tariffa
098000120327     c                   IF        *inka
098100120327     c                   exsr      gesw01
098200120327     c                   goto      suagg
098300120327     c                   ENDIF
098400120319       //?F3 - Fine
098500120319     c   kc              goto      fine
098600940622     C*
098700940531     C** CMD12 - FINE LAVORO NO AGG.
098800950117     C   KL              GOTO      FOR02
098900920525     C*
099000940531     C** CMD10 - STAMPA TARIFFA
099100940628     C     *INKJ         IFEQ      *ON
099200940628     C                   EXSR      STAMPA
099300940628     C                   GOTO      FINE
099400940628     C                   ENDIF
099500920525     C*
099600961212     C** CMD17-D E L T A
099700980902     C   KR              DO
099800031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
099900031124     c                   Clear                   Tibs02Ds
100000031124     c                   Clear                   dCat
100100031124     c                   Eval      T02Mod = 'C'
100200031124     c                   Eval      T02Sif = Knsif
100300031124     c                   Eval      T02Cod = 'CAT'
100400031124     c                   Movel(p)  '1'           T02Ke1
100500031124     c                   Call      'TIBS02R'
100600031124     c                   Parm                    Kpjba
100700031124     c                   Parm                    Tibs02Ds
100800031124     c                   If        T02Err = *Blanks
100900031124     c                   Movel     T02Uni        dCat
101000031124     c                   EndIf
101100031124    1 *
101200961212     C                   CLEAR                   DSTE00
101300961212     C                   MOVEL     'T05'         D00OP0
101400961212     C                   MOVEL     'F17'         D00OP1
101500961212     C                   MOVEL     'D'           D00TLA
101600980526     C                   MOVEL     'F'           D00DSF
101700980526     C                   MOVEL     'T'           D00CTO
101800961212     C                   MOVEL     V1CKSC        D00KSC
101900961212     C                   MOVEL     V1CCTR        D00CTR
102000961212     C                   MOVEL     V1CPRG        D00PRG
102100961212     C                   MOVEL     V1CFIE        D00FIE
102200961212     C                   MOVEL     V1CTSP        D00TSP
102300011003     C                   MOVEL     V1CDIV        D00DIV
102400020201     C                   MOVEL     V1CNET        D00DPD
102500011030     C                   MOVEL     *BLANKS       D00PRI
102600031124      * propongo data massima tra la data del giorno e quella recuperata
102700031124      * dalla tavella CAT
102800031124     c                   if        dateu8 > §CATdta
102900031124     C                   MOVEL     DATEU8        D00DTC
103000031124     C                   ELSE
103100031124     C                   MOVEL     §CATDTA       D00DTC
103200031124     C                   ENDIF
103300031124      *
103400970131     C                   CALL      'TNTE01C'
103500961212     C                   PARM                    KPJBA
103600961212     C                   PARM                    DSTE00
103700980902     C                   ENDDO
103800120319       //?F24 Altri tasti
103900120319     c                   IF        *inky
104000120319     c                   exsr      gesf24
104100120319     c                   goto      suagg
104200120319     c                   ENDIF
104300961212     C*
104400920525     C** ROLLUP  AVANZAMENTO NO AGG.
104500940607     C     *IN25         IFEQ      *ON
104600950327     C*
104700950327     C* IMPOSSIBILE CARICARE PIU' DI 9900 RECORD
104800950327     C     NRR           IFGE      9900
104900950327     C                   MOVEL     MSG(3)        VCCMSG
105000950327     C                   SETON                                        28
105100950327     C                   GOTO      SUAGG1
105200950327     C                   ENDIF
105300950327     C*
105400940531     C                   MOVEL     KE            KX
105500990622     C     KSCOR         SETLL     TITAD000                               22
105600990622     C   22KTAM          READE     TITAD000                               31
105700940531     C                   EXSR      CARSFA
105800940531     C                   GOTO      SUAGG1
105900940531     C                   ENDIF
106000940531     C*
106100940811     C                   MOVEL     V1CRDA        CONFR             5
106200940811     C                   MOVE      V1CRA         CONFR
106300940903     C     CONFR         COMP      CONFRP                             1414
106400940903     C   14              MOVEL     V1CRDA        CONFRP
106500940903     C   14              MOVE      V1CRA         CONFRP            5
106600000000     C** SALVO LA CHIAVE PER CONFRONTO SUCESSIVO
106700940903     C   14              GOTO      POI20
106800940903     C** 14 CHIAVE RICERCA
106900990915     C* SE DATO ENTER METTO 'C' NEL FLAG DI RITORNO
107000990915     C  N14              MOVEL     'C'           FLGRTN
107100940903     C  N14              GOTO      FINE
107200940903     C** 14 OFF - FINE TORNO A VIDEATA INIZIALE
107300000000     C** E' STATA DIGITATA UNA CHIAVE DI RICERCA  E
107400000000     C** RICERCO LA CHIAVE E RIEMETTO IL SUBFILE
107500000000     C** DA QUEL PUNTO IN POI
107600000000     C                   ENDSR
107700940531     C*
107800940531     C*--- CARICAMENTO SUBFILE IN AVANTI -----------------------------*
107900920525     C     CARSFA        BEGSR
108000950327     C                   Z-ADD     SAVREC        NRR               5 0
108100990622     C     KTAM          READE     TITAD000                               31
108200941126     C                   SETOFF                                       13
108300900925     C* FINE SCORRIMENTO
108400940607     C   31              DO
108500900925     C                   MOVEL     KE            KI
108600900925     C                   MOVEL     CTSE          CTSI
108700940622     C                   MOVEL     MSG(2)        VCCMSG
108800940622     C                   SETON                                        28
108900940622     C*
109000900925     C                   SETOFF                                       21
109100900925     C                   MOVEL     '000'         CONFRP
109200900925     C                   MOVE      '  '          CONFRP
109300900925     C                   GOTO      ENDCAR
109400900925     C                   END
109500940627     C*
109600900409     C** PULIZIA SUBFILE AGGIORNAMENTO
109700900925     C                   Z-ADD     0             B                 3 0
109800900925     C                   DO        *HIVAL
109900900925     C     TADATB        IFNE      'A'
110000951009     C*
110100951009     C*
110200900925     C                   ADD       1             B
110300900925     C                   ADD       1             NRR
110400931117     C** KIAVE INIZIALE
110500900925     C     B             IFEQ      1
110600900409     C                   MOVEL     KIAVE         KI
110700900409     C                   MOVEL     TADCTS        CTSI              2
110800900409     C                   END
110900931117     C*
111000940531     C** DESCRIZIONE CODICE TASSAZIONE
111100900409     C                   Z-ADD     1             A
111200940607     C     TADCTS        LOOKUP    CCT(A)                                 35
111300940607     C   35              MOVEL     DCT(A)        DESCTS
111400940607     C  N35              MOVEL     *BLANKS       DESCTS
111500941126     C*
111600941126     C* SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
111700941126     C*   LA LETTERA "M" SULLA RIGA DI DETTAGLIO
111800941126    3C     TADMIN        IFGT      0
111900941126     C     TADMAX        ORGT      0
112000941126     C                   SETON                                        13
112100941126    3C                   ENDIF
112200931117     C*
112300940811     C                   WRITE     TA01S03
112400941126     C                   SETOFF                                       13
112500941126     C*
112600941124     C     B             CABEQ     16            ENDAGA
112700900409     C                   END
112800900925     C*
112900951009     C     SOTTO         TAG
113000990622     C     KTAM          READE     TITAD000                               31
113100940607     C  N31              END
113200940622     C*
113300900409     C     ENDAGA        TAG
113400900925     C                   Z-ADD     NRR           REC
113500900925     C                   Z-ADD     NRR           SAVREC            9 0
113600900409     C                   MOVEL     KIAVE         KE
113700900409     C                   MOVEL     TADCTS        CTSE              2
113800900409     C** SE NON CI SONO PIU RECORD DA LEGGERE
113900900409     C** KE = KI
114000900925     C     ENDCAR        ENDSR
114100940531     C*
114200940531     C*--- ELIMINAZIONE SUBFILE SU DISCO -----------------------------*
114300900409     C     ELISF         BEGSR
114400000000     C                   SETON                                        93
114500940811     C                   WRITE     TA01C03
114600000000     C                   SETOFF                                       9493
114700000000     C                   ENDSR
114800940531     C*
114900940606     C*--- CARICAMENTO TABELLE ---------------------------------------*
115000920525     C     CARTAB        BEGSR
115100940531     C*
115200020206     C                   CLEAR                   DSCT
115300940606     C****  CODICI TASSAZIONE E DESCRIZIONI: CCT / DCT / ORP  ****
115400940531     C                   Z-ADD     1             K                 5 0
115500000000     C                   MOVEL     'CT'          COD
115600940531     C     KTAB2         SETLL     TABEL
115700940607     C     KTAB2         READE     TABEL                                  30
115800940531     C*
115900940629    1C     *IN30         DOWEQ     *OFF
116000940629    2C     TBLFLG        IFEQ      ' '
116100000000     C                   MOVEL     TBLKEY        CCT(K)
116200000000     C                   MOVEL     TBLUNI        DCT(K)
116300020206     C                   MOVEL     TBLUNI        DSCT
116400020206     C                   MOVE      §CTCOR        ORP(K)
116500940531     C                   ADD       1             K
116600940629    2C                   ENDIF
116700940531     C*
116800940607     C     KTAB2         READE     TABEL                                  30
116900940629    1C                   ENDDO
117000940531     C*
117100000000     C                   ENDSR
117200920525     C*
117300940531     C*--- STAMPA TARIFFA --------------------------------------------*
117400920525     C     STAMPA        BEGSR
117500940627     C*
117600970514     C*
117700940811     C                   Z-ADD     V1CKSC        PA2CLI
117800940310     C                   Z-ADD     PRG           PA2PRG
117900940310     C                   Z-ADD     VCTR          PA2CTR
118000940811     C                   MOVEL     V1DKSC        PA2RGS
118100920525     C                   MOVEL     PARAM2        KPJBU
118200940310     C*
118300941220     C                   CALL      'FNLV43R'
118400920525     C                   PARM                    KPJBA
118500920525     C                   GOTO      FINE
118600920525     C                   ENDSR
118700120319
118800120319      /free
118900120319       //--------------------------------------------------------------
119000120319       //?Imposto i tasti funzionali.
119100120319       //--------------------------------------------------------------
119200120319       BEGSR TastiFun;
119300120319
119400120319       //?Imposto la seconda riga variabile
119500120319         clear wpos;
119600120319         clear wposda;
119700120319         clear wpostf1;
119800120319         clear wpostf2;
119900120319         clear wpostf3;
120000120319         clear wrig;
120100120319         clear wtf1;
120200120319         clear wtf2;
120300120319         clear wtf3;
120400120319
120500120319         tf = 1;
120600120319         FOR tf by 1 to %elem(wTfv);
120700120321
120800120321         //?Abilito F1
120900120321           IF  not *in97 and
121000120321               %subst(wTfv(tf):1:3) = 'F1=';
121100120321             iter;
121200120321           ENDIF;
121300120319
121400120319         //?Abilito F2
121500120319           IF  (wQuarta or wTerza or not *in18) and
121600120319               %subst(wTfv(tf):1:3) = 'F2=';
121700120319             iter;
121800120319           ENDIF;
121900120319
122000120319         //?Abilito F8 - Note tariffa
122100130905           IF  (wTerza or wQuarta or not *in18) and
122200120319               %subst(wTfv(tf):1:3) = 'F8=';
122300120319             iter;
122400120319           ENDIF;
122500120319
122600120319         //?Abilito F10
122700120319           IF  (wQuarta or (wPrima and not *in10)) and
122800120319               %subst(wTfv(tf):1:3) = 'F10';
122900120319             iter;
123000120319           ENDIF;
123100120319
123200120319         //?Abilito F11
123300120319           IF  (wPrima or wSeconda) and
123400120319               %subst(wTfv(tf):1:3) = 'F11';
123500120319             iter;
123600120319           ENDIF;
123700120319
123800120319         //?Abilito F18
123900120319           IF  (wTerza or wQuarta or (wPrima and not *in10) or
124000120319               not *in18 or *in50) and
124100120319               %subst(wTfv(tf):1:3) = 'F18';
124200120319             iter;
124300120319           ENDIF;
124400120319
124500120319           IF  wTfv(tf) <> *blanks;
124600120319             wpos = %scan('#':wTfv(tf));
124700120319             wpostf1 += wpos;
124800120319
124900120319             IF  wpostf1 <= %len(VZFd01);
125000120319               wposda = wpostf1 - wpos + 1;
125100120319               %subst(wtf1:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
125200120319             ELSE;
125300120319               wpostf2 += wpos;
125400120319               IF  wpostf2 <= %len(VZFd01);
125500120319                 wposda = wpostf2 - wpos + 1;
125600120319                 %subst(wtf2:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
125700120319               ELSE;
125800120319                 wpostf3 += wpos;
125900120319                 IF  wpostf3 <= %len(VZFd01);
126000120319                   wposda = wpostf3 - wpos + 1;
126100120319                   %subst(wtf3:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
126200120319                 ENDIF;
126300120319               ENDIF;
126400120319             ENDIF;
126500120319
126600120319           ENDIF;
126700120319         ENDFOR;
126800120319
126900120319       //?Imposto F24 giusto
127000120319         SELECT;
127100120319           WHEN  wtf3 <> *blanks;
127200120319             VZFd02 = cf2413;
127300120319             wf24 = 3;
127400120319           WHEN  wtf2 <> *blanks;
127500120319             VZFd02 = cf2412;
127600120319             wf24 = 2;
127700120319           OTHER;
127800120319             clear VZFd02;
127900120319             wf24 = 1;
128000120319         ENDSL;
128100120319
128200120319         VZFd01 = wtf1;
128300120319         wrig = 1;
128400120319
128500120319       ENDSR;
128600120319
128700120319       //--------------------------------------------------------------
128800120319       //?Gestione tasto funzionale F24
128900120319       //?F24=Altri Tasti
129000120319       //--------------------------------------------------------------
129100120319       BEGSR gesf24;
129200120319
129300120319         SELECT;
129400120319           WHEN  (wf24 = 3 and wrig = 3) or
129500120319                 (wf24 = 2 and wrig = 2);
129600120319             VZFd01 = wtf1;
129700120319             wrig = 1;
129800120319             IF  wf24 = 2;
129900120319               VZFd02 = cf2412;
130000120319             ELSE;
130100120319               VZFd02 = cf2413;
130200120319             ENDIF;
130300120319
130400120319           WHEN  (wf24 = 3 and wrig = 1) or
130500120319                 (wf24 = 2 and wrig = 1);
130600120319             VZFd01 = wtf2;
130700120319             wrig = 2;
130800120319             IF  wf24 = 2;
130900120319               VZFd02 = cf2422;
131000120319             ELSE;
131100120319               VZFd02 = cf2423;
131200120319             ENDIF;
131300120319
131400120319           WHEN  wf24 = 3 and wrig = 2;
131500120319             VZFd01 = wtf3;
131600120319             wrig = 3;
131700120319             VZFd02 = cf2433;
131800120319
131900120319         ENDSL;
132000120319
132100120319       ENDSR;
132200120321
132300120321       //--------------------------------------------------------------
132400120321       //?Visualizza Dati manca tariffa
132500120321       //--------------------------------------------------------------
132600120321       BEGSR GesW01;
132700120321
132800120321         wFine = *off;
132900120321         W01dsp = ISB48dsp;
133000120321         W01tbl = ISB48tbl;
133100120321         W01lnp = ISB48lnp;
133200120321         W01lna = ISB48lna;
133300120321         W01pro = ISB48pro;
133400120321         W01cts = ISB48cts;
133500120321         W01ctr = ISB48ctr;
133600120321         W01ncl = ISB48ncl;
133700120321         W01pkf = ISB48pkf;
133800120321         W01vlf = ISB48vlf;
133900120321         W01err = ISB48err;
134000120321
134100120321         DOW wFine = *off;
134200120321           exfmt  TA01W01;
134300120321           //?- F12 = Ritorno
134400120321           IF  *inkl;
134500120321             wFine = *on;
134600120321           ENDIF;
134700120321         ENDDO;
134800120321
134900120321       ENDSR;
135000120713
135100120713       //--------------------------------------------------------------
135200120713       //?Ricerca Codice Tassazione
135300120713       //--------------------------------------------------------------
135400120713       BEGSR Ric_CT;
135500120713
135600120713         clear TRTB09DS;
135700120713         TB09fun = '1';
135800120713         TB09ord = '1';
135900120713         IF  V1Cnet = 'D';
136000120713           TB09est = 'E';
136100120713         ELSE;
136200120713           TB09est = V1Cfie;
136300120713         ENDIF;
136400120713         TB09uta = 'S';
136500120713         IF  V1Cnet = 'F';
136600120713           TB09utf = 'S';
136700120713         ELSE;
136800120713           TB09utf = 'N';
136900120713         ENDIF;
137000120713         kpjbu = TRTB09DS;
137100120713         trtb09r (kpjba);
137200120713         TRTB09DS = kpjbu;
137300120713
137400120713       ENDSR;
137500120713
137600120319      /end-free
137700940629     C*
137800000000     C*---------------------------------------------------------------*
137900950327**   TES
138000900409 ** INTERROGAZ. TARIFFE  CLIENTI **
138100940607**   MSG
138200940607Non esistono righe di dettaglio per questa tariffa
138300940622Fine Scorrimento
138400950327Impossibile visualizzare ulteriori righe di dettaglio: utilizzare la ricerca
138500051202Selezione NON concessa in visualizzazione
138600120319
138700120319** - wTfv (in ordine di visualizzazione) ------------------------------------*
138800120321F1=Manca Tariffa  #
138900120319F3=Fine  #
139000120319F2=Rubrica  #
139100120319F8=Note Tar.  #
139200120319F10=Stampa  #
139300120319F11=Altri Dati  #
139400120319F18=Note  #
139500120319F12=Ritorno   #
