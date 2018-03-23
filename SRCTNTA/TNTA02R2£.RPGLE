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
014200120319     d wInternet       s               n   inz(*off)
014300120319
014400120319       // -?Campi x gestione riga mobile tasti funzione
014500120319     d wTfv            s             61    dim(20) ctdata perrcd(1)
014600120319     d wf24            s              2  0
014700120319     d wpos            s              2  0
014800120319     d wposda          s              2  0
014900120319     d wpostf1         s              5  0
015000120319     d wpostf2         s              5  0
015100120319     d wpostf3         s              5  0
015200120319     d wrig            s              2  0
015300120319     d wtf1            s             61
015400120319     d wtf2            s             61
015500120319     d wtf3            s             61
015600120319     d tf              s              3  0
015700120319
015800961212     D*
015900961212     D* DS PER TNTE01R - TARIFFE DA C/ECONOMICO
016000961212     D DSTE00        E DS                  EXTNAME(TNTE00DS)
016100940811     D*
016200940811     D                 DS
016300940811     D  AA                     1      4  0
016400940811     D  MM                     5      6  0
016500940811     D  GG                     7      8  0
016600940811     D  DATA                   1      8  0
016700970130     D                 DS
016800970130     D  ROVGG                  1      2  0
016900970130     D  ROVMM                  3      4  0
017000970130     D  ROVAA                  5      8  0
017100970130     D  ROVDAT                 1      8  0
017200970130     D*
017300020206     D DSCT          E DS
017400960209     D*
017500960209     D* DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT/GEST PARTIC.
017600960209     D D7PQRS        E DS                  EXTNAME(DS7PQRS)
017700941123     D DSTR          E DS
017800990622     D DSCV          E DS
017900101013
018000101013     d TRTB12ds1     e ds                  inz
018100101013     d   D12opz      e                     inz('5')
018200031119      *
018300000000     D KPJBA         E DS
018400900424     D CNCR80        E DS
018500900424     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018600980504     D DSTA01        E DS
018700090608     d ds5e          e ds
018800031124
018900031124     d Tibs02Ds      e ds
019000031124      * Ds data massima elaborazione CAT da passare al TNTE01R
019100031124     d dcat          e ds
019200030718      * ds per visualizzazione variazioni tariffe
019300030718     D TNTA04DS      E DS
019400090709
019500090709     d tnta12ds      e ds
019600100407     d TNTA28DS      e ds
019700100407     d TRMK20DS      e ds
019800101013
019900101013     d wDate_EUR       s               d   datfmt(*eur)
020000101013     d wDate_ISO       s               d   datfmt(*iso)
020100120321
020200120321       // -?ds dati passati da TNSB48R - Manca Tariffa
020300120321     d TNSB48ds      e ds
020400120321     d Parm48          s                   like(tnsb48ds)
020500120713
020600120713       // -?passaggio dati a TRTB09R - Ricerca codice tassazione
020700120713     d TRTB09DS      e ds
020800120319
020900120319       // -?ds x tabella VPO decorrenza visualizza tariffa internet
021000120319     d dvpodeco      e ds
021100120319
021200120319      //---------------------------------------------------------------
021300120319      //?Costanti
021400120319      //---------------------------------------------------------------
021500120319     d cf2413          c                   const('F24=AlTasti(1/3)')
021600120319     d cf2423          c                   const('F24=AlTasti(2/3)')
021700120319     d cf2433          c                   const('F24=AlTasti(3/3)')
021800120319     d cf2412          c                   const('F24=AlTasti(1/2)')
021900120319     d cf2422          c                   const('F24=AlTasti(2/2)')
022000120319
022100120319       // -?campo per dspatr NO DISPLAY + protetto
022200120319     d nodisp          C                   CONST(X'A7')
022300120319
022400120319       // -?campo per dspatr protetto
022500120319     d protet          C                   CONST(X'A6')
022600101013
022700101013      //---------------------------------------------------------------
022800101013      //?Definizione procedure esterne.
022900101013      //---------------------------------------------------------------
023000101013      // - Reperimento Scatti ISTAT
023100101013     d trtb12r1        pr                  extpgm('TRTB12R1')
023200101013     d  kpjba                              likeds(KPJBA)
023300120713
023400120713      // - Interrogazione codici tassazione
023500120713     d trtb09r         pr                  extpgm('TRTB09R')
023600120713     d  kpjba                              likeds(KPJBA)
023700100407
023800100407      //---------------------------------------------------------------
023900100407      //?prototipi
024000100407      //---------------------------------------------------------------
024100100407      /copy gaitrasrc/srcprotopr,tnta28r
024200100407      /copy gaitrasrc/srcprotopr,trmk20r
024300100407
024400030718      *
024500940531     C*****************************************************************
024600940531     C* RIEPILOGO INDICATORI
024700940531     C*****************************************************************
024800941019     C* 13    - ABILITO L'INTERROGAZIONE PARTICOLARITA' C/ASSEGNO
024900941126     C*         USATO ANCHE NEL DETTAGLIO PER VISUALIZZARE LA LETTERA
025000941126     C*         "M" CHE INDICA LA PRESENZA DEL MINIMO O DEL MASSIMO
025100940531     C* 15    - INTERROGAZIONE
025200950105     C* 16    - INDICA IL TIPO TARIFFA "4 - A VALORE"
025300130124     C* 63    - ABILITO L'INTERROGAZIONE PARTICOLARITA' GIACENZA
025400941019     C* 19    - ABILITO L'INTERROGAZIONE PARTICOALRITA' CONSEGNA
025500941019     C* 24    - ABILITO L'INTERROGAZIONE PARTICOLARITA' VARIE
025600940607     C* 25    - ROLLUP
025700940622     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
025800940607     C* 30    - DI COMODO
025900940607     C* 35    - USATO PER LOKUP
026000940903     C* 18    - NON VISUALIZZO LE NOTE IN SEDE
026100941123     C* 88    - SI TRATTA DI TARIFFA A PERCENTUALE
026200950116     C* 95    - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
026300950116     C* 96    - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
026400120321      * 97    - Pgm richiamato x manca tariffa TNSB48
026500940531     C*****************************************************************
026600000000     C     *ENTRY        PLIST
026700000000     C                   PARM                    KPJBA
026800120321     c                   parm                    Parm48
026900120321
027000120321      /free
027100120321       //?controllo se passati tutti i parametri
027200120321         IF  %parms = 1;
027300120321           clear tnsb48ds;
027400120321           *in97 = *off;
027500120321         ELSE;
027600120321           clear tnsb48ds;
027700120321           tnsb48ds = parm48;
027800120321           *in97 = *on;
027900120321         ENDIF;
028000120321
028100120321         clear OSB48err;
028200120321         clear OSB48msg;
028300120321      /end-free
028400100505
028500000000     C                   Z-ADD     1             CODUT
028600900424     C                   CALL      'X§PARUT'
028700900424     C                   PARM                    UT§DSE
028800940811     C                   MOVEL     RAGUT         V1CRSU
028900900424     C                   MOVEL     REC80         CNCR80
029000100505
029100970130     C* LA DATA DEL GIORNO LA PRENDO DA TIME
029200970130     C                   TIME                    W0140            14 0
029300970130     C                   MOVE      W0140         UDATE8            8 0
029400970130     C                   Z-ADD     UDATE8        ROVDAT
029500970130     C                   Z-ADD     ROVGG         MMGG              4 0
029600970130     C                   MOVEL     ROVMM         MMGG
029700970130     C                   Z-ADD     MMGG          DATEU8            8 0
029800970130     C                   MOVEL     ROVAA         DATEU8
029900970404     C                   MOVE      KNSIF         WSIF              4
030000051202      * Nome pgm.
030100051202     c                   movel     SDSpgm        V1Cpgm
030200970130     C*
030300000000     C*---------------------------------------------------------------*
030400000000     C                   SETOFF                                       212223
030500000000     C                   SETOFF                                       242526
030600000000     C                   SETOFF                                       27
030700940622     C*
030800990622     C* ACCESSO TNTAM01L/TITAD04L
030900000000     C     KTAM          KLIST
031000940811     C                   KFLD                    V1CKSC
031100900409     C                   KFLD                    VCTR
031200900409     C                   KFLD                    PRG
031300990622     C* ACCESSO TITAD04L
031400000000     C     KSCOR         KLIST
031500940811     C                   KFLD                    V1CKSC
031600900409     C                   KFLD                    TAMCTR
031700000000     C                   KFLD                    PRG
031800000000     C                   KFLD                    LNPX
031900000000     C                   KFLD                    ORPX
032000990622     C                   KFLD                    NAZX
032100000000     C                   KFLD                    PARX
032200000000     C                   KFLD                    SCGX
032300000000     C     KTTAD         KLIST
032400940811     C                   KFLD                    V1CKSC
032500900409     C                   KFLD                    TAMCTR
032600000000     C                   KFLD                    PRG
032700940811     C                   KFLD                    V1CRDA
032800990622     C                   KFLD                    VIDORP            5
032900940531     C* ACCESSO TABEL00F
033000000000     C     KTAB          KLIST
033100000000     C                   KFLD                    CODUT
033200940531     C                   KFLD                    COD
033300940531     C                   KFLD                    KEY
033400940531     C     KTAB2         KLIST
033500920525     C                   KFLD                    CODUT
033600920525     C                   KFLD                    COD
033700940531     C*
033800940622     C* DEFINIZIONE CAMPI
033900940531     C     *LIKE         DEFINE    TBLCOD        COD
034000940531     C     *LIKE         DEFINE    TBLKEY        KEY
034100920525     C*
034200940531     C*---------------------------------------------------------------*
034300940622     C* 15 ON  - I N T E R R O G A Z I O N E
034400940622     C                   SETON                                        15
034500940531     C*
034600000000     C                   MOVEL     *ZEROS        KI
034700000000     C                   MOVEL     *ZEROS        KE
034800000000     C                   MOVEL     *ZEROS        KX
034900000000     C                   MOVEL     *ZEROS        KIAVE
035000000000     C                   MOVEL     *BLANKS       TADORP
035100940628     C                   MOVEL     *BLANKS       TADCAP
035200000000     C                   MOVEL     *BLANKS       ORPI
035300000000     C                   MOVEL     *BLANKS       PARI
035400000000     C                   MOVEL     *BLANKS       PARE
035500000000     C                   MOVEL     *BLANKS       ORPE
035600000000     C                   MOVEL     *BLANKS       ORPX
035700000000     C                   MOVEL     *BLANKS       PARX
035800990622     C                   MOVEL     *BLANKS       NAZI
035900990622     C                   MOVEL     *BLANKS       NAZE
036000990622     C                   MOVEL     *BLANKS       NAZX
036100940620     C*
036200940531     C* TESTATA PGM
036300940628     C                   MOVE      TES(1)        V1CTES
036400920708     C*
036500940903     C* 18 ON  - SONO IN FILIALE ---> IN SEDE NON VISUALIZZO LE NOTE
036600940903    1C  N18SIMFEL        IFNE      0
036700940903     C                   SETON                                        18
036800940629    1C                   ENDIF
036900940607     C*
037000940811     C                   Z-ADD     0             V1CKSC
037100000000     C                   Z-ADD     0             PRG
037200000000     C                   Z-ADD     0             CCC
037300940811     C                   Z-ADD     0             V1CRDA
037400940811     C                   MOVEL     *BLANKS       V1CRA
037500940811     C                   MOVEL     V1CRDA        CONFRP
037600940811     C                   MOVEL     V1CRDA        CONFR
037700940811     C                   MOVE      V1CRA         CONFRP
037800940811     C                   MOVE      V1CRA         CONFR
037900940811     C                   MOVEL     *BLANKS       V1DKSC
038000000000     C                   MOVEL     KPJBU         PARAMX
038100990914     C* IMPOSTO IL CODICE DI RITORNO AL PROGRAMMA CHIAMANTE
038200990914     C*
038300990914     C                   MOVEL     'R'           FLGRTN
038400990914     C****
038500951009     C                   MOVEL     V1CKSC        W0030             3 0
038600120319
038700120319      *?controllo se visualizzare o no la tariffa in internet
038800120319     c                   clear                   tibs02ds
038900120319     c                   clear                   dvpodeco
039000120319     c                   eval      t02mod = 'C'
039100120319     c                   eval      t02sif = knsif
039200120319     c                   eval      t02cod = 'VPO'
039300120319     c                   eval      t02ke1 = 'DECO'
039400120319     c                   call      'TIBS02R'
039500120319     c                   parm                    kpjba
039600120319     c                   parm                    tibs02ds
039700120319    2c                   if        t02err = *blanks
039800120319     c                   eval      dvpodeco  = t02uni
039900120319    2c                   endif
040000120319     c                   eval      wInternet = (dateu8 >= §vpotarint)
040100120319     c                   IF        not wInternet
040200120319     c                   movel     nodisp        a2Internet
040300120320     c                   movel     nodisp        a2desInter
040400120319     c                   ELSE
040500120319     c                   movel     protet        a2Internet
040600120320     c                   clear                   a2desInter
040700120319     c                   ENDIF
040800100806
040900940531     C** CARICAMENTO TABELLE
041000920525     C                   EXSR      CARTAB
041100940607     C*
041200000000     C** CAPO CONTO CLIENTI
041300940903     C                   SETOFF                                       011014
041400940615     C     KTAM          CHAIN     TNTAM000                           01
041500000000     C  N01TAMATB        COMP      'A'                                    01
041600900409     C   01              GOTO      FINE
041700000000     C** 10  AGGIORNAMENTO
041800920525     C                   SETON                                        10
041900940811     C                   Z-ADD     V1CKSC        CLI               7 0
042000950116     C* CARICO DATI VIDEATA
042100950116     C                   EXSR      RIED01
042200120319       //?Imposto i tasti funzione nella riga mobile
042300120319     c                   eval      wPrima = *on
042400120319     c                   eval      wSeconda = *off
042500120319     c                   eval      wTerza = *off
042600120319     c                   eval      wQuarta = *off
042700120319     c                   ExSr      Tastifun
042800940629     C*
042900940603     C** EMISSIONE VIDEO 2
043000950116     C     FOR01         TAG
043100120319     C                   write     TA01Z01
043200940628     C                   EXFMT     TA01D01
043300051202     c                   setoff                                       28  90
043400051202     c                   clear                   V1Cmsg
043500120319       //?F3 - Fine
043600120319     c   kc              goto      fine
043700940622     C*
043800940622     C* CMD12 - FINE LAVORO
043900940531     C   KL              GOTO      FINE
044000030718     C*
044100950116     C* COMANDI
044200950116     C                   EXSR      COMAND
044300950116     C*
044400950116     C* WFLG = '1' ---> E' STATO PREMUTO UN TASTO DI FUNZIONE
044500950116    1C     WFLG          IFEQ      '1'
044600950116     C* SE PREMUTO CMD10 OPPURE SE PREMUTO CMD3 DALLE TARIF.PARTICOLARI
044700950116     C*   VADO A FINE PGM ALTRIMENTI RIEMETTO IL FORMATO
044800950116     C     *INKJ         IFEQ      *ON
044900950117     C     *INKV         OREQ      *ON
045000950117     C     FLG4          ANDNE     ' '
045100950116     C                   GOTO      FINE
045200950116     C                   ENDIF
045300950116     C*
045400950116     C                   GOTO      FOR01
045500950116    1C                   ENDIF
045600120319
045700120319       //?Imposto i tasti funzione nella riga mobile
045800120319     c                   eval      wPrima = *off
045900120319     c                   eval      wSeconda = *on
046000120319     c                   eval      wTerza = *off
046100120319     c                   eval      wQuarta = *off
046200120319     c                   ExSr      Tastifun
046300940629     C*
046400940811     C****
046500940811     C* EMISSIONE VIDEO 3
046600950116     C     FOR02         TAG
046700940811     C                   Z-ADD     0             V1CRDA
046800940811     C                   MOVEL     *BLANKS       V1CRA
046900940811     C*
047000120319     C                   write     TA01Z02
047100940811     C                   EXFMT     TA01D02
047200941201     C*
047300941201     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE
047400941201     C                   CLEAR                   V1CMSG
047500941201     C                   SETOFF                                       28
047600120319       //?F3 - Fine
047700120319     c   kc              goto      fine
047800960209     C** CMD12 - FINE LAVORO
047900960209     C   KL              GOTO      FOR01
048000960209     C**
048100960209     C                   SETOFF                                       31
048200960209     C* VEDO SE RICHIESTO IL '?' SULLE PARTICOLARITA
048300960209     C     V2CGCR        IFEQ      '?'
048400960209     C                   CLEAR                   D7PQRS
048500960209     C                   MOVEL     'S'           DS7RIC
048600960209     C                   MOVEL     'V'           DS7GES
048700960209     C                   MOVEL     V2CGCA        DS7COD
048800960209     C                   MOVEL     D7PQRS        KPJBU
048900960209     C                   CALL      'TRTB72R'                            31
049000960209     C                   PARM                    KPJBA
049100960209     C                   SETON                                          31
049200960209     C                   MOVEL     *BLANKS       V2CGCR
049300960209     C                   ENDIF
049400960209     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
049500960209     C     V2CGGR        IFEQ      '?'
049600960209     C                   CLEAR                   D7PQRS
049700960209     C                   MOVEL     'S'           DS7RIC
049800960209     C                   MOVEL     'V'           DS7GES
049900960209     C                   MOVEL     V2CGGA        DS7COD
050000960209     C                   MOVEL     D7PQRS        KPJBU
050100960209     C                   CALL      'TRTB69R'                            31
050200960209     C                   PARM                    KPJBA
050300960209     C                   SETON                                          31
050400960209     C                   MOVEL     *BLANKS       V2CGGR
050500960209     C                   ENDIF
050600960209     C*
050700960209     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
050800960209     C     V2CGMR        IFEQ      '?'
050900960209     C                   CLEAR                   D7PQRS
051000960209     C                   MOVEL     'S'           DS7RIC
051100960209     C                   MOVEL     'V'           DS7GES
051200960209     C                   MOVEL     V2CGMA        DS7COD
051300960209     C                   MOVEL     D7PQRS        KPJBU
051400960209     C                   CALL      'TRTB70R'                            31
051500960209     C                   PARM                    KPJBA
051600960209     C                   SETON                                          31
051700960209     C                   MOVEL     *BLANKS       V2CGMR
051800960209     C                   ENDIF
051900960209     C*
052000960209     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
052100960209     C     V2CGVR        IFEQ      '?'
052200960209     C                   CLEAR                   D7PQRS
052300960209     C                   MOVEL     'S'           DS7RIC
052400960209     C                   MOVEL     'V'           DS7GES
052500960209     C                   MOVEL     V2CGVA        DS7COD
052600960209     C                   MOVEL     D7PQRS        KPJBU
052700960209     C                   CALL      'TRTB73R'                            31
052800960209     C                   PARM                    KPJBA
052900960209     C                   SETON                                          31
053000960209     C                   MOVEL     *BLANKS       V2CGVR
053100960209     C                   ENDIF
053200960209     C*
053300960209     C   31              GOTO      FOR02
053400941201     C*
053500950116     C* COMANDI
053600950116     C                   EXSR      COMAND
053700950116     C*
053800950116     C* WFLG = '1' ---> E' STATO PREMUTO UN TASTO DI FUNZIONE
053900950116    1C     WFLG          IFEQ      '1'
054000950116     C* SE PREMUTO CMD10 OPPURE SE PREMUTO CMD3 DALLE TARIF.PARTICOLARI
054100950116     C*   VADO A FINE PGM ALTRIMENTI RIEMETTO IL FORMATO
054200950116     C     *INKJ         IFEQ      *ON
054300950117     C     *INKV         OREQ      *ON
054400950117     C     FLG4          ANDNE     ' '
054500950116     C                   GOTO      FINE
054600950116     C                   ENDIF
054700950116     C*
054800950116     C                   GOTO      FOR02
054900950116    1C                   ENDIF
055000900409     C*
055100000000     C     SUSF1         TAG
055200940811     C     V1CRDA        COMP      0                                  2020
055300940811     C   20V1CRA         COMP      *BLANKS                            2020
055400940811     C   20              MOVEL     V1CRDA        CONFRP
055500940811     C   20              MOVE      V1CRA         CONFRP
055600940622     C**  20     E' STATA BATTUTA UNA     CHIAVE DI RICERCA
055700940531     C** N20 NON E' STATA BATTUTA NESSUNA CHIAVE DI RICERCA
055800000000     C**-----------------------------------------------------------
055900940627     C* CHAIN PER CLIENTE
056000900925     C                   SETOFF                                       2221
056100940629     C*
056200940629     C     *IN20         IFEQ      *OFF
056300990622     C     KTAM          SETLL     TITAD000                               21
056400940629     C*
056500900925     C   21              MOVEL     'A'           TADATB
056600940629     C   21TADATB        DOWNE     ' '
056700990622     C     KTAM          READE     TITAD000                               32
056800900925     C  N32              END
056900920525     C* SE TUTTI ANNULLATI COME SE NON CI FOSSE
057000900925     C   21
057100900925     CAN 32              SETOFF                                       3221
057200920525     C*
057300990622     C   21KTAM          SETLL     TITAD000
057400900925     C   21              EXSR      PULSF
057500940629     C** ESEGUO CARICAMENTO SF
057600900925     C   21              EXSR      CARSFA
057700940629     C** ESEGUO SR AGGIORNAMENTO
057800900925     C   21              EXSR      AGGIOR
057900940629     C* SE NON CI SONO RIGHE DI DETTAGLIO ERRORE
058000940622     C  N21              MOVEL     MSG(1)        V1CMSG
058100940622     C  N21              SETON                                        28
058200950116     C  N21              GOTO      FOR02
058300900925     C                   END
058400000000     C**----------------------------------------------------------
058500000000     C     POI20         TAG
058600940903     C   14              SETON                                        2045
058700000000     C   20              DO
058800120713     c                   IF        %scan('?':V1Cra) > *zeros
058900120713     c                   exsr      Ric_CT
059000120713     c                   eval      V1Cra = TB09cod
059100120713     c                   ENDIF
059200000000     C                   Z-ADD     1             A                 3 0
059300940811     C     V1CRA         LOOKUP    CCT(A)                                 35
059400020215     C   35              MOVE      ORP(A)        VIDORP
059500940607     C  N35              MOVEL     *BLANKS       VIDORP
059600900424     C* SE CHIAVE RIC. NON ESISTE NE' PER 0 NE' PER > LEGGO DALL'INIZIO
059700990622     C     KTTAD         SETLL     TITAD000                           30
059800900424     C     SUTAD         TAG
059900990622     C  N30KTAM          READE     TITAD000                               30
060000940607     C  N30TADATB        CABEQ     'A'           SUTAD
060100900424     C*
060200990622     C  N30KTTAD         SETLL     TITAD000
060300990622     C   30KTAM          SETLL     TITAD000
060400940620     C** CARICO
060500900925     C                   EXSR      PULSF
060600900409     C                   EXSR      CARSFA
060700000000     C** ESEGUO CARICAMENTO SF
060800900409     C                   EXSR      AGGIOR
060900000000     C** ESEGUO SR AGGIORNAMENTO
061000900409     C                   END
061100940531     C*                                                                ---
061200940531     C** ELIMINAZIONE SU DISCO SUBFILE SF E S2
061300000000     C                   EXSR      ELISF
061400990915     C* SE DATO ENTER METTO 'C' NEL FLAG DI RITORNO
061500990915     C                   MOVEL     'C'           FLGRTN
061600940531     C*
061700000000     C     FINE          TAG
061800940615     C     KTAM          SETLL     TNTAM000                               01
061900990622     C     KTAM          SETLL     TITAD000                               01
062000990914     C*
062100990914     C* RITORNO PARAMETRI AL TNTA01R
062200990914     C                   MOVEL     PARAMX        KPJBU
062300940620     C*
062400000000     C                   RETURN
062500940531     C*
062600950116     C*--- ROUTINE COMANDI -------------------------------------------*
062700950116     C     COMAND        BEGSR
062800950116     C                   MOVEL     ' '           WFLG              1
062900101013
063000101013      /free
063100120321       //?F1 - dati manca tariffa
063200120321        IF  *inka;
063300120321          exsr gesw01;
063400120321          wflg = '1';
063500120321        ENDIF;
063600120321
063700101013       //?F4 = ISTAT
063800101013        IF  *inkd;
063900101013          reset TRTB12DS1;
064000101013          kpjbu = TRTB12DS1;
064100101013          trtb12r1 (kpjba);
064200101013          TRTB12DS1 = kpjbu;
064300101013          wflg = '1';
064400101013        ENDIF;
064500101013      /end-free
064600051202      *
064700051202      * F7 Visualizzazine storico variazioni
064800051202     c                   if        *inkg
064900051202     c                   clear                   tnta04ds
065000051202     c                   eval      i04ksc = v1cksc
065100051202     c                   eval      i04ctr = v1cctr
065200051202     c                   eval      i04prg = v1cprg
065300051202     c                   eval      i04rag = v1dksc
065400051202     c                   eval      i04dct = v1dctr
065500051202     c                   eval      i04dcv = v1cdcv
065600051202     c                   eval      i04div = v1cdiv
065700051202     c                   eval      i04ndf = v1cnet
065800051202      *
065900051202     c                   call      'TNTA04R'
066000051202     c                   parm                    tnta04ds
066100051202      *
066200051202     c                   movel     '1'           WFLG
066300051202     c                   leavesr
066400051202     c                   endif
066500950116     C*
066600950116     C** CMD14 - INTERROGAZIONE TARIFFE GIACENZE
066700950116    1C     *INKN         IFEQ      *ON
066800950116     C                   Z-ADD     V1CKSC        PA3CLI
066900950116     C                   Z-ADD     V1CCTR        PA3CTR
067000950116     C                   Z-ADD     V1CPRG        PA3PRG
067100950116     C                   MOVEL     V1DCTR        PA3DCT
067200950116     C                   MOVEL     V1CDCV        PA3DCV
067300990824     C                   MOVEL     V1CDIV        PA3DIV
067400090916
067500950116     C                   MOVEL     PARAM3        KPJBU
067600950116     C                   CALL      'TNTA15R'
067700950116     C                   PARM                    KPJBA
067800950116     C*
067900950116     C                   MOVEL     '1'           WFLG
068000950116     C                   GOTO      ENDCOM
068100950116    1C                   ENDIF
068200950116     C*
068300950116     C** CMD21 - INTERROGAZIONE TARIFFE PARTICOLARI
068400950116    1C     *INKV         IFEQ      *ON
068500950116     C                   Z-ADD     V1CKSC        CLI5
068600950116     C                   MOVEL     V1DKSC        RAG5
068700950116     C                   MOVEL     ' '           FLG4
068800970325     C                   MOVEL     V1CFIE        PARFIE
068900020201     C* PASSO IL TIPO SERVIZIO BOLLA
069000020201     C                   MOVE      V1CTSP        PA5TSP
069100020201     C* Flag Network
069200020201     C                   MOVEL     V1CNET        FLGNET
069300020201      *
069400950116     C                   MOVEL     PARAM5        KPJBU
069500120321     c                   IF        not *in97
069600950116     C                   CALL      'TNTA68R'
069700950116     C                   PARM                    KPJBA
069800120321     c                   ELSE
069900120321     C                   CALL      'TNTA68R'
070000120321     C                   PARM                    KPJBA
070100120321     c                   parm                    tnsb48ds
070200120321     c                   ENDIF
070300950116     C*
070400950116     C                   MOVEL     KPJBU         PARAM5
070500950116     C*
070600950116     C                   MOVEL     '1'           WFLG
070700950116     C                   GOTO      ENDCOM
070800950116    1C                   ENDIF
070900950116     C*
071000950116     C** CMD18 - GESTIONE NOTE
071100100407      /free
071200100806       //?Nuovo programma gestione note
071300100806        IF  *inks;
071400100407          clear trmk20ds;
071500110511          IMK20tla = 'L';
071600100407          IMK20flm = 'I';
071700100407          IMK20ksc = v1cksc;
071800100407          IMK20rsc = v1dksc;
071900100407
072000100407          trmk20r (kpjba : TRMK20DS);
072100100407          wflg = '1';
072200100407          leavesr;
072300100407        ENDIF;
072400100407
072500100407       //?F8 - Note tariffa
072600100806        IF  *inkh;
072700100407          clear TNTA28ds;
072800100407
072900100407          ITA28ela = 'I';
073000100407          ITA28tip = 'C';
073100100407          ITA28cod = V1cksc;
073200100407          ITA28rsc = V1dksc;
073300100407          kpjbu    = TNTA28DS;
073400100407
073500100407          tnta28r (kpjba);
073600100407          wflg = '1';
073700100407          leavesr;
073800100407        ENDIF;
073900100407      /end-free
074000090709
074100090709     C** F2=RUBRICA
074200090709    1c                   if        *inkb
074300090709     c                   clear                   tnta12ds
074400090709     c                   eval      ta12ric = 'V'
074500090709     c  n50              Eval      ta12apl = 'C'
074600090709     c  n50              Eval      ta12ksc = v1cksc
074700090709     c   50              Eval      ta12apl = 'V'
074800090709     c   50              Eval      ta12nrv = v1cnrv
074900090709     c                   Eval      ta12rag = v1dksc
075000090709     c                   Call      'TNTA12R'
075100090709     c                   Parm                    kpjba
075200090709     c                   Parm                    tnta12ds
075300090709     C*
075400090709     C                   MOVEL     '1'           WFLG
075500090709     C                   GOTO      ENDCOM
075600090709    1C                   ENDIF
075700950116     C*
075800950116     C** CMD10 - STAMPA TARIFFA
075900950116    1C     *INKJ         IFEQ      *ON
076000950116     C                   EXSR      STAMPA
076100950116     C*
076200950116     C                   MOVEL     '1'           WFLG
076300950116     C                   GOTO      ENDCOM
076400950116    1C                   ENDIF
076500120319      /free
076600120319       //?F24 Altri tasti
076700120319         IF  *inky;
076800120319           exsr Gesf24;
076900120319           wflg = '1';
077000120319           leavesr;
077100120319         ENDIF;
077200120319      /end-free
077300950116     C*
077400950116     C     ENDCOM        ENDSR
077500950116     C*
077600940615     C*--- MUOVO CAMPI DA TNTAM A VIDEO ------------------------------*
077700950116     C     RIED01        BEGSR
077800051202     C                   setoff                                       43
077900950105     C                   SETOFF                                       8816
078000950116     C                   SETOFF                                       9596
078100940616     C*
078200940616     C****  CODICE TARIFFA  ****
078300940616     C                   Z-ADD     1             A                 3 0
078400950105     C                   MOVEL     TAMCTR        W001              1
078500940616     C                   MOVEL     'TR'          COD
078600940616     C                   MOVEL     *BLANKS       KEY
078700950105     C                   MOVEL     W001          KEY
078800940616     C     KTAB          CHAIN     TABEL                              30
078900950105    1C     *IN30         IFEQ      *OFF
079000941123     C                   MOVEL     TBLUNI        DSTR
079100941123     C                   MOVEL     §TRDES        V1DCTR
079200941123     C     §TRTAP        IFEQ      'S'
079300941123     C                   SETON                                        88
079400941123     C                   ENDIF
079500950105    1C                   ENDIF
079600950105     C*
079700950105     C* 16 ON  - INDICA IL TIPO TARIFFA "4 - A VALORE"
079800950105    1C     W001          IFEQ      '4'
079900950105     C                   SETON                                        16
080000950105    1C                   ENDIF
080100940616     C*
080200940616     C****  DATA SCADENZA TARIFFA  ****
080300940629    1C     TAMDDT        IFGT      0
080400940629    2C     TAMDST        IFGT      0
080500940811     C                   Z-ADD     TAMDST        DATA
080600940811     C                   Z-ADD     MM            GGMM              4 0
080700940811     C                   MOVEL     GG            GGMM
080800940811     C                   Z-ADD     AA            V1CDST
080900940811     C                   MOVEL     GGMM          V1CDST
081000940629    2C                   ENDIF
081100940616     C*
081200940616     C****  DATA DECORRENZA TARIFFA  ****
081300940811     C                   Z-ADD     TAMDDT        DATA
081400940811     C                   Z-ADD     MM            GGMM
081500940811     C                   MOVEL     GG            GGMM
081600940811     C                   Z-ADD     AA            V1CDDT
081700940811     C                   MOVEL     GGMM          V1CDDT
081800940629    1C                   ENDIF
081900940616     C*
082000940616     C****  DATA ULTIMA VARIAZIONE  ****
082100940629    1C     TAMDUV        IFGT      0
082200940811     C                   Z-ADD     TAMDUV        DATA
082300940811     C                   Z-ADD     MM            GGMM              4 0
082400940811     C                   MOVEL     GG            GGMM
082500940811     C                   Z-ADD     AA            V1CDUV
082600940811     C                   MOVEL     GGMM          V1CDUV
082700940629    1C                   ENDIF
082800940607     C*
082900940617     C                   Z-ADD     TAMPRG        V1CPRG
083000940617     C                   MOVE      TAMCTR        V1CCTR
083100940617     C                   MOVE      TAMDCV        V1CDCV
083200101013     C                   Z-ADD     TAMrct        V1Crct
083300940629     C                   Z-ADD     TAMPRI        V1CPRI
083400101013
083500101013      /free
083600101013       //?Cerco la data decorrenza scatto ISTAT
083700101013         clear V1Dist;
083800101013         IF  V1Crct > 0;
083900101013           SISsca = V1Crct;
084000101013           chain (SISsca) TISIS01L;
084100101013           IF  %Found(TISIS01L);
084200101013             wDate_ISO = %date(SISdata:*iso);
084300101013             wDate_Eur = wDate_ISO;
084400101013             V1Dist    = %dec(wDate_EUR);
084500101013           ENDIF;
084600101013         ENDIF;
084700101013      /end-free
084800101013
084900940617     C*
085000990827     C                   Z-ADD     TAMLRC        V1CMIS
085100990827     C                   Z-ADD     TAMLAS        V1CMAS
085200940629     C                   Z-ADD     TAMPAG        V1CPAG
085300041129     C                   Z-ADD     TAMPpa        V1CPpa
085400940622     C                   MOVEL     TAMTSP        V1CTSP
085500941109     C* DECODIFICA TIPO SPEDIZIONE
085600941109     C                   MOVEL     '5E'          COD
085700941109     C                   MOVEL     *BLANKS       KEY
085800941109     C                   MOVEL     V1CTSP        KEY
085900941109     C     KTAB          CHAIN     TABEL                              32
086000090608     C  N32              MOVEL     TBLUNI        ds5e
086100090608     C  N32              MOVEL     §5ed08        V1DTSP
086200941109     C*
086300940622     C                   MOVEL     TAMBAP        V1CBAP
086400940622     C                   MOVE      TAMFTP        V1CFTP
086500940622     C* CONTROLLO SE KG O MC PER CALCOLO VOLUME AUTOMATICO
086600940622     C                   Z-ADD     0             V1CKPM
086700940622     C                   Z-ADD     0             V1CMPC
086800940629    1C     TAMTIN        IFEQ      'K'
086900940622     C                   Z-ADD     TAMKPM        V1CKPM
087000940629    1C                   ENDIF
087100940629    1C     TAMTIN        IFEQ      'C'
087200940622     C                   MOVE      TAMKPM        V1CMPC
087300940629    1C                   ENDIF
087400940629     C                   Z-ADD     TAMATA        V1CATA
087500940629     C                   Z-ADD     TAMARL        V1CARL
087600940622     C                   Z-ADD     TAMARF        V1CARF
087700940629     C                   Z-ADD     TAMARO        V1CARO
087800940622     C*
087900940811     C                   MOVEL     TAMFTM        V2CFTM
088000940811     C                   MOVEL     TAMFTS        V2CFTS
088100940622     C                   MOVEL     TAMFIE        V1CFIE
088200940811     C                   MOVEL     TAMNAS        V2CNAS
088300940811     C                   MOVEL     TAMNOT        V2CNOT
088400940811     C                   MOVEL     TAMFIS        V2CFIS
088500940811     C                   MOVEL     TAMSIS        V2CSIS
088600940811     C                   MOVEL     TAMFVD        V2CFVD
088700970128     C                   MOVEL     TAMBAM        V2CBAM
088800980507     C* MODO APPLICAZIONE TARIFFA
088900980507     C                   MOVEL     TAMFLO        DSTA01
089000980507     C                   MOVEL     §TAF02        V1CMAT
089100020205      *
089200020205     C                   CLEAR                   V1CNET
089300020201     C     §TADPD        IFEQ      'S'
089400020201     C                   MOVE      'D'           V1CNET
089500020201     C                   ENDIF
089600020201     C     §TAFED        IFEQ      'S'
089700020201     C                   MOVE      'F'           V1CNET
089800020201     C                   ENDIF
089900020109     C*
090000020109     C                   MOVE      §TAPCA        V1CPCA
090100020109     C                   MOVE      §TATAP        V1CTAP
090200050914     c                   Movel     §taovb        v1covb
090300990622     C* DIVISA TARIFFA
090400990622     C                   MOVEL     §TADIV        V1CDIV
090500990622     C                   MOVEL     'CV'          COD
090600990622     C                   MOVEL     *BLANKS       KEY
090700990622     C                   MOVEL     V1CDIV        KEY
090800990622     C     KTAB          CHAIN     TABEL                              32
090900990622     C  N32              MOVEL     TBLUNI        DSCV
091000990622     C  N32              MOVEL     §CVDES        V1DDIV
091100990622     C   32              CLEAR                   V1DDIV
091200991119     C                   MOVE      V1CDIV        VICDIV
091300941018     C*
091400941019     C                   MOVEL     TAMGCA        V2CGCA
091500941018     C                   CLEAR                   V2CGCR
091600040601     c                   Movel     §tabva        v2cbva
091700060308     c                   eval      v2cfmp = tamfmp
091800120319
091900120319     c                   IF        TAMeds = 'N'
092000120319     c                   eval      v2Internet = 'NO'
092100120319     c                   ELSE
092200120319     c                   clear                   v2Internet
092300120319     c                   ENDIF
092400120319
092500941019     C* SOLO SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
092600941019     C     V2CGCA        COMP      *BLANKS                            1313
092700941019     C*
092800941019     C                   MOVEL     TAMGGA        V2CGGA
092900941019     C                   CLEAR                   V2CGGR
093000941019     C* SOLO SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
093100130124     C     V2CGGA        COMP      *BLANKS                            6363
093200941019     C*
093300941019     C                   MOVEL     TAMGMA        V2CGMA
093400941019     C                   CLEAR                   V2CGMR
093500941019     C* SOLO SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
093600960209     C     V2CGMA        COMP      *BLANKS                            1919
093700941019     C*
093800941019     C                   MOVEL     TAMGVA        V2CGVA
093900941019     C                   CLEAR                   V2CGVR
094000941019     C* SOLO SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
094100960209     C     V2CGVA        COMP      *BLANKS                            2424
094200941019     C*
094300950116     C*
094400950116     C* 95 ON  - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
094500950116     C                   SETOFF                                       95
094600990622     C     KTAM          SETLL     TITPT000
094700100407     C     KTAM          READE     TITPT000                               32
094800950116    3C     *IN32         DOWEQ     *OFF
094900950116     C     TPTATB        IFEQ      ' '
095000950116     C                   SETON                                        3295
095100950116     C                   ELSE
095200100407     C     KTAM          READE     TITPT000                               32
095300950116     C                   ENDIF
095400950116    3C                   ENDDO
095500950116     C*
095600950116     C* 96 ON  - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
095700100407     C     KTAM          CHAIN     TITGC000                           32
095800950116     C  N32TGCATB        IFEQ      ' '
095900950116     C                   SETON                                        96
096000950116     C                   ENDIF
096100950116     C*
096200900409     C                   ENDSR
096300940531     C*
096400940531     C*--- PULIZIA SUBFILE AGGIORNAMENTO -----------------------------*
096500900409     C     PULSF         BEGSR
096600000000     C                   SETON                                        94
096700940811     C                   WRITE     TA01C03
096800000000     C                   SETOFF                                       94
096900900925     C                   Z-ADD     0             SAVREC
097000000000     C                   ENDSR
097100940531     C*
097200940621     C*--- SR AGGIORNAMENTO ------------------------------------------*
097300920525     C     AGGIOR        BEGSR
097400120319
097500120319       //?Imposto i tasti funzione nella riga mobile
097600120319     c                   eval      wPrima = *off
097700120319     c                   eval      wSeconda = *off
097800120319     c                   eval      wTerza = *on
097900120319     c                   eval      wQuarta = *off
098000120319     c                   ExSr      Tastifun
098100120319
098200000000     C     SUAGG1        TAG
098300940811     C                   Z-ADD     0             V1CRDA
098400940811     C                   MOVEL     *BLANKS       V1CRA
098500940627     C*
098600000000     C     SUAGG         TAG
098700000000     C                   SETOFF                                       45
098800120319     C                   WRITE     TA01Z04
098900940811     C                   EXFMT     TA01C03
099000940622     C*
099100940622     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE
099200940622     C                   CLEAR                   VCCMSG
099300940622     C                   SETOFF                                       28
099400120327       //?F1 - dati manca tariffa
099500120327     c                   IF        *inka
099600120327     c                   exsr      gesw01
099700120327     c                   goto      suagg
099800120327     c                   ENDIF
099900120319       //?F3 - Fine
100000120319     c   kc              goto      fine
100100940622     C*
100200940531     C** CMD12 - FINE LAVORO NO AGG.
100300950117     C   KL              GOTO      FOR02
100400920525     C*
100500940531     C** CMD10 - STAMPA TARIFFA
100600940628     C     *INKJ         IFEQ      *ON
100700940628     C                   EXSR      STAMPA
100800940628     C                   GOTO      FINE
100900940628     C                   ENDIF
101000920525     C*
101100961212     C** CMD17-D E L T A
101200980902     C   KR              DO
101300031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
101400031124     c                   Clear                   Tibs02Ds
101500031124     c                   Clear                   dCat
101600031124     c                   Eval      T02Mod = 'C'
101700031124     c                   Eval      T02Sif = Knsif
101800031124     c                   Eval      T02Cod = 'CAT'
101900031124     c                   Movel(p)  '1'           T02Ke1
102000031124     c                   Call      'TIBS02R'
102100031124     c                   Parm                    Kpjba
102200031124     c                   Parm                    Tibs02Ds
102300031124     c                   If        T02Err = *Blanks
102400031124     c                   Movel     T02Uni        dCat
102500031124     c                   EndIf
102600031124    1 *
102700961212     C                   CLEAR                   DSTE00
102800961212     C                   MOVEL     'T05'         D00OP0
102900961212     C                   MOVEL     'F17'         D00OP1
103000961212     C                   MOVEL     'D'           D00TLA
103100980526     C                   MOVEL     'F'           D00DSF
103200980526     C                   MOVEL     'T'           D00CTO
103300961212     C                   MOVEL     V1CKSC        D00KSC
103400961212     C                   MOVEL     V1CCTR        D00CTR
103500961212     C                   MOVEL     V1CPRG        D00PRG
103600961212     C                   MOVEL     V1CFIE        D00FIE
103700961212     C                   MOVEL     V1CTSP        D00TSP
103800011003     C                   MOVEL     V1CDIV        D00DIV
103900020201     C                   MOVEL     V1CNET        D00DPD
104000011030     C                   MOVEL     *BLANKS       D00PRI
104100031124      * propongo data massima tra la data del giorno e quella recuperata
104200031124      * dalla tavella CAT
104300031124     c                   if        dateu8 > §CATdta
104400031124     C                   MOVEL     DATEU8        D00DTC
104500031124     C                   ELSE
104600031124     C                   MOVEL     §CATDTA       D00DTC
104700031124     C                   ENDIF
104800031124      *
104900970131     C                   CALL      'TNTE01C'
105000961212     C                   PARM                    KPJBA
105100961212     C                   PARM                    DSTE00
105200980902     C                   ENDDO
105300120319       //?F24 Altri tasti
105400120319     c                   IF        *inky
105500120319     c                   exsr      gesf24
105600120319     c                   goto      suagg
105700120319     c                   ENDIF
105800961212     C*
105900920525     C** ROLLUP  AVANZAMENTO NO AGG.
106000940607     C     *IN25         IFEQ      *ON
106100950327     C*
106200950327     C* IMPOSSIBILE CARICARE PIU' DI 9900 RECORD
106300950327     C     NRR           IFGE      9900
106400950327     C                   MOVEL     MSG(3)        VCCMSG
106500950327     C                   SETON                                        28
106600950327     C                   GOTO      SUAGG1
106700950327     C                   ENDIF
106800950327     C*
106900940531     C                   MOVEL     KE            KX
107000990622     C     KSCOR         SETLL     TITAD000                               22
107100990622     C   22KTAM          READE     TITAD000                               31
107200940531     C                   EXSR      CARSFA
107300940531     C                   GOTO      SUAGG1
107400940531     C                   ENDIF
107500940531     C*
107600940811     C                   MOVEL     V1CRDA        CONFR             5
107700940811     C                   MOVE      V1CRA         CONFR
107800940903     C     CONFR         COMP      CONFRP                             1414
107900940903     C   14              MOVEL     V1CRDA        CONFRP
108000940903     C   14              MOVE      V1CRA         CONFRP            5
108100000000     C** SALVO LA CHIAVE PER CONFRONTO SUCESSIVO
108200940903     C   14              GOTO      POI20
108300940903     C** 14 CHIAVE RICERCA
108400990915     C* SE DATO ENTER METTO 'C' NEL FLAG DI RITORNO
108500990915     C  N14              MOVEL     'C'           FLGRTN
108600940903     C  N14              GOTO      FINE
108700940903     C** 14 OFF - FINE TORNO A VIDEATA INIZIALE
108800000000     C** E' STATA DIGITATA UNA CHIAVE DI RICERCA  E
108900000000     C** RICERCO LA CHIAVE E RIEMETTO IL SUBFILE
109000000000     C** DA QUEL PUNTO IN POI
109100000000     C                   ENDSR
109200940531     C*
109300940531     C*--- CARICAMENTO SUBFILE IN AVANTI -----------------------------*
109400920525     C     CARSFA        BEGSR
109500950327     C                   Z-ADD     SAVREC        NRR               5 0
109600990622     C     KTAM          READE     TITAD000                               31
109700941126     C                   SETOFF                                       13
109800900925     C* FINE SCORRIMENTO
109900940607     C   31              DO
110000900925     C                   MOVEL     KE            KI
110100900925     C                   MOVEL     CTSE          CTSI
110200940622     C                   MOVEL     MSG(2)        VCCMSG
110300940622     C                   SETON                                        28
110400940622     C*
110500900925     C                   SETOFF                                       21
110600900925     C                   MOVEL     '000'         CONFRP
110700900925     C                   MOVE      '  '          CONFRP
110800900925     C                   GOTO      ENDCAR
110900900925     C                   END
111000940627     C*
111100900409     C** PULIZIA SUBFILE AGGIORNAMENTO
111200900925     C                   Z-ADD     0             B                 3 0
111300900925     C                   DO        *HIVAL
111400900925     C     TADATB        IFNE      'A'
111500951009     C*
111600951009     C*
111700900925     C                   ADD       1             B
111800900925     C                   ADD       1             NRR
111900931117     C** KIAVE INIZIALE
112000900925     C     B             IFEQ      1
112100900409     C                   MOVEL     KIAVE         KI
112200900409     C                   MOVEL     TADCTS        CTSI              2
112300900409     C                   END
112400931117     C*
112500940531     C** DESCRIZIONE CODICE TASSAZIONE
112600900409     C                   Z-ADD     1             A
112700940607     C     TADCTS        LOOKUP    CCT(A)                                 35
112800940607     C   35              MOVEL     DCT(A)        DESCTS
112900940607     C  N35              MOVEL     *BLANKS       DESCTS
113000941126     C*
113100941126     C* SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
113200941126     C*   LA LETTERA "M" SULLA RIGA DI DETTAGLIO
113300941126    3C     TADMIN        IFGT      0
113400941126     C     TADMAX        ORGT      0
113500941126     C                   SETON                                        13
113600941126    3C                   ENDIF
113700931117     C*
113800940811     C                   WRITE     TA01S03
113900941126     C                   SETOFF                                       13
114000941126     C*
114100941124     C     B             CABEQ     16            ENDAGA
114200900409     C                   END
114300900925     C*
114400951009     C     SOTTO         TAG
114500990622     C     KTAM          READE     TITAD000                               31
114600940607     C  N31              END
114700940622     C*
114800900409     C     ENDAGA        TAG
114900900925     C                   Z-ADD     NRR           REC
115000900925     C                   Z-ADD     NRR           SAVREC            9 0
115100900409     C                   MOVEL     KIAVE         KE
115200900409     C                   MOVEL     TADCTS        CTSE              2
115300900409     C** SE NON CI SONO PIU RECORD DA LEGGERE
115400900409     C** KE = KI
115500900925     C     ENDCAR        ENDSR
115600940531     C*
115700940531     C*--- ELIMINAZIONE SUBFILE SU DISCO -----------------------------*
115800900409     C     ELISF         BEGSR
115900000000     C                   SETON                                        93
116000940811     C                   WRITE     TA01C03
116100000000     C                   SETOFF                                       9493
116200000000     C                   ENDSR
116300940531     C*
116400940606     C*--- CARICAMENTO TABELLE ---------------------------------------*
116500920525     C     CARTAB        BEGSR
116600940531     C*
116700020206     C                   CLEAR                   DSCT
116800940606     C****  CODICI TASSAZIONE E DESCRIZIONI: CCT / DCT / ORP  ****
116900940531     C                   Z-ADD     1             K                 5 0
117000000000     C                   MOVEL     'CT'          COD
117100940531     C     KTAB2         SETLL     TABEL
117200940607     C     KTAB2         READE     TABEL                                  30
117300940531     C*
117400940629    1C     *IN30         DOWEQ     *OFF
117500940629    2C     TBLFLG        IFEQ      ' '
117600000000     C                   MOVEL     TBLKEY        CCT(K)
117700000000     C                   MOVEL     TBLUNI        DCT(K)
117800020206     C                   MOVEL     TBLUNI        DSCT
117900020206     C                   MOVE      §CTCOR        ORP(K)
118000940531     C                   ADD       1             K
118100940629    2C                   ENDIF
118200940531     C*
118300940607     C     KTAB2         READE     TABEL                                  30
118400940629    1C                   ENDDO
118500940531     C*
118600000000     C                   ENDSR
118700920525     C*
118800940531     C*--- STAMPA TARIFFA --------------------------------------------*
118900920525     C     STAMPA        BEGSR
119000940627     C*
119100970514     C*
119200940811     C                   Z-ADD     V1CKSC        PA2CLI
119300940310     C                   Z-ADD     PRG           PA2PRG
119400940310     C                   Z-ADD     VCTR          PA2CTR
119500940811     C                   MOVEL     V1DKSC        PA2RGS
119600920525     C                   MOVEL     PARAM2        KPJBU
119700940310     C*
119800941220     C                   CALL      'FNLV43R'
119900920525     C                   PARM                    KPJBA
120000920525     C                   GOTO      FINE
120100920525     C                   ENDSR
120200120319
120300120319      /free
120400120319       //--------------------------------------------------------------
120500120319       //?Imposto i tasti funzionali.
120600120319       //--------------------------------------------------------------
120700120319       BEGSR TastiFun;
120800120319
120900120319       //?Imposto la seconda riga variabile
121000120319         clear wpos;
121100120319         clear wposda;
121200120319         clear wpostf1;
121300120319         clear wpostf2;
121400120319         clear wpostf3;
121500120319         clear wrig;
121600120319         clear wtf1;
121700120319         clear wtf2;
121800120319         clear wtf3;
121900120319
122000120319         tf = 1;
122100120319         FOR tf by 1 to %elem(wTfv);
122200120321
122300120321         //?Abilito F1
122400120321           IF  not *in97 and
122500120321               %subst(wTfv(tf):1:3) = 'F1=';
122600120321             iter;
122700120321           ENDIF;
122800120319
122900120319         //?Abilito F2
123000120319           IF  (wQuarta or wTerza or not *in18) and
123100120319               %subst(wTfv(tf):1:3) = 'F2=';
123200120319             iter;
123300120319           ENDIF;
123400120319
123500120319         //?Abilito F8 - Note tariffa
123600130905           IF  (wTerza or wQuarta or not *in18) and
123700120319               %subst(wTfv(tf):1:3) = 'F8=';
123800120319             iter;
123900120319           ENDIF;
124000120319
124100120319         //?Abilito F10
124200120319           IF  (wQuarta or (wPrima and not *in10)) and
124300120319               %subst(wTfv(tf):1:3) = 'F10';
124400120319             iter;
124500120319           ENDIF;
124600120319
124700120319         //?Abilito F11
124800120319           IF  (wPrima or wSeconda) and
124900120319               %subst(wTfv(tf):1:3) = 'F11';
125000120319             iter;
125100120319           ENDIF;
125200120319
125300120319         //?Abilito F18
125400120319           IF  (wTerza or wQuarta or (wPrima and not *in10) or
125500120319               not *in18 or *in50) and
125600120319               %subst(wTfv(tf):1:3) = 'F18';
125700120319             iter;
125800120319           ENDIF;
125900120319
126000120319           IF  wTfv(tf) <> *blanks;
126100120319             wpos = %scan('#':wTfv(tf));
126200120319             wpostf1 += wpos;
126300120319
126400120319             IF  wpostf1 <= %len(VZFd01);
126500120319               wposda = wpostf1 - wpos + 1;
126600120319               %subst(wtf1:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
126700120319             ELSE;
126800120319               wpostf2 += wpos;
126900120319               IF  wpostf2 <= %len(VZFd01);
127000120319                 wposda = wpostf2 - wpos + 1;
127100120319                 %subst(wtf2:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
127200120319               ELSE;
127300120319                 wpostf3 += wpos;
127400120319                 IF  wpostf3 <= %len(VZFd01);
127500120319                   wposda = wpostf3 - wpos + 1;
127600120319                   %subst(wtf3:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
127700120319                 ENDIF;
127800120319               ENDIF;
127900120319             ENDIF;
128000120319
128100120319           ENDIF;
128200120319         ENDFOR;
128300120319
128400120319       //?Imposto F24 giusto
128500120319         SELECT;
128600120319           WHEN  wtf3 <> *blanks;
128700120319             VZFd02 = cf2413;
128800120319             wf24 = 3;
128900120319           WHEN  wtf2 <> *blanks;
129000120319             VZFd02 = cf2412;
129100120319             wf24 = 2;
129200120319           OTHER;
129300120319             clear VZFd02;
129400120319             wf24 = 1;
129500120319         ENDSL;
129600120319
129700120319         VZFd01 = wtf1;
129800120319         wrig = 1;
129900120319
130000120319       ENDSR;
130100120319
130200120319       //--------------------------------------------------------------
130300120319       //?Gestione tasto funzionale F24
130400120319       //?F24=Altri Tasti
130500120319       //--------------------------------------------------------------
130600120319       BEGSR gesf24;
130700120319
130800120319         SELECT;
130900120319           WHEN  (wf24 = 3 and wrig = 3) or
131000120319                 (wf24 = 2 and wrig = 2);
131100120319             VZFd01 = wtf1;
131200120319             wrig = 1;
131300120319             IF  wf24 = 2;
131400120319               VZFd02 = cf2412;
131500120319             ELSE;
131600120319               VZFd02 = cf2413;
131700120319             ENDIF;
131800120319
131900120319           WHEN  (wf24 = 3 and wrig = 1) or
132000120319                 (wf24 = 2 and wrig = 1);
132100120319             VZFd01 = wtf2;
132200120319             wrig = 2;
132300120319             IF  wf24 = 2;
132400120319               VZFd02 = cf2422;
132500120319             ELSE;
132600120319               VZFd02 = cf2423;
132700120319             ENDIF;
132800120319
132900120319           WHEN  wf24 = 3 and wrig = 2;
133000120319             VZFd01 = wtf3;
133100120319             wrig = 3;
133200120319             VZFd02 = cf2433;
133300120319
133400120319         ENDSL;
133500120319
133600120319       ENDSR;
133700120321
133800120321       //--------------------------------------------------------------
133900120321       //?Visualizza Dati manca tariffa
134000120321       //--------------------------------------------------------------
134100120321       BEGSR GesW01;
134200120321
134300120321         wFine = *off;
134400120321         W01dsp = ISB48dsp;
134500120321         W01tbl = ISB48tbl;
134600120321         W01lnp = ISB48lnp;
134700120321         W01lna = ISB48lna;
134800120321         W01pro = ISB48pro;
134900120321         W01cts = ISB48cts;
135000120321         W01ctr = ISB48ctr;
135100120321         W01ncl = ISB48ncl;
135200120321         W01pkf = ISB48pkf;
135300120321         W01vlf = ISB48vlf;
135400120321         W01err = ISB48err;
135500120321
135600120321         DOW wFine = *off;
135700120321           exfmt  TA01W01;
135800120321           //?- F12 = Ritorno
135900120321           IF  *inkl;
136000120321             wFine = *on;
136100120321           ENDIF;
136200120321         ENDDO;
136300120321
136400120321       ENDSR;
136500120713
136600120713       //--------------------------------------------------------------
136700120713       //?Ricerca Codice Tassazione
136800120713       //--------------------------------------------------------------
136900120713       BEGSR Ric_CT;
137000120713
137100120713         clear TRTB09DS;
137200120713         TB09fun = '1';
137300120713         TB09ord = '1';
137400120713         IF  V1Cnet = 'D';
137500120713           TB09est = 'E';
137600120713         ELSE;
137700120713           TB09est = V1Cfie;
137800120713         ENDIF;
137900120713         TB09uta = 'S';
138000120713         IF  V1Cnet = 'F';
138100120713           TB09utf = 'S';
138200120713         ELSE;
138300120713           TB09utf = 'N';
138400120713         ENDIF;
138500120713         kpjbu = TRTB09DS;
138600120713         trtb09r (kpjba);
138700120713         TRTB09DS = kpjbu;
138800120713
138900120713       ENDSR;
139000120713
139100120319      /end-free
139200940629     C*
139300000000     C*---------------------------------------------------------------*
139400950327**   TES
139500900409 ** INTERROGAZ. TARIFFE  CLIENTI **
139600940607**   MSG
139700940607Non esistono righe di dettaglio per questa tariffa
139800940622Fine Scorrimento
139900950327Impossibile visualizzare ulteriori righe di dettaglio: utilizzare la ricerca
140000051202Selezione NON concessa in visualizzazione
140100120319
140200120319** - wTfv (in ordine di visualizzazione) ------------------------------------*
140300120321F1=Manca Tariffa  #
140400120319F3=Fine  #
140500120319F2=Rubrica  #
140600120319F8=Note Tar.  #
140700120319F10=Stampa  #
140800120319F11=Altri Dati  #
140900120319F18=Note  #
141000120319F12=Ritorno   #
