000100070918      *PARMS CLOSQLCSR(*ENDMOD)
000200070918      *PARMS DYNUSRPRF(*OWNER)
000300030723     H DECEDIT('0,') DATEDIT(*DMY/)
000400941216     H* FNLR25R *-----------------------------------------------------*
000500930126     H*                - ABBINAMENTO FOGLI VIAGGIO -                  *
000600930126     H*---------------------------------------------------------------*
000700941216     FFNLR25D   CF   E             WORKSTN
000800941216     F                                     SFILE(LR25DSF:NRR)
000900921109     FTABEL00F  IF   E           K DISK
001000970721     FAZCPC03L  IF   E           K DISK
001100960723     FFNFVV01L  UF   E           K DISK
001200941221     FFNFVA02L  IF   E           K DISK
001300941221     FFNFVA01L  UF   E           K DISK
001400941221     F                                     RENAME(FNFVA000:FNFVA001)
001500951128     FFNFVA04L  IF   E           K DISK
001600951128     F                                     RENAME(FNFVA000:FNFVA004)
001700030722     f                                     prefix(F04:3)
001800000628     FFNFWA01L  IF   E           K DISK
001900030722     FFNFV401L  IF   E           K DISK
002000070123     FFNBRV02L  IF   E           K DISK
002100941220     FFNARB01L  UF   E           K DISK
002200950621     FFNARB06L  UF   E           K DISK
002300950621     F                                     RENAME(FNARB000:FNARB006)
002400941220     FFNART27L  UF   E           K DISK
002500950407     FFNAGB01L  IF A E           K DISK
002600971223     FAZORG01L  IF   E           K DISK
002700921109     D*
002800951114     D* DEFINIZIONE SCHIERE
002900921120     D FVA             S              9  0 DIM(70)                              FVA DA INTERR
003000980109     D F2              S              8  0 DIM(208)                             FV GIA' SELEZ
003100980109     D FV              S             11  0 DIM(208)                             FV GIA' SELEZ
003200921118     D*
003300921118     D FVS             S             11  0 DIM(208)                             FV SELEZIONATI
003400921118     D STS             S              1    DIM(208)                             STAMP DE FV SE
003500921118     D ATS             S              1    DIM(208)                             FV IN TRANSITO
003600951117     D TRN             S              7  0 DIM(208)                             NUM. TRAINO FV SELEZ
003700951117     D DTN             S              8  0 DIM(208)                             DATA TRAINO FV SELEZ
003800040122     D DFV             S              8  0 DIM(208)                             DATA TRAINO FV SELEZ
003900960222     D DHA             S             11    DIM(208)                             DATA/ORA/CAUS.ARRIVO
004000950328     D*
004100950328     D L6              S              3  0 DIM(30)                              F. MEC    GES
004200150116     D*MECN            S              3  0 DIM(30)                              L6 NO GESTITA
004300040122     D NOG             S              3    DIM(30)                              L1 NO GESTITA
004400951114     D*
004500000606     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             MESSAGGI ERRORE
004600960202     D*
004700941216     D* KPJBU PER STAMPA SPEDIZIONI NON ABILITATE  - FNLR52R -
004800950626     D PARA52          DS                  INZ
004900960518     D  P52DAT               108    115  0
005000941221     D  P52LNP               118    120  0
005100941221     D  P52FVA               121    125  0
005200941221     D  P52FEL               126    128  0
005300950215     D  P52OUT               129    129
005400970825     D* USATI PER L'ELENCO BOLLE DA CONSEGNARE
005500950626     D  P52INC               130    131
005600950626     D  P52IMA               132    133
005700960321     D  P52DFA               134    141  0
005800970516     D  P52FIA               142    146  0
005900970516     D  P52FGS               147    149  0
006000980226     D**
006100980226     D  P52LAI               150    152  0
006200040123
006300040123
006400941221     D PARA26          DS
006500941221     D* CHAIAMATA AL PGM DI RICERCA FOGLI PARTENZA ALL'ARRIVO -FNLR26R-
006600941221     D  P26PGM                 1      7
006700941221     D  P26LAI                 8     10  0
006800941221     D  P26FEL                11     13  0
006900941221     D  P26ABB                14     14
007000941221     D  P26DSD                15     22  0
007100941221     D  P26DSA                23     30  0
007200941221     D  P26FLG                31     31
007300941221     D  P26LNP                32     34  0
007400971002     D  P26LAN                35     35
007500980204     D  P26MSG                36    113
007600941221     D PARA04          DS
007700941221     D* PARA04: PARAMETRI IN INPUT/OUTPUT AL PGM FNLR04R
007800941221     D  P04NFV                 1      5
007900941221     D  P04LNP                 6      8  0
008000941221     D  P04LNA                 9     11  0
008100941221     D  P04NRS                12     13  0
008200941221     D  P04NCD                14     20  0
008300941221     D  P04NCA                21     27  0
008400941221     D  P04NPS                28     29  0
008500941221     D  P04NPG                30     30  0
008600941221     D  P04FLG                31     31
008700941221     D  P04PGM                32     38
008800941221     D  P04FGS                39     41  0
008900101202     d* bolla
009000101202     d  p04bolp               42     44  0
009100101202     d  p04bors               45     46  0
009200101202     d  p04bosp               47     53  0
009300101202     d  p04boan               54     57  0
009400101202     D  P04ERR                58     58
009500101202     d*
009600950328     D PARA05          DS
009700950328     D* PARA05: PARAMETRI PER AGGIORNAMENTO BOLLE  FNLV52R
009800950328     D  P05TBO                 1      1
009900950328     D  P05AGB                 2      3
010000951115     D*
010100960201     D* PASSAGGIO DATI A PGM TRASMISS. FNFVA/FNFV4  - FNLR20R -
010200960201     D PARAM2          DS
010300960201     D* PARNUM E' USATO PER RISOTTOMETTERE L'AZIONE DI TRASMISSIONE
010400960201     D  PA2NUM                 1      1  0 INZ
010500911010     D WLBDAT          DS
010600941221     D  G02DAT                 1      8  0
010700941221     D  G02INV                 9     16  0
010800941221     D  G02ERR                17     17
010900941221     D  G02TGI                18     22  0
011000960304     D WGIDAT          DS                  INZ
011100960304     D  GIODAT                 1      8  0
011200960304     D  GIOINV                 9     16  0
011300960304     D  GIOTGI                17     21  0
011400920428     D                 DS
011500941221     D  ARTAAS                 1      4  0
011600941221     D  ARTLNP                 5      7  0
011700941221     D  ARTNRS                 8      9  0
011800941221     D  ARTNSP                10     16  0
011900941221     D  ARTSPE                 1     16  0
012000070921     D                 DS
012100070921     D  WsaAAS                 1      4  0
012200070921     D  WSALNP                 5      7  0
012300070921     D  WSANRS                 8      9  0
012400070921     D  WsaNSP                10     16  0
012500070921     D  SAVSPE                 1     16  0
012600921118     D                 DS
012700921118     D  COMLNP                 1      3  0
012800921118     D  VI2FVA                 4      8  0
012900921119     D  VI2LAI                 9     11
013000921118     D  VI2KEY                 1     11  0
013100951117     D                 DS
013200951117     D  WLNP                   1      3  0
013300951117     D  WFVA                   4      8  0
013400951117     D  WLAI                   9     11
013500951117     D  WKEY                   1     11  0
013600921119     D                 DS
013700921119     D  FVALNP                 1      3  0
013800921120     D  FVANFV                 4      8  0
013900921119     D  FVALAI                 9     11  0
014000921119     D  FVAKEY                 1     11  0
014100951121     D                 DS
014200951121     D  VI2AED                 1      6  0
014300951121     D  VI2AEH                 7     10  0
014400960222     D  VI2RTC                11     11
014500960222     D  WDHARR                 1     11
014600030924
014700030924     d                 ds
014800030924     d  wore                   1      2
014900030924     d  wminuti                3      4
015000030924     d  worario                1      4
015100951110     D*
015200950328     D* DS PER TRUL06R - CARICAMENTO £X
015300950328     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015400950328     D  LIN                    1     90  0
015500950328     D                                     DIM(30)                              FIL. COMODO
015600040122      * Parametri per richiamo a TRUL26R
015700150116     d*Trul26ds      e ds
015800150116     d**skpo                  11    760  0 dim(250)
015900150116     d**skpot                761   1510  0 dim(250)
016000040120      * Parametri per richiamo a TRUL09R
016100040120     d Trul09ds      e ds
016200040120     d  mec                   19    108  0 dim(30)
016300951114     D*
016400951114     D* DS PER TNTL35R - CONTROLLO ESISTENZA RITAR./ANTIC. ARRIVO FV
016500951114     D DSTL35        E DS                  EXTNAME(TNTL35DS)
016600960201     D*
016700960201     D* DS PER FNLV15R - GESTIONE DESCRIZIONE CAUSALE RITARDO/ANTICIPO
016800960201     D DSLV15        E DS                  EXTNAME(FNLV15DS)
016900951114     D*
017000970925     D* DS PER TNSD99R - CAOCLO TEMPI DI TRAZIONI INTERNI
017100970925     D DSSD99        E DS                  EXTNAME(TNSD99DS)
017200970430     D DSLV53        E DS                  EXTNAME(FNLV53DS)
017300970825     D DSLV55        E DS                  EXTNAME(FNLV55DS)
017400960304     D*
017500960207     D DS1X          E DS
017600960626     D DS7AF         E DS
017700930208     D DS7N          E DS
017800920901     D KPJBA         E DS
017900110103      *
018000110103      * - Parametri per richiamo al pgm di controllo profilo utenti
018100110103     d TIBS34ds      e ds
018200110103      * - Ds di riferimento al file esterno AzUte00F
018300110103     d AZUTEds       e ds                  ExtName(AzUte00F)
018400110103      * - Ds per dati organigramma
018500110103     d dDatiUte      e ds
018600110103      *
018700941221     D DSLR01        E DS                  INZ
018800000628     D OG143         E DS
018900070918     dwoldaed          s                   like(fvaaed)
019000070918     dwoldaeh          s                   like(fvaaeh)
019100070918     d wrkgetlista     s           4096    varying
019101141110     d in              s              3  0
019200951114     I*
019300000000     C*---------------------------------------------------------------*
019400911010     C* INDICATORI
019500911010     C*---------------------------------------------------------------*
019600921123     C*  01    - SI TRATTA SOLO DI UNA BUSTA
019700030722     C*  04    - ABILITO CAMBIO FILIALE SE PREMUTO F18
019800921120     C*  05    - SONO UNA FILIALE DI 2 LIVELLO
019900921123     C*  09    - RICERCA FOGLI
020000911010     C*  10    - ROLL UP
020100960108     C*  16    - E' STATA MODIFICATA LA DATA E ORA ARRIVO REALE OPPURE
020200960108     C*          SI TRATTA DI FOGLIO VIAGGIO CON NUMERO F.ARRIVI = 0
020300960626     C*  17    - ABBINAMENTO DI FOGLIO ARRIVI CON DATA <= DATA ULTIMA
020400960626     C*          ELABORAZIONE IDD IN FORMA DEFINITIVA
020500951115     C*  18    - ARRIVO IN ANTICIPO/RITARDO
020600951115     C*  28    - EMETTE CAMPO MESSAGGI
020700951115     C*  30/32 - DI COMOD0
020800941221     C*  33    - LETTURA FNFVA
020900951117     C*  34/35 - DI COMODO
021000951227     C*  38    - RECORD ALLOCATO IN FNARB06L
021100951117     C*  40/59 - ERRORE
021200971003     C*  60/63 - POSIZIONAMENTO CURSORE PER MESSAGGI DI ERRORE
021300911010     C*  72/73 - DI SUBFILE
021400911010     C*  90    - ERRORE GENERICO
021500911010     C*---------------------------------------------------------------*
021600000000     C     *ENTRY        PLIST
021700000000     C                   PARM                    KPJBA
021800110103      /free
021900110103       // Impostazione opzioni per SQL
022000110103       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
022100110103       exec sql   set  option  DynUsrPrf = *Owner,
022200110103                               CloSqlCsr = *EndMod;
022300110103      /end-free
022400110103      *
022500971002     C* SE RICLAN=6 VUOL DIRE CHE E' RICHIAMO DA FNLR26R
022600971002     C                   MOVEL     KPJBU         RICLAN            1
022700941221     C*
022800110103     C                   Z-ADD     1             CODUT             1 0
022900110103      *
023000110103      * Reperimento dati utente/aziendali
023100110103     c                   exsr      sr_DatiJob
023200951115     C*
023300900511     C*---------------------------------------------------------------*
023400950407     C* OPERAZIONE FITTIZIA PER GIUSTIFICARE IL FILE IN INPUT
023500950407     C                   READ      FNAGB000                               30
023600921109     C*
023700921109     C                   EXSR      CARTAB
023800900517     C*
023900911010     C     INIZIO        TAG
024000911008     C                   EXSR      PULIZ
024100971223     C*
024200921123     C     FOR01         TAG
024300941216     C   09              WRITE     LR25D03
024400911008     C*
024500941216     C                   EXFMT     LR25D01
024600030722     C** F3 - FINE LAVORO
024700971223     C   28
024800971223     COR KC              GOTO      FINE
024900930127     C*
025000030722     C** F18- CAMBIO FILIALE
025100921117     C   KS              SETON                                        04
025200921117     C   KS              GOTO      FOR01
025300921117     C*
025400921117     C   04              DO
025500930208     C                   SETOFF                                       09
025600941220     C                   MOVEL     VIDFGS        ALF3              3
025700960227     C     ALF3          LOOKUP    NOG                                    30
025800960227     C   30              SETON                                        49
025900960227     C   30              GOTO      FOR01
026000960626     C* CHAIN SULLA TABELLA 7A PER PRENDERE LA DATA ULTIMA ELABORAZIONE
026100960626     C* IDD
026200960626     C                   EXSR      CTB7A
026300960227     C* SE TUTTO OK CARICO LA L6
026400960227     C                   EXSR      CARL6
026500921117     C                   END
026600921117     C*
026700911008     C* CONTROLLO FOGLIO VIAGGIO ARRIVI
026800911008     C                   EXSR      CONTR1
026900930127     C* SE ERRORE
027000930127     C   90              GOTO      FOR01
027100930127     C                   Z-ADD     1             REC
027200911008     C*
027300930127     C* EMISSIONE SFILE
027400951120     C                   SETOFF                                       45
027500911010     C     FORCT2        TAG
027600941216     C                   WRITE     LR25D02
027700961014     C   72              WRITE     LR25D04
027800911008     C     FORCT         TAG
027900911010     C                   Z-ADD     1             NRR
028000941216     C                   EXFMT     LR25DCT
028100951115     C*
028200951115     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
028300951115     C                   CLEAR                   VCCMSG
028400951117     C                   SETOFF                                       286061
028500030924     C                   SETOFF                                       626353
028600951115     C*
028700951121     C* CONTROLLO SE POSSONO ESSERE PREMUTI COMANDI CHE NON AGGIORNANO
028800961014     C  N72*INKC         IFEQ      *ON
028900951121     C     *INKL         OREQ      *ON
029000960109     C*
029100960110     C* SE TROVATI RECORD ALLOCATI COSTRINGO A PREMERE NUOVAMENTE F6
029200960110     C     WALL          IFEQ      '1'
029300960110     C                   MOVEL     MSG(12)       VCCMSG
029400960110     C                   SETON                                        28
029500960110     C                   GOTO      FORCT2
029600960109     C                   ENDIF
029700960109     C*
029800960202     C* CONTROLLO CAUSALE
029900960626     C                   EXSR      F03F12
030000960626     C   90              GOTO      FORCT2
030100960202     C*
030200960202     C* SE VARIATO QUALCOSA IN FNFV4 ED ESCO SENZA CONFERMARE
030300960202     C*   SOTTOMETTO PGM DI TRASMISSIONE
030400960202     C     WAGG          IFNE      ' '
030500960202     C                   MOVEL     PARAM2        KPJBU
030600960202     C                   MOVEL     'LR20'        KCOAZ
030700960202     C                   CALL      'BCH10'
030800960202     C                   PARM                    KPJBA
030900960202     C                   ENDIF
031000951121     C                   ENDIF
031100951121     C*
031200030722     C** F3 - FINE
031300911030     C   KC              GOTO      FINE
031400930127     C*
031500030722     C** F12 - RITORNO
031600911008     C   KL              GOTO      INIZIO
031700930127     C*
031800030722     C** F7 - INTERROG. SPUNTE PARTENZE ALL'ARRIVO
031900941221     C   KG              DO
032000941221     C                   CLEAR                   PARA04
032100941221     C                   Z-ADD     VIDFGS        P04FGS                         FIL.GESTIONE
032200941221     C                   MOVEL     VIDNPG        P04NPG                         CATEGORIA
032300941221     C                   MOVEL     VIDNFV        P04NFV                         NR.FOGLIO
032400941221     C                   Z-ADD     99            P04NRS
032500941221     C                   Z-ADD     99            P04NPS
032600941221     C                   MOVEL     '1'           P04FLG                         '1'=INTERROGAZIONE
032700941221     C                   MOVEL     'FNLR25R '    P04PGM                         RICHIAMATO
032800941221     C                   MOVEL     PARA04        KPJBU
032900941221     C                   CALL      'FNLR04R'
033000920225     C                   PARM                    KPJBA
033100941221     C                   GOTO      FORCT2
033200941221     C                   ENDDO
033300921120     C*
033400960626     C** 10 ON  - ROLL UP SOLO SE L'ULTIMO E' PIENO E 17 OFF
033500960626     C   10              DO
033600960626     C                   SETOFF                                       30
033700960626     C     SAVNRR        SUB       1             NRR
033800960626     C  N17NRR           CHAIN     LR25DSF                            30
033900920316     C  N30VI2LNP        IFLE      *ZEROS
034000920316     C     VI2FVA        ANDEQ     *ZEROS
034100960626     C     *IN17         OREQ      *ON
034200920316     C                   SETON                                        43
034300911008     C*
034400920316     C                   Z-ADD     NRR           REC
034500920316     C                   GOTO      FORCT
034600920316     C                   END
034700911008     C*
034800920316     C                   ADD       1             NRR
034900920316     C                   Z-ADD     1             B
035000920316     C                   Z-ADD     NRR           REC
035100911008     C                   EXSR      PULSFL
035200920316     C                   GOTO      FORCT
035300911008     C                   END
035400911008     C* CONTROLLO SUBFILE
035500911008     C                   EXSR      CTRSFL
035600911030     C   45
035700960206     COR 09              GOTO      FORCT2
035800920706     C   90              GOTO      FORCT
035900951120     C  NKF
036000951120     CANNKN              GOTO      FORCT2
036100030722     C* F6  - ELABORAZIONE
036200030722     C* F14 - FORZATURA
036300950104     C   KN
036400950104     COR KF              DO
036500921029     C                   EXSR      ELAB
036600960205     C*
036700960205     C* WALL = "1" ---> RECORD ALLOCATO IN FNARB06L: NON TUTTE LE BOLLE
036800960205     C*   SONO STATE AGGIORNATE, PREMERE NUOVAMENTE F6
036900960205     C     WALL          IFEQ      '1'
037000960205     C                   MOVEL     MSG(11)       VCCMSG
037100960205     C                   SETON                                          28
037200960205     C                   GOTO      FORCT
037300960205     C                   ENDIF
037400960723     C**
037500960723     C* CHIUDO FOGLIO ARRIVI SE 17 ON
037600960723     C     *IN17         IFEQ      *ON
037700960723     C     KFVV1         CHAIN     FNFVV01L                           30
037800960723     C     *IN30         IFEQ      *OFF
037900960723     C                   MOVE      'S'           FVVFCF
038000960723     C                   UPDATE    FNFVV000
038100960723     C                   END
038200960723     C                   END
038300960205     C**
038400960201     C* SOTTOMETTO AZIONE DI TRASMISSIONE FNFVA00F/FNFV400F IN SEDE E
038500960201     C*   ALLA FILIALE DI PARTENZA
038600960201     C                   MOVEL     PARAM2        KPJBU
038700960129     C                   MOVEL     'LR20'        KCOAZ
038800951228     C                   CALL      'BCH10'
038900951228     C                   PARM                    KPJBA
039000921029     C                   END
039100951227     C*
039200000000     C     FINE          TAG
039300960202     C*
039400960202     C* RICHIAMO PGM FNLV15R PER CHIUSURA FILES
039500960202     C     WLV15         IFNE      ' '
039600960202     C                   CLEAR                   DSLV15
039700960202     C                   MOVEL     'C'           D15TLA
039800960202     C                   MOVEL     DSLV15        KPJBU
039900960202     C                   CALL      'FNLV15R'
040000960202     C                   PARM                    KPJBA
040100960202     C                   ENDIF
040200960202     C* RICHIAMO PGM TNTL35R PER CHIUSURA FILES
040300960202     C     WTL35         IFNE      ' '
040400960202     C                   CLEAR                   DSTL35
040500960202     C                   MOVEL     'C'           D35TLA
040600960202     C                   MOVEL     DSTL35        KPJBU
040700960202     C                   CALL      'TNTL35R'
040800960202     C                   PARM                    KPJBA
040900960202     C                   ENDIF
041000970430     C*
041100970430     C     WLV53         IFEQ      '1'
041200970430     C                   MOVEL     'C'           D53TLA
041300970430     C                   CALL      'FNLV53R'
041400970430     C                   PARM                    DSLV53
041500970430     C                   END
041600970825     C                   MOVEL     'C'           D55TLA
041700970825     C                   CALL      'FNLV55R'
041800970825     C                   PARM                    DSLV55
041900040123     c*
042000040123     c                   eval      i09tla='C'
042100040123     c                   call      'TRUL09R'
042200040123     c                   parm                    trul09ds
042300960202     C*
042400000000     C                   SETON                                        LR
042500960202     C*
042600941216     C**************************************************************************
042700941216     C*    CARICO TABELLE
042800941216     C**************************************************************************
042900921109     C     CARTAB        BEGSR
043000040122      * Carico tutti i p.o. gestiti con il relativo p.o. gestore
043100150116     c***                Clear                   Trul26ds
043200150116     c***                Eval      I26tla = 'L'
043300150116     c***                Eval      I26tpt = '6'
043400150116     c***                Call      'TRUL26R'
043500150116     c***                Parm                    Trul26ds
043600960227     C***
043700960227     C* CARICO TABELLA FILIALI GESTITE DA ALTRE  SU TUTTO L'AS
043800040122     C***
043900040122     C                   CLEAR                   DSUL06
044000040122     C                   MOVE      '£6'          D06COD
044100040122     C                   MOVEL     SIMFEL        D06KEY
044200040122     C                   MOVEL     'G'           D06ESC
044300040122     C                   MOVEL     DSUL06        KPJBU
044400960227     C*
044500040122     C                   CALL      'TRUL06R'
044600040122     C                   PARM                    KPJBA
044700040122     C                   MOVEL     KPJBU         DSUL06
044800040122     C                   MOVEL     LIN           NOG
044900950328     C***
045000020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
045100950328     C***
045200110103     C                   Z-ADD     dutPOU        VIDFGS
045300110103     C     dutLPO        IFEQ      '2'
045400110103     C     dutLPO        OREQ      *BLANKS
045500020424     C                   SETON                                        05
045600020424     C                   ENDIF
045700960227     C* CARICO LA L6 DELLA FILIALE GESTIONE IMPOSTATA
045800960227     C                   EXSR      CARL6
045900921120     C*
046000921120     C* IMPOSTO CAMPI DI PASSAGGIO
046100941221     C                   MOVEL     2             PARNPG            1 0
046200960626     C*
046300960626     C                   EXSR      CTB7A
046400921109     C                   ENDSR
046500960227     C**************************************************************************
046600960227     C*    CARICO LA £6
046700960227     C**************************************************************************
046800960227     C     CARL6         BEGSR
046900960227     C***
047000960227     C* CARICO TABELLA FILIALI GESTITE DA ALTRE
047100960227     C***
047200960227     C                   CLEAR                   DSUL06
047300960227     C                   MOVE      '£6'          D06COD
047400960227     C                   MOVEL     VIDFGS        D06KEY
047500960227     C                   MOVEL     'L'           D06TLA
047600960227     C                   MOVEL     DSUL06        KPJBU
047700960227     C*
047800960227     C                   CALL      'TRUL06R'
047900960227     C                   PARM                    KPJBA
048000960227     C                   MOVEL     KPJBU         DSUL06
048100960227     C                   MOVEA     LIN           L6
048200960227     C                   ENDSR
048300960626     C**************************************************************************
048400960626     C* CHAINO TABELLA 7A PER REPERIRE DATA ULTIMA ELABORAZIONE IDD
048500960626     C**************************************************************************
048600960626     C     CTB7A         BEGSR
048700960626     C***
048800960626     C* CHAIN SULLA TABELLA 7A PER PRENDERE LA DATA ULTIMA ELABORAZIONE
048900960626     C* IDD
049000960626     C                   MOVEL     '7A'          COD
049100960626     C                   MOVEL(P)  VIDFGS        KEY
049200960626     C     KTAB          CHAIN     TABEL                              30
049300960626     C  N30              MOVEL     TBLUNI        DS7AF
049400960626     C   30              MOVE      *ZEROS        §7ADID
049500960626     C                   ENDSR
049600941216     C**************************************************************************
049700941216     C*    PULIZIA CAMPI
049800941216     C**************************************************************************
049900911008     C     PULIZ         BEGSR
050000960202     C                   CLEAR                   WAGG
050100960202     C                   CLEAR                   WALL
050200941216     C*
050300920709     C* PRIMA VIDEATA
050400911009     C                   MOVEL     *BLANKS       VIDNFV
050500120709     C                   MOVEL     'N'           VIDSTA
050600941221     C                   TIME                    UTIME            14 0          ORA E DATA
050700960205     C* DATA A 8
050800960202     C                   MOVE      UTIME         UDATE8            8 0          GG/MM/AAAA
050900960227     C                   Z-ADD     UDATE8        G02DAT
051000960227     C                   MOVEL     *BLANK        G02ERR
051100960227     C                   CALL      'XSRDA8'
051200960227     C                   PARM                    WLBDAT
051300960227     C                   Z-ADD     G02INV        DATEU8            8 0          AAAA/MM/GG
051400960205     C* DATA A 6
051500960205     C                   MOVE      UDATE8        W0020             2 0
051600960205     C                   MOVEL     UDATE8        UDATE6            6 0
051700960205     C                   MOVE      W0020         UDATE6
051800960205     C*
051900960206     C                   MOVEL     UTIME         WTIME             4 0          HH:MM
052000040227     c*
052100040227     C                   MOVEL     '2'           VIDNPG
052200911009     C* SUBFILE
052300961014     C                   SETON                                        720973
052400941216     C                   WRITE     LR25DCT
052500961014     C                   SETOFF                                       720473
052600911008     C*
052700911008     C                   ENDSR
052800941216     C**************************************************************************
052900941216     C*    CARICO 1 PAGINA DI SUBFILE VUOTA
053000941216     C**************************************************************************
053100911008     C     PULSFL        BEGSR
053200920706     C                   SETOFF                                       01
053300951115     C* LA DATA ARRIVO MEZZO LA VISUALIZZO LUNGA 6
053400921120     C*
053500930127     C     B             DOWLE     13
053600960205     C                   EXSR      PULRIG
053700941216     C                   WRITE     LR25DSF
053800911008     C                   ADD       1             NRR
053900920316     C                   ADD       1             B                 3 0
054000911008     C                   END
054100911008     C* SALVO L'ULTIMO
054200920316     C                   Z-ADD     NRR           SAVNRR            5 0
054300941216     C*
054400911008     C                   ENDSR
054500941216     C**************************************************************************
054600960205     C*    PULIZIA DI UNA RIGA DI SFL
054700941216     C**************************************************************************
054800960205     C     PULRIG        BEGSR
054900960205     C                   MOVEL     *ZEROS        VI2FVA
055000960205     C                   MOVEL     *BLANKS       VI2LNP
055100000628     C                   MOVE      *BLANKS       VI2PTV
055200960205     C                   MOVEL     *BLANKS       VI2LAI
055300960205     C                   Z-ADD     UDATE6        VI2AED
055400960205     C                   CLEAR                   VI2AEH
055500960205     C                   CLEAR                   SA2AED
055600960205     C                   CLEAR                   SA2AEH
055700960205     C                   Z-ADD     0             VI2DFV
055800960205     C                   CLEAR                   VI2RTC
055900960205     C                   CLEAR                   SAVRTC
056000960205     C                   CLEAR                   VI2DTC
056100960205     C                   CLEAR                   VI2GES
056200960205     C                   CLEAR                   VI2DES
056300960402     C                   CLEAR                   VI2AST
056400960205     C                   CLEAR                   WFORZ
056500960205     C                   CLEAR                   WFORZ2
056600960227     C                   CLEAR                   WFORZ3
056700960205     C                   MOVEL     VIDSTA        VI2STA
056800960205     C                   CLEAR                   VI2VFV
056900960206     C                   CLEAR                   WPRVOL
057000960205     C                   ENDSR
057100960205     C**************************************************************************
057200960205     C*    CONTROLLI FOGLIO VIAGGIO
057300960205     C**************************************************************************
057400960205     C     CONTR1        BEGSR
057500941216     C*
057600911008     C                   SETOFF                                       303190
057700951120     C                   SETOFF                                       09
057800930208     C* CATEGORIA FOGLI
057900930208     C     VIDNPG        IFEQ      ' '
058000930208     C                   SETON                                        5790
058100930208     C                   GOTO      ENDCT1
058200930208     C                   END
058300930208     C     VIDNPG        IFEQ      '?'
058400930208     C                   MOVEL     CODUT         §KUT
058500930208     C                   MOVEL     '7N'          §COD
058600930208     C                   MOVE      ' '           VIDNPG
058700930208     C                   MOVEL     *BLANKS       §KEY
058800930208     C                   CALL      'X§TABER'
058900930208     C                   PARM                    §KUT              1 0
059000930208     C                   PARM                    §COD              2
059100930208     C                   PARM                    §KEY              8
059200930208     C                   PARM                    §DES             30
059300930208     C                   MOVEL     §KEY          VIDNPG
059400040120     C                   SETON                                        90
059500930208     C                   GOTO      ENDCT1
059600930208     C                   END
059700930208     C* CONTROLLO
059800930208     C                   MOVEL     '7N'          COD
059900930208     C                   MOVEL     *BLANKS       KEY
060000930208     C                   MOVEL     VIDNPG        KEY
060100930208     C* INESISTENTE
060200930208     C     KTAB          CHAIN     TABEL                              57
060300930208     C* ANNULLATA
060400930609     C  N57TBLFLG        IFNE      ' '
060500930208     C                   SETON                                        57
060600930208     C                   ELSE
060700930208     C     VIDNPG        IFNE      '2'
060800930208     C                   SETON                                        57
060900930208     C                   END
061000930208     C                   END
061100930208     C* ERRATA
061200930208     C   57              SETON                                        90
061300930208     C   57              GOTO      ENDCT1
061400930208     C*
061500930208     C                   MOVEL     TBLUNI        DS7N
061600930208     C* NON UTILIZZABILE DALLA FILIALE IN GESTIONE
061700930208     C     §7NUTI        IFEQ      '9'
061800930208     C                   SETON                                        5890
061900930208     C                   GOTO      ENDCT1
062000930208     C*
062100930208     C                   ELSE
062200930208     C*
062300930208     C* SOLO DALLA PRIMO LIVELLO UTILIZZABILE
062400930208     C     §7NUTI        IFEQ      '1'
062500941220     C     VIDFGS        ANDNE     SIMFEL
062600930208     C                   SETON                                        5890
062700930208     C                   GOTO      ENDCT1
062800930208     C                   END
062900930208     C     §7NUTI        IFEQ      '2'
063000941220     C     VIDFGS        ANDEQ     SIMFEL
063100930208     C                   SETON                                        5890
063200930208     C                   GOTO      ENDCT1
063300930208     C                   END
063400930208     C                   END
063500930208     C                   MOVEL     VIDNPG        PARNPG
063600930208     C*
063700911008     C* NUMERO FOGLIO VIAGGIO
063800911008     C     VIDNFV        IFEQ      *BLANKS
063900911008     C     VIDNFV        OREQ      *ZEROS
064000911008     C                   SETON                                        4090
064100911008     C                   GOTO      ENDCT1
064200911008     C*
064300911008     C                   ELSE
064400911008     C* RICERCA
064500950328     C     '?'           SCAN      VIDNFV                                 09
064600911010     C   09              DO
064700941221     C                   CLEAR                   DSLR01
064800941221     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
064900941221     C                   MOVEL     PARNPG        DLRNPG                         CATEGORIA FOGLIO
065000941221     C                   MOVEL     '2'           DLRTFV                         TIPO FOGLIO
065100941221     C                   MOVEL     'FNLR25R'     DLRPGM                         NOME PROGRAMMA
065200941221     C                   Z-ADD     31129999      DLRGAL                         AL GG/MM/AAAA
065300941221     C                   Z-ADD     99991231      DLRAAL                         AL AAAA/MM/GG
065400941221     C                   MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
065500941221     C                   MOVEL     DSLR01        KPJBU
065600941221     C                   CALL      'FNLR02R'
065700921120     C                   PARM                    KPJBA
065800941221     C                   MOVEL     KPJBU         DSLR01
065900941221     C                   MOVEL     DLRNFV        VIDNFV
066000911009     C                   SETON                                        90
066100911008     C                   GOTO      ENDCT1
066200911008     C                   END
066300911010     C* CONTROLLI
066400911008     C                   TESTN                   VIDNFV               31
066500930127     C  N31              SETON                                        30
066600911008     C*
066700930127     C  N30              MOVEL     VIDNFV        COMNFV
066800960723     C  N30KFVV1         CHAIN(N)  FNFVV01L                           30
066900970421     C* INESISTENTE
067000970421     C     *IN30         IFEQ      *ON
067100911008     C* ANNULLATO
067200970421     C     FVVATB        ORNE      *BLANKS
067300961010     C                   SETON                                        4090
067400961010     C                   GOTO      ENDCT1
067500911008     C                   END
067600961010     C                   END
067700921120     C*
067800960626     C* IMPOSTO INDICATORE PER PERMETTERE SOLO LA VARIAZ. DI DATA/ORA
067900960626     C* ARRIVO REALE DI FV GIA' ABBINATI SE DATA FOGLIO ARRIVI <= DATA
068000960626     C* ULTIMA ELABORAZIONE IDD
068100990512     C     FVVDFV        IFLE      §7ADID
068200970721     C* ANCHE SE FOGLIO CHIUSO POSSO SOLO MODIFICARE LA DATA E ORA DI
068300961010     C*  ARRIVO
068400961010     C     FVVFCF        OREQ      'S'
068500960626     C                   SETON                                        17
068600960626     C                   ELSE
068700960626     C                   SETOFF                                       17
068800960626     C                   END
068900970721     C**
069000970721     C* CALL TISI96R PER AVERE LA VERSIONE
069100970721     C**
069200970721     C                   MOVEL     FVVDFV        I96DRI
069300970721     C                   CLEAR                   O96VER
069400970721     C                   CALL      'TISI96R'
069500970721     C                   PARM                    I96DRI
069600970721     C                   PARM                    O96VER
069700970721     C**
069800970721     C                   MOVEL     O96VER        KVER
069900930127     C*
070000930127     C* CARICAMENTO SFILE
070100930127     C                   EXSR      CARSFL
070200930127     C*
070300930127     C     ENDCT1        ENDSR
070400941216     C**************************************************************************
070500941216     C*    CARICAMENTO SFILE
070600941216     C**************************************************************************
070700930127     C     CARSFL        BEGSR
070800941216     C*
070900930519     C* DATA FOGLIO ARRIVI
071000920709     C                   MOVE      FVVDFV        G02INV
071100920709     C                   MOVE      *ZEROS        G02DAT
071200920709     C                   MOVEL     3             G02ERR
071300941221     C                   CALL      'XSRDA8'
071400920709     C                   PARM                    WLBDAT
071500920709     C                   Z-ADD     G02DAT        VIDDFA
071600911008     C*
071700960205     C* CAMPO USATO PER IMMETTERE LA DESCRIZIONE RITARDO/ANTICIPO
071800960205     C                   CLEAR                   VI2GES
071900960205     C* CAMPO USATO PER VISUALIZZARE IL FV
072000960205     C                   CLEAR                   VI2VFV                          VISUAL FV
072100960205     C* STAMPA ELENCO SPEDIZIONI NON PARTITE
072200960205     C                   MOVEL     'N'           VI2STA                          STAMPA ANOM
072201141110     c* aggiungo eventuali filiali estere secondi livello della fgs
072202141110     c* per permettere al terminal di abbinare eventuali diretti a filiale estera
072203141110     c                   Clear                   Trul09ds
072204141110     c                   Eval      I09Mod = 'TFU'
072205141110     c                   Eval      I09Tfa = vidfgs
072206141110     c                   Eval      I09dta = fvvdfv
072207141110     c                   Call      'TRUL09R'
072208141110     c                   Parm                    Trul09ds
072209141110     c                   if        mec(1)>*zeros
072210141110     c                   eval      in=%lookup(000:l6)
072211141110     C                   Z-ADD     1             XX                3 0
072212141110     c                   dow       xx<=%elem(mec) and mec(xx)>*zeros and
072213141110     c                             in<=%elem(l6)
072214141110     c* verifico se filiale estera e in questo caso la carico in £6
072215141110     C                   clear                   OG143
072216141110     c     mec(xx)       chain     azorg01l
072217141110     c                   if        %found(azorg01l)
072218141110     c                   movel     orgde3        og143
072219141110     c                   endif
072220141110     c                   if        §ogntw='EEX' or §ogntw='DPD' or §ogntw='FED'
072221141110     c                   eval      l6(in)=mec(xx)
072222141110     c                   add       1             in
072223141110     c                   endif
072224141110     c                   add       1             xx
072225141110     c                   enddo
072226141110     c                   endif
072300960205     C*
072400920428     C* CARICO FOGLI VIAGGIO GIA' ABBINATI SE ESISTONO
072500960318     C* CARICO SOLO QUELLI CON FVALAI = ALLA FIL. GESTIONE FOGLIO
072600911008     C                   Z-ADD     0             NRR
072700911008     C                   MOVEA     *ZEROS        FV
072800980109     C                   MOVEA     *ZEROS        F2
072900980109     C                   Z-ADD     1             XX                3 0
073000980109    1C     L6(XX)        DOWGT     0
073100941221     C     KFVA2         SETLL     FNFVA02L
073200941221     C     KFVA2         READE     FNFVA02L                               30
073300911008     C*
073400980109    2C     *IN30         DOWEQ     '0'
073500960318     C*
073600980109     C                   MOVEL     FVAKEY        W0080             8 0
073700980109     C     W0080         LOOKUP    F2                                     39
073800980109    3C     *IN39         IFEQ      *OFF
073900921119     C                   MOVEL     FVANFV        VI2FVA                          NUM FOGLIO
074000921119     C                   MOVEL     FVALNP        VI2LNP                          LINEA PART
074100000628     C                   MOVEL     FVALAI        VI2LAI                          ARRIVO INV
074200951120     C*
074300960205     C                   CLEAR                   VI2AEH
074400960402     C                   CLEAR                   VI2AST
074500000628     C                   CLEAR                   VI2PTV
074600951120     C                   CLEAR                   VI2RTC
074700951121     C                   CLEAR                   SAVRTC
074800951120     C                   CLEAR                   VI2DTC
074900951120     C                   CLEAR                   VI2DES
075000960222     C                   MOVEL     '1'           WFORZ
075100960222     C                   CLEAR                   WFORZ2
075200960227     C                   CLEAR                   WFORZ3
075300960222     C                   CLEAR                   WPRVOL
075400960205     C*
075500960205     C                   EXSR      DATARR
075600951115     C*
075700911010     C                   ADD       1             NRR
075800921119     C                   Z-ADD     FVAKEY        FV(NRR)
075900980109     C                   MOVEL     FVAKEY        F2(NRR)
076000941216     C                   WRITE     LR25DSF
076100980109    3C                   ENDIF
076200911008     C*
076300941221     C     KFVA2         READE     FNFVA02L                               30
076400980109    2C                   ENDDO
076500980109     C                   ADD       1             XX
076600980109    1C                   ENDDO
076700921118     C*
076800980109    2C     *IN17         IFEQ      *OFF
076900911008     C* FINISCO DI PULIRE IL SUBFILE
077000920316     C                   ADD       1             NRR               5 0
077100930129     C*
077200930129     C     NRR           DIV       13            RIS               4 0
077300930129     C                   MVR                     RESTO             3 0
077400930129     C*
077500930129     C                   Z-ADD     RESTO         B
077600911008     C                   EXSR      PULSFL
077700960626     C                   ELSE
077800960626     C                   Z-ADD     NRR           SAVNRR            5 0
077900961014     C     NRR           COMP      0                                      72
078000960626     C*
078100960626     C                   END
078200930127     C*
078300930127     C                   ENDSR
078400941216     C**************************************************************************
078500941216     C*    CONTROLLO SUB FILE
078600941216     C**************************************************************************
078700911008     C     CTRSFL        BEGSR
078800941216     C*
078900921119     C                   SETOFF                                       904509
079000911009     C                   Z-ADD     1             NRR
079100040120     C                   Z-ADD     0             X                 3 0
079200911011     C                   MOVEA     *ZEROS        FVS
079300920316     C                   MOVEA     *BLANKS       STS
079400921119     C                   MOVEA     *BLANKS       ATS
079500951117     C                   MOVEA     *ZEROS        TRN
079600951117     C                   MOVEA     *ZEROS        DTN
079700040122     C                   MOVEA     *ZEROS        DFV
079800960222     C                   CLEAR                   DHA
079900911009     C*
080000911009     C* CONTROLLO TUTTO IL SUBFILE
080100951109    1C     NRR           DOWLE     SAVNRR
080200941216     C     NRR           CHAIN     LR25DSF                            30
080300911008     C*
080400951109    2C  N30              DO
080500921123     C                   SETOFF                                       01
080600911009     C*
080700911009     C* RICERCA IN TUTTI I FOGLI VIAGGIO
080800960627   2AC     *IN17         IFEQ      *OFF
080900960627     C*
081000950328     C     '?'           SCAN      VI2LNP                                 09
081100951109    3C   09VI2FVA        IFGT      *ZEROS
081200920327     C                   SETON                                        4990
081300920327     C                   SETOFF                                       09
081400920327     C                   GOTO      ENDCTS
081500951109    3C                   END
081600971002     C*
081700971002     C* SE RICHIMAMATO DA LR26 NON POSSO RITORNARCI
081800971002     C   09RICLAN        IFEQ      '6'
081900971002     C                   SETON                                        5090
082000971002     C                   SETOFF                                       09
082100971002     C                   GOTO      ENDCTS
082200971002     C                   ENDIF
082300920327     C*
082400951109    3C   09              DO
082500960205     C                   EXSR      PULRIG
082600941221     C                   CLEAR                   PARA26
082700921119     C                   MOVEA     *ZEROS        FVA
082800941221     C                   MOVEL     'FNLR25R'     P26PGM
082900941221     C                   MOVEL     VIDFGS        P26LAI
083000941221     C                   MOVEL     SIMFEL        P26FEL
083100950104     C                   MOVEL     'N'           P26ABB                         N=NON ABBINATI
083200971002     C* RICHIAMATO DA FNLR25R
083300971002     C                   MOVEL     '5'           P26LAN
083400941221     C                   MOVEL     PARA26        KPJBU
083500941221     C                   CALL      'FNLR26R'
083600911009     C                   PARM                    KPJBA
083700921119     C                   PARM                    FVA
083800941221     C                   MOVEL     KPJBU         PARA26
083900911009     C*
084000911009     C* SE SELEZIONATI FV LI SCRIVO
084100911009     C                   MOVEL     *ZEROS        VI2FVA
084200911010     C                   MOVEL     *ZEROS        VI2LNP
084300941216     C                   UPDATE    LR25DSF
084400911009     C*
084500911009     C                   SUB       1             NRR
084600911009     C                   Z-ADD     1             I                 4 0
084700911009     C*
084800980204     C     I             DOWLE     70
084900980204    4C     FVA(I)        ANDNE     *ZEROS
085000911010     C                   ADD       1             NRR
085100941216     C     NRR           CHAIN     LR25DSF                            30
085200911009     C*
085300911009     C* CREO NUOVA PAGINA DEL SUBFILE
085400920316     C   30              Z-ADD     NRR           COMNRR            5 0
085500920316     C   30              Z-ADD     1             B
085600911009     C   30              EXSR      PULSFL
085700920316     C   30              Z-ADD     COMNRR        NRR
085800941216     C   30NRR           CHAIN     LR25DSF                            30
085900911010     C* MEMORIZZO SOLO SE E' VUOTO ALTRIMENTI SU ALTRA RIGA
086000951109    5C     VI2LNP        IFEQ      *ZEROS
086100911010     C     VI2LNP        OREQ      *BLANKS
086200951109    6C     VI2FVA        IFEQ      0
086300911009     C                   MOVEL     FVA(I)        VI2LNP
086400911009     C                   MOVE      FVA(I)        VI2FVA
086500980109     C* CERCO SULLA L6
086600980109     C                   Z-ADD     1             XX
086700980109    7C     L6(XX)        DOWGT     0
086800960205     C                   MOVEL     VI2LNP        COMLNP
086900980112     C                   MOVEL     L6(XX)        COMLAI
087000960205     C     KFVA1         CHAIN(N)  FNFVA01L                           31
087100980109    8C     *IN31         IFEQ      *OFF
087200980109     C                   MOVEL     L6(XX)        VI2LAI
087300980204     C     VI2KEY        LOOKUP    FVS                                    31
087400980204     C  N31              EXSR      DATARR
087500980204     C  N31              UPDATE    LR25DSF
087600980204     C   31              SUB       1             NRR
087700980204     C                   ADD       1             I
087800980109     C                   Z-ADD     30            XX
087900980109   X8C                   ELSE
088000980109     C                   ADD       1             XX
088100980109    7C                   ENDIF
088200980109    6C                   ENDDO
088300980109     C**
088400951109    6C                   END
088500951109    5C                   END
088600911009     C*
088700951109    4C                   END
088800980204     C**
088900980204     C* SE C'E' MESSAGGIO EMETTO
089000980204     C     P26MSG        IFNE      *BLANKS
089100980204     C                   MOVEL     P26MSG        VCCMSG
089200980204     C                   SETON                                        28
089300980204     C                   CLEAR                   P26MSG
089400980204     C                   ENDIF
089500911009     C* ESCO DA CONTROLLI
089600911009     C                   SETON                                        90
089700951109    4C     NRR           IFLT      1
089800920327     C                   Z-ADD     1             NRR
089900951109    4C                   END
090000951109     C                   GOTO      ENDCTS
090100951109    3C                   END
090200960627     C*
090300960627   2AC                   END
090400911009     C*
090500951109    3C     VI2LNP        IFNE      *ZEROS
090600911010     C     VI2LNP        ANDNE     *BLANKS
090700920709     C     VI2FVA        ORNE      0
090800960202     C*
090900911010     C* LINEA PARTENZA
091000911010     C                   TESTN                   VI2LNP               31
091100911008     C  N31              SETON                                        4490
091200911008     C  N31              GOTO      ENDCTS
091300911008     C*
091400911010     C                   MOVEL     VI2LNP        COMLNP            3 0
091500911010     C*
091600951109    4C     COMLNP        IFEQ      0
091700920709     C     VI2FVA        ANDNE     0
091800911010     C                   SETON                                        4490
091900951109    4C                   END
092000951109    4C     COMLNP        IFNE      0
092100920709     C     VI2FVA        ANDEQ     0
092200911010     C                   SETON                                        4690
092300951109    4C                   END
092400911010     C*
092500911010     C   90              GOTO      ENDCTS
092600911010     C*
092700980109     C                   Z-ADD     1             XX
092800980109     C                   CLEAR                   WTROV
092900980109     C**
093000980109    4C     L6(XX)        DOWGT     0
093100980109     C                   Z-ADD     L6(XX)        COMLAI
093200980112     C                   MOVEL     L6(XX)        VI2LAI
093300980109     C     KFVA1         CHAIN(N)  FNFVA01L                           32
093400980109    5C     *IN32         IFEQ      *ON
093500980109     C                   ADD       1             XX
093600980109   X5C                   ELSE
093700980109     C                   MOVEL     'S'           WTROV             1
093800980109     C                   Z-ADD     30            XX
093900911010     C*
094000921123     C* GIA' ABBINATO CON ALTRO FOGLIO VIAGGIO
094100980109    6C     FVANFA        IFNE      0
094200921123     C     FVANFA        ANDNE     COMNFV
094300030723     c                   movel     MSG(4)        VCCmsg
094400030723     c                   eval      %subst(VCCmsg:54:1) =
094500030723     c                               %trim(%editw(FVAnpg:'0 '))
094600030723     c                   eval      %subst(VCCmsg:58:5) =
094700030723     c                               %trim(%editw(FVAnfa:'0     '))
094800951121     C                   SETON                                        459028
094900921123     C                   GOTO      ENDCTS
095000980109    6C                   END
095100950104     C*
095200950104     C     VI2KEY        LOOKUP    FVS                                    31
095300950104     C* MEMORIZZO NELLA SCHIERA DEI FV SELEZIONATI
095400980109    6C     *IN31         IFEQ      *OFF
095500980109     C                   ADD       1             X
095600980109     C                   Z-ADD     VI2KEY        FVS(X)
095700980109     C                   MOVEL     VI2STA        STS(X)
095800980109     C                   MOVEL     FVAATR        ATS(X)
095900980109     C                   Z-ADD     FVATRN        TRN(X)
096000980109     C                   Z-ADD     FVADTN        DTN(X)
096100040122     C                   Z-ADD     FVADfv        Dfv(X)
096200980109     C                   MOVEA     WDHARR        DHA(X)
096300980109     C                   ELSE
096400950104     C* DOPPIO
096500980109     C                   SETON                                        4890
096600980109     C                   GOTO      ENDCTS
096700980109    6C                   ENDIF
096800950104     C*
096900960205     C                   EXSR      DATARR
097000980109    5C                   ENDIF
097100980109    4C                   ENDDO
097200980109     C** FOGLIO NON TROVATO
097300980109     C     WTROV         IFEQ      *BLANKS
097400980109     C                   SETON                                        4690
097500980109     C                   GOTO      ENDCTS
097600980109     C                   ENDIF
097700950104     C*
097800950104     C* SE IMMESSO VALORE NEGATIVO TRASFORMO IN POSITIVO
097900950104     C                   MLLZO     1             VI2AEH
098000950104     C*
098100950104     C                   MOVEL     VI2AEH        HH                2 0
098200950104     C                   MOVE      VI2AEH        MM                2 0
098300950104     C*
098400951109    4C     HH            IFGT      23
098500960205     C     MM            ORGT      59
098600960205     C     VI2AEH        OREQ      0
098700950104     C                   SETON                                        5690
098800950104     C                   GOTO      ENDCTS
098900951109    4C                   ENDIF
099000930129     C*
099100951114     C* CONTROLLO DATA ARRIVO MEZZO (DATA E ORA SONO OBBLIGATORIE)
099200930129     C                   Z-ADD     VI2AED        G02DAT
099300930129     C                   MOVEL     *BLANKS       G02ERR
099400941221     C                   CALL      'XSRDA8'
099500930129     C                   PARM                    WLBDAT
099600930129     C*
099700951115    4C     G02ERR        IFEQ      '1'
099800930129     C                   SETON                                        5590
099900930129     C                   GOTO      ENDCTS
100000951115    4C                   ENDIF
100100951228     C                   Z-ADD     G02INV        WAED8
100200941221     C*
100300941221     C* DATA ARRIVO NON PUO' ESSERE < DATA FOGLIO
100400951115    4C     G02INV        IFLT      FVADFV
100500930129     C                   SETON                                        5490
100600930129     C                   GOTO      ENDCTS
100700951115    4C                   ENDIF
100800030924      * Per foglio illuminato
100900030924    4c                   If        FvaAtr <> *Blanks
101000030924      * cerco il terminal di arrivo
101100030924     c                   ExSr      RicTfa
101200030924      * se secondo livello in arrivo su altro AS
101300030924    5c                   If        Simfel <> d98Tfa and
101400030924      * Controllo l'ora arrivo reale
101500030924      * non deve essere maggiore dell'ora di abbinamento foglio
101600030924     c                             vi2aeh <> sa2aeh and
101700030924     c                             vi2aeh >  sa2aeh
101800030924     c                   If        sa2aeh > *Zeros
101900030924     c                   Move      sa2aeh        worario
102000030924     c                   Else
102100030924     c                   Move      wtime         worario
102200030924     c                   EndIf
102300030924     c                   Eval      VccMsg = Msg(19)
102400030924     c                   Eval      %subst(VccMsg:72:2) = wore
102500030924     c                   Eval      %subst(VccMsg:75:2) = wminuti
102600030924     c                   Eval      *In28 = *On
102700030924     c                   Eval      *In53 = *On
102800030924     c                   Eval      *In90 = *On
102900030924     c                   GoTo      EndCts
103000030924    5c                   EndIf
103100030924    4c                   EndIf
103200941221     C*
103300960227     C* PULISCO I CAMPI DI FORZATURA SOLO SE VARIANO DATA/ORA ARRIVO
103400960227    4C     SA2AED        IFNE      VI2AED
103500960227     C     SA2AEH        ORNE      VI2AEH
103600960227     C     FVATRN        IFGT      0
103700960227     C                   CLEAR                   WFORZ
103800960227     C                   CLEAR                   WFORZ2
103900960227     C                   ENDIF
104000960227     C*
104100960227     C                   CLEAR                   WFORZ3
104200960227     C                   MOVEL     VI2AED        SA2AED
104300960227     C                   MOVEL     VI2AEH        SA2AEH
104400960227    4C                   ENDIF
104500960227     C*
104600960227     C* DATA/ORA ARRIVO NON POSSONO ESSERE MAGGIORI DELLA DATA/ORA
104700960227     C*   CORRENTE: ENTER PER FORZARE
104800960227    4C     WFORZ3        IFEQ      ' '
104900000606     C* DATA ARRIVO NON PUO' ESSERE > DATA FOGLIO ARRIVI
105000000606    5C     WAED8         IFGT      FVVDFV
105100000606     C                   MOVEL     MSG(20)       VCCMSG
105200000606     C                   SETON                                        289061
105300000606     C                   MOVEL     '1'           WFORZ3
105400000606     C                   GOTO      ENDCTS
105500000606    5C                   ENDIF
105600960227    4C                   ENDIF
105700960227     C*
105800951117     C* CONTROLLO SE PRESENTE   R I T A R D O / A N T I C I P O
105900951116     C                   SETOFF                                       18
106000951115     C* CONTROLLO SE SI TRATTA DI FOGLIO VIAGGIO ABBINATO AD UN TRAINO
106100951116    4C     FVATRN        IFGT      0
106200960205     C*
106300960227     C* SOLO SE F.VIAGGIO NON E' ILLUMINATO
106400960227   5AC     FVAATR        IFEQ      ' '
106500951115     C*
106600951117     C* SE SONO STATI IMMESSI PIU' F.VIAGGIO CON STESSO NUM.TRAINO E
106700960222     C*   DATA TRAINO CONTROLLO ABBIANO LA STESSA DATA/ORA/CAUS. ARRIVO
106800951117     C                   Z-ADD     1             Y                 3 0
106900951120     C     FVATRN        LOOKUP    TRN(Y)                                 35
107000951117    5C     *IN35         IFEQ      *ON
107100951117     C     VI2KEY        ANDNE     FVS(Y)
107200951117     C     FVADTN        ANDEQ     DTN(Y)
107300951121     C     WDHARR        ANDNE     DHA(Y)
107400030723     c                   movel     MSG(7)        VCCmsg
107500030723     c                   eval      %subst(VCCmsg:07:3) = VI2lnp
107600030723     c                   eval      %subst(VCCmsg:11:5) =
107700030723     c                               %trim(%editc(VI2fva:'Z'))
107800030723     c                   z-add     FVS(Y)        WKEY
107900030723     c                   eval      %subst(VCCmsg:19:3) =
108000030723     c                               %trim(%editw(Wlnp:'0   '))
108100030723     c                   eval      %subst(VCCmsg:23:5) =
108200030723     c                               %trim(%editc(Wfva:'Z'))
108300030723     c                   eval      %subst(VCCmsg:40:7) =
108400030723     c                               %editc(FVAtrn:'Z')
108500951117     C                   SETON                                        289061
108600951117     C                   GOTO      ENDCTS
108700951117    5C                   ENDIF
108800951109     C*
108900951128     C*
109000951128     C* CONTROLLO SE ESISTONO ALTRI F.VIAGGIO ABBINATI AD ALTRO F.ARRIVI
109100960222     C*   CON LO STESSO NUMERO E DATA TRAINO MA DIVERSE DATA/ORA/CAUS.
109200960222     C*   ARRIVO: SE ESISTONO EMETTO MESSAGGIO FORZABILE PER AVVISARE
109300951128    5C     WFORZ2        IFEQ      ' '
109400951128     C     KFVA4         SETLL     FNFVA004
109500951128     C     KFVA4         READE     FNFVA004                               34
109600951128    6C     *IN34         DOWEQ     *OFF
109700960202    7C     F04NFA        IFGT      0
109800960202     C     F04NFA        ANDNE     COMNFV
109900960227     C     F04LAI        ANDEQ     FVALAI
110000960202    8C     F04AED        IFNE      WAED8
110100960202     C     F04AEH        ORNE      VI2AEH
110200960222     C     F04RTA        ORNE      VI2RTC
110300030723      * Foglio letto già abbinato con data reale <> di arrivo
110400030723     c                   movel     MSG(10)       VCCmsg
110500030723      * - foglio in errore
110600030723     c                   eval      %subst(VCCmsg:01:3) =
110700030723     c                               %trim(%editw(FVAlnp:'0   '))
110800030723     c                   eval      %subst(VCCmsg:05:5) =
110900030723     c                               %trim(%editc(FVAnfv:'Z'))
111000030723      * - data di arrivo
111100030723     c                   z-add     F04aed        G02inv
111200030723     c                   clear                   G02dat
111300030723     c                   movel     3             G02err
111400030723     c                   call      'XSRDA8'
111500030723     c                   parm                    WLBDAT
111600030723     c                   eval      %subst(VCCmsg:36:5) =
111700030723     c                               %subst(%editc(G02dat:'Y'):1:5)
111800030723      * - ora di arrivo
111900030723     c                   eval      %subst(VCCmsg:42:5) =
112000030723     c                               %editw(F04aeh:'0 :  ')
112100030723     c                   eval      %subst(VCCmsg:48:1) = F04rta
112200030723     c                   eval      %subst(VCCmsg:56:3) =
112300030723     c                               %trim(%editw(F04lnp:'0   '))
112400030723     c                   eval      %subst(VCCmsg:60:5) =
112500030723     c                               %trim(%editc(F04nfv:'Z'))
112600951128     C                   SETON                                        289061
112700951128     C                   MOVEL     '1'           WFORZ2
112800951128     C                   GOTO      ENDCTS
112900951128    8C                   ENDIF
113000951128    7C                   ENDIF
113100951128     C     KFVA4         READE     FNFVA004                               34
113200951128    6C                   ENDDO
113300951128    5C                   ENDIF
113400951128     C*
113500960227   5AC                   ENDIF
113600951115    4C                   ENDIF
113700951114     C*
113800960626     C*
113900960626     C* CONTROLLO ESISTENZA RITARDO SOLO SE IL FV NON E' ILLUMINATO E
114000960626     C*   DATA/ORA TEORICA E' DIVERSA DA DATA/ORA REALE
114100960626    4C     FVADTA        IFGT      0
114200960626    5C     FVADTA        IFNE      WAED8
114300960626     C     FVAHTA        ORNE      VI2AEH
114400960626     C                   CLEAR                   DSTL35
114500960626     C                   MOVEL     'C'           D35TUP
114600970825     C                   MOVEL     'A'           D35EPA
114700960626     C                   MOVEL     FVADTA        D35DTE
114800960626     C                   MOVEL     FVAHTA        D35HTE
114900960626     C                   MOVEL     WAED8         D35DRE
115000960626     C                   MOVEL     VI2AEH        D35HRE
115100960626     C                   MOVEL     DSTL35        KPJBU
115200960626     C                   CALL      'TNTL35R'
115300960626     C                   PARM                    KPJBA
115400960626     C                   MOVEL     KPJBU         DSTL35
115500960626     C                   MOVEL     '1'           WTL35             1
115600960626     C*
115700960626     C* D35FLG <> " "  ---> PRESENTE RITARDO/ANT. NELL'ARRIVO DEL MEZZO
115800960626    6C     D35FLG        IFNE      ' '
115900960626     C                   SETON                                        18
116000960626    6C                   ENDIF
116100960626    5C                   ENDIF
116200960626     C*
116300960626   X4C                   ELSE
116400960626     C*
116500960626     C* SE FV ILLUMINATO
116600960626    5C     FVAATR        IFNE      ' '
116700960626     C* CALCOLO I TEMPI DI TRAZIONE ED IN BASE ALLE SOGLIE CALCOLO
116800960626     C*   L'ORA AL DI SOPRA DELLA QUALE CHIEDERE IL RITARDO
116900960626     C                   EXSR      CALSOG
117000960626     C*
117100960626     C* CONTROLLO SE PRESENTE RITARDO
117200960626     C                   SELECT
117300960626     C     WAED8         WHENEQ    DATSOG
117400960626    6C     VI2AEH        ANDGT     ORASOG
117500960626     C                   SETON                                        18
117600960626     C*
117700960626     C     WAED8         WHENGT    DATSOG
117800960626     C                   SETON                                        18
117900960626     C                   ENDSL
118000960626     C*
118100960626     C                   ELSE
118200960626     C                   MOVEL     '1'           WFORZ
118300960626    5C                   ENDIF
118400960626    4C                   ENDIF
118500960626     C*
118600960626     C* 17 ON -  DETERMINO SE CAUSALE RITARDO MANUTENZIONABILE
118700960626     C                   MOVE      *BLANKS       WCOSA             1            TUTTO
118800960626    4C     *IN17         IFEQ      *ON
118900960626    5C     *IN18         IFEQ      *ON
119000960626    6C     SAVRTC        IFNE      *BLANKS
119100960626     C                   MOVE      'N'           WCOSA                          NESSUNA VAR.
119200960626    6C                   END
119300960626   X5C                   ELSE
119400960626    6C     SAVRTC        IFNE      *BLANKS
119500960626     C                   MOVE      'T'           WCOSA                          SOLO TOGLIER
119600960626   X6C                   ELSE
119700960626     C                   MOVE      'N'           WCOSA                          NESSUNA VAR.
119800960626    6C                   END
119900960626    5C                   END
120000960626    4C                   END
120100960626     C*
120200960626    4C                   SELECT
120300960626     C* WCOSA='N' --> CAUSALE/DESCRIZIONI RITARDO NON MANUTENZIONABILI
120400960626     C     WCOSA         WHENEQ    'N'
120500960626    5C     VI2RTC        IFNE      SAVRTC
120600960626    6C     SAVRTC        IFNE      *BLANKS
120700960626     c* Impossibile modificare o togliere la causale
120800030723     c                   movel     MSG(14)       VCCmsg
120900030723     c                   eval      %subst(VCCmsg:71:1) = SAVrtc
121000960626   x6C                   ELSE
121100960626     c* Impossibile immettere la causale
121200960626     C                   MOVEL     MSG(15)       VCCMSG
121300960626    6C                   END
121400960626     C                   SETON                                        906028
121500960626     C                   GOTO      ENDCTS
121600960626    5C                   END
121700960626    5C     VI2GES        IFNE      *BLANKS
121800960626     c* impossibile modificare la descrizione
121900960626     C                   MOVEL     MSG(16)       VCCMSG
122000960626     C                   SETON                                        906228
122100960626     C                   GOTO      ENDCTS
122200960626    5C                   END
122300960626     C* WCOSA='T' --> LA CAUSALE PUO' SOLO ESSERE ABBLENCATA
122400960626     C     WCOSA         WHENEQ    'T'
122500960626    5C     VI2RTC        IFNE      *BLANKS
122600960626     C     VI2RTC        ANDNE     SAVRTC
122700030723     c                   movel     MSG(17)       VCCmsg
122800030723     c                   eval      %subst(VCCmsg:66:1) = SAVrtc
122900960626     C                   SETON                                        906028
123000960626     C                   GOTO      ENDCTS
123100960626    5C                   END
123200960626    4C                   ENDSL
123300960626     C*
123400951115     C* RICERCA CAUSALE RITARDO/ANTICIPO
123500951115    4C     VI2RTC        IFEQ      '?'
123600951116     C                   MOVEL     'TC'          §COD
123700951115     C                   MOVEL     *BLANKS       VI2RTC
123800951116     C                   MOVEL     *BLANKS       §DES
123900951115     C                   CALL      'X§TABER'
124000951115     C                   PARM                    CODUT
124100951116     C                   PARM                    §COD
124200951116     C                   PARM                    §KEY
124300951116     C                   PARM                    §DES
124400951116     C                   MOVEL     §KEY          VI2RTC
124500951116     C                   MOVEL     §DES          VI2DTC
124600960207     C                   SETON                                        906045
124700951115     C                   GOTO      ENDCTS
124800951115    4C                   ENDIF
124900951115     C*
125000951115     C* CONTROLLO VALIDITA' CAUSALE
125100951115    4C     VI2RTC        IFNE      ' '
125200951115     C                   MOVEL     'TC'          COD
125300951115     C                   MOVEL(P)  VI2RTC        KEY
125400951115     C     KTAB          CHAIN     TABEL                              34
125500951115    5C     *IN34         IFEQ      *OFF
125600951115     C     TBLFLG        ANDNE     ' '
125700951115     C                   SETON                                        34
125800951115    5C                   ENDIF
125900951115     C*
126000951115    5C     *IN34         IFEQ      *ON
126100951115     C                   MOVEL     MSG(1)        VCCMSG
126200951115     C                   SETON                                        289060
126300951115     C                   GOTO      ENDCTS
126400951115   X5C                   ELSE
126500951115     C                   MOVEL     TBLUNI        VI2DTC
126600951115    5C                   ENDIF
126700951115     C*
126800951115   X4C                   ELSE
126900951115     C                   CLEAR                   VI2DTC
127000951115    4C                   ENDIF
127100960205     C*
127200951115     C* SE ARRIVO IN RITARDO/ANTICIPO --> CAUSALE OBBLIGATORIA
127300951115    4C     *IN18         IFEQ      *ON
127400951115     C     VI2RTC        ANDEQ     ' '
127500951228     C                   MOVEL     MSG(2)        VCCMSG
127600951115     C                   SETON                                        289060
127700951115     C                   GOTO      ENDCTS
127800951115    4C                   ENDIF
127900951115     C*
128000951115     C* SE MODIFICANDO LA DATA/ORA ARRIVO EFFETTIVA IL RITARDO/ANTICIPO
128100951115     C*   SCOMPARE, CHIEDERE FORZABILE DI ELIMINARE LA CAUSALE
128200951228    4C  N18WFORZ         IFEQ      ' '
128300960202     C     VI2RTC        ANDNE     ' '
128400951115     C                   MOVEL     MSG(3)        VCCMSG
128500951115     C                   SETON                                        289060
128600951228     C                   MOVEL     '1'           WFORZ
128700951115     C                   GOTO      ENDCTS
128800951115    4C                   ENDIF
128900951115     C*
129000951115     C* SE IMMESSA LA "G" DI CAUSALE GENERICA, RICHIEDERE OBBLIGATORIA
129100951115     C*   LA DESCRIZIONE RITARDO/ANTICIPO
129200951115    4C     VI2RTC        IFEQ      'G'
129300951115     C* W001A = " " --> NON ESISTE LA DESCRIZIONE RITARDO/ANTICIPO
129400960205     C     VI2DES        ANDEQ     *BLANKS
129500951115     C     VI2GES        ANDEQ     ' '
129600951228     C                   MOVEL     MSG(5)        VCCMSG
129700960205     C                   SETON                                        289062
129800951115     C                   GOTO      ENDCTS
129900951115    4C                   ENDIF
130000951115     C*
130100951115     C* CONTROLLO SE SI VUOLE INSERIRE LA DESCRIZIONE RITARDO/ANTICIPO
130200951115    4C     VI2GES        IFNE      ' '
130300960201     C                   CLEAR                   DSLV15
130400960201     C                   MOVEL     'M'           D15TUP
130500960201     C                   MOVEL     'A'           D15EPA
130600960201     C                   MOVEL     VI2RTC        D15RTC
130700960201     C                   MOVEL     FVALNP        D15LNP
130800960201     C                   MOVEL     FVANFV        D15NFV
130900960201     C                   MOVEL     FVALAI        D15LAI
131000960201     C                   MOVEL     'R'           D15TRC
131100960201     C                   MOVEL     FVATRN        D15TRN
131200960222     C                   MOVEL     FVATTR        D15TTR
131300960201     C                   MOVEL     FVADFV        D15DFV
131400960305     C                   MOVEL     FVAATR        D15ATR
131500951115     C*
131600960201     C                   MOVEL     DSLV15        KPJBU
131700951115     C                   CALL      'FNLV15R'
131800951115     C                   PARM                    KPJBA
131900960201     C                   MOVEL     KPJBU         DSLV15
132000960202     C                   MOVEL     '1'           WLV15             1
132100951115     C*
132200960201     C* SE VARIATO QUALCOSA IN FNFV4 ED ESCO DALL'ABBINAMENTO SENZA
132300960201     C*   CONFERMARE SOTTOMETTO PGM DI TRASMISSIONE
132400960201    5C     D15FLG        IFNE      ' '
132500960201     C                   MOVEL     '1'           WAGG              1
132600951115    5C                   ENDIF
132700951121     C*
132800951121     C* PULISCO IL CAMPO UTILIZZATO PER VISUALIZZARE LA DESCRIZIONE
132900951121     C                   CLEAR                   VI2GES
133000951115     C*
133100951115     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
133200951115     C*   RIEMPIO CAMPO CHE NE INDICA L'ESISTENZA
133300951115     C                   EXSR      CTRDRA
133400960201    5C     W001A         IFNE      ' '
133500951130     C                   MOVEL     W010A         VI2DES
133600960205   X5C                   ELSE
133700960205     C                   CLEAR                   VI2DES
133800960205    5C                   ENDIF
133900960205    4C                   ENDIF
134000951121     C*
134100951121     C* SE INSERITA LA DESCRIZIONE RITARDO/ANTICIPO OCCORRE INSERIRE
134200951121     C*   ANCHE LA CAUSALE
134300960205    4C     VI2DES        IFNE      *BLANKS
134400960205     C     VI2RTC        ANDEQ     ' '
134500951228     C                   MOVEL     MSG(6)        VCCMSG
134600951121     C                   SETON                                        289060
134700951121     C                   GOTO      ENDCTS
134800960205    4C                   ENDIF
134900951115     C*
135000951117     C* CONTROLLO SE RICHIESTA LA VISUALIZZAZIONE DEL F.VIAG. PARTENZA
135100951117    4C     VI2VFV        IFEQ      'V'
135200971002     C* NON POSSO ANDARCI SE PROVENGO DA FNLR26
135300971002     C     RICLAN        IFEQ      '6'
135400971002     C                   MOVEL     MSG(18)       VCCMSG
135500971003     C                   SETON                                        289063
135600971002     C                   GOTO      ENDCTS
135700971002     C                   ENDIF
135800971002     C**
135900951120     C                   CLEAR                   PARA26
136000951120     C                   Z-ADD     VI2FVA        FVA(1)
136100951120     C                   MOVEL     'FNLR25R'     P26PGM
136200951120     C                   MOVEL     VI2LNP        P26LNP
136300951120     C                   MOVEL     VI2LAI        P26LAI
136400971002     C* RICHIAMATO DA FNLR25R
136500971002     C                   MOVEL     '5'           P26LAN
136600951120     C                   MOVEL     PARA26        KPJBU
136700951120     C                   CALL      'FNLR26R'
136800951120     C                   PARM                    KPJBA
136900951120     C                   PARM                    FVA
137000951120     C*
137100951120     C* PULISCO IL CAMPO UTILIZZATO PER VISUALIZZARE IL FOGLIO
137200951120     C                   CLEAR                   VI2VFV
137300951117    4C                   ENDIF
137400951109     C*
137500951109   X3C                   ELSE
137600960205     C                   EXSR      PULRIG
137700951109    3C                   END
137800951109    2C                   END
137900930127     C*
138000911009     C     ENDCTS        TAG
138100920706     C*
138200941216     C  N09
138300941216     CANN30              UPDATE    LR25DSF
138400920316     C   90              Z-ADD     NRR           REC
138500920316     C   90              Z-ADD     SAVNRR        NRR
138600911009     C                   ADD       1             NRR
138700951109    1C                   END
138800911008     C*
138900911009     C                   ENDSR
139000960304     C**************************************************************************
139100960304     C*  CALCOLO DATA ORA DI SOGLIA PER I FV ILLUMINATI
139200960304     C**************************************************************************
139300960304     C     CALSOG        BEGSR
139400970925     C* PRIMA FACCIO LA CALL AL TNSD99R PER AVERE I TEMPI DI TRAZIONE
139500970925     C                   CLEAR                   DSSD99
139600960304     C                   MOVEL     'T'           D98TLA
139700960304     C                   MOVEL     FVALNP        D98LNP
139800960304     C                   MOVEL     FVALNP        D98TFP
139900960304     C                   MOVEL     FVALAI        D98LNA
140000960304     C                   MOVEL     FVADFV        D98DSP
140100960304     C                   MOVEL     'E'           D98TSP
140200960304     C* CAP
140300960304     C                   CLEAR                   KNAR
140400960304     C                   MOVEL     FVALAI        KLNA
140500970721     C     KCAP          SETLL     AZCPC03L
140600970721     C     KCAP          READE     AZCPC03L                               32
140700960304     C*
140800960304    1C     *IN32         DOWEQ     *OFF
140900960304     C* SE ANNULLATO O FITTIZIO RILEGGO
141000970721     C     CPCATB        IFNE      ' '
141100970722     C     CPCFIT        OREQ      'S'
141200980109     C                   CLEAR                   CPCCAP
141300970721     C     KCAP          READE     AZCPC03L                               32
141400960304     C                   ELSE
141500960304     C                   SETON                                        32
141600960304     C                   ENDIF
141700960304    1C                   ENDDO
141800980109     C*
141900980109     C* SE NON LO TROVO LO PRENDO DA ORGANIGRAMMA
142000980109     C     CPCCAP        IFEQ      *BLANKS
142100980109     C     FVALAI        CHAIN     AZORG01L                           30
142200980109     C  N30              MOVEL     ORGCPF        D98CAD
142300980109     C                   ELSE
142400970721     C                   MOVEL     CPCCAP        D98CAD
142500980109     C                   ENDIF
142600980109     C**
142700970825     C* CERCO TERMINAL DI ARRIVO
142800960304     C                   EXSR      RICTFA
142900960304     C*
143000970925     C                   CALL      'TNSD99R'
143100970925     C                   PARM                    DSSD99
143200960304     C*
143300960304     C* REPERISCO LE DUE ORE DI SOGLIA
143400960304     C                   MOVEL     '1X'          COD
143500960304     C                   MOVEL(P)  FVALAI        KEY
143600960304     C                   MOVE      'OSA  '       KEY
143700960304     C     KTAB          CHAIN     TABEL                              32
143800980209     C     *IN32         IFEQ      *OFF
143900960304     C  N32              MOVEL     TBLUNI        DS1X
144000960304     C                   MOVEL     §1XHHO        HHOUNO
144100960304     C*
144200960304     C                   MOVE      'OSA1 '       KEY
144300960304     C     KTAB          CHAIN     TABEL                              32
144400960304     C  N32              MOVEL     TBLUNI        DS1X
144500960304     C   32              CLEAR                   §1XHHO
144600960304     C                   MOVEL     §1XHHO        HHODUE
144700980209     C                   ELSE
144800980209     C                   MOVEL(P)  '   '         KEY
144900980209     C                   MOVE      'OSA  '       KEY
145000980209     C     KTAB          CHAIN     TABEL                              32
145100980209     C  N32              MOVEL     TBLUNI        DS1X
145200980209     C   32              CLEAR                   §1XHHO
145300980209     C                   MOVEL     §1XHHO        HHOUNO
145400980209     C*
145500980209     C                   MOVE      'OSA1 '       KEY
145600980209     C     KTAB          CHAIN     TABEL                              32
145700980209     C  N32              MOVEL     TBLUNI        DS1X
145800980209     C   32              CLEAR                   §1XHHO
145900980209     C                   MOVEL     §1XHHO        HHODUE
146000980209     C                   ENDIF
146100960304     C*
146200960304     C*
146300960304    1C                   SELECT
146400960304     C     D98HTI        WHENLE    HHOUNO
146500960304     C                   Z-ADD     HHOUNO        ORASOG
146600960304     C                   Z-ADD     D98DTI        DATSOG
146700960304     C*
146800960304     C     D98HTI        WHENLE    HHODUE
146900960304     C                   Z-ADD     HHODUE        ORASOG
147000960304     C                   Z-ADD     D98DTI        DATSOG
147100960304     C*
147200960304   X1C                   OTHER
147300960304     C*
147400960304     C* SOMMO 1 GIORNO E PRENDO LA PRIMA SOGLIA DEL GIORNO DOPO
147500960304     C                   CLEAR                   WLBDAT
147600960304     C                   MOVEL     '3'           G02ERR
147700960304     C                   Z-ADD     D98DTI        G02INV
147800960304     C                   CALL      'XSRDA8'
147900960304     C                   PARM                    WLBDAT
148000960304     C*
148100960304     C* ADD 1 A GIORNI
148200960304     C     G02TGI        ADD       1             GIOTGI
148300960304     C                   CALL      'XSRGI8'
148400960304     C                   PARM                    WGIDAT
148500960304     C                   MOVEL     GIOINV        DATSOG
148600960304     C                   Z-ADD     HHOUNO        ORASOG
148700960304    1C                   ENDSL
148800960304     C                   ENDSR
148900960304     C**************************************************************************
149000960304     C*  PREPARA MESSAGGIO 10
149100960304     C**************************************************************************
149200960304     C     RICTFA        BEGSR
149300970825     C                   CLEAR                   DSLV55
149400970825     C                   MOVEL     'A'           D55TPT
149500970825     C                   MOVEL     FVALAI        D55LIN
149600970825     C                   MOVEL     FVALNP        D55LNP
149700970825     C                   MOVEL     FVADFV        D55DRF
149800970825     C                   CALL      'FNLV55R'
149900970825     C                   PARM                    DSLV55
150000970825     C     D55ERR        IFEQ      ' '
150100970825     C                   MOVEL     D55TFA        D98TFA
150200970825     C                   ELSE
150300970825     C                   MOVEL     FVALAI        D98TFA
150400960304    2C                   ENDIF
150500960304     C*
150600960304     C                   ENDSR
150700951115     C**************************************************************************
150800951115     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
150900951115     C**************************************************************************
151000951115     C     CTRDRA        BEGSR
151100951115     C                   CLEAR                   W001A
151200951130     C                   CLEAR                   W010A
151300951115     C*
151400960202     C     KFV4          SETLL     FNFV401L                               34
151500951115    1C     *IN34         IFEQ      *ON
151600070918     C     KFV4          READE     FNFV401L                               34
151700951115    2C     *IN34         DOWEQ     *OFF
151800951115    3C     FV4ATB        IFEQ      ' '
151900951116     C                   MOVEL     '1'           W001A             1
152000951130     C                   MOVEL     FV4NOT        W010A            10
152100951115     C                   SETON                                        34
152200951115   X3C                   ELSE
152300070918     C     KFV4          READE     FNFV401L                               34
152400951115    3C                   ENDIF
152500951115    2C                   ENDDO
152600951115    1C                   ENDIF
152700951115     C*
152800951115     C                   ENDSR
152900960111     C**************************************************************************
153000960111     C* CONTROLLO COSA PROPORRE NELLA DATA/ORA ARRIVO REALE
153100000628     C* ... carica anche P.O. gestione POSTE
153200960111     C**************************************************************************
153300960111     C     DATARR        BEGSR
153400960205     C****
153500960205     C** D A T A / O R A     A R R I V O
153600960205     C****
153700960205     C* SE NON HO DIGITATO NULLA VERIFICO COSA DEVO PROPORRE
153800960205    1C     VI2AEH        IFEQ      0
153900960205     C*
154000960205     C* SE PRESENTE PROPONGO DATA/ORA ARRIVO REALE IN QUANTO SI TRATTA
154100960205     C*   DI UN FV CHE HO GIA' RICEVUTO IN PRECEDENZA
154200960205    2C     FVAAED        IFGT      0
154300960205     C* DATA ARRIVO REALE
154400960205     C                   Z-ADD     FVAAED        G02INV
154500960205     C                   MOVE      *ZEROS        G02DAT
154600960205     C                   MOVEL     3             G02ERR
154700960205     C                   CALL      'XSRDA8'
154800960205     C                   PARM                    WLBDAT
154900960205     C* LA DATA ARRIVO MEZZO LA VISUALIZZO LUNGA 6
155000960205     C                   MOVE      G02DAT        W0020
155100960205     C                   MOVEL     G02DAT        VI2AED
155200960205     C                   MOVE      W0020         VI2AED
155300960205     C                   MOVEL     VI2AED        SA2AED
155400960205     C* ORA ARRIVO REALE
155500960205     C                   MOVEL     FVAAEH        VI2AEH
155600960205     C                   MOVEL     FVAAEH        SA2AEH
155700960205     C*
155800960205   X2C                   ELSE
155900960205     C*
156000960111     C* PER I FV ABBINATI A TRAINO PROPORRE LA DATA TEORICA; PER GLI
156100960206     C*   ALTRI PROPORRE LA DATA DEL GIORNO CON ORA 00:00
156200960205    3C     FVADTA        IFGT      0
156300960205     C* DATA ARRIVO TEORICA
156400960111     C                   Z-ADD     FVADTA        G02INV
156500960111     C                   MOVE      *ZEROS        G02DAT
156600960111     C                   MOVEL     3             G02ERR
156700960111     C                   CALL      'XSRDA8'
156800960111     C                   PARM                    WLBDAT
156900960205     C                   MOVE      G02DAT        W0020
157000960205     C                   MOVEL     G02DAT        VI2AED
157100960111     C                   MOVE      W0020         VI2AED
157200960205     C                   MOVEL     VI2AED        SA2AED
157300960111     C* ORA ARRIVO TEORICA
157400960111     C                   Z-ADD     FVAHTA        VI2AEH
157500960205     C                   MOVEL     FVAHTA        SA2AEH
157600960206     C*
157700960206   X3C                   ELSE
157800960206     C* SE FV ILLUMINATO NON PROPONGO COME ORA 00:00 MA PROPONGO TIME
157900960206    4C     FVAATR        IFNE      ' '
158000960206     C                   MOVEL     WTIME         VI2AEH
158100960206     C                   MOVEL     VI2AEH        SA2AEH
158200960206    4C                   ENDIF
158300960205    3C                   ENDIF
158400960205    2C                   ENDIF
158500960205    1C                   ENDIF
158600960111     C*
158700960205     C****
158800960205     C** C A U S A L E     R I T A R D O
158900960205     C****
159000960205     C* SE NON HO DIGITATO NULLA VERIFICO SE C'E' QUALCOSA DA PROPORRE
159100960205    1C     VI2RTC        IFEQ      ' '
159200960206     C     WPRVOL        ANDEQ     ' '
159300960205     C*
159400960205     C* SE PRESENTE VISUALIZZO LA CAUSALE RITARDO/ANTICIPO
159500960205     C     FVARTA        ANDNE     ' '
159600960111     C                   MOVEL     FVARTA        VI2RTC
159700960111     C                   MOVEL     FVARTA        SAVRTC
159800960111     C* DECODIFICA
159900960111     C                   MOVEL     'TC'          COD
160000960111     C                   MOVEL(P)  VI2RTC        KEY
160100960111     C     KTAB          CHAIN     TABEL                              34
160200960205    2C     *IN34         IFEQ      *OFF
160300960111     C     TBLFLG        ANDEQ     ' '
160400960111     C                   MOVEL     TBLUNI        VI2DTC
160500960205    2C                   ENDIF
160600960206     C*
160700960206     C                   MOVEL     '1'           WPRVOL
160800960205    1C                   ENDIF
160900960205     C*
161000960205     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
161100960205     C                   EXSR      CTRDRA
161200960205     C* W001A = " " --> NON ESISTE LA DESCRIZIONE RITARDO/ANTICIPO
161300960205    2C     W001A         IFNE      ' '
161400960205     C                   MOVEL     W010A         VI2DES
161500960205    2C                   ENDIF
161600960202     C*
161700960202     C* GIRO DATA PARTENZA
161800960202     C                   MOVE      FVADFV        G02INV
161900960202     C                   MOVE      *ZEROS        G02DAT
162000960202     C                   MOVEL     3             G02ERR
162100960202     C                   CALL      'XSRDA8'
162200960202     C                   PARM                    WLBDAT
162300971223     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
162400960202     C                   MOVEL     G02DAT        VI2DFV
162500960202     C*
162600960202     C* SE E' SOLO BUSTA METTO CAMPO IN R.I.
162700960202    1C     FVAATR        IFEQ      'T'
162800960202     C                   SETON                                        01
162900960202   X1C                   ELSE
163000960202     C                   SETOFF                                       01
163100960202    1C                   END
163200960402     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
163300960402     C*  VISUALIZZO CON 2 ASTERISCHI
163400960402     C                   MOVEL     VI2LNP        COMLNP
163500960402     C     KARB          SETLL     FNARB06L                               34
163600960402     C     *IN34         IFEQ      *ON
163700960402     C                   CLEAR                   VI2AST
163800960402     C                   ELSE
163900960402     C                   MOVEL     '**'          VI2AST
164000960402     C                   ENDIF
164100000628     C* P.O.GESTIONE
164200000628     C     KFWA1         CHAIN     FNFWA01L                           74
164300000628     C     *IN74         IFEQ      *OFF
164400000628     C     FWAFGS        ANDNE     0
164500000628     C                   Z-ADD     FWAFGS        WORKPT            3 0
164600000628     C                   ELSE
164700000628     C                   Z-ADD     FVALNP        WORKPT            3 0
164800000628     C                   ENDIF
164900000628     C                   CLEAR                   OG143
165000000628     C     WORKPT        CHAIN     AZORG                              74
165100000628     C   74              MOVE      '  '          VI2PTV
165200000628     C  N74              MOVEL     ORGDE3        OG143
165300020207     C  N74§OGNTW        IFEQ      'PPT'
165400000628     C                   MOVE      'PT'          VI2PTV
165500000628     C                   ELSE
165600000628     C                   MOVE      '  '          VI2PTV
165700000628     C                   ENDIF
165800960111     C*
165900960111     C                   ENDSR
166000951121     C**************************************************************************
166100951121     C*    CONTROLLO F03 / F12
166200951121     C**************************************************************************
166300951121     C     F03F12        BEGSR
166400951121     C                   SETOFF                                       90
166500951121     C* SE IMMESSA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12 O
166600951121     C*   F3 A MENO CHE NON ESISTEVA GIA' LA CAUSALE
166700951121     C                   Z-ADD     1             NRR
166800951121     C     NRR           CHAIN     LR25DSF                            34
166900951121    1C     *IN34         DOWEQ     *OFF
167000951121     C*
167100951121    2C     VI2LNP        IFNE      *ZEROS
167200951121     C     VI2LNP        ANDNE     *BLANKS
167300951121     C*
167400951121     C* SE IMMESSA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12 O
167500951121     C*   F3 A MENO CHE NON ESISTEVA GIA' LA CAUSALE
167600951121    3C     VI2DES        IFNE      *BLANKS
167700951121    4C     SAVRTC        IFEQ      ' '
167800951121    5C     VI2RTC        IFEQ      ' '
167900951121     C                   MOVEL     MSG(6)        VCCMSG
168000960205     C                   SETON                                        286290
168100951121     C                   GOTO      ENDF
168200951121   X5C                   ELSE
168300951121     C                   MOVEL     MSG(8)        VCCMSG
168400960205     C                   SETON                                        286290
168500951121     C                   GOTO      ENDF
168600951121    5C                   ENDIF
168700951121    4C                   ENDIF
168800951121     C*
168900951121   X3C                   ELSE
169000951121     C* SE ELIMINATA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12
169100951121     C*   O F3 SE LA CAUSALE CHE E' GIA' STATA IMMESSA E' "G-GENERICA"
169200951121    4C     SAVRTC        IFEQ      'G'
169300951121     C                   MOVEL     MSG(9)        VCCMSG
169400960205     C                   SETON                                        286290
169500951121     C                   GOTO      ENDF
169600951121    4C                   ENDIF
169700951121    3C                   ENDIF
169800951121    2C                   ENDIF
169900951121     C*
170000951121     C     ENDF          TAG
170100960205     C* ERRORE
170200960205     C     *IN90         IFEQ      *ON
170300960205     C                   MOVEL     VI2LNP        COMLNP
170400960205     C                   MOVEL     VI2LAI        COMLAI
170500960205     C     KFVA1         CHAIN(N)  FNFVA01L                           32
170600960205     C  N32FVAATR        IFEQ      ' '
170700960205     C                   SETOFF                                       01
170800960205     C                   ELSE
170900960205     C                   SETON                                        01
171000960205     C                   ENDIF
171100960205     C**
171200960205     C                   Z-ADD     NRR           REC
171300960205     C                   SETON                                        34
171400951121     C                   UPDATE    LR25DSF
171500960205     C                   ELSE
171600951121     C*
171700951121     C                   ADD       1             NRR
171800960205     C     NRR           CHAIN     LR25DSF                            34
171900960205     C                   ENDIF
172000951121    1C                   ENDDO
172100951121     C*
172200951121     C                   ENDSR
172300951121     C*
172400941216     C**************************************************************************
172500941216     C*    ELABORAZIONE ABBINAMENTO
172600941216     C**************************************************************************
172700911008     C     ELAB          BEGSR
172800911010     C*
172900930129     C                   Z-ADD     1             NRR
173000950328     C* CAMPI FISSI DEL FILE FNAGB PER AGGIORNAMENTO BOLLE
173100950328     C                   MOVEL     'A'           AGBTBO
173200950328     C                   MOVEL     'AB'          AGBAGB
173300950925     C                   MOVEL     'S'           AGBFBS
173400950925     C                   MOVEL     ' '           AGBFVC
173500930129     C*
173600950621     C* LEGGO TUTTI I FOGLI VIAGGIO ABBINATI
173700951228    1C     NRR           DOWLE     SAVNRR
173800941216     C     NRR           CHAIN     LR25DSF                            30
173900930129     C*
174000951228    2C  N30VI2LNP        IFGT      *ZEROS
174100070917     C                   SETOFF                                       1615
174200951228     C*
174300930129     C                   MOVEL     VI2LNP        COMLNP
174400951227     C* GIRO DATA ARRIVO
174500951227     C                   Z-ADD     VI2AED        G02DAT
174600951227     C                   Z-ADD     *ZEROS        G02INV
174700951227     C                   CALL      'XSRDA8'
174800951227     C                   PARM                    WLBDAT
174900951227     C                   Z-ADD     G02INV        WAED8
175000960304     C* L'AGGIORNAMENTO DEI FV LO ESEGUO PER TUTTI I FV DELLA LNA
175100960304     C*  DELLA L6
175200960304     C                   Z-ADD     1             Y                 3 0
175300960304   2AC     L6(Y)         DOWGT     0
175400960304     C                   MOVEL     L6(Y)         COMLAI
175500921120     C*
175600921120     C* CHAIN FOGLIO PARTENZE PER AGGIONARE DATA E NUM FOGLIO ABBINATO
175700941220     C* E IMPOSTO LINEE ARRIVO E SCARICO DEL FOGLIO
175800941221     C     KFVA1         CHAIN     FNFVA01L                           33
175900960304   2BC     *IN33         IFEQ      *OFF
176000951228     C*
176100960108     C* 16 ON  - E' STATA MODIFICATA LA DATA E ORA ARRIVO REALE OPPURE
176200960108     C*   SI TRATTA DI FOGLIO VIAGGIO CON NUMERO FOGLIO ARRIVI = 0
176300951228    3C     FVAAED        IFNE      WAED8
176400951228     C     FVAAEH        ORNE      VI2AEH
176500960108     C     FVANFA        OREQ      0
176600951228     C                   SETON                                        16
176700070917     c* Memorizzo se cambiata solo la data/ora
176800070918     c                   if        fvanfa>0
176900070918     c                   movel     fvaaed        woldaed
177000070918     c                   movel     fvaaeh        woldaeh
177100070917     C                   SETON                                        15
177200070917     c                   endif
177300951228    3C                   ENDIF
177400951228     C*
177500951228     C* SFLAGGO I FOGLI VIAGGIO SOLO SE HO MODIFICATO DATA/ORA ARRIVO
177600960108     C*   REALE OPPURE LA CAUSALE RITARDO/ANTICIPO OPPURE SE SI TRATTA
177700960108     C*   DI FOGLIO VIAGGIO CON NUMERO FOGLIO ARRIVI = 0
177800951228     C     *IN16         IFEQ      *ON
177900951228     C     FVARTA        ORNE      VI2RTC
178000960201     C* SE IL FOGLIO VIAGGIO E' ILLUMINATO NON LO SFLAGGO PERCHE'
178100960201     C*   NON E' DA TRASMETTERE
178200960201     C     FVAATR        IFEQ      ' '
178300951228     C                   CLEAR                   FVAFTR
178400951228     C                   CLEAR                   FVADTR
178500960201     C                   ENDIF
178600951228     C                   ENDIF
178700951228     C*
178800951227     C                   Z-ADD     WAED8         FVAAED
178900930127     C                   Z-ADD     VI2AEH        FVAAEH
179000951128     C                   MOVEL     VI2RTC        FVARTA
179100921120     C                   MOVEL     VIDNFV        FVANFA
179200930519     C                   MOVEL     VIDNPG        FVANPG
179300921120     C                   MOVEL     FVVDFV        FVADFA
179400951228     C*
179500941221     C                   UPDATE    FNFVA001
179600951115     C*
179700951228     C* I FOGLI VIAGGIO GIA' ABBINATI NON LI RIELABORO A MENO CHE NON
179800951228     C*   SIANO CAMBIATE LA DATA O L'ORA DI ARRIVO REALE
179900951228    3C     *IN16         IFEQ      *ON
180000951115     C*
180100950621     C** LEGGO TUTTE LE BOLLE DEL FOGLIO E CI METTO IL TOCCO BOLLA
180200970430     C                   CLEAR                   LNASAV
180300950621     C     KARB          SETLL     FNARB06L
180400960108     C     KARB          READE(N)  FNARB06L                               30
180500950621     C*
180600960108    4C     *IN30         DOWEQ     *OFF
180700970430     C     ARBLNA        IFNE      LNASAV
180800970430     C                   MOVE      ARBLNA        LNASAV
180900970430     C                   CLEAR                   DSLV53
181000970430     C                   MOVEL     'A'           D53TFO
181100970430     C                   MOVE      FVANFV        D53NFV
181200970502     C                   MOVE      FVALNP        D53FGS
181300970430     C                   MOVE      FVALAI        D53LAI
181400031215     C                   MOVEL     'B'           D53ABB
181500970430     C                   MOVE      ARBLNA        D53LNA
181600970430     C                   MOVE      ARBTFA        D53TFA
181700970502     C                   MOVE      SIMFEL        D53FEL
181800970430     C                   CALL      'FNLV53R'
181900970430     C                   PARM                    DSLV53
182000970430     C                   MOVE      '1'           WLV53             1
182100970430     C                   END
182200970825     C** SI TRTTA DI BOLLA RELATIVA AL FOGLIO ABBINATO--> ELAVORO
182300970430    5C     D53ERR        IFEQ      *BLANKS
182400070921     c     karb01        chain(E)  fnarb01l
182500070921     c                   if        not %error and %found(fnarb01l)
182600070921     c                   clear                   arbdti
182700070921     c                   clear                   arbhti
182800111118     c                   select
182900111118     c                   when      arbfbs=' '
183000070921     c                   eval      arbfbs='S'
183100111118
183200111118     c                   when      arbfbs='V' or arbfbs='P'
183300111118     c                   eval      arbfbs='E'
183400111118     c                   endsl
183500111118     c
183600070921     c                   update    fnarb000
183700070921     c                   else
183800960108     C                   MOVEL     ARBAAS        AGBAAS
183900960108     C                   MOVEL     ARBLNP        AGBLNP
184000960108     C                   MOVEL     ARBNRS        AGBNRS
184100960108     C                   MOVEL     ARBNSP        AGBNSP
184200960108     C                   WRITE     FNAGB000                             32
184300070921     c                   endif
184400070921     c
184500040315     c                   else
184600040315     c* se errore fnlv53 mi posiziono su nuova lna
184700040315     c     karb6         setgt     fnarb06l
184800951228    5C                   ENDIF
184900950621     C*
185000960108     C     KARB          READE(N)  FNARB06L                               30
185100951228    4C                   ENDDO
185200040120     c*
185300040120      * Carico tutti i secondi livello in arrivo del terminal di arrivo dell'utente
185400040120      * su stesso AS
185500040120     c                   Clear                   Trul09ds
185600040120     c                   Eval      I09Mod = 'TFU'
185700040120     c                   Eval      I09Tfa = fvalai
185800040120     c                   Eval      I09dta = fvadfv
185900040120     c                   Call      'TRUL09R'
186000040120     c                   Parm                    Trul09ds
186100040123     c* se lai non è terminal di nessuno metto comunque lai in mec
186200040123     c                   if        mec(1) = *zeros
186300040123     c                   eval      mec(1) = fvalai
186400040123     c                   endif
186500951227     C*
186600950621     C** LEGGO LE SPUNTE
186700040120     C                   Z-ADD     1             C                 3 0
186800040120     C     MEC(C)        dowNE     *ZEROS
186900960227     C*
187000970515     C                   CLEAR                   DSLV53
187100970515     C                   MOVEL     'A'           D53TFO
187200970515     C                   MOVE      FVANFV        D53NFV
187300970515     C                   MOVE      FVALNP        D53FGS
187400970515     C                   MOVE      FVALAI        D53LAI
187500070917     C                   MOVEL     'B'           D53ABB
187600970515     C                   MOVE      MEC(C)        D53LNA
187700970515     C                   MOVE      SIMFEL        D53FEL
187800970515     C                   CALL      'FNLV53R'
187900970515     C                   PARM                    DSLV53
188000970515     C                   MOVE      '1'           WLV53             1
188100970825     C**
188200970825     C* SI TRATTA DI LINEA RELATIVA AL FOGLIO ABBINATO--> ELABORO
188300970515    5C     D53ERR        IFEQ      *BLANKS
188400921120     C                   MOVEL     MEC(C)        COMLAI
188500070921     c                   clear                   savspe
188600070921     c                   clear                   wtragb
188700911010     C*
188800941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
188900000118     C                   Z-ADD     VI2FVA        WI2FVA
189000070123     C     KBRV1         SETLL     FNBRV02L
189100070123     C     KBRV1         READE     FNBRV02L                               30
189200911010     C*
189300951228    6C     *IN30         DOWEQ     '0'
189400921120     C*
189500941221     C     KBRV2         CHAIN     FNART27L                           31
189600951016     C*
189700951228    7C     *IN31         IFEQ      *OFF
189800070921     c* La prima volta salvo la chiave spedizione  per aggiornare quando
189900070921     c*  ha letto l'ultimo collo e non il primo
190000070921     c                   if        savspe=0
190100070921     c                   movel     artspe        savspe
190200070921     c                   endif
190300951016     C*
190400971002     C* IMPOSTO IL FALG DI ABBINATO SOLTANTO SE LA DATA SUL COLLO
190500971002     C* CORRISPONDE ALLA DATA FOGLIO VIAGGIO
190600971002     C     BRVATR        IFEQ      ' '
190700971002     C     ARTDFV        ANDEQ     FVADFV
190800971002     C     BRVATR        ORNE      ' '
190900971002     C     ARTDUT        ANDEQ     FVADFV
191000070921     c                   if        savspe=artspe
191100951016     C                   MOVEL     'S'           WTRAGB            1
191200070921     c                   endif
191300950328     C                   MOVEL     'S'           ARTABN                         'S'=SPUNTA ABBINATA
191400971119     C                   ENDIF
191500950111     C*
191600950111     C                   UPDATE    FNART000
191700921202     C*
191800070921    8C     ARTSPE        ifne      SAVSPE
191900070921     c                   EXSR      Aggspe
192000960110    8C                   ENDIF
192100951228    7C                   ENDIF
192200911010     C*
192300070123     C     KBRV1         READE     FNBRV02L                               30
192400951228    6C                   ENDDO
192500070921     c
192600070921     c* Elaboro l'ultima bolla
192700070921     c                   if        savspe>0
192800070921     c                   exsr      AggSpe
192900070921     c                   endif
193000070921     c
193100070917     c* Cerco anche altre bolla a cui ho impostato la data e ora modificata
193200070921     c*  ma che non sono partite come spunta su questo foglio
193300070917     c*  (ad esempio disguidi o colli part-non arrivati o colli arr-non part
193400070921    6c                   if        *in15
193500070918     c                   exSR      AGGIOBOL
193600070921    6C                   endif
193700070921    5C                   ENDIF
193800921120     C*
193900921120     C                   ADD       1             C
194000951228    4C                   ENDDO
194100951228     C*
194200951228    3C                   ENDIF
194300960304   2BC                   ENDIF
194400960304     C*
194500960304     C                   ADD       1             Y
194600960304   2AC                   ENDDO
194700960304    2C                   ENDIF
194800911010     C*
194900930129     C                   ADD       1             NRR
195000951228    1C                   ENDDO
195100911010     C**
195200911010     C** SE HO TOLTO DEI FOGLI VIAGGIO DEVO DISABILITARE
195300911010     C**
195400911010     C                   Z-ADD     1             I
195500920428     C                   Z-ADD     0             SAVSPE
195600950328     C                   MOVEL     'DT'          AGBAGB
195700960205     C                   CLEAR                   WALL
195800921120     C*
195900940620    1C     FV(I)         DOWNE     0
196000911010     C     FV(I)         LOOKUP    FVS                                    30
196100911010     C*
196200940620    2C  N30              DO
196300921119     C                   MOVEL     FV(I)         VI2KEY
196400960304     C                   Z-ADD     1             Y
196500960304     C* LEGGO TUTTA LA L6
196600960304   2AC     L6(Y)         DOWGT     0
196700960304     C                   MOVEL     L6(Y)         COMLAI
196800960304     C*
196900960304     C* CHAIN FOGLIO PARTENZE PER AGGIORNARE CAMPI
197000960304     C* E IMPOSTARE LINEE ARRIVO E SCARICO DEL FOGLIO
197100960304     C     KFVA1         CHAIN     FNFVA01L                           33
197200960304   2BC     *IN33         IFEQ      *OFF
197300921120     C*
197400960110     C** LEGGO TUTTE LE BOLLE DEL FOGLIO E TOLGO DATA E ORA ARRIVO
197500960110     C**   EFFETTIVA ULTIMO COLLO
197600970430     C                   CLEAR                   LNASAV
197700960110     C     KARB          SETLL     FNARB06L
197800960110     C     KARB          READE     FNARB06L                             3830
197900960110     C*
198000960110    3C     *IN30         DOWEQ     *OFF
198100960110     C     *IN38         ANDEQ     *OFF
198200970516     C*
198300970430     C     ARBLNA        IFNE      LNASAV
198400970430     C                   MOVE      ARBLNA        LNASAV
198500970430     C                   CLEAR                   DSLV53
198600970430     C                   MOVEL     'A'           D53TFO
198700970430     C                   MOVE      FVANFV        D53NFV
198800970502     C                   MOVE      FVALNP        D53FGS
198900970430     C                   MOVE      FVALAI        D53LAI
199000031215     C                   MOVEL     'B'           D53ABB
199100970430     C                   MOVE      ARBLNA        D53LNA
199200970430     C                   MOVE      ARBTFA        D53TFA
199300970502     C                   MOVE      SIMFEL        D53FEL
199400970430     C                   CALL      'FNLV53R'
199500970430     C                   PARM                    DSLV53
199600970430     C                   MOVE      '1'           WLV53             1
199700970430     C                   END
199800970825     C* SE E' BOLLA RELATIVA AL FOGLIO ABBINATO--> SBINO
199900970430    4C     D53ERR        IFEQ      *BLANKS
200000960110     C                   CLEAR                   ARBDTI
200100960110     C                   CLEAR                   ARBHTI
200200960111     C                   UPDATE    FNARB006
200300040315     c                   else
200400040315     c* se errore fnlv53 mi posiziono su nuova lna
200500040315     c     karb6         setgt     fnarb06l
200600960110    4C                   ENDIF
200700960110     C*
200800960110     C     KARB          READE     FNARB06L                             3830
200900960110    3C                   ENDDO
201000960110     C*
201100960110     C* 38 ON  - RECORD ALLOCATO
201200960110   3AC     *IN38         IFEQ      *ON
201300960110     C                   MOVEL     '1'           WALL              1
201400960110  X3AC                   ELSE
201500960110     C*
201600950328     C* SALVO LA DATA DI ARRIVO EFFETTIVO CHE C'ERA
201700950328     C                   MOVEL     FVAAED        SAVAED
201800950328     C*
201900921120     C                   MOVEL     *ZEROS        FVAAEH
202000921120     C                   MOVEL     *ZEROS        FVAAED
202100960304     C                   MOVEL     ' '           FVARTA
202200921120     C                   MOVEL     *ZEROS        FVANFA
202300921120     C                   MOVEL     *ZEROS        FVADFA
202400960304     C                   MOVEL     0             FVANPG
202500960202     C                   CLEAR                   FVARTA
202600960202     C*
202700941221     C  N33              UPDATE    FNFVA001
202800040120      * Carico tutti i secondi livello in arrivo del terminal di arrivo dell'utente
202900040120      * su stesso AS
203000040120     c                   Clear                   Trul09ds
203100040120     c                   Eval      I09Mod = 'TFU'
203200040120     c                   Eval      I09Tfa = fvalai
203300040120     c                   Eval      I09dta = fvadfv
203400040120     c                   Call      'TRUL09R'
203500040120     c                   Parm                    Trul09ds
203600040123     c* se lai non è terminal di nessuno metto comunque lai in mec
203700040123     c                   if        mec(1) = *zeros
203800040123     c                   eval      mec(1) = fvalai
203900040123     c                   endif
204000960202     C*
204100921120     C                   Z-ADD     1             C
204200040120     C     MEC(C)        downe     *ZEROS
204300970515     C*
204400970515     C                   CLEAR                   DSLV53
204500970515     C                   MOVEL     'A'           D53TFO
204600970515     C                   MOVE      FVANFV        D53NFV
204700970515     C                   MOVE      FVALNP        D53FGS
204800970515     C                   MOVE      FVALAI        D53LAI
204900070917     C                   MOVEL     'B'           D53ABB
205000970515     C                   MOVE      MEC(C)        D53LNA
205100970515     C                   MOVE      SIMFEL        D53FEL
205200970515     C                   CALL      'FNLV53R'
205300970515     C                   PARM                    DSLV53
205400970515     C                   MOVE      '1'           WLV53             1
205500960227     C*
205600970515    4C     D53ERR        IFEQ      *BLANKS
205700921120     C                   MOVEL     MEC(C)        COMLAI
205800921120     C*
205900941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
206000000118     C                   Z-ADD     VI2FVA        WI2FVA
206100070123     C     KBRV1         SETLL     FNBRV02L
206200070123     C     KBRV1         READE     FNBRV02L                               30
206300911010     C*
206400940620    5C     *IN30         DOWEQ     '0'
206500921120     C*
206600941221     C     KBRV2         CHAIN     FNART27L                           31
206700950328     C*
206800951228    6C     *IN31         IFEQ      *OFF
206900971002     C* AGGIORNO ARTABN E LA BOLLO SOLO SE LA DATA CORRISPONDE
207000971002   6AC     BRVATR        IFEQ      ' '
207100971002     C     ARTDFV        ANDEQ     FVADFV
207200971002     C     BRVATR        ORNE      ' '
207300971002     C     ARTDUT        ANDEQ     FVADFV
207400941221     C                   MOVEL     *BLANKS       ARTABN                         SPUNTA NON ABBINATA
207500950328     C*
207600950328     C                   UPDATE    FNART000
207700911011     C*
207800911014     C* C'E' DETTAGLIO COLLI
207900950328    7C     ARTSPE        IFNE      SAVSPE
208000941220     C     KART          CHAIN     FNARB01L                           32
208100911011     C*
208200911011     C* DISABILITO ALLA CONSEGNA
208300940620    8C     ARBAMA        IFNE      2
208400921126     C                   MOVEL     0             ARBAMP
208500940620    8C                   END
208600940620    8C     ARBAMA        IFEQ      1
208700921126     C                   MOVEL     0             ARBAMA
208800940620    8C                   END
208900940613     C* TOLGO DATA E ORA ARRIVO EFFETTIVA ULTIMO COLLO
209000940613     C                   CLEAR                   ARBDTI
209100940613     C                   CLEAR                   ARBHTI
209200941220     C                   UPDATE    FNARB000
209300920428     C                   MOVEL     ARTSPE        SAVSPE
209400940620    7C                   END
209500950407    6C                   ENDIF
209600971002     C                   ENDIF
209700911011     C*
209800070123     C     KBRV1         READE     FNBRV02L                               30
209900940620    5C                   END
210000940620    4C                   END
210100911010     C**
210200921120     C                   ADD       1             C
210300940620    3C                   END
210400960110     C*
210500960110     C* SE TUTTO OK TOLGO IL FOGLIO VIAGGIO DALLA SCHIERA IN MODO CHE
210600960110     C*   VI RIMANGANO SOLO QUELLI CHE NON SONO RIUSCITA AD ELABORARE
210700960110     C*   PERCHE' HO TROVATO I RECORD DI ALCUNE BOLLE ALLOCATI
210800960110     C                   CLEAR                   FV(I)
210900960110   3AC                   ENDIF
211000960304   2BC                   END
211100960304     C**
211200960304     C                   ADD       1             Y
211300960304   2AC                   ENDDO
211400960304    2C                   ENDDO
211500911010     C*
211600911010     C                   ADD       1             I
211700960304    1C                   ENDDO
211800950328     C**
211900950328     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
212000960205    0C     WALL          IFEQ      ' '
212100950328     C                   MOVEL     'A'           P05TBO
212200950407     C                   CLEAR                   P05AGB
212300950328     C                   MOVEL     PARA05        KPJBU
212400950328     C*
212500971218     C* FORZATURA DELLA CODA DIFFE PERCHE' QEUSTI LAVORI DEVONO
212600971218     C*  ANDARE IN SUCCESSIONE E LR27 DEVE ESSERE ELABORATO IN UNA
212700971218     C*  UNICA CODA
212800971218     C                   MOVEL(P)  'KSDIFFE'     KQIEB
212900971218     C*
213000950328     C                   MOVEL     'LV52'        KCOAZ
213100950328     C                   CALL      'BCH10'
213200950328     C                   PARM                    KPJBA
213300971218     C* VIENE SOTTOMESSA L'ABILITAZIONE BOLLE PERCHE' DEVE GIRARE PRIMA
213400971218     C*  DELLE STAMPE: ELENCO SPEDIZIONI NON PARTITE,PERCHE' SERVONO
213500971218     C*  LE BOLLE ABILITATE
213600971218     C*  TUTTO QUESTO ANCHE SE L'ABILITAZIONE BOLLE VIENE ANCHE SOTTOM.
213700971218     C*  DALL'LV52
213800970828     C                   MOVEL     'LR27'        KCOAZ
213900970828     C                   CALL      'BCH10'
214000970828     C                   PARM                    KPJBA
214100950926     C**
214200921119     C**
214300921119     C* LANCIO SE RICHIESTA ELENCO SPEDIZ NO ABILITATE
214400921119     C* PRIMA PER I FOGLI DIRETTI
214500921119     C* POI PER I FOGLI CON SCALO AL TRANSITO E AL TERMINAL
214600960205     C                   MOVEL     ' '           COMATS
214700940620    1C                   DO        2
214800920330     C                   Z-ADD     1             I
214900920330     C*
215000940620    2C     FVS(I)        DOWNE     0
215100940620     C*
215200940620    3C     STS(I)        IFEQ      'S'
215300921119     C     ATS(I)        ANDEQ     COMATS
215400921109     C*
215500921109     C                   Z-ADD     1             C
215600921119     C                   MOVEL     FVS(I)        VI2KEY
215700980303     C                   MOVEL     VI2LAI        COMLAI
215800941221     C                   MOVEL     COMLNP        P52LNP
215900941221     C                   MOVE      VI2FVA        P52FVA
216000040122      * Carico tutti i secondi livello in arrivo del terminal di arrivo
216100040122      * su stesso AS
216200040122     c                   Clear                   Trul09ds
216300150116     c****               Clear                   mecn
216400040122     c                   Eval      I09Mod = 'TFU'
216500040122     c                   Eval      I09dta = dfv(I)
216600040122     c                   move      vi2lai        i09tfa
216700040122     c                   Call      'TRUL09R'
216800040122     c                   Parm                    Trul09ds
216900040123     c* se lai non è terminal di nessuno metto in mecn metto lai
217000040123     c                   if        mec(1) = *zeros
217100150116     c                   eval      mec(1) = i09tfa
217200150116     c**                 else
217300040123     c* ora carico schiera dei p.o. di cui ter-arr non gestiti da altri p.o.
217400150116     c*****              exsr      CarMecn
217500150116     c***                if        mecn(1) = *zeros
217600150116     c***                eval      mecn(1) = i09tfa
217700150116     c***                endif
217800040123     c                   endif
217900921119     C*
218000150116     C     MEC(C)        downe     *ZEROS
218100970515     C*
218200970515     C* PER SAPERE SE ELABORARE RICHIAMO PGM FNLV53R
218300970515     C                   CLEAR                   DSLV53
218400031215     C                   MOVEL     'A'           D53TFO
218500970515     C                   MOVE      VI2FVA        D53NFV
218600970515     C                   MOVE      COMLNP        D53FGS
218700970515     C                   MOVE      COMLAI        D53LAI
218800031215     C                   MOVEL     'B'           D53ABB
218900150116     C                   MOVE      MEC(C)        D53LNA
219000970515     C                   MOVE      SIMFEL        D53FEL
219100970515     C                   CALL      'FNLV53R'
219200970515     C                   PARM                    DSLV53
219300970515     C                   MOVE      '1'           WLV53             1
219400970515     C*
219500970515    5C     D53ERR        IFEQ      *BLANKS
219600940620     C*
219700950215     C                   MOVEL     '1'           P52OUT                         OUTQ DA AZORG00F
219800150116     c**se mecn(c) è p.o. gestito anzichè passare mecn(c) passo il suo
219900150116     c**gestore
220000150116     c***                z-add     1             ii                3 0
220100150116     c***  mecn(C)       lookup    skpo(II)                               30
220200150116     c** 30              eval      p52fel = skpot(ii)
220201150116     c* N.B.: Viene richiamato lr52 anche per le filiali in £6 (es. 102): Non dovrebbe farlo
220202150116     c*       ma visto ch è sempre stato così e dal momento che adesso la stampa non viene
220203150116     c*       quasi mai lanciata e serve solo per i SI DDT ci disinteressiamo
220300150116     C                   eval      p52fel = mec(c)
220400960518     C                   MOVEL     99991231      P52DAT
220500030826     C                   MOVEL(p)  PARA52        KPJBU
220600941216     C                   MOVEL     'LR52'        KCOAZ
220700920330     C                   CALL      'BCH10'
220800920330     C                   PARM                    KPJBA
220900940620    5C                   END
221000921119     C*
221100921109     C                   ADD       1             C
221200940620    4C                   END
221300921109     C*
221400940620    3C                   END
221500921109     C*
221600920330     C                   ADD       1             I
221700940620    2C                   END
221800921119     C*
221900921119     C                   MOVEL     'T'           COMATS            1
222000940620    1C                   END
222100960205    0C                   END
222200911010     C*
222300971218     C                   CLEAR                   KQIEB
222400911008     C                   ENDSR
222500070921     c* Aggiorno a cambio spedizione FNARB per l'abilit.bolla -------*
222600070921     c     AGGSpe        BEGSR
222700070921     c* Aggiorno direttamente la bolla se riesco, altrimenti
222800070921     c*  scrivo AGB se allocata
222900070921    1c                   if        WTRAGB='S'
223000070921     c     ksav          chain(E)  fnarb01l
223100070921    2c                   if        not %error and %found(fnarb01l)
223200070921     c                   clear                   arbdti
223300070921     c                   clear                   arbhti
223400111118     c                   select
223500111118     c                   when      arbfbs=' '
223600111118     c                   eval      arbfbs='S'
223700111118
223800111118     c                   when      arbfbs='V' or arbfbs='P'
223900111118     c                   eval      arbfbs='E'
224000111118     c                   endsl
224100070921     c                   update    fnarb000
224200070921   x2c                   else
224300070921     C                   MOVEL     WsaAAS        AGBAAS
224400070921     C                   MOVEL     WsaLNP        AGBLNP
224500070921     C                   MOVEL     WsaNRS        AGBNRS
224600070921     C                   MOVEL     WsaNSP        AGBNSP
224700070921     C                   WRITE     FNAGB000                             32
224800070921    2c                   endif
224900070921    1c                   endif
225000110103     c*
225100070921     C                   CLEAR                   WTRAGB
225200070921     C                   MOVEL     ARTSPE        SAVSPE
225300110103     c*
225400070921     c* Se per l'ultima spunta di nuova spedizione ho aggiornato
225500070921     c*  fnart, lo memorizzo
225600070921    1c                   if        artabn='S'
225700070921     C                   MOVEL     'S'           WTRAGB            1
225800070921    1c                   endif
225900070921     c                   ENDSR
226000070918     C*
226100070918     C*--- Cerco le bolle da aggiornare con la stessa data ora arrivo *
226200070918     C     Aggiobol      BEGSR
226300110103      *
226400070918      *?Imposto la parte fissa dell'istruzione sql
226500070918     c                   eval      wrkgetlista =
226600070918     c                             'select arbaas, arblnp, arbnrs, arbnsp +
226700070919     c                             from fnarb00f where arbdti ='+
226800070918     c                             %editc(woldaed:'X') + ' and +
226900070918     c                             arbhti ='+ %editc(woldaeh:'X') +
227000070919     c                             ' and arblna=' + %editc(comlai:'X')
227100070918      *?Solo lettura
227200070918     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
227300070918
227400070918     C/EXEC SQL
227500070918     C+ PREPARE s1 FROM :wrkgetlista
227600070918     C/END-EXEC
227700070918     C
227800070918     C/EXEC SQL
227900070918     C+ DECLARE A1 CURSOR FOR S1
228000070918     C/END-EXEC
228100070918     C
228200070918     C/EXEC SQL
228300070918     C+ OPEN a1
228400070918     C/END-EXEC
228500070918
228600070918      *?Leggo il file
228700070918     c                   do        *hival
228800070918     C/EXEC SQL
228900070918     C+ FETCH NEXT FROM a1 INTO : arbaas, :arblnp, :arbnrs, :arbnsp
229000070918     C/END-EXEC
229100070918
229200070919     c                   if        sqlcod = 100 or sqlcod<0
229300070918     c                   leave
229400070918     c                   else
229500070921     c     karb01        chain(E)  fnarb01l
229600070918     c                   if        not %error and  %found(fnarb01l)
229700070918     c                   clear                   arbdti
229800070918     c                   clear                   arbhti
229900111118     c*****              movel     'S'           arbfbs
230000111118     c                   select
230100111118     c                   when      arbfbs=' '
230200111118     c                   eval      arbfbs='S'
230300111118
230400111118     c                   when      arbfbs='V' or arbfbs='P'
230500111118     c                   eval      arbfbs='E'
230600111118     c                   endsl
230700070918     c                   update    fnarb000
230800070918     c                   endif
230900070918     c
231000070918     c                   ENDif
231100070918     c                   enddo
231200070918     C/EXEC SQL
231300070918     C+ CLOSE a1
231400070918     C/END-EXEC
231500070918     c                   ENDSR
231600960202     C*
231700951115     C*--- SR INIZIALE -----------------------------------------------*
231800951115     C     *INZSR        BEGSR
231900951115     C*
232000951115     C****  KLIST  ****
232100960304     C     KCAP          KLIST
232200970721     C                   KFLD                    KVER
232300970721     C                   KFLD                    KNAR
232400960304     C                   KFLD                    KLNA
232500951115     C     KARB          KLIST
232600951115     C                   KFLD                    COMLNP
232700951115     C                   KFLD                    VI2FVA
232800040315     C     KARB6         KLIST
232900040315     C                   KFLD                    COMLNP
233000040315     C                   KFLD                    VI2FVA
233100040315     C                   KFLD                    arblna
233200951115     C     KFVV1         KLIST
233300951115     C                   KFLD                    PARNPG
233400951115     C                   KFLD                    COMNFV            5 0
233500951115     C                   KFLD                    VIDFGS
233600951115     C     KFVA1         KLIST
233700951115     C                   KFLD                    COMLNP
233800951115     C                   KFLD                    VI2FVA
233900951115     C                   KFLD                    COMLAI            3 0
234000951115     C     KFVA2         KLIST                                                  FNFVA02L
234100951115     C                   KFLD                    PARNPG                         ..CATEGORIA
234200951115     C                   KFLD                    COMNFV                         ..FOGLIO
234300980421     C                   KFLD                    L6(XX)
234400951128     C     KFVA4         KLIST                                                  FNFVA02L
234500951128     C                   KFLD                    FVATRN
234600951128     C                   KFLD                    FVADTN
234700951115     C     KBRV1         KLIST                                                  FNBRV02L
234800951115     C                   KFLD                    KNPG                           ..CATEGORIA
234900000118     C                   KFLD                    WI2FVA                         ..NR.FOGLIO
235000951115     C                   KFLD                    COMLNP                         ..FIL.GESTIONE
235100951115     C                   KFLD                    COMLAI                         ..LNA
235200951115     C     KBRV2         KLIST                                                  FNART27L
235300951115     C                   KFLD                    BRVLNP                         ..LNP
235400951115     C                   KFLD                    BRVLNA                         ..LNA
235500951115     C                   KFLD                    BRVNRS                         ..SERIE
235600040120     C                   KFLD                    BRVNSC                         ..SEGNACOLLO
235700951115     C     KART          KLIST
235800951115     C                   KFLD                    ARTAAS
235900951115     C                   KFLD                    ARTLNP
236000951115     C                   KFLD                    ARTNRS
236100951115     C                   KFLD                    ARTNSP
236200070921     C     KSAV          KLIST
236300070921     C                   KFLD                    WsaAAS
236400070921     C                   KFLD                    WsaLNP
236500070921     C                   KFLD                    WsaNRS
236600070921     C                   KFLD                    WsaNSP
236700070919     C     KARB01        KLIST
236800070918     C                   KFLD                    ARbAAS
236900070918     C                   KFLD                    ARbLNP
237000070918     C                   KFLD                    ARbNRS
237100070918     C                   KFLD                    ARbNSP
237200951115     C     KTAB          KLIST
237300951115     C                   KFLD                    CODUT
237400951115     C                   KFLD                    COD
237500951115     C                   KFLD                    KEY
237600951115     C     KFV4          KLIST
237700951228     C                   KFLD                    FVALNP
237800951115     C                   KFLD                    FVANFV
237900951115     C                   KFLD                    WEPA
238000951228     C                   KFLD                    FVALAI
238100951115     C                   KFLD                    WTRC
238200000628      * ESTENSIONE FV
238300000628     C     KFWA1         KLIST
238400000628     C                   KFLD                    FVALNP
238500000628     C                   KFLD                    FVANFV
238600000628     C                   KFLD                    FVALAI
238700951115     C*
238800951115     C****  DEFINIZIONE CAMPI  ****
238900970721     C     *LIKE         DEFINE    CPCVER        O96VER
239000970721     C     *LIKE         DEFINE    CPCVER        KVER
239100970721     C     *LIKE         DEFINE    FVVDFV        I96DRI
239200970721     C     *LIKE         DEFINE    §1XHHO        HHOUNO
239300960304     C     *LIKE         DEFINE    §1XHHO        HHODUE
239400960304     C     *LIKE         DEFINE    D98DTI        DATSOG
239500960304     C     *LIKE         DEFINE    D98HTI        ORASOG
239600970721     C     *LIKE         DEFINE    CPCNAR        KNAR
239700970721     C     *LIKE         DEFINE    CPCLNA        KLNA
239800960304     C     *LIKE         DEFINE    BRVNPG        KNPG
239900951115     C     *LIKE         DEFINE    FVAAED        SAVAED
240000951228     C     *LIKE         DEFINE    FVAAED        WAED8
240100951116     C     *LIKE         DEFINE    FV4TRC        WTRC
240200951115     C     *LIKE         DEFINE    TBLCOD        COD
240300951115     C     *LIKE         DEFINE    TBLKEY        KEY
240400970430     C     *LIKE         DEFINE    ARBLNA        LNASAV
240500970825     C     *LIKE         DEFINE    FV4EPA        WEPA
240600000118     C     *LIKE         DEFINE    BRVNFV        WI2FVA
240700951115     C*
240800970825     C                   MOVEL     'A'           WEPA                           *ARRIVO
240900960202     C                   MOVEL     'R'           WTRC
241000951115     C                   ENDSR
241100040122     C*--- CARICAMENTO SCHIERA MECN ----------------------------------*
241200150116     C**   CarMecn       BEGSR
241300150116     c**                 z-add     0             k                 2 0
241400150116     c**                 Do        30            x
241500150116     c**                 If        Mec(x) = *Zeros
241600150116     c**                 Leave
241700150116     c**                 EndIf
241800150116     c**   Mec(x)        Lookup    Skpot                                  30
241900150116     c**                 If        *In30
242000150116     c**                 Add       1             k
242100150116     c**                 Eval      Mecn(k) = Mec(x)
242200150116     c**                 Iter
242300150116     c**                 EndIf
242400150116     c**   Mec(x)        Lookup    Skpo                                   30
242500150116     c**                 If        Not *In30
242600150116     c**                 Add       1             k
242700150116     c***                Eval      Mecn(k) = Mec(x)
242800150116     c***                Iter
242900150116     c***                EndIf
243000150116     c***                EndDo
243100150116     c***                endsr
243200110103
243300110103      *---------------------------------------------------------------*
243400110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
243500110103      *---------------------------------------------------------------*
243600110103     c     sr_DatiJob    BEGSR
243700110103      *
243800110103     c     *dtaara       define    §azute        AZUTEds
243900110103     c     *dtaara       define    §datiute      dDatiUte
244000110103      *
244100110103     c                   in(E)     *dtaara
244200110103     c                   IF        %Error or RSUT = *blanks
244300110103     c                   clear                   TIBS34ds
244400110103     c                   call      'TIBS34R'
244500110103     c                   parm                    TIBS34ds
244600110103     c                   in        *dtaara
244700110103     c                   ENDIF
244800110103      *
244900110103     c                   ENDSR
245000951114     C*---------------------------------------------------------------*
245100000606**  MSG
245200951115Immettere causale valida                                                      1
245300951115Presente ritardo/anticipo nell'arrivo del foglio: immettere la causale        2
245400951121Non c'e' ritardo/anticipo: se si vuole si può eliminare la causale            3
245500951121FOGLIO VIAGGIO GIA' ABBINATO CON FOGLIO ARRIVI N.RO  X - XXXXX                4
245600951115Se immessa causale "G" occorre inserire descrizione ritardo/anticipo          5
245700960201Presente descrizione ritardo: immettere la causale o eliminare la descrizione 6
245800960227Fogli xxx-xxxxx e xxx-xxxxx del traino xxxxxxx hanno data/ora/causale diverse 7
245900960201Presente descrizione ritardo: premere F6 per conferma o eliminare descrizione 8
246000960201Eliminata descrizione ritardo: confermare con F6 o reinserire la descrizione  9
246100960227xxx-xxxxx deve avere stesso arrivo(xx/xx xx:xx x)del F.xxx-xxxxx: ENTER x forz10
246200960109Non tutte le bolle sono state aggiornate: premere nuovamente F6 !!            11
246300960202Impossibile uscire perchè non tutte le bolle sono state aggiornate: premere F612
246400960227Data/ora arrivo non puo' essere superiore alla data/ora corrente: ENTER x forz13
246500960626Impossibile modificare o eliminare la causale. Unica causale ammessa: X       14
246600960626Impossibile immettere la causale                                              15
246700960626Impossibile modificare la descrizione                                         16
246800960626Impossibile modificare la causale: eliminarla oppure lasciarla = X            17
246900971002Impossibile visualizzare il foglio: provieni dalla visualizzazione!!          18
247000030924Impossibile posticipare l'ora di arrivo del foglio illuminato oltre le xx:xx  19
247100000606Data arrivo non puo' essere superiore alla data foglio arrivi     ENTER x forz20
