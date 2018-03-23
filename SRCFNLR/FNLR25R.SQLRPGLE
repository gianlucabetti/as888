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
009500951115     D*
009600960201     D* PASSAGGIO DATI A PGM TRASMISS. FNFVA/FNFV4  - FNLR20R -
009700960201     D PARAM2          DS
009800960201     D* PARNUM E' USATO PER RISOTTOMETTERE L'AZIONE DI TRASMISSIONE
009900960201     D  PA2NUM                 1      1  0 INZ
010000911010     D WLBDAT          DS
010100941221     D  G02DAT                 1      8  0
010200941221     D  G02INV                 9     16  0
010300941221     D  G02ERR                17     17
010400941221     D  G02TGI                18     22  0
010500960304     D WGIDAT          DS                  INZ
010600960304     D  GIODAT                 1      8  0
010700960304     D  GIOINV                 9     16  0
010800960304     D  GIOTGI                17     21  0
010900920428     D                 DS
011000941221     D  ARTAAS                 1      4  0
011100941221     D  ARTLNP                 5      7  0
011200941221     D  ARTNRS                 8      9  0
011300941221     D  ARTNSP                10     16  0
011400941221     D  ARTSPE                 1     16  0
011500070921     D                 DS
011600070921     D  WsaAAS                 1      4  0
011700070921     D  WSALNP                 5      7  0
011800070921     D  WSANRS                 8      9  0
011900070921     D  WsaNSP                10     16  0
012000070921     D  SAVSPE                 1     16  0
012100921118     D                 DS
012200921118     D  COMLNP                 1      3  0
012300921118     D  VI2FVA                 4      8  0
012400921119     D  VI2LAI                 9     11
012500921118     D  VI2KEY                 1     11  0
012600951117     D                 DS
012700951117     D  WLNP                   1      3  0
012800951117     D  WFVA                   4      8  0
012900951117     D  WLAI                   9     11
013000951117     D  WKEY                   1     11  0
013100921119     D                 DS
013200921119     D  FVALNP                 1      3  0
013300921120     D  FVANFV                 4      8  0
013400921119     D  FVALAI                 9     11  0
013500921119     D  FVAKEY                 1     11  0
013600951121     D                 DS
013700951121     D  VI2AED                 1      6  0
013800951121     D  VI2AEH                 7     10  0
013900960222     D  VI2RTC                11     11
014000960222     D  WDHARR                 1     11
014100030924
014200030924     d                 ds
014300030924     d  wore                   1      2
014400030924     d  wminuti                3      4
014500030924     d  worario                1      4
014600951110     D*
014700950328     D* DS PER TRUL06R - CARICAMENTO £X
014800950328     D DSUL06        E DS                  EXTNAME(TRUL06DS)
014900950328     D  LIN                    1     90  0
015000950328     D                                     DIM(30)                              FIL. COMODO
015100040122      * Parametri per richiamo a TRUL26R
015200150116     d*Trul26ds      e ds
015300150116     d**skpo                  11    760  0 dim(250)
015400150116     d**skpot                761   1510  0 dim(250)
015500040120      * Parametri per richiamo a TRUL09R
015600040120     d Trul09ds      e ds
015700040120     d  mec                   19    108  0 dim(30)
015800951114     D*
015900951114     D* DS PER TNTL35R - CONTROLLO ESISTENZA RITAR./ANTIC. ARRIVO FV
016000951114     D DSTL35        E DS                  EXTNAME(TNTL35DS)
016100960201     D*
016200960201     D* DS PER FNLV15R - GESTIONE DESCRIZIONE CAUSALE RITARDO/ANTICIPO
016300960201     D DSLV15        E DS                  EXTNAME(FNLV15DS)
016400951114     D*
016500970925     D* DS PER TNSD99R - CAOCLO TEMPI DI TRAZIONI INTERNI
016600970925     D DSSD99        E DS                  EXTNAME(TNSD99DS)
016700970430     D DSLV53        E DS                  EXTNAME(FNLV53DS)
016800970825     D DSLV55        E DS                  EXTNAME(FNLV55DS)
016900960304     D*
017000960207     D DS1X          E DS
017100960626     D DS7AF         E DS
017200930208     D DS7N          E DS
017300920901     D KPJBA         E DS
017400110103      *
017500110103      * - Parametri per richiamo al pgm di controllo profilo utenti
017600110103     d TIBS34ds      e ds
017700110103      * - Ds di riferimento al file esterno AzUte00F
017800110103     d AZUTEds       e ds                  ExtName(AzUte00F)
017900110103      * - Ds per dati organigramma
018000110103     d dDatiUte      e ds
018100110103      *
018200941221     D DSLR01        E DS                  INZ
018300000628     D OG143         E DS
018400070918     dwoldaed          s                   like(fvaaed)
018500070918     dwoldaeh          s                   like(fvaaeh)
018600070918     d wrkgetlista     s           4096    varying
018700141110     d in              s              3  0
018800951114     I*
018900000000     C*---------------------------------------------------------------*
019000911010     C* INDICATORI
019100911010     C*---------------------------------------------------------------*
019200921123     C*  01    - SI TRATTA SOLO DI UNA BUSTA
019300030722     C*  04    - ABILITO CAMBIO FILIALE SE PREMUTO F18
019400921120     C*  05    - SONO UNA FILIALE DI 2 LIVELLO
019500921123     C*  09    - RICERCA FOGLI
019600911010     C*  10    - ROLL UP
019700960108     C*  16    - E' STATA MODIFICATA LA DATA E ORA ARRIVO REALE OPPURE
019800960108     C*          SI TRATTA DI FOGLIO VIAGGIO CON NUMERO F.ARRIVI = 0
019900960626     C*  17    - ABBINAMENTO DI FOGLIO ARRIVI CON DATA <= DATA ULTIMA
020000960626     C*          ELABORAZIONE IDD IN FORMA DEFINITIVA
020100951115     C*  18    - ARRIVO IN ANTICIPO/RITARDO
020200951115     C*  28    - EMETTE CAMPO MESSAGGI
020300951115     C*  30/32 - DI COMOD0
020400941221     C*  33    - LETTURA FNFVA
020500951117     C*  34/35 - DI COMODO
020600951227     C*  38    - RECORD ALLOCATO IN FNARB06L
020700951117     C*  40/59 - ERRORE
020800971003     C*  60/63 - POSIZIONAMENTO CURSORE PER MESSAGGI DI ERRORE
020900911010     C*  72/73 - DI SUBFILE
021000911010     C*  90    - ERRORE GENERICO
021100911010     C*---------------------------------------------------------------*
021200000000     C     *ENTRY        PLIST
021300000000     C                   PARM                    KPJBA
021400110103      /free
021500110103       // Impostazione opzioni per SQL
021600110103       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
021700110103       exec sql   set  option  DynUsrPrf = *Owner,
021800110103                               CloSqlCsr = *EndMod;
021900110103      /end-free
022000110103      *
022100971002     C* SE RICLAN=6 VUOL DIRE CHE E' RICHIAMO DA FNLR26R
022200971002     C                   MOVEL     KPJBU         RICLAN            1
022300941221     C*
022400110103     C                   Z-ADD     1             CODUT             1 0
022500110103      *
022600110103      * Reperimento dati utente/aziendali
022700110103     c                   exsr      sr_DatiJob
022800951115     C*
022900900511     C*---------------------------------------------------------------*
023000950407     C* OPERAZIONE FITTIZIA PER GIUSTIFICARE IL FILE IN INPUT
023100950407     C                   READ      FNAGB000                               30
023200921109     C*
023300921109     C                   EXSR      CARTAB
023400900517     C*
023500911010     C     INIZIO        TAG
023600911008     C                   EXSR      PULIZ
023700971223     C*
023800921123     C     FOR01         TAG
023900941216     C   09              WRITE     LR25D03
024000911008     C*
024100941216     C                   EXFMT     LR25D01
024200030722     C** F3 - FINE LAVORO
024300971223     C   28
024400971223     COR KC              GOTO      FINE
024500930127     C*
024600030722     C** F18- CAMBIO FILIALE
024700921117     C   KS              SETON                                        04
024800921117     C   KS              GOTO      FOR01
024900921117     C*
025000921117     C   04              DO
025100930208     C                   SETOFF                                       09
025200941220     C                   MOVEL     VIDFGS        ALF3              3
025300960227     C     ALF3          LOOKUP    NOG                                    30
025400960227     C   30              SETON                                        49
025500960227     C   30              GOTO      FOR01
025600960626     C* CHAIN SULLA TABELLA 7A PER PRENDERE LA DATA ULTIMA ELABORAZIONE
025700960626     C* IDD
025800960626     C                   EXSR      CTB7A
025900960227     C* SE TUTTO OK CARICO LA L6
026000960227     C                   EXSR      CARL6
026100921117     C                   END
026200921117     C*
026300911008     C* CONTROLLO FOGLIO VIAGGIO ARRIVI
026400911008     C                   EXSR      CONTR1
026500930127     C* SE ERRORE
026600930127     C   90              GOTO      FOR01
026700930127     C                   Z-ADD     1             REC
026800911008     C*
026900930127     C* EMISSIONE SFILE
027000951120     C                   SETOFF                                       45
027100911010     C     FORCT2        TAG
027200941216     C                   WRITE     LR25D02
027300961014     C   72              WRITE     LR25D04
027400911008     C     FORCT         TAG
027500911010     C                   Z-ADD     1             NRR
027600941216     C                   EXFMT     LR25DCT
027700951115     C*
027800951115     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
027900951115     C                   CLEAR                   VCCMSG
028000951117     C                   SETOFF                                       286061
028100030924     C                   SETOFF                                       626353
028200951115     C*
028300951121     C* CONTROLLO SE POSSONO ESSERE PREMUTI COMANDI CHE NON AGGIORNANO
028400961014     C  N72*INKC         IFEQ      *ON
028500951121     C     *INKL         OREQ      *ON
028600960109     C*
028700960110     C* SE TROVATI RECORD ALLOCATI COSTRINGO A PREMERE NUOVAMENTE F6
028800960110     C     WALL          IFEQ      '1'
028900960110     C                   MOVEL     MSG(12)       VCCMSG
029000960110     C                   SETON                                        28
029100960110     C                   GOTO      FORCT2
029200960109     C                   ENDIF
029300960109     C*
029400960202     C* CONTROLLO CAUSALE
029500960626     C                   EXSR      F03F12
029600960626     C   90              GOTO      FORCT2
029700960202     C*
029800960202     C* SE VARIATO QUALCOSA IN FNFV4 ED ESCO SENZA CONFERMARE
029900960202     C*   SOTTOMETTO PGM DI TRASMISSIONE
030000960202     C     WAGG          IFNE      ' '
030100960202     C                   MOVEL     PARAM2        KPJBU
030200960202     C                   MOVEL     'LR20'        KCOAZ
030300960202     C                   CALL      'BCH10'
030400960202     C                   PARM                    KPJBA
030500960202     C                   ENDIF
030600951121     C                   ENDIF
030700951121     C*
030800030722     C** F3 - FINE
030900911030     C   KC              GOTO      FINE
031000930127     C*
031100030722     C** F12 - RITORNO
031200911008     C   KL              GOTO      INIZIO
031300930127     C*
031400030722     C** F7 - INTERROG. SPUNTE PARTENZE ALL'ARRIVO
031500941221     C   KG              DO
031600941221     C                   CLEAR                   PARA04
031700941221     C                   Z-ADD     VIDFGS        P04FGS                         FIL.GESTIONE
031800941221     C                   MOVEL     VIDNPG        P04NPG                         CATEGORIA
031900941221     C                   MOVEL     VIDNFV        P04NFV                         NR.FOGLIO
032000941221     C                   Z-ADD     99            P04NRS
032100941221     C                   Z-ADD     99            P04NPS
032200941221     C                   MOVEL     '1'           P04FLG                         '1'=INTERROGAZIONE
032300941221     C                   MOVEL     'FNLR25R '    P04PGM                         RICHIAMATO
032400941221     C                   MOVEL     PARA04        KPJBU
032500941221     C                   CALL      'FNLR04R'
032600920225     C                   PARM                    KPJBA
032700941221     C                   GOTO      FORCT2
032800941221     C                   ENDDO
032900921120     C*
033000960626     C** 10 ON  - ROLL UP SOLO SE L'ULTIMO E' PIENO E 17 OFF
033100960626     C   10              DO
033200960626     C                   SETOFF                                       30
033300960626     C     SAVNRR        SUB       1             NRR
033400960626     C  N17NRR           CHAIN     LR25DSF                            30
033500920316     C  N30VI2LNP        IFLE      *ZEROS
033600920316     C     VI2FVA        ANDEQ     *ZEROS
033700960626     C     *IN17         OREQ      *ON
033800920316     C                   SETON                                        43
033900911008     C*
034000920316     C                   Z-ADD     NRR           REC
034100920316     C                   GOTO      FORCT
034200920316     C                   END
034300911008     C*
034400920316     C                   ADD       1             NRR
034500920316     C                   Z-ADD     1             B
034600920316     C                   Z-ADD     NRR           REC
034700911008     C                   EXSR      PULSFL
034800920316     C                   GOTO      FORCT
034900911008     C                   END
035000911008     C* CONTROLLO SUBFILE
035100911008     C                   EXSR      CTRSFL
035200911030     C   45
035300960206     COR 09              GOTO      FORCT2
035400920706     C   90              GOTO      FORCT
035500951120     C  NKF
035600951120     CANNKN              GOTO      FORCT2
035700030722     C* F6  - ELABORAZIONE
035800030722     C* F14 - FORZATURA
035900950104     C   KN
036000950104     COR KF              DO
036100921029     C                   EXSR      ELAB
036200960205     C*
036300960205     C* WALL = "1" ---> RECORD ALLOCATO IN FNARB06L: NON TUTTE LE BOLLE
036400960205     C*   SONO STATE AGGIORNATE, PREMERE NUOVAMENTE F6
036500960205     C     WALL          IFEQ      '1'
036600960205     C                   MOVEL     MSG(11)       VCCMSG
036700960205     C                   SETON                                          28
036800960205     C                   GOTO      FORCT
036900960205     C                   ENDIF
037000960723     C**
037100960723     C* CHIUDO FOGLIO ARRIVI SE 17 ON
037200960723     C     *IN17         IFEQ      *ON
037300960723     C     KFVV1         CHAIN     FNFVV01L                           30
037400960723     C     *IN30         IFEQ      *OFF
037500960723     C                   MOVE      'S'           FVVFCF
037600960723     C                   UPDATE    FNFVV000
037700960723     C                   END
037800960723     C                   END
037900960205     C**
038000960201     C* SOTTOMETTO AZIONE DI TRASMISSIONE FNFVA00F/FNFV400F IN SEDE E
038100960201     C*   ALLA FILIALE DI PARTENZA
038200960201     C                   MOVEL     PARAM2        KPJBU
038300960129     C                   MOVEL     'LR20'        KCOAZ
038400951228     C                   CALL      'BCH10'
038500951228     C                   PARM                    KPJBA
038600921029     C                   END
038700951227     C*
038800000000     C     FINE          TAG
038900960202     C*
039000960202     C* RICHIAMO PGM FNLV15R PER CHIUSURA FILES
039100960202     C     WLV15         IFNE      ' '
039200960202     C                   CLEAR                   DSLV15
039300960202     C                   MOVEL     'C'           D15TLA
039400960202     C                   MOVEL     DSLV15        KPJBU
039500960202     C                   CALL      'FNLV15R'
039600960202     C                   PARM                    KPJBA
039700960202     C                   ENDIF
039800960202     C* RICHIAMO PGM TNTL35R PER CHIUSURA FILES
039900960202     C     WTL35         IFNE      ' '
040000960202     C                   CLEAR                   DSTL35
040100960202     C                   MOVEL     'C'           D35TLA
040200960202     C                   MOVEL     DSTL35        KPJBU
040300960202     C                   CALL      'TNTL35R'
040400960202     C                   PARM                    KPJBA
040500960202     C                   ENDIF
040600970430     C*
040700970430     C     WLV53         IFEQ      '1'
040800970430     C                   MOVEL     'C'           D53TLA
040900970430     C                   CALL      'FNLV53R'
041000970430     C                   PARM                    DSLV53
041100970430     C                   END
041200970825     C                   MOVEL     'C'           D55TLA
041300970825     C                   CALL      'FNLV55R'
041400970825     C                   PARM                    DSLV55
041500040123     c*
041600040123     c                   eval      i09tla='C'
041700040123     c                   call      'TRUL09R'
041800040123     c                   parm                    trul09ds
041900960202     C*
042000000000     C                   SETON                                        LR
042100960202     C*
042200941216     C**************************************************************************
042300941216     C*    CARICO TABELLE
042400941216     C**************************************************************************
042500921109     C     CARTAB        BEGSR
042600040122      * Carico tutti i p.o. gestiti con il relativo p.o. gestore
042700150116     c***                Clear                   Trul26ds
042800150116     c***                Eval      I26tla = 'L'
042900150116     c***                Eval      I26tpt = '6'
043000150116     c***                Call      'TRUL26R'
043100150116     c***                Parm                    Trul26ds
043200960227     C***
043300960227     C* CARICO TABELLA FILIALI GESTITE DA ALTRE  SU TUTTO L'AS
043400040122     C***
043500040122     C                   CLEAR                   DSUL06
043600040122     C                   MOVE      '£6'          D06COD
043700040122     C                   MOVEL     SIMFEL        D06KEY
043800040122     C                   MOVEL     'G'           D06ESC
043900040122     C                   MOVEL     DSUL06        KPJBU
044000960227     C*
044100040122     C                   CALL      'TRUL06R'
044200040122     C                   PARM                    KPJBA
044300040122     C                   MOVEL     KPJBU         DSUL06
044400040122     C                   MOVEL     LIN           NOG
044500950328     C***
044600020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
044700950328     C***
044800110103     C                   Z-ADD     dutPOU        VIDFGS
044900110103     C     dutLPO        IFEQ      '2'
045000110103     C     dutLPO        OREQ      *BLANKS
045100020424     C                   SETON                                        05
045200020424     C                   ENDIF
045300960227     C* CARICO LA L6 DELLA FILIALE GESTIONE IMPOSTATA
045400960227     C                   EXSR      CARL6
045500921120     C*
045600921120     C* IMPOSTO CAMPI DI PASSAGGIO
045700941221     C                   MOVEL     2             PARNPG            1 0
045800960626     C*
045900960626     C                   EXSR      CTB7A
046000921109     C                   ENDSR
046100960227     C**************************************************************************
046200960227     C*    CARICO LA £6
046300960227     C**************************************************************************
046400960227     C     CARL6         BEGSR
046500960227     C***
046600960227     C* CARICO TABELLA FILIALI GESTITE DA ALTRE
046700960227     C***
046800960227     C                   CLEAR                   DSUL06
046900960227     C                   MOVE      '£6'          D06COD
047000960227     C                   MOVEL     VIDFGS        D06KEY
047100960227     C                   MOVEL     'L'           D06TLA
047200960227     C                   MOVEL     DSUL06        KPJBU
047300960227     C*
047400960227     C                   CALL      'TRUL06R'
047500960227     C                   PARM                    KPJBA
047600960227     C                   MOVEL     KPJBU         DSUL06
047700960227     C                   MOVEA     LIN           L6
047800960227     C                   ENDSR
047900960626     C**************************************************************************
048000960626     C* CHAINO TABELLA 7A PER REPERIRE DATA ULTIMA ELABORAZIONE IDD
048100960626     C**************************************************************************
048200960626     C     CTB7A         BEGSR
048300960626     C***
048400960626     C* CHAIN SULLA TABELLA 7A PER PRENDERE LA DATA ULTIMA ELABORAZIONE
048500960626     C* IDD
048600960626     C                   MOVEL     '7A'          COD
048700960626     C                   MOVEL(P)  VIDFGS        KEY
048800960626     C     KTAB          CHAIN     TABEL                              30
048900960626     C  N30              MOVEL     TBLUNI        DS7AF
049000960626     C   30              MOVE      *ZEROS        §7ADID
049100960626     C                   ENDSR
049200941216     C**************************************************************************
049300941216     C*    PULIZIA CAMPI
049400941216     C**************************************************************************
049500911008     C     PULIZ         BEGSR
049600960202     C                   CLEAR                   WAGG
049700960202     C                   CLEAR                   WALL
049800941216     C*
049900920709     C* PRIMA VIDEATA
050000911009     C                   MOVEL     *BLANKS       VIDNFV
050100120709     C                   MOVEL     'N'           VIDSTA
050200941221     C                   TIME                    UTIME            14 0          ORA E DATA
050300960205     C* DATA A 8
050400960202     C                   MOVE      UTIME         UDATE8            8 0          GG/MM/AAAA
050500960227     C                   Z-ADD     UDATE8        G02DAT
050600960227     C                   MOVEL     *BLANK        G02ERR
050700960227     C                   CALL      'XSRDA8'
050800960227     C                   PARM                    WLBDAT
050900960227     C                   Z-ADD     G02INV        DATEU8            8 0          AAAA/MM/GG
051000960205     C* DATA A 6
051100960205     C                   MOVE      UDATE8        W0020             2 0
051200960205     C                   MOVEL     UDATE8        UDATE6            6 0
051300960205     C                   MOVE      W0020         UDATE6
051400960205     C*
051500960206     C                   MOVEL     UTIME         WTIME             4 0          HH:MM
051600040227     c*
051700040227     C                   MOVEL     '2'           VIDNPG
051800911009     C* SUBFILE
051900961014     C                   SETON                                        720973
052000941216     C                   WRITE     LR25DCT
052100961014     C                   SETOFF                                       720473
052200911008     C*
052300911008     C                   ENDSR
052400941216     C**************************************************************************
052500941216     C*    CARICO 1 PAGINA DI SUBFILE VUOTA
052600941216     C**************************************************************************
052700911008     C     PULSFL        BEGSR
052800920706     C                   SETOFF                                       01
052900951115     C* LA DATA ARRIVO MEZZO LA VISUALIZZO LUNGA 6
053000921120     C*
053100930127     C     B             DOWLE     13
053200960205     C                   EXSR      PULRIG
053300941216     C                   WRITE     LR25DSF
053400911008     C                   ADD       1             NRR
053500920316     C                   ADD       1             B                 3 0
053600911008     C                   END
053700911008     C* SALVO L'ULTIMO
053800920316     C                   Z-ADD     NRR           SAVNRR            5 0
053900941216     C*
054000911008     C                   ENDSR
054100941216     C**************************************************************************
054200960205     C*    PULIZIA DI UNA RIGA DI SFL
054300941216     C**************************************************************************
054400960205     C     PULRIG        BEGSR
054500960205     C                   MOVEL     *ZEROS        VI2FVA
054600960205     C                   MOVEL     *BLANKS       VI2LNP
054700000628     C                   MOVE      *BLANKS       VI2PTV
054800960205     C                   MOVEL     *BLANKS       VI2LAI
054900960205     C                   Z-ADD     UDATE6        VI2AED
055000960205     C                   CLEAR                   VI2AEH
055100960205     C                   CLEAR                   SA2AED
055200960205     C                   CLEAR                   SA2AEH
055300960205     C                   Z-ADD     0             VI2DFV
055400960205     C                   CLEAR                   VI2RTC
055500960205     C                   CLEAR                   SAVRTC
055600960205     C                   CLEAR                   VI2DTC
055700960205     C                   CLEAR                   VI2GES
055800960205     C                   CLEAR                   VI2DES
055900960402     C                   CLEAR                   VI2AST
056000960205     C                   CLEAR                   WFORZ
056100960205     C                   CLEAR                   WFORZ2
056200960227     C                   CLEAR                   WFORZ3
056300960205     C                   MOVEL     VIDSTA        VI2STA
056400960205     C                   CLEAR                   VI2VFV
056500960206     C                   CLEAR                   WPRVOL
056600960205     C                   ENDSR
056700960205     C**************************************************************************
056800960205     C*    CONTROLLI FOGLIO VIAGGIO
056900960205     C**************************************************************************
057000960205     C     CONTR1        BEGSR
057100941216     C*
057200911008     C                   SETOFF                                       303190
057300951120     C                   SETOFF                                       09
057400930208     C* CATEGORIA FOGLI
057500930208     C     VIDNPG        IFEQ      ' '
057600930208     C                   SETON                                        5790
057700930208     C                   GOTO      ENDCT1
057800930208     C                   END
057900930208     C     VIDNPG        IFEQ      '?'
058000930208     C                   MOVEL     CODUT         §KUT
058100930208     C                   MOVEL     '7N'          §COD
058200930208     C                   MOVE      ' '           VIDNPG
058300930208     C                   MOVEL     *BLANKS       §KEY
058400930208     C                   CALL      'X§TABER'
058500930208     C                   PARM                    §KUT              1 0
058600930208     C                   PARM                    §COD              2
058700930208     C                   PARM                    §KEY              8
058800930208     C                   PARM                    §DES             30
058900930208     C                   MOVEL     §KEY          VIDNPG
059000040120     C                   SETON                                        90
059100930208     C                   GOTO      ENDCT1
059200930208     C                   END
059300930208     C* CONTROLLO
059400930208     C                   MOVEL     '7N'          COD
059500930208     C                   MOVEL     *BLANKS       KEY
059600930208     C                   MOVEL     VIDNPG        KEY
059700930208     C* INESISTENTE
059800930208     C     KTAB          CHAIN     TABEL                              57
059900930208     C* ANNULLATA
060000930609     C  N57TBLFLG        IFNE      ' '
060100930208     C                   SETON                                        57
060200930208     C                   ELSE
060300930208     C     VIDNPG        IFNE      '2'
060400930208     C                   SETON                                        57
060500930208     C                   END
060600930208     C                   END
060700930208     C* ERRATA
060800930208     C   57              SETON                                        90
060900930208     C   57              GOTO      ENDCT1
061000930208     C*
061100930208     C                   MOVEL     TBLUNI        DS7N
061200930208     C* NON UTILIZZABILE DALLA FILIALE IN GESTIONE
061300930208     C     §7NUTI        IFEQ      '9'
061400930208     C                   SETON                                        5890
061500930208     C                   GOTO      ENDCT1
061600930208     C*
061700930208     C                   ELSE
061800930208     C*
061900930208     C* SOLO DALLA PRIMO LIVELLO UTILIZZABILE
062000930208     C     §7NUTI        IFEQ      '1'
062100941220     C     VIDFGS        ANDNE     SIMFEL
062200930208     C                   SETON                                        5890
062300930208     C                   GOTO      ENDCT1
062400930208     C                   END
062500930208     C     §7NUTI        IFEQ      '2'
062600941220     C     VIDFGS        ANDEQ     SIMFEL
062700930208     C                   SETON                                        5890
062800930208     C                   GOTO      ENDCT1
062900930208     C                   END
063000930208     C                   END
063100930208     C                   MOVEL     VIDNPG        PARNPG
063200930208     C*
063300911008     C* NUMERO FOGLIO VIAGGIO
063400911008     C     VIDNFV        IFEQ      *BLANKS
063500911008     C     VIDNFV        OREQ      *ZEROS
063600911008     C                   SETON                                        4090
063700911008     C                   GOTO      ENDCT1
063800911008     C*
063900911008     C                   ELSE
064000911008     C* RICERCA
064100950328     C     '?'           SCAN      VIDNFV                                 09
064200911010     C   09              DO
064300941221     C                   CLEAR                   DSLR01
064400941221     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
064500941221     C                   MOVEL     PARNPG        DLRNPG                         CATEGORIA FOGLIO
064600941221     C                   MOVEL     '2'           DLRTFV                         TIPO FOGLIO
064700941221     C                   MOVEL     'FNLR25R'     DLRPGM                         NOME PROGRAMMA
064800941221     C                   Z-ADD     31129999      DLRGAL                         AL GG/MM/AAAA
064900941221     C                   Z-ADD     99991231      DLRAAL                         AL AAAA/MM/GG
065000941221     C                   MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
065100941221     C                   MOVEL     DSLR01        KPJBU
065200941221     C                   CALL      'FNLR02R'
065300921120     C                   PARM                    KPJBA
065400941221     C                   MOVEL     KPJBU         DSLR01
065500941221     C                   MOVEL     DLRNFV        VIDNFV
065600911009     C                   SETON                                        90
065700911008     C                   GOTO      ENDCT1
065800911008     C                   END
065900911010     C* CONTROLLI
066000911008     C                   TESTN                   VIDNFV               31
066100930127     C  N31              SETON                                        30
066200911008     C*
066300930127     C  N30              MOVEL     VIDNFV        COMNFV
066400960723     C  N30KFVV1         CHAIN(N)  FNFVV01L                           30
066500970421     C* INESISTENTE
066600970421     C     *IN30         IFEQ      *ON
066700911008     C* ANNULLATO
066800970421     C     FVVATB        ORNE      *BLANKS
066900961010     C                   SETON                                        4090
067000961010     C                   GOTO      ENDCT1
067100911008     C                   END
067200180126     c* se foglio incompatibili non lo posso usare per l'abbinamento
067300180126     c                   if        fvvspg<>' '
067400180126     C                   SETON                                        4090
067500180126     C                   GOTO      ENDCT1
067600180126     C                   endif
067700961010     C                   END
067800921120     C*
067900960626     C* IMPOSTO INDICATORE PER PERMETTERE SOLO LA VARIAZ. DI DATA/ORA
068000960626     C* ARRIVO REALE DI FV GIA' ABBINATI SE DATA FOGLIO ARRIVI <= DATA
068100960626     C* ULTIMA ELABORAZIONE IDD
068200990512     C     FVVDFV        IFLE      §7ADID
068300970721     C* ANCHE SE FOGLIO CHIUSO POSSO SOLO MODIFICARE LA DATA E ORA DI
068400961010     C*  ARRIVO
068500961010     C     FVVFCF        OREQ      'S'
068600960626     C                   SETON                                        17
068700960626     C                   ELSE
068800960626     C                   SETOFF                                       17
068900960626     C                   END
069000970721     C**
069100970721     C* CALL TISI96R PER AVERE LA VERSIONE
069200970721     C**
069300970721     C                   MOVEL     FVVDFV        I96DRI
069400970721     C                   CLEAR                   O96VER
069500970721     C                   CALL      'TISI96R'
069600970721     C                   PARM                    I96DRI
069700970721     C                   PARM                    O96VER
069800970721     C**
069900970721     C                   MOVEL     O96VER        KVER
070000930127     C*
070100930127     C* CARICAMENTO SFILE
070200930127     C                   EXSR      CARSFL
070300930127     C*
070400930127     C     ENDCT1        ENDSR
070500941216     C**************************************************************************
070600941216     C*    CARICAMENTO SFILE
070700941216     C**************************************************************************
070800930127     C     CARSFL        BEGSR
070900941216     C*
071000930519     C* DATA FOGLIO ARRIVI
071100920709     C                   MOVE      FVVDFV        G02INV
071200920709     C                   MOVE      *ZEROS        G02DAT
071300920709     C                   MOVEL     3             G02ERR
071400941221     C                   CALL      'XSRDA8'
071500920709     C                   PARM                    WLBDAT
071600920709     C                   Z-ADD     G02DAT        VIDDFA
071700911008     C*
071800960205     C* CAMPO USATO PER IMMETTERE LA DESCRIZIONE RITARDO/ANTICIPO
071900960205     C                   CLEAR                   VI2GES
072000960205     C* CAMPO USATO PER VISUALIZZARE IL FV
072100960205     C                   CLEAR                   VI2VFV                          VISUAL FV
072200960205     C* STAMPA ELENCO SPEDIZIONI NON PARTITE
072300960205     C                   MOVEL     'N'           VI2STA                          STAMPA ANOM
072400141110     c* aggiungo eventuali filiali estere secondi livello della fgs
072500141110     c* per permettere al terminal di abbinare eventuali diretti a filiale estera
072600141110     c                   Clear                   Trul09ds
072700141110     c                   Eval      I09Mod = 'TFU'
072800141110     c                   Eval      I09Tfa = vidfgs
072900141110     c                   Eval      I09dta = fvvdfv
073000141110     c                   Call      'TRUL09R'
073100141110     c                   Parm                    Trul09ds
073200141110     c                   if        mec(1)>*zeros
073300141110     c                   eval      in=%lookup(000:l6)
073400141110     C                   Z-ADD     1             XX                3 0
073500141110     c                   dow       xx<=%elem(mec) and mec(xx)>*zeros and
073600141110     c                             in<=%elem(l6)
073700141110     c* verifico se filiale estera e in questo caso la carico in £6
073800141110     C                   clear                   OG143
073900141110     c     mec(xx)       chain     azorg01l
074000141110     c                   if        %found(azorg01l)
074100141110     c                   movel     orgde3        og143
074200141110     c                   endif
074300141110     c                   if        §ogntw='EEX' or §ogntw='DPD' or §ogntw='FED'
074400141110     c                   eval      l6(in)=mec(xx)
074500141110     c                   add       1             in
074600141110     c                   endif
074700141110     c                   add       1             xx
074800141110     c                   enddo
074900141110     c                   endif
075000960205     C*
075100920428     C* CARICO FOGLI VIAGGIO GIA' ABBINATI SE ESISTONO
075200960318     C* CARICO SOLO QUELLI CON FVALAI = ALLA FIL. GESTIONE FOGLIO
075300911008     C                   Z-ADD     0             NRR
075400911008     C                   MOVEA     *ZEROS        FV
075500980109     C                   MOVEA     *ZEROS        F2
075600980109     C                   Z-ADD     1             XX                3 0
075700980109    1C     L6(XX)        DOWGT     0
075800941221     C     KFVA2         SETLL     FNFVA02L
075900941221     C     KFVA2         READE     FNFVA02L                               30
076000911008     C*
076100980109    2C     *IN30         DOWEQ     '0'
076200960318     C*
076300980109     C                   MOVEL     FVAKEY        W0080             8 0
076400980109     C     W0080         LOOKUP    F2                                     39
076500980109    3C     *IN39         IFEQ      *OFF
076600921119     C                   MOVEL     FVANFV        VI2FVA                          NUM FOGLIO
076700921119     C                   MOVEL     FVALNP        VI2LNP                          LINEA PART
076800000628     C                   MOVEL     FVALAI        VI2LAI                          ARRIVO INV
076900951120     C*
077000960205     C                   CLEAR                   VI2AEH
077100960402     C                   CLEAR                   VI2AST
077200000628     C                   CLEAR                   VI2PTV
077300951120     C                   CLEAR                   VI2RTC
077400951121     C                   CLEAR                   SAVRTC
077500951120     C                   CLEAR                   VI2DTC
077600951120     C                   CLEAR                   VI2DES
077700960222     C                   MOVEL     '1'           WFORZ
077800960222     C                   CLEAR                   WFORZ2
077900960227     C                   CLEAR                   WFORZ3
078000960222     C                   CLEAR                   WPRVOL
078100960205     C*
078200960205     C                   EXSR      DATARR
078300951115     C*
078400911010     C                   ADD       1             NRR
078500921119     C                   Z-ADD     FVAKEY        FV(NRR)
078600980109     C                   MOVEL     FVAKEY        F2(NRR)
078700941216     C                   WRITE     LR25DSF
078800980109    3C                   ENDIF
078900911008     C*
079000941221     C     KFVA2         READE     FNFVA02L                               30
079100980109    2C                   ENDDO
079200980109     C                   ADD       1             XX
079300980109    1C                   ENDDO
079400921118     C*
079500980109    2C     *IN17         IFEQ      *OFF
079600911008     C* FINISCO DI PULIRE IL SUBFILE
079700920316     C                   ADD       1             NRR               5 0
079800930129     C*
079900930129     C     NRR           DIV       13            RIS               4 0
080000930129     C                   MVR                     RESTO             3 0
080100930129     C*
080200930129     C                   Z-ADD     RESTO         B
080300911008     C                   EXSR      PULSFL
080400960626     C                   ELSE
080500960626     C                   Z-ADD     NRR           SAVNRR            5 0
080600961014     C     NRR           COMP      0                                      72
080700960626     C*
080800960626     C                   END
080900930127     C*
081000930127     C                   ENDSR
081100941216     C**************************************************************************
081200941216     C*    CONTROLLO SUB FILE
081300941216     C**************************************************************************
081400911008     C     CTRSFL        BEGSR
081500941216     C*
081600921119     C                   SETOFF                                       904509
081700911009     C                   Z-ADD     1             NRR
081800040120     C                   Z-ADD     0             X                 3 0
081900911011     C                   MOVEA     *ZEROS        FVS
082000920316     C                   MOVEA     *BLANKS       STS
082100921119     C                   MOVEA     *BLANKS       ATS
082200951117     C                   MOVEA     *ZEROS        TRN
082300951117     C                   MOVEA     *ZEROS        DTN
082400040122     C                   MOVEA     *ZEROS        DFV
082500960222     C                   CLEAR                   DHA
082600911009     C*
082700911009     C* CONTROLLO TUTTO IL SUBFILE
082800951109    1C     NRR           DOWLE     SAVNRR
082900941216     C     NRR           CHAIN     LR25DSF                            30
083000911008     C*
083100951109    2C  N30              DO
083200921123     C                   SETOFF                                       01
083300911009     C*
083400911009     C* RICERCA IN TUTTI I FOGLI VIAGGIO
083500960627   2AC     *IN17         IFEQ      *OFF
083600960627     C*
083700950328     C     '?'           SCAN      VI2LNP                                 09
083800951109    3C   09VI2FVA        IFGT      *ZEROS
083900920327     C                   SETON                                        4990
084000920327     C                   SETOFF                                       09
084100920327     C                   GOTO      ENDCTS
084200951109    3C                   END
084300971002     C*
084400971002     C* SE RICHIMAMATO DA LR26 NON POSSO RITORNARCI
084500971002     C   09RICLAN        IFEQ      '6'
084600971002     C                   SETON                                        5090
084700971002     C                   SETOFF                                       09
084800971002     C                   GOTO      ENDCTS
084900971002     C                   ENDIF
085000920327     C*
085100951109    3C   09              DO
085200960205     C                   EXSR      PULRIG
085300941221     C                   CLEAR                   PARA26
085400921119     C                   MOVEA     *ZEROS        FVA
085500941221     C                   MOVEL     'FNLR25R'     P26PGM
085600941221     C                   MOVEL     VIDFGS        P26LAI
085700941221     C                   MOVEL     SIMFEL        P26FEL
085800950104     C                   MOVEL     'N'           P26ABB                         N=NON ABBINATI
085900971002     C* RICHIAMATO DA FNLR25R
086000971002     C                   MOVEL     '5'           P26LAN
086100941221     C                   MOVEL     PARA26        KPJBU
086200941221     C                   CALL      'FNLR26R'
086300911009     C                   PARM                    KPJBA
086400921119     C                   PARM                    FVA
086500941221     C                   MOVEL     KPJBU         PARA26
086600911009     C*
086700911009     C* SE SELEZIONATI FV LI SCRIVO
086800911009     C                   MOVEL     *ZEROS        VI2FVA
086900911010     C                   MOVEL     *ZEROS        VI2LNP
087000941216     C                   UPDATE    LR25DSF
087100911009     C*
087200911009     C                   SUB       1             NRR
087300911009     C                   Z-ADD     1             I                 4 0
087400911009     C*
087500980204     C     I             DOWLE     70
087600980204    4C     FVA(I)        ANDNE     *ZEROS
087700911010     C                   ADD       1             NRR
087800941216     C     NRR           CHAIN     LR25DSF                            30
087900911009     C*
088000911009     C* CREO NUOVA PAGINA DEL SUBFILE
088100920316     C   30              Z-ADD     NRR           COMNRR            5 0
088200920316     C   30              Z-ADD     1             B
088300911009     C   30              EXSR      PULSFL
088400920316     C   30              Z-ADD     COMNRR        NRR
088500941216     C   30NRR           CHAIN     LR25DSF                            30
088600911010     C* MEMORIZZO SOLO SE E' VUOTO ALTRIMENTI SU ALTRA RIGA
088700951109    5C     VI2LNP        IFEQ      *ZEROS
088800911010     C     VI2LNP        OREQ      *BLANKS
088900951109    6C     VI2FVA        IFEQ      0
089000911009     C                   MOVEL     FVA(I)        VI2LNP
089100911009     C                   MOVE      FVA(I)        VI2FVA
089200980109     C* CERCO SULLA L6
089300980109     C                   Z-ADD     1             XX
089400980109    7C     L6(XX)        DOWGT     0
089500960205     C                   MOVEL     VI2LNP        COMLNP
089600980112     C                   MOVEL     L6(XX)        COMLAI
089700960205     C     KFVA1         CHAIN(N)  FNFVA01L                           31
089800980109    8C     *IN31         IFEQ      *OFF
089900980109     C                   MOVEL     L6(XX)        VI2LAI
090000980204     C     VI2KEY        LOOKUP    FVS                                    31
090100980204     C  N31              EXSR      DATARR
090200980204     C  N31              UPDATE    LR25DSF
090300980204     C   31              SUB       1             NRR
090400980204     C                   ADD       1             I
090500980109     C                   Z-ADD     30            XX
090600980109   X8C                   ELSE
090700980109     C                   ADD       1             XX
090800980109    7C                   ENDIF
090900980109    6C                   ENDDO
091000980109     C**
091100951109    6C                   END
091200951109    5C                   END
091300911009     C*
091400951109    4C                   END
091500980204     C**
091600980204     C* SE C'E' MESSAGGIO EMETTO
091700980204     C     P26MSG        IFNE      *BLANKS
091800980204     C                   MOVEL     P26MSG        VCCMSG
091900980204     C                   SETON                                        28
092000980204     C                   CLEAR                   P26MSG
092100980204     C                   ENDIF
092200911009     C* ESCO DA CONTROLLI
092300911009     C                   SETON                                        90
092400951109    4C     NRR           IFLT      1
092500920327     C                   Z-ADD     1             NRR
092600951109    4C                   END
092700951109     C                   GOTO      ENDCTS
092800951109    3C                   END
092900960627     C*
093000960627   2AC                   END
093100911009     C*
093200951109    3C     VI2LNP        IFNE      *ZEROS
093300911010     C     VI2LNP        ANDNE     *BLANKS
093400920709     C     VI2FVA        ORNE      0
093500960202     C*
093600911010     C* LINEA PARTENZA
093700911010     C                   TESTN                   VI2LNP               31
093800911008     C  N31              SETON                                        4490
093900911008     C  N31              GOTO      ENDCTS
094000911008     C*
094100911010     C                   MOVEL     VI2LNP        COMLNP            3 0
094200911010     C*
094300951109    4C     COMLNP        IFEQ      0
094400920709     C     VI2FVA        ANDNE     0
094500911010     C                   SETON                                        4490
094600951109    4C                   END
094700951109    4C     COMLNP        IFNE      0
094800920709     C     VI2FVA        ANDEQ     0
094900911010     C                   SETON                                        4690
095000951109    4C                   END
095100911010     C*
095200911010     C   90              GOTO      ENDCTS
095300911010     C*
095400980109     C                   Z-ADD     1             XX
095500980109     C                   CLEAR                   WTROV
095600980109     C**
095700980109    4C     L6(XX)        DOWGT     0
095800980109     C                   Z-ADD     L6(XX)        COMLAI
095900980112     C                   MOVEL     L6(XX)        VI2LAI
096000980109     C     KFVA1         CHAIN(N)  FNFVA01L                           32
096100980109    5C     *IN32         IFEQ      *ON
096200980109     C                   ADD       1             XX
096300980109   X5C                   ELSE
096400980109     C                   MOVEL     'S'           WTROV             1
096500980109     C                   Z-ADD     30            XX
096600911010     C*
096700921123     C* GIA' ABBINATO CON ALTRO FOGLIO VIAGGIO
096800980109    6C     FVANFA        IFNE      0
096900921123     C     FVANFA        ANDNE     COMNFV
097000030723     c                   movel     MSG(4)        VCCmsg
097100030723     c                   eval      %subst(VCCmsg:54:1) =
097200030723     c                               %trim(%editw(FVAnpg:'0 '))
097300030723     c                   eval      %subst(VCCmsg:58:5) =
097400030723     c                               %trim(%editw(FVAnfa:'0     '))
097500951121     C                   SETON                                        459028
097600921123     C                   GOTO      ENDCTS
097700980109    6C                   END
097800950104     C*
097900950104     C     VI2KEY        LOOKUP    FVS                                    31
098000950104     C* MEMORIZZO NELLA SCHIERA DEI FV SELEZIONATI
098100980109    6C     *IN31         IFEQ      *OFF
098200980109     C                   ADD       1             X
098300980109     C                   Z-ADD     VI2KEY        FVS(X)
098400980109     C                   MOVEL     VI2STA        STS(X)
098500980109     C                   MOVEL     FVAATR        ATS(X)
098600980109     C                   Z-ADD     FVATRN        TRN(X)
098700980109     C                   Z-ADD     FVADTN        DTN(X)
098800040122     C                   Z-ADD     FVADfv        Dfv(X)
098900980109     C                   MOVEA     WDHARR        DHA(X)
099000980109     C                   ELSE
099100950104     C* DOPPIO
099200980109     C                   SETON                                        4890
099300980109     C                   GOTO      ENDCTS
099400980109    6C                   ENDIF
099500950104     C*
099600960205     C                   EXSR      DATARR
099700980109    5C                   ENDIF
099800980109    4C                   ENDDO
099900980109     C** FOGLIO NON TROVATO
100000980109     C     WTROV         IFEQ      *BLANKS
100100980109     C                   SETON                                        4690
100200980109     C                   GOTO      ENDCTS
100300980109     C                   ENDIF
100400950104     C*
100500950104     C* SE IMMESSO VALORE NEGATIVO TRASFORMO IN POSITIVO
100600950104     C                   MLLZO     1             VI2AEH
100700950104     C*
100800950104     C                   MOVEL     VI2AEH        HH                2 0
100900950104     C                   MOVE      VI2AEH        MM                2 0
101000950104     C*
101100951109    4C     HH            IFGT      23
101200960205     C     MM            ORGT      59
101300960205     C     VI2AEH        OREQ      0
101400950104     C                   SETON                                        5690
101500950104     C                   GOTO      ENDCTS
101600951109    4C                   ENDIF
101700930129     C*
101800951114     C* CONTROLLO DATA ARRIVO MEZZO (DATA E ORA SONO OBBLIGATORIE)
101900930129     C                   Z-ADD     VI2AED        G02DAT
102000930129     C                   MOVEL     *BLANKS       G02ERR
102100941221     C                   CALL      'XSRDA8'
102200930129     C                   PARM                    WLBDAT
102300930129     C*
102400951115    4C     G02ERR        IFEQ      '1'
102500930129     C                   SETON                                        5590
102600930129     C                   GOTO      ENDCTS
102700951115    4C                   ENDIF
102800951228     C                   Z-ADD     G02INV        WAED8
102900941221     C*
103000941221     C* DATA ARRIVO NON PUO' ESSERE < DATA FOGLIO
103100951115    4C     G02INV        IFLT      FVADFV
103200930129     C                   SETON                                        5490
103300930129     C                   GOTO      ENDCTS
103400951115    4C                   ENDIF
103500030924      * Per foglio illuminato
103600030924    4c                   If        FvaAtr <> *Blanks
103700030924      * cerco il terminal di arrivo
103800030924     c                   ExSr      RicTfa
103900030924      * se secondo livello in arrivo su altro AS
104000030924    5c                   If        Simfel <> d98Tfa and
104100030924      * Controllo l'ora arrivo reale
104200030924      * non deve essere maggiore dell'ora di abbinamento foglio
104300030924     c                             vi2aeh <> sa2aeh and
104400030924     c                             vi2aeh >  sa2aeh
104500030924     c                   If        sa2aeh > *Zeros
104600030924     c                   Move      sa2aeh        worario
104700030924     c                   Else
104800030924     c                   Move      wtime         worario
104900030924     c                   EndIf
105000030924     c                   Eval      VccMsg = Msg(19)
105100030924     c                   Eval      %subst(VccMsg:72:2) = wore
105200030924     c                   Eval      %subst(VccMsg:75:2) = wminuti
105300030924     c                   Eval      *In28 = *On
105400030924     c                   Eval      *In53 = *On
105500030924     c                   Eval      *In90 = *On
105600030924     c                   GoTo      EndCts
105700030924    5c                   EndIf
105800030924    4c                   EndIf
105900941221     C*
106000960227     C* PULISCO I CAMPI DI FORZATURA SOLO SE VARIANO DATA/ORA ARRIVO
106100960227    4C     SA2AED        IFNE      VI2AED
106200960227     C     SA2AEH        ORNE      VI2AEH
106300960227     C     FVATRN        IFGT      0
106400960227     C                   CLEAR                   WFORZ
106500960227     C                   CLEAR                   WFORZ2
106600960227     C                   ENDIF
106700960227     C*
106800960227     C                   CLEAR                   WFORZ3
106900960227     C                   MOVEL     VI2AED        SA2AED
107000960227     C                   MOVEL     VI2AEH        SA2AEH
107100960227    4C                   ENDIF
107200960227     C*
107300960227     C* DATA/ORA ARRIVO NON POSSONO ESSERE MAGGIORI DELLA DATA/ORA
107400960227     C*   CORRENTE: ENTER PER FORZARE
107500960227    4C     WFORZ3        IFEQ      ' '
107600000606     C* DATA ARRIVO NON PUO' ESSERE > DATA FOGLIO ARRIVI
107700000606    5C     WAED8         IFGT      FVVDFV
107800000606     C                   MOVEL     MSG(20)       VCCMSG
107900000606     C                   SETON                                        289061
108000000606     C                   MOVEL     '1'           WFORZ3
108100000606     C                   GOTO      ENDCTS
108200000606    5C                   ENDIF
108300960227    4C                   ENDIF
108400960227     C*
108500951117     C* CONTROLLO SE PRESENTE   R I T A R D O / A N T I C I P O
108600951116     C                   SETOFF                                       18
108700951115     C* CONTROLLO SE SI TRATTA DI FOGLIO VIAGGIO ABBINATO AD UN TRAINO
108800951116    4C     FVATRN        IFGT      0
108900960205     C*
109000960227     C* SOLO SE F.VIAGGIO NON E' ILLUMINATO
109100960227   5AC     FVAATR        IFEQ      ' '
109200951115     C*
109300951117     C* SE SONO STATI IMMESSI PIU' F.VIAGGIO CON STESSO NUM.TRAINO E
109400960222     C*   DATA TRAINO CONTROLLO ABBIANO LA STESSA DATA/ORA/CAUS. ARRIVO
109500951117     C                   Z-ADD     1             Y                 3 0
109600951120     C     FVATRN        LOOKUP    TRN(Y)                                 35
109700951117    5C     *IN35         IFEQ      *ON
109800951117     C     VI2KEY        ANDNE     FVS(Y)
109900951117     C     FVADTN        ANDEQ     DTN(Y)
110000951121     C     WDHARR        ANDNE     DHA(Y)
110100030723     c                   movel     MSG(7)        VCCmsg
110200030723     c                   eval      %subst(VCCmsg:07:3) = VI2lnp
110300030723     c                   eval      %subst(VCCmsg:11:5) =
110400030723     c                               %trim(%editc(VI2fva:'Z'))
110500030723     c                   z-add     FVS(Y)        WKEY
110600030723     c                   eval      %subst(VCCmsg:19:3) =
110700030723     c                               %trim(%editw(Wlnp:'0   '))
110800030723     c                   eval      %subst(VCCmsg:23:5) =
110900030723     c                               %trim(%editc(Wfva:'Z'))
111000030723     c                   eval      %subst(VCCmsg:40:7) =
111100030723     c                               %editc(FVAtrn:'Z')
111200951117     C                   SETON                                        289061
111300951117     C                   GOTO      ENDCTS
111400951117    5C                   ENDIF
111500951109     C*
111600951128     C*
111700951128     C* CONTROLLO SE ESISTONO ALTRI F.VIAGGIO ABBINATI AD ALTRO F.ARRIVI
111800960222     C*   CON LO STESSO NUMERO E DATA TRAINO MA DIVERSE DATA/ORA/CAUS.
111900960222     C*   ARRIVO: SE ESISTONO EMETTO MESSAGGIO FORZABILE PER AVVISARE
112000951128    5C     WFORZ2        IFEQ      ' '
112100951128     C     KFVA4         SETLL     FNFVA004
112200951128     C     KFVA4         READE     FNFVA004                               34
112300951128    6C     *IN34         DOWEQ     *OFF
112400960202    7C     F04NFA        IFGT      0
112500960202     C     F04NFA        ANDNE     COMNFV
112600960227     C     F04LAI        ANDEQ     FVALAI
112700960202    8C     F04AED        IFNE      WAED8
112800960202     C     F04AEH        ORNE      VI2AEH
112900960222     C     F04RTA        ORNE      VI2RTC
113000030723      * Foglio letto già abbinato con data reale <> di arrivo
113100030723     c                   movel     MSG(10)       VCCmsg
113200030723      * - foglio in errore
113300030723     c                   eval      %subst(VCCmsg:01:3) =
113400030723     c                               %trim(%editw(FVAlnp:'0   '))
113500030723     c                   eval      %subst(VCCmsg:05:5) =
113600030723     c                               %trim(%editc(FVAnfv:'Z'))
113700030723      * - data di arrivo
113800030723     c                   z-add     F04aed        G02inv
113900030723     c                   clear                   G02dat
114000030723     c                   movel     3             G02err
114100030723     c                   call      'XSRDA8'
114200030723     c                   parm                    WLBDAT
114300030723     c                   eval      %subst(VCCmsg:36:5) =
114400030723     c                               %subst(%editc(G02dat:'Y'):1:5)
114500030723      * - ora di arrivo
114600030723     c                   eval      %subst(VCCmsg:42:5) =
114700030723     c                               %editw(F04aeh:'0 :  ')
114800030723     c                   eval      %subst(VCCmsg:48:1) = F04rta
114900030723     c                   eval      %subst(VCCmsg:56:3) =
115000030723     c                               %trim(%editw(F04lnp:'0   '))
115100030723     c                   eval      %subst(VCCmsg:60:5) =
115200030723     c                               %trim(%editc(F04nfv:'Z'))
115300951128     C                   SETON                                        289061
115400951128     C                   MOVEL     '1'           WFORZ2
115500951128     C                   GOTO      ENDCTS
115600951128    8C                   ENDIF
115700951128    7C                   ENDIF
115800951128     C     KFVA4         READE     FNFVA004                               34
115900951128    6C                   ENDDO
116000951128    5C                   ENDIF
116100951128     C*
116200960227   5AC                   ENDIF
116300951115    4C                   ENDIF
116400951114     C*
116500960626     C*
116600960626     C* CONTROLLO ESISTENZA RITARDO SOLO SE IL FV NON E' ILLUMINATO E
116700960626     C*   DATA/ORA TEORICA E' DIVERSA DA DATA/ORA REALE
116800960626    4C     FVADTA        IFGT      0
116900960626    5C     FVADTA        IFNE      WAED8
117000960626     C     FVAHTA        ORNE      VI2AEH
117100960626     C                   CLEAR                   DSTL35
117200960626     C                   MOVEL     'C'           D35TUP
117300970825     C                   MOVEL     'A'           D35EPA
117400960626     C                   MOVEL     FVADTA        D35DTE
117500960626     C                   MOVEL     FVAHTA        D35HTE
117600960626     C                   MOVEL     WAED8         D35DRE
117700960626     C                   MOVEL     VI2AEH        D35HRE
117800960626     C                   MOVEL     DSTL35        KPJBU
117900960626     C                   CALL      'TNTL35R'
118000960626     C                   PARM                    KPJBA
118100960626     C                   MOVEL     KPJBU         DSTL35
118200960626     C                   MOVEL     '1'           WTL35             1
118300960626     C*
118400960626     C* D35FLG <> " "  ---> PRESENTE RITARDO/ANT. NELL'ARRIVO DEL MEZZO
118500960626    6C     D35FLG        IFNE      ' '
118600960626     C                   SETON                                        18
118700960626    6C                   ENDIF
118800960626    5C                   ENDIF
118900960626     C*
119000960626   X4C                   ELSE
119100960626     C*
119200960626     C* SE FV ILLUMINATO
119300960626    5C     FVAATR        IFNE      ' '
119400960626     C* CALCOLO I TEMPI DI TRAZIONE ED IN BASE ALLE SOGLIE CALCOLO
119500960626     C*   L'ORA AL DI SOPRA DELLA QUALE CHIEDERE IL RITARDO
119600960626     C                   EXSR      CALSOG
119700960626     C*
119800960626     C* CONTROLLO SE PRESENTE RITARDO
119900960626     C                   SELECT
120000960626     C     WAED8         WHENEQ    DATSOG
120100960626    6C     VI2AEH        ANDGT     ORASOG
120200960626     C                   SETON                                        18
120300960626     C*
120400960626     C     WAED8         WHENGT    DATSOG
120500960626     C                   SETON                                        18
120600960626     C                   ENDSL
120700960626     C*
120800960626     C                   ELSE
120900960626     C                   MOVEL     '1'           WFORZ
121000960626    5C                   ENDIF
121100960626    4C                   ENDIF
121200960626     C*
121300960626     C* 17 ON -  DETERMINO SE CAUSALE RITARDO MANUTENZIONABILE
121400960626     C                   MOVE      *BLANKS       WCOSA             1            TUTTO
121500960626    4C     *IN17         IFEQ      *ON
121600960626    5C     *IN18         IFEQ      *ON
121700960626    6C     SAVRTC        IFNE      *BLANKS
121800960626     C                   MOVE      'N'           WCOSA                          NESSUNA VAR.
121900960626    6C                   END
122000960626   X5C                   ELSE
122100960626    6C     SAVRTC        IFNE      *BLANKS
122200960626     C                   MOVE      'T'           WCOSA                          SOLO TOGLIER
122300960626   X6C                   ELSE
122400960626     C                   MOVE      'N'           WCOSA                          NESSUNA VAR.
122500960626    6C                   END
122600960626    5C                   END
122700960626    4C                   END
122800960626     C*
122900960626    4C                   SELECT
123000960626     C* WCOSA='N' --> CAUSALE/DESCRIZIONI RITARDO NON MANUTENZIONABILI
123100960626     C     WCOSA         WHENEQ    'N'
123200960626    5C     VI2RTC        IFNE      SAVRTC
123300960626    6C     SAVRTC        IFNE      *BLANKS
123400960626     c* Impossibile modificare o togliere la causale
123500030723     c                   movel     MSG(14)       VCCmsg
123600030723     c                   eval      %subst(VCCmsg:71:1) = SAVrtc
123700960626   x6C                   ELSE
123800960626     c* Impossibile immettere la causale
123900960626     C                   MOVEL     MSG(15)       VCCMSG
124000960626    6C                   END
124100960626     C                   SETON                                        906028
124200960626     C                   GOTO      ENDCTS
124300960626    5C                   END
124400960626    5C     VI2GES        IFNE      *BLANKS
124500960626     c* impossibile modificare la descrizione
124600960626     C                   MOVEL     MSG(16)       VCCMSG
124700960626     C                   SETON                                        906228
124800960626     C                   GOTO      ENDCTS
124900960626    5C                   END
125000960626     C* WCOSA='T' --> LA CAUSALE PUO' SOLO ESSERE ABBLENCATA
125100960626     C     WCOSA         WHENEQ    'T'
125200960626    5C     VI2RTC        IFNE      *BLANKS
125300960626     C     VI2RTC        ANDNE     SAVRTC
125400030723     c                   movel     MSG(17)       VCCmsg
125500030723     c                   eval      %subst(VCCmsg:66:1) = SAVrtc
125600960626     C                   SETON                                        906028
125700960626     C                   GOTO      ENDCTS
125800960626    5C                   END
125900960626    4C                   ENDSL
126000960626     C*
126100951115     C* RICERCA CAUSALE RITARDO/ANTICIPO
126200951115    4C     VI2RTC        IFEQ      '?'
126300951116     C                   MOVEL     'TC'          §COD
126400951115     C                   MOVEL     *BLANKS       VI2RTC
126500951116     C                   MOVEL     *BLANKS       §DES
126600951115     C                   CALL      'X§TABER'
126700951115     C                   PARM                    CODUT
126800951116     C                   PARM                    §COD
126900951116     C                   PARM                    §KEY
127000951116     C                   PARM                    §DES
127100951116     C                   MOVEL     §KEY          VI2RTC
127200951116     C                   MOVEL     §DES          VI2DTC
127300960207     C                   SETON                                        906045
127400951115     C                   GOTO      ENDCTS
127500951115    4C                   ENDIF
127600951115     C*
127700951115     C* CONTROLLO VALIDITA' CAUSALE
127800951115    4C     VI2RTC        IFNE      ' '
127900951115     C                   MOVEL     'TC'          COD
128000951115     C                   MOVEL(P)  VI2RTC        KEY
128100951115     C     KTAB          CHAIN     TABEL                              34
128200951115    5C     *IN34         IFEQ      *OFF
128300951115     C     TBLFLG        ANDNE     ' '
128400951115     C                   SETON                                        34
128500951115    5C                   ENDIF
128600951115     C*
128700951115    5C     *IN34         IFEQ      *ON
128800951115     C                   MOVEL     MSG(1)        VCCMSG
128900951115     C                   SETON                                        289060
129000951115     C                   GOTO      ENDCTS
129100951115   X5C                   ELSE
129200951115     C                   MOVEL     TBLUNI        VI2DTC
129300951115    5C                   ENDIF
129400951115     C*
129500951115   X4C                   ELSE
129600951115     C                   CLEAR                   VI2DTC
129700951115    4C                   ENDIF
129800960205     C*
129900951115     C* SE ARRIVO IN RITARDO/ANTICIPO --> CAUSALE OBBLIGATORIA
130000951115    4C     *IN18         IFEQ      *ON
130100951115     C     VI2RTC        ANDEQ     ' '
130200951228     C                   MOVEL     MSG(2)        VCCMSG
130300951115     C                   SETON                                        289060
130400951115     C                   GOTO      ENDCTS
130500951115    4C                   ENDIF
130600951115     C*
130700951115     C* SE MODIFICANDO LA DATA/ORA ARRIVO EFFETTIVA IL RITARDO/ANTICIPO
130800951115     C*   SCOMPARE, CHIEDERE FORZABILE DI ELIMINARE LA CAUSALE
130900951228    4C  N18WFORZ         IFEQ      ' '
131000960202     C     VI2RTC        ANDNE     ' '
131100951115     C                   MOVEL     MSG(3)        VCCMSG
131200951115     C                   SETON                                        289060
131300951228     C                   MOVEL     '1'           WFORZ
131400951115     C                   GOTO      ENDCTS
131500951115    4C                   ENDIF
131600951115     C*
131700951115     C* SE IMMESSA LA "G" DI CAUSALE GENERICA, RICHIEDERE OBBLIGATORIA
131800951115     C*   LA DESCRIZIONE RITARDO/ANTICIPO
131900951115    4C     VI2RTC        IFEQ      'G'
132000951115     C* W001A = " " --> NON ESISTE LA DESCRIZIONE RITARDO/ANTICIPO
132100960205     C     VI2DES        ANDEQ     *BLANKS
132200951115     C     VI2GES        ANDEQ     ' '
132300951228     C                   MOVEL     MSG(5)        VCCMSG
132400960205     C                   SETON                                        289062
132500951115     C                   GOTO      ENDCTS
132600951115    4C                   ENDIF
132700951115     C*
132800951115     C* CONTROLLO SE SI VUOLE INSERIRE LA DESCRIZIONE RITARDO/ANTICIPO
132900951115    4C     VI2GES        IFNE      ' '
133000960201     C                   CLEAR                   DSLV15
133100960201     C                   MOVEL     'M'           D15TUP
133200960201     C                   MOVEL     'A'           D15EPA
133300960201     C                   MOVEL     VI2RTC        D15RTC
133400960201     C                   MOVEL     FVALNP        D15LNP
133500960201     C                   MOVEL     FVANFV        D15NFV
133600960201     C                   MOVEL     FVALAI        D15LAI
133700960201     C                   MOVEL     'R'           D15TRC
133800960201     C                   MOVEL     FVATRN        D15TRN
133900960222     C                   MOVEL     FVATTR        D15TTR
134000960201     C                   MOVEL     FVADFV        D15DFV
134100960305     C                   MOVEL     FVAATR        D15ATR
134200951115     C*
134300960201     C                   MOVEL     DSLV15        KPJBU
134400951115     C                   CALL      'FNLV15R'
134500951115     C                   PARM                    KPJBA
134600960201     C                   MOVEL     KPJBU         DSLV15
134700960202     C                   MOVEL     '1'           WLV15             1
134800951115     C*
134900960201     C* SE VARIATO QUALCOSA IN FNFV4 ED ESCO DALL'ABBINAMENTO SENZA
135000960201     C*   CONFERMARE SOTTOMETTO PGM DI TRASMISSIONE
135100960201    5C     D15FLG        IFNE      ' '
135200960201     C                   MOVEL     '1'           WAGG              1
135300951115    5C                   ENDIF
135400951121     C*
135500951121     C* PULISCO IL CAMPO UTILIZZATO PER VISUALIZZARE LA DESCRIZIONE
135600951121     C                   CLEAR                   VI2GES
135700951115     C*
135800951115     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
135900951115     C*   RIEMPIO CAMPO CHE NE INDICA L'ESISTENZA
136000951115     C                   EXSR      CTRDRA
136100960201    5C     W001A         IFNE      ' '
136200951130     C                   MOVEL     W010A         VI2DES
136300960205   X5C                   ELSE
136400960205     C                   CLEAR                   VI2DES
136500960205    5C                   ENDIF
136600960205    4C                   ENDIF
136700951121     C*
136800951121     C* SE INSERITA LA DESCRIZIONE RITARDO/ANTICIPO OCCORRE INSERIRE
136900951121     C*   ANCHE LA CAUSALE
137000960205    4C     VI2DES        IFNE      *BLANKS
137100960205     C     VI2RTC        ANDEQ     ' '
137200951228     C                   MOVEL     MSG(6)        VCCMSG
137300951121     C                   SETON                                        289060
137400951121     C                   GOTO      ENDCTS
137500960205    4C                   ENDIF
137600951115     C*
137700951117     C* CONTROLLO SE RICHIESTA LA VISUALIZZAZIONE DEL F.VIAG. PARTENZA
137800951117    4C     VI2VFV        IFEQ      'V'
137900971002     C* NON POSSO ANDARCI SE PROVENGO DA FNLR26
138000971002     C     RICLAN        IFEQ      '6'
138100971002     C                   MOVEL     MSG(18)       VCCMSG
138200971003     C                   SETON                                        289063
138300971002     C                   GOTO      ENDCTS
138400971002     C                   ENDIF
138500971002     C**
138600951120     C                   CLEAR                   PARA26
138700951120     C                   Z-ADD     VI2FVA        FVA(1)
138800951120     C                   MOVEL     'FNLR25R'     P26PGM
138900951120     C                   MOVEL     VI2LNP        P26LNP
139000951120     C                   MOVEL     VI2LAI        P26LAI
139100971002     C* RICHIAMATO DA FNLR25R
139200971002     C                   MOVEL     '5'           P26LAN
139300951120     C                   MOVEL     PARA26        KPJBU
139400951120     C                   CALL      'FNLR26R'
139500951120     C                   PARM                    KPJBA
139600951120     C                   PARM                    FVA
139700951120     C*
139800951120     C* PULISCO IL CAMPO UTILIZZATO PER VISUALIZZARE IL FOGLIO
139900951120     C                   CLEAR                   VI2VFV
140000951117    4C                   ENDIF
140100951109     C*
140200951109   X3C                   ELSE
140300960205     C                   EXSR      PULRIG
140400951109    3C                   END
140500951109    2C                   END
140600930127     C*
140700911009     C     ENDCTS        TAG
140800920706     C*
140900941216     C  N09
141000941216     CANN30              UPDATE    LR25DSF
141100920316     C   90              Z-ADD     NRR           REC
141200920316     C   90              Z-ADD     SAVNRR        NRR
141300911009     C                   ADD       1             NRR
141400951109    1C                   END
141500911008     C*
141600911009     C                   ENDSR
141700960304     C**************************************************************************
141800960304     C*  CALCOLO DATA ORA DI SOGLIA PER I FV ILLUMINATI
141900960304     C**************************************************************************
142000960304     C     CALSOG        BEGSR
142100970925     C* PRIMA FACCIO LA CALL AL TNSD99R PER AVERE I TEMPI DI TRAZIONE
142200970925     C                   CLEAR                   DSSD99
142300960304     C                   MOVEL     'T'           D98TLA
142400960304     C                   MOVEL     FVALNP        D98LNP
142500960304     C                   MOVEL     FVALNP        D98TFP
142600960304     C                   MOVEL     FVALAI        D98LNA
142700960304     C                   MOVEL     FVADFV        D98DSP
142800960304     C                   MOVEL     'E'           D98TSP
142900960304     C* CAP
143000960304     C                   CLEAR                   KNAR
143100960304     C                   MOVEL     FVALAI        KLNA
143200970721     C     KCAP          SETLL     AZCPC03L
143300970721     C     KCAP          READE     AZCPC03L                               32
143400960304     C*
143500960304    1C     *IN32         DOWEQ     *OFF
143600960304     C* SE ANNULLATO O FITTIZIO RILEGGO
143700970721     C     CPCATB        IFNE      ' '
143800970722     C     CPCFIT        OREQ      'S'
143900980109     C                   CLEAR                   CPCCAP
144000970721     C     KCAP          READE     AZCPC03L                               32
144100960304     C                   ELSE
144200960304     C                   SETON                                        32
144300960304     C                   ENDIF
144400960304    1C                   ENDDO
144500980109     C*
144600980109     C* SE NON LO TROVO LO PRENDO DA ORGANIGRAMMA
144700980109     C     CPCCAP        IFEQ      *BLANKS
144800980109     C     FVALAI        CHAIN     AZORG01L                           30
144900980109     C  N30              MOVEL     ORGCPF        D98CAD
145000980109     C                   ELSE
145100970721     C                   MOVEL     CPCCAP        D98CAD
145200980109     C                   ENDIF
145300980109     C**
145400970825     C* CERCO TERMINAL DI ARRIVO
145500960304     C                   EXSR      RICTFA
145600960304     C*
145700970925     C                   CALL      'TNSD99R'
145800970925     C                   PARM                    DSSD99
145900960304     C*
146000960304     C* REPERISCO LE DUE ORE DI SOGLIA
146100960304     C                   MOVEL     '1X'          COD
146200960304     C                   MOVEL(P)  FVALAI        KEY
146300960304     C                   MOVE      'OSA  '       KEY
146400960304     C     KTAB          CHAIN     TABEL                              32
146500980209     C     *IN32         IFEQ      *OFF
146600960304     C  N32              MOVEL     TBLUNI        DS1X
146700960304     C                   MOVEL     §1XHHO        HHOUNO
146800960304     C*
146900960304     C                   MOVE      'OSA1 '       KEY
147000960304     C     KTAB          CHAIN     TABEL                              32
147100960304     C  N32              MOVEL     TBLUNI        DS1X
147200960304     C   32              CLEAR                   §1XHHO
147300960304     C                   MOVEL     §1XHHO        HHODUE
147400980209     C                   ELSE
147500980209     C                   MOVEL(P)  '   '         KEY
147600980209     C                   MOVE      'OSA  '       KEY
147700980209     C     KTAB          CHAIN     TABEL                              32
147800980209     C  N32              MOVEL     TBLUNI        DS1X
147900980209     C   32              CLEAR                   §1XHHO
148000980209     C                   MOVEL     §1XHHO        HHOUNO
148100980209     C*
148200980209     C                   MOVE      'OSA1 '       KEY
148300980209     C     KTAB          CHAIN     TABEL                              32
148400980209     C  N32              MOVEL     TBLUNI        DS1X
148500980209     C   32              CLEAR                   §1XHHO
148600980209     C                   MOVEL     §1XHHO        HHODUE
148700980209     C                   ENDIF
148800960304     C*
148900960304     C*
149000960304    1C                   SELECT
149100960304     C     D98HTI        WHENLE    HHOUNO
149200960304     C                   Z-ADD     HHOUNO        ORASOG
149300960304     C                   Z-ADD     D98DTI        DATSOG
149400960304     C*
149500960304     C     D98HTI        WHENLE    HHODUE
149600960304     C                   Z-ADD     HHODUE        ORASOG
149700960304     C                   Z-ADD     D98DTI        DATSOG
149800960304     C*
149900960304   X1C                   OTHER
150000960304     C*
150100960304     C* SOMMO 1 GIORNO E PRENDO LA PRIMA SOGLIA DEL GIORNO DOPO
150200960304     C                   CLEAR                   WLBDAT
150300960304     C                   MOVEL     '3'           G02ERR
150400960304     C                   Z-ADD     D98DTI        G02INV
150500960304     C                   CALL      'XSRDA8'
150600960304     C                   PARM                    WLBDAT
150700960304     C*
150800960304     C* ADD 1 A GIORNI
150900960304     C     G02TGI        ADD       1             GIOTGI
151000960304     C                   CALL      'XSRGI8'
151100960304     C                   PARM                    WGIDAT
151200960304     C                   MOVEL     GIOINV        DATSOG
151300960304     C                   Z-ADD     HHOUNO        ORASOG
151400960304    1C                   ENDSL
151500960304     C                   ENDSR
151600960304     C**************************************************************************
151700960304     C*  PREPARA MESSAGGIO 10
151800960304     C**************************************************************************
151900960304     C     RICTFA        BEGSR
152000970825     C                   CLEAR                   DSLV55
152100970825     C                   MOVEL     'A'           D55TPT
152200970825     C                   MOVEL     FVALAI        D55LIN
152300970825     C                   MOVEL     FVALNP        D55LNP
152400970825     C                   MOVEL     FVADFV        D55DRF
152500970825     C                   CALL      'FNLV55R'
152600970825     C                   PARM                    DSLV55
152700970825     C     D55ERR        IFEQ      ' '
152800970825     C                   MOVEL     D55TFA        D98TFA
152900970825     C                   ELSE
153000970825     C                   MOVEL     FVALAI        D98TFA
153100960304    2C                   ENDIF
153200960304     C*
153300960304     C                   ENDSR
153400951115     C**************************************************************************
153500951115     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
153600951115     C**************************************************************************
153700951115     C     CTRDRA        BEGSR
153800951115     C                   CLEAR                   W001A
153900951130     C                   CLEAR                   W010A
154000951115     C*
154100960202     C     KFV4          SETLL     FNFV401L                               34
154200951115    1C     *IN34         IFEQ      *ON
154300070918     C     KFV4          READE     FNFV401L                               34
154400951115    2C     *IN34         DOWEQ     *OFF
154500951115    3C     FV4ATB        IFEQ      ' '
154600951116     C                   MOVEL     '1'           W001A             1
154700951130     C                   MOVEL     FV4NOT        W010A            10
154800951115     C                   SETON                                        34
154900951115   X3C                   ELSE
155000070918     C     KFV4          READE     FNFV401L                               34
155100951115    3C                   ENDIF
155200951115    2C                   ENDDO
155300951115    1C                   ENDIF
155400951115     C*
155500951115     C                   ENDSR
155600960111     C**************************************************************************
155700960111     C* CONTROLLO COSA PROPORRE NELLA DATA/ORA ARRIVO REALE
155800000628     C* ... carica anche P.O. gestione POSTE
155900960111     C**************************************************************************
156000960111     C     DATARR        BEGSR
156100960205     C****
156200960205     C** D A T A / O R A     A R R I V O
156300960205     C****
156400960205     C* SE NON HO DIGITATO NULLA VERIFICO COSA DEVO PROPORRE
156500960205    1C     VI2AEH        IFEQ      0
156600960205     C*
156700960205     C* SE PRESENTE PROPONGO DATA/ORA ARRIVO REALE IN QUANTO SI TRATTA
156800960205     C*   DI UN FV CHE HO GIA' RICEVUTO IN PRECEDENZA
156900960205    2C     FVAAED        IFGT      0
157000960205     C* DATA ARRIVO REALE
157100960205     C                   Z-ADD     FVAAED        G02INV
157200960205     C                   MOVE      *ZEROS        G02DAT
157300960205     C                   MOVEL     3             G02ERR
157400960205     C                   CALL      'XSRDA8'
157500960205     C                   PARM                    WLBDAT
157600960205     C* LA DATA ARRIVO MEZZO LA VISUALIZZO LUNGA 6
157700960205     C                   MOVE      G02DAT        W0020
157800960205     C                   MOVEL     G02DAT        VI2AED
157900960205     C                   MOVE      W0020         VI2AED
158000960205     C                   MOVEL     VI2AED        SA2AED
158100960205     C* ORA ARRIVO REALE
158200960205     C                   MOVEL     FVAAEH        VI2AEH
158300960205     C                   MOVEL     FVAAEH        SA2AEH
158400960205     C*
158500960205   X2C                   ELSE
158600960205     C*
158700960111     C* PER I FV ABBINATI A TRAINO PROPORRE LA DATA TEORICA; PER GLI
158800960206     C*   ALTRI PROPORRE LA DATA DEL GIORNO CON ORA 00:00
158900960205    3C     FVADTA        IFGT      0
159000960205     C* DATA ARRIVO TEORICA
159100960111     C                   Z-ADD     FVADTA        G02INV
159200960111     C                   MOVE      *ZEROS        G02DAT
159300960111     C                   MOVEL     3             G02ERR
159400960111     C                   CALL      'XSRDA8'
159500960111     C                   PARM                    WLBDAT
159600960205     C                   MOVE      G02DAT        W0020
159700960205     C                   MOVEL     G02DAT        VI2AED
159800960111     C                   MOVE      W0020         VI2AED
159900960205     C                   MOVEL     VI2AED        SA2AED
160000960111     C* ORA ARRIVO TEORICA
160100960111     C                   Z-ADD     FVAHTA        VI2AEH
160200960205     C                   MOVEL     FVAHTA        SA2AEH
160300960206     C*
160400960206   X3C                   ELSE
160500960206     C* SE FV ILLUMINATO NON PROPONGO COME ORA 00:00 MA PROPONGO TIME
160600960206    4C     FVAATR        IFNE      ' '
160700960206     C                   MOVEL     WTIME         VI2AEH
160800960206     C                   MOVEL     VI2AEH        SA2AEH
160900960206    4C                   ENDIF
161000960205    3C                   ENDIF
161100960205    2C                   ENDIF
161200960205    1C                   ENDIF
161300960111     C*
161400960205     C****
161500960205     C** C A U S A L E     R I T A R D O
161600960205     C****
161700960205     C* SE NON HO DIGITATO NULLA VERIFICO SE C'E' QUALCOSA DA PROPORRE
161800960205    1C     VI2RTC        IFEQ      ' '
161900960206     C     WPRVOL        ANDEQ     ' '
162000960205     C*
162100960205     C* SE PRESENTE VISUALIZZO LA CAUSALE RITARDO/ANTICIPO
162200960205     C     FVARTA        ANDNE     ' '
162300960111     C                   MOVEL     FVARTA        VI2RTC
162400960111     C                   MOVEL     FVARTA        SAVRTC
162500960111     C* DECODIFICA
162600960111     C                   MOVEL     'TC'          COD
162700960111     C                   MOVEL(P)  VI2RTC        KEY
162800960111     C     KTAB          CHAIN     TABEL                              34
162900960205    2C     *IN34         IFEQ      *OFF
163000960111     C     TBLFLG        ANDEQ     ' '
163100960111     C                   MOVEL     TBLUNI        VI2DTC
163200960205    2C                   ENDIF
163300960206     C*
163400960206     C                   MOVEL     '1'           WPRVOL
163500960205    1C                   ENDIF
163600960205     C*
163700960205     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
163800960205     C                   EXSR      CTRDRA
163900960205     C* W001A = " " --> NON ESISTE LA DESCRIZIONE RITARDO/ANTICIPO
164000960205    2C     W001A         IFNE      ' '
164100960205     C                   MOVEL     W010A         VI2DES
164200960205    2C                   ENDIF
164300960202     C*
164400960202     C* GIRO DATA PARTENZA
164500960202     C                   MOVE      FVADFV        G02INV
164600960202     C                   MOVE      *ZEROS        G02DAT
164700960202     C                   MOVEL     3             G02ERR
164800960202     C                   CALL      'XSRDA8'
164900960202     C                   PARM                    WLBDAT
165000971223     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
165100960202     C                   MOVEL     G02DAT        VI2DFV
165200960202     C*
165300960202     C* SE E' SOLO BUSTA METTO CAMPO IN R.I.
165400960202    1C     FVAATR        IFEQ      'T'
165500960202     C                   SETON                                        01
165600960202   X1C                   ELSE
165700960202     C                   SETOFF                                       01
165800960202    1C                   END
165900960402     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
166000960402     C*  VISUALIZZO CON 2 ASTERISCHI
166100960402     C                   MOVEL     VI2LNP        COMLNP
166200960402     C     KARB          SETLL     FNARB06L                               34
166300960402     C     *IN34         IFEQ      *ON
166400960402     C                   CLEAR                   VI2AST
166500960402     C                   ELSE
166600960402     C                   MOVEL     '**'          VI2AST
166700960402     C                   ENDIF
166800000628     C* P.O.GESTIONE
166900000628     C     KFWA1         CHAIN     FNFWA01L                           74
167000000628     C     *IN74         IFEQ      *OFF
167100000628     C     FWAFGS        ANDNE     0
167200000628     C                   Z-ADD     FWAFGS        WORKPT            3 0
167300000628     C                   ELSE
167400000628     C                   Z-ADD     FVALNP        WORKPT            3 0
167500000628     C                   ENDIF
167600000628     C                   CLEAR                   OG143
167700000628     C     WORKPT        CHAIN     AZORG                              74
167800000628     C   74              MOVE      '  '          VI2PTV
167900000628     C  N74              MOVEL     ORGDE3        OG143
168000020207     C  N74§OGNTW        IFEQ      'PPT'
168100000628     C                   MOVE      'PT'          VI2PTV
168200000628     C                   ELSE
168300000628     C                   MOVE      '  '          VI2PTV
168400000628     C                   ENDIF
168500960111     C*
168600960111     C                   ENDSR
168700951121     C**************************************************************************
168800951121     C*    CONTROLLO F03 / F12
168900951121     C**************************************************************************
169000951121     C     F03F12        BEGSR
169100951121     C                   SETOFF                                       90
169200951121     C* SE IMMESSA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12 O
169300951121     C*   F3 A MENO CHE NON ESISTEVA GIA' LA CAUSALE
169400951121     C                   Z-ADD     1             NRR
169500951121     C     NRR           CHAIN     LR25DSF                            34
169600951121    1C     *IN34         DOWEQ     *OFF
169700951121     C*
169800951121    2C     VI2LNP        IFNE      *ZEROS
169900951121     C     VI2LNP        ANDNE     *BLANKS
170000951121     C*
170100951121     C* SE IMMESSA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12 O
170200951121     C*   F3 A MENO CHE NON ESISTEVA GIA' LA CAUSALE
170300951121    3C     VI2DES        IFNE      *BLANKS
170400951121    4C     SAVRTC        IFEQ      ' '
170500951121    5C     VI2RTC        IFEQ      ' '
170600951121     C                   MOVEL     MSG(6)        VCCMSG
170700960205     C                   SETON                                        286290
170800951121     C                   GOTO      ENDF
170900951121   X5C                   ELSE
171000951121     C                   MOVEL     MSG(8)        VCCMSG
171100960205     C                   SETON                                        286290
171200951121     C                   GOTO      ENDF
171300951121    5C                   ENDIF
171400951121    4C                   ENDIF
171500951121     C*
171600951121   X3C                   ELSE
171700951121     C* SE ELIMINATA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12
171800951121     C*   O F3 SE LA CAUSALE CHE E' GIA' STATA IMMESSA E' "G-GENERICA"
171900951121    4C     SAVRTC        IFEQ      'G'
172000951121     C                   MOVEL     MSG(9)        VCCMSG
172100960205     C                   SETON                                        286290
172200951121     C                   GOTO      ENDF
172300951121    4C                   ENDIF
172400951121    3C                   ENDIF
172500951121    2C                   ENDIF
172600951121     C*
172700951121     C     ENDF          TAG
172800960205     C* ERRORE
172900960205     C     *IN90         IFEQ      *ON
173000960205     C                   MOVEL     VI2LNP        COMLNP
173100960205     C                   MOVEL     VI2LAI        COMLAI
173200960205     C     KFVA1         CHAIN(N)  FNFVA01L                           32
173300960205     C  N32FVAATR        IFEQ      ' '
173400960205     C                   SETOFF                                       01
173500960205     C                   ELSE
173600960205     C                   SETON                                        01
173700960205     C                   ENDIF
173800960205     C**
173900960205     C                   Z-ADD     NRR           REC
174000960205     C                   SETON                                        34
174100951121     C                   UPDATE    LR25DSF
174200960205     C                   ELSE
174300951121     C*
174400951121     C                   ADD       1             NRR
174500960205     C     NRR           CHAIN     LR25DSF                            34
174600960205     C                   ENDIF
174700951121    1C                   ENDDO
174800951121     C*
174900951121     C                   ENDSR
175000951121     C*
175100941216     C**************************************************************************
175200941216     C*    ELABORAZIONE ABBINAMENTO
175300941216     C**************************************************************************
175400911008     C     ELAB          BEGSR
175500911010     C*
175600930129     C                   Z-ADD     1             NRR
175700950328     C* CAMPI FISSI DEL FILE FNAGB PER AGGIORNAMENTO BOLLE
175800950328     C                   MOVEL     'A'           AGBTBO
175900950328     C                   MOVEL     'AB'          AGBAGB
176000950925     C                   MOVEL     'S'           AGBFBS
176100950925     C                   MOVEL     ' '           AGBFVC
176200930129     C*
176300950621     C* LEGGO TUTTI I FOGLI VIAGGIO ABBINATI
176400951228    1C     NRR           DOWLE     SAVNRR
176500941216     C     NRR           CHAIN     LR25DSF                            30
176600930129     C*
176700951228    2C  N30VI2LNP        IFGT      *ZEROS
176800070917     C                   SETOFF                                       1615
176900951228     C*
177000930129     C                   MOVEL     VI2LNP        COMLNP
177100951227     C* GIRO DATA ARRIVO
177200951227     C                   Z-ADD     VI2AED        G02DAT
177300951227     C                   Z-ADD     *ZEROS        G02INV
177400951227     C                   CALL      'XSRDA8'
177500951227     C                   PARM                    WLBDAT
177600951227     C                   Z-ADD     G02INV        WAED8
177700960304     C* L'AGGIORNAMENTO DEI FV LO ESEGUO PER TUTTI I FV DELLA LNA
177800960304     C*  DELLA L6
177900960304     C                   Z-ADD     1             Y                 3 0
178000960304   2AC     L6(Y)         DOWGT     0
178100960304     C                   MOVEL     L6(Y)         COMLAI
178200921120     C*
178300921120     C* CHAIN FOGLIO PARTENZE PER AGGIONARE DATA E NUM FOGLIO ABBINATO
178400941220     C* E IMPOSTO LINEE ARRIVO E SCARICO DEL FOGLIO
178500941221     C     KFVA1         CHAIN     FNFVA01L                           33
178600960304   2BC     *IN33         IFEQ      *OFF
178700951228     C*
178800960108     C* 16 ON  - E' STATA MODIFICATA LA DATA E ORA ARRIVO REALE OPPURE
178900960108     C*   SI TRATTA DI FOGLIO VIAGGIO CON NUMERO FOGLIO ARRIVI = 0
179000951228    3C     FVAAED        IFNE      WAED8
179100951228     C     FVAAEH        ORNE      VI2AEH
179200960108     C     FVANFA        OREQ      0
179300951228     C                   SETON                                        16
179400070917     c* Memorizzo se cambiata solo la data/ora
179500070918     c                   if        fvanfa>0
179600070918     c                   movel     fvaaed        woldaed
179700070918     c                   movel     fvaaeh        woldaeh
179800070917     C                   SETON                                        15
179900070917     c                   endif
180000951228    3C                   ENDIF
180100951228     C*
180200951228     C* SFLAGGO I FOGLI VIAGGIO SOLO SE HO MODIFICATO DATA/ORA ARRIVO
180300960108     C*   REALE OPPURE LA CAUSALE RITARDO/ANTICIPO OPPURE SE SI TRATTA
180400960108     C*   DI FOGLIO VIAGGIO CON NUMERO FOGLIO ARRIVI = 0
180500951228     C     *IN16         IFEQ      *ON
180600951228     C     FVARTA        ORNE      VI2RTC
180700960201     C* SE IL FOGLIO VIAGGIO E' ILLUMINATO NON LO SFLAGGO PERCHE'
180800960201     C*   NON E' DA TRASMETTERE
180900960201     C     FVAATR        IFEQ      ' '
181000951228     C                   CLEAR                   FVAFTR
181100951228     C                   CLEAR                   FVADTR
181200960201     C                   ENDIF
181300951228     C                   ENDIF
181400951228     C*
181500951227     C                   Z-ADD     WAED8         FVAAED
181600930127     C                   Z-ADD     VI2AEH        FVAAEH
181700951128     C                   MOVEL     VI2RTC        FVARTA
181800921120     C                   MOVEL     VIDNFV        FVANFA
181900930519     C                   MOVEL     VIDNPG        FVANPG
182000921120     C                   MOVEL     FVVDFV        FVADFA
182100951228     C*
182200941221     C                   UPDATE    FNFVA001
182300951115     C*
182400951228     C* I FOGLI VIAGGIO GIA' ABBINATI NON LI RIELABORO A MENO CHE NON
182500951228     C*   SIANO CAMBIATE LA DATA O L'ORA DI ARRIVO REALE
182600951228    3C     *IN16         IFEQ      *ON
182700951115     C*
182800950621     C** LEGGO TUTTE LE BOLLE DEL FOGLIO E CI METTO IL TOCCO BOLLA
182900970430     C                   CLEAR                   LNASAV
183000950621     C     KARB          SETLL     FNARB06L
183100960108     C     KARB          READE(N)  FNARB06L                               30
183200950621     C*
183300960108    4C     *IN30         DOWEQ     *OFF
183400970430     C     ARBLNA        IFNE      LNASAV
183500970430     C                   MOVE      ARBLNA        LNASAV
183600970430     C                   CLEAR                   DSLV53
183700970430     C                   MOVEL     'A'           D53TFO
183800970430     C                   MOVE      FVANFV        D53NFV
183900970502     C                   MOVE      FVALNP        D53FGS
184000970430     C                   MOVE      FVALAI        D53LAI
184100031215     C                   MOVEL     'B'           D53ABB
184200970430     C                   MOVE      ARBLNA        D53LNA
184300970430     C                   MOVE      ARBTFA        D53TFA
184400970502     C                   MOVE      SIMFEL        D53FEL
184500970430     C                   CALL      'FNLV53R'
184600970430     C                   PARM                    DSLV53
184700970430     C                   MOVE      '1'           WLV53             1
184800970430     C                   END
184900970825     C** SI TRTTA DI BOLLA RELATIVA AL FOGLIO ABBINATO--> ELAVORO
185000970430    5C     D53ERR        IFEQ      *BLANKS
185100070921     c     karb01        chain(E)  fnarb01l
185200070921     c                   if        not %error and %found(fnarb01l)
185300070921     c                   clear                   arbdti
185400070921     c                   clear                   arbhti
185500111118     c                   select
185600111118     c                   when      arbfbs=' '
185700070921     c                   eval      arbfbs='S'
185800111118
185900111118     c                   when      arbfbs='V' or arbfbs='P'
186000111118     c                   eval      arbfbs='E'
186100111118     c                   endsl
186200111118     c
186300070921     c                   update    fnarb000
186400070921     c                   else
186500960108     C                   MOVEL     ARBAAS        AGBAAS
186600960108     C                   MOVEL     ARBLNP        AGBLNP
186700960108     C                   MOVEL     ARBNRS        AGBNRS
186800960108     C                   MOVEL     ARBNSP        AGBNSP
186900960108     C                   WRITE     FNAGB000                             32
187000070921     c                   endif
187100070921     c
187200040315     c                   else
187300040315     c* se errore fnlv53 mi posiziono su nuova lna
187400040315     c     karb6         setgt     fnarb06l
187500951228    5C                   ENDIF
187600950621     C*
187700960108     C     KARB          READE(N)  FNARB06L                               30
187800951228    4C                   ENDDO
187900040120     c*
188000040120      * Carico tutti i secondi livello in arrivo del terminal di arrivo dell'utente
188100040120      * su stesso AS
188200040120     c                   Clear                   Trul09ds
188300040120     c                   Eval      I09Mod = 'TFU'
188400040120     c                   Eval      I09Tfa = fvalai
188500040120     c                   Eval      I09dta = fvadfv
188600040120     c                   Call      'TRUL09R'
188700040120     c                   Parm                    Trul09ds
188800040123     c* se lai non è terminal di nessuno metto comunque lai in mec
188900040123     c                   if        mec(1) = *zeros
189000040123     c                   eval      mec(1) = fvalai
189100040123     c                   endif
189200951227     C*
189300950621     C** LEGGO LE SPUNTE
189400040120     C                   Z-ADD     1             C                 3 0
189500040120     C     MEC(C)        dowNE     *ZEROS
189600960227     C*
189700970515     C                   CLEAR                   DSLV53
189800970515     C                   MOVEL     'A'           D53TFO
189900970515     C                   MOVE      FVANFV        D53NFV
190000970515     C                   MOVE      FVALNP        D53FGS
190100970515     C                   MOVE      FVALAI        D53LAI
190200070917     C                   MOVEL     'B'           D53ABB
190300970515     C                   MOVE      MEC(C)        D53LNA
190400970515     C                   MOVE      SIMFEL        D53FEL
190500970515     C                   CALL      'FNLV53R'
190600970515     C                   PARM                    DSLV53
190700970515     C                   MOVE      '1'           WLV53             1
190800970825     C**
190900970825     C* SI TRATTA DI LINEA RELATIVA AL FOGLIO ABBINATO--> ELABORO
191000970515    5C     D53ERR        IFEQ      *BLANKS
191100921120     C                   MOVEL     MEC(C)        COMLAI
191200070921     c                   clear                   savspe
191300070921     c                   clear                   wtragb
191400911010     C*
191500941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
191600000118     C                   Z-ADD     VI2FVA        WI2FVA
191700070123     C     KBRV1         SETLL     FNBRV02L
191800070123     C     KBRV1         READE     FNBRV02L                               30
191900911010     C*
192000951228    6C     *IN30         DOWEQ     '0'
192100921120     C*
192200941221     C     KBRV2         CHAIN     FNART27L                           31
192300951016     C*
192400951228    7C     *IN31         IFEQ      *OFF
192500070921     c* La prima volta salvo la chiave spedizione  per aggiornare quando
192600070921     c*  ha letto l'ultimo collo e non il primo
192700070921     c                   if        savspe=0
192800070921     c                   movel     artspe        savspe
192900070921     c                   endif
193000951016     C*
193100971002     C* IMPOSTO IL FALG DI ABBINATO SOLTANTO SE LA DATA SUL COLLO
193200971002     C* CORRISPONDE ALLA DATA FOGLIO VIAGGIO
193300971002     C     BRVATR        IFEQ      ' '
193400971002     C     ARTDFV        ANDEQ     FVADFV
193500971002     C     BRVATR        ORNE      ' '
193600971002     C     ARTDUT        ANDEQ     FVADFV
193700070921     c                   if        savspe=artspe
193800951016     C                   MOVEL     'S'           WTRAGB            1
193900070921     c                   endif
194000950328     C                   MOVEL     'S'           ARTABN                         'S'=SPUNTA ABBINATA
194100971119     C                   ENDIF
194200950111     C*
194300950111     C                   UPDATE    FNART000
194400921202     C*
194500070921    8C     ARTSPE        ifne      SAVSPE
194600070921     c                   EXSR      Aggspe
194700960110    8C                   ENDIF
194800951228    7C                   ENDIF
194900911010     C*
195000070123     C     KBRV1         READE     FNBRV02L                               30
195100951228    6C                   ENDDO
195200070921     c
195300070921     c* Elaboro l'ultima bolla
195400070921     c                   if        savspe>0
195500070921     c                   exsr      AggSpe
195600070921     c                   endif
195700070921     c
195800070917     c* Cerco anche altre bolla a cui ho impostato la data e ora modificata
195900070921     c*  ma che non sono partite come spunta su questo foglio
196000070917     c*  (ad esempio disguidi o colli part-non arrivati o colli arr-non part
196100070921    6c                   if        *in15
196200070918     c                   exSR      AGGIOBOL
196300070921    6C                   endif
196400070921    5C                   ENDIF
196500921120     C*
196600921120     C                   ADD       1             C
196700951228    4C                   ENDDO
196800951228     C*
196900951228    3C                   ENDIF
197000960304   2BC                   ENDIF
197100960304     C*
197200960304     C                   ADD       1             Y
197300960304   2AC                   ENDDO
197400960304    2C                   ENDIF
197500911010     C*
197600930129     C                   ADD       1             NRR
197700951228    1C                   ENDDO
197800911010     C**
197900911010     C** SE HO TOLTO DEI FOGLI VIAGGIO DEVO DISABILITARE
198000911010     C**
198100911010     C                   Z-ADD     1             I
198200920428     C                   Z-ADD     0             SAVSPE
198300950328     C                   MOVEL     'DT'          AGBAGB
198400960205     C                   CLEAR                   WALL
198500921120     C*
198600940620    1C     FV(I)         DOWNE     0
198700911010     C     FV(I)         LOOKUP    FVS                                    30
198800911010     C*
198900940620    2C  N30              DO
199000921119     C                   MOVEL     FV(I)         VI2KEY
199100960304     C                   Z-ADD     1             Y
199200960304     C* LEGGO TUTTA LA L6
199300960304   2AC     L6(Y)         DOWGT     0
199400960304     C                   MOVEL     L6(Y)         COMLAI
199500960304     C*
199600960304     C* CHAIN FOGLIO PARTENZE PER AGGIORNARE CAMPI
199700960304     C* E IMPOSTARE LINEE ARRIVO E SCARICO DEL FOGLIO
199800960304     C     KFVA1         CHAIN     FNFVA01L                           33
199900960304   2BC     *IN33         IFEQ      *OFF
200000921120     C*
200100960110     C** LEGGO TUTTE LE BOLLE DEL FOGLIO E TOLGO DATA E ORA ARRIVO
200200960110     C**   EFFETTIVA ULTIMO COLLO
200300970430     C                   CLEAR                   LNASAV
200400960110     C     KARB          SETLL     FNARB06L
200500960110     C     KARB          READE     FNARB06L                             3830
200600960110     C*
200700960110    3C     *IN30         DOWEQ     *OFF
200800960110     C     *IN38         ANDEQ     *OFF
200900970516     C*
201000970430     C     ARBLNA        IFNE      LNASAV
201100970430     C                   MOVE      ARBLNA        LNASAV
201200970430     C                   CLEAR                   DSLV53
201300970430     C                   MOVEL     'A'           D53TFO
201400970430     C                   MOVE      FVANFV        D53NFV
201500970502     C                   MOVE      FVALNP        D53FGS
201600970430     C                   MOVE      FVALAI        D53LAI
201700031215     C                   MOVEL     'B'           D53ABB
201800970430     C                   MOVE      ARBLNA        D53LNA
201900970430     C                   MOVE      ARBTFA        D53TFA
202000970502     C                   MOVE      SIMFEL        D53FEL
202100970430     C                   CALL      'FNLV53R'
202200970430     C                   PARM                    DSLV53
202300970430     C                   MOVE      '1'           WLV53             1
202400970430     C                   END
202500970825     C* SE E' BOLLA RELATIVA AL FOGLIO ABBINATO--> SBINO
202600970430    4C     D53ERR        IFEQ      *BLANKS
202700960110     C                   CLEAR                   ARBDTI
202800960110     C                   CLEAR                   ARBHTI
202900960111     C                   UPDATE    FNARB006
203000040315     c                   else
203100040315     c* se errore fnlv53 mi posiziono su nuova lna
203200040315     c     karb6         setgt     fnarb06l
203300960110    4C                   ENDIF
203400960110     C*
203500960110     C     KARB          READE     FNARB06L                             3830
203600960110    3C                   ENDDO
203700960110     C*
203800960110     C* 38 ON  - RECORD ALLOCATO
203900960110   3AC     *IN38         IFEQ      *ON
204000960110     C                   MOVEL     '1'           WALL              1
204100960110  X3AC                   ELSE
204200960110     C*
204300950328     C* SALVO LA DATA DI ARRIVO EFFETTIVO CHE C'ERA
204400950328     C                   MOVEL     FVAAED        SAVAED
204500950328     C*
204600921120     C                   MOVEL     *ZEROS        FVAAEH
204700921120     C                   MOVEL     *ZEROS        FVAAED
204800960304     C                   MOVEL     ' '           FVARTA
204900921120     C                   MOVEL     *ZEROS        FVANFA
205000921120     C                   MOVEL     *ZEROS        FVADFA
205100960304     C                   MOVEL     0             FVANPG
205200960202     C                   CLEAR                   FVARTA
205300960202     C*
205400941221     C  N33              UPDATE    FNFVA001
205500040120      * Carico tutti i secondi livello in arrivo del terminal di arrivo dell'utente
205600040120      * su stesso AS
205700040120     c                   Clear                   Trul09ds
205800040120     c                   Eval      I09Mod = 'TFU'
205900040120     c                   Eval      I09Tfa = fvalai
206000040120     c                   Eval      I09dta = fvadfv
206100040120     c                   Call      'TRUL09R'
206200040120     c                   Parm                    Trul09ds
206300040123     c* se lai non è terminal di nessuno metto comunque lai in mec
206400040123     c                   if        mec(1) = *zeros
206500040123     c                   eval      mec(1) = fvalai
206600040123     c                   endif
206700960202     C*
206800921120     C                   Z-ADD     1             C
206900040120     C     MEC(C)        downe     *ZEROS
207000970515     C*
207100970515     C                   CLEAR                   DSLV53
207200970515     C                   MOVEL     'A'           D53TFO
207300970515     C                   MOVE      FVANFV        D53NFV
207400970515     C                   MOVE      FVALNP        D53FGS
207500970515     C                   MOVE      FVALAI        D53LAI
207600070917     C                   MOVEL     'B'           D53ABB
207700970515     C                   MOVE      MEC(C)        D53LNA
207800970515     C                   MOVE      SIMFEL        D53FEL
207900970515     C                   CALL      'FNLV53R'
208000970515     C                   PARM                    DSLV53
208100970515     C                   MOVE      '1'           WLV53             1
208200960227     C*
208300970515    4C     D53ERR        IFEQ      *BLANKS
208400921120     C                   MOVEL     MEC(C)        COMLAI
208500921120     C*
208600941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
208700000118     C                   Z-ADD     VI2FVA        WI2FVA
208800070123     C     KBRV1         SETLL     FNBRV02L
208900070123     C     KBRV1         READE     FNBRV02L                               30
209000911010     C*
209100940620    5C     *IN30         DOWEQ     '0'
209200921120     C*
209300941221     C     KBRV2         CHAIN     FNART27L                           31
209400950328     C*
209500951228    6C     *IN31         IFEQ      *OFF
209600971002     C* AGGIORNO ARTABN E LA BOLLO SOLO SE LA DATA CORRISPONDE
209700971002   6AC     BRVATR        IFEQ      ' '
209800971002     C     ARTDFV        ANDEQ     FVADFV
209900971002     C     BRVATR        ORNE      ' '
210000971002     C     ARTDUT        ANDEQ     FVADFV
210100941221     C                   MOVEL     *BLANKS       ARTABN                         SPUNTA NON ABBINATA
210200950328     C*
210300950328     C                   UPDATE    FNART000
210400911011     C*
210500911014     C* C'E' DETTAGLIO COLLI
210600950328    7C     ARTSPE        IFNE      SAVSPE
210700941220     C     KART          CHAIN     FNARB01L                           32
210800911011     C*
210900911011     C* DISABILITO ALLA CONSEGNA
211000940620    8C     ARBAMA        IFNE      2
211100921126     C                   MOVEL     0             ARBAMP
211200940620    8C                   END
211300940620    8C     ARBAMA        IFEQ      1
211400921126     C                   MOVEL     0             ARBAMA
211500940620    8C                   END
211600940613     C* TOLGO DATA E ORA ARRIVO EFFETTIVA ULTIMO COLLO
211700940613     C                   CLEAR                   ARBDTI
211800940613     C                   CLEAR                   ARBHTI
211900941220     C                   UPDATE    FNARB000
212000920428     C                   MOVEL     ARTSPE        SAVSPE
212100940620    7C                   END
212200950407    6C                   ENDIF
212300971002     C                   ENDIF
212400911011     C*
212500070123     C     KBRV1         READE     FNBRV02L                               30
212600940620    5C                   END
212700940620    4C                   END
212800911010     C**
212900921120     C                   ADD       1             C
213000940620    3C                   END
213100960110     C*
213200960110     C* SE TUTTO OK TOLGO IL FOGLIO VIAGGIO DALLA SCHIERA IN MODO CHE
213300960110     C*   VI RIMANGANO SOLO QUELLI CHE NON SONO RIUSCITA AD ELABORARE
213400960110     C*   PERCHE' HO TROVATO I RECORD DI ALCUNE BOLLE ALLOCATI
213500960110     C                   CLEAR                   FV(I)
213600960110   3AC                   ENDIF
213700960304   2BC                   END
213800960304     C**
213900960304     C                   ADD       1             Y
214000960304   2AC                   ENDDO
214100960304    2C                   ENDDO
214200911010     C*
214300911010     C                   ADD       1             I
214400960304    1C                   ENDDO
214500950328     C**
214600950328     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
214700960205    0C     WALL          IFEQ      ' '
214800151022     C**                 MOVEL     'A'           P05TBO
214900151022     C**                 CLEAR                   P05AGB
215000151022     C**                 MOVEL     PARA05        KPJBU
215100950328     C*
215200971218     C* FORZATURA DELLA CODA DIFFE PERCHE' QEUSTI LAVORI DEVONO
215300971218     C*  ANDARE IN SUCCESSIONE E LR27 DEVE ESSERE ELABORATO IN UNA
215400971218     C*  UNICA CODA
215500151022     C**                 MOVEL(P)  'KSDIFFE'     KQIEB
215600971218     C*
215700151022     C**                 MOVEL     'LV52'        KCOAZ
215800151022     C**                 CALL      'BCH10'
215900151022     C*+                 PARM                    KPJBA
216000971218     C* VIENE SOTTOMESSA L'ABILITAZIONE BOLLE PERCHE' DEVE GIRARE PRIMA
216100971218     C*  DELLE STAMPE: ELENCO SPEDIZIONI NON PARTITE,PERCHE' SERVONO
216200971218     C*  LE BOLLE ABILITATE
216300971218     C*  TUTTO QUESTO ANCHE SE L'ABILITAZIONE BOLLE VIENE ANCHE SOTTOM.
216400971218     C*  DALL'LV52
216500151022     C**                 MOVEL     'LR27'        KCOAZ
216600151022     C**                 CALL      'BCH10'
216700151022     C**                 PARM                    KPJBA
216800950926     C**
216900921119     C**
217000921119     C* LANCIO SE RICHIESTA ELENCO SPEDIZ NO ABILITATE
217100921119     C* PRIMA PER I FOGLI DIRETTI
217200921119     C* POI PER I FOGLI CON SCALO AL TRANSITO E AL TERMINAL
217300960205     C                   MOVEL     ' '           COMATS
217400940620    1C                   DO        2
217500920330     C                   Z-ADD     1             I
217600920330     C*
217700940620    2C     FVS(I)        DOWNE     0
217800940620     C*
217900940620    3C     STS(I)        IFEQ      'S'
218000921119     C     ATS(I)        ANDEQ     COMATS
218100921109     C*
218200921109     C                   Z-ADD     1             C
218300921119     C                   MOVEL     FVS(I)        VI2KEY
218400980303     C                   MOVEL     VI2LAI        COMLAI
218500941221     C                   MOVEL     COMLNP        P52LNP
218600941221     C                   MOVE      VI2FVA        P52FVA
218700040122      * Carico tutti i secondi livello in arrivo del terminal di arrivo
218800040122      * su stesso AS
218900040122     c                   Clear                   Trul09ds
219000150116     c****               Clear                   mecn
219100040122     c                   Eval      I09Mod = 'TFU'
219200040122     c                   Eval      I09dta = dfv(I)
219300040122     c                   move      vi2lai        i09tfa
219400040122     c                   Call      'TRUL09R'
219500040122     c                   Parm                    Trul09ds
219600040123     c* se lai non è terminal di nessuno metto in mecn metto lai
219700040123     c                   if        mec(1) = *zeros
219800150116     c                   eval      mec(1) = i09tfa
219900150116     c**                 else
220000040123     c* ora carico schiera dei p.o. di cui ter-arr non gestiti da altri p.o.
220100150116     c*****              exsr      CarMecn
220200150116     c***                if        mecn(1) = *zeros
220300150116     c***                eval      mecn(1) = i09tfa
220400150116     c***                endif
220500040123     c                   endif
220600921119     C*
220700150116     C     MEC(C)        downe     *ZEROS
220800970515     C*
220900970515     C* PER SAPERE SE ELABORARE RICHIAMO PGM FNLV53R
221000970515     C                   CLEAR                   DSLV53
221100031215     C                   MOVEL     'A'           D53TFO
221200970515     C                   MOVE      VI2FVA        D53NFV
221300970515     C                   MOVE      COMLNP        D53FGS
221400970515     C                   MOVE      COMLAI        D53LAI
221500031215     C                   MOVEL     'B'           D53ABB
221600150116     C                   MOVE      MEC(C)        D53LNA
221700970515     C                   MOVE      SIMFEL        D53FEL
221800970515     C                   CALL      'FNLV53R'
221900970515     C                   PARM                    DSLV53
222000970515     C                   MOVE      '1'           WLV53             1
222100970515     C*
222200970515    5C     D53ERR        IFEQ      *BLANKS
222300940620     C*
222400950215     C                   MOVEL     '1'           P52OUT                         OUTQ DA AZORG00F
222500150116     c**se mecn(c) è p.o. gestito anzichè passare mecn(c) passo il suo
222600150116     c**gestore
222700150116     c***                z-add     1             ii                3 0
222800150116     c***  mecn(C)       lookup    skpo(II)                               30
222900150116     c** 30              eval      p52fel = skpot(ii)
223000150116     c* N.B.: Viene richiamato lr52 anche per le filiali in £6 (es. 102): Non dovrebbe farlo
223100150116     c*       ma visto ch è sempre stato così e dal momento che adesso la stampa non viene
223200150116     c*       quasi mai lanciata e serve solo per i SI DDT ci disinteressiamo
223300150116     C                   eval      p52fel = mec(c)
223400960518     C                   MOVEL     99991231      P52DAT
223500030826     C                   MOVEL(p)  PARA52        KPJBU
223600941216     C                   MOVEL     'LR52'        KCOAZ
223700920330     C                   CALL      'BCH10'
223800920330     C                   PARM                    KPJBA
223900940620    5C                   END
224000921119     C*
224100921109     C                   ADD       1             C
224200940620    4C                   END
224300921109     C*
224400940620    3C                   END
224500921109     C*
224600920330     C                   ADD       1             I
224700940620    2C                   END
224800921119     C*
224900921119     C                   MOVEL     'T'           COMATS            1
225000940620    1C                   END
225100960205    0C                   END
225200911010     C*
225300971218     C                   CLEAR                   KQIEB
225400911008     C                   ENDSR
225500070921     c* Aggiorno a cambio spedizione FNARB per l'abilit.bolla -------*
225600070921     c     AGGSpe        BEGSR
225700070921     c* Aggiorno direttamente la bolla se riesco, altrimenti
225800070921     c*  scrivo AGB se allocata
225900070921    1c                   if        WTRAGB='S'
226000070921     c     ksav          chain(E)  fnarb01l
226100070921    2c                   if        not %error and %found(fnarb01l)
226200070921     c                   clear                   arbdti
226300070921     c                   clear                   arbhti
226400111118     c                   select
226500111118     c                   when      arbfbs=' '
226600111118     c                   eval      arbfbs='S'
226700111118
226800111118     c                   when      arbfbs='V' or arbfbs='P'
226900111118     c                   eval      arbfbs='E'
227000111118     c                   endsl
227100070921     c                   update    fnarb000
227200070921   x2c                   else
227300070921     C                   MOVEL     WsaAAS        AGBAAS
227400070921     C                   MOVEL     WsaLNP        AGBLNP
227500070921     C                   MOVEL     WsaNRS        AGBNRS
227600070921     C                   MOVEL     WsaNSP        AGBNSP
227700070921     C                   WRITE     FNAGB000                             32
227800070921    2c                   endif
227900070921    1c                   endif
228000110103     c*
228100070921     C                   CLEAR                   WTRAGB
228200070921     C                   MOVEL     ARTSPE        SAVSPE
228300110103     c*
228400070921     c* Se per l'ultima spunta di nuova spedizione ho aggiornato
228500070921     c*  fnart, lo memorizzo
228600070921    1c                   if        artabn='S'
228700070921     C                   MOVEL     'S'           WTRAGB            1
228800070921    1c                   endif
228900070921     c                   ENDSR
229000070918     C*
229100070918     C*--- Cerco le bolle da aggiornare con la stessa data ora arrivo *
229200070918     C     Aggiobol      BEGSR
229300110103      *
229400070918      *?Imposto la parte fissa dell'istruzione sql
229500070918     c                   eval      wrkgetlista =
229600070918     c                             'select arbaas, arblnp, arbnrs, arbnsp +
229700070919     c                             from fnarb00f where arbdti ='+
229800070918     c                             %editc(woldaed:'X') + ' and +
229900070918     c                             arbhti ='+ %editc(woldaeh:'X') +
230000070919     c                             ' and arblna=' + %editc(comlai:'X')
230100070918      *?Solo lettura
230200070918     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
230300070918
230400070918     C/EXEC SQL
230500070918     C+ PREPARE s1 FROM :wrkgetlista
230600070918     C/END-EXEC
230700070918     C
230800070918     C/EXEC SQL
230900070918     C+ DECLARE A1 CURSOR FOR S1
231000070918     C/END-EXEC
231100070918     C
231200070918     C/EXEC SQL
231300070918     C+ OPEN a1
231400070918     C/END-EXEC
231500070918
231600070918      *?Leggo il file
231700070918     c                   do        *hival
231800070918     C/EXEC SQL
231900070918     C+ FETCH NEXT FROM a1 INTO : arbaas, :arblnp, :arbnrs, :arbnsp
232000070918     C/END-EXEC
232100070918
232200070919     c                   if        sqlcod = 100 or sqlcod<0
232300070918     c                   leave
232400070918     c                   else
232500070921     c     karb01        chain(E)  fnarb01l
232600070918     c                   if        not %error and  %found(fnarb01l)
232700070918     c                   clear                   arbdti
232800070918     c                   clear                   arbhti
232900111118     c*****              movel     'S'           arbfbs
233000111118     c                   select
233100111118     c                   when      arbfbs=' '
233200111118     c                   eval      arbfbs='S'
233300111118
233400111118     c                   when      arbfbs='V' or arbfbs='P'
233500111118     c                   eval      arbfbs='E'
233600111118     c                   endsl
233700070918     c                   update    fnarb000
233800070918     c                   endif
233900070918     c
234000070918     c                   ENDif
234100070918     c                   enddo
234200070918     C/EXEC SQL
234300070918     C+ CLOSE a1
234400070918     C/END-EXEC
234500070918     c                   ENDSR
234600960202     C*
234700951115     C*--- SR INIZIALE -----------------------------------------------*
234800951115     C     *INZSR        BEGSR
234900951115     C*
235000951115     C****  KLIST  ****
235100960304     C     KCAP          KLIST
235200970721     C                   KFLD                    KVER
235300970721     C                   KFLD                    KNAR
235400960304     C                   KFLD                    KLNA
235500951115     C     KARB          KLIST
235600951115     C                   KFLD                    COMLNP
235700951115     C                   KFLD                    VI2FVA
235800040315     C     KARB6         KLIST
235900040315     C                   KFLD                    COMLNP
236000040315     C                   KFLD                    VI2FVA
236100040315     C                   KFLD                    arblna
236200951115     C     KFVV1         KLIST
236300951115     C                   KFLD                    PARNPG
236400951115     C                   KFLD                    COMNFV            5 0
236500951115     C                   KFLD                    VIDFGS
236600951115     C     KFVA1         KLIST
236700951115     C                   KFLD                    COMLNP
236800951115     C                   KFLD                    VI2FVA
236900951115     C                   KFLD                    COMLAI            3 0
237000951115     C     KFVA2         KLIST                                                  FNFVA02L
237100951115     C                   KFLD                    PARNPG                         ..CATEGORIA
237200951115     C                   KFLD                    COMNFV                         ..FOGLIO
237300980421     C                   KFLD                    L6(XX)
237400951128     C     KFVA4         KLIST                                                  FNFVA02L
237500951128     C                   KFLD                    FVATRN
237600951128     C                   KFLD                    FVADTN
237700951115     C     KBRV1         KLIST                                                  FNBRV02L
237800951115     C                   KFLD                    KNPG                           ..CATEGORIA
237900000118     C                   KFLD                    WI2FVA                         ..NR.FOGLIO
238000951115     C                   KFLD                    COMLNP                         ..FIL.GESTIONE
238100951115     C                   KFLD                    COMLAI                         ..LNA
238200951115     C     KBRV2         KLIST                                                  FNART27L
238300951115     C                   KFLD                    BRVLNP                         ..LNP
238400951115     C                   KFLD                    BRVLNA                         ..LNA
238500951115     C                   KFLD                    BRVNRS                         ..SERIE
238600040120     C                   KFLD                    BRVNSC                         ..SEGNACOLLO
238700951115     C     KART          KLIST
238800951115     C                   KFLD                    ARTAAS
238900951115     C                   KFLD                    ARTLNP
239000951115     C                   KFLD                    ARTNRS
239100951115     C                   KFLD                    ARTNSP
239200070921     C     KSAV          KLIST
239300070921     C                   KFLD                    WsaAAS
239400070921     C                   KFLD                    WsaLNP
239500070921     C                   KFLD                    WsaNRS
239600070921     C                   KFLD                    WsaNSP
239700070919     C     KARB01        KLIST
239800070918     C                   KFLD                    ARbAAS
239900070918     C                   KFLD                    ARbLNP
240000070918     C                   KFLD                    ARbNRS
240100070918     C                   KFLD                    ARbNSP
240200951115     C     KTAB          KLIST
240300951115     C                   KFLD                    CODUT
240400951115     C                   KFLD                    COD
240500951115     C                   KFLD                    KEY
240600951115     C     KFV4          KLIST
240700951228     C                   KFLD                    FVALNP
240800951115     C                   KFLD                    FVANFV
240900951115     C                   KFLD                    WEPA
241000951228     C                   KFLD                    FVALAI
241100951115     C                   KFLD                    WTRC
241200000628      * ESTENSIONE FV
241300000628     C     KFWA1         KLIST
241400000628     C                   KFLD                    FVALNP
241500000628     C                   KFLD                    FVANFV
241600000628     C                   KFLD                    FVALAI
241700951115     C*
241800951115     C****  DEFINIZIONE CAMPI  ****
241900970721     C     *LIKE         DEFINE    CPCVER        O96VER
242000970721     C     *LIKE         DEFINE    CPCVER        KVER
242100970721     C     *LIKE         DEFINE    FVVDFV        I96DRI
242200970721     C     *LIKE         DEFINE    §1XHHO        HHOUNO
242300960304     C     *LIKE         DEFINE    §1XHHO        HHODUE
242400960304     C     *LIKE         DEFINE    D98DTI        DATSOG
242500960304     C     *LIKE         DEFINE    D98HTI        ORASOG
242600970721     C     *LIKE         DEFINE    CPCNAR        KNAR
242700970721     C     *LIKE         DEFINE    CPCLNA        KLNA
242800960304     C     *LIKE         DEFINE    BRVNPG        KNPG
242900951115     C     *LIKE         DEFINE    FVAAED        SAVAED
243000951228     C     *LIKE         DEFINE    FVAAED        WAED8
243100951116     C     *LIKE         DEFINE    FV4TRC        WTRC
243200951115     C     *LIKE         DEFINE    TBLCOD        COD
243300951115     C     *LIKE         DEFINE    TBLKEY        KEY
243400970430     C     *LIKE         DEFINE    ARBLNA        LNASAV
243500970825     C     *LIKE         DEFINE    FV4EPA        WEPA
243600000118     C     *LIKE         DEFINE    BRVNFV        WI2FVA
243700951115     C*
243800970825     C                   MOVEL     'A'           WEPA                           *ARRIVO
243900960202     C                   MOVEL     'R'           WTRC
244000951115     C                   ENDSR
244100040122     C*--- CARICAMENTO SCHIERA MECN ----------------------------------*
244200150116     C**   CarMecn       BEGSR
244300150116     c**                 z-add     0             k                 2 0
244400150116     c**                 Do        30            x
244500150116     c**                 If        Mec(x) = *Zeros
244600150116     c**                 Leave
244700150116     c**                 EndIf
244800150116     c**   Mec(x)        Lookup    Skpot                                  30
244900150116     c**                 If        *In30
245000150116     c**                 Add       1             k
245100150116     c**                 Eval      Mecn(k) = Mec(x)
245200150116     c**                 Iter
245300150116     c**                 EndIf
245400150116     c**   Mec(x)        Lookup    Skpo                                   30
245500150116     c**                 If        Not *In30
245600150116     c**                 Add       1             k
245700150116     c***                Eval      Mecn(k) = Mec(x)
245800150116     c***                Iter
245900150116     c***                EndIf
246000150116     c***                EndDo
246100150116     c***                endsr
246200110103
246300110103      *---------------------------------------------------------------*
246400110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
246500110103      *---------------------------------------------------------------*
246600110103     c     sr_DatiJob    BEGSR
246700110103      *
246800110103     c     *dtaara       define    §azute        AZUTEds
246900110103     c     *dtaara       define    §datiute      dDatiUte
247000110103      *
247100110103     c                   in(E)     *dtaara
247200110103     c                   IF        %Error or RSUT = *blanks
247300110103     c                   clear                   TIBS34ds
247400110103     c                   call      'TIBS34R'
247500110103     c                   parm                    TIBS34ds
247600110103     c                   in        *dtaara
247700110103     c                   ENDIF
247800110103      *
247900110103     c                   ENDSR
248000951114     C*---------------------------------------------------------------*
248100000606**  MSG
248200951115Immettere causale valida                                                      1
248300951115Presente ritardo/anticipo nell'arrivo del foglio: immettere la causale        2
248400951121Non c'e' ritardo/anticipo: se si vuole si può eliminare la causale            3
248500951121FOGLIO VIAGGIO GIA' ABBINATO CON FOGLIO ARRIVI N.RO  X - XXXXX                4
248600951115Se immessa causale "G" occorre inserire descrizione ritardo/anticipo          5
248700960201Presente descrizione ritardo: immettere la causale o eliminare la descrizione 6
248800960227Fogli xxx-xxxxx e xxx-xxxxx del traino xxxxxxx hanno data/ora/causale diverse 7
248900960201Presente descrizione ritardo: premere F6 per conferma o eliminare descrizione 8
249000960201Eliminata descrizione ritardo: confermare con F6 o reinserire la descrizione  9
249100960227xxx-xxxxx deve avere stesso arrivo(xx/xx xx:xx x)del F.xxx-xxxxx: ENTER x forz10
249200960109Non tutte le bolle sono state aggiornate: premere nuovamente F6 !!            11
249300960202Impossibile uscire perchè non tutte le bolle sono state aggiornate: premere F612
249400960227Data/ora arrivo non puo' essere superiore alla data/ora corrente: ENTER x forz13
249500960626Impossibile modificare o eliminare la causale. Unica causale ammessa: X       14
249600960626Impossibile immettere la causale                                              15
249700960626Impossibile modificare la descrizione                                         16
249800960626Impossibile modificare la causale: eliminarla oppure lasciarla = X            17
249900971002Impossibile visualizzare il foglio: provieni dalla visualizzazione!!          18
250000030924Impossibile posticipare l'ora di arrivo del foglio illuminato oltre le xx:xx  19
250100000606Data arrivo non puo' essere superiore alla data foglio arrivi     ENTER x forz20
