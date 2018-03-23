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
067200961010     C                   END
067300921120     C*
067400960626     C* IMPOSTO INDICATORE PER PERMETTERE SOLO LA VARIAZ. DI DATA/ORA
067500960626     C* ARRIVO REALE DI FV GIA' ABBINATI SE DATA FOGLIO ARRIVI <= DATA
067600960626     C* ULTIMA ELABORAZIONE IDD
067700990512     C     FVVDFV        IFLE      §7ADID
067800970721     C* ANCHE SE FOGLIO CHIUSO POSSO SOLO MODIFICARE LA DATA E ORA DI
067900961010     C*  ARRIVO
068000961010     C     FVVFCF        OREQ      'S'
068100960626     C                   SETON                                        17
068200960626     C                   ELSE
068300960626     C                   SETOFF                                       17
068400960626     C                   END
068500970721     C**
068600970721     C* CALL TISI96R PER AVERE LA VERSIONE
068700970721     C**
068800970721     C                   MOVEL     FVVDFV        I96DRI
068900970721     C                   CLEAR                   O96VER
069000970721     C                   CALL      'TISI96R'
069100970721     C                   PARM                    I96DRI
069200970721     C                   PARM                    O96VER
069300970721     C**
069400970721     C                   MOVEL     O96VER        KVER
069500930127     C*
069600930127     C* CARICAMENTO SFILE
069700930127     C                   EXSR      CARSFL
069800930127     C*
069900930127     C     ENDCT1        ENDSR
070000941216     C**************************************************************************
070100941216     C*    CARICAMENTO SFILE
070200941216     C**************************************************************************
070300930127     C     CARSFL        BEGSR
070400941216     C*
070500930519     C* DATA FOGLIO ARRIVI
070600920709     C                   MOVE      FVVDFV        G02INV
070700920709     C                   MOVE      *ZEROS        G02DAT
070800920709     C                   MOVEL     3             G02ERR
070900941221     C                   CALL      'XSRDA8'
071000920709     C                   PARM                    WLBDAT
071100920709     C                   Z-ADD     G02DAT        VIDDFA
071200911008     C*
071300960205     C* CAMPO USATO PER IMMETTERE LA DESCRIZIONE RITARDO/ANTICIPO
071400960205     C                   CLEAR                   VI2GES
071500960205     C* CAMPO USATO PER VISUALIZZARE IL FV
071600960205     C                   CLEAR                   VI2VFV                          VISUAL FV
071700960205     C* STAMPA ELENCO SPEDIZIONI NON PARTITE
071800960205     C                   MOVEL     'N'           VI2STA                          STAMPA ANOM
071900141110     c* aggiungo eventuali filiali estere secondi livello della fgs
072000141110     c* per permettere al terminal di abbinare eventuali diretti a filiale estera
072100141110     c                   Clear                   Trul09ds
072200141110     c                   Eval      I09Mod = 'TFU'
072300141110     c                   Eval      I09Tfa = vidfgs
072400141110     c                   Eval      I09dta = fvvdfv
072500141110     c                   Call      'TRUL09R'
072600141110     c                   Parm                    Trul09ds
072700141110     c                   if        mec(1)>*zeros
072800141110     c                   eval      in=%lookup(000:l6)
072900141110     C                   Z-ADD     1             XX                3 0
073000141110     c                   dow       xx<=%elem(mec) and mec(xx)>*zeros and
073100141110     c                             in<=%elem(l6)
073200141110     c* verifico se filiale estera e in questo caso la carico in £6
073300141110     C                   clear                   OG143
073400141110     c     mec(xx)       chain     azorg01l
073500141110     c                   if        %found(azorg01l)
073600141110     c                   movel     orgde3        og143
073700141110     c                   endif
073800141110     c                   if        §ogntw='EEX' or §ogntw='DPD' or §ogntw='FED'
073900141110     c                   eval      l6(in)=mec(xx)
074000141110     c                   add       1             in
074100141110     c                   endif
074200141110     c                   add       1             xx
074300141110     c                   enddo
074400141110     c                   endif
074500960205     C*
074600920428     C* CARICO FOGLI VIAGGIO GIA' ABBINATI SE ESISTONO
074700960318     C* CARICO SOLO QUELLI CON FVALAI = ALLA FIL. GESTIONE FOGLIO
074800911008     C                   Z-ADD     0             NRR
074900911008     C                   MOVEA     *ZEROS        FV
075000980109     C                   MOVEA     *ZEROS        F2
075100980109     C                   Z-ADD     1             XX                3 0
075200980109    1C     L6(XX)        DOWGT     0
075300941221     C     KFVA2         SETLL     FNFVA02L
075400941221     C     KFVA2         READE     FNFVA02L                               30
075500911008     C*
075600980109    2C     *IN30         DOWEQ     '0'
075700960318     C*
075800980109     C                   MOVEL     FVAKEY        W0080             8 0
075900980109     C     W0080         LOOKUP    F2                                     39
076000980109    3C     *IN39         IFEQ      *OFF
076100921119     C                   MOVEL     FVANFV        VI2FVA                          NUM FOGLIO
076200921119     C                   MOVEL     FVALNP        VI2LNP                          LINEA PART
076300000628     C                   MOVEL     FVALAI        VI2LAI                          ARRIVO INV
076400951120     C*
076500960205     C                   CLEAR                   VI2AEH
076600960402     C                   CLEAR                   VI2AST
076700000628     C                   CLEAR                   VI2PTV
076800951120     C                   CLEAR                   VI2RTC
076900951121     C                   CLEAR                   SAVRTC
077000951120     C                   CLEAR                   VI2DTC
077100951120     C                   CLEAR                   VI2DES
077200960222     C                   MOVEL     '1'           WFORZ
077300960222     C                   CLEAR                   WFORZ2
077400960227     C                   CLEAR                   WFORZ3
077500960222     C                   CLEAR                   WPRVOL
077600960205     C*
077700960205     C                   EXSR      DATARR
077800951115     C*
077900911010     C                   ADD       1             NRR
078000921119     C                   Z-ADD     FVAKEY        FV(NRR)
078100980109     C                   MOVEL     FVAKEY        F2(NRR)
078200941216     C                   WRITE     LR25DSF
078300980109    3C                   ENDIF
078400911008     C*
078500941221     C     KFVA2         READE     FNFVA02L                               30
078600980109    2C                   ENDDO
078700980109     C                   ADD       1             XX
078800980109    1C                   ENDDO
078900921118     C*
079000980109    2C     *IN17         IFEQ      *OFF
079100911008     C* FINISCO DI PULIRE IL SUBFILE
079200920316     C                   ADD       1             NRR               5 0
079300930129     C*
079400930129     C     NRR           DIV       13            RIS               4 0
079500930129     C                   MVR                     RESTO             3 0
079600930129     C*
079700930129     C                   Z-ADD     RESTO         B
079800911008     C                   EXSR      PULSFL
079900960626     C                   ELSE
080000960626     C                   Z-ADD     NRR           SAVNRR            5 0
080100961014     C     NRR           COMP      0                                      72
080200960626     C*
080300960626     C                   END
080400930127     C*
080500930127     C                   ENDSR
080600941216     C**************************************************************************
080700941216     C*    CONTROLLO SUB FILE
080800941216     C**************************************************************************
080900911008     C     CTRSFL        BEGSR
081000941216     C*
081100921119     C                   SETOFF                                       904509
081200911009     C                   Z-ADD     1             NRR
081300040120     C                   Z-ADD     0             X                 3 0
081400911011     C                   MOVEA     *ZEROS        FVS
081500920316     C                   MOVEA     *BLANKS       STS
081600921119     C                   MOVEA     *BLANKS       ATS
081700951117     C                   MOVEA     *ZEROS        TRN
081800951117     C                   MOVEA     *ZEROS        DTN
081900040122     C                   MOVEA     *ZEROS        DFV
082000960222     C                   CLEAR                   DHA
082100911009     C*
082200911009     C* CONTROLLO TUTTO IL SUBFILE
082300951109    1C     NRR           DOWLE     SAVNRR
082400941216     C     NRR           CHAIN     LR25DSF                            30
082500911008     C*
082600951109    2C  N30              DO
082700921123     C                   SETOFF                                       01
082800911009     C*
082900911009     C* RICERCA IN TUTTI I FOGLI VIAGGIO
083000960627   2AC     *IN17         IFEQ      *OFF
083100960627     C*
083200950328     C     '?'           SCAN      VI2LNP                                 09
083300951109    3C   09VI2FVA        IFGT      *ZEROS
083400920327     C                   SETON                                        4990
083500920327     C                   SETOFF                                       09
083600920327     C                   GOTO      ENDCTS
083700951109    3C                   END
083800971002     C*
083900971002     C* SE RICHIMAMATO DA LR26 NON POSSO RITORNARCI
084000971002     C   09RICLAN        IFEQ      '6'
084100971002     C                   SETON                                        5090
084200971002     C                   SETOFF                                       09
084300971002     C                   GOTO      ENDCTS
084400971002     C                   ENDIF
084500920327     C*
084600951109    3C   09              DO
084700960205     C                   EXSR      PULRIG
084800941221     C                   CLEAR                   PARA26
084900921119     C                   MOVEA     *ZEROS        FVA
085000941221     C                   MOVEL     'FNLR25R'     P26PGM
085100941221     C                   MOVEL     VIDFGS        P26LAI
085200941221     C                   MOVEL     SIMFEL        P26FEL
085300950104     C                   MOVEL     'N'           P26ABB                         N=NON ABBINATI
085400971002     C* RICHIAMATO DA FNLR25R
085500971002     C                   MOVEL     '5'           P26LAN
085600941221     C                   MOVEL     PARA26        KPJBU
085700941221     C                   CALL      'FNLR26R'
085800911009     C                   PARM                    KPJBA
085900921119     C                   PARM                    FVA
086000941221     C                   MOVEL     KPJBU         PARA26
086100911009     C*
086200911009     C* SE SELEZIONATI FV LI SCRIVO
086300911009     C                   MOVEL     *ZEROS        VI2FVA
086400911010     C                   MOVEL     *ZEROS        VI2LNP
086500941216     C                   UPDATE    LR25DSF
086600911009     C*
086700911009     C                   SUB       1             NRR
086800911009     C                   Z-ADD     1             I                 4 0
086900911009     C*
087000980204     C     I             DOWLE     70
087100980204    4C     FVA(I)        ANDNE     *ZEROS
087200911010     C                   ADD       1             NRR
087300941216     C     NRR           CHAIN     LR25DSF                            30
087400911009     C*
087500911009     C* CREO NUOVA PAGINA DEL SUBFILE
087600920316     C   30              Z-ADD     NRR           COMNRR            5 0
087700920316     C   30              Z-ADD     1             B
087800911009     C   30              EXSR      PULSFL
087900920316     C   30              Z-ADD     COMNRR        NRR
088000941216     C   30NRR           CHAIN     LR25DSF                            30
088100911010     C* MEMORIZZO SOLO SE E' VUOTO ALTRIMENTI SU ALTRA RIGA
088200951109    5C     VI2LNP        IFEQ      *ZEROS
088300911010     C     VI2LNP        OREQ      *BLANKS
088400951109    6C     VI2FVA        IFEQ      0
088500911009     C                   MOVEL     FVA(I)        VI2LNP
088600911009     C                   MOVE      FVA(I)        VI2FVA
088700980109     C* CERCO SULLA L6
088800980109     C                   Z-ADD     1             XX
088900980109    7C     L6(XX)        DOWGT     0
089000960205     C                   MOVEL     VI2LNP        COMLNP
089100980112     C                   MOVEL     L6(XX)        COMLAI
089200960205     C     KFVA1         CHAIN(N)  FNFVA01L                           31
089300980109    8C     *IN31         IFEQ      *OFF
089400980109     C                   MOVEL     L6(XX)        VI2LAI
089500980204     C     VI2KEY        LOOKUP    FVS                                    31
089600980204     C  N31              EXSR      DATARR
089700980204     C  N31              UPDATE    LR25DSF
089800980204     C   31              SUB       1             NRR
089900980204     C                   ADD       1             I
090000980109     C                   Z-ADD     30            XX
090100980109   X8C                   ELSE
090200980109     C                   ADD       1             XX
090300980109    7C                   ENDIF
090400980109    6C                   ENDDO
090500980109     C**
090600951109    6C                   END
090700951109    5C                   END
090800911009     C*
090900951109    4C                   END
091000980204     C**
091100980204     C* SE C'E' MESSAGGIO EMETTO
091200980204     C     P26MSG        IFNE      *BLANKS
091300980204     C                   MOVEL     P26MSG        VCCMSG
091400980204     C                   SETON                                        28
091500980204     C                   CLEAR                   P26MSG
091600980204     C                   ENDIF
091700911009     C* ESCO DA CONTROLLI
091800911009     C                   SETON                                        90
091900951109    4C     NRR           IFLT      1
092000920327     C                   Z-ADD     1             NRR
092100951109    4C                   END
092200951109     C                   GOTO      ENDCTS
092300951109    3C                   END
092400960627     C*
092500960627   2AC                   END
092600911009     C*
092700951109    3C     VI2LNP        IFNE      *ZEROS
092800911010     C     VI2LNP        ANDNE     *BLANKS
092900920709     C     VI2FVA        ORNE      0
093000960202     C*
093100911010     C* LINEA PARTENZA
093200911010     C                   TESTN                   VI2LNP               31
093300911008     C  N31              SETON                                        4490
093400911008     C  N31              GOTO      ENDCTS
093500911008     C*
093600911010     C                   MOVEL     VI2LNP        COMLNP            3 0
093700911010     C*
093800951109    4C     COMLNP        IFEQ      0
093900920709     C     VI2FVA        ANDNE     0
094000911010     C                   SETON                                        4490
094100951109    4C                   END
094200951109    4C     COMLNP        IFNE      0
094300920709     C     VI2FVA        ANDEQ     0
094400911010     C                   SETON                                        4690
094500951109    4C                   END
094600911010     C*
094700911010     C   90              GOTO      ENDCTS
094800911010     C*
094900980109     C                   Z-ADD     1             XX
095000980109     C                   CLEAR                   WTROV
095100980109     C**
095200980109    4C     L6(XX)        DOWGT     0
095300980109     C                   Z-ADD     L6(XX)        COMLAI
095400980112     C                   MOVEL     L6(XX)        VI2LAI
095500980109     C     KFVA1         CHAIN(N)  FNFVA01L                           32
095600980109    5C     *IN32         IFEQ      *ON
095700980109     C                   ADD       1             XX
095800980109   X5C                   ELSE
095900980109     C                   MOVEL     'S'           WTROV             1
096000980109     C                   Z-ADD     30            XX
096100911010     C*
096200921123     C* GIA' ABBINATO CON ALTRO FOGLIO VIAGGIO
096300980109    6C     FVANFA        IFNE      0
096400921123     C     FVANFA        ANDNE     COMNFV
096500030723     c                   movel     MSG(4)        VCCmsg
096600030723     c                   eval      %subst(VCCmsg:54:1) =
096700030723     c                               %trim(%editw(FVAnpg:'0 '))
096800030723     c                   eval      %subst(VCCmsg:58:5) =
096900030723     c                               %trim(%editw(FVAnfa:'0     '))
097000951121     C                   SETON                                        459028
097100921123     C                   GOTO      ENDCTS
097200980109    6C                   END
097300950104     C*
097400950104     C     VI2KEY        LOOKUP    FVS                                    31
097500950104     C* MEMORIZZO NELLA SCHIERA DEI FV SELEZIONATI
097600980109    6C     *IN31         IFEQ      *OFF
097700980109     C                   ADD       1             X
097800980109     C                   Z-ADD     VI2KEY        FVS(X)
097900980109     C                   MOVEL     VI2STA        STS(X)
098000980109     C                   MOVEL     FVAATR        ATS(X)
098100980109     C                   Z-ADD     FVATRN        TRN(X)
098200980109     C                   Z-ADD     FVADTN        DTN(X)
098300040122     C                   Z-ADD     FVADfv        Dfv(X)
098400980109     C                   MOVEA     WDHARR        DHA(X)
098500980109     C                   ELSE
098600950104     C* DOPPIO
098700980109     C                   SETON                                        4890
098800980109     C                   GOTO      ENDCTS
098900980109    6C                   ENDIF
099000950104     C*
099100960205     C                   EXSR      DATARR
099200980109    5C                   ENDIF
099300980109    4C                   ENDDO
099400980109     C** FOGLIO NON TROVATO
099500980109     C     WTROV         IFEQ      *BLANKS
099600980109     C                   SETON                                        4690
099700980109     C                   GOTO      ENDCTS
099800980109     C                   ENDIF
099900950104     C*
100000950104     C* SE IMMESSO VALORE NEGATIVO TRASFORMO IN POSITIVO
100100950104     C                   MLLZO     1             VI2AEH
100200950104     C*
100300950104     C                   MOVEL     VI2AEH        HH                2 0
100400950104     C                   MOVE      VI2AEH        MM                2 0
100500950104     C*
100600951109    4C     HH            IFGT      23
100700960205     C     MM            ORGT      59
100800960205     C     VI2AEH        OREQ      0
100900950104     C                   SETON                                        5690
101000950104     C                   GOTO      ENDCTS
101100951109    4C                   ENDIF
101200930129     C*
101300951114     C* CONTROLLO DATA ARRIVO MEZZO (DATA E ORA SONO OBBLIGATORIE)
101400930129     C                   Z-ADD     VI2AED        G02DAT
101500930129     C                   MOVEL     *BLANKS       G02ERR
101600941221     C                   CALL      'XSRDA8'
101700930129     C                   PARM                    WLBDAT
101800930129     C*
101900951115    4C     G02ERR        IFEQ      '1'
102000930129     C                   SETON                                        5590
102100930129     C                   GOTO      ENDCTS
102200951115    4C                   ENDIF
102300951228     C                   Z-ADD     G02INV        WAED8
102400941221     C*
102500941221     C* DATA ARRIVO NON PUO' ESSERE < DATA FOGLIO
102600951115    4C     G02INV        IFLT      FVADFV
102700930129     C                   SETON                                        5490
102800930129     C                   GOTO      ENDCTS
102900951115    4C                   ENDIF
103000030924      * Per foglio illuminato
103100030924    4c                   If        FvaAtr <> *Blanks
103200030924      * cerco il terminal di arrivo
103300030924     c                   ExSr      RicTfa
103400030924      * se secondo livello in arrivo su altro AS
103500030924    5c                   If        Simfel <> d98Tfa and
103600030924      * Controllo l'ora arrivo reale
103700030924      * non deve essere maggiore dell'ora di abbinamento foglio
103800030924     c                             vi2aeh <> sa2aeh and
103900030924     c                             vi2aeh >  sa2aeh
104000030924     c                   If        sa2aeh > *Zeros
104100030924     c                   Move      sa2aeh        worario
104200030924     c                   Else
104300030924     c                   Move      wtime         worario
104400030924     c                   EndIf
104500030924     c                   Eval      VccMsg = Msg(19)
104600030924     c                   Eval      %subst(VccMsg:72:2) = wore
104700030924     c                   Eval      %subst(VccMsg:75:2) = wminuti
104800030924     c                   Eval      *In28 = *On
104900030924     c                   Eval      *In53 = *On
105000030924     c                   Eval      *In90 = *On
105100030924     c                   GoTo      EndCts
105200030924    5c                   EndIf
105300030924    4c                   EndIf
105400941221     C*
105500960227     C* PULISCO I CAMPI DI FORZATURA SOLO SE VARIANO DATA/ORA ARRIVO
105600960227    4C     SA2AED        IFNE      VI2AED
105700960227     C     SA2AEH        ORNE      VI2AEH
105800960227     C     FVATRN        IFGT      0
105900960227     C                   CLEAR                   WFORZ
106000960227     C                   CLEAR                   WFORZ2
106100960227     C                   ENDIF
106200960227     C*
106300960227     C                   CLEAR                   WFORZ3
106400960227     C                   MOVEL     VI2AED        SA2AED
106500960227     C                   MOVEL     VI2AEH        SA2AEH
106600960227    4C                   ENDIF
106700960227     C*
106800960227     C* DATA/ORA ARRIVO NON POSSONO ESSERE MAGGIORI DELLA DATA/ORA
106900960227     C*   CORRENTE: ENTER PER FORZARE
107000960227    4C     WFORZ3        IFEQ      ' '
107100000606     C* DATA ARRIVO NON PUO' ESSERE > DATA FOGLIO ARRIVI
107200000606    5C     WAED8         IFGT      FVVDFV
107300000606     C                   MOVEL     MSG(20)       VCCMSG
107400000606     C                   SETON                                        289061
107500000606     C                   MOVEL     '1'           WFORZ3
107600000606     C                   GOTO      ENDCTS
107700000606    5C                   ENDIF
107800960227    4C                   ENDIF
107900960227     C*
108000951117     C* CONTROLLO SE PRESENTE   R I T A R D O / A N T I C I P O
108100951116     C                   SETOFF                                       18
108200951115     C* CONTROLLO SE SI TRATTA DI FOGLIO VIAGGIO ABBINATO AD UN TRAINO
108300951116    4C     FVATRN        IFGT      0
108400960205     C*
108500960227     C* SOLO SE F.VIAGGIO NON E' ILLUMINATO
108600960227   5AC     FVAATR        IFEQ      ' '
108700951115     C*
108800951117     C* SE SONO STATI IMMESSI PIU' F.VIAGGIO CON STESSO NUM.TRAINO E
108900960222     C*   DATA TRAINO CONTROLLO ABBIANO LA STESSA DATA/ORA/CAUS. ARRIVO
109000951117     C                   Z-ADD     1             Y                 3 0
109100951120     C     FVATRN        LOOKUP    TRN(Y)                                 35
109200951117    5C     *IN35         IFEQ      *ON
109300951117     C     VI2KEY        ANDNE     FVS(Y)
109400951117     C     FVADTN        ANDEQ     DTN(Y)
109500951121     C     WDHARR        ANDNE     DHA(Y)
109600030723     c                   movel     MSG(7)        VCCmsg
109700030723     c                   eval      %subst(VCCmsg:07:3) = VI2lnp
109800030723     c                   eval      %subst(VCCmsg:11:5) =
109900030723     c                               %trim(%editc(VI2fva:'Z'))
110000030723     c                   z-add     FVS(Y)        WKEY
110100030723     c                   eval      %subst(VCCmsg:19:3) =
110200030723     c                               %trim(%editw(Wlnp:'0   '))
110300030723     c                   eval      %subst(VCCmsg:23:5) =
110400030723     c                               %trim(%editc(Wfva:'Z'))
110500030723     c                   eval      %subst(VCCmsg:40:7) =
110600030723     c                               %editc(FVAtrn:'Z')
110700951117     C                   SETON                                        289061
110800951117     C                   GOTO      ENDCTS
110900951117    5C                   ENDIF
111000951109     C*
111100951128     C*
111200951128     C* CONTROLLO SE ESISTONO ALTRI F.VIAGGIO ABBINATI AD ALTRO F.ARRIVI
111300960222     C*   CON LO STESSO NUMERO E DATA TRAINO MA DIVERSE DATA/ORA/CAUS.
111400960222     C*   ARRIVO: SE ESISTONO EMETTO MESSAGGIO FORZABILE PER AVVISARE
111500951128    5C     WFORZ2        IFEQ      ' '
111600951128     C     KFVA4         SETLL     FNFVA004
111700951128     C     KFVA4         READE     FNFVA004                               34
111800951128    6C     *IN34         DOWEQ     *OFF
111900960202    7C     F04NFA        IFGT      0
112000960202     C     F04NFA        ANDNE     COMNFV
112100960227     C     F04LAI        ANDEQ     FVALAI
112200960202    8C     F04AED        IFNE      WAED8
112300960202     C     F04AEH        ORNE      VI2AEH
112400960222     C     F04RTA        ORNE      VI2RTC
112500030723      * Foglio letto già abbinato con data reale <> di arrivo
112600030723     c                   movel     MSG(10)       VCCmsg
112700030723      * - foglio in errore
112800030723     c                   eval      %subst(VCCmsg:01:3) =
112900030723     c                               %trim(%editw(FVAlnp:'0   '))
113000030723     c                   eval      %subst(VCCmsg:05:5) =
113100030723     c                               %trim(%editc(FVAnfv:'Z'))
113200030723      * - data di arrivo
113300030723     c                   z-add     F04aed        G02inv
113400030723     c                   clear                   G02dat
113500030723     c                   movel     3             G02err
113600030723     c                   call      'XSRDA8'
113700030723     c                   parm                    WLBDAT
113800030723     c                   eval      %subst(VCCmsg:36:5) =
113900030723     c                               %subst(%editc(G02dat:'Y'):1:5)
114000030723      * - ora di arrivo
114100030723     c                   eval      %subst(VCCmsg:42:5) =
114200030723     c                               %editw(F04aeh:'0 :  ')
114300030723     c                   eval      %subst(VCCmsg:48:1) = F04rta
114400030723     c                   eval      %subst(VCCmsg:56:3) =
114500030723     c                               %trim(%editw(F04lnp:'0   '))
114600030723     c                   eval      %subst(VCCmsg:60:5) =
114700030723     c                               %trim(%editc(F04nfv:'Z'))
114800951128     C                   SETON                                        289061
114900951128     C                   MOVEL     '1'           WFORZ2
115000951128     C                   GOTO      ENDCTS
115100951128    8C                   ENDIF
115200951128    7C                   ENDIF
115300951128     C     KFVA4         READE     FNFVA004                               34
115400951128    6C                   ENDDO
115500951128    5C                   ENDIF
115600951128     C*
115700960227   5AC                   ENDIF
115800951115    4C                   ENDIF
115900951114     C*
116000960626     C*
116100960626     C* CONTROLLO ESISTENZA RITARDO SOLO SE IL FV NON E' ILLUMINATO E
116200960626     C*   DATA/ORA TEORICA E' DIVERSA DA DATA/ORA REALE
116300960626    4C     FVADTA        IFGT      0
116400960626    5C     FVADTA        IFNE      WAED8
116500960626     C     FVAHTA        ORNE      VI2AEH
116600960626     C                   CLEAR                   DSTL35
116700960626     C                   MOVEL     'C'           D35TUP
116800970825     C                   MOVEL     'A'           D35EPA
116900960626     C                   MOVEL     FVADTA        D35DTE
117000960626     C                   MOVEL     FVAHTA        D35HTE
117100960626     C                   MOVEL     WAED8         D35DRE
117200960626     C                   MOVEL     VI2AEH        D35HRE
117300960626     C                   MOVEL     DSTL35        KPJBU
117400960626     C                   CALL      'TNTL35R'
117500960626     C                   PARM                    KPJBA
117600960626     C                   MOVEL     KPJBU         DSTL35
117700960626     C                   MOVEL     '1'           WTL35             1
117800960626     C*
117900960626     C* D35FLG <> " "  ---> PRESENTE RITARDO/ANT. NELL'ARRIVO DEL MEZZO
118000960626    6C     D35FLG        IFNE      ' '
118100960626     C                   SETON                                        18
118200960626    6C                   ENDIF
118300960626    5C                   ENDIF
118400960626     C*
118500960626   X4C                   ELSE
118600960626     C*
118700960626     C* SE FV ILLUMINATO
118800960626    5C     FVAATR        IFNE      ' '
118900960626     C* CALCOLO I TEMPI DI TRAZIONE ED IN BASE ALLE SOGLIE CALCOLO
119000960626     C*   L'ORA AL DI SOPRA DELLA QUALE CHIEDERE IL RITARDO
119100960626     C                   EXSR      CALSOG
119200960626     C*
119300960626     C* CONTROLLO SE PRESENTE RITARDO
119400960626     C                   SELECT
119500960626     C     WAED8         WHENEQ    DATSOG
119600960626    6C     VI2AEH        ANDGT     ORASOG
119700960626     C                   SETON                                        18
119800960626     C*
119900960626     C     WAED8         WHENGT    DATSOG
120000960626     C                   SETON                                        18
120100960626     C                   ENDSL
120200960626     C*
120300960626     C                   ELSE
120400960626     C                   MOVEL     '1'           WFORZ
120500960626    5C                   ENDIF
120600960626    4C                   ENDIF
120700960626     C*
120800960626     C* 17 ON -  DETERMINO SE CAUSALE RITARDO MANUTENZIONABILE
120900960626     C                   MOVE      *BLANKS       WCOSA             1            TUTTO
121000960626    4C     *IN17         IFEQ      *ON
121100960626    5C     *IN18         IFEQ      *ON
121200960626    6C     SAVRTC        IFNE      *BLANKS
121300960626     C                   MOVE      'N'           WCOSA                          NESSUNA VAR.
121400960626    6C                   END
121500960626   X5C                   ELSE
121600960626    6C     SAVRTC        IFNE      *BLANKS
121700960626     C                   MOVE      'T'           WCOSA                          SOLO TOGLIER
121800960626   X6C                   ELSE
121900960626     C                   MOVE      'N'           WCOSA                          NESSUNA VAR.
122000960626    6C                   END
122100960626    5C                   END
122200960626    4C                   END
122300960626     C*
122400960626    4C                   SELECT
122500960626     C* WCOSA='N' --> CAUSALE/DESCRIZIONI RITARDO NON MANUTENZIONABILI
122600960626     C     WCOSA         WHENEQ    'N'
122700960626    5C     VI2RTC        IFNE      SAVRTC
122800960626    6C     SAVRTC        IFNE      *BLANKS
122900960626     c* Impossibile modificare o togliere la causale
123000030723     c                   movel     MSG(14)       VCCmsg
123100030723     c                   eval      %subst(VCCmsg:71:1) = SAVrtc
123200960626   x6C                   ELSE
123300960626     c* Impossibile immettere la causale
123400960626     C                   MOVEL     MSG(15)       VCCMSG
123500960626    6C                   END
123600960626     C                   SETON                                        906028
123700960626     C                   GOTO      ENDCTS
123800960626    5C                   END
123900960626    5C     VI2GES        IFNE      *BLANKS
124000960626     c* impossibile modificare la descrizione
124100960626     C                   MOVEL     MSG(16)       VCCMSG
124200960626     C                   SETON                                        906228
124300960626     C                   GOTO      ENDCTS
124400960626    5C                   END
124500960626     C* WCOSA='T' --> LA CAUSALE PUO' SOLO ESSERE ABBLENCATA
124600960626     C     WCOSA         WHENEQ    'T'
124700960626    5C     VI2RTC        IFNE      *BLANKS
124800960626     C     VI2RTC        ANDNE     SAVRTC
124900030723     c                   movel     MSG(17)       VCCmsg
125000030723     c                   eval      %subst(VCCmsg:66:1) = SAVrtc
125100960626     C                   SETON                                        906028
125200960626     C                   GOTO      ENDCTS
125300960626    5C                   END
125400960626    4C                   ENDSL
125500960626     C*
125600951115     C* RICERCA CAUSALE RITARDO/ANTICIPO
125700951115    4C     VI2RTC        IFEQ      '?'
125800951116     C                   MOVEL     'TC'          §COD
125900951115     C                   MOVEL     *BLANKS       VI2RTC
126000951116     C                   MOVEL     *BLANKS       §DES
126100951115     C                   CALL      'X§TABER'
126200951115     C                   PARM                    CODUT
126300951116     C                   PARM                    §COD
126400951116     C                   PARM                    §KEY
126500951116     C                   PARM                    §DES
126600951116     C                   MOVEL     §KEY          VI2RTC
126700951116     C                   MOVEL     §DES          VI2DTC
126800960207     C                   SETON                                        906045
126900951115     C                   GOTO      ENDCTS
127000951115    4C                   ENDIF
127100951115     C*
127200951115     C* CONTROLLO VALIDITA' CAUSALE
127300951115    4C     VI2RTC        IFNE      ' '
127400951115     C                   MOVEL     'TC'          COD
127500951115     C                   MOVEL(P)  VI2RTC        KEY
127600951115     C     KTAB          CHAIN     TABEL                              34
127700951115    5C     *IN34         IFEQ      *OFF
127800951115     C     TBLFLG        ANDNE     ' '
127900951115     C                   SETON                                        34
128000951115    5C                   ENDIF
128100951115     C*
128200951115    5C     *IN34         IFEQ      *ON
128300951115     C                   MOVEL     MSG(1)        VCCMSG
128400951115     C                   SETON                                        289060
128500951115     C                   GOTO      ENDCTS
128600951115   X5C                   ELSE
128700951115     C                   MOVEL     TBLUNI        VI2DTC
128800951115    5C                   ENDIF
128900951115     C*
129000951115   X4C                   ELSE
129100951115     C                   CLEAR                   VI2DTC
129200951115    4C                   ENDIF
129300960205     C*
129400951115     C* SE ARRIVO IN RITARDO/ANTICIPO --> CAUSALE OBBLIGATORIA
129500951115    4C     *IN18         IFEQ      *ON
129600951115     C     VI2RTC        ANDEQ     ' '
129700951228     C                   MOVEL     MSG(2)        VCCMSG
129800951115     C                   SETON                                        289060
129900951115     C                   GOTO      ENDCTS
130000951115    4C                   ENDIF
130100951115     C*
130200951115     C* SE MODIFICANDO LA DATA/ORA ARRIVO EFFETTIVA IL RITARDO/ANTICIPO
130300951115     C*   SCOMPARE, CHIEDERE FORZABILE DI ELIMINARE LA CAUSALE
130400951228    4C  N18WFORZ         IFEQ      ' '
130500960202     C     VI2RTC        ANDNE     ' '
130600951115     C                   MOVEL     MSG(3)        VCCMSG
130700951115     C                   SETON                                        289060
130800951228     C                   MOVEL     '1'           WFORZ
130900951115     C                   GOTO      ENDCTS
131000951115    4C                   ENDIF
131100951115     C*
131200951115     C* SE IMMESSA LA "G" DI CAUSALE GENERICA, RICHIEDERE OBBLIGATORIA
131300951115     C*   LA DESCRIZIONE RITARDO/ANTICIPO
131400951115    4C     VI2RTC        IFEQ      'G'
131500951115     C* W001A = " " --> NON ESISTE LA DESCRIZIONE RITARDO/ANTICIPO
131600960205     C     VI2DES        ANDEQ     *BLANKS
131700951115     C     VI2GES        ANDEQ     ' '
131800951228     C                   MOVEL     MSG(5)        VCCMSG
131900960205     C                   SETON                                        289062
132000951115     C                   GOTO      ENDCTS
132100951115    4C                   ENDIF
132200951115     C*
132300951115     C* CONTROLLO SE SI VUOLE INSERIRE LA DESCRIZIONE RITARDO/ANTICIPO
132400951115    4C     VI2GES        IFNE      ' '
132500960201     C                   CLEAR                   DSLV15
132600960201     C                   MOVEL     'M'           D15TUP
132700960201     C                   MOVEL     'A'           D15EPA
132800960201     C                   MOVEL     VI2RTC        D15RTC
132900960201     C                   MOVEL     FVALNP        D15LNP
133000960201     C                   MOVEL     FVANFV        D15NFV
133100960201     C                   MOVEL     FVALAI        D15LAI
133200960201     C                   MOVEL     'R'           D15TRC
133300960201     C                   MOVEL     FVATRN        D15TRN
133400960222     C                   MOVEL     FVATTR        D15TTR
133500960201     C                   MOVEL     FVADFV        D15DFV
133600960305     C                   MOVEL     FVAATR        D15ATR
133700951115     C*
133800960201     C                   MOVEL     DSLV15        KPJBU
133900951115     C                   CALL      'FNLV15R'
134000951115     C                   PARM                    KPJBA
134100960201     C                   MOVEL     KPJBU         DSLV15
134200960202     C                   MOVEL     '1'           WLV15             1
134300951115     C*
134400960201     C* SE VARIATO QUALCOSA IN FNFV4 ED ESCO DALL'ABBINAMENTO SENZA
134500960201     C*   CONFERMARE SOTTOMETTO PGM DI TRASMISSIONE
134600960201    5C     D15FLG        IFNE      ' '
134700960201     C                   MOVEL     '1'           WAGG              1
134800951115    5C                   ENDIF
134900951121     C*
135000951121     C* PULISCO IL CAMPO UTILIZZATO PER VISUALIZZARE LA DESCRIZIONE
135100951121     C                   CLEAR                   VI2GES
135200951115     C*
135300951115     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
135400951115     C*   RIEMPIO CAMPO CHE NE INDICA L'ESISTENZA
135500951115     C                   EXSR      CTRDRA
135600960201    5C     W001A         IFNE      ' '
135700951130     C                   MOVEL     W010A         VI2DES
135800960205   X5C                   ELSE
135900960205     C                   CLEAR                   VI2DES
136000960205    5C                   ENDIF
136100960205    4C                   ENDIF
136200951121     C*
136300951121     C* SE INSERITA LA DESCRIZIONE RITARDO/ANTICIPO OCCORRE INSERIRE
136400951121     C*   ANCHE LA CAUSALE
136500960205    4C     VI2DES        IFNE      *BLANKS
136600960205     C     VI2RTC        ANDEQ     ' '
136700951228     C                   MOVEL     MSG(6)        VCCMSG
136800951121     C                   SETON                                        289060
136900951121     C                   GOTO      ENDCTS
137000960205    4C                   ENDIF
137100951115     C*
137200951117     C* CONTROLLO SE RICHIESTA LA VISUALIZZAZIONE DEL F.VIAG. PARTENZA
137300951117    4C     VI2VFV        IFEQ      'V'
137400971002     C* NON POSSO ANDARCI SE PROVENGO DA FNLR26
137500971002     C     RICLAN        IFEQ      '6'
137600971002     C                   MOVEL     MSG(18)       VCCMSG
137700971003     C                   SETON                                        289063
137800971002     C                   GOTO      ENDCTS
137900971002     C                   ENDIF
138000971002     C**
138100951120     C                   CLEAR                   PARA26
138200951120     C                   Z-ADD     VI2FVA        FVA(1)
138300951120     C                   MOVEL     'FNLR25R'     P26PGM
138400951120     C                   MOVEL     VI2LNP        P26LNP
138500951120     C                   MOVEL     VI2LAI        P26LAI
138600971002     C* RICHIAMATO DA FNLR25R
138700971002     C                   MOVEL     '5'           P26LAN
138800951120     C                   MOVEL     PARA26        KPJBU
138900951120     C                   CALL      'FNLR26R'
139000951120     C                   PARM                    KPJBA
139100951120     C                   PARM                    FVA
139200951120     C*
139300951120     C* PULISCO IL CAMPO UTILIZZATO PER VISUALIZZARE IL FOGLIO
139400951120     C                   CLEAR                   VI2VFV
139500951117    4C                   ENDIF
139600951109     C*
139700951109   X3C                   ELSE
139800960205     C                   EXSR      PULRIG
139900951109    3C                   END
140000951109    2C                   END
140100930127     C*
140200911009     C     ENDCTS        TAG
140300920706     C*
140400941216     C  N09
140500941216     CANN30              UPDATE    LR25DSF
140600920316     C   90              Z-ADD     NRR           REC
140700920316     C   90              Z-ADD     SAVNRR        NRR
140800911009     C                   ADD       1             NRR
140900951109    1C                   END
141000911008     C*
141100911009     C                   ENDSR
141200960304     C**************************************************************************
141300960304     C*  CALCOLO DATA ORA DI SOGLIA PER I FV ILLUMINATI
141400960304     C**************************************************************************
141500960304     C     CALSOG        BEGSR
141600970925     C* PRIMA FACCIO LA CALL AL TNSD99R PER AVERE I TEMPI DI TRAZIONE
141700970925     C                   CLEAR                   DSSD99
141800960304     C                   MOVEL     'T'           D98TLA
141900960304     C                   MOVEL     FVALNP        D98LNP
142000960304     C                   MOVEL     FVALNP        D98TFP
142100960304     C                   MOVEL     FVALAI        D98LNA
142200960304     C                   MOVEL     FVADFV        D98DSP
142300960304     C                   MOVEL     'E'           D98TSP
142400960304     C* CAP
142500960304     C                   CLEAR                   KNAR
142600960304     C                   MOVEL     FVALAI        KLNA
142700970721     C     KCAP          SETLL     AZCPC03L
142800970721     C     KCAP          READE     AZCPC03L                               32
142900960304     C*
143000960304    1C     *IN32         DOWEQ     *OFF
143100960304     C* SE ANNULLATO O FITTIZIO RILEGGO
143200970721     C     CPCATB        IFNE      ' '
143300970722     C     CPCFIT        OREQ      'S'
143400980109     C                   CLEAR                   CPCCAP
143500970721     C     KCAP          READE     AZCPC03L                               32
143600960304     C                   ELSE
143700960304     C                   SETON                                        32
143800960304     C                   ENDIF
143900960304    1C                   ENDDO
144000980109     C*
144100980109     C* SE NON LO TROVO LO PRENDO DA ORGANIGRAMMA
144200980109     C     CPCCAP        IFEQ      *BLANKS
144300980109     C     FVALAI        CHAIN     AZORG01L                           30
144400980109     C  N30              MOVEL     ORGCPF        D98CAD
144500980109     C                   ELSE
144600970721     C                   MOVEL     CPCCAP        D98CAD
144700980109     C                   ENDIF
144800980109     C**
144900970825     C* CERCO TERMINAL DI ARRIVO
145000960304     C                   EXSR      RICTFA
145100960304     C*
145200970925     C                   CALL      'TNSD99R'
145300970925     C                   PARM                    DSSD99
145400960304     C*
145500960304     C* REPERISCO LE DUE ORE DI SOGLIA
145600960304     C                   MOVEL     '1X'          COD
145700960304     C                   MOVEL(P)  FVALAI        KEY
145800960304     C                   MOVE      'OSA  '       KEY
145900960304     C     KTAB          CHAIN     TABEL                              32
146000980209     C     *IN32         IFEQ      *OFF
146100960304     C  N32              MOVEL     TBLUNI        DS1X
146200960304     C                   MOVEL     §1XHHO        HHOUNO
146300960304     C*
146400960304     C                   MOVE      'OSA1 '       KEY
146500960304     C     KTAB          CHAIN     TABEL                              32
146600960304     C  N32              MOVEL     TBLUNI        DS1X
146700960304     C   32              CLEAR                   §1XHHO
146800960304     C                   MOVEL     §1XHHO        HHODUE
146900980209     C                   ELSE
147000980209     C                   MOVEL(P)  '   '         KEY
147100980209     C                   MOVE      'OSA  '       KEY
147200980209     C     KTAB          CHAIN     TABEL                              32
147300980209     C  N32              MOVEL     TBLUNI        DS1X
147400980209     C   32              CLEAR                   §1XHHO
147500980209     C                   MOVEL     §1XHHO        HHOUNO
147600980209     C*
147700980209     C                   MOVE      'OSA1 '       KEY
147800980209     C     KTAB          CHAIN     TABEL                              32
147900980209     C  N32              MOVEL     TBLUNI        DS1X
148000980209     C   32              CLEAR                   §1XHHO
148100980209     C                   MOVEL     §1XHHO        HHODUE
148200980209     C                   ENDIF
148300960304     C*
148400960304     C*
148500960304    1C                   SELECT
148600960304     C     D98HTI        WHENLE    HHOUNO
148700960304     C                   Z-ADD     HHOUNO        ORASOG
148800960304     C                   Z-ADD     D98DTI        DATSOG
148900960304     C*
149000960304     C     D98HTI        WHENLE    HHODUE
149100960304     C                   Z-ADD     HHODUE        ORASOG
149200960304     C                   Z-ADD     D98DTI        DATSOG
149300960304     C*
149400960304   X1C                   OTHER
149500960304     C*
149600960304     C* SOMMO 1 GIORNO E PRENDO LA PRIMA SOGLIA DEL GIORNO DOPO
149700960304     C                   CLEAR                   WLBDAT
149800960304     C                   MOVEL     '3'           G02ERR
149900960304     C                   Z-ADD     D98DTI        G02INV
150000960304     C                   CALL      'XSRDA8'
150100960304     C                   PARM                    WLBDAT
150200960304     C*
150300960304     C* ADD 1 A GIORNI
150400960304     C     G02TGI        ADD       1             GIOTGI
150500960304     C                   CALL      'XSRGI8'
150600960304     C                   PARM                    WGIDAT
150700960304     C                   MOVEL     GIOINV        DATSOG
150800960304     C                   Z-ADD     HHOUNO        ORASOG
150900960304    1C                   ENDSL
151000960304     C                   ENDSR
151100960304     C**************************************************************************
151200960304     C*  PREPARA MESSAGGIO 10
151300960304     C**************************************************************************
151400960304     C     RICTFA        BEGSR
151500970825     C                   CLEAR                   DSLV55
151600970825     C                   MOVEL     'A'           D55TPT
151700970825     C                   MOVEL     FVALAI        D55LIN
151800970825     C                   MOVEL     FVALNP        D55LNP
151900970825     C                   MOVEL     FVADFV        D55DRF
152000970825     C                   CALL      'FNLV55R'
152100970825     C                   PARM                    DSLV55
152200970825     C     D55ERR        IFEQ      ' '
152300970825     C                   MOVEL     D55TFA        D98TFA
152400970825     C                   ELSE
152500970825     C                   MOVEL     FVALAI        D98TFA
152600960304    2C                   ENDIF
152700960304     C*
152800960304     C                   ENDSR
152900951115     C**************************************************************************
153000951115     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
153100951115     C**************************************************************************
153200951115     C     CTRDRA        BEGSR
153300951115     C                   CLEAR                   W001A
153400951130     C                   CLEAR                   W010A
153500951115     C*
153600960202     C     KFV4          SETLL     FNFV401L                               34
153700951115    1C     *IN34         IFEQ      *ON
153800070918     C     KFV4          READE     FNFV401L                               34
153900951115    2C     *IN34         DOWEQ     *OFF
154000951115    3C     FV4ATB        IFEQ      ' '
154100951116     C                   MOVEL     '1'           W001A             1
154200951130     C                   MOVEL     FV4NOT        W010A            10
154300951115     C                   SETON                                        34
154400951115   X3C                   ELSE
154500070918     C     KFV4          READE     FNFV401L                               34
154600951115    3C                   ENDIF
154700951115    2C                   ENDDO
154800951115    1C                   ENDIF
154900951115     C*
155000951115     C                   ENDSR
155100960111     C**************************************************************************
155200960111     C* CONTROLLO COSA PROPORRE NELLA DATA/ORA ARRIVO REALE
155300000628     C* ... carica anche P.O. gestione POSTE
155400960111     C**************************************************************************
155500960111     C     DATARR        BEGSR
155600960205     C****
155700960205     C** D A T A / O R A     A R R I V O
155800960205     C****
155900960205     C* SE NON HO DIGITATO NULLA VERIFICO COSA DEVO PROPORRE
156000960205    1C     VI2AEH        IFEQ      0
156100960205     C*
156200960205     C* SE PRESENTE PROPONGO DATA/ORA ARRIVO REALE IN QUANTO SI TRATTA
156300960205     C*   DI UN FV CHE HO GIA' RICEVUTO IN PRECEDENZA
156400960205    2C     FVAAED        IFGT      0
156500960205     C* DATA ARRIVO REALE
156600960205     C                   Z-ADD     FVAAED        G02INV
156700960205     C                   MOVE      *ZEROS        G02DAT
156800960205     C                   MOVEL     3             G02ERR
156900960205     C                   CALL      'XSRDA8'
157000960205     C                   PARM                    WLBDAT
157100960205     C* LA DATA ARRIVO MEZZO LA VISUALIZZO LUNGA 6
157200960205     C                   MOVE      G02DAT        W0020
157300960205     C                   MOVEL     G02DAT        VI2AED
157400960205     C                   MOVE      W0020         VI2AED
157500960205     C                   MOVEL     VI2AED        SA2AED
157600960205     C* ORA ARRIVO REALE
157700960205     C                   MOVEL     FVAAEH        VI2AEH
157800960205     C                   MOVEL     FVAAEH        SA2AEH
157900960205     C*
158000960205   X2C                   ELSE
158100960205     C*
158200960111     C* PER I FV ABBINATI A TRAINO PROPORRE LA DATA TEORICA; PER GLI
158300960206     C*   ALTRI PROPORRE LA DATA DEL GIORNO CON ORA 00:00
158400960205    3C     FVADTA        IFGT      0
158500960205     C* DATA ARRIVO TEORICA
158600960111     C                   Z-ADD     FVADTA        G02INV
158700960111     C                   MOVE      *ZEROS        G02DAT
158800960111     C                   MOVEL     3             G02ERR
158900960111     C                   CALL      'XSRDA8'
159000960111     C                   PARM                    WLBDAT
159100960205     C                   MOVE      G02DAT        W0020
159200960205     C                   MOVEL     G02DAT        VI2AED
159300960111     C                   MOVE      W0020         VI2AED
159400960205     C                   MOVEL     VI2AED        SA2AED
159500960111     C* ORA ARRIVO TEORICA
159600960111     C                   Z-ADD     FVAHTA        VI2AEH
159700960205     C                   MOVEL     FVAHTA        SA2AEH
159800960206     C*
159900960206   X3C                   ELSE
160000960206     C* SE FV ILLUMINATO NON PROPONGO COME ORA 00:00 MA PROPONGO TIME
160100960206    4C     FVAATR        IFNE      ' '
160200960206     C                   MOVEL     WTIME         VI2AEH
160300960206     C                   MOVEL     VI2AEH        SA2AEH
160400960206    4C                   ENDIF
160500960205    3C                   ENDIF
160600960205    2C                   ENDIF
160700960205    1C                   ENDIF
160800960111     C*
160900960205     C****
161000960205     C** C A U S A L E     R I T A R D O
161100960205     C****
161200960205     C* SE NON HO DIGITATO NULLA VERIFICO SE C'E' QUALCOSA DA PROPORRE
161300960205    1C     VI2RTC        IFEQ      ' '
161400960206     C     WPRVOL        ANDEQ     ' '
161500960205     C*
161600960205     C* SE PRESENTE VISUALIZZO LA CAUSALE RITARDO/ANTICIPO
161700960205     C     FVARTA        ANDNE     ' '
161800960111     C                   MOVEL     FVARTA        VI2RTC
161900960111     C                   MOVEL     FVARTA        SAVRTC
162000960111     C* DECODIFICA
162100960111     C                   MOVEL     'TC'          COD
162200960111     C                   MOVEL(P)  VI2RTC        KEY
162300960111     C     KTAB          CHAIN     TABEL                              34
162400960205    2C     *IN34         IFEQ      *OFF
162500960111     C     TBLFLG        ANDEQ     ' '
162600960111     C                   MOVEL     TBLUNI        VI2DTC
162700960205    2C                   ENDIF
162800960206     C*
162900960206     C                   MOVEL     '1'           WPRVOL
163000960205    1C                   ENDIF
163100960205     C*
163200960205     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
163300960205     C                   EXSR      CTRDRA
163400960205     C* W001A = " " --> NON ESISTE LA DESCRIZIONE RITARDO/ANTICIPO
163500960205    2C     W001A         IFNE      ' '
163600960205     C                   MOVEL     W010A         VI2DES
163700960205    2C                   ENDIF
163800960202     C*
163900960202     C* GIRO DATA PARTENZA
164000960202     C                   MOVE      FVADFV        G02INV
164100960202     C                   MOVE      *ZEROS        G02DAT
164200960202     C                   MOVEL     3             G02ERR
164300960202     C                   CALL      'XSRDA8'
164400960202     C                   PARM                    WLBDAT
164500971223     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
164600960202     C                   MOVEL     G02DAT        VI2DFV
164700960202     C*
164800960202     C* SE E' SOLO BUSTA METTO CAMPO IN R.I.
164900960202    1C     FVAATR        IFEQ      'T'
165000960202     C                   SETON                                        01
165100960202   X1C                   ELSE
165200960202     C                   SETOFF                                       01
165300960202    1C                   END
165400960402     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
165500960402     C*  VISUALIZZO CON 2 ASTERISCHI
165600960402     C                   MOVEL     VI2LNP        COMLNP
165700960402     C     KARB          SETLL     FNARB06L                               34
165800960402     C     *IN34         IFEQ      *ON
165900960402     C                   CLEAR                   VI2AST
166000960402     C                   ELSE
166100960402     C                   MOVEL     '**'          VI2AST
166200960402     C                   ENDIF
166300000628     C* P.O.GESTIONE
166400000628     C     KFWA1         CHAIN     FNFWA01L                           74
166500000628     C     *IN74         IFEQ      *OFF
166600000628     C     FWAFGS        ANDNE     0
166700000628     C                   Z-ADD     FWAFGS        WORKPT            3 0
166800000628     C                   ELSE
166900000628     C                   Z-ADD     FVALNP        WORKPT            3 0
167000000628     C                   ENDIF
167100000628     C                   CLEAR                   OG143
167200000628     C     WORKPT        CHAIN     AZORG                              74
167300000628     C   74              MOVE      '  '          VI2PTV
167400000628     C  N74              MOVEL     ORGDE3        OG143
167500020207     C  N74§OGNTW        IFEQ      'PPT'
167600000628     C                   MOVE      'PT'          VI2PTV
167700000628     C                   ELSE
167800000628     C                   MOVE      '  '          VI2PTV
167900000628     C                   ENDIF
168000960111     C*
168100960111     C                   ENDSR
168200951121     C**************************************************************************
168300951121     C*    CONTROLLO F03 / F12
168400951121     C**************************************************************************
168500951121     C     F03F12        BEGSR
168600951121     C                   SETOFF                                       90
168700951121     C* SE IMMESSA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12 O
168800951121     C*   F3 A MENO CHE NON ESISTEVA GIA' LA CAUSALE
168900951121     C                   Z-ADD     1             NRR
169000951121     C     NRR           CHAIN     LR25DSF                            34
169100951121    1C     *IN34         DOWEQ     *OFF
169200951121     C*
169300951121    2C     VI2LNP        IFNE      *ZEROS
169400951121     C     VI2LNP        ANDNE     *BLANKS
169500951121     C*
169600951121     C* SE IMMESSA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12 O
169700951121     C*   F3 A MENO CHE NON ESISTEVA GIA' LA CAUSALE
169800951121    3C     VI2DES        IFNE      *BLANKS
169900951121    4C     SAVRTC        IFEQ      ' '
170000951121    5C     VI2RTC        IFEQ      ' '
170100951121     C                   MOVEL     MSG(6)        VCCMSG
170200960205     C                   SETON                                        286290
170300951121     C                   GOTO      ENDF
170400951121   X5C                   ELSE
170500951121     C                   MOVEL     MSG(8)        VCCMSG
170600960205     C                   SETON                                        286290
170700951121     C                   GOTO      ENDF
170800951121    5C                   ENDIF
170900951121    4C                   ENDIF
171000951121     C*
171100951121   X3C                   ELSE
171200951121     C* SE ELIMINATA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12
171300951121     C*   O F3 SE LA CAUSALE CHE E' GIA' STATA IMMESSA E' "G-GENERICA"
171400951121    4C     SAVRTC        IFEQ      'G'
171500951121     C                   MOVEL     MSG(9)        VCCMSG
171600960205     C                   SETON                                        286290
171700951121     C                   GOTO      ENDF
171800951121    4C                   ENDIF
171900951121    3C                   ENDIF
172000951121    2C                   ENDIF
172100951121     C*
172200951121     C     ENDF          TAG
172300960205     C* ERRORE
172400960205     C     *IN90         IFEQ      *ON
172500960205     C                   MOVEL     VI2LNP        COMLNP
172600960205     C                   MOVEL     VI2LAI        COMLAI
172700960205     C     KFVA1         CHAIN(N)  FNFVA01L                           32
172800960205     C  N32FVAATR        IFEQ      ' '
172900960205     C                   SETOFF                                       01
173000960205     C                   ELSE
173100960205     C                   SETON                                        01
173200960205     C                   ENDIF
173300960205     C**
173400960205     C                   Z-ADD     NRR           REC
173500960205     C                   SETON                                        34
173600951121     C                   UPDATE    LR25DSF
173700960205     C                   ELSE
173800951121     C*
173900951121     C                   ADD       1             NRR
174000960205     C     NRR           CHAIN     LR25DSF                            34
174100960205     C                   ENDIF
174200951121    1C                   ENDDO
174300951121     C*
174400951121     C                   ENDSR
174500951121     C*
174600941216     C**************************************************************************
174700941216     C*    ELABORAZIONE ABBINAMENTO
174800941216     C**************************************************************************
174900911008     C     ELAB          BEGSR
175000911010     C*
175100930129     C                   Z-ADD     1             NRR
175200950328     C* CAMPI FISSI DEL FILE FNAGB PER AGGIORNAMENTO BOLLE
175300950328     C                   MOVEL     'A'           AGBTBO
175400950328     C                   MOVEL     'AB'          AGBAGB
175500950925     C                   MOVEL     'S'           AGBFBS
175600950925     C                   MOVEL     ' '           AGBFVC
175700930129     C*
175800950621     C* LEGGO TUTTI I FOGLI VIAGGIO ABBINATI
175900951228    1C     NRR           DOWLE     SAVNRR
176000941216     C     NRR           CHAIN     LR25DSF                            30
176100930129     C*
176200951228    2C  N30VI2LNP        IFGT      *ZEROS
176300070917     C                   SETOFF                                       1615
176400951228     C*
176500930129     C                   MOVEL     VI2LNP        COMLNP
176600951227     C* GIRO DATA ARRIVO
176700951227     C                   Z-ADD     VI2AED        G02DAT
176800951227     C                   Z-ADD     *ZEROS        G02INV
176900951227     C                   CALL      'XSRDA8'
177000951227     C                   PARM                    WLBDAT
177100951227     C                   Z-ADD     G02INV        WAED8
177200960304     C* L'AGGIORNAMENTO DEI FV LO ESEGUO PER TUTTI I FV DELLA LNA
177300960304     C*  DELLA L6
177400960304     C                   Z-ADD     1             Y                 3 0
177500960304   2AC     L6(Y)         DOWGT     0
177600960304     C                   MOVEL     L6(Y)         COMLAI
177700921120     C*
177800921120     C* CHAIN FOGLIO PARTENZE PER AGGIONARE DATA E NUM FOGLIO ABBINATO
177900941220     C* E IMPOSTO LINEE ARRIVO E SCARICO DEL FOGLIO
178000941221     C     KFVA1         CHAIN     FNFVA01L                           33
178100960304   2BC     *IN33         IFEQ      *OFF
178200951228     C*
178300960108     C* 16 ON  - E' STATA MODIFICATA LA DATA E ORA ARRIVO REALE OPPURE
178400960108     C*   SI TRATTA DI FOGLIO VIAGGIO CON NUMERO FOGLIO ARRIVI = 0
178500951228    3C     FVAAED        IFNE      WAED8
178600951228     C     FVAAEH        ORNE      VI2AEH
178700960108     C     FVANFA        OREQ      0
178800951228     C                   SETON                                        16
178900070917     c* Memorizzo se cambiata solo la data/ora
179000070918     c                   if        fvanfa>0
179100070918     c                   movel     fvaaed        woldaed
179200070918     c                   movel     fvaaeh        woldaeh
179300070917     C                   SETON                                        15
179400070917     c                   endif
179500951228    3C                   ENDIF
179600951228     C*
179700951228     C* SFLAGGO I FOGLI VIAGGIO SOLO SE HO MODIFICATO DATA/ORA ARRIVO
179800960108     C*   REALE OPPURE LA CAUSALE RITARDO/ANTICIPO OPPURE SE SI TRATTA
179900960108     C*   DI FOGLIO VIAGGIO CON NUMERO FOGLIO ARRIVI = 0
180000951228     C     *IN16         IFEQ      *ON
180100951228     C     FVARTA        ORNE      VI2RTC
180200960201     C* SE IL FOGLIO VIAGGIO E' ILLUMINATO NON LO SFLAGGO PERCHE'
180300960201     C*   NON E' DA TRASMETTERE
180400960201     C     FVAATR        IFEQ      ' '
180500951228     C                   CLEAR                   FVAFTR
180600951228     C                   CLEAR                   FVADTR
180700960201     C                   ENDIF
180800951228     C                   ENDIF
180900951228     C*
181000951227     C                   Z-ADD     WAED8         FVAAED
181100930127     C                   Z-ADD     VI2AEH        FVAAEH
181200951128     C                   MOVEL     VI2RTC        FVARTA
181300921120     C                   MOVEL     VIDNFV        FVANFA
181400930519     C                   MOVEL     VIDNPG        FVANPG
181500921120     C                   MOVEL     FVVDFV        FVADFA
181600951228     C*
181700941221     C                   UPDATE    FNFVA001
181800951115     C*
181900951228     C* I FOGLI VIAGGIO GIA' ABBINATI NON LI RIELABORO A MENO CHE NON
182000951228     C*   SIANO CAMBIATE LA DATA O L'ORA DI ARRIVO REALE
182100951228    3C     *IN16         IFEQ      *ON
182200951115     C*
182300950621     C** LEGGO TUTTE LE BOLLE DEL FOGLIO E CI METTO IL TOCCO BOLLA
182400970430     C                   CLEAR                   LNASAV
182500950621     C     KARB          SETLL     FNARB06L
182600960108     C     KARB          READE(N)  FNARB06L                               30
182700950621     C*
182800960108    4C     *IN30         DOWEQ     *OFF
182900970430     C     ARBLNA        IFNE      LNASAV
183000970430     C                   MOVE      ARBLNA        LNASAV
183100970430     C                   CLEAR                   DSLV53
183200970430     C                   MOVEL     'A'           D53TFO
183300970430     C                   MOVE      FVANFV        D53NFV
183400970502     C                   MOVE      FVALNP        D53FGS
183500970430     C                   MOVE      FVALAI        D53LAI
183600031215     C                   MOVEL     'B'           D53ABB
183700970430     C                   MOVE      ARBLNA        D53LNA
183800970430     C                   MOVE      ARBTFA        D53TFA
183900970502     C                   MOVE      SIMFEL        D53FEL
184000970430     C                   CALL      'FNLV53R'
184100970430     C                   PARM                    DSLV53
184200970430     C                   MOVE      '1'           WLV53             1
184300970430     C                   END
184400970825     C** SI TRTTA DI BOLLA RELATIVA AL FOGLIO ABBINATO--> ELAVORO
184500970430    5C     D53ERR        IFEQ      *BLANKS
184600070921     c     karb01        chain(E)  fnarb01l
184700070921     c                   if        not %error and %found(fnarb01l)
184800070921     c                   clear                   arbdti
184900070921     c                   clear                   arbhti
185000111118     c                   select
185100111118     c                   when      arbfbs=' '
185200070921     c                   eval      arbfbs='S'
185300111118
185400111118     c                   when      arbfbs='V' or arbfbs='P'
185500111118     c                   eval      arbfbs='E'
185600111118     c                   endsl
185700111118     c
185800070921     c                   update    fnarb000
185900070921     c                   else
186000960108     C                   MOVEL     ARBAAS        AGBAAS
186100960108     C                   MOVEL     ARBLNP        AGBLNP
186200960108     C                   MOVEL     ARBNRS        AGBNRS
186300960108     C                   MOVEL     ARBNSP        AGBNSP
186400960108     C                   WRITE     FNAGB000                             32
186500070921     c                   endif
186600070921     c
186700040315     c                   else
186800040315     c* se errore fnlv53 mi posiziono su nuova lna
186900040315     c     karb6         setgt     fnarb06l
187000951228    5C                   ENDIF
187100950621     C*
187200960108     C     KARB          READE(N)  FNARB06L                               30
187300951228    4C                   ENDDO
187400040120     c*
187500040120      * Carico tutti i secondi livello in arrivo del terminal di arrivo dell'utente
187600040120      * su stesso AS
187700040120     c                   Clear                   Trul09ds
187800040120     c                   Eval      I09Mod = 'TFU'
187900040120     c                   Eval      I09Tfa = fvalai
188000040120     c                   Eval      I09dta = fvadfv
188100040120     c                   Call      'TRUL09R'
188200040120     c                   Parm                    Trul09ds
188300040123     c* se lai non è terminal di nessuno metto comunque lai in mec
188400040123     c                   if        mec(1) = *zeros
188500040123     c                   eval      mec(1) = fvalai
188600040123     c                   endif
188700951227     C*
188800950621     C** LEGGO LE SPUNTE
188900040120     C                   Z-ADD     1             C                 3 0
189000040120     C     MEC(C)        dowNE     *ZEROS
189100960227     C*
189200970515     C                   CLEAR                   DSLV53
189300970515     C                   MOVEL     'A'           D53TFO
189400970515     C                   MOVE      FVANFV        D53NFV
189500970515     C                   MOVE      FVALNP        D53FGS
189600970515     C                   MOVE      FVALAI        D53LAI
189700070917     C                   MOVEL     'B'           D53ABB
189800970515     C                   MOVE      MEC(C)        D53LNA
189900970515     C                   MOVE      SIMFEL        D53FEL
190000970515     C                   CALL      'FNLV53R'
190100970515     C                   PARM                    DSLV53
190200970515     C                   MOVE      '1'           WLV53             1
190300970825     C**
190400970825     C* SI TRATTA DI LINEA RELATIVA AL FOGLIO ABBINATO--> ELABORO
190500970515    5C     D53ERR        IFEQ      *BLANKS
190600921120     C                   MOVEL     MEC(C)        COMLAI
190700070921     c                   clear                   savspe
190800070921     c                   clear                   wtragb
190900911010     C*
191000941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
191100000118     C                   Z-ADD     VI2FVA        WI2FVA
191200070123     C     KBRV1         SETLL     FNBRV02L
191300070123     C     KBRV1         READE     FNBRV02L                               30
191400911010     C*
191500951228    6C     *IN30         DOWEQ     '0'
191600921120     C*
191700941221     C     KBRV2         CHAIN     FNART27L                           31
191800951016     C*
191900951228    7C     *IN31         IFEQ      *OFF
192000070921     c* La prima volta salvo la chiave spedizione  per aggiornare quando
192100070921     c*  ha letto l'ultimo collo e non il primo
192200070921     c                   if        savspe=0
192300070921     c                   movel     artspe        savspe
192400070921     c                   endif
192500951016     C*
192600971002     C* IMPOSTO IL FALG DI ABBINATO SOLTANTO SE LA DATA SUL COLLO
192700971002     C* CORRISPONDE ALLA DATA FOGLIO VIAGGIO
192800971002     C     BRVATR        IFEQ      ' '
192900971002     C     ARTDFV        ANDEQ     FVADFV
193000971002     C     BRVATR        ORNE      ' '
193100971002     C     ARTDUT        ANDEQ     FVADFV
193200070921     c                   if        savspe=artspe
193300951016     C                   MOVEL     'S'           WTRAGB            1
193400070921     c                   endif
193500950328     C                   MOVEL     'S'           ARTABN                         'S'=SPUNTA ABBINATA
193600971119     C                   ENDIF
193700950111     C*
193800950111     C                   UPDATE    FNART000
193900921202     C*
194000070921    8C     ARTSPE        ifne      SAVSPE
194100070921     c                   EXSR      Aggspe
194200960110    8C                   ENDIF
194300951228    7C                   ENDIF
194400911010     C*
194500070123     C     KBRV1         READE     FNBRV02L                               30
194600951228    6C                   ENDDO
194700070921     c
194800070921     c* Elaboro l'ultima bolla
194900070921     c                   if        savspe>0
195000070921     c                   exsr      AggSpe
195100070921     c                   endif
195200070921     c
195300070917     c* Cerco anche altre bolla a cui ho impostato la data e ora modificata
195400070921     c*  ma che non sono partite come spunta su questo foglio
195500070917     c*  (ad esempio disguidi o colli part-non arrivati o colli arr-non part
195600070921    6c                   if        *in15
195700070918     c                   exSR      AGGIOBOL
195800070921    6C                   endif
195900070921    5C                   ENDIF
196000921120     C*
196100921120     C                   ADD       1             C
196200951228    4C                   ENDDO
196300951228     C*
196400951228    3C                   ENDIF
196500960304   2BC                   ENDIF
196600960304     C*
196700960304     C                   ADD       1             Y
196800960304   2AC                   ENDDO
196900960304    2C                   ENDIF
197000911010     C*
197100930129     C                   ADD       1             NRR
197200951228    1C                   ENDDO
197300911010     C**
197400911010     C** SE HO TOLTO DEI FOGLI VIAGGIO DEVO DISABILITARE
197500911010     C**
197600911010     C                   Z-ADD     1             I
197700920428     C                   Z-ADD     0             SAVSPE
197800950328     C                   MOVEL     'DT'          AGBAGB
197900960205     C                   CLEAR                   WALL
198000921120     C*
198100940620    1C     FV(I)         DOWNE     0
198200911010     C     FV(I)         LOOKUP    FVS                                    30
198300911010     C*
198400940620    2C  N30              DO
198500921119     C                   MOVEL     FV(I)         VI2KEY
198600960304     C                   Z-ADD     1             Y
198700960304     C* LEGGO TUTTA LA L6
198800960304   2AC     L6(Y)         DOWGT     0
198900960304     C                   MOVEL     L6(Y)         COMLAI
199000960304     C*
199100960304     C* CHAIN FOGLIO PARTENZE PER AGGIORNARE CAMPI
199200960304     C* E IMPOSTARE LINEE ARRIVO E SCARICO DEL FOGLIO
199300960304     C     KFVA1         CHAIN     FNFVA01L                           33
199400960304   2BC     *IN33         IFEQ      *OFF
199500921120     C*
199600960110     C** LEGGO TUTTE LE BOLLE DEL FOGLIO E TOLGO DATA E ORA ARRIVO
199700960110     C**   EFFETTIVA ULTIMO COLLO
199800970430     C                   CLEAR                   LNASAV
199900960110     C     KARB          SETLL     FNARB06L
200000960110     C     KARB          READE     FNARB06L                             3830
200100960110     C*
200200960110    3C     *IN30         DOWEQ     *OFF
200300960110     C     *IN38         ANDEQ     *OFF
200400970516     C*
200500970430     C     ARBLNA        IFNE      LNASAV
200600970430     C                   MOVE      ARBLNA        LNASAV
200700970430     C                   CLEAR                   DSLV53
200800970430     C                   MOVEL     'A'           D53TFO
200900970430     C                   MOVE      FVANFV        D53NFV
201000970502     C                   MOVE      FVALNP        D53FGS
201100970430     C                   MOVE      FVALAI        D53LAI
201200031215     C                   MOVEL     'B'           D53ABB
201300970430     C                   MOVE      ARBLNA        D53LNA
201400970430     C                   MOVE      ARBTFA        D53TFA
201500970502     C                   MOVE      SIMFEL        D53FEL
201600970430     C                   CALL      'FNLV53R'
201700970430     C                   PARM                    DSLV53
201800970430     C                   MOVE      '1'           WLV53             1
201900970430     C                   END
202000970825     C* SE E' BOLLA RELATIVA AL FOGLIO ABBINATO--> SBINO
202100970430    4C     D53ERR        IFEQ      *BLANKS
202200960110     C                   CLEAR                   ARBDTI
202300960110     C                   CLEAR                   ARBHTI
202400960111     C                   UPDATE    FNARB006
202500040315     c                   else
202600040315     c* se errore fnlv53 mi posiziono su nuova lna
202700040315     c     karb6         setgt     fnarb06l
202800960110    4C                   ENDIF
202900960110     C*
203000960110     C     KARB          READE     FNARB06L                             3830
203100960110    3C                   ENDDO
203200960110     C*
203300960110     C* 38 ON  - RECORD ALLOCATO
203400960110   3AC     *IN38         IFEQ      *ON
203500960110     C                   MOVEL     '1'           WALL              1
203600960110  X3AC                   ELSE
203700960110     C*
203800950328     C* SALVO LA DATA DI ARRIVO EFFETTIVO CHE C'ERA
203900950328     C                   MOVEL     FVAAED        SAVAED
204000950328     C*
204100921120     C                   MOVEL     *ZEROS        FVAAEH
204200921120     C                   MOVEL     *ZEROS        FVAAED
204300960304     C                   MOVEL     ' '           FVARTA
204400921120     C                   MOVEL     *ZEROS        FVANFA
204500921120     C                   MOVEL     *ZEROS        FVADFA
204600960304     C                   MOVEL     0             FVANPG
204700960202     C                   CLEAR                   FVARTA
204800960202     C*
204900941221     C  N33              UPDATE    FNFVA001
205000040120      * Carico tutti i secondi livello in arrivo del terminal di arrivo dell'utente
205100040120      * su stesso AS
205200040120     c                   Clear                   Trul09ds
205300040120     c                   Eval      I09Mod = 'TFU'
205400040120     c                   Eval      I09Tfa = fvalai
205500040120     c                   Eval      I09dta = fvadfv
205600040120     c                   Call      'TRUL09R'
205700040120     c                   Parm                    Trul09ds
205800040123     c* se lai non è terminal di nessuno metto comunque lai in mec
205900040123     c                   if        mec(1) = *zeros
206000040123     c                   eval      mec(1) = fvalai
206100040123     c                   endif
206200960202     C*
206300921120     C                   Z-ADD     1             C
206400040120     C     MEC(C)        downe     *ZEROS
206500970515     C*
206600970515     C                   CLEAR                   DSLV53
206700970515     C                   MOVEL     'A'           D53TFO
206800970515     C                   MOVE      FVANFV        D53NFV
206900970515     C                   MOVE      FVALNP        D53FGS
207000970515     C                   MOVE      FVALAI        D53LAI
207100070917     C                   MOVEL     'B'           D53ABB
207200970515     C                   MOVE      MEC(C)        D53LNA
207300970515     C                   MOVE      SIMFEL        D53FEL
207400970515     C                   CALL      'FNLV53R'
207500970515     C                   PARM                    DSLV53
207600970515     C                   MOVE      '1'           WLV53             1
207700960227     C*
207800970515    4C     D53ERR        IFEQ      *BLANKS
207900921120     C                   MOVEL     MEC(C)        COMLAI
208000921120     C*
208100941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
208200000118     C                   Z-ADD     VI2FVA        WI2FVA
208300070123     C     KBRV1         SETLL     FNBRV02L
208400070123     C     KBRV1         READE     FNBRV02L                               30
208500911010     C*
208600940620    5C     *IN30         DOWEQ     '0'
208700921120     C*
208800941221     C     KBRV2         CHAIN     FNART27L                           31
208900950328     C*
209000951228    6C     *IN31         IFEQ      *OFF
209100971002     C* AGGIORNO ARTABN E LA BOLLO SOLO SE LA DATA CORRISPONDE
209200971002   6AC     BRVATR        IFEQ      ' '
209300971002     C     ARTDFV        ANDEQ     FVADFV
209400971002     C     BRVATR        ORNE      ' '
209500971002     C     ARTDUT        ANDEQ     FVADFV
209600941221     C                   MOVEL     *BLANKS       ARTABN                         SPUNTA NON ABBINATA
209700950328     C*
209800950328     C                   UPDATE    FNART000
209900911011     C*
210000911014     C* C'E' DETTAGLIO COLLI
210100950328    7C     ARTSPE        IFNE      SAVSPE
210200941220     C     KART          CHAIN     FNARB01L                           32
210300911011     C*
210400911011     C* DISABILITO ALLA CONSEGNA
210500940620    8C     ARBAMA        IFNE      2
210600921126     C                   MOVEL     0             ARBAMP
210700940620    8C                   END
210800940620    8C     ARBAMA        IFEQ      1
210900921126     C                   MOVEL     0             ARBAMA
211000940620    8C                   END
211100940613     C* TOLGO DATA E ORA ARRIVO EFFETTIVA ULTIMO COLLO
211200940613     C                   CLEAR                   ARBDTI
211300940613     C                   CLEAR                   ARBHTI
211400941220     C                   UPDATE    FNARB000
211500920428     C                   MOVEL     ARTSPE        SAVSPE
211600940620    7C                   END
211700950407    6C                   ENDIF
211800971002     C                   ENDIF
211900911011     C*
212000070123     C     KBRV1         READE     FNBRV02L                               30
212100940620    5C                   END
212200940620    4C                   END
212300911010     C**
212400921120     C                   ADD       1             C
212500940620    3C                   END
212600960110     C*
212700960110     C* SE TUTTO OK TOLGO IL FOGLIO VIAGGIO DALLA SCHIERA IN MODO CHE
212800960110     C*   VI RIMANGANO SOLO QUELLI CHE NON SONO RIUSCITA AD ELABORARE
212900960110     C*   PERCHE' HO TROVATO I RECORD DI ALCUNE BOLLE ALLOCATI
213000960110     C                   CLEAR                   FV(I)
213100960110   3AC                   ENDIF
213200960304   2BC                   END
213300960304     C**
213400960304     C                   ADD       1             Y
213500960304   2AC                   ENDDO
213600960304    2C                   ENDDO
213700911010     C*
213800911010     C                   ADD       1             I
213900960304    1C                   ENDDO
214000950328     C**
214100950328     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
214200960205    0C     WALL          IFEQ      ' '
214300151022     C**                 MOVEL     'A'           P05TBO
214400151022     C**                 CLEAR                   P05AGB
214500151022     C**                 MOVEL     PARA05        KPJBU
214600950328     C*
214700971218     C* FORZATURA DELLA CODA DIFFE PERCHE' QEUSTI LAVORI DEVONO
214800971218     C*  ANDARE IN SUCCESSIONE E LR27 DEVE ESSERE ELABORATO IN UNA
214900971218     C*  UNICA CODA
215000151022     C**                 MOVEL(P)  'KSDIFFE'     KQIEB
215100971218     C*
215200151022     C**                 MOVEL     'LV52'        KCOAZ
215300151022     C**                 CALL      'BCH10'
215400151022     C*+                 PARM                    KPJBA
215500971218     C* VIENE SOTTOMESSA L'ABILITAZIONE BOLLE PERCHE' DEVE GIRARE PRIMA
215600971218     C*  DELLE STAMPE: ELENCO SPEDIZIONI NON PARTITE,PERCHE' SERVONO
215700971218     C*  LE BOLLE ABILITATE
215800971218     C*  TUTTO QUESTO ANCHE SE L'ABILITAZIONE BOLLE VIENE ANCHE SOTTOM.
215900971218     C*  DALL'LV52
216000151022     C**                 MOVEL     'LR27'        KCOAZ
216100151022     C**                 CALL      'BCH10'
216200151022     C**                 PARM                    KPJBA
216300950926     C**
216400921119     C**
216500921119     C* LANCIO SE RICHIESTA ELENCO SPEDIZ NO ABILITATE
216600921119     C* PRIMA PER I FOGLI DIRETTI
216700921119     C* POI PER I FOGLI CON SCALO AL TRANSITO E AL TERMINAL
216800960205     C                   MOVEL     ' '           COMATS
216900940620    1C                   DO        2
217000920330     C                   Z-ADD     1             I
217100920330     C*
217200940620    2C     FVS(I)        DOWNE     0
217300940620     C*
217400940620    3C     STS(I)        IFEQ      'S'
217500921119     C     ATS(I)        ANDEQ     COMATS
217600921109     C*
217700921109     C                   Z-ADD     1             C
217800921119     C                   MOVEL     FVS(I)        VI2KEY
217900980303     C                   MOVEL     VI2LAI        COMLAI
218000941221     C                   MOVEL     COMLNP        P52LNP
218100941221     C                   MOVE      VI2FVA        P52FVA
218200040122      * Carico tutti i secondi livello in arrivo del terminal di arrivo
218300040122      * su stesso AS
218400040122     c                   Clear                   Trul09ds
218500150116     c****               Clear                   mecn
218600040122     c                   Eval      I09Mod = 'TFU'
218700040122     c                   Eval      I09dta = dfv(I)
218800040122     c                   move      vi2lai        i09tfa
218900040122     c                   Call      'TRUL09R'
219000040122     c                   Parm                    Trul09ds
219100040123     c* se lai non è terminal di nessuno metto in mecn metto lai
219200040123     c                   if        mec(1) = *zeros
219300150116     c                   eval      mec(1) = i09tfa
219400150116     c**                 else
219500040123     c* ora carico schiera dei p.o. di cui ter-arr non gestiti da altri p.o.
219600150116     c*****              exsr      CarMecn
219700150116     c***                if        mecn(1) = *zeros
219800150116     c***                eval      mecn(1) = i09tfa
219900150116     c***                endif
220000040123     c                   endif
220100921119     C*
220200150116     C     MEC(C)        downe     *ZEROS
220300970515     C*
220400970515     C* PER SAPERE SE ELABORARE RICHIAMO PGM FNLV53R
220500970515     C                   CLEAR                   DSLV53
220600031215     C                   MOVEL     'A'           D53TFO
220700970515     C                   MOVE      VI2FVA        D53NFV
220800970515     C                   MOVE      COMLNP        D53FGS
220900970515     C                   MOVE      COMLAI        D53LAI
221000031215     C                   MOVEL     'B'           D53ABB
221100150116     C                   MOVE      MEC(C)        D53LNA
221200970515     C                   MOVE      SIMFEL        D53FEL
221300970515     C                   CALL      'FNLV53R'
221400970515     C                   PARM                    DSLV53
221500970515     C                   MOVE      '1'           WLV53             1
221600970515     C*
221700970515    5C     D53ERR        IFEQ      *BLANKS
221800940620     C*
221900950215     C                   MOVEL     '1'           P52OUT                         OUTQ DA AZORG00F
222000150116     c**se mecn(c) è p.o. gestito anzichè passare mecn(c) passo il suo
222100150116     c**gestore
222200150116     c***                z-add     1             ii                3 0
222300150116     c***  mecn(C)       lookup    skpo(II)                               30
222400150116     c** 30              eval      p52fel = skpot(ii)
222500150116     c* N.B.: Viene richiamato lr52 anche per le filiali in £6 (es. 102): Non dovrebbe farlo
222600150116     c*       ma visto ch è sempre stato così e dal momento che adesso la stampa non viene
222700150116     c*       quasi mai lanciata e serve solo per i SI DDT ci disinteressiamo
222800150116     C                   eval      p52fel = mec(c)
222900960518     C                   MOVEL     99991231      P52DAT
223000030826     C                   MOVEL(p)  PARA52        KPJBU
223100941216     C                   MOVEL     'LR52'        KCOAZ
223200920330     C                   CALL      'BCH10'
223300920330     C                   PARM                    KPJBA
223400940620    5C                   END
223500921119     C*
223600921109     C                   ADD       1             C
223700940620    4C                   END
223800921109     C*
223900940620    3C                   END
224000921109     C*
224100920330     C                   ADD       1             I
224200940620    2C                   END
224300921119     C*
224400921119     C                   MOVEL     'T'           COMATS            1
224500940620    1C                   END
224600960205    0C                   END
224700911010     C*
224800971218     C                   CLEAR                   KQIEB
224900911008     C                   ENDSR
225000070921     c* Aggiorno a cambio spedizione FNARB per l'abilit.bolla -------*
225100070921     c     AGGSpe        BEGSR
225200070921     c* Aggiorno direttamente la bolla se riesco, altrimenti
225300070921     c*  scrivo AGB se allocata
225400070921    1c                   if        WTRAGB='S'
225500070921     c     ksav          chain(E)  fnarb01l
225600070921    2c                   if        not %error and %found(fnarb01l)
225700070921     c                   clear                   arbdti
225800070921     c                   clear                   arbhti
225900111118     c                   select
226000111118     c                   when      arbfbs=' '
226100111118     c                   eval      arbfbs='S'
226200111118
226300111118     c                   when      arbfbs='V' or arbfbs='P'
226400111118     c                   eval      arbfbs='E'
226500111118     c                   endsl
226600070921     c                   update    fnarb000
226700070921   x2c                   else
226800070921     C                   MOVEL     WsaAAS        AGBAAS
226900070921     C                   MOVEL     WsaLNP        AGBLNP
227000070921     C                   MOVEL     WsaNRS        AGBNRS
227100070921     C                   MOVEL     WsaNSP        AGBNSP
227200070921     C                   WRITE     FNAGB000                             32
227300070921    2c                   endif
227400070921    1c                   endif
227500110103     c*
227600070921     C                   CLEAR                   WTRAGB
227700070921     C                   MOVEL     ARTSPE        SAVSPE
227800110103     c*
227900070921     c* Se per l'ultima spunta di nuova spedizione ho aggiornato
228000070921     c*  fnart, lo memorizzo
228100070921    1c                   if        artabn='S'
228200070921     C                   MOVEL     'S'           WTRAGB            1
228300070921    1c                   endif
228400070921     c                   ENDSR
228500070918     C*
228600070918     C*--- Cerco le bolle da aggiornare con la stessa data ora arrivo *
228700070918     C     Aggiobol      BEGSR
228800110103      *
228900070918      *?Imposto la parte fissa dell'istruzione sql
229000070918     c                   eval      wrkgetlista =
229100070918     c                             'select arbaas, arblnp, arbnrs, arbnsp +
229200070919     c                             from fnarb00f where arbdti ='+
229300070918     c                             %editc(woldaed:'X') + ' and +
229400070918     c                             arbhti ='+ %editc(woldaeh:'X') +
229500070919     c                             ' and arblna=' + %editc(comlai:'X')
229600070918      *?Solo lettura
229700070918     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
229800070918
229900070918     C/EXEC SQL
230000070918     C+ PREPARE s1 FROM :wrkgetlista
230100070918     C/END-EXEC
230200070918     C
230300070918     C/EXEC SQL
230400070918     C+ DECLARE A1 CURSOR FOR S1
230500070918     C/END-EXEC
230600070918     C
230700070918     C/EXEC SQL
230800070918     C+ OPEN a1
230900070918     C/END-EXEC
231000070918
231100070918      *?Leggo il file
231200070918     c                   do        *hival
231300070918     C/EXEC SQL
231400070918     C+ FETCH NEXT FROM a1 INTO : arbaas, :arblnp, :arbnrs, :arbnsp
231500070918     C/END-EXEC
231600070918
231700070919     c                   if        sqlcod = 100 or sqlcod<0
231800070918     c                   leave
231900070918     c                   else
232000070921     c     karb01        chain(E)  fnarb01l
232100070918     c                   if        not %error and  %found(fnarb01l)
232200070918     c                   clear                   arbdti
232300070918     c                   clear                   arbhti
232400111118     c*****              movel     'S'           arbfbs
232500111118     c                   select
232600111118     c                   when      arbfbs=' '
232700111118     c                   eval      arbfbs='S'
232800111118
232900111118     c                   when      arbfbs='V' or arbfbs='P'
233000111118     c                   eval      arbfbs='E'
233100111118     c                   endsl
233200070918     c                   update    fnarb000
233300070918     c                   endif
233400070918     c
233500070918     c                   ENDif
233600070918     c                   enddo
233700070918     C/EXEC SQL
233800070918     C+ CLOSE a1
233900070918     C/END-EXEC
234000070918     c                   ENDSR
234100960202     C*
234200951115     C*--- SR INIZIALE -----------------------------------------------*
234300951115     C     *INZSR        BEGSR
234400951115     C*
234500951115     C****  KLIST  ****
234600960304     C     KCAP          KLIST
234700970721     C                   KFLD                    KVER
234800970721     C                   KFLD                    KNAR
234900960304     C                   KFLD                    KLNA
235000951115     C     KARB          KLIST
235100951115     C                   KFLD                    COMLNP
235200951115     C                   KFLD                    VI2FVA
235300040315     C     KARB6         KLIST
235400040315     C                   KFLD                    COMLNP
235500040315     C                   KFLD                    VI2FVA
235600040315     C                   KFLD                    arblna
235700951115     C     KFVV1         KLIST
235800951115     C                   KFLD                    PARNPG
235900951115     C                   KFLD                    COMNFV            5 0
236000951115     C                   KFLD                    VIDFGS
236100951115     C     KFVA1         KLIST
236200951115     C                   KFLD                    COMLNP
236300951115     C                   KFLD                    VI2FVA
236400951115     C                   KFLD                    COMLAI            3 0
236500951115     C     KFVA2         KLIST                                                  FNFVA02L
236600951115     C                   KFLD                    PARNPG                         ..CATEGORIA
236700951115     C                   KFLD                    COMNFV                         ..FOGLIO
236800980421     C                   KFLD                    L6(XX)
236900951128     C     KFVA4         KLIST                                                  FNFVA02L
237000951128     C                   KFLD                    FVATRN
237100951128     C                   KFLD                    FVADTN
237200951115     C     KBRV1         KLIST                                                  FNBRV02L
237300951115     C                   KFLD                    KNPG                           ..CATEGORIA
237400000118     C                   KFLD                    WI2FVA                         ..NR.FOGLIO
237500951115     C                   KFLD                    COMLNP                         ..FIL.GESTIONE
237600951115     C                   KFLD                    COMLAI                         ..LNA
237700951115     C     KBRV2         KLIST                                                  FNART27L
237800951115     C                   KFLD                    BRVLNP                         ..LNP
237900951115     C                   KFLD                    BRVLNA                         ..LNA
238000951115     C                   KFLD                    BRVNRS                         ..SERIE
238100040120     C                   KFLD                    BRVNSC                         ..SEGNACOLLO
238200951115     C     KART          KLIST
238300951115     C                   KFLD                    ARTAAS
238400951115     C                   KFLD                    ARTLNP
238500951115     C                   KFLD                    ARTNRS
238600951115     C                   KFLD                    ARTNSP
238700070921     C     KSAV          KLIST
238800070921     C                   KFLD                    WsaAAS
238900070921     C                   KFLD                    WsaLNP
239000070921     C                   KFLD                    WsaNRS
239100070921     C                   KFLD                    WsaNSP
239200070919     C     KARB01        KLIST
239300070918     C                   KFLD                    ARbAAS
239400070918     C                   KFLD                    ARbLNP
239500070918     C                   KFLD                    ARbNRS
239600070918     C                   KFLD                    ARbNSP
239700951115     C     KTAB          KLIST
239800951115     C                   KFLD                    CODUT
239900951115     C                   KFLD                    COD
240000951115     C                   KFLD                    KEY
240100951115     C     KFV4          KLIST
240200951228     C                   KFLD                    FVALNP
240300951115     C                   KFLD                    FVANFV
240400951115     C                   KFLD                    WEPA
240500951228     C                   KFLD                    FVALAI
240600951115     C                   KFLD                    WTRC
240700000628      * ESTENSIONE FV
240800000628     C     KFWA1         KLIST
240900000628     C                   KFLD                    FVALNP
241000000628     C                   KFLD                    FVANFV
241100000628     C                   KFLD                    FVALAI
241200951115     C*
241300951115     C****  DEFINIZIONE CAMPI  ****
241400970721     C     *LIKE         DEFINE    CPCVER        O96VER
241500970721     C     *LIKE         DEFINE    CPCVER        KVER
241600970721     C     *LIKE         DEFINE    FVVDFV        I96DRI
241700970721     C     *LIKE         DEFINE    §1XHHO        HHOUNO
241800960304     C     *LIKE         DEFINE    §1XHHO        HHODUE
241900960304     C     *LIKE         DEFINE    D98DTI        DATSOG
242000960304     C     *LIKE         DEFINE    D98HTI        ORASOG
242100970721     C     *LIKE         DEFINE    CPCNAR        KNAR
242200970721     C     *LIKE         DEFINE    CPCLNA        KLNA
242300960304     C     *LIKE         DEFINE    BRVNPG        KNPG
242400951115     C     *LIKE         DEFINE    FVAAED        SAVAED
242500951228     C     *LIKE         DEFINE    FVAAED        WAED8
242600951116     C     *LIKE         DEFINE    FV4TRC        WTRC
242700951115     C     *LIKE         DEFINE    TBLCOD        COD
242800951115     C     *LIKE         DEFINE    TBLKEY        KEY
242900970430     C     *LIKE         DEFINE    ARBLNA        LNASAV
243000970825     C     *LIKE         DEFINE    FV4EPA        WEPA
243100000118     C     *LIKE         DEFINE    BRVNFV        WI2FVA
243200951115     C*
243300970825     C                   MOVEL     'A'           WEPA                           *ARRIVO
243400960202     C                   MOVEL     'R'           WTRC
243500951115     C                   ENDSR
243600040122     C*--- CARICAMENTO SCHIERA MECN ----------------------------------*
243700150116     C**   CarMecn       BEGSR
243800150116     c**                 z-add     0             k                 2 0
243900150116     c**                 Do        30            x
244000150116     c**                 If        Mec(x) = *Zeros
244100150116     c**                 Leave
244200150116     c**                 EndIf
244300150116     c**   Mec(x)        Lookup    Skpot                                  30
244400150116     c**                 If        *In30
244500150116     c**                 Add       1             k
244600150116     c**                 Eval      Mecn(k) = Mec(x)
244700150116     c**                 Iter
244800150116     c**                 EndIf
244900150116     c**   Mec(x)        Lookup    Skpo                                   30
245000150116     c**                 If        Not *In30
245100150116     c**                 Add       1             k
245200150116     c***                Eval      Mecn(k) = Mec(x)
245300150116     c***                Iter
245400150116     c***                EndIf
245500150116     c***                EndDo
245600150116     c***                endsr
245700110103
245800110103      *---------------------------------------------------------------*
245900110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
246000110103      *---------------------------------------------------------------*
246100110103     c     sr_DatiJob    BEGSR
246200110103      *
246300110103     c     *dtaara       define    §azute        AZUTEds
246400110103     c     *dtaara       define    §datiute      dDatiUte
246500110103      *
246600110103     c                   in(E)     *dtaara
246700110103     c                   IF        %Error or RSUT = *blanks
246800110103     c                   clear                   TIBS34ds
246900110103     c                   call      'TIBS34R'
247000110103     c                   parm                    TIBS34ds
247100110103     c                   in        *dtaara
247200110103     c                   ENDIF
247300110103      *
247400110103     c                   ENDSR
247500951114     C*---------------------------------------------------------------*
247600000606**  MSG
247700951115Immettere causale valida                                                      1
247800951115Presente ritardo/anticipo nell'arrivo del foglio: immettere la causale        2
247900951121Non c'e' ritardo/anticipo: se si vuole si può eliminare la causale            3
248000951121FOGLIO VIAGGIO GIA' ABBINATO CON FOGLIO ARRIVI N.RO  X - XXXXX                4
248100951115Se immessa causale "G" occorre inserire descrizione ritardo/anticipo          5
248200960201Presente descrizione ritardo: immettere la causale o eliminare la descrizione 6
248300960227Fogli xxx-xxxxx e xxx-xxxxx del traino xxxxxxx hanno data/ora/causale diverse 7
248400960201Presente descrizione ritardo: premere F6 per conferma o eliminare descrizione 8
248500960201Eliminata descrizione ritardo: confermare con F6 o reinserire la descrizione  9
248600960227xxx-xxxxx deve avere stesso arrivo(xx/xx xx:xx x)del F.xxx-xxxxx: ENTER x forz10
248700960109Non tutte le bolle sono state aggiornate: premere nuovamente F6 !!            11
248800960202Impossibile uscire perchè non tutte le bolle sono state aggiornate: premere F612
248900960227Data/ora arrivo non puo' essere superiore alla data/ora corrente: ENTER x forz13
249000960626Impossibile modificare o eliminare la causale. Unica causale ammessa: X       14
249100960626Impossibile immettere la causale                                              15
249200960626Impossibile modificare la descrizione                                         16
249300960626Impossibile modificare la causale: eliminarla oppure lasciarla = X            17
249400971002Impossibile visualizzare il foglio: provieni dalla visualizzazione!!          18
249500030924Impossibile posticipare l'ora di arrivo del foglio illuminato oltre le xx:xx  19
249600000606Data arrivo non puo' essere superiore alla data foglio arrivi     ENTER x forz20
