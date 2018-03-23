000100970526     H*------------------------------------------------------------------------*
000200991008     H* Gestione codici unificanti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800011205     Ftis7k7d   cf   e             WORKSTN INFDS(DEVDS1)
000900011205     F                                     SFILE(i7k7DMS:MSGULT)
001000000306     Fazorg01l  IF   E           K DISK
001100021127     Fcnaco00f  IF   E           K DISK
001200000807     Ftntbe01l  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D* MESSAGGI
001700970526     D*------------
001800000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001900970526     D*------------
002000970526     D* DESCRIZIONI TASTI FUNZIONALI
002100970526     D*------------
002200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* FUNZIONI DA CARICARE A VIDEO
002500970526     D*------------
002600970526     D FUV             S             19    DIM(24)
002700970526     D*------------------------------------------------------------------------*
002800970526     D* INPUT
002900970526     D*------------------------------------------------------------------------*
003000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003100970526     D*------------
003200970526     D FUDDS           DS
003300970526     D  FUDDE1                 1      1
003400970526     D  FUDFUN                 2      3
003500970526     D  FUDDE2                 4     19
003600970526     D  FUDFI0                20     34
003700970526     D  FUDATT                35     35
003800970526     D  FUDFI1                36     80
003900970526     D  FUDDES                 1     19
004000970526     D  FUDRIG                 1     80
004100970526     D*------------
004200970526     D* DS GESTIONE TASTI FUNZIONALI
004300970526     D*------------
004400970526     D DSKEY         E DS
004500970526     D DEVDS1          DS
004600970526     D  KEY                  369    369
004700970526     D*------------
004800970526     D* DS "XMSG" - GESTIONE MESSAGGI
004900970526     D*------------
005000970526     D DSMSG         E DS
005100970528     D*------------------
005200970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005300970528     D*------------------
005400970528     D WLBDA8          DS                  INZ
005500970528     D  G08DAT                 1      8  0
005600970528     D  G08INV                 9     16  0
005700970528     D  G08ERR                17     17
005800970528     D  G08TGI                18     22  0
005900991222     D*------------
006000020903     D* DS REPERIMENTO DATI UTENTE
006100991222     D*------------
006200011205     D TIBS02DS      E DS
006300020903     D TIBS34DS      E DS                                                       *Profili utente
006400020903     D DDATIUTE      E DS                                                       *Dati utente
006500020903     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006600020903     D TRUL06DS      E DS                                                       *caricamento £x
006700020903     D  LIA                    1     90  0 DIM(30)                               -filiali
006800020903     D TRUL26DS      E DS                                                       *caricamento P.O.
006900020903     D  SKPO                  11    760  0 dim(250)
007000020903     D  SKPOT                761   1510  0 dim(250)
007100991214     D*------------
007200991214     D* DS DI PROCEDURA
007300991214     D*------------
007400991216     D DTLE          E DS
007500970526     D*------------
007600970526     D* ARCHITETTURA
007700970526     D*------------
007800970526     D KPJBA         E DS
007900991214     D*------------------
008000991214     D* DS X PASSAGGIO PARAMETRI
008100991214     D*------------------
008200991214     D PARAM           DS                  INZ
008300011205     D  PARDDE                 1      8  0
008400011205     D  PARTLE                 9     10
008500011205     D  PARDLE                11     40
008600011205     D  PARFIL                41     43  0
008700011205     D  PARDFI                44     73
008800011205     D  PARFIT                74     74
008900011205     D  PAREDD                75     82  0
009000011206     D  PARFIC                83     83  0
009100011206     D  PARFIR                84    104
009200021127     D  PARKUN               105    111  0
009300011205     D*------------------
009400011205     D* VARIABILI NON RIFERITE AL DATABASE
009500011205     D*------------------
009600011205     D tipprf          s              3    inz(*blanks)
009700000810     D*-------------------
009800000810     D* FILIALI IN GESTIONE
009900000810     D*-------------------
010000020903     D L1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
010100021127     D*-------------------
010200021127     D* DS REPERIMENTO PADRE/FIGLI
010300021127     D*-------------------
010400021127     D TIBS10DS      E DS
010500970526     C*------------------------------------------------------------------------*
010600000303     C* MAIN LINES
010700970526     C*------------------------------------------------------------------------*
010800970526     C*
010900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011000991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011300970526     C*
011400970526     C* EMETTE FORMATO VIDEO
011500970526     C                   EXSR      BUTFMR
011600970526     C*
011700970526     C* GESTIONE TASTI FUNZIONALI
011800970526     C                   EXSR      FUNCON
011900970526E   2C                   ENDDO
012000970526     C*
012100970526     C* FINE PGM
012200970526     C                   SETON                                        LR
012300970526     C*------------------------------------------------------------------------*
012400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012500970526     C*------------------------------------------------------------------------*
012600970526     C     BUTFMR        BEGSR
012700970526     C*
012800970526     C* EMETTE LA TESTATA
012900011205     C                   WRITE     i7k7D01
013000970526     C*
013100970526     C* SPEDISCE EVENTUALE MESSAGGIO
013200970526     C                   EXSR      SNDMSG
013300970526     C*
013400970526     C* EMETTE LE FUNZIONI
013500011205     C                   WRITE     i7k7D02
013600970526     C*
013700970526     C* EMETTE IL VIDEO
013800011205     C                   EXFMT     i7k7D03
013900970526     C*
014000991214     C* REIMPOSTA FLAG DI ERRORE
014100991214     C                   MOVEL     '0'           ERRL00
014200970526     C*
014300970526     C* REIMPOSTA INDICATORI DI ERRORE
014400991214     C*
014500011205     C                   SETOFF                                       535459
014600991214     C*
014700970526     C*
014800970526     C                   ENDSR
014900970526     C*------------------------------------------------------------------------*
015000970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015100970526     C*------------------------------------------------------------------------*
015200970526     C     SNDMSG        BEGSR
015300970526     C*
015400970526     C* PULISCE IL SFILE MESSAGGI
015500970526     C                   MOVEL     '1'           *IN40
015600011205     C                   WRITE     i7k7DMC
015700970526     C                   MOVEL     '0'           *IN40
015800970526     C*
015900970526     C* VISUALIZZA IL MESSAGGIO
016000970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016100970526     C                   CALL      'XMSG'        XMSGPA
016200970526IF  1C     DSMSER        IFEQ      *ZEROS
016300970526     C                   MOVEL     DSMSKE        MSGKEY
016400970526E   1C                   ENDIF
016500970526     C                   Z-ADD     1             MSGULT
016600011205     C                   WRITE     i7k7DMS
016700011205     C                   WRITE     i7k7DMC
016800970526     C*
016900970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017000970526     C                   MOVEL     *BLANKS       DSMSMS
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* ENTGES - GESTIONE ENTER -> INVIO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     ENTGES        BEGSR
017700970526     C*
017800970526     C* CONTROLLA I CAMPI IMMESSI
017900970526     C                   EXSR      DECODI
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* F03GES - GESTIONE F03 -> FINE
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F03GES        BEGSR
018600970526     C*
018700970526     C* IMPOSTA "PREMUTO F03"
018800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018900970526     C*
019000970526     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200991214     C* F06GES - GESTIONE F06 -> CONFERMA
019300970526     C*------------------------------------------------------------------------*
019400970526     C     F06GES        BEGSR
019500970526     C*
019600991214     C* CONTROLLA I CAMPI IMMESSI
019700991214     C                   EXSR      DECODI
019800991214     C     ERRL00        IFEQ      '0'
019900991214     C* ESEGUE ROUTINE DI CONFERMA
020000991214     C                   EXSR      ESEGUI
020100991214     C                   ENDIF
020200970526     C*
020300991214     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F12GES - GESTIONE F12 -> RITORNO
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F12GES        BEGSR
020800970526     C*
020900970526     C* IMPOSTA "PREMUTO F12"
021000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F24GES        BEGSR
021700970526     C*
021800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021900970526     C*
022000970526     C                   ENDSR
022100991214     C*------------------------------------------------------------------------*
022200991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
022300991214     C*------------------------------------------------------------------------*
022400991214     C     PASPAR        BEGSR
022500991214     C*
022600011206     C                   Z-ADD     DDEVID        PARDDE
022700991214     C                   MOVEL     VIDTLE        PARTLE
022800000306     C                   MOVEL     VDDTLE        PARDLE
022900011205     C                   MOVEL     VIDFIL        PARFIL
023000000306     C                   MOVEL     VDDFIL        PARDFI
023100020903     C                   MOVEL     DUTLPO        PARFIT
023200011206     C                   Z-ADD     VIDDDE        PAREDD
023300020903     C                   Z-ADD     1             PARFIC
023400020916     C                   MOVEL     RSUT          PARFIR
023500021127     C                   Z-ADD     VIDKUN        PARKUN
023600021127     C* SE INDICATO 1 PARTICOLARE CLIENTE UNIFICANTE E NN INDICATO 1 P.O. SPECIFICO
023700021127     C* VALORIZZO IL P.O. CON QUELLO DEL CLIENTE UNIFICANTE RICHIESTO
023800021127     C                   IF        PARFIL = *ZEROS
023900021127     C                   MOVEL(P)  PARKUN        PARFIL
024000021127     C                   ENDIF
024100991214     C*
024200991214     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400011205     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
024500970526     C*------------------------------------------------------------------------*
024600991214     C     ESEGUI        BEGSR
024700991214     C*
024800991214     C*
024900991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
025000011205     C                   MOVEL     'S7K8'        KCOAZ
025100991214     C                   EXSR      PASPAR
025200991214     C                   MOVEL     PARAM         KPJBU
025300020208     C                   CALL      'BCH10'
025400020208 MOM C*                  CALL      'TIS7K8R'
025500991214     C                   PARM                    KPJBA
025600991214     C*
025700970526     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026000970526     C*------------------------------------------------------------------------*
026100970526     C     DECODI        BEGSR
026200000306     C*
026300011205     C                   SETOFF                                       535459
026400000306     C*
026500000306     C*----------------------
026600000306     C* CONTROLLO LA FILIALE
026700000306     C*----------------------
026800000306     C                   MOVEL     *BLANKS       VDDFIL
026900000306     C     ERRL00        IFEQ      '0'
027000000306     C     VIDFIL        IFNE      *BLANKS
027100000829     C     VIDFIL        ANDNE     *ZEROS
027200000306     C                   MOVE      VIDFIL        WLIN
027300000306     C                   EXSR      CHKLIN
027400000306     C     ERRL00        IFEQ      '0'
027500000306     C                   MOVEL     WLIN          VIDFIL
027600000306     C                   MOVEL     WDLIN         VDDFIL
027700000306     C                   ELSE
027800000306     C                   SETON                                        59
027900000306     C                   ENDIF
028000000306     C                   ENDIF
028100000306     C                   ENDIF
028200991214     C*--------------------
028300000830     C* TIPO LEGAME
028400991214     C*--------------------
028500021127     C                   MOVE      *BLANKS       WTLECU
028600021127     C                   MOVE      *BLANKS       WTLECE
028700991008IF  1C     ERRL00        IFEQ      '0'
028800021127IF  2C     VIDtle        IFNE      *BLANKS
028900021127     C                   EXSR      CNTTLE
029000991008X   2C                   ELSE
029100991008     C                   MOVEL     '1'           ERRL00
029200991214     C                   SETON                                        53
029300991011     C                   MOVEL     MSG(12)       DSMSMS
029400991008E   2C                   ENDIF
029500991008E   1C                   ENDIF
029600000829     C*                  ENDIF
029700991015     C*--------------------
029800011205     C* DATA RIFERIMENTO
029900991015     C*--------------------
030000991015     C     *LIKE         DEFINE    VIDDDE        DDEVID
030100991015IF  1C     ERRL00        IFEQ      '0'
030200011206     C                   Z-ADD     VIDDDE        WDA8
030300011206     C                   Z-ADD     *ZEROS        DA8W
030400991015     C                   EXSR      CHKDA8
030500991220     C                   Z-ADD     WDA8          VIDDDE
030600011206     C                   Z-ADD     DA8W          DDEVID
030700991015IF  2C     ERRL00        IFEQ      '1'
030800991214     C                   SETON                                        54
030900021127     C                   MOVEL     MSG(14)       DSMSMS
031000991015E   2C                   ENDIF
031100991015E   1C                   ENDIF
031200021127     C*--------------------
031300021127     C* CLIENTE UNIFICANTE
031400021127     C*--------------------
031500021127     C     ERRL00        IFNE      '1'
031600021127     C                   IF        VIDKUN > *ZEROS
031700021127     C                   CLEAR                   TIBS10DS
031800021127     C                   EVAL      D10DRF = DDEVID                              *data riferimento
031900021127     C                   EVAL      D10TLE = VIDTLE                              *tipo legame
032000021127     C                   EVAL      D10PAF = 'F'                                 *cerca se padre
032100021127     C                   EVAL      D10COD = VIDKUN
032200021127     C                   CALL      'TIBS10R'
032300021127     C                   PARM                    TIBS10DS
032400021127     C                   IF        D10ERR = *BLANKS                             *Figlio
032500021127     C* REPERISCO LA RAGIONE SOCIALE
032600021127     C                   Z-ADD     1             ACOkut
032700021127     C                   Z-ADD     DUTKCI        ACOkcc
032800021127     C                   Z-ADD     VIDKUN        ACOksc
032900021127     C     KEYaco        CHAIN     cnaco00f
033000021127     C                   IF        %found(cnaco00f)
033100021127     C                   MOVEL     ACOrag        VDDKUN
033200021127     C                   ELSE
033300021127     C                   MOVEL     '1'           ERRL00
033400021127     C                   SETON                                        55
033500021127     C                   MOVEL     MSG(3)        DSMSMS
033600021127     C                   MOVEL     *BLANKS       VDDKUN
033700021127     C                   ENDIF
033800021127     C                   ELSE
033900021127     C                   MOVEL     '1'           ERRL00
034000021127     C                   SETON                                        55
034100021127     C                   MOVEL     MSG(2)        DSMSMS
034200021127     C                   ENDIF
034300021127     C                   ENDIF
034400021127     C                   ENDIF
034500021127     C*
034600970528     C                   ENDSR
034700000810     C*------------------------------------------------------------------------*
034800000306     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
034900000306     C*------------------------------------------------------------------------*
035000000306     C     CHKLIN        BEGSR
035100000306     C*
035200000306     C* CONTROLLO SE E' STATO INSERITO '?'
035300000306     C     '?'           SCAN      WLIN          A                 1 0    99
035400000306     C*
035500000306     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
035600000306IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
035700000306     C                   MOVEL     *BLANKS       §COD1             3
035800000306     C                   MOVE      *BLANKS       §DES1
035900000306     C                   MOVE      *BLANKS       §PRA
036000000306     C                   CALL      'TNSD24R'
036100000306     C                   PARM                    §COD1
036200000306     C                   PARM                    §TIP              1
036300000306     C                   PARM                    §DES1            25
036400000306     C                   PARM                    §PRA              3
036500000306     C                   MOVEL     §COD1         WLIN              3
036600000306     C                   MOVEL     §DES1         WDLIN            20
036700000306     C*
036800000306X   1C                   ENDIF
036900000306     C*
037000000306     C* CARATTERI ALFABETICI NON VALIDI
037100000306IF  2C     WLIN          IFLT      *ZEROS
037200000306     C                   MOVEL     '1'           ERRL00
037300000306     C                   MOVEL     MSG(13)       DSMSMS
037400000306     C                   MOVEL     *ALL'*'       WDLIN
037500000306X   2C                   ELSE
037600000306     C*
037700000306     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
037800000306     C                   MOVEL     WLIN          CODFIL            3 0
037900000306     C     CODFIL        CHAIN     AZORG01L                           99
038000000306IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
038100000306     C                   MOVEL     ORGDES        WDLIN
038200000306X   3C                   ELSE                                                   <-- 2 -->
038300000306     C                   MOVEL     '1'           ERRL00
038400000306     C                   MOVEL     MSG(13)       DSMSMS
038500000306     C                   MOVEL     *ALL'*'       WDLIN
038600000306E   3C                   END                                                    <-- 2 ---
038700000306IF  3C     ORGFAG        IFEQ      'V'                                          --- 2 -->
038800000306     C                   MOVEL     '1'           ERRL00
038900000306     C                   MOVEL     MSG(13)       DSMSMS
039000000306     C                   MOVEL     *ALL'*'       WDLIN
039100000306E   3C                   ENDIF                                                  <-- 2 ---
039200000306E   2C                   ENDIF                                                  <-- 2 ---
039300000306     C*
039400000306     C     ERRL00        IFNE      '1'
039500000810     C                   SETOFF                                           28
039600020910     C     CODFIL        LOOKUP    L1                                     28
039700020903     C     DUTLPO        IFNE      'S'
039800000810     C     *IN28         ANDEQ     *OFF
039900000306     C                   MOVEL     '1'           ERRL00
040000000306     C                   MOVEL     MSG(9)        DSMSMS
040100000306     C                   MOVEL     *ALL'*'       WDLIN
040200000306     C                   ENDIF                                                  <-- 2 ---
040300000810     C                   ENDIF                                                  <-- 2 ---
040400000306     C*
040500000306     C                   ENDSR
040600991008     C*------------------------------------------------------------------------*
040700000810     C* Cnttle - CONTROLLA codice tipo legame
040800991008     C*------------------------------------------------------------------------*
040900991214     C     cnttle        begsr
041000991214     C                   clear                   TIBS02DS
041100991214     C*
041200991008     C     '?'           SCAN      Vidtle                                 30
041300991214     D*  Ricerca
041400991214     C                   eval      t02tla = 'L'
041500991008    1C                   IF        *IN30 = *ON
041600991008     c                   movel     'R'           t02mod
041700991008     c                   else
041800991008     c                   movel     'C'           t02mod
041900991008     c                   movel(p)  vidtle        t02ke1
042000991008     c                   end
042100991008     C                   movel     'TLE'         t02cod
042200991008     c                   movel     knsif         t02sif
042300991008     C                   CALL      'TIBS02R'
042400991008     c                   parm                    KPJBA
042500991008     c                   parm                    TIBS02DS
042600991008     c
042700000829     C*
042800991008    2C                   IF        T02err = *blanks
042900000829     C                   movel     T02ke1        Vidtle
043000000829     C                   movel     T02uni        dtle
043100000807     C                   exsr      chkaut
043200011206     C                   if        errl00 = '0'
043300991014     C                   movel     §tledesc      Vddtle
043400991014     C                   movel     §tlecu        Wtlecu            1
043500991014     C                   movel     §tlece        wtlece            1
043600000807     C                   endif
043700991014     c                   else
043800991014     c                   movel     '1'           errl00
043900991014     c                   seton                                        66
044000991014     C                   MOVEL     MSG(12)       DSMSMS
044100991008     C                   ENDIF
044200991008     c
044300991008     c                   endsr
044400000807     C*------------------------------------------------------------------------*
044500000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
044600000807     C*------------------------------------------------------------------------*
044700000807     C     CHKAUT        BEGSR
044800000807     C*
044900000807     C* - controllo su "operabilità" di TIPO LEGAME -
045000000807     C                   eval      tipprf =  %subst(%trim(KNMUS):1:3)
045100020903     C     DUTLPO        ifeq      'S'
045200000807     C* X SEDE
045300000807     C     ERRL00        ifeq      '0'
045400000807     C                   movel     'TLE'         KTBcod
045500000807     C                   movel(P)  vidtle        KTBky1
045600000807     C     KEYTBE        chain     tntbe01l                           70
045700000807     C     *in70         ifeq      *off
045800000807     C     tbeatb        andeq     *blanks
045900000807     C                   movel     tbeuni        dtle
046000000807     C                   if        §tleaus = 'S' or
046100000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
046200000807     C                   else
046300000810     C                   SETON                                        53
046400000807     C                   MOVEL     '1'           ERRL00
046500000807     C                   MOVEL     MSG(22)       DSMSMS
046600000807     C                   endif
046700000807     C                   else
046800000807     C                   MOVEL     '1'           ERRL00
046900000807     C                   MOVEL     MSG(12)       DSMSMS
047000000807     C                   endif
047100000807     C                   endif
047200000807     C                   else
047300000807     C* X FILIALE
047400000807     C     ERRL00        ifeq      '0'
047500000807     C                   movel     'TLE'         KTBcod
047600000807     C                   movel(P)  vidtle        KTBky1
047700000807     C     KEYTBE        chain     tntbe01l                           70
047800000807     C     *in70         ifeq      *off
047900000807     C     tbeatb        andeq     *blanks
048000000807     C                   movel     tbeuni        dtle
048100000807     C                   if        §tleaut = 'S' or
048200000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
048300000807     C                   else
048400000810     C                   SETON                                        53
048500000807     C                   MOVEL     '1'           ERRL00
048600000807     C                   MOVEL     MSG(22)       DSMSMS
048700000807     C                   endif
048800000807     C                   else
048900000807     C                   MOVEL     '1'           ERRL00
049000000807     C                   MOVEL     MSG(12)       DSMSMS
049100000807     C                   endif
049200000807     C                   endif
049300000807     C                   endif
049400000807     C*
049500000807     C                   ENDSR
049600970528     C*------------------------------------------------------------------------*
049700970528     C* CHKDA8 - CONTROLLA LA DATA (8)
049800970528     C*------------------------------------------------------------------------*
049900970528     C     CHKDA8        BEGSR
050000970528     C*
050100970528     C     *LIKE         DEFINE    VIDDDE        WDA8
050200970528     C     *LIKE         DEFINE    VIDDDE        DA8W
050300970528     C*
050400970528     C                   Z-ADD     WDA8          G08DAT
050500970528     C                   Z-ADD     *ZEROS        G08INV
050600970528     C                   MOVEL     '0'           G08ERR
050700970528     C                   CALL      'XSRDA8'
050800970528     C                   PARM                    WLBDA8
050900970528     C                   Z-ADD     G08DAT        WDA8
051000970528     C                   Z-ADD     G08INV        DA8W
051100970528IF  1C     G08ERR        IFEQ      '1'
051200970528     C                   MOVEL     '1'           ERRL00
051300970528E   1C                   ENDIF
051400970528     C*
051500970528     C                   ENDSR
051600970526     C*------------------------------------------------------------------------*
051700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
051800970526     C*------------------------------------------------------------------------*
051900970526     C     AZZVID        BEGSR
052000970526     C* CAMPI
052100991214     C                   MOVEL     *BLANKS       VIDTLE
052200970528     C                   Z-ADD     *ZEROS        VIDDDE
052300970526     C* DECODIFICHE
052400991214     C                   MOVEL     *BLANKS       VDDTLE
052500000829     C                   MOVEL     *BLANKS       VDDFIL
052600970526     C*
052700970526     C                   ENDSR
052800000303     C*------------------------------------------------------------------------*
052900000905     C* IMPVID - IMPOSTA I DEFAULT A VIDEO
053000000303     C*------------------------------------------------------------------------*
053100000303     C     IMPVID        BEGSR
053200000303     C*
053300011205     C                   Z-ADD     G08DAT        VIDDDE
053400011205     C                   MOVEL     'ST'          VIDTLE
053500011205     C                   MOVEL     *BLANKS       VIDFIL
053600000303     C*
053700000303     C                   ENDSR
053800970526     C*------------------------------------------------------------------------*
053900970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
054000970526     C*------------------------------------------------------------------------*
054100970526     C     IMPFUN        BEGSR
054200970526     C*
054300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
054400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
054500970526     C                   EXSR      FUNGES
054600970526     C*
054700970526     C                   ENDSR
054800000810     C*------------------------------------------------------------------------*
054900000810     C* CARTAB - CARICA LE TABELLE OCCORRENTI
055000000810     C*------------------------------------------------------------------------*
055100000810     C     CARTAB        BEGSR
055200020903     C*
055300020903     C* REPERISCE I DATI UTENTE
055400020903     C                   EXSR      REPDATIUTE
055500020903     C*
055600020903     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
055700020903     C                   CLEAR                   L1
055800020903     C                   CLEAR                   TRUL06DS
055900020903     C                   CLEAR                   LIA
056000020903     C                   CLEAR                   TRUL26DS
056100020903     C                   CLEAR                   SKPO
056200020903     C                   CLEAR                   SKPOT
056300020903     C*
056400020903     C* CARICA LE FILIALI IN GESTIONE
056500020903     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
056600020903     C                   EVAL      DEPKPJBU = KPJBU
056700020903     C                   MOVEL     'L'           D06TLA
056800020903     C                   MOVEL     '£1'          D06COD
056900021007     C                   MOVEL     duttfp        D06KEY
057000020903     C                   MOVEL     TRUL06DS      KPJBU
057100020903     C                   CALL      'TRUL06R'
057200020903     C                   PARM                    KPJBA
057300020903     C                   MOVEL     KPJBU         TRUL06DS
057400020903     C                   MOVEA     LIA           L1                              -P.O. terminal
057500020903     C                   EVAL      KPJBU = DEPKPJBU
057600020903     C                   ENDIF
057700020903     C*
057800020903     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
057900020906     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
058000020903     C                   ENDIF
058100020903     C*
058200020903     C                   IF        DUTLPO = 'S'                                 *SEDE
058300020903     C                   EVAL      I26TLA = 'L'
058400020903     C                   EVAL      I26TPT = 'P'
058500020903     C                   CALL      'TRUL26R'
058600020903     C                   PARM                    TRUL26DS
058700020903     C                   MOVEA     SKPO          L1                              -tutti i P.O.
058800020903     C                   ENDIF
058900000810     C*
059000000810     C                   ENDSR
059100020903     C*--------------------------------------------------------------------------------------------*
059200020903     C* REPERISCE I DATI UTENTE
059300020903     C*--------------------------------------------------------------------------------------------*
059400020903     C     REPDATIUTE    BEGSR
059500020903     C*
059600020903     C* INIZIALIZZA VARIABILI DI WRK
059700020903     C                   CLEAR                   TIBS34DS
059800020903     C                   CLEAR                   AZUTEDS
059900020903     C                   CLEAR                   DDATIUTE
060000020903     C*
060100020903     C     *DTAARA       DEFINE    §azute        azuteds
060200020903     C     *DTAARA       DEFINE    §datiute      ddatiute
060300020903     C                   IN(E)     *DTAARA
060400020903if  1C                   IF        %Error
060500020903     c                   EVAL      I34Tla = 'L'
060600020903     C                   CALL      'TIBS34R'
060700020903     C                   PARM                    Tibs34Ds
060800020903     C                   IN        *DTAARA
060900020903e   1C                   ENDIF
061000020903     C*
061100020903     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
061200020916     C                   MOVEL(P)  RSUT          DSFIRS           20
061300020903     C*
061400020903     C                   ENDSR
061500970526     C*------------------------------------------------------------------------*
061600970526     C* *INZSR - OPERAZIONI INIZIALI
061700970526     C*------------------------------------------------------------------------*
061800970526     C     *INZSR        BEGSR
061900991217     C*
062000991217     C     *ENTRY        PLIST
062100991217     C                   PARM                    KPJBA
062200011205     C*
062300000807     C* LETTURA tntbe01l -
062400000807     C     KEYtbe        KLIST
062500000807     C                   KFLD                    Ktbcod                         *CODICE TABELLA
062600000807     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
062700021127     C*
062800021127     C* LETTURA cnaco00f -
062900021127     C     KEYaco        KLIST
063000021127     C                   KFLD                    ACOkut
063100021127     C                   KFLD                    ACOkcc
063200021127     C                   KFLD                    ACOksc
063300000807     C*---
063400000807     C* VARIABILI RIFERITE AL DATA BASE
063500000807     C*---
063600000807     C     *LIKE         DEFINE    tbecod        KTBcod
063700000807     C     *LIKE         DEFINE    tbeke1        KTBky1
063800020903     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
063900970526     C*
064000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
064100970526     C     XMSGPA        PLIST
064200970526     C                   PARM                    DSMSMS
064300970526     C                   PARM                    DSMSPG
064400970526     C                   PARM                    DSMSKE
064500970526     C                   PARM                    DSMSER
064600970526     C*
064700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
064800970526     C                   CALL      'XKEY'
064900970526     C                   PARM                    DSKEY
065000970526     C*---
065100970526     C* VARIABILI NON RIFERITE AL DATA BASE
065200970526     C*---
065300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
065400991214     C                   MOVEL     *BLANKS       DEPOP1            3
065500970526     C*---
065600970526     C* VARIABILI DI CONTROLLO
065700970526     C*---
065800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
065900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
066000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
066100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
066200970526     C*
066300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
066400011205     C                   MOVEL     'TIS7K7R'     DSMSPG
066500011205     C                   MOVEL     'TIS7K7R'     MSGQUE
066600970526     C                   MOVEL     *BLANKS       MSGKEY
066700970526     C                   MOVEL     *BLANKS       DSMSMS
066800970528     C*
066900970528     C* CALCOLA LA DATA CORRENTE
067000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
067100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
067200970528     C                   Z-ADD     WN8           G08DAT
067300970528     C                   Z-ADD     *ZEROS        G08INV
067400970528     C                   MOVEL     '0'           G08ERR
067500970528     C                   CALL      'XSRDA8'
067600970528     C                   PARM                    WLBDA8
067700970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
067800000810     C*
067900000810     C* CARICA LE TABELLE OCCORRENTI
068000000810     C                   EXSR      CARTAB
068100000310     C*
068200000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
068300000310     C                   EXSR      AZZVID
068400000310     C                   EXSR      IMPVID
068500970526     C*
068600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
068700970526     C                   EXSR      IMPFUN
068800970526     C*
068900970526     C                   ENDSR
069000970526     C*------------------------------------------------------------------------*
069100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
069200970526     C*------------------------------------------------------------------------*
069300970526     C     FUNGES        BEGSR
069400970526     C*
069500970526     C* PULISCO CAMPI DI LAVORO
069600970526     C                   Z-ADD     0             W                 5 0
069700970526     C*
069800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
069900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
070000970526     C     1             DO        24            I
070100970526     C                   MOVEL     FUD(I)        FUDRIG
070200970526     C*
070300970526     C* PULIZIA FUNZIONI PRECEDENTI
070400970526     C                   MOVEL     *BLANKS       FUV(I)
070500970526     C*
070600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
070700970526     C* (SALTA F24)
070800970526     C     FUDATT        IFEQ      'S'
070900970526     C     FUDFUN        ANDNE     '24'
071000970526     C                   ADD       1             W
071100970526     C                   MOVEL     FUDDES        FUV(W)
071200970526     C                   END
071300970526     C                   END                                                    DO WHILE
071400970526     C*
071500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
071600970526     C* ALTRIMENTI DISATTIVO F24
071700970526     C     1             DO        24            I
071800970526     C                   MOVEL     FUD(I)        FUDRIG
071900970526     C*
072000970526     C     FUDFUN        IFEQ      '24'
072100970526     C     W             IFGT      6
072200970526     C                   MOVEL     'S'           FUDATT
072300970526     C                   ELSE
072400970526     C                   MOVEL     *BLANKS       FUDATT
072500970526     C                   END                                                    W GT 8
072600970526     C                   MOVEL     FUDRIG        FUD(I)
072700970526     C                   END
072800970526     C                   END                                                    DO WHILE
072900970526     C*
073000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
073100970526     C     W             DIV       6             I
073200970526     C                   MVR                     Z
073300970526     C*
073400970526     C     Z             IFNE      0
073500970526     C     I             ADD       1             Z
073600970526     C                   ELSE
073700970526     C                   Z-ADD     I             Z                 5 0
073800970526     C                   END
073900970526     C*
074000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
074100970526     C                   ADD       1             FUNGRU
074200970526     C     FUNGRU        IFGT      Z
074300970526     C     FUNGRU        ORLT      1
074400970526     C                   Z-ADD     1             FUNGRU
074500970526     C                   END
074600970526     C*
074700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
074800970526     C     FUNGRU        IFEQ      1
074900970526     C                   MOVEL     FUV(1)        FUN001
075000970526     C                   MOVEL     FUV(2)        FUN002
075100970526     C                   MOVEL     FUV(3)        FUN003
075200970526     C                   MOVEL     FUV(4)        FUN004
075300970526     C                   MOVEL     FUV(5)        FUN005
075400970526     C                   MOVEL     FUV(6)        FUN006
075500970526     C                   END
075600970526     C*
075700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
075800970526     C     FUNGRU        IFEQ      2
075900970526     C                   MOVEL     FUV(7)        FUN001
076000970526     C                   MOVEL     FUV(8)        FUN002
076100970526     C                   MOVEL     FUV(9)        FUN003
076200970526     C                   MOVEL     FUV(10)       FUN004
076300970526     C                   MOVEL     FUV(11)       FUN005
076400970526     C                   MOVEL     FUV(12)       FUN006
076500970526     C                   END
076600970526     C*
076700970526     C* TERZO GRUPPO DI 6 FUNZIONI
076800970526     C     FUNGRU        IFEQ      3
076900970526     C                   MOVEL     FUV(13)       FUN001
077000970526     C                   MOVEL     FUV(14)       FUN002
077100970526     C                   MOVEL     FUV(15)       FUN003
077200970526     C                   MOVEL     FUV(16)       FUN004
077300970526     C                   MOVEL     FUV(17)       FUN005
077400970526     C                   MOVEL     FUV(18)       FUN006
077500970526     C                   END
077600970526     C*
077700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
077800970526     C     FUNGRU        IFEQ      4
077900970526     C                   MOVEL     FUV(19)       FUN001
078000970526     C                   MOVEL     FUV(20)       FUN002
078100970526     C                   MOVEL     FUV(21)       FUN003
078200970526     C                   MOVEL     FUV(22)       FUN004
078300970526     C                   MOVEL     *BLANKS       FUN005
078400970526     C                   MOVEL     *BLANKS       FUN006
078500970526     C                   END
078600970526     C*
078700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
078800970526     C                   MOVEL     *BLANKS       FUN007
078900970526     C                   MOVEL     *BLANKS       FUN008
079000970526     C*
079100970526     C     1             DO        24            I
079200970526     C                   MOVEL     FUD(I)        FUDRIG
079300970526     C     FUDFUN        IFEQ      '24'
079400970526     C     FUDATT        ANDEQ     'S'
079500970526     C                   MOVEL     FUDDES        FUN008
079600970526     C                   END
079700970526     C                   END                                                    DO
079800970526     C*
079900970526     C                   ENDSR
080000970526     C*------------------------------------------------------------------------*
080100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
080200970526     C*------------------------------------------------------------------------*
080300970526     C     FUNCON        BEGSR
080400970526     C*
080500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
080600970526     C                   EXSR      FUNVAR
080700970526     C*
080800970526     C* PULISCO CAMPI DI LAVORO
080900970526     C                   Z-ADD     0             I                 5 0
081000970526     C                   Z-ADD     0             W                 5 0
081100970526     C*
081200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
081300970526     C     FUNKEY        IFNE      *BLANKS
081400970526     C     1             DO        24            I
081500970526     C                   MOVEL     FUD(I)        FUDRIG
081600970526     C*
081700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
081800970526     C     FUNKEY        IFEQ      FUDFUN
081900970526     C     FUDATT        ANDEQ     'S'
082000970526     C                   Z-ADD     I             W
082100970526     C                   END
082200970526     C                   END                                                    DO WHILE
082300970526     C*
082400970526     C* FUNZIONE NON ATTIVA (ERRORE)
082500970526     C     W             IFEQ      0
082600970526     C*
082700970526     C* EMISSIONE MESSAGGIO DI ERRORE
082800970526     C                   MOVEL     MSG(1)        DSMSMS
082900970526     C*
083000970526     C                   END                                                    W=0
083100970526     C                   END                                                    FUNKEY NE *BLANKS
083200970526     C*
083300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
083400970526     C     W             IFNE      0
083500970526     C     FUNKEY        OREQ      *BLANKS
083600970526     C     FUNKEY        IFEQ      *BLANKS
083700970526     C                   EXSR      ENTGES
083800970526     C                   END
083900970526     C     FUNKEY        IFEQ      '03'
084000970526     C                   EXSR      F03GES
084100970526     C                   END
084200970526     C     FUNKEY        IFEQ      '06'
084300970526     C                   EXSR      F06GES
084400970526     C                   END
084500970526     C     FUNKEY        IFEQ      '12'
084600970526     C                   EXSR      F12GES
084700970526     C                   END
084800970526     C     FUNKEY        IFEQ      '24'
084900970526     C                   EXSR      F24GES
085000970526     C                   END
085100970526     C                   END
085200970526     C*
085300970526     C                   ENDSR
085400970526     C*------------------------------------------------------------------------*
085500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
085600970526     C*------------------------------------------------------------------------*
085700970526     C     FUNVAR        BEGSR
085800970526     C*
085900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
086000970526     C*
086100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
086200970526     C     KEY           IFEQ      ENTER
086300970526     C                   MOVEL     *BLANKS       FUNKEY
086400970526     C                   END
086500970526     C     KEY           IFEQ      F1
086600970526     C                   MOVEL     '01'          FUNKEY
086700970526     C                   END
086800970526     C     KEY           IFEQ      F2
086900970526     C                   MOVEL     '02'          FUNKEY
087000970526     C                   END
087100970526     C     KEY           IFEQ      F3
087200970526     C                   MOVEL     '03'          FUNKEY
087300970526     C                   END
087400970526     C     KEY           IFEQ      F4
087500970526     C                   MOVEL     '04'          FUNKEY
087600970526     C                   END
087700970526     C     KEY           IFEQ      F5
087800970526     C                   MOVEL     '05'          FUNKEY
087900970526     C                   END
088000970526     C     KEY           IFEQ      F6
088100970526     C                   MOVEL     '06'          FUNKEY
088200970526     C                   END
088300970526     C     KEY           IFEQ      F7
088400970526     C                   MOVEL     '07'          FUNKEY
088500970526     C                   END
088600970526     C     KEY           IFEQ      F8
088700970526     C                   MOVEL     '08'          FUNKEY
088800970526     C                   END
088900970526     C     KEY           IFEQ      F9
089000970526     C                   MOVEL     '09'          FUNKEY
089100970526     C                   END
089200970526     C     KEY           IFEQ      F10
089300970526     C                   MOVEL     '10'          FUNKEY
089400970526     C                   END
089500970526     C     KEY           IFEQ      F11
089600970526     C                   MOVEL     '11'          FUNKEY
089700970526     C                   END
089800970526     C     KEY           IFEQ      F12
089900970526     C                   MOVEL     '12'          FUNKEY
090000970526     C                   END
090100970526     C     KEY           IFEQ      F13
090200970526     C                   MOVEL     '13'          FUNKEY
090300970526     C                   END
090400970526     C     KEY           IFEQ      F14
090500970526     C                   MOVEL     '14'          FUNKEY
090600970526     C                   END
090700970526     C     KEY           IFEQ      F15
090800970526     C                   MOVEL     '15'          FUNKEY
090900970526     C                   END
091000970526     C     KEY           IFEQ      F16
091100970526     C                   MOVEL     '16'          FUNKEY
091200970526     C                   END
091300970526     C     KEY           IFEQ      F17
091400970526     C                   MOVEL     '17'          FUNKEY
091500970526     C                   END
091600970526     C     KEY           IFEQ      F18
091700970526     C                   MOVEL     '18'          FUNKEY
091800970526     C                   END
091900970526     C     KEY           IFEQ      F19
092000970526     C                   MOVEL     '19'          FUNKEY
092100970526     C                   END
092200970526     C     KEY           IFEQ      F20
092300970526     C                   MOVEL     '20'          FUNKEY
092400970526     C                   END
092500970526     C     KEY           IFEQ      F21
092600970526     C                   MOVEL     '21'          FUNKEY
092700970526     C                   END
092800970526     C     KEY           IFEQ      F22
092900970526     C                   MOVEL     '22'          FUNKEY
093000970526     C                   END
093100970526     C     KEY           IFEQ      F23
093200970526     C                   MOVEL     '23'          FUNKEY
093300970526     C                   END
093400970526     C     KEY           IFEQ      F24
093500970526     C                   MOVEL     '24'          FUNKEY
093600970526     C                   END
093700970526     C     KEY           IFEQ      ROLLUP
093800970526     C                   MOVEL     'RU'          FUNKEY
093900970526     C                   END
094000970526     C     KEY           IFEQ      HELP
094100970526     C                   MOVEL     'HP'          FUNKEY
094200970526     C                   END
094300970526     C                   ENDSR
094400970526     C*------------------------------------------------------------------------*
094500970526     C* NOTE COSTRUZIONE SCHIERE
094600970526     C*------------------------------------------------------------------------*
094700970526     C*
094800970526     C* -MSG-MESSAGGI
094900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
095000970526     C* (NON TOCCARE MSG,1 E MSG,2)
095100970526     C*
095200970526     C* -CMD-COMANDI
095300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
095400970526     C*
095500970526     C* -OPD-OPZIONI
095600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
095700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
095800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
095900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
096000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
096100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
096200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
096300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
096400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
096500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
096600970526     C*
096700970526     C* -FUD-FUNZIONI
096800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
096900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
097000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
097100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
097200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
097300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
097400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
097500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
097600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
097700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
097800970526     O*------------------------------------------------------------------------*
097900970526** MSG - MESSAGGI
098000011205TIS7K7R- Tasto funzionale non gestito                                          1
098100021127TIS7K7R- Il cliente selezionato non risulta essere unificante.                 2
098200021127TIS7K7R- Cliente errato o inesistente.                                         3
098300011205TIS7K7R-                                                                       4
098400011205TIS7K7R-                                                                       5
098500011205TIS7K7R-                                                                       6
098600011205TIS7K7R- Cliente incompatibile per Filiale selezionata.                        7
098700011205TIS7K7R- Cliente non autorizzato.                                              8
098800011205TIS7K7R- Punto operativo non autorizzato.                                      9
098900011205TIS7K7R- Anno inserito non accettabile.                                        10
099000011205TIS7K7R- Mese inserito non valido.                                             11
099100011205TIS7K7R- Tipo legame errato verificarne sulla tabella l'esistenza              12
099200011205TIS7K7R- Punto operativo errato.                                               13
099300011205TIS7K7R- Data errata                                                           14
099400011205TIS7K7R- La data scadenza deve essere maggiore della decorrenza                15
099500011205TIS7K7R- Cliente inserito inesistente.                                         16
099600011205TIS7K7R- Cliente inserito sospeso.                                             17
099700011205TIS7K7R- Tipo lancio obbligatorio.                                             18
099800011205TIS7K7R- Cliente inserito è senza P.IVA o Cod.Fisc.                            19
099900011205TIS7K7R- Il codice figlio non può essere uguale al codice padre.               20
100000011205TIS7K7R- Scelta obbligatoria.                                                  21
100100011205TIS7K7R- Tipo legame non autorizzato.                                          22
100200011205TIS7K7R- Filiale non abilitata alla gestione dei clienti scelti.               23
100300011205TIS7K7R-                                                                       24
100400970526** FUD - FUNZIONI
100500970526                   -        (S/N)=N                        funz.01¬
100600970526                   -        (S/N)=N                        funz.02¬
100700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
100800970526                   -        (S/N)=N                        funz.04¬
100900970526                   -        (S/N)=N                        funz.05¬
101000991214F06=Conferma       -        (S/N)=S                        funz.06¬
101100970527                   -        (S/N)=N                        funz.07¬
101200970527                   -        (S/N)=N                        funz.08¬
101300970527                   -        (S/N)=N                        funz.09¬
101400970527                   -        (S/N)=N                        funz.10¬
101500970526                   -        (S/N)=N                        funz.11¬
101600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
101700970526                   -        (S/N)=N                        funz.13¬
101800970526                   -        (S/N)=N                        funz.14¬
101900970526                   -        (S/N)=N                        funz.15¬
102000970526                   -        (S/N)=N                        funz.16¬
102100970526                   -        (S/N)=N                        funz.17¬
102200970526                   -        (S/N)=N                        funz.18¬
102300970526                   -        (S/N)=N                        funz.19¬
102400970526                   -        (S/N)=N                        funz.20¬
102500970526                   -        (S/N)=N                        funz.21¬
102600970526                   -        (S/N)=N                        funz.22¬
102700970526                   -        (S/N)=N                        funz.23¬
102800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
