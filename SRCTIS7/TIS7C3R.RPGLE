000100970526     H*------------------------------------------------------------------------*
000200991008     H* Gestione codici unificanti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800020208     Ftis7c3d   cf   e             WORKSTN INFDS(DEVDS1)
000900020208     F                                     SFILE(i7c3DMS:MSGULT)
001000000306     Fazorg01l  IF   E           K DISK
001100000807     Ftntbe01l  IF   E           K DISK
001200000810     Ftabel00f  IF   E           K DISK
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
006300991214     D*------------
006400991214     D* DS DI PROCEDURA
006500991214     D*------------
006600991216     D DTLE          E DS
006700970526     D*------------
006800970526     D* ARCHITETTURA
006900970526     D*------------
007000970526     D KPJBA         E DS
007100991214     D*------------------
007200991214     D* DS X PASSAGGIO PARAMETRI
007300991214     D*------------------
007400991214     D PARAM           DS                  INZ
007500011205     D  PARDDE                 1      8  0
007600011205     D  PARTLE                 9     10
007700011205     D  PARDLE                11     40
007800011205     D  PARFIL                41     43  0
007900011205     D  PARDFI                44     73
008000011205     D  PARFIT                74     74
008100011205     D  PAREDD                75     82  0
008200011206     D  PARFIC                83     83  0
008300011206     D  PARFIR                84    104
008400020208     D  PARANO               105    105
008500011205     D*------------------
008600011205     D* VARIABILI NON RIFERITE AL DATABASE
008700011205     D*------------------
008800011205     D tipprf          s              3    inz(*blanks)
008900000810     D*-------------------
009000000810     D* FILIALI IN GESTIONE
009100000810     D*-------------------
009200020903     D L1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
009300020903     D*-------------
009400020903     D* DS DI WRK
009500020903     D*-------------
009600020903     D TIBS34DS      E DS                                                       *Profili utente
009700020903     D DDATIUTE      E DS                                                       *Dati utente
009800020903     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009900020903     D TRUL06DS      E DS                                                       *caricamento £x
010000020903     D  LIA                    1     90  0 DIM(30)                               -filiali
010100020903     D TRUL26DS      E DS                                                       *caricamento P.O.
010200020903     D  SKPO                  11    760  0 dim(250)
010300020903     D  SKPOT                761   1510  0 dim(250)
010400970526     C*------------------------------------------------------------------------*
010500000303     C* MAIN LINES
010600970526     C*------------------------------------------------------------------------*
010700970526     C*
010800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010900991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011200970526     C*
011300970526     C* EMETTE FORMATO VIDEO
011400970526     C                   EXSR      BUTFMR
011500970526     C*
011600970526     C* GESTIONE TASTI FUNZIONALI
011700970526     C                   EXSR      FUNCON
011800970526E   2C                   ENDDO
011900970526     C*
012000970526     C* FINE PGM
012100970526     C                   SETON                                        LR
012200970526     C*------------------------------------------------------------------------*
012300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012400970526     C*------------------------------------------------------------------------*
012500970526     C     BUTFMR        BEGSR
012600970526     C*
012700970526     C* EMETTE LA TESTATA
012800020208     C                   WRITE     i7c3D01
012900970526     C*
013000970526     C* SPEDISCE EVENTUALE MESSAGGIO
013100970526     C                   EXSR      SNDMSG
013200970526     C*
013300970526     C* EMETTE LE FUNZIONI
013400020208     C                   WRITE     i7c3D02
013500970526     C*
013600970526     C* EMETTE IL VIDEO
013700020208     C                   EXFMT     i7c3D03
013800970526     C*
013900991214     C* REIMPOSTA FLAG DI ERRORE
014000991214     C                   MOVEL     '0'           ERRL00
014100970526     C*
014200970526     C* REIMPOSTA INDICATORI DI ERRORE
014300991214     C*
014400011205     C                   SETOFF                                       535459
014500991214     C*
014600970526     C*
014700970526     C                   ENDSR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015000970526     C*------------------------------------------------------------------------*
015100970526     C     SNDMSG        BEGSR
015200970526     C*
015300970526     C* PULISCE IL SFILE MESSAGGI
015400970526     C                   MOVEL     '1'           *IN40
015500020208     C                   WRITE     i7c3DMC
015600970526     C                   MOVEL     '0'           *IN40
015700970526     C*
015800970526     C* VISUALIZZA IL MESSAGGIO
015900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016000970526     C                   CALL      'XMSG'        XMSGPA
016100970526IF  1C     DSMSER        IFEQ      *ZEROS
016200970526     C                   MOVEL     DSMSKE        MSGKEY
016300970526E   1C                   ENDIF
016400970526     C                   Z-ADD     1             MSGULT
016500020208     C                   WRITE     i7c3DMS
016600020208     C                   WRITE     i7c3DMC
016700970526     C*
016800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016900970526     C                   MOVEL     *BLANKS       DSMSMS
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* ENTGES - GESTIONE ENTER -> INVIO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     ENTGES        BEGSR
017600970526     C*
017700970526     C* CONTROLLA I CAMPI IMMESSI
017800970526     C                   EXSR      DECODI
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F03GES - GESTIONE F03 -> FINE
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F03GES        BEGSR
018500970526     C*
018600970526     C* IMPOSTA "PREMUTO F03"
018700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018800970526     C*
018900970526     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100991214     C* F06GES - GESTIONE F06 -> CONFERMA
019200970526     C*------------------------------------------------------------------------*
019300970526     C     F06GES        BEGSR
019400970526     C*
019500991214     C* CONTROLLA I CAMPI IMMESSI
019600991214     C                   EXSR      DECODI
019700991214     C     ERRL00        IFEQ      '0'
019800991214     C* ESEGUE ROUTINE DI CONFERMA
019900991214     C                   EXSR      ESEGUI
020000020315     C                   MOVEL     '1'           FINL00
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
023300020208     C                   MOVEL     VIDANO        PARANO
023400020903     C                   Z-ADD     1             PARFIC
023500020916     C                   MOVEL     RSUT          PARFIR
023600991214     C*
023700991214     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900011205     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
024000970526     C*------------------------------------------------------------------------*
024100991214     C     ESEGUI        BEGSR
024200991214     C*
024300991214     C*
024400991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
024500020208     C                   MOVEL     'S7C4'        KCOAZ
024600991214     C                   EXSR      PASPAR
024700991214     C                   MOVEL     PARAM         KPJBU
024800020208     C                   CALL      'BCH10'
024900020208 MOM C*                  CALL      'TIS7C4R'
025000991214     C                   PARM                    KPJBA
025100991214     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025500970526     C*------------------------------------------------------------------------*
025600970526     C     DECODI        BEGSR
025700000306     C*
025800011205     C                   SETOFF                                       535459
025900000306     C*
026000000306     C*----------------------
026100000306     C* CONTROLLO LA FILIALE
026200000306     C*----------------------
026300000306     C                   MOVEL     *BLANKS       VDDFIL
026400000306     C     ERRL00        IFEQ      '0'
026500000306     C     VIDFIL        IFNE      *BLANKS
026600000829     C     VIDFIL        ANDNE     *ZEROS
026700000306     C                   MOVE      VIDFIL        WLIN
026800000306     C                   EXSR      CHKLIN
026900000306     C     ERRL00        IFEQ      '0'
027000000306     C                   MOVEL     WLIN          VIDFIL
027100000306     C                   MOVEL     WDLIN         VDDFIL
027200000306     C                   ELSE
027300000306     C                   SETON                                        59
027400000306     C                   ENDIF
027500000306     C                   ENDIF
027600000306     C                   ENDIF
027700991214     C*--------------------
027800000830     C* TIPO LEGAME
027900991214     C*--------------------
028000000306     C                   move      *blank        wtlecu
028100000306     C                   move      *blank        wtlece
028200991008IF  1C     ERRL00        IFEQ      '0'
028300000829IF  2C     VIDtle        IFNE      *blank
028400000306     C                   exsr      cnttle
028500991008X   2C                   ELSE
028600991008     C                   MOVEL     '1'           ERRL00
028700991214     C                   SETON                                        53
028800991011     C                   MOVEL     MSG(12)       DSMSMS
028900991008E   2C                   ENDIF
029000991008E   1C                   ENDIF
029100000829     C*                  ENDIF
029200991015     C*--------------------
029300011205     C* DATA RIFERIMENTO
029400991015     C*--------------------
029500991015     C     *LIKE         DEFINE    VIDDDE        DDEVID
029600991015IF  1C     ERRL00        IFEQ      '0'
029700011206     C                   Z-ADD     VIDDDE        WDA8
029800011206     C                   Z-ADD     *ZEROS        DA8W
029900991015     C                   EXSR      CHKDA8
030000991220     C                   Z-ADD     WDA8          VIDDDE
030100011206     C                   Z-ADD     DA8W          DDEVID
030200991015IF  2C     ERRL00        IFEQ      '1'
030300991214     C                   SETON                                        54
030400991015     C                   MOVEL     MSG(14)       dsmsms
030500991015E   2C                   ENDIF
030600991015E   1C                   ENDIF
030700000306     C*
030800970528     C                   ENDSR
030900000810     C*------------------------------------------------------------------------*
031000000306     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
031100000306     C*------------------------------------------------------------------------*
031200000306     C     CHKLIN        BEGSR
031300000306     C*
031400000306     C* CONTROLLO SE E' STATO INSERITO '?'
031500000306     C     '?'           SCAN      WLIN          A                 1 0    99
031600000306     C*
031700000306     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
031800000306IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
031900000306     C                   MOVEL     *BLANKS       §COD1             3
032000000306     C                   MOVE      *BLANKS       §DES1
032100000306     C                   MOVE      *BLANKS       §PRA
032200000306     C                   CALL      'TNSD24R'
032300000306     C                   PARM                    §COD1
032400000306     C                   PARM                    §TIP              1
032500000306     C                   PARM                    §DES1            25
032600000306     C                   PARM                    §PRA              3
032700000306     C                   MOVEL     §COD1         WLIN              3
032800000306     C                   MOVEL     §DES1         WDLIN            20
032900000306     C*
033000000306X   1C                   ENDIF
033100000306     C*
033200000306     C* CARATTERI ALFABETICI NON VALIDI
033300000306IF  2C     WLIN          IFLT      *ZEROS
033400000306     C                   MOVEL     '1'           ERRL00
033500000306     C                   MOVEL     MSG(13)       DSMSMS
033600000306     C                   MOVEL     *ALL'*'       WDLIN
033700000306X   2C                   ELSE
033800000306     C*
033900000306     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
034000000306     C                   MOVEL     WLIN          CODFIL            3 0
034100000306     C     CODFIL        CHAIN     AZORG01L                           99
034200000306IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
034300000306     C                   MOVEL     ORGDES        WDLIN
034400000306X   3C                   ELSE                                                   <-- 2 -->
034500000306     C                   MOVEL     '1'           ERRL00
034600000306     C                   MOVEL     MSG(13)       DSMSMS
034700000306     C                   MOVEL     *ALL'*'       WDLIN
034800000306E   3C                   END                                                    <-- 2 ---
034900000306IF  3C     ORGFAG        IFEQ      'V'                                          --- 2 -->
035000000306     C                   MOVEL     '1'           ERRL00
035100000306     C                   MOVEL     MSG(13)       DSMSMS
035200000306     C                   MOVEL     *ALL'*'       WDLIN
035300000306E   3C                   ENDIF                                                  <-- 2 ---
035400000306E   2C                   ENDIF                                                  <-- 2 ---
035500000306     C*
035600000306     C     ERRL00        IFNE      '1'
035700000810     C                   SETOFF                                           28
035800020910     C     CODFIL        LOOKUP    L1                                     28
035900020903     C     DUTLPO        IFNE      'S'
036000000810     C     *IN28         ANDEQ     *OFF
036100000306     C                   MOVEL     '1'           ERRL00
036200000306     C                   MOVEL     MSG(9)        DSMSMS
036300000306     C                   MOVEL     *ALL'*'       WDLIN
036400000306     C                   ENDIF                                                  <-- 2 ---
036500000810     C                   ENDIF                                                  <-- 2 ---
036600000306     C*
036700000306     C                   ENDSR
036800991008     C*------------------------------------------------------------------------*
036900000810     C* Cnttle - CONTROLLA codice tipo legame
037000991008     C*------------------------------------------------------------------------*
037100991214     C     cnttle        begsr
037200991214     C                   clear                   TIBS02DS
037300991214     C*
037400991008     C     '?'           SCAN      Vidtle                                 30
037500991214     D*  Ricerca
037600991214     C                   eval      t02tla = 'L'
037700991008    1C                   IF        *IN30 = *ON
037800991008     c                   movel     'R'           t02mod
037900991008     c                   else
038000991008     c                   movel     'C'           t02mod
038100991008     c                   movel(p)  vidtle        t02ke1
038200991008     c                   end
038300991008     C                   movel     'TLE'         t02cod
038400991008     c                   movel     knsif         t02sif
038500991008     C                   CALL      'TIBS02R'
038600991008     c                   parm                    KPJBA
038700991008     c                   parm                    TIBS02DS
038800991008     c
038900000829     C*
039000991008    2C                   IF        T02err = *blanks
039100000829     C                   movel     T02ke1        Vidtle
039200000829     C                   movel     T02uni        dtle
039300000807     C                   exsr      chkaut
039400011206     C                   if        errl00 = '0'
039500991014     C                   movel     §tledesc      Vddtle
039600991014     C                   movel     §tlecu        Wtlecu            1
039700991014     C                   movel     §tlece        wtlece            1
039800000807     C                   endif
039900991014     c                   else
040000991014     c                   movel     '1'           errl00
040100991014     c                   seton                                        66
040200991014     C                   MOVEL     MSG(12)       DSMSMS
040300991008     C                   ENDIF
040400991008     c
040500991008     c                   endsr
040600000807     C*------------------------------------------------------------------------*
040700000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
040800000807     C*------------------------------------------------------------------------*
040900000807     C     CHKAUT        BEGSR
041000000807     C*
041100000807     C* - controllo su "operabilità" di TIPO LEGAME -
041200000807     C                   eval      tipprf =  %subst(%trim(KNMUS):1:3)
041300020903     C     DUTLPO        ifeq      'S'
041400000807     C* X SEDE
041500000807     C     ERRL00        ifeq      '0'
041600000807     C                   movel     'TLE'         KTBcod
041700000807     C                   movel(P)  vidtle        KTBky1
041800000807     C     KEYTBE        chain     tntbe01l                           70
041900000807     C     *in70         ifeq      *off
042000000807     C     tbeatb        andeq     *blanks
042100000807     C                   movel     tbeuni        dtle
042200000807     C                   if        §tleaus = 'S' or
042300000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
042400000807     C                   else
042500000810     C                   SETON                                        53
042600000807     C                   MOVEL     '1'           ERRL00
042700000807     C                   MOVEL     MSG(22)       DSMSMS
042800000807     C                   endif
042900000807     C                   else
043000000807     C                   MOVEL     '1'           ERRL00
043100000807     C                   MOVEL     MSG(12)       DSMSMS
043200000807     C                   endif
043300000807     C                   endif
043400000807     C                   else
043500000807     C* X FILIALE
043600000807     C     ERRL00        ifeq      '0'
043700000807     C                   movel     'TLE'         KTBcod
043800000807     C                   movel(P)  vidtle        KTBky1
043900000807     C     KEYTBE        chain     tntbe01l                           70
044000000807     C     *in70         ifeq      *off
044100000807     C     tbeatb        andeq     *blanks
044200000807     C                   movel     tbeuni        dtle
044300000807     C                   if        §tleaut = 'S' or
044400000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
044500000807     C                   else
044600000810     C                   SETON                                        53
044700000807     C                   MOVEL     '1'           ERRL00
044800000807     C                   MOVEL     MSG(22)       DSMSMS
044900000807     C                   endif
045000000807     C                   else
045100000807     C                   MOVEL     '1'           ERRL00
045200000807     C                   MOVEL     MSG(12)       DSMSMS
045300000807     C                   endif
045400000807     C                   endif
045500000807     C                   endif
045600000807     C*
045700000807     C                   ENDSR
045800970528     C*------------------------------------------------------------------------*
045900970528     C* CHKDA8 - CONTROLLA LA DATA (8)
046000970528     C*------------------------------------------------------------------------*
046100970528     C     CHKDA8        BEGSR
046200970528     C*
046300970528     C     *LIKE         DEFINE    VIDDDE        WDA8
046400970528     C     *LIKE         DEFINE    VIDDDE        DA8W
046500970528     C*
046600970528     C                   Z-ADD     WDA8          G08DAT
046700970528     C                   Z-ADD     *ZEROS        G08INV
046800970528     C                   MOVEL     '0'           G08ERR
046900970528     C                   CALL      'XSRDA8'
047000970528     C                   PARM                    WLBDA8
047100970528     C                   Z-ADD     G08DAT        WDA8
047200970528     C                   Z-ADD     G08INV        DA8W
047300970528IF  1C     G08ERR        IFEQ      '1'
047400970528     C                   MOVEL     '1'           ERRL00
047500970528E   1C                   ENDIF
047600970528     C*
047700970528     C                   ENDSR
047800970526     C*------------------------------------------------------------------------*
047900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
048000970526     C*------------------------------------------------------------------------*
048100970526     C     AZZVID        BEGSR
048200970526     C* CAMPI
048300991214     C                   MOVEL     *BLANKS       VIDTLE
048400970528     C                   Z-ADD     *ZEROS        VIDDDE
048500970526     C* DECODIFICHE
048600991214     C                   MOVEL     *BLANKS       VDDTLE
048700000829     C                   MOVEL     *BLANKS       VDDFIL
048800970526     C*
048900970526     C                   ENDSR
049000000303     C*------------------------------------------------------------------------*
049100000905     C* IMPVID - IMPOSTA I DEFAULT A VIDEO
049200000303     C*------------------------------------------------------------------------*
049300000303     C     IMPVID        BEGSR
049400000303     C*
049500011205     C                   Z-ADD     G08DAT        VIDDDE
049600011205     C                   MOVEL     'ST'          VIDTLE
049700011205     C                   MOVEL     *BLANKS       VIDFIL
049800020208     C                   MOVEL     'S'           VIDANO
049900000303     C*
050000000303     C                   ENDSR
050100970526     C*------------------------------------------------------------------------*
050200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
050300970526     C*------------------------------------------------------------------------*
050400970526     C     IMPFUN        BEGSR
050500970526     C*
050600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
050700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
050800970526     C                   EXSR      FUNGES
050900970526     C*
051000970526     C                   ENDSR
051100000810     C*------------------------------------------------------------------------*
051200000810     C* CARTAB - CARICA LE TABELLE OCCORRENTI
051300000810     C*------------------------------------------------------------------------*
051400000810     C     CARTAB        BEGSR
051500020903     C*
051600020903     C* REPERISCE I DATI UTENTE
051700020903     C                   EXSR      REPDATIUTE
051800020903     C*
051900020903     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
052000020903     C                   CLEAR                   L1
052100020903     C                   CLEAR                   TRUL06DS
052200020903     C                   CLEAR                   LIA
052300020903     C                   CLEAR                   TRUL26DS
052400020903     C                   CLEAR                   SKPO
052500020903     C                   CLEAR                   SKPOT
052600020903     C*
052700020903     C* CARICA LE FILIALI IN GESTIONE
052800020903     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
052900020903     C                   EVAL      DEPKPJBU = KPJBU
053000020903     C                   MOVEL     'L'           D06TLA
053100020903     C                   MOVEL     '£1'          D06COD
053200021007     C                   MOVEL     duttfp        D06KEY
053300020903     C                   MOVEL     TRUL06DS      KPJBU
053400020903     C                   CALL      'TRUL06R'
053500020903     C                   PARM                    KPJBA
053600020903     C                   MOVEL     KPJBU         TRUL06DS
053700020903     C                   MOVEA     LIA           L1                              -P.O. terminal
053800020903     C                   EVAL      KPJBU = DEPKPJBU
053900020903     C                   ENDIF
054000020903     C*
054100020903     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
054200020906     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
054300020903     C                   ENDIF
054400020903     C*
054500020903     C                   IF        DUTLPO = 'S'                                 *SEDE
054600020903     C                   EVAL      I26TLA = 'L'
054700020903     C                   EVAL      I26TPT = 'P'
054800020903     C                   CALL      'TRUL26R'
054900020903     C                   PARM                    TRUL26DS
055000020903     C                   MOVEA     SKPO          L1                              -tutti i P.O.
055100020903     C                   ENDIF
055200000810     C*
055300000810     C                   ENDSR
055400020903     C*--------------------------------------------------------------------------------------------*
055500020903     C* REPERISCE I DATI UTENTE
055600020903     C*--------------------------------------------------------------------------------------------*
055700020903     C     REPDATIUTE    BEGSR
055800020903     C*
055900020903     C* INIZIALIZZA VARIABILI DI WRK
056000020903     C                   CLEAR                   TIBS34DS
056100020903     C                   CLEAR                   AZUTEDS
056200020903     C                   CLEAR                   DDATIUTE
056300020903     C*
056400020903     C     *DTAARA       DEFINE    §azute        azuteds
056500020903     C     *DTAARA       DEFINE    §datiute      ddatiute
056600020903     C                   IN(E)     *DTAARA
056700020903if  1C                   IF        %Error
056800020903     c                   EVAL      I34Tla = 'L'
056900020903     C                   CALL      'TIBS34R'
057000020903     C                   PARM                    Tibs34Ds
057100020903     C                   IN        *DTAARA
057200020903e   1C                   ENDIF
057300020903     C*
057400020903     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
057500020916     C                   MOVEL(P)  RSUT          DSFIRS           20
057600020903     C*
057700020903     C                   ENDSR
057800970526     C*------------------------------------------------------------------------*
057900970526     C* *INZSR - OPERAZIONI INIZIALI
058000970526     C*------------------------------------------------------------------------*
058100970526     C     *INZSR        BEGSR
058200991217     C*
058300991217     C     *ENTRY        PLIST
058400991217     C                   PARM                    KPJBA
058500011205     C*
058600000807     C* LETTURA tntbe01l -
058700000807     C     KEYtbe        KLIST
058800000807     C                   KFLD                    Ktbcod                         *CODICE TABELLA
058900000807     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
059000000810     C* LETTURA tabel00f - PARZIALE
059100000810     C     KE2tab        KLIST
059200000810     C                   KFLD                    kblkut                         *UTENTE
059300000810     C                   KFLD                    kblcod                         *TABELLA
059400000807     C*---
059500000807     C* VARIABILI RIFERITE AL DATA BASE
059600000807     C*---
059700000807     C     *LIKE         DEFINE    tbecod        KTBcod
059800000807     C     *LIKE         DEFINE    tbeke1        KTBky1
059900000810     C     *LIKE         DEFINE    tblkut        kblkut                         *TABEL00F
060000000810     C     *LIKE         DEFINE    tblcod        kblcod
060100020903     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
060200970526     C*
060300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
060400970526     C     XMSGPA        PLIST
060500970526     C                   PARM                    DSMSMS
060600970526     C                   PARM                    DSMSPG
060700970526     C                   PARM                    DSMSKE
060800970526     C                   PARM                    DSMSER
060900970526     C*
061000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
061100970526     C                   CALL      'XKEY'
061200970526     C                   PARM                    DSKEY
061300970526     C*---
061400970526     C* VARIABILI NON RIFERITE AL DATA BASE
061500970526     C*---
061600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
061700991214     C                   MOVEL     *BLANKS       DEPOP1            3
061800970526     C*---
061900970526     C* VARIABILI DI CONTROLLO
062000970526     C*---
062100970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
062200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
062300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
062400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
062500970526     C*
062600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
062700020208     C                   MOVEL     'TIS7C3R'     DSMSPG
062800020208     C                   MOVEL     'TIS7C3R'     MSGQUE
062900970526     C                   MOVEL     *BLANKS       MSGKEY
063000970526     C                   MOVEL     *BLANKS       DSMSMS
063100970528     C*
063200970528     C* CALCOLA LA DATA CORRENTE
063300970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
063400970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
063500970528     C                   Z-ADD     WN8           G08DAT
063600970528     C                   Z-ADD     *ZEROS        G08INV
063700970528     C                   MOVEL     '0'           G08ERR
063800970528     C                   CALL      'XSRDA8'
063900970528     C                   PARM                    WLBDA8
064000970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
064100000810     C*
064200000810     C* CARICA LE TABELLE OCCORRENTI
064300000810     C                   EXSR      CARTAB
064400000310     C*
064500000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
064600000310     C                   EXSR      AZZVID
064700000310     C                   EXSR      IMPVID
064800970526     C*
064900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
065000970526     C                   EXSR      IMPFUN
065100970526     C*
065200970526     C                   ENDSR
065300970526     C*------------------------------------------------------------------------*
065400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
065500970526     C*------------------------------------------------------------------------*
065600970526     C     FUNGES        BEGSR
065700970526     C*
065800970526     C* PULISCO CAMPI DI LAVORO
065900970526     C                   Z-ADD     0             W                 5 0
066000970526     C*
066100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
066200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
066300970526     C     1             DO        24            I
066400970526     C                   MOVEL     FUD(I)        FUDRIG
066500970526     C*
066600970526     C* PULIZIA FUNZIONI PRECEDENTI
066700970526     C                   MOVEL     *BLANKS       FUV(I)
066800970526     C*
066900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
067000970526     C* (SALTA F24)
067100970526     C     FUDATT        IFEQ      'S'
067200970526     C     FUDFUN        ANDNE     '24'
067300970526     C                   ADD       1             W
067400970526     C                   MOVEL     FUDDES        FUV(W)
067500970526     C                   END
067600970526     C                   END                                                    DO WHILE
067700970526     C*
067800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
067900970526     C* ALTRIMENTI DISATTIVO F24
068000970526     C     1             DO        24            I
068100970526     C                   MOVEL     FUD(I)        FUDRIG
068200970526     C*
068300970526     C     FUDFUN        IFEQ      '24'
068400970526     C     W             IFGT      6
068500970526     C                   MOVEL     'S'           FUDATT
068600970526     C                   ELSE
068700970526     C                   MOVEL     *BLANKS       FUDATT
068800970526     C                   END                                                    W GT 8
068900970526     C                   MOVEL     FUDRIG        FUD(I)
069000970526     C                   END
069100970526     C                   END                                                    DO WHILE
069200970526     C*
069300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
069400970526     C     W             DIV       6             I
069500970526     C                   MVR                     Z
069600970526     C*
069700970526     C     Z             IFNE      0
069800970526     C     I             ADD       1             Z
069900970526     C                   ELSE
070000970526     C                   Z-ADD     I             Z                 5 0
070100970526     C                   END
070200970526     C*
070300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
070400970526     C                   ADD       1             FUNGRU
070500970526     C     FUNGRU        IFGT      Z
070600970526     C     FUNGRU        ORLT      1
070700970526     C                   Z-ADD     1             FUNGRU
070800970526     C                   END
070900970526     C*
071000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
071100970526     C     FUNGRU        IFEQ      1
071200970526     C                   MOVEL     FUV(1)        FUN001
071300970526     C                   MOVEL     FUV(2)        FUN002
071400970526     C                   MOVEL     FUV(3)        FUN003
071500970526     C                   MOVEL     FUV(4)        FUN004
071600970526     C                   MOVEL     FUV(5)        FUN005
071700970526     C                   MOVEL     FUV(6)        FUN006
071800970526     C                   END
071900970526     C*
072000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
072100970526     C     FUNGRU        IFEQ      2
072200970526     C                   MOVEL     FUV(7)        FUN001
072300970526     C                   MOVEL     FUV(8)        FUN002
072400970526     C                   MOVEL     FUV(9)        FUN003
072500970526     C                   MOVEL     FUV(10)       FUN004
072600970526     C                   MOVEL     FUV(11)       FUN005
072700970526     C                   MOVEL     FUV(12)       FUN006
072800970526     C                   END
072900970526     C*
073000970526     C* TERZO GRUPPO DI 6 FUNZIONI
073100970526     C     FUNGRU        IFEQ      3
073200970526     C                   MOVEL     FUV(13)       FUN001
073300970526     C                   MOVEL     FUV(14)       FUN002
073400970526     C                   MOVEL     FUV(15)       FUN003
073500970526     C                   MOVEL     FUV(16)       FUN004
073600970526     C                   MOVEL     FUV(17)       FUN005
073700970526     C                   MOVEL     FUV(18)       FUN006
073800970526     C                   END
073900970526     C*
074000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
074100970526     C     FUNGRU        IFEQ      4
074200970526     C                   MOVEL     FUV(19)       FUN001
074300970526     C                   MOVEL     FUV(20)       FUN002
074400970526     C                   MOVEL     FUV(21)       FUN003
074500970526     C                   MOVEL     FUV(22)       FUN004
074600970526     C                   MOVEL     *BLANKS       FUN005
074700970526     C                   MOVEL     *BLANKS       FUN006
074800970526     C                   END
074900970526     C*
075000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
075100970526     C                   MOVEL     *BLANKS       FUN007
075200970526     C                   MOVEL     *BLANKS       FUN008
075300970526     C*
075400970526     C     1             DO        24            I
075500970526     C                   MOVEL     FUD(I)        FUDRIG
075600970526     C     FUDFUN        IFEQ      '24'
075700970526     C     FUDATT        ANDEQ     'S'
075800970526     C                   MOVEL     FUDDES        FUN008
075900970526     C                   END
076000970526     C                   END                                                    DO
076100970526     C*
076200970526     C                   ENDSR
076300970526     C*------------------------------------------------------------------------*
076400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
076500970526     C*------------------------------------------------------------------------*
076600970526     C     FUNCON        BEGSR
076700970526     C*
076800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
076900970526     C                   EXSR      FUNVAR
077000970526     C*
077100970526     C* PULISCO CAMPI DI LAVORO
077200970526     C                   Z-ADD     0             I                 5 0
077300970526     C                   Z-ADD     0             W                 5 0
077400970526     C*
077500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
077600970526     C     FUNKEY        IFNE      *BLANKS
077700970526     C     1             DO        24            I
077800970526     C                   MOVEL     FUD(I)        FUDRIG
077900970526     C*
078000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
078100970526     C     FUNKEY        IFEQ      FUDFUN
078200970526     C     FUDATT        ANDEQ     'S'
078300970526     C                   Z-ADD     I             W
078400970526     C                   END
078500970526     C                   END                                                    DO WHILE
078600970526     C*
078700970526     C* FUNZIONE NON ATTIVA (ERRORE)
078800970526     C     W             IFEQ      0
078900970526     C*
079000970526     C* EMISSIONE MESSAGGIO DI ERRORE
079100970526     C                   MOVEL     MSG(1)        DSMSMS
079200970526     C*
079300970526     C                   END                                                    W=0
079400970526     C                   END                                                    FUNKEY NE *BLANKS
079500970526     C*
079600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
079700970526     C     W             IFNE      0
079800970526     C     FUNKEY        OREQ      *BLANKS
079900970526     C     FUNKEY        IFEQ      *BLANKS
080000970526     C                   EXSR      ENTGES
080100970526     C                   END
080200970526     C     FUNKEY        IFEQ      '03'
080300970526     C                   EXSR      F03GES
080400970526     C                   END
080500970526     C     FUNKEY        IFEQ      '06'
080600970526     C                   EXSR      F06GES
080700970526     C                   END
080800970526     C     FUNKEY        IFEQ      '12'
080900970526     C                   EXSR      F12GES
081000970526     C                   END
081100970526     C     FUNKEY        IFEQ      '24'
081200970526     C                   EXSR      F24GES
081300970526     C                   END
081400970526     C                   END
081500970526     C*
081600970526     C                   ENDSR
081700970526     C*------------------------------------------------------------------------*
081800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
081900970526     C*------------------------------------------------------------------------*
082000970526     C     FUNVAR        BEGSR
082100970526     C*
082200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
082300970526     C*
082400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
082500970526     C     KEY           IFEQ      ENTER
082600970526     C                   MOVEL     *BLANKS       FUNKEY
082700970526     C                   END
082800970526     C     KEY           IFEQ      F1
082900970526     C                   MOVEL     '01'          FUNKEY
083000970526     C                   END
083100970526     C     KEY           IFEQ      F2
083200970526     C                   MOVEL     '02'          FUNKEY
083300970526     C                   END
083400970526     C     KEY           IFEQ      F3
083500970526     C                   MOVEL     '03'          FUNKEY
083600970526     C                   END
083700970526     C     KEY           IFEQ      F4
083800970526     C                   MOVEL     '04'          FUNKEY
083900970526     C                   END
084000970526     C     KEY           IFEQ      F5
084100970526     C                   MOVEL     '05'          FUNKEY
084200970526     C                   END
084300970526     C     KEY           IFEQ      F6
084400970526     C                   MOVEL     '06'          FUNKEY
084500970526     C                   END
084600970526     C     KEY           IFEQ      F7
084700970526     C                   MOVEL     '07'          FUNKEY
084800970526     C                   END
084900970526     C     KEY           IFEQ      F8
085000970526     C                   MOVEL     '08'          FUNKEY
085100970526     C                   END
085200970526     C     KEY           IFEQ      F9
085300970526     C                   MOVEL     '09'          FUNKEY
085400970526     C                   END
085500970526     C     KEY           IFEQ      F10
085600970526     C                   MOVEL     '10'          FUNKEY
085700970526     C                   END
085800970526     C     KEY           IFEQ      F11
085900970526     C                   MOVEL     '11'          FUNKEY
086000970526     C                   END
086100970526     C     KEY           IFEQ      F12
086200970526     C                   MOVEL     '12'          FUNKEY
086300970526     C                   END
086400970526     C     KEY           IFEQ      F13
086500970526     C                   MOVEL     '13'          FUNKEY
086600970526     C                   END
086700970526     C     KEY           IFEQ      F14
086800970526     C                   MOVEL     '14'          FUNKEY
086900970526     C                   END
087000970526     C     KEY           IFEQ      F15
087100970526     C                   MOVEL     '15'          FUNKEY
087200970526     C                   END
087300970526     C     KEY           IFEQ      F16
087400970526     C                   MOVEL     '16'          FUNKEY
087500970526     C                   END
087600970526     C     KEY           IFEQ      F17
087700970526     C                   MOVEL     '17'          FUNKEY
087800970526     C                   END
087900970526     C     KEY           IFEQ      F18
088000970526     C                   MOVEL     '18'          FUNKEY
088100970526     C                   END
088200970526     C     KEY           IFEQ      F19
088300970526     C                   MOVEL     '19'          FUNKEY
088400970526     C                   END
088500970526     C     KEY           IFEQ      F20
088600970526     C                   MOVEL     '20'          FUNKEY
088700970526     C                   END
088800970526     C     KEY           IFEQ      F21
088900970526     C                   MOVEL     '21'          FUNKEY
089000970526     C                   END
089100970526     C     KEY           IFEQ      F22
089200970526     C                   MOVEL     '22'          FUNKEY
089300970526     C                   END
089400970526     C     KEY           IFEQ      F23
089500970526     C                   MOVEL     '23'          FUNKEY
089600970526     C                   END
089700970526     C     KEY           IFEQ      F24
089800970526     C                   MOVEL     '24'          FUNKEY
089900970526     C                   END
090000970526     C     KEY           IFEQ      ROLLUP
090100970526     C                   MOVEL     'RU'          FUNKEY
090200970526     C                   END
090300970526     C     KEY           IFEQ      HELP
090400970526     C                   MOVEL     'HP'          FUNKEY
090500970526     C                   END
090600970526     C                   ENDSR
090700970526     C*------------------------------------------------------------------------*
090800970526     C* NOTE COSTRUZIONE SCHIERE
090900970526     C*------------------------------------------------------------------------*
091000970526     C*
091100970526     C* -MSG-MESSAGGI
091200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
091300970526     C* (NON TOCCARE MSG,1 E MSG,2)
091400970526     C*
091500970526     C* -CMD-COMANDI
091600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
091700970526     C*
091800970526     C* -OPD-OPZIONI
091900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
092000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
092100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
092200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
092300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
092400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
092500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
092600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
092700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
092800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
092900970526     C*
093000970526     C* -FUD-FUNZIONI
093100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
093200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
093300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
093400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
093500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
093600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
093700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
093800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
093900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
094000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
094100970526     O*------------------------------------------------------------------------*
094200970526** MSG - MESSAGGI
094300020208TIS7C3R- Tasto funzionale non gestito                                          1
094400020208TIS7C3R-                                                                       2
094500020208TIS7C3R-                                                                       3
094600020208TIS7C3R-                                                                       4
094700020208TIS7C3R-                                                                       5
094800020208TIS7C3R-                                                                       6
094900020208TIS7C3R- Cliente incompatibile per Filiale selezionata.                        7
095000020208TIS7C3R- Cliente non autorizzato.                                              8
095100020208TIS7C3R- Punto operativo non autorizzato.                                      9
095200020208TIS7C3R- Anno inserito non accettabile.                                        10
095300020208TIS7C3R- Mese inserito non valido.                                             11
095400020208TIS7C3R- Tipo legame errato verificarne sulla tabella l'esistenza              12
095500020208TIS7C3R- Punto operativo errato.                                               13
095600020208TIS7C3R- Data errata                                                           14
095700020208TIS7C3R- La data scadenza deve essere maggiore della decorrenza                15
095800020208TIS7C3R- Cliente inserito inesistente.                                         16
095900020208TIS7C3R- Cliente inserito sospeso.                                             17
096000020208TIS7C3R- Tipo lancio obbligatorio.                                             18
096100020208TIS7C3R- Cliente inserito è senza P.IVA o Cod.Fisc.                            19
096200020208TIS7C3R- Il codice figlio non può essere uguale al codice padre.               20
096300020208TIS7C3R- Scelta obbligatoria.                                                  21
096400020208TIS7C3R- Tipo legame non autorizzato.                                          22
096500020208TIS7C3R- Filiale non abilitata alla gestione dei clienti scelti.               23
096600020208TIS7C3R-                                                                       24
096700970526** FUD - FUNZIONI
096800970526                   -        (S/N)=N                        funz.01¬
096900970526                   -        (S/N)=N                        funz.02¬
097000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
097100970526                   -        (S/N)=N                        funz.04¬
097200970526                   -        (S/N)=N                        funz.05¬
097300991214F06=Conferma       -        (S/N)=S                        funz.06¬
097400970527                   -        (S/N)=N                        funz.07¬
097500970527                   -        (S/N)=N                        funz.08¬
097600970527                   -        (S/N)=N                        funz.09¬
097700970527                   -        (S/N)=N                        funz.10¬
097800970526                   -        (S/N)=N                        funz.11¬
097900970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
098000970526                   -        (S/N)=N                        funz.13¬
098100970526                   -        (S/N)=N                        funz.14¬
098200970526                   -        (S/N)=N                        funz.15¬
098300970526                   -        (S/N)=N                        funz.16¬
098400970526                   -        (S/N)=N                        funz.17¬
098500970526                   -        (S/N)=N                        funz.18¬
098600970526                   -        (S/N)=N                        funz.19¬
098700970526                   -        (S/N)=N                        funz.20¬
098800970526                   -        (S/N)=N                        funz.21¬
098900970526                   -        (S/N)=N                        funz.22¬
099000970526                   -        (S/N)=N                        funz.23¬
099100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
