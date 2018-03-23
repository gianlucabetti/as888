000100970526     H*------------------------------------------------------------------------*
000200991008     H* Gestione codici unificanti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800991214     Ftis7k1d   cf   e             WORKSTN INFDS(DEVDS1)
000900991214     F                                     SFILE(i7k1DMS:MSGULT)
001000000306     Fcnaco00f  IF   E           K DISK    USROPN
001100000306     F                                     RENAME(cnaco000:cnacoXXX)
001200000306     Fazorg01l  IF   E           K DISK
001300000807     Ftntbe01l  IF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D* MESSAGGI
001800970526     D*------------
001900000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* DESCRIZIONI TASTI FUNZIONALI
002200970526     D*------------
002300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* FUNZIONI DA CARICARE A VIDEO
002600970526     D*------------
002700970526     D FUV             S             19    DIM(24)
002800000829     D*------------
002900000829     D* DESCRIZIONI VARIE
003000000829     D*------------
003100000829     D DES             S             40    DIM(02) CTDATA PERRCD(1)
003200970526     D*------------------------------------------------------------------------*
003300970526     D* INPUT
003400970526     D*------------------------------------------------------------------------*
003500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003600970526     D*------------
003700970526     D FUDDS           DS
003800970526     D  FUDDE1                 1      1
003900970526     D  FUDFUN                 2      3
004000970526     D  FUDDE2                 4     19
004100970526     D  FUDFI0                20     34
004200970526     D  FUDATT                35     35
004300970526     D  FUDFI1                36     80
004400970526     D  FUDDES                 1     19
004500970526     D  FUDRIG                 1     80
004600970526     D*------------
004700970526     D* DS GESTIONE TASTI FUNZIONALI
004800970526     D*------------
004900970526     D DSKEY         E DS
005000970526     D DEVDS1          DS
005100970526     D  KEY                  369    369
005200970526     D*------------
005300970526     D* DS "XMSG" - GESTIONE MESSAGGI
005400970526     D*------------
005500970526     D DSMSG         E DS
005600970528     D*------------------
005700970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800970528     D*------------------
005900970528     D WLBDA8          DS                  INZ
006000970528     D  G08DAT                 1      8  0
006100970528     D  G08INV                 9     16  0
006200970528     D  G08ERR                17     17
006300970528     D  G08TGI                18     22  0
006400000306     D*
006500000306     D                 DS
006600000306     D  AAAA                   1      4  0
006700000306     D  MM                     5      6  0
006800000306     D  GG                     7      8  0
006900000306     D DATA                    1      8  0
007000991214     D*------------
007100991214     D* DS DI PROCEDURA
007200991214     D*------------
007300991214     D TIBS02DS      E DS
007400991216     D DTLE          E DS
007500991216     D*------------------
007600991216     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
007700991216     D*------------------
007800991216     D DSXC          e DS                  extname(XCLIRDS)
007900991216     D*---
008000991216     D                 DS
008100991216     D dsksn                   1      4p 0
008200991216     D dsksa                   1      4
008300970526     D*------------
008400970526     D* ARCHITETTURA
008500970526     D*------------
008600970526     D KPJBA         E DS
008700991215     D*------------
008800991215     D* TABELLE "XCLIR"- RICERCA CLIENTI
008900991215     D*------------
009000991216     D KSC             s              7  0 dim(30)
009100991216     D KSA             s              4    dim(30)
009200991216     D*------------------
009300991216     D BS69DS        e ds                  extname(TIBS69DS)
009400991216     D ACODS         e ds                  extname(cnaco00f)
009500991216     D INDDS         e ds                  extname(cnind00f)
009600991216     D CLPDS         e ds                  extname(cnclp00f)
009700991216     D CLSDS         e ds                  extname(fncls00f)
009800991214     D*------------------
009900991214     D* DS X PASSAGGIO PARAMETRI
010000991214     D*------------------
010100991214     D PARAM           DS                  INZ
010200991214     D  PARAAA                 1      4  0
010300991214     D  PARMES                 5      6  0
010400991214     D  PARTLE                 7      8
010500991214     D  PARDDE                 9     16  0
010600991214     D  PARDSC                17     24  0
010700991216     D  PARKSC                25     40
010800991216     D  PARFLG                41     41
010900991217     D  PARSOC                42     89
011000991217     D  PARCLI                90     96
011100000303     D  PARANO                97     97
011200000306     D  PARDLE                98    127
011300000306     D  PARFIL               128    130  0
011400000306     D  PARAS4               131    133
011500000306     D  PARDFI               134    163
011600000310     D  PARABI               164    164
011700000310     D  PARFIC               165    165  0
011800020212     D  PAREXL               166    166
011900020301     D  PARBLK               167    167
012000020903     D*-------------
012100020903     D* DS DI WRK
012200020903     D*-------------
012300020903     D TIBS34DS      E DS                                                       *Profili utente
012400020903     D DDATIUTE      E DS                                                       *Dati utente
012500020903     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012600020903     D TRUL06DS      E DS                                                       *caricamento £x
012700020903     D  LIA                    1     90  0 DIM(30)                               -filiali
012800020903     D TRUL26DS      E DS                                                       *caricamento P.O.
012900020903     D  SKPO                  11    760  0 dim(250)
013000020903     D  SKPOT                761   1510  0 dim(250)
013100000807     D*------------------
013200000807     D* VARIABILI NON RIFERITE AL DATABASE
013300000807     D*------------------
013400000807     D $esikun         S              1                                         *Esistenza KUN
013500000807     D chk             S              1    inz('1')
013600000807     D giro            S              1  0 inz(*zeros)
013700000807     D tipprf          s              3    inz(*blanks)
013800000810     D*-------------------
013900000810     D* FILIALI IN GESTIONE
014000000810     D*-------------------
014100020903     D L1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
014200970526     C*------------------------------------------------------------------------*
014300000303     C* MAIN LINES
014400970526     C*------------------------------------------------------------------------*
014500970526     C*
014600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015000970526     C*
015100970526     C* EMETTE FORMATO VIDEO
015200970526     C                   EXSR      BUTFMR
015300970526     C*
015400970526     C* GESTIONE TASTI FUNZIONALI
015500970526     C                   EXSR      FUNCON
015600970526E   2C                   ENDDO
015700000306     C*
015800020516     C**N67              CLOSE     cnaco31c
015900020516     C  N67              CLOSE     cnaco00f
016000000306     C   67              CLOSE     cnaco00f
016100970526     C*
016200970526     C* FINE PGM
016300970526     C                   SETON                                        LR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016600970526     C*------------------------------------------------------------------------*
016700970526     C     BUTFMR        BEGSR
016800970526     C*
016900970526     C* EMETTE LA TESTATA
017000991214     C                   WRITE     i7k1D01
017100970526     C*
017200970526     C* SPEDISCE EVENTUALE MESSAGGIO
017300970526     C                   EXSR      SNDMSG
017400970526     C*
017500970526     C* EMETTE LE FUNZIONI
017600991214     C                   WRITE     i7k1D02
017700970526     C*
017800970526     C* EMETTE IL VIDEO
017900991214     C                   EXFMT     i7k1D03
018000970526     C*
018100991214     C* REIMPOSTA FLAG DI ERRORE
018200991214     C                   MOVEL     '0'           ERRL00
018300970526     C*
018400970526     C* REIMPOSTA INDICATORI DI ERRORE
018500991214     C*
018600991214     C                   SETOFF                                       515253
018700991214     C                   SETOFF                                       5455
018800991215     C                   SETOFF                                       5657
018900991214     C*
019000970526     C*
019100970526     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
019400970526     C*------------------------------------------------------------------------*
019500970526     C     SNDMSG        BEGSR
019600970526     C*
019700970526     C* PULISCE IL SFILE MESSAGGI
019800970526     C                   MOVEL     '1'           *IN40
019900991214     C                   WRITE     i7k1DMC
020000970526     C                   MOVEL     '0'           *IN40
020100970526     C*
020200970526     C* VISUALIZZA IL MESSAGGIO
020300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
020400970526     C                   CALL      'XMSG'        XMSGPA
020500970526IF  1C     DSMSER        IFEQ      *ZEROS
020600970526     C                   MOVEL     DSMSKE        MSGKEY
020700970526E   1C                   ENDIF
020800970526     C                   Z-ADD     1             MSGULT
020900991214     C                   WRITE     i7k1DMS
021000991214     C                   WRITE     i7k1DMC
021100970526     C*
021200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
021300970526     C                   MOVEL     *BLANKS       DSMSMS
021400970526     C*
021500970526     C                   ENDSR
021600970526     C*------------------------------------------------------------------------*
021700970526     C* ENTGES - GESTIONE ENTER -> INVIO
021800970526     C*------------------------------------------------------------------------*
021900970526     C     ENTGES        BEGSR
022000970526     C*
022100970526     C* CONTROLLA I CAMPI IMMESSI
022200970526     C                   EXSR      DECODI
022300970526     C*
022400970526     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* F03GES - GESTIONE F03 -> FINE
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F03GES        BEGSR
022900970526     C*
023000970526     C* IMPOSTA "PREMUTO F03"
023100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500991214     C* F06GES - GESTIONE F06 -> CONFERMA
023600970526     C*------------------------------------------------------------------------*
023700970526     C     F06GES        BEGSR
023800970526     C*
023900991214     C* CONTROLLA I CAMPI IMMESSI
024000991214     C                   EXSR      DECODI
024100991214     C     ERRL00        IFEQ      '0'
024200991214     C* ESEGUE ROUTINE DI CONFERMA
024300991214     C                   EXSR      ESEGUI
024400991214     C                   ENDIF
024500970526     C*
024600991214     C                   ENDSR
024700970526     C*------------------------------------------------------------------------*
024800970526     C* F12GES - GESTIONE F12 -> RITORNO
024900970526     C*------------------------------------------------------------------------*
025000970526     C     F12GES        BEGSR
025100970526     C*
025200970526     C* IMPOSTA "PREMUTO F12"
025300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025800970526     C*------------------------------------------------------------------------*
025900970526     C     F24GES        BEGSR
026000970526     C*
026100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026200970526     C*
026300970526     C                   ENDSR
026400991214     C*------------------------------------------------------------------------*
026500991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
026600991214     C*------------------------------------------------------------------------*
026700991214     C     PASPAR        BEGSR
026800991214     C*
026900991214     C                   Z-ADD     VIDAAA        PARAAA
027000991214     C                   Z-ADD     VIDMES        PARMES
027100991214     C                   MOVEL     VIDTLE        PARTLE
027200991220     C                   Z-ADD     VIDDDE        PARDDE
027300991220     C                   Z-ADD     VIDDSC        PARDSC
027400991216     C                   MOVEL     VIHIVA        PARKSC
027500991215     C                   MOVEL     VIDFLG        PARFLG
027600991217     C                   MOVEL     VIDRAG        PARSOC
027700991217     C                   MOVEL     VIDKSC        PARCLI
027800000303     C                   MOVEL     VIDANO        PARANO
027900000306     C                   MOVEL     VDDTLE        PARDLE
028000000306     C                   MOVEL     VIDFIL        PARFIL
028100000306     C                   MOVEL     VDDFIL        PARDFI
028200000310     C                   MOVEL     VIDABI        PARABI
028300020903     C                   Z-ADD     1             PARFIC
028400020212     C                   MOVEL     VIDEXL        PAREXL
028500020301     C                   MOVEL     VIDBLK        PARBLK
028600991214     C*
028700991214     C                   ENDSR
028800970526     C*------------------------------------------------------------------------*
028900991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
029000970526     C*------------------------------------------------------------------------*
029100991214     C     ESEGUI        BEGSR
029200991214     C*
029300991214     C*
029400991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
029500991217     C                   MOVEL     'S7K2'        KCOAZ
029600991214     C                   EXSR      PASPAR
029700991214     C                   MOVEL     PARAM         KPJBU
029800020301     C                   CALL      'BCH10'
029900020301 MOM C*                  CALL      'TIS7K2C'
030000991214     C                   PARM                    KPJBA
030100991214     C*
030200970526     C                   ENDSR
030300970526     C*------------------------------------------------------------------------*
030400991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
030500970526     C*------------------------------------------------------------------------*
030600970526     C     DECODI        BEGSR
030700000306     C*
030800000306     C                   SETOFF                                       515253
030900000306     C                   SETOFF                                       545556
031000000306     C                   SETOFF                                       575859
031100000306     C*
031200991008     C*--------------------
031300000306     C* ANNO
031400991008     C*--------------------
031500991214IF  1C     ERRL00        IFEQ      '0'
031600991214     C     VIDAAA        IFLT      1900
031700991214     C                   MOVEL     '1'           ERRL00
031800991214     C                   SETON                                        51
031900991217     C                   MOVEL     MSG(10)       DSMSMS
032000991214E   2C                   ENDIF
032100991214E   1C                   ENDIF
032200991214     C*--------------------
032300000306     C* MESE
032400991214     C*--------------------
032500991214IF  1C     ERRL00        IFEQ      '0'
032600991214     C     VIDMES        IFLT      1
032700991214     C     VIDMES        ORGT      12
032800991214     C                   MOVEL     '1'           ERRL00
032900991214     C                   SETON                                        52
033000991217     C                   MOVEL     MSG(11)       DSMSMS
033100991214E   2C                   ENDIF
033200991214E   1C                   ENDIF
033300000306     C*----------------------
033400000306     C* CONTROLLO LA FILIALE
033500000306     C*----------------------
033600000306     C                   MOVEL     *BLANKS       VDDFIL
033700000306     C     ERRL00        IFEQ      '0'
033800000306     C     VIDFIL        IFNE      *BLANKS
033900000829     C     VIDFIL        ANDNE     *ZEROS
034000000306     C                   MOVE      VIDFIL        WLIN
034100000306     C                   EXSR      CHKLIN
034200000306     C     ERRL00        IFEQ      '0'
034300000306     C                   MOVEL     WLIN          VIDFIL
034400000306     C                   MOVEL     WDLIN         VDDFIL
034500000306     C                   ELSE
034600000306     C                   SETON                                        59
034700000306     C                   ENDIF
034800000829     C                   ELSE
034900020903     C     DUTLPO        IFEQ      'S'
035000000829     C                   MOVEL     DES(01)       VDDFIL
035100000829     C                   ENDIF
035200020903     C     DUTLPO        IFEQ      '1'
035300000829     C                   MOVEL     DES(02)       VDDFIL
035400000829     C                   ENDIF
035500000306     C                   ENDIF
035600000306     C                   ENDIF
035700991215     C*--------------------
035800000306     C* CLIENTI
035900991215     C*--------------------
036000991215     C     ERRL00        IFEQ      '0'
036100991216     C     '?'           SCAN      VIDKSC                                 30
036200991215     C     *IN30         IFEQ      *ON
036300991215     C                   CLEAR                   DSXC
036400020924     C                   Z-ADD     DUTKCI        DXCCAP
036500991215     C                   CALL      'XCLIR'
036600991215     C                   PARM                    DSXC
036700991215     C     DXCERR        ifne      '1'
036800991216     C*-------------
036900991216     C                   MOVEA     DXCKSC        KSA                        ALF
037000991216DO  2C     1             DO        30            J                 3 0
037100991216     C                   MOVE      KSA(J)        DSKSA                      F
037200991216     C                   Z-ADD     DSKSN         KSC(J)                     M
037300991216E   2C                   ENDDO
037400991216     C*-------------
037500991216     C                   MOVE      KSC(1)        VIDKSC
037600991215     C                   ENDIF
037700991215     C                   ENDIF
037800991216     C     VIDKSC        IFNE      *zeros
037900991216     C     VIDKSC        ANDNE     *blanks
038000020924     C                   Z-ADD     DUTKCI        kclkcc
038100991216     C                   MOVE      VIDKSC        kclksc
038200020516     C**N67keycli        chain     cnaco31c                           99
038300020516     C  N67keycli        chain     cnaco00f                           99
038400000810     C   67keycli        chain     cnaco00f                           99
038500991215     C     *IN99         ifeq      *off
038600991215     C     acoflg        ifeq      '*'
038700130314     C     acoabl        orne      *blanks
038800991215     C                   MOVEL     '1'           ERRL00
038900991215     C                   SETON                                        56
039000000810     C                   MOVEL     MSG(17)       DSMSMS
039100991216     C                   else
039200991216     C                   MOVEL     acorag        VIDRAG
039300991216     C* ---------- LETTURA SU CNIND00F -----------
039400991216     C                   CLEAR                   BS69DS
039500991216     C                   CLEAR                   INDDS
039600991216     C                   MOVEL     KNSIF         I69SIF
039700991216     C                   Z-ADD     acoksc        I69KIN
039800991216     C                   CALL      'TIBS69R'
039900991216     C                   PARM                    BS69DS
040000991216     C                   PARM                    ACODS
040100991216     C                   PARM                    INDDS
040200991216     C                   PARM                    CLPDS
040300991216     C                   PARM                    CLSDS
040400991216     C     O69ERR        IFNE      '1'
040500991216     C* (SE P.IVA E COD.FISC. = *BLANK => ESCLUDO CLIENTE)
040600991216     C     INDIVA        IFNE      *BLANKS
040700991216     C                   MOVEL     INDIVA        VIHIVA
040800991216     C                   ELSE
040900991216     C     INDCDF        IFNE      *BLANKS
041000991216     C                   MOVEL     INDCDF        VIHIVA
041100991216     C                   ELSE
041200991216     C                   MOVEL     '1'           ERRL00
041300991216     C                   SETON                                        56
041400991220     C                   MOVEL     MSG(19)       DSMSMS
041500991216     C                   ENDIF
041600991216     C                   ENDIF
041700991216     C                   ELSE
041800991216     C                   MOVEL     '1'           ERRL00
041900991216     C                   SETON                                        56
042000991216     C                   MOVEL     O69MSG        DSMSMS
042100991216     C                   ENDIF
042200991216     C*-------- FINE LETTURA CNIND00F -----------
042300991215     C                   ENDIF
042400991215     C                   else
042500991215     C                   SETON                                        56
042600991215     C                   MOVEL     MSG(16)       DSMSMS
042700991215     C                   ENDIF
042800991217     C                   ELSE
042900991217     C                   MOVEL     *blanks       VIHIVA
043000991217     C                   MOVEL     *blanks       VIDRAG
043100991215     C                   ENDIF
043200991215     C                   ENDIF
043300000306     C*
043400000306     C     ERRL00        IFEQ      '0'
043500000306     C     VIDKSC        ANDNE     *blanks
043600000306     C     VIDKSC        ANDNE     *zeros
043700000306     C                   MOVEL     VIDKSC        FILCLI_n          3 0
043800000306     C                   MOVEL     VIDKSC        FILCLI_a          3
043900000306     C     VIDFIL        IFNE      *blanks
044000000306     C     VIDFIL        ANDNE     *zeros
044100000306     C     FILCLI_a      IFNE      VIDFIL
044200000306     C                   SETON                                        5659
044300000306     C                   MOVEL     '1'           ERRL00
044400000810     C                   MOVEL     MSG(7)        DSMSMS
044500000306     C                   MOVEL     *blanks       VIHIVA
044600000306     C                   ENDIF
044700000810     C                   ELSE
044800020903     C     DUTLPO        IFEQ      '1'
044900000810     C                   SETOFF                                           28
045000020903     C     FILCLI_n      LOOKUP    L1                                     28
045100000810IF  3C     *IN28         IFEQ      *OFF                                         *CLIENTE NON DELL'AR
045200000810     C                   SETON                                        56
045300000810     C                   MOVEL     '1'           ERRL00
045400000810     C                   MOVEL     MSG(8)        DSMSMS
045500000810     C                   MOVEL     *blanks       VIHIVA
045600000810     C                   ENDIF
045700000810     C                   ENDIF
045800020903     C     DUTLPO        IFEQ      '2'
045900020903     C     FILCLI_n      IFNE      DUTPOU
046000000810     C                   MOVEL     '1'           ERRL00
046100000810     C                   MOVEL     MSG(8)        DSMSMS
046200000810     C                   MOVEL     *blanks       VIHIVA
046300000810     C                   ENDIF
046400000810     C                   ENDIF
046500000306     C*
046600000306     C                   ENDIF
046700000306     C                   ENDIF
046800991214     C*--------------------
046900000830     C* TIPO LEGAME
047000991214     C*--------------------
047100000306     C                   move      *blank        wtlecu
047200000306     C                   move      *blank        wtlece
047300991008IF  1C     ERRL00        IFEQ      '0'
047400000829IF  2C     VIDtle        IFNE      *blank
047500000306     C                   exsr      cnttle
047600991008X   2C                   ELSE
047700991008     C                   MOVEL     '1'           ERRL00
047800991214     C                   SETON                                        53
047900991011     C                   MOVEL     MSG(12)       DSMSMS
048000991008E   2C                   ENDIF
048100991008E   1C                   ENDIF
048200000829     C*                  ENDIF
048300991015     C*--------------------
048400991015     C* DATA DECORRENZA
048500991015     C*--------------------
048600991015     C     *LIKE         DEFINE    VIDDDE        DDEVID
048700991015IF  1C     ERRL00        IFEQ      '0'
048800991015     C                   Z-ADD     VIDDDE        wda8
048900991015     C                   Z-ADD     *ZEROS        da8w
049000991015     C                   EXSR      CHKDA8
049100991220     C                   Z-ADD     WDA8          VIDDDE
049200991015     C                   Z-ADD     DA8W          ddevid
049300991015IF  2C     ERRL00        IFEQ      '1'
049400991214     C                   SETON                                        54
049500991015     C                   MOVEL     MSG(14)       dsmsms
049600991015E   2C                   ENDIF
049700991015E   1C                   ENDIF
049800970528     C*--------------------
049900970528     C* DATA SCADENZA
050000970528     C*--------------------
050100970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
050200970528IF  1C     ERRL00        IFEQ      '0'
050300991220     C                   Z-ADD     VIDDSC        wda8
050400991220     C                   Z-ADD     *ZEROS        da8w
050500970528     C                   EXSR      CHKDA8
050600970528     C                   Z-ADD     WDA8          VIDDSC
050700991220     C                   Z-ADD     DA8W          dscvid
050800970528IF  2C     ERRL00        IFEQ      '1'
050900991214     C                   SETON                                        55
051000970528     C                   MOVEL     MSG(14)       DSMSMS
051100970528E   2C                   ENDIF
051200970528E   1C                   ENDIF
051300970528     C* SCADENZA > DECORRENZA
051400970528IF  1C     ERRL00        IFEQ      '0'
051500970528IF  2C     DSCVID        IFLT      DDEVID
051600991214     C                   SETON                                        55
051700970528     C                   MOVEL     MSG(15)       DSMSMS
051800970528     C                   MOVEL     '1'           ERRL00
051900970528E   2C                   ENDIF
052000970528E   1C                   ENDIF
052100970528     C* SCADENZA >= DATA CORRENTE
052200970528IF  1C     ERRL00        IFEQ      '0'
052300970528IF  3C     DSCVID        IFLT      DATCOR
052400991214     C                   SETON                                        55
052500970528     C                   MOVEL     MSG(16)       DSMSMS
052600970528     C                   MOVEL     '1'           ERRL00
052700970528E   3C                   ENDIF
052800970528E   2C                   ENDIF
052900991215     C*--------------------
053000000306     C* LANCIO
053100991215     C*--------------------
053200991215IF  1C     ERRL00        IFEQ      '0'
053300991215     C     VIDFLG        IFEQ      *blanks
053400991215     C                   MOVEL     '1'           ERRL00
053500991215     C                   SETON                                        57
053600991215     C                   MOVEL     MSG(18)       DSMSMS
053700991215E   2C                   ENDIF
053800991215E   1C                   ENDIF
053900000303     C*--------------------
054000000303     C* SOLO ANOMALIE
054100000303     C*--------------------
054200000303IF  1C     ERRL00        IFEQ      '0'
054300000303     C     VIDANO        IFEQ      *blanks
054400000303     C                   MOVEL     '1'           ERRL00
054500000303     C                   SETON                                        58
054600000303     C                   MOVEL     MSG(21)       DSMSMS
054700000303E   2C                   ENDIF
054800000303E   1C                   ENDIF
054900970618     C*
055000000310     C*--------------------
055100000310     C* CONTROLLO ABILITAZIONI
055200000310     C*--------------------
055300000310IF  1C     ERRL00        IFEQ      '0'
055400000310     C     VIDABI        IFEQ      *blanks
055500000310     C                   MOVEL     '1'           ERRL00
055600000310     C                   SETON                                        61
055700000310     C                   MOVEL     MSG(21)       DSMSMS
055800000310E   2C                   ENDIF
055900000310E   1C                   ENDIF
056000000306     C*
056100970528     C                   ENDSR
056200000810     C*------------------------------------------------------------------------*
056300000306     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
056400000306     C*------------------------------------------------------------------------*
056500000306     C     CHKLIN        BEGSR
056600000306     C*
056700000306     C* CONTROLLO SE E' STATO INSERITO '?'
056800000306     C     '?'           SCAN      WLIN          A                 1 0    99
056900000306     C*
057000000306     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
057100000306IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
057200000306     C                   MOVEL     *BLANKS       §COD1             3
057300000306     C                   MOVE      *BLANKS       §DES1
057400000306     C                   MOVE      *BLANKS       §PRA
057500000306     C                   CALL      'TNSD24R'
057600000306     C                   PARM                    §COD1
057700000306     C                   PARM                    §TIP              1
057800000306     C                   PARM                    §DES1            25
057900000306     C                   PARM                    §PRA              3
058000000306     C                   MOVEL     §COD1         WLIN              3
058100000306     C                   MOVEL     §DES1         WDLIN            20
058200000306     C*
058300000306X   1C                   ENDIF
058400000306     C*
058500000306     C* CARATTERI ALFABETICI NON VALIDI
058600000306IF  2C     WLIN          IFLT      *ZEROS
058700000306     C                   MOVEL     '1'           ERRL00
058800000306     C                   MOVEL     MSG(13)       DSMSMS
058900000306     C                   MOVEL     *ALL'*'       WDLIN
059000000306X   2C                   ELSE
059100000306     C*
059200000306     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
059300000306     C                   MOVEL     WLIN          CODFIL            3 0
059400000306     C     CODFIL        CHAIN     AZORG01L                           99
059500000306IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
059600000306     C                   MOVEL     ORGDES        WDLIN
059700000306X   3C                   ELSE                                                   <-- 2 -->
059800000306     C                   MOVEL     '1'           ERRL00
059900000306     C                   MOVEL     MSG(13)       DSMSMS
060000000306     C                   MOVEL     *ALL'*'       WDLIN
060100000306E   3C                   END                                                    <-- 2 ---
060200000306IF  3C     ORGFAG        IFEQ      'V'                                          --- 2 -->
060300000306     C                   MOVEL     '1'           ERRL00
060400000306     C                   MOVEL     MSG(13)       DSMSMS
060500000306     C                   MOVEL     *ALL'*'       WDLIN
060600000306E   3C                   ENDIF                                                  <-- 2 ---
060700000306E   2C                   ENDIF                                                  <-- 2 ---
060800000306     C*
060900000306     C     ERRL00        IFNE      '1'
061000000810     C                   SETOFF                                           28
061100020903     C     DUTPOU        LOOKUP    L1                                     28
061200020903     C     DUTLPO        IFNE      'S'
061300000810     C     *IN28         ANDEQ     *OFF
061400000306     C                   MOVEL     '1'           ERRL00
061500000306     C                   MOVEL     MSG(9)        DSMSMS
061600000306     C                   MOVEL     *ALL'*'       WDLIN
061700000306     C                   ENDIF                                                  <-- 2 ---
061800000810     C                   ENDIF                                                  <-- 2 ---
061900000306     C*
062000000306     C                   ENDSR
062100991008     C*------------------------------------------------------------------------*
062200000810     C* Cnttle - CONTROLLA codice tipo legame
062300991008     C*------------------------------------------------------------------------*
062400991214     C     cnttle        begsr
062500991214     C                   clear                   TIBS02DS
062600991214     C*
062700991008     C     '?'           SCAN      Vidtle                                 30
062800991214     D*  Ricerca
062900991214     C                   eval      t02tla = 'L'
063000991008    1C                   IF        *IN30 = *ON
063100991008     c                   movel     'R'           t02mod
063200991008     c                   else
063300991008     c                   movel     'C'           t02mod
063400991008     c                   movel(p)  vidtle        t02ke1
063500991008     c                   end
063600991008     C                   movel     'TLE'         t02cod
063700991008     c                   movel     knsif         t02sif
063800991008     C                   CALL      'TIBS02R'
063900991008     c                   parm                    KPJBA
064000991008     c                   parm                    TIBS02DS
064100991008     c
064200000829     C*
064300991008    2C                   IF        T02err = *blanks
064400000829     C                   movel     T02ke1        Vidtle
064500000829     C                   movel     T02uni        dtle
064600000807     C                   exsr      chkaut
064700011206     C                   if        errl00 = '0'
064800991014     C                   movel     §tledesc      Vddtle
064900991014     C                   movel     §tlecu        Wtlecu            1
065000991014     C                   movel     §tlece        wtlece            1
065100000807     C                   endif
065200991014     c                   else
065300991014     c                   movel     '1'           errl00
065400991014     c                   seton                                        66
065500991014     C                   MOVEL     MSG(12)       DSMSMS
065600991008     C                   ENDIF
065700991008     c
065800991008     c                   endsr
065900000807     C*------------------------------------------------------------------------*
066000000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
066100000807     C*------------------------------------------------------------------------*
066200000807     C     CHKAUT        BEGSR
066300000807     C*
066400000807     C* - controllo su "operabilità" di TIPO LEGAME -
066500000807     C                   eval      tipprf =  %subst(%trim(KNMUS):1:3)
066600020903     C     DUTLPO        ifeq      'S'
066700000807     C* X SEDE
066800000807     C     ERRL00        ifeq      '0'
066900000807     C                   movel     'TLE'         KTBcod
067000000807     C                   movel(P)  vidtle        KTBky1
067100000807     C     KEYTBE        chain     tntbe01l                           70
067200000807     C     *in70         ifeq      *off
067300000807     C     tbeatb        andeq     *blanks
067400000807     C                   movel     tbeuni        dtle
067500000807     C                   if        §tleaus = 'S' or
067600000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
067700000807     C                   else
067800000810     C                   SETON                                        53
067900000807     C                   MOVEL     '1'           ERRL00
068000000807     C                   MOVEL     MSG(22)       DSMSMS
068100000807     C                   endif
068200000807     C                   else
068300000807     C                   MOVEL     '1'           ERRL00
068400000807     C                   MOVEL     MSG(12)       DSMSMS
068500000807     C                   endif
068600000807     C                   endif
068700000807     C                   else
068800000807     C* X FILIALE
068900000807     C     ERRL00        ifeq      '0'
069000000807     C                   movel     'TLE'         KTBcod
069100000807     C                   movel(P)  vidtle        KTBky1
069200000807     C     KEYTBE        chain     tntbe01l                           70
069300000807     C     *in70         ifeq      *off
069400000807     C     tbeatb        andeq     *blanks
069500000807     C                   movel     tbeuni        dtle
069600000807     C                   if        §tleaut = 'S' or
069700000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
069800000807     C                   else
069900000810     C                   SETON                                        53
070000000807     C                   MOVEL     '1'           ERRL00
070100000807     C                   MOVEL     MSG(22)       DSMSMS
070200000807     C                   endif
070300000807     C                   else
070400000807     C                   MOVEL     '1'           ERRL00
070500000807     C                   MOVEL     MSG(12)       DSMSMS
070600000807     C                   endif
070700000807     C                   endif
070800000807     C                   endif
070900000807     C*
071000000807     C                   ENDSR
071100970528     C*------------------------------------------------------------------------*
071200970528     C* CHKDA8 - CONTROLLA LA DATA (8)
071300970528     C*------------------------------------------------------------------------*
071400970528     C     CHKDA8        BEGSR
071500970528     C*
071600970528     C     *LIKE         DEFINE    VIDDDE        WDA8
071700970528     C     *LIKE         DEFINE    VIDDDE        DA8W
071800970528     C*
071900970528     C                   Z-ADD     WDA8          G08DAT
072000970528     C                   Z-ADD     *ZEROS        G08INV
072100970528     C                   MOVEL     '0'           G08ERR
072200970528     C                   CALL      'XSRDA8'
072300970528     C                   PARM                    WLBDA8
072400970528     C                   Z-ADD     G08DAT        WDA8
072500970528     C                   Z-ADD     G08INV        DA8W
072600970528IF  1C     G08ERR        IFEQ      '1'
072700970528     C                   MOVEL     '1'           ERRL00
072800970528E   1C                   ENDIF
072900970528     C*
073000970528     C                   ENDSR
073100970526     C*------------------------------------------------------------------------*
073200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
073300970526     C*------------------------------------------------------------------------*
073400970526     C     AZZVID        BEGSR
073500970526     C* CAMPI
073600991214     C                   MOVEL     *BLANKS       VIDTLE
073700970528     C                   Z-ADD     *ZEROS        VIDDDE
073800970528     C                   Z-ADD     *ZEROS        VIDDSC
073900991216     C                   MOVEL     *BLANKS       VIDKSC
074000991216     C                   MOVEL     *BLANKS       VIHIVA
074100970526     C* DECODIFICHE
074200991214     C                   MOVEL     *BLANKS       VDDTLE
074300000829     C                   MOVEL     *BLANKS       VDDFIL
074400991216     C                   MOVEL     *BLANKS       VIDRAG
074500000829     C*
074600000829     C* VALORIZZAZIONE VARIABILI DESCRIZIONI A VIDEO
074700020903     C     DUTLPO        IFEQ      'S'
074800000829     C                   MOVEL     DES(01)       VDDFIL
074900000829     C                   ENDIF
075000020903     C     DUTLPO        IFEQ      '1'
075100000829     C                   MOVEL     DES(02)       VDDFIL
075200000829     C                   ENDIF
075300970526     C*
075400970526     C                   ENDSR
075500000303     C*------------------------------------------------------------------------*
075600000905     C* IMPVID - IMPOSTA I DEFAULT A VIDEO
075700000303     C*------------------------------------------------------------------------*
075800000303     C     IMPVID        BEGSR
075900000303     C*
076000000905     C                   MOVEL     'S'           VIDFLG
076100000907     C                   MOVEL     'S'           VIDANO
076200020212     C                   MOVEL     'N'           VIDEXL
076300020903     C     DUTLPO        IFEQ      'S'
076400000905     C                   MOVEL     'S'           VIDABI
076500000905     C                   ELSE
076600000905     C                   MOVEL     'N'           VIDABI
076700000905     C                   ENDIF
076800020301     C                   MOVEL     'N'           VIDBLK
076900000303     C*
077000000303     C                   ENDSR
077100970526     C*------------------------------------------------------------------------*
077200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
077300970526     C*------------------------------------------------------------------------*
077400970526     C     IMPFUN        BEGSR
077500970526     C*
077600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
077700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
077800970526     C                   EXSR      FUNGES
077900970526     C*
078000970526     C                   ENDSR
078100000810     C*------------------------------------------------------------------------*
078200000810     C* CARTAB - CARICA LE TABELLE OCCORRENTI
078300000810     C*------------------------------------------------------------------------*
078400000810     C     CARTAB        BEGSR
078500020903     C*
078600020903     C* REPERISCE I DATI UTENTE
078700020903     C                   EXSR      REPDATIUTE
078800020903     C*
078900020903     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
079000020903     C                   CLEAR                   L1
079100020903     C                   CLEAR                   TRUL06DS
079200020903     C                   CLEAR                   LIA
079300020903     C                   CLEAR                   TRUL26DS
079400020903     C                   CLEAR                   SKPO
079500020903     C                   CLEAR                   SKPOT
079600020903     C*
079700020903     C* CARICA LE FILIALI IN GESTIONE
079800020903     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
079900020903     C                   EVAL      DEPKPJBU = KPJBU
080000020903     C                   MOVEL     'L'           D06TLA
080100020903     C                   MOVEL     '£1'          D06COD
080200021007     C                   MOVEL     duttfp        D06KEY
080300020903     C                   MOVEL     TRUL06DS      KPJBU
080400020903     C                   CALL      'TRUL06R'
080500020903     C                   PARM                    KPJBA
080600020903     C                   MOVEL     KPJBU         TRUL06DS
080700020903     C                   MOVEA     LIA           L1                              -P.O. terminal
080800020903     C                   EVAL      KPJBU = DEPKPJBU
080900020903     C                   ENDIF
081000020903     C*
081100020903     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
081200020906     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
081300020903     C                   ENDIF
081400020903     C*
081500020903     C                   IF        DUTLPO = 'S'                                 *SEDE
081600020903     C                   EVAL      I26TLA = 'L'
081700020903     C                   EVAL      I26TPT = 'P'
081800020903     C                   CALL      'TRUL26R'
081900020903     C                   PARM                    TRUL26DS
082000020903     C                   MOVEA     SKPO          L1                              -tutti i P.O.
082100020903     C                   ENDIF
082200000810     C*
082300000810     C                   ENDSR
082400020903     C*--------------------------------------------------------------------------------------------*
082500020903     C* REPERISCE I DATI UTENTE
082600020903     C*--------------------------------------------------------------------------------------------*
082700020903     C     REPDATIUTE    BEGSR
082800020903     C*
082900020903     C* INIZIALIZZA VARIABILI DI WRK
083000020903     C                   CLEAR                   TIBS34DS
083100020903     C                   CLEAR                   AZUTEDS
083200020903     C                   CLEAR                   DDATIUTE
083300020903     C*
083400020903     C     *DTAARA       DEFINE    §azute        azuteds
083500020903     C     *DTAARA       DEFINE    §datiute      ddatiute
083600020903     C                   IN(E)     *DTAARA
083700020903if  1C                   IF        %Error
083800020903     c                   EVAL      I34Tla = 'L'
083900020903     C                   CALL      'TIBS34R'
084000020903     C                   PARM                    Tibs34Ds
084100020903     C                   IN        *DTAARA
084200020903e   1C                   ENDIF
084300020903     C*
084400020903     C                   ENDSR
084500970526     C*------------------------------------------------------------------------*
084600970526     C* *INZSR - OPERAZIONI INIZIALI
084700970526     C*------------------------------------------------------------------------*
084800970526     C     *INZSR        BEGSR
084900991217     C*
085000991217     C     *ENTRY        PLIST
085100991217     C                   PARM                    KPJBA
085200991215     C*
085300000306     C* LETTURA cnaco31c/cnaco00f
085400991215     C     keycli        KLIST                                                  *di cnaco/ind/clp
085500991215     C                   KFLD                    kclkut            1 0          -utente
085600991215     C                   KFLD                    kclkcc            4 0          -capoconto
085700991215     C                   KFLD                    kclksc            7 0          -codice
085800991216     C                   z-add     1             kclkut
085900000807     C* LETTURA tntbe01l -
086000000807     C     KEYtbe        KLIST
086100000807     C                   KFLD                    Ktbcod                         *CODICE TABELLA
086200000807     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
086300000807     C*---
086400000807     C* VARIABILI RIFERITE AL DATA BASE
086500000807     C*---
086600000807     C     *LIKE         DEFINE    tbecod        KTBcod
086700000807     C     *LIKE         DEFINE    tbeke1        KTBky1
086800020903     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
086900970526     C*
087000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
087100970526     C     XMSGPA        PLIST
087200970526     C                   PARM                    DSMSMS
087300970526     C                   PARM                    DSMSPG
087400970526     C                   PARM                    DSMSKE
087500970526     C                   PARM                    DSMSER
087600970526     C*
087700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
087800970526     C                   CALL      'XKEY'
087900970526     C                   PARM                    DSKEY
088000970526     C*---
088100970526     C* VARIABILI NON RIFERITE AL DATA BASE
088200970526     C*---
088300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
088400991214     C                   MOVEL     *BLANKS       DEPOP1            3
088500970526     C*---
088600970526     C* VARIABILI DI CONTROLLO
088700970526     C*---
088800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
088900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
089000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
089100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
089200970526     C*
089300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
089400991214     C                   MOVEL     'TIS7K1R'     DSMSPG
089500991214     C                   MOVEL     'TIS7K1R'     MSGQUE
089600970526     C                   MOVEL     *BLANKS       MSGKEY
089700970526     C                   MOVEL     *BLANKS       DSMSMS
089800970528     C*
089900970528     C* CALCOLA LA DATA CORRENTE
090000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
090100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
090200970528     C                   Z-ADD     WN8           G08DAT
090300970528     C                   Z-ADD     *ZEROS        G08INV
090400970528     C                   MOVEL     '0'           G08ERR
090500970528     C                   CALL      'XSRDA8'
090600970528     C                   PARM                    WLBDA8
090700970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
090800000306     C*
090900000306     C* SCOMPONGO LA DATA
091000000306     C                   Z-ADD     DATCOR        DATA
091100000306     C                   Z-ADD     AAAA          VIDAAA
091200000306     C                   Z-ADD     MM            VIDMES
091300000306     C                   SETON                                        60
091400000810     C*
091500000810     C* CARICA LE TABELLE OCCORRENTI
091600000810     C                   EXSR      CARTAB
091700000303     C*
091800020903     C     DUTLPO        IFNE      'S'
091900020903     C     DUTLPO        IFEQ      '1'
092000000810     C                   SETOFF                                       80
092100000810     C                   ELSE
092200000810     C                   SETON                                        80
092300000810     C                   ENDIF
092400000306     C                   OPEN      cnaco00f
092500000330     C                   SETON                                        67
092600000303     C                   ELSE
092700020516     C                   OPEN      cnaco00f
092800000303     C                   SETOFF                                       67
092900000303     C                   ENDIF
093000000310     C*
093100000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
093200000310     C                   EXSR      AZZVID
093300000310     C                   EXSR      IMPVID
093400970526     C*
093500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
093600970526     C                   EXSR      IMPFUN
093700970526     C*
093800970526     C                   ENDSR
093900970526     C*------------------------------------------------------------------------*
094000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
094100970526     C*------------------------------------------------------------------------*
094200970526     C     FUNGES        BEGSR
094300970526     C*
094400970526     C* PULISCO CAMPI DI LAVORO
094500970526     C                   Z-ADD     0             W                 5 0
094600970526     C*
094700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
094800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
094900970526     C     1             DO        24            I
095000970526     C                   MOVEL     FUD(I)        FUDRIG
095100970526     C*
095200970526     C* PULIZIA FUNZIONI PRECEDENTI
095300970526     C                   MOVEL     *BLANKS       FUV(I)
095400970526     C*
095500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
095600970526     C* (SALTA F24)
095700970526     C     FUDATT        IFEQ      'S'
095800970526     C     FUDFUN        ANDNE     '24'
095900970526     C                   ADD       1             W
096000970526     C                   MOVEL     FUDDES        FUV(W)
096100970526     C                   END
096200970526     C                   END                                                    DO WHILE
096300970526     C*
096400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
096500970526     C* ALTRIMENTI DISATTIVO F24
096600970526     C     1             DO        24            I
096700970526     C                   MOVEL     FUD(I)        FUDRIG
096800970526     C*
096900970526     C     FUDFUN        IFEQ      '24'
097000970526     C     W             IFGT      6
097100970526     C                   MOVEL     'S'           FUDATT
097200970526     C                   ELSE
097300970526     C                   MOVEL     *BLANKS       FUDATT
097400970526     C                   END                                                    W GT 8
097500970526     C                   MOVEL     FUDRIG        FUD(I)
097600970526     C                   END
097700970526     C                   END                                                    DO WHILE
097800970526     C*
097900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
098000970526     C     W             DIV       6             I
098100970526     C                   MVR                     Z
098200970526     C*
098300970526     C     Z             IFNE      0
098400970526     C     I             ADD       1             Z
098500970526     C                   ELSE
098600970526     C                   Z-ADD     I             Z                 5 0
098700970526     C                   END
098800970526     C*
098900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
099000970526     C                   ADD       1             FUNGRU
099100970526     C     FUNGRU        IFGT      Z
099200970526     C     FUNGRU        ORLT      1
099300970526     C                   Z-ADD     1             FUNGRU
099400970526     C                   END
099500970526     C*
099600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
099700970526     C     FUNGRU        IFEQ      1
099800970526     C                   MOVEL     FUV(1)        FUN001
099900970526     C                   MOVEL     FUV(2)        FUN002
100000970526     C                   MOVEL     FUV(3)        FUN003
100100970526     C                   MOVEL     FUV(4)        FUN004
100200970526     C                   MOVEL     FUV(5)        FUN005
100300970526     C                   MOVEL     FUV(6)        FUN006
100400970526     C                   END
100500970526     C*
100600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
100700970526     C     FUNGRU        IFEQ      2
100800970526     C                   MOVEL     FUV(7)        FUN001
100900970526     C                   MOVEL     FUV(8)        FUN002
101000970526     C                   MOVEL     FUV(9)        FUN003
101100970526     C                   MOVEL     FUV(10)       FUN004
101200970526     C                   MOVEL     FUV(11)       FUN005
101300970526     C                   MOVEL     FUV(12)       FUN006
101400970526     C                   END
101500970526     C*
101600970526     C* TERZO GRUPPO DI 6 FUNZIONI
101700970526     C     FUNGRU        IFEQ      3
101800970526     C                   MOVEL     FUV(13)       FUN001
101900970526     C                   MOVEL     FUV(14)       FUN002
102000970526     C                   MOVEL     FUV(15)       FUN003
102100970526     C                   MOVEL     FUV(16)       FUN004
102200970526     C                   MOVEL     FUV(17)       FUN005
102300970526     C                   MOVEL     FUV(18)       FUN006
102400970526     C                   END
102500970526     C*
102600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
102700970526     C     FUNGRU        IFEQ      4
102800970526     C                   MOVEL     FUV(19)       FUN001
102900970526     C                   MOVEL     FUV(20)       FUN002
103000970526     C                   MOVEL     FUV(21)       FUN003
103100970526     C                   MOVEL     FUV(22)       FUN004
103200970526     C                   MOVEL     *BLANKS       FUN005
103300970526     C                   MOVEL     *BLANKS       FUN006
103400970526     C                   END
103500970526     C*
103600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
103700970526     C                   MOVEL     *BLANKS       FUN007
103800970526     C                   MOVEL     *BLANKS       FUN008
103900970526     C*
104000970526     C     1             DO        24            I
104100970526     C                   MOVEL     FUD(I)        FUDRIG
104200970526     C     FUDFUN        IFEQ      '24'
104300970526     C     FUDATT        ANDEQ     'S'
104400970526     C                   MOVEL     FUDDES        FUN008
104500970526     C                   END
104600970526     C                   END                                                    DO
104700970526     C*
104800970526     C                   ENDSR
104900970526     C*------------------------------------------------------------------------*
105000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
105100970526     C*------------------------------------------------------------------------*
105200970526     C     FUNCON        BEGSR
105300970526     C*
105400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
105500970526     C                   EXSR      FUNVAR
105600970526     C*
105700970526     C* PULISCO CAMPI DI LAVORO
105800970526     C                   Z-ADD     0             I                 5 0
105900970526     C                   Z-ADD     0             W                 5 0
106000970526     C*
106100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
106200970526     C     FUNKEY        IFNE      *BLANKS
106300970526     C     1             DO        24            I
106400970526     C                   MOVEL     FUD(I)        FUDRIG
106500970526     C*
106600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
106700970526     C     FUNKEY        IFEQ      FUDFUN
106800970526     C     FUDATT        ANDEQ     'S'
106900970526     C                   Z-ADD     I             W
107000970526     C                   END
107100970526     C                   END                                                    DO WHILE
107200970526     C*
107300970526     C* FUNZIONE NON ATTIVA (ERRORE)
107400970526     C     W             IFEQ      0
107500970526     C*
107600970526     C* EMISSIONE MESSAGGIO DI ERRORE
107700970526     C                   MOVEL     MSG(1)        DSMSMS
107800970526     C*
107900970526     C                   END                                                    W=0
108000970526     C                   END                                                    FUNKEY NE *BLANKS
108100970526     C*
108200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
108300970526     C     W             IFNE      0
108400970526     C     FUNKEY        OREQ      *BLANKS
108500970526     C     FUNKEY        IFEQ      *BLANKS
108600970526     C                   EXSR      ENTGES
108700970526     C                   END
108800970526     C     FUNKEY        IFEQ      '03'
108900970526     C                   EXSR      F03GES
109000970526     C                   END
109100970526     C     FUNKEY        IFEQ      '06'
109200970526     C                   EXSR      F06GES
109300970526     C                   END
109400970526     C     FUNKEY        IFEQ      '12'
109500970526     C                   EXSR      F12GES
109600970526     C                   END
109700970526     C     FUNKEY        IFEQ      '24'
109800970526     C                   EXSR      F24GES
109900970526     C                   END
110000970526     C                   END
110100970526     C*
110200970526     C                   ENDSR
110300970526     C*------------------------------------------------------------------------*
110400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
110500970526     C*------------------------------------------------------------------------*
110600970526     C     FUNVAR        BEGSR
110700970526     C*
110800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
110900970526     C*
111000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
111100970526     C     KEY           IFEQ      ENTER
111200970526     C                   MOVEL     *BLANKS       FUNKEY
111300970526     C                   END
111400970526     C     KEY           IFEQ      F1
111500970526     C                   MOVEL     '01'          FUNKEY
111600970526     C                   END
111700970526     C     KEY           IFEQ      F2
111800970526     C                   MOVEL     '02'          FUNKEY
111900970526     C                   END
112000970526     C     KEY           IFEQ      F3
112100970526     C                   MOVEL     '03'          FUNKEY
112200970526     C                   END
112300970526     C     KEY           IFEQ      F4
112400970526     C                   MOVEL     '04'          FUNKEY
112500970526     C                   END
112600970526     C     KEY           IFEQ      F5
112700970526     C                   MOVEL     '05'          FUNKEY
112800970526     C                   END
112900970526     C     KEY           IFEQ      F6
113000970526     C                   MOVEL     '06'          FUNKEY
113100970526     C                   END
113200970526     C     KEY           IFEQ      F7
113300970526     C                   MOVEL     '07'          FUNKEY
113400970526     C                   END
113500970526     C     KEY           IFEQ      F8
113600970526     C                   MOVEL     '08'          FUNKEY
113700970526     C                   END
113800970526     C     KEY           IFEQ      F9
113900970526     C                   MOVEL     '09'          FUNKEY
114000970526     C                   END
114100970526     C     KEY           IFEQ      F10
114200970526     C                   MOVEL     '10'          FUNKEY
114300970526     C                   END
114400970526     C     KEY           IFEQ      F11
114500970526     C                   MOVEL     '11'          FUNKEY
114600970526     C                   END
114700970526     C     KEY           IFEQ      F12
114800970526     C                   MOVEL     '12'          FUNKEY
114900970526     C                   END
115000970526     C     KEY           IFEQ      F13
115100970526     C                   MOVEL     '13'          FUNKEY
115200970526     C                   END
115300970526     C     KEY           IFEQ      F14
115400970526     C                   MOVEL     '14'          FUNKEY
115500970526     C                   END
115600970526     C     KEY           IFEQ      F15
115700970526     C                   MOVEL     '15'          FUNKEY
115800970526     C                   END
115900970526     C     KEY           IFEQ      F16
116000970526     C                   MOVEL     '16'          FUNKEY
116100970526     C                   END
116200970526     C     KEY           IFEQ      F17
116300970526     C                   MOVEL     '17'          FUNKEY
116400970526     C                   END
116500970526     C     KEY           IFEQ      F18
116600970526     C                   MOVEL     '18'          FUNKEY
116700970526     C                   END
116800970526     C     KEY           IFEQ      F19
116900970526     C                   MOVEL     '19'          FUNKEY
117000970526     C                   END
117100970526     C     KEY           IFEQ      F20
117200970526     C                   MOVEL     '20'          FUNKEY
117300970526     C                   END
117400970526     C     KEY           IFEQ      F21
117500970526     C                   MOVEL     '21'          FUNKEY
117600970526     C                   END
117700970526     C     KEY           IFEQ      F22
117800970526     C                   MOVEL     '22'          FUNKEY
117900970526     C                   END
118000970526     C     KEY           IFEQ      F23
118100970526     C                   MOVEL     '23'          FUNKEY
118200970526     C                   END
118300970526     C     KEY           IFEQ      F24
118400970526     C                   MOVEL     '24'          FUNKEY
118500970526     C                   END
118600970526     C     KEY           IFEQ      ROLLUP
118700970526     C                   MOVEL     'RU'          FUNKEY
118800970526     C                   END
118900970526     C     KEY           IFEQ      HELP
119000970526     C                   MOVEL     'HP'          FUNKEY
119100970526     C                   END
119200970526     C                   ENDSR
119300970526     C*------------------------------------------------------------------------*
119400970526     C* NOTE COSTRUZIONE SCHIERE
119500970526     C*------------------------------------------------------------------------*
119600970526     C*
119700970526     C* -MSG-MESSAGGI
119800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
119900970526     C* (NON TOCCARE MSG,1 E MSG,2)
120000970526     C*
120100970526     C* -CMD-COMANDI
120200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
120300970526     C*
120400970526     C* -OPD-OPZIONI
120500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
120600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
120700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
120800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
120900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
121000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
121100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
121200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
121300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
121400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
121500970526     C*
121600970526     C* -FUD-FUNZIONI
121700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
121800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
121900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
122000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
122100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
122200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
122300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
122400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
122500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
122600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
122700970526     O*------------------------------------------------------------------------*
122800970526** MSG - MESSAGGI
122900991214TIS7K1R- Tasto funzionale non gestito                                          1
123000991214TIS7K1R-                                                                       2
123100991214TIS7K1R-                                                                       3
123200991214TIS7K1R-                                                                       4
123300991214TIS7K1R-                                                                       5
123400991214TIS7K1R-                                                                       6
123500000306TIS7K1R- Cliente incompatibile per Filiale selezionata.                        7
123600000306TIS7K1R- Cliente non autorizzato.                                              8
123700000306TIS7K1R- Punto operativo non autorizzato.                                      9
123800991214TIS7K1R- Anno inserito non accettabile.                                        10
123900991214TIS7K1R- Mese inserito non valido.                                             11
124000991214TIS7K1R- Tipo legame errato verificarne sulla tabella l'esistenza              12
124100000306TIS7K1R- Punto operativo errato.                                               13
124200991214TIS7K1R- Data errata                                                           14
124300991214TIS7K1R- La data scadenza deve essere maggiore della decorrenza                15
124400991215TIS7K1R- Cliente inserito inesistente.                                         16
124500991215TIS7K1R- Cliente inserito sospeso.                                             17
124600991215TIS7K1R- Tipo lancio obbligatorio.                                             18
124700991216TIS7K1R- Cliente inserito è senza P.IVA o Cod.Fisc.                            19
124800000303TIS7K1R- Il codice figlio non può essere uguale al codice padre.               20
124900000303TIS7K1R- Scelta obbligatoria.                                                  21
125000000807TIS7K1R- Tipo legame non autorizzato.                                          22
125100000807TIS7K1R- Filiale non abilitata alla gestione dei clienti scelti.               23
125200000807TIS7K1R-                                                                       24
125300970526** FUD - FUNZIONI
125400970526                   -        (S/N)=N                        funz.01¬
125500970526                   -        (S/N)=N                        funz.02¬
125600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
125700970526                   -        (S/N)=N                        funz.04¬
125800970526                   -        (S/N)=N                        funz.05¬
125900991214F06=Conferma       -        (S/N)=S                        funz.06¬
126000970527                   -        (S/N)=N                        funz.07¬
126100970527                   -        (S/N)=N                        funz.08¬
126200970527                   -        (S/N)=N                        funz.09¬
126300970527                   -        (S/N)=N                        funz.10¬
126400970526                   -        (S/N)=N                        funz.11¬
126500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
126600970526                   -        (S/N)=N                        funz.13¬
126700970526                   -        (S/N)=N                        funz.14¬
126800970526                   -        (S/N)=N                        funz.15¬
126900970526                   -        (S/N)=N                        funz.16¬
127000970526                   -        (S/N)=N                        funz.17¬
127100970526                   -        (S/N)=N                        funz.18¬
127200970526                   -        (S/N)=N                        funz.19¬
127300970526                   -        (S/N)=N                        funz.20¬
127400970526                   -        (S/N)=N                        funz.21¬
127500970526                   -        (S/N)=N                        funz.22¬
127600970526                   -        (S/N)=N                        funz.23¬
127700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
127800000829** DES - DESCRIZIONI VARIE A VIDEO
127900000829Tutti i clienti
128000000829'   '=tutti i P.O. dell'area
