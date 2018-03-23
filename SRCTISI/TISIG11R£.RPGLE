000100970526     H*------------------------------------------------------------------------*
000200150202     H* Gestione anagrafico BANCALI clienti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150130     FTISIG11d  CF   E             WORKSTN INFDS(DEVDS1)
000900021211     F                                     SFILE(SIG2DMS:MSGULT)
001000150130     Fazcgb01l  UF A E           K DISK
001100150211     Fazagb01l  iF   E           K DISK
001200150211     Ffnspe01l  iF   E           K DISK
001300150210     Fcnaco00f  IF   E           K DISK
001400150212     Fazfgb02l  if   E           k DISK    rename(azfgb000:azfgb2)
001500150219     Fazfgb01l  uf a E           k DISK
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------
002000970526     D* DECODIFICHE
002100970526     D*------------
002200150210     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* MESSAGGI
002500970526     D*------------
002600150330     D MSG             S             78    DIM(21) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* DESCRIZIONI TASTI FUNZIONALI
002900970526     D*------------
003000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* FUNZIONI DA CARICARE A VIDEO
003300970526     D*------------
003400970526     D FUV             S             19    DIM(24)
003500970526     D*------------------------------------------------------------------------*
003600970526     D* INPUT
003700970526     D*------------------------------------------------------------------------*
003800970526     D*------------
003900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004000970526     D*------------
004100970526     D FUDDS           DS
004200970526     D  FUDDE1                 1      1
004300970526     D  FUDFUN                 2      3
004400970526     D  FUDDE2                 4     19
004500970526     D  FUDFI0                20     34
004600970526     D  FUDATT                35     35
004700970526     D  FUDFI1                36     80
004800970526     D  FUDDES                 1     19
004900970526     D  FUDRIG                 1     80
005000970526     D*------------
005100970526     D* DS GESTIONE TASTI FUNZIONALI
005200970526     D*------------
005300970526     D DSKEY         E DS
005400970526     D DEVDS1          DS
005500970526     D  KEY                  369    369
005600970526     D*------------
005700970526     D* DS "XMSG" - GESTIONE MESSAGGI
005800970526     D*------------
005900970526     D DSMSG         E DS
006000970528     D*------------------
006100970526     D* ARCHITETTURA
006200970526     D*------------
006300970526     D KPJBA         E DS
006400150203     D savkpjbu        s                   like(kpjbu)
006500000407     D*------------------
006600000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
006700000407     D*------------------
006800000407     D WLBDA8          DS                  INZ
006900000407     D  G08DAT                 1      8  0
007000000407     D  G08INV                 9     16  0
007100000407     D  G08ERR                17     17
007200000407     D  G08TGI                18     22  0
007300030123     D*------------
007400030123     D* DS REPERIMENTO DATI UTENTE
007500030123     D*------------
007600030123     D TIBS34DS      E DS                                                       *Profili utente
007700030123     D DDATIUTE      E DS                                                       *Dati utente
007800030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007900150212     D dute01        E DS                                                       *Utenti
008000150212     d dLat          e ds
008100150212     D*-------------------
008200150212     D TRUL31DS      E DS
008300150212     D TRUL31DS2     E DS
008400150219     d dsemail       e ds
008500150219      *
008600150212     d wabi            s                   like(UteAut)
008700150212     D ksu3            s              3
008800150224     D diffe           s              5  0
008900150224     D dataiso         s               d   datfmt(*iso)
009000150224     D dataiso1        s               d   datfmt(*iso)
009100150212     d* p.o. abilitati
009200150212     D $pog            s              3    dim(250)
009300150212     D $DIG            s              1    dim(20)
009400150212     D $ARG            s              3    dim(50)
009500030207     D*------------------
009600030207     D* DS REPERIMENTO / CONTROLLO DA TABELLE
009700030207     D*------------------
009800030207     D TIBS02DS      E DS
009900150211     D Tnta81DS      E DS
010000150210     D xclirds       E DS
010100150210     d ksa             s              4    dim(30)
010200150210     d ksc             s              7  0 dim(30)
010300150210     d                 ds
010400150210     d dsksn                   1      4p 0
010500150210     d dsksa                   1      4
010600030207     D*------------------
010700150202     d Tibs69DS      E DS                  INZ
010800150202     d DS_cnaco      E DS                  extname(CNACO00F)
010900150202     d DS_cnind      E DS                  extname(CNIND00F)
011000150202     d DS_cnclp      E DS                  extname(CNCLP00F)
011100150202     d DS_fncls      E DS                  extname(FNCLS00F)
011200000407     D*------------------
011300000407     D* DS PASSAGGIO PARAMETRI
011400000407     D*------------------
011500000407     D PARAM           DS                  INZ
011600150202     d  pop1                          3
011700150202     D  Pksu                          8
011800150202     D  Ptsc                          1
011900150302     D  Pdti                          8  0
012000150202     D  Pmsg                         80
012100970526     C*------------------------------------------------------------------------*
012200970526     C* MAIN LINES
012300970526     C*------------------------------------------------------------------------*
012400970526     C*
012500970526     C* NON CI SONO ERRORI IN ENTRATA
012600021211     C     ERRL00        IFEQ      '0'
012700970526     C*
012800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012900021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013200970526     C*
013300970526     C* EMETTE FORMATO VIDEO
013400970526     C                   EXSR      BUTFMR
013500970526     C*
013600970526     C* GESTIONE TASTI FUNZIONALI
013700970526     C                   EXSR      FUNCON
013800021211     C                   ENDDO
013900970526     C*
014000021211     C                   ENDIF
014100021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
014200021211     C                   MOVEL     PARAM         KPJBU
014300970526     C* FINE PGM
014400970526     C                   SETON                                        LR
014500970526     C*------------------------------------------------------------------------*
014600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014700970526     C*------------------------------------------------------------------------*
014800970526     C     BUTFMR        BEGSR
014900970526     C*
015000970526     C* EMETTE LA TESTATA
015100021211     C                   WRITE     SIG2D01
015200970526     C*
015300970526     C* SPEDISCE EVENTUALE MESSAGGIO
015400970526     C                   EXSR      SNDMSG
015500970526     C*
015600970526     C* EMETTE LE FUNZIONI
015700021211     C                   WRITE     SIG2D02
015800970526     C*
015900970526     C* EMETTE IL VIDEO
016000021211     C                   EXFMT     SIG2D03
016100970526     C*
016200970526     C* REIMPOSTA FLAG DI ERRORE
016300970526     C                   MOVEL     '0'           ERRL00
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016800970526     C*------------------------------------------------------------------------*
016900970526     C     SNDMSG        BEGSR
017000970526     C*
017100970526     C* PULISCE IL SFILE MESSAGGI
017200970526     C                   MOVEL     '1'           *IN40
017300021211     C                   WRITE     SIG2DMC
017400970526     C                   MOVEL     '0'           *IN40
017500970526     C*
017600970526     C* VISUALIZZA IL MESSAGGIO
017700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017800970526     C                   CALL      'XMSG'        XMSGPA
017900021211     C     DSMSER        IFEQ      *ZEROS
018000970526     C                   MOVEL     DSMSKE        MSGKEY
018100021211     C                   ENDIF
018200970526     C                   Z-ADD     1             MSGULT
018300021211     C                   WRITE     SIG2DMS
018400021211     C                   WRITE     SIG2DMC
018500970526     C*
018600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018700970526     C                   MOVEL     *BLANKS       DSMSMS
018800970526     C*
018900970526     C                   ENDSR
019000150210     C*------------------------------------------------------------------------*
019100150210     C* PASPAR - ASSEGNAZIONE VALORI PER PASSAGIO PARAMETRI
019200150210     C*------------------------------------------------------------------------*
019300150210     C     PASPAR        BEGSR
019400150210     C*
019500150210     C                   MOVEL     PARAM         KPJBU
019600150210     C*
019700150210     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* ENTGES - GESTIONE ENTER -> INVIO
020000970526     C*------------------------------------------------------------------------*
020100970526     C     ENTGES        BEGSR
020200970526     C*
020300970526     C* CONTROLLA I CAMPI IMMESSI
020400970526     C                   EXSR      DECODI
020500970526     C*
020600970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
020700150302     C*    POP1          IFEQ      'O05'
020800150302     C*                  MOVEL     *BLANKS       PMSG
020900150302     C*                  MOVEL     '1'           FINL00
021000150302     C*                  ENDIF
021100970526     C*
021200970526     C                   ENDSR
021300150210     C*------------------------------------------------------------------------*
021400150210     C* F02GES - GESTIONE F02 -> abbinati
021500150210     C*------------------------------------------------------------------------*
021600150210     C     F02GES        BEGSR
021700150210     C*
021800150210     C* IMPOSTA "PREMUTO F02"
021900150210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022000150226     c                   movel     kpjbu         savkpjbu
022100150210     C                   EXSR      PASPAR                                       *RICARICA SFILE
022200150210     C                   CALL      'TISIG12R'
022300150210     C                   PARM                    KPJBA
022400150210     C                   MOVEL     KPJBU         PARAM
022500150226     c                   movel     savkpjbu      kpjbu
022600150211     c                   except    libera
022700150210     C                   ENDIF
022800150210     C*
022900150210     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100970526     C* F03GES - GESTIONE F03 -> FINE
023200970526     C*------------------------------------------------------------------------*
023300970526     C     F03GES        BEGSR
023400970526     C*
023500970526     C* IMPOSTA "PREMUTO F03"
023600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
023700970526     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
024100970526     C*------------------------------------------------------------------------*
024200970526     C     F06GES        BEGSR
024300970526     C*
024400021211     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
024500150204     C     POP1          IFEQ      'F06'
024600021211     C                   EXSR      INSERI
024700021211     C                   ENDIF
024800150210     C     POP1          IFEQ      'F02'
024900150210     C                   EXSR      F02GES
025000150210     C                   ENDIF
025100021211     C     POP1          IFEQ      'O02'
025200021211     C                   EXSR      MODIFI
025300021211     C                   ENDIF
025400021211     C     POP1          IFEQ      'O03'
025500021211     C                   EXSR      COPIA
025600021211     C                   ENDIF
025700021211     C     POP1          IFEQ      'O04'
025800021211     C                   EXSR      ANNULL
025900021211     C                   ENDIF
026000021211     C*
026100970526     C                   ENDSR
026200970526     C*------------------------------------------------------------------------*
026300970526     C* F12GES - GESTIONE F12 -> RITORNO
026400970526     C*------------------------------------------------------------------------*
026500970526     C     F12GES        BEGSR
026600970526     C*
026700970526     C* IMPOSTA "PREMUTO F12"
026800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026900970526     C*
027000970526     C                   ENDSR
027100970526     C*------------------------------------------------------------------------*
027200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
027300970526     C*------------------------------------------------------------------------*
027400970526     C     F24GES        BEGSR
027500970526     C*
027600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
027700970526     C*
027800970526     C                   ENDSR
027900970526     C*------------------------------------------------------------------------*
028000970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
028100970526     C*------------------------------------------------------------------------*
028200970526     C     INSERI        BEGSR
028300970526     C*
028400970526     C* CONTROLLA I CAMPI IMMESSI
028500970526     C                   EXSR      DECODI
028600021211     C                   EXSR      DECODINS
028700970526     C*
028800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029000150202     C                   CLEAR                   Azcgb000
029100021211     C                   EXSR      IMPCAM
029200150202     C                   WRITE     AZcgb000
029300000407     C                   MOVEL     MSG(2)        PMSG                           *MESSAGGIO
029400150219     c     vidksu        chain     azfgb01l
029500150219     c                   if        not %found(azfgb01l)
029600150212     c                   exsr      abbinato
029700150219     c                   endif
029800970602     C*
029900970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
030000970602     C                   EXSR      AZZVID
030100021211     C                   ENDIF
030200970526     C*
030300970526     C                   ENDSR
030400970526     C*------------------------------------------------------------------------*
030500970526     C* MODIFI - OPERAZIONI DI MODIFICA
030600970526     C*------------------------------------------------------------------------*
030700970526     C     MODIFI        BEGSR
030800970526     C*
030900970526     C* CONTROLLA I CAMPI IMMESSI
031000970526     C                   EXSR      DECODI
031100150302     c     kinse         setgt     azcgb01l
031200150302     c     kinse         readpe    azcgb01l
031300150302     c                   if        not %eof(azcgb01l)
031400150302     c                   if        cgbdti > viddtig
031500150302     C                   MOVEL     '1'           ERRL00
031600150302     c                   seton                                        88
031700150302     C                   MOVEL     MSG(19)       dsmsms
031800150302     c                   endif
031900150302     c                   endif
032000970526     C*
032100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032200021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032300970619     C                   EXSR      LETCAS
032400970526     C*
032500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
032600021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032700000407     C                   EXSR      IMPCAM
032800150202     C                   UPDATE    AZcgb000
032900000407     C                   MOVEL     MSG(3)        PMSG
033000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033100021211     C                   ENDIF
033200021211     C                   ENDIF
033300970526     C*
033400970526     C                   ENDSR
033500021211     C*------------------------------------------------------------------------*
033600021211     C* COPIA - OPERAZIONI DI COPIA RECORD
033700021211     C*------------------------------------------------------------------------*
033800021211     C     COPIA         BEGSR
033900021211     C*
034000021211     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
034100021211     C                   EXSR      LETCAS
034200021211     C*
034300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034400021211     C*
034500021211     C* CONTROLLA I CAMPI IMMESSI
034600150226     c                   clear                   viddte
034700021211     C                   EXSR      DECODI
034800150219     C                   EXSR      DECODIns
034900150226     C     VIDdti        ifNE      *zeros
035000150226     C                   Z-ADD     viddti        g08dat
035100150226     C                   Z-ADD     *ZEROS        g08inv
035200150226     C                   MOVEL     '0'           G08ERR
035300150226     C                   CALL      'XSRDA8'
035400150226     C                   PARM                    WLBDA8
035500150226     C                   Z-ADD     G08dat        viddti
035600150226     C                   Z-ADD     G08inv        viddtig           8 0
035700150226      *data inizio non retroattiva
035800150226     c                   if        viddtig < datcor
035900150226     C                   MOVEL     '1'           ERRL00
036000150226     c                   seton                                        87
036100150226     C                   MOVEL     MSG(15)       dsmsms
036200150226     c                   endif
036300150226      *data inizio non maggiore di trenta giorni
036400150226     C                   MOVE      viddtig       dataiso
036500150226     c                   move      datcor        dataiso1
036600150226     C     dataiso       subdur    dataiso1      diffe:*d
036700150226     c                   if        diffe > 30
036800150226     C                   MOVEL     '1'           ERRL00
036900150226     c                   seton                                        87
037000150226     C                   MOVEL     MSG(17)       dsmsms
037100150226     c                   endif
037200150226     C                   ENDIF
037300150226     C     ERRL00        IFEQ      '0'
037400150226     C     VIDdtf        ifne      *zeros
037500150226     C                   Z-ADD     viddtf        g08dat
037600150226     C                   Z-ADD     *ZEROS        g08inv
037700150226     C                   MOVEL     '0'           G08ERR
037800150226     C                   CALL      'XSRDA8'
037900150226     C                   PARM                    WLBDA8
038000150226     C                   Z-ADD     G08dat        viddtf
038100150226     C                   Z-ADD     G08inv        viddtfg           8 0
038200150226     c                   if        viddtfg < datcor
038300150226     C                   MOVEL     '1'           ERRL00
038400150226     c                   seton                                        88
038500150226     C                   MOVEL     MSG(15)       dsmsms
038600150226     c                   endif
038700150226     c                   else
038800150226     c                   z-add     31122039      viddtf
038900150226     C                   ENDIF
039000150226     C                   ENDIF
039100150226     C     ERRL00        IFEQ      '0'
039200150226     C     VIDdtig       andgt     viddtfg
039300150226     C                   MOVEL     '1'           ERRL00
039400150226     c                   seton                                        88
039500150226     C                   MOVEL     MSG(16)       dsmsms
039600150226     C                   ENDIF
039700021211     C*
039800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039900150202     C                   CLEAR                   AZcgb000
040000021211     C                   EXSR      IMPCAM
040100150202     C                   WRITE     AZcgb000
040200021211     C                   MOVEL     MSG(5)        PMSG                           *MESSAGGIO
040300021211     C                   MOVEL     '1'           FINL00                         *FINE PGM
040400150302     c     vidksu        chain     azfgb01l
040500150302     c                   if        not %found(azfgb01l)
040600150302     c                   exsr      abbinato
040700150302     c                   endif
040800021211     C                   ENDIF
040900021211     C                   ENDIF
041000021211     C*
041100021211     C                   ENDSR
041200970526     C*------------------------------------------------------------------------*
041300970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
041400970526     C*------------------------------------------------------------------------*
041500970526     C     ANNULL        BEGSR
041600000411     C*
041700000411     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
041800000411     C                   EXSR      LETCAS
041900000411     C*
042000000411     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
042100000411     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042200150202     C                   DELETE    AZcgb000
042300000411     C                   MOVEL     MSG(4)        PMSG
042400000411     C                   MOVEL     '1'           FINL00                         *FINE PGM
042500000411     C                   ENDIF
042600000411     C*
042700970526     C                   ENDSR
042800970526     C*------------------------------------------------------------------------*
042900970619     C* LETCAS - LEGGE IL RECORD PERL'AGGIORNAMENTO
043000970526     C*------------------------------------------------------------------------*
043100970619     C     LETCAS        BEGSR
043200970526     C*
043300150224     C  n90kmodif        CHAIN     AZcgb01L                           99
043400150224     C   90kmodif        CHAIN(n)  AZcgb01L                           99
043500021211     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
043600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
043700000407     C                   MOVEL     MSG(6)        PMSG                           *MESSAGGIO
043800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
043900021211     C                   ENDIF
044000970526     C*
044100970526     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
044400970526     C*------------------------------------------------------------------------*
044500970526     C     DECODI        BEGSR
044600000407     C*
044700000407     C                   MOVEL     '0'           ERRL00
044800000407     C*
044900000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
045000150225     C                   SETOFF                                       7879
045100000407     C                   SETOFF                                       808182
045200150202     C                   SETOFF                                       838485
045300150203     C                   SETOFF                                       868788
045400030207     C*--------------------
045500150202     C* CONTROLLO ESISTENZA validità CODICE cliente unificante
045600030207     C*--------------------
045700030207     C     ERRL00        IFEQ      '0'
045800150202     C     VIDksu        ANDNE     *blanks
045900150202     C                   EXSR      CNTksu
046000030207     C                   ENDIF
046100150202     C*--------------------
046200150203     C* CONTROLLO ESISTENZA CODICE ARTICOLO IN anagrafica articolo
046300150202     C*--------------------
046400150211     c                   clear                   quale             1
046500150202     C     ERRL00        IFEQ      '0'
046600150210     C     VIDCOD1       ANDNE     *blanks
046700150210     c                   move      vidcod1       vidcod           15
046800150211     c                   move      '1'           quale
046900150202     C                   EXSR      CNTCOD
047000150210     c                   movel     vidcod        vidcod1
047100150210     c                   movel     vidcodd       vidcodd1
047200150202     C                   ENDIF
047300150210     C     ERRL00        IFEQ      '0'
047400150210     C     VIDCOD2       ANDNE     *blanks
047500150210     c                   move      vidcod2       vidcod
047600150211     c                   move      '2'           quale
047700150210     C                   EXSR      CNTCOD
047800150210     c                   movel     vidcod        vidcod2
047900150210     c                   movel     vidcodd       vidcodd2
048000150210     C                   ENDIF
048100150210     C     ERRL00        IFEQ      '0'
048200150210     C     VIDCOD3       ANDNE     *blanks
048300150210     c                   move      vidcod3       vidcod
048400150211     c                   move      '3'           quale
048500150210     C                   EXSR      CNTCOD
048600150210     c                   movel     vidcod        vidcod3
048700150210     c                   movel     vidcodd       vidcodd3
048800150210     C                   ENDIF
048900150225     C     ERRL00        IFEQ      '0'
049000150225     c     vidtsc        andeq     'B'
049100150225     c     vidsco        andlt     10
049200150225     C                   MOVEL     '1'           ERRL00
049300150225     c                   seton                                        79
049400150225     C                   MOVEL     MSG(18)       dsmsms
049500150225     c                   endif
049600150312     C     ERRL00        IFEQ      '0'
049700150312     c     vidtsc        andeq     'B'
049800150312     c     vidqta        andlt     100
049900150312     C                   MOVEL     '1'           ERRL00
050000150312     c                   seton                                        84
050100150312     C                   MOVEL     MSG(20)       dsmsms
050200150312     c                   endif
050300150210     C     ERRL00        IFEQ      '0'
050400150220     C     VIDdti        ifne      *zeros
050500150220     C                   Z-ADD     viddti        g08dat
050600150220     C                   Z-ADD     *ZEROS        g08inv
050700150220     C                   MOVEL     '0'           G08ERR
050800150220     C                   CALL      'XSRDA8'
050900150220     C                   PARM                    WLBDA8
051000150330     c                   if        g08err =  '0'
051100150220     C                   Z-ADD     G08dat        viddti
051200150224     C                   Z-ADD     G08inv        viddtig
051300150330     c                   else
051400150330     c                   seton                                        87
051500150330     C                   MOVEL     MSG(21)       dsmsms
051600150330     c                   move      '1'           errl00
051700150330     C                   ENDIF
051800150220     c                   else
051900150220     c                   clear                   viddti
052000150220     C                   ENDIF
052100150220     C     VIDdtf        ifne      *zeros
052200150220     C                   Z-ADD     viddtf        g08dat
052300150220     C                   Z-ADD     *ZEROS        g08inv
052400150220     C                   MOVEL     '0'           G08ERR
052500150220     C                   CALL      'XSRDA8'
052600150220     C                   PARM                    WLBDA8
052700150330     c                   if        g08err = '0'
052800150220     C                   Z-ADD     G08dat        viddtf
052900150224     C                   Z-ADD     G08inv        viddtfg
053000150330     c                   else
053100150330     c                   seton                                        88
053200150330     C                   MOVEL     MSG(21)       dsmsms
053300150330     c                   move      '1'           errl00
053400150330     C                   ENDIF
053500150304      *data fine non retroattiva
053600150304     C                   IF        errl00 = '0' and cgbdtf > 0
053700150304     c                   if        viddtfg <> cgbdtf and
053800150304     c                             viddtfg < datcor
053900150304     C                   MOVEL     '1'           ERRL00
054000150304     c                   seton                                        88
054100150304     C                   MOVEL     MSG(15)       dsmsms
054200150304     c                   endif
054300150304     c                   endif
054400150220     c                   else
054500150220     c                   clear                   viddtf
054600150220     C                   ENDIF
054700150210     C     VIDdte        ifne      *zeros
054800150210     C                   Z-ADD     viddte        g08dat
054900150210     C                   Z-ADD     *ZEROS        g08inv
055000150210     C                   MOVEL     '0'           G08ERR
055100150210     C                   CALL      'XSRDA8'
055200150210     C                   PARM                    WLBDA8
055300150210     C                   Z-ADD     G08dat        viddte
055400150210     c                   else
055500150210     c                   clear                   viddte
055600150210     C                   ENDIF
055700150210     C                   ENDIF
055800150310      * verifico se data inizio maggiore di data fine se la fine è inserita
055900150224     C     ERRL00        IFEQ      '0'
056000150310     C     viddtfg       andgt     0
056100150224     C     VIDdtig       andgt     viddtfg
056200150224     C                   MOVEL     '1'           ERRL00
056300150224     c                   seton                                        88
056400150224     C                   MOVEL     MSG(16)       dsmsms
056500150224     C                   ENDIF
056600150210      * Luogo invio fattura
056700150226     c     sopra         tag
056800150309     C     ERRL00        IFEQ      '0'
056900150210     c                   move      vidksu        kspecli
057000150210     c                   setoff                                       89
057100150210     c     kFnspe        Chain     Fnspe01l
057200150210     c                   If        %Found(Fnspe01l) and speflg = *Blanks
057300150210     c                   Eval      *In89 = *On
057400150210     c                   EndIf
057500150211     c                   if        vflgluo <> *blank
057600150305     c                   move      '5'           I81OPZ
057700150219     c  n90
057800150219     can 89              move      '2'           I81OPZ
057900150219     c   90
058000150219     can 89              move      '5'           I81OPZ
058100150304     c  n90
058200150304     cann89              move      ' '           I81OPZ
058300150211     c                   move      '888'         I81COD
058400150211     c                   move      vidksu        I81CLI
058500150211     c                   move      '1'           I81GCLI
058600150211     c                   move      '1'           I81ICLI
058700150211     c                   call      'TNTA81R'
058800150211     c                   Parm                    Kpjba
058900150211     c                   Parm                    Tnta81ds
059000150225     c                   clear                   vflgluo
059100150226     c                   goto      sopra
059200150211     c                   endif
059300150309     c                   endif
059400150219     c                   If        vidmail <> *blanks
059500150219     c                   clear                   DSEMAIL
059600150219     c                   movel     vidmail       §emlindi
059700150219     c                   call      'XEMAIL'
059800150219     c                   parm                    DSEMAIL
059900150219      * errore
060000150219     c                   If        §emlerro = '1'
060100150219     c                   movel     §emlmsgo      dsmsms
060200150219     C                   MOVEL     '1'           ERRL00
060300150219     c                   seton                                        85
060400150219     c                   else
060500150219     c                   movel     §emlindo      vidmail
060600150219     c                   endif
060700150219     c                   endif
060800000407     C*--------------------
060900000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
061000000407     C*--------------------
061100000407     C     ERRL00        IFEQ      '0'
061200150202     C     VIDksu        IFEQ      *blanks
061300150219     C     VIDksu        oreq      *zeros
061400150202     C                   MOVEL     '1'           ERRL00
061500150202     C                   SETON                                        83
061600150202     C                   ENDIF
061700150210     C     VIDCOD1       IFEQ      *blanks
061800150210     C     VIDCOD2       andeq     *blanks
061900150210     C     VIDCOD3       andeq     *blanks
062000000407     C                   MOVEL     '1'           ERRL00
062100150212     C                   SETON                                        80
062200000407     C                   ENDIF
062300150202     C     VIDtsc        IFEQ      *blanks
062400000407     C                   MOVEL     '1'           ERRL00
062500000407     C                   SETON                                        82
062600000407     C                   ENDIF
062700150312     C     VIDqta        IFEQ      *zeros
062800150312     C     VIDtsc        andeq     'B'
062900150312     C                   MOVEL     '1'           ERRL00
063000150312     C                   SETON                                        84
063100150312     C                   ENDIF
063200150202     C     VIDdti        IFEQ      *zeros
063300150202     C                   MOVEL     '1'           ERRL00
063400150202     C                   SETON                                        87
063500150202     C                   ENDIF
063600150202     C     VIDdtf        IFEQ      *zeros
063700150202     C                   MOVEL     '1'           ERRL00
063800150202     C                   SETON                                        88
063900150202     C                   ENDIF
064000150202     C     VIDfre        IFEQ      *zeros
064100150202     C                   MOVEL     '1'           ERRL00
064200150202     C                   SETON                                        86
064300150202     C                   ENDIF
064400150202     C     VIDmail       IFEQ      *blanks
064500150202     C                   MOVEL     '1'           ERRL00
064600150202     C                   SETON                                        85
064700150202     C                   ENDIF
064800000407     C     ERRL00        IFEQ      '1'
064900000407     C                   MOVEL     MSG(12)       DSMSMS
065000000407     C                   ENDIF
065100000407     C                   ENDIF
065200000407     C*
065300970528     C                   ENDSR
065400021211     C*------------------------------------------------------------------------*
065500021211     C* DECODINS - CONTROLLA CAMPI E DECODIFICA X INSERIMENTO
065600021211     C*------------------------------------------------------------------------*
065700021211     C     DECODINS      BEGSR
065800021211     C*
065900150310     C*m                 MOVEL     '0'           ERRL00
066000021211     C*
066100021211     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
066200150211     C                   SETOFF                                       7879
066300021211     C                   SETOFF                                       808182
066400150203     C                   SETOFF                                       838485
066500150203     C                   SETOFF                                       868788
066600021211     C*--------------------
066700021211     C* CONTROLLO GIA' ESISTENZA CODICE ARTICOLO
066800021211     C*--------------------
066900021211     C     ERRL00        IFEQ      '0'
067000150204     C     POP1          IFEQ      'F06'
067100021211     C     POP1          OREQ      'O03'
067200150203     C     kinse         setgt     AZcgb01L                           99
067300150203     C     kinse         readpe    AZcgb01L                               99
067400150226     C                   IF        not %eof(azcgb01l)
067500021211     C                   MOVEL     '1'           ERRL00
067600150226     C                   SETON                                        80
067700021211     C                   MOVEL     MSG(7)        DSMSMS
067800150304     C*                  endif
067900150212     C                   endif
068000150219     C     vidksu        setll     AZfgb02L
068100150219     c                   do        *hival
068200150219     C     vidksu        reade     AZfgb02L
068300150219     C                   IF        %eof(azfgb02l)
068400150219     c                   leave
068500150219     c                   endif
068600150226     c                   if        fgbksu = fgbksc or
068700150226     c                             fgbann = 'A'
068800150219     c                   iter
068900150219     c                   endif
069000150212     C                   MOVEL     '1'           ERRL00
069100150212     C                   SETON                                        83
069200150212     C                   MOVEL     MSG(14)       DSMSMS
069300150219     c                   enddo
069400021211     C                   ENDIF
069500021211     C                   ENDIF
069600150310      * controlli delle date
069700150310     C     ERRL00        IFEQ      '0'
069800150310     C     VIDdti        ANDNE     *zeros
069900150310     C                   Z-ADD     viddti        g08dat
070000150310     C                   Z-ADD     *ZEROS        g08inv
070100150310     C                   MOVEL     '0'           G08ERR
070200150310     C                   CALL      'XSRDA8'
070300150310     C                   PARM                    WLBDA8
070400150310     C                   Z-ADD     G08dat        viddti
070500150310     C                   Z-ADD     G08inv        viddtig           8 0
070600150310      *data inizio non retroattiva
070700150310     c                   if        viddtig < datcor
070800150310     C                   MOVEL     '1'           ERRL00
070900150310     c                   seton                                        87
071000150310     C                   MOVEL     MSG(15)       dsmsms
071100150310     c                   endif
071200150310      *data inizio non maggiore di trenta giorni
071300150310     C                   MOVE      viddtig       dataiso
071400150310     c                   move      datcor        dataiso1
071500150310     C     dataiso       subdur    dataiso1      diffe:*d
071600150310     c                   if        diffe > 30
071700150310     C                   MOVEL     '1'           ERRL00
071800150310     c                   seton                                        87
071900150310     C                   MOVEL     MSG(17)       dsmsms
072000150310     c                   endif
072100150310     C                   ENDIF
072200150310     C     ERRL00        IFEQ      '0'
072300150310     C     VIDdtf        ifne      *zeros
072400150310     C                   Z-ADD     viddtf        g08dat
072500150310     C                   Z-ADD     *ZEROS        g08inv
072600150310     C                   MOVEL     '0'           G08ERR
072700150310     C                   CALL      'XSRDA8'
072800150310     C                   PARM                    WLBDA8
072900150310     C                   Z-ADD     G08dat        viddtf
073000150310     C                   Z-ADD     G08inv        viddtfg           8 0
073100150310     c                   if        viddtfg < datcor
073200150310     C                   MOVEL     '1'           ERRL00
073300150310     c                   seton                                        88
073400150310     C                   MOVEL     MSG(15)       dsmsms
073500150310     c                   endif
073600150310     c                   else
073700150310     c                   z-add     31122039      viddtf
073800150310     C                   ENDIF
073900150310     C                   ENDIF
074000150310      *
074100150310     C     ERRL00        IFEQ      '0'
074200150310     C     VIDdtig       andgt     viddtfg
074300150310     C                   MOVEL     '1'           ERRL00
074400150310     c                   seton                                        88
074500150310     C                   MOVEL     MSG(16)       dsmsms
074600150310     C                   ENDIF
074700150309      *controllo obbligatorietà
074800150309     C     ERRL00        IFEQ      '0'
074900150309     C     VIDksu        IFEQ      *blanks
075000150309     C     VIDksu        oreq      *zeros
075100150309     C                   MOVEL     '1'           ERRL00
075200150309     C                   SETON                                        83
075300150309     C                   ENDIF
075400150309     C     VIDCOD1       IFEQ      *blanks
075500150309     C     VIDCOD2       andeq     *blanks
075600150309     C     VIDCOD3       andeq     *blanks
075700150309     C                   MOVEL     '1'           ERRL00
075800150309     C                   SETON                                        80
075900150309     C                   ENDIF
076000150309     C     VIDtsc        IFEQ      *blanks
076100150309     C                   MOVEL     '1'           ERRL00
076200150309     C                   SETON                                        82
076300150309     C                   ENDIF
076400150309     C     VIDdti        IFEQ      *zeros
076500150309     C                   MOVEL     '1'           ERRL00
076600150309     C                   SETON                                        87
076700150309     C                   ENDIF
076800150309     C     VIDdtf        IFEQ      *zeros
076900150309     C                   MOVEL     '1'           ERRL00
077000150309     C                   SETON                                        88
077100150309     C                   ENDIF
077200150309     C     VIDfre        IFEQ      *zeros
077300150309     C                   MOVEL     '1'           ERRL00
077400150309     C                   SETON                                        86
077500150309     C                   ENDIF
077600150309     C     VIDmail       IFEQ      *blanks
077700150309     C                   MOVEL     '1'           ERRL00
077800150309     C                   SETON                                        85
077900150309     C                   ENDIF
078000150309     C     ERRL00        IFEQ      '1'
078100150309     C                   MOVEL     MSG(12)       DSMSMS
078200150309     C                   ENDIF
078300150309     C                   ENDIF
078400021211     C*
078500021211     C                   ENDSR
078600030207     C*------------------------------------------------------------------------*
078700150203     C* CNTCOD - CONTROLLA PRESENZA DEL CODICE ARTICOLO IN anagrafica agb
078800030207     C*------------------------------------------------------------------------*
078900030207     C     CNTCOD        BEGSR
079000030207     C*
079100030207     C                   CLEAR                   TIBS02DS
079200150210     C                   CLEAR                   vidcodd          30
079300030207     C*
079400150210     C     '?'           SCAN      VIDCOD                                 30
079500030207     C*
079600030207     C*  Ricerca
079700030207     C                   IF        *IN30 = *ON
079800150203     C* RICHIESTO ELENCO ARTICOLI
079900150203     C                   MOVEL     KPJBU         SAVKPJBU
080000150203     C                   MOVEL(P)  'P01'         KPJBU
080100150203     C                   CALL      'TISIG1R'
080200150203     C                   PARM                    KPJBA
080300150203     C                   EVAL      VIDCOD = %subst(KPJBU:4:15)
080400150203     C                   MOVEL     SAVKPJBU      KPJBU
080500150203     C                   ELSE
080600150203     C     vidcod        chain     azagb01l
080700150203     C                   IF        %found(azagb01l)
080800150203     C                   EVAL      vidcodd = agbdes
080900150203     c                   else
081000150203     C                   MOVEL     '1'           ERRL00
081100150211     c                   select
081200150211     c                   when      quale = '1'
081300150211     C                   SETON                                        80
081400150211     c                   when      quale = '2'
081500150211     C                   SETON                                        81
081600150211     c                   when      quale = '3'
081700150211     C                   SETON                                        78
081800150211     c                   endsl
081900150203     C                   MOVEL     MSG(9)        DSMSMS
082000150203     C                   ENDIF
082100150203     C                   ENDIF
082200030207     C*
082300150211     C                   IF        %subst(vidcod:1:1) <> vidtsc
082400150211     C                   MOVEL     '1'           ERRL00
082500150211     c                   select
082600150211     c                   when      quale = '1'
082700150211     C                   SETON                                        80
082800150211     c                   when      quale = '2'
082900150211     C                   SETON                                        81
083000150211     c                   when      quale = '3'
083100150211     C                   SETON                                        78
083200150211     c                   endsl
083300150211     C                   MOVEL     MSG(11)       DSMSMS
083400150211     C                   ENDIF
083500030207     C                   ENDSR
083600150202     C*------------------------------------------------------------------------*
083700150202     C* CNTksu - CONTROLLA PRESENZA DEL CODICE ARTICOLO IN TABELLA "TBA"
083800150202     C*------------------------------------------------------------------------*
083900150202     C     CNTksu        BEGSR
084000150202     C*
084100150210     C                   CLEAR                   xclirds
084200150202     C*
084300150202     C     '?'           SCAN      VIDksu                                 30
084400150202     C*
084500150202     C*  Ricerca
084600150202     C                   IF        *IN30 = *ON
084700150210     C* RICHIESTO ELENCO CLIENTI
084800150210     C                   clear                   xCLIrds
084900150210     C***  DXCaut = OTAA1cabi;
085000150210     C                   EVAL      DXCcap = DUTkci
085100150225     C                   EVAL      DXCAUT = wabi
085200150210     C                   CALL      'XCLIR'
085300150210     C                   PARM                    xCLIrds
085400150210     C                   EVAL      ksa = DXCksc
085500150210     C                   EVAL      Dsksa = ksa(1)
085600150210     C                   EVAL      ksc(1) = dsksn
085700150212     C                   move      ksc(1)        VIDksu
085800150202     C                   ELSE
085900150304     C                   TESTN                   vidksu               30
086000150304     C                   MOVE      vidksu        W001A             1
086100150304    2C     *IN30         IFEQ      *OFF
086200150304     C     W001A         ORLT      '0'
086300150304     C                   MOVEL     '1'           ERRL00
086400150304     C                   SETON                                        83
086500150304     C                   MOVEL     MSG(08)       DSMSMS
086600150304     C                   leavesr
086700150304     c                   endif
086800150202     C*  Controllo
086900150210     C                   Z-ADD     1             ACOkut
087000150210     C                   Z-ADD     DUTKCI        ACOkcc
087100150210     C                   EVAL      ACOKsc = %dec(VIDksu : 7 :0)
087200150210     C     K03ACO00      CHAIN     cnaco00f
087300150210     C                   IF        %found(cnaco00f)
087400150210     C                   MOVEL     ACOrag        Vidksud
087500150202     C                   ELSE
087600150202     C                   MOVEL     '1'           ERRL00
087700150202     C                   SETON                                        83
087800150202     C                   MOVEL     MSG(10)       DSMSMS
087900150202     C                   ENDIF
088000150202     C                   ENDIF
088100150212     c                   eval      ksu3 = %subst(vidksu:2:3)
088200150212     c     ksu3          lookup    $pog                                   66
088300150212     c                   if        not *in66
088400150212     C                   MOVEL     '1'           ERRL00
088500150212     C                   SETON                                        83
088600150212     C                   MOVEL     MSG(13)       DSMSMS
088700150212     C                   ENDIF
088800150202     C*
088900150202     C                   ENDSR
089000970526     C*------------------------------------------------------------------------*
089100970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
089200970526     C*------------------------------------------------------------------------*
089300970526     C     IREVID        BEGSR
089400000407     C*
089500150210     C                   MOVEL     cgbCOD1       VIDCOD1
089600150210     C                   MOVEL     cgbCOD2       VIDCOD2
089700150210     C                   MOVEL     cgbCOD3       VIDCOD3
089800150212     C                   MOVE      cgbksu        VIDksu
089900150202     C                   MOVEL     cgbtsc        VIDtsc
090000150202     C                   MOVEL     cgbmail       VIDmail
090100150210     C                   MOVEL     cgbnote       VIDnote
090200150202     C                   z-add     cgbsco        VIDsco
090300150202     C                   z-add     cgbfre        VIDfre
090400150210     C                   z-add     cgbqta        VIDqta
090500150203      * data inizio validità
090600150202     C                   Z-ADD     cgbdti        G08inv
090700150202     C                   Z-ADD     *ZEROS        G08dat
090800150202     C                   MOVEL     '3'           G08ERR
090900150202     C                   CALL      'XSRDA8'
091000150202     C                   PARM                    WLBDA8
091100150202     C                   Z-ADD     G08dat        viddti
091200150203      * data fine validità
091300150202     C                   Z-ADD     cgbdtf        G08inv
091400150202     C                   Z-ADD     *ZEROS        G08dat
091500150202     C                   MOVEL     '3'           G08ERR
091600150202     C                   CALL      'XSRDA8'
091700150202     C                   PARM                    WLBDA8
091800150202     C                   Z-ADD     G08dat        viddtf
091900150203      * data ultimo movimento
092000150203     C                   Z-ADD     cgbdte        G08inv
092100150203     C                   Z-ADD     *ZEROS        G08dat
092200150203     C                   MOVEL     '3'           G08ERR
092300150203     C                   CALL      'XSRDA8'
092400150203     C                   PARM                    WLBDA8
092500150203     C                   Z-ADD     G08dat        viddte
092600970527     C*
092700970526     C                   ENDSR
092800970526     C*------------------------------------------------------------------------*
092900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
093000970526     C*------------------------------------------------------------------------*
093100970526     C     AZZVID        BEGSR
093200000407     C*
093300150210     C                   MOVEL     *BLANKS       VIDCOD1
093400150210     C                   MOVEL     *BLANKS       VIDCOD2
093500150210     C                   MOVEL     *BLANKS       VIDCOD3
093600150202     C                   MOVEL     *BLANKS       VIDksu
093700150202     C                   MOVEL     *BLANKS       VIDtsc
093800150202     C                   MOVEL     *BLANKS       VIDmail
093900150210     C                   MOVEL     *BLANKS       VIDnote
094000150210     C                   z-add     0             VIDqta
094100150202     C                   z-add     0             VIDsco
094200150202     C                   z-add     0             VIDfre
094300150202     C                   z-add     0             viddti
094400150202     C                   z-add     0             viddtf
094500150203     C                   z-add     0             viddte
094600970526     C                   ENDSR
094700000407     C*------------------------------------------------------------------------*
094800000407     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
094900000407     C*------------------------------------------------------------------------*
095000000407     C     IMPCAM        BEGSR
095100000407     C*
095200150210     C                   MOVEL     'B'           cgbTIP
095300150210     C                   MOVEL     VIDCOD1       cgbCOD1
095400150210     C                   MOVEL     VIDCOD2       cgbCOD2
095500150210     C                   MOVEL     VIDCOD3       cgbCOD3
095600150212     C                   MOVE      VIDksu        cgbksu
095700150202     C                   MOVEL     VIDtsc        cgbtsc
095800150202     C                   MOVEL     VIDmail       cgbmail
095900150210     C                   MOVEL     VIDnote       cgbnote
096000150202     C                   z-add     VIDsco        cgbsco
096100150210     C                   z-add     VIDqta        cgbqta
096200150202     C                   z-add     VIDfre        cgbfre
096300150202      *
096400150210     c                   if        viddti > 0
096500150202     C                   Z-ADD     viddti        g08dat
096600150202     C                   Z-ADD     *ZEROS        g08inv
096700150202     C                   MOVEL     '0'           G08ERR
096800150202     C                   CALL      'XSRDA8'
096900150202     C                   PARM                    WLBDA8
097000150202     C                   Z-ADD     G08inv        cgbdti
097100150210     c                   else
097200150210     C                   Z-ADD     0             cgbdti
097300150210     c                   endif
097400150202      *
097500150210     c                   if        viddtf > 0
097600150202     C                   Z-ADD     viddtf        g08dat
097700150202     C                   Z-ADD     *ZEROS        g08inv
097800150202     C                   MOVEL     '0'           G08ERR
097900150202     C                   CALL      'XSRDA8'
098000150202     C                   PARM                    WLBDA8
098100150202     C                   Z-ADD     G08inv        cgbdtf
098200150210     c                   else
098300150210     C                   Z-ADD     0             cgbdtf
098400150210     c                   endif
098500000407     C*
098600150210     c                   if        viddte > 0
098700150210     C                   Z-ADD     viddte        g08dat
098800150210     C                   Z-ADD     *ZEROS        g08inv
098900150210     C                   MOVEL     '0'           G08ERR
099000150210     C                   CALL      'XSRDA8'
099100150210     C                   PARM                    WLBDA8
099200150210     C                   Z-ADD     G08inv        cgbdte
099300150210     c                   else
099400150210     C                   Z-ADD     0             cgbdte
099500150210     c                   endif
099600150210     C                   Z-ADD     WN14          cgbdtu
099700150210     C                   move      knmus         cgbute
099800000407     C                   ENDSR
099900150212     C*___________________________________________________________________________
100000150212     C* Abbinato scrive record principale su file codici abbinati con codice abbinato uguale
100100150212     C*------------------------------------------------------------------------*
100200150212     C     abbinato      BEGSR
100300150212     C*
100400150225     C                   MOVE      *blank        fgbann
100500150212     C                   MOVE      VIDksu        fgbksu
100600150212     C                   MOVE      VIDksu        fgbksc
100700150212     C                   Z-ADD     WN14          fgbdtu
100800150212     C                   move      knmus         fgbute
100900150212     c                   write     azfgb000
101000150212     C                   ENDSR
101100970526     C*------------------------------------------------------------------------*
101200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
101300970526     C*------------------------------------------------------------------------*
101400970526     C     IMPFUN        BEGSR
101500970526     C*
101600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
101700021211     C     POP1          IFEQ      'O05'
101800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
101900970526     C                   MOVEL     'N'           FUDATT
102000970526     C                   MOVEL     FUDRIG        FUD(6)
102100021211     C                   ENDIF
102200970526     C*
102300000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
102400150204     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
102500000407     C                   EXSR      LETCAS
102600980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
102700980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
102800980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
102900000407     C                   ELSE                                                   *INSERIMENTO
103000150225     C                   MOVEL     FUD(2)        FUDRIG                         *AGGIORNAMENTO
103100150225     C                   MOVEL     'N'           FUDATT
103200150225     C                   MOVEL     FUDRIG        FUD(2)
103300970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
103400000407     C                   ENDIF
103500150226     c     pop1          ifeq      'O03'
103600150226     c                   clear                   viddte
103700150226     C                   MOVEL     FUD(2)        FUDRIG                         *AGGIORNAMENTO
103800150226     C                   MOVEL     'N'           FUDATT
103900150226     C                   MOVEL     FUDRIG        FUD(2)
104000150226     C                   ENDIF
104100970526     C*
104200150225     C* GESTIONE TASTI FUNZIONALI A VIDEO
104300150225     C                   Z-ADD     *ZEROS        FUNGRU            5 0
104400150225     C                   EXSR      FUNGES
104500150225     C*
104600021211     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
104700021211     C     POP1          IFEQ      'O02'
104800021211     C                   MOVEL     DEC(1)        VIDFUN
104900021211     C                   ENDIF
105000021211     C     POP1          IFEQ      'O03'
105100021211     C                   MOVEL     DEC(5)        VIDFUN
105200021211     C                   ENDIF
105300021211     C     POP1          IFEQ      'O04'
105400021211     C                   MOVEL     DEC(2)        VIDFUN
105500021211     C                   ENDIF
105600021211     C     POP1          IFEQ      'O05'
105700021211     C                   MOVEL     DEC(3)        VIDFUN
105800021211     C                   ENDIF
105900150204     C     POP1          IFEQ      'F06'
106000021211     C                   MOVEL     DEC(4)        VIDFUN
106100021211     C                   ENDIF
106200150210     C     POP1          IFEQ      'F02'
106300150210     C                   MOVEL     DEC(6)        VIDFUN
106400150210     C                   ENDIF
106500021211     C*
106600021211     C                   ENDSR
106700030123     C*--------------------------------------------------------------------------------------------*
106800030123     C* REPERISCE I DATI UTENTE
106900030123     C*--------------------------------------------------------------------------------------------*
107000030123     C     REPDATIUTE    BEGSR
107100030123     C*
107200030123     C* INIZIALIZZA VARIABILI DI WRK
107300030123     C                   CLEAR                   TIBS34DS
107400030123     C                   CLEAR                   AZUTEDS
107500030123     C                   CLEAR                   DDATIUTE
107600030123     C*
107700030123     C     *DTAARA       DEFINE    §azute        azuteds
107800030123     C     *DTAARA       DEFINE    §datiute      ddatiute
107900030123     C                   IN(E)     *DTAARA
108000030123     C                   IF        %Error
108100030123     C                   EVAL      I34Tla = 'L'
108200030123     C                   CALL      'TIBS34R'
108300030123     C                   PARM                    Tibs34Ds
108400030123     C                   IN        *DTAARA
108500030123     C                   ENDIF
108600150212     c                   Clear                   wabi
108700150212     c                   Clear                   dLat
108800150212
108900150212      * Verifica errori e autorità profilo
109000150212s   1c                   Select
109100150212      * se ho errori nei dati utente esco dal pgm
109200150212w   1c                   When      DutErr = 'E'
109300150212     c                   seton                                        lr
109400150212     c                   return
109500150212      * se non c'è l'abilitazione
109600150212      * --> se 1° livello, abilitazioni al terminal
109700150212      *     se 2° livello, abilitazioni al punto operativo
109800150212w   1c                   When      UteAut = *Blanks
109900150212if  2c                   If        DutLpo = '1'
110000150212     c                   Eval      wabi   = 'TP'
110100150212e   2c                   EndIf
110200150212if  2c                   If        DutLpo = '2'
110300150212     c                   Eval      wabi   = 'PO'
110400150212e   2c                   EndIf
110500150212if  2c                   If        DutLpo = 'S'
110600150212     c                   Eval      wabi   = 'AZ'
110700150212e   2c                   EndIf
110800150212      * carica le abilitazioni del profilo
110900150212x   1c                   Other
111000150212     c                   Movel     UteFaf        Dute01
111100150212     c                   If        §Utecgb <> *Blanks
111200150212     c                   Eval      wabi = §Utecgb
111300150212     c                   Else
111400150212     c                   Eval      wabi = UteAut
111500150212     c                   EndIf
111600150212e   1c                   EndSl
111700150212      * controllo se ok l'abilitazione dell'utente
111800150212     c                   Clear                   Tibs02ds
111900150212     c                   Eval      T02Mod = 'C'
112000150212     c                   Eval      T02Sif = knsif
112100150212     c                   Eval      T02Cod = 'LAT'
112200150212     c                   Movel(p)  wabi          T02Ke1
112300150212     c                   Call      'TIBS02R'
112400150212     c                   Parm                    kpjba
112500150212     c                   Parm                    Tibs02ds
112600150212if  1c                   If        T02Err = *Blanks
112700150212     c                   Eval      dLat = T02Uni
112800150212e   1c                   EndIf
112900150212      * errore o non abilitato (impossibile xchè controllato nel filtro)
113000150212if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
113100150212   x1c                   Else
113200150212      * Reperimento dei P.O. gestibili dall'utente
113300150212     c                   clear                   TRUL31DS
113400150212     c                   clear                   TRUL31DS2
113500150212     c                   eval      I31abi = wabi
113600150212     c                   eval      I31cdi = DUTdis
113700150212     c                   eval      I31car = DUTare
113800150212     c                   eval      I31cpo = DUTpou
113900150212     c                   call      'TRUL31R'
114000150212     c                   parm                    KPJBA
114100150212     c                   parm                    TRUL31DS
114200150212     c                   parm                    TRUL31DS2
114300150212if  2c                   if        O31pog > *zeros
114400150212     c                   movea     O31pog        $pog
114500150212     c                   movea     O31arg        $arg
114600150212     c                   movea     O31dig        $dig
114700150212x   2c                   else
114800150212     c* impossibile perchè controllato nel filtrp
114900150212     c                   seton                                        lr
115000150212     c                   return
115100150212e   2c                   endif
115200150212    1c                   endif
115300030123     C*
115400030123     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
115500030123     C                   MOVEL(P)  RSUT          DSFIRS           20
115600030123     C*
115700030123     C                   ENDSR
115800970526     C*------------------------------------------------------------------------*
115900970526     C* *INZSR - OPERAZIONI INIZIALI
116000970526     C*------------------------------------------------------------------------*
116100970526     C     *INZSR        BEGSR
116200970526     C*---
116300970526     C* RICEVIMENTO PARAMETRI
116400970526     C*---
116500970526     C     *ENTRY        PLIST
116600970612     C                   PARM                    KPJBA
116700000407     C                   MOVEL     KPJBU         PARAM
116800150202     C*chiave inserimento
116900150202     C     kinse         kLIST
117000150202     C                   kfld                    vidKSU
117100150204     C                   kfld                    vidTSC
117200150210     C*luoghi
117300150210     c     kFnspe        Klist
117400150210     c                   Kfld                    kspefls           1
117500150210     c                   Kfld                    kspecli           7 0
117600150210     c                   Kfld                    kspecod           3
117700150210     c                   move      'L'           kspefls
117800150210     c                   move      '888'         kspecod
117900150210
118000150202      *chiave modifica
118100150202     C     kmodif        kLIST
118200150202     C                   kfld                    pKSU
118300150204     C                   kfld                    pTSC
118400150302     C                   kfld                    pdti
118500150210     C* LETTURA cnaco00f -
118600150210     C     K03ACO00      KLIST
118700150210     C                   KFLD                    ACOkut
118800150210     C                   KFLD                    ACOkcc
118900150210     C                   KFLD                    ACOksc
119000000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
119100000407     C     POP1          IFEQ      'O05'
119200000407     C                   SETON                                        90
119300000407     C                   ENDIF
119400000407     C     POP1          IFEQ      'O02'
119500000407     C                   SETON                                        91
119600000407     C                   ENDIF
119700030123     C*
119800030123     C* REPERIMENTO DATI UTENTE
119900030123     C                   EXSR      REPDATIUTE
120000000407     C*
120100000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
120200150225     C                   SETOFF                                       7879
120300000407     C                   SETOFF                                       808182
120400150203     C                   SETOFF                                       838485
120500150203     C                   SETOFF                                       868788
120600970526     C*
120700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
120800970526     C     XMSGPA        PLIST
120900970526     C                   PARM                    DSMSMS
121000970526     C                   PARM                    DSMSPG
121100970526     C                   PARM                    DSMSKE
121200970526     C                   PARM                    DSMSER
121300970526     C*
121400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
121500970526     C                   CALL      'XKEY'
121600970526     C                   PARM                    DSKEY
121700970526     C*---
121800970526     C* VARIABILI NON RIFERITE AL DATA BASE
121900970526     C*---
122000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
122100970526     C*---
122200970526     C* VARIABILI DI CONTROLLO
122300970526     C*---
122400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
122500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
122600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
122700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
122800970526     C*
122900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
123000150202     C                   MOVEL     'TISIG11R'    DSMSPG
123100150202     C                   MOVEL     'TISIG11R'    MSGQUE
123200970526     C                   MOVEL     *BLANKS       MSGKEY
123300970526     C                   MOVEL     *BLANKS       DSMSMS
123400000407     C*
123500000407     C* CALCOLA LA DATA CORRENTE
123600000407     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
123700000407     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
123800000407     C                   Z-ADD     WN8           G08DAT
123900000407     C                   Z-ADD     *ZEROS        G08INV
124000000407     C                   MOVEL     '0'           G08ERR
124100000407     C                   CALL      'XSRDA8'
124200000407     C                   PARM                    WLBDA8
124300000407     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
124400970526     C*
124500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
124600970526     C                   EXSR      IMPFUN
124700970526     C*
124800970526     C                   ENDSR
124900970526     C*------------------------------------------------------------------------*
125000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
125100970526     C*------------------------------------------------------------------------*
125200970526     C     FUNGES        BEGSR
125300970526     C*
125400970526     C* PULISCO CAMPI DI LAVORO
125500970526     C                   Z-ADD     0             W                 5 0
125600970526     C*
125700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
125800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
125900970526     C     1             DO        24            I
126000970526     C                   MOVEL     FUD(I)        FUDRIG
126100970526     C*
126200970526     C* PULIZIA FUNZIONI PRECEDENTI
126300970526     C                   MOVEL     *BLANKS       FUV(I)
126400970526     C*
126500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
126600970526     C* (SALTA F24)
126700970526     C     FUDATT        IFEQ      'S'
126800970526     C     FUDFUN        ANDNE     '24'
126900970526     C                   ADD       1             W
127000970526     C                   MOVEL     FUDDES        FUV(W)
127100970526     C                   END
127200970526     C                   END                                                    DO WHILE
127300970526     C*
127400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
127500970526     C* ALTRIMENTI DISATTIVO F24
127600970526     C     1             DO        24            I
127700970526     C                   MOVEL     FUD(I)        FUDRIG
127800970526     C*
127900970526     C     FUDFUN        IFEQ      '24'
128000970526     C     W             IFGT      6
128100970526     C                   MOVEL     'S'           FUDATT
128200970526     C                   ELSE
128300970526     C                   MOVEL     *BLANKS       FUDATT
128400970526     C                   END                                                    W GT 8
128500970526     C                   MOVEL     FUDRIG        FUD(I)
128600970526     C                   END
128700970526     C                   END                                                    DO WHILE
128800970526     C*
128900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
129000970526     C     W             DIV       6             I
129100970526     C                   MVR                     Z
129200970526     C*
129300970526     C     Z             IFNE      0
129400970526     C     I             ADD       1             Z
129500970526     C                   ELSE
129600970526     C                   Z-ADD     I             Z                 5 0
129700970526     C                   END
129800970526     C*
129900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
130000970526     C                   ADD       1             FUNGRU
130100970526     C     FUNGRU        IFGT      Z
130200970526     C     FUNGRU        ORLT      1
130300970526     C                   Z-ADD     1             FUNGRU
130400970526     C                   END
130500970526     C*
130600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
130700970526     C     FUNGRU        IFEQ      1
130800970526     C                   MOVEL     FUV(1)        FUN001
130900970526     C                   MOVEL     FUV(2)        FUN002
131000970526     C                   MOVEL     FUV(3)        FUN003
131100970526     C                   MOVEL     FUV(4)        FUN004
131200970526     C                   MOVEL     FUV(5)        FUN005
131300970526     C                   MOVEL     FUV(6)        FUN006
131400970526     C                   END
131500970526     C*
131600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
131700970526     C     FUNGRU        IFEQ      2
131800970526     C                   MOVEL     FUV(7)        FUN001
131900970526     C                   MOVEL     FUV(8)        FUN002
132000970526     C                   MOVEL     FUV(9)        FUN003
132100970526     C                   MOVEL     FUV(10)       FUN004
132200970526     C                   MOVEL     FUV(11)       FUN005
132300970526     C                   MOVEL     FUV(12)       FUN006
132400970526     C                   END
132500970526     C*
132600970526     C* TERZO GRUPPO DI 6 FUNZIONI
132700970526     C     FUNGRU        IFEQ      3
132800970526     C                   MOVEL     FUV(13)       FUN001
132900970526     C                   MOVEL     FUV(14)       FUN002
133000970526     C                   MOVEL     FUV(15)       FUN003
133100970526     C                   MOVEL     FUV(16)       FUN004
133200970526     C                   MOVEL     FUV(17)       FUN005
133300970526     C                   MOVEL     FUV(18)       FUN006
133400970526     C                   END
133500970526     C*
133600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
133700970526     C     FUNGRU        IFEQ      4
133800970526     C                   MOVEL     FUV(19)       FUN001
133900970526     C                   MOVEL     FUV(20)       FUN002
134000970526     C                   MOVEL     FUV(21)       FUN003
134100970526     C                   MOVEL     FUV(22)       FUN004
134200970526     C                   MOVEL     *BLANKS       FUN005
134300970526     C                   MOVEL     *BLANKS       FUN006
134400970526     C                   END
134500970526     C*
134600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
134700970526     C                   MOVEL     *BLANKS       FUN007
134800970526     C                   MOVEL     *BLANKS       FUN008
134900970526     C*
135000970526     C     1             DO        24            I
135100970526     C                   MOVEL     FUD(I)        FUDRIG
135200970526     C     FUDFUN        IFEQ      '24'
135300970526     C     FUDATT        ANDEQ     'S'
135400970526     C                   MOVEL     FUDDES        FUN008
135500970526     C                   END
135600970526     C                   END                                                    DO
135700970526     C*
135800970526     C                   ENDSR
135900970526     C*------------------------------------------------------------------------*
136000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
136100970526     C*------------------------------------------------------------------------*
136200970526     C     FUNCON        BEGSR
136300970526     C*
136400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
136500970526     C                   EXSR      FUNVAR
136600970526     C*
136700970526     C* PULISCO CAMPI DI LAVORO
136800970526     C                   Z-ADD     0             I                 5 0
136900970526     C                   Z-ADD     0             W                 5 0
137000970526     C*
137100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
137200970526     C     FUNKEY        IFNE      *BLANKS
137300970526     C     1             DO        24            I
137400970526     C                   MOVEL     FUD(I)        FUDRIG
137500970526     C*
137600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
137700970526     C     FUNKEY        IFEQ      FUDFUN
137800970526     C     FUDATT        ANDEQ     'S'
137900970526     C                   Z-ADD     I             W
138000970526     C                   END
138100970526     C                   END                                                    DO WHILE
138200970526     C*
138300970526     C* FUNZIONE NON ATTIVA (ERRORE)
138400970526     C     W             IFEQ      0
138500970526     C*
138600970526     C* EMISSIONE MESSAGGIO DI ERRORE
138700970526     C                   MOVEL     MSG(1)        DSMSMS
138800970526     C*
138900970526     C                   END                                                    W=0
139000970526     C                   END                                                    FUNKEY NE *BLANKS
139100970526     C*
139200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
139300970526     C     W             IFNE      0
139400970526     C     FUNKEY        OREQ      *BLANKS
139500970526     C     FUNKEY        IFEQ      *BLANKS
139600970526     C                   EXSR      ENTGES
139700970526     C                   END
139800150210     C     FUNKEY        IFEQ      '02'
139900150210     C                   EXSR      F02GES
140000150210     C                   END
140100970526     C     FUNKEY        IFEQ      '03'
140200970526     C                   EXSR      F03GES
140300970526     C                   END
140400970526     C     FUNKEY        IFEQ      '06'
140500970526     C                   EXSR      F06GES
140600970526     C                   END
140700970526     C     FUNKEY        IFEQ      '12'
140800970526     C                   EXSR      F12GES
140900970526     C                   END
141000970526     C     FUNKEY        IFEQ      '24'
141100970526     C                   EXSR      F24GES
141200970526     C                   END
141300970526     C                   END
141400970526     C*
141500970526     C                   ENDSR
141600970526     C*------------------------------------------------------------------------*
141700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
141800970526     C*------------------------------------------------------------------------*
141900970526     C     FUNVAR        BEGSR
142000970526     C*
142100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
142200970526     C*
142300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
142400970526     C     KEY           IFEQ      ENTER
142500970526     C                   MOVEL     *BLANKS       FUNKEY
142600970526     C                   END
142700970526     C     KEY           IFEQ      F1
142800970526     C                   MOVEL     '01'          FUNKEY
142900970526     C                   END
143000970526     C     KEY           IFEQ      F2
143100970526     C                   MOVEL     '02'          FUNKEY
143200970526     C                   END
143300970526     C     KEY           IFEQ      F3
143400970526     C                   MOVEL     '03'          FUNKEY
143500970526     C                   END
143600970526     C     KEY           IFEQ      F4
143700970526     C                   MOVEL     '04'          FUNKEY
143800970526     C                   END
143900970526     C     KEY           IFEQ      F5
144000970526     C                   MOVEL     '05'          FUNKEY
144100970526     C                   END
144200970526     C     KEY           IFEQ      F6
144300970526     C                   MOVEL     '06'          FUNKEY
144400970526     C                   END
144500970526     C     KEY           IFEQ      F7
144600970526     C                   MOVEL     '07'          FUNKEY
144700970526     C                   END
144800970526     C     KEY           IFEQ      F8
144900970526     C                   MOVEL     '08'          FUNKEY
145000970526     C                   END
145100970526     C     KEY           IFEQ      F9
145200970526     C                   MOVEL     '09'          FUNKEY
145300970526     C                   END
145400970526     C     KEY           IFEQ      F10
145500970526     C                   MOVEL     '10'          FUNKEY
145600970526     C                   END
145700970526     C     KEY           IFEQ      F11
145800970526     C                   MOVEL     '11'          FUNKEY
145900970526     C                   END
146000970526     C     KEY           IFEQ      F12
146100970526     C                   MOVEL     '12'          FUNKEY
146200970526     C                   END
146300970526     C     KEY           IFEQ      F13
146400970526     C                   MOVEL     '13'          FUNKEY
146500970526     C                   END
146600970526     C     KEY           IFEQ      F14
146700970526     C                   MOVEL     '14'          FUNKEY
146800970526     C                   END
146900970526     C     KEY           IFEQ      F15
147000970526     C                   MOVEL     '15'          FUNKEY
147100970526     C                   END
147200970526     C     KEY           IFEQ      F16
147300970526     C                   MOVEL     '16'          FUNKEY
147400970526     C                   END
147500970526     C     KEY           IFEQ      F17
147600970526     C                   MOVEL     '17'          FUNKEY
147700970526     C                   END
147800970526     C     KEY           IFEQ      F18
147900970526     C                   MOVEL     '18'          FUNKEY
148000970526     C                   END
148100970526     C     KEY           IFEQ      F19
148200970526     C                   MOVEL     '19'          FUNKEY
148300970526     C                   END
148400970526     C     KEY           IFEQ      F20
148500970526     C                   MOVEL     '20'          FUNKEY
148600970526     C                   END
148700970526     C     KEY           IFEQ      F21
148800970526     C                   MOVEL     '21'          FUNKEY
148900970526     C                   END
149000970526     C     KEY           IFEQ      F22
149100970526     C                   MOVEL     '22'          FUNKEY
149200970526     C                   END
149300970526     C     KEY           IFEQ      F23
149400970526     C                   MOVEL     '23'          FUNKEY
149500970526     C                   END
149600970526     C     KEY           IFEQ      F24
149700970526     C                   MOVEL     '24'          FUNKEY
149800970526     C                   END
149900970526     C     KEY           IFEQ      ROLLUP
150000970526     C                   MOVEL     'RU'          FUNKEY
150100970526     C                   END
150200970526     C     KEY           IFEQ      HELP
150300970526     C                   MOVEL     'HP'          FUNKEY
150400970526     C                   END
150500970526     C                   ENDSR
150600970526     C*------------------------------------------------------------------------*
150700970526     C* NOTE COSTRUZIONE SCHIERE
150800970526     C*------------------------------------------------------------------------*
150900970526     C*
151000970526     C* -MSG-MESSAGGI
151100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
151200970526     C* (NON TOCCARE MSG,1 E MSG,2)
151300970526     C*
151400970526     C* -CMD-COMANDI
151500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
151600970526     C*
151700970526     C* -OPD-OPZIONI
151800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
151900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
152000970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
152100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
152200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
152300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
152400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
152500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
152600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
152700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
152800970526     C*
152900970526     C* -FUD-FUNZIONI
153000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
153100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
153200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
153300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
153400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
153500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
153600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
153700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
153800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
153900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
154000970526     O*------------------------------------------------------------------------*
154100150211     oazcgb000  e            libera
154200150211     O*_________________________________________________________________________
154300970526** DEC - DECODIFICHE
154400970526    MODIFICA                                                      1
154500970526  ANNULLAMENTO                                                    2
154600970526VISUALIZZAZIONE                                                   3
154700021211  INSERIMENTO                                                     4
154800021211     COPIA                                                        5
154900150210    GESTIONE                                                      6
155000970528** MSG - MESSAGGI
155100150202TISIG11R- Tasto funzionale non gestito                                          1
155200150202TISIG11R- Inserimento effettuato                                                2
155300150202TISIG11R- Modifica effettuata                                                   3
155400150202TISIG11R- Annullamento effettuato                                               4
155500150202TISIG11R- Copia effettuata                                                      5
155600150202TISIG11R- Richiesta funzione per record inesistente                             6
155700150305TISIG11R- Impossibile inserire, cliente/gestione già presenti nel periodo       7
155800150304TISIG11R- Carattere non valido verificare i campi a video                       8
155900150203TISIG11R- Codice articolo non presente in anagrafica articoli                   9
156000150224TISIG11R- Codice cliente raggruppamento bancali non corretto                   10
156100150211TISIG11R- Codice articolo incongruente con tipo gestione                       11
156200150203TISIG11R- Il campo è obbligatorio                                              12
156300150212TISIG11R- Codice non manutenzionabile da questo profilo                        13
156400150212TISIG11R- Codice non inseribile presente negli archivi come codice abbinato    14
156500150219TISIG11R- La Data non può essere retroattiva                                   15
156600150219TISIG11R- La Data inizio è maggiore della data fine                            16
156700150224TISIG11R- La Data inizio non può essere oltre 30 giorni la data odierna        17
156800150225TISIG11R- Per il tipo gestione "B" la franchigia deve essere minimo al 10%     18
156900150312TISIG11R- Esistono anagrafiche del cliente successive manutenzione errata      19
157000150312TISIG11R- Per il tipo gestione "B" la quantità soglia deve essere > 100        20
157100150330TISIG11R- La Data non è corretta                                               21
157200970526** FUD - FUNZIONI
157300970526                   -        (S/N)=N                        funz.01¬
157400150210F02=Abbinati       -        (S/N)=S                        funz.02¬
157500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
157600970526                   -        (S/N)=N                        funz.04¬
157700970526                   -        (S/N)=N                        funz.05¬
157800000407F06=Conferma       -        (S/N)=S                        funz.06¬
157900970527                   -        (S/N)=N                        funz.07¬
158000970527                   -        (S/N)=N                        funz.08¬
158100970527                   -        (S/N)=N                        funz.09¬
158200970527                   -        (S/N)=N                        funz.10¬
158300970526                   -        (S/N)=N                        funz.11¬
158400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
158500970526                   -        (S/N)=N                        funz.13¬
158600970526                   -        (S/N)=N                        funz.14¬
158700970526                   -        (S/N)=N                        funz.15¬
158800970526                   -        (S/N)=N                        funz.16¬
158900970526                   -        (S/N)=N                        funz.17¬
159000970526                   -        (S/N)=N                        funz.18¬
159100970526                   -        (S/N)=N                        funz.19¬
159200970526                   -        (S/N)=N                        funz.20¬
159300970526                   -        (S/N)=N                        funz.21¬
159400970526                   -        (S/N)=N                        funz.22¬
159500970526                   -        (S/N)=N                        funz.23¬
159600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
