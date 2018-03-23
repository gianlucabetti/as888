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
033300160609      *verifico se il codice su se stesso è annullato o inesistente lo inserisco o lo aggiorno
033400160609      * se non è regolare
033500160609     c                   move      vidksu        ksuf
033600160609     c     kfgb          chain     azfgb01l
033700160609     c                   if        not %found(azfgb01l)
033800160609     c                   exsr      abbinato
033900160609     c                   else
034000160609     c                   if        fgbann <> *blank
034100160609     c                   move      *blank        fgbann
034200160609     c                   update    azfgb000
034300160609     c                   endif
034400160609     c                   endif
034500970526     C*
034600970526     C                   ENDSR
034700021211     C*------------------------------------------------------------------------*
034800021211     C* COPIA - OPERAZIONI DI COPIA RECORD
034900021211     C*------------------------------------------------------------------------*
035000021211     C     COPIA         BEGSR
035100021211     C*
035200021211     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
035300021211     C                   EXSR      LETCAS
035400021211     C*
035500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035600021211     C*
035700021211     C* CONTROLLA I CAMPI IMMESSI
035800150226     c                   clear                   viddte
035900021211     C                   EXSR      DECODI
036000150219     C                   EXSR      DECODIns
036100150226     C     VIDdti        ifNE      *zeros
036200150226     C                   Z-ADD     viddti        g08dat
036300150226     C                   Z-ADD     *ZEROS        g08inv
036400150226     C                   MOVEL     '0'           G08ERR
036500150226     C                   CALL      'XSRDA8'
036600150226     C                   PARM                    WLBDA8
036700150226     C                   Z-ADD     G08dat        viddti
036800150226     C                   Z-ADD     G08inv        viddtig           8 0
036900150226      *data inizio non retroattiva
037000150226     c                   if        viddtig < datcor
037100150226     C                   MOVEL     '1'           ERRL00
037200150226     c                   seton                                        87
037300150226     C                   MOVEL     MSG(15)       dsmsms
037400150226     c                   endif
037500150226      *data inizio non maggiore di trenta giorni
037600150226     C                   MOVE      viddtig       dataiso
037700150226     c                   move      datcor        dataiso1
037800150226     C     dataiso       subdur    dataiso1      diffe:*d
037900150226     c                   if        diffe > 30
038000150226     C                   MOVEL     '1'           ERRL00
038100150226     c                   seton                                        87
038200150226     C                   MOVEL     MSG(17)       dsmsms
038300150226     c                   endif
038400150226     C                   ENDIF
038500150226     C     ERRL00        IFEQ      '0'
038600150226     C     VIDdtf        ifne      *zeros
038700150226     C                   Z-ADD     viddtf        g08dat
038800150226     C                   Z-ADD     *ZEROS        g08inv
038900150226     C                   MOVEL     '0'           G08ERR
039000150226     C                   CALL      'XSRDA8'
039100150226     C                   PARM                    WLBDA8
039200150226     C                   Z-ADD     G08dat        viddtf
039300150226     C                   Z-ADD     G08inv        viddtfg           8 0
039400150226     c                   if        viddtfg < datcor
039500150226     C                   MOVEL     '1'           ERRL00
039600150226     c                   seton                                        88
039700150226     C                   MOVEL     MSG(15)       dsmsms
039800150226     c                   endif
039900150226     c                   else
040000150226     c                   z-add     31122039      viddtf
040100150226     C                   ENDIF
040200150226     C                   ENDIF
040300150226     C     ERRL00        IFEQ      '0'
040400150226     C     VIDdtig       andgt     viddtfg
040500150226     C                   MOVEL     '1'           ERRL00
040600150226     c                   seton                                        88
040700150226     C                   MOVEL     MSG(16)       dsmsms
040800150226     C                   ENDIF
040900021211     C*
041000021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041100150202     C                   CLEAR                   AZcgb000
041200021211     C                   EXSR      IMPCAM
041300150202     C                   WRITE     AZcgb000
041400021211     C                   MOVEL     MSG(5)        PMSG                           *MESSAGGIO
041500021211     C                   MOVEL     '1'           FINL00                         *FINE PGM
041600150302     c     vidksu        chain     azfgb01l
041700150302     c                   if        not %found(azfgb01l)
041800150302     c                   exsr      abbinato
041900150302     c                   endif
042000021211     C                   ENDIF
042100021211     C                   ENDIF
042200021211     C*
042300021211     C                   ENDSR
042400970526     C*------------------------------------------------------------------------*
042500970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
042600970526     C*------------------------------------------------------------------------*
042700970526     C     ANNULL        BEGSR
042800000411     C*
042900000411     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
043000000411     C                   EXSR      LETCAS
043100000411     C*
043200000411     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
043300000411     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
043400150202     C                   DELETE    AZcgb000
043500000411     C                   MOVEL     MSG(4)        PMSG
043600000411     C                   MOVEL     '1'           FINL00                         *FINE PGM
043700000411     C                   ENDIF
043800000411     C*
043900970526     C                   ENDSR
044000970526     C*------------------------------------------------------------------------*
044100970619     C* LETCAS - LEGGE IL RECORD PERL'AGGIORNAMENTO
044200970526     C*------------------------------------------------------------------------*
044300970619     C     LETCAS        BEGSR
044400970526     C*
044500150224     C  n90kmodif        CHAIN     AZcgb01L                           99
044600150224     C   90kmodif        CHAIN(n)  AZcgb01L                           99
044700021211     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
044800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
044900000407     C                   MOVEL     MSG(6)        PMSG                           *MESSAGGIO
045000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
045100021211     C                   ENDIF
045200970526     C*
045300970526     C                   ENDSR
045400970526     C*------------------------------------------------------------------------*
045500970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
045600970526     C*------------------------------------------------------------------------*
045700970526     C     DECODI        BEGSR
045800000407     C*
045900000407     C                   MOVEL     '0'           ERRL00
046000000407     C*
046100000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
046200150225     C                   SETOFF                                       7879
046300000407     C                   SETOFF                                       808182
046400150202     C                   SETOFF                                       838485
046500150203     C                   SETOFF                                       868788
046600030207     C*--------------------
046700150202     C* CONTROLLO ESISTENZA validità CODICE cliente unificante
046800030207     C*--------------------
046900030207     C     ERRL00        IFEQ      '0'
047000150202     C     VIDksu        ANDNE     *blanks
047100150202     C                   EXSR      CNTksu
047200030207     C                   ENDIF
047300150202     C*--------------------
047400150203     C* CONTROLLO ESISTENZA CODICE ARTICOLO IN anagrafica articolo
047500150202     C*--------------------
047600150211     c                   clear                   quale             1
047700150202     C     ERRL00        IFEQ      '0'
047800150210     C     VIDCOD1       ANDNE     *blanks
047900150210     c                   move      vidcod1       vidcod           15
048000150211     c                   move      '1'           quale
048100150202     C                   EXSR      CNTCOD
048200150210     c                   movel     vidcod        vidcod1
048300150210     c                   movel     vidcodd       vidcodd1
048400150202     C                   ENDIF
048500150210     C     ERRL00        IFEQ      '0'
048600150210     C     VIDCOD2       ANDNE     *blanks
048700150210     c                   move      vidcod2       vidcod
048800150211     c                   move      '2'           quale
048900150210     C                   EXSR      CNTCOD
049000150210     c                   movel     vidcod        vidcod2
049100150210     c                   movel     vidcodd       vidcodd2
049200150210     C                   ENDIF
049300150210     C     ERRL00        IFEQ      '0'
049400150210     C     VIDCOD3       ANDNE     *blanks
049500150210     c                   move      vidcod3       vidcod
049600150211     c                   move      '3'           quale
049700150210     C                   EXSR      CNTCOD
049800150210     c                   movel     vidcod        vidcod3
049900150210     c                   movel     vidcodd       vidcodd3
050000150210     C                   ENDIF
050100150225     C     ERRL00        IFEQ      '0'
050200150225     c     vidtsc        andeq     'B'
050300150225     c     vidsco        andlt     10
050400150225     C                   MOVEL     '1'           ERRL00
050500150225     c                   seton                                        79
050600150225     C                   MOVEL     MSG(18)       dsmsms
050700150225     c                   endif
050800150312     C     ERRL00        IFEQ      '0'
050900150312     c     vidtsc        andeq     'B'
051000150312     c     vidqta        andlt     100
051100150312     C                   MOVEL     '1'           ERRL00
051200150312     c                   seton                                        84
051300150312     C                   MOVEL     MSG(20)       dsmsms
051400150312     c                   endif
051500150210     C     ERRL00        IFEQ      '0'
051600150220     C     VIDdti        ifne      *zeros
051700150220     C                   Z-ADD     viddti        g08dat
051800150220     C                   Z-ADD     *ZEROS        g08inv
051900150220     C                   MOVEL     '0'           G08ERR
052000150220     C                   CALL      'XSRDA8'
052100150220     C                   PARM                    WLBDA8
052200150330     c                   if        g08err =  '0'
052300150220     C                   Z-ADD     G08dat        viddti
052400150224     C                   Z-ADD     G08inv        viddtig
052500150330     c                   else
052600150330     c                   seton                                        87
052700150330     C                   MOVEL     MSG(21)       dsmsms
052800150330     c                   move      '1'           errl00
052900150330     C                   ENDIF
053000150220     c                   else
053100150220     c                   clear                   viddti
053200150220     C                   ENDIF
053300150220     C     VIDdtf        ifne      *zeros
053400150220     C                   Z-ADD     viddtf        g08dat
053500150220     C                   Z-ADD     *ZEROS        g08inv
053600150220     C                   MOVEL     '0'           G08ERR
053700150220     C                   CALL      'XSRDA8'
053800150220     C                   PARM                    WLBDA8
053900150330     c                   if        g08err = '0'
054000150220     C                   Z-ADD     G08dat        viddtf
054100150224     C                   Z-ADD     G08inv        viddtfg
054200150330     c                   else
054300150330     c                   seton                                        88
054400150330     C                   MOVEL     MSG(21)       dsmsms
054500150330     c                   move      '1'           errl00
054600150330     C                   ENDIF
054700150304      *data fine non retroattiva
054800150304     C                   IF        errl00 = '0' and cgbdtf > 0
054900160609     c                   if        viddtfg <> cgbdtf and
055000160609     c                             viddtfg > datcor
055100160609     c                   exsr      contrABB
055200160609     C                   if        errl00 = '1'
055300160609     c                   leavesr
055400160609     c                   endif
055500160609     c                   endif
055600150304     c                   if        viddtfg <> cgbdtf and
055700150304     c                             viddtfg < datcor
055800150304     C                   MOVEL     '1'           ERRL00
055900150304     c                   seton                                        88
056000150304     C                   MOVEL     MSG(15)       dsmsms
056100150304     c                   endif
056200150304     c                   endif
056300150220     c                   else
056400150220     c                   clear                   viddtf
056500150220     C                   ENDIF
056600150210     C     VIDdte        ifne      *zeros
056700150210     C                   Z-ADD     viddte        g08dat
056800150210     C                   Z-ADD     *ZEROS        g08inv
056900150210     C                   MOVEL     '0'           G08ERR
057000150210     C                   CALL      'XSRDA8'
057100150210     C                   PARM                    WLBDA8
057200150210     C                   Z-ADD     G08dat        viddte
057300150210     c                   else
057400150210     c                   clear                   viddte
057500150210     C                   ENDIF
057600150210     C                   ENDIF
057700150310      * verifico se data inizio maggiore di data fine se la fine è inserita
057800150224     C     ERRL00        IFEQ      '0'
057900150310     C     viddtfg       andgt     0
058000150224     C     VIDdtig       andgt     viddtfg
058100150224     C                   MOVEL     '1'           ERRL00
058200150224     c                   seton                                        88
058300150224     C                   MOVEL     MSG(16)       dsmsms
058400150224     C                   ENDIF
058500150210      * Luogo invio fattura
058600150226     c     sopra         tag
058700150309     C     ERRL00        IFEQ      '0'
058800150210     c                   move      vidksu        kspecli
058900150210     c                   setoff                                       89
059000150210     c     kFnspe        Chain     Fnspe01l
059100150210     c                   If        %Found(Fnspe01l) and speflg = *Blanks
059200150210     c                   Eval      *In89 = *On
059300150210     c                   EndIf
059400150211     c                   if        vflgluo <> *blank
059500150305     c                   move      '5'           I81OPZ
059600150219     c  n90
059700150219     can 89              move      '2'           I81OPZ
059800150219     c   90
059900150219     can 89              move      '5'           I81OPZ
060000150304     c  n90
060100150304     cann89              move      ' '           I81OPZ
060200150211     c                   move      '888'         I81COD
060300150211     c                   move      vidksu        I81CLI
060400150211     c                   move      '1'           I81GCLI
060500150211     c                   move      '1'           I81ICLI
060600150211     c                   call      'TNTA81R'
060700150211     c                   Parm                    Kpjba
060800150211     c                   Parm                    Tnta81ds
060900150225     c                   clear                   vflgluo
061000150226     c                   goto      sopra
061100150211     c                   endif
061200150309     c                   endif
061300150219     c                   If        vidmail <> *blanks
061400150219     c                   clear                   DSEMAIL
061500150219     c                   movel     vidmail       §emlindi
061600150219     c                   call      'XEMAIL'
061700150219     c                   parm                    DSEMAIL
061800150219      * errore
061900150219     c                   If        §emlerro = '1'
062000150219     c                   movel     §emlmsgo      dsmsms
062100150219     C                   MOVEL     '1'           ERRL00
062200150219     c                   seton                                        85
062300150219     c                   else
062400150219     c                   movel     §emlindo      vidmail
062500150219     c                   endif
062600150219     c                   endif
062700000407     C*--------------------
062800000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
062900000407     C*--------------------
063000000407     C     ERRL00        IFEQ      '0'
063100150202     C     VIDksu        IFEQ      *blanks
063200150219     C     VIDksu        oreq      *zeros
063300150202     C                   MOVEL     '1'           ERRL00
063400150202     C                   SETON                                        83
063500150202     C                   ENDIF
063600150210     C     VIDCOD1       IFEQ      *blanks
063700150210     C     VIDCOD2       andeq     *blanks
063800150210     C     VIDCOD3       andeq     *blanks
063900000407     C                   MOVEL     '1'           ERRL00
064000150212     C                   SETON                                        80
064100000407     C                   ENDIF
064200150202     C     VIDtsc        IFEQ      *blanks
064300000407     C                   MOVEL     '1'           ERRL00
064400000407     C                   SETON                                        82
064500000407     C                   ENDIF
064600150312     C     VIDqta        IFEQ      *zeros
064700150312     C     VIDtsc        andeq     'B'
064800150312     C                   MOVEL     '1'           ERRL00
064900150312     C                   SETON                                        84
065000150312     C                   ENDIF
065100150202     C     VIDdti        IFEQ      *zeros
065200150202     C                   MOVEL     '1'           ERRL00
065300150202     C                   SETON                                        87
065400150202     C                   ENDIF
065500150202     C     VIDdtf        IFEQ      *zeros
065600150202     C                   MOVEL     '1'           ERRL00
065700150202     C                   SETON                                        88
065800150202     C                   ENDIF
065900150202     C     VIDfre        IFEQ      *zeros
066000150202     C                   MOVEL     '1'           ERRL00
066100150202     C                   SETON                                        86
066200150202     C                   ENDIF
066300150202     C     VIDmail       IFEQ      *blanks
066400150202     C                   MOVEL     '1'           ERRL00
066500150202     C                   SETON                                        85
066600150202     C                   ENDIF
066700000407     C     ERRL00        IFEQ      '1'
066800000407     C                   MOVEL     MSG(12)       DSMSMS
066900000407     C                   ENDIF
067000000407     C                   ENDIF
067100000407     C*
067200970528     C                   ENDSR
067300021211     C*------------------------------------------------------------------------*
067400021211     C* DECODINS - CONTROLLA CAMPI E DECODIFICA X INSERIMENTO
067500021211     C*------------------------------------------------------------------------*
067600021211     C     DECODINS      BEGSR
067700021211     C*
067800150310     C*m                 MOVEL     '0'           ERRL00
067900021211     C*
068000021211     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
068100150211     C                   SETOFF                                       7879
068200021211     C                   SETOFF                                       808182
068300150203     C                   SETOFF                                       838485
068400150203     C                   SETOFF                                       868788
068500021211     C*--------------------
068600021211     C* CONTROLLO GIA' ESISTENZA CODICE ARTICOLO
068700021211     C*--------------------
068800021211     C     ERRL00        IFEQ      '0'
068900150204     C     POP1          IFEQ      'F06'
069000021211     C     POP1          OREQ      'O03'
069100150203     C     kinse         setgt     AZcgb01L                           99
069200150203     C     kinse         readpe    AZcgb01L                               99
069300150226     C                   IF        not %eof(azcgb01l)
069400021211     C                   MOVEL     '1'           ERRL00
069500150226     C                   SETON                                        80
069600021211     C                   MOVEL     MSG(7)        DSMSMS
069700150212     C                   endif
069800160609      * controllo su ABBINATI
069900160609     c                   exsr      contrABB
070000021211     C                   ENDIF
070100021211     C                   ENDIF
070200150310      * controlli delle date
070300150310     C     ERRL00        IFEQ      '0'
070400150310     C     VIDdti        ANDNE     *zeros
070500150310     C                   Z-ADD     viddti        g08dat
070600150310     C                   Z-ADD     *ZEROS        g08inv
070700150310     C                   MOVEL     '0'           G08ERR
070800150310     C                   CALL      'XSRDA8'
070900150310     C                   PARM                    WLBDA8
071000150310     C                   Z-ADD     G08dat        viddti
071100150310     C                   Z-ADD     G08inv        viddtig           8 0
071200150310      *data inizio non retroattiva
071300150310     c                   if        viddtig < datcor
071400150310     C                   MOVEL     '1'           ERRL00
071500150310     c                   seton                                        87
071600150310     C                   MOVEL     MSG(15)       dsmsms
071700150310     c                   endif
071800150310      *data inizio non maggiore di trenta giorni
071900150310     C                   MOVE      viddtig       dataiso
072000150310     c                   move      datcor        dataiso1
072100150310     C     dataiso       subdur    dataiso1      diffe:*d
072200150310     c                   if        diffe > 30
072300150310     C                   MOVEL     '1'           ERRL00
072400150310     c                   seton                                        87
072500150310     C                   MOVEL     MSG(17)       dsmsms
072600150310     c                   endif
072700150310     C                   ENDIF
072800150310     C     ERRL00        IFEQ      '0'
072900150310     C     VIDdtf        ifne      *zeros
073000150310     C                   Z-ADD     viddtf        g08dat
073100150310     C                   Z-ADD     *ZEROS        g08inv
073200150310     C                   MOVEL     '0'           G08ERR
073300150310     C                   CALL      'XSRDA8'
073400150310     C                   PARM                    WLBDA8
073500150310     C                   Z-ADD     G08dat        viddtf
073600150310     C                   Z-ADD     G08inv        viddtfg           8 0
073700150310     c                   if        viddtfg < datcor
073800150310     C                   MOVEL     '1'           ERRL00
073900150310     c                   seton                                        88
074000150310     C                   MOVEL     MSG(15)       dsmsms
074100150310     c                   endif
074200150310     c                   else
074300150310     c                   z-add     31122039      viddtf
074400150310     C                   ENDIF
074500150310     C                   ENDIF
074600150310      *
074700150310     C     ERRL00        IFEQ      '0'
074800150310     C     VIDdtig       andgt     viddtfg
074900150310     C                   MOVEL     '1'           ERRL00
075000150310     c                   seton                                        88
075100150310     C                   MOVEL     MSG(16)       dsmsms
075200150310     C                   ENDIF
075300150309      *controllo obbligatorietà
075400150309     C     ERRL00        IFEQ      '0'
075500150309     C     VIDksu        IFEQ      *blanks
075600150309     C     VIDksu        oreq      *zeros
075700150309     C                   MOVEL     '1'           ERRL00
075800150309     C                   SETON                                        83
075900150309     C                   ENDIF
076000150309     C     VIDCOD1       IFEQ      *blanks
076100150309     C     VIDCOD2       andeq     *blanks
076200150309     C     VIDCOD3       andeq     *blanks
076300150309     C                   MOVEL     '1'           ERRL00
076400150309     C                   SETON                                        80
076500150309     C                   ENDIF
076600150309     C     VIDtsc        IFEQ      *blanks
076700150309     C                   MOVEL     '1'           ERRL00
076800150309     C                   SETON                                        82
076900150309     C                   ENDIF
077000150309     C     VIDdti        IFEQ      *zeros
077100150309     C                   MOVEL     '1'           ERRL00
077200150309     C                   SETON                                        87
077300150309     C                   ENDIF
077400150309     C     VIDdtf        IFEQ      *zeros
077500150309     C                   MOVEL     '1'           ERRL00
077600150309     C                   SETON                                        88
077700150309     C                   ENDIF
077800150309     C     VIDfre        IFEQ      *zeros
077900150309     C                   MOVEL     '1'           ERRL00
078000150309     C                   SETON                                        86
078100150309     C                   ENDIF
078200150309     C     VIDmail       IFEQ      *blanks
078300150309     C                   MOVEL     '1'           ERRL00
078400150309     C                   SETON                                        85
078500150309     C                   ENDIF
078600150309     C     ERRL00        IFEQ      '1'
078700150309     C                   MOVEL     MSG(12)       DSMSMS
078800150309     C                   ENDIF
078900150309     C                   ENDIF
079000021211     C*
079100021211     C                   ENDSR
079200160609      *_________________________________________________________
079300160609     c     contrABB      begsr
079400160609      *_________________________________________________________
079500160609     C     vidksu        setll     AZfgb02L
079600160609     c                   do        *hival
079700160609     C     vidksu        reade     AZfgb02L
079800160609     C                   IF        %eof(azfgb02l)
079900160609     c                   leave
080000160609     c                   endif
080100160609     c                   if        fgbksu = fgbksc or
080200160609     c                             fgbann = 'A'
080300160609     c                   iter
080400160609     c                   endif
080500160609     C                   MOVEL     '1'           ERRL00
080600160609     C                   SETON                                        83
080700160609     C                   MOVEL     MSG(14)       DSMSMS
080800160609     c                   enddo
080900160609     c                   endsr
081000030207     C*------------------------------------------------------------------------*
081100150203     C* CNTCOD - CONTROLLA PRESENZA DEL CODICE ARTICOLO IN anagrafica agb
081200030207     C*------------------------------------------------------------------------*
081300030207     C     CNTCOD        BEGSR
081400030207     C*
081500030207     C                   CLEAR                   TIBS02DS
081600150210     C                   CLEAR                   vidcodd          30
081700030207     C*
081800150210     C     '?'           SCAN      VIDCOD                                 30
081900030207     C*
082000030207     C*  Ricerca
082100030207     C                   IF        *IN30 = *ON
082200150203     C* RICHIESTO ELENCO ARTICOLI
082300150203     C                   MOVEL     KPJBU         SAVKPJBU
082400150203     C                   MOVEL(P)  'P01'         KPJBU
082500150203     C                   CALL      'TISIG1R'
082600150203     C                   PARM                    KPJBA
082700150203     C                   EVAL      VIDCOD = %subst(KPJBU:4:15)
082800150203     C                   MOVEL     SAVKPJBU      KPJBU
082900150203     C                   ELSE
083000150203     C     vidcod        chain     azagb01l
083100150203     C                   IF        %found(azagb01l)
083200150203     C                   EVAL      vidcodd = agbdes
083300150203     c                   else
083400150203     C                   MOVEL     '1'           ERRL00
083500150211     c                   select
083600150211     c                   when      quale = '1'
083700150211     C                   SETON                                        80
083800150211     c                   when      quale = '2'
083900150211     C                   SETON                                        81
084000150211     c                   when      quale = '3'
084100150211     C                   SETON                                        78
084200150211     c                   endsl
084300150203     C                   MOVEL     MSG(9)        DSMSMS
084400150203     C                   ENDIF
084500150203     C                   ENDIF
084600030207     C*
084700150211     C                   IF        %subst(vidcod:1:1) <> vidtsc
084800150211     C                   MOVEL     '1'           ERRL00
084900150211     c                   select
085000150211     c                   when      quale = '1'
085100150211     C                   SETON                                        80
085200150211     c                   when      quale = '2'
085300150211     C                   SETON                                        81
085400150211     c                   when      quale = '3'
085500150211     C                   SETON                                        78
085600150211     c                   endsl
085700150211     C                   MOVEL     MSG(11)       DSMSMS
085800150211     C                   ENDIF
085900030207     C                   ENDSR
086000150202     C*------------------------------------------------------------------------*
086100150202     C* CNTksu - CONTROLLA PRESENZA DEL CODICE ARTICOLO IN TABELLA "TBA"
086200150202     C*------------------------------------------------------------------------*
086300150202     C     CNTksu        BEGSR
086400150202     C*
086500150210     C                   CLEAR                   xclirds
086600150202     C*
086700150202     C     '?'           SCAN      VIDksu                                 30
086800150202     C*
086900150202     C*  Ricerca
087000150202     C                   IF        *IN30 = *ON
087100150210     C* RICHIESTO ELENCO CLIENTI
087200150210     C                   clear                   xCLIrds
087300150210     C***  DXCaut = OTAA1cabi;
087400150210     C                   EVAL      DXCcap = DUTkci
087500150225     C                   EVAL      DXCAUT = wabi
087600150210     C                   CALL      'XCLIR'
087700150210     C                   PARM                    xCLIrds
087800150210     C                   EVAL      ksa = DXCksc
087900150210     C                   EVAL      Dsksa = ksa(1)
088000150210     C                   EVAL      ksc(1) = dsksn
088100150212     C                   move      ksc(1)        VIDksu
088200150202     C                   ELSE
088300150304     C                   TESTN                   vidksu               30
088400150304     C                   MOVE      vidksu        W001A             1
088500150304    2C     *IN30         IFEQ      *OFF
088600150304     C     W001A         ORLT      '0'
088700150304     C                   MOVEL     '1'           ERRL00
088800150304     C                   SETON                                        83
088900150304     C                   MOVEL     MSG(08)       DSMSMS
089000150304     C                   leavesr
089100150304     c                   endif
089200150202     C*  Controllo
089300150210     C                   Z-ADD     1             ACOkut
089400150210     C                   Z-ADD     DUTKCI        ACOkcc
089500150210     C                   EVAL      ACOKsc = %dec(VIDksu : 7 :0)
089600150210     C     K03ACO00      CHAIN     cnaco00f
089700150210     C                   IF        %found(cnaco00f)
089800150210     C                   MOVEL     ACOrag        Vidksud
089900150202     C                   ELSE
090000150202     C                   MOVEL     '1'           ERRL00
090100150202     C                   SETON                                        83
090200150202     C                   MOVEL     MSG(10)       DSMSMS
090300150202     C                   ENDIF
090400150202     C                   ENDIF
090500150212     c                   eval      ksu3 = %subst(vidksu:2:3)
090600150212     c     ksu3          lookup    $pog                                   66
090700150212     c                   if        not *in66
090800150212     C                   MOVEL     '1'           ERRL00
090900150212     C                   SETON                                        83
091000150212     C                   MOVEL     MSG(13)       DSMSMS
091100150212     C                   ENDIF
091200150202     C*
091300150202     C                   ENDSR
091400970526     C*------------------------------------------------------------------------*
091500970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
091600970526     C*------------------------------------------------------------------------*
091700970526     C     IREVID        BEGSR
091800000407     C*
091900150210     C                   MOVEL     cgbCOD1       VIDCOD1
092000150210     C                   MOVEL     cgbCOD2       VIDCOD2
092100150210     C                   MOVEL     cgbCOD3       VIDCOD3
092200150212     C                   MOVE      cgbksu        VIDksu
092300150202     C                   MOVEL     cgbtsc        VIDtsc
092400150202     C                   MOVEL     cgbmail       VIDmail
092500150210     C                   MOVEL     cgbnote       VIDnote
092600150202     C                   z-add     cgbsco        VIDsco
092700150202     C                   z-add     cgbfre        VIDfre
092800150210     C                   z-add     cgbqta        VIDqta
092900150203      * data inizio validità
093000150202     C                   Z-ADD     cgbdti        G08inv
093100150202     C                   Z-ADD     *ZEROS        G08dat
093200150202     C                   MOVEL     '3'           G08ERR
093300150202     C                   CALL      'XSRDA8'
093400150202     C                   PARM                    WLBDA8
093500150202     C                   Z-ADD     G08dat        viddti
093600150203      * data fine validità
093700150202     C                   Z-ADD     cgbdtf        G08inv
093800150202     C                   Z-ADD     *ZEROS        G08dat
093900150202     C                   MOVEL     '3'           G08ERR
094000150202     C                   CALL      'XSRDA8'
094100150202     C                   PARM                    WLBDA8
094200150202     C                   Z-ADD     G08dat        viddtf
094300150203      * data ultimo movimento
094400150203     C                   Z-ADD     cgbdte        G08inv
094500150203     C                   Z-ADD     *ZEROS        G08dat
094600150203     C                   MOVEL     '3'           G08ERR
094700150203     C                   CALL      'XSRDA8'
094800150203     C                   PARM                    WLBDA8
094900150203     C                   Z-ADD     G08dat        viddte
095000970527     C*
095100970526     C                   ENDSR
095200970526     C*------------------------------------------------------------------------*
095300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
095400970526     C*------------------------------------------------------------------------*
095500970526     C     AZZVID        BEGSR
095600000407     C*
095700150210     C                   MOVEL     *BLANKS       VIDCOD1
095800150210     C                   MOVEL     *BLANKS       VIDCOD2
095900150210     C                   MOVEL     *BLANKS       VIDCOD3
096000150202     C                   MOVEL     *BLANKS       VIDksu
096100150202     C                   MOVEL     *BLANKS       VIDtsc
096200150202     C                   MOVEL     *BLANKS       VIDmail
096300150210     C                   MOVEL     *BLANKS       VIDnote
096400150210     C                   z-add     0             VIDqta
096500150202     C                   z-add     0             VIDsco
096600150202     C                   z-add     0             VIDfre
096700150202     C                   z-add     0             viddti
096800150202     C                   z-add     0             viddtf
096900150203     C                   z-add     0             viddte
097000970526     C                   ENDSR
097100000407     C*------------------------------------------------------------------------*
097200000407     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
097300000407     C*------------------------------------------------------------------------*
097400000407     C     IMPCAM        BEGSR
097500000407     C*
097600150210     C                   MOVEL     'B'           cgbTIP
097700150210     C                   MOVEL     VIDCOD1       cgbCOD1
097800150210     C                   MOVEL     VIDCOD2       cgbCOD2
097900150210     C                   MOVEL     VIDCOD3       cgbCOD3
098000150212     C                   MOVE      VIDksu        cgbksu
098100150202     C                   MOVEL     VIDtsc        cgbtsc
098200150202     C                   MOVEL     VIDmail       cgbmail
098300150210     C                   MOVEL     VIDnote       cgbnote
098400150202     C                   z-add     VIDsco        cgbsco
098500150210     C                   z-add     VIDqta        cgbqta
098600150202     C                   z-add     VIDfre        cgbfre
098700150202      *
098800150210     c                   if        viddti > 0
098900150202     C                   Z-ADD     viddti        g08dat
099000150202     C                   Z-ADD     *ZEROS        g08inv
099100150202     C                   MOVEL     '0'           G08ERR
099200150202     C                   CALL      'XSRDA8'
099300150202     C                   PARM                    WLBDA8
099400150202     C                   Z-ADD     G08inv        cgbdti
099500150210     c                   else
099600150210     C                   Z-ADD     0             cgbdti
099700150210     c                   endif
099800150202      *
099900150210     c                   if        viddtf > 0
100000150202     C                   Z-ADD     viddtf        g08dat
100100150202     C                   Z-ADD     *ZEROS        g08inv
100200150202     C                   MOVEL     '0'           G08ERR
100300150202     C                   CALL      'XSRDA8'
100400150202     C                   PARM                    WLBDA8
100500150202     C                   Z-ADD     G08inv        cgbdtf
100600150210     c                   else
100700150210     C                   Z-ADD     0             cgbdtf
100800150210     c                   endif
100900000407     C*
101000150210     c                   if        viddte > 0
101100150210     C                   Z-ADD     viddte        g08dat
101200150210     C                   Z-ADD     *ZEROS        g08inv
101300150210     C                   MOVEL     '0'           G08ERR
101400150210     C                   CALL      'XSRDA8'
101500150210     C                   PARM                    WLBDA8
101600150210     C                   Z-ADD     G08inv        cgbdte
101700150210     c                   else
101800150210     C                   Z-ADD     0             cgbdte
101900150210     c                   endif
102000150210     C                   Z-ADD     WN14          cgbdtu
102100150210     C                   move      knmus         cgbute
102200000407     C                   ENDSR
102300150212     C*___________________________________________________________________________
102400150212     C* Abbinato scrive record principale su file codici abbinati con codice abbinato uguale
102500150212     C*------------------------------------------------------------------------*
102600150212     C     abbinato      BEGSR
102700150212     C*
102800150225     C                   MOVE      *blank        fgbann
102900150212     C                   MOVE      VIDksu        fgbksu
103000150212     C                   MOVE      VIDksu        fgbksc
103100150212     C                   Z-ADD     WN14          fgbdtu
103200150212     C                   move      knmus         fgbute
103300150212     c                   write     azfgb000
103400150212     C                   ENDSR
103500970526     C*------------------------------------------------------------------------*
103600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
103700970526     C*------------------------------------------------------------------------*
103800970526     C     IMPFUN        BEGSR
103900970526     C*
104000970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
104100021211     C     POP1          IFEQ      'O05'
104200970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
104300970526     C                   MOVEL     'N'           FUDATT
104400970526     C                   MOVEL     FUDRIG        FUD(6)
104500021211     C                   ENDIF
104600970526     C*
104700000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
104800150204     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
104900000407     C                   EXSR      LETCAS
105000980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
105100980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
105200980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
105300000407     C                   ELSE                                                   *INSERIMENTO
105400150225     C                   MOVEL     FUD(2)        FUDRIG                         *AGGIORNAMENTO
105500150225     C                   MOVEL     'N'           FUDATT
105600150225     C                   MOVEL     FUDRIG        FUD(2)
105700970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
105800000407     C                   ENDIF
105900150226     c     pop1          ifeq      'O03'
106000150226     c                   clear                   viddte
106100150226     C                   MOVEL     FUD(2)        FUDRIG                         *AGGIORNAMENTO
106200150226     C                   MOVEL     'N'           FUDATT
106300150226     C                   MOVEL     FUDRIG        FUD(2)
106400150226     C                   ENDIF
106500970526     C*
106600150225     C* GESTIONE TASTI FUNZIONALI A VIDEO
106700150225     C                   Z-ADD     *ZEROS        FUNGRU            5 0
106800150225     C                   EXSR      FUNGES
106900150225     C*
107000021211     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
107100021211     C     POP1          IFEQ      'O02'
107200021211     C                   MOVEL     DEC(1)        VIDFUN
107300021211     C                   ENDIF
107400021211     C     POP1          IFEQ      'O03'
107500021211     C                   MOVEL     DEC(5)        VIDFUN
107600021211     C                   ENDIF
107700021211     C     POP1          IFEQ      'O04'
107800021211     C                   MOVEL     DEC(2)        VIDFUN
107900021211     C                   ENDIF
108000021211     C     POP1          IFEQ      'O05'
108100021211     C                   MOVEL     DEC(3)        VIDFUN
108200021211     C                   ENDIF
108300150204     C     POP1          IFEQ      'F06'
108400021211     C                   MOVEL     DEC(4)        VIDFUN
108500021211     C                   ENDIF
108600150210     C     POP1          IFEQ      'F02'
108700150210     C                   MOVEL     DEC(6)        VIDFUN
108800150210     C                   ENDIF
108900021211     C*
109000021211     C                   ENDSR
109100030123     C*--------------------------------------------------------------------------------------------*
109200030123     C* REPERISCE I DATI UTENTE
109300030123     C*--------------------------------------------------------------------------------------------*
109400030123     C     REPDATIUTE    BEGSR
109500030123     C*
109600030123     C* INIZIALIZZA VARIABILI DI WRK
109700030123     C                   CLEAR                   TIBS34DS
109800030123     C                   CLEAR                   AZUTEDS
109900030123     C                   CLEAR                   DDATIUTE
110000030123     C*
110100030123     C     *DTAARA       DEFINE    §azute        azuteds
110200030123     C     *DTAARA       DEFINE    §datiute      ddatiute
110300030123     C                   IN(E)     *DTAARA
110400030123     C                   IF        %Error
110500030123     C                   EVAL      I34Tla = 'L'
110600030123     C                   CALL      'TIBS34R'
110700030123     C                   PARM                    Tibs34Ds
110800030123     C                   IN        *DTAARA
110900030123     C                   ENDIF
111000150212     c                   Clear                   wabi
111100150212     c                   Clear                   dLat
111200150212
111300150212      * Verifica errori e autorità profilo
111400150212s   1c                   Select
111500150212      * se ho errori nei dati utente esco dal pgm
111600150212w   1c                   When      DutErr = 'E'
111700150212     c                   seton                                        lr
111800150212     c                   return
111900150212      * se non c'è l'abilitazione
112000150212      * --> se 1° livello, abilitazioni al terminal
112100150212      *     se 2° livello, abilitazioni al punto operativo
112200150212w   1c                   When      UteAut = *Blanks
112300150212if  2c                   If        DutLpo = '1'
112400150212     c                   Eval      wabi   = 'TP'
112500150212e   2c                   EndIf
112600150212if  2c                   If        DutLpo = '2'
112700150212     c                   Eval      wabi   = 'PO'
112800150212e   2c                   EndIf
112900150212if  2c                   If        DutLpo = 'S'
113000150212     c                   Eval      wabi   = 'AZ'
113100150212e   2c                   EndIf
113200150212      * carica le abilitazioni del profilo
113300150212x   1c                   Other
113400150212     c                   Movel     UteFaf        Dute01
113500150212     c                   If        §Utecgb <> *Blanks
113600150212     c                   Eval      wabi = §Utecgb
113700150212     c                   Else
113800150212     c                   Eval      wabi = UteAut
113900150212     c                   EndIf
114000150212e   1c                   EndSl
114100150212      * controllo se ok l'abilitazione dell'utente
114200150212     c                   Clear                   Tibs02ds
114300150212     c                   Eval      T02Mod = 'C'
114400150212     c                   Eval      T02Sif = knsif
114500150212     c                   Eval      T02Cod = 'LAT'
114600150212     c                   Movel(p)  wabi          T02Ke1
114700150212     c                   Call      'TIBS02R'
114800150212     c                   Parm                    kpjba
114900150212     c                   Parm                    Tibs02ds
115000150212if  1c                   If        T02Err = *Blanks
115100150212     c                   Eval      dLat = T02Uni
115200150212e   1c                   EndIf
115300150212      * errore o non abilitato (impossibile xchè controllato nel filtro)
115400150212if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
115500150212   x1c                   Else
115600150212      * Reperimento dei P.O. gestibili dall'utente
115700150212     c                   clear                   TRUL31DS
115800150212     c                   clear                   TRUL31DS2
115900150212     c                   eval      I31abi = wabi
116000150212     c                   eval      I31cdi = DUTdis
116100150212     c                   eval      I31car = DUTare
116200150212     c                   eval      I31cpo = DUTpou
116300150212     c                   call      'TRUL31R'
116400150212     c                   parm                    KPJBA
116500150212     c                   parm                    TRUL31DS
116600150212     c                   parm                    TRUL31DS2
116700150212if  2c                   if        O31pog > *zeros
116800150212     c                   movea     O31pog        $pog
116900150212     c                   movea     O31arg        $arg
117000150212     c                   movea     O31dig        $dig
117100150212x   2c                   else
117200150212     c* impossibile perchè controllato nel filtrp
117300150212     c                   seton                                        lr
117400150212     c                   return
117500150212e   2c                   endif
117600150212    1c                   endif
117700030123     C*
117800030123     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
117900030123     C                   MOVEL(P)  RSUT          DSFIRS           20
118000030123     C*
118100030123     C                   ENDSR
118200970526     C*------------------------------------------------------------------------*
118300970526     C* *INZSR - OPERAZIONI INIZIALI
118400970526     C*------------------------------------------------------------------------*
118500970526     C     *INZSR        BEGSR
118600970526     C*---
118700970526     C* RICEVIMENTO PARAMETRI
118800970526     C*---
118900970526     C     *ENTRY        PLIST
119000970612     C                   PARM                    KPJBA
119100000407     C                   MOVEL     KPJBU         PARAM
119200150202     C*chiave inserimento
119300150202     C     kinse         kLIST
119400150202     C                   kfld                    vidKSU
119500150204     C                   kfld                    vidTSC
119600160609     C*chiave fgb
119700160609     C     kfgb          kLIST
119800160609     C                   kfld                    vidKSU
119900160609     C                   kfld                    ksuf              8
120000150210     C*luoghi
120100150210     c     kFnspe        Klist
120200150210     c                   Kfld                    kspefls           1
120300150210     c                   Kfld                    kspecli           7 0
120400150210     c                   Kfld                    kspecod           3
120500150210     c                   move      'L'           kspefls
120600150210     c                   move      '888'         kspecod
120700150210
120800150202      *chiave modifica
120900150202     C     kmodif        kLIST
121000150202     C                   kfld                    pKSU
121100150204     C                   kfld                    pTSC
121200150302     C                   kfld                    pdti
121300150210     C* LETTURA cnaco00f -
121400150210     C     K03ACO00      KLIST
121500150210     C                   KFLD                    ACOkut
121600150210     C                   KFLD                    ACOkcc
121700150210     C                   KFLD                    ACOksc
121800000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
121900000407     C     POP1          IFEQ      'O05'
122000000407     C                   SETON                                        90
122100000407     C                   ENDIF
122200000407     C     POP1          IFEQ      'O02'
122300000407     C                   SETON                                        91
122400000407     C                   ENDIF
122500030123     C*
122600030123     C* REPERIMENTO DATI UTENTE
122700030123     C                   EXSR      REPDATIUTE
122800000407     C*
122900000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
123000150225     C                   SETOFF                                       7879
123100000407     C                   SETOFF                                       808182
123200150203     C                   SETOFF                                       838485
123300150203     C                   SETOFF                                       868788
123400970526     C*
123500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
123600970526     C     XMSGPA        PLIST
123700970526     C                   PARM                    DSMSMS
123800970526     C                   PARM                    DSMSPG
123900970526     C                   PARM                    DSMSKE
124000970526     C                   PARM                    DSMSER
124100970526     C*
124200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
124300970526     C                   CALL      'XKEY'
124400970526     C                   PARM                    DSKEY
124500970526     C*---
124600970526     C* VARIABILI NON RIFERITE AL DATA BASE
124700970526     C*---
124800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
124900970526     C*---
125000970526     C* VARIABILI DI CONTROLLO
125100970526     C*---
125200970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
125300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
125400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
125500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
125600970526     C*
125700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
125800150202     C                   MOVEL     'TISIG11R'    DSMSPG
125900150202     C                   MOVEL     'TISIG11R'    MSGQUE
126000970526     C                   MOVEL     *BLANKS       MSGKEY
126100970526     C                   MOVEL     *BLANKS       DSMSMS
126200000407     C*
126300000407     C* CALCOLA LA DATA CORRENTE
126400000407     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
126500000407     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
126600000407     C                   Z-ADD     WN8           G08DAT
126700000407     C                   Z-ADD     *ZEROS        G08INV
126800000407     C                   MOVEL     '0'           G08ERR
126900000407     C                   CALL      'XSRDA8'
127000000407     C                   PARM                    WLBDA8
127100000407     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
127200970526     C*
127300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
127400970526     C                   EXSR      IMPFUN
127500970526     C*
127600970526     C                   ENDSR
127700970526     C*------------------------------------------------------------------------*
127800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
127900970526     C*------------------------------------------------------------------------*
128000970526     C     FUNGES        BEGSR
128100970526     C*
128200970526     C* PULISCO CAMPI DI LAVORO
128300970526     C                   Z-ADD     0             W                 5 0
128400970526     C*
128500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
128600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
128700970526     C     1             DO        24            I
128800970526     C                   MOVEL     FUD(I)        FUDRIG
128900970526     C*
129000970526     C* PULIZIA FUNZIONI PRECEDENTI
129100970526     C                   MOVEL     *BLANKS       FUV(I)
129200970526     C*
129300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
129400970526     C* (SALTA F24)
129500970526     C     FUDATT        IFEQ      'S'
129600970526     C     FUDFUN        ANDNE     '24'
129700970526     C                   ADD       1             W
129800970526     C                   MOVEL     FUDDES        FUV(W)
129900970526     C                   END
130000970526     C                   END                                                    DO WHILE
130100970526     C*
130200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
130300970526     C* ALTRIMENTI DISATTIVO F24
130400970526     C     1             DO        24            I
130500970526     C                   MOVEL     FUD(I)        FUDRIG
130600970526     C*
130700970526     C     FUDFUN        IFEQ      '24'
130800970526     C     W             IFGT      6
130900970526     C                   MOVEL     'S'           FUDATT
131000970526     C                   ELSE
131100970526     C                   MOVEL     *BLANKS       FUDATT
131200970526     C                   END                                                    W GT 8
131300970526     C                   MOVEL     FUDRIG        FUD(I)
131400970526     C                   END
131500970526     C                   END                                                    DO WHILE
131600970526     C*
131700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
131800970526     C     W             DIV       6             I
131900970526     C                   MVR                     Z
132000970526     C*
132100970526     C     Z             IFNE      0
132200970526     C     I             ADD       1             Z
132300970526     C                   ELSE
132400970526     C                   Z-ADD     I             Z                 5 0
132500970526     C                   END
132600970526     C*
132700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
132800970526     C                   ADD       1             FUNGRU
132900970526     C     FUNGRU        IFGT      Z
133000970526     C     FUNGRU        ORLT      1
133100970526     C                   Z-ADD     1             FUNGRU
133200970526     C                   END
133300970526     C*
133400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
133500970526     C     FUNGRU        IFEQ      1
133600970526     C                   MOVEL     FUV(1)        FUN001
133700970526     C                   MOVEL     FUV(2)        FUN002
133800970526     C                   MOVEL     FUV(3)        FUN003
133900970526     C                   MOVEL     FUV(4)        FUN004
134000970526     C                   MOVEL     FUV(5)        FUN005
134100970526     C                   MOVEL     FUV(6)        FUN006
134200970526     C                   END
134300970526     C*
134400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
134500970526     C     FUNGRU        IFEQ      2
134600970526     C                   MOVEL     FUV(7)        FUN001
134700970526     C                   MOVEL     FUV(8)        FUN002
134800970526     C                   MOVEL     FUV(9)        FUN003
134900970526     C                   MOVEL     FUV(10)       FUN004
135000970526     C                   MOVEL     FUV(11)       FUN005
135100970526     C                   MOVEL     FUV(12)       FUN006
135200970526     C                   END
135300970526     C*
135400970526     C* TERZO GRUPPO DI 6 FUNZIONI
135500970526     C     FUNGRU        IFEQ      3
135600970526     C                   MOVEL     FUV(13)       FUN001
135700970526     C                   MOVEL     FUV(14)       FUN002
135800970526     C                   MOVEL     FUV(15)       FUN003
135900970526     C                   MOVEL     FUV(16)       FUN004
136000970526     C                   MOVEL     FUV(17)       FUN005
136100970526     C                   MOVEL     FUV(18)       FUN006
136200970526     C                   END
136300970526     C*
136400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
136500970526     C     FUNGRU        IFEQ      4
136600970526     C                   MOVEL     FUV(19)       FUN001
136700970526     C                   MOVEL     FUV(20)       FUN002
136800970526     C                   MOVEL     FUV(21)       FUN003
136900970526     C                   MOVEL     FUV(22)       FUN004
137000970526     C                   MOVEL     *BLANKS       FUN005
137100970526     C                   MOVEL     *BLANKS       FUN006
137200970526     C                   END
137300970526     C*
137400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
137500970526     C                   MOVEL     *BLANKS       FUN007
137600970526     C                   MOVEL     *BLANKS       FUN008
137700970526     C*
137800970526     C     1             DO        24            I
137900970526     C                   MOVEL     FUD(I)        FUDRIG
138000970526     C     FUDFUN        IFEQ      '24'
138100970526     C     FUDATT        ANDEQ     'S'
138200970526     C                   MOVEL     FUDDES        FUN008
138300970526     C                   END
138400970526     C                   END                                                    DO
138500970526     C*
138600970526     C                   ENDSR
138700970526     C*------------------------------------------------------------------------*
138800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
138900970526     C*------------------------------------------------------------------------*
139000970526     C     FUNCON        BEGSR
139100970526     C*
139200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
139300970526     C                   EXSR      FUNVAR
139400970526     C*
139500970526     C* PULISCO CAMPI DI LAVORO
139600970526     C                   Z-ADD     0             I                 5 0
139700970526     C                   Z-ADD     0             W                 5 0
139800970526     C*
139900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
140000970526     C     FUNKEY        IFNE      *BLANKS
140100970526     C     1             DO        24            I
140200970526     C                   MOVEL     FUD(I)        FUDRIG
140300970526     C*
140400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
140500970526     C     FUNKEY        IFEQ      FUDFUN
140600970526     C     FUDATT        ANDEQ     'S'
140700970526     C                   Z-ADD     I             W
140800970526     C                   END
140900970526     C                   END                                                    DO WHILE
141000970526     C*
141100970526     C* FUNZIONE NON ATTIVA (ERRORE)
141200970526     C     W             IFEQ      0
141300970526     C*
141400970526     C* EMISSIONE MESSAGGIO DI ERRORE
141500970526     C                   MOVEL     MSG(1)        DSMSMS
141600970526     C*
141700970526     C                   END                                                    W=0
141800970526     C                   END                                                    FUNKEY NE *BLANKS
141900970526     C*
142000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
142100970526     C     W             IFNE      0
142200970526     C     FUNKEY        OREQ      *BLANKS
142300970526     C     FUNKEY        IFEQ      *BLANKS
142400970526     C                   EXSR      ENTGES
142500970526     C                   END
142600150210     C     FUNKEY        IFEQ      '02'
142700150210     C                   EXSR      F02GES
142800150210     C                   END
142900970526     C     FUNKEY        IFEQ      '03'
143000970526     C                   EXSR      F03GES
143100970526     C                   END
143200970526     C     FUNKEY        IFEQ      '06'
143300970526     C                   EXSR      F06GES
143400970526     C                   END
143500970526     C     FUNKEY        IFEQ      '12'
143600970526     C                   EXSR      F12GES
143700970526     C                   END
143800970526     C     FUNKEY        IFEQ      '24'
143900970526     C                   EXSR      F24GES
144000970526     C                   END
144100970526     C                   END
144200970526     C*
144300970526     C                   ENDSR
144400970526     C*------------------------------------------------------------------------*
144500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
144600970526     C*------------------------------------------------------------------------*
144700970526     C     FUNVAR        BEGSR
144800970526     C*
144900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
145000970526     C*
145100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
145200970526     C     KEY           IFEQ      ENTER
145300970526     C                   MOVEL     *BLANKS       FUNKEY
145400970526     C                   END
145500970526     C     KEY           IFEQ      F1
145600970526     C                   MOVEL     '01'          FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      F2
145900970526     C                   MOVEL     '02'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      F3
146200970526     C                   MOVEL     '03'          FUNKEY
146300970526     C                   END
146400970526     C     KEY           IFEQ      F4
146500970526     C                   MOVEL     '04'          FUNKEY
146600970526     C                   END
146700970526     C     KEY           IFEQ      F5
146800970526     C                   MOVEL     '05'          FUNKEY
146900970526     C                   END
147000970526     C     KEY           IFEQ      F6
147100970526     C                   MOVEL     '06'          FUNKEY
147200970526     C                   END
147300970526     C     KEY           IFEQ      F7
147400970526     C                   MOVEL     '07'          FUNKEY
147500970526     C                   END
147600970526     C     KEY           IFEQ      F8
147700970526     C                   MOVEL     '08'          FUNKEY
147800970526     C                   END
147900970526     C     KEY           IFEQ      F9
148000970526     C                   MOVEL     '09'          FUNKEY
148100970526     C                   END
148200970526     C     KEY           IFEQ      F10
148300970526     C                   MOVEL     '10'          FUNKEY
148400970526     C                   END
148500970526     C     KEY           IFEQ      F11
148600970526     C                   MOVEL     '11'          FUNKEY
148700970526     C                   END
148800970526     C     KEY           IFEQ      F12
148900970526     C                   MOVEL     '12'          FUNKEY
149000970526     C                   END
149100970526     C     KEY           IFEQ      F13
149200970526     C                   MOVEL     '13'          FUNKEY
149300970526     C                   END
149400970526     C     KEY           IFEQ      F14
149500970526     C                   MOVEL     '14'          FUNKEY
149600970526     C                   END
149700970526     C     KEY           IFEQ      F15
149800970526     C                   MOVEL     '15'          FUNKEY
149900970526     C                   END
150000970526     C     KEY           IFEQ      F16
150100970526     C                   MOVEL     '16'          FUNKEY
150200970526     C                   END
150300970526     C     KEY           IFEQ      F17
150400970526     C                   MOVEL     '17'          FUNKEY
150500970526     C                   END
150600970526     C     KEY           IFEQ      F18
150700970526     C                   MOVEL     '18'          FUNKEY
150800970526     C                   END
150900970526     C     KEY           IFEQ      F19
151000970526     C                   MOVEL     '19'          FUNKEY
151100970526     C                   END
151200970526     C     KEY           IFEQ      F20
151300970526     C                   MOVEL     '20'          FUNKEY
151400970526     C                   END
151500970526     C     KEY           IFEQ      F21
151600970526     C                   MOVEL     '21'          FUNKEY
151700970526     C                   END
151800970526     C     KEY           IFEQ      F22
151900970526     C                   MOVEL     '22'          FUNKEY
152000970526     C                   END
152100970526     C     KEY           IFEQ      F23
152200970526     C                   MOVEL     '23'          FUNKEY
152300970526     C                   END
152400970526     C     KEY           IFEQ      F24
152500970526     C                   MOVEL     '24'          FUNKEY
152600970526     C                   END
152700970526     C     KEY           IFEQ      ROLLUP
152800970526     C                   MOVEL     'RU'          FUNKEY
152900970526     C                   END
153000970526     C     KEY           IFEQ      HELP
153100970526     C                   MOVEL     'HP'          FUNKEY
153200970526     C                   END
153300970526     C                   ENDSR
153400970526     C*------------------------------------------------------------------------*
153500970526     C* NOTE COSTRUZIONE SCHIERE
153600970526     C*------------------------------------------------------------------------*
153700970526     C*
153800970526     C* -MSG-MESSAGGI
153900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
154000970526     C* (NON TOCCARE MSG,1 E MSG,2)
154100970526     C*
154200970526     C* -CMD-COMANDI
154300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
154400970526     C*
154500970526     C* -OPD-OPZIONI
154600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
154700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
154800970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
154900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
155000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
155100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
155200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
155300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
155400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
155500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
155600970526     C*
155700970526     C* -FUD-FUNZIONI
155800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
155900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
156000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
156100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
156200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
156300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
156400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
156500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
156600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
156700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
156800970526     O*------------------------------------------------------------------------*
156900150211     oazcgb000  e            libera
157000150211     O*_________________________________________________________________________
157100970526** DEC - DECODIFICHE
157200970526    MODIFICA                                                      1
157300970526  ANNULLAMENTO                                                    2
157400970526VISUALIZZAZIONE                                                   3
157500021211  INSERIMENTO                                                     4
157600021211     COPIA                                                        5
157700150210    GESTIONE                                                      6
157800970528** MSG - MESSAGGI
157900150202TISIG11R- Tasto funzionale non gestito                                          1
158000150202TISIG11R- Inserimento effettuato                                                2
158100150202TISIG11R- Modifica effettuata                                                   3
158200150202TISIG11R- Annullamento effettuato                                               4
158300150202TISIG11R- Copia effettuata                                                      5
158400150202TISIG11R- Richiesta funzione per record inesistente                             6
158500150305TISIG11R- Impossibile inserire, cliente/gestione già presenti nel periodo       7
158600150304TISIG11R- Carattere non valido verificare i campi a video                       8
158700150203TISIG11R- Codice articolo non presente in anagrafica articoli                   9
158800150224TISIG11R- Codice cliente raggruppamento bancali non corretto                   10
158900150211TISIG11R- Codice articolo incongruente con tipo gestione                       11
159000150203TISIG11R- Il campo è obbligatorio                                              12
159100150212TISIG11R- Codice non manutenzionabile da questo profilo                        13
159200150212TISIG11R- Codice non inseribile presente negli archivi come codice abbinato    14
159300150219TISIG11R- La Data non può essere retroattiva                                   15
159400150219TISIG11R- La Data inizio è maggiore della data fine                            16
159500150224TISIG11R- La Data inizio non può essere oltre 30 giorni la data odierna        17
159600150225TISIG11R- Per il tipo gestione "B" la franchigia deve essere minimo al 10%     18
159700150312TISIG11R- Esistono anagrafiche del cliente successive manutenzione errata      19
159800150312TISIG11R- Per il tipo gestione "B" la quantità soglia deve essere > 100        20
159900150330TISIG11R- La Data non è corretta                                               21
160000970526** FUD - FUNZIONI
160100970526                   -        (S/N)=N                        funz.01¬
160200150210F02=Abbinati       -        (S/N)=S                        funz.02¬
160300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
160400970526                   -        (S/N)=N                        funz.04¬
160500970526                   -        (S/N)=N                        funz.05¬
160600000407F06=Conferma       -        (S/N)=S                        funz.06¬
160700970527                   -        (S/N)=N                        funz.07¬
160800970527                   -        (S/N)=N                        funz.08¬
160900970527                   -        (S/N)=N                        funz.09¬
161000970527                   -        (S/N)=N                        funz.10¬
161100970526                   -        (S/N)=N                        funz.11¬
161200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
161300970526                   -        (S/N)=N                        funz.13¬
161400970526                   -        (S/N)=N                        funz.14¬
161500970526                   -        (S/N)=N                        funz.15¬
161600970526                   -        (S/N)=N                        funz.16¬
161700970526                   -        (S/N)=N                        funz.17¬
161800970526                   -        (S/N)=N                        funz.18¬
161900970526                   -        (S/N)=N                        funz.19¬
162000970526                   -        (S/N)=N                        funz.20¬
162100970526                   -        (S/N)=N                        funz.21¬
162200970526                   -        (S/N)=N                        funz.22¬
162300970526                   -        (S/N)=N                        funz.23¬
162400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
