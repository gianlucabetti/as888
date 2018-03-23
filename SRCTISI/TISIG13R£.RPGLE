000100970526     H*------------------------------------------------------------------------*
000200150202     H* Gestione anagrafico BANCALI clienti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150211     FTISIG13d  CF   E             WORKSTN INFDS(DEVDS1)
000900021211     F                                     SFILE(SIG2DMS:MSGULT)
001000150211     Fazfgb01l  UF A E           K DISK
001100150212     Fazfgb02l  iF   E           K DISK    rename(azfgb000:fgb2) prefix(x)
001200150210     Fcnaco00f  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900150210     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* DESCRIZIONI TASTI FUNZIONALI
002600970526     D*------------
002700970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* FUNZIONI DA CARICARE A VIDEO
003000970526     D*------------
003100970526     D FUV             S             19    DIM(24)
003200970526     D*------------------------------------------------------------------------*
003300970526     D* INPUT
003400970526     D*------------------------------------------------------------------------*
003500970526     D*------------
003600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003700970526     D*------------
003800970526     D FUDDS           DS
003900970526     D  FUDDE1                 1      1
004000970526     D  FUDFUN                 2      3
004100970526     D  FUDDE2                 4     19
004200970526     D  FUDFI0                20     34
004300970526     D  FUDATT                35     35
004400970526     D  FUDFI1                36     80
004500970526     D  FUDDES                 1     19
004600970526     D  FUDRIG                 1     80
004700970526     D*------------
004800970526     D* DS GESTIONE TASTI FUNZIONALI
004900970526     D*------------
005000970526     D DSKEY         E DS
005100970526     D DEVDS1          DS
005200970526     D  KEY                  369    369
005300970526     D*------------
005400970526     D* DS "XMSG" - GESTIONE MESSAGGI
005500970526     D*------------
005600970526     D DSMSG         E DS
005700970528     D*------------------
005800970526     D* ARCHITETTURA
005900970526     D*------------
006000970526     D KPJBA         E DS
006100150203     D savkpjbu        s                   like(kpjbu)
006200000407     D*------------------
006300000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
006400000407     D*------------------
006500000407     D WLBDA8          DS                  INZ
006600000407     D  G08DAT                 1      8  0
006700000407     D  G08INV                 9     16  0
006800000407     D  G08ERR                17     17
006900000407     D  G08TGI                18     22  0
007000030123     D*------------
007100030123     D* DS REPERIMENTO DATI UTENTE
007200030123     D*------------
007300030123     D TIBS34DS      E DS                                                       *Profili utente
007400030123     D DDATIUTE      E DS                                                       *Dati utente
007500030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007600030207     D*------------------
007700030207     D* DS REPERIMENTO / CONTROLLO DA TABELLE
007800030207     D*------------------
007900030207     D TIBS02DS      E DS
008000150210     D xclirds       E DS
008100150210     d ksa             s              4    dim(30)
008200150210     d ksc             s              7  0 dim(30)
008300150210     d                 ds
008400150210     d dsksn                   1      4p 0
008500150210     d dsksa                   1      4
008600030207     D*------------------
008700150202     d Tibs69DS      E DS                  INZ
008800150202     d DS_cnaco      E DS                  extname(CNACO00F)
008900150202     d DS_cnind      E DS                  extname(CNIND00F)
009000150202     d DS_cnclp      E DS                  extname(CNCLP00F)
009100150202     d DS_fncls      E DS                  extname(FNCLS00F)
009200000407     D*------------------
009300000407     D* DS PASSAGGIO PARAMETRI
009400000407     D*------------------
009500000407     D PARAM           DS                  INZ
009600150202     d  pop1                          3
009700150202     D  Pksu                          8
009800150211     D  Pksc                          8
009900150202     D  Pmsg                         80
010000970526     C*------------------------------------------------------------------------*
010100970526     C* MAIN LINES
010200970526     C*------------------------------------------------------------------------*
010300970526     C*
010400970526     C* NON CI SONO ERRORI IN ENTRATA
010500021211     C     ERRL00        IFEQ      '0'
010600970526     C*
010700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010800021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011100970526     C*
011200150211      * cerco i dati anagrafici
011300150211     c                   clear                   tibs69ds
011400150211     c                   move      pksu          vidksu
011500150211     c                   move      vidksu        i69kac
011600150211     c                   call      'TIBS69R'
011700150211     c                   parm                    tibs69ds
011800150211     c                   parm                    ds_cnaco
011900150211     c                   parm                    ds_cnind
012000150211     c                   parm                    ds_cnclp
012100150211     c                   parm                    ds_fncls
012200150211      * ragione sociale
012300150211     c                   eval      vidksud = acorag
012400970526     C* EMETTE FORMATO VIDEO
012500970526     C                   EXSR      BUTFMR
012600970526     C*
012700970526     C* GESTIONE TASTI FUNZIONALI
012800970526     C                   EXSR      FUNCON
012900021211     C                   ENDDO
013000970526     C*
013100021211     C                   ENDIF
013200021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
013300021211     C                   MOVEL     PARAM         KPJBU
013400970526     C* FINE PGM
013500970526     C                   SETON                                        LR
013600970526     C*------------------------------------------------------------------------*
013700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013800970526     C*------------------------------------------------------------------------*
013900970526     C     BUTFMR        BEGSR
014000970526     C*
014100970526     C* EMETTE LA TESTATA
014200021211     C                   WRITE     SIG2D01
014300970526     C*
014400970526     C* SPEDISCE EVENTUALE MESSAGGIO
014500970526     C                   EXSR      SNDMSG
014600970526     C*
014700970526     C* EMETTE LE FUNZIONI
014800021211     C                   WRITE     SIG2D02
014900970526     C*
015000970526     C* EMETTE IL VIDEO
015100021211     C                   EXFMT     SIG2D03
015200970526     C*
015300970526     C* REIMPOSTA FLAG DI ERRORE
015400970526     C                   MOVEL     '0'           ERRL00
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015900970526     C*------------------------------------------------------------------------*
016000970526     C     SNDMSG        BEGSR
016100970526     C*
016200970526     C* PULISCE IL SFILE MESSAGGI
016300970526     C                   MOVEL     '1'           *IN40
016400021211     C                   WRITE     SIG2DMC
016500970526     C                   MOVEL     '0'           *IN40
016600970526     C*
016700970526     C* VISUALIZZA IL MESSAGGIO
016800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016900970526     C                   CALL      'XMSG'        XMSGPA
017000021211     C     DSMSER        IFEQ      *ZEROS
017100970526     C                   MOVEL     DSMSKE        MSGKEY
017200021211     C                   ENDIF
017300970526     C                   Z-ADD     1             MSGULT
017400021211     C                   WRITE     SIG2DMS
017500021211     C                   WRITE     SIG2DMC
017600970526     C*
017700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017800970526     C                   MOVEL     *BLANKS       DSMSMS
017900970526     C*
018000970526     C                   ENDSR
018100150210     C*------------------------------------------------------------------------*
018200150210     C* PASPAR - ASSEGNAZIONE VALORI PER PASSAGIO PARAMETRI
018300150210     C*------------------------------------------------------------------------*
018400150210     C     PASPAR        BEGSR
018500150210     C*
018600150210     C                   MOVEL     PARAM         KPJBU
018700150210     C*
018800150210     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* ENTGES - GESTIONE ENTER -> INVIO
019100970526     C*------------------------------------------------------------------------*
019200970526     C     ENTGES        BEGSR
019300970526     C*
019400970526     C* CONTROLLA I CAMPI IMMESSI
019500970526     C                   EXSR      DECODI
019600970526     C*
019700970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
019800021211     C     POP1          IFEQ      'O05'
019900021211     C                   MOVEL     *BLANKS       PMSG
020000970526     C                   MOVEL     '1'           FINL00
020100021211     C                   ENDIF
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F03GES - GESTIONE F03 -> FINE
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F03GES        BEGSR
020800970526     C*
020900970526     C* IMPOSTA "PREMUTO F03"
021000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F06GES        BEGSR
021700970526     C*
021800021211     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
021900150204     C     POP1          IFEQ      'F06'
022000021211     C                   EXSR      INSERI
022100021211     C                   ENDIF
022200021211     C     POP1          IFEQ      'O02'
022300021211     C                   EXSR      MODIFI
022400021211     C                   ENDIF
022500021211     C     POP1          IFEQ      'O03'
022600021211     C                   EXSR      COPIA
022700021211     C                   ENDIF
022800021211     C     POP1          IFEQ      'O04'
022900021211     C                   EXSR      ANNULL
023000021211     C                   ENDIF
023100021211     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400970526     C* F12GES - GESTIONE F12 -> RITORNO
023500970526     C*------------------------------------------------------------------------*
023600970526     C     F12GES        BEGSR
023700970526     C*
023800970526     C* IMPOSTA "PREMUTO F12"
023900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024400970526     C*------------------------------------------------------------------------*
024500970526     C     F24GES        BEGSR
024600970526     C*
024700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
025200970526     C*------------------------------------------------------------------------*
025300970526     C     INSERI        BEGSR
025400970526     C*
025500970526     C* CONTROLLA I CAMPI IMMESSI
025600970526     C                   EXSR      DECODI
025700160316     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025800021211     C                   EXSR      DECODINS
025900160316     c                   endif
026000970526     C*
026100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026200021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026300150211     C                   CLEAR                   Azfgb000
026400021211     C                   EXSR      IMPCAM
026500150211     C                   WRITE     AZfgb000
026600150226     C                   MOVEL     MSG(2)        PMSG                           *MESSAGGIO
026700970602     C*
026800970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
026900970602     C                   EXSR      AZZVID
027000021211     C                   ENDIF
027100970526     C*
027200970526     C                   ENDSR
027300970526     C*------------------------------------------------------------------------*
027400970526     C* MODIFI - OPERAZIONI DI MODIFICA
027500970526     C*------------------------------------------------------------------------*
027600970526     C     MODIFI        BEGSR
027700970526     C*
027800970526     C* CONTROLLA I CAMPI IMMESSI
027900970526     C                   EXSR      DECODI
028000970526     C*
028100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
028200021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028300970619     C                   EXSR      LETCAS
028400970526     C*
028500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028600021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028700000407     C                   EXSR      IMPCAM
028800150211     C                   UPDATE    AZfgb000
028900150226     C                   MOVEL     MSG(3)        PMSG
029000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029100021211     C                   ENDIF
029200021211     C                   ENDIF
029300970526     C*
029400970526     C                   ENDSR
029500021211     C*------------------------------------------------------------------------*
029600021211     C* COPIA - OPERAZIONI DI COPIA RECORD
029700021211     C*------------------------------------------------------------------------*
029800021211     C     COPIA         BEGSR
029900021211     C*
030000021211     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030100021211     C                   EXSR      LETCAS
030200021211     C*
030300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030400021211     C*
030500021211     C* CONTROLLA I CAMPI IMMESSI
030600021211     C                   EXSR      DECODI
030700021211     C                   EXSR      DECODINS
030800021211     C*
030900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031000150211     C                   CLEAR                   AZfgb000
031100021211     C                   EXSR      IMPCAM
031200150211     C                   WRITE     AZfgb000
031300150226     C                   MOVEL     MSG(5)        PMSG                           *MESSAGGIO
031400021211     C                   MOVEL     '1'           FINL00                         *FINE PGM
031500021211     C                   ENDIF
031600021211     C                   ENDIF
031700021211     C*
031800021211     C                   ENDSR
031900970526     C*------------------------------------------------------------------------*
032000970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
032100970526     C*------------------------------------------------------------------------*
032200970526     C     ANNULL        BEGSR
032300000411     C*
032400000411     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032500000411     C                   EXSR      LETCAS
032600150219     c                   if        vidksu = vidksc
032700150219     c                   move      '1'           errl00
032800150219     c                   seton                                        83
032900150219     c                   movel     msg(09)       dsmsms
033000150219     c                   endif
033100000411     C*
033200000411     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
033300000411     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033400150211     c                   move      'A'           fgbann
033500150211     C                   update    AZfgb000
033600150226     C                   MOVEL     MSG(4)        PMSG
033700000411     C                   MOVEL     '1'           FINL00                         *FINE PGM
033800000411     C                   ENDIF
033900000411     C*
034000970526     C                   ENDSR
034100970526     C*------------------------------------------------------------------------*
034200970619     C* LETCAS - LEGGE IL RECORD PERL'AGGIORNAMENTO
034300970526     C*------------------------------------------------------------------------*
034400970619     C     LETCAS        BEGSR
034500970526     C*
034600150211     C     kmodif        CHAIN     AZfgb01L                           99
034700021211     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
034800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034900150226     C                   MOVEL     MSG(6)        PMSG                           *MESSAGGIO
035000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035100021211     C                   ENDIF
035200970526     C*
035300970526     C                   ENDSR
035400970526     C*------------------------------------------------------------------------*
035500970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
035600970526     C*------------------------------------------------------------------------*
035700970526     C     DECODI        BEGSR
035800000407     C*
035900000407     C                   MOVEL     '0'           ERRL00
036000000407     C*
036100000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
036200150211     C                   SETOFF                                       83
036300030207     C*--------------------
036400150202     C* CONTROLLO ESISTENZA validità CODICE cliente unificante
036500030207     C*--------------------
036600030207     C     ERRL00        IFEQ      '0'
036700150211     C     VIDksc        ANDNE     *blanks
036800150211     C                   EXSR      CNTksc
036900030207     C                   ENDIF
037000000407     C*--------------------
037100000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
037200000407     C*--------------------
037300000407     C     ERRL00        IFEQ      '0'
037400150211     C     VIDksc        IFEQ      *blanks
037500150219     C     VIDksc        oreq      *zeros
037600150202     C                   MOVEL     '1'           ERRL00
037700150202     C                   SETON                                        83
037800150202     C                   ENDIF
037900000407     C     ERRL00        IFEQ      '1'
038000000407     C                   MOVEL     MSG(12)       DSMSMS
038100000407     C                   ENDIF
038200000407     C                   ENDIF
038300000407     C*
038400970528     C                   ENDSR
038500021211     C*------------------------------------------------------------------------*
038600021211     C* DECODINS - CONTROLLA CAMPI E DECODIFICA X INSERIMENTO
038700021211     C*------------------------------------------------------------------------*
038800021211     C     DECODINS      BEGSR
038900021211     C*
039000021211     C                   MOVEL     '0'           ERRL00
039100021211     C*
039200021211     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
039300150211     C                   SETOFF                                       83
039400021211     C*--------------------
039500021211     C* CONTROLLO GIA' ESISTENZA CODICE ARTICOLO
039600021211     C*--------------------
039700021211     C     ERRL00        IFEQ      '0'
039800150204     C     POP1          IFEQ      'F06'
039900021211     C     POP1          OREQ      'O03'
040000150302     C     kinse         setll     AZfgb01L
040100150302     c                   do        *hival
040200150302     C     kinse         reade     AZfgb01L
040300150302     C                   IF        %eof(azfgb01l)
040400150302     c                   leave
040500150302     c                   endif
040600150302     C                   IF        fgbann <> *blank
040700150302     c                   iter
040800150302     c                   endif
040900021211     C                   MOVEL     '1'           ERRL00
041000150211     C                   SETON                                        83
041100021211     C                   MOVEL     MSG(7)        DSMSMS
041200150212     c                   leavesr
041300150302     C                   ENDdo
041400150302     c
041500150302     C     vidksc        setll     AZfgb02L
041600150302     c                   do        *hival
041700150302     C     vidksc        reade     AZfgb02L
041800150302     C                   IF        %eof(azfgb02l)
041900150302     c                   leave
042000150302     c                   endif
042100150302     C                   IF        xfgbann <> *blank
042200150302     c                   iter
042300150302     c                   endif
042400150212     C                   MOVEL     '1'           ERRL00
042500150212     C                   SETON                                        83
042600150212     C                   MOVEL     MSG(11)       DSMSMS
042700150302     c                   leave
042800150302     C                   ENDdo
042900021211     C                   ENDIF
043000021211     C                   ENDIF
043100021211     C*
043200021211     C                   ENDSR
043300150202     C*------------------------------------------------------------------------*
043400150211     C* CNTksc - CONTROLLA PRESENZA DEL CODICE ARTICOLO IN TABELLA "TBA"
043500150202     C*------------------------------------------------------------------------*
043600150211     C     CNTksc        BEGSR
043700150202     C*
043800150210     C                   CLEAR                   xclirds
043900150202     C*
044000150211     C     '?'           SCAN      VIDksc                                 30
044100150202     C*
044200150202     C*  Ricerca
044300150202     C                   IF        *IN30 = *ON
044400150210     C* RICHIESTO ELENCO CLIENTI
044500150210     C                   clear                   xCLIrds
044600150210     C***  DXCaut = OTAA1cabi;
044700150210     C                   EVAL      DXCcap = DUTkci
044800150220     C                   EVAL      DXCAUT = 'AZ'
044900150210     C                   CALL      'XCLIR'
045000150210     C                   PARM                    xCLIrds
045100150210     C                   EVAL      ksa = DXCksc
045200150210     C                   EVAL      Dsksa = ksa(1)
045300150210     C                   EVAL      ksc(1) = dsksn
045400150211     C                   move      ksc(1)        vidksc
045500150202     C                   ELSE
045600150202     C*  Controllo
045700150210     C                   Z-ADD     1             ACOkut
045800150210     C                   Z-ADD     DUTKCI        ACOkcc
045900160316     c                   if        %subst(vidksc:8:1) = *blank
046000160316     c                   move      '1'           errl00
046100160316     C                   SETON                                        83
046200160316     C                   MOVEL     MSG(10)       DSMSMS
046300160316     c                   leavesr
046400160316     C                   ENDIF
046500160316     c
046600160316     C                   move      vidksc        acoksc
046700150210     C     K03ACO00      CHAIN     cnaco00f
046800150210     C                   IF        %found(cnaco00f)
046900150211     C                   MOVEL     ACOrag        Vidkscd
047000150202     C                   ELSE
047100150202     C                   MOVEL     '1'           ERRL00
047200150202     C                   ENDIF
047300160316     C                   ENDIF
047400150202     C*
047500150202     C                   ENDSR
047600970526     C*------------------------------------------------------------------------*
047700970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
047800970526     C*------------------------------------------------------------------------*
047900970526     C     IREVID        BEGSR
048000000407     C*
048100150211     C                   MOVEL     pksu          VIDksu
048200150211     C                   MOVEL     fgbksc        VIDksc
048300970527     C*
048400970526     C                   ENDSR
048500970526     C*------------------------------------------------------------------------*
048600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
048700970526     C*------------------------------------------------------------------------*
048800970526     C     AZZVID        BEGSR
048900000407     C*
049000150211     C                   MOVEL     *BLANKS       VIDksc
049100150211     C                   MOVEL     *BLANKS       VIDkscd
049200970526     C                   ENDSR
049300000407     C*------------------------------------------------------------------------*
049400000407     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
049500000407     C*------------------------------------------------------------------------*
049600000407     C     IMPCAM        BEGSR
049700000407     C*
049800150211     C                   MOVEL     VIDksu        fgbksu
049900150211     C                   MOVEL     VIDksc        fgbksc
050000150211     c                   z-add     wn14          fgbdtu
050100150211     c                   move      knmus         fgbute
050200000407     C                   ENDSR
050300970526     C*------------------------------------------------------------------------*
050400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
050500970526     C*------------------------------------------------------------------------*
050600970526     C     IMPFUN        BEGSR
050700970526     C*
050800970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
050900021211     C     POP1          IFEQ      'O05'
051000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
051100970526     C                   MOVEL     'N'           FUDATT
051200970526     C                   MOVEL     FUDRIG        FUD(6)
051300021211     C                   ENDIF
051400970526     C*
051500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
051600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
051700970526     C                   EXSR      FUNGES
051800970526     C*
051900000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
052000150204     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
052100000407     C                   EXSR      LETCAS
052200980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
052300980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
052400980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
052500000407     C                   ELSE                                                   *INSERIMENTO
052600970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
052700000407     C                   ENDIF
052800970526     C*
052900021211     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
053000021211     C     POP1          IFEQ      'O02'
053100021211     C                   MOVEL     DEC(1)        VIDFUN
053200021211     C                   ENDIF
053300021211     C     POP1          IFEQ      'O03'
053400021211     C                   MOVEL     DEC(5)        VIDFUN
053500021211     C                   ENDIF
053600021211     C     POP1          IFEQ      'O04'
053700021211     C                   MOVEL     DEC(2)        VIDFUN
053800021211     C                   ENDIF
053900021211     C     POP1          IFEQ      'O05'
054000021211     C                   MOVEL     DEC(3)        VIDFUN
054100021211     C                   ENDIF
054200150204     C     POP1          IFEQ      'F06'
054300021211     C                   MOVEL     DEC(4)        VIDFUN
054400021211     C                   ENDIF
054500150210     C     POP1          IFEQ      'F02'
054600150210     C                   MOVEL     DEC(6)        VIDFUN
054700150210     C                   ENDIF
054800021211     C*
054900021211     C                   ENDSR
055000030123     C*--------------------------------------------------------------------------------------------*
055100030123     C* REPERISCE I DATI UTENTE
055200030123     C*--------------------------------------------------------------------------------------------*
055300030123     C     REPDATIUTE    BEGSR
055400030123     C*
055500030123     C* INIZIALIZZA VARIABILI DI WRK
055600030123     C                   CLEAR                   TIBS34DS
055700030123     C                   CLEAR                   AZUTEDS
055800030123     C                   CLEAR                   DDATIUTE
055900030123     C*
056000030123     C     *DTAARA       DEFINE    §azute        azuteds
056100030123     C     *DTAARA       DEFINE    §datiute      ddatiute
056200030123     C                   IN(E)     *DTAARA
056300030123     C                   IF        %Error
056400030123     C                   EVAL      I34Tla = 'L'
056500030123     C                   CALL      'TIBS34R'
056600030123     C                   PARM                    Tibs34Ds
056700030123     C                   IN        *DTAARA
056800030123     C                   ENDIF
056900030123     C*
057000030123     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
057100030123     C                   MOVEL(P)  RSUT          DSFIRS           20
057200030123     C*
057300030123     C                   ENDSR
057400970526     C*------------------------------------------------------------------------*
057500970526     C* *INZSR - OPERAZIONI INIZIALI
057600970526     C*------------------------------------------------------------------------*
057700970526     C     *INZSR        BEGSR
057800970526     C*---
057900970526     C* RICEVIMENTO PARAMETRI
058000970526     C*---
058100970526     C     *ENTRY        PLIST
058200970612     C                   PARM                    KPJBA
058300000407     C                   MOVEL     KPJBU         PARAM
058400150202     C*chiave inserimento
058500150202     C     kinse         kLIST
058600150202     C                   kfld                    vidKSU
058700150211     C                   kfld                    vidksc
058800150211      *
058900150211     C* LETTURA cnaco00f -
059000150211     C     K03ACO00      KLIST
059100150211     C                   KFLD                    ACOkut
059200150211     C                   KFLD                    ACOkcc
059300150211     C                   KFLD                    ACOksc
059400150202      *chiave modifica
059500150202     C     kmodif        kLIST
059600150202     C                   kfld                    pKSU
059700150211     C                   kfld                    pksc
059800000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
059900000407     C     POP1          IFEQ      'O05'
060000000407     C                   SETON                                        90
060100000407     C                   ENDIF
060200000407     C     POP1          IFEQ      'O02'
060300000407     C                   SETON                                        91
060400000407     C                   ENDIF
060500030123     C*
060600030123     C* REPERIMENTO DATI UTENTE
060700030123     C                   EXSR      REPDATIUTE
060800000407     C*
060900000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
061000150211     C                   SETOFF                                       83
061100970526     C*
061200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
061300970526     C     XMSGPA        PLIST
061400970526     C                   PARM                    DSMSMS
061500970526     C                   PARM                    DSMSPG
061600970526     C                   PARM                    DSMSKE
061700970526     C                   PARM                    DSMSER
061800970526     C*
061900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
062000970526     C                   CALL      'XKEY'
062100970526     C                   PARM                    DSKEY
062200970526     C*---
062300970526     C* VARIABILI NON RIFERITE AL DATA BASE
062400970526     C*---
062500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
062600970526     C*---
062700970526     C* VARIABILI DI CONTROLLO
062800970526     C*---
062900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
063000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
063100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
063200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
063300970526     C*
063400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
063500150211     C                   MOVEL     'TISIG13R'    DSMSPG
063600150211     C                   MOVEL     'TISIG13R'    MSGQUE
063700970526     C                   MOVEL     *BLANKS       MSGKEY
063800970526     C                   MOVEL     *BLANKS       DSMSMS
063900000407     C*
064000000407     C* CALCOLA LA DATA CORRENTE
064100000407     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
064200000407     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
064300000407     C                   Z-ADD     WN8           G08DAT
064400000407     C                   Z-ADD     *ZEROS        G08INV
064500000407     C                   MOVEL     '0'           G08ERR
064600000407     C                   CALL      'XSRDA8'
064700000407     C                   PARM                    WLBDA8
064800000407     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
064900970526     C*
065000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
065100970526     C                   EXSR      IMPFUN
065200970526     C*
065300970526     C                   ENDSR
065400970526     C*------------------------------------------------------------------------*
065500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
065600970526     C*------------------------------------------------------------------------*
065700970526     C     FUNGES        BEGSR
065800970526     C*
065900970526     C* PULISCO CAMPI DI LAVORO
066000970526     C                   Z-ADD     0             W                 5 0
066100970526     C*
066200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
066300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
066400970526     C     1             DO        24            I
066500970526     C                   MOVEL     FUD(I)        FUDRIG
066600970526     C*
066700970526     C* PULIZIA FUNZIONI PRECEDENTI
066800970526     C                   MOVEL     *BLANKS       FUV(I)
066900970526     C*
067000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
067100970526     C* (SALTA F24)
067200970526     C     FUDATT        IFEQ      'S'
067300970526     C     FUDFUN        ANDNE     '24'
067400970526     C                   ADD       1             W
067500970526     C                   MOVEL     FUDDES        FUV(W)
067600970526     C                   END
067700970526     C                   END                                                    DO WHILE
067800970526     C*
067900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
068000970526     C* ALTRIMENTI DISATTIVO F24
068100970526     C     1             DO        24            I
068200970526     C                   MOVEL     FUD(I)        FUDRIG
068300970526     C*
068400970526     C     FUDFUN        IFEQ      '24'
068500970526     C     W             IFGT      6
068600970526     C                   MOVEL     'S'           FUDATT
068700970526     C                   ELSE
068800970526     C                   MOVEL     *BLANKS       FUDATT
068900970526     C                   END                                                    W GT 8
069000970526     C                   MOVEL     FUDRIG        FUD(I)
069100970526     C                   END
069200970526     C                   END                                                    DO WHILE
069300970526     C*
069400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
069500970526     C     W             DIV       6             I
069600970526     C                   MVR                     Z
069700970526     C*
069800970526     C     Z             IFNE      0
069900970526     C     I             ADD       1             Z
070000970526     C                   ELSE
070100970526     C                   Z-ADD     I             Z                 5 0
070200970526     C                   END
070300970526     C*
070400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
070500970526     C                   ADD       1             FUNGRU
070600970526     C     FUNGRU        IFGT      Z
070700970526     C     FUNGRU        ORLT      1
070800970526     C                   Z-ADD     1             FUNGRU
070900970526     C                   END
071000970526     C*
071100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
071200970526     C     FUNGRU        IFEQ      1
071300970526     C                   MOVEL     FUV(1)        FUN001
071400970526     C                   MOVEL     FUV(2)        FUN002
071500970526     C                   MOVEL     FUV(3)        FUN003
071600970526     C                   MOVEL     FUV(4)        FUN004
071700970526     C                   MOVEL     FUV(5)        FUN005
071800970526     C                   MOVEL     FUV(6)        FUN006
071900970526     C                   END
072000970526     C*
072100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
072200970526     C     FUNGRU        IFEQ      2
072300970526     C                   MOVEL     FUV(7)        FUN001
072400970526     C                   MOVEL     FUV(8)        FUN002
072500970526     C                   MOVEL     FUV(9)        FUN003
072600970526     C                   MOVEL     FUV(10)       FUN004
072700970526     C                   MOVEL     FUV(11)       FUN005
072800970526     C                   MOVEL     FUV(12)       FUN006
072900970526     C                   END
073000970526     C*
073100970526     C* TERZO GRUPPO DI 6 FUNZIONI
073200970526     C     FUNGRU        IFEQ      3
073300970526     C                   MOVEL     FUV(13)       FUN001
073400970526     C                   MOVEL     FUV(14)       FUN002
073500970526     C                   MOVEL     FUV(15)       FUN003
073600970526     C                   MOVEL     FUV(16)       FUN004
073700970526     C                   MOVEL     FUV(17)       FUN005
073800970526     C                   MOVEL     FUV(18)       FUN006
073900970526     C                   END
074000970526     C*
074100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
074200970526     C     FUNGRU        IFEQ      4
074300970526     C                   MOVEL     FUV(19)       FUN001
074400970526     C                   MOVEL     FUV(20)       FUN002
074500970526     C                   MOVEL     FUV(21)       FUN003
074600970526     C                   MOVEL     FUV(22)       FUN004
074700970526     C                   MOVEL     *BLANKS       FUN005
074800970526     C                   MOVEL     *BLANKS       FUN006
074900970526     C                   END
075000970526     C*
075100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
075200970526     C                   MOVEL     *BLANKS       FUN007
075300970526     C                   MOVEL     *BLANKS       FUN008
075400970526     C*
075500970526     C     1             DO        24            I
075600970526     C                   MOVEL     FUD(I)        FUDRIG
075700970526     C     FUDFUN        IFEQ      '24'
075800970526     C     FUDATT        ANDEQ     'S'
075900970526     C                   MOVEL     FUDDES        FUN008
076000970526     C                   END
076100970526     C                   END                                                    DO
076200970526     C*
076300970526     C                   ENDSR
076400970526     C*------------------------------------------------------------------------*
076500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
076600970526     C*------------------------------------------------------------------------*
076700970526     C     FUNCON        BEGSR
076800970526     C*
076900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
077000970526     C                   EXSR      FUNVAR
077100970526     C*
077200970526     C* PULISCO CAMPI DI LAVORO
077300970526     C                   Z-ADD     0             I                 5 0
077400970526     C                   Z-ADD     0             W                 5 0
077500970526     C*
077600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
077700970526     C     FUNKEY        IFNE      *BLANKS
077800970526     C     1             DO        24            I
077900970526     C                   MOVEL     FUD(I)        FUDRIG
078000970526     C*
078100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
078200970526     C     FUNKEY        IFEQ      FUDFUN
078300970526     C     FUDATT        ANDEQ     'S'
078400970526     C                   Z-ADD     I             W
078500970526     C                   END
078600970526     C                   END                                                    DO WHILE
078700970526     C*
078800970526     C* FUNZIONE NON ATTIVA (ERRORE)
078900970526     C     W             IFEQ      0
079000970526     C*
079100970526     C* EMISSIONE MESSAGGIO DI ERRORE
079200970526     C                   MOVEL     MSG(1)        DSMSMS
079300970526     C*
079400970526     C                   END                                                    W=0
079500970526     C                   END                                                    FUNKEY NE *BLANKS
079600970526     C*
079700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
079800970526     C     W             IFNE      0
079900970526     C     FUNKEY        OREQ      *BLANKS
080000970526     C     FUNKEY        IFEQ      *BLANKS
080100970526     C                   EXSR      ENTGES
080200970526     C                   END
080300970526     C     FUNKEY        IFEQ      '03'
080400970526     C                   EXSR      F03GES
080500970526     C                   END
080600970526     C     FUNKEY        IFEQ      '06'
080700970526     C                   EXSR      F06GES
080800970526     C                   END
080900970526     C     FUNKEY        IFEQ      '12'
081000970526     C                   EXSR      F12GES
081100970526     C                   END
081200970526     C     FUNKEY        IFEQ      '24'
081300970526     C                   EXSR      F24GES
081400970526     C                   END
081500970526     C                   END
081600970526     C*
081700970526     C                   ENDSR
081800970526     C*------------------------------------------------------------------------*
081900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
082000970526     C*------------------------------------------------------------------------*
082100970526     C     FUNVAR        BEGSR
082200970526     C*
082300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
082400970526     C*
082500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
082600970526     C     KEY           IFEQ      ENTER
082700970526     C                   MOVEL     *BLANKS       FUNKEY
082800970526     C                   END
082900970526     C     KEY           IFEQ      F1
083000970526     C                   MOVEL     '01'          FUNKEY
083100970526     C                   END
083200970526     C     KEY           IFEQ      F2
083300970526     C                   MOVEL     '02'          FUNKEY
083400970526     C                   END
083500970526     C     KEY           IFEQ      F3
083600970526     C                   MOVEL     '03'          FUNKEY
083700970526     C                   END
083800970526     C     KEY           IFEQ      F4
083900970526     C                   MOVEL     '04'          FUNKEY
084000970526     C                   END
084100970526     C     KEY           IFEQ      F5
084200970526     C                   MOVEL     '05'          FUNKEY
084300970526     C                   END
084400970526     C     KEY           IFEQ      F6
084500970526     C                   MOVEL     '06'          FUNKEY
084600970526     C                   END
084700970526     C     KEY           IFEQ      F7
084800970526     C                   MOVEL     '07'          FUNKEY
084900970526     C                   END
085000970526     C     KEY           IFEQ      F8
085100970526     C                   MOVEL     '08'          FUNKEY
085200970526     C                   END
085300970526     C     KEY           IFEQ      F9
085400970526     C                   MOVEL     '09'          FUNKEY
085500970526     C                   END
085600970526     C     KEY           IFEQ      F10
085700970526     C                   MOVEL     '10'          FUNKEY
085800970526     C                   END
085900970526     C     KEY           IFEQ      F11
086000970526     C                   MOVEL     '11'          FUNKEY
086100970526     C                   END
086200970526     C     KEY           IFEQ      F12
086300970526     C                   MOVEL     '12'          FUNKEY
086400970526     C                   END
086500970526     C     KEY           IFEQ      F13
086600970526     C                   MOVEL     '13'          FUNKEY
086700970526     C                   END
086800970526     C     KEY           IFEQ      F14
086900970526     C                   MOVEL     '14'          FUNKEY
087000970526     C                   END
087100970526     C     KEY           IFEQ      F15
087200970526     C                   MOVEL     '15'          FUNKEY
087300970526     C                   END
087400970526     C     KEY           IFEQ      F16
087500970526     C                   MOVEL     '16'          FUNKEY
087600970526     C                   END
087700970526     C     KEY           IFEQ      F17
087800970526     C                   MOVEL     '17'          FUNKEY
087900970526     C                   END
088000970526     C     KEY           IFEQ      F18
088100970526     C                   MOVEL     '18'          FUNKEY
088200970526     C                   END
088300970526     C     KEY           IFEQ      F19
088400970526     C                   MOVEL     '19'          FUNKEY
088500970526     C                   END
088600970526     C     KEY           IFEQ      F20
088700970526     C                   MOVEL     '20'          FUNKEY
088800970526     C                   END
088900970526     C     KEY           IFEQ      F21
089000970526     C                   MOVEL     '21'          FUNKEY
089100970526     C                   END
089200970526     C     KEY           IFEQ      F22
089300970526     C                   MOVEL     '22'          FUNKEY
089400970526     C                   END
089500970526     C     KEY           IFEQ      F23
089600970526     C                   MOVEL     '23'          FUNKEY
089700970526     C                   END
089800970526     C     KEY           IFEQ      F24
089900970526     C                   MOVEL     '24'          FUNKEY
090000970526     C                   END
090100970526     C     KEY           IFEQ      ROLLUP
090200970526     C                   MOVEL     'RU'          FUNKEY
090300970526     C                   END
090400970526     C     KEY           IFEQ      HELP
090500970526     C                   MOVEL     'HP'          FUNKEY
090600970526     C                   END
090700970526     C                   ENDSR
090800970526     C*------------------------------------------------------------------------*
090900970526     C* NOTE COSTRUZIONE SCHIERE
091000970526     C*------------------------------------------------------------------------*
091100970526     C*
091200970526     C* -MSG-MESSAGGI
091300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
091400970526     C* (NON TOCCARE MSG,1 E MSG,2)
091500970526     C*
091600970526     C* -CMD-COMANDI
091700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
091800970526     C*
091900970526     C* -OPD-OPZIONI
092000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
092100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
092200970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
092300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
092400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
092500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
092600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
092700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
092800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
092900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
093000970526     C*
093100970526     C* -FUD-FUNZIONI
093200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
093300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
093400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
093500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
093600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
093700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
093800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
093900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
094000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
094100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
094200970526     O*------------------------------------------------------------------------*
094300970526** DEC - DECODIFICHE
094400970526    MODIFICA                                                      1
094500970526  ANNULLAMENTO                                                    2
094600970526VISUALIZZAZIONE                                                   3
094700021211  INSERIMENTO                                                     4
094800021211     COPIA                                                        5
094900150210    GESTIONE                                                      6
095000970528** MSG - MESSAGGI
095100150211TISIG13R- Tasto funzionale non gestito                                          1
095200150211TISIG13R- Inserimento effettuato                                                2
095300150211TISIG13R- Modifica effettuata                                                   3
095400150211TISIG13R- Annullamento effettuato                                               4
095500150211TISIG13R- Copia effettuata                                                      5
095600150211TISIG13R- Richiesta funzione per record inesistente                             6
095700150212TISIG13R- Impossibile inserire, cliente e abbinato già presenti                 7                  7
095800150211TISIG13R- Richiesta funzione per record esistente                               8
095900150226TISIG13R- Impossibile annullare il codice cliente raggruppamento bancali        9
096000150224TISIG13R- Codice cliente raggruppamento bancali non corretto                   10
096100150212TISIG13R- Codice abbinato già presente legato ad altro codice                  11
096200150211TISIG13R- Il campo è obbligatorio                                              12
096300150211TISIG13R- 13                                                                   13
096400150211TISIG13R- 14                                                                   14
096500150211TISIG13R- 15                                                                   15
096600150211TISIG13R- 16                                                                   16
096700150211TISIG13R- 17                                                                   17
09680002121118                                                                             18
09690002121119                                                                             19
09700002121120                                                                             20
097100970526** FUD - FUNZIONI
097200970526                   -        (S/N)=N                        funz.01¬
097300150211                   -        (S/N)=N                        funz.02¬
097400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
097500970526                   -        (S/N)=N                        funz.04¬
097600970526                   -        (S/N)=N                        funz.05¬
097700000407F06=Conferma       -        (S/N)=S                        funz.06¬
097800970527                   -        (S/N)=N                        funz.07¬
097900970527                   -        (S/N)=N                        funz.08¬
098000970527                   -        (S/N)=N                        funz.09¬
098100970527                   -        (S/N)=N                        funz.10¬
098200970526                   -        (S/N)=N                        funz.11¬
098300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
098400970526                   -        (S/N)=N                        funz.13¬
098500970526                   -        (S/N)=N                        funz.14¬
098600970526                   -        (S/N)=N                        funz.15¬
098700970526                   -        (S/N)=N                        funz.16¬
098800970526                   -        (S/N)=N                        funz.17¬
098900970526                   -        (S/N)=N                        funz.18¬
099000970526                   -        (S/N)=N                        funz.19¬
099100970526                   -        (S/N)=N                        funz.20¬
099200970526                   -        (S/N)=N                        funz.21¬
099300970526                   -        (S/N)=N                        funz.22¬
099400970526                   -        (S/N)=N                        funz.23¬
099500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
