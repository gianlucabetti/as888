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
001100160608     Fazfgb02l  uF   E           K DISK    rename(azfgb000:fgb2) prefix(x)
001200150210     Fcnaco00f  IF   E           K DISK
001300160608     Fazcgb01l  iF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D*------------
001800970526     D* DECODIFICHE
001900970526     D*------------
002000150210     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* MESSAGGI
002300970526     D*------------
002400970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* DESCRIZIONI TASTI FUNZIONALI
002700970526     D*------------
002800970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* FUNZIONI DA CARICARE A VIDEO
003100970526     D*------------
003200970526     D FUV             S             19    DIM(24)
003300970526     D*------------------------------------------------------------------------*
003400970526     D* INPUT
003500970526     D*------------------------------------------------------------------------*
003600970526     D*------------
003700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003800970526     D*------------
003900970526     D FUDDS           DS
004000970526     D  FUDDE1                 1      1
004100970526     D  FUDFUN                 2      3
004200970526     D  FUDDE2                 4     19
004300970526     D  FUDFI0                20     34
004400970526     D  FUDATT                35     35
004500970526     D  FUDFI1                36     80
004600970526     D  FUDDES                 1     19
004700970526     D  FUDRIG                 1     80
004800970526     D*------------
004900970526     D* DS GESTIONE TASTI FUNZIONALI
005000970526     D*------------
005100970526     D DSKEY         E DS
005200970526     D DEVDS1          DS
005300970526     D  KEY                  369    369
005400970526     D*------------
005500970526     D* DS "XMSG" - GESTIONE MESSAGGI
005600970526     D*------------
005700970526     D DSMSG         E DS
005800970528     D*------------------
005900970526     D* ARCHITETTURA
006000970526     D*------------
006100970526     D KPJBA         E DS
006200150203     D savkpjbu        s                   like(kpjbu)
006300000407     D*------------------
006400000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
006500000407     D*------------------
006600000407     D WLBDA8          DS                  INZ
006700000407     D  G08DAT                 1      8  0
006800000407     D  G08INV                 9     16  0
006900000407     D  G08ERR                17     17
007000000407     D  G08TGI                18     22  0
007100030123     D*------------
007200030123     D* DS REPERIMENTO DATI UTENTE
007300030123     D*------------
007400030123     D TIBS34DS      E DS                                                       *Profili utente
007500030123     D DDATIUTE      E DS                                                       *Dati utente
007600030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007700030207     D*------------------
007800030207     D* DS REPERIMENTO / CONTROLLO DA TABELLE
007900030207     D*------------------
008000030207     D TIBS02DS      E DS
008100150210     D xclirds       E DS
008200150210     d ksa             s              4    dim(30)
008300150210     d ksc             s              7  0 dim(30)
008400150210     d                 ds
008500150210     d dsksn                   1      4p 0
008600150210     d dsksa                   1      4
008700030207     D*------------------
008800150202     d Tibs69DS      E DS                  INZ
008900150202     d DS_cnaco      E DS                  extname(CNACO00F)
009000150202     d DS_cnind      E DS                  extname(CNIND00F)
009100150202     d DS_cnclp      E DS                  extname(CNCLP00F)
009200150202     d DS_fncls      E DS                  extname(FNCLS00F)
009300000407     D*------------------
009400000407     D* DS PASSAGGIO PARAMETRI
009500000407     D*------------------
009600000407     D PARAM           DS                  INZ
009700150202     d  pop1                          3
009800150202     D  Pksu                          8
009900150211     D  Pksc                          8
010000150202     D  Pmsg                         80
010100970526     C*------------------------------------------------------------------------*
010200970526     C* MAIN LINES
010300970526     C*------------------------------------------------------------------------*
010400970526     C*
010500970526     C* NON CI SONO ERRORI IN ENTRATA
010600021211     C     ERRL00        IFEQ      '0'
010700970526     C*
010800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010900021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011200970526     C*
011300150211      * cerco i dati anagrafici
011400150211     c                   clear                   tibs69ds
011500150211     c                   move      pksu          vidksu
011600150211     c                   move      vidksu        i69kac
011700150211     c                   call      'TIBS69R'
011800150211     c                   parm                    tibs69ds
011900150211     c                   parm                    ds_cnaco
012000150211     c                   parm                    ds_cnind
012100150211     c                   parm                    ds_cnclp
012200150211     c                   parm                    ds_fncls
012300150211      * ragione sociale
012400150211     c                   eval      vidksud = acorag
012500970526     C* EMETTE FORMATO VIDEO
012600970526     C                   EXSR      BUTFMR
012700970526     C*
012800970526     C* GESTIONE TASTI FUNZIONALI
012900970526     C                   EXSR      FUNCON
013000021211     C                   ENDDO
013100970526     C*
013200021211     C                   ENDIF
013300021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
013400021211     C                   MOVEL     PARAM         KPJBU
013500970526     C* FINE PGM
013600970526     C                   SETON                                        LR
013700970526     C*------------------------------------------------------------------------*
013800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013900970526     C*------------------------------------------------------------------------*
014000970526     C     BUTFMR        BEGSR
014100970526     C*
014200970526     C* EMETTE LA TESTATA
014300021211     C                   WRITE     SIG2D01
014400970526     C*
014500970526     C* SPEDISCE EVENTUALE MESSAGGIO
014600970526     C                   EXSR      SNDMSG
014700970526     C*
014800970526     C* EMETTE LE FUNZIONI
014900021211     C                   WRITE     SIG2D02
015000970526     C*
015100970526     C* EMETTE IL VIDEO
015200021211     C                   EXFMT     SIG2D03
015300970526     C*
015400970526     C* REIMPOSTA FLAG DI ERRORE
015500970526     C                   MOVEL     '0'           ERRL00
015600970526     C*
015700970526     C                   ENDSR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016000970526     C*------------------------------------------------------------------------*
016100970526     C     SNDMSG        BEGSR
016200970526     C*
016300970526     C* PULISCE IL SFILE MESSAGGI
016400970526     C                   MOVEL     '1'           *IN40
016500021211     C                   WRITE     SIG2DMC
016600970526     C                   MOVEL     '0'           *IN40
016700970526     C*
016800970526     C* VISUALIZZA IL MESSAGGIO
016900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017000970526     C                   CALL      'XMSG'        XMSGPA
017100021211     C     DSMSER        IFEQ      *ZEROS
017200970526     C                   MOVEL     DSMSKE        MSGKEY
017300021211     C                   ENDIF
017400970526     C                   Z-ADD     1             MSGULT
017500021211     C                   WRITE     SIG2DMS
017600021211     C                   WRITE     SIG2DMC
017700970526     C*
017800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017900970526     C                   MOVEL     *BLANKS       DSMSMS
018000970526     C*
018100970526     C                   ENDSR
018200150210     C*------------------------------------------------------------------------*
018300150210     C* PASPAR - ASSEGNAZIONE VALORI PER PASSAGIO PARAMETRI
018400150210     C*------------------------------------------------------------------------*
018500150210     C     PASPAR        BEGSR
018600150210     C*
018700150210     C                   MOVEL     PARAM         KPJBU
018800150210     C*
018900150210     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100970526     C* ENTGES - GESTIONE ENTER -> INVIO
019200970526     C*------------------------------------------------------------------------*
019300970526     C     ENTGES        BEGSR
019400970526     C*
019500970526     C* CONTROLLA I CAMPI IMMESSI
019600970526     C                   EXSR      DECODI
019700970526     C*
019800970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
019900021211     C     POP1          IFEQ      'O05'
020000021211     C                   MOVEL     *BLANKS       PMSG
020100970526     C                   MOVEL     '1'           FINL00
020200021211     C                   ENDIF
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F03GES - GESTIONE F03 -> FINE
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F03GES        BEGSR
020900970526     C*
021000970526     C* IMPOSTA "PREMUTO F03"
021100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F06GES        BEGSR
021800970526     C*
021900021211     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
022000150204     C     POP1          IFEQ      'F06'
022100021211     C                   EXSR      INSERI
022200021211     C                   ENDIF
022300021211     C     POP1          IFEQ      'O02'
022400021211     C                   EXSR      MODIFI
022500021211     C                   ENDIF
022600021211     C     POP1          IFEQ      'O03'
022700021211     C                   EXSR      COPIA
022800021211     C                   ENDIF
022900021211     C     POP1          IFEQ      'O04'
023000021211     C                   EXSR      ANNULL
023100021211     C                   ENDIF
023200021211     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* F12GES - GESTIONE F12 -> RITORNO
023600970526     C*------------------------------------------------------------------------*
023700970526     C     F12GES        BEGSR
023800970526     C*
023900970526     C* IMPOSTA "PREMUTO F12"
024000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024100970526     C*
024200970526     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024500970526     C*------------------------------------------------------------------------*
024600970526     C     F24GES        BEGSR
024700970526     C*
024800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024900970526     C*
025000970526     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
025300970526     C*------------------------------------------------------------------------*
025400970526     C     INSERI        BEGSR
025500970526     C*
025600970526     C* CONTROLLA I CAMPI IMMESSI
025700970526     C                   EXSR      DECODI
025800160316     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025900021211     C                   EXSR      DECODINS
026000160316     c                   endif
026100970526     C*
026200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026400150211     C                   CLEAR                   Azfgb000
026500021211     C                   EXSR      IMPCAM
026600150211     C                   WRITE     AZfgb000
026700150226     C                   MOVEL     MSG(2)        PMSG                           *MESSAGGIO
026800970602     C*
026900970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
027000970602     C                   EXSR      AZZVID
027100021211     C                   ENDIF
027200970526     C*
027300970526     C                   ENDSR
027400970526     C*------------------------------------------------------------------------*
027500970526     C* MODIFI - OPERAZIONI DI MODIFICA
027600970526     C*------------------------------------------------------------------------*
027700970526     C     MODIFI        BEGSR
027800970526     C*
027900970526     C* CONTROLLA I CAMPI IMMESSI
028000970526     C                   EXSR      DECODI
028100970526     C*
028200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
028300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028400970619     C                   EXSR      LETCAS
028500970526     C*
028600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028700021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028800000407     C                   EXSR      IMPCAM
028900150211     C                   UPDATE    AZfgb000
029000150226     C                   MOVEL     MSG(3)        PMSG
029100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029200021211     C                   ENDIF
029300021211     C                   ENDIF
029400970526     C*
029500970526     C                   ENDSR
029600021211     C*------------------------------------------------------------------------*
029700021211     C* COPIA - OPERAZIONI DI COPIA RECORD
029800021211     C*------------------------------------------------------------------------*
029900021211     C     COPIA         BEGSR
030000021211     C*
030100021211     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030200021211     C                   EXSR      LETCAS
030300021211     C*
030400021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030500021211     C*
030600021211     C* CONTROLLA I CAMPI IMMESSI
030700021211     C                   EXSR      DECODI
030800021211     C                   EXSR      DECODINS
030900021211     C*
031000021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031100150211     C                   CLEAR                   AZfgb000
031200021211     C                   EXSR      IMPCAM
031300150211     C                   WRITE     AZfgb000
031400150226     C                   MOVEL     MSG(5)        PMSG                           *MESSAGGIO
031500021211     C                   MOVEL     '1'           FINL00                         *FINE PGM
031600021211     C                   ENDIF
031700021211     C                   ENDIF
031800021211     C*
031900021211     C                   ENDSR
032000970526     C*------------------------------------------------------------------------*
032100970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
032200970526     C*------------------------------------------------------------------------*
032300970526     C     ANNULL        BEGSR
032400000411     C*
032500000411     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032600000411     C                   EXSR      LETCAS
032700150219     c                   if        vidksu = vidksc
032800150219     c                   move      '1'           errl00
032900150219     c                   seton                                        83
033000150219     c                   movel     msg(09)       dsmsms
033100150219     c                   endif
033200000411     C*
033300000411     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
033400000411     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033500150211     c                   move      'A'           fgbann
033600150211     C                   update    AZfgb000
033700150226     C                   MOVEL     MSG(4)        PMSG
033800000411     C                   MOVEL     '1'           FINL00                         *FINE PGM
033900000411     C                   ENDIF
034000000411     C*
034100970526     C                   ENDSR
034200970526     C*------------------------------------------------------------------------*
034300970619     C* LETCAS - LEGGE IL RECORD PERL'AGGIORNAMENTO
034400970526     C*------------------------------------------------------------------------*
034500970619     C     LETCAS        BEGSR
034600970526     C*
034700150211     C     kmodif        CHAIN     AZfgb01L                           99
034800021211     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
034900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035000150226     C                   MOVEL     MSG(6)        PMSG                           *MESSAGGIO
035100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035200021211     C                   ENDIF
035300970526     C*
035400970526     C                   ENDSR
035500970526     C*------------------------------------------------------------------------*
035600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
035700970526     C*------------------------------------------------------------------------*
035800970526     C     DECODI        BEGSR
035900000407     C*
036000000407     C                   MOVEL     '0'           ERRL00
036100000407     C*
036200000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
036300150211     C                   SETOFF                                       83
036400030207     C*--------------------
036500150202     C* CONTROLLO ESISTENZA validità CODICE cliente unificante
036600030207     C*--------------------
036700030207     C     ERRL00        IFEQ      '0'
036800150211     C     VIDksc        ANDNE     *blanks
036900150211     C                   EXSR      CNTksc
037000030207     C                   ENDIF
037100000407     C*--------------------
037200000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
037300000407     C*--------------------
037400000407     C     ERRL00        IFEQ      '0'
037500150211     C     VIDksc        IFEQ      *blanks
037600150219     C     VIDksc        oreq      *zeros
037700150202     C                   MOVEL     '1'           ERRL00
037800150202     C                   SETON                                        83
037900150202     C                   ENDIF
038000000407     C     ERRL00        IFEQ      '1'
038100000407     C                   MOVEL     MSG(12)       DSMSMS
038200000407     C                   ENDIF
038300000407     C                   ENDIF
038400000407     C*
038500970528     C                   ENDSR
038600021211     C*------------------------------------------------------------------------*
038700021211     C* DECODINS - CONTROLLA CAMPI E DECODIFICA X INSERIMENTO
038800021211     C*------------------------------------------------------------------------*
038900021211     C     DECODINS      BEGSR
039000021211     C*
039100021211     C                   MOVEL     '0'           ERRL00
039200021211     C*
039300021211     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
039400150211     C                   SETOFF                                       83
039500021211     C*--------------------
039600021211     C* CONTROLLO GIA' ESISTENZA CODICE ARTICOLO
039700021211     C*--------------------
039800021211     C     ERRL00        IFEQ      '0'
039900150204     C     POP1          IFEQ      'F06'
040000021211     C     POP1          OREQ      'O03'
040100150302     C     kinse         setll     AZfgb01L
040200150302     c                   do        *hival
040300150302     C     kinse         reade     AZfgb01L
040400150302     C                   IF        %eof(azfgb01l)
040500150302     c                   leave
040600150302     c                   endif
040700150302     C                   IF        fgbann <> *blank
040800150302     c                   iter
040900150302     c                   endif
041000021211     C                   MOVEL     '1'           ERRL00
041100150211     C                   SETON                                        83
041200021211     C                   MOVEL     MSG(7)        DSMSMS
041300150212     c                   leavesr
041400150302     C                   ENDdo
041500150302     c
041600150302     C     vidksc        setll     AZfgb02L
041700150302     c                   do        *hival
041800150302     C     vidksc        reade     AZfgb02L
041900150302     C                   IF        %eof(azfgb02l)
042000150302     c                   leave
042100150302     c                   endif
042200150302     C                   IF        xfgbann <> *blank
042300150302     c                   iter
042400150302     c                   endif
042500160608     c                   clear                   valido            1
042600160608     C     xfgbksu       setll     AZcgb01L
042700160608     c                   do        *hival
042800160608     C     xfgbksu       reade     AZcgb01L
042900160608     C                   IF        %eof(azcgb01l)
043000160608     c                   leave
043100160608     c                   endif
043200160608     c                   if        cgbdtf > datcor
043300160608     c                   move      'X'           valido
043400160608     c                   leave
043500160608     c                   endif
043600160608     C                   ENDdo
043700160608     c                   if        valido <> *blank
043800150212     C                   MOVEL     '1'           ERRL00
043900150212     C                   SETON                                        83
044000150212     C                   MOVEL     MSG(11)       DSMSMS
044100160608     c                   move      ' '           xfgbann
044200160608     c                   except    vuoto
044300150302     c                   leave
044400160608     c                   else
044500160608     c                   move      'A'           xfgbann
044600160608     c                   update    fgb2
044700160608     c                   endif
044800160608     c                   enddo
044900021211     C                   ENDIF
045000021211     C                   ENDIF
045100021211     C*
045200021211     C                   ENDSR
045300150202     C*------------------------------------------------------------------------*
045400150211     C* CNTksc - CONTROLLA PRESENZA DEL CODICE ARTICOLO IN TABELLA "TBA"
045500150202     C*------------------------------------------------------------------------*
045600150211     C     CNTksc        BEGSR
045700150202     C*
045800150210     C                   CLEAR                   xclirds
045900150202     C*
046000150211     C     '?'           SCAN      VIDksc                                 30
046100150202     C*
046200150202     C*  Ricerca
046300150202     C                   IF        *IN30 = *ON
046400150210     C* RICHIESTO ELENCO CLIENTI
046500150210     C                   clear                   xCLIrds
046600150210     C***  DXCaut = OTAA1cabi;
046700150210     C                   EVAL      DXCcap = DUTkci
046800150220     C                   EVAL      DXCAUT = 'AZ'
046900150210     C                   CALL      'XCLIR'
047000150210     C                   PARM                    xCLIrds
047100150210     C                   EVAL      ksa = DXCksc
047200150210     C                   EVAL      Dsksa = ksa(1)
047300150210     C                   EVAL      ksc(1) = dsksn
047400150211     C                   move      ksc(1)        vidksc
047500150202     C                   ELSE
047600150202     C*  Controllo
047700150210     C                   Z-ADD     1             ACOkut
047800150210     C                   Z-ADD     DUTKCI        ACOkcc
047900160316     c                   if        %subst(vidksc:8:1) = *blank
048000160316     c                   move      '1'           errl00
048100160316     C                   SETON                                        83
048200160316     C                   MOVEL     MSG(10)       DSMSMS
048300160316     c                   leavesr
048400160316     C                   ENDIF
048500160316     c
048600160316     C                   move      vidksc        acoksc
048700150210     C     K03ACO00      CHAIN     cnaco00f
048800150210     C                   IF        %found(cnaco00f)
048900150211     C                   MOVEL     ACOrag        Vidkscd
049000150202     C                   ELSE
049100150202     C                   MOVEL     '1'           ERRL00
049200150202     C                   ENDIF
049300160316     C                   ENDIF
049400150202     C*
049500150202     C                   ENDSR
049600970526     C*------------------------------------------------------------------------*
049700970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
049800970526     C*------------------------------------------------------------------------*
049900970526     C     IREVID        BEGSR
050000000407     C*
050100150211     C                   MOVEL     pksu          VIDksu
050200150211     C                   MOVEL     fgbksc        VIDksc
050300970527     C*
050400970526     C                   ENDSR
050500970526     C*------------------------------------------------------------------------*
050600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
050700970526     C*------------------------------------------------------------------------*
050800970526     C     AZZVID        BEGSR
050900000407     C*
051000150211     C                   MOVEL     *BLANKS       VIDksc
051100150211     C                   MOVEL     *BLANKS       VIDkscd
051200970526     C                   ENDSR
051300000407     C*------------------------------------------------------------------------*
051400000407     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
051500000407     C*------------------------------------------------------------------------*
051600000407     C     IMPCAM        BEGSR
051700000407     C*
051800150211     C                   MOVEL     VIDksu        fgbksu
051900150211     C                   MOVEL     VIDksc        fgbksc
052000150211     c                   z-add     wn14          fgbdtu
052100150211     c                   move      knmus         fgbute
052200000407     C                   ENDSR
052300970526     C*------------------------------------------------------------------------*
052400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
052500970526     C*------------------------------------------------------------------------*
052600970526     C     IMPFUN        BEGSR
052700970526     C*
052800970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
052900021211     C     POP1          IFEQ      'O05'
053000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
053100970526     C                   MOVEL     'N'           FUDATT
053200970526     C                   MOVEL     FUDRIG        FUD(6)
053300021211     C                   ENDIF
053400970526     C*
053500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
053600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
053700970526     C                   EXSR      FUNGES
053800970526     C*
053900000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
054000150204     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
054100000407     C                   EXSR      LETCAS
054200980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
054300980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
054400980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
054500000407     C                   ELSE                                                   *INSERIMENTO
054600970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
054700000407     C                   ENDIF
054800970526     C*
054900021211     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
055000021211     C     POP1          IFEQ      'O02'
055100021211     C                   MOVEL     DEC(1)        VIDFUN
055200021211     C                   ENDIF
055300021211     C     POP1          IFEQ      'O03'
055400021211     C                   MOVEL     DEC(5)        VIDFUN
055500021211     C                   ENDIF
055600021211     C     POP1          IFEQ      'O04'
055700021211     C                   MOVEL     DEC(2)        VIDFUN
055800021211     C                   ENDIF
055900021211     C     POP1          IFEQ      'O05'
056000021211     C                   MOVEL     DEC(3)        VIDFUN
056100021211     C                   ENDIF
056200150204     C     POP1          IFEQ      'F06'
056300021211     C                   MOVEL     DEC(4)        VIDFUN
056400021211     C                   ENDIF
056500150210     C     POP1          IFEQ      'F02'
056600150210     C                   MOVEL     DEC(6)        VIDFUN
056700150210     C                   ENDIF
056800021211     C*
056900021211     C                   ENDSR
057000030123     C*--------------------------------------------------------------------------------------------*
057100030123     C* REPERISCE I DATI UTENTE
057200030123     C*--------------------------------------------------------------------------------------------*
057300030123     C     REPDATIUTE    BEGSR
057400030123     C*
057500030123     C* INIZIALIZZA VARIABILI DI WRK
057600030123     C                   CLEAR                   TIBS34DS
057700030123     C                   CLEAR                   AZUTEDS
057800030123     C                   CLEAR                   DDATIUTE
057900030123     C*
058000030123     C     *DTAARA       DEFINE    §azute        azuteds
058100030123     C     *DTAARA       DEFINE    §datiute      ddatiute
058200030123     C                   IN(E)     *DTAARA
058300030123     C                   IF        %Error
058400030123     C                   EVAL      I34Tla = 'L'
058500030123     C                   CALL      'TIBS34R'
058600030123     C                   PARM                    Tibs34Ds
058700030123     C                   IN        *DTAARA
058800030123     C                   ENDIF
058900030123     C*
059000030123     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
059100030123     C                   MOVEL(P)  RSUT          DSFIRS           20
059200030123     C*
059300030123     C                   ENDSR
059400970526     C*------------------------------------------------------------------------*
059500970526     C* *INZSR - OPERAZIONI INIZIALI
059600970526     C*------------------------------------------------------------------------*
059700970526     C     *INZSR        BEGSR
059800970526     C*---
059900970526     C* RICEVIMENTO PARAMETRI
060000970526     C*---
060100970526     C     *ENTRY        PLIST
060200970612     C                   PARM                    KPJBA
060300000407     C                   MOVEL     KPJBU         PARAM
060400150202     C*chiave inserimento
060500150202     C     kinse         kLIST
060600150202     C                   kfld                    vidKSU
060700150211     C                   kfld                    vidksc
060800150211      *
060900150211     C* LETTURA cnaco00f -
061000150211     C     K03ACO00      KLIST
061100150211     C                   KFLD                    ACOkut
061200150211     C                   KFLD                    ACOkcc
061300150211     C                   KFLD                    ACOksc
061400150202      *chiave modifica
061500150202     C     kmodif        kLIST
061600150202     C                   kfld                    pKSU
061700150211     C                   kfld                    pksc
061800000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
061900000407     C     POP1          IFEQ      'O05'
062000000407     C                   SETON                                        90
062100000407     C                   ENDIF
062200000407     C     POP1          IFEQ      'O02'
062300000407     C                   SETON                                        91
062400000407     C                   ENDIF
062500030123     C*
062600030123     C* REPERIMENTO DATI UTENTE
062700030123     C                   EXSR      REPDATIUTE
062800000407     C*
062900000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
063000150211     C                   SETOFF                                       83
063100970526     C*
063200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
063300970526     C     XMSGPA        PLIST
063400970526     C                   PARM                    DSMSMS
063500970526     C                   PARM                    DSMSPG
063600970526     C                   PARM                    DSMSKE
063700970526     C                   PARM                    DSMSER
063800970526     C*
063900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
064000970526     C                   CALL      'XKEY'
064100970526     C                   PARM                    DSKEY
064200970526     C*---
064300970526     C* VARIABILI NON RIFERITE AL DATA BASE
064400970526     C*---
064500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
064600970526     C*---
064700970526     C* VARIABILI DI CONTROLLO
064800970526     C*---
064900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
065000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
065100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
065200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
065300970526     C*
065400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
065500150211     C                   MOVEL     'TISIG13R'    DSMSPG
065600150211     C                   MOVEL     'TISIG13R'    MSGQUE
065700970526     C                   MOVEL     *BLANKS       MSGKEY
065800970526     C                   MOVEL     *BLANKS       DSMSMS
065900000407     C*
066000000407     C* CALCOLA LA DATA CORRENTE
066100000407     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
066200000407     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
066300000407     C                   Z-ADD     WN8           G08DAT
066400000407     C                   Z-ADD     *ZEROS        G08INV
066500000407     C                   MOVEL     '0'           G08ERR
066600000407     C                   CALL      'XSRDA8'
066700000407     C                   PARM                    WLBDA8
066800000407     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
066900970526     C*
067000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
067100970526     C                   EXSR      IMPFUN
067200970526     C*
067300970526     C                   ENDSR
067400970526     C*------------------------------------------------------------------------*
067500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
067600970526     C*------------------------------------------------------------------------*
067700970526     C     FUNGES        BEGSR
067800970526     C*
067900970526     C* PULISCO CAMPI DI LAVORO
068000970526     C                   Z-ADD     0             W                 5 0
068100970526     C*
068200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
068300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
068400970526     C     1             DO        24            I
068500970526     C                   MOVEL     FUD(I)        FUDRIG
068600970526     C*
068700970526     C* PULIZIA FUNZIONI PRECEDENTI
068800970526     C                   MOVEL     *BLANKS       FUV(I)
068900970526     C*
069000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
069100970526     C* (SALTA F24)
069200970526     C     FUDATT        IFEQ      'S'
069300970526     C     FUDFUN        ANDNE     '24'
069400970526     C                   ADD       1             W
069500970526     C                   MOVEL     FUDDES        FUV(W)
069600970526     C                   END
069700970526     C                   END                                                    DO WHILE
069800970526     C*
069900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
070000970526     C* ALTRIMENTI DISATTIVO F24
070100970526     C     1             DO        24            I
070200970526     C                   MOVEL     FUD(I)        FUDRIG
070300970526     C*
070400970526     C     FUDFUN        IFEQ      '24'
070500970526     C     W             IFGT      6
070600970526     C                   MOVEL     'S'           FUDATT
070700970526     C                   ELSE
070800970526     C                   MOVEL     *BLANKS       FUDATT
070900970526     C                   END                                                    W GT 8
071000970526     C                   MOVEL     FUDRIG        FUD(I)
071100970526     C                   END
071200970526     C                   END                                                    DO WHILE
071300970526     C*
071400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
071500970526     C     W             DIV       6             I
071600970526     C                   MVR                     Z
071700970526     C*
071800970526     C     Z             IFNE      0
071900970526     C     I             ADD       1             Z
072000970526     C                   ELSE
072100970526     C                   Z-ADD     I             Z                 5 0
072200970526     C                   END
072300970526     C*
072400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
072500970526     C                   ADD       1             FUNGRU
072600970526     C     FUNGRU        IFGT      Z
072700970526     C     FUNGRU        ORLT      1
072800970526     C                   Z-ADD     1             FUNGRU
072900970526     C                   END
073000970526     C*
073100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
073200970526     C     FUNGRU        IFEQ      1
073300970526     C                   MOVEL     FUV(1)        FUN001
073400970526     C                   MOVEL     FUV(2)        FUN002
073500970526     C                   MOVEL     FUV(3)        FUN003
073600970526     C                   MOVEL     FUV(4)        FUN004
073700970526     C                   MOVEL     FUV(5)        FUN005
073800970526     C                   MOVEL     FUV(6)        FUN006
073900970526     C                   END
074000970526     C*
074100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
074200970526     C     FUNGRU        IFEQ      2
074300970526     C                   MOVEL     FUV(7)        FUN001
074400970526     C                   MOVEL     FUV(8)        FUN002
074500970526     C                   MOVEL     FUV(9)        FUN003
074600970526     C                   MOVEL     FUV(10)       FUN004
074700970526     C                   MOVEL     FUV(11)       FUN005
074800970526     C                   MOVEL     FUV(12)       FUN006
074900970526     C                   END
075000970526     C*
075100970526     C* TERZO GRUPPO DI 6 FUNZIONI
075200970526     C     FUNGRU        IFEQ      3
075300970526     C                   MOVEL     FUV(13)       FUN001
075400970526     C                   MOVEL     FUV(14)       FUN002
075500970526     C                   MOVEL     FUV(15)       FUN003
075600970526     C                   MOVEL     FUV(16)       FUN004
075700970526     C                   MOVEL     FUV(17)       FUN005
075800970526     C                   MOVEL     FUV(18)       FUN006
075900970526     C                   END
076000970526     C*
076100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
076200970526     C     FUNGRU        IFEQ      4
076300970526     C                   MOVEL     FUV(19)       FUN001
076400970526     C                   MOVEL     FUV(20)       FUN002
076500970526     C                   MOVEL     FUV(21)       FUN003
076600970526     C                   MOVEL     FUV(22)       FUN004
076700970526     C                   MOVEL     *BLANKS       FUN005
076800970526     C                   MOVEL     *BLANKS       FUN006
076900970526     C                   END
077000970526     C*
077100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
077200970526     C                   MOVEL     *BLANKS       FUN007
077300970526     C                   MOVEL     *BLANKS       FUN008
077400970526     C*
077500970526     C     1             DO        24            I
077600970526     C                   MOVEL     FUD(I)        FUDRIG
077700970526     C     FUDFUN        IFEQ      '24'
077800970526     C     FUDATT        ANDEQ     'S'
077900970526     C                   MOVEL     FUDDES        FUN008
078000970526     C                   END
078100970526     C                   END                                                    DO
078200970526     C*
078300970526     C                   ENDSR
078400970526     C*------------------------------------------------------------------------*
078500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
078600970526     C*------------------------------------------------------------------------*
078700970526     C     FUNCON        BEGSR
078800970526     C*
078900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
079000970526     C                   EXSR      FUNVAR
079100970526     C*
079200970526     C* PULISCO CAMPI DI LAVORO
079300970526     C                   Z-ADD     0             I                 5 0
079400970526     C                   Z-ADD     0             W                 5 0
079500970526     C*
079600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
079700970526     C     FUNKEY        IFNE      *BLANKS
079800970526     C     1             DO        24            I
079900970526     C                   MOVEL     FUD(I)        FUDRIG
080000970526     C*
080100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
080200970526     C     FUNKEY        IFEQ      FUDFUN
080300970526     C     FUDATT        ANDEQ     'S'
080400970526     C                   Z-ADD     I             W
080500970526     C                   END
080600970526     C                   END                                                    DO WHILE
080700970526     C*
080800970526     C* FUNZIONE NON ATTIVA (ERRORE)
080900970526     C     W             IFEQ      0
081000970526     C*
081100970526     C* EMISSIONE MESSAGGIO DI ERRORE
081200970526     C                   MOVEL     MSG(1)        DSMSMS
081300970526     C*
081400970526     C                   END                                                    W=0
081500970526     C                   END                                                    FUNKEY NE *BLANKS
081600970526     C*
081700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
081800970526     C     W             IFNE      0
081900970526     C     FUNKEY        OREQ      *BLANKS
082000970526     C     FUNKEY        IFEQ      *BLANKS
082100970526     C                   EXSR      ENTGES
082200970526     C                   END
082300970526     C     FUNKEY        IFEQ      '03'
082400970526     C                   EXSR      F03GES
082500970526     C                   END
082600970526     C     FUNKEY        IFEQ      '06'
082700970526     C                   EXSR      F06GES
082800970526     C                   END
082900970526     C     FUNKEY        IFEQ      '12'
083000970526     C                   EXSR      F12GES
083100970526     C                   END
083200970526     C     FUNKEY        IFEQ      '24'
083300970526     C                   EXSR      F24GES
083400970526     C                   END
083500970526     C                   END
083600970526     C*
083700970526     C                   ENDSR
083800970526     C*------------------------------------------------------------------------*
083900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
084000970526     C*------------------------------------------------------------------------*
084100970526     C     FUNVAR        BEGSR
084200970526     C*
084300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
084400970526     C*
084500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
084600970526     C     KEY           IFEQ      ENTER
084700970526     C                   MOVEL     *BLANKS       FUNKEY
084800970526     C                   END
084900970526     C     KEY           IFEQ      F1
085000970526     C                   MOVEL     '01'          FUNKEY
085100970526     C                   END
085200970526     C     KEY           IFEQ      F2
085300970526     C                   MOVEL     '02'          FUNKEY
085400970526     C                   END
085500970526     C     KEY           IFEQ      F3
085600970526     C                   MOVEL     '03'          FUNKEY
085700970526     C                   END
085800970526     C     KEY           IFEQ      F4
085900970526     C                   MOVEL     '04'          FUNKEY
086000970526     C                   END
086100970526     C     KEY           IFEQ      F5
086200970526     C                   MOVEL     '05'          FUNKEY
086300970526     C                   END
086400970526     C     KEY           IFEQ      F6
086500970526     C                   MOVEL     '06'          FUNKEY
086600970526     C                   END
086700970526     C     KEY           IFEQ      F7
086800970526     C                   MOVEL     '07'          FUNKEY
086900970526     C                   END
087000970526     C     KEY           IFEQ      F8
087100970526     C                   MOVEL     '08'          FUNKEY
087200970526     C                   END
087300970526     C     KEY           IFEQ      F9
087400970526     C                   MOVEL     '09'          FUNKEY
087500970526     C                   END
087600970526     C     KEY           IFEQ      F10
087700970526     C                   MOVEL     '10'          FUNKEY
087800970526     C                   END
087900970526     C     KEY           IFEQ      F11
088000970526     C                   MOVEL     '11'          FUNKEY
088100970526     C                   END
088200970526     C     KEY           IFEQ      F12
088300970526     C                   MOVEL     '12'          FUNKEY
088400970526     C                   END
088500970526     C     KEY           IFEQ      F13
088600970526     C                   MOVEL     '13'          FUNKEY
088700970526     C                   END
088800970526     C     KEY           IFEQ      F14
088900970526     C                   MOVEL     '14'          FUNKEY
089000970526     C                   END
089100970526     C     KEY           IFEQ      F15
089200970526     C                   MOVEL     '15'          FUNKEY
089300970526     C                   END
089400970526     C     KEY           IFEQ      F16
089500970526     C                   MOVEL     '16'          FUNKEY
089600970526     C                   END
089700970526     C     KEY           IFEQ      F17
089800970526     C                   MOVEL     '17'          FUNKEY
089900970526     C                   END
090000970526     C     KEY           IFEQ      F18
090100970526     C                   MOVEL     '18'          FUNKEY
090200970526     C                   END
090300970526     C     KEY           IFEQ      F19
090400970526     C                   MOVEL     '19'          FUNKEY
090500970526     C                   END
090600970526     C     KEY           IFEQ      F20
090700970526     C                   MOVEL     '20'          FUNKEY
090800970526     C                   END
090900970526     C     KEY           IFEQ      F21
091000970526     C                   MOVEL     '21'          FUNKEY
091100970526     C                   END
091200970526     C     KEY           IFEQ      F22
091300970526     C                   MOVEL     '22'          FUNKEY
091400970526     C                   END
091500970526     C     KEY           IFEQ      F23
091600970526     C                   MOVEL     '23'          FUNKEY
091700970526     C                   END
091800970526     C     KEY           IFEQ      F24
091900970526     C                   MOVEL     '24'          FUNKEY
092000970526     C                   END
092100970526     C     KEY           IFEQ      ROLLUP
092200970526     C                   MOVEL     'RU'          FUNKEY
092300970526     C                   END
092400970526     C     KEY           IFEQ      HELP
092500970526     C                   MOVEL     'HP'          FUNKEY
092600970526     C                   END
092700970526     C                   ENDSR
092800970526     C*------------------------------------------------------------------------*
092900970526     C* NOTE COSTRUZIONE SCHIERE
093000970526     C*------------------------------------------------------------------------*
093100970526     C*
093200970526     C* -MSG-MESSAGGI
093300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
093400970526     C* (NON TOCCARE MSG,1 E MSG,2)
093500970526     C*
093600970526     C* -CMD-COMANDI
093700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
093800970526     C*
093900970526     C* -OPD-OPZIONI
094000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
094100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
094200970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
094300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
094400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
094500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
094600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
094700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
094800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
094900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
095000970526     C*
095100970526     C* -FUD-FUNZIONI
095200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
095300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
095400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
095500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
095600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
095700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
095800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
095900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
096000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
096100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
096200970526     O*------------------------------------------------------------------------*
096300160608     OFgb2      E            vuoto
096400970526** DEC - DECODIFICHE
096500970526    MODIFICA                                                      1
096600970526  ANNULLAMENTO                                                    2
096700970526VISUALIZZAZIONE                                                   3
096800021211  INSERIMENTO                                                     4
096900021211     COPIA                                                        5
097000150210    GESTIONE                                                      6
097100970528** MSG - MESSAGGI
097200150211TISIG13R- Tasto funzionale non gestito                                          1
097300150211TISIG13R- Inserimento effettuato                                                2
097400150211TISIG13R- Modifica effettuata                                                   3
097500150211TISIG13R- Annullamento effettuato                                               4
097600150211TISIG13R- Copia effettuata                                                      5
097700150211TISIG13R- Richiesta funzione per record inesistente                             6
097800150212TISIG13R- Impossibile inserire, cliente e abbinato già presenti                 7                  7
097900150211TISIG13R- Richiesta funzione per record esistente                               8
098000150226TISIG13R- Impossibile annullare il codice cliente raggruppamento bancali        9
098100150224TISIG13R- Codice cliente raggruppamento bancali non corretto                   10
098200150212TISIG13R- Codice abbinato già presente legato ad altro codice                  11
098300150211TISIG13R- Il campo è obbligatorio                                              12
098400150211TISIG13R- 13                                                                   13
098500150211TISIG13R- 14                                                                   14
098600150211TISIG13R- 15                                                                   15
098700150211TISIG13R- 16                                                                   16
098800150211TISIG13R- 17                                                                   17
09890002121118                                                                             18
09900002121119                                                                             19
09910002121120                                                                             20
099200970526** FUD - FUNZIONI
099300970526                   -        (S/N)=N                        funz.01¬
099400150211                   -        (S/N)=N                        funz.02¬
099500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
099600970526                   -        (S/N)=N                        funz.04¬
099700970526                   -        (S/N)=N                        funz.05¬
099800000407F06=Conferma       -        (S/N)=S                        funz.06¬
099900970527                   -        (S/N)=N                        funz.07¬
100000970527                   -        (S/N)=N                        funz.08¬
100100970527                   -        (S/N)=N                        funz.09¬
100200970527                   -        (S/N)=N                        funz.10¬
100300970526                   -        (S/N)=N                        funz.11¬
100400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
100500970526                   -        (S/N)=N                        funz.13¬
100600970526                   -        (S/N)=N                        funz.14¬
100700970526                   -        (S/N)=N                        funz.15¬
100800970526                   -        (S/N)=N                        funz.16¬
100900970526                   -        (S/N)=N                        funz.17¬
101000970526                   -        (S/N)=N                        funz.18¬
101100970526                   -        (S/N)=N                        funz.19¬
101200970526                   -        (S/N)=N                        funz.20¬
101300970526                   -        (S/N)=N                        funz.21¬
101400970526                   -        (S/N)=N                        funz.22¬
101500970526                   -        (S/N)=N                        funz.23¬
101600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
