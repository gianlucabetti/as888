000100970526     H*------------------------------------------------------------------------*
000200991008     H* Gestione codici unificanti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800991008     Ftibs13d   cf   e             WORKSTN INFDS(DEVDS1)
000900991008     F                                     SFILE(bs13DMS:MSGULT)
001000970527     F*------------
001100991008     Ftikun01L  UF A E           K DISK
001200991015     Ftikun02L  iF   E           K DISK    rename(tikun000:tiku2) prefix(x)
001300090806     Ftikun03l  IF   E           K DISK    RENAME(tikun000:tiku3)
001400020927     Ftivss05l  IF   E           K DISK    usropn
001500000307     Ftntbe01l  IF   E           K DISK
001600160606     Fazorg01l  IF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700160606     D MSG             S             78    DIM(31) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* FUNZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D FUV             S             19    DIM(24)
003600970526     D*------------------------------------------------------------------------*
003700970526     D* INPUT
003800970526     D*------------------------------------------------------------------------*
003900970526     D*------------
004000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004100970526     D*------------
004200970526     D FUDDS           DS
004300970526     D  FUDDE1                 1      1
004400970526     D  FUDFUN                 2      3
004500970526     D  FUDDE2                 4     19
004600970526     D  FUDFI0                20     34
004700970526     D  FUDATT                35     35
004800970526     D  FUDFI1                36     80
004900970526     D  FUDDES                 1     19
005000970526     D  FUDRIG                 1     80
005100970526     D*------------
005200970526     D* DS GESTIONE TASTI FUNZIONALI
005300970526     D*------------
005400970526     D DSKEY         E DS
005500970526     D DEVDS1          DS
005600970526     D  KEY                  369    369
005700970526     D*------------
005800970526     D* DS "XMSG" - GESTIONE MESSAGGI
005900970526     D*------------
006000970526     D DSMSG         E DS
006100020212     D*------------
006200020212     D* DS X LETTURA NUOVA TABELLA ISV
006300020212     D*------------
006400020212     D DSISV         E DS                  EXTNAME(DISV)
006500970528     D*------------------
006600970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006700970528     D*------------------
006800970528     D WLBDA8          DS                  INZ
006900970528     D  G08DAT                 1      8  0
007000970528     D  G08INV                 9     16  0
007100970528     D  G08ERR                17     17
007200970528     D  G08TGI                18     22  0
007300970526     D*------------
007400970526     D* DS DI PROCEDURA
007500970526     D*------------
007600991014     D TIBS02DS      E DS
007700991008     D tibds         E DS                  EXTNAME(tibs12ds)
007800991014     D dtle          E DS
007900970526     D*------------
008000970526     D* ARCHITETTURA
008100970526     D*------------
008200970526     D KPJBA         E DS
008300020212     D*------------
008400000807     D* TABELLE "XCLIR"- RICERCA CLIENTI
008500020212     D*------------
008600000807     D ksc             s              7  0 dim(30)
008700000807     D ksa             s              4    dim(30)
008800020212     D isvkun          s              2    dim(100)
008900000807     D*------------------
009000000807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
009100000807     D*------------------
009200000807     DDSXC           e DS                  extname(XCLIRDS)
009300000807     D*---
009400000807     D                 DS
009500000807     D dsksn                   1      4p 0
009600000807     D dsksa                   1      4
009700000807     DBS69DS         e ds                  extname(TIBS69DS)
009800000807     DACODS          e ds                  extname(cnaco00f)
009900000807     DINDDS          e ds                  extname(cnind00f)
010000000807     DCLPDS          e ds                  extname(cnclp00f)
010100000807     DCLSDS          e ds                  extname(fncls00f)
010200000307     D*------------
010300000307     D* SCOMPOSIZIONE CODICI CLIENTI IN FORMATO "STRATEGI"
010400000307     D*------------
010500000307     D                 DS
010600000307     D  FILCLI                 5      7  0
010700000307     D  STRATECLI              1     11
010800000807     D*------------------
010900000807     D* VARIABILI DI WRK
011000000807     D*-------------------
011100000807     D tipprf          s              3    inz(*blanks)
011200091116     D wDATA_ISO       S               D   DATFMT(*ISO) INZ(D'1999-01-01')
011300020905     D*------------
011400020905     D* DS REPERIMENTO DATI UTENTE
011500020905     D*------------
011600130412     d Status         sds
011700130412     d  SDSpgm           *proc
011800130412     d  SDSprm           *parms
011900130412     d  SDSuser              254    263
012000020905     D TIBS34DS      E DS
012100020905     D DDATIUTE      E DS
012200020905     D AZUTEDS       E DS                  extname(AZUTE00F)
012300060113     d dute01        e ds
012400060113     d trul31ds      e ds
012500160606     d og143         e ds
012600020905     D*------------
012700020905     D* SCHIERA P.O. IN GESTOINE ALL'UTENTE
012800020905     D*------------
012900060113     D skPOG           S              3    DIM(250) INZ(*zeros)
013000090806     D*------------------
013100090806     D* VARIABILI DI WRK
013200090806     D*------------
013300090806     D skTIPTLEWW      S              2    DIM(100) INZ(*blanks)
013400090806     D skTIPTLEST      S              2    DIM(100) INZ(*blanks)
013500090806     D jskTIPTLEWW     S              3  0 INZ(*zeros)
013600090806     D jskTIPTLEST     S              3  0 INZ(*zeros)
013700160606      *
013800160606     d ntwp            s              3
013900160606     d ntwf            s              3
014000970526     C*------------------------------------------------------------------------*
014100970526     C* MAIN LINES
014200970526     C*------------------------------------------------------------------------*
014300970526     C*
014400970526     C* NON CI SONO ERRORI IN ENTRATA
014500970526IF  1C     ERRL00        IFEQ      '0'
014600970526     C*
014700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015100970526     C*
015200970526     C* EMETTE FORMATO VIDEO
015300970526     C                   EXSR      BUTFMR
015400970526     C*
015500970526     C* GESTIONE TASTI FUNZIONALI
015600970526     C                   EXSR      FUNCON
015700970526E   2C                   ENDDO
015800970526     C*
015900970526     C* CI SONO ERRORI IN ENTRATA
016000970527X   1C                   ELSE
016100991008     C                   MOVEL     '1'           k12err
016200991008     C                   MOVEL     DSMSMS        k12msg
016300970526E   1C                   ENDIF
016400970526     C* FINE PGM
016500991210     c                   exsr      finsr
016600970526     C                   SETON                                        LR
016700970526     C*------------------------------------------------------------------------*
016800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016900970526     C*------------------------------------------------------------------------*
017000970526     C     BUTFMR        BEGSR
017100970526     C*
017200970526     C* EMETTE LA TESTATA
017300991011     C                   WRITE     bs13D01
017400970526     C*
017500970526     C* SPEDISCE EVENTUALE MESSAGGIO
017600970526     C                   EXSR      SNDMSG
017700970526     C*
017800970526     C* EMETTE LE FUNZIONI
017900991011     C                   WRITE     bs13D02
018000970526     C*
018100970526     C* EMETTE IL VIDEO
018200991011     C                   EXFMT     bs13D03
018300970526     C*
018400970526     C* REIMPOSTA FLAG DI ERRORE
018500970526     C                   MOVEL     '0'           ERRL00
018600970526     C*
018700970526     C* REIMPOSTA INDICATORI DI ERRORE
018800000307     C                   SETOFF                                         5859
018900970526     C                   SETOFF                                       606162
019000970526     C                   SETOFF                                       636465
019100970526     C                   SETOFF                                       666768
019200970526     C                   SETOFF                                       697071
019300970526     C                   SETOFF                                       727374
019400970526     C                   SETOFF                                       757677
019500970526     C                   SETOFF                                       787980
019600970526     C                   SETOFF                                       818283
019700970526     C                   SETOFF                                       848586
019800970526     C                   SETOFF                                       878889
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
020300970526     C*------------------------------------------------------------------------*
020400970526     C     SNDMSG        BEGSR
020500970526     C*
020600970526     C* PULISCE IL SFILE MESSAGGI
020700970526     C                   MOVEL     '1'           *IN40
020800991011     C                   WRITE     bs13DMC
020900970526     C                   MOVEL     '0'           *IN40
021000970526     C*
021100970526     C* VISUALIZZA IL MESSAGGIO
021200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
021300970526     C                   CALL      'XMSG'        XMSGPA
021400970526IF  1C     DSMSER        IFEQ      *ZEROS
021500970526     C                   MOVEL     DSMSKE        MSGKEY
021600970526E   1C                   ENDIF
021700970526     C                   Z-ADD     1             MSGULT
021800991011     C                   WRITE     bs13DMS
021900991011     C                   WRITE     bs13DMC
022000970526     C*
022100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
022200970526     C                   MOVEL     *BLANKS       DSMSMS
022300970526     C*
022400970526     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* ENTGES - GESTIONE ENTER -> INVIO
022700970526     C*------------------------------------------------------------------------*
022800970526     C     ENTGES        BEGSR
022900970526     C*
023000970526     C* CONTROLLA I CAMPI IMMESSI
023100021024IF  1C     DEPOP1        IFNE      'O04'
023200970526     C                   EXSR      DECODI
023300021024E   1C                   ENDIF
023400970526     C*
023500970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
023600970526IF  1C     DEPOP1        IFEQ      'O05'
023700970526     C                   MOVEL     '1'           FINL00
023800970526E   1C                   ENDIF
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* F03GES - GESTIONE F03 -> FINE
024300970526     C*------------------------------------------------------------------------*
024400970526     C     F03GES        BEGSR
024500970526     C*
024600970526     C* IMPOSTA "PREMUTO F03"
024700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
024800991008     C                   MOVEL     '1'           K12F03                         *DI PROCEDURA
024900970526     C*
025000970526     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
025300970526     C*------------------------------------------------------------------------*
025400970526     C     F06GES        BEGSR
025500970526     C*
025600970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
025700970526IF  1C     DEPOP1        IFEQ      'F06'
025800970526     C                   EXSR      INSERI
025900970526E   1C                   ENDIF
026000970526IF  1C     DEPOP1        IFEQ      'O02'
026100970526     C                   EXSR      MODIFI
026200970526E   1C                   ENDIF
026300991014IF  1C     DEPOP1        IFEQ      'O03'
026400000307     C                   EXSR      COPIA
026500991014E   1C                   ENDIF
026600000307IF  1C     DEPOP1        IFEQ      'O04'
026700000307     C                   EXSR      ANNULL
026800000307E   1C                   ENDIF
026900970526IF  1C     DEPOP1        IFEQ      'O07'
027000970526     C                   EXSR      RIPRIS
027100970526E   1C                   ENDIF
027200970526     C*
027300970526     C                   ENDSR
027400970526     C*------------------------------------------------------------------------*
027500970526     C* F12GES - GESTIONE F12 -> RITORNO
027600970526     C*------------------------------------------------------------------------*
027700970526     C     F12GES        BEGSR
027800970526     C*
027900970526     C* IMPOSTA "PREMUTO F12"
028000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
028100991008     C                   MOVEL     '1'           k12F12                         *DI PROCEDURA
028200970526     C*
028300970526     C                   ENDSR
028400970526     C*------------------------------------------------------------------------*
028500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028600970526     C*------------------------------------------------------------------------*
028700970526     C     F24GES        BEGSR
028800970526     C*
028900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
029000970526     C*
029100970526     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
029400970526     C*------------------------------------------------------------------------*
029500970526     C     INSERI        BEGSR
029600970526     C*
029700970526     C* CONTROLLA I CAMPI IMMESSI
029800970526     C                   EXSR      DECODI
029900970526     C*
030000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030200991014     C                   movel     VIDTle        kuntle
030300991008     C                   MOVEL     VIDazp        kunazp                         *CHIAVE
030400991008     C                   z-add     copVID        kuncop
030500991011     C                   MOVEL     vidazf        kunazf
030600991011     C                   EXSR      IMPkun                                       *ALTRI CAMPI
030700991008     C                   Z-ADD     *ZEROS        kunDTR                         *DATA TRASMISSIONE
030800991008     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
030900130412     C                   EVAL      KUNDIM = %dec(%date())
031000130412     C                   EVAL      KUNDUV = %dec(%date())
031100130412     C                   EVAL      KUNPUV = SDSUser
031200991008     C                   WRITE     tikun000
031300991008     C                   MOVEL     MSG(2)        k12MSG                         *MESSAGGIO
031400970602     C*
031500970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
031600991108     C                   move      *ZEROS        VIDcof
031700991108     C                   move      *blank        vddcof
031800970526E   1C                   ENDIF
031900970526     C*
032000970526     C                   ENDSR
032100970526     C*------------------------------------------------------------------------*
032200970526     C* MODIFI - OPERAZIONI DI MODIFICA
032300970526     C*------------------------------------------------------------------------*
032400970526     C     MODIFI        BEGSR
032500970526     C*
032600970526     C* CONTROLLA I CAMPI IMMESSI
032700970526     C                   EXSR      DECODI
032800970526     C*
032900970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
033000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033100991011     C                   EXSR      LETkun
033200970526     C*
033300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
033400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033500991011     C                   EXSR      IMPkun                                       *IMPOSTA I CAMPI
033600991008     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
033700130412     C                   EVAL      KUNDUV = %dec(%date())
033800130412     C                   EVAL      KUNPUV = SDSUser
033900991008     C                   UPDATE    tikun000
034000991008     C                   MOVEL     MSG(3)        k12MSG
034100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
034200970526E   2C                   ENDIF
034300970526E   1C                   ENDIF
034400970526     C*
034500970526     C                   ENDSR
034600991014     C*------------------------------------------------------------------------*
034700000307     C* COPIA  - OPERAZIONI DI COPIA
034800991014     C*------------------------------------------------------------------------*
034900000307     C     COPIA         BEGSR
035000991014     C*
035100991014     C* CONTROLLA I CAMPI IMMESSI
035200991014     C                   EXSR      DECODI
035300991014     C*
035400991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
035500991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035600991014     C                   movel     VIDTle        kuntle
035700991014     C                   MOVEL     VIDazp        kunazp                         *CHIAVE
035800991014     C                   z-add     copVID        kuncop
035900991014     C                   MOVEL     vidazf        kunazf
036000991014     C                   EXSR      IMPkun                                       *IMPOSTA I CAMPI
036100991014     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
036200991111     C                   Z-ADD     *ZEROS        kunDTR                         *DATA TRASMISSIONE
036300130412     C                   EVAL      KUNDIM = %dec(%date())
036400130412     C                   EVAL      KUNDUV = %dec(%date())
036500130412     C                   EVAL      KUNPUV = SDSUser
036600991014     C                   write     tikun000
036700991014     C                   MOVEL     MSG(3)        k12MSG
036800991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
036900991014E   2C                   ENDIF
037000991014     C*
037100991014     C                   ENDSR
037200970526     C*------------------------------------------------------------------------*
037300970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
037400970526     C*------------------------------------------------------------------------*
037500000307     C     ANNULL        BEGSR
037600000307     C*
037700000307     C* CONTROLLA I CAMPI IMMESSI
037800021024     C                   EXSR      DECODI
037900970526     C*
038000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
038100000307     C                   EXSR      LETkun
038200970527     C*
038300970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
038400000307IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038500000307IF  2C     kunvat        IFEQ      *BLANKS                                      *NO ANNULLATI
038600130416IF  3C     kunDUV        IFGT      *ZEROS                                       *GIA' TRASMESSO
038700091116     C     *ISO          MOVE      datcor        wDATA_ISO
038800091116     C     wDATA_ISO     SUBDUR    1:*D          wDATA_ISO
038900091116     C     *ISO          MOVE      wDATA_ISO     kundsc
039000000307     C                   MOVEL     'A'           kunvat                         *FLAG ANNULLAMENTO
039100000307     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
039200130412     C                   EVAL      KUNDUV = %dec(%date())
039300130412     C                   EVAL      KUNPUV = SDSUser
039400000307     C                   UPDATE    tikun000                                     *ANNULLA
039500000307X   3C                   ELSE                                                   *MAI TRASMESSO
039600000307     C                   DELETE    tikun000                                     *CANCELLA
039700000307E   3C                   ENDIF
039800000307E   2C                   ENDIF
039900000307     C                   MOVEL     MSG(4)        k12MSG                         *MESSAGGIO
040000000307     C                   MOVEL     '1'           FINL00                         *FINE PGM
040100000307E   1C                   ENDIF
040200970526     C*
040300000307     C                   ENDSR
040400970526     C*------------------------------------------------------------------------*
040500970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
040600970526     C*------------------------------------------------------------------------*
040700000807     C     RIPRIS        BEGSR
040800970526     C*
040900970526     C* CONTROLLA I CAMPI IMMESSI
041000970526     C                   EXSR      DECODI
041100970526     C*
041200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
041300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041400991011     C                   EXSR      LETkun
041500970526     C*
041600970526     C* IMPOSTA IL RECORD DA SCRIVERE
041700970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041800991008     C                   MOVEL     *BLANKS       kunvat                         *FLAG ANNULLAMENTO
041900991008     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
042000130412     C                   EVAL      KUNDUV = %dec(%date())
042100130412     C                   EVAL      KUNPUV = SDSUser
042200991008     C                   UPDATE    tikun000                                     *ANNULLA
042300970526E   2C                   ENDIF
042400991008     C                   MOVEL     MSG(5)        k12MSG                         *MESSAGGIO
042500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
042600970526E   1C                   ENDIF
042700970526     C*
042800970526     C                   ENDSR
042900000807     C*------------------------------------------------------------------------*
043000000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
043100000807     C*------------------------------------------------------------------------*
043200000807     C     CHKAUT        BEGSR
043300000807     C*
043400000807     C* - controllo su "operabilità" di TIPO LEGAME -
043500000807     C                   eval      tipprf =  %subst(%trim(KNMUS):1:3)
043600060117     C     DUTLPO        ifeq      'S'
043700000807     C* X SEDE
043800060117     C     ERRL00        ifeq      '0'
043900060117     C                   movel     'TLE'         KTBcod
044000060117     C                   movel(p)  vidtle        KTBky1
044100060117     C     KEYTBE        chain     tntbe01l                           70
044200060117     C     *in70         ifeq      *off
044300060117     C     tbeatb        andeq     *blanks
044400060117     C                   movel     tbeuni        dtle
044500060117     C                   if        §tleaus = 'S' or
044600060117     C                             (tipprf = 'EDP' and §tleaue = 'S')
044700060117     C                   else
044800060117     C                   SETON                                        66
044900060117     C                   MOVEL     '1'           ERRL00
045000060117     C                   MOVEL     MSG(23)       DSMSMS
045100060117     C                   endif
045200060117     C                   else
045300060117     C                   MOVEL     '1'           ERRL00
045400060117     C                   MOVEL     MSG(12)       DSMSMS
045500060117     C                   endif
045600060117     C                   endif
045700060117     C                   else
045800000807     C* X FILIALE
045900000807     C     ERRL00        ifeq      '0'
046000000807     C                   movel     'TLE'         KTBcod
046100000807     C                   movel(P)  vidtle        KTBky1
046200000807     C     KEYTBE        chain     tntbe01l                           70
046300000807     C     *in70         ifeq      *off
046400000807     C     tbeatb        andeq     *blanks
046500000807     C                   movel     tbeuni        dtle
046600000807     C                   if        §tleaut = 'S' or
046700000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
046800000807     C                   else
046900000810     C                   SETON                                        66
047000000807     C                   MOVEL     '1'           ERRL00
047100000807     C                   MOVEL     MSG(23)       DSMSMS
047200000807     C                   endif
047300000807     C                   else
047400000807     C                   MOVEL     '1'           ERRL00
047500000807     C                   MOVEL     MSG(12)       DSMSMS
047600000807     C                   endif
047700000807     C                   endif
047800060117     C                   endif
047900000807     C*
048000000807     C*
048100000807     C     ERRL00        ifeq      '0'
048200001018     C     tipprf        andne     'EDP'
048300160615     C     dutntw        andne     'LOG'
048400000807     C* - controllo su "operabilità" di FILIALE (1° o 2° livello)
048500000807     C                   setoff                                       11
048600000807     C* CONTROLLO CLIENTE "PADRE"
048700000807     C     vidcop        ifne      *zeros
048800000807     C                   move(p)   vidcop        STRATECLI
048900060113     c                   move      filcli        w003a             3
049000060113     c     w003a         lookup    skpog                                  99
049100000807     C     *IN99         ifeq      '0'
049200000807     C                   seton                                        11
049300000807     C                   endif
049400000807     C                   endif
049500000807     C*
049600000807     C                   setoff                                       12
049700000807     C*
049800000807     C* CONSIDERAZIONI FINALI DEL CONTROLLO
049900000807     C     *in11         ifeq      *on
050000000830     C                   seton                                        61
050100000830     C                   movel     '1'           ERRL00
050200000830     C                   movel     MSG(24)       DSMSMS
050300000830     C                   endif
050400000830     C                   endif
050500000807     C*
050600000807     C                   ENDSR
050700970526     C*------------------------------------------------------------------------*
050800991011     C* LETkun - LEGGE IL RECORD PERL'AGGIORNAMENTO
050900970526     C*------------------------------------------------------------------------*
051000991011     C     LETkun        BEGSR
051100970526     C*
051200000808     C                   MOVEL     vidtle        KCEtle
051300000808     C                   MOVEL     vidazp        KCEazp
051400000808     C                   MOVEL     vidazf        KCEazf
051500000808     C                   MOVE(p)   vidcop        KCEcop
051600000808     C                   MOVE(p)   vidcof        KCEcof
051700000808     C                   Z-ADD     ddevid        KCEdde
051800991008     C     KEYkun        CHAIN     tikun01L                           9998
051900000904     C                   IF        DEPOP1 = 'F06' OR
052000000904     C                             DEPOP1 = 'O03'
052100000808IF  1C     *IN99         IFEQ      *OFF                                         *GIA' PRESENTE
052200000808     C                   MOVEL     '1'           ERRL00                         *ERRORE
052300000808E   1C                   ENDIF
052400000808     C                   ELSE
052500970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
052600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
052700991008     C                   MOVEL     MSG(7)        k12MSG                         *MESSAGGIO
052800000808     C                   MOVEL     '1'           FINL00                         *FINE PGM
052900970526E   1C                   ENDIF
053000970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
053100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
053200991008     C                   MOVEL     MSG(6)        k12MSG                         *MESSAGGIO
053300000808     C                   MOVEL     '1'           FINL00                         *FINE PGM
053400970526E   1C                   ENDIF
053500000808     C                   ENDIF
053600970526     C*
053700970526     C                   ENDSR
053800970526     C*------------------------------------------------------------------------*
053900970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
054000970526     C*------------------------------------------------------------------------*
054100970526     C     DECODI        BEGSR
054200020211     C*
054300000904     C*--------------------
054400000904     C* DATA DECORRENZA
054500000904     C*--------------------
054600000904     C     *LIKE         DEFINE    VIDDDE        DDEVID
054700000904IF  1C     ERRL00        IFEQ      '0'
054800000904     C                   Z-ADD     VIDDDE        wda8
054900000904     C                   Z-ADD     *ZEROS        da8w
055000000904     C                   EXSR      CHKDA8
055100000904     C                   Z-ADD     WDA8          viddde
055200000904     C                   Z-ADD     DA8W          ddevid
055300000904IF  2C     ERRL00        IFEQ      '1'
055400000904     C                   SETON                                        63
055500000904     C                   MOVEL     MSG(14)       dsmsms
055600000904E   2C                   ENDIF
055700150505      *
055800150505      * non è ammesso solo x "CA"
055900150505      * inserire una data decorrenza antecedente alla UDATE
056000150505     c                   if        vidTLE ='CA' and
056100150505     c                             ddevid < %dec(%date())
056200150505     c                   if        DEPOP1 = 'F06' or
056300150505     c                             DEPOP1 = 'O03'
056400150505     C                   SETON                                        63
056500150505     C                   MOVEL     MSG(29)       dsmsms
056600150505     c                   end
056700150505     c                   end
056800150505      *
056900000904E   1C                   ENDIF
057000000904     C*--------------------
057100000904     C* DATA SCADENZA
057200000904     C*--------------------
057300000904     C     *LIKE         DEFINE    VIDDSC        DSCVID
057400000904IF  1C     ERRL00        IFEQ      '0'
057500000904     C                   Z-ADD     VIDDSC        WDA8
057600000904     C                   Z-ADD     *ZEROS        DA8W
057700000904     C                   EXSR      CHKDA8
057800000904     C                   Z-ADD     WDA8          VIDDSC
057900000904     C                   Z-ADD     DA8W          DSCVID
058000000904IF  2C     ERRL00        IFEQ      '1'
058100000904     C                   SETON                                        64
058200000904     C                   MOVEL     MSG(14)       DSMSMS
058300000904E   2C                   ENDIF
058400000904E   1C                   ENDIF
058500000308     C*
058600000308     C* IN CASO DI RICHIESTA DI ANNULLAMENTO BY-PASSO I CONTROLLI DI VALIDITA'
058700000308     C     DEPOP1        IFNE      'O04'                                        *ANNULLAMENTO
058800000811     C     DEPOP1        ANDNE     'O05'                                        *VISUALIZZAZIONE
058900021024     C     DEPOP1        ANDNE     'O07'                                        *RIPRISTINO
059000991008     C*--------------------
059100991008     C* tipo legame
059200991008     C*--------------------
059300991008IF  1C     ERRL00        IFEQ      '0'
059400000808     C                   move      *blank        wtlecu
059500000808     C                   move      *blank        wtlece
059600991008IF  2C     VIDtle        IFne      *blank
059700000322     C                   exsr      cnttle
059800991008X   2C                   ELSE
059900991008     C                   MOVEL     '1'           ERRL00
060000991011     C                   SETON                                        66
060100991011     C                   MOVEL     MSG(12)       DSMSMS
060200991008E   2C                   ENDIF
060300991008E   1C                   ENDIF
060400000904     C*--------------------
060500000904     C* codice padre
060600000323     C*--------------------
060700011206IF  1C     ERRL00        IFEQ      '0'
060800011206     C                   MOVEL     VIDcop        chkPot            4 0
060900011206IF  1C                   IF        chkPot > *zeros
061000011206     C                   SETON                                        61
061100011206     C                   MOVEL     MSG(13)       dsmsms
061200011206     C                   MOVE      '1'           errl00
061300011206     C                   MOVEL     *ZEROS        copVID
061400011206E   1C                   ENDIF
061500011206E   1C                   ENDIF
061600011206     C*
061700000323IF  1C     ERRL00        IFEQ      '0'
061800000808     C     *LIKE         DEFINE    kuncop        copVID
061900000323     C                   MOVEL     *BLANKS       VDDcop
062000000323IF  2C     VIDcop        IFEQ      *blanks                                      *STANDARD
062100000323IF  2C     VIDcop        oreq      *zeros                                       *STANDARD
062200000323     C                   SETON                                        61
062300000323     C                   MOVEL     MSG(21)       dsmsms
062400000323     C                   MOVE      '1'           errl00
062500000323     C                   MOVEL     *ZEROS        copVID
062600000323X   2C                   ELSE
062700000323     C                   move      wtlecu        clipot            1
062800000323     C                   MOVEL     VIDcop        Wcop
062900000323     C                   MOVEL     *blank        Wdcop
063000000323     C                   EXSR      CHKcop
063100000323     C                   MOVEL     Wcop          VIDcop
063200000323     C                   MOVEL     WDcop         VDDcop
063300000323     C                   MOVEL     VIDcop        copVID
063400160606      * ntw padre
063500160607     c                   clear                   ntwp
063600160607     C                   move(p)   vidcop        STRATECLI
063700160607     c     filcli        chain     azorg01l
063800160607     c                   if        %found(azorg01l)
063900160607     c                   movel     orgde3        og143
064000160607     c                   move      §ogntw        ntwp
064100160607     c                   endif
064200160607      *
064300160607IF  3C     ERRL00        IFEQ      '1'
064400160607     C                   SETON                                        61
064500160607     C                   MOVEL     MSG(13)       DSMSMS
064600160607E   3C                   ENDIF
064700160607E   2C                   ENDIF
064800160607E   1C                   ENDIF
064900000904     c*
065000160608     c* Verifico che il codice "padre" non sia un già presente per legame LT
065100160608IF  1C     ERRL00        IFEQ      '0'
065200160608     c     vidtle        andeq     'LT'
065300160623     C     DEPOP1        ANDNE     'O02'                                        *VISUALIZZAZIONE
065400160608     c                   move      vidcop        kuncop
065500160608     c     kkun03        setll     tikun03l
065600160608     c                   do        *hival
065700160608     c     kkun03        reade     tikun03l
065800160608     c                   if        %eof(tikun03l)
065900160608     c                   leave
066000160608     c                   endif
066100160608     c                   if        kunvat = *blanks
066200160608     C                   SETON                                        61
066300160608     C                   MOVEL     MSG(28)       DSMSMS
066400160608     C                   MOVE      '1'           errl00
066500160608     c                   endif
066600160608     c                   enddo
066700160608     c                   endif
066800000904     c* Verifico che il codice "padre" non sia un già un "figlio"
066900150505     c*  tranne x il legame è "CA"
067000000323IF  1C     ERRL00        IFEQ      '0'
067100150505     c     vidtle        andne     'CA'
067200000904     C                   move      copvid        kpar3
067300000904     c     keypar        setll     tikun02l
067400160606     c     keypar        reade     tikun02l                               99
067500000904     c                   dow       not %eof
067600000904     c                   if        not *in99             AND
067700000904     c                             ((ddevid > xkundsc    AND
067800000904     c                               dscvid > xkundsc)   OR
067900000904     c                              (ddevid < xkundde    AND
068000000904     c                               dscvid < xkundde))
068100000904     c                   else
068200000904     c                   if        xkunvat = *blanks
068300000904     C                   SETON                                        61
068400000904     C                   MOVEL     MSG(19)       DSMSMS
068500000904     C                   MOVE      '1'           errl00
068600000904     c                   endif
068700000904     c                   endif
068800160606     c     keypar        reade     tikun02l                               99
068900000904     c                   enddo
069000000808     C                   ENDIF
069100000323     C*--------------------
069200000323     C* codice figlio
069300000323     C*--------------------
069400011206IF  1C     ERRL00        IFEQ      '0'
069500011206     C                   MOVEL     VIDcof        chkPot            4 0
069600011206IF  1C                   IF        chkPot > *zeros
069700011206     C                   SETON                                        61
069800011206     C                   MOVEL     MSG(18)       dsmsms
069900011206     C                   MOVE      '1'           errl00
070000011206     C                   MOVEL     *ZEROS        cofVID
070100011206E   1C                   ENDIF
070200011206E   1C                   ENDIF
070300011206     C*
070400000323IF  1C     ERRL00        IFEQ      '0'
070500000808     C     *LIKE         DEFINE    kuncof        cofVID
070600000323     C                   MOVEL     *BLANKS       VDDcof
070700000323IF  2C     VIDcof        IFEQ      *blanks                                      *STANDARD
070800000323IF  2C     VIDcof        oreq      *zeros                                       *STANDARD
070900000323     C                   SETON                                        65
071000000323     C                   MOVEL     MSG(22)       dsmsms
071100000323     C                   MOVE      '1'           errl00
071200000323     C                   MOVEL     *ZEROS        cofVID
071300000323X   2C                   ELSE
071400000323     c                   movel     wtlece        clipot
071500000323     C                   MOVEL     VIDcof        Wcop
071600000323     C                   MOVEL     *blank        Wdcop
071700000323     C                   EXSR      CHKcop
071800000323     C                   MOVEL     Wcop          VIDcof
071900000323     C                   MOVEL     WDcop         VDDcof
072000000323     C                   MOVEL     VIDcof        cofVID
072100160606      * ntw figlio
072200160606     c                   clear                   ntwf
072300160606     C                   move(p)   vidcof        STRATECLI
072400160606     c     filcli        chain     azorg01l
072500160606     c                   if        %found(azorg01l)
072600160606     c                   movel     orgde3        og143
072700160606     c                   move      §ogntw        ntwf
072800160606     c                   endif
072900160606      *
073000000323IF  3C     ERRL00        IFEQ      '1'
073100000323     C                   SETON                                        65
073200000323     C                   MOVEL     MSG(18)       dsmsms
073300000323E   3C                   ENDIF
073400000323E   2C                   ENDIF
073500000323E   1C                   ENDIF
073600160606      * controllo il tipo legame sulla base del network
073700160606      * per tipo legame ST il ntw padre figlio devono essere dello stesso tipo
073800160606      * per tipo legame LT il ntw padre deve essere corriere e il figlio logistica
073900160606     C     errl00        ifeq      '0'
074000160606     c                   select
074100160606     c                   when      vidtle = 'LT'
074200160606     c                   if        ntwp = 'LOG' or ntwf <> 'LOG'
074300160606     C                   seton                                        6165
074400160606     C                   MOVEL     '1'           errl00
074500160606     C                   MOVEL     MSG(30)       dsmsms
074600160606     c                   endif
074700160606     c                   when      vidtle = 'ST'
074800160606     c                   if        ntwp = 'LOG' and ntwf <> 'LOG' or
074900160606     c                             ntwp <> 'LOG' and ntwf = 'LOG'
075000160606     C                   seton                                        6165
075100160606     C                   MOVEL     '1'           errl00
075200160606     C                   MOVEL     MSG(31)       dsmsms
075300160606     c                   endif
075400160606     c                   endsl
075500160606     c                   endif
075600160606      *
075700000323     C     errl00        ifeq      '0'
075800150505      *** chiodo x "CA" deve essere possibile inserire lo stesso codice
075900000323     C     vidcop        ifeq      vidcof
076000150505     C     vidtle        andNE     'CA'
076100000323     C                   seton                                        65
076200000323     C                   MOVEL     '1'           errl00
076300000323     C                   MOVEL     MSG(20)       dsmsms
076400000323     C                   endif
076500000323     C                   endif
076600000904     c*
076700000904     c* Verifico che il codice "figlio" non sia già un "padre"
076800150505     c*  tranne x il legame è "CA"
076900000323IF  1C     ERRL00        IFEQ      '0'
077000150505     c     vidtle        andne     'CA'
077100000323     C                   move      cofvid        kpar3
077200000808     C     keypar        setll     tikun01l
077300160606     C     keypar        reade(n)  tikun01l                               99
077400000808     C                   dow       not %eof
077500000904     c                   if        not *in99             AND
077600000904     c                             ((ddevid > kundsc    AND
077700000904     c                               dscvid > kundsc)   OR
077800000904     c                              (ddevid < kundde    AND
077900000904     c                               dscvid < kundde))
078000000904     c                   else
078100000904     c                   if        kunvat = *blanks
078200000904     C                   SETON                                        65
078300000904     C                   MOVEL     MSG(9)        DSMSMS
078400000904     C                   MOVE      '1'           errl00
078500000904     c                   endif
078600000904     c                   endif
078700160606     C     keypar        reade(n)  tikun01l                               99
078800000808     C                   enddo
078900000808     C                   ENDIF
079000000904     c*
079100000904     c* Verifico che il codice "figlio" non sia un già "figlio" di un diverso "padre"
079200000808IF  1C     ERRL00        IFEQ      '0'
079300000904     C                   move      cofvid        kpar3
079400000808     C     keypar        setll     tikun02l
079500160606     C     keypar        reade     tikun02l                               99
079600000808     C                   dow       not %eof
079700000904     c                   if        not *in99             AND
079800000904     c                             ((ddevid > xkundsc    AND
079900000904     c                               dscvid > xkundsc)   OR
080000000904     c                              (ddevid < xkundde    AND
080100000904     c                               dscvid < xkundde))
080200000904     c                   else
080300000904     c                   if        xkunvat =  *blanks AND
080400000904     c                             xkuncop <> copvid
080500000904     C                   SETON                                        65
080600010228     C                   EVAL      DSMSMS = %trim(MSG(10)) + ' ' +
080700010228     C                                      %trim(%editc(xkuncop : '4'))
080800000904     C                   MOVE      '1'           errl00
080900010228     C                   endif
081000010228     C                   endif
081100160606     C     keypar        reade     tikun02l                               99
081200000808     C                   enddo
081300000808     C                   ENDIF
081400000808     C*--------------------
081500000323     C* inserimento codice azienda (attualmente non abilitato)
081600000808     C*--------------------
081700000323IF  1C     ERRL00        IFEQ      '0'
081800000323IF  2C     VIDazf        IFne      *blank
081900000323IF  3C     VIDazp        orne      *blank
082000000323     C                   SETON                                        6260
082100000323     C                   MOVEL     MSG(19)       dsmsms
082200000323     C                   MOVEL     '1'           errl00
082300000323E   2C                   ENDIF
082400000323E   1C                   ENDIF
082500000322     C* SCADENZA > DECORRENZA
082600000322IF  1C     ERRL00        IFEQ      '0'
082700000322IF  2C     DSCVID        IFLT      DDEVID
082800000322     C                   SETON                                        64
082900000322     C                   MOVEL     MSG(15)       DSMSMS
083000000322     C                   MOVEL     '1'           ERRL00
083100000322E   2C                   ENDIF
083200000322E   1C                   ENDIF
083300000901     C*--------------------
083400000912     C* SCADENZA >= DATA CORRENTE  ===> "Asterisco questo controllo"
083500000901     C*--------------------
083600000912IF  1C*    ERRL00        IFEQ      '0'
083700000912IF  2C*    DEPOP1        IFEQ      'O02'
083800000912     C*    DEPOP1        OREQ      'O07'
083900000912     C*    DEPOP1        OREQ      'F06'
084000000912     C*    DEPOP1        OREQ      'O03'
084100000912IF  3C*    DSCVID        IFLT      DATCOR
084200000912     C*                  SETON                                        64
084300000912     C*                  MOVEL     MSG(16)       DSMSMS
084400000912     C*                  MOVEL     '1'           ERRL00
084500000912E   3C*                  ENDIF
084600000912E   2C*                  ENDIF
084700000912E   1C*                  ENDIF
084800000904     C*--------------------
084900000904     c* scadenza <=> decorrenza  (accavalamento periodo)
085000000904     c*--------------------
085100000904IF  1c                   if        errl00 = '0'
085200000904     c                   movel     vidtle        kcetle
085300000904     c                   z-add     copvid        kcecop
085400000904     c                   z-add     cofvid        kcecof
085500000904     c                   movel     vidazp        kceazp
085600000904     c                   movel     vidazf        kceazf
085700000904     c     KE2kun        setll     tikun01L
085800000904     c     KE2kun        reade(n)  tikun01L                               99
085900000904DO  2c                   dow       *in99 = *OFF
086000000904     c                   if        (ddevid > kundsc  AND
086100000904     c                              dscvid > kundsc) OR
086200000904     c                             (ddevid < kundde  AND
086300000904     c                              dscvid < kundde)
086400000904     c                   else
086500000904     c                   if        ddevid <> kundde AND
086600000904     c                             kunvat  =  *blanks
086700000904     C                   seton                                        606162
086800000904     C                   seton                                        6364
086900000904     C                   movel     MSG(17)       DSMSMS
087000000904     C                   movel     '1'           ERRL00
087100000904     c                   leave
087200000904     c                   endif
087300000904     c                   endif
087400000904     c     KE2kun        reade(n)  tikun01L                               99
087500000904E   2c                   enddo
087600000904E   1c                   endif
087700000904     c*
087800000307     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (X CLIENTE "PADRE"/"FIGLIO")
087900000307     C                   EXSR      CHKAUT
088000000808     C*--------------------
088100000811     C* inserimento
088200000808     C*--------------------
088300000808     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
088400000904     C     DEPOP1        OREQ      'O03'                                        *COPIA
088500000808IF  1C     ERRL00        IFEQ      '0'
088600000808     C                   EXSR      LETkun
088700000808IF  1C     ERRL00        IFNE      '0'
088800000808     C                   MOVEL     MSG(25)       DSMSMS                         *MESSAGGIO DIRETTO
088900000808E   1C                   ENDIF
089000000808E   1C                   ENDIF
089100000808     C                   ENDIF
089200000808     C*--------------------
089300000808     C* annullamento sospensione
089400000808     C*--------------------
089500000808IF  1C     ERRL00        IFEQ      '0'
089600000808IF  2C     VIDvat        IFEQ      'S'
089700000808     C     VIDvat        OREQ      'A'
089800000808     C     VIDvat        OREQ      ' '
089900000808X   2C                   ELSE
090000000808     C                   MOVEL     '1'           ERRL00
090100000808     C                   SETON                                        67
090200000808     C                   MOVEL     MSG(11)       DSMSMS
090300000808E   2C                   ENDIF
090400000808E   1C                   ENDIF
090500000808     C*
090600020927     C*
090700020927     C* Il controllo seguente lo effettuo solo se trattasi di utente di sede in quanto in filiale
090800020927     C* il file TIVSS nn c'è
090900091117     C*                  if        simfel = *zeros
091000000308     C*
091100020927     C* VERIFICA CHE IL CLIENTE FIGLIO NON SIA GIA' UN CLIENTE ABILITATO (X SERVIZI CHE RICHIEDONO
091200020212     C* IL CONTROLLO)
091300091117     C*    ERRL00        ifeq      '0'
091400091117     C*                  if        vidtle = 'WW'
091500091117     C*                  move      vidcof        kssksu
091600091117     C*    keyvss        setll     tivss05l
091700091117     C*    keyvss        reade     tivss05l
091800091117     C*                  dow       not %eof(tivss05l)
091900091117     C*                  if        vssvat = *blanks                              *non annull/sosp.
092000091117     C*                  if        vssdsc < ddevid or                            *non valido
092100091117     C*                            vssdde > dscvid
092200091117     C*                  else                                                    *valido
092300091117     C* Verifico che il servizio del record corrente del TIVSS sia soggetto a controllo unificazioni
092400091117     C*    vssisv        lookup    isvkun                                 22
092500091117     C*                  if        *in22 = *on
092600091117     C*                  seton                                        65
092700091117     C*                  movel     MSG(26)       dsmsms
092800091117     C*                  movel     '1'           errl00
092900091117     C*                  leave
093000091117     C*                  endif
093100091117     C*                  endif
093200091117     C*                  endif
093300091117     C*    keyvss        reade     tivss05l
093400091117     C*                  enddo
093500091117     C*                  endif
093600091117     C*                  endif
093700091117     C*                  endif
093800090806     C*
093900090806     C* Verifico che codice padre nn già padre rispetto ad altro tipo legame dello stesso gruppo
094000090806     C     ERRL00        ifeq      '0'
094100090806     C                   exsr      CHKTIPTLE
094200090806     C                   endif
094300021024     C*
094400021024     C                   ENDIF
094500020211     C*
094600970528     C                   ENDSR
094700991008     C*------------------------------------------------------------------------*
094800991008     C* Cnttle - CONTROLLA codice tipo legame
094900991008     C*------------------------------------------------------------------------*
095000991008     c     cnttle        begsr
095100991008     c                   clear                   TIBS02DS
095200991008      *
095300991008     C     '?'           SCAN      Vidtle                                 30
095400991008      *  Ricerca
095500991008     c                   eval      t02tla = 'L'
095600991008    1C                   IF        *IN30 = *ON
095700991008     c                   movel     'R'           t02mod
095800991008     c                   else
095900991008     c                   movel     'C'           t02mod
096000991008     c                   movel(p)  vidtle        t02ke1
096100991008     c                   end
096200991008     C                   movel     'TLE'         t02cod
096300991008     c                   movel     knsif         t02sif
096400991008     C                   CALL      'TIBS02R'
096500991008     c                   parm                    KPJBA
096600991008     c                   parm                    TIBS02DS
096700991008     c
096800991008      *
096900991008    2C                   IF        T02err = *blanks
097000991008     C                   movel     T02ke1        Vidtle
097100991014     C                   movel     T02uni        dtle
097200991014     C                   movel     §tledesc      Vddtle
097300991014     C                   movel     §tlecu        Wtlecu            1
097400991014     C                   movel     §tlece        wtlece            1
097500991014     c                   else
097600991014     c                   movel     '1'           errl00
097700991014     c                   seton                                        66
097800991014     C                   MOVEL     MSG(12)       DSMSMS
097900991008     C                   ENDIF
098000991008     c
098100991008     c                   endsr
098200970528     C*------------------------------------------------------------------------*
098300000901     C* CHKcop - CONTROLLA codice padre figlio
098400970528     C*------------------------------------------------------------------------*
098500991008     C     CHKcop        BEGSR
098600970528     C*
098700991008     C     *LIKE         DEFINE    VIDcop        Wcop
098800991008     C     *LIKE         DEFINE    VDDcop        WDcop
098900991014     c                   move      *zeros        wksc
099000970528     C*
099100970528     C* RICHIESTO ELENCO
099200970528     C                   SETOFF                                           28
099300991008     C     '?'           SCAN      Wcop                                   28
099400970528IF  1C     *IN28         IFEQ      *ON
099500991008     C* RICHIESTO ELENCO CODICI
099600091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
099700091126      * deciso di non utilizzarli per le unificazioni dei codici
099800091126      * quindi non controllo il tipo codice e do per scontato che è sempre 'C'=cliente
099900991008     C                   CLEAR                   DSXC
100000991008     C                   Z-ADD     KCCCL         DXCCAP
100100991008     C                   CALL      'XCLIR'
100200991008     C                   PARM                    DSXC
100300991008     C* SCOMPATTA
100400991008     C                   MOVEA     DXCKSC        KSA                        ALF
100500991008DO  2C     1             DO        30            J                 3 0
100600991008     C                   MOVE      KSA(J)        DSKSA                      F
100700991008     C                   Z-ADD     DSKSN         KSC(J)                     M
100800991008E   2C                   ENDDO
100900991011     C                   MOVEL     KSC(1)        WKSC              7
101000991011     C                   MOVEL     '1'           $TESVD            1        TA
101100991008E   1C                   ENDIF
101200991008     C*
101300991008     C* CODICE CLIENTE NUMERICO
101400991008IF  1C     WKSC          IFLT      *ZEROS
101500991011     C                   MOVEL     '1'           WERR              1
101600991008X   1C                   ELSE
101700970528     C*
101800970528     C* SOLO CARATTERI NUMERICI
101900991011     c   28              move      *all'0'       wcop
102000991011     c   28              move      wksc          wcop
102100970528     C                   SETOFF                                       28
102200991008     C                   TESTN                   Wcop                 28
102300991015IF  2C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
102400970528     C                   MOVEL     '1'           ERRL00
102500991015X   2C                   ELSE                                                   *E'NUMERO DA TESTN
102600991008     C                   MOVE      Wcop          W001              1
102700991015IF  3C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
102800970528     C                   MOVEL     '1'           ERRL00
102900991015X   3C                   ELSE                                                   *NUMERO
103000970528     C*
103100970528     C* CONTROLLA ESISTENZA CODICE
103200991210     C                   MOVE      Wcop          wcli              7 0
103300991210     c                   exsr      dectib
103400991210     c                   movel     wfvrag        wdcop
103500991015E   3C                   ENDIF
103600991015E   2C                   ENDIF
103700991011E   1C                   ENDIF
103800970528     C*
103900970528     C                   ENDSR
104000991210     C*------------------------------------------------------------------------*
104100991210     C* Dectib - DECODIFICA CLIENTE
104200991210     C*------------------------------------------------------------------------*
104300991210     C     Dectib        BEGSR
104400991210     C*
104500991210     C                   CLEAR                   BS69DS
104600991210     C                   CLEAR                   ACODS
104700991210     C                   MOVEL     KNSIF         I69SIF
104800991210     C                   Z-ADD     WCLI          I69KAC
104900991210     C                   CALL      'TIBS69R'
105000991210     C                   PARM                    BS69DS
105100991210     C                   PARM                    ACODS
105200991210     C                   PARM                    INDDS
105300991210     C                   PARM                    CLPDS
105400991210     C                   PARM                    CLSDS
105500991210     C     O69ERR        IFNE      '1'
105600991210     C                   MOVEL     ACORAG        WFVRAG           40
105700991210     C                   ELSE
105800991210     C                   MOVEL     *ALL'*'       WFVRAG
105900991210     c                   movel     '1'           errl00
106000991210     C                   ENDIF
106100991210     C*
106200991210     C                   ENDSR
106300991210     C*------------------------------------------------------------------------*
106400991210     C* FINSR - OPERAZIONI FINALI
106500991210     C*------------------------------------------------------------------------*
106600991210     C     FINSR         BEGSR
106700991210     C*
106800991210     C* CHIUDE I FILE DEI PGM CHIAMATI
106900991210     C                   CLEAR                   bs69ds
107000991210     C                   CLEAR                   acods
107100991210     C                   CLEAR                   indds
107200991210     C                   CLEAR                   clpds
107300991210     C                   CLEAR                   clsds
107400991210     C                   MOVEL     'C'           i69tla
107500991210     C                   MOVEL     knsif         i69sif
107600991210     C                   CALL      'TIBS69R'
107700991210     C                   PARM                    bs69ds
107800991210     C                   PARM                    acods
107900991210     C                   PARM                    indds
108000991210     C                   PARM                    clpds
108100991210     C                   PARM                    clsds
108200991210     C*
108300991210     C                   ENDSR
108400970528     C*------------------------------------------------------------------------*
108500970528     C* CHKDA8 - CONTROLLA LA DATA (8)
108600970528     C*------------------------------------------------------------------------*
108700970528     C     CHKDA8        BEGSR
108800970528     C*
108900970528     C     *LIKE         DEFINE    VIDDDE        WDA8
109000970528     C     *LIKE         DEFINE    VIDDDE        DA8W
109100970528     C*
109200970528     C                   Z-ADD     WDA8          G08DAT
109300970528     C                   Z-ADD     *ZEROS        G08INV
109400970528     C                   MOVEL     '0'           G08ERR
109500970528     C                   CALL      'XSRDA8'
109600970528     C                   PARM                    WLBDA8
109700970528     C                   Z-ADD     G08DAT        WDA8
109800970528     C                   Z-ADD     G08INV        DA8W
109900970528IF  1C     G08ERR        IFEQ      '1'
110000970528     C                   MOVEL     '1'           ERRL00
110100970528E   1C                   ENDIF
110200970528     C*
110300970528     C                   ENDSR
110400970526     C*------------------------------------------------------------------------*
110500970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
110600970526     C*------------------------------------------------------------------------*
110700970526     C     IREVID        BEGSR
110800970526     C* CAMPI
110900991011     C                   MOVEL     kuntle        VIDtle
111000991011     C                   MOVEL     kunvat        VIDvat
111100151125     C                   EVAL      VIDSTSRCD = *blank
111200151125     C                   SELECT
111300151125     C                   WHEN      VIDVAT = *blank
111400151125     C                   EVAL      VIDSTSRCD = *blank
111500151125     C                   WHEN      VIDVAT = 'A'
111600151125     C                   EVAL      VIDSTSRCD = 'ANNULLATO'
111700151125     C                   WHEN      VIDVAT = 'S'
111800151125     C                   EVAL      VIDSTSRCD = 'SOSPESO'
111900151125     C                   OTHER
112000151125     C                   EVAL      VIDSTSRCD = *blank
112100151125     C                   ENDSL
112200991011     C                   MOVEL     kunazp        VIDazp
112300991011     C                   MOVEL     kunazf        VIDazf
112400991011     C                   move      kuncop        vidcop
112500991011     C                   move      kuncof        vidcof
112600991011     C                   Z-ADD     kunDDE        G08INV
112700970528     C                   Z-ADD     *ZEROS        G08DAT
112800970528     C                   MOVEL     '3'           G08ERR
112900970528     C                   CALL      'XSRDA8'
113000970528     C                   PARM                    WLBDA8
113100970528     C                   Z-ADD     G08DAT        VIDDDE
113200991011     C                   Z-ADD     kunDSC        G08INV
113300970528     C                   Z-ADD     *ZEROS        G08DAT
113400970528     C                   MOVEL     '3'           G08ERR
113500970528     C                   CALL      'XSRDA8'
113600970528     C                   PARM                    WLBDA8
113700970528     C                   Z-ADD     G08DAT        VIDDSC
113800130412     C                   Z-ADD     kunDIM        G08INV
113900130412     C                   Z-ADD     *ZEROS        G08DAT
114000130412     C                   MOVEL     '3'           G08ERR
114100130412     C                   CALL      'XSRDA8'
114200130412     C                   PARM                    WLBDA8
114300130412     C                   Z-ADD     G08DAT        VIDDIM
114400130412     C                   Z-ADD     kunDUV        G08INV
114500130412     C                   Z-ADD     *ZEROS        G08DAT
114600130412     C                   MOVEL     '3'           G08ERR
114700130412     C                   CALL      'XSRDA8'
114800130412     C                   PARM                    WLBDA8
114900130412     C                   Z-ADD     G08DAT        VIDDUV
115000130412     C                   EVAL      VIDPUV = KUNPUV
115100060726     C*
115200060726     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
115300060726     C                   IF        DEPOP1 = 'F06' OR
115400150506     C                             DEPOP1 = 'O03'
115500060726     C                   EVAL      VIDDDE = DATCORINV
115600060726     C                   EVAL      VIDDSC = 31122039
115700060726     C                   ENDIF
115800970527     C*
115900970526     C                   ENDSR
116000970526     C*------------------------------------------------------------------------*
116100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
116200970526     C*------------------------------------------------------------------------*
116300970526     C     AZZVID        BEGSR
116400970526     C* CAMPI
116500151125     C                   EVAL      VIDSTSRCD = *blank
116600991008     C                   MOVEL     *BLANKS       VIDtle
116700991008     C                   MOVEL     *BLANKS       VIDazf
116800991008     C                   MOVEL     *BLANKS       VIDazp
116900991008     C                   MOVEL     *BLANKS       VIDvat
117000060726     C                   EVAL      VIDDDE = DATCORINV
117100060726     C                   EVAL      VIDDSC = 31122039
117200991011     C                   move      *ZEROS        VIDcop
117300991011     C                   move      *ZEROS        VIDcof
117400970526     C* DECODIFICHE
117500991011     C                   MOVEL     *BLANKS       VDDtle
117600991008     C                   MOVEL     *BLANKS       VDDcop
117700991008     C                   MOVEL     *BLANKS       VDDcof
117800970526     C*
117900970526     C                   ENDSR
118000970526     C*------------------------------------------------------------------------*
118100991011     C* IMPkun - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
118200970526     C*------------------------------------------------------------------------*
118300991011     C     IMPkun        BEGSR
118400970526     C*
118500000913     C                   MOVEL     vidvat        kunvat                         *CHIAVE
118600991014     C                   z-add     cofVID        kuncof
118700991014     C                   Z-ADD     DDEVID        kunDDE
118800991008     C                   Z-ADD     DSCVID        kunDSC
118900970526     C*
119000970526     C                   ENDSR
119100970526     C*------------------------------------------------------------------------*
119200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
119300970526     C*------------------------------------------------------------------------*
119400970526     C     IMPFUN        BEGSR
119500970526     C*
119600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
119700970526IF  1C     DEPOP1        IFEQ      'O05'
119800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
119900970526     C                   MOVEL     'N'           FUDATT
120000970526     C                   MOVEL     FUDRIG        FUD(6)
120100970526E   1C                   ENDIF
120200970526     C*
120300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
120400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
120500970526     C                   EXSR      FUNGES
120600970526     C*
120700970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
120800970526     C*---
120900970526     C* 90 -> PROTEZIONE CHIAVE
121000970526     C* 91 -> PROTEZIONE TOTALE
121100970526     C*---
121200970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
121300970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
121400000904     C     DEPOP1        OREQ      'O03'                                        *COPIA
121500970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
121600970526     C                   SETOFF                                       91
121700970526E   2C                   ENDIF
121800970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
121900000904IF  2C     DEPOP1        OREQ      'O01'                                        *SCELTA
122000970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
122100970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
122200970526E   2C                   ENDIF
122300970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
122400970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
122500970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
122600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
122700970526     C                   SETON                                        91
122800970526E   2C                   ENDIF
122900970526X   1C                   ELSE                                                   *I N T E R R O G A
123000970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
123100970526     C                   SETON                                        91
123200970526E   1C                   ENDIF
123300970526     C*
123400970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
123500970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
123600991011     C                   MOVEL     k12tle        KCEtle
123700991011     C                   Z-ADD     k12cop        KCEcop
123800991011     C                   Z-ADD     k12cof        KCEcof
123900991011     C                   Z-ADD     k12dde        KCEDDE
124000991011     C     KEYkun        CHAIN     tikun01L                           9998
124100970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
124200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
124300970526     C                   MOVEL     MSG(6)        DSMSMS
124400970526X   2C                   ELSE
124500970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
124600980128IF  3C     DEPOP1        IFEQ      'O05'
124700991014IF  3C     DEPOP1        oreq      'O02'
124800980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
124900980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
125000980128E   3C                   ENDIF
125100970526E   2C                   ENDIF
125200970526X   1C                   ELSE                                                   *INSERIMENTO
125300970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
125400970526E   1C                   ENDIF
125500970526     C*
125600970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
125700970526IF  1C     DEPOP1        IFEQ      'O02'
125800970526     C                   MOVEL     DEC(1)        VIDFUN
125900970526E   1C                   ENDIF
126000991014IF  1C     DEPOP1        IFEQ      'O03'
126100991014     C                   MOVEL     DEC(6)        VIDFUN
126200991014E   1C                   ENDIF
126300970526IF  1C     DEPOP1        IFEQ      'O04'
126400970526     C                   MOVEL     DEC(2)        VIDFUN
126500970526E   1C                   ENDIF
126600970526IF  1C     DEPOP1        IFEQ      'O05'
126700970526     C                   MOVEL     DEC(3)        VIDFUN
126800970526E   1C                   ENDIF
126900970526IF  1C     DEPOP1        IFEQ      'O07'
127000970526     C                   MOVEL     DEC(4)        VIDFUN
127100970526E   1C                   ENDIF
127200970526IF  1C     DEPOP1        IFEQ      'F06'
127300970526     C                   MOVEL     DEC(5)        VIDFUN
127400970526E   1C                   ENDIF
127500970526     C*
127600970526     C                   ENDSR
127700020212     C*--------------------------------------------------------------------------------------------*
127800020212     C* CARTAB - CARICA LE TABELLE OCCORRENTI
127900020212     C*--------------------------------------------------------------------------------------------*
128000020212     C     CARTAB        BEGSR
128100020905     C*
128200020905     C* REPERISCE I DATI UTENTE
128300020905     C                   EXSR      REPDATIUTE
128400020905     C*
128500020212     C*----------
128600020212     C* Tipi servizi soggetti a controllo unificazioni
128700020212     C*----------
128800020212     C                   z-add     *zeros        x                 3 0
128900020212     C                   eval      tbecod = 'ISV'
129000020212     C     ke2tbe        setll     tntbe01l
129100020212     C     ke2tbe        reade     tntbe01l
129200020212     C                   dow       not %eof(tntbe01l)
129300020212     C                   movel     tbeuni        dsisv
129400020212     C                   if        §isvuni  = 'S'
129500020212     C                   add       1             x
129600020212     C                   eval      isvkun(x) = tbeke1
129700020212     C                   endif
129800020212     C     ke2tbe        reade     tntbe01l
129900020212     C                   enddo
130000090806     C*----------
130100090806     C* Tipi unificazioni
130200090806     C*----------
130300090806     C                   eval      tbecod = 'TLE'
130400090806     C     tbecod        setll     tntbe01l
130500160606     C     tbecod        reade     tntbe01l
130600090806     C                   dow       not %eof(tntbe01l)
130700090806     C                   movel     tbeuni        dtle
130800090806     C                   select
130900090806     C                   when      §tlegrp  = 'W'
131000090806     C                   add       1             jskTIPTLEWW
131100090806     C                   eval      skTIPTLEWW(jskTIPTLEWW) = tbeke1
131200090806     C                   when      §tlegrp  = 'S'
131300090806     C                   add       1             jskTIPTLEST
131400090806     C                   eval      skTIPTLEST(jskTIPTLEST) = tbeke1
131500090806     C                   endsl
131600160606     C     tbecod        reade     tntbe01l
131700090806     C                   enddo
131800020212     C*
131900020212     C                   ENDSR
132000020905     C*--------------------------------------------------------------------------------------------*
132100020905     C* REPERISCE I DATI UTENTE
132200020905     C*--------------------------------------------------------------------------------------------*
132300020905     C     REPDATIUTE    BEGSR
132400020905     C*
132500020905     C* INIZIALIZZA VARIABILI DI WRK
132600020905     C                   CLEAR                   TIBS34DS
132700020905     C                   CLEAR                   AZUTEDS
132800020905     C                   CLEAR                   DDATIUTE
132900020905     C*
133000020905     C     *DTAARA       DEFINE    §azute        azuteds
133100020905     C     *DTAARA       DEFINE    §datiute      ddatiute
133200020905     C                   IN(E)     *DTAARA
133300020905if  1C                   IF        %Error
133400020905     c                   EVAL      I34Tla = 'L'
133500020905     C                   CALL      'TIBS34R'
133600020905     C                   PARM                    Tibs34Ds
133700020905     C                   IN        *DTAARA
133800020905e   1C                   ENDIF
133900020905     C*
134000020905     C                   MOVEL     RSUT          DSFIRS
134100060113     C*
134200060113     C* Carica le abilitazioni particolari del profilo
134300060113     C                   Movel     *blanks       wabi              2
134400060113     C                   Movel     utefaf        dute01
134500060113     C                   If        §utecli <> *Blanks
134600060113     C                   Eval      wabi = §utecli
134700060113     C                   Else
134800060113     C                   Eval      wabi = uteaut
134900060113     C                   EndIf
135000060113     C*
135100060113     C* Reperimento dei P.O. gestibili dall'utente per i codici clienti
135200060113     C                   Clear                   Trul31ds
135300060113     C                   Eval      i31abi = wabi
135400060113     C                   Eval      i31cdi = dutdis
135500060113     C                   Eval      i31car = dutare
135600060113     C                   Eval      i31cpo = dutpou
135700060113     C                   Call      'TRUL31R'
135800060113     C                   Parm                    kpjba
135900060113     C                   Parm                    Trul31ds
136000060113     C                   If        o31pog > *zeros
136100060113     C                   Movea     o31pog        skPOG
136200060113     C                   Else
136300060113     C                   Movel     '1'           ERRl00
136400060113     C                   Movel     MSG(27)       DSMSMS
136500060113     C                   EndIf
136600020905     C*
136700020905     C                   ENDSR
136800090806     C*------------------------------------------------------------------------*
136900090806     C* CHKTIPTLE - VERIFICHE X TIPI LEGAMI
137000090806     C*------------------------------------------------------------------------*
137100090806     C     CHKTIPTLE     BEGSR
137200090806     C*
137300090806     C* VERIFICA CHE IL NUOVO PADRE NN SIA GIA' PRESENTE COME PADRE X UNO QUALUNQUE DEI
137400090806     C* LEGAMI DI TIPO 'W'
137500090806     C                   MOVE(p)   vidcop        KCEcop
137600090806     C     KCEcop        SETLL     tikun03l
137700090806     C                   IF        %found(tikun03l)
137800090806     C     KCEcop        READE     tikun03l
137900090806     C                   DOW       not %eof(tikun03l)
138000090806     C                   IF        (ddevid > kundsc    AND
138100090806     C                              dscvid > kundsc)   OR
138200090806     C                             (ddevid < kundde    AND
138300090806     C                              dscvid < kundde)
138400090806     C                   ELSE
138500091020     C* Ovviamente nn considero il record correntemente a video
138600091020     C                   IF        vidtle <> kuntle
138700090806     C*
138800090806     C                   IF        §TLEGRP = 'W'
138900090806     C     KUNTLE        LOOKUP    skTIPTLEWW                             30
139000090806     C                   IF        *IN30
139100090806     C                   SETON                                        61
139200090806     C                   MOVEL     '1'           ERRL00
139300090806     C                   EVAL      DSMSMS = MSG(28)
139400090806     C                   LEAVE
139500090806     C                   ENDIF
139600090806     C                   ENDIF
139700090806     C*
139800090806     C                   IF        §TLEGRP = 'S'
139900090806     C     KUNTLE        LOOKUP    skTIPTLEST                             30
140000090806     C                   IF        *IN30
140100090806     C                   SETON                                        61
140200090806     C                   MOVEL     '1'           ERRL00
140300090806     C                   EVAL      DSMSMS = MSG(28)
140400090806     C                   LEAVE
140500090806     C                   ENDIF
140600090806     C                   ENDIF
140700091020     C*
140800091020     C                   ENDIF
140900091020     C*
141000090806     C                   ENDIF
141100090806     C*
141200090806     C     KCEcop        READE     tikun03l
141300090806     C                   ENDDO
141400090806     C                   ENDIF
141500090806     C*
141600090806     C                   ENDSR
141700970526     C*------------------------------------------------------------------------*
141800970526     C* *INZSR - OPERAZIONI INIZIALI
141900970526     C*------------------------------------------------------------------------*
142000970526     C     *INZSR        BEGSR
142100970526     C*---
142200970526     C* RICEVIMENTO PARAMETRI
142300970526     C*---
142400970526     C     *ENTRY        PLIST
142500970612     C                   PARM                    KPJBA
142600991028     C                   movel     kpjbu         tibds
142700970526     C*
142800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
142900970526     C     XMSGPA        PLIST
143000970526     C                   PARM                    DSMSMS
143100970526     C                   PARM                    DSMSPG
143200970526     C                   PARM                    DSMSKE
143300970526     C                   PARM                    DSMSER
143400970526     C*
143500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
143600970526     C                   CALL      'XKEY'
143700970526     C                   PARM                    DSKEY
143800020212     C*
143900020212     C* CARICA LE TABELLE OCCORRENTI
144000020212     C                   EXSR      CARTAB
144100020927     C*
144200020927     C* IN BASE ALL'UTENTE EFFETTUO ALCUNE CONSIDERAZIONI
144300021007     C                   IF        simfel = *zeros
144400020927     C                   OPEN      tivss05l
144500020927     C                   ENDIF
144600970526     C*---
144700970526     C* VARIABILI NON RIFERITE AL DATA BASE
144800970526     C*---
144900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
145000991011     C     *LIKE         DEFINE    k12OP0        DEPOP0
145100991011     C     *LIKE         DEFINE    k12OP1        DEPOP1
145200991011     C     *LIKE         DEFINE    k12OP0        WOP0
145300991011     C     *LIKE         DEFINE    k12OP1        WOP1
145400970526     C*---
145500970526     C* VARIABILI DI CONTROLLO
145600970526     C*---
145700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
145800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
145900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
146000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
146100000307     C                   SETON                                        50
146200970526     C*
146300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
146400991008     C                   MOVEL     'TIBS13R'     DSMSPG
146500991008     C                   MOVEL     'TIBS13R'     MSGQUE
146600970526     C                   MOVEL     *BLANKS       MSGKEY
146700970526     C                   MOVEL     *BLANKS       DSMSMS
146800970526     C*---
146900970526     C* VARIABILI RIFERITE AL DATA BASE
147000970526     C*---
147100020830     C     *LIKE         DEFINE    acokcc        kcccl
147200991011     C     *LIKE         DEFINE    kuntle        KCEtle
147300991011     C     *LIKE         DEFINE    kuncop        KCEcop
147400991015     C     *LIKE         DEFINE    kuncop        Kpar3
147500991011     C     *LIKE         DEFINE    kuncof        KCEcof
147600991011     C     *LIKE         DEFINE    kunDDE        KCEDDE
147700991011     C     *LIKE         DEFINE    kunazf        KCEazf
147800991011     C     *LIKE         DEFINE    kunazp        KCEazp
147900000307     C     *LIKE         DEFINE    tbecod        KTBcod
148000000307     C     *LIKE         DEFINE    tbeke1        KTBky1
148100020211     C     *LIKE         DEFINE    vssksu        KSSksu
148200020905     C     *LIKE         DEFINE    kpjbu         Depkpjbu
148300020830     C*
148400020830     C* INIZIALIZZAZIONE VARIABILI DI WRK
148500020830     C                   z-add     1             kclkut
148600020916     C                   z-add     DUTKCI        kcccl
148700020830     C                   movel     *blank        kceazp
148800020830     C                   movel     *blank        kceazf
148900970526     C*---
149000970526     C* CHIAVI DI LETTURA
149100970526     C*---
149200991011     C* LETTURA tikun01L - COMPLETA
149300991011     C     KEYkun        KLIST
149400090806     C                   KFLD                    KCEtle
149500090806     C                   KFLD                    KCEazp
149600090806     C                   KFLD                    KCEcop
149700090806     C                   KFLD                    KCEazf
149800090806     C                   KFLD                    KCEcof
149900090806     C                   KFLD                    KCEdde
150000090806     C     KEYpar        KLIST
150100090806     C                   KFLD                    vidtle
150200090806     C                   KFLD                    KCEazp
150300090806     C                   KFLD                    Kpar3
150400160608     C     Kkun03        KLIST
150500160608     C                   KFLD                    kcecop
150600160608     C                   KFLD                    vidtle
150700090806     C* LETTURA tikun01L - COMPLETA
150800090806     C     KE2kun        KLIST
150900090806     C                   KFLD                    KCEtle
151000090806     C                   KFLD                    KCEazp
151100090806     C                   KFLD                    KCEcop
151200090806     C                   KFLD                    KCEazf
151300090806     C                   KFLD                    KCEcof
151400991210     C* LETTURA cnaco31c
151500991008     C     keycli        KLIST                                                  *di cnaco/ind/clp
151600991008     C                   KFLD                    kclkut            1 0          -utente
151700991008     C                   KFLD                    kcccl                          -capoconto
151800991008     C                   KFLD                    kcksc             7 0          -codice
151900000307     C* LETTURA tntbe01l -
152000000307     C     KEYtbe        KLIST
152100000307     C                   KFLD                    Ktbcod                         *CODICE TABELLA
152200000307     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
152300020211     C* LETTURA tivss02L - PARZIALE
152400020211     C     KEYvss        KLIST
152500020211     C                   KFLD                    KSSksu                         *CODICE CLI ABIL.
152600020212     C* LETTURA tntbe01l - PARZIALE
152700020212     C     KE2tbe        KLIST
152800020212     C                   KFLD                    tbecod                         *CODICE TABELLA
152900970528     C*
153000970528     C* CALCOLA LA DATA CORRENTE
153100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
153200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
153300970528     C                   Z-ADD     WN8           G08DAT
153400060726     C                   Z-ADD     WN8           DATCORINV         8 0
153500970528     C                   Z-ADD     *ZEROS        G08INV
153600970528     C                   MOVEL     '0'           G08ERR
153700970528     C                   CALL      'XSRDA8'
153800970528     C                   PARM                    WLBDA8
153900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
154000970526     C*
154100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
154200991011     C                   MOVEL     k12OP0        DEPOP0                         *LIVELLO PROCEDURA
154300991011     C                   MOVEL     k12OP1        DEPOP1                         *LIVELLO PROGRAMMA
154400060726     C*
154500060726     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
154600060726     C                   IF        DEPOP1 = 'F06' OR
154700150506     C                             DEPOP1 = 'O03'
154800060726     C                   EVAL      VIDDDE = DATCORINV
154900060726     C                   EVAL      VIDDSC = 31122039
155000060726     C                   ENDIF
155100970526     C*
155200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
155300970526     C                   EXSR      IMPFUN
155400970526     C*
155500970526     C                   ENDSR
155600970526     C*------------------------------------------------------------------------*
155700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
155800970526     C*------------------------------------------------------------------------*
155900970526     C     FUNGES        BEGSR
156000970526     C*
156100970526     C* PULISCO CAMPI DI LAVORO
156200970526     C                   Z-ADD     0             W                 5 0
156300970526     C*
156400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
156500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
156600970526     C     1             DO        24            I
156700970526     C                   MOVEL     FUD(I)        FUDRIG
156800970526     C*
156900970526     C* PULIZIA FUNZIONI PRECEDENTI
157000970526     C                   MOVEL     *BLANKS       FUV(I)
157100970526     C*
157200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
157300970526     C* (SALTA F24)
157400970526     C     FUDATT        IFEQ      'S'
157500970526     C     FUDFUN        ANDNE     '24'
157600970526     C                   ADD       1             W
157700970526     C                   MOVEL     FUDDES        FUV(W)
157800970526     C                   END
157900970526     C                   END                                                    DO WHILE
158000970526     C*
158100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
158200970526     C* ALTRIMENTI DISATTIVO F24
158300970526     C     1             DO        24            I
158400970526     C                   MOVEL     FUD(I)        FUDRIG
158500970526     C*
158600970526     C     FUDFUN        IFEQ      '24'
158700970526     C     W             IFGT      6
158800970526     C                   MOVEL     'S'           FUDATT
158900970526     C                   ELSE
159000970526     C                   MOVEL     *BLANKS       FUDATT
159100970526     C                   END                                                    W GT 8
159200970526     C                   MOVEL     FUDRIG        FUD(I)
159300970526     C                   END
159400970526     C                   END                                                    DO WHILE
159500970526     C*
159600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
159700970526     C     W             DIV       6             I
159800970526     C                   MVR                     Z
159900970526     C*
160000970526     C     Z             IFNE      0
160100970526     C     I             ADD       1             Z
160200970526     C                   ELSE
160300970526     C                   Z-ADD     I             Z                 5 0
160400970526     C                   END
160500970526     C*
160600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
160700970526     C                   ADD       1             FUNGRU
160800970526     C     FUNGRU        IFGT      Z
160900970526     C     FUNGRU        ORLT      1
161000970526     C                   Z-ADD     1             FUNGRU
161100970526     C                   END
161200970526     C*
161300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
161400970526     C     FUNGRU        IFEQ      1
161500970526     C                   MOVEL     FUV(1)        FUN001
161600970526     C                   MOVEL     FUV(2)        FUN002
161700970526     C                   MOVEL     FUV(3)        FUN003
161800970526     C                   MOVEL     FUV(4)        FUN004
161900970526     C                   MOVEL     FUV(5)        FUN005
162000970526     C                   MOVEL     FUV(6)        FUN006
162100970526     C                   END
162200970526     C*
162300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
162400970526     C     FUNGRU        IFEQ      2
162500970526     C                   MOVEL     FUV(7)        FUN001
162600970526     C                   MOVEL     FUV(8)        FUN002
162700970526     C                   MOVEL     FUV(9)        FUN003
162800970526     C                   MOVEL     FUV(10)       FUN004
162900970526     C                   MOVEL     FUV(11)       FUN005
163000970526     C                   MOVEL     FUV(12)       FUN006
163100970526     C                   END
163200970526     C*
163300970526     C* TERZO GRUPPO DI 6 FUNZIONI
163400970526     C     FUNGRU        IFEQ      3
163500970526     C                   MOVEL     FUV(13)       FUN001
163600970526     C                   MOVEL     FUV(14)       FUN002
163700970526     C                   MOVEL     FUV(15)       FUN003
163800970526     C                   MOVEL     FUV(16)       FUN004
163900970526     C                   MOVEL     FUV(17)       FUN005
164000970526     C                   MOVEL     FUV(18)       FUN006
164100970526     C                   END
164200970526     C*
164300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
164400970526     C     FUNGRU        IFEQ      4
164500970526     C                   MOVEL     FUV(19)       FUN001
164600970526     C                   MOVEL     FUV(20)       FUN002
164700970526     C                   MOVEL     FUV(21)       FUN003
164800970526     C                   MOVEL     FUV(22)       FUN004
164900970526     C                   MOVEL     *BLANKS       FUN005
165000970526     C                   MOVEL     *BLANKS       FUN006
165100970526     C                   END
165200970526     C*
165300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
165400970526     C                   MOVEL     *BLANKS       FUN007
165500970526     C                   MOVEL     *BLANKS       FUN008
165600970526     C*
165700970526     C     1             DO        24            I
165800970526     C                   MOVEL     FUD(I)        FUDRIG
165900970526     C     FUDFUN        IFEQ      '24'
166000970526     C     FUDATT        ANDEQ     'S'
166100970526     C                   MOVEL     FUDDES        FUN008
166200970526     C                   END
166300970526     C                   END                                                    DO
166400970526     C*
166500970526     C                   ENDSR
166600970526     C*------------------------------------------------------------------------*
166700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
166800970526     C*------------------------------------------------------------------------*
166900970526     C     FUNCON        BEGSR
167000970526     C*
167100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
167200970526     C                   EXSR      FUNVAR
167300970526     C*
167400970526     C* PULISCO CAMPI DI LAVORO
167500970526     C                   Z-ADD     0             I                 5 0
167600970526     C                   Z-ADD     0             W                 5 0
167700970526     C*
167800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
167900970526     C     FUNKEY        IFNE      *BLANKS
168000970526     C     1             DO        24            I
168100970526     C                   MOVEL     FUD(I)        FUDRIG
168200970526     C*
168300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
168400970526     C     FUNKEY        IFEQ      FUDFUN
168500970526     C     FUDATT        ANDEQ     'S'
168600970526     C                   Z-ADD     I             W
168700970526     C                   END
168800970526     C                   END                                                    DO WHILE
168900970526     C*
169000970526     C* FUNZIONE NON ATTIVA (ERRORE)
169100970526     C     W             IFEQ      0
169200970526     C*
169300970526     C* EMISSIONE MESSAGGIO DI ERRORE
169400970526     C                   MOVEL     MSG(1)        DSMSMS
169500970526     C*
169600970526     C                   END                                                    W=0
169700970526     C                   END                                                    FUNKEY NE *BLANKS
169800970526     C*
169900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
170000970526     C     W             IFNE      0
170100970526     C     FUNKEY        OREQ      *BLANKS
170200970526     C     FUNKEY        IFEQ      *BLANKS
170300970526     C                   EXSR      ENTGES
170400970526     C                   END
170500970526     C     FUNKEY        IFEQ      '03'
170600970526     C                   EXSR      F03GES
170700970526     C                   END
170800970526     C     FUNKEY        IFEQ      '06'
170900970526     C                   EXSR      F06GES
171000970526     C                   END
171100970526     C     FUNKEY        IFEQ      '12'
171200970526     C                   EXSR      F12GES
171300970526     C                   END
171400970526     C     FUNKEY        IFEQ      '24'
171500970526     C                   EXSR      F24GES
171600970526     C                   END
171700970526     C                   END
171800970526     C*
171900970526     C                   ENDSR
172000970526     C*------------------------------------------------------------------------*
172100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
172200970526     C*------------------------------------------------------------------------*
172300970526     C     FUNVAR        BEGSR
172400970526     C*
172500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
172600970526     C*
172700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
172800970526     C     KEY           IFEQ      ENTER
172900970526     C                   MOVEL     *BLANKS       FUNKEY
173000970526     C                   END
173100970526     C     KEY           IFEQ      F1
173200970526     C                   MOVEL     '01'          FUNKEY
173300970526     C                   END
173400970526     C     KEY           IFEQ      F2
173500970526     C                   MOVEL     '02'          FUNKEY
173600970526     C                   END
173700970526     C     KEY           IFEQ      F3
173800970526     C                   MOVEL     '03'          FUNKEY
173900970526     C                   END
174000970526     C     KEY           IFEQ      F4
174100970526     C                   MOVEL     '04'          FUNKEY
174200970526     C                   END
174300970526     C     KEY           IFEQ      F5
174400970526     C                   MOVEL     '05'          FUNKEY
174500970526     C                   END
174600970526     C     KEY           IFEQ      F6
174700970526     C                   MOVEL     '06'          FUNKEY
174800970526     C                   END
174900970526     C     KEY           IFEQ      F7
175000970526     C                   MOVEL     '07'          FUNKEY
175100970526     C                   END
175200970526     C     KEY           IFEQ      F8
175300970526     C                   MOVEL     '08'          FUNKEY
175400970526     C                   END
175500970526     C     KEY           IFEQ      F9
175600970526     C                   MOVEL     '09'          FUNKEY
175700970526     C                   END
175800970526     C     KEY           IFEQ      F10
175900970526     C                   MOVEL     '10'          FUNKEY
176000970526     C                   END
176100970526     C     KEY           IFEQ      F11
176200970526     C                   MOVEL     '11'          FUNKEY
176300970526     C                   END
176400970526     C     KEY           IFEQ      F12
176500970526     C                   MOVEL     '12'          FUNKEY
176600970526     C                   END
176700970526     C     KEY           IFEQ      F13
176800970526     C                   MOVEL     '13'          FUNKEY
176900970526     C                   END
177000970526     C     KEY           IFEQ      F14
177100970526     C                   MOVEL     '14'          FUNKEY
177200970526     C                   END
177300970526     C     KEY           IFEQ      F15
177400970526     C                   MOVEL     '15'          FUNKEY
177500970526     C                   END
177600970526     C     KEY           IFEQ      F16
177700970526     C                   MOVEL     '16'          FUNKEY
177800970526     C                   END
177900970526     C     KEY           IFEQ      F17
178000970526     C                   MOVEL     '17'          FUNKEY
178100970526     C                   END
178200970526     C     KEY           IFEQ      F18
178300970526     C                   MOVEL     '18'          FUNKEY
178400970526     C                   END
178500970526     C     KEY           IFEQ      F19
178600970526     C                   MOVEL     '19'          FUNKEY
178700970526     C                   END
178800970526     C     KEY           IFEQ      F20
178900970526     C                   MOVEL     '20'          FUNKEY
179000970526     C                   END
179100970526     C     KEY           IFEQ      F21
179200970526     C                   MOVEL     '21'          FUNKEY
179300970526     C                   END
179400970526     C     KEY           IFEQ      F22
179500970526     C                   MOVEL     '22'          FUNKEY
179600970526     C                   END
179700970526     C     KEY           IFEQ      F23
179800970526     C                   MOVEL     '23'          FUNKEY
179900970526     C                   END
180000970526     C     KEY           IFEQ      F24
180100970526     C                   MOVEL     '24'          FUNKEY
180200970526     C                   END
180300970526     C     KEY           IFEQ      ROLLUP
180400970526     C                   MOVEL     'RU'          FUNKEY
180500970526     C                   END
180600970526     C     KEY           IFEQ      HELP
180700970526     C                   MOVEL     'HP'          FUNKEY
180800970526     C                   END
180900970526     C                   ENDSR
181000970526     C*------------------------------------------------------------------------*
181100970526     C* NOTE COSTRUZIONE SCHIERE
181200970526     C*------------------------------------------------------------------------*
181300970526     C*
181400970526     C* -MSG-MESSAGGI
181500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
181600970526     C* (NON TOCCARE MSG,1 E MSG,2)
181700970526     C*
181800970526     C* -CMD-COMANDI
181900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
182000970526     C*
182100970526     C* -OPD-OPZIONI
182200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
182300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
182400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
182500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
182600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
182700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
182800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
182900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
183000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
183100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
183200970526     C*
183300970526     C* -FUD-FUNZIONI
183400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
183500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
183600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
183700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
183800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
183900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
184000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
184100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
184200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
184300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
184400970526     O*------------------------------------------------------------------------*
184500970526** DEC - DECODIFICHE
184600970526    MODIFICA                                                      1
184700970526  ANNULLAMENTO                                                    2
184800970526VISUALIZZAZIONE                                                   3
184900970526   RIPRISTINO                                                     4
185000970526  INSERIMENTO                                                     5
185100991014     COPIA                                                        6
185200970526** MSG - MESSAGGI
185300991011TIBS13R- Tasto funzionale non gestito                                          1
185400991011TIBS13R- Inserimento effettuato                                                2
185500991011TIBS13R- Modifica effettuata                                                   3
185600991011TIBS13R- Annullamento effettuato                                               4
185700991011TIBS13R- Ripristino effettuato                                                 5
185800991011TIBS13R- Richiesta funzione per record inesistente                             6
185900991011TIBS13R- Record momentaneamente allocato: riprovare più tardi                  7
186000991011TIBS13R- Richiesta funzione per record esistente                               8
186100000307TIBS13R- Codice figlio già presente come codice padre con questo legame        9
186200010228TIBS13R- Codice figlio già associato nel periodo ad un'altro padre             10
186300991011TIBS13R- Valore inserito errato i valori ammessi sono(S=Sosp A=Ann Blank)      11
186400991011TIBS13R- Tipo legame errato verificarne sulla tabella l'esistenza              12
186500991011TIBS13R- Codice padre errato                                                   13
186600991011TIBS13R- Data errata                                                           14
186700991011TIBS13R- La data scadenza deve essere maggiore della decorrenza                15
186800991011TIBS13R- Il record non può scadere in un giorno già trascorso                  16
186900991011TIBS13R- Il record immesso si sovrappone ad uno già in decorrenza              17
187000991011TIBS13R- Codice figlio errato                                                  18
187100000211TIBS13R- Codice padre già presente come codice figlio con questo legame        19
187200991014TIBS13R- Il codice figlio non può essere uguale al codice padre                20
187300000211TIBS13R- Codice padre obbligatorio                                             21
187400000807TIBS13R- Codice figlio obbligatorio                                            22
187500000807TIBS13R- Tipo legame non autorizzato.                                          23
187600060113TIBS13R- Utente non abilitato alla gestione dei clienti scelti.                24
187700000808TIBS13R- Richiesta inserimento per dati già esistenti, possibile solo modifica 25
187800020211TIBS13R- Codice figlio già presente nelle abilitazioni.                        26
187900060113TIBS13R- Errore in caricamento dati utente, avvisare il CED                    27
188000090806TIBS13R- Codice unificante risulta già padre su legame di tipo analogo.        28
188100150505TIBS13R- Non ammessa una Data Decorrenza antecedente la data Odierna.          29
188200160607TIBS13R- Legame LT Cod.padre deve essere cliente corriere il figlio logistica  30
188300160607TIBS13R- Legame ST Cod.padre e figlio devono appartenere allo stesso Network   31
188400970526** FUD - FUNZIONI
188500970526                   -        (S/N)=N                        funz.01¬
188600970526                   -        (S/N)=N                        funz.02¬
188700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
188800970526                   -        (S/N)=N                        funz.04¬
188900970526                   -        (S/N)=N                        funz.05¬
189000970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
189100970527                   -        (S/N)=N                        funz.07¬
189200970527                   -        (S/N)=N                        funz.08¬
189300970527                   -        (S/N)=N                        funz.09¬
189400970527                   -        (S/N)=N                        funz.10¬
189500970526                   -        (S/N)=N                        funz.11¬
189600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
189700970526                   -        (S/N)=N                        funz.13¬
189800970526                   -        (S/N)=N                        funz.14¬
189900970526                   -        (S/N)=N                        funz.15¬
190000970526                   -        (S/N)=N                        funz.16¬
190100970526                   -        (S/N)=N                        funz.17¬
190200970526                   -        (S/N)=N                        funz.18¬
190300970526                   -        (S/N)=N                        funz.19¬
190400970526                   -        (S/N)=N                        funz.20¬
190500970526                   -        (S/N)=N                        funz.21¬
190600970526                   -        (S/N)=N                        funz.22¬
190700970526                   -        (S/N)=N                        funz.23¬
190800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
