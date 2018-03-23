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
002700170906     D MSG             S             78    DIM(32) CTDATA PERRCD(1)
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
050500170906     C*
050600170906     C*  CHIODO per la LOGISTICA che può usarla solo POC212
050700170906     C                   if        ERRL00 = '0' and vidTLE ='LT' and
050800170906     C                               knmus <> 'POC212    ' and
050900170906     C                              tipprf <> 'EDP'
051000170906     C                   seton                                        66
051100170906     C                   movel     '1'           ERRL00
051200170906     C                   movel     MSG(32)       DSMSMS
051300170906     C                   endif
051400000807     C*
051500000807     C                   ENDSR
051600970526     C*------------------------------------------------------------------------*
051700991011     C* LETkun - LEGGE IL RECORD PERL'AGGIORNAMENTO
051800970526     C*------------------------------------------------------------------------*
051900991011     C     LETkun        BEGSR
052000970526     C*
052100000808     C                   MOVEL     vidtle        KCEtle
052200000808     C                   MOVEL     vidazp        KCEazp
052300000808     C                   MOVEL     vidazf        KCEazf
052400000808     C                   MOVE(p)   vidcop        KCEcop
052500000808     C                   MOVE(p)   vidcof        KCEcof
052600000808     C                   Z-ADD     ddevid        KCEdde
052700991008     C     KEYkun        CHAIN     tikun01L                           9998
052800000904     C                   IF        DEPOP1 = 'F06' OR
052900000904     C                             DEPOP1 = 'O03'
053000000808IF  1C     *IN99         IFEQ      *OFF                                         *GIA' PRESENTE
053100000808     C                   MOVEL     '1'           ERRL00                         *ERRORE
053200000808E   1C                   ENDIF
053300000808     C                   ELSE
053400970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
053500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
053600991008     C                   MOVEL     MSG(7)        k12MSG                         *MESSAGGIO
053700000808     C                   MOVEL     '1'           FINL00                         *FINE PGM
053800970526E   1C                   ENDIF
053900970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
054000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
054100991008     C                   MOVEL     MSG(6)        k12MSG                         *MESSAGGIO
054200000808     C                   MOVEL     '1'           FINL00                         *FINE PGM
054300970526E   1C                   ENDIF
054400000808     C                   ENDIF
054500970526     C*
054600970526     C                   ENDSR
054700970526     C*------------------------------------------------------------------------*
054800970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
054900970526     C*------------------------------------------------------------------------*
055000970526     C     DECODI        BEGSR
055100020211     C*
055200000904     C*--------------------
055300000904     C* DATA DECORRENZA
055400000904     C*--------------------
055500000904     C     *LIKE         DEFINE    VIDDDE        DDEVID
055600000904IF  1C     ERRL00        IFEQ      '0'
055700000904     C                   Z-ADD     VIDDDE        wda8
055800000904     C                   Z-ADD     *ZEROS        da8w
055900000904     C                   EXSR      CHKDA8
056000000904     C                   Z-ADD     WDA8          viddde
056100000904     C                   Z-ADD     DA8W          ddevid
056200000904IF  2C     ERRL00        IFEQ      '1'
056300000904     C                   SETON                                        63
056400000904     C                   MOVEL     MSG(14)       dsmsms
056500000904E   2C                   ENDIF
056600150505      *
056700150505      * non è ammesso solo x "CA"
056800150505      * inserire una data decorrenza antecedente alla UDATE
056900150505     c                   if        vidTLE ='CA' and
057000150505     c                             ddevid < %dec(%date())
057100150505     c                   if        DEPOP1 = 'F06' or
057200150505     c                             DEPOP1 = 'O03'
057300150505     C                   SETON                                        63
057400150505     C                   MOVEL     MSG(29)       dsmsms
057500150505     c                   end
057600150505     c                   end
057700150505      *
057800000904E   1C                   ENDIF
057900000904     C*--------------------
058000000904     C* DATA SCADENZA
058100000904     C*--------------------
058200000904     C     *LIKE         DEFINE    VIDDSC        DSCVID
058300000904IF  1C     ERRL00        IFEQ      '0'
058400000904     C                   Z-ADD     VIDDSC        WDA8
058500000904     C                   Z-ADD     *ZEROS        DA8W
058600000904     C                   EXSR      CHKDA8
058700000904     C                   Z-ADD     WDA8          VIDDSC
058800000904     C                   Z-ADD     DA8W          DSCVID
058900000904IF  2C     ERRL00        IFEQ      '1'
059000000904     C                   SETON                                        64
059100000904     C                   MOVEL     MSG(14)       DSMSMS
059200000904E   2C                   ENDIF
059300000904E   1C                   ENDIF
059400000308     C*
059500000308     C* IN CASO DI RICHIESTA DI ANNULLAMENTO BY-PASSO I CONTROLLI DI VALIDITA'
059600000308     C     DEPOP1        IFNE      'O04'                                        *ANNULLAMENTO
059700000811     C     DEPOP1        ANDNE     'O05'                                        *VISUALIZZAZIONE
059800021024     C     DEPOP1        ANDNE     'O07'                                        *RIPRISTINO
059900991008     C*--------------------
060000991008     C* tipo legame
060100991008     C*--------------------
060200991008IF  1C     ERRL00        IFEQ      '0'
060300000808     C                   move      *blank        wtlecu
060400000808     C                   move      *blank        wtlece
060500991008IF  2C     VIDtle        IFne      *blank
060600000322     C                   exsr      cnttle
060700991008X   2C                   ELSE
060800991008     C                   MOVEL     '1'           ERRL00
060900991011     C                   SETON                                        66
061000991011     C                   MOVEL     MSG(12)       DSMSMS
061100991008E   2C                   ENDIF
061200991008E   1C                   ENDIF
061300000904     C*--------------------
061400000904     C* codice padre
061500000323     C*--------------------
061600011206IF  1C     ERRL00        IFEQ      '0'
061700011206     C                   MOVEL     VIDcop        chkPot            4 0
061800011206IF  1C                   IF        chkPot > *zeros
061900011206     C                   SETON                                        61
062000011206     C                   MOVEL     MSG(13)       dsmsms
062100011206     C                   MOVE      '1'           errl00
062200011206     C                   MOVEL     *ZEROS        copVID
062300011206E   1C                   ENDIF
062400011206E   1C                   ENDIF
062500011206     C*
062600000323IF  1C     ERRL00        IFEQ      '0'
062700000808     C     *LIKE         DEFINE    kuncop        copVID
062800000323     C                   MOVEL     *BLANKS       VDDcop
062900000323IF  2C     VIDcop        IFEQ      *blanks                                      *STANDARD
063000000323IF  2C     VIDcop        oreq      *zeros                                       *STANDARD
063100000323     C                   SETON                                        61
063200000323     C                   MOVEL     MSG(21)       dsmsms
063300000323     C                   MOVE      '1'           errl00
063400000323     C                   MOVEL     *ZEROS        copVID
063500000323X   2C                   ELSE
063600000323     C                   move      wtlecu        clipot            1
063700000323     C                   MOVEL     VIDcop        Wcop
063800000323     C                   MOVEL     *blank        Wdcop
063900000323     C                   EXSR      CHKcop
064000000323     C                   MOVEL     Wcop          VIDcop
064100000323     C                   MOVEL     WDcop         VDDcop
064200000323     C                   MOVEL     VIDcop        copVID
064300160606      * ntw padre
064400160607     c                   clear                   ntwp
064500160607     C                   move(p)   vidcop        STRATECLI
064600160607     c     filcli        chain     azorg01l
064700160607     c                   if        %found(azorg01l)
064800160607     c                   movel     orgde3        og143
064900160607     c                   move      §ogntw        ntwp
065000160607     c                   endif
065100160607      *
065200160607IF  3C     ERRL00        IFEQ      '1'
065300160607     C                   SETON                                        61
065400160607     C                   MOVEL     MSG(13)       DSMSMS
065500160607E   3C                   ENDIF
065600160607E   2C                   ENDIF
065700160607E   1C                   ENDIF
065800000904     c*
065900160608     c* Verifico che il codice "padre" non sia un già presente per legame LT
066000160608IF  1C     ERRL00        IFEQ      '0'
066100160608     c     vidtle        andeq     'LT'
066200160623     C     DEPOP1        ANDNE     'O02'                                        *VISUALIZZAZIONE
066300160608     c                   move      vidcop        kuncop
066400160608     c     kkun03        setll     tikun03l
066500160608     c                   do        *hival
066600160608     c     kkun03        reade     tikun03l
066700160608     c                   if        %eof(tikun03l)
066800160608     c                   leave
066900160608     c                   endif
067000160608     c                   if        kunvat = *blanks
067100160608     C                   SETON                                        61
067200160608     C                   MOVEL     MSG(28)       DSMSMS
067300160608     C                   MOVE      '1'           errl00
067400160608     c                   endif
067500160608     c                   enddo
067600160608     c                   endif
067700000904     c* Verifico che il codice "padre" non sia un già un "figlio"
067800150505     c*  tranne x il legame è "CA"
067900000323IF  1C     ERRL00        IFEQ      '0'
068000150505     c     vidtle        andne     'CA'
068100000904     C                   move      copvid        kpar3
068200000904     c     keypar        setll     tikun02l
068300160606     c     keypar        reade     tikun02l                               99
068400000904     c                   dow       not %eof
068500000904     c                   if        not *in99             AND
068600000904     c                             ((ddevid > xkundsc    AND
068700000904     c                               dscvid > xkundsc)   OR
068800000904     c                              (ddevid < xkundde    AND
068900000904     c                               dscvid < xkundde))
069000000904     c                   else
069100000904     c                   if        xkunvat = *blanks
069200000904     C                   SETON                                        61
069300000904     C                   MOVEL     MSG(19)       DSMSMS
069400000904     C                   MOVE      '1'           errl00
069500000904     c                   endif
069600000904     c                   endif
069700160606     c     keypar        reade     tikun02l                               99
069800000904     c                   enddo
069900000808     C                   ENDIF
070000000323     C*--------------------
070100000323     C* codice figlio
070200000323     C*--------------------
070300011206IF  1C     ERRL00        IFEQ      '0'
070400011206     C                   MOVEL     VIDcof        chkPot            4 0
070500011206IF  1C                   IF        chkPot > *zeros
070600011206     C                   SETON                                        61
070700011206     C                   MOVEL     MSG(18)       dsmsms
070800011206     C                   MOVE      '1'           errl00
070900011206     C                   MOVEL     *ZEROS        cofVID
071000011206E   1C                   ENDIF
071100011206E   1C                   ENDIF
071200011206     C*
071300000323IF  1C     ERRL00        IFEQ      '0'
071400000808     C     *LIKE         DEFINE    kuncof        cofVID
071500000323     C                   MOVEL     *BLANKS       VDDcof
071600000323IF  2C     VIDcof        IFEQ      *blanks                                      *STANDARD
071700000323IF  2C     VIDcof        oreq      *zeros                                       *STANDARD
071800000323     C                   SETON                                        65
071900000323     C                   MOVEL     MSG(22)       dsmsms
072000000323     C                   MOVE      '1'           errl00
072100000323     C                   MOVEL     *ZEROS        cofVID
072200000323X   2C                   ELSE
072300000323     c                   movel     wtlece        clipot
072400000323     C                   MOVEL     VIDcof        Wcop
072500000323     C                   MOVEL     *blank        Wdcop
072600000323     C                   EXSR      CHKcop
072700000323     C                   MOVEL     Wcop          VIDcof
072800000323     C                   MOVEL     WDcop         VDDcof
072900000323     C                   MOVEL     VIDcof        cofVID
073000160606      * ntw figlio
073100160606     c                   clear                   ntwf
073200160606     C                   move(p)   vidcof        STRATECLI
073300160606     c     filcli        chain     azorg01l
073400160606     c                   if        %found(azorg01l)
073500160606     c                   movel     orgde3        og143
073600160606     c                   move      §ogntw        ntwf
073700160606     c                   endif
073800160606      *
073900000323IF  3C     ERRL00        IFEQ      '1'
074000000323     C                   SETON                                        65
074100000323     C                   MOVEL     MSG(18)       dsmsms
074200000323E   3C                   ENDIF
074300000323E   2C                   ENDIF
074400000323E   1C                   ENDIF
074500160606      * controllo il tipo legame sulla base del network
074600160606      * per tipo legame ST il ntw padre figlio devono essere dello stesso tipo
074700160606      * per tipo legame LT il ntw padre deve essere corriere e il figlio logistica
074800160606     C     errl00        ifeq      '0'
074900160606     c                   select
075000160606     c                   when      vidtle = 'LT'
075100160606     c                   if        ntwp = 'LOG' or ntwf <> 'LOG'
075200160606     C                   seton                                        6165
075300160606     C                   MOVEL     '1'           errl00
075400160606     C                   MOVEL     MSG(30)       dsmsms
075500160606     c                   endif
075600160606     c                   when      vidtle = 'ST'
075700160606     c                   if        ntwp = 'LOG' and ntwf <> 'LOG' or
075800160606     c                             ntwp <> 'LOG' and ntwf = 'LOG'
075900160606     C                   seton                                        6165
076000160606     C                   MOVEL     '1'           errl00
076100160606     C                   MOVEL     MSG(31)       dsmsms
076200160606     c                   endif
076300160606     c                   endsl
076400160606     c                   endif
076500160606      *
076600000323     C     errl00        ifeq      '0'
076700150505      *** chiodo x "CA" deve essere possibile inserire lo stesso codice
076800000323     C     vidcop        ifeq      vidcof
076900150505     C     vidtle        andNE     'CA'
077000000323     C                   seton                                        65
077100000323     C                   MOVEL     '1'           errl00
077200000323     C                   MOVEL     MSG(20)       dsmsms
077300000323     C                   endif
077400000323     C                   endif
077500000904     c*
077600000904     c* Verifico che il codice "figlio" non sia già un "padre"
077700150505     c*  tranne x il legame è "CA"
077800000323IF  1C     ERRL00        IFEQ      '0'
077900150505     c     vidtle        andne     'CA'
078000000323     C                   move      cofvid        kpar3
078100000808     C     keypar        setll     tikun01l
078200160606     C     keypar        reade(n)  tikun01l                               99
078300000808     C                   dow       not %eof
078400000904     c                   if        not *in99             AND
078500000904     c                             ((ddevid > kundsc    AND
078600000904     c                               dscvid > kundsc)   OR
078700000904     c                              (ddevid < kundde    AND
078800000904     c                               dscvid < kundde))
078900000904     c                   else
079000000904     c                   if        kunvat = *blanks
079100000904     C                   SETON                                        65
079200000904     C                   MOVEL     MSG(9)        DSMSMS
079300000904     C                   MOVE      '1'           errl00
079400000904     c                   endif
079500000904     c                   endif
079600160606     C     keypar        reade(n)  tikun01l                               99
079700000808     C                   enddo
079800000808     C                   ENDIF
079900000904     c*
080000000904     c* Verifico che il codice "figlio" non sia un già "figlio" di un diverso "padre"
080100000808IF  1C     ERRL00        IFEQ      '0'
080200000904     C                   move      cofvid        kpar3
080300000808     C     keypar        setll     tikun02l
080400160606     C     keypar        reade     tikun02l                               99
080500000808     C                   dow       not %eof
080600000904     c                   if        not *in99             AND
080700000904     c                             ((ddevid > xkundsc    AND
080800000904     c                               dscvid > xkundsc)   OR
080900000904     c                              (ddevid < xkundde    AND
081000000904     c                               dscvid < xkundde))
081100000904     c                   else
081200000904     c                   if        xkunvat =  *blanks AND
081300000904     c                             xkuncop <> copvid
081400000904     C                   SETON                                        65
081500010228     C                   EVAL      DSMSMS = %trim(MSG(10)) + ' ' +
081600010228     C                                      %trim(%editc(xkuncop : '4'))
081700000904     C                   MOVE      '1'           errl00
081800010228     C                   endif
081900010228     C                   endif
082000160606     C     keypar        reade     tikun02l                               99
082100000808     C                   enddo
082200000808     C                   ENDIF
082300000808     C*--------------------
082400000323     C* inserimento codice azienda (attualmente non abilitato)
082500000808     C*--------------------
082600000323IF  1C     ERRL00        IFEQ      '0'
082700000323IF  2C     VIDazf        IFne      *blank
082800000323IF  3C     VIDazp        orne      *blank
082900000323     C                   SETON                                        6260
083000000323     C                   MOVEL     MSG(19)       dsmsms
083100000323     C                   MOVEL     '1'           errl00
083200000323E   2C                   ENDIF
083300000323E   1C                   ENDIF
083400000322     C* SCADENZA > DECORRENZA
083500000322IF  1C     ERRL00        IFEQ      '0'
083600000322IF  2C     DSCVID        IFLT      DDEVID
083700000322     C                   SETON                                        64
083800000322     C                   MOVEL     MSG(15)       DSMSMS
083900000322     C                   MOVEL     '1'           ERRL00
084000000322E   2C                   ENDIF
084100000322E   1C                   ENDIF
084200000901     C*--------------------
084300000912     C* SCADENZA >= DATA CORRENTE  ===> "Asterisco questo controllo"
084400000901     C*--------------------
084500000912IF  1C*    ERRL00        IFEQ      '0'
084600000912IF  2C*    DEPOP1        IFEQ      'O02'
084700000912     C*    DEPOP1        OREQ      'O07'
084800000912     C*    DEPOP1        OREQ      'F06'
084900000912     C*    DEPOP1        OREQ      'O03'
085000000912IF  3C*    DSCVID        IFLT      DATCOR
085100000912     C*                  SETON                                        64
085200000912     C*                  MOVEL     MSG(16)       DSMSMS
085300000912     C*                  MOVEL     '1'           ERRL00
085400000912E   3C*                  ENDIF
085500000912E   2C*                  ENDIF
085600000912E   1C*                  ENDIF
085700000904     C*--------------------
085800000904     c* scadenza <=> decorrenza  (accavalamento periodo)
085900000904     c*--------------------
086000000904IF  1c                   if        errl00 = '0'
086100000904     c                   movel     vidtle        kcetle
086200000904     c                   z-add     copvid        kcecop
086300000904     c                   z-add     cofvid        kcecof
086400000904     c                   movel     vidazp        kceazp
086500000904     c                   movel     vidazf        kceazf
086600000904     c     KE2kun        setll     tikun01L
086700000904     c     KE2kun        reade(n)  tikun01L                               99
086800000904DO  2c                   dow       *in99 = *OFF
086900000904     c                   if        (ddevid > kundsc  AND
087000000904     c                              dscvid > kundsc) OR
087100000904     c                             (ddevid < kundde  AND
087200000904     c                              dscvid < kundde)
087300000904     c                   else
087400000904     c                   if        ddevid <> kundde AND
087500000904     c                             kunvat  =  *blanks
087600000904     C                   seton                                        606162
087700000904     C                   seton                                        6364
087800000904     C                   movel     MSG(17)       DSMSMS
087900000904     C                   movel     '1'           ERRL00
088000000904     c                   leave
088100000904     c                   endif
088200000904     c                   endif
088300000904     c     KE2kun        reade(n)  tikun01L                               99
088400000904E   2c                   enddo
088500000904E   1c                   endif
088600000904     c*
088700000307     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (X CLIENTE "PADRE"/"FIGLIO")
088800000307     C                   EXSR      CHKAUT
088900000808     C*--------------------
089000000811     C* inserimento
089100000808     C*--------------------
089200000808     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
089300000904     C     DEPOP1        OREQ      'O03'                                        *COPIA
089400000808IF  1C     ERRL00        IFEQ      '0'
089500000808     C                   EXSR      LETkun
089600000808IF  1C     ERRL00        IFNE      '0'
089700000808     C                   MOVEL     MSG(25)       DSMSMS                         *MESSAGGIO DIRETTO
089800000808E   1C                   ENDIF
089900000808E   1C                   ENDIF
090000000808     C                   ENDIF
090100000808     C*--------------------
090200000808     C* annullamento sospensione
090300000808     C*--------------------
090400000808IF  1C     ERRL00        IFEQ      '0'
090500000808IF  2C     VIDvat        IFEQ      'S'
090600000808     C     VIDvat        OREQ      'A'
090700000808     C     VIDvat        OREQ      ' '
090800000808X   2C                   ELSE
090900000808     C                   MOVEL     '1'           ERRL00
091000000808     C                   SETON                                        67
091100000808     C                   MOVEL     MSG(11)       DSMSMS
091200000808E   2C                   ENDIF
091300000808E   1C                   ENDIF
091400000808     C*
091500020927     C*
091600020927     C* Il controllo seguente lo effettuo solo se trattasi di utente di sede in quanto in filiale
091700020927     C* il file TIVSS nn c'è
091800091117     C*                  if        simfel = *zeros
091900000308     C*
092000020927     C* VERIFICA CHE IL CLIENTE FIGLIO NON SIA GIA' UN CLIENTE ABILITATO (X SERVIZI CHE RICHIEDONO
092100020212     C* IL CONTROLLO)
092200091117     C*    ERRL00        ifeq      '0'
092300091117     C*                  if        vidtle = 'WW'
092400091117     C*                  move      vidcof        kssksu
092500091117     C*    keyvss        setll     tivss05l
092600091117     C*    keyvss        reade     tivss05l
092700091117     C*                  dow       not %eof(tivss05l)
092800091117     C*                  if        vssvat = *blanks                              *non annull/sosp.
092900091117     C*                  if        vssdsc < ddevid or                            *non valido
093000091117     C*                            vssdde > dscvid
093100091117     C*                  else                                                    *valido
093200091117     C* Verifico che il servizio del record corrente del TIVSS sia soggetto a controllo unificazioni
093300091117     C*    vssisv        lookup    isvkun                                 22
093400091117     C*                  if        *in22 = *on
093500091117     C*                  seton                                        65
093600091117     C*                  movel     MSG(26)       dsmsms
093700091117     C*                  movel     '1'           errl00
093800091117     C*                  leave
093900091117     C*                  endif
094000091117     C*                  endif
094100091117     C*                  endif
094200091117     C*    keyvss        reade     tivss05l
094300091117     C*                  enddo
094400091117     C*                  endif
094500091117     C*                  endif
094600091117     C*                  endif
094700090806     C*
094800090806     C* Verifico che codice padre nn già padre rispetto ad altro tipo legame dello stesso gruppo
094900090806     C     ERRL00        ifeq      '0'
095000090806     C                   exsr      CHKTIPTLE
095100090806     C                   endif
095200021024     C*
095300021024     C                   ENDIF
095400020211     C*
095500970528     C                   ENDSR
095600991008     C*------------------------------------------------------------------------*
095700991008     C* Cnttle - CONTROLLA codice tipo legame
095800991008     C*------------------------------------------------------------------------*
095900991008     c     cnttle        begsr
096000991008     c                   clear                   TIBS02DS
096100991008      *
096200991008     C     '?'           SCAN      Vidtle                                 30
096300991008      *  Ricerca
096400991008     c                   eval      t02tla = 'L'
096500991008    1C                   IF        *IN30 = *ON
096600991008     c                   movel     'R'           t02mod
096700991008     c                   else
096800991008     c                   movel     'C'           t02mod
096900991008     c                   movel(p)  vidtle        t02ke1
097000991008     c                   end
097100991008     C                   movel     'TLE'         t02cod
097200991008     c                   movel     knsif         t02sif
097300991008     C                   CALL      'TIBS02R'
097400991008     c                   parm                    KPJBA
097500991008     c                   parm                    TIBS02DS
097600991008     c
097700991008      *
097800991008    2C                   IF        T02err = *blanks
097900991008     C                   movel     T02ke1        Vidtle
098000991014     C                   movel     T02uni        dtle
098100991014     C                   movel     §tledesc      Vddtle
098200991014     C                   movel     §tlecu        Wtlecu            1
098300991014     C                   movel     §tlece        wtlece            1
098400991014     c                   else
098500991014     c                   movel     '1'           errl00
098600991014     c                   seton                                        66
098700991014     C                   MOVEL     MSG(12)       DSMSMS
098800991008     C                   ENDIF
098900991008     c
099000991008     c                   endsr
099100970528     C*------------------------------------------------------------------------*
099200000901     C* CHKcop - CONTROLLA codice padre figlio
099300970528     C*------------------------------------------------------------------------*
099400991008     C     CHKcop        BEGSR
099500970528     C*
099600991008     C     *LIKE         DEFINE    VIDcop        Wcop
099700991008     C     *LIKE         DEFINE    VDDcop        WDcop
099800991014     c                   move      *zeros        wksc
099900970528     C*
100000970528     C* RICHIESTO ELENCO
100100970528     C                   SETOFF                                           28
100200991008     C     '?'           SCAN      Wcop                                   28
100300970528IF  1C     *IN28         IFEQ      *ON
100400991008     C* RICHIESTO ELENCO CODICI
100500091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
100600091126      * deciso di non utilizzarli per le unificazioni dei codici
100700091126      * quindi non controllo il tipo codice e do per scontato che è sempre 'C'=cliente
100800991008     C                   CLEAR                   DSXC
100900991008     C                   Z-ADD     KCCCL         DXCCAP
101000991008     C                   CALL      'XCLIR'
101100991008     C                   PARM                    DSXC
101200991008     C* SCOMPATTA
101300991008     C                   MOVEA     DXCKSC        KSA                        ALF
101400991008DO  2C     1             DO        30            J                 3 0
101500991008     C                   MOVE      KSA(J)        DSKSA                      F
101600991008     C                   Z-ADD     DSKSN         KSC(J)                     M
101700991008E   2C                   ENDDO
101800991011     C                   MOVEL     KSC(1)        WKSC              7
101900991011     C                   MOVEL     '1'           $TESVD            1        TA
102000991008E   1C                   ENDIF
102100991008     C*
102200991008     C* CODICE CLIENTE NUMERICO
102300991008IF  1C     WKSC          IFLT      *ZEROS
102400991011     C                   MOVEL     '1'           WERR              1
102500991008X   1C                   ELSE
102600970528     C*
102700970528     C* SOLO CARATTERI NUMERICI
102800991011     c   28              move      *all'0'       wcop
102900991011     c   28              move      wksc          wcop
103000970528     C                   SETOFF                                       28
103100991008     C                   TESTN                   Wcop                 28
103200991015IF  2C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
103300970528     C                   MOVEL     '1'           ERRL00
103400991015X   2C                   ELSE                                                   *E'NUMERO DA TESTN
103500991008     C                   MOVE      Wcop          W001              1
103600991015IF  3C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
103700970528     C                   MOVEL     '1'           ERRL00
103800991015X   3C                   ELSE                                                   *NUMERO
103900970528     C*
104000970528     C* CONTROLLA ESISTENZA CODICE
104100991210     C                   MOVE      Wcop          wcli              7 0
104200991210     c                   exsr      dectib
104300991210     c                   movel     wfvrag        wdcop
104400991015E   3C                   ENDIF
104500991015E   2C                   ENDIF
104600991011E   1C                   ENDIF
104700970528     C*
104800970528     C                   ENDSR
104900991210     C*------------------------------------------------------------------------*
105000991210     C* Dectib - DECODIFICA CLIENTE
105100991210     C*------------------------------------------------------------------------*
105200991210     C     Dectib        BEGSR
105300991210     C*
105400991210     C                   CLEAR                   BS69DS
105500991210     C                   CLEAR                   ACODS
105600991210     C                   MOVEL     KNSIF         I69SIF
105700991210     C                   Z-ADD     WCLI          I69KAC
105800991210     C                   CALL      'TIBS69R'
105900991210     C                   PARM                    BS69DS
106000991210     C                   PARM                    ACODS
106100991210     C                   PARM                    INDDS
106200991210     C                   PARM                    CLPDS
106300991210     C                   PARM                    CLSDS
106400991210     C     O69ERR        IFNE      '1'
106500991210     C                   MOVEL     ACORAG        WFVRAG           40
106600991210     C                   ELSE
106700991210     C                   MOVEL     *ALL'*'       WFVRAG
106800991210     c                   movel     '1'           errl00
106900991210     C                   ENDIF
107000991210     C*
107100991210     C                   ENDSR
107200991210     C*------------------------------------------------------------------------*
107300991210     C* FINSR - OPERAZIONI FINALI
107400991210     C*------------------------------------------------------------------------*
107500991210     C     FINSR         BEGSR
107600991210     C*
107700991210     C* CHIUDE I FILE DEI PGM CHIAMATI
107800991210     C                   CLEAR                   bs69ds
107900991210     C                   CLEAR                   acods
108000991210     C                   CLEAR                   indds
108100991210     C                   CLEAR                   clpds
108200991210     C                   CLEAR                   clsds
108300991210     C                   MOVEL     'C'           i69tla
108400991210     C                   MOVEL     knsif         i69sif
108500991210     C                   CALL      'TIBS69R'
108600991210     C                   PARM                    bs69ds
108700991210     C                   PARM                    acods
108800991210     C                   PARM                    indds
108900991210     C                   PARM                    clpds
109000991210     C                   PARM                    clsds
109100991210     C*
109200991210     C                   ENDSR
109300970528     C*------------------------------------------------------------------------*
109400970528     C* CHKDA8 - CONTROLLA LA DATA (8)
109500970528     C*------------------------------------------------------------------------*
109600970528     C     CHKDA8        BEGSR
109700970528     C*
109800970528     C     *LIKE         DEFINE    VIDDDE        WDA8
109900970528     C     *LIKE         DEFINE    VIDDDE        DA8W
110000970528     C*
110100970528     C                   Z-ADD     WDA8          G08DAT
110200970528     C                   Z-ADD     *ZEROS        G08INV
110300970528     C                   MOVEL     '0'           G08ERR
110400970528     C                   CALL      'XSRDA8'
110500970528     C                   PARM                    WLBDA8
110600970528     C                   Z-ADD     G08DAT        WDA8
110700970528     C                   Z-ADD     G08INV        DA8W
110800970528IF  1C     G08ERR        IFEQ      '1'
110900970528     C                   MOVEL     '1'           ERRL00
111000970528E   1C                   ENDIF
111100970528     C*
111200970528     C                   ENDSR
111300970526     C*------------------------------------------------------------------------*
111400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
111500970526     C*------------------------------------------------------------------------*
111600970526     C     IREVID        BEGSR
111700970526     C* CAMPI
111800991011     C                   MOVEL     kuntle        VIDtle
111900991011     C                   MOVEL     kunvat        VIDvat
112000151125     C                   EVAL      VIDSTSRCD = *blank
112100151125     C                   SELECT
112200151125     C                   WHEN      VIDVAT = *blank
112300151125     C                   EVAL      VIDSTSRCD = *blank
112400151125     C                   WHEN      VIDVAT = 'A'
112500151125     C                   EVAL      VIDSTSRCD = 'ANNULLATO'
112600151125     C                   WHEN      VIDVAT = 'S'
112700151125     C                   EVAL      VIDSTSRCD = 'SOSPESO'
112800151125     C                   OTHER
112900151125     C                   EVAL      VIDSTSRCD = *blank
113000151125     C                   ENDSL
113100991011     C                   MOVEL     kunazp        VIDazp
113200991011     C                   MOVEL     kunazf        VIDazf
113300991011     C                   move      kuncop        vidcop
113400991011     C                   move      kuncof        vidcof
113500991011     C                   Z-ADD     kunDDE        G08INV
113600970528     C                   Z-ADD     *ZEROS        G08DAT
113700970528     C                   MOVEL     '3'           G08ERR
113800970528     C                   CALL      'XSRDA8'
113900970528     C                   PARM                    WLBDA8
114000970528     C                   Z-ADD     G08DAT        VIDDDE
114100991011     C                   Z-ADD     kunDSC        G08INV
114200970528     C                   Z-ADD     *ZEROS        G08DAT
114300970528     C                   MOVEL     '3'           G08ERR
114400970528     C                   CALL      'XSRDA8'
114500970528     C                   PARM                    WLBDA8
114600970528     C                   Z-ADD     G08DAT        VIDDSC
114700130412     C                   Z-ADD     kunDIM        G08INV
114800130412     C                   Z-ADD     *ZEROS        G08DAT
114900130412     C                   MOVEL     '3'           G08ERR
115000130412     C                   CALL      'XSRDA8'
115100130412     C                   PARM                    WLBDA8
115200130412     C                   Z-ADD     G08DAT        VIDDIM
115300130412     C                   Z-ADD     kunDUV        G08INV
115400130412     C                   Z-ADD     *ZEROS        G08DAT
115500130412     C                   MOVEL     '3'           G08ERR
115600130412     C                   CALL      'XSRDA8'
115700130412     C                   PARM                    WLBDA8
115800130412     C                   Z-ADD     G08DAT        VIDDUV
115900130412     C                   EVAL      VIDPUV = KUNPUV
116000060726     C*
116100060726     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
116200060726     C                   IF        DEPOP1 = 'F06' OR
116300150506     C                             DEPOP1 = 'O03'
116400060726     C                   EVAL      VIDDDE = DATCORINV
116500060726     C                   EVAL      VIDDSC = 31122039
116600060726     C                   ENDIF
116700970527     C*
116800970526     C                   ENDSR
116900970526     C*------------------------------------------------------------------------*
117000970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
117100970526     C*------------------------------------------------------------------------*
117200970526     C     AZZVID        BEGSR
117300970526     C* CAMPI
117400151125     C                   EVAL      VIDSTSRCD = *blank
117500991008     C                   MOVEL     *BLANKS       VIDtle
117600991008     C                   MOVEL     *BLANKS       VIDazf
117700991008     C                   MOVEL     *BLANKS       VIDazp
117800991008     C                   MOVEL     *BLANKS       VIDvat
117900060726     C                   EVAL      VIDDDE = DATCORINV
118000060726     C                   EVAL      VIDDSC = 31122039
118100991011     C                   move      *ZEROS        VIDcop
118200991011     C                   move      *ZEROS        VIDcof
118300970526     C* DECODIFICHE
118400991011     C                   MOVEL     *BLANKS       VDDtle
118500991008     C                   MOVEL     *BLANKS       VDDcop
118600991008     C                   MOVEL     *BLANKS       VDDcof
118700970526     C*
118800970526     C                   ENDSR
118900970526     C*------------------------------------------------------------------------*
119000991011     C* IMPkun - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
119100970526     C*------------------------------------------------------------------------*
119200991011     C     IMPkun        BEGSR
119300970526     C*
119400000913     C                   MOVEL     vidvat        kunvat                         *CHIAVE
119500991014     C                   z-add     cofVID        kuncof
119600991014     C                   Z-ADD     DDEVID        kunDDE
119700991008     C                   Z-ADD     DSCVID        kunDSC
119800970526     C*
119900970526     C                   ENDSR
120000970526     C*------------------------------------------------------------------------*
120100970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
120200970526     C*------------------------------------------------------------------------*
120300970526     C     IMPFUN        BEGSR
120400970526     C*
120500970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
120600970526IF  1C     DEPOP1        IFEQ      'O05'
120700970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
120800970526     C                   MOVEL     'N'           FUDATT
120900970526     C                   MOVEL     FUDRIG        FUD(6)
121000970526E   1C                   ENDIF
121100970526     C*
121200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
121300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
121400970526     C                   EXSR      FUNGES
121500970526     C*
121600970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
121700970526     C*---
121800970526     C* 90 -> PROTEZIONE CHIAVE
121900970526     C* 91 -> PROTEZIONE TOTALE
122000970526     C*---
122100970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
122200970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
122300000904     C     DEPOP1        OREQ      'O03'                                        *COPIA
122400970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
122500970526     C                   SETOFF                                       91
122600970526E   2C                   ENDIF
122700970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
122800000904IF  2C     DEPOP1        OREQ      'O01'                                        *SCELTA
122900970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
123000970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
123100970526E   2C                   ENDIF
123200970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
123300970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
123400970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
123500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
123600970526     C                   SETON                                        91
123700970526E   2C                   ENDIF
123800970526X   1C                   ELSE                                                   *I N T E R R O G A
123900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
124000970526     C                   SETON                                        91
124100970526E   1C                   ENDIF
124200970526     C*
124300970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
124400970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
124500991011     C                   MOVEL     k12tle        KCEtle
124600991011     C                   Z-ADD     k12cop        KCEcop
124700991011     C                   Z-ADD     k12cof        KCEcof
124800991011     C                   Z-ADD     k12dde        KCEDDE
124900991011     C     KEYkun        CHAIN     tikun01L                           9998
125000970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
125100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
125200970526     C                   MOVEL     MSG(6)        DSMSMS
125300970526X   2C                   ELSE
125400970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
125500980128IF  3C     DEPOP1        IFEQ      'O05'
125600991014IF  3C     DEPOP1        oreq      'O02'
125700980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
125800980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
125900980128E   3C                   ENDIF
126000970526E   2C                   ENDIF
126100970526X   1C                   ELSE                                                   *INSERIMENTO
126200970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
126300970526E   1C                   ENDIF
126400970526     C*
126500970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
126600970526IF  1C     DEPOP1        IFEQ      'O02'
126700970526     C                   MOVEL     DEC(1)        VIDFUN
126800970526E   1C                   ENDIF
126900991014IF  1C     DEPOP1        IFEQ      'O03'
127000991014     C                   MOVEL     DEC(6)        VIDFUN
127100991014E   1C                   ENDIF
127200970526IF  1C     DEPOP1        IFEQ      'O04'
127300970526     C                   MOVEL     DEC(2)        VIDFUN
127400970526E   1C                   ENDIF
127500970526IF  1C     DEPOP1        IFEQ      'O05'
127600970526     C                   MOVEL     DEC(3)        VIDFUN
127700970526E   1C                   ENDIF
127800970526IF  1C     DEPOP1        IFEQ      'O07'
127900970526     C                   MOVEL     DEC(4)        VIDFUN
128000970526E   1C                   ENDIF
128100970526IF  1C     DEPOP1        IFEQ      'F06'
128200970526     C                   MOVEL     DEC(5)        VIDFUN
128300970526E   1C                   ENDIF
128400970526     C*
128500970526     C                   ENDSR
128600020212     C*--------------------------------------------------------------------------------------------*
128700020212     C* CARTAB - CARICA LE TABELLE OCCORRENTI
128800020212     C*--------------------------------------------------------------------------------------------*
128900020212     C     CARTAB        BEGSR
129000020905     C*
129100020905     C* REPERISCE I DATI UTENTE
129200020905     C                   EXSR      REPDATIUTE
129300020905     C*
129400020212     C*----------
129500020212     C* Tipi servizi soggetti a controllo unificazioni
129600020212     C*----------
129700020212     C                   z-add     *zeros        x                 3 0
129800020212     C                   eval      tbecod = 'ISV'
129900020212     C     ke2tbe        setll     tntbe01l
130000020212     C     ke2tbe        reade     tntbe01l
130100020212     C                   dow       not %eof(tntbe01l)
130200020212     C                   movel     tbeuni        dsisv
130300020212     C                   if        §isvuni  = 'S'
130400020212     C                   add       1             x
130500020212     C                   eval      isvkun(x) = tbeke1
130600020212     C                   endif
130700020212     C     ke2tbe        reade     tntbe01l
130800020212     C                   enddo
130900090806     C*----------
131000090806     C* Tipi unificazioni
131100090806     C*----------
131200090806     C                   eval      tbecod = 'TLE'
131300090806     C     tbecod        setll     tntbe01l
131400160606     C     tbecod        reade     tntbe01l
131500090806     C                   dow       not %eof(tntbe01l)
131600090806     C                   movel     tbeuni        dtle
131700090806     C                   select
131800090806     C                   when      §tlegrp  = 'W'
131900090806     C                   add       1             jskTIPTLEWW
132000090806     C                   eval      skTIPTLEWW(jskTIPTLEWW) = tbeke1
132100090806     C                   when      §tlegrp  = 'S'
132200090806     C                   add       1             jskTIPTLEST
132300090806     C                   eval      skTIPTLEST(jskTIPTLEST) = tbeke1
132400090806     C                   endsl
132500160606     C     tbecod        reade     tntbe01l
132600090806     C                   enddo
132700020212     C*
132800020212     C                   ENDSR
132900020905     C*--------------------------------------------------------------------------------------------*
133000020905     C* REPERISCE I DATI UTENTE
133100020905     C*--------------------------------------------------------------------------------------------*
133200020905     C     REPDATIUTE    BEGSR
133300020905     C*
133400020905     C* INIZIALIZZA VARIABILI DI WRK
133500020905     C                   CLEAR                   TIBS34DS
133600020905     C                   CLEAR                   AZUTEDS
133700020905     C                   CLEAR                   DDATIUTE
133800020905     C*
133900020905     C     *DTAARA       DEFINE    §azute        azuteds
134000020905     C     *DTAARA       DEFINE    §datiute      ddatiute
134100020905     C                   IN(E)     *DTAARA
134200020905if  1C                   IF        %Error
134300020905     c                   EVAL      I34Tla = 'L'
134400020905     C                   CALL      'TIBS34R'
134500020905     C                   PARM                    Tibs34Ds
134600020905     C                   IN        *DTAARA
134700020905e   1C                   ENDIF
134800020905     C*
134900020905     C                   MOVEL     RSUT          DSFIRS
135000060113     C*
135100060113     C* Carica le abilitazioni particolari del profilo
135200060113     C                   Movel     *blanks       wabi              2
135300060113     C                   Movel     utefaf        dute01
135400060113     C                   If        §utecli <> *Blanks
135500060113     C                   Eval      wabi = §utecli
135600060113     C                   Else
135700060113     C                   Eval      wabi = uteaut
135800060113     C                   EndIf
135900060113     C*
136000060113     C* Reperimento dei P.O. gestibili dall'utente per i codici clienti
136100060113     C                   Clear                   Trul31ds
136200060113     C                   Eval      i31abi = wabi
136300060113     C                   Eval      i31cdi = dutdis
136400060113     C                   Eval      i31car = dutare
136500060113     C                   Eval      i31cpo = dutpou
136600060113     C                   Call      'TRUL31R'
136700060113     C                   Parm                    kpjba
136800060113     C                   Parm                    Trul31ds
136900060113     C                   If        o31pog > *zeros
137000060113     C                   Movea     o31pog        skPOG
137100060113     C                   Else
137200060113     C                   Movel     '1'           ERRl00
137300060113     C                   Movel     MSG(27)       DSMSMS
137400060113     C                   EndIf
137500020905     C*
137600020905     C                   ENDSR
137700090806     C*------------------------------------------------------------------------*
137800090806     C* CHKTIPTLE - VERIFICHE X TIPI LEGAMI
137900090806     C*------------------------------------------------------------------------*
138000090806     C     CHKTIPTLE     BEGSR
138100090806     C*
138200090806     C* VERIFICA CHE IL NUOVO PADRE NN SIA GIA' PRESENTE COME PADRE X UNO QUALUNQUE DEI
138300090806     C* LEGAMI DI TIPO 'W'
138400090806     C                   MOVE(p)   vidcop        KCEcop
138500090806     C     KCEcop        SETLL     tikun03l
138600090806     C                   IF        %found(tikun03l)
138700090806     C     KCEcop        READE     tikun03l
138800090806     C                   DOW       not %eof(tikun03l)
138900090806     C                   IF        (ddevid > kundsc    AND
139000090806     C                              dscvid > kundsc)   OR
139100090806     C                             (ddevid < kundde    AND
139200090806     C                              dscvid < kundde)
139300090806     C                   ELSE
139400091020     C* Ovviamente nn considero il record correntemente a video
139500091020     C                   IF        vidtle <> kuntle
139600090806     C*
139700090806     C                   IF        §TLEGRP = 'W'
139800090806     C     KUNTLE        LOOKUP    skTIPTLEWW                             30
139900090806     C                   IF        *IN30
140000090806     C                   SETON                                        61
140100090806     C                   MOVEL     '1'           ERRL00
140200090806     C                   EVAL      DSMSMS = MSG(28)
140300090806     C                   LEAVE
140400090806     C                   ENDIF
140500090806     C                   ENDIF
140600090806     C*
140700090806     C                   IF        §TLEGRP = 'S'
140800090806     C     KUNTLE        LOOKUP    skTIPTLEST                             30
140900090806     C                   IF        *IN30
141000090806     C                   SETON                                        61
141100090806     C                   MOVEL     '1'           ERRL00
141200090806     C                   EVAL      DSMSMS = MSG(28)
141300090806     C                   LEAVE
141400090806     C                   ENDIF
141500090806     C                   ENDIF
141600091020     C*
141700091020     C                   ENDIF
141800091020     C*
141900090806     C                   ENDIF
142000090806     C*
142100090806     C     KCEcop        READE     tikun03l
142200090806     C                   ENDDO
142300090806     C                   ENDIF
142400090806     C*
142500090806     C                   ENDSR
142600970526     C*------------------------------------------------------------------------*
142700970526     C* *INZSR - OPERAZIONI INIZIALI
142800970526     C*------------------------------------------------------------------------*
142900970526     C     *INZSR        BEGSR
143000970526     C*---
143100970526     C* RICEVIMENTO PARAMETRI
143200970526     C*---
143300970526     C     *ENTRY        PLIST
143400970612     C                   PARM                    KPJBA
143500991028     C                   movel     kpjbu         tibds
143600970526     C*
143700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
143800970526     C     XMSGPA        PLIST
143900970526     C                   PARM                    DSMSMS
144000970526     C                   PARM                    DSMSPG
144100970526     C                   PARM                    DSMSKE
144200970526     C                   PARM                    DSMSER
144300970526     C*
144400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
144500970526     C                   CALL      'XKEY'
144600970526     C                   PARM                    DSKEY
144700020212     C*
144800020212     C* CARICA LE TABELLE OCCORRENTI
144900020212     C                   EXSR      CARTAB
145000020927     C*
145100020927     C* IN BASE ALL'UTENTE EFFETTUO ALCUNE CONSIDERAZIONI
145200021007     C                   IF        simfel = *zeros
145300020927     C                   OPEN      tivss05l
145400020927     C                   ENDIF
145500970526     C*---
145600970526     C* VARIABILI NON RIFERITE AL DATA BASE
145700970526     C*---
145800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
145900991011     C     *LIKE         DEFINE    k12OP0        DEPOP0
146000991011     C     *LIKE         DEFINE    k12OP1        DEPOP1
146100991011     C     *LIKE         DEFINE    k12OP0        WOP0
146200991011     C     *LIKE         DEFINE    k12OP1        WOP1
146300970526     C*---
146400970526     C* VARIABILI DI CONTROLLO
146500970526     C*---
146600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
146700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
146800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
146900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
147000000307     C                   SETON                                        50
147100970526     C*
147200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
147300991008     C                   MOVEL     'TIBS13R'     DSMSPG
147400991008     C                   MOVEL     'TIBS13R'     MSGQUE
147500970526     C                   MOVEL     *BLANKS       MSGKEY
147600970526     C                   MOVEL     *BLANKS       DSMSMS
147700970526     C*---
147800970526     C* VARIABILI RIFERITE AL DATA BASE
147900970526     C*---
148000020830     C     *LIKE         DEFINE    acokcc        kcccl
148100991011     C     *LIKE         DEFINE    kuntle        KCEtle
148200991011     C     *LIKE         DEFINE    kuncop        KCEcop
148300991015     C     *LIKE         DEFINE    kuncop        Kpar3
148400991011     C     *LIKE         DEFINE    kuncof        KCEcof
148500991011     C     *LIKE         DEFINE    kunDDE        KCEDDE
148600991011     C     *LIKE         DEFINE    kunazf        KCEazf
148700991011     C     *LIKE         DEFINE    kunazp        KCEazp
148800000307     C     *LIKE         DEFINE    tbecod        KTBcod
148900000307     C     *LIKE         DEFINE    tbeke1        KTBky1
149000020211     C     *LIKE         DEFINE    vssksu        KSSksu
149100020905     C     *LIKE         DEFINE    kpjbu         Depkpjbu
149200020830     C*
149300020830     C* INIZIALIZZAZIONE VARIABILI DI WRK
149400020830     C                   z-add     1             kclkut
149500020916     C                   z-add     DUTKCI        kcccl
149600020830     C                   movel     *blank        kceazp
149700020830     C                   movel     *blank        kceazf
149800970526     C*---
149900970526     C* CHIAVI DI LETTURA
150000970526     C*---
150100991011     C* LETTURA tikun01L - COMPLETA
150200991011     C     KEYkun        KLIST
150300090806     C                   KFLD                    KCEtle
150400090806     C                   KFLD                    KCEazp
150500090806     C                   KFLD                    KCEcop
150600090806     C                   KFLD                    KCEazf
150700090806     C                   KFLD                    KCEcof
150800090806     C                   KFLD                    KCEdde
150900090806     C     KEYpar        KLIST
151000090806     C                   KFLD                    vidtle
151100090806     C                   KFLD                    KCEazp
151200090806     C                   KFLD                    Kpar3
151300160608     C     Kkun03        KLIST
151400160608     C                   KFLD                    kcecop
151500160608     C                   KFLD                    vidtle
151600090806     C* LETTURA tikun01L - COMPLETA
151700090806     C     KE2kun        KLIST
151800090806     C                   KFLD                    KCEtle
151900090806     C                   KFLD                    KCEazp
152000090806     C                   KFLD                    KCEcop
152100090806     C                   KFLD                    KCEazf
152200090806     C                   KFLD                    KCEcof
152300991210     C* LETTURA cnaco31c
152400991008     C     keycli        KLIST                                                  *di cnaco/ind/clp
152500991008     C                   KFLD                    kclkut            1 0          -utente
152600991008     C                   KFLD                    kcccl                          -capoconto
152700991008     C                   KFLD                    kcksc             7 0          -codice
152800000307     C* LETTURA tntbe01l -
152900000307     C     KEYtbe        KLIST
153000000307     C                   KFLD                    Ktbcod                         *CODICE TABELLA
153100000307     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
153200020211     C* LETTURA tivss02L - PARZIALE
153300020211     C     KEYvss        KLIST
153400020211     C                   KFLD                    KSSksu                         *CODICE CLI ABIL.
153500020212     C* LETTURA tntbe01l - PARZIALE
153600020212     C     KE2tbe        KLIST
153700020212     C                   KFLD                    tbecod                         *CODICE TABELLA
153800970528     C*
153900970528     C* CALCOLA LA DATA CORRENTE
154000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
154100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
154200970528     C                   Z-ADD     WN8           G08DAT
154300060726     C                   Z-ADD     WN8           DATCORINV         8 0
154400970528     C                   Z-ADD     *ZEROS        G08INV
154500970528     C                   MOVEL     '0'           G08ERR
154600970528     C                   CALL      'XSRDA8'
154700970528     C                   PARM                    WLBDA8
154800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
154900970526     C*
155000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
155100991011     C                   MOVEL     k12OP0        DEPOP0                         *LIVELLO PROCEDURA
155200991011     C                   MOVEL     k12OP1        DEPOP1                         *LIVELLO PROGRAMMA
155300060726     C*
155400060726     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
155500060726     C                   IF        DEPOP1 = 'F06' OR
155600150506     C                             DEPOP1 = 'O03'
155700060726     C                   EVAL      VIDDDE = DATCORINV
155800060726     C                   EVAL      VIDDSC = 31122039
155900060726     C                   ENDIF
156000970526     C*
156100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
156200970526     C                   EXSR      IMPFUN
156300970526     C*
156400970526     C                   ENDSR
156500970526     C*------------------------------------------------------------------------*
156600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
156700970526     C*------------------------------------------------------------------------*
156800970526     C     FUNGES        BEGSR
156900970526     C*
157000970526     C* PULISCO CAMPI DI LAVORO
157100970526     C                   Z-ADD     0             W                 5 0
157200970526     C*
157300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
157400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
157500970526     C     1             DO        24            I
157600970526     C                   MOVEL     FUD(I)        FUDRIG
157700970526     C*
157800970526     C* PULIZIA FUNZIONI PRECEDENTI
157900970526     C                   MOVEL     *BLANKS       FUV(I)
158000970526     C*
158100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
158200970526     C* (SALTA F24)
158300970526     C     FUDATT        IFEQ      'S'
158400970526     C     FUDFUN        ANDNE     '24'
158500970526     C                   ADD       1             W
158600970526     C                   MOVEL     FUDDES        FUV(W)
158700970526     C                   END
158800970526     C                   END                                                    DO WHILE
158900970526     C*
159000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
159100970526     C* ALTRIMENTI DISATTIVO F24
159200970526     C     1             DO        24            I
159300970526     C                   MOVEL     FUD(I)        FUDRIG
159400970526     C*
159500970526     C     FUDFUN        IFEQ      '24'
159600970526     C     W             IFGT      6
159700970526     C                   MOVEL     'S'           FUDATT
159800970526     C                   ELSE
159900970526     C                   MOVEL     *BLANKS       FUDATT
160000970526     C                   END                                                    W GT 8
160100970526     C                   MOVEL     FUDRIG        FUD(I)
160200970526     C                   END
160300970526     C                   END                                                    DO WHILE
160400970526     C*
160500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
160600970526     C     W             DIV       6             I
160700970526     C                   MVR                     Z
160800970526     C*
160900970526     C     Z             IFNE      0
161000970526     C     I             ADD       1             Z
161100970526     C                   ELSE
161200970526     C                   Z-ADD     I             Z                 5 0
161300970526     C                   END
161400970526     C*
161500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
161600970526     C                   ADD       1             FUNGRU
161700970526     C     FUNGRU        IFGT      Z
161800970526     C     FUNGRU        ORLT      1
161900970526     C                   Z-ADD     1             FUNGRU
162000970526     C                   END
162100970526     C*
162200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
162300970526     C     FUNGRU        IFEQ      1
162400970526     C                   MOVEL     FUV(1)        FUN001
162500970526     C                   MOVEL     FUV(2)        FUN002
162600970526     C                   MOVEL     FUV(3)        FUN003
162700970526     C                   MOVEL     FUV(4)        FUN004
162800970526     C                   MOVEL     FUV(5)        FUN005
162900970526     C                   MOVEL     FUV(6)        FUN006
163000970526     C                   END
163100970526     C*
163200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
163300970526     C     FUNGRU        IFEQ      2
163400970526     C                   MOVEL     FUV(7)        FUN001
163500970526     C                   MOVEL     FUV(8)        FUN002
163600970526     C                   MOVEL     FUV(9)        FUN003
163700970526     C                   MOVEL     FUV(10)       FUN004
163800970526     C                   MOVEL     FUV(11)       FUN005
163900970526     C                   MOVEL     FUV(12)       FUN006
164000970526     C                   END
164100970526     C*
164200970526     C* TERZO GRUPPO DI 6 FUNZIONI
164300970526     C     FUNGRU        IFEQ      3
164400970526     C                   MOVEL     FUV(13)       FUN001
164500970526     C                   MOVEL     FUV(14)       FUN002
164600970526     C                   MOVEL     FUV(15)       FUN003
164700970526     C                   MOVEL     FUV(16)       FUN004
164800970526     C                   MOVEL     FUV(17)       FUN005
164900970526     C                   MOVEL     FUV(18)       FUN006
165000970526     C                   END
165100970526     C*
165200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
165300970526     C     FUNGRU        IFEQ      4
165400970526     C                   MOVEL     FUV(19)       FUN001
165500970526     C                   MOVEL     FUV(20)       FUN002
165600970526     C                   MOVEL     FUV(21)       FUN003
165700970526     C                   MOVEL     FUV(22)       FUN004
165800970526     C                   MOVEL     *BLANKS       FUN005
165900970526     C                   MOVEL     *BLANKS       FUN006
166000970526     C                   END
166100970526     C*
166200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
166300970526     C                   MOVEL     *BLANKS       FUN007
166400970526     C                   MOVEL     *BLANKS       FUN008
166500970526     C*
166600970526     C     1             DO        24            I
166700970526     C                   MOVEL     FUD(I)        FUDRIG
166800970526     C     FUDFUN        IFEQ      '24'
166900970526     C     FUDATT        ANDEQ     'S'
167000970526     C                   MOVEL     FUDDES        FUN008
167100970526     C                   END
167200970526     C                   END                                                    DO
167300970526     C*
167400970526     C                   ENDSR
167500970526     C*------------------------------------------------------------------------*
167600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
167700970526     C*------------------------------------------------------------------------*
167800970526     C     FUNCON        BEGSR
167900970526     C*
168000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
168100970526     C                   EXSR      FUNVAR
168200970526     C*
168300970526     C* PULISCO CAMPI DI LAVORO
168400970526     C                   Z-ADD     0             I                 5 0
168500970526     C                   Z-ADD     0             W                 5 0
168600970526     C*
168700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
168800970526     C     FUNKEY        IFNE      *BLANKS
168900970526     C     1             DO        24            I
169000970526     C                   MOVEL     FUD(I)        FUDRIG
169100970526     C*
169200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
169300970526     C     FUNKEY        IFEQ      FUDFUN
169400970526     C     FUDATT        ANDEQ     'S'
169500970526     C                   Z-ADD     I             W
169600970526     C                   END
169700970526     C                   END                                                    DO WHILE
169800970526     C*
169900970526     C* FUNZIONE NON ATTIVA (ERRORE)
170000970526     C     W             IFEQ      0
170100970526     C*
170200970526     C* EMISSIONE MESSAGGIO DI ERRORE
170300970526     C                   MOVEL     MSG(1)        DSMSMS
170400970526     C*
170500970526     C                   END                                                    W=0
170600970526     C                   END                                                    FUNKEY NE *BLANKS
170700970526     C*
170800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
170900970526     C     W             IFNE      0
171000970526     C     FUNKEY        OREQ      *BLANKS
171100970526     C     FUNKEY        IFEQ      *BLANKS
171200970526     C                   EXSR      ENTGES
171300970526     C                   END
171400970526     C     FUNKEY        IFEQ      '03'
171500970526     C                   EXSR      F03GES
171600970526     C                   END
171700970526     C     FUNKEY        IFEQ      '06'
171800970526     C                   EXSR      F06GES
171900970526     C                   END
172000970526     C     FUNKEY        IFEQ      '12'
172100970526     C                   EXSR      F12GES
172200970526     C                   END
172300970526     C     FUNKEY        IFEQ      '24'
172400970526     C                   EXSR      F24GES
172500970526     C                   END
172600970526     C                   END
172700970526     C*
172800970526     C                   ENDSR
172900970526     C*------------------------------------------------------------------------*
173000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
173100970526     C*------------------------------------------------------------------------*
173200970526     C     FUNVAR        BEGSR
173300970526     C*
173400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
173500970526     C*
173600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
173700970526     C     KEY           IFEQ      ENTER
173800970526     C                   MOVEL     *BLANKS       FUNKEY
173900970526     C                   END
174000970526     C     KEY           IFEQ      F1
174100970526     C                   MOVEL     '01'          FUNKEY
174200970526     C                   END
174300970526     C     KEY           IFEQ      F2
174400970526     C                   MOVEL     '02'          FUNKEY
174500970526     C                   END
174600970526     C     KEY           IFEQ      F3
174700970526     C                   MOVEL     '03'          FUNKEY
174800970526     C                   END
174900970526     C     KEY           IFEQ      F4
175000970526     C                   MOVEL     '04'          FUNKEY
175100970526     C                   END
175200970526     C     KEY           IFEQ      F5
175300970526     C                   MOVEL     '05'          FUNKEY
175400970526     C                   END
175500970526     C     KEY           IFEQ      F6
175600970526     C                   MOVEL     '06'          FUNKEY
175700970526     C                   END
175800970526     C     KEY           IFEQ      F7
175900970526     C                   MOVEL     '07'          FUNKEY
176000970526     C                   END
176100970526     C     KEY           IFEQ      F8
176200970526     C                   MOVEL     '08'          FUNKEY
176300970526     C                   END
176400970526     C     KEY           IFEQ      F9
176500970526     C                   MOVEL     '09'          FUNKEY
176600970526     C                   END
176700970526     C     KEY           IFEQ      F10
176800970526     C                   MOVEL     '10'          FUNKEY
176900970526     C                   END
177000970526     C     KEY           IFEQ      F11
177100970526     C                   MOVEL     '11'          FUNKEY
177200970526     C                   END
177300970526     C     KEY           IFEQ      F12
177400970526     C                   MOVEL     '12'          FUNKEY
177500970526     C                   END
177600970526     C     KEY           IFEQ      F13
177700970526     C                   MOVEL     '13'          FUNKEY
177800970526     C                   END
177900970526     C     KEY           IFEQ      F14
178000970526     C                   MOVEL     '14'          FUNKEY
178100970526     C                   END
178200970526     C     KEY           IFEQ      F15
178300970526     C                   MOVEL     '15'          FUNKEY
178400970526     C                   END
178500970526     C     KEY           IFEQ      F16
178600970526     C                   MOVEL     '16'          FUNKEY
178700970526     C                   END
178800970526     C     KEY           IFEQ      F17
178900970526     C                   MOVEL     '17'          FUNKEY
179000970526     C                   END
179100970526     C     KEY           IFEQ      F18
179200970526     C                   MOVEL     '18'          FUNKEY
179300970526     C                   END
179400970526     C     KEY           IFEQ      F19
179500970526     C                   MOVEL     '19'          FUNKEY
179600970526     C                   END
179700970526     C     KEY           IFEQ      F20
179800970526     C                   MOVEL     '20'          FUNKEY
179900970526     C                   END
180000970526     C     KEY           IFEQ      F21
180100970526     C                   MOVEL     '21'          FUNKEY
180200970526     C                   END
180300970526     C     KEY           IFEQ      F22
180400970526     C                   MOVEL     '22'          FUNKEY
180500970526     C                   END
180600970526     C     KEY           IFEQ      F23
180700970526     C                   MOVEL     '23'          FUNKEY
180800970526     C                   END
180900970526     C     KEY           IFEQ      F24
181000970526     C                   MOVEL     '24'          FUNKEY
181100970526     C                   END
181200970526     C     KEY           IFEQ      ROLLUP
181300970526     C                   MOVEL     'RU'          FUNKEY
181400970526     C                   END
181500970526     C     KEY           IFEQ      HELP
181600970526     C                   MOVEL     'HP'          FUNKEY
181700970526     C                   END
181800970526     C                   ENDSR
181900970526     C*------------------------------------------------------------------------*
182000970526     C* NOTE COSTRUZIONE SCHIERE
182100970526     C*------------------------------------------------------------------------*
182200970526     C*
182300970526     C* -MSG-MESSAGGI
182400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
182500970526     C* (NON TOCCARE MSG,1 E MSG,2)
182600970526     C*
182700970526     C* -CMD-COMANDI
182800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
182900970526     C*
183000970526     C* -OPD-OPZIONI
183100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
183200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
183300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
183400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
183500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
183600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
183700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
183800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
183900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
184000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
184100970526     C*
184200970526     C* -FUD-FUNZIONI
184300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
184400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
184500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
184600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
184700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
184800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
184900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
185000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
185100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
185200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
185300970526     O*------------------------------------------------------------------------*
185400970526** DEC - DECODIFICHE
185500970526    MODIFICA                                                      1
185600970526  ANNULLAMENTO                                                    2
185700970526VISUALIZZAZIONE                                                   3
185800970526   RIPRISTINO                                                     4
185900970526  INSERIMENTO                                                     5
186000991014     COPIA                                                        6
186100970526** MSG - MESSAGGI
186200991011TIBS13R- Tasto funzionale non gestito                                          1
186300991011TIBS13R- Inserimento effettuato                                                2
186400991011TIBS13R- Modifica effettuata                                                   3
186500991011TIBS13R- Annullamento effettuato                                               4
186600991011TIBS13R- Ripristino effettuato                                                 5
186700991011TIBS13R- Richiesta funzione per record inesistente                             6
186800991011TIBS13R- Record momentaneamente allocato: riprovare più tardi                  7
186900991011TIBS13R- Richiesta funzione per record esistente                               8
187000000307TIBS13R- Codice figlio già presente come codice padre con questo legame        9
187100010228TIBS13R- Codice figlio già associato nel periodo ad un'altro padre             10
187200991011TIBS13R- Valore inserito errato i valori ammessi sono(S=Sosp A=Ann Blank)      11
187300991011TIBS13R- Tipo legame errato verificarne sulla tabella l'esistenza              12
187400991011TIBS13R- Codice padre errato                                                   13
187500991011TIBS13R- Data errata                                                           14
187600991011TIBS13R- La data scadenza deve essere maggiore della decorrenza                15
187700991011TIBS13R- Il record non può scadere in un giorno già trascorso                  16
187800991011TIBS13R- Il record immesso si sovrappone ad uno già in decorrenza              17
187900991011TIBS13R- Codice figlio errato                                                  18
188000000211TIBS13R- Codice padre già presente come codice figlio con questo legame        19
188100991014TIBS13R- Il codice figlio non può essere uguale al codice padre                20
188200000211TIBS13R- Codice padre obbligatorio                                             21
188300000807TIBS13R- Codice figlio obbligatorio                                            22
188400000807TIBS13R- Tipo legame non autorizzato.                                          23
188500060113TIBS13R- Utente non abilitato alla gestione dei clienti scelti.                24
188600000808TIBS13R- Richiesta inserimento per dati già esistenti, possibile solo modifica 25
188700020211TIBS13R- Codice figlio già presente nelle abilitazioni.                        26
188800060113TIBS13R- Errore in caricamento dati utente, avvisare il CED                    27
188900090806TIBS13R- Codice unificante risulta già padre su legame di tipo analogo.        28
189000150505TIBS13R- Non ammessa una Data Decorrenza antecedente la data Odierna.          29
189100160607TIBS13R- Legame LT Cod.padre deve essere cliente corriere il figlio logistica  30
189200160607TIBS13R- Legame ST Cod.padre e figlio devono appartenere allo stesso Network   31
189300170906TIBS13R- Utente non abilitato alla gestione Logistica                          32
189400970526** FUD - FUNZIONI
189500970526                   -        (S/N)=N                        funz.01¬
189600970526                   -        (S/N)=N                        funz.02¬
189700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
189800970526                   -        (S/N)=N                        funz.04¬
189900970526                   -        (S/N)=N                        funz.05¬
190000970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
190100970527                   -        (S/N)=N                        funz.07¬
190200970527                   -        (S/N)=N                        funz.08¬
190300970527                   -        (S/N)=N                        funz.09¬
190400970527                   -        (S/N)=N                        funz.10¬
190500970526                   -        (S/N)=N                        funz.11¬
190600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
190700970526                   -        (S/N)=N                        funz.13¬
190800970526                   -        (S/N)=N                        funz.14¬
190900970526                   -        (S/N)=N                        funz.15¬
191000970526                   -        (S/N)=N                        funz.16¬
191100970526                   -        (S/N)=N                        funz.17¬
191200970526                   -        (S/N)=N                        funz.18¬
191300970526                   -        (S/N)=N                        funz.19¬
191400970526                   -        (S/N)=N                        funz.20¬
191500970526                   -        (S/N)=N                        funz.21¬
191600970526                   -        (S/N)=N                        funz.22¬
191700970526                   -        (S/N)=N                        funz.23¬
191800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
