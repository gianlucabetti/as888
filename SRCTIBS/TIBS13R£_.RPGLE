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
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------
002000970526     D* DECODIFICHE
002100970526     D*------------
002200991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* MESSAGGI
002500970526     D*------------
002600090806     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
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
006000020212     D*------------
006100020212     D* DS X LETTURA NUOVA TABELLA ISV
006200020212     D*------------
006300020212     D DSISV         E DS                  EXTNAME(DISV)
006400970528     D*------------------
006500970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600970528     D*------------------
006700970528     D WLBDA8          DS                  INZ
006800970528     D  G08DAT                 1      8  0
006900970528     D  G08INV                 9     16  0
007000970528     D  G08ERR                17     17
007100970528     D  G08TGI                18     22  0
007200970526     D*------------
007300970526     D* DS DI PROCEDURA
007400970526     D*------------
007500991014     D TIBS02DS      E DS
007600991008     D tibds         E DS                  EXTNAME(tibs12ds)
007700991014     D dtle          E DS
007800970526     D*------------
007900970526     D* ARCHITETTURA
008000970526     D*------------
008100970526     D KPJBA         E DS
008200020212     D*------------
008300000807     D* TABELLE "XCLIR"- RICERCA CLIENTI
008400020212     D*------------
008500000807     D ksc             s              7  0 dim(30)
008600000807     D ksa             s              4    dim(30)
008700020212     D isvkun          s              2    dim(100)
008800000807     D*------------------
008900000807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
009000000807     D*------------------
009100000807     DDSXC           e DS                  extname(XCLIRDS)
009200000807     D*---
009300000807     D                 DS
009400000807     D dsksn                   1      4p 0
009500000807     D dsksa                   1      4
009600000807     DBS69DS         e ds                  extname(TIBS69DS)
009700000807     DACODS          e ds                  extname(cnaco00f)
009800000807     DINDDS          e ds                  extname(cnind00f)
009900000807     DCLPDS          e ds                  extname(cnclp00f)
010000000807     DCLSDS          e ds                  extname(fncls00f)
010100000307     D*------------
010200000307     D* SCOMPOSIZIONE CODICI CLIENTI IN FORMATO "STRATEGI"
010300000307     D*------------
010400000307     D                 DS
010500000307     D  FILCLI                 5      7  0
010600000307     D  STRATECLI              1     11
010700000807     D*------------------
010800000807     D* VARIABILI DI WRK
010900000807     D*-------------------
011000000807     D tipprf          s              3    inz(*blanks)
011100091116     D wDATA_ISO       S               D   DATFMT(*ISO) INZ(D'1999-01-01')
011200020905     D*------------
011300020905     D* DS REPERIMENTO DATI UTENTE
011400020905     D*------------
011500130412     d Status         sds
011600130412     d  SDSpgm           *proc
011700130412     d  SDSprm           *parms
011800130412     d  SDSuser              254    263
011900020905     D TIBS34DS      E DS
012000020905     D DDATIUTE      E DS
012100020905     D AZUTEDS       E DS                  extname(AZUTE00F)
012200060113     d dute01        e ds
012300060113     d trul31ds      e ds
012400020905     D*------------
012500020905     D* SCHIERA P.O. IN GESTOINE ALL'UTENTE
012600020905     D*------------
012700060113     D skPOG           S              3    DIM(250) INZ(*zeros)
012800090806     D*------------------
012900090806     D* VARIABILI DI WRK
013000090806     D*------------
013100090806     D skTIPTLEWW      S              2    DIM(100) INZ(*blanks)
013200090806     D skTIPTLEST      S              2    DIM(100) INZ(*blanks)
013300090806     D jskTIPTLEWW     S              3  0 INZ(*zeros)
013400090806     D jskTIPTLEST     S              3  0 INZ(*zeros)
013500970526     C*------------------------------------------------------------------------*
013600970526     C* MAIN LINES
013700970526     C*------------------------------------------------------------------------*
013800970526     C*
013900970526     C* NON CI SONO ERRORI IN ENTRATA
014000970526IF  1C     ERRL00        IFEQ      '0'
014100970526     C*
014200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014600970526     C*
014700970526     C* EMETTE FORMATO VIDEO
014800970526     C                   EXSR      BUTFMR
014900970526     C*
015000970526     C* GESTIONE TASTI FUNZIONALI
015100970526     C                   EXSR      FUNCON
015200970526E   2C                   ENDDO
015300970526     C*
015400970526     C* CI SONO ERRORI IN ENTRATA
015500970527X   1C                   ELSE
015600991008     C                   MOVEL     '1'           k12err
015700991008     C                   MOVEL     DSMSMS        k12msg
015800970526E   1C                   ENDIF
015900970526     C* FINE PGM
016000991210     c                   exsr      finsr
016100970526     C                   SETON                                        LR
016200970526     C*------------------------------------------------------------------------*
016300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016400970526     C*------------------------------------------------------------------------*
016500970526     C     BUTFMR        BEGSR
016600970526     C*
016700970526     C* EMETTE LA TESTATA
016800991011     C                   WRITE     bs13D01
016900970526     C*
017000970526     C* SPEDISCE EVENTUALE MESSAGGIO
017100970526     C                   EXSR      SNDMSG
017200970526     C*
017300970526     C* EMETTE LE FUNZIONI
017400991011     C                   WRITE     bs13D02
017500970526     C*
017600970526     C* EMETTE IL VIDEO
017700991011     C                   EXFMT     bs13D03
017800970526     C*
017900970526     C* REIMPOSTA FLAG DI ERRORE
018000970526     C                   MOVEL     '0'           ERRL00
018100970526     C*
018200970526     C* REIMPOSTA INDICATORI DI ERRORE
018300000307     C                   SETOFF                                         5859
018400970526     C                   SETOFF                                       606162
018500970526     C                   SETOFF                                       636465
018600970526     C                   SETOFF                                       666768
018700970526     C                   SETOFF                                       697071
018800970526     C                   SETOFF                                       727374
018900970526     C                   SETOFF                                       757677
019000970526     C                   SETOFF                                       787980
019100970526     C                   SETOFF                                       818283
019200970526     C                   SETOFF                                       848586
019300970526     C                   SETOFF                                       878889
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     SNDMSG        BEGSR
020000970526     C*
020100970526     C* PULISCE IL SFILE MESSAGGI
020200970526     C                   MOVEL     '1'           *IN40
020300991011     C                   WRITE     bs13DMC
020400970526     C                   MOVEL     '0'           *IN40
020500970526     C*
020600970526     C* VISUALIZZA IL MESSAGGIO
020700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
020800970526     C                   CALL      'XMSG'        XMSGPA
020900970526IF  1C     DSMSER        IFEQ      *ZEROS
021000970526     C                   MOVEL     DSMSKE        MSGKEY
021100970526E   1C                   ENDIF
021200970526     C                   Z-ADD     1             MSGULT
021300991011     C                   WRITE     bs13DMS
021400991011     C                   WRITE     bs13DMC
021500970526     C*
021600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
021700970526     C                   MOVEL     *BLANKS       DSMSMS
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* ENTGES - GESTIONE ENTER -> INVIO
022200970526     C*------------------------------------------------------------------------*
022300970526     C     ENTGES        BEGSR
022400970526     C*
022500970526     C* CONTROLLA I CAMPI IMMESSI
022600021024IF  1C     DEPOP1        IFNE      'O04'
022700970526     C                   EXSR      DECODI
022800021024E   1C                   ENDIF
022900970526     C*
023000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
023100970526IF  1C     DEPOP1        IFEQ      'O05'
023200970526     C                   MOVEL     '1'           FINL00
023300970526E   1C                   ENDIF
023400970526     C*
023500970526     C                   ENDSR
023600970526     C*------------------------------------------------------------------------*
023700970526     C* F03GES - GESTIONE F03 -> FINE
023800970526     C*------------------------------------------------------------------------*
023900970526     C     F03GES        BEGSR
024000970526     C*
024100970526     C* IMPOSTA "PREMUTO F03"
024200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
024300991008     C                   MOVEL     '1'           K12F03                         *DI PROCEDURA
024400970526     C*
024500970526     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
024800970526     C*------------------------------------------------------------------------*
024900970526     C     F06GES        BEGSR
025000970526     C*
025100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
025200970526IF  1C     DEPOP1        IFEQ      'F06'
025300970526     C                   EXSR      INSERI
025400970526E   1C                   ENDIF
025500970526IF  1C     DEPOP1        IFEQ      'O02'
025600970526     C                   EXSR      MODIFI
025700970526E   1C                   ENDIF
025800991014IF  1C     DEPOP1        IFEQ      'O03'
025900000307     C                   EXSR      COPIA
026000991014E   1C                   ENDIF
026100000307IF  1C     DEPOP1        IFEQ      'O04'
026200000307     C                   EXSR      ANNULL
026300000307E   1C                   ENDIF
026400970526IF  1C     DEPOP1        IFEQ      'O07'
026500970526     C                   EXSR      RIPRIS
026600970526E   1C                   ENDIF
026700970526     C*
026800970526     C                   ENDSR
026900970526     C*------------------------------------------------------------------------*
027000970526     C* F12GES - GESTIONE F12 -> RITORNO
027100970526     C*------------------------------------------------------------------------*
027200970526     C     F12GES        BEGSR
027300970526     C*
027400970526     C* IMPOSTA "PREMUTO F12"
027500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
027600991008     C                   MOVEL     '1'           k12F12                         *DI PROCEDURA
027700970526     C*
027800970526     C                   ENDSR
027900970526     C*------------------------------------------------------------------------*
028000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028100970526     C*------------------------------------------------------------------------*
028200970526     C     F24GES        BEGSR
028300970526     C*
028400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028500970526     C*
028600970526     C                   ENDSR
028700970526     C*------------------------------------------------------------------------*
028800970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
028900970526     C*------------------------------------------------------------------------*
029000970526     C     INSERI        BEGSR
029100970526     C*
029200970526     C* CONTROLLA I CAMPI IMMESSI
029300970526     C                   EXSR      DECODI
029400970526     C*
029500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
029600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029700991014     C                   movel     VIDTle        kuntle
029800991008     C                   MOVEL     VIDazp        kunazp                         *CHIAVE
029900991008     C                   z-add     copVID        kuncop
030000991011     C                   MOVEL     vidazf        kunazf
030100991011     C                   EXSR      IMPkun                                       *ALTRI CAMPI
030200991008     C                   Z-ADD     *ZEROS        kunDTR                         *DATA TRASMISSIONE
030300991008     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
030400130412     C                   EVAL      KUNDIM = %dec(%date())
030500130412     C                   EVAL      KUNDUV = %dec(%date())
030600130412     C                   EVAL      KUNPUV = SDSUser
030700991008     C                   WRITE     tikun000
030800991008     C                   MOVEL     MSG(2)        k12MSG                         *MESSAGGIO
030900970602     C*
031000970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
031100991108     C                   move      *ZEROS        VIDcof
031200991108     C                   move      *blank        vddcof
031300970526E   1C                   ENDIF
031400970526     C*
031500970526     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* MODIFI - OPERAZIONI DI MODIFICA
031800970526     C*------------------------------------------------------------------------*
031900970526     C     MODIFI        BEGSR
032000970526     C*
032100970526     C* CONTROLLA I CAMPI IMMESSI
032200970526     C                   EXSR      DECODI
032300970526     C*
032400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032600991011     C                   EXSR      LETkun
032700970526     C*
032800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
032900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033000991011     C                   EXSR      IMPkun                                       *IMPOSTA I CAMPI
033100991008     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
033200130412     C                   EVAL      KUNDUV = %dec(%date())
033300130412     C                   EVAL      KUNPUV = SDSUser
033400991008     C                   UPDATE    tikun000
033500991008     C                   MOVEL     MSG(3)        k12MSG
033600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033700970526E   2C                   ENDIF
033800970526E   1C                   ENDIF
033900970526     C*
034000970526     C                   ENDSR
034100991014     C*------------------------------------------------------------------------*
034200000307     C* COPIA  - OPERAZIONI DI COPIA
034300991014     C*------------------------------------------------------------------------*
034400000307     C     COPIA         BEGSR
034500991014     C*
034600991014     C* CONTROLLA I CAMPI IMMESSI
034700991014     C                   EXSR      DECODI
034800991014     C*
034900991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
035000991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035100991014     C                   movel     VIDTle        kuntle
035200991014     C                   MOVEL     VIDazp        kunazp                         *CHIAVE
035300991014     C                   z-add     copVID        kuncop
035400991014     C                   MOVEL     vidazf        kunazf
035500991014     C                   EXSR      IMPkun                                       *IMPOSTA I CAMPI
035600991014     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
035700991111     C                   Z-ADD     *ZEROS        kunDTR                         *DATA TRASMISSIONE
035800130412     C                   EVAL      KUNDIM = %dec(%date())
035900130412     C                   EVAL      KUNDUV = %dec(%date())
036000130412     C                   EVAL      KUNPUV = SDSUser
036100991014     C                   write     tikun000
036200991014     C                   MOVEL     MSG(3)        k12MSG
036300991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
036400991014E   2C                   ENDIF
036500991014     C*
036600991014     C                   ENDSR
036700970526     C*------------------------------------------------------------------------*
036800970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
036900970526     C*------------------------------------------------------------------------*
037000000307     C     ANNULL        BEGSR
037100000307     C*
037200000307     C* CONTROLLA I CAMPI IMMESSI
037300021024     C                   EXSR      DECODI
037400970526     C*
037500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
037600000307     C                   EXSR      LETkun
037700970527     C*
037800970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
037900000307IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038000000307IF  2C     kunvat        IFEQ      *BLANKS                                      *NO ANNULLATI
038100130416IF  3C     kunDUV        IFGT      *ZEROS                                       *GIA' TRASMESSO
038200091116     C     *ISO          MOVE      datcor        wDATA_ISO
038300091116     C     wDATA_ISO     SUBDUR    1:*D          wDATA_ISO
038400091116     C     *ISO          MOVE      wDATA_ISO     kundsc
038500000307     C                   MOVEL     'A'           kunvat                         *FLAG ANNULLAMENTO
038600000307     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
038700130412     C                   EVAL      KUNDUV = %dec(%date())
038800130412     C                   EVAL      KUNPUV = SDSUser
038900000307     C                   UPDATE    tikun000                                     *ANNULLA
039000000307X   3C                   ELSE                                                   *MAI TRASMESSO
039100000307     C                   DELETE    tikun000                                     *CANCELLA
039200000307E   3C                   ENDIF
039300000307E   2C                   ENDIF
039400000307     C                   MOVEL     MSG(4)        k12MSG                         *MESSAGGIO
039500000307     C                   MOVEL     '1'           FINL00                         *FINE PGM
039600000307E   1C                   ENDIF
039700970526     C*
039800000307     C                   ENDSR
039900970526     C*------------------------------------------------------------------------*
040000970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
040100970526     C*------------------------------------------------------------------------*
040200000807     C     RIPRIS        BEGSR
040300970526     C*
040400970526     C* CONTROLLA I CAMPI IMMESSI
040500970526     C                   EXSR      DECODI
040600970526     C*
040700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
040800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040900991011     C                   EXSR      LETkun
041000970526     C*
041100970526     C* IMPOSTA IL RECORD DA SCRIVERE
041200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041300991008     C                   MOVEL     *BLANKS       kunvat                         *FLAG ANNULLAMENTO
041400991008     C                   MOVEL     *BLANKS       kunFTR                         *FLAG TRASMISSIONE
041500130412     C                   EVAL      KUNDUV = %dec(%date())
041600130412     C                   EVAL      KUNPUV = SDSUser
041700991008     C                   UPDATE    tikun000                                     *ANNULLA
041800970526E   2C                   ENDIF
041900991008     C                   MOVEL     MSG(5)        k12MSG                         *MESSAGGIO
042000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
042100970526E   1C                   ENDIF
042200970526     C*
042300970526     C                   ENDSR
042400000807     C*------------------------------------------------------------------------*
042500000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
042600000807     C*------------------------------------------------------------------------*
042700000807     C     CHKAUT        BEGSR
042800000807     C*
042900000807     C* - controllo su "operabilità" di TIPO LEGAME -
043000000807     C                   eval      tipprf =  %subst(%trim(KNMUS):1:3)
043100060117     C     DUTLPO        ifeq      'S'
043200000807     C* X SEDE
043300060117     C     ERRL00        ifeq      '0'
043400060117     C                   movel     'TLE'         KTBcod
043500060117     C                   movel(p)  vidtle        KTBky1
043600060117     C     KEYTBE        chain     tntbe01l                           70
043700060117     C     *in70         ifeq      *off
043800060117     C     tbeatb        andeq     *blanks
043900060117     C                   movel     tbeuni        dtle
044000060117     C                   if        §tleaus = 'S' or
044100060117     C                             (tipprf = 'EDP' and §tleaue = 'S')
044200060117     C                   else
044300060117     C                   SETON                                        66
044400060117     C                   MOVEL     '1'           ERRL00
044500060117     C                   MOVEL     MSG(23)       DSMSMS
044600060117     C                   endif
044700060117     C                   else
044800060117     C                   MOVEL     '1'           ERRL00
044900060117     C                   MOVEL     MSG(12)       DSMSMS
045000060117     C                   endif
045100060117     C                   endif
045200060117     C                   else
045300000807     C* X FILIALE
045400000807     C     ERRL00        ifeq      '0'
045500000807     C                   movel     'TLE'         KTBcod
045600000807     C                   movel(P)  vidtle        KTBky1
045700000807     C     KEYTBE        chain     tntbe01l                           70
045800000807     C     *in70         ifeq      *off
045900000807     C     tbeatb        andeq     *blanks
046000000807     C                   movel     tbeuni        dtle
046100000807     C                   if        §tleaut = 'S' or
046200000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
046300000807     C                   else
046400000810     C                   SETON                                        66
046500000807     C                   MOVEL     '1'           ERRL00
046600000807     C                   MOVEL     MSG(23)       DSMSMS
046700000807     C                   endif
046800000807     C                   else
046900000807     C                   MOVEL     '1'           ERRL00
047000000807     C                   MOVEL     MSG(12)       DSMSMS
047100000807     C                   endif
047200000807     C                   endif
047300060117     C                   endif
047400000807     C*
047500000807     C*
047600000807     C     ERRL00        ifeq      '0'
047700001018     C     tipprf        andne     'EDP'
047800000807     C* - controllo su "operabilità" di FILIALE (1° o 2° livello)
047900000807     C                   setoff                                       11
048000000807     C* CONTROLLO CLIENTE "PADRE"
048100000807     C     vidcop        ifne      *zeros
048200000807     C                   move(p)   vidcop        STRATECLI
048300060113     c                   move      filcli        w003a             3
048400060113     c     w003a         lookup    skpog                                  99
048500000807     C     *IN99         ifeq      '0'
048600000807     C                   seton                                        11
048700000807     C                   endif
048800000807     C                   endif
048900000807     C*
049000000807     C                   setoff                                       12
049100000807     C*
049200000807     C* CONSIDERAZIONI FINALI DEL CONTROLLO
049300000807     C     *in11         ifeq      *on
049400000830     C                   seton                                        61
049500000830     C                   movel     '1'           ERRL00
049600000830     C                   movel     MSG(24)       DSMSMS
049700000830     C                   endif
049800000830     C                   endif
049900000807     C*
050000000807     C                   ENDSR
050100970526     C*------------------------------------------------------------------------*
050200991011     C* LETkun - LEGGE IL RECORD PERL'AGGIORNAMENTO
050300970526     C*------------------------------------------------------------------------*
050400991011     C     LETkun        BEGSR
050500970526     C*
050600000808     C                   MOVEL     vidtle        KCEtle
050700000808     C                   MOVEL     vidazp        KCEazp
050800000808     C                   MOVEL     vidazf        KCEazf
050900000808     C                   MOVE(p)   vidcop        KCEcop
051000000808     C                   MOVE(p)   vidcof        KCEcof
051100000808     C                   Z-ADD     ddevid        KCEdde
051200991008     C     KEYkun        CHAIN     tikun01L                           9998
051300000904     C                   IF        DEPOP1 = 'F06' OR
051400000904     C                             DEPOP1 = 'O03'
051500000808IF  1C     *IN99         IFEQ      *OFF                                         *GIA' PRESENTE
051600000808     C                   MOVEL     '1'           ERRL00                         *ERRORE
051700000808E   1C                   ENDIF
051800000808     C                   ELSE
051900970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
052000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
052100991008     C                   MOVEL     MSG(7)        k12MSG                         *MESSAGGIO
052200000808     C                   MOVEL     '1'           FINL00                         *FINE PGM
052300970526E   1C                   ENDIF
052400970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
052500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
052600991008     C                   MOVEL     MSG(6)        k12MSG                         *MESSAGGIO
052700000808     C                   MOVEL     '1'           FINL00                         *FINE PGM
052800970526E   1C                   ENDIF
052900000808     C                   ENDIF
053000970526     C*
053100970526     C                   ENDSR
053200970526     C*------------------------------------------------------------------------*
053300970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
053400970526     C*------------------------------------------------------------------------*
053500970526     C     DECODI        BEGSR
053600020211     C*
053700000904     C*--------------------
053800000904     C* DATA DECORRENZA
053900000904     C*--------------------
054000000904     C     *LIKE         DEFINE    VIDDDE        DDEVID
054100000904IF  1C     ERRL00        IFEQ      '0'
054200000904     C                   Z-ADD     VIDDDE        wda8
054300000904     C                   Z-ADD     *ZEROS        da8w
054400000904     C                   EXSR      CHKDA8
054500000904     C                   Z-ADD     WDA8          viddde
054600000904     C                   Z-ADD     DA8W          ddevid
054700000904IF  2C     ERRL00        IFEQ      '1'
054800000904     C                   SETON                                        63
054900000904     C                   MOVEL     MSG(14)       dsmsms
055000000904E   2C                   ENDIF
055100150505      *
055200150505      * non è ammesso solo x "CA"
055300150505      * inserire una data decorrenza antecedente alla UDATE
055400150505     c                   if        vidTLE ='CA' and
055500150505     c                             ddevid < %dec(%date())
055600150505     c                   if        DEPOP1 = 'F06' or
055700150505     c                             DEPOP1 = 'O03'
055800150505     C                   SETON                                        63
055900150505     C                   MOVEL     MSG(29)       dsmsms
056000150505     c                   end
056100150505     c                   end
056200150505      *
056300000904E   1C                   ENDIF
056400000904     C*--------------------
056500000904     C* DATA SCADENZA
056600000904     C*--------------------
056700000904     C     *LIKE         DEFINE    VIDDSC        DSCVID
056800000904IF  1C     ERRL00        IFEQ      '0'
056900000904     C                   Z-ADD     VIDDSC        WDA8
057000000904     C                   Z-ADD     *ZEROS        DA8W
057100000904     C                   EXSR      CHKDA8
057200000904     C                   Z-ADD     WDA8          VIDDSC
057300000904     C                   Z-ADD     DA8W          DSCVID
057400000904IF  2C     ERRL00        IFEQ      '1'
057500000904     C                   SETON                                        64
057600000904     C                   MOVEL     MSG(14)       DSMSMS
057700000904E   2C                   ENDIF
057800000904E   1C                   ENDIF
057900000308     C*
058000000308     C* IN CASO DI RICHIESTA DI ANNULLAMENTO BY-PASSO I CONTROLLI DI VALIDITA'
058100000308     C     DEPOP1        IFNE      'O04'                                        *ANNULLAMENTO
058200000811     C     DEPOP1        ANDNE     'O05'                                        *VISUALIZZAZIONE
058300021024     C     DEPOP1        ANDNE     'O07'                                        *RIPRISTINO
058400991008     C*--------------------
058500991008     C* tipo legame
058600991008     C*--------------------
058700991008IF  1C     ERRL00        IFEQ      '0'
058800000808     C                   move      *blank        wtlecu
058900000808     C                   move      *blank        wtlece
059000991008IF  2C     VIDtle        IFne      *blank
059100000322     C                   exsr      cnttle
059200991008X   2C                   ELSE
059300991008     C                   MOVEL     '1'           ERRL00
059400991011     C                   SETON                                        66
059500991011     C                   MOVEL     MSG(12)       DSMSMS
059600991008E   2C                   ENDIF
059700991008E   1C                   ENDIF
059800000904     C*--------------------
059900000904     C* codice padre
060000000323     C*--------------------
060100011206IF  1C     ERRL00        IFEQ      '0'
060200011206     C                   MOVEL     VIDcop        chkPot            4 0
060300011206IF  1C                   IF        chkPot > *zeros
060400011206     C                   SETON                                        61
060500011206     C                   MOVEL     MSG(13)       dsmsms
060600011206     C                   MOVE      '1'           errl00
060700011206     C                   MOVEL     *ZEROS        copVID
060800011206E   1C                   ENDIF
060900011206E   1C                   ENDIF
061000011206     C*
061100000323IF  1C     ERRL00        IFEQ      '0'
061200000808     C     *LIKE         DEFINE    kuncop        copVID
061300000323     C                   MOVEL     *BLANKS       VDDcop
061400000323IF  2C     VIDcop        IFEQ      *blanks                                      *STANDARD
061500000323IF  2C     VIDcop        oreq      *zeros                                       *STANDARD
061600000323     C                   SETON                                        61
061700000323     C                   MOVEL     MSG(21)       dsmsms
061800000323     C                   MOVE      '1'           errl00
061900000323     C                   MOVEL     *ZEROS        copVID
062000000323X   2C                   ELSE
062100000323     C                   move      wtlecu        clipot            1
062200000323     C                   MOVEL     VIDcop        Wcop
062300000323     C                   MOVEL     *blank        Wdcop
062400000323     C                   EXSR      CHKcop
062500000323     C                   MOVEL     Wcop          VIDcop
062600000323     C                   MOVEL     WDcop         VDDcop
062700000323     C                   MOVEL     VIDcop        copVID
062800000323IF  3C     ERRL00        IFEQ      '1'
062900000323     C                   SETON                                        61
063000000323     C                   MOVEL     MSG(13)       DSMSMS
063100000323E   3C                   ENDIF
063200000323E   2C                   ENDIF
063300000323E   1C                   ENDIF
063400000904     c*
063500000904     c* Verifico che il codice "padre" non sia un già un "figlio"
063600150505     c*  tranne x il legame è "CA"
063700000323IF  1C     ERRL00        IFEQ      '0'
063800150505     c     vidtle        andne     'CA'
063900000904     C                   move      copvid        kpar3
064000000904     c     keypar        setll     tikun02l
064100000904     c     keypar        reade(n)  tikun02l                               99
064200000904     c                   dow       not %eof
064300000904     c                   if        not *in99             AND
064400000904     c                             ((ddevid > xkundsc    AND
064500000904     c                               dscvid > xkundsc)   OR
064600000904     c                              (ddevid < xkundde    AND
064700000904     c                               dscvid < xkundde))
064800000904     c                   else
064900000904     c                   if        xkunvat = *blanks
065000000904     C                   SETON                                        61
065100000904     C                   MOVEL     MSG(19)       DSMSMS
065200000904     C                   MOVE      '1'           errl00
065300000904     c                   endif
065400000904     c                   endif
065500000904     c     keypar        reade(n)  tikun02l                               99
065600000904     c                   enddo
065700000808     C                   ENDIF
065800000323     C*--------------------
065900000323     C* codice figlio
066000000323     C*--------------------
066100011206IF  1C     ERRL00        IFEQ      '0'
066200011206     C                   MOVEL     VIDcof        chkPot            4 0
066300011206IF  1C                   IF        chkPot > *zeros
066400011206     C                   SETON                                        61
066500011206     C                   MOVEL     MSG(18)       dsmsms
066600011206     C                   MOVE      '1'           errl00
066700011206     C                   MOVEL     *ZEROS        cofVID
066800011206E   1C                   ENDIF
066900011206E   1C                   ENDIF
067000011206     C*
067100000323IF  1C     ERRL00        IFEQ      '0'
067200000808     C     *LIKE         DEFINE    kuncof        cofVID
067300000323     C                   MOVEL     *BLANKS       VDDcof
067400000323IF  2C     VIDcof        IFEQ      *blanks                                      *STANDARD
067500000323IF  2C     VIDcof        oreq      *zeros                                       *STANDARD
067600000323     C                   SETON                                        65
067700000323     C                   MOVEL     MSG(22)       dsmsms
067800000323     C                   MOVE      '1'           errl00
067900000323     C                   MOVEL     *ZEROS        cofVID
068000000323X   2C                   ELSE
068100000323     c                   movel     wtlece        clipot
068200000323     C                   MOVEL     VIDcof        Wcop
068300000323     C                   MOVEL     *blank        Wdcop
068400000323     C                   EXSR      CHKcop
068500000323     C                   MOVEL     Wcop          VIDcof
068600000323     C                   MOVEL     WDcop         VDDcof
068700000323     C                   MOVEL     VIDcof        cofVID
068800000323IF  3C     ERRL00        IFEQ      '1'
068900000323     C                   SETON                                        65
069000000323     C                   MOVEL     MSG(18)       dsmsms
069100000323E   3C                   ENDIF
069200000323E   2C                   ENDIF
069300000323E   1C                   ENDIF
069400000323     C     errl00        ifeq      '0'
069500150505      *** chiodo x "CA" deve essere possibile inserire lo stesso codice
069600000323     C     vidcop        ifeq      vidcof
069700150505     C     vidtle        andNE     'CA'
069800000323     C                   seton                                        65
069900000323     C                   MOVEL     '1'           errl00
070000000323     C                   MOVEL     MSG(20)       dsmsms
070100000323     C                   endif
070200000323     C                   endif
070300000904     c*
070400000904     c* Verifico che il codice "figlio" non sia già un "padre"
070500150505     c*  tranne x il legame è "CA"
070600000323IF  1C     ERRL00        IFEQ      '0'
070700150505     c     vidtle        andne     'CA'
070800000323     C                   move      cofvid        kpar3
070900000808     C     keypar        setll     tikun01l
071000000808     C     keypar        reade(n)  tikun01l                               99
071100000808     C                   dow       not %eof
071200000904     c                   if        not *in99             AND
071300000904     c                             ((ddevid > kundsc    AND
071400000904     c                               dscvid > kundsc)   OR
071500000904     c                              (ddevid < kundde    AND
071600000904     c                               dscvid < kundde))
071700000904     c                   else
071800000904     c                   if        kunvat = *blanks
071900000904     C                   SETON                                        65
072000000904     C                   MOVEL     MSG(9)        DSMSMS
072100000904     C                   MOVE      '1'           errl00
072200000904     c                   endif
072300000904     c                   endif
072400000808     C     keypar        reade(n)  tikun01l                               99
072500000808     C                   enddo
072600000808     C                   ENDIF
072700000904     c*
072800000904     c* Verifico che il codice "figlio" non sia un già "figlio" di un diverso "padre"
072900000808IF  1C     ERRL00        IFEQ      '0'
073000000904     C                   move      cofvid        kpar3
073100000808     C     keypar        setll     tikun02l
073200000901     C     keypar        reade(n)  tikun02l                               99
073300000808     C                   dow       not %eof
073400000904     c                   if        not *in99             AND
073500000904     c                             ((ddevid > xkundsc    AND
073600000904     c                               dscvid > xkundsc)   OR
073700000904     c                              (ddevid < xkundde    AND
073800000904     c                               dscvid < xkundde))
073900000904     c                   else
074000000904     c                   if        xkunvat =  *blanks AND
074100000904     c                             xkuncop <> copvid
074200000904     C                   SETON                                        65
074300010228     C                   EVAL      DSMSMS = %trim(MSG(10)) + ' ' +
074400010228     C                                      %trim(%editc(xkuncop : '4'))
074500000904     C                   MOVE      '1'           errl00
074600010228     C                   endif
074700010228     C                   endif
074800000808     C     keypar        reade(n)  tikun02l                               99
074900000808     C                   enddo
075000000808     C                   ENDIF
075100000808     C*--------------------
075200000323     C* inserimento codice azienda (attualmente non abilitato)
075300000808     C*--------------------
075400000323IF  1C     ERRL00        IFEQ      '0'
075500000323IF  2C     VIDazf        IFne      *blank
075600000323IF  3C     VIDazp        orne      *blank
075700000323     C                   SETON                                        6260
075800000323     C                   MOVEL     MSG(19)       dsmsms
075900000323     C                   MOVEL     '1'           errl00
076000000323E   2C                   ENDIF
076100000323E   1C                   ENDIF
076200000322     C* SCADENZA > DECORRENZA
076300000322IF  1C     ERRL00        IFEQ      '0'
076400000322IF  2C     DSCVID        IFLT      DDEVID
076500000322     C                   SETON                                        64
076600000322     C                   MOVEL     MSG(15)       DSMSMS
076700000322     C                   MOVEL     '1'           ERRL00
076800000322E   2C                   ENDIF
076900000322E   1C                   ENDIF
077000000901     C*--------------------
077100000912     C* SCADENZA >= DATA CORRENTE  ===> "Asterisco questo controllo"
077200000901     C*--------------------
077300000912IF  1C*    ERRL00        IFEQ      '0'
077400000912IF  2C*    DEPOP1        IFEQ      'O02'
077500000912     C*    DEPOP1        OREQ      'O07'
077600000912     C*    DEPOP1        OREQ      'F06'
077700000912     C*    DEPOP1        OREQ      'O03'
077800000912IF  3C*    DSCVID        IFLT      DATCOR
077900000912     C*                  SETON                                        64
078000000912     C*                  MOVEL     MSG(16)       DSMSMS
078100000912     C*                  MOVEL     '1'           ERRL00
078200000912E   3C*                  ENDIF
078300000912E   2C*                  ENDIF
078400000912E   1C*                  ENDIF
078500000904     C*--------------------
078600000904     c* scadenza <=> decorrenza  (accavalamento periodo)
078700000904     c*--------------------
078800000904IF  1c                   if        errl00 = '0'
078900000904     c                   movel     vidtle        kcetle
079000000904     c                   z-add     copvid        kcecop
079100000904     c                   z-add     cofvid        kcecof
079200000904     c                   movel     vidazp        kceazp
079300000904     c                   movel     vidazf        kceazf
079400000904     c     KE2kun        setll     tikun01L
079500000904     c     KE2kun        reade(n)  tikun01L                               99
079600000904DO  2c                   dow       *in99 = *OFF
079700000904     c                   if        (ddevid > kundsc  AND
079800000904     c                              dscvid > kundsc) OR
079900000904     c                             (ddevid < kundde  AND
080000000904     c                              dscvid < kundde)
080100000904     c                   else
080200000904     c                   if        ddevid <> kundde AND
080300000904     c                             kunvat  =  *blanks
080400000904     C                   seton                                        606162
080500000904     C                   seton                                        6364
080600000904     C                   movel     MSG(17)       DSMSMS
080700000904     C                   movel     '1'           ERRL00
080800000904     c                   leave
080900000904     c                   endif
081000000904     c                   endif
081100000904     c     KE2kun        reade(n)  tikun01L                               99
081200000904E   2c                   enddo
081300000904E   1c                   endif
081400000904     c*
081500000307     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (X CLIENTE "PADRE"/"FIGLIO")
081600000307     C                   EXSR      CHKAUT
081700000808     C*--------------------
081800000811     C* inserimento
081900000808     C*--------------------
082000000808     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
082100000904     C     DEPOP1        OREQ      'O03'                                        *COPIA
082200000808IF  1C     ERRL00        IFEQ      '0'
082300000808     C                   EXSR      LETkun
082400000808IF  1C     ERRL00        IFNE      '0'
082500000808     C                   MOVEL     MSG(25)       DSMSMS                         *MESSAGGIO DIRETTO
082600000808E   1C                   ENDIF
082700000808E   1C                   ENDIF
082800000808     C                   ENDIF
082900000808     C*--------------------
083000000808     C* annullamento sospensione
083100000808     C*--------------------
083200000808IF  1C     ERRL00        IFEQ      '0'
083300000808IF  2C     VIDvat        IFEQ      'S'
083400000808     C     VIDvat        OREQ      'A'
083500000808     C     VIDvat        OREQ      ' '
083600000808X   2C                   ELSE
083700000808     C                   MOVEL     '1'           ERRL00
083800000808     C                   SETON                                        67
083900000808     C                   MOVEL     MSG(11)       DSMSMS
084000000808E   2C                   ENDIF
084100000808E   1C                   ENDIF
084200000808     C*
084300020927     C*
084400020927     C* Il controllo seguente lo effettuo solo se trattasi di utente di sede in quanto in filiale
084500020927     C* il file TIVSS nn c'è
084600091117     C*                  if        simfel = *zeros
084700000308     C*
084800020927     C* VERIFICA CHE IL CLIENTE FIGLIO NON SIA GIA' UN CLIENTE ABILITATO (X SERVIZI CHE RICHIEDONO
084900020212     C* IL CONTROLLO)
085000091117     C*    ERRL00        ifeq      '0'
085100091117     C*                  if        vidtle = 'WW'
085200091117     C*                  move      vidcof        kssksu
085300091117     C*    keyvss        setll     tivss05l
085400091117     C*    keyvss        reade     tivss05l
085500091117     C*                  dow       not %eof(tivss05l)
085600091117     C*                  if        vssvat = *blanks                              *non annull/sosp.
085700091117     C*                  if        vssdsc < ddevid or                            *non valido
085800091117     C*                            vssdde > dscvid
085900091117     C*                  else                                                    *valido
086000091117     C* Verifico che il servizio del record corrente del TIVSS sia soggetto a controllo unificazioni
086100091117     C*    vssisv        lookup    isvkun                                 22
086200091117     C*                  if        *in22 = *on
086300091117     C*                  seton                                        65
086400091117     C*                  movel     MSG(26)       dsmsms
086500091117     C*                  movel     '1'           errl00
086600091117     C*                  leave
086700091117     C*                  endif
086800091117     C*                  endif
086900091117     C*                  endif
087000091117     C*    keyvss        reade     tivss05l
087100091117     C*                  enddo
087200091117     C*                  endif
087300091117     C*                  endif
087400091117     C*                  endif
087500090806     C*
087600090806     C* Verifico che codice padre nn già padre rispetto ad altro tipo legame dello stesso gruppo
087700090806     C     ERRL00        ifeq      '0'
087800090806     C                   exsr      CHKTIPTLE
087900090806     C                   endif
088000021024     C*
088100021024     C                   ENDIF
088200020211     C*
088300970528     C                   ENDSR
088400991008     C*------------------------------------------------------------------------*
088500991008     C* Cnttle - CONTROLLA codice tipo legame
088600991008     C*------------------------------------------------------------------------*
088700991008     c     cnttle        begsr
088800991008     c                   clear                   TIBS02DS
088900991008      *
089000991008     C     '?'           SCAN      Vidtle                                 30
089100991008      *  Ricerca
089200991008     c                   eval      t02tla = 'L'
089300991008    1C                   IF        *IN30 = *ON
089400991008     c                   movel     'R'           t02mod
089500991008     c                   else
089600991008     c                   movel     'C'           t02mod
089700991008     c                   movel(p)  vidtle        t02ke1
089800991008     c                   end
089900991008     C                   movel     'TLE'         t02cod
090000991008     c                   movel     knsif         t02sif
090100991008     C                   CALL      'TIBS02R'
090200991008     c                   parm                    KPJBA
090300991008     c                   parm                    TIBS02DS
090400991008     c
090500991008      *
090600991008    2C                   IF        T02err = *blanks
090700991008     C                   movel     T02ke1        Vidtle
090800991014     C                   movel     T02uni        dtle
090900991014     C                   movel     §tledesc      Vddtle
091000991014     C                   movel     §tlecu        Wtlecu            1
091100991014     C                   movel     §tlece        wtlece            1
091200991014     c                   else
091300991014     c                   movel     '1'           errl00
091400991014     c                   seton                                        66
091500991014     C                   MOVEL     MSG(12)       DSMSMS
091600991008     C                   ENDIF
091700991008     c
091800991008     c                   endsr
091900970528     C*------------------------------------------------------------------------*
092000000901     C* CHKcop - CONTROLLA codice padre figlio
092100970528     C*------------------------------------------------------------------------*
092200991008     C     CHKcop        BEGSR
092300970528     C*
092400991008     C     *LIKE         DEFINE    VIDcop        Wcop
092500991008     C     *LIKE         DEFINE    VDDcop        WDcop
092600991014     c                   move      *zeros        wksc
092700970528     C*
092800970528     C* RICHIESTO ELENCO
092900970528     C                   SETOFF                                           28
093000991008     C     '?'           SCAN      Wcop                                   28
093100970528IF  1C     *IN28         IFEQ      *ON
093200991008     C* RICHIESTO ELENCO CODICI
093300091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
093400091126      * deciso di non utilizzarli per le unificazioni dei codici
093500091126      * quindi non controllo il tipo codice e do per scontato che è sempre 'C'=cliente
093600991008     C                   CLEAR                   DSXC
093700991008     C                   Z-ADD     KCCCL         DXCCAP
093800991008     C                   CALL      'XCLIR'
093900991008     C                   PARM                    DSXC
094000991008     C* SCOMPATTA
094100991008     C                   MOVEA     DXCKSC        KSA                        ALF
094200991008DO  2C     1             DO        30            J                 3 0
094300991008     C                   MOVE      KSA(J)        DSKSA                      F
094400991008     C                   Z-ADD     DSKSN         KSC(J)                     M
094500991008E   2C                   ENDDO
094600991011     C                   MOVEL     KSC(1)        WKSC              7
094700991011     C                   MOVEL     '1'           $TESVD            1        TA
094800991008E   1C                   ENDIF
094900991008     C*
095000991008     C* CODICE CLIENTE NUMERICO
095100991008IF  1C     WKSC          IFLT      *ZEROS
095200991011     C                   MOVEL     '1'           WERR              1
095300991008X   1C                   ELSE
095400970528     C*
095500970528     C* SOLO CARATTERI NUMERICI
095600991011     c   28              move      *all'0'       wcop
095700991011     c   28              move      wksc          wcop
095800970528     C                   SETOFF                                       28
095900991008     C                   TESTN                   Wcop                 28
096000991015IF  2C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
096100970528     C                   MOVEL     '1'           ERRL00
096200991015X   2C                   ELSE                                                   *E'NUMERO DA TESTN
096300991008     C                   MOVE      Wcop          W001              1
096400991015IF  3C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
096500970528     C                   MOVEL     '1'           ERRL00
096600991015X   3C                   ELSE                                                   *NUMERO
096700970528     C*
096800970528     C* CONTROLLA ESISTENZA CODICE
096900991210     C                   MOVE      Wcop          wcli              7 0
097000991210     c                   exsr      dectib
097100991210     c                   movel     wfvrag        wdcop
097200991015E   3C                   ENDIF
097300991015E   2C                   ENDIF
097400991011E   1C                   ENDIF
097500970528     C*
097600970528     C                   ENDSR
097700991210     C*------------------------------------------------------------------------*
097800991210     C* Dectib - DECODIFICA CLIENTE
097900991210     C*------------------------------------------------------------------------*
098000991210     C     Dectib        BEGSR
098100991210     C*
098200991210     C                   CLEAR                   BS69DS
098300991210     C                   CLEAR                   ACODS
098400991210     C                   MOVEL     KNSIF         I69SIF
098500991210     C                   Z-ADD     WCLI          I69KAC
098600991210     C                   CALL      'TIBS69R'
098700991210     C                   PARM                    BS69DS
098800991210     C                   PARM                    ACODS
098900991210     C                   PARM                    INDDS
099000991210     C                   PARM                    CLPDS
099100991210     C                   PARM                    CLSDS
099200991210     C     O69ERR        IFNE      '1'
099300991210     C                   MOVEL     ACORAG        WFVRAG           40
099400991210     C                   ELSE
099500991210     C                   MOVEL     *ALL'*'       WFVRAG
099600991210     c                   movel     '1'           errl00
099700991210     C                   ENDIF
099800991210     C*
099900991210     C                   ENDSR
100000991210     C*------------------------------------------------------------------------*
100100991210     C* FINSR - OPERAZIONI FINALI
100200991210     C*------------------------------------------------------------------------*
100300991210     C     FINSR         BEGSR
100400991210     C*
100500991210     C* CHIUDE I FILE DEI PGM CHIAMATI
100600991210     C                   CLEAR                   bs69ds
100700991210     C                   CLEAR                   acods
100800991210     C                   CLEAR                   indds
100900991210     C                   CLEAR                   clpds
101000991210     C                   CLEAR                   clsds
101100991210     C                   MOVEL     'C'           i69tla
101200991210     C                   MOVEL     knsif         i69sif
101300991210     C                   CALL      'TIBS69R'
101400991210     C                   PARM                    bs69ds
101500991210     C                   PARM                    acods
101600991210     C                   PARM                    indds
101700991210     C                   PARM                    clpds
101800991210     C                   PARM                    clsds
101900991210     C*
102000991210     C                   ENDSR
102100970528     C*------------------------------------------------------------------------*
102200970528     C* CHKDA8 - CONTROLLA LA DATA (8)
102300970528     C*------------------------------------------------------------------------*
102400970528     C     CHKDA8        BEGSR
102500970528     C*
102600970528     C     *LIKE         DEFINE    VIDDDE        WDA8
102700970528     C     *LIKE         DEFINE    VIDDDE        DA8W
102800970528     C*
102900970528     C                   Z-ADD     WDA8          G08DAT
103000970528     C                   Z-ADD     *ZEROS        G08INV
103100970528     C                   MOVEL     '0'           G08ERR
103200970528     C                   CALL      'XSRDA8'
103300970528     C                   PARM                    WLBDA8
103400970528     C                   Z-ADD     G08DAT        WDA8
103500970528     C                   Z-ADD     G08INV        DA8W
103600970528IF  1C     G08ERR        IFEQ      '1'
103700970528     C                   MOVEL     '1'           ERRL00
103800970528E   1C                   ENDIF
103900970528     C*
104000970528     C                   ENDSR
104100970526     C*------------------------------------------------------------------------*
104200970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
104300970526     C*------------------------------------------------------------------------*
104400970526     C     IREVID        BEGSR
104500970526     C* CAMPI
104600991011     C                   MOVEL     kuntle        VIDtle
104700991011     C                   MOVEL     kunvat        VIDvat
104800151125     C                   EVAL      VIDSTSRCD = *blank
104900151125     C                   SELECT
105000151125     C                   WHEN      VIDVAT = *blank
105100151125     C                   EVAL      VIDSTSRCD = *blank
105200151125     C                   WHEN      VIDVAT = 'A'
105300151125     C                   EVAL      VIDSTSRCD = 'ANNULLATO'
105400151125     C                   WHEN      VIDVAT = 'S'
105500151125     C                   EVAL      VIDSTSRCD = 'SOSPESO'
105600151125     C                   OTHER
105700151125     C                   EVAL      VIDSTSRCD = *blank
105800151125     C                   ENDSL
105900991011     C                   MOVEL     kunazp        VIDazp
106000991011     C                   MOVEL     kunazf        VIDazf
106100991011     C                   move      kuncop        vidcop
106200991011     C                   move      kuncof        vidcof
106300991011     C                   Z-ADD     kunDDE        G08INV
106400970528     C                   Z-ADD     *ZEROS        G08DAT
106500970528     C                   MOVEL     '3'           G08ERR
106600970528     C                   CALL      'XSRDA8'
106700970528     C                   PARM                    WLBDA8
106800970528     C                   Z-ADD     G08DAT        VIDDDE
106900991011     C                   Z-ADD     kunDSC        G08INV
107000970528     C                   Z-ADD     *ZEROS        G08DAT
107100970528     C                   MOVEL     '3'           G08ERR
107200970528     C                   CALL      'XSRDA8'
107300970528     C                   PARM                    WLBDA8
107400970528     C                   Z-ADD     G08DAT        VIDDSC
107500130412     C                   Z-ADD     kunDIM        G08INV
107600130412     C                   Z-ADD     *ZEROS        G08DAT
107700130412     C                   MOVEL     '3'           G08ERR
107800130412     C                   CALL      'XSRDA8'
107900130412     C                   PARM                    WLBDA8
108000130412     C                   Z-ADD     G08DAT        VIDDIM
108100130412     C                   Z-ADD     kunDUV        G08INV
108200130412     C                   Z-ADD     *ZEROS        G08DAT
108300130412     C                   MOVEL     '3'           G08ERR
108400130412     C                   CALL      'XSRDA8'
108500130412     C                   PARM                    WLBDA8
108600130412     C                   Z-ADD     G08DAT        VIDDUV
108700130412     C                   EVAL      VIDPUV = KUNPUV
108800060726     C*
108900060726     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
109000060726     C                   IF        DEPOP1 = 'F06' OR
109100150506     C                             DEPOP1 = 'O03'
109200060726     C                   EVAL      VIDDDE = DATCORINV
109300060726     C                   EVAL      VIDDSC = 31122039
109400060726     C                   ENDIF
109500970527     C*
109600970526     C                   ENDSR
109700970526     C*------------------------------------------------------------------------*
109800970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
109900970526     C*------------------------------------------------------------------------*
110000970526     C     AZZVID        BEGSR
110100970526     C* CAMPI
110200151125     C                   EVAL      VIDSTSRCD = *blank
110300991008     C                   MOVEL     *BLANKS       VIDtle
110400991008     C                   MOVEL     *BLANKS       VIDazf
110500991008     C                   MOVEL     *BLANKS       VIDazp
110600991008     C                   MOVEL     *BLANKS       VIDvat
110700060726     C                   EVAL      VIDDDE = DATCORINV
110800060726     C                   EVAL      VIDDSC = 31122039
110900991011     C                   move      *ZEROS        VIDcop
111000991011     C                   move      *ZEROS        VIDcof
111100970526     C* DECODIFICHE
111200991011     C                   MOVEL     *BLANKS       VDDtle
111300991008     C                   MOVEL     *BLANKS       VDDcop
111400991008     C                   MOVEL     *BLANKS       VDDcof
111500970526     C*
111600970526     C                   ENDSR
111700970526     C*------------------------------------------------------------------------*
111800991011     C* IMPkun - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
111900970526     C*------------------------------------------------------------------------*
112000991011     C     IMPkun        BEGSR
112100970526     C*
112200000913     C                   MOVEL     vidvat        kunvat                         *CHIAVE
112300991014     C                   z-add     cofVID        kuncof
112400991014     C                   Z-ADD     DDEVID        kunDDE
112500991008     C                   Z-ADD     DSCVID        kunDSC
112600970526     C*
112700970526     C                   ENDSR
112800970526     C*------------------------------------------------------------------------*
112900970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
113000970526     C*------------------------------------------------------------------------*
113100970526     C     IMPFUN        BEGSR
113200970526     C*
113300970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
113400970526IF  1C     DEPOP1        IFEQ      'O05'
113500970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
113600970526     C                   MOVEL     'N'           FUDATT
113700970526     C                   MOVEL     FUDRIG        FUD(6)
113800970526E   1C                   ENDIF
113900970526     C*
114000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
114100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
114200970526     C                   EXSR      FUNGES
114300970526     C*
114400970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
114500970526     C*---
114600970526     C* 90 -> PROTEZIONE CHIAVE
114700970526     C* 91 -> PROTEZIONE TOTALE
114800970526     C*---
114900970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
115000970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
115100000904     C     DEPOP1        OREQ      'O03'                                        *COPIA
115200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
115300970526     C                   SETOFF                                       91
115400970526E   2C                   ENDIF
115500970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
115600000904IF  2C     DEPOP1        OREQ      'O01'                                        *SCELTA
115700970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
115800970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
115900970526E   2C                   ENDIF
116000970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
116100970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
116200970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
116300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
116400970526     C                   SETON                                        91
116500970526E   2C                   ENDIF
116600970526X   1C                   ELSE                                                   *I N T E R R O G A
116700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
116800970526     C                   SETON                                        91
116900970526E   1C                   ENDIF
117000970526     C*
117100970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
117200970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
117300991011     C                   MOVEL     k12tle        KCEtle
117400991011     C                   Z-ADD     k12cop        KCEcop
117500991011     C                   Z-ADD     k12cof        KCEcof
117600991011     C                   Z-ADD     k12dde        KCEDDE
117700991011     C     KEYkun        CHAIN     tikun01L                           9998
117800970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
117900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
118000970526     C                   MOVEL     MSG(6)        DSMSMS
118100970526X   2C                   ELSE
118200970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
118300980128IF  3C     DEPOP1        IFEQ      'O05'
118400991014IF  3C     DEPOP1        oreq      'O02'
118500980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
118600980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
118700980128E   3C                   ENDIF
118800970526E   2C                   ENDIF
118900970526X   1C                   ELSE                                                   *INSERIMENTO
119000970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
119100970526E   1C                   ENDIF
119200970526     C*
119300970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
119400970526IF  1C     DEPOP1        IFEQ      'O02'
119500970526     C                   MOVEL     DEC(1)        VIDFUN
119600970526E   1C                   ENDIF
119700991014IF  1C     DEPOP1        IFEQ      'O03'
119800991014     C                   MOVEL     DEC(6)        VIDFUN
119900991014E   1C                   ENDIF
120000970526IF  1C     DEPOP1        IFEQ      'O04'
120100970526     C                   MOVEL     DEC(2)        VIDFUN
120200970526E   1C                   ENDIF
120300970526IF  1C     DEPOP1        IFEQ      'O05'
120400970526     C                   MOVEL     DEC(3)        VIDFUN
120500970526E   1C                   ENDIF
120600970526IF  1C     DEPOP1        IFEQ      'O07'
120700970526     C                   MOVEL     DEC(4)        VIDFUN
120800970526E   1C                   ENDIF
120900970526IF  1C     DEPOP1        IFEQ      'F06'
121000970526     C                   MOVEL     DEC(5)        VIDFUN
121100970526E   1C                   ENDIF
121200970526     C*
121300970526     C                   ENDSR
121400020212     C*--------------------------------------------------------------------------------------------*
121500020212     C* CARTAB - CARICA LE TABELLE OCCORRENTI
121600020212     C*--------------------------------------------------------------------------------------------*
121700020212     C     CARTAB        BEGSR
121800020905     C*
121900020905     C* REPERISCE I DATI UTENTE
122000020905     C                   EXSR      REPDATIUTE
122100020905     C*
122200020212     C*----------
122300020212     C* Tipi servizi soggetti a controllo unificazioni
122400020212     C*----------
122500020212     C                   z-add     *zeros        x                 3 0
122600020212     C                   eval      tbecod = 'ISV'
122700020212     C     ke2tbe        setll     tntbe01l
122800020212     C     ke2tbe        reade     tntbe01l
122900020212     C                   dow       not %eof(tntbe01l)
123000020212     C                   movel     tbeuni        dsisv
123100020212     C                   if        §isvuni  = 'S'
123200020212     C                   add       1             x
123300020212     C                   eval      isvkun(x) = tbeke1
123400020212     C                   endif
123500020212     C     ke2tbe        reade     tntbe01l
123600020212     C                   enddo
123700090806     C*----------
123800090806     C* Tipi unificazioni
123900090806     C*----------
124000090806     C                   eval      tbecod = 'TLE'
124100090806     C     tbecod        setll     tntbe01l
124200090806     C     tbecod        reade(n)  tntbe01l
124300090806     C                   dow       not %eof(tntbe01l)
124400090806     C                   movel     tbeuni        dtle
124500090806     C                   select
124600090806     C                   when      §tlegrp  = 'W'
124700090806     C                   add       1             jskTIPTLEWW
124800090806     C                   eval      skTIPTLEWW(jskTIPTLEWW) = tbeke1
124900090806     C                   when      §tlegrp  = 'S'
125000090806     C                   add       1             jskTIPTLEST
125100090806     C                   eval      skTIPTLEST(jskTIPTLEST) = tbeke1
125200090806     C                   endsl
125300090806     C     tbecod        reade(n)  tntbe01l
125400090806     C                   enddo
125500020212     C*
125600020212     C                   ENDSR
125700020905     C*--------------------------------------------------------------------------------------------*
125800020905     C* REPERISCE I DATI UTENTE
125900020905     C*--------------------------------------------------------------------------------------------*
126000020905     C     REPDATIUTE    BEGSR
126100020905     C*
126200020905     C* INIZIALIZZA VARIABILI DI WRK
126300020905     C                   CLEAR                   TIBS34DS
126400020905     C                   CLEAR                   AZUTEDS
126500020905     C                   CLEAR                   DDATIUTE
126600020905     C*
126700020905     C     *DTAARA       DEFINE    §azute        azuteds
126800020905     C     *DTAARA       DEFINE    §datiute      ddatiute
126900020905     C                   IN(E)     *DTAARA
127000020905if  1C                   IF        %Error
127100020905     c                   EVAL      I34Tla = 'L'
127200020905     C                   CALL      'TIBS34R'
127300020905     C                   PARM                    Tibs34Ds
127400020905     C                   IN        *DTAARA
127500020905e   1C                   ENDIF
127600020905     C*
127700020905     C                   MOVEL     RSUT          DSFIRS
127800060113     C*
127900060113     C* Carica le abilitazioni particolari del profilo
128000060113     C                   Movel     *blanks       wabi              2
128100060113     C                   Movel     utefaf        dute01
128200060113     C                   If        §utecli <> *Blanks
128300060113     C                   Eval      wabi = §utecli
128400060113     C                   Else
128500060113     C                   Eval      wabi = uteaut
128600060113     C                   EndIf
128700060113     C*
128800060113     C* Reperimento dei P.O. gestibili dall'utente per i codici clienti
128900060113     C                   Clear                   Trul31ds
129000060113     C                   Eval      i31abi = wabi
129100060113     C                   Eval      i31cdi = dutdis
129200060113     C                   Eval      i31car = dutare
129300060113     C                   Eval      i31cpo = dutpou
129400060113     C                   Call      'TRUL31R'
129500060113     C                   Parm                    kpjba
129600060113     C                   Parm                    Trul31ds
129700060113     C                   If        o31pog > *zeros
129800060113     C                   Movea     o31pog        skPOG
129900060113     C                   Else
130000060113     C                   Movel     '1'           ERRl00
130100060113     C                   Movel     MSG(27)       DSMSMS
130200060113     C                   EndIf
130300020905     C*
130400020905     C                   ENDSR
130500090806     C*------------------------------------------------------------------------*
130600090806     C* CHKTIPTLE - VERIFICHE X TIPI LEGAMI
130700090806     C*------------------------------------------------------------------------*
130800090806     C     CHKTIPTLE     BEGSR
130900090806     C*
131000090806     C* VERIFICA CHE IL NUOVO PADRE NN SIA GIA' PRESENTE COME PADRE X UNO QUALUNQUE DEI
131100090806     C* LEGAMI DI TIPO 'W'
131200090806     C                   MOVE(p)   vidcop        KCEcop
131300090806     C     KCEcop        SETLL     tikun03l
131400090806     C                   IF        %found(tikun03l)
131500090806     C     KCEcop        READE     tikun03l
131600090806     C                   DOW       not %eof(tikun03l)
131700090806     C                   IF        (ddevid > kundsc    AND
131800090806     C                              dscvid > kundsc)   OR
131900090806     C                             (ddevid < kundde    AND
132000090806     C                              dscvid < kundde)
132100090806     C                   ELSE
132200091020     C* Ovviamente nn considero il record correntemente a video
132300091020     C                   IF        vidtle <> kuntle
132400090806     C*
132500090806     C                   IF        §TLEGRP = 'W'
132600090806     C     KUNTLE        LOOKUP    skTIPTLEWW                             30
132700090806     C                   IF        *IN30
132800090806     C                   SETON                                        61
132900090806     C                   MOVEL     '1'           ERRL00
133000090806     C                   EVAL      DSMSMS = MSG(28)
133100090806     C                   LEAVE
133200090806     C                   ENDIF
133300090806     C                   ENDIF
133400090806     C*
133500090806     C                   IF        §TLEGRP = 'S'
133600090806     C     KUNTLE        LOOKUP    skTIPTLEST                             30
133700090806     C                   IF        *IN30
133800090806     C                   SETON                                        61
133900090806     C                   MOVEL     '1'           ERRL00
134000090806     C                   EVAL      DSMSMS = MSG(28)
134100090806     C                   LEAVE
134200090806     C                   ENDIF
134300090806     C                   ENDIF
134400091020     C*
134500091020     C                   ENDIF
134600091020     C*
134700090806     C                   ENDIF
134800090806     C*
134900090806     C     KCEcop        READE     tikun03l
135000090806     C                   ENDDO
135100090806     C                   ENDIF
135200090806     C*
135300090806     C                   ENDSR
135400970526     C*------------------------------------------------------------------------*
135500970526     C* *INZSR - OPERAZIONI INIZIALI
135600970526     C*------------------------------------------------------------------------*
135700970526     C     *INZSR        BEGSR
135800970526     C*---
135900970526     C* RICEVIMENTO PARAMETRI
136000970526     C*---
136100970526     C     *ENTRY        PLIST
136200970612     C                   PARM                    KPJBA
136300991028     C                   movel     kpjbu         tibds
136400970526     C*
136500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
136600970526     C     XMSGPA        PLIST
136700970526     C                   PARM                    DSMSMS
136800970526     C                   PARM                    DSMSPG
136900970526     C                   PARM                    DSMSKE
137000970526     C                   PARM                    DSMSER
137100970526     C*
137200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
137300970526     C                   CALL      'XKEY'
137400970526     C                   PARM                    DSKEY
137500020212     C*
137600020212     C* CARICA LE TABELLE OCCORRENTI
137700020212     C                   EXSR      CARTAB
137800020927     C*
137900020927     C* IN BASE ALL'UTENTE EFFETTUO ALCUNE CONSIDERAZIONI
138000021007     C                   IF        simfel = *zeros
138100020927     C                   OPEN      tivss05l
138200020927     C                   ENDIF
138300970526     C*---
138400970526     C* VARIABILI NON RIFERITE AL DATA BASE
138500970526     C*---
138600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
138700991011     C     *LIKE         DEFINE    k12OP0        DEPOP0
138800991011     C     *LIKE         DEFINE    k12OP1        DEPOP1
138900991011     C     *LIKE         DEFINE    k12OP0        WOP0
139000991011     C     *LIKE         DEFINE    k12OP1        WOP1
139100970526     C*---
139200970526     C* VARIABILI DI CONTROLLO
139300970526     C*---
139400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
139500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
139600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
139700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
139800000307     C                   SETON                                        50
139900970526     C*
140000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
140100991008     C                   MOVEL     'TIBS13R'     DSMSPG
140200991008     C                   MOVEL     'TIBS13R'     MSGQUE
140300970526     C                   MOVEL     *BLANKS       MSGKEY
140400970526     C                   MOVEL     *BLANKS       DSMSMS
140500970526     C*---
140600970526     C* VARIABILI RIFERITE AL DATA BASE
140700970526     C*---
140800020830     C     *LIKE         DEFINE    acokcc        kcccl
140900991011     C     *LIKE         DEFINE    kuntle        KCEtle
141000991011     C     *LIKE         DEFINE    kuncop        KCEcop
141100991015     C     *LIKE         DEFINE    kuncop        Kpar3
141200991011     C     *LIKE         DEFINE    kuncof        KCEcof
141300991011     C     *LIKE         DEFINE    kunDDE        KCEDDE
141400991011     C     *LIKE         DEFINE    kunazf        KCEazf
141500991011     C     *LIKE         DEFINE    kunazp        KCEazp
141600000307     C     *LIKE         DEFINE    tbecod        KTBcod
141700000307     C     *LIKE         DEFINE    tbeke1        KTBky1
141800020211     C     *LIKE         DEFINE    vssksu        KSSksu
141900020905     C     *LIKE         DEFINE    kpjbu         Depkpjbu
142000020830     C*
142100020830     C* INIZIALIZZAZIONE VARIABILI DI WRK
142200020830     C                   z-add     1             kclkut
142300020916     C                   z-add     DUTKCI        kcccl
142400020830     C                   movel     *blank        kceazp
142500020830     C                   movel     *blank        kceazf
142600970526     C*---
142700970526     C* CHIAVI DI LETTURA
142800970526     C*---
142900991011     C* LETTURA tikun01L - COMPLETA
143000991011     C     KEYkun        KLIST
143100090806     C                   KFLD                    KCEtle
143200090806     C                   KFLD                    KCEazp
143300090806     C                   KFLD                    KCEcop
143400090806     C                   KFLD                    KCEazf
143500090806     C                   KFLD                    KCEcof
143600090806     C                   KFLD                    KCEdde
143700090806     C     KEYpar        KLIST
143800090806     C                   KFLD                    vidtle
143900090806     C                   KFLD                    KCEazp
144000090806     C                   KFLD                    Kpar3
144100090806     C* LETTURA tikun01L - COMPLETA
144200090806     C     KE2kun        KLIST
144300090806     C                   KFLD                    KCEtle
144400090806     C                   KFLD                    KCEazp
144500090806     C                   KFLD                    KCEcop
144600090806     C                   KFLD                    KCEazf
144700090806     C                   KFLD                    KCEcof
144800991210     C* LETTURA cnaco31c
144900991008     C     keycli        KLIST                                                  *di cnaco/ind/clp
145000991008     C                   KFLD                    kclkut            1 0          -utente
145100991008     C                   KFLD                    kcccl                          -capoconto
145200991008     C                   KFLD                    kcksc             7 0          -codice
145300000307     C* LETTURA tntbe01l -
145400000307     C     KEYtbe        KLIST
145500000307     C                   KFLD                    Ktbcod                         *CODICE TABELLA
145600000307     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
145700020211     C* LETTURA tivss02L - PARZIALE
145800020211     C     KEYvss        KLIST
145900020211     C                   KFLD                    KSSksu                         *CODICE CLI ABIL.
146000020212     C* LETTURA tntbe01l - PARZIALE
146100020212     C     KE2tbe        KLIST
146200020212     C                   KFLD                    tbecod                         *CODICE TABELLA
146300970528     C*
146400970528     C* CALCOLA LA DATA CORRENTE
146500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
146600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
146700970528     C                   Z-ADD     WN8           G08DAT
146800060726     C                   Z-ADD     WN8           DATCORINV         8 0
146900970528     C                   Z-ADD     *ZEROS        G08INV
147000970528     C                   MOVEL     '0'           G08ERR
147100970528     C                   CALL      'XSRDA8'
147200970528     C                   PARM                    WLBDA8
147300970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
147400970526     C*
147500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
147600991011     C                   MOVEL     k12OP0        DEPOP0                         *LIVELLO PROCEDURA
147700991011     C                   MOVEL     k12OP1        DEPOP1                         *LIVELLO PROGRAMMA
147800060726     C*
147900060726     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
148000060726     C                   IF        DEPOP1 = 'F06' OR
148100150506     C                             DEPOP1 = 'O03'
148200060726     C                   EVAL      VIDDDE = DATCORINV
148300060726     C                   EVAL      VIDDSC = 31122039
148400060726     C                   ENDIF
148500970526     C*
148600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
148700970526     C                   EXSR      IMPFUN
148800970526     C*
148900970526     C                   ENDSR
149000970526     C*------------------------------------------------------------------------*
149100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
149200970526     C*------------------------------------------------------------------------*
149300970526     C     FUNGES        BEGSR
149400970526     C*
149500970526     C* PULISCO CAMPI DI LAVORO
149600970526     C                   Z-ADD     0             W                 5 0
149700970526     C*
149800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
149900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
150000970526     C     1             DO        24            I
150100970526     C                   MOVEL     FUD(I)        FUDRIG
150200970526     C*
150300970526     C* PULIZIA FUNZIONI PRECEDENTI
150400970526     C                   MOVEL     *BLANKS       FUV(I)
150500970526     C*
150600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
150700970526     C* (SALTA F24)
150800970526     C     FUDATT        IFEQ      'S'
150900970526     C     FUDFUN        ANDNE     '24'
151000970526     C                   ADD       1             W
151100970526     C                   MOVEL     FUDDES        FUV(W)
151200970526     C                   END
151300970526     C                   END                                                    DO WHILE
151400970526     C*
151500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
151600970526     C* ALTRIMENTI DISATTIVO F24
151700970526     C     1             DO        24            I
151800970526     C                   MOVEL     FUD(I)        FUDRIG
151900970526     C*
152000970526     C     FUDFUN        IFEQ      '24'
152100970526     C     W             IFGT      6
152200970526     C                   MOVEL     'S'           FUDATT
152300970526     C                   ELSE
152400970526     C                   MOVEL     *BLANKS       FUDATT
152500970526     C                   END                                                    W GT 8
152600970526     C                   MOVEL     FUDRIG        FUD(I)
152700970526     C                   END
152800970526     C                   END                                                    DO WHILE
152900970526     C*
153000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
153100970526     C     W             DIV       6             I
153200970526     C                   MVR                     Z
153300970526     C*
153400970526     C     Z             IFNE      0
153500970526     C     I             ADD       1             Z
153600970526     C                   ELSE
153700970526     C                   Z-ADD     I             Z                 5 0
153800970526     C                   END
153900970526     C*
154000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
154100970526     C                   ADD       1             FUNGRU
154200970526     C     FUNGRU        IFGT      Z
154300970526     C     FUNGRU        ORLT      1
154400970526     C                   Z-ADD     1             FUNGRU
154500970526     C                   END
154600970526     C*
154700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
154800970526     C     FUNGRU        IFEQ      1
154900970526     C                   MOVEL     FUV(1)        FUN001
155000970526     C                   MOVEL     FUV(2)        FUN002
155100970526     C                   MOVEL     FUV(3)        FUN003
155200970526     C                   MOVEL     FUV(4)        FUN004
155300970526     C                   MOVEL     FUV(5)        FUN005
155400970526     C                   MOVEL     FUV(6)        FUN006
155500970526     C                   END
155600970526     C*
155700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
155800970526     C     FUNGRU        IFEQ      2
155900970526     C                   MOVEL     FUV(7)        FUN001
156000970526     C                   MOVEL     FUV(8)        FUN002
156100970526     C                   MOVEL     FUV(9)        FUN003
156200970526     C                   MOVEL     FUV(10)       FUN004
156300970526     C                   MOVEL     FUV(11)       FUN005
156400970526     C                   MOVEL     FUV(12)       FUN006
156500970526     C                   END
156600970526     C*
156700970526     C* TERZO GRUPPO DI 6 FUNZIONI
156800970526     C     FUNGRU        IFEQ      3
156900970526     C                   MOVEL     FUV(13)       FUN001
157000970526     C                   MOVEL     FUV(14)       FUN002
157100970526     C                   MOVEL     FUV(15)       FUN003
157200970526     C                   MOVEL     FUV(16)       FUN004
157300970526     C                   MOVEL     FUV(17)       FUN005
157400970526     C                   MOVEL     FUV(18)       FUN006
157500970526     C                   END
157600970526     C*
157700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
157800970526     C     FUNGRU        IFEQ      4
157900970526     C                   MOVEL     FUV(19)       FUN001
158000970526     C                   MOVEL     FUV(20)       FUN002
158100970526     C                   MOVEL     FUV(21)       FUN003
158200970526     C                   MOVEL     FUV(22)       FUN004
158300970526     C                   MOVEL     *BLANKS       FUN005
158400970526     C                   MOVEL     *BLANKS       FUN006
158500970526     C                   END
158600970526     C*
158700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
158800970526     C                   MOVEL     *BLANKS       FUN007
158900970526     C                   MOVEL     *BLANKS       FUN008
159000970526     C*
159100970526     C     1             DO        24            I
159200970526     C                   MOVEL     FUD(I)        FUDRIG
159300970526     C     FUDFUN        IFEQ      '24'
159400970526     C     FUDATT        ANDEQ     'S'
159500970526     C                   MOVEL     FUDDES        FUN008
159600970526     C                   END
159700970526     C                   END                                                    DO
159800970526     C*
159900970526     C                   ENDSR
160000970526     C*------------------------------------------------------------------------*
160100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
160200970526     C*------------------------------------------------------------------------*
160300970526     C     FUNCON        BEGSR
160400970526     C*
160500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
160600970526     C                   EXSR      FUNVAR
160700970526     C*
160800970526     C* PULISCO CAMPI DI LAVORO
160900970526     C                   Z-ADD     0             I                 5 0
161000970526     C                   Z-ADD     0             W                 5 0
161100970526     C*
161200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
161300970526     C     FUNKEY        IFNE      *BLANKS
161400970526     C     1             DO        24            I
161500970526     C                   MOVEL     FUD(I)        FUDRIG
161600970526     C*
161700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
161800970526     C     FUNKEY        IFEQ      FUDFUN
161900970526     C     FUDATT        ANDEQ     'S'
162000970526     C                   Z-ADD     I             W
162100970526     C                   END
162200970526     C                   END                                                    DO WHILE
162300970526     C*
162400970526     C* FUNZIONE NON ATTIVA (ERRORE)
162500970526     C     W             IFEQ      0
162600970526     C*
162700970526     C* EMISSIONE MESSAGGIO DI ERRORE
162800970526     C                   MOVEL     MSG(1)        DSMSMS
162900970526     C*
163000970526     C                   END                                                    W=0
163100970526     C                   END                                                    FUNKEY NE *BLANKS
163200970526     C*
163300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
163400970526     C     W             IFNE      0
163500970526     C     FUNKEY        OREQ      *BLANKS
163600970526     C     FUNKEY        IFEQ      *BLANKS
163700970526     C                   EXSR      ENTGES
163800970526     C                   END
163900970526     C     FUNKEY        IFEQ      '03'
164000970526     C                   EXSR      F03GES
164100970526     C                   END
164200970526     C     FUNKEY        IFEQ      '06'
164300970526     C                   EXSR      F06GES
164400970526     C                   END
164500970526     C     FUNKEY        IFEQ      '12'
164600970526     C                   EXSR      F12GES
164700970526     C                   END
164800970526     C     FUNKEY        IFEQ      '24'
164900970526     C                   EXSR      F24GES
165000970526     C                   END
165100970526     C                   END
165200970526     C*
165300970526     C                   ENDSR
165400970526     C*------------------------------------------------------------------------*
165500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
165600970526     C*------------------------------------------------------------------------*
165700970526     C     FUNVAR        BEGSR
165800970526     C*
165900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
166000970526     C*
166100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
166200970526     C     KEY           IFEQ      ENTER
166300970526     C                   MOVEL     *BLANKS       FUNKEY
166400970526     C                   END
166500970526     C     KEY           IFEQ      F1
166600970526     C                   MOVEL     '01'          FUNKEY
166700970526     C                   END
166800970526     C     KEY           IFEQ      F2
166900970526     C                   MOVEL     '02'          FUNKEY
167000970526     C                   END
167100970526     C     KEY           IFEQ      F3
167200970526     C                   MOVEL     '03'          FUNKEY
167300970526     C                   END
167400970526     C     KEY           IFEQ      F4
167500970526     C                   MOVEL     '04'          FUNKEY
167600970526     C                   END
167700970526     C     KEY           IFEQ      F5
167800970526     C                   MOVEL     '05'          FUNKEY
167900970526     C                   END
168000970526     C     KEY           IFEQ      F6
168100970526     C                   MOVEL     '06'          FUNKEY
168200970526     C                   END
168300970526     C     KEY           IFEQ      F7
168400970526     C                   MOVEL     '07'          FUNKEY
168500970526     C                   END
168600970526     C     KEY           IFEQ      F8
168700970526     C                   MOVEL     '08'          FUNKEY
168800970526     C                   END
168900970526     C     KEY           IFEQ      F9
169000970526     C                   MOVEL     '09'          FUNKEY
169100970526     C                   END
169200970526     C     KEY           IFEQ      F10
169300970526     C                   MOVEL     '10'          FUNKEY
169400970526     C                   END
169500970526     C     KEY           IFEQ      F11
169600970526     C                   MOVEL     '11'          FUNKEY
169700970526     C                   END
169800970526     C     KEY           IFEQ      F12
169900970526     C                   MOVEL     '12'          FUNKEY
170000970526     C                   END
170100970526     C     KEY           IFEQ      F13
170200970526     C                   MOVEL     '13'          FUNKEY
170300970526     C                   END
170400970526     C     KEY           IFEQ      F14
170500970526     C                   MOVEL     '14'          FUNKEY
170600970526     C                   END
170700970526     C     KEY           IFEQ      F15
170800970526     C                   MOVEL     '15'          FUNKEY
170900970526     C                   END
171000970526     C     KEY           IFEQ      F16
171100970526     C                   MOVEL     '16'          FUNKEY
171200970526     C                   END
171300970526     C     KEY           IFEQ      F17
171400970526     C                   MOVEL     '17'          FUNKEY
171500970526     C                   END
171600970526     C     KEY           IFEQ      F18
171700970526     C                   MOVEL     '18'          FUNKEY
171800970526     C                   END
171900970526     C     KEY           IFEQ      F19
172000970526     C                   MOVEL     '19'          FUNKEY
172100970526     C                   END
172200970526     C     KEY           IFEQ      F20
172300970526     C                   MOVEL     '20'          FUNKEY
172400970526     C                   END
172500970526     C     KEY           IFEQ      F21
172600970526     C                   MOVEL     '21'          FUNKEY
172700970526     C                   END
172800970526     C     KEY           IFEQ      F22
172900970526     C                   MOVEL     '22'          FUNKEY
173000970526     C                   END
173100970526     C     KEY           IFEQ      F23
173200970526     C                   MOVEL     '23'          FUNKEY
173300970526     C                   END
173400970526     C     KEY           IFEQ      F24
173500970526     C                   MOVEL     '24'          FUNKEY
173600970526     C                   END
173700970526     C     KEY           IFEQ      ROLLUP
173800970526     C                   MOVEL     'RU'          FUNKEY
173900970526     C                   END
174000970526     C     KEY           IFEQ      HELP
174100970526     C                   MOVEL     'HP'          FUNKEY
174200970526     C                   END
174300970526     C                   ENDSR
174400970526     C*------------------------------------------------------------------------*
174500970526     C* NOTE COSTRUZIONE SCHIERE
174600970526     C*------------------------------------------------------------------------*
174700970526     C*
174800970526     C* -MSG-MESSAGGI
174900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
175000970526     C* (NON TOCCARE MSG,1 E MSG,2)
175100970526     C*
175200970526     C* -CMD-COMANDI
175300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
175400970526     C*
175500970526     C* -OPD-OPZIONI
175600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
175700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
175800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
175900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
176000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
176100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
176200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
176300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
176400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
176500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
176600970526     C*
176700970526     C* -FUD-FUNZIONI
176800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
176900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
177000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
177100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
177200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
177300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
177400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
177500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
177600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
177700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
177800970526     O*------------------------------------------------------------------------*
177900970526** DEC - DECODIFICHE
178000970526    MODIFICA                                                      1
178100970526  ANNULLAMENTO                                                    2
178200970526VISUALIZZAZIONE                                                   3
178300970526   RIPRISTINO                                                     4
178400970526  INSERIMENTO                                                     5
178500991014     COPIA                                                        6
178600970526** MSG - MESSAGGI
178700991011TIBS13R- Tasto funzionale non gestito                                          1
178800991011TIBS13R- Inserimento effettuato                                                2
178900991011TIBS13R- Modifica effettuata                                                   3
179000991011TIBS13R- Annullamento effettuato                                               4
179100991011TIBS13R- Ripristino effettuato                                                 5
179200991011TIBS13R- Richiesta funzione per record inesistente                             6
179300991011TIBS13R- Record momentaneamente allocato: riprovare più tardi                  7
179400991011TIBS13R- Richiesta funzione per record esistente                               8
179500000307TIBS13R- Codice figlio già presente come codice padre con questo legame        9
179600010228TIBS13R- Codice figlio già associato nel periodo ad un'altro padre             10
179700991011TIBS13R- Valore inserito errato i valori ammessi sono(S=Sosp A=Ann Blank)      11
179800991011TIBS13R- Tipo legame errato verificarne sulla tabella l'esistenza              12
179900991011TIBS13R- Codice padre errato                                                   13
180000991011TIBS13R- Data errata                                                           14
180100991011TIBS13R- La data scadenza deve essere maggiore della decorrenza                15
180200991011TIBS13R- Il record non può scadere in un giorno già trascorso                  16
180300991011TIBS13R- Il record immesso si sovrappone ad uno già in decorrenza              17
180400991011TIBS13R- Codice figlio errato                                                  18
180500000211TIBS13R- Codice padre già presente come codice figlio con questo legame        19
180600991014TIBS13R- Il codice figlio non può essere uguale al codice padre                20
180700000211TIBS13R- Codice padre obbligatorio                                             21
180800000807TIBS13R- Codice figlio obbligatorio                                            22
180900000807TIBS13R- Tipo legame non autorizzato.                                          23
181000060113TIBS13R- Utente non abilitato alla gestione dei clienti scelti.                24
181100000808TIBS13R- Richiesta inserimento per dati già esistenti, possibile solo modifica 25
181200020211TIBS13R- Codice figlio già presente nelle abilitazioni.                        26
181300060113TIBS13R- Errore in caricamento dati utente, avvisare il CED                    27
181400090806TIBS13R- Codice unificante risulta già padre su legame di tipo analogo.        28
181500150505TIBS13R- Non ammessa una Data Decorrenza antecedente la data Odierna.          29
181600090806                                                                               30
181700970526** FUD - FUNZIONI
181800970526                   -        (S/N)=N                        funz.01¬
181900970526                   -        (S/N)=N                        funz.02¬
182000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
182100970526                   -        (S/N)=N                        funz.04¬
182200970526                   -        (S/N)=N                        funz.05¬
182300970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
182400970527                   -        (S/N)=N                        funz.07¬
182500970527                   -        (S/N)=N                        funz.08¬
182600970527                   -        (S/N)=N                        funz.09¬
182700970527                   -        (S/N)=N                        funz.10¬
182800970526                   -        (S/N)=N                        funz.11¬
182900970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
183000970526                   -        (S/N)=N                        funz.13¬
183100970526                   -        (S/N)=N                        funz.14¬
183200970526                   -        (S/N)=N                        funz.15¬
183300970526                   -        (S/N)=N                        funz.16¬
183400970526                   -        (S/N)=N                        funz.17¬
183500970526                   -        (S/N)=N                        funz.18¬
183600970526                   -        (S/N)=N                        funz.19¬
183700970526                   -        (S/N)=N                        funz.20¬
183800970526                   -        (S/N)=N                        funz.21¬
183900970526                   -        (S/N)=N                        funz.22¬
184000970526                   -        (S/N)=N                        funz.23¬
184100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
