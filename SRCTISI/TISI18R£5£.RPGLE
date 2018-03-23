000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020418      *------------------------------------------------------------------------*
000300070622      * CAPPARIO - GESTIONE TERMINAL                       - ROUTINE
000400020418      *------------------------------------------------------------------------*
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970528     FTISI18D   CF   E             WORKSTN INFDS(DEVDS1)
000900970528     F                                     SFILE(SI18DMS:MSGULT)
001000970528     FAZORG01L  IF   E           K DISK
001100970527     F*------------
001200970528     F* ECCEZIONI TERMINAL
001300970527     F*------------
001400970618     FAZCAE01L  UF A E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100131015     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500131015     D MSG             S             78    DIM(28) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* DESCRIZIONI TASTI FUNZIONALI
002800970526     D*------------
002900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* FUNZIONI DA CARICARE A VIDEO
003200970526     D*------------
003300970526     D FUV             S             19    DIM(24)
003400970526     D*------------------------------------------------------------------------*
003500970526     D* INPUT
003600970526     D*------------------------------------------------------------------------*
003700970526     D*------------
003800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003900970526     D*------------
004000970526     D FUDDS           DS
004100970526     D  FUDDE1                 1      1
004200970526     D  FUDFUN                 2      3
004300970526     D  FUDDE2                 4     19
004400970526     D  FUDFI0                20     34
004500970526     D  FUDATT                35     35
004600970526     D  FUDFI1                36     80
004700970526     D  FUDDES                 1     19
004800970526     D  FUDRIG                 1     80
004900970526     D*------------
005000970526     D* DS GESTIONE TASTI FUNZIONALI
005100970526     D*------------
005200970526     D DSKEY         E DS
005300970526     D DEVDS1          DS
005400970526     D  KEY                  369    369
005500970526     D*------------
005600970526     D* DS "XMSG" - GESTIONE MESSAGGI
005700970526     D*------------
005800970526     D DSMSG         E DS
005900970528     D*------------------
006000970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006100970528     D*------------------
006200970528     D WLBDA8          DS                  INZ
006300970528     D  G08DAT                 1      8  0
006400970528     D  G08INV                 9     16  0
006500970528     D  G08ERR                17     17
006600970528     D  G08TGI                18     22  0
006700970526     D*------------
006800970526     D* DS DI PROCEDURA
006900970526     D*------------
007000970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
007100970526     D*------------
007200970526     D* ARCHITETTURA
007300970526     D*------------
007400970526     D KPJBA         E DS
007500020506
007600020506     d wcaetfa         s              3
007700020506     d wcaetfe         s              3
007800020510     d sav_viddsc      s                   like(viddsc)
007900020510     d sav_vidtfe      s                   like(vidtfe)
008000020516     d sav_tfe1        s                   like(caetfe)
008100020516     d sav_tfe2        s                   like(caetfe)
008200020516     d ok_date         s              1    inz('0')
008300040624     d WFORZ           s              1
008400090115     d forzater        s              1
008500120214     d forzaSCA        s              1
008600120216     d Dataiso         s               d   datfmt(*iso)
008700120216     d datam1          s              8  0
008800970526     C*------------------------------------------------------------------------*
008900970526     C* MAIN LINES
009000970526     C*------------------------------------------------------------------------*
009100970526     C*
009200970526     C* NON CI SONO ERRORI IN ENTRATA
009300970526IF  1C     ERRL00        IFEQ      '0'
009400970526     C*
009500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009900970526     C*
010000970526     C* EMETTE FORMATO VIDEO
010100970526     C                   EXSR      BUTFMR
010200970526     C*
010300970526     C* GESTIONE TASTI FUNZIONALI
010400970526     C                   EXSR      FUNCON
010500970526E   2C                   ENDDO
010600970526     C*
010700970526     C* CI SONO ERRORI IN ENTRATA
010800970527X   1C                   ELSE
010900970526     C                   MOVEL     '1'           D00ERR
011000970526     C                   MOVEL     DSMSMS        D00MSG
011100970526E   1C                   ENDIF
011200970526     C* FINE PGM
011300970526     C                   SETON                                        LR
011400970526     C*------------------------------------------------------------------------*
011500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011600970526     C*------------------------------------------------------------------------*
011700970526     C     BUTFMR        BEGSR
011800970526     C*
011900970526     C* EMETTE LA TESTATA
012000970528     C                   WRITE     SI18D01
012100970526     C*
012200970526     C* SPEDISCE EVENTUALE MESSAGGIO
012300970526     C                   EXSR      SNDMSG
012400970526     C*
012500970526     C* EMETTE LE FUNZIONI
012600970528     C                   WRITE     SI18D02
012700970526     C*
012800970526     C* EMETTE IL VIDEO
012900970528     C                   EXFMT     SI18D03
013000970526     C*
013100970526     C* REIMPOSTA FLAG DI ERRORE
013200970526     C                   MOVEL     '0'           ERRL00
013300970526     C*
013400970526     C* REIMPOSTA INDICATORI DI ERRORE
013500970526     C                   SETOFF                                         5859
013600970526     C                   SETOFF                                       606162
013700970526     C                   SETOFF                                       636465
013800970526     C                   SETOFF                                       666768
013900970526     C                   SETOFF                                       697071
014000970526     C                   SETOFF                                       727374
014100970526     C                   SETOFF                                       757677
014200970526     C                   SETOFF                                       787980
014300970526     C                   SETOFF                                       818283
014400970526     C                   SETOFF                                       848586
014500970526     C                   SETOFF                                       878889
014600970526     C*
014700970526     C                   ENDSR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015000970526     C*------------------------------------------------------------------------*
015100970526     C     SNDMSG        BEGSR
015200970526     C*
015300970526     C* PULISCE IL SFILE MESSAGGI
015400970526     C                   MOVEL     '1'           *IN40
015500970528     C                   WRITE     SI18DMC
015600970526     C                   MOVEL     '0'           *IN40
015700970526     C*
015800970526     C* VISUALIZZA IL MESSAGGIO
015900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016000970526     C                   CALL      'XMSG'        XMSGPA
016100970526IF  1C     DSMSER        IFEQ      *ZEROS
016200970526     C                   MOVEL     DSMSKE        MSGKEY
016300970526E   1C                   ENDIF
016400970526     C                   Z-ADD     1             MSGULT
016500970528     C                   WRITE     SI18DMS
016600970528     C                   WRITE     SI18DMC
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
018000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
018100970526IF  1C     DEPOP1        IFEQ      'O05'
018200970526     C                   MOVEL     '1'           FINL00
018300970526E   1C                   ENDIF
018400970526     C*
018500970526     C                   ENDSR
018600970526     C*------------------------------------------------------------------------*
018700970526     C* F03GES - GESTIONE F03 -> FINE
018800970526     C*------------------------------------------------------------------------*
018900970526     C     F03GES        BEGSR
019000970526     C*
019100970526     C* IMPOSTA "PREMUTO F03"
019200131015     c                   unlock    azcae01l
019300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019400970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800000515     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F06GES        BEGSR
020100970526     C*
020200970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
020300970526IF  1C     DEPOP1        IFEQ      'F06'
020400970526     C                   EXSR      INSERI
020500970526E   1C                   ENDIF
020600970526IF  1C     DEPOP1        IFEQ      'O02'
020700970526     C                   EXSR      MODIFI
020800970526E   1C                   ENDIF
020900970526IF  1C     DEPOP1        IFEQ      'O04'
021000970526     C                   EXSR      ANNULL
021100970526E   1C                   ENDIF
021200970526IF  1C     DEPOP1        IFEQ      'O07'
021300970526     C                   EXSR      RIPRIS
021400970526E   1C                   ENDIF
021500131015IF  1C     DEPOP1        IFEQ      'O+1'
021600131015     c                   unlock    azcae01l
021700131015
021800131015     C                   call      'TISI18R2'
021900131015     c                   parm                    kpjba
022000131015     c                   parm                    dssi00
022100131015     C*
022200131015     c                   if        d00err=' '
022300131015     C                   MOVEL     MSG(3)        D00MSG
022400131015     c                   else
022500131015     C                   MOVEL     MSG(28)       D00MSG
022600131015     c                   endif
022700131015     C                   MOVEL     '1'           FINL00                         *FINE PGM
022800131015E   1C                   ENDIF
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* F12GES - GESTIONE F12 -> RITORNO
023300970526     C*------------------------------------------------------------------------*
023400970526     C     F12GES        BEGSR
023500970526     C*
023600970526     C* IMPOSTA "PREMUTO F12"
023700131015     c                   unlock    azcae01l
023800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023900970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
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
025600000515     C                   EXSR      DECODI
025700970526     C*
025800120216     c* Se inserimento di terminal PAR/ARR e profilo EDP
025900120216     c* si fa scadere eventuale terminal decorrente
026000120216     c                   if        winizioprf='EDP' and
026100120216     c                             (videpa='P' or videpa='A')
026200120216     c                   exsr      scaden
026300120216     c                   endif
026400970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026600970618     C                   MOVEL     VIDEPA        CAEEPA                         *CHIAVE
026700970618     C                   MOVEL     TFPVID        CAETFP
026800970618     C                   MOVEL     TFAVID        CAETFA
026900970618     C                   Z-ADD     DDEVID        CAEDDE
027000970618     C                   EXSR      IMPCAE                                       *ALTRI CAMPI
027100970618     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
027200970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
027300970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
027400970618     C                   WRITE     AZCAE000
027500000427     C*
027600000427     C                   EXSR      LETCAE
027700970526     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
027800120831     C*
027900120903     C* se il tipo terminal inserito è £6
028000120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
028100120831     C                   IF        CAEEPA = '6' and
028200120911     C                             VIDCRC = 'SI'
028300120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
028400120903     C* lo inserisco
028500120903     C                   WRITE     AZCAE000
028600120831     C                   ENDIF
028700970602     C*
028800970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
028900970602     C                   EXSR      AZZVID
029000970526E   1C                   ENDIF
029100970526     C*
029200970526     C                   ENDSR
029300120831     C*------------------------------------------------------------------------*
029400120831     C* SCADEN - OPERAZIONI per far scadere il terminal già decorrente
029500120831     C*------------------------------------------------------------------------*
029600120831     C     SCADEN        BEGSR
029700120216     C                   MOVEL     VIDEPA        KCEEPA
029800120216     C                   MOVEL     TFPVID        KCETFP
029900120216     C                   MOVEL     TFAVID        KCETFA
030000120216     C     KE2CAE        SETLL     AZCAE01L
030100120216     C     KE2CAE        READE     AZCAE01L
030200120216     C                   DOW       not %eof(azcae01l)
030300120216     C     CAEATB        IFEQ      *BLANKS
030400120216     C     DDEVID        andgt     CAEDDE                                       *DECORRENZA
030500120216     C     DDEVID        ANDLE     CAEDSC
030600120216     c* data decorrenza del nuovo record - un giorno = data scadenza del record
030700120216     c* già esistente
030800120216     c                   move      ddevid        dataiso
030900120216     c                   subdur    1:*d          dataiso
031000120216     c                   move      dataiso       caedsc
031100120216     c                   update    azcae000
031200120216     c                   leave
031300120216     c                   endif
031400120216     C     KE2CAE        READE     AZCAE01L
031500120216     c                   enddo
031600120216     c                   endsr
031700970526     C*------------------------------------------------------------------------*
031800970526     C* MODIFI - OPERAZIONI DI MODIFICA
031900970526     C*------------------------------------------------------------------------*
032000970526     C     MODIFI        BEGSR
032100970526     C*
032200970526     C* CONTROLLA I CAMPI IMMESSI
032300970526     C                   EXSR      DECODI
032400970526     C*
032500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032700970618     C                   EXSR      LETCAE
032800030310     C*
032900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
033000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033100970618     C                   EXSR      IMPCAE                                       *IMPOSTA I CAMPI
033200970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
033300970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
033400970618     C                   UPDATE    AZCAE000
033500000427     C*
033600970526     C                   MOVEL     MSG(3)        D00MSG
033700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033800120903     C*
033900120903     C* se il tipo terminal inserito è £6
034000120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
034100120903     C                   IF        CAEEPA = '6' and
034200120911     C                             VIDCRC = 'SI'
034300120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
034400120903     C                   EXSR      LETCAE2
034500120903     C* lo modifico
034600120903     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
034700120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034800120903     C                   EXSR      IMPCAE                                       *IMPOSTA I CAMPI
034900120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
035000120903     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
035100120903     C                   UPDATE    AZCAE000
035200120903     C                   ENDIF
035300120903     C                   ENDIF
035400120903     C*
035500970526E   2C                   ENDIF
035600970526E   1C                   ENDIF
035700970526     C*
035800970526     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
036100970526     C*------------------------------------------------------------------------*
036200970526     C     ANNULL        BEGSR
036300970526     C*
036400040624     c                   exsr      sr_atten
036500040624     c*
036600970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
036700000602     C                   MOVEL     '0'           ANNDLT            1
036800970618     C                   EXSR      LETCAE
036900970527     C*
037000970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
037100970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037200970618IF  2C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
037300970618IF  3C     CAEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
037400970618     C                   MOVEL     'A'           CAEATB                         *FLAG ANNULLAMENTO
037500970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
037600970618     C                   UPDATE    AZCAE000                                     *ANNULLA
037700970526X   3C                   ELSE                                                   *MAI TRASMESSO
037800970618     C                   DELETE    AZCAE000                                     *CANCELLA
037900000602     C                   MOVEL     '1'           ANNDLT
038000970526E   3C                   ENDIF
038100970526E   2C                   ENDIF
038200970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
038300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
038400120903     C*
038500120903IF  2C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
038600120903     C* se il tipo terminal inserito è £6
038700120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
038800120903IF   C                   IF        CAEEPA = '6' and
038900120911     C                             VIDCRC = 'SI'
039000120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
039100120903     C                   EXSR      LETCAE2
039200120903IF  3C     CAEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
039300120903     C* lo annullo logicamente
039400120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039500120903     C                   MOVEL     'A'           CAEATB                         *FLAG ANNULLAMENTO
039600120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
039700120903     C                   UPDATE    AZCAE000
039800120903     C                   ENDIF
039900120903     C                   ELSE
040000120903     C* lo cancello
040100120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040200120903     C                   DELETE    AZCAE000
040300120903     C                   ENDIF
040400120903     C                   ENDIF
040500120903     C                   ENDIF
040600120903     C                   ENDIF
040700120903     C*
040800970526E   1C                   ENDIF
040900970526     C*
041000970526     C                   ENDSR
041100970526     C*------------------------------------------------------------------------*
041200970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
041300970526     C*------------------------------------------------------------------------*
041400970526     C     RIPRIS        BEGSR
041500970526     C*
041600970526     C* CONTROLLA I CAMPI IMMESSI
041700970526     C                   EXSR      DECODI
041800970526     C*
041900970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
042000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042100970618     C                   EXSR      LETCAE
042200970526     C*
042300970526     C* IMPOSTA IL RECORD DA SCRIVERE
042400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042500970618     C                   MOVEL     *BLANKS       CAEATB                         *FLAG ANNULLAMENTO
042600970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
042700970618     C                   UPDATE    AZCAE000                                     *ANNULLA
042800970526E   2C                   ENDIF
042900970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
043000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
043100120903     C*
043200120903     C* se il tipo terminal inserito è £6
043300120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
043400120903     C                   IF        CAEEPA = '6' and
043500120911     C                             VIDCRC = 'SI'
043600120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
043700120903     C                   EXSR      LETCAE2
043800120903     C* lo ripristino
043900120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
044000120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
044100120903     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
044200120903     C                   UPDATE    AZCAE000
044300120903     C                   ENDIF
044400120903     C                   ENDIF
044500120903     C*
044600970526E   1C                   ENDIF
044700970526     C*
044800970526     C                   ENDSR
044900970526     C*------------------------------------------------------------------------*
045000970618     C* LETCAE - LEGGE IL RECORD PERL'AGGIORNAMENTO
045100970526     C*------------------------------------------------------------------------*
045200970618     C     LETCAE        BEGSR
045300970526     C*
045400000427     C     DEPOP1        IFNE      'F06'
045500980127     C                   MOVEL     D00EPA        KCEEPA
045600980127     C                   MOVEL     D00ETP        KCETFP
045700980127     C                   MOVEL     D00ETA        KCETFA
045800980127     C                   Z-ADD     D00EDD        KCEDDE
045900970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
046000970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
046100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
046200970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
046300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
046400970526E   1C                   ENDIF
046500970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
046600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
046700970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
046800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
046900970526E   1C                   ENDIF
047000000427     C                   ENDIF
047100970526     C*
047200970526     C                   ENDSR
047300120903     C*------------------------------------------------------------------------*
047400120903     C* LETCAE2 - LEGGE IL RECORD tipo terminal C corrispondente al £6 PER L'AGGIORNAMENTO
047500120903     C*------------------------------------------------------------------------*
047600120903     C     LETCAE2       BEGSR
047700120903     C*
047800120903     C* LETTURA AZCAE01L - COMPLETA
047900120903     C     K04CAE01      KLIST
048000120903     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
048100120903     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
048200120903     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
048300120903     C                   KFLD                    CAEDDE                         *DECORRENZA
048400120903     C*
048500120903     C     K04CAE01      CHAIN     AZCAE01L                           9998
048600120903IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
048700120903     C                   MOVEL     '1'           ERRL00                         *ERRORE
048800120903     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
048900120903     C                   MOVEL     '1'           FINL00                         *FINE PGM
049000120903E   1C                   ENDIF
049100120903IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
049200120903     C                   MOVEL     '1'           ERRL00                         *ERRORE
049300120903     C                   MOVEL     MSG(27)       D00MSG                         *MESSAGGIO
049400120903     C                   MOVEL     '1'           FINL00                         *FINE PGM
049500120903E   1C                   ENDIF
049600120903     C*
049700120903     C                   ENDSR
049800970526     C*------------------------------------------------------------------------*
049900970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
050000970526     C*------------------------------------------------------------------------*
050100970526     C     DECODI        BEGSR
050200970528     C*--------------------
050300970528     C* PARTENZA/ARRIVO
050400970528     C*--------------------
050500970528IF  1C     ERRL00        IFEQ      '0'
050600970528IF  2C     VIDEPA        IFEQ      'P'
050700970528     C     VIDEPA        OREQ      'A'
050800020418     C     VIDEPA        OREQ      '6'
050900030310     C     VIDEPA        OREQ      'C'
051000060706     C     VIDEPA        OREQ      'O'
051100140117     C     VIDEPA        OREQ      'J'
051200970528X   2C                   ELSE
051300970528     C                   MOVEL     '1'           ERRL00
051400970528     C                   SETON                                        60
051500970528     C                   MOVEL     MSG(11)       DSMSMS
051600970528E   2C                   ENDIF
051700970528E   1C                   ENDIF
051800970528     C*--------------------
051900970528     C* TERMINAL / LINEA DI PARTENZA
052000970528     C*--------------------
052100970618     C     *LIKE         DEFINE    VIDTFP        TFPVID
052200020418IF  1c                   if        errl00 = *off
052300020418     c                   clear                   vddtfp
052400030310     c                   if        videpa = '6' and vidtfp > *zeros or
052500140117     c                             videpa = 'C' and vidtfp > *zeros or
052600140117     c                             videpa = 'J' and vidtfp > *zeros
052700030310     c                   eval      errl00 = *on
052800020418     c                   eval      *in61 = *on
052900020418     c                   eval      dsmsms = msg(21)
053000020418     c                   endif
053100020418     c                   endif
053200020418
053300970528IF  1C     ERRL00        IFEQ      '0'
053400970618     C                   MOVEL     *BLANKS       VDDTFP
053500140117     c                   if        (videpa ='A' or videpa='6' or videpa='C' or
053600140117     c                              videpa ='J' ) and
053700020418     c                             (vidtfp = *blanks or vidtfp = *zeros)
053800970618     C                   MOVEL     *ZEROS        TFPVID
053900970618X   2C                   ELSE
054000970618     C                   MOVEL     VIDTFP        WLIN
054100970618     C                   MOVEL     *BLANKS       WDLIN
054200970618     C                   EXSR      CHKLIN
054300970618     C                   MOVEL     WLIN          VIDTFP
054400970618     C                   MOVEL     WDLIN         VDDTFP
054500970618     C                   MOVEL     VIDTFP        TFPVID
054600970618IF  3C     ERRL00        IFEQ      '1'
054700970528     C                   SETON                                        61
054800970618     C                   MOVEL     MSG(12)       DSMSMS
054900970618E   3C                   ENDIF
055000970618E   2C                   ENDIF
055100970528E   1C                   ENDIF
055200970528     C*--------------------
055300970528     C* TERMINAL / LINEA DI ARRIVO
055400970528     C*--------------------
055500970618     C     *LIKE         DEFINE    VIDTFA        TFAVID
055600970528IF  1C     ERRL00        IFEQ      '0'
055700970618     C                   MOVEL     *BLANKS       VDDTFA
055800140117     c                   if        (videpa ='P' or videpa='O') and
055900060706     c                             (vidtfa = *blanks or vidtfa = *zeros)
056000970618     C                   MOVEL     *ZEROS        TFAVID
056100970618X   2C                   ELSE
056200970528     C                   MOVEL     VIDTFA        WLIN
056300970528     C                   MOVEL     *BLANKS       WDLIN
056400970528     C                   EXSR      CHKLIN
056500970528     C                   MOVEL     WLIN          VIDTFA
056600970528     C                   MOVEL     WDLIN         VDDTFA
056700970618     C                   MOVEL     VIDTFA        TFAVID
056800970618IF  3C     ERRL00        IFEQ      '1'
056900970528     C                   SETON                                        62
057000970528     C                   MOVEL     MSG(13)       DSMSMS
057100970618E   3C                   ENDIF
057200970618E   2C                   ENDIF
057300970528E   1C                   ENDIF
057400970618     C*
057500060706     C* NO ECCEZIONE IN PARTENZA o per ottico
057600970618IF  1C     ERRL00        IFEQ      '0'
057700140117     c                   if        videpa='P' or videpa='O'
057800970618IF  3C     VIDTFP        IFGT      *ZEROS
057900970618     C     VIDTFA        ANDGT     *ZEROS
058000970618     C     VIDTFE        ANDGT     *ZEROS
058100060706     C                   SETON                                        62
058200970618     C                   MOVEL     MSG(19)       DSMSMS
058300970618     C                   MOVEL     '1'           ERRL00
058400970618E   3C                   ENDIF
058500970618E   2C                   ENDIF
058600970618E   1C                   ENDIF
058700970528     C*--------------------
058800970528     C* DATA DECORRENZA
058900970528     C*--------------------
059000970528     C     *LIKE         DEFINE    VIDDDE        DDEVID
059100970528IF  1C     ERRL00        IFEQ      '0'
059200970528     C                   Z-ADD     VIDDDE        WDA8
059300970528     C                   Z-ADD     *ZEROS        DA8W
059400970528     C                   EXSR      CHKDA8
059500970528     C                   Z-ADD     WDA8          VIDDDE
059600090115     c* pulisco flag di forzatura sulla date se cambiano
059700090115     c                   if        da8w<>ddevid
059800090115     c                   clear                   forzater
059900120216     c                   setoff                                       42
060000090115     c                   endif
060100970528     C                   Z-ADD     DA8W          DDEVID
060200970528IF  2C     ERRL00        IFEQ      '1'
060300970528     C                   SETON                                        63
060400970528     C                   MOVEL     MSG(14)       DSMSMS
060500970528E   2C                   ENDIF
060600970528E   1C                   ENDIF
060700970528     C* INSERIMENTO -> RECORD INESISTENTE
060800970528IF  1C     ERRL00        IFEQ      '0'
060900970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
061000970618     C                   MOVEL     VIDEPA        KCEEPA
061100970618     C                   MOVEL     TFPVID        KCETFP
061200970618     C                   MOVEL     TFAVID        KCETFA
061300970618     C                   Z-ADD     DDEVID        KCEDDE
061400970618     C     KEYCAE        CHAIN(N)  AZCAE01L                           99
061500970528IF  3C     *IN99         IFEQ      *OFF
061600970528     C                   MOVEL     '1'           ERRL00
061700970528     C                   MOVEL     MSG(8)        DSMSMS
061800970528     C                   SETON                                        606162
061900970528     C                   SETON                                        63
062000970528E   3C                   ENDIF
062100970528E   2C                   ENDIF
062200970528E   1C                   ENDIF
062300970528     C*--------------------
062400970528     C* DATA SCADENZA
062500970528     C*--------------------
062600970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
062700970528IF  1C     ERRL00        IFEQ      '0'
062800970528     C                   Z-ADD     VIDDSC        WDA8
062900970528     C                   Z-ADD     *ZEROS        DA8W
063000970528     C                   EXSR      CHKDA8
063100970528     C                   Z-ADD     WDA8          VIDDSC
063200090115     c                   if        da8w<>dscvid
063300090115     c                   clear                   forzater
063400120216     c                   setoff                                       42
063500120214     c                   clear                   forzaSCA
063600090115     c                   endif
063700970528     C                   Z-ADD     DA8W          DSCVID
063800970528IF  2C     ERRL00        IFEQ      '1'
063900970528     C                   SETON                                        64
064000970528     C                   MOVEL     MSG(14)       DSMSMS
064100970528E   2C                   ENDIF
064200970528E   1C                   ENDIF
064300970528     C* SCADENZA > DECORRENZA
064400970528IF  1C     ERRL00        IFEQ      '0'
064500970528IF  2C     DSCVID        IFLT      DDEVID
064600970619     C                   SETON                                        64
064700970528     C                   MOVEL     MSG(15)       DSMSMS
064800970528     C                   MOVEL     '1'           ERRL00
064900970528E   2C                   ENDIF
065000970528E   1C                   ENDIF
065100970528     C* SCADENZA >= DATA CORRENTE
065200050412     c* Per EDP e per terminal partenza e £6, non faccio il controllo
065300970528IF  1C     ERRL00        IFEQ      '0'
065400050412     c                   if        (winizioprf='EDP' and videpa='P') or
065500050412     c                             (winizioprf='EDP' and videpa='6')
065600050412     c                   else
065700120214     c
065800120214     c* Per EDP forzatura
065900970528IF  2C     DEPOP1        IFEQ      'O02'
066000970528     C     DEPOP1        OREQ      'O07'
066100970528     C     DEPOP1        OREQ      'F06'
066200970528IF  3C     DSCVID        IFLT      DATCOR
066300120214     c     forzasca      andeq     ' '
066400970619     C                   SETON                                        64
066500970528     C                   MOVEL     MSG(16)       DSMSMS
066600120214     c                   if         winizioprf='EDP'
066700120214     c                   eval      forzaSCA='S'
066800120214     C                   MOVEL     MSG(26)       DSMSMS
066900120214     c                   endif
067000120214     c
067100970528     C                   MOVEL     '1'           ERRL00
067200970528E   3C                   ENDIF
067300050412E   3C                   ENDIF
067400970528E   2C                   ENDIF
067500970528E   1C                   ENDIF
067600970528     C*---
067700970528     C* DECORRENZA + SCADENZA
067800970528     C*---
067900970528     C* ACCAVALLAMENTO DATI
068000970528IF  1C     ERRL00        IFEQ      '0'
068100090115     c     forzater      andeq     ' '
068200970618     C                   MOVEL     VIDEPA        KCEEPA
068300970618     C                   MOVEL     TFPVID        KCETFP
068400970618     C                   MOVEL     TFAVID        KCETFA
068500970618     C     KE2CAE        SETLL     AZCAE01L
068600970618     C     KE2CAE        READE(N)  AZCAE01L                               99
068700970528DO  2C     *IN99         DOWEQ     *OFF
068800970528IF  3C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
068900970618     C     DDEVID        ANDNE     CAEDDE                                       *RECORD IN LINEA
069000970528     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
069100970618IF  4C     CAEATB        IFEQ      *BLANKS
069200970618IF  5C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
069300970618     C     DDEVID        ANDLE     CAEDSC
069400970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
069500970618     C     DSCVID        ANDLE     CAEDSC
069600970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
069700970618     C     CAEDSC        ANDLE     DSCVID
069800970528     C                   SETON                                        606162
069900970619     C                   SETON                                        6364
070000970528     C                   MOVEL     MSG(17)       DSMSMS
070100970528     C                   MOVEL     '1'           ERRL00
070200120216     c                   select
070300120216     c                   when      winizioprf='EDP' and videpa='6'
070400120216     c                   eval      forzater='S'
070500120216     C                   MOVEL     MSG(10)       DSMSMS
070600120216     c                   when      winizioprf='EDP' and
070700120216     c                             (videpa='P' or videpa='A')
070800120216     c                             and ddevid>caedde
070900120216     c                   eval      forzater='S'
071000120216     c                   seton                                        42
071100120216     C                   clear                   DSMSMS
071200120216     c
071300120216     c                   endsl
071400090115     c
071500970528     C                   LEAVE
071600970528E   5C                   ENDIF
071700970528E   4C                   ENDIF
071800970528E   3C                   ENDIF
071900970618     C     KE2CAE        READE(N)  AZCAE01L                               99
072000970528E   2C                   ENDDO
072100970528E   1C                   ENDIF
072200970528     C*--------------------
072300970528     C* TERMINAL / LINEA ECCEZIONE
072400970528     C*--------------------
072500970528IF  1C     ERRL00        IFEQ      '0'
072600970528     C                   MOVEL     VIDTFE        WLIN
072700970528     C                   MOVEL     *BLANKS       WDLIN
072800970528     C                   EXSR      CHKLIN
072900970528     C                   MOVEL     WLIN          VIDTFE
073000970528     C                   MOVEL     WDLIN         VDDTFE
073100970528IF  2C     ERRL00        IFEQ      '1'
073200970528     C                   SETON                                        65
073300970528     C                   MOVEL     MSG(18)       DSMSMS
073400970528E   2C                   ENDIF
073500970528E   1C                   ENDIF
073600970528     C*
073700970618     C* SE ECCEZIONE IN ARRIVO, ECCEZIONE <> ARRIVO STANDARD
073800970618IF  1C     ERRL00        IFEQ      '0'
073900970618IF  2C     VIDEPA        IFEQ      'A'
074000970618IF  3C     VIDTFP        IFGT      *ZEROS
074100970618     C     VIDTFA        ANDGT     *ZEROS
074200970618     C     VIDTFE        ANDGT     *ZEROS
074300970618     C                   MOVEL     'A'           KCEEPA
074400970618     C                   Z-ADD     *ZEROS        KCETFP
074500970618     C                   MOVEL     TFAVID        KCETFA
074600970618     C     KE2CAE        SETLL     AZCAE01L
074700970618     C     KE2CAE        READE(N)  AZCAE01L                               99
074800970618DO  4C     *IN99         DOWEQ     *OFF
074900970618IF  5C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
075000970618IF  5C     CAETFE        IFEQ      VIDTFE
075100970618IF  7C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
075200970618     C     DDEVID        ANDLE     CAEDSC
075300970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
075400970618     C     DSCVID        ANDLE     CAEDSC
075500970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
075600970618     C     CAEDSC        ANDLE     DSCVID
075700970618     C                   SETON                                        606162
075800970618     C                   SETON                                        63
075900970618     C                   MOVEL     MSG(20)       DSMSMS
076000970618     C                   MOVEL     '1'           ERRL00
076100970618     C                   LEAVE
076200970618E   7C                   ENDIF
076300970618E   6C                   ENDIF
076400970618E   5C                   ENDIF
076500970618     C     KE2CAE        READE(N)  AZCAE01L                               99
076600970618E   4C                   ENDDO                                                  *FINE TERM.ARR.STAND
076700970618E   3C                   ENDIF
076800970618E   2C                   ENDIF
076900970618E   1C                   ENDIF
077000020507
077100020507      * controlli per incongruenze tra record di tipo 'P' (partenza) e record di tip '6' (£6)
077200020507 b1  c                   if        errl00 = *off and
077300020507     c                             (videpa = 'P' or videpa = '6')
077400020507     c                   exsr      chk£6
077500020507 e1  c                   endif
077600040624     c
077700040624     c                   exsr      sr_atten
077800970618     C*
077900970528     C                   ENDSR
078000040624     C*------------------------------------------------------------------------*
078100040624     C* SR_atten -  Emissione messaggio informativo
078200040624     C*------------------------------------------------------------------------*
078300040624     C     SR_atten      BEGSR
078400040624     c     ERRL00        ifeq      '0'
078500040624     c     videpa        andeq     'A'
078600040624     c     funkey        andeq     '06'
078700040624     c     wforz         andeq     *blanks
078800040624     c* modificata linea o data
078900040624     c     depop1        ifeq      'O02'
079000040624     c     sav_vidtfe    andne     vidtfe
079100040624     c     depop1        oreq      'O02'
079200040624     c     sav_viddsc    andne     viddsc
079300040624     c* ripristino
079400040624     c     depop1        oreq      'O07'
079500040624     c* cancellazione
079600040624     c     depop1        oreq      'O04'
079700040624     c                   eval      wforz='F'
079800040624     c                   eval      errl00='1'
079900040624     c                   seton                                        41
080000040624     c                   endif
080100040624     c                   endif
080200040624     c                   endsr
080300970528     C*------------------------------------------------------------------------*
080400970528     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
080500970528     C*------------------------------------------------------------------------*
080600970528     C     CHKLIN        BEGSR
080700970528     C*
080800970618     C     *LIKE         DEFINE    VIDTFA        WLIN
080900970618     C     *LIKE         DEFINE    VDDTFA        WDLIN
081000970528     C*
081100970528     C* RICHIESTO ELENCO
081200970528     C                   SETOFF                                           28
081300970528     C     '?'           SCAN      WLIN                                   28
081400970528IF  1C     *IN28         IFEQ      *ON
081500970528     C                   MOVEL     *BLANKS       §COD1             3
081600970528     C                   MOVEL     *BLANKS       §TIP              1
081700970528     C                   MOVEL     *BLANKS       §DES1            25
081800971203     C                   MOVEL     *BLANKS       §PRA              3
081900971203     C                   CALL      'TNSD24R'
082000970528     C                   PARM                    §COD1
082100970528     C                   PARM                    §TIP
082200970528     C                   PARM                    §DES1
082300971203     C                   PARM                    §PRA
082400970528     C                   MOVEL     §COD1         WLIN
082500970528     C                   MOVEL     §DES1         WDLIN
082600970528E   1C                   ENDIF
082700970528     C*
082800970528     C* SOLO CARATTERI NUMERICI
082900970528     C                   SETOFF                                       28
083000970528     C                   TESTN                   WLIN                 28
083100970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
083200970528     C                   MOVEL     '1'           ERRL00
083300970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
083400970528     C                   MOVE      WLIN          W001              1
083500970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
083600970528     C                   MOVEL     '1'           ERRL00
083700970528X   2C                   ELSE                                                   *NUMERO
083800970528     C*
083900970528     C* CONTROLLA ESISTENZA CODICE
084000970528     C                   MOVEL     WLIN          KORFIL
084100970528     C     KEYORG        CHAIN     AZORG01L                           99
084200970528IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
084300970528     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
084400970528     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
084500970528     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
084600970528X   3C                   ELSE
084700970528     C                   MOVEL     '1'           ERRL00
084800970528E   3C                   ENDIF
084900970528E   2C                   ENDIF
085000970528E   1C                   ENDIF
085100970528     C*
085200970528     C                   ENDSR
085300970528     C*------------------------------------------------------------------------*
085400970528     C* CHKDA8 - CONTROLLA LA DATA (8)
085500970528     C*------------------------------------------------------------------------*
085600970528     C     CHKDA8        BEGSR
085700970528     C*
085800970528     C     *LIKE         DEFINE    VIDDDE        WDA8
085900970528     C     *LIKE         DEFINE    VIDDDE        DA8W
086000970528     C*
086100970528     C                   Z-ADD     WDA8          G08DAT
086200970528     C                   Z-ADD     *ZEROS        G08INV
086300970528     C                   MOVEL     '0'           G08ERR
086400970528     C                   CALL      'XSRDA8'
086500970528     C                   PARM                    WLBDA8
086600970528     C                   Z-ADD     G08DAT        WDA8
086700970528     C                   Z-ADD     G08INV        DA8W
086800970528IF  1C     G08ERR        IFEQ      '1'
086900970528     C                   MOVEL     '1'           ERRL00
087000970528E   1C                   ENDIF
087100970528     C*
087200970528     C                   ENDSR
087300020506      *------------------------------------------------------------------------*
087400080806      * CHK£6  - CONTROLLA INCONGRUENZE TRA RECOD 'A' E RECORD '6'
087500020506      *------------------------------------------------------------------------*
087600020507     c     chk£6         begsr
087700080806      ***
087800020507      * Record tipo 'P'
087900020507      *----------------
088000080806     c***                if        videpa = 'A'
088100080806     c***                eval      kceepa = '6'
088200080806     c***  ke1cae        setll     azcae01l
088300080806     c***                do        *hival
088400080806     c***  ke1cae        reade(n)  azcae01l
088500080806     c***                if        %eof(azcae01l)
088600080806     c***                leave
088700080806     c***                endif
088800080806     c***                if        caeatb<>*blanks
088900080806     c***                iter
089000080806     c***                endif
089100080806     c***
089200080806     c***                movel     caetfa        wcaetfa
089300080806     c***                movel     caetfe        wcaetfe
089400080806      **controllo la congruenza della scadenza tra record 'A' e record '6'
089500080806     c***                if        sav_viddsc <> viddsc and
089600080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid) and
089700080806     c***                          caedsc > dscvid
089800080806     c***                eval      *in64 = *on
089900080806     c***                eval      dsmsms = msg(22)
090000080806     c***                eval      errl00 = *on
090100080806     c***                leave
090200080806     c***                endif
090300080806      **controllo che non esistano ancora dei record di tipo '6'
090400080806     c***                if        sav_vidtfe <> vidtfe and
090500080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid)
090600080806     c***                eval      *in65 = *on
090700080806     c***                eval      dsmsms = msg(23)
090800080806     c***                eval      errl00 = *on
090900080806     c***                leave
091000080806     c***                endif
091100080806     c***                enddo
091200080806      ***
091300080806     c***                endif
091400080806      ***
091500080806      **Record tipo '6'
091600080806      *----------------
091700080806     c***                if        videpa = '6'
091800080806     c***                eval      kceepa = 'A'
091900080806     c***                eval      ok_date = *off
092000080806     c***                clear                   sav_tfe1
092100080806      **controllo il terminal di partenza del p.o. gestito
092200080806     c***                movel     vidtfa        kcetfp
092300080806     c***                clear                   kcetfa
092400080806     c***  ke2cae        setll     azcae01l
092500080806     c***                do        *hival
092600080806     c***  ke2cae        reade(n)  azcae01l
092700080806      **fine file
092800080806     c***                if        %eof(azcae01l)
092900080806     c***                leave
093000080806     c***                endif
093100080806      ***
093200080806     c***                eval      *in63 = *off
093300080806     c***                eval      *in64 = *off
093400080806      **controllo le date
093500080806     c***                if        ddevid >= caedde and dscvid <= caedsc
093600080806     c***                eval      sav_tfe1 = caetfe
093700080806     c***                eval      ok_date = *on
093800080806     c***                leave
093900080806      ***
094000080806      **controllo quale data non corrisponde
094100080806     c***                if        ok_date = *off
094200080806     c***                eval      *in63 = (ddevid < caedde)
094300080806     c***                eval      *in64 = (dscvid > caedsc)
094400080806     c***                endif
094500080806      ***
094600080806     c***                enddo
094700080806      **le date non corrispondono errore
094800080806     c***                if        ok_date = *off and (*in63 or *in64)
094900080806     c***                eval      dsmsms = msg(25)
095000080806     c***                eval      errl00 = *on
095100080806     c***                endif
095200080806
095300080806      **controllo il terminal di partenza del p.o. gestione
095400080806     c***                if        errl00 = *off
095500080806     c***                eval      ok_date = *off
095600080806     c***                clear                   sav_tfe2
095700080806     c***                movel     vidtfe        kcetfp
095800080806     c***                clear                   kcetfa
095900080806     c***  ke2cae        setll     azcae01l
096000080806     c***                do        *hival
096100080806     c***  ke2cae        reade(n)  azcae01l
096200020516      * fine file
096300080806     C***                if        %eof(azcae01l)
096400080806     c***                leave
096500080806     c***                endif
096600080806      ***
096700080806     c***                eval      *in63 = *off
096800080806     c***                eval      *in64 = *off
096900080806      **controllo le date
097000080806     c***                if        ddevid >= caedde and dscvid <= caedsc
097100080806     c***                eval      sav_tfe2 = caetfe
097200080806     c***                eval      ok_date = *on
097300080806     c***                leave
097400080806     c***                endif
097500020516      * controllo quale data non corrisponde
097600080806     c***                if        ok_date = *off
097700080806     c***                eval      *in63 = (ddevid < caedde)
097800080806     c***                eval      *in64 = (dscvid > caedsc)
097900080806     c***                endif
098000080806      ***
098100080806     c***                enddo
098200080806      **le date non corrispondono errore
098300080806     c***                if        ok_date = *off and (*in63 or *in64)
098400080806     c***                eval      dsmsms = msg(25)
098500080806     c***                eval      errl00 = *on
098600080806     c***                endif
098700080806      ***
098800080806     c***                endif
098900080806      ***
099000080806      **controllo se i 2 terminal  corrispondono
099100080806     c***                if        errl00 = *off and
099200080806     c***                          sav_tfe1 <> sav_tfe2
099300080806     c***                eval      *in65 = *on
099400080806     c***                eval      dsmsms = msg(24)
099500080806     c***                eval      errl00 = *on
099600080806     c***                endif
099700080806      ***
099800080806     c***                endif
099900020506
100000020506     c                   endsr
100100970526     C*------------------------------------------------------------------------*
100200970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
100300970526     C*------------------------------------------------------------------------*
100400970526     C     IREVID        BEGSR
100500090115     c                   clear                   forzater
100600120216     c                   setoff                                       42
100700120214     c                   clear                   forzaSCA
100800970526     C* CAMPI
100900970618     C                   MOVEL     CAEEPA        VIDEPA
101000970618     C                   MOVEL     CAETFP        VIDTFP
101100970618     C                   MOVEL     CAETFA        VIDTFA
101200970618     C                   Z-ADD     CAEDDE        G08INV
101300970528     C                   Z-ADD     *ZEROS        G08DAT
101400970528     C                   MOVEL     '3'           G08ERR
101500970528     C                   CALL      'XSRDA8'
101600970528     C                   PARM                    WLBDA8
101700970528     C                   Z-ADD     G08DAT        VIDDDE
101800970618     C                   Z-ADD     CAEDSC        G08INV
101900970528     C                   Z-ADD     *ZEROS        G08DAT
102000970528     C                   MOVEL     '3'           G08ERR
102100970528     C                   CALL      'XSRDA8'
102200970528     C                   PARM                    WLBDA8
102300970528     C                   Z-ADD     G08DAT        VIDDSC
102400970618     C                   Z-ADD     CAETFE        VIDTFE
102500020510
102600020510      * salvo data scadenza e terminal per controlli congruenze tra record
102700020510      * di tipo 'P' e record di tipo '6'
102800020510     c                   eval      sav_viddsc = viddsc
102900020510     c                   eval      sav_vidtfe = vidtfe
103000120903
103100120903     C* metto SI come default per gestione corrispondente rcd C se tipo terminal £6
103200120911     C                   MOVEL     'SI'          VIDCRC
103300970527     C*
103400970526     C                   ENDSR
103500970526     C*------------------------------------------------------------------------*
103600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
103700970526     C*------------------------------------------------------------------------*
103800970526     C     AZZVID        BEGSR
103900970526     C* CAMPI
104000090115     c                   clear                   forzater
104100120216     c                   setoff                                       42
104200120214     c                   clear                   forzaSCA
104300970528     C                   MOVEL     *BLANKS       VIDEPA
104400970618     C                   MOVEL     *BLANKS       VIDTFP
104500970618     C                   MOVEL     *BLANKS       VIDTFA
104600970528     C                   Z-ADD     *ZEROS        VIDDDE
104700970528     C                   Z-ADD     *ZEROS        VIDDSC
104800970618     C                   Z-ADD     *ZEROS        VIDTFE
104900120911     C                   MOVEL     'SI'          VIDCRC
105000970526     C* DECODIFICHE
105100970528     C                   MOVEL     *BLANKS       VDDTFP
105200970528     C                   MOVEL     *BLANKS       VDDTFA
105300970528     C                   MOVEL     *BLANKS       VDDTFE
105400970526     C*
105500970526     C                   ENDSR
105600970526     C*------------------------------------------------------------------------*
105700970618     C* IMPCAE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
105800970526     C*------------------------------------------------------------------------*
105900970618     C     IMPCAE        BEGSR
106000970526     C*
106100970618     C                   Z-ADD     DSCVID        CAEDSC
106200970618     C                   Z-ADD     VIDTFE        CAETFE
106300970526     C*
106400970526     C                   ENDSR
106500970526     C*------------------------------------------------------------------------*
106600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
106700970526     C*------------------------------------------------------------------------*
106800970526     C     IMPFUN        BEGSR
106900970526     C*
107000970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
107100970526IF  1C     DEPOP1        IFEQ      'O05'
107200970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
107300970526     C                   MOVEL     'N'           FUDATT
107400970526     C                   MOVEL     FUDRIG        FUD(6)
107500970526E   1C                   ENDIF
107600970526     C*
107700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
107800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
107900970526     C                   EXSR      FUNGES
108000970526     C*
108100970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
108200970526     C*---
108300970526     C* 90 -> PROTEZIONE CHIAVE
108400970526     C* 91 -> PROTEZIONE TOTALE
108500970526     C*---
108600970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
108700970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
108800970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
108900970526     C                   SETOFF                                       91
109000120903     C                   SETOFF                                       92        *NO protect flag £6
109100970526E   2C                   ENDIF
109200970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
109300970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
109400120903     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
109500120911     C                   IF        D00EPA = '6'
109600120911     C                   SETOFF                                       92        *NO PR/ND flag £6
109700120911E   2C                   ELSE
109800120911     C                   SETON                                        92        *protect flag £6
109900120911E   2C                   ENDIF
110000120911E   2C                   ENDIF
110100970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
110200120903     C***  DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
110300970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
110400970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
110500970526     C                   SETON                                        91
110600120911     C                   IF        D00EPA = '6'
110700120911     C                   SETOFF                                       92        *NO PR/ND flag £6
110800120911E   2C                   ELSE
110900120911     C                   SETON                                        92        *protect flag £6
111000120911E   2C                   ENDIF
111100970526E   2C                   ENDIF
111200120903     C     DEPOP1        IFEQ      'O05'                                        *VISUALIZZAZIONE
111300120903     C                   SETON                                        90        *TUTTE LE PROTEZIONI
111400120903     C                   SETON                                        91
111500120911     C                   SETON                                        92        *protect flag £6
111600120903E   2C                   ENDIF
111700131015     c
111800131015     C     DEPOP1        IFEQ      'O+1'                                        *VISUALIZZAZIONE
111900131015     C                   SETON                                        90        *TUTTE LE PROTEZIONI
112000131015     C                   SETON                                        91
112100131015     C                   SETON                                        92        *protect flag £6
112200131015E   2C                   ENDIF
112300970526X   1C                   ELSE                                                   *I N T E R R O G A
112400970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
112500970526     C                   SETON                                        91
112600120903     C                   SETON                                        92        *Protect flag £6
112700970526E   1C                   ENDIF
112800970526     C*
112900970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
113000970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
113100970618     C                   MOVEL     D00EPA        KCEEPA
113200970618     C                   Z-ADD     D00ETP        KCETFP
113300970618     C                   Z-ADD     D00ETA        KCETFA
113400970618     C                   Z-ADD     D00EDD        KCEDDE
113500970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
113600970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
113700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
113800970526     C                   MOVEL     MSG(6)        DSMSMS
113900970526X   2C                   ELSE
114000970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
114100980128IF  3C     DEPOP1        IFEQ      'O05'
114200980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
114300980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
114400980128E   3C                   ENDIF
114500970526E   2C                   ENDIF
114600970526X   1C                   ELSE                                                   *INSERIMENTO
114700970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
114800970526E   1C                   ENDIF
114900970526     C*
115000970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
115100970526IF  1C     DEPOP1        IFEQ      'O02'
115200970526     C                   MOVEL     DEC(1)        VIDFUN
115300970526E   1C                   ENDIF
115400970526IF  1C     DEPOP1        IFEQ      'O04'
115500970526     C                   MOVEL     DEC(2)        VIDFUN
115600970526E   1C                   ENDIF
115700970526IF  1C     DEPOP1        IFEQ      'O05'
115800970526     C                   MOVEL     DEC(3)        VIDFUN
115900970526E   1C                   ENDIF
116000970526IF  1C     DEPOP1        IFEQ      'O07'
116100970526     C                   MOVEL     DEC(4)        VIDFUN
116200970526E   1C                   ENDIF
116300970526IF  1C     DEPOP1        IFEQ      'F06'
116400970526     C                   MOVEL     DEC(5)        VIDFUN
116500970526E   1C                   ENDIF
116600131015IF  1C     DEPOP1        IFEQ      'O+1'
116700131015     C                   MOVEL     DEC(6)        VIDFUN
116800131015E   1C                   ENDIF
116900970526     C*
117000970526     C                   ENDSR
117100970526     C*------------------------------------------------------------------------*
117200970526     C* *INZSR - OPERAZIONI INIZIALI
117300970526     C*------------------------------------------------------------------------*
117400970526     C     *INZSR        BEGSR
117500970526     C*---
117600970526     C* RICEVIMENTO PARAMETRI
117700970526     C*---
117800970526     C     *ENTRY        PLIST
117900970612     C                   PARM                    KPJBA
118000970612     C                   PARM                    DSSI00
118100000427     C*
118200000427     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
118300970526     C*
118400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
118500970526     C     XMSGPA        PLIST
118600970526     C                   PARM                    DSMSMS
118700970526     C                   PARM                    DSMSPG
118800970526     C                   PARM                    DSMSKE
118900970526     C                   PARM                    DSMSER
119000970526     C*
119100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
119200970526     C                   CALL      'XKEY'
119300970526     C                   PARM                    DSKEY
119400970526     C*---
119500970526     C* VARIABILI NON RIFERITE AL DATA BASE
119600970526     C*---
119700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
119800970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
119900970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
120000970526     C     *LIKE         DEFINE    D00OP0        WOP0
120100970526     C     *LIKE         DEFINE    D00OP1        WOP1
120200970526     C*---
120300970526     C* VARIABILI DI CONTROLLO
120400970526     C*---
120500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
120600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
120700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
120800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
120900970526     C*
121000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
121100970528     C                   MOVEL     'TISI18R'     DSMSPG
121200970528     C                   MOVEL     'TISI18R'     MSGQUE
121300970526     C                   MOVEL     *BLANKS       MSGKEY
121400970526     C                   MOVEL     *BLANKS       DSMSMS
121500970526     C*---
121600970526     C* VARIABILI RIFERITE AL DATA BASE
121700970526     C*---
121800970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
121900970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
122000970618     C     *LIKE         DEFINE    CAETFP        KCETFP
122100970618     C     *LIKE         DEFINE    CAETFA        KCETFA
122200970618     C     *LIKE         DEFINE    CAEDDE        KCEDDE
122300970526     C*---
122400970526     C* CHIAVI DI LETTURA
122500970526     C*---
122600970528     C* LETTURA AZORG01L - COMPLETA
122700970528     C     KEYORG        KLIST
122800970528     C                   KFLD                    KORFIL                         *FILIALE
122900970618     C* LETTURA AZCAE01L - COMPLETA
123000970618     C     KEYCAE        KLIST
123100970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
123200970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
123300970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
123400970618     C                   KFLD                    KCEDDE                         *DECORRENZA
123500970618     C* LETTURA AZCAE01L - PARZIALE
123600970618     C     KE2CAE        KLIST
123700970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
123800970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
123900970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
124000020506      * Lettura AZCAE01L
124100020506     c     ke1cae        klist
124200020506     c                   kfld                    kceepa
124300970528     C*
124400970528     C* CALCOLA LA DATA CORRENTE
124500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
124600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
124700970528     C                   Z-ADD     WN8           G08DAT
124800970528     C                   Z-ADD     *ZEROS        G08INV
124900970528     C                   MOVEL     '0'           G08ERR
125000970528     C                   CALL      'XSRDA8'
125100970528     C                   PARM                    WLBDA8
125200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
125300970526     C*
125400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
125500970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
125600970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
125700971231     C*
125800971231     C* IT'S A JOKE
125900971231IF  1C     KNMUS         IFEQ      'EDPES'
126000971231     C                   SETON                                            01
126100971231E   1C                   ENDIF
126200050412     c                   movel     knmus         winizioprf        3
126300970526     C*
126400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
126500970526     C                   EXSR      IMPFUN
126600970526     C*
126700970526     C                   ENDSR
126800970526     C*------------------------------------------------------------------------*
126900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
127000970526     C*------------------------------------------------------------------------*
127100970526     C     FUNGES        BEGSR
127200970526     C*
127300970526     C* PULISCO CAMPI DI LAVORO
127400970526     C                   Z-ADD     0             W                 5 0
127500970526     C*
127600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
127700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
127800970526     C     1             DO        24            I
127900970526     C                   MOVEL     FUD(I)        FUDRIG
128000970526     C*
128100970526     C* PULIZIA FUNZIONI PRECEDENTI
128200970526     C                   MOVEL     *BLANKS       FUV(I)
128300970526     C*
128400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
128500970526     C* (SALTA F24)
128600970526     C     FUDATT        IFEQ      'S'
128700970526     C     FUDFUN        ANDNE     '24'
128800970526     C                   ADD       1             W
128900970526     C                   MOVEL     FUDDES        FUV(W)
129000970526     C                   END
129100970526     C                   END                                                    DO WHILE
129200970526     C*
129300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
129400970526     C* ALTRIMENTI DISATTIVO F24
129500970526     C     1             DO        24            I
129600970526     C                   MOVEL     FUD(I)        FUDRIG
129700970526     C*
129800970526     C     FUDFUN        IFEQ      '24'
129900970526     C     W             IFGT      6
130000970526     C                   MOVEL     'S'           FUDATT
130100970526     C                   ELSE
130200970526     C                   MOVEL     *BLANKS       FUDATT
130300970526     C                   END                                                    W GT 8
130400970526     C                   MOVEL     FUDRIG        FUD(I)
130500970526     C                   END
130600970526     C                   END                                                    DO WHILE
130700970526     C*
130800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
130900970526     C     W             DIV       6             I
131000970526     C                   MVR                     Z
131100970526     C*
131200970526     C     Z             IFNE      0
131300970526     C     I             ADD       1             Z
131400970526     C                   ELSE
131500970526     C                   Z-ADD     I             Z                 5 0
131600970526     C                   END
131700970526     C*
131800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
131900970526     C                   ADD       1             FUNGRU
132000970526     C     FUNGRU        IFGT      Z
132100970526     C     FUNGRU        ORLT      1
132200970526     C                   Z-ADD     1             FUNGRU
132300970526     C                   END
132400970526     C*
132500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
132600970526     C     FUNGRU        IFEQ      1
132700970526     C                   MOVEL     FUV(1)        FUN001
132800970526     C                   MOVEL     FUV(2)        FUN002
132900970526     C                   MOVEL     FUV(3)        FUN003
133000970526     C                   MOVEL     FUV(4)        FUN004
133100970526     C                   MOVEL     FUV(5)        FUN005
133200970526     C                   MOVEL     FUV(6)        FUN006
133300970526     C                   END
133400970526     C*
133500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
133600970526     C     FUNGRU        IFEQ      2
133700970526     C                   MOVEL     FUV(7)        FUN001
133800970526     C                   MOVEL     FUV(8)        FUN002
133900970526     C                   MOVEL     FUV(9)        FUN003
134000970526     C                   MOVEL     FUV(10)       FUN004
134100970526     C                   MOVEL     FUV(11)       FUN005
134200970526     C                   MOVEL     FUV(12)       FUN006
134300970526     C                   END
134400970526     C*
134500970526     C* TERZO GRUPPO DI 6 FUNZIONI
134600970526     C     FUNGRU        IFEQ      3
134700970526     C                   MOVEL     FUV(13)       FUN001
134800970526     C                   MOVEL     FUV(14)       FUN002
134900970526     C                   MOVEL     FUV(15)       FUN003
135000970526     C                   MOVEL     FUV(16)       FUN004
135100970526     C                   MOVEL     FUV(17)       FUN005
135200970526     C                   MOVEL     FUV(18)       FUN006
135300970526     C                   END
135400970526     C*
135500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
135600970526     C     FUNGRU        IFEQ      4
135700970526     C                   MOVEL     FUV(19)       FUN001
135800970526     C                   MOVEL     FUV(20)       FUN002
135900970526     C                   MOVEL     FUV(21)       FUN003
136000970526     C                   MOVEL     FUV(22)       FUN004
136100970526     C                   MOVEL     *BLANKS       FUN005
136200970526     C                   MOVEL     *BLANKS       FUN006
136300970526     C                   END
136400970526     C*
136500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
136600970526     C                   MOVEL     *BLANKS       FUN007
136700970526     C                   MOVEL     *BLANKS       FUN008
136800970526     C*
136900970526     C     1             DO        24            I
137000970526     C                   MOVEL     FUD(I)        FUDRIG
137100970526     C     FUDFUN        IFEQ      '24'
137200970526     C     FUDATT        ANDEQ     'S'
137300970526     C                   MOVEL     FUDDES        FUN008
137400970526     C                   END
137500970526     C                   END                                                    DO
137600970526     C*
137700970526     C                   ENDSR
137800970526     C*------------------------------------------------------------------------*
137900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
138000970526     C*------------------------------------------------------------------------*
138100970526     C     FUNCON        BEGSR
138200970526     C*
138300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
138400970526     C                   EXSR      FUNVAR
138500970526     C*
138600970526     C* PULISCO CAMPI DI LAVORO
138700970526     C                   Z-ADD     0             I                 5 0
138800970526     C                   Z-ADD     0             W                 5 0
138900970526     C*
139000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
139100970526     C     FUNKEY        IFNE      *BLANKS
139200970526     C     1             DO        24            I
139300970526     C                   MOVEL     FUD(I)        FUDRIG
139400970526     C*
139500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
139600970526     C     FUNKEY        IFEQ      FUDFUN
139700970526     C     FUDATT        ANDEQ     'S'
139800970526     C                   Z-ADD     I             W
139900970526     C                   END
140000970526     C                   END                                                    DO WHILE
140100970526     C*
140200970526     C* FUNZIONE NON ATTIVA (ERRORE)
140300970526     C     W             IFEQ      0
140400970526     C*
140500970526     C* EMISSIONE MESSAGGIO DI ERRORE
140600970526     C                   MOVEL     MSG(1)        DSMSMS
140700970526     C*
140800970526     C                   END                                                    W=0
140900970526     C                   END                                                    FUNKEY NE *BLANKS
141000970526     C*
141100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
141200970526     C     W             IFNE      0
141300970526     C     FUNKEY        OREQ      *BLANKS
141400970526     C     FUNKEY        IFEQ      *BLANKS
141500970526     C                   EXSR      ENTGES
141600970526     C                   END
141700970526     C     FUNKEY        IFEQ      '03'
141800970526     C                   EXSR      F03GES
141900970526     C                   END
142000970526     C     FUNKEY        IFEQ      '06'
142100970526     C                   EXSR      F06GES
142200970526     C                   END
142300970526     C     FUNKEY        IFEQ      '12'
142400970526     C                   EXSR      F12GES
142500970526     C                   END
142600970526     C     FUNKEY        IFEQ      '24'
142700970526     C                   EXSR      F24GES
142800970526     C                   END
142900970526     C                   END
143000970526     C*
143100970526     C                   ENDSR
143200970526     C*------------------------------------------------------------------------*
143300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
143400970526     C*------------------------------------------------------------------------*
143500970526     C     FUNVAR        BEGSR
143600970526     C*
143700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
143800970526     C*
143900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
144000970526     C     KEY           IFEQ      ENTER
144100970526     C                   MOVEL     *BLANKS       FUNKEY
144200970526     C                   END
144300970526     C     KEY           IFEQ      F1
144400970526     C                   MOVEL     '01'          FUNKEY
144500970526     C                   END
144600970526     C     KEY           IFEQ      F2
144700970526     C                   MOVEL     '02'          FUNKEY
144800970526     C                   END
144900970526     C     KEY           IFEQ      F3
145000970526     C                   MOVEL     '03'          FUNKEY
145100970526     C                   END
145200970526     C     KEY           IFEQ      F4
145300970526     C                   MOVEL     '04'          FUNKEY
145400970526     C                   END
145500970526     C     KEY           IFEQ      F5
145600970526     C                   MOVEL     '05'          FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      F6
145900970526     C                   MOVEL     '06'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      F7
146200970526     C                   MOVEL     '07'          FUNKEY
146300970526     C                   END
146400970526     C     KEY           IFEQ      F8
146500970526     C                   MOVEL     '08'          FUNKEY
146600970526     C                   END
146700970526     C     KEY           IFEQ      F9
146800970526     C                   MOVEL     '09'          FUNKEY
146900970526     C                   END
147000970526     C     KEY           IFEQ      F10
147100970526     C                   MOVEL     '10'          FUNKEY
147200970526     C                   END
147300970526     C     KEY           IFEQ      F11
147400970526     C                   MOVEL     '11'          FUNKEY
147500970526     C                   END
147600970526     C     KEY           IFEQ      F12
147700970526     C                   MOVEL     '12'          FUNKEY
147800970526     C                   END
147900970526     C     KEY           IFEQ      F13
148000970526     C                   MOVEL     '13'          FUNKEY
148100970526     C                   END
148200970526     C     KEY           IFEQ      F14
148300970526     C                   MOVEL     '14'          FUNKEY
148400970526     C                   END
148500970526     C     KEY           IFEQ      F15
148600970526     C                   MOVEL     '15'          FUNKEY
148700970526     C                   END
148800970526     C     KEY           IFEQ      F16
148900970526     C                   MOVEL     '16'          FUNKEY
149000970526     C                   END
149100970526     C     KEY           IFEQ      F17
149200970526     C                   MOVEL     '17'          FUNKEY
149300970526     C                   END
149400970526     C     KEY           IFEQ      F18
149500970526     C                   MOVEL     '18'          FUNKEY
149600970526     C                   END
149700970526     C     KEY           IFEQ      F19
149800970526     C                   MOVEL     '19'          FUNKEY
149900970526     C                   END
150000970526     C     KEY           IFEQ      F20
150100970526     C                   MOVEL     '20'          FUNKEY
150200970526     C                   END
150300970526     C     KEY           IFEQ      F21
150400970526     C                   MOVEL     '21'          FUNKEY
150500970526     C                   END
150600970526     C     KEY           IFEQ      F22
150700970526     C                   MOVEL     '22'          FUNKEY
150800970526     C                   END
150900970526     C     KEY           IFEQ      F23
151000970526     C                   MOVEL     '23'          FUNKEY
151100970526     C                   END
151200970526     C     KEY           IFEQ      F24
151300970526     C                   MOVEL     '24'          FUNKEY
151400970526     C                   END
151500970526     C     KEY           IFEQ      ROLLUP
151600970526     C                   MOVEL     'RU'          FUNKEY
151700970526     C                   END
151800970526     C     KEY           IFEQ      HELP
151900970526     C                   MOVEL     'HP'          FUNKEY
152000970526     C                   END
152100970526     C                   ENDSR
152200970526     C*------------------------------------------------------------------------*
152300970526     C* NOTE COSTRUZIONE SCHIERE
152400970526     C*------------------------------------------------------------------------*
152500970526     C*
152600970526     C* -MSG-MESSAGGI
152700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
152800970526     C* (NON TOCCARE MSG,1 E MSG,2)
152900970526     C*
153000970526     C* -CMD-COMANDI
153100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
153200970526     C*
153300970526     C* -OPD-OPZIONI
153400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
153500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
153600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
153700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
153800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
153900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
154000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
154100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
154200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
154300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
154400970526     C*
154500970526     C* -FUD-FUNZIONI
154600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
154700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
154800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
154900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
155000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
155100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
155200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
155300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
155400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
155500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
155600970526     O*------------------------------------------------------------------------*
155700970526** DEC - DECODIFICHE
155800970526    MODIFICA                                                      1
155900970526  ANNULLAMENTO                                                    2
156000970526VISUALIZZAZIONE                                                   3
156100970526   RIPRISTINO                                                     4
156200970526  INSERIMENTO                                                     5
156300131015PosticipaDecorr.                                                  6
156400970526** MSG - MESSAGGI
156500970528TISI18R- Tasto funzionale non gestito                                          1
156600970528TISI18R- Inserimento effettuato                                                2
156700970528TISI18R- Modifica effettuata                                                   3
156800970528TISI18R- Annullamento effettuato                                               4
156900970528TISI18R- Ripristino effettuato                                                 5
157000970528TISI18R- Richiesta funzione per record inesistente                             6
157100970528TISI18R- Record momentaneamente allocato: riprovare più tardi                  7
157200970528TISI18R- Richiesta funzione per record esistente                               8
157300970528TISI18R- 9
157400090115TISI18R- Il record immesso si sovrappone ad uno già in decorrenza.ENTER forza  10
157500140117TISI18R- Immettere un tipo terminal esistente                                  11
157600970528TISI18R- Terminal / Linea partenza errato                                      12
157700970528TISI18R- Terminal / Linea arrivo errato                                        13
157800970528TISI18R- Data errata                                                           14
157900970528TISI18R- La data scadenza deve essere maggiore della decorrenza                15
158000120214TISI18R- Il record non può scadere in un giorno già trascorso.                 16
158100971203TISI18R- Il record immesso si sovrappone ad uno già in decorrenza              17
158200971203TISI18R- Terminal / linea errati                                               18
158300140117TISI18R- Impossibile inserire terminal Arrivo per tipo "P" "O"                 19
158400970618TISI18R- L'eccezione non può essere uguale al terminal arrivo standard         20
158500140117TISI18R- Per tipo terminal "6"  "C"  "J" non inserire il terminal partenza     21
158600020507TISI18R- Scadenza incongruente con scadenza record tipo "6"                    22
158700020507TISI18R- Esistono dei record di tipo "6". Impossibile la modifica              23
158800080806TISI18R- Il p.o. gestione ha il terminal di Arrivo   diverso dal p.o. gestito  24
158900080806TISI18R- Data decorrenza/scadenza incongruenti con le date del terminal Arr.   25
159000120214TISI18R- Il record non può scadere in un giorno già trascorso.ENTER forza      26
159100120903TISI18R- Operazione effettuata sul tipo terminal £6 ma non esiste il tipo C    27
159200131015TISI18R- Errori nel posticipare la decorrenza : controllare!!                  28
159300970526** FUD - FUNZIONI
159400970526                   -        (S/N)=N                        funz.01¬
159500970526                   -        (S/N)=N                        funz.02¬
159600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
159700970526                   -        (S/N)=N                        funz.04¬
159800970526                   -        (S/N)=N                        funz.05¬
159900970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
160000970527                   -        (S/N)=N                        funz.07¬
160100970527                   -        (S/N)=N                        funz.08¬
160200970527                   -        (S/N)=N                        funz.09¬
160300970527                   -        (S/N)=N                        funz.10¬
160400970526                   -        (S/N)=N                        funz.11¬
160500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
160600970526                   -        (S/N)=N                        funz.13¬
160700970526                   -        (S/N)=N                        funz.14¬
160800970526                   -        (S/N)=N                        funz.15¬
160900970526                   -        (S/N)=N                        funz.16¬
161000970526                   -        (S/N)=N                        funz.17¬
161100970526                   -        (S/N)=N                        funz.18¬
161200970526                   -        (S/N)=N                        funz.19¬
161300970526                   -        (S/N)=N                        funz.20¬
161400970526                   -        (S/N)=N                        funz.21¬
161500970526                   -        (S/N)=N                        funz.22¬
161600970526                   -        (S/N)=N                        funz.23¬
161700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
