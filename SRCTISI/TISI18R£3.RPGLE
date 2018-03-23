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
002100970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500120903     D MSG             S             78    DIM(27) CTDATA PERRCD(1)
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
019200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019300970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700000515     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     F06GES        BEGSR
020000970526     C*
020100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
020200970526IF  1C     DEPOP1        IFEQ      'F06'
020300970526     C                   EXSR      INSERI
020400970526E   1C                   ENDIF
020500970526IF  1C     DEPOP1        IFEQ      'O02'
020600970526     C                   EXSR      MODIFI
020700970526E   1C                   ENDIF
020800970526IF  1C     DEPOP1        IFEQ      'O04'
020900970526     C                   EXSR      ANNULL
021000970526E   1C                   ENDIF
021100970526IF  1C     DEPOP1        IFEQ      'O07'
021200970526     C                   EXSR      RIPRIS
021300970526E   1C                   ENDIF
021400970526     C*
021500970526     C                   ENDSR
021600970526     C*------------------------------------------------------------------------*
021700970526     C* F12GES - GESTIONE F12 -> RITORNO
021800970526     C*------------------------------------------------------------------------*
021900970526     C     F12GES        BEGSR
022000970526     C*
022100970526     C* IMPOSTA "PREMUTO F12"
022200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022300970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
022400970526     C*
022500970526     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022800970526     C*------------------------------------------------------------------------*
022900970526     C     F24GES        BEGSR
023000970526     C*
023100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
023600970526     C*------------------------------------------------------------------------*
023700970526     C     INSERI        BEGSR
023800970526     C*
023900970526     C* CONTROLLA I CAMPI IMMESSI
024000000515     C                   EXSR      DECODI
024100970526     C*
024200120216     c* Se inserimento di terminal PAR/ARR e profilo EDP
024300120216     c* si fa scadere eventuale terminal decorrente
024400120216     c                   if        winizioprf='EDP' and
024500120216     c                             (videpa='P' or videpa='A')
024600120216     c                   exsr      scaden
024700120216     c                   endif
024800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025000970618     C                   MOVEL     VIDEPA        CAEEPA                         *CHIAVE
025100970618     C                   MOVEL     TFPVID        CAETFP
025200970618     C                   MOVEL     TFAVID        CAETFA
025300970618     C                   Z-ADD     DDEVID        CAEDDE
025400970618     C                   EXSR      IMPCAE                                       *ALTRI CAMPI
025500970618     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
025600970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
025700970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
025800970618     C                   WRITE     AZCAE000
025900000427     C*
026000000427     C                   EXSR      LETCAE
026100970526     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
026200120831     C*
026300120903     C* se il tipo terminal inserito è £6
026400120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
026500120831     C                   IF        CAEEPA = '6' and
026600120911     C                             VIDCRC = 'SI'
026700120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
026800120903     C* lo inserisco
026900120903     C                   WRITE     AZCAE000
027000120831     C                   ENDIF
027100970602     C*
027200970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
027300970602     C                   EXSR      AZZVID
027400970526E   1C                   ENDIF
027500970526     C*
027600970526     C                   ENDSR
027700120831     C*------------------------------------------------------------------------*
027800120831     C* SCADEN - OPERAZIONI per far scadere il terminal già decorrente
027900120831     C*------------------------------------------------------------------------*
028000120831     C     SCADEN        BEGSR
028100120216     C                   MOVEL     VIDEPA        KCEEPA
028200120216     C                   MOVEL     TFPVID        KCETFP
028300120216     C                   MOVEL     TFAVID        KCETFA
028400120216     C     KE2CAE        SETLL     AZCAE01L
028500120216     C     KE2CAE        READE     AZCAE01L
028600120216     C                   DOW       not %eof(azcae01l)
028700120216     C     CAEATB        IFEQ      *BLANKS
028800120216     C     DDEVID        andgt     CAEDDE                                       *DECORRENZA
028900120216     C     DDEVID        ANDLE     CAEDSC
029000120216     c* data decorrenza del nuovo record - un giorno = data scadenza del record
029100120216     c* già esistente
029200120216     c                   move      ddevid        dataiso
029300120216     c                   subdur    1:*d          dataiso
029400120216     c                   move      dataiso       caedsc
029500120216     c                   update    azcae000
029600120216     c                   leave
029700120216     c                   endif
029800120216     C     KE2CAE        READE     AZCAE01L
029900120216     c                   enddo
030000120216     c                   endsr
030100970526     C*------------------------------------------------------------------------*
030200970526     C* MODIFI - OPERAZIONI DI MODIFICA
030300970526     C*------------------------------------------------------------------------*
030400970526     C     MODIFI        BEGSR
030500970526     C*
030600970526     C* CONTROLLA I CAMPI IMMESSI
030700970526     C                   EXSR      DECODI
030800970526     C*
030900970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
031000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031100970618     C                   EXSR      LETCAE
031200030310     C*
031300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
031400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031500970618     C                   EXSR      IMPCAE                                       *IMPOSTA I CAMPI
031600970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
031700970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
031800970618     C                   UPDATE    AZCAE000
031900000427     C*
032000970526     C                   MOVEL     MSG(3)        D00MSG
032100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032200120903     C*
032300120903     C* se il tipo terminal inserito è £6
032400120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
032500120903     C                   IF        CAEEPA = '6' and
032600120911     C                             VIDCRC = 'SI'
032700120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
032800120903     C                   EXSR      LETCAE2
032900120903     C* lo modifico
033000120903     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
033100120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033200120903     C                   EXSR      IMPCAE                                       *IMPOSTA I CAMPI
033300120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
033400120903     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
033500120903     C                   UPDATE    AZCAE000
033600120903     C                   ENDIF
033700120903     C                   ENDIF
033800120903     C*
033900970526E   2C                   ENDIF
034000970526E   1C                   ENDIF
034100970526     C*
034200970526     C                   ENDSR
034300970526     C*------------------------------------------------------------------------*
034400970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
034500970526     C*------------------------------------------------------------------------*
034600970526     C     ANNULL        BEGSR
034700970526     C*
034800040624     c                   exsr      sr_atten
034900040624     c*
035000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
035100000602     C                   MOVEL     '0'           ANNDLT            1
035200970618     C                   EXSR      LETCAE
035300970527     C*
035400970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
035500970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035600970618IF  2C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
035700970618IF  3C     CAEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
035800970618     C                   MOVEL     'A'           CAEATB                         *FLAG ANNULLAMENTO
035900970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
036000970618     C                   UPDATE    AZCAE000                                     *ANNULLA
036100970526X   3C                   ELSE                                                   *MAI TRASMESSO
036200970618     C                   DELETE    AZCAE000                                     *CANCELLA
036300000602     C                   MOVEL     '1'           ANNDLT
036400970526E   3C                   ENDIF
036500970526E   2C                   ENDIF
036600970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
036700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036800120903     C*
036900120903IF  2C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
037000120903     C* se il tipo terminal inserito è £6
037100120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
037200120903IF   C                   IF        CAEEPA = '6' and
037300120911     C                             VIDCRC = 'SI'
037400120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
037500120903     C                   EXSR      LETCAE2
037600120903IF  3C     CAEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
037700120903     C* lo annullo logicamente
037800120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037900120903     C                   MOVEL     'A'           CAEATB                         *FLAG ANNULLAMENTO
038000120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
038100120903     C                   UPDATE    AZCAE000
038200120903     C                   ENDIF
038300120903     C                   ELSE
038400120903     C* lo cancello
038500120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038600120903     C                   DELETE    AZCAE000
038700120903     C                   ENDIF
038800120903     C                   ENDIF
038900120903     C                   ENDIF
039000120903     C                   ENDIF
039100120903     C*
039200970526E   1C                   ENDIF
039300970526     C*
039400970526     C                   ENDSR
039500970526     C*------------------------------------------------------------------------*
039600970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
039700970526     C*------------------------------------------------------------------------*
039800970526     C     RIPRIS        BEGSR
039900970526     C*
040000970526     C* CONTROLLA I CAMPI IMMESSI
040100970526     C                   EXSR      DECODI
040200970526     C*
040300970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
040400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040500970618     C                   EXSR      LETCAE
040600970526     C*
040700970526     C* IMPOSTA IL RECORD DA SCRIVERE
040800970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040900970618     C                   MOVEL     *BLANKS       CAEATB                         *FLAG ANNULLAMENTO
041000970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
041100970618     C                   UPDATE    AZCAE000                                     *ANNULLA
041200970526E   2C                   ENDIF
041300970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
041400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
041500120903     C*
041600120903     C* se il tipo terminal inserito è £6
041700120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
041800120903     C                   IF        CAEEPA = '6' and
041900120911     C                             VIDCRC = 'SI'
042000120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
042100120903     C                   EXSR      LETCAE2
042200120903     C* lo ripristino
042300120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042400120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
042500120903     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
042600120903     C                   UPDATE    AZCAE000
042700120903     C                   ENDIF
042800120903     C                   ENDIF
042900120903     C*
043000970526E   1C                   ENDIF
043100970526     C*
043200970526     C                   ENDSR
043300970526     C*------------------------------------------------------------------------*
043400970618     C* LETCAE - LEGGE IL RECORD PERL'AGGIORNAMENTO
043500970526     C*------------------------------------------------------------------------*
043600970618     C     LETCAE        BEGSR
043700970526     C*
043800000427     C     DEPOP1        IFNE      'F06'
043900980127     C                   MOVEL     D00EPA        KCEEPA
044000980127     C                   MOVEL     D00ETP        KCETFP
044100980127     C                   MOVEL     D00ETA        KCETFA
044200980127     C                   Z-ADD     D00EDD        KCEDDE
044300970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
044400970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
044500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
044600970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
044700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
044800970526E   1C                   ENDIF
044900970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
045000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
045100970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
045200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
045300970526E   1C                   ENDIF
045400000427     C                   ENDIF
045500970526     C*
045600970526     C                   ENDSR
045700120903     C*------------------------------------------------------------------------*
045800120903     C* LETCAE2 - LEGGE IL RECORD tipo terminal C corrispondente al £6 PER L'AGGIORNAMENTO
045900120903     C*------------------------------------------------------------------------*
046000120903     C     LETCAE2       BEGSR
046100120903     C*
046200120903     C* LETTURA AZCAE01L - COMPLETA
046300120903     C     K04CAE01      KLIST
046400120903     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
046500120903     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
046600120903     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
046700120903     C                   KFLD                    CAEDDE                         *DECORRENZA
046800120903     C*
046900120903     C     K04CAE01      CHAIN     AZCAE01L                           9998
047000120903IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
047100120903     C                   MOVEL     '1'           ERRL00                         *ERRORE
047200120903     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
047300120903     C                   MOVEL     '1'           FINL00                         *FINE PGM
047400120903E   1C                   ENDIF
047500120903IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
047600120903     C                   MOVEL     '1'           ERRL00                         *ERRORE
047700120903     C                   MOVEL     MSG(27)       D00MSG                         *MESSAGGIO
047800120903     C                   MOVEL     '1'           FINL00                         *FINE PGM
047900120903E   1C                   ENDIF
048000120903     C*
048100120903     C                   ENDSR
048200970526     C*------------------------------------------------------------------------*
048300970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
048400970526     C*------------------------------------------------------------------------*
048500970526     C     DECODI        BEGSR
048600970528     C*--------------------
048700970528     C* PARTENZA/ARRIVO
048800970528     C*--------------------
048900970528IF  1C     ERRL00        IFEQ      '0'
049000970528IF  2C     VIDEPA        IFEQ      'P'
049100970528     C     VIDEPA        OREQ      'A'
049200020418     C     VIDEPA        OREQ      '6'
049300030310     C     VIDEPA        OREQ      'C'
049400060706     C     VIDEPA        OREQ      'O'
049500970528X   2C                   ELSE
049600970528     C                   MOVEL     '1'           ERRL00
049700970528     C                   SETON                                        60
049800970528     C                   MOVEL     MSG(11)       DSMSMS
049900970528E   2C                   ENDIF
050000970528E   1C                   ENDIF
050100970528     C*--------------------
050200970528     C* TERMINAL / LINEA DI PARTENZA
050300970528     C*--------------------
050400970618     C     *LIKE         DEFINE    VIDTFP        TFPVID
050500020418IF  1c                   if        errl00 = *off
050600020418     c                   clear                   vddtfp
050700030310     c                   if        videpa = '6' and vidtfp > *zeros or
050800030310     c                             videpa = 'C' and vidtfp > *zeros
050900030310     c                   eval      errl00 = *on
051000020418     c                   eval      *in61 = *on
051100020418     c                   eval      dsmsms = msg(21)
051200020418     c                   endif
051300020418     c                   endif
051400020418
051500970528IF  1C     ERRL00        IFEQ      '0'
051600970618     C                   MOVEL     *BLANKS       VDDTFP
051700030310     c                   if        (videpa ='A' or videpa='6' or videpa='C') and
051800020418     c                             (vidtfp = *blanks or vidtfp = *zeros)
051900970618     C                   MOVEL     *ZEROS        TFPVID
052000970618X   2C                   ELSE
052100970618     C                   MOVEL     VIDTFP        WLIN
052200970618     C                   MOVEL     *BLANKS       WDLIN
052300970618     C                   EXSR      CHKLIN
052400970618     C                   MOVEL     WLIN          VIDTFP
052500970618     C                   MOVEL     WDLIN         VDDTFP
052600970618     C                   MOVEL     VIDTFP        TFPVID
052700970618IF  3C     ERRL00        IFEQ      '1'
052800970528     C                   SETON                                        61
052900970618     C                   MOVEL     MSG(12)       DSMSMS
053000970618E   3C                   ENDIF
053100970618E   2C                   ENDIF
053200970528E   1C                   ENDIF
053300970528     C*--------------------
053400970528     C* TERMINAL / LINEA DI ARRIVO
053500970528     C*--------------------
053600970618     C     *LIKE         DEFINE    VIDTFA        TFAVID
053700970528IF  1C     ERRL00        IFEQ      '0'
053800970618     C                   MOVEL     *BLANKS       VDDTFA
053900060706     c                   if        (videpa ='P' or videpa='O') and
054000060706     c                             (vidtfa = *blanks or vidtfa = *zeros)
054100970618     C                   MOVEL     *ZEROS        TFAVID
054200970618X   2C                   ELSE
054300970528     C                   MOVEL     VIDTFA        WLIN
054400970528     C                   MOVEL     *BLANKS       WDLIN
054500970528     C                   EXSR      CHKLIN
054600970528     C                   MOVEL     WLIN          VIDTFA
054700970528     C                   MOVEL     WDLIN         VDDTFA
054800970618     C                   MOVEL     VIDTFA        TFAVID
054900970618IF  3C     ERRL00        IFEQ      '1'
055000970528     C                   SETON                                        62
055100970528     C                   MOVEL     MSG(13)       DSMSMS
055200970618E   3C                   ENDIF
055300970618E   2C                   ENDIF
055400970528E   1C                   ENDIF
055500970618     C*
055600060706     C* NO ECCEZIONE IN PARTENZA o per ottico
055700970618IF  1C     ERRL00        IFEQ      '0'
055800060706     c                   if        videpa='P' or videpa='O'
055900970618IF  3C     VIDTFP        IFGT      *ZEROS
056000970618     C     VIDTFA        ANDGT     *ZEROS
056100970618     C     VIDTFE        ANDGT     *ZEROS
056200060706     C                   SETON                                        62
056300970618     C                   MOVEL     MSG(19)       DSMSMS
056400970618     C                   MOVEL     '1'           ERRL00
056500970618E   3C                   ENDIF
056600970618E   2C                   ENDIF
056700970618E   1C                   ENDIF
056800970528     C*--------------------
056900970528     C* DATA DECORRENZA
057000970528     C*--------------------
057100970528     C     *LIKE         DEFINE    VIDDDE        DDEVID
057200970528IF  1C     ERRL00        IFEQ      '0'
057300970528     C                   Z-ADD     VIDDDE        WDA8
057400970528     C                   Z-ADD     *ZEROS        DA8W
057500970528     C                   EXSR      CHKDA8
057600970528     C                   Z-ADD     WDA8          VIDDDE
057700090115     c* pulisco flag di forzatura sulla date se cambiano
057800090115     c                   if        da8w<>ddevid
057900090115     c                   clear                   forzater
058000120216     c                   setoff                                       42
058100090115     c                   endif
058200970528     C                   Z-ADD     DA8W          DDEVID
058300970528IF  2C     ERRL00        IFEQ      '1'
058400970528     C                   SETON                                        63
058500970528     C                   MOVEL     MSG(14)       DSMSMS
058600970528E   2C                   ENDIF
058700970528E   1C                   ENDIF
058800970528     C* INSERIMENTO -> RECORD INESISTENTE
058900970528IF  1C     ERRL00        IFEQ      '0'
059000970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
059100970618     C                   MOVEL     VIDEPA        KCEEPA
059200970618     C                   MOVEL     TFPVID        KCETFP
059300970618     C                   MOVEL     TFAVID        KCETFA
059400970618     C                   Z-ADD     DDEVID        KCEDDE
059500970618     C     KEYCAE        CHAIN(N)  AZCAE01L                           99
059600970528IF  3C     *IN99         IFEQ      *OFF
059700970528     C                   MOVEL     '1'           ERRL00
059800970528     C                   MOVEL     MSG(8)        DSMSMS
059900970528     C                   SETON                                        606162
060000970528     C                   SETON                                        63
060100970528E   3C                   ENDIF
060200970528E   2C                   ENDIF
060300970528E   1C                   ENDIF
060400970528     C*--------------------
060500970528     C* DATA SCADENZA
060600970528     C*--------------------
060700970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
060800970528IF  1C     ERRL00        IFEQ      '0'
060900970528     C                   Z-ADD     VIDDSC        WDA8
061000970528     C                   Z-ADD     *ZEROS        DA8W
061100970528     C                   EXSR      CHKDA8
061200970528     C                   Z-ADD     WDA8          VIDDSC
061300090115     c                   if        da8w<>dscvid
061400090115     c                   clear                   forzater
061500120216     c                   setoff                                       42
061600120214     c                   clear                   forzaSCA
061700090115     c                   endif
061800970528     C                   Z-ADD     DA8W          DSCVID
061900970528IF  2C     ERRL00        IFEQ      '1'
062000970528     C                   SETON                                        64
062100970528     C                   MOVEL     MSG(14)       DSMSMS
062200970528E   2C                   ENDIF
062300970528E   1C                   ENDIF
062400970528     C* SCADENZA > DECORRENZA
062500970528IF  1C     ERRL00        IFEQ      '0'
062600970528IF  2C     DSCVID        IFLT      DDEVID
062700970619     C                   SETON                                        64
062800970528     C                   MOVEL     MSG(15)       DSMSMS
062900970528     C                   MOVEL     '1'           ERRL00
063000970528E   2C                   ENDIF
063100970528E   1C                   ENDIF
063200970528     C* SCADENZA >= DATA CORRENTE
063300050412     c* Per EDP e per terminal partenza e £6, non faccio il controllo
063400970528IF  1C     ERRL00        IFEQ      '0'
063500050412     c                   if        (winizioprf='EDP' and videpa='P') or
063600050412     c                             (winizioprf='EDP' and videpa='6')
063700050412     c                   else
063800120214     c
063900120214     c* Per EDP forzatura
064000970528IF  2C     DEPOP1        IFEQ      'O02'
064100970528     C     DEPOP1        OREQ      'O07'
064200970528     C     DEPOP1        OREQ      'F06'
064300970528IF  3C     DSCVID        IFLT      DATCOR
064400120214     c     forzasca      andeq     ' '
064500970619     C                   SETON                                        64
064600970528     C                   MOVEL     MSG(16)       DSMSMS
064700120214     c                   if         winizioprf='EDP'
064800120214     c                   eval      forzaSCA='S'
064900120214     C                   MOVEL     MSG(26)       DSMSMS
065000120214     c                   endif
065100120214     c
065200970528     C                   MOVEL     '1'           ERRL00
065300970528E   3C                   ENDIF
065400050412E   3C                   ENDIF
065500970528E   2C                   ENDIF
065600970528E   1C                   ENDIF
065700970528     C*---
065800970528     C* DECORRENZA + SCADENZA
065900970528     C*---
066000970528     C* ACCAVALLAMENTO DATI
066100970528IF  1C     ERRL00        IFEQ      '0'
066200090115     c     forzater      andeq     ' '
066300970618     C                   MOVEL     VIDEPA        KCEEPA
066400970618     C                   MOVEL     TFPVID        KCETFP
066500970618     C                   MOVEL     TFAVID        KCETFA
066600970618     C     KE2CAE        SETLL     AZCAE01L
066700970618     C     KE2CAE        READE(N)  AZCAE01L                               99
066800970528DO  2C     *IN99         DOWEQ     *OFF
066900970528IF  3C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
067000970618     C     DDEVID        ANDNE     CAEDDE                                       *RECORD IN LINEA
067100970528     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
067200970618IF  4C     CAEATB        IFEQ      *BLANKS
067300970618IF  5C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
067400970618     C     DDEVID        ANDLE     CAEDSC
067500970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
067600970618     C     DSCVID        ANDLE     CAEDSC
067700970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
067800970618     C     CAEDSC        ANDLE     DSCVID
067900970528     C                   SETON                                        606162
068000970619     C                   SETON                                        6364
068100970528     C                   MOVEL     MSG(17)       DSMSMS
068200970528     C                   MOVEL     '1'           ERRL00
068300120216     c                   select
068400120216     c                   when      winizioprf='EDP' and videpa='6'
068500120216     c                   eval      forzater='S'
068600120216     C                   MOVEL     MSG(10)       DSMSMS
068700120216     c                   when      winizioprf='EDP' and
068800120216     c                             (videpa='P' or videpa='A')
068900120216     c                             and ddevid>caedde
069000120216     c                   eval      forzater='S'
069100120216     c                   seton                                        42
069200120216     C                   clear                   DSMSMS
069300120216     c
069400120216     c                   endsl
069500090115     c
069600970528     C                   LEAVE
069700970528E   5C                   ENDIF
069800970528E   4C                   ENDIF
069900970528E   3C                   ENDIF
070000970618     C     KE2CAE        READE(N)  AZCAE01L                               99
070100970528E   2C                   ENDDO
070200970528E   1C                   ENDIF
070300970528     C*--------------------
070400970528     C* TERMINAL / LINEA ECCEZIONE
070500970528     C*--------------------
070600970528IF  1C     ERRL00        IFEQ      '0'
070700970528     C                   MOVEL     VIDTFE        WLIN
070800970528     C                   MOVEL     *BLANKS       WDLIN
070900970528     C                   EXSR      CHKLIN
071000970528     C                   MOVEL     WLIN          VIDTFE
071100970528     C                   MOVEL     WDLIN         VDDTFE
071200970528IF  2C     ERRL00        IFEQ      '1'
071300970528     C                   SETON                                        65
071400970528     C                   MOVEL     MSG(18)       DSMSMS
071500970528E   2C                   ENDIF
071600970528E   1C                   ENDIF
071700970528     C*
071800970618     C* SE ECCEZIONE IN ARRIVO, ECCEZIONE <> ARRIVO STANDARD
071900970618IF  1C     ERRL00        IFEQ      '0'
072000970618IF  2C     VIDEPA        IFEQ      'A'
072100970618IF  3C     VIDTFP        IFGT      *ZEROS
072200970618     C     VIDTFA        ANDGT     *ZEROS
072300970618     C     VIDTFE        ANDGT     *ZEROS
072400970618     C                   MOVEL     'A'           KCEEPA
072500970618     C                   Z-ADD     *ZEROS        KCETFP
072600970618     C                   MOVEL     TFAVID        KCETFA
072700970618     C     KE2CAE        SETLL     AZCAE01L
072800970618     C     KE2CAE        READE(N)  AZCAE01L                               99
072900970618DO  4C     *IN99         DOWEQ     *OFF
073000970618IF  5C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
073100970618IF  5C     CAETFE        IFEQ      VIDTFE
073200970618IF  7C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
073300970618     C     DDEVID        ANDLE     CAEDSC
073400970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
073500970618     C     DSCVID        ANDLE     CAEDSC
073600970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
073700970618     C     CAEDSC        ANDLE     DSCVID
073800970618     C                   SETON                                        606162
073900970618     C                   SETON                                        63
074000970618     C                   MOVEL     MSG(20)       DSMSMS
074100970618     C                   MOVEL     '1'           ERRL00
074200970618     C                   LEAVE
074300970618E   7C                   ENDIF
074400970618E   6C                   ENDIF
074500970618E   5C                   ENDIF
074600970618     C     KE2CAE        READE(N)  AZCAE01L                               99
074700970618E   4C                   ENDDO                                                  *FINE TERM.ARR.STAND
074800970618E   3C                   ENDIF
074900970618E   2C                   ENDIF
075000970618E   1C                   ENDIF
075100020507
075200020507      * controlli per incongruenze tra record di tipo 'P' (partenza) e record di tip '6' (£6)
075300020507 b1  c                   if        errl00 = *off and
075400020507     c                             (videpa = 'P' or videpa = '6')
075500020507     c                   exsr      chk£6
075600020507 e1  c                   endif
075700040624     c
075800040624     c                   exsr      sr_atten
075900970618     C*
076000970528     C                   ENDSR
076100040624     C*------------------------------------------------------------------------*
076200040624     C* SR_atten -  Emissione messaggio informativo
076300040624     C*------------------------------------------------------------------------*
076400040624     C     SR_atten      BEGSR
076500040624     c     ERRL00        ifeq      '0'
076600040624     c     videpa        andeq     'A'
076700040624     c     funkey        andeq     '06'
076800040624     c     wforz         andeq     *blanks
076900040624     c* modificata linea o data
077000040624     c     depop1        ifeq      'O02'
077100040624     c     sav_vidtfe    andne     vidtfe
077200040624     c     depop1        oreq      'O02'
077300040624     c     sav_viddsc    andne     viddsc
077400040624     c* ripristino
077500040624     c     depop1        oreq      'O07'
077600040624     c* cancellazione
077700040624     c     depop1        oreq      'O04'
077800040624     c                   eval      wforz='F'
077900040624     c                   eval      errl00='1'
078000040624     c                   seton                                        41
078100040624     c                   endif
078200040624     c                   endif
078300040624     c                   endsr
078400970528     C*------------------------------------------------------------------------*
078500970528     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
078600970528     C*------------------------------------------------------------------------*
078700970528     C     CHKLIN        BEGSR
078800970528     C*
078900970618     C     *LIKE         DEFINE    VIDTFA        WLIN
079000970618     C     *LIKE         DEFINE    VDDTFA        WDLIN
079100970528     C*
079200970528     C* RICHIESTO ELENCO
079300970528     C                   SETOFF                                           28
079400970528     C     '?'           SCAN      WLIN                                   28
079500970528IF  1C     *IN28         IFEQ      *ON
079600970528     C                   MOVEL     *BLANKS       §COD1             3
079700970528     C                   MOVEL     *BLANKS       §TIP              1
079800970528     C                   MOVEL     *BLANKS       §DES1            25
079900971203     C                   MOVEL     *BLANKS       §PRA              3
080000971203     C                   CALL      'TNSD24R'
080100970528     C                   PARM                    §COD1
080200970528     C                   PARM                    §TIP
080300970528     C                   PARM                    §DES1
080400971203     C                   PARM                    §PRA
080500970528     C                   MOVEL     §COD1         WLIN
080600970528     C                   MOVEL     §DES1         WDLIN
080700970528E   1C                   ENDIF
080800970528     C*
080900970528     C* SOLO CARATTERI NUMERICI
081000970528     C                   SETOFF                                       28
081100970528     C                   TESTN                   WLIN                 28
081200970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
081300970528     C                   MOVEL     '1'           ERRL00
081400970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
081500970528     C                   MOVE      WLIN          W001              1
081600970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
081700970528     C                   MOVEL     '1'           ERRL00
081800970528X   2C                   ELSE                                                   *NUMERO
081900970528     C*
082000970528     C* CONTROLLA ESISTENZA CODICE
082100970528     C                   MOVEL     WLIN          KORFIL
082200970528     C     KEYORG        CHAIN     AZORG01L                           99
082300970528IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
082400970528     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
082500970528     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
082600970528     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
082700970528X   3C                   ELSE
082800970528     C                   MOVEL     '1'           ERRL00
082900970528E   3C                   ENDIF
083000970528E   2C                   ENDIF
083100970528E   1C                   ENDIF
083200970528     C*
083300970528     C                   ENDSR
083400970528     C*------------------------------------------------------------------------*
083500970528     C* CHKDA8 - CONTROLLA LA DATA (8)
083600970528     C*------------------------------------------------------------------------*
083700970528     C     CHKDA8        BEGSR
083800970528     C*
083900970528     C     *LIKE         DEFINE    VIDDDE        WDA8
084000970528     C     *LIKE         DEFINE    VIDDDE        DA8W
084100970528     C*
084200970528     C                   Z-ADD     WDA8          G08DAT
084300970528     C                   Z-ADD     *ZEROS        G08INV
084400970528     C                   MOVEL     '0'           G08ERR
084500970528     C                   CALL      'XSRDA8'
084600970528     C                   PARM                    WLBDA8
084700970528     C                   Z-ADD     G08DAT        WDA8
084800970528     C                   Z-ADD     G08INV        DA8W
084900970528IF  1C     G08ERR        IFEQ      '1'
085000970528     C                   MOVEL     '1'           ERRL00
085100970528E   1C                   ENDIF
085200970528     C*
085300970528     C                   ENDSR
085400020506      *------------------------------------------------------------------------*
085500080806      * CHK£6  - CONTROLLA INCONGRUENZE TRA RECOD 'A' E RECORD '6'
085600020506      *------------------------------------------------------------------------*
085700020507     c     chk£6         begsr
085800080806      ***
085900020507      * Record tipo 'P'
086000020507      *----------------
086100080806     c***                if        videpa = 'A'
086200080806     c***                eval      kceepa = '6'
086300080806     c***  ke1cae        setll     azcae01l
086400080806     c***                do        *hival
086500080806     c***  ke1cae        reade(n)  azcae01l
086600080806     c***                if        %eof(azcae01l)
086700080806     c***                leave
086800080806     c***                endif
086900080806     c***                if        caeatb<>*blanks
087000080806     c***                iter
087100080806     c***                endif
087200080806     c***
087300080806     c***                movel     caetfa        wcaetfa
087400080806     c***                movel     caetfe        wcaetfe
087500080806      **controllo la congruenza della scadenza tra record 'A' e record '6'
087600080806     c***                if        sav_viddsc <> viddsc and
087700080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid) and
087800080806     c***                          caedsc > dscvid
087900080806     c***                eval      *in64 = *on
088000080806     c***                eval      dsmsms = msg(22)
088100080806     c***                eval      errl00 = *on
088200080806     c***                leave
088300080806     c***                endif
088400080806      **controllo che non esistano ancora dei record di tipo '6'
088500080806     c***                if        sav_vidtfe <> vidtfe and
088600080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid)
088700080806     c***                eval      *in65 = *on
088800080806     c***                eval      dsmsms = msg(23)
088900080806     c***                eval      errl00 = *on
089000080806     c***                leave
089100080806     c***                endif
089200080806     c***                enddo
089300080806      ***
089400080806     c***                endif
089500080806      ***
089600080806      **Record tipo '6'
089700080806      *----------------
089800080806     c***                if        videpa = '6'
089900080806     c***                eval      kceepa = 'A'
090000080806     c***                eval      ok_date = *off
090100080806     c***                clear                   sav_tfe1
090200080806      **controllo il terminal di partenza del p.o. gestito
090300080806     c***                movel     vidtfa        kcetfp
090400080806     c***                clear                   kcetfa
090500080806     c***  ke2cae        setll     azcae01l
090600080806     c***                do        *hival
090700080806     c***  ke2cae        reade(n)  azcae01l
090800080806      **fine file
090900080806     c***                if        %eof(azcae01l)
091000080806     c***                leave
091100080806     c***                endif
091200080806      ***
091300080806     c***                eval      *in63 = *off
091400080806     c***                eval      *in64 = *off
091500080806      **controllo le date
091600080806     c***                if        ddevid >= caedde and dscvid <= caedsc
091700080806     c***                eval      sav_tfe1 = caetfe
091800080806     c***                eval      ok_date = *on
091900080806     c***                leave
092000080806      ***
092100080806      **controllo quale data non corrisponde
092200080806     c***                if        ok_date = *off
092300080806     c***                eval      *in63 = (ddevid < caedde)
092400080806     c***                eval      *in64 = (dscvid > caedsc)
092500080806     c***                endif
092600080806      ***
092700080806     c***                enddo
092800080806      **le date non corrispondono errore
092900080806     c***                if        ok_date = *off and (*in63 or *in64)
093000080806     c***                eval      dsmsms = msg(25)
093100080806     c***                eval      errl00 = *on
093200080806     c***                endif
093300080806
093400080806      **controllo il terminal di partenza del p.o. gestione
093500080806     c***                if        errl00 = *off
093600080806     c***                eval      ok_date = *off
093700080806     c***                clear                   sav_tfe2
093800080806     c***                movel     vidtfe        kcetfp
093900080806     c***                clear                   kcetfa
094000080806     c***  ke2cae        setll     azcae01l
094100080806     c***                do        *hival
094200080806     c***  ke2cae        reade(n)  azcae01l
094300020516      * fine file
094400080806     C***                if        %eof(azcae01l)
094500080806     c***                leave
094600080806     c***                endif
094700080806      ***
094800080806     c***                eval      *in63 = *off
094900080806     c***                eval      *in64 = *off
095000080806      **controllo le date
095100080806     c***                if        ddevid >= caedde and dscvid <= caedsc
095200080806     c***                eval      sav_tfe2 = caetfe
095300080806     c***                eval      ok_date = *on
095400080806     c***                leave
095500080806     c***                endif
095600020516      * controllo quale data non corrisponde
095700080806     c***                if        ok_date = *off
095800080806     c***                eval      *in63 = (ddevid < caedde)
095900080806     c***                eval      *in64 = (dscvid > caedsc)
096000080806     c***                endif
096100080806      ***
096200080806     c***                enddo
096300080806      **le date non corrispondono errore
096400080806     c***                if        ok_date = *off and (*in63 or *in64)
096500080806     c***                eval      dsmsms = msg(25)
096600080806     c***                eval      errl00 = *on
096700080806     c***                endif
096800080806      ***
096900080806     c***                endif
097000080806      ***
097100080806      **controllo se i 2 terminal  corrispondono
097200080806     c***                if        errl00 = *off and
097300080806     c***                          sav_tfe1 <> sav_tfe2
097400080806     c***                eval      *in65 = *on
097500080806     c***                eval      dsmsms = msg(24)
097600080806     c***                eval      errl00 = *on
097700080806     c***                endif
097800080806      ***
097900080806     c***                endif
098000020506
098100020506     c                   endsr
098200970526     C*------------------------------------------------------------------------*
098300970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
098400970526     C*------------------------------------------------------------------------*
098500970526     C     IREVID        BEGSR
098600090115     c                   clear                   forzater
098700120216     c                   setoff                                       42
098800120214     c                   clear                   forzaSCA
098900970526     C* CAMPI
099000970618     C                   MOVEL     CAEEPA        VIDEPA
099100970618     C                   MOVEL     CAETFP        VIDTFP
099200970618     C                   MOVEL     CAETFA        VIDTFA
099300970618     C                   Z-ADD     CAEDDE        G08INV
099400970528     C                   Z-ADD     *ZEROS        G08DAT
099500970528     C                   MOVEL     '3'           G08ERR
099600970528     C                   CALL      'XSRDA8'
099700970528     C                   PARM                    WLBDA8
099800970528     C                   Z-ADD     G08DAT        VIDDDE
099900970618     C                   Z-ADD     CAEDSC        G08INV
100000970528     C                   Z-ADD     *ZEROS        G08DAT
100100970528     C                   MOVEL     '3'           G08ERR
100200970528     C                   CALL      'XSRDA8'
100300970528     C                   PARM                    WLBDA8
100400970528     C                   Z-ADD     G08DAT        VIDDSC
100500970618     C                   Z-ADD     CAETFE        VIDTFE
100600020510
100700020510      * salvo data scadenza e terminal per controlli congruenze tra record
100800020510      * di tipo 'P' e record di tipo '6'
100900020510     c                   eval      sav_viddsc = viddsc
101000020510     c                   eval      sav_vidtfe = vidtfe
101100120903
101200120903     C* metto SI come default per gestione corrispondente rcd C se tipo terminal £6
101300120911     C                   MOVEL     'SI'          VIDCRC
101400970527     C*
101500970526     C                   ENDSR
101600970526     C*------------------------------------------------------------------------*
101700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
101800970526     C*------------------------------------------------------------------------*
101900970526     C     AZZVID        BEGSR
102000970526     C* CAMPI
102100090115     c                   clear                   forzater
102200120216     c                   setoff                                       42
102300120214     c                   clear                   forzaSCA
102400970528     C                   MOVEL     *BLANKS       VIDEPA
102500970618     C                   MOVEL     *BLANKS       VIDTFP
102600970618     C                   MOVEL     *BLANKS       VIDTFA
102700970528     C                   Z-ADD     *ZEROS        VIDDDE
102800970528     C                   Z-ADD     *ZEROS        VIDDSC
102900970618     C                   Z-ADD     *ZEROS        VIDTFE
103000120911     C                   MOVEL     'SI'          VIDCRC
103100970526     C* DECODIFICHE
103200970528     C                   MOVEL     *BLANKS       VDDTFP
103300970528     C                   MOVEL     *BLANKS       VDDTFA
103400970528     C                   MOVEL     *BLANKS       VDDTFE
103500970526     C*
103600970526     C                   ENDSR
103700970526     C*------------------------------------------------------------------------*
103800970618     C* IMPCAE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
103900970526     C*------------------------------------------------------------------------*
104000970618     C     IMPCAE        BEGSR
104100970526     C*
104200970618     C                   Z-ADD     DSCVID        CAEDSC
104300970618     C                   Z-ADD     VIDTFE        CAETFE
104400970526     C*
104500970526     C                   ENDSR
104600970526     C*------------------------------------------------------------------------*
104700970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
104800970526     C*------------------------------------------------------------------------*
104900970526     C     IMPFUN        BEGSR
105000970526     C*
105100970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
105200970526IF  1C     DEPOP1        IFEQ      'O05'
105300970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
105400970526     C                   MOVEL     'N'           FUDATT
105500970526     C                   MOVEL     FUDRIG        FUD(6)
105600970526E   1C                   ENDIF
105700970526     C*
105800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
105900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
106000970526     C                   EXSR      FUNGES
106100970526     C*
106200970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
106300970526     C*---
106400970526     C* 90 -> PROTEZIONE CHIAVE
106500970526     C* 91 -> PROTEZIONE TOTALE
106600970526     C*---
106700970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
106800970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
106900970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
107000970526     C                   SETOFF                                       91
107100120903     C                   SETOFF                                       92        *NO protect flag £6
107200970526E   2C                   ENDIF
107300970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
107400970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
107500120903     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
107600120911     C                   IF        D00EPA = '6'
107700120911     C                   SETOFF                                       92        *NO PR/ND flag £6
107800120911E   2C                   ELSE
107900120911     C                   SETON                                        92        *protect flag £6
108000120911E   2C                   ENDIF
108100120911E   2C                   ENDIF
108200970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
108300120903     C***  DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
108400970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
108500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
108600970526     C                   SETON                                        91
108700120911     C                   IF        D00EPA = '6'
108800120911     C                   SETOFF                                       92        *NO PR/ND flag £6
108900120911E   2C                   ELSE
109000120911     C                   SETON                                        92        *protect flag £6
109100120911E   2C                   ENDIF
109200970526E   2C                   ENDIF
109300120903     C     DEPOP1        IFEQ      'O05'                                        *VISUALIZZAZIONE
109400120903     C                   SETON                                        90        *TUTTE LE PROTEZIONI
109500120903     C                   SETON                                        91
109600120911     C                   SETON                                        92        *protect flag £6
109700120903E   2C                   ENDIF
109800970526X   1C                   ELSE                                                   *I N T E R R O G A
109900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
110000970526     C                   SETON                                        91
110100120903     C                   SETON                                        92        *Protect flag £6
110200970526E   1C                   ENDIF
110300970526     C*
110400970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
110500970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
110600970618     C                   MOVEL     D00EPA        KCEEPA
110700970618     C                   Z-ADD     D00ETP        KCETFP
110800970618     C                   Z-ADD     D00ETA        KCETFA
110900970618     C                   Z-ADD     D00EDD        KCEDDE
111000970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
111100970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
111200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
111300970526     C                   MOVEL     MSG(6)        DSMSMS
111400970526X   2C                   ELSE
111500970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
111600980128IF  3C     DEPOP1        IFEQ      'O05'
111700980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
111800980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
111900980128E   3C                   ENDIF
112000970526E   2C                   ENDIF
112100970526X   1C                   ELSE                                                   *INSERIMENTO
112200970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
112300970526E   1C                   ENDIF
112400970526     C*
112500970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
112600970526IF  1C     DEPOP1        IFEQ      'O02'
112700970526     C                   MOVEL     DEC(1)        VIDFUN
112800970526E   1C                   ENDIF
112900970526IF  1C     DEPOP1        IFEQ      'O04'
113000970526     C                   MOVEL     DEC(2)        VIDFUN
113100970526E   1C                   ENDIF
113200970526IF  1C     DEPOP1        IFEQ      'O05'
113300970526     C                   MOVEL     DEC(3)        VIDFUN
113400970526E   1C                   ENDIF
113500970526IF  1C     DEPOP1        IFEQ      'O07'
113600970526     C                   MOVEL     DEC(4)        VIDFUN
113700970526E   1C                   ENDIF
113800970526IF  1C     DEPOP1        IFEQ      'F06'
113900970526     C                   MOVEL     DEC(5)        VIDFUN
114000970526E   1C                   ENDIF
114100970526     C*
114200970526     C                   ENDSR
114300970526     C*------------------------------------------------------------------------*
114400970526     C* *INZSR - OPERAZIONI INIZIALI
114500970526     C*------------------------------------------------------------------------*
114600970526     C     *INZSR        BEGSR
114700970526     C*---
114800970526     C* RICEVIMENTO PARAMETRI
114900970526     C*---
115000970526     C     *ENTRY        PLIST
115100970612     C                   PARM                    KPJBA
115200970612     C                   PARM                    DSSI00
115300000427     C*
115400000427     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
115500970526     C*
115600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
115700970526     C     XMSGPA        PLIST
115800970526     C                   PARM                    DSMSMS
115900970526     C                   PARM                    DSMSPG
116000970526     C                   PARM                    DSMSKE
116100970526     C                   PARM                    DSMSER
116200970526     C*
116300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
116400970526     C                   CALL      'XKEY'
116500970526     C                   PARM                    DSKEY
116600970526     C*---
116700970526     C* VARIABILI NON RIFERITE AL DATA BASE
116800970526     C*---
116900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
117000970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
117100970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
117200970526     C     *LIKE         DEFINE    D00OP0        WOP0
117300970526     C     *LIKE         DEFINE    D00OP1        WOP1
117400970526     C*---
117500970526     C* VARIABILI DI CONTROLLO
117600970526     C*---
117700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
117800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
117900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
118000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
118100970526     C*
118200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
118300970528     C                   MOVEL     'TISI18R'     DSMSPG
118400970528     C                   MOVEL     'TISI18R'     MSGQUE
118500970526     C                   MOVEL     *BLANKS       MSGKEY
118600970526     C                   MOVEL     *BLANKS       DSMSMS
118700970526     C*---
118800970526     C* VARIABILI RIFERITE AL DATA BASE
118900970526     C*---
119000970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
119100970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
119200970618     C     *LIKE         DEFINE    CAETFP        KCETFP
119300970618     C     *LIKE         DEFINE    CAETFA        KCETFA
119400970618     C     *LIKE         DEFINE    CAEDDE        KCEDDE
119500970526     C*---
119600970526     C* CHIAVI DI LETTURA
119700970526     C*---
119800970528     C* LETTURA AZORG01L - COMPLETA
119900970528     C     KEYORG        KLIST
120000970528     C                   KFLD                    KORFIL                         *FILIALE
120100970618     C* LETTURA AZCAE01L - COMPLETA
120200970618     C     KEYCAE        KLIST
120300970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
120400970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
120500970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
120600970618     C                   KFLD                    KCEDDE                         *DECORRENZA
120700970618     C* LETTURA AZCAE01L - PARZIALE
120800970618     C     KE2CAE        KLIST
120900970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
121000970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
121100970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
121200020506      * Lettura AZCAE01L
121300020506     c     ke1cae        klist
121400020506     c                   kfld                    kceepa
121500970528     C*
121600970528     C* CALCOLA LA DATA CORRENTE
121700970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
121800970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
121900970528     C                   Z-ADD     WN8           G08DAT
122000970528     C                   Z-ADD     *ZEROS        G08INV
122100970528     C                   MOVEL     '0'           G08ERR
122200970528     C                   CALL      'XSRDA8'
122300970528     C                   PARM                    WLBDA8
122400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
122500970526     C*
122600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
122700970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
122800970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
122900971231     C*
123000971231     C* IT'S A JOKE
123100971231IF  1C     KNMUS         IFEQ      'EDPES'
123200971231     C                   SETON                                            01
123300971231E   1C                   ENDIF
123400050412     c                   movel     knmus         winizioprf        3
123500970526     C*
123600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
123700970526     C                   EXSR      IMPFUN
123800970526     C*
123900970526     C                   ENDSR
124000970526     C*------------------------------------------------------------------------*
124100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
124200970526     C*------------------------------------------------------------------------*
124300970526     C     FUNGES        BEGSR
124400970526     C*
124500970526     C* PULISCO CAMPI DI LAVORO
124600970526     C                   Z-ADD     0             W                 5 0
124700970526     C*
124800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
124900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
125000970526     C     1             DO        24            I
125100970526     C                   MOVEL     FUD(I)        FUDRIG
125200970526     C*
125300970526     C* PULIZIA FUNZIONI PRECEDENTI
125400970526     C                   MOVEL     *BLANKS       FUV(I)
125500970526     C*
125600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
125700970526     C* (SALTA F24)
125800970526     C     FUDATT        IFEQ      'S'
125900970526     C     FUDFUN        ANDNE     '24'
126000970526     C                   ADD       1             W
126100970526     C                   MOVEL     FUDDES        FUV(W)
126200970526     C                   END
126300970526     C                   END                                                    DO WHILE
126400970526     C*
126500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
126600970526     C* ALTRIMENTI DISATTIVO F24
126700970526     C     1             DO        24            I
126800970526     C                   MOVEL     FUD(I)        FUDRIG
126900970526     C*
127000970526     C     FUDFUN        IFEQ      '24'
127100970526     C     W             IFGT      6
127200970526     C                   MOVEL     'S'           FUDATT
127300970526     C                   ELSE
127400970526     C                   MOVEL     *BLANKS       FUDATT
127500970526     C                   END                                                    W GT 8
127600970526     C                   MOVEL     FUDRIG        FUD(I)
127700970526     C                   END
127800970526     C                   END                                                    DO WHILE
127900970526     C*
128000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
128100970526     C     W             DIV       6             I
128200970526     C                   MVR                     Z
128300970526     C*
128400970526     C     Z             IFNE      0
128500970526     C     I             ADD       1             Z
128600970526     C                   ELSE
128700970526     C                   Z-ADD     I             Z                 5 0
128800970526     C                   END
128900970526     C*
129000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
129100970526     C                   ADD       1             FUNGRU
129200970526     C     FUNGRU        IFGT      Z
129300970526     C     FUNGRU        ORLT      1
129400970526     C                   Z-ADD     1             FUNGRU
129500970526     C                   END
129600970526     C*
129700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
129800970526     C     FUNGRU        IFEQ      1
129900970526     C                   MOVEL     FUV(1)        FUN001
130000970526     C                   MOVEL     FUV(2)        FUN002
130100970526     C                   MOVEL     FUV(3)        FUN003
130200970526     C                   MOVEL     FUV(4)        FUN004
130300970526     C                   MOVEL     FUV(5)        FUN005
130400970526     C                   MOVEL     FUV(6)        FUN006
130500970526     C                   END
130600970526     C*
130700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
130800970526     C     FUNGRU        IFEQ      2
130900970526     C                   MOVEL     FUV(7)        FUN001
131000970526     C                   MOVEL     FUV(8)        FUN002
131100970526     C                   MOVEL     FUV(9)        FUN003
131200970526     C                   MOVEL     FUV(10)       FUN004
131300970526     C                   MOVEL     FUV(11)       FUN005
131400970526     C                   MOVEL     FUV(12)       FUN006
131500970526     C                   END
131600970526     C*
131700970526     C* TERZO GRUPPO DI 6 FUNZIONI
131800970526     C     FUNGRU        IFEQ      3
131900970526     C                   MOVEL     FUV(13)       FUN001
132000970526     C                   MOVEL     FUV(14)       FUN002
132100970526     C                   MOVEL     FUV(15)       FUN003
132200970526     C                   MOVEL     FUV(16)       FUN004
132300970526     C                   MOVEL     FUV(17)       FUN005
132400970526     C                   MOVEL     FUV(18)       FUN006
132500970526     C                   END
132600970526     C*
132700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
132800970526     C     FUNGRU        IFEQ      4
132900970526     C                   MOVEL     FUV(19)       FUN001
133000970526     C                   MOVEL     FUV(20)       FUN002
133100970526     C                   MOVEL     FUV(21)       FUN003
133200970526     C                   MOVEL     FUV(22)       FUN004
133300970526     C                   MOVEL     *BLANKS       FUN005
133400970526     C                   MOVEL     *BLANKS       FUN006
133500970526     C                   END
133600970526     C*
133700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
133800970526     C                   MOVEL     *BLANKS       FUN007
133900970526     C                   MOVEL     *BLANKS       FUN008
134000970526     C*
134100970526     C     1             DO        24            I
134200970526     C                   MOVEL     FUD(I)        FUDRIG
134300970526     C     FUDFUN        IFEQ      '24'
134400970526     C     FUDATT        ANDEQ     'S'
134500970526     C                   MOVEL     FUDDES        FUN008
134600970526     C                   END
134700970526     C                   END                                                    DO
134800970526     C*
134900970526     C                   ENDSR
135000970526     C*------------------------------------------------------------------------*
135100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
135200970526     C*------------------------------------------------------------------------*
135300970526     C     FUNCON        BEGSR
135400970526     C*
135500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
135600970526     C                   EXSR      FUNVAR
135700970526     C*
135800970526     C* PULISCO CAMPI DI LAVORO
135900970526     C                   Z-ADD     0             I                 5 0
136000970526     C                   Z-ADD     0             W                 5 0
136100970526     C*
136200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
136300970526     C     FUNKEY        IFNE      *BLANKS
136400970526     C     1             DO        24            I
136500970526     C                   MOVEL     FUD(I)        FUDRIG
136600970526     C*
136700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
136800970526     C     FUNKEY        IFEQ      FUDFUN
136900970526     C     FUDATT        ANDEQ     'S'
137000970526     C                   Z-ADD     I             W
137100970526     C                   END
137200970526     C                   END                                                    DO WHILE
137300970526     C*
137400970526     C* FUNZIONE NON ATTIVA (ERRORE)
137500970526     C     W             IFEQ      0
137600970526     C*
137700970526     C* EMISSIONE MESSAGGIO DI ERRORE
137800970526     C                   MOVEL     MSG(1)        DSMSMS
137900970526     C*
138000970526     C                   END                                                    W=0
138100970526     C                   END                                                    FUNKEY NE *BLANKS
138200970526     C*
138300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
138400970526     C     W             IFNE      0
138500970526     C     FUNKEY        OREQ      *BLANKS
138600970526     C     FUNKEY        IFEQ      *BLANKS
138700970526     C                   EXSR      ENTGES
138800970526     C                   END
138900970526     C     FUNKEY        IFEQ      '03'
139000970526     C                   EXSR      F03GES
139100970526     C                   END
139200970526     C     FUNKEY        IFEQ      '06'
139300970526     C                   EXSR      F06GES
139400970526     C                   END
139500970526     C     FUNKEY        IFEQ      '12'
139600970526     C                   EXSR      F12GES
139700970526     C                   END
139800970526     C     FUNKEY        IFEQ      '24'
139900970526     C                   EXSR      F24GES
140000970526     C                   END
140100970526     C                   END
140200970526     C*
140300970526     C                   ENDSR
140400970526     C*------------------------------------------------------------------------*
140500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
140600970526     C*------------------------------------------------------------------------*
140700970526     C     FUNVAR        BEGSR
140800970526     C*
140900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
141000970526     C*
141100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
141200970526     C     KEY           IFEQ      ENTER
141300970526     C                   MOVEL     *BLANKS       FUNKEY
141400970526     C                   END
141500970526     C     KEY           IFEQ      F1
141600970526     C                   MOVEL     '01'          FUNKEY
141700970526     C                   END
141800970526     C     KEY           IFEQ      F2
141900970526     C                   MOVEL     '02'          FUNKEY
142000970526     C                   END
142100970526     C     KEY           IFEQ      F3
142200970526     C                   MOVEL     '03'          FUNKEY
142300970526     C                   END
142400970526     C     KEY           IFEQ      F4
142500970526     C                   MOVEL     '04'          FUNKEY
142600970526     C                   END
142700970526     C     KEY           IFEQ      F5
142800970526     C                   MOVEL     '05'          FUNKEY
142900970526     C                   END
143000970526     C     KEY           IFEQ      F6
143100970526     C                   MOVEL     '06'          FUNKEY
143200970526     C                   END
143300970526     C     KEY           IFEQ      F7
143400970526     C                   MOVEL     '07'          FUNKEY
143500970526     C                   END
143600970526     C     KEY           IFEQ      F8
143700970526     C                   MOVEL     '08'          FUNKEY
143800970526     C                   END
143900970526     C     KEY           IFEQ      F9
144000970526     C                   MOVEL     '09'          FUNKEY
144100970526     C                   END
144200970526     C     KEY           IFEQ      F10
144300970526     C                   MOVEL     '10'          FUNKEY
144400970526     C                   END
144500970526     C     KEY           IFEQ      F11
144600970526     C                   MOVEL     '11'          FUNKEY
144700970526     C                   END
144800970526     C     KEY           IFEQ      F12
144900970526     C                   MOVEL     '12'          FUNKEY
145000970526     C                   END
145100970526     C     KEY           IFEQ      F13
145200970526     C                   MOVEL     '13'          FUNKEY
145300970526     C                   END
145400970526     C     KEY           IFEQ      F14
145500970526     C                   MOVEL     '14'          FUNKEY
145600970526     C                   END
145700970526     C     KEY           IFEQ      F15
145800970526     C                   MOVEL     '15'          FUNKEY
145900970526     C                   END
146000970526     C     KEY           IFEQ      F16
146100970526     C                   MOVEL     '16'          FUNKEY
146200970526     C                   END
146300970526     C     KEY           IFEQ      F17
146400970526     C                   MOVEL     '17'          FUNKEY
146500970526     C                   END
146600970526     C     KEY           IFEQ      F18
146700970526     C                   MOVEL     '18'          FUNKEY
146800970526     C                   END
146900970526     C     KEY           IFEQ      F19
147000970526     C                   MOVEL     '19'          FUNKEY
147100970526     C                   END
147200970526     C     KEY           IFEQ      F20
147300970526     C                   MOVEL     '20'          FUNKEY
147400970526     C                   END
147500970526     C     KEY           IFEQ      F21
147600970526     C                   MOVEL     '21'          FUNKEY
147700970526     C                   END
147800970526     C     KEY           IFEQ      F22
147900970526     C                   MOVEL     '22'          FUNKEY
148000970526     C                   END
148100970526     C     KEY           IFEQ      F23
148200970526     C                   MOVEL     '23'          FUNKEY
148300970526     C                   END
148400970526     C     KEY           IFEQ      F24
148500970526     C                   MOVEL     '24'          FUNKEY
148600970526     C                   END
148700970526     C     KEY           IFEQ      ROLLUP
148800970526     C                   MOVEL     'RU'          FUNKEY
148900970526     C                   END
149000970526     C     KEY           IFEQ      HELP
149100970526     C                   MOVEL     'HP'          FUNKEY
149200970526     C                   END
149300970526     C                   ENDSR
149400970526     C*------------------------------------------------------------------------*
149500970526     C* NOTE COSTRUZIONE SCHIERE
149600970526     C*------------------------------------------------------------------------*
149700970526     C*
149800970526     C* -MSG-MESSAGGI
149900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
150000970526     C* (NON TOCCARE MSG,1 E MSG,2)
150100970526     C*
150200970526     C* -CMD-COMANDI
150300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
150400970526     C*
150500970526     C* -OPD-OPZIONI
150600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
150700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
150800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
150900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
151000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
151100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
151200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
151300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
151400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
151500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
151600970526     C*
151700970526     C* -FUD-FUNZIONI
151800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
151900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
152000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
152100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
152200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
152300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
152400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
152500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
152600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
152700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
152800970526     O*------------------------------------------------------------------------*
152900970526** DEC - DECODIFICHE
153000970526    MODIFICA                                                      1
153100970526  ANNULLAMENTO                                                    2
153200970526VISUALIZZAZIONE                                                   3
153300970526   RIPRISTINO                                                     4
153400970526  INSERIMENTO                                                     5
153500970526** MSG - MESSAGGI
153600970528TISI18R- Tasto funzionale non gestito                                          1
153700970528TISI18R- Inserimento effettuato                                                2
153800970528TISI18R- Modifica effettuata                                                   3
153900970528TISI18R- Annullamento effettuato                                               4
154000970528TISI18R- Ripristino effettuato                                                 5
154100970528TISI18R- Richiesta funzione per record inesistente                             6
154200970528TISI18R- Record momentaneamente allocato: riprovare più tardi                  7
154300970528TISI18R- Richiesta funzione per record esistente                               8
154400970528TISI18R- 9
154500090115TISI18R- Il record immesso si sovrappone ad uno già in decorrenza.ENTER forza  10
154600030310TISI18R- Immettere P=Partenza o A=Arrivo o 6=£6 o C=Coop                       11
154700970528TISI18R- Terminal / Linea partenza errato                                      12
154800970528TISI18R- Terminal / Linea arrivo errato                                        13
154900970528TISI18R- Data errata                                                           14
155000970528TISI18R- La data scadenza deve essere maggiore della decorrenza                15
155100120214TISI18R- Il record non può scadere in un giorno già trascorso.                 16
155200971203TISI18R- Il record immesso si sovrappone ad uno già in decorrenza              17
155300971203TISI18R- Terminal / linea errati                                               18
155400060706TISI18R- Impossibile inserire terminal Arrivo per tipo "P" o "O"               19
155500970618TISI18R- L'eccezione non può essere uguale al terminal arrivo standard         20
155600030310TISI18R- Per tipo terminal "6" o "C" non inserire il terminal partenza         21
155700020507TISI18R- Scadenza incongruente con scadenza record tipo "6"                    22
155800020507TISI18R- Esistono dei record di tipo "6". Impossibile la modifica              23
155900080806TISI18R- Il p.o. gestione ha il terminal di Arrivo   diverso dal p.o. gestito  24
156000080806TISI18R- Data decorrenza/scadenza incongruenti con le date del terminal Arr.   25
156100120214TISI18R- Il record non può scadere in un giorno già trascorso.ENTER forza      26
156200120903TISI18R- Operazione effettuata sul tipo terminal £6 ma non esiste il tipo C    27
156300970526** FUD - FUNZIONI
156400970526                   -        (S/N)=N                        funz.01¬
156500970526                   -        (S/N)=N                        funz.02¬
156600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
156700970526                   -        (S/N)=N                        funz.04¬
156800970526                   -        (S/N)=N                        funz.05¬
156900970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
157000970527                   -        (S/N)=N                        funz.07¬
157100970527                   -        (S/N)=N                        funz.08¬
157200970527                   -        (S/N)=N                        funz.09¬
157300970527                   -        (S/N)=N                        funz.10¬
157400970526                   -        (S/N)=N                        funz.11¬
157500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
157600970526                   -        (S/N)=N                        funz.13¬
157700970526                   -        (S/N)=N                        funz.14¬
157800970526                   -        (S/N)=N                        funz.15¬
157900970526                   -        (S/N)=N                        funz.16¬
158000970526                   -        (S/N)=N                        funz.17¬
158100970526                   -        (S/N)=N                        funz.18¬
158200970526                   -        (S/N)=N                        funz.19¬
158300970526                   -        (S/N)=N                        funz.20¬
158400970526                   -        (S/N)=N                        funz.21¬
158500970526                   -        (S/N)=N                        funz.22¬
158600970526                   -        (S/N)=N                        funz.23¬
158700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
