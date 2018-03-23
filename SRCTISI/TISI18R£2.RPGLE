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
002500120214     D MSG             S             78    DIM(26) CTDATA PERRCD(1)
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
008501120216     d Dataiso         s               d   datfmt(*iso)
008502120216     d datam1          s              8  0
008600970526     C*------------------------------------------------------------------------*
008700970526     C* MAIN LINES
008800970526     C*------------------------------------------------------------------------*
008900970526     C*
009000970526     C* NON CI SONO ERRORI IN ENTRATA
009100970526IF  1C     ERRL00        IFEQ      '0'
009200970526     C*
009300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009700970526     C*
009800970526     C* EMETTE FORMATO VIDEO
009900970526     C                   EXSR      BUTFMR
010000970526     C*
010100970526     C* GESTIONE TASTI FUNZIONALI
010200970526     C                   EXSR      FUNCON
010300970526E   2C                   ENDDO
010400970526     C*
010500970526     C* CI SONO ERRORI IN ENTRATA
010600970527X   1C                   ELSE
010700970526     C                   MOVEL     '1'           D00ERR
010800970526     C                   MOVEL     DSMSMS        D00MSG
010900970526E   1C                   ENDIF
011000970526     C* FINE PGM
011100970526     C                   SETON                                        LR
011200970526     C*------------------------------------------------------------------------*
011300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011400970526     C*------------------------------------------------------------------------*
011500970526     C     BUTFMR        BEGSR
011600970526     C*
011700970526     C* EMETTE LA TESTATA
011800970528     C                   WRITE     SI18D01
011900970526     C*
012000970526     C* SPEDISCE EVENTUALE MESSAGGIO
012100970526     C                   EXSR      SNDMSG
012200970526     C*
012300970526     C* EMETTE LE FUNZIONI
012400970528     C                   WRITE     SI18D02
012500970526     C*
012600970526     C* EMETTE IL VIDEO
012700970528     C                   EXFMT     SI18D03
012800970526     C*
012900970526     C* REIMPOSTA FLAG DI ERRORE
013000970526     C                   MOVEL     '0'           ERRL00
013100970526     C*
013200970526     C* REIMPOSTA INDICATORI DI ERRORE
013300970526     C                   SETOFF                                         5859
013400970526     C                   SETOFF                                       606162
013500970526     C                   SETOFF                                       636465
013600970526     C                   SETOFF                                       666768
013700970526     C                   SETOFF                                       697071
013800970526     C                   SETOFF                                       727374
013900970526     C                   SETOFF                                       757677
014000970526     C                   SETOFF                                       787980
014100970526     C                   SETOFF                                       818283
014200970526     C                   SETOFF                                       848586
014300970526     C                   SETOFF                                       878889
014400970526     C*
014500970526     C                   ENDSR
014600970526     C*------------------------------------------------------------------------*
014700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014800970526     C*------------------------------------------------------------------------*
014900970526     C     SNDMSG        BEGSR
015000970526     C*
015100970526     C* PULISCE IL SFILE MESSAGGI
015200970526     C                   MOVEL     '1'           *IN40
015300970528     C                   WRITE     SI18DMC
015400970526     C                   MOVEL     '0'           *IN40
015500970526     C*
015600970526     C* VISUALIZZA IL MESSAGGIO
015700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015800970526     C                   CALL      'XMSG'        XMSGPA
015900970526IF  1C     DSMSER        IFEQ      *ZEROS
016000970526     C                   MOVEL     DSMSKE        MSGKEY
016100970526E   1C                   ENDIF
016200970526     C                   Z-ADD     1             MSGULT
016300970528     C                   WRITE     SI18DMS
016400970528     C                   WRITE     SI18DMC
016500970526     C*
016600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016700970526     C                   MOVEL     *BLANKS       DSMSMS
016800970526     C*
016900970526     C                   ENDSR
017000970526     C*------------------------------------------------------------------------*
017100970526     C* ENTGES - GESTIONE ENTER -> INVIO
017200970526     C*------------------------------------------------------------------------*
017300970526     C     ENTGES        BEGSR
017400970526     C*
017500970526     C* CONTROLLA I CAMPI IMMESSI
017600970526     C                   EXSR      DECODI
017700970526     C*
017800970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
017900970526IF  1C     DEPOP1        IFEQ      'O05'
018000970526     C                   MOVEL     '1'           FINL00
018100970526E   1C                   ENDIF
018200970526     C*
018300970526     C                   ENDSR
018400970526     C*------------------------------------------------------------------------*
018500970526     C* F03GES - GESTIONE F03 -> FINE
018600970526     C*------------------------------------------------------------------------*
018700970526     C     F03GES        BEGSR
018800970526     C*
018900970526     C* IMPOSTA "PREMUTO F03"
019000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019100970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
019200970526     C*
019300970526     C                   ENDSR
019400970526     C*------------------------------------------------------------------------*
019500000515     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019600970526     C*------------------------------------------------------------------------*
019700970526     C     F06GES        BEGSR
019800970526     C*
019900970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
020000970526IF  1C     DEPOP1        IFEQ      'F06'
020100970526     C                   EXSR      INSERI
020200970526E   1C                   ENDIF
020300970526IF  1C     DEPOP1        IFEQ      'O02'
020400970526     C                   EXSR      MODIFI
020500970526E   1C                   ENDIF
020600970526IF  1C     DEPOP1        IFEQ      'O04'
020700970526     C                   EXSR      ANNULL
020800970526E   1C                   ENDIF
020900970526IF  1C     DEPOP1        IFEQ      'O07'
021000970526     C                   EXSR      RIPRIS
021100970526E   1C                   ENDIF
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970526     C* F12GES - GESTIONE F12 -> RITORNO
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F12GES        BEGSR
021800970526     C*
021900970526     C* IMPOSTA "PREMUTO F12"
022000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022100970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
022200970526     C*
022300970526     C                   ENDSR
022400970526     C*------------------------------------------------------------------------*
022500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022600970526     C*------------------------------------------------------------------------*
022700970526     C     F24GES        BEGSR
022800970526     C*
022900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
023400970526     C*------------------------------------------------------------------------*
023500970526     C     INSERI        BEGSR
023600970526     C*
023700970526     C* CONTROLLA I CAMPI IMMESSI
023800000515     C                   EXSR      DECODI
023900970526     C*
023901120216     c* Se inserimento di terminal PAR/ARR e profilo EDP
023902120216     c* si fa scadere eventuale terminal decorrente
023904120216     c                   if        winizioprf='EDP' and
023905120216     c                             (videpa='P' or videpa='A')
023906120216     c                   exsr      scaden
023907120216     c                   endif
024000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024200970618     C                   MOVEL     VIDEPA        CAEEPA                         *CHIAVE
024300970618     C                   MOVEL     TFPVID        CAETFP
024400970618     C                   MOVEL     TFAVID        CAETFA
024500970618     C                   Z-ADD     DDEVID        CAEDDE
024600970618     C                   EXSR      IMPCAE                                       *ALTRI CAMPI
024700970618     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
024800970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
024900970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
025000970618     C                   WRITE     AZCAE000
025100000427     C*
025200000427     C                   EXSR      LETCAE
025300970526     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
025400970602     C*
025500970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
025600970602     C                   EXSR      AZZVID
025700970526E   1C                   ENDIF
025800970526     C*
025900970526     C                   ENDSR
025901120216     C*------------------------------------------------------------------------*
025902120216     C* SCADEN - OPERAZIONI per far scadere il terminal già decorrente
025903120216     C*------------------------------------------------------------------------*
025904120216     C     SCADEN        BEGSR
025907120216     C                   MOVEL     VIDEPA        KCEEPA
025908120216     C                   MOVEL     TFPVID        KCETFP
025909120216     C                   MOVEL     TFAVID        KCETFA
025910120216     C     KE2CAE        SETLL     AZCAE01L
025911120216     C     KE2CAE        READE     AZCAE01L
025912120216     C                   DOW       not %eof(azcae01l)
025913120216     C     CAEATB        IFEQ      *BLANKS
025914120216     C     DDEVID        andgt     CAEDDE                                       *DECORRENZA
025915120216     C     DDEVID        ANDLE     CAEDSC
025917120216     c* data decorrenza del nuovo record - un giorno = data scadenza del record
025918120216     c* già esistente
025919120216     c                   move      ddevid        dataiso
025920120216     c                   subdur    1:*d          dataiso
025921120216     c                   move      dataiso       caedsc
025922120216     c                   update    azcae000
025923120216     c                   leave
025924120216     c                   endif
025925120216     C     KE2CAE        READE     AZCAE01L
025926120216     c                   enddo
025927120216     c                   endsr
026000970526     C*------------------------------------------------------------------------*
026100970526     C* MODIFI - OPERAZIONI DI MODIFICA
026200970526     C*------------------------------------------------------------------------*
026300970526     C     MODIFI        BEGSR
026400970526     C*
026500970526     C* CONTROLLA I CAMPI IMMESSI
026600970526     C                   EXSR      DECODI
026700970526     C*
026800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
026900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027000970618     C                   EXSR      LETCAE
027100030310     C*
027200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
027300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027400970618     C                   EXSR      IMPCAE                                       *IMPOSTA I CAMPI
027500970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
027600970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
027700970618     C                   UPDATE    AZCAE000
027800000427     C*
027900970526     C                   MOVEL     MSG(3)        D00MSG
028000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
028100970526E   2C                   ENDIF
028200970526E   1C                   ENDIF
028300970526     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
028700970526     C*------------------------------------------------------------------------*
028800970526     C     ANNULL        BEGSR
028900970526     C*
029000040624     c                   exsr      sr_atten
029100040624     c*
029200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
029300000602     C                   MOVEL     '0'           ANNDLT            1
029400970618     C                   EXSR      LETCAE
029500970527     C*
029600970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
029700970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029800970618IF  2C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
029900970618IF  3C     CAEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
030000970618     C                   MOVEL     'A'           CAEATB                         *FLAG ANNULLAMENTO
030100970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
030200970618     C                   UPDATE    AZCAE000                                     *ANNULLA
030300970526X   3C                   ELSE                                                   *MAI TRASMESSO
030400970618     C                   DELETE    AZCAE000                                     *CANCELLA
030500000602     C                   MOVEL     '1'           ANNDLT
030600970526E   3C                   ENDIF
030700970526E   2C                   ENDIF
030800970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
030900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031000970526E   1C                   ENDIF
031100970526     C*
031200970526     C                   ENDSR
031300970526     C*------------------------------------------------------------------------*
031400970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
031500970526     C*------------------------------------------------------------------------*
031600970526     C     RIPRIS        BEGSR
031700970526     C*
031800970526     C* CONTROLLA I CAMPI IMMESSI
031900970526     C                   EXSR      DECODI
032000970526     C*
032100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032300970618     C                   EXSR      LETCAE
032400970526     C*
032500970526     C* IMPOSTA IL RECORD DA SCRIVERE
032600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032700970618     C                   MOVEL     *BLANKS       CAEATB                         *FLAG ANNULLAMENTO
032800970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
032900970618     C                   UPDATE    AZCAE000                                     *ANNULLA
033000970526E   2C                   ENDIF
033100970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
033200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033300970526E   1C                   ENDIF
033400970526     C*
033500970526     C                   ENDSR
033600970526     C*------------------------------------------------------------------------*
033700970618     C* LETCAE - LEGGE IL RECORD PERL'AGGIORNAMENTO
033800970526     C*------------------------------------------------------------------------*
033900970618     C     LETCAE        BEGSR
034000970526     C*
034100000427     C     DEPOP1        IFNE      'F06'
034200980127     C                   MOVEL     D00EPA        KCEEPA
034300980127     C                   MOVEL     D00ETP        KCETFP
034400980127     C                   MOVEL     D00ETA        KCETFA
034500980127     C                   Z-ADD     D00EDD        KCEDDE
034600970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
034700970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
034800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034900970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
035000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035100970526E   1C                   ENDIF
035200970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
035300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035400970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
035500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035600970526E   1C                   ENDIF
035700000427     C                   ENDIF
035800970526     C*
035900970526     C                   ENDSR
036000970526     C*------------------------------------------------------------------------*
036100970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
036200970526     C*------------------------------------------------------------------------*
036300970526     C     DECODI        BEGSR
036400970528     C*--------------------
036500970528     C* PARTENZA/ARRIVO
036600970528     C*--------------------
036700970528IF  1C     ERRL00        IFEQ      '0'
036800970528IF  2C     VIDEPA        IFEQ      'P'
036900970528     C     VIDEPA        OREQ      'A'
037000020418     C     VIDEPA        OREQ      '6'
037100030310     C     VIDEPA        OREQ      'C'
037200060706     C     VIDEPA        OREQ      'O'
037300970528X   2C                   ELSE
037400970528     C                   MOVEL     '1'           ERRL00
037500970528     C                   SETON                                        60
037600970528     C                   MOVEL     MSG(11)       DSMSMS
037700970528E   2C                   ENDIF
037800970528E   1C                   ENDIF
037900970528     C*--------------------
038000970528     C* TERMINAL / LINEA DI PARTENZA
038100970528     C*--------------------
038200970618     C     *LIKE         DEFINE    VIDTFP        TFPVID
038300020418IF  1c                   if        errl00 = *off
038400020418     c                   clear                   vddtfp
038500030310     c                   if        videpa = '6' and vidtfp > *zeros or
038600030310     c                             videpa = 'C' and vidtfp > *zeros
038700030310     c                   eval      errl00 = *on
038800020418     c                   eval      *in61 = *on
038900020418     c                   eval      dsmsms = msg(21)
039000020418     c                   endif
039100020418     c                   endif
039200020418
039300970528IF  1C     ERRL00        IFEQ      '0'
039400970618     C                   MOVEL     *BLANKS       VDDTFP
039500030310     c                   if        (videpa ='A' or videpa='6' or videpa='C') and
039600020418     c                             (vidtfp = *blanks or vidtfp = *zeros)
039700970618     C                   MOVEL     *ZEROS        TFPVID
039800970618X   2C                   ELSE
039900970618     C                   MOVEL     VIDTFP        WLIN
040000970618     C                   MOVEL     *BLANKS       WDLIN
040100970618     C                   EXSR      CHKLIN
040200970618     C                   MOVEL     WLIN          VIDTFP
040300970618     C                   MOVEL     WDLIN         VDDTFP
040400970618     C                   MOVEL     VIDTFP        TFPVID
040500970618IF  3C     ERRL00        IFEQ      '1'
040600970528     C                   SETON                                        61
040700970618     C                   MOVEL     MSG(12)       DSMSMS
040800970618E   3C                   ENDIF
040900970618E   2C                   ENDIF
041000970528E   1C                   ENDIF
041100970528     C*--------------------
041200970528     C* TERMINAL / LINEA DI ARRIVO
041300970528     C*--------------------
041400970618     C     *LIKE         DEFINE    VIDTFA        TFAVID
041500970528IF  1C     ERRL00        IFEQ      '0'
041600970618     C                   MOVEL     *BLANKS       VDDTFA
041700060706     c                   if        (videpa ='P' or videpa='O') and
041800060706     c                             (vidtfa = *blanks or vidtfa = *zeros)
041900970618     C                   MOVEL     *ZEROS        TFAVID
042000970618X   2C                   ELSE
042100970528     C                   MOVEL     VIDTFA        WLIN
042200970528     C                   MOVEL     *BLANKS       WDLIN
042300970528     C                   EXSR      CHKLIN
042400970528     C                   MOVEL     WLIN          VIDTFA
042500970528     C                   MOVEL     WDLIN         VDDTFA
042600970618     C                   MOVEL     VIDTFA        TFAVID
042700970618IF  3C     ERRL00        IFEQ      '1'
042800970528     C                   SETON                                        62
042900970528     C                   MOVEL     MSG(13)       DSMSMS
043000970618E   3C                   ENDIF
043100970618E   2C                   ENDIF
043200970528E   1C                   ENDIF
043300970618     C*
043400060706     C* NO ECCEZIONE IN PARTENZA o per ottico
043500970618IF  1C     ERRL00        IFEQ      '0'
043600060706     c                   if        videpa='P' or videpa='O'
043700970618IF  3C     VIDTFP        IFGT      *ZEROS
043800970618     C     VIDTFA        ANDGT     *ZEROS
043900970618     C     VIDTFE        ANDGT     *ZEROS
044000060706     C                   SETON                                        62
044100970618     C                   MOVEL     MSG(19)       DSMSMS
044200970618     C                   MOVEL     '1'           ERRL00
044300970618E   3C                   ENDIF
044400970618E   2C                   ENDIF
044500970618E   1C                   ENDIF
044600970528     C*--------------------
044700970528     C* DATA DECORRENZA
044800970528     C*--------------------
044900970528     C     *LIKE         DEFINE    VIDDDE        DDEVID
045000970528IF  1C     ERRL00        IFEQ      '0'
045100970528     C                   Z-ADD     VIDDDE        WDA8
045200970528     C                   Z-ADD     *ZEROS        DA8W
045300970528     C                   EXSR      CHKDA8
045400970528     C                   Z-ADD     WDA8          VIDDDE
045500090115     c* pulisco flag di forzatura sulla date se cambiano
045600090115     c                   if        da8w<>ddevid
045700090115     c                   clear                   forzater
045701120216     c                   setoff                                       42
045800090115     c                   endif
045900970528     C                   Z-ADD     DA8W          DDEVID
046000970528IF  2C     ERRL00        IFEQ      '1'
046100970528     C                   SETON                                        63
046200970528     C                   MOVEL     MSG(14)       DSMSMS
046300970528E   2C                   ENDIF
046400970528E   1C                   ENDIF
046500970528     C* INSERIMENTO -> RECORD INESISTENTE
046600970528IF  1C     ERRL00        IFEQ      '0'
046700970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
046800970618     C                   MOVEL     VIDEPA        KCEEPA
046900970618     C                   MOVEL     TFPVID        KCETFP
047000970618     C                   MOVEL     TFAVID        KCETFA
047100970618     C                   Z-ADD     DDEVID        KCEDDE
047200970618     C     KEYCAE        CHAIN(N)  AZCAE01L                           99
047300970528IF  3C     *IN99         IFEQ      *OFF
047400970528     C                   MOVEL     '1'           ERRL00
047500970528     C                   MOVEL     MSG(8)        DSMSMS
047600970528     C                   SETON                                        606162
047700970528     C                   SETON                                        63
047800970528E   3C                   ENDIF
047900970528E   2C                   ENDIF
048000970528E   1C                   ENDIF
048100970528     C*--------------------
048200970528     C* DATA SCADENZA
048300970528     C*--------------------
048400970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
048500970528IF  1C     ERRL00        IFEQ      '0'
048600970528     C                   Z-ADD     VIDDSC        WDA8
048700970528     C                   Z-ADD     *ZEROS        DA8W
048800970528     C                   EXSR      CHKDA8
048900970528     C                   Z-ADD     WDA8          VIDDSC
049000090115     c                   if        da8w<>dscvid
049100090115     c                   clear                   forzater
049101120216     c                   setoff                                       42
049200120214     c                   clear                   forzaSCA
049300090115     c                   endif
049400970528     C                   Z-ADD     DA8W          DSCVID
049500970528IF  2C     ERRL00        IFEQ      '1'
049600970528     C                   SETON                                        64
049700970528     C                   MOVEL     MSG(14)       DSMSMS
049800970528E   2C                   ENDIF
049900970528E   1C                   ENDIF
050000970528     C* SCADENZA > DECORRENZA
050100970528IF  1C     ERRL00        IFEQ      '0'
050200970528IF  2C     DSCVID        IFLT      DDEVID
050300970619     C                   SETON                                        64
050400970528     C                   MOVEL     MSG(15)       DSMSMS
050500970528     C                   MOVEL     '1'           ERRL00
050600970528E   2C                   ENDIF
050700970528E   1C                   ENDIF
050800970528     C* SCADENZA >= DATA CORRENTE
050900050412     c* Per EDP e per terminal partenza e £6, non faccio il controllo
051000970528IF  1C     ERRL00        IFEQ      '0'
051100050412     c                   if        (winizioprf='EDP' and videpa='P') or
051200050412     c                             (winizioprf='EDP' and videpa='6')
051300050412     c                   else
051400120214     c
051500120214     c* Per EDP forzatura
051600970528IF  2C     DEPOP1        IFEQ      'O02'
051700970528     C     DEPOP1        OREQ      'O07'
051800970528     C     DEPOP1        OREQ      'F06'
051900970528IF  3C     DSCVID        IFLT      DATCOR
052000120214     c     forzasca      andeq     ' '
052100970619     C                   SETON                                        64
052200970528     C                   MOVEL     MSG(16)       DSMSMS
052300120214     c                   if         winizioprf='EDP'
052400120214     c                   eval      forzaSCA='S'
052500120214     C                   MOVEL     MSG(26)       DSMSMS
052600120214     c                   endif
052700120214     c
052800970528     C                   MOVEL     '1'           ERRL00
052900970528E   3C                   ENDIF
053000050412E   3C                   ENDIF
053100970528E   2C                   ENDIF
053200970528E   1C                   ENDIF
053300970528     C*---
053400970528     C* DECORRENZA + SCADENZA
053500970528     C*---
053600970528     C* ACCAVALLAMENTO DATI
053700970528IF  1C     ERRL00        IFEQ      '0'
053800090115     c     forzater      andeq     ' '
053900970618     C                   MOVEL     VIDEPA        KCEEPA
054000970618     C                   MOVEL     TFPVID        KCETFP
054100970618     C                   MOVEL     TFAVID        KCETFA
054200970618     C     KE2CAE        SETLL     AZCAE01L
054300970618     C     KE2CAE        READE(N)  AZCAE01L                               99
054400970528DO  2C     *IN99         DOWEQ     *OFF
054500970528IF  3C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
054600970618     C     DDEVID        ANDNE     CAEDDE                                       *RECORD IN LINEA
054700970528     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
054800970618IF  4C     CAEATB        IFEQ      *BLANKS
054900970618IF  5C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
055000970618     C     DDEVID        ANDLE     CAEDSC
055100970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
055200970618     C     DSCVID        ANDLE     CAEDSC
055300970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
055400970618     C     CAEDSC        ANDLE     DSCVID
055500970528     C                   SETON                                        606162
055600970619     C                   SETON                                        6364
055700970528     C                   MOVEL     MSG(17)       DSMSMS
055800970528     C                   MOVEL     '1'           ERRL00
056301120216     c                   select
056302120216     c                   when      winizioprf='EDP' and videpa='6'
056303120216     c                   eval      forzater='S'
056304120216     C                   MOVEL     MSG(10)       DSMSMS
056305120216     c                   when      winizioprf='EDP' and
056306120216     c                             (videpa='P' or videpa='A')
056307120216     c                             and ddevid>caedde
056308120216     c                   eval      forzater='S'
056309120216     c                   seton                                        42
056310120216     C                   clear                   DSMSMS
056311120216     c
056312120216     c                   endsl
056400090115     c
056500970528     C                   LEAVE
056600970528E   5C                   ENDIF
056700970528E   4C                   ENDIF
056800970528E   3C                   ENDIF
056900970618     C     KE2CAE        READE(N)  AZCAE01L                               99
057000970528E   2C                   ENDDO
057100970528E   1C                   ENDIF
057200970528     C*--------------------
057300970528     C* TERMINAL / LINEA ECCEZIONE
057400970528     C*--------------------
057500970528IF  1C     ERRL00        IFEQ      '0'
057600970528     C                   MOVEL     VIDTFE        WLIN
057700970528     C                   MOVEL     *BLANKS       WDLIN
057800970528     C                   EXSR      CHKLIN
057900970528     C                   MOVEL     WLIN          VIDTFE
058000970528     C                   MOVEL     WDLIN         VDDTFE
058100970528IF  2C     ERRL00        IFEQ      '1'
058200970528     C                   SETON                                        65
058300970528     C                   MOVEL     MSG(18)       DSMSMS
058400970528E   2C                   ENDIF
058500970528E   1C                   ENDIF
058600970528     C*
058700970618     C* SE ECCEZIONE IN ARRIVO, ECCEZIONE <> ARRIVO STANDARD
058800970618IF  1C     ERRL00        IFEQ      '0'
058900970618IF  2C     VIDEPA        IFEQ      'A'
059000970618IF  3C     VIDTFP        IFGT      *ZEROS
059100970618     C     VIDTFA        ANDGT     *ZEROS
059200970618     C     VIDTFE        ANDGT     *ZEROS
059300970618     C                   MOVEL     'A'           KCEEPA
059400970618     C                   Z-ADD     *ZEROS        KCETFP
059500970618     C                   MOVEL     TFAVID        KCETFA
059600970618     C     KE2CAE        SETLL     AZCAE01L
059700970618     C     KE2CAE        READE(N)  AZCAE01L                               99
059800970618DO  4C     *IN99         DOWEQ     *OFF
059900970618IF  5C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
060000970618IF  5C     CAETFE        IFEQ      VIDTFE
060100970618IF  7C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
060200970618     C     DDEVID        ANDLE     CAEDSC
060300970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
060400970618     C     DSCVID        ANDLE     CAEDSC
060500970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
060600970618     C     CAEDSC        ANDLE     DSCVID
060700970618     C                   SETON                                        606162
060800970618     C                   SETON                                        63
060900970618     C                   MOVEL     MSG(20)       DSMSMS
061000970618     C                   MOVEL     '1'           ERRL00
061100970618     C                   LEAVE
061200970618E   7C                   ENDIF
061300970618E   6C                   ENDIF
061400970618E   5C                   ENDIF
061500970618     C     KE2CAE        READE(N)  AZCAE01L                               99
061600970618E   4C                   ENDDO                                                  *FINE TERM.ARR.STAND
061700970618E   3C                   ENDIF
061800970618E   2C                   ENDIF
061900970618E   1C                   ENDIF
062000020507
062100020507      * controlli per incongruenze tra record di tipo 'P' (partenza) e record di tip '6' (£6)
062200020507 b1  c                   if        errl00 = *off and
062300020507     c                             (videpa = 'P' or videpa = '6')
062400020507     c                   exsr      chk£6
062500020507 e1  c                   endif
062600040624     c
062700040624     c                   exsr      sr_atten
062800970618     C*
062900970528     C                   ENDSR
063000040624     C*------------------------------------------------------------------------*
063100040624     C* SR_atten -  Emissione messaggio informativo
063200040624     C*------------------------------------------------------------------------*
063300040624     C     SR_atten      BEGSR
063400040624     c     ERRL00        ifeq      '0'
063500040624     c     videpa        andeq     'A'
063600040624     c     funkey        andeq     '06'
063700040624     c     wforz         andeq     *blanks
063800040624     c* modificata linea o data
063900040624     c     depop1        ifeq      'O02'
064000040624     c     sav_vidtfe    andne     vidtfe
064100040624     c     depop1        oreq      'O02'
064200040624     c     sav_viddsc    andne     viddsc
064300040624     c* ripristino
064400040624     c     depop1        oreq      'O07'
064500040624     c* cancellazione
064600040624     c     depop1        oreq      'O04'
064700040624     c                   eval      wforz='F'
064800040624     c                   eval      errl00='1'
064900040624     c                   seton                                        41
065000040624     c                   endif
065100040624     c                   endif
065200040624     c                   endsr
065300970528     C*------------------------------------------------------------------------*
065400970528     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
065500970528     C*------------------------------------------------------------------------*
065600970528     C     CHKLIN        BEGSR
065700970528     C*
065800970618     C     *LIKE         DEFINE    VIDTFA        WLIN
065900970618     C     *LIKE         DEFINE    VDDTFA        WDLIN
066000970528     C*
066100970528     C* RICHIESTO ELENCO
066200970528     C                   SETOFF                                           28
066300970528     C     '?'           SCAN      WLIN                                   28
066400970528IF  1C     *IN28         IFEQ      *ON
066500970528     C                   MOVEL     *BLANKS       §COD1             3
066600970528     C                   MOVEL     *BLANKS       §TIP              1
066700970528     C                   MOVEL     *BLANKS       §DES1            25
066800971203     C                   MOVEL     *BLANKS       §PRA              3
066900971203     C                   CALL      'TNSD24R'
067000970528     C                   PARM                    §COD1
067100970528     C                   PARM                    §TIP
067200970528     C                   PARM                    §DES1
067300971203     C                   PARM                    §PRA
067400970528     C                   MOVEL     §COD1         WLIN
067500970528     C                   MOVEL     §DES1         WDLIN
067600970528E   1C                   ENDIF
067700970528     C*
067800970528     C* SOLO CARATTERI NUMERICI
067900970528     C                   SETOFF                                       28
068000970528     C                   TESTN                   WLIN                 28
068100970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
068200970528     C                   MOVEL     '1'           ERRL00
068300970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
068400970528     C                   MOVE      WLIN          W001              1
068500970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
068600970528     C                   MOVEL     '1'           ERRL00
068700970528X   2C                   ELSE                                                   *NUMERO
068800970528     C*
068900970528     C* CONTROLLA ESISTENZA CODICE
069000970528     C                   MOVEL     WLIN          KORFIL
069100970528     C     KEYORG        CHAIN     AZORG01L                           99
069200970528IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
069300970528     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
069400970528     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
069500970528     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
069600970528X   3C                   ELSE
069700970528     C                   MOVEL     '1'           ERRL00
069800970528E   3C                   ENDIF
069900970528E   2C                   ENDIF
070000970528E   1C                   ENDIF
070100970528     C*
070200970528     C                   ENDSR
070300970528     C*------------------------------------------------------------------------*
070400970528     C* CHKDA8 - CONTROLLA LA DATA (8)
070500970528     C*------------------------------------------------------------------------*
070600970528     C     CHKDA8        BEGSR
070700970528     C*
070800970528     C     *LIKE         DEFINE    VIDDDE        WDA8
070900970528     C     *LIKE         DEFINE    VIDDDE        DA8W
071000970528     C*
071100970528     C                   Z-ADD     WDA8          G08DAT
071200970528     C                   Z-ADD     *ZEROS        G08INV
071300970528     C                   MOVEL     '0'           G08ERR
071400970528     C                   CALL      'XSRDA8'
071500970528     C                   PARM                    WLBDA8
071600970528     C                   Z-ADD     G08DAT        WDA8
071700970528     C                   Z-ADD     G08INV        DA8W
071800970528IF  1C     G08ERR        IFEQ      '1'
071900970528     C                   MOVEL     '1'           ERRL00
072000970528E   1C                   ENDIF
072100970528     C*
072200970528     C                   ENDSR
072300020506      *------------------------------------------------------------------------*
072400080806      * CHK£6  - CONTROLLA INCONGRUENZE TRA RECOD 'A' E RECORD '6'
072500020506      *------------------------------------------------------------------------*
072600020507     c     chk£6         begsr
072700080806      ***
072800020507      * Record tipo 'P'
072900020507      *----------------
073000080806     c***                if        videpa = 'A'
073100080806     c***                eval      kceepa = '6'
073200080806     c***  ke1cae        setll     azcae01l
073300080806     c***                do        *hival
073400080806     c***  ke1cae        reade(n)  azcae01l
073500080806     c***                if        %eof(azcae01l)
073600080806     c***                leave
073700080806     c***                endif
073800080806     c***                if        caeatb<>*blanks
073900080806     c***                iter
074000080806     c***                endif
074100080806     c***
074200080806     c***                movel     caetfa        wcaetfa
074300080806     c***                movel     caetfe        wcaetfe
074400080806      **controllo la congruenza della scadenza tra record 'A' e record '6'
074500080806     c***                if        sav_viddsc <> viddsc and
074600080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid) and
074700080806     c***                          caedsc > dscvid
074800080806     c***                eval      *in64 = *on
074900080806     c***                eval      dsmsms = msg(22)
075000080806     c***                eval      errl00 = *on
075100080806     c***                leave
075200080806     c***                endif
075300080806      **controllo che non esistano ancora dei record di tipo '6'
075400080806     c***                if        sav_vidtfe <> vidtfe and
075500080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid)
075600080806     c***                eval      *in65 = *on
075700080806     c***                eval      dsmsms = msg(23)
075800080806     c***                eval      errl00 = *on
075900080806     c***                leave
076000080806     c***                endif
076100080806     c***                enddo
076200080806      ***
076300080806     c***                endif
076400080806      ***
076500080806      **Record tipo '6'
076600080806      *----------------
076700080806     c***                if        videpa = '6'
076800080806     c***                eval      kceepa = 'A'
076900080806     c***                eval      ok_date = *off
077000080806     c***                clear                   sav_tfe1
077100080806      **controllo il terminal di partenza del p.o. gestito
077200080806     c***                movel     vidtfa        kcetfp
077300080806     c***                clear                   kcetfa
077400080806     c***  ke2cae        setll     azcae01l
077500080806     c***                do        *hival
077600080806     c***  ke2cae        reade(n)  azcae01l
077700080806      **fine file
077800080806     c***                if        %eof(azcae01l)
077900080806     c***                leave
078000080806     c***                endif
078100080806      ***
078200080806     c***                eval      *in63 = *off
078300080806     c***                eval      *in64 = *off
078400080806      **controllo le date
078500080806     c***                if        ddevid >= caedde and dscvid <= caedsc
078600080806     c***                eval      sav_tfe1 = caetfe
078700080806     c***                eval      ok_date = *on
078800080806     c***                leave
078900080806      ***
079000080806      **controllo quale data non corrisponde
079100080806     c***                if        ok_date = *off
079200080806     c***                eval      *in63 = (ddevid < caedde)
079300080806     c***                eval      *in64 = (dscvid > caedsc)
079400080806     c***                endif
079500080806      ***
079600080806     c***                enddo
079700080806      **le date non corrispondono errore
079800080806     c***                if        ok_date = *off and (*in63 or *in64)
079900080806     c***                eval      dsmsms = msg(25)
080000080806     c***                eval      errl00 = *on
080100080806     c***                endif
080200080806
080300080806      **controllo il terminal di partenza del p.o. gestione
080400080806     c***                if        errl00 = *off
080500080806     c***                eval      ok_date = *off
080600080806     c***                clear                   sav_tfe2
080700080806     c***                movel     vidtfe        kcetfp
080800080806     c***                clear                   kcetfa
080900080806     c***  ke2cae        setll     azcae01l
081000080806     c***                do        *hival
081100080806     c***  ke2cae        reade(n)  azcae01l
081200020516      * fine file
081300080806     C***                if        %eof(azcae01l)
081400080806     c***                leave
081500080806     c***                endif
081600080806      ***
081700080806     c***                eval      *in63 = *off
081800080806     c***                eval      *in64 = *off
081900080806      **controllo le date
082000080806     c***                if        ddevid >= caedde and dscvid <= caedsc
082100080806     c***                eval      sav_tfe2 = caetfe
082200080806     c***                eval      ok_date = *on
082300080806     c***                leave
082400080806     c***                endif
082500020516      * controllo quale data non corrisponde
082600080806     c***                if        ok_date = *off
082700080806     c***                eval      *in63 = (ddevid < caedde)
082800080806     c***                eval      *in64 = (dscvid > caedsc)
082900080806     c***                endif
083000080806      ***
083100080806     c***                enddo
083200080806      **le date non corrispondono errore
083300080806     c***                if        ok_date = *off and (*in63 or *in64)
083400080806     c***                eval      dsmsms = msg(25)
083500080806     c***                eval      errl00 = *on
083600080806     c***                endif
083700080806      ***
083800080806     c***                endif
083900080806      ***
084000080806      **controllo se i 2 terminal  corrispondono
084100080806     c***                if        errl00 = *off and
084200080806     c***                          sav_tfe1 <> sav_tfe2
084300080806     c***                eval      *in65 = *on
084400080806     c***                eval      dsmsms = msg(24)
084500080806     c***                eval      errl00 = *on
084600080806     c***                endif
084700080806      ***
084800080806     c***                endif
084900020506
085000020506     c                   endsr
085100970526     C*------------------------------------------------------------------------*
085200970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
085300970526     C*------------------------------------------------------------------------*
085400970526     C     IREVID        BEGSR
085500090115     c                   clear                   forzater
085501120216     c                   setoff                                       42
085600120214     c                   clear                   forzaSCA
085700970526     C* CAMPI
085800970618     C                   MOVEL     CAEEPA        VIDEPA
085900970618     C                   MOVEL     CAETFP        VIDTFP
086000970618     C                   MOVEL     CAETFA        VIDTFA
086100970618     C                   Z-ADD     CAEDDE        G08INV
086200970528     C                   Z-ADD     *ZEROS        G08DAT
086300970528     C                   MOVEL     '3'           G08ERR
086400970528     C                   CALL      'XSRDA8'
086500970528     C                   PARM                    WLBDA8
086600970528     C                   Z-ADD     G08DAT        VIDDDE
086700970618     C                   Z-ADD     CAEDSC        G08INV
086800970528     C                   Z-ADD     *ZEROS        G08DAT
086900970528     C                   MOVEL     '3'           G08ERR
087000970528     C                   CALL      'XSRDA8'
087100970528     C                   PARM                    WLBDA8
087200970528     C                   Z-ADD     G08DAT        VIDDSC
087300970618     C                   Z-ADD     CAETFE        VIDTFE
087400020510
087500020510      * salvo data scadenza e terminal per controlli congruenze tra record
087600020510      * di tipo 'P' e record di tipo '6'
087700020510     c                   eval      sav_viddsc = viddsc
087800020510     c                   eval      sav_vidtfe = vidtfe
087900970527     C*
088000970526     C                   ENDSR
088100970526     C*------------------------------------------------------------------------*
088200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
088300970526     C*------------------------------------------------------------------------*
088400970526     C     AZZVID        BEGSR
088500970526     C* CAMPI
088600090115     c                   clear                   forzater
088601120216     c                   setoff                                       42
088700120214     c                   clear                   forzaSCA
088800970528     C                   MOVEL     *BLANKS       VIDEPA
088900970618     C                   MOVEL     *BLANKS       VIDTFP
089000970618     C                   MOVEL     *BLANKS       VIDTFA
089100970528     C                   Z-ADD     *ZEROS        VIDDDE
089200970528     C                   Z-ADD     *ZEROS        VIDDSC
089300970618     C                   Z-ADD     *ZEROS        VIDTFE
089400970526     C* DECODIFICHE
089500970528     C                   MOVEL     *BLANKS       VDDTFP
089600970528     C                   MOVEL     *BLANKS       VDDTFA
089700970528     C                   MOVEL     *BLANKS       VDDTFE
089800970526     C*
089900970526     C                   ENDSR
090000970526     C*------------------------------------------------------------------------*
090100970618     C* IMPCAE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
090200970526     C*------------------------------------------------------------------------*
090300970618     C     IMPCAE        BEGSR
090400970526     C*
090500970618     C                   Z-ADD     DSCVID        CAEDSC
090600970618     C                   Z-ADD     VIDTFE        CAETFE
090700970526     C*
090800970526     C                   ENDSR
090900970526     C*------------------------------------------------------------------------*
091000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
091100970526     C*------------------------------------------------------------------------*
091200970526     C     IMPFUN        BEGSR
091300970526     C*
091400970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
091500970526IF  1C     DEPOP1        IFEQ      'O05'
091600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
091700970526     C                   MOVEL     'N'           FUDATT
091800970526     C                   MOVEL     FUDRIG        FUD(6)
091900970526E   1C                   ENDIF
092000970526     C*
092100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
092200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
092300970526     C                   EXSR      FUNGES
092400970526     C*
092500970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
092600970526     C*---
092700970526     C* 90 -> PROTEZIONE CHIAVE
092800970526     C* 91 -> PROTEZIONE TOTALE
092900970526     C*---
093000970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
093100970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
093200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
093300970526     C                   SETOFF                                       91
093400970526E   2C                   ENDIF
093500970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
093600970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
093700970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
093800970526E   2C                   ENDIF
093900970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
094000970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
094100970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
094200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
094300970526     C                   SETON                                        91
094400970526E   2C                   ENDIF
094500970526X   1C                   ELSE                                                   *I N T E R R O G A
094600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
094700970526     C                   SETON                                        91
094800970526E   1C                   ENDIF
094900970526     C*
095000970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
095100970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
095200970618     C                   MOVEL     D00EPA        KCEEPA
095300970618     C                   Z-ADD     D00ETP        KCETFP
095400970618     C                   Z-ADD     D00ETA        KCETFA
095500970618     C                   Z-ADD     D00EDD        KCEDDE
095600970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
095700970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
095800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
095900970526     C                   MOVEL     MSG(6)        DSMSMS
096000970526X   2C                   ELSE
096100970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
096200980128IF  3C     DEPOP1        IFEQ      'O05'
096300980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
096400980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
096500980128E   3C                   ENDIF
096600970526E   2C                   ENDIF
096700970526X   1C                   ELSE                                                   *INSERIMENTO
096800970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
096900970526E   1C                   ENDIF
097000970526     C*
097100970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
097200970526IF  1C     DEPOP1        IFEQ      'O02'
097300970526     C                   MOVEL     DEC(1)        VIDFUN
097400970526E   1C                   ENDIF
097500970526IF  1C     DEPOP1        IFEQ      'O04'
097600970526     C                   MOVEL     DEC(2)        VIDFUN
097700970526E   1C                   ENDIF
097800970526IF  1C     DEPOP1        IFEQ      'O05'
097900970526     C                   MOVEL     DEC(3)        VIDFUN
098000970526E   1C                   ENDIF
098100970526IF  1C     DEPOP1        IFEQ      'O07'
098200970526     C                   MOVEL     DEC(4)        VIDFUN
098300970526E   1C                   ENDIF
098400970526IF  1C     DEPOP1        IFEQ      'F06'
098500970526     C                   MOVEL     DEC(5)        VIDFUN
098600970526E   1C                   ENDIF
098700970526     C*
098800970526     C                   ENDSR
098900970526     C*------------------------------------------------------------------------*
099000970526     C* *INZSR - OPERAZIONI INIZIALI
099100970526     C*------------------------------------------------------------------------*
099200970526     C     *INZSR        BEGSR
099300970526     C*---
099400970526     C* RICEVIMENTO PARAMETRI
099500970526     C*---
099600970526     C     *ENTRY        PLIST
099700970612     C                   PARM                    KPJBA
099800970612     C                   PARM                    DSSI00
099900000427     C*
100000000427     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
100100970526     C*
100200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
100300970526     C     XMSGPA        PLIST
100400970526     C                   PARM                    DSMSMS
100500970526     C                   PARM                    DSMSPG
100600970526     C                   PARM                    DSMSKE
100700970526     C                   PARM                    DSMSER
100800970526     C*
100900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
101000970526     C                   CALL      'XKEY'
101100970526     C                   PARM                    DSKEY
101200970526     C*---
101300970526     C* VARIABILI NON RIFERITE AL DATA BASE
101400970526     C*---
101500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
101600970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
101700970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
101800970526     C     *LIKE         DEFINE    D00OP0        WOP0
101900970526     C     *LIKE         DEFINE    D00OP1        WOP1
102000970526     C*---
102100970526     C* VARIABILI DI CONTROLLO
102200970526     C*---
102300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
102400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
102500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
102600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
102700970526     C*
102800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
102900970528     C                   MOVEL     'TISI18R'     DSMSPG
103000970528     C                   MOVEL     'TISI18R'     MSGQUE
103100970526     C                   MOVEL     *BLANKS       MSGKEY
103200970526     C                   MOVEL     *BLANKS       DSMSMS
103300970526     C*---
103400970526     C* VARIABILI RIFERITE AL DATA BASE
103500970526     C*---
103600970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
103700970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
103800970618     C     *LIKE         DEFINE    CAETFP        KCETFP
103900970618     C     *LIKE         DEFINE    CAETFA        KCETFA
104000970618     C     *LIKE         DEFINE    CAEDDE        KCEDDE
104100970526     C*---
104200970526     C* CHIAVI DI LETTURA
104300970526     C*---
104400970528     C* LETTURA AZORG01L - COMPLETA
104500970528     C     KEYORG        KLIST
104600970528     C                   KFLD                    KORFIL                         *FILIALE
104700970618     C* LETTURA AZCAE01L - COMPLETA
104800970618     C     KEYCAE        KLIST
104900970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
105000970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
105100970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
105200970618     C                   KFLD                    KCEDDE                         *DECORRENZA
105300970618     C* LETTURA AZCAE01L - PARZIALE
105400970618     C     KE2CAE        KLIST
105500970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
105600970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
105700970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
105800020506      * Lettura AZCAE01L
105900020506     c     ke1cae        klist
106000020506     c                   kfld                    kceepa
106100970528     C*
106200970528     C* CALCOLA LA DATA CORRENTE
106300970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
106400970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
106500970528     C                   Z-ADD     WN8           G08DAT
106600970528     C                   Z-ADD     *ZEROS        G08INV
106700970528     C                   MOVEL     '0'           G08ERR
106800970528     C                   CALL      'XSRDA8'
106900970528     C                   PARM                    WLBDA8
107000970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
107100970526     C*
107200970526     C* DEPOSITA LE OPZIONI DI ENTRATA
107300970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
107400970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
107500971231     C*
107600971231     C* IT'S A JOKE
107700971231IF  1C     KNMUS         IFEQ      'EDPES'
107800971231     C                   SETON                                            01
107900971231E   1C                   ENDIF
108000050412     c                   movel     knmus         winizioprf        3
108100970526     C*
108200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
108300970526     C                   EXSR      IMPFUN
108400970526     C*
108500970526     C                   ENDSR
108600970526     C*------------------------------------------------------------------------*
108700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
108800970526     C*------------------------------------------------------------------------*
108900970526     C     FUNGES        BEGSR
109000970526     C*
109100970526     C* PULISCO CAMPI DI LAVORO
109200970526     C                   Z-ADD     0             W                 5 0
109300970526     C*
109400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
109500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
109600970526     C     1             DO        24            I
109700970526     C                   MOVEL     FUD(I)        FUDRIG
109800970526     C*
109900970526     C* PULIZIA FUNZIONI PRECEDENTI
110000970526     C                   MOVEL     *BLANKS       FUV(I)
110100970526     C*
110200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
110300970526     C* (SALTA F24)
110400970526     C     FUDATT        IFEQ      'S'
110500970526     C     FUDFUN        ANDNE     '24'
110600970526     C                   ADD       1             W
110700970526     C                   MOVEL     FUDDES        FUV(W)
110800970526     C                   END
110900970526     C                   END                                                    DO WHILE
111000970526     C*
111100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
111200970526     C* ALTRIMENTI DISATTIVO F24
111300970526     C     1             DO        24            I
111400970526     C                   MOVEL     FUD(I)        FUDRIG
111500970526     C*
111600970526     C     FUDFUN        IFEQ      '24'
111700970526     C     W             IFGT      6
111800970526     C                   MOVEL     'S'           FUDATT
111900970526     C                   ELSE
112000970526     C                   MOVEL     *BLANKS       FUDATT
112100970526     C                   END                                                    W GT 8
112200970526     C                   MOVEL     FUDRIG        FUD(I)
112300970526     C                   END
112400970526     C                   END                                                    DO WHILE
112500970526     C*
112600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
112700970526     C     W             DIV       6             I
112800970526     C                   MVR                     Z
112900970526     C*
113000970526     C     Z             IFNE      0
113100970526     C     I             ADD       1             Z
113200970526     C                   ELSE
113300970526     C                   Z-ADD     I             Z                 5 0
113400970526     C                   END
113500970526     C*
113600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
113700970526     C                   ADD       1             FUNGRU
113800970526     C     FUNGRU        IFGT      Z
113900970526     C     FUNGRU        ORLT      1
114000970526     C                   Z-ADD     1             FUNGRU
114100970526     C                   END
114200970526     C*
114300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
114400970526     C     FUNGRU        IFEQ      1
114500970526     C                   MOVEL     FUV(1)        FUN001
114600970526     C                   MOVEL     FUV(2)        FUN002
114700970526     C                   MOVEL     FUV(3)        FUN003
114800970526     C                   MOVEL     FUV(4)        FUN004
114900970526     C                   MOVEL     FUV(5)        FUN005
115000970526     C                   MOVEL     FUV(6)        FUN006
115100970526     C                   END
115200970526     C*
115300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
115400970526     C     FUNGRU        IFEQ      2
115500970526     C                   MOVEL     FUV(7)        FUN001
115600970526     C                   MOVEL     FUV(8)        FUN002
115700970526     C                   MOVEL     FUV(9)        FUN003
115800970526     C                   MOVEL     FUV(10)       FUN004
115900970526     C                   MOVEL     FUV(11)       FUN005
116000970526     C                   MOVEL     FUV(12)       FUN006
116100970526     C                   END
116200970526     C*
116300970526     C* TERZO GRUPPO DI 6 FUNZIONI
116400970526     C     FUNGRU        IFEQ      3
116500970526     C                   MOVEL     FUV(13)       FUN001
116600970526     C                   MOVEL     FUV(14)       FUN002
116700970526     C                   MOVEL     FUV(15)       FUN003
116800970526     C                   MOVEL     FUV(16)       FUN004
116900970526     C                   MOVEL     FUV(17)       FUN005
117000970526     C                   MOVEL     FUV(18)       FUN006
117100970526     C                   END
117200970526     C*
117300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
117400970526     C     FUNGRU        IFEQ      4
117500970526     C                   MOVEL     FUV(19)       FUN001
117600970526     C                   MOVEL     FUV(20)       FUN002
117700970526     C                   MOVEL     FUV(21)       FUN003
117800970526     C                   MOVEL     FUV(22)       FUN004
117900970526     C                   MOVEL     *BLANKS       FUN005
118000970526     C                   MOVEL     *BLANKS       FUN006
118100970526     C                   END
118200970526     C*
118300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
118400970526     C                   MOVEL     *BLANKS       FUN007
118500970526     C                   MOVEL     *BLANKS       FUN008
118600970526     C*
118700970526     C     1             DO        24            I
118800970526     C                   MOVEL     FUD(I)        FUDRIG
118900970526     C     FUDFUN        IFEQ      '24'
119000970526     C     FUDATT        ANDEQ     'S'
119100970526     C                   MOVEL     FUDDES        FUN008
119200970526     C                   END
119300970526     C                   END                                                    DO
119400970526     C*
119500970526     C                   ENDSR
119600970526     C*------------------------------------------------------------------------*
119700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
119800970526     C*------------------------------------------------------------------------*
119900970526     C     FUNCON        BEGSR
120000970526     C*
120100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
120200970526     C                   EXSR      FUNVAR
120300970526     C*
120400970526     C* PULISCO CAMPI DI LAVORO
120500970526     C                   Z-ADD     0             I                 5 0
120600970526     C                   Z-ADD     0             W                 5 0
120700970526     C*
120800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
120900970526     C     FUNKEY        IFNE      *BLANKS
121000970526     C     1             DO        24            I
121100970526     C                   MOVEL     FUD(I)        FUDRIG
121200970526     C*
121300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
121400970526     C     FUNKEY        IFEQ      FUDFUN
121500970526     C     FUDATT        ANDEQ     'S'
121600970526     C                   Z-ADD     I             W
121700970526     C                   END
121800970526     C                   END                                                    DO WHILE
121900970526     C*
122000970526     C* FUNZIONE NON ATTIVA (ERRORE)
122100970526     C     W             IFEQ      0
122200970526     C*
122300970526     C* EMISSIONE MESSAGGIO DI ERRORE
122400970526     C                   MOVEL     MSG(1)        DSMSMS
122500970526     C*
122600970526     C                   END                                                    W=0
122700970526     C                   END                                                    FUNKEY NE *BLANKS
122800970526     C*
122900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
123000970526     C     W             IFNE      0
123100970526     C     FUNKEY        OREQ      *BLANKS
123200970526     C     FUNKEY        IFEQ      *BLANKS
123300970526     C                   EXSR      ENTGES
123400970526     C                   END
123500970526     C     FUNKEY        IFEQ      '03'
123600970526     C                   EXSR      F03GES
123700970526     C                   END
123800970526     C     FUNKEY        IFEQ      '06'
123900970526     C                   EXSR      F06GES
124000970526     C                   END
124100970526     C     FUNKEY        IFEQ      '12'
124200970526     C                   EXSR      F12GES
124300970526     C                   END
124400970526     C     FUNKEY        IFEQ      '24'
124500970526     C                   EXSR      F24GES
124600970526     C                   END
124700970526     C                   END
124800970526     C*
124900970526     C                   ENDSR
125000970526     C*------------------------------------------------------------------------*
125100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
125200970526     C*------------------------------------------------------------------------*
125300970526     C     FUNVAR        BEGSR
125400970526     C*
125500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
125600970526     C*
125700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
125800970526     C     KEY           IFEQ      ENTER
125900970526     C                   MOVEL     *BLANKS       FUNKEY
126000970526     C                   END
126100970526     C     KEY           IFEQ      F1
126200970526     C                   MOVEL     '01'          FUNKEY
126300970526     C                   END
126400970526     C     KEY           IFEQ      F2
126500970526     C                   MOVEL     '02'          FUNKEY
126600970526     C                   END
126700970526     C     KEY           IFEQ      F3
126800970526     C                   MOVEL     '03'          FUNKEY
126900970526     C                   END
127000970526     C     KEY           IFEQ      F4
127100970526     C                   MOVEL     '04'          FUNKEY
127200970526     C                   END
127300970526     C     KEY           IFEQ      F5
127400970526     C                   MOVEL     '05'          FUNKEY
127500970526     C                   END
127600970526     C     KEY           IFEQ      F6
127700970526     C                   MOVEL     '06'          FUNKEY
127800970526     C                   END
127900970526     C     KEY           IFEQ      F7
128000970526     C                   MOVEL     '07'          FUNKEY
128100970526     C                   END
128200970526     C     KEY           IFEQ      F8
128300970526     C                   MOVEL     '08'          FUNKEY
128400970526     C                   END
128500970526     C     KEY           IFEQ      F9
128600970526     C                   MOVEL     '09'          FUNKEY
128700970526     C                   END
128800970526     C     KEY           IFEQ      F10
128900970526     C                   MOVEL     '10'          FUNKEY
129000970526     C                   END
129100970526     C     KEY           IFEQ      F11
129200970526     C                   MOVEL     '11'          FUNKEY
129300970526     C                   END
129400970526     C     KEY           IFEQ      F12
129500970526     C                   MOVEL     '12'          FUNKEY
129600970526     C                   END
129700970526     C     KEY           IFEQ      F13
129800970526     C                   MOVEL     '13'          FUNKEY
129900970526     C                   END
130000970526     C     KEY           IFEQ      F14
130100970526     C                   MOVEL     '14'          FUNKEY
130200970526     C                   END
130300970526     C     KEY           IFEQ      F15
130400970526     C                   MOVEL     '15'          FUNKEY
130500970526     C                   END
130600970526     C     KEY           IFEQ      F16
130700970526     C                   MOVEL     '16'          FUNKEY
130800970526     C                   END
130900970526     C     KEY           IFEQ      F17
131000970526     C                   MOVEL     '17'          FUNKEY
131100970526     C                   END
131200970526     C     KEY           IFEQ      F18
131300970526     C                   MOVEL     '18'          FUNKEY
131400970526     C                   END
131500970526     C     KEY           IFEQ      F19
131600970526     C                   MOVEL     '19'          FUNKEY
131700970526     C                   END
131800970526     C     KEY           IFEQ      F20
131900970526     C                   MOVEL     '20'          FUNKEY
132000970526     C                   END
132100970526     C     KEY           IFEQ      F21
132200970526     C                   MOVEL     '21'          FUNKEY
132300970526     C                   END
132400970526     C     KEY           IFEQ      F22
132500970526     C                   MOVEL     '22'          FUNKEY
132600970526     C                   END
132700970526     C     KEY           IFEQ      F23
132800970526     C                   MOVEL     '23'          FUNKEY
132900970526     C                   END
133000970526     C     KEY           IFEQ      F24
133100970526     C                   MOVEL     '24'          FUNKEY
133200970526     C                   END
133300970526     C     KEY           IFEQ      ROLLUP
133400970526     C                   MOVEL     'RU'          FUNKEY
133500970526     C                   END
133600970526     C     KEY           IFEQ      HELP
133700970526     C                   MOVEL     'HP'          FUNKEY
133800970526     C                   END
133900970526     C                   ENDSR
134000970526     C*------------------------------------------------------------------------*
134100970526     C* NOTE COSTRUZIONE SCHIERE
134200970526     C*------------------------------------------------------------------------*
134300970526     C*
134400970526     C* -MSG-MESSAGGI
134500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
134600970526     C* (NON TOCCARE MSG,1 E MSG,2)
134700970526     C*
134800970526     C* -CMD-COMANDI
134900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
135000970526     C*
135100970526     C* -OPD-OPZIONI
135200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
135300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
135400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
135500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
135600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
135700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
135800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
135900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
136000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
136100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
136200970526     C*
136300970526     C* -FUD-FUNZIONI
136400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
136500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
136600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
136700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
136800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
136900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
137000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
137100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
137200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
137300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
137400970526     O*------------------------------------------------------------------------*
137500970526** DEC - DECODIFICHE
137600970526    MODIFICA                                                      1
137700970526  ANNULLAMENTO                                                    2
137800970526VISUALIZZAZIONE                                                   3
137900970526   RIPRISTINO                                                     4
138000970526  INSERIMENTO                                                     5
138100970526** MSG - MESSAGGI
138200970528TISI18R- Tasto funzionale non gestito                                          1
138300970528TISI18R- Inserimento effettuato                                                2
138400970528TISI18R- Modifica effettuata                                                   3
138500970528TISI18R- Annullamento effettuato                                               4
138600970528TISI18R- Ripristino effettuato                                                 5
138700970528TISI18R- Richiesta funzione per record inesistente                             6
138800970528TISI18R- Record momentaneamente allocato: riprovare più tardi                  7
138900970528TISI18R- Richiesta funzione per record esistente                               8
139000970528TISI18R- 9
139100090115TISI18R- Il record immesso si sovrappone ad uno già in decorrenza.ENTER forza  10
139200030310TISI18R- Immettere P=Partenza o A=Arrivo o 6=£6 o C=Coop                       11
139300970528TISI18R- Terminal / Linea partenza errato                                      12
139400970528TISI18R- Terminal / Linea arrivo errato                                        13
139500970528TISI18R- Data errata                                                           14
139600970528TISI18R- La data scadenza deve essere maggiore della decorrenza                15
139700120214TISI18R- Il record non può scadere in un giorno già trascorso.                 16
139800971203TISI18R- Il record immesso si sovrappone ad uno già in decorrenza              17
139900971203TISI18R- Terminal / linea errati                                               18
140000060706TISI18R- Impossibile inserire terminal Arrivo per tipo "P" o "O"               19
140100970618TISI18R- L'eccezione non può essere uguale al terminal arrivo standard         20
140200030310TISI18R- Per tipo terminal "6" o "C" non inserire il terminal partenza         21
140300020507TISI18R- Scadenza incongruente con scadenza record tipo "6"                    22
140400020507TISI18R- Esistono dei record di tipo "6". Impossibile la modifica              23
140500080806TISI18R- Il p.o. gestione ha il terminal di Arrivo   diverso dal p.o. gestito  24
140600080806TISI18R- Data decorrenza/scadenza incongruenti con le date del terminal Arr.   25
140700120214TISI18R- Il record non può scadere in un giorno già trascorso.ENTER forza      26
140800970526** FUD - FUNZIONI
140900970526                   -        (S/N)=N                        funz.01¬
141000970526                   -        (S/N)=N                        funz.02¬
141100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
141200970526                   -        (S/N)=N                        funz.04¬
141300970526                   -        (S/N)=N                        funz.05¬
141400970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
141500970527                   -        (S/N)=N                        funz.07¬
141600970527                   -        (S/N)=N                        funz.08¬
141700970527                   -        (S/N)=N                        funz.09¬
141800970527                   -        (S/N)=N                        funz.10¬
141900970526                   -        (S/N)=N                        funz.11¬
142000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
142100970526                   -        (S/N)=N                        funz.13¬
142200970526                   -        (S/N)=N                        funz.14¬
142300970526                   -        (S/N)=N                        funz.15¬
142400970526                   -        (S/N)=N                        funz.16¬
142500970526                   -        (S/N)=N                        funz.17¬
142600970526                   -        (S/N)=N                        funz.18¬
142700970526                   -        (S/N)=N                        funz.19¬
142800970526                   -        (S/N)=N                        funz.20¬
142900970526                   -        (S/N)=N                        funz.21¬
143000970526                   -        (S/N)=N                        funz.22¬
143100970526                   -        (S/N)=N                        funz.23¬
143200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
