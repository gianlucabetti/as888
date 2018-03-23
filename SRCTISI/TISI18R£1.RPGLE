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
002500020507     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
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
008500970526     C*------------------------------------------------------------------------*
008600970526     C* MAIN LINES
008700970526     C*------------------------------------------------------------------------*
008800970526     C*
008900970526     C* NON CI SONO ERRORI IN ENTRATA
009000970526IF  1C     ERRL00        IFEQ      '0'
009100970526     C*
009200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009600970526     C*
009700970526     C* EMETTE FORMATO VIDEO
009800970526     C                   EXSR      BUTFMR
009900970526     C*
010000970526     C* GESTIONE TASTI FUNZIONALI
010100970526     C                   EXSR      FUNCON
010200970526E   2C                   ENDDO
010300970526     C*
010400970526     C* CI SONO ERRORI IN ENTRATA
010500970527X   1C                   ELSE
010600970526     C                   MOVEL     '1'           D00ERR
010700970526     C                   MOVEL     DSMSMS        D00MSG
010800970526E   1C                   ENDIF
010900970526     C* FINE PGM
011000970526     C                   SETON                                        LR
011100970526     C*------------------------------------------------------------------------*
011200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011300970526     C*------------------------------------------------------------------------*
011400970526     C     BUTFMR        BEGSR
011500970526     C*
011600970526     C* EMETTE LA TESTATA
011700970528     C                   WRITE     SI18D01
011800970526     C*
011900970526     C* SPEDISCE EVENTUALE MESSAGGIO
012000970526     C                   EXSR      SNDMSG
012100970526     C*
012200970526     C* EMETTE LE FUNZIONI
012300970528     C                   WRITE     SI18D02
012400970526     C*
012500970526     C* EMETTE IL VIDEO
012600970528     C                   EXFMT     SI18D03
012700970526     C*
012800970526     C* REIMPOSTA FLAG DI ERRORE
012900970526     C                   MOVEL     '0'           ERRL00
013000970526     C*
013100970526     C* REIMPOSTA INDICATORI DI ERRORE
013200970526     C                   SETOFF                                         5859
013300970526     C                   SETOFF                                       606162
013400970526     C                   SETOFF                                       636465
013500970526     C                   SETOFF                                       666768
013600970526     C                   SETOFF                                       697071
013700970526     C                   SETOFF                                       727374
013800970526     C                   SETOFF                                       757677
013900970526     C                   SETOFF                                       787980
014000970526     C                   SETOFF                                       818283
014100970526     C                   SETOFF                                       848586
014200970526     C                   SETOFF                                       878889
014300970526     C*
014400970526     C                   ENDSR
014500970526     C*------------------------------------------------------------------------*
014600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014700970526     C*------------------------------------------------------------------------*
014800970526     C     SNDMSG        BEGSR
014900970526     C*
015000970526     C* PULISCE IL SFILE MESSAGGI
015100970526     C                   MOVEL     '1'           *IN40
015200970528     C                   WRITE     SI18DMC
015300970526     C                   MOVEL     '0'           *IN40
015400970526     C*
015500970526     C* VISUALIZZA IL MESSAGGIO
015600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015700970526     C                   CALL      'XMSG'        XMSGPA
015800970526IF  1C     DSMSER        IFEQ      *ZEROS
015900970526     C                   MOVEL     DSMSKE        MSGKEY
016000970526E   1C                   ENDIF
016100970526     C                   Z-ADD     1             MSGULT
016200970528     C                   WRITE     SI18DMS
016300970528     C                   WRITE     SI18DMC
016400970526     C*
016500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016600970526     C                   MOVEL     *BLANKS       DSMSMS
016700970526     C*
016800970526     C                   ENDSR
016900970526     C*------------------------------------------------------------------------*
017000970526     C* ENTGES - GESTIONE ENTER -> INVIO
017100970526     C*------------------------------------------------------------------------*
017200970526     C     ENTGES        BEGSR
017300970526     C*
017400970526     C* CONTROLLA I CAMPI IMMESSI
017500970526     C                   EXSR      DECODI
017600970526     C*
017700970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
017800970526IF  1C     DEPOP1        IFEQ      'O05'
017900970526     C                   MOVEL     '1'           FINL00
018000970526E   1C                   ENDIF
018100970526     C*
018200970526     C                   ENDSR
018300970526     C*------------------------------------------------------------------------*
018400970526     C* F03GES - GESTIONE F03 -> FINE
018500970526     C*------------------------------------------------------------------------*
018600970526     C     F03GES        BEGSR
018700970526     C*
018800970526     C* IMPOSTA "PREMUTO F03"
018900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019000970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
019100970526     C*
019200970526     C                   ENDSR
019300970526     C*------------------------------------------------------------------------*
019400000515     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019500970526     C*------------------------------------------------------------------------*
019600970526     C     F06GES        BEGSR
019700970526     C*
019800970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
019900970526IF  1C     DEPOP1        IFEQ      'F06'
020000970526     C                   EXSR      INSERI
020100970526E   1C                   ENDIF
020200970526IF  1C     DEPOP1        IFEQ      'O02'
020300970526     C                   EXSR      MODIFI
020400970526E   1C                   ENDIF
020500970526IF  1C     DEPOP1        IFEQ      'O04'
020600970526     C                   EXSR      ANNULL
020700970526E   1C                   ENDIF
020800970526IF  1C     DEPOP1        IFEQ      'O07'
020900970526     C                   EXSR      RIPRIS
021000970526E   1C                   ENDIF
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F12GES - GESTIONE F12 -> RITORNO
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F12GES        BEGSR
021700970526     C*
021800970526     C* IMPOSTA "PREMUTO F12"
021900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022000970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022500970526     C*------------------------------------------------------------------------*
022600970526     C     F24GES        BEGSR
022700970526     C*
022800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
023300970526     C*------------------------------------------------------------------------*
023400970526     C     INSERI        BEGSR
023500970526     C*
023600970526     C* CONTROLLA I CAMPI IMMESSI
023700000515     C                   EXSR      DECODI
023800970526     C*
023900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024100970618     C                   MOVEL     VIDEPA        CAEEPA                         *CHIAVE
024200970618     C                   MOVEL     TFPVID        CAETFP
024300970618     C                   MOVEL     TFAVID        CAETFA
024400970618     C                   Z-ADD     DDEVID        CAEDDE
024500970618     C                   EXSR      IMPCAE                                       *ALTRI CAMPI
024600970618     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
024700970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
024800970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
024900970618     C                   WRITE     AZCAE000
025000000427     C*
025100000427     C                   EXSR      LETCAE
025200970526     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
025300970602     C*
025400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
025500970602     C                   EXSR      AZZVID
025600970526E   1C                   ENDIF
025700970526     C*
025800970526     C                   ENDSR
025900970526     C*------------------------------------------------------------------------*
026000970526     C* MODIFI - OPERAZIONI DI MODIFICA
026100970526     C*------------------------------------------------------------------------*
026200970526     C     MODIFI        BEGSR
026300970526     C*
026400970526     C* CONTROLLA I CAMPI IMMESSI
026500970526     C                   EXSR      DECODI
026600970526     C*
026700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
026800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026900970618     C                   EXSR      LETCAE
027000030310     C*
027100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
027200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027300970618     C                   EXSR      IMPCAE                                       *IMPOSTA I CAMPI
027400970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
027500970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
027600970618     C                   UPDATE    AZCAE000
027700000427     C*
027800970526     C                   MOVEL     MSG(3)        D00MSG
027900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
028000970526E   2C                   ENDIF
028100970526E   1C                   ENDIF
028200970526     C*
028300970526     C                   ENDSR
028400970526     C*------------------------------------------------------------------------*
028500970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
028600970526     C*------------------------------------------------------------------------*
028700970526     C     ANNULL        BEGSR
028800970526     C*
028900040624     c                   exsr      sr_atten
029000040624     c*
029100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
029200000602     C                   MOVEL     '0'           ANNDLT            1
029300970618     C                   EXSR      LETCAE
029400970527     C*
029500970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
029600970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029700970618IF  2C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
029800970618IF  3C     CAEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
029900970618     C                   MOVEL     'A'           CAEATB                         *FLAG ANNULLAMENTO
030000970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
030100970618     C                   UPDATE    AZCAE000                                     *ANNULLA
030200970526X   3C                   ELSE                                                   *MAI TRASMESSO
030300970618     C                   DELETE    AZCAE000                                     *CANCELLA
030400000602     C                   MOVEL     '1'           ANNDLT
030500970526E   3C                   ENDIF
030600970526E   2C                   ENDIF
030700970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
030800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
030900970526E   1C                   ENDIF
031000970526     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
031400970526     C*------------------------------------------------------------------------*
031500970526     C     RIPRIS        BEGSR
031600970526     C*
031700970526     C* CONTROLLA I CAMPI IMMESSI
031800970526     C                   EXSR      DECODI
031900970526     C*
032000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032200970618     C                   EXSR      LETCAE
032300970526     C*
032400970526     C* IMPOSTA IL RECORD DA SCRIVERE
032500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032600970618     C                   MOVEL     *BLANKS       CAEATB                         *FLAG ANNULLAMENTO
032700970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
032800970618     C                   UPDATE    AZCAE000                                     *ANNULLA
032900970526E   2C                   ENDIF
033000970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
033100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033200970526E   1C                   ENDIF
033300970526     C*
033400970526     C                   ENDSR
033500970526     C*------------------------------------------------------------------------*
033600970618     C* LETCAE - LEGGE IL RECORD PERL'AGGIORNAMENTO
033700970526     C*------------------------------------------------------------------------*
033800970618     C     LETCAE        BEGSR
033900970526     C*
034000000427     C     DEPOP1        IFNE      'F06'
034100980127     C                   MOVEL     D00EPA        KCEEPA
034200980127     C                   MOVEL     D00ETP        KCETFP
034300980127     C                   MOVEL     D00ETA        KCETFA
034400980127     C                   Z-ADD     D00EDD        KCEDDE
034500970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
034600970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
034700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034800970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
034900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035000970526E   1C                   ENDIF
035100970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
035200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035300970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
035400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035500970526E   1C                   ENDIF
035600000427     C                   ENDIF
035700970526     C*
035800970526     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
036100970526     C*------------------------------------------------------------------------*
036200970526     C     DECODI        BEGSR
036300970528     C*--------------------
036400970528     C* PARTENZA/ARRIVO
036500970528     C*--------------------
036600970528IF  1C     ERRL00        IFEQ      '0'
036700970528IF  2C     VIDEPA        IFEQ      'P'
036800970528     C     VIDEPA        OREQ      'A'
036900020418     C     VIDEPA        OREQ      '6'
037000030310     C     VIDEPA        OREQ      'C'
037100060706     C     VIDEPA        OREQ      'O'
037200970528X   2C                   ELSE
037300970528     C                   MOVEL     '1'           ERRL00
037400970528     C                   SETON                                        60
037500970528     C                   MOVEL     MSG(11)       DSMSMS
037600970528E   2C                   ENDIF
037700970528E   1C                   ENDIF
037800970528     C*--------------------
037900970528     C* TERMINAL / LINEA DI PARTENZA
038000970528     C*--------------------
038100970618     C     *LIKE         DEFINE    VIDTFP        TFPVID
038200020418IF  1c                   if        errl00 = *off
038300020418     c                   clear                   vddtfp
038400030310     c                   if        videpa = '6' and vidtfp > *zeros or
038500030310     c                             videpa = 'C' and vidtfp > *zeros
038600030310     c                   eval      errl00 = *on
038700020418     c                   eval      *in61 = *on
038800020418     c                   eval      dsmsms = msg(21)
038900020418     c                   endif
039000020418     c                   endif
039100020418
039200970528IF  1C     ERRL00        IFEQ      '0'
039300970618     C                   MOVEL     *BLANKS       VDDTFP
039400030310     c                   if        (videpa ='A' or videpa='6' or videpa='C') and
039500020418     c                             (vidtfp = *blanks or vidtfp = *zeros)
039600970618     C                   MOVEL     *ZEROS        TFPVID
039700970618X   2C                   ELSE
039800970618     C                   MOVEL     VIDTFP        WLIN
039900970618     C                   MOVEL     *BLANKS       WDLIN
040000970618     C                   EXSR      CHKLIN
040100970618     C                   MOVEL     WLIN          VIDTFP
040200970618     C                   MOVEL     WDLIN         VDDTFP
040300970618     C                   MOVEL     VIDTFP        TFPVID
040400970618IF  3C     ERRL00        IFEQ      '1'
040500970528     C                   SETON                                        61
040600970618     C                   MOVEL     MSG(12)       DSMSMS
040700970618E   3C                   ENDIF
040800970618E   2C                   ENDIF
040900970528E   1C                   ENDIF
041000970528     C*--------------------
041100970528     C* TERMINAL / LINEA DI ARRIVO
041200970528     C*--------------------
041300970618     C     *LIKE         DEFINE    VIDTFA        TFAVID
041400970528IF  1C     ERRL00        IFEQ      '0'
041500970618     C                   MOVEL     *BLANKS       VDDTFA
041600060706     c                   if        (videpa ='P' or videpa='O') and
041700060706     c                             (vidtfa = *blanks or vidtfa = *zeros)
041800970618     C                   MOVEL     *ZEROS        TFAVID
041900970618X   2C                   ELSE
042000970528     C                   MOVEL     VIDTFA        WLIN
042100970528     C                   MOVEL     *BLANKS       WDLIN
042200970528     C                   EXSR      CHKLIN
042300970528     C                   MOVEL     WLIN          VIDTFA
042400970528     C                   MOVEL     WDLIN         VDDTFA
042500970618     C                   MOVEL     VIDTFA        TFAVID
042600970618IF  3C     ERRL00        IFEQ      '1'
042700970528     C                   SETON                                        62
042800970528     C                   MOVEL     MSG(13)       DSMSMS
042900970618E   3C                   ENDIF
043000970618E   2C                   ENDIF
043100970528E   1C                   ENDIF
043200970618     C*
043300060706     C* NO ECCEZIONE IN PARTENZA o per ottico
043400970618IF  1C     ERRL00        IFEQ      '0'
043500060706     c                   if        videpa='P' or videpa='O'
043600970618IF  3C     VIDTFP        IFGT      *ZEROS
043700970618     C     VIDTFA        ANDGT     *ZEROS
043800970618     C     VIDTFE        ANDGT     *ZEROS
043900060706     C                   SETON                                        62
044000970618     C                   MOVEL     MSG(19)       DSMSMS
044100970618     C                   MOVEL     '1'           ERRL00
044200970618E   3C                   ENDIF
044300970618E   2C                   ENDIF
044400970618E   1C                   ENDIF
044500970528     C*--------------------
044600970528     C* DATA DECORRENZA
044700970528     C*--------------------
044800970528     C     *LIKE         DEFINE    VIDDDE        DDEVID
044900970528IF  1C     ERRL00        IFEQ      '0'
045000970528     C                   Z-ADD     VIDDDE        WDA8
045100970528     C                   Z-ADD     *ZEROS        DA8W
045200970528     C                   EXSR      CHKDA8
045300970528     C                   Z-ADD     WDA8          VIDDDE
045400090115     c* pulisco flag di forzatura sulla date se cambiano
045500090115     c                   if        da8w<>ddevid
045600090115     c                   clear                   forzater
045700090115     c                   endif
045800970528     C                   Z-ADD     DA8W          DDEVID
045900970528IF  2C     ERRL00        IFEQ      '1'
046000970528     C                   SETON                                        63
046100970528     C                   MOVEL     MSG(14)       DSMSMS
046200970528E   2C                   ENDIF
046300970528E   1C                   ENDIF
046400970528     C* INSERIMENTO -> RECORD INESISTENTE
046500970528IF  1C     ERRL00        IFEQ      '0'
046600970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
046700970618     C                   MOVEL     VIDEPA        KCEEPA
046800970618     C                   MOVEL     TFPVID        KCETFP
046900970618     C                   MOVEL     TFAVID        KCETFA
047000970618     C                   Z-ADD     DDEVID        KCEDDE
047100970618     C     KEYCAE        CHAIN(N)  AZCAE01L                           99
047200970528IF  3C     *IN99         IFEQ      *OFF
047300970528     C                   MOVEL     '1'           ERRL00
047400970528     C                   MOVEL     MSG(8)        DSMSMS
047500970528     C                   SETON                                        606162
047600970528     C                   SETON                                        63
047700970528E   3C                   ENDIF
047800970528E   2C                   ENDIF
047900970528E   1C                   ENDIF
048000970528     C*--------------------
048100970528     C* DATA SCADENZA
048200970528     C*--------------------
048300970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
048400970528IF  1C     ERRL00        IFEQ      '0'
048500970528     C                   Z-ADD     VIDDSC        WDA8
048600970528     C                   Z-ADD     *ZEROS        DA8W
048700970528     C                   EXSR      CHKDA8
048800970528     C                   Z-ADD     WDA8          VIDDSC
048900090115     c                   if        da8w<>dscvid
049000090115     c                   clear                   forzater
049100090115     c                   endif
049200970528     C                   Z-ADD     DA8W          DSCVID
049300970528IF  2C     ERRL00        IFEQ      '1'
049400970528     C                   SETON                                        64
049500970528     C                   MOVEL     MSG(14)       DSMSMS
049600970528E   2C                   ENDIF
049700970528E   1C                   ENDIF
049800970528     C* SCADENZA > DECORRENZA
049900970528IF  1C     ERRL00        IFEQ      '0'
050000970528IF  2C     DSCVID        IFLT      DDEVID
050100970619     C                   SETON                                        64
050200970528     C                   MOVEL     MSG(15)       DSMSMS
050300970528     C                   MOVEL     '1'           ERRL00
050400970528E   2C                   ENDIF
050500970528E   1C                   ENDIF
050600970528     C* SCADENZA >= DATA CORRENTE
050700050412     c* Per EDP e per terminal partenza e £6, non faccio il controllo
050800970528IF  1C     ERRL00        IFEQ      '0'
050900050412     c                   if        (winizioprf='EDP' and videpa='P') or
051000050412     c                             (winizioprf='EDP' and videpa='6')
051100050412     c                   else
051200970528IF  2C     DEPOP1        IFEQ      'O02'
051300970528     C     DEPOP1        OREQ      'O07'
051400970528     C     DEPOP1        OREQ      'F06'
051500970528IF  3C     DSCVID        IFLT      DATCOR
051600970619     C                   SETON                                        64
051700970528     C                   MOVEL     MSG(16)       DSMSMS
051800970528     C                   MOVEL     '1'           ERRL00
051900970528E   3C                   ENDIF
052000050412E   3C                   ENDIF
052100970528E   2C                   ENDIF
052200970528E   1C                   ENDIF
052300970528     C*---
052400970528     C* DECORRENZA + SCADENZA
052500970528     C*---
052600970528     C* ACCAVALLAMENTO DATI
052700970528IF  1C     ERRL00        IFEQ      '0'
052800090115     c     forzater      andeq     ' '
052900970618     C                   MOVEL     VIDEPA        KCEEPA
053000970618     C                   MOVEL     TFPVID        KCETFP
053100970618     C                   MOVEL     TFAVID        KCETFA
053200970618     C     KE2CAE        SETLL     AZCAE01L
053300970618     C     KE2CAE        READE(N)  AZCAE01L                               99
053400970528DO  2C     *IN99         DOWEQ     *OFF
053500970528IF  3C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
053600970618     C     DDEVID        ANDNE     CAEDDE                                       *RECORD IN LINEA
053700970528     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
053800970618IF  4C     CAEATB        IFEQ      *BLANKS
053900970618IF  5C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
054000970618     C     DDEVID        ANDLE     CAEDSC
054100970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
054200970618     C     DSCVID        ANDLE     CAEDSC
054300970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
054400970618     C     CAEDSC        ANDLE     DSCVID
054500970528     C                   SETON                                        606162
054600970619     C                   SETON                                        6364
054700970528     C                   MOVEL     MSG(17)       DSMSMS
054800970528     C                   MOVEL     '1'           ERRL00
054900090115     c                   if        (winizioprf='EDP' and videpa='P') or
055000090115     c                             (winizioprf='EDP' and videpa='6')
055100090115     c                   eval      forzater='S'
055200090115     C                   MOVEL     MSG(10)       DSMSMS
055300090115     c                   endif
055400090115     c
055500970528     C                   LEAVE
055600970528E   5C                   ENDIF
055700970528E   4C                   ENDIF
055800970528E   3C                   ENDIF
055900970618     C     KE2CAE        READE(N)  AZCAE01L                               99
056000970528E   2C                   ENDDO
056100970528E   1C                   ENDIF
056200970528     C*--------------------
056300970528     C* TERMINAL / LINEA ECCEZIONE
056400970528     C*--------------------
056500970528IF  1C     ERRL00        IFEQ      '0'
056600970528     C                   MOVEL     VIDTFE        WLIN
056700970528     C                   MOVEL     *BLANKS       WDLIN
056800970528     C                   EXSR      CHKLIN
056900970528     C                   MOVEL     WLIN          VIDTFE
057000970528     C                   MOVEL     WDLIN         VDDTFE
057100970528IF  2C     ERRL00        IFEQ      '1'
057200970528     C                   SETON                                        65
057300970528     C                   MOVEL     MSG(18)       DSMSMS
057400970528E   2C                   ENDIF
057500970528E   1C                   ENDIF
057600970528     C*
057700970618     C* SE ECCEZIONE IN ARRIVO, ECCEZIONE <> ARRIVO STANDARD
057800970618IF  1C     ERRL00        IFEQ      '0'
057900970618IF  2C     VIDEPA        IFEQ      'A'
058000970618IF  3C     VIDTFP        IFGT      *ZEROS
058100970618     C     VIDTFA        ANDGT     *ZEROS
058200970618     C     VIDTFE        ANDGT     *ZEROS
058300970618     C                   MOVEL     'A'           KCEEPA
058400970618     C                   Z-ADD     *ZEROS        KCETFP
058500970618     C                   MOVEL     TFAVID        KCETFA
058600970618     C     KE2CAE        SETLL     AZCAE01L
058700970618     C     KE2CAE        READE(N)  AZCAE01L                               99
058800970618DO  4C     *IN99         DOWEQ     *OFF
058900970618IF  5C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
059000970618IF  5C     CAETFE        IFEQ      VIDTFE
059100970618IF  7C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
059200970618     C     DDEVID        ANDLE     CAEDSC
059300970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
059400970618     C     DSCVID        ANDLE     CAEDSC
059500970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
059600970618     C     CAEDSC        ANDLE     DSCVID
059700970618     C                   SETON                                        606162
059800970618     C                   SETON                                        63
059900970618     C                   MOVEL     MSG(20)       DSMSMS
060000970618     C                   MOVEL     '1'           ERRL00
060100970618     C                   LEAVE
060200970618E   7C                   ENDIF
060300970618E   6C                   ENDIF
060400970618E   5C                   ENDIF
060500970618     C     KE2CAE        READE(N)  AZCAE01L                               99
060600970618E   4C                   ENDDO                                                  *FINE TERM.ARR.STAND
060700970618E   3C                   ENDIF
060800970618E   2C                   ENDIF
060900970618E   1C                   ENDIF
061000020507
061100020507      * controlli per incongruenze tra record di tipo 'P' (partenza) e record di tip '6' (£6)
061200020507 b1  c                   if        errl00 = *off and
061300020507     c                             (videpa = 'P' or videpa = '6')
061400020507     c                   exsr      chk£6
061500020507 e1  c                   endif
061600040624     c
061700040624     c                   exsr      sr_atten
061800970618     C*
061900970528     C                   ENDSR
062000040624     C*------------------------------------------------------------------------*
062100040624     C* SR_atten -  Emissione messaggio informativo
062200040624     C*------------------------------------------------------------------------*
062300040624     C     SR_atten      BEGSR
062400040624     c     ERRL00        ifeq      '0'
062500040624     c     videpa        andeq     'A'
062600040624     c     funkey        andeq     '06'
062700040624     c     wforz         andeq     *blanks
062800040624     c* modificata linea o data
062900040624     c     depop1        ifeq      'O02'
063000040624     c     sav_vidtfe    andne     vidtfe
063100040624     c     depop1        oreq      'O02'
063200040624     c     sav_viddsc    andne     viddsc
063300040624     c* ripristino
063400040624     c     depop1        oreq      'O07'
063500040624     c* cancellazione
063600040624     c     depop1        oreq      'O04'
063700040624     c                   eval      wforz='F'
063800040624     c                   eval      errl00='1'
063900040624     c                   seton                                        41
064000040624     c                   endif
064100040624     c                   endif
064200040624     c                   endsr
064300970528     C*------------------------------------------------------------------------*
064400970528     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
064500970528     C*------------------------------------------------------------------------*
064600970528     C     CHKLIN        BEGSR
064700970528     C*
064800970618     C     *LIKE         DEFINE    VIDTFA        WLIN
064900970618     C     *LIKE         DEFINE    VDDTFA        WDLIN
065000970528     C*
065100970528     C* RICHIESTO ELENCO
065200970528     C                   SETOFF                                           28
065300970528     C     '?'           SCAN      WLIN                                   28
065400970528IF  1C     *IN28         IFEQ      *ON
065500970528     C                   MOVEL     *BLANKS       §COD1             3
065600970528     C                   MOVEL     *BLANKS       §TIP              1
065700970528     C                   MOVEL     *BLANKS       §DES1            25
065800971203     C                   MOVEL     *BLANKS       §PRA              3
065900971203     C                   CALL      'TNSD24R'
066000970528     C                   PARM                    §COD1
066100970528     C                   PARM                    §TIP
066200970528     C                   PARM                    §DES1
066300971203     C                   PARM                    §PRA
066400970528     C                   MOVEL     §COD1         WLIN
066500970528     C                   MOVEL     §DES1         WDLIN
066600970528E   1C                   ENDIF
066700970528     C*
066800970528     C* SOLO CARATTERI NUMERICI
066900970528     C                   SETOFF                                       28
067000970528     C                   TESTN                   WLIN                 28
067100970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
067200970528     C                   MOVEL     '1'           ERRL00
067300970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
067400970528     C                   MOVE      WLIN          W001              1
067500970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
067600970528     C                   MOVEL     '1'           ERRL00
067700970528X   2C                   ELSE                                                   *NUMERO
067800970528     C*
067900970528     C* CONTROLLA ESISTENZA CODICE
068000970528     C                   MOVEL     WLIN          KORFIL
068100970528     C     KEYORG        CHAIN     AZORG01L                           99
068200970528IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
068300970528     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
068400970528     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
068500970528     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
068600970528X   3C                   ELSE
068700970528     C                   MOVEL     '1'           ERRL00
068800970528E   3C                   ENDIF
068900970528E   2C                   ENDIF
069000970528E   1C                   ENDIF
069100970528     C*
069200970528     C                   ENDSR
069300970528     C*------------------------------------------------------------------------*
069400970528     C* CHKDA8 - CONTROLLA LA DATA (8)
069500970528     C*------------------------------------------------------------------------*
069600970528     C     CHKDA8        BEGSR
069700970528     C*
069800970528     C     *LIKE         DEFINE    VIDDDE        WDA8
069900970528     C     *LIKE         DEFINE    VIDDDE        DA8W
070000970528     C*
070100970528     C                   Z-ADD     WDA8          G08DAT
070200970528     C                   Z-ADD     *ZEROS        G08INV
070300970528     C                   MOVEL     '0'           G08ERR
070400970528     C                   CALL      'XSRDA8'
070500970528     C                   PARM                    WLBDA8
070600970528     C                   Z-ADD     G08DAT        WDA8
070700970528     C                   Z-ADD     G08INV        DA8W
070800970528IF  1C     G08ERR        IFEQ      '1'
070900970528     C                   MOVEL     '1'           ERRL00
071000970528E   1C                   ENDIF
071100970528     C*
071200970528     C                   ENDSR
071300020506      *------------------------------------------------------------------------*
071400080806      * CHK£6  - CONTROLLA INCONGRUENZE TRA RECOD 'A' E RECORD '6'
071500020506      *------------------------------------------------------------------------*
071600020507     c     chk£6         begsr
071700080806      ***
071800020507      * Record tipo 'P'
071900020507      *----------------
072000080806     c***                if        videpa = 'A'
072100080806     c***                eval      kceepa = '6'
072200080806     c***  ke1cae        setll     azcae01l
072300080806     c***                do        *hival
072400080806     c***  ke1cae        reade(n)  azcae01l
072500080806     c***                if        %eof(azcae01l)
072600080806     c***                leave
072700080806     c***                endif
072800080806     c***                if        caeatb<>*blanks
072900080806     c***                iter
073000080806     c***                endif
073100080806     c***
073200080806     c***                movel     caetfa        wcaetfa
073300080806     c***                movel     caetfe        wcaetfe
073400080806      **controllo la congruenza della scadenza tra record 'A' e record '6'
073500080806     c***                if        sav_viddsc <> viddsc and
073600080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid) and
073700080806     c***                          caedsc > dscvid
073800080806     c***                eval      *in64 = *on
073900080806     c***                eval      dsmsms = msg(22)
074000080806     c***                eval      errl00 = *on
074100080806     c***                leave
074200080806     c***                endif
074300080806      **controllo che non esistano ancora dei record di tipo '6'
074400080806     c***                if        sav_vidtfe <> vidtfe and
074500080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid)
074600080806     c***                eval      *in65 = *on
074700080806     c***                eval      dsmsms = msg(23)
074800080806     c***                eval      errl00 = *on
074900080806     c***                leave
075000080806     c***                endif
075100080806     c***                enddo
075200080806      ***
075300080806     c***                endif
075400080806      ***
075500080806      **Record tipo '6'
075600080806      *----------------
075700080806     c***                if        videpa = '6'
075800080806     c***                eval      kceepa = 'A'
075900080806     c***                eval      ok_date = *off
076000080806     c***                clear                   sav_tfe1
076100080806      **controllo il terminal di partenza del p.o. gestito
076200080806     c***                movel     vidtfa        kcetfp
076300080806     c***                clear                   kcetfa
076400080806     c***  ke2cae        setll     azcae01l
076500080806     c***                do        *hival
076600080806     c***  ke2cae        reade(n)  azcae01l
076700080806      **fine file
076800080806     c***                if        %eof(azcae01l)
076900080806     c***                leave
077000080806     c***                endif
077100080806      ***
077200080806     c***                eval      *in63 = *off
077300080806     c***                eval      *in64 = *off
077400080806      **controllo le date
077500080806     c***                if        ddevid >= caedde and dscvid <= caedsc
077600080806     c***                eval      sav_tfe1 = caetfe
077700080806     c***                eval      ok_date = *on
077800080806     c***                leave
077900080806      ***
078000080806      **controllo quale data non corrisponde
078100080806     c***                if        ok_date = *off
078200080806     c***                eval      *in63 = (ddevid < caedde)
078300080806     c***                eval      *in64 = (dscvid > caedsc)
078400080806     c***                endif
078500080806      ***
078600080806     c***                enddo
078700080806      **le date non corrispondono errore
078800080806     c***                if        ok_date = *off and (*in63 or *in64)
078900080806     c***                eval      dsmsms = msg(25)
079000080806     c***                eval      errl00 = *on
079100080806     c***                endif
079200080806
079300080806      **controllo il terminal di partenza del p.o. gestione
079400080806     c***                if        errl00 = *off
079500080806     c***                eval      ok_date = *off
079600080806     c***                clear                   sav_tfe2
079700080806     c***                movel     vidtfe        kcetfp
079800080806     c***                clear                   kcetfa
079900080806     c***  ke2cae        setll     azcae01l
080000080806     c***                do        *hival
080100080806     c***  ke2cae        reade(n)  azcae01l
080200020516      * fine file
080300080806     C***                if        %eof(azcae01l)
080400080806     c***                leave
080500080806     c***                endif
080600080806      ***
080700080806     c***                eval      *in63 = *off
080800080806     c***                eval      *in64 = *off
080900080806      **controllo le date
081000080806     c***                if        ddevid >= caedde and dscvid <= caedsc
081100080806     c***                eval      sav_tfe2 = caetfe
081200080806     c***                eval      ok_date = *on
081300080806     c***                leave
081400080806     c***                endif
081500020516      * controllo quale data non corrisponde
081600080806     c***                if        ok_date = *off
081700080806     c***                eval      *in63 = (ddevid < caedde)
081800080806     c***                eval      *in64 = (dscvid > caedsc)
081900080806     c***                endif
082000080806      ***
082100080806     c***                enddo
082200080806      **le date non corrispondono errore
082300080806     c***                if        ok_date = *off and (*in63 or *in64)
082400080806     c***                eval      dsmsms = msg(25)
082500080806     c***                eval      errl00 = *on
082600080806     c***                endif
082700080806      ***
082800080806     c***                endif
082900080806      ***
083000080806      **controllo se i 2 terminal  corrispondono
083100080806     c***                if        errl00 = *off and
083200080806     c***                          sav_tfe1 <> sav_tfe2
083300080806     c***                eval      *in65 = *on
083400080806     c***                eval      dsmsms = msg(24)
083500080806     c***                eval      errl00 = *on
083600080806     c***                endif
083700080806      ***
083800080806     c***                endif
083900020506
084000020506     c                   endsr
084100970526     C*------------------------------------------------------------------------*
084200970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
084300970526     C*------------------------------------------------------------------------*
084400970526     C     IREVID        BEGSR
084500090115     c                   clear                   forzater
084600970526     C* CAMPI
084700970618     C                   MOVEL     CAEEPA        VIDEPA
084800970618     C                   MOVEL     CAETFP        VIDTFP
084900970618     C                   MOVEL     CAETFA        VIDTFA
085000970618     C                   Z-ADD     CAEDDE        G08INV
085100970528     C                   Z-ADD     *ZEROS        G08DAT
085200970528     C                   MOVEL     '3'           G08ERR
085300970528     C                   CALL      'XSRDA8'
085400970528     C                   PARM                    WLBDA8
085500970528     C                   Z-ADD     G08DAT        VIDDDE
085600970618     C                   Z-ADD     CAEDSC        G08INV
085700970528     C                   Z-ADD     *ZEROS        G08DAT
085800970528     C                   MOVEL     '3'           G08ERR
085900970528     C                   CALL      'XSRDA8'
086000970528     C                   PARM                    WLBDA8
086100970528     C                   Z-ADD     G08DAT        VIDDSC
086200970618     C                   Z-ADD     CAETFE        VIDTFE
086300020510
086400020510      * salvo data scadenza e terminal per controlli congruenze tra record
086500020510      * di tipo 'P' e record di tipo '6'
086600020510     c                   eval      sav_viddsc = viddsc
086700020510     c                   eval      sav_vidtfe = vidtfe
086800970527     C*
086900970526     C                   ENDSR
087000970526     C*------------------------------------------------------------------------*
087100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
087200970526     C*------------------------------------------------------------------------*
087300970526     C     AZZVID        BEGSR
087400970526     C* CAMPI
087500090115     c                   clear                   forzater
087600970528     C                   MOVEL     *BLANKS       VIDEPA
087700970618     C                   MOVEL     *BLANKS       VIDTFP
087800970618     C                   MOVEL     *BLANKS       VIDTFA
087900970528     C                   Z-ADD     *ZEROS        VIDDDE
088000970528     C                   Z-ADD     *ZEROS        VIDDSC
088100970618     C                   Z-ADD     *ZEROS        VIDTFE
088200970526     C* DECODIFICHE
088300970528     C                   MOVEL     *BLANKS       VDDTFP
088400970528     C                   MOVEL     *BLANKS       VDDTFA
088500970528     C                   MOVEL     *BLANKS       VDDTFE
088600970526     C*
088700970526     C                   ENDSR
088800970526     C*------------------------------------------------------------------------*
088900970618     C* IMPCAE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
089000970526     C*------------------------------------------------------------------------*
089100970618     C     IMPCAE        BEGSR
089200970526     C*
089300970618     C                   Z-ADD     DSCVID        CAEDSC
089400970618     C                   Z-ADD     VIDTFE        CAETFE
089500970526     C*
089600970526     C                   ENDSR
089700970526     C*------------------------------------------------------------------------*
089800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
089900970526     C*------------------------------------------------------------------------*
090000970526     C     IMPFUN        BEGSR
090100970526     C*
090200970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
090300970526IF  1C     DEPOP1        IFEQ      'O05'
090400970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
090500970526     C                   MOVEL     'N'           FUDATT
090600970526     C                   MOVEL     FUDRIG        FUD(6)
090700970526E   1C                   ENDIF
090800970526     C*
090900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
091000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
091100970526     C                   EXSR      FUNGES
091200970526     C*
091300970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
091400970526     C*---
091500970526     C* 90 -> PROTEZIONE CHIAVE
091600970526     C* 91 -> PROTEZIONE TOTALE
091700970526     C*---
091800970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
091900970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
092000970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
092100970526     C                   SETOFF                                       91
092200970526E   2C                   ENDIF
092300970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
092400970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
092500970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
092600970526E   2C                   ENDIF
092700970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
092800970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
092900970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
093000970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
093100970526     C                   SETON                                        91
093200970526E   2C                   ENDIF
093300970526X   1C                   ELSE                                                   *I N T E R R O G A
093400970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
093500970526     C                   SETON                                        91
093600970526E   1C                   ENDIF
093700970526     C*
093800970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
093900970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
094000970618     C                   MOVEL     D00EPA        KCEEPA
094100970618     C                   Z-ADD     D00ETP        KCETFP
094200970618     C                   Z-ADD     D00ETA        KCETFA
094300970618     C                   Z-ADD     D00EDD        KCEDDE
094400970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
094500970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
094600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
094700970526     C                   MOVEL     MSG(6)        DSMSMS
094800970526X   2C                   ELSE
094900970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
095000980128IF  3C     DEPOP1        IFEQ      'O05'
095100980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
095200980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
095300980128E   3C                   ENDIF
095400970526E   2C                   ENDIF
095500970526X   1C                   ELSE                                                   *INSERIMENTO
095600970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
095700970526E   1C                   ENDIF
095800970526     C*
095900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
096000970526IF  1C     DEPOP1        IFEQ      'O02'
096100970526     C                   MOVEL     DEC(1)        VIDFUN
096200970526E   1C                   ENDIF
096300970526IF  1C     DEPOP1        IFEQ      'O04'
096400970526     C                   MOVEL     DEC(2)        VIDFUN
096500970526E   1C                   ENDIF
096600970526IF  1C     DEPOP1        IFEQ      'O05'
096700970526     C                   MOVEL     DEC(3)        VIDFUN
096800970526E   1C                   ENDIF
096900970526IF  1C     DEPOP1        IFEQ      'O07'
097000970526     C                   MOVEL     DEC(4)        VIDFUN
097100970526E   1C                   ENDIF
097200970526IF  1C     DEPOP1        IFEQ      'F06'
097300970526     C                   MOVEL     DEC(5)        VIDFUN
097400970526E   1C                   ENDIF
097500970526     C*
097600970526     C                   ENDSR
097700970526     C*------------------------------------------------------------------------*
097800970526     C* *INZSR - OPERAZIONI INIZIALI
097900970526     C*------------------------------------------------------------------------*
098000970526     C     *INZSR        BEGSR
098100970526     C*---
098200970526     C* RICEVIMENTO PARAMETRI
098300970526     C*---
098400970526     C     *ENTRY        PLIST
098500970612     C                   PARM                    KPJBA
098600970612     C                   PARM                    DSSI00
098700000427     C*
098800000427     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
098900970526     C*
099000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
099100970526     C     XMSGPA        PLIST
099200970526     C                   PARM                    DSMSMS
099300970526     C                   PARM                    DSMSPG
099400970526     C                   PARM                    DSMSKE
099500970526     C                   PARM                    DSMSER
099600970526     C*
099700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
099800970526     C                   CALL      'XKEY'
099900970526     C                   PARM                    DSKEY
100000970526     C*---
100100970526     C* VARIABILI NON RIFERITE AL DATA BASE
100200970526     C*---
100300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
100400970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
100500970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
100600970526     C     *LIKE         DEFINE    D00OP0        WOP0
100700970526     C     *LIKE         DEFINE    D00OP1        WOP1
100800970526     C*---
100900970526     C* VARIABILI DI CONTROLLO
101000970526     C*---
101100970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
101200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
101300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
101400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
101500970526     C*
101600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
101700970528     C                   MOVEL     'TISI18R'     DSMSPG
101800970528     C                   MOVEL     'TISI18R'     MSGQUE
101900970526     C                   MOVEL     *BLANKS       MSGKEY
102000970526     C                   MOVEL     *BLANKS       DSMSMS
102100970526     C*---
102200970526     C* VARIABILI RIFERITE AL DATA BASE
102300970526     C*---
102400970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
102500970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
102600970618     C     *LIKE         DEFINE    CAETFP        KCETFP
102700970618     C     *LIKE         DEFINE    CAETFA        KCETFA
102800970618     C     *LIKE         DEFINE    CAEDDE        KCEDDE
102900970526     C*---
103000970526     C* CHIAVI DI LETTURA
103100970526     C*---
103200970528     C* LETTURA AZORG01L - COMPLETA
103300970528     C     KEYORG        KLIST
103400970528     C                   KFLD                    KORFIL                         *FILIALE
103500970618     C* LETTURA AZCAE01L - COMPLETA
103600970618     C     KEYCAE        KLIST
103700970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
103800970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
103900970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
104000970618     C                   KFLD                    KCEDDE                         *DECORRENZA
104100970618     C* LETTURA AZCAE01L - PARZIALE
104200970618     C     KE2CAE        KLIST
104300970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
104400970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
104500970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
104600020506      * Lettura AZCAE01L
104700020506     c     ke1cae        klist
104800020506     c                   kfld                    kceepa
104900970528     C*
105000970528     C* CALCOLA LA DATA CORRENTE
105100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
105200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
105300970528     C                   Z-ADD     WN8           G08DAT
105400970528     C                   Z-ADD     *ZEROS        G08INV
105500970528     C                   MOVEL     '0'           G08ERR
105600970528     C                   CALL      'XSRDA8'
105700970528     C                   PARM                    WLBDA8
105800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
105900970526     C*
106000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
106100970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
106200970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
106300971231     C*
106400971231     C* IT'S A JOKE
106500971231IF  1C     KNMUS         IFEQ      'EDPES'
106600971231     C                   SETON                                            01
106700971231E   1C                   ENDIF
106800050412     c                   movel     knmus         winizioprf        3
106900970526     C*
107000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
107100970526     C                   EXSR      IMPFUN
107200970526     C*
107300970526     C                   ENDSR
107400970526     C*------------------------------------------------------------------------*
107500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
107600970526     C*------------------------------------------------------------------------*
107700970526     C     FUNGES        BEGSR
107800970526     C*
107900970526     C* PULISCO CAMPI DI LAVORO
108000970526     C                   Z-ADD     0             W                 5 0
108100970526     C*
108200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
108300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
108400970526     C     1             DO        24            I
108500970526     C                   MOVEL     FUD(I)        FUDRIG
108600970526     C*
108700970526     C* PULIZIA FUNZIONI PRECEDENTI
108800970526     C                   MOVEL     *BLANKS       FUV(I)
108900970526     C*
109000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
109100970526     C* (SALTA F24)
109200970526     C     FUDATT        IFEQ      'S'
109300970526     C     FUDFUN        ANDNE     '24'
109400970526     C                   ADD       1             W
109500970526     C                   MOVEL     FUDDES        FUV(W)
109600970526     C                   END
109700970526     C                   END                                                    DO WHILE
109800970526     C*
109900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
110000970526     C* ALTRIMENTI DISATTIVO F24
110100970526     C     1             DO        24            I
110200970526     C                   MOVEL     FUD(I)        FUDRIG
110300970526     C*
110400970526     C     FUDFUN        IFEQ      '24'
110500970526     C     W             IFGT      6
110600970526     C                   MOVEL     'S'           FUDATT
110700970526     C                   ELSE
110800970526     C                   MOVEL     *BLANKS       FUDATT
110900970526     C                   END                                                    W GT 8
111000970526     C                   MOVEL     FUDRIG        FUD(I)
111100970526     C                   END
111200970526     C                   END                                                    DO WHILE
111300970526     C*
111400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
111500970526     C     W             DIV       6             I
111600970526     C                   MVR                     Z
111700970526     C*
111800970526     C     Z             IFNE      0
111900970526     C     I             ADD       1             Z
112000970526     C                   ELSE
112100970526     C                   Z-ADD     I             Z                 5 0
112200970526     C                   END
112300970526     C*
112400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
112500970526     C                   ADD       1             FUNGRU
112600970526     C     FUNGRU        IFGT      Z
112700970526     C     FUNGRU        ORLT      1
112800970526     C                   Z-ADD     1             FUNGRU
112900970526     C                   END
113000970526     C*
113100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
113200970526     C     FUNGRU        IFEQ      1
113300970526     C                   MOVEL     FUV(1)        FUN001
113400970526     C                   MOVEL     FUV(2)        FUN002
113500970526     C                   MOVEL     FUV(3)        FUN003
113600970526     C                   MOVEL     FUV(4)        FUN004
113700970526     C                   MOVEL     FUV(5)        FUN005
113800970526     C                   MOVEL     FUV(6)        FUN006
113900970526     C                   END
114000970526     C*
114100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
114200970526     C     FUNGRU        IFEQ      2
114300970526     C                   MOVEL     FUV(7)        FUN001
114400970526     C                   MOVEL     FUV(8)        FUN002
114500970526     C                   MOVEL     FUV(9)        FUN003
114600970526     C                   MOVEL     FUV(10)       FUN004
114700970526     C                   MOVEL     FUV(11)       FUN005
114800970526     C                   MOVEL     FUV(12)       FUN006
114900970526     C                   END
115000970526     C*
115100970526     C* TERZO GRUPPO DI 6 FUNZIONI
115200970526     C     FUNGRU        IFEQ      3
115300970526     C                   MOVEL     FUV(13)       FUN001
115400970526     C                   MOVEL     FUV(14)       FUN002
115500970526     C                   MOVEL     FUV(15)       FUN003
115600970526     C                   MOVEL     FUV(16)       FUN004
115700970526     C                   MOVEL     FUV(17)       FUN005
115800970526     C                   MOVEL     FUV(18)       FUN006
115900970526     C                   END
116000970526     C*
116100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
116200970526     C     FUNGRU        IFEQ      4
116300970526     C                   MOVEL     FUV(19)       FUN001
116400970526     C                   MOVEL     FUV(20)       FUN002
116500970526     C                   MOVEL     FUV(21)       FUN003
116600970526     C                   MOVEL     FUV(22)       FUN004
116700970526     C                   MOVEL     *BLANKS       FUN005
116800970526     C                   MOVEL     *BLANKS       FUN006
116900970526     C                   END
117000970526     C*
117100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
117200970526     C                   MOVEL     *BLANKS       FUN007
117300970526     C                   MOVEL     *BLANKS       FUN008
117400970526     C*
117500970526     C     1             DO        24            I
117600970526     C                   MOVEL     FUD(I)        FUDRIG
117700970526     C     FUDFUN        IFEQ      '24'
117800970526     C     FUDATT        ANDEQ     'S'
117900970526     C                   MOVEL     FUDDES        FUN008
118000970526     C                   END
118100970526     C                   END                                                    DO
118200970526     C*
118300970526     C                   ENDSR
118400970526     C*------------------------------------------------------------------------*
118500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
118600970526     C*------------------------------------------------------------------------*
118700970526     C     FUNCON        BEGSR
118800970526     C*
118900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
119000970526     C                   EXSR      FUNVAR
119100970526     C*
119200970526     C* PULISCO CAMPI DI LAVORO
119300970526     C                   Z-ADD     0             I                 5 0
119400970526     C                   Z-ADD     0             W                 5 0
119500970526     C*
119600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
119700970526     C     FUNKEY        IFNE      *BLANKS
119800970526     C     1             DO        24            I
119900970526     C                   MOVEL     FUD(I)        FUDRIG
120000970526     C*
120100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
120200970526     C     FUNKEY        IFEQ      FUDFUN
120300970526     C     FUDATT        ANDEQ     'S'
120400970526     C                   Z-ADD     I             W
120500970526     C                   END
120600970526     C                   END                                                    DO WHILE
120700970526     C*
120800970526     C* FUNZIONE NON ATTIVA (ERRORE)
120900970526     C     W             IFEQ      0
121000970526     C*
121100970526     C* EMISSIONE MESSAGGIO DI ERRORE
121200970526     C                   MOVEL     MSG(1)        DSMSMS
121300970526     C*
121400970526     C                   END                                                    W=0
121500970526     C                   END                                                    FUNKEY NE *BLANKS
121600970526     C*
121700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
121800970526     C     W             IFNE      0
121900970526     C     FUNKEY        OREQ      *BLANKS
122000970526     C     FUNKEY        IFEQ      *BLANKS
122100970526     C                   EXSR      ENTGES
122200970526     C                   END
122300970526     C     FUNKEY        IFEQ      '03'
122400970526     C                   EXSR      F03GES
122500970526     C                   END
122600970526     C     FUNKEY        IFEQ      '06'
122700970526     C                   EXSR      F06GES
122800970526     C                   END
122900970526     C     FUNKEY        IFEQ      '12'
123000970526     C                   EXSR      F12GES
123100970526     C                   END
123200970526     C     FUNKEY        IFEQ      '24'
123300970526     C                   EXSR      F24GES
123400970526     C                   END
123500970526     C                   END
123600970526     C*
123700970526     C                   ENDSR
123800970526     C*------------------------------------------------------------------------*
123900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
124000970526     C*------------------------------------------------------------------------*
124100970526     C     FUNVAR        BEGSR
124200970526     C*
124300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
124400970526     C*
124500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
124600970526     C     KEY           IFEQ      ENTER
124700970526     C                   MOVEL     *BLANKS       FUNKEY
124800970526     C                   END
124900970526     C     KEY           IFEQ      F1
125000970526     C                   MOVEL     '01'          FUNKEY
125100970526     C                   END
125200970526     C     KEY           IFEQ      F2
125300970526     C                   MOVEL     '02'          FUNKEY
125400970526     C                   END
125500970526     C     KEY           IFEQ      F3
125600970526     C                   MOVEL     '03'          FUNKEY
125700970526     C                   END
125800970526     C     KEY           IFEQ      F4
125900970526     C                   MOVEL     '04'          FUNKEY
126000970526     C                   END
126100970526     C     KEY           IFEQ      F5
126200970526     C                   MOVEL     '05'          FUNKEY
126300970526     C                   END
126400970526     C     KEY           IFEQ      F6
126500970526     C                   MOVEL     '06'          FUNKEY
126600970526     C                   END
126700970526     C     KEY           IFEQ      F7
126800970526     C                   MOVEL     '07'          FUNKEY
126900970526     C                   END
127000970526     C     KEY           IFEQ      F8
127100970526     C                   MOVEL     '08'          FUNKEY
127200970526     C                   END
127300970526     C     KEY           IFEQ      F9
127400970526     C                   MOVEL     '09'          FUNKEY
127500970526     C                   END
127600970526     C     KEY           IFEQ      F10
127700970526     C                   MOVEL     '10'          FUNKEY
127800970526     C                   END
127900970526     C     KEY           IFEQ      F11
128000970526     C                   MOVEL     '11'          FUNKEY
128100970526     C                   END
128200970526     C     KEY           IFEQ      F12
128300970526     C                   MOVEL     '12'          FUNKEY
128400970526     C                   END
128500970526     C     KEY           IFEQ      F13
128600970526     C                   MOVEL     '13'          FUNKEY
128700970526     C                   END
128800970526     C     KEY           IFEQ      F14
128900970526     C                   MOVEL     '14'          FUNKEY
129000970526     C                   END
129100970526     C     KEY           IFEQ      F15
129200970526     C                   MOVEL     '15'          FUNKEY
129300970526     C                   END
129400970526     C     KEY           IFEQ      F16
129500970526     C                   MOVEL     '16'          FUNKEY
129600970526     C                   END
129700970526     C     KEY           IFEQ      F17
129800970526     C                   MOVEL     '17'          FUNKEY
129900970526     C                   END
130000970526     C     KEY           IFEQ      F18
130100970526     C                   MOVEL     '18'          FUNKEY
130200970526     C                   END
130300970526     C     KEY           IFEQ      F19
130400970526     C                   MOVEL     '19'          FUNKEY
130500970526     C                   END
130600970526     C     KEY           IFEQ      F20
130700970526     C                   MOVEL     '20'          FUNKEY
130800970526     C                   END
130900970526     C     KEY           IFEQ      F21
131000970526     C                   MOVEL     '21'          FUNKEY
131100970526     C                   END
131200970526     C     KEY           IFEQ      F22
131300970526     C                   MOVEL     '22'          FUNKEY
131400970526     C                   END
131500970526     C     KEY           IFEQ      F23
131600970526     C                   MOVEL     '23'          FUNKEY
131700970526     C                   END
131800970526     C     KEY           IFEQ      F24
131900970526     C                   MOVEL     '24'          FUNKEY
132000970526     C                   END
132100970526     C     KEY           IFEQ      ROLLUP
132200970526     C                   MOVEL     'RU'          FUNKEY
132300970526     C                   END
132400970526     C     KEY           IFEQ      HELP
132500970526     C                   MOVEL     'HP'          FUNKEY
132600970526     C                   END
132700970526     C                   ENDSR
132800970526     C*------------------------------------------------------------------------*
132900970526     C* NOTE COSTRUZIONE SCHIERE
133000970526     C*------------------------------------------------------------------------*
133100970526     C*
133200970526     C* -MSG-MESSAGGI
133300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
133400970526     C* (NON TOCCARE MSG,1 E MSG,2)
133500970526     C*
133600970526     C* -CMD-COMANDI
133700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
133800970526     C*
133900970526     C* -OPD-OPZIONI
134000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
134100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
134200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
134300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
134400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
134500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
134600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
134700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
134800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
134900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
135000970526     C*
135100970526     C* -FUD-FUNZIONI
135200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
135300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
135400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
135500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
135600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
135700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
135800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
135900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
136000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
136100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
136200970526     O*------------------------------------------------------------------------*
136300970526** DEC - DECODIFICHE
136400970526    MODIFICA                                                      1
136500970526  ANNULLAMENTO                                                    2
136600970526VISUALIZZAZIONE                                                   3
136700970526   RIPRISTINO                                                     4
136800970526  INSERIMENTO                                                     5
136900970526** MSG - MESSAGGI
137000970528TISI18R- Tasto funzionale non gestito                                          1
137100970528TISI18R- Inserimento effettuato                                                2
137200970528TISI18R- Modifica effettuata                                                   3
137300970528TISI18R- Annullamento effettuato                                               4
137400970528TISI18R- Ripristino effettuato                                                 5
137500970528TISI18R- Richiesta funzione per record inesistente                             6
137600970528TISI18R- Record momentaneamente allocato: riprovare più tardi                  7
137700970528TISI18R- Richiesta funzione per record esistente                               8
137800970528TISI18R- 9
137900090115TISI18R- Il record immesso si sovrappone ad uno già in decorrenza.ENTER forza  10
138000030310TISI18R- Immettere P=Partenza o A=Arrivo o 6=£6 o C=Coop                       11
138100970528TISI18R- Terminal / Linea partenza errato                                      12
138200970528TISI18R- Terminal / Linea arrivo errato                                        13
138300970528TISI18R- Data errata                                                           14
138400970528TISI18R- La data scadenza deve essere maggiore della decorrenza                15
138500971203TISI18R- Il record non può scadere in un giorno già trascorso                  16
138600971203TISI18R- Il record immesso si sovrappone ad uno già in decorrenza              17
138700971203TISI18R- Terminal / linea errati                                               18
138800060706TISI18R- Impossibile inserire terminal Arrivo per tipo "P" o "O"               19
138900970618TISI18R- L'eccezione non può essere uguale al terminal arrivo standard         20
139000030310TISI18R- Per tipo terminal "6" o "C" non inserire il terminal partenza         21
139100020507TISI18R- Scadenza incongruente con scadenza record tipo "6"                    22
139200020507TISI18R- Esistono dei record di tipo "6". Impossibile la modifica              23
139300080806TISI18R- Il p.o. gestione ha il terminal di Arrivo   diverso dal p.o. gestito  24
139400080806TISI18R- Data decorrenza/scadenza incongruenti con le date del terminal Arr.   25
139500970526** FUD - FUNZIONI
139600970526                   -        (S/N)=N                        funz.01¬
139700970526                   -        (S/N)=N                        funz.02¬
139800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
139900970526                   -        (S/N)=N                        funz.04¬
140000970526                   -        (S/N)=N                        funz.05¬
140100970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
140200970527                   -        (S/N)=N                        funz.07¬
140300970527                   -        (S/N)=N                        funz.08¬
140400970527                   -        (S/N)=N                        funz.09¬
140500970527                   -        (S/N)=N                        funz.10¬
140600970526                   -        (S/N)=N                        funz.11¬
140700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
140800970526                   -        (S/N)=N                        funz.13¬
140900970526                   -        (S/N)=N                        funz.14¬
141000970526                   -        (S/N)=N                        funz.15¬
141100970526                   -        (S/N)=N                        funz.16¬
141200970526                   -        (S/N)=N                        funz.17¬
141300970526                   -        (S/N)=N                        funz.18¬
141400970526                   -        (S/N)=N                        funz.19¬
141500970526                   -        (S/N)=N                        funz.20¬
141600970526                   -        (S/N)=N                        funz.21¬
141700970526                   -        (S/N)=N                        funz.22¬
141800970526                   -        (S/N)=N                        funz.23¬
141900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
