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
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------
002000970526     D* DECODIFICHE
002100970526     D*------------
002200131015     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* MESSAGGI
002500970526     D*------------
002600170403     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
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
006100970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200970528     D*------------------
006300970528     D WLBDA8          DS                  INZ
006400970528     D  G08DAT                 1      8  0
006500970528     D  G08INV                 9     16  0
006600970528     D  G08ERR                17     17
006700970528     D  G08TGI                18     22  0
006800970526     D*------------
006900970526     D* DS DI PROCEDURA
007000970526     D*------------
007100970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
007200170308     d
007600970526     D*------------
007700970526     D* ARCHITETTURA
007800970526     D*------------
007900970526     D KPJBA         E DS
008000020506
008100020506     d wcaetfa         s              3
008200020506     d wcaetfe         s              3
008600020510     d sav_viddsc      s                   like(viddsc)
008700020510     d sav_vidtfe      s                   like(vidtfe)
008800020516     d sav_tfe1        s                   like(caetfe)
008900020516     d sav_tfe2        s                   like(caetfe)
009000020516     d ok_date         s              1    inz('0')
009100040624     d WFORZ           s              1
009200090115     d forzater        s              1
009300120214     d forzaSCA        s              1
009400120216     d Dataiso         s               d   datfmt(*iso)
009401170403     d wdata           s              8  0
009500120216     d datam1          s              8  0
009501170403     d dw              s              1s 0
009600970526     C*------------------------------------------------------------------------*
009700970526     C* MAIN LINES
009800970526     C*------------------------------------------------------------------------*
009900970526     C*
009901170403         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009902170403
010000970526     C* NON CI SONO ERRORI IN ENTRATA
010100970526IF  1C     ERRL00        IFEQ      '0'
010200970526     C*
010300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010700970526     C*
010800970526     C* EMETTE FORMATO VIDEO
010900970526     C                   EXSR      BUTFMR
011000970526     C*
011100970526     C* GESTIONE TASTI FUNZIONALI
011200970526     C                   EXSR      FUNCON
011300970526E   2C                   ENDDO
011400970526     C*
011500970526     C* CI SONO ERRORI IN ENTRATA
011600970527X   1C                   ELSE
011700970526     C                   MOVEL     '1'           D00ERR
011800970526     C                   MOVEL     DSMSMS        D00MSG
011900970526E   1C                   ENDIF
012000970526     C* FINE PGM
012100970526     C                   SETON                                        LR
012200970526     C*------------------------------------------------------------------------*
012300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012400970526     C*------------------------------------------------------------------------*
012500970526     C     BUTFMR        BEGSR
012600970526     C*
012700970526     C* EMETTE LA TESTATA
012800970528     C                   WRITE     SI18D01
012900970526     C*
013000970526     C* SPEDISCE EVENTUALE MESSAGGIO
013100970526     C                   EXSR      SNDMSG
013200970526     C*
013300970526     C* EMETTE LE FUNZIONI
013400970528     C                   WRITE     SI18D02
013500970526     C*
013600970526     C* EMETTE IL VIDEO
013700970528     C                   EXFMT     SI18D03
013800970526     C*
013900970526     C* REIMPOSTA FLAG DI ERRORE
014000970526     C                   MOVEL     '0'           ERRL00
014100970526     C*
014200970526     C* REIMPOSTA INDICATORI DI ERRORE
014300170308     C                   SETOFF                                       575859
014400970526     C                   SETOFF                                       606162
014500970526     C                   SETOFF                                       636465
014600970526     C                   SETOFF                                       666768
014700970526     C                   SETOFF                                       697071
014800970526     C                   SETOFF                                       727374
014900970526     C                   SETOFF                                       757677
015000970526     C                   SETOFF                                       787980
015100970526     C                   SETOFF                                       818283
015200970526     C                   SETOFF                                       848586
015300970526     C                   SETOFF                                       878889
015400970526     C*
015500970526     C                   ENDSR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015800970526     C*------------------------------------------------------------------------*
015900970526     C     SNDMSG        BEGSR
016000970526     C*
016100970526     C* PULISCE IL SFILE MESSAGGI
016200970526     C                   MOVEL     '1'           *IN40
016300970528     C                   WRITE     SI18DMC
016400970526     C                   MOVEL     '0'           *IN40
016500970526     C*
016600970526     C* VISUALIZZA IL MESSAGGIO
016700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016800970526     C                   CALL      'XMSG'        XMSGPA
016900970526IF  1C     DSMSER        IFEQ      *ZEROS
017000970526     C                   MOVEL     DSMSKE        MSGKEY
017100970526E   1C                   ENDIF
017200970526     C                   Z-ADD     1             MSGULT
017300970528     C                   WRITE     SI18DMS
017400970528     C                   WRITE     SI18DMC
017500970526     C*
017600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017700970526     C                   MOVEL     *BLANKS       DSMSMS
017800970526     C*
017900970526     C                   ENDSR
018000970526     C*------------------------------------------------------------------------*
018100970526     C* ENTGES - GESTIONE ENTER -> INVIO
018200970526     C*------------------------------------------------------------------------*
018300970526     C     ENTGES        BEGSR
018400970526     C*
018500970526     C* CONTROLLA I CAMPI IMMESSI
018600970526     C                   EXSR      DECODI
018700970526     C*
018800970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
018900970526IF  1C     DEPOP1        IFEQ      'O05'
019000970526     C                   MOVEL     '1'           FINL00
019100970526E   1C                   ENDIF
019200970526     C*
019300970526     C                   ENDSR
019400970526     C*------------------------------------------------------------------------*
019500970526     C* F03GES - GESTIONE F03 -> FINE
019600970526     C*------------------------------------------------------------------------*
019700970526     C     F03GES        BEGSR
019800970526     C*
019900970526     C* IMPOSTA "PREMUTO F03"
020000131015     c                   unlock    azcae01l
020100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020200970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600000515     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F06GES        BEGSR
020900970526     C*
021000970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
021100970526IF  1C     DEPOP1        IFEQ      'F06'
021200970526     C                   EXSR      INSERI
021300970526E   1C                   ENDIF
021400970526IF  1C     DEPOP1        IFEQ      'O02'
021500970526     C                   EXSR      MODIFI
021600970526E   1C                   ENDIF
021700970526IF  1C     DEPOP1        IFEQ      'O04'
021800970526     C                   EXSR      ANNULL
021900970526E   1C                   ENDIF
022000970526IF  1C     DEPOP1        IFEQ      'O07'
022100970526     C                   EXSR      RIPRIS
022200970526E   1C                   ENDIF
022300131015IF  1C     DEPOP1        IFEQ      'O+1'
022400131015     c                   unlock    azcae01l
022500131015
022600131015     C                   call      'TISI18R2'
022700131015     c                   parm                    kpjba
022800131015     c                   parm                    dssi00
022900131015     C*
023000131015     c                   if        d00err=' '
023100131015     C                   MOVEL     MSG(3)        D00MSG
023200131015     c                   else
023300131015     C                   MOVEL     MSG(28)       D00MSG
023400131015     c                   endif
023500131015     C                   MOVEL     '1'           FINL00                         *FINE PGM
023600131015E   1C                   ENDIF
023700970526     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* F12GES - GESTIONE F12 -> RITORNO
024100970526     C*------------------------------------------------------------------------*
024200970526     C     F12GES        BEGSR
024300970526     C*
024400970526     C* IMPOSTA "PREMUTO F12"
024500131015     c                   unlock    azcae01l
024600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024700970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025200970526     C*------------------------------------------------------------------------*
025300970526     C     F24GES        BEGSR
025400970526     C*
025500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025600970526     C*
025700970526     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
026000970526     C*------------------------------------------------------------------------*
026100970526     C     INSERI        BEGSR
026200970526     C*
026300970526     C* CONTROLLA I CAMPI IMMESSI
026400000515     C                   EXSR      DECODI
026500170308     c
026600170308IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026700970526     C*
026800120216     c* Se inserimento di terminal PAR/ARR e profilo EDP
026900120216     c* si fa scadere eventuale terminal decorrente
027000170308    2c                   if        winizioprf='EDP' and
027100120216     c                             (videpa='P' or videpa='A')
027200170308     c                             and vidgio=' '
027300120216     c                   exsr      scaden
027400170308    2c                   endif
027500170308     c
027600170308    2c                   if        vidgio<>' '
027700170308     c                   exsr      ins_ecce
027800170308   x2c                   else
027900170308
028000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028100970618     C                   MOVEL     VIDEPA        CAEEPA                         *CHIAVE
028200970618     C                   MOVEL     TFPVID        CAETFP
028300970618     C                   MOVEL     TFAVID        CAETFA
028400970618     C                   Z-ADD     DDEVID        CAEDDE
028500970618     C                   EXSR      IMPCAE                                       *ALTRI CAMPI
028600970618     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
028700970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
028800970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
028900970618     C                   WRITE     AZCAE000
029000000427     C*
029100000427     C                   EXSR      LETCAE
029200970526     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
029300120831     C*
029400120903     C* se il tipo terminal inserito è £6
029500120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
029600170308    3C                   IF        CAEEPA = '6' and
029700120911     C                             VIDCRC = 'SI'
029800120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
029900120903     C* lo inserisco
030000120903     C                   WRITE     AZCAE000
030100170308    3C                   ENDIF
030200170308    2C                   ENDIF
030300970602     C*
030400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
030500970602     C                   EXSR      AZZVID
030600970526E   1C                   ENDIF
030700970526     C*
030800970526     C                   ENDSR
030900170308     C*------------------------------------------------------------------------*
031000170308     C* inserimento eccezione di un giorno della settimana per tutto l'anno
031100170308     C*------------------------------------------------------------------------*
031200170308     C     ins_ecce      BEGSR
031300170308     C*
031301170403     c                   eval      wdata=ddevid
031302170403     c                   dow       wdata<=dscvid
031303170403       //       Determino il giorno della settimana della data
031304170403               dataiso=%date(wdata:*iso);
031305170403               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
031306170403     c                   if        dw=%int(vidgio)
031307170403     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
031308170403     C                   MOVEL     VIDEPA        CAEEPA                         *CHIAVE
031309170403     C                   MOVEL     TFPVID        CAETFP
031310170403     C                   MOVEL     TFAVID        CAETFA
031311170403     C                   Z-ADD     wdata         CAEDDE
031312170403     C                   Z-ADD     wdata         CAEDSC
031313170403     C                   Z-ADD     VIDTFE        CAETFE
031314170403     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
031315170403     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
031316170403     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
031317170403     C                   WRITE(e)  AZCAE000
031318170403     c                   endif
031319170403       //   Incremento la data di un giorno
031320170403             dataiso=dataiso+%days(1);
031321170403             wdata=%dec(dataiso);
031322170403     c                   enddo
032400170308     C                   ENDSR
038200120831     C*------------------------------------------------------------------------*
038300120831     C* SCADEN - OPERAZIONI per far scadere il terminal già decorrente
038400120831     C*------------------------------------------------------------------------*
038500120831     C     SCADEN        BEGSR
038600120216     C                   MOVEL     VIDEPA        KCEEPA
038700120216     C                   MOVEL     TFPVID        KCETFP
038800120216     C                   MOVEL     TFAVID        KCETFA
038900120216     C     KE2CAE        SETLL     AZCAE01L
039000120216     C     KE2CAE        READE     AZCAE01L
039100120216     C                   DOW       not %eof(azcae01l)
039200120216     C     CAEATB        IFEQ      *BLANKS
039300120216     C     DDEVID        andgt     CAEDDE                                       *DECORRENZA
039400120216     C     DDEVID        ANDLE     CAEDSC
039500120216     c* data decorrenza del nuovo record - un giorno = data scadenza del record
039600120216     c* già esistente
039700120216     c                   move      ddevid        dataiso
039800120216     c                   subdur    1:*d          dataiso
039900120216     c                   move      dataiso       caedsc
040000120216     c                   update    azcae000
040100120216     c                   leave
040200120216     c                   endif
040300120216     C     KE2CAE        READE     AZCAE01L
040400120216     c                   enddo
040500120216     c                   endsr
040600970526     C*------------------------------------------------------------------------*
040700970526     C* MODIFI - OPERAZIONI DI MODIFICA
040800970526     C*------------------------------------------------------------------------*
040900970526     C     MODIFI        BEGSR
041000970526     C*
041100970526     C* CONTROLLA I CAMPI IMMESSI
041200970526     C                   EXSR      DECODI
041300970526     C*
041400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
041500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041600970618     C                   EXSR      LETCAE
041700030310     C*
041800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
041900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042000970618     C                   EXSR      IMPCAE                                       *IMPOSTA I CAMPI
042100970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
042200970618     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
042300970618     C                   UPDATE    AZCAE000
042400000427     C*
042500970526     C                   MOVEL     MSG(3)        D00MSG
042600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
042700120903     C*
042800120903     C* se il tipo terminal inserito è £6
042900120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
043000120903     C                   IF        CAEEPA = '6' and
043100120911     C                             VIDCRC = 'SI'
043200120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
043300120903     C                   EXSR      LETCAE2
043400120903     C* lo modifico
043500120903     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
043600120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
043700120903     C                   EXSR      IMPCAE                                       *IMPOSTA I CAMPI
043800120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
043900120903     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
044000120903     C                   UPDATE    AZCAE000
044100120903     C                   ENDIF
044200120903     C                   ENDIF
044300120903     C*
044400970526E   2C                   ENDIF
044500970526E   1C                   ENDIF
044600970526     C*
044700970526     C                   ENDSR
044800970526     C*------------------------------------------------------------------------*
044900970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
045000970526     C*------------------------------------------------------------------------*
045100970526     C     ANNULL        BEGSR
045200970526     C*
045300040624     c                   exsr      sr_atten
045400040624     c*
045500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
045600000602     C                   MOVEL     '0'           ANNDLT            1
045700970618     C                   EXSR      LETCAE
045800970527     C*
045900970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
046000970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
046100970618IF  2C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
046200970618IF  3C     CAEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
046300970618     C                   MOVEL     'A'           CAEATB                         *FLAG ANNULLAMENTO
046400970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
046500970618     C                   UPDATE    AZCAE000                                     *ANNULLA
046600970526X   3C                   ELSE                                                   *MAI TRASMESSO
046700970618     C                   DELETE    AZCAE000                                     *CANCELLA
046800000602     C                   MOVEL     '1'           ANNDLT
046900970526E   3C                   ENDIF
047000970526E   2C                   ENDIF
047100970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
047200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
047300120903     C*
047400120903IF  2C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
047500120903     C* se il tipo terminal inserito è £6
047600120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
047700120903IF   C                   IF        CAEEPA = '6' and
047800120911     C                             VIDCRC = 'SI'
047900120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
048000120903     C                   EXSR      LETCAE2
048100120903IF  3C     CAEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
048200120903     C* lo annullo logicamente
048300120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
048400120903     C                   MOVEL     'A'           CAEATB                         *FLAG ANNULLAMENTO
048500120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
048600120903     C                   UPDATE    AZCAE000
048700120903     C                   ENDIF
048800120903     C                   ELSE
048900120903     C* lo cancello
049000120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
049100120903     C                   DELETE    AZCAE000
049200120903     C                   ENDIF
049300120903     C                   ENDIF
049400120903     C                   ENDIF
049500120903     C                   ENDIF
049600120903     C*
049700970526E   1C                   ENDIF
049800970526     C*
049900970526     C                   ENDSR
050000970526     C*------------------------------------------------------------------------*
050100970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
050200970526     C*------------------------------------------------------------------------*
050300970526     C     RIPRIS        BEGSR
050400970526     C*
050500970526     C* CONTROLLA I CAMPI IMMESSI
050600970526     C                   EXSR      DECODI
050700970526     C*
050800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
050900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051000970618     C                   EXSR      LETCAE
051100970526     C*
051200970526     C* IMPOSTA IL RECORD DA SCRIVERE
051300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051400970618     C                   MOVEL     *BLANKS       CAEATB                         *FLAG ANNULLAMENTO
051500970618     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
051600970618     C                   UPDATE    AZCAE000                                     *ANNULLA
051700970526E   2C                   ENDIF
051800970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
051900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
052000120903     C*
052100120903     C* se il tipo terminal inserito è £6
052200120903     C* e si vuole gestire automaticamente il corrispettivo rcd con tipo terminal C
052300120903     C                   IF        CAEEPA = '6' and
052400120911     C                             VIDCRC = 'SI'
052500120903     C                   MOVEL     'C'           CAEEPA                         *TP TERMINAL
052600120903     C                   EXSR      LETCAE2
052700120903     C* lo ripristino
052800120903IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052900120903     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
053000120903     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
053100120903     C                   UPDATE    AZCAE000
053200120903     C                   ENDIF
053300120903     C                   ENDIF
053400120903     C*
053500970526E   1C                   ENDIF
053600970526     C*
053700970526     C                   ENDSR
053800970526     C*------------------------------------------------------------------------*
053900970618     C* LETCAE - LEGGE IL RECORD PERL'AGGIORNAMENTO
054000970526     C*------------------------------------------------------------------------*
054100970618     C     LETCAE        BEGSR
054200970526     C*
054300000427     C     DEPOP1        IFNE      'F06'
054400980127     C                   MOVEL     D00EPA        KCEEPA
054500980127     C                   MOVEL     D00ETP        KCETFP
054600980127     C                   MOVEL     D00ETA        KCETFA
054700980127     C                   Z-ADD     D00EDD        KCEDDE
054800970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
054900970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
055000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
055100970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
055200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
055300970526E   1C                   ENDIF
055400970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
055500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
055600970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
055700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
055800970526E   1C                   ENDIF
055900000427     C                   ENDIF
056000970526     C*
056100970526     C                   ENDSR
056200120903     C*------------------------------------------------------------------------*
056300120903     C* LETCAE2 - LEGGE IL RECORD tipo terminal C corrispondente al £6 PER L'AGGIORNAMENTO
056400120903     C*------------------------------------------------------------------------*
056500120903     C     LETCAE2       BEGSR
056600120903     C*
056700120903     C* LETTURA AZCAE01L - COMPLETA
056800120903     C     K04CAE01      KLIST
056900120903     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
057000120903     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
057100120903     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
057200120903     C                   KFLD                    CAEDDE                         *DECORRENZA
057300120903     C*
057400120903     C     K04CAE01      CHAIN     AZCAE01L                           9998
057500120903IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
057600120903     C                   MOVEL     '1'           ERRL00                         *ERRORE
057700120903     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
057800120903     C                   MOVEL     '1'           FINL00                         *FINE PGM
057900120903E   1C                   ENDIF
058000120903IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
058100120903     C                   MOVEL     '1'           ERRL00                         *ERRORE
058200120903     C                   MOVEL     MSG(27)       D00MSG                         *MESSAGGIO
058300120903     C                   MOVEL     '1'           FINL00                         *FINE PGM
058400120903E   1C                   ENDIF
058500120903     C*
058600120903     C                   ENDSR
058700970526     C*------------------------------------------------------------------------*
058800970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
058900970526     C*------------------------------------------------------------------------*
059000970526     C     DECODI        BEGSR
059100970528     C*--------------------
059200970528     C* PARTENZA/ARRIVO
059300970528     C*--------------------
059400970528IF  1C     ERRL00        IFEQ      '0'
059500970528IF  2C     VIDEPA        IFEQ      'P'
059600970528     C     VIDEPA        OREQ      'A'
059700020418     C     VIDEPA        OREQ      '6'
059800030310     C     VIDEPA        OREQ      'C'
059900060706     C     VIDEPA        OREQ      'O'
060000140117     C     VIDEPA        OREQ      'J'
060100970528X   2C                   ELSE
060200970528     C                   MOVEL     '1'           ERRL00
060300970528     C                   SETON                                        60
060400970528     C                   MOVEL     MSG(11)       DSMSMS
060500970528E   2C                   ENDIF
060600970528E   1C                   ENDIF
060700970528     C*--------------------
060800970528     C* TERMINAL / LINEA DI PARTENZA
060900970528     C*--------------------
061000970618     C     *LIKE         DEFINE    VIDTFP        TFPVID
061100020418IF  1c                   if        errl00 = *off
061200020418     c                   clear                   vddtfp
061300030310     c                   if        videpa = '6' and vidtfp > *zeros or
061400140117     c                             videpa = 'C' and vidtfp > *zeros or
061500140117     c                             videpa = 'J' and vidtfp > *zeros
061600030310     c                   eval      errl00 = *on
061700020418     c                   eval      *in61 = *on
061800020418     c                   eval      dsmsms = msg(21)
061900020418     c                   endif
062000020418     c                   endif
062100020418
062200970528IF  1C     ERRL00        IFEQ      '0'
062300970618     C                   MOVEL     *BLANKS       VDDTFP
062400140117     c                   if        (videpa ='A' or videpa='6' or videpa='C' or
062500140117     c                              videpa ='J' ) and
062600020418     c                             (vidtfp = *blanks or vidtfp = *zeros)
062700970618     C                   MOVEL     *ZEROS        TFPVID
062800970618X   2C                   ELSE
062900970618     C                   MOVEL     VIDTFP        WLIN
063000970618     C                   MOVEL     *BLANKS       WDLIN
063100970618     C                   EXSR      CHKLIN
063200970618     C                   MOVEL     WLIN          VIDTFP
063300970618     C                   MOVEL     WDLIN         VDDTFP
063400970618     C                   MOVEL     VIDTFP        TFPVID
063500970618IF  3C     ERRL00        IFEQ      '1'
063600970528     C                   SETON                                        61
063700970618     C                   MOVEL     MSG(12)       DSMSMS
063800970618E   3C                   ENDIF
063900970618E   2C                   ENDIF
064000970528E   1C                   ENDIF
064100970528     C*--------------------
064200970528     C* TERMINAL / LINEA DI ARRIVO
064300970528     C*--------------------
064400970618     C     *LIKE         DEFINE    VIDTFA        TFAVID
064500970528IF  1C     ERRL00        IFEQ      '0'
064600970618     C                   MOVEL     *BLANKS       VDDTFA
064700140117     c                   if        (videpa ='P' or videpa='O') and
064800060706     c                             (vidtfa = *blanks or vidtfa = *zeros)
064900970618     C                   MOVEL     *ZEROS        TFAVID
065000970618X   2C                   ELSE
065100970528     C                   MOVEL     VIDTFA        WLIN
065200970528     C                   MOVEL     *BLANKS       WDLIN
065300970528     C                   EXSR      CHKLIN
065400970528     C                   MOVEL     WLIN          VIDTFA
065500970528     C                   MOVEL     WDLIN         VDDTFA
065600970618     C                   MOVEL     VIDTFA        TFAVID
065700970618IF  3C     ERRL00        IFEQ      '1'
065800970528     C                   SETON                                        62
065900970528     C                   MOVEL     MSG(13)       DSMSMS
066000970618E   3C                   ENDIF
066100970618E   2C                   ENDIF
066200970528E   1C                   ENDIF
066300970618     C*
066400060706     C* NO ECCEZIONE IN PARTENZA o per ottico
066500970618IF  1C     ERRL00        IFEQ      '0'
066600140117     c                   if        videpa='P' or videpa='O'
066700970618IF  3C     VIDTFP        IFGT      *ZEROS
066800970618     C     VIDTFA        ANDGT     *ZEROS
066900970618     C     VIDTFE        ANDGT     *ZEROS
067000060706     C                   SETON                                        62
067100970618     C                   MOVEL     MSG(19)       DSMSMS
067200970618     C                   MOVEL     '1'           ERRL00
067300970618E   3C                   ENDIF
067400970618E   2C                   ENDIF
067500970618E   1C                   ENDIF
067600970528     C*--------------------
067700970528     C* DATA DECORRENZA
067800970528     C*--------------------
067900970528     C     *LIKE         DEFINE    VIDDDE        DDEVID
068000970528IF  1C     ERRL00        IFEQ      '0'
068100970528     C                   Z-ADD     VIDDDE        WDA8
068200970528     C                   Z-ADD     *ZEROS        DA8W
068300970528     C                   EXSR      CHKDA8
068400970528     C                   Z-ADD     WDA8          VIDDDE
068500090115     c* pulisco flag di forzatura sulla date se cambiano
068600090115     c                   if        da8w<>ddevid
068700090115     c                   clear                   forzater
068800120216     c                   setoff                                       42
068900090115     c                   endif
069000970528     C                   Z-ADD     DA8W          DDEVID
069100970528IF  2C     ERRL00        IFEQ      '1'
069200970528     C                   SETON                                        63
069300970528     C                   MOVEL     MSG(14)       DSMSMS
069400970528E   2C                   ENDIF
069500970528E   1C                   ENDIF
069600970528     C* INSERIMENTO -> RECORD INESISTENTE
069700970528IF  1C     ERRL00        IFEQ      '0'
069800970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
069900170308     c     vidgio        andeq     ' '
070000970618     C                   MOVEL     VIDEPA        KCEEPA
070100970618     C                   MOVEL     TFPVID        KCETFP
070200970618     C                   MOVEL     TFAVID        KCETFA
070300970618     C                   Z-ADD     DDEVID        KCEDDE
070400970618     C     KEYCAE        CHAIN(N)  AZCAE01L                           99
070500970528IF  3C     *IN99         IFEQ      *OFF
070600970528     C                   MOVEL     '1'           ERRL00
070700970528     C                   MOVEL     MSG(8)        DSMSMS
070800970528     C                   SETON                                        606162
070900970528     C                   SETON                                        63
071000970528E   3C                   ENDIF
071100970528E   2C                   ENDIF
071200970528E   1C                   ENDIF
071300970528     C*--------------------
071400970528     C* DATA SCADENZA
071500970528     C*--------------------
071600970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
071700970528IF  1C     ERRL00        IFEQ      '0'
071800970528     C                   Z-ADD     VIDDSC        WDA8
071900970528     C                   Z-ADD     *ZEROS        DA8W
072000970528     C                   EXSR      CHKDA8
072100970528     C                   Z-ADD     WDA8          VIDDSC
072200090115     c                   if        da8w<>dscvid
072300090115     c                   clear                   forzater
072400120216     c                   setoff                                       42
072500120214     c                   clear                   forzaSCA
072600090115     c                   endif
072700970528     C                   Z-ADD     DA8W          DSCVID
072800970528IF  2C     ERRL00        IFEQ      '1'
072900970528     C                   SETON                                        64
073000970528     C                   MOVEL     MSG(14)       DSMSMS
073100970528E   2C                   ENDIF
073200970528E   1C                   ENDIF
073300970528     C* SCADENZA > DECORRENZA
073400970528IF  1C     ERRL00        IFEQ      '0'
073500970528IF  2C     DSCVID        IFLT      DDEVID
073600970619     C                   SETON                                        64
073700970528     C                   MOVEL     MSG(15)       DSMSMS
073800970528     C                   MOVEL     '1'           ERRL00
073900970528E   2C                   ENDIF
074000970528E   1C                   ENDIF
074100970528     C* SCADENZA >= DATA CORRENTE
074200050412     c* Per EDP e per terminal partenza e £6, non faccio il controllo
074300970528IF  1C     ERRL00        IFEQ      '0'
074400170308     c     vidgio        andeq     ' '
074500050412     c                   if        (winizioprf='EDP' and videpa='P') or
074600050412     c                             (winizioprf='EDP' and videpa='6')
074700050412     c                   else
074800120214     c
074900120214     c* Per EDP forzatura
075000970528IF  2C     DEPOP1        IFEQ      'O02'
075100970528     C     DEPOP1        OREQ      'O07'
075200970528     C     DEPOP1        OREQ      'F06'
075300970528IF  3C     DSCVID        IFLT      DATCOR
075400120214     c     forzasca      andeq     ' '
075500970619     C                   SETON                                        64
075600970528     C                   MOVEL     MSG(16)       DSMSMS
075700120214     c                   if         winizioprf='EDP'
075800120214     c                   eval      forzaSCA='S'
075900120214     C                   MOVEL     MSG(26)       DSMSMS
076000120214     c                   endif
076100120214     c
076200970528     C                   MOVEL     '1'           ERRL00
076300970528E   3C                   ENDIF
076400050412E   3C                   ENDIF
076500970528E   2C                   ENDIF
076600970528E   1C                   ENDIF
076700970528     C*---
076800970528     C* DECORRENZA + SCADENZA
076900970528     C*---
077000970528     C* ACCAVALLAMENTO DATI
077100970528IF  1C     ERRL00        IFEQ      '0'
077200090115     c     forzater      andeq     ' '
077300170308     c     vidgio        andeq     ' '
077400970618     C                   MOVEL     VIDEPA        KCEEPA
077500970618     C                   MOVEL     TFPVID        KCETFP
077600970618     C                   MOVEL     TFAVID        KCETFA
077700970618     C     KE2CAE        SETLL     AZCAE01L
077800970618     C     KE2CAE        READE(N)  AZCAE01L                               99
077900970528DO  2C     *IN99         DOWEQ     *OFF
078000970528IF  3C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
078100970618     C     DDEVID        ANDNE     CAEDDE                                       *RECORD IN LINEA
078200970528     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
078300970618IF  4C     CAEATB        IFEQ      *BLANKS
078400970618IF  5C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
078500970618     C     DDEVID        ANDLE     CAEDSC
078600970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
078700970618     C     DSCVID        ANDLE     CAEDSC
078800970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
078900970618     C     CAEDSC        ANDLE     DSCVID
079000970528     C                   SETON                                        606162
079100970619     C                   SETON                                        6364
079200970528     C                   MOVEL     MSG(17)       DSMSMS
079300970528     C                   MOVEL     '1'           ERRL00
079400120216     c                   select
079500120216     c                   when      winizioprf='EDP' and videpa='6'
079600120216     c                   eval      forzater='S'
079700120216     C                   MOVEL     MSG(10)       DSMSMS
079800120216     c                   when      winizioprf='EDP' and
079900120216     c                             (videpa='P' or videpa='A')
080000120216     c                             and ddevid>caedde
080100120216     c                   eval      forzater='S'
080200120216     c                   seton                                        42
080300120216     C                   clear                   DSMSMS
080400120216     c
080500120216     c                   endsl
080600090115     c
080700970528     C                   LEAVE
080800970528E   5C                   ENDIF
080900970528E   4C                   ENDIF
081000970528E   3C                   ENDIF
081100970618     C     KE2CAE        READE(N)  AZCAE01L                               99
081200970528E   2C                   ENDDO
081300970528E   1C                   ENDIF
081400970528     C*--------------------
081500970528     C* TERMINAL / LINEA ECCEZIONE
081600970528     C*--------------------
081700970528IF  1C     ERRL00        IFEQ      '0'
081800970528     C                   MOVEL     VIDTFE        WLIN
081900970528     C                   MOVEL     *BLANKS       WDLIN
082000970528     C                   EXSR      CHKLIN
082100970528     C                   MOVEL     WLIN          VIDTFE
082200970528     C                   MOVEL     WDLIN         VDDTFE
082300970528IF  2C     ERRL00        IFEQ      '1'
082400970528     C                   SETON                                        65
082500970528     C                   MOVEL     MSG(18)       DSMSMS
082600970528E   2C                   ENDIF
082700970528E   1C                   ENDIF
082800970528     C*
082900970618     C* SE ECCEZIONE IN ARRIVO, ECCEZIONE <> ARRIVO STANDARD
083000970618IF  1C     ERRL00        IFEQ      '0'
083100170308     c     vidgio        andeq     ' '
083200970618IF  2C     VIDEPA        IFEQ      'A'
083300970618IF  3C     VIDTFP        IFGT      *ZEROS
083400970618     C     VIDTFA        ANDGT     *ZEROS
083500970618     C     VIDTFE        ANDGT     *ZEROS
083600970618     C                   MOVEL     'A'           KCEEPA
083700970618     C                   Z-ADD     *ZEROS        KCETFP
083800970618     C                   MOVEL     TFAVID        KCETFA
083900970618     C     KE2CAE        SETLL     AZCAE01L
084000970618     C     KE2CAE        READE(N)  AZCAE01L                               99
084100970618DO  4C     *IN99         DOWEQ     *OFF
084200970618IF  5C     CAEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
084300970618IF  5C     CAETFE        IFEQ      VIDTFE
084400970618IF  7C     DDEVID        IFGE      CAEDDE                                       *DECORRENZA
084500970618     C     DDEVID        ANDLE     CAEDSC
084600970618     C     DSCVID        ORGE      CAEDDE                                       *SCADENZA
084700970618     C     DSCVID        ANDLE     CAEDSC
084800970618     C     CAEDDE        ORGE      DDEVID                                       *DECORRENZA
084900970618     C     CAEDSC        ANDLE     DSCVID
085000970618     C                   SETON                                        606162
085100970618     C                   SETON                                        63
085200970618     C                   MOVEL     MSG(20)       DSMSMS
085300970618     C                   MOVEL     '1'           ERRL00
085400970618     C                   LEAVE
085500970618E   7C                   ENDIF
085600970618E   6C                   ENDIF
085700970618E   5C                   ENDIF
085800970618     C     KE2CAE        READE(N)  AZCAE01L                               99
085900970618E   4C                   ENDDO                                                  *FINE TERM.ARR.STAND
086000970618E   3C                   ENDIF
086100970618E   2C                   ENDIF
086200970618E   1C                   ENDIF
086300170308     c* L'inserimento per un giorno per un certo giorno della settimana
086400170308     c*  vale solo per terminal di arrivo con eccezione di TFP
086500170403     c                   if        errl00=*off and
086600170308     c                             vidgio<>' ' and videpa<>'A'
086700170308     C                   SETON                                        57
086800170308     C                   MOVEL     MSG(29)       DSMSMS
086900170308     C                   MOVEL     '1'           ERRL00
087000170308     c                   endif
087100170403     c                   if        errl00=*off and
087200170403     c                             vidgio<>' ' and vidtfp<=*zeros
087300170308     C                   SETON                                        57
087400170308     C                   MOVEL     MSG(29)       DSMSMS
087500170308     C                   MOVEL     '1'           ERRL00
087600170308     c                   endif
087700170308     c* non inserire decorreza/scadenza
087800170403     c*                  if        errl00=*off and
087900170403     c*                            vidgio<>' ' and (viddde>0 or viddsc>0)
088000170403     C*                  SETON                                        57
088100170403     C*                  MOVEL     MSG(30)       DSMSMS
088200170403     C*                  MOVEL     '1'           ERRL00
088300170403     c*                  endif
088400020507
088500020507      * controlli per incongruenze tra record di tipo 'P' (partenza) e record di tip '6' (£6)
088600020507 b1  c                   if        errl00 = *off and
088700020507     c                             (videpa = 'P' or videpa = '6')
088800020507     c                   exsr      chk£6
088900020507 e1  c                   endif
089000040624     c
089100040624     c                   exsr      sr_atten
089200970618     C*
089300970528     C                   ENDSR
089400040624     C*------------------------------------------------------------------------*
089500040624     C* SR_atten -  Emissione messaggio informativo
089600040624     C*------------------------------------------------------------------------*
089700040624     C     SR_atten      BEGSR
089800040624     c     ERRL00        ifeq      '0'
089900040624     c     videpa        andeq     'A'
090000040624     c     funkey        andeq     '06'
090100040624     c     wforz         andeq     *blanks
090200040624     c* modificata linea o data
090300040624     c     depop1        ifeq      'O02'
090400040624     c     sav_vidtfe    andne     vidtfe
090500040624     c     depop1        oreq      'O02'
090600040624     c     sav_viddsc    andne     viddsc
090700040624     c* ripristino
090800040624     c     depop1        oreq      'O07'
090900040624     c* cancellazione
091000040624     c     depop1        oreq      'O04'
091100040624     c                   eval      wforz='F'
091200040624     c                   eval      errl00='1'
091300040624     c                   seton                                        41
091400040624     c                   endif
091500040624     c                   endif
091600040624     c                   endsr
091700970528     C*------------------------------------------------------------------------*
091800970528     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
091900970528     C*------------------------------------------------------------------------*
092000970528     C     CHKLIN        BEGSR
092100970528     C*
092200970618     C     *LIKE         DEFINE    VIDTFA        WLIN
092300970618     C     *LIKE         DEFINE    VDDTFA        WDLIN
092400970528     C*
092500970528     C* RICHIESTO ELENCO
092600970528     C                   SETOFF                                           28
092700970528     C     '?'           SCAN      WLIN                                   28
092800970528IF  1C     *IN28         IFEQ      *ON
092900970528     C                   MOVEL     *BLANKS       §COD1             3
093000970528     C                   MOVEL     *BLANKS       §TIP              1
093100970528     C                   MOVEL     *BLANKS       §DES1            25
093200971203     C                   MOVEL     *BLANKS       §PRA              3
093300971203     C                   CALL      'TNSD24R'
093400970528     C                   PARM                    §COD1
093500970528     C                   PARM                    §TIP
093600970528     C                   PARM                    §DES1
093700971203     C                   PARM                    §PRA
093800970528     C                   MOVEL     §COD1         WLIN
093900970528     C                   MOVEL     §DES1         WDLIN
094000970528E   1C                   ENDIF
094100970528     C*
094200970528     C* SOLO CARATTERI NUMERICI
094300970528     C                   SETOFF                                       28
094400970528     C                   TESTN                   WLIN                 28
094500970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
094600970528     C                   MOVEL     '1'           ERRL00
094700970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
094800970528     C                   MOVE      WLIN          W001              1
094900970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
095000970528     C                   MOVEL     '1'           ERRL00
095100970528X   2C                   ELSE                                                   *NUMERO
095200970528     C*
095300970528     C* CONTROLLA ESISTENZA CODICE
095400970528     C                   MOVEL     WLIN          KORFIL
095500970528     C     KEYORG        CHAIN     AZORG01L                           99
095600970528IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
095700970528     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
095800970528     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
095900970528     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
096000970528X   3C                   ELSE
096100970528     C                   MOVEL     '1'           ERRL00
096200970528E   3C                   ENDIF
096300970528E   2C                   ENDIF
096400970528E   1C                   ENDIF
096500970528     C*
096600970528     C                   ENDSR
096700970528     C*------------------------------------------------------------------------*
096800970528     C* CHKDA8 - CONTROLLA LA DATA (8)
096900970528     C*------------------------------------------------------------------------*
097000970528     C     CHKDA8        BEGSR
097100970528     C*
097200970528     C     *LIKE         DEFINE    VIDDDE        WDA8
097300970528     C     *LIKE         DEFINE    VIDDDE        DA8W
097400970528     C*
097500970528     C                   Z-ADD     WDA8          G08DAT
097600970528     C                   Z-ADD     *ZEROS        G08INV
097700970528     C                   MOVEL     '0'           G08ERR
097800970528     C                   CALL      'XSRDA8'
097900970528     C                   PARM                    WLBDA8
098000970528     C                   Z-ADD     G08DAT        WDA8
098100970528     C                   Z-ADD     G08INV        DA8W
098200970528IF  1C     G08ERR        IFEQ      '1'
098300970528     C                   MOVEL     '1'           ERRL00
098400970528E   1C                   ENDIF
098500970528     C*
098600970528     C                   ENDSR
098700020506      *------------------------------------------------------------------------*
098800080806      * CHK£6  - CONTROLLA INCONGRUENZE TRA RECOD 'A' E RECORD '6'
098900020506      *------------------------------------------------------------------------*
099000020507     c     chk£6         begsr
099100080806      ***
099200020507      * Record tipo 'P'
099300020507      *----------------
099400080806     c***                if        videpa = 'A'
099500080806     c***                eval      kceepa = '6'
099600080806     c***  ke1cae        setll     azcae01l
099700080806     c***                do        *hival
099800080806     c***  ke1cae        reade(n)  azcae01l
099900080806     c***                if        %eof(azcae01l)
100000080806     c***                leave
100100080806     c***                endif
100200080806     c***                if        caeatb<>*blanks
100300080806     c***                iter
100400080806     c***                endif
100500080806     c***
100600080806     c***                movel     caetfa        wcaetfa
100700080806     c***                movel     caetfe        wcaetfe
100800080806      **controllo la congruenza della scadenza tra record 'A' e record '6'
100900080806     c***                if        sav_viddsc <> viddsc and
101000080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid) and
101100080806     c***                          caedsc > dscvid
101200080806     c***                eval      *in64 = *on
101300080806     c***                eval      dsmsms = msg(22)
101400080806     c***                eval      errl00 = *on
101500080806     c***                leave
101600080806     c***                endif
101700080806      **controllo che non esistano ancora dei record di tipo '6'
101800080806     c***                if        sav_vidtfe <> vidtfe and
101900080806     c***                          (wcaetfa = tfpvid or wcaetfe = tfpvid)
102000080806     c***                eval      *in65 = *on
102100080806     c***                eval      dsmsms = msg(23)
102200080806     c***                eval      errl00 = *on
102300080806     c***                leave
102400080806     c***                endif
102500080806     c***                enddo
102600080806      ***
102700080806     c***                endif
102800080806      ***
102900080806      **Record tipo '6'
103000080806      *----------------
103100080806     c***                if        videpa = '6'
103200080806     c***                eval      kceepa = 'A'
103300080806     c***                eval      ok_date = *off
103400080806     c***                clear                   sav_tfe1
103500080806      **controllo il terminal di partenza del p.o. gestito
103600080806     c***                movel     vidtfa        kcetfp
103700080806     c***                clear                   kcetfa
103800080806     c***  ke2cae        setll     azcae01l
103900080806     c***                do        *hival
104000080806     c***  ke2cae        reade(n)  azcae01l
104100080806      **fine file
104200080806     c***                if        %eof(azcae01l)
104300080806     c***                leave
104400080806     c***                endif
104500080806      ***
104600080806     c***                eval      *in63 = *off
104700080806     c***                eval      *in64 = *off
104800080806      **controllo le date
104900080806     c***                if        ddevid >= caedde and dscvid <= caedsc
105000080806     c***                eval      sav_tfe1 = caetfe
105100080806     c***                eval      ok_date = *on
105200080806     c***                leave
105300080806      ***
105400080806      **controllo quale data non corrisponde
105500080806     c***                if        ok_date = *off
105600080806     c***                eval      *in63 = (ddevid < caedde)
105700080806     c***                eval      *in64 = (dscvid > caedsc)
105800080806     c***                endif
105900080806      ***
106000080806     c***                enddo
106100080806      **le date non corrispondono errore
106200080806     c***                if        ok_date = *off and (*in63 or *in64)
106300080806     c***                eval      dsmsms = msg(25)
106400080806     c***                eval      errl00 = *on
106500080806     c***                endif
106600080806
106700080806      **controllo il terminal di partenza del p.o. gestione
106800080806     c***                if        errl00 = *off
106900080806     c***                eval      ok_date = *off
107000080806     c***                clear                   sav_tfe2
107100080806     c***                movel     vidtfe        kcetfp
107200080806     c***                clear                   kcetfa
107300080806     c***  ke2cae        setll     azcae01l
107400080806     c***                do        *hival
107500080806     c***  ke2cae        reade(n)  azcae01l
107600020516      * fine file
107700080806     C***                if        %eof(azcae01l)
107800080806     c***                leave
107900080806     c***                endif
108000080806      ***
108100080806     c***                eval      *in63 = *off
108200080806     c***                eval      *in64 = *off
108300080806      **controllo le date
108400080806     c***                if        ddevid >= caedde and dscvid <= caedsc
108500080806     c***                eval      sav_tfe2 = caetfe
108600080806     c***                eval      ok_date = *on
108700080806     c***                leave
108800080806     c***                endif
108900020516      * controllo quale data non corrisponde
109000080806     c***                if        ok_date = *off
109100080806     c***                eval      *in63 = (ddevid < caedde)
109200080806     c***                eval      *in64 = (dscvid > caedsc)
109300080806     c***                endif
109400080806      ***
109500080806     c***                enddo
109600080806      **le date non corrispondono errore
109700080806     c***                if        ok_date = *off and (*in63 or *in64)
109800080806     c***                eval      dsmsms = msg(25)
109900080806     c***                eval      errl00 = *on
110000080806     c***                endif
110100080806      ***
110200080806     c***                endif
110300080806      ***
110400080806      **controllo se i 2 terminal  corrispondono
110500080806     c***                if        errl00 = *off and
110600080806     c***                          sav_tfe1 <> sav_tfe2
110700080806     c***                eval      *in65 = *on
110800080806     c***                eval      dsmsms = msg(24)
110900080806     c***                eval      errl00 = *on
111000080806     c***                endif
111100080806      ***
111200080806     c***                endif
111300020506
111400020506     c                   endsr
111500970526     C*------------------------------------------------------------------------*
111600970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
111700970526     C*------------------------------------------------------------------------*
111800970526     C     IREVID        BEGSR
111900090115     c                   clear                   forzater
112000120216     c                   setoff                                       42
112100120214     c                   clear                   forzaSCA
112200970526     C* CAMPI
112300970618     C                   MOVEL     CAEEPA        VIDEPA
112400970618     C                   MOVEL     CAETFP        VIDTFP
112500970618     C                   MOVEL     CAETFA        VIDTFA
112600970618     C                   Z-ADD     CAEDDE        G08INV
112700970528     C                   Z-ADD     *ZEROS        G08DAT
112800970528     C                   MOVEL     '3'           G08ERR
112900970528     C                   CALL      'XSRDA8'
113000970528     C                   PARM                    WLBDA8
113100970528     C                   Z-ADD     G08DAT        VIDDDE
113200970618     C                   Z-ADD     CAEDSC        G08INV
113300970528     C                   Z-ADD     *ZEROS        G08DAT
113400970528     C                   MOVEL     '3'           G08ERR
113500970528     C                   CALL      'XSRDA8'
113600970528     C                   PARM                    WLBDA8
113700970528     C                   Z-ADD     G08DAT        VIDDSC
113800970618     C                   Z-ADD     CAETFE        VIDTFE
113900020510
114000020510      * salvo data scadenza e terminal per controlli congruenze tra record
114100020510      * di tipo 'P' e record di tipo '6'
114200020510     c                   eval      sav_viddsc = viddsc
114300020510     c                   eval      sav_vidtfe = vidtfe
114400120903
114500120903     C* metto SI come default per gestione corrispondente rcd C se tipo terminal £6
114600120911     C                   MOVEL     'SI'          VIDCRC
114700970527     C*
114800970526     C                   ENDSR
114900970526     C*------------------------------------------------------------------------*
115000970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
115100970526     C*------------------------------------------------------------------------*
115200970526     C     AZZVID        BEGSR
115300970526     C* CAMPI
115400090115     c                   clear                   forzater
115500120216     c                   setoff                                       42
115600120214     c                   clear                   forzaSCA
115700970528     C                   MOVEL     *BLANKS       VIDEPA
115800970618     C                   MOVEL     *BLANKS       VIDTFP
115900970618     C                   MOVEL     *BLANKS       VIDTFA
116000170308     C                   MOVEL     *BLANKS       VIDgio
116100970528     C                   Z-ADD     *ZEROS        VIDDDE
116200970528     C                   Z-ADD     *ZEROS        VIDDSC
116300970618     C                   Z-ADD     *ZEROS        VIDTFE
116400120911     C                   MOVEL     'SI'          VIDCRC
116500970526     C* DECODIFICHE
116600970528     C                   MOVEL     *BLANKS       VDDTFP
116700970528     C                   MOVEL     *BLANKS       VDDTFA
116800970528     C                   MOVEL     *BLANKS       VDDTFE
116900970526     C*
117000970526     C                   ENDSR
117100970526     C*------------------------------------------------------------------------*
117200970618     C* IMPCAE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
117300970526     C*------------------------------------------------------------------------*
117400970618     C     IMPCAE        BEGSR
117500970526     C*
117600970618     C                   Z-ADD     DSCVID        CAEDSC
117700970618     C                   Z-ADD     VIDTFE        CAETFE
117800970526     C*
117900970526     C                   ENDSR
118000970526     C*------------------------------------------------------------------------*
118100970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
118200970526     C*------------------------------------------------------------------------*
118300970526     C     IMPFUN        BEGSR
118400970526     C*
118500970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
118600970526IF  1C     DEPOP1        IFEQ      'O05'
118700970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
118800970526     C                   MOVEL     'N'           FUDATT
118900970526     C                   MOVEL     FUDRIG        FUD(6)
119000970526E   1C                   ENDIF
119100970526     C*
119200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
119300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
119400970526     C                   EXSR      FUNGES
119500970526     C*
119600970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
119700970526     C*---
119800970526     C* 90 -> PROTEZIONE CHIAVE
119900970526     C* 91 -> PROTEZIONE TOTALE
120000970526     C*---
120100970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
120200970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
120300970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
120400970526     C                   SETOFF                                       91
120500120903     C                   SETOFF                                       92        *NO protect flag £6
120600970526E   2C                   ENDIF
120700970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
120800970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
120900120903     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
121000120911     C                   IF        D00EPA = '6'
121100120911     C                   SETOFF                                       92        *NO PR/ND flag £6
121200120911E   2C                   ELSE
121300120911     C                   SETON                                        92        *protect flag £6
121400120911E   2C                   ENDIF
121500120911E   2C                   ENDIF
121600970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
121700120903     C***  DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
121800970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
121900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
122000970526     C                   SETON                                        91
122100120911     C                   IF        D00EPA = '6'
122200120911     C                   SETOFF                                       92        *NO PR/ND flag £6
122300120911E   2C                   ELSE
122400120911     C                   SETON                                        92        *protect flag £6
122500120911E   2C                   ENDIF
122600970526E   2C                   ENDIF
122700120903     C     DEPOP1        IFEQ      'O05'                                        *VISUALIZZAZIONE
122800120903     C                   SETON                                        90        *TUTTE LE PROTEZIONI
122900120903     C                   SETON                                        91
123000120911     C                   SETON                                        92        *protect flag £6
123100120903E   2C                   ENDIF
123200131015     c
123300131015     C     DEPOP1        IFEQ      'O+1'                                        *VISUALIZZAZIONE
123400131015     C                   SETON                                        90        *TUTTE LE PROTEZIONI
123500131015     C                   SETON                                        91
123600131015     C                   SETON                                        92        *protect flag £6
123700131015E   2C                   ENDIF
123800970526X   1C                   ELSE                                                   *I N T E R R O G A
123900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
124000970526     C                   SETON                                        91
124100120903     C                   SETON                                        92        *Protect flag £6
124200970526E   1C                   ENDIF
124300970526     C*
124400970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
124500970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
124600970618     C                   MOVEL     D00EPA        KCEEPA
124700970618     C                   Z-ADD     D00ETP        KCETFP
124800970618     C                   Z-ADD     D00ETA        KCETFA
124900970618     C                   Z-ADD     D00EDD        KCEDDE
125000970618     C     KEYCAE        CHAIN     AZCAE01L                           9998
125100970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
125200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
125300970526     C                   MOVEL     MSG(6)        DSMSMS
125400970526X   2C                   ELSE
125500970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
125600980128IF  3C     DEPOP1        IFEQ      'O05'
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
126900970526IF  1C     DEPOP1        IFEQ      'O04'
127000970526     C                   MOVEL     DEC(2)        VIDFUN
127100970526E   1C                   ENDIF
127200970526IF  1C     DEPOP1        IFEQ      'O05'
127300970526     C                   MOVEL     DEC(3)        VIDFUN
127400970526E   1C                   ENDIF
127500970526IF  1C     DEPOP1        IFEQ      'O07'
127600970526     C                   MOVEL     DEC(4)        VIDFUN
127700970526E   1C                   ENDIF
127800970526IF  1C     DEPOP1        IFEQ      'F06'
127900970526     C                   MOVEL     DEC(5)        VIDFUN
128000970526E   1C                   ENDIF
128100131015IF  1C     DEPOP1        IFEQ      'O+1'
128200131015     C                   MOVEL     DEC(6)        VIDFUN
128300131015E   1C                   ENDIF
128400970526     C*
128500970526     C                   ENDSR
128600970526     C*------------------------------------------------------------------------*
128700970526     C* *INZSR - OPERAZIONI INIZIALI
128800970526     C*------------------------------------------------------------------------*
128900970526     C     *INZSR        BEGSR
129000970526     C*---
129100970526     C* RICEVIMENTO PARAMETRI
129200970526     C*---
129300970526     C     *ENTRY        PLIST
129400970612     C                   PARM                    KPJBA
129500970612     C                   PARM                    DSSI00
129600000427     C*
129700000427     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
129800970526     C*
129900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
130000970526     C     XMSGPA        PLIST
130100970526     C                   PARM                    DSMSMS
130200970526     C                   PARM                    DSMSPG
130300970526     C                   PARM                    DSMSKE
130400970526     C                   PARM                    DSMSER
130500970526     C*
130600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
130700970526     C                   CALL      'XKEY'
130800970526     C                   PARM                    DSKEY
130900970526     C*---
131000970526     C* VARIABILI NON RIFERITE AL DATA BASE
131100970526     C*---
131200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
131300970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
131400970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
131500970526     C     *LIKE         DEFINE    D00OP0        WOP0
131600970526     C     *LIKE         DEFINE    D00OP1        WOP1
131700970526     C*---
131800970526     C* VARIABILI DI CONTROLLO
131900970526     C*---
132000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
132100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
132200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
132300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
132400970526     C*
132500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
132600970528     C                   MOVEL     'TISI18R'     DSMSPG
132700970528     C                   MOVEL     'TISI18R'     MSGQUE
132800970526     C                   MOVEL     *BLANKS       MSGKEY
132900970526     C                   MOVEL     *BLANKS       DSMSMS
133000970526     C*---
133100970526     C* VARIABILI RIFERITE AL DATA BASE
133200970526     C*---
133300970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
133400970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
133500970618     C     *LIKE         DEFINE    CAETFP        KCETFP
133600970618     C     *LIKE         DEFINE    CAETFA        KCETFA
133700970618     C     *LIKE         DEFINE    CAEDDE        KCEDDE
133800970526     C*---
133900970526     C* CHIAVI DI LETTURA
134000970526     C*---
134100970528     C* LETTURA AZORG01L - COMPLETA
134200970528     C     KEYORG        KLIST
134300970528     C                   KFLD                    KORFIL                         *FILIALE
134800970618     C* LETTURA AZCAE01L - COMPLETA
134900970618     C     KEYCAE        KLIST
135000970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
135100970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
135200970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
135300970618     C                   KFLD                    KCEDDE                         *DECORRENZA
135400970618     C* LETTURA AZCAE01L - PARZIALE
135500970618     C     KE2CAE        KLIST
135600970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
135700970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
135800970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
135900020506      * Lettura AZCAE01L
136000020506     c     ke1cae        klist
136100020506     c                   kfld                    kceepa
136200970528     C*
136300970528     C* CALCOLA LA DATA CORRENTE
136400970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
136500970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
136600970528     C                   Z-ADD     WN8           G08DAT
136700970528     C                   Z-ADD     *ZEROS        G08INV
136800970528     C                   MOVEL     '0'           G08ERR
136900970528     C                   CALL      'XSRDA8'
137000970528     C                   PARM                    WLBDA8
137100970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
137200970526     C*
137300970526     C* DEPOSITA LE OPZIONI DI ENTRATA
137400970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
137500970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
137600971231     C*
137700971231     C* IT'S A JOKE
137800971231IF  1C     KNMUS         IFEQ      'EDPES'
137900971231     C                   SETON                                            01
138000971231E   1C                   ENDIF
138100050412     c                   movel     knmus         winizioprf        3
138200970526     C*
138300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
138400970526     C                   EXSR      IMPFUN
138500970526     C*
138600970526     C                   ENDSR
138700970526     C*------------------------------------------------------------------------*
138800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
138900970526     C*------------------------------------------------------------------------*
139000970526     C     FUNGES        BEGSR
139100970526     C*
139200970526     C* PULISCO CAMPI DI LAVORO
139300970526     C                   Z-ADD     0             W                 5 0
139400970526     C*
139500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
139600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
139700970526     C     1             DO        24            I
139800970526     C                   MOVEL     FUD(I)        FUDRIG
139900970526     C*
140000970526     C* PULIZIA FUNZIONI PRECEDENTI
140100970526     C                   MOVEL     *BLANKS       FUV(I)
140200970526     C*
140300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
140400970526     C* (SALTA F24)
140500970526     C     FUDATT        IFEQ      'S'
140600970526     C     FUDFUN        ANDNE     '24'
140700970526     C                   ADD       1             W
140800970526     C                   MOVEL     FUDDES        FUV(W)
140900970526     C                   END
141000970526     C                   END                                                    DO WHILE
141100970526     C*
141200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
141300970526     C* ALTRIMENTI DISATTIVO F24
141400970526     C     1             DO        24            I
141500970526     C                   MOVEL     FUD(I)        FUDRIG
141600970526     C*
141700970526     C     FUDFUN        IFEQ      '24'
141800970526     C     W             IFGT      6
141900970526     C                   MOVEL     'S'           FUDATT
142000970526     C                   ELSE
142100970526     C                   MOVEL     *BLANKS       FUDATT
142200970526     C                   END                                                    W GT 8
142300970526     C                   MOVEL     FUDRIG        FUD(I)
142400970526     C                   END
142500970526     C                   END                                                    DO WHILE
142600970526     C*
142700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
142800970526     C     W             DIV       6             I
142900970526     C                   MVR                     Z
143000970526     C*
143100970526     C     Z             IFNE      0
143200970526     C     I             ADD       1             Z
143300970526     C                   ELSE
143400970526     C                   Z-ADD     I             Z                 5 0
143500970526     C                   END
143600970526     C*
143700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
143800970526     C                   ADD       1             FUNGRU
143900970526     C     FUNGRU        IFGT      Z
144000970526     C     FUNGRU        ORLT      1
144100970526     C                   Z-ADD     1             FUNGRU
144200970526     C                   END
144300970526     C*
144400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
144500970526     C     FUNGRU        IFEQ      1
144600970526     C                   MOVEL     FUV(1)        FUN001
144700970526     C                   MOVEL     FUV(2)        FUN002
144800970526     C                   MOVEL     FUV(3)        FUN003
144900970526     C                   MOVEL     FUV(4)        FUN004
145000970526     C                   MOVEL     FUV(5)        FUN005
145100970526     C                   MOVEL     FUV(6)        FUN006
145200970526     C                   END
145300970526     C*
145400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
145500970526     C     FUNGRU        IFEQ      2
145600970526     C                   MOVEL     FUV(7)        FUN001
145700970526     C                   MOVEL     FUV(8)        FUN002
145800970526     C                   MOVEL     FUV(9)        FUN003
145900970526     C                   MOVEL     FUV(10)       FUN004
146000970526     C                   MOVEL     FUV(11)       FUN005
146100970526     C                   MOVEL     FUV(12)       FUN006
146200970526     C                   END
146300970526     C*
146400970526     C* TERZO GRUPPO DI 6 FUNZIONI
146500970526     C     FUNGRU        IFEQ      3
146600970526     C                   MOVEL     FUV(13)       FUN001
146700970526     C                   MOVEL     FUV(14)       FUN002
146800970526     C                   MOVEL     FUV(15)       FUN003
146900970526     C                   MOVEL     FUV(16)       FUN004
147000970526     C                   MOVEL     FUV(17)       FUN005
147100970526     C                   MOVEL     FUV(18)       FUN006
147200970526     C                   END
147300970526     C*
147400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
147500970526     C     FUNGRU        IFEQ      4
147600970526     C                   MOVEL     FUV(19)       FUN001
147700970526     C                   MOVEL     FUV(20)       FUN002
147800970526     C                   MOVEL     FUV(21)       FUN003
147900970526     C                   MOVEL     FUV(22)       FUN004
148000970526     C                   MOVEL     *BLANKS       FUN005
148100970526     C                   MOVEL     *BLANKS       FUN006
148200970526     C                   END
148300970526     C*
148400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
148500970526     C                   MOVEL     *BLANKS       FUN007
148600970526     C                   MOVEL     *BLANKS       FUN008
148700970526     C*
148800970526     C     1             DO        24            I
148900970526     C                   MOVEL     FUD(I)        FUDRIG
149000970526     C     FUDFUN        IFEQ      '24'
149100970526     C     FUDATT        ANDEQ     'S'
149200970526     C                   MOVEL     FUDDES        FUN008
149300970526     C                   END
149400970526     C                   END                                                    DO
149500970526     C*
149600970526     C                   ENDSR
149700970526     C*------------------------------------------------------------------------*
149800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
149900970526     C*------------------------------------------------------------------------*
150000970526     C     FUNCON        BEGSR
150100970526     C*
150200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
150300970526     C                   EXSR      FUNVAR
150400970526     C*
150500970526     C* PULISCO CAMPI DI LAVORO
150600970526     C                   Z-ADD     0             I                 5 0
150700970526     C                   Z-ADD     0             W                 5 0
150800970526     C*
150900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
151000970526     C     FUNKEY        IFNE      *BLANKS
151100970526     C     1             DO        24            I
151200970526     C                   MOVEL     FUD(I)        FUDRIG
151300970526     C*
151400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
151500970526     C     FUNKEY        IFEQ      FUDFUN
151600970526     C     FUDATT        ANDEQ     'S'
151700970526     C                   Z-ADD     I             W
151800970526     C                   END
151900970526     C                   END                                                    DO WHILE
152000970526     C*
152100970526     C* FUNZIONE NON ATTIVA (ERRORE)
152200970526     C     W             IFEQ      0
152300970526     C*
152400970526     C* EMISSIONE MESSAGGIO DI ERRORE
152500970526     C                   MOVEL     MSG(1)        DSMSMS
152600970526     C*
152700970526     C                   END                                                    W=0
152800970526     C                   END                                                    FUNKEY NE *BLANKS
152900970526     C*
153000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
153100970526     C     W             IFNE      0
153200970526     C     FUNKEY        OREQ      *BLANKS
153300970526     C     FUNKEY        IFEQ      *BLANKS
153400970526     C                   EXSR      ENTGES
153500970526     C                   END
153600970526     C     FUNKEY        IFEQ      '03'
153700970526     C                   EXSR      F03GES
153800970526     C                   END
153900970526     C     FUNKEY        IFEQ      '06'
154000970526     C                   EXSR      F06GES
154100970526     C                   END
154200970526     C     FUNKEY        IFEQ      '12'
154300970526     C                   EXSR      F12GES
154400970526     C                   END
154500970526     C     FUNKEY        IFEQ      '24'
154600970526     C                   EXSR      F24GES
154700970526     C                   END
154800970526     C                   END
154900970526     C*
155000970526     C                   ENDSR
155100970526     C*------------------------------------------------------------------------*
155200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
155300970526     C*------------------------------------------------------------------------*
155400970526     C     FUNVAR        BEGSR
155500970526     C*
155600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
155700970526     C*
155800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
155900970526     C     KEY           IFEQ      ENTER
156000970526     C                   MOVEL     *BLANKS       FUNKEY
156100970526     C                   END
156200970526     C     KEY           IFEQ      F1
156300970526     C                   MOVEL     '01'          FUNKEY
156400970526     C                   END
156500970526     C     KEY           IFEQ      F2
156600970526     C                   MOVEL     '02'          FUNKEY
156700970526     C                   END
156800970526     C     KEY           IFEQ      F3
156900970526     C                   MOVEL     '03'          FUNKEY
157000970526     C                   END
157100970526     C     KEY           IFEQ      F4
157200970526     C                   MOVEL     '04'          FUNKEY
157300970526     C                   END
157400970526     C     KEY           IFEQ      F5
157500970526     C                   MOVEL     '05'          FUNKEY
157600970526     C                   END
157700970526     C     KEY           IFEQ      F6
157800970526     C                   MOVEL     '06'          FUNKEY
157900970526     C                   END
158000970526     C     KEY           IFEQ      F7
158100970526     C                   MOVEL     '07'          FUNKEY
158200970526     C                   END
158300970526     C     KEY           IFEQ      F8
158400970526     C                   MOVEL     '08'          FUNKEY
158500970526     C                   END
158600970526     C     KEY           IFEQ      F9
158700970526     C                   MOVEL     '09'          FUNKEY
158800970526     C                   END
158900970526     C     KEY           IFEQ      F10
159000970526     C                   MOVEL     '10'          FUNKEY
159100970526     C                   END
159200970526     C     KEY           IFEQ      F11
159300970526     C                   MOVEL     '11'          FUNKEY
159400970526     C                   END
159500970526     C     KEY           IFEQ      F12
159600970526     C                   MOVEL     '12'          FUNKEY
159700970526     C                   END
159800970526     C     KEY           IFEQ      F13
159900970526     C                   MOVEL     '13'          FUNKEY
160000970526     C                   END
160100970526     C     KEY           IFEQ      F14
160200970526     C                   MOVEL     '14'          FUNKEY
160300970526     C                   END
160400970526     C     KEY           IFEQ      F15
160500970526     C                   MOVEL     '15'          FUNKEY
160600970526     C                   END
160700970526     C     KEY           IFEQ      F16
160800970526     C                   MOVEL     '16'          FUNKEY
160900970526     C                   END
161000970526     C     KEY           IFEQ      F17
161100970526     C                   MOVEL     '17'          FUNKEY
161200970526     C                   END
161300970526     C     KEY           IFEQ      F18
161400970526     C                   MOVEL     '18'          FUNKEY
161500970526     C                   END
161600970526     C     KEY           IFEQ      F19
161700970526     C                   MOVEL     '19'          FUNKEY
161800970526     C                   END
161900970526     C     KEY           IFEQ      F20
162000970526     C                   MOVEL     '20'          FUNKEY
162100970526     C                   END
162200970526     C     KEY           IFEQ      F21
162300970526     C                   MOVEL     '21'          FUNKEY
162400970526     C                   END
162500970526     C     KEY           IFEQ      F22
162600970526     C                   MOVEL     '22'          FUNKEY
162700970526     C                   END
162800970526     C     KEY           IFEQ      F23
162900970526     C                   MOVEL     '23'          FUNKEY
163000970526     C                   END
163100970526     C     KEY           IFEQ      F24
163200970526     C                   MOVEL     '24'          FUNKEY
163300970526     C                   END
163400970526     C     KEY           IFEQ      ROLLUP
163500970526     C                   MOVEL     'RU'          FUNKEY
163600970526     C                   END
163700970526     C     KEY           IFEQ      HELP
163800970526     C                   MOVEL     'HP'          FUNKEY
163900970526     C                   END
164000970526     C                   ENDSR
164100970526     C*------------------------------------------------------------------------*
164200970526     C* NOTE COSTRUZIONE SCHIERE
164300970526     C*------------------------------------------------------------------------*
164400970526     C*
164500970526     C* -MSG-MESSAGGI
164600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
164700970526     C* (NON TOCCARE MSG,1 E MSG,2)
164800970526     C*
164900970526     C* -CMD-COMANDI
165000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
165100970526     C*
165200970526     C* -OPD-OPZIONI
165300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
165400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
165500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
165600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
165700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
165800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
165900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
166000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
166100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
166200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
166300970526     C*
166400970526     C* -FUD-FUNZIONI
166500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
166600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
166700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
166800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
166900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
167000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
167100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
167200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
167300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
167400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
167500970526     O*------------------------------------------------------------------------*
167600970526** DEC - DECODIFICHE
167700970526    MODIFICA                                                      1
167800970526  ANNULLAMENTO                                                    2
167900970526VISUALIZZAZIONE                                                   3
168000970526   RIPRISTINO                                                     4
168100970526  INSERIMENTO                                                     5
168200131015PosticipaDecorr.                                                  6
168300970526** MSG - MESSAGGI
168400970528TISI18R- Tasto funzionale non gestito                                          1
168500970528TISI18R- Inserimento effettuato                                                2
168600970528TISI18R- Modifica effettuata                                                   3
168700970528TISI18R- Annullamento effettuato                                               4
168800970528TISI18R- Ripristino effettuato                                                 5
168900970528TISI18R- Richiesta funzione per record inesistente                             6
169000970528TISI18R- Record momentaneamente allocato: riprovare più tardi                  7
169100970528TISI18R- Richiesta funzione per record esistente                               8
169200970528TISI18R- 9
169300090115TISI18R- Il record immesso si sovrappone ad uno già in decorrenza.ENTER forza  10
169400140117TISI18R- Immettere un tipo terminal esistente                                  11
169500970528TISI18R- Terminal / Linea partenza errato                                      12
169600970528TISI18R- Terminal / Linea arrivo errato                                        13
169700970528TISI18R- Data errata                                                           14
169800970528TISI18R- La data scadenza deve essere maggiore della decorrenza                15
169900120214TISI18R- Il record non può scadere in un giorno già trascorso.                 16
170000971203TISI18R- Il record immesso si sovrappone ad uno già in decorrenza              17
170100971203TISI18R- Terminal / linea errati                                               18
170200140117TISI18R- Impossibile inserire terminal Arrivo per tipo "P" "O"                 19
170300970618TISI18R- L'eccezione non può essere uguale al terminal arrivo standard         20
170400140117TISI18R- Per tipo terminal "6"  "C"  "J" non inserire il terminal partenza     21
170500020507TISI18R- Scadenza incongruente con scadenza record tipo "6"                    22
170600020507TISI18R- Esistono dei record di tipo "6". Impossibile la modifica              23
170700080806TISI18R- Il p.o. gestione ha il terminal di Arrivo   diverso dal p.o. gestito  24
170800080806TISI18R- Data decorrenza/scadenza incongruenti con le date del terminal Arr.   25
170900120214TISI18R- Il record non può scadere in un giorno già trascorso.ENTER forza      26
171000120903TISI18R- Operazione effettuata sul tipo terminal £6 ma non esiste il tipo C    27
171100131015TISI18R- Errori nel posticipare la decorrenza : controllare!!                  28
171200170403Automatismo inserimento solo per ter.ARRIVO con eccezione lnp                  29
171300170308Per automatismo inserimento eccezione ter.arrivo, non immettere decor/scad     30
171400970526** FUD - FUNZIONI
171500970526                   -        (S/N)=N                        funz.01¬
171600970526                   -        (S/N)=N                        funz.02¬
171700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
171800970526                   -        (S/N)=N                        funz.04¬
171900970526                   -        (S/N)=N                        funz.05¬
172000970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
172100970527                   -        (S/N)=N                        funz.07¬
172200970527                   -        (S/N)=N                        funz.08¬
172300970527                   -        (S/N)=N                        funz.09¬
172400970527                   -        (S/N)=N                        funz.10¬
172500970526                   -        (S/N)=N                        funz.11¬
172600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
172700970526                   -        (S/N)=N                        funz.13¬
172800970526                   -        (S/N)=N                        funz.14¬
172900970526                   -        (S/N)=N                        funz.15¬
173000970526                   -        (S/N)=N                        funz.16¬
173100970526                   -        (S/N)=N                        funz.17¬
173200970526                   -        (S/N)=N                        funz.18¬
173300970526                   -        (S/N)=N                        funz.19¬
173400970526                   -        (S/N)=N                        funz.20¬
173500970526                   -        (S/N)=N                        funz.21¬
173600970526                   -        (S/N)=N                        funz.22¬
173700970526                   -        (S/N)=N                        funz.23¬
173800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
