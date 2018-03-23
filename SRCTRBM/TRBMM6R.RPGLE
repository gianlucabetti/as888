000100970526     H*------------------------------------------------------------------------*
000200030822     H* Gestione - regole invii spool via e-mail
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800100219     FTRBMM6D   CF   E             WORKSTN INFDS(DEVDS1)
000900030822     F                                     SFILE(TCM6DMS:MSGULT)
001000030822     Ftimar01l  UF A E           K DISK
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D*------------
001500970526     D* DECODIFICHE
001600970526     D*------------
001700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* MESSAGGI
002000970526     D*------------
002100050816     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* DESCRIZIONI TASTI FUNZIONALI
002400970526     D*------------
002500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* FUNZIONI DA CARICARE A VIDEO
002800970526     D*------------
002900970526     D FUV             S             19    DIM(24)
003000970526     D*------------------------------------------------------------------------*
003100970526     D* INPUT
003200970526     D*------------------------------------------------------------------------*
003300970526     D*------------
003400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003500970526     D*------------
003600970526     D FUDDS           DS
003700970526     D  FUDDE1                 1      1
003800970526     D  FUDFUN                 2      3
003900970526     D  FUDDE2                 4     19
004000970526     D  FUDFI0                20     34
004100970526     D  FUDATT                35     35
004200970526     D  FUDFI1                36     80
004300970526     D  FUDDES                 1     19
004400970526     D  FUDRIG                 1     80
004500970526     D*------------
004600970526     D* DS GESTIONE TASTI FUNZIONALI
004700970526     D*------------
004800970526     D DSKEY         E DS
004900970526     D DEVDS1          DS
005000970526     D  KEY                  369    369
005100970526     D*------------
005200970526     D* DS "XMSG" - GESTIONE MESSAGGI
005300970526     D*------------
005400970526     D DSMSG         E DS
005500021219     D*------------
005600970526     D* ARCHITETTURA
005700970526     D*------------
005800970526     D KPJBA         E DS
005900021219     D*------------------
006000021219     D* PASSAGGIO PARAMETRI
006100021219     D*------------------
006200030822     D PARAM           DS                  INZ
006300030822     D  POP0                   1      3
006400030822     D  POP1                   4      6
006500030822     D  PCODR                  7      9
006600030822     D  PMSG                  10     89
006700021219     D*-------------------
006800021219     D* DS REPERIMENTO DATI UTENTE
006900021219     D*-------------------
007000021219     D TIBS34DS      E DS                                                       *Profili utente
007100021219     D DDATIUTE      E DS                                                       *Dati utente
007200021219     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007300040615     D*-------------------
007400040615     D* COSTANTE X CONTROLLO FORMALE NOMI FILE
007500040615     D*-------------------
007600040726     D SkNomiNoOK      C                   CONST('/\:;,!"#£$%&|"=''+§°çòèéì[]{}-
007700040726     D                                     àù?<>')
007800040726     D SkPathNoOK      C                   CONST('/?:;,!"#£$%&|"=''+§°çòèéì[]{}-
007900040726     D                                     àù<>')
008000970526     C*------------------------------------------------------------------------*
008100970526     C* MAIN LINES
008200970526     C*------------------------------------------------------------------------*
008300970526     C*
008400970526     C* NON CI SONO ERRORI IN ENTRATA
008500021211     C     ERRL00        IFEQ      '0'
008600970526     C*
008700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008800021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009100970526     C*
009200970526     C* EMETTE FORMATO VIDEO
009300970526     C                   EXSR      BUTFMR
009400970526     C*
009500970526     C* GESTIONE TASTI FUNZIONALI
009600970526     C                   EXSR      FUNCON
009700021211     C                   ENDDO
009800970526     C*
009900021211     C                   ENDIF
010000021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
010100021211     C                   MOVEL     PARAM         KPJBU
010200970526     C* FINE PGM
010300970526     C                   SETON                                        LR
010400970526     C*------------------------------------------------------------------------*
010500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010600970526     C*------------------------------------------------------------------------*
010700970526     C     BUTFMR        BEGSR
010800970526     C*
010900970526     C* EMETTE LA TESTATA
011000030822     C                   WRITE     TCM6D01
011100970526     C*
011200970526     C* SPEDISCE EVENTUALE MESSAGGIO
011300970526     C                   EXSR      SNDMSG
011400970526     C*
011500970526     C* EMETTE LE FUNZIONI
011600030822     C                   WRITE     TCM6D02
011700970526     C*
011800970526     C* EMETTE IL VIDEO
011900030822     C                   EXFMT     TCM6D03
012000970526     C*
012100970526     C* REIMPOSTA FLAG DI ERRORE
012200970526     C                   MOVEL     '0'           ERRL00
012300970526     C*
012400970526     C                   ENDSR
012500970526     C*------------------------------------------------------------------------*
012600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
012700970526     C*------------------------------------------------------------------------*
012800970526     C     SNDMSG        BEGSR
012900970526     C*
013000970526     C* PULISCE IL SFILE MESSAGGI
013100970526     C                   MOVEL     '1'           *IN40
013200030822     C                   WRITE     TCM6DMC
013300970526     C                   MOVEL     '0'           *IN40
013400970526     C*
013500970526     C* VISUALIZZA IL MESSAGGIO
013600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
013700970526     C                   CALL      'XMSG'        XMSGPA
013800021211     C     DSMSER        IFEQ      *ZEROS
013900970526     C                   MOVEL     DSMSKE        MSGKEY
014000021211     C                   ENDIF
014100970526     C                   Z-ADD     1             MSGULT
014200030822     C                   WRITE     TCM6DMS
014300030822     C                   WRITE     TCM6DMC
014400970526     C*
014500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
014600970526     C                   MOVEL     *BLANKS       DSMSMS
014700970526     C*
014800970526     C                   ENDSR
014900970526     C*------------------------------------------------------------------------*
015000021219     C* ENTGES - GESTIONE ENTER -> INVIO
015100970526     C*------------------------------------------------------------------------*
015200970526     C     ENTGES        BEGSR
015300970526     C*
015400970526     C* CONTROLLA I CAMPI IMMESSI
015500030822     C                   EXSR      DECODI
015600970526     C*
015700970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
015800021211     C     POP1          IFEQ      'O05'
015900021211     C                   MOVEL     *BLANKS       PMSG
016000970526     C                   MOVEL     '1'           FINL00
016100021211     C                   ENDIF
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* F03GES - GESTIONE F03 -> FINE
016600970526     C*------------------------------------------------------------------------*
016700970526     C     F03GES        BEGSR
016800970526     C*
016900970526     C* IMPOSTA "PREMUTO F03"
017000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017100970526     C*
017200970526     C                   ENDSR
017300030509     C*------------------------------------------------------------------------*
017400030509     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
017500030509     C*------------------------------------------------------------------------*
017600030509     C     F06GES        BEGSR
017700030822     C*
017800030822     C                   EXSR      DECODI
017900030509     C*
018000030509     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
018100030509     C     POP1          IFEQ      'O02'
018200030509     C                   EXSR      MODIFI
018300030509     C                   ENDIF
018400040217     C     POP1          IFEQ      'O03'
018500040217     C                   EXSR      COPIA
018600040217     C                   ENDIF
018700030822     C     POP1          IFEQ      'F06'
018800030822     C                   EXSR      INSERI
018900030822     C                   ENDIF
019000030509     C*
019100030509     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300970526     C* F12GES - GESTIONE F12 -> RITORNO
019400970526     C*------------------------------------------------------------------------*
019500970526     C     F12GES        BEGSR
019600970526     C*
019700970526     C* IMPOSTA "PREMUTO F12"
019800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020300970526     C*------------------------------------------------------------------------*
020400970526     C     F24GES        BEGSR
020500970526     C*
020600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
020700970526     C*
020800970526     C                   ENDSR
020900030509     C*------------------------------------------------------------------------*
021000030509     C* MODIFI - OPERAZIONI DI MODIFICA RECORD
021100030509     C*------------------------------------------------------------------------*
021200030509     C     MODIFI        BEGSR
021300030509     C*
021400030509     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
021500030509     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021600030822     C                   EVAL      MARCODD  = VIDCODD
021700030822     C                   EVAL      MARTOUT  = VIDTOUT
021800030822     C                   EVAL      MARPOUT  = VIDPOUT
021900030822     C                   EVAL      MARFOUT  = VIDFOUT
022000030822     C                   EVAL      MARTFIL  = VIDTFIL
022100030822     C                   EVAL      MARTMOD  = VIDTMOD
022200030822     C                   EVAL      MAROPAG  = VIDOPAG
022300030822     C                   EVAL      MARCPI   = VIDCPI
022400030822     C                   EVAL      MARDCHR  = VIDDCHR
022500030822     C                   EVAL      MARINTL  = VIDINTL
022600040930     C                   EVAL      MARMARSX = VIDMARSX
022700040930     C                   EVAL      MARUNIM  = VIDUNIM
022800030822     C                   EVAL      MARFONT  = VIDFONT
022900030822     C                   EVAL      MARTEML  = VIDTEML
023000030822     C                   EVAL      MARTITP  = VIDTITP
023100030822     C                   EVAL      MAROBJP  = VIDOBJP
023200030822     C                   EVAL      MARKEYP  = VIDKEYP
023300030822     C                   EVAL      MARFLOG  = VIDFLOG
023400030822     C                   EVAL      MARLOGO  = VIDLOGO
023500030822     C                   EVAL      MARLOGS  = VIDLOGS
023600030822     C                   EVAL      MARLOGT  = VIDLOGT
023700030822     C                   EVAL      MARLOGA  = VIDLOGA
023800030822     C                   EVAL      MARLOGL  = VIDLOGL
023900030822     C                   EVAL      MARLOGQ  = VIDLOGQ
024000030822     C                   EVAL      MARSLOG  = VIDSLOG
024100040929     C                   EVAL      MARLOGOVR = VIDLOGOVR
024200040614     C                   EVAL      MAROFT   = VIDOFT
024300040614     C                   EVAL      MARHTML  = VIDHTML
024400040722     C                   EVAL      MAREMLSAV= VIDEMLSAV
024500050812     C                   EVAL      MAREMLRCV= VIDEMLRCV
024600050816     C                   EVAL      MAREMLFMT= VIDEMLFMT
024700030822     C                   UPDATE    TIMAR000
024800030509     C                   MOVEL     '1'           F03L00                         *USCITA PGM
024900030509     C                   EVAL      PMSG = MSG(2)
025000030509     C                   ENDIF
025100030509     C*
025200030509     C                   ENDSR
025300040217     C*------------------------------------------------------------------------*
025400040217     C* COPIA - OPERAZIONI DI COPIA RECORD
025500040217     C*------------------------------------------------------------------------*
025600040217     C     COPIA         BEGSR
025700040217     C*
025800040217     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
025900040217     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026000040217     C                   CLEAR                   TIMAR000
026100040217     C                   EVAL      MARCODR  = VIDCODR
026200040217     C                   EVAL      MARCODD  = VIDCODD
026300040217     C                   EVAL      MARTOUT  = VIDTOUT
026400040217     C                   EVAL      MARPOUT  = VIDPOUT
026500040217     C                   EVAL      MARFOUT  = VIDFOUT
026600040217     C                   EVAL      MARTFIL  = VIDTFIL
026700040217     C                   EVAL      MARTMOD  = VIDTMOD
026800040217     C                   EVAL      MAROPAG  = VIDOPAG
026900040217     C                   EVAL      MARCPI   = VIDCPI
027000040217     C                   EVAL      MARDCHR  = VIDDCHR
027100040217     C                   EVAL      MARINTL  = VIDINTL
027200040930     C                   EVAL      MARMARSX = VIDMARSX
027300040930     C                   EVAL      MARUNIM  = VIDUNIM
027400040217     C                   EVAL      MARFONT  = VIDFONT
027500040217     C                   EVAL      MARTEML  = VIDTEML
027600040217     C                   EVAL      MARTITP  = VIDTITP
027700040217     C                   EVAL      MAROBJP  = VIDOBJP
027800040217     C                   EVAL      MARKEYP  = VIDKEYP
027900040217     C                   EVAL      MARFLOG  = VIDFLOG
028000040217     C                   EVAL      MARLOGO  = VIDLOGO
028100040217     C                   EVAL      MARLOGS  = VIDLOGS
028200040217     C                   EVAL      MARLOGT  = VIDLOGT
028300040217     C                   EVAL      MARLOGA  = VIDLOGA
028400040217     C                   EVAL      MARLOGL  = VIDLOGL
028500040217     C                   EVAL      MARLOGQ  = VIDLOGQ
028600040217     C                   EVAL      MARSLOG  = VIDSLOG
028700040929     C                   EVAL      MARLOGOVR = VIDLOGOVR
028800040614     C                   EVAL      MAROFT   = VIDOFT
028900040614     C                   EVAL      MARHTML  = VIDHTML
029000040722     C                   EVAL      MAREMLSAV= VIDEMLSAV
029100050812     C                   EVAL      MAREMLRCV= VIDEMLRCV
029200050816     C                   EVAL      MAREMLFMT= VIDEMLFMT
029300040217     C                   WRITE     TIMAR000
029400040217     C                   EXSR      AZZVID
029500040217     C                   EVAL      PMSG = MSG(11)
029600040217     C                   MOVEL     '1'           F03L00                         *USCITA PGM
029700040217     C                   ENDIF
029800040217     C*
029900040217     C                   ENDSR
030000030822     C*------------------------------------------------------------------------*
030100030822     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
030200030822     C*------------------------------------------------------------------------*
030300030822     C     INSERI        BEGSR
030400030822     C*
030500030822     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030600030822     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030700030822     C                   CLEAR                   TIMAR000
030800030822     C                   EVAL      MARCODR  = VIDCODR
030900030822     C                   EVAL      MARCODD  = VIDCODD
031000030822     C                   EVAL      MARTOUT  = VIDTOUT
031100030822     C                   EVAL      MARPOUT  = VIDPOUT
031200030822     C                   EVAL      MARFOUT  = VIDFOUT
031300030822     C                   EVAL      MARTFIL  = VIDTFIL
031400030822     C                   EVAL      MARTMOD  = VIDTMOD
031500030822     C                   EVAL      MAROPAG  = VIDOPAG
031600030822     C                   EVAL      MARCPI   = VIDCPI
031700030822     C                   EVAL      MARDCHR  = VIDDCHR
031800030822     C                   EVAL      MARINTL  = VIDINTL
031900040930     C                   EVAL      MARMARSX = VIDMARSX
032000040930     C                   EVAL      MARUNIM  = VIDUNIM
032100030822     C                   EVAL      MARFONT  = VIDFONT
032200030822     C                   EVAL      MARTEML  = VIDTEML
032300030822     C                   EVAL      MARTITP  = VIDTITP
032400030822     C                   EVAL      MAROBJP  = VIDOBJP
032500030822     C                   EVAL      MARKEYP  = VIDKEYP
032600030822     C                   EVAL      MARFLOG  = VIDFLOG
032700030822     C                   EVAL      MARLOGO  = VIDLOGO
032800030822     C                   EVAL      MARLOGS  = VIDLOGS
032900030822     C                   EVAL      MARLOGT  = VIDLOGT
033000030822     C                   EVAL      MARLOGA  = VIDLOGA
033100030822     C                   EVAL      MARLOGL  = VIDLOGL
033200030822     C                   EVAL      MARLOGQ  = VIDLOGQ
033300030822     C                   EVAL      MARSLOG  = VIDSLOG
033400040929     C                   EVAL      MARLOGOVR = VIDLOGOVR
033500040614     C                   EVAL      MAROFT   = VIDOFT
033600040614     C                   EVAL      MARHTML  = VIDHTML
033700040722     C                   EVAL      MAREMLSAV= VIDEMLSAV
033800050812     C                   EVAL      MAREMLRCV= VIDEMLRCV
033900050816     C                   EVAL      MAREMLFMT= VIDEMLFMT
034000030822     C                   WRITE     TIMAR000
034100030822     C                   EXSR      AZZVID
034200030822     C                   EVAL      PMSG = MSG(4)
034300030822     C                   ENDIF
034400030822     C*
034500030822     C                   ENDSR
034600030822     C*------------------------------------------------------------------------*
034700040614     C* DECODI - OPERAZIONI DI CONTROLLO INSERIMENTI A VIDEO
034800030822     C*------------------------------------------------------------------------*
034900030822     C     DECODI        BEGSR
035000030822     C*
035100030822     C* INIZIALIZZA IL FLAG DI ERRORE
035200030822     C                   EVAL      ERRL00 = '0'
035300030822     C*
035400030822     C* INIZIALIZZA GLI INDICATORI DI ERRORE
035500030822     C                   SETOFF                                       505152
035600030822     C                   SETOFF                                       535455
035700030822     C                   SETOFF                                       565758
035800030822     C                   SETOFF                                       596061
035900030822     C                   SETOFF                                       626364
036000030822     C                   SETOFF                                       656667
036100030822     C                   SETOFF                                       686970
036200040614     C                   SETOFF                                       717273
036300040726     C                   SETOFF                                       437444
036400040930     C                   SETOFF                                       757677
036500050816     C                   SETOFF                                       7879
036600030822     C*
036700030822     C* CONTROLLO CAMPI CORRELATI AL "TIPO OUTPUT"
036800040722     C***                IF        ERRL00 = '0'
036900040722     C***                IF        VIDPOUT <> *blanks AND
037000040722     C***                          VIDTOUT <> 'F'
037100040722     C***                SETON                                        5051
037200040722     C***                EVAL      ERRL00 = '1'
037300040722     C***                EVAL      DSMSMS = MSG(5)
037400040722     C***                ENDIF
037500040722     C***                ENDIF
037600040722     C***
037700030822     C                   IF        ERRL00 = '0'
037800040217     C                   IF        VIDFOUT <> *blanks AND
037900040217     C                             VIDTOUT <> 'F'     AND
038000050812     C                             VIDTOUT <> 'A'
038100030822     C                   SETON                                        5052
038200030822     C                   EVAL      ERRL00 = '1'
038300050812     C                   EVAL      DSMSMS = MSG(5)
038400030822     C                   ENDIF
038500030822     C                   ENDIF
038600030822     C*
038700030822     C                   IF        ERRL00 = '0'
038800040701     C                   IF        VIDTEML <> *blanks AND VIDTOUT = 'F'
038900030822     C                   SETON                                        5060
039000030822     C                   EVAL      ERRL00 = '1'
039100040701     C                   EVAL      DSMSMS = MSG(13)
039200030822     C                   ENDIF
039300030822     C                   ENDIF
039400030822     C*
039500030822     C* CONTROLLO CAMPI CORRELATI AL "TIPO FILE"
039600030822     C                   IF        ERRL00 = '0'
039700060721     C                   IF        VIDTMOD <> *blanks AND VIDTFIL <> 'P' AND
039800060721     C                             VIDTFIL <> '1'     AND VIDTFIL <> '2'
039900030822     C                   SETON                                        5354
040000030822     C                   EVAL      ERRL00 = '1'
040100030822     C                   EVAL      DSMSMS = MSG(6)
040200030822     C                   ENDIF
040300030822     C                   ENDIF
040400030822     C*
040500030822     C                   IF        ERRL00 = '0'
040600060721     C                   IF        VIDOPAG <> *blanks AND VIDTFIL <> 'P' AND
040700060721     C                             VIDTFIL <> '1'     AND VIDTFIL <> '2'
040800030822     C                   SETON                                        5355
040900030822     C                   EVAL      ERRL00 = '1'
041000030822     C                   EVAL      DSMSMS = MSG(6)
041100030822     C                   ENDIF
041200030822     C                   ENDIF
041300030822     C*
041400030822     C                   IF        ERRL00 = '0'
041500060721     C                   IF        VIDCPI  > *zeros AND VIDTFIL <> 'P'
041600030822     C                   SETON                                        5356
041700030822     C                   EVAL      ERRL00 = '1'
041800030822     C                   EVAL      DSMSMS = MSG(6)
041900030822     C                   ENDIF
042000030822     C                   ENDIF
042100030822     C*
042200051108     C***                IF        ERRL00 = '0'
042300051108     C***                IF        VIDDCHR > *zeros AND VIDTFIL <> 'P'
042400051108     C***                SETON                                        5357
042500051108     C***                EVAL      ERRL00 = '1'
042600051108     C***                EVAL      DSMSMS = MSG(6)
042700051108     C***                ENDIF
042800051108     C***                ENDIF
042900030822     C*
043000030822     C                   IF        ERRL00 = '0'
043100030822     C                   IF        VIDINTL > *zeros AND VIDTFIL <> 'P'
043200030822     C                   SETON                                        5358
043300030822     C                   EVAL      ERRL00 = '1'
043400030822     C                   EVAL      DSMSMS = MSG(6)
043500030822     C                   ENDIF
043600030822     C                   ENDIF
043700040930     C*
043800040930     C                   IF        ERRL00 = '0'
043900040930     C                   IF        VIDMARSX > *zeros AND VIDTFIL <> 'P'
044000040930     C                   SETON                                        5376
044100040930     C                   EVAL      ERRL00 = '1'
044200040930     C                   EVAL      DSMSMS = MSG(6)
044300040930     C                   ENDIF
044400040930     C                   ENDIF
044500040930     C*
044600040930     C                   IF        ERRL00 = '0'
044700040930     C                   IF        VIDUNIM > *zeros AND VIDTFIL <> 'P'
044800040930     C                   SETON                                        5377
044900040930     C                   EVAL      ERRL00 = '1'
045000040930     C                   EVAL      DSMSMS = MSG(6)
045100040930     C                   ENDIF
045200040930     C                   ENDIF
045300030822     C*
045400030822     C                   IF        ERRL00 = '0'
045500030822     C                   IF        VIDFONT <> *blanks AND VIDTFIL <> 'P'
045600030822     C                   SETON                                        5359
045700030822     C                   EVAL      ERRL00 = '1'
045800030822     C                   EVAL      DSMSMS = MSG(6)
045900030822     C                   ENDIF
046000030822     C                   ENDIF
046100030822     C*
046200030822     C                   IF        ERRL00 = '0'
046300060721     C                   IF        VIDTITP <> *blanks AND VIDTFIL <> 'P' AND
046400060721     C                             VIDTFIL <> '1'     AND VIDTFIL <> '2'
046500030822     C                   SETON                                        5361
046600030822     C                   EVAL      ERRL00 = '1'
046700030822     C                   EVAL      DSMSMS = MSG(6)
046800030822     C                   ENDIF
046900030822     C                   ENDIF
047000030822     C*
047100030822     C                   IF        ERRL00 = '0'
047200060721     C                   IF        VIDOBJP <> *blanks AND VIDTFIL <> 'P' AND
047300060721     C                             VIDTFIL <> '1'     AND VIDTFIL <> '2'
047400030822     C                   SETON                                        5362
047500030822     C                   EVAL      ERRL00 = '1'
047600030822     C                   EVAL      DSMSMS = MSG(6)
047700030822     C                   ENDIF
047800030822     C                   ENDIF
047900030822     C*
048000030822     C                   IF        ERRL00 = '0'
048100060721     C                   IF        VIDKEYP <> *blanks AND VIDTFIL <> 'P' AND
048200060721     C                             VIDTFIL <> '1'     AND VIDTFIL <> '2'
048300030822     C                   SETON                                        5363
048400030822     C                   EVAL      ERRL00 = '1'
048500030822     C                   EVAL      DSMSMS = MSG(6)
048600030822     C                   ENDIF
048700030822     C                   ENDIF
048800030822     C*
048900030822     C                   IF        ERRL00 = '0'
049000040614     C                   IF        VIDFLOG = 'S' AND VIDTFIL <> 'P'
049100040726     C                   SETON                                        536444
049200030822     C                   EVAL      ERRL00 = '1'
049300030822     C                   EVAL      DSMSMS = MSG(6)
049400030822     C                   ENDIF
049500030822     C                   ENDIF
049600030822     C*
049700030822     C                   IF        ERRL00 = '0'
049800040726     C                   IF        VIDLOGO <> *blanks AND VIDFLOG <> 'S'
049900040726     C                   SETON                                        6465
050000030822     C                   EVAL      ERRL00 = '1'
050100040726     C                   EVAL      DSMSMS = MSG(19)
050200030822     C                   ENDIF
050300030822     C                   ENDIF
050400030822     C*
050500030822     C                   IF        ERRL00 = '0'
050600040726     C                   IF        VIDLOGS  > *zeros  AND VIDFLOG <> 'S'
050700040726     C                   SETON                                        6466
050800030822     C                   EVAL      ERRL00 = '1'
050900040726     C                   EVAL      DSMSMS = MSG(19)
051000030822     C                   ENDIF
051100030822     C                   ENDIF
051200030822     C*
051300030822     C                   IF        ERRL00 = '0'
051400040726     C                   IF        VIDLOGT  > *zeros  AND VIDFLOG <> 'S'
051500040726     C                   SETON                                        6467
051600030822     C                   EVAL      ERRL00 = '1'
051700040726     C                   EVAL      DSMSMS = MSG(19)
051800030822     C                   ENDIF
051900030822     C                   ENDIF
052000030822     C*
052100030822     C                   IF        ERRL00 = '0'
052200040726     C                   IF        VIDLOGA  > *zeros  AND VIDFLOG <> 'S'
052300040726     C                   SETON                                        6468
052400030822     C                   EVAL      ERRL00 = '1'
052500040726     C                   EVAL      DSMSMS = MSG(19)
052600030822     C                   ENDIF
052700030822     C                   ENDIF
052800030822     C*
052900030822     C                   IF        ERRL00 = '0'
053000040726     C                   IF        VIDLOGL  > *zeros  AND VIDFLOG <> 'S'
053100040726     C                   SETON                                        6469
053200030822     C                   EVAL      ERRL00 = '1'
053300040726     C                   EVAL      DSMSMS = MSG(19)
053400030822     C                   ENDIF
053500030822     C                   ENDIF
053600030822     C*
053700030822     C                   IF        ERRL00 = '0'
053800040726     C                   IF        VIDLOGQ  > *zeros  AND VIDFLOG <> 'S'
053900040726     C                   SETON                                        6470
054000030822     C                   EVAL      ERRL00 = '1'
054100040726     C                   EVAL      DSMSMS = MSG(19)
054200030822     C                   ENDIF
054300030822     C                   ENDIF
054400030822     C*
054500030822     C                   IF        ERRL00 = '0'
054600040726     C                   IF        VIDSLOG = 'S' AND VIDFLOG <> 'S'
054700040726     C                   SETON                                        6471
054800030822     C                   EVAL      ERRL00 = '1'
054900040726     C                   EVAL      DSMSMS = MSG(19)
055000030822     C                   ENDIF
055100030822     C                   ENDIF
055200040726     C                   IF        ERRL00 = '0'
055300040726     C                   IF        VIDSLOG = *blanks AND VIDFLOG = 'S'
055400040726     C                   SETON                                        6471
055500040726     C                   EVAL      ERRL00 = '1'
055600040726     C                   EVAL      DSMSMS = MSG(22)
055700040726     C                   ENDIF
055800040726     C                   ENDIF
055900040726     C*
056000040929     C                   IF        ERRL00 = '0'
056100040929     C                   IF        VIDLOGOVR = 'S' AND VIDFLOG <> 'S'
056200040929     C                   SETON                                        6475
056300040929     C                   EVAL      ERRL00 = '1'
056400040929     C                   EVAL      DSMSMS = MSG(19)
056500040929     C                   ENDIF
056600040929     C                   ENDIF
056700040929     C                   IF        ERRL00 = '0'
056800040929     C                   IF        VIDLOGOVR = *blanks AND VIDFLOG = 'S'
056900040929     C                   SETON                                        6475
057000040929     C                   EVAL      ERRL00 = '1'
057100040929     C                   EVAL      DSMSMS = MSG(22)
057200040929     C                   ENDIF
057300040929     C                   ENDIF
057400040929     C*
057500040726     C                   IF        ERRL00 = '0'
057600040726     C                   IF        VIDFLOG = 'S'
057700040726     C                   IF        VIDLOGQ >= 1 AND VIDLOGQ <= 100
057800040726     C                   ELSE
057900040726     C                   SETON                                        6470
058000040726     C                   EVAL      ERRL00 = '1'
058100040726     C                   EVAL      DSMSMS = MSG(20)
058200040726     C                   ENDIF
058300040726     C                   ENDIF
058400040726     C                   ENDIF
058500040726     C*
058600040726     C                   IF        ERRL00 = '0'
058700040726     C                   IF        VIDFLOG = 'S'
058800040726     C                   IF        VIDLOGA >= 1
058900040726     C                   ELSE
059000040726     C                   SETON                                        6468
059100040726     C                   EVAL      ERRL00 = '1'
059200040726     C                   EVAL      DSMSMS = MSG(21)
059300040726     C                   ENDIF
059400040726     C                   ENDIF
059500040726     C                   ENDIF
059600040726     C*
059700040726     C                   IF        ERRL00 = '0'
059800040726     C                   IF        VIDFLOG = 'S'
059900040726     C                   IF        VIDLOGL >= 1
060000040726     C                   ELSE
060100040726     C                   SETON                                        6469
060200040726     C                   EVAL      ERRL00 = '1'
060300040726     C                   EVAL      DSMSMS = MSG(21)
060400040726     C                   ENDIF
060500040726     C                   ENDIF
060600040726     C                   ENDIF
060700030822     C*
060800030822     C* OBBLIGATORIETA' CAMPI
060900030822     C                   IF        ERRL00 = '0'
061000030822     C                   IF        VIDTOUT = *blanks
061100030822     C                   SETON                                        50
061200030822     C                   EVAL      ERRL00 = '1'
061300030822     C                   EVAL      DSMSMS = MSG(8)
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C*
061700040615     C***                IF        ERRL00 = '0'
061800040615     C***                IF        VIDTOUT = 'F' AND VIDPOUT = *blanks
061900040615     C***                SETON                                        51
062000040615     C***                EVAL      ERRL00 = '1'
062100040615     C***                EVAL      DSMSMS = MSG(9)
062200040615     C***                ENDIF
062300040615     C***                ENDIF
062400030822     C*
062500030822     C                   IF        ERRL00 = '0'
062600030822     C                   IF        VIDTOUT = 'F' AND VIDFOUT = *blanks
062700030822     C                   SETON                                        52
062800030822     C                   EVAL      ERRL00 = '1'
062900030822     C                   EVAL      DSMSMS = MSG(9)
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C*
063300030822     C                   IF        ERRL00 = '0'
063400050812     C                   IF        VIDTFIL = *blanks AND VIDTOUT <> 'E'
063500050812     C                                               AND VIDTOUT <> 'T'
063600040614     C                   SETON                                        505343
063700030822     C                   EVAL      ERRL00 = '1'
063800040614     C                   EVAL      DSMSMS = MSG(15)
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100040614     C*
064200040614     C                   IF        ERRL00 = '0'
064300050812     C                   IF        VIDTFIL <> *blanks AND (VIDTOUT = 'E' OR
064400050812     C                                                     VIDTOUT = 'T')
064500040614     C                   SETON                                        505343
064600040614     C                   EVAL      ERRL00 = '1'
064700040614     C                   EVAL      DSMSMS = MSG(12)
064800040614     C                   ENDIF
064900040614     C                   ENDIF
065000060706     C*
065100060706     C                   IF        ERRL00 = '0'
065200060706     C                   IF        (VIDTFIL  = '1'  or VIDTFIL  = '2') AND
065300060706     C                             (VIDTOUT <> 'A' and VIDTOUT <> 'F')
065400060706     C                   SETON                                        505343
065500060706     C                   EVAL      ERRL00 = '1'
065600060706     C                   EVAL      DSMSMS = MSG(12)
065700060706     C                   ENDIF
065800060706     C                   ENDIF
065900040614     C*
066000040614     C                   IF        ERRL00 = '0'
066100040614     C                   IF        VIDOFT  <> *blanks AND VIDTOUT = 'F'
066200040614     C                   SETON                                        5072
066300040614     C                   EVAL      ERRL00 = '1'
066400040614     C                   EVAL      DSMSMS = MSG(13)
066500040614     C                   ENDIF
066600040614     C                   ENDIF
066700050812     C*
066800050812     C                   IF        ERRL00 = '0'
066900050812     C                   IF        VIDOFT  = *blanks AND VIDTOUT = 'T'
067000050812     C                   SETON                                        5072
067100050812     C                   EVAL      ERRL00 = '1'
067200050812     C                   EVAL      DSMSMS = MSG(23)
067300050812     C                   ENDIF
067400050812     C                   ENDIF
067500040614     C* CONTROLLO SE INDICATO IL MODELLO CHE ABBIA ANCHE L'ESTENSIONE GIUSTA
067600040614     C                   IF        ERRL00 = '0'
067700040614     C                   IF        VIDOFT  <> *blanks
067800040701     C                   IF        %scan('.OFT':VIDOFT) = *zeros AND
067900040701     C                             %scan('.oft':VIDOFT) = *zeros
068000040614     C                   EVAL      VIDOFT = %trim(VIDOFT) + '.OFT'
068100040614     C                   ENDIF
068200040614     C                   ENDIF
068300040614     C                   ENDIF
068400040722     C*
068500050812     C                   IF        ERRL00 = '0'
068600040722     C                   IF        VIDEMLSAV <> *blanks AND VIDTOUT = 'F'
068700040722     C                   SETON                                        5074
068800040722     C                   EVAL      ERRL00 = '1'
068900040722     C                   EVAL      DSMSMS = MSG(13)
069000040722     C                   ENDIF
069100040722     C                   ENDIF
069200040722     C                   IF        ERRL00 = '0'
069300040722     C                   IF        VIDEMLSAV = *blanks AND
069400050812     C                             (VIDTOUT='E' OR VIDTOUT='A' OR VIDTOUT='T')
069500040722     C                   SETON                                        5074
069600040722     C                   EVAL      ERRL00 = '1'
069700040722     C                   EVAL      DSMSMS = MSG(18)
069800040722     C                   ENDIF
069900040722     C                   ENDIF
070000050812     C*
070100050812     C                   IF        ERRL00 = '0'
070200050812     C                   IF        VIDEMLRCV <> *blanks AND VIDTOUT = 'F'
070300050812     C                   SETON                                        5078
070400050812     C                   EVAL      ERRL00 = '1'
070500050812     C                   EVAL      DSMSMS = MSG(13)
070600050812     C                   ENDIF
070700050812     C                   ENDIF
070800050812     C                   IF        ERRL00 = '0'
070900050812     C                   IF        VIDEMLRCV = *blanks AND
071000050812     C                             (VIDTOUT='E' OR VIDTOUT='A' OR VIDTOUT='T')
071100050812     C                   SETON                                        5078
071200050812     C                   EVAL      ERRL00 = '1'
071300050812     C                   EVAL      DSMSMS = MSG(18)
071400050812     C                   ENDIF
071500050812     C                   ENDIF
071600050816     C*
071700050816     C                   IF        ERRL00 = '0'
071800050816     C                   IF        VIDEMLFMT <> *blanks AND VIDTOUT = 'F'
071900050816     C                   SETON                                        5079
072000050816     C                   EVAL      ERRL00 = '1'
072100050816     C                   EVAL      DSMSMS = MSG(13)
072200050816     C                   ENDIF
072300050816     C                   ENDIF
072400050816     C                   IF        ERRL00 = '0'
072500050816     C                   IF        VIDEMLFMT = *blanks AND
072600050816     C                             (VIDTOUT='E' OR VIDTOUT='A' OR VIDTOUT='T')
072700050816     C                   SETON                                        5079
072800050816     C                   EVAL      ERRL00 = '1'
072900050816     C                   EVAL      DSMSMS = MSG(18)
073000050816     C                   ENDIF
073100050816     C                   ENDIF
073200050816     C                   IF        ERRL00 = '0'
073300050816     C                   IF        VIDEMLFMT <> 'H' AND VIDTOUT = 'T'
073400050816     C                   SETON                                        5079
073500050816     C                   EVAL      ERRL00 = '1'
073600050816     C                   EVAL      DSMSMS = MSG(24)
073700050816     C                   ENDIF
073800050816     C                   ENDIF
073900040614     C*
074000040614     C                   IF        ERRL00 = '0'
074100040614     C                   IF        VIDHTML <> *blanks AND VIDTOUT <> 'E'
074200040614     C                   SETON                                        5073
074300040614     C                   EVAL      ERRL00 = '1'
074400040615     C                   EVAL      DSMSMS = MSG(14)
074500040614     C                   ENDIF
074600040614     C                   ENDIF
074700030822     C*
074800030822     C                   IF        ERRL00 = '0'
074900060721     C                   IF        (VIDTFIL='P' OR VIDTFIL='1' OR VIDTFIL='2')
075000060721     C                             AND VIDTMOD = *blanks
075100030822     C                   SETON                                        54
075200030822     C                   EVAL      ERRL00 = '1'
075300030822     C                   EVAL      DSMSMS = MSG(10)
075400030822     C                   ENDIF
075500030822     C                   ENDIF
075600030822     C*
075700030822     C                   IF        ERRL00 = '0'
075800060721     C                   IF        (VIDTFIL='P' OR VIDTFIL='1' OR VIDTFIL='2')
075900060721     C                             AND VIDOPAG = *blanks
076000030822     C                   SETON                                        55
076100030822     C                   EVAL      ERRL00 = '1'
076200030822     C                   EVAL      DSMSMS = MSG(10)
076300030822     C                   ENDIF
076400030822     C                   ENDIF
076500030822     C*
076600041023     C*                  IF        ERRL00 = '0'
076700041023     C*                  IF        VIDTFIL = 'P' AND VIDCPI  = *zeros
076800041023     C*                  SETON                                        56
076900041023     C*                  EVAL      ERRL00 = '1'
077000041023     C*                  EVAL      DSMSMS = MSG(10)
077100041023     C*                  ENDIF
077200041023     C*                  ENDIF
077300030822     C*
077400040929     C*                  IF        ERRL00 = '0'
077500040929     C*                  IF        VIDTFIL = 'P' AND VIDDCHR = *zeros
077600040929     C*                  SETON                                        57
077700040929     C*                  EVAL      ERRL00 = '1'
077800040929     C*                  EVAL      DSMSMS = MSG(10)
077900040929     C*                  ENDIF
078000040929     C*                  ENDIF
078100030822     C*
078200040929     C*                  IF        ERRL00 = '0'
078300040929     C*                  IF        VIDTFIL = 'P' AND VIDINTL = *zeros
078400040929     C*                  SETON                                        58
078500040929     C*                  EVAL      ERRL00 = '1'
078600040929     C*                  EVAL      DSMSMS = MSG(10)
078700040929     C*                  ENDIF
078800040929     C*                  ENDIF
078900030822     C*
079000030822     C                   IF        ERRL00 = '0'
079100030822     C                   IF        VIDTFIL = 'P' AND VIDFONT = *blanks
079200030822     C                   SETON                                        59
079300030822     C                   EVAL      ERRL00 = '1'
079400030822     C                   EVAL      DSMSMS = MSG(10)
079500030822     C                   ENDIF
079600030822     C                   ENDIF
079700040930     C*
079800040930     C*                  IF        ERRL00 = '0'
079900040930     C*                  IF        VIDTFIL = 'P' AND VIDMARSX = *zeros
080000040930     C*                  SETON                                        76
080100040930     C*                  EVAL      ERRL00 = '1'
080200040930     C*                  EVAL      DSMSMS = MSG(10)
080300040930     C*                  ENDIF
080400040930     C*                  ENDIF
080500040930     C*
080600040930     C                   IF        ERRL00 = '0'
080700040930     C                   IF        VIDTFIL = 'P' AND VIDUNIM = *blanks
080800040930     C                   SETON                                        77
080900040930     C                   EVAL      ERRL00 = '1'
081000040930     C                   EVAL      DSMSMS = MSG(10)
081100040930     C                   ENDIF
081200040930     C                   ENDIF
081300040615     C*
081400040615     C* CONTROLLO SE INDICATO SI LOGO CHE IL RANGE QUALITA' LOGO SIA DA 0 A 100
081500040615     C                   IF        ERRL00 = '0'
081600040615     C                   IF        VIDFLOG = 'S'
081700040615     C                   IF        VIDLOGQ < 1 OR VIDLOGQ > 100
081800040615     C                   SETON                                        70
081900040615     C                   EVAL      ERRL00 = '1'
082000040615     C                   EVAL      DSMSMS = MSG(16)
082100040615     C                   ENDIF
082200040615     C                   ENDIF
082300040615     C                   ENDIF
082400040615     C*
082500040615     C* CONTROLLI CORRETTEZZA FORMALE NOMI FILE E PERCORSI
082600040615     C                   IF        ERRL00 = '0'
082700040615     C                   IF        VIDPOUT <> *blanks
082800040615     C                   IF        %subst(%trim(VIDPOUT):1:1) = '\' OR
082900040615     C                             %subst(%trim(VIDPOUT):%len(%trim(VIDPOUT)):1)
083000040615     C                             = '\'
083100040615     C                   SETON                                        51
083200040615     C                   EVAL      ERRL00 = '1'
083300040615     C                   EVAL      DSMSMS = %trim(MSG(17))
083400040615     C                   ENDIF
083500040615     C                   ENDIF
083600040615     C                   ENDIF
083700040615     C*
083800040615     C                   IF        ERRL00 = '0'
083900040615     C                   IF        VIDLOGO <> *blanks
084000040615     C                   IF        %subst(%trim(VIDLOGO):1:1) = '\' OR
084100040615     C                             %subst(%trim(VIDLOGO):%len(%trim(VIDLOGO)):1)
084200040615     C                             = '\'
084300040615     C                   SETON                                        65
084400040615     C                   EVAL      ERRL00 = '1'
084500040615     C                   EVAL      DSMSMS = %trim(MSG(17))
084600040615     C                   ENDIF
084700040615     C                   ENDIF
084800040615     C                   ENDIF
084900040615     C*
085000040615     C                   IF        ERRL00 = '0'
085100040615     C                   IF        VIDFOUT <> *blanks
085200040615     C                   Z-ADD     1             SkJ               2 0
085300040615     C                   MOVEL     *blanks       SkChar            1
085400040615     C                   DOW       SkJ <= %len(SkNomiNoOK)
085500040615     C                   EVAL      SkChar = %subst(SkNomiNoOK:SkJ:1)
085600040615     C                   IF        %scan(SkChar:VIDFOUT) > 0
085700040615     C                   SETON                                        52
085800040615     C                   EVAL      ERRL00 = '1'
085900040615     C                   EVAL      DSMSMS = %trim(MSG(16)) + ': ' + SkChar
086000040615     C                   LEAVE
086100040615     C                   ENDIF
086200040615     C                   ADD       1             SkJ
086300040615     C                   ENDDO
086400040615     C                   ENDIF
086500040615     C                   ENDIF
086600040615     C*
086700040615     C                   IF        ERRL00 = '0'
086800040615     C                   IF        VIDOFT <> *blanks
086900040615     C                   Z-ADD     1             SkJ
087000040615     C                   MOVEL     *blanks       SkChar
087100040615     C                   DOW       SkJ <= %len(SkNomiNoOK)
087200040615     C                   EVAL      SkChar = %subst(SkNomiNoOK:SkJ:1)
087300040615     C                   IF        %scan(SkChar:VIDOFT) > 0
087400040615     C                   SETON                                        72
087500040615     C                   EVAL      ERRL00 = '1'
087600040615     C                   EVAL      DSMSMS = %trim(MSG(16)) + ': ' + SkChar
087700040615     C                   LEAVE
087800040615     C                   ENDIF
087900040615     C                   ADD       1             SkJ
088000040615     C                   ENDDO
088100040615     C                   ENDIF
088200040615     C                   ENDIF
088300040615     C*
088400040615     C                   IF        ERRL00 = '0'
088500040615     C                   IF        VIDTITP <> *blanks
088600040615     C                   Z-ADD     1             SkJ
088700040615     C                   MOVEL     *blanks       SkChar
088800040615     C                   DOW       SkJ <= %len(SkNomiNoOK)
088900040615     C                   EVAL      SkChar = %subst(SkNomiNoOK:SkJ:1)
089000040615     C                   IF        %scan(SkChar:VIDTITP) > 0
089100040615     C                   SETON                                        61
089200040615     C                   EVAL      ERRL00 = '1'
089300040615     C                   EVAL      DSMSMS = %trim(MSG(16)) + ': ' + SkChar
089400040615     C                   LEAVE
089500040615     C                   ENDIF
089600040615     C                   ADD       1             SkJ
089700040615     C                   ENDDO
089800040615     C                   ENDIF
089900040615     C                   ENDIF
090000040615     C*
090100040615     C                   IF        ERRL00 = '0'
090200040615     C                   IF        VIDOBJP <> *blanks
090300040615     C                   Z-ADD     1             SkJ
090400040615     C                   MOVEL     *blanks       SkChar
090500040615     C                   DOW       SkJ <= %len(SkNomiNoOK)
090600040615     C                   EVAL      SkChar = %subst(SkNomiNoOK:SkJ:1)
090700040615     C                   IF        %scan(SkChar:VIDOBJP) > 0
090800040615     C                   SETON                                        62
090900040615     C                   EVAL      ERRL00 = '1'
091000040615     C                   EVAL      DSMSMS = %trim(MSG(16)) + ': ' + SkChar
091100040615     C                   LEAVE
091200040615     C                   ENDIF
091300040615     C                   ADD       1             SkJ
091400040615     C                   ENDDO
091500040615     C                   ENDIF
091600040615     C                   ENDIF
091700040615     C*
091800040615     C                   IF        ERRL00 = '0'
091900040615     C                   IF        VIDKEYP <> *blanks
092000040615     C                   Z-ADD     1             SkJ
092100040615     C                   MOVEL     *blanks       SkChar
092200040615     C                   DOW       SkJ <= %len(SkNomiNoOK)
092300040615     C                   EVAL      SkChar = %subst(SkNomiNoOK:SkJ:1)
092400040615     C                   IF        %scan(SkChar:VIDKEYP) > 0
092500040615     C                   SETON                                        63
092600040615     C                   EVAL      ERRL00 = '1'
092700040615     C                   EVAL      DSMSMS = %trim(MSG(16)) + ': ' + SkChar
092800040615     C                   LEAVE
092900040615     C                   ENDIF
093000040615     C                   ADD       1             SkJ
093100040615     C                   ENDDO
093200040615     C                   ENDIF
093300040615     C                   ENDIF
093400040615     C*
093500040615     C                   IF        ERRL00 = '0'
093600040615     C                   IF        VIDLOGO <> *blanks
093700040615     C                   Z-ADD     1             SkJ
093800040615     C                   MOVEL     *blanks       SkChar
093900040726     C                   DOW       SkJ <= %len(SkPathNoOK)
094000040726     C                   EVAL      SkChar = %subst(SkPathNoOK:SkJ:1)
094100040615     C                   IF        %scan(SkChar:VIDLOGO) > 0
094200040615     C                   SETON                                        65
094300040615     C                   EVAL      ERRL00 = '1'
094400040615     C                   EVAL      DSMSMS = %trim(MSG(16)) + ': ' + SkChar
094500040615     C                   LEAVE
094600040615     C                   ENDIF
094700040615     C                   ADD       1             SkJ
094800040615     C                   ENDDO
094900040615     C                   ENDIF
095000040615     C                   ENDIF
095100030822     C*
095200030822     C                   ENDSR
095300030822     C*------------------------------------------------------------------------*
095400030822     C* AZZVID - IMPOSTA I VALORI DI DEFAULT A VIDEO
095500030822     C*------------------------------------------------------------------------*
095600030822     C     AZZVID        BEGSR
095700030822     C*
095800030822     C                   CLEAR                   TCM6D03
095900030822     C*
096000040218     C                   EVAL      VIDTOUT  = 'A'
096100030822     C                   EVAL      VIDTFIL  = 'P'
096200030822     C                   EVAL      VIDTMOD  = 'A4'
096300030822     C                   EVAL      VIDOPAG  = 'O'
096400030822     C                   EVAL      VIDCPI   = 15
096500030822     C                   EVAL      VIDDCHR  = 6
096600030822     C                   EVAL      VIDINTL  = 2
096700030822     C                   EVAL      VIDFONT  = 'Courier New'
096800040218     C                   EVAL      VIDTITP  = '*PROG*'
096900040218     C                   EVAL      VIDKEYP  = '*PROG*'
097000030822     C                   EVAL      VIDFLOG  = 'N'
097100040930     C                   EVAL      VIDUNIM  = 'mm'
097200040930     C                   EVAL      VIDMARSX = 2
097300030822     C*
097400030822     C                   ENDSR
097500021219     C*------------------------------------------------------------------------*
097600030509     C* LETREC - LEGGE IL RECORD E LO CARICA A VIDEO
097700021219     C*------------------------------------------------------------------------*
097800030509     C     LETREC        BEGSR
097900021219     C*
098000030822     C     KEYMAR        CHAIN     TIMAR01L                           99
098100030822     C                   IF        %found(TIMAR01L)
098200030822     C                   EVAL      VIDCODR  = MARCODR
098300030822     C                   EVAL      VIDCODD  = MARCODD
098400030822     C                   EVAL      VIDTOUT  = MARTOUT
098500030822     C                   EVAL      VIDPOUT  = MARPOUT
098600030822     C                   EVAL      VIDFOUT  = MARFOUT
098700030822     C                   EVAL      VIDTFIL  = MARTFIL
098800030822     C                   EVAL      VIDTMOD  = MARTMOD
098900030822     C                   EVAL      VIDOPAG  = MAROPAG
099000030822     C                   EVAL      VIDCPI   = MARCPI
099100030822     C                   EVAL      VIDDCHR  = MARDCHR
099200030822     C                   EVAL      VIDINTL  = MARINTL
099300040930     C                   EVAL      VIDMARSX = MARMARSX
099400040930     C                   EVAL      VIDUNIM  = MARUNIM
099500030822     C                   EVAL      VIDFONT  = MARFONT
099600030822     C                   EVAL      VIDTEML  = MARTEML
099700030822     C                   EVAL      VIDTITP  = MARTITP
099800030822     C                   EVAL      VIDOBJP  = MAROBJP
099900030822     C                   EVAL      VIDKEYP  = MARKEYP
100000030822     C                   EVAL      VIDFLOG  = MARFLOG
100100030822     C                   EVAL      VIDLOGO  = MARLOGO
100200030822     C                   EVAL      VIDLOGS  = MARLOGS
100300030822     C                   EVAL      VIDLOGT  = MARLOGT
100400030822     C                   EVAL      VIDLOGA  = MARLOGA
100500030822     C                   EVAL      VIDLOGL  = MARLOGL
100600030822     C                   EVAL      VIDLOGQ  = MARLOGQ
100700030822     C                   EVAL      VIDSLOG  = MARSLOG
100800040929     C                   EVAL      VIDLOGOVR = MARLOGOVR
100900040614     C                   EVAL      VIDOFT   = MAROFT
101000040614     C                   EVAL      VIDHTML  = MARHTML
101100040722     C                   EVAL      VIDEMLSAV= MAREMLSAV
101200050812     C                   EVAL      VIDEMLRCV= MAREMLRCV
101300050816     C                   EVAL      VIDEMLFMT= MAREMLFMT
101400030509     C                   ELSE
101500030509     C                   MOVEL     '1'           ERRL00                         *ERRORE
101600030509     C                   MOVEL     '1'           F03L00                         *USCITA PGM
101700030509     C                   EVAL      PMSG = MSG(3)
101800030509     C                   ENDIF
101900021219     C*
102000021219     C                   ENDSR
102100970526     C*------------------------------------------------------------------------*
102200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
102300970526     C*------------------------------------------------------------------------*
102400970526     C     IMPFUN        BEGSR
102500970526     C*
102600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
102700021211     C     POP1          IFEQ      'O05'
102800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
102900970526     C                   MOVEL     'N'           FUDATT
103000970526     C                   MOVEL     FUDRIG        FUD(6)
103100021211     C                   ENDIF
103200970526     C*
103300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
103400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
103500970526     C                   EXSR      FUNGES
103600970526     C*
103700000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
103800021219     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
103900030509     C                   EXSR      LETREC
104000980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
104100000407     C                   ELSE                                                   *INSERIMENTO
104200030822     C                   EXSR      AZZVID
104300030822     C                   ENDIF                                                  *INIZIALIZZA VIDEATA
104400970526     C*
104500030509     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
104600021211     C     POP1          IFEQ      'O02'
104700021211     C                   MOVEL     DEC(1)        VIDFUN
104800021211     C                   ENDIF
104900040217     C     POP1          IFEQ      'O03'
105000040217     C                   MOVEL     DEC(4)        VIDFUN
105100040217     C                   ENDIF
105200021211     C     POP1          IFEQ      'O05'
105300030509     C                   MOVEL     DEC(2)        VIDFUN
105400021211     C                   ENDIF
105500030822     C     POP1          IFEQ      'F06'
105600030822     C                   MOVEL     DEC(3)        VIDFUN
105700030822     C                   ENDIF
105800021211     C*
105900021211     C                   ENDSR
106000021219     C*--------------------------------------------------------------------------------------------*
106100021219     C* REPERISCE I DATI UTENTE
106200021219     C*--------------------------------------------------------------------------------------------*
106300021219     C     REPDATIUTE    BEGSR
106400021219     C*
106500021219     C* INIZIALIZZA VARIABILI DI WRK
106600021219     C                   CLEAR                   TIBS34DS
106700021219     C                   CLEAR                   AZUTEDS
106800021219     C                   CLEAR                   DDATIUTE
106900021219     C*
107000021219     C     *DTAARA       DEFINE    §azute        azuteds
107100021219     C     *DTAARA       DEFINE    §datiute      ddatiute
107200021219     C                   IN(E)     *DTAARA
107300021219     C                   IF        %Error
107400021220     C                   EVAL      I34Tla = 'L'
107500021219     C                   CALL      'TIBS34R'
107600021219     C                   PARM                    Tibs34Ds
107700021219     C                   IN        *DTAARA
107800021219     C                   ENDIF
107900021219     C*
108000021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
108100021219     C                   MOVEL(P)  RSUT          DSFIRS           20
108200021219     C*
108300021219     C                   ENDSR
108400970526     C*------------------------------------------------------------------------*
108500970526     C* *INZSR - OPERAZIONI INIZIALI
108600970526     C*------------------------------------------------------------------------*
108700970526     C     *INZSR        BEGSR
108800970526     C*---
108900970526     C* RICEVIMENTO PARAMETRI
109000970526     C*---
109100970526     C     *ENTRY        PLIST
109200970612     C                   PARM                    KPJBA
109300000407     C                   MOVEL     KPJBU         PARAM
109400021219     C*
109500021219     C* REPERISCE I DATI DELL'UTENTE
109600021219     C                   EXSR      REPDATIUTE
109700000407     C*
109800000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
109900000407     C     POP1          IFEQ      'O05'
110000021230     C                   SETON                                        90
110100000407     C                   ENDIF
110200030509     C     POP1          IFEQ      'O02'
110300030509     C                   SETOFF                                       90
110400030822     C                   SETON                                        91
110500030509     C                   ENDIF
110600040217     C     POP1          IFEQ      'O03'
110700030822     C                   SETOFF                                       90
110800030822     C                   SETOFF                                       91
110900030822     C                   ENDIF
111000040217     C     POP1          IFEQ      'F06'
111100040217     C                   SETOFF                                       90
111200040217     C                   SETOFF                                       91
111300040217     C                   ENDIF
111400021219     C*---
111500021219     C* CHIAVI DI LETTURA
111600021219     C*---
111700030822     C* LETTURA TIMAR01L - COMPLETA
111800030822     C     KEYMAR        KLIST
111900030822     C                   KFLD                    PCODR                          *CODICE REGOLA
112000970526     C*
112100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
112200970526     C     XMSGPA        PLIST
112300970526     C                   PARM                    DSMSMS
112400970526     C                   PARM                    DSMSPG
112500970526     C                   PARM                    DSMSKE
112600970526     C                   PARM                    DSMSER
112700970526     C*
112800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
112900970526     C                   CALL      'XKEY'
113000970526     C                   PARM                    DSKEY
113100970526     C*---
113200970526     C* VARIABILI NON RIFERITE AL DATA BASE
113300970526     C*---
113400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
113500970526     C*---
113600970526     C* VARIABILI DI CONTROLLO
113700970526     C*---
113800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
113900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
114000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
114100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
114200970526     C*
114300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
114400100219     C                   MOVEL     'TRBMM6R'     DSMSPG
114500100219     C                   MOVEL     'TRBMM6R'     MSGQUE
114600970526     C                   MOVEL     *BLANKS       MSGKEY
114700970526     C                   MOVEL     *BLANKS       DSMSMS
114800970526     C*
114900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
115000970526     C                   EXSR      IMPFUN
115100970526     C*
115200970526     C                   ENDSR
115300970526     C*------------------------------------------------------------------------*
115400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
115500970526     C*------------------------------------------------------------------------*
115600970526     C     FUNGES        BEGSR
115700970526     C*
115800970526     C* PULISCO CAMPI DI LAVORO
115900970526     C                   Z-ADD     0             W                 5 0
116000970526     C*
116100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
116200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
116300970526     C     1             DO        24            I
116400970526     C                   MOVEL     FUD(I)        FUDRIG
116500970526     C*
116600970526     C* PULIZIA FUNZIONI PRECEDENTI
116700970526     C                   MOVEL     *BLANKS       FUV(I)
116800970526     C*
116900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
117000970526     C* (SALTA F24)
117100970526     C     FUDATT        IFEQ      'S'
117200970526     C     FUDFUN        ANDNE     '24'
117300970526     C                   ADD       1             W
117400970526     C                   MOVEL     FUDDES        FUV(W)
117500970526     C                   END
117600970526     C                   END                                                    DO WHILE
117700970526     C*
117800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
117900970526     C* ALTRIMENTI DISATTIVO F24
118000970526     C     1             DO        24            I
118100970526     C                   MOVEL     FUD(I)        FUDRIG
118200970526     C*
118300970526     C     FUDFUN        IFEQ      '24'
118400970526     C     W             IFGT      6
118500970526     C                   MOVEL     'S'           FUDATT
118600970526     C                   ELSE
118700970526     C                   MOVEL     *BLANKS       FUDATT
118800970526     C                   END                                                    W GT 8
118900970526     C                   MOVEL     FUDRIG        FUD(I)
119000970526     C                   END
119100970526     C                   END                                                    DO WHILE
119200970526     C*
119300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
119400970526     C     W             DIV       6             I
119500970526     C                   MVR                     Z
119600970526     C*
119700970526     C     Z             IFNE      0
119800970526     C     I             ADD       1             Z
119900970526     C                   ELSE
120000970526     C                   Z-ADD     I             Z                 5 0
120100970526     C                   END
120200970526     C*
120300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
120400970526     C                   ADD       1             FUNGRU
120500970526     C     FUNGRU        IFGT      Z
120600970526     C     FUNGRU        ORLT      1
120700970526     C                   Z-ADD     1             FUNGRU
120800970526     C                   END
120900970526     C*
121000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
121100970526     C     FUNGRU        IFEQ      1
121200970526     C                   MOVEL     FUV(1)        FUN001
121300970526     C                   MOVEL     FUV(2)        FUN002
121400970526     C                   MOVEL     FUV(3)        FUN003
121500970526     C                   MOVEL     FUV(4)        FUN004
121600970526     C                   MOVEL     FUV(5)        FUN005
121700970526     C                   MOVEL     FUV(6)        FUN006
121800970526     C                   END
121900970526     C*
122000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
122100970526     C     FUNGRU        IFEQ      2
122200970526     C                   MOVEL     FUV(7)        FUN001
122300970526     C                   MOVEL     FUV(8)        FUN002
122400970526     C                   MOVEL     FUV(9)        FUN003
122500970526     C                   MOVEL     FUV(10)       FUN004
122600970526     C                   MOVEL     FUV(11)       FUN005
122700970526     C                   MOVEL     FUV(12)       FUN006
122800970526     C                   END
122900970526     C*
123000970526     C* TERZO GRUPPO DI 6 FUNZIONI
123100970526     C     FUNGRU        IFEQ      3
123200970526     C                   MOVEL     FUV(13)       FUN001
123300970526     C                   MOVEL     FUV(14)       FUN002
123400970526     C                   MOVEL     FUV(15)       FUN003
123500970526     C                   MOVEL     FUV(16)       FUN004
123600970526     C                   MOVEL     FUV(17)       FUN005
123700970526     C                   MOVEL     FUV(18)       FUN006
123800970526     C                   END
123900970526     C*
124000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
124100970526     C     FUNGRU        IFEQ      4
124200970526     C                   MOVEL     FUV(19)       FUN001
124300970526     C                   MOVEL     FUV(20)       FUN002
124400970526     C                   MOVEL     FUV(21)       FUN003
124500970526     C                   MOVEL     FUV(22)       FUN004
124600970526     C                   MOVEL     *BLANKS       FUN005
124700970526     C                   MOVEL     *BLANKS       FUN006
124800970526     C                   END
124900970526     C*
125000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
125100970526     C                   MOVEL     *BLANKS       FUN007
125200970526     C                   MOVEL     *BLANKS       FUN008
125300970526     C*
125400970526     C     1             DO        24            I
125500970526     C                   MOVEL     FUD(I)        FUDRIG
125600970526     C     FUDFUN        IFEQ      '24'
125700970526     C     FUDATT        ANDEQ     'S'
125800970526     C                   MOVEL     FUDDES        FUN008
125900970526     C                   END
126000970526     C                   END                                                    DO
126100970526     C*
126200970526     C                   ENDSR
126300970526     C*------------------------------------------------------------------------*
126400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
126500970526     C*------------------------------------------------------------------------*
126600970526     C     FUNCON        BEGSR
126700970526     C*
126800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
126900970526     C                   EXSR      FUNVAR
127000970526     C*
127100970526     C* PULISCO CAMPI DI LAVORO
127200970526     C                   Z-ADD     0             I                 5 0
127300970526     C                   Z-ADD     0             W                 5 0
127400970526     C*
127500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
127600970526     C     FUNKEY        IFNE      *BLANKS
127700970526     C     1             DO        24            I
127800970526     C                   MOVEL     FUD(I)        FUDRIG
127900970526     C*
128000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
128100970526     C     FUNKEY        IFEQ      FUDFUN
128200970526     C     FUDATT        ANDEQ     'S'
128300970526     C                   Z-ADD     I             W
128400970526     C                   END
128500970526     C                   END                                                    DO WHILE
128600970526     C*
128700970526     C* FUNZIONE NON ATTIVA (ERRORE)
128800970526     C     W             IFEQ      0
128900970526     C*
129000970526     C* EMISSIONE MESSAGGIO DI ERRORE
129100970526     C                   MOVEL     MSG(1)        DSMSMS
129200970526     C*
129300970526     C                   END                                                    W=0
129400970526     C                   END                                                    FUNKEY NE *BLANKS
129500970526     C*
129600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
129700970526     C     W             IFNE      0
129800970526     C     FUNKEY        OREQ      *BLANKS
129900970526     C     FUNKEY        IFEQ      *BLANKS
130000970526     C                   EXSR      ENTGES
130100970526     C                   END
130200970526     C     FUNKEY        IFEQ      '03'
130300970526     C                   EXSR      F03GES
130400970526     C                   END
130500970526     C     FUNKEY        IFEQ      '06'
130600970526     C                   EXSR      F06GES
130700970526     C                   END
130800970526     C     FUNKEY        IFEQ      '12'
130900970526     C                   EXSR      F12GES
131000970526     C                   END
131100970526     C     FUNKEY        IFEQ      '24'
131200970526     C                   EXSR      F24GES
131300970526     C                   END
131400970526     C                   END
131500970526     C*
131600970526     C                   ENDSR
131700970526     C*------------------------------------------------------------------------*
131800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
131900970526     C*------------------------------------------------------------------------*
132000970526     C     FUNVAR        BEGSR
132100970526     C*
132200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
132300970526     C*
132400030509     C* FUNKEY=*BLANKS TCMNIFICA ENTER
132500970526     C     KEY           IFEQ      ENTER
132600970526     C                   MOVEL     *BLANKS       FUNKEY
132700970526     C                   END
132800970526     C     KEY           IFEQ      F1
132900970526     C                   MOVEL     '01'          FUNKEY
133000970526     C                   END
133100970526     C     KEY           IFEQ      F2
133200970526     C                   MOVEL     '02'          FUNKEY
133300970526     C                   END
133400970526     C     KEY           IFEQ      F3
133500970526     C                   MOVEL     '03'          FUNKEY
133600970526     C                   END
133700970526     C     KEY           IFEQ      F4
133800970526     C                   MOVEL     '04'          FUNKEY
133900970526     C                   END
134000970526     C     KEY           IFEQ      F5
134100970526     C                   MOVEL     '05'          FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F6
134400970526     C                   MOVEL     '06'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F7
134700970526     C                   MOVEL     '07'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F8
135000970526     C                   MOVEL     '08'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F9
135300970526     C                   MOVEL     '09'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      F10
135600970526     C                   MOVEL     '10'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      F11
135900970526     C                   MOVEL     '11'          FUNKEY
136000970526     C                   END
136100970526     C     KEY           IFEQ      F12
136200970526     C                   MOVEL     '12'          FUNKEY
136300970526     C                   END
136400970526     C     KEY           IFEQ      F13
136500970526     C                   MOVEL     '13'          FUNKEY
136600970526     C                   END
136700970526     C     KEY           IFEQ      F14
136800970526     C                   MOVEL     '14'          FUNKEY
136900970526     C                   END
137000970526     C     KEY           IFEQ      F15
137100970526     C                   MOVEL     '15'          FUNKEY
137200970526     C                   END
137300970526     C     KEY           IFEQ      F16
137400970526     C                   MOVEL     '16'          FUNKEY
137500970526     C                   END
137600970526     C     KEY           IFEQ      F17
137700970526     C                   MOVEL     '17'          FUNKEY
137800970526     C                   END
137900970526     C     KEY           IFEQ      F18
138000970526     C                   MOVEL     '18'          FUNKEY
138100970526     C                   END
138200970526     C     KEY           IFEQ      F19
138300970526     C                   MOVEL     '19'          FUNKEY
138400970526     C                   END
138500970526     C     KEY           IFEQ      F20
138600970526     C                   MOVEL     '20'          FUNKEY
138700970526     C                   END
138800970526     C     KEY           IFEQ      F21
138900970526     C                   MOVEL     '21'          FUNKEY
139000970526     C                   END
139100970526     C     KEY           IFEQ      F22
139200970526     C                   MOVEL     '22'          FUNKEY
139300970526     C                   END
139400970526     C     KEY           IFEQ      F23
139500970526     C                   MOVEL     '23'          FUNKEY
139600970526     C                   END
139700970526     C     KEY           IFEQ      F24
139800970526     C                   MOVEL     '24'          FUNKEY
139900970526     C                   END
140000970526     C     KEY           IFEQ      ROLLUP
140100970526     C                   MOVEL     'RU'          FUNKEY
140200970526     C                   END
140300970526     C     KEY           IFEQ      HELP
140400970526     C                   MOVEL     'HP'          FUNKEY
140500970526     C                   END
140600970526     C                   ENDSR
140700970526     C*------------------------------------------------------------------------*
140800970526     C* NOTE COSTRUZIONE SCHIERE
140900970526     C*------------------------------------------------------------------------*
141000970526     C*
141100970526     C* -MSG-MESSAGGI
141200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
141300970526     C* (NON TOCCARE MSG,1 E MSG,2)
141400970526     C*
141500970526     C* -CMD-COMANDI
141600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
141700970526     C*
141800970526     C* -OPD-OPZIONI
141900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
142000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
142100970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
142200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
142300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
142400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
142500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
142600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
142700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
142800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
142900970526     C*
143000970526     C* -FUD-FUNZIONI
143100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
143200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
143300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
143400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
143500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
143600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
143700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
143800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
143900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
144000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
144100970526     O*------------------------------------------------------------------------*
144200970526** DEC - DECODIFICHE
144300970526    MODIFICA                                                      1
144400030509VISUALIZZAZIONE                                                   2
144500030822   INSERIMENTO                                                    3
144600040217     COPIA                                                        4
1447000305095
144800970528** MSG - MESSAGGI
144900100219TRBMM6R- Tasto funzionale non gestito                                          1
145000100219TRBMM6R- Modifica effettuata                                                   2
145100100219TRBMM6R- Record non trovato                                                    3
145200100219TRBMM6R- Inserimento effettuato                                                4
145300100219TRBMM6R- Valori consentiti solo se "Tipo output" = 'F' o 'A'                   5
145400100219TRBMM6R- Valori consentiti solo se "Tipo file" = 'P'                           6
145500100219TRBMM6R- Valori consentiti solo se "Tipo output" = 'A'                         7
145600100219TRBMM6R- Campo obbligatorio                                                    8
145700100219TRBMM6R- Campo obbligatorio se "Tipo output" = 'F'                             9
145800100219TRBMM6R- Campo obbligatorio se "Tipo file" = 'P'                               10
145900100219TRBMM6R- Copia effettuata                                                      11
146000100219TRBMM6R- Valori consentiti solo se "Tipo output" = 'A' o 'F'                   12
146100100219TRBMM6R- Valori consentiti solo se "Tipo output" = 'A' o 'E' o 'T'             13
146200100219TRBMM6R- Valori consentiti solo se "Tipo output" = 'E'                         14
146300100219TRBMM6R- Campo obbligatorio se "Tipo output" = 'A' o 'F'                       15
146400100219TRBMM6R- Valori non validi nel campo                                           16
146500100219TRBMM6R- Il campo non può iniziare o finire col carattere '\'                  17
146600100219TRBMM6R- Campo obbligatorio se "Tipo output" = 'E' o 'A' o 'T'                 18
146700100219TRBMM6R- Valori consentiti solo se "Logo" = 'S'                                19
146800100219TRBMM6R- Valori consentiti per il campo: da 1 a 100 (%)                        20
146900100219TRBMM6R- Se "Logo" = 'S' il campo deve essere > 0                              21
147000100219TRBMM6R- Campo obbligatorio se "Logo" = 'S'                                    22
147100100219TRBMM6R- Campo obbligatorio se "Tipo output" = 'T'                             23
147200100219TRBMM6R- Obbligatorio valorizzazione campo a 'H' se "Tipo output" = 'T'        24
147300970526** FUD - FUNZIONI
147400970526                   -        (S/N)=N                        funz.01¬
147500970526                   -        (S/N)=N                        funz.02¬
147600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
147700970526                   -        (S/N)=N                        funz.04¬
147800970526                   -        (S/N)=N                        funz.05¬
147900000407F06=Conferma       -        (S/N)=S                        funz.06¬
148000970527                   -        (S/N)=N                        funz.07¬
148100970527                   -        (S/N)=N                        funz.08¬
148200970527                   -        (S/N)=N                        funz.09¬
148300970527                   -        (S/N)=N                        funz.10¬
148400970526                   -        (S/N)=N                        funz.11¬
148500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
148600970526                   -        (S/N)=N                        funz.13¬
148700970526                   -        (S/N)=N                        funz.14¬
148800970526                   -        (S/N)=N                        funz.15¬
148900970526                   -        (S/N)=N                        funz.16¬
149000970526                   -        (S/N)=N                        funz.17¬
149100970526                   -        (S/N)=N                        funz.18¬
149200970526                   -        (S/N)=N                        funz.19¬
149300970526                   -        (S/N)=N                        funz.20¬
149400970526                   -        (S/N)=N                        funz.21¬
149500970526                   -        (S/N)=N                        funz.22¬
149600970526                   -        (S/N)=N                        funz.23¬
149700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
