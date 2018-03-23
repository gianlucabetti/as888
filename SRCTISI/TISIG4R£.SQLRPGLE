000100970526     H*------------------------------------------------------------------------*
000200021219     H* Gestione movimenti GABBIE e BANCALI (AZMGB00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800021219     FTISIG4D   CF   E             WORKSTN INFDS(DEVDS1)
000900021219     F                                     SFILE(SIG4DMS:MSGULT)
001000160727     Fazmgb03l  uF   E           K DISK
001100021219     Fazorg01l  IF   E           K DISK
001200021219     Fcnaco00f  IF   E           K DISK
001300021219     Fazdst04l  IF   E           K DISK
001400021219     Fazagb01l  IF   E           K DISK
001500150204     Fazcgb01l  IF   E           K DISK
001600021219     Ftntbe01l  IF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700150311     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
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
006100021219     D*------------
006200970526     D* ARCHITETTURA
006300970526     D*------------
006400970526     D KPJBA         E DS
006500021219     D*------------
006600021219     D* DS LETTURA NUOVA TABELLA GGB
006700021219     D*------------
006800021219     D DGGB          E DS
006900021219     D DGGB_C        E DS                  EXTNAME(DGGB) PREFIX(W_)
007000021219     D*------------
007100021219     D* DS DI PASSAGGIO PARAMETRI X "DRIVER" MOVIMENTI GABBIE/BANCALI
007200021219     D*------------
007300021219     D TISIG0DS      E DS
007400021220     D*------------
007500021220     D* TABELLE "XCLIR"- RICERCA CLIENTI
007600021220     D*------------
007700021220     D KSC             S              7  0 dim(30)
007800021220     D KSA             S              4    dim(30)
007900021220     D*------------------
008000021220     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
008100021220     D*------------------
008200021220     D DSXC          E DS                  extname(XCLIRDS)
008300021220     D*------------------
008400021220     D                 DS
008500021220     D DSKSN                   1      4p 0
008600021220     D DSKSA                   1      4
008700021220     D*------------------
008800021220     D* DS REPERIMENTO DATI UTENTE
008900021220     D*-------------------
009000021220     D BS69DS        E DS                  EXTNAME(TIBS69DS)
009100021220     D ACODS         E DS                  EXTNAME(CNACO00F)
009200021220     D INDDS         E DS                  EXTNAME(CNIND00F)
009300021220     D CLPDS         E DS                  EXTNAME(CNCLP00F)
009400021220     D CLSDS         E DS                  EXTNAME(FNCLS00F)
009500000407     D*------------------
009600000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
009700000407     D*------------------
009800000407     D WLBDA8          DS                  INZ
009900000407     D  G08DAT                 1      8  0
010000000407     D  G08INV                 9     16  0
010100000407     D  G08ERR                17     17
010200000407     D  G08TGI                18     22  0
010300021219     D*------------------
010400021219     D* PASSAGGIO PARAMETRI
010500021219     D*------------------
010600030122     D PARAM           DS                  INZ
010700030122     D  POP1                   1      3
010800030122     D  PPRG                   4     18  0
010900030122     D  PMSG                  19     98
011000030123     D PARAM2          DS                  INZ
011100030123     D  P2OP1                  1      3
011200030123     D  P2CAU                  4      7
011300030123     D  P2DES                  8     42
011400030123     D  P2MSG                 43    122
011500021219     D*-------------------
011600021219     D* DS REPERIMENTO DATI UTENTE
011700021219     D*-------------------
011800021219     D TIBS34DS      E DS                                                       *Profili utente
011900021219     D DDATIUTE      E DS                                                       *Dati utente
012000021219     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012100150311     D dute01        E DS                                                       *Utenti
012200150311     d dLat          e ds
012300150311     d tibs02ds      e ds
012400030702     D*------------------
012500030702     D* DS CARICAMENTO P.O. GESTITI
012600030702     D*-------------------
012700030702     D TRUL31DS      E DS                  INZ
012800150311     D TRUL31DS2     E DS                  INZ
012900030702     D*-------------------
013000030702     D* FILIALI DEI CLIENTI DA VISUALIZZARE
013100030702     D*-------------------
013200030702     D L1              S              3    DIM(250) INZ(*zeros)
013300150311     D $DIG            s              1    dim(20)
013400150311     D $ARG            s              3    dim(50)
013500070509
013600070509      * flag per forzare messaggio errore sul cliente
013700070509     d wforza          s              1    inz(*off)
013800150216     d qtatole         s              7  0
013900150408     d qtafranc        s              9  0
014000070509
014100970526     C*------------------------------------------------------------------------*
014200970526     C* MAIN LINES
014300970526     C*------------------------------------------------------------------------*
014400970526     C*
014500970526     C* NON CI SONO ERRORI IN ENTRATA
014600021211     C     ERRL00        IFEQ      '0'
014700970526     C*
014800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014900021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015200970526     C*
015300970526     C* EMETTE FORMATO VIDEO
015400970526     C                   EXSR      BUTFMR
015500970526     C*
015600970526     C* GESTIONE TASTI FUNZIONALI
015700970526     C                   EXSR      FUNCON
015800021211     C                   ENDDO
015900970526     C*
016000021211     C                   ENDIF
016100021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
016200021211     C                   MOVEL     PARAM         KPJBU
016300970526     C* FINE PGM
016400970526     C                   SETON                                        LR
016500970526     C*------------------------------------------------------------------------*
016600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016700970526     C*------------------------------------------------------------------------*
016800970526     C     BUTFMR        BEGSR
016900970526     C*
017000970526     C* EMETTE LA TESTATA
017100021219     C                   WRITE     SIG4D01
017200970526     C*
017300970526     C* SPEDISCE EVENTUALE MESSAGGIO
017400970526     C                   EXSR      SNDMSG
017500970526     C*
017600970526     C* EMETTE LE FUNZIONI
017700021219     C                   WRITE     SIG4D02
017800970526     C* EMETTE IL VIDEO
017900021219     C                   EXFMT     SIG4D03
018000970526     C*
018100970526     C* REIMPOSTA FLAG DI ERRORE
018200970526     C                   MOVEL     '0'           ERRL00
018300970526     C*
018400970526     C                   ENDSR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
018700970526     C*------------------------------------------------------------------------*
018800970526     C     SNDMSG        BEGSR
018900970526     C*
019000970526     C* PULISCE IL SFILE MESSAGGI
019100970526     C                   MOVEL     '1'           *IN40
019200021219     C                   WRITE     SIG4DMC
019300970526     C                   MOVEL     '0'           *IN40
019400970526     C*
019500970526     C* VISUALIZZA IL MESSAGGIO
019600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
019700970526     C                   CALL      'XMSG'        XMSGPA
019800021211     C     DSMSER        IFEQ      *ZEROS
019900970526     C                   MOVEL     DSMSKE        MSGKEY
020000021211     C                   ENDIF
020100970526     C                   Z-ADD     1             MSGULT
020200021219     C                   WRITE     SIG4DMS
020300021219     C                   WRITE     SIG4DMC
020400970526     C*
020500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
020600970526     C                   MOVEL     *BLANKS       DSMSMS
020700970526     C*
020800970526     C                   ENDSR
020900970526     C*------------------------------------------------------------------------*
021000021219     C* ENTGES - GESTIONE ENTER -> INVIO
021100970526     C*------------------------------------------------------------------------*
021200970526     C     ENTGES        BEGSR
021300970526     C*
021400970526     C* CONTROLLA I CAMPI IMMESSI
021500970526     C                   EXSR      DECODI
021600970526     C*
021700970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
021800021211     C     POP1          IFEQ      'O05'
021900021211     C                   MOVEL     *BLANKS       PMSG
022000970526     C                   MOVEL     '1'           FINL00
022100021211     C                   ENDIF
022200970526     C*
022300970526     C                   ENDSR
022400970526     C*------------------------------------------------------------------------*
022500970526     C* F03GES - GESTIONE F03 -> FINE
022600970526     C*------------------------------------------------------------------------*
022700970526     C     F03GES        BEGSR
022800970526     C*
022900970526     C* IMPOSTA "PREMUTO F03"
023000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400021219     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
023500970526     C*------------------------------------------------------------------------*
023600970526     C     F06GES        BEGSR
023700970526     C*
023800021211     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
023900021211     C     POP1          IFEQ      'F06'
024000021211     C                   EXSR      INSERI
024100021211     C                   ENDIF
024200021211     C*
024300970526     C                   ENDSR
024400160727     C*------------------------------------------------------------------------*
024500160727     C* F08GES - GESTIONE F08 -> cancellazione rekord
024600160727     C*------------------------------------------------------------------------*
024700160727     C     F08GES        BEGSR
024800160727     C*
024900160727     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
025000160727     C     POP1          IFEQ      'O04'
025100160727     C                   EXSR      cancella
025200160727     C                   ENDIF
025300160727     C*
025400160727     C                   ENDSR
025500970526     C*------------------------------------------------------------------------*
025600970526     C* F12GES - GESTIONE F12 -> RITORNO
025700970526     C*------------------------------------------------------------------------*
025800970526     C     F12GES        BEGSR
025900970526     C*
026000970526     C* IMPOSTA "PREMUTO F12"
026100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026200970526     C*
026300970526     C                   ENDSR
026400970526     C*------------------------------------------------------------------------*
026500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026600970526     C*------------------------------------------------------------------------*
026700970526     C     F24GES        BEGSR
026800970526     C*
026900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
027000970526     C*
027100970526     C                   ENDSR
027200970526     C*------------------------------------------------------------------------*
027300021219     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
027400970526     C*------------------------------------------------------------------------*
027500970526     C     INSERI        BEGSR
027600970526     C*
027700970526     C* CONTROLLA I CAMPI IMMESSI
027800970526     C                   EXSR      DECODI
027900970526     C*
028000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028100021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028200021211     C                   EXSR      IMPCAM
028300021230     C                   CALL      'TISIG5R'
028400150227     C                   PARM                    kpjba
028500021230     C                   PARM                    TISIG0DS
028600021230     C*
028700021230     C* SE TUTTO OK
028800021230     C                   IF        OG0ERR = *blanks
028900150216      *secondo giro forzato per inserimanto riga TOLE = pareggio o franchigia
029000150223     c                   if        cgbsco > *zeros and §ggbsco = 'S'
029100150408     c                   eval(h)   Qtafranc  = ((vidqt1 * 100) /(100-cgbsco))
029200150408     c                   eval(h)   QtaTOLE  = qtafranc - vidqt1
029300150408     c                   eval      vidqt1 = qtatole
029400150216     c                   eval      vidca1 = 'TOLE'
029500150330     c                   if        vidqt1 >= 1
029600150216     C                   EXSR      IMPCAM
029700150216     C                   CALL      'TISIG5R'
029800150227     C                   PARM                    kpjba
029900150216     C                   PARM                    TISIG0DS
030000150330     c                   endif
030100150216     c                   endif
030200150216      *
030300021230     C                   MOVEL     MSG(2)        PMSG                           *MESSAGGIO
030400030107     C                   MOVEL     MSG(2)        DSMSMS
030500021230     C                   SETON                                        9192
030600021230     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
030700021230     C                   EXSR      AZZVID
030800021230     C                   ELSE
030900021230     C                   MOVEL     OG0MSG        PMSG                           *MESSAGGIO
031000030107     C                   MOVEL     OG0MSG        DSMSMS
031100021230     C                   ENDIF
031200021211     C                   ENDIF
031300970526     C*
031400970526     C                   ENDSR
031500160727     C*------------------------------------------------------------------------*
031600160727     C* cancella OPERAZIONI DI cancellazione RECORD
031700160727     C*------------------------------------------------------------------------*
031800160727     C     cancella      BEGSR
031900160727     C*
032000160727     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
032100160727     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032200160727      *se record non collegato ad altro rekord provo a cancellare
032300160803     c                   if        §ggbsco = 'S' or vidca1 = 'TOLE'
032400160803     C                   MOVEL     MSG(22)       PMSG                           *MESSAGGIO
032500160803     C                   MOVEL     MSG(22)       DSMSMS
032600160727     C                   ELSE
032700160803     c     keymgb        chain     azmgb03l
032800160803     c                   if        %found(azmgb03l)
032900160803     c                   delete    azmgb000
033000160803      *
033100160803     C                   MOVEL     MSG(23)       PMSG                           *MESSAGGIO
033200160803     C                   MOVEL     MSG(23)       DSMSMS
033300160803     C                   SETON                                        9192
033400160803     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
033500160803     C                   EXSR      AZZVID
033600160727     C                   ENDIF
033700160727     C                   ENDIF
033800160727     C                   ENDIF
033900160727     C*
034000160727     C                   ENDSR
034100970526     C*------------------------------------------------------------------------*
034200021219     C* DECODI - CONTROLLA CAMPI E DECODIFICA
034300970526     C*------------------------------------------------------------------------*
034400970526     C     DECODI        BEGSR
034500000407     C*
034600000407     C                   MOVEL     '0'           ERRL00
034700000407     C*
034800000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
034900000407     C                   SETOFF                                       808182
035000021224     C                   SETOFF                                       838485
035100021219     C*--------------------
035200021219     C* SCELTA/CONTROLLO CAUSALE
035300021219     C*--------------------
035400021219     C     ERRL00        IFEQ      '0'
035500021223     C                   IF        VIDCA1 <> *blanks
035600021223     C                   EVAL      Chk_DES = *blanks
035700021220     C                   EVAL      Chk_CAU = VIDCA1
035800021220     C                   EXSR      REPCAU
035900021220     C                   EVAL      VIDCA1 = Chk_CAU
036000021220     C                   EVAL      VIDDE1 = Chk_DES
036100021224     C                   EVAL      VIHTI1 = §GGBTIP
036200021224     C                   EVAL      VIHSE1 = §GGBSEG
036300150330     C* Controllo che la causale sia ammessa per il profilo fra sede e filiale
036400150303     C                   IF        ERRL00 = '0' and not *in90
036500150223     c                   select
036600150223     C                   when      §GGBaut = 'S' AND dutlpo <> 'S'
036700150223     C                   EVAL      ERRL00 = '1'
036800150223     C                   SETON                                        809192
036900150223     C                   MOVEL     MSG(18)       DSMSMS
037000150223     C                   when      §GGBaut = 'F' AND dutlpo = 'S'
037100150223     C                   EVAL      ERRL00 = '1'
037200150223     C                   SETON                                        809192
037300150223     C                   MOVEL     MSG(18)       DSMSMS
037400150223     C                   Endsl
037500150223     C                   Endif
037600021224     C* Controllo che la causale movimento inserita dall'utente sia solo d tipo manuale
037700021230     C                   IF        ERRL00 = '0'
037800030109     C                   IF        §GGBTIM <> 'M' AND POP1 = 'F06'
037900021230     C                   EVAL      ERRL00 = '1'
038000021230     C                   SETON                                        809192
038100021230     C                   MOVEL     MSG(10)       DSMSMS
038200021223     C                   ELSE
038300021230     C                   SETOFF                                       91
038400021224     C* Se causale corretta => propongo già (qualora prevista) la cusale movimento correlato
038500021224     C                   IF        §GGBCAC <> *blanks
038600021224     C                   EVAL      Chk_DES = *blanks
038700021224     C                   EVAL      Chk_CAU = §GGBCAC
038800021224     C                   EXSR      REPCAU
038900021224     C                   EVAL      VIDCA2 = Chk_CAU
039000021224     C                   EVAL      VIDDE2 = Chk_DES
039100021224     C                   EVAL      VIHTI2 = §GGBTIP
039200021224     C                   EVAL      VIHSE2 = §GGBSEG
039300021224     C*
039400021223     C                   SETOFF                                       92
039500021230     C                   ELSE
039600021230     C                   SETON                                        92
039700021220     C                   ENDIF
039800021224     C                   ENDIF
039900150223     C*m                 ELSE
040000150223     C*m                 SETON                                        809192
040100150223     C*m                 MOVEL     MSG(3)        DSMSMS
040200021224     C                   ENDIF
040300030107     C*
040400030107     C                   IF        VIHTI1 = 'F'
040500030107     C                   SETOFF                                       55
040600030107     C                   ELSE
040700030107     C                   SETON                                        55
040800030107     C                   ENDIF
040900030107     C                   IF        VIHTI2 = 'F'
041000030107     C                   SETOFF                                       65
041100030107     C                   ELSE
041200030107     C                   SETON                                        65
041300030107     C                   ENDIF
041400030107     C                   ENDIF
041500030107     C                   ENDIF
041600021219     C*--------------------
041700021219     C* SCELTA/CONTROLLO SOGGETTO
041800021219     C*--------------------
041900021219     C     ERRL00        IFEQ      '0'
042000021223     C* Decodifico il tipo di soggetto movimento
042100021224     C                   IF        VIHTI1 = 'C'
042200021223     C                   EVAL      VIDDT1 = 'Cliente ................?:'
042300021223     C                   ENDIF
042400021224     C                   IF        VIHTI1 = 'D'
042500021223     C                   EVAL      VIDDT1 = 'Destinatario ...........?:'
042600021223     C                   ENDIF
042700021224     C                   IF        VIHTI1 = 'F'
042800021223     C                   EVAL      VIDDT1 = 'P.O. ...................?:'
042900021223     C                   ENDIF
043000021223     C* Eseguo i controlli di validità soggetto movimento
043100030107     C                   IF        *IN55   =  *ON
043200030107     C                   IF        VIDSO1K <> *blanks
043300030107     C                   EVAL      Chk_DES =  *blanks
043400030107     C                   MOVEL     *zeros        Chk_SOG
043500030107     C                   MOVE      VIDSO1K       Chk_SOG
043600021220     C*
043700021224     C                   IF        VIHTI1 = 'C'
043800021220     C                   EXSR      REPKSC
043900021220     C                   ENDIF
044000021220     C*
044100021224     C                   IF        VIHTI1 = 'D'
044200021220     C                   EXSR      REPDST
044300021220     C                   ENDIF
044400021220     C*
044500021224     C                   IF        VIHTI1 = 'F'
044600021220     C                   EXSR      REPPO
044700021220     C                   ENDIF
044800021220     C*
044900030107     C                   MOVE(P)   Chk_SOG       VIDSO1K
045000030107     C                   EVAL      VIDDS1  = Chk_DES
045100030107     C                   IF        ERRL00  = '1'
045200021220     C                   SETON                                        81
045300021220     C                   MOVEL     MSG(4)        DSMSMS
045400021220     C                   ENDIF
045500030107     C                   ENDIF
045600030107     C                   ELSE
045700030107     C                   IF        VIDSO1F <> *blanks
045800030107     C                   EVAL      Chk_DES = *blanks
045900030107     C                   MOVEL     *zeros        Chk_SOG
046000030107     C                   MOVE      VIDSO1F       Chk_SOG
046100030107     C*
046200030107     C                   IF        VIHTI1 = 'C'
046300030107     C                   EXSR      REPKSC
046400030107     C                   ENDIF
046500030107     C*
046600030107     C                   IF        VIHTI1 = 'D'
046700030107     C                   EXSR      REPDST
046800030107     C                   ENDIF
046900030107     C*
047000030107     C                   IF        VIHTI1 = 'F'
047100030107     C                   EXSR      REPPO
047200030107     C                   ENDIF
047300030107     C*
047400030107     C                   MOVE(P)   Chk_SOG       VIDSO1F
047500030107     C                   EVAL      VIDDS1  = Chk_DES
047600030107     C                   IF        ERRL00  = '1'
047700030107     C                   SETON                                        81
047800030107     C                   MOVEL     MSG(4)        DSMSMS
047900030107     C                   ENDIF
048000030107     C                   ENDIF
048100030107     C                   ENDIF
048200030107     C                   ENDIF
048300030702     C*--------------------
048400030702     C* CONTROLLO AUTORIZZAZIONI UTENTE SU CODICE SOGGETTO MOVIMENTO "PRINCIPALE"
048500030702     C*--------------------
048600030702     C     ERRL00        IFEQ      '0'
048700040702     C     POP1          IFNE      'O05'
048800030702     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
048900071127     C                   IF        VIHTI1 = 'F'
049000030702     C                   MOVEL     VIDSO1F       WN3               3
049100030702     C                   SETOFF                                           38
049200030702     C     WN3           LOOKUP    L1                                     38
049300030702     C     *IN38         IFEQ      *OFF
049400070509     c     wforza        andeq     *off
049500030702     C                   MOVEL     '1'           ERRL00
049600030702     C                   SETON                                        81
049700150311     C                   MOVEL     MSG(21)       DSMSMS
049800150311     C*m                 MOVEL     MSG(13)       DSMSMS
049900150311     c*m                 eval      wforza = *on
050000030702     C                   ENDIF
050100030702     C                   ENDIF
050200030702     C                   IF        VIHTI1 = 'C'
050300030702     C                   MOVEL     VIDSO1K       WA4               4
050400030702     C                   MOVE      WA4           WN3               3
050500030702     C                   SETOFF                                           38
050600030702     C     WN3           LOOKUP    L1                                     38
050700030702     C     *IN38         IFEQ      *OFF
050800070509     c     wforza        andeq     *off
050900150311     C                   MOVEL     '1'           ERRL00
051000030702     C                   SETON                                        81
051100150311     C                   MOVEL     MSG(21)       DSMSMS
051200150311     C*m                 MOVEL     MSG(13)       DSMSMS
051300150311     c*m                 eval      wforza = *on
051400030702     C                   ENDIF
051500030702     C                   ENDIF
051600040702     C                   ENDIF
051700030702     C                   ENDIF
051800021219     C*--------------------
051900021219     C* SCELTA/CONTROLLO ARTICOLO
052000021219     C*--------------------
052100021219     C     ERRL00        IFEQ      '0'
052200021223     C                   IF        VIDCO1 <> *blanks
052300021223     C                   EVAL      Chk_DES = *blanks
052400021220     C                   EVAL      Chk_COD = VIDCO1
052500021220     C                   EXSR      REPCOD
052600021219     C                   ENDIF
052700021223     C                   ENDIF
052800021224     C*--------------------
052900021224     C* QUANTITA' MOVIMENTATA
053000021224     C*--------------------
053100021224     C     ERRL00        IFEQ      '0'
053200021224     C                   IF        VIDQT1 = *ZEROS
053300021224     C                   ELSE
053400021224     C* A seconda del segno della causale gestisco a programma il segno della quantità
053500021224     C                   IF        VIHSE1 = 'E'
053600021230     C                   EVAL      VIDDQ1 = 'ENTRATA'
053700021230     C                   EVAL      VIDDQ2 = 'USCITA'
053800150223      *se movimento verso il cliente verifico se la quantità è in capienza
053900150223      * la quantità massima evadibile è data dal saldo dei movimenti - la % tolleranza(rotture)
054000150303     c*m                 if        vihti1 = 'C' and §ggbsco = 'S'
054100150305     c                   if        vihti1 = 'C' and not *in90
054200150305     c                   exsr      contrqta
054300150223     C                   endif
054400021224     C                   ENDIF
054500021224     C                   IF        VIHSE1 = 'U'
054600021230     C                   EVAL      VIDDQ1 = 'USCITA'
054700021230     C                   EVAL      VIDDQ2 = 'ENTRATA'
054800021224     C                   ENDIF
054900021230     C                   EVAL      VIDQT1 = %abs(VIDQT1)
055000021230     C                   EVAL      VIDQT2 = %abs(VIDQT1)
055100021224     C*
055200021224     C                   ENDIF
055300021224     C                   ENDIF
055400021219     C*--------------------
055500021220     C* CONTROLLO DATA RIFERIMENTO DOCUMENTO
055600021219     C*--------------------
055700021219     C     ERRL00        IFEQ      '0'
055800021223     C                   IF        VIDDD1 <> *zeros
055900021220     C     *LIKE         DEFINE    VIDDD1        DD1VID
056000021220     C                   Z-ADD     VIDDD1        WDA8
056100021220     C                   Z-ADD     *ZEROS        DA8W
056200021220     C                   EXSR      CHKDA8
056300021220     C                   Z-ADD     WDA8          VIDDD1
056400021220     C                   Z-ADD     DA8W          DD1VID
056500021220     C     ERRL00        IFEQ      '1'
056600021220     C                   SETON                                        85
056700021220     C                   MOVEL     MSG(9)        DSMSMS
056800021220     C                   ENDIF
056900021219     C                   ENDIF
057000021223     C                   ENDIF
057100021220     C*--------------------
057200021220     C* SCELTA/CONTROLLO SOGGETTO MOVIMENTO CORRELATO
057300021220     C*--------------------
057400021220     C     ERRL00        IFEQ      '0'
057500021223     C* Decodifico il tipo di soggetto movimento correlato
057600021224     C                   IF        VIHTI2 = 'C'
057700021223     C                   EVAL      VIDDT2 = 'Cliente ................?:'
057800021223     C                   ENDIF
057900021224     C                   IF        VIHTI2 = 'D'
058000021223     C                   EVAL      VIDDT2 = 'Destinatario ...........?:'
058100021223     C                   ENDIF
058200021224     C                   IF        VIHTI2 = 'F'
058300021223     C                   EVAL      VIDDT2 = 'P.O. ...................?:'
058400021223     C                   ENDIF
058500021223     C* Eseguo i controlli di validità soggetto movimento
058600030107     C                   IF        *IN65   =  *ON
058700030107     C                   IF        VIDSO2K <> *blanks
058800030107     C                   EVAL      Chk_DES =  *blanks
058900030107     C                   MOVEL     *zeros        Chk_SOG
059000030107     C                   MOVE      VIDSO2K       Chk_SOG
059100021220     C*
059200021224     C                   IF        VIHTI2 = 'C'
059300021220     C                   EXSR      REPKSC
059400021220     C                   ENDIF
059500021220     C*
059600021224     C                   IF        VIHTI2 = 'D'
059700021220     C                   EXSR      REPDST
059800021220     C                   ENDIF
059900021220     C*
060000021224     C                   IF        VIHTI2 = 'F'
060100021220     C                   EXSR      REPPO
060200021220     C                   ENDIF
060300021220     C*
060400030107     C                   MOVE(P)   Chk_SOG       VIDSO2K
060500021220     C                   EVAL      VIDDS2 = Chk_DES
060600021220     C                   IF        ERRL00 = '1'
060700021220     C                   SETON                                        84
060800021220     C                   MOVEL     MSG(4)        DSMSMS
060900021220     C                   ENDIF
061000021223     C                   ENDIF
061100030107     C                   ELSE
061200030107     C                   IF        VIDSO2F <> *blanks
061300030107     C                   EVAL      Chk_DES = *blanks
061400030107     C                   MOVEL     *zeros        Chk_SOG
061500030107     C                   MOVE      VIDSO2F       Chk_SOG
061600030107     C*
061700030107     C                   IF        VIHTI2 = 'C'
061800030107     C                   EXSR      REPKSC
061900030107     C                   ENDIF
062000030107     C*
062100030107     C                   IF        VIHTI2 = 'D'
062200030107     C                   EXSR      REPDST
062300030107     C                   ENDIF
062400030107     C*
062500030107     C                   IF        VIHTI2 = 'F'
062600030107     C                   EXSR      REPPO
062700030107     C                   ENDIF
062800030107     C*
062900030107     C                   MOVE(P)   Chk_SOG       VIDSO2F
063000030107     C                   EVAL      VIDDS2  = Chk_DES
063100030107     C                   IF        ERRL00  = '1'
063200030107     C                   SETON                                        84
063300030107     C                   MOVEL     MSG(4)        DSMSMS
063400030107     C                   ENDIF
063500030107     C                   ENDIF
063600030107     C                   ENDIF
063700030107     C                   ENDIF
063800150226     C                   IF        ERRL00 = '0' and pop1 = 'F06' and
063900150226     C                             VIHTI2 = 'C'
064000150226     c                   movel     vidco1        wtsc
064100150225     c     kcgb          setgt     azcgb01l
064200150225     c     kcgb          readpe    azcgb01l
064300150224     c                   if        %eof(azcgb01l)
064400150204     c                   move      '1'           errl00
064500150204     c                   seton                                        8182
064600150204     C                   MOVEL     MSG(14)       DSMSMS
064700150204     c                   endif
064800150204     c                   endif
064900000407     C*--------------------
065000000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
065100000407     C*--------------------
065200000407     C     ERRL00        IFEQ      '0'
065300021230     C     ERRL00        IFEQ      '0'
065400021219     C     VIDCA1        IFEQ      *BLANKS
065500000407     C                   MOVEL     '1'           ERRL00
065600000407     C                   SETON                                        80
065700021219     C                   ENDIF
065800021230     C                   ENDIF
065900021219     C     ERRL00        IFEQ      '0'
066000030107     C                   IF        (VIHTI1 =  'F' AND (VIDSO1F = *BLANKS  OR
066100030107     C                                                 VIDSO1F = *ZEROS)) OR
066200030107     C                             (VIHTI1 <> 'F' AND (VIDSO1K = *BLANKS  OR
066300030107     C                                                 VIDSO1K = *ZEROS))
066400000407     C                   MOVEL     '1'           ERRL00
066500000407     C                   SETON                                        81
066600000407     C                   ENDIF
066700021219     C                   ENDIF
066800021219     C     ERRL00        IFEQ      '0'
066900021219     C     VIDCO1        IFEQ      *BLANKS
067000000407     C                   MOVEL     '1'           ERRL00
067100000407     C                   SETON                                        82
067200000407     C                   ENDIF
067300021219     C                   ENDIF
067400021219     C     ERRL00        IFEQ      '0'
067500021219     C     VIDQT1        IFEQ      *ZEROS
067600021219     C                   MOVEL     '1'           ERRL00
067700021219     C                   SETON                                        83
067800021219     C                   ENDIF
067900021219     C                   ENDIF
068000021219     C     ERRL00        IFEQ      '0'
068100030107     C                   IF        ((VIHTI2 =  'F' AND (VIDSO2F = *BLANKS   OR
068200030107     C                                                  VIDSO2F = *ZEROS))  OR
068300030107     C                              (VIHTI2 <> 'F' AND (VIDSO2K = *BLANKS   OR
068400030107     C                                                  VIDSO2K = *ZEROS))) AND
068500030107     C                             *IN92 = *OFF
068600021219     C                   MOVEL     '1'           ERRL00
068700021219     C                   SETON                                        84
068800021219     C                   ENDIF
068900021219     C                   ENDIF
069000021220     C     ERRL00        IFEQ      '1'
069100000407     C                   MOVEL     MSG(12)       DSMSMS
069200000407     C                   ENDIF
069300021220     C                   ENDIF
069400021223     C*
069500021230     C                   EVAL      VIDRD2 = VIDRD1
069600021230     C                   EVAL      VIDDD2 = VIDDD1
069700030612     C                   IF        POP1 = 'F06'
069800021223     C                   EVAL      VIDUT1 = DUTUTE
069900030711     C                   EVAL      VIDPO1 = DUTPOU
070000030711     C                   EVAL      VIDDA1 = DATCOR
070100030711     C                   EVAL      VIDOR1 = ORACOR
070200030612     C                   ENDIF
070300030711     C                   EVAL      VIDUT2 = VIDUT1
070400021230     C                   EVAL      VIDPO2 = VIDPO1
070500021223     C                   EVAL      VIDDA2 = VIDDA1
070600021223     C                   EVAL      VIDOR2 = VIDOR1
070700071127     C*
070800071127     C                   IF        ERRL00 = '1'
070900071127     C                   EVAL      wforza = *off
071000071127     C                   ENDIF
071100030109     C*
071200970528     C                   ENDSR
071300150213     C*------------------------------------------------------------------------*
071400150213     C* contrqta controllo per i clienti se la quantità è evadibile
071500150213     C*------------------------------------------------------------------------*
071600150213     C     contrqta      BEGSR
071700150223     c                   clear                   saldoc            7 0
071800150213     c                   clear                   saldo             7 0
071900150213     c                   clear                   franchigia        7 0
072000150213      * cerca il saldo dei movimenti per cliente dalla data dell'ultimo movimento
072100150213      *
072200150213     C/EXEC SQL
072300150213     C+ select sum(mgbqta) into :saldo from azmgb00f
072400150216     C+ where mgbsog = :vidso1k  and mgbcod = :vidco1 and mgbtip = 'C'
072500150213     C/END-EXEC
072600150223     c                   if        saldo = 0
072700150216     C                   MOVEL     '1'           ERRL00
072800150216     c                   seton                                        83
072900150216     c                   eval      dsmsms = msg(17)
073000150216     c                   endif
073100150216      * saldo maggiore di zero verifico se quantità richiesta è in capienza
073200150303     c                   if        §ggbsco = 'S'
073300150223     c                   if        saldo <> 0 and cgbsco > 0
073400150414     c                   eval(h)   franchigia = (saldo * cgbsco) /100
073500150213     c                   eval      saldo = saldo - franchigia
073600150303     c                   endif
073700150303     c                   endif
073800150223     c                   if        saldo < 0
073900150223     c                   eval      saldoc = saldo + vidqt1
074000150223     c                   else
074100150507     c                   eval      saldoc =  saldo
074200150507     c*                  eval      saldoc = vidqt1 - saldo
074300150223     c                   endif
074400150223     c                   if        saldoc > 0
074500150216     C                   MOVEL     '1'           ERRL00
074600150216     c                   seton                                        83
074700150223     c                   eval      dsmsms = %trim(msg(16)) + %editc(saldoc:'Z')
074800150213     c                   endif
074900150213     c                   endsr
075000970526     C*------------------------------------------------------------------------*
075100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
075200970526     C*------------------------------------------------------------------------*
075300970526     C     AZZVID        BEGSR
075400000407     C*
075500021219     C                   MOVEL     *BLANKS       VIDCA1
075600021219     C                   MOVEL     *BLANKS       VIDDE1
075700021219     C                   MOVEL     *BLANKS       VIDDT1
075800030107     C                   MOVEL     *BLANKS       VIDSO1F
075900030107     C                   MOVEL     *BLANKS       VIDSO1K
076000021219     C                   MOVEL     *BLANKS       VIDDS1
076100021219     C                   MOVEL     *BLANKS       VIDCO1
076200021219     C                   MOVEL     *BLANKS       VIDDC1
076300021219     C                   Z-ADD     *ZEROS        VIDQT1
076400021220     C                   MOVEL     *BLANKS       VIDRD1
076500021220     C                   Z-ADD     *ZEROS        VIDDD1
076600021219     C                   MOVEL     *BLANKS       VIDUT1
076700030711     C                   Z-ADD     *ZEROS        VIDPO1
076800021219     C                   Z-ADD     *ZEROS        VIDDA1
076900021219     C                   Z-ADD     *ZEROS        VIDOR1
077000021219     C*
077100021219     C                   MOVEL     *BLANKS       VIDCA2
077200021219     C                   MOVEL     *BLANKS       VIDDE2
077300021219     C                   MOVEL     *BLANKS       VIDDT2
077400030107     C                   MOVEL     *BLANKS       VIDSO2K
077500030107     C                   MOVEL     *BLANKS       VIDSO2F
077600021219     C                   MOVEL     *BLANKS       VIDDS2
077700021219     C                   MOVEL     *BLANKS       VIDCO2
077800021219     C                   MOVEL     *BLANKS       VIDDC2
077900021219     C                   Z-ADD     *ZEROS        VIDQT2
078000021220     C                   MOVEL     *BLANKS       VIDRD2
078100021220     C                   Z-ADD     *ZEROS        VIDDD2
078200021219     C                   MOVEL     *BLANKS       VIDUT2
078300021219     C                   Z-ADD     *ZEROS        VIDDA2
078400021219     C                   Z-ADD     *ZEROS        VIDOR2
078500071127     C*
078600071127     C* Spengo il flag di forzatura messaggio
078700071127     C                   eval      wforza = *off
078800071127     C
078900970526     C*
079000970526     C                   ENDSR
079100000407     C*------------------------------------------------------------------------*
079200021219     C* IMPCAM - IMPOSTA I CAMPI DAL VIDEO ALLA DS DEL "DRIVER" MOVIMENTI GABBIE/BANCALI
079300000407     C*------------------------------------------------------------------------*
079400000407     C     IMPCAM        BEGSR
079500021219     C*
079600021219     C                   CLEAR                   TISIG0DS
079700021219     C                   Z-ADD     DATCOR        IG0DAT
079800021219     C                   Z-ADD     ORACOR        IG0ORA
079900021230     C                   MOVEL     DUTUTE        IG0UTE
080000021230     C                   Z-ADD     DUTPOU        IG0POU
080100021219     C                   MOVEL     VIDCA1        IG0CAU
080200030107     C                   IF        VIHTI1 = 'F'
080300030107     C                   MOVEL     *zeros        IG0SOG
080400030107     C                   MOVE      VIDSO1F       IG0SOG
080500030107     C                   ELSE
080600030107     C                   MOVEL     *zeros        IG0SOG
080700030107     C                   MOVE      VIDSO1K       IG0SOG
080800030107     C                   ENDIF
080900021219     C                   MOVEL     VIDCO1        IG0COD
081000021219     C                   Z-ADD     VIDQT1        IG0QTA
081100021230     C                   MOVEL     VIDRD1        IG0RDO
081200060208     C*
081300060208     C                   Z-ADD     VIDDD1        G08DAT
081400060208     C                   Z-ADD     *ZEROS        G08INV
081500060208     C                   MOVEL     '0'           G08ERR
081600060208     C                   CALL      'XSRDA8'
081700060208     C                   PARM                    WLBDA8
081800060208     C                   Z-ADD     G08INV        IG0DDO
081900060208     C*
082000030107     C                   IF        VIHTI2 = 'F'
082100030107     C                   MOVEL     *zeros        IG0SMC
082200030107     C                   MOVE      VIDSO2F       IG0SMC
082300030107     C                   ELSE
082400030107     C                   MOVEL     *zeros        IG0SMC
082500030107     C                   MOVE      VIDSO2K       IG0SMC
082600030107     C                   ENDIF
082700000407     C*
082800000407     C                   ENDSR
082900030123     C*------------------------------------------------------------------------*
083000030123     C* REPCAU - CONTROLLA CAUSALE MOVIMENTO
083100030123     C*------------------------------------------------------------------------*
083200030123     C     REPCAU        BEGSR
083300030123     C*
083400030123     C     *LIKE         DEFINE    mgbCAU        Chk_CAU
083500030123     C     *LIKE         DEFINE    VIDDE1        Chk_DES
083600030123     C*
083700030123     C* RICHIESTO ELENCO
083800030123     C                   SETOFF                                           28
083900030123     C     '?'           SCAN      Chk_CAU                                28
084000030123     C     *IN28         IFEQ      *ON
084100030123     C*
084200030123     C* VALORIZZO I PARAMETRI DI PASSAGGIO
084300030123     C                   CLEAR                   PARAM2
084400030123     C                   EVAL      P2OP1 = 'P1M'
084500030123     C                   MOVEL     PARAM2        KPJBU
084600030123     C                   CALL      'TNTB31R'
084700030123     C                   PARM                    KPJBA
084800030123     C*
084900030123     C                   MOVEL     KPJBU         PARAM2
085000030123     C                   MOVEL     P2CAU         Chk_CAU
085100030123     C                   ENDIF
085200030123     C*
085300030123     C* IN OGNI CASO CONTROLLO ESISTENZA CODICE CAUSALE
085400030123     C                   EVAL      tbeCOD = 'GGB'
085500030123     C                   EVAL      tbeKE1 = Chk_CAU
085600030123     C     KEYTBEP       CHAIN     tntbe01l
085700030123     C                   IF        %found(tntbe01l)
085800030123     C                   CLEAR                   DGGB
085900030123     C                   MOVEL(P)  tbeUNI        DGGB
086000030123     C                   MOVEL(P)  §GGBDES       Chk_DES
086100030123     C                   ELSE
086200030123     C                   MOVEL     '1'           ERRL00
086300030123     C                   MOVEL(P)  *all'*'       Chk_DES
086400030123     C                   ENDIF
086500030123     C*
086600030123     C                   ENDSR
086700021220     C*------------------------------------------------------------------------*
086800021220     C* REPKSC - CONTROLLA CODICE CLIENTE
086900021220     C*------------------------------------------------------------------------*
087000021220     C     REPKSC        BEGSR
087100021220     C*
087200021220     C     *LIKE         DEFINE    mgbSOG        Chk_SOG
087300021220     C*
087400021220     C* RICHIESTO ELENCO
087500021220     C                   SETOFF                                           28
087600021220     C     '?'           SCAN      Chk_SOG                                28
087700021220     C     *IN28         IFEQ      *ON
087800021220     C*
087900021220     C* RICHIESTO ELENCO CLIENTI
088000150212     C                   MOVEL     KPJBU         SAVKPJBU
088100150212     C                   MOVEL(P)  'P01'         KPJBU
088200150212     C                   CALL      'TISIG10R'
088300150212     C                   PARM                    KPJBA
088400150212     C                   EVAL      Chk_sog = %subst(KPJBU:4:8)
088500150212     C                   MOVEL     SAVKPJBU      KPJBU
088600021220     C*
088700021220     C* CONTROLLA ESISTENZA CLIENTE
088800021220     C                   ENDIF
088900150304     C                   TESTN                   chk_sog              30
089000150304     C                   MOVE      chk_sog       W001A             1
089100150304    2C     *IN30         IFEQ      *OFF
089200150304     C     W001A         ORLT      '0'
089300150304     C                   MOVEL     '1'           ERRL00
089400150304     C                   SETON                                        81
089500150304     C                   leavesr
089600150304     c                   endif
089700150212     C                   EXSR      DECTIB
089800021220     C*
089900021220     C                   ENDSR
090000021220     C*------------------------------------------------------------------------*
090100021220     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
090200021220     C*------------------------------------------------------------------------*
090300021220     C     DECTIB        BEGSR
090400021220     C*
090500021220     C                   CLEAR                   BS69DS
090600021220     C                   CLEAR                   ACODS
090700021220     C                   MOVEL     KNSIF         I69SIF
090800021220     C                   MOVE(P)   Chk_SOG       I69KAC
090900021220     C                   CALL      'TIBS69R'
091000021220     C                   PARM                    BS69DS
091100021220     C                   PARM                    ACODS
091200021220     C                   PARM                    INDDS
091300021220     C                   PARM                    CLPDS
091400021220     C                   PARM                    CLSDS
091500021220     C     O69ERR        IFNE      '1'
091600021223     C                   EVAL      Chk_DES = acoRAG
091700021220     C                   ELSE
091800021220     C                   MOVEL     '1'           ERRL00
091900021220     C                   ENDIF
092000021220     C*
092100021220     C                   ENDSR
092200021220     C*------------------------------------------------------------------------*
092300021220     C* REPDST - CONTROLLA CODICE DESTINATARIO
092400021220     C*------------------------------------------------------------------------*
092500021220     C     REPDST        BEGSR
092600021220     C*
092700021220     C                   MOVEL     *all'0'       WrkDST_A          7
092800021220     C                   Z-ADD     *zeros        WrkDST_N          7 0
092900021220     C*
093000021220     C* RICHIESTO ELENCO
093100021220     C                   SETOFF                                           28
093200021220     C     '?'           SCAN      Chk_SOG                                28
093300021220     C     *IN28         IFEQ      *ON
093400021220     C*
093500021220     C* RICHIESTO ELENCO DESTINATARI
093600021220     C                   MOVEL     KPJBU         SAVKPJBU
093700021220     C                   MOVEL(P)  'P01'         KPJBU
093800021220     C                   CALL      'TISIT1R'
093900021220     C                   PARM                    KPJBA
094000021220     C                   EVAL      WrkDST_A = %subst(KPJBU:4:7)
094100021220     C                   MOVEL     *all'0'       Chk_SOG
094200021220     C                   MOVE      WrkDST_A      Chk_SOG
094300021220     C                   MOVEL     SAVKPJBU      KPJBU
094400021220     C                   ELSE
094500021220     C                   MOVE(P)   Chk_SOG       WrkDST_N
094600021220     C     WrkDST_N      CHAIN     azdst04l
094700021220     C                   IF        %found(azdst04l)
094800021220     C                   EVAL      Chk_DES = dstRAG
094900021220     C                   ELSE
095000021220     C                   EVAL      ERRL00 = '1'
095100021220     C                   EVAL      Chk_DES = *all'*'
095200021220     C                   ENDIF
095300021220     C                   ENDIF
095400021220     C*
095500021220     C                   ENDSR
095600021220     C*------------------------------------------------------------------------*
095700021220     C* REPPO - CONTROLLA CODICE P.O.
095800021220     C*------------------------------------------------------------------------*
095900021220     C     REPPO         BEGSR
096000021220     C*
096100021220     C                   Z-ADD     *zeros        WrkPO_N           3 0
096200021220     C*
096300021220     C* RICHIESTO ELENCO
096400021220     C                   SETOFF                                           28
096500021220     C     '?'           SCAN      Chk_SOG                                28
096600021220     C     *IN28         IFEQ      *ON
096700021220     C*
096800021220     C* RICHIESTO ELENCO P.O.
096900021220     C                   CALL      'TNSD24R'
097000021220     C                   PARM                    §COD              3
097100021220     C                   PARM      'F'           §TIP              1
097200021220     C                   PARM                    §§DES            25
097300021220     C                   PARM                    §§PRA             3
097400021220     C                   MOVEL     *all'0'       Chk_SOG
097500021220     C                   MOVE      §COD          Chk_SOG
097600021220     C                   ELSE
097700021220     C                   MOVE      Chk_SOG       WrkPO_N
097800021220     C     WrkPO_N       CHAIN     azorg01l
097900021220     C                   IF        %found(azorg01l)
098000021220     C                   EVAL      Chk_DES = orgDES
098100021220     C                   ELSE
098200021220     C                   EVAL      ERRL00 = '1'
098300021220     C                   EVAL      Chk_DES = *all'*'
098400021220     C                   ENDIF
098500021220     C                   ENDIF
098600021220     C*
098700021220     C                   ENDSR
098800021220     C*------------------------------------------------------------------------*
098900021220     C* REPCOD - CONTROLLA CODICE ARTICOLO
099000021220     C*------------------------------------------------------------------------*
099100021220     C     REPCOD        BEGSR
099200021220     C*
099300021220     C     *LIKE         DEFINE    mgbCOD        Chk_COD
099400021220     C*
099500021220     C* RICHIESTO ELENCO
099600021220     C                   SETOFF                                           28
099700021220     C     '?'           SCAN      Chk_COD                                28
099800021220     C     *IN28         IFEQ      *ON
099900021220     C*
100000021220     C* RICHIESTO ELENCO ARTICOLI
100100021220     C                   MOVEL     KPJBU         SAVKPJBU
100200021220     C                   MOVEL(P)  'P01'         KPJBU
100300021220     C                   CALL      'TISIG1R'
100400021220     C                   PARM                    KPJBA
100500021220     C                   EVAL      Chk_COD = %subst(KPJBU:4:15)
100600150226     C                   movel     Chk_COD       vidco1
100700021220     C                   MOVEL     SAVKPJBU      KPJBU
100800021220     C                   ELSE
100900021220     C     Chk_COD       CHAIN     azagb01l
101000021220     C                   IF        %found(azagb01l)
101100021220     C                   EVAL      Chk_DES = agbDES
101200021220     C                   ELSE
101300150213     C                   SETON                                        82
101400150213     C                   MOVEL     MSG(05)       DSMSMS
101500021220     C                   EVAL      ERRL00 = '1'
101600021220     C                   EVAL      Chk_DES = *all'*'
101700021220     C                   ENDIF
101800021220     C                   ENDIF
101900150304      *
102000150304     c                   movel     vidco1        wtsc
102100150212     C                   IF        errl00 = '0'
102200150303     C                   IF        VIHTI1 = 'C'
102300150225     c     kcgb          setgt     azcgb01l
102400150225     c     kcgb          readpe    azcgb01l
102500150303     C                   IF        %eof(azcgb01l)
102600150303     C                   EVAL      ERRL00 = '1'
102700150303     C                   SETON                                        82
102800150303     C                   MOVEL     MSG(15)       DSMSMS
102900150303     C                   ENDIF
103000150224     C                   if        Datcor < cgbdti or Datcor > cgbdtf
103100150224     C                   EVAL      ERRL00 = '1'
103200150224     C                   SETON                                        82
103300150224     C                   MOVEL     MSG(19)       DSMSMS
103400150224     C                   ENDIF
103500150212     C                   IF        chk_cod <> cgbcod1 and
103600150212     C                             chk_cod <> cgbcod2 and
103700150212     C                             chk_cod <> cgbcod3
103800150212     C                   EVAL      ERRL00 = '1'
103900150212     C                   SETON                                        82
104000150212     C                   MOVEL     MSG(15)       DSMSMS
104100150212     C                   ENDIF
104200150212     C                   ENDIF
104300150303     C                   ENDIF
104400150304     C                   IF        errl00 = '0'
104500150304     C                   IF        §ggbtsc <> *blank and
104600150304     c                             §ggbtsc <> wtsc
104700150304     C                   EVAL      ERRL00 = '1'
104800150304     C                   SETON                                        82
104900150304     C                   MOVEL     MSG(20)       DSMSMS
105000150304     C                   ENDIF
105100150304     C                   ENDIF
105200021220     C*
105300021220     C                   ENDSR
105400021220     C*------------------------------------------------------------------------*
105500021220     C* CHKDA8 - CONTROLLA LA DATA (8)
105600021220     C*------------------------------------------------------------------------*
105700021220     C     CHKDA8        BEGSR
105800021220     C*
105900021223     C     *LIKE         DEFINE    VIDDD1        WDA8
106000021223     C     *LIKE         DEFINE    VIDDD1        DA8W
106100021220     C*
106200021220     C                   Z-ADD     WDA8          G08DAT
106300021220     C                   Z-ADD     *ZEROS        G08INV
106400021220     C                   MOVEL     '0'           G08ERR
106500021220     C                   CALL      'XSRDA8'
106600021220     C                   PARM                    WLBDA8
106700021220     C                   Z-ADD     G08DAT        WDA8
106800021220     C                   Z-ADD     G08INV        DA8W
106900021220     C     G08ERR        IFEQ      '1'
107000021220     C                   MOVEL     '1'           ERRL00
107100021220     C                   ENDIF
107200021220     C*
107300021220     C                   ENDSR
107400021219     C*------------------------------------------------------------------------*
107500021220     C* LETMOV - LEGGE I 2 RECORD DEL MOVIMENTO
107600021219     C*------------------------------------------------------------------------*
107700021219     C     LETMOV        BEGSR
107800021219     C*
107900021219     C* 1° MOVIMENTO
108000160727     C     KEYMGB        CHAIN(n)  AZMGB03L                           99
108100021219     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
108200021219     C                   MOVEL     '1'           ERRL00                         *ERRORE
108300021219     C                   MOVEL     MSG(6)        PMSG                           *MESSAGGIO
108400030107     C                   MOVEL     MSG(6)        DSMSMS
108500021219     C                   MOVEL     '1'           FINL00                         *FINE PGM
108600021219     C                   ELSE
108700021219     C                   MOVEL     mgbCAU        VIDCA1
108800030107     C                   IF        mgbTIP = 'F'
108900030107     C                   SETOFF                                       55
109000030107     C                   MOVE(P)   mgbSOG        VIDSO1F
109100030107     C                   ELSE
109200030107     C                   SETON                                        55
109300030107     C                   MOVE(P)   mgbSOG        VIDSO1K
109400030107     C                   ENDIF
109500021219     C                   MOVEL     mgbCOD        VIDCO1
109600021219     C                   Z-ADD     mgbQTA        VIDQT1
109700021219     C                   MOVEL     mgbUTE        VIDUT1
109800030711     C                   Z-ADD     mgbPOU        VIDPO1
109900021219     C                   Z-ADD     mgbDAT        VIDDA1
110000021219     C                   Z-ADD     mgbORA        VIDOR1
110100030122     C                   Z-ADD     mgbPRG        VIDPRG1
110200030311     C                   MOVEL     mgbRDO        VIDRD1
110300071218     C*
110400071218     C                   Z-ADD     mgbDDO        G08INV
110500071218     C                   Z-ADD     *ZEROS        G08DAT
110600071218     C                   MOVEL     '3'           G08ERR
110700071218     C                   CALL      'XSRDA8'
110800071218     C                   PARM                    WLBDA8
110900071218     C                   Z-ADD     G08DAT        VIDDD1
111000021219     C                   ENDIF
111100021219     C*
111200021219     C* 2° MOVIMENTO
111300160727     C     KEYMGB        READE(n)  AZMGB03L                               99
111400021219     C     *IN99         IFEQ      *OFF                                         *ESISTENTE
111500021219     C                   MOVEL     mgbCAU        VIDCA2
111600030107     C                   IF        mgbTIP = 'F'
111700030107     C                   SETOFF                                       65
111800030107     C                   MOVE(P)   mgbSOG        VIDSO2F
111900030107     C                   ELSE
112000030107     C                   SETON                                        65
112100030107     C                   MOVE(P)   mgbSOG        VIDSO2K
112200030107     C                   ENDIF
112300021219     C                   MOVEL     mgbCOD        VIDCO2
112400021219     C                   Z-ADD     mgbQTA        VIDQT2
112500021219     C                   MOVEL     mgbUTE        VIDUT2
112600021219     C                   Z-ADD     mgbDAT        VIDDA2
112700021219     C                   Z-ADD     mgbORA        VIDOR2
112800030122     C                   Z-ADD     mgbPRG        VIDPRG2
112900030311     C                   MOVEL     mgbRDO        VIDRD2
113000071218     C*
113100071218     C                   Z-ADD     mgbDDO        G08INV
113200071218     C                   Z-ADD     *ZEROS        G08DAT
113300071218     C                   MOVEL     '3'           G08ERR
113400071218     C                   CALL      'XSRDA8'
113500071218     C                   PARM                    WLBDA8
113600071218     C                   Z-ADD     G08DAT        VIDDD2
113700021219     C                   ENDIF
113800021219     C*
113900021219     C                   ENDSR
114000970526     C*------------------------------------------------------------------------*
114100970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
114200970526     C*------------------------------------------------------------------------*
114300970526     C     IMPFUN        BEGSR
114400970526     C*
114500970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
114600021211     C     POP1          IFEQ      'O05'
114700970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
114800970526     C                   MOVEL     'N'           FUDATT
114900970526     C                   MOVEL     FUDRIG        FUD(6)
115000021211     C                   ENDIF
115100160727     C* SE cancellazione   TOGLIE LA FUNZIONE DI AGGIORNAMENTO
115200160727     C     POP1          IFEQ      'O04'
115300160727     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
115400160727     C                   MOVEL     'N'           FUDATT
115500160727     C                   MOVEL     FUDRIG        FUD(6)
115600160727     C                   MOVEL     FUD(8)        FUDRIG                         *AGGIORNAMENTO
115700160727     C                   MOVEL     'S'           FUDATT
115800160727     C                   MOVEL     FUDRIG        FUD(8)
115900160727     C                   ENDIF
116000970526     C*
116100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
116200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
116300970526     C                   EXSR      FUNGES
116400970526     C*
116500000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
116600021219     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
116700021219     C                   EXSR      LETMOV
116800980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
116900980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
117000000407     C                   ELSE                                                   *INSERIMENTO
117100970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
117200000407     C                   ENDIF
117300970526     C*
117400021211     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
117500021211     C     POP1          IFEQ      'O02'
117600021211     C                   MOVEL     DEC(1)        VIDFUN
117700021211     C                   ENDIF
117800021211     C     POP1          IFEQ      'O03'
117900021211     C                   MOVEL     DEC(5)        VIDFUN
118000021211     C                   ENDIF
118100021211     C     POP1          IFEQ      'O04'
118200021211     C                   MOVEL     DEC(2)        VIDFUN
118300021211     C                   ENDIF
118400021211     C     POP1          IFEQ      'O05'
118500021211     C                   MOVEL     DEC(3)        VIDFUN
118600021211     C                   ENDIF
118700021211     C     POP1          IFEQ      'F06'
118800021211     C                   MOVEL     DEC(4)        VIDFUN
118900021211     C                   ENDIF
119000021211     C*
119100021211     C                   ENDSR
119200021219     C*--------------------------------------------------------------------------------------------*
119300021219     C* REPERISCE I DATI UTENTE
119400021219     C*--------------------------------------------------------------------------------------------*
119500021219     C     REPDATIUTE    BEGSR
119600021219     C*
119700021219     C* INIZIALIZZA VARIABILI DI WRK
119800021219     C                   CLEAR                   TIBS34DS
119900021219     C                   CLEAR                   AZUTEDS
120000021219     C                   CLEAR                   DDATIUTE
120100021219     C*
120200021219     C     *DTAARA       DEFINE    §azute        azuteds
120300021219     C     *DTAARA       DEFINE    §datiute      ddatiute
120400021219     C                   IN(E)     *DTAARA
120500021219     C                   IF        %Error
120600021220     C                   EVAL      I34Tla = 'L'
120700021219     C                   CALL      'TIBS34R'
120800021219     C                   PARM                    Tibs34Ds
120900021219     C                   IN        *DTAARA
121000021219     C                   ENDIF
121100030702     C*
121200030702     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
121300030702     C                   CLEAR                   L1
121400030702     C                   CLEAR                   WAbi              2
121500150311     c                   Clear                   dLat
121600150311      * Verifica errori e autorità profilo
121700150311s   1c                   Select
121800150311      * se ho errori nei dati utente esco dal pgm
121900150311w   1c                   When      DutErr = 'E'
122000150311     c                   seton                                        lr
122100150311     c                   return
122200150311      * se non c'è l'abilitazione
122300150311      * --> se 1° livello, abilitazioni al terminal
122400150311      *     se 2° livello, abilitazioni al punto operativo
122500150311w   1c                   When      UteAut = *Blanks
122600150311if  2c                   If        DutLpo = '1'
122700150311     c                   Eval      wabi   = 'TP'
122800150311e   2c                   EndIf
122900150311if  2c                   If        DutLpo = '2'
123000150311     c                   Eval      wabi   = 'PO'
123100150311e   2c                   EndIf
123200150311if  2c                   If        DutLpo = 'S'
123300150311     c                   Eval      wabi   = 'AZ'
123400150311e   2c                   EndIf
123500150311      * carica le abilitazioni del profilo
123600150311x   1c                   Other
123700150311     c                   Movel     UteFaf        Dute01
123800150311     c                   If        §Utemgb <> *Blanks
123900150311     c                   Eval      wabi = §Utemgb
124000150311     c                   Else
124100150311     c                   Eval      wabi = UteAut
124200150311     c                   EndIf
124300150311e   1c                   EndSl
124400150311      * controllo se ok l'abilitazione dell'utente
124500150311     c                   Clear                   Tibs02ds
124600150311     c                   Eval      T02Mod = 'C'
124700150311     c                   Eval      T02Sif = knsif
124800150311     c                   Eval      T02Cod = 'LAT'
124900150311     c                   Movel(p)  wabi          T02Ke1
125000150311     c                   Call      'TIBS02R'
125100150311     c                   Parm                    kpjba
125200150311     c                   Parm                    Tibs02ds
125300150311if  1c                   If        T02Err = *Blanks
125400150311     c                   Eval      dLat = T02Uni
125500150311e   1c                   EndIf
125600150311      * errore o non abilitato (impossibile xchè controllato nel filtro)
125700150311if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
125800150311   x1c                   Else
125900150311      * Reperimento dei P.O. gestibili dall'utente
126000150311     c                   clear                   TRUL31DS
126100150311     c                   clear                   TRUL31DS2
126200150311     c                   eval      I31abi = wabi
126300150311     c                   eval      I31cdi = DUTdis
126400150311     c                   eval      I31car = DUTare
126500150311     c                   eval      I31cpo = DUTpou
126600150311     c                   call      'TRUL31R'
126700150311     c                   parm                    KPJBA
126800150311     c                   parm                    TRUL31DS
126900150311     c                   parm                    TRUL31DS2
127000150311if  2c                   if        O31pog > *zeros
127100150311     c                   movea     O31pog        L1
127200150311     c                   movea     O31arg        $arg
127300150311     c                   movea     O31dig        $dig
127400150311x   2c                   else
127500150311     c* impossibile perchè controllato nel filtrp
127600150311     c                   seton                                        lr
127700150311     c                   return
127800150311e   2c                   endif
127900150311    1c                   endif
128000021219     C*
128100021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
128200021219     C                   MOVEL(P)  RSUT          DSFIRS           20
128300021219     C*
128400021219     C                   ENDSR
128500970526     C*------------------------------------------------------------------------*
128600970526     C* *INZSR - OPERAZIONI INIZIALI
128700970526     C*------------------------------------------------------------------------*
128800970526     C     *INZSR        BEGSR
128900970526     C*---
129000970526     C* RICEVIMENTO PARAMETRI
129100970526     C*---
129200970526     C     *ENTRY        PLIST
129300970612     C                   PARM                    KPJBA
129400000407     C                   MOVEL     KPJBU         PARAM
129500021220     C*
129600021220     C* DICHIARAZIONE VARIABILI DI WRK
129700021220     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
129800021219     C*
129900021219     C* REPERISCE I DATI DELL'UTENTE
130000021219     C                   EXSR      REPDATIUTE
130100000407     C*
130200000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
130300000407     C     POP1          IFEQ      'O05'
130400160727     C     POP1          orEQ      'O04'
130500021230     C                   SETON                                        90
130600000407     C                   ENDIF
130700021219     C     POP1          IFEQ      'F06'
130800021223     C                   SETON                                        9192
130900000407     C                   ENDIF
131000021219     C*---
131100021219     C* INIZIALIZZAZIONE CAMPI DI WRK
131200021219     C*---
131300021219     C                   Z-ADD     1             ACOKUT
131400021219     C                   Z-ADD     151           ACOKCC
131500150303     C*
131600150303     C* VALORIZZO I DATI RELATIVI AD UTENTE MODIFICA/DATA MODIFICA/ORA MODIFICA
131700150303     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
131800150303     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
131900150303     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
132000150303     C                   Z-ADD     WN8           G08DAT
132100150303     C                   Z-ADD     *ZEROS        G08INV
132200150303     C                   MOVEL     '0'           G08ERR
132300150303     C                   CALL      'XSRDA8'
132400150303     C                   PARM                    WLBDA8
132500150303     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
132600021219     C*---
132700021219     C* CHIAVI DI LETTURA
132800021219     C*---
132900030122     C* LETTURA AZMGB03L - COMPLETA
133000021219     C     KEYMGB        KLIST
133100030122     C                   KFLD                    PPRG                           *PROGRESSIVO UNIVOCO
133200150225     C* LETTURA AZcgb01L - COMPLETA
133300150225     C     Kcgb          KLIST
133400150225     C                   KFLD                    vidso1k
133500150225     C                   KFLD                    wtsc              1
133600021219     C* LETTURA TNTBE01L - PARZIALE
133700030123     C     KEYTBEP       KLIST
133800021219     C                   KFLD                    tbeCOD                         *CODICE TABELLA
133900021219     C                   KFLD                    tbeKE1                         *CHIAVE 1
134000021219     C* LETTURA CNACO00F - COMPLETA
134100021219     C     KEYACO        KLIST
134200021219     C                   KFLD                    acoKUT                         *CODICE UTENTE
134300021219     C                   KFLD                    acoKCC                         *CODICE CAPOCONTO
134400021219     C                   KFLD                    acoKSC                         *CODICE SOTTOCONTO
134500970526     C*
134600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
134700970526     C     XMSGPA        PLIST
134800970526     C                   PARM                    DSMSMS
134900970526     C                   PARM                    DSMSPG
135000970526     C                   PARM                    DSMSKE
135100970526     C                   PARM                    DSMSER
135200970526     C*
135300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
135400970526     C                   CALL      'XKEY'
135500970526     C                   PARM                    DSKEY
135600970526     C*---
135700970526     C* VARIABILI NON RIFERITE AL DATA BASE
135800970526     C*---
135900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
136000970526     C*---
136100970526     C* VARIABILI DI CONTROLLO
136200970526     C*---
136300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
136400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
136500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
136600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
136700970526     C*
136800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
136900021219     C                   MOVEL     'TISIG4R'     DSMSPG
137000021219     C                   MOVEL     'TISIG4R'     MSGQUE
137100970526     C                   MOVEL     *BLANKS       MSGKEY
137200970526     C                   MOVEL     *BLANKS       DSMSMS
137300970526     C*
137400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
137500970526     C                   EXSR      IMPFUN
137600970526     C*
137700970526     C                   ENDSR
137800970526     C*------------------------------------------------------------------------*
137900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
138000970526     C*------------------------------------------------------------------------*
138100970526     C     FUNGES        BEGSR
138200970526     C*
138300970526     C* PULISCO CAMPI DI LAVORO
138400970526     C                   Z-ADD     0             W                 5 0
138500970526     C*
138600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
138700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
138800970526     C     1             DO        24            I
138900970526     C                   MOVEL     FUD(I)        FUDRIG
139000970526     C*
139100970526     C* PULIZIA FUNZIONI PRECEDENTI
139200970526     C                   MOVEL     *BLANKS       FUV(I)
139300970526     C*
139400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
139500970526     C* (SALTA F24)
139600970526     C     FUDATT        IFEQ      'S'
139700970526     C     FUDFUN        ANDNE     '24'
139800970526     C                   ADD       1             W
139900970526     C                   MOVEL     FUDDES        FUV(W)
140000970526     C                   END
140100970526     C                   END                                                    DO WHILE
140200970526     C*
140300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
140400970526     C* ALTRIMENTI DISATTIVO F24
140500970526     C     1             DO        24            I
140600970526     C                   MOVEL     FUD(I)        FUDRIG
140700970526     C*
140800970526     C     FUDFUN        IFEQ      '24'
140900970526     C     W             IFGT      6
141000970526     C                   MOVEL     'S'           FUDATT
141100970526     C                   ELSE
141200970526     C                   MOVEL     *BLANKS       FUDATT
141300970526     C                   END                                                    W GT 8
141400970526     C                   MOVEL     FUDRIG        FUD(I)
141500970526     C                   END
141600970526     C                   END                                                    DO WHILE
141700970526     C*
141800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
141900970526     C     W             DIV       6             I
142000970526     C                   MVR                     Z
142100970526     C*
142200970526     C     Z             IFNE      0
142300970526     C     I             ADD       1             Z
142400970526     C                   ELSE
142500970526     C                   Z-ADD     I             Z                 5 0
142600970526     C                   END
142700970526     C*
142800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
142900970526     C                   ADD       1             FUNGRU
143000970526     C     FUNGRU        IFGT      Z
143100970526     C     FUNGRU        ORLT      1
143200970526     C                   Z-ADD     1             FUNGRU
143300970526     C                   END
143400970526     C*
143500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
143600970526     C     FUNGRU        IFEQ      1
143700970526     C                   MOVEL     FUV(1)        FUN001
143800970526     C                   MOVEL     FUV(2)        FUN002
143900970526     C                   MOVEL     FUV(3)        FUN003
144000970526     C                   MOVEL     FUV(4)        FUN004
144100970526     C                   MOVEL     FUV(5)        FUN005
144200970526     C                   MOVEL     FUV(6)        FUN006
144300970526     C                   END
144400970526     C*
144500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
144600970526     C     FUNGRU        IFEQ      2
144700970526     C                   MOVEL     FUV(7)        FUN001
144800970526     C                   MOVEL     FUV(8)        FUN002
144900970526     C                   MOVEL     FUV(9)        FUN003
145000970526     C                   MOVEL     FUV(10)       FUN004
145100970526     C                   MOVEL     FUV(11)       FUN005
145200970526     C                   MOVEL     FUV(12)       FUN006
145300970526     C                   END
145400970526     C*
145500970526     C* TERZO GRUPPO DI 6 FUNZIONI
145600970526     C     FUNGRU        IFEQ      3
145700970526     C                   MOVEL     FUV(13)       FUN001
145800970526     C                   MOVEL     FUV(14)       FUN002
145900970526     C                   MOVEL     FUV(15)       FUN003
146000970526     C                   MOVEL     FUV(16)       FUN004
146100970526     C                   MOVEL     FUV(17)       FUN005
146200970526     C                   MOVEL     FUV(18)       FUN006
146300970526     C                   END
146400970526     C*
146500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
146600970526     C     FUNGRU        IFEQ      4
146700970526     C                   MOVEL     FUV(19)       FUN001
146800970526     C                   MOVEL     FUV(20)       FUN002
146900970526     C                   MOVEL     FUV(21)       FUN003
147000970526     C                   MOVEL     FUV(22)       FUN004
147100970526     C                   MOVEL     *BLANKS       FUN005
147200970526     C                   MOVEL     *BLANKS       FUN006
147300970526     C                   END
147400970526     C*
147500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
147600970526     C                   MOVEL     *BLANKS       FUN007
147700970526     C                   MOVEL     *BLANKS       FUN008
147800970526     C*
147900970526     C     1             DO        24            I
148000970526     C                   MOVEL     FUD(I)        FUDRIG
148100970526     C     FUDFUN        IFEQ      '24'
148200970526     C     FUDATT        ANDEQ     'S'
148300970526     C                   MOVEL     FUDDES        FUN008
148400970526     C                   END
148500970526     C                   END                                                    DO
148600970526     C*
148700970526     C                   ENDSR
148800970526     C*------------------------------------------------------------------------*
148900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
149000970526     C*------------------------------------------------------------------------*
149100970526     C     FUNCON        BEGSR
149200970526     C*
149300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
149400970526     C                   EXSR      FUNVAR
149500970526     C*
149600970526     C* PULISCO CAMPI DI LAVORO
149700970526     C                   Z-ADD     0             I                 5 0
149800970526     C                   Z-ADD     0             W                 5 0
149900970526     C*
150000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
150100970526     C     FUNKEY        IFNE      *BLANKS
150200970526     C     1             DO        24            I
150300970526     C                   MOVEL     FUD(I)        FUDRIG
150400970526     C*
150500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
150600970526     C     FUNKEY        IFEQ      FUDFUN
150700970526     C     FUDATT        ANDEQ     'S'
150800970526     C                   Z-ADD     I             W
150900970526     C                   END
151000970526     C                   END                                                    DO WHILE
151100970526     C*
151200970526     C* FUNZIONE NON ATTIVA (ERRORE)
151300970526     C     W             IFEQ      0
151400970526     C*
151500970526     C* EMISSIONE MESSAGGIO DI ERRORE
151600970526     C                   MOVEL     MSG(1)        DSMSMS
151700970526     C*
151800970526     C                   END                                                    W=0
151900970526     C                   END                                                    FUNKEY NE *BLANKS
152000970526     C*
152100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
152200970526     C     W             IFNE      0
152300970526     C     FUNKEY        OREQ      *BLANKS
152400970526     C     FUNKEY        IFEQ      *BLANKS
152500970526     C                   EXSR      ENTGES
152600970526     C                   END
152700970526     C     FUNKEY        IFEQ      '03'
152800970526     C                   EXSR      F03GES
152900970526     C                   END
153000970526     C     FUNKEY        IFEQ      '06'
153100970526     C                   EXSR      F06GES
153200970526     C                   END
153300160727     C     FUNKEY        IFEQ      '08'
153400160727     C                   EXSR      F08GES
153500160727     C                   END
153600970526     C     FUNKEY        IFEQ      '12'
153700970526     C                   EXSR      F12GES
153800970526     C                   END
153900970526     C     FUNKEY        IFEQ      '24'
154000970526     C                   EXSR      F24GES
154100970526     C                   END
154200970526     C                   END
154300970526     C*
154400970526     C                   ENDSR
154500970526     C*------------------------------------------------------------------------*
154600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
154700970526     C*------------------------------------------------------------------------*
154800970526     C     FUNVAR        BEGSR
154900970526     C*
155000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
155100970526     C*
155200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
155300970526     C     KEY           IFEQ      ENTER
155400970526     C                   MOVEL     *BLANKS       FUNKEY
155500970526     C                   END
155600970526     C     KEY           IFEQ      F1
155700970526     C                   MOVEL     '01'          FUNKEY
155800970526     C                   END
155900970526     C     KEY           IFEQ      F2
156000970526     C                   MOVEL     '02'          FUNKEY
156100970526     C                   END
156200970526     C     KEY           IFEQ      F3
156300970526     C                   MOVEL     '03'          FUNKEY
156400970526     C                   END
156500970526     C     KEY           IFEQ      F4
156600970526     C                   MOVEL     '04'          FUNKEY
156700970526     C                   END
156800970526     C     KEY           IFEQ      F5
156900970526     C                   MOVEL     '05'          FUNKEY
157000970526     C                   END
157100970526     C     KEY           IFEQ      F6
157200970526     C                   MOVEL     '06'          FUNKEY
157300970526     C                   END
157400970526     C     KEY           IFEQ      F7
157500970526     C                   MOVEL     '07'          FUNKEY
157600970526     C                   END
157700970526     C     KEY           IFEQ      F8
157800970526     C                   MOVEL     '08'          FUNKEY
157900970526     C                   END
158000970526     C     KEY           IFEQ      F9
158100970526     C                   MOVEL     '09'          FUNKEY
158200970526     C                   END
158300970526     C     KEY           IFEQ      F10
158400970526     C                   MOVEL     '10'          FUNKEY
158500970526     C                   END
158600970526     C     KEY           IFEQ      F11
158700970526     C                   MOVEL     '11'          FUNKEY
158800970526     C                   END
158900970526     C     KEY           IFEQ      F12
159000970526     C                   MOVEL     '12'          FUNKEY
159100970526     C                   END
159200970526     C     KEY           IFEQ      F13
159300970526     C                   MOVEL     '13'          FUNKEY
159400970526     C                   END
159500970526     C     KEY           IFEQ      F14
159600970526     C                   MOVEL     '14'          FUNKEY
159700970526     C                   END
159800970526     C     KEY           IFEQ      F15
159900970526     C                   MOVEL     '15'          FUNKEY
160000970526     C                   END
160100970526     C     KEY           IFEQ      F16
160200970526     C                   MOVEL     '16'          FUNKEY
160300970526     C                   END
160400970526     C     KEY           IFEQ      F17
160500970526     C                   MOVEL     '17'          FUNKEY
160600970526     C                   END
160700970526     C     KEY           IFEQ      F18
160800970526     C                   MOVEL     '18'          FUNKEY
160900970526     C                   END
161000970526     C     KEY           IFEQ      F19
161100970526     C                   MOVEL     '19'          FUNKEY
161200970526     C                   END
161300970526     C     KEY           IFEQ      F20
161400970526     C                   MOVEL     '20'          FUNKEY
161500970526     C                   END
161600970526     C     KEY           IFEQ      F21
161700970526     C                   MOVEL     '21'          FUNKEY
161800970526     C                   END
161900970526     C     KEY           IFEQ      F22
162000970526     C                   MOVEL     '22'          FUNKEY
162100970526     C                   END
162200970526     C     KEY           IFEQ      F23
162300970526     C                   MOVEL     '23'          FUNKEY
162400970526     C                   END
162500970526     C     KEY           IFEQ      F24
162600970526     C                   MOVEL     '24'          FUNKEY
162700970526     C                   END
162800970526     C     KEY           IFEQ      ROLLUP
162900970526     C                   MOVEL     'RU'          FUNKEY
163000970526     C                   END
163100970526     C     KEY           IFEQ      HELP
163200970526     C                   MOVEL     'HP'          FUNKEY
163300970526     C                   END
163400970526     C                   ENDSR
163500970526     C*------------------------------------------------------------------------*
163600970526     C* NOTE COSTRUZIONE SCHIERE
163700970526     C*------------------------------------------------------------------------*
163800970526     C*
163900970526     C* -MSG-MESSAGGI
164000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
164100970526     C* (NON TOCCARE MSG,1 E MSG,2)
164200970526     C*
164300970526     C* -CMD-COMANDI
164400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
164500970526     C*
164600970526     C* -OPD-OPZIONI
164700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
164800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
164900970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
165000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
165100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
165200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
165300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
165400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
165500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
165600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
165700970526     C*
165800970526     C* -FUD-FUNZIONI
165900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
166000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
166100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
166200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
166300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
166400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
166500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
166600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
166700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
166800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
166900970526     O*------------------------------------------------------------------------*
167000970526** DEC - DECODIFICHE
167100970526    MODIFICA                                                      1
167200970526  ANNULLAMENTO                                                    2
167300970526VISUALIZZAZIONE                                                   3
167400021211  INSERIMENTO                                                     4
167500021211     COPIA                                                        5
167600970528** MSG - MESSAGGI
167700021219TISIG4R- Tasto funzionale non gestito                                          1
167800021219TISIG4R- Inserimento effettuato                                                2
167900021220TISIG4R- Codice causale indicato errato                                        3
168000021220TISIG4R- Soggetto movimento errato                                             4
168100021220TISIG4R- Articolo errato                                                       5
168200021219TISIG4R- Richiesta funzione per record inesistente                             6
168300021219TISIG4R- Impossibile inserire, movimento già presente                          7
168400021219TISIG4R- Richiesta funzione per record esistente                               8
168500021220TISIG4R- Data non corretta                                                     9
168600021224TISIG4R- La causale indicata non è di tipo manuale, correggere.                10
168700021224TISIG4R- Indicare la quantità movimentata                                      11
168800021219TISIG4R- Tutti i campi sono obbligatori                                        12
168900070509TISIG4R- Soggetto indicato non in gestione all'utente. Enter per forzare!      13
169000150204TISIG4R- Il cliente/articolo non sono abilitati alla gestione bancali          14
169100150212TISIG4R- Articolo incongruente con anagrafica del cliente                      15
169200150305TISIG4R- Quantità immessa supera la quantità massima calcolata di              16
169300150216TISIG4R- Questo articolo ha un saldo pari a 0 inserimento non possibile        17
169400150223TISIG4R- La causale indicata non è utilizzabile dall'utente.                   18
169500150224TISIG4R- Data movimento incongruente con la validita anagrafica cliente        19
169600150304TISIG4R- Causale non utilizzabile per la movimentazione di questo articolo     20
169700150311TISIG4R- Soggetto o filiale non in gestione all'utente                         21
169800160727TISIG4R- Cancellazione impossibile esiste un record abbinato                   22
169900160727TISIG4R- Cancellazione eseguita con successo                                   23
170000150311TISIG4R-                                                                       24
170100150311TISIG4R-                                                                       25
170200970526** FUD - FUNZIONI
170300970526                   -        (S/N)=N                        funz.01¬
170400970526                   -        (S/N)=N                        funz.02¬
170500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
170600970526                   -        (S/N)=N                        funz.04¬
170700970526                   -        (S/N)=N                        funz.05¬
170800000407F06=Conferma       -        (S/N)=S                        funz.06¬
170900970527                   -        (S/N)=N                        funz.07¬
171000160727F08=Conf.Elimina   -        (S/N)=N                        funz.08¬
171100160727                   -        (S/N)=N                        funz.09¬
171200970527                   -        (S/N)=N                        funz.10¬
171300970526                   -        (S/N)=N                        funz.11¬
171400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
171500970526                   -        (S/N)=N                        funz.13¬
171600970526                   -        (S/N)=N                        funz.14¬
171700970526                   -        (S/N)=N                        funz.15¬
171800970526                   -        (S/N)=N                        funz.16¬
171900970526                   -        (S/N)=N                        funz.17¬
172000970526                   -        (S/N)=N                        funz.18¬
172100970526                   -        (S/N)=N                        funz.19¬
172200970526                   -        (S/N)=N                        funz.20¬
172300970526                   -        (S/N)=N                        funz.21¬
172400970526                   -        (S/N)=N                        funz.22¬
172500970526                   -        (S/N)=N                        funz.23¬
172600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
