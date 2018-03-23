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
032300161006     c                   if        §ggbsco = 'S'
032400161006     c                             and %subst(knmus:1:3) <> 'EDP'
032500161006     c                              or vidca1 = 'TOLE'
032600161005     c                             and %subst(knmus:1:3) <> 'EDP'
032700160803     C                   MOVEL     MSG(22)       PMSG                           *MESSAGGIO
032800160803     C                   MOVEL     MSG(22)       DSMSMS
032900160727     C                   ELSE
033000160803     c     keymgb        chain     azmgb03l
033100160803     c                   if        %found(azmgb03l)
033200160803     c                   delete    azmgb000
033300160803      *
033400160803     C                   MOVEL     MSG(23)       PMSG                           *MESSAGGIO
033500160803     C                   MOVEL     MSG(23)       DSMSMS
033600160803     C                   SETON                                        9192
033700160803     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
033800160803     C                   EXSR      AZZVID
033900160727     C                   ENDIF
034000160727     C                   ENDIF
034100160727     C                   ENDIF
034200160727     C*
034300160727     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500021219     C* DECODI - CONTROLLA CAMPI E DECODIFICA
034600970526     C*------------------------------------------------------------------------*
034700970526     C     DECODI        BEGSR
034800000407     C*
034900000407     C                   MOVEL     '0'           ERRL00
035000000407     C*
035100000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
035200000407     C                   SETOFF                                       808182
035300021224     C                   SETOFF                                       838485
035400021219     C*--------------------
035500021219     C* SCELTA/CONTROLLO CAUSALE
035600021219     C*--------------------
035700021219     C     ERRL00        IFEQ      '0'
035800021223     C                   IF        VIDCA1 <> *blanks
035900021223     C                   EVAL      Chk_DES = *blanks
036000021220     C                   EVAL      Chk_CAU = VIDCA1
036100021220     C                   EXSR      REPCAU
036200021220     C                   EVAL      VIDCA1 = Chk_CAU
036300021220     C                   EVAL      VIDDE1 = Chk_DES
036400021224     C                   EVAL      VIHTI1 = §GGBTIP
036500021224     C                   EVAL      VIHSE1 = §GGBSEG
036600150330     C* Controllo che la causale sia ammessa per il profilo fra sede e filiale
036700150303     C                   IF        ERRL00 = '0' and not *in90
036800150223     c                   select
036900150223     C                   when      §GGBaut = 'S' AND dutlpo <> 'S'
037000150223     C                   EVAL      ERRL00 = '1'
037100150223     C                   SETON                                        809192
037200150223     C                   MOVEL     MSG(18)       DSMSMS
037300150223     C                   when      §GGBaut = 'F' AND dutlpo = 'S'
037400150223     C                   EVAL      ERRL00 = '1'
037500150223     C                   SETON                                        809192
037600150223     C                   MOVEL     MSG(18)       DSMSMS
037700150223     C                   Endsl
037800150223     C                   Endif
037900021224     C* Controllo che la causale movimento inserita dall'utente sia solo d tipo manuale
038000021230     C                   IF        ERRL00 = '0'
038100030109     C                   IF        §GGBTIM <> 'M' AND POP1 = 'F06'
038200021230     C                   EVAL      ERRL00 = '1'
038300021230     C                   SETON                                        809192
038400021230     C                   MOVEL     MSG(10)       DSMSMS
038500021223     C                   ELSE
038600021230     C                   SETOFF                                       91
038700021224     C* Se causale corretta => propongo già (qualora prevista) la cusale movimento correlato
038800021224     C                   IF        §GGBCAC <> *blanks
038900021224     C                   EVAL      Chk_DES = *blanks
039000021224     C                   EVAL      Chk_CAU = §GGBCAC
039100021224     C                   EXSR      REPCAU
039200021224     C                   EVAL      VIDCA2 = Chk_CAU
039300021224     C                   EVAL      VIDDE2 = Chk_DES
039400021224     C                   EVAL      VIHTI2 = §GGBTIP
039500021224     C                   EVAL      VIHSE2 = §GGBSEG
039600021224     C*
039700021223     C                   SETOFF                                       92
039800021230     C                   ELSE
039900021230     C                   SETON                                        92
040000021220     C                   ENDIF
040100021224     C                   ENDIF
040200150223     C*m                 ELSE
040300150223     C*m                 SETON                                        809192
040400150223     C*m                 MOVEL     MSG(3)        DSMSMS
040500021224     C                   ENDIF
040600030107     C*
040700030107     C                   IF        VIHTI1 = 'F'
040800030107     C                   SETOFF                                       55
040900030107     C                   ELSE
041000030107     C                   SETON                                        55
041100030107     C                   ENDIF
041200030107     C                   IF        VIHTI2 = 'F'
041300030107     C                   SETOFF                                       65
041400030107     C                   ELSE
041500030107     C                   SETON                                        65
041600030107     C                   ENDIF
041700030107     C                   ENDIF
041800030107     C                   ENDIF
041900021219     C*--------------------
042000021219     C* SCELTA/CONTROLLO SOGGETTO
042100021219     C*--------------------
042200021219     C     ERRL00        IFEQ      '0'
042300021223     C* Decodifico il tipo di soggetto movimento
042400021224     C                   IF        VIHTI1 = 'C'
042500021223     C                   EVAL      VIDDT1 = 'Cliente ................?:'
042600021223     C                   ENDIF
042700021224     C                   IF        VIHTI1 = 'D'
042800021223     C                   EVAL      VIDDT1 = 'Destinatario ...........?:'
042900021223     C                   ENDIF
043000021224     C                   IF        VIHTI1 = 'F'
043100021223     C                   EVAL      VIDDT1 = 'P.O. ...................?:'
043200021223     C                   ENDIF
043300021223     C* Eseguo i controlli di validità soggetto movimento
043400030107     C                   IF        *IN55   =  *ON
043500030107     C                   IF        VIDSO1K <> *blanks
043600030107     C                   EVAL      Chk_DES =  *blanks
043700030107     C                   MOVEL     *zeros        Chk_SOG
043800030107     C                   MOVE      VIDSO1K       Chk_SOG
043900021220     C*
044000021224     C                   IF        VIHTI1 = 'C'
044100021220     C                   EXSR      REPKSC
044200021220     C                   ENDIF
044300021220     C*
044400021224     C                   IF        VIHTI1 = 'D'
044500021220     C                   EXSR      REPDST
044600021220     C                   ENDIF
044700021220     C*
044800021224     C                   IF        VIHTI1 = 'F'
044900021220     C                   EXSR      REPPO
045000021220     C                   ENDIF
045100021220     C*
045200030107     C                   MOVE(P)   Chk_SOG       VIDSO1K
045300030107     C                   EVAL      VIDDS1  = Chk_DES
045400030107     C                   IF        ERRL00  = '1'
045500021220     C                   SETON                                        81
045600021220     C                   MOVEL     MSG(4)        DSMSMS
045700021220     C                   ENDIF
045800030107     C                   ENDIF
045900030107     C                   ELSE
046000030107     C                   IF        VIDSO1F <> *blanks
046100030107     C                   EVAL      Chk_DES = *blanks
046200030107     C                   MOVEL     *zeros        Chk_SOG
046300030107     C                   MOVE      VIDSO1F       Chk_SOG
046400030107     C*
046500030107     C                   IF        VIHTI1 = 'C'
046600030107     C                   EXSR      REPKSC
046700030107     C                   ENDIF
046800030107     C*
046900030107     C                   IF        VIHTI1 = 'D'
047000030107     C                   EXSR      REPDST
047100030107     C                   ENDIF
047200030107     C*
047300030107     C                   IF        VIHTI1 = 'F'
047400030107     C                   EXSR      REPPO
047500030107     C                   ENDIF
047600030107     C*
047700030107     C                   MOVE(P)   Chk_SOG       VIDSO1F
047800030107     C                   EVAL      VIDDS1  = Chk_DES
047900030107     C                   IF        ERRL00  = '1'
048000030107     C                   SETON                                        81
048100030107     C                   MOVEL     MSG(4)        DSMSMS
048200030107     C                   ENDIF
048300030107     C                   ENDIF
048400030107     C                   ENDIF
048500030107     C                   ENDIF
048600030702     C*--------------------
048700030702     C* CONTROLLO AUTORIZZAZIONI UTENTE SU CODICE SOGGETTO MOVIMENTO "PRINCIPALE"
048800030702     C*--------------------
048900030702     C     ERRL00        IFEQ      '0'
049000040702     C     POP1          IFNE      'O05'
049100030702     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
049200071127     C                   IF        VIHTI1 = 'F'
049300030702     C                   MOVEL     VIDSO1F       WN3               3
049400030702     C                   SETOFF                                           38
049500030702     C     WN3           LOOKUP    L1                                     38
049600030702     C     *IN38         IFEQ      *OFF
049700070509     c     wforza        andeq     *off
049800030702     C                   MOVEL     '1'           ERRL00
049900030702     C                   SETON                                        81
050000150311     C                   MOVEL     MSG(21)       DSMSMS
050100150311     C*m                 MOVEL     MSG(13)       DSMSMS
050200150311     c*m                 eval      wforza = *on
050300030702     C                   ENDIF
050400030702     C                   ENDIF
050500030702     C                   IF        VIHTI1 = 'C'
050600030702     C                   MOVEL     VIDSO1K       WA4               4
050700030702     C                   MOVE      WA4           WN3               3
050800030702     C                   SETOFF                                           38
050900030702     C     WN3           LOOKUP    L1                                     38
051000030702     C     *IN38         IFEQ      *OFF
051100070509     c     wforza        andeq     *off
051200150311     C                   MOVEL     '1'           ERRL00
051300030702     C                   SETON                                        81
051400150311     C                   MOVEL     MSG(21)       DSMSMS
051500150311     C*m                 MOVEL     MSG(13)       DSMSMS
051600150311     c*m                 eval      wforza = *on
051700030702     C                   ENDIF
051800030702     C                   ENDIF
051900040702     C                   ENDIF
052000030702     C                   ENDIF
052100021219     C*--------------------
052200021219     C* SCELTA/CONTROLLO ARTICOLO
052300021219     C*--------------------
052400021219     C     ERRL00        IFEQ      '0'
052500021223     C                   IF        VIDCO1 <> *blanks
052600021223     C                   EVAL      Chk_DES = *blanks
052700021220     C                   EVAL      Chk_COD = VIDCO1
052800021220     C                   EXSR      REPCOD
052900021219     C                   ENDIF
053000021223     C                   ENDIF
053100021224     C*--------------------
053200021224     C* QUANTITA' MOVIMENTATA
053300021224     C*--------------------
053400021224     C     ERRL00        IFEQ      '0'
053500021224     C                   IF        VIDQT1 = *ZEROS
053600021224     C                   ELSE
053700021224     C* A seconda del segno della causale gestisco a programma il segno della quantità
053800021224     C                   IF        VIHSE1 = 'E'
053900021230     C                   EVAL      VIDDQ1 = 'ENTRATA'
054000021230     C                   EVAL      VIDDQ2 = 'USCITA'
054100150223      *se movimento verso il cliente verifico se la quantità è in capienza
054200150223      * la quantità massima evadibile è data dal saldo dei movimenti - la % tolleranza(rotture)
054300150303     c*m                 if        vihti1 = 'C' and §ggbsco = 'S'
054400150305     c                   if        vihti1 = 'C' and not *in90
054500150305     c                   exsr      contrqta
054600150223     C                   endif
054700021224     C                   ENDIF
054800021224     C                   IF        VIHSE1 = 'U'
054900021230     C                   EVAL      VIDDQ1 = 'USCITA'
055000021230     C                   EVAL      VIDDQ2 = 'ENTRATA'
055100021224     C                   ENDIF
055200021230     C                   EVAL      VIDQT1 = %abs(VIDQT1)
055300021230     C                   EVAL      VIDQT2 = %abs(VIDQT1)
055400021224     C*
055500021224     C                   ENDIF
055600021224     C                   ENDIF
055700021219     C*--------------------
055800021220     C* CONTROLLO DATA RIFERIMENTO DOCUMENTO
055900021219     C*--------------------
056000021219     C     ERRL00        IFEQ      '0'
056100021223     C                   IF        VIDDD1 <> *zeros
056200021220     C     *LIKE         DEFINE    VIDDD1        DD1VID
056300021220     C                   Z-ADD     VIDDD1        WDA8
056400021220     C                   Z-ADD     *ZEROS        DA8W
056500021220     C                   EXSR      CHKDA8
056600021220     C                   Z-ADD     WDA8          VIDDD1
056700021220     C                   Z-ADD     DA8W          DD1VID
056800021220     C     ERRL00        IFEQ      '1'
056900021220     C                   SETON                                        85
057000021220     C                   MOVEL     MSG(9)        DSMSMS
057100021220     C                   ENDIF
057200021219     C                   ENDIF
057300021223     C                   ENDIF
057400021220     C*--------------------
057500021220     C* SCELTA/CONTROLLO SOGGETTO MOVIMENTO CORRELATO
057600021220     C*--------------------
057700021220     C     ERRL00        IFEQ      '0'
057800021223     C* Decodifico il tipo di soggetto movimento correlato
057900021224     C                   IF        VIHTI2 = 'C'
058000021223     C                   EVAL      VIDDT2 = 'Cliente ................?:'
058100021223     C                   ENDIF
058200021224     C                   IF        VIHTI2 = 'D'
058300021223     C                   EVAL      VIDDT2 = 'Destinatario ...........?:'
058400021223     C                   ENDIF
058500021224     C                   IF        VIHTI2 = 'F'
058600021223     C                   EVAL      VIDDT2 = 'P.O. ...................?:'
058700021223     C                   ENDIF
058800021223     C* Eseguo i controlli di validità soggetto movimento
058900030107     C                   IF        *IN65   =  *ON
059000030107     C                   IF        VIDSO2K <> *blanks
059100030107     C                   EVAL      Chk_DES =  *blanks
059200030107     C                   MOVEL     *zeros        Chk_SOG
059300030107     C                   MOVE      VIDSO2K       Chk_SOG
059400021220     C*
059500021224     C                   IF        VIHTI2 = 'C'
059600021220     C                   EXSR      REPKSC
059700021220     C                   ENDIF
059800021220     C*
059900021224     C                   IF        VIHTI2 = 'D'
060000021220     C                   EXSR      REPDST
060100021220     C                   ENDIF
060200021220     C*
060300021224     C                   IF        VIHTI2 = 'F'
060400021220     C                   EXSR      REPPO
060500021220     C                   ENDIF
060600021220     C*
060700030107     C                   MOVE(P)   Chk_SOG       VIDSO2K
060800021220     C                   EVAL      VIDDS2 = Chk_DES
060900021220     C                   IF        ERRL00 = '1'
061000021220     C                   SETON                                        84
061100021220     C                   MOVEL     MSG(4)        DSMSMS
061200021220     C                   ENDIF
061300021223     C                   ENDIF
061400030107     C                   ELSE
061500030107     C                   IF        VIDSO2F <> *blanks
061600030107     C                   EVAL      Chk_DES = *blanks
061700030107     C                   MOVEL     *zeros        Chk_SOG
061800030107     C                   MOVE      VIDSO2F       Chk_SOG
061900030107     C*
062000030107     C                   IF        VIHTI2 = 'C'
062100030107     C                   EXSR      REPKSC
062200030107     C                   ENDIF
062300030107     C*
062400030107     C                   IF        VIHTI2 = 'D'
062500030107     C                   EXSR      REPDST
062600030107     C                   ENDIF
062700030107     C*
062800030107     C                   IF        VIHTI2 = 'F'
062900030107     C                   EXSR      REPPO
063000030107     C                   ENDIF
063100030107     C*
063200030107     C                   MOVE(P)   Chk_SOG       VIDSO2F
063300030107     C                   EVAL      VIDDS2  = Chk_DES
063400030107     C                   IF        ERRL00  = '1'
063500030107     C                   SETON                                        84
063600030107     C                   MOVEL     MSG(4)        DSMSMS
063700030107     C                   ENDIF
063800030107     C                   ENDIF
063900030107     C                   ENDIF
064000030107     C                   ENDIF
064100150226     C                   IF        ERRL00 = '0' and pop1 = 'F06' and
064200150226     C                             VIHTI2 = 'C'
064300150226     c                   movel     vidco1        wtsc
064400150225     c     kcgb          setgt     azcgb01l
064500150225     c     kcgb          readpe    azcgb01l
064600150224     c                   if        %eof(azcgb01l)
064700150204     c                   move      '1'           errl00
064800150204     c                   seton                                        8182
064900150204     C                   MOVEL     MSG(14)       DSMSMS
065000150204     c                   endif
065100150204     c                   endif
065200000407     C*--------------------
065300000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
065400000407     C*--------------------
065500000407     C     ERRL00        IFEQ      '0'
065600021230     C     ERRL00        IFEQ      '0'
065700021219     C     VIDCA1        IFEQ      *BLANKS
065800000407     C                   MOVEL     '1'           ERRL00
065900000407     C                   SETON                                        80
066000021219     C                   ENDIF
066100021230     C                   ENDIF
066200021219     C     ERRL00        IFEQ      '0'
066300030107     C                   IF        (VIHTI1 =  'F' AND (VIDSO1F = *BLANKS  OR
066400030107     C                                                 VIDSO1F = *ZEROS)) OR
066500030107     C                             (VIHTI1 <> 'F' AND (VIDSO1K = *BLANKS  OR
066600030107     C                                                 VIDSO1K = *ZEROS))
066700000407     C                   MOVEL     '1'           ERRL00
066800000407     C                   SETON                                        81
066900000407     C                   ENDIF
067000021219     C                   ENDIF
067100021219     C     ERRL00        IFEQ      '0'
067200021219     C     VIDCO1        IFEQ      *BLANKS
067300000407     C                   MOVEL     '1'           ERRL00
067400000407     C                   SETON                                        82
067500000407     C                   ENDIF
067600021219     C                   ENDIF
067700021219     C     ERRL00        IFEQ      '0'
067800021219     C     VIDQT1        IFEQ      *ZEROS
067900021219     C                   MOVEL     '1'           ERRL00
068000021219     C                   SETON                                        83
068100021219     C                   ENDIF
068200021219     C                   ENDIF
068300021219     C     ERRL00        IFEQ      '0'
068400030107     C                   IF        ((VIHTI2 =  'F' AND (VIDSO2F = *BLANKS   OR
068500030107     C                                                  VIDSO2F = *ZEROS))  OR
068600030107     C                              (VIHTI2 <> 'F' AND (VIDSO2K = *BLANKS   OR
068700030107     C                                                  VIDSO2K = *ZEROS))) AND
068800030107     C                             *IN92 = *OFF
068900021219     C                   MOVEL     '1'           ERRL00
069000021219     C                   SETON                                        84
069100021219     C                   ENDIF
069200021219     C                   ENDIF
069300021220     C     ERRL00        IFEQ      '1'
069400000407     C                   MOVEL     MSG(12)       DSMSMS
069500000407     C                   ENDIF
069600021220     C                   ENDIF
069700021223     C*
069800021230     C                   EVAL      VIDRD2 = VIDRD1
069900021230     C                   EVAL      VIDDD2 = VIDDD1
070000030612     C                   IF        POP1 = 'F06'
070100021223     C                   EVAL      VIDUT1 = DUTUTE
070200030711     C                   EVAL      VIDPO1 = DUTPOU
070300030711     C                   EVAL      VIDDA1 = DATCOR
070400030711     C                   EVAL      VIDOR1 = ORACOR
070500030612     C                   ENDIF
070600030711     C                   EVAL      VIDUT2 = VIDUT1
070700021230     C                   EVAL      VIDPO2 = VIDPO1
070800021223     C                   EVAL      VIDDA2 = VIDDA1
070900021223     C                   EVAL      VIDOR2 = VIDOR1
071000071127     C*
071100071127     C                   IF        ERRL00 = '1'
071200071127     C                   EVAL      wforza = *off
071300071127     C                   ENDIF
071400030109     C*
071500970528     C                   ENDSR
071600150213     C*------------------------------------------------------------------------*
071700150213     C* contrqta controllo per i clienti se la quantità è evadibile
071800150213     C*------------------------------------------------------------------------*
071900150213     C     contrqta      BEGSR
072000150223     c                   clear                   saldoc            7 0
072100150213     c                   clear                   saldo             7 0
072200150213     c                   clear                   franchigia        7 0
072300150213      * cerca il saldo dei movimenti per cliente dalla data dell'ultimo movimento
072400150213      *
072500150213     C/EXEC SQL
072600150213     C+ select sum(mgbqta) into :saldo from azmgb00f
072700150216     C+ where mgbsog = :vidso1k  and mgbcod = :vidco1 and mgbtip = 'C'
072800150213     C/END-EXEC
072900150223     c                   if        saldo = 0
073000150216     C                   MOVEL     '1'           ERRL00
073100150216     c                   seton                                        83
073200150216     c                   eval      dsmsms = msg(17)
073300150216     c                   endif
073400150216      * saldo maggiore di zero verifico se quantità richiesta è in capienza
073500150303     c                   if        §ggbsco = 'S'
073600150223     c                   if        saldo <> 0 and cgbsco > 0
073700150414     c                   eval(h)   franchigia = (saldo * cgbsco) /100
073800150213     c                   eval      saldo = saldo - franchigia
073900150303     c                   endif
074000150303     c                   endif
074100150223     c                   if        saldo < 0
074200150223     c                   eval      saldoc = saldo + vidqt1
074300150223     c                   else
074400150507     c                   eval      saldoc =  saldo
074500150507     c*                  eval      saldoc = vidqt1 - saldo
074600150223     c                   endif
074700150223     c                   if        saldoc > 0
074800150216     C                   MOVEL     '1'           ERRL00
074900150216     c                   seton                                        83
075000150223     c                   eval      dsmsms = %trim(msg(16)) + %editc(saldoc:'Z')
075100150213     c                   endif
075200150213     c                   endsr
075300970526     C*------------------------------------------------------------------------*
075400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
075500970526     C*------------------------------------------------------------------------*
075600970526     C     AZZVID        BEGSR
075700000407     C*
075800021219     C                   MOVEL     *BLANKS       VIDCA1
075900021219     C                   MOVEL     *BLANKS       VIDDE1
076000021219     C                   MOVEL     *BLANKS       VIDDT1
076100030107     C                   MOVEL     *BLANKS       VIDSO1F
076200030107     C                   MOVEL     *BLANKS       VIDSO1K
076300021219     C                   MOVEL     *BLANKS       VIDDS1
076400021219     C                   MOVEL     *BLANKS       VIDCO1
076500021219     C                   MOVEL     *BLANKS       VIDDC1
076600021219     C                   Z-ADD     *ZEROS        VIDQT1
076700021220     C                   MOVEL     *BLANKS       VIDRD1
076800021220     C                   Z-ADD     *ZEROS        VIDDD1
076900021219     C                   MOVEL     *BLANKS       VIDUT1
077000030711     C                   Z-ADD     *ZEROS        VIDPO1
077100021219     C                   Z-ADD     *ZEROS        VIDDA1
077200021219     C                   Z-ADD     *ZEROS        VIDOR1
077300021219     C*
077400021219     C                   MOVEL     *BLANKS       VIDCA2
077500021219     C                   MOVEL     *BLANKS       VIDDE2
077600021219     C                   MOVEL     *BLANKS       VIDDT2
077700030107     C                   MOVEL     *BLANKS       VIDSO2K
077800030107     C                   MOVEL     *BLANKS       VIDSO2F
077900021219     C                   MOVEL     *BLANKS       VIDDS2
078000021219     C                   MOVEL     *BLANKS       VIDCO2
078100021219     C                   MOVEL     *BLANKS       VIDDC2
078200021219     C                   Z-ADD     *ZEROS        VIDQT2
078300021220     C                   MOVEL     *BLANKS       VIDRD2
078400021220     C                   Z-ADD     *ZEROS        VIDDD2
078500021219     C                   MOVEL     *BLANKS       VIDUT2
078600021219     C                   Z-ADD     *ZEROS        VIDDA2
078700021219     C                   Z-ADD     *ZEROS        VIDOR2
078800071127     C*
078900071127     C* Spengo il flag di forzatura messaggio
079000071127     C                   eval      wforza = *off
079100071127     C
079200970526     C*
079300970526     C                   ENDSR
079400000407     C*------------------------------------------------------------------------*
079500021219     C* IMPCAM - IMPOSTA I CAMPI DAL VIDEO ALLA DS DEL "DRIVER" MOVIMENTI GABBIE/BANCALI
079600000407     C*------------------------------------------------------------------------*
079700000407     C     IMPCAM        BEGSR
079800021219     C*
079900021219     C                   CLEAR                   TISIG0DS
080000021219     C                   Z-ADD     DATCOR        IG0DAT
080100021219     C                   Z-ADD     ORACOR        IG0ORA
080200021230     C                   MOVEL     DUTUTE        IG0UTE
080300021230     C                   Z-ADD     DUTPOU        IG0POU
080400021219     C                   MOVEL     VIDCA1        IG0CAU
080500030107     C                   IF        VIHTI1 = 'F'
080600030107     C                   MOVEL     *zeros        IG0SOG
080700030107     C                   MOVE      VIDSO1F       IG0SOG
080800030107     C                   ELSE
080900030107     C                   MOVEL     *zeros        IG0SOG
081000030107     C                   MOVE      VIDSO1K       IG0SOG
081100030107     C                   ENDIF
081200021219     C                   MOVEL     VIDCO1        IG0COD
081300021219     C                   Z-ADD     VIDQT1        IG0QTA
081400021230     C                   MOVEL     VIDRD1        IG0RDO
081500060208     C*
081600060208     C                   Z-ADD     VIDDD1        G08DAT
081700060208     C                   Z-ADD     *ZEROS        G08INV
081800060208     C                   MOVEL     '0'           G08ERR
081900060208     C                   CALL      'XSRDA8'
082000060208     C                   PARM                    WLBDA8
082100060208     C                   Z-ADD     G08INV        IG0DDO
082200060208     C*
082300030107     C                   IF        VIHTI2 = 'F'
082400030107     C                   MOVEL     *zeros        IG0SMC
082500030107     C                   MOVE      VIDSO2F       IG0SMC
082600030107     C                   ELSE
082700030107     C                   MOVEL     *zeros        IG0SMC
082800030107     C                   MOVE      VIDSO2K       IG0SMC
082900030107     C                   ENDIF
083000000407     C*
083100000407     C                   ENDSR
083200030123     C*------------------------------------------------------------------------*
083300030123     C* REPCAU - CONTROLLA CAUSALE MOVIMENTO
083400030123     C*------------------------------------------------------------------------*
083500030123     C     REPCAU        BEGSR
083600030123     C*
083700030123     C     *LIKE         DEFINE    mgbCAU        Chk_CAU
083800030123     C     *LIKE         DEFINE    VIDDE1        Chk_DES
083900030123     C*
084000030123     C* RICHIESTO ELENCO
084100030123     C                   SETOFF                                           28
084200030123     C     '?'           SCAN      Chk_CAU                                28
084300030123     C     *IN28         IFEQ      *ON
084400030123     C*
084500030123     C* VALORIZZO I PARAMETRI DI PASSAGGIO
084600030123     C                   CLEAR                   PARAM2
084700030123     C                   EVAL      P2OP1 = 'P1M'
084800030123     C                   MOVEL     PARAM2        KPJBU
084900030123     C                   CALL      'TNTB31R'
085000030123     C                   PARM                    KPJBA
085100030123     C*
085200030123     C                   MOVEL     KPJBU         PARAM2
085300030123     C                   MOVEL     P2CAU         Chk_CAU
085400030123     C                   ENDIF
085500030123     C*
085600030123     C* IN OGNI CASO CONTROLLO ESISTENZA CODICE CAUSALE
085700030123     C                   EVAL      tbeCOD = 'GGB'
085800030123     C                   EVAL      tbeKE1 = Chk_CAU
085900030123     C     KEYTBEP       CHAIN     tntbe01l
086000030123     C                   IF        %found(tntbe01l)
086100030123     C                   CLEAR                   DGGB
086200030123     C                   MOVEL(P)  tbeUNI        DGGB
086300030123     C                   MOVEL(P)  §GGBDES       Chk_DES
086400030123     C                   ELSE
086500030123     C                   MOVEL     '1'           ERRL00
086600030123     C                   MOVEL(P)  *all'*'       Chk_DES
086700030123     C                   ENDIF
086800030123     C*
086900030123     C                   ENDSR
087000021220     C*------------------------------------------------------------------------*
087100021220     C* REPKSC - CONTROLLA CODICE CLIENTE
087200021220     C*------------------------------------------------------------------------*
087300021220     C     REPKSC        BEGSR
087400021220     C*
087500021220     C     *LIKE         DEFINE    mgbSOG        Chk_SOG
087600021220     C*
087700021220     C* RICHIESTO ELENCO
087800021220     C                   SETOFF                                           28
087900021220     C     '?'           SCAN      Chk_SOG                                28
088000021220     C     *IN28         IFEQ      *ON
088100021220     C*
088200021220     C* RICHIESTO ELENCO CLIENTI
088300150212     C                   MOVEL     KPJBU         SAVKPJBU
088400150212     C                   MOVEL(P)  'P01'         KPJBU
088500150212     C                   CALL      'TISIG10R'
088600150212     C                   PARM                    KPJBA
088700150212     C                   EVAL      Chk_sog = %subst(KPJBU:4:8)
088800150212     C                   MOVEL     SAVKPJBU      KPJBU
088900021220     C*
089000021220     C* CONTROLLA ESISTENZA CLIENTE
089100021220     C                   ENDIF
089200150304     C                   TESTN                   chk_sog              30
089300150304     C                   MOVE      chk_sog       W001A             1
089400150304    2C     *IN30         IFEQ      *OFF
089500150304     C     W001A         ORLT      '0'
089600150304     C                   MOVEL     '1'           ERRL00
089700150304     C                   SETON                                        81
089800150304     C                   leavesr
089900150304     c                   endif
090000150212     C                   EXSR      DECTIB
090100021220     C*
090200021220     C                   ENDSR
090300021220     C*------------------------------------------------------------------------*
090400021220     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
090500021220     C*------------------------------------------------------------------------*
090600021220     C     DECTIB        BEGSR
090700021220     C*
090800021220     C                   CLEAR                   BS69DS
090900021220     C                   CLEAR                   ACODS
091000021220     C                   MOVEL     KNSIF         I69SIF
091100021220     C                   MOVE(P)   Chk_SOG       I69KAC
091200021220     C                   CALL      'TIBS69R'
091300021220     C                   PARM                    BS69DS
091400021220     C                   PARM                    ACODS
091500021220     C                   PARM                    INDDS
091600021220     C                   PARM                    CLPDS
091700021220     C                   PARM                    CLSDS
091800021220     C     O69ERR        IFNE      '1'
091900021223     C                   EVAL      Chk_DES = acoRAG
092000021220     C                   ELSE
092100021220     C                   MOVEL     '1'           ERRL00
092200021220     C                   ENDIF
092300021220     C*
092400021220     C                   ENDSR
092500021220     C*------------------------------------------------------------------------*
092600021220     C* REPDST - CONTROLLA CODICE DESTINATARIO
092700021220     C*------------------------------------------------------------------------*
092800021220     C     REPDST        BEGSR
092900021220     C*
093000021220     C                   MOVEL     *all'0'       WrkDST_A          7
093100021220     C                   Z-ADD     *zeros        WrkDST_N          7 0
093200021220     C*
093300021220     C* RICHIESTO ELENCO
093400021220     C                   SETOFF                                           28
093500021220     C     '?'           SCAN      Chk_SOG                                28
093600021220     C     *IN28         IFEQ      *ON
093700021220     C*
093800021220     C* RICHIESTO ELENCO DESTINATARI
093900021220     C                   MOVEL     KPJBU         SAVKPJBU
094000021220     C                   MOVEL(P)  'P01'         KPJBU
094100021220     C                   CALL      'TISIT1R'
094200021220     C                   PARM                    KPJBA
094300021220     C                   EVAL      WrkDST_A = %subst(KPJBU:4:7)
094400021220     C                   MOVEL     *all'0'       Chk_SOG
094500021220     C                   MOVE      WrkDST_A      Chk_SOG
094600021220     C                   MOVEL     SAVKPJBU      KPJBU
094700021220     C                   ELSE
094800021220     C                   MOVE(P)   Chk_SOG       WrkDST_N
094900021220     C     WrkDST_N      CHAIN     azdst04l
095000021220     C                   IF        %found(azdst04l)
095100021220     C                   EVAL      Chk_DES = dstRAG
095200021220     C                   ELSE
095300021220     C                   EVAL      ERRL00 = '1'
095400021220     C                   EVAL      Chk_DES = *all'*'
095500021220     C                   ENDIF
095600021220     C                   ENDIF
095700021220     C*
095800021220     C                   ENDSR
095900021220     C*------------------------------------------------------------------------*
096000021220     C* REPPO - CONTROLLA CODICE P.O.
096100021220     C*------------------------------------------------------------------------*
096200021220     C     REPPO         BEGSR
096300021220     C*
096400021220     C                   Z-ADD     *zeros        WrkPO_N           3 0
096500021220     C*
096600021220     C* RICHIESTO ELENCO
096700021220     C                   SETOFF                                           28
096800021220     C     '?'           SCAN      Chk_SOG                                28
096900021220     C     *IN28         IFEQ      *ON
097000021220     C*
097100021220     C* RICHIESTO ELENCO P.O.
097200021220     C                   CALL      'TNSD24R'
097300021220     C                   PARM                    §COD              3
097400021220     C                   PARM      'F'           §TIP              1
097500021220     C                   PARM                    §§DES            25
097600021220     C                   PARM                    §§PRA             3
097700021220     C                   MOVEL     *all'0'       Chk_SOG
097800021220     C                   MOVE      §COD          Chk_SOG
097900021220     C                   ELSE
098000021220     C                   MOVE      Chk_SOG       WrkPO_N
098100021220     C     WrkPO_N       CHAIN     azorg01l
098200021220     C                   IF        %found(azorg01l)
098300021220     C                   EVAL      Chk_DES = orgDES
098400021220     C                   ELSE
098500021220     C                   EVAL      ERRL00 = '1'
098600021220     C                   EVAL      Chk_DES = *all'*'
098700021220     C                   ENDIF
098800021220     C                   ENDIF
098900021220     C*
099000021220     C                   ENDSR
099100021220     C*------------------------------------------------------------------------*
099200021220     C* REPCOD - CONTROLLA CODICE ARTICOLO
099300021220     C*------------------------------------------------------------------------*
099400021220     C     REPCOD        BEGSR
099500021220     C*
099600021220     C     *LIKE         DEFINE    mgbCOD        Chk_COD
099700021220     C*
099800021220     C* RICHIESTO ELENCO
099900021220     C                   SETOFF                                           28
100000021220     C     '?'           SCAN      Chk_COD                                28
100100021220     C     *IN28         IFEQ      *ON
100200021220     C*
100300021220     C* RICHIESTO ELENCO ARTICOLI
100400021220     C                   MOVEL     KPJBU         SAVKPJBU
100500021220     C                   MOVEL(P)  'P01'         KPJBU
100600021220     C                   CALL      'TISIG1R'
100700021220     C                   PARM                    KPJBA
100800021220     C                   EVAL      Chk_COD = %subst(KPJBU:4:15)
100900150226     C                   movel     Chk_COD       vidco1
101000021220     C                   MOVEL     SAVKPJBU      KPJBU
101100021220     C                   ELSE
101200021220     C     Chk_COD       CHAIN     azagb01l
101300021220     C                   IF        %found(azagb01l)
101400021220     C                   EVAL      Chk_DES = agbDES
101500021220     C                   ELSE
101600150213     C                   SETON                                        82
101700150213     C                   MOVEL     MSG(05)       DSMSMS
101800021220     C                   EVAL      ERRL00 = '1'
101900021220     C                   EVAL      Chk_DES = *all'*'
102000021220     C                   ENDIF
102100021220     C                   ENDIF
102200150304      *
102300150304     c                   movel     vidco1        wtsc
102400150212     C                   IF        errl00 = '0'
102500150303     C                   IF        VIHTI1 = 'C'
102600150225     c     kcgb          setgt     azcgb01l
102700150225     c     kcgb          readpe    azcgb01l
102800150303     C                   IF        %eof(azcgb01l)
102900150303     C                   EVAL      ERRL00 = '1'
103000150303     C                   SETON                                        82
103100150303     C                   MOVEL     MSG(15)       DSMSMS
103200150303     C                   ENDIF
103300150224     C                   if        Datcor < cgbdti or Datcor > cgbdtf
103400150224     C                   EVAL      ERRL00 = '1'
103500150224     C                   SETON                                        82
103600150224     C                   MOVEL     MSG(19)       DSMSMS
103700150224     C                   ENDIF
103800150212     C                   IF        chk_cod <> cgbcod1 and
103900150212     C                             chk_cod <> cgbcod2 and
104000150212     C                             chk_cod <> cgbcod3
104100150212     C                   EVAL      ERRL00 = '1'
104200150212     C                   SETON                                        82
104300150212     C                   MOVEL     MSG(15)       DSMSMS
104400150212     C                   ENDIF
104500150212     C                   ENDIF
104600150303     C                   ENDIF
104700150304     C                   IF        errl00 = '0'
104800150304     C                   IF        §ggbtsc <> *blank and
104900150304     c                             §ggbtsc <> wtsc
105000150304     C                   EVAL      ERRL00 = '1'
105100150304     C                   SETON                                        82
105200150304     C                   MOVEL     MSG(20)       DSMSMS
105300150304     C                   ENDIF
105400150304     C                   ENDIF
105500021220     C*
105600021220     C                   ENDSR
105700021220     C*------------------------------------------------------------------------*
105800021220     C* CHKDA8 - CONTROLLA LA DATA (8)
105900021220     C*------------------------------------------------------------------------*
106000021220     C     CHKDA8        BEGSR
106100021220     C*
106200021223     C     *LIKE         DEFINE    VIDDD1        WDA8
106300021223     C     *LIKE         DEFINE    VIDDD1        DA8W
106400021220     C*
106500021220     C                   Z-ADD     WDA8          G08DAT
106600021220     C                   Z-ADD     *ZEROS        G08INV
106700021220     C                   MOVEL     '0'           G08ERR
106800021220     C                   CALL      'XSRDA8'
106900021220     C                   PARM                    WLBDA8
107000021220     C                   Z-ADD     G08DAT        WDA8
107100021220     C                   Z-ADD     G08INV        DA8W
107200021220     C     G08ERR        IFEQ      '1'
107300021220     C                   MOVEL     '1'           ERRL00
107400021220     C                   ENDIF
107500021220     C*
107600021220     C                   ENDSR
107700021219     C*------------------------------------------------------------------------*
107800021220     C* LETMOV - LEGGE I 2 RECORD DEL MOVIMENTO
107900021219     C*------------------------------------------------------------------------*
108000021219     C     LETMOV        BEGSR
108100021219     C*
108200021219     C* 1° MOVIMENTO
108300160727     C     KEYMGB        CHAIN(n)  AZMGB03L                           99
108400021219     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
108500021219     C                   MOVEL     '1'           ERRL00                         *ERRORE
108600021219     C                   MOVEL     MSG(6)        PMSG                           *MESSAGGIO
108700030107     C                   MOVEL     MSG(6)        DSMSMS
108800021219     C                   MOVEL     '1'           FINL00                         *FINE PGM
108900021219     C                   ELSE
109000021219     C                   MOVEL     mgbCAU        VIDCA1
109100030107     C                   IF        mgbTIP = 'F'
109200030107     C                   SETOFF                                       55
109300030107     C                   MOVE(P)   mgbSOG        VIDSO1F
109400030107     C                   ELSE
109500030107     C                   SETON                                        55
109600030107     C                   MOVE(P)   mgbSOG        VIDSO1K
109700030107     C                   ENDIF
109800021219     C                   MOVEL     mgbCOD        VIDCO1
109900021219     C                   Z-ADD     mgbQTA        VIDQT1
110000021219     C                   MOVEL     mgbUTE        VIDUT1
110100030711     C                   Z-ADD     mgbPOU        VIDPO1
110200021219     C                   Z-ADD     mgbDAT        VIDDA1
110300021219     C                   Z-ADD     mgbORA        VIDOR1
110400030122     C                   Z-ADD     mgbPRG        VIDPRG1
110500030311     C                   MOVEL     mgbRDO        VIDRD1
110600071218     C*
110700071218     C                   Z-ADD     mgbDDO        G08INV
110800071218     C                   Z-ADD     *ZEROS        G08DAT
110900071218     C                   MOVEL     '3'           G08ERR
111000071218     C                   CALL      'XSRDA8'
111100071218     C                   PARM                    WLBDA8
111200071218     C                   Z-ADD     G08DAT        VIDDD1
111300021219     C                   ENDIF
111400021219     C*
111500021219     C* 2° MOVIMENTO
111600160727     C     KEYMGB        READE(n)  AZMGB03L                               99
111700021219     C     *IN99         IFEQ      *OFF                                         *ESISTENTE
111800021219     C                   MOVEL     mgbCAU        VIDCA2
111900030107     C                   IF        mgbTIP = 'F'
112000030107     C                   SETOFF                                       65
112100030107     C                   MOVE(P)   mgbSOG        VIDSO2F
112200030107     C                   ELSE
112300030107     C                   SETON                                        65
112400030107     C                   MOVE(P)   mgbSOG        VIDSO2K
112500030107     C                   ENDIF
112600021219     C                   MOVEL     mgbCOD        VIDCO2
112700021219     C                   Z-ADD     mgbQTA        VIDQT2
112800021219     C                   MOVEL     mgbUTE        VIDUT2
112900021219     C                   Z-ADD     mgbDAT        VIDDA2
113000021219     C                   Z-ADD     mgbORA        VIDOR2
113100030122     C                   Z-ADD     mgbPRG        VIDPRG2
113200030311     C                   MOVEL     mgbRDO        VIDRD2
113300071218     C*
113400071218     C                   Z-ADD     mgbDDO        G08INV
113500071218     C                   Z-ADD     *ZEROS        G08DAT
113600071218     C                   MOVEL     '3'           G08ERR
113700071218     C                   CALL      'XSRDA8'
113800071218     C                   PARM                    WLBDA8
113900071218     C                   Z-ADD     G08DAT        VIDDD2
114000021219     C                   ENDIF
114100021219     C*
114200021219     C                   ENDSR
114300970526     C*------------------------------------------------------------------------*
114400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
114500970526     C*------------------------------------------------------------------------*
114600970526     C     IMPFUN        BEGSR
114700970526     C*
114800970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
114900021211     C     POP1          IFEQ      'O05'
115000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
115100970526     C                   MOVEL     'N'           FUDATT
115200970526     C                   MOVEL     FUDRIG        FUD(6)
115300021211     C                   ENDIF
115400160727     C* SE cancellazione   TOGLIE LA FUNZIONE DI AGGIORNAMENTO
115500160727     C     POP1          IFEQ      'O04'
115600160727     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
115700160727     C                   MOVEL     'N'           FUDATT
115800160727     C                   MOVEL     FUDRIG        FUD(6)
115900160727     C                   MOVEL     FUD(8)        FUDRIG                         *AGGIORNAMENTO
116000160727     C                   MOVEL     'S'           FUDATT
116100160727     C                   MOVEL     FUDRIG        FUD(8)
116200160727     C                   ENDIF
116300970526     C*
116400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
116500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
116600970526     C                   EXSR      FUNGES
116700970526     C*
116800000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
116900021219     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
117000021219     C                   EXSR      LETMOV
117100980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
117200980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
117300000407     C                   ELSE                                                   *INSERIMENTO
117400970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
117500000407     C                   ENDIF
117600970526     C*
117700021211     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
117800021211     C     POP1          IFEQ      'O02'
117900021211     C                   MOVEL     DEC(1)        VIDFUN
118000021211     C                   ENDIF
118100021211     C     POP1          IFEQ      'O03'
118200021211     C                   MOVEL     DEC(5)        VIDFUN
118300021211     C                   ENDIF
118400021211     C     POP1          IFEQ      'O04'
118500021211     C                   MOVEL     DEC(2)        VIDFUN
118600021211     C                   ENDIF
118700021211     C     POP1          IFEQ      'O05'
118800021211     C                   MOVEL     DEC(3)        VIDFUN
118900021211     C                   ENDIF
119000021211     C     POP1          IFEQ      'F06'
119100021211     C                   MOVEL     DEC(4)        VIDFUN
119200021211     C                   ENDIF
119300021211     C*
119400021211     C                   ENDSR
119500021219     C*--------------------------------------------------------------------------------------------*
119600021219     C* REPERISCE I DATI UTENTE
119700021219     C*--------------------------------------------------------------------------------------------*
119800021219     C     REPDATIUTE    BEGSR
119900021219     C*
120000021219     C* INIZIALIZZA VARIABILI DI WRK
120100021219     C                   CLEAR                   TIBS34DS
120200021219     C                   CLEAR                   AZUTEDS
120300021219     C                   CLEAR                   DDATIUTE
120400021219     C*
120500021219     C     *DTAARA       DEFINE    §azute        azuteds
120600021219     C     *DTAARA       DEFINE    §datiute      ddatiute
120700021219     C                   IN(E)     *DTAARA
120800021219     C                   IF        %Error
120900021220     C                   EVAL      I34Tla = 'L'
121000021219     C                   CALL      'TIBS34R'
121100021219     C                   PARM                    Tibs34Ds
121200021219     C                   IN        *DTAARA
121300021219     C                   ENDIF
121400030702     C*
121500030702     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
121600030702     C                   CLEAR                   L1
121700030702     C                   CLEAR                   WAbi              2
121800150311     c                   Clear                   dLat
121900150311      * Verifica errori e autorità profilo
122000150311s   1c                   Select
122100150311      * se ho errori nei dati utente esco dal pgm
122200150311w   1c                   When      DutErr = 'E'
122300150311     c                   seton                                        lr
122400150311     c                   return
122500150311      * se non c'è l'abilitazione
122600150311      * --> se 1° livello, abilitazioni al terminal
122700150311      *     se 2° livello, abilitazioni al punto operativo
122800150311w   1c                   When      UteAut = *Blanks
122900150311if  2c                   If        DutLpo = '1'
123000150311     c                   Eval      wabi   = 'TP'
123100150311e   2c                   EndIf
123200150311if  2c                   If        DutLpo = '2'
123300150311     c                   Eval      wabi   = 'PO'
123400150311e   2c                   EndIf
123500150311if  2c                   If        DutLpo = 'S'
123600150311     c                   Eval      wabi   = 'AZ'
123700150311e   2c                   EndIf
123800150311      * carica le abilitazioni del profilo
123900150311x   1c                   Other
124000150311     c                   Movel     UteFaf        Dute01
124100150311     c                   If        §Utemgb <> *Blanks
124200150311     c                   Eval      wabi = §Utemgb
124300150311     c                   Else
124400150311     c                   Eval      wabi = UteAut
124500150311     c                   EndIf
124600150311e   1c                   EndSl
124700150311      * controllo se ok l'abilitazione dell'utente
124800150311     c                   Clear                   Tibs02ds
124900150311     c                   Eval      T02Mod = 'C'
125000150311     c                   Eval      T02Sif = knsif
125100150311     c                   Eval      T02Cod = 'LAT'
125200150311     c                   Movel(p)  wabi          T02Ke1
125300150311     c                   Call      'TIBS02R'
125400150311     c                   Parm                    kpjba
125500150311     c                   Parm                    Tibs02ds
125600150311if  1c                   If        T02Err = *Blanks
125700150311     c                   Eval      dLat = T02Uni
125800150311e   1c                   EndIf
125900150311      * errore o non abilitato (impossibile xchè controllato nel filtro)
126000150311if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
126100150311   x1c                   Else
126200150311      * Reperimento dei P.O. gestibili dall'utente
126300150311     c                   clear                   TRUL31DS
126400150311     c                   clear                   TRUL31DS2
126500150311     c                   eval      I31abi = wabi
126600150311     c                   eval      I31cdi = DUTdis
126700150311     c                   eval      I31car = DUTare
126800150311     c                   eval      I31cpo = DUTpou
126900150311     c                   call      'TRUL31R'
127000150311     c                   parm                    KPJBA
127100150311     c                   parm                    TRUL31DS
127200150311     c                   parm                    TRUL31DS2
127300150311if  2c                   if        O31pog > *zeros
127400150311     c                   movea     O31pog        L1
127500150311     c                   movea     O31arg        $arg
127600150311     c                   movea     O31dig        $dig
127700150311x   2c                   else
127800150311     c* impossibile perchè controllato nel filtrp
127900150311     c                   seton                                        lr
128000150311     c                   return
128100150311e   2c                   endif
128200150311    1c                   endif
128300021219     C*
128400021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
128500021219     C                   MOVEL(P)  RSUT          DSFIRS           20
128600021219     C*
128700021219     C                   ENDSR
128800970526     C*------------------------------------------------------------------------*
128900970526     C* *INZSR - OPERAZIONI INIZIALI
129000970526     C*------------------------------------------------------------------------*
129100970526     C     *INZSR        BEGSR
129200970526     C*---
129300970526     C* RICEVIMENTO PARAMETRI
129400970526     C*---
129500970526     C     *ENTRY        PLIST
129600970612     C                   PARM                    KPJBA
129700000407     C                   MOVEL     KPJBU         PARAM
129800021220     C*
129900021220     C* DICHIARAZIONE VARIABILI DI WRK
130000021220     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
130100021219     C*
130200021219     C* REPERISCE I DATI DELL'UTENTE
130300021219     C                   EXSR      REPDATIUTE
130400000407     C*
130500000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
130600000407     C     POP1          IFEQ      'O05'
130700160727     C     POP1          orEQ      'O04'
130800021230     C                   SETON                                        90
130900000407     C                   ENDIF
131000021219     C     POP1          IFEQ      'F06'
131100021223     C                   SETON                                        9192
131200000407     C                   ENDIF
131300021219     C*---
131400021219     C* INIZIALIZZAZIONE CAMPI DI WRK
131500021219     C*---
131600021219     C                   Z-ADD     1             ACOKUT
131700021219     C                   Z-ADD     151           ACOKCC
131800150303     C*
131900150303     C* VALORIZZO I DATI RELATIVI AD UTENTE MODIFICA/DATA MODIFICA/ORA MODIFICA
132000150303     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
132100150303     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
132200150303     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
132300150303     C                   Z-ADD     WN8           G08DAT
132400150303     C                   Z-ADD     *ZEROS        G08INV
132500150303     C                   MOVEL     '0'           G08ERR
132600150303     C                   CALL      'XSRDA8'
132700150303     C                   PARM                    WLBDA8
132800150303     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
132900021219     C*---
133000021219     C* CHIAVI DI LETTURA
133100021219     C*---
133200030122     C* LETTURA AZMGB03L - COMPLETA
133300021219     C     KEYMGB        KLIST
133400030122     C                   KFLD                    PPRG                           *PROGRESSIVO UNIVOCO
133500150225     C* LETTURA AZcgb01L - COMPLETA
133600150225     C     Kcgb          KLIST
133700150225     C                   KFLD                    vidso1k
133800150225     C                   KFLD                    wtsc              1
133900021219     C* LETTURA TNTBE01L - PARZIALE
134000030123     C     KEYTBEP       KLIST
134100021219     C                   KFLD                    tbeCOD                         *CODICE TABELLA
134200021219     C                   KFLD                    tbeKE1                         *CHIAVE 1
134300021219     C* LETTURA CNACO00F - COMPLETA
134400021219     C     KEYACO        KLIST
134500021219     C                   KFLD                    acoKUT                         *CODICE UTENTE
134600021219     C                   KFLD                    acoKCC                         *CODICE CAPOCONTO
134700021219     C                   KFLD                    acoKSC                         *CODICE SOTTOCONTO
134800970526     C*
134900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
135000970526     C     XMSGPA        PLIST
135100970526     C                   PARM                    DSMSMS
135200970526     C                   PARM                    DSMSPG
135300970526     C                   PARM                    DSMSKE
135400970526     C                   PARM                    DSMSER
135500970526     C*
135600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
135700970526     C                   CALL      'XKEY'
135800970526     C                   PARM                    DSKEY
135900970526     C*---
136000970526     C* VARIABILI NON RIFERITE AL DATA BASE
136100970526     C*---
136200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
136300970526     C*---
136400970526     C* VARIABILI DI CONTROLLO
136500970526     C*---
136600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
136700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
136800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
136900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
137000970526     C*
137100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
137200021219     C                   MOVEL     'TISIG4R'     DSMSPG
137300021219     C                   MOVEL     'TISIG4R'     MSGQUE
137400970526     C                   MOVEL     *BLANKS       MSGKEY
137500970526     C                   MOVEL     *BLANKS       DSMSMS
137600970526     C*
137700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
137800970526     C                   EXSR      IMPFUN
137900970526     C*
138000970526     C                   ENDSR
138100970526     C*------------------------------------------------------------------------*
138200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
138300970526     C*------------------------------------------------------------------------*
138400970526     C     FUNGES        BEGSR
138500970526     C*
138600970526     C* PULISCO CAMPI DI LAVORO
138700970526     C                   Z-ADD     0             W                 5 0
138800970526     C*
138900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
139000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
139100970526     C     1             DO        24            I
139200970526     C                   MOVEL     FUD(I)        FUDRIG
139300970526     C*
139400970526     C* PULIZIA FUNZIONI PRECEDENTI
139500970526     C                   MOVEL     *BLANKS       FUV(I)
139600970526     C*
139700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
139800970526     C* (SALTA F24)
139900970526     C     FUDATT        IFEQ      'S'
140000970526     C     FUDFUN        ANDNE     '24'
140100970526     C                   ADD       1             W
140200970526     C                   MOVEL     FUDDES        FUV(W)
140300970526     C                   END
140400970526     C                   END                                                    DO WHILE
140500970526     C*
140600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
140700970526     C* ALTRIMENTI DISATTIVO F24
140800970526     C     1             DO        24            I
140900970526     C                   MOVEL     FUD(I)        FUDRIG
141000970526     C*
141100970526     C     FUDFUN        IFEQ      '24'
141200970526     C     W             IFGT      6
141300970526     C                   MOVEL     'S'           FUDATT
141400970526     C                   ELSE
141500970526     C                   MOVEL     *BLANKS       FUDATT
141600970526     C                   END                                                    W GT 8
141700970526     C                   MOVEL     FUDRIG        FUD(I)
141800970526     C                   END
141900970526     C                   END                                                    DO WHILE
142000970526     C*
142100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
142200970526     C     W             DIV       6             I
142300970526     C                   MVR                     Z
142400970526     C*
142500970526     C     Z             IFNE      0
142600970526     C     I             ADD       1             Z
142700970526     C                   ELSE
142800970526     C                   Z-ADD     I             Z                 5 0
142900970526     C                   END
143000970526     C*
143100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
143200970526     C                   ADD       1             FUNGRU
143300970526     C     FUNGRU        IFGT      Z
143400970526     C     FUNGRU        ORLT      1
143500970526     C                   Z-ADD     1             FUNGRU
143600970526     C                   END
143700970526     C*
143800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
143900970526     C     FUNGRU        IFEQ      1
144000970526     C                   MOVEL     FUV(1)        FUN001
144100970526     C                   MOVEL     FUV(2)        FUN002
144200970526     C                   MOVEL     FUV(3)        FUN003
144300970526     C                   MOVEL     FUV(4)        FUN004
144400970526     C                   MOVEL     FUV(5)        FUN005
144500970526     C                   MOVEL     FUV(6)        FUN006
144600970526     C                   END
144700970526     C*
144800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
144900970526     C     FUNGRU        IFEQ      2
145000970526     C                   MOVEL     FUV(7)        FUN001
145100970526     C                   MOVEL     FUV(8)        FUN002
145200970526     C                   MOVEL     FUV(9)        FUN003
145300970526     C                   MOVEL     FUV(10)       FUN004
145400970526     C                   MOVEL     FUV(11)       FUN005
145500970526     C                   MOVEL     FUV(12)       FUN006
145600970526     C                   END
145700970526     C*
145800970526     C* TERZO GRUPPO DI 6 FUNZIONI
145900970526     C     FUNGRU        IFEQ      3
146000970526     C                   MOVEL     FUV(13)       FUN001
146100970526     C                   MOVEL     FUV(14)       FUN002
146200970526     C                   MOVEL     FUV(15)       FUN003
146300970526     C                   MOVEL     FUV(16)       FUN004
146400970526     C                   MOVEL     FUV(17)       FUN005
146500970526     C                   MOVEL     FUV(18)       FUN006
146600970526     C                   END
146700970526     C*
146800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
146900970526     C     FUNGRU        IFEQ      4
147000970526     C                   MOVEL     FUV(19)       FUN001
147100970526     C                   MOVEL     FUV(20)       FUN002
147200970526     C                   MOVEL     FUV(21)       FUN003
147300970526     C                   MOVEL     FUV(22)       FUN004
147400970526     C                   MOVEL     *BLANKS       FUN005
147500970526     C                   MOVEL     *BLANKS       FUN006
147600970526     C                   END
147700970526     C*
147800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
147900970526     C                   MOVEL     *BLANKS       FUN007
148000970526     C                   MOVEL     *BLANKS       FUN008
148100970526     C*
148200970526     C     1             DO        24            I
148300970526     C                   MOVEL     FUD(I)        FUDRIG
148400970526     C     FUDFUN        IFEQ      '24'
148500970526     C     FUDATT        ANDEQ     'S'
148600970526     C                   MOVEL     FUDDES        FUN008
148700970526     C                   END
148800970526     C                   END                                                    DO
148900970526     C*
149000970526     C                   ENDSR
149100970526     C*------------------------------------------------------------------------*
149200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
149300970526     C*------------------------------------------------------------------------*
149400970526     C     FUNCON        BEGSR
149500970526     C*
149600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
149700970526     C                   EXSR      FUNVAR
149800970526     C*
149900970526     C* PULISCO CAMPI DI LAVORO
150000970526     C                   Z-ADD     0             I                 5 0
150100970526     C                   Z-ADD     0             W                 5 0
150200970526     C*
150300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
150400970526     C     FUNKEY        IFNE      *BLANKS
150500970526     C     1             DO        24            I
150600970526     C                   MOVEL     FUD(I)        FUDRIG
150700970526     C*
150800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
150900970526     C     FUNKEY        IFEQ      FUDFUN
151000970526     C     FUDATT        ANDEQ     'S'
151100970526     C                   Z-ADD     I             W
151200970526     C                   END
151300970526     C                   END                                                    DO WHILE
151400970526     C*
151500970526     C* FUNZIONE NON ATTIVA (ERRORE)
151600970526     C     W             IFEQ      0
151700970526     C*
151800970526     C* EMISSIONE MESSAGGIO DI ERRORE
151900970526     C                   MOVEL     MSG(1)        DSMSMS
152000970526     C*
152100970526     C                   END                                                    W=0
152200970526     C                   END                                                    FUNKEY NE *BLANKS
152300970526     C*
152400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
152500970526     C     W             IFNE      0
152600970526     C     FUNKEY        OREQ      *BLANKS
152700970526     C     FUNKEY        IFEQ      *BLANKS
152800970526     C                   EXSR      ENTGES
152900970526     C                   END
153000970526     C     FUNKEY        IFEQ      '03'
153100970526     C                   EXSR      F03GES
153200970526     C                   END
153300970526     C     FUNKEY        IFEQ      '06'
153400970526     C                   EXSR      F06GES
153500970526     C                   END
153600160727     C     FUNKEY        IFEQ      '08'
153700160727     C                   EXSR      F08GES
153800160727     C                   END
153900970526     C     FUNKEY        IFEQ      '12'
154000970526     C                   EXSR      F12GES
154100970526     C                   END
154200970526     C     FUNKEY        IFEQ      '24'
154300970526     C                   EXSR      F24GES
154400970526     C                   END
154500970526     C                   END
154600970526     C*
154700970526     C                   ENDSR
154800970526     C*------------------------------------------------------------------------*
154900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
155000970526     C*------------------------------------------------------------------------*
155100970526     C     FUNVAR        BEGSR
155200970526     C*
155300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
155400970526     C*
155500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
155600970526     C     KEY           IFEQ      ENTER
155700970526     C                   MOVEL     *BLANKS       FUNKEY
155800970526     C                   END
155900970526     C     KEY           IFEQ      F1
156000970526     C                   MOVEL     '01'          FUNKEY
156100970526     C                   END
156200970526     C     KEY           IFEQ      F2
156300970526     C                   MOVEL     '02'          FUNKEY
156400970526     C                   END
156500970526     C     KEY           IFEQ      F3
156600970526     C                   MOVEL     '03'          FUNKEY
156700970526     C                   END
156800970526     C     KEY           IFEQ      F4
156900970526     C                   MOVEL     '04'          FUNKEY
157000970526     C                   END
157100970526     C     KEY           IFEQ      F5
157200970526     C                   MOVEL     '05'          FUNKEY
157300970526     C                   END
157400970526     C     KEY           IFEQ      F6
157500970526     C                   MOVEL     '06'          FUNKEY
157600970526     C                   END
157700970526     C     KEY           IFEQ      F7
157800970526     C                   MOVEL     '07'          FUNKEY
157900970526     C                   END
158000970526     C     KEY           IFEQ      F8
158100970526     C                   MOVEL     '08'          FUNKEY
158200970526     C                   END
158300970526     C     KEY           IFEQ      F9
158400970526     C                   MOVEL     '09'          FUNKEY
158500970526     C                   END
158600970526     C     KEY           IFEQ      F10
158700970526     C                   MOVEL     '10'          FUNKEY
158800970526     C                   END
158900970526     C     KEY           IFEQ      F11
159000970526     C                   MOVEL     '11'          FUNKEY
159100970526     C                   END
159200970526     C     KEY           IFEQ      F12
159300970526     C                   MOVEL     '12'          FUNKEY
159400970526     C                   END
159500970526     C     KEY           IFEQ      F13
159600970526     C                   MOVEL     '13'          FUNKEY
159700970526     C                   END
159800970526     C     KEY           IFEQ      F14
159900970526     C                   MOVEL     '14'          FUNKEY
160000970526     C                   END
160100970526     C     KEY           IFEQ      F15
160200970526     C                   MOVEL     '15'          FUNKEY
160300970526     C                   END
160400970526     C     KEY           IFEQ      F16
160500970526     C                   MOVEL     '16'          FUNKEY
160600970526     C                   END
160700970526     C     KEY           IFEQ      F17
160800970526     C                   MOVEL     '17'          FUNKEY
160900970526     C                   END
161000970526     C     KEY           IFEQ      F18
161100970526     C                   MOVEL     '18'          FUNKEY
161200970526     C                   END
161300970526     C     KEY           IFEQ      F19
161400970526     C                   MOVEL     '19'          FUNKEY
161500970526     C                   END
161600970526     C     KEY           IFEQ      F20
161700970526     C                   MOVEL     '20'          FUNKEY
161800970526     C                   END
161900970526     C     KEY           IFEQ      F21
162000970526     C                   MOVEL     '21'          FUNKEY
162100970526     C                   END
162200970526     C     KEY           IFEQ      F22
162300970526     C                   MOVEL     '22'          FUNKEY
162400970526     C                   END
162500970526     C     KEY           IFEQ      F23
162600970526     C                   MOVEL     '23'          FUNKEY
162700970526     C                   END
162800970526     C     KEY           IFEQ      F24
162900970526     C                   MOVEL     '24'          FUNKEY
163000970526     C                   END
163100970526     C     KEY           IFEQ      ROLLUP
163200970526     C                   MOVEL     'RU'          FUNKEY
163300970526     C                   END
163400970526     C     KEY           IFEQ      HELP
163500970526     C                   MOVEL     'HP'          FUNKEY
163600970526     C                   END
163700970526     C                   ENDSR
163800970526     C*------------------------------------------------------------------------*
163900970526     C* NOTE COSTRUZIONE SCHIERE
164000970526     C*------------------------------------------------------------------------*
164100970526     C*
164200970526     C* -MSG-MESSAGGI
164300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
164400970526     C* (NON TOCCARE MSG,1 E MSG,2)
164500970526     C*
164600970526     C* -CMD-COMANDI
164700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
164800970526     C*
164900970526     C* -OPD-OPZIONI
165000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
165100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
165200970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
165300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
165400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
165500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
165600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
165700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
165800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
165900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
166000970526     C*
166100970526     C* -FUD-FUNZIONI
166200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
166300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
166400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
166500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
166600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
166700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
166800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
166900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
167000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
167100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
167200970526     O*------------------------------------------------------------------------*
167300970526** DEC - DECODIFICHE
167400970526    MODIFICA                                                      1
167500970526  ANNULLAMENTO                                                    2
167600970526VISUALIZZAZIONE                                                   3
167700021211  INSERIMENTO                                                     4
167800021211     COPIA                                                        5
167900970528** MSG - MESSAGGI
168000021219TISIG4R- Tasto funzionale non gestito                                          1
168100021219TISIG4R- Inserimento effettuato                                                2
168200021220TISIG4R- Codice causale indicato errato                                        3
168300021220TISIG4R- Soggetto movimento errato                                             4
168400021220TISIG4R- Articolo errato                                                       5
168500021219TISIG4R- Richiesta funzione per record inesistente                             6
168600021219TISIG4R- Impossibile inserire, movimento già presente                          7
168700021219TISIG4R- Richiesta funzione per record esistente                               8
168800021220TISIG4R- Data non corretta                                                     9
168900021224TISIG4R- La causale indicata non è di tipo manuale, correggere.                10
169000021224TISIG4R- Indicare la quantità movimentata                                      11
169100021219TISIG4R- Tutti i campi sono obbligatori                                        12
169200070509TISIG4R- Soggetto indicato non in gestione all'utente. Enter per forzare!      13
169300150204TISIG4R- Il cliente/articolo non sono abilitati alla gestione bancali          14
169400150212TISIG4R- Articolo incongruente con anagrafica del cliente                      15
169500150305TISIG4R- Quantità immessa supera la quantità massima calcolata di              16
169600150216TISIG4R- Questo articolo ha un saldo pari a 0 inserimento non possibile        17
169700150223TISIG4R- La causale indicata non è utilizzabile dall'utente.                   18
169800150224TISIG4R- Data movimento incongruente con la validita anagrafica cliente        19
169900150304TISIG4R- Causale non utilizzabile per la movimentazione di questo articolo     20
170000150311TISIG4R- Soggetto o filiale non in gestione all'utente                         21
170100160727TISIG4R- Cancellazione impossibile esiste un record abbinato                   22
170200160727TISIG4R- Cancellazione eseguita con successo                                   23
170300150311TISIG4R-                                                                       24
170400150311TISIG4R-                                                                       25
170500970526** FUD - FUNZIONI
170600970526                   -        (S/N)=N                        funz.01¬
170700970526                   -        (S/N)=N                        funz.02¬
170800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
170900970526                   -        (S/N)=N                        funz.04¬
171000970526                   -        (S/N)=N                        funz.05¬
171100000407F06=Conferma       -        (S/N)=S                        funz.06¬
171200970527                   -        (S/N)=N                        funz.07¬
171300160727F08=Conf.Elimina   -        (S/N)=N                        funz.08¬
171400160727                   -        (S/N)=N                        funz.09¬
171500970527                   -        (S/N)=N                        funz.10¬
171600970526                   -        (S/N)=N                        funz.11¬
171700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
171800970526                   -        (S/N)=N                        funz.13¬
171900970526                   -        (S/N)=N                        funz.14¬
172000970526                   -        (S/N)=N                        funz.15¬
172100970526                   -        (S/N)=N                        funz.16¬
172200970526                   -        (S/N)=N                        funz.17¬
172300970526                   -        (S/N)=N                        funz.18¬
172400970526                   -        (S/N)=N                        funz.19¬
172500970526                   -        (S/N)=N                        funz.20¬
172600970526                   -        (S/N)=N                        funz.21¬
172700970526                   -        (S/N)=N                        funz.22¬
172800970526                   -        (S/N)=N                        funz.23¬
172900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
