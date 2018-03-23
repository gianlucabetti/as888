000100970526     H*------------------------------------------------------------------------*
000200051229     H* Cambio unificanti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800051229     Ftis7c7d   CF   E             WORKSTN INFDS(DEVDS1)
000900051229     F                                     SFILE(s7c7DMS:MSGULT)
001000051229     Ftikun01l  UF A E           K DISK
001100090806     Ftikun03l  IF   E           K DISK    RENAME(tikun000:tikun003)
001200051229     Ftivss02l  UF A E           K DISK
001300060127     Ftntbe01l  UF A E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D* MESSAGGI
001800970526     D*------------
001900000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* DESCRIZIONI TASTI FUNZIONALI
002200970526     D*------------
002300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* FUNZIONI DA CARICARE A VIDEO
002600970526     D*------------
002700970526     D FUV             S             19    DIM(24)
002800970526     D*------------------------------------------------------------------------*
002900970526     D* INPUT
003000970526     D*------------------------------------------------------------------------*
003100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003200970526     D*------------
003300970526     D FUDDS           DS
003400970526     D  FUDDE1                 1      1
003500970526     D  FUDFUN                 2      3
003600970526     D  FUDDE2                 4     19
003700970526     D  FUDFI0                20     34
003800970526     D  FUDATT                35     35
003900970526     D  FUDFI1                36     80
004000970526     D  FUDDES                 1     19
004100970526     D  FUDRIG                 1     80
004200970526     D*------------
004300970526     D* DS GESTIONE TASTI FUNZIONALI
004400970526     D*------------
004500970526     D DSKEY         E DS
004600970526     D DEVDS1          DS
004700970526     D  KEY                  369    369
004800970526     D*------------
004900970526     D* DS "XMSG" - GESTIONE MESSAGGI
005000970526     D*------------
005100970526     D DSMSG         E DS
005200970526     D*------------
005300970526     D* ARCHITETTURA
005400970526     D*------------
005500970526     D KPJBA         E DS
005600020211     D*------------------
005700020211     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800020211     D*------------------
005900020211     D WLBDA8          DS                  INZ
006000020211     D  G08DAT                 1      8  0
006100020211     D  G08INV                 9     16  0
006200020211     D  G08ERR                17     17
006300020211     D  G08TGI                18     22  0
006400020710     D*------------------
006500051229     D* DS REPERIMENTO ATTRIBUTI CODICE CLIENTE
006600020710     D*------------
006700051229     D BS69DS        E DS                  extname(TIBS69DS)
006800051229     D ACODS         E DS                  extname(CNACO00F)
006900051229     D INDDS         E DS                  extname(CNIND00F)
007000051229     D CLPDS         E DS                  extname(CNCLP00F)
007100051229     D CLSDS         E DS                  extname(FNCLS00F)
007200051229     D*-------------------
007300051229     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
007400051229     D*-------------------
007500051229     D TIBS10DS      E DS
007600051229     D*-------------------
007700051229     D* DS REPERIMENTO DATI UTENTE CORRENTE
007800051229     D*-------------------
007900051229     D Tibs34ds      e ds
008000051229     D Azuteds       e ds                  Extname(Azute00f)
008100051229     D dDatiute      e ds
008200051229     D dute01        e ds
008300051229     D*-------------------
008400051229     D* DS REPERIMENTO P.O. IN GESTIONE
008500051229     D*-------------------
008600051229     D TRUL31DS      E DS
008700051229     D*-------------------
008800051229     D* DS RIDEFINIZIONE TABELLA 'ISV' - TIPI SERVIZI
008900051229     D*-------------------
009000051229     D DISV          E DS
009100060127     D*-------------------
009200060127     D* DS X SALVATAGGIO OCCORRENZE TABELLE
009300060127     D*-------------------
009400060127     D TBE_DS        e ds                  Extname(tntbe00f)
009500060127     D TBE_DS_SAV    e ds                  Extname(tntbe00f) Prefix(S_)
009600090806     D*-------------------
009700090806     D* DS TIPI LEGAMI UNIFICANTI
009800090806     D*-------------------
009900090806     D DTLE          e ds
010000051229     D*------------------
010100051229     D* VARIABILI DI WRK
010200051229     D*------------
010300051229     D wCLICOD         S                   LIKE(I69KAC) INZ(*zeros)
010400051229     D wCLIRAG         S                   LIKE(ACORAG) INZ(*blanks)
010500051229     D wTLETLE         S                   LIKE(D10TLE) INZ(*blanks)
010600051229     D wTLETIP         S                   LIKE(D10PAF) INZ(*blanks)
010700051229     D wTLECOD         S                   LIKE(D10COD) INZ(*zeros)
010800051229     D wTLEDAT         S                   LIKE(D10DRF) INZ(*zeros)
010900051229     D wDATGGMMAAAA    S                   LIKE(VIDSTDDEC) INZ(*zeros)
011000051229     D wDATAAAAMMGG    S                   LIKE(VIDSTDDEC) INZ(*zeros)
011100051229     D skPOG           S              3    DIM(250) INZ(*zeros)
011200051229     D skISV_TT        S              2    DIM(100) INZ(*blanks)
011300090806     D skTIPTLEWW      S              2    DIM(100) INZ(*blanks)
011400090806     D jskTIPTLEWW     S              3  0 INZ(*zeros)
011500051229     D jskISV_TT       S              3  0 INZ(*zeros)
011600051229     D wDATA_ISO       S               D   DATFMT(*ISO) INZ(D'1999-01-01')
011700051229     D KEYVSSKSU       S                   LIKE(VSSKSU)
011800051229     D K_kuntle        S                   LIKE(KUNTLE)
011900051229     D K_kunazp        S                   LIKE(KUNAZP)
012000051229     D K_kuncop        S                   LIKE(KUNCOP)
012100051229     D wMAX_KUN_DDE    S                   LIKE(KUNDDE) INZ(*zeros)
012200051230     D wMAX_VSS_DDE    S                   LIKE(VSSDDE) INZ(*zeros)
012300970526     C*------------------------------------------------------------------------*
012400000303     C* MAIN LINES
012500970526     C*------------------------------------------------------------------------*
012600051229     C*
012700051229     C* REPERIMENTO DATI UTENTE CORRENTE
012800051229     C                   EXSR      REPDATIUTE
012900051229     C*
013000051229     C* CARICAMENTO TABELLE
013100051229     C                   EXSR      CARTAB
013200051229     C*
013300051229     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
013400051229     C                   EXSR      AZZVID
013500970526     C*
013600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014000970526     C*
014100970526     C* EMETTE FORMATO VIDEO
014200970526     C                   EXSR      BUTFMR
014300970526     C*
014400970526     C* GESTIONE TASTI FUNZIONALI
014500970526     C                   EXSR      FUNCON
014600051229     C                   ENDDO
014700970526     C*
014800970526     C* FINE PGM
014900970526     C                   SETON                                        LR
015000970526     C*------------------------------------------------------------------------*
015100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
015200970526     C*------------------------------------------------------------------------*
015300970526     C     BUTFMR        BEGSR
015400970526     C*
015500970526     C* EMETTE LA TESTATA
015600051229     C                   WRITE     s7c7D01
015700970526     C*
015800970526     C* SPEDISCE EVENTUALE MESSAGGIO
015900970526     C                   EXSR      SNDMSG
016000970526     C*
016100970526     C* EMETTE LE FUNZIONI
016200051229     C                   WRITE     s7c7D02
016300970526     C*
016400970526     C* EMETTE IL VIDEO
016500051229     C                   EXFMT     s7c7D03
016600970526     C*
016700970526     C                   ENDSR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017000970526     C*------------------------------------------------------------------------*
017100970526     C     SNDMSG        BEGSR
017200970526     C*
017300970526     C* PULISCE IL SFILE MESSAGGI
017400970526     C                   MOVEL     '1'           *IN40
017500051229     C                   WRITE     s7c7DMC
017600970526     C                   MOVEL     '0'           *IN40
017700970526     C*
017800970526     C* VISUALIZZA IL MESSAGGIO
017900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018000970526     C                   CALL      'XMSG'        XMSGPA
018100051229     C     DSMSER        IFEQ      *ZEROS
018200970526     C                   MOVEL     DSMSKE        MSGKEY
018300051229     C                   ENDIF
018400970526     C                   Z-ADD     1             MSGULT
018500051229     C                   WRITE     s7c7DMS
018600051229     C                   WRITE     s7c7DMC
018700970526     C*
018800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018900970526     C                   MOVEL     *BLANKS       DSMSMS
019000970526     C*
019100970526     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300970526     C* ENTGES - GESTIONE ENTER -> INVIO
019400970526     C*------------------------------------------------------------------------*
019500970526     C     ENTGES        BEGSR
019600970526     C*
019700970526     C* CONTROLLA I CAMPI IMMESSI
019800970526     C                   EXSR      DECODI
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* F03GES - GESTIONE F03 -> FINE
020300970526     C*------------------------------------------------------------------------*
020400970526     C     F03GES        BEGSR
020500970526     C*
020600970526     C* IMPOSTA "PREMUTO F03"
020700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100991214     C* F06GES - GESTIONE F06 -> CONFERMA
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F06GES        BEGSR
021400970526     C*
021500991214     C* CONTROLLA I CAMPI IMMESSI
021600991214     C                   EXSR      DECODI
021700051229     C*
021800051229     C* SE TUTTO OK => ELABORA
021900991214     C     ERRL00        IFEQ      '0'
022000051229     C*
022100991214     C* ESEGUE ROUTINE DI CONFERMA
022200051229     C* ===> 1° COSA DA FARE: LEGAMI
022300051229     C                   EXSR      EXE_LEGAMI
022400051229     C* ===> 2° COSA DA FARE: ABILITAZIONI
022500051229     C                   EXSR      EXE_ABIL
022600051229     C                   MOVEL     MSG(6)        DSMSMS
022700051229     C                   EXSR      AZZVID
022800991214     C                   ENDIF
022900970526     C*
023000991214     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* F12GES - GESTIONE F12 -> RITORNO
023300970526     C*------------------------------------------------------------------------*
023400970526     C     F12GES        BEGSR
023500970526     C*
023600970526     C* IMPOSTA "PREMUTO F12"
023700051229     C                   MOVEL     '1'           F12L00                         *FINE *PGM
023800970526     C*
023900970526     C                   ENDSR
024000970526     C*------------------------------------------------------------------------*
024100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024200970526     C*------------------------------------------------------------------------*
024300970526     C     F24GES        BEGSR
024400970526     C*
024500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024600970526     C*
024700970526     C                   ENDSR
024800970526     C*------------------------------------------------------------------------*
024900051229     C* EXE_LEGAMI - OPERAZIONI DI ELABORAZIONE LEGATI (TIKUN00F)
025000970526     C*------------------------------------------------------------------------*
025100051229     C     EXE_LEGAMI    BEGSR
025200991214     C*
025300051229     C*********************
025400051229     C* ELABORO X LEGAMI 'WW'
025500051229     C*********************
025600051229     C                   IF        VIDWWKOLD <>  *zeros AND
025700051229     C                             VIDWWKNEW <>  *zeros
025800051229     C                   EVAL      K_kuntle = 'WW'
025900051229     C                   EVAL      K_kunazp = *blanks
026000051229     C                   EVAL      K_kuncop = VIDWWKOLD
026100051229     C     KEYkun01_P    SETLL     tikun01l
026200051229     C                   IF        %found(tikun01l)
026300051229     C     KEYkun01_P    READE     tikun01l
026400051229     C                   DOW       not %eof(tikun01l)
026500051229     C*
026600051229     C* ELABORO SOLO SE LEGAME NN GIA' SCADUTO
026700051229     C                   IF        kundsc >= datcor
026800051229     C*
026900051229     C* ===> 1° COSA DA FARE: FAR SCADERE LEGAMI ESISTENTI SU CODICE "OLD"
027000051229     C     *ISO          MOVE      datcor        wDATA_ISO
027100051229     C     wDATA_ISO     SUBDUR    1:*D          wDATA_ISO
027200051229     C     *ISO          MOVE      wDATA_ISO     kundsc
027300051230     C* SE TRA I FIGLI DEL CODICE PADRE "OLD" CI FOSSE GIA' IL CODICE PADRE "NEW"
027400060209     C* OLTRE A FARLO SCADERE LO ANNULLO ANCHE (LOGICAMENTE)
027500051230     C                   IF        kuncof = VIDWWKNEW
027600051230     C                   EVAL      kunvat = 'A'
027700051230     C                   ENDIF
027800051229     C                   EVAL      kunftr = *blanks
027900051229     C                   UPDATE    tikun000
028000051229     C*
028100051229     C* ===> 2° COSA DA FARE: INSERIRE NUOVO LEGAME SU CODICE "NEW"
028200051229     C*
028300051229     C* SE CODICE PADRE "NEW" DIVERSO DA FIGLIO CORRENTE INSERISCO NUOVO LEGAME
028400051229     C                   IF        VIDWWKNEW <> kuncof
028500051229     C                   EVAL      kuncop = VIDWWKNEW
028600051229     C                   EVAL      kundde = datcor
028700051229     C                   EVAL      kundsc = 20391231
028800051229     C                   EVAL      kunftr = *blanks
028900051229     C                   EVAL      kundtr = *zeros
029000060130     C                   WRITE(e)  tikun000
029100051229     C                   ENDIF
029200051229     C                   ENDIF
029300051229     C*
029400051229     C* PROSEGUO CON LA LETTURA DEI LEGAMI DEL CODICE "OLD"
029500051229     C     KEYkun01_P    READE     tikun01l
029600051229     C                   ENDDO
029700051229     C                   ENDIF
029800051229     C*
029900051229     C* ===> 3° COSA DA FARE: INSERIRE NUOVO LEGAME SU CODICE "NEW" CON FIGLIO PADRE "OLD"
030000051229     C                   CLEAR                   tikun000
030100051229     C                   EVAL      kuntle = 'WW'
030200051229     C                   EVAL      kuncop = VIDWWKNEW
030300051229     C                   EVAL      kuncof = VIDWWKOLD
030400051229     C                   EVAL      kundde = datcor
030500051229     C                   EVAL      kundsc = 20391231
030600051229     C                   EVAL      kundtr = *zeros
030700060130     C                   WRITE(e)  tikun000
030800051230     C                   ENDIF
030900051229     C*
031000051229     C*********************
031100051229     C* ELABORO X LEGAMI 'ST'
031200051229     C*********************
031300051229     C                   IF        VIDSTKOLD <>  *zeros AND
031400051229     C                             VIDSTKNEW <>  *zeros AND
031500051229     C                             VIDSTDDEC <>  *zeros
031600051229     C                   EVAL      K_kuntle = 'ST'
031700051229     C                   EVAL      K_kunazp = *blanks
031800051229     C                   EVAL      K_kuncop = VIDSTKOLD
031900051229     C     KEYkun01_P    SETLL     tikun01l
032000051229     C                   IF        %found(tikun01l)
032100051229     C     KEYkun01_P    READE     tikun01l
032200051229     C                   DOW       not %eof(tikun01l)
032300051229     C*
032400051229     C* ELABORO SOLO SE LEGAME NN GIA' SCADUTO
032500051229     C                   IF        kundsc >= wDATAAAAMMGG
032600051229     C*
032700051229     C* ===> 1° COSA DA FARE: FAR SCADERE LEGAMI ESISTENTI SU CODICE "OLD"
032800051229     C     *ISO          MOVE      wDATAAAAMMGG  wDATA_ISO
032900051229     C     wDATA_ISO     SUBDUR    1:*D          wDATA_ISO
033000051229     C     *ISO          MOVE      wDATA_ISO     kundsc
033100051229     C                   EVAL      kunftr = *blanks
033200051229     C                   UPDATE    tikun000
033300051229     C*
033400051229     C* ===> 2° COSA DA FARE: INSERIRE NUOVO LEGAME SU CODICE "NEW"
033500051229     C*
033600051229     C* SE CODICE PADRE "NEW" DIVERSO DA FIGLIO CORRENTE INSERISCO NUOVO LEGAME
033700051229     C                   IF        VIDSTKNEW <> kuncof
033800051229     C                   EVAL      kuncop = VIDSTKNEW
033900051229     C                   EVAL      kundde = wDATAAAAMMGG
034000051229     C                   EVAL      kundsc = 20391231
034100051229     C                   EVAL      kunftr = *blanks
034200051229     C                   EVAL      kunftr = *zeros
034300060130     C                   WRITE(e)  tikun000
034400051229     C                   ENDIF
034500051229     C                   ENDIF
034600051229     C*
034700051229     C* PROSEGUO CON LA LETTURA DEI LEGAMI DEL CODICE "OLD"
034800051229     C     KEYkun01_P    READE     tikun01l
034900051229     C                   ENDDO
035000051229     C                   ENDIF
035100051229     C*
035200051229     C* ===> 3° COSA DA FARE: INSERIRE NUOVO LEGAME SU CODICE "NEW" CON FIGLIO PADRE "OLD"
035300051229     C                   CLEAR                   tikun000
035400051229     C                   EVAL      kuntle = 'ST'
035500051229     C                   EVAL      kuncop = VIDSTKNEW
035600051229     C                   EVAL      kuncof = VIDSTKOLD
035700051229     C                   EVAL      kundde = wDATAAAAMMGG
035800051229     C                   EVAL      kundsc = 20391231
035900051229     C                   EVAL      kunftr = *zeros
036000060130     C                   WRITE(e)  tikun000
036100051229     C                   ENDIF
036200991214     C*
036300970526     C                   ENDSR
036400051229     C*------------------------------------------------------------------------*
036500051229     C* EXE_ABIL - OPERAZIONI DI ELABORAZIONE ABILITAZIONI (TIVSS00F)
036600051229     C*------------------------------------------------------------------------*
036700051229     C     EXE_ABIL      BEGSR
036800051229     C*
036900051229     C* SU TUTTE LE ABILITAZIONE (D QUALUNQUE UTENTE VSSSUN) AVENTE COME CODICE
037000051229     C* CLIENTE UNIFICANTE IL CODICE "OLD" - LEGAME 'WW' MODIFICO E IMPOSTO
037100051229     C* IL CODICE "NEW" - LEGAME 'WW'
037200051229     C                   IF        VIDWWKOLD <> *zeros AND
037300051229     C                             VIDWWKNEW <> *zeros
037400051229     C                   EVAL      KEYVSSKSU = *all'0'
037500051229     C                   MOVE      VIDWWKOLD     KEYVSSKSU
037600051229     C     KEYVSSKSU     SETLL     TIVSS02L
037700051229     C                   IF        %found(TIVSS02L)
037800051229     C     KEYVSSKSU     READE     TIVSS02L
037900051229     C                   DOW       not %eof(TIVSS02L)
038000091116     C                   IF        VSSTLE = 'WW' OR
038100091116     C                             VSSTLE = *blanks
038200051229     C     VSSISV        LOOKUP    skISV_TT                               30
038300051229     C                   IF        *IN30
038400051229     C                   EVAL      VSSKSU = *all'0'
038500051229     C                   MOVE      VIDWWKNEW     VSSKSU
038600051229     C                   EVAL      VSSFTR = *blanks
038700051229     C                   UPDATE    TIVSS000
038800060127     C*
038900060127     C* Verifico anche se codice "OLD" presente in tabella VAP
039000060127     C                   SETOFF                                       55
039100060127     C                   EVAL      tbeCOD = 'VAP'
039200060127     C                   EVAL      tbeKE1 = KEYVSSKSU
039300060127     C     KEYtbe01_P    CHAIN(n)  tntbe01l
039400060127     C                   IF        %found(tntbe01l)
039500060127     C                   EVAL      TBE_DS_SAV = TBE_DS
039600060127     C                   SETON                                        55
039700060127     C*
039800060127     C* Verifico se codice "NEW" presente in tabella VAP e se nn già presente
039900060127     C* inserisco nuovo record sul codice "NEW"
040000060127     C                   EVAL      tbeKE1 = VSSKSU
040100060127     C     KEYtbe01_P    CHAIN(n)  tntbe01l
040200060127     C                   IF        not %found(tntbe01l)
040300060127     C                   EVAL      TBE_DS = TBE_DS_SAV
040400060127     C                   EVAL      tbeKE1 = VSSKSU
040500060127     C                   EVAL      tbeFTR = *blanks
040600060127     C                   WRITE     TNTBE000
040700060127     C                   ENDIF
040800060127     C                   ENDIF
040900051229     C                   ENDIF
041000090803     C                   ENDIF
041100051229     C     KEYVSSKSU     READE     TIVSS02L
041200051229     C                   ENDDO
041300051229     C                   ENDIF
041400051229     C                   ENDIF
041500051229     C*
041600051229     C                   ENDSR
041700970526     C*------------------------------------------------------------------------*
041800991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
041900970526     C*------------------------------------------------------------------------*
042000051229     C     DECODI        BEGSR
042100051229     C*
042200051229     C* REIMPOSTA INDICATORI DI ERRORE
042300051229     C                   SETOFF                                       808182
042400051229     C                   SETOFF                                       8384
042500051229     C*
042600051229     C* REIMPOSTA FLAG DI ERRORE
042700051229     C                   MOVEL     '0'           ERRL00
042800060105     C*
042900060105     C* COME PRIMA COSA VERIFICO EVENTUALE ERRORE D AUTORIZZAZIONE UTENTE
043000060105     C                   IF        ERRUTE = '1'
043100060105     C                   MOVEL     '1'           ERRL00
043200060105     C                   MOVEL     MSG(7)        DSMSMS
043300060105     C                   ENDIF
043400060127     C*
043500060127     C* VERIFICA CODICI "OLD" E "NEW" DEVONO ESSERE DIVERSI
043600060127     C* ....LEGAME "ST"
043700060127     C                   IF        ERRL00 = '0'
043800060127     C                   IF        VIDSTKOLD <> *zeros AND
043900060127     C                             VIDSTKNEW <> *zeros
044000060127     C                   IF        VIDSTKOLD  = VIDSTKNEW
044100060127     C                   SETON                                        8283
044200060127     C                   MOVEL     '1'           ERRL00
044300060127     C                   MOVEL     MSG(14)       DSMSMS
044400060127     C                   ENDIF
044500060127     C                   ENDIF
044600060127     C                   ENDIF
044700060127     C* ....LEGAME "WW"
044800060127     C                   IF        ERRL00 = '0'
044900060127     C                   IF        VIDWWKOLD <> *zeros AND
045000060127     C                             VIDWWKNEW <> *zeros
045100060127     C                   IF        VIDWWKOLD  = VIDWWKNEW
045200060127     C                   SETON                                        8081
045300060127     C                   MOVEL     '1'           ERRL00
045400060127     C                   MOVEL     MSG(14)       DSMSMS
045500060127     C                   ENDIF
045600060127     C                   ENDIF
045700060127     C                   ENDIF
045800051229     C*
045900051229     C* VERIFICA OBBLIGATORIETA CAMPI "IN"
046000051229     C*
046100051229     C* CONTROLLI X OPERAZIONI LEGAME 'WW'
046200051229     C                   IF        ERRL00 = '0'
046300051229     C                   IF        VIDWWKOLD = *zeros AND
046400051229     C                             VIDWWKNEW = *zeros
046500051229     C                   ELSE
046600051229     C                   IF        VIDWWKOLD = *zeros
046700051229     C                   MOVEL     '1'           ERRL00
046800051229     C                   MOVEL     MSG(4)        DSMSMS
046900051229     C                   ELSE
047000051229     C                   EVAL      wCLICOD = VIDWWKOLD
047100051229     C                   EXSR      DECCLI
047200051229     C                   EVAL      VDDWWKOLD = wCLIRAG
047300051229     C                   ENDIF
047400051229     C                   IF        ERRL00 = '1'
047500051229     C                   SETON                                        80
047600051229     C                   ENDIF
047700051229     C*
047800051229     C                   IF        VIDWWKNEW = *zeros
047900051229     C                   MOVEL     '1'           ERRL00
048000051229     C                   MOVEL     MSG(4)        DSMSMS
048100051229     C                   ELSE
048200051229     C                   EVAL      wCLICOD = VIDWWKNEW
048300051229     C                   EXSR      DECCLI
048400051229     C                   EVAL      VDDWWKNEW = wCLIRAG
048500051229     C                   ENDIF
048600051229     C                   IF        ERRL00 = '1'
048700051229     C                   SETON                                        81
048800051229     C                   ENDIF
048900051229     C                   ENDIF
049000051229     C                   ENDIF
049100051229     C*
049200051229     C* CONTROLLI X OPERAZIONI LEGAME 'ST'
049300051229     C                   IF        ERRL00 = '0'
049400051229     C                   IF        VIDSTKOLD = *zeros AND
049500051229     C                             VIDSTKNEW = *zeros AND
049600051229     C                             VIDSTDDEC = *zeros
049700051229     C                   ELSE
049800051229     C                   IF        VIDSTKOLD = *zeros
049900051229     C                   MOVEL     '1'           ERRL00
050000051229     C                   MOVEL     MSG(4)        DSMSMS
050100051229     C                   ELSE
050200051229     C                   EVAL      wCLICOD = VIDSTKOLD
050300051229     C                   EXSR      DECCLI
050400051229     C                   EVAL      VDDSTKOLD = wCLIRAG
050500051229     C                   ENDIF
050600051229     C                   IF        ERRL00 = '1'
050700051229     C                   SETON                                        82
050800051229     C                   ENDIF
050900051229     C*
051000051229     C                   IF        VIDSTKNEW = *zeros
051100051229     C                   MOVEL     '1'           ERRL00
051200051229     C                   MOVEL     MSG(4)        DSMSMS
051300051229     C                   ELSE
051400051229     C                   EVAL      wCLICOD = VIDSTKNEW
051500051229     C                   EXSR      DECCLI
051600051229     C                   EVAL      VDDSTKNEW = wCLIRAG
051700051229     C                   ENDIF
051800051229     C                   IF        ERRL00 = '1'
051900051229     C                   SETON                                        83
052000051229     C                   ENDIF
052100051229     C*
052200051229     C                   IF        VIDSTDDEC = *zeros
052300051229     C                   MOVEL     '1'           ERRL00
052400051229     C                   MOVEL     MSG(4)        DSMSMS
052500051229     C                   ELSE
052600051229     C                   EVAL      wDATGGMMAAAA = VIDSTDDEC
052700051229     C                   EXSR      CHKDA8
052800051229     C                   EVAL      VIDSTDDEC = wDATGGMMAAAA
052900051229     C                   ENDIF
053000051229     C                   IF        ERRL00 = '1'
053100051229     C                   SETON                                        84
053200051229     C                   ENDIF
053300051229     C                   ENDIF
053400051229     C                   ENDIF
053500051229     C*
053600051229     C* VERIFICA ABILITAZIONE UTENTE CORRENTE AI CODICI CLIENTI INDICATI
053700051229     C*
053800051229     C* VERIFICA CODICE CLIENTE "OLD" - LEGAME 'WW'
053900051229     C                   IF        ERRL00 = '0'
054000051229     C                   IF        VIDWWKOLD <> *zeros
054100051229     C                   MOVEL     VIDWWKOLD     wPO_Alfa          3
054200051229     C     wPO_Alfa      LOOKUP    skPOG                                  30
054300051229     C                   IF        NOT *IN30
054400051229     C                   SETON                                        80
054500051229     C                   MOVEL     '1'           ERRL00
054600051229     C                   MOVEL     MSG(8)        DSMSMS
054700051229     C                   ENDIF
054800051229     C                   ENDIF
054900051229     C                   ENDIF
055000051229     C*
055100051229     C* VERIFICA CODICE CLIENTE "NEW" - LEGAME 'WW'
055200051229     C                   IF        ERRL00 = '0'
055300051229     C                   IF        VIDWWKNEW <> *zeros
055400051229     C                   MOVEL     VIDWWKNEW     wPO_Alfa          3
055500051229     C     wPO_Alfa      LOOKUP    skPOG                                  30
055600051229     C                   IF        NOT *IN30
055700051229     C                   SETON                                        81
055800051229     C                   MOVEL     '1'           ERRL00
055900051229     C                   MOVEL     MSG(8)        DSMSMS
056000051229     C                   ENDIF
056100051229     C                   ENDIF
056200051229     C                   ENDIF
056300051229     C*
056400051229     C* VERIFICA CODICE CLIENTE "OLD" - LEGAME 'ST'
056500051229     C                   IF        ERRL00 = '0'
056600051229     C                   IF        VIDSTKOLD <> *zeros
056700051229     C                   MOVEL     VIDSTKOLD     wPO_Alfa          3
056800051229     C     wPO_Alfa      LOOKUP    skPOG                                  30
056900051229     C                   IF        NOT *IN30
057000051229     C                   SETON                                        82
057100051229     C                   MOVEL     '1'           ERRL00
057200051229     C                   MOVEL     MSG(8)        DSMSMS
057300051229     C                   ENDIF
057400051229     C                   ENDIF
057500051229     C                   ENDIF
057600051229     C*
057700051229     C* VERIFICA CODICE CLIENTE "NEW" - LEGAME 'ST'
057800051229     C                   IF        ERRL00 = '0'
057900051229     C                   IF        VIDSTKNEW <> *zeros
058000051229     C                   MOVEL     VIDSTKNEW     wPO_Alfa          3
058100051229     C     wPO_Alfa      LOOKUP    skPOG                                  30
058200051229     C                   IF        NOT *IN30
058300051229     C                   SETON                                        83
058400051229     C                   MOVEL     '1'           ERRL00
058500051229     C                   MOVEL     MSG(8)        DSMSMS
058600051229     C                   ENDIF
058700051229     C                   ENDIF
058800090806     C                   ENDIF
058900051229     C*
059000051230     C* VERIFICA CHE PADRE "OLD" - LEGAME 'WW' ALLA DATA CORRENTE NN SIA GIA' FIGLIO
059100051230     C* DI CHIUNQUE X LEGAME 'WW'
059200051229     C                   IF        ERRL00 = '0'
059300051229     C                   IF        VIDWWKOLD <> *zeros
059400051229     C                   EVAL      wTLETLE = 'WW'
059500051229     C                   EVAL      wTLECOD = VIDWWKOLD
059600051230     C                   EVAL      wTLETIP = 'P'
059700051229     C                   EVAL      wTLEDAT = datcor
059800051230     C                   EXSR      REPUNI
059900051230     C*                  IF        d10err <> *blanks
060000051230     C*                  ELSE
060100051230     C*                  SETON                                        80        *SI FIGLIO
060200051230     C*                  MOVEL     '1'           ERRL00
060300051230     C*                  EVAL      DSMSMS = %trim(MSG(3))
060400051230     C*                  ENDIF
060500060404     C                   IF        d10err <> *blanks                            *NO FIGLIO
060600060404     C***                          OR d10cop =  VIDWWKNEW                       *SI FIGLIO DI "NEW"
060700051230     C                   ELSE
060800051230     C                   SETON                                        80
060900051230     C                   MOVEL     '1'           ERRL00
061000060404     C                   EVAL      DSMSMS = %trim(MSG(11))+
061100051230     C                                      %trim(%editc(d10cop:'4'))
061200051230     C                   ENDIF
061300051229     C                   ENDIF
061400051229     C                   ENDIF
061500051229     C*
061600051229     C* VERIFICA CHE PADRE "NEW" - LEGAME 'WW' NN SIA GIA' FIGLIO DI ALTRO PADRE
061700060404     C* SE NN EVENTUALMENTE D PADRE "OLD" ALLA DATA CORRENTE
061800051229     C                   IF        ERRL00 = '0'
061900051229     C                   IF        VIDWWKNEW <> *zeros
062000051229     C                   EVAL      wTLETLE = 'WW'
062100051229     C                   EVAL      wTLECOD = VIDWWKNEW
062200051229     C                   EVAL      wTLETIP = 'P'
062300051229     C                   EVAL      wTLEDAT = datcor
062400051229     C                   EXSR      REPUNI
062500060404     C                   IF        d10err <> *blanks                            *NO FIGLIO
062600060404     C                             OR d10cop =  VIDWWKOLD                       *SI FIGLIO DI "OLD"
062700051229     C                   ELSE
062800051229     C                   SETON                                        81
062900051229     C                   MOVEL     '1'           ERRL00
063000051229     C                   EVAL      DSMSMS = %trim(MSG(2))+
063100051229     C                                      %trim(%editc(d10cop:'4'))
063200051229     C                   ENDIF
063300051229     C                   ENDIF
063400051229     C                   ENDIF
063500051229     C*
063600051230     C* VERIFICA CHE PADRE "OLD" - LEGAME 'ST' ALLA DATA CORRENTE NN SIA GIA' FIGLIO
063700051230     C* DI CHIUNQUE X LEGAME 'ST'
063800051229     C                   IF        ERRL00 = '0'
063900051229     C                   IF        VIDSTKOLD <> *zeros
064000051229     C                   EVAL      wTLETLE = 'ST'
064100051229     C                   EVAL      wTLECOD = VIDSTKOLD
064200051230     C                   EVAL      wTLETIP = 'P'
064300051229     C                   EVAL      wDATGGMMAAAA = VIDSTDDEC
064400051229     C                   EXSR      CHKDA8
064500051229     C                   EVAL      wTLEDAT = wDATAAAAMMGG
064600051229     C                   EXSR      REPUNI
064700051230     C*                  IF        d10err <> *blanks
064800051230     C*                  ELSE
064900051230     C*                  SETON                                        8482      *SI FIGLIO
065000051230     C*                  MOVEL     '1'           ERRL00
065100051230     C*                  EVAL      DSMSMS = %trim(MSG(3))
065200051230     C*                  ENDIF
065300060404     C                   IF        d10err <> *blanks                            *NO FIGLIO
065400060404     C***                          OR d10cop =  VIDWWKNEW                       *SI FIGLIO DI "NEW"
065500051230     C                   ELSE
065600051230     C                   SETON                                        8482
065700051230     C                   MOVEL     '1'           ERRL00
065800051230     C                   EVAL      DSMSMS = %trim(MSG(11))+
065900051230     C                                      %trim(%editc(d10cop:'4'))
066000051230     C                   ENDIF
066100051229     C                   ENDIF
066200051229     C                   ENDIF
066300051229     C*
066400051229     C* VERIFICA CHE PADRE "NEW" - LEGAME 'ST' NN SIA GIA' FIGLIO DI ALTRO PADRE
066500060404     C* SE NN EVENTUALMENTE D PADRE "OLD" ALLA DATA CORRENTE
066600051229     C                   IF        ERRL00 = '0'
066700051229     C                   IF        VIDSTKNEW <> *zeros
066800051229     C                   EVAL      wTLETLE = 'ST'
066900051229     C                   EVAL      wTLECOD = VIDSTKNEW
067000051229     C                   EVAL      wTLETIP = 'P'
067100051229     C                   EVAL      wDATGGMMAAAA = VIDSTDDEC
067200051229     C                   EXSR      CHKDA8
067300051229     C                   EVAL      wTLEDAT = wDATAAAAMMGG
067400051229     C                   EXSR      REPUNI
067500060404     C                   IF        d10err <> *blanks                            *NO FIGLIO
067600060404     C                             OR d10cop =  VIDSTKOLD                       *SI FIGLIO DI "OLD"
067700051229     C                   ELSE
067800051229     C                   SETON                                        8483
067900051229     C                   MOVEL     '1'           ERRL00
068000051229     C                   EVAL      DSMSMS = %trim(MSG(2))+
068100051229     C                                      %trim(%editc(d10cop:'4'))
068200051229     C                   ENDIF
068300051229     C                   ENDIF
068400051229     C                   ENDIF
068500051229     C*
068600051229     C* VERIFICA DATA DECORRENZA (LEGAME 'ST') NN SIA INFERIORE ALLA MAGGIOR
068700051229     C* DATA DECORRENZA GIA' PRESENTE X LO STESSO TIPO LEGAME X IL CODICE PADRE "OLD"
068800051229     C                   IF        ERRL00 = '0'
068900051230     C                   IF        VIDSTKOLD <> *zeros
069000051230     C                   CLEAR                   wMAX_KUN_DDE
069100051229     C                   EVAL      K_kuntle = 'ST'
069200051229     C                   EVAL      K_kunazp = *blanks
069300051229     C                   EVAL      K_kuncop = VIDSTKOLD
069400051229     C     KEYkun01_P    SETLL     tikun01l
069500051229     C                   IF        %found(tikun01l)
069600051229     C     KEYkun01_P    READE     tikun01l
069700051229     C                   DOW       not %eof(tikun01l)
069800051229     C                   IF        kundde >= wMAX_KUN_DDE
069900051229     C                   EVAL      wMAX_KUN_DDE = kundde
070000051229     C                   ENDIF
070100051229     C     KEYkun01_P    READE     tikun01l
070200051229     C                   ENDDO
070300051229     C                   ENDIF
070400060209     C                   IF        wDATAAAAMMGG <= wMAX_KUN_DDE
070500051230     C                   SETON                                        8482
070600051229     C                   MOVEL     '1'           ERRL00
070700051229     C                   MOVEL     MSG(10)       DSMSMS
070800051229     C                   ENDIF
070900051230     C                   ENDIF
071000051229     C                   ENDIF
071100051230     C*
071200051230     C* VERIFICA DATA DECORRENZA (LEGAME 'ST') NN SIA INFERIORE ALLA MAGGIOR
071300051230     C* DATA DECORRENZA GIA' PRESENTE X LO STESSO TIPO LEGAME X IL CODICE PADRE "NEW"
071400051230     C                   IF        ERRL00 = '0'
071500051230     C                   IF        VIDSTKNEW <> *zeros
071600051230     C                   CLEAR                   wMAX_KUN_DDE
071700051230     C                   EVAL      K_kuntle = 'ST'
071800051230     C                   EVAL      K_kunazp = *blanks
071900051230     C                   EVAL      K_kuncop = VIDSTKNEW
072000051230     C     KEYkun01_P    SETLL     tikun01l
072100051230     C                   IF        %found(tikun01l)
072200051230     C     KEYkun01_P    READE     tikun01l
072300051230     C                   DOW       not %eof(tikun01l)
072400051230     C                   IF        kundde >= wMAX_KUN_DDE
072500051230     C                   EVAL      wMAX_KUN_DDE = kundde
072600051230     C                   ENDIF
072700051230     C     KEYkun01_P    READE     tikun01l
072800051230     C                   ENDDO
072900051230     C                   ENDIF
073000060209     C                   IF        wDATAAAAMMGG <= wMAX_KUN_DDE
073100051230     C                   SETON                                        8483
073200051230     C                   MOVEL     '1'           ERRL00
073300051230     C                   MOVEL     MSG(10)       DSMSMS
073400051230     C                   ENDIF
073500051230     C                   ENDIF
073600051230     C                   ENDIF
073700051230     C*
073800060110     C* VERIFICA DATA DECORRENZA (LEGAME 'WW') NN SIA INFERIORE ALLA MAGGIOR
073900051230     C* DATA DECORRENZA GIA' PRESENTE X LO STESSO TIPO LEGAME X IL CODICE PADRE "OLD"
074000051230     C                   IF        ERRL00 = '0'
074100051230     C                   IF        VIDWWKOLD <> *zeros
074200051230     C                   CLEAR                   wMAX_KUN_DDE
074300051230     C                   EVAL      K_kuntle = 'WW'
074400051230     C                   EVAL      K_kunazp = *blanks
074500051230     C                   EVAL      K_kuncop = VIDWWKOLD
074600051230     C     KEYkun01_P    SETLL     tikun01l
074700051230     C                   IF        %found(tikun01l)
074800051230     C     KEYkun01_P    READE     tikun01l
074900051230     C                   DOW       not %eof(tikun01l)
075000051230     C                   IF        kundde >= wMAX_KUN_DDE
075100051230     C                   EVAL      wMAX_KUN_DDE = kundde
075200051230     C                   ENDIF
075300051230     C     KEYkun01_P    READE     tikun01l
075400051230     C                   ENDDO
075500051230     C                   ENDIF
075600060110     C***                IF        wDATAAAAMMGG < wMAX_KUN_DDE
075700060209     C                   IF        datcor <= wMAX_KUN_DDE
075800051230     C                   SETON                                        80
075900051230     C                   MOVEL     '1'           ERRL00
076000060110     C                   MOVEL     wMAX_KUN_DDE  wDataAlfa         8
076100060110     C                   EVAL      DSMSMS = %trim(MSG(12)) + ' ' + wDataAlfa
076200051230     C                   ENDIF
076300051230     C                   ENDIF
076400051230     C                   ENDIF
076500051230     C*
076600060110     C* VERIFICA DATA DECORRENZA (LEGAME 'WW') NN SIA INFERIORE ALLA MAGGIOR
076700051230     C* DATA DECORRENZA GIA' PRESENTE X LO STESSO TIPO LEGAME X IL CODICE PADRE "NEW"
076800051230     C                   IF        ERRL00 = '0'
076900051230     C                   IF        VIDWWKNEW <> *zeros
077000051230     C                   CLEAR                   wMAX_KUN_DDE
077100051230     C                   EVAL      K_kuntle = 'WW'
077200051230     C                   EVAL      K_kunazp = *blanks
077300051230     C                   EVAL      K_kuncop = VIDWWKNEW
077400051230     C     KEYkun01_P    SETLL     tikun01l
077500051230     C                   IF        %found(tikun01l)
077600051230     C     KEYkun01_P    READE     tikun01l
077700051230     C                   DOW       not %eof(tikun01l)
077800051230     C                   IF        kundde >= wMAX_KUN_DDE
077900051230     C                   EVAL      wMAX_KUN_DDE = kundde
078000051230     C                   ENDIF
078100051230     C     KEYkun01_P    READE     tikun01l
078200051230     C                   ENDDO
078300051230     C                   ENDIF
078400060110     C***                IF        wDATAAAAMMGG < wMAX_KUN_DDE
078500060209     C                   IF        datcor <= wMAX_KUN_DDE
078600051230     C                   SETON                                        81
078700051230     C                   MOVEL     '1'           ERRL00
078800060110     C                   MOVEL     wMAX_KUN_DDE  wDataAlfa         8
078900060110     C                   EVAL      DSMSMS = %trim(MSG(12)) + ' ' + wDataAlfa
079000051230     C                   ENDIF
079100051230     C                   ENDIF
079200051230     C                   ENDIF
079300051230     C*
079400051230     C* VERIFICA NN SIANO PRESENTI ABILITAZIONI (X I SOLI SERVIZI INTERESSATI, OVVERO D TIPO "T&T")
079500051230     C* SUL CODICE PADRE "OLD"
079600051230     C                   IF        ERRL00 = '0'
079700051230     C                   IF        VIDWWKOLD <> *zeros
079800051230     C                   CLEAR                   wMAX_VSS_DDE
079900051230     C                   EVAL      KEYVSSKSU = *all'0'
080000051230     C                   MOVE      VIDWWKOLD     KEYVSSKSU
080100051230     C     KEYVSSKSU     SETLL     TIVSS02L
080200051230     C                   IF        %found(TIVSS02L)
080300051230     C     KEYVSSKSU     READE     TIVSS02L
080400051230     C                   DOW       not %eof(TIVSS02L)
080500051230     C     VSSISV        LOOKUP    skISV_TT                               30
080600051230     C                   IF        *IN30
080700051230     C                   IF        vssdde >= wMAX_VSS_DDE
080800051230     C                   EVAL      wMAX_VSS_DDE = vssdde
080900051230     C                   ENDIF
081000051230     C                   ENDIF
081100051230     C     KEYVSSKSU     READE     TIVSS02L
081200051230     C                   ENDDO
081300051230     C                   ENDIF
081400060209     C                   IF        datcor <= wMAX_VSS_DDE
081500051230     C                   SETON                                        80
081600051230     C                   MOVEL     '1'           ERRL00
081700051230     C                   MOVEL     MSG(13)       DSMSMS
081800051230     C                   ENDIF
081900051230     C                   ENDIF
082000051230     C                   ENDIF
082100051230     C*
082200051230     C* VERIFICA NN SIANO PRESENTI ABILITAZIONI (X I SOLI SERVIZI INTERESSATI, OVVERO D TIPO "T&T")
082300051230     C* SUL CODICE PADRE "NEW"
082400051230     C                   IF        ERRL00 = '0'
082500051230     C                   IF        VIDWWKNEW <> *zeros
082600051230     C                   CLEAR                   wMAX_VSS_DDE
082700051230     C                   EVAL      KEYVSSKSU = *all'0'
082800051230     C                   MOVE      VIDWWKNEW     KEYVSSKSU
082900051230     C     KEYVSSKSU     SETLL     TIVSS02L
083000051230     C                   IF        %found(TIVSS02L)
083100051230     C     KEYVSSKSU     READE     TIVSS02L
083200051230     C                   DOW       not %eof(TIVSS02L)
083300051230     C     VSSISV        LOOKUP    skISV_TT                               30
083400051230     C                   IF        *IN30
083500051230     C                   IF        vssdde >= wMAX_VSS_DDE
083600051230     C                   EVAL      wMAX_VSS_DDE = vssdde
083700051230     C                   ENDIF
083800051230     C                   ENDIF
083900051230     C     KEYVSSKSU     READE     TIVSS02L
084000051230     C                   ENDDO
084100051230     C                   ENDIF
084200060209     C                   IF        datcor <= wMAX_VSS_DDE
084300051230     C                   SETON                                        81
084400051230     C                   MOVEL     '1'           ERRL00
084500051230     C                   MOVEL     MSG(13)       DSMSMS
084600051230     C                   ENDIF
084700051230     C                   ENDIF
084800051230     C                   ENDIF
084900090806     C*
085000090806     C* VERIFICA CHE IL NUOVO PADRE NN SIA GIA' PRESENTE COME PADRE X UNO QUALUNQUE DEI
085100090806     C* LEGAMI DI TIPO 'W'
085200090806     C                   IF        ERRL00 = '0'
085300090806     C                   EVAL      K_kuncop = VIDWWKNEW
085400090806     C     K_kuncop      SETLL     tikun03l
085500090806     C                   IF        %found(tikun03l)
085600090806     C     K_kuncop      READE     tikun03l
085700090806     C                   DOW       not %eof(tikun03l)
085800090806     C                   IF        (datcor > kundsc    AND
085900090806     C                              20391231 > kundsc)   OR
086000090806     C                             (datcor < kundde    AND
086100090806     C                              20391231 < kundde)
086200090806     C                   ELSE
086300090806     C*
086400090806     C     KUNTLE        LOOKUP    skTIPTLEWW                             30
086500090806     C                   IF        *IN30
086600090806     C                   SETON                                        81
086700090806     C                   MOVEL     '1'           ERRL00
086800090806     C                   EVAL      DSMSMS = MSG(15)
086900090806     C                   LEAVE
087000090806     C                   ENDIF
087100090806     C*
087200090806     C                   ENDIF
087300090806     C     K_kuncop      READE     tikun03l
087400090806     C                   ENDDO
087500090806     C                   ENDIF
087600090806     C                   ENDIF
087700000306     C*
087800000306     C                   ENDSR
087900051229     C*------------------------------------------------------------------------*
088000051229     C* DECCLI - REPERIMENTO ATTRIBUTI CLIENTE
088100051229     C*------------------------------------------------------------------------*
088200051229     C     DECCLI        BEGSR
088300051229     C*
088400051229     C                   CLEAR                   BS69DS
088500051229     C                   CLEAR                   ACODS
088600051229     C                   CLEAR                   INDDS
088700051229     C                   CLEAR                   CLPDS
088800051229     C                   CLEAR                   CLSDS
088900051229     C                   MOVEL     KNSIF         I69SIF
089000051229     C                   Z-ADD     wCLICOD       I69KAC
089100051229     C                   CALL      'TIBS69R'
089200051229     C                   PARM                    BS69DS
089300051229     C                   PARM                    ACODS
089400051229     C                   PARM                    INDDS
089500051229     C                   PARM                    CLPDS
089600051229     C                   PARM                    CLSDS
089700051229     C     O69ERR        IFNE      '1'
089800051229     C                   MOVEL     ACORAG        wCLIRAG
089900051229     C                   ELSE
090000051229     C                   MOVEL     *ALL'*'       wCLIRAG
090100051229     C                   MOVEL     '1'           ERRL00
090200051229     C                   MOVEL     MSG(5)        DSMSMS
090300051229     C                   ENDIF
090400051229     C*
090500051229     C                   ENDSR
090600051229     C*------------------------------------------------------------------------*
090700051229     C* REPUNI - REPERIMENTO UNIFICAZIONI
090800051229     C*------------------------------------------------------------------------*
090900051229     C     REPUNI        BEGSR
091000051229     C*
091100051229     C                   clear                   tibs10ds
091200051229     C                   eval      d10drf = wTLEDAT                             *data riferimento
091300051229     C                   eval      d10tle = wTLETLE                             *tipo legame
091400051229     C                   eval      d10paf = wTLETIP                             *cerca padre/figlio
091500051229     C                   eval      d10cod = wTLECOD                             *cod cliente da tst
091600051229     C                   call      'TIBS10R'
091700051229     C                   parm                    tibs10ds
091800051229     C*
091900051229     C                   ENDSR
092000051229     C*------------------------------------------------------------------------*
092100051229     C* CHKDA8 - CONTROLLA LA DATA (8)
092200051229     C*------------------------------------------------------------------------*
092300051229     C     CHKDA8        BEGSR
092400051229     C*
092500051229     C                   Z-ADD     wDATGGMMAAAA  G08DAT
092600051229     C                   Z-ADD     *ZEROS        G08INV
092700051229     C                   MOVEL     '0'           G08ERR
092800051229     C                   CALL      'XSRDA8'
092900051229     C                   PARM                    WLBDA8
093000051229     C                   Z-ADD     G08DAT        wDATGGMMAAAA
093100051229     C                   Z-ADD     G08INV        wDATAAAAMMGG
093200051229     C     G08ERR        IFEQ      '1'
093300051229     C                   MOVEL     '1'           ERRL00
093400051229     C                   MOVEL     MSG(9)        DSMSMS
093500051229     C                   ENDIF
093600051229     C*
093700051229     C                   ENDSR
093800970526     C*------------------------------------------------------------------------*
093900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
094000970526     C*------------------------------------------------------------------------*
094100970526     C     AZZVID        BEGSR
094200060130     C*
094300060130     C* CAMPI DESCRITTIVI
094400060130     C                   EVAL      DSFIRS = RSUT
094500020211     C*
094600051229     C* CAMPI "IN"
094700051229     C                   MOVEL     *blanks       VIDWWKOLD
094800051229     C                   MOVEL     *blanks       VIDWWKNEW
094900051229     C                   MOVEL     *blanks       VIDSTKOLD
095000051229     C                   MOVEL     *blanks       VIDSTKNEW
095100051229     C                   Z-ADD     DATCORINV     VIDSTDDEC
095200051229     C*
095300051229     C* DECODIFICHE
095400051229     C                   MOVEL     *blanks       VDDWWKOLD
095500051229     C                   MOVEL     *blanks       VDDWWKNEW
095600051229     C                   MOVEL     *blanks       VDDSTKOLD
095700051229     C                   MOVEL     *blanks       VDDSTKNEW
095800970526     C*
095900970526     C                   ENDSR
096000970526     C*------------------------------------------------------------------------*
096100970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
096200970526     C*------------------------------------------------------------------------*
096300970526     C     IMPFUN        BEGSR
096400970526     C*
096500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
096600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
096700970526     C                   EXSR      FUNGES
096800970526     C*
096900970526     C                   ENDSR
097000051229     C*--------------------------------------------------------------------------------------------*
097100051229     C* CARTAB - CARICA LE TABELLE OCCORRENTI
097200051229     C*--------------------------------------------------------------------------------------------*
097300051229     C     CARTAB        BEGSR
097400051229     C*----------
097500051229     C* Tipi servizi
097600051229     C*----------
097700051229     C                   eval      tbecod = 'ISV'
097800051229     C     tbecod        setll     tntbe01l
097900060127     C     tbecod        reade(n)  tntbe01l
098000051229     C                   dow       not %eof(tntbe01l)
098100051229     C                   movel     tbeuni        disv
098200051229     C                   if        §isvrgr  = 'T'
098300051229     C                   add       1             jskISV_TT
098400051229     C                   eval      skISV_TT(jskISV_TT) = tbeke1
098500051229     C                   endif
098600060127     C     tbecod        reade(n)  tntbe01l
098700051229     C                   enddo
098800090806     C*----------
098900090806     C* Tipi unificazioni
099000090806     C*----------
099100090806     C                   eval      tbecod = 'TLE'
099200090806     C     tbecod        setll     tntbe01l
099300090806     C     tbecod        reade(n)  tntbe01l
099400090806     C                   dow       not %eof(tntbe01l)
099500090806     C                   movel     tbeuni        dtle
099600090806     C                   if        §tlegrp  = 'W'
099700090806     C                   add       1             jskTIPTLEWW
099800090806     C                   eval      skTIPTLEWW(jskTIPTLEWW) = tbeke1
099900090806     C                   endif
100000090806     C     tbecod        reade(n)  tntbe01l
100100090806     C                   enddo
100200051229     C*
100300051229     C                   ENDSR
100400051229     C*------------------------------------------------------------------------*
100500051229     C* REPDATIUTE - REPERISCE DATI UTENTE CORRENTE
100600051229     C*------------------------------------------------------------------------*
100700051229     C     REPDATIUTE    BEGSR
100800051229     C*
100900051229     C     *dtaara       define    §azute        azuteds
101000051229     C     *dtaara       define    §datiute      ddatiute
101100051229     C                   in(E)     *dtaara
101200051229     C                   If        %error  or rsut = *blanks
101300051229     C                   Clear                   Tibs34ds
101400051229     C                   Call      'TIBS34R'
101500051229     C                   Parm                    Tibs34ds
101600051229     C                   In        *dtaara
101700051229     C                   EndIf
101800051229     C*
101900051229     C* Carica le abilitazioni particolari del profilo
102000051229     C                   Movel     *blanks       wabi              2
102100051229     C                   Movel     utefaf        dute01
102200051229     C                   If        §utecli <> *Blanks
102300051229     C                   Eval      wabi = §utecli
102400051229     C                   Else
102500051229     C                   Eval      wabi = uteaut
102600051229     C                   EndIf
102700051229     C*
102800051229     C* Reperimento dei P.O. gestibili dall'utente per i codici clienti
102900051229     C                   Clear                   Trul31ds
103000051229     C                   Eval      i31abi = wabi
103100051229     C                   Eval      i31cdi = dutdis
103200051229     C                   Eval      i31car = dutare
103300051229     C                   Eval      i31cpo = dutpou
103400051229     C                   Call      'TRUL31R'
103500051229     C                   Parm                    kpjba
103600051229     C                   Parm                    Trul31ds
103700051229     C                   If        o31pog > *zeros
103800051229     C                   Movea     o31pog        skPOG
103900051229     C                   Else
104000060105     C                   Movel     '1'           ERRUTE            1
104100051229     C                   Movel     MSG(7)        DSMSMS
104200051229     C                   EndIf
104300051229     C*
104400051229     C                   ENDSR
104500970526     C*------------------------------------------------------------------------*
104600970526     C* *INZSR - OPERAZIONI INIZIALI
104700970526     C*------------------------------------------------------------------------*
104800970526     C     *INZSR        BEGSR
104900991217     C*
105000991217     C     *ENTRY        PLIST
105100991217     C                   PARM                    KPJBA
105200970526     C*
105300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
105400970526     C     XMSGPA        PLIST
105500970526     C                   PARM                    DSMSMS
105600970526     C                   PARM                    DSMSPG
105700970526     C                   PARM                    DSMSKE
105800970526     C                   PARM                    DSMSER
105900970526     C*
106000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
106100970526     C                   CALL      'XKEY'
106200970526     C                   PARM                    DSKEY
106300051229     C*---
106400051229     C* DEFINIZIONE CHIAVI
106500051229     C*---
106600051229     C     KEYkun01_P    KLIST
106700051229     C                   KFLD                    K_kuntle                       *TIPO LEGAME
106800051229     C                   KFLD                    K_kunazp                       *COD. AZIENDA PADRE
106900051229     C                   KFLD                    K_kuncop                       *COD. CLIENTE PADRE
107000051230     C*
107100051230     C     KEYkun02_P    KLIST
107200051230     C                   KFLD                    kuntle                         *TIPO LEGAME
107300051230     C                   KFLD                    kunazf                         *COD. AZIENDA FIGLIO
107400051230     C                   KFLD                    kuncof                         *COD. CLIENTE FIGLIO
107500051230     C                   KFLD                    kunazp                         *COD. AZIENDA PADRE
107600051230     C                   KFLD                    kuncop                         *COD. CLIENTE PADRE
107700060127     C*
107800060127     C     KEYtbe01_P    KLIST
107900060127     C                   KFLD                    tbeCOD
108000060127     C                   KFLD                    tbeKE1
108100970526     C*---
108200970526     C* VARIABILI NON RIFERITE AL DATA BASE
108300970526     C*---
108400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
108500991214     C                   MOVEL     *BLANKS       DEPOP1            3
108600970526     C*---
108700970526     C* VARIABILI DI CONTROLLO
108800970526     C*---
108900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
109000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
109100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
109200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
109300970526     C*
109400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
109500051229     C                   MOVEL     'TIS7C7R'     DSMSPG
109600051229     C                   MOVEL     'TIS7C7R'     MSGQUE
109700970526     C                   MOVEL     *BLANKS       MSGKEY
109800970526     C                   MOVEL     *BLANKS       DSMSMS
109900970528     C*
110000970528     C* CALCOLA LA DATA CORRENTE
110100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
110200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
110300020211     C                   Z-ADD     WN8           G08DAT
110400020211     C                   Z-ADD     *ZEROS        G08INV
110500020211     C                   MOVEL     '0'           G08ERR
110600020211     C                   CALL      'XSRDA8'
110700020211     C                   PARM                    WLBDA8
110800020211     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
110900051229     C                   Z-ADD     G08DAT        DATCORINV         8 0          *DATA CORRENTE AA/M/
111000970526     C*
111100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
111200970526     C                   EXSR      IMPFUN
111300970526     C*
111400970526     C                   ENDSR
111500970526     C*------------------------------------------------------------------------*
111600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
111700970526     C*------------------------------------------------------------------------*
111800970526     C     FUNGES        BEGSR
111900970526     C*
112000970526     C* PULISCO CAMPI DI LAVORO
112100970526     C                   Z-ADD     0             W                 5 0
112200970526     C*
112300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
112400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
112500970526     C     1             DO        24            I
112600970526     C                   MOVEL     FUD(I)        FUDRIG
112700970526     C*
112800970526     C* PULIZIA FUNZIONI PRECEDENTI
112900970526     C                   MOVEL     *BLANKS       FUV(I)
113000970526     C*
113100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
113200970526     C* (SALTA F24)
113300970526     C     FUDATT        IFEQ      'S'
113400970526     C     FUDFUN        ANDNE     '24'
113500970526     C                   ADD       1             W
113600970526     C                   MOVEL     FUDDES        FUV(W)
113700970526     C                   END
113800970526     C                   END                                                    DO WHILE
113900970526     C*
114000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
114100970526     C* ALTRIMENTI DISATTIVO F24
114200970526     C     1             DO        24            I
114300970526     C                   MOVEL     FUD(I)        FUDRIG
114400970526     C*
114500970526     C     FUDFUN        IFEQ      '24'
114600970526     C     W             IFGT      6
114700970526     C                   MOVEL     'S'           FUDATT
114800970526     C                   ELSE
114900970526     C                   MOVEL     *BLANKS       FUDATT
115000970526     C                   END                                                    W GT 8
115100970526     C                   MOVEL     FUDRIG        FUD(I)
115200970526     C                   END
115300970526     C                   END                                                    DO WHILE
115400970526     C*
115500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
115600970526     C     W             DIV       6             I
115700970526     C                   MVR                     Z
115800970526     C*
115900970526     C     Z             IFNE      0
116000970526     C     I             ADD       1             Z
116100970526     C                   ELSE
116200970526     C                   Z-ADD     I             Z                 5 0
116300970526     C                   END
116400970526     C*
116500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
116600970526     C                   ADD       1             FUNGRU
116700970526     C     FUNGRU        IFGT      Z
116800970526     C     FUNGRU        ORLT      1
116900970526     C                   Z-ADD     1             FUNGRU
117000970526     C                   END
117100970526     C*
117200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
117300970526     C     FUNGRU        IFEQ      1
117400970526     C                   MOVEL     FUV(1)        FUN001
117500970526     C                   MOVEL     FUV(2)        FUN002
117600970526     C                   MOVEL     FUV(3)        FUN003
117700970526     C                   MOVEL     FUV(4)        FUN004
117800970526     C                   MOVEL     FUV(5)        FUN005
117900970526     C                   MOVEL     FUV(6)        FUN006
118000970526     C                   END
118100970526     C*
118200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
118300970526     C     FUNGRU        IFEQ      2
118400970526     C                   MOVEL     FUV(7)        FUN001
118500970526     C                   MOVEL     FUV(8)        FUN002
118600970526     C                   MOVEL     FUV(9)        FUN003
118700970526     C                   MOVEL     FUV(10)       FUN004
118800970526     C                   MOVEL     FUV(11)       FUN005
118900970526     C                   MOVEL     FUV(12)       FUN006
119000970526     C                   END
119100970526     C*
119200970526     C* TERZO GRUPPO DI 6 FUNZIONI
119300970526     C     FUNGRU        IFEQ      3
119400970526     C                   MOVEL     FUV(13)       FUN001
119500970526     C                   MOVEL     FUV(14)       FUN002
119600970526     C                   MOVEL     FUV(15)       FUN003
119700970526     C                   MOVEL     FUV(16)       FUN004
119800970526     C                   MOVEL     FUV(17)       FUN005
119900970526     C                   MOVEL     FUV(18)       FUN006
120000970526     C                   END
120100970526     C*
120200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
120300970526     C     FUNGRU        IFEQ      4
120400970526     C                   MOVEL     FUV(19)       FUN001
120500970526     C                   MOVEL     FUV(20)       FUN002
120600970526     C                   MOVEL     FUV(21)       FUN003
120700970526     C                   MOVEL     FUV(22)       FUN004
120800970526     C                   MOVEL     *BLANKS       FUN005
120900970526     C                   MOVEL     *BLANKS       FUN006
121000970526     C                   END
121100970526     C*
121200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
121300970526     C                   MOVEL     *BLANKS       FUN007
121400970526     C                   MOVEL     *BLANKS       FUN008
121500970526     C*
121600970526     C     1             DO        24            I
121700970526     C                   MOVEL     FUD(I)        FUDRIG
121800970526     C     FUDFUN        IFEQ      '24'
121900970526     C     FUDATT        ANDEQ     'S'
122000970526     C                   MOVEL     FUDDES        FUN008
122100970526     C                   END
122200970526     C                   END                                                    DO
122300970526     C*
122400970526     C                   ENDSR
122500970526     C*------------------------------------------------------------------------*
122600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
122700970526     C*------------------------------------------------------------------------*
122800970526     C     FUNCON        BEGSR
122900970526     C*
123000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
123100970526     C                   EXSR      FUNVAR
123200970526     C*
123300970526     C* PULISCO CAMPI DI LAVORO
123400970526     C                   Z-ADD     0             I                 5 0
123500970526     C                   Z-ADD     0             W                 5 0
123600970526     C*
123700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
123800970526     C     FUNKEY        IFNE      *BLANKS
123900970526     C     1             DO        24            I
124000970526     C                   MOVEL     FUD(I)        FUDRIG
124100970526     C*
124200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
124300970526     C     FUNKEY        IFEQ      FUDFUN
124400970526     C     FUDATT        ANDEQ     'S'
124500970526     C                   Z-ADD     I             W
124600970526     C                   END
124700970526     C                   END                                                    DO WHILE
124800970526     C*
124900970526     C* FUNZIONE NON ATTIVA (ERRORE)
125000970526     C     W             IFEQ      0
125100970526     C*
125200970526     C* EMISSIONE MESSAGGIO DI ERRORE
125300970526     C                   MOVEL     MSG(1)        DSMSMS
125400970526     C*
125500970526     C                   END                                                    W=0
125600970526     C                   END                                                    FUNKEY NE *BLANKS
125700970526     C*
125800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
125900970526     C     W             IFNE      0
126000970526     C     FUNKEY        OREQ      *BLANKS
126100970526     C     FUNKEY        IFEQ      *BLANKS
126200970526     C                   EXSR      ENTGES
126300970526     C                   END
126400970526     C     FUNKEY        IFEQ      '03'
126500970526     C                   EXSR      F03GES
126600970526     C                   END
126700970526     C     FUNKEY        IFEQ      '06'
126800970526     C                   EXSR      F06GES
126900970526     C                   END
127000970526     C     FUNKEY        IFEQ      '12'
127100970526     C                   EXSR      F12GES
127200970526     C                   END
127300970526     C     FUNKEY        IFEQ      '24'
127400970526     C                   EXSR      F24GES
127500970526     C                   END
127600970526     C                   END
127700970526     C*
127800970526     C                   ENDSR
127900970526     C*------------------------------------------------------------------------*
128000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
128100970526     C*------------------------------------------------------------------------*
128200970526     C     FUNVAR        BEGSR
128300970526     C*
128400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
128500970526     C*
128600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
128700970526     C     KEY           IFEQ      ENTER
128800970526     C                   MOVEL     *BLANKS       FUNKEY
128900970526     C                   END
129000970526     C     KEY           IFEQ      F1
129100970526     C                   MOVEL     '01'          FUNKEY
129200970526     C                   END
129300970526     C     KEY           IFEQ      F2
129400970526     C                   MOVEL     '02'          FUNKEY
129500970526     C                   END
129600970526     C     KEY           IFEQ      F3
129700970526     C                   MOVEL     '03'          FUNKEY
129800970526     C                   END
129900970526     C     KEY           IFEQ      F4
130000970526     C                   MOVEL     '04'          FUNKEY
130100970526     C                   END
130200970526     C     KEY           IFEQ      F5
130300970526     C                   MOVEL     '05'          FUNKEY
130400970526     C                   END
130500970526     C     KEY           IFEQ      F6
130600970526     C                   MOVEL     '06'          FUNKEY
130700970526     C                   END
130800970526     C     KEY           IFEQ      F7
130900970526     C                   MOVEL     '07'          FUNKEY
131000970526     C                   END
131100970526     C     KEY           IFEQ      F8
131200970526     C                   MOVEL     '08'          FUNKEY
131300970526     C                   END
131400970526     C     KEY           IFEQ      F9
131500970526     C                   MOVEL     '09'          FUNKEY
131600970526     C                   END
131700970526     C     KEY           IFEQ      F10
131800970526     C                   MOVEL     '10'          FUNKEY
131900970526     C                   END
132000970526     C     KEY           IFEQ      F11
132100970526     C                   MOVEL     '11'          FUNKEY
132200970526     C                   END
132300970526     C     KEY           IFEQ      F12
132400970526     C                   MOVEL     '12'          FUNKEY
132500970526     C                   END
132600970526     C     KEY           IFEQ      F13
132700970526     C                   MOVEL     '13'          FUNKEY
132800970526     C                   END
132900970526     C     KEY           IFEQ      F14
133000970526     C                   MOVEL     '14'          FUNKEY
133100970526     C                   END
133200970526     C     KEY           IFEQ      F15
133300970526     C                   MOVEL     '15'          FUNKEY
133400970526     C                   END
133500970526     C     KEY           IFEQ      F16
133600970526     C                   MOVEL     '16'          FUNKEY
133700970526     C                   END
133800970526     C     KEY           IFEQ      F17
133900970526     C                   MOVEL     '17'          FUNKEY
134000970526     C                   END
134100970526     C     KEY           IFEQ      F18
134200970526     C                   MOVEL     '18'          FUNKEY
134300970526     C                   END
134400970526     C     KEY           IFEQ      F19
134500970526     C                   MOVEL     '19'          FUNKEY
134600970526     C                   END
134700970526     C     KEY           IFEQ      F20
134800970526     C                   MOVEL     '20'          FUNKEY
134900970526     C                   END
135000970526     C     KEY           IFEQ      F21
135100970526     C                   MOVEL     '21'          FUNKEY
135200970526     C                   END
135300970526     C     KEY           IFEQ      F22
135400970526     C                   MOVEL     '22'          FUNKEY
135500970526     C                   END
135600970526     C     KEY           IFEQ      F23
135700970526     C                   MOVEL     '23'          FUNKEY
135800970526     C                   END
135900970526     C     KEY           IFEQ      F24
136000970526     C                   MOVEL     '24'          FUNKEY
136100970526     C                   END
136200970526     C     KEY           IFEQ      ROLLUP
136300970526     C                   MOVEL     'RU'          FUNKEY
136400970526     C                   END
136500970526     C     KEY           IFEQ      HELP
136600970526     C                   MOVEL     'HP'          FUNKEY
136700970526     C                   END
136800970526     C                   ENDSR
136900970526     C*------------------------------------------------------------------------*
137000970526     C* NOTE COSTRUZIONE SCHIERE
137100970526     C*------------------------------------------------------------------------*
137200970526     C*
137300970526     C* -MSG-MESSAGGI
137400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
137500970526     C* (NON TOCCARE MSG,1 E MSG,2)
137600970526     C*
137700970526     C* -CMD-COMANDI
137800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
137900970526     C*
138000970526     C* -OPD-OPZIONI
138100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
138200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
138300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
138400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
138500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
138600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
138700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
138800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
138900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
139000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
139100970526     C*
139200970526     C* -FUD-FUNZIONI
139300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
139400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
139500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
139600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
139700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
139800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
139900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
140000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
140100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
140200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
140300970526     O*------------------------------------------------------------------------*
140400970526** MSG - MESSAGGI
140500051230Tasto funzionale non gestito                                                   1
140600051230Il "nuovo codice" unificante risulta già figlio del codice:                    2
140700051230Il "vecchio codice" non risulta essere attualmente unificante                  3
140800051230Campo obbligatorio                                                             4
140900051230Codice cliente inesistente                                                     5
141000051230Elaborazione effettuata                                                        6
141100051230Errore in caricamento dati utente, avvisare il CED                             7
141200051230Cliente non in gestione all'utente, impossibile eseguire richiesta             8
141300051230Data non valida                                                                9
141400051230Data indicata inferiore a data decorrenza di legame già esistente              10
141500051230Il "vecchio codice" risulta già figlio del codice:                             11
141600060209Già presenti per il cliente legami WW con decorrenza >= ad oggi                12
141700060209Già presenti per il cliente abilit. servizi con decorrenza >= ad oggi          13
141800060127Codici "vecchio" e "nuovo" sono uguali, la richiesta è inconsistente!          14
141900090806Il "nuovo codice" unificante risulta già padre su legame di tipo analogo.      15
142000051230                                                                               16
142100051230                                                                               17
142200051230                                                                               18
142300051230                                                                               19
142400051230                                                                               20
142500051230                                                                               21
142600051230                                                                               22
142700051230                                                                               23
142800051230                                                                               24
142900970526** FUD - FUNZIONI
143000970526                   -        (S/N)=N                        funz.01¬
143100970526                   -        (S/N)=N                        funz.02¬
143200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
143300970526                   -        (S/N)=N                        funz.04¬
143400970526                   -        (S/N)=N                        funz.05¬
143500051229F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
143600970527                   -        (S/N)=N                        funz.07¬
143700970527                   -        (S/N)=N                        funz.08¬
143800970527                   -        (S/N)=N                        funz.09¬
143900970527                   -        (S/N)=N                        funz.10¬
144000970526                   -        (S/N)=N                        funz.11¬
144100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
144200970526                   -        (S/N)=N                        funz.13¬
144300970526                   -        (S/N)=N                        funz.14¬
144400970526                   -        (S/N)=N                        funz.15¬
144500970526                   -        (S/N)=N                        funz.16¬
144600970526                   -        (S/N)=N                        funz.17¬
144700970526                   -        (S/N)=N                        funz.18¬
144800970526                   -        (S/N)=N                        funz.19¬
144900970526                   -        (S/N)=N                        funz.20¬
145000970526                   -        (S/N)=N                        funz.21¬
145100970526                   -        (S/N)=N                        funz.22¬
145200970526                   -        (S/N)=N                        funz.23¬
145300051229F24=Altri tasti    -        (S/N)=N                        funz.24¬
