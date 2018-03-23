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
007200130412     d Status         sds
007300130412     d  SDSpgm           *proc
007400130412     d  SDSprm           *parms
007500130412     d  SDSuser              254    263
007600051229     D*-------------------
007700051229     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
007800051229     D*-------------------
007900051229     D TIBS10DS      E DS
008000051229     D*-------------------
008100051229     D* DS REPERIMENTO DATI UTENTE CORRENTE
008200051229     D*-------------------
008300051229     D Tibs34ds      e ds
008400051229     D Azuteds       e ds                  Extname(Azute00f)
008500051229     D dDatiute      e ds
008600051229     D dute01        e ds
008700051229     D*-------------------
008800051229     D* DS REPERIMENTO P.O. IN GESTIONE
008900051229     D*-------------------
009000051229     D TRUL31DS      E DS
009100051229     D*-------------------
009200051229     D* DS RIDEFINIZIONE TABELLA 'ISV' - TIPI SERVIZI
009300051229     D*-------------------
009400051229     D DISV          E DS
009500060127     D*-------------------
009600060127     D* DS X SALVATAGGIO OCCORRENZE TABELLE
009700060127     D*-------------------
009800060127     D TBE_DS        e ds                  Extname(tntbe00f)
009900060127     D TBE_DS_SAV    e ds                  Extname(tntbe00f) Prefix(S_)
010000090806     D*-------------------
010100090806     D* DS TIPI LEGAMI UNIFICANTI
010200090806     D*-------------------
010300090806     D DTLE          e ds
010400051229     D*------------------
010500051229     D* VARIABILI DI WRK
010600051229     D*------------
010700051229     D wCLICOD         S                   LIKE(I69KAC) INZ(*zeros)
010800051229     D wCLIRAG         S                   LIKE(ACORAG) INZ(*blanks)
010900051229     D wTLETLE         S                   LIKE(D10TLE) INZ(*blanks)
011000051229     D wTLETIP         S                   LIKE(D10PAF) INZ(*blanks)
011100051229     D wTLECOD         S                   LIKE(D10COD) INZ(*zeros)
011200051229     D wTLEDAT         S                   LIKE(D10DRF) INZ(*zeros)
011300051229     D wDATGGMMAAAA    S                   LIKE(VIDSTDDEC) INZ(*zeros)
011400051229     D wDATAAAAMMGG    S                   LIKE(VIDSTDDEC) INZ(*zeros)
011500051229     D skPOG           S              3    DIM(250) INZ(*zeros)
011600051229     D skISV_TT        S              2    DIM(100) INZ(*blanks)
011700090806     D skTIPTLEWW      S              2    DIM(100) INZ(*blanks)
011800090806     D jskTIPTLEWW     S              3  0 INZ(*zeros)
011900051229     D jskISV_TT       S              3  0 INZ(*zeros)
012000051229     D wDATA_ISO       S               D   DATFMT(*ISO) INZ(D'1999-01-01')
012100051229     D KEYVSSKSU       S                   LIKE(VSSKSU)
012200051229     D K_kuntle        S                   LIKE(KUNTLE)
012300051229     D K_kunazp        S                   LIKE(KUNAZP)
012400051229     D K_kuncop        S                   LIKE(KUNCOP)
012500051229     D wMAX_KUN_DDE    S                   LIKE(KUNDDE) INZ(*zeros)
012600051230     D wMAX_VSS_DDE    S                   LIKE(VSSDDE) INZ(*zeros)
012700970526     C*------------------------------------------------------------------------*
012800000303     C* MAIN LINES
012900970526     C*------------------------------------------------------------------------*
013000051229     C*
013100051229     C* REPERIMENTO DATI UTENTE CORRENTE
013200051229     C                   EXSR      REPDATIUTE
013300051229     C*
013400051229     C* CARICAMENTO TABELLE
013500051229     C                   EXSR      CARTAB
013600051229     C*
013700051229     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
013800051229     C                   EXSR      AZZVID
013900970526     C*
014000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014100991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014400970526     C*
014500970526     C* EMETTE FORMATO VIDEO
014600970526     C                   EXSR      BUTFMR
014700970526     C*
014800970526     C* GESTIONE TASTI FUNZIONALI
014900970526     C                   EXSR      FUNCON
015000051229     C                   ENDDO
015100970526     C*
015200970526     C* FINE PGM
015300970526     C                   SETON                                        LR
015400970526     C*------------------------------------------------------------------------*
015500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
015600970526     C*------------------------------------------------------------------------*
015700970526     C     BUTFMR        BEGSR
015800970526     C*
015900970526     C* EMETTE LA TESTATA
016000051229     C                   WRITE     s7c7D01
016100970526     C*
016200970526     C* SPEDISCE EVENTUALE MESSAGGIO
016300970526     C                   EXSR      SNDMSG
016400970526     C*
016500970526     C* EMETTE LE FUNZIONI
016600051229     C                   WRITE     s7c7D02
016700970526     C*
016800970526     C* EMETTE IL VIDEO
016900051229     C                   EXFMT     s7c7D03
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     SNDMSG        BEGSR
017600970526     C*
017700970526     C* PULISCE IL SFILE MESSAGGI
017800970526     C                   MOVEL     '1'           *IN40
017900051229     C                   WRITE     s7c7DMC
018000970526     C                   MOVEL     '0'           *IN40
018100970526     C*
018200970526     C* VISUALIZZA IL MESSAGGIO
018300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018400970526     C                   CALL      'XMSG'        XMSGPA
018500051229     C     DSMSER        IFEQ      *ZEROS
018600970526     C                   MOVEL     DSMSKE        MSGKEY
018700051229     C                   ENDIF
018800970526     C                   Z-ADD     1             MSGULT
018900051229     C                   WRITE     s7c7DMS
019000051229     C                   WRITE     s7c7DMC
019100970526     C*
019200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019300970526     C                   MOVEL     *BLANKS       DSMSMS
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* ENTGES - GESTIONE ENTER -> INVIO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     ENTGES        BEGSR
020000970526     C*
020100970526     C* CONTROLLA I CAMPI IMMESSI
020200970526     C                   EXSR      DECODI
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F03GES - GESTIONE F03 -> FINE
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F03GES        BEGSR
020900970526     C*
021000970526     C* IMPOSTA "PREMUTO F03"
021100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500991214     C* F06GES - GESTIONE F06 -> CONFERMA
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F06GES        BEGSR
021800970526     C*
021900991214     C* CONTROLLA I CAMPI IMMESSI
022000991214     C                   EXSR      DECODI
022100051229     C*
022200051229     C* SE TUTTO OK => ELABORA
022300991214     C     ERRL00        IFEQ      '0'
022400051229     C*
022500991214     C* ESEGUE ROUTINE DI CONFERMA
022600051229     C* ===> 1° COSA DA FARE: LEGAMI
022700051229     C                   EXSR      EXE_LEGAMI
022800051229     C* ===> 2° COSA DA FARE: ABILITAZIONI
022900051229     C                   EXSR      EXE_ABIL
023000051229     C                   MOVEL     MSG(6)        DSMSMS
023100051229     C                   EXSR      AZZVID
023200991214     C                   ENDIF
023300970526     C*
023400991214     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600970526     C* F12GES - GESTIONE F12 -> RITORNO
023700970526     C*------------------------------------------------------------------------*
023800970526     C     F12GES        BEGSR
023900970526     C*
024000970526     C* IMPOSTA "PREMUTO F12"
024100051229     C                   MOVEL     '1'           F12L00                         *FINE *PGM
024200970526     C*
024300970526     C                   ENDSR
024400970526     C*------------------------------------------------------------------------*
024500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024600970526     C*------------------------------------------------------------------------*
024700970526     C     F24GES        BEGSR
024800970526     C*
024900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025000970526     C*
025100970526     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300051229     C* EXE_LEGAMI - OPERAZIONI DI ELABORAZIONE LEGATI (TIKUN00F)
025400970526     C*------------------------------------------------------------------------*
025500051229     C     EXE_LEGAMI    BEGSR
025600991214     C*
025700051229     C*********************
025800051229     C* ELABORO X LEGAMI 'WW'
025900051229     C*********************
026000051229     C                   IF        VIDWWKOLD <>  *zeros AND
026100051229     C                             VIDWWKNEW <>  *zeros
026200051229     C                   EVAL      K_kuntle = 'WW'
026300051229     C                   EVAL      K_kunazp = *blanks
026400051229     C                   EVAL      K_kuncop = VIDWWKOLD
026500051229     C     KEYkun01_P    SETLL     tikun01l
026600051229     C                   IF        %found(tikun01l)
026700051229     C     KEYkun01_P    READE     tikun01l
026800051229     C                   DOW       not %eof(tikun01l)
026900051229     C*
027000051229     C* ELABORO SOLO SE LEGAME NN GIA' SCADUTO
027100051229     C                   IF        kundsc >= datcor
027200051229     C*
027300051229     C* ===> 1° COSA DA FARE: FAR SCADERE LEGAMI ESISTENTI SU CODICE "OLD"
027400051229     C     *ISO          MOVE      datcor        wDATA_ISO
027500051229     C     wDATA_ISO     SUBDUR    1:*D          wDATA_ISO
027600051229     C     *ISO          MOVE      wDATA_ISO     kundsc
027700051230     C* SE TRA I FIGLI DEL CODICE PADRE "OLD" CI FOSSE GIA' IL CODICE PADRE "NEW"
027800060209     C* OLTRE A FARLO SCADERE LO ANNULLO ANCHE (LOGICAMENTE)
027900051230     C                   IF        kuncof = VIDWWKNEW
028000051230     C                   EVAL      kunvat = 'A'
028100051230     C                   ENDIF
028200051229     C                   EVAL      kunftr = *blanks
028300130412     C                   EVAL      KUNDUV = %dec(%date())
028400130412     C                   EVAL      KUNPUV = SDSUser
028500051229     C                   UPDATE    tikun000
028600051229     C*
028700051229     C* ===> 2° COSA DA FARE: INSERIRE NUOVO LEGAME SU CODICE "NEW"
028800051229     C*
028900051229     C* SE CODICE PADRE "NEW" DIVERSO DA FIGLIO CORRENTE INSERISCO NUOVO LEGAME
029000051229     C                   IF        VIDWWKNEW <> kuncof
029100051229     C                   EVAL      kuncop = VIDWWKNEW
029200051229     C                   EVAL      kundde = datcor
029300051229     C                   EVAL      kundsc = 20391231
029400051229     C                   EVAL      kunftr = *blanks
029500051229     C                   EVAL      kundtr = *zeros
029600130412     C                   EVAL      KUNDIM = %dec(%date())
029700130412     C                   EVAL      KUNDUV = %dec(%date())
029800130412     C                   EVAL      KUNPUV = SDSUser
029900060130     C                   WRITE(e)  tikun000
030000051229     C                   ENDIF
030100051229     C                   ENDIF
030200051229     C*
030300051229     C* PROSEGUO CON LA LETTURA DEI LEGAMI DEL CODICE "OLD"
030400051229     C     KEYkun01_P    READE     tikun01l
030500051229     C                   ENDDO
030600051229     C                   ENDIF
030700051229     C*
030800051229     C* ===> 3° COSA DA FARE: INSERIRE NUOVO LEGAME SU CODICE "NEW" CON FIGLIO PADRE "OLD"
030900051229     C                   CLEAR                   tikun000
031000051229     C                   EVAL      kuntle = 'WW'
031100051229     C                   EVAL      kuncop = VIDWWKNEW
031200051229     C                   EVAL      kuncof = VIDWWKOLD
031300051229     C                   EVAL      kundde = datcor
031400051229     C                   EVAL      kundsc = 20391231
031500051229     C                   EVAL      kundtr = *zeros
031600130412     C                   EVAL      KUNDIM = %dec(%date())
031700130412     C                   EVAL      KUNDUV = %dec(%date())
031800130412     C                   EVAL      KUNPUV = SDSUser
031900060130     C                   WRITE(e)  tikun000
032000051230     C                   ENDIF
032100051229     C*
032200051229     C*********************
032300051229     C* ELABORO X LEGAMI 'ST'
032400051229     C*********************
032500051229     C                   IF        VIDSTKOLD <>  *zeros AND
032600051229     C                             VIDSTKNEW <>  *zeros AND
032700051229     C                             VIDSTDDEC <>  *zeros
032800051229     C                   EVAL      K_kuntle = 'ST'
032900051229     C                   EVAL      K_kunazp = *blanks
033000051229     C                   EVAL      K_kuncop = VIDSTKOLD
033100051229     C     KEYkun01_P    SETLL     tikun01l
033200051229     C                   IF        %found(tikun01l)
033300051229     C     KEYkun01_P    READE     tikun01l
033400051229     C                   DOW       not %eof(tikun01l)
033500051229     C*
033600051229     C* ELABORO SOLO SE LEGAME NN GIA' SCADUTO
033700051229     C                   IF        kundsc >= wDATAAAAMMGG
033800051229     C*
033900051229     C* ===> 1° COSA DA FARE: FAR SCADERE LEGAMI ESISTENTI SU CODICE "OLD"
034000051229     C     *ISO          MOVE      wDATAAAAMMGG  wDATA_ISO
034100051229     C     wDATA_ISO     SUBDUR    1:*D          wDATA_ISO
034200051229     C     *ISO          MOVE      wDATA_ISO     kundsc
034300051229     C                   EVAL      kunftr = *blanks
034400130412     C                   EVAL      KUNDUV = %dec(%date())
034500130412     C                   EVAL      KUNPUV = SDSUser
034600051229     C                   UPDATE    tikun000
034700051229     C*
034800051229     C* ===> 2° COSA DA FARE: INSERIRE NUOVO LEGAME SU CODICE "NEW"
034900051229     C*
035000051229     C* SE CODICE PADRE "NEW" DIVERSO DA FIGLIO CORRENTE INSERISCO NUOVO LEGAME
035100051229     C                   IF        VIDSTKNEW <> kuncof
035200051229     C                   EVAL      kuncop = VIDSTKNEW
035300051229     C                   EVAL      kundde = wDATAAAAMMGG
035400051229     C                   EVAL      kundsc = 20391231
035500051229     C                   EVAL      kunftr = *blanks
035600130417     C                   EVAL      kundtr = *zeros
035700130412     C                   EVAL      KUNDIM = %dec(%date())
035800130412     C                   EVAL      KUNDUV = %dec(%date())
035900130412     C                   EVAL      KUNPUV = SDSUser
036000060130     C                   WRITE(e)  tikun000
036100051229     C                   ENDIF
036200051229     C                   ENDIF
036300051229     C*
036400051229     C* PROSEGUO CON LA LETTURA DEI LEGAMI DEL CODICE "OLD"
036500051229     C     KEYkun01_P    READE     tikun01l
036600051229     C                   ENDDO
036700051229     C                   ENDIF
036800051229     C*
036900051229     C* ===> 3° COSA DA FARE: INSERIRE NUOVO LEGAME SU CODICE "NEW" CON FIGLIO PADRE "OLD"
037000051229     C                   CLEAR                   tikun000
037100051229     C                   EVAL      kuntle = 'ST'
037200051229     C                   EVAL      kuncop = VIDSTKNEW
037300051229     C                   EVAL      kuncof = VIDSTKOLD
037400051229     C                   EVAL      kundde = wDATAAAAMMGG
037500051229     C                   EVAL      kundsc = 20391231
037600130417     C                   EVAL      kunftr = *blanks
037700130412     C                   EVAL      KUNDIM = %dec(%date())
037800130412     C                   EVAL      KUNDUV = %dec(%date())
037900130412     C                   EVAL      KUNPUV = SDSUser
038000060130     C                   WRITE(e)  tikun000
038100051229     C                   ENDIF
038200991214     C*
038300970526     C                   ENDSR
038400051229     C*------------------------------------------------------------------------*
038500051229     C* EXE_ABIL - OPERAZIONI DI ELABORAZIONE ABILITAZIONI (TIVSS00F)
038600051229     C*------------------------------------------------------------------------*
038700051229     C     EXE_ABIL      BEGSR
038800051229     C*
038900051229     C* SU TUTTE LE ABILITAZIONE (D QUALUNQUE UTENTE VSSSUN) AVENTE COME CODICE
039000051229     C* CLIENTE UNIFICANTE IL CODICE "OLD" - LEGAME 'WW' MODIFICO E IMPOSTO
039100051229     C* IL CODICE "NEW" - LEGAME 'WW'
039200051229     C                   IF        VIDWWKOLD <> *zeros AND
039300051229     C                             VIDWWKNEW <> *zeros
039400051229     C                   EVAL      KEYVSSKSU = *all'0'
039500051229     C                   MOVE      VIDWWKOLD     KEYVSSKSU
039600051229     C     KEYVSSKSU     SETLL     TIVSS02L
039700051229     C                   IF        %found(TIVSS02L)
039800051229     C     KEYVSSKSU     READE     TIVSS02L
039900051229     C                   DOW       not %eof(TIVSS02L)
040000091116     C                   IF        VSSTLE = 'WW' OR
040100091116     C                             VSSTLE = *blanks
040200051229     C     VSSISV        LOOKUP    skISV_TT                               30
040300051229     C                   IF        *IN30
040400051229     C                   EVAL      VSSKSU = *all'0'
040500051229     C                   MOVE      VIDWWKNEW     VSSKSU
040600051229     C                   EVAL      VSSFTR = *blanks
040700051229     C                   UPDATE    TIVSS000
040800060127     C*
040900060127     C* Verifico anche se codice "OLD" presente in tabella VAP
041000060127     C                   SETOFF                                       55
041100060127     C                   EVAL      tbeCOD = 'VAP'
041200060127     C                   EVAL      tbeKE1 = KEYVSSKSU
041300060127     C     KEYtbe01_P    CHAIN(n)  tntbe01l
041400060127     C                   IF        %found(tntbe01l)
041500060127     C                   EVAL      TBE_DS_SAV = TBE_DS
041600060127     C                   SETON                                        55
041700060127     C*
041800060127     C* Verifico se codice "NEW" presente in tabella VAP e se nn già presente
041900060127     C* inserisco nuovo record sul codice "NEW"
042000060127     C                   EVAL      tbeKE1 = VSSKSU
042100060127     C     KEYtbe01_P    CHAIN(n)  tntbe01l
042200060127     C                   IF        not %found(tntbe01l)
042300060127     C                   EVAL      TBE_DS = TBE_DS_SAV
042400060127     C                   EVAL      tbeKE1 = VSSKSU
042500060127     C                   EVAL      tbeFTR = *blanks
042600060127     C                   WRITE     TNTBE000
042700060127     C                   ENDIF
042800060127     C                   ENDIF
042900051229     C                   ENDIF
043000090803     C                   ENDIF
043100051229     C     KEYVSSKSU     READE     TIVSS02L
043200051229     C                   ENDDO
043300051229     C                   ENDIF
043400051229     C                   ENDIF
043500051229     C*
043600051229     C                   ENDSR
043700970526     C*------------------------------------------------------------------------*
043800991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
043900970526     C*------------------------------------------------------------------------*
044000051229     C     DECODI        BEGSR
044100051229     C*
044200051229     C* REIMPOSTA INDICATORI DI ERRORE
044300051229     C                   SETOFF                                       808182
044400051229     C                   SETOFF                                       8384
044500051229     C*
044600051229     C* REIMPOSTA FLAG DI ERRORE
044700051229     C                   MOVEL     '0'           ERRL00
044800060105     C*
044900060105     C* COME PRIMA COSA VERIFICO EVENTUALE ERRORE D AUTORIZZAZIONE UTENTE
045000060105     C                   IF        ERRUTE = '1'
045100060105     C                   MOVEL     '1'           ERRL00
045200060105     C                   MOVEL     MSG(7)        DSMSMS
045300060105     C                   ENDIF
045400060127     C*
045500060127     C* VERIFICA CODICI "OLD" E "NEW" DEVONO ESSERE DIVERSI
045600060127     C* ....LEGAME "ST"
045700060127     C                   IF        ERRL00 = '0'
045800060127     C                   IF        VIDSTKOLD <> *zeros AND
045900060127     C                             VIDSTKNEW <> *zeros
046000060127     C                   IF        VIDSTKOLD  = VIDSTKNEW
046100060127     C                   SETON                                        8283
046200060127     C                   MOVEL     '1'           ERRL00
046300060127     C                   MOVEL     MSG(14)       DSMSMS
046400060127     C                   ENDIF
046500060127     C                   ENDIF
046600060127     C                   ENDIF
046700060127     C* ....LEGAME "WW"
046800060127     C                   IF        ERRL00 = '0'
046900060127     C                   IF        VIDWWKOLD <> *zeros AND
047000060127     C                             VIDWWKNEW <> *zeros
047100060127     C                   IF        VIDWWKOLD  = VIDWWKNEW
047200060127     C                   SETON                                        8081
047300060127     C                   MOVEL     '1'           ERRL00
047400060127     C                   MOVEL     MSG(14)       DSMSMS
047500060127     C                   ENDIF
047600060127     C                   ENDIF
047700060127     C                   ENDIF
047800051229     C*
047900051229     C* VERIFICA OBBLIGATORIETA CAMPI "IN"
048000051229     C*
048100051229     C* CONTROLLI X OPERAZIONI LEGAME 'WW'
048200051229     C                   IF        ERRL00 = '0'
048300051229     C                   IF        VIDWWKOLD = *zeros AND
048400051229     C                             VIDWWKNEW = *zeros
048500051229     C                   ELSE
048600051229     C                   IF        VIDWWKOLD = *zeros
048700051229     C                   MOVEL     '1'           ERRL00
048800051229     C                   MOVEL     MSG(4)        DSMSMS
048900051229     C                   ELSE
049000051229     C                   EVAL      wCLICOD = VIDWWKOLD
049100051229     C                   EXSR      DECCLI
049200051229     C                   EVAL      VDDWWKOLD = wCLIRAG
049300051229     C                   ENDIF
049400051229     C                   IF        ERRL00 = '1'
049500051229     C                   SETON                                        80
049600051229     C                   ENDIF
049700051229     C*
049800051229     C                   IF        VIDWWKNEW = *zeros
049900051229     C                   MOVEL     '1'           ERRL00
050000051229     C                   MOVEL     MSG(4)        DSMSMS
050100051229     C                   ELSE
050200051229     C                   EVAL      wCLICOD = VIDWWKNEW
050300051229     C                   EXSR      DECCLI
050400051229     C                   EVAL      VDDWWKNEW = wCLIRAG
050500051229     C                   ENDIF
050600051229     C                   IF        ERRL00 = '1'
050700051229     C                   SETON                                        81
050800051229     C                   ENDIF
050900051229     C                   ENDIF
051000051229     C                   ENDIF
051100051229     C*
051200051229     C* CONTROLLI X OPERAZIONI LEGAME 'ST'
051300051229     C                   IF        ERRL00 = '0'
051400051229     C                   IF        VIDSTKOLD = *zeros AND
051500051229     C                             VIDSTKNEW = *zeros AND
051600051229     C                             VIDSTDDEC = *zeros
051700051229     C                   ELSE
051800051229     C                   IF        VIDSTKOLD = *zeros
051900051229     C                   MOVEL     '1'           ERRL00
052000051229     C                   MOVEL     MSG(4)        DSMSMS
052100051229     C                   ELSE
052200051229     C                   EVAL      wCLICOD = VIDSTKOLD
052300051229     C                   EXSR      DECCLI
052400051229     C                   EVAL      VDDSTKOLD = wCLIRAG
052500051229     C                   ENDIF
052600051229     C                   IF        ERRL00 = '1'
052700051229     C                   SETON                                        82
052800051229     C                   ENDIF
052900051229     C*
053000051229     C                   IF        VIDSTKNEW = *zeros
053100051229     C                   MOVEL     '1'           ERRL00
053200051229     C                   MOVEL     MSG(4)        DSMSMS
053300051229     C                   ELSE
053400051229     C                   EVAL      wCLICOD = VIDSTKNEW
053500051229     C                   EXSR      DECCLI
053600051229     C                   EVAL      VDDSTKNEW = wCLIRAG
053700051229     C                   ENDIF
053800051229     C                   IF        ERRL00 = '1'
053900051229     C                   SETON                                        83
054000051229     C                   ENDIF
054100051229     C*
054200051229     C                   IF        VIDSTDDEC = *zeros
054300051229     C                   MOVEL     '1'           ERRL00
054400051229     C                   MOVEL     MSG(4)        DSMSMS
054500051229     C                   ELSE
054600051229     C                   EVAL      wDATGGMMAAAA = VIDSTDDEC
054700051229     C                   EXSR      CHKDA8
054800051229     C                   EVAL      VIDSTDDEC = wDATGGMMAAAA
054900051229     C                   ENDIF
055000051229     C                   IF        ERRL00 = '1'
055100051229     C                   SETON                                        84
055200051229     C                   ENDIF
055300051229     C                   ENDIF
055400051229     C                   ENDIF
055500051229     C*
055600051229     C* VERIFICA ABILITAZIONE UTENTE CORRENTE AI CODICI CLIENTI INDICATI
055700051229     C*
055800051229     C* VERIFICA CODICE CLIENTE "OLD" - LEGAME 'WW'
055900051229     C                   IF        ERRL00 = '0'
056000051229     C                   IF        VIDWWKOLD <> *zeros
056100051229     C                   MOVEL     VIDWWKOLD     wPO_Alfa          3
056200051229     C     wPO_Alfa      LOOKUP    skPOG                                  30
056300051229     C                   IF        NOT *IN30
056400051229     C                   SETON                                        80
056500051229     C                   MOVEL     '1'           ERRL00
056600051229     C                   MOVEL     MSG(8)        DSMSMS
056700051229     C                   ENDIF
056800051229     C                   ENDIF
056900051229     C                   ENDIF
057000051229     C*
057100051229     C* VERIFICA CODICE CLIENTE "NEW" - LEGAME 'WW'
057200051229     C                   IF        ERRL00 = '0'
057300051229     C                   IF        VIDWWKNEW <> *zeros
057400051229     C                   MOVEL     VIDWWKNEW     wPO_Alfa          3
057500051229     C     wPO_Alfa      LOOKUP    skPOG                                  30
057600051229     C                   IF        NOT *IN30
057700051229     C                   SETON                                        81
057800051229     C                   MOVEL     '1'           ERRL00
057900051229     C                   MOVEL     MSG(8)        DSMSMS
058000051229     C                   ENDIF
058100051229     C                   ENDIF
058200051229     C                   ENDIF
058300051229     C*
058400051229     C* VERIFICA CODICE CLIENTE "OLD" - LEGAME 'ST'
058500051229     C                   IF        ERRL00 = '0'
058600051229     C                   IF        VIDSTKOLD <> *zeros
058700051229     C                   MOVEL     VIDSTKOLD     wPO_Alfa          3
058800051229     C     wPO_Alfa      LOOKUP    skPOG                                  30
058900051229     C                   IF        NOT *IN30
059000051229     C                   SETON                                        82
059100051229     C                   MOVEL     '1'           ERRL00
059200051229     C                   MOVEL     MSG(8)        DSMSMS
059300051229     C                   ENDIF
059400051229     C                   ENDIF
059500051229     C                   ENDIF
059600051229     C*
059700051229     C* VERIFICA CODICE CLIENTE "NEW" - LEGAME 'ST'
059800051229     C                   IF        ERRL00 = '0'
059900051229     C                   IF        VIDSTKNEW <> *zeros
060000051229     C                   MOVEL     VIDSTKNEW     wPO_Alfa          3
060100051229     C     wPO_Alfa      LOOKUP    skPOG                                  30
060200051229     C                   IF        NOT *IN30
060300051229     C                   SETON                                        83
060400051229     C                   MOVEL     '1'           ERRL00
060500051229     C                   MOVEL     MSG(8)        DSMSMS
060600051229     C                   ENDIF
060700051229     C                   ENDIF
060800090806     C                   ENDIF
060900051229     C*
061000051230     C* VERIFICA CHE PADRE "OLD" - LEGAME 'WW' ALLA DATA CORRENTE NN SIA GIA' FIGLIO
061100051230     C* DI CHIUNQUE X LEGAME 'WW'
061200051229     C                   IF        ERRL00 = '0'
061300051229     C                   IF        VIDWWKOLD <> *zeros
061400051229     C                   EVAL      wTLETLE = 'WW'
061500051229     C                   EVAL      wTLECOD = VIDWWKOLD
061600051230     C                   EVAL      wTLETIP = 'P'
061700051229     C                   EVAL      wTLEDAT = datcor
061800051230     C                   EXSR      REPUNI
061900051230     C*                  IF        d10err <> *blanks
062000051230     C*                  ELSE
062100051230     C*                  SETON                                        80        *SI FIGLIO
062200051230     C*                  MOVEL     '1'           ERRL00
062300051230     C*                  EVAL      DSMSMS = %trim(MSG(3))
062400051230     C*                  ENDIF
062500060404     C                   IF        d10err <> *blanks                            *NO FIGLIO
062600060404     C***                          OR d10cop =  VIDWWKNEW                       *SI FIGLIO DI "NEW"
062700051230     C                   ELSE
062800051230     C                   SETON                                        80
062900051230     C                   MOVEL     '1'           ERRL00
063000060404     C                   EVAL      DSMSMS = %trim(MSG(11))+
063100051230     C                                      %trim(%editc(d10cop:'4'))
063200051230     C                   ENDIF
063300051229     C                   ENDIF
063400051229     C                   ENDIF
063500051229     C*
063600051229     C* VERIFICA CHE PADRE "NEW" - LEGAME 'WW' NN SIA GIA' FIGLIO DI ALTRO PADRE
063700060404     C* SE NN EVENTUALMENTE D PADRE "OLD" ALLA DATA CORRENTE
063800051229     C                   IF        ERRL00 = '0'
063900051229     C                   IF        VIDWWKNEW <> *zeros
064000051229     C                   EVAL      wTLETLE = 'WW'
064100051229     C                   EVAL      wTLECOD = VIDWWKNEW
064200051229     C                   EVAL      wTLETIP = 'P'
064300051229     C                   EVAL      wTLEDAT = datcor
064400051229     C                   EXSR      REPUNI
064500060404     C                   IF        d10err <> *blanks                            *NO FIGLIO
064600060404     C                             OR d10cop =  VIDWWKOLD                       *SI FIGLIO DI "OLD"
064700051229     C                   ELSE
064800051229     C                   SETON                                        81
064900051229     C                   MOVEL     '1'           ERRL00
065000051229     C                   EVAL      DSMSMS = %trim(MSG(2))+
065100051229     C                                      %trim(%editc(d10cop:'4'))
065200051229     C                   ENDIF
065300051229     C                   ENDIF
065400051229     C                   ENDIF
065500051229     C*
065600051230     C* VERIFICA CHE PADRE "OLD" - LEGAME 'ST' ALLA DATA CORRENTE NN SIA GIA' FIGLIO
065700051230     C* DI CHIUNQUE X LEGAME 'ST'
065800051229     C                   IF        ERRL00 = '0'
065900051229     C                   IF        VIDSTKOLD <> *zeros
066000051229     C                   EVAL      wTLETLE = 'ST'
066100051229     C                   EVAL      wTLECOD = VIDSTKOLD
066200051230     C                   EVAL      wTLETIP = 'P'
066300051229     C                   EVAL      wDATGGMMAAAA = VIDSTDDEC
066400051229     C                   EXSR      CHKDA8
066500051229     C                   EVAL      wTLEDAT = wDATAAAAMMGG
066600051229     C                   EXSR      REPUNI
066700051230     C*                  IF        d10err <> *blanks
066800051230     C*                  ELSE
066900051230     C*                  SETON                                        8482      *SI FIGLIO
067000051230     C*                  MOVEL     '1'           ERRL00
067100051230     C*                  EVAL      DSMSMS = %trim(MSG(3))
067200051230     C*                  ENDIF
067300060404     C                   IF        d10err <> *blanks                            *NO FIGLIO
067400060404     C***                          OR d10cop =  VIDWWKNEW                       *SI FIGLIO DI "NEW"
067500051230     C                   ELSE
067600051230     C                   SETON                                        8482
067700051230     C                   MOVEL     '1'           ERRL00
067800051230     C                   EVAL      DSMSMS = %trim(MSG(11))+
067900051230     C                                      %trim(%editc(d10cop:'4'))
068000051230     C                   ENDIF
068100051229     C                   ENDIF
068200051229     C                   ENDIF
068300051229     C*
068400051229     C* VERIFICA CHE PADRE "NEW" - LEGAME 'ST' NN SIA GIA' FIGLIO DI ALTRO PADRE
068500060404     C* SE NN EVENTUALMENTE D PADRE "OLD" ALLA DATA CORRENTE
068600051229     C                   IF        ERRL00 = '0'
068700051229     C                   IF        VIDSTKNEW <> *zeros
068800051229     C                   EVAL      wTLETLE = 'ST'
068900051229     C                   EVAL      wTLECOD = VIDSTKNEW
069000051229     C                   EVAL      wTLETIP = 'P'
069100051229     C                   EVAL      wDATGGMMAAAA = VIDSTDDEC
069200051229     C                   EXSR      CHKDA8
069300051229     C                   EVAL      wTLEDAT = wDATAAAAMMGG
069400051229     C                   EXSR      REPUNI
069500060404     C                   IF        d10err <> *blanks                            *NO FIGLIO
069600060404     C                             OR d10cop =  VIDSTKOLD                       *SI FIGLIO DI "OLD"
069700051229     C                   ELSE
069800051229     C                   SETON                                        8483
069900051229     C                   MOVEL     '1'           ERRL00
070000051229     C                   EVAL      DSMSMS = %trim(MSG(2))+
070100051229     C                                      %trim(%editc(d10cop:'4'))
070200051229     C                   ENDIF
070300051229     C                   ENDIF
070400051229     C                   ENDIF
070500051229     C*
070600051229     C* VERIFICA DATA DECORRENZA (LEGAME 'ST') NN SIA INFERIORE ALLA MAGGIOR
070700051229     C* DATA DECORRENZA GIA' PRESENTE X LO STESSO TIPO LEGAME X IL CODICE PADRE "OLD"
070800051229     C                   IF        ERRL00 = '0'
070900051230     C                   IF        VIDSTKOLD <> *zeros
071000051230     C                   CLEAR                   wMAX_KUN_DDE
071100051229     C                   EVAL      K_kuntle = 'ST'
071200051229     C                   EVAL      K_kunazp = *blanks
071300051229     C                   EVAL      K_kuncop = VIDSTKOLD
071400051229     C     KEYkun01_P    SETLL     tikun01l
071500051229     C                   IF        %found(tikun01l)
071600051229     C     KEYkun01_P    READE     tikun01l
071700051229     C                   DOW       not %eof(tikun01l)
071800051229     C                   IF        kundde >= wMAX_KUN_DDE
071900051229     C                   EVAL      wMAX_KUN_DDE = kundde
072000051229     C                   ENDIF
072100051229     C     KEYkun01_P    READE     tikun01l
072200051229     C                   ENDDO
072300051229     C                   ENDIF
072400060209     C                   IF        wDATAAAAMMGG <= wMAX_KUN_DDE
072500051230     C                   SETON                                        8482
072600051229     C                   MOVEL     '1'           ERRL00
072700051229     C                   MOVEL     MSG(10)       DSMSMS
072800051229     C                   ENDIF
072900051230     C                   ENDIF
073000051229     C                   ENDIF
073100051230     C*
073200051230     C* VERIFICA DATA DECORRENZA (LEGAME 'ST') NN SIA INFERIORE ALLA MAGGIOR
073300051230     C* DATA DECORRENZA GIA' PRESENTE X LO STESSO TIPO LEGAME X IL CODICE PADRE "NEW"
073400051230     C                   IF        ERRL00 = '0'
073500051230     C                   IF        VIDSTKNEW <> *zeros
073600051230     C                   CLEAR                   wMAX_KUN_DDE
073700051230     C                   EVAL      K_kuntle = 'ST'
073800051230     C                   EVAL      K_kunazp = *blanks
073900051230     C                   EVAL      K_kuncop = VIDSTKNEW
074000051230     C     KEYkun01_P    SETLL     tikun01l
074100051230     C                   IF        %found(tikun01l)
074200051230     C     KEYkun01_P    READE     tikun01l
074300051230     C                   DOW       not %eof(tikun01l)
074400051230     C                   IF        kundde >= wMAX_KUN_DDE
074500051230     C                   EVAL      wMAX_KUN_DDE = kundde
074600051230     C                   ENDIF
074700051230     C     KEYkun01_P    READE     tikun01l
074800051230     C                   ENDDO
074900051230     C                   ENDIF
075000060209     C                   IF        wDATAAAAMMGG <= wMAX_KUN_DDE
075100051230     C                   SETON                                        8483
075200051230     C                   MOVEL     '1'           ERRL00
075300051230     C                   MOVEL     MSG(10)       DSMSMS
075400051230     C                   ENDIF
075500051230     C                   ENDIF
075600051230     C                   ENDIF
075700051230     C*
075800060110     C* VERIFICA DATA DECORRENZA (LEGAME 'WW') NN SIA INFERIORE ALLA MAGGIOR
075900051230     C* DATA DECORRENZA GIA' PRESENTE X LO STESSO TIPO LEGAME X IL CODICE PADRE "OLD"
076000051230     C                   IF        ERRL00 = '0'
076100051230     C                   IF        VIDWWKOLD <> *zeros
076200051230     C                   CLEAR                   wMAX_KUN_DDE
076300051230     C                   EVAL      K_kuntle = 'WW'
076400051230     C                   EVAL      K_kunazp = *blanks
076500051230     C                   EVAL      K_kuncop = VIDWWKOLD
076600051230     C     KEYkun01_P    SETLL     tikun01l
076700051230     C                   IF        %found(tikun01l)
076800051230     C     KEYkun01_P    READE     tikun01l
076900051230     C                   DOW       not %eof(tikun01l)
077000051230     C                   IF        kundde >= wMAX_KUN_DDE
077100051230     C                   EVAL      wMAX_KUN_DDE = kundde
077200051230     C                   ENDIF
077300051230     C     KEYkun01_P    READE     tikun01l
077400051230     C                   ENDDO
077500051230     C                   ENDIF
077600060110     C***                IF        wDATAAAAMMGG < wMAX_KUN_DDE
077700060209     C                   IF        datcor <= wMAX_KUN_DDE
077800051230     C                   SETON                                        80
077900051230     C                   MOVEL     '1'           ERRL00
078000060110     C                   MOVEL     wMAX_KUN_DDE  wDataAlfa         8
078100060110     C                   EVAL      DSMSMS = %trim(MSG(12)) + ' ' + wDataAlfa
078200051230     C                   ENDIF
078300051230     C                   ENDIF
078400051230     C                   ENDIF
078500051230     C*
078600060110     C* VERIFICA DATA DECORRENZA (LEGAME 'WW') NN SIA INFERIORE ALLA MAGGIOR
078700051230     C* DATA DECORRENZA GIA' PRESENTE X LO STESSO TIPO LEGAME X IL CODICE PADRE "NEW"
078800051230     C                   IF        ERRL00 = '0'
078900051230     C                   IF        VIDWWKNEW <> *zeros
079000051230     C                   CLEAR                   wMAX_KUN_DDE
079100051230     C                   EVAL      K_kuntle = 'WW'
079200051230     C                   EVAL      K_kunazp = *blanks
079300051230     C                   EVAL      K_kuncop = VIDWWKNEW
079400051230     C     KEYkun01_P    SETLL     tikun01l
079500051230     C                   IF        %found(tikun01l)
079600051230     C     KEYkun01_P    READE     tikun01l
079700051230     C                   DOW       not %eof(tikun01l)
079800051230     C                   IF        kundde >= wMAX_KUN_DDE
079900051230     C                   EVAL      wMAX_KUN_DDE = kundde
080000051230     C                   ENDIF
080100051230     C     KEYkun01_P    READE     tikun01l
080200051230     C                   ENDDO
080300051230     C                   ENDIF
080400060110     C***                IF        wDATAAAAMMGG < wMAX_KUN_DDE
080500060209     C                   IF        datcor <= wMAX_KUN_DDE
080600051230     C                   SETON                                        81
080700051230     C                   MOVEL     '1'           ERRL00
080800060110     C                   MOVEL     wMAX_KUN_DDE  wDataAlfa         8
080900060110     C                   EVAL      DSMSMS = %trim(MSG(12)) + ' ' + wDataAlfa
081000051230     C                   ENDIF
081100051230     C                   ENDIF
081200051230     C                   ENDIF
081300051230     C*
081400051230     C* VERIFICA NN SIANO PRESENTI ABILITAZIONI (X I SOLI SERVIZI INTERESSATI, OVVERO D TIPO "T&T")
081500051230     C* SUL CODICE PADRE "OLD"
081600051230     C                   IF        ERRL00 = '0'
081700051230     C                   IF        VIDWWKOLD <> *zeros
081800051230     C                   CLEAR                   wMAX_VSS_DDE
081900051230     C                   EVAL      KEYVSSKSU = *all'0'
082000051230     C                   MOVE      VIDWWKOLD     KEYVSSKSU
082100051230     C     KEYVSSKSU     SETLL     TIVSS02L
082200051230     C                   IF        %found(TIVSS02L)
082300051230     C     KEYVSSKSU     READE     TIVSS02L
082400051230     C                   DOW       not %eof(TIVSS02L)
082500051230     C     VSSISV        LOOKUP    skISV_TT                               30
082600051230     C                   IF        *IN30
082700051230     C                   IF        vssdde >= wMAX_VSS_DDE
082800051230     C                   EVAL      wMAX_VSS_DDE = vssdde
082900051230     C                   ENDIF
083000051230     C                   ENDIF
083100051230     C     KEYVSSKSU     READE     TIVSS02L
083200051230     C                   ENDDO
083300051230     C                   ENDIF
083400060209     C                   IF        datcor <= wMAX_VSS_DDE
083500051230     C                   SETON                                        80
083600051230     C                   MOVEL     '1'           ERRL00
083700051230     C                   MOVEL     MSG(13)       DSMSMS
083800051230     C                   ENDIF
083900051230     C                   ENDIF
084000051230     C                   ENDIF
084100051230     C*
084200051230     C* VERIFICA NN SIANO PRESENTI ABILITAZIONI (X I SOLI SERVIZI INTERESSATI, OVVERO D TIPO "T&T")
084300051230     C* SUL CODICE PADRE "NEW"
084400051230     C                   IF        ERRL00 = '0'
084500051230     C                   IF        VIDWWKNEW <> *zeros
084600051230     C                   CLEAR                   wMAX_VSS_DDE
084700051230     C                   EVAL      KEYVSSKSU = *all'0'
084800051230     C                   MOVE      VIDWWKNEW     KEYVSSKSU
084900051230     C     KEYVSSKSU     SETLL     TIVSS02L
085000051230     C                   IF        %found(TIVSS02L)
085100051230     C     KEYVSSKSU     READE     TIVSS02L
085200051230     C                   DOW       not %eof(TIVSS02L)
085300051230     C     VSSISV        LOOKUP    skISV_TT                               30
085400051230     C                   IF        *IN30
085500051230     C                   IF        vssdde >= wMAX_VSS_DDE
085600051230     C                   EVAL      wMAX_VSS_DDE = vssdde
085700051230     C                   ENDIF
085800051230     C                   ENDIF
085900051230     C     KEYVSSKSU     READE     TIVSS02L
086000051230     C                   ENDDO
086100051230     C                   ENDIF
086200060209     C                   IF        datcor <= wMAX_VSS_DDE
086300051230     C                   SETON                                        81
086400051230     C                   MOVEL     '1'           ERRL00
086500051230     C                   MOVEL     MSG(13)       DSMSMS
086600051230     C                   ENDIF
086700051230     C                   ENDIF
086800051230     C                   ENDIF
086900090806     C*
087000090806     C* VERIFICA CHE IL NUOVO PADRE NN SIA GIA' PRESENTE COME PADRE X UNO QUALUNQUE DEI
087100090806     C* LEGAMI DI TIPO 'W'
087200090806     C                   IF        ERRL00 = '0'
087300090806     C                   EVAL      K_kuncop = VIDWWKNEW
087400090806     C     K_kuncop      SETLL     tikun03l
087500090806     C                   IF        %found(tikun03l)
087600090806     C     K_kuncop      READE     tikun03l
087700090806     C                   DOW       not %eof(tikun03l)
087800090806     C                   IF        (datcor > kundsc    AND
087900090806     C                              20391231 > kundsc)   OR
088000090806     C                             (datcor < kundde    AND
088100090806     C                              20391231 < kundde)
088200090806     C                   ELSE
088300090806     C*
088400090806     C     KUNTLE        LOOKUP    skTIPTLEWW                             30
088500090806     C                   IF        *IN30
088600090806     C                   SETON                                        81
088700090806     C                   MOVEL     '1'           ERRL00
088800090806     C                   EVAL      DSMSMS = MSG(15)
088900090806     C                   LEAVE
089000090806     C                   ENDIF
089100090806     C*
089200090806     C                   ENDIF
089300090806     C     K_kuncop      READE     tikun03l
089400090806     C                   ENDDO
089500090806     C                   ENDIF
089600090806     C                   ENDIF
089700000306     C*
089800000306     C                   ENDSR
089900051229     C*------------------------------------------------------------------------*
090000051229     C* DECCLI - REPERIMENTO ATTRIBUTI CLIENTE
090100051229     C*------------------------------------------------------------------------*
090200051229     C     DECCLI        BEGSR
090300051229     C*
090400051229     C                   CLEAR                   BS69DS
090500051229     C                   CLEAR                   ACODS
090600051229     C                   CLEAR                   INDDS
090700051229     C                   CLEAR                   CLPDS
090800051229     C                   CLEAR                   CLSDS
090900051229     C                   MOVEL     KNSIF         I69SIF
091000051229     C                   Z-ADD     wCLICOD       I69KAC
091100051229     C                   CALL      'TIBS69R'
091200051229     C                   PARM                    BS69DS
091300051229     C                   PARM                    ACODS
091400051229     C                   PARM                    INDDS
091500051229     C                   PARM                    CLPDS
091600051229     C                   PARM                    CLSDS
091700051229     C     O69ERR        IFNE      '1'
091800051229     C                   MOVEL     ACORAG        wCLIRAG
091900051229     C                   ELSE
092000051229     C                   MOVEL     *ALL'*'       wCLIRAG
092100051229     C                   MOVEL     '1'           ERRL00
092200051229     C                   MOVEL     MSG(5)        DSMSMS
092300051229     C                   ENDIF
092400051229     C*
092500051229     C                   ENDSR
092600051229     C*------------------------------------------------------------------------*
092700051229     C* REPUNI - REPERIMENTO UNIFICAZIONI
092800051229     C*------------------------------------------------------------------------*
092900051229     C     REPUNI        BEGSR
093000051229     C*
093100051229     C                   clear                   tibs10ds
093200051229     C                   eval      d10drf = wTLEDAT                             *data riferimento
093300051229     C                   eval      d10tle = wTLETLE                             *tipo legame
093400051229     C                   eval      d10paf = wTLETIP                             *cerca padre/figlio
093500051229     C                   eval      d10cod = wTLECOD                             *cod cliente da tst
093600051229     C                   call      'TIBS10R'
093700051229     C                   parm                    tibs10ds
093800051229     C*
093900051229     C                   ENDSR
094000051229     C*------------------------------------------------------------------------*
094100051229     C* CHKDA8 - CONTROLLA LA DATA (8)
094200051229     C*------------------------------------------------------------------------*
094300051229     C     CHKDA8        BEGSR
094400051229     C*
094500051229     C                   Z-ADD     wDATGGMMAAAA  G08DAT
094600051229     C                   Z-ADD     *ZEROS        G08INV
094700051229     C                   MOVEL     '0'           G08ERR
094800051229     C                   CALL      'XSRDA8'
094900051229     C                   PARM                    WLBDA8
095000051229     C                   Z-ADD     G08DAT        wDATGGMMAAAA
095100051229     C                   Z-ADD     G08INV        wDATAAAAMMGG
095200051229     C     G08ERR        IFEQ      '1'
095300051229     C                   MOVEL     '1'           ERRL00
095400051229     C                   MOVEL     MSG(9)        DSMSMS
095500051229     C                   ENDIF
095600051229     C*
095700051229     C                   ENDSR
095800970526     C*------------------------------------------------------------------------*
095900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
096000970526     C*------------------------------------------------------------------------*
096100970526     C     AZZVID        BEGSR
096200060130     C*
096300060130     C* CAMPI DESCRITTIVI
096400060130     C                   EVAL      DSFIRS = RSUT
096500020211     C*
096600051229     C* CAMPI "IN"
096700051229     C                   MOVEL     *blanks       VIDWWKOLD
096800051229     C                   MOVEL     *blanks       VIDWWKNEW
096900051229     C                   MOVEL     *blanks       VIDSTKOLD
097000051229     C                   MOVEL     *blanks       VIDSTKNEW
097100051229     C                   Z-ADD     DATCORINV     VIDSTDDEC
097200051229     C*
097300051229     C* DECODIFICHE
097400051229     C                   MOVEL     *blanks       VDDWWKOLD
097500051229     C                   MOVEL     *blanks       VDDWWKNEW
097600051229     C                   MOVEL     *blanks       VDDSTKOLD
097700051229     C                   MOVEL     *blanks       VDDSTKNEW
097800970526     C*
097900970526     C                   ENDSR
098000970526     C*------------------------------------------------------------------------*
098100970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
098200970526     C*------------------------------------------------------------------------*
098300970526     C     IMPFUN        BEGSR
098400970526     C*
098500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
098600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
098700970526     C                   EXSR      FUNGES
098800970526     C*
098900970526     C                   ENDSR
099000051229     C*--------------------------------------------------------------------------------------------*
099100051229     C* CARTAB - CARICA LE TABELLE OCCORRENTI
099200051229     C*--------------------------------------------------------------------------------------------*
099300051229     C     CARTAB        BEGSR
099400051229     C*----------
099500051229     C* Tipi servizi
099600051229     C*----------
099700051229     C                   eval      tbecod = 'ISV'
099800051229     C     tbecod        setll     tntbe01l
099900060127     C     tbecod        reade(n)  tntbe01l
100000051229     C                   dow       not %eof(tntbe01l)
100100051229     C                   movel     tbeuni        disv
100200051229     C                   if        §isvrgr  = 'T'
100300051229     C                   add       1             jskISV_TT
100400051229     C                   eval      skISV_TT(jskISV_TT) = tbeke1
100500051229     C                   endif
100600060127     C     tbecod        reade(n)  tntbe01l
100700051229     C                   enddo
100800090806     C*----------
100900090806     C* Tipi unificazioni
101000090806     C*----------
101100090806     C                   eval      tbecod = 'TLE'
101200090806     C     tbecod        setll     tntbe01l
101300090806     C     tbecod        reade(n)  tntbe01l
101400090806     C                   dow       not %eof(tntbe01l)
101500090806     C                   movel     tbeuni        dtle
101600090806     C                   if        §tlegrp  = 'W'
101700090806     C                   add       1             jskTIPTLEWW
101800090806     C                   eval      skTIPTLEWW(jskTIPTLEWW) = tbeke1
101900090806     C                   endif
102000090806     C     tbecod        reade(n)  tntbe01l
102100090806     C                   enddo
102200051229     C*
102300051229     C                   ENDSR
102400051229     C*------------------------------------------------------------------------*
102500051229     C* REPDATIUTE - REPERISCE DATI UTENTE CORRENTE
102600051229     C*------------------------------------------------------------------------*
102700051229     C     REPDATIUTE    BEGSR
102800051229     C*
102900051229     C     *dtaara       define    §azute        azuteds
103000051229     C     *dtaara       define    §datiute      ddatiute
103100051229     C                   in(E)     *dtaara
103200051229     C                   If        %error  or rsut = *blanks
103300051229     C                   Clear                   Tibs34ds
103400051229     C                   Call      'TIBS34R'
103500051229     C                   Parm                    Tibs34ds
103600051229     C                   In        *dtaara
103700051229     C                   EndIf
103800051229     C*
103900051229     C* Carica le abilitazioni particolari del profilo
104000051229     C                   Movel     *blanks       wabi              2
104100051229     C                   Movel     utefaf        dute01
104200051229     C                   If        §utecli <> *Blanks
104300051229     C                   Eval      wabi = §utecli
104400051229     C                   Else
104500051229     C                   Eval      wabi = uteaut
104600051229     C                   EndIf
104700051229     C*
104800051229     C* Reperimento dei P.O. gestibili dall'utente per i codici clienti
104900051229     C                   Clear                   Trul31ds
105000051229     C                   Eval      i31abi = wabi
105100051229     C                   Eval      i31cdi = dutdis
105200051229     C                   Eval      i31car = dutare
105300051229     C                   Eval      i31cpo = dutpou
105400051229     C                   Call      'TRUL31R'
105500051229     C                   Parm                    kpjba
105600051229     C                   Parm                    Trul31ds
105700051229     C                   If        o31pog > *zeros
105800051229     C                   Movea     o31pog        skPOG
105900051229     C                   Else
106000060105     C                   Movel     '1'           ERRUTE            1
106100051229     C                   Movel     MSG(7)        DSMSMS
106200051229     C                   EndIf
106300051229     C*
106400051229     C                   ENDSR
106500970526     C*------------------------------------------------------------------------*
106600970526     C* *INZSR - OPERAZIONI INIZIALI
106700970526     C*------------------------------------------------------------------------*
106800970526     C     *INZSR        BEGSR
106900991217     C*
107000991217     C     *ENTRY        PLIST
107100991217     C                   PARM                    KPJBA
107200970526     C*
107300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
107400970526     C     XMSGPA        PLIST
107500970526     C                   PARM                    DSMSMS
107600970526     C                   PARM                    DSMSPG
107700970526     C                   PARM                    DSMSKE
107800970526     C                   PARM                    DSMSER
107900970526     C*
108000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
108100970526     C                   CALL      'XKEY'
108200970526     C                   PARM                    DSKEY
108300051229     C*---
108400051229     C* DEFINIZIONE CHIAVI
108500051229     C*---
108600051229     C     KEYkun01_P    KLIST
108700051229     C                   KFLD                    K_kuntle                       *TIPO LEGAME
108800051229     C                   KFLD                    K_kunazp                       *COD. AZIENDA PADRE
108900051229     C                   KFLD                    K_kuncop                       *COD. CLIENTE PADRE
109000051230     C*
109100051230     C     KEYkun02_P    KLIST
109200051230     C                   KFLD                    kuntle                         *TIPO LEGAME
109300051230     C                   KFLD                    kunazf                         *COD. AZIENDA FIGLIO
109400051230     C                   KFLD                    kuncof                         *COD. CLIENTE FIGLIO
109500051230     C                   KFLD                    kunazp                         *COD. AZIENDA PADRE
109600051230     C                   KFLD                    kuncop                         *COD. CLIENTE PADRE
109700060127     C*
109800060127     C     KEYtbe01_P    KLIST
109900060127     C                   KFLD                    tbeCOD
110000060127     C                   KFLD                    tbeKE1
110100970526     C*---
110200970526     C* VARIABILI NON RIFERITE AL DATA BASE
110300970526     C*---
110400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
110500991214     C                   MOVEL     *BLANKS       DEPOP1            3
110600970526     C*---
110700970526     C* VARIABILI DI CONTROLLO
110800970526     C*---
110900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
111000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
111100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
111200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
111300970526     C*
111400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
111500051229     C                   MOVEL     'TIS7C7R'     DSMSPG
111600051229     C                   MOVEL     'TIS7C7R'     MSGQUE
111700970526     C                   MOVEL     *BLANKS       MSGKEY
111800970526     C                   MOVEL     *BLANKS       DSMSMS
111900970528     C*
112000970528     C* CALCOLA LA DATA CORRENTE
112100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
112200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
112300020211     C                   Z-ADD     WN8           G08DAT
112400020211     C                   Z-ADD     *ZEROS        G08INV
112500020211     C                   MOVEL     '0'           G08ERR
112600020211     C                   CALL      'XSRDA8'
112700020211     C                   PARM                    WLBDA8
112800020211     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
112900051229     C                   Z-ADD     G08DAT        DATCORINV         8 0          *DATA CORRENTE AA/M/
113000970526     C*
113100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
113200970526     C                   EXSR      IMPFUN
113300970526     C*
113400970526     C                   ENDSR
113500970526     C*------------------------------------------------------------------------*
113600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
113700970526     C*------------------------------------------------------------------------*
113800970526     C     FUNGES        BEGSR
113900970526     C*
114000970526     C* PULISCO CAMPI DI LAVORO
114100970526     C                   Z-ADD     0             W                 5 0
114200970526     C*
114300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
114400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
114500970526     C     1             DO        24            I
114600970526     C                   MOVEL     FUD(I)        FUDRIG
114700970526     C*
114800970526     C* PULIZIA FUNZIONI PRECEDENTI
114900970526     C                   MOVEL     *BLANKS       FUV(I)
115000970526     C*
115100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
115200970526     C* (SALTA F24)
115300970526     C     FUDATT        IFEQ      'S'
115400970526     C     FUDFUN        ANDNE     '24'
115500970526     C                   ADD       1             W
115600970526     C                   MOVEL     FUDDES        FUV(W)
115700970526     C                   END
115800970526     C                   END                                                    DO WHILE
115900970526     C*
116000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
116100970526     C* ALTRIMENTI DISATTIVO F24
116200970526     C     1             DO        24            I
116300970526     C                   MOVEL     FUD(I)        FUDRIG
116400970526     C*
116500970526     C     FUDFUN        IFEQ      '24'
116600970526     C     W             IFGT      6
116700970526     C                   MOVEL     'S'           FUDATT
116800970526     C                   ELSE
116900970526     C                   MOVEL     *BLANKS       FUDATT
117000970526     C                   END                                                    W GT 8
117100970526     C                   MOVEL     FUDRIG        FUD(I)
117200970526     C                   END
117300970526     C                   END                                                    DO WHILE
117400970526     C*
117500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
117600970526     C     W             DIV       6             I
117700970526     C                   MVR                     Z
117800970526     C*
117900970526     C     Z             IFNE      0
118000970526     C     I             ADD       1             Z
118100970526     C                   ELSE
118200970526     C                   Z-ADD     I             Z                 5 0
118300970526     C                   END
118400970526     C*
118500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
118600970526     C                   ADD       1             FUNGRU
118700970526     C     FUNGRU        IFGT      Z
118800970526     C     FUNGRU        ORLT      1
118900970526     C                   Z-ADD     1             FUNGRU
119000970526     C                   END
119100970526     C*
119200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
119300970526     C     FUNGRU        IFEQ      1
119400970526     C                   MOVEL     FUV(1)        FUN001
119500970526     C                   MOVEL     FUV(2)        FUN002
119600970526     C                   MOVEL     FUV(3)        FUN003
119700970526     C                   MOVEL     FUV(4)        FUN004
119800970526     C                   MOVEL     FUV(5)        FUN005
119900970526     C                   MOVEL     FUV(6)        FUN006
120000970526     C                   END
120100970526     C*
120200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
120300970526     C     FUNGRU        IFEQ      2
120400970526     C                   MOVEL     FUV(7)        FUN001
120500970526     C                   MOVEL     FUV(8)        FUN002
120600970526     C                   MOVEL     FUV(9)        FUN003
120700970526     C                   MOVEL     FUV(10)       FUN004
120800970526     C                   MOVEL     FUV(11)       FUN005
120900970526     C                   MOVEL     FUV(12)       FUN006
121000970526     C                   END
121100970526     C*
121200970526     C* TERZO GRUPPO DI 6 FUNZIONI
121300970526     C     FUNGRU        IFEQ      3
121400970526     C                   MOVEL     FUV(13)       FUN001
121500970526     C                   MOVEL     FUV(14)       FUN002
121600970526     C                   MOVEL     FUV(15)       FUN003
121700970526     C                   MOVEL     FUV(16)       FUN004
121800970526     C                   MOVEL     FUV(17)       FUN005
121900970526     C                   MOVEL     FUV(18)       FUN006
122000970526     C                   END
122100970526     C*
122200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
122300970526     C     FUNGRU        IFEQ      4
122400970526     C                   MOVEL     FUV(19)       FUN001
122500970526     C                   MOVEL     FUV(20)       FUN002
122600970526     C                   MOVEL     FUV(21)       FUN003
122700970526     C                   MOVEL     FUV(22)       FUN004
122800970526     C                   MOVEL     *BLANKS       FUN005
122900970526     C                   MOVEL     *BLANKS       FUN006
123000970526     C                   END
123100970526     C*
123200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
123300970526     C                   MOVEL     *BLANKS       FUN007
123400970526     C                   MOVEL     *BLANKS       FUN008
123500970526     C*
123600970526     C     1             DO        24            I
123700970526     C                   MOVEL     FUD(I)        FUDRIG
123800970526     C     FUDFUN        IFEQ      '24'
123900970526     C     FUDATT        ANDEQ     'S'
124000970526     C                   MOVEL     FUDDES        FUN008
124100970526     C                   END
124200970526     C                   END                                                    DO
124300970526     C*
124400970526     C                   ENDSR
124500970526     C*------------------------------------------------------------------------*
124600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
124700970526     C*------------------------------------------------------------------------*
124800970526     C     FUNCON        BEGSR
124900970526     C*
125000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
125100970526     C                   EXSR      FUNVAR
125200970526     C*
125300970526     C* PULISCO CAMPI DI LAVORO
125400970526     C                   Z-ADD     0             I                 5 0
125500970526     C                   Z-ADD     0             W                 5 0
125600970526     C*
125700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
125800970526     C     FUNKEY        IFNE      *BLANKS
125900970526     C     1             DO        24            I
126000970526     C                   MOVEL     FUD(I)        FUDRIG
126100970526     C*
126200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
126300970526     C     FUNKEY        IFEQ      FUDFUN
126400970526     C     FUDATT        ANDEQ     'S'
126500970526     C                   Z-ADD     I             W
126600970526     C                   END
126700970526     C                   END                                                    DO WHILE
126800970526     C*
126900970526     C* FUNZIONE NON ATTIVA (ERRORE)
127000970526     C     W             IFEQ      0
127100970526     C*
127200970526     C* EMISSIONE MESSAGGIO DI ERRORE
127300970526     C                   MOVEL     MSG(1)        DSMSMS
127400970526     C*
127500970526     C                   END                                                    W=0
127600970526     C                   END                                                    FUNKEY NE *BLANKS
127700970526     C*
127800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
127900970526     C     W             IFNE      0
128000970526     C     FUNKEY        OREQ      *BLANKS
128100970526     C     FUNKEY        IFEQ      *BLANKS
128200970526     C                   EXSR      ENTGES
128300970526     C                   END
128400970526     C     FUNKEY        IFEQ      '03'
128500970526     C                   EXSR      F03GES
128600970526     C                   END
128700970526     C     FUNKEY        IFEQ      '06'
128800970526     C                   EXSR      F06GES
128900970526     C                   END
129000970526     C     FUNKEY        IFEQ      '12'
129100970526     C                   EXSR      F12GES
129200970526     C                   END
129300970526     C     FUNKEY        IFEQ      '24'
129400970526     C                   EXSR      F24GES
129500970526     C                   END
129600970526     C                   END
129700970526     C*
129800970526     C                   ENDSR
129900970526     C*------------------------------------------------------------------------*
130000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
130100970526     C*------------------------------------------------------------------------*
130200970526     C     FUNVAR        BEGSR
130300970526     C*
130400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
130500970526     C*
130600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
130700970526     C     KEY           IFEQ      ENTER
130800970526     C                   MOVEL     *BLANKS       FUNKEY
130900970526     C                   END
131000970526     C     KEY           IFEQ      F1
131100970526     C                   MOVEL     '01'          FUNKEY
131200970526     C                   END
131300970526     C     KEY           IFEQ      F2
131400970526     C                   MOVEL     '02'          FUNKEY
131500970526     C                   END
131600970526     C     KEY           IFEQ      F3
131700970526     C                   MOVEL     '03'          FUNKEY
131800970526     C                   END
131900970526     C     KEY           IFEQ      F4
132000970526     C                   MOVEL     '04'          FUNKEY
132100970526     C                   END
132200970526     C     KEY           IFEQ      F5
132300970526     C                   MOVEL     '05'          FUNKEY
132400970526     C                   END
132500970526     C     KEY           IFEQ      F6
132600970526     C                   MOVEL     '06'          FUNKEY
132700970526     C                   END
132800970526     C     KEY           IFEQ      F7
132900970526     C                   MOVEL     '07'          FUNKEY
133000970526     C                   END
133100970526     C     KEY           IFEQ      F8
133200970526     C                   MOVEL     '08'          FUNKEY
133300970526     C                   END
133400970526     C     KEY           IFEQ      F9
133500970526     C                   MOVEL     '09'          FUNKEY
133600970526     C                   END
133700970526     C     KEY           IFEQ      F10
133800970526     C                   MOVEL     '10'          FUNKEY
133900970526     C                   END
134000970526     C     KEY           IFEQ      F11
134100970526     C                   MOVEL     '11'          FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F12
134400970526     C                   MOVEL     '12'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F13
134700970526     C                   MOVEL     '13'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F14
135000970526     C                   MOVEL     '14'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F15
135300970526     C                   MOVEL     '15'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      F16
135600970526     C                   MOVEL     '16'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      F17
135900970526     C                   MOVEL     '17'          FUNKEY
136000970526     C                   END
136100970526     C     KEY           IFEQ      F18
136200970526     C                   MOVEL     '18'          FUNKEY
136300970526     C                   END
136400970526     C     KEY           IFEQ      F19
136500970526     C                   MOVEL     '19'          FUNKEY
136600970526     C                   END
136700970526     C     KEY           IFEQ      F20
136800970526     C                   MOVEL     '20'          FUNKEY
136900970526     C                   END
137000970526     C     KEY           IFEQ      F21
137100970526     C                   MOVEL     '21'          FUNKEY
137200970526     C                   END
137300970526     C     KEY           IFEQ      F22
137400970526     C                   MOVEL     '22'          FUNKEY
137500970526     C                   END
137600970526     C     KEY           IFEQ      F23
137700970526     C                   MOVEL     '23'          FUNKEY
137800970526     C                   END
137900970526     C     KEY           IFEQ      F24
138000970526     C                   MOVEL     '24'          FUNKEY
138100970526     C                   END
138200970526     C     KEY           IFEQ      ROLLUP
138300970526     C                   MOVEL     'RU'          FUNKEY
138400970526     C                   END
138500970526     C     KEY           IFEQ      HELP
138600970526     C                   MOVEL     'HP'          FUNKEY
138700970526     C                   END
138800970526     C                   ENDSR
138900970526     C*------------------------------------------------------------------------*
139000970526     C* NOTE COSTRUZIONE SCHIERE
139100970526     C*------------------------------------------------------------------------*
139200970526     C*
139300970526     C* -MSG-MESSAGGI
139400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
139500970526     C* (NON TOCCARE MSG,1 E MSG,2)
139600970526     C*
139700970526     C* -CMD-COMANDI
139800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
139900970526     C*
140000970526     C* -OPD-OPZIONI
140100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
140200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
140300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
140400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
140500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
140600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
140700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
140800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
140900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
141000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
141100970526     C*
141200970526     C* -FUD-FUNZIONI
141300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
141400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
141500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
141600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
141700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
141800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
141900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
142000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
142100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
142200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
142300970526     O*------------------------------------------------------------------------*
142400970526** MSG - MESSAGGI
142500051230Tasto funzionale non gestito                                                   1
142600051230Il "nuovo codice" unificante risulta già figlio del codice:                    2
142700051230Il "vecchio codice" non risulta essere attualmente unificante                  3
142800051230Campo obbligatorio                                                             4
142900051230Codice cliente inesistente                                                     5
143000051230Elaborazione effettuata                                                        6
143100051230Errore in caricamento dati utente, avvisare il CED                             7
143200051230Cliente non in gestione all'utente, impossibile eseguire richiesta             8
143300051230Data non valida                                                                9
143400051230Data indicata inferiore a data decorrenza di legame già esistente              10
143500051230Il "vecchio codice" risulta già figlio del codice:                             11
143600060209Già presenti per il cliente legami WW con decorrenza >= ad oggi                12
143700060209Già presenti per il cliente abilit. servizi con decorrenza >= ad oggi          13
143800060127Codici "vecchio" e "nuovo" sono uguali, la richiesta è inconsistente!          14
143900090806Il "nuovo codice" unificante risulta già padre su legame di tipo analogo.      15
144000051230                                                                               16
144100051230                                                                               17
144200051230                                                                               18
144300051230                                                                               19
144400051230                                                                               20
144500051230                                                                               21
144600051230                                                                               22
144700051230                                                                               23
144800051230                                                                               24
144900970526** FUD - FUNZIONI
145000970526                   -        (S/N)=N                        funz.01¬
145100970526                   -        (S/N)=N                        funz.02¬
145200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
145300970526                   -        (S/N)=N                        funz.04¬
145400970526                   -        (S/N)=N                        funz.05¬
145500051229F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
145600970527                   -        (S/N)=N                        funz.07¬
145700970527                   -        (S/N)=N                        funz.08¬
145800970527                   -        (S/N)=N                        funz.09¬
145900970527                   -        (S/N)=N                        funz.10¬
146000970526                   -        (S/N)=N                        funz.11¬
146100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
146200970526                   -        (S/N)=N                        funz.13¬
146300970526                   -        (S/N)=N                        funz.14¬
146400970526                   -        (S/N)=N                        funz.15¬
146500970526                   -        (S/N)=N                        funz.16¬
146600970526                   -        (S/N)=N                        funz.17¬
146700970526                   -        (S/N)=N                        funz.18¬
146800970526                   -        (S/N)=N                        funz.19¬
146900970526                   -        (S/N)=N                        funz.20¬
147000970526                   -        (S/N)=N                        funz.21¬
147100970526                   -        (S/N)=N                        funz.22¬
147200970526                   -        (S/N)=N                        funz.23¬
147300051229F24=Altri tasti    -        (S/N)=N                        funz.24¬
