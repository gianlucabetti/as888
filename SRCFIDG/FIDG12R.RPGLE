000100970526     H*------------------------------------------------------------------------*
000200070122     H* Gestione anagrafica giri
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800070404     Ffidg12d   cf   e             WORKSTN INFDS(DEVDS1)
000900070404     F                                     SFILE(DG12DMS:MSGULT)
001000970527     F*------------
001100070406     Ffigrv01l  UF A E           K DISK
001200070417     Ffigrv02l  IF   E           K DISK    rename(figrv000:figrv002) prefix(w1_)
001300070404     Ffigrg04l  UF A E           K DISK
001400070404     Fazorg01l  IF   E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500060616     D MSG             S             78    DIM(27) CTDATA PERRCD(1)
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
007000070404     D fidg11ds      E DS
007100070404     D DGRGFLO       E DS
007200070122     D*------------
007300070122     D* DS REPERIMENTO DATI UTENTE
007400070122     D*------------
007500070122     D TIBS34DS      E DS                                                       *Profili utente
007600070122     D DDATIUTE      E DS                                                       *Dati utente
007700070122     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007800970526     D*------------
007900970526     D* ARCHITETTURA
008000970526     D*------------
008100970526     D KPJBA         E DS
008200040816     D*------------
008300970526     C*------------------------------------------------------------------------*
008400970526     C* MAIN LINES
008500970526     C*------------------------------------------------------------------------*
008600970526     C*
008700970526     C* NON CI SONO ERRORI IN ENTRATA
008800970526IF  1C     ERRL00        IFEQ      '0'
008900970526     C*
009000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009400970526     C*
009500970526     C* EMETTE FORMATO VIDEO
009600970526     C                   EXSR      BUTFMR
009700970526     C*
009800970526     C* GESTIONE TASTI FUNZIONALI
009900970526     C                   EXSR      FUNCON
010000970526E   2C                   ENDDO
010100970526     C*
010200970526     C* CI SONO ERRORI IN ENTRATA
010300970527X   1C                   ELSE
010400070404     C                   MOVEL     '1'           D11oerr
010500070404     C                   MOVEL     DSMSMS        D11omsg
010600970526E   1C                   ENDIF
010700070123     C*
010800070123     C* RITORNO PARAMETRI
010900070404     C                   MOVEL(P)  fidg11ds      KPJBU
011000070122     C* FINE PGM
011100970526     C                   SETON                                        LR
011200970526     C*------------------------------------------------------------------------*
011300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011400970526     C*------------------------------------------------------------------------*
011500970526     C     BUTFMR        BEGSR
011600970526     C*
011700970526     C* EMETTE LA TESTATA
011800070404     C                   WRITE     DG12D01
011900970526     C*
012000970526     C* SPEDISCE EVENTUALE MESSAGGIO
012100970526     C                   EXSR      SNDMSG
012200970526     C*
012300970526     C* EMETTE LE FUNZIONI
012400070404     C                   WRITE     DG12D02
012500970526     C*
012600970526     C* EMETTE IL VIDEO
012700070404     C                   EXFMT     DG12D03
012800970526     C*
012900970526     C* REIMPOSTA FLAG DI ERRORE
013000970526     C                   MOVEL     '0'           ERRL00
013100970526     C*
013200970526     C* REIMPOSTA INDICATORI DI ERRORE
013300070122     C                   SETOFF                                       707172
013400070417     C                   SETOFF                                       7475
013500970526     C*
013600970526     C                   ENDSR
013700970526     C*------------------------------------------------------------------------*
013800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013900970526     C*------------------------------------------------------------------------*
014000970526     C     SNDMSG        BEGSR
014100970526     C*
014200970526     C* PULISCE IL SFILE MESSAGGI
014300970526     C                   MOVEL     '1'           *IN40
014400070404     C                   WRITE     DG12DMC
014500970526     C                   MOVEL     '0'           *IN40
014600970526     C*
014700970526     C* VISUALIZZA IL MESSAGGIO
014800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014900970526     C                   CALL      'XMSG'        XMSGPA
015000970526IF  1C     DSMSER        IFEQ      *ZEROS
015100970526     C                   MOVEL     DSMSKE        MSGKEY
015200970526E   1C                   ENDIF
015300970526     C                   Z-ADD     1             MSGULT
015400070404     C                   WRITE     DG12DMS
015500070404     C                   WRITE     DG12DMC
015600970526     C*
015700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015800970526     C                   MOVEL     *BLANKS       DSMSMS
015900970526     C*
016000970526     C                   ENDSR
016100970526     C*------------------------------------------------------------------------*
016200970526     C* ENTGES - GESTIONE ENTER -> INVIO
016300970526     C*------------------------------------------------------------------------*
016400970526     C     ENTGES        BEGSR
016500970526     C*
016600970526     C* CONTROLLA I CAMPI IMMESSI
016700970526     C                   EXSR      DECODI
016800970526     C*
016900970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
017000970526IF  1C     DEPOP1        IFEQ      'O05'
017100970526     C                   MOVEL     '1'           FINL00
017200970526E   1C                   ENDIF
017300970526     C*
017400970526     C                   ENDSR
017500970526     C*------------------------------------------------------------------------*
017600970526     C* F03GES - GESTIONE F03 -> FINE
017700970526     C*------------------------------------------------------------------------*
017800970526     C     F03GES        BEGSR
017900970526     C*
018000970526     C* IMPOSTA "PREMUTO F03"
018100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018200070404     C                   MOVEL     '1'           D11OF03                        *DI PROCEDURA
018300970526     C*
018400970526     C                   ENDSR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
018700970526     C*------------------------------------------------------------------------*
018800970526     C     F06GES        BEGSR
018900970526     C*
019000970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
019100970526IF  1C     DEPOP1        IFEQ      'F06'
019200970526     C                   EXSR      INSERI
019300970526E   1C                   ENDIF
019400970526IF  1C     DEPOP1        IFEQ      'O02'
019500970526     C                   EXSR      MODIFI
019600970526E   1C                   ENDIF
019700991014IF  1C     DEPOP1        IFEQ      'O03'
019800011129     C                   EXSR      COPIA
019900991014E   1C                   ENDIF
020000011129     C*
020100970526     C                   ENDSR
020200970526     C*------------------------------------------------------------------------*
020300970526     C* F12GES - GESTIONE F12 -> RITORNO
020400970526     C*------------------------------------------------------------------------*
020500970526     C     F12GES        BEGSR
020600970526     C*
020700970526     C* IMPOSTA "PREMUTO F12"
020800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020900070404     C                   MOVEL     '1'           D11OF12                        *DI PROCEDURA
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021400970526     C*------------------------------------------------------------------------*
021500970526     C     F24GES        BEGSR
021600970526     C*
021700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
022200970526     C*------------------------------------------------------------------------*
022300970526     C     INSERI        BEGSR
022400970526     C*
022500970526     C* CONTROLLA I CAMPI IMMESSI
022600970526     C                   EXSR      DECODI
022700970526     C*
022800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
022900991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
023000070404     C                   clear                   figrv000
023100070404     C                   exsr      impgrv
023200070404     C                   write     figrv000
023300070404     C                   exsr      fisgrg                                       *AGGIUNGE GIRI FISSI
023400070404     C                   movel     MSG(2)        D11OMSG                        *MESSAGGIO
023500970602     C*
023600970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
023700070122     C                   exsr      azzvid
023800070122E   1C                   endif
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* MODIFI - OPERAZIONI DI MODIFICA
024300970526     C*------------------------------------------------------------------------*
024400970526     C     MODIFI        BEGSR
024500970526     C*
024600970526     C* CONTROLLA I CAMPI IMMESSI
024700970526     C                   EXSR      DECODI
024800970526     C*
024900970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
025000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025100070404     C                   EXSR      LETgrv
025200970526     C*
025300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
025400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025500070404     C                   EXSR      impgrv                                       *IMPOSTA I CAMPI
025600070404     C                   UPDATE    figrv000
025700070404     C                   EXSR      moddscadgrg                                  *MOD. SCADENZA GIRI
025800070404     C                   MOVEL     MSG(3)        D11OMSG
025900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
026000970526E   2C                   ENDIF
026100970526E   1C                   ENDIF
026200970526     C*
026300970526     C                   ENDSR
026400991014     C*------------------------------------------------------------------------*
026500070404     C* COPIA  - OPERAZIONI DI COPIA
026600991014     C*------------------------------------------------------------------------*
026700070122     C     COPIA         BEGSR
026800991014     C*
026900070404     C* CONTROLLA I CAMPI IMMESSI
027000070404     C                   EXSR      DECODI
027100070404     C*
027200070404     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
027300070404IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027400070404     C                   EXSR      LETgrv
027500070404     C*
027600070404     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
027700070404IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027800070404     C                   CLEAR                   figrv000
027900070404     C                   EXSR      impgrv                                       *IMPOSTA I CAMPI
028000070404     C                   WRITE     figrv000
028100070404     C                   EXSR      dupgrg                                       *DUPLICA I GIRI
028200070404     C                   EXSR      fisgrg                                       *AGGIUNGE GIRI FISSI
028300070404     C                   MOVEL     MSG(23)       D11OMSG
028400070404     C                   MOVEL     '1'           FINL00                         *FINE PGM
028500070404E   2C                   ENDIF
028600070404E   1C                   ENDIF
028700991014     C*
028800991014     C                   ENDSR
028900070404     C*------------------------------------------------------------------------*
029000070404     C* FISGRG - AGGIUTA GIRI "FISSI"
029100070404     C*------------------------------------------------------------------------*
029200070404     C     FISGRG        BEGSR
029300070404     C*
029400070404     C                   clear                   figrg000
029500070404     C                   clear                   DGRGFLO
029600070404     C*
029700070404     C                   z-add     wviddde       grgdde
029800070404     C                   z-add     wviddscad     grgdscad
029900070404     C                   move(p)   wvidfgs       grgfgs
030000070404     C                   movel     '='           grgain
030100070404     C                   movel     'N'           §GRGDST
030200070404     C                   movel     'S'           §GRGEDP
030300071024     C                   movel     'C'           §GRGTUG
030400070404     C                   movel     'S'           grgftt
030500070404     C                   z-add     datcor        grgduv
030600070404     C                   movel     knmus         grgute
030700070404     C*
030800070417     C                   movel     'S'           §GRGFIT
030900070417     C                   movel     DGRGFLO       grgflo
031000070404     C                   movel     'CS'          grgcgi
031100070404     C                   eval      grgdes = 'CONTI SERVIZIO'
031200070411     C                   write(e)  figrg000
031300070404     C*
031500070423     C                   movel     *blanks       §GRGDST
031600071024     C                   movel     'E'           §GRGTUG
031700070417     C                   movel     DGRGFLO       grgflo
031800160401     C                   movel     'VP'          grgcgi
031900160401     C                   eval      grgdes = 'VEDI PACCO'
032000070411     C                   write(e)  figrg000
032001160401     C*
032002160401     C                   movel     *blanks       §GRGFIT
032003160401     C                   movel     *blanks       §GRGDST
032004160401     C                   movel     'E'           §GRGTUG
032005160401     C                   movel     DGRGFLO       grgflo
032006160401     C                   movel     'DIR'         grgcgi
032007160401     C                   eval      grgdes = 'DIROTTAMENTI'
032008160401     C                   write(e)  figrg000
032100070404     C*
032200070404     C                   ENDSR
032300070404     C*------------------------------------------------------------------------*
032400070404     C* DUPGRG - DUPLICA GIRI DA VERSIONE A VERSIONE
032500070404     C*------------------------------------------------------------------------*
032600070404     C     DUPGRG        BEGSR
032700070404     C*
032800070404     C                   eval      K_grvfgs = d11ifgs
032900070404     C                   eval      K_grvdde = d11idde
033000070404     C     KEYgrg04_c    setll     figrg04l
033100070404     C                   if        %found(figrg04l)
033200070404     C     KEYgrg04_c    reade     figrg04l
033300070404     C                   dow       not %eof(figrg04l)
033400070411     C                   z-add     wvidfgs       grgfgs
033500070404     C                   z-add     wviddde       grgdde
033600070404     C                   z-add     wviddscad     grgdscad
033700070404     C                   movel     'S'           grgftt
033800070404     C                   z-add     datcor        grgduv
033900070404     C                   movel     knmus         grgute
034000070404     C                   write     figrg000
034100070404     C     KEYgrg04_c    reade     figrg04l
034200070404     C                   enddo
034300070404     C                   endif
034400070404     C*
034500070404     C                   ENDSR
034600070404     C*------------------------------------------------------------------------*
034700070404     C* MODDSCADGRG - AGGIORNA DATA SCADENZA SUI GIRI DELLA VERSIONE MODIFICATA
034800070404     C*------------------------------------------------------------------------*
034900070404     C     MODDSCADGRG   BEGSR
035000070404     C*
035100070404     C                   eval      K_grvfgs = wvidfgs
035200070404     C                   eval      K_grvdde = wviddde
035300070404     C     KEYgrg04_c    setll     figrg04l
035400070404     C                   if        %found(figrg04l)
035500070404     C     KEYgrg04_c    reade     figrg04l
035600070404     C                   dow       not %eof(figrg04l)
035700070404     C                   if        grgdscad > wviddscad
035800070404     C                   z-add     wviddscad     grgdscad
035900070404     C                   movel     'S'           grgftt
036000070404     C                   z-add     datcor        grgduv
036100070404     C                   movel     knmus         grgute
036200070404     C                   update    figrg000
036300070404     C                   endif
036400070404     C     KEYgrg04_c    reade     figrg04l
036500070404     C                   enddo
036600070404     C                   endif
036700070404     C*
036800070404     C                   ENDSR
036900970526     C*------------------------------------------------------------------------*
037000070404     C* LETgrv - LEGGE IL RECORD PERL'AGGIORNAMENTO
037100970526     C*------------------------------------------------------------------------*
037200070404     C     LETgrv        BEGSR
037300970526     C*
037400070404     C                   IF        DEPOP1 <> 'F06' AND
037500070404     C                             DEPOP1 <> 'O03'
037600070404     C                   Z-ADD     D11ifgs       K_grvfgs
037700070404     C                   Z-ADD     D11idde       K_grvdde
037800070206     C                   ENDIF
037900070406     C     KEYgrv01_c    CHAIN     figrv01l                           8081
038000070122IF  1C     *IN81         IFEQ      *ON                                          *ALLOCATO
038100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
038200070404     C                   MOVEL     MSG(7)        D11OMSG                        *MESSAGGIO
038300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
038400970526E   1C                   ENDIF
038500070122IF  1C     *IN80         IFEQ      *ON                                          *INSESISTENTE
038600070404     C                   IF        DEPOP1 <> 'F06' AND
038700070404     C                             DEPOP1 <> 'O03'
038800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
038900070404     C                   MOVEL     MSG(6)        D11OMSG                        *MESSAGGIO
039000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
039100070122     C                   ENDIF
039200970526E   1C                   ENDIF
039300970526     C*
039400970526     C                   ENDSR
039500970526     C*------------------------------------------------------------------------*
039600070404     C* DECODI - CONTROLLA CAMPI E DECODIFICA
039700970526     C*------------------------------------------------------------------------*
039800970526     C     DECODI        BEGSR
039900001120     C*
040000070404     C*--------------------
040100070404     C* CONTROLLO UTENTE => SOLO EDP
040200070404     C*--------------------
040300070404     C     ERRL00        IFEQ      '0'
040400070404     C                   IF        %subst(KNMUS:1:3) = 'EDP'
040500070404     C                   ELSE
040600070404     C                   MOVEL     '1'           ERRL00
040700070404     C                   MOVEL     MSG(21)       DSMSMS
040800070404     C                   SETON                                        707172
040900070404     C                   SETON                                        74
041000070404     C                   ENDIF
041100070404     C                   ENDIF
041200070403     C*--------------------
041300070403     C* CONTROLLO FILIALE
041400070403     C*--------------------
041500070403     C     ERRL00        IFEQ      '0'
041600070403     C     vidfgs        IFNE      *blanks
041700070403     C     vidfgs        ANDNE     *zeros
041800070403     C                   EXSR      cntpo
041900070403     C                   ELSE
042000070403     C                   MOVEL     '1'           ERRL00
042100070403     C                   MOVEL     MSG(12)       DSMSMS
042200070403     C                   SETON                                        72
042300070403     C                   ENDIF
042400070403     C                   ENDIF
042500070404     C*--------------------
042600070404     C* OBBLIGATORIETA CAMPI
042700070404     C*--------------------
042800070404     C     ERRL00        IFEQ      '0'
042900070404     C                   IF        viddde = *zeros
043000070404     C                   MOVEL     '1'           ERRL00
043100070404     C                   MOVEL     MSG(12)       DSMSMS
043200070404     C                   SETON                                        70
043300070404     C                   ENDIF
043400070404     C                   IF        viddscad = *zeros
043500070404     C                   MOVEL     '1'           ERRL00
043600070404     C                   MOVEL     MSG(12)       DSMSMS
043700070404     C                   SETON                                        71
043800070404     C                   ENDIF
043900070404     C                   IF        vidfgs = *blanks OR
044000070404     C                             vidfgs = *zeros
044100070404     C                   MOVEL     '1'           ERRL00
044200070404     C                   MOVEL     MSG(12)       DSMSMS
044300070404     C                   SETON                                        72
044400070404     C                   ENDIF
044500070404     C                   IF        viddes = *blanks
044600070404     C                   MOVEL     '1'           ERRL00
044700070404     C                   MOVEL     MSG(12)       DSMSMS
044800070404     C                   SETON                                        74
044900070404     C                   ENDIF
045000070404     C                   ENDIF
045100991026     C*--------------------
045200070403     C* DATA DECORRENZA
045300991026     C*--------------------
045400070122     C     *LIKE         DEFINE    viddde        wviddde
045500001120     C     ERRL00        IFEQ      '0'
045600070122     C                   Z-ADD     viddde        wda8
045700991026     C                   Z-ADD     *ZEROS        da8w
045800991026     C                   EXSR      CHKDA8
045900070122     C                   Z-ADD     WDA8          viddde
046000070122     C                   Z-ADD     DA8W          wviddde
046100001120     C     ERRL00        IFEQ      '1'
046200070122     C                   SETON                                        70
046300070122     C                   MOVEL     MSG(14)       DSMSMS
046400001120     C                   ENDIF
046500001120     C                   ENDIF
046600070122     C*--------------------
046700070122     C* DATA SCADENZA
046800070122     C*--------------------
046900070124     C     *LIKE         DEFINE    viddscad      wviddscad
047000070122     C     ERRL00        IFEQ      '0'
047100070124     C                   Z-ADD     viddscad      WDA8
047200070122     C                   Z-ADD     *ZEROS        DA8W
047300070122     C                   EXSR      CHKDA8
047400070124     C                   Z-ADD     WDA8          viddscad
047500070124     C                   Z-ADD     DA8W          wviddscad
047600070122     C     ERRL00        IFEQ      '1'
047700070122     C                   SETON                                        71
047800070122     C                   MOVEL     MSG(14)       DSMSMS
047900070122     C                   ENDIF
048000070122     C                   ENDIF
048100070122     C*--------------------
048200070122     C* SCADENZA < DECORRENZA
048300070122     C*--------------------
048400070122     C     ERRL00        IFEQ      '0'
048500070124     C     wviddscad     IFLT      wviddde
048600070122     C                   SETON                                        7071
048700070122     C                   MOVEL     MSG(15)       DSMSMS
048800070122     C                   MOVEL     '1'           ERRL00
048900070122     C                   ENDIF
049000070122     C                   ENDIF
049100070122     C*--------------------
049200070122     C* SCADENZA >= DATA CORRENTE
049300070122     C*--------------------
049400070122     C     ERRL00        IFEQ      '0'
049500070122     C     DEPOP1        IFEQ      'O02'
049600070404     C     DEPOP1        OREQ      'O03'
049700070122     C     DEPOP1        OREQ      'F06'
049800070124     C     wviddscad     IFLT      DATCOR
049900070122     C                   SETON                                        71
050000070122     C                   MOVEL     MSG(16)       DSMSMS
050100070122     C                   MOVEL     '1'           ERRL00
050200070122     C                   ENDIF
050300070122     C                   ENDIF
050400070122     C                   ENDIF
050500070404     C*--------------------
050600070404     C* PERIODO NN SOVRAPPOSTO AD ALTRO X STESSA FILIALE
050700070404     C*--------------------
050800070404     C     ERRL00        IFEQ      '0'
050900070406     C     wvidfgs       SETLL     figrv01l
051000070406     C                   IF        %found(figrv01l)
051100070406     C     wvidfgs       READE(n)  figrv01l
051200070406     C                   DOW       not %eof(figrv01l)
051300070424     C                   IF        DEPOP1 = 'O02' OR
051400070424     C                             DEPOP1 = 'O05'
051500070423     C                   IF        wviddde   <> grvdde     AND
051600070423     C                             ((grvdscad >= wviddde    AND
051700070423     C                              grvdscad <= wviddscad) OR
051800070423     C                             (grvdde   >= wviddde    AND
051900070423     C                              grvdde   <= wviddscad))
052000070404     C                   MOVEL     '1'           ERRL00
052100070404     C                   SETON                                        7071
052200070404     C                   MOVEL     MSG(22)       DSMSMS
052300070404     C                   LEAVE
052400070423     C                   ENDIF
052500070423     C                   ELSE
052600070423     C                   IF        (grvdscad >= wviddde    AND
052700070423     C                              grvdscad <= wviddscad) OR
052800070423     C                             (grvdde   >= wviddde    AND
052900070423     C                              grvdde   <= wviddscad)
053000070423     C                   MOVEL     '1'           ERRL00
053100070423     C                   SETON                                        7071
053200070423     C                   MOVEL     MSG(22)       DSMSMS
053300070423     C                   LEAVE
053400070423     C                   ENDIF
053500070404     C                   ENDIF
053600070404     C*
053700070406     C     wvidfgs       READE(n)  figrv01l
053800070404     C                   ENDDO
053900070404     C                   ENDIF
054000070404     C                   ENDIF
054100070417     C*--------------------
054200070417     C* FILIALE / KEY VERSIONE TELLUS => UNIVOCI
054300070417     C*--------------------
054400070417     C     ERRL00        IFEQ      '0'
054500070423     C                   IF        DEPOP1 = 'F06' OR
054600070423     C                             DEPOP1 = 'O03'
054700070417     C                   eval      K_grvfgs = wvidfgs
054800070417     C                   eval      K_grvkey = VIDKEY
054900070417     C     KEYgrv02_c    CHAIN(n)  figrv02l
055000070420     C                   IF        %found(figrv02l)
055100070417     C                   MOVEL     '1'           ERRL00
055200070417     C                   SETON                                        7275
055300070417     C                   MOVEL     MSG(24)       DSMSMS
055400070423     C                   ENDIF
055500070417     C                   ENDIF
055600070417     C                   ENDIF
055700070420     C*--------------------
055800070420     C* FILIALE / KEY VERSIONE TELLUS => SOLO DETERMINATI VALORI
055900070420     C*--------------------
056000070420     C     ERRL00        IFEQ      '0'
056100070420     C                   MOVEL     wviddde       wKEY              8
056200070420     C                   IF        VIDKEY = *blanks OR
056300070420     C                             VIDKEY = wKEY
056400070420     C                   ELSE
056500070420     C                   MOVEL     '1'           ERRL00
056600070420     C                   SETON                                        7275
056700070420     C                   MOVEL     MSG(25)       DSMSMS
056800070420     C                   ENDIF
056900070420     C                   ENDIF
057000070122     C*--------------------
057100991026     C* INSERIMENTO -> RECORD INESISTENTE
057200070122     C*--------------------
057300001120     C     ERRL00        IFEQ      '0'
057400070404     C                   IF        DEPOP1 = 'F06' OR
057500070404     C                             DEPOP1 = 'O03'
057600070404     C                   z-add     wvidfgs       K_grvfgs
057700070404     C                   z-add     wviddde       K_grvdde
057800070404     C                   exsr      letgrv
057900070206     C                   if        *in80 = *off
058000070122     C                   movel     '1'           ERRL00
058100070122     C                   movel     MSG(8)        DSMSMS
058200070404     C                   seton                                        7072
058300070122     C                   endif
058400070122     C                   ENDIF
058500070122     C                   ENDIF
058600001120     C*
058700001120     C                   ENDSR
058800991027     C*------------------------------------------------------------------------*
058900070122     C* CNTPO  - Controlla codice filiale
059000991027     C*------------------------------------------------------------------------*
059100070122     C     CNTPO         begsr
059200001120     C*
059300070122     C                   movel     *blanks       vddfgs
059400070122     C     '?'           scan      vidfgs                                 97
059500001120     C                   if        *in97
059600070122     C                   movel     *blanks       cod1
059700070122     C                   movel     *blanks       des1
059800070122     C                   movel     *blanks       pra1
059900001120     C                   call      'TNSD24R'
060000001120     C                   parm                    cod1              3
060100001120     C                   parm                    tip1              1
060200001120     C                   parm                    des1             25
060300001120     C                   parm                    pra1              3
060400070122     C                   movel     cod1          vidfgs
060500001120     C                   endif
060600070122     C                   move      vidfgs        cod1n             3 0
060700001120     C     cod1n         chain     azorg01l                           98
060800001120     C                   if        not *in98 and orgfag <>'V'
060900070123     C                   z-add     orgfil        wvidfgs           3 0
061000070122     C                   movel     orgdes        vddfgs
061100001120     C                   else
061200001120     C                   seton                                        69
061300001120     C                   move      '1'           errl00
061400001120     C                   movel     msg(13)       dsmsms
061500001120     C                   endif
061600001120     C*
061700001120     C                   endsr
061800991026     C*------------------------------------------------------------------------*
061900991026     C* CHKDA8 - CONTROLLA LA DATA (8)
062000991026     C*------------------------------------------------------------------------*
062100991026     C     CHKDA8        BEGSR
062200991026     C*
062300070122     C     *LIKE         DEFINE    viddde        WDA8
062400070122     C     *LIKE         DEFINE    viddde        DA8W
062500991026     C*
062600991026     C                   Z-ADD     WDA8          G08DAT
062700991026     C                   Z-ADD     *ZEROS        G08INV
062800991026     C                   MOVEL     '0'           G08ERR
062900991026     C                   CALL      'XSRDA8'
063000991026     C                   PARM                    WLBDA8
063100991026     C                   Z-ADD     G08DAT        WDA8
063200991026     C                   Z-ADD     G08INV        DA8W
063300991026IF  1C     G08ERR        IFEQ      '1'
063400991026     C                   MOVEL     '1'           ERRL00
063500991026E   1C                   ENDIF
063600991026     C*
063700991026     C                   ENDSR
063800970526     C*------------------------------------------------------------------------*
063900970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
064000970526     C*------------------------------------------------------------------------*
064100970526     C     IREVID        BEGSR
064200970526     C* CAMPI
064300070404     C                   Z-ADD     grvdde        G08INV
064400070122     C                   Z-ADD     *ZEROS        G08DAT
064500070122     C                   MOVEL     '3'           G08ERR
064600070122     C                   CALL      'XSRDA8'
064700070122     C                   PARM                    WLBDA8
064800070122     C                   Z-ADD     G08DAT        viddde
064900070122     C                   Z-ADD     G08INV        wviddde
065000070122     C*
065100070404     C                   Z-ADD     grvdscad      G08INV
065200070122     C                   Z-ADD     *ZEROS        G08DAT
065300070122     C                   MOVEL     '3'           G08ERR
065400070122     C                   CALL      'XSRDA8'
065500070122     C                   PARM                    WLBDA8
065600070124     C                   Z-ADD     G08DAT        viddscad
065700070124     C                   Z-ADD     G08INV        wviddscad
065800070122     C*
065900070404     C                   MOVE(P)   grvfgs        vidfgs
066000070406     C                   MOVEL(P)  grvkey        vidkey
066100070406     C                   MOVEL(P)  grvdes        viddes
066200020218     C*
066300970526     C                   ENDSR
066400970526     C*------------------------------------------------------------------------*
066500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
066600970526     C*------------------------------------------------------------------------*
066700970526     C     AZZVID        BEGSR
066800970526     C* CAMPI
066900070122     C                   MOVEA     *IN           wSavIn           99
067000070404     C                   CLEAR                   DG12D03
067100070122     C                   MOVEA     wSavIn        *IN
067200970526     C*
067300970526     C                   ENDSR
067400970526     C*------------------------------------------------------------------------*
067500070404     C* IMPGRV - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
067600970526     C*------------------------------------------------------------------------*
067700070404     C     IMPGRV        BEGSR
067800970526     C*
067900070122     C                   Z-ADD     viddde        G08dat
068000070122     C                   Z-ADD     *ZEROS        G08inv
068100070122     C                   MOVEL     '0'           G08ERR
068200070122     C                   CALL      'XSRDA8'
068300070122     C                   PARM                    WLBDA8
068400070404     C                   Z-ADD     G08inv        grvdde
068500070122     C*
068600070124     C                   z-add     viddscad      G08dat
068700070122     C                   Z-ADD     *ZEROS        G08inv
068800070122     C                   MOVEL     '0'           G08ERR
068900070122     C                   CALL      'XSRDA8'
069000070122     C                   PARM                    WLBDA8
069100070404     C                   Z-ADD     G08inv        grvdscad
069200070122     C*
069300070404     C                   move(p)   vidfgs        grvfgs
069400070406     C                   movel(p)  vidkey        grvkey
069500070406     C                   movel(p)  viddes        grvdes
069600970526     C*
069700970526     C                   ENDSR
069800970526     C*------------------------------------------------------------------------*
069900970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
070000970526     C*------------------------------------------------------------------------*
070100970526     C     IMPFUN        BEGSR
070200970526     C*
070300970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
070400970526IF  1C     DEPOP1        IFEQ      'O05'
070500970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
070600970526     C                   MOVEL     'N'           FUDATT
070700970526     C                   MOVEL     FUDRIG        FUD(6)
070800970526E   1C                   ENDIF
070900970526     C*
071000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
071100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
071200970526     C                   EXSR      FUNGES
071300970526     C*
071400970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
071500970526     C*---
071600070122     C* 90 -> VISUALIZZAZIONE
071700070122     C* 91 -> MODIFICA
071800070122     C* 92 -> INSERIMENTO
071900070411     C* 94 -> COPIA
072000970526     C*---
072100070411     C*
072200070123     C                   SETON                                        90
072300070123     C                   SETON                                        91
072400070411     C                   SETOFF                                       92
072500070411     C                   SETOFF                                       94
072600070411     C     DEPOP1        IFEQ      'O03'                                        *COPIA
072700070411     C                   SETON                                        94
072800070411     C                   ENDIF
072900070122IF  1C     DEPOP0        IFEQ      'P03'                                        GESTIONE
073000070122     C     DEPOP0        OREQ      'P04'                                        GESTIONE FULL
073100970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
073200070411     C     DEPOP1        OREQ      'O03'                                        *COPIA
073300970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
073400970526     C                   SETOFF                                       91
073500070122     C                   SETON                                        92
073600070404     C                   SETOFF                                       93
073700970526E   2C                   ENDIF
073800970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
073900970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
074000970526E   2C                   ENDIF
074100970526E   1C                   ENDIF
074200970526     C*
074300970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
074400970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
074500070404     C                   Z-ADD     D11ifgs       K_grvfgs
074600070404     C                   Z-ADD     D11idde       K_grvdde
074700070406     C     KEYgrv01_c    CHAIN     figrv01l
074800070406IF  2C                   IF        not %found(figrv01l)                         *NON TROVATO
074900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
075000070122     C                   MOVEL     MSG(6)        DSMSMS
075100970526X   2C                   ELSE
075200970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
075300980128IF  3C     DEPOP1        IFEQ      'O05'
075400070122IF  3C     DEPOP1        OREQ      'O02'
075500070404IF  3C     DEPOP1        OREQ      'O03'
075600980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
075700980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
075800980128E   3C                   ENDIF
075900970526E   2C                   ENDIF
076000970526X   1C                   ELSE                                                   *INSERIMENTO
076100970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
076200970526E   1C                   ENDIF
076300970526     C*
076400970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
076500970526IF  1C     DEPOP1        IFEQ      'O02'
076600970526     C                   MOVEL     DEC(1)        VIDFUN
076700970526E   1C                   ENDIF
076800991014IF  1C     DEPOP1        IFEQ      'O03'
076900991014     C                   MOVEL     DEC(6)        VIDFUN
077000991014E   1C                   ENDIF
077100970526IF  1C     DEPOP1        IFEQ      'O04'
077200970526     C                   MOVEL     DEC(2)        VIDFUN
077300970526E   1C                   ENDIF
077400970526IF  1C     DEPOP1        IFEQ      'O05'
077500970526     C                   MOVEL     DEC(3)        VIDFUN
077600970526E   1C                   ENDIF
077700970526IF  1C     DEPOP1        IFEQ      'O07'
077800970526     C                   MOVEL     DEC(4)        VIDFUN
077900970526E   1C                   ENDIF
078000970526IF  1C     DEPOP1        IFEQ      'F06'
078100970526     C                   MOVEL     DEC(5)        VIDFUN
078200970526E   1C                   ENDIF
078300970526     C*
078400970526     C                   ENDSR
078500070122     C*--------------------------------------------------------------------------------------------*
078600070122     C* REPERISCE I DATI UTENTE
078700070122     C*--------------------------------------------------------------------------------------------*
078800070122     C     REPDATIUTE    BEGSR
078900070122     C*
079000070122     C* INIZIALIZZA VARIABILI DI WRK
079100070122     C                   CLEAR                   TIBS34DS
079200070122     C                   CLEAR                   AZUTEDS
079300070122     C                   CLEAR                   DDATIUTE
079400070122     C*
079500070122     C     *DTAARA       DEFINE    §azute        azuteds
079600070122     C     *DTAARA       DEFINE    §datiute      ddatiute
079700070122     C                   IN(E)     *DTAARA
079800070122if  1C                   IF        %Error
079900070122     c                   EVAL      I34Tla = 'L'
080000070122     C                   CALL      'TIBS34R'
080100070122     C                   PARM                    Tibs34Ds
080200070122     C                   IN        *DTAARA
080300070122e   1C                   ENDIF
080400070122     C*
080500070122     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
080600070122     C                   MOVEL(P)  RSUT          DSFIRS           20
080700070122     C*
080800070122     C                   ENDSR
080900970526     C*------------------------------------------------------------------------*
081000970526     C* *INZSR - OPERAZIONI INIZIALI
081100970526     C*------------------------------------------------------------------------*
081200970526     C     *INZSR        BEGSR
081300970526     C*---
081400970526     C* RICEVIMENTO PARAMETRI
081500970526     C*---
081600970526     C     *ENTRY        PLIST
081700970612     C                   PARM                    KPJBA
081800070404     C                   MOVEL(P)  KPJBU         fidg11ds
081900070122     C*
082000070122     C* REPERIMENTO DATI UTENTE
082100070122     C                   EXSR      REPDATIUTE
082200970526     C*
082300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
082400970526     C     XMSGPA        PLIST
082500970526     C                   PARM                    DSMSMS
082600970526     C                   PARM                    DSMSPG
082700970526     C                   PARM                    DSMSKE
082800970526     C                   PARM                    DSMSER
082900991108     C*---
083000970526     C*
083100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
083200970526     C                   CALL      'XKEY'
083300970526     C                   PARM                    DSKEY
083400970526     C*---
083500970526     C* VARIABILI NON RIFERITE AL DATA BASE
083600970526     C*---
083700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
083800070404     C     *LIKE         DEFINE    D11IOP0       DEPOP0
083900070404     C     *LIKE         DEFINE    D11IOP1       DEPOP1
084000970526     C*---
084100070122     C* VARIABILI DI CONTROLLO
084200970526     C*---
084300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
084400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
084500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
084600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
084700970526     C*
084800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
084900070404     C                   MOVEL     'FIDG12R'     DSMSPG
085000070404     C                   MOVEL     'FIDG12R'     MSGQUE
085100970526     C                   MOVEL     *BLANKS       MSGKEY
085200970526     C                   MOVEL     *BLANKS       DSMSMS
085300970526     C*---
085400970526     C* VARIABILI RIFERITE AL DATA BASE
085500970526     C*---
085600070403     C     *LIKE         DEFINE    grvfgs        K_grvfgs
085700070403     C     *LIKE         DEFINE    grvdde        K_grvdde
085800070417     C     *LIKE         DEFINE    grvkey        K_grvkey
085900970526     C*---
086000970526     C* CHIAVI DI LETTURA
086100970526     C*---
086200070406     C* LETTURA figrv01l - COMPLETA
086300070406     C     KEYgrv01_c    KLIST
086400070403     C                   KFLD                    K_grvfgs
086500070403     C                   KFLD                    K_grvdde
086600070417     C* LETTURA figrv02l - COMPLETA
086700070417     C     KEYgrv02_c    KLIST
086800070417     C                   KFLD                    K_grvfgs
086900070417     C                   KFLD                    K_grvkey
087000070404     C* LETTURA figrg04l - COMPLETA
087100070404     C     KEYgrg04_c    KLIST
087200070404     C                   KFLD                    K_grvfgs
087300070404     C                   KFLD                    K_grvdde
087400970528     C*
087500970528     C* CALCOLA LA DATA CORRENTE
087600970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
087700970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
087800970528     C                   Z-ADD     WN8           G08DAT
087900051122     C                   Z-ADD     WN8           DATCORINV         8 0
088000970528     C                   Z-ADD     *ZEROS        G08INV
088100970528     C                   MOVEL     '0'           G08ERR
088200970528     C                   CALL      'XSRDA8'
088300970528     C                   PARM                    WLBDA8
088400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
088500970526     C*
088600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
088700070404     C                   MOVEL     D11IOP0       DEPOP0                         *LIVELLO PROCEDURA
088800070404     C                   MOVEL     D11IOP1       DEPOP1                         *LIVELLO PROGRAMMA
088900970526     C*
089000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
089100970526     C                   EXSR      IMPFUN
089200970526     C*
089300970526     C                   ENDSR
089400970526     C*------------------------------------------------------------------------*
089500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
089600970526     C*------------------------------------------------------------------------*
089700970526     C     FUNGES        BEGSR
089800970526     C*
089900970526     C* PULISCO CAMPI DI LAVORO
090000970526     C                   Z-ADD     0             W                 5 0
090100970526     C*
090200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
090300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
090400970526     C     1             DO        24            I
090500970526     C                   MOVEL     FUD(I)        FUDRIG
090600970526     C*
090700970526     C* PULIZIA FUNZIONI PRECEDENTI
090800970526     C                   MOVEL     *BLANKS       FUV(I)
090900970526     C*
091000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
091100970526     C* (SALTA F24)
091200970526     C     FUDATT        IFEQ      'S'
091300970526     C     FUDFUN        ANDNE     '24'
091400970526     C                   ADD       1             W
091500970526     C                   MOVEL     FUDDES        FUV(W)
091600970526     C                   END
091700970526     C                   END                                                    DO WHILE
091800970526     C*
091900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
092000970526     C* ALTRIMENTI DISATTIVO F24
092100970526     C     1             DO        24            I
092200970526     C                   MOVEL     FUD(I)        FUDRIG
092300970526     C*
092400970526     C     FUDFUN        IFEQ      '24'
092500970526     C     W             IFGT      6
092600970526     C                   MOVEL     'S'           FUDATT
092700970526     C                   ELSE
092800970526     C                   MOVEL     *BLANKS       FUDATT
092900970526     C                   END                                                    W GT 8
093000970526     C                   MOVEL     FUDRIG        FUD(I)
093100970526     C                   END
093200970526     C                   END                                                    DO WHILE
093300970526     C*
093400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
093500970526     C     W             DIV       6             I
093600970526     C                   MVR                     Z
093700970526     C*
093800970526     C     Z             IFNE      0
093900970526     C     I             ADD       1             Z
094000970526     C                   ELSE
094100970526     C                   Z-ADD     I             Z                 5 0
094200970526     C                   END
094300970526     C*
094400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
094500970526     C                   ADD       1             FUNGRU
094600970526     C     FUNGRU        IFGT      Z
094700970526     C     FUNGRU        ORLT      1
094800970526     C                   Z-ADD     1             FUNGRU
094900970526     C                   END
095000970526     C*
095100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
095200970526     C     FUNGRU        IFEQ      1
095300970526     C                   MOVEL     FUV(1)        FUN001
095400970526     C                   MOVEL     FUV(2)        FUN002
095500970526     C                   MOVEL     FUV(3)        FUN003
095600970526     C                   MOVEL     FUV(4)        FUN004
095700970526     C                   MOVEL     FUV(5)        FUN005
095800970526     C                   MOVEL     FUV(6)        FUN006
095900970526     C                   END
096000970526     C*
096100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
096200970526     C     FUNGRU        IFEQ      2
096300970526     C                   MOVEL     FUV(7)        FUN001
096400970526     C                   MOVEL     FUV(8)        FUN002
096500970526     C                   MOVEL     FUV(9)        FUN003
096600970526     C                   MOVEL     FUV(10)       FUN004
096700970526     C                   MOVEL     FUV(11)       FUN005
096800970526     C                   MOVEL     FUV(12)       FUN006
096900970526     C                   END
097000970526     C*
097100970526     C* TERZO GRUPPO DI 6 FUNZIONI
097200970526     C     FUNGRU        IFEQ      3
097300970526     C                   MOVEL     FUV(13)       FUN001
097400970526     C                   MOVEL     FUV(14)       FUN002
097500970526     C                   MOVEL     FUV(15)       FUN003
097600970526     C                   MOVEL     FUV(16)       FUN004
097700970526     C                   MOVEL     FUV(17)       FUN005
097800970526     C                   MOVEL     FUV(18)       FUN006
097900970526     C                   END
098000970526     C*
098100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
098200970526     C     FUNGRU        IFEQ      4
098300970526     C                   MOVEL     FUV(19)       FUN001
098400970526     C                   MOVEL     FUV(20)       FUN002
098500970526     C                   MOVEL     FUV(21)       FUN003
098600970526     C                   MOVEL     FUV(22)       FUN004
098700970526     C                   MOVEL     *BLANKS       FUN005
098800970526     C                   MOVEL     *BLANKS       FUN006
098900970526     C                   END
099000970526     C*
099100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
099200970526     C                   MOVEL     *BLANKS       FUN007
099300970526     C                   MOVEL     *BLANKS       FUN008
099400970526     C*
099500970526     C     1             DO        24            I
099600970526     C                   MOVEL     FUD(I)        FUDRIG
099700970526     C     FUDFUN        IFEQ      '24'
099800970526     C     FUDATT        ANDEQ     'S'
099900970526     C                   MOVEL     FUDDES        FUN008
100000970526     C                   END
100100970526     C                   END                                                    DO
100200970526     C*
100300970526     C                   ENDSR
100400970526     C*------------------------------------------------------------------------*
100500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
100600970526     C*------------------------------------------------------------------------*
100700970526     C     FUNCON        BEGSR
100800970526     C*
100900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
101000970526     C                   EXSR      FUNVAR
101100970526     C*
101200970526     C* PULISCO CAMPI DI LAVORO
101300970526     C                   Z-ADD     0             I                 5 0
101400970526     C                   Z-ADD     0             W                 5 0
101500970526     C*
101600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
101700970526     C     FUNKEY        IFNE      *BLANKS
101800970526     C     1             DO        24            I
101900970526     C                   MOVEL     FUD(I)        FUDRIG
102000970526     C*
102100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
102200970526     C     FUNKEY        IFEQ      FUDFUN
102300970526     C     FUDATT        ANDEQ     'S'
102400970526     C                   Z-ADD     I             W
102500970526     C                   END
102600970526     C                   END                                                    DO WHILE
102700970526     C*
102800970526     C* FUNZIONE NON ATTIVA (ERRORE)
102900970526     C     W             IFEQ      0
103000970526     C*
103100970526     C* EMISSIONE MESSAGGIO DI ERRORE
103200970526     C                   MOVEL     MSG(1)        DSMSMS
103300970526     C*
103400970526     C                   END                                                    W=0
103500970526     C                   END                                                    FUNKEY NE *BLANKS
103600970526     C*
103700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
103800970526     C     W             IFNE      0
103900970526     C     FUNKEY        OREQ      *BLANKS
104000970526     C     FUNKEY        IFEQ      *BLANKS
104100970526     C                   EXSR      ENTGES
104200970526     C                   END
104300970526     C     FUNKEY        IFEQ      '03'
104400970526     C                   EXSR      F03GES
104500970526     C                   END
104600970526     C     FUNKEY        IFEQ      '06'
104700970526     C                   EXSR      F06GES
104800970526     C                   END
104900970526     C     FUNKEY        IFEQ      '12'
105000970526     C                   EXSR      F12GES
105100970526     C                   END
105200970526     C     FUNKEY        IFEQ      '24'
105300970526     C                   EXSR      F24GES
105400970526     C                   END
105500970526     C                   END
105600970526     C*
105700970526     C                   ENDSR
105800970526     C*------------------------------------------------------------------------*
105900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
106000970526     C*------------------------------------------------------------------------*
106100970526     C     FUNVAR        BEGSR
106200970526     C*
106300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
106400970526     C*
106500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
106600970526     C     KEY           IFEQ      ENTER
106700970526     C                   MOVEL     *BLANKS       FUNKEY
106800970526     C                   END
106900970526     C     KEY           IFEQ      F1
107000970526     C                   MOVEL     '01'          FUNKEY
107100970526     C                   END
107200970526     C     KEY           IFEQ      F2
107300970526     C                   MOVEL     '02'          FUNKEY
107400970526     C                   END
107500970526     C     KEY           IFEQ      F3
107600970526     C                   MOVEL     '03'          FUNKEY
107700970526     C                   END
107800970526     C     KEY           IFEQ      F4
107900970526     C                   MOVEL     '04'          FUNKEY
108000970526     C                   END
108100970526     C     KEY           IFEQ      F5
108200970526     C                   MOVEL     '05'          FUNKEY
108300970526     C                   END
108400970526     C     KEY           IFEQ      F6
108500970526     C                   MOVEL     '06'          FUNKEY
108600970526     C                   END
108700970526     C     KEY           IFEQ      F7
108800970526     C                   MOVEL     '07'          FUNKEY
108900970526     C                   END
109000970526     C     KEY           IFEQ      F8
109100970526     C                   MOVEL     '08'          FUNKEY
109200970526     C                   END
109300970526     C     KEY           IFEQ      F9
109400970526     C                   MOVEL     '09'          FUNKEY
109500970526     C                   END
109600970526     C     KEY           IFEQ      F10
109700970526     C                   MOVEL     '10'          FUNKEY
109800970526     C                   END
109900970526     C     KEY           IFEQ      F11
110000970526     C                   MOVEL     '11'          FUNKEY
110100970526     C                   END
110200970526     C     KEY           IFEQ      F12
110300970526     C                   MOVEL     '12'          FUNKEY
110400970526     C                   END
110500970526     C     KEY           IFEQ      F13
110600970526     C                   MOVEL     '13'          FUNKEY
110700970526     C                   END
110800970526     C     KEY           IFEQ      F14
110900970526     C                   MOVEL     '14'          FUNKEY
111000970526     C                   END
111100970526     C     KEY           IFEQ      F15
111200970526     C                   MOVEL     '15'          FUNKEY
111300970526     C                   END
111400970526     C     KEY           IFEQ      F16
111500970526     C                   MOVEL     '16'          FUNKEY
111600970526     C                   END
111700970526     C     KEY           IFEQ      F17
111800970526     C                   MOVEL     '17'          FUNKEY
111900970526     C                   END
112000970526     C     KEY           IFEQ      F18
112100970526     C                   MOVEL     '18'          FUNKEY
112200970526     C                   END
112300970526     C     KEY           IFEQ      F19
112400970526     C                   MOVEL     '19'          FUNKEY
112500970526     C                   END
112600970526     C     KEY           IFEQ      F20
112700970526     C                   MOVEL     '20'          FUNKEY
112800970526     C                   END
112900970526     C     KEY           IFEQ      F21
113000970526     C                   MOVEL     '21'          FUNKEY
113100970526     C                   END
113200970526     C     KEY           IFEQ      F22
113300970526     C                   MOVEL     '22'          FUNKEY
113400970526     C                   END
113500970526     C     KEY           IFEQ      F23
113600970526     C                   MOVEL     '23'          FUNKEY
113700970526     C                   END
113800970526     C     KEY           IFEQ      F24
113900970526     C                   MOVEL     '24'          FUNKEY
114000970526     C                   END
114100970526     C     KEY           IFEQ      ROLLUP
114200970526     C                   MOVEL     'RU'          FUNKEY
114300970526     C                   END
114400970526     C     KEY           IFEQ      HELP
114500970526     C                   MOVEL     'HP'          FUNKEY
114600970526     C                   END
114700970526     C                   ENDSR
114800970526     C*------------------------------------------------------------------------*
114900970526     C* NOTE COSTRUZIONE SCHIERE
115000970526     C*------------------------------------------------------------------------*
115100970526     C*
115200970526     C* -MSG-MESSAGGI
115300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
115400970526     C* (NON TOCCARE MSG,1 E MSG,2)
115500970526     C*
115600970526     C* -CMD-COMANDI
115700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
115800970526     C*
115900970526     C* -OPD-OPZIONI
116000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
116100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
116200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
116300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
116400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
116500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
116600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
116700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
116800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
116900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
117000970526     C*
117100970526     C* -FUD-FUNZIONI
117200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
117300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
117400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
117500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
117600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
117700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
117800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
117900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
118000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
118100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
118200970526     O*------------------------------------------------------------------------*
118300970526** DEC - DECODIFICHE
118400970526    MODIFICA                                                      1
118500970526  ANNULLAMENTO                                                    2
118600970526VISUALIZZAZIONE                                                   3
118700970526   RIPRISTINO                                                     4
118800970526  INSERIMENTO                                                     5
118900991014     COPIA                                                        6
119000970526** MSG - MESSAGGI
119100070404FIDG12R- Tasto funzionale non gestito                                          1
119200070404FIDG12R- Inserimento effettuato                                                2
119300070404FIDG12R- Modifica effettuata                                                   3
119400070404FIDG12R- Annullamento effettuato                                               4
119500070404FIDG12R- Ripristino effettuato                                                 5
119600070404FIDG12R- Richiesta funzione per record inesistente                             6
119700070404FIDG12R- Record momentaneamente allocato: riprovare più tardi                  7
119800070404FIDG12R- Richiesta funzione per record esistente                               8
119900070404FIDG12R- Giro già presente con questo codice                                   9
120000070404FIDG12R- Giro già presente con questa descrizione                              10
120100070404FIDG12R- Codice giro fuori convenzione, indicare l'ambito nel codice           11
120200070404FIDG12R- Campo obbligatorio                                                    12
120300070404FIDG12R- Codice filiale inesistente o di categoria V=varia                     13
120400070404FIDG12R- Data errata                                                           14
120500070404FIDG12R- La data fine deve essere maggiore della data inizio                   15
120600070404FIDG12R- Il record non può scadere in un giorno già trascorso                  16
120700070404FIDG12R- Il record immesso si sovrappone ad uno già in decorrenza              17
120800070404FIDG12R- Gestione automatica giri consegna non ancora attiva sulla Filiale     18
120900070404FIDG12R- Nessuna versione attiva alla data decorrenza indicata                 19
121000070404FIDG12R- Data scadenza deve essere inferiore o uguale al:                      20
121100070404FIDG12R- Utilizzo esclusivo a utenti EDP                                       21
121200070404FIDG12R- Il periodo indicato si sovrappone ad un altro già in essere           22
121300070404FIDG12R- Copia effettuata                                                      23
121400070417FIDG12R- Key versione indicata già utilizzata per la filiale corrente          24
121500070420FIDG12R- Il valore indicato nella Keyword x TELLUS non è tra quelli ammessi.   25
121600070404FIDG12R-                                                                       26
121700070404FIDG12R- Valori incompatibili, correggere!                                     27
121800970526** FUD - FUNZIONI
121900070201                   -        (S/N)=N                        funz.01¬
122000970526                   -        (S/N)=N                        funz.02¬
122100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
122200970526                   -        (S/N)=N                        funz.04¬
122300970526                   -        (S/N)=N                        funz.05¬
122400970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
122500970527                   -        (S/N)=N                        funz.07¬
122600970527                   -        (S/N)=N                        funz.08¬
122700970527                   -        (S/N)=N                        funz.09¬
122800970527                   -        (S/N)=N                        funz.10¬
122900970526                   -        (S/N)=N                        funz.11¬
123000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
123100970526                   -        (S/N)=N                        funz.13¬
123200970526                   -        (S/N)=N                        funz.14¬
123300970526                   -        (S/N)=N                        funz.15¬
123400970526                   -        (S/N)=N                        funz.16¬
123500970526                   -        (S/N)=N                        funz.17¬
123600970526                   -        (S/N)=N                        funz.18¬
123700970526                   -        (S/N)=N                        funz.19¬
123800970526                   -        (S/N)=N                        funz.20¬
123900970526                   -        (S/N)=N                        funz.21¬
124000970526                   -        (S/N)=N                        funz.22¬
124100970526                   -        (S/N)=N                        funz.23¬
124200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
