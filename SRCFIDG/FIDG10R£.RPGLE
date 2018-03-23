000100970526     H*------------------------------------------------------------------------*
000200070122     H* Gestione anagrafica giri
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800070122     Ffidg10d   cf   e             WORKSTN INFDS(DEVDS1)
000900070122     F                                     SFILE(DG10DMS:MSGULT)
001000970527     F*------------
001100070122     Ffigrg01l  UF A E           K DISK
001200070122     Fazorg01l  IF   E           K DISK
001300070403     Ffigrv01l  IF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D*------------
001800970526     D* DECODIFICHE
001900970526     D*------------
002000991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* MESSAGGI
002300970526     D*------------
002400060616     D MSG             S             78    DIM(27) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* DESCRIZIONI TASTI FUNZIONALI
002700970526     D*------------
002800970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* FUNZIONI DA CARICARE A VIDEO
003100970526     D*------------
003200970526     D FUV             S             19    DIM(24)
003300970526     D*------------------------------------------------------------------------*
003400970526     D* INPUT
003500970526     D*------------------------------------------------------------------------*
003600970526     D*------------
003700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003800970526     D*------------
003900970526     D FUDDS           DS
004000970526     D  FUDDE1                 1      1
004100970526     D  FUDFUN                 2      3
004200970526     D  FUDDE2                 4     19
004300970526     D  FUDFI0                20     34
004400970526     D  FUDATT                35     35
004500970526     D  FUDFI1                36     80
004600970526     D  FUDDES                 1     19
004700970526     D  FUDRIG                 1     80
004800970526     D*------------
004900970526     D* DS GESTIONE TASTI FUNZIONALI
005000970526     D*------------
005100970526     D DSKEY         E DS
005200970526     D DEVDS1          DS
005300970526     D  KEY                  369    369
005400970526     D*------------
005500970526     D* DS "XMSG" - GESTIONE MESSAGGI
005600970526     D*------------
005700970526     D DSMSG         E DS
005800970528     D*------------------
005900970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006000970528     D*------------------
006100970528     D WLBDA8          DS                  INZ
006200970528     D  G08DAT                 1      8  0
006300970528     D  G08INV                 9     16  0
006400970528     D  G08ERR                17     17
006500970528     D  G08TGI                18     22  0
006600970526     D*------------
006700970526     D* DS DI PROCEDURA
006800970526     D*------------
006900070122     D fidg09ds      E DS
007000070403     D fidg11ds      E DS
007100070209     D DGRGFLO       E DS
007200070122     D*------------
007300070122     D* DS REPERIMENTO DATI UTENTE
007400070122     D*------------
007500070122     D TIBS34DS      E DS                                                       *Profili utente
007600070122     D DDATIUTE      E DS                                                       *Dati utente
007700070122     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007800070124     D*------------
007900070124     D* DS CONTROLLO FILIALI IN GESTIONE
008000070124     D*------------
008100070124     D FNLV50DS      E DS
008200070124     D*------------
008300070124     D* DS DESCRIZIONI ORGANIGRAMMA
008400070124     D*------------
008500070124     D OG148         E DS
008600970526     D*------------
008700970526     D* ARCHITETTURA
008800970526     D*------------
008900970526     D KPJBA         E DS
009000040816     D*------------
009100070426     D* COSTANTI
009200070426     D*------------
009300070426     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
009400070426     D                                     1234567890 ')
009500070426     D*-------------------
009600070426     D* SCHIERE DI WRK
009700070426     D*-------------------
009800070426     D SKcharOK        S              1    DIM(37)
009900110624     d
010000110624     D DSGRG         E DS                  extname(figrg00F)
010100110624     D oldgrg        E DS                  extname(figrg00F) prefix(O_)
010200110624     D newgrg        E DS                  extname(figrg00F) prefix(N_)
010300970526     C*------------------------------------------------------------------------*
010400970526     C* MAIN LINES
010500970526     C*------------------------------------------------------------------------*
010600970526     C*
010700970526     C* NON CI SONO ERRORI IN ENTRATA
010800970526IF  1C     ERRL00        IFEQ      '0'
010900970526     C*
011000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011400970526     C*
011500970526     C* EMETTE FORMATO VIDEO
011600970526     C                   EXSR      BUTFMR
011700970526     C*
011800970526     C* GESTIONE TASTI FUNZIONALI
011900970526     C                   EXSR      FUNCON
012000970526E   2C                   ENDDO
012100970526     C*
012200970526     C* CI SONO ERRORI IN ENTRATA
012300970527X   1C                   ELSE
012400070124     C                   MOVEL     '1'           D09oerr
012500070124     C                   MOVEL     DSMSMS        D09omsg
012600970526E   1C                   ENDIF
012700070123     C*
012800070123     C* RITORNO PARAMETRI
012900070123     C                   MOVEL(P)  fidg09ds      KPJBU
013000070122     C* FINE PGM
013100970526     C                   SETON                                        LR
013200970526     C*------------------------------------------------------------------------*
013300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013400970526     C*------------------------------------------------------------------------*
013500970526     C     BUTFMR        BEGSR
013600970526     C*
013700970526     C* EMETTE LA TESTATA
013800070122     C                   WRITE     DG10D01
013900970526     C*
014000970526     C* SPEDISCE EVENTUALE MESSAGGIO
014100970526     C                   EXSR      SNDMSG
014200970526     C*
014300970526     C* EMETTE LE FUNZIONI
014400070122     C                   WRITE     DG10D02
014500970526     C*
014600970526     C* EMETTE IL VIDEO
014700070122     C                   EXFMT     DG10D03
014800970526     C*
014900970526     C* REIMPOSTA FLAG DI ERRORE
015000970526     C                   MOVEL     '0'           ERRL00
015100970526     C*
015200970526     C* REIMPOSTA INDICATORI DI ERRORE
015300070122     C                   SETOFF                                       707172
015400070122     C                   SETOFF                                       737475
015500120402     C                   setoff                                       787796
015600970526     C*
015700970526     C                   ENDSR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016000970526     C*------------------------------------------------------------------------*
016100970526     C     SNDMSG        BEGSR
016200970526     C*
016300970526     C* PULISCE IL SFILE MESSAGGI
016400970526     C                   MOVEL     '1'           *IN40
016500070122     C                   WRITE     DG10DMC
016600970526     C                   MOVEL     '0'           *IN40
016700970526     C*
016800970526     C* VISUALIZZA IL MESSAGGIO
016900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017000970526     C                   CALL      'XMSG'        XMSGPA
017100970526IF  1C     DSMSER        IFEQ      *ZEROS
017200970526     C                   MOVEL     DSMSKE        MSGKEY
017300970526E   1C                   ENDIF
017400970526     C                   Z-ADD     1             MSGULT
017500070122     C                   WRITE     DG10DMS
017600070122     C                   WRITE     DG10DMC
017700970526     C*
017800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017900970526     C                   MOVEL     *BLANKS       DSMSMS
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* ENTGES - GESTIONE ENTER -> INVIO
018400970526     C*------------------------------------------------------------------------*
018500970526     C     ENTGES        BEGSR
018600970526     C*
018700970526     C* CONTROLLA I CAMPI IMMESSI
018800970526     C                   EXSR      DECODI
018900970526     C*
019000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
019100970526IF  1C     DEPOP1        IFEQ      'O05'
019200970526     C                   MOVEL     '1'           FINL00
019300970526E   1C                   ENDIF
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* F03GES - GESTIONE F03 -> FINE
019800970526     C*------------------------------------------------------------------------*
019900970526     C     F03GES        BEGSR
020000970526     C*
020100970526     C* IMPOSTA "PREMUTO F03"
020200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020300070124     C                   MOVEL     '1'           D09OF03                        *DI PROCEDURA
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F06GES        BEGSR
021000970526     C*
021100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
021200970526IF  1C     DEPOP1        IFEQ      'F06'
021300970526     C                   EXSR      INSERI
021400970526E   1C                   ENDIF
021500970526IF  1C     DEPOP1        IFEQ      'O02'
021600970526     C                   EXSR      MODIFI
021700970526E   1C                   ENDIF
021800991014IF  1C     DEPOP1        IFEQ      'O03'
021900011129     C                   EXSR      COPIA
022000991014E   1C                   ENDIF
022100070123IF  1C     DEPOP1        IFEQ      'O04'
022200070123     C                   EXSR      ANNULL
022300070123E   1C                   ENDIF
022400970526IF  1C     DEPOP1        IFEQ      'O07'
022500970526     C                   EXSR      RIPRIS
022600970526E   1C                   ENDIF
022700011129     C*
022800970526     C                   ENDSR
022900070403     C*------------------------------------------------------------------------*
023000070403     C* F10GES - GESTIONE F10 -> RICERCA VERSIONI
023100070403     C*------------------------------------------------------------------------*
023200070403     C     F10GES        BEGSR
023300070403     C*
023400070403     C                   clear                   fidg11ds
023500070403     C                   eval      d11iop0 = 'P01'
023600070423     C                   if        VIDFGS  > *zeros
023700070423     C                   move(p)   VIDFGS        d11ifgs
023800070423     C                   endif
023900070403     C                   z-add     datcor        d11idda
024000070403     C                   eval      kpjbu = fidg11ds
024100070403     C                   call      'FIDG11R'
024200070403     C                   parm                    kpjba
024300070403     C                   eval      fidg11ds = kpjbu
024400070403     C                   if        d11oerr  =  *blanks AND
024500070403     C                             d11odde  <> *zeros
024600070403     C                   eval      VIDDDE   =  d11odde
024700070406     C                   if        VIDDSCAD = *zeros
024800070403     C                   eval      VIDDSCAD =  d11odsc
024900070406     C                   endif
025000070403     C                   endif
025100070403     C*
025200070403     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400970526     C* F12GES - GESTIONE F12 -> RITORNO
025500970526     C*------------------------------------------------------------------------*
025600970526     C     F12GES        BEGSR
025700970526     C*
025800970526     C* IMPOSTA "PREMUTO F12"
025900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026000070124     C                   MOVEL     '1'           D09OF12                        *DI PROCEDURA
026100970526     C*
026200970526     C                   ENDSR
026300970526     C*------------------------------------------------------------------------*
026400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026500970526     C*------------------------------------------------------------------------*
026600970526     C     F24GES        BEGSR
026700970526     C*
026800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026900970526     C*
027000970526     C                   ENDSR
027100970526     C*------------------------------------------------------------------------*
027200970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
027300970526     C*------------------------------------------------------------------------*
027400970526     C     INSERI        BEGSR
027500970526     C*
027600970526     C* CONTROLLA I CAMPI IMMESSI
027700970526     C                   EXSR      DECODI
027800970526     C*
027900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028000991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
028100070122     C                   clear                   figrg000
028200070122     C                   exsr      impgrg
028300070614     C                   if        vidFIT <> 'S'
028400070614     C                   eval      grgFTT =  'S'
028500070614     C                   endif
028600070122     C                   write     figrg000
028700070124     C                   movel     MSG(2)        D09OMSG                        *MESSAGGIO
028800970602     C*
028900970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
029000070122     C                   exsr      azzvid
029100070122E   1C                   endif
029200970526     C*
029300970526     C                   ENDSR
029400970526     C*------------------------------------------------------------------------*
029500970526     C* MODIFI - OPERAZIONI DI MODIFICA
029600970526     C*------------------------------------------------------------------------*
029700970526     C     MODIFI        BEGSR
029800970526     C*
029900970526     C* CONTROLLA I CAMPI IMMESSI
030000970526     C                   EXSR      DECODI
030100970526     C*
030200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030400070122     C                   EXSR      LETgrg
030500110624     c                   movel     dsgrg         oldgrg
030600110624     c* clearo flag trasmissione e falg centro storico
030700110624     c*  che non mi servono per decidere se scaricare a Tellus
030800110624     c                   clear                   o_grgftt
030900110624     c                   clear                   o_grgute
031000110624     c                   clear                   o_grgduv
031100110624     C                   movel     o_grgflo      DGRGFLO
031200110624     c                   clear                   §grgcsto
031300120402     c                   clear                   §grgfgm
031400120402     c                   movel     dgrgflo       o_grgflo
031500970526     C*
031600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
031700970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031800070122     C                   EXSR      impgrg                                       *IMPOSTA I CAMPI
031900110624     c
032000070614     C                   if        vidFIT <> 'S'
032100110624     c* se ho modificato un dato sensibile allo scarico --> flaggo
032200110624     c                   movel     dsgrg         newgrg
032300110624     c* clearo flag trasmissione e falg centro storico
032400110624     c*  che non mi servono per decidere se scaricare a Tellus
032500110624     c                   clear                   n_grgftt
032600110624     c                   clear                   n_grgute
032700110624     c                   clear                   n_grgduv
032800110624     C                   movel     n_grgflo      DGRGFLO
032900110624     c                   clear                   §grgcsto
033000120402     c                   clear                   §grgfgm
033100110624     c                   movel     dgrgflo       n_grgflo
033200110624     c
033300110624     c                   if        oldgrg<>newgrg
033400070614     C                   eval      grgFTT =  'S'
033500070614     C                   endif
033600110624     c                   else
033700110624     c                   clear                   grgftt
033800110624     C                   endif
033900110624     c
034000070122     C                   UPDATE    figrg000
034100070124     C                   MOVEL     MSG(3)        D09OMSG
034200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
034300970526E   2C                   ENDIF
034400970526E   1C                   ENDIF
034500970526     C*
034600970526     C                   ENDSR
034700991014     C*------------------------------------------------------------------------*
034800070122     C* COPIA  - OPERAZIONI DI COPIA
034900991014     C*------------------------------------------------------------------------*
035000070122     C     COPIA         BEGSR
035100991014     C*
035200991014     C*
035300991014     C                   ENDSR
035400970526     C*------------------------------------------------------------------------*
035500970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
035600970526     C*------------------------------------------------------------------------*
035700070122     C     ANNULL        BEGSR
035800991025     C*
035900991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
036000070122     C                   EXSR      LETgrg
036100991025     C*
036200991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
036300070122IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036400070122     C                   EVAL      grgATB = 'A'
036500070614     C                   if        vidFIT <> 'S'
036600070614     C                   eval      grgFTT =  'S'
036700070614     C                   endif
036800070122     C                   UPDATE    figrg000                                     *CANCELLA
036900070124     C                   MOVEL     MSG(4)        D09OMSG                        *MESSAGGIO
037000070122     C                   MOVEL     '1'           FINL00                         *FINE PGM
037100070122E   1C                   ENDIF
037200970526     C*
037300070122     C                   ENDSR
037400970526     C*------------------------------------------------------------------------*
037500970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
037600970526     C*------------------------------------------------------------------------*
037700970526     C     RIPRIS        BEGSR
037800070122     C*
037900070122     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
038000070122     C                   EXSR      LETgrg
038100070122     C*
038200070122     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
038300070122IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038400070122     C                   EVAL      grgATB = *blanks
038500070614     C                   if        vidFIT <> 'S'
038600070614     C                   eval      grgFTT =  'S'
038700070614     C                   endif
038800070122     C                   UPDATE    figrg000                                     *CANCELLA
038900070124     C                   MOVEL     MSG(5)        D09OMSG                        *MESSAGGIO
039000070122     C                   MOVEL     '1'           FINL00                         *FINE PGM
039100070122E   1C                   ENDIF
039200070122     C*
039300970526     C                   ENDSR
039400970526     C*------------------------------------------------------------------------*
039500070122     C* LETgrg - LEGGE IL RECORD PERL'AGGIORNAMENTO
039600970526     C*------------------------------------------------------------------------*
039700070122     C     LETgrg        BEGSR
039800970526     C*
039900070206     C                   IF        DEPOP1 <> 'F06'
040000070124     C                   Z-ADD     D09idde       K_grgdde
040100070124     C                   Z-ADD     D09ifgs       K_grgfgs
040200070124     C                   MOVEL     D09icgi       K_grgcgi
040300070206     C                   ENDIF
040400070122     C     KEYgrg01      CHAIN     figrg01l                           8081
040500070122IF  1C     *IN81         IFEQ      *ON                                          *ALLOCATO
040600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
040700070124     C                   MOVEL     MSG(7)        D09OMSG                        *MESSAGGIO
040800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
040900970526E   1C                   ENDIF
041000070122IF  1C     *IN80         IFEQ      *ON                                          *INSESISTENTE
041100070122     C                   IF        DEPOP1 <> 'F06'
041200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
041300070124     C                   MOVEL     MSG(6)        D09OMSG                        *MESSAGGIO
041400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
041500070122     C                   ENDIF
041600970526E   1C                   ENDIF
041700970526     C*
041800970526     C                   ENDSR
041900970526     C*------------------------------------------------------------------------*
042000970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
042100970526     C*------------------------------------------------------------------------*
042200970526     C     DECODI        BEGSR
042300001120     C*
042400070403     C*--------------------
042500070403     C* CONTROLLO FILIALE
042600070403     C*--------------------
042700070403     C     ERRL00        IFEQ      '0'
042800070403     C     vidfgs        IFNE      *blanks
042900070403     C     vidfgs        ANDNE     *zeros
043000070403     C                   EXSR      cntpo
043100070403     C                   ELSE
043200070403     C                   MOVEL     '1'           ERRL00
043300070403     C                   MOVEL     MSG(12)       DSMSMS
043400070403     C                   SETON                                        72
043500070403     C                   ENDIF
043600070403     C                   ENDIF
043700070403     C*--------------------
043800070403     C* CONTROLLO ABILITAZIONE UTENTE ALLA FILIALE
043900070403     C*--------------------
044000070403     C     ERRL00        IFEQ      '0'
044100070403     C                   IF        VIDFGS <> *BLANKS AND
044200070403     C                             VIDFGS <> *ZEROS  AND
044300070403     C                             *IN94 = *OFF
044400070403     C                   CLEAR                   FNLV50DS
044500070403     C                   EVAL      D50TLA = 'L'
044600070403     C                   EVAL      D50PRU = KNMUS
044700070403     C                   EVAL      D50FGS = wvidfgs
044800070403     C                   CALL      'FNLV50R'
044900070403     C                   PARM                    FNLV50DS
045000070403     C                   IF        D50ERR <> *BLANKS
045100070403     C                   MOVEL     '1'           ERRL00
045200070403     C                   MOVEL     D50MSG        DSMSMS
045300070403     C                   SETON                                        72
045400070403     C                   ENDIF
045500070403     C                   ENDIF
045600070403     C                   ENDIF
045700070403     C*--------------------
045800070403     C* CONTROLLO FILIALE ATTIVATA A "TELLUS"
045900070403     C*--------------------
046000070403     C     ERRL00        IFEQ      '0'
046100070403     C                   IF        VIDFGS <> *BLANKS AND
046200070403     C                             VIDFGS <> *ZEROS  AND
046300070403     C                             *IN94 = *OFF
046400070403     C     wvidfgs       CHAIN     azorg01l
046500070403     C                   IF        %found(azorg01l)
046600070403     C                   EVAL      OG148 = orgde8
046700070403     C                   ELSE
046800070403     C                   CLEAR                   OG148
046900070403     C                   ENDIF
047000070403     C*
047100070403     C                   IF        §OGTLLS = *BLANKS
047200070403     C                   MOVEL     '1'           ERRL00
047300070403     C                   MOVEL     MSG(18)       DSMSMS
047400070403     C                   SETON                                        72
047500070403     C                   ENDIF
047600070403     C                   ENDIF
047700070403     C                   ENDIF
047800070403     C*--------------------
047900070404     C* VERIFICA VERSIONI APERTE X LA FILIALE CORRENTE (SOLO IN INSERIMENTO)
048000070403     C*--------------------
048100070403     C     ERRL00        IFEQ      '0'
048200070404     C                   IF        DEPOP1 = 'F06'
048300070403     C                   EXSR      CNTVER
048400070403     C                   IF        wCntVer = *zeros
048500070403     C                   MOVEL     '1'           ERRL00
048600070403     C                   SETON                                        70
048700070403     C                   MOVEL     MSG(19)       DSMSMS
048800070403     C                   ENDIF
048900070403     C*
049000070403     C                   IF        wCntVer = 1
049100070403     C                   Z-ADD     wTopDdeVer    G08INV
049200070403     C                   Z-ADD     *ZEROS        G08DAT
049300070403     C                   MOVEL     '3'           G08ERR
049400070403     C                   CALL      'XSRDA8'
049500070403     C                   PARM                    WLBDA8
049600070403     C                   Z-ADD     G08DAT        viddde
049700070403     C                   Z-ADD     G08INV        wviddde
049800070420     C                   Z-ADD     wTopDscVer    G08INV
049900070420     C                   Z-ADD     *ZEROS        G08DAT
050000070420     C                   MOVEL     '3'           G08ERR
050100070420     C                   CALL      'XSRDA8'
050200070420     C                   PARM                    WLBDA8
050300070420     C                   Z-ADD     G08DAT        viddscad
050400070420     C                   Z-ADD     G08INV        wviddscad
050500070420     C                   SETON                                        93
050600070403     C                   ENDIF
050700070403     C*
050800070406     C                   IF        wCntVer > 1      AND
050900070406     C                             viddde  = *zeros
051000070403     C                   EXSR      F10GES
051100070404     C                   ENDIF
051200070403     C                   ENDIF
051300070403     C                   ENDIF
051400991026     C*--------------------
051500070403     C* DATA DECORRENZA
051600991026     C*--------------------
051700070122     C     *LIKE         DEFINE    viddde        wviddde
051800001120     C     ERRL00        IFEQ      '0'
051900070122     C                   Z-ADD     viddde        wda8
052000991026     C                   Z-ADD     *ZEROS        da8w
052100991026     C                   EXSR      CHKDA8
052200070122     C                   Z-ADD     WDA8          viddde
052300070122     C                   Z-ADD     DA8W          wviddde
052400001120     C     ERRL00        IFEQ      '1'
052500070122     C                   SETON                                        70
052600070122     C                   MOVEL     MSG(14)       DSMSMS
052700001120     C                   ENDIF
052800001120     C                   ENDIF
052900070403     C*--------------------
053000070403     C* DATA DECORRENZA - VERIFICA VERSIONE
053100070403     C*--------------------
053200070403     C     ERRL00        IFEQ      '0'
053300070403     C                   Z-ADD     wvidfgs       K_grvfgs
053400070403     C                   Z-ADD     wviddde       K_grvdde
053500070406     C     KEYgrv01_c    CHAIN     figrv01l
053600070403     C                   IF        %found(figrv01l)
053700070403     C                   EVAL      wTopDdeVer = grvdde
053800070403     C                   EVAL      wTopDscVer = grvdscad
053900070403     C                   ELSE
054000070403     C                   MOVEL     '1'           ERRL00
054100070403     C                   SETON                                        70
054200070403     C                   MOVEL     MSG(19)       DSMSMS
054300070403     C                   ENDIF
054400070403     C                   ENDIF
054500070122     C*--------------------
054600070122     C* DATA SCADENZA
054700070122     C*--------------------
054800070124     C     *LIKE         DEFINE    viddscad      wviddscad
054900070122     C     ERRL00        IFEQ      '0'
055000070124     C                   Z-ADD     viddscad      WDA8
055100070122     C                   Z-ADD     *ZEROS        DA8W
055200070122     C                   EXSR      CHKDA8
055300070124     C                   Z-ADD     WDA8          viddscad
055400070124     C                   Z-ADD     DA8W          wviddscad
055500070122     C     ERRL00        IFEQ      '1'
055600070122     C                   SETON                                        71
055700070122     C                   MOVEL     MSG(14)       DSMSMS
055800070122     C                   ENDIF
055900070122     C                   ENDIF
056000070403     C*--------------------
056100070403     C* DATA SCADENZA ENTRO LIMITI SCADENZA VERSIONE RELATIVA
056200070403     C*--------------------
056300070403     C     ERRL00        IFEQ      '0'
056400070403     C                   IF        wviddscad <= wTopDscVer
056500070403     C                   ELSE
056600070403     C                   MOVEL     '1'           ERRL00
056700070403     C                   SETON                                        71
056800070403     C                   EVAL      DSMSMS = %trim(MSG(20))+ ' ' +
056900070403     C                                      %editw(wTopDscVer:'    /  /  ')
057000070403     C                   ENDIF
057100070403     C                   ENDIF
057200070122     C*--------------------
057300070122     C* SCADENZA < DECORRENZA
057400070122     C*--------------------
057500070122     C     ERRL00        IFEQ      '0'
057600070124     C     wviddscad     IFLT      wviddde
057700070122     C                   SETON                                        7071
057800070122     C                   MOVEL     MSG(15)       DSMSMS
057900070122     C                   MOVEL     '1'           ERRL00
058000070122     C                   ENDIF
058100070122     C                   ENDIF
058200070122     C*--------------------
058300070122     C* SCADENZA >= DATA CORRENTE
058400070122     C*--------------------
058500070122     C     ERRL00        IFEQ      '0'
058600070122     C     DEPOP1        IFEQ      'O02'
058700070122     C     DEPOP1        OREQ      'O07'
058800070122     C     DEPOP1        OREQ      'F06'
058900070124     C     wviddscad     IFLT      DATCOR
059000070122     C                   SETON                                        71
059100070122     C                   MOVEL     MSG(16)       DSMSMS
059200070122     C                   MOVEL     '1'           ERRL00
059300070122     C                   ENDIF
059400070122     C                   ENDIF
059500070122     C                   ENDIF
059600070122     C*--------------------
059700070122     C* DECORRENZA + SCADENZA
059800070122     C*--------------------
059900070122     C* ACCAVALLAMENTO DATI
060000070122     C     ERRL00        IFEQ      '0'
060100070122     C     *start        SETLL     figrg01l
060200070122     C                   READ(N)   figrg01l
060300070122     C                   DOW       not %eof(figrg01l)
060400070123     C                   IF        grgfgs  = wvidfgs AND
060500070123     C                             grgcgi  = vidcgi  AND
060600070123     C                             grgdde <> wviddde
060700070124     C                   IF        (wviddde  <=grgdscad AND grgdde<=wviddde) OR
060800070124     C                             (wviddscad>=grgdde AND grgdscad >= wviddscad)
060900070122     C                   SETON                                        7071
061000070122     C                   MOVEL     MSG(17)       DSMSMS
061100070122     C                   MOVEL     '1'           ERRL00
061200070122     C                   LEAVE
061300070122     C                   ENDIF
061400070122     C                   ENDIF
061500070122     C                   READ(N)   figrg01l
061600070122     C                   ENDDO
061700070122     C                   ENDIF
061800070406     C*--------------------
061900070406     C* OBBLIGATORIETA CAMPI
062000070406     C*--------------------
062100070406     C     ERRL00        IFEQ      '0'
062200070406     C                   IF        viddde = *zeros
062300070406     C                   MOVEL     '1'           ERRL00
062400070406     C                   MOVEL     MSG(12)       DSMSMS
062500070406     C                   SETON                                        70
062600070406     C                   ENDIF
062700070406     C                   IF        viddscad = *zeros
062800070406     C                   MOVEL     '1'           ERRL00
062900070406     C                   MOVEL     MSG(12)       DSMSMS
063000070406     C                   SETON                                        71
063100070406     C                   ENDIF
063200070406     C                   IF        vidfgs = *blanks OR
063300070406     C                             vidfgs = *zeros
063400070406     C                   MOVEL     '1'           ERRL00
063500070406     C                   MOVEL     MSG(12)       DSMSMS
063600070406     C                   SETON                                        72
063700070406     C                   ENDIF
063800070406     C                   IF        vidcgi = *blanks
063900070406     C                   MOVEL     '1'           ERRL00
064000070406     C                   MOVEL     MSG(12)       DSMSMS
064100070406     C                   SETON                                        73
064200070406     C                   ENDIF
064300070406     C                   IF        viddes = *blanks
064400070406     C                   MOVEL     '1'           ERRL00
064500070406     C                   MOVEL     MSG(12)       DSMSMS
064600070406     C                   SETON                                        74
064700070406     C                   ENDIF
064800070406     C                   IF        vidain = *blanks
064900070406     C                   MOVEL     '1'           ERRL00
065000070406     C                   MOVEL     MSG(12)       DSMSMS
065100070406     C                   SETON                                        75
065200070406     C                   ENDIF
065300070406     C                   ENDIF
065400120402     c* flg giro micro solo se ambito SOTTO
065500120402     C                   IF        vidain <>'<' and vidfgm <> ' '
065600120402     c                             and ERRL00 = '0'
065700120402     C                   MOVEL     '1'           ERRL00
065800120402     C                   MOVEL     MSG(24)       DSMSMS
065900120402     C                   SETON                                        96
066000120402     C                   ENDIF
066100070426     C*--------------------
066200070426     C* SOLO CARATTERI AMMESSI
066300070426     C*--------------------
066400070426     C     ERRL00        IFEQ      '0'
066500070426     C* Codice giro
066600070426     C                   Z-ADD     *zeros        wLen              2 0
066700070426     C                   EVAL      wLen = %len(%trim(VIDCGI))
066800070426     C     1             DO        wLen          x                 2 0
066900070426     C                   MOVEL     *blanks       wrkCHAR           1
067000070426     C                   EVAL      wrkCHAR = %subst(VIDCGI:x:1)
067100070426     C     wrkCHAR       LOOKUP    SKcharOK                               20
067200070426     C                   IF        *in20 = *off or wrkCHAR = *blanks
067300070426     C                   MOVEL     '1'           ERRL00
067400070426     C                   MOVEL     MSG(21)       DSMSMS
067500070426     C                   SETON                                        73
067600070426     C                   ENDIF
067700070426     C                   ENDDO
067800070426     C* Descrizione giro
067900070426     C     1             DO        30            x                 2 0
068000070426     C                   MOVEL     *blanks       wrkCHAR           1
068100070426     C                   EVAL      wrkCHAR = %subst(VIDDES:x:1)
068200070426     C     wrkCHAR       LOOKUP    SKcharOK                               20
068300070426     C                   IF        *in20 = *off
068400070426     C                   MOVEL     '1'           ERRL00
068500070426     C                   MOVEL     MSG(21)       DSMSMS
068600070426     C                   SETON                                        74
068700070426     C                   ENDIF
068800070426     C                   ENDDO
068900070426     C*
069000070426     C                   ENDIF
069100070622     C*--------------------
069200070622     C* CONTROLLO TIPOLOGIA GIRO
069300070622     C*--------------------
069400070622     C     ERRL00        IFEQ      '0'
069500070622     C     VIDTUG        ANDEQ     ' '
069600070622     C
069700070622     C                   MOVEL     '1'           ERRL00
069800070622     C                   MOVEL     MSG(12)       DSMSMS
069900070622     C                   SETON                                        77
070000070622     C                   ENDIF
070100080124     C*--------------------
070200080124     C* IN CASO DI MODIFICA TIPOLOGIA GIRO DA CONSEGNA/ENTRAMBI  A SOLO RITIRO
070300080124     C*--------------------
070400080124     C     ERRL00        IFEQ      '0'
070500080514     C                   IF        VIDTUG = 'R' AND (§GRGTUG = 'C'
070600080514     C                             or §grgtug = 'E')
070700080124     C                   MOVEL     '1'           ERRL00
070800080124     C                   MOVEL     MSG(23)       DSMSMS
070900080124     C                   SETON                                        77
071000080124     C                   ENDIF
071100080124     C                   ENDIF
071200070622     C*--------------------
071300070622     C* IN CASO DI GIRO CON USO SOLO RITRO
071400070622     C*--------------------
071500070622     C     ERRL00        IFEQ      '0'
071600070622     C                   IF        VIDTUG = 'R' AND VIDFIT = ' '
071700070622     C                   MOVEL     '1'           ERRL00
071800070622     C                   MOVEL     MSG(22)       DSMSMS
071900070622     C                   SETON                                        78
072000070622     C                   ENDIF
072100070622     C                   ENDIF
072200070122     C*--------------------
072300991026     C* INSERIMENTO -> RECORD INESISTENTE
072400070122     C*--------------------
072500001120     C     ERRL00        IFEQ      '0'
072600001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
072700070206     C                   z-add     wviddde       K_grgdde
072800070124     C                   z-add     wvidfgs       K_grgfgs
072900070124     C                   movel     vidcgi        K_grgcgi
073000070122     C                   exsr      letgrg
073100070206     C                   if        *in80 = *off
073200070122     C                   movel     '1'           ERRL00
073300070122     C                   movel     MSG(8)        DSMSMS
073400070122     C                   seton                                        707172
073500070122     C                   seton                                        73
073600070122     C                   endif
073700070122     C                   ENDIF
073800070122     C                   ENDIF
073900001120     C*
074000001120     C                   ENDSR
074100991027     C*------------------------------------------------------------------------*
074200070122     C* CNTPO  - Controlla codice filiale
074300991027     C*------------------------------------------------------------------------*
074400070122     C     CNTPO         begsr
074500001120     C*
074600070122     C                   movel     *blanks       vddfgs
074700070122     C     '?'           scan      vidfgs                                 97
074800001120     C                   if        *in97
074900070122     C                   movel     *blanks       cod1
075000070122     C                   movel     *blanks       des1
075100070122     C                   movel     *blanks       pra1
075200001120     C                   call      'TNSD24R'
075300001120     C                   parm                    cod1              3
075400001120     C                   parm                    tip1              1
075500001120     C                   parm                    des1             25
075600001120     C                   parm                    pra1              3
075700070122     C                   movel     cod1          vidfgs
075800001120     C                   endif
075900070122     C                   move      vidfgs        cod1n             3 0
076000001120     C     cod1n         chain     azorg01l                           98
076100001120     C                   if        not *in98 and orgfag <>'V'
076200070123     C                   z-add     orgfil        wvidfgs           3 0
076300070122     C                   movel     orgdes        vddfgs
076400001120     C                   else
076500001120     C                   seton                                        69
076600001120     C                   move      '1'           errl00
076700001120     C                   movel     msg(13)       dsmsms
076800001120     C                   endif
076900001120     C*
077000001120     C                   endsr
077100991026     C*------------------------------------------------------------------------*
077200991026     C* CHKDA8 - CONTROLLA LA DATA (8)
077300991026     C*------------------------------------------------------------------------*
077400991026     C     CHKDA8        BEGSR
077500991026     C*
077600070122     C     *LIKE         DEFINE    viddde        WDA8
077700070122     C     *LIKE         DEFINE    viddde        DA8W
077800991026     C*
077900991026     C                   Z-ADD     WDA8          G08DAT
078000991026     C                   Z-ADD     *ZEROS        G08INV
078100991026     C                   MOVEL     '0'           G08ERR
078200991026     C                   CALL      'XSRDA8'
078300991026     C                   PARM                    WLBDA8
078400991026     C                   Z-ADD     G08DAT        WDA8
078500991026     C                   Z-ADD     G08INV        DA8W
078600991026IF  1C     G08ERR        IFEQ      '1'
078700991026     C                   MOVEL     '1'           ERRL00
078800991026E   1C                   ENDIF
078900991026     C*
079000991026     C                   ENDSR
079100970526     C*------------------------------------------------------------------------*
079200970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
079300970526     C*------------------------------------------------------------------------*
079400970526     C     IREVID        BEGSR
079500970526     C* CAMPI
079600070122     C                   Z-ADD     grgdde        G08INV
079700070122     C                   Z-ADD     *ZEROS        G08DAT
079800070122     C                   MOVEL     '3'           G08ERR
079900070122     C                   CALL      'XSRDA8'
080000070122     C                   PARM                    WLBDA8
080100070122     C                   Z-ADD     G08DAT        viddde
080200070122     C                   Z-ADD     G08INV        wviddde
080300070122     C*
080400070124     C                   Z-ADD     grgdscad      G08INV
080500070122     C                   Z-ADD     *ZEROS        G08DAT
080600070122     C                   MOVEL     '3'           G08ERR
080700070122     C                   CALL      'XSRDA8'
080800070122     C                   PARM                    WLBDA8
080900070124     C                   Z-ADD     G08DAT        viddscad
081000070124     C                   Z-ADD     G08INV        wviddscad
081100070122     C*
081200070122     C                   MOVE(P)   grgfgs        vidfgs
081300070123     C                   MOVEL     grgcgi        vidcgi
081400070122     C                   MOVEL     grgain        vidain
081500070122     C                   MOVEL     grgdes        viddes
081600070209     C                   MOVEL     grgflo        DGRGFLO
081700070209     C                   MOVEL     §GRGFIT       vidfit
081800070320     C                   MOVEL     §GRGDST       viddst
081900070320     C                   MOVEL     §GRGEDP       videdp
082000070621     C                   MOVEL     §GRGTUG       vidtug
082100110624     C                   MOVEL     §GRGcsto      vidcsto
082200120402     C                   MOVEL     §GRGfgm       vidfgm
082300070122     C                   MOVEL     grgact        vidact
082400070122     C                   MOVEL     grgute        vidute
082500070122     C*
082600070122     C                   Z-ADD     grgduv        G08INV
082700070122     C                   Z-ADD     *ZEROS        G08DAT
082800070122     C                   MOVEL     '3'           G08ERR
082900070122     C                   CALL      'XSRDA8'
083000070122     C                   PARM                    WLBDA8
083100070122     C                   Z-ADD     G08DAT        vidduv
083200070307     C*
083300070307     C* SE TRATTASI D GIRO ANNULLATO => EVIDENZIO SUBITO
083400070307     C                   IF        grgATB = 'A'
083500070307     C                   SETON                                        76
083600070320     C                   SETON                                        91
083700070307     C                   ENDIF
083800051122     C*
083900051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
084000070123     C                   IF        DEPOP1 = 'F06'
084100070201     C***                EVAL      viddde   = DATCORINV
084200070404     C***                EVAL      viddde   = 01012007
084300070404     C***                EVAL      viddscad = 31122039
084400051122     C                   ENDIF
084500070718     C*
084600070718     C* SE TRATTASI D GIRO IN GESTIONE SOLO UTENTI EDP* E UTENTE CORRENTE NO EDP* => SOLO VISUAL.
084700070718     C                   IF        *IN94  = *off AND
084800070718     C                             videdp = 'S'
084900070718     C                   SETON                                        90
085000070718     C                   SETON                                        91
085100070718     C                   ENDIF
085200020218     C*
085300970526     C                   ENDSR
085400970526     C*------------------------------------------------------------------------*
085500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
085600970526     C*------------------------------------------------------------------------*
085700970526     C     AZZVID        BEGSR
085800970526     C* CAMPI
085900070122     C                   MOVEA     *IN           wSavIn           99
086000070122     C                   CLEAR                   DG10D03
086100070122     C                   MOVEA     wSavIn        *IN
086200070709     C                   EVAL      VIDTUG  = 'E'
086300970526     C*
086400970526     C                   ENDSR
086500970526     C*------------------------------------------------------------------------*
086600070122     C* IMPGRG - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
086700970526     C*------------------------------------------------------------------------*
086800070122     C     IMPGRG        BEGSR
086900970526     C*
087000070122     C                   Z-ADD     viddde        G08dat
087100070122     C                   Z-ADD     *ZEROS        G08inv
087200070122     C                   MOVEL     '0'           G08ERR
087300070122     C                   CALL      'XSRDA8'
087400070122     C                   PARM                    WLBDA8
087500070122     C                   Z-ADD     G08inv        grgdde
087600070122     C*
087700070124     C                   z-add     viddscad      G08dat
087800070122     C                   Z-ADD     *ZEROS        G08inv
087900070122     C                   MOVEL     '0'           G08ERR
088000070122     C                   CALL      'XSRDA8'
088100070122     C                   PARM                    WLBDA8
088200070124     C                   Z-ADD     G08inv        grgdscad
088300070122     C*
088400070122     C                   move(p)   vidfgs        grgfgs
088500070123     C                   movel     vidcgi        grgcgi
088600070122     C                   movel     vidain        grgain
088700070209     C                   movel     viddes        grgdes
088800110624     C                   movel     vidcsto       §GRGcsto
088900120402     C                   movel     vidfgm        §GRGfgm
089000110624     C                   movel     vidfit        §GRGFIT
089100070320     C                   movel     viddst        §GRGDST
089200070320     C                   movel     videdp        §GRGEDP
089300070621     C                   movel     vidtug        §GRGTUG
089400070209     C                   movel     DGRGFLO       grgflo
089500070122     C                   z-add     datcor        grgduv
089600070122     C                   movel     knmus         grgute
089700970526     C*
089800970526     C                   ENDSR
089900970526     C*------------------------------------------------------------------------*
090000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
090100970526     C*------------------------------------------------------------------------*
090200970526     C     IMPFUN        BEGSR
090300970526     C*
090400970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
090500970526IF  1C     DEPOP1        IFEQ      'O05'
090600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
090700970526     C                   MOVEL     'N'           FUDATT
090800970526     C                   MOVEL     FUDRIG        FUD(6)
090900970526E   1C                   ENDIF
091000070404     C*
091100070404     C* SE IN INSERIMENTO ABILITA LA FUNZIONE DI RICERCA VERSIONI
091200070404IF  1C     DEPOP1        IFEQ      'F06'
091300070404     C                   MOVEL     FUD(10)       FUDRIG                         *AGGIORNAMENTO
091400070404     C                   MOVEL     'S'           FUDATT
091500070406     C                   MOVEL     FUDRIG        FUD(10)
091600070404E   1C                   ENDIF
091700970526     C*
091800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
091900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
092000970526     C                   EXSR      FUNGES
092100970526     C*
092200970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
092300970526     C*---
092400070122     C* 90 -> VISUALIZZAZIONE
092500070122     C* 91 -> MODIFICA
092600070122     C* 92 -> INSERIMENTO
092700970526     C*---
092800070123     C                   SETON                                        90
092900070123     C                   SETON                                        91
093000070123     C                   SETOFF                                       92
093100070122IF  1C     DEPOP0        IFEQ      'P03'                                        GESTIONE
093200070122     C     DEPOP0        OREQ      'P04'                                        GESTIONE FULL
093300970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
093400970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
093500970526     C                   SETOFF                                       91
093600070122     C                   SETON                                        92
093700070404     C                   SETOFF                                       93
093800970526E   2C                   ENDIF
093900970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
094000970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
094100970526E   2C                   ENDIF
094200970526E   1C                   ENDIF
094300970526     C*
094400970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
094500970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
094600070124     C                   Z-ADD     D09idde       K_grgdde
094700070124     C                   Z-ADD     D09ifgs       K_grgfgs
094800070124     C                   MOVEL     D09icgi       K_grgcgi
094900070122     C     KEYgrg01      CHAIN     figrg01l
095000070122IF  2C                   IF        not %found(figrg01l)                         *NON TROVATO
095100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
095200070122     C                   MOVEL     MSG(6)        DSMSMS
095300970526X   2C                   ELSE
095400970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
095500980128IF  3C     DEPOP1        IFEQ      'O05'
095600070122IF  3C     DEPOP1        OREQ      'O02'
095700980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
095800980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
095900980128E   3C                   ENDIF
096000970526E   2C                   ENDIF
096100970526X   1C                   ELSE                                                   *INSERIMENTO
096200970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
096300970526E   1C                   ENDIF
096400970526     C*
096500970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
096600970526IF  1C     DEPOP1        IFEQ      'O02'
096700970526     C                   MOVEL     DEC(1)        VIDFUN
096800970526E   1C                   ENDIF
096900991014IF  1C     DEPOP1        IFEQ      'O03'
097000991014     C                   MOVEL     DEC(6)        VIDFUN
097100991014E   1C                   ENDIF
097200970526IF  1C     DEPOP1        IFEQ      'O04'
097300970526     C                   MOVEL     DEC(2)        VIDFUN
097400970526E   1C                   ENDIF
097500970526IF  1C     DEPOP1        IFEQ      'O05'
097600970526     C                   MOVEL     DEC(3)        VIDFUN
097700970526E   1C                   ENDIF
097800970526IF  1C     DEPOP1        IFEQ      'O07'
097900970526     C                   MOVEL     DEC(4)        VIDFUN
098000970526E   1C                   ENDIF
098100970526IF  1C     DEPOP1        IFEQ      'F06'
098200970526     C                   MOVEL     DEC(5)        VIDFUN
098300970526E   1C                   ENDIF
098400970526     C*
098500970526     C                   ENDSR
098600070122     C*--------------------------------------------------------------------------------------------*
098700070122     C* REPERISCE I DATI UTENTE
098800070122     C*--------------------------------------------------------------------------------------------*
098900070122     C     REPDATIUTE    BEGSR
099000070122     C*
099100070122     C* INIZIALIZZA VARIABILI DI WRK
099200070122     C                   CLEAR                   TIBS34DS
099300070122     C                   CLEAR                   AZUTEDS
099400070122     C                   CLEAR                   DDATIUTE
099500070122     C*
099600070122     C     *DTAARA       DEFINE    §azute        azuteds
099700070122     C     *DTAARA       DEFINE    §datiute      ddatiute
099800070122     C                   IN(E)     *DTAARA
099900070122if  1C                   IF        %Error
100000070122     c                   EVAL      I34Tla = 'L'
100100070122     C                   CALL      'TIBS34R'
100200070122     C                   PARM                    Tibs34Ds
100300070122     C                   IN        *DTAARA
100400070122e   1C                   ENDIF
100500070122     C*
100600070122     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
100700070122     C                   MOVEL(P)  RSUT          DSFIRS           20
100800070122     C*
100900070122     C                   ENDSR
101000070403     C*------------------------------------------------------------------------*
101100070403     C* CNTVER - Conta le versiono aperte di una filiale
101200070403     C*------------------------------------------------------------------------*
101300070403     C     CNTVER        begsr
101400070403     C*
101500070403     C                   z-add     *zeros        wCntVer           3 0
101600070403     C     wvidfgs       setll     figrv01l
101700070403     C                   if        %found(figrv01l)
101800070403     C                   dow       not %eof(figrv01l)
101900070403     C                   read      figrv01l
102000070403     C                   if        grvfgs = wvidfgs
102100070403     C                   if        datcor <= grvdscad
102200070403     C                   add       1             wCntVer
102300070403     C                   z-add     grvdde        wTopDdeVer        8 0
102400070403     C                   z-add     grvdscad      wTopDscVer        8 0
102500070403     C                   endif
102600070403     C                   else
102700070403     C                   leave
102800070403     C                   endif
102900070403     C                   enddo
103000070403     C                   endif
103100070403     C*
103200070403     C                   endsr
103300970526     C*------------------------------------------------------------------------*
103400970526     C* *INZSR - OPERAZIONI INIZIALI
103500970526     C*------------------------------------------------------------------------*
103600970526     C     *INZSR        BEGSR
103700070426     C*
103800070426     C                   movea     charOK        SKcharOK
103900970526     C*---
104000970526     C* RICEVIMENTO PARAMETRI
104100970526     C*---
104200970526     C     *ENTRY        PLIST
104300970612     C                   PARM                    KPJBA
104400070122     C                   MOVEL(P)  KPJBU         fidg09ds
104500070320     C*
104600070320     C* VERIFICO SUBITO SE TRATTASI D UTENTE EDP*
104700070320     C                   IF        %subst(KNMUS:1:3) = 'EDP'
104800070320     C                   SETON                                        94
104900070320     C                   ELSE
105000070320     C                   SETOFF                                       94
105100070320     C                   ENDIF
105200170329      * imposto sempre tipo utilizzo giro = E-ENTRAMBI
105300170329     c                   eval      vidtug = 'E'
105400070122     C*
105500070122     C* REPERIMENTO DATI UTENTE
105600070122     C                   EXSR      REPDATIUTE
105700970526     C*
105800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
105900970526     C     XMSGPA        PLIST
106000970526     C                   PARM                    DSMSMS
106100970526     C                   PARM                    DSMSPG
106200970526     C                   PARM                    DSMSKE
106300970526     C                   PARM                    DSMSER
106400991108     C*---
106500970526     C*
106600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
106700970526     C                   CALL      'XKEY'
106800970526     C                   PARM                    DSKEY
106900970526     C*---
107000970526     C* VARIABILI NON RIFERITE AL DATA BASE
107100970526     C*---
107200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
107300070124     C     *LIKE         DEFINE    D09IOP0       DEPOP0
107400070124     C     *LIKE         DEFINE    D09IOP1       DEPOP1
107500970526     C*---
107600070122     C* VARIABILI DI CONTROLLO
107700970526     C*---
107800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
107900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
108000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
108100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
108200970526     C*
108300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
108400070122     C                   MOVEL     'FIDG10R'     DSMSPG
108500070122     C                   MOVEL     'FIDG10R'     MSGQUE
108600970526     C                   MOVEL     *BLANKS       MSGKEY
108700970526     C                   MOVEL     *BLANKS       DSMSMS
108800970526     C*---
108900970526     C* VARIABILI RIFERITE AL DATA BASE
109000970526     C*---
109100070123     C     *LIKE         DEFINE    grgcgi        K_grgcgi
109200070123     C     *LIKE         DEFINE    grgdde        K_grgdde
109300070123     C     *LIKE         DEFINE    grgfgs        K_grgfgs
109400070403     C     *LIKE         DEFINE    grvfgs        K_grvfgs
109500070403     C     *LIKE         DEFINE    grvdde        K_grvdde
109600970526     C*---
109700970526     C* CHIAVI DI LETTURA
109800970526     C*---
109900070122     C* LETTURA figrg01l - COMPLETA
110000070122     C     KEYgrg01      KLIST
110100070122     C                   KFLD                    K_grgdde
110200070123     C                   KFLD                    K_grgfgs
110300070123     C                   KFLD                    K_grgcgi
110400070406     C* LETTURA figrv01l - COMPLETA
110500070406     C     KEYgrv01_c    KLIST
110600070403     C                   KFLD                    K_grvfgs
110700070403     C                   KFLD                    K_grvdde
110800970528     C*
110900970528     C* CALCOLA LA DATA CORRENTE
111000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
111100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
111200970528     C                   Z-ADD     WN8           G08DAT
111300051122     C                   Z-ADD     WN8           DATCORINV         8 0
111400970528     C                   Z-ADD     *ZEROS        G08INV
111500970528     C                   MOVEL     '0'           G08ERR
111600970528     C                   CALL      'XSRDA8'
111700970528     C                   PARM                    WLBDA8
111800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
111900970526     C*
112000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
112100070124     C                   MOVEL     D09IOP0       DEPOP0                         *LIVELLO PROCEDURA
112200070124     C                   MOVEL     D09IOP1       DEPOP1                         *LIVELLO PROGRAMMA
112300970526     C*
112400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
112500970526     C                   EXSR      IMPFUN
112600970526     C*
112700970526     C                   ENDSR
112800970526     C*------------------------------------------------------------------------*
112900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
113000970526     C*------------------------------------------------------------------------*
113100970526     C     FUNGES        BEGSR
113200970526     C*
113300970526     C* PULISCO CAMPI DI LAVORO
113400970526     C                   Z-ADD     0             W                 5 0
113500970526     C*
113600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
113700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
113800970526     C     1             DO        24            I
113900970526     C                   MOVEL     FUD(I)        FUDRIG
114000970526     C*
114100970526     C* PULIZIA FUNZIONI PRECEDENTI
114200970526     C                   MOVEL     *BLANKS       FUV(I)
114300970526     C*
114400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
114500970526     C* (SALTA F24)
114600970526     C     FUDATT        IFEQ      'S'
114700970526     C     FUDFUN        ANDNE     '24'
114800970526     C                   ADD       1             W
114900970526     C                   MOVEL     FUDDES        FUV(W)
115000970526     C                   END
115100970526     C                   END                                                    DO WHILE
115200970526     C*
115300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
115400970526     C* ALTRIMENTI DISATTIVO F24
115500970526     C     1             DO        24            I
115600970526     C                   MOVEL     FUD(I)        FUDRIG
115700970526     C*
115800970526     C     FUDFUN        IFEQ      '24'
115900970526     C     W             IFGT      6
116000970526     C                   MOVEL     'S'           FUDATT
116100970526     C                   ELSE
116200970526     C                   MOVEL     *BLANKS       FUDATT
116300970526     C                   END                                                    W GT 8
116400970526     C                   MOVEL     FUDRIG        FUD(I)
116500970526     C                   END
116600970526     C                   END                                                    DO WHILE
116700970526     C*
116800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
116900970526     C     W             DIV       6             I
117000970526     C                   MVR                     Z
117100970526     C*
117200970526     C     Z             IFNE      0
117300970526     C     I             ADD       1             Z
117400970526     C                   ELSE
117500970526     C                   Z-ADD     I             Z                 5 0
117600970526     C                   END
117700970526     C*
117800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
117900970526     C                   ADD       1             FUNGRU
118000970526     C     FUNGRU        IFGT      Z
118100970526     C     FUNGRU        ORLT      1
118200970526     C                   Z-ADD     1             FUNGRU
118300970526     C                   END
118400970526     C*
118500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
118600970526     C     FUNGRU        IFEQ      1
118700970526     C                   MOVEL     FUV(1)        FUN001
118800970526     C                   MOVEL     FUV(2)        FUN002
118900970526     C                   MOVEL     FUV(3)        FUN003
119000970526     C                   MOVEL     FUV(4)        FUN004
119100970526     C                   MOVEL     FUV(5)        FUN005
119200970526     C                   MOVEL     FUV(6)        FUN006
119300970526     C                   END
119400970526     C*
119500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
119600970526     C     FUNGRU        IFEQ      2
119700970526     C                   MOVEL     FUV(7)        FUN001
119800970526     C                   MOVEL     FUV(8)        FUN002
119900970526     C                   MOVEL     FUV(9)        FUN003
120000970526     C                   MOVEL     FUV(10)       FUN004
120100970526     C                   MOVEL     FUV(11)       FUN005
120200970526     C                   MOVEL     FUV(12)       FUN006
120300970526     C                   END
120400970526     C*
120500970526     C* TERZO GRUPPO DI 6 FUNZIONI
120600970526     C     FUNGRU        IFEQ      3
120700970526     C                   MOVEL     FUV(13)       FUN001
120800970526     C                   MOVEL     FUV(14)       FUN002
120900970526     C                   MOVEL     FUV(15)       FUN003
121000970526     C                   MOVEL     FUV(16)       FUN004
121100970526     C                   MOVEL     FUV(17)       FUN005
121200970526     C                   MOVEL     FUV(18)       FUN006
121300970526     C                   END
121400970526     C*
121500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
121600970526     C     FUNGRU        IFEQ      4
121700970526     C                   MOVEL     FUV(19)       FUN001
121800970526     C                   MOVEL     FUV(20)       FUN002
121900970526     C                   MOVEL     FUV(21)       FUN003
122000970526     C                   MOVEL     FUV(22)       FUN004
122100970526     C                   MOVEL     *BLANKS       FUN005
122200970526     C                   MOVEL     *BLANKS       FUN006
122300970526     C                   END
122400970526     C*
122500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
122600970526     C                   MOVEL     *BLANKS       FUN007
122700970526     C                   MOVEL     *BLANKS       FUN008
122800970526     C*
122900970526     C     1             DO        24            I
123000970526     C                   MOVEL     FUD(I)        FUDRIG
123100970526     C     FUDFUN        IFEQ      '24'
123200970526     C     FUDATT        ANDEQ     'S'
123300970526     C                   MOVEL     FUDDES        FUN008
123400970526     C                   END
123500970526     C                   END                                                    DO
123600970526     C*
123700970526     C                   ENDSR
123800970526     C*------------------------------------------------------------------------*
123900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
124000970526     C*------------------------------------------------------------------------*
124100970526     C     FUNCON        BEGSR
124200970526     C*
124300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
124400970526     C                   EXSR      FUNVAR
124500970526     C*
124600970526     C* PULISCO CAMPI DI LAVORO
124700970526     C                   Z-ADD     0             I                 5 0
124800970526     C                   Z-ADD     0             W                 5 0
124900970526     C*
125000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
125100970526     C     FUNKEY        IFNE      *BLANKS
125200970526     C     1             DO        24            I
125300970526     C                   MOVEL     FUD(I)        FUDRIG
125400970526     C*
125500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
125600970526     C     FUNKEY        IFEQ      FUDFUN
125700970526     C     FUDATT        ANDEQ     'S'
125800970526     C                   Z-ADD     I             W
125900970526     C                   END
126000970526     C                   END                                                    DO WHILE
126100970526     C*
126200970526     C* FUNZIONE NON ATTIVA (ERRORE)
126300970526     C     W             IFEQ      0
126400970526     C*
126500970526     C* EMISSIONE MESSAGGIO DI ERRORE
126600970526     C                   MOVEL     MSG(1)        DSMSMS
126700970526     C*
126800970526     C                   END                                                    W=0
126900970526     C                   END                                                    FUNKEY NE *BLANKS
127000970526     C*
127100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
127200970526     C     W             IFNE      0
127300970526     C     FUNKEY        OREQ      *BLANKS
127400970526     C     FUNKEY        IFEQ      *BLANKS
127500970526     C                   EXSR      ENTGES
127600970526     C                   END
127700970526     C     FUNKEY        IFEQ      '03'
127800970526     C                   EXSR      F03GES
127900970526     C                   END
128000970526     C     FUNKEY        IFEQ      '06'
128100970526     C                   EXSR      F06GES
128200970526     C                   END
128300070403     C     FUNKEY        IFEQ      '10'
128400070403     C                   EXSR      F10GES
128500070403     C                   END
128600970526     C     FUNKEY        IFEQ      '12'
128700970526     C                   EXSR      F12GES
128800970526     C                   END
128900970526     C     FUNKEY        IFEQ      '24'
129000970526     C                   EXSR      F24GES
129100970526     C                   END
129200970526     C                   END
129300970526     C*
129400970526     C                   ENDSR
129500970526     C*------------------------------------------------------------------------*
129600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
129700970526     C*------------------------------------------------------------------------*
129800970526     C     FUNVAR        BEGSR
129900970526     C*
130000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
130100970526     C*
130200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
130300970526     C     KEY           IFEQ      ENTER
130400970526     C                   MOVEL     *BLANKS       FUNKEY
130500970526     C                   END
130600970526     C     KEY           IFEQ      F1
130700970526     C                   MOVEL     '01'          FUNKEY
130800970526     C                   END
130900970526     C     KEY           IFEQ      F2
131000970526     C                   MOVEL     '02'          FUNKEY
131100970526     C                   END
131200970526     C     KEY           IFEQ      F3
131300970526     C                   MOVEL     '03'          FUNKEY
131400970526     C                   END
131500970526     C     KEY           IFEQ      F4
131600970526     C                   MOVEL     '04'          FUNKEY
131700970526     C                   END
131800970526     C     KEY           IFEQ      F5
131900970526     C                   MOVEL     '05'          FUNKEY
132000970526     C                   END
132100970526     C     KEY           IFEQ      F6
132200970526     C                   MOVEL     '06'          FUNKEY
132300970526     C                   END
132400970526     C     KEY           IFEQ      F7
132500970526     C                   MOVEL     '07'          FUNKEY
132600970526     C                   END
132700970526     C     KEY           IFEQ      F8
132800970526     C                   MOVEL     '08'          FUNKEY
132900970526     C                   END
133000970526     C     KEY           IFEQ      F9
133100970526     C                   MOVEL     '09'          FUNKEY
133200970526     C                   END
133300970526     C     KEY           IFEQ      F10
133400970526     C                   MOVEL     '10'          FUNKEY
133500970526     C                   END
133600970526     C     KEY           IFEQ      F11
133700970526     C                   MOVEL     '11'          FUNKEY
133800970526     C                   END
133900970526     C     KEY           IFEQ      F12
134000970526     C                   MOVEL     '12'          FUNKEY
134100970526     C                   END
134200970526     C     KEY           IFEQ      F13
134300970526     C                   MOVEL     '13'          FUNKEY
134400970526     C                   END
134500970526     C     KEY           IFEQ      F14
134600970526     C                   MOVEL     '14'          FUNKEY
134700970526     C                   END
134800970526     C     KEY           IFEQ      F15
134900970526     C                   MOVEL     '15'          FUNKEY
135000970526     C                   END
135100970526     C     KEY           IFEQ      F16
135200970526     C                   MOVEL     '16'          FUNKEY
135300970526     C                   END
135400970526     C     KEY           IFEQ      F17
135500970526     C                   MOVEL     '17'          FUNKEY
135600970526     C                   END
135700970526     C     KEY           IFEQ      F18
135800970526     C                   MOVEL     '18'          FUNKEY
135900970526     C                   END
136000970526     C     KEY           IFEQ      F19
136100970526     C                   MOVEL     '19'          FUNKEY
136200970526     C                   END
136300970526     C     KEY           IFEQ      F20
136400970526     C                   MOVEL     '20'          FUNKEY
136500970526     C                   END
136600970526     C     KEY           IFEQ      F21
136700970526     C                   MOVEL     '21'          FUNKEY
136800970526     C                   END
136900970526     C     KEY           IFEQ      F22
137000970526     C                   MOVEL     '22'          FUNKEY
137100970526     C                   END
137200970526     C     KEY           IFEQ      F23
137300970526     C                   MOVEL     '23'          FUNKEY
137400970526     C                   END
137500970526     C     KEY           IFEQ      F24
137600970526     C                   MOVEL     '24'          FUNKEY
137700970526     C                   END
137800970526     C     KEY           IFEQ      ROLLUP
137900970526     C                   MOVEL     'RU'          FUNKEY
138000970526     C                   END
138100970526     C     KEY           IFEQ      HELP
138200970526     C                   MOVEL     'HP'          FUNKEY
138300970526     C                   END
138400970526     C                   ENDSR
138500970526     C*------------------------------------------------------------------------*
138600970526     C* NOTE COSTRUZIONE SCHIERE
138700970526     C*------------------------------------------------------------------------*
138800970526     C*
138900970526     C* -MSG-MESSAGGI
139000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
139100970526     C* (NON TOCCARE MSG,1 E MSG,2)
139200970526     C*
139300970526     C* -CMD-COMANDI
139400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
139500970526     C*
139600970526     C* -OPD-OPZIONI
139700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
139800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
139900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
140000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
140100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
140200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
140300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
140400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
140500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
140600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
140700970526     C*
140800970526     C* -FUD-FUNZIONI
140900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
141000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
141100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
141200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
141300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
141400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
141500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
141600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
141700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
141800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
141900970526     O*------------------------------------------------------------------------*
142000970526** DEC - DECODIFICHE
142100970526    MODIFICA                                                      1
142200970526  ANNULLAMENTO                                                    2
142300970526VISUALIZZAZIONE                                                   3
142400970526   RIPRISTINO                                                     4
142500970526  INSERIMENTO                                                     5
142600991014     COPIA                                                        6
142700970526** MSG - MESSAGGI
142800070122FIDG10R- Tasto funzionale non gestito                                          1
142900070122FIDG10R- Inserimento effettuato                                                2
143000070122FIDG10R- Modifica effettuata                                                   3
143100070122FIDG10R- Annullamento effettuato                                               4
143200070122FIDG10R- Ripristino effettuato                                                 5
143300070122FIDG10R- Richiesta funzione per record inesistente                             6
143400070122FIDG10R- Record momentaneamente allocato: riprovare più tardi                  7
143500070122FIDG10R- Richiesta funzione per record esistente                               8
143600070122FIDG10R- Giro già presente con questo codice                                   9
143700070122FIDG10R- Giro già presente con questa descrizione                              10
143800070122FIDG10R- Codice giro fuori convenzione, indicare l'ambito nel codice           11
143900070122FIDG10R- Campo obbligatorio                                                    12
144000070122FIDG10R- Codice filiale inesistente o di categoria V=varia                     13
144100070122FIDG10R- Data errata                                                           14
144200070122FIDG10R- La data fine deve essere maggiore della data inizio                   15
144300070122FIDG10R- Il record non può scadere in un giorno già trascorso                  16
144400070122FIDG10R- Il record immesso si sovrappone ad uno già in decorrenza              17
144500070403FIDG10R- Gestione automatica giri consegna non ancora attiva sulla Filiale     18
144600070403FIDG10R- Nessuna versione attiva alla data decorrenza indicata (F10)           19
144700070403FIDG10R- Data scadenza deve essere inferiore o uguale al:                      20
144800070426FIDG10R- Presenti caratteri non validi, utilizzare solo numeri o lettere.      21
144900070622FIDG10R- I giri di solo ritiro devono essere fittizi (no su mappa)             22
145000080124FIDG10R- Errore modificare tipo utilizzo giro da consegna/entrambi a ritiro    23
145100120402FIDG10R- Flag giro MICRO ammesso solo se ambito SOTTO                          24
145200070122FIDG10R-                                                                       25
145300070122FIDG10R-                                                                       26
145400070122FIDG10R- Valori incompatibili, correggere!                                     27
145500970526** FUD - FUNZIONI
145600070201                   -        (S/N)=N                        funz.01¬
145700970526                   -        (S/N)=N                        funz.02¬
145800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
145900970526                   -        (S/N)=N                        funz.04¬
146000970526                   -        (S/N)=N                        funz.05¬
146100970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
146200970527                   -        (S/N)=N                        funz.07¬
146300970527                   -        (S/N)=N                        funz.08¬
146400970527                   -        (S/N)=N                        funz.09¬
146500070404F10=Ricerca vers.  -        (S/N)=N                        funz.10¬
146600970526                   -        (S/N)=N                        funz.11¬
146700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
146800970526                   -        (S/N)=N                        funz.13¬
146900970526                   -        (S/N)=N                        funz.14¬
147000970526                   -        (S/N)=N                        funz.15¬
147100970526                   -        (S/N)=N                        funz.16¬
147200970526                   -        (S/N)=N                        funz.17¬
147300970526                   -        (S/N)=N                        funz.18¬
147400970526                   -        (S/N)=N                        funz.19¬
147500970526                   -        (S/N)=N                        funz.20¬
147600970526                   -        (S/N)=N                        funz.21¬
147700970526                   -        (S/N)=N                        funz.22¬
147800970526                   -        (S/N)=N                        funz.23¬
147900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
