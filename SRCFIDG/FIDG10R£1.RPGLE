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
008600070703     D*------------
008700070703     D* DS DESCRIZIONI ORGANIGRAMMA
008800070703     D*------------
008900070703     D OG147         E DS
009000970526     D*------------
009100970526     D* ARCHITETTURA
009200970526     D*------------
009300970526     D KPJBA         E DS
009400040816     D*------------
009500070426     D* COSTANTI
009600070426     D*------------
009700070426     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
009800070426     D                                     1234567890 ')
009900070426     D*-------------------
010000070426     D* SCHIERE DI WRK
010100070426     D*-------------------
010200070426     D SKcharOK        S              1    DIM(37)
010300110624     d
010400110624     D DSGRG         E DS                  extname(figrg00F)
010500110624     D oldgrg        E DS                  extname(figrg00F) prefix(O_)
010600110624     D newgrg        E DS                  extname(figrg00F) prefix(N_)
010700970526     C*------------------------------------------------------------------------*
010800970526     C* MAIN LINES
010900970526     C*------------------------------------------------------------------------*
011000970526     C*
011100970526     C* NON CI SONO ERRORI IN ENTRATA
011200970526IF  1C     ERRL00        IFEQ      '0'
011300970526     C*
011400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011800970526     C*
011900970526     C* EMETTE FORMATO VIDEO
012000970526     C                   EXSR      BUTFMR
012100970526     C*
012200970526     C* GESTIONE TASTI FUNZIONALI
012300970526     C                   EXSR      FUNCON
012400970526E   2C                   ENDDO
012500970526     C*
012600970526     C* CI SONO ERRORI IN ENTRATA
012700970527X   1C                   ELSE
012800070124     C                   MOVEL     '1'           D09oerr
012900070124     C                   MOVEL     DSMSMS        D09omsg
013000970526E   1C                   ENDIF
013100070123     C*
013200070123     C* RITORNO PARAMETRI
013300070123     C                   MOVEL(P)  fidg09ds      KPJBU
013400070122     C* FINE PGM
013500970526     C                   SETON                                        LR
013600970526     C*------------------------------------------------------------------------*
013700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013800970526     C*------------------------------------------------------------------------*
013900970526     C     BUTFMR        BEGSR
014000970526     C*
014100970526     C* EMETTE LA TESTATA
014200070122     C                   WRITE     DG10D01
014300970526     C*
014400970526     C* SPEDISCE EVENTUALE MESSAGGIO
014500970526     C                   EXSR      SNDMSG
014600970526     C*
014700970526     C* EMETTE LE FUNZIONI
014800070122     C                   WRITE     DG10D02
014900970526     C*
015000970526     C* EMETTE IL VIDEO
015100070122     C                   EXFMT     DG10D03
015200970526     C*
015300970526     C* REIMPOSTA FLAG DI ERRORE
015400970526     C                   MOVEL     '0'           ERRL00
015500970526     C*
015600970526     C* REIMPOSTA INDICATORI DI ERRORE
015700070122     C                   SETOFF                                       707172
015800070122     C                   SETOFF                                       737475
015900070622     C                   setoff                                       7877
016000970526     C*
016100970526     C                   ENDSR
016200970526     C*------------------------------------------------------------------------*
016300970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016400970526     C*------------------------------------------------------------------------*
016500970526     C     SNDMSG        BEGSR
016600970526     C*
016700970526     C* PULISCE IL SFILE MESSAGGI
016800970526     C                   MOVEL     '1'           *IN40
016900070122     C                   WRITE     DG10DMC
017000970526     C                   MOVEL     '0'           *IN40
017100970526     C*
017200970526     C* VISUALIZZA IL MESSAGGIO
017300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017400970526     C                   CALL      'XMSG'        XMSGPA
017500970526IF  1C     DSMSER        IFEQ      *ZEROS
017600970526     C                   MOVEL     DSMSKE        MSGKEY
017700970526E   1C                   ENDIF
017800970526     C                   Z-ADD     1             MSGULT
017900070122     C                   WRITE     DG10DMS
018000070122     C                   WRITE     DG10DMC
018100970526     C*
018200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018300970526     C                   MOVEL     *BLANKS       DSMSMS
018400970526     C*
018500970526     C                   ENDSR
018600970526     C*------------------------------------------------------------------------*
018700970526     C* ENTGES - GESTIONE ENTER -> INVIO
018800970526     C*------------------------------------------------------------------------*
018900970526     C     ENTGES        BEGSR
019000970526     C*
019100970526     C* CONTROLLA I CAMPI IMMESSI
019200970526     C                   EXSR      DECODI
019300970526     C*
019400970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
019500970526IF  1C     DEPOP1        IFEQ      'O05'
019600970526     C                   MOVEL     '1'           FINL00
019700970526E   1C                   ENDIF
019800970526     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100970526     C* F03GES - GESTIONE F03 -> FINE
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F03GES        BEGSR
020400970526     C*
020500970526     C* IMPOSTA "PREMUTO F03"
020600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020700070124     C                   MOVEL     '1'           D09OF03                        *DI PROCEDURA
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F06GES        BEGSR
021400970526     C*
021500970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
021600970526IF  1C     DEPOP1        IFEQ      'F06'
021700970526     C                   EXSR      INSERI
021800970526E   1C                   ENDIF
021900970526IF  1C     DEPOP1        IFEQ      'O02'
022000970526     C                   EXSR      MODIFI
022100970526E   1C                   ENDIF
022200991014IF  1C     DEPOP1        IFEQ      'O03'
022300011129     C                   EXSR      COPIA
022400991014E   1C                   ENDIF
022500070123IF  1C     DEPOP1        IFEQ      'O04'
022600070123     C                   EXSR      ANNULL
022700070123E   1C                   ENDIF
022800970526IF  1C     DEPOP1        IFEQ      'O07'
022900970526     C                   EXSR      RIPRIS
023000970526E   1C                   ENDIF
023100011129     C*
023200970526     C                   ENDSR
023300070403     C*------------------------------------------------------------------------*
023400070403     C* F10GES - GESTIONE F10 -> RICERCA VERSIONI
023500070403     C*------------------------------------------------------------------------*
023600070403     C     F10GES        BEGSR
023700070403     C*
023800070403     C                   clear                   fidg11ds
023900070403     C                   eval      d11iop0 = 'P01'
024000070423     C                   if        VIDFGS  > *zeros
024100070423     C                   move(p)   VIDFGS        d11ifgs
024200070423     C                   endif
024300070403     C                   z-add     datcor        d11idda
024400070403     C                   eval      kpjbu = fidg11ds
024500070403     C                   call      'FIDG11R'
024600070403     C                   parm                    kpjba
024700070403     C                   eval      fidg11ds = kpjbu
024800070403     C                   if        d11oerr  =  *blanks AND
024900070403     C                             d11odde  <> *zeros
025000070403     C                   eval      VIDDDE   =  d11odde
025100070406     C                   if        VIDDSCAD = *zeros
025200070403     C                   eval      VIDDSCAD =  d11odsc
025300070406     C                   endif
025400070403     C                   endif
025500070403     C*
025600070403     C                   ENDSR
025700970526     C*------------------------------------------------------------------------*
025800970526     C* F12GES - GESTIONE F12 -> RITORNO
025900970526     C*------------------------------------------------------------------------*
026000970526     C     F12GES        BEGSR
026100970526     C*
026200970526     C* IMPOSTA "PREMUTO F12"
026300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026400070124     C                   MOVEL     '1'           D09OF12                        *DI PROCEDURA
026500970526     C*
026600970526     C                   ENDSR
026700970526     C*------------------------------------------------------------------------*
026800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026900970526     C*------------------------------------------------------------------------*
027000970526     C     F24GES        BEGSR
027100970526     C*
027200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
027300970526     C*
027400970526     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
027700970526     C*------------------------------------------------------------------------*
027800970526     C     INSERI        BEGSR
027900970526     C*
028000970526     C* CONTROLLA I CAMPI IMMESSI
028100970526     C                   EXSR      DECODI
028200970526     C*
028300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028400991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
028500070122     C                   clear                   figrg000
028600070122     C                   exsr      impgrg
028700070614     C                   if        vidFIT <> 'S'
028800070614     C                   eval      grgFTT =  'S'
028900070614     C                   endif
029000070122     C                   write     figrg000
029100070124     C                   movel     MSG(2)        D09OMSG                        *MESSAGGIO
029200970602     C*
029300970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
029400070122     C                   exsr      azzvid
029500070122E   1C                   endif
029600970526     C*
029700970526     C                   ENDSR
029800970526     C*------------------------------------------------------------------------*
029900970526     C* MODIFI - OPERAZIONI DI MODIFICA
030000970526     C*------------------------------------------------------------------------*
030100970526     C     MODIFI        BEGSR
030200970526     C*
030300970526     C* CONTROLLA I CAMPI IMMESSI
030400970526     C                   EXSR      DECODI
030500970526     C*
030600970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030800070122     C                   EXSR      LETgrg
030900110624     c                   movel     dsgrg         oldgrg
031000110624     c* clearo flag trasmissione e falg centro storico
031100110624     c*  che non mi servono per decidere se scaricare a Tellus
031200110624     c                   clear                   o_grgftt
031300110624     c                   clear                   o_grgute
031400110624     c                   clear                   o_grgduv
031500110624     C                   movel     o_grgflo      DGRGFLO
031600110624     c                   clear                   §grgcsto
031700110624     c                   movel     dgrgflo       o_grgflo
031800970526     C*
031900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
032000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032100070122     C                   EXSR      impgrg                                       *IMPOSTA I CAMPI
032200110624     c
032300070614     C                   if        vidFIT <> 'S'
032400110624     c* se ho modificato un dato sensibile allo scarico --> flaggo
032500110624     c                   movel     dsgrg         newgrg
032600110624     c* clearo flag trasmissione e falg centro storico
032700110624     c*  che non mi servono per decidere se scaricare a Tellus
032800110624     c                   clear                   n_grgftt
032900110624     c                   clear                   n_grgute
033000110624     c                   clear                   n_grgduv
033100110624     C                   movel     n_grgflo      DGRGFLO
033200110624     c                   clear                   §grgcsto
033300110624     c                   movel     dgrgflo       n_grgflo
033400110624     c
033500110624     c                   if        oldgrg<>newgrg
033600070614     C                   eval      grgFTT =  'S'
033700070614     C                   endif
033800110624     c                   else
033900110624     c                   clear                   grgftt
034000110624     C                   endif
034100110624     c
034200070122     C                   UPDATE    figrg000
034300070124     C                   MOVEL     MSG(3)        D09OMSG
034400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
034500970526E   2C                   ENDIF
034600970526E   1C                   ENDIF
034700970526     C*
034800970526     C                   ENDSR
034900991014     C*------------------------------------------------------------------------*
035000070122     C* COPIA  - OPERAZIONI DI COPIA
035100991014     C*------------------------------------------------------------------------*
035200070122     C     COPIA         BEGSR
035300991014     C*
035400991014     C*
035500991014     C                   ENDSR
035600970526     C*------------------------------------------------------------------------*
035700970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
035800970526     C*------------------------------------------------------------------------*
035900070122     C     ANNULL        BEGSR
036000991025     C*
036100991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
036200070122     C                   EXSR      LETgrg
036300991025     C*
036400991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
036500070122IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036600070122     C                   EVAL      grgATB = 'A'
036700070614     C                   if        vidFIT <> 'S'
036800070614     C                   eval      grgFTT =  'S'
036900070614     C                   endif
037000070122     C                   UPDATE    figrg000                                     *CANCELLA
037100070124     C                   MOVEL     MSG(4)        D09OMSG                        *MESSAGGIO
037200070122     C                   MOVEL     '1'           FINL00                         *FINE PGM
037300070122E   1C                   ENDIF
037400970526     C*
037500070122     C                   ENDSR
037600970526     C*------------------------------------------------------------------------*
037700970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
037800970526     C*------------------------------------------------------------------------*
037900970526     C     RIPRIS        BEGSR
038000070122     C*
038100070122     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
038200070122     C                   EXSR      LETgrg
038300070122     C*
038400070122     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
038500070122IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038600070122     C                   EVAL      grgATB = *blanks
038700070614     C                   if        vidFIT <> 'S'
038800070614     C                   eval      grgFTT =  'S'
038900070614     C                   endif
039000070122     C                   UPDATE    figrg000                                     *CANCELLA
039100070124     C                   MOVEL     MSG(5)        D09OMSG                        *MESSAGGIO
039200070122     C                   MOVEL     '1'           FINL00                         *FINE PGM
039300070122E   1C                   ENDIF
039400070122     C*
039500970526     C                   ENDSR
039600970526     C*------------------------------------------------------------------------*
039700070122     C* LETgrg - LEGGE IL RECORD PERL'AGGIORNAMENTO
039800970526     C*------------------------------------------------------------------------*
039900070122     C     LETgrg        BEGSR
040000970526     C*
040100070206     C                   IF        DEPOP1 <> 'F06'
040200070124     C                   Z-ADD     D09idde       K_grgdde
040300070124     C                   Z-ADD     D09ifgs       K_grgfgs
040400070124     C                   MOVEL     D09icgi       K_grgcgi
040500070206     C                   ENDIF
040600070122     C     KEYgrg01      CHAIN     figrg01l                           8081
040700070122IF  1C     *IN81         IFEQ      *ON                                          *ALLOCATO
040800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
040900070124     C                   MOVEL     MSG(7)        D09OMSG                        *MESSAGGIO
041000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
041100970526E   1C                   ENDIF
041200070122IF  1C     *IN80         IFEQ      *ON                                          *INSESISTENTE
041300070122     C                   IF        DEPOP1 <> 'F06'
041400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
041500070124     C                   MOVEL     MSG(6)        D09OMSG                        *MESSAGGIO
041600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
041700070122     C                   ENDIF
041800970526E   1C                   ENDIF
041900970526     C*
042000970526     C                   ENDSR
042100970526     C*------------------------------------------------------------------------*
042200970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
042300970526     C*------------------------------------------------------------------------*
042400970526     C     DECODI        BEGSR
042500001120     C*
042600070403     C*--------------------
042700070403     C* CONTROLLO FILIALE
042800070403     C*--------------------
042900070403     C     ERRL00        IFEQ      '0'
043000070403     C     vidfgs        IFNE      *blanks
043100070403     C     vidfgs        ANDNE     *zeros
043200070403     C                   EXSR      cntpo
043300070403     C                   ELSE
043400070403     C                   MOVEL     '1'           ERRL00
043500070403     C                   MOVEL     MSG(12)       DSMSMS
043600070403     C                   SETON                                        72
043700070403     C                   ENDIF
043800070403     C                   ENDIF
043900070403     C*--------------------
044000070403     C* CONTROLLO ABILITAZIONE UTENTE ALLA FILIALE
044100070403     C*--------------------
044200070403     C     ERRL00        IFEQ      '0'
044300070403     C                   IF        VIDFGS <> *BLANKS AND
044400070403     C                             VIDFGS <> *ZEROS  AND
044500070403     C                             *IN94 = *OFF
044600070403     C                   CLEAR                   FNLV50DS
044700070403     C                   EVAL      D50TLA = 'L'
044800070403     C                   EVAL      D50PRU = KNMUS
044900070403     C                   EVAL      D50FGS = wvidfgs
045000070403     C                   CALL      'FNLV50R'
045100070403     C                   PARM                    FNLV50DS
045200070403     C                   IF        D50ERR <> *BLANKS
045300070403     C                   MOVEL     '1'           ERRL00
045400070403     C                   MOVEL     D50MSG        DSMSMS
045500070403     C                   SETON                                        72
045600070403     C                   ENDIF
045700070403     C                   ENDIF
045800070403     C                   ENDIF
045900070403     C*--------------------
046000070403     C* CONTROLLO FILIALE ATTIVATA A "TELLUS"
046100070403     C*--------------------
046200070403     C     ERRL00        IFEQ      '0'
046300070403     C                   IF        VIDFGS <> *BLANKS AND
046400070403     C                             VIDFGS <> *ZEROS  AND
046500070403     C                             *IN94 = *OFF
046600070403     C     wvidfgs       CHAIN     azorg01l
046700070403     C                   IF        %found(azorg01l)
046800070403     C                   EVAL      OG148 = orgde8
046900070703     C                   EVAL      OG147 = orgde7
047000070403     C                   ELSE
047100070403     C                   CLEAR                   OG148
047200070703     C                   CLEAR                   OG147
047300070403     C                   ENDIF
047400070403     C*
047500070403     C                   IF        §OGTLLS = *BLANKS
047600070403     C                   MOVEL     '1'           ERRL00
047700070403     C                   MOVEL     MSG(18)       DSMSMS
047800070403     C                   SETON                                        72
047900070403     C                   ENDIF
048000070403     C                   ENDIF
048100070403     C                   ENDIF
048200070703      * verifico se è stata attivata la gestione giro ritiro accendo 95
048300070703      * flag di attivazione su OG174 e utente EDP
048400070703     C                   IF        %subst(KNMUS:1:3) = 'EDP'
048500070703     c                             or §Ogcgio = 'S'
048600070703     C                   SETON                                        95
048700070703     C                   ELSE
048800070703     C                   SETOFF                                       95
048900070703     C                   ENDIF
049000070403     C*--------------------
049100070404     C* VERIFICA VERSIONI APERTE X LA FILIALE CORRENTE (SOLO IN INSERIMENTO)
049200070403     C*--------------------
049300070403     C     ERRL00        IFEQ      '0'
049400070404     C                   IF        DEPOP1 = 'F06'
049500070403     C                   EXSR      CNTVER
049600070403     C                   IF        wCntVer = *zeros
049700070403     C                   MOVEL     '1'           ERRL00
049800070403     C                   SETON                                        70
049900070403     C                   MOVEL     MSG(19)       DSMSMS
050000070403     C                   ENDIF
050100070403     C*
050200070403     C                   IF        wCntVer = 1
050300070403     C                   Z-ADD     wTopDdeVer    G08INV
050400070403     C                   Z-ADD     *ZEROS        G08DAT
050500070403     C                   MOVEL     '3'           G08ERR
050600070403     C                   CALL      'XSRDA8'
050700070403     C                   PARM                    WLBDA8
050800070403     C                   Z-ADD     G08DAT        viddde
050900070403     C                   Z-ADD     G08INV        wviddde
051000070420     C                   Z-ADD     wTopDscVer    G08INV
051100070420     C                   Z-ADD     *ZEROS        G08DAT
051200070420     C                   MOVEL     '3'           G08ERR
051300070420     C                   CALL      'XSRDA8'
051400070420     C                   PARM                    WLBDA8
051500070420     C                   Z-ADD     G08DAT        viddscad
051600070420     C                   Z-ADD     G08INV        wviddscad
051700070420     C                   SETON                                        93
051800070403     C                   ENDIF
051900070403     C*
052000070406     C                   IF        wCntVer > 1      AND
052100070406     C                             viddde  = *zeros
052200070403     C                   EXSR      F10GES
052300070404     C                   ENDIF
052400070403     C                   ENDIF
052500070403     C                   ENDIF
052600991026     C*--------------------
052700070403     C* DATA DECORRENZA
052800991026     C*--------------------
052900070122     C     *LIKE         DEFINE    viddde        wviddde
053000001120     C     ERRL00        IFEQ      '0'
053100070122     C                   Z-ADD     viddde        wda8
053200991026     C                   Z-ADD     *ZEROS        da8w
053300991026     C                   EXSR      CHKDA8
053400070122     C                   Z-ADD     WDA8          viddde
053500070122     C                   Z-ADD     DA8W          wviddde
053600001120     C     ERRL00        IFEQ      '1'
053700070122     C                   SETON                                        70
053800070122     C                   MOVEL     MSG(14)       DSMSMS
053900001120     C                   ENDIF
054000001120     C                   ENDIF
054100070403     C*--------------------
054200070403     C* DATA DECORRENZA - VERIFICA VERSIONE
054300070403     C*--------------------
054400070403     C     ERRL00        IFEQ      '0'
054500070403     C                   Z-ADD     wvidfgs       K_grvfgs
054600070403     C                   Z-ADD     wviddde       K_grvdde
054700070406     C     KEYgrv01_c    CHAIN     figrv01l
054800070403     C                   IF        %found(figrv01l)
054900070403     C                   EVAL      wTopDdeVer = grvdde
055000070403     C                   EVAL      wTopDscVer = grvdscad
055100070403     C                   ELSE
055200070403     C                   MOVEL     '1'           ERRL00
055300070403     C                   SETON                                        70
055400070403     C                   MOVEL     MSG(19)       DSMSMS
055500070403     C                   ENDIF
055600070403     C                   ENDIF
055700070122     C*--------------------
055800070122     C* DATA SCADENZA
055900070122     C*--------------------
056000070124     C     *LIKE         DEFINE    viddscad      wviddscad
056100070122     C     ERRL00        IFEQ      '0'
056200070124     C                   Z-ADD     viddscad      WDA8
056300070122     C                   Z-ADD     *ZEROS        DA8W
056400070122     C                   EXSR      CHKDA8
056500070124     C                   Z-ADD     WDA8          viddscad
056600070124     C                   Z-ADD     DA8W          wviddscad
056700070122     C     ERRL00        IFEQ      '1'
056800070122     C                   SETON                                        71
056900070122     C                   MOVEL     MSG(14)       DSMSMS
057000070122     C                   ENDIF
057100070122     C                   ENDIF
057200070403     C*--------------------
057300070403     C* DATA SCADENZA ENTRO LIMITI SCADENZA VERSIONE RELATIVA
057400070403     C*--------------------
057500070403     C     ERRL00        IFEQ      '0'
057600070403     C                   IF        wviddscad <= wTopDscVer
057700070403     C                   ELSE
057800070403     C                   MOVEL     '1'           ERRL00
057900070403     C                   SETON                                        71
058000070403     C                   EVAL      DSMSMS = %trim(MSG(20))+ ' ' +
058100070403     C                                      %editw(wTopDscVer:'    /  /  ')
058200070403     C                   ENDIF
058300070403     C                   ENDIF
058400070122     C*--------------------
058500070122     C* SCADENZA < DECORRENZA
058600070122     C*--------------------
058700070122     C     ERRL00        IFEQ      '0'
058800070124     C     wviddscad     IFLT      wviddde
058900070122     C                   SETON                                        7071
059000070122     C                   MOVEL     MSG(15)       DSMSMS
059100070122     C                   MOVEL     '1'           ERRL00
059200070122     C                   ENDIF
059300070122     C                   ENDIF
059400070122     C*--------------------
059500070122     C* SCADENZA >= DATA CORRENTE
059600070122     C*--------------------
059700070122     C     ERRL00        IFEQ      '0'
059800070122     C     DEPOP1        IFEQ      'O02'
059900070122     C     DEPOP1        OREQ      'O07'
060000070122     C     DEPOP1        OREQ      'F06'
060100070124     C     wviddscad     IFLT      DATCOR
060200070122     C                   SETON                                        71
060300070122     C                   MOVEL     MSG(16)       DSMSMS
060400070122     C                   MOVEL     '1'           ERRL00
060500070122     C                   ENDIF
060600070122     C                   ENDIF
060700070122     C                   ENDIF
060800070122     C*--------------------
060900070122     C* DECORRENZA + SCADENZA
061000070122     C*--------------------
061100070122     C* ACCAVALLAMENTO DATI
061200070122     C     ERRL00        IFEQ      '0'
061300070122     C     *start        SETLL     figrg01l
061400070122     C                   READ(N)   figrg01l
061500070122     C                   DOW       not %eof(figrg01l)
061600070123     C                   IF        grgfgs  = wvidfgs AND
061700070123     C                             grgcgi  = vidcgi  AND
061800070123     C                             grgdde <> wviddde
061900070124     C                   IF        (wviddde  <=grgdscad AND grgdde<=wviddde) OR
062000070124     C                             (wviddscad>=grgdde AND grgdscad >= wviddscad)
062100070122     C                   SETON                                        7071
062200070122     C                   MOVEL     MSG(17)       DSMSMS
062300070122     C                   MOVEL     '1'           ERRL00
062400070122     C                   LEAVE
062500070122     C                   ENDIF
062600070122     C                   ENDIF
062700070122     C                   READ(N)   figrg01l
062800070122     C                   ENDDO
062900070122     C                   ENDIF
063000070406     C*--------------------
063100070406     C* OBBLIGATORIETA CAMPI
063200070406     C*--------------------
063300070406     C     ERRL00        IFEQ      '0'
063400070406     C                   IF        viddde = *zeros
063500070406     C                   MOVEL     '1'           ERRL00
063600070406     C                   MOVEL     MSG(12)       DSMSMS
063700070406     C                   SETON                                        70
063800070406     C                   ENDIF
063900070406     C                   IF        viddscad = *zeros
064000070406     C                   MOVEL     '1'           ERRL00
064100070406     C                   MOVEL     MSG(12)       DSMSMS
064200070406     C                   SETON                                        71
064300070406     C                   ENDIF
064400070406     C                   IF        vidfgs = *blanks OR
064500070406     C                             vidfgs = *zeros
064600070406     C                   MOVEL     '1'           ERRL00
064700070406     C                   MOVEL     MSG(12)       DSMSMS
064800070406     C                   SETON                                        72
064900070406     C                   ENDIF
065000070406     C                   IF        vidcgi = *blanks
065100070406     C                   MOVEL     '1'           ERRL00
065200070406     C                   MOVEL     MSG(12)       DSMSMS
065300070406     C                   SETON                                        73
065400070406     C                   ENDIF
065500070406     C                   IF        viddes = *blanks
065600070406     C                   MOVEL     '1'           ERRL00
065700070406     C                   MOVEL     MSG(12)       DSMSMS
065800070406     C                   SETON                                        74
065900070406     C                   ENDIF
066000070406     C                   IF        vidain = *blanks
066100070406     C                   MOVEL     '1'           ERRL00
066200070406     C                   MOVEL     MSG(12)       DSMSMS
066300070406     C                   SETON                                        75
066400070406     C                   ENDIF
066500070406     C                   ENDIF
066600070426     C*--------------------
066700070426     C* SOLO CARATTERI AMMESSI
066800070426     C*--------------------
066900070426     C     ERRL00        IFEQ      '0'
067000070426     C* Codice giro
067100070426     C                   Z-ADD     *zeros        wLen              2 0
067200070426     C                   EVAL      wLen = %len(%trim(VIDCGI))
067300070426     C     1             DO        wLen          x                 2 0
067400070426     C                   MOVEL     *blanks       wrkCHAR           1
067500070426     C                   EVAL      wrkCHAR = %subst(VIDCGI:x:1)
067600070426     C     wrkCHAR       LOOKUP    SKcharOK                               20
067700070426     C                   IF        *in20 = *off or wrkCHAR = *blanks
067800070426     C                   MOVEL     '1'           ERRL00
067900070426     C                   MOVEL     MSG(21)       DSMSMS
068000070426     C                   SETON                                        73
068100070426     C                   ENDIF
068200070426     C                   ENDDO
068300070426     C* Descrizione giro
068400070426     C     1             DO        30            x                 2 0
068500070426     C                   MOVEL     *blanks       wrkCHAR           1
068600070426     C                   EVAL      wrkCHAR = %subst(VIDDES:x:1)
068700070426     C     wrkCHAR       LOOKUP    SKcharOK                               20
068800070426     C                   IF        *in20 = *off
068900070426     C                   MOVEL     '1'           ERRL00
069000070426     C                   MOVEL     MSG(21)       DSMSMS
069100070426     C                   SETON                                        74
069200070426     C                   ENDIF
069300070426     C                   ENDDO
069400070426     C*
069500070426     C                   ENDIF
069600070622     C*--------------------
069700070622     C* CONTROLLO TIPOLOGIA GIRO
069800070622     C*--------------------
069900070622     C     ERRL00        IFEQ      '0'
070000070622     C     VIDTUG        ANDEQ     ' '
070100070622     C
070200070622     C                   MOVEL     '1'           ERRL00
070300070622     C                   MOVEL     MSG(12)       DSMSMS
070400070622     C                   SETON                                        77
070500070622     C                   ENDIF
070600080124     C*--------------------
070700080124     C* IN CASO DI MODIFICA TIPOLOGIA GIRO DA CONSEGNA/ENTRAMBI  A SOLO RITIRO
070800080124     C*--------------------
070900080124     C     ERRL00        IFEQ      '0'
071000080514     C                   IF        VIDTUG = 'R' AND (§GRGTUG = 'C'
071100080514     C                             or §grgtug = 'E')
071200080124     C                   MOVEL     '1'           ERRL00
071300080124     C                   MOVEL     MSG(23)       DSMSMS
071400080124     C                   SETON                                        77
071500080124     C                   ENDIF
071600080124     C                   ENDIF
071700070622     C*--------------------
071800070622     C* IN CASO DI GIRO CON USO SOLO RITRO
071900070622     C*--------------------
072000070622     C     ERRL00        IFEQ      '0'
072100070622     C                   IF        VIDTUG = 'R' AND VIDFIT = ' '
072200070622     C                   MOVEL     '1'           ERRL00
072300070622     C                   MOVEL     MSG(22)       DSMSMS
072400070622     C                   SETON                                        78
072500070622     C                   ENDIF
072600070622     C                   ENDIF
072700070122     C*--------------------
072800991026     C* INSERIMENTO -> RECORD INESISTENTE
072900070122     C*--------------------
073000001120     C     ERRL00        IFEQ      '0'
073100001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
073200070206     C                   z-add     wviddde       K_grgdde
073300070124     C                   z-add     wvidfgs       K_grgfgs
073400070124     C                   movel     vidcgi        K_grgcgi
073500070122     C                   exsr      letgrg
073600070206     C                   if        *in80 = *off
073700070122     C                   movel     '1'           ERRL00
073800070122     C                   movel     MSG(8)        DSMSMS
073900070122     C                   seton                                        707172
074000070122     C                   seton                                        73
074100070122     C                   endif
074200070122     C                   ENDIF
074300070122     C                   ENDIF
074400001120     C*
074500001120     C                   ENDSR
074600991027     C*------------------------------------------------------------------------*
074700070122     C* CNTPO  - Controlla codice filiale
074800991027     C*------------------------------------------------------------------------*
074900070122     C     CNTPO         begsr
075000001120     C*
075100070122     C                   movel     *blanks       vddfgs
075200070122     C     '?'           scan      vidfgs                                 97
075300001120     C                   if        *in97
075400070122     C                   movel     *blanks       cod1
075500070122     C                   movel     *blanks       des1
075600070122     C                   movel     *blanks       pra1
075700001120     C                   call      'TNSD24R'
075800001120     C                   parm                    cod1              3
075900001120     C                   parm                    tip1              1
076000001120     C                   parm                    des1             25
076100001120     C                   parm                    pra1              3
076200070122     C                   movel     cod1          vidfgs
076300001120     C                   endif
076400070122     C                   move      vidfgs        cod1n             3 0
076500001120     C     cod1n         chain     azorg01l                           98
076600001120     C                   if        not *in98 and orgfag <>'V'
076700070123     C                   z-add     orgfil        wvidfgs           3 0
076800070122     C                   movel     orgdes        vddfgs
076900001120     C                   else
077000001120     C                   seton                                        69
077100001120     C                   move      '1'           errl00
077200001120     C                   movel     msg(13)       dsmsms
077300001120     C                   endif
077400001120     C*
077500001120     C                   endsr
077600991026     C*------------------------------------------------------------------------*
077700991026     C* CHKDA8 - CONTROLLA LA DATA (8)
077800991026     C*------------------------------------------------------------------------*
077900991026     C     CHKDA8        BEGSR
078000991026     C*
078100070122     C     *LIKE         DEFINE    viddde        WDA8
078200070122     C     *LIKE         DEFINE    viddde        DA8W
078300991026     C*
078400991026     C                   Z-ADD     WDA8          G08DAT
078500991026     C                   Z-ADD     *ZEROS        G08INV
078600991026     C                   MOVEL     '0'           G08ERR
078700991026     C                   CALL      'XSRDA8'
078800991026     C                   PARM                    WLBDA8
078900991026     C                   Z-ADD     G08DAT        WDA8
079000991026     C                   Z-ADD     G08INV        DA8W
079100991026IF  1C     G08ERR        IFEQ      '1'
079200991026     C                   MOVEL     '1'           ERRL00
079300991026E   1C                   ENDIF
079400991026     C*
079500991026     C                   ENDSR
079600970526     C*------------------------------------------------------------------------*
079700970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
079800970526     C*------------------------------------------------------------------------*
079900970526     C     IREVID        BEGSR
080000970526     C* CAMPI
080100070122     C                   Z-ADD     grgdde        G08INV
080200070122     C                   Z-ADD     *ZEROS        G08DAT
080300070122     C                   MOVEL     '3'           G08ERR
080400070122     C                   CALL      'XSRDA8'
080500070122     C                   PARM                    WLBDA8
080600070122     C                   Z-ADD     G08DAT        viddde
080700070122     C                   Z-ADD     G08INV        wviddde
080800070122     C*
080900070124     C                   Z-ADD     grgdscad      G08INV
081000070122     C                   Z-ADD     *ZEROS        G08DAT
081100070122     C                   MOVEL     '3'           G08ERR
081200070122     C                   CALL      'XSRDA8'
081300070122     C                   PARM                    WLBDA8
081400070124     C                   Z-ADD     G08DAT        viddscad
081500070124     C                   Z-ADD     G08INV        wviddscad
081600070122     C*
081700070122     C                   MOVE(P)   grgfgs        vidfgs
081800070123     C                   MOVEL     grgcgi        vidcgi
081900070122     C                   MOVEL     grgain        vidain
082000070122     C                   MOVEL     grgdes        viddes
082100070209     C                   MOVEL     grgflo        DGRGFLO
082200070209     C                   MOVEL     §GRGFIT       vidfit
082300070320     C                   MOVEL     §GRGDST       viddst
082400070320     C                   MOVEL     §GRGEDP       videdp
082500070621     C                   MOVEL     §GRGTUG       vidtug
082600110624     C                   MOVEL     §GRGcsto      vidcsto
082700070122     C                   MOVEL     grgact        vidact
082800070122     C                   MOVEL     grgute        vidute
082900070122     C*
083000070122     C                   Z-ADD     grgduv        G08INV
083100070122     C                   Z-ADD     *ZEROS        G08DAT
083200070122     C                   MOVEL     '3'           G08ERR
083300070122     C                   CALL      'XSRDA8'
083400070122     C                   PARM                    WLBDA8
083500070122     C                   Z-ADD     G08DAT        vidduv
083600070307     C*
083700070307     C* SE TRATTASI D GIRO ANNULLATO => EVIDENZIO SUBITO
083800070307     C                   IF        grgATB = 'A'
083900070307     C                   SETON                                        76
084000070320     C                   SETON                                        91
084100070307     C                   ENDIF
084200051122     C*
084300051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
084400070123     C                   IF        DEPOP1 = 'F06'
084500070201     C***                EVAL      viddde   = DATCORINV
084600070404     C***                EVAL      viddde   = 01012007
084700070404     C***                EVAL      viddscad = 31122039
084800051122     C                   ENDIF
084900070718     C*
085000070718     C* SE TRATTASI D GIRO IN GESTIONE SOLO UTENTI EDP* E UTENTE CORRENTE NO EDP* => SOLO VISUAL.
085100070718     C                   IF        *IN94  = *off AND
085200070718     C                             videdp = 'S'
085300070718     C                   SETON                                        90
085400070718     C                   SETON                                        91
085500070718     C                   ENDIF
085600020218     C*
085700970526     C                   ENDSR
085800970526     C*------------------------------------------------------------------------*
085900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
086000970526     C*------------------------------------------------------------------------*
086100970526     C     AZZVID        BEGSR
086200970526     C* CAMPI
086300070122     C                   MOVEA     *IN           wSavIn           99
086400070122     C                   CLEAR                   DG10D03
086500070201     C***                EVAL      viddde   = DATCORINV
086600070404     C***                EVAL      viddde   = 01012007
086700070404     C***                EVAL      viddscad = 31122039
086800070122     C                   MOVEA     wSavIn        *IN
086900070709     C***  N95              EVAL      VIDTUG  = 'E'
087000070709     C                   EVAL      VIDTUG  = 'E'
087100970526     C*
087200970526     C                   ENDSR
087300970526     C*------------------------------------------------------------------------*
087400070122     C* IMPGRG - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
087500970526     C*------------------------------------------------------------------------*
087600070122     C     IMPGRG        BEGSR
087700970526     C*
087800070122     C                   Z-ADD     viddde        G08dat
087900070122     C                   Z-ADD     *ZEROS        G08inv
088000070122     C                   MOVEL     '0'           G08ERR
088100070122     C                   CALL      'XSRDA8'
088200070122     C                   PARM                    WLBDA8
088300070122     C                   Z-ADD     G08inv        grgdde
088400070122     C*
088500070124     C                   z-add     viddscad      G08dat
088600070122     C                   Z-ADD     *ZEROS        G08inv
088700070122     C                   MOVEL     '0'           G08ERR
088800070122     C                   CALL      'XSRDA8'
088900070122     C                   PARM                    WLBDA8
089000070124     C                   Z-ADD     G08inv        grgdscad
089100070122     C*
089200070122     C                   move(p)   vidfgs        grgfgs
089300070123     C                   movel     vidcgi        grgcgi
089400070122     C                   movel     vidain        grgain
089500070209     C                   movel     viddes        grgdes
089600110624     C                   movel     vidcsto       §GRGcsto
089700110624     C                   movel     vidfit        §GRGFIT
089800070320     C                   movel     viddst        §GRGDST
089900070320     C                   movel     videdp        §GRGEDP
090000070621     C                   movel     vidtug        §GRGTUG
090100070209     C                   movel     DGRGFLO       grgflo
090200070122     C                   z-add     datcor        grgduv
090300070122     C                   movel     knmus         grgute
090400970526     C*
090500970526     C                   ENDSR
090600970526     C*------------------------------------------------------------------------*
090700970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
090800970526     C*------------------------------------------------------------------------*
090900970526     C     IMPFUN        BEGSR
091000970526     C*
091100970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
091200970526IF  1C     DEPOP1        IFEQ      'O05'
091300970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
091400970526     C                   MOVEL     'N'           FUDATT
091500970526     C                   MOVEL     FUDRIG        FUD(6)
091600970526E   1C                   ENDIF
091700070404     C*
091800070404     C* SE IN INSERIMENTO ABILITA LA FUNZIONE DI RICERCA VERSIONI
091900070404IF  1C     DEPOP1        IFEQ      'F06'
092000070404     C                   MOVEL     FUD(10)       FUDRIG                         *AGGIORNAMENTO
092100070404     C                   MOVEL     'S'           FUDATT
092200070406     C                   MOVEL     FUDRIG        FUD(10)
092300070404E   1C                   ENDIF
092400970526     C*
092500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
092600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
092700970526     C                   EXSR      FUNGES
092800970526     C*
092900970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
093000970526     C*---
093100070122     C* 90 -> VISUALIZZAZIONE
093200070122     C* 91 -> MODIFICA
093300070122     C* 92 -> INSERIMENTO
093400970526     C*---
093500070123     C                   SETON                                        90
093600070123     C                   SETON                                        91
093700070123     C                   SETOFF                                       92
093800070122IF  1C     DEPOP0        IFEQ      'P03'                                        GESTIONE
093900070122     C     DEPOP0        OREQ      'P04'                                        GESTIONE FULL
094000970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
094100970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
094200970526     C                   SETOFF                                       91
094300070122     C                   SETON                                        92
094400070404     C                   SETOFF                                       93
094500970526E   2C                   ENDIF
094600970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
094700970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
094800970526E   2C                   ENDIF
094900970526E   1C                   ENDIF
095000970526     C*
095100970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
095200970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
095300070124     C                   Z-ADD     D09idde       K_grgdde
095400070124     C                   Z-ADD     D09ifgs       K_grgfgs
095500070124     C                   MOVEL     D09icgi       K_grgcgi
095600070122     C     KEYgrg01      CHAIN     figrg01l
095700070122IF  2C                   IF        not %found(figrg01l)                         *NON TROVATO
095800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
095900070122     C                   MOVEL     MSG(6)        DSMSMS
096000970526X   2C                   ELSE
096100970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
096200980128IF  3C     DEPOP1        IFEQ      'O05'
096300070122IF  3C     DEPOP1        OREQ      'O02'
096400980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
096500980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
096600980128E   3C                   ENDIF
096700970526E   2C                   ENDIF
096800970526X   1C                   ELSE                                                   *INSERIMENTO
096900970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
097000970526E   1C                   ENDIF
097100970526     C*
097200970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
097300970526IF  1C     DEPOP1        IFEQ      'O02'
097400970526     C                   MOVEL     DEC(1)        VIDFUN
097500970526E   1C                   ENDIF
097600991014IF  1C     DEPOP1        IFEQ      'O03'
097700991014     C                   MOVEL     DEC(6)        VIDFUN
097800991014E   1C                   ENDIF
097900970526IF  1C     DEPOP1        IFEQ      'O04'
098000970526     C                   MOVEL     DEC(2)        VIDFUN
098100970526E   1C                   ENDIF
098200970526IF  1C     DEPOP1        IFEQ      'O05'
098300970526     C                   MOVEL     DEC(3)        VIDFUN
098400970526E   1C                   ENDIF
098500970526IF  1C     DEPOP1        IFEQ      'O07'
098600970526     C                   MOVEL     DEC(4)        VIDFUN
098700970526E   1C                   ENDIF
098800970526IF  1C     DEPOP1        IFEQ      'F06'
098900970526     C                   MOVEL     DEC(5)        VIDFUN
099000970526E   1C                   ENDIF
099100970526     C*
099200970526     C                   ENDSR
099300070122     C*--------------------------------------------------------------------------------------------*
099400070122     C* REPERISCE I DATI UTENTE
099500070122     C*--------------------------------------------------------------------------------------------*
099600070122     C     REPDATIUTE    BEGSR
099700070122     C*
099800070122     C* INIZIALIZZA VARIABILI DI WRK
099900070122     C                   CLEAR                   TIBS34DS
100000070122     C                   CLEAR                   AZUTEDS
100100070122     C                   CLEAR                   DDATIUTE
100200070122     C*
100300070122     C     *DTAARA       DEFINE    §azute        azuteds
100400070122     C     *DTAARA       DEFINE    §datiute      ddatiute
100500070122     C                   IN(E)     *DTAARA
100600070122if  1C                   IF        %Error
100700070122     c                   EVAL      I34Tla = 'L'
100800070122     C                   CALL      'TIBS34R'
100900070122     C                   PARM                    Tibs34Ds
101000070122     C                   IN        *DTAARA
101100070122e   1C                   ENDIF
101200070122     C*
101300070122     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
101400070122     C                   MOVEL(P)  RSUT          DSFIRS           20
101500070122     C*
101600070122     C                   ENDSR
101700070403     C*------------------------------------------------------------------------*
101800070403     C* CNTVER - Conta le versiono aperte di una filiale
101900070403     C*------------------------------------------------------------------------*
102000070403     C     CNTVER        begsr
102100070403     C*
102200070403     C                   z-add     *zeros        wCntVer           3 0
102300070403     C     wvidfgs       setll     figrv01l
102400070403     C                   if        %found(figrv01l)
102500070403     C                   dow       not %eof(figrv01l)
102600070403     C                   read      figrv01l
102700070403     C                   if        grvfgs = wvidfgs
102800070403     C                   if        datcor <= grvdscad
102900070403     C                   add       1             wCntVer
103000070403     C                   z-add     grvdde        wTopDdeVer        8 0
103100070403     C                   z-add     grvdscad      wTopDscVer        8 0
103200070403     C                   endif
103300070403     C                   else
103400070403     C                   leave
103500070403     C                   endif
103600070403     C                   enddo
103700070403     C                   endif
103800070403     C*
103900070403     C                   endsr
104000970526     C*------------------------------------------------------------------------*
104100970526     C* *INZSR - OPERAZIONI INIZIALI
104200970526     C*------------------------------------------------------------------------*
104300970526     C     *INZSR        BEGSR
104400070426     C*
104500070426     C                   movea     charOK        SKcharOK
104600970526     C*---
104700970526     C* RICEVIMENTO PARAMETRI
104800970526     C*---
104900970526     C     *ENTRY        PLIST
105000970612     C                   PARM                    KPJBA
105100070122     C                   MOVEL(P)  KPJBU         fidg09ds
105200070320     C*
105300070320     C* VERIFICO SUBITO SE TRATTASI D UTENTE EDP*
105400070320     C                   IF        %subst(KNMUS:1:3) = 'EDP'
105500070320     C                   SETON                                        94
105600070320     C                   ELSE
105700070320     C                   SETOFF                                       94
105800070320     C                   ENDIF
105900070621      * indipendentemente dall'attivazione imposto sempre tipo utilizzo giro =  a E=ENTRAMBI (conse-
106000070621      * gna / ritiro )
106100070622     c  N95              eval      vidtug = 'E'
106200070122     C*
106300070122     C* REPERIMENTO DATI UTENTE
106400070122     C                   EXSR      REPDATIUTE
106500970526     C*
106600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
106700970526     C     XMSGPA        PLIST
106800970526     C                   PARM                    DSMSMS
106900970526     C                   PARM                    DSMSPG
107000970526     C                   PARM                    DSMSKE
107100970526     C                   PARM                    DSMSER
107200991108     C*---
107300970526     C*
107400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
107500970526     C                   CALL      'XKEY'
107600970526     C                   PARM                    DSKEY
107700970526     C*---
107800970526     C* VARIABILI NON RIFERITE AL DATA BASE
107900970526     C*---
108000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
108100070124     C     *LIKE         DEFINE    D09IOP0       DEPOP0
108200070124     C     *LIKE         DEFINE    D09IOP1       DEPOP1
108300970526     C*---
108400070122     C* VARIABILI DI CONTROLLO
108500970526     C*---
108600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
108700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
108800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
108900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
109000970526     C*
109100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
109200070122     C                   MOVEL     'FIDG10R'     DSMSPG
109300070122     C                   MOVEL     'FIDG10R'     MSGQUE
109400970526     C                   MOVEL     *BLANKS       MSGKEY
109500970526     C                   MOVEL     *BLANKS       DSMSMS
109600970526     C*---
109700970526     C* VARIABILI RIFERITE AL DATA BASE
109800970526     C*---
109900070123     C     *LIKE         DEFINE    grgcgi        K_grgcgi
110000070123     C     *LIKE         DEFINE    grgdde        K_grgdde
110100070123     C     *LIKE         DEFINE    grgfgs        K_grgfgs
110200070403     C     *LIKE         DEFINE    grvfgs        K_grvfgs
110300070403     C     *LIKE         DEFINE    grvdde        K_grvdde
110400970526     C*---
110500970526     C* CHIAVI DI LETTURA
110600970526     C*---
110700070122     C* LETTURA figrg01l - COMPLETA
110800070122     C     KEYgrg01      KLIST
110900070122     C                   KFLD                    K_grgdde
111000070123     C                   KFLD                    K_grgfgs
111100070123     C                   KFLD                    K_grgcgi
111200070406     C* LETTURA figrv01l - COMPLETA
111300070406     C     KEYgrv01_c    KLIST
111400070403     C                   KFLD                    K_grvfgs
111500070403     C                   KFLD                    K_grvdde
111600970528     C*
111700970528     C* CALCOLA LA DATA CORRENTE
111800970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
111900970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
112000970528     C                   Z-ADD     WN8           G08DAT
112100051122     C                   Z-ADD     WN8           DATCORINV         8 0
112200970528     C                   Z-ADD     *ZEROS        G08INV
112300970528     C                   MOVEL     '0'           G08ERR
112400970528     C                   CALL      'XSRDA8'
112500970528     C                   PARM                    WLBDA8
112600970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
112700970526     C*
112800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
112900070124     C                   MOVEL     D09IOP0       DEPOP0                         *LIVELLO PROCEDURA
113000070124     C                   MOVEL     D09IOP1       DEPOP1                         *LIVELLO PROGRAMMA
113100970526     C*
113200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
113300970526     C                   EXSR      IMPFUN
113400970526     C*
113500970526     C                   ENDSR
113600970526     C*------------------------------------------------------------------------*
113700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
113800970526     C*------------------------------------------------------------------------*
113900970526     C     FUNGES        BEGSR
114000970526     C*
114100970526     C* PULISCO CAMPI DI LAVORO
114200970526     C                   Z-ADD     0             W                 5 0
114300970526     C*
114400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
114500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
114600970526     C     1             DO        24            I
114700970526     C                   MOVEL     FUD(I)        FUDRIG
114800970526     C*
114900970526     C* PULIZIA FUNZIONI PRECEDENTI
115000970526     C                   MOVEL     *BLANKS       FUV(I)
115100970526     C*
115200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
115300970526     C* (SALTA F24)
115400970526     C     FUDATT        IFEQ      'S'
115500970526     C     FUDFUN        ANDNE     '24'
115600970526     C                   ADD       1             W
115700970526     C                   MOVEL     FUDDES        FUV(W)
115800970526     C                   END
115900970526     C                   END                                                    DO WHILE
116000970526     C*
116100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
116200970526     C* ALTRIMENTI DISATTIVO F24
116300970526     C     1             DO        24            I
116400970526     C                   MOVEL     FUD(I)        FUDRIG
116500970526     C*
116600970526     C     FUDFUN        IFEQ      '24'
116700970526     C     W             IFGT      6
116800970526     C                   MOVEL     'S'           FUDATT
116900970526     C                   ELSE
117000970526     C                   MOVEL     *BLANKS       FUDATT
117100970526     C                   END                                                    W GT 8
117200970526     C                   MOVEL     FUDRIG        FUD(I)
117300970526     C                   END
117400970526     C                   END                                                    DO WHILE
117500970526     C*
117600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
117700970526     C     W             DIV       6             I
117800970526     C                   MVR                     Z
117900970526     C*
118000970526     C     Z             IFNE      0
118100970526     C     I             ADD       1             Z
118200970526     C                   ELSE
118300970526     C                   Z-ADD     I             Z                 5 0
118400970526     C                   END
118500970526     C*
118600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
118700970526     C                   ADD       1             FUNGRU
118800970526     C     FUNGRU        IFGT      Z
118900970526     C     FUNGRU        ORLT      1
119000970526     C                   Z-ADD     1             FUNGRU
119100970526     C                   END
119200970526     C*
119300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
119400970526     C     FUNGRU        IFEQ      1
119500970526     C                   MOVEL     FUV(1)        FUN001
119600970526     C                   MOVEL     FUV(2)        FUN002
119700970526     C                   MOVEL     FUV(3)        FUN003
119800970526     C                   MOVEL     FUV(4)        FUN004
119900970526     C                   MOVEL     FUV(5)        FUN005
120000970526     C                   MOVEL     FUV(6)        FUN006
120100970526     C                   END
120200970526     C*
120300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
120400970526     C     FUNGRU        IFEQ      2
120500970526     C                   MOVEL     FUV(7)        FUN001
120600970526     C                   MOVEL     FUV(8)        FUN002
120700970526     C                   MOVEL     FUV(9)        FUN003
120800970526     C                   MOVEL     FUV(10)       FUN004
120900970526     C                   MOVEL     FUV(11)       FUN005
121000970526     C                   MOVEL     FUV(12)       FUN006
121100970526     C                   END
121200970526     C*
121300970526     C* TERZO GRUPPO DI 6 FUNZIONI
121400970526     C     FUNGRU        IFEQ      3
121500970526     C                   MOVEL     FUV(13)       FUN001
121600970526     C                   MOVEL     FUV(14)       FUN002
121700970526     C                   MOVEL     FUV(15)       FUN003
121800970526     C                   MOVEL     FUV(16)       FUN004
121900970526     C                   MOVEL     FUV(17)       FUN005
122000970526     C                   MOVEL     FUV(18)       FUN006
122100970526     C                   END
122200970526     C*
122300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
122400970526     C     FUNGRU        IFEQ      4
122500970526     C                   MOVEL     FUV(19)       FUN001
122600970526     C                   MOVEL     FUV(20)       FUN002
122700970526     C                   MOVEL     FUV(21)       FUN003
122800970526     C                   MOVEL     FUV(22)       FUN004
122900970526     C                   MOVEL     *BLANKS       FUN005
123000970526     C                   MOVEL     *BLANKS       FUN006
123100970526     C                   END
123200970526     C*
123300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
123400970526     C                   MOVEL     *BLANKS       FUN007
123500970526     C                   MOVEL     *BLANKS       FUN008
123600970526     C*
123700970526     C     1             DO        24            I
123800970526     C                   MOVEL     FUD(I)        FUDRIG
123900970526     C     FUDFUN        IFEQ      '24'
124000970526     C     FUDATT        ANDEQ     'S'
124100970526     C                   MOVEL     FUDDES        FUN008
124200970526     C                   END
124300970526     C                   END                                                    DO
124400970526     C*
124500970526     C                   ENDSR
124600970526     C*------------------------------------------------------------------------*
124700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
124800970526     C*------------------------------------------------------------------------*
124900970526     C     FUNCON        BEGSR
125000970526     C*
125100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
125200970526     C                   EXSR      FUNVAR
125300970526     C*
125400970526     C* PULISCO CAMPI DI LAVORO
125500970526     C                   Z-ADD     0             I                 5 0
125600970526     C                   Z-ADD     0             W                 5 0
125700970526     C*
125800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
125900970526     C     FUNKEY        IFNE      *BLANKS
126000970526     C     1             DO        24            I
126100970526     C                   MOVEL     FUD(I)        FUDRIG
126200970526     C*
126300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
126400970526     C     FUNKEY        IFEQ      FUDFUN
126500970526     C     FUDATT        ANDEQ     'S'
126600970526     C                   Z-ADD     I             W
126700970526     C                   END
126800970526     C                   END                                                    DO WHILE
126900970526     C*
127000970526     C* FUNZIONE NON ATTIVA (ERRORE)
127100970526     C     W             IFEQ      0
127200970526     C*
127300970526     C* EMISSIONE MESSAGGIO DI ERRORE
127400970526     C                   MOVEL     MSG(1)        DSMSMS
127500970526     C*
127600970526     C                   END                                                    W=0
127700970526     C                   END                                                    FUNKEY NE *BLANKS
127800970526     C*
127900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
128000970526     C     W             IFNE      0
128100970526     C     FUNKEY        OREQ      *BLANKS
128200970526     C     FUNKEY        IFEQ      *BLANKS
128300970526     C                   EXSR      ENTGES
128400970526     C                   END
128500970526     C     FUNKEY        IFEQ      '03'
128600970526     C                   EXSR      F03GES
128700970526     C                   END
128800970526     C     FUNKEY        IFEQ      '06'
128900970526     C                   EXSR      F06GES
129000970526     C                   END
129100070403     C     FUNKEY        IFEQ      '10'
129200070403     C                   EXSR      F10GES
129300070403     C                   END
129400970526     C     FUNKEY        IFEQ      '12'
129500970526     C                   EXSR      F12GES
129600970526     C                   END
129700970526     C     FUNKEY        IFEQ      '24'
129800970526     C                   EXSR      F24GES
129900970526     C                   END
130000970526     C                   END
130100970526     C*
130200970526     C                   ENDSR
130300970526     C*------------------------------------------------------------------------*
130400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
130500970526     C*------------------------------------------------------------------------*
130600970526     C     FUNVAR        BEGSR
130700970526     C*
130800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
130900970526     C*
131000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
131100970526     C     KEY           IFEQ      ENTER
131200970526     C                   MOVEL     *BLANKS       FUNKEY
131300970526     C                   END
131400970526     C     KEY           IFEQ      F1
131500970526     C                   MOVEL     '01'          FUNKEY
131600970526     C                   END
131700970526     C     KEY           IFEQ      F2
131800970526     C                   MOVEL     '02'          FUNKEY
131900970526     C                   END
132000970526     C     KEY           IFEQ      F3
132100970526     C                   MOVEL     '03'          FUNKEY
132200970526     C                   END
132300970526     C     KEY           IFEQ      F4
132400970526     C                   MOVEL     '04'          FUNKEY
132500970526     C                   END
132600970526     C     KEY           IFEQ      F5
132700970526     C                   MOVEL     '05'          FUNKEY
132800970526     C                   END
132900970526     C     KEY           IFEQ      F6
133000970526     C                   MOVEL     '06'          FUNKEY
133100970526     C                   END
133200970526     C     KEY           IFEQ      F7
133300970526     C                   MOVEL     '07'          FUNKEY
133400970526     C                   END
133500970526     C     KEY           IFEQ      F8
133600970526     C                   MOVEL     '08'          FUNKEY
133700970526     C                   END
133800970526     C     KEY           IFEQ      F9
133900970526     C                   MOVEL     '09'          FUNKEY
134000970526     C                   END
134100970526     C     KEY           IFEQ      F10
134200970526     C                   MOVEL     '10'          FUNKEY
134300970526     C                   END
134400970526     C     KEY           IFEQ      F11
134500970526     C                   MOVEL     '11'          FUNKEY
134600970526     C                   END
134700970526     C     KEY           IFEQ      F12
134800970526     C                   MOVEL     '12'          FUNKEY
134900970526     C                   END
135000970526     C     KEY           IFEQ      F13
135100970526     C                   MOVEL     '13'          FUNKEY
135200970526     C                   END
135300970526     C     KEY           IFEQ      F14
135400970526     C                   MOVEL     '14'          FUNKEY
135500970526     C                   END
135600970526     C     KEY           IFEQ      F15
135700970526     C                   MOVEL     '15'          FUNKEY
135800970526     C                   END
135900970526     C     KEY           IFEQ      F16
136000970526     C                   MOVEL     '16'          FUNKEY
136100970526     C                   END
136200970526     C     KEY           IFEQ      F17
136300970526     C                   MOVEL     '17'          FUNKEY
136400970526     C                   END
136500970526     C     KEY           IFEQ      F18
136600970526     C                   MOVEL     '18'          FUNKEY
136700970526     C                   END
136800970526     C     KEY           IFEQ      F19
136900970526     C                   MOVEL     '19'          FUNKEY
137000970526     C                   END
137100970526     C     KEY           IFEQ      F20
137200970526     C                   MOVEL     '20'          FUNKEY
137300970526     C                   END
137400970526     C     KEY           IFEQ      F21
137500970526     C                   MOVEL     '21'          FUNKEY
137600970526     C                   END
137700970526     C     KEY           IFEQ      F22
137800970526     C                   MOVEL     '22'          FUNKEY
137900970526     C                   END
138000970526     C     KEY           IFEQ      F23
138100970526     C                   MOVEL     '23'          FUNKEY
138200970526     C                   END
138300970526     C     KEY           IFEQ      F24
138400970526     C                   MOVEL     '24'          FUNKEY
138500970526     C                   END
138600970526     C     KEY           IFEQ      ROLLUP
138700970526     C                   MOVEL     'RU'          FUNKEY
138800970526     C                   END
138900970526     C     KEY           IFEQ      HELP
139000970526     C                   MOVEL     'HP'          FUNKEY
139100970526     C                   END
139200970526     C                   ENDSR
139300970526     C*------------------------------------------------------------------------*
139400970526     C* NOTE COSTRUZIONE SCHIERE
139500970526     C*------------------------------------------------------------------------*
139600970526     C*
139700970526     C* -MSG-MESSAGGI
139800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
139900970526     C* (NON TOCCARE MSG,1 E MSG,2)
140000970526     C*
140100970526     C* -CMD-COMANDI
140200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
140300970526     C*
140400970526     C* -OPD-OPZIONI
140500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
140600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
140700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
140800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
140900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
141000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
141100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
141200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
141300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
141400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
141500970526     C*
141600970526     C* -FUD-FUNZIONI
141700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
141800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
141900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
142000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
142100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
142200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
142300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
142400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
142500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
142600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
142700970526     O*------------------------------------------------------------------------*
142800970526** DEC - DECODIFICHE
142900970526    MODIFICA                                                      1
143000970526  ANNULLAMENTO                                                    2
143100970526VISUALIZZAZIONE                                                   3
143200970526   RIPRISTINO                                                     4
143300970526  INSERIMENTO                                                     5
143400991014     COPIA                                                        6
143500970526** MSG - MESSAGGI
143600070122FIDG10R- Tasto funzionale non gestito                                          1
143700070122FIDG10R- Inserimento effettuato                                                2
143800070122FIDG10R- Modifica effettuata                                                   3
143900070122FIDG10R- Annullamento effettuato                                               4
144000070122FIDG10R- Ripristino effettuato                                                 5
144100070122FIDG10R- Richiesta funzione per record inesistente                             6
144200070122FIDG10R- Record momentaneamente allocato: riprovare più tardi                  7
144300070122FIDG10R- Richiesta funzione per record esistente                               8
144400070122FIDG10R- Giro già presente con questo codice                                   9
144500070122FIDG10R- Giro già presente con questa descrizione                              10
144600070122FIDG10R- Codice giro fuori convenzione, indicare l'ambito nel codice           11
144700070122FIDG10R- Campo obbligatorio                                                    12
144800070122FIDG10R- Codice filiale inesistente o di categoria V=varia                     13
144900070122FIDG10R- Data errata                                                           14
145000070122FIDG10R- La data fine deve essere maggiore della data inizio                   15
145100070122FIDG10R- Il record non può scadere in un giorno già trascorso                  16
145200070122FIDG10R- Il record immesso si sovrappone ad uno già in decorrenza              17
145300070403FIDG10R- Gestione automatica giri consegna non ancora attiva sulla Filiale     18
145400070403FIDG10R- Nessuna versione attiva alla data decorrenza indicata (F10)           19
145500070403FIDG10R- Data scadenza deve essere inferiore o uguale al:                      20
145600070426FIDG10R- Presenti caratteri non validi, utilizzare solo numeri o lettere.      21
145700070622FIDG10R- I giri di solo ritiro devono essere fittizi (no su mappa)             22
145800080124FIDG10R- Errore modificare tipo utilizzo giro da consegna/entrambi a ritiro    23
145900070122FIDG10R-                                                                       24
146000070122FIDG10R-                                                                       25
146100070122FIDG10R-                                                                       26
146200070122FIDG10R- Valori incompatibili, correggere!                                     27
146300970526** FUD - FUNZIONI
146400070201                   -        (S/N)=N                        funz.01¬
146500970526                   -        (S/N)=N                        funz.02¬
146600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
146700970526                   -        (S/N)=N                        funz.04¬
146800970526                   -        (S/N)=N                        funz.05¬
146900970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
147000970527                   -        (S/N)=N                        funz.07¬
147100970527                   -        (S/N)=N                        funz.08¬
147200970527                   -        (S/N)=N                        funz.09¬
147300070404F10=Ricerca vers.  -        (S/N)=N                        funz.10¬
147400970526                   -        (S/N)=N                        funz.11¬
147500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
147600970526                   -        (S/N)=N                        funz.13¬
147700970526                   -        (S/N)=N                        funz.14¬
147800970526                   -        (S/N)=N                        funz.15¬
147900970526                   -        (S/N)=N                        funz.16¬
148000970526                   -        (S/N)=N                        funz.17¬
148100970526                   -        (S/N)=N                        funz.18¬
148200970526                   -        (S/N)=N                        funz.19¬
148300970526                   -        (S/N)=N                        funz.20¬
148400970526                   -        (S/N)=N                        funz.21¬
148500970526                   -        (S/N)=N                        funz.22¬
148600970526                   -        (S/N)=N                        funz.23¬
148700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
