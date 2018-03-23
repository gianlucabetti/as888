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
008100170620     D**** FNLV50DS      E DS
008200070124     D*------------
008300070124     D* DS DESCRIZIONI ORGANIGRAMMA
008400070124     D*------------
008500070124     D OG148         E DS
008600970526     D*------------
008700970526     D* ARCHITETTURA
008800970526     D*------------
008900970526     D KPJBA         E DS
009000170620
009100170620     d sav_kpjbu       s                   like(kpjbu)
009200170620
009300170620      // - Reperimento filiali gestite dall'utente
009400170620     d TRUL31DS      e ds
009500170620     d  POG                   10    759    dim(250)
009600170620
009700040816     D*------------
009800070426     D* COSTANTI
009900070426     D*------------
010000070426     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
010100070426     D                                     1234567890 ')
010200070426     D*-------------------
010300070426     D* SCHIERE DI WRK
010400070426     D*-------------------
010500070426     D SKcharOK        S              1    DIM(37)
010600110624     d
010700110624     D DSGRG         E DS                  extname(figrg00F)
010800110624     D oldgrg        E DS                  extname(figrg00F) prefix(O_)
010900110624     D newgrg        E DS                  extname(figrg00F) prefix(N_)
011000170620
011100170620      //---------------------------------------------------------------
011200170620      //?Definizione Procedure usate.
011300170620      //---------------------------------------------------------------
011400170620      // - Caricamento Filiali in gestione
011500170620     d TRUL31R         pr                  extpgm('TRUL31R')
011600170620     d  kpjba                              likeds(kpjba)
011700170620     d  trul31ds                           likeds(trul31ds)
011800170620
011900970526     C*------------------------------------------------------------------------*
012000970526     C* MAIN LINES
012100970526     C*------------------------------------------------------------------------*
012200970526     C*
012300970526     C* NON CI SONO ERRORI IN ENTRATA
012400970526IF  1C     ERRL00        IFEQ      '0'
012500970526     C*
012600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012700970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013000970526     C*
013100970526     C* EMETTE FORMATO VIDEO
013200970526     C                   EXSR      BUTFMR
013300970526     C*
013400970526     C* GESTIONE TASTI FUNZIONALI
013500970526     C                   EXSR      FUNCON
013600970526E   2C                   ENDDO
013700970526     C*
013800970526     C* CI SONO ERRORI IN ENTRATA
013900970527X   1C                   ELSE
014000070124     C                   MOVEL     '1'           D09oerr
014100070124     C                   MOVEL     DSMSMS        D09omsg
014200970526E   1C                   ENDIF
014300070123     C*
014400070123     C* RITORNO PARAMETRI
014500070123     C                   MOVEL(P)  fidg09ds      KPJBU
014600070122     C* FINE PGM
014700970526     C                   SETON                                        LR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
015000970526     C*------------------------------------------------------------------------*
015100970526     C     BUTFMR        BEGSR
015200970526     C*
015300970526     C* EMETTE LA TESTATA
015400070122     C                   WRITE     DG10D01
015500970526     C*
015600970526     C* SPEDISCE EVENTUALE MESSAGGIO
015700970526     C                   EXSR      SNDMSG
015800970526     C*
015900970526     C* EMETTE LE FUNZIONI
016000070122     C                   WRITE     DG10D02
016100970526     C*
016200970526     C* EMETTE IL VIDEO
016300070122     C                   EXFMT     DG10D03
016400970526     C*
016500970526     C* REIMPOSTA FLAG DI ERRORE
016600970526     C                   MOVEL     '0'           ERRL00
016700970526     C*
016800970526     C* REIMPOSTA INDICATORI DI ERRORE
016900070122     C                   SETOFF                                       707172
017000070122     C                   SETOFF                                       737475
017100120402     C                   setoff                                       787796
017200970526     C*
017300970526     C                   ENDSR
017400970526     C*------------------------------------------------------------------------*
017500970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017600970526     C*------------------------------------------------------------------------*
017700970526     C     SNDMSG        BEGSR
017800970526     C*
017900970526     C* PULISCE IL SFILE MESSAGGI
018000970526     C                   MOVEL     '1'           *IN40
018100070122     C                   WRITE     DG10DMC
018200970526     C                   MOVEL     '0'           *IN40
018300970526     C*
018400970526     C* VISUALIZZA IL MESSAGGIO
018500970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018600970526     C                   CALL      'XMSG'        XMSGPA
018700970526IF  1C     DSMSER        IFEQ      *ZEROS
018800970526     C                   MOVEL     DSMSKE        MSGKEY
018900970526E   1C                   ENDIF
019000970526     C                   Z-ADD     1             MSGULT
019100070122     C                   WRITE     DG10DMS
019200070122     C                   WRITE     DG10DMC
019300970526     C*
019400970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019500970526     C                   MOVEL     *BLANKS       DSMSMS
019600970526     C*
019700970526     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* ENTGES - GESTIONE ENTER -> INVIO
020000970526     C*------------------------------------------------------------------------*
020100970526     C     ENTGES        BEGSR
020200970526     C*
020300970526     C* CONTROLLA I CAMPI IMMESSI
020400970526     C                   EXSR      DECODI
020500970526     C*
020600970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
020700970526IF  1C     DEPOP1        IFEQ      'O05'
020800970526     C                   MOVEL     '1'           FINL00
020900970526E   1C                   ENDIF
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* F03GES - GESTIONE F03 -> FINE
021400970526     C*------------------------------------------------------------------------*
021500970526     C     F03GES        BEGSR
021600970526     C*
021700970526     C* IMPOSTA "PREMUTO F03"
021800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021900070124     C                   MOVEL     '1'           D09OF03                        *DI PROCEDURA
022000970526     C*
022100970526     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
022400970526     C*------------------------------------------------------------------------*
022500970526     C     F06GES        BEGSR
022600970526     C*
022700970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
022800970526IF  1C     DEPOP1        IFEQ      'F06'
022900970526     C                   EXSR      INSERI
023000970526E   1C                   ENDIF
023100970526IF  1C     DEPOP1        IFEQ      'O02'
023200970526     C                   EXSR      MODIFI
023300970526E   1C                   ENDIF
023400991014IF  1C     DEPOP1        IFEQ      'O03'
023500011129     C                   EXSR      COPIA
023600991014E   1C                   ENDIF
023700070123IF  1C     DEPOP1        IFEQ      'O04'
023800070123     C                   EXSR      ANNULL
023900070123E   1C                   ENDIF
024000970526IF  1C     DEPOP1        IFEQ      'O07'
024100970526     C                   EXSR      RIPRIS
024200970526E   1C                   ENDIF
024300011129     C*
024400970526     C                   ENDSR
024500070403     C*------------------------------------------------------------------------*
024600070403     C* F10GES - GESTIONE F10 -> RICERCA VERSIONI
024700070403     C*------------------------------------------------------------------------*
024800070403     C     F10GES        BEGSR
024900070403     C*
025000070403     C                   clear                   fidg11ds
025100070403     C                   eval      d11iop0 = 'P01'
025200070423     C                   if        VIDFGS  > *zeros
025300070423     C                   move(p)   VIDFGS        d11ifgs
025400070423     C                   endif
025500070403     C                   z-add     datcor        d11idda
025600070403     C                   eval      kpjbu = fidg11ds
025700070403     C                   call      'FIDG11R'
025800070403     C                   parm                    kpjba
025900070403     C                   eval      fidg11ds = kpjbu
026000070403     C                   if        d11oerr  =  *blanks AND
026100070403     C                             d11odde  <> *zeros
026200070403     C                   eval      VIDDDE   =  d11odde
026300070406     C                   if        VIDDSCAD = *zeros
026400070403     C                   eval      VIDDSCAD =  d11odsc
026500070406     C                   endif
026600070403     C                   endif
026700070403     C*
026800070403     C                   ENDSR
026900970526     C*------------------------------------------------------------------------*
027000970526     C* F12GES - GESTIONE F12 -> RITORNO
027100970526     C*------------------------------------------------------------------------*
027200970526     C     F12GES        BEGSR
027300970526     C*
027400970526     C* IMPOSTA "PREMUTO F12"
027500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
027600070124     C                   MOVEL     '1'           D09OF12                        *DI PROCEDURA
027700970526     C*
027800970526     C                   ENDSR
027900970526     C*------------------------------------------------------------------------*
028000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028100970526     C*------------------------------------------------------------------------*
028200970526     C     F24GES        BEGSR
028300970526     C*
028400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028500970526     C*
028600970526     C                   ENDSR
028700970526     C*------------------------------------------------------------------------*
028800970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
028900970526     C*------------------------------------------------------------------------*
029000970526     C     INSERI        BEGSR
029100970526     C*
029200970526     C* CONTROLLA I CAMPI IMMESSI
029300970526     C                   EXSR      DECODI
029400970526     C*
029500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
029600991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
029700070122     C                   clear                   figrg000
029800070122     C                   exsr      impgrg
029900070614     C                   if        vidFIT <> 'S'
030000070614     C                   eval      grgFTT =  'S'
030100070614     C                   endif
030200070122     C                   write     figrg000
030300070124     C                   movel     MSG(2)        D09OMSG                        *MESSAGGIO
030400970602     C*
030500970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
030600070122     C                   exsr      azzvid
030700070122E   1C                   endif
030800970526     C*
030900970526     C                   ENDSR
031000970526     C*------------------------------------------------------------------------*
031100970526     C* MODIFI - OPERAZIONI DI MODIFICA
031200970526     C*------------------------------------------------------------------------*
031300970526     C     MODIFI        BEGSR
031400970526     C*
031500970526     C* CONTROLLA I CAMPI IMMESSI
031600970526     C                   EXSR      DECODI
031700970526     C*
031800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
031900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032000070122     C                   EXSR      LETgrg
032100110624     c                   movel     dsgrg         oldgrg
032200110624     c* clearo flag trasmissione e falg centro storico
032300110624     c*  che non mi servono per decidere se scaricare a Tellus
032400110624     c                   clear                   o_grgftt
032500110624     c                   clear                   o_grgute
032600110624     c                   clear                   o_grgduv
032700110624     C                   movel     o_grgflo      DGRGFLO
032800110624     c                   clear                   §grgcsto
032900120402     c                   clear                   §grgfgm
033000120402     c                   movel     dgrgflo       o_grgflo
033100970526     C*
033200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
033300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033400070122     C                   EXSR      impgrg                                       *IMPOSTA I CAMPI
033500110624     c
033600070614     C                   if        vidFIT <> 'S'
033700110624     c* se ho modificato un dato sensibile allo scarico --> flaggo
033800110624     c                   movel     dsgrg         newgrg
033900110624     c* clearo flag trasmissione e falg centro storico
034000110624     c*  che non mi servono per decidere se scaricare a Tellus
034100110624     c                   clear                   n_grgftt
034200110624     c                   clear                   n_grgute
034300110624     c                   clear                   n_grgduv
034400110624     C                   movel     n_grgflo      DGRGFLO
034500110624     c                   clear                   §grgcsto
034600120402     c                   clear                   §grgfgm
034700110624     c                   movel     dgrgflo       n_grgflo
034800110624     c
034900110624     c                   if        oldgrg<>newgrg
035000070614     C                   eval      grgFTT =  'S'
035100070614     C                   endif
035200110624     c                   else
035300110624     c                   clear                   grgftt
035400110624     C                   endif
035500110624     c
035600070122     C                   UPDATE    figrg000
035700070124     C                   MOVEL     MSG(3)        D09OMSG
035800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035900970526E   2C                   ENDIF
036000970526E   1C                   ENDIF
036100970526     C*
036200970526     C                   ENDSR
036300991014     C*------------------------------------------------------------------------*
036400070122     C* COPIA  - OPERAZIONI DI COPIA
036500991014     C*------------------------------------------------------------------------*
036600070122     C     COPIA         BEGSR
036700991014     C*
036800991014     C*
036900991014     C                   ENDSR
037000970526     C*------------------------------------------------------------------------*
037100970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
037200970526     C*------------------------------------------------------------------------*
037300070122     C     ANNULL        BEGSR
037400991025     C*
037500991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
037600070122     C                   EXSR      LETgrg
037700991025     C*
037800991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
037900070122IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038000070122     C                   EVAL      grgATB = 'A'
038100070614     C                   if        vidFIT <> 'S'
038200070614     C                   eval      grgFTT =  'S'
038300070614     C                   endif
038400070122     C                   UPDATE    figrg000                                     *CANCELLA
038500070124     C                   MOVEL     MSG(4)        D09OMSG                        *MESSAGGIO
038600070122     C                   MOVEL     '1'           FINL00                         *FINE PGM
038700070122E   1C                   ENDIF
038800970526     C*
038900070122     C                   ENDSR
039000970526     C*------------------------------------------------------------------------*
039100970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
039200970526     C*------------------------------------------------------------------------*
039300970526     C     RIPRIS        BEGSR
039400070122     C*
039500070122     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
039600070122     C                   EXSR      LETgrg
039700070122     C*
039800070122     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
039900070122IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040000070122     C                   EVAL      grgATB = *blanks
040100070614     C                   if        vidFIT <> 'S'
040200070614     C                   eval      grgFTT =  'S'
040300070614     C                   endif
040400070122     C                   UPDATE    figrg000                                     *CANCELLA
040500070124     C                   MOVEL     MSG(5)        D09OMSG                        *MESSAGGIO
040600070122     C                   MOVEL     '1'           FINL00                         *FINE PGM
040700070122E   1C                   ENDIF
040800070122     C*
040900970526     C                   ENDSR
041000970526     C*------------------------------------------------------------------------*
041100070122     C* LETgrg - LEGGE IL RECORD PERL'AGGIORNAMENTO
041200970526     C*------------------------------------------------------------------------*
041300070122     C     LETgrg        BEGSR
041400970526     C*
041500070206     C                   IF        DEPOP1 <> 'F06'
041600070124     C                   Z-ADD     D09idde       K_grgdde
041700070124     C                   Z-ADD     D09ifgs       K_grgfgs
041800070124     C                   MOVEL     D09icgi       K_grgcgi
041900070206     C                   ENDIF
042000070122     C     KEYgrg01      CHAIN     figrg01l                           8081
042100070122IF  1C     *IN81         IFEQ      *ON                                          *ALLOCATO
042200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
042300070124     C                   MOVEL     MSG(7)        D09OMSG                        *MESSAGGIO
042400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
042500970526E   1C                   ENDIF
042600070122IF  1C     *IN80         IFEQ      *ON                                          *INSESISTENTE
042700070122     C                   IF        DEPOP1 <> 'F06'
042800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
042900070124     C                   MOVEL     MSG(6)        D09OMSG                        *MESSAGGIO
043000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
043100070122     C                   ENDIF
043200970526E   1C                   ENDIF
043300970526     C*
043400970526     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
043700970526     C*------------------------------------------------------------------------*
043800970526     C     DECODI        BEGSR
043900001120     C*
044000070403     C*--------------------
044100070403     C* CONTROLLO FILIALE
044200070403     C*--------------------
044300070403     C     ERRL00        IFEQ      '0'
044400070403     C     vidfgs        IFNE      *blanks
044500070403     C     vidfgs        ANDNE     *zeros
044600070403     C                   EXSR      cntpo
044700070403     C                   ELSE
044800070403     C                   MOVEL     '1'           ERRL00
044900070403     C                   MOVEL     MSG(12)       DSMSMS
045000070403     C                   SETON                                        72
045100070403     C                   ENDIF
045200070403     C                   ENDIF
045300070403     C*--------------------
045400070403     C* CONTROLLO ABILITAZIONE UTENTE ALLA FILIALE
045500070403     C*--------------------
045600070403     C     ERRL00        IFEQ      '0'
045700070403     C                   IF        VIDFGS <> *BLANKS AND
045800070403     C                             VIDFGS <> *ZEROS  AND
045900070403     C                             *IN94 = *OFF
046000170620     C****               CLEAR                   FNLV50DS
046100170620     C****               EVAL      D50TLA = 'L'
046200170620     C****               EVAL      D50PRU = KNMUS
046300170620     C****               EVAL      D50FGS = wvidfgs
046400170620     C****               CALL      'FNLV50R'
046500170620     C****               PARM                    FNLV50DS
046600170620     C****               IF        D50ERR <> *BLANKS
046700170620       //?Filiale in gestione all'utente
046800170620         IF  %lookup(%editc(wvidfgs:'X'):pog) = 0;
046900070403     C                   MOVEL     '1'           ERRL00
047000170620     C****               MOVEL     D50MSG        DSMSMS
047100170620           dsmsms = 'Filiale in gestione errata';
047200070403     C                   SETON                                        72
047300070403     C                   ENDIF
047400070403     C                   ENDIF
047500070403     C                   ENDIF
047600070403     C*--------------------
047700070403     C* CONTROLLO FILIALE ATTIVATA A "TELLUS"
047800070403     C*--------------------
047900070403     C     ERRL00        IFEQ      '0'
048000070403     C                   IF        VIDFGS <> *BLANKS AND
048100070403     C                             VIDFGS <> *ZEROS  AND
048200070403     C                             *IN94 = *OFF
048300070403     C     wvidfgs       CHAIN     azorg01l
048400070403     C                   IF        %found(azorg01l)
048500070403     C                   EVAL      OG148 = orgde8
048600070403     C                   ELSE
048700070403     C                   CLEAR                   OG148
048800070403     C                   ENDIF
048900070403     C*
049000070403     C                   IF        §OGTLLS = *BLANKS
049100070403     C                   MOVEL     '1'           ERRL00
049200070403     C                   MOVEL     MSG(18)       DSMSMS
049300070403     C                   SETON                                        72
049400070403     C                   ENDIF
049500070403     C                   ENDIF
049600070403     C                   ENDIF
049700070403     C*--------------------
049800070404     C* VERIFICA VERSIONI APERTE X LA FILIALE CORRENTE (SOLO IN INSERIMENTO)
049900070403     C*--------------------
050000070403     C     ERRL00        IFEQ      '0'
050100070404     C                   IF        DEPOP1 = 'F06'
050200070403     C                   EXSR      CNTVER
050300070403     C                   IF        wCntVer = *zeros
050400070403     C                   MOVEL     '1'           ERRL00
050500070403     C                   SETON                                        70
050600070403     C                   MOVEL     MSG(19)       DSMSMS
050700070403     C                   ENDIF
050800070403     C*
050900070403     C                   IF        wCntVer = 1
051000070403     C                   Z-ADD     wTopDdeVer    G08INV
051100070403     C                   Z-ADD     *ZEROS        G08DAT
051200070403     C                   MOVEL     '3'           G08ERR
051300070403     C                   CALL      'XSRDA8'
051400070403     C                   PARM                    WLBDA8
051500070403     C                   Z-ADD     G08DAT        viddde
051600070403     C                   Z-ADD     G08INV        wviddde
051700070420     C                   Z-ADD     wTopDscVer    G08INV
051800070420     C                   Z-ADD     *ZEROS        G08DAT
051900070420     C                   MOVEL     '3'           G08ERR
052000070420     C                   CALL      'XSRDA8'
052100070420     C                   PARM                    WLBDA8
052200070420     C                   Z-ADD     G08DAT        viddscad
052300070420     C                   Z-ADD     G08INV        wviddscad
052400070420     C                   SETON                                        93
052500070403     C                   ENDIF
052600070403     C*
052700070406     C                   IF        wCntVer > 1      AND
052800070406     C                             viddde  = *zeros
052900070403     C                   EXSR      F10GES
053000070404     C                   ENDIF
053100070403     C                   ENDIF
053200070403     C                   ENDIF
053300991026     C*--------------------
053400070403     C* DATA DECORRENZA
053500991026     C*--------------------
053600070122     C     *LIKE         DEFINE    viddde        wviddde
053700001120     C     ERRL00        IFEQ      '0'
053800070122     C                   Z-ADD     viddde        wda8
053900991026     C                   Z-ADD     *ZEROS        da8w
054000991026     C                   EXSR      CHKDA8
054100070122     C                   Z-ADD     WDA8          viddde
054200070122     C                   Z-ADD     DA8W          wviddde
054300001120     C     ERRL00        IFEQ      '1'
054400070122     C                   SETON                                        70
054500070122     C                   MOVEL     MSG(14)       DSMSMS
054600001120     C                   ENDIF
054700001120     C                   ENDIF
054800070403     C*--------------------
054900070403     C* DATA DECORRENZA - VERIFICA VERSIONE
055000070403     C*--------------------
055100070403     C     ERRL00        IFEQ      '0'
055200070403     C                   Z-ADD     wvidfgs       K_grvfgs
055300070403     C                   Z-ADD     wviddde       K_grvdde
055400070406     C     KEYgrv01_c    CHAIN     figrv01l
055500070403     C                   IF        %found(figrv01l)
055600070403     C                   EVAL      wTopDdeVer = grvdde
055700070403     C                   EVAL      wTopDscVer = grvdscad
055800070403     C                   ELSE
055900070403     C                   MOVEL     '1'           ERRL00
056000070403     C                   SETON                                        70
056100070403     C                   MOVEL     MSG(19)       DSMSMS
056200070403     C                   ENDIF
056300070403     C                   ENDIF
056400070122     C*--------------------
056500070122     C* DATA SCADENZA
056600070122     C*--------------------
056700070124     C     *LIKE         DEFINE    viddscad      wviddscad
056800070122     C     ERRL00        IFEQ      '0'
056900070124     C                   Z-ADD     viddscad      WDA8
057000070122     C                   Z-ADD     *ZEROS        DA8W
057100070122     C                   EXSR      CHKDA8
057200070124     C                   Z-ADD     WDA8          viddscad
057300070124     C                   Z-ADD     DA8W          wviddscad
057400070122     C     ERRL00        IFEQ      '1'
057500070122     C                   SETON                                        71
057600070122     C                   MOVEL     MSG(14)       DSMSMS
057700070122     C                   ENDIF
057800070122     C                   ENDIF
057900070403     C*--------------------
058000070403     C* DATA SCADENZA ENTRO LIMITI SCADENZA VERSIONE RELATIVA
058100070403     C*--------------------
058200070403     C     ERRL00        IFEQ      '0'
058300070403     C                   IF        wviddscad <= wTopDscVer
058400070403     C                   ELSE
058500070403     C                   MOVEL     '1'           ERRL00
058600070403     C                   SETON                                        71
058700070403     C                   EVAL      DSMSMS = %trim(MSG(20))+ ' ' +
058800070403     C                                      %editw(wTopDscVer:'    /  /  ')
058900070403     C                   ENDIF
059000070403     C                   ENDIF
059100070122     C*--------------------
059200070122     C* SCADENZA < DECORRENZA
059300070122     C*--------------------
059400070122     C     ERRL00        IFEQ      '0'
059500070124     C     wviddscad     IFLT      wviddde
059600070122     C                   SETON                                        7071
059700070122     C                   MOVEL     MSG(15)       DSMSMS
059800070122     C                   MOVEL     '1'           ERRL00
059900070122     C                   ENDIF
060000070122     C                   ENDIF
060100070122     C*--------------------
060200070122     C* SCADENZA >= DATA CORRENTE
060300070122     C*--------------------
060400070122     C     ERRL00        IFEQ      '0'
060500070122     C     DEPOP1        IFEQ      'O02'
060600070122     C     DEPOP1        OREQ      'O07'
060700070122     C     DEPOP1        OREQ      'F06'
060800070124     C     wviddscad     IFLT      DATCOR
060900070122     C                   SETON                                        71
061000070122     C                   MOVEL     MSG(16)       DSMSMS
061100070122     C                   MOVEL     '1'           ERRL00
061200070122     C                   ENDIF
061300070122     C                   ENDIF
061400070122     C                   ENDIF
061500070122     C*--------------------
061600070122     C* DECORRENZA + SCADENZA
061700070122     C*--------------------
061800070122     C* ACCAVALLAMENTO DATI
061900070122     C     ERRL00        IFEQ      '0'
062000070122     C     *start        SETLL     figrg01l
062100070122     C                   READ(N)   figrg01l
062200070122     C                   DOW       not %eof(figrg01l)
062300070123     C                   IF        grgfgs  = wvidfgs AND
062400070123     C                             grgcgi  = vidcgi  AND
062500070123     C                             grgdde <> wviddde
062600070124     C                   IF        (wviddde  <=grgdscad AND grgdde<=wviddde) OR
062700070124     C                             (wviddscad>=grgdde AND grgdscad >= wviddscad)
062800070122     C                   SETON                                        7071
062900070122     C                   MOVEL     MSG(17)       DSMSMS
063000070122     C                   MOVEL     '1'           ERRL00
063100070122     C                   LEAVE
063200070122     C                   ENDIF
063300070122     C                   ENDIF
063400070122     C                   READ(N)   figrg01l
063500070122     C                   ENDDO
063600070122     C                   ENDIF
063700070406     C*--------------------
063800070406     C* OBBLIGATORIETA CAMPI
063900070406     C*--------------------
064000070406     C     ERRL00        IFEQ      '0'
064100070406     C                   IF        viddde = *zeros
064200070406     C                   MOVEL     '1'           ERRL00
064300070406     C                   MOVEL     MSG(12)       DSMSMS
064400070406     C                   SETON                                        70
064500070406     C                   ENDIF
064600070406     C                   IF        viddscad = *zeros
064700070406     C                   MOVEL     '1'           ERRL00
064800070406     C                   MOVEL     MSG(12)       DSMSMS
064900070406     C                   SETON                                        71
065000070406     C                   ENDIF
065100070406     C                   IF        vidfgs = *blanks OR
065200070406     C                             vidfgs = *zeros
065300070406     C                   MOVEL     '1'           ERRL00
065400070406     C                   MOVEL     MSG(12)       DSMSMS
065500070406     C                   SETON                                        72
065600070406     C                   ENDIF
065700070406     C                   IF        vidcgi = *blanks
065800070406     C                   MOVEL     '1'           ERRL00
065900070406     C                   MOVEL     MSG(12)       DSMSMS
066000070406     C                   SETON                                        73
066100070406     C                   ENDIF
066200070406     C                   IF        viddes = *blanks
066300070406     C                   MOVEL     '1'           ERRL00
066400070406     C                   MOVEL     MSG(12)       DSMSMS
066500070406     C                   SETON                                        74
066600070406     C                   ENDIF
066700070406     C                   IF        vidain = *blanks
066800070406     C                   MOVEL     '1'           ERRL00
066900070406     C                   MOVEL     MSG(12)       DSMSMS
067000070406     C                   SETON                                        75
067100070406     C                   ENDIF
067200070406     C                   ENDIF
067300120402     c* flg giro micro solo se ambito SOTTO
067400120402     C                   IF        vidain <>'<' and vidfgm <> ' '
067500120402     c                             and ERRL00 = '0'
067600120402     C                   MOVEL     '1'           ERRL00
067700120402     C                   MOVEL     MSG(24)       DSMSMS
067800120402     C                   SETON                                        96
067900120402     C                   ENDIF
068000070426     C*--------------------
068100070426     C* SOLO CARATTERI AMMESSI
068200070426     C*--------------------
068300070426     C     ERRL00        IFEQ      '0'
068400070426     C* Codice giro
068500070426     C                   Z-ADD     *zeros        wLen              2 0
068600070426     C                   EVAL      wLen = %len(%trim(VIDCGI))
068700070426     C     1             DO        wLen          x                 2 0
068800070426     C                   MOVEL     *blanks       wrkCHAR           1
068900070426     C                   EVAL      wrkCHAR = %subst(VIDCGI:x:1)
069000070426     C     wrkCHAR       LOOKUP    SKcharOK                               20
069100070426     C                   IF        *in20 = *off or wrkCHAR = *blanks
069200070426     C                   MOVEL     '1'           ERRL00
069300070426     C                   MOVEL     MSG(21)       DSMSMS
069400070426     C                   SETON                                        73
069500070426     C                   ENDIF
069600070426     C                   ENDDO
069700070426     C* Descrizione giro
069800070426     C     1             DO        30            x                 2 0
069900070426     C                   MOVEL     *blanks       wrkCHAR           1
070000070426     C                   EVAL      wrkCHAR = %subst(VIDDES:x:1)
070100070426     C     wrkCHAR       LOOKUP    SKcharOK                               20
070200070426     C                   IF        *in20 = *off
070300070426     C                   MOVEL     '1'           ERRL00
070400070426     C                   MOVEL     MSG(21)       DSMSMS
070500070426     C                   SETON                                        74
070600070426     C                   ENDIF
070700070426     C                   ENDDO
070800070426     C*
070900070426     C                   ENDIF
071000070622     C*--------------------
071100070622     C* CONTROLLO TIPOLOGIA GIRO
071200070622     C*--------------------
071300070622     C     ERRL00        IFEQ      '0'
071400070622     C     VIDTUG        ANDEQ     ' '
071500070622     C
071600070622     C                   MOVEL     '1'           ERRL00
071700070622     C                   MOVEL     MSG(12)       DSMSMS
071800070622     C                   SETON                                        77
071900070622     C                   ENDIF
072000080124     C*--------------------
072100080124     C* IN CASO DI MODIFICA TIPOLOGIA GIRO DA CONSEGNA/ENTRAMBI  A SOLO RITIRO
072200080124     C*--------------------
072300080124     C     ERRL00        IFEQ      '0'
072400080514     C                   IF        VIDTUG = 'R' AND (§GRGTUG = 'C'
072500080514     C                             or §grgtug = 'E')
072600080124     C                   MOVEL     '1'           ERRL00
072700080124     C                   MOVEL     MSG(23)       DSMSMS
072800080124     C                   SETON                                        77
072900080124     C                   ENDIF
073000080124     C                   ENDIF
073100070622     C*--------------------
073200070622     C* IN CASO DI GIRO CON USO SOLO RITRO
073300070622     C*--------------------
073400070622     C     ERRL00        IFEQ      '0'
073500070622     C                   IF        VIDTUG = 'R' AND VIDFIT = ' '
073600070622     C                   MOVEL     '1'           ERRL00
073700070622     C                   MOVEL     MSG(22)       DSMSMS
073800070622     C                   SETON                                        78
073900070622     C                   ENDIF
074000070622     C                   ENDIF
074100070122     C*--------------------
074200991026     C* INSERIMENTO -> RECORD INESISTENTE
074300070122     C*--------------------
074400001120     C     ERRL00        IFEQ      '0'
074500001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
074600070206     C                   z-add     wviddde       K_grgdde
074700070124     C                   z-add     wvidfgs       K_grgfgs
074800070124     C                   movel     vidcgi        K_grgcgi
074900070122     C                   exsr      letgrg
075000070206     C                   if        *in80 = *off
075100070122     C                   movel     '1'           ERRL00
075200070122     C                   movel     MSG(8)        DSMSMS
075300070122     C                   seton                                        707172
075400070122     C                   seton                                        73
075500070122     C                   endif
075600070122     C                   ENDIF
075700070122     C                   ENDIF
075800001120     C*
075900001120     C                   ENDSR
076000991027     C*------------------------------------------------------------------------*
076100070122     C* CNTPO  - Controlla codice filiale
076200991027     C*------------------------------------------------------------------------*
076300070122     C     CNTPO         begsr
076400001120     C*
076500070122     C                   movel     *blanks       vddfgs
076600070122     C     '?'           scan      vidfgs                                 97
076700001120     C                   if        *in97
076800070122     C                   movel     *blanks       cod1
076900070122     C                   movel     *blanks       des1
077000070122     C                   movel     *blanks       pra1
077100001120     C                   call      'TNSD24R'
077200001120     C                   parm                    cod1              3
077300001120     C                   parm                    tip1              1
077400001120     C                   parm                    des1             25
077500001120     C                   parm                    pra1              3
077600070122     C                   movel     cod1          vidfgs
077700001120     C                   endif
077800070122     C                   move      vidfgs        cod1n             3 0
077900001120     C     cod1n         chain     azorg01l                           98
078000001120     C                   if        not *in98 and orgfag <>'V'
078100070123     C                   z-add     orgfil        wvidfgs           3 0
078200070122     C                   movel     orgdes        vddfgs
078300001120     C                   else
078400001120     C                   seton                                        69
078500001120     C                   move      '1'           errl00
078600001120     C                   movel     msg(13)       dsmsms
078700001120     C                   endif
078800001120     C*
078900001120     C                   endsr
079000991026     C*------------------------------------------------------------------------*
079100991026     C* CHKDA8 - CONTROLLA LA DATA (8)
079200991026     C*------------------------------------------------------------------------*
079300991026     C     CHKDA8        BEGSR
079400991026     C*
079500070122     C     *LIKE         DEFINE    viddde        WDA8
079600070122     C     *LIKE         DEFINE    viddde        DA8W
079700991026     C*
079800991026     C                   Z-ADD     WDA8          G08DAT
079900991026     C                   Z-ADD     *ZEROS        G08INV
080000991026     C                   MOVEL     '0'           G08ERR
080100991026     C                   CALL      'XSRDA8'
080200991026     C                   PARM                    WLBDA8
080300991026     C                   Z-ADD     G08DAT        WDA8
080400991026     C                   Z-ADD     G08INV        DA8W
080500991026IF  1C     G08ERR        IFEQ      '1'
080600991026     C                   MOVEL     '1'           ERRL00
080700991026E   1C                   ENDIF
080800991026     C*
080900991026     C                   ENDSR
081000970526     C*------------------------------------------------------------------------*
081100970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
081200970526     C*------------------------------------------------------------------------*
081300970526     C     IREVID        BEGSR
081400970526     C* CAMPI
081500070122     C                   Z-ADD     grgdde        G08INV
081600070122     C                   Z-ADD     *ZEROS        G08DAT
081700070122     C                   MOVEL     '3'           G08ERR
081800070122     C                   CALL      'XSRDA8'
081900070122     C                   PARM                    WLBDA8
082000070122     C                   Z-ADD     G08DAT        viddde
082100070122     C                   Z-ADD     G08INV        wviddde
082200070122     C*
082300070124     C                   Z-ADD     grgdscad      G08INV
082400070122     C                   Z-ADD     *ZEROS        G08DAT
082500070122     C                   MOVEL     '3'           G08ERR
082600070122     C                   CALL      'XSRDA8'
082700070122     C                   PARM                    WLBDA8
082800070124     C                   Z-ADD     G08DAT        viddscad
082900070124     C                   Z-ADD     G08INV        wviddscad
083000070122     C*
083100070122     C                   MOVE(P)   grgfgs        vidfgs
083200070123     C                   MOVEL     grgcgi        vidcgi
083300070122     C                   MOVEL     grgain        vidain
083400070122     C                   MOVEL     grgdes        viddes
083500070209     C                   MOVEL     grgflo        DGRGFLO
083600070209     C                   MOVEL     §GRGFIT       vidfit
083700070320     C                   MOVEL     §GRGDST       viddst
083800070320     C                   MOVEL     §GRGEDP       videdp
083900070621     C                   MOVEL     §GRGTUG       vidtug
084000110624     C                   MOVEL     §GRGcsto      vidcsto
084100120402     C                   MOVEL     §GRGfgm       vidfgm
084200070122     C                   MOVEL     grgact        vidact
084300070122     C                   MOVEL     grgute        vidute
084400070122     C*
084500070122     C                   Z-ADD     grgduv        G08INV
084600070122     C                   Z-ADD     *ZEROS        G08DAT
084700070122     C                   MOVEL     '3'           G08ERR
084800070122     C                   CALL      'XSRDA8'
084900070122     C                   PARM                    WLBDA8
085000070122     C                   Z-ADD     G08DAT        vidduv
085100070307     C*
085200070307     C* SE TRATTASI D GIRO ANNULLATO => EVIDENZIO SUBITO
085300070307     C                   IF        grgATB = 'A'
085400070307     C                   SETON                                        76
085500070320     C                   SETON                                        91
085600070307     C                   ENDIF
085700051122     C*
085800051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
085900070123     C                   IF        DEPOP1 = 'F06'
086000070201     C***                EVAL      viddde   = DATCORINV
086100070404     C***                EVAL      viddde   = 01012007
086200070404     C***                EVAL      viddscad = 31122039
086300051122     C                   ENDIF
086400070718     C*
086500070718     C* SE TRATTASI D GIRO IN GESTIONE SOLO UTENTI EDP* E UTENTE CORRENTE NO EDP* => SOLO VISUAL.
086600070718     C                   IF        *IN94  = *off AND
086700070718     C                             videdp = 'S'
086800070718     C                   SETON                                        90
086900070718     C                   SETON                                        91
087000070718     C                   ENDIF
087100020218     C*
087200970526     C                   ENDSR
087300970526     C*------------------------------------------------------------------------*
087400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
087500970526     C*------------------------------------------------------------------------*
087600970526     C     AZZVID        BEGSR
087700970526     C* CAMPI
087800070122     C                   MOVEA     *IN           wSavIn           99
087900070122     C                   CLEAR                   DG10D03
088000070122     C                   MOVEA     wSavIn        *IN
088100070709     C                   EVAL      VIDTUG  = 'E'
088200970526     C*
088300970526     C                   ENDSR
088400970526     C*------------------------------------------------------------------------*
088500070122     C* IMPGRG - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
088600970526     C*------------------------------------------------------------------------*
088700070122     C     IMPGRG        BEGSR
088800970526     C*
088900070122     C                   Z-ADD     viddde        G08dat
089000070122     C                   Z-ADD     *ZEROS        G08inv
089100070122     C                   MOVEL     '0'           G08ERR
089200070122     C                   CALL      'XSRDA8'
089300070122     C                   PARM                    WLBDA8
089400070122     C                   Z-ADD     G08inv        grgdde
089500070122     C*
089600070124     C                   z-add     viddscad      G08dat
089700070122     C                   Z-ADD     *ZEROS        G08inv
089800070122     C                   MOVEL     '0'           G08ERR
089900070122     C                   CALL      'XSRDA8'
090000070122     C                   PARM                    WLBDA8
090100070124     C                   Z-ADD     G08inv        grgdscad
090200070122     C*
090300070122     C                   move(p)   vidfgs        grgfgs
090400070123     C                   movel     vidcgi        grgcgi
090500070122     C                   movel     vidain        grgain
090600070209     C                   movel     viddes        grgdes
090700110624     C                   movel     vidcsto       §GRGcsto
090800120402     C                   movel     vidfgm        §GRGfgm
090900110624     C                   movel     vidfit        §GRGFIT
091000070320     C                   movel     viddst        §GRGDST
091100070320     C                   movel     videdp        §GRGEDP
091200070621     C                   movel     vidtug        §GRGTUG
091300070209     C                   movel     DGRGFLO       grgflo
091400070122     C                   z-add     datcor        grgduv
091500070122     C                   movel     knmus         grgute
091600970526     C*
091700970526     C                   ENDSR
091800970526     C*------------------------------------------------------------------------*
091900970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
092000970526     C*------------------------------------------------------------------------*
092100970526     C     IMPFUN        BEGSR
092200970526     C*
092300970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
092400970526IF  1C     DEPOP1        IFEQ      'O05'
092500970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
092600970526     C                   MOVEL     'N'           FUDATT
092700970526     C                   MOVEL     FUDRIG        FUD(6)
092800970526E   1C                   ENDIF
092900070404     C*
093000070404     C* SE IN INSERIMENTO ABILITA LA FUNZIONE DI RICERCA VERSIONI
093100070404IF  1C     DEPOP1        IFEQ      'F06'
093200070404     C                   MOVEL     FUD(10)       FUDRIG                         *AGGIORNAMENTO
093300070404     C                   MOVEL     'S'           FUDATT
093400070406     C                   MOVEL     FUDRIG        FUD(10)
093500070404E   1C                   ENDIF
093600970526     C*
093700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
093800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
093900970526     C                   EXSR      FUNGES
094000970526     C*
094100970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
094200970526     C*---
094300070122     C* 90 -> VISUALIZZAZIONE
094400070122     C* 91 -> MODIFICA
094500070122     C* 92 -> INSERIMENTO
094600970526     C*---
094700070123     C                   SETON                                        90
094800070123     C                   SETON                                        91
094900070123     C                   SETOFF                                       92
095000070122IF  1C     DEPOP0        IFEQ      'P03'                                        GESTIONE
095100070122     C     DEPOP0        OREQ      'P04'                                        GESTIONE FULL
095200970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
095300970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
095400970526     C                   SETOFF                                       91
095500070122     C                   SETON                                        92
095600070404     C                   SETOFF                                       93
095700970526E   2C                   ENDIF
095800970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
095900970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
096000970526E   2C                   ENDIF
096100970526E   1C                   ENDIF
096200970526     C*
096300970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
096400970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
096500070124     C                   Z-ADD     D09idde       K_grgdde
096600070124     C                   Z-ADD     D09ifgs       K_grgfgs
096700070124     C                   MOVEL     D09icgi       K_grgcgi
096800070122     C     KEYgrg01      CHAIN     figrg01l
096900070122IF  2C                   IF        not %found(figrg01l)                         *NON TROVATO
097000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
097100070122     C                   MOVEL     MSG(6)        DSMSMS
097200970526X   2C                   ELSE
097300970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
097400980128IF  3C     DEPOP1        IFEQ      'O05'
097500070122IF  3C     DEPOP1        OREQ      'O02'
097600980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
097700980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
097800980128E   3C                   ENDIF
097900970526E   2C                   ENDIF
098000970526X   1C                   ELSE                                                   *INSERIMENTO
098100970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
098200970526E   1C                   ENDIF
098300970526     C*
098400970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
098500970526IF  1C     DEPOP1        IFEQ      'O02'
098600970526     C                   MOVEL     DEC(1)        VIDFUN
098700970526E   1C                   ENDIF
098800991014IF  1C     DEPOP1        IFEQ      'O03'
098900991014     C                   MOVEL     DEC(6)        VIDFUN
099000991014E   1C                   ENDIF
099100970526IF  1C     DEPOP1        IFEQ      'O04'
099200970526     C                   MOVEL     DEC(2)        VIDFUN
099300970526E   1C                   ENDIF
099400970526IF  1C     DEPOP1        IFEQ      'O05'
099500970526     C                   MOVEL     DEC(3)        VIDFUN
099600970526E   1C                   ENDIF
099700970526IF  1C     DEPOP1        IFEQ      'O07'
099800970526     C                   MOVEL     DEC(4)        VIDFUN
099900970526E   1C                   ENDIF
100000970526IF  1C     DEPOP1        IFEQ      'F06'
100100970526     C                   MOVEL     DEC(5)        VIDFUN
100200970526E   1C                   ENDIF
100300970526     C*
100400970526     C                   ENDSR
100500070122     C*--------------------------------------------------------------------------------------------*
100600070122     C* REPERISCE I DATI UTENTE
100700070122     C*--------------------------------------------------------------------------------------------*
100800070122     C     REPDATIUTE    BEGSR
100900070122     C*
101000070122     C* INIZIALIZZA VARIABILI DI WRK
101100070122     C                   CLEAR                   TIBS34DS
101200070122     C                   CLEAR                   AZUTEDS
101300070122     C                   CLEAR                   DDATIUTE
101400070122     C*
101500070122     C     *DTAARA       DEFINE    §azute        azuteds
101600070122     C     *DTAARA       DEFINE    §datiute      ddatiute
101700070122     C                   IN(E)     *DTAARA
101800070122if  1C                   IF        %Error
101900070122     c                   EVAL      I34Tla = 'L'
102000070122     C                   CALL      'TIBS34R'
102100070122     C                   PARM                    Tibs34Ds
102200070122     C                   IN        *DTAARA
102300070122e   1C                   ENDIF
102400070122     C*
102500070122     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
102600070122     C                   MOVEL(P)  RSUT          DSFIRS           20
102700170620
102800170620      // Reperimento delle filiali gestibili dall'utente
102900170620        sav_kpjbu = kpjbu;
103000170620        clear TRUL31DS;
103100170620        IF  DUTlpo = '1';
103200170620          I31abi = 'RC';
103300170620        ELSE;
103400170620          I31abi = 'PO';
103500170620        ENDIF;
103600170620        I31cdi = DUTdis;
103700170620        I31car = DUTare;
103800170620        I31cpo = DUTpou;
103900170620        trul31r (kpjba:trul31ds);
104000170620        IF  O31pog <= *zeros;
104100170620          clear POG;
104200170620        ENDIF;
104300170620        kpjbu = sav_kpjbu;
104400070122     C*
104500070122     C                   ENDSR
104600070403     C*------------------------------------------------------------------------*
104700070403     C* CNTVER - Conta le versiono aperte di una filiale
104800070403     C*------------------------------------------------------------------------*
104900070403     C     CNTVER        begsr
105000070403     C*
105100070403     C                   z-add     *zeros        wCntVer           3 0
105200070403     C     wvidfgs       setll     figrv01l
105300070403     C                   if        %found(figrv01l)
105400070403     C                   dow       not %eof(figrv01l)
105500070403     C                   read      figrv01l
105600070403     C                   if        grvfgs = wvidfgs
105700070403     C                   if        datcor <= grvdscad
105800070403     C                   add       1             wCntVer
105900070403     C                   z-add     grvdde        wTopDdeVer        8 0
106000070403     C                   z-add     grvdscad      wTopDscVer        8 0
106100070403     C                   endif
106200070403     C                   else
106300070403     C                   leave
106400070403     C                   endif
106500070403     C                   enddo
106600070403     C                   endif
106700070403     C*
106800070403     C                   endsr
106900970526     C*------------------------------------------------------------------------*
107000970526     C* *INZSR - OPERAZIONI INIZIALI
107100970526     C*------------------------------------------------------------------------*
107200970526     C     *INZSR        BEGSR
107300070426     C*
107400070426     C                   movea     charOK        SKcharOK
107500970526     C*---
107600970526     C* RICEVIMENTO PARAMETRI
107700970526     C*---
107800970526     C     *ENTRY        PLIST
107900970612     C                   PARM                    KPJBA
108000070122     C                   MOVEL(P)  KPJBU         fidg09ds
108100070320     C*
108200070320     C* VERIFICO SUBITO SE TRATTASI D UTENTE EDP*
108300070320     C                   IF        %subst(KNMUS:1:3) = 'EDP'
108400070320     C                   SETON                                        94
108500070320     C                   ELSE
108600070320     C                   SETOFF                                       94
108700070320     C                   ENDIF
108800170329      * imposto sempre tipo utilizzo giro = E-ENTRAMBI
108900170329     c                   eval      vidtug = 'E'
109000070122     C*
109100070122     C* REPERIMENTO DATI UTENTE
109200070122     C                   EXSR      REPDATIUTE
109300970526     C*
109400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
109500970526     C     XMSGPA        PLIST
109600970526     C                   PARM                    DSMSMS
109700970526     C                   PARM                    DSMSPG
109800970526     C                   PARM                    DSMSKE
109900970526     C                   PARM                    DSMSER
110000991108     C*---
110100970526     C*
110200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
110300970526     C                   CALL      'XKEY'
110400970526     C                   PARM                    DSKEY
110500970526     C*---
110600970526     C* VARIABILI NON RIFERITE AL DATA BASE
110700970526     C*---
110800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
110900070124     C     *LIKE         DEFINE    D09IOP0       DEPOP0
111000070124     C     *LIKE         DEFINE    D09IOP1       DEPOP1
111100970526     C*---
111200070122     C* VARIABILI DI CONTROLLO
111300970526     C*---
111400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
111500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
111600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
111700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
111800970526     C*
111900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
112000070122     C                   MOVEL     'FIDG10R'     DSMSPG
112100070122     C                   MOVEL     'FIDG10R'     MSGQUE
112200970526     C                   MOVEL     *BLANKS       MSGKEY
112300970526     C                   MOVEL     *BLANKS       DSMSMS
112400970526     C*---
112500970526     C* VARIABILI RIFERITE AL DATA BASE
112600970526     C*---
112700070123     C     *LIKE         DEFINE    grgcgi        K_grgcgi
112800070123     C     *LIKE         DEFINE    grgdde        K_grgdde
112900070123     C     *LIKE         DEFINE    grgfgs        K_grgfgs
113000070403     C     *LIKE         DEFINE    grvfgs        K_grvfgs
113100070403     C     *LIKE         DEFINE    grvdde        K_grvdde
113200970526     C*---
113300970526     C* CHIAVI DI LETTURA
113400970526     C*---
113500070122     C* LETTURA figrg01l - COMPLETA
113600070122     C     KEYgrg01      KLIST
113700070122     C                   KFLD                    K_grgdde
113800070123     C                   KFLD                    K_grgfgs
113900070123     C                   KFLD                    K_grgcgi
114000070406     C* LETTURA figrv01l - COMPLETA
114100070406     C     KEYgrv01_c    KLIST
114200070403     C                   KFLD                    K_grvfgs
114300070403     C                   KFLD                    K_grvdde
114400970528     C*
114500970528     C* CALCOLA LA DATA CORRENTE
114600970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
114700970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
114800970528     C                   Z-ADD     WN8           G08DAT
114900051122     C                   Z-ADD     WN8           DATCORINV         8 0
115000970528     C                   Z-ADD     *ZEROS        G08INV
115100970528     C                   MOVEL     '0'           G08ERR
115200970528     C                   CALL      'XSRDA8'
115300970528     C                   PARM                    WLBDA8
115400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
115500970526     C*
115600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
115700070124     C                   MOVEL     D09IOP0       DEPOP0                         *LIVELLO PROCEDURA
115800070124     C                   MOVEL     D09IOP1       DEPOP1                         *LIVELLO PROGRAMMA
115900970526     C*
116000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
116100970526     C                   EXSR      IMPFUN
116200970526     C*
116300970526     C                   ENDSR
116400970526     C*------------------------------------------------------------------------*
116500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
116600970526     C*------------------------------------------------------------------------*
116700970526     C     FUNGES        BEGSR
116800970526     C*
116900970526     C* PULISCO CAMPI DI LAVORO
117000970526     C                   Z-ADD     0             W                 5 0
117100970526     C*
117200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
117300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
117400970526     C     1             DO        24            I
117500970526     C                   MOVEL     FUD(I)        FUDRIG
117600970526     C*
117700970526     C* PULIZIA FUNZIONI PRECEDENTI
117800970526     C                   MOVEL     *BLANKS       FUV(I)
117900970526     C*
118000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
118100970526     C* (SALTA F24)
118200970526     C     FUDATT        IFEQ      'S'
118300970526     C     FUDFUN        ANDNE     '24'
118400970526     C                   ADD       1             W
118500970526     C                   MOVEL     FUDDES        FUV(W)
118600970526     C                   END
118700970526     C                   END                                                    DO WHILE
118800970526     C*
118900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
119000970526     C* ALTRIMENTI DISATTIVO F24
119100970526     C     1             DO        24            I
119200970526     C                   MOVEL     FUD(I)        FUDRIG
119300970526     C*
119400970526     C     FUDFUN        IFEQ      '24'
119500970526     C     W             IFGT      6
119600970526     C                   MOVEL     'S'           FUDATT
119700970526     C                   ELSE
119800970526     C                   MOVEL     *BLANKS       FUDATT
119900970526     C                   END                                                    W GT 8
120000970526     C                   MOVEL     FUDRIG        FUD(I)
120100970526     C                   END
120200970526     C                   END                                                    DO WHILE
120300970526     C*
120400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
120500970526     C     W             DIV       6             I
120600970526     C                   MVR                     Z
120700970526     C*
120800970526     C     Z             IFNE      0
120900970526     C     I             ADD       1             Z
121000970526     C                   ELSE
121100970526     C                   Z-ADD     I             Z                 5 0
121200970526     C                   END
121300970526     C*
121400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
121500970526     C                   ADD       1             FUNGRU
121600970526     C     FUNGRU        IFGT      Z
121700970526     C     FUNGRU        ORLT      1
121800970526     C                   Z-ADD     1             FUNGRU
121900970526     C                   END
122000970526     C*
122100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
122200970526     C     FUNGRU        IFEQ      1
122300970526     C                   MOVEL     FUV(1)        FUN001
122400970526     C                   MOVEL     FUV(2)        FUN002
122500970526     C                   MOVEL     FUV(3)        FUN003
122600970526     C                   MOVEL     FUV(4)        FUN004
122700970526     C                   MOVEL     FUV(5)        FUN005
122800970526     C                   MOVEL     FUV(6)        FUN006
122900970526     C                   END
123000970526     C*
123100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
123200970526     C     FUNGRU        IFEQ      2
123300970526     C                   MOVEL     FUV(7)        FUN001
123400970526     C                   MOVEL     FUV(8)        FUN002
123500970526     C                   MOVEL     FUV(9)        FUN003
123600970526     C                   MOVEL     FUV(10)       FUN004
123700970526     C                   MOVEL     FUV(11)       FUN005
123800970526     C                   MOVEL     FUV(12)       FUN006
123900970526     C                   END
124000970526     C*
124100970526     C* TERZO GRUPPO DI 6 FUNZIONI
124200970526     C     FUNGRU        IFEQ      3
124300970526     C                   MOVEL     FUV(13)       FUN001
124400970526     C                   MOVEL     FUV(14)       FUN002
124500970526     C                   MOVEL     FUV(15)       FUN003
124600970526     C                   MOVEL     FUV(16)       FUN004
124700970526     C                   MOVEL     FUV(17)       FUN005
124800970526     C                   MOVEL     FUV(18)       FUN006
124900970526     C                   END
125000970526     C*
125100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
125200970526     C     FUNGRU        IFEQ      4
125300970526     C                   MOVEL     FUV(19)       FUN001
125400970526     C                   MOVEL     FUV(20)       FUN002
125500970526     C                   MOVEL     FUV(21)       FUN003
125600970526     C                   MOVEL     FUV(22)       FUN004
125700970526     C                   MOVEL     *BLANKS       FUN005
125800970526     C                   MOVEL     *BLANKS       FUN006
125900970526     C                   END
126000970526     C*
126100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
126200970526     C                   MOVEL     *BLANKS       FUN007
126300970526     C                   MOVEL     *BLANKS       FUN008
126400970526     C*
126500970526     C     1             DO        24            I
126600970526     C                   MOVEL     FUD(I)        FUDRIG
126700970526     C     FUDFUN        IFEQ      '24'
126800970526     C     FUDATT        ANDEQ     'S'
126900970526     C                   MOVEL     FUDDES        FUN008
127000970526     C                   END
127100970526     C                   END                                                    DO
127200970526     C*
127300970526     C                   ENDSR
127400970526     C*------------------------------------------------------------------------*
127500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
127600970526     C*------------------------------------------------------------------------*
127700970526     C     FUNCON        BEGSR
127800970526     C*
127900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
128000970526     C                   EXSR      FUNVAR
128100970526     C*
128200970526     C* PULISCO CAMPI DI LAVORO
128300970526     C                   Z-ADD     0             I                 5 0
128400970526     C                   Z-ADD     0             W                 5 0
128500970526     C*
128600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
128700970526     C     FUNKEY        IFNE      *BLANKS
128800970526     C     1             DO        24            I
128900970526     C                   MOVEL     FUD(I)        FUDRIG
129000970526     C*
129100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
129200970526     C     FUNKEY        IFEQ      FUDFUN
129300970526     C     FUDATT        ANDEQ     'S'
129400970526     C                   Z-ADD     I             W
129500970526     C                   END
129600970526     C                   END                                                    DO WHILE
129700970526     C*
129800970526     C* FUNZIONE NON ATTIVA (ERRORE)
129900970526     C     W             IFEQ      0
130000970526     C*
130100970526     C* EMISSIONE MESSAGGIO DI ERRORE
130200970526     C                   MOVEL     MSG(1)        DSMSMS
130300970526     C*
130400970526     C                   END                                                    W=0
130500970526     C                   END                                                    FUNKEY NE *BLANKS
130600970526     C*
130700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
130800970526     C     W             IFNE      0
130900970526     C     FUNKEY        OREQ      *BLANKS
131000970526     C     FUNKEY        IFEQ      *BLANKS
131100970526     C                   EXSR      ENTGES
131200970526     C                   END
131300970526     C     FUNKEY        IFEQ      '03'
131400970526     C                   EXSR      F03GES
131500970526     C                   END
131600970526     C     FUNKEY        IFEQ      '06'
131700970526     C                   EXSR      F06GES
131800970526     C                   END
131900070403     C     FUNKEY        IFEQ      '10'
132000070403     C                   EXSR      F10GES
132100070403     C                   END
132200970526     C     FUNKEY        IFEQ      '12'
132300970526     C                   EXSR      F12GES
132400970526     C                   END
132500970526     C     FUNKEY        IFEQ      '24'
132600970526     C                   EXSR      F24GES
132700970526     C                   END
132800970526     C                   END
132900970526     C*
133000970526     C                   ENDSR
133100970526     C*------------------------------------------------------------------------*
133200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
133300970526     C*------------------------------------------------------------------------*
133400970526     C     FUNVAR        BEGSR
133500970526     C*
133600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
133700970526     C*
133800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
133900970526     C     KEY           IFEQ      ENTER
134000970526     C                   MOVEL     *BLANKS       FUNKEY
134100970526     C                   END
134200970526     C     KEY           IFEQ      F1
134300970526     C                   MOVEL     '01'          FUNKEY
134400970526     C                   END
134500970526     C     KEY           IFEQ      F2
134600970526     C                   MOVEL     '02'          FUNKEY
134700970526     C                   END
134800970526     C     KEY           IFEQ      F3
134900970526     C                   MOVEL     '03'          FUNKEY
135000970526     C                   END
135100970526     C     KEY           IFEQ      F4
135200970526     C                   MOVEL     '04'          FUNKEY
135300970526     C                   END
135400970526     C     KEY           IFEQ      F5
135500970526     C                   MOVEL     '05'          FUNKEY
135600970526     C                   END
135700970526     C     KEY           IFEQ      F6
135800970526     C                   MOVEL     '06'          FUNKEY
135900970526     C                   END
136000970526     C     KEY           IFEQ      F7
136100970526     C                   MOVEL     '07'          FUNKEY
136200970526     C                   END
136300970526     C     KEY           IFEQ      F8
136400970526     C                   MOVEL     '08'          FUNKEY
136500970526     C                   END
136600970526     C     KEY           IFEQ      F9
136700970526     C                   MOVEL     '09'          FUNKEY
136800970526     C                   END
136900970526     C     KEY           IFEQ      F10
137000970526     C                   MOVEL     '10'          FUNKEY
137100970526     C                   END
137200970526     C     KEY           IFEQ      F11
137300970526     C                   MOVEL     '11'          FUNKEY
137400970526     C                   END
137500970526     C     KEY           IFEQ      F12
137600970526     C                   MOVEL     '12'          FUNKEY
137700970526     C                   END
137800970526     C     KEY           IFEQ      F13
137900970526     C                   MOVEL     '13'          FUNKEY
138000970526     C                   END
138100970526     C     KEY           IFEQ      F14
138200970526     C                   MOVEL     '14'          FUNKEY
138300970526     C                   END
138400970526     C     KEY           IFEQ      F15
138500970526     C                   MOVEL     '15'          FUNKEY
138600970526     C                   END
138700970526     C     KEY           IFEQ      F16
138800970526     C                   MOVEL     '16'          FUNKEY
138900970526     C                   END
139000970526     C     KEY           IFEQ      F17
139100970526     C                   MOVEL     '17'          FUNKEY
139200970526     C                   END
139300970526     C     KEY           IFEQ      F18
139400970526     C                   MOVEL     '18'          FUNKEY
139500970526     C                   END
139600970526     C     KEY           IFEQ      F19
139700970526     C                   MOVEL     '19'          FUNKEY
139800970526     C                   END
139900970526     C     KEY           IFEQ      F20
140000970526     C                   MOVEL     '20'          FUNKEY
140100970526     C                   END
140200970526     C     KEY           IFEQ      F21
140300970526     C                   MOVEL     '21'          FUNKEY
140400970526     C                   END
140500970526     C     KEY           IFEQ      F22
140600970526     C                   MOVEL     '22'          FUNKEY
140700970526     C                   END
140800970526     C     KEY           IFEQ      F23
140900970526     C                   MOVEL     '23'          FUNKEY
141000970526     C                   END
141100970526     C     KEY           IFEQ      F24
141200970526     C                   MOVEL     '24'          FUNKEY
141300970526     C                   END
141400970526     C     KEY           IFEQ      ROLLUP
141500970526     C                   MOVEL     'RU'          FUNKEY
141600970526     C                   END
141700970526     C     KEY           IFEQ      HELP
141800970526     C                   MOVEL     'HP'          FUNKEY
141900970526     C                   END
142000970526     C                   ENDSR
142100970526     C*------------------------------------------------------------------------*
142200970526     C* NOTE COSTRUZIONE SCHIERE
142300970526     C*------------------------------------------------------------------------*
142400970526     C*
142500970526     C* -MSG-MESSAGGI
142600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
142700970526     C* (NON TOCCARE MSG,1 E MSG,2)
142800970526     C*
142900970526     C* -CMD-COMANDI
143000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
143100970526     C*
143200970526     C* -OPD-OPZIONI
143300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
143400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
143500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
143600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
143700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
143800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
143900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
144000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
144100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
144200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
144300970526     C*
144400970526     C* -FUD-FUNZIONI
144500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
144600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
144700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
144800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
144900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
145000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
145100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
145200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
145300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
145400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
145500970526     O*------------------------------------------------------------------------*
145600970526** DEC - DECODIFICHE
145700970526    MODIFICA                                                      1
145800970526  ANNULLAMENTO                                                    2
145900970526VISUALIZZAZIONE                                                   3
146000970526   RIPRISTINO                                                     4
146100970526  INSERIMENTO                                                     5
146200991014     COPIA                                                        6
146300970526** MSG - MESSAGGI
146400070122FIDG10R- Tasto funzionale non gestito                                          1
146500070122FIDG10R- Inserimento effettuato                                                2
146600070122FIDG10R- Modifica effettuata                                                   3
146700070122FIDG10R- Annullamento effettuato                                               4
146800070122FIDG10R- Ripristino effettuato                                                 5
146900070122FIDG10R- Richiesta funzione per record inesistente                             6
147000070122FIDG10R- Record momentaneamente allocato: riprovare più tardi                  7
147100070122FIDG10R- Richiesta funzione per record esistente                               8
147200070122FIDG10R- Giro già presente con questo codice                                   9
147300070122FIDG10R- Giro già presente con questa descrizione                              10
147400070122FIDG10R- Codice giro fuori convenzione, indicare l'ambito nel codice           11
147500070122FIDG10R- Campo obbligatorio                                                    12
147600070122FIDG10R- Codice filiale inesistente o di categoria V=varia                     13
147700070122FIDG10R- Data errata                                                           14
147800070122FIDG10R- La data fine deve essere maggiore della data inizio                   15
147900070122FIDG10R- Il record non può scadere in un giorno già trascorso                  16
148000070122FIDG10R- Il record immesso si sovrappone ad uno già in decorrenza              17
148100070403FIDG10R- Gestione automatica giri consegna non ancora attiva sulla Filiale     18
148200070403FIDG10R- Nessuna versione attiva alla data decorrenza indicata (F10)           19
148300070403FIDG10R- Data scadenza deve essere inferiore o uguale al:                      20
148400070426FIDG10R- Presenti caratteri non validi, utilizzare solo numeri o lettere.      21
148500070622FIDG10R- I giri di solo ritiro devono essere fittizi (no su mappa)             22
148600080124FIDG10R- Errore modificare tipo utilizzo giro da consegna/entrambi a ritiro    23
148700120402FIDG10R- Flag giro MICRO ammesso solo se ambito SOTTO                          24
148800070122FIDG10R-                                                                       25
148900070122FIDG10R-                                                                       26
149000070122FIDG10R- Valori incompatibili, correggere!                                     27
149100970526** FUD - FUNZIONI
149200070201                   -        (S/N)=N                        funz.01¬
149300970526                   -        (S/N)=N                        funz.02¬
149400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
149500970526                   -        (S/N)=N                        funz.04¬
149600970526                   -        (S/N)=N                        funz.05¬
149700970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
149800970527                   -        (S/N)=N                        funz.07¬
149900970527                   -        (S/N)=N                        funz.08¬
150000970527                   -        (S/N)=N                        funz.09¬
150100070404F10=Ricerca vers.  -        (S/N)=N                        funz.10¬
150200970526                   -        (S/N)=N                        funz.11¬
150300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
150400970526                   -        (S/N)=N                        funz.13¬
150500970526                   -        (S/N)=N                        funz.14¬
150600970526                   -        (S/N)=N                        funz.15¬
150700970526                   -        (S/N)=N                        funz.16¬
150800970526                   -        (S/N)=N                        funz.17¬
150900970526                   -        (S/N)=N                        funz.18¬
151000970526                   -        (S/N)=N                        funz.19¬
151100970526                   -        (S/N)=N                        funz.20¬
151200970526                   -        (S/N)=N                        funz.21¬
151300970526                   -        (S/N)=N                        funz.22¬
151400970526                   -        (S/N)=N                        funz.23¬
151500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
