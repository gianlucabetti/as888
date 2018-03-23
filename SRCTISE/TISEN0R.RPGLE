000100000000     H*------------------------------------------------------------------------*
000200091029     H* STATISTICA DI CONTROLLO NUMEROSITÀ LANCIO AZIONI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800091029     FTISEN0D   CF   E             WORKSTN INFDS(DEVDS1)
000900091029     F                                     SFILE(SEN0DVM:MSGULT)
001000091029     F                                     SFILE(SEN0DSF:NRR)
001100091029     Fkfazn11l  IF   E           K DISK
001200091103     Fkplog04l  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI, COMANDI E FUNZIONI
001800000000     D*-------------------
001900090901     D MSG             S             78    DIM(13) CTDATA PERRCD(1)             *MESSAGGI
002000000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002100000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002200091029     D
002300091029     D AZN             S              4    DIM(20) CTDATA PERRCD(20)
002400091029     D AZNp            S              4    DIM(20)
002500091030     D FRGp            S              3    DIM(20)
002600091030     D FRpp            S              3    DIM(20)
002700091030     D*-------------------
002800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002900000000     D*-------------------
003000000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003100000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003200000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003300000000     D*-------------------
003400000000     D* TABELLA DESCRIZIONI A VIDEO
003500000000     D*-------------------
003600000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003700000000     D*-------------------
003800000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003900000000     D*-------------------
004000000000     D KEYS            S              1    DIM(40)
004100000000     D*------------------------------------------------------------------------*
004200000000     D* INPUT
004300000000     D*------------------------------------------------------------------------*
004400000000     D*---------------------
004500000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004600000000     D*---------------------
004700000000     D DSFUN         E DS
004800000000     D  FUNRIG                 1     40
004900000000     D*---------------------
005000000000     D* DS PER GESTIONE TASTI FUNZIONALI
005100000000     D*---------------------
005200000000     D DSKEYS        E DS
005300000000     D  KEYRIG                 1     40
005400000000     D*---------------------
005500000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005600000000     D*---------------------
005700000000     D                SDS
005800000000     D  USER                 254    263
005900000000     D*---------------------
006000000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006100000000     D*---------------------
006200000000     D DEVDS1          DS
006300000000     D  KEY                  369    369
006400000000     D  CURSOR               370    371
006500000000     D*---------------------
006600000000     D* ARCHITETTURA
006700000000     D*---------------------
006800000000     D KPJBA         E DS
006900000000     D*---------------------
007000000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007100000000     D*---------------------
007200000000     D DSMSG         E DS
007300000000     D*---------------------
007400020917     D* DS REPERIMENTO DATI UTENTE
007500020917     D*-------------------
007600020917     D TIBS34DS      E DS                                                       *Profili utente
007700020917     D DDATIUTE      E DS                                                       *Dati utente
007800020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007900000000     D*---------------------
008000000000     D* DS PARAMETRI DI LANCIO
008100000000     D*------------------
008200091029     D TISEN0DS        DS                  INZ
008300000000     D  PARDTI                 1      8  0
008400000000     D  PARDTF                 9     16  0
008500091029     D  PARtipo               17     17
008600091029     D  PARAZN                18     97
008700091030     D  PARfrg                98    157
008800091030     D  PARfrp               158    217
008900000000     D*---------------------
009000000000     D* DS CONTROLLO DATA (8)
009100000000     D*------------------
009200000000     D WLBDA8          DS                  INZ
009300000000     D  G08DAT                 1      8  0
009400000000     D  G08INV                 9     16  0
009500000000     D  G08ERR                17     17
009600000000     D  G08TGI                18     22  0
009700091102     d
009800091102     d v1tipo          s              1
009900091103     d dataiso         s               d   datfmt(*iso)
010000091103     d data7           s              7  0
010100000000     C*------------------------------------------------------------------------*
010200000000     C* MAIN LINE
010300000000     C*------------------------------------------------------------------------*
010400090901     c                   setoff                                       87
010500000000     C*
010600000000     C* CICLO FINO A CHE:
010700000000DO  1C     $FINE         DOUEQ     '1'
010800000000     C*
010900000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
011000000000     C*
011100000000     C* CONTROLLA GESTIONE VIDEO
011200000000IF  2C     $GES          IFEQ      'V1'
011300000000     C                   EXSR      GESV1                                        *PARAMETRI
011400000000E   2C                   ENDIF
011500000000     C*
011600000000E   1C                   ENDDO
011700000000     C*
011800000000     C                   SETON                                        LR
011900000000     C*------------------------------------------------------------------------*
012000000000     C* GESV1 - GESTIONE VIDEO UNO
012100000000     C*------------------------------------------------------------------------*
012200000000     C     GESV1         BEGSR
012300000000     C*
012400000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
012500000000     C                   EXSR      CARV1                                        *CARICA VIDEO
012600000000E   1C                   ENDIF
012700000000     C*
012800000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012900000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
013000000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
013100000000E   1C                   ENDIF
013200000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
013300000000     C*
013400091029     C                   EXFMT     SEN0DCT                                      *PARAMETRI
013500000000     C*
013600000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013700000000     C*
013800000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013900000000SL  2C                   SELECT
014000000000     C     KEY           WHENEQ    ENTER
014100000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
014200091029     C     KEY           WHENEQ    F1
014300091029     C                   EXSR      F01GES                                       *CONFERMA
014400091029     C     KEY           WHENEQ    F6
014500091029     C                   EXSR      F06GES                                       *CONFERMA
014600091029     C     KEY           WHENEQ    F9
014700091029     C                   EXSR      F09GES                                       *GESTIONE skedula
014800000000     C     KEY           WHENEQ    F22
014900000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
015000000000     C                   OTHER
015100000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015200000000E   2C                   ENDSL
015300000000E   1C                   ENDIF
015400000000     C*
015500000000     C                   ENDSR
015600000000     C*------------------------------------------------------------------------*
015700000000     C* CARV1 - CARICA VIDEO UNO
015800000000     C*------------------------------------------------------------------------*
015900000000     C     CARV1         BEGSR
016000000000     C*
016100000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016200091029     c                   z-add     1             rec
016300000000     C*
016400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016600000000     C*
016700091029     C                   WRITE     SEN0DVT                                      *TESTATA
016800091029     C                   WRITE     SEN0DVF                                      *FUNZIONI
016900091029     C
017000000000     C                   ENDSR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
017300000000     C*------------------------------------------------------------------------*
017400000000     C     ENTV1         BEGSR
017500000000     C*
017600000000     C* CONTROLLA I CAMPI IMMESSI
017700000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
017800000000     C*
017900000000     C                   ENDSR
018000000000     C*------------------------------------------------------------------------*
018100000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
018200000000     C*------------------------------------------------------------------------*
018300000000     C     F06GES        BEGSR
018400000000     C*
018500000000     C* CONTROLLA I CAMPI IMMESSI
018600000000     C                   EXSR      DECODI
018700000000     C*
018800000000     C* LANCIA IL PROGRAMMA DI STAMPA
018900000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
019000091029     c                   clear                   parazn
019100091029     c                   clear                   parfrg
019200091029     c                   clear                   parfrp
019300091029     c                   clear                   aznp
019400091029     c                   clear                   frgp
019500091029     c                   clear                   frpp
019600091029     c
019700091029     c                   z-add     1             nrr               5 0
019800091029     c                   z-add     1             xx                3 0
019900091029    2c                   do        20            nrr
020000091029     c     nrr           chain     sen0dsf                            30
020100091029    3c  n30              if        v1sazn<>*blanks
020200091029     c                   eval      aznp(xx)=v1sazn
020300091029     c                   movel     v1sfrg        frgp(xx)
020400091029     c                   movel     v1sfrp        frpp(xx)
020500091029     c                   add       1             xx
020600091029     c                   endif
020700091029     c                   enddo
020800091029     c*
020900091029     c                   movea     aznp          parazn
021000091029     c                   movea     frgp          parfrg
021100091029     c                   movea     frpp          parfrp
021200091029     c
021300000000     C                   Z-ADD     DTIV1C        PARDTI
021400000000     C                   Z-ADD     DTFV1C        PARDTF
021500090601     C                   MOVE      V1tipo        PARtipo
021600000000     C*
021700091029     C                   MOVEL     'SEN1'        KCOAZ
021800091029     C                   MOVEL     TISEN0DS      KPJBU
021900091103     C                   CALL      'BCH10'
022000091103     C***                CALL      'TISEN1R'
022100000000     C                   PARM                    KPJBA
022200000000     C*
022300091029     C                   MOVEL     MSG(4)        $MSG                           *STAMPA LANCIATA
022400091029     C                   EXSR      CARMSG
022500090901     c
022600000000E   1C                   ENDIF
022700000000     C*
022800000000     C                   ENDSR
022900090901     C*------------------------------------------------------------------------*
023000091029     C* F01GES -conferma le scelte effettuate
023100090901     C*------------------------------------------------------------------------*
023200091029     C     F01GES        BEGSR
023300091029     C*
023400091029     C* CONTROLLA I CAMPI IMMESSI
023500091029     C                   EXSR      DECODI
023600091029     C*
023700091029     C* LANCIA IL PROGRAMMA DI STAMPA
023800091029IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
023900091029     c                   clear                   parazn
024000091029     c                   clear                   parfrg
024100091029     c                   clear                   parfrp
024200091029     c                   clear                   aznp
024300091029     c                   clear                   frgp
024400091029     c                   clear                   frpp
024500091029     c
024600091029     c                   z-add     1             nrr               5 0
024700091029     c                   z-add     1             xx                3 0
024800091029    2c                   do        20            nrr
024900091029     c     nrr           chain     sen0dsf                            30
025000091029    3c  n30              if        v1sazn<>*blanks  and v1sopz='1'
025100091029     c                   eval      aznp(xx)=v1sazn
025200091029     c                   movel     v1sfrg        frgp(xx)
025300091029     c                   movel     v1sfrp        frpp(xx)
025400091029     c                   add       1             xx
025500091029     c                   endif
025600091029     c                   enddo
025700091029     c*
025800091029     c                   movea     aznp          parazn
025900091029     c                   movea     frgp          parfrg
026000091029     c                   movea     frpp          parfrp
026100091029     c
026200091029     C                   Z-ADD     DTIV1C        PARDTI
026300091029     C                   Z-ADD     DTFV1C        PARDTF
026400091029     C                   MOVE      V1tipo        PARtipo
026500091029     C*
026600091029     C                   MOVEL     'SEN1'        KCOAZ
026700091029     C                   MOVEL     TISEN0DS      KPJBU
026800091103     C                   CALL      'BCH10'
026900091103     C***                CALL      'TISEN1R'
027000091029     C                   PARM                    KPJBA
027100091029     C*
027200091029     C                   MOVEL     MSG(4)        $MSG                           *STAMPA LANCIATA
027300091029     C                   EXSR      CARMSG
027400091029     c
027500091029E   1C                   ENDIF
027600091029     C*
027700091029     c
027800090901     C*
027900090901     C                   ENDSR
028000000000     C*------------------------------------------------------------------------*
028100000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
028200000000     C*------------------------------------------------------------------------*
028300000000     C     F22GES        BEGSR
028400000000     C*
028500000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
028600000000     C                   Z-ADD     80            LENGH            15 5
028700000000     C                   CALL      'QCAEXEC'
028800000000     C                   PARM                    CMD(1)
028900000000     C                   PARM                    LENGH
029000000000     C*
029100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029600000000     C*------------------------------------------------------------------------*
029700000000     C     DECODI        BEGSR
029800000000     C*
029900000000     C* REIMPOSTA INDICATORI DI ERRORE
030000000000     C                   SETOFF                                       606162
030100091029     C                   SETOFF                                       6364
030200000000     C*--------------------
030300000000     C* DATA INIZIO
030400000000     C*--------------------
030500000000     C     *LIKE         DEFINE    V1CDTI        DTIV1C
030600000000     C                   Z-ADD     *ZEROS        DTIV1C
030700091103
030800000000IF  1C     $ERR          IFEQ      '0'
030900000000     C                   Z-ADD     V1CDTI        WDA8
031000000000     C                   Z-ADD     *ZEROS        DA8W
031100000000     C                   EXSR      CHKDAT
031200000000     C                   Z-ADD     WDA8          V1CDTI
031300000000     C                   Z-ADD     DA8W          DTIV1C
031400000000IF  2C     $ERR          IFEQ      '1'
031500000000     C                   SETON                                        60
031600091029     C                   MOVEL     MSG(1)        $MSG
031700000000     C                   EXSR      CARMSG
031800000000E   2C                   ENDIF
031900000000E   1C                   ENDIF
032000091103     c* Verifico se ci sono dati nel kplog
032100091103IF  1C     $ERR          IFEQ      '0'
032200091103     c                   eval      dataiso=%date(dtiv1c:*iso)
032300091103     c                   eval      data7  =%dec(dataiso:*cymd)
032400091103     c     data7         chain     kplog04l
032500091103     c                   if        not %found(kplog04l)
032600091103     C                   SETON                                        60
032700091103     C                   MOVEL     MSG(7)        $MSG
032800091103     C                   MOVEL     '1'           $ERR
032900091103     C                   EXSR      CARMSG
033000091103     c                   endif
033100091103     c                   endif
033200000000     C*--------------------
033300000000     C* DATA FINE
033400000000     C*--------------------
033500000000     C     *LIKE         DEFINE    V1CDTF        DTFV1C
033600000000     C                   Z-ADD     *ZEROS        DTFV1C
033700000000IF  1C     $ERR          IFEQ      '0'
033800000000     C                   Z-ADD     V1CDTF        WDA8
033900000000     C                   Z-ADD     *ZEROS        DA8W
034000000000     C                   EXSR      CHKDAT
034100000000     C                   Z-ADD     WDA8          V1CDTF
034200000000     C                   Z-ADD     DA8W          DTFV1C
034300000000IF  2C     $ERR          IFEQ      '1'
034400000000     C                   SETON                                        61
034500091029     C                   MOVEL     MSG(1)        $MSG
034600000000     C                   EXSR      CARMSG
034700000000E   2C                   ENDIF
034800000000E   1C                   ENDIF
034900090601
035000090601     c* Data fine non può essere < data inizio
035100090601IF  1C     $ERR          IFEQ      '0'
035200091029    2c                   if        dtfv1c<dtiv1c
035300090601     C                   MOVEL     '1'           $ERR
035400090601     C                   SETON                                        61
035500091029     C                   MOVEL     MSG(1)        $MSG
035600090601     C                   EXSR      CARMSG
035700090601E   2C                   ENDIF
035800090601E   1C                   ENDIF
035900091103     c
036000091103     c* Verifico se ci sono dati nel kplog
036100091103IF  1C     $ERR          IFEQ      '0'
036200091103     c                   eval      dataiso=%date(dtfv1c:*iso)
036300091103     c                   eval      data7  =%dec(dataiso:*cymd)
036400091103     c     data7         chain     kplog04l
036500091103     c                   if        not %found(kplog04l)
036600091103     C                   SETON                                        61
036700091103     C                   MOVEL     MSG(7)        $MSG
036800091103     C                   MOVEL     '1'           $ERR
036900091103     C                   EXSR      CARMSG
037000091103     c                   endif
037100091103     c                   endif
037200091029     c
037300091029IF  1C     $ERR          IFEQ      '0'
037400091102     c**                 if        dtIv1c=dtfv1c and v1tipo='R'
037500091102     C**                 MOVEL     '1'           $ERR
037600091102     C**                 SETON                                        61
037700091102     C**                 MOVEL     MSG(3)        $MSG
037800091102     C**                 EXSR      CARMSG
037900091102E   2C**                 ENDIF
038000091029E   1C                   ENDIF
038100091029     c
038200091029IF  1C     $ERR          IFEQ      '0'
038300091102     c**                 if        dtFv1c>dtiv1c and v1tipo='G'
038400091102     C**                 MOVEL     '1'           $ERR
038500091102     C**                 SETON                                        61
038600091102     C**                 MOVEL     MSG(5)        $MSG
038700091102     C**                 EXSR      CARMSG
038800091102E   2C**                 ENDIF
038900091029E   1C                   ENDIF
039000091029c
039100091102     c* Se date uguali --> frequenza solo giornaliera
039200091102     c                   if        dtfv1c=dtiv1c
039300091102     c                   eval      v1tipo='G'
039400091102     c                   else
039500091102     c                   eval      v1tipo='R'
039600091102     c                   endif
039700091029     c* Se richiesta conferma totale, tutte le opzioni decono avere
039800091029     c*  la frequenza giornaliera e del periodo
039900091029    1c                   if        $err='0' and (*inkf or *inka)
040000091029     c                   clear                   wscelta           1
040100091029     c                   z-add     1             nrr               5 0
040200091029    2c                   do        20            nrr
040300091029     c     nrr           chain     sen0dsf                            30
040400091029    3c  n30              if        v1sazn<>*blanks
040500091029     c
040600091029    4c                   if        *inkf or *inka and v1sopz='1'
040700091029     c
040800091029     c* Se  effettuata una scelta --> ok
040900091029     c                   eval      wscelta='S'
041000091029     c
041100091029    5c                   if        v1sfrg=0
041200091029     C                   MOVEL     '1'           $ERR
041300091029     C                   SETON                                        62
041400091029     C                   MOVEL     MSG(2)        $MSG
041500091029     C                   EXSR      CARMSG
041600091029    5c                   endif
041700091029     c
041800091029    5c                   if        v1sfrp=0  and v1tipo='R'
041900091029     C                   MOVEL     '1'           $ERR
042000091029     C                   SETON                                        63
042100091029     C                   MOVEL     MSG(2)        $MSG
042200091029     C                   EXSR      CARMSG
042300091029    5c                   endif
042400091029     c
042500091029    4c                   endif
042600091029     c
042700091029     c                   update    sen0dsf
042800091029     C                   SETOff                                       626364
042900091029     c
043000091029    3c                   endif
043100091029    2c                   enddo
043200091029     c*
043300091029    2c                   if        Wscelta=' '
043400091029     c                   z-add     1             nrr
043500091029     c     nrr           chain     sen0dsf                            30
043600091029     C                   MOVEL     '1'           $ERR
043700091029     C                   SETON                                        64
043800091029     C                   MOVEL     MSG(6)        $MSG
043900091029     C                   EXSR      CARMSG
044000091029     c
044100091029     c                   update    sen0dsf
044200091029     C                   SETOff                                       6263
044300091029    2c                   endif
044400091029     c
044500091029    1c                   endif
044600000000     C*
044700000000     C                   ENDSR
044800000000     C*------------------------------------------------------------------------*
044900000000     C* CHKDAT - CONTROLLA LA DATA
045000000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
045100000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
045200000000     C*------------------------------------------------------------------------*
045300000000     C     CHKDAT        BEGSR
045400000000     C*
045500000000     C     *LIKE         DEFINE    V1CDTI        WDA8
045600000000     C     *LIKE         DEFINE    V1CDTI        DA8W
045700000000     C*
045800000000     C                   Z-ADD     *ZEROS        DA8W
045900000000     C*
046000000000     C                   Z-ADD     WDA8          G08DAT
046100000000     C                   Z-ADD     *ZEROS        G08INV
046200000000     C                   MOVEL     '0'           G08ERR
046300000000     C                   CALL      'XSRDA8'
046400000000     C                   PARM                    WLBDA8
046500000000     C                   Z-ADD     G08DAT        WDA8
046600000000IF  1C     G08ERR        IFEQ      '1'
046700000000     C                   MOVEL     '1'           $ERR
046800000000     C                   ELSE
046900000000     C                   Z-ADD     G08INV        DA8W
047000000000E   1C                   ENDIF
047100000000     C*
047200000000     C                   ENDSR
047300000000     C*------------------------------------------------------------------------*
047400000000     C* IMPVID - IMPOSTAZIONI VIDEO
047500000000     C*------------------------------------------------------------------------*
047600000000     C     IMPVID        BEGSR
047700000000     C*
047800090601     c                   MOVE      'R'           V1tipo                         *lancio
047900091029     C* PULIZIA SFL E CARICAMENTO DELLE AZIONI
048000091029     C*  DA CONTROLLARE
048100091029     C                   SETON                                        7273
048200091029     C                   WRITE     SEN0DCT
048300091029     C                   SETOFF                                       7273
048400091029     C*
048500091029     C                   Z-ADD     1             nrr
048600091029     C                   DOW       AZN(nrr)<>*BLANKS
048700091029     c     azn(nrr)      chain     kfazn11l
048800091029     c                   if        %found(kfazn11l)
048900091029     c                   movel     kdsaz         v1sdazn
049000091029     c                   else
049100091029     c                   eval      v1sdazn='?'
049200091029     c                   endif
049300091029     c                   eval      v1sazn=azn(nrr)
049400091030     c                   eval      v1sfrg=1
049500091030     c                   eval      v1sfrp=3
049600091030     c                   if        v1sazn='SE62'
049700091030     c                   eval      v1sfrg=10
049800091030     c                   eval      v1sfrp=100
049900091030     c                   endif
050000091029     c
050100091029     c                   write     sen0dsf
050200091029     c
050300091029     c                   add       1             nrr
050400091029     c                   enddo
050500091029     c
050600091029     c                   clear                   v1sfrg
050700091029     c                   clear                   v1sfrp
050800091029     c                   clear                   v1sopz
050900000000     C*
051000000000     C                   ENDSR
051100020917     C*--------------------------------------------------------------------------------------------*
051200020917     C* REPERISCE I DATI UTENTE
051300020917     C*--------------------------------------------------------------------------------------------*
051400020917     C     REPDATIUTE    BEGSR
051500020917     C*
051600020917     C* INIZIALIZZA VARIABILI DI WRK
051700020917     C                   CLEAR                   TIBS34DS
051800020917     C                   CLEAR                   AZUTEDS
051900020917     C                   CLEAR                   DDATIUTE
052000020917     C*
052100020917     C     *DTAARA       DEFINE    §azute        azuteds
052200020917     C     *DTAARA       DEFINE    §datiute      ddatiute
052300020917     C                   IN(E)     *DTAARA
052400020917if  1C                   IF        %Error
052500020917     c                   EVAL      I34Tla = 'L'
052600020917     C                   CALL      'TIBS34R'
052700020917     C                   PARM                    Tibs34Ds
052800020917     C                   IN        *DTAARA
052900020917e   1C                   ENDIF
053000020917     C*
053100020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
053200020917     C                   MOVEL(P)  RSUT          DSFIRS           20
053300020917     C*
053400020917     C                   ENDSR
053500000000     C*------------------------------------------------------------------------*
053600000000     C* *INZSR - ROUTINE INIZIALE
053700000000     C*------------------------------------------------------------------------*
053800000000     C     *INZSR        BEGSR
053900000000     C*--------------------
054000000000     C* RICEVIMENTO PARAMETRI
054100000000     C*--------------------
054200000000     C     *ENTRY        PLIST
054300000000     C                   PARM                    KPJBA
054400000000     C*--------------------
054500000000     C* VARIABILI DI CONTROLLO
054600000000     C*--------------------
054700000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
054800000000     C***
054900000000     C* IMPOSTAZIONI VIDEO
055000000000     C***
055100000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
055200000000     C*
055300000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
055400000000     C                   EXSR      INZSTD
055500000000     C*
055600000000     C* IMPOSTAZIONI VIDEO
055700000000     C                   EXSR      IMPVID
055800000000     C*
055900000000     C* EMETTE VIDEATA
056000000000     C                   EXSR      CARV1
056100000000     C*
056200000000     C                   ENDSR
056300000000     C*------------------------------------------------------------------------*
056400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
056500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
056600000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
056700000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
056800000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
056900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057100000000     C*------------------------------------------------------------------------*
057200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
057300000000     C*------------------------------------------------------------------------*
057400000000     C     INZSTD        BEGSR
057500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
057600000000     C                   CALL      'XKEYS'
057700000000     C                   PARM                    DSKEYS
057800000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
057900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
058000000000     C     XMSGPA        PLIST
058100000000     C                   PARM                    DSMSMS
058200000000     C                   PARM                    DSMSPG
058300000000     C                   PARM                    DSMSKE
058400000000     C                   PARM                    DSMSER
058500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
058600091029     C                   MOVEL     'TNSEN0R'     DSMSPG                         *NOME PGM CHIAMANTE
058700091029     C                   MOVEL     'TNSEN0R'     VMCQUE                         *CODA MESSAGGI DESTI
058800000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
058900000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
059000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
059100000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
059200000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
059300000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
059400000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
059500000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
059600000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
059700000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
059800000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
059900000000     C* LANCIO ROUTINE PER VERIFICARE SE
060000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
060100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
060200020917     C                   EXSR      REPDATIUTE
060300000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
060400000000     C                   BITOFF    '01234567'    POSMI1            1
060500000000     C                   BITOFF    '01234567'    POSMI2            1
060600000000     C                   BITOFF    '01234567'    POSMA1            1
060700000000     C                   BITOFF    '01234567'    POSMA2            1
060800000000     C                   BITON     '34'          POSMI1            1            *18
060900000000     C                   BITON     '7'           POSMI2            1            *01
061000000000     C                   BITON     '34'          POSMA1            1            *18
061100000000     C                   BITON     '0'           POSMA2            1            *80
061200000000     C                   MOVE      POSMI2        POSMIN            2
061300000000     C                   MOVEL     POSMI1        POSMIN
061400000000     C                   MOVE      POSMA2        POSMAX            2
061500000000     C                   MOVEL     POSMA1        POSMAX
061600000000     C                   ENDSR
061700000000     C*------------------------------------------------------------------------*
061800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
061900000000     C*------------------------------------------------------------------------*
062000000000     C     FUNSTD        BEGSR
062100000000     C                   SELECT
062200000000     C     KEY           WHENEQ    F3
062300000000     C                   EXSR      F03STD
062400000000     C     KEY           WHENEQ    F12
062500000000     C                   EXSR      F12STD
062600000000     C     KEY           WHENEQ    F24
062700000000     C                   EXSR      F24STD
062800000000     C                   OTHER
062900000000     C                   MOVEL     MSG(2)        $MSG
063000000000     C                   EXSR      CARMSG
063100000000     C                   ENDSL
063200000000     C                   ENDSR
063300000000     C*------------------------------------------------------------------------*
063400000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
063500000000     C*------------------------------------------------------------------------*
063600000000     C     F03STD        BEGSR
063700000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
063800000000     C                   MOVEL     '1'           $F03
063900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
064000000000     C                   MOVEL     '1'           $FINE
064100000000     C                   ENDSR
064200000000     C*------------------------------------------------------------------------*
064300000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
064400000000     C*------------------------------------------------------------------------*
064500000000     C     F12STD        BEGSR
064600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
064700000000     C                   MOVEL     '1'           $F12
064800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
064900000000     C                   MOVEL     '1'           $FINE
065000000000     C                   ENDSR
065100000000     C*------------------------------------------------------------------------*
065200000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
065300000000     C*------------------------------------------------------------------------*
065400000000     C     F24STD        BEGSR
065500000000     C*
065600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
065700000000     C                   EXSR      OUTFUN
065800000000     C*
065900000000     C                   ENDSR
066000090527     C*------------------------------------------------------------------------*
066100090527     C* F09GEs - GEST. TASTO FUNZ. F09          ==> skedulazione
066200090527     C*------------------------------------------------------------------------*
066300090527     C     F09GES        BEGSR
066400090527     C*
066500090527     C                   MOVE      '1'           KRITB
066600091029     C                   MOVEL     'SEN1'        KCOAZ
066700090527     c
066800090527     c                   call      'BCH09'
066900090527     c                   parm                    kpjba
067000090527     C*
067100090527     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
067200090527     C*
067300090527     C                   ENDSR
067400000000     C*------------------------------------------------------------------------*
067500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
067600000000     C*          CODA MESSAGGI DEL PGM
067700000000     C*------------------------------------------------------------------------*
067800000000     C     CARMSG        BEGSR
067900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
068000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
068100000000     C     MSGULT        IFEQ      0
068200000000     C                   MOVEL     '1'           *IN40
068300091029     C                   WRITE     SEN0DVK
068400000000     C                   MOVEL     '0'           *IN40
068500000000     C                   ENDIF
068600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
068700000000     C                   MOVEL     $MSG          DSMSMS
068800000000     C                   CALL      'XMSG'        XMSGPA
068900000000     C     DSMSER        IFEQ      0
069000000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
069100000000     C                   ENDIF
069200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
069300000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
069400091029     C                   WRITE     SEN0DVM                                      *SFL MESS
069500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
069600000000     C                   MOVEL     *BLANKS       $MSG
069700000000     C                   ENDSR
069800000000     C*------------------------------------------------------------------------*
069900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
070000000000     C*------------------------------------------------------------------------*
070100000000     C     OUTMSG        BEGSR
070200000000     C* VISUALIZZAZIONE MESSAGGIO
070300000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
070400091029     C                   WRITE     SEN0DVK                                      *CONTR MESS
070500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
070600000000     C                   Z-ADD     *ZEROS        MSGULT
070700000000     C                   ENDSR
070800000000     C*------------------------------------------------------------------------*
070900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
071000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
071100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
071200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
071300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
071400000000     C*------------------------------------------------------------------------*
071500000000     C     CARFUN        BEGSR
071600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
071700000000     C     1             DO        31            I                 5 0          --- 1 -->
071800000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
071900000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
072000000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
072100020917     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
072200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
072300000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
072400000000     C                   ELSE                                                   *INTERROG.
072500000000     C                   MOVEL     FUNASI        FAT(I)
072600000000     C                   ENDIF
072700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
072800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
072900000000     C                   MOVEL     FUNAFG        FAT(I)
073000000000     C                   ELSE                                                   *INTERROG.
073100000000     C                   MOVEL     FUNAFI        FAT(I)
073200000000     C                   ENDIF
073300000000     C                   ENDIF                                                  <-- 2 ---
073400000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
073500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
073600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
073700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
073800000000     C                   ELSE
073900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
074000000000     C                   ENDIF
074100000000     C                   ENDIF
074200000000     C                   ENDDO                                                  <-- 1 ---
074300000000     C                   ENDSR
074400000000     C*------------------------------------------------------------------------*
074500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
074600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
074700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
074800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
074900000000     C*------------------------------------------------------------------------*
075000000000     C     OUTFUN        BEGSR
075100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
075200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
075300000000     C                   ENDDO
075400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
075500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
075600000000     C     1             DO        31            I
075700000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
075800000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
075900000000     C                   ADD       1             FUNNAT
076000000000     C                   ENDIF
076100000000     C                   ENDDO
076200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
076300000000     C     FUNNAT        IFGT      8
076400000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
076500000000     C                   ENDIF
076600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
076700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
076800000000     C                   Z-ADD     8             FUNVIS            5 0
076900000000     C     FAT(23)       IFEQ      'S'
077000000000     C                   SUB       1             FUNVIS
077100000000     C                   ENDIF
077200000000     C     FAT(24)       IFEQ      'S'
077300000000     C                   SUB       1             FUNVIS
077400000000     C                   ENDIF
077500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
077600000000     C                   Z-ADD     *ZEROS        W                 5 0
077700000000     C                   ADD       1             FUNULT
077800000000     C     FUNULT        IFGT      31
077900000000     C                   Z-ADD     1             FUNULT
078000000000     C                   ENDIF
078100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
078200000000     C     FUNULT        DO        31            I                              --- 1 -->
078300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
078400000000     C     W             IFLT      FUNVIS                                       --- 2 -->
078500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
078600000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
078700000000     C     FDE(I)        ANDNE     *BLANKS
078800000000     C     I             IFEQ      23                                           *TOLGO F23
078900000000     C     I             OREQ      24                                           *E F24
079000000000     C                   ELSE
079100000000     C                   ADD       1             W
079200000000     C                   MOVEL     FDE(I)        FUV(W)
079300000000     C                   ENDIF
079400000000     C                   ENDIF                                                  <-- 3 ---
079500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
079600000000     C                   Z-ADD     I             FUNULT
079700000000     C                   ENDIF                                                  <-- 2 ---
079800000000     C                   ENDDO                                                  <-- 1 ---
079900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
080000000000     C                   MOVEL     FUV(1)        VFD001
080100000000     C                   MOVEL     FUV(2)        VFD002
080200000000     C                   MOVEL     FUV(3)        VFD003
080300000000     C                   MOVEL     FUV(4)        VFD004
080400000000     C                   MOVEL     FUV(5)        VFD005
080500000000     C                   MOVEL     FUV(6)        VFD006
080600000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
080700000000     C     FAT(24)       ANDEQ     'N'
080800000000     C                   MOVEL     FUV(7)        VFD007
080900000000     C                   MOVEL     FUV(8)        VFD008
081000000000     C                   ELSE                                                   <-- 1 -->
081100000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
081200000000     C     FAT(24)       ANDEQ     'S'
081300000000     C                   MOVEL     FDE(23)       VFD007
081400000000     C                   MOVEL     FDE(24)       VFD008
081500000000     C                   ELSE                                                   <-- 2 -->
081600000000     C                   MOVEL     FUV(7)        VFD007
081700000000     C     FAT(23)       IFEQ      'S'
081800000000     C                   MOVEL     FDE(23)       VFD008
081900000000     C                   ELSE
082000000000     C                   MOVEL     FDE(24)       VFD008
082100000000     C                   ENDIF
082200000000     C                   ENDIF                                                  <-- 2 ---
082300000000     C                   ENDIF                                                  <-- 1 ---
082400000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
082500000000     C                   ENDSR
082600000000     C*------------------------------------------------------------------------*
082700000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
082800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
082900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
083000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
083100000000     C*------------------------------------------------------------------------*
083200000000     C     CONFUN        BEGSR
083300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
083400000000     C     1             DO        31            I                              --- 1 -->
083500000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
083600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
083700000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
083800000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
083900000000     C                   EXSR      CARMSG
084000000000     C                   MOVEL     '1'           $ERR
084100000000     C                   ENDIF                                                  <-- 3 ---
084200000000     C                   ENDIF                                                  <-- 2 ---
084300000000     C                   ENDDO                                                  <-- 1 ---
084400000000     C                   ENDSR
084500000000     O*------------------------------------------------------------------------*
084600000000** MSG - (MESSAGGI)
084700091029TISEN0R- Data errata                                                           1
084800091029TISEN0R- Impostare la frequenza                                                2
084900091029TISEN0R- Se richiesta solo una data impossibile elaborare tipo "R"             3
085000091029TISEN0R- Stampa lanciata ....                                                  4
085100091029TISEN0R- Se richiesto range di date impossibile elaborare tipo "G"             5
085200091029TISEN0R- Per conferma con F1 effettuare almeno una scelta con opzione '1'      6
085300091103TISEN0R- Dati mancati o già puliti per la data richiesta                       7
085400000000** CMD  (COMANDI)
085500000000WRKSPLF
085600000000
085700000000
085800000000
085900000000
086000000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
086100091029F1    -F1=Conferma Scelte -    SS     SS
086200000000F2    -                   -    NN     NN
086300000000F3    -F3=Fine            -    SS     SS
086400000000F4    -                   -    NN     NN
086500000000F5    -                   -    NN     NN
086600091029F6    -F6=Conferma Totale -    SS     SS
086700000000F7    -                   -    NN     NN
086800091029F8    -                   -    NN     NN
086900090527F9    -F9=Gest.Skedulaz.  -    SS     SS
087000000000F10   -                   -    NN     NN
087100000000F11   -                   -    NN     NN
087200000000F12   -F12=Ritorno        -    SS     SS
087300000000F13   -                   -    NN     NN
087400000000F14   -                   -    NN     NN
087500000000F15   -                   -    NN     NN
087600000000F16   -                   -    NN     NN
087700091029F17   -                   -    NN     NN
087800000000F18   -                   -    NN     NN
087900000000F19   -                   -    NN     NN
088000000000F20   -                   -    NN     NN
088100000000F21   -                   -    NN     NN
088200000000F22   -F22=Gestione stampe-    EE     EE
088300000000F23   -                   -    NN     NN
088400000000F24   -F24=Altri tasti    -    NN     NN
088500000000ENTER -                   -    SS     SS
088600000000ROLLUP-                   -    NN     NN
088700000000ROLLDN-                   -    NN     NN
088800000000HELP  -                   -    NN     NN
088900000000PRINT -                   -    NN     NN
089000000000BSPACE-                   -    NN     NN
089100000000CLEAR -                   -    NN     NN
089200091029**
089300091030TA08MK13SE62
