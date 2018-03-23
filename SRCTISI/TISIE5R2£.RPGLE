000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020418      *------------------------------------------------------------------------*
000300060919      * STAMPA Legami Cod.Depot DPD/Cod.PdC
000400020418      *------------------------------------------------------------------------*
000500970911     F*------------------------------------------------------------------------*
000600970911     F* DATA BASE
000700970911     F*------------------------------------------------------------------------*
000800020531     fazorg01l  if   e           k disk
000900060919     Ftabel00f  IF   E           K DISK
001000060919     FDPPDC01L  IF   E           K DISK
001100060919     FDPCDP01L  IF   E           K DISK
001200060919     FTISIE5P   O    E             PRINTER OFLIND(*IN01)
001300060919     D*------------
001400060919     D* DS REPERIMENTO DATI UTENTE
001500060919     D*------------
001600060919     D TIBS34DS      E DS                                                       *Profili utente
001700060919     D DDATIUTE      E DS                                                       *Dati utente
001800060919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001900970911     D*----------------
002000970911     D* DS CONTROLLO DATE
002100970911     D*----------------
002200970911     D WLBDA8          DS
002300970911     D  G08DAT                 1      8  0
002400970911     D  G08INV                 9     16  0
002500970911     D  G08ERR                17     17
002600970911     D  G08TGI                18     22  0
002700971128     D*------------
002800060919     D* DS di procedura
002900971128     D*------------
003000060919     D tisie5ds      E DS
003100060919      * - Ds per Anagrafiche Clienti
003200060919     d TIBS69DS      E DS
003300060919     d DS_cnaco      E DS                  extname(CNACO00F)
003400060919     d DS_cnind      E DS                  extname(CNIND00F)
003500060919     d DS_cnclp      E DS                  extname(CNCLP00F)
003600060919     d DS_fncls      E DS                  extname(FNCLS00F)
003700970911     D*-------------------
003800970911     D* ARCHITETTURA
003900970911     D*-------------------
004000970911     D KPJBA         E DS
004100020531
004200060919     d og143         E DS
004300060919     d ds15          E ds
004400060919     d trul06ds      E DS
004500060919     D  LIN                    1     90  0 dim(30)
004600060919     D tisie2ds      E DS
004700060919     D*-------------------
004800060919     D* VARIABILI DI WRK
004900060919     D*-------------------
005000060919     D ldp             S              3S 0 DIM(20)
005100060919     D skNaz           S              3    DIM(1000)
005200060920     d dataiso         s               d   datfmt(*iso)
005300060920     d dataeur         s               d   datfmt(*eur)
005400970911     C*------------------------------------------------------------------------*
005500970911     C* MAIN LINE
005600970911     C*------------------------------------------------------------------------*
005700970911     C*
005800970911     C* STAMPA I PARAMETRI DI LANCIO
005900970911     C                   EXSR      STAPAR
006000060920     c                   movel     de5dpt        pdcdpc
006100060920     C     pdcdpc        SETLL     DPpdc01l
006200060920     c                   do        *hival
006300060920     C                   read      DPpdc01l
006400060920     c                   if        %eof(dppdc01l)
006500060920     c                   leave
006600060920     c                   endif
006700060920     c* record non valido se:
006800060920     c* 1) nazione deposito  non presente in schiera
006900060920     c* 2) decorrenza richiesta non inclusa nella decorrenza/scadenza
007000060920     c* 3) selezione tipologia deposito (con o senza cod. PdC) non congruent
007100060920     c*    col deposito letto
007200060920     c* nazione
007300060920     c                   if        %subst(knmus:1:3)<>'EDP'
007400060920     c     pdcnar        lookup    sknaz                                  98
007500060920     c                   if        *in98='0'
007600060920     c                   iter
007700060920     c                   endif
007800060920     c                   endif
007900060920     c* decorrenza
008000060920     c                   if        de5ddev >0 and (de5ddev < pdcdde or
008100060920     c                             de5ddev > pdcdsc)
008200060920     c                   iter
008300060920     c                   endif
008400060920     c* solo depositi senza codice PdC
008500060920     c                   if        de5sel='S' and pdcksc > 0
008600060920     c                   iter
008700060920     c                   endif
008800060920     c                   exsr      states
008900060920     c* stampo dati depot
009000060920     c                   exsr      riefld
009100060920     c                   write     sie5de
009200060920     c                   if        pdcksc>0
009300060920     c                   write     sie5dek
009400060920     c                   endif
009500060920     c
009600060920     c                   enddo
009700970911     C*
009800970911     C* STAMPA RIGA DI FINE STAMPA
009900060919     C                   WRITE     sie5FI
010000971128     C*
010100971128     C* OPERAZIONI FINALI
010200971128     C                   EXSR      FINSR
010300970911     C*
010400970911     C                   SETON                                        LR
010500060920     C*------------------------------------------------------------------------*
010600060920     C* riefld - Riempimento campi record dettaglio di stampa
010700060920     C*------------------------------------------------------------------------*
010800060920     C     riefld        BEGSR
010900060920     c                   clear                   pclisep
011000060920     c                   clear                   p_pdcksc
011100060920     c                   clear                   denam1
011200060920     c                   clear                   decty
011300060920     c                   clear                   deadr1
011400060920     c                   clear                   deptc
011500060920     c                   clear                   dedde
011600060920     c                   clear                   dedsc
011700060920     c                   if        pdcksc>0
011800060920     c                   movel     '/'           pclisep
011900060920     c                   move      pdcksc        p_pdcksc
012000060920     c                   endif
012100060920     c* decodifica del depot (denam1 e decty) solo se non è scaduto
012200060920    1c                   if        pdcdsc >= datcor
012300060920     c* recupero l'ultima versione di cappario
012400060920     c                   clear                   tisie2ds
012500060920     c                   eval      sie2dri=*all'9'
012600060920     c                   call      'TISIE2R'
012700060920     c                   parm                    tisie2ds
012800060920     c* se trovata versione reperisco  descrizioni deposito
012900060920    2c                   if        sie2ver>0
013000060920     c     keycdp        chain     dpcdp01l
013100060920    3c                   if        %found(dpcdp01l)
013200060920     c                   movel     cdpnam1       denam1
013300060920     c                   movel     cdpcty        decty
013400060920     c                   movel     cdpadr1       deadr1
013500060920     c                   movel     cdpptc        deptc
013600060920    3c                   endif
013700060920    2c                   endif
013800060920    1c                   endif
013900060920     c* decorrenza/scadenza
014000060920     c                   move      pdcdde        dataiso                        GG/MM/AAAA
014100060920     c                   move      dataiso       dataeur
014200060920     c                   move      dataeur       dedde
014300060920     c                   move      pdcdsc        dataiso                        GG/MM/AAAA
014400060920     c                   move      dataiso       dataeur
014500060920     c                   move      dataeur       dedsc
014600060920     c*
014700060920     c                   if        pdcksc > 0
014800060920     c                   clear                   derag
014900060920     c                   clear                   devia
015000060920     c                   clear                   decap
015100060920     c                   clear                   decit
015200060920     C                   clear                   TIBS69DS
015300060920     c                   z-add     pdcksc        I69kac
015400060920     c                   z-add     pdcksc        I69kin
015500060920     C                   call      'TIBS69R'
015600060920     C                   parm                    tibs69DS
015700060920     C                   parm                    DS_cnaco
015800060920     C                   parm                    DS_cnind
015900060920     C                   parm                    DS_cnclp
016000060920     C                   parm                    DS_fncls
016100060920    2c                   If         O69ERR <>  '1'
016200060920     c                   movel     ACOrag        derag
016300060920     c                   movel     indvia        devia
016400060920     c                   movel     indcap        decap
016500060920     c                   movel     indcit        decit
016600060920    2c                   Endif
016700060920     c
016800060920     c                   endif
016900060920     c
017000060920     c                   endsr
017100971128     C*------------------------------------------------------------------------*
017200971128     C* FINSR - OPERAZIONI FINALI
017300971128     C*------------------------------------------------------------------------*
017400971128     C     FINSR         BEGSR
017500971128     C*
017600060919     C                   CLEAR                   tibs69ds
017700060919     C                   eval      I69TLA  = 'C'
017800060919     C                   call      'TIBS69R'
017900060919     C                   parm                    TIBS69DS
018000971128     C*
018100971128     C                   ENDSR
018200970911     C*------------------------------------------------------------------------*
018300970911     C* STATES - STAMPA TESTATA
018400970911     C*------------------------------------------------------------------------*
018500970911     C     STATES        BEGSR
018600970911     C*
018700970911IF  1C     *IN01         IFEQ      *ON                                          *SE FINE PAGINA
018800060920     C                   WRITE     SIe5TE                                       *TESTATA
018900060920     C                   WRITE     SIe5IN                                       *INTESTAZIONE
019000970911     C                   SETOFF                                       01
019100970911E   1C                   ENDIF
019200970911     C*
019300970911     C                   ENDSR
019400970911     C*------------------------------------------------------------------------*
019500060920     C* STAPAR - STAMPA I PARAEMTRI DI LANCIO
019600970911     C*------------------------------------------------------------------------*
019700970911     C     STAPAR        BEGSR
019800970911     C*
019900970911     C* DECODIFICHE
020000060919     C                   Z-ADD     de5ddev       G08INV
020100970911     C                   Z-ADD     *ZEROS        G08DAT
020200970911     C                   MOVEL     '3'           G08ERR
020300970911     C                   CALL      'XSRDA8'
020400970911     C                   PARM                    WLBDA8
020500060919     C                   Z-ADD     G08DAT        PAdde
020600970911     C*
020700060919     C                   WRITE     SIE5TE                                       *TESTATA
020800060919     C                   WRITE     SIE5PA                                       *PARAMETRI
020900970911     C*
021000970911     C                   ENDSR
021100970911     C*------------------------------------------------------------------------*
021200970911     C* *INZSR - OPERAZIONI INIZIALI
021300970911     C*------------------------------------------------------------------------*
021400970911     C     *INZSR        BEGSR
021500970911     C*
021600970911     C* RICEVIMENTO PARAMETRI
021700970911     C     *ENTRY        PLIST
021800970911     C                   PARM                    KPJBA
021900060919     C                   PARM                    tisie5ds
022000970911     C*
022100970911     C*
022200970911     C* CHIAVI DI LETTURA
022300060919     c* TABEL00F - Parziale
022400060919     C     KEYtabel_P    KLIST
022500060919     C                   KFLD                    tblKUT
022600060919     C                   KFLD                    tblCOD
022700060920     C* DPCDP01L - Totale
022800060920     C     KEYcdp        KLIST
022900060920     C                   KFLD                    sie2ver
023000060920     C                   KFLD                    pdcdpc
023100970911     C*
023200970911     C* VARIABILI DI CONTROLLO
023300970911     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
023400970911     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
023500970911     C                   MOVEL     '1'           *IN01                          *OVERFLOW
023600060919     C* REPERIMENTO DATI UTENTE
023700060919     C                   EXSR      REPDATIUTE
023800060919     C* CARICA LE TABELLE OCCORRENTI
023900060919     C                   EXSR      CARTAB
024000971128     C*
024100971128     C* CALCOLA LA DATA CORRENTE
024200971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
024300971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
024400971128     C                   Z-ADD     WN8           G08DAT
024500971128     C                   Z-ADD     *ZEROS        G08INV
024600971128     C                   MOVEL     '0'           G08ERR
024700971128     C                   CALL      'XSRDA8'
024800971128     C                   PARM                    WLBDA8
024900971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
025000970911     C*
025100020531
025200970911     C*
025300970911     C                   ENDSR
025400060919     c*
025500060920     C*------------------------------------------------------------------------*
025600060920     C* cartab - caricamento tabelle
025700060920     C*------------------------------------------------------------------------*
025800060919     C     CARTAB        BEGSR
025900060919     C* CARICO TABELLA PUNTI OPERATIVI dell'area di partenza del p.o. utente
026000060919     C***
026100060919     C                   CLEAR                   trul06ds
026200060919     C                   MOVE      '£1'          D06COD
026300060919     C                   MOVEL     duttfp        D06KEY
026400060919     C                   MOVEL     'L'           D06TLA
026500060919     C                   MOVEL     'S'           D06esc
026600060919     C                   MOVEL     trul06ds      KPJBU
026700060919     C*
026800060919     C                   CALL      'TRUL06R'
026900060919     C                   PARM                    KPJBA
027000060919     C                   MOVEL     KPJBU         trul06ds
027100060919     c* carico la schiera dei p.o. dpd della £1
027200060919     c                   z-add     1             ii                2 0
027300060919     c                   z-add     0             xx                2 0
027400060919     c                   clear                   ldp
027500060919    1c                   dow       lin(ii) > *zeros  and
027600060919     c                             ii <= %elem(lin)
027700060919     c     lin(ii)       chain     azorg01l
027800060919    2c                   if        %found(azorg01l)
027900060919     c                   movel     orgde3        og143
028000060919    3c                   if        §ogntw = 'DPD'
028100060919     c                   add       1             xx
028200060919     c                   z-add     orgfil        ldp(xx)
028300060919    3c                   endif
028400060919    2c                   endif
028500060919     c                   add       1             ii
028600060919    1c                   enddo
028700060919     c* lettura tabella nazioni a caricamento delle nazioni servite dai p.o.
028800060919     c* dpd caricati
028900060919     c* Carico tutte le nazioni servite dai p.o. dpd dell'area  di partenza
029000060919     c                   z-add     0             yy                4 0
029100060919     C                   CLEAR                   skNaz
029200060919     C                   CLEAR                   DS15
029300060919     C                   EVAL      tblKUT = 1
029400060919     C                   EVAL      tblCOD = '15'
029500060919     c     keytabel_P    setll     tabel00f
029600060919    1c                   do        *hival
029700060919     c     keytabel_P    reade     tabel00f
029800060919    2c                   if        %eof(tabel00f)
029900060919     c                   leave
030000060919    2c                   endif
030100060919     c                   movel     tbluni        ds15
030200060919    2c                   if        §15lad > 0
030300060919     c     §15lad        lookup    ldp                                    99
030400060919    3c                   if        *in99
030500060919     c                   add       1             yy
030600060919     c                   movel     tblkey        sknaz(yy)
030700060919    3c                   endif
030800060919    2c                   endif
030900060919    1c                   enddo
031000060919     C*
031100060919     C                   ENDSR
031200060920     C*--------------------------------------------------------------------------------------------*
031300060920     C* REPERISCE I DATI UTENTE
031400060920     C*--------------------------------------------------------------------------------------------*
031500060920     C     REPDATIUTE    BEGSR
031600060920     C*
031700060920     C* INIZIALIZZA VARIABILI DI WRK
031800060920     C                   CLEAR                   TIBS34DS
031900060920     C                   CLEAR                   AZUTEDS
032000060920     C                   CLEAR                   DDATIUTE
032100060920     C*
032200060920     C     *DTAARA       DEFINE    §azute        azuteds
032300060920     C     *DTAARA       DEFINE    §datiute      ddatiute
032400060920     C                   IN(E)     *DTAARA
032500060920if  1C                   IF        %Error
032600060920     c                   EVAL      I34Tla = 'L'
032700060920     C                   CALL      'TIBS34R'
032800060920     C                   PARM                    Tibs34Ds
032900060920     C                   IN        *DTAARA
033000060920e   1C                   ENDIF
033100060920     C*
033200060920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
033300060920     C                   MOVEL(P)  RSUT          DSFIRS           20
033400060920     C*
033500060920     C                   ENDSR
