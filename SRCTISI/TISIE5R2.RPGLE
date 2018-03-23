000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020418      *------------------------------------------------------------------------*
000300170914      * STAMPA Legami Cod.Depot DPD/Cod.LEG
000400020418      *------------------------------------------------------------------------*
000500970911     F*------------------------------------------------------------------------*
000600970911     F* DATA BASE
000700970911     F*------------------------------------------------------------------------*
000800020531     fazorg01l  if   e           k disk
000900060919     Ftabel00f  IF   E           K DISK
001000170914     FDPLEG11L  IF   E           K DISK
001100170915     FDPCDP11i  IF   E           K DISK
001200170919     FTISIE5P   O    E             PRINTER OFLIND(*IN01)
001300060919     D*------------
001400060919     D* DS REPERIMENTO DATI UTENTE
001500060919     D*------------
001600060919     D TIBS34DS      E DS                                                       *Profili utente
001700060919     D DDATIUTE      E DS                                                       *Dati utente
001800060919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001900170918     D*------------------------------------------------------------------------*
002000170918     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
002100970911     D*----------------
002200970911     D* DS CONTROLLO DATE
002300970911     D*----------------
002400970911     D WLBDA8          DS
002500970911     D  G08DAT                 1      8  0
002600970911     D  G08INV                 9     16  0
002700970911     D  G08ERR                17     17
002800970911     D  G08TGI                18     22  0
002900971128     D*------------
003000060919     D* DS di procedura
003100971128     D*------------
003200170919     D tisie5ds      E DS
003300060919      * - Ds per Anagrafiche Clienti
003400060919     d TIBS69DS      E DS
003500060919     d DS_cnaco      E DS                  extname(CNACO00F)
003600060919     d DS_cnind      E DS                  extname(CNIND00F)
003700060919     d DS_cnclp      E DS                  extname(CNCLP00F)
003800060919     d DS_fncls      E DS                  extname(FNCLS00F)
003900970911     D*-------------------
004000970911     D* ARCHITETTURA
004100970911     D*-------------------
004200970911     D KPJBA         E DS
004300020531
004400060919     d og143         E DS
004500060919     d ds15          E ds
004600060919     d trul06ds      E DS
004700060919     D  LIN                    1     90  0 dim(30)
004800060919     D*-------------------
004900060919     D* VARIABILI DI WRK
005000060919     D*-------------------
005100060919     D ldp             S              3S 0 DIM(20)
005200060919     D skNaz           S              3    DIM(1000)
005300060920     d dataiso         s               d   datfmt(*iso)
005400060920     d dataeur         s               d   datfmt(*eur)
005500970911     C*------------------------------------------------------------------------*
005600970911     C* MAIN LINE
005700970911     C*------------------------------------------------------------------------*
005800970911     C*
005900970911     C* STAMPA I PARAMETRI DI LANCIO
006000970911     C                   EXSR      STAPAR
006100170914     c                   movel     de5dpt        LEGdpc
006200170915     C     LEGdep7       SETLL     DPLEG11l
006300060920     c                   do        *hival
006400170914     C                   read      DPLEG11l
006500170914     c                   if        %eof(dpLEG11l)
006600060920     c                   leave
006700060920     c                   endif
006800060920     c* record non valido se:
006900060920     c* 1) nazione deposito  non presente in schiera
007000060920     c* 2) decorrenza richiesta non inclusa nella decorrenza/scadenza
007100170914     c* 3) selezione tipologia deposito (con o senza cod. LEG) non congruent
007200060920     c*    col deposito letto
007300060920     c* nazione
007400060920     c                   if        %subst(knmus:1:3)<>'EDP'
007500170914     c     LEGnar        lookup    sknaz                                  98
007600060920     c                   if        *in98='0'
007700060920     c                   iter
007800060920     c                   endif
007900060920     c                   endif
008000060920     c* decorrenza
008100170914     c                   if        de5ddev >0 and (de5ddev < LEGdde or
008200170914     c                             de5ddev > LEGdsc)
008300060920     c                   iter
008400060920     c                   endif
008500170914     c* solo depositi senza codice LEG
008600170914     c                   if        de5sel='S' and LEGksc > 0
008700060920     c                   iter
008800060920     c                   endif
008900060920     c                   exsr      states
009000060920     c* stampo dati depot
009100060920     c                   exsr      riefld
009200060920     c                   write     sie5de
009300170914     c                   if        LEGksc>0
009400060920     c                   write     sie5dek
009500060920     c                   endif
009600060920     c
009700060920     c                   enddo
009800970911     C*
009900970911     C* STAMPA RIGA DI FINE STAMPA
010000060919     C                   WRITE     sie5FI
010100971128     C*
010200971128     C* OPERAZIONI FINALI
010300971128     C                   EXSR      FINSR
010400970911     C*
010500970911     C                   SETON                                        LR
010600060920     C*------------------------------------------------------------------------*
010700060920     C* riefld - Riempimento campi record dettaglio di stampa
010800060920     C*------------------------------------------------------------------------*
010900060920     C     riefld        BEGSR
011000060920     c                   clear                   pclisep
011100170914     c                   clear                   p_LEGksc
011200060920     c                   clear                   denam1
011300060920     c                   clear                   decty
011400060920     c                   clear                   deadr1
011500060920     c                   clear                   deptc
011600060920     c                   clear                   dedde
011700060920     c                   clear                   dedsc
011800170914     c                   if        LEGksc>0
011900060920     c                   movel     '/'           pclisep
012000170914     c                   move      LEGksc        p_LEGksc
012100060920     c                   endif
012200060920     c* decodifica del depot (denam1 e decty) solo se non è scaduto
012300170914    1c                   if        LEGdsc >= datcor
012400060920     c* recupero l'ultima versione di cappario
012500170918     C                   clear                   ISID1DRI
012600170918     C                   clear                   OSID1VER
012700170918     C                   clear                   OSID1VERD
012800170918     C                   clear                   OSID1DDE
012900170918     C                   clear                   OSID1DSC
013000170918     C                   clear                   OSID1BCID
013100170918     c                   call      'TISID1R'
013200170918     C                   PARM                    ISID1DRI
013300170918     C                   PARM                    OSID1VER
013400170918     C                   PARM                    OSID1VERD
013500170918     C                   PARM                    OSID1DDE
013600170918     C                   PARM                    OSID1DSC
013700170918     C                   PARM                    OSID1BCID
013800170918     c* se trovata versione reperisco  descrizioni deposito
013900170918    2c                   if        osid1ver>0
014000170915     c     keycdp        chain     dpcdp11i
014100170915    3c                   if        %found(dpcdp11i)
014200170915     c                   movel     cdpname1      denam1
014300170915     c                   movel     cdpcity       decty
014400170915     c                   movel     cdpaddr1      deadr1
014500060920     c                   movel     cdpptc        deptc
014600060920    3c                   endif
014700060920    2c                   endif
014800060920    1c                   endif
014900060920     c* decorrenza/scadenza
015000170914     c                   move      LEGdde        dataiso                        GG/MM/AAAA
015100060920     c                   move      dataiso       dataeur
015200060920     c                   move      dataeur       dedde
015300170914     c                   move      LEGdsc        dataiso                        GG/MM/AAAA
015400060920     c                   move      dataiso       dataeur
015500060920     c                   move      dataeur       dedsc
015600060920     c*
015700170914     c                   if        LEGksc > 0
015800060920     c                   clear                   derag
015900060920     c                   clear                   devia
016000060920     c                   clear                   decap
016100060920     c                   clear                   decit
016200060920     C                   clear                   TIBS69DS
016300170914     c                   z-add     LEGksc        I69kac
016400170914     c                   z-add     LEGksc        I69kin
016500060920     C                   call      'TIBS69R'
016600060920     C                   parm                    tibs69DS
016700060920     C                   parm                    DS_cnaco
016800060920     C                   parm                    DS_cnind
016900060920     C                   parm                    DS_cnclp
017000060920     C                   parm                    DS_fncls
017100060920    2c                   If         O69ERR <>  '1'
017200060920     c                   movel     ACOrag        derag
017300060920     c                   movel     indvia        devia
017400060920     c                   movel     indcap        decap
017500060920     c                   movel     indcit        decit
017600060920    2c                   Endif
017700060920     c
017800060920     c                   endif
017900060920     c
018000060920     c                   endsr
018100971128     C*------------------------------------------------------------------------*
018200971128     C* FINSR - OPERAZIONI FINALI
018300971128     C*------------------------------------------------------------------------*
018400971128     C     FINSR         BEGSR
018500971128     C*
018600060919     C                   CLEAR                   tibs69ds
018700060919     C                   eval      I69TLA  = 'C'
018800060919     C                   call      'TIBS69R'
018900060919     C                   parm                    TIBS69DS
019000971128     C*
019100971128     C                   ENDSR
019200970911     C*------------------------------------------------------------------------*
019300970911     C* STATES - STAMPA TESTATA
019400970911     C*------------------------------------------------------------------------*
019500970911     C     STATES        BEGSR
019600970911     C*
019700970911IF  1C     *IN01         IFEQ      *ON                                          *SE FINE PAGINA
019800060920     C                   WRITE     SIe5TE                                       *TESTATA
019900060920     C                   WRITE     SIe5IN                                       *INTESTAZIONE
020000970911     C                   SETOFF                                       01
020100970911E   1C                   ENDIF
020200970911     C*
020300970911     C                   ENDSR
020400970911     C*------------------------------------------------------------------------*
020500060920     C* STAPAR - STAMPA I PARAEMTRI DI LANCIO
020600970911     C*------------------------------------------------------------------------*
020700970911     C     STAPAR        BEGSR
020800970911     C*
020900970911     C* DECODIFICHE
021000060919     C                   Z-ADD     de5ddev       G08INV
021100970911     C                   Z-ADD     *ZEROS        G08DAT
021200970911     C                   MOVEL     '3'           G08ERR
021300970911     C                   CALL      'XSRDA8'
021400970911     C                   PARM                    WLBDA8
021500060919     C                   Z-ADD     G08DAT        PAdde
021600970911     C*
021700060919     C                   WRITE     SIE5TE                                       *TESTATA
021800060919     C                   WRITE     SIE5PA                                       *PARAMETRI
021900970911     C*
022000970911     C                   ENDSR
022100970911     C*------------------------------------------------------------------------*
022200970911     C* *INZSR - OPERAZIONI INIZIALI
022300970911     C*------------------------------------------------------------------------*
022400970911     C     *INZSR        BEGSR
022500970911     C*
022600970911     C* RICEVIMENTO PARAMETRI
022700970911     C     *ENTRY        PLIST
022800970911     C                   PARM                    KPJBA
022900170919     C                   PARM                    tisie5ds
023000970911     C*
023100970911     C* CHIAVI DI LETTURA
023200060919     c* TABEL00F - Parziale
023300060919     C     KEYtabel_P    KLIST
023400060919     C                   KFLD                    tblKUT
023500060919     C                   KFLD                    tblCOD
023600060920     C* DPCDP01L - Totale
023700060920     C     KEYcdp        KLIST
023800170915     C                   KFLD                    cdpATB
023900170918     C                   KFLD                    osid1ver
024000170915     C                   KFLD                    LEGdep7
024100970911     C*
024200970911     C* VARIABILI DI CONTROLLO
024300970911     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
024400970911     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
024500970911     C                   MOVEL     '1'           *IN01                          *OVERFLOW
024600060919     C* REPERIMENTO DATI UTENTE
024700060919     C                   EXSR      REPDATIUTE
024800060919     C* CARICA LE TABELLE OCCORRENTI
024900060919     C                   EXSR      CARTAB
025000971128     C*
025100971128     C* CALCOLA LA DATA CORRENTE
025200971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
025300971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
025400971128     C                   Z-ADD     WN8           G08DAT
025500971128     C                   Z-ADD     *ZEROS        G08INV
025600971128     C                   MOVEL     '0'           G08ERR
025700971128     C                   CALL      'XSRDA8'
025800971128     C                   PARM                    WLBDA8
025900971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
026000970911     C*
026100020531
026200970911     C*
026300970911     C                   ENDSR
026400060919     c*
026500060920     C*------------------------------------------------------------------------*
026600060920     C* cartab - caricamento tabelle
026700060920     C*------------------------------------------------------------------------*
026800060919     C     CARTAB        BEGSR
026900060919     C* CARICO TABELLA PUNTI OPERATIVI dell'area di partenza del p.o. utente
027000060919     C***
027100060919     C                   CLEAR                   trul06ds
027200060919     C                   MOVE      '£1'          D06COD
027300060919     C                   MOVEL     duttfp        D06KEY
027400060919     C                   MOVEL     'L'           D06TLA
027500060919     C                   MOVEL     'S'           D06esc
027600060919     C                   MOVEL     trul06ds      KPJBU
027700060919     C*
027800060919     C                   CALL      'TRUL06R'
027900060919     C                   PARM                    KPJBA
028000060919     C                   MOVEL     KPJBU         trul06ds
028100060919     c* carico la schiera dei p.o. dpd della £1
028200060919     c                   z-add     1             ii                2 0
028300060919     c                   z-add     0             xx                2 0
028400060919     c                   clear                   ldp
028500060919    1c                   dow       lin(ii) > *zeros  and
028600060919     c                             ii <= %elem(lin)
028700060919     c     lin(ii)       chain     azorg01l
028800060919    2c                   if        %found(azorg01l)
028900060919     c                   movel     orgde3        og143
029000060919    3c                   if        §ogntw = 'DPD'
029100060919     c                   add       1             xx
029200060919     c                   z-add     orgfil        ldp(xx)
029300060919    3c                   endif
029400060919    2c                   endif
029500060919     c                   add       1             ii
029600060919    1c                   enddo
029700060919     c* lettura tabella nazioni a caricamento delle nazioni servite dai p.o.
029800060919     c* dpd caricati
029900060919     c* Carico tutte le nazioni servite dai p.o. dpd dell'area  di partenza
030000060919     c                   z-add     0             yy                4 0
030100060919     C                   CLEAR                   skNaz
030200060919     C                   CLEAR                   DS15
030300060919     C                   EVAL      tblKUT = 1
030400060919     C                   EVAL      tblCOD = '15'
030500060919     c     keytabel_P    setll     tabel00f
030600060919    1c                   do        *hival
030700060919     c     keytabel_P    reade     tabel00f
030800060919    2c                   if        %eof(tabel00f)
030900060919     c                   leave
031000060919    2c                   endif
031100060919     c                   movel     tbluni        ds15
031200060919    2c                   if        §15lad > 0
031300060919     c     §15lad        lookup    ldp                                    99
031400060919    3c                   if        *in99
031500060919     c                   add       1             yy
031600060919     c                   movel     tblkey        sknaz(yy)
031700060919    3c                   endif
031800060919    2c                   endif
031900060919    1c                   enddo
032000060919     C*
032100060919     C                   ENDSR
032200060920     C*--------------------------------------------------------------------------------------------*
032300060920     C* REPERISCE I DATI UTENTE
032400060920     C*--------------------------------------------------------------------------------------------*
032500060920     C     REPDATIUTE    BEGSR
032600060920     C*
032700060920     C* INIZIALIZZA VARIABILI DI WRK
032800060920     C                   CLEAR                   TIBS34DS
032900060920     C                   CLEAR                   AZUTEDS
033000060920     C                   CLEAR                   DDATIUTE
033100060920     C*
033200060920     C     *DTAARA       DEFINE    §azute        azuteds
033300060920     C     *DTAARA       DEFINE    §datiute      ddatiute
033400060920     C                   IN(E)     *DTAARA
033500060920if  1C                   IF        %Error
033600060920     c                   EVAL      I34Tla = 'L'
033700060920     C                   CALL      'TIBS34R'
033800060920     C                   PARM                    Tibs34Ds
033900060920     C                   IN        *DTAARA
034000060920e   1C                   ENDIF
034100060920     C*
034200060920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
034300060920     C                   MOVEL(P)  RSUT          DSFIRS           20
034400060920     C*
034500060920     C                   ENDSR
