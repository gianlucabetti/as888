000100000000     H*------------------------------------------------------------------------*
000200960528     H* SIMULAZIONE DELIVERY E TASSAZIONE - ESEGUE                             *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800970707     FTISI93D   CF   E             WORKSTN INFDS(DEVDS1)
000900970707     F                                     SFILE(SI93DVM:MSGULT)
001000050415     F                                     SFILE(si93ws1:NRR)
001100000000     FAZCLN01L  IF   E           K DISK
001200000000     FAZORG01L  IF   E           K DISK
001300000000     FTABEL00F  IF   E           K DISK
001400010823     FTNTBE01L  IF   E           K DISK
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* SCHIERE A TEMPO DI COMPILAZIONE PER GESTIONE VIDEO
002000000000     D*-------------------
002100000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002200000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZIONI
002300000000     D*-------------------
002400000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002500000000     D*-------------------
002600000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002700000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002800000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002900000000     D*-------------------
003000000000     D* TABELLA DESCRIZIONI A VIDEO
003100000000     D*-------------------
003200000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003300000000     D*-------------------
003400000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003500000000     D*-------------------
003600000000     D KEYS            S              1    DIM(40)
003700000000     D*-------------------
003800000000     D* SCHIERE A TEMPO DI COMPILAZIONE
003900000000     D*-------------------
004000091222     D DEC             S             25    DIM(13) CTDATA PERRCD(1)             *DECODIFICHE
004100000000     D DGIO            S             25    DIM(7) CTDATA PERRCD(1)              *DECODIFICHE GIORNI
004200960530     D ERR             S             80    DIM(5) CTDATA PERRCD(1)              *ERRORI DI ELABORAZI
004300110620     d
004400110620     D NarEEX          S              3    DIM(25) CTDATA PERRCD(25)            *ERRORI DI ELABORAZI
004500000000     D*-------------------
004600000000     D* SCHIERE PER DECODIFICA GIORNI DELLA SETTIMANA
004700000000     D*-------------------
004800000000     D GIO             S             25    DIM(7)                               *GIORNI DELLA SETTIM
004900000000     D MES             S              1    DIM(31)                              *GIORNI DEL MESE
005000951027     D*-------------------
005100971009     D* SCHIERA NAZIONI
005200951027     D*-------------------
005300011218     D NAR             S              3    DIM(500)                             *NAZIONE
005400020514     D NARLAD          S              3  0 DIM(500)                             *LNA DPD X NAZIONE
005500000000     D*------------------------------------------------------------------------*
005600000000     D* INPUT
005700000000     D*------------------------------------------------------------------------*
005800050608     D Wvalore         S             11  3                                      *LNA DPD X NAZIONE
005900000000     D*-------------------
006000000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006100000000     D*-------------------
006200000000     D DSFUN         E DS
006300000000     D  FUNRIG                 1     40
006400000000     D*-------------------
006500000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006600000000     D*-------------------
006700000000     D DSMSG         E DS
006800000000     D*-------------------
006900000000     D* DS PER GESTIONE TASTI FUNZIONALI
007000000000     D*-------------------
007100000000     D DSKEYS        E DS
007200000000     D  KEYRIG                 1     40
007300000000     D*-------------------
007400000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007500000000     D*-------------------
007600000000     D                SDS
007700000000     D  USER                 254    263                                         UTENTE
007800000000     D*-------------------
007900000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008000000000     D*-------------------
008100000000     D DEVDS1          DS
008200000000     D  KEY                  369    369
008300000000     D  CURSOR               370    371
008400000000     D  FRTSFL               378    379B 0
008500970704     D*------------------
008600970704     D* DS CONTROLLO DATE (8)
008700970704     D*------------------
008800970704     D WLBDA8          DS                  INZ
008900970704     D  G08DAT                 1      8  0
009000970704     D  G08INV                 9     16  0
009100970704     D  G08ERR                17     17
009200970704     D  G08TGI                18     22  0
009300970704     D*------------------
009400970704     D* SCOMPOSIZIONE DATA SPEDIZIONE (A/M/G)
009500970704     D*------------------
009600970704     D                 DS
009700970704     D  DSPAAS                 1      4  0
009800970704     D  DSPMES                 5      6  0
009900970704     D  DSPGIO                 7      8  0
010000970704     D  DSPV1C                 1      8  0
010100970704     D*------------------
010200970704     D* SCOMPOSIZIONE DATA CALCOLATA (G/M/A)
010300970704     D*------------------
010400970704     D                 DS
010500970704     D  DEEGIO                 1      2  0
010600970704     D  DEEMES                 3      4  0
010700970704     D  DEEANN                 5      8  0
010800970704     D  V1CDEE                 1      8  0
010900131016
011000131016      * Sk per filiali abilitate a Orari di Servizio
011100131016     d FilOkOrari      ds           256
011200131016     d  skFilOkOrari           1    255    dim(85)
011300131016
011400970704     D*-------------------
011500020919     D* DS REPERIMENTO DATI UTENTE
011600970704     D*-------------------
011700020919     D TIBS34DS      E DS                                                       *Profili utente
011800020919     D DDATIUTE      E DS                                                       *Dati utente
011900020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012000030619
012100030619     d TIBS69DS      E DS                  INZ
012200030619     d DS_cnaco      E DS                  extname(CNACO00F)
012300030619     d DS_cnind      E DS                  extname(CNIND00F)
012400030619     d DS_cnclp      E DS                  extname(CNCLP00F)
012500030619     d DS_fncls      E DS                  extname(FNCLS00F)
012600030619
012700970704     D*------------------
012800970704     D* DS NAZIONI
012900970704     D DS15          E DS
013000110623     D*------------------
013100110623     D* DS tipi inoltro
013200110623     D DS9A          E DS
013300050415     D*------------------
013400050415     D* DS sigle varie
013500050415     D DS$2          E DS
013600970704     D*------------------
013700970704     D* DS TIPI SERVIZIO
013800970704     D DS5E          E DS
013900991123     D*------------------
014000991123     D* DS GESTIONALE: IMPORTI VARI
014100991123     D DGEI          E DS
014200000623     D*------------------
014300000623     D* DS LETTURA CAMPO ORGDE3
014400000623     D OG143         E DS
014500000000     D*-------------------
014600000000     D* DS DI PROCEDURA
014700970707     D DSSD92        E DS                  EXTNAME(TISI92DS)
014800970704     D*-------------------
014900970704     D* DS "TISI95R" - CONTROLLA CAP
015000970704     D DSSI95        E DS                  EXTNAME(TISI95DS)
015100000000     D*-------------------
015200971009     D* DS "TNSD99R" - CALCOLO DELIVERY
015300971009     D DSSD99        E DS                  EXTNAME(TNSD99DS)
015400050428      *
015500050428     D*-------------------
015600050428     D* DS PER FNLV16R - DS DEI CAMPI DI TASSAZIONE
015700050428     D DSLV16        E DS                  EXTNAME(FNLV16DS)
015800960528     D*-------------------
015900960528     D* DS "TNSF20R" - TASSAZIONE SINGOLA BOLLA
016000110818     D DTAS01        E DS
016100991123     D DTAS          E DS
016200991123     D DTASV         E DS
016300050415     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
016400020813     D  VAR                   21    140P 3 DIM(20)
016500960528     D*-------------------
016600991123     D* DS "FNLV04R" - CALCOLO TOTALE FATTURA
016700960528     D*-------------------
016800991123     D DSLV04        E DS                  EXTNAME(FNLV04DS)
016900971020     D*------------------
017000971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
017100971020     D*------------------
017200971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
017300991124     D*-------------------
017400991124     D* DS "TIBS02R" - RICERCA TABELLA
017500020225     D*-------------------
017600991124     D DSBS02        E DS                  EXTNAME(TIBS02DS)
017700010823     D*------------------
017800010823     D* DS REPERIMENTO DIVISE DI CONTO
017900010823     D*-------------------
018000010823     D DGED          E DS
018100020225     D*-------------------
018200020225     D* DS DEFINIZIONE TABELLA "NETWORK"
018300020225     D*-------------------
018400020225     D DNTW          E DS
018500970704     D*-------------------
018600970704     D* ARCHITETTURA
018700970704     D*-------------------
018800970704     D KPJBA         E DS
018900131016
019000131016      // - DS per interrogazione Orari Servizio
019100140203     d TRULORSds     e ds
019200091222     d
019300091222     d Wttc            s              5
019400091222     d WttcCAP         s              5
019500091222     d C               s              3  0
019600110620     d Indx            s              3  0
019700131016
019800131016      //---------------------------------------------------------------
019900131016      //?Definizione procedure utilizzate.
020000131016      //---------------------------------------------------------------
020100131016
020200131016      // -Interrogazione orari di servizio
020300140203     d trulorsr        pr                  extpgm('TRULORSR')
020400131016     d  kpjba                              likeds(KPJBA)
020500140203     d  trulorSds                          likeds(TRULORSDS)
020600131016
020700000000     C*------------------------------------------------------------------------*
020800000000     C* MAIN LINE
020900000000     C*------------------------------------------------------------------------*
021000000000     C*
021100000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
021200000000IF  1C     $FINE         IFEQ      '1'
021300000000     C*
021400000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
021500000000     C                   MOVEL     '1'           D92ERR
021600000000     C*
021700000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
021800000000X   1C                   ELSE
021900000000     C*
022000000000     C* CICLO FINO A CHE:
022100000000DO  2C     $FINE         DOWEQ     '0'
022200000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
022300000000     C*
022400000000     C* CONTROLLA GESTIONE VIDEO
022500000000IF  3C     $GES          IFEQ      'V1'
022600000000     C                   EXSR      GESV1                                        *MANUTENZIONE TEMPO
022700000000E   3C                   ENDIF
022800000000     C*
022900000000E   2C                   ENDDO
023000000000E   1C                   ENDIF
023100000000     C*
023200000000     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
023300000000IF  1C     $F03          IFEQ      '1'
023400000000     C                   MOVEL     '1'           D92F03
023500000000E   1C                   ENDIF
023600000000     C*
023700000000     C* SE E' STATO PREMUTO F12 IMPOSTO FLAG DELLA DS DI PROCEDURA
023800000000IF  1C     $FINE         IFEQ      '1'
023900000000     C                   MOVEL     '1'           D92F12
024000000000E   1C                   ENDIF
024100000000     C*
024200970704     C* OPERAZIONI DI CHIUSURA
024300970704     C                   EXSR      CHIUDI                                       *CHIUDE FILE
024400970704     C*
024500970704     C                   MOVEL     DSSD92        KPJBU
024600000000     C                   SETON                                        LR
024700000000     C*------------------------------------------------------------------------*
024800000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
024900000000     C*------------------------------------------------------------------------*
025000000000     C     CARV1         BEGSR
025100000000     C*
025200000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
025300000000     C*
025400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
025500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
025600000000     C*
025700970707     C                   WRITE     SI93DVT                                      *TESTATA
025800970707     C                   WRITE     SI93DVF                                      *FUNZIONI
025900000000     C*
026000000000     C                   ENDSR
026100000000     C*------------------------------------------------------------------------*
026200000000     C* GESV1  - GESTIONE PRIMO VIDEO
026300000000     C*------------------------------------------------------------------------*
026400000000     C     GESV1         BEGSR
026500000000     C*
026600000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
026700000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
026800000000E   1C                   ENDIF
026900000000     C*
027000000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
027100000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
027200000000E   1C                   ENDIF
027300000000     C*
027400970707     C                   EXFMT     SI93DV1
027500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
027600000000     C*
027700000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
027800000000SL  2C                   SELECT
027900000000     C     KEY           WHENEQ    ENTER
028000000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
028100000000     C                   OTHER
028200000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
028300000000E   2C                   ENDSL
028400000000E   1C                   ENDIF
028500000000     C*
028600000000     C                   ENDSR
028700000000     C*------------------------------------------------------------------------*
028800000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
028900000000     C*------------------------------------------------------------------------*
029000000000     C     ENTV1         BEGSR
029100000000     C*
029200000000     C                   ENDSR
029300000000     C*------------------------------------------------------------------------*
029400000000     C* RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
029500000000     C*------------------------------------------------------------------------*
029600000000     C     RECVID        BEGSR
029700000000     C***
029800000000     C* PARAMETRI PASSATI
029900000000     C***
030000140911     c                   setoff                                       11
030100000000     C                   Z-ADD     D92DSP        DSPV1C
030200000000     C                   Z-ADD     D92DSP        G08INV
030300000000     C                   Z-ADD     *ZEROS        G08DAT
030400000000     C                   MOVEL     '3'           G08ERR
030500000000     C                   CALL      'XSRDA8'
030600000000     C                   PARM                    WLBDA8
030700000000     C                   Z-ADD     G08DAT        V1CDSP                         *DATA SPEDIZIONE
030800000000     C                   Z-ADD     D92LNP        V1CLNP                         *LINEA DI PARTENZA
030900000000     C                   MOVEL     D92NAR        V1CNAR                         *NAZIONE
031000000000     C                   MOVEL     D92CAD        V1CCAD                         *CAP
031100970704     C                   MOVEL     D92LOD        V1CLOD                         *LOCALITA'
031200100715     C                   MOVEL     D92LOD        V1CLOD35         35            *LOCALITA'
031300000000     C                   MOVEL     D92TSP        V1CTSP                         *TIPO SERVIZIO
031400980116     C                   MOVEL     D92FFD        V1CFFD                         *FERMO DEPOSITO
031500140911     c                   if        v1cffd='S'
031600140911     c                   seton                                        11
031700140911     c                   endif
031800000919     C                   MOVEL     D92FDN        V1CFDN                         *MERCE A MAGAZZINO
031900980703     C                   MOVEL     D92TPO        V1CTPO                         *TIPO PORTO
032000980703     C                   MOVEL     D92NZM        V1CNZM                         *NAZIONE MITTENTE
032100980703     C                   MOVEL     D92CAM        V1CCAM                         *CAP MITTENTE
032200980703     C                   MOVEL     D92LOM        V1CLOM                         *LOCALITA' MITTENTE
032300020225     C                   MOVEL     D92NTW        V1CNTW                         *NETWORK
032400021111     C                   MOVEL     D92DDT        V1CDDT                         *CONSEGNA DDT
032500050413     C                   MOVEL     D92tc1        V1Ctc1                         *CONSEGNA DDT
032600050413     C                   MOVEL     D92tc2        V1Ctc2                         *CONSEGNA DDT
032700130925     C***                MOVEL     d92emd        V1Cemd                         *avviso email dest
032800130925     c                   select
032900130925     c                   when      d92emd='S'
033000130925     c                   eval      v1cemd='SMS '
033100130925     c                   when      d92emd='M'
033200130925     c                   eval      v1cemd='MAIL'
033300130925     c                   other
033400130925     c                   eval      v1cemd='NO  '
033500130925     c                   endsl
033600000000     C***
033700970704     C* DECODIFICHE PARAMETRI O PARAMETRI DESUNTI
033800000000     C***
033900000000     C* GIORNO DELLA SETTIMANA DATA SPEDIZIONE
034000000000     C                   Z-ADD     *ZEROS        KCLTFP
034100000000     C                   Z-ADD     *ZEROS        KCLTFA
034200000000     C                   Z-ADD     DSPAAS        KCLANN
034300000000     C                   Z-ADD     DSPMES        KCLMES
034400000000     C     KEYCLN        CHAIN     AZCLN01L                           99
034500000000IF  1C     *IN99         IFEQ      *OFF
034600000000     C                   MOVEA     CLNMAT        MES                             *GIORNI DEL MESE
034700000000     C                   Z-ADD     DSPGIO        NG                2 0           *NUMERO GIORNO MESE
034800000000     C                   MOVEL     MES(NG)       G                 1 0           *GG SETTIMANA (1÷7)
034900000000     C                   MOVEL     DGIO(G)       V1DDSP                          *DECODIFICA GIORNO
035000000000E   1C                   ENDIF
035100000623     C*
035200000623     C* DESCRIZIONE FILIALE PARTENZA E REPERIMENTO TERMINAL
035300000623     C                   Z-ADD     V1CLNP        KORFIL
035400000623     C     KEYORG        CHAIN     AZORG01L                           99
035500000623IF  1C     *IN99         IFEQ      *OFF
035600000623     C                   MOVEL     ORGDES        V1DLNP                         *FILIALE
035700000623     C                   MOVEL     ORGDE3        OG143
035800020627IF  2C     §OGNTW        IFEQ      'PPT'                                        *FILIALE POSTE
035900000623     C                   Z-ADD     V1CLNP        V1CTFP                         *TERMINAL = FILIALE
036000000623X   2C                   ELSE                                                   *NO FILIALE POSTE
036100000623     C* RECUPERA TERMINAL
036200971020     C                   CLEAR                   DSLV55
036300971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
036400971020     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
036500971020     C                   Z-ADD     V1CLNP        D55LIN                         *LINEA
036600070406     C***                Z-ADD     DATCOR        D55DRF                         *DATA RIFERIMENTO
036700070406     C                   Z-ADD     d92dsp        D55DRF                         *DATA RIFERIMENTO
036800971020     C                   CALL      'FNLV55R'
036900971020     C                   PARM                    DSLV55
037000971020     C                   Z-ADD     D55TFP        V1CTFP                         *TERMINAL PARTENZA
037100000623E   2C                   ENDIF
037200000623E   1C                   ENDIF
037300971020     C*
037400000623     C* DESCRIZIONE TERMINAL PARTENZA
037500000000     C                   Z-ADD     V1CTFP        KORFIL
037600000000     C     KEYORG        CHAIN     AZORG01L                           99
037700971020IF  1C     *IN99         IFEQ      *OFF
037800971020     C                   MOVEL     ORGDES        V1DTFP                         *TERMINAL
037900971020E   1C                   ENDIF
038000000000     C* NAZIONE
038100020919     C                   MOVEL     1             KTBKUT
038200000000     C                   MOVEL     '15'          KTBCOD
038300000000     C                   MOVEL     *BLANKS       KTBKEY
038400000000     C                   MOVEL     V1CNAR        KTBKEY
038500000000     C     KEYTAB        CHAIN     TABEL00F                           99
038600000000IF  1C     *IN99         IFEQ      *OFF
038700000000     C     TBLFLG        ANDEQ     *BLANKS
038800000000     C                   MOVEL     TBLUNI        DS15
038900000000     C                   MOVEL     §15DES        V1DNAR
039000000000X   1C                   ELSE
039100000000     C                   MOVEL     *ALL'*'       V1DNAR
039200000000E   1C                   ENDIF
039300970704     C* TIPO SERVIZIO
039400020919     C                   MOVEL     1             KTBKUT
039500970704     C                   MOVEL     '5E'          KTBCOD
039600970704     C                   MOVEL     *BLANKS       KTBKEY
039700970704     C                   MOVEL     V1CTSP        KTBKEY
039800970704     C     KEYTAB        CHAIN     TABEL00F                           99
039900970704IF  1C     *IN99         IFEQ      *OFF
040000970704     C     TBLFLG        ANDEQ     *BLANKS
040100970704     C                   MOVEL     TBLUNI        DS5E
040200090605     C                   MOVEL     §5ED08        V1DTSP
040300970704X   1C                   ELSE
040400970704     C                   MOVEL     *ALL'*'       V1DTSP
040500970704E   1C                   ENDIF
040600050413     C* consegne particolari
040700050413     c                   clear                   v1dtc1
040800050413     c                   clear                   v1dtc2
040900050413     c                   if        v1ctc1<>' '
041000050413     C                   MOVEL     1             KTBKUT
041100050413     C                   MOVEL     '1P'          KTBCOD
041200050413     C                   MOVEL     *BLANKS       KTBKEY
041300050413     C                   MOVEL     V1Ctc1        KTBKEY
041400050413     C     KEYTAB        CHAIN     TABEL00F                           99
041500050413IF  1C     *IN99         IFEQ      *OFF
041600050413     C     TBLFLG        ANDEQ     *BLANKS
041700050413     C                   MOVEL     tbluni        V1Dtc1
041800050413X   1C                   ELSE
041900050413     C                   MOVEL     *ALL'*'       V1Dtc1
042000050413E   1C                   ENDIF
042100050413E   1C                   ENDIF
042200050413     C*
042300050413     c                   if        v1ctc2<>' '
042400050413     C                   MOVEL     1             KTBKUT
042500050413     C                   MOVEL     '1P'          KTBCOD
042600050413     C                   MOVEL     *BLANKS       KTBKEY
042700050413     C                   MOVEL     V1Ctc2        KTBKEY
042800050413     C     KEYTAB        CHAIN     TABEL00F                           99
042900050413IF  1C     *IN99         IFEQ      *OFF
043000050413     C     TBLFLG        ANDEQ     *BLANKS
043100050413     C                   MOVEL     tbluni        V1Dtc2
043200050413X   1C                   ELSE
043300050413     C                   MOVEL     *ALL'*'       V1Dtc2
043400050413E   1C                   ENDIF
043500050413E   1C                   ENDIF
043600050413     c**
043700971126     C* TIPO PORTO
043800971126IF  1C     V1CTPO        IFEQ      'F'
043900980120     C                   MOVEL     DEC(6)        V1DTPO
044000971126X   1C                   ELSE
044100980120     C                   MOVEL     DEC(7)        V1DTPO
044200971126E   1C                   ENDIF
044300020226     C* NETWORK
044400020226     C                   EXSR      DECNTW
044500970704     C*---
044600091222     C* LEGGE IL CAP/Località
044700970704     C*---
044800970704     C                   CLEAR                   DSSI95
044900970704     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
045000970704     C                   MOVEL     '3'           I95TCN                         *CONTROLLO FORMALE
045100970704     C                   MOVEL     D92NAR        I95NAR                         *NAZIONE
045200970704     C                   MOVEL     D92CAD        I95CAP                         *CAP
045300970714     C                   MOVEL     D92LOD        I95LOC                         *LOCALITA'
045400970704     C                   Z-ADD     D92DSP        I95DAT                         *DATA RIFERIMENTO
045500971126     C                   MOVEL     D92TPO        I95TPO                         *TIPO PORTO
045600980116     C                   MOVEL     D92FFD        I95FFD                         *FERMO DEPOSITO
045700980119     C                   Z-ADD     D92PKB        I95LKG                         *PESO
045800980119     C                   Z-ADD     D92VLF        I95LMC                         *VOLUME
045900980602     C                   MOVEL     D92TSP        I95TSP                         *TIPO SERVIZIO
046000990303     C                   Z-ADD     V1CTFP        I95TFP                         *TERMINAL PARTENZA
046100020225     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
046200971126IF  1C     D92CAS        IFGT      *ZEROS
046300971126     C                   MOVEL     'S'           I95FCA                         *SI C/ASSEGNO
046400971126X   1C                   ELSE
046500971126     C                   MOVEL     'N'           I95FCA                         *NO C/ASSEGNO
046600971126E   1C                   ENDIF
046700021028     C                   IF        D92NTW = 'DPD'
046800021028     C                   MOVEL     'D'           I95FI1
046900021028     C                   ENDIF
047000020225     C                   IF        D92NTW = 'FED'
047100020225     C                   MOVEL     'S'           I95FI1
047200020225     C                   ENDIF
047300050413     c*
047400050413     c                   movel     v1ctc1        i95fi2
047500050413     c                   move      v1ctc2        i95fi2
047600050413     c*
047700970704     C                   CALL      'TISI95R'
047800970704     C                   PARM                    DSSI95
047900091222     c
048000091222     c* Verifico per la località se il tipo servizio prevede un codice tempo
048100091222     c*  consegna obbligato, anche parziale
048200091222    1c                   if        d92tsp<>' '  and §5ettc<>*blanks
048300091222     c* controllo posizione per posizione escudendo le "x"
048400091222     c                   eval      Wttc=§5ettc
048500091222     c                   eval      WttcCAP=%editc(o95ttc:'X')
048600091222     c                   z-add     1             c
048700091222    3c                   do        5             c
048800091222     c                   if        %subst(wttc:c:1)<>'x'
048900091222     c                             and
049000091222     c                             %subst(wttc:c:1)<>%subst(wttcCAP:c:1)
049100091222     C                   MOVEL     ERR(2)        d92msg
049200110509     c                   eval      d92msg = %trim(d92msg) + ' "' +
049300110510     c                             v1ctsp + '-' + §5edes
049400110510     c                   eval      d92msg = %trim(d92msg) + '"'
049500091222     C                   MOVEL     '1'           d92err
049600091222     C                   MOVEL     '1'           $fine
049700091222     c                   endif
049800091222    3c                   enddo
049900091222    2c                   endif
050000970704     C* INOLTRO
050100110622     C                   MOVEL     1             KTBKUT
050200110622     C                   MOVEL     '9A'          KTBCOD
050300110622     C                   MOVEL     *BLANKS       KTBKEY
050400110622     C                   MOVEL     o95iso        KTBKEY
050500110622     C     KEYTAB        CHAIN     TABEL00F                           99
050600110622IF  1C     *IN99         IFEQ      *OFF
050700110622     C     TBLFLG        ANDEQ     *BLANKS
050800110622     C                   MOVEL     TBLUNI        DS9a
050900110622     c                   eval      v1diso=§9adesbr
051000110622     c                   endif
051100110622     c
051200110622IF  1C***  O95ISO        IFEQ      'I'
051300110622     C***                MOVEL     DEC(1)        V1DISO                         *DECODIFICA CAP
051400110622E   1C***                ENDIF
051500110622IF  1C***  O95ISO        IFEQ      'P'
051600110622     C***                MOVEL     DEC(2)        V1DISO                         *DECODIFICA CAP
051700110622E   1C***                ENDIF
051800110622IF  1C***  O95ISO        IFEQ      'C'
051900110622     C***                MOVEL     DEC(3)        V1DISO                         *DECODIFICA CAP
052000110622E   1C***                ENDIF
052100110622IF  1C***  O95ISO        IFEQ      'D'
052200110622     C***                MOVEL     DEC(4)        V1DISO                         *DECODIFICA CAP
052300110622E   1C***                ENDIF
052400980116     C* LINEA / ZONA ARRIVO
052500970704     C                   Z-ADD     O95LNA        V1CLNA                         *LINEA ARRIVO
052600980116     C                   Z-ADD     O95ZNC        V1CZNC                         *ZONA CONSEGNA
052700000000     C                   Z-ADD     V1CLNA        KORFIL
052800000000     C     KEYORG        CHAIN     AZORG01L                           99
052900971020IF  1C     *IN99         IFEQ      *OFF
053000971020     C                   MOVEL     ORGDES        V1DLNA                         *DESCRIZIONE
053100971020E   1C                   ENDIF
053200971020     C* TERMINAL ARRIVO
053300971020     C                   CLEAR                   DSLV55
053400971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
053500971020     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
053600971020     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
053700070406     C                   Z-ADD     d92dsp        D55DRF                         *DATA RIFERIMENTO
053800070406     C****               Z-ADD     DATCOR        D55DRF                         *DATA RIFERIMENTO
053900971020     C                   CALL      'FNLV55R'
054000971020     C                   PARM                    DSLV55
054100971020     C                   Z-ADD     D55TFA        V1CTFA                         *TERMINAL ARRIVO
054200971020     C                   Z-ADD     V1CTFA        KORFIL
054300971020     C     KEYORG        CHAIN     AZORG01L                           99
054400971020IF  1C     *IN99         IFEQ      *OFF
054500971020     C                   MOVEL     ORGDES        V1DTFA                         *DESCRIZIONE
054600971020E   1C                   ENDIF
054700960926     C* TASSAZIONE
054800960926     C                   Z-ADD     D92NCL        V1CNCL                         *COLLI
054900960926     C                   Z-ADD     D92PKB        V1CPKB                         *PESO
055000960926     C                   Z-ADD     D92VLF        V1CVLF                         *VOLUME
055100960926     C                   Z-ADD     D92CAS        V1CCAS                         *IMPORTO C/ASSEGNO
055200960926     C                   MOVEL     D92VCA        V1CVCA                         *VALUTA  C/ASSEGNO
055300960926     C                   MOVEL     D92TIC        V1CTIC                         *INCASSO C/ASSENGO
055400960926     C                   MOVEL     D92VAS        V1CVAS                         *VALUTA ASSICURAZION
055500960926     C                   Z-ADD     D92IAS        V1CIAS                         *ASSICURAZIONE
055600050415     C                   Z-ADD     D92qft        V1Cqft                         *ASSICURAZIONE
055700020813     C                   Z-ADD     D92SCO        V1CSCO                         *% DI SCONTO
055800000000     C***
055900000000     C* LANCIA PROGRAMMA CALCOLO DELIVERY
056000000000     C***
056100971009     C                   CLEAR                   DSSD99
056200000000     C                   MOVEL     'P'           D98TLA                         *TIPO LANCIO
056300000000     C                   Z-ADD     DSPV1C        D98DSP                         *DATA SPEDIZIONE
056400000000     C                   Z-ADD     V1CLNP        D98LNP                         *LINEA DI PARTENZA
056500000000     C                   Z-ADD     V1CTFP        D98TFP                         *TERMINAL PARTENZA
056600000000     C                   MOVEL     V1CNAR        D98NZD                         *NAZIONE DI DESTINO
056700000000     C                   MOVEL     V1CCAD        D98CAD                         *CAP DI DESTINO
056800100715     C**                 MOVEL     V1CLOD        D98LOD                         *LOCALITA' DESTINO
056900100715     C                   MOVEL     V1CLOD35      D98LOD                         *LOCALITA' DESTINO
057000000000     C                   Z-ADD     V1CLNA        D98LNA                         *LINEA DI ARRIVO
057100000000     C                   Z-ADD     V1CTFA        D98TFA                         *TERMINAL DI ARRIVO
057200000000     C                   MOVEL     V1CTSP        D98TSP                         *TIPO SPEDIZIONE
057300000000     C                   MOVEL     D92ISO        D98FIN                         *FLAG INOLTRO-CAP IS
057400000000     C                   Z-ADD     1             D98NCD                         *BOLLA CON COLLI
057500971009     C                   CALL      'TNSD99R'
057600971009     C                   PARM                    DSSD99
057700091222     c
057800000000IF  1C     D98ERR        IFEQ      '0'                                          * N O  ERRORI
057900091222     c* se tipo servizio "H" e non mi ha restituito il tempo 10
058000091222     c*  significa che non è località servita quindi esco con errore
058100091222    2c                   if        v1ctsp='H' and d98oee<>1030
058200091222     C                   MOVEL     '1'           $FINE
058300091222     C                   MOVEL     '1'           D92ERR
058400091222     C                   eval      D92MSG=err(2)
058500110509     c                   eval      d92msg = %trim(d92msg) + ' "' +
058600110510     c                             v1ctsp + '-' + §5edes
058700110510     c                   eval      d92msg = %trim(d92msg) + '"'
058800091222   x2c                   else
058900091222     c*
059000141103     c                   clear                   g08inv
059100141103     c                   if        d98dee>0
059200000000     C                   Z-ADD     D98DEE        G08INV
059300141103     c                   else
059400141103     c                   if        d98nzd<>*blanks  and d92ntw<>*blanks
059500141103     C                   Z-ADD     D98DcE        G08INV
059600141103     c                   endif
059700141103     c                   endif
059800000000     C                   Z-ADD     *ZEROS        G08DAT
059900000000     C                   MOVEL     '3'           G08ERR
060000000000     C                   CALL      'XSRDA8'
060100000000     C                   PARM                    WLBDA8
060200000000     C                   Z-ADD     G08DAT        V1CDEE                         *DATA TEOR.CONS.CLI.
060300050505     C                   Z-ADD     D98OEE        V1COEE            4 0          *ORA  TEOR.CONS.CLI.
060400050505     c* Decodifica
060500091222    3c                   select
060600091222     c                   when      v1coee=1030
060700091222     c                   eval      v1doee=dec(13)
060800091222     c                   when      v1coee=1200
060900091222     c                   eval      v1doee=dec(12)
061000091222     c                   other
061100050505     c                   eval      v1doee=dec(11)
061200091222    3c                   endsl
061300000000     C*
061400000000     C                   Z-ADD     D98TPC        V1CTPC                         *ORA  TEOR.PART.CLI.
061500000000     C                   Z-ADD     D98TTC        V1CTTC                         *ORA  TEOR.TRAZ.CLI.
061600000000     C                   Z-ADD     D98TCC        V1CTCC                         *ORA  TEOR.CONS.CLI.
061700000000     C                   Z-ADD     D98TRC        V1CTRC                         *ORA  TEOR.RESA CLI.
061800000000     C*
061900000000     C* GIORNO DELLA SETTIMANA DATA CALCOLATA
062000000000     C                   Z-ADD     *ZEROS        KCLTFP
062100000000     C                   Z-ADD     *ZEROS        KCLTFA
062200000000     C                   Z-ADD     DEEANN        KCLANN
062300000000     C                   Z-ADD     DEEMES        KCLMES
062400000000     C     KEYCLN        CHAIN     AZCLN01L                           99
062500091222IF  3C     *IN99         IFEQ      *OFF
062600950419     C                   MOVEA     CLNMAT        MES                            *GIORNI DEL MESE
062700950419     C                   Z-ADD     DEEGIO        NG                2 0          *NUMERO GIORNO MESE
062800950419     C                   MOVEL     MES(NG)       G                 1 0          *GG SETTIMANA (1÷7)
062900950419     C                   MOVEL     DGIO(G)       V1DDEE                         *DECODIFICA GIORNO
063000091222E   3C                   ENDIF
063100950419     C*
063200110622     C* SE FORZATURA tipo inoltro
063300110622     C                   MOVEL     1             KTBKUT
063400110622     C                   MOVEL     '9A'          KTBCOD
063500110622     C                   MOVEL     *BLANKS       KTBKEY
063600110622     C                   MOVEL     D92iso        KTBKEY
063700110622     C     KEYTAB        CHAIN     TABEL00F                           99
063800110622IF  1C     *IN99         IFEQ      *OFF
063900110622     C     TBLFLG        ANDEQ     *BLANKS
064000110622     C                   MOVEL     TBLUNI        DS9a
064100110623     c                   eval      v1dtcc='Forza '+§9adesbr
064200110623    1c                   endif
064300110622IF  3C**   D92ISO        IFEQ      'I'
064400110622     C**                 MOVEL     DEC(5)        V1DTCC                         *DECOD. ORE CONSEGNA
064500110622E   3C**                 ENDIF
064600110622IF  3C**   D92ISO        IFEQ      'D'
064700110622     C**                 MOVEL     DEC(8)        V1DTCC                         *DECOD. ORE CONSEGNA
064800110622E   3C**                 ENDIF
064900110622IF  3C**   D92ISO        IFEQ      'P'
065000110623     C**                 MOVEL     DEC(9)        V1DTCC                         *DECOD. ORE CONSEGNA
065100110623E   3C**                 ENDIF
065200110623IF  3C**   D92ISO        IFEQ      'C'
065300110623     C**                 MOVEL     DEC(10)       V1DTCC                         *DECOD. ORE CONSEGNA
065400110623E   3C**                 ENDIF
065500110623     c
065600091222E   2C                   ENDIF
065700091222X   1C                   ELSE                                                   * S I  ERRORI
065800950531     C                   MOVEL     '1'           $FINE
065900950531     C                   MOVEL     D98ERR        D92ERR
066000950531     C                   MOVEL     D98MSG        D92MSG
066100091222E   1C                   ENDIF
066200050505     c                   eval      v2dtcc=v1dtcc
066300960528     C***
066400960528     C* LANCIA PROGRAMMA TASSAZIONE SINGOLA BOLLA
066500960528     C***
066600960729IF  1C     D92NCL        IFGT      *ZEROS
066700960729IF  2C     $ERR          IFEQ      '0'
066800991123     C                   CLEAR                   DTAS                           *PULISCE DS DI LANCI
066900991123     C                   CLEAR                   DTASV
067000970704     C                   MOVEL     'L'           TASTLA                         *ELABORA E CHIUDE
067100960703     C                   MOVEL     D92KSC        TASKSC
067200960703     C                   MOVEL     D92CTR        TASCTR
067300960703     C                   MOVEL     D92KSC        V1CKSC                         *VIDEO
067400960703     C                   MOVEL     D92CTR        V1CCTR
067500980703     C*
067600960703     C* CALCOLA I SINGOLI ELEMENTI DI TASSAZIONE
067700991124IF  3C     D92TPO        IFEQ      'F'                                          *FRANCO->DATI DESTIN
067800980703     C                   MOVEL     'F1'          TASTBL                         *TIPO BOLLA
067900991124X   3C                   ELSE                                                   *ASSEGNATO->DATI MIT
068000980703     C                   MOVEL     'A2'          TASTBL                         *TIPO BOLLA
068100980703     C                   MOVEL     DEPMCT        TASMCT                         *CODICE TASSAZIONE
068200980703     C                   MOVEL     DEPFAP        TASFAP                         *FLAG INOLTRO
068300991124E   3C                   ENDIF
068400050428     c* Se consegna a magazzino, flag anteporto='C'
068500050428     c                   if        d92fdn='S'
068600050428     c                   eval      tasfap='C'
068700050428     c                   endif
068800050428     c
068900000919     C                   MOVEL     D92FDN        TASFDN                         *MERCE A MAGAZZINO
069000960528     C                   Z-ADD     D92NCL        TASNCL                         *COLLI
069100960528     C                   Z-ADD     D92PKB        TASPKF                         *PESO
069200960528     C                   Z-ADD     D92VLF        TASVLF                         *VOLUME
069300960926     C                   Z-ADD     D92CAS        TASCAS                         *IMPORTO C/ASSEGNO
069400960926     C                   MOVEL     D92VCA        TASVCA                         *VALUTA  C/ASSEGNO
069500960926     C                   MOVEL     D92TIC        TASTIC                         *INCASSO C/ASSENGO
069600960926     C                   MOVEL     D92VAS        TASVAS                         *VALUTA ASSICURAZION
069700960926     C                   Z-ADD     D92IAS        TASIAS                         *ASSICURAZIONE
069800050415     C                   Z-ADD     D92qft        TASqft                         *ASSICURAZIONE
069900960528     C                   Z-ADD     D92DSP        TASDSP                         *DATA SPEDIZIONE
070000970414     C                   Z-ADD     D92DSP        TASDCT                         *DATA CAR TAR CAR
070100991123     C                   MOVEL     D92NAR        TASNZD                         *NAZIONE DESTINO
070200991123     C                   MOVEL     D92NZM        TASNZM                         *NAZIONE MITTENTE
070300960528     C                   MOVEL     D92CAD        TASCAD                         *CAP DESTINATARIO
070400980703     C                   MOVEL     O95CTS        TASCTS                         *CODICE TASSAZIONE
070500010823     C                   MOVEL     O95ISO        TASFIN                         *FLAG INOLTRO
070600010823     C                   MOVEL     §GEDCN        TASDIV                         *DIVISA NON LA SO
070700991124IF  3C     D92ISO        IFNE      *BLANKS
070800960528     C                   MOVEL     D92ISO        TASFIN                         INOLTRO ISOLA FORZAT
070900991124E   3C                   ENDIF
071000050428     c* Se fermo deposito flag inoltro = 'C'
071100050428     c                   if        d92ffd='S'
071200050428     c                   movel     'C'           tasfin
071300050428     c                   endif
071400050413     C                   MOVEL     V1cTC1        TASTC1
071500050413     C                   MOVEL     V1cTC2        TASTC2
071600960528     C                   MOVEL     D92TSP        TASTSP                         *TIPO SERVIZIO
071700960528     C                   Z-ADD     D92LNP        TASLNP                         *LINEA PARTENZA
071800020514     C                   Z-ADD     O95LNA        TASLNA                         *LINEA ARRIVO
071900020514     C* IMPOSTO LA LINEA DI ARRIVA IN RELAZIONE AL NETWROK
072000020514     C     D92NTW        IFEQ      'DPD'
072100020514     C                   Z-ADD     1             I
072200020514     C     D92NAR        LOOKUP    NAR(I)                                 55
072300020514     C                   IF        %FOUND
072400020514     C                   Z-ADD     NARLAD(I)     TASLNA                         *LINEA ARRIVO
072500020514     C                   ENDIF
072600020514     C                   ENDIF
072700970414     C                   MOVEL     D92DDT        TASDDT                         *SI/NO DDT
072800110818     c                   CLEAR                   DTAS01
072900130925     c***                movel     d92emd        §asemd
073000130925     c                   select
073100130925     c                   when      d92emd='S'
073200130925     c                   eval      §asemd='X'
073300130925     c                   when      d92emd='M'
073400130925     c                   eval      §asemd='S'
073500130925     c                   endsl
073600110818     c                   EVAL      TASFLO = DTAS01
073700030618      * se codice cliente è una cartello primi 3 caratteri = a 888 aggiungo il calcolo della varia D
073800030619      * recuperandolo da cliente linea partenza + 8888 se franco o + 9999 se assegnato
073900030618     c                   movel     tasksc        codfil            3 0
074000030618     c                   if        codfil = 888
074100030619      * costruisco il codice cliente
074200030619     c                   clear                   codksc            7 0
074300030619     c                   eval      codksc = d92lnp * 10000
074400030619      * franco
074500030619     c                   if        d92tpo = 'F'
074600030619      * costruisco il codioce cliente
074700030619     c                   add       8888          codksc
074800030619      * assegnato
074900030619     c                   else
075000030619     c                   add       9999          codksc
075100030619     c                   endif
075200030619      *
075300030619     c                   clear                   tibs69ds
075400030619     c                   eval      i69kin = codksc
075500030619     c                   eval      i69tla = 'L'
075600030619      * cerco anagrafica
075700030619      *
075800030619     C                   CALL      'TIBS69R'
075900030619     C                   PARM                    tibs69DS
076000030619     C                   PARM                    DS_cnaco
076100030619     C                   PARM                    DS_cnind
076200030619     C                   PARM                    DS_cnclp
076300030619     C                   PARM                    DS_fncls
076400030619      *
076500030619     c                   if        o69err = ' '
076600030619     c                   move      indsin        tasivdf
076700030618     c                   move      'D'           tassvdf
076800030618     c                   endif
076900030618      *
077000030619     c                   endif
077100030619      *
077200960528     C                   CALL      'TNSF20R'
077300960528     C                   PARM                    KPJBA
077400991123     C                   PARM                    DTAS
077500991123     C                   PARM                    DTASV
077600960716     C*
077700960716     C* CALCOLA IL TOTALE FATTURA
077800991124IF  3C     TASERR        IFEQ      *BLANKS                                      * N O  ERRORI
077900020813     C*
078000020813     C* APPLICA LA % DI SCONTO, SE INSERITA, SU TUTTI GLI IMPORTI
078100020813IF  4C     D92SCO        IFGT      *ZEROS
078200020813     C                   EXSR      APPSCO
078300020813E   4C                   ENDIF
078400020813     C*
078500991123     C                   CLEAR                   DSLV04
078600960716     C                   MOVEL     'L'           D04TLA                         *ELABORA E CHIUDE
078700991124     C                   Z-ADD     TASIMV        D04IMV                         *IMPONIBILE
078800030619     C                   EXSR      RIEGEI                                       *REPERISCE IMPORTI V
078900991123     C                   MOVEL     §GEAED        D04AED                         *ARROTONDAMENTO
079000991123     C                   Z-ADD     §GEAVL        D04VAR                         *TIPO ARROTONDAMENTO
079100960528     C                   MOVEL     '1'           D04SPE                         *TASSAZIONE 1^BOLLA
079200960926     C                   Z-ADD     D92CAS        D04CAS                         *IMPORTO C/ASSEGNO
079300960926     C                   MOVEL     D92VCA        D04VCA                         *VALUTA  C/ASSEGNO
079400960926     C                   MOVEL     D92TIC        D04TIC                         *INCASSO C/ASSENGO
079500980224     C                   MOVEL     D92DSP        D04DFT                         *DATA FATTURA
079600020108     C                   MOVEL     §GEDCN        D04DIV                         *DIVISA
079700991123     C                   CALL      'FNLV04R'
079800991123     C                   PARM                    DSLV04
079900991123     C                   PARM                    DTASV
080000971009     C                   Z-ADD     D04IMV        V1CPOR                         *PORTO
080100971009     C                   Z-ADD     D04ALI        V1CALI                         *ALIQUOTA
080200971009     C                   Z-ADD     D04IVA        V1CIVA                         *IVA
080300971009     C                   Z-ADD     D04IFT        V1CIFT                         *IMPONIBILE+IVA
080400991124     C                   MOVEL     TASDIV        V1CDIV                         *DIVISA
080500050428     c
080600050415     c* Aggiungo l'arrotondamento al porto o alla prima varia buona
080700050415    3C                   SELECT
080800050415     C     tasPOR        WHENGT    0
080900050415     C                   add       D04POR        taspoR
081000050415     C     var(1)        WHENGT    0
081100050415     C     vsv(1)        ANDNE     §$2BOL
081200050415     C     vsv(1)        ANDNE     §$2IVA
081300050415     C                   ADD       D04POR        var(1)
081400050415     C     var(2)        WHENGT    0
081500050415     C     vsv(2)        ANDNE     §$2BOL
081600050415     C     vsv(2)        ANDNE     §$2IVA
081700050415     C                   ADD       D04POR        var(2)
081800050415     C     var(3)        WHENGT    0
081900050415     C                   ADD       D04POR        var(3)
082000050415    3C                   ENDSL
082100960729X   3C                   ELSE                                                   * S I  ERRORI
082200960529     C                   MOVEL     '1'           $ERR
082300960530     C                   MOVEL     ERR(1)        $MSG
082400960529     C                   EXSR      CARMSG
082500960729E   3C                   ENDIF
082600960729E   2C                   ENDIF
082700960729X   1C                   ELSE
082800971009     C                   Z-ADD     *ZEROS        V1CPOR                         *TRASPORTO
082900971009     C                   Z-ADD     *ZEROS        V1CALI                         *ALIQUOTA
083000971009     C                   Z-ADD     *ZEROS        V1CIVA                         *IVA
083100971009     C                   Z-ADD     *ZEROS        V1CIFT                         *IMPONIBILE VUOTO
083200991124     C                   MOVEL     *BLANKS       V1CDIV                         *DIVISA
083300960604E   1C                   ENDIF
083400000000     C*
083500000000     C                   ENDSR
083600020813     C*------------------------------------------------------------------------*
083700020813     C* APPLICA LA % DI SCONTO, SE INSERITA, SU TUTTI GLI IMPORTI
083800020813     C*------------------------------------------------------------------------*
083900020813     C     APPSCO        BEGSR
084000020813     C*
084100020813     C* Calcola la % da applicare per ottener l'importo scontato
084200020813     C                   EVAL      WPer = 100 - D92SCO
084300020813     C*---
084400020813     C* Totale imponibile
084500020813     C*---
084600050428     C                   EVAL(H)   tasimv = (tasimv * WPer) / 100
084700020813     C*---
084800020813     C* Varie
084900020813     C*---
085000020813do  1c     1             DO        20            v                 5 0
085100020813if  2c                   IF        var(v) > *zeros
085200050428     C                   EVAL(H)   var(v) = (var(v) * WPer) / 100
085300020813e   2c                   ENDIF
085400020813e   1c                   ENDDO
085500050428     C*---
085600050428     C* Trasporto
085700050428     C*---
085800050428     C                   EVAL(H)   taspor = (taspor * WPer) / 100
085900050428     C                   CLEAR                   DSLV16
086000050428     C                   MOVEL     'T'           D17FIM
086100050428     C                   Z-ADD     TASPOR        D17POR
086200050428     C                   CALL      'FNLV16R'
086300050428     C                   PARM                    DSLV16
086400050428     C                   PARM                    DTASV
086500050428     c                   eval      tasimv=o17imv
086600020813     c*
086700020813     C                   ENDSR
086800020225     C*------------------------------------------------------------------------*
086900020225     C* DECNTW - DECODIFICA IL NETWORK
087000020225     C*------------------------------------------------------------------------*
087100020225     C     DECNTW        BEGSR
087200020225     C*
087300020225     C* VERIFICA VALORE IN TABELLA
087400020225     C                   CLEAR                   DSBS02
087500020225     C                   MOVEL     'NTW'         T02COD
087600020225     C                   MOVEL     KNSIF         T02SIF
087700020225     C                   MOVEL     V1CNTW        T02KE1
087800020225     C                   MOVEL     'C'           T02MOD
087900020225     C                   CALL      'TIBS02R'
088000020225     C                   PARM                    KPJBA
088100020225     C                   PARM                    DSBS02
088200020225     C*
088300020225     C     T02ERR        IFEQ      *BLANKS
088400020225     C                   MOVEL     T02UNI        DNTW
088500020225     C                   ENDIF
088600020225     C*
088700020225     C                   ENDSR
088800991123     C*------------------------------------------------------------------------*
088900991123     C* RIEGEI - REPERIENTO IMPORTI VARI
089000991123     C*------------------------------------------------------------------------*
089100991123     C     RIEGEI        BEGSR
089200991123     C*
089300991123     C                   CLEAR                   DSBS02
089400991123     C                   CLEAR                   DGEI
089500991123     C                   MOVEL     'C'           T02MOD
089600991123     C                   MOVEL     KNSIF         T02SIF
089700991123     C                   MOVEL     'GEI'         T02COD
089800991124     C                   MOVEL     TASDIV        T02KE1
089900991123     C                   CALL      'TIBS02R'
090000991123     C                   PARM                    KPJBA
090100991123     C                   PARM                    DSBS02
090200991123IF  1C     T02ERR        IFEQ      *BLANKS
090300991123     C                   MOVEL     T02UNI        DGEI
090400991123E   1C                   ENDIF
090500991123     C*
090600991123     C                   ENDSR
090700951030     C*------------------------------------------------------------------------*
090800951030     C* CARTAB - CARICA TABELLE
090900951030     C*------------------------------------------------------------------------*
091000951027     C     CARTAB        BEGSR
091100960604     C*--------------------
091200960604     C* NAZIONI
091300960604     C*--------------------
091400951027     C                   Z-ADD     0             I
091500020919     C                   Z-ADD     1             KTBKUT
091600951027     C                   MOVEL     '15'          KTBCOD
091700951027     C                   MOVEL     *BLANKS       KTBKEY
091800951027     C     KEYTAB        SETLL     TABEL00F                           99
091900951027     C  N99              READ      TABEL00F                               99
092000951030IF  1C     *IN99         IFEQ      *ON
092100951027     C                   MOVEL     MSG(6)        $MSG
092200951027     C                   EXSR      CARMSG
092300951030E   1C                   ENDIF
092400951030DO  1C     *IN99         DOWEQ     *OFF
092500951027     C     TBLCOD        ANDEQ     '15'
092600951030IF  2C     TBLFLG        IFEQ      *BLANKS
092700951027     C                   ADD       1             I
092800951027     C                   MOVEL     TBLKEY        NAR(I)
092900951027     C                   MOVEL     TBLUNI        DS15
093000020514     C                   Z-ADD     §15LAD        NARLAD(I)
093100951030E   2C                   ENDIF
093200951027     C                   READ      TABEL00F                               99
093300951030E   1C                   ENDDO
093400010823     C*--------------------
093500010823     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
093600010823     C*--------------------
093700010823     C                   MOVEL     'GED'         TBECOD
093800010823     C                   MOVEL     *BLANKS       TBEKE1
093900010823     C                   MOVEL     *BLANKS       TBEKE2
094000010823     C                   MOVEL     '1'           TBEKE1
094100010823     C     KTBE          CHAIN     TNTBE01L                           21
094200010823     C  N21              MOVEL     TBEUNI        DGED
094300050415     C*--------------------
094400050415     C* reperisco sigle varie
094500050415     C*--------------------
094600050415     C                   Z-ADD     1             KTBKUT
094700050415     C                   MOVEL     '$2'          KTBCOD
094800050415     C                   MOVEL(P)  '1'           KTBKEY
094900050415     C     KEYTAB        chain     TABEL00F                           99
095000050415     C  N99              MOVEL     TBLUNI        DS$2
095100050415     C   99              CLEAR                   DS$2
095200131016     C*--------------------------------------
095300131016     C* Controllo se filiale utente è già attiva per gli orari servizio
095400131016     C*--------------------------------------
095500131016     c                   eval      TBEcod = 'VPO'
095600131016     c                   eval      TBEke1 = 'DECOFI816'
095700131016     c                   clear                   TBEke2
095800131016     c     KTBE          chain     TNTBE01L
095900131016     c                   IF        %found(TNTBE01L)
096000131016     c                   eval      FilOkOrari = TBEuni
096100131016     c                   ENDIF
096200131016      /free
096300131016         *in04 = *off;
096400131016         IF  skFilOkOrari(1) = '999' or
096500131016             %lookup(%editc(DUtpou:'X'):skFilOkOrari) > 0;
096600131016           *in04 = *on;
096700131016         ENDIF;
096800131016      /end-free
096900951027     C*
097000951027     C                   ENDSR
097100970704     C*------------------------------------------------------------------------*
097200970704     C* CHIUDI  - CHIUDE EVENTUALI PGM CON USCITA RETURN
097300970704     C*------------------------------------------------------------------------*
097400970704     C     CHIUDI        BEGSR
097500970704     C* TISI95R
097600970704     C                   CLEAR                   DSSI95
097700970704     C                   MOVEL     'C'           I95TLA                         'C'=CHIUDI
097800970704     C                   CALL      'TISI95R'                            99
097900970704     C                   PARM                    DSSI95
098000971020     C* TNSD99R
098100971009     C                   CLEAR                   DSSD99
098200970704     C                   MOVEL     'C'           D98TLA                         'C'=CHIUDI
098300971009     C                   CALL      'TNSD99R'                            99
098400971009     C                   PARM                    DSSD99
098500971020     C* FNLV55R
098600971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
098700971020     C                   MOVEL     'C'           D55TLA
098800971020     C                   CALL      'FNLV55R'
098900971020     C                   PARM                    DSLV55
099000970704     C*
099100970704     C                   ENDSR
099200020919     C*--------------------------------------------------------------------------------------------*
099300020919     C* REPERISCE I DATI UTENTE
099400020919     C*--------------------------------------------------------------------------------------------*
099500020919     C     REPDATIUTE    BEGSR
099600020919     C*
099700020919     C* INIZIALIZZA VARIABILI DI WRK
099800020919     C                   CLEAR                   TIBS34DS
099900020919     C                   CLEAR                   AZUTEDS
100000020919     C                   CLEAR                   DDATIUTE
100100020919     C*
100200020919     C     *DTAARA       DEFINE    §azute        azuteds
100300020919     C     *DTAARA       DEFINE    §datiute      ddatiute
100400020919     C                   IN(E)     *DTAARA
100500020919if  1C                   IF        %Error
100600020919     c                   EVAL      I34Tla = 'L'
100700020919     C                   CALL      'TIBS34R'
100800020919     C                   PARM                    Tibs34Ds
100900020919     C                   IN        *DTAARA
101000020919e   1C                   ENDIF
101100020919     C*
101200020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
101300020919     C                   MOVEL(P)  RSUT          DSFIRS           20
101400020919     C*
101500020919     C                   ENDSR
101600000000     C*------------------------------------------------------------------------*
101700000000     C*  *INZSR - ROUTINE INIZIALE
101800000000     C*------------------------------------------------------------------------*
101900000000     C     *INZSR        BEGSR
102000000000     C*--------------------
102100000000     C* RICEVIMENTO PARAMETRI
102200000000     C*--------------------
102300000000     C     *ENTRY        PLIST
102400000000     C                   PARM                    KPJBA
102500970704     C                   MOVEL     KPJBU         DSSD92
102600000000     C*--------------------
102700000000     C* CAMPI RIFERITI AL DATA BASE
102800000000     C*--------------------
102900000000     C     *LIKE         DEFINE    CLNTFP        KCLTFP                         *AZCLN00F
103000000000     C     *LIKE         DEFINE    CLNTFA        KCLTFA
103100000000     C     *LIKE         DEFINE    CLNANN        KCLANN
103200000000     C     *LIKE         DEFINE    CLNMES        KCLMES
103300000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
103400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
103500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
103600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
103700980703     C     *LIKE         DEFINE    TASMCT        DEPMCT                         *DEPOSITI
103800980703     C     *LIKE         DEFINE    TASFAP        DEPFAP
103900020813     C     *LIKE         DEFINE    D92SCO        WPer
104000020813     C*--------------------                                                     *di lavoro
104100000000     C* CHIAVI DI LETTURA
104200000000     C*--------------------
104300970704     C* DI AZCLN01L - COMPLETA
104400000000     C     KEYCLN        KLIST
104500000000     C                   KFLD                    KCLTFP                         *TERMINAL PARTENZA
104600000000     C                   KFLD                    KCLTFA                         *TERMINAL ARRIVO
104700000000     C                   KFLD                    KCLANN                         *ANNO
104800000000     C                   KFLD                    KCLMES                         *MESE
104900970704     C* DI AZORG01L - COMPLETA
105000000000     C     KEYORG        KLIST
105100000000     C                   KFLD                    KORFIL                         *FILIALE
105200970704     C* DI TABEL00F - COMPLETA
105300000000     C     KEYTAB        KLIST
105400000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
105500000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
105600000000     C                   KFLD                    KTBKEY                         *CHIAVE
105700010823     C* DI TNTBE00F - PARZIALE
105800010823     C     KTBE          KLIST
105900010823     C                   KFLD                    TBECOD
106000010823     C                   KFLD                    TBEKE1
106100010823     C                   KFLD                    TBEKE2
106200980703     C*
106300000000     C* CARICA LE DECODIFICHE DEI GIORNI DELLA SETTIMANA
106400000000     C                   MOVEL     DGIO(1)       GIO(1)                         *LUNEDI'
106500000000     C                   MOVEL     DGIO(2)       GIO(2)                         *MARTEDI'
106600000000     C                   MOVEL     DGIO(3)       GIO(3)                         *MERCOLEDI'
106700000000     C                   MOVEL     DGIO(4)       GIO(4)                         *GIOVEDI'
106800000000     C                   MOVEL     DGIO(5)       GIO(5)                         *VENERDI'
106900000000     C                   MOVEL     DGIO(6)       GIO(6)                         *MERCOLEDI'
107000000000     C                   MOVEL     DGIO(7)       GIO(7)                         *GIOVEDI'
107100971020     C*
107200971020     C* REPERISCE LA DATA CORRENTE
107300971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
107400971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
107500971020     C                   Z-ADD     N8            G08DAT
107600971020     C                   Z-ADD     *ZEROS        G08INV
107700971020     C                   MOVEL     '0'           G08ERR
107800971020     C                   CALL      'XSRDA8'
107900971020     C                   PARM                    WLBDA8
108000971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
108100980703     C*
108200980703     C* REPERISCE I DATI MITTENTE PER LA TASSAZIONE
108300980703IF  1C     D92TPO        IFEQ      'A'
108400980703     C                   CLEAR                   DSSI95
108500980703     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
108600980703     C                   MOVEL     '3'           I95TCN                         *CONTROLLO FORMALE
108700980703     C                   MOVEL     D92NZM        I95NAR                         *NAZIONE
108800980703     C                   MOVEL     D92CAM        I95CAP                         *CAP
108900980703     C                   MOVEL     D92LOM        I95LOC                         *LOCALITA'
109000980703     C                   Z-ADD     D92DSP        I95DAT                         *DATA RIFERIMENTO
109100980703     C                   MOVEL     D92TPO        I95TPO                         *TIPO PORTO
109200980703     C                   Z-ADD     D92PKB        I95LKG                         *PESO
109300980703     C                   Z-ADD     D92VLF        I95LMC                         *VOLUME
109400980703     C                   MOVEL     D92TSP        I95TSP                         *TIPO SERVIZIO
109500021028     C                   IF        D92NTW = 'DPD'
109600021028     C                   MOVEL     'D'           I95FI1
109700021028     C                   ENDIF
109800020225     C                   IF        D92NTW = 'FED'
109900020225     C                   MOVEL     'S'           I95FI1
110000020225     C                   ENDIF
110100980703     C                   CALL      'TISI95R'
110200980703     C                   PARM                    DSSI95
110300980703     C                   MOVEL     O95CTS        DEPMCT                         *CODICE TASSAZIONE
110400980703     C                   MOVEL     O95ISO        DEPFAP                         *FLAG INOLTRO
110500980703E   1C                   ENDIF
110600110620     c* Se si tratta di EEX con nazione Danimarca, Estonia, Lituania, Lettonia
110700110620     c*  non devo visulizzare i tempi di consegna
110800110620     c                   if        d92ntw='EEX'
110900110620     c                   eval      Indx =%lookup(d92nar:nareex)
111000110620     c                   eval      *in01=Indx>0
111100110620     c                   endif
111200110620     c
111300000000     C*--------------------
111400000000     C* IMPOSTAZIONI VIDEO
111500000000     C*--------------------
111600000000     C                   MOVEL     'I'           $TGES             1            *INTERROGAZIONE
111700000000     C                   EXSR      INZSTD                                       *OPERAZIONI STANDARD
111800951027     C                   EXSR      CARTAB                                       *CARICA LE TABELLE
111900000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
112000000000     C                   EXSR      CARV1                                        *CARICA PRIMO VIDEO
112100000000     C*
112200000000     C                   ENDSR
112300000000     C*------------------------------------------------------------------------*
112400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112600000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
112700000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
112800000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
112900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113100000000     C*------------------------------------------------------------------------*
113200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
113300000000     C*------------------------------------------------------------------------*
113400000000     C     INZSTD        BEGSR
113500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
113600000000     C                   CALL      'XKEYS'
113700000000     C                   PARM                    DSKEYS
113800000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
113900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
114000000000     C     XMSGPA        PLIST
114100000000     C                   PARM                    DSMSMS
114200000000     C                   PARM                    DSMSPG
114300000000     C                   PARM                    DSMSKE
114400000000     C                   PARM                    DSMSER
114500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
114600970707     C                   MOVEL     'TISI93R'     DSMSPG                         *NOME PGM CHIAMANTE
114700970707     C                   MOVEL     'TISI93R'     VMCQUE                         *CODA MESSAGGI
114800000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
114900000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
115000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
115100000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
115200000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
115300000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
115400000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
115500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
115600000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
115700000000     C* LANCIO ROUTINE PER VERIFICARE SE
115800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
115900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
116000020919     C                   EXSR      REPDATIUTE
116100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
116200000000     C                   BITOFF    '01234567'    POSMI1            1
116300000000     C                   BITOFF    '01234567'    POSMI2            1
116400000000     C                   BITOFF    '01234567'    POSMA1            1
116500000000     C                   BITOFF    '01234567'    POSMA2            1
116600000000     C                   BITON     '34'          POSMI1            1            *18
116700000000     C                   BITON     '7'           POSMI2            1            *01
116800000000     C                   BITON     '34'          POSMA1            1            *18
116900000000     C                   BITON     '0'           POSMA2            1            *80
117000000000     C                   MOVE      POSMI2        POSMIN            2
117100000000     C                   MOVEL     POSMI1        POSMIN
117200000000     C                   MOVE      POSMA2        POSMAX            2
117300000000     C                   MOVEL     POSMA1        POSMAX
117400000000     C                   ENDSR
117500000000     C*------------------------------------------------------------------------*
117600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
117700000000     C*------------------------------------------------------------------------*
117800000000     C     FUNSTD        BEGSR
117900000000     C                   SELECT
118000000000     C     KEY           WHENEQ    F3
118100000000     C                   EXSR      F03STD
118200000000     C     KEY           WHENEQ    F12
118300000000     C                   EXSR      F12STD
118400000000     C     KEY           WHENEQ    F24
118500000000     C                   EXSR      F24STD
118600050415     C     KEY           WHENEQ    F9
118700050415     C                   EXSR      F09STD
118800050415     C     KEY           WHENEQ    F14
118900050505     c                   setoff                                       40
119000131016     c     KEY           WHENEQ    F16
119100131016     c                   exsr      F16STD
119200050505     C     KEY           WHENEQ    F17
119300050505     C                   EXSR      F17STD
119400000000     C                   OTHER
119500000000     C                   MOVEL     MSG(2)        $MSG
119600000000     C                   EXSR      CARMSG
119700000000     C                   ENDSL
119800000000     C                   ENDSR
119900000000     C*------------------------------------------------------------------------*
120000000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
120100000000     C*------------------------------------------------------------------------*
120200000000     C     F03STD        BEGSR
120300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
120400000000     C                   MOVEL     '1'           $F03
120500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
120600000000     C                   MOVEL     '1'           $FINE
120700000000     C                   ENDSR
120800050415     C*------------------------------------------------------------------------*
120900050415     C* F09STD -GEST.TASTO FUNZ. F09 visalizza dettaglio tassazione
121000050415     C*------------------------------------------------------------------------*
121100050415     C     F09STD        BEGSR
121200050415     c* Pulizia sfl
121300050415     C                   SETON                                        70
121400050415     C                   WRITE     si93wc1
121500050415     C                   SETOFF                                       70
121600050415     c
121700050415     c                   z-add     1             ww                3 0
121800050415     c                   z-add     0             nrr               4 0
121900050428     c                   clear                   vsfsv1
122000050428     c                   setoff                                       02
122100050415     c
122200050415     C* IMPOSTO il porto nella prima posizione del sfl
122300050415     c                   if        taspor>0
122400050428     C                   eval      vsfsv1='Trasporto.:'
122500050608     c**                 z-add     taspor        vsfva1
122600050608     c                   clear                   vsfva1
122700050608     c                   eval      vsfva1=%editc(taspor:'4')
122800050415     c                   add       1             nrr
122900050415     c                   write     si93ws1
123000050415     c                   endif
123100050415     c
123200050415     C* IMPOSTO le varie
123300050415     c                   dow       vsv(ww)<>' '
123400050415     c* Tolgo l'iva, già visualizzata
123500050415     c                   if        vsv(ww)<>§$2iva
123600050428     C                   MOVEL     1             KTBKUT
123700050428     C                   MOVEL     'CC'          KTBCOD
123800050428     C                   MOVEL(P)  'VARIE'       KTBKEY
123900050428     C                   MOVE      vsv(ww)       KTBKEY
124000050428     C     KEYTAB        CHAIN     TABEL00F                           99
124100050428IF  2C     *IN99         IFEQ      *OFF
124200050428     C                   MOVEL     TBLUNI        w009a             9
124300050428     c                   else
124400050428     c                   clear                   w009a
124500050428    2c                   endif
124600050428     c                   eval      vsfsv1=(vsv(ww)+'-'+ w009a)
124700050608     c**                 z-add     var(ww)       vsfva1
124800050608     c                   clear                   vsfva1
124900050608     c                   if        var(WW)>0
125000050608     c                   eval      vsfva1=%editc(var(ww):'4')
125100050608     c                   else
125200050608     c                   move      '0    '       vsfva1
125300050608     c                   endif
125400050608     c
125500050415     c                   add       1             nrr
125600050415     c                   write     si93ws1
125700050428    1c                   endif
125800050415     c
125900050415     c                   add       1             ww
126000050415     c                   enddo
126100050428     c
126200050428     c* Se le varie sono meno di 6 imposto imponibile iVA e totale
126300050428     c* alle posizioni  9 10 11 altrimenti in coda
126400050428     c                   dow       nrr<8
126500050428     c                   clear                   vsfsv1
126600050428     c                   clear                   vsfva1
126700050428     c                   add       1             nrr
126800050428     c                   seton                                        03
126900050428     c                   write     si93ws1
127000050428     c                   enddo
127100050428     c                   setoff                                       03
127200050428     c* Imponibile
127300050428     c                   eval      vsfsv1='Imponibile:'
127400050608     c**                 eval      vsfva1=v1cpor
127500050608     c                   clear                   vsfva1
127600050608     c                   if        v1cpor>0
127700050608     c                   eval      wvalore=v1cpor
127800050608     c                   eval      vsfva1=%editc(wvalore:'4')
127900050608     c                   else
128000050608     c                   move      '0    '       vsfva1
128100050608     c                   endif
128200050428     c                   add       1             nrr
128300050428     c                   write     si93ws1
128400050428     c* IVA
128500050428     c                   eval      vsfsv1='IVA '+ %editc(v1cali:'2')+'..:'
128600050608     c**                 eval      vsfva1=v1civa
128700050608     c                   clear                   vsfva1
128800050608     c                   if        v1civa>0
128900050608     c                   eval      wvalore=v1civa
129000050608     c                   eval      vsfva1=%editc(wvalore:'4')
129100050608     c                   else
129200050608     c                   move      '0    '       vsfva1
129300050608     c                   endif
129400050428     c                   seton                                        02
129500050428     c                   add       1             nrr
129600050428     c                   write     si93ws1
129700050428     c                   setoff                                       02
129800050428     c* totale imponibile
129900050428     c                   eval      vsfsv1='Totale....:'
130000050608     c**                 eval      vsfva1=v1cift
130100050608     c                   clear                   vsfva1
130200050608     c                   if        v1cift>0
130300050608     c                   eval      wvalore=v1cift
130400050608     c                   eval      vsfva1=%editc(wvalore:'4')
130500050608     c                   else
130600050608     c                   move      '0    '       vsfva1
130700050608     c                   endif
130800050428     c                   add       1             nrr
130900050428     c                   write     si93ws1
131000050428     c
131100050415     c*
131200050415     c     for09         tag
131300050415     c                   exfmt     si93wc1
131400050415     c*
131500050415     c                   select
131600050415     c* F3 - fine
131700050415     c                   when      *inkc
131800050415     C                   MOVEL     '1'           $FINE
131900050415     C                   MOVEL     '1'           $F03
132000050415     c* F3 - ritorno
132100050415     c                   when      *inkl
132200050415     C                   MOVEL     '1'           $FINE
132300050415     c* F14- delivery
132400050415     c                   when      *inkn
132500050505     c                   setoff                                       40
132600131017      // F16-OrariSr
132700131017     c                   WHEN      *inkq
132800131017     c                   exsr      F16STD
132900050505     c* F17- ORE delivery
133000050505     c                   when      *inkr
133100050505     c                   seton                                        40
133200050415     c                   other
133300050415     c
133400050415     c                   goto      for09
133500050415     c                   endsl
133600050415     c
133700050415     C                   ENDSR
133800000000     C*------------------------------------------------------------------------*
133900000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
134000000000     C*------------------------------------------------------------------------*
134100000000     C     F12STD        BEGSR
134200000000     C* IMPOSTO FLAG DI FIN PGM
134300000000     C                   MOVEL     '1'           $FINE
134400000000     C                   ENDSR
134500131016
134600131016      /free
134700131016       //--------------------------------------------------------------------
134800131016       //?F16 - Interrogazione orari di servizio.
134900131016       //--------------------------------------------------------------------
135000131016       BEGSR F16STD;
135100131016
135200140203         clear trulorsds;
135300131016         IOREfil = O95lna;
135400131016         IOREcap = D92cad;
135500131016         IOREloc = D92lod;
135600131016         IOREnar = D92nar;
135700140203         //   IOREdta = D92dsp;
135800131016         IOREtser = 'E';
135900131104         IOREtsp = D92tsp;
136000140203         IOREtfp = v1ctfp;
136100140203         IOREtfa = v1ctfa;
136200140826         IOREdei = d98dei;
136300140826         IOREoei = d98oei;
136400140203         IOREdta = d98dce;
136500140203         IOREcol = 35    ;
136600131016
136700140203         trulorsr (kpjba:trulorsds);
136800131017
136900131017         IF  OOREerr <> *blanks;
137000131017           $msg = 'Interrogazione Orari Servizio non possibile.';
137100131017           $msg = %trim($msg) + ' ' + %trim(OOREmsg);
137200131017           exsr carmsg;
137300131017           $err = '1';
137400131017         ENDIF;
137500131016
137600131016       ENDSR;
137700131016
137800131016      /end-free
137900131016
138000050505     C*------------------------------------------------------------------------*
138100050505     C* F17STD -GEST.TASTO FUNZ. F17 ORE delivery
138200050505     C*------------------------------------------------------------------------*
138300050505     C     F17STD        BEGSR
138400050505     C                   seton                                        40
138500050505     C                   ENDSR
138600000000     C*------------------------------------------------------------------------*
138700000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
138800000000     C*------------------------------------------------------------------------*
138900000000     C     F24STD        BEGSR
139000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
139100000000     C                   EXSR      OUTFUN
139200000000     C                   ENDSR
139300000000     C*------------------------------------------------------------------------*
139400000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
139500000000     C*          CODA MESSAGGI DEL PGM
139600000000     C*------------------------------------------------------------------------*
139700000000     C     CARMSG        BEGSR
139800000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
139900000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
140000000000     C     MSGULT        IFEQ      0
140100000000     C                   MOVEL     '1'           *IN40
140200970707     C                   WRITE     SI93DVK
140300000000     C                   MOVEL     '0'           *IN40
140400000000     C                   ENDIF
140500000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
140600000000     C                   MOVEL     $MSG          DSMSMS
140700000000     C                   CALL      'XMSG'        XMSGPA
140800000000     C     DSMSER        IFEQ      0
140900000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
141000000000     C                   ENDIF
141100000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
141200000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
141300970707     C                   WRITE     SI93DVM                                      *SFL MESS
141400000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
141500000000     C                   MOVEL     *BLANKS       $MSG
141600000000     C                   ENDSR
141700000000     C*------------------------------------------------------------------------*
141800000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
141900000000     C*------------------------------------------------------------------------*
142000000000     C     OUTMSG        BEGSR
142100000000     C* VISUALIZZAZIONE MESSAGGIO
142200000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
142300970707     C                   WRITE     SI93DVK                                      *CONTR MESS
142400000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
142500000000     C                   Z-ADD     *ZEROS        MSGULT
142600000000     C                   ENDSR
142700000000     C*------------------------------------------------------------------------*
142800000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
142900000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
143000000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
143100000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
143200000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
143300000000     C*------------------------------------------------------------------------*
143400000000     C     CARFUN        BEGSR
143500110620     C* SE 01 ON --> NON VISUALIZZO F17
143600110624     C**                 MOVEL     FUN(17)       FUNRIG                         *DS RIGA FUNZIONI
143700110624     C**                 IF        *IN01
143800110624     c**                 eval      funasg='N'
143900110624     c**                 eval      funasi='N'
144000110624     c**                 eval      funafg='N'
144100110624     c**                 eval      funafi='N'
144200110624     c**                 else
144300110624     c**                 eval      funasg='S'
144400110624     c**                 eval      funasi='S'
144500110624     c*+                 eval      funafg='S'
144600110624     c**                 eval      funafi='S'
144700110624     c**                 endif
144800110624     c**                 movel     funrig        fun(17)
144900131016
145000131016      /free
145100131016       //?Se 04 spento non devo visualizzare il tasto F16=OrariSr
145200131016         funrig = fun(16);
145300131016         IF  not *in04;
145400131017           funasg = 'S';
145500131017           funasi = 'S';
145600131016           funafg = 'N';
145700131016           funafi = 'N';
145800131016         ELSE;
145900131016           funasg = 'S';
146000131017           funasi = 'S';
146100131016           funafg = 'S';
146200131016           funafi = 'S';
146300131016         ENDIF;
146400131016         fun(16) = funrig;
146500131016      /end-free
146600110620     c
146700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
146800000000     C     1             DO        31            I                 5 0          --- 1 -->
146900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
147000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
147100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
147200020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
147300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
147400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
147500000000     C                   ELSE                                                   *INTERROG.
147600000000     C                   MOVEL     FUNASI        FAT(I)
147700000000     C                   ENDIF
147800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
147900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
148000000000     C                   MOVEL     FUNAFG        FAT(I)
148100000000     C                   ELSE                                                   *INTERROG.
148200000000     C                   MOVEL     FUNAFI        FAT(I)
148300000000     C                   ENDIF
148400000000     C                   ENDIF                                                  <-- 2 ---
148500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
148600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
148700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
148800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
148900000000     C                   ELSE
149000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
149100000000     C                   ENDIF
149200000000     C                   ENDIF
149300000000     C                   ENDDO                                                  <-- 1 ---
149400000000     C                   ENDSR
149500000000     C*------------------------------------------------------------------------*
149600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
149700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
149800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
149900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
150000000000     C*------------------------------------------------------------------------*
150100000000     C     OUTFUN        BEGSR
150200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
150300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
150400000000     C                   ENDDO
150500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
150600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
150700000000     C     1             DO        31            I
150800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
150900050415     C**   FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
151000000000     C                   ADD       1             FUNNAT
151100000000     C                   ENDIF
151200000000     C                   ENDDO
151300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
151400000000     C     FUNNAT        IFGT      8
151500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
151600000000     C                   ENDIF
151700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
151800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
151900000000     C                   Z-ADD     8             FUNVIS            5 0
152000000000     C     FAT(23)       IFEQ      'S'
152100000000     C                   SUB       1             FUNVIS
152200000000     C                   ENDIF
152300000000     C     FAT(24)       IFEQ      'S'
152400000000     C                   SUB       1             FUNVIS
152500000000     C                   ENDIF
152600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
152700000000     C                   Z-ADD     *ZEROS        W                 5 0
152800000000     C                   ADD       1             FUNULT
152900000000     C     FUNULT        IFGT      31
153000000000     C                   Z-ADD     1             FUNULT
153100000000     C                   ENDIF
153200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
153300000000     C     FUNULT        DO        31            I                              --- 1 -->
153400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
153500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
153600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
153700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
153800050415     C**   FDE(I)        ANDNE     *BLANKS
153900000000     C     I             IFEQ      23                                           *TOLGO F23
154000000000     C     I             OREQ      24                                           *E F24
154100000000     C                   ELSE
154200000000     C                   ADD       1             W
154300000000     C                   MOVEL     FDE(I)        FUV(W)
154400000000     C                   ENDIF
154500000000     C                   ENDIF                                                  <-- 3 ---
154600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
154700000000     C                   Z-ADD     I             FUNULT
154800000000     C                   ENDIF                                                  <-- 2 ---
154900000000     C                   ENDDO                                                  <-- 1 ---
155000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
155100000000     C                   MOVEL     FUV(1)        VFD001
155200000000     C                   MOVEL     FUV(2)        VFD002
155300000000     C                   MOVEL     FUV(3)        VFD003
155400000000     C                   MOVEL     FUV(4)        VFD004
155500000000     C                   MOVEL     FUV(5)        VFD005
155600000000     C                   MOVEL     FUV(6)        VFD006
155700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
155800000000     C     FAT(24)       ANDEQ     'N'
155900000000     C                   MOVEL     FUV(7)        VFD007
156000000000     C                   MOVEL     FUV(8)        VFD008
156100000000     C                   ELSE                                                   <-- 1 -->
156200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
156300000000     C     FAT(24)       ANDEQ     'S'
156400000000     C                   MOVEL     FDE(23)       VFD007
156500000000     C                   MOVEL     FDE(24)       VFD008
156600000000     C                   ELSE                                                   <-- 2 -->
156700000000     C                   MOVEL     FUV(7)        VFD007
156800000000     C     FAT(23)       IFEQ      'S'
156900000000     C                   MOVEL     FDE(23)       VFD008
157000000000     C                   ELSE
157100000000     C                   MOVEL     FDE(24)       VFD008
157200000000     C                   ENDIF
157300000000     C                   ENDIF                                                  <-- 2 ---
157400000000     C                   ENDIF                                                  <-- 1 ---
157500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
157600000000     C                   ENDSR
157700000000     C*------------------------------------------------------------------------*
157800000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
157900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
158000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
158100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
158200000000     C*------------------------------------------------------------------------*
158300000000     C     CONFUN        BEGSR
158400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
158500000000     C     1             DO        31            I                              --- 1 -->
158600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
158700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
158800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
158900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
159000000000     C                   EXSR      CARMSG
159100000000     C                   MOVEL     '1'           $ERR
159200000000     C                   ENDIF                                                  <-- 3 ---
159300000000     C                   ENDIF                                                  <-- 2 ---
159400000000     C                   ENDDO                                                  <-- 1 ---
159500000000     C                   ENDSR
159600960530** MSG - MESSAGGI VIDEO
159700970707TISI93R- Tasto funzionale non valido.                                          1
159800970707TISI93R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
159900970707TISI93R- Opzione non valida.                                                   3
160000970707TISI93R- Opzione abilitata ma non gestita. Avvertire CED.                      4
160100970707TISI93R- Inizio elenco.                                                        5
160200970707TISI93R- Fine elenco.                                                          6
160300970707TISI93R- 7
160400970707TISI93R- 8
160500970707TISI93R- 9
160600970707TISI93R- 10
160700970707TISI93R- 11
160800970707TISI93R- 12
160900970707TISI93R- 13
161000970707TISI93R- 14
161100970707TISI93R- 15
161200970707TISI93R- 16
161300970707TISI93R- 17
161400970707TISI93R- 18
161500970707TISI93R- 19
161600970707TISI93R- 20
161700000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
161800000000F1    -                   -    NN     NN
161900000000F2    -                   -    NN     NN
162000000000F3    -F3=Fine            -    SS     SS
162100050428F4    -                   -    NN     NN
162200000000F5    -                   -    NN     NN
162300000000F6    -                   -    NN     NN
162400000000F7    -                   -    NN     NN
162500000000F8    -                   -    NN     NN
162600050415F9    -F9=Dettaglio Tassaz-    SS     SS
162700050415F10   -                   -    SS     SS
162800000000F11   -                   -    NN     NN
162900000000F12   -F12=Ritorno        -    SS     SS
163000000000F13   -                   -    NN     NN
163100050415F14   -F14=Delivery       -    SS     SS
163200000000F15   -                   -    NN     NN
163300131016F16   -F16=OrariSer       -    SS     SS
163400110624F17   -F17=ORE Delivery   -    NN     NN
163500000000F18   -                   -    NN     NN
163600000000F19   -                   -    NN     NN
163700000000F20   -                   -    NN     NN
163800000000F21   -                   -    NN     NN
163900000000F22   -                   -    NN     NN
164000000000F23   -                   -    NN     NN
164100000000F24   -F24=Altri tasti    -    NN     NN
164200000000ENTER -                   -    SS     SS
164300000000ROLLUP-                   -    NN     NN
164400000000ROLLDN-                   -    NN     NN
164500000000HELP  -                   -    NN     NN
164600000000PRINT -                   -    NN     NN
164700000000BSPACE-                   -    NN     NN
164800000000CLEAR -                   -    NN     NN
164900000000** DEC - DECODIFICHE
165000000000ISOLA                    1
165100000000PROVINCIA                2
165200000000CITTA'                   3
165300970704DISAGIATA                4
165400960529FORZ.ISOLA               5
165500971126FRANCO                   6
165600971126ASSEGNATO                7
165700021104FORZ.DISAG               8
165800021104FORZ.PROV.               9
165900021104FORZ.CSTOR               10
166000050505In Giornata  (24:00)     11
166100050505Entro le ore  12:00      12
166200091222Entro le ore  10:30      13
166300110915** DGIO - DECODIFICHE GIORNI DELLA SETTIMANA
166400000000LUNEDI'                  1
166500000000MARTEDI'                 2
166600000000MERCOLEDI'               3
166700000000GIOVEDI'                 4
166800000000VENERDI'                 5
166900000000SABATO                   6
167000000000DOMENICA                 7
167100960530** ERR - ERRORI DI ELABORAZIONE
167200960530TNSF20R- Errori in tassazione                                               1
167300110509La località non è servita dal tipo servizio
1674009605303
1675009605304
1676009605305
167700110620** nareex -  Nazioni con tempi di consegna particolari per EEX
167800110620DK LT LV EE
