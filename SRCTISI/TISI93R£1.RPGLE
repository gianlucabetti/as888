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
030000000000     C                   Z-ADD     D92DSP        DSPV1C
030100000000     C                   Z-ADD     D92DSP        G08INV
030200000000     C                   Z-ADD     *ZEROS        G08DAT
030300000000     C                   MOVEL     '3'           G08ERR
030400000000     C                   CALL      'XSRDA8'
030500000000     C                   PARM                    WLBDA8
030600000000     C                   Z-ADD     G08DAT        V1CDSP                         *DATA SPEDIZIONE
030700000000     C                   Z-ADD     D92LNP        V1CLNP                         *LINEA DI PARTENZA
030800000000     C                   MOVEL     D92NAR        V1CNAR                         *NAZIONE
030900000000     C                   MOVEL     D92CAD        V1CCAD                         *CAP
031000970704     C                   MOVEL     D92LOD        V1CLOD                         *LOCALITA'
031100100715     C                   MOVEL     D92LOD        V1CLOD35         35            *LOCALITA'
031200000000     C                   MOVEL     D92TSP        V1CTSP                         *TIPO SERVIZIO
031300980116     C                   MOVEL     D92FFD        V1CFFD                         *FERMO DEPOSITO
031400000919     C                   MOVEL     D92FDN        V1CFDN                         *MERCE A MAGAZZINO
031500980703     C                   MOVEL     D92TPO        V1CTPO                         *TIPO PORTO
031600980703     C                   MOVEL     D92NZM        V1CNZM                         *NAZIONE MITTENTE
031700980703     C                   MOVEL     D92CAM        V1CCAM                         *CAP MITTENTE
031800980703     C                   MOVEL     D92LOM        V1CLOM                         *LOCALITA' MITTENTE
031900020225     C                   MOVEL     D92NTW        V1CNTW                         *NETWORK
032000021111     C                   MOVEL     D92DDT        V1CDDT                         *CONSEGNA DDT
032100050413     C                   MOVEL     D92tc1        V1Ctc1                         *CONSEGNA DDT
032200050413     C                   MOVEL     D92tc2        V1Ctc2                         *CONSEGNA DDT
032300130925     C***                MOVEL     d92emd        V1Cemd                         *avviso email dest
032400130925     c                   select
032500130925     c                   when      d92emd='S'
032600130925     c                   eval      v1cemd='SMS '
032700130925     c                   when      d92emd='M'
032800130925     c                   eval      v1cemd='MAIL'
032900130925     c                   other
033000130925     c                   eval      v1cemd='NO  '
033100130925     c                   endsl
033200000000     C***
033300970704     C* DECODIFICHE PARAMETRI O PARAMETRI DESUNTI
033400000000     C***
033500000000     C* GIORNO DELLA SETTIMANA DATA SPEDIZIONE
033600000000     C                   Z-ADD     *ZEROS        KCLTFP
033700000000     C                   Z-ADD     *ZEROS        KCLTFA
033800000000     C                   Z-ADD     DSPAAS        KCLANN
033900000000     C                   Z-ADD     DSPMES        KCLMES
034000000000     C     KEYCLN        CHAIN     AZCLN01L                           99
034100000000IF  1C     *IN99         IFEQ      *OFF
034200000000     C                   MOVEA     CLNMAT        MES                             *GIORNI DEL MESE
034300000000     C                   Z-ADD     DSPGIO        NG                2 0           *NUMERO GIORNO MESE
034400000000     C                   MOVEL     MES(NG)       G                 1 0           *GG SETTIMANA (1÷7)
034500000000     C                   MOVEL     DGIO(G)       V1DDSP                          *DECODIFICA GIORNO
034600000000E   1C                   ENDIF
034700000623     C*
034800000623     C* DESCRIZIONE FILIALE PARTENZA E REPERIMENTO TERMINAL
034900000623     C                   Z-ADD     V1CLNP        KORFIL
035000000623     C     KEYORG        CHAIN     AZORG01L                           99
035100000623IF  1C     *IN99         IFEQ      *OFF
035200000623     C                   MOVEL     ORGDES        V1DLNP                         *FILIALE
035300000623     C                   MOVEL     ORGDE3        OG143
035400020627IF  2C     §OGNTW        IFEQ      'PPT'                                        *FILIALE POSTE
035500000623     C                   Z-ADD     V1CLNP        V1CTFP                         *TERMINAL = FILIALE
035600000623X   2C                   ELSE                                                   *NO FILIALE POSTE
035700000623     C* RECUPERA TERMINAL
035800971020     C                   CLEAR                   DSLV55
035900971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
036000971020     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
036100971020     C                   Z-ADD     V1CLNP        D55LIN                         *LINEA
036200070406     C***                Z-ADD     DATCOR        D55DRF                         *DATA RIFERIMENTO
036300070406     C                   Z-ADD     d92dsp        D55DRF                         *DATA RIFERIMENTO
036400971020     C                   CALL      'FNLV55R'
036500971020     C                   PARM                    DSLV55
036600971020     C                   Z-ADD     D55TFP        V1CTFP                         *TERMINAL PARTENZA
036700000623E   2C                   ENDIF
036800000623E   1C                   ENDIF
036900971020     C*
037000000623     C* DESCRIZIONE TERMINAL PARTENZA
037100000000     C                   Z-ADD     V1CTFP        KORFIL
037200000000     C     KEYORG        CHAIN     AZORG01L                           99
037300971020IF  1C     *IN99         IFEQ      *OFF
037400971020     C                   MOVEL     ORGDES        V1DTFP                         *TERMINAL
037500971020E   1C                   ENDIF
037600000000     C* NAZIONE
037700020919     C                   MOVEL     1             KTBKUT
037800000000     C                   MOVEL     '15'          KTBCOD
037900000000     C                   MOVEL     *BLANKS       KTBKEY
038000000000     C                   MOVEL     V1CNAR        KTBKEY
038100000000     C     KEYTAB        CHAIN     TABEL00F                           99
038200000000IF  1C     *IN99         IFEQ      *OFF
038300000000     C     TBLFLG        ANDEQ     *BLANKS
038400000000     C                   MOVEL     TBLUNI        DS15
038500000000     C                   MOVEL     §15DES        V1DNAR
038600000000X   1C                   ELSE
038700000000     C                   MOVEL     *ALL'*'       V1DNAR
038800000000E   1C                   ENDIF
038900970704     C* TIPO SERVIZIO
039000020919     C                   MOVEL     1             KTBKUT
039100970704     C                   MOVEL     '5E'          KTBCOD
039200970704     C                   MOVEL     *BLANKS       KTBKEY
039300970704     C                   MOVEL     V1CTSP        KTBKEY
039400970704     C     KEYTAB        CHAIN     TABEL00F                           99
039500970704IF  1C     *IN99         IFEQ      *OFF
039600970704     C     TBLFLG        ANDEQ     *BLANKS
039700970704     C                   MOVEL     TBLUNI        DS5E
039800090605     C                   MOVEL     §5ED08        V1DTSP
039900970704X   1C                   ELSE
040000970704     C                   MOVEL     *ALL'*'       V1DTSP
040100970704E   1C                   ENDIF
040200050413     C* consegne particolari
040300050413     c                   clear                   v1dtc1
040400050413     c                   clear                   v1dtc2
040500050413     c                   if        v1ctc1<>' '
040600050413     C                   MOVEL     1             KTBKUT
040700050413     C                   MOVEL     '1P'          KTBCOD
040800050413     C                   MOVEL     *BLANKS       KTBKEY
040900050413     C                   MOVEL     V1Ctc1        KTBKEY
041000050413     C     KEYTAB        CHAIN     TABEL00F                           99
041100050413IF  1C     *IN99         IFEQ      *OFF
041200050413     C     TBLFLG        ANDEQ     *BLANKS
041300050413     C                   MOVEL     tbluni        V1Dtc1
041400050413X   1C                   ELSE
041500050413     C                   MOVEL     *ALL'*'       V1Dtc1
041600050413E   1C                   ENDIF
041700050413E   1C                   ENDIF
041800050413     C*
041900050413     c                   if        v1ctc2<>' '
042000050413     C                   MOVEL     1             KTBKUT
042100050413     C                   MOVEL     '1P'          KTBCOD
042200050413     C                   MOVEL     *BLANKS       KTBKEY
042300050413     C                   MOVEL     V1Ctc2        KTBKEY
042400050413     C     KEYTAB        CHAIN     TABEL00F                           99
042500050413IF  1C     *IN99         IFEQ      *OFF
042600050413     C     TBLFLG        ANDEQ     *BLANKS
042700050413     C                   MOVEL     tbluni        V1Dtc2
042800050413X   1C                   ELSE
042900050413     C                   MOVEL     *ALL'*'       V1Dtc2
043000050413E   1C                   ENDIF
043100050413E   1C                   ENDIF
043200050413     c**
043300971126     C* TIPO PORTO
043400971126IF  1C     V1CTPO        IFEQ      'F'
043500980120     C                   MOVEL     DEC(6)        V1DTPO
043600971126X   1C                   ELSE
043700980120     C                   MOVEL     DEC(7)        V1DTPO
043800971126E   1C                   ENDIF
043900020226     C* NETWORK
044000020226     C                   EXSR      DECNTW
044100970704     C*---
044200091222     C* LEGGE IL CAP/Località
044300970704     C*---
044400970704     C                   CLEAR                   DSSI95
044500970704     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
044600970704     C                   MOVEL     '3'           I95TCN                         *CONTROLLO FORMALE
044700970704     C                   MOVEL     D92NAR        I95NAR                         *NAZIONE
044800970704     C                   MOVEL     D92CAD        I95CAP                         *CAP
044900970714     C                   MOVEL     D92LOD        I95LOC                         *LOCALITA'
045000970704     C                   Z-ADD     D92DSP        I95DAT                         *DATA RIFERIMENTO
045100971126     C                   MOVEL     D92TPO        I95TPO                         *TIPO PORTO
045200980116     C                   MOVEL     D92FFD        I95FFD                         *FERMO DEPOSITO
045300980119     C                   Z-ADD     D92PKB        I95LKG                         *PESO
045400980119     C                   Z-ADD     D92VLF        I95LMC                         *VOLUME
045500980602     C                   MOVEL     D92TSP        I95TSP                         *TIPO SERVIZIO
045600990303     C                   Z-ADD     V1CTFP        I95TFP                         *TERMINAL PARTENZA
045700020225     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
045800971126IF  1C     D92CAS        IFGT      *ZEROS
045900971126     C                   MOVEL     'S'           I95FCA                         *SI C/ASSEGNO
046000971126X   1C                   ELSE
046100971126     C                   MOVEL     'N'           I95FCA                         *NO C/ASSEGNO
046200971126E   1C                   ENDIF
046300021028     C                   IF        D92NTW = 'DPD'
046400021028     C                   MOVEL     'D'           I95FI1
046500021028     C                   ENDIF
046600020225     C                   IF        D92NTW = 'FED'
046700020225     C                   MOVEL     'S'           I95FI1
046800020225     C                   ENDIF
046900050413     c*
047000050413     c                   movel     v1ctc1        i95fi2
047100050413     c                   move      v1ctc2        i95fi2
047200050413     c*
047300970704     C                   CALL      'TISI95R'
047400970704     C                   PARM                    DSSI95
047500091222     c
047600091222     c* Verifico per la località se il tipo servizio prevede un codice tempo
047700091222     c*  consegna obbligato, anche parziale
047800091222    1c                   if        d92tsp<>' '  and §5ettc<>*blanks
047900091222     c* controllo posizione per posizione escudendo le "x"
048000091222     c                   eval      Wttc=§5ettc
048100091222     c                   eval      WttcCAP=%editc(o95ttc:'X')
048200091222     c                   z-add     1             c
048300091222    3c                   do        5             c
048400091222     c                   if        %subst(wttc:c:1)<>'x'
048500091222     c                             and
048600091222     c                             %subst(wttc:c:1)<>%subst(wttcCAP:c:1)
048700091222     C                   MOVEL     ERR(2)        d92msg
048800110509     c                   eval      d92msg = %trim(d92msg) + ' "' +
048900110510     c                             v1ctsp + '-' + §5edes
049000110510     c                   eval      d92msg = %trim(d92msg) + '"'
049100091222     C                   MOVEL     '1'           d92err
049200091222     C                   MOVEL     '1'           $fine
049300091222     c                   endif
049400091222    3c                   enddo
049500091222    2c                   endif
049600970704     C* INOLTRO
049700110622     C                   MOVEL     1             KTBKUT
049800110622     C                   MOVEL     '9A'          KTBCOD
049900110622     C                   MOVEL     *BLANKS       KTBKEY
050000110622     C                   MOVEL     o95iso        KTBKEY
050100110622     C     KEYTAB        CHAIN     TABEL00F                           99
050200110622IF  1C     *IN99         IFEQ      *OFF
050300110622     C     TBLFLG        ANDEQ     *BLANKS
050400110622     C                   MOVEL     TBLUNI        DS9a
050500110622     c                   eval      v1diso=§9adesbr
050600110622     c                   endif
050700110622     c
050800110622IF  1C***  O95ISO        IFEQ      'I'
050900110622     C***                MOVEL     DEC(1)        V1DISO                         *DECODIFICA CAP
051000110622E   1C***                ENDIF
051100110622IF  1C***  O95ISO        IFEQ      'P'
051200110622     C***                MOVEL     DEC(2)        V1DISO                         *DECODIFICA CAP
051300110622E   1C***                ENDIF
051400110622IF  1C***  O95ISO        IFEQ      'C'
051500110622     C***                MOVEL     DEC(3)        V1DISO                         *DECODIFICA CAP
051600110622E   1C***                ENDIF
051700110622IF  1C***  O95ISO        IFEQ      'D'
051800110622     C***                MOVEL     DEC(4)        V1DISO                         *DECODIFICA CAP
051900110622E   1C***                ENDIF
052000980116     C* LINEA / ZONA ARRIVO
052100970704     C                   Z-ADD     O95LNA        V1CLNA                         *LINEA ARRIVO
052200980116     C                   Z-ADD     O95ZNC        V1CZNC                         *ZONA CONSEGNA
052300000000     C                   Z-ADD     V1CLNA        KORFIL
052400000000     C     KEYORG        CHAIN     AZORG01L                           99
052500971020IF  1C     *IN99         IFEQ      *OFF
052600971020     C                   MOVEL     ORGDES        V1DLNA                         *DESCRIZIONE
052700971020E   1C                   ENDIF
052800971020     C* TERMINAL ARRIVO
052900971020     C                   CLEAR                   DSLV55
053000971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
053100971020     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
053200971020     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
053300070406     C                   Z-ADD     d92dsp        D55DRF                         *DATA RIFERIMENTO
053400070406     C****               Z-ADD     DATCOR        D55DRF                         *DATA RIFERIMENTO
053500971020     C                   CALL      'FNLV55R'
053600971020     C                   PARM                    DSLV55
053700971020     C                   Z-ADD     D55TFA        V1CTFA                         *TERMINAL ARRIVO
053800971020     C                   Z-ADD     V1CTFA        KORFIL
053900971020     C     KEYORG        CHAIN     AZORG01L                           99
054000971020IF  1C     *IN99         IFEQ      *OFF
054100971020     C                   MOVEL     ORGDES        V1DTFA                         *DESCRIZIONE
054200971020E   1C                   ENDIF
054300960926     C* TASSAZIONE
054400960926     C                   Z-ADD     D92NCL        V1CNCL                         *COLLI
054500960926     C                   Z-ADD     D92PKB        V1CPKB                         *PESO
054600960926     C                   Z-ADD     D92VLF        V1CVLF                         *VOLUME
054700960926     C                   Z-ADD     D92CAS        V1CCAS                         *IMPORTO C/ASSEGNO
054800960926     C                   MOVEL     D92VCA        V1CVCA                         *VALUTA  C/ASSEGNO
054900960926     C                   MOVEL     D92TIC        V1CTIC                         *INCASSO C/ASSENGO
055000960926     C                   MOVEL     D92VAS        V1CVAS                         *VALUTA ASSICURAZION
055100960926     C                   Z-ADD     D92IAS        V1CIAS                         *ASSICURAZIONE
055200050415     C                   Z-ADD     D92qft        V1Cqft                         *ASSICURAZIONE
055300020813     C                   Z-ADD     D92SCO        V1CSCO                         *% DI SCONTO
055400000000     C***
055500000000     C* LANCIA PROGRAMMA CALCOLO DELIVERY
055600000000     C***
055700971009     C                   CLEAR                   DSSD99
055800000000     C                   MOVEL     'P'           D98TLA                         *TIPO LANCIO
055900000000     C                   Z-ADD     DSPV1C        D98DSP                         *DATA SPEDIZIONE
056000000000     C                   Z-ADD     V1CLNP        D98LNP                         *LINEA DI PARTENZA
056100000000     C                   Z-ADD     V1CTFP        D98TFP                         *TERMINAL PARTENZA
056200000000     C                   MOVEL     V1CNAR        D98NZD                         *NAZIONE DI DESTINO
056300000000     C                   MOVEL     V1CCAD        D98CAD                         *CAP DI DESTINO
056400100715     C**                 MOVEL     V1CLOD        D98LOD                         *LOCALITA' DESTINO
056500100715     C                   MOVEL     V1CLOD35      D98LOD                         *LOCALITA' DESTINO
056600000000     C                   Z-ADD     V1CLNA        D98LNA                         *LINEA DI ARRIVO
056700000000     C                   Z-ADD     V1CTFA        D98TFA                         *TERMINAL DI ARRIVO
056800000000     C                   MOVEL     V1CTSP        D98TSP                         *TIPO SPEDIZIONE
056900000000     C                   MOVEL     D92ISO        D98FIN                         *FLAG INOLTRO-CAP IS
057000000000     C                   Z-ADD     1             D98NCD                         *BOLLA CON COLLI
057100971009     C                   CALL      'TNSD99R'
057200971009     C                   PARM                    DSSD99
057300091222     c
057400000000IF  1C     D98ERR        IFEQ      '0'                                          * N O  ERRORI
057500091222     c* se tipo servizio "H" e non mi ha restituito il tempo 10
057600091222     c*  significa che non è località servita quindi esco con errore
057700091222    2c                   if        v1ctsp='H' and d98oee<>1030
057800091222     C                   MOVEL     '1'           $FINE
057900091222     C                   MOVEL     '1'           D92ERR
058000091222     C                   eval      D92MSG=err(2)
058100110509     c                   eval      d92msg = %trim(d92msg) + ' "' +
058200110510     c                             v1ctsp + '-' + §5edes
058300110510     c                   eval      d92msg = %trim(d92msg) + '"'
058400091222   x2c                   else
058500091222     c*
058600000000     C                   Z-ADD     D98DEE        G08INV
058700000000     C                   Z-ADD     *ZEROS        G08DAT
058800000000     C                   MOVEL     '3'           G08ERR
058900000000     C                   CALL      'XSRDA8'
059000000000     C                   PARM                    WLBDA8
059100000000     C                   Z-ADD     G08DAT        V1CDEE                         *DATA TEOR.CONS.CLI.
059200050505     C                   Z-ADD     D98OEE        V1COEE            4 0          *ORA  TEOR.CONS.CLI.
059300050505     c* Decodifica
059400091222    3c                   select
059500091222     c                   when      v1coee=1030
059600091222     c                   eval      v1doee=dec(13)
059700091222     c                   when      v1coee=1200
059800091222     c                   eval      v1doee=dec(12)
059900091222     c                   other
060000050505     c                   eval      v1doee=dec(11)
060100091222    3c                   endsl
060200000000     C*
060300000000     C                   Z-ADD     D98TPC        V1CTPC                         *ORA  TEOR.PART.CLI.
060400000000     C                   Z-ADD     D98TTC        V1CTTC                         *ORA  TEOR.TRAZ.CLI.
060500000000     C                   Z-ADD     D98TCC        V1CTCC                         *ORA  TEOR.CONS.CLI.
060600000000     C                   Z-ADD     D98TRC        V1CTRC                         *ORA  TEOR.RESA CLI.
060700000000     C*
060800000000     C* GIORNO DELLA SETTIMANA DATA CALCOLATA
060900000000     C                   Z-ADD     *ZEROS        KCLTFP
061000000000     C                   Z-ADD     *ZEROS        KCLTFA
061100000000     C                   Z-ADD     DEEANN        KCLANN
061200000000     C                   Z-ADD     DEEMES        KCLMES
061300000000     C     KEYCLN        CHAIN     AZCLN01L                           99
061400091222IF  3C     *IN99         IFEQ      *OFF
061500950419     C                   MOVEA     CLNMAT        MES                            *GIORNI DEL MESE
061600950419     C                   Z-ADD     DEEGIO        NG                2 0          *NUMERO GIORNO MESE
061700950419     C                   MOVEL     MES(NG)       G                 1 0          *GG SETTIMANA (1÷7)
061800950419     C                   MOVEL     DGIO(G)       V1DDEE                         *DECODIFICA GIORNO
061900091222E   3C                   ENDIF
062000950419     C*
062100110622     C* SE FORZATURA tipo inoltro
062200110622     C                   MOVEL     1             KTBKUT
062300110622     C                   MOVEL     '9A'          KTBCOD
062400110622     C                   MOVEL     *BLANKS       KTBKEY
062500110622     C                   MOVEL     D92iso        KTBKEY
062600110622     C     KEYTAB        CHAIN     TABEL00F                           99
062700110622IF  1C     *IN99         IFEQ      *OFF
062800110622     C     TBLFLG        ANDEQ     *BLANKS
062900110622     C                   MOVEL     TBLUNI        DS9a
063000110623     c                   eval      v1dtcc='Forza '+§9adesbr
063100110623    1c                   endif
063200110622IF  3C**   D92ISO        IFEQ      'I'
063300110622     C**                 MOVEL     DEC(5)        V1DTCC                         *DECOD. ORE CONSEGNA
063400110622E   3C**                 ENDIF
063500110622IF  3C**   D92ISO        IFEQ      'D'
063600110622     C**                 MOVEL     DEC(8)        V1DTCC                         *DECOD. ORE CONSEGNA
063700110622E   3C**                 ENDIF
063800110622IF  3C**   D92ISO        IFEQ      'P'
063900110623     C**                 MOVEL     DEC(9)        V1DTCC                         *DECOD. ORE CONSEGNA
064000110623E   3C**                 ENDIF
064100110623IF  3C**   D92ISO        IFEQ      'C'
064200110623     C**                 MOVEL     DEC(10)       V1DTCC                         *DECOD. ORE CONSEGNA
064300110623E   3C**                 ENDIF
064400110623     c
064500091222E   2C                   ENDIF
064600091222X   1C                   ELSE                                                   * S I  ERRORI
064700950531     C                   MOVEL     '1'           $FINE
064800950531     C                   MOVEL     D98ERR        D92ERR
064900950531     C                   MOVEL     D98MSG        D92MSG
065000091222E   1C                   ENDIF
065100050505     c                   eval      v2dtcc=v1dtcc
065200960528     C***
065300960528     C* LANCIA PROGRAMMA TASSAZIONE SINGOLA BOLLA
065400960528     C***
065500960729IF  1C     D92NCL        IFGT      *ZEROS
065600960729IF  2C     $ERR          IFEQ      '0'
065700991123     C                   CLEAR                   DTAS                           *PULISCE DS DI LANCI
065800991123     C                   CLEAR                   DTASV
065900970704     C                   MOVEL     'L'           TASTLA                         *ELABORA E CHIUDE
066000960703     C                   MOVEL     D92KSC        TASKSC
066100960703     C                   MOVEL     D92CTR        TASCTR
066200960703     C                   MOVEL     D92KSC        V1CKSC                         *VIDEO
066300960703     C                   MOVEL     D92CTR        V1CCTR
066400980703     C*
066500960703     C* CALCOLA I SINGOLI ELEMENTI DI TASSAZIONE
066600991124IF  3C     D92TPO        IFEQ      'F'                                          *FRANCO->DATI DESTIN
066700980703     C                   MOVEL     'F1'          TASTBL                         *TIPO BOLLA
066800991124X   3C                   ELSE                                                   *ASSEGNATO->DATI MIT
066900980703     C                   MOVEL     'A2'          TASTBL                         *TIPO BOLLA
067000980703     C                   MOVEL     DEPMCT        TASMCT                         *CODICE TASSAZIONE
067100980703     C                   MOVEL     DEPFAP        TASFAP                         *FLAG INOLTRO
067200991124E   3C                   ENDIF
067300050428     c* Se consegna a magazzino, flag anteporto='C'
067400050428     c                   if        d92fdn='S'
067500050428     c                   eval      tasfap='C'
067600050428     c                   endif
067700050428     c
067800000919     C                   MOVEL     D92FDN        TASFDN                         *MERCE A MAGAZZINO
067900960528     C                   Z-ADD     D92NCL        TASNCL                         *COLLI
068000960528     C                   Z-ADD     D92PKB        TASPKF                         *PESO
068100960528     C                   Z-ADD     D92VLF        TASVLF                         *VOLUME
068200960926     C                   Z-ADD     D92CAS        TASCAS                         *IMPORTO C/ASSEGNO
068300960926     C                   MOVEL     D92VCA        TASVCA                         *VALUTA  C/ASSEGNO
068400960926     C                   MOVEL     D92TIC        TASTIC                         *INCASSO C/ASSENGO
068500960926     C                   MOVEL     D92VAS        TASVAS                         *VALUTA ASSICURAZION
068600960926     C                   Z-ADD     D92IAS        TASIAS                         *ASSICURAZIONE
068700050415     C                   Z-ADD     D92qft        TASqft                         *ASSICURAZIONE
068800960528     C                   Z-ADD     D92DSP        TASDSP                         *DATA SPEDIZIONE
068900970414     C                   Z-ADD     D92DSP        TASDCT                         *DATA CAR TAR CAR
069000991123     C                   MOVEL     D92NAR        TASNZD                         *NAZIONE DESTINO
069100991123     C                   MOVEL     D92NZM        TASNZM                         *NAZIONE MITTENTE
069200960528     C                   MOVEL     D92CAD        TASCAD                         *CAP DESTINATARIO
069300980703     C                   MOVEL     O95CTS        TASCTS                         *CODICE TASSAZIONE
069400010823     C                   MOVEL     O95ISO        TASFIN                         *FLAG INOLTRO
069500010823     C                   MOVEL     §GEDCN        TASDIV                         *DIVISA NON LA SO
069600991124IF  3C     D92ISO        IFNE      *BLANKS
069700960528     C                   MOVEL     D92ISO        TASFIN                         INOLTRO ISOLA FORZAT
069800991124E   3C                   ENDIF
069900050428     c* Se fermo deposito flag inoltro = 'C'
070000050428     c                   if        d92ffd='S'
070100050428     c                   movel     'C'           tasfin
070200050428     c                   endif
070300050413     C                   MOVEL     V1cTC1        TASTC1
070400050413     C                   MOVEL     V1cTC2        TASTC2
070500960528     C                   MOVEL     D92TSP        TASTSP                         *TIPO SERVIZIO
070600960528     C                   Z-ADD     D92LNP        TASLNP                         *LINEA PARTENZA
070700020514     C                   Z-ADD     O95LNA        TASLNA                         *LINEA ARRIVO
070800020514     C* IMPOSTO LA LINEA DI ARRIVA IN RELAZIONE AL NETWROK
070900020514     C     D92NTW        IFEQ      'DPD'
071000020514     C                   Z-ADD     1             I
071100020514     C     D92NAR        LOOKUP    NAR(I)                                 55
071200020514     C                   IF        %FOUND
071300020514     C                   Z-ADD     NARLAD(I)     TASLNA                         *LINEA ARRIVO
071400020514     C                   ENDIF
071500020514     C                   ENDIF
071600970414     C                   MOVEL     D92DDT        TASDDT                         *SI/NO DDT
071700110818     c                   CLEAR                   DTAS01
071800130925     c***                movel     d92emd        §asemd
071900130925     c                   select
072000130925     c                   when      d92emd='S'
072100130925     c                   eval      §asemd='X'
072200130925     c                   when      d92emd='M'
072300130925     c                   eval      §asemd='S'
072400130925     c                   endsl
072500110818     c                   EVAL      TASFLO = DTAS01
072600030618      * se codice cliente è una cartello primi 3 caratteri = a 888 aggiungo il calcolo della varia D
072700030619      * recuperandolo da cliente linea partenza + 8888 se franco o + 9999 se assegnato
072800030618     c                   movel     tasksc        codfil            3 0
072900030618     c                   if        codfil = 888
073000030619      * costruisco il codice cliente
073100030619     c                   clear                   codksc            7 0
073200030619     c                   eval      codksc = d92lnp * 10000
073300030619      * franco
073400030619     c                   if        d92tpo = 'F'
073500030619      * costruisco il codioce cliente
073600030619     c                   add       8888          codksc
073700030619      * assegnato
073800030619     c                   else
073900030619     c                   add       9999          codksc
074000030619     c                   endif
074100030619      *
074200030619     c                   clear                   tibs69ds
074300030619     c                   eval      i69kin = codksc
074400030619     c                   eval      i69tla = 'L'
074500030619      * cerco anagrafica
074600030619      *
074700030619     C                   CALL      'TIBS69R'
074800030619     C                   PARM                    tibs69DS
074900030619     C                   PARM                    DS_cnaco
075000030619     C                   PARM                    DS_cnind
075100030619     C                   PARM                    DS_cnclp
075200030619     C                   PARM                    DS_fncls
075300030619      *
075400030619     c                   if        o69err = ' '
075500030619     c                   move      indsin        tasivdf
075600030618     c                   move      'D'           tassvdf
075700030618     c                   endif
075800030618      *
075900030619     c                   endif
076000030619      *
076100960528     C                   CALL      'TNSF20R'
076200960528     C                   PARM                    KPJBA
076300991123     C                   PARM                    DTAS
076400991123     C                   PARM                    DTASV
076500960716     C*
076600960716     C* CALCOLA IL TOTALE FATTURA
076700991124IF  3C     TASERR        IFEQ      *BLANKS                                      * N O  ERRORI
076800020813     C*
076900020813     C* APPLICA LA % DI SCONTO, SE INSERITA, SU TUTTI GLI IMPORTI
077000020813IF  4C     D92SCO        IFGT      *ZEROS
077100020813     C                   EXSR      APPSCO
077200020813E   4C                   ENDIF
077300020813     C*
077400991123     C                   CLEAR                   DSLV04
077500960716     C                   MOVEL     'L'           D04TLA                         *ELABORA E CHIUDE
077600991124     C                   Z-ADD     TASIMV        D04IMV                         *IMPONIBILE
077700030619     C                   EXSR      RIEGEI                                       *REPERISCE IMPORTI V
077800991123     C                   MOVEL     §GEAED        D04AED                         *ARROTONDAMENTO
077900991123     C                   Z-ADD     §GEAVL        D04VAR                         *TIPO ARROTONDAMENTO
078000960528     C                   MOVEL     '1'           D04SPE                         *TASSAZIONE 1^BOLLA
078100960926     C                   Z-ADD     D92CAS        D04CAS                         *IMPORTO C/ASSEGNO
078200960926     C                   MOVEL     D92VCA        D04VCA                         *VALUTA  C/ASSEGNO
078300960926     C                   MOVEL     D92TIC        D04TIC                         *INCASSO C/ASSENGO
078400980224     C                   MOVEL     D92DSP        D04DFT                         *DATA FATTURA
078500020108     C                   MOVEL     §GEDCN        D04DIV                         *DIVISA
078600991123     C                   CALL      'FNLV04R'
078700991123     C                   PARM                    DSLV04
078800991123     C                   PARM                    DTASV
078900971009     C                   Z-ADD     D04IMV        V1CPOR                         *PORTO
079000971009     C                   Z-ADD     D04ALI        V1CALI                         *ALIQUOTA
079100971009     C                   Z-ADD     D04IVA        V1CIVA                         *IVA
079200971009     C                   Z-ADD     D04IFT        V1CIFT                         *IMPONIBILE+IVA
079300991124     C                   MOVEL     TASDIV        V1CDIV                         *DIVISA
079400050428     c
079500050415     c* Aggiungo l'arrotondamento al porto o alla prima varia buona
079600050415    3C                   SELECT
079700050415     C     tasPOR        WHENGT    0
079800050415     C                   add       D04POR        taspoR
079900050415     C     var(1)        WHENGT    0
080000050415     C     vsv(1)        ANDNE     §$2BOL
080100050415     C     vsv(1)        ANDNE     §$2IVA
080200050415     C                   ADD       D04POR        var(1)
080300050415     C     var(2)        WHENGT    0
080400050415     C     vsv(2)        ANDNE     §$2BOL
080500050415     C     vsv(2)        ANDNE     §$2IVA
080600050415     C                   ADD       D04POR        var(2)
080700050415     C     var(3)        WHENGT    0
080800050415     C                   ADD       D04POR        var(3)
080900050415    3C                   ENDSL
081000960729X   3C                   ELSE                                                   * S I  ERRORI
081100960529     C                   MOVEL     '1'           $ERR
081200960530     C                   MOVEL     ERR(1)        $MSG
081300960529     C                   EXSR      CARMSG
081400960729E   3C                   ENDIF
081500960729E   2C                   ENDIF
081600960729X   1C                   ELSE
081700971009     C                   Z-ADD     *ZEROS        V1CPOR                         *TRASPORTO
081800971009     C                   Z-ADD     *ZEROS        V1CALI                         *ALIQUOTA
081900971009     C                   Z-ADD     *ZEROS        V1CIVA                         *IVA
082000971009     C                   Z-ADD     *ZEROS        V1CIFT                         *IMPONIBILE VUOTO
082100991124     C                   MOVEL     *BLANKS       V1CDIV                         *DIVISA
082200960604E   1C                   ENDIF
082300000000     C*
082400000000     C                   ENDSR
082500020813     C*------------------------------------------------------------------------*
082600020813     C* APPLICA LA % DI SCONTO, SE INSERITA, SU TUTTI GLI IMPORTI
082700020813     C*------------------------------------------------------------------------*
082800020813     C     APPSCO        BEGSR
082900020813     C*
083000020813     C* Calcola la % da applicare per ottener l'importo scontato
083100020813     C                   EVAL      WPer = 100 - D92SCO
083200020813     C*---
083300020813     C* Totale imponibile
083400020813     C*---
083500050428     C                   EVAL(H)   tasimv = (tasimv * WPer) / 100
083600020813     C*---
083700020813     C* Varie
083800020813     C*---
083900020813do  1c     1             DO        20            v                 5 0
084000020813if  2c                   IF        var(v) > *zeros
084100050428     C                   EVAL(H)   var(v) = (var(v) * WPer) / 100
084200020813e   2c                   ENDIF
084300020813e   1c                   ENDDO
084400050428     C*---
084500050428     C* Trasporto
084600050428     C*---
084700050428     C                   EVAL(H)   taspor = (taspor * WPer) / 100
084800050428     C                   CLEAR                   DSLV16
084900050428     C                   MOVEL     'T'           D17FIM
085000050428     C                   Z-ADD     TASPOR        D17POR
085100050428     C                   CALL      'FNLV16R'
085200050428     C                   PARM                    DSLV16
085300050428     C                   PARM                    DTASV
085400050428     c                   eval      tasimv=o17imv
085500020813     c*
085600020813     C                   ENDSR
085700020225     C*------------------------------------------------------------------------*
085800020225     C* DECNTW - DECODIFICA IL NETWORK
085900020225     C*------------------------------------------------------------------------*
086000020225     C     DECNTW        BEGSR
086100020225     C*
086200020225     C* VERIFICA VALORE IN TABELLA
086300020225     C                   CLEAR                   DSBS02
086400020225     C                   MOVEL     'NTW'         T02COD
086500020225     C                   MOVEL     KNSIF         T02SIF
086600020225     C                   MOVEL     V1CNTW        T02KE1
086700020225     C                   MOVEL     'C'           T02MOD
086800020225     C                   CALL      'TIBS02R'
086900020225     C                   PARM                    KPJBA
087000020225     C                   PARM                    DSBS02
087100020225     C*
087200020225     C     T02ERR        IFEQ      *BLANKS
087300020225     C                   MOVEL     T02UNI        DNTW
087400020225     C                   ENDIF
087500020225     C*
087600020225     C                   ENDSR
087700991123     C*------------------------------------------------------------------------*
087800991123     C* RIEGEI - REPERIENTO IMPORTI VARI
087900991123     C*------------------------------------------------------------------------*
088000991123     C     RIEGEI        BEGSR
088100991123     C*
088200991123     C                   CLEAR                   DSBS02
088300991123     C                   CLEAR                   DGEI
088400991123     C                   MOVEL     'C'           T02MOD
088500991123     C                   MOVEL     KNSIF         T02SIF
088600991123     C                   MOVEL     'GEI'         T02COD
088700991124     C                   MOVEL     TASDIV        T02KE1
088800991123     C                   CALL      'TIBS02R'
088900991123     C                   PARM                    KPJBA
089000991123     C                   PARM                    DSBS02
089100991123IF  1C     T02ERR        IFEQ      *BLANKS
089200991123     C                   MOVEL     T02UNI        DGEI
089300991123E   1C                   ENDIF
089400991123     C*
089500991123     C                   ENDSR
089600951030     C*------------------------------------------------------------------------*
089700951030     C* CARTAB - CARICA TABELLE
089800951030     C*------------------------------------------------------------------------*
089900951027     C     CARTAB        BEGSR
090000960604     C*--------------------
090100960604     C* NAZIONI
090200960604     C*--------------------
090300951027     C                   Z-ADD     0             I
090400020919     C                   Z-ADD     1             KTBKUT
090500951027     C                   MOVEL     '15'          KTBCOD
090600951027     C                   MOVEL     *BLANKS       KTBKEY
090700951027     C     KEYTAB        SETLL     TABEL00F                           99
090800951027     C  N99              READ      TABEL00F                               99
090900951030IF  1C     *IN99         IFEQ      *ON
091000951027     C                   MOVEL     MSG(6)        $MSG
091100951027     C                   EXSR      CARMSG
091200951030E   1C                   ENDIF
091300951030DO  1C     *IN99         DOWEQ     *OFF
091400951027     C     TBLCOD        ANDEQ     '15'
091500951030IF  2C     TBLFLG        IFEQ      *BLANKS
091600951027     C                   ADD       1             I
091700951027     C                   MOVEL     TBLKEY        NAR(I)
091800951027     C                   MOVEL     TBLUNI        DS15
091900020514     C                   Z-ADD     §15LAD        NARLAD(I)
092000951030E   2C                   ENDIF
092100951027     C                   READ      TABEL00F                               99
092200951030E   1C                   ENDDO
092300010823     C*--------------------
092400010823     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
092500010823     C*--------------------
092600010823     C                   MOVEL     'GED'         TBECOD
092700010823     C                   MOVEL     *BLANKS       TBEKE1
092800010823     C                   MOVEL     *BLANKS       TBEKE2
092900010823     C                   MOVEL     '1'           TBEKE1
093000010823     C     KTBE          CHAIN     TNTBE01L                           21
093100010823     C  N21              MOVEL     TBEUNI        DGED
093200050415     C*--------------------
093300050415     C* reperisco sigle varie
093400050415     C*--------------------
093500050415     C                   Z-ADD     1             KTBKUT
093600050415     C                   MOVEL     '$2'          KTBCOD
093700050415     C                   MOVEL(P)  '1'           KTBKEY
093800050415     C     KEYTAB        chain     TABEL00F                           99
093900050415     C  N99              MOVEL     TBLUNI        DS$2
094000050415     C   99              CLEAR                   DS$2
094100131016     C*--------------------------------------
094200131016     C* Controllo se filiale utente è già attiva per gli orari servizio
094300131016     C*--------------------------------------
094400131016     c                   eval      TBEcod = 'VPO'
094500131016     c                   eval      TBEke1 = 'DECOFI816'
094600131016     c                   clear                   TBEke2
094700131016     c     KTBE          chain     TNTBE01L
094800131016     c                   IF        %found(TNTBE01L)
094900131016     c                   eval      FilOkOrari = TBEuni
095000131016     c                   ENDIF
095100131016      /free
095200131016         *in04 = *off;
095300131016         IF  skFilOkOrari(1) = '999' or
095400131016             %lookup(%editc(DUtpou:'X'):skFilOkOrari) > 0;
095500131016           *in04 = *on;
095600131016         ENDIF;
095700131016      /end-free
095800951027     C*
095900951027     C                   ENDSR
096000970704     C*------------------------------------------------------------------------*
096100970704     C* CHIUDI  - CHIUDE EVENTUALI PGM CON USCITA RETURN
096200970704     C*------------------------------------------------------------------------*
096300970704     C     CHIUDI        BEGSR
096400970704     C* TISI95R
096500970704     C                   CLEAR                   DSSI95
096600970704     C                   MOVEL     'C'           I95TLA                         'C'=CHIUDI
096700970704     C                   CALL      'TISI95R'                            99
096800970704     C                   PARM                    DSSI95
096900971020     C* TNSD99R
097000971009     C                   CLEAR                   DSSD99
097100970704     C                   MOVEL     'C'           D98TLA                         'C'=CHIUDI
097200971009     C                   CALL      'TNSD99R'                            99
097300971009     C                   PARM                    DSSD99
097400971020     C* FNLV55R
097500971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
097600971020     C                   MOVEL     'C'           D55TLA
097700971020     C                   CALL      'FNLV55R'
097800971020     C                   PARM                    DSLV55
097900970704     C*
098000970704     C                   ENDSR
098100020919     C*--------------------------------------------------------------------------------------------*
098200020919     C* REPERISCE I DATI UTENTE
098300020919     C*--------------------------------------------------------------------------------------------*
098400020919     C     REPDATIUTE    BEGSR
098500020919     C*
098600020919     C* INIZIALIZZA VARIABILI DI WRK
098700020919     C                   CLEAR                   TIBS34DS
098800020919     C                   CLEAR                   AZUTEDS
098900020919     C                   CLEAR                   DDATIUTE
099000020919     C*
099100020919     C     *DTAARA       DEFINE    §azute        azuteds
099200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
099300020919     C                   IN(E)     *DTAARA
099400020919if  1C                   IF        %Error
099500020919     c                   EVAL      I34Tla = 'L'
099600020919     C                   CALL      'TIBS34R'
099700020919     C                   PARM                    Tibs34Ds
099800020919     C                   IN        *DTAARA
099900020919e   1C                   ENDIF
100000020919     C*
100100020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
100200020919     C                   MOVEL(P)  RSUT          DSFIRS           20
100300020919     C*
100400020919     C                   ENDSR
100500000000     C*------------------------------------------------------------------------*
100600000000     C*  *INZSR - ROUTINE INIZIALE
100700000000     C*------------------------------------------------------------------------*
100800000000     C     *INZSR        BEGSR
100900000000     C*--------------------
101000000000     C* RICEVIMENTO PARAMETRI
101100000000     C*--------------------
101200000000     C     *ENTRY        PLIST
101300000000     C                   PARM                    KPJBA
101400970704     C                   MOVEL     KPJBU         DSSD92
101500000000     C*--------------------
101600000000     C* CAMPI RIFERITI AL DATA BASE
101700000000     C*--------------------
101800000000     C     *LIKE         DEFINE    CLNTFP        KCLTFP                         *AZCLN00F
101900000000     C     *LIKE         DEFINE    CLNTFA        KCLTFA
102000000000     C     *LIKE         DEFINE    CLNANN        KCLANN
102100000000     C     *LIKE         DEFINE    CLNMES        KCLMES
102200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
102300000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
102400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
102500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
102600980703     C     *LIKE         DEFINE    TASMCT        DEPMCT                         *DEPOSITI
102700980703     C     *LIKE         DEFINE    TASFAP        DEPFAP
102800020813     C     *LIKE         DEFINE    D92SCO        WPer
102900020813     C*--------------------                                                     *di lavoro
103000000000     C* CHIAVI DI LETTURA
103100000000     C*--------------------
103200970704     C* DI AZCLN01L - COMPLETA
103300000000     C     KEYCLN        KLIST
103400000000     C                   KFLD                    KCLTFP                         *TERMINAL PARTENZA
103500000000     C                   KFLD                    KCLTFA                         *TERMINAL ARRIVO
103600000000     C                   KFLD                    KCLANN                         *ANNO
103700000000     C                   KFLD                    KCLMES                         *MESE
103800970704     C* DI AZORG01L - COMPLETA
103900000000     C     KEYORG        KLIST
104000000000     C                   KFLD                    KORFIL                         *FILIALE
104100970704     C* DI TABEL00F - COMPLETA
104200000000     C     KEYTAB        KLIST
104300000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
104400000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
104500000000     C                   KFLD                    KTBKEY                         *CHIAVE
104600010823     C* DI TNTBE00F - PARZIALE
104700010823     C     KTBE          KLIST
104800010823     C                   KFLD                    TBECOD
104900010823     C                   KFLD                    TBEKE1
105000010823     C                   KFLD                    TBEKE2
105100980703     C*
105200000000     C* CARICA LE DECODIFICHE DEI GIORNI DELLA SETTIMANA
105300000000     C                   MOVEL     DGIO(1)       GIO(1)                         *LUNEDI'
105400000000     C                   MOVEL     DGIO(2)       GIO(2)                         *MARTEDI'
105500000000     C                   MOVEL     DGIO(3)       GIO(3)                         *MERCOLEDI'
105600000000     C                   MOVEL     DGIO(4)       GIO(4)                         *GIOVEDI'
105700000000     C                   MOVEL     DGIO(5)       GIO(5)                         *VENERDI'
105800000000     C                   MOVEL     DGIO(6)       GIO(6)                         *MERCOLEDI'
105900000000     C                   MOVEL     DGIO(7)       GIO(7)                         *GIOVEDI'
106000971020     C*
106100971020     C* REPERISCE LA DATA CORRENTE
106200971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
106300971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
106400971020     C                   Z-ADD     N8            G08DAT
106500971020     C                   Z-ADD     *ZEROS        G08INV
106600971020     C                   MOVEL     '0'           G08ERR
106700971020     C                   CALL      'XSRDA8'
106800971020     C                   PARM                    WLBDA8
106900971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
107000980703     C*
107100980703     C* REPERISCE I DATI MITTENTE PER LA TASSAZIONE
107200980703IF  1C     D92TPO        IFEQ      'A'
107300980703     C                   CLEAR                   DSSI95
107400980703     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
107500980703     C                   MOVEL     '3'           I95TCN                         *CONTROLLO FORMALE
107600980703     C                   MOVEL     D92NZM        I95NAR                         *NAZIONE
107700980703     C                   MOVEL     D92CAM        I95CAP                         *CAP
107800980703     C                   MOVEL     D92LOM        I95LOC                         *LOCALITA'
107900980703     C                   Z-ADD     D92DSP        I95DAT                         *DATA RIFERIMENTO
108000980703     C                   MOVEL     D92TPO        I95TPO                         *TIPO PORTO
108100980703     C                   Z-ADD     D92PKB        I95LKG                         *PESO
108200980703     C                   Z-ADD     D92VLF        I95LMC                         *VOLUME
108300980703     C                   MOVEL     D92TSP        I95TSP                         *TIPO SERVIZIO
108400021028     C                   IF        D92NTW = 'DPD'
108500021028     C                   MOVEL     'D'           I95FI1
108600021028     C                   ENDIF
108700020225     C                   IF        D92NTW = 'FED'
108800020225     C                   MOVEL     'S'           I95FI1
108900020225     C                   ENDIF
109000980703     C                   CALL      'TISI95R'
109100980703     C                   PARM                    DSSI95
109200980703     C                   MOVEL     O95CTS        DEPMCT                         *CODICE TASSAZIONE
109300980703     C                   MOVEL     O95ISO        DEPFAP                         *FLAG INOLTRO
109400980703E   1C                   ENDIF
109500110620     c* Se si tratta di EEX con nazione Danimarca, Estonia, Lituania, Lettonia
109600110620     c*  non devo visulizzare i tempi di consegna
109700110620     c                   if        d92ntw='EEX'
109800110620     c                   eval      Indx =%lookup(d92nar:nareex)
109900110620     c                   eval      *in01=Indx>0
110000110620     c                   endif
110100110620     c
110200000000     C*--------------------
110300000000     C* IMPOSTAZIONI VIDEO
110400000000     C*--------------------
110500000000     C                   MOVEL     'I'           $TGES             1            *INTERROGAZIONE
110600000000     C                   EXSR      INZSTD                                       *OPERAZIONI STANDARD
110700951027     C                   EXSR      CARTAB                                       *CARICA LE TABELLE
110800000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
110900000000     C                   EXSR      CARV1                                        *CARICA PRIMO VIDEO
111000000000     C*
111100000000     C                   ENDSR
111200000000     C*------------------------------------------------------------------------*
111300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
111400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
111500000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
111600000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
111700000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
111800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
111900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112000000000     C*------------------------------------------------------------------------*
112100000000     C* INZSTD - ROUTINE INIZIALE STANDARD
112200000000     C*------------------------------------------------------------------------*
112300000000     C     INZSTD        BEGSR
112400000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
112500000000     C                   CALL      'XKEYS'
112600000000     C                   PARM                    DSKEYS
112700000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
112800000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
112900000000     C     XMSGPA        PLIST
113000000000     C                   PARM                    DSMSMS
113100000000     C                   PARM                    DSMSPG
113200000000     C                   PARM                    DSMSKE
113300000000     C                   PARM                    DSMSER
113400000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
113500970707     C                   MOVEL     'TISI93R'     DSMSPG                         *NOME PGM CHIAMANTE
113600970707     C                   MOVEL     'TISI93R'     VMCQUE                         *CODA MESSAGGI
113700000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
113800000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
113900000000     C* INIZIALIZZO FLAG CONTROLLO PGM
114000000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
114100000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
114200000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
114300000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
114400000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
114500000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
114600000000     C* LANCIO ROUTINE PER VERIFICARE SE
114700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
114800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
114900020919     C                   EXSR      REPDATIUTE
115000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
115100000000     C                   BITOFF    '01234567'    POSMI1            1
115200000000     C                   BITOFF    '01234567'    POSMI2            1
115300000000     C                   BITOFF    '01234567'    POSMA1            1
115400000000     C                   BITOFF    '01234567'    POSMA2            1
115500000000     C                   BITON     '34'          POSMI1            1            *18
115600000000     C                   BITON     '7'           POSMI2            1            *01
115700000000     C                   BITON     '34'          POSMA1            1            *18
115800000000     C                   BITON     '0'           POSMA2            1            *80
115900000000     C                   MOVE      POSMI2        POSMIN            2
116000000000     C                   MOVEL     POSMI1        POSMIN
116100000000     C                   MOVE      POSMA2        POSMAX            2
116200000000     C                   MOVEL     POSMA1        POSMAX
116300000000     C                   ENDSR
116400000000     C*------------------------------------------------------------------------*
116500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
116600000000     C*------------------------------------------------------------------------*
116700000000     C     FUNSTD        BEGSR
116800000000     C                   SELECT
116900000000     C     KEY           WHENEQ    F3
117000000000     C                   EXSR      F03STD
117100000000     C     KEY           WHENEQ    F12
117200000000     C                   EXSR      F12STD
117300000000     C     KEY           WHENEQ    F24
117400000000     C                   EXSR      F24STD
117500050415     C     KEY           WHENEQ    F9
117600050415     C                   EXSR      F09STD
117700050415     C     KEY           WHENEQ    F14
117800050505     c                   setoff                                       40
117900131016     c     KEY           WHENEQ    F16
118000131016     c                   exsr      F16STD
118100050505     C     KEY           WHENEQ    F17
118200050505     C                   EXSR      F17STD
118300000000     C                   OTHER
118400000000     C                   MOVEL     MSG(2)        $MSG
118500000000     C                   EXSR      CARMSG
118600000000     C                   ENDSL
118700000000     C                   ENDSR
118800000000     C*------------------------------------------------------------------------*
118900000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
119000000000     C*------------------------------------------------------------------------*
119100000000     C     F03STD        BEGSR
119200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
119300000000     C                   MOVEL     '1'           $F03
119400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
119500000000     C                   MOVEL     '1'           $FINE
119600000000     C                   ENDSR
119700050415     C*------------------------------------------------------------------------*
119800050415     C* F09STD -GEST.TASTO FUNZ. F09 visalizza dettaglio tassazione
119900050415     C*------------------------------------------------------------------------*
120000050415     C     F09STD        BEGSR
120100050415     c* Pulizia sfl
120200050415     C                   SETON                                        70
120300050415     C                   WRITE     si93wc1
120400050415     C                   SETOFF                                       70
120500050415     c
120600050415     c                   z-add     1             ww                3 0
120700050415     c                   z-add     0             nrr               4 0
120800050428     c                   clear                   vsfsv1
120900050428     c                   setoff                                       02
121000050415     c
121100050415     C* IMPOSTO il porto nella prima posizione del sfl
121200050415     c                   if        taspor>0
121300050428     C                   eval      vsfsv1='Trasporto.:'
121400050608     c**                 z-add     taspor        vsfva1
121500050608     c                   clear                   vsfva1
121600050608     c                   eval      vsfva1=%editc(taspor:'4')
121700050415     c                   add       1             nrr
121800050415     c                   write     si93ws1
121900050415     c                   endif
122000050415     c
122100050415     C* IMPOSTO le varie
122200050415     c                   dow       vsv(ww)<>' '
122300050415     c* Tolgo l'iva, già visualizzata
122400050415     c                   if        vsv(ww)<>§$2iva
122500050428     C                   MOVEL     1             KTBKUT
122600050428     C                   MOVEL     'CC'          KTBCOD
122700050428     C                   MOVEL(P)  'VARIE'       KTBKEY
122800050428     C                   MOVE      vsv(ww)       KTBKEY
122900050428     C     KEYTAB        CHAIN     TABEL00F                           99
123000050428IF  2C     *IN99         IFEQ      *OFF
123100050428     C                   MOVEL     TBLUNI        w009a             9
123200050428     c                   else
123300050428     c                   clear                   w009a
123400050428    2c                   endif
123500050428     c                   eval      vsfsv1=(vsv(ww)+'-'+ w009a)
123600050608     c**                 z-add     var(ww)       vsfva1
123700050608     c                   clear                   vsfva1
123800050608     c                   if        var(WW)>0
123900050608     c                   eval      vsfva1=%editc(var(ww):'4')
124000050608     c                   else
124100050608     c                   move      '0    '       vsfva1
124200050608     c                   endif
124300050608     c
124400050415     c                   add       1             nrr
124500050415     c                   write     si93ws1
124600050428    1c                   endif
124700050415     c
124800050415     c                   add       1             ww
124900050415     c                   enddo
125000050428     c
125100050428     c* Se le varie sono meno di 6 imposto imponibile iVA e totale
125200050428     c* alle posizioni  9 10 11 altrimenti in coda
125300050428     c                   dow       nrr<8
125400050428     c                   clear                   vsfsv1
125500050428     c                   clear                   vsfva1
125600050428     c                   add       1             nrr
125700050428     c                   seton                                        03
125800050428     c                   write     si93ws1
125900050428     c                   enddo
126000050428     c                   setoff                                       03
126100050428     c* Imponibile
126200050428     c                   eval      vsfsv1='Imponibile:'
126300050608     c**                 eval      vsfva1=v1cpor
126400050608     c                   clear                   vsfva1
126500050608     c                   if        v1cpor>0
126600050608     c                   eval      wvalore=v1cpor
126700050608     c                   eval      vsfva1=%editc(wvalore:'4')
126800050608     c                   else
126900050608     c                   move      '0    '       vsfva1
127000050608     c                   endif
127100050428     c                   add       1             nrr
127200050428     c                   write     si93ws1
127300050428     c* IVA
127400050428     c                   eval      vsfsv1='IVA '+ %editc(v1cali:'2')+'..:'
127500050608     c**                 eval      vsfva1=v1civa
127600050608     c                   clear                   vsfva1
127700050608     c                   if        v1civa>0
127800050608     c                   eval      wvalore=v1civa
127900050608     c                   eval      vsfva1=%editc(wvalore:'4')
128000050608     c                   else
128100050608     c                   move      '0    '       vsfva1
128200050608     c                   endif
128300050428     c                   seton                                        02
128400050428     c                   add       1             nrr
128500050428     c                   write     si93ws1
128600050428     c                   setoff                                       02
128700050428     c* totale imponibile
128800050428     c                   eval      vsfsv1='Totale....:'
128900050608     c**                 eval      vsfva1=v1cift
129000050608     c                   clear                   vsfva1
129100050608     c                   if        v1cift>0
129200050608     c                   eval      wvalore=v1cift
129300050608     c                   eval      vsfva1=%editc(wvalore:'4')
129400050608     c                   else
129500050608     c                   move      '0    '       vsfva1
129600050608     c                   endif
129700050428     c                   add       1             nrr
129800050428     c                   write     si93ws1
129900050428     c
130000050415     c*
130100050415     c     for09         tag
130200050415     c                   exfmt     si93wc1
130300050415     c*
130400050415     c                   select
130500050415     c* F3 - fine
130600050415     c                   when      *inkc
130700050415     C                   MOVEL     '1'           $FINE
130800050415     C                   MOVEL     '1'           $F03
130900050415     c* F3 - ritorno
131000050415     c                   when      *inkl
131100050415     C                   MOVEL     '1'           $FINE
131200050415     c* F14- delivery
131300050415     c                   when      *inkn
131400050505     c                   setoff                                       40
131500131017      // F16-OrariSr
131600131017     c                   WHEN      *inkq
131700131017     c                   exsr      F16STD
131800050505     c* F17- ORE delivery
131900050505     c                   when      *inkr
132000050505     c                   seton                                        40
132100050415     c                   other
132200050415     c
132300050415     c                   goto      for09
132400050415     c                   endsl
132500050415     c
132600050415     C                   ENDSR
132700000000     C*------------------------------------------------------------------------*
132800000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
132900000000     C*------------------------------------------------------------------------*
133000000000     C     F12STD        BEGSR
133100000000     C* IMPOSTO FLAG DI FIN PGM
133200000000     C                   MOVEL     '1'           $FINE
133300000000     C                   ENDSR
133400131016
133500131016      /free
133600131016       //--------------------------------------------------------------------
133700131016       //?F16 - Interrogazione orari di servizio.
133800131016       //--------------------------------------------------------------------
133900131016       BEGSR F16STD;
134000131016
134100140203         clear trulorsds;
134200131016         IOREfil = O95lna;
134300131016         IOREcap = D92cad;
134400131016         IOREloc = D92lod;
134500131016         IOREnar = D92nar;
134600140203         //   IOREdta = D92dsp;
134700131016         IOREtser = 'E';
134800131104         IOREtsp = D92tsp;
134900140203         IOREtfp = v1ctfp;
135000140203         IOREtfa = v1ctfa;
135100140203         IOREdei = d98dee;
135200140203         IOREoei = d98oee;
135300140203         IOREdta = d98dce;
135400140203         IOREcol = 35    ;
135500131016
135600140203         trulorsr (kpjba:trulorsds);
135700131017
135800131017         IF  OOREerr <> *blanks;
135900131017           $msg = 'Interrogazione Orari Servizio non possibile.';
136000131017           $msg = %trim($msg) + ' ' + %trim(OOREmsg);
136100131017           exsr carmsg;
136200131017           $err = '1';
136300131017         ENDIF;
136400131016
136500131016       ENDSR;
136600131016
136700131016      /end-free
136800131016
136900050505     C*------------------------------------------------------------------------*
137000050505     C* F17STD -GEST.TASTO FUNZ. F17 ORE delivery
137100050505     C*------------------------------------------------------------------------*
137200050505     C     F17STD        BEGSR
137300050505     C                   seton                                        40
137400050505     C                   ENDSR
137500000000     C*------------------------------------------------------------------------*
137600000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
137700000000     C*------------------------------------------------------------------------*
137800000000     C     F24STD        BEGSR
137900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
138000000000     C                   EXSR      OUTFUN
138100000000     C                   ENDSR
138200000000     C*------------------------------------------------------------------------*
138300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
138400000000     C*          CODA MESSAGGI DEL PGM
138500000000     C*------------------------------------------------------------------------*
138600000000     C     CARMSG        BEGSR
138700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
138800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
138900000000     C     MSGULT        IFEQ      0
139000000000     C                   MOVEL     '1'           *IN40
139100970707     C                   WRITE     SI93DVK
139200000000     C                   MOVEL     '0'           *IN40
139300000000     C                   ENDIF
139400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
139500000000     C                   MOVEL     $MSG          DSMSMS
139600000000     C                   CALL      'XMSG'        XMSGPA
139700000000     C     DSMSER        IFEQ      0
139800000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
139900000000     C                   ENDIF
140000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
140100000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
140200970707     C                   WRITE     SI93DVM                                      *SFL MESS
140300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
140400000000     C                   MOVEL     *BLANKS       $MSG
140500000000     C                   ENDSR
140600000000     C*------------------------------------------------------------------------*
140700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
140800000000     C*------------------------------------------------------------------------*
140900000000     C     OUTMSG        BEGSR
141000000000     C* VISUALIZZAZIONE MESSAGGIO
141100000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
141200970707     C                   WRITE     SI93DVK                                      *CONTR MESS
141300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
141400000000     C                   Z-ADD     *ZEROS        MSGULT
141500000000     C                   ENDSR
141600000000     C*------------------------------------------------------------------------*
141700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
141800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
141900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
142000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
142100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
142200000000     C*------------------------------------------------------------------------*
142300000000     C     CARFUN        BEGSR
142400110620     C* SE 01 ON --> NON VISUALIZZO F17
142500110624     C**                 MOVEL     FUN(17)       FUNRIG                         *DS RIGA FUNZIONI
142600110624     C**                 IF        *IN01
142700110624     c**                 eval      funasg='N'
142800110624     c**                 eval      funasi='N'
142900110624     c**                 eval      funafg='N'
143000110624     c**                 eval      funafi='N'
143100110624     c**                 else
143200110624     c**                 eval      funasg='S'
143300110624     c**                 eval      funasi='S'
143400110624     c*+                 eval      funafg='S'
143500110624     c**                 eval      funafi='S'
143600110624     c**                 endif
143700110624     c**                 movel     funrig        fun(17)
143800131016
143900131016      /free
144000131016       //?Se 04 spento non devo visualizzare il tasto F16=OrariSr
144100131016         funrig = fun(16);
144200131016         IF  not *in04;
144300131017           funasg = 'S';
144400131017           funasi = 'S';
144500131016           funafg = 'N';
144600131016           funafi = 'N';
144700131016         ELSE;
144800131016           funasg = 'S';
144900131017           funasi = 'S';
145000131016           funafg = 'S';
145100131016           funafi = 'S';
145200131016         ENDIF;
145300131016         fun(16) = funrig;
145400131016      /end-free
145500110620     c
145600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
145700000000     C     1             DO        31            I                 5 0          --- 1 -->
145800000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
145900000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
146000000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
146100020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
146200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
146300000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
146400000000     C                   ELSE                                                   *INTERROG.
146500000000     C                   MOVEL     FUNASI        FAT(I)
146600000000     C                   ENDIF
146700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
146800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
146900000000     C                   MOVEL     FUNAFG        FAT(I)
147000000000     C                   ELSE                                                   *INTERROG.
147100000000     C                   MOVEL     FUNAFI        FAT(I)
147200000000     C                   ENDIF
147300000000     C                   ENDIF                                                  <-- 2 ---
147400000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
147500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
147600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
147700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
147800000000     C                   ELSE
147900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
148000000000     C                   ENDIF
148100000000     C                   ENDIF
148200000000     C                   ENDDO                                                  <-- 1 ---
148300000000     C                   ENDSR
148400000000     C*------------------------------------------------------------------------*
148500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
148600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
148700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
148800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
148900000000     C*------------------------------------------------------------------------*
149000000000     C     OUTFUN        BEGSR
149100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
149200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
149300000000     C                   ENDDO
149400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
149500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
149600000000     C     1             DO        31            I
149700000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
149800050415     C**   FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
149900000000     C                   ADD       1             FUNNAT
150000000000     C                   ENDIF
150100000000     C                   ENDDO
150200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
150300000000     C     FUNNAT        IFGT      8
150400000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
150500000000     C                   ENDIF
150600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
150700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
150800000000     C                   Z-ADD     8             FUNVIS            5 0
150900000000     C     FAT(23)       IFEQ      'S'
151000000000     C                   SUB       1             FUNVIS
151100000000     C                   ENDIF
151200000000     C     FAT(24)       IFEQ      'S'
151300000000     C                   SUB       1             FUNVIS
151400000000     C                   ENDIF
151500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
151600000000     C                   Z-ADD     *ZEROS        W                 5 0
151700000000     C                   ADD       1             FUNULT
151800000000     C     FUNULT        IFGT      31
151900000000     C                   Z-ADD     1             FUNULT
152000000000     C                   ENDIF
152100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
152200000000     C     FUNULT        DO        31            I                              --- 1 -->
152300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
152400000000     C     W             IFLT      FUNVIS                                       --- 2 -->
152500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
152600000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
152700050415     C**   FDE(I)        ANDNE     *BLANKS
152800000000     C     I             IFEQ      23                                           *TOLGO F23
152900000000     C     I             OREQ      24                                           *E F24
153000000000     C                   ELSE
153100000000     C                   ADD       1             W
153200000000     C                   MOVEL     FDE(I)        FUV(W)
153300000000     C                   ENDIF
153400000000     C                   ENDIF                                                  <-- 3 ---
153500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
153600000000     C                   Z-ADD     I             FUNULT
153700000000     C                   ENDIF                                                  <-- 2 ---
153800000000     C                   ENDDO                                                  <-- 1 ---
153900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
154000000000     C                   MOVEL     FUV(1)        VFD001
154100000000     C                   MOVEL     FUV(2)        VFD002
154200000000     C                   MOVEL     FUV(3)        VFD003
154300000000     C                   MOVEL     FUV(4)        VFD004
154400000000     C                   MOVEL     FUV(5)        VFD005
154500000000     C                   MOVEL     FUV(6)        VFD006
154600000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
154700000000     C     FAT(24)       ANDEQ     'N'
154800000000     C                   MOVEL     FUV(7)        VFD007
154900000000     C                   MOVEL     FUV(8)        VFD008
155000000000     C                   ELSE                                                   <-- 1 -->
155100000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
155200000000     C     FAT(24)       ANDEQ     'S'
155300000000     C                   MOVEL     FDE(23)       VFD007
155400000000     C                   MOVEL     FDE(24)       VFD008
155500000000     C                   ELSE                                                   <-- 2 -->
155600000000     C                   MOVEL     FUV(7)        VFD007
155700000000     C     FAT(23)       IFEQ      'S'
155800000000     C                   MOVEL     FDE(23)       VFD008
155900000000     C                   ELSE
156000000000     C                   MOVEL     FDE(24)       VFD008
156100000000     C                   ENDIF
156200000000     C                   ENDIF                                                  <-- 2 ---
156300000000     C                   ENDIF                                                  <-- 1 ---
156400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
156500000000     C                   ENDSR
156600000000     C*------------------------------------------------------------------------*
156700000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
156800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
156900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
157000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
157100000000     C*------------------------------------------------------------------------*
157200000000     C     CONFUN        BEGSR
157300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
157400000000     C     1             DO        31            I                              --- 1 -->
157500000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
157600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
157700000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
157800000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
157900000000     C                   EXSR      CARMSG
158000000000     C                   MOVEL     '1'           $ERR
158100000000     C                   ENDIF                                                  <-- 3 ---
158200000000     C                   ENDIF                                                  <-- 2 ---
158300000000     C                   ENDDO                                                  <-- 1 ---
158400000000     C                   ENDSR
158500960530** MSG - MESSAGGI VIDEO
158600970707TISI93R- Tasto funzionale non valido.                                          1
158700970707TISI93R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
158800970707TISI93R- Opzione non valida.                                                   3
158900970707TISI93R- Opzione abilitata ma non gestita. Avvertire CED.                      4
159000970707TISI93R- Inizio elenco.                                                        5
159100970707TISI93R- Fine elenco.                                                          6
159200970707TISI93R- 7
159300970707TISI93R- 8
159400970707TISI93R- 9
159500970707TISI93R- 10
159600970707TISI93R- 11
159700970707TISI93R- 12
159800970707TISI93R- 13
159900970707TISI93R- 14
160000970707TISI93R- 15
160100970707TISI93R- 16
160200970707TISI93R- 17
160300970707TISI93R- 18
160400970707TISI93R- 19
160500970707TISI93R- 20
160600000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
160700000000F1    -                   -    NN     NN
160800000000F2    -                   -    NN     NN
160900000000F3    -F3=Fine            -    SS     SS
161000050428F4    -                   -    NN     NN
161100000000F5    -                   -    NN     NN
161200000000F6    -                   -    NN     NN
161300000000F7    -                   -    NN     NN
161400000000F8    -                   -    NN     NN
161500050415F9    -F9=Dettaglio Tassaz-    SS     SS
161600050415F10   -                   -    SS     SS
161700000000F11   -                   -    NN     NN
161800000000F12   -F12=Ritorno        -    SS     SS
161900000000F13   -                   -    NN     NN
162000050415F14   -F14=Delivery       -    SS     SS
162100000000F15   -                   -    NN     NN
162200131016F16   -F16=OrariSer       -    SS     SS
162300110624F17   -F17=ORE Delivery   -    NN     NN
162400000000F18   -                   -    NN     NN
162500000000F19   -                   -    NN     NN
162600000000F20   -                   -    NN     NN
162700000000F21   -                   -    NN     NN
162800000000F22   -                   -    NN     NN
162900000000F23   -                   -    NN     NN
163000000000F24   -F24=Altri tasti    -    NN     NN
163100000000ENTER -                   -    SS     SS
163200000000ROLLUP-                   -    NN     NN
163300000000ROLLDN-                   -    NN     NN
163400000000HELP  -                   -    NN     NN
163500000000PRINT -                   -    NN     NN
163600000000BSPACE-                   -    NN     NN
163700000000CLEAR -                   -    NN     NN
163800000000** DEC - DECODIFICHE
163900000000ISOLA                    1
164000000000PROVINCIA                2
164100000000CITTA'                   3
164200970704DISAGIATA                4
164300960529FORZ.ISOLA               5
164400971126FRANCO                   6
164500971126ASSEGNATO                7
164600021104FORZ.DISAG               8
164700021104FORZ.PROV.               9
164800021104FORZ.CSTOR               10
164900050505In Giornata  (24:00)     11
165000050505Entro le ore  12:00      12
165100091222Entro le ore  10:30      13
165200110915** DGIO - DECODIFICHE GIORNI DELLA SETTIMANA
165300000000LUNEDI'                  1
165400000000MARTEDI'                 2
165500000000MERCOLEDI'               3
165600000000GIOVEDI'                 4
165700000000VENERDI'                 5
165800000000SABATO                   6
165900000000DOMENICA                 7
166000960530** ERR - ERRORI DI ELABORAZIONE
166100960530TNSF20R- Errori in tassazione                                               1
166200110509La località non è servita dal tipo servizio
1663009605303
1664009605304
1665009605305
166600110620** nareex -  Nazioni con tempi di consegna particolari per EEX
166700110620DK LT LV EE
