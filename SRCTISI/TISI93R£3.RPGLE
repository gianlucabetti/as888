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
018900150729     D*-------------------
019000150729     d* Campo IOREFLO
019100150729     D Diore01       E DS
019200131016
019300131016      // - DS per interrogazione Orari Servizio
019400140203     d TRULORSds     e ds
019500091222     d
019600091222     d Wttc            s              5
019700091222     d WttcCAP         s              5
019800091222     d C               s              3  0
019900110620     d Indx            s              3  0
020000131016
020100131016      //---------------------------------------------------------------
020200131016      //?Definizione procedure utilizzate.
020300131016      //---------------------------------------------------------------
020400131016
020500131016      // -Interrogazione orari di servizio
020600140203     d trulorsr        pr                  extpgm('TRULORSR')
020700131016     d  kpjba                              likeds(KPJBA)
020800140203     d  trulorSds                          likeds(TRULORSDS)
020900131016
021000000000     C*------------------------------------------------------------------------*
021100000000     C* MAIN LINE
021200000000     C*------------------------------------------------------------------------*
021300000000     C*
021400000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
021500000000IF  1C     $FINE         IFEQ      '1'
021600000000     C*
021700000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
021800000000     C                   MOVEL     '1'           D92ERR
021900000000     C*
022000000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
022100000000X   1C                   ELSE
022200000000     C*
022300000000     C* CICLO FINO A CHE:
022400000000DO  2C     $FINE         DOWEQ     '0'
022500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
022600000000     C*
022700000000     C* CONTROLLA GESTIONE VIDEO
022800000000IF  3C     $GES          IFEQ      'V1'
022900000000     C                   EXSR      GESV1                                        *MANUTENZIONE TEMPO
023000000000E   3C                   ENDIF
023100000000     C*
023200000000E   2C                   ENDDO
023300000000E   1C                   ENDIF
023400000000     C*
023500000000     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
023600000000IF  1C     $F03          IFEQ      '1'
023700000000     C                   MOVEL     '1'           D92F03
023800000000E   1C                   ENDIF
023900000000     C*
024000000000     C* SE E' STATO PREMUTO F12 IMPOSTO FLAG DELLA DS DI PROCEDURA
024100000000IF  1C     $FINE         IFEQ      '1'
024200000000     C                   MOVEL     '1'           D92F12
024300000000E   1C                   ENDIF
024400000000     C*
024500970704     C* OPERAZIONI DI CHIUSURA
024600970704     C                   EXSR      CHIUDI                                       *CHIUDE FILE
024700970704     C*
024800970704     C                   MOVEL     DSSD92        KPJBU
024900000000     C                   SETON                                        LR
025000000000     C*------------------------------------------------------------------------*
025100000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
025200000000     C*------------------------------------------------------------------------*
025300000000     C     CARV1         BEGSR
025400000000     C*
025500000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
025600000000     C*
025700000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
025800000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
025900000000     C*
026000970707     C                   WRITE     SI93DVT                                      *TESTATA
026100970707     C                   WRITE     SI93DVF                                      *FUNZIONI
026200000000     C*
026300000000     C                   ENDSR
026400000000     C*------------------------------------------------------------------------*
026500000000     C* GESV1  - GESTIONE PRIMO VIDEO
026600000000     C*------------------------------------------------------------------------*
026700000000     C     GESV1         BEGSR
026800000000     C*
026900000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
027000000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
027100000000E   1C                   ENDIF
027200000000     C*
027300000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
027400000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
027500000000E   1C                   ENDIF
027600000000     C*
027700970707     C                   EXFMT     SI93DV1
027800000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
027900000000     C*
028000000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
028100000000SL  2C                   SELECT
028200000000     C     KEY           WHENEQ    ENTER
028300000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
028400000000     C                   OTHER
028500000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
028600000000E   2C                   ENDSL
028700000000E   1C                   ENDIF
028800000000     C*
028900000000     C                   ENDSR
029000000000     C*------------------------------------------------------------------------*
029100000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
029200000000     C*------------------------------------------------------------------------*
029300000000     C     ENTV1         BEGSR
029400000000     C*
029500000000     C                   ENDSR
029600000000     C*------------------------------------------------------------------------*
029700000000     C* RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
029800000000     C*------------------------------------------------------------------------*
029900000000     C     RECVID        BEGSR
030000000000     C***
030100000000     C* PARAMETRI PASSATI
030200000000     C***
030300140911     c                   setoff                                       11
030400000000     C                   Z-ADD     D92DSP        DSPV1C
030500000000     C                   Z-ADD     D92DSP        G08INV
030600000000     C                   Z-ADD     *ZEROS        G08DAT
030700000000     C                   MOVEL     '3'           G08ERR
030800000000     C                   CALL      'XSRDA8'
030900000000     C                   PARM                    WLBDA8
031000000000     C                   Z-ADD     G08DAT        V1CDSP                         *DATA SPEDIZIONE
031100000000     C                   Z-ADD     D92LNP        V1CLNP                         *LINEA DI PARTENZA
031200000000     C                   MOVEL     D92NAR        V1CNAR                         *NAZIONE
031300000000     C                   MOVEL     D92CAD        V1CCAD                         *CAP
031400970704     C                   MOVEL     D92LOD        V1CLOD                         *LOCALITA'
031500100715     C                   MOVEL     D92LOD        V1CLOD35         35            *LOCALITA'
031600000000     C                   MOVEL     D92TSP        V1CTSP                         *TIPO SERVIZIO
031700980116     C                   MOVEL     D92FFD        V1CFFD                         *FERMO DEPOSITO
031800140911     c                   if        v1cffd='S'
031900140911     c                   seton                                        11
032000140911     c                   endif
032100000919     C                   MOVEL     D92FDN        V1CFDN                         *MERCE A MAGAZZINO
032200980703     C                   MOVEL     D92TPO        V1CTPO                         *TIPO PORTO
032300980703     C                   MOVEL     D92NZM        V1CNZM                         *NAZIONE MITTENTE
032400980703     C                   MOVEL     D92CAM        V1CCAM                         *CAP MITTENTE
032500980703     C                   MOVEL     D92LOM        V1CLOM                         *LOCALITA' MITTENTE
032600020225     C                   MOVEL     D92NTW        V1CNTW                         *NETWORK
032700021111     C                   MOVEL     D92DDT        V1CDDT                         *CONSEGNA DDT
032800050413     C                   MOVEL     D92tc1        V1Ctc1                         *CONSEGNA DDT
032900050413     C                   MOVEL     D92tc2        V1Ctc2                         *CONSEGNA DDT
033000130925     C***                MOVEL     d92emd        V1Cemd                         *avviso email dest
033100130925     c                   select
033200130925     c                   when      d92emd='S'
033300130925     c                   eval      v1cemd='SMS '
033400130925     c                   when      d92emd='M'
033500130925     c                   eval      v1cemd='MAIL'
033600130925     c                   other
033700130925     c                   eval      v1cemd='NO  '
033800130925     c                   endsl
033900000000     C***
034000970704     C* DECODIFICHE PARAMETRI O PARAMETRI DESUNTI
034100000000     C***
034200000000     C* GIORNO DELLA SETTIMANA DATA SPEDIZIONE
034300000000     C                   Z-ADD     *ZEROS        KCLTFP
034400000000     C                   Z-ADD     *ZEROS        KCLTFA
034500000000     C                   Z-ADD     DSPAAS        KCLANN
034600000000     C                   Z-ADD     DSPMES        KCLMES
034700000000     C     KEYCLN        CHAIN     AZCLN01L                           99
034800000000IF  1C     *IN99         IFEQ      *OFF
034900000000     C                   MOVEA     CLNMAT        MES                             *GIORNI DEL MESE
035000000000     C                   Z-ADD     DSPGIO        NG                2 0           *NUMERO GIORNO MESE
035100000000     C                   MOVEL     MES(NG)       G                 1 0           *GG SETTIMANA (1÷7)
035200000000     C                   MOVEL     DGIO(G)       V1DDSP                          *DECODIFICA GIORNO
035300000000E   1C                   ENDIF
035400000623     C*
035500000623     C* DESCRIZIONE FILIALE PARTENZA E REPERIMENTO TERMINAL
035600000623     C                   Z-ADD     V1CLNP        KORFIL
035700000623     C     KEYORG        CHAIN     AZORG01L                           99
035800000623IF  1C     *IN99         IFEQ      *OFF
035900000623     C                   MOVEL     ORGDES        V1DLNP                         *FILIALE
036000000623     C                   MOVEL     ORGDE3        OG143
036100020627IF  2C     §OGNTW        IFEQ      'PPT'                                        *FILIALE POSTE
036200000623     C                   Z-ADD     V1CLNP        V1CTFP                         *TERMINAL = FILIALE
036300000623X   2C                   ELSE                                                   *NO FILIALE POSTE
036400000623     C* RECUPERA TERMINAL
036500971020     C                   CLEAR                   DSLV55
036600971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
036700971020     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
036800971020     C                   Z-ADD     V1CLNP        D55LIN                         *LINEA
036900070406     C***                Z-ADD     DATCOR        D55DRF                         *DATA RIFERIMENTO
037000070406     C                   Z-ADD     d92dsp        D55DRF                         *DATA RIFERIMENTO
037100971020     C                   CALL      'FNLV55R'
037200971020     C                   PARM                    DSLV55
037300971020     C                   Z-ADD     D55TFP        V1CTFP                         *TERMINAL PARTENZA
037400000623E   2C                   ENDIF
037500000623E   1C                   ENDIF
037600971020     C*
037700000623     C* DESCRIZIONE TERMINAL PARTENZA
037800000000     C                   Z-ADD     V1CTFP        KORFIL
037900000000     C     KEYORG        CHAIN     AZORG01L                           99
038000971020IF  1C     *IN99         IFEQ      *OFF
038100971020     C                   MOVEL     ORGDES        V1DTFP                         *TERMINAL
038200971020E   1C                   ENDIF
038300000000     C* NAZIONE
038400020919     C                   MOVEL     1             KTBKUT
038500000000     C                   MOVEL     '15'          KTBCOD
038600000000     C                   MOVEL     *BLANKS       KTBKEY
038700000000     C                   MOVEL     V1CNAR        KTBKEY
038800000000     C     KEYTAB        CHAIN     TABEL00F                           99
038900000000IF  1C     *IN99         IFEQ      *OFF
039000000000     C     TBLFLG        ANDEQ     *BLANKS
039100000000     C                   MOVEL     TBLUNI        DS15
039200000000     C                   MOVEL     §15DES        V1DNAR
039300000000X   1C                   ELSE
039400000000     C                   MOVEL     *ALL'*'       V1DNAR
039500000000E   1C                   ENDIF
039600970704     C* TIPO SERVIZIO
039700020919     C                   MOVEL     1             KTBKUT
039800970704     C                   MOVEL     '5E'          KTBCOD
039900970704     C                   MOVEL     *BLANKS       KTBKEY
040000970704     C                   MOVEL     V1CTSP        KTBKEY
040100970704     C     KEYTAB        CHAIN     TABEL00F                           99
040200970704IF  1C     *IN99         IFEQ      *OFF
040300970704     C     TBLFLG        ANDEQ     *BLANKS
040400970704     C                   MOVEL     TBLUNI        DS5E
040500090605     C                   MOVEL     §5ED08        V1DTSP
040600970704X   1C                   ELSE
040700970704     C                   MOVEL     *ALL'*'       V1DTSP
040800970704E   1C                   ENDIF
040900050413     C* consegne particolari
041000050413     c                   clear                   v1dtc1
041100050413     c                   clear                   v1dtc2
041200050413     c                   if        v1ctc1<>' '
041300050413     C                   MOVEL     1             KTBKUT
041400050413     C                   MOVEL     '1P'          KTBCOD
041500050413     C                   MOVEL     *BLANKS       KTBKEY
041600050413     C                   MOVEL     V1Ctc1        KTBKEY
041700050413     C     KEYTAB        CHAIN     TABEL00F                           99
041800050413IF  1C     *IN99         IFEQ      *OFF
041900050413     C     TBLFLG        ANDEQ     *BLANKS
042000050413     C                   MOVEL     tbluni        V1Dtc1
042100050413X   1C                   ELSE
042200050413     C                   MOVEL     *ALL'*'       V1Dtc1
042300050413E   1C                   ENDIF
042400050413E   1C                   ENDIF
042500050413     C*
042600050413     c                   if        v1ctc2<>' '
042700050413     C                   MOVEL     1             KTBKUT
042800050413     C                   MOVEL     '1P'          KTBCOD
042900050413     C                   MOVEL     *BLANKS       KTBKEY
043000050413     C                   MOVEL     V1Ctc2        KTBKEY
043100050413     C     KEYTAB        CHAIN     TABEL00F                           99
043200050413IF  1C     *IN99         IFEQ      *OFF
043300050413     C     TBLFLG        ANDEQ     *BLANKS
043400050413     C                   MOVEL     tbluni        V1Dtc2
043500050413X   1C                   ELSE
043600050413     C                   MOVEL     *ALL'*'       V1Dtc2
043700050413E   1C                   ENDIF
043800050413E   1C                   ENDIF
043900050413     c**
044000971126     C* TIPO PORTO
044100971126IF  1C     V1CTPO        IFEQ      'F'
044200980120     C                   MOVEL     DEC(6)        V1DTPO
044300971126X   1C                   ELSE
044400980120     C                   MOVEL     DEC(7)        V1DTPO
044500971126E   1C                   ENDIF
044600020226     C* NETWORK
044700020226     C                   EXSR      DECNTW
044800970704     C*---
044900091222     C* LEGGE IL CAP/Località
045000970704     C*---
045100970704     C                   CLEAR                   DSSI95
045200970704     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
045300970704     C                   MOVEL     '3'           I95TCN                         *CONTROLLO FORMALE
045400970704     C                   MOVEL     D92NAR        I95NAR                         *NAZIONE
045500970704     C                   MOVEL     D92CAD        I95CAP                         *CAP
045600970714     C                   MOVEL     D92LOD        I95LOC                         *LOCALITA'
045700970704     C                   Z-ADD     D92DSP        I95DAT                         *DATA RIFERIMENTO
045800971126     C                   MOVEL     D92TPO        I95TPO                         *TIPO PORTO
045900980116     C                   MOVEL     D92FFD        I95FFD                         *FERMO DEPOSITO
046000980119     C                   Z-ADD     D92PKB        I95LKG                         *PESO
046100980119     C                   Z-ADD     D92VLF        I95LMC                         *VOLUME
046200980602     C                   MOVEL     D92TSP        I95TSP                         *TIPO SERVIZIO
046300990303     C                   Z-ADD     V1CTFP        I95TFP                         *TERMINAL PARTENZA
046400020225     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
046500971126IF  1C     D92CAS        IFGT      *ZEROS
046600971126     C                   MOVEL     'S'           I95FCA                         *SI C/ASSEGNO
046700971126X   1C                   ELSE
046800971126     C                   MOVEL     'N'           I95FCA                         *NO C/ASSEGNO
046900971126E   1C                   ENDIF
047000021028     C                   IF        D92NTW = 'DPD'
047100021028     C                   MOVEL     'D'           I95FI1
047200021028     C                   ENDIF
047300020225     C                   IF        D92NTW = 'FED'
047400020225     C                   MOVEL     'S'           I95FI1
047500020225     C                   ENDIF
047600050413     c*
047700050413     c                   movel     v1ctc1        i95fi2
047800050413     c                   move      v1ctc2        i95fi2
047900050413     c*
048000970704     C                   CALL      'TISI95R'
048100970704     C                   PARM                    DSSI95
048200091222     c
048300091222     c* Verifico per la località se il tipo servizio prevede un codice tempo
048400091222     c*  consegna obbligato, anche parziale
048500091222    1c                   if        d92tsp<>' '  and §5ettc<>*blanks
048600091222     c* controllo posizione per posizione escudendo le "x"
048700091222     c                   eval      Wttc=§5ettc
048800091222     c                   eval      WttcCAP=%editc(o95ttc:'X')
048900091222     c                   z-add     1             c
049000091222    3c                   do        5             c
049100091222     c                   if        %subst(wttc:c:1)<>'x'
049200091222     c                             and
049300091222     c                             %subst(wttc:c:1)<>%subst(wttcCAP:c:1)
049400091222     C                   MOVEL     ERR(2)        d92msg
049500110509     c                   eval      d92msg = %trim(d92msg) + ' "' +
049600110510     c                             v1ctsp + '-' + §5edes
049700110510     c                   eval      d92msg = %trim(d92msg) + '"'
049800091222     C                   MOVEL     '1'           d92err
049900091222     C                   MOVEL     '1'           $fine
050000091222     c                   endif
050100091222    3c                   enddo
050200091222    2c                   endif
050300970704     C* INOLTRO
050400110622     C                   MOVEL     1             KTBKUT
050500110622     C                   MOVEL     '9A'          KTBCOD
050600110622     C                   MOVEL     *BLANKS       KTBKEY
050700110622     C                   MOVEL     o95iso        KTBKEY
050800110622     C     KEYTAB        CHAIN     TABEL00F                           99
050900110622IF  1C     *IN99         IFEQ      *OFF
051000110622     C     TBLFLG        ANDEQ     *BLANKS
051100110622     C                   MOVEL     TBLUNI        DS9a
051200110622     c                   eval      v1diso=§9adesbr
051300110622     c                   endif
051400110622     c
051500110622IF  1C***  O95ISO        IFEQ      'I'
051600110622     C***                MOVEL     DEC(1)        V1DISO                         *DECODIFICA CAP
051700110622E   1C***                ENDIF
051800110622IF  1C***  O95ISO        IFEQ      'P'
051900110622     C***                MOVEL     DEC(2)        V1DISO                         *DECODIFICA CAP
052000110622E   1C***                ENDIF
052100110622IF  1C***  O95ISO        IFEQ      'C'
052200110622     C***                MOVEL     DEC(3)        V1DISO                         *DECODIFICA CAP
052300110622E   1C***                ENDIF
052400110622IF  1C***  O95ISO        IFEQ      'D'
052500110622     C***                MOVEL     DEC(4)        V1DISO                         *DECODIFICA CAP
052600110622E   1C***                ENDIF
052700980116     C* LINEA / ZONA ARRIVO
052800970704     C                   Z-ADD     O95LNA        V1CLNA                         *LINEA ARRIVO
052900980116     C                   Z-ADD     O95ZNC        V1CZNC                         *ZONA CONSEGNA
053000000000     C                   Z-ADD     V1CLNA        KORFIL
053100000000     C     KEYORG        CHAIN     AZORG01L                           99
053200971020IF  1C     *IN99         IFEQ      *OFF
053300971020     C                   MOVEL     ORGDES        V1DLNA                         *DESCRIZIONE
053400971020E   1C                   ENDIF
053500971020     C* TERMINAL ARRIVO
053600971020     C                   CLEAR                   DSLV55
053700971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
053800971020     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
053900971020     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
054000070406     C                   Z-ADD     d92dsp        D55DRF                         *DATA RIFERIMENTO
054100070406     C****               Z-ADD     DATCOR        D55DRF                         *DATA RIFERIMENTO
054200971020     C                   CALL      'FNLV55R'
054300971020     C                   PARM                    DSLV55
054400971020     C                   Z-ADD     D55TFA        V1CTFA                         *TERMINAL ARRIVO
054500971020     C                   Z-ADD     V1CTFA        KORFIL
054600971020     C     KEYORG        CHAIN     AZORG01L                           99
054700971020IF  1C     *IN99         IFEQ      *OFF
054800971020     C                   MOVEL     ORGDES        V1DTFA                         *DESCRIZIONE
054900971020E   1C                   ENDIF
055000960926     C* TASSAZIONE
055100960926     C                   Z-ADD     D92NCL        V1CNCL                         *COLLI
055200960926     C                   Z-ADD     D92PKB        V1CPKB                         *PESO
055300960926     C                   Z-ADD     D92VLF        V1CVLF                         *VOLUME
055400960926     C                   Z-ADD     D92CAS        V1CCAS                         *IMPORTO C/ASSEGNO
055500960926     C                   MOVEL     D92VCA        V1CVCA                         *VALUTA  C/ASSEGNO
055600960926     C                   MOVEL     D92TIC        V1CTIC                         *INCASSO C/ASSENGO
055700960926     C                   MOVEL     D92VAS        V1CVAS                         *VALUTA ASSICURAZION
055800960926     C                   Z-ADD     D92IAS        V1CIAS                         *ASSICURAZIONE
055900050415     C                   Z-ADD     D92qft        V1Cqft                         *ASSICURAZIONE
056000020813     C                   Z-ADD     D92SCO        V1CSCO                         *% DI SCONTO
056100000000     C***
056200000000     C* LANCIA PROGRAMMA CALCOLO DELIVERY
056300000000     C***
056400971009     C                   CLEAR                   DSSD99
056500000000     C                   MOVEL     'P'           D98TLA                         *TIPO LANCIO
056600000000     C                   Z-ADD     DSPV1C        D98DSP                         *DATA SPEDIZIONE
056700000000     C                   Z-ADD     V1CLNP        D98LNP                         *LINEA DI PARTENZA
056800000000     C                   Z-ADD     V1CTFP        D98TFP                         *TERMINAL PARTENZA
056900000000     C                   MOVEL     V1CNAR        D98NZD                         *NAZIONE DI DESTINO
057000000000     C                   MOVEL     V1CCAD        D98CAD                         *CAP DI DESTINO
057100100715     C**                 MOVEL     V1CLOD        D98LOD                         *LOCALITA' DESTINO
057200100715     C                   MOVEL     V1CLOD35      D98LOD                         *LOCALITA' DESTINO
057300000000     C                   Z-ADD     V1CLNA        D98LNA                         *LINEA DI ARRIVO
057400000000     C                   Z-ADD     V1CTFA        D98TFA                         *TERMINAL DI ARRIVO
057500000000     C                   MOVEL     V1CTSP        D98TSP                         *TIPO SPEDIZIONE
057600000000     C                   MOVEL     D92ISO        D98FIN                         *FLAG INOLTRO-CAP IS
057700000000     C                   Z-ADD     1             D98NCD                         *BOLLA CON COLLI
057800971009     C                   CALL      'TNSD99R'
057900971009     C                   PARM                    DSSD99
058000091222     c
058100000000IF  1C     D98ERR        IFEQ      '0'                                          * N O  ERRORI
058200091222     c* se tipo servizio "H" e non mi ha restituito il tempo 10
058300091222     c*  significa che non è località servita quindi esco con errore
058400091222    2c                   if        v1ctsp='H' and d98oee<>1030
058500091222     C                   MOVEL     '1'           $FINE
058600091222     C                   MOVEL     '1'           D92ERR
058700091222     C                   eval      D92MSG=err(2)
058800110509     c                   eval      d92msg = %trim(d92msg) + ' "' +
058900110510     c                             v1ctsp + '-' + §5edes
059000110510     c                   eval      d92msg = %trim(d92msg) + '"'
059100091222   x2c                   else
059200091222     c*
059300141103     c                   clear                   g08inv
059400141103     c                   if        d98dee>0
059500000000     C                   Z-ADD     D98DEE        G08INV
059600141103     c                   else
059700141103     c                   if        d98nzd<>*blanks  and d92ntw<>*blanks
059800141103     C                   Z-ADD     D98DcE        G08INV
059900141103     c                   endif
060000141103     c                   endif
060100000000     C                   Z-ADD     *ZEROS        G08DAT
060200000000     C                   MOVEL     '3'           G08ERR
060300000000     C                   CALL      'XSRDA8'
060400000000     C                   PARM                    WLBDA8
060500000000     C                   Z-ADD     G08DAT        V1CDEE                         *DATA TEOR.CONS.CLI.
060600050505     C                   Z-ADD     D98OEE        V1COEE            4 0          *ORA  TEOR.CONS.CLI.
060700050505     c* Decodifica
060800091222    3c                   select
060900091222     c                   when      v1coee=1030
061000091222     c                   eval      v1doee=dec(13)
061100091222     c                   when      v1coee=1200
061200091222     c                   eval      v1doee=dec(12)
061300091222     c                   other
061400050505     c                   eval      v1doee=dec(11)
061500091222    3c                   endsl
061600000000     C*
061700000000     C                   Z-ADD     D98TPC        V1CTPC                         *ORA  TEOR.PART.CLI.
061800000000     C                   Z-ADD     D98TTC        V1CTTC                         *ORA  TEOR.TRAZ.CLI.
061900000000     C                   Z-ADD     D98TCC        V1CTCC                         *ORA  TEOR.CONS.CLI.
062000000000     C                   Z-ADD     D98TRC        V1CTRC                         *ORA  TEOR.RESA CLI.
062100000000     C*
062200000000     C* GIORNO DELLA SETTIMANA DATA CALCOLATA
062300000000     C                   Z-ADD     *ZEROS        KCLTFP
062400000000     C                   Z-ADD     *ZEROS        KCLTFA
062500000000     C                   Z-ADD     DEEANN        KCLANN
062600000000     C                   Z-ADD     DEEMES        KCLMES
062700000000     C     KEYCLN        CHAIN     AZCLN01L                           99
062800091222IF  3C     *IN99         IFEQ      *OFF
062900950419     C                   MOVEA     CLNMAT        MES                            *GIORNI DEL MESE
063000950419     C                   Z-ADD     DEEGIO        NG                2 0          *NUMERO GIORNO MESE
063100950419     C                   MOVEL     MES(NG)       G                 1 0          *GG SETTIMANA (1÷7)
063200950419     C                   MOVEL     DGIO(G)       V1DDEE                         *DECODIFICA GIORNO
063300091222E   3C                   ENDIF
063400950419     C*
063500110622     C* SE FORZATURA tipo inoltro
063600110622     C                   MOVEL     1             KTBKUT
063700110622     C                   MOVEL     '9A'          KTBCOD
063800110622     C                   MOVEL     *BLANKS       KTBKEY
063900110622     C                   MOVEL     D92iso        KTBKEY
064000110622     C     KEYTAB        CHAIN     TABEL00F                           99
064100110622IF  1C     *IN99         IFEQ      *OFF
064200110622     C     TBLFLG        ANDEQ     *BLANKS
064300110622     C                   MOVEL     TBLUNI        DS9a
064400110623     c                   eval      v1dtcc='Forza '+§9adesbr
064500110623    1c                   endif
064600110622IF  3C**   D92ISO        IFEQ      'I'
064700110622     C**                 MOVEL     DEC(5)        V1DTCC                         *DECOD. ORE CONSEGNA
064800110622E   3C**                 ENDIF
064900110622IF  3C**   D92ISO        IFEQ      'D'
065000110622     C**                 MOVEL     DEC(8)        V1DTCC                         *DECOD. ORE CONSEGNA
065100110622E   3C**                 ENDIF
065200110622IF  3C**   D92ISO        IFEQ      'P'
065300110623     C**                 MOVEL     DEC(9)        V1DTCC                         *DECOD. ORE CONSEGNA
065400110623E   3C**                 ENDIF
065500110623IF  3C**   D92ISO        IFEQ      'C'
065600110623     C**                 MOVEL     DEC(10)       V1DTCC                         *DECOD. ORE CONSEGNA
065700110623E   3C**                 ENDIF
065800110623     c
065900091222E   2C                   ENDIF
066000091222X   1C                   ELSE                                                   * S I  ERRORI
066100950531     C                   MOVEL     '1'           $FINE
066200950531     C                   MOVEL     D98ERR        D92ERR
066300950531     C                   MOVEL     D98MSG        D92MSG
066400091222E   1C                   ENDIF
066500050505     c                   eval      v2dtcc=v1dtcc
066600960528     C***
066700960528     C* LANCIA PROGRAMMA TASSAZIONE SINGOLA BOLLA
066800960528     C***
066900960729IF  1C     D92NCL        IFGT      *ZEROS
067000960729IF  2C     $ERR          IFEQ      '0'
067100991123     C                   CLEAR                   DTAS                           *PULISCE DS DI LANCI
067200991123     C                   CLEAR                   DTASV
067300970704     C                   MOVEL     'L'           TASTLA                         *ELABORA E CHIUDE
067400960703     C                   MOVEL     D92KSC        TASKSC
067500960703     C                   MOVEL     D92CTR        TASCTR
067600960703     C                   MOVEL     D92KSC        V1CKSC                         *VIDEO
067700960703     C                   MOVEL     D92CTR        V1CCTR
067800980703     C*
067900960703     C* CALCOLA I SINGOLI ELEMENTI DI TASSAZIONE
068000991124IF  3C     D92TPO        IFEQ      'F'                                          *FRANCO->DATI DESTIN
068100980703     C                   MOVEL     'F1'          TASTBL                         *TIPO BOLLA
068200991124X   3C                   ELSE                                                   *ASSEGNATO->DATI MIT
068300980703     C                   MOVEL     'A2'          TASTBL                         *TIPO BOLLA
068400980703     C                   MOVEL     DEPMCT        TASMCT                         *CODICE TASSAZIONE
068500980703     C                   MOVEL     DEPFAP        TASFAP                         *FLAG INOLTRO
068600991124E   3C                   ENDIF
068700050428     c* Se consegna a magazzino, flag anteporto='C'
068800050428     c                   if        d92fdn='S'
068900050428     c                   eval      tasfap='C'
069000050428     c                   endif
069100050428     c
069200000919     C                   MOVEL     D92FDN        TASFDN                         *MERCE A MAGAZZINO
069300960528     C                   Z-ADD     D92NCL        TASNCL                         *COLLI
069400960528     C                   Z-ADD     D92PKB        TASPKF                         *PESO
069500960528     C                   Z-ADD     D92VLF        TASVLF                         *VOLUME
069600960926     C                   Z-ADD     D92CAS        TASCAS                         *IMPORTO C/ASSEGNO
069700960926     C                   MOVEL     D92VCA        TASVCA                         *VALUTA  C/ASSEGNO
069800960926     C                   MOVEL     D92TIC        TASTIC                         *INCASSO C/ASSENGO
069900960926     C                   MOVEL     D92VAS        TASVAS                         *VALUTA ASSICURAZION
070000960926     C                   Z-ADD     D92IAS        TASIAS                         *ASSICURAZIONE
070100050415     C                   Z-ADD     D92qft        TASqft                         *ASSICURAZIONE
070200960528     C                   Z-ADD     D92DSP        TASDSP                         *DATA SPEDIZIONE
070300970414     C                   Z-ADD     D92DSP        TASDCT                         *DATA CAR TAR CAR
070400991123     C                   MOVEL     D92NAR        TASNZD                         *NAZIONE DESTINO
070500991123     C                   MOVEL     D92NZM        TASNZM                         *NAZIONE MITTENTE
070600960528     C                   MOVEL     D92CAD        TASCAD                         *CAP DESTINATARIO
070700980703     C                   MOVEL     O95CTS        TASCTS                         *CODICE TASSAZIONE
070800010823     C                   MOVEL     O95ISO        TASFIN                         *FLAG INOLTRO
070900010823     C                   MOVEL     §GEDCN        TASDIV                         *DIVISA NON LA SO
071000991124IF  3C     D92ISO        IFNE      *BLANKS
071100960528     C                   MOVEL     D92ISO        TASFIN                         INOLTRO ISOLA FORZAT
071200991124E   3C                   ENDIF
071300050428     c* Se fermo deposito flag inoltro = 'C'
071400050428     c                   if        d92ffd='S'
071500050428     c                   movel     'C'           tasfin
071600050428     c                   endif
071700050413     C                   MOVEL     V1cTC1        TASTC1
071800050413     C                   MOVEL     V1cTC2        TASTC2
071900960528     C                   MOVEL     D92TSP        TASTSP                         *TIPO SERVIZIO
072000960528     C                   Z-ADD     D92LNP        TASLNP                         *LINEA PARTENZA
072100020514     C                   Z-ADD     O95LNA        TASLNA                         *LINEA ARRIVO
072200020514     C* IMPOSTO LA LINEA DI ARRIVA IN RELAZIONE AL NETWROK
072300020514     C     D92NTW        IFEQ      'DPD'
072400020514     C                   Z-ADD     1             I
072500020514     C     D92NAR        LOOKUP    NAR(I)                                 55
072600020514     C                   IF        %FOUND
072700020514     C                   Z-ADD     NARLAD(I)     TASLNA                         *LINEA ARRIVO
072800020514     C                   ENDIF
072900020514     C                   ENDIF
073000970414     C                   MOVEL     D92DDT        TASDDT                         *SI/NO DDT
073100110818     c                   CLEAR                   DTAS01
073200130925     c***                movel     d92emd        §asemd
073300130925     c                   select
073400130925     c                   when      d92emd='S'
073500130925     c                   eval      §asemd='X'
073600130925     c                   when      d92emd='M'
073700130925     c                   eval      §asemd='S'
073800130925     c                   endsl
073900110818     c                   EVAL      TASFLO = DTAS01
074000030618      * se codice cliente è una cartello primi 3 caratteri = a 888 aggiungo il calcolo della varia D
074100030619      * recuperandolo da cliente linea partenza + 8888 se franco o + 9999 se assegnato
074200030618     c                   movel     tasksc        codfil            3 0
074300030618     c                   if        codfil = 888
074400030619      * costruisco il codice cliente
074500030619     c                   clear                   codksc            7 0
074600030619     c                   eval      codksc = d92lnp * 10000
074700030619      * franco
074800030619     c                   if        d92tpo = 'F'
074900030619      * costruisco il codioce cliente
075000030619     c                   add       8888          codksc
075100030619      * assegnato
075200030619     c                   else
075300030619     c                   add       9999          codksc
075400030619     c                   endif
075500030619      *
075600030619     c                   clear                   tibs69ds
075700030619     c                   eval      i69kin = codksc
075800030619     c                   eval      i69tla = 'L'
075900030619      * cerco anagrafica
076000030619      *
076100030619     C                   CALL      'TIBS69R'
076200030619     C                   PARM                    tibs69DS
076300030619     C                   PARM                    DS_cnaco
076400030619     C                   PARM                    DS_cnind
076500030619     C                   PARM                    DS_cnclp
076600030619     C                   PARM                    DS_fncls
076700030619      *
076800030619     c                   if        o69err = ' '
076900030619     c                   move      indsin        tasivdf
077000030618     c                   move      'D'           tassvdf
077100030618     c                   endif
077200030618      *
077300030619     c                   endif
077400030619      *
077500960528     C                   CALL      'TNSF20R'
077600960528     C                   PARM                    KPJBA
077700991123     C                   PARM                    DTAS
077800991123     C                   PARM                    DTASV
077900960716     C*
078000960716     C* CALCOLA IL TOTALE FATTURA
078100991124IF  3C     TASERR        IFEQ      *BLANKS                                      * N O  ERRORI
078200020813     C*
078300020813     C* APPLICA LA % DI SCONTO, SE INSERITA, SU TUTTI GLI IMPORTI
078400020813IF  4C     D92SCO        IFGT      *ZEROS
078500020813     C                   EXSR      APPSCO
078600020813E   4C                   ENDIF
078700020813     C*
078800991123     C                   CLEAR                   DSLV04
078900960716     C                   MOVEL     'L'           D04TLA                         *ELABORA E CHIUDE
079000991124     C                   Z-ADD     TASIMV        D04IMV                         *IMPONIBILE
079100030619     C                   EXSR      RIEGEI                                       *REPERISCE IMPORTI V
079200991123     C                   MOVEL     §GEAED        D04AED                         *ARROTONDAMENTO
079300991123     C                   Z-ADD     §GEAVL        D04VAR                         *TIPO ARROTONDAMENTO
079400960528     C                   MOVEL     '1'           D04SPE                         *TASSAZIONE 1^BOLLA
079500960926     C                   Z-ADD     D92CAS        D04CAS                         *IMPORTO C/ASSEGNO
079600960926     C                   MOVEL     D92VCA        D04VCA                         *VALUTA  C/ASSEGNO
079700960926     C                   MOVEL     D92TIC        D04TIC                         *INCASSO C/ASSENGO
079800980224     C                   MOVEL     D92DSP        D04DFT                         *DATA FATTURA
079900020108     C                   MOVEL     §GEDCN        D04DIV                         *DIVISA
080000991123     C                   CALL      'FNLV04R'
080100991123     C                   PARM                    DSLV04
080200991123     C                   PARM                    DTASV
080300971009     C                   Z-ADD     D04IMV        V1CPOR                         *PORTO
080400971009     C                   Z-ADD     D04ALI        V1CALI                         *ALIQUOTA
080500971009     C                   Z-ADD     D04IVA        V1CIVA                         *IVA
080600971009     C                   Z-ADD     D04IFT        V1CIFT                         *IMPONIBILE+IVA
080700991124     C                   MOVEL     TASDIV        V1CDIV                         *DIVISA
080800050428     c
080900050415     c* Aggiungo l'arrotondamento al porto o alla prima varia buona
081000050415    3C                   SELECT
081100050415     C     tasPOR        WHENGT    0
081200050415     C                   add       D04POR        taspoR
081300050415     C     var(1)        WHENGT    0
081400050415     C     vsv(1)        ANDNE     §$2BOL
081500050415     C     vsv(1)        ANDNE     §$2IVA
081600050415     C                   ADD       D04POR        var(1)
081700050415     C     var(2)        WHENGT    0
081800050415     C     vsv(2)        ANDNE     §$2BOL
081900050415     C     vsv(2)        ANDNE     §$2IVA
082000050415     C                   ADD       D04POR        var(2)
082100050415     C     var(3)        WHENGT    0
082200050415     C                   ADD       D04POR        var(3)
082300050415    3C                   ENDSL
082400960729X   3C                   ELSE                                                   * S I  ERRORI
082500960529     C                   MOVEL     '1'           $ERR
082600960530     C                   MOVEL     ERR(1)        $MSG
082700960529     C                   EXSR      CARMSG
082800960729E   3C                   ENDIF
082900960729E   2C                   ENDIF
083000960729X   1C                   ELSE
083100971009     C                   Z-ADD     *ZEROS        V1CPOR                         *TRASPORTO
083200971009     C                   Z-ADD     *ZEROS        V1CALI                         *ALIQUOTA
083300971009     C                   Z-ADD     *ZEROS        V1CIVA                         *IVA
083400971009     C                   Z-ADD     *ZEROS        V1CIFT                         *IMPONIBILE VUOTO
083500991124     C                   MOVEL     *BLANKS       V1CDIV                         *DIVISA
083600960604E   1C                   ENDIF
083700000000     C*
083800000000     C                   ENDSR
083900020813     C*------------------------------------------------------------------------*
084000020813     C* APPLICA LA % DI SCONTO, SE INSERITA, SU TUTTI GLI IMPORTI
084100020813     C*------------------------------------------------------------------------*
084200020813     C     APPSCO        BEGSR
084300020813     C*
084400020813     C* Calcola la % da applicare per ottener l'importo scontato
084500020813     C                   EVAL      WPer = 100 - D92SCO
084600020813     C*---
084700020813     C* Totale imponibile
084800020813     C*---
084900050428     C                   EVAL(H)   tasimv = (tasimv * WPer) / 100
085000020813     C*---
085100020813     C* Varie
085200020813     C*---
085300020813do  1c     1             DO        20            v                 5 0
085400020813if  2c                   IF        var(v) > *zeros
085500050428     C                   EVAL(H)   var(v) = (var(v) * WPer) / 100
085600020813e   2c                   ENDIF
085700020813e   1c                   ENDDO
085800050428     C*---
085900050428     C* Trasporto
086000050428     C*---
086100050428     C                   EVAL(H)   taspor = (taspor * WPer) / 100
086200050428     C                   CLEAR                   DSLV16
086300050428     C                   MOVEL     'T'           D17FIM
086400050428     C                   Z-ADD     TASPOR        D17POR
086500050428     C                   CALL      'FNLV16R'
086600050428     C                   PARM                    DSLV16
086700050428     C                   PARM                    DTASV
086800050428     c                   eval      tasimv=o17imv
086900020813     c*
087000020813     C                   ENDSR
087100020225     C*------------------------------------------------------------------------*
087200020225     C* DECNTW - DECODIFICA IL NETWORK
087300020225     C*------------------------------------------------------------------------*
087400020225     C     DECNTW        BEGSR
087500020225     C*
087600020225     C* VERIFICA VALORE IN TABELLA
087700020225     C                   CLEAR                   DSBS02
087800020225     C                   MOVEL     'NTW'         T02COD
087900020225     C                   MOVEL     KNSIF         T02SIF
088000020225     C                   MOVEL     V1CNTW        T02KE1
088100020225     C                   MOVEL     'C'           T02MOD
088200020225     C                   CALL      'TIBS02R'
088300020225     C                   PARM                    KPJBA
088400020225     C                   PARM                    DSBS02
088500020225     C*
088600020225     C     T02ERR        IFEQ      *BLANKS
088700020225     C                   MOVEL     T02UNI        DNTW
088800020225     C                   ENDIF
088900020225     C*
089000020225     C                   ENDSR
089100991123     C*------------------------------------------------------------------------*
089200991123     C* RIEGEI - REPERIENTO IMPORTI VARI
089300991123     C*------------------------------------------------------------------------*
089400991123     C     RIEGEI        BEGSR
089500991123     C*
089600991123     C                   CLEAR                   DSBS02
089700991123     C                   CLEAR                   DGEI
089800991123     C                   MOVEL     'C'           T02MOD
089900991123     C                   MOVEL     KNSIF         T02SIF
090000991123     C                   MOVEL     'GEI'         T02COD
090100991124     C                   MOVEL     TASDIV        T02KE1
090200991123     C                   CALL      'TIBS02R'
090300991123     C                   PARM                    KPJBA
090400991123     C                   PARM                    DSBS02
090500991123IF  1C     T02ERR        IFEQ      *BLANKS
090600991123     C                   MOVEL     T02UNI        DGEI
090700991123E   1C                   ENDIF
090800991123     C*
090900991123     C                   ENDSR
091000951030     C*------------------------------------------------------------------------*
091100951030     C* CARTAB - CARICA TABELLE
091200951030     C*------------------------------------------------------------------------*
091300951027     C     CARTAB        BEGSR
091400960604     C*--------------------
091500960604     C* NAZIONI
091600960604     C*--------------------
091700951027     C                   Z-ADD     0             I
091800020919     C                   Z-ADD     1             KTBKUT
091900951027     C                   MOVEL     '15'          KTBCOD
092000951027     C                   MOVEL     *BLANKS       KTBKEY
092100951027     C     KEYTAB        SETLL     TABEL00F                           99
092200951027     C  N99              READ      TABEL00F                               99
092300951030IF  1C     *IN99         IFEQ      *ON
092400951027     C                   MOVEL     MSG(6)        $MSG
092500951027     C                   EXSR      CARMSG
092600951030E   1C                   ENDIF
092700951030DO  1C     *IN99         DOWEQ     *OFF
092800951027     C     TBLCOD        ANDEQ     '15'
092900951030IF  2C     TBLFLG        IFEQ      *BLANKS
093000951027     C                   ADD       1             I
093100951027     C                   MOVEL     TBLKEY        NAR(I)
093200951027     C                   MOVEL     TBLUNI        DS15
093300020514     C                   Z-ADD     §15LAD        NARLAD(I)
093400951030E   2C                   ENDIF
093500951027     C                   READ      TABEL00F                               99
093600951030E   1C                   ENDDO
093700010823     C*--------------------
093800010823     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
093900010823     C*--------------------
094000010823     C                   MOVEL     'GED'         TBECOD
094100010823     C                   MOVEL     *BLANKS       TBEKE1
094200010823     C                   MOVEL     *BLANKS       TBEKE2
094300010823     C                   MOVEL     '1'           TBEKE1
094400010823     C     KTBE          CHAIN     TNTBE01L                           21
094500010823     C  N21              MOVEL     TBEUNI        DGED
094600050415     C*--------------------
094700050415     C* reperisco sigle varie
094800050415     C*--------------------
094900050415     C                   Z-ADD     1             KTBKUT
095000050415     C                   MOVEL     '$2'          KTBCOD
095100050415     C                   MOVEL(P)  '1'           KTBKEY
095200050415     C     KEYTAB        chain     TABEL00F                           99
095300050415     C  N99              MOVEL     TBLUNI        DS$2
095400050415     C   99              CLEAR                   DS$2
095500131016     C*--------------------------------------
095600131016     C* Controllo se filiale utente è già attiva per gli orari servizio
095700131016     C*--------------------------------------
095800131016     c                   eval      TBEcod = 'VPO'
095900131016     c                   eval      TBEke1 = 'DECOFI816'
096000131016     c                   clear                   TBEke2
096100131016     c     KTBE          chain     TNTBE01L
096200131016     c                   IF        %found(TNTBE01L)
096300131016     c                   eval      FilOkOrari = TBEuni
096400131016     c                   ENDIF
096500131016      /free
096600131016         *in04 = *off;
096700131016         IF  skFilOkOrari(1) = '999' or
096800131016             %lookup(%editc(DUtpou:'X'):skFilOkOrari) > 0;
096900131016           *in04 = *on;
097000131016         ENDIF;
097100131016      /end-free
097200951027     C*
097300951027     C                   ENDSR
097400970704     C*------------------------------------------------------------------------*
097500970704     C* CHIUDI  - CHIUDE EVENTUALI PGM CON USCITA RETURN
097600970704     C*------------------------------------------------------------------------*
097700970704     C     CHIUDI        BEGSR
097800970704     C* TISI95R
097900970704     C                   CLEAR                   DSSI95
098000970704     C                   MOVEL     'C'           I95TLA                         'C'=CHIUDI
098100970704     C                   CALL      'TISI95R'                            99
098200970704     C                   PARM                    DSSI95
098300971020     C* TNSD99R
098400971009     C                   CLEAR                   DSSD99
098500970704     C                   MOVEL     'C'           D98TLA                         'C'=CHIUDI
098600971009     C                   CALL      'TNSD99R'                            99
098700971009     C                   PARM                    DSSD99
098800971020     C* FNLV55R
098900971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
099000971020     C                   MOVEL     'C'           D55TLA
099100971020     C                   CALL      'FNLV55R'
099200971020     C                   PARM                    DSLV55
099300970704     C*
099400970704     C                   ENDSR
099500020919     C*--------------------------------------------------------------------------------------------*
099600020919     C* REPERISCE I DATI UTENTE
099700020919     C*--------------------------------------------------------------------------------------------*
099800020919     C     REPDATIUTE    BEGSR
099900020919     C*
100000020919     C* INIZIALIZZA VARIABILI DI WRK
100100020919     C                   CLEAR                   TIBS34DS
100200020919     C                   CLEAR                   AZUTEDS
100300020919     C                   CLEAR                   DDATIUTE
100400020919     C*
100500020919     C     *DTAARA       DEFINE    §azute        azuteds
100600020919     C     *DTAARA       DEFINE    §datiute      ddatiute
100700020919     C                   IN(E)     *DTAARA
100800020919if  1C                   IF        %Error
100900020919     c                   EVAL      I34Tla = 'L'
101000020919     C                   CALL      'TIBS34R'
101100020919     C                   PARM                    Tibs34Ds
101200020919     C                   IN        *DTAARA
101300020919e   1C                   ENDIF
101400020919     C*
101500020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
101600020919     C                   MOVEL(P)  RSUT          DSFIRS           20
101700020919     C*
101800020919     C                   ENDSR
101900000000     C*------------------------------------------------------------------------*
102000000000     C*  *INZSR - ROUTINE INIZIALE
102100000000     C*------------------------------------------------------------------------*
102200000000     C     *INZSR        BEGSR
102300000000     C*--------------------
102400000000     C* RICEVIMENTO PARAMETRI
102500000000     C*--------------------
102600000000     C     *ENTRY        PLIST
102700000000     C                   PARM                    KPJBA
102800970704     C                   MOVEL     KPJBU         DSSD92
102900000000     C*--------------------
103000000000     C* CAMPI RIFERITI AL DATA BASE
103100000000     C*--------------------
103200000000     C     *LIKE         DEFINE    CLNTFP        KCLTFP                         *AZCLN00F
103300000000     C     *LIKE         DEFINE    CLNTFA        KCLTFA
103400000000     C     *LIKE         DEFINE    CLNANN        KCLANN
103500000000     C     *LIKE         DEFINE    CLNMES        KCLMES
103600000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
103700000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
103800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
103900000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
104000980703     C     *LIKE         DEFINE    TASMCT        DEPMCT                         *DEPOSITI
104100980703     C     *LIKE         DEFINE    TASFAP        DEPFAP
104200020813     C     *LIKE         DEFINE    D92SCO        WPer
104300020813     C*--------------------                                                     *di lavoro
104400000000     C* CHIAVI DI LETTURA
104500000000     C*--------------------
104600970704     C* DI AZCLN01L - COMPLETA
104700000000     C     KEYCLN        KLIST
104800000000     C                   KFLD                    KCLTFP                         *TERMINAL PARTENZA
104900000000     C                   KFLD                    KCLTFA                         *TERMINAL ARRIVO
105000000000     C                   KFLD                    KCLANN                         *ANNO
105100000000     C                   KFLD                    KCLMES                         *MESE
105200970704     C* DI AZORG01L - COMPLETA
105300000000     C     KEYORG        KLIST
105400000000     C                   KFLD                    KORFIL                         *FILIALE
105500970704     C* DI TABEL00F - COMPLETA
105600000000     C     KEYTAB        KLIST
105700000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
105800000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
105900000000     C                   KFLD                    KTBKEY                         *CHIAVE
106000010823     C* DI TNTBE00F - PARZIALE
106100010823     C     KTBE          KLIST
106200010823     C                   KFLD                    TBECOD
106300010823     C                   KFLD                    TBEKE1
106400010823     C                   KFLD                    TBEKE2
106500980703     C*
106600000000     C* CARICA LE DECODIFICHE DEI GIORNI DELLA SETTIMANA
106700000000     C                   MOVEL     DGIO(1)       GIO(1)                         *LUNEDI'
106800000000     C                   MOVEL     DGIO(2)       GIO(2)                         *MARTEDI'
106900000000     C                   MOVEL     DGIO(3)       GIO(3)                         *MERCOLEDI'
107000000000     C                   MOVEL     DGIO(4)       GIO(4)                         *GIOVEDI'
107100000000     C                   MOVEL     DGIO(5)       GIO(5)                         *VENERDI'
107200000000     C                   MOVEL     DGIO(6)       GIO(6)                         *MERCOLEDI'
107300000000     C                   MOVEL     DGIO(7)       GIO(7)                         *GIOVEDI'
107400971020     C*
107500971020     C* REPERISCE LA DATA CORRENTE
107600971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
107700971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
107800971020     C                   Z-ADD     N8            G08DAT
107900971020     C                   Z-ADD     *ZEROS        G08INV
108000971020     C                   MOVEL     '0'           G08ERR
108100971020     C                   CALL      'XSRDA8'
108200971020     C                   PARM                    WLBDA8
108300971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
108400980703     C*
108500980703     C* REPERISCE I DATI MITTENTE PER LA TASSAZIONE
108600980703IF  1C     D92TPO        IFEQ      'A'
108700980703     C                   CLEAR                   DSSI95
108800980703     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
108900980703     C                   MOVEL     '3'           I95TCN                         *CONTROLLO FORMALE
109000980703     C                   MOVEL     D92NZM        I95NAR                         *NAZIONE
109100980703     C                   MOVEL     D92CAM        I95CAP                         *CAP
109200980703     C                   MOVEL     D92LOM        I95LOC                         *LOCALITA'
109300980703     C                   Z-ADD     D92DSP        I95DAT                         *DATA RIFERIMENTO
109400980703     C                   MOVEL     D92TPO        I95TPO                         *TIPO PORTO
109500980703     C                   Z-ADD     D92PKB        I95LKG                         *PESO
109600980703     C                   Z-ADD     D92VLF        I95LMC                         *VOLUME
109700980703     C                   MOVEL     D92TSP        I95TSP                         *TIPO SERVIZIO
109800021028     C                   IF        D92NTW = 'DPD'
109900021028     C                   MOVEL     'D'           I95FI1
110000021028     C                   ENDIF
110100020225     C                   IF        D92NTW = 'FED'
110200020225     C                   MOVEL     'S'           I95FI1
110300020225     C                   ENDIF
110400980703     C                   CALL      'TISI95R'
110500980703     C                   PARM                    DSSI95
110600980703     C                   MOVEL     O95CTS        DEPMCT                         *CODICE TASSAZIONE
110700980703     C                   MOVEL     O95ISO        DEPFAP                         *FLAG INOLTRO
110800980703E   1C                   ENDIF
110900110620     c* Se si tratta di EEX con nazione Danimarca, Estonia, Lituania, Lettonia
111000110620     c*  non devo visulizzare i tempi di consegna
111100110620     c                   if        d92ntw='EEX'
111200110620     c                   eval      Indx =%lookup(d92nar:nareex)
111300110620     c                   eval      *in01=Indx>0
111400110620     c                   endif
111500110620     c
111600000000     C*--------------------
111700000000     C* IMPOSTAZIONI VIDEO
111800000000     C*--------------------
111900000000     C                   MOVEL     'I'           $TGES             1            *INTERROGAZIONE
112000000000     C                   EXSR      INZSTD                                       *OPERAZIONI STANDARD
112100951027     C                   EXSR      CARTAB                                       *CARICA LE TABELLE
112200000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
112300000000     C                   EXSR      CARV1                                        *CARICA PRIMO VIDEO
112400000000     C*
112500000000     C                   ENDSR
112600000000     C*------------------------------------------------------------------------*
112700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112900000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
113000000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
113100000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
113200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113400000000     C*------------------------------------------------------------------------*
113500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
113600000000     C*------------------------------------------------------------------------*
113700000000     C     INZSTD        BEGSR
113800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
113900000000     C                   CALL      'XKEYS'
114000000000     C                   PARM                    DSKEYS
114100000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
114200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
114300000000     C     XMSGPA        PLIST
114400000000     C                   PARM                    DSMSMS
114500000000     C                   PARM                    DSMSPG
114600000000     C                   PARM                    DSMSKE
114700000000     C                   PARM                    DSMSER
114800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
114900970707     C                   MOVEL     'TISI93R'     DSMSPG                         *NOME PGM CHIAMANTE
115000970707     C                   MOVEL     'TISI93R'     VMCQUE                         *CODA MESSAGGI
115100000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
115200000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
115300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
115400000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
115500000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
115600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
115700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
115800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
115900000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
116000000000     C* LANCIO ROUTINE PER VERIFICARE SE
116100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
116200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
116300020919     C                   EXSR      REPDATIUTE
116400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
116500000000     C                   BITOFF    '01234567'    POSMI1            1
116600000000     C                   BITOFF    '01234567'    POSMI2            1
116700000000     C                   BITOFF    '01234567'    POSMA1            1
116800000000     C                   BITOFF    '01234567'    POSMA2            1
116900000000     C                   BITON     '34'          POSMI1            1            *18
117000000000     C                   BITON     '7'           POSMI2            1            *01
117100000000     C                   BITON     '34'          POSMA1            1            *18
117200000000     C                   BITON     '0'           POSMA2            1            *80
117300000000     C                   MOVE      POSMI2        POSMIN            2
117400000000     C                   MOVEL     POSMI1        POSMIN
117500000000     C                   MOVE      POSMA2        POSMAX            2
117600000000     C                   MOVEL     POSMA1        POSMAX
117700000000     C                   ENDSR
117800000000     C*------------------------------------------------------------------------*
117900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
118000000000     C*------------------------------------------------------------------------*
118100000000     C     FUNSTD        BEGSR
118200000000     C                   SELECT
118300000000     C     KEY           WHENEQ    F3
118400000000     C                   EXSR      F03STD
118500000000     C     KEY           WHENEQ    F12
118600000000     C                   EXSR      F12STD
118700000000     C     KEY           WHENEQ    F24
118800000000     C                   EXSR      F24STD
118900050415     C     KEY           WHENEQ    F9
119000050415     C                   EXSR      F09STD
119100050415     C     KEY           WHENEQ    F14
119200050505     c                   setoff                                       40
119300131016     c     KEY           WHENEQ    F16
119400131016     c                   exsr      F16STD
119500050505     C     KEY           WHENEQ    F17
119600050505     C                   EXSR      F17STD
119700000000     C                   OTHER
119800000000     C                   MOVEL     MSG(2)        $MSG
119900000000     C                   EXSR      CARMSG
120000000000     C                   ENDSL
120100000000     C                   ENDSR
120200000000     C*------------------------------------------------------------------------*
120300000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
120400000000     C*------------------------------------------------------------------------*
120500000000     C     F03STD        BEGSR
120600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
120700000000     C                   MOVEL     '1'           $F03
120800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
120900000000     C                   MOVEL     '1'           $FINE
121000000000     C                   ENDSR
121100050415     C*------------------------------------------------------------------------*
121200050415     C* F09STD -GEST.TASTO FUNZ. F09 visalizza dettaglio tassazione
121300050415     C*------------------------------------------------------------------------*
121400050415     C     F09STD        BEGSR
121500050415     c* Pulizia sfl
121600050415     C                   SETON                                        70
121700050415     C                   WRITE     si93wc1
121800050415     C                   SETOFF                                       70
121900050415     c
122000050415     c                   z-add     1             ww                3 0
122100050415     c                   z-add     0             nrr               4 0
122200050428     c                   clear                   vsfsv1
122300050428     c                   setoff                                       02
122400050415     c
122500050415     C* IMPOSTO il porto nella prima posizione del sfl
122600050415     c                   if        taspor>0
122700050428     C                   eval      vsfsv1='Trasporto.:'
122800050608     c**                 z-add     taspor        vsfva1
122900050608     c                   clear                   vsfva1
123000050608     c                   eval      vsfva1=%editc(taspor:'4')
123100050415     c                   add       1             nrr
123200050415     c                   write     si93ws1
123300050415     c                   endif
123400050415     c
123500050415     C* IMPOSTO le varie
123600050415     c                   dow       vsv(ww)<>' '
123700050415     c* Tolgo l'iva, già visualizzata
123800050415     c                   if        vsv(ww)<>§$2iva
123900050428     C                   MOVEL     1             KTBKUT
124000050428     C                   MOVEL     'CC'          KTBCOD
124100050428     C                   MOVEL(P)  'VARIE'       KTBKEY
124200050428     C                   MOVE      vsv(ww)       KTBKEY
124300050428     C     KEYTAB        CHAIN     TABEL00F                           99
124400050428IF  2C     *IN99         IFEQ      *OFF
124500050428     C                   MOVEL     TBLUNI        w009a             9
124600050428     c                   else
124700050428     c                   clear                   w009a
124800050428    2c                   endif
124900050428     c                   eval      vsfsv1=(vsv(ww)+'-'+ w009a)
125000050608     c**                 z-add     var(ww)       vsfva1
125100050608     c                   clear                   vsfva1
125200050608     c                   if        var(WW)>0
125300050608     c                   eval      vsfva1=%editc(var(ww):'4')
125400050608     c                   else
125500050608     c                   move      '0    '       vsfva1
125600050608     c                   endif
125700050608     c
125800050415     c                   add       1             nrr
125900050415     c                   write     si93ws1
126000050428    1c                   endif
126100050415     c
126200050415     c                   add       1             ww
126300050415     c                   enddo
126400050428     c
126500050428     c* Se le varie sono meno di 6 imposto imponibile iVA e totale
126600050428     c* alle posizioni  9 10 11 altrimenti in coda
126700050428     c                   dow       nrr<8
126800050428     c                   clear                   vsfsv1
126900050428     c                   clear                   vsfva1
127000050428     c                   add       1             nrr
127100050428     c                   seton                                        03
127200050428     c                   write     si93ws1
127300050428     c                   enddo
127400050428     c                   setoff                                       03
127500050428     c* Imponibile
127600050428     c                   eval      vsfsv1='Imponibile:'
127700050608     c**                 eval      vsfva1=v1cpor
127800050608     c                   clear                   vsfva1
127900050608     c                   if        v1cpor>0
128000050608     c                   eval      wvalore=v1cpor
128100050608     c                   eval      vsfva1=%editc(wvalore:'4')
128200050608     c                   else
128300050608     c                   move      '0    '       vsfva1
128400050608     c                   endif
128500050428     c                   add       1             nrr
128600050428     c                   write     si93ws1
128700050428     c* IVA
128800050428     c                   eval      vsfsv1='IVA '+ %editc(v1cali:'2')+'..:'
128900050608     c**                 eval      vsfva1=v1civa
129000050608     c                   clear                   vsfva1
129100050608     c                   if        v1civa>0
129200050608     c                   eval      wvalore=v1civa
129300050608     c                   eval      vsfva1=%editc(wvalore:'4')
129400050608     c                   else
129500050608     c                   move      '0    '       vsfva1
129600050608     c                   endif
129700050428     c                   seton                                        02
129800050428     c                   add       1             nrr
129900050428     c                   write     si93ws1
130000050428     c                   setoff                                       02
130100050428     c* totale imponibile
130200050428     c                   eval      vsfsv1='Totale....:'
130300050608     c**                 eval      vsfva1=v1cift
130400050608     c                   clear                   vsfva1
130500050608     c                   if        v1cift>0
130600050608     c                   eval      wvalore=v1cift
130700050608     c                   eval      vsfva1=%editc(wvalore:'4')
130800050608     c                   else
130900050608     c                   move      '0    '       vsfva1
131000050608     c                   endif
131100050428     c                   add       1             nrr
131200050428     c                   write     si93ws1
131300050428     c
131400050415     c*
131500050415     c     for09         tag
131600050415     c                   exfmt     si93wc1
131700050415     c*
131800050415     c                   select
131900050415     c* F3 - fine
132000050415     c                   when      *inkc
132100050415     C                   MOVEL     '1'           $FINE
132200050415     C                   MOVEL     '1'           $F03
132300050415     c* F3 - ritorno
132400050415     c                   when      *inkl
132500050415     C                   MOVEL     '1'           $FINE
132600050415     c* F14- delivery
132700050415     c                   when      *inkn
132800050505     c                   setoff                                       40
132900131017      // F16-OrariSr
133000131017     c                   WHEN      *inkq
133100131017     c                   exsr      F16STD
133200050505     c* F17- ORE delivery
133300050505     c                   when      *inkr
133400050505     c                   seton                                        40
133500050415     c                   other
133600050415     c
133700050415     c                   goto      for09
133800050415     c                   endsl
133900050415     c
134000050415     C                   ENDSR
134100000000     C*------------------------------------------------------------------------*
134200000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
134300000000     C*------------------------------------------------------------------------*
134400000000     C     F12STD        BEGSR
134500000000     C* IMPOSTO FLAG DI FIN PGM
134600000000     C                   MOVEL     '1'           $FINE
134700000000     C                   ENDSR
134800131016
134900131016      /free
135000131016       //--------------------------------------------------------------------
135100131016       //?F16 - Interrogazione orari di servizio.
135200131016       //--------------------------------------------------------------------
135300131016       BEGSR F16STD;
135400131016
135500140203         clear trulorsds;
135600131016         IOREfil = O95lna;
135700131016         IOREcap = D92cad;
135800131016         IOREloc = D92lod;
135900131016         IOREnar = D92nar;
136000140203         //   IOREdta = D92dsp;
136100131016         IOREtser = 'E';
136200131104         IOREtsp = D92tsp;
136300140203         IOREtfp = v1ctfp;
136400140203         IOREtfa = v1ctfa;
136500140826         IOREdei = d98dei;
136600140826         IOREoei = d98oei;
136700140203         IOREdta = d98dce;
136800140203         IOREcol = 35    ;
136900150729         clear diore01 ;
137000150729              §IORETRAZC= %editc(d98ttc:'X') ;
137100150729              §IOREconsC= %editc(d98tcc:'X') ;
137200150729               ioreflo=diore01  ;
137300131016
137400140203         trulorsr (kpjba:trulorsds);
137500131017
137600131017         IF  OOREerr <> *blanks;
137700131017           $msg = 'Interrogazione Orari Servizio non possibile.';
137800131017           $msg = %trim($msg) + ' ' + %trim(OOREmsg);
137900131017           exsr carmsg;
138000131017           $err = '1';
138100131017         ENDIF;
138200131016
138300131016       ENDSR;
138400131016
138500131016      /end-free
138600131016
138700050505     C*------------------------------------------------------------------------*
138800050505     C* F17STD -GEST.TASTO FUNZ. F17 ORE delivery
138900050505     C*------------------------------------------------------------------------*
139000050505     C     F17STD        BEGSR
139100050505     C                   seton                                        40
139200050505     C                   ENDSR
139300000000     C*------------------------------------------------------------------------*
139400000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
139500000000     C*------------------------------------------------------------------------*
139600000000     C     F24STD        BEGSR
139700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
139800000000     C                   EXSR      OUTFUN
139900000000     C                   ENDSR
140000000000     C*------------------------------------------------------------------------*
140100000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
140200000000     C*          CODA MESSAGGI DEL PGM
140300000000     C*------------------------------------------------------------------------*
140400000000     C     CARMSG        BEGSR
140500000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
140600000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
140700000000     C     MSGULT        IFEQ      0
140800000000     C                   MOVEL     '1'           *IN40
140900970707     C                   WRITE     SI93DVK
141000000000     C                   MOVEL     '0'           *IN40
141100000000     C                   ENDIF
141200000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
141300000000     C                   MOVEL     $MSG          DSMSMS
141400000000     C                   CALL      'XMSG'        XMSGPA
141500000000     C     DSMSER        IFEQ      0
141600000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
141700000000     C                   ENDIF
141800000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
141900000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
142000970707     C                   WRITE     SI93DVM                                      *SFL MESS
142100000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
142200000000     C                   MOVEL     *BLANKS       $MSG
142300000000     C                   ENDSR
142400000000     C*------------------------------------------------------------------------*
142500000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
142600000000     C*------------------------------------------------------------------------*
142700000000     C     OUTMSG        BEGSR
142800000000     C* VISUALIZZAZIONE MESSAGGIO
142900000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
143000970707     C                   WRITE     SI93DVK                                      *CONTR MESS
143100000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
143200000000     C                   Z-ADD     *ZEROS        MSGULT
143300000000     C                   ENDSR
143400000000     C*------------------------------------------------------------------------*
143500000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
143600000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
143700000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
143800000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
143900000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
144000000000     C*------------------------------------------------------------------------*
144100000000     C     CARFUN        BEGSR
144200110620     C* SE 01 ON --> NON VISUALIZZO F17
144300110624     C**                 MOVEL     FUN(17)       FUNRIG                         *DS RIGA FUNZIONI
144400110624     C**                 IF        *IN01
144500110624     c**                 eval      funasg='N'
144600110624     c**                 eval      funasi='N'
144700110624     c**                 eval      funafg='N'
144800110624     c**                 eval      funafi='N'
144900110624     c**                 else
145000110624     c**                 eval      funasg='S'
145100110624     c**                 eval      funasi='S'
145200110624     c*+                 eval      funafg='S'
145300110624     c**                 eval      funafi='S'
145400110624     c**                 endif
145500110624     c**                 movel     funrig        fun(17)
145600131016
145700131016      /free
145800131016       //?Se 04 spento non devo visualizzare il tasto F16=OrariSr
145900131016         funrig = fun(16);
146000131016         IF  not *in04;
146100131017           funasg = 'S';
146200131017           funasi = 'S';
146300131016           funafg = 'N';
146400131016           funafi = 'N';
146500131016         ELSE;
146600131016           funasg = 'S';
146700131017           funasi = 'S';
146800131016           funafg = 'S';
146900131016           funafi = 'S';
147000131016         ENDIF;
147100131016         fun(16) = funrig;
147200131016      /end-free
147300110620     c
147400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
147500000000     C     1             DO        31            I                 5 0          --- 1 -->
147600000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
147700000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
147800000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
147900020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
148000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
148100000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
148200000000     C                   ELSE                                                   *INTERROG.
148300000000     C                   MOVEL     FUNASI        FAT(I)
148400000000     C                   ENDIF
148500000000     C                   ELSE                                                   <-- 2 --> *FILIALE
148600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
148700000000     C                   MOVEL     FUNAFG        FAT(I)
148800000000     C                   ELSE                                                   *INTERROG.
148900000000     C                   MOVEL     FUNAFI        FAT(I)
149000000000     C                   ENDIF
149100000000     C                   ENDIF                                                  <-- 2 ---
149200000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
149300000000     C                   MOVEL     USER          WEDP              3            *PROFILO
149400000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
149500000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
149600000000     C                   ELSE
149700000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
149800000000     C                   ENDIF
149900000000     C                   ENDIF
150000000000     C                   ENDDO                                                  <-- 1 ---
150100000000     C                   ENDSR
150200000000     C*------------------------------------------------------------------------*
150300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
150400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
150500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
150600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
150700000000     C*------------------------------------------------------------------------*
150800000000     C     OUTFUN        BEGSR
150900000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
151000000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
151100000000     C                   ENDDO
151200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
151300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
151400000000     C     1             DO        31            I
151500000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
151600050415     C**   FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
151700000000     C                   ADD       1             FUNNAT
151800000000     C                   ENDIF
151900000000     C                   ENDDO
152000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
152100000000     C     FUNNAT        IFGT      8
152200000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
152300000000     C                   ENDIF
152400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
152500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
152600000000     C                   Z-ADD     8             FUNVIS            5 0
152700000000     C     FAT(23)       IFEQ      'S'
152800000000     C                   SUB       1             FUNVIS
152900000000     C                   ENDIF
153000000000     C     FAT(24)       IFEQ      'S'
153100000000     C                   SUB       1             FUNVIS
153200000000     C                   ENDIF
153300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
153400000000     C                   Z-ADD     *ZEROS        W                 5 0
153500000000     C                   ADD       1             FUNULT
153600000000     C     FUNULT        IFGT      31
153700000000     C                   Z-ADD     1             FUNULT
153800000000     C                   ENDIF
153900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
154000000000     C     FUNULT        DO        31            I                              --- 1 -->
154100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
154200000000     C     W             IFLT      FUNVIS                                       --- 2 -->
154300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
154400000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
154500050415     C**   FDE(I)        ANDNE     *BLANKS
154600000000     C     I             IFEQ      23                                           *TOLGO F23
154700000000     C     I             OREQ      24                                           *E F24
154800000000     C                   ELSE
154900000000     C                   ADD       1             W
155000000000     C                   MOVEL     FDE(I)        FUV(W)
155100000000     C                   ENDIF
155200000000     C                   ENDIF                                                  <-- 3 ---
155300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
155400000000     C                   Z-ADD     I             FUNULT
155500000000     C                   ENDIF                                                  <-- 2 ---
155600000000     C                   ENDDO                                                  <-- 1 ---
155700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
155800000000     C                   MOVEL     FUV(1)        VFD001
155900000000     C                   MOVEL     FUV(2)        VFD002
156000000000     C                   MOVEL     FUV(3)        VFD003
156100000000     C                   MOVEL     FUV(4)        VFD004
156200000000     C                   MOVEL     FUV(5)        VFD005
156300000000     C                   MOVEL     FUV(6)        VFD006
156400000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
156500000000     C     FAT(24)       ANDEQ     'N'
156600000000     C                   MOVEL     FUV(7)        VFD007
156700000000     C                   MOVEL     FUV(8)        VFD008
156800000000     C                   ELSE                                                   <-- 1 -->
156900000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
157000000000     C     FAT(24)       ANDEQ     'S'
157100000000     C                   MOVEL     FDE(23)       VFD007
157200000000     C                   MOVEL     FDE(24)       VFD008
157300000000     C                   ELSE                                                   <-- 2 -->
157400000000     C                   MOVEL     FUV(7)        VFD007
157500000000     C     FAT(23)       IFEQ      'S'
157600000000     C                   MOVEL     FDE(23)       VFD008
157700000000     C                   ELSE
157800000000     C                   MOVEL     FDE(24)       VFD008
157900000000     C                   ENDIF
158000000000     C                   ENDIF                                                  <-- 2 ---
158100000000     C                   ENDIF                                                  <-- 1 ---
158200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
158300000000     C                   ENDSR
158400000000     C*------------------------------------------------------------------------*
158500000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
158600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
158700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
158800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
158900000000     C*------------------------------------------------------------------------*
159000000000     C     CONFUN        BEGSR
159100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
159200000000     C     1             DO        31            I                              --- 1 -->
159300000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
159400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
159500000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
159600000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
159700000000     C                   EXSR      CARMSG
159800000000     C                   MOVEL     '1'           $ERR
159900000000     C                   ENDIF                                                  <-- 3 ---
160000000000     C                   ENDIF                                                  <-- 2 ---
160100000000     C                   ENDDO                                                  <-- 1 ---
160200000000     C                   ENDSR
160300960530** MSG - MESSAGGI VIDEO
160400970707TISI93R- Tasto funzionale non valido.                                          1
160500970707TISI93R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
160600970707TISI93R- Opzione non valida.                                                   3
160700970707TISI93R- Opzione abilitata ma non gestita. Avvertire CED.                      4
160800970707TISI93R- Inizio elenco.                                                        5
160900970707TISI93R- Fine elenco.                                                          6
161000970707TISI93R- 7
161100970707TISI93R- 8
161200970707TISI93R- 9
161300970707TISI93R- 10
161400970707TISI93R- 11
161500970707TISI93R- 12
161600970707TISI93R- 13
161700970707TISI93R- 14
161800970707TISI93R- 15
161900970707TISI93R- 16
162000970707TISI93R- 17
162100970707TISI93R- 18
162200970707TISI93R- 19
162300970707TISI93R- 20
162400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
162500000000F1    -                   -    NN     NN
162600000000F2    -                   -    NN     NN
162700000000F3    -F3=Fine            -    SS     SS
162800050428F4    -                   -    NN     NN
162900000000F5    -                   -    NN     NN
163000000000F6    -                   -    NN     NN
163100000000F7    -                   -    NN     NN
163200000000F8    -                   -    NN     NN
163300050415F9    -F9=Dettaglio Tassaz-    SS     SS
163400050415F10   -                   -    SS     SS
163500000000F11   -                   -    NN     NN
163600000000F12   -F12=Ritorno        -    SS     SS
163700000000F13   -                   -    NN     NN
163800050415F14   -F14=Delivery       -    SS     SS
163900000000F15   -                   -    NN     NN
164000131016F16   -F16=OrariSer       -    SS     SS
164100110624F17   -F17=ORE Delivery   -    NN     NN
164200000000F18   -                   -    NN     NN
164300000000F19   -                   -    NN     NN
164400000000F20   -                   -    NN     NN
164500000000F21   -                   -    NN     NN
164600000000F22   -                   -    NN     NN
164700000000F23   -                   -    NN     NN
164800000000F24   -F24=Altri tasti    -    NN     NN
164900000000ENTER -                   -    SS     SS
165000000000ROLLUP-                   -    NN     NN
165100000000ROLLDN-                   -    NN     NN
165200000000HELP  -                   -    NN     NN
165300000000PRINT -                   -    NN     NN
165400000000BSPACE-                   -    NN     NN
165500000000CLEAR -                   -    NN     NN
165600000000** DEC - DECODIFICHE
165700000000ISOLA                    1
165800000000PROVINCIA                2
165900000000CITTA'                   3
166000970704DISAGIATA                4
166100960529FORZ.ISOLA               5
166200971126FRANCO                   6
166300971126ASSEGNATO                7
166400021104FORZ.DISAG               8
166500021104FORZ.PROV.               9
166600021104FORZ.CSTOR               10
166700050505In Giornata  (24:00)     11
166800050505Entro le ore  12:00      12
166900091222Entro le ore  10:30      13
167000110915** DGIO - DECODIFICHE GIORNI DELLA SETTIMANA
167100000000LUNEDI'                  1
167200000000MARTEDI'                 2
167300000000MERCOLEDI'               3
167400000000GIOVEDI'                 4
167500000000VENERDI'                 5
167600000000SABATO                   6
167700000000DOMENICA                 7
167800960530** ERR - ERRORI DI ELABORAZIONE
167900960530TNSF20R- Errori in tassazione                                               1
168000110509La località non è servita dal tipo servizio
1681009605303
1682009605304
1683009605305
168400110620** nareex -  Nazioni con tempi di consegna particolari per EEX
168500110620DK LT LV EE
