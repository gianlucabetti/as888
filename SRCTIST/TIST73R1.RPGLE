000100970526     H*------------------------------------------------------------------------*
000200030929     H* STATISTICA MANUTENZIONE BOLLE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800071105     Fazorg01l  if   e           K DISK
000900071105     Ftabel00f  if   e           K DISK
001000071105     Ftist73d1  cf   e             WORKSTN INFDS(DEVDS1)
001100030929     F                                     SFILE(st73DMS:MSGULT)
001101151203     FCNACO00F  IF   E           K DISK
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500970526     D* MESSAGGI
001600970526     D*------------
001700000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* DESCRIZIONI TASTI FUNZIONALI
002000970526     D*------------
002100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* FUNZIONI DA CARICARE A VIDEO
002400970526     D*------------
002500970526     D FUV             S             19    DIM(24)
002600970526     D*------------------------------------------------------------------------*
002700970526     D* INPUT
002800970526     D*------------------------------------------------------------------------*
002900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003000970526     D*------------
003100970526     D FUDDS           DS
003200970526     D  FUDDE1                 1      1
003300970526     D  FUDFUN                 2      3
003400970526     D  FUDDE2                 4     19
003500970526     D  FUDFI0                20     34
003600970526     D  FUDATT                35     35
003700970526     D  FUDFI1                36     80
003800970526     D  FUDDES                 1     19
003900970526     D  FUDRIG                 1     80
004000970526     D*------------
004100970526     D* DS GESTIONE TASTI FUNZIONALI
004200970526     D*------------
004300970526     D DSKEY         E DS
004400970526     D DEVDS1          DS
004500970526     D  KEY                  369    369
004600970526     D*------------
004700970526     D* DS "XMSG" - GESTIONE MESSAGGI
004800970526     D*------------
004900970526     D DSMSG         E DS
005000970528     D*------------------
005100970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200970528     D*------------------
005300970528     D WLBDA8          DS                  INZ
005400970528     D  G08DAT                 1      8  0
005500970528     D  G08INV                 9     16  0
005600970528     D  G08ERR                17     17
005700970528     D  G08TGI                18     22  0
005800970526     D*------------
005900970526     D* ARCHITETTURA
006000970526     D*------------
006100970526     D KPJBA         E DS
006200071105     D*------------
006300071105     D* DS TABELLA $3 - DATE AVANZAMENTO TRASFERIMENTI BOLLE 00/10/P
006400071105     D*------------
006500071105     D DS$3          E DS
006600991214     D*------------------
006700991214     D* DS X PASSAGGIO PARAMETRI
006800991214     D*------------------
006900991214     D PARAM           DS                  INZ
007000071105     D  PARTIP                        2
007100070323     D  PARTID                        1
007200070323     D  PARDAD                        8  0
007300070323     D  PARDAA                        8  0
007400071105     D  PARLNA                        3  0
007500071105     D  PARLAV                        1
007600071105     D  PARFGC                        1
007700071105     D  PARNNF                        1
007800071105     D  PARFFDA                       1
007900071105     D  PARGGCA                       1
008000071105     D  PARDCR                        1
008100071105     D  PARSUP                        1
008200071105     D  PARDADINV                     8  0
008300071105     D  PARDAAINV                     8  0
008400071105     D  PARLNADES                    25
008500071106     D  PARRSUT                      20
008501151203     d  parccm                        7  0
008502151203     d  partik                        1
008600020923     D*-------------------
008700020923     D* DS REPERIMENTO DATI UTENTE
008800020923     D*-------------------
008900020923     D TIBS34DS      E DS                                                       *Profili utente
009000020923     D DDATIUTE      E DS                                                       *Dati utente
009100020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009200071106     D*-------------------
009300071106     D* CARICAMENTO SCHIERE FILIALI IN GESTIONE
009400071106     D*-------------------
009500071106     D TRUL06DS      E DS                  INZ
009600071106     D TRUL31DS      E DS                  INZ
009700071106     D*------------
009800071106     D* Filiali gestibili
009900071106     D*------------
010000071106     D FIL             S              3    DIM(250) INZ
010100071105     D*-------------------
010200071105     D* VARIABILI D WRK
010300071105     D*-------------------
010400071105     D wDataDa         S               D
010500071105     D wDataA          S               D
010501151203     d ragksc          s                   like(acorag)
010600970526     C*------------------------------------------------------------------------*
010700000303     C* MAIN LINES
010800970526     C*------------------------------------------------------------------------*
010900970526     C*
011000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011100991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
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
012200970526     C* FINE PGM
012300970526     C                   SETON                                        LR
012400970526     C*------------------------------------------------------------------------*
012500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012600970526     C*------------------------------------------------------------------------*
012700970526     C     BUTFMR        BEGSR
012800970526     C*
012900970526     C* EMETTE LA TESTATA
013000030929     C                   WRITE     st73D01
013100970526     C*
013200970526     C* SPEDISCE EVENTUALE MESSAGGIO
013300970526     C                   EXSR      SNDMSG
013400970526     C*
013500970526     C* EMETTE LE FUNZIONI
013600030929     C                   WRITE     st73D02
013700970526     C*
013800970526     C* EMETTE IL VIDEO
013900030929     C                   EXFMT     st73D03
014000970526     C*
014100991214     C* REIMPOSTA FLAG DI ERRORE
014200991214     C                   MOVEL     '0'           ERRL00
014300970526     C*
014400970526     C* REIMPOSTA INDICATORI DI ERRORE
014500071106     C                   SETOFF                                       545556
014600970526     C*
014700970526     C                   ENDSR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015000970526     C*------------------------------------------------------------------------*
015100970526     C     SNDMSG        BEGSR
015200970526     C*
015300970526     C* PULISCE IL SFILE MESSAGGI
015400970526     C                   MOVEL     '1'           *IN40
015500030929     C                   WRITE     st73DMC
015600970526     C                   MOVEL     '0'           *IN40
015700970526     C*
015800970526     C* VISUALIZZA IL MESSAGGIO
015900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016000970526     C                   CALL      'XMSG'        XMSGPA
016100020611     C     DSMSER        IFEQ      *ZEROS
016200970526     C                   MOVEL     DSMSKE        MSGKEY
016300020611     C                   ENDIF
016400970526     C                   Z-ADD     1             MSGULT
016500030929     C                   WRITE     st73DMS
016600030929     C                   WRITE     st73DMC
016700970526     C*
016800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016900970526     C                   MOVEL     *BLANKS       DSMSMS
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* ENTGES - GESTIONE ENTER -> INVIO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     ENTGES        BEGSR
017600970526     C*
017700970526     C* CONTROLLA I CAMPI IMMESSI
017800970526     C                   EXSR      DECODI
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F03GES - GESTIONE F03 -> FINE
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F03GES        BEGSR
018500970526     C*
018600970526     C* IMPOSTA "PREMUTO F03"
018700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018800970526     C*
018900970526     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100991214     C* F06GES - GESTIONE F06 -> CONFERMA
019200970526     C*------------------------------------------------------------------------*
019300970526     C     F06GES        BEGSR
019400970526     C*
019500991214     C* CONTROLLA I CAMPI IMMESSI
019600991214     C                   EXSR      DECODI
019700020611     C*
019800991214     C* ESEGUE ROUTINE DI CONFERMA
019900020611     C     ERRL00        IFEQ      '0'
020000991214     C                   EXSR      ESEGUI
020100031001     C* IMPOSTA FINE PROGRAMMA
020200031001     C                   MOVEL     '1'           FINL00
020300991214     C                   ENDIF
020400970526     C*
020500991214     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F12GES - GESTIONE F12 -> RITORNO
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F12GES        BEGSR
021000970526     C*
021100970526     C* IMPOSTA "PREMUTO F12"
021200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021300970526     C*
021400970526     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F24GES        BEGSR
021900970526     C*
022000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022100970526     C*
022200970526     C                   ENDSR
022300991214     C*------------------------------------------------------------------------*
022400991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
022500991214     C*------------------------------------------------------------------------*
022600991214     C     PASPAR        BEGSR
022700991214     C*
022800070323     C                   MOVEL     VIDTID        PARTID
022900030929     C                   Z-ADD     DADVID        PARDAD
023000030929     C                   Z-ADD     DAAVID        PARDAA
023100071105     C                   MOVEL     VIDLNA        PARLNA
023200071105     C                   MOVEL     VIDLAV        PARLAV
023300071105     C                   MOVEL     VIDFGC        PARFGC
023400071105     C                   MOVEL     VIDNNF        PARNNF
023500071105     C                   MOVEL     VIDFFDA       PARFFDA
023600071105     C                   MOVEL     VIDGGCA       PARGGCA
023700071105     C                   MOVEL     VIDDCR        PARDCR
023800071105     C                   MOVEL     VIDSUP        PARSUP
023900071105     C                   Z-ADD     VIDDAD        PARDADINV
024000071105     C                   Z-ADD     VIDDAA        PARDAAINV
024100071105     C                   MOVEL     VIDLNAD       PARLNADES
024200071106     C                   MOVEL     RSUT          PARRSUT
024201151203     C                   Z-ADD     vidccm        PARccm
024202151203     C                   movel     vidtik        PARtik
024300991214     C*
024400991214     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
024700970526     C*------------------------------------------------------------------------*
024800991214     C     ESEGUI        BEGSR
024900991214     C*
025000991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
025100030929     C                   MOVEL     'ST74'        KCOAZ
025200991214     C                   EXSR      PASPAR
025300991214     C                   MOVEL     PARAM         KPJBU
025400151207     C                   CALL      'BCH10'
025500151207 MOM C***                CALL      'TIST74C'
025600071129 MOM C***                CALL      'TIST74R'
025700991214     C                   PARM                    KPJBA
025800991214     C*
025900970526     C                   ENDSR
026000970526     C*------------------------------------------------------------------------*
026100991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026200970526     C*------------------------------------------------------------------------*
026300970526     C     DECODI        BEGSR
026400000306     C*
026500071105     C                   SETOFF                                       545556
026501151203     C                   SETOFF                                       57
026600991015     C*--------------------
026700071105     C* DATA INIZIO
026800991015     C*--------------------
026900030929     C     *LIKE         DEFINE    VIDDAD        DADVID
027000020611     C     ERRL00        IFEQ      '0'
027100030929     C                   Z-ADD     VIDDAD        WDA8
027200020611     C                   Z-ADD     *ZEROS        DA8W
027300991015     C                   EXSR      CHKDA8
027400030929     C                   Z-ADD     WDA8          VIDDAD
027500030929     C                   Z-ADD     DA8W          DADVID
027600020611     C     ERRL00        IFEQ      '1'
027700991214     C                   SETON                                        54
027800020611     C                   MOVEL     MSG(14)       DSMSMS
027900020611     C                   ENDIF
028000020611     C                   ENDIF
028100970528     C*--------------------
028200020611     C* DATA FINE
028300970528     C*--------------------
028400030929     C     *LIKE         DEFINE    VIDDAA        DAAVID
028500020611     C     ERRL00        IFEQ      '0'
028600030929     C                   Z-ADD     VIDDAA        WDA8
028700020611     C                   Z-ADD     *ZEROS        DA8W
028800970528     C                   EXSR      CHKDA8
028900030929     C                   Z-ADD     WDA8          VIDDAA
029000030929     C                   Z-ADD     DA8W          DAAVID
029100020611     C     ERRL00        IFEQ      '1'
029200991214     C                   SETON                                        55
029300970528     C                   MOVEL     MSG(14)       DSMSMS
029400020611     C                   ENDIF
029500020611     C                   ENDIF
029600020611     C*
029700071105     C* DATA FINE > DATA INIZIO
029800020611     C     ERRL00        IFEQ      '0'
029900030929     C     DAAVID        IFLT      DADVID
030000020611     C                   SETON                                        5455
030100970528     C                   MOVEL     MSG(15)       DSMSMS
030200970528     C                   MOVEL     '1'           ERRL00
030300020611     C                   ENDIF
030400020611     C                   ENDIF
030500040408     C*
030600071105     C* DATA INIZIO/DATA FINE ANTECEDENTI AL 15/03/2004 (DATA INIZIO MONITORIZZAZIONE MODIFICHE
030700071105     C* BOLLE IN PARTENZA E IN ARRIVO)
030800040408     C     ERRL00        IFEQ      '0'
030900040408     C                   IF        DADVID < 20040315 OR
031000040408     C                             DAAVID < 20040315
031100040408     C                   SETON                                        5455
031200040408     C                   MOVEL     MSG(16)       DSMSMS
031300040408     C                   MOVEL     '1'           ERRL00
031400040408     C                   ENDIF
031500040408     C                   ENDIF
031600071105     C*
031700071129     C* DATA INIZIO/DATA FINE => RANGE MAX 1 MESE
031800071129     C     ERRL00        IFEQ      '0'
031900071129     C                   MOVEL     DAAVID        wDataA
032000071129     C                   MOVEL     DADVID        wDataDa
032100071129     C     wDataA        SUBDUR    wDataDa       wGGdif:*D         5 0
032200071129     C                   IF        wGGdif > 31
032300071129     C                   SETON                                        5455
032400071129     C                   MOVEL     MSG(17)       DSMSMS
032500071129     C                   MOVEL     '1'           ERRL00
032600071129     C                   ENDIF
032700071129     C                   ENDIF
032800071105     C*
032900071105     C* DATA INIZIO > DATA TRASFERIMENTO BOLLE IN TITASP
033000071105     C     ERRL00        IFEQ      '0'
033100071105     C                   Z-ADD     1             tblKUT
033200071105     C                   MOVEL     '$3'          tblCOD
033300071105     C                   MOVEL     '1'           tblKEY
033400071105     C     KEYTAB        CHAIN(N)  TABEL00F
033500071105     C                   IF        %found(TABEL00F)
033600071105     C                   MOVEL     TBLUNI        DS$3
033700071105     C                   IF        DADVID < §$3D10
033800071105     C                   SETON                                        54
033900071105     C                   MOVEL     MSG(18)       DSMSMS
034000071105     C                   MOVEL     '1'           ERRL00
034100071105     C                   ENDIF
034200071105     C                   ENDIF
034300071105     C                   ENDIF
034400071105     C*
034500071105     C* CONTROLLO/SCELTA LINEA DI ARRIVO
034600071105     C     ERRL00        IFEQ      '0'
034700071105     C                   IF        VIDLNA <> *blanks AND
034800071105     C                             VIDLNA <> *zeros
034900071105     C                   EVAL      wFIL = VIDLNA
035000071105     C                   EXSR      CHKFIL
035100071105     C                   IF        wERR = *blanks
035200071105     C                   MOVE(P)   wFIL          VIDLNA
035300071105     C                   MOVEL(P)  wFILDES       VIDLNAD
035400071105     C                   ELSE
035500071105     C                   SETON                                        56
035600071105     C                   MOVEL     wERRMSG       DSMSMS
035700071105     C                   MOVEL     '1'           ERRL00
035800071105     C                   ENDIF
035900071105     C                   ELSE
036000071105     C                   SETON                                        56
036100071105     C                   MOVEL     MSG(22)       DSMSMS
036200071105     C                   MOVEL     '1'           ERRL00
036300071105     C                   ENDIF
036400071105     C                   ENDIF
036500071106     C*
036600071106     C* CONTROLLO ABILITAZINE UTENTE/LINA ARRIVO
036700071106     C     ERRL00        IFEQ      '0'
036800071106     C* - se utente EDP* => tutte linee
036900071106     C                   IF        %subst(KNMUS:1:3) = 'EDP'
037000071106     C                   ELSE
037100071106     C*
037200071106     C* - se utente FIL 1° LIV => "lui" + tutte linee della sua area
037300071106     C                   IF        DUTLPO = '1'
037400071106     C     VIDLNA        LOOKUP    FIL                                    20
037500071106     C                   IF        %equal
037600071106     C                   ELSE
037700071106     C                   SETON                                        56
037800071106     C                   MOVEL     MSG(23)       DSMSMS
037900071106     C                   MOVEL     '1'           ERRL00
038000071106     C                   ENDIF
038100071106     C                   ELSE
038200071106     C*
038300071106     C* - se utente FIL 2° LIV => "lui" + tutte linee della sua £6
038400071106     C                   IF        DUTLPO = '2'
038500071106     C     VIDLNA        LOOKUP    FIL                                    20
038600071106     C                   IF        %equal
038700071106     C                   ELSE
038800071106     C                   SETON                                        56
038900071106     C                   MOVEL     MSG(23)       DSMSMS
039000071106     C                   MOVEL     '1'           ERRL00
039100071106     C                   ENDIF
039200071106     C                   ELSE
039300071106     C*
039400071106     C* - se utente SEDE => niente!!!
039500071106     C                   SETON                                        56
039600071106     C                   MOVEL     MSG(23)       DSMSMS
039700071106     C                   MOVEL     '1'           ERRL00
039800071106     C                   ENDIF
039900071106     C                   ENDIF
040000071106     C                   ENDIF
040100071106     C                   ENDIF
040200000306     C*
040300080327     C***  ERRL00        IFEQ      '0'
040400080327     C***                IF        VIDLNA <> '005'
040500080327     C***                SETON                                        56
040600080327     C***                MOVEL     MSG(23)       DSMSMS
040700080327     C***                MOVEL     '1'           ERRL00
040800080327     C***                ENDIF
040900080327     C***                ENDIF
040901151203     c* parzializzazione per codice cliente
040902151203     C     ERRL00        IFEQ      '0'
040903151203    1C     Vidccm        IFGT      0
040904151203     c                   clear                   vidccmd
040905151203     C     KCLI          CHAIN     CNACO00f
040906151203     c                   if        not %found(cnaco00f) or acoflg<>' '
040907151203     C                   SETON                                        57
040908151203     C                   MOVEL     MSG(24)       DSMSMS
040909151203     C                   MOVEL     '1'           ERRL00
040910151203     C                   ELSE
040911151203     C                   MOVEL     ACORAG        VIDCCMD
040912151203     c                   endif
040913151203     c                   else
040914151203     c* se non immesso codice cliente vedo se richiesta la ricerca alfabetica
040915151203    1C     Vidccmd       IFne      *blanks
040916151203     C                   z-add     dutkci        PARKCI
040917151203     C                   MOVEL     Vidccmd       RAGKSC
040918151203     C                   MOVEL     rsut          PARDUT           30
040919151203     C* PARSTA = 9 --> ESCLUDO ANNULLATI
040920151203     C                   Z-ADD     9             PARSTA
040921151203     C                   Z-ADD     1             PAXNUM
040922151203     C* SOLO CLIENTI DEL S.I.
040923151203     C                   CLEAR                   PARDIT
040924151203     C                   CALL      'XALFA3BR'
040925151203     C                   PARM                    PARDUT
040926151203     C                   PARM                    CODUT
040927151203     C                   PARM                    RAGKSC
040928151203     C                   PARM                    PARKCI            4 0
040929151203     C                   PARM                    PARSTA            1 0
040930151203     C                   PARM                    PARFLR           90
040931151203     C                   PARM                    PARDIT            3
040932151203     C                   PARM                    PAXNUM            2 0
040933151203     C                   PARM                    PAXKCM           80
040934151203     C                   PARM                    PAXKSM          140
040935151203     C                   PARM                    PAXKDM           60
040936151203     C     PARSTA        IFNE      -1
040937151203     C                   MOVEL     PAXKSM        Vidccm
040938151203     C                   MOVEL     RAGKSC        Vidccmd
040939151203     C                   ENDIF
040940151203     C*
040941151203     c                   endif
040942151203     c                   endif
040943151203     C                   ENDIF
041000071106     C*
041100970528     C                   ENDSR
041200970528     C*------------------------------------------------------------------------*
041300970528     C* CHKDA8 - CONTROLLA LA DATA (8)
041400970528     C*------------------------------------------------------------------------*
041500970528     C     CHKDA8        BEGSR
041600970528     C*
041700030929     C     *LIKE         DEFINE    VIDDAD        WDA8
041800030929     C     *LIKE         DEFINE    VIDDAA        DA8W
041900970528     C*
042000970528     C                   Z-ADD     WDA8          G08DAT
042100970528     C                   Z-ADD     *ZEROS        G08INV
042200970528     C                   MOVEL     '0'           G08ERR
042300970528     C                   CALL      'XSRDA8'
042400970528     C                   PARM                    WLBDA8
042500970528     C                   Z-ADD     G08DAT        WDA8
042600970528     C                   Z-ADD     G08INV        DA8W
042700020611     C     G08ERR        IFEQ      '1'
042800970528     C                   MOVEL     '1'           ERRL00
042900020611     C                   ENDIF
043000970528     C*
043100970528     C                   ENDSR
043200071105     C*------------------------------------------------------------------------*
043300071105     C* CHKFIL - SCELTA/CONTROLLO FILIALE
043400071105     C*------------------------------------------------------------------------*
043500071105     C     CHKFIL        BEGSR
043600071105     C*
043700071105     C     *like         define    VIDLNA        wFIL
043800071105     C                   movel     *blanks       wFILNUM           3 0
043900071105     C                   movel     *blanks       wFILDES          25
044000071105     C                   movel     *blanks       wERR              1
044100071105     C                   movel     *blanks       wERRMSG          30
044200071105     C*
044300071105     C* Verifica richiesta scelta filiale
044400071105     C                   if        wFIL <> *blanks OR
044500071105     C                             wFIL <> *zeros
044600071105     C                   if        %scan('?':wFIL) > *zeros
044700071105     C                   move      *BLANKS       §COD1
044800071105     C                   move      *BLANKS       wFIL
044900071105     C                   call      'TNSD19R'
045000071105     C                   parm                    §COD1             3
045100071105     C                   parm                    §TIP              1
045200071105     C                   parm                    §DES             25
045300071105     C                   movel     §COD1         wFIL
045400071105     C                   movel     §DES          wFILDES
045500071105     C                   else
045600071105     C*
045700071105     C* Verifico numericità
045800071105     C                   eval      PiStr=wFIL
045900071105     C                   exsr      CHKNUM
046000071105     C                   if        PiInt=*on
046100071105     C                   z-add     PiVal         wFILNUM
046200071105     C     wFILNUM       chain     azorg01l
046300071105     C                   if        %found(azorg01l)
046400071105     C                   if        (orgfva <> *blank) or
046500071105     C                             (orgfag <> 'F'  and
046600071105     C                              orgfag <> 'A')
046700071105     C                   movel     '1'           wERR
046800071105     C                   movel     MSG(19)       wERRMSG
046900071105     C                   else
047000071105     C                   movel     orgDES        wFILDES
047100071105     C                   endif
047200071105     C                   else
047300071105     C                   movel     '1'           wERR
047400071105     C                   movel     MSG(14)       wERRMSG
047500071105     C                   endif
047600071105     C                   else
047700071105     C                   movel     '1'           wERR
047800071105     C                   movel     MSG(14)       wERRMSG
047900071105     C                   endif
048000071105     C                   endif
048100071105     C                   endif
048200071105     C*
048300071105     C                   ENDSR
048400071105     C*----------------------------------------------------*
048500071105     C*  CONTROLLO NUMERICITA' CAMPI
048600071105     C*----------------------------------------------------*
048700071105     C     CHKNUM        BEGSR
048800071105     C*
048900071105     C                   call(e)   'ISNUMERIC'
049000071105     C                   parm                    PiStr            30
049100071105     C                   parm      ','           PiDecChr          1
049200071105     C                   parm      *ZEROS        PiVal            30 9
049300071105     C                   parm      '0'           PiInt             1
049400071105     C                   parm      '0'           PiNum             1
049500071105     C                   if        %error
049600071105     C                   eval      PiInt=*off
049700071105     C                   endif
049800071105     C*
049900071105     C                   ENDSR
050000970526     C*------------------------------------------------------------------------*
050100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
050200970526     C*------------------------------------------------------------------------*
050300970526     C     AZZVID        BEGSR
050400970526     C* CAMPI
050500070509     C                   MOVEL     'C'           VIDTID
050600030929     C                   Z-ADD     *ZEROS        VIDDAD
050700030929     C                   Z-ADD     *ZEROS        VIDDAA
050800071105     C                   MOVEL     DUTPOU        VIDLNA
050900071105     C                   MOVEL     'S'           VIDLAV
051000071105     C                   MOVEL     'S'           VIDFGC
051100071105     C                   MOVEL     'S'           VIDNNF
051200071105     C                   MOVEL     'S'           VIDFFDA
051300071105     C                   MOVEL     'S'           VIDGGCA
051400071105     C                   MOVEL     'S'           VIDDCR
051500071105     C                   MOVEL     'S'           VIDSUP
051501151203     c                   clear                   vidccm
051502151203     c                   clear                   vidccmd
051503151204     c* solo per EDP visualizzo possibilità di parzializzazione per cliente
051504151204     c                   seton                                        98
051505151204     c                   if        %subst(knmus:1:3)='EDP'
051506151204     c                   setoff                                       98
051507151204     c                   endif
051600970526     C*
051700970526     C                   ENDSR
051800970526     C*------------------------------------------------------------------------*
051900020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
052000970526     C*------------------------------------------------------------------------*
052100970526     C     IMPFUN        BEGSR
052200970526     C*
052300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
052400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
052500970526     C                   EXSR      FUNGES
052600970526     C*
052700970526     C                   ENDSR
052800071106     C*--------------------------------------------------------------------------------------------*
052900071106     C* REPERISCE I DATI UTENTE
053000071106     C*--------------------------------------------------------------------------------------------*
053100071106     C     REPFIL        BEGSR
053200071106     C*
053300071106     C                   clear                   FIL
053400071106     C*
053500071106     C* Se utente FIL 1° LIV => Reperimento filiali della sua area
053600071106     C                   if        DUTLPO = '1'
053700071106     C                   clear                   TRUL31DS
053800071106     C                   eval      I31abi = 'RA'
053900071106     C                   eval      I31cpo = DUTPOU
054000071106     C                   call      'TRUL31R'
054100071106     C                   parm                    KPJBA
054200071106     C                   parm                    TRUL31DS
054300071106     C                   if        O31pog > *zeros
054400071106     C                   movea     O31pog        FIL
054500071106     C                   else
054600071106     C                   move(P)   DUTPOU        FIL(1)
054700071106     C                   endif
054800071106     C                   endif
054900071106     C*
055000071106     C* Se utente FIL 2° LIV => Reperimento filiali gestione Arrivi (£6)
055100071106     C                   if        DUTLPO = '2'
055200071106     C                   clear                   TRUL06DS
055300071106     C                   eval      D06cod   = '£6'
055400071106     C                   movel(P)  DUTPOU        D06key
055500071106     C                   movel(p)  TRUL06DS      KPJBU
055600071106     C                   call      'TRUL06R'
055700071106     C                   parm                    KPJBA
055800071106     C                   movel     KPJBU         TRUL06DS
055900071106     C                   move(P)   DUTPOU        FIL(1)
056000071106     C                   movea     D06LIA        FIL(2)
056100071106     C                   endif
056200071106     C*
056300071106     C                   ENDSR
056400020923     C*--------------------------------------------------------------------------------------------*
056500020923     C* REPERISCE I DATI UTENTE
056600020923     C*--------------------------------------------------------------------------------------------*
056700020923     C     REPDATIUTE    BEGSR
056800020923     C*
056900020923     C* INIZIALIZZA VARIABILI DI WRK
057000020923     C                   CLEAR                   TIBS34DS
057100020923     C                   CLEAR                   AZUTEDS
057200020923     C                   CLEAR                   DDATIUTE
057201151203     c                   z-add     1             codut             1 0
057300020923     C*
057400020923     C     *DTAARA       DEFINE    §azute        azuteds
057500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
057600020923     C                   IN(E)     *DTAARA
057700020923if  1C                   IF        %Error
057800020923     c                   EVAL      I34Tla = 'L'
057900020923     C                   CALL      'TIBS34R'
058000020923     C                   PARM                    Tibs34Ds
058100020923     C                   IN        *DTAARA
058200020923e   1C                   ENDIF
058300020923     C*
058400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
058500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
058600020923     C*
058700020923     C                   ENDSR
058800970526     C*------------------------------------------------------------------------*
058900970526     C* *INZSR - OPERAZIONI INIZIALI
059000970526     C*------------------------------------------------------------------------*
059100970526     C     *INZSR        BEGSR
059200991217     C*
059300991217     C     *ENTRY        PLIST
059400991217     C                   PARM                    KPJBA
059500071105     C                   MOVEL(P)  KPJBU         PARAM
059600071105     C*
059700071105     C* CHIAVE LETTURA TABEL00F - COMPLETA
059800071105     C     KEYTAB        KLIST
059900071105     C                   KFLD                    tblKUT
060000071105     C                   KFLD                    tblCOD
060100071105     C                   KFLD                    tblKEY
060101151203     C* ACCESSO CNACO00F
060102151203     C     KCLI          KLIST
060103151203     C                   KFLD                    CODUT
060104151203     C                   KFLD                    dutkci
060105151203     C                   KFLD                    Vidccm
060200020923     C*
060300020923     C* REPERISCE I DATI DELL'UTENTE
060400020923     C                   EXSR      REPDATIUTE
060500071106     C*
060600071106     C* REPERISCE LE RELATIVE FILIALI IN GESTIONE (RAGIONANDO X "ARRIVO")
060700071106     C                   EXSR      REPFIL
060800970526     C*
060900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
061000970526     C     XMSGPA        PLIST
061100970526     C                   PARM                    DSMSMS
061200970526     C                   PARM                    DSMSPG
061300970526     C                   PARM                    DSMSKE
061400970526     C                   PARM                    DSMSER
061500970526     C*
061600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
061700970526     C                   CALL      'XKEY'
061800970526     C                   PARM                    DSKEY
061900970526     C*---
062000970526     C* VARIABILI NON RIFERITE AL DATA BASE
062100970526     C*---
062200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
062300991214     C                   MOVEL     *BLANKS       DEPOP1            3
062400970526     C*---
062500970526     C* VARIABILI DI CONTROLLO
062600970526     C*---
062700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
062800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
062900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
063000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
063100970526     C*
063200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
063300071105     C                   MOVEL     'TI0T73R1'    DSMSPG
063400071105     C                   MOVEL     'TIST73R1'    MSGQUE
063500970526     C                   MOVEL     *BLANKS       MSGKEY
063600970526     C                   MOVEL     *BLANKS       DSMSMS
063700970528     C*
063800970528     C* CALCOLA LA DATA CORRENTE
063900970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
064000970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
064100970528     C                   Z-ADD     WN8           G08DAT
064200970528     C                   Z-ADD     *ZEROS        G08INV
064300970528     C                   MOVEL     '0'           G08ERR
064400970528     C                   CALL      'XSRDA8'
064500970528     C                   PARM                    WLBDA8
064600970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
064700000310     C*
064800000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
064900000310     C                   EXSR      AZZVID
065000970526     C*
065100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
065200970526     C                   EXSR      IMPFUN
065300970526     C*
065400970526     C                   ENDSR
065500970526     C*------------------------------------------------------------------------*
065600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
065700970526     C*------------------------------------------------------------------------*
065800970526     C     FUNGES        BEGSR
065900970526     C*
066000970526     C* PULISCO CAMPI DI LAVORO
066100970526     C                   Z-ADD     0             W                 5 0
066200970526     C*
066300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
066400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
066500970526     C     1             DO        24            I
066600970526     C                   MOVEL     FUD(I)        FUDRIG
066700970526     C*
066800970526     C* PULIZIA FUNZIONI PRECEDENTI
066900970526     C                   MOVEL     *BLANKS       FUV(I)
067000970526     C*
067100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
067200970526     C* (SALTA F24)
067300970526     C     FUDATT        IFEQ      'S'
067400970526     C     FUDFUN        ANDNE     '24'
067500970526     C                   ADD       1             W
067600970526     C                   MOVEL     FUDDES        FUV(W)
067700970526     C                   END
067800970526     C                   END                                                    DO WHILE
067900970526     C*
068000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
068100970526     C* ALTRIMENTI DISATTIVO F24
068200970526     C     1             DO        24            I
068300970526     C                   MOVEL     FUD(I)        FUDRIG
068400970526     C*
068500970526     C     FUDFUN        IFEQ      '24'
068600970526     C     W             IFGT      6
068700970526     C                   MOVEL     'S'           FUDATT
068800970526     C                   ELSE
068900970526     C                   MOVEL     *BLANKS       FUDATT
069000970526     C                   END                                                    W GT 8
069100970526     C                   MOVEL     FUDRIG        FUD(I)
069200970526     C                   END
069300970526     C                   END                                                    DO WHILE
069400970526     C*
069500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
069600970526     C     W             DIV       6             I
069700970526     C                   MVR                     Z
069800970526     C*
069900970526     C     Z             IFNE      0
070000970526     C     I             ADD       1             Z
070100970526     C                   ELSE
070200970526     C                   Z-ADD     I             Z                 5 0
070300970526     C                   END
070400970526     C*
070500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
070600970526     C                   ADD       1             FUNGRU
070700970526     C     FUNGRU        IFGT      Z
070800970526     C     FUNGRU        ORLT      1
070900970526     C                   Z-ADD     1             FUNGRU
071000970526     C                   END
071100970526     C*
071200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
071300970526     C     FUNGRU        IFEQ      1
071400970526     C                   MOVEL     FUV(1)        FUN001
071500970526     C                   MOVEL     FUV(2)        FUN002
071600970526     C                   MOVEL     FUV(3)        FUN003
071700970526     C                   MOVEL     FUV(4)        FUN004
071800970526     C                   MOVEL     FUV(5)        FUN005
071900970526     C                   MOVEL     FUV(6)        FUN006
072000970526     C                   END
072100970526     C*
072200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
072300970526     C     FUNGRU        IFEQ      2
072400970526     C                   MOVEL     FUV(7)        FUN001
072500970526     C                   MOVEL     FUV(8)        FUN002
072600970526     C                   MOVEL     FUV(9)        FUN003
072700970526     C                   MOVEL     FUV(10)       FUN004
072800970526     C                   MOVEL     FUV(11)       FUN005
072900970526     C                   MOVEL     FUV(12)       FUN006
073000970526     C                   END
073100970526     C*
073200970526     C* TERZO GRUPPO DI 6 FUNZIONI
073300970526     C     FUNGRU        IFEQ      3
073400970526     C                   MOVEL     FUV(13)       FUN001
073500970526     C                   MOVEL     FUV(14)       FUN002
073600970526     C                   MOVEL     FUV(15)       FUN003
073700970526     C                   MOVEL     FUV(16)       FUN004
073800970526     C                   MOVEL     FUV(17)       FUN005
073900970526     C                   MOVEL     FUV(18)       FUN006
074000970526     C                   END
074100970526     C*
074200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
074300970526     C     FUNGRU        IFEQ      4
074400970526     C                   MOVEL     FUV(19)       FUN001
074500970526     C                   MOVEL     FUV(20)       FUN002
074600970526     C                   MOVEL     FUV(21)       FUN003
074700970526     C                   MOVEL     FUV(22)       FUN004
074800970526     C                   MOVEL     *BLANKS       FUN005
074900970526     C                   MOVEL     *BLANKS       FUN006
075000970526     C                   END
075100970526     C*
075200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
075300970526     C                   MOVEL     *BLANKS       FUN007
075400970526     C                   MOVEL     *BLANKS       FUN008
075500970526     C*
075600970526     C     1             DO        24            I
075700970526     C                   MOVEL     FUD(I)        FUDRIG
075800970526     C     FUDFUN        IFEQ      '24'
075900970526     C     FUDATT        ANDEQ     'S'
076000970526     C                   MOVEL     FUDDES        FUN008
076100970526     C                   END
076200970526     C                   END                                                    DO
076300970526     C*
076400970526     C                   ENDSR
076500970526     C*------------------------------------------------------------------------*
076600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
076700970526     C*------------------------------------------------------------------------*
076800970526     C     FUNCON        BEGSR
076900970526     C*
077000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
077100970526     C                   EXSR      FUNVAR
077200970526     C*
077300970526     C* PULISCO CAMPI DI LAVORO
077400970526     C                   Z-ADD     0             I                 5 0
077500970526     C                   Z-ADD     0             W                 5 0
077600970526     C*
077700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
077800970526     C     FUNKEY        IFNE      *BLANKS
077900970526     C     1             DO        24            I
078000970526     C                   MOVEL     FUD(I)        FUDRIG
078100970526     C*
078200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
078300970526     C     FUNKEY        IFEQ      FUDFUN
078400970526     C     FUDATT        ANDEQ     'S'
078500970526     C                   Z-ADD     I             W
078600970526     C                   END
078700970526     C                   END                                                    DO WHILE
078800970526     C*
078900970526     C* FUNZIONE NON ATTIVA (ERRORE)
079000970526     C     W             IFEQ      0
079100970526     C*
079200970526     C* EMISSIONE MESSAGGIO DI ERRORE
079300970526     C                   MOVEL     MSG(1)        DSMSMS
079400970526     C*
079500970526     C                   END                                                    W=0
079600970526     C                   END                                                    FUNKEY NE *BLANKS
079700970526     C*
079800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
079900970526     C     W             IFNE      0
080000970526     C     FUNKEY        OREQ      *BLANKS
080100970526     C     FUNKEY        IFEQ      *BLANKS
080200970526     C                   EXSR      ENTGES
080300970526     C                   END
080400970526     C     FUNKEY        IFEQ      '03'
080500970526     C                   EXSR      F03GES
080600970526     C                   END
080700970526     C     FUNKEY        IFEQ      '06'
080800970526     C                   EXSR      F06GES
080900970526     C                   END
081000970526     C     FUNKEY        IFEQ      '12'
081100970526     C                   EXSR      F12GES
081200970526     C                   END
081300970526     C     FUNKEY        IFEQ      '24'
081400970526     C                   EXSR      F24GES
081500970526     C                   END
081600970526     C                   END
081700970526     C*
081800970526     C                   ENDSR
081900970526     C*------------------------------------------------------------------------*
082000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
082100970526     C*------------------------------------------------------------------------*
082200970526     C     FUNVAR        BEGSR
082300970526     C*
082400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
082500970526     C*
082600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
082700970526     C     KEY           IFEQ      ENTER
082800970526     C                   MOVEL     *BLANKS       FUNKEY
082900970526     C                   END
083000970526     C     KEY           IFEQ      F1
083100970526     C                   MOVEL     '01'          FUNKEY
083200970526     C                   END
083300970526     C     KEY           IFEQ      F2
083400970526     C                   MOVEL     '02'          FUNKEY
083500970526     C                   END
083600970526     C     KEY           IFEQ      F3
083700970526     C                   MOVEL     '03'          FUNKEY
083800970526     C                   END
083900970526     C     KEY           IFEQ      F4
084000970526     C                   MOVEL     '04'          FUNKEY
084100970526     C                   END
084200970526     C     KEY           IFEQ      F5
084300970526     C                   MOVEL     '05'          FUNKEY
084400970526     C                   END
084500970526     C     KEY           IFEQ      F6
084600970526     C                   MOVEL     '06'          FUNKEY
084700970526     C                   END
084800970526     C     KEY           IFEQ      F7
084900970526     C                   MOVEL     '07'          FUNKEY
085000970526     C                   END
085100970526     C     KEY           IFEQ      F8
085200970526     C                   MOVEL     '08'          FUNKEY
085300970526     C                   END
085400970526     C     KEY           IFEQ      F9
085500970526     C                   MOVEL     '09'          FUNKEY
085600970526     C                   END
085700970526     C     KEY           IFEQ      F10
085800970526     C                   MOVEL     '10'          FUNKEY
085900970526     C                   END
086000970526     C     KEY           IFEQ      F11
086100970526     C                   MOVEL     '11'          FUNKEY
086200970526     C                   END
086300970526     C     KEY           IFEQ      F12
086400970526     C                   MOVEL     '12'          FUNKEY
086500970526     C                   END
086600970526     C     KEY           IFEQ      F13
086700970526     C                   MOVEL     '13'          FUNKEY
086800970526     C                   END
086900970526     C     KEY           IFEQ      F14
087000970526     C                   MOVEL     '14'          FUNKEY
087100970526     C                   END
087200970526     C     KEY           IFEQ      F15
087300970526     C                   MOVEL     '15'          FUNKEY
087400970526     C                   END
087500970526     C     KEY           IFEQ      F16
087600970526     C                   MOVEL     '16'          FUNKEY
087700970526     C                   END
087800970526     C     KEY           IFEQ      F17
087900970526     C                   MOVEL     '17'          FUNKEY
088000970526     C                   END
088100970526     C     KEY           IFEQ      F18
088200970526     C                   MOVEL     '18'          FUNKEY
088300970526     C                   END
088400970526     C     KEY           IFEQ      F19
088500970526     C                   MOVEL     '19'          FUNKEY
088600970526     C                   END
088700970526     C     KEY           IFEQ      F20
088800970526     C                   MOVEL     '20'          FUNKEY
088900970526     C                   END
089000970526     C     KEY           IFEQ      F21
089100970526     C                   MOVEL     '21'          FUNKEY
089200970526     C                   END
089300970526     C     KEY           IFEQ      F22
089400970526     C                   MOVEL     '22'          FUNKEY
089500970526     C                   END
089600970526     C     KEY           IFEQ      F23
089700970526     C                   MOVEL     '23'          FUNKEY
089800970526     C                   END
089900970526     C     KEY           IFEQ      F24
090000970526     C                   MOVEL     '24'          FUNKEY
090100970526     C                   END
090200970526     C     KEY           IFEQ      ROLLUP
090300970526     C                   MOVEL     'RU'          FUNKEY
090400970526     C                   END
090500970526     C     KEY           IFEQ      HELP
090600970526     C                   MOVEL     'HP'          FUNKEY
090700970526     C                   END
090800970526     C                   ENDSR
090900970526     C*------------------------------------------------------------------------*
091000970526     C* NOTE COSTRUZIONE SCHIERE
091100970526     C*------------------------------------------------------------------------*
091200970526     C*
091300970526     C* -MSG-MESSAGGI
091400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
091500970526     C* (NON TOCCARE MSG,1 E MSG,2)
091600970526     C*
091700970526     C* -CMD-COMANDI
091800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
091900970526     C*
092000970526     C* -OPD-OPZIONI
092100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
092200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
092300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
092400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
092500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
092600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
092700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
092800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
092900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
093000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
093100970526     C*
093200970526     C* -FUD-FUNZIONI
093300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
093400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
093500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
093600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
093700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
093800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
093900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
094000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
094100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
094200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
094300970526     O*------------------------------------------------------------------------*
094400970526** MSG - MESSAGGI
094500071105TIST73R1- Tasto funzionale non gestito                                         1
094600071105TIST73R1-                                                                      2
094700071105TIST73R1-                                                                      3
094800071105TIST73R1-                                                                      4
094900071105TIST73R1-                                                                      5
095000071105TIST73R1-                                                                      6
095100071105TIST73R1-                                                                      7
095200071105TIST73R1-                                                                      8
095300071105TIST73R1-                                                                      9
095400071105TIST73R1-                                                                      10
095500071105TIST73R1-                                                                      11
095600071105TIST73R1-                                                                      12
095700071105TIST73R1-                                                                      13
095800071105TIST73R1- Data errata                                                          14
095900071105TIST73R1- La data inizio deve essere maggiore della data fine                  15
096000071105TIST73R1- Non è possibile richiedere un periodo antecedente il 15/03/2004.     16
096100071105TIST73R1- Periodo troppo ampio...  massimo consentito: 1 mese.                 17
096200071105TIST73R1- Data "da" oltre limite minimo bolle, indicare una data più recente.  18
096300071105TIST73R1- Filiale indicata errata!                                             19
096400071105TIST73R1- Filiale indicata annullata o non utilizzabile!                       20
096500071105TIST73R1- Scelta obbligatoria.                                                 21
096600071105TIST73R1- Linea di arrivo obbligatoria!                                        22
096700071106TIST73R1- Utente non autorizzato alla linea di arrivo indicata!                23
096800151203TIST73R1- Cliente errato                                                       24
096900970526** FUD - FUNZIONI
097000970526                   -        (S/N)=N                        funz.01¬
097100970526                   -        (S/N)=N                        funz.02¬
097200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
097300970526                   -        (S/N)=N                        funz.04¬
097400970526                   -        (S/N)=N                        funz.05¬
097500991214F06=Conferma       -        (S/N)=S                        funz.06¬
097600970527                   -        (S/N)=N                        funz.07¬
097700970527                   -        (S/N)=N                        funz.08¬
097800970527                   -        (S/N)=N                        funz.09¬
097900970527                   -        (S/N)=N                        funz.10¬
098000970526                   -        (S/N)=N                        funz.11¬
098100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
098200970526                   -        (S/N)=N                        funz.13¬
098300970526                   -        (S/N)=N                        funz.14¬
098400970526                   -        (S/N)=N                        funz.15¬
098500970526                   -        (S/N)=N                        funz.16¬
098600970526                   -        (S/N)=N                        funz.17¬
098700970526                   -        (S/N)=N                        funz.18¬
098800970526                   -        (S/N)=N                        funz.19¬
098900970526                   -        (S/N)=N                        funz.20¬
099000970526                   -        (S/N)=N                        funz.21¬
099100970526                   -        (S/N)=N                        funz.22¬
099200970526                   -        (S/N)=N                        funz.23¬
099300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
