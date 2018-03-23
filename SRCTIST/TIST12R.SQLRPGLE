000100160304     /*END
000200160304      *------------------------------------------------------------------------*
000300160304      * REPORT COMMERCIALE - CREAZIONE REPORT - PARTE 1^                       *
000400160304      *------------------------------------------------------------------------*
000500980902     H DECEDIT('0,') DATEDIT(*DMY.)
000600160304      *------------------------------------------------------------------------*
000700160304      * DATA BASE
000800160304      *------------------------------------------------------------------------*
000900160304      *---
001000160304      * WORKFILE
001100160304      *---
001200050408     FWFREP11L  UF A E           K DISK
001300160308      *
001400160308      *?Anagrafica Commerciali?
001500160308     fAZCMM01L  if   e           k disk    prefix( XCU : 3 )
001600160304      *---
001700160304      * SALDI COMMERCIALE
001800160304      *---
001900980902     FTFSAC01L  IF   E           K DISK
002000160304      *---
002100160304      * ANAGRAFICHE CLIENTI
002200160304      *---
002300020516     FCNCLP01L  IF   E           K DISK    PREFIX(I_)
002400020516     FCNCLP00F  IF   E           K DISK    RENAME(CNCLP000:CNCLP2)
002500160304      *---
002600160304      * SALDI CLIENTE
002700160304      *---
002800990610     FSISDC01L  IF   E           K DISK
002900160304      *---
003000160304      * TABELLE
003100160304      *---
003200980903     FTABEL00F  IF   E           K DISK
003300160304      *---
003400160304      * ORGANIGRAMMA
003500160304      *---
003600160304     FAZORG01L  IF   E           K DISK
003700160304      *------------------------------------------------------------------------*
003800160304      * SCHIERE
003900160304      *------------------------------------------------------------------------*
004000160304      *
004100160304      * -?Filiali e relativi Aree e Distretti?
004200160304     d sk_Fil          s                   like(ORGfil)  dim(999)
004300160304     d                                     inz
004400160304     d sk_Are          s                   like(ORGcar)  dim(999)
004500160304     d                                     inz
004600160304     d sk_Dis          s                   like(ORGfl3)  dim(999)
004700160304     d                                     inz
004800160304      *---
004900160304      * FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA
005000160304      *---
005100980902     D FRBL            S              1    DIM(10)                              *RAGGRUPP. TIPO BOLL
005200980902     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
005300980902     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
005400160304      *---
005500160304      * PERIODO DA ELABORARE
005600160304      *---
005700980902     D AAA             S              4  0 DIM(12)                              *ANNI
005800980902     D MMM             S              2  0 DIM(12)                              *MESI
005900160304      *---
006000160304      * SCHIERE DI MEMORIZZAZIONE
006100160304      *---
006200981111     D $CTR            S              1    DIM(12)                              *CONTROLLO MEMORIZZA
006300980902     D POT             S              9  0 DIM(12)                              *POTENZIALI
006400980902     D PTR             S              9  0 DIM(12)                              *POTENZIALI CONTATTA
006500980902     D MAI             S              9  0 DIM(12)                              *MAIL INVIATI
006600980902     D VPT             S              9  0 DIM(12)                              *VISITE POTENZIALI
006700980902     D VCL             S              9  0 DIM(12)                              *VISITE CLIENTI
006800980902     D IOPT            S              9  0 DIM(12)                              *OFFERTE POTENZIALI
006900980902     D IOCL            S              9  0 DIM(12)                              *OFFERTE CLIENTI
007000980902     D ITPT            S              9  0 DIM(12)                              *TARIFFE POTENZIALI
007100980902     D ITCL            S              9  0 DIM(12)                              *TARIFFE CLIENTI
007200980902     D EOPT            S              9  0 DIM(12)                              *OFFERTE POTENZIALI
007300980902     D EOCL            S              9  0 DIM(12)                              *OFFERTE CLIENTI
007400980902     D ETPT            S              9  0 DIM(12)                              *TARIFFE POTENZIALI
007500980902     D ETCL            S              9  0 DIM(12)                              *TARIFFE CLIENTI
007600980902     D ISPN            S             11  0 DIM(12)                              *SPEDIZIONI    NEW
007700980902     D IPSN            S             11  1 DIM(12)                              *PESO          NEW
007800980902     D ICLN            S             11  0 DIM(12)                              *COLLI         NEW
007900980902     D IVLN            S             11  3 DIM(12)                              *VOLUME        NEW
008000980902     D IVCN            S             11  3 DIM(12)                              *VOLUME DI CML NEW
008100980902     D IVSN            S             11  3 DIM(12)                              *VOLUME CML    NEW
008200980902     D ISPA            S             11  0 DIM(12)                              *SPEDIZIONI    ACQ
008300980902     D IPSA            S             11  1 DIM(12)                              *PESO          ACQ
008400980902     D ICLA            S             11  0 DIM(12)                              *COLLI         ACQ
008500980902     D IVLA            S             11  3 DIM(12)                              *VOLUME        ACQ
008600980902     D IVCA            S             11  3 DIM(12)                              *VOLUME DI CML ACQ
008700980902     D IVSA            S             11  3 DIM(12)                              *VOLUME CML    ACQ
008800980902     D ESPN            S             11  0 DIM(12)                              *SPEDIZIONI    NEW
008900980902     D EPSN            S             11  1 DIM(12)                              *PESO          NEW
009000980902     D ECLN            S             11  0 DIM(12)                              *COLLI         NEW
009100980902     D EVLN            S             11  3 DIM(12)                              *VOLUME        NEW
009200980902     D EVCN            S             11  3 DIM(12)                              *VOLUME DI CML NEW
009300980902     D EVSN            S             11  3 DIM(12)                              *VOLUME CML    NEW
009400980902     D ESPA            S             11  0 DIM(12)                              *SPEDIZIONI    ACQ
009500980902     D EPSA            S             11  1 DIM(12)                              *PESO          ACQ
009600980902     D ECLA            S             11  0 DIM(12)                              *COLLI         ACQ
009700980902     D EVLA            S             11  3 DIM(12)                              *VOLUME        ACQ
009800980902     D EVCA            S             11  3 DIM(12)                              *VOLUME DI CML ACQ
009900980902     D EVSA            S             11  3 DIM(12)                              *VOLUME CML    ACQ
010000980902     D IRNA            S             15  3 DIM(12)                              *RICAVO REALE    A N
010100980902     D IPNA            S             15  3 DIM(12)                              *RICAVO PRESUNTO A N
010200980902     D IRNB            S             15  3 DIM(12)                              *RICAVO REALE    B N
010300980902     D IPNB            S             15  3 DIM(12)                              *RICAVO PRESUNTO B N
010400160907     D IRNC            S             15  3 DIM(12)                              *RICAVO REALE    CEN
010500160907     D IPNC            S             15  3 DIM(12)                              *RICAVO PRESUNTO CEN
010600980902     D IRNT            S             15  3 DIM(12)                              *RICAVO REALE    T N
010700980902     D IPNT            S             15  3 DIM(12)                              *RICAVO PRESUNTO T N
010800020215     D IRND            S             15  3 DIM(12)                              *RICAVO REALE    D N
010900020215     D IPND            S             15  3 DIM(12)                              *RICAVO PRESUNTO D N
011000980902     D IRAA            S             15  3 DIM(12)                              *RICAVO REALE    A A
011100980902     D IPAA            S             15  3 DIM(12)                              *RICAVO PRESUNTO A A
011200980902     D IRAB            S             15  3 DIM(12)                              *RICAVO REALE    B A
011300980902     D IPAB            S             15  3 DIM(12)                              *RICAVO PRESUNTO B A
011400160907     D IRAC            S             15  3 DIM(12)                              *RICAVO REALE    CEA
011500160907     D IPAC            S             15  3 DIM(12)                              *RICAVO PRESUNTO CEA
011600980902     D IRAT            S             15  3 DIM(12)                              *RICAVO REALE    T A
011700980902     D IPAT            S             15  3 DIM(12)                              *RICAVO PRESUNTO T A
011800020215     D IRAD            S             15  3 DIM(12)                              *RICAVO REALE    D A
011900020215     D IPAD            S             15  3 DIM(12)                              *RICAVO PRESUNTO D A
012000980902     D IVLD            S             11  3 DIM(12)                              *VOLUME DISTRIBUZION
012100980902     D ERNA            S             15  3 DIM(12)                              *RICAVO REALE    A N
012200980902     D EPNA            S             15  3 DIM(12)                              *RICAVO PRESUNTO A N
012300980902     D ERNB            S             15  3 DIM(12)                              *RICAVO REALE    B N
012400980902     D EPNB            S             15  3 DIM(12)                              *RICAVO PRESUNTO B N
012500160907     D ERNC            S             15  3 DIM(12)                              *RICAVO REALE    CEN
012600160907     D EPNC            S             15  3 DIM(12)                              *RICAVO PRESUNTO CEN
012700980902     D ERNT            S             15  3 DIM(12)                              *RICAVO REALE    T N
012800980902     D EPNT            S             15  3 DIM(12)                              *RICAVO PRESUNTO T N
012900020215     D ERND            S             15  3 DIM(12)                              *RICAVO REALE    D N
013000020215     D EPND            S             15  3 DIM(12)                              *RICAVO PRESUNTO D N
013100980902     D ERAA            S             15  3 DIM(12)                              *RICAVO REALE    A A
013200980902     D EPAA            S             15  3 DIM(12)                              *RICAVO PRESUNTO A A
013300980902     D ERAB            S             15  3 DIM(12)                              *RICAVO REALE    B A
013400980902     D EPAB            S             15  3 DIM(12)                              *RICAVO PRESUNTO B A
013500160907     D ERAC            S             15  3 DIM(12)                              *RICAVO REALE    CEA
013600160907     D EPAC            S             15  3 DIM(12)                              *RICAVO PRESUNTO CEA
013700980902     D ERAT            S             15  3 DIM(12)                              *RICAVO REALE    T A
013800980902     D EPAT            S             15  3 DIM(12)                              *RICAVO PRESUNTO T A
013900020215     D ERAD            S             15  3 DIM(12)                              *RICAVO REALE    D A
014000020215     D EPAD            S             15  3 DIM(12)                              *RICAVO PRESUNTO D A
014100980902     D EVLD            S             11  3 DIM(12)                              *VOLUME DISTRIBUZION
014200160907     d IRNCC           s             15  3 dim(12)                              Ricavo reale    C  N
014300160907     d IPNCC           s             15  3 dim(12)                              Ricavo presunto C  N
014400160907     d IRNE            s             15  3 dim(12)                              Ricavo reale    E  N
014500160907     d IPNE            s             15  3 dim(12)                              Ricavo presunto E  N
014600160907     d IRACC           s             15  3 dim(12)                              Ricavo reale    C  A
014700160907     d IPACC           s             15  3 dim(12)                              Ricavo presunto C  A
014800160907     d IRAE            s             15  3 dim(12)                              Ricavo reale    E  A
014900160907     d IPAE            s             15  3 dim(12)                              Ricavo presunto E  A
015000160907     d ERNCC           s             15  3 dim(12)                              Ricavo reale    C  N
015100160907     d EPNCC           s             15  3 dim(12)                              Ricavo presunto C  N
015200160907     d ERNE            s             15  3 dim(12)                              Ricavo reale    E  N
015300160907     d EPNE            s             15  3 dim(12)                              Ricavo presunto E  N
015400160907     d ERACC           s             15  3 dim(12)                              Ricavo reale    C  A
015500160907     d EPACC           s             15  3 dim(12)                              Ricavo presunto C  A
015600160907     d ERAE            s             15  3 dim(12)                              Ricavo reale    E  A
015700160907     d EPAE            s             15  3 dim(12)                              Ricavo presunto E  A
015800160304      *---
015900160304      * SCHIERA DI RIDEFINIZIONE ELENCO CLIENTI FIGLI
016000160304      *---
016100020328     D SKC             S             11    DIM(500)
016200160304      *---
016300160304      * SCHIERA DI MEMORIZZAZIONE ELNCO CLIENTI X AGENTE UNIFICANTE
016400160304      *---
016500020328     D SKCAGEU         S             11    DIM(5000)
016600160304      *------------------
016700160304      * RAGGRUPPAMENTI REGIONI/NETWORK
016800160304      *------------------
016900020329     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIO
017000020329     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RA
017100160304      *------------------
017200160304      * FASCE DI PESO
017300160304      *------------------
017400020402     D FPE             S              2    DIM(10)
017500160304      *------------------------------------------------------------------------*
017600160304      * STRUTTURE DATI                                                         *
017700160304      *------------------------------------------------------------------------*
017800160304      *---
017900160304      * DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
018000160304      *---
018100980902     D DS2W          E DS
018200160304      *---
018300160304      * DS COMPOSIZIONE DATA(6) -TFSAC-
018400160304      *---
018500981007     D                 DS                  INZ
018600980902     D  SACAAA                 1      4  0
018700980902     D  SACMMM                 5      6  0
018800980902     D  SACDAT                 1      6  0
018900160304      *---
019000160304      * DS SCOMPOSIZIONE DATA PRIMA SPEDIZIONE FATTURA(4) -CNCLP-
019100160304      *---
019200981007     D                 DS                  INZ
019300020409     D  CLPSEC                 1      2  0
019400020409     D  CLPANN                 3      4  0
019500020409     D  CLPMES                 5      6  0
019600020409     D  CLPDAT                 1      6  0
019700020409     D  DSDPS                  1      8  0
019800160304      *---
019900160304      * DS COMPOSIZIONE DATA SALDO(4) -SISDC-
020000160304      *---
020100981007     D                 DS                  INZ
020200990610     D  SDCANN                 1      4  0
020300990610     D  SDCMES                 5      6  0
020400990610     D  SDCDAT                 1      6  0
020500160304      *---
020600160304      * DS COMPOSIZIONE DATA(6) -WFREP-
020700160304      *---
020800981007     D                 DS                  INZ
020900980902     D  REPANN                 1      4  0
021000980902     D  REPMES                 5      6  0
021100980902     D  REPDAT                 1      6  0
021200160304      *---
021300160304      * DS PARAMETRI INPUT
021400160304      *---
021500980902     D PARAM           DS
021600980902     D  PARAMI                 1      6  0
021700980902     D  PARAAI                 1      4  0
021800980902     D  PARMMI                 5      6  0
021900980902     D  PARDAI                 3      6  0
022000980902     D  PARAMF                 7     12  0
022100980902     D  PARAAF                 7     10  0
022200980902     D  PARA2F                 9     10  0
022300980902     D  PARMMF                11     12  0
022400980902     D  PARDAF                 9     12  0
022500160304      *---
022600160304      * ARCHITETTURA
022700160304      *---
022800980902     D KPJBA         E DS
022900160304      *---
023000160304      * DS X REPERIMENTO/TEST CLIENTI PADRI/FIGLI
023100160304      *---
023200020328     D TIBS10        E DS                  EXTNAME(TIBS10DS)
023300160304      *------------------
023400160304      * DS DI LETTURA TABELLA "2M"
023500160304      *------------------
023600020329     D DS2M          E DS
023700160304      *------------------
023800160304      * DS DI LETTURA TABELLA "2L"
023900160304      *------------------
024000020402     D DS2L          E DS
024100160304      *
024200160304      * -?Anagrafica Commerciali?
024300160304     d AZCMM_ds      e ds                  extname(AZCMM00F)  inz
024400160304
024500160304       //--------------------------------------------------------------
024600160304       //?Variabili.                                                   ?
024700160304       //--------------------------------------------------------------
024800160304
024900160304       // -?Indici di schiera?
025000160304     d xF              s              3  0 inz
025100160304
025200160304       //--------------------------------------------------------------
025300160304       //?Definizione aree dati.                                       ?
025400160304       //--------------------------------------------------------------
025500160304
025600160304       // -?Dati utente?
025700160304     d §AzUte        e ds                  extname(AZUTE00F)
025800160304     d                                     dtaara
025900160304     d §DatiUte      e ds                  extname(dDatiUte)
026000160304     d                                     dtaara
026100160304
026200160304       //--------------------------------------------------------------
026300160304       //?Definizione prototipi procedure.                             ?
026400160304       //--------------------------------------------------------------
026500160304
026600160304       // -?Reperimento dati utente?
026700160304     d TIBS34ds      e ds
026800160304      /copy gaitrasrc/srcProtoPR,TIBS34R
026900160304
027000160304       // -?Parametri API QCAPCMD (Process Commands)?
027100160304     d Qcmd            s           2048    inz  varying
027200160304      /copy qSysInc/qRpgleSrc,QCAPCMD
027300160304       // -?API QCAPCMD (Process Commands)?
027400160304      /copy gaitrasrc/srcProtoPR,QCAPCMD
027500160304
027600160304       // -?Parametri gestione errori API.?
027700160304      /copy qsysinc/qrpglesrc,QUSEC
027800160304
027900160304      *------------------------------------------------------------------------*
028000160304      * MAIN LINE
028100160304      *------------------------------------------------------------------------*
028200980902     C*
028300160304     C* POSIZIONAMENTO E PRIMA LETTURA COMMERCIALI
028400980902     C                   EXSR      SETFIL
028500980902     C*
028600980902     C* CICLO FINO A FINE FILE
028700980902DO  1C     $FINE         DOWEQ     'N'
028800980902     C*
028900980902     C* OPERAZIONI PER NUOVO COMMERCIALE UNIFICANTE
029000980902     C                   EXSR      NEWUNI
029100980902     C*
029200980902     C* CICLO FINO A ROTTURA DI COMMERCIALE UNIFICANTE
029300980902DO  2C     $FINE         DOWEQ     'N'
029400160304     C     CMMuni        ANDEQ     DEPRGF
029500160304      *
029600160307if  3c                   If        xF > *zero
029700980902     C*
029800980902     C* ELABORA I SALDI ATTIVITA' COMMERCIALE
029900980902     C                   EXSR      ELASAC
030000020402     C*
030100020402     C* ELABORA I SALDI CLIENTE X AGENTE UNIFICANTE
030200020402     C                   EXSR      ELACLP
030300160307      *
030400160307e   3c                   EndIf
030500980902     C*
030600160304     C* LETTURA SUCCESSIVA COMMERCIALI
030700980902     C                   EXSR      LETFIL
030800160307      *
030900980902E   2C                   ENDDO                                                  *ROTTURA UNIFICANTE
031000980902     C*
031100980902     C* SCRIVE UN RECORD PER IL COMMERCIALE UNIFICANTE
031200160307if  2c                   If        xF > *zero
031300980902     C                   EXSR      SCRREP
031400160307e   2c                   EndIf
031500160307      *
031600980902E   1C                   ENDDO                                                  *FINE FILE
031700980902     C*
031800980902     C* OPERAZIONI FINALI
031900980902     C                   SETON                                        LR
032000980902     C*------------------------------------------------------------------------*
032100160304     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                  -COMMERCIALI- *
032200980902     C*------------------------------------------------------------------------*
032300980902     C     SETFIL        BEGSR
032400160304      /free
032500160304
032600160304         exec sql   set option   dynusrprf = *owner,
032700160304                                 closqlcsr = *endmod;
032800160304
032900160304         // -?Dichiarazione cursore per leggere i commerciali per Unificante?
033000160304         exec sql   declare A1   cursor for
033100160304                     select *
033200160304                       from AZCMM00F
033300160304                      where CMMatb = ' '
033400160304                   order by CMMuni, CMMcod;
033500160304
033600160304         // -?Apertura cursore?
033700160304         exec sql   open A1;
033800160304
033900160304         // -?Lettura cursore?
034000160304         exsr  LetFIL;
034100160304
034200160304      /end-free
034300160304      *
034400980902     C                   ENDSR
034500160304      *------------------------------------------------------------------------*
034600160304      * LETFIL - LETTURA PROSSIMO RECORD                         -COMMERCIALI- *
034700160304      *------------------------------------------------------------------------*
034800980902     C     LETFIL        BEGSR
034900160304      *
035000160304      /free
035100160304
035200160304         exec SQL   fetch next  from A1  into :AZCMM_ds;
035300160304
035400160304         Select;
035500160304           // -?E.o.F.?
035600160304           When  SqlCode = 100;
035700160304             $Fine = 'S';
035800160304           // -?Errore?
035900160304           When  SqlCode < *zero;
036000160304             exsr  sr_PrintErr;
036100160304             $Fine = 'S';
036200160304         EndSl;
036300160304
036400160304         // -?Chiusura cursore?
036500160304         if  $Fine = 'S';
036600160304           exec sql   close A1;
036700160304         endif;
036800160304
036900160304      /end-free
037000160304      *
037100980902     C                   ENDSR
037200980902     C*------------------------------------------------------------------------*
037300160304     C* NEWUNI - OPERAZIONI PER NUOVO COMMERCIALE UNIFICANTE                   *
037400980902     C*------------------------------------------------------------------------*
037500980902     C     NEWUNI        BEGSR
037600980902     C*
037700160304     C* MEMORIZZA IL NUOVO COMMERCIALE UNIFICANTE
037800160304     C                   Z-ADD     cmmUNI        DEPRGF                         *DEPOSITO
037900020328     C*
038000020328     C* PULISCO LA SCHIERA DEL SUO ELENCO CLIENTI
038100020328     C                   CLEAR                   SKCAGEU
038200020328     C                   Z-ADD     *ZEROS        Y                 4 0
038300980902     C*
038400160304     C* MEMORIZZA ALTRI DATI DEL COMMERCIALE UNIFICANTE
038500160304     C                   MOVEL     CMMuni        DEPFIL                         *FILIALE
038600160308      /free
038700160308
038800160308         // -?Reperimento Distretto & Area del commerciale unificante?
038900160308         xF   = %lookup( DEPfil : sk_Fil );
039000160308
039100160308         if  xF = *zero;
039200160308           leavesr;
039300160308         endif;
039400160308
039500160308         chain  ( CMMuni )  AZCMM000;
039600160308
039700160308         if  NOT %found(AZCMM01L);
039800160308           clear  xF;
039900160308           leavesr;
040000160308         endif;
040100160308
040200160308      /end-free
040300160308     c                   movel     sk_Dis(xF)    DEPDIV                         *DISTRETTO
040400160308     c                   z-add     sk_Are(xF)    DEPARE                         *AREA
040500160308     c                   movel     XCUdes        DEPDRG                         *DESCRIZIONE
040600160308     c                   z-add     XCUpli        DEIPCF                         *% LAVORO ITALIA
040700160308     c                   z-add     XCUple        DEEPCF                         *% LAVORO ESTERO
040800160308     c                   z-add     XCUdip        DEpdip                         *CODICE  DIPENDENTE
040900160308     c                   movel     XCUsoc        DEpsoc                         *SOCIETÀ DIPENDENTE
041000980902     C*
041100020215     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
041200981111     C                   CLEAR                   $CTR
041300980903     C                   CLEAR                   POT
041400980903     C                   CLEAR                   PTR
041500980903     C                   CLEAR                   MAI
041600980903     C                   CLEAR                   VPT
041700980903     C                   CLEAR                   VCL
041800980903     C                   CLEAR                   IOPT
041900980903     C                   CLEAR                   IOCL
042000980903     C                   CLEAR                   ITPT
042100980903     C                   CLEAR                   ITCL
042200980903     C                   CLEAR                   EOPT
042300980903     C                   CLEAR                   EOCL
042400980903     C                   CLEAR                   ETPT
042500980903     C                   CLEAR                   ETCL
042600980903     C                   CLEAR                   ISPN
042700980903     C                   CLEAR                   IPSN
042800980903     C                   CLEAR                   ICLN
042900980903     C                   CLEAR                   IVLN
043000980903     C                   CLEAR                   IVCN
043100980903     C                   CLEAR                   IVSN
043200980903     C                   CLEAR                   ISPA
043300980903     C                   CLEAR                   IPSA
043400980903     C                   CLEAR                   ICLA
043500980903     C                   CLEAR                   IVLA
043600980903     C                   CLEAR                   IVCA
043700980903     C                   CLEAR                   IVSA
043800980903     C                   CLEAR                   ESPN
043900980903     C                   CLEAR                   EPSN
044000980903     C                   CLEAR                   ECLN
044100980903     C                   CLEAR                   EVLN
044200980903     C                   CLEAR                   EVCN
044300980903     C                   CLEAR                   EVSN
044400980903     C                   CLEAR                   ESPA
044500980903     C                   CLEAR                   EPSA
044600980903     C                   CLEAR                   ECLA
044700980903     C                   CLEAR                   EVLA
044800980903     C                   CLEAR                   EVCA
044900980903     C                   CLEAR                   EVSA
045000980903     C                   CLEAR                   IRNA
045100980903     C                   CLEAR                   IPNA
045200980903     C                   CLEAR                   IRNB
045300980903     C                   CLEAR                   IPNB
045400980903     C                   CLEAR                   IRNC
045500980903     C                   CLEAR                   IPNC
045600980903     C                   CLEAR                   IRNT
045700980903     C                   CLEAR                   IPNT
045800020215     C                   CLEAR                   IRND
045900020215     C                   CLEAR                   IPND
046000980903     C                   CLEAR                   IRAA
046100980903     C                   CLEAR                   IPAA
046200980903     C                   CLEAR                   IRAB
046300980903     C                   CLEAR                   IPAB
046400980903     C                   CLEAR                   IRAC
046500980903     C                   CLEAR                   IPAC
046600980903     C                   CLEAR                   IRAT
046700980903     C                   CLEAR                   IPAT
046800020215     C                   CLEAR                   IRAD
046900020215     C                   CLEAR                   IPAD
047000980903     C                   CLEAR                   IVLD
047100980903     C                   CLEAR                   ERNA
047200980903     C                   CLEAR                   EPNA
047300980903     C                   CLEAR                   ERNB
047400980903     C                   CLEAR                   EPNB
047500980903     C                   CLEAR                   ERNC
047600980903     C                   CLEAR                   EPNC
047700980903     C                   CLEAR                   ERNT
047800980903     C                   CLEAR                   EPNT
047900020215     C                   CLEAR                   ERND
048000020215     C                   CLEAR                   EPND
048100980903     C                   CLEAR                   ERAA
048200980903     C                   CLEAR                   EPAA
048300980903     C                   CLEAR                   ERAB
048400980903     C                   CLEAR                   EPAB
048500980903     C                   CLEAR                   ERAC
048600980903     C                   CLEAR                   EPAC
048700980903     C                   CLEAR                   ERAT
048800980903     C                   CLEAR                   EPAT
048900020215     C                   CLEAR                   ERAD
049000020215     C                   CLEAR                   EPAD
049100980903     C                   CLEAR                   EVLD
049200160907     c                   clear                   IRNCC
049300160907     c                   clear                   IPNCC
049400160907     c                   clear                   IRNE
049500160907     c                   clear                   IPNE
049600160907     c                   clear                   IRACC
049700160907     c                   clear                   IPACC
049800160907     c                   clear                   IRAE
049900160907     c                   clear                   IPAE
050000160907     c                   clear                   ERNCC
050100160907     c                   clear                   EPNCC
050200160907     c                   clear                   ERNE
050300160907     c                   clear                   EPNE
050400160907     c                   clear                   ERACC
050500160907     c                   clear                   EPACC
050600160907     c                   clear                   ERAE
050700160907     c                   clear                   EPAE
050800980902     C*
050900980902     C                   ENDSR
051000980902     C*------------------------------------------------------------------------*
051100160304     C* ELASAC - ELABORA I SALDI ATTIVITA' COMMERCIALE UNIFICANTE              *
051200980902     C*------------------------------------------------------------------------*
051300980902     C     ELASAC        BEGSR
051400980902     C*
051500980902     C* POSIZIONAMENTO E PRIMA LETTURA FILE
051600980902     C                   EXSR      SETSAC
051700980902     C*
051800980902     C* CICLO FINO A FINE FILE
051900980902DO  1C     $FSAC         DOWEQ     'N'
052000980902     C*
052100980902     C* MEMORIZZA I DATI
052200980902     C                   EXSR      MEMSAC
052300980902     C*
052400980902     C* LETTURA SUCCESSIVA FILE
052500980902     C                   EXSR      LETSAC
052600980902E   1C                   ENDDO                                                  *FINE FILE
052700980902     C*
052800980902     C                   ENDSR
052900980902     C*------------------------------------------------------------------------*
053000160304     C* SETSAC - POSIZIONAMENTO E PRIMA LETTURA               -SALDI ATTIVITÀ- *
053100980902     C*------------------------------------------------------------------------*
053200980902     C     SETSAC        BEGSR
053300980902     C*
053400981111     C* REIMPOSTA LE VARIABILI DI CONTROLLO
053500981111     C                   MOVEL     'N'           $FSAC                          *FINE X TFSAC
053600160304     C                   Z-ADD     CMMcod        KSACMM                         *CODICE AGENTE
053700980902     C                   Z-ADD     PARAAI        KSAAAA                         *ANNO INIZIALE
053800980902     C                   Z-ADD     PARMMI        KSAMMM                         *MESE INIZIALE
053900980902     C     KEYSAC        SETLL     TFSAC01L                           98
054000980902IF  1C     *IN98         IFEQ      *ON                                          *FINE FILE
054100980902     C                   MOVEL     'S'           $FSAC                          *FINE LETTURA
054200980902X   1C                   ELSE
054300980902     C                   EXSR      LETSAC                                       *LETTURA RECORD
054400980902E   1C                   ENDIF
054500980902     C*
054600980902     C                   ENDSR
054700980902     C*------------------------------------------------------------------------*
054800160304     C* LETSAC - LETTURA PROSSIMO RECORD                      -SALDI ATTIVITÀ- *
054900980902     C*------------------------------------------------------------------------*
055000980902     C     LETSAC        BEGSR
055100980902     C*
055200980902     C* LETTURA FINO A:
055300980902     C*  - FINE FILE
055400980902     C*  - TROVATO RECORD VALIDO
055500980902     C                   MOVEL     'N'           WRECOK
055600980902DO  1C     $FSAC         DOWEQ     'N'
055700980902     C     WRECOK        ANDEQ     'N'
055800980902     C     KE2SAC        READE     TFSAC01L                               98
055900980902IF  2C     *IN98         IFEQ      *ON                                          *FINE ARCHIVIO
056000980902     C                   MOVEL     'S'           $FSAC                          *FINE LETTURA
056100980902X   2C                   ELSE
056200980902     C                   EXSR      CHKSAC                                       *CONTROLLA RECORD
056300980902E   2C                   ENDIF
056400980902E   1C                   ENDDO
056500980902     C*
056600980902     C                   ENDSR
056700980902     C*------------------------------------------------------------------------*
056800160304     C* CHKSAC - CONTROLLA VALIDITA' RECORD                   -SALDI ATTIVITÀ- *
056900980902     C*------------------------------------------------------------------------*
057000980902     C     CHKSAC        BEGSR
057100980902     C*
057200980902     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
057300980902     C*
057400980902     C* PERIODO NON RICHIESTO, ESCLUDE
057500980902IF  1C     SACDAT        IFGE      PARAMI
057600980902     C     SACDAT        ANDLE     PARAMF
057700980902X   1C                   ELSE
057800980902     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057900980902IF  2C     SACDAT        IFGT      PARAMF                                       *FINE PERIODO
058000981008     C                   MOVEL     'S'           $FSAC                          *FINE FILE
058100980902E   2C                   ENDIF
058200980902E   1C                   ENDIF
058300980902     C*
058400980902     C* RECORD FASULLO, ESCLUDE
058500981008IF  1C     SACFFZ        IFNE      *BLANKS
058600980902     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
058700980902E   1C                   ENDIF
058800980902     C*
058900980902     C                   ENDSR
059000980902     C*------------------------------------------------------------------------*
059100160304     C* MEMSAC - MEMORIZZA I DATI                             -SALDI ATTIVITÀ- *
059200980902     C*------------------------------------------------------------------------*
059300980902     C     MEMSAC        BEGSR
059400980902     C*
059500980902     C* RECUPERA IL MESE
059600980902     C                   SETOFF                                           28
059700980902     C                   Z-ADD     1             M                 2 0
059800980903     C     SACMMM        LOOKUP    MMM(M)                                 28
059900980902IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
060000981111     C                   MOVEL     'S'           $CTR(M)                        *MEMORIZZATO QC
060100980902     C*---
060200980902     C* ITALIA e ESTERO
060300980902     C*---
060400980903IF  2C     SACFIE        IFEQ      *BLANKS
060500981229     C* POTENZIALI DA TRATTARE
060600980903IF  3C     SACAPL        IFEQ      'P'
060700980902     C                   ADD       SACTO1        POT(M)
060800980903E   3C                   ENDIF
060900980902     C* VISITE POTENZIALI
061000980903IF  3C     SACAPL        IFEQ      'N'
061100980902     C                   ADD       SACTO1        VPT(M)
061200980903E   3C                   ENDIF
061300980902     C* VISITE CLIENTI
061400980903IF  3C     SACAPL        IFEQ      'C'
061500980902     C                   ADD       SACTO1        VCL(M)
061600980903E   3C                   ENDIF
061700980903E   2C                   ENDIF
061800980902     C*---
061900980902     C* ITALIA
062000980903     C*---
062100980903IF  2C     SACFIE        IFEQ      'I'
062200980902     C* OFFERTE POTENZIALI
062300980903IF  3C     SACAPL        IFEQ      'N'
062400980902     C                   ADD       SACTO2        IOPT(M)
062500980903E   3C                   ENDIF
062600980902     C* OFFERTE CLIENTI
062700980903IF  3C     SACAPL        IFEQ      'C'
062800980902     C                   ADD       SACTO2        IOCL(M)
062900980903E   3C                   ENDIF
063000980902     C* TARIFFE POTENZIALI
063100980903IF  3C     SACAPL        IFEQ      'N'
063200980902     C                   ADD       SACTO3        ITPT(M)
063300980903E   3C                   ENDIF
063400980902     C* TARIFFE CLIENTI
063500980903IF  3C     SACAPL        IFEQ      'C'
063600980902     C                   ADD       SACTO3        ITCL(M)
063700980903E   3C                   ENDIF
063800980903E   2C                   ENDIF
063900980902     C*---
064000980902     C* ESTERO
064100980902     C*---
064200980903IF  2C     SACFIE        IFEQ      'E'
064300980902     C* OFFERTE POTENZIALI
064400980903IF  3C     SACAPL        IFEQ      'N'
064500980902     C                   ADD       SACTO2        EOPT(M)
064600980903E   3C                   ENDIF
064700980902     C* OFFERTE CLIENTI
064800980903IF  3C     SACAPL        IFEQ      'C'
064900980902     C                   ADD       SACTO2        EOCL(M)
065000980903E   3C                   ENDIF
065100980902     C* TARIFFE POTENZIALI
065200980903IF  3C     SACAPL        IFEQ      'N'
065300980902     C                   ADD       SACTO3        ETPT(M)
065400980903E   3C                   ENDIF
065500980902     C* TARIFFE CLIENTI
065600980903IF  3C     SACAPL        IFEQ      'C'
065700980902     C                   ADD       SACTO3        ETCL(M)
065800980903E   3C                   ENDIF
065900980903E   2C                   ENDIF
066000980903E   1C                   ENDIF
066100980902     C*
066200980902     C                   ENDSR
066300980902     C*------------------------------------------------------------------------*
066400160304     C* ELACLP - ELABORA I SALDI CLIENTE                                       *
066500980902     C*------------------------------------------------------------------------*
066600980902     C     ELACLP        BEGSR
066700980902     C*
066800980902     C* POSIZIONAMENTO E PRIMA LETTURA FILE
066900980902     C                   EXSR      SETCLP
067000980902     C*
067100980902     C* CICLO FINO A FINE FILE
067200980902DO  1C     $FCLP         DOWEQ     'N'
067300020328     C*
067400020328     C* CICLO SULLA SCHIERA DEI FIGLI
067500020328     C     1             DO        500           C                 3 0
067600020328     C     SKC(C)        IFGT      *ZEROS
067700020328     C*
067800020402     C* VERIFICO CHE IL CLIENTE CORRENTE NON SIA GIA' SATO CONSIDERATO X L'AGENTE UNIFICANTE
067900020328     C     SKC(C)        LOOKUP    SKCAGEU                                55
068000020328     C                   IF        *IN55 = *OFF
068100020328     C*
068200020328     C* SE NN TROVATO LO AGGIUNGO E PROSEGUO L'ELABORAZIONE
068300020328     C                   ADD       1             Y
068400020328     C                   EVAL      SKCAGEU(Y) = SKC(C)
068500020328     C*
068600020328     C* "CHAINO" IL CLIENTE FIGLIO CORRENTE
068700020328     C                   MOVE      SKC(C)        KLPKSC
068800020516     C     KEYCLP2       CHAIN     CNCLP00F
068900020516     C                   IF        %FOUND(CNCLP00F)
069000980902     C*
069100980902     C* OPERAZIONI PER NUOVO CLIENTE
069200980902     C                   EXSR      NEWKSC
069300980902     C*
069400980902     C* ELABORA I SALDI CLIENTE TESTATA
069500980902     C                   EXSR      ELASDC
069600020328     C                   ENDIF
069700020328     C                   ENDIF
069800020328     C*
069900020328     C                   ELSE
070000020328     C                   LEAVE
070100020328     C                   ENDIF
070200020328     C                   ENDDO
070300980902     C*
070400980902     C* LETTURA SUCCESSIVA FILE
070500980902     C                   EXSR      LETCLP
070600020328     C*
070700980902E   1C                   ENDDO                                                  *FINE FILE
070800980902     C*
070900980902     C                   ENDSR
071000980902     C*------------------------------------------------------------------------*
071100160304     C* SETCLP - POSIZIONAMENTO E PRIMA LETTURA             -ARCHIVIO CLIENTI- *
071200980902     C*------------------------------------------------------------------------*
071300980902     C     SETCLP        BEGSR
071400981111     C*
071500981111     C* REIMPOSTA LE VARIABILI DI CONTROLLO
071600981111     C                   MOVEL     'N'           $FCLP                          *FINE X CNCLP
071700980902     C*
071800020906     C                   Z-ADD     1             KLPKUT                         *UTENTE
071900160304     C                   Z-ADD     DUTkci        KLPKCC                         *CAPOCONTO
072000160304     C                   Z-ADD     CMMcod        KLPAGE                         *AGENTE
072100020516     C     KEYCLP        SETLL     CNCLP01L                           98
072200980902IF  1C     *IN98         IFEQ      *ON                                          *FINE FILE
072300980902     C                   MOVEL     'S'           $FCLP                          *FINE LETTURA
072400980902X   1C                   ELSE
072500980902     C                   EXSR      LETCLP                                       *LETTURA RECORD
072600980902E   1C                   ENDIF
072700980902     C*
072800980902     C                   ENDSR
072900980902     C*------------------------------------------------------------------------*
073000160304     C* LETCLP - LETTURA PROSSIMO RECORD                    -ARCHIVIO CLIENTI- *
073100980902     C*------------------------------------------------------------------------*
073200980902     C     LETCLP        BEGSR
073300980902     C*
073400980902     C* LETTURA FINO A:
073500980902     C*  - FINE FILE
073600980902     C*  - TROVATO RECORD VALIDO
073700980902     C                   MOVEL     'N'           WRECOK
073800980902DO  1C     $FCLP         DOWEQ     'N'
073900980902     C     WRECOK        ANDEQ     'N'
074000020516     C     KEYCLP        READE     CNCLP01L                               98
074100980902IF  2C     *IN98         IFEQ      *ON                                          *FINE ARCHIVIO
074200980902     C                   MOVEL     'S'           $FCLP                          *FINE LETTURA
074300980902X   2C                   ELSE
074400980902     C                   EXSR      CHKCLP                                       *CONTROLLA RECORD
074500980902E   2C                   ENDIF
074600980902E   1C                   ENDDO
074700980902     C*
074800980902     C                   ENDSR
074900980902     C*------------------------------------------------------------------------*
075000160304     C* CHKCLP - CONTROLLA VALIDITA' RECORD                 -ARCHIVIO CLIENTI- *
075100980902     C*------------------------------------------------------------------------*
075200980902     C     CHKCLP        BEGSR
075300980902     C*
075400980902     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
075500020328     C                   CLEAR                   SKC
075600020328     C*
075700020402     C* VERIFICO SE IL CLIENTE E' PADRE
075800020328     C                   CLEAR                   TIBS10
075900020328     C                   Z-ADD     PARAMGF       D10DRF                         *DATA RIFERIMENTO
076000020328     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
076100020402     C                   MOVEL     'F'           D10PAF                         *CERCO COME PADRE
076200020328     C                   Z-ADD     I_CLPKSC      D10COD                         *CODICE CLIENTE
076300020328     C                   CALL      'TIBS10R'
076400020328     C                   PARM                    TIBS10
076500020402     C     D10ERR        IFEQ      *BLANKS                                      *SE PADRE
076600020328     C                   MOVEA     D10SKC        SKC                            *CLIENTI FIGLI
076700020328     C                   ELSE
076800020402     C*
076900020402     C* VERIFICO SE IL CLIENTE E' FIGLIO
077000020402     C                   CLEAR                   TIBS10
077100020402     C                   Z-ADD     PARAMGF       D10DRF                         *DATA RIFERIMENTO
077200020402     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
077300020402     C                   MOVEL     'P'           D10PAF                         *CERCO COME FIGLIO
077400020402     C                   Z-ADD     I_CLPKSC      D10COD                         *CODICE CLIENTE
077500020402     C                   CALL      'TIBS10R'
077600020402     C                   PARM                    TIBS10
077700020402     C     D10ERR        IFEQ      *BLANKS                                      *SE FIGLIO
077800020402     C                   MOVEL     'N'           WRECOK                         *NON LO CONSIDERO
077900020402     C                   ELSE
078000020402     C*
078100020402     C* SE NN E' PADRE E NN E' FIGLIO VUOL DIRE CHE E' 1 CLIENTE NN LEGATO E QUINDI LO CONSIDERO
078200020328     C                   MOVEL     D10COD        SKC(1)
078300020328     C                   ENDIF
078400020402     C                   ENDIF
078500980902     C*
078600980902     C                   ENDSR
078700980902     C*------------------------------------------------------------------------*
078800160304     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE                                  *
078900980902     C*------------------------------------------------------------------------*
079000980902     C     NEWKSC        BEGSR
079100980902     C*
079200980902     C* REIMPOSTA LE VARIABILI DI LAVORO
079300980902     C                   MOVEL     *BLANKS       CLPCLN            1            *NUOVO O ACQUISITO
079400980902     C*
079500980902     C* CLIENTE NUOVO/ACQUISITO
079600020409     C                   Z-ADD     CLPDPS        DSDPS                          *DAT PRIMA SPEDIZION
079700020409     C*
079800020409     C* AGGIUNGE IL SECOLO ALLA DATA PRIMA SPEDIZIONE
079900020409IF  1C     CLPANN        IFGT      60
080000020409     C                   Z-ADD     19            CLPSEC
080100020409X   1C                   ELSE
080200020409IF  2C     CLPANN        IFGT      *ZEROS
080300020409     C                   Z-ADD     20            CLPSEC
080400020409E   2C                   ENDIF
080500020409E   1C                   ENDIF
080600020409     C*
080700980902IF  1C     CLPDAT        IFEQ      *ZEROS                                       *NON ANCORA FATTURAT
080800020409     C     CLPDAT        ORGE      PARAMI                                       *FATTURATO NELL'ANNO
080900980902     C                   MOVEL     'N'           CLPCLN                         *NUOVO
081000980902X   1C                   ELSE                                                   *FATTURATO PRIMA
081100980902     C                   MOVEL     'A'           CLPCLN                         *ACQUISITO
081200980902E   1C                   ENDIF
081300980902     C*
081400980902     C                   ENDSR
081500980902     C*------------------------------------------------------------------------*
081600160304     C* ELASDC - ELABORA I SALDI CLIENTE                                       *
081700980902     C*------------------------------------------------------------------------*
081800980902     C     ELASDC        BEGSR
081900980902     C*
082000980902     C* POSIZIONAMENTO E PRIMA LETTURA FILE
082100980902     C                   EXSR      SETSDC
082200980902     C*
082300980902     C* CICLO FINO A FINE FILE
082400980902DO  1C     $FSDC         DOWEQ     'N'
082500980902     C*
082600980902     C* MEMORIZZA I DATI
082700980902     C                   EXSR      MEMSDC
082800980902     C*
082900980902     C* LETTURA SUCCESSIVA FILE
083000980902     C                   EXSR      LETSDC
083100980902E   1C                   ENDDO                                                  *FINE FILE
083200980902     C*
083300980902     C                   ENDSR
083400980902     C*------------------------------------------------------------------------*
083500160304     C* SETSDC - POSIZIONAMENTO E PRIMA LETTURA                -SALDI CLEINTE- *
083600980902     C*------------------------------------------------------------------------*
083700980902     C     SETSDC        BEGSR
083800981111     C*
083900981111     C* AZZERA LE VARIABILI DI CONTROLLO
084000990610     C                   MOVEL     'N'           $FSDC                          *FINE X SISDC
084100980902     C*
084200980902     C                   Z-ADD     CLPKSC        KSCKSC                         *CLIENTE
084300990610     C                   Z-ADD     PARAAI        KSCANN                         *ANNO INIZIALE
084400980902     C                   Z-ADD     PARMMI        KSCMES                         *MESE INIZIALE
084500990610     C     KEYSDC        SETLL     SISDC01L                           97
084600981008IF  1C     *IN97         IFEQ      *ON                                          *FINE FILE
084700980902     C                   MOVEL     'S'           $FSDC                          *FINE LETTURA
084800980902X   1C                   ELSE
084900980902     C                   EXSR      LETSDC                                       *LETTURA RECORD
085000980902E   1C                   ENDIF
085100980902     C*
085200980902     C                   ENDSR
085300980902     C*------------------------------------------------------------------------*
085400160304     C* LETSDC - LETTURA PROSSIMO RECORD                       -SALDI CLIENTE- *
085500980902     C*------------------------------------------------------------------------*
085600980902     C     LETSDC        BEGSR
085700980902     C*
085800980902     C* LETTURA FINO A:
085900980902     C*  - FINE FILE
086000980902     C*  - TROVATO RECORD VALIDO
086100980902     C                   MOVEL     'N'           WRECOK
086200981007DO  1C     $FSDC         DOWEQ     'N'
086300980902     C     WRECOK        ANDEQ     'N'
086400990610     C     KE2SDC        READE     SISDC01L                               97
086500981008IF  2C     *IN97         IFEQ      *ON                                          *FINE ARCHIVIO
086600980902     C                   MOVEL     'S'           $FSDC                          *FINE LETTURA
086700980902X   2C                   ELSE
086800980902     C                   EXSR      CHKSDC                                       *CONTROLLA RECORD
086900980902E   2C                   ENDIF
087000980902E   1C                   ENDDO
087100980902     C*
087200980902     C                   ENDSR
087300980902     C*------------------------------------------------------------------------*
087400160304     C* CHKSDC - CONTROLLA VALIDITA' RECORD                    -SALDI CLIENTE- *
087500980902     C*------------------------------------------------------------------------*
087600980902     C     CHKSDC        BEGSR
087700980902     C*
087800980902     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
087900020516     C*
088000020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
088100020516IF  1C     WRECOK        IFEQ      'S'
088200020516IF  2C     SDCNSP        IFEQ      *ZEROS
088300020516     C     SDCPKG        ANDEQ     *ZEROS
088400020516     C     SDCPCC        ANDEQ     *ZEROS
088500020516     C     SDCPLS        ANDEQ     *ZEROS
088600020516     C     SDCNCO        ANDEQ     *ZEROS
088700020516     C     SDCVOL        ANDEQ     *ZEROS
088800020516     C     SDCVCC        ANDEQ     *ZEROS
088900020516     C     SDCVLS        ANDEQ     *ZEROS
089000020516     C     SDCIRB        ANDEQ     *ZEROS
089100020516     C     SDCIRR        ANDEQ     *ZEROS
089200020516     C     SDCINC        ANDEQ     *ZEROS
089300020516     C     SDCIRP        ANDEQ     *ZEROS
089400020516     C     SDCIRC        ANDEQ     *ZEROS
089500020516     C     SDCIRD        ANDEQ     *ZEROS
089600020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
089700020516E   2C                   ENDIF
089800020516E   1C                   ENDIF
089900980902     C*
090000980902     C* PERIODO NON RICHIESTO, ESCLUDE
090100990712IF  1C     SDCDAT        IFGE      PARAMI
090200990712     C     SDCDAT        ANDLE     PARAMF
090300980902X   1C                   ELSE
090400980902     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090500990712IF  2C     SDCDAT        IFGT      PARAMF                                       *FINE PERIODO
090600981008     C                   MOVEL     'S'           $FSDC                          *FINE FILE
090700980902E   2C                   ENDIF
090800980902E   1C                   ENDIF
090900020402     C*
091000020402     C* CONTROLLO FASCE DI PESO
091100020402     C                   Z-ADD     1             P
091200020402     C                   SETOFF                                           28
091300020402     C     SDCFPE        LOOKUP    FPE(P)                                 28
091400020402IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
091500020402     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
091600020402E   1C                   ENDIF
091700980902     C*
091800980902     C                   ENDSR
091900980902     C*------------------------------------------------------------------------*
092000160304     C* MEMSDC - MEMORIZZA I DATI                              -SALDI CLIENTE- *
092100980902     C*------------------------------------------------------------------------*
092200980902     C     MEMSDC        BEGSR
092300020329     C*
092400020329     C* DETERMINA IL NETWORK
092500020329     C                   MOVEL     *BLANKS       DEPNTWP           3
092600020329     C                   Z-ADD     1             R
092700020329     C     SDCREP        LOOKUP    RGREG(R)                               55
092800020329     C                   IF        *IN55 = *ON
092900020329     C                   EVAL      DEPNTWP = RGNTW(R)
093000020329     C                   ENDIF
093100020329     C*
093200020329     C                   MOVEL     *BLANKS       DEPNTWA           3
093300020329     C                   Z-ADD     1             R
093400020329     C     SDCREA        LOOKUP    RGREG(R)                               55
093500020329     C                   IF        *IN55 = *ON
093600020329     C                   EVAL      DEPNTWA = RGNTW(R)
093700020329     C                   ENDIF
093800980902     C*
093900980902     C* IMPOSTA FLAG ITALIA/ESTERO
094000980904     C                   MOVEL     *BLANKS       SDCFIE            1            *ITALIA/ESTERO
094100020329IF  1C     DEPNTWP       IFNE      *BLANKS
094200020329     C     DEPNTWA       ORNE      *BLANKS
094300980904     C                   MOVEL     'E'           SDCFIE                         *ESTERO
094400980902X   1C                   ELSE
094500980904     C                   MOVEL     'I'           SDCFIE                         *ITALIA
094600980902E   1C                   ENDIF
094700980902     C*
094800980902     C* RECUPERA IL MESE
094900980902     C                   SETOFF                                           28
095000980902     C                   Z-ADD     1             M
095100980903     C     SDCMES        LOOKUP    MMM(M)                                 28
095200980902IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
095300980902     C*
095400980902     C* RECUPERA IL RAGGRUPPAMENTO BOLLE
095500980902     C                   SETOFF                                           28
095600980902     C                   Z-ADD     1             I
095700980902     C     SDCRBL        LOOKUP    FRBL(I)                                28
095800980902IF  2C     *IN28         IFEQ      *ON                                          *TROVATO
095900981111     C                   MOVEL     'S'           $CTR(M)                        *MEMORIZZATO QC
096000980902     C*
096100980902     C* BOLLA NON VALIDA COME N°SPEDIZIONE, CANCELLA I DATI
096200980902IF  3C     FSP(I)        IFNE      'S'
096300980902     C                   Z-ADD     *ZEROS        SDCNSP                         *SPEDIZIONI
096400980902     C                   Z-ADD     *ZEROS        SDCPKG                         *PESO
096500020828     C                   Z-ADD     *ZEROS        SDCPCC                         *PESO CON CML
096600020828     C                   Z-ADD     *ZEROS        SDCPLS                         *PESO CML
096700980902     C                   Z-ADD     *ZEROS        SDCNCO                         *NUMERO COLLI
096800980902     C                   Z-ADD     *ZEROS        SDCVOL                         *VOLUME TOTALE
096900980902     C                   Z-ADD     *ZEROS        SDCVCC                         *VOLUME CON CML
097000980902     C                   Z-ADD     *ZEROS        SDCVLS                         *VOLUME CML
097100980902E   3C                   ENDIF
097200980902     C*
097300980902     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
097400980902     C                   Z-ADD     *ZEROS        RICDEL           15 3
097500980902     C     SDCIRD        ADD       SDCIRR        RICDEL
097600980902     C     SDCINC        ADD       RICDEL        RICDEL
097700980902     C*---
097800980902     C* ITALIA
097900980902     C*---
098000980904IF  3C     SDCFIE        IFEQ      'I'
098100980902     C***
098200980902     C* NUOVI
098300980902     C***
098400980902IF  4C     CLPCLN        IFEQ      'N'
098500980902     C* SPEDIZIONI
098600980902     C                   ADD       SDCNSP        ISPN(M)
098700980902     C* PESO
098800980902     C                   ADD       SDCPKG        IPSN(M)
098900980902     C* COLLI
099000980902     C                   ADD       SDCNCO        ICLN(M)
099100980902     C* VOLUME
099200980902     C                   ADD       SDCVOL        IVLN(M)
099300980902     C* VOLUME DI CML
099400980902     C                   ADD       SDCVCC        IVCN(M)
099500980902     C* VOLUME CML
099600980902     C                   ADD       SDCVLS        IVSN(M)
099700020215     C* RICAVO REALE e PRESUNTO  "A"
099800980902IF  5C     CLPCLV        IFEQ      'A'
099900980902     C                   ADD       RICDEL        IRNA(M)
100000980902     C                   ADD       SDCIRP        IPNA(M)
100100980902E   5C                   ENDIF
100200980902     C* RICAVO REALE e PRESUNTO  "B"
100300980902IF  5C     CLPCLV        IFEQ      'B'
100400980902     C                   ADD       RICDEL        IRNB(M)
100500980902     C                   ADD       SDCIRP        IPNB(M)
100600980902E   5C                   ENDIF
100700160907     C* RICAVO REALE e PRESUNTO  "C" + "E" e "C"
100800980902IF  5C     CLPCLV        IFEQ      'C'
100900980902     C                   ADD       RICDEL        IRNC(M)
101000980902     C                   ADD       SDCIRP        IPNC(M)
101100160907     c                   add       ricdel        irncc(m)
101200160907     c                   add       sdcirp        ipncc(m)
101300980902E   5C                   ENDIF
101400160907      * RICAVO REALE e PRESUNTO  "C" + "E" e "E"
101500160907IF  5c                   IF        CLPclv = 'E'
101600160907     c                   add       ricdel        irnc(m)
101700160907     c                   add       sdcirp        ipnc(m)
101800160907     c                   add       ricdel        irne(m)
101900160907     c                   add       sdcirp        ipne(m)
102000160907E   5c                   ENDIF
102100980902     C* RICAVO REALE e PRESUNTO  "T"
102200980902IF  5C     CLPCLV        IFEQ      'T'
102300980902     C                   ADD       RICDEL        IRNT(M)
102400980902     C                   ADD       SDCIRP        IPNT(M)
102500980902E   5C                   ENDIF
102600020215     C* RICAVO REALE e PRESUNTO  "D"
102700020215IF  5C     CLPCLV        IFEQ      'D'
102800020215     C                   ADD       RICDEL        IRND(M)
102900020215     C                   ADD       SDCIRP        IPND(M)
103000020215E   5C                   ENDIF
103100980902E   4C                   ENDIF
103200980902     C***
103300980902     C* ACQUISITI
103400980902     C***
103500980902IF  4C     CLPCLN        IFEQ      'A'
103600980902     C* SPEDIZIONI
103700980902     C                   ADD       SDCNSP        ISPA(M)
103800980902     C* PESO
103900980902     C                   ADD       SDCPKG        IPSA(M)
104000980902     C* COLLI
104100980902     C                   ADD       SDCNCO        ICLA(M)
104200980902     C* VOLUME
104300980902     C                   ADD       SDCVOL        IVLA(M)
104400980902     C* VOLUME DI CML
104500980902     C                   ADD       SDCVCC        IVCA(M)
104600980902     C* VOLUME CML
104700980902     C                   ADD       SDCVLS        IVSA(M)
104800020215     C* RICAVO REALE e PRESUNTO  "A"
104900980902IF  5C     CLPCLV        IFEQ      'A'
105000980902     C                   ADD       RICDEL        IRAA(M)
105100980902     C                   ADD       SDCIRP        IPAA(M)
105200980902E   5C                   ENDIF
105300980902     C* RICAVO REALE e PRESUNTO  "B"
105400980902IF  5C     CLPCLV        IFEQ      'B'
105500980902     C                   ADD       RICDEL        IRAB(M)
105600980902     C                   ADD       SDCIRP        IPAB(M)
105700980902E   5C                   ENDIF
105800160907     C* RICAVO REALE e PRESUNTO  "C" + "E" e "C"
105900980902IF  5C     CLPCLV        IFEQ      'C'
106000980902     C                   ADD       RICDEL        IRAC(M)
106100980902     C                   ADD       SDCIRP        IPAC(M)
106200160907     c                   add       ricdel        iracc(m)
106300160907     c                   add       sdcirp        ipacc(m)
106400980902E   5C                   ENDIF
106500160907      * RICAVO REALE e PRESUNTO  "C" + "E" e "E"
106600160907     c                   IF        CLPclv = 'E'
106700160907     c                   add       ricdel        irac(m)
106800160907     c                   add       sdcirp        ipac(m)
106900160907     c                   add       ricdel        irae(m)
107000160907     c                   add       sdcirp        ipae(m)
107100160907E   5c                   ENDIF
107200980902     C* RICAVO REALE e PRESUNTO  "T"
107300980902IF  5C     CLPCLV        IFEQ      'T'
107400980902     C                   ADD       RICDEL        IRAT(M)
107500980902     C                   ADD       SDCIRP        IPAT(M)
107600980902E   5C                   ENDIF
107700020215IF  5C     CLPCLV        IFEQ      'D'
107800020215     C                   ADD       RICDEL        IRAD(M)
107900020215     C                   ADD       SDCIRP        IPAD(M)
108000020215E   5C                   ENDIF
108100980902E   4C                   ENDIF
108200980902     C***
108300980902     C* NUOVI e ACQUISITI
108400980902     C***
108500980902     C* VOLUME DISTRIBUZIONE
108600980902IF  4C     SDCTSP        IFEQ      'D'
108700980902     C                   ADD       SDCVOL        IVLD(M)
108800980902E   4C                   ENDIF
108900980902E   3C                   ENDIF
109000980902     C*---
109100980902     C* ESTERO
109200980902     C*---
109300980904IF  3C     SDCFIE        IFEQ      'E'
109400980902     C***
109500980902     C* NUOVI
109600980902     C***
109700980902IF  4C     CLPCLN        IFEQ      'N'
109800980902     C* SPEDIZIONI
109900980902     C                   ADD       SDCNSP        ESPN(M)
110000980902     C* PESO
110100980902     C                   ADD       SDCPKG        EPSN(M)
110200980902     C* COLLI
110300980902     C                   ADD       SDCNCO        ECLN(M)
110400980902     C* VOLUME
110500980902     C                   ADD       SDCVOL        EVLN(M)
110600980902     C* VOLUME DI CML
110700980902     C                   ADD       SDCVCC        EVCN(M)
110800980902     C* VOLUME CML
110900980902     C                   ADD       SDCVLS        EVSN(M)
111000020215     C* RICAVO REALE e PRESUNTO  "A"
111100980902IF  5C     CLPCLV        IFEQ      'A'
111200980902     C                   ADD       RICDEL        ERNA(M)
111300980902     C                   ADD       SDCIRP        EPNA(M)
111400980902E   5C                   ENDIF
111500980902     C* RICAVO REALE e PRESUNTO  "B"
111600980902IF  5C     CLPCLV        IFEQ      'B'
111700980902     C                   ADD       RICDEL        ERNB(M)
111800980902     C                   ADD       SDCIRP        EPNB(M)
111900980902E   5C                   ENDIF
112000160907     C* RICAVO REALE e PRESUNTO  "C" + "E" e "C"
112100980902IF  5C     CLPCLV        IFEQ      'C'
112200980902     C                   ADD       RICDEL        ERNC(M)
112300980902     C                   ADD       SDCIRP        EPNC(M)
112400160907     c                   add       ricdel        erncc(m)
112500160907     c                   add       sdcirp        epncc(m)
112600980902E   5C                   ENDIF
112700160907      * RICAVO REALE e PRESUNTO  "C" + "E" e "E"
112800160907IF  5c                   IF        CLPclv = 'E'
112900160907     c                   add       ricdel        ernc(m)
113000160907     c                   add       sdcirp        epnc(m)
113100160907     c                   add       ricdel        erne(m)
113200160907     c                   add       sdcirp        epne(m)
113300160907E   5c                   ENDIF
113400980902     C* RICAVO REALE e PRESUNTO  "T"
113500980902IF  5C     CLPCLV        IFEQ      'T'
113600980902     C                   ADD       RICDEL        ERNT(M)
113700980902     C                   ADD       SDCIRP        EPNT(M)
113800980902E   5C                   ENDIF
113900020215IF  5C     CLPCLV        IFEQ      'D'
114000020215     C                   ADD       RICDEL        ERND(M)
114100020215     C                   ADD       SDCIRP        EPND(M)
114200020215E   5C                   ENDIF
114300980902E   4C                   ENDIF
114400980902     C***
114500980902     C* ACQUISITI
114600980902     C***
114700980902IF  4C     CLPCLN        IFEQ      'A'
114800980902     C* SPEDIZIONI
114900980902     C                   ADD       SDCNSP        ESPA(M)
115000980902     C* PESO
115100980902     C                   ADD       SDCPKG        EPSA(M)
115200980902     C* COLLI
115300980902     C                   ADD       SDCNCO        ECLA(M)
115400980902     C* VOLUME
115500980902     C                   ADD       SDCVOL        EVLA(M)
115600980902     C* VOLUME DI CML
115700980902     C                   ADD       SDCVCC        EVCA(M)
115800980902     C* VOLUME CML
115900980902     C                   ADD       SDCVLS        EVSA(M)
116000020215     C* RICAVO REALE e PRESUNTO  "A"
116100980902IF  5C     CLPCLV        IFEQ      'A'
116200980902     C                   ADD       RICDEL        ERAA(M)
116300980902     C                   ADD       SDCIRP        EPAA(M)
116400980902E   5C                   ENDIF
116500980902     C* RICAVO REALE e PRESUNTO  "B"
116600980902IF  5C     CLPCLV        IFEQ      'B'
116700980902     C                   ADD       RICDEL        ERAB(M)
116800980902     C                   ADD       SDCIRP        EPAB(M)
116900980902E   5C                   ENDIF
117000160907     C* RICAVO REALE e PRESUNTO  "C" + "E" e "C"
117100980902IF  5C     CLPCLV        IFEQ      'C'
117200980902     C                   ADD       RICDEL        ERAC(M)
117300980902     C                   ADD       SDCIRP        EPAC(M)
117400160907     c                   add       ricdel        eracc(m)
117500160907     c                   add       sdcirp        epacc(m)
117600980902E   5C                   ENDIF
117700160907      * RICAVO REALE e PRESUNTO  "C" + "E" e "E"
117800160907IF  5c                   IF        CLPclv = 'E'
117900160907     c                   add       ricdel        erac(m)
118000160907     c                   add       sdcirp        epac(m)
118100160907     c                   add       ricdel        erae(m)
118200160907     c                   add       sdcirp        epae(m)
118300160907E   5c                   ENDIF
118400980902     C* RICAVO REALE e PRESUNTO  "T"
118500980902IF  5C     CLPCLV        IFEQ      'T'
118600980902     C                   ADD       RICDEL        ERAT(M)
118700980902     C                   ADD       SDCIRP        EPAT(M)
118800980902E   5C                   ENDIF
118900020215IF  5C     CLPCLV        IFEQ      'D'
119000020215     C                   ADD       RICDEL        ERAD(M)
119100020215     C                   ADD       SDCIRP        EPAD(M)
119200020215E   5C                   ENDIF
119300980902E   4C                   ENDIF
119400980902     C***
119500980902     C* NUOVI e ACQUISITI
119600980902     C***
119700980902     C* VOLUME DISTRIBUZIONE
119800980902IF  4C     SDCTSP        IFEQ      'D'
119900980902     C                   ADD       SDCVOL        EVLD(M)
120000980902E   4C                   ENDIF
120100980902E   3C                   ENDIF
120200980902E   2C                   ENDIF
120300980902E   1C                   ENDIF
120400980902     C*
120500980902     C                   ENDSR
120600980902     C*------------------------------------------------------------------------*
120700160304     C* SCRREP - SCRIVE UN RECORD PER IL COMMERCIALE UNIFICANTE                *
120800980902     C*------------------------------------------------------------------------*
120900980902     C     SCRREP        BEGSR
121000980902     C*
121100980902     C* IMPOSTA LA CHIAVE DI LETTURA
121200980902     C                   MOVEL     DEPDIV        KEPDIV                         *DIVISIONE
121300980902     C                   Z-ADD     DEPARE        KEPARE                         *AREA
121400980902     C                   Z-ADD     DEPFIL        KEPFIL                         *FILIALE
121500980902     C                   Z-ADD     DEPRGF        KEPRGF                         *COMMERCIALE
121600980902     C*
121700980902     C* PER CIASCUN MESE MEMORIZZATO, SCRIVE UN RECORD NEL REPORT
121800980902DO  1C     1             DO        12            M
121900980902IF  2C     MMM(M)        IFNE      *ZEROS
122000980902     C                   Z-ADD     AAA(M)        KEPANN                         *ANNO
122100980902     C                   Z-ADD     MMM(M)        KEPMES                         *MESE
122200980902     C                   CLEAR                   WFREP000                       *PULISCE RECORD
122300050408     C     KEYREP        CHAIN     WFREP11L                           97
122400980902IF  4C     *IN97         IFEQ      *OFF                                         *MODIFICA
122500980902     C                   EXSR      IMPREP                                       *IMPOSTA CAMPI
122600980902     C                   UPDATE    WFREP000                                     *AGGIORNA
122700980902X   4C                   ELSE                                                   *INSERIMENTO
122800980902     C                   MOVEL     KEPDIV        REPDIV                         *IMPOSTA CHIAVE
122900980902     C                   Z-ADD     KEPARE        REPARE
123000980902     C                   Z-ADD     KEPFIL        REPFIL
123100980902     C                   Z-ADD     KEPRGF        REPRGF
123200980902     C                   Z-ADD     KEPANN        REPANN
123300980902     C                   Z-ADD     KEPMES        REPMES
123400050607     C                   Z-ADD     depdip        REPdip
123500050607     C                   movel     depsoc        REPsoc
123600980902     C                   EXSR      IMPREP                                       *IMPOSTA CAMPI
123700980902     C                   WRITE     WFREP000                                     *SCRIVE
123800980902E   4C                   ENDIF
123900980902E   2C                   ENDIF
124000980902E   1C                   ENDDO                                                  *FINE PERIODO
124100980902     C*
124200980902     C                   ENDSR
124300980902     C*------------------------------------------------------------------------*
124400160304     C* IMPREP - IMPOSTA UN RECORD PER IL REPORT                               *
124500980902     C*------------------------------------------------------------------------*
124600980902     C     IMPREP        BEGSR
124700980902     C*
124800980902     C* CAMPI COMUNI
124900980902     C                   MOVEL     DEPDRG        REPDRG
125000980902     C                   Z-ADD     POT(M)        REPPOT
125100980902     C                   Z-ADD     PTR(M)        REPPTR
125200980902     C                   Z-ADD     MAI(M)        REPMAI
125300980902     C                   Z-ADD     VPT(M)        REPVPT
125400980902     C                   Z-ADD     VCL(M)        REPVCL
125500980902     C* ITALIA
125600980902     C                   Z-ADD     DEIPCF        REIPCF
125700980902     C                   Z-ADD     IOPT(M)       REIOPT
125800980902     C                   Z-ADD     IOCL(M)       REIOCL
125900980902     C                   Z-ADD     ITPT(M)       REITPT
126000980902     C                   Z-ADD     ITCL(M)       REITCL
126100980902     C                   Z-ADD     ISPN(M)       REISPN
126200980902     C                   Z-ADD     IPSN(M)       REIPSN
126300980902     C                   Z-ADD     ICLN(M)       REICLN
126400980902     C                   Z-ADD     IVLN(M)       REIVLN
126500980902     C                   Z-ADD     IVCN(M)       REIVCN
126600980902     C                   Z-ADD     IVSN(M)       REIVSN
126700980902     C                   Z-ADD     ISPA(M)       REISPA
126800980902     C                   Z-ADD     IPSA(M)       REIPSA
126900980902     C                   Z-ADD     ICLA(M)       REICLA
127000980902     C                   Z-ADD     IVLA(M)       REIVLA
127100980902     C                   Z-ADD     IVCA(M)       REIVCA
127200980902     C                   Z-ADD     IVSA(M)       REIVSA
127300980902     C                   Z-ADD     IRNA(M)       REIRNA
127400980902     C                   Z-ADD     IPNA(M)       REIPNA
127500980902     C                   Z-ADD     IRNB(M)       REIRNB
127600980902     C                   Z-ADD     IPNB(M)       REIPNB
127700980902     C                   Z-ADD     IRNC(M)       REIRNC
127800980902     C                   Z-ADD     IPNC(M)       REIPNC
127900980902     C                   Z-ADD     IRNT(M)       REIRNT
128000980902     C                   Z-ADD     IPNT(M)       REIPNT
128100020215     C                   Z-ADD     IRND(M)       REIRND
128200020215     C                   Z-ADD     IPND(M)       REIPND
128300980902     C                   Z-ADD     IRAA(M)       REIRAA
128400980902     C                   Z-ADD     IPAA(M)       REIPAA
128500980902     C                   Z-ADD     IRAB(M)       REIRAB
128600980902     C                   Z-ADD     IPAB(M)       REIPAB
128700980902     C                   Z-ADD     IRAC(M)       REIRAC
128800980902     C                   Z-ADD     IPAC(M)       REIPAC
128900980902     C                   Z-ADD     IRAT(M)       REIRAT
129000980902     C                   Z-ADD     IPAT(M)       REIPAT
129100020215     C                   Z-ADD     IRAD(M)       REIRAD
129200020215     C                   Z-ADD     IPAD(M)       REIPAD
129300980902     C                   Z-ADD     IVLD(M)       REIVLD
129400160907     c                   z-add     irncc(m)      REIRNCC
129500160907     c                   z-add     ipncc(m)      REIPNCC
129600160907     c                   z-add     irne(m)       REIRNE
129700160907     c                   z-add     ipne(m)       REIPNE
129800160907     c                   z-add     iracc(m)      REIRACC
129900160907     c                   z-add     ipacc(m)      REIPACC
130000160907     c                   z-add     irae(m)       REIRAE
130100160907     c                   z-add     ipae(m)       REIPAE
130200980902     C******
130300980902     C                   Z-ADD     *ZEROS        REISDN                         *CAMPI DA VALORIZZAR
130400980902     C                   Z-ADD     *ZEROS        REISAB
130500980902     C                   Z-ADD     *ZEROS        REICDB
130600980902     C                   Z-ADD     *ZEROS        REITSC                         *CAMPI SOSPESI
130700980902     C                   Z-ADD     *ZEROS        REITNF
130800980902     C******
130900020215     C* ESTERO
131000980902     C                   Z-ADD     DEEPCF        REEPCF
131100980902     C                   Z-ADD     EOPT(M)       REEOPT
131200980902     C                   Z-ADD     EOCL(M)       REEOCL
131300980902     C                   Z-ADD     ETPT(M)       REETPT
131400980902     C                   Z-ADD     ETCL(M)       REETCL
131500980902     C                   Z-ADD     ESPN(M)       REESPN
131600980902     C                   Z-ADD     EPSN(M)       REEPSN
131700980902     C                   Z-ADD     ECLN(M)       REECLN
131800980902     C                   Z-ADD     EVLN(M)       REEVLN
131900980902     C                   Z-ADD     EVCN(M)       REEVCN
132000980902     C                   Z-ADD     EVSN(M)       REEVSN
132100980902     C                   Z-ADD     ESPA(M)       REESPA
132200980902     C                   Z-ADD     EPSA(M)       REEPSA
132300980902     C                   Z-ADD     ECLA(M)       REECLA
132400980902     C                   Z-ADD     EVLA(M)       REEVLA
132500980902     C                   Z-ADD     EVCA(M)       REEVCA
132600980902     C                   Z-ADD     EVSA(M)       REEVSA
132700980902     C                   Z-ADD     ERNA(M)       REERNA
132800980902     C                   Z-ADD     EPNA(M)       REEPNA
132900980902     C                   Z-ADD     ERNB(M)       REERNB
133000980902     C                   Z-ADD     EPNB(M)       REEPNB
133100980902     C                   Z-ADD     ERNC(M)       REERNC
133200980902     C                   Z-ADD     EPNC(M)       REEPNC
133300980902     C                   Z-ADD     ERNT(M)       REERNT
133400980902     C                   Z-ADD     EPNT(M)       REEPNT
133500020215     C                   Z-ADD     ERND(M)       REERND
133600020215     C                   Z-ADD     EPND(M)       REEPND
133700980902     C                   Z-ADD     ERAA(M)       REERAA
133800980902     C                   Z-ADD     EPAA(M)       REEPAA
133900980902     C                   Z-ADD     ERAB(M)       REERAB
134000980902     C                   Z-ADD     EPAB(M)       REEPAB
134100980902     C                   Z-ADD     ERAC(M)       REERAC
134200980902     C                   Z-ADD     EPAC(M)       REEPAC
134300980902     C                   Z-ADD     ERAT(M)       REERAT
134400980902     C                   Z-ADD     EPAT(M)       REEPAT
134500020215     C                   Z-ADD     ERAD(M)       REERAD
134600020215     C                   Z-ADD     EPAD(M)       REEPAD
134700980902     C                   Z-ADD     EVLD(M)       REEVLD
134800160907     c                   z-add     erncc(m)      REERNCC
134900160907     c                   z-add     epncc(m)      REEPNCC
135000160907     c                   z-add     erne(m)       REERNE
135100160907     c                   z-add     epne(m)       REEPNE
135200160907     c                   z-add     eracc(m)      REERACC
135300160907     c                   z-add     epacc(m)      REEPACC
135400160907     c                   z-add     erae(m)       REERAE
135500160907     c                   z-add     epae(m)       REEPAE
135600980902     C******
135700980902     C                   Z-ADD     *ZEROS        REESDN                         *CAMPI DA VALORIZZAR
135800980902     C                   Z-ADD     *ZEROS        REESAB
135900980902     C                   Z-ADD     *ZEROS        REECDB
136000980902     C                   Z-ADD     *ZEROS        REETSC                         *CAMPI SOSPESI
136100980902     C                   Z-ADD     *ZEROS        REETNF
136200980902     C******
136300980902     C                   ENDSR
136400980902     C*------------------------------------------------------------------------*
136500160304     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI                                *
136600980902     C*------------------------------------------------------------------------*
136700980902     C     CARTAB        BEGSR
136800020402     C*---
136900980902     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
137000980902     C*---
137100980903     C                   Z-ADD     *ZEROS        I                 4 0
137200020906     C                   Z-ADD     1             KTBKUT
137300980902     C                   MOVEL     '2W'          KTBCOD
137400980902     C     KE2TAB        SETLL     TABEL00F
137500980902     C     KE2TAB        READE     TABEL00F                               99
137600980902DO  1C     *IN99         DOWEQ     *OFF
137700980902IF  2C     TBLFLG        IFEQ      *BLANKS
137800980902     C                   ADD       1             I
137900980902     C                   MOVEL     TBLUNI        DS2W
138000980902     C                   MOVEL     TBLKEY        FRBL(I)                        *RAGGRUPP.TIPO BOLLA
138100980902     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONI
138200980902     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVI
138300980902E   2C                   ENDIF
138400980902     C     KE2TAB        READE     TABEL00F                               99
138500980902E   1C                   ENDDO
138600020329     C*-------------------------
138700020329     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
138800020329     C*-------------------------
138900020329     C                   Z-ADD     *ZEROS        R
139000020906     C                   Z-ADD     1             KTBKUT
139100020329     C                   MOVEL     '2M'          KTBCOD
139200020329     C     KE2TAB        SETLL     TABEL00F
139300020329     C     KE2TAB        READE     TABEL00F                               99
139400020329DO  1C     *IN99         DOWEQ     *OFF
139500020329IF  2C     TBLFLG        IFEQ      *BLANKS
139600020329     C                   ADD       1             R
139700020329     C                   MOVEL     TBLKEY        RGREG(R)
139800020329     C                   MOVEL     TBLUNI        DS2M
139900020329     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
140000020329E   2C                   ENDIF
140100020329     C     KE2TAB        READE     TABEL00F                               99
140200020329E   1C                   ENDDO
140300020402     C*--------------------
140400020402     C* FASCE DI PESO
140500020402     C*--------------------
140600020402IF  1C     PARAMF        IFLE      199812                                       *FASCE FINO AL '98
140700020402     C                   MOVEL     'SDC'         WSX               3
140800020402X   1C                   ELSE                                                   *FASCE DAL '99
140900020402IF  2C     PARAMF        IFLE      200112                                       *FASCE DAL '99 FINO
141000020402     C                   MOVEL     'S9C'         WSX
141100020402X   2C                   ELSE                                                   *FASCE DAL 2002
141200020402     C                   MOVEL     'S2C'         WSX
141300020402E   2C                   ENDIF
141400020402E   1C                   ENDIF
141500020402     C                   Z-ADD     *ZEROS        P
141600020906     C                   Z-ADD     1             KTBKUT
141700020402     C                   MOVEL     '2L'          KTBCOD
141800020402     C     KE2TAB        SETLL     TABEL00F
141900020402     C     KE2TAB        READE     TABEL00F                               99
142000020402DO  1C     *IN99         DOWEQ     *OFF
142100020402IF  2C     TBLFLG        IFEQ      *BLANKS
142200020402     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
142300020402     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
142400020402IF  3C     FILEST        IFEQ      WSX
142500020402     C                   MOVEL     TBLUNI        DS2L
142600020402IF  4C     §2LKGI        IFGE      5000
142700020402     C     PARAMI        ANDLT     200001
142800020402     C                   ELSE
142900020402     C                   ADD       1             P
143000020402     C                   MOVE      STACOD        FPE(P)                         *...+XX
143100020402E   4C                   ENDIF
143200020402E   3C                   ENDIF
143300020402E   2C                   ENDIF
143400020402     C     KE2TAB        READE     TABEL00F                               99
143500020402E   1C                   ENDDO
143600160304      /free
143700160304
143800160304         // -?Intabellamento Filiali e relativi Aree / Distretti?
143900160304         clear  xF;
144000160304         setll  ( *loval )  AZORG;
144100160304         read  AZORG;
144200160304         DoW  Not %eof(AZORG01L);
144300160304           if  ORGfva = *blank  and  ORGfag <> 'V';
144400160304             xF += 1;
144500160304             sk_Fil(xF) = ORGfil;
144600160304             sk_Are(xF) = ORGcar;
144700160304             sk_Dis(xF) = ORGfl3;
144800160304           endif;
144900160304           read  AZORG;
145000160304         EndDo;
145100160304
145200160304      /end-free
145300980902     C*
145400980902     C                   ENDSR
145500980902     C*------------------------------------------------------------------------*
145600160304     C* CARMES - CARICA I MESI/ANNI DA ELEBORARE                               *
145700980902     C*------------------------------------------------------------------------*
145800980902     C     CARMES        BEGSR
145900980902     C*
146000980902     C* REIMPOSTA LE VARIABILI DI LAVORO
146100980902     C                   CLEAR                   AAA                            *ANNI
146200980902     C                   CLEAR                   MMM                            *MESI
146300980902     C                   Z-ADD     *ZEROS        J                 4 0          *N° MESI DA ELABORAR
146400980902     C                   Z-ADD     PARAAF        WAA4              4 0          *ANNO FINALE LAVORO
146500980902     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
146600980902     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
146700980902     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
146800980902     C*
146900980902     C* ANNO INIZIALE UGUALE AD ANNO FINALE
147000980902IF  1C     WAA4          IFEQ      WAA
147100980902     C*
147200980902     C* CALCOLO I MESI DI DIFFERENZA
147300980902DO  2C     WMM           DOWLE     WMM2
147400980902     C                   ADD       1             J
147500980902     C                   Z-ADD     WAA           AAA(J)
147600980902     C                   Z-ADD     WMM           MMM(J)
147700980902     C                   ADD       1             WMM
147800980902E   2C                   ENDDO
147900980902     C*
148000980902     C* IL RESTO DELLA SCHIERA A ZERO
148100980902DO  2C     J             DOWLT     12
148200980902     C                   ADD       1             J
148300980902     C                   Z-ADD     *ZEROS        AAA(J)
148400980902     C                   Z-ADD     *ZEROS        MMM(J)
148500980902E   2C                   ENDDO
148600980902     C*
148700980902     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
148800980902X   1C                   ELSE
148900980902     C*
149000980902     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
149100980902DO  2C     WMM           DOWLE     12
149200980902     C                   ADD       1             J
149300980902     C                   Z-ADD     WAA           AAA(J)
149400980902     C                   Z-ADD     WMM           MMM(J)
149500980902     C                   ADD       1             WMM
149600980902E   2C                   ENDDO
149700980902     C*
149800980902     C* MEMORIZZO I MESI DELL'ANNO FINALE
149900980902     C                   Z-ADD     01            WMM
150000980902DO  2C     WMM           DOWLE     WMM2
150100980902     C                   ADD       1             J
150200980902     C                   Z-ADD     WAA4          AAA(J)
150300980902     C                   Z-ADD     WMM           MMM(J)
150400980902     C                   ADD       1             WMM
150500980902E   2C                   ENDDO
150600980902     C*
150700980902     C* IL RESTO DELLA SCHIERA A ZERO
150800980902DO  2C     J             DOWLT     12
150900980902     C                   ADD       1             J
151000980902     C                   Z-ADD     *ZEROS        AAA(J)
151100980902     C                   Z-ADD     *ZEROS        MMM(J)
151200980902E   2C                   ENDDO
151300980902E   1C                   ENDIF
151400980902     C*
151500980902     C                   ENDSR
151600980902     C*------------------------------------------------------------------------*
151700160304     C* CANREP - CANCELLA IL WORKFILE                                          *
151800980902     C*------------------------------------------------------------------------*
151900980903     C     CANREP        BEGSR
152000980902     C*
152100980902     C* CANCELLA IL WFILE PER IL PERIODO RICHIESTO
152200050408     C     *LOVAL        SETLL     WFREP11L
152300050408     C                   READ      WFREP11L                               99
152400980902DO  1C     *IN99         DOWEQ     *OFF
152500980902IF  2C     REPDAT        IFGE      PARAMI
152600980902     C     REPDAT        ANDLE     PARAMF
152700980902     C                   DELETE    WFREP000
152800980902E   2C                   ENDIF
152900050408     C                   READ      WFREP11L                               99
153000980902E   1C                   ENDDO
153100980902     C*
153200980902     C                   ENDSR
153300980902     C*------------------------------------------------------------------------*
153400160304     C* *INZSR - OPERAZIONI INIZIALI                                           *
153500980902     C*------------------------------------------------------------------------*
153600980902     C     *INZSR        BEGSR
153700980902     C*
153800980902     C* RICEVIMENTO PARAMETRI
153900980902     C     *ENTRY        PLIST
154000980902     C                   PARM                    KPJBA
154100980902     C                   MOVEL     KPJBU         PARAM
154200160304      *
154300160304      * -?Reperimento dati job?
154400160304     c                   exsr      sr_DatiJob
154500020328     C*
154600020402     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
154700020328     C                   MOVEL     PARAMF        PARAMGF           8 0
154800020402     C                   MOVE      '28'          PARAMGF
154900081119     C*
155000081119     C*------- GIORNO    di    F I N E    M E S E ---------------------------*
155100081119     C*
155200081119     C*  Ultimo giorno del mese
155300081119     C                   MOVE      PARAMGF       MMGG_4            4 0
155400081119     C                   MOVEL     MMGG_4        Mm_2              2 0
155500081119     C                   MOVEl     PARAMGF       Anno_4            4 0
155600081119      * se è febbraio
155700081119     c                   If        Mm_2 = 2
155800081119      *
155900081119      *  potrebbe essere bisestile 29 gg.
156000081119     c     Anno_4        div       4             risulta           3 0
156100081119     c                   mvr                     resto             3 0
156200081119      *  Se bisestile
156300081119     c                   If        resto = 0
156400081119     C                   MOVE      '29'          PARAMGF
156500081119     c                   end
156600081119      *
156700081119     c                   else
156800081119      *
156900081119      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
157000081119     c                   If           Mm_2 = 4 or Mm_2 = 6 or
157100081119     c                                Mm_2 = 9 or Mm_2 = 11
157200081119     C                   MOVE      '30'          PARAMGF
157300081119     c                   else
157400081119     C                   MOVE      '31'          PARAMGF
157500081119     c                   end
157600081119      *
157700081119     c                   endIf
157800081119     C*
157900081119     C*----------------------- F I N E    M E S E ---------------------------*
158000081119     C*
158100980902     C*
158200980902     C* VARIABILI RIFERITE AL DATABASE
158300980902     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
158400980902     C     *LIKE         DEFINE    TBLCOD        KTBCOD
158500980902     C     *LIKE         DEFINE    TBLKEY        KTBKEY
158600980902     C     *LIKE         DEFINE    SACCMM        KSACMM                         *TFSAC00F
158700980902     C     *LIKE         DEFINE    SACAAA        KSAAAA
158800980902     C     *LIKE         DEFINE    SACMMM        KSAMMM
158900980902     C     *LIKE         DEFINE    CLPKUT        KLPKUT                         *CNCLP00F
159000980902     C     *LIKE         DEFINE    CLPKCC        KLPKCC
159100020328     C     *LIKE         DEFINE    CLPKSC        KLPKSC
159200980902     C     *LIKE         DEFINE    CLPAGE        KLPAGE
159300990610     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC00F
159400980902     C     *LIKE         DEFINE    SDCANN        KSCANN
159500980902     C     *LIKE         DEFINE    SDCMES        KSCMES
159600050408     C     *LIKE         DEFINE    REPDIV        KEPDIV                         *WFREP10F
159700980902     C     *LIKE         DEFINE    REPARE        KEPARE
159800980902     C     *LIKE         DEFINE    REPFIL        KEPFIL
159900980902     C     *LIKE         DEFINE    REPRGF        KEPRGF
160000050607     C     *LIKE         DEFINE    REPdip        depdip
160100050607     C     *LIKE         DEFINE    REPsoc        depsoc
160200980902     C     *LIKE         DEFINE    REPANN        KEPANN
160300980902     C     *LIKE         DEFINE    REPMES        KEPMES
160400980902     C     *LIKE         DEFINE    REPRGF        DEPRGF                         *DI DEPOSITO
160500980902     C     *LIKE         DEFINE    REPDIV        DEPDIV
160600980902     C     *LIKE         DEFINE    REPARE        DEPARE
160700980902     C     *LIKE         DEFINE    REPFIL        DEPFIL
160800980902     C     *LIKE         DEFINE    REPDRG        DEPDRG
160900980902     C     *LIKE         DEFINE    REIPCF        DEIPCF
161000980902     C     *LIKE         DEFINE    REEPCF        DEEPCF
161100980902     C*
161200980902     C* CHAIVI DI LETTURA
161300980902     C     KEYTAB        KLIST                                                  *TABEL00F
161400980902     C                   KFLD                    KTBKUT                          -UTENTE
161500980902     C                   KFLD                    KTBCOD                          -TABELLA
161600980902     C                   KFLD                    KTBKEY                          -CHIAVE
161700980902     C     KE2TAB        KLIST                                                  *TABEL00F
161800980902     C                   KFLD                    KTBKUT                          -UTENTE
161900980902     C                   KFLD                    KTBCOD                          -TABELLA
162000980902     C     KEYSAC        KLIST                                                  *TFSAC00F
162100980902     C                   KFLD                    KSACMM                          -COMMERCIALE
162200980902     C                   KFLD                    KSAAAA                          -ANNO
162300980902     C                   KFLD                    KSAMMM                          -MESE
162400980902     C     KE2SAC        KLIST                                                  *TFSAC00F
162500980902     C                   KFLD                    KSACMM                          -COMMERCIALE
162600020516     C     KEYCLP        KLIST                                                  *CNCLP01L
162700980902     C                   KFLD                    KLPKUT                          -UTENTE
162800980902     C                   KFLD                    KLPKCC                          -CAPOCONTO
162900020328     C                   KFLD                    KLPAGE                          -AGENTE
163000020516     C     KEYCLP2       KLIST                                                  *CNCLP00F
163100020328     C                   KFLD                    KLPKUT                          -UTENTE
163200020328     C                   KFLD                    KLPKCC                          -CAPOCONTO
163300020328     C                   KFLD                    KLPKSC                          -CLIENTE
163400990610     C     KEYSDC        KLIST                                                  *SISDC00F
163500980902     C                   KFLD                    KSCKSC                          -CODICE CLIENTE
163600980902     C                   KFLD                    KSCANN                          -ANNO
163700980902     C                   KFLD                    KSCMES                          -MESE
163800990610     C     KE2SDC        KLIST                                                  *SISDC00F
163900980902     C                   KFLD                    KSCKSC                          -CODICE CLIENTE
164000050408     C     KEYREP        KLIST                                                  *WFREP10F
164100980902     C                   KFLD                    KEPDIV                          -DIVISIONE
164200980902     C                   KFLD                    KEPARE                          -AREA
164300980902     C                   KFLD                    KEPFIL                          -FILIALE
164400980902     C                   KFLD                    KEPRGF                          -AGENTE
164500980902     C                   KFLD                    KEPANN                          -ANNO
164600980902     C                   KFLD                    KEPMES                          -MESE
164700980902     C*
164800980902     C* VARIABILI DI CONTROLLO
164900980902     C                   MOVEL     'N'           $FINE             1            *FINE PGM
165000980902     C                   MOVEL     'N'           $FSAC             1            *FINE X TFSAC
165100980902     C                   MOVEL     'N'           $FCLP             1            *FINE X CNCLP
165200990610     C                   MOVEL     'N'           $FSDC             1            *FINE X SISDC
165300980902     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
165400980902     C                   MOVEL     '1'           *IN01                          *OVERFLOW
165500020402     C                   Z-ADD     *ZEROS        R                 3 0          *PER RAGGRUPP. REG.
165600020402     C                   Z-ADD     *ZEROS        P                 4 0          *FASCE DI PERO
165700980902     C*
165800980902     C* CARICAMENTO TABELLE OCCORRENTI
165900980902     C                   EXSR      CARTAB
166000980902     C*
166100980902     C* CARICAMENTO MESI/ANNI DA ELABORARE
166200980902     C                   EXSR      CARMES
166300980902     C*
166400980902     C* CANELLA IL WFILE PER IL PERIODO RICHIESTO
166500980902     C                   EXSR      CANREP
166600980902     C*
166700980902     C                   ENDSR
166800160304      /free
166900160304       //--------------------------------------------------------------
167000160304       //?Reperimento Dati del job (Utente/Operativi).                 ?
167100160304       //--------------------------------------------------------------
167200160304       BEGSR  sr_DatiJob;
167300160304
167400160304         in(E)  §AzUte;
167500160304         if  NOT  %error;
167600160304           in(E)  §DatiUte;
167700160304         endif;
167800160304         if  %error  or  RSut = *blanks;
167900160304           clear  TIBS34ds;
168000160304           tibs34r ( tibs34ds );
168100160304           in  §AzUte;
168200160304           in  §DatiUte;
168300160304         endif;
168400160304
168500160304       ENDSR;
168600160304
168700160304       //--------------------------------------------------------------
168800160304       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
168900160304       //--------------------------------------------------------------
169000160304       BEGSR  sr_PrintErr;
169100160304
169200160304         // -?Stampa del Dump?
169300160304         Dump(A);
169400160304
169500160304         // -?Stampa del Job-Log?
169600160304         Qcmd = 'DSPJOBLOG job(*) output(*print)';
169700160304         exsr  sr_ExecCmd;
169800160304
169900160304         //// -?Chiusura del programma?
170000160304         ////*inH1 = *on;
170100160304         ////$Fine = 'S';
170200160304
170300160304       ENDSR;
170400160304
170500160304       //--------------------------------------------------------------
170600160304       //?Esecuzione del comando (già impostato).                      ?
170700160304       //--------------------------------------------------------------
170800160304       BEGSR  sr_ExecCmd;
170900160304
171000160304         clear Qcap0100;
171100160304         Qcabcsdh = *off;
171200160304         Qcapa    = *off;
171300160304         Qcacmdss = *off;
171400160304         Qcaerved = *allX'00';
171500160304
171600160304         clear Qusec;
171700160304         Qusbprv  = %size(Qusec);
171800160304
171900160304         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
172000160304                           %size(Qcap0100) : 'CPOP0100' : *omit :
172100160304                           0 : 0 : Qusec);
172200160304
172300160304         //if  Qusei <> *blank;
172400160304         //  exsr  sr_PrintErr;
172500160304         //endif;
172600160304
172700160304       ENDSR;
172800160304
172900160304      /end-free
