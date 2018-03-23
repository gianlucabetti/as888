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
010400980902     D IRNC            S             15  3 DIM(12)                              *RICAVO REALE    C N
010500980902     D IPNC            S             15  3 DIM(12)                              *RICAVO PRESUNTO C N
010600980902     D IRNT            S             15  3 DIM(12)                              *RICAVO REALE    T N
010700980902     D IPNT            S             15  3 DIM(12)                              *RICAVO PRESUNTO T N
010800020215     D IRND            S             15  3 DIM(12)                              *RICAVO REALE    D N
010900020215     D IPND            S             15  3 DIM(12)                              *RICAVO PRESUNTO D N
011000980902     D IRAA            S             15  3 DIM(12)                              *RICAVO REALE    A A
011100980902     D IPAA            S             15  3 DIM(12)                              *RICAVO PRESUNTO A A
011200980902     D IRAB            S             15  3 DIM(12)                              *RICAVO REALE    B A
011300980902     D IPAB            S             15  3 DIM(12)                              *RICAVO PRESUNTO B A
011400980902     D IRAC            S             15  3 DIM(12)                              *RICAVO REALE    C A
011500980902     D IPAC            S             15  3 DIM(12)                              *RICAVO PRESUNTO C A
011600980902     D IRAT            S             15  3 DIM(12)                              *RICAVO REALE    T A
011700980902     D IPAT            S             15  3 DIM(12)                              *RICAVO PRESUNTO T A
011800020215     D IRAD            S             15  3 DIM(12)                              *RICAVO REALE    D A
011900020215     D IPAD            S             15  3 DIM(12)                              *RICAVO PRESUNTO D A
012000980902     D IVLD            S             11  3 DIM(12)                              *VOLUME DISTRIBUZION
012100980902     D ERNA            S             15  3 DIM(12)                              *RICAVO REALE    A N
012200980902     D EPNA            S             15  3 DIM(12)                              *RICAVO PRESUNTO A N
012300980902     D ERNB            S             15  3 DIM(12)                              *RICAVO REALE    B N
012400980902     D EPNB            S             15  3 DIM(12)                              *RICAVO PRESUNTO B N
012500980902     D ERNC            S             15  3 DIM(12)                              *RICAVO REALE    C N
012600980902     D EPNC            S             15  3 DIM(12)                              *RICAVO PRESUNTO C N
012700980902     D ERNT            S             15  3 DIM(12)                              *RICAVO REALE    T N
012800980902     D EPNT            S             15  3 DIM(12)                              *RICAVO PRESUNTO T N
012900020215     D ERND            S             15  3 DIM(12)                              *RICAVO REALE    D N
013000020215     D EPND            S             15  3 DIM(12)                              *RICAVO PRESUNTO D N
013100980902     D ERAA            S             15  3 DIM(12)                              *RICAVO REALE    A A
013200980902     D EPAA            S             15  3 DIM(12)                              *RICAVO PRESUNTO A A
013300980902     D ERAB            S             15  3 DIM(12)                              *RICAVO REALE    B A
013400980902     D EPAB            S             15  3 DIM(12)                              *RICAVO PRESUNTO B A
013500980902     D ERAC            S             15  3 DIM(12)                              *RICAVO REALE    C A
013600980902     D EPAC            S             15  3 DIM(12)                              *RICAVO PRESUNTO C A
013700980902     D ERAT            S             15  3 DIM(12)                              *RICAVO REALE    T A
013800980902     D EPAT            S             15  3 DIM(12)                              *RICAVO PRESUNTO T A
013900020215     D ERAD            S             15  3 DIM(12)                              *RICAVO REALE    D A
014000020215     D EPAD            S             15  3 DIM(12)                              *RICAVO PRESUNTO D A
014100980902     D EVLD            S             11  3 DIM(12)                              *VOLUME DISTRIBUZION
014200160304      *---
014300160304      * SCHIERA DI RIDEFINIZIONE ELENCO CLIENTI FIGLI
014400160304      *---
014500020328     D SKC             S             11    DIM(500)
014600160304      *---
014700160304      * SCHIERA DI MEMORIZZAZIONE ELNCO CLIENTI X AGENTE UNIFICANTE
014800160304      *---
014900020328     D SKCAGEU         S             11    DIM(5000)
015000160304      *------------------
015100160304      * RAGGRUPPAMENTI REGIONI/NETWORK
015200160304      *------------------
015300020329     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIO
015400020329     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RA
015500160304      *------------------
015600160304      * FASCE DI PESO
015700160304      *------------------
015800020402     D FPE             S              2    DIM(10)
015900160304      *------------------------------------------------------------------------*
016000160304      * STRUTTURE DATI                                                         *
016100160304      *------------------------------------------------------------------------*
016200160304      *---
016300160304      * DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
016400160304      *---
016500980902     D DS2W          E DS
016600160304      *---
016700160304      * DS COMPOSIZIONE DATA(6) -TFSAC-
016800160304      *---
016900981007     D                 DS                  INZ
017000980902     D  SACAAA                 1      4  0
017100980902     D  SACMMM                 5      6  0
017200980902     D  SACDAT                 1      6  0
017300160304      *---
017400160304      * DS SCOMPOSIZIONE DATA PRIMA SPEDIZIONE FATTURA(4) -CNCLP-
017500160304      *---
017600981007     D                 DS                  INZ
017700020409     D  CLPSEC                 1      2  0
017800020409     D  CLPANN                 3      4  0
017900020409     D  CLPMES                 5      6  0
018000020409     D  CLPDAT                 1      6  0
018100020409     D  DSDPS                  1      8  0
018200160304      *---
018300160304      * DS COMPOSIZIONE DATA SALDO(4) -SISDC-
018400160304      *---
018500981007     D                 DS                  INZ
018600990610     D  SDCANN                 1      4  0
018700990610     D  SDCMES                 5      6  0
018800990610     D  SDCDAT                 1      6  0
018900160304      *---
019000160304      * DS COMPOSIZIONE DATA(6) -WFREP-
019100160304      *---
019200981007     D                 DS                  INZ
019300980902     D  REPANN                 1      4  0
019400980902     D  REPMES                 5      6  0
019500980902     D  REPDAT                 1      6  0
019600160304      *---
019700160304      * DS PARAMETRI INPUT
019800160304      *---
019900980902     D PARAM           DS
020000980902     D  PARAMI                 1      6  0
020100980902     D  PARAAI                 1      4  0
020200980902     D  PARMMI                 5      6  0
020300980902     D  PARDAI                 3      6  0
020400980902     D  PARAMF                 7     12  0
020500980902     D  PARAAF                 7     10  0
020600980902     D  PARA2F                 9     10  0
020700980902     D  PARMMF                11     12  0
020800980902     D  PARDAF                 9     12  0
020900160304      *---
021000160304      * ARCHITETTURA
021100160304      *---
021200980902     D KPJBA         E DS
021300160304      *---
021400160304      * DS X REPERIMENTO/TEST CLIENTI PADRI/FIGLI
021500160304      *---
021600020328     D TIBS10        E DS                  EXTNAME(TIBS10DS)
021700160304      *------------------
021800160304      * DS DI LETTURA TABELLA "2M"
021900160304      *------------------
022000020329     D DS2M          E DS
022100160304      *------------------
022200160304      * DS DI LETTURA TABELLA "2L"
022300160304      *------------------
022400020402     D DS2L          E DS
022500160304      *
022600160304      * -?Anagrafica Commerciali?
022700160304     d AZCMM_ds      e ds                  extname(AZCMM00F)  inz
022800160304
022900160304       //--------------------------------------------------------------
023000160304       //?Variabili.                                                   ?
023100160304       //--------------------------------------------------------------
023200160304
023300160304       // -?Indici di schiera?
023400160304     d xF              s              3  0 inz
023500160304
023600160304       //--------------------------------------------------------------
023700160304       //?Definizione aree dati.                                       ?
023800160304       //--------------------------------------------------------------
023900160304
024000160304       // -?Dati utente?
024100160304     d §AzUte        e ds                  extname(AZUTE00F)
024200160304     d                                     dtaara
024300160304     d §DatiUte      e ds                  extname(dDatiUte)
024400160304     d                                     dtaara
024500160304
024600160304       //--------------------------------------------------------------
024700160304       //?Definizione prototipi procedure.                             ?
024800160304       //--------------------------------------------------------------
024900160304
025000160304       // -?Reperimento dati utente?
025100160304     d TIBS34ds      e ds
025200160304      /copy gaitrasrc/srcProtoPR,TIBS34R
025300160304
025400160304       // -?Parametri API QCAPCMD (Process Commands)?
025500160304     d Qcmd            s           2048    inz  varying
025600160304      /copy qSysInc/qRpgleSrc,QCAPCMD
025700160304       // -?API QCAPCMD (Process Commands)?
025800160304      /copy gaitrasrc/srcProtoPR,QCAPCMD
025900160304
026000160304       // -?Parametri gestione errori API.?
026100160304      /copy qsysinc/qrpglesrc,QUSEC
026200160304
026300160304      *------------------------------------------------------------------------*
026400160304      * MAIN LINE
026500160304      *------------------------------------------------------------------------*
026600980902     C*
026700160304     C* POSIZIONAMENTO E PRIMA LETTURA COMMERCIALI
026800980902     C                   EXSR      SETFIL
026900980902     C*
027000980902     C* CICLO FINO A FINE FILE
027100980902DO  1C     $FINE         DOWEQ     'N'
027200980902     C*
027300980902     C* OPERAZIONI PER NUOVO COMMERCIALE UNIFICANTE
027400980902     C                   EXSR      NEWUNI
027500980902     C*
027600980902     C* CICLO FINO A ROTTURA DI COMMERCIALE UNIFICANTE
027700980902DO  2C     $FINE         DOWEQ     'N'
027800160304     C     CMMuni        ANDEQ     DEPRGF
027900160304      *
028000160307if  3c                   If        xF > *zero
028100980902     C*
028200980902     C* ELABORA I SALDI ATTIVITA' COMMERCIALE
028300980902     C                   EXSR      ELASAC
028400020402     C*
028500020402     C* ELABORA I SALDI CLIENTE X AGENTE UNIFICANTE
028600020402     C                   EXSR      ELACLP
028700160307      *
028800160307e   3c                   EndIf
028900980902     C*
029000160304     C* LETTURA SUCCESSIVA COMMERCIALI
029100980902     C                   EXSR      LETFIL
029200160307      *
029300980902E   2C                   ENDDO                                                  *ROTTURA UNIFICANTE
029400980902     C*
029500980902     C* SCRIVE UN RECORD PER IL COMMERCIALE UNIFICANTE
029600160307if  2c                   If        xF > *zero
029700980902     C                   EXSR      SCRREP
029800160307e   2c                   EndIf
029900160307      *
030000980902E   1C                   ENDDO                                                  *FINE FILE
030100980902     C*
030200980902     C* OPERAZIONI FINALI
030300980902     C                   SETON                                        LR
030400980902     C*------------------------------------------------------------------------*
030500160304     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                  -COMMERCIALI- *
030600980902     C*------------------------------------------------------------------------*
030700980902     C     SETFIL        BEGSR
030800160304      /free
030900160304
031000160304         exec sql   set option   dynusrprf = *owner,
031100160304                                 closqlcsr = *endmod;
031200160304
031300160304         // -?Dichiarazione cursore per leggere i commerciali per Unificante?
031400160304         exec sql   declare A1   cursor for
031500160304                     select *
031600160304                       from AZCMM00F
031700160304                      where CMMatb = ' '
031800160304                   order by CMMuni, CMMcod;
031900160304
032000160304         // -?Apertura cursore?
032100160304         exec sql   open A1;
032200160304
032300160304         // -?Lettura cursore?
032400160304         exsr  LetFIL;
032500160304
032600160304      /end-free
032700160304      *
032800980902     C                   ENDSR
032900160304      *------------------------------------------------------------------------*
033000160304      * LETFIL - LETTURA PROSSIMO RECORD                         -COMMERCIALI- *
033100160304      *------------------------------------------------------------------------*
033200980902     C     LETFIL        BEGSR
033300160304      *
033400160304      /free
033500160304
033600160304         exec SQL   fetch next  from A1  into :AZCMM_ds;
033700160304
033800160304         Select;
033900160304           // -?E.o.F.?
034000160304           When  SqlCode = 100;
034100160304             $Fine = 'S';
034200160304           // -?Errore?
034300160304           When  SqlCode < *zero;
034400160304             exsr  sr_PrintErr;
034500160304             $Fine = 'S';
034600160304         EndSl;
034700160304
034800160304         // -?Chiusura cursore?
034900160304         if  $Fine = 'S';
035000160304           exec sql   close A1;
035100160304         endif;
035200160304
035300160304      /end-free
035400160304      *
035500980902     C                   ENDSR
035600980902     C*------------------------------------------------------------------------*
035700160304     C* NEWUNI - OPERAZIONI PER NUOVO COMMERCIALE UNIFICANTE                   *
035800980902     C*------------------------------------------------------------------------*
035900980902     C     NEWUNI        BEGSR
036000980902     C*
036100160304     C* MEMORIZZA IL NUOVO COMMERCIALE UNIFICANTE
036200160304     C                   Z-ADD     cmmUNI        DEPRGF                         *DEPOSITO
036300020328     C*
036400020328     C* PULISCO LA SCHIERA DEL SUO ELENCO CLIENTI
036500020328     C                   CLEAR                   SKCAGEU
036600020328     C                   Z-ADD     *ZEROS        Y                 4 0
036700980902     C*
036800160304     C* MEMORIZZA ALTRI DATI DEL COMMERCIALE UNIFICANTE
036900160304     C                   MOVEL     CMMuni        DEPFIL                         *FILIALE
037000160308      /free
037100160308
037200160308         // -?Reperimento Distretto & Area del commerciale unificante?
037300160308         xF   = %lookup( DEPfil : sk_Fil );
037400160308
037500160308         if  xF = *zero;
037600160308           leavesr;
037700160308         endif;
037800160308
037900160308         chain  ( CMMuni )  AZCMM000;
038000160308
038100160308         if  NOT %found(AZCMM01L);
038200160308           clear  xF;
038300160308           leavesr;
038400160308         endif;
038500160308
038600160308      /end-free
038700160308     c                   movel     sk_Dis(xF)    DEPDIV                         *DISTRETTO
038800160308     c                   z-add     sk_Are(xF)    DEPARE                         *AREA
038900160308     c                   movel     XCUdes        DEPDRG                         *DESCRIZIONE
039000160308     c                   z-add     XCUpli        DEIPCF                         *% LAVORO ITALIA
039100160308     c                   z-add     XCUple        DEEPCF                         *% LAVORO ESTERO
039200160308     c                   z-add     XCUdip        DEpdip                         *CODICE  DIPENDENTE
039300160308     c                   movel     XCUsoc        DEpsoc                         *SOCIETÀ DIPENDENTE
039400980902     C*
039500020215     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
039600981111     C                   CLEAR                   $CTR
039700980903     C                   CLEAR                   POT
039800980903     C                   CLEAR                   PTR
039900980903     C                   CLEAR                   MAI
040000980903     C                   CLEAR                   VPT
040100980903     C                   CLEAR                   VCL
040200980903     C                   CLEAR                   IOPT
040300980903     C                   CLEAR                   IOCL
040400980903     C                   CLEAR                   ITPT
040500980903     C                   CLEAR                   ITCL
040600980903     C                   CLEAR                   EOPT
040700980903     C                   CLEAR                   EOCL
040800980903     C                   CLEAR                   ETPT
040900980903     C                   CLEAR                   ETCL
041000980903     C                   CLEAR                   ISPN
041100980903     C                   CLEAR                   IPSN
041200980903     C                   CLEAR                   ICLN
041300980903     C                   CLEAR                   IVLN
041400980903     C                   CLEAR                   IVCN
041500980903     C                   CLEAR                   IVSN
041600980903     C                   CLEAR                   ISPA
041700980903     C                   CLEAR                   IPSA
041800980903     C                   CLEAR                   ICLA
041900980903     C                   CLEAR                   IVLA
042000980903     C                   CLEAR                   IVCA
042100980903     C                   CLEAR                   IVSA
042200980903     C                   CLEAR                   ESPN
042300980903     C                   CLEAR                   EPSN
042400980903     C                   CLEAR                   ECLN
042500980903     C                   CLEAR                   EVLN
042600980903     C                   CLEAR                   EVCN
042700980903     C                   CLEAR                   EVSN
042800980903     C                   CLEAR                   ESPA
042900980903     C                   CLEAR                   EPSA
043000980903     C                   CLEAR                   ECLA
043100980903     C                   CLEAR                   EVLA
043200980903     C                   CLEAR                   EVCA
043300980903     C                   CLEAR                   EVSA
043400980903     C                   CLEAR                   IRNA
043500980903     C                   CLEAR                   IPNA
043600980903     C                   CLEAR                   IRNB
043700980903     C                   CLEAR                   IPNB
043800980903     C                   CLEAR                   IRNC
043900980903     C                   CLEAR                   IPNC
044000980903     C                   CLEAR                   IRNT
044100980903     C                   CLEAR                   IPNT
044200020215     C                   CLEAR                   IRND
044300020215     C                   CLEAR                   IPND
044400980903     C                   CLEAR                   IRAA
044500980903     C                   CLEAR                   IPAA
044600980903     C                   CLEAR                   IRAB
044700980903     C                   CLEAR                   IPAB
044800980903     C                   CLEAR                   IRAC
044900980903     C                   CLEAR                   IPAC
045000980903     C                   CLEAR                   IRAT
045100980903     C                   CLEAR                   IPAT
045200020215     C                   CLEAR                   IRAD
045300020215     C                   CLEAR                   IPAD
045400980903     C                   CLEAR                   IVLD
045500980903     C                   CLEAR                   ERNA
045600980903     C                   CLEAR                   EPNA
045700980903     C                   CLEAR                   ERNB
045800980903     C                   CLEAR                   EPNB
045900980903     C                   CLEAR                   ERNC
046000980903     C                   CLEAR                   EPNC
046100980903     C                   CLEAR                   ERNT
046200980903     C                   CLEAR                   EPNT
046300020215     C                   CLEAR                   ERND
046400020215     C                   CLEAR                   EPND
046500980903     C                   CLEAR                   ERAA
046600980903     C                   CLEAR                   EPAA
046700980903     C                   CLEAR                   ERAB
046800980903     C                   CLEAR                   EPAB
046900980903     C                   CLEAR                   ERAC
047000980903     C                   CLEAR                   EPAC
047100980903     C                   CLEAR                   ERAT
047200980903     C                   CLEAR                   EPAT
047300020215     C                   CLEAR                   ERAD
047400020215     C                   CLEAR                   EPAD
047500980903     C                   CLEAR                   EVLD
047600980902     C*
047700980902     C                   ENDSR
047800980902     C*------------------------------------------------------------------------*
047900160304     C* ELASAC - ELABORA I SALDI ATTIVITA' COMMERCIALE UNIFICANTE              *
048000980902     C*------------------------------------------------------------------------*
048100980902     C     ELASAC        BEGSR
048200980902     C*
048300980902     C* POSIZIONAMENTO E PRIMA LETTURA FILE
048400980902     C                   EXSR      SETSAC
048500980902     C*
048600980902     C* CICLO FINO A FINE FILE
048700980902DO  1C     $FSAC         DOWEQ     'N'
048800980902     C*
048900980902     C* MEMORIZZA I DATI
049000980902     C                   EXSR      MEMSAC
049100980902     C*
049200980902     C* LETTURA SUCCESSIVA FILE
049300980902     C                   EXSR      LETSAC
049400980902E   1C                   ENDDO                                                  *FINE FILE
049500980902     C*
049600980902     C                   ENDSR
049700980902     C*------------------------------------------------------------------------*
049800160304     C* SETSAC - POSIZIONAMENTO E PRIMA LETTURA               -SALDI ATTIVITÀ- *
049900980902     C*------------------------------------------------------------------------*
050000980902     C     SETSAC        BEGSR
050100980902     C*
050200981111     C* REIMPOSTA LE VARIABILI DI CONTROLLO
050300981111     C                   MOVEL     'N'           $FSAC                          *FINE X TFSAC
050400160304     C                   Z-ADD     CMMcod        KSACMM                         *CODICE AGENTE
050500980902     C                   Z-ADD     PARAAI        KSAAAA                         *ANNO INIZIALE
050600980902     C                   Z-ADD     PARMMI        KSAMMM                         *MESE INIZIALE
050700980902     C     KEYSAC        SETLL     TFSAC01L                           98
050800980902IF  1C     *IN98         IFEQ      *ON                                          *FINE FILE
050900980902     C                   MOVEL     'S'           $FSAC                          *FINE LETTURA
051000980902X   1C                   ELSE
051100980902     C                   EXSR      LETSAC                                       *LETTURA RECORD
051200980902E   1C                   ENDIF
051300980902     C*
051400980902     C                   ENDSR
051500980902     C*------------------------------------------------------------------------*
051600160304     C* LETSAC - LETTURA PROSSIMO RECORD                      -SALDI ATTIVITÀ- *
051700980902     C*------------------------------------------------------------------------*
051800980902     C     LETSAC        BEGSR
051900980902     C*
052000980902     C* LETTURA FINO A:
052100980902     C*  - FINE FILE
052200980902     C*  - TROVATO RECORD VALIDO
052300980902     C                   MOVEL     'N'           WRECOK
052400980902DO  1C     $FSAC         DOWEQ     'N'
052500980902     C     WRECOK        ANDEQ     'N'
052600980902     C     KE2SAC        READE     TFSAC01L                               98
052700980902IF  2C     *IN98         IFEQ      *ON                                          *FINE ARCHIVIO
052800980902     C                   MOVEL     'S'           $FSAC                          *FINE LETTURA
052900980902X   2C                   ELSE
053000980902     C                   EXSR      CHKSAC                                       *CONTROLLA RECORD
053100980902E   2C                   ENDIF
053200980902E   1C                   ENDDO
053300980902     C*
053400980902     C                   ENDSR
053500980902     C*------------------------------------------------------------------------*
053600160304     C* CHKSAC - CONTROLLA VALIDITA' RECORD                   -SALDI ATTIVITÀ- *
053700980902     C*------------------------------------------------------------------------*
053800980902     C     CHKSAC        BEGSR
053900980902     C*
054000980902     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
054100980902     C*
054200980902     C* PERIODO NON RICHIESTO, ESCLUDE
054300980902IF  1C     SACDAT        IFGE      PARAMI
054400980902     C     SACDAT        ANDLE     PARAMF
054500980902X   1C                   ELSE
054600980902     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
054700980902IF  2C     SACDAT        IFGT      PARAMF                                       *FINE PERIODO
054800981008     C                   MOVEL     'S'           $FSAC                          *FINE FILE
054900980902E   2C                   ENDIF
055000980902E   1C                   ENDIF
055100980902     C*
055200980902     C* RECORD FASULLO, ESCLUDE
055300981008IF  1C     SACFFZ        IFNE      *BLANKS
055400980902     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055500980902E   1C                   ENDIF
055600980902     C*
055700980902     C                   ENDSR
055800980902     C*------------------------------------------------------------------------*
055900160304     C* MEMSAC - MEMORIZZA I DATI                             -SALDI ATTIVITÀ- *
056000980902     C*------------------------------------------------------------------------*
056100980902     C     MEMSAC        BEGSR
056200980902     C*
056300980902     C* RECUPERA IL MESE
056400980902     C                   SETOFF                                           28
056500980902     C                   Z-ADD     1             M                 2 0
056600980903     C     SACMMM        LOOKUP    MMM(M)                                 28
056700980902IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
056800981111     C                   MOVEL     'S'           $CTR(M)                        *MEMORIZZATO QC
056900980902     C*---
057000980902     C* ITALIA e ESTERO
057100980902     C*---
057200980903IF  2C     SACFIE        IFEQ      *BLANKS
057300981229     C* POTENZIALI DA TRATTARE
057400980903IF  3C     SACAPL        IFEQ      'P'
057500980902     C                   ADD       SACTO1        POT(M)
057600980903E   3C                   ENDIF
057700980902     C* VISITE POTENZIALI
057800980903IF  3C     SACAPL        IFEQ      'N'
057900980902     C                   ADD       SACTO1        VPT(M)
058000980903E   3C                   ENDIF
058100980902     C* VISITE CLIENTI
058200980903IF  3C     SACAPL        IFEQ      'C'
058300980902     C                   ADD       SACTO1        VCL(M)
058400980903E   3C                   ENDIF
058500980903E   2C                   ENDIF
058600980902     C*---
058700980902     C* ITALIA
058800980903     C*---
058900980903IF  2C     SACFIE        IFEQ      'I'
059000980902     C* OFFERTE POTENZIALI
059100980903IF  3C     SACAPL        IFEQ      'N'
059200980902     C                   ADD       SACTO2        IOPT(M)
059300980903E   3C                   ENDIF
059400980902     C* OFFERTE CLIENTI
059500980903IF  3C     SACAPL        IFEQ      'C'
059600980902     C                   ADD       SACTO2        IOCL(M)
059700980903E   3C                   ENDIF
059800980902     C* TARIFFE POTENZIALI
059900980903IF  3C     SACAPL        IFEQ      'N'
060000980902     C                   ADD       SACTO3        ITPT(M)
060100980903E   3C                   ENDIF
060200980902     C* TARIFFE CLIENTI
060300980903IF  3C     SACAPL        IFEQ      'C'
060400980902     C                   ADD       SACTO3        ITCL(M)
060500980903E   3C                   ENDIF
060600980903E   2C                   ENDIF
060700980902     C*---
060800980902     C* ESTERO
060900980902     C*---
061000980903IF  2C     SACFIE        IFEQ      'E'
061100980902     C* OFFERTE POTENZIALI
061200980903IF  3C     SACAPL        IFEQ      'N'
061300980902     C                   ADD       SACTO2        EOPT(M)
061400980903E   3C                   ENDIF
061500980902     C* OFFERTE CLIENTI
061600980903IF  3C     SACAPL        IFEQ      'C'
061700980902     C                   ADD       SACTO2        EOCL(M)
061800980903E   3C                   ENDIF
061900980902     C* TARIFFE POTENZIALI
062000980903IF  3C     SACAPL        IFEQ      'N'
062100980902     C                   ADD       SACTO3        ETPT(M)
062200980903E   3C                   ENDIF
062300980902     C* TARIFFE CLIENTI
062400980903IF  3C     SACAPL        IFEQ      'C'
062500980902     C                   ADD       SACTO3        ETCL(M)
062600980903E   3C                   ENDIF
062700980903E   2C                   ENDIF
062800980903E   1C                   ENDIF
062900980902     C*
063000980902     C                   ENDSR
063100980902     C*------------------------------------------------------------------------*
063200160304     C* ELACLP - ELABORA I SALDI CLIENTE                                       *
063300980902     C*------------------------------------------------------------------------*
063400980902     C     ELACLP        BEGSR
063500980902     C*
063600980902     C* POSIZIONAMENTO E PRIMA LETTURA FILE
063700980902     C                   EXSR      SETCLP
063800980902     C*
063900980902     C* CICLO FINO A FINE FILE
064000980902DO  1C     $FCLP         DOWEQ     'N'
064100020328     C*
064200020328     C* CICLO SULLA SCHIERA DEI FIGLI
064300020328     C     1             DO        500           C                 3 0
064400020328     C     SKC(C)        IFGT      *ZEROS
064500020328     C*
064600020402     C* VERIFICO CHE IL CLIENTE CORRENTE NON SIA GIA' SATO CONSIDERATO X L'AGENTE UNIFICANTE
064700020328     C     SKC(C)        LOOKUP    SKCAGEU                                55
064800020328     C                   IF        *IN55 = *OFF
064900020328     C*
065000020328     C* SE NN TROVATO LO AGGIUNGO E PROSEGUO L'ELABORAZIONE
065100020328     C                   ADD       1             Y
065200020328     C                   EVAL      SKCAGEU(Y) = SKC(C)
065300020328     C*
065400020328     C* "CHAINO" IL CLIENTE FIGLIO CORRENTE
065500020328     C                   MOVE      SKC(C)        KLPKSC
065600020516     C     KEYCLP2       CHAIN     CNCLP00F
065700020516     C                   IF        %FOUND(CNCLP00F)
065800980902     C*
065900980902     C* OPERAZIONI PER NUOVO CLIENTE
066000980902     C                   EXSR      NEWKSC
066100980902     C*
066200980902     C* ELABORA I SALDI CLIENTE TESTATA
066300980902     C                   EXSR      ELASDC
066400020328     C                   ENDIF
066500020328     C                   ENDIF
066600020328     C*
066700020328     C                   ELSE
066800020328     C                   LEAVE
066900020328     C                   ENDIF
067000020328     C                   ENDDO
067100980902     C*
067200980902     C* LETTURA SUCCESSIVA FILE
067300980902     C                   EXSR      LETCLP
067400020328     C*
067500980902E   1C                   ENDDO                                                  *FINE FILE
067600980902     C*
067700980902     C                   ENDSR
067800980902     C*------------------------------------------------------------------------*
067900160304     C* SETCLP - POSIZIONAMENTO E PRIMA LETTURA             -ARCHIVIO CLIENTI- *
068000980902     C*------------------------------------------------------------------------*
068100980902     C     SETCLP        BEGSR
068200981111     C*
068300981111     C* REIMPOSTA LE VARIABILI DI CONTROLLO
068400981111     C                   MOVEL     'N'           $FCLP                          *FINE X CNCLP
068500980902     C*
068600020906     C                   Z-ADD     1             KLPKUT                         *UTENTE
068700160304     C                   Z-ADD     DUTkci        KLPKCC                         *CAPOCONTO
068800160304     C                   Z-ADD     CMMcod        KLPAGE                         *AGENTE
068900020516     C     KEYCLP        SETLL     CNCLP01L                           98
069000980902IF  1C     *IN98         IFEQ      *ON                                          *FINE FILE
069100980902     C                   MOVEL     'S'           $FCLP                          *FINE LETTURA
069200980902X   1C                   ELSE
069300980902     C                   EXSR      LETCLP                                       *LETTURA RECORD
069400980902E   1C                   ENDIF
069500980902     C*
069600980902     C                   ENDSR
069700980902     C*------------------------------------------------------------------------*
069800160304     C* LETCLP - LETTURA PROSSIMO RECORD                    -ARCHIVIO CLIENTI- *
069900980902     C*------------------------------------------------------------------------*
070000980902     C     LETCLP        BEGSR
070100980902     C*
070200980902     C* LETTURA FINO A:
070300980902     C*  - FINE FILE
070400980902     C*  - TROVATO RECORD VALIDO
070500980902     C                   MOVEL     'N'           WRECOK
070600980902DO  1C     $FCLP         DOWEQ     'N'
070700980902     C     WRECOK        ANDEQ     'N'
070800020516     C     KEYCLP        READE     CNCLP01L                               98
070900980902IF  2C     *IN98         IFEQ      *ON                                          *FINE ARCHIVIO
071000980902     C                   MOVEL     'S'           $FCLP                          *FINE LETTURA
071100980902X   2C                   ELSE
071200980902     C                   EXSR      CHKCLP                                       *CONTROLLA RECORD
071300980902E   2C                   ENDIF
071400980902E   1C                   ENDDO
071500980902     C*
071600980902     C                   ENDSR
071700980902     C*------------------------------------------------------------------------*
071800160304     C* CHKCLP - CONTROLLA VALIDITA' RECORD                 -ARCHIVIO CLIENTI- *
071900980902     C*------------------------------------------------------------------------*
072000980902     C     CHKCLP        BEGSR
072100980902     C*
072200980902     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
072300020328     C                   CLEAR                   SKC
072400020328     C*
072500020402     C* VERIFICO SE IL CLIENTE E' PADRE
072600020328     C                   CLEAR                   TIBS10
072700020328     C                   Z-ADD     PARAMGF       D10DRF                         *DATA RIFERIMENTO
072800020328     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
072900020402     C                   MOVEL     'F'           D10PAF                         *CERCO COME PADRE
073000020328     C                   Z-ADD     I_CLPKSC      D10COD                         *CODICE CLIENTE
073100020328     C                   CALL      'TIBS10R'
073200020328     C                   PARM                    TIBS10
073300020402     C     D10ERR        IFEQ      *BLANKS                                      *SE PADRE
073400020328     C                   MOVEA     D10SKC        SKC                            *CLIENTI FIGLI
073500020328     C                   ELSE
073600020402     C*
073700020402     C* VERIFICO SE IL CLIENTE E' FIGLIO
073800020402     C                   CLEAR                   TIBS10
073900020402     C                   Z-ADD     PARAMGF       D10DRF                         *DATA RIFERIMENTO
074000020402     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
074100020402     C                   MOVEL     'P'           D10PAF                         *CERCO COME FIGLIO
074200020402     C                   Z-ADD     I_CLPKSC      D10COD                         *CODICE CLIENTE
074300020402     C                   CALL      'TIBS10R'
074400020402     C                   PARM                    TIBS10
074500020402     C     D10ERR        IFEQ      *BLANKS                                      *SE FIGLIO
074600020402     C                   MOVEL     'N'           WRECOK                         *NON LO CONSIDERO
074700020402     C                   ELSE
074800020402     C*
074900020402     C* SE NN E' PADRE E NN E' FIGLIO VUOL DIRE CHE E' 1 CLIENTE NN LEGATO E QUINDI LO CONSIDERO
075000020328     C                   MOVEL     D10COD        SKC(1)
075100020328     C                   ENDIF
075200020402     C                   ENDIF
075300980902     C*
075400980902     C                   ENDSR
075500980902     C*------------------------------------------------------------------------*
075600160304     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE                                  *
075700980902     C*------------------------------------------------------------------------*
075800980902     C     NEWKSC        BEGSR
075900980902     C*
076000980902     C* REIMPOSTA LE VARIABILI DI LAVORO
076100980902     C                   MOVEL     *BLANKS       CLPCLN            1            *NUOVO O ACQUISITO
076200980902     C*
076300980902     C* CLIENTE NUOVO/ACQUISITO
076400020409     C                   Z-ADD     CLPDPS        DSDPS                          *DAT PRIMA SPEDIZION
076500020409     C*
076600020409     C* AGGIUNGE IL SECOLO ALLA DATA PRIMA SPEDIZIONE
076700020409IF  1C     CLPANN        IFGT      60
076800020409     C                   Z-ADD     19            CLPSEC
076900020409X   1C                   ELSE
077000020409IF  2C     CLPANN        IFGT      *ZEROS
077100020409     C                   Z-ADD     20            CLPSEC
077200020409E   2C                   ENDIF
077300020409E   1C                   ENDIF
077400020409     C*
077500980902IF  1C     CLPDAT        IFEQ      *ZEROS                                       *NON ANCORA FATTURAT
077600020409     C     CLPDAT        ORGE      PARAMI                                       *FATTURATO NELL'ANNO
077700980902     C                   MOVEL     'N'           CLPCLN                         *NUOVO
077800980902X   1C                   ELSE                                                   *FATTURATO PRIMA
077900980902     C                   MOVEL     'A'           CLPCLN                         *ACQUISITO
078000980902E   1C                   ENDIF
078100980902     C*
078200980902     C                   ENDSR
078300980902     C*------------------------------------------------------------------------*
078400160304     C* ELASDC - ELABORA I SALDI CLIENTE                                       *
078500980902     C*------------------------------------------------------------------------*
078600980902     C     ELASDC        BEGSR
078700980902     C*
078800980902     C* POSIZIONAMENTO E PRIMA LETTURA FILE
078900980902     C                   EXSR      SETSDC
079000980902     C*
079100980902     C* CICLO FINO A FINE FILE
079200980902DO  1C     $FSDC         DOWEQ     'N'
079300980902     C*
079400980902     C* MEMORIZZA I DATI
079500980902     C                   EXSR      MEMSDC
079600980902     C*
079700980902     C* LETTURA SUCCESSIVA FILE
079800980902     C                   EXSR      LETSDC
079900980902E   1C                   ENDDO                                                  *FINE FILE
080000980902     C*
080100980902     C                   ENDSR
080200980902     C*------------------------------------------------------------------------*
080300160304     C* SETSDC - POSIZIONAMENTO E PRIMA LETTURA                -SALDI CLEINTE- *
080400980902     C*------------------------------------------------------------------------*
080500980902     C     SETSDC        BEGSR
080600981111     C*
080700981111     C* AZZERA LE VARIABILI DI CONTROLLO
080800990610     C                   MOVEL     'N'           $FSDC                          *FINE X SISDC
080900980902     C*
081000980902     C                   Z-ADD     CLPKSC        KSCKSC                         *CLIENTE
081100990610     C                   Z-ADD     PARAAI        KSCANN                         *ANNO INIZIALE
081200980902     C                   Z-ADD     PARMMI        KSCMES                         *MESE INIZIALE
081300990610     C     KEYSDC        SETLL     SISDC01L                           97
081400981008IF  1C     *IN97         IFEQ      *ON                                          *FINE FILE
081500980902     C                   MOVEL     'S'           $FSDC                          *FINE LETTURA
081600980902X   1C                   ELSE
081700980902     C                   EXSR      LETSDC                                       *LETTURA RECORD
081800980902E   1C                   ENDIF
081900980902     C*
082000980902     C                   ENDSR
082100980902     C*------------------------------------------------------------------------*
082200160304     C* LETSDC - LETTURA PROSSIMO RECORD                       -SALDI CLIENTE- *
082300980902     C*------------------------------------------------------------------------*
082400980902     C     LETSDC        BEGSR
082500980902     C*
082600980902     C* LETTURA FINO A:
082700980902     C*  - FINE FILE
082800980902     C*  - TROVATO RECORD VALIDO
082900980902     C                   MOVEL     'N'           WRECOK
083000981007DO  1C     $FSDC         DOWEQ     'N'
083100980902     C     WRECOK        ANDEQ     'N'
083200990610     C     KE2SDC        READE     SISDC01L                               97
083300981008IF  2C     *IN97         IFEQ      *ON                                          *FINE ARCHIVIO
083400980902     C                   MOVEL     'S'           $FSDC                          *FINE LETTURA
083500980902X   2C                   ELSE
083600980902     C                   EXSR      CHKSDC                                       *CONTROLLA RECORD
083700980902E   2C                   ENDIF
083800980902E   1C                   ENDDO
083900980902     C*
084000980902     C                   ENDSR
084100980902     C*------------------------------------------------------------------------*
084200160304     C* CHKSDC - CONTROLLA VALIDITA' RECORD                    -SALDI CLIENTE- *
084300980902     C*------------------------------------------------------------------------*
084400980902     C     CHKSDC        BEGSR
084500980902     C*
084600980902     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
084700020516     C*
084800020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
084900020516IF  1C     WRECOK        IFEQ      'S'
085000020516IF  2C     SDCNSP        IFEQ      *ZEROS
085100020516     C     SDCPKG        ANDEQ     *ZEROS
085200020516     C     SDCPCC        ANDEQ     *ZEROS
085300020516     C     SDCPLS        ANDEQ     *ZEROS
085400020516     C     SDCNCO        ANDEQ     *ZEROS
085500020516     C     SDCVOL        ANDEQ     *ZEROS
085600020516     C     SDCVCC        ANDEQ     *ZEROS
085700020516     C     SDCVLS        ANDEQ     *ZEROS
085800020516     C     SDCIRB        ANDEQ     *ZEROS
085900020516     C     SDCIRR        ANDEQ     *ZEROS
086000020516     C     SDCINC        ANDEQ     *ZEROS
086100020516     C     SDCIRP        ANDEQ     *ZEROS
086200020516     C     SDCIRC        ANDEQ     *ZEROS
086300020516     C     SDCIRD        ANDEQ     *ZEROS
086400020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086500020516E   2C                   ENDIF
086600020516E   1C                   ENDIF
086700980902     C*
086800980902     C* PERIODO NON RICHIESTO, ESCLUDE
086900990712IF  1C     SDCDAT        IFGE      PARAMI
087000990712     C     SDCDAT        ANDLE     PARAMF
087100980902X   1C                   ELSE
087200980902     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
087300990712IF  2C     SDCDAT        IFGT      PARAMF                                       *FINE PERIODO
087400981008     C                   MOVEL     'S'           $FSDC                          *FINE FILE
087500980902E   2C                   ENDIF
087600980902E   1C                   ENDIF
087700020402     C*
087800020402     C* CONTROLLO FASCE DI PESO
087900020402     C                   Z-ADD     1             P
088000020402     C                   SETOFF                                           28
088100020402     C     SDCFPE        LOOKUP    FPE(P)                                 28
088200020402IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
088300020402     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
088400020402E   1C                   ENDIF
088500980902     C*
088600980902     C                   ENDSR
088700980902     C*------------------------------------------------------------------------*
088800160304     C* MEMSDC - MEMORIZZA I DATI                              -SALDI CLIENTE- *
088900980902     C*------------------------------------------------------------------------*
089000980902     C     MEMSDC        BEGSR
089100020329     C*
089200020329     C* DETERMINA IL NETWORK
089300020329     C                   MOVEL     *BLANKS       DEPNTWP           3
089400020329     C                   Z-ADD     1             R
089500020329     C     SDCREP        LOOKUP    RGREG(R)                               55
089600020329     C                   IF        *IN55 = *ON
089700020329     C                   EVAL      DEPNTWP = RGNTW(R)
089800020329     C                   ENDIF
089900020329     C*
090000020329     C                   MOVEL     *BLANKS       DEPNTWA           3
090100020329     C                   Z-ADD     1             R
090200020329     C     SDCREA        LOOKUP    RGREG(R)                               55
090300020329     C                   IF        *IN55 = *ON
090400020329     C                   EVAL      DEPNTWA = RGNTW(R)
090500020329     C                   ENDIF
090600980902     C*
090700980902     C* IMPOSTA FLAG ITALIA/ESTERO
090800980904     C                   MOVEL     *BLANKS       SDCFIE            1            *ITALIA/ESTERO
090900020329IF  1C     DEPNTWP       IFNE      *BLANKS
091000020329     C     DEPNTWA       ORNE      *BLANKS
091100980904     C                   MOVEL     'E'           SDCFIE                         *ESTERO
091200980902X   1C                   ELSE
091300980904     C                   MOVEL     'I'           SDCFIE                         *ITALIA
091400980902E   1C                   ENDIF
091500980902     C*
091600980902     C* RECUPERA IL MESE
091700980902     C                   SETOFF                                           28
091800980902     C                   Z-ADD     1             M
091900980903     C     SDCMES        LOOKUP    MMM(M)                                 28
092000980902IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
092100980902     C*
092200980902     C* RECUPERA IL RAGGRUPPAMENTO BOLLE
092300980902     C                   SETOFF                                           28
092400980902     C                   Z-ADD     1             I
092500980902     C     SDCRBL        LOOKUP    FRBL(I)                                28
092600980902IF  2C     *IN28         IFEQ      *ON                                          *TROVATO
092700981111     C                   MOVEL     'S'           $CTR(M)                        *MEMORIZZATO QC
092800980902     C*
092900980902     C* BOLLA NON VALIDA COME N°SPEDIZIONE, CANCELLA I DATI
093000980902IF  3C     FSP(I)        IFNE      'S'
093100980902     C                   Z-ADD     *ZEROS        SDCNSP                         *SPEDIZIONI
093200980902     C                   Z-ADD     *ZEROS        SDCPKG                         *PESO
093300020828     C                   Z-ADD     *ZEROS        SDCPCC                         *PESO CON CML
093400020828     C                   Z-ADD     *ZEROS        SDCPLS                         *PESO CML
093500980902     C                   Z-ADD     *ZEROS        SDCNCO                         *NUMERO COLLI
093600980902     C                   Z-ADD     *ZEROS        SDCVOL                         *VOLUME TOTALE
093700980902     C                   Z-ADD     *ZEROS        SDCVCC                         *VOLUME CON CML
093800980902     C                   Z-ADD     *ZEROS        SDCVLS                         *VOLUME CML
093900980902E   3C                   ENDIF
094000980902     C*
094100980902     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
094200980902     C                   Z-ADD     *ZEROS        RICDEL           15 3
094300980902     C     SDCIRD        ADD       SDCIRR        RICDEL
094400980902     C     SDCINC        ADD       RICDEL        RICDEL
094500980902     C*---
094600980902     C* ITALIA
094700980902     C*---
094800980904IF  3C     SDCFIE        IFEQ      'I'
094900980902     C***
095000980902     C* NUOVI
095100980902     C***
095200980902IF  4C     CLPCLN        IFEQ      'N'
095300980902     C* SPEDIZIONI
095400980902     C                   ADD       SDCNSP        ISPN(M)
095500980902     C* PESO
095600980902     C                   ADD       SDCPKG        IPSN(M)
095700980902     C* COLLI
095800980902     C                   ADD       SDCNCO        ICLN(M)
095900980902     C* VOLUME
096000980902     C                   ADD       SDCVOL        IVLN(M)
096100980902     C* VOLUME DI CML
096200980902     C                   ADD       SDCVCC        IVCN(M)
096300980902     C* VOLUME CML
096400980902     C                   ADD       SDCVLS        IVSN(M)
096500020215     C* RICAVO REALE e PRESUNTO  "A"
096600980902IF  5C     CLPCLV        IFEQ      'A'
096700980902     C                   ADD       RICDEL        IRNA(M)
096800980902     C                   ADD       SDCIRP        IPNA(M)
096900980902E   5C                   ENDIF
097000980902     C* RICAVO REALE e PRESUNTO  "B"
097100980902IF  5C     CLPCLV        IFEQ      'B'
097200980902     C                   ADD       RICDEL        IRNB(M)
097300980902     C                   ADD       SDCIRP        IPNB(M)
097400980902E   5C                   ENDIF
097500980902     C* RICAVO REALE e PRESUNTO  "C"
097600980902IF  5C     CLPCLV        IFEQ      'C'
097700980902     C                   ADD       RICDEL        IRNC(M)
097800980902     C                   ADD       SDCIRP        IPNC(M)
097900980902E   5C                   ENDIF
098000980902     C* RICAVO REALE e PRESUNTO  "T"
098100980902IF  5C     CLPCLV        IFEQ      'T'
098200980902     C                   ADD       RICDEL        IRNT(M)
098300980902     C                   ADD       SDCIRP        IPNT(M)
098400980902E   5C                   ENDIF
098500020215     C* RICAVO REALE e PRESUNTO  "D"
098600020215IF  5C     CLPCLV        IFEQ      'D'
098700020215     C                   ADD       RICDEL        IRND(M)
098800020215     C                   ADD       SDCIRP        IPND(M)
098900020215E   5C                   ENDIF
099000980902E   4C                   ENDIF
099100980902     C***
099200980902     C* ACQUISITI
099300980902     C***
099400980902IF  4C     CLPCLN        IFEQ      'A'
099500980902     C* SPEDIZIONI
099600980902     C                   ADD       SDCNSP        ISPA(M)
099700980902     C* PESO
099800980902     C                   ADD       SDCPKG        IPSA(M)
099900980902     C* COLLI
100000980902     C                   ADD       SDCNCO        ICLA(M)
100100980902     C* VOLUME
100200980902     C                   ADD       SDCVOL        IVLA(M)
100300980902     C* VOLUME DI CML
100400980902     C                   ADD       SDCVCC        IVCA(M)
100500980902     C* VOLUME CML
100600980902     C                   ADD       SDCVLS        IVSA(M)
100700020215     C* RICAVO REALE e PRESUNTO  "A"
100800980902IF  5C     CLPCLV        IFEQ      'A'
100900980902     C                   ADD       RICDEL        IRAA(M)
101000980902     C                   ADD       SDCIRP        IPAA(M)
101100980902E   5C                   ENDIF
101200980902     C* RICAVO REALE e PRESUNTO  "B"
101300980902IF  5C     CLPCLV        IFEQ      'B'
101400980902     C                   ADD       RICDEL        IRAB(M)
101500980902     C                   ADD       SDCIRP        IPAB(M)
101600980902E   5C                   ENDIF
101700980902     C* RICAVO REALE e PRESUNTO  "C"
101800980902IF  5C     CLPCLV        IFEQ      'C'
101900980902     C                   ADD       RICDEL        IRAC(M)
102000980902     C                   ADD       SDCIRP        IPAC(M)
102100980902E   5C                   ENDIF
102200980902     C* RICAVO REALE e PRESUNTO  "T"
102300980902IF  5C     CLPCLV        IFEQ      'T'
102400980902     C                   ADD       RICDEL        IRAT(M)
102500980902     C                   ADD       SDCIRP        IPAT(M)
102600980902E   5C                   ENDIF
102700020215IF  5C     CLPCLV        IFEQ      'D'
102800020215     C                   ADD       RICDEL        IRAD(M)
102900020215     C                   ADD       SDCIRP        IPAD(M)
103000020215E   5C                   ENDIF
103100980902E   4C                   ENDIF
103200980902     C***
103300980902     C* NUOVI e ACQUISITI
103400980902     C***
103500980902     C* VOLUME DISTRIBUZIONE
103600980902IF  4C     SDCTSP        IFEQ      'D'
103700980902     C                   ADD       SDCVOL        IVLD(M)
103800980902E   4C                   ENDIF
103900980902E   3C                   ENDIF
104000980902     C*---
104100980902     C* ESTERO
104200980902     C*---
104300980904IF  3C     SDCFIE        IFEQ      'E'
104400980902     C***
104500980902     C* NUOVI
104600980902     C***
104700980902IF  4C     CLPCLN        IFEQ      'N'
104800980902     C* SPEDIZIONI
104900980902     C                   ADD       SDCNSP        ESPN(M)
105000980902     C* PESO
105100980902     C                   ADD       SDCPKG        EPSN(M)
105200980902     C* COLLI
105300980902     C                   ADD       SDCNCO        ECLN(M)
105400980902     C* VOLUME
105500980902     C                   ADD       SDCVOL        EVLN(M)
105600980902     C* VOLUME DI CML
105700980902     C                   ADD       SDCVCC        EVCN(M)
105800980902     C* VOLUME CML
105900980902     C                   ADD       SDCVLS        EVSN(M)
106000020215     C* RICAVO REALE e PRESUNTO  "A"
106100980902IF  5C     CLPCLV        IFEQ      'A'
106200980902     C                   ADD       RICDEL        ERNA(M)
106300980902     C                   ADD       SDCIRP        EPNA(M)
106400980902E   5C                   ENDIF
106500980902     C* RICAVO REALE e PRESUNTO  "B"
106600980902IF  5C     CLPCLV        IFEQ      'B'
106700980902     C                   ADD       RICDEL        ERNB(M)
106800980902     C                   ADD       SDCIRP        EPNB(M)
106900980902E   5C                   ENDIF
107000980902     C* RICAVO REALE e PRESUNTO  "C"
107100980902IF  5C     CLPCLV        IFEQ      'C'
107200980902     C                   ADD       RICDEL        ERNC(M)
107300980902     C                   ADD       SDCIRP        EPNC(M)
107400980902E   5C                   ENDIF
107500980902     C* RICAVO REALE e PRESUNTO  "T"
107600980902IF  5C     CLPCLV        IFEQ      'T'
107700980902     C                   ADD       RICDEL        ERNT(M)
107800980902     C                   ADD       SDCIRP        EPNT(M)
107900980902E   5C                   ENDIF
108000020215IF  5C     CLPCLV        IFEQ      'D'
108100020215     C                   ADD       RICDEL        ERND(M)
108200020215     C                   ADD       SDCIRP        EPND(M)
108300020215E   5C                   ENDIF
108400980902E   4C                   ENDIF
108500980902     C***
108600980902     C* ACQUISITI
108700980902     C***
108800980902IF  4C     CLPCLN        IFEQ      'A'
108900980902     C* SPEDIZIONI
109000980902     C                   ADD       SDCNSP        ESPA(M)
109100980902     C* PESO
109200980902     C                   ADD       SDCPKG        EPSA(M)
109300980902     C* COLLI
109400980902     C                   ADD       SDCNCO        ECLA(M)
109500980902     C* VOLUME
109600980902     C                   ADD       SDCVOL        EVLA(M)
109700980902     C* VOLUME DI CML
109800980902     C                   ADD       SDCVCC        EVCA(M)
109900980902     C* VOLUME CML
110000980902     C                   ADD       SDCVLS        EVSA(M)
110100020215     C* RICAVO REALE e PRESUNTO  "A"
110200980902IF  5C     CLPCLV        IFEQ      'A'
110300980902     C                   ADD       RICDEL        ERAA(M)
110400980902     C                   ADD       SDCIRP        EPAA(M)
110500980902E   5C                   ENDIF
110600980902     C* RICAVO REALE e PRESUNTO  "B"
110700980902IF  5C     CLPCLV        IFEQ      'B'
110800980902     C                   ADD       RICDEL        ERAB(M)
110900980902     C                   ADD       SDCIRP        EPAB(M)
111000980902E   5C                   ENDIF
111100980902     C* RICAVO REALE e PRESUNTO  "C"
111200980902IF  5C     CLPCLV        IFEQ      'C'
111300980902     C                   ADD       RICDEL        ERAC(M)
111400980902     C                   ADD       SDCIRP        EPAC(M)
111500980902E   5C                   ENDIF
111600980902     C* RICAVO REALE e PRESUNTO  "T"
111700980902IF  5C     CLPCLV        IFEQ      'T'
111800980902     C                   ADD       RICDEL        ERAT(M)
111900980902     C                   ADD       SDCIRP        EPAT(M)
112000980902E   5C                   ENDIF
112100020215IF  5C     CLPCLV        IFEQ      'D'
112200020215     C                   ADD       RICDEL        ERAD(M)
112300020215     C                   ADD       SDCIRP        EPAD(M)
112400020215E   5C                   ENDIF
112500980902E   4C                   ENDIF
112600980902     C***
112700980902     C* NUOVI e ACQUISITI
112800980902     C***
112900980902     C* VOLUME DISTRIBUZIONE
113000980902IF  4C     SDCTSP        IFEQ      'D'
113100980902     C                   ADD       SDCVOL        EVLD(M)
113200980902E   4C                   ENDIF
113300980902E   3C                   ENDIF
113400980902E   2C                   ENDIF
113500980902E   1C                   ENDIF
113600980902     C*
113700980902     C                   ENDSR
113800980902     C*------------------------------------------------------------------------*
113900160304     C* SCRREP - SCRIVE UN RECORD PER IL COMMERCIALE UNIFICANTE                *
114000980902     C*------------------------------------------------------------------------*
114100980902     C     SCRREP        BEGSR
114200980902     C*
114300980902     C* IMPOSTA LA CHIAVE DI LETTURA
114400980902     C                   MOVEL     DEPDIV        KEPDIV                         *DIVISIONE
114500980902     C                   Z-ADD     DEPARE        KEPARE                         *AREA
114600980902     C                   Z-ADD     DEPFIL        KEPFIL                         *FILIALE
114700980902     C                   Z-ADD     DEPRGF        KEPRGF                         *COMMERCIALE
114800980902     C*
114900980902     C* PER CIASCUN MESE MEMORIZZATO, SCRIVE UN RECORD NEL REPORT
115000980902DO  1C     1             DO        12            M
115100980902IF  2C     MMM(M)        IFNE      *ZEROS
115200980902     C                   Z-ADD     AAA(M)        KEPANN                         *ANNO
115300980902     C                   Z-ADD     MMM(M)        KEPMES                         *MESE
115400980902     C                   CLEAR                   WFREP000                       *PULISCE RECORD
115500050408     C     KEYREP        CHAIN     WFREP11L                           97
115600980902IF  4C     *IN97         IFEQ      *OFF                                         *MODIFICA
115700980902     C                   EXSR      IMPREP                                       *IMPOSTA CAMPI
115800980902     C                   UPDATE    WFREP000                                     *AGGIORNA
115900980902X   4C                   ELSE                                                   *INSERIMENTO
116000980902     C                   MOVEL     KEPDIV        REPDIV                         *IMPOSTA CHIAVE
116100980902     C                   Z-ADD     KEPARE        REPARE
116200980902     C                   Z-ADD     KEPFIL        REPFIL
116300980902     C                   Z-ADD     KEPRGF        REPRGF
116400980902     C                   Z-ADD     KEPANN        REPANN
116500980902     C                   Z-ADD     KEPMES        REPMES
116600050607     C                   Z-ADD     depdip        REPdip
116700050607     C                   movel     depsoc        REPsoc
116800980902     C                   EXSR      IMPREP                                       *IMPOSTA CAMPI
116900980902     C                   WRITE     WFREP000                                     *SCRIVE
117000980902E   4C                   ENDIF
117100980902E   2C                   ENDIF
117200980902E   1C                   ENDDO                                                  *FINE PERIODO
117300980902     C*
117400980902     C                   ENDSR
117500980902     C*------------------------------------------------------------------------*
117600160304     C* IMPREP - IMPOSTA UN RECORD PER IL REPORT                               *
117700980902     C*------------------------------------------------------------------------*
117800980902     C     IMPREP        BEGSR
117900980902     C*
118000980902     C* CAMPI COMUNI
118100980902     C                   MOVEL     DEPDRG        REPDRG
118200980902     C                   Z-ADD     POT(M)        REPPOT
118300980902     C                   Z-ADD     PTR(M)        REPPTR
118400980902     C                   Z-ADD     MAI(M)        REPMAI
118500980902     C                   Z-ADD     VPT(M)        REPVPT
118600980902     C                   Z-ADD     VCL(M)        REPVCL
118700980902     C* ITALIA
118800980902     C                   Z-ADD     DEIPCF        REIPCF
118900980902     C                   Z-ADD     IOPT(M)       REIOPT
119000980902     C                   Z-ADD     IOCL(M)       REIOCL
119100980902     C                   Z-ADD     ITPT(M)       REITPT
119200980902     C                   Z-ADD     ITCL(M)       REITCL
119300980902     C                   Z-ADD     ISPN(M)       REISPN
119400980902     C                   Z-ADD     IPSN(M)       REIPSN
119500980902     C                   Z-ADD     ICLN(M)       REICLN
119600980902     C                   Z-ADD     IVLN(M)       REIVLN
119700980902     C                   Z-ADD     IVCN(M)       REIVCN
119800980902     C                   Z-ADD     IVSN(M)       REIVSN
119900980902     C                   Z-ADD     ISPA(M)       REISPA
120000980902     C                   Z-ADD     IPSA(M)       REIPSA
120100980902     C                   Z-ADD     ICLA(M)       REICLA
120200980902     C                   Z-ADD     IVLA(M)       REIVLA
120300980902     C                   Z-ADD     IVCA(M)       REIVCA
120400980902     C                   Z-ADD     IVSA(M)       REIVSA
120500980902     C                   Z-ADD     IRNA(M)       REIRNA
120600980902     C                   Z-ADD     IPNA(M)       REIPNA
120700980902     C                   Z-ADD     IRNB(M)       REIRNB
120800980902     C                   Z-ADD     IPNB(M)       REIPNB
120900980902     C                   Z-ADD     IRNC(M)       REIRNC
121000980902     C                   Z-ADD     IPNC(M)       REIPNC
121100980902     C                   Z-ADD     IRNT(M)       REIRNT
121200980902     C                   Z-ADD     IPNT(M)       REIPNT
121300020215     C                   Z-ADD     IRND(M)       REIRND
121400020215     C                   Z-ADD     IPND(M)       REIPND
121500980902     C                   Z-ADD     IRAA(M)       REIRAA
121600980902     C                   Z-ADD     IPAA(M)       REIPAA
121700980902     C                   Z-ADD     IRAB(M)       REIRAB
121800980902     C                   Z-ADD     IPAB(M)       REIPAB
121900980902     C                   Z-ADD     IRAC(M)       REIRAC
122000980902     C                   Z-ADD     IPAC(M)       REIPAC
122100980902     C                   Z-ADD     IRAT(M)       REIRAT
122200980902     C                   Z-ADD     IPAT(M)       REIPAT
122300020215     C                   Z-ADD     IRAD(M)       REIRAD
122400020215     C                   Z-ADD     IPAD(M)       REIPAD
122500980902     C                   Z-ADD     IVLD(M)       REIVLD
122600980902     C******
122700980902     C                   Z-ADD     *ZEROS        REISDN                         *CAMPI DA VALORIZZAR
122800980902     C                   Z-ADD     *ZEROS        REISAB
122900980902     C                   Z-ADD     *ZEROS        REICDB
123000980902     C                   Z-ADD     *ZEROS        REITSC                         *CAMPI SOSPESI
123100980902     C                   Z-ADD     *ZEROS        REITNF
123200980902     C******
123300020215     C* ESTERO
123400980902     C                   Z-ADD     DEEPCF        REEPCF
123500980902     C                   Z-ADD     EOPT(M)       REEOPT
123600980902     C                   Z-ADD     EOCL(M)       REEOCL
123700980902     C                   Z-ADD     ETPT(M)       REETPT
123800980902     C                   Z-ADD     ETCL(M)       REETCL
123900980902     C                   Z-ADD     ESPN(M)       REESPN
124000980902     C                   Z-ADD     EPSN(M)       REEPSN
124100980902     C                   Z-ADD     ECLN(M)       REECLN
124200980902     C                   Z-ADD     EVLN(M)       REEVLN
124300980902     C                   Z-ADD     EVCN(M)       REEVCN
124400980902     C                   Z-ADD     EVSN(M)       REEVSN
124500980902     C                   Z-ADD     ESPA(M)       REESPA
124600980902     C                   Z-ADD     EPSA(M)       REEPSA
124700980902     C                   Z-ADD     ECLA(M)       REECLA
124800980902     C                   Z-ADD     EVLA(M)       REEVLA
124900980902     C                   Z-ADD     EVCA(M)       REEVCA
125000980902     C                   Z-ADD     EVSA(M)       REEVSA
125100980902     C                   Z-ADD     ERNA(M)       REERNA
125200980902     C                   Z-ADD     EPNA(M)       REEPNA
125300980902     C                   Z-ADD     ERNB(M)       REERNB
125400980902     C                   Z-ADD     EPNB(M)       REEPNB
125500980902     C                   Z-ADD     ERNC(M)       REERNC
125600980902     C                   Z-ADD     EPNC(M)       REEPNC
125700980902     C                   Z-ADD     ERNT(M)       REERNT
125800980902     C                   Z-ADD     EPNT(M)       REEPNT
125900020215     C                   Z-ADD     ERND(M)       REERND
126000020215     C                   Z-ADD     EPND(M)       REEPND
126100980902     C                   Z-ADD     ERAA(M)       REERAA
126200980902     C                   Z-ADD     EPAA(M)       REEPAA
126300980902     C                   Z-ADD     ERAB(M)       REERAB
126400980902     C                   Z-ADD     EPAB(M)       REEPAB
126500980902     C                   Z-ADD     ERAC(M)       REERAC
126600980902     C                   Z-ADD     EPAC(M)       REEPAC
126700980902     C                   Z-ADD     ERAT(M)       REERAT
126800980902     C                   Z-ADD     EPAT(M)       REEPAT
126900020215     C                   Z-ADD     ERAD(M)       REERAD
127000020215     C                   Z-ADD     EPAD(M)       REEPAD
127100980902     C                   Z-ADD     EVLD(M)       REEVLD
127200980902     C******
127300980902     C                   Z-ADD     *ZEROS        REESDN                         *CAMPI DA VALORIZZAR
127400980902     C                   Z-ADD     *ZEROS        REESAB
127500980902     C                   Z-ADD     *ZEROS        REECDB
127600980902     C                   Z-ADD     *ZEROS        REETSC                         *CAMPI SOSPESI
127700980902     C                   Z-ADD     *ZEROS        REETNF
127800980902     C******
127900980902     C                   ENDSR
128000980902     C*------------------------------------------------------------------------*
128100160304     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI                                *
128200980902     C*------------------------------------------------------------------------*
128300980902     C     CARTAB        BEGSR
128400020402     C*---
128500980902     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
128600980902     C*---
128700980903     C                   Z-ADD     *ZEROS        I                 4 0
128800020906     C                   Z-ADD     1             KTBKUT
128900980902     C                   MOVEL     '2W'          KTBCOD
129000980902     C     KE2TAB        SETLL     TABEL00F
129100980902     C     KE2TAB        READE     TABEL00F                               99
129200980902DO  1C     *IN99         DOWEQ     *OFF
129300980902IF  2C     TBLFLG        IFEQ      *BLANKS
129400980902     C                   ADD       1             I
129500980902     C                   MOVEL     TBLUNI        DS2W
129600980902     C                   MOVEL     TBLKEY        FRBL(I)                        *RAGGRUPP.TIPO BOLLA
129700980902     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONI
129800980902     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVI
129900980902E   2C                   ENDIF
130000980902     C     KE2TAB        READE     TABEL00F                               99
130100980902E   1C                   ENDDO
130200020329     C*-------------------------
130300020329     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
130400020329     C*-------------------------
130500020329     C                   Z-ADD     *ZEROS        R
130600020906     C                   Z-ADD     1             KTBKUT
130700020329     C                   MOVEL     '2M'          KTBCOD
130800020329     C     KE2TAB        SETLL     TABEL00F
130900020329     C     KE2TAB        READE     TABEL00F                               99
131000020329DO  1C     *IN99         DOWEQ     *OFF
131100020329IF  2C     TBLFLG        IFEQ      *BLANKS
131200020329     C                   ADD       1             R
131300020329     C                   MOVEL     TBLKEY        RGREG(R)
131400020329     C                   MOVEL     TBLUNI        DS2M
131500020329     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
131600020329E   2C                   ENDIF
131700020329     C     KE2TAB        READE     TABEL00F                               99
131800020329E   1C                   ENDDO
131900020402     C*--------------------
132000020402     C* FASCE DI PESO
132100020402     C*--------------------
132200020402IF  1C     PARAMF        IFLE      199812                                       *FASCE FINO AL '98
132300020402     C                   MOVEL     'SDC'         WSX               3
132400020402X   1C                   ELSE                                                   *FASCE DAL '99
132500020402IF  2C     PARAMF        IFLE      200112                                       *FASCE DAL '99 FINO
132600020402     C                   MOVEL     'S9C'         WSX
132700020402X   2C                   ELSE                                                   *FASCE DAL 2002
132800020402     C                   MOVEL     'S2C'         WSX
132900020402E   2C                   ENDIF
133000020402E   1C                   ENDIF
133100020402     C                   Z-ADD     *ZEROS        P
133200020906     C                   Z-ADD     1             KTBKUT
133300020402     C                   MOVEL     '2L'          KTBCOD
133400020402     C     KE2TAB        SETLL     TABEL00F
133500020402     C     KE2TAB        READE     TABEL00F                               99
133600020402DO  1C     *IN99         DOWEQ     *OFF
133700020402IF  2C     TBLFLG        IFEQ      *BLANKS
133800020402     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
133900020402     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
134000020402IF  3C     FILEST        IFEQ      WSX
134100020402     C                   MOVEL     TBLUNI        DS2L
134200020402IF  4C     §2LKGI        IFGE      5000
134300020402     C     PARAMI        ANDLT     200001
134400020402     C                   ELSE
134500020402     C                   ADD       1             P
134600020402     C                   MOVE      STACOD        FPE(P)                         *...+XX
134700020402E   4C                   ENDIF
134800020402E   3C                   ENDIF
134900020402E   2C                   ENDIF
135000020402     C     KE2TAB        READE     TABEL00F                               99
135100020402E   1C                   ENDDO
135200160304      /free
135300160304
135400160304         // -?Intabellamento Filiali e relativi Aree / Distretti?
135500160304         clear  xF;
135600160304         setll  ( *loval )  AZORG;
135700160304         read  AZORG;
135800160304         DoW  Not %eof(AZORG01L);
135900160304           if  ORGfva = *blank  and  ORGfag <> 'V';
136000160304             xF += 1;
136100160304             sk_Fil(xF) = ORGfil;
136200160304             sk_Are(xF) = ORGcar;
136300160304             sk_Dis(xF) = ORGfl3;
136400160304           endif;
136500160304           read  AZORG;
136600160304         EndDo;
136700160304
136800160304      /end-free
136900980902     C*
137000980902     C                   ENDSR
137100980902     C*------------------------------------------------------------------------*
137200160304     C* CARMES - CARICA I MESI/ANNI DA ELEBORARE                               *
137300980902     C*------------------------------------------------------------------------*
137400980902     C     CARMES        BEGSR
137500980902     C*
137600980902     C* REIMPOSTA LE VARIABILI DI LAVORO
137700980902     C                   CLEAR                   AAA                            *ANNI
137800980902     C                   CLEAR                   MMM                            *MESI
137900980902     C                   Z-ADD     *ZEROS        J                 4 0          *N° MESI DA ELABORAR
138000980902     C                   Z-ADD     PARAAF        WAA4              4 0          *ANNO FINALE LAVORO
138100980902     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
138200980902     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
138300980902     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
138400980902     C*
138500980902     C* ANNO INIZIALE UGUALE AD ANNO FINALE
138600980902IF  1C     WAA4          IFEQ      WAA
138700980902     C*
138800980902     C* CALCOLO I MESI DI DIFFERENZA
138900980902DO  2C     WMM           DOWLE     WMM2
139000980902     C                   ADD       1             J
139100980902     C                   Z-ADD     WAA           AAA(J)
139200980902     C                   Z-ADD     WMM           MMM(J)
139300980902     C                   ADD       1             WMM
139400980902E   2C                   ENDDO
139500980902     C*
139600980902     C* IL RESTO DELLA SCHIERA A ZERO
139700980902DO  2C     J             DOWLT     12
139800980902     C                   ADD       1             J
139900980902     C                   Z-ADD     *ZEROS        AAA(J)
140000980902     C                   Z-ADD     *ZEROS        MMM(J)
140100980902E   2C                   ENDDO
140200980902     C*
140300980902     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
140400980902X   1C                   ELSE
140500980902     C*
140600980902     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
140700980902DO  2C     WMM           DOWLE     12
140800980902     C                   ADD       1             J
140900980902     C                   Z-ADD     WAA           AAA(J)
141000980902     C                   Z-ADD     WMM           MMM(J)
141100980902     C                   ADD       1             WMM
141200980902E   2C                   ENDDO
141300980902     C*
141400980902     C* MEMORIZZO I MESI DELL'ANNO FINALE
141500980902     C                   Z-ADD     01            WMM
141600980902DO  2C     WMM           DOWLE     WMM2
141700980902     C                   ADD       1             J
141800980902     C                   Z-ADD     WAA4          AAA(J)
141900980902     C                   Z-ADD     WMM           MMM(J)
142000980902     C                   ADD       1             WMM
142100980902E   2C                   ENDDO
142200980902     C*
142300980902     C* IL RESTO DELLA SCHIERA A ZERO
142400980902DO  2C     J             DOWLT     12
142500980902     C                   ADD       1             J
142600980902     C                   Z-ADD     *ZEROS        AAA(J)
142700980902     C                   Z-ADD     *ZEROS        MMM(J)
142800980902E   2C                   ENDDO
142900980902E   1C                   ENDIF
143000980902     C*
143100980902     C                   ENDSR
143200980902     C*------------------------------------------------------------------------*
143300160304     C* CANREP - CANCELLA IL WORKFILE                                          *
143400980902     C*------------------------------------------------------------------------*
143500980903     C     CANREP        BEGSR
143600980902     C*
143700980902     C* CANCELLA IL WFILE PER IL PERIODO RICHIESTO
143800050408     C     *LOVAL        SETLL     WFREP11L
143900050408     C                   READ      WFREP11L                               99
144000980902DO  1C     *IN99         DOWEQ     *OFF
144100980902IF  2C     REPDAT        IFGE      PARAMI
144200980902     C     REPDAT        ANDLE     PARAMF
144300980902     C                   DELETE    WFREP000
144400980902E   2C                   ENDIF
144500050408     C                   READ      WFREP11L                               99
144600980902E   1C                   ENDDO
144700980902     C*
144800980902     C                   ENDSR
144900980902     C*------------------------------------------------------------------------*
145000160304     C* *INZSR - OPERAZIONI INIZIALI                                           *
145100980902     C*------------------------------------------------------------------------*
145200980902     C     *INZSR        BEGSR
145300980902     C*
145400980902     C* RICEVIMENTO PARAMETRI
145500980902     C     *ENTRY        PLIST
145600980902     C                   PARM                    KPJBA
145700980902     C                   MOVEL     KPJBU         PARAM
145800160304      *
145900160304      * -?Reperimento dati job?
146000160304     c                   exsr      sr_DatiJob
146100020328     C*
146200020402     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
146300020328     C                   MOVEL     PARAMF        PARAMGF           8 0
146400020402     C                   MOVE      '28'          PARAMGF
146500081119     C*
146600081119     C*------- GIORNO    di    F I N E    M E S E ---------------------------*
146700081119     C*
146800081119     C*  Ultimo giorno del mese
146900081119     C                   MOVE      PARAMGF       MMGG_4            4 0
147000081119     C                   MOVEL     MMGG_4        Mm_2              2 0
147100081119     C                   MOVEl     PARAMGF       Anno_4            4 0
147200081119      * se è febbraio
147300081119     c                   If        Mm_2 = 2
147400081119      *
147500081119      *  potrebbe essere bisestile 29 gg.
147600081119     c     Anno_4        div       4             risulta           3 0
147700081119     c                   mvr                     resto             3 0
147800081119      *  Se bisestile
147900081119     c                   If        resto = 0
148000081119     C                   MOVE      '29'          PARAMGF
148100081119     c                   end
148200081119      *
148300081119     c                   else
148400081119      *
148500081119      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
148600081119     c                   If           Mm_2 = 4 or Mm_2 = 6 or
148700081119     c                                Mm_2 = 9 or Mm_2 = 11
148800081119     C                   MOVE      '30'          PARAMGF
148900081119     c                   else
149000081119     C                   MOVE      '31'          PARAMGF
149100081119     c                   end
149200081119      *
149300081119     c                   endIf
149400081119     C*
149500081119     C*----------------------- F I N E    M E S E ---------------------------*
149600081119     C*
149700980902     C*
149800980902     C* VARIABILI RIFERITE AL DATABASE
149900980902     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
150000980902     C     *LIKE         DEFINE    TBLCOD        KTBCOD
150100980902     C     *LIKE         DEFINE    TBLKEY        KTBKEY
150200980902     C     *LIKE         DEFINE    SACCMM        KSACMM                         *TFSAC00F
150300980902     C     *LIKE         DEFINE    SACAAA        KSAAAA
150400980902     C     *LIKE         DEFINE    SACMMM        KSAMMM
150500980902     C     *LIKE         DEFINE    CLPKUT        KLPKUT                         *CNCLP00F
150600980902     C     *LIKE         DEFINE    CLPKCC        KLPKCC
150700020328     C     *LIKE         DEFINE    CLPKSC        KLPKSC
150800980902     C     *LIKE         DEFINE    CLPAGE        KLPAGE
150900990610     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC00F
151000980902     C     *LIKE         DEFINE    SDCANN        KSCANN
151100980902     C     *LIKE         DEFINE    SDCMES        KSCMES
151200050408     C     *LIKE         DEFINE    REPDIV        KEPDIV                         *WFREP10F
151300980902     C     *LIKE         DEFINE    REPARE        KEPARE
151400980902     C     *LIKE         DEFINE    REPFIL        KEPFIL
151500980902     C     *LIKE         DEFINE    REPRGF        KEPRGF
151600050607     C     *LIKE         DEFINE    REPdip        depdip
151700050607     C     *LIKE         DEFINE    REPsoc        depsoc
151800980902     C     *LIKE         DEFINE    REPANN        KEPANN
151900980902     C     *LIKE         DEFINE    REPMES        KEPMES
152000980902     C     *LIKE         DEFINE    REPRGF        DEPRGF                         *DI DEPOSITO
152100980902     C     *LIKE         DEFINE    REPDIV        DEPDIV
152200980902     C     *LIKE         DEFINE    REPARE        DEPARE
152300980902     C     *LIKE         DEFINE    REPFIL        DEPFIL
152400980902     C     *LIKE         DEFINE    REPDRG        DEPDRG
152500980902     C     *LIKE         DEFINE    REIPCF        DEIPCF
152600980902     C     *LIKE         DEFINE    REEPCF        DEEPCF
152700980902     C*
152800980902     C* CHAIVI DI LETTURA
152900980902     C     KEYTAB        KLIST                                                  *TABEL00F
153000980902     C                   KFLD                    KTBKUT                          -UTENTE
153100980902     C                   KFLD                    KTBCOD                          -TABELLA
153200980902     C                   KFLD                    KTBKEY                          -CHIAVE
153300980902     C     KE2TAB        KLIST                                                  *TABEL00F
153400980902     C                   KFLD                    KTBKUT                          -UTENTE
153500980902     C                   KFLD                    KTBCOD                          -TABELLA
153600980902     C     KEYSAC        KLIST                                                  *TFSAC00F
153700980902     C                   KFLD                    KSACMM                          -COMMERCIALE
153800980902     C                   KFLD                    KSAAAA                          -ANNO
153900980902     C                   KFLD                    KSAMMM                          -MESE
154000980902     C     KE2SAC        KLIST                                                  *TFSAC00F
154100980902     C                   KFLD                    KSACMM                          -COMMERCIALE
154200020516     C     KEYCLP        KLIST                                                  *CNCLP01L
154300980902     C                   KFLD                    KLPKUT                          -UTENTE
154400980902     C                   KFLD                    KLPKCC                          -CAPOCONTO
154500020328     C                   KFLD                    KLPAGE                          -AGENTE
154600020516     C     KEYCLP2       KLIST                                                  *CNCLP00F
154700020328     C                   KFLD                    KLPKUT                          -UTENTE
154800020328     C                   KFLD                    KLPKCC                          -CAPOCONTO
154900020328     C                   KFLD                    KLPKSC                          -CLIENTE
155000990610     C     KEYSDC        KLIST                                                  *SISDC00F
155100980902     C                   KFLD                    KSCKSC                          -CODICE CLIENTE
155200980902     C                   KFLD                    KSCANN                          -ANNO
155300980902     C                   KFLD                    KSCMES                          -MESE
155400990610     C     KE2SDC        KLIST                                                  *SISDC00F
155500980902     C                   KFLD                    KSCKSC                          -CODICE CLIENTE
155600050408     C     KEYREP        KLIST                                                  *WFREP10F
155700980902     C                   KFLD                    KEPDIV                          -DIVISIONE
155800980902     C                   KFLD                    KEPARE                          -AREA
155900980902     C                   KFLD                    KEPFIL                          -FILIALE
156000980902     C                   KFLD                    KEPRGF                          -AGENTE
156100980902     C                   KFLD                    KEPANN                          -ANNO
156200980902     C                   KFLD                    KEPMES                          -MESE
156300980902     C*
156400980902     C* VARIABILI DI CONTROLLO
156500980902     C                   MOVEL     'N'           $FINE             1            *FINE PGM
156600980902     C                   MOVEL     'N'           $FSAC             1            *FINE X TFSAC
156700980902     C                   MOVEL     'N'           $FCLP             1            *FINE X CNCLP
156800990610     C                   MOVEL     'N'           $FSDC             1            *FINE X SISDC
156900980902     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
157000980902     C                   MOVEL     '1'           *IN01                          *OVERFLOW
157100020402     C                   Z-ADD     *ZEROS        R                 3 0          *PER RAGGRUPP. REG.
157200020402     C                   Z-ADD     *ZEROS        P                 4 0          *FASCE DI PERO
157300980902     C*
157400980902     C* CARICAMENTO TABELLE OCCORRENTI
157500980902     C                   EXSR      CARTAB
157600980902     C*
157700980902     C* CARICAMENTO MESI/ANNI DA ELABORARE
157800980902     C                   EXSR      CARMES
157900980902     C*
158000980902     C* CANELLA IL WFILE PER IL PERIODO RICHIESTO
158100980902     C                   EXSR      CANREP
158200980902     C*
158300980902     C                   ENDSR
158400160304      /free
158500160304       //--------------------------------------------------------------
158600160304       //?Reperimento Dati del job (Utente/Operativi).                 ?
158700160304       //--------------------------------------------------------------
158800160304       BEGSR  sr_DatiJob;
158900160304
159000160304         in(E)  §AzUte;
159100160304         if  NOT  %error;
159200160304           in(E)  §DatiUte;
159300160304         endif;
159400160304         if  %error  or  RSut = *blanks;
159500160304           clear  TIBS34ds;
159600160304           tibs34r ( tibs34ds );
159700160304           in  §AzUte;
159800160304           in  §DatiUte;
159900160304         endif;
160000160304
160100160304       ENDSR;
160200160304
160300160304       //--------------------------------------------------------------
160400160304       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
160500160304       //--------------------------------------------------------------
160600160304       BEGSR  sr_PrintErr;
160700160304
160800160304         // -?Stampa del Dump?
160900160304         Dump(A);
161000160304
161100160304         // -?Stampa del Job-Log?
161200160304         Qcmd = 'DSPJOBLOG job(*) output(*print)';
161300160304         exsr  sr_ExecCmd;
161400160304
161500160304         //// -?Chiusura del programma?
161600160304         ////*inH1 = *on;
161700160304         ////$Fine = 'S';
161800160304
161900160304       ENDSR;
162000160304
162100160304       //--------------------------------------------------------------
162200160304       //?Esecuzione del comando (già impostato).                      ?
162300160304       //--------------------------------------------------------------
162400160304       BEGSR  sr_ExecCmd;
162500160304
162600160304         clear Qcap0100;
162700160304         Qcabcsdh = *off;
162800160304         Qcapa    = *off;
162900160304         Qcacmdss = *off;
163000160304         Qcaerved = *allX'00';
163100160304
163200160304         clear Qusec;
163300160304         Qusbprv  = %size(Qusec);
163400160304
163500160304         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
163600160304                           %size(Qcap0100) : 'CPOP0100' : *omit :
163700160304                           0 : 0 : Qusec);
163800160304
163900160304         //if  Qusei <> *blank;
164000160304         //  exsr  sr_PrintErr;
164100160304         //endif;
164200160304
164300160304       ENDSR;
164400160304
164500160304      /end-free
