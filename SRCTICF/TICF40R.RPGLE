000100000000     H*------------------------------------------------------------------------*
000200060809     H* STATISTICA DELTA BOLLE  - GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500160929     Ftitas39c  IF   E           K DISK
000600130226     FTNTAM00L  IF   E           K DISK
000700990820     FTITAD04L  IF   E           K DISK
000800970418     FTABEL00F  IF   E           K DISK
000900020301     FAZORG01L  IF   E           K DISK
001000131007     FECCET30C  IF   E           K DISK
001100131007     FECCED30C  IF   E           K DISK
001200000329     FWFDBF01L  O    E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600970418     D*-------------------
001700970418     D* MESSAGGI
001800970418     D*-------------------
001900970418     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000970418     D*-------------------
002100970418     D* DATI TARIFFA
002200970418     D*-------------------
002300060222     D SSGL            S             13  3 DIM(200)                             *SCAGLIONI
002400050103     D CTS             S              2    DIM(600)                             *CODICI TASSAZIONE
002500020301     D*-------------------
002600020301     D* SCHIERA FILIALI FEDEX
002700020301     D*-------------------
002800020301     D FILFED          S              3  0 DIM(999)
002801170508     D*-------------------
002802170508     D* SCHIERA FILIALI DPD
002803170508     D*-------------------
002804170508     D FILDPD          S              3  0 DIM(999)
002900000000     D*------------------------------------------------------------------------*
003000000000     D* INPUT
003100000000     D*------------------------------------------------------------------------*
003200970418     D*-------------------
003300970418     D* DS CODICI TASSAZIONE
003400970418     D*-------------------
003500970418     D DSCT          E DS
003600131007     D*-------------------
003700131007     D DS3A          E DS
003800131007     D*-------------------
003900131007     D DSTB          E DS
004000021202     D*-------------------
004100021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
004400970415     D*-------------------
004500130507     D* DS PASSAGGIO PARAMETRI -TNTE50R-
004600970415     D*-------------------
004700970415     D DS50          E DS                  EXTNAME(TNTE50DS)
004800000000     D*-------------------
004900970415     D* DS SCOMPOSIZIONE DATA
005000000000     D*-------------------
005100000000     D                 DS                  INZ
005200970418     D  DSAAS                  1      4  0
005300970418     D  DSMES                  5      6  0
005400970418     D  DSGIO                  7      8  0
005500970418     D  DSMGS                  5      8  0
005600970418     D  DSDAT                  1      8  0
005700970513     D*-------------------
005800970513     D* INFDS - INFORMAZIONI PROGRAMMA
005900970513     D*-------------------
006000970513     D                SDS
006100981001     D  JOB                  244    253
006200970513     D  USER                 254    263
006300000000     D*-------------------
006400970418     D* DS PARAMETRI DI LANCIO
006500000000     D*-------------------
006600950703     D PARAM           DS
007800160929     d  parlnp                 1      3  0
008400160929     d  pards1                 4     11  0
008500160929     d  pards2                12     19  0
010100160929     d  parksc                20     26  0
010200160929     d  parctr                27     29  0
010300160929     d  parprg                30     32  0
010500160929     d  pardtcal              33     33
010600160929     d  pardet                34     34
010900130226
011000970418     D*-------------------
011100970418     D* ARCHIETTURA
011200970418     D*-------------------
011300970418     D KPJBA         E DS
011400020301     D*-------------------
011500020301     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
011600020301     D*-------------------
011700020301     D OG143         E DS
011800020911     D*-------------
011900020911     D* DS DI WRK
012000020911     D*-------------
012100060809     D TIBS02DS      E DS                                                       *Ricerca tabella
012200060809     D*------------------
012300060809     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
012400060809     D*-------------------
012500060809     D DUTE01        E DS                  INZ                                  *Abilitazioni utenti
012600060809     D*------------------
012700060809     D* DS FUNZIONI AZIENDALI
012800060809     D*-------------------
012900060809     D DLAT          E DS                  INZ                                  *Funzioni aziendali
013000060809     D TRUL31DS      E DS                  INZ
013100060809     D  S31                   10    759  0 dim(250)
013200020911     D TIBS34DS      E DS                                                       *Profili utente
013300020911     D DDATIUTE      E DS                                                       *Dati utente
013400020911     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
013500020911     D*-------------
013600020911     D* FILIALI IN GESTIONE
013700020911     D*-------------
013800060904     D l1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
013900070530     D wpes            S                   like(taspkf)                         *FILIALI IN GESTIONE
014000070530     D wvol            S                   like(tasvlf)                         *FILIALI IN GESTIONE
014001160929     d KbpAAS          s                   like(TASAAS)
014002160929     d KbpLNP          s                   like(TASLNP)
014003160929     d KbpMGS          s                   like(TASMGS)
014100131007     d KCTAAS          s                   like(TASAAS)
014200131007     d KCTLNP          s                   like(TASLNP)
014300131007     d KCTNRS          s                   like(TASNRS)
014400131007     d KCTNSP          s                   like(TASNSP)
014500131007     d KCTTBL          s                   like(TASTBL)
014501160930     d ParDDT          s              8
014600130226
014700130226       // -?Campi Data
014800130226     d wOggi           s              8s 0 inz
014900130226
015000160929     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS39C
015100060809     ITITAS000      01
015200060809     ITITAS010      02
015201140521     ITITASP00      03
015300000000     C*------------------------------------------------------------------------*
015400000000     C* MAIN LINE
015500000000     C*------------------------------------------------------------------------*
015600000000     C*
015700000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
015800000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
015900020911     C*
016400000000     C*
016500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
016600000000     C                   EXSR      SETFIL
016700000000     C*
016800000000     C* CICLO FINO A FINE FILE
016900000000     C     FINL00        DOWEQ     '0'                                          --- 2 -->
017000970415     C*
017100970415     C* LANCIA PGM CALCOLO RICAVI/COMPETENZE
017200970415     C                   EXSR      LANCIA
017300000000     C*
017400000000     C* CREA IL RECORD SUI FILE DI LAVORO
017500000000     C                   EXSR      CREARE
017600000000     C*
017700000000     C* LEGGE IL RECORD SUCCESSIVO
017800000000     C                   EXSR      LETFIL
017900020911     C                   ENDDO                                                  <-- 2 ---
018000020911     C*
018500020911     C                   ENDIF                                                  <-- 1 ---
018600000000     C*
018700970418     C                   MOVEL     'C'           D50TLA
018800130507     C                   CALL      'TNTE50R'
018900970418     C                   PARM                    DS50
019000970418     C*
019100000000     C                   SETON                                        LR
019200000000     C*------------------------------------------------------------------------*
019300000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
019400000000     C*------------------------------------------------------------------------*
019500000000     C     SETFIL        BEGSR
019501160929     c
019502160929     c                   eval      kbplnp = parlnp
019503160929     c                   eval      kbpaas = waasi
019504160929     c                   eval      kbpmgs = wmgsi
019600000000     C*
019700000000     C* POSIZIONAMENTO E PRIMA LETTURA
019800160929     C     keytas        SETLL     titas39c                           99        *FINE FILE
019900000000     C*
019930160929      *
020000000000     C     *IN99         IFEQ      '1'
020100000000     C                   MOVEL     '1'           FINL00
020200000000     C                   ELSE
020300000000     C                   EXSR      LETFIL
020400000000     C                   END
020500000000     C*
020600000000     C                   ENDSR
020700000000     C*------------------------------------------------------------------------*
020800000000     C* LETFIL - LETTURA PROSSIMO RECORD
020900000000     C*------------------------------------------------------------------------*
021000000000     C     LETFIL        BEGSR
021100000000     C*
021200000000     C* LEGGO FINO A:
021300000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
021400000000     C*     - TROVATO RECORD VALIDO
021500000000     C                   MOVEL     'N'           WRECOK
021600000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
021700000000     C     WRECOK        ANDEQ     'N'
021800000000     C*
021900060809     c                   setoff                                       0102
022000000000     C* LETTURE SUCCESSIVE ALLA PRIMA
022100160929     C     kbplnp        READe     titas39c                               99
025400000000     C*
025401160929     c* imposto il peso con regola CML
025402160929IF  2C     tasPKC        IFGT      *ZEROS
025403160929     C     tasNCL        ANDEQ     tasNCP
025404160929     C                   Z-ADD     tasPKC        Wpes
025405160929X   2C                   ELSE
025406160929     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
025407160929IF  3C     tasPKF        IFGT      tasPKC
025408160929     C                   Z-ADD     tasPKF        Wpes
025409160929X   3C                   ELSE
025410160929     C                   Z-ADD     tasPKC        Wpes
025411160929E   3C                   ENDIF
025412160929E   2C                   ENDIF
025413160929     c* imposto il volume con regola CML
025414160929IF  1C     tasFVF        IFEQ      'T'
025415160929     C                   Z-ADD     tasVLF        Wvol
025416160929X   1C                   ELSE
025417160929     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
025418160929IF  2C     tasVLC        IFGT      *ZEROS
025419160929     C     tasNCL        ANDEQ     tasNCR
025420160929     C                   Z-ADD     tasVLC        Wvol
025421160929X   2C                   ELSE
025422160929     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
025423160929IF  3C     tasVLF        IFGT      tasVLC
025424160929     C                   Z-ADD     tasVLF        Wvol
025425160929X   3C                   ELSE
025426160929     C                   Z-ADD     tasVLC        Wvol
025427160929E   3C                   ENDIF
025428160929E   2C                   ENDIF
025429160929E   1C                   ENDIF
025430160929      ***
025500000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
025600000000     C                   MOVEL     '1'           FINL00
025700000000     C                   ELSE
025800000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
025900000000     C                   END
026000000000     C*
026100000000     C                   END                                                    <-- 1 ---
026200000000     C*
026300000000     C                   ENDSR
026400000000     C*------------------------------------------------------------------------*
026500000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
026600000000     C*------------------------------------------------------------------------*
026700000000     C     CHKREC        BEGSR
026800000000     C*
026900000329     C                   MOVEL     'S'           WRECOK            1
029700060904     C* CONTROLLO DATA
029800060904     C                   Z-ADD     tasAAS        DSAAS
029900060904     C                   Z-ADD     tasMGS        DSMGS
030000060904     C     DSDAT         IFGE      PARDS1
030100060904     C     DSDAT         ANDLE     PARDS2
030200060904     C                   ELSE
030300060904     C                   MOVEL     'N'           WRECOK
030400060904     C                   ENDIF
041900020301     C* VERIFICO SE LNP/LNA E' FILIALE FEDEX
042000060809     C                   IF        tasLNP > *zeros
042100060809     C     tasLNP        LOOKUP    FILFED                                 55
042200020301     C                   IF        *IN55 = *ON
042300020301     C                   MOVEL     'N'           WRECOK
042400020301     C                   ENDIF
042500020301     C                   ENDIF
042600060809     C                   IF        tasLNA > *zeros
042700060809     C     tasLNA        LOOKUP    FILFED                                 55
042800020301     C                   IF        *IN55 = *ON
042900020301     C                   MOVEL     'N'           WRECOK
043000020301     C                   ENDIF
043100020301     C                   ENDIF
043200020301     C*
043201170508     C* VERIFICO SE LNA E' FILIALE DPD   PRENDO SOLO BOLLE DIRETTE A DPD
043208170508     C                   IF        tasLNA > *zeros
043209170508     C     tasLNA        LOOKUP    FILDPD                                 55
043210170508     C                   IF        *IN55 = *OFF
043211170508     C                   MOVEL     'N'           WRECOK
043212170508     C                   ENDIF
043213170508     C                   ENDIF
043214170508     C*
043300000000     C                   ENDSR
043400000000     C*------------------------------------------------------------------------*
043500970415     C* LANCIA - LANCIA PGM CHE RESTITUISCE RICAVI E COMPETENZE
043600000000     C*------------------------------------------------------------------------*
043700970415     C     LANCIA        BEGSR
043800000000     C*
043900970416     C     $ERR          IFEQ      '0'
044000970415     C                   MOVEL     *BLANKS       D50TLA
044100970415     C                   MOVEL     'P'           D50TLA
044200060809     C                   Z-ADD     tasAAS        D50AAS
044300060809     C                   Z-ADD     tasLNP        D50LNP
044400060809     C                   Z-ADD     tasNRS        D50NRS
044500060809     C                   Z-ADD     tasNSP        D50NSP
044600970415     C                   MOVEL     *BLANKS       D50CTO
044700130226     C                   Z-ADD     PARKsc        D50KSC
044800130226     C                   Z-ADD     PARctr        D50CTR
044900130226     C                   Z-ADD     parprg        D50PRG
045000130226     c                   clear                   D50dtr
045100130226     c                   IF        pardtcal = 'O'
045200130226     c                   eval      D50dtr = wOggi
045300130226     c                   ENDIF
045400970415     C                   Z-ADD     *ZEROS        D50RIC
045500970415     C                   Z-ADD     *ZEROS        D50COM
045600970415     C                   Z-ADD     *ZEROS        D50DEL
045700970415     C                   Z-ADD     *ZEROS        D50PVL
045800970416     C                   MOVEL     *BLANKS       D50ERR
045900970416     C                   MOVEL     *BLANKS       D50MSG
046000130507     C                   CALL      'TNTE50R'
046100970415     C                   PARM                    DS50
046200130228     c                   parm                    parddt
046300970415     C                   ENDIF
046400000000     C*
046500000000     C                   ENDSR
046600970415     C*------------------------------------------------------------------------*
046700970415     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
046800970415     C*------------------------------------------------------------------------*
046900970415     C     CREARE        BEGSR
047000970415     C*
047100981001     C                   MOVEL     JOB           WFDUSR
047200060809     C                   Z-ADD     tasAAS        WFDAAS
047300060809     C                   Z-ADD     tasMGS        WFDMGS
047400060809     C                   Z-ADD     tasLNP        WFDLNP
047500060809     C                   Z-ADD     tasNRS        WFDNRS
047600060809     C                   Z-ADD     tasNSP        WFDNSP
047700060809     C                   Z-ADD     tasKSC        WFDKSC
047800970418     C* CODICE TASSAZIONE
047900060809     C                   MOVEL     tasCTS        WCTSI
048000970418     C                   EXSR      CERCTS
048100970418     C                   MOVEL     WCTSO         WFDCTS
048200970418     C* SCAGLIONE
048300130226     C                   MOVEL     PARctr        WTIP              1            *TIPO TARIFFA
048400000721     C     WTIP          IFEQ      '0'                                          *A QUINTALI
048500000721     C     WTIP          OREQ      '3'                                          *A KILOGRAMMI
048600070530     C                   Z-ADD     wpes          WSGL             13 3
048700000721     C                   ELSE
048800060809     C                   Z-ADD     tasNCL        WSGL
048900000721     C                   ENDIF
049000970418     C                   DO        ULTSGL        I
049100000721     C     WSGL          IFLE      SSGL(I)
049200970416     C                   Z-ADD     SSGL(I)       WFDSGL
049300970415     C                   LEAVE
049400970415     C                   ENDIF
049500970415     C                   ENDDO
049600970707     C     D50COM        IFEQ      *ZEROS
049700970707     C                   Z-ADD     *ZEROS        WFDPKG
049800970707     C                   Z-ADD     *ZEROS        WFDNCL
049900970707     C                   Z-ADD     *ZEROS        WFDVLB
050000970707     C                   Z-ADD     *ZEROS        WFDQFT
050100970707     C                   ELSE
050200070530     C                   Z-ADD     wpes          WFDPKG
050300070530     C                   Z-ADD     wvol          WFDVLB
050400070528     C                   Z-ADD     tasNCL        WFDNCL
050500060809     C                   Z-ADD     tasQFT        WFDQFT
050600970707     C                   ENDIF
050700970415     C                   Z-ADD     D50RIC        WFDRIC
050800970415     C                   Z-ADD     D50COM        WFDCOM
050900970415     C                   Z-ADD     D50DEL        WFDDEL
051000970415     C                   MOVEL     D50MSG        WFDERR
051100131007    1c                   if        wfderr <> *blanks
051200131007     c                   eval      %subst(wfderr: 72: 2) =  tastbl
051300131007      * se dal programma TNTE50R è tornato un errore ed il tipo di errore è uguale a NONC cioè
051400131007      * bolla non calcolabile mi carico le competenze dai file ECCET / ECCED
051500131007    2c                   if        %subst(d50msg: 75: 4) = 'NONC'
051600131007     c                   exsr      Tot_compe
051700131007     C                   Z-ADD     TOTCOMPE      WFDCOM
051800131007      * SE RAGGRUPPAMENTO BOLLA DIVERSO DA CONTO SERVIZIO SOMMO IMPONIBILE IVA NEI TOTALI RICAVI
051900131007     C                   Z-ADD     CODUT         KTBKUT
052000131007     C                   MOVEL     '3A'          KTBCOD
052100131007     C                   MOVEL     *BLANKS       KTBKEY
052200131007     C                   MOVEL     tasCBO        KTBKEY
052300131007     C     KEYTAB        CHAIN     TABEL00F                           99
052400131007    3C                   IF        %FOUND(TABEL00F)
052500131007     C                   MOVEL     TBLUNI        DS3A
052600131007     C                   MOVEL     'TB'          KTBCOD
052700131007     C                   MOVEL     *BLANKS       KTBKEY
052800131007     C                   MOVEL     §3ATB1        KTBKEY
052900131007     C     KEYTAB        CHAIN     TABEL00F                           99
053000131007    4C                   IF        %FOUND(TABEL00F)
053100131007     C                   MOVEL     TBLUNI        DSTB
053200131007    5C     §tbrbl        IFNE      'C'
053300131007     C                   Z-ADD     tasimv        WFDric
053400131007    5c                   endif
053500131007    4c                   endif
053600131007    3c                   endif
053700131007    2c                   endif
053800131007    1c                   endif
053900970513     C                   WRITE     WFDBF000
054000970415     C*
054100970415     C                   ENDSR
054200970418     C*------------------------------------------------------------------------*
054300970418     C* CERCTS - CERCA IL CTS DELLA TARIFFA
054400970418     C*------------------------------------------------------------------------*
054500970418     C     CERCTS        BEGSR
054600970418     C*
054700970418     C* CONTROLLA CON CODICE TASSAZIONE ELEMENTARE
054800970418     C                   MOVEL     *BLANKS       WCTSO
054900970418     C                   DO        ULTCTS        I
055000970418     C     WCTSI         IFEQ      CTS(I)
055100970418     C                   MOVEL     WCTSI         WCTSO
055200970418     C                   ENDIF
055300970418     C                   ENDDO
055400970418     C*
055500970418     C* SE NON TROVATO, CONTROLLA CON CODICE TASSAZIONE REGIONALE
055600970418IF  1C     WCTSO         IFEQ      *BLANKS
055700970418     C                   MOVEL     CODUT         KTBKUT
055800970418     C                   MOVE      'CT'          KTBCOD
055900970418     C                   MOVE      *BLANKS       KTBKEY
056000970418     C                   MOVEL     WCTSI         KTBKEY
056100970418     C     KEYTAB        CHAIN     TABEL00F                           99
056200970418IF  2C     *IN99         IFEQ      '1'
056300970418     C     TBLFLG        OREQ      '*'
056400970418     C                   MOVEL     'TE56'        D50ERR
056500011102     C                   MOVEL     MSG(1)        D50MSG                         *CTS NON TROV.TAB.CT
056600970418     C                   MOVEL     '1'           $ERR
056700970418X   2C                   ELSE
056800970418     C                   MOVEL     TBLUNI        DSCT
056900970418     C                   DO        ULTCTS        I
057000970418     C     §CTRAP        IFEQ      CTS(I)
057100970418     C                   MOVEL     §CTRAP        WCTSO
057200970418     C                   ENDIF
057300970418     C                   ENDDO
057400970418E   2C                   ENDIF
057500970418E   1C                   ENDIF
057600970418     C*
057700970418     C* SE NON TROVATO, CONTROLLA CON CODICE TASSAZIONE NAZIONALE
057800970418IF  1C     WCTSO         IFEQ      *BLANKS
057900970418     C     $ERR          OREQ      '1'
058000970418IF  2C     §CTNAR        IFEQ      *BLANKS
058100970418     C                   MOVEL     'IT'          WNAZ              2
058200970418X   2C                   ELSE
058300970418     C                   MOVEL     'EE'          WNAZ
058400970418E   2C                   ENDIF
058500970418     C                   DO        ULTCTS        I
058600970418     C     WNAZ          IFEQ      CTS(I)
058700970418     C                   MOVEL     WNAZ          WCTSO
058800970418     C                   ENDIF
058900970418     C                   ENDDO
059000970418E   1C                   ENDIF
059100970418     C*
059200970418     C                   ENDSR
059300970415     C*------------------------------------------------------------------------*
059400970415     C* CARSGL - CARICA GLI SCAGLIONI
059500970415     C*------------------------------------------------------------------------*
059600970415     C     CARSGL        BEGSR
059700011102     C*
059800011102     C* AZZERA VARIABILI DI LAVORO
059900011102     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO NO TROVAT
060000011102     C                   Z-ADD     *ZEROS        I                 4 0
060100011102     C                   Z-ADD     *ZEROS        C                 4 0
060200011102     C*---
060300130226     C* CERCA IL DETTAGLIO TARIFFARIO
060400130226     C                   Z-ADD     PARKsc        KTDKSC
060500130226     C                   Z-ADD     PARctr        KTDCTR
060600130205     C                   Z-ADD     parPRG        KTDPRG
060700130226      /free
060900130226          setll (KTDksc:KTDctr:KTDprg) TITAD04L;
061000130226          reade (KTDksc:KTDctr:KTDprg) TITAD04L;
061500130226        DOW  not %eof(TITAD04L);
061600130226      /end-free
061700011102     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
061800011102     C                   EXSR      MEMTAD                                       *MEMORIZZA DATI
061900130226      /free
062100130226          reade (KTDksc:KTDctr:KTDprg) TITAD04L;
062500130226      /end-free
062600011102E   1C                   ENDDO                                                  *FINE DETTAGLIO
062601160929     c* imposto data decorrenza tariffa
062602160929     c     KEYTAM        CHAIN     TNTAM00L
062603160929     C                   If        %found(tntam00l)
062604160930     c                   movel     tamddt        parddt
062605160929     c                   endif
062700011102     C*
062800011102     C* ELEMENTI MEMORIZZATI
062900011102     C                   Z-ADD     I             ULTSGL            6 0
063000011102     C                   Z-ADD     C             ULTCTS            6 0
063100011102     C*
063200011102     C* DETTAGLIO NON TROVATO, ESCE DAL PGM
063300011102IF  1C     $TAD          IFEQ      '0'                                          *DETT.NON TROVATO
063400011102     C                   MOVEL     '1'           FINL00                         *FINE PGM
063500011102E   1C                   ENDIF
063600011102     C*
063700970415     C                   ENDSR
063800011102     C*------------------------------------------------------------------------*
063900011102     C* MEMORIZZA DETTAGLIO TARIFFARIO
064000011102     C*------------------------------------------------------------------------*
064100011102     C     MEMTAD        BEGSR
064200011102     C*
064300011102     C* SCAGLIONI
064400011102     C                   SETOFF                                           98
064500011102     C     TADSGL        LOOKUP    SSGL                                   98
064600011102IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
064700011102     C                   ADD       1             I
064800011102     C                   Z-ADD     TADSGL        SSGL(I)
064900011102E   1C                   ENDIF
065000011102     C* CODICI TASSAZIONE
065100011102     C                   SETOFF                                           98
065200011102     C     TADCTS        LOOKUP    CTS                                    98
065300011102IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
065400011102     C                   ADD       1             C
065500011102     C                   MOVEL     TADCTS        CTS(C)
065600011102E   1C                   ENDIF
065700011102     C*
065800011102     C                   ENDSR
065900011102     C*------------------------------------------------------------------------*
066000011102     C* CARICA LE TABELLE OCCORRENTI
066100011102     C*------------------------------------------------------------------------*
066200011102     C     CARTAB        BEGSR
066300020911     C*
066400020911     C* REPERISCE I DATI UTENTE
066500020911     C                   EXSR      REPDATIUTE
066600020301     C*
066700170508     C* CARICA FILIALI FEDEX  e DPD
066800020301     C                   Z-ADD     *ZEROS        K                 3 0
066801170508     C                   Z-ADD     *ZEROS        X                 3 0
066900020301     C     *LOVAL        SETLL     AZORG01L
067000020301     C                   READ      AZORG01L                               98
067100020301DO  1C     *IN98         DOWEQ     *OFF
067200020301IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
067300020301     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
067400020301     C* VERIFICO SE FILIALE FEDEX
067500020301     C                   MOVEL     ORGDE3        OG143
067600020301     C                   IF        §OGNTW = 'FED'
067700020301     C                   ADD       1             K
067800020301     C                   Z-ADD     ORGFIL        FILFED(K)
067900020301     C                   ENDIF
067901170508     C                   IF        §OGNTW = 'DPD'
067902170508     C                   ADD       1             X
067903170508     C                   Z-ADD     ORGFIL        FILDPD(X)
067904170508     C                   ENDIF
068000020301E   2C                   ENDIF
068100020301     C                   READ      AZORG01L                               98
068200020301E   1C                   ENDDO
068300011102     C*
068400011102     C                   ENDSR
068500020911     C*--------------------------------------------------------------------------------------------*
068600020911     C* REPERISCE I DATI UTENTE
068700020911     C*--------------------------------------------------------------------------------------------*
068800020911     C     REPDATIUTE    BEGSR
068900020911     C*
069000020911     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
069100020911     C                   CLEAR                   L1
069200020911     C                   CLEAR                   TIBS34DS
069300020911     C                   CLEAR                   AZUTEDS
069400020911     C                   CLEAR                   DDATIUTE
069500060809     C                   CLEAR                   WABI              2
069600060809     C                   CLEAR                   DLAT
069700060809     C                   CLEAR                   werr              1
069900020911     C*
070000020911     C     *DTAARA       DEFINE    §azute        azuteds
070100020911     C     *DTAARA       DEFINE    §datiute      ddatiute
070200020911     C                   IN(E)     *DTAARA
070300020911if  1C                   IF        %Error
070400020911     c                   EVAL      I34Tla = 'L'
070500020911     C                   CALL      'TIBS34R'
070600020911     C                   PARM                    Tibs34Ds
070700020911     C                   IN        *DTAARA
070800020911e   1C                   ENDIF
071600020911     C*
071700060809     C                   SELECT
071800060809     C                   WHEN      DUTERR = 'E'
071900060809     C                   EVAL      WERR  = '1'
072000060809     C*
072100060809     C* NON siamo in sede (filiale remota o filiale locale)
072200060809     C* --> se 1° livello, abilitazioni al terminal
072300060809     C*     se 2° livello, abilitazioni al punto operativo
072400060809     C                   WHEN      UTEAUT = *blanks
072500060809     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
072600060809     C                   EVAL      WABI   = 'TP'
072700060809     C                   ENDIF
072800060809     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
072900060809     C                   EVAL      WABI   = 'PO'
073000060809     C                   ENDIF
073100060809     C*
073200060809     C* siamo in sede
073300060809     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
073400060809     C*     carica le abilitazioni proprie del profilo
073500060809     C                   OTHER
073600060809     C                   MOVEL     UTEFAF        DUTE01
073700060809     C                   IF        §UTEIST <> *blanks                           *esiste specifica
073800060809     C                   EVAL      WABI = §UTEIST
073900060809     C                   ELSE                                                   *NON esiste specific
074000060809     C                   EVAL      WABI = UTEAUT
074100060809     C                   ENDIF
074200060809     C                   ENDSL
074300060809     C*
074400060809     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
074500060809     C                   CLEAR                   TIBS02DS
074600060809     C                   MOVEL     'C'           T02MOD
074700060809     C                   MOVEL     KNSIF         T02SIF
074800060809     C                   MOVEL     'LAT'         T02COD
074900060809     C                   MOVEL(P)  WABI          T02KE1
075000060809     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
075100060809     C                   CALL      'TIBS02R'
075200060809     C                   PARM                    KPJBA
075300060809     C                   PARM                    TIBS02DS
075400060809     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
075500060809     C                   IF        T02ERR = *blanks
075600060809     C                   MOVEL     T02UNI        DLAT
075700060809     C                   ENDIF
075800060809     C                   IF        T02ERR <> *blanks OR
075900060809     C                             §LATABI = 'S'
076000060809     C                   EVAL      WERR  = '1'
076100060809     C                   ENDIF
076200060809     C*
076300060809     C* Reperisce i P.O. gestibili dall'utente
076400060809     C                   IF        WERR <> '1'                                  *NO errori
076500060809     C                   CLEAR                   TRUL31DS
076600060809     C                   EVAL      I31ABI   = WABI
076700060809     C                   EVAL      I31CDI   = DUTDIS
076800060809     C                   EVAL      I31CAR   = DUTARE
076900060809     C                   EVAL      I31CPO   = DUTPOU
077000060809     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
077100060809     C                   CALL      'TRUL31R'
077200060809     C                   PARM                    KPJBA
077300060809     C                   PARM                    TRUL31DS
077400060809     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
077500060809if  2C                   IF        O31POG > *zeros
077600060809     C                   MOVEA     s31           L1
077700060809x   2C                   ELSE
077800060809     C                   EVAL      WERR  = '1'
077900060809e   2C                   ENDIF
078000060809e   1C                   ENDIF
078200020911     C*
078300020911     C                   ENDSR
078400131007     C*------------------------------------------------------------------------*
078500131007     C* TOT_COMPE calcola le competenze delle bolle non calcolabili
078600131007     C*------------------------------------------------------------------------*
078700131007     C     Tot_Compe     BEGSR
078800131007     C
078900131007     C                   CLEAR                   TOTCOMPE         15 5
079000131007     C*
079100131007     C* LEGGE LE COMPETENZE FISSE
079200131007     C                   Z-ADD     TASAAS        KCTAAS                         *ANNO
079300131007     C                   Z-ADD     TASLNP        KCTLNP                         *LINEA DI PARTENZA
079400131007     C                   Z-ADD     TASNRS        KCTNRS                         *SERIE
079500131007     C                   Z-ADD     TASNSP        KCTNSP                         *SPEDIZIONE
079600131007     C                   MOVEL     TASTBL        KCTTBL                         *TIPO BOLLA
079700131007     C     KEYCET        CHAIN     ECCET30C                           99
079800131007IF  1C     *IN99         IFEQ      *OFF                                         *TROVATE
079900131007     C*
080000131007     C* CARICA LE COMPETENZE FISSE
080100131007     C                   ADD       CETPAR        TOTCOMPE
080200131007     C                   ADD       CETPAM        TOTCOMPE
080300131007     C                   ADD       CETPAU        TOTCOMPE
080400131007     C                   ADD       CETPAV        TOTCOMPE
080500131007     C                   ADD       CETARD        TOTCOMPE
080600131007     C                   ADD       CETARM        TOTCOMPE
080700131007     C                   ADD       CETARU        TOTCOMPE
080800131007     C                   ADD       CETARV        TOTCOMPE
080900131007     C                   ADD       CETTRL        TOTCOMPE
081000131007     C                   ADD       CETPAG        TOTCOMPE
081100131007     C                   ADD       CETSEA        TOTCOMPE
081200131007     C                   ADD       CETSEC        TOTCOMPE
081300131007     C                   ADD       CETRCV        TOTCOMPE
081400131007     C                   ADD       CETMDE        TOTCOMPE
081500131007     C                   ADD       CETTDE        TOTCOMPE
081600131007     C                   ADD       CETMAF        TOTCOMPE
081700131007     C                   ADD       CETTAF        TOTCOMPE
081800131007     C                   ADD       CETSES        TOTCOMPE
081900131007     C*
082000131007     C* LEGGE LE COMPETENZE VARIABILI
082100131007IF  2C     CETDET        IFEQ      'S'                                          *SE CI SONO
082200131007     C     KEYCET        SETLL     ECCED30C
082300131007     C     KEYCET        READE     ECCED30C                               99
082400131007DO  3C     *IN99         DOWEQ     *OFF
082500131007     C*
082600131007     C* CARICA LE COMPETENZE VARIABILI
082700131007IF  4C     CEDIMP        IFGT      *ZEROS
082800131007     C                   ADD       CEDIMP        TOTCOMPE
082900131007E   4C                   ENDIF
083000131007     C     KEYCET        READE     ECCED30C                               99
083100131007E   3C                   ENDDO
083200131007E   2C                   ENDIF
083300131007E   2C                   ENDIF
083400131007E   2
083500131007E   2C                   ENDSR
083600000000     C*------------------------------------------------------------------------*
083700000000     C* *INZSR - ROUTINE INIZIALE
083800000000     C*------------------------------------------------------------------------*
083900000000     C     *INZSR        BEGSR
084000000000     C*--------------------
084100000000     C* RICEVIMENTO PARAMETRI
084200000000     C*--------------------
084300000000     C     *ENTRY        PLIST
084400000000     C                   PARM                    KPJBA
084500950703     C                   MOVEL     KPJBU         PARAM
084600000000     C*--------------------
084700970418     C* VARIABILI RIFERITE AL DATA BASE
084800000000     C*--------------------
084900970418     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
085000970418     C     *LIKE         DEFINE    TBLCOD        KTBCOD
085100970418     C     *LIKE         DEFINE    TBLKEY        KTBKEY
085500060809     C     *LIKE         DEFINE    TAMKSC        KTMKSC
085600970415     C     *LIKE         DEFINE    TAMCTR        KTMCTR
085700130205     C     *LIKE         DEFINE    TAMPRG        KTMprg
085800060809     C     *LIKE         DEFINE    TADKSC        KTDKSC
085900970415     C     *LIKE         DEFINE    TADCTR        KTDCTR
086000970415     C     *LIKE         DEFINE    TADPRG        KTDPRG
086100060809     C     *LIKE         DEFINE    TADCTS        WCTSI
086200970418     C     *LIKE         DEFINE    TADCTS        WCTSO
086300011102     C     *LIKE         DEFINE    TAMKSC        CARKSC
086400011102     C     *LIKE         DEFINE    TAMCTR        CARCTR
086500011102     C     *LIKE         DEFINE    TAMPRG        CARPRG
086600060809     C     *LIKE         DEFINE    KPJBU         DEPKPJBU
086700000000     C*--------------------
086800000000     C* CHIAVI DI LETTURA
086900000000     C*--------------------
087000060809     C* CHIAVE LETTURA titas39c - PARZIALE
087100060809     C     KEYtas        KLIST
087200020911     C                   KFLD                    KBPLNP                         *LINEA DI PARTENZA
087300160929     C                   KFLD                    KBPAAS
087400160929     C                   KFLD                    KBPMGS
087500160929     C* LETTURA TNTAM00L - PARZIALE
087600970415     C     KEYTAM        KLIST
087700160929     C                   KFLD                    KTdKSC                         *CLIENTE
087800160929     C                   KFLD                    KTdCTR                         *CODICE TARIFFA
087900160929     C                   KFLD                    KTdprg                         *PROGR. TARIFFA
088000011102     C* LETTURA TITAD04L - PARZIAL
088100970415     C     KEYTAD        KLIST
088200970415     C                   KFLD                    KTDKSC                         *CLIENTE
088300970415     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
088400970415     C                   KFLD                    KTDPRG                         *PROGRESSIVO
088500970418     C* LETTURA TABEL00F - COMPLETA
088600970418     C     KEYTAB        KLIST
088700970418     C                   KFLD                    KTBKUT                         *UTENTE
088800970418     C                   KFLD                    KTBCOD                         *TABELLA
088900970418     C                   KFLD                    KTBKEY                         *CODICE
089000131007     C* LETTURA ECCET30C - COMPELTA
089100131007     C     KEYCET        KLIST
089200131007     C                   KFLD                    KCTAAS                         *ANNO
089300131007     C                   KFLD                    KCTLNP                         *LINEA DI PARTENZA
089400131007     C                   KFLD                    KCTNRS                         *SERIE
089500131007     C                   KFLD                    KCTNSP                         *SPEDIZIONE
089600131007     C                   KFLD                    KCTTBL                         *TIPO BOLLA
089700000000     C*--------------------
089800970418     C* VARIABILI DI CONTROLLO
089900000000     C*--------------------
090000000000     C                   Z-ADD     1             CODUT             1 0
090100970416     C                   MOVEL     '0'           FINL00            1
090200970416     C                   MOVEL     '0'           $ERR              1
090300970414     C*
090400970418     C* IMPOSTA LE DATE DI LANCIO
090500970415     C                   Z-ADD     PARDS1        DSDAT
090600970415     C                   Z-ADD     DSMGS         WMGSI             4 0
090700970415     C                   Z-ADD     DSAAS         WAASI             4 0
090800970415     C                   Z-ADD     PARDS2        DSDAT
090900970415     C                   Z-ADD     DSMGS         WMGSF             4 0
091000970415     C                   Z-ADD     DSAAS         WAASF             4 0
091100130226
091200130226      /free
091300130226       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
091400130226         wOggi = %dec(%date());
091500130226      /end-free
091600130226
091700011102     C*
091800011102     C* CARICA TABELLE OCCORRENTI
091900011102     C                   EXSR      CARTAB
092000970415     C*
092100160929     C* CARICA GLI SCAGLIONI DELLA TARIFFA E DTA DECORRENZA TARIFFA
092200970415     C                   EXSR      CARSGL
092300000000     C*
092400970415     C                   ENDSR
092500970415     O*------------------------------------------------------------------------*
092600011102** MSG - MESSAGGI
092700011102TNTE56R- Cpdice tassazione non trovato in tabella "CT".                        1
092800011102TNTE56R- 2                                                                     2
092900970415TNTE56R- 3                                                                     3
093000970415TNTE56R- 4                                                                     4
093100970415TNTE56R- 5                                                                     5
093200970415TNTE56R- 6                                                                     6
093300970415TNTE56R- 7                                                                     7
093400970415TNTE56R- 8                                                                     8
093500970415TNTE56R- 9                                                                     9
093600970415TNTE56R- 10                                                                   10
093700970415TNTE56R- 11                                                                   11
093800970415TNTE56R- 12                                                                   12
093900970415TNTE56R- 13                                                                   13
094000970415TNTE56R- 14                                                                   14
094100970415TNTE56R- 15                                                                   15
094200970415TNTE56R- 16                                                                   16
094300970415TNTE56R- 17                                                                   17
094400970415TNTE56R- 18                                                                   18
094500970415TNTE56R- 19                                                                   19
094600970415TNTE56R- 20                                                                   20
