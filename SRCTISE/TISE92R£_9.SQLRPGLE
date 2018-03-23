000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA DELTA - CREAZIONE WFILE DI STAMPA                           *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FAZORG01L  IF   E           K DISK
000900000824     FTABEL00F  IF   E           K DISK
001000020404     FTNTBE01L  IF   E           K DISK
001100990609     FSISDC01L  IF   E           K DISK
001200990609     FSISDO01L  IF   E           K DISK
001300070531     FWFDEL01L  UF A E           K DISK    usropn
001400070531     FWFDEL04L  UF   E           K DISK    usropn
001500000824     F                                     RENAME(WFDEL000:WFDEL4)
001600070723     FWFDEL05L  UF   E           K DISK    usropn
001700070723     F                                     RENAME(WFDEL000:WFDEL5)
001800070531     FWFdfpv1L  UF A E           K DISK    usropn
001900070531     FWFdfpv4L  UF   E           K DISK    usropn
002000070531     F                                     RENAME(WFDFPV00:WFDFPV)
002100070723     FWFdfpv5L  UF   E           K DISK    usropn
002200070723     F                                     RENAME(WFDFPV00:WFDFP5)
002300000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002400000000     D*------------------------------------------------------------------------*
002500000000     D* SCHIERE
002600000000     D*------------------------------------------------------------------------*
002700000000     D*------------------
002800000000     D* RIGHE DI STAMPA
002900000000     D*------------------
003000000000     D STA             S             66    DIM(5) CTDATA PERRCD(1)
003100000000     D*------------------
003200000000     D* COMMERCIALI IN ERRORE
003300000000     D*------------------
003400020314     D ERG             S              7  0 DIM(500)
003500000000     D*------------------
003600000000     D* ORGANIGRAMMA
003700000000     D*------------------
003800971124     D FIL             S              3  0 DIM(500)                             *FILIALI
003900971124     D DFIL            S             20    DIM(500)
004000971124     D ARE             S              3  0 DIM(500)                             *AREE
004100971124     D DARE            S             20    DIM(500)
004200971124     D DIV             S              1    DIM(500)                             *DIVISIONI
004300971124     D DDIV            S             20    DIM(500)
004400000000     D*------------------
004500000000     D* COMMERCIALI
004600000000     D*------------------
004700100202     D AGE             S              7  0 DIM(5000)                            *CODICI AGENTE
004800100202     D RGF             S              7  0 DIM(5000)                            *CODICI RAGGRUPPAMEN
004900100202     D DAGE            S             25    DIM(5000)                            *DECODIFICHE AGENTI
005000000000     D*------------------
005100000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
005200000000     D*------------------
005300000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
005400000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
005500000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
005600970821     D*------------------
005700970821     D* FASCE DI PESO
005800970821     D*------------------
005900020522     D FPE             S              2    DIM(11)
006000970821     D*------------------
006100970821     D* PERIODO DA ELABORARE
006200970821     D*------------------
006300990609     D ANN             S              4  0 DIM(12)                              *ANNO
006400000823     D MES             S              2  0 DIM(12)                              *MESE
006500000825     D*------------------
006600000825     D* MESI CON SCOSTAMENTO DELTA FASCE DA SEGNALARE
006700000825     D*------------------
006800000825     D SMEI            S              2  0 DIM(12)                              *ITALIA
006900000825     D SMEE            S              2  0 DIM(12)                              *ESTERO
007000000825     D SMET            S              2  0 DIM(12)                              *TOTALE
007100020314     D*------------------
007200020314     D* RAGGRUPPAMENTI REGIONI/NETWORK
007300020314     D*------------------
007400020315     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIONE
007500020315     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RAGGR. REGIONE
007600020404     D*------------------
007700020404     D* NETWORK AZIENDALI
007800020404     D*------------------
007900020404     D NTWCOD          S              3    DIM(100)                             *CODICI NETWORK
008000020404     D NTWORD          S              2  0 DIM(100)                             *PRIORITA' CONSIDERAZIONE NETWORK
008100000823     D*------------------
008200000823     D* DS CONTROLLO DATA (8)
008300000823     D*------------------
008400000823     D WLBDA8          DS                  INZ
008500000823     D  G08DAT                 1      8  0
008600000823     D  G08INV                 9     16  0
008700000823     D  G08ERR                17     17
008800000823     D  G08TGI                18     22  0
008900000823     D*------------------
009000000823     D* DS DI PROCEDURA
009100000823     D*------------------
009200000823     D BS10DS        E DS                  EXTNAME(TIBS10DS)
009300000000     D*------------------
009400000000     D* PARAMETRI DI LANCIO
009500000000     D*------------------
009600000000     D PARAM           DS
009700990609     D  PARDAT                 1      6  0
009800990609     D  PARAAI                 1      4  0
009900990609     D  PARMMI                 5      6  0
010000990609     D  PARDA2                 7     12  0
010100990609     D  PARAAF                 7     10  0
010200990609     D  PARMMF                11     12  0
010300990609     D  PARDCK                13     13
010400990609     D  PARDCC                14     14
010500990609     D  PARDCF                15     15
010600990609     D  PARFIC                16     18  0
010700990609     D  PARARC                19     21  0
010800990609     D  PARDIC                22     22
010900990609     D  PARDFF                23     23
011000990609     D  PARCOF                24     30  0
011100990609     D  PARDIF                31     31
011200990609     D  PARSTA                32     32
011300000824     D  PARSCE                33     33
011400990609     D  PARTSP                34     34
011500990609     D  PARIOE                35     35
011600990609     D  PARCLV                36     36
011700990609     D  PARDPT                37     42  0
011800990609     D  PARAPI                37     40  0
011900990609     D  PARMPI                41     42  0
012000990609     D  PARDP2                43     48  0
012100990609     D  PARAFP                43     46  0
012200990609     D  PARMFP                47     48  0
012300990609     D  PARSPD                49     52  1
012400990609     D  PARPDD                53     56  1
012500990609     D  PARPDA                57     60  1
012600000707     D  PARPOS                61     61
012700000825     D  PARCRE                62     62
012800010618     D  PARCL1                63     63
012900010618     D  PARCL2                64     64
013000010618     D  PARSIN                65     65
013100020404     D  PARCL3                66     66
013200020404     D  PARCL4                67     67
013300020404     D  PARNTW                68     68
013400030923     D  PARDFD                69     72  1
013500030923     D  PARDFA                73     76  1
013600030923     D  PARDFDP               77     80  1
013700030923     D  PARDFAP               81     84  1
013800030923     D  PARFPC                85     85
013900030923     D  PARWPC                86     86
014000030923     D  PARWCP                87     87
014100070531     D  PARWfdpv              88     88
014200000000     D*------------------
014300990609     D* DATA DI SISDC01L
014400000000     D*------------------
014500000000     D                 DS
014600990609     D  SDCANN                 1      4  0
014700990609     D  SDCMES                 5      6  0
014800990609     D  SDCDAT                 1      6  0
014900970821     D*------------------
015000970821     D* DATA DI WFDEL00F
015100970821     D*------------------
015200970821     D                 DS
015300970821     D  WFDANN                 1      4  0
015400970821     D  WFDMES                 5      6  0
015500990609     D  WFDDAT                 1      6  0
015600000000     D*------------------
015700000000     D* DS COMMERCIALI
015800000000     D*------------------
015900000000     D DS01          E DS
016000000000     D*------------------
016100000000     D* DS FASCE DI PESO
016200000000     D*------------------
016300000000     D DS2L          E DS
016400000000     D*------------------
016500000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
016600000000     D*------------------
016700000000     D DS2W          E DS
016800980403     D*------------------
016900980403     D* DS REPERIMENTO DATI UTENTE
017000980403     D*-------------------
017100980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
017200980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
017300980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
017400980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
017500980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
017600000000     D*------------------
017700000000     D* ARCHITETTURA
017800000000     D*------------------
017900000000     D KPJBA         E DS
018000020314     D*------------------
018100020314     D* DS DI LETTURA TABELLA "2M"
018200020314     D*------------------
018300020314     D DS2M          E DS
018400020404     D*-------------------
018500020404     D* DS DEFINIZIONE TABELLA "NETWORK"
018600020404     D*-------------------
018700020404     D DNTW          E DS
018800000000     C*------------------------------------------------------------------------*
018900000000     C* MAIN LINE
019000000000     C*------------------------------------------------------------------------*
019100000000     C*
019200000000     C* LEGGO IL PRIMO CLIENTE VALIDO
019300000000     C                   Z-ADD     *ZEROS        KSOKSC
019400990609     C     KEYSDO        SETGT     SISDO01L
019500990609     C                   READ      SISDO01L                               99
019600000000     C*
019700000000     C* PER TUTTI I CLIENTI PRESENTI NEI SALDI
019800000000DO  1C     *IN99         DOWEQ     *OFF
019900980119     C                   Z-ADD     SDOKSC        DEPKSC                         *DEPOSITO CLIENTE
020000980119     C*
020100980119     C* CONTROLLA VALIDITA' RECORD
020200980119     C                   EXSR      CHKSDO
020300000000     C*
020400000000     C* OPERAZIONI PER NUOVO CLIENTE
020500980119IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
020600000000     C                   EXSR      NEWKSC
020700000000     C*
020800000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
020900000000     C                   EXSR      SETFIL
021000000000     C*
021100000000     C* CICLO FINO A ROTTURA CODICE CLIENTE
021200980119DO  3C     $FINE         DOWEQ     '0'
021300000000     C*
021400000000     C* OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
021500000000     C                   EXSR      NEWDAT
021600000000     C*
021700000000     C* CICLO FINO A ROTTURA PERIODO SALDO CLIENTE
021800980119DO  4C     $FINE         DOWEQ     '0'
021900000000     C     SDCDAT        ANDEQ     DEPDAT
022000000000     C*
022100000000     C* CREA I SALDI NEL WORK FILE
022200000824     C                   EXSR      MEMSAL
022300000000     C*
022400000000     C* LEGGE IL RECORD SUCCESSIVO
022500000000     C                   EXSR      LETFIL
022600980119E   4C                   ENDDO                                                  *ROTTURA PERIODO
022700980119E   3C                   ENDDO                                                  *ROTTURA CLIENTE
022800000824E   2C                   ENDIF
022900000000     C*
023000000000     C* LEGGO IL SUCESSIVO CLIENTE VALIDO
023100000000     C                   Z-ADD     DEPKSC        KSOKSC
023200990609     C     KEYSDO        SETGT     SISDO01L
023300990609     C                   READ      SISDO01L                               99
023400000824E   1C                   ENDDO
023500000824     C*
023600000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
023700000825     C                   EXSR      AGGWFD
023800070723     C*
023900070723      *?reimposta correttamente il cliente Nuovo/Acquisito x Unificante...  ?
024000070723     c                   if        parSCE = 'U'
024100070723     C                   EXSR      AGGWFD_Uni
024200070723     c                   end
024300980403     C*
024400980403     C* OPERAZIONI FINALI
024500980403     C                   EXSR      FINSR
024600000000     C*
024700000000     C                   SETON                                        LR
024800000000     C*------------------------------------------------------------------------*
024900000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
025000000000     C*------------------------------------------------------------------------*
025100000000     C     NEWKSC        BEGSR
025200000824     C*
025300000824     C* IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-
025400000823IF  1C     PARSCE        IFEQ      'U'                                          *STAT PER UNIFICANTE
025500000823     C                   CLEAR                   BS10DS
025600001122     C*---               Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
025700010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
025800000823     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
025900000823     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
026000000823     C                   MOVE      DEPKSC        D10COD
026100000823     C                   CALL      'TIBS10R'
026200000823     C                   PARM                    BS10DS
026300000823     C                   Z-ADD     DEPKSC        DEPCLI
026400000823IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
026500000823     C     D10COP        ANDNE     *ZEROS
026600000823     C                   Z-ADD     D10COP        DEPCLI                         *UNIFICANTE
026700000823E   2C                   ENDIF
026800000823X   1C                   ELSE                                                   *STAT PER CLIENTE
026900000823     C                   Z-ADD     DEPKSC        DEPCLI
027000000823E   1C                   ENDIF
027100000824     C*
027200000000     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE
027300000823     C                   MOVEL     DEPCLI        DEPFIL                         *FIL+XXXX
027400000000     C                   Z-ADD     1             F
027500000000     C                   SETOFF                                           99
027600000000     C     DEPFIL        LOOKUP    FIL(F)                                 99
027700000000IF  1C     *IN99         IFEQ      *ON
027800000000     C                   Z-ADD     ARE(F)        DEPARE
027900000000     C                   MOVEL     DIV(F)        DEPDIV
028000000000X   1C                   ELSE
028100000000     C                   Z-ADD     *ZEROS        DEPARE
028200000000     C                   MOVEL     *BLANKS       DEPDIV
028300000000E   1C                   ENDIF
028400000824     C*
028500000824     C* REPERISCE IL COMMERCIALE, SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
028600980403     C                   CLEAR                   BS69DS
028700980403     C                   CLEAR                   CLPDS
028800980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
028900000823     C                   MOVEL     DEPCLI        I69KCP                         *CLIENTE X CNCLP
029000980403     C                   CALL      'TIBS69R'
029100980403     C                   PARM                    BS69DS
029200980403     C                   PARM                    ACODS
029300980403     C                   PARM                    INDDS
029400980403     C                   PARM                    CLPDS
029500980403     C                   PARM                    CLSDS
029600980406IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
029700980403     C     CLPAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
029800000823     C                   MOVEL     DEPCLI        NUM3              3 0          *CODICE FILIALE +
029900980403     C                   Z-ADD     *ZEROS        DEPAGE                         *AGENTE A ZERO  =
030000980403     C                   MOVEL     NUM3          DEPAGE                         *FIL+0000
030100980403X   1C                   ELSE                                                   *NO ERRORE
030200980403     C                   Z-ADD     CLPAGE        DEPAGE                         *CODICE AGENTE
030300980403E   1C                   ENDIF
030400000824     C*
030500000000     C* REPERISCE IL CODICE QUALITA'
030600000000     C                   MOVEL     'C'           DEPCLV
030700000000IF  1C     CLPCLV        IFNE      *BLANKS
030800000000     C                   MOVEL     CLPCLV        DEPCLV
030900000000E   1C                   ENDIF
031000000824     C*
031100000000     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
031200000000     C                   Z-ADD     1             C
031300000000     C                   SETOFF                                           99
031400000000     C     DEPAGE        LOOKUP    AGE(C)                                 99
031500000000IF  1C     *IN99         IFEQ      *OFF
031600000000     C                   EXSR      STAERR
031700000000     C                   Z-ADD     DEPAGE        DEPRGF                         RAGGRUPP = COD AGENT
031800000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
031900000000X   1C                   ELSE
032000000000IF  2C     RGF(C)        IFEQ      *ZEROS
032100000000     C                   Z-ADD     DEPAGE        DEPRGF
032200000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
032300000000X   2C                   ELSE
032400000000     C                   Z-ADD     RGF(C)        DEPRGF
032500000000     C*
032600000000     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
032700000000     C                   Z-ADD     1             C
032800000000     C                   SETOFF                                           99
032900000000     C     DEPRGF        LOOKUP    AGE(C)                                 99
033000000000IF  3C     *IN99         IFEQ      *ON
033100000000     C                   MOVEL     DAGE(C)       DEPRGD                         DESCRIZIONE RAGGR.TO
033200000000X   3C                   ELSE
033300000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGR.TO
033400000000E   3C                   ENDIF
033500000000E   2C                   ENDIF
033600000000E   1C                   ENDIF
033700000824     C*
033800000000     C* REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
033900000000     C                   Z-ADD     1             F
034000000000     C                   SETOFF                                           99
034100000000     C                   MOVEL     DEPRGF        NUM3                           *FIL+XXXX
034200000000     C     NUM3          LOOKUP    FIL(F)                                 99
034300000000IF  1C     *IN99         IFEQ      *ON
034400000000     C                   MOVEL     DIV(F)        DEPDIC
034500000000X   1C                   ELSE
034600000000     C                   MOVEL     *BLANKS       DEPDIC
034700000000E   1C                   ENDIF
034800000000     C*
034900000000     C                   ENDSR
035000000000     C*------------------------------------------------------------------------*
035100000000     C* NEWDAT - OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
035200000000     C*------------------------------------------------------------------------*
035300000000     C     NEWDAT        BEGSR
035400000000     C*
035500990609     C                   Z-ADD     SDCDAT        DEPDAT            6 0
035600000000     C*
035700000000     C* VEDO SE IL CLIENTE, NEL PERIODO, E' NUOVO O ACQUISITO
035800070720     C********           Z-ADD     DEPCLI        KSOKSC
035900070720     C*
036000070720      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
036100070720     C                   Z-ADD     DEPksc        KSOKSC
036200070720     C*
036300000000     C                   Z-ADD     SDCANN        KSOANN
036400000000     C                   Z-ADD     SDCMES        KSOMES
036500990609     C     KE2SDO        CHAIN     SISDO01L                           99
036600000000IF  1C     *IN99         IFEQ      *OFF
036700070720     C                   MOVEL     SDOCLN        DEPCLN
036800000000X   1C                   ELSE
036900000000     C                   MOVEL     *BLANKS       DEPCLN
037000000000E   1C                   ENDIF
037100000000     C*
037200000000     C                   ENDSR
037300980119     C*------------------------------------------------------------------------*
037400980119     C* CHKSDO - CONTROLLA VALIDITA' RECORD
037500980119     C*------------------------------------------------------------------------*
037600980119     C     CHKSDO        BEGSR
037700980119     C*
037800980119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
037900980119     C*
038000980119     C                   ENDSR
038100000000     C*------------------------------------------------------------------------*
038200000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
038300000000     C*------------------------------------------------------------------------*
038400000000     C     SETFIL        BEGSR
038500000000     C*
038600000000     C* REIMPOSTO FLAG DI FINE LETTURA SALDI
038700000000     C                   MOVEL     '0'           $FINE
038800000000     C*
038900000000     C                   Z-ADD     DEPKSC        KSCKSC                         *CLIENTE
039000000000     C                   Z-ADD     PARAPI        KSCANN                         *ANNO PRECEDENTE INI
039100000000     C                   Z-ADD     PARMPI        KSCMES                         *MESE PRECEDENTE INI
039200000000     C*
039300000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO SALDO CLIENTE VALIDO
039400990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
039500000000     C*
039600000000IF  1C     *IN99         IFEQ      '1'
039700000000     C                   MOVEL     '1'           $FINE
039800000000X   1C                   ELSE
039900000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
040000000000E   1C                   ENDIF
040100000000     C*
040200000000     C                   ENDSR
040300000000     C*------------------------------------------------------------------------*
040400000000     C* LETFIL - LETTURA PROSSIMO RECORD SALDI CLIENTE
040500000000     C*------------------------------------------------------------------------*
040600000000     C     LETFIL        BEGSR
040700000000     C*
040800000000     C* LEGGO FINO A:
040900000000     C*     - FINE FILE
041000000000     C*     - TROVATO RECORD VALIDO
041100980601     C                   MOVEL     'N'           WRECOK
041200000000DO  1C     $FINE         DOWEQ     '0'
041300000000     C     WRECOK        ANDEQ     'N'
041400000000     C*
041500990609     C     KSCKSC        READE     SISDC01L                               99
041600000000     C*
041700000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
041800000000     C                   MOVEL     '1'           $FINE
041900000000X   2C                   ELSE
042000000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
042100000000E   2C                   ENDIF
042200000000     C*
042300000000E   1C                   ENDDO
042400000000     C*
042500000000     C                   ENDSR
042600000000     C*------------------------------------------------------------------------*
042700000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
042800000000     C*------------------------------------------------------------------------*
042900000000     C     CHKREC        BEGSR
043000000000     C*
043100980601     C                   MOVEL     'S'           WRECOK
043200020516     C*
043300020516     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
043400020516     C                   Z-ADD     1             G
043500020516     C                   SETOFF                                           28
043600020516     C     SDCRBL        LOOKUP    FLAB(G)                                28
043700020516IF  1C     *IN28         IFEQ      *OFF                                         *TIPO NON TROVATO --
043800020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043900020516E   1C                   ENDIF
044000020516     C*
044100020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
044200020516IF  1C     WRECOK        IFEQ      'S'
044300020516IF  2C     SDCNSP        IFEQ      *ZEROS
044400020516     C     SDCPKG        ANDEQ     *ZEROS
044500020516     C     SDCPCC        ANDEQ     *ZEROS
044600020516     C     SDCPLS        ANDEQ     *ZEROS
044700020516     C     SDCNCO        ANDEQ     *ZEROS
044800020516     C     SDCVOL        ANDEQ     *ZEROS
044900020516     C     SDCVCC        ANDEQ     *ZEROS
045000020516     C     SDCVLS        ANDEQ     *ZEROS
045100020516     C     SDCIRB        ANDEQ     *ZEROS
045200020516     C     SDCIRR        ANDEQ     *ZEROS
045300020516     C     SDCINC        ANDEQ     *ZEROS
045400020516     C     SDCIRP        ANDEQ     *ZEROS
045500020516     C     SDCIRC        ANDEQ     *ZEROS
045600020516     C     SDCIRD        ANDEQ     *ZEROS
045700020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
045800020516E   2C                   ENDIF
045900020516E   1C                   ENDIF
046000000000     C*--------------------
046100000000     C* ESCLUSIONI
046200000000     C*--------------------
046300970821     C                   Z-ADD     1             P
046400970821     C                   SETOFF                                           28
046500970821     C     SDCFPE        LOOKUP    FPE(P)                                 28
046600970821IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
046700970821     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046800970821E   1C                   ENDIF
046900000000     C*--------------------
047000000000     C* SELEZIONI
047100000000     C*--------------------
047200000000     C* SE I SALDI SONO DI DATA SUPERIORE A QUELLA RICHIESTA, ESCO DAL CICLO
047300000000IF  1C     SDCDAT        IFGT      PARDA2
047400000000     C                   MOVEL     'N'           WRECOK
047500000000     C                   MOVEL     '1'           $FINE
047600000000E   1C                   ENDIF
047700000000     C*
047800000000     C* SE TIPO SERVIZIO <> TIPO SERVIZIO RICHIESTO, ESCLUDO
047900000000IF  1C     PARTSP        IFNE      *BLANKS                                      *NON TUTTI
048000000000     C     SDCTSP        ANDNE     PARTSP
048100000000     C                   MOVEL     'N'           WRECOK
048200000000E   1C                   ENDIF
048300000000     C*
048400000000     C                   ENDSR
048500000000     C*------------------------------------------------------------------------*
048600000824     C* MEMSAL - MEMORIZZA I DATI
048700000000     C*------------------------------------------------------------------------*
048800000824     C     MEMSAL        BEGSR
048900070531     C*
049000070531     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
049100070531     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
049200070531     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
049300070531     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
049400070531     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
049500000000     C*
049600000000     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
049700000000     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
049800000000IF  1C     FSP(G)        IFNE      'S'
049900000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
050000070531     C                   z-add     0             sdcPKG                         *PESO
050100070531     C                   z-add     0             sdcPCC                         *PESO CON CML
050200070531     C                   z-add     0             sdcPLS                         *PESO CML
050300070531     C                   z-add     0             sdcVOL                         *VOLUME TOTALE
050400070531     C                   z-add     0             sdcVCC                         *VOLUME CON CML
050500070531     C                   z-add     0             sdcVLS                         *VOLUME CML
050600000000E   1C                   ENDIF
050700000000     C*
050800070531     C* imposta il Peso ed il Volume
050900070531     c                   z-add     sdcPKG        depPKG
051000070531     c                   z-add     sdcVOL        depVOL
051100070531     C*
051200000000     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
051300000000     C                   Z-ADD     *ZEROS        DEPRRN
051400000000     C     SDCIRD        ADD       SDCIRR        DEPRRN
051500000000     C     SDCINC        ADD       DEPRRN        DEPRRN
051600020314     C*
051700020314     C* DETERMINA IL NETWORK
051800020404     C                   MOVEL     *BLANKS       DEPNTWP           3
051900020404     C                   Z-ADD     *ZEROS        DEPORDP           3 0
052000020314     C                   Z-ADD     1             R
052100020314     C     SDCREP        LOOKUP    RGREG(R)                               55
052200020314     C                   IF        *IN55 = *ON
052300020404     C                   EVAL      DEPNTWP = RGNTW(R)
052400020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
052500020404     C                   Z-ADD     1             X
052600020404     C     DEPNTWP       LOOKUP    NTWCOD(X)                              55
052700020404     C                   IF        *IN55 = *ON
052800020404     C                   EVAL      DEPORDP = NTWORD(X)
052900020404     C                   ENDIF
053000020314     C                   ENDIF
053100020314     C*
053200020404     C                   MOVEL     *BLANKS       DEPNTWA           3
053300020404     C                   Z-ADD     *ZEROS        DEPORDA           3 0
053400020314     C                   Z-ADD     1             R
053500020314     C     SDCREA        LOOKUP    RGREG(R)                               55
053600020314     C                   IF        *IN55 = *ON
053700020404     C                   EVAL      DEPNTWA = RGNTW(R)
053800020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
053900020404     C                   Z-ADD     1             X
054000020404     C     DEPNTWA       LOOKUP    NTWCOD(X)                              55
054100020404     C                   IF        *IN55 = *ON
054200020404     C                   EVAL      DEPORDA = NTWORD(X)
054300020404     C                   ENDIF
054400020314     C                   ENDIF
054500020404     C*
054600020404     C* IN BASE ALL'ORDINE DI CONSIDERAZIONE ATTRIBUISCO IL NETWORK DELLA SPEDIZIONE (ATTUALMENTE SOLO X ESTERO)
054700020404     C*
054800020404     C* 1) Italia su Italia
054900020404     C                   IF        DEPNTWP = *BLANKS AND
055000020404     C                             DEPNTWA = *BLANKS
055100020404     C                   EVAL      DEPNTW = *BLANKS
055200020404     C                   ELSE
055300020404     C* 2) Estero su Estero
055400020404     C                   IF        DEPNTWP <> *BLANKS AND
055500020404     C                             DEPNTWA <> *BLANKS
055600020404     C                   IF        DEPORDP < DEPORDA
055700020404     C                   EVAL      DEPNTW = DEPNTWP
055800020404     C                   ELSE
055900020404     C                   EVAL      DEPNTW = DEPNTWA
056000020404     C                   ENDIF
056100020404     C                   ELSE
056200020404     C* 3) Estero su Italia / Italia su Estero
056300020404     C                   IF        DEPNTWP <> *BLANKS
056400020404     C                   EVAL      DEPNTW = DEPNTWP
056500020404     C                   ELSE
056600020404     C                   EVAL      DEPNTW = DEPNTWA
056700020404     C                   ENDIF
056800020404     C                   ENDIF
056900020404     C                   ENDIF
057000020315     C*
057100020315     C* IMPOSTA SE FLAG ITALIA O ESTERO
057200020404     C     DEPNTWP       IFNE      *BLANKS
057300020404     C     DEPNTWA       ORNE      *BLANKS
057400020315     C                   MOVEL     'E'           DEPIOE                         *ESTERO
057500020315X   1C                   ELSE
057600020315     C                   MOVEL     'I'           DEPIOE                         *ITALIA
057700020315E   1C                   ENDIF
057800000000     C*
057900000000     C* CONTROLLA ESISTENZA RECORD DI LAVORO
058000000000     C                   MOVEL     DEPDIV        KWDDIV                         *DIVISIONE
058100000000     C                   Z-ADD     DEPARE        KWDARE                         *AREA
058200000000     C                   Z-ADD     DEPFIL        KWDFIL                         *FILIALE
058300000000     C                   Z-ADD     DEPAGE        KWDAGE                         *COMMERCIALE
058400000000     C                   MOVEL     DEPCLV        KWDCLV                         *CODICE QUALITA'
058500000823     C                   Z-ADD     DEPCLI        KWDCLI                         *CLIENTE
058600000824     C                   Z-ADD     SDCANN        KWDANN                         *ANNO
058700000824     C                   Z-ADD     SDCMES        KWDMES                         *MESE
058800000824     C                   MOVEL     SDCFPE        KWDFPE                         *FASCIA DI PESO
058900970821     C                   MOVEL     DEPIOE        KWDIOE                         *ITALIA O ESTERO
059000020315     C                   MOVEL     DEPNTW        KWDNTW                         *NETWORK
059100070531     c                   if        PARWfdpv = 'S'
059200070531     C     KEYDEL        CHAIN     WFdfpv1L                           99
059300070531     c                   else
059400000000     C     KEYDEL        CHAIN     WFDEL01L                           99
059500070531     c                   end
059600000000     C***
059700000000     C* I M M I S S I O N E
059800000000     C***
059900000000IF  1C     *IN99         IFEQ      '1'
060000000825     c                   CLEAR                   WFDEL000
060100000000     C                   MOVEL     DEPDIV        WFDDIV                         *DIVISIONE
060200000000     C                   Z-ADD     DEPARE        WFDARE                         *AREA
060300000000     C                   Z-ADD     DEPFIL        WFDFIL                         *FILIALE
060400000000     C                   Z-ADD     DEPAGE        WFDAGE                         *COMMERCIALE
060500000000     C                   MOVEL     DEPCLV        WFDCLV                         *CODICE QUALITA'
060600000823     C                   Z-ADD     DEPCLI        WFDCLI                         *CLIENTE
060700000824     C                   Z-ADD     SDCANN        WFDANN                         *ANNO
060800000824     C                   Z-ADD     SDCMES        WFDMES                         *MESE
060900000824     C                   MOVEL     SDCFPE        WFDFPE                         *FASCIA DI PESO
061000000824     C                   MOVEL     DEPIOE        WFDIOE                         *ITALIA O ESTERO
061100000000     C                   Z-ADD     DEPRGF        WFDRGF                         *RAGGRUPP FIL AGENTE
061200000000     C                   MOVEL     DEPRGD        WFDRGD                         *DESCRIZ RAGGRUP.TO
061300000000     C                   MOVEL     DEPDIC        WFDDIC                         DIVISIONE COMMERCIAL
061400000000     C                   Z-ADD     SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
061500070531     C                   z-add     depPKG        WFDpkg                         *Peso
061600070531     C                   z-add     depVOL        WFDvol                         *Volume
061700000000     C                   Z-ADD     DEPRRN        WFDRRN                         RIC REALE NETTO DELT
061800000000     C                   Z-ADD     SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
061900070720      * --
062000000000     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
062100070720      * --
062200980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
062300020315     C                   MOVEL     DEPNTW        WFDNTW                         *NETWORK
062400070531     c                   if        PARWfdpv = 'S'
062500070531     C                   WRITE     WFdfpv00
062600070531     c                   else
062700980528     C                   WRITE     WFDEL000
062800070531     c                   end
062900000000X   1C                   ELSE
063000000000     C***
063100000000     C* V A R I A Z I O N E
063200000000     C***
063300000000     C     WFDNSP        ADD       SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
063400070531     C     WFDpkg        add       depPKG        WFDpkg                         *Peso
063500070531     C     WFDvol        add       depVOL        WFDvol                         *Volume
063600000000     C     WFDRRN        ADD       DEPRRN        WFDRRN                         RIC REALE NETTO DELT
063700000000     C     WFDRPN        ADD       SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
063800980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
063900070720      * ---
064000070720      * solo x unificante
064100070720      *?  Attenzione: se lanciata x Unificante, occorre determinare se il codice  ?
064200070720      *?    unificante è nuovo o vecchio considerando tutti i suoi figli e,  ?
064300070821      *?    qualora uno di essi sia considerato vecchio, sarà quest'ultimo
064400070821      *?     a dare la caratteristica di vecchio al codice Unificante Padre.
064500070720      *?  >> SU SISDO l'info di Nuovo/Vecchio è sempre per singolo Cliente. <<
064600070723     c                   if        parSCE = 'U'
064700070723     c                   if        wfdCLN = *blank or
064800070723     c                             wfdCLN = 'Y' and DEPCLN = 'S'
064900070720     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
065000070720     c                   end
065100070723     c                   end
065200070720      * ---
065300070531     c                   if        PARWfdpv = 'S'
065400070531     C                   UPDATE    WFdfpv00
065500070531     c                   else
065600980528     C                   UPDATE    WFDEL000
065700070531     c                   end
065800000000E   1C                   ENDIF
065900000000     C*
066000000000     C                   ENDSR
066100000824     C*------------------------------------------------------------------------*
066200000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
066300000824     C*------------------------------------------------------------------------*
066400000825     C     AGGWFD        BEGSR
066500000825     C*
066600000825     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
066700070531     c                   if        PARWfdpv = 'S'
066800070531     C     *LOVAL        SETLL     WFdfpv4L
066900070531     C                   READ(N)   WFdfpv4L                               99
067000070531     c                   else
067100000825     C     *LOVAL        SETLL     WFDEL04L
067200000825     C                   READ(N)   WFDEL04L                               99
067300070531     c                   end
067400000825     C*
067500000825     C* CICLO FINO A FINE FILE
067600000825DO  1C     *IN99         DOWEQ     *OFF
067700000825     C*
067800000825     C* OPERAZIONE PER NUOVO CLIENTE
067900000825     C                   EXSR      NEWCLI
068000000825     C*
068100000825     C* CICLO FINO A ROTTURA CLIENTE
068200000825DO  2C     *IN99         DOWEQ     *OFF
068300000825     C     DEPCLI        ANDEQ     WFDCLI
068400070723      *
068500070723      *
068600000825IF  3C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
068700000825     C     WFDDAT        ANDLE     PARDA2
068800000825     C* MEMORIZZA I DATI -WFDEL-
068900000825     C                   EXSR      MEMWFD
069000000825E   3C                   ENDIF
069100000825     C*
069200000825     C* LETTURA SUCCESSIVA DATI CLIENTE
069300070531     c                   if        PARWfdpv = 'S'
069400070531     C                   READ(N)   WFdfpv4L                               99
069500070531     c                   else
069600000825     C                   READ(N)   WFDEL04L                               99
069700070531     c                   end
069800070723     C*
069900000825E   2C                   ENDDO                                                  *ROTTURA CLIENTE
070000000825     C*
070100000825     C* CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
070200000825     C                   EXSR      MODCLI
070300000825     C*
070400000825     C* SI RIPOSIZIONA E LEGGE I DATI DEL CLIENT SUCCESSIVO
070500070531     c                   if        PARWfdpv = 'S'
070600070531     C     DEPCLI        SETGT     WFdfpv4L
070700070531     C                   READ(N)   WFdfpv4L                               99
070800070531     c                   else
070900000825     C     DEPCLI        SETGT     WFDEL04L
071000000825     C                   READ(N)   WFDEL04L                               99
071100070531     c                   end
071200000825E   1C                   ENDDO
071300000824     C*
071400000824     C                   ENDSR
071500000825     C*------------------------------------------------------------------------*
071600000825     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE -WFDEL-
071700000825     C*------------------------------------------------------------------------*
071800000825     C     NEWCLI        BEGSR
071900000825     C*
072000000825     C* MEMORIZZA IL NUOVO CLIENTE IN UN DEPOSITO
072100000825     C                   Z-ADD     WFDCLI        DEPCLI
072200000825     C*
072300000825     C* AZZERA LE VARIABILI DI LAVORO
072400020116     C                   Z-ADD     *ZEROS        TOTIRD           19 5          *RICAVI
072500020116     C                   Z-ADD     *ZEROS        TOTIRP           19 5
072600020116     C                   Z-ADD     *ZEROS        TOTERD           19 5
072700020116     C                   Z-ADD     *ZEROS        TOTERP           19 5
072800020116     C                   Z-ADD     *ZEROS        TOTTRD           19 5
072900020116     C                   Z-ADD     *ZEROS        TOTTRP           19 5
073000000825     C                   Z-ADD     *ZEROS        PERDEI            4 1          *% DELTA
073100000825     C                   Z-ADD     *ZEROS        PERDEE            4 1
073200000825     C                   Z-ADD     *ZEROS        PERDET            4 1
073300000825     C                   MOVEL     'S'           $OKDEI            1            *DELTA OK
073400000825     C                   MOVEL     'S'           $OKDEE            1
073500000825     C                   MOVEL     'S'           $OKDET            1
073600000825     C                   CLEAR                   SMEI                           *MM CON % SEGNALARE
073700000825     C                   CLEAR                   SMEE
073800000825     C                   CLEAR                   SMET
073900000825     C                   Z-ADD     *ZEROS        MI                2 0          *INDICE MESI
074000000825     C                   Z-ADD     *ZEROS        ME                2 0
074100000825     C                   Z-ADD     *ZEROS        MT                2 0
074200000825     C*
074300000825     C                   ENDSR
074400000825     C*------------------------------------------------------------------------*
074500000825     C* MEMWFD - MEMORIZZA I DATI DEL WFILE
074600000825     C*------------------------------------------------------------------------*
074700000825     C     MEMWFD        BEGSR
074800000825     C
074900000825     C* MEMORIZZA I RICAVI ITALIA E ESTERO DEL CLIENTE PER IL PERIODO RICHIESTO
075000000825IF  1C     WFDIOE        IFEQ      'I'                                          *ITALIA
075100000825     C                   ADD       WFDRRN        TOTIRD                         *TOT RIC REALE
075200000825     C                   ADD       WFDRPN        TOTIRP                         *TOT RIC PRESUNTO
075300000825E   1C                   ENDIF
075400000825IF  1C     WFDIOE        IFEQ      'E'                                          *ESTERO
075500000825     C                   ADD       WFDRRN        TOTERD                         *TOT RIC REALE
075600000825     C                   ADD       WFDRPN        TOTERP                         *TOT RIC PRESUNTO
075700000825E   1C                   ENDIF
075800000825IF  1C     WFDIOE        IFEQ      *BLANKS                                      *ITALIA + ESTERO
075900000825     C                   ADD       WFDRRN        TOTTRD                         *TOT RIC REALE
076000000825     C                   ADD       WFDRPN        TOTTRP                         *TOT RIC PRESUNTO
076100000825E   1C                   ENDIF
076200000825     C*
076300000825     C* CONTROLLA SE LA % DELTA DI CIASCUNA FASCIA DI PESO RIENTRA IN QUELLA RICHIESTA
076400000825IF  1C     PARSPD        IFNE      *ZEROS                                       *RICHIESTA
076500000825     C*
076600000825     C* ITALIA
076700000825IF  2C     WFDIOE        IFEQ      'I'
076800000825IF  3C     WFDRRN        IFGT      *ZEROS
076900000825     C     WFDRPN        ORGT      *ZEROS
077000020116     C                   Z-ADD     WFDRRN        WRRW             17 5          *RICAVO REALE
077100020116     C                   Z-ADD     WFDRPN        WRPW             17 5          *RICAVO PRESUNTO
077200000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
077300000825     C                   EXSR      CALDEL
077400000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
077500000825     C                   SETOFF                                           97
077600000825     C     WFDMES        LOOKUP    SMEI                                   97
077700000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
077800000825     C                   ADD       1             MI
077900000825     C                   Z-ADD     WFDMES        SMEI(MI)                       *MESE DA SEGNALARE
078000000825E   5C                   ENDIF
078100000825E   4C                   ENDIF
078200000825E   3C                   ENDIF
078300000825E   2C                   ENDIF
078400000825     C* ESTERO
078500000825IF  2C     WFDIOE        IFEQ      'E'
078600000825IF  3C     WFDRRN        IFGT      *ZEROS
078700000825     C     WFDRPN        ORGT      *ZEROS
078800020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
078900020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
079000000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
079100000825     C                   EXSR      CALDEL
079200000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
079300000825     C                   SETOFF                                           97
079400000825     C     WFDMES        LOOKUP    SMEE                                   97
079500000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
079600000825     C                   ADD       1             ME
079700000825     C                   Z-ADD     WFDMES        SMEE(ME)                       *MESE DA SEGNALARE
079800000825E   5C                   ENDIF
079900000825E   4C                   ENDIF
080000000825E   3C                   ENDIF
080100000825E   2C                   ENDIF
080200000825     C* ITALIA + ESTERO
080300000825IF  2C     WFDIOE        IFEQ      *BLANKS
080400000825IF  3C     WFDRRN        IFGT      *ZEROS
080500000825     C     WFDRPN        ORGT      *ZEROS
080600020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
080700020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
080800000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
080900000825     C                   EXSR      CALDEL
081000000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
081100000825     C                   SETOFF                                           97
081200000825     C     WFDMES        LOOKUP    SMET                                   97
081300000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
081400000825     C                   ADD       1             MT
081500000825     C                   Z-ADD     WFDMES        SMET(MT)                       *MESE DA SEGNALARE
081600000825E   5C                   ENDIF
081700000825E   4C                   ENDIF
081800000825E   3C                   ENDIF
081900000825E   2C                   ENDIF
082000000825     C*
082100000825E   1C                   ENDIF
082200000825     C*
082300000825     C                   ENDSR
082400000825     C*------------------------------------------------------------------------*
082500000825     C* MODCLI - CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
082600000825     C*------------------------------------------------------------------------*
082700000825     C     MODCLI        BEGSR
082800000825     C*
082900000825     C* CALCOLA IL DELTA DEL CLIENTE
083000000825     C                   EXSR      CALCLI
083100000825     C*
083200000825     C* RILEGGE I DATI DEL CLIENTE -->
083300000825     C* - CANCELLA IL CLIENTE SE NON RIENTRA NEL RANGE DELTA RICHIESTO
083400000825     C* - AGGIORNA IL CLIENTE SE HA UNA % DELTA FASCE DA SEGNALARE
083500070531     c                   if        PARWfdpv = 'S'
083600070531     C     DEPCLI        SETLL     WFdfpv4L
083700070531     C     DEPCLI        READE     WFdfpv4L                               98
083800070531     c                   else
083900000825     C     DEPCLI        SETLL     WFDEL04L
084000000825     C     DEPCLI        READE     WFDEL04L                               98
084100070531     c                   end
084200070531      *
084300000825DO  1C     *IN98         DOWEQ     *OFF
084400000825     C*
084500000825     C* ITALIA
084600000825IF  2C     WFDIOE        IFEQ      'I'
084700000825IF  3C     $OKDEI        IFEQ      'N'                                          *DELTA NON OK
084800070531     c                   if        PARWfdpv = 'S'
084900070531     C                   DELETE    WFDfpv
085000070531     c                   else
085100000825     C                   DELETE    WFDEL4
085200070531     c                   end
085300000825X   3C                   ELSE                                                   *DELTA OK
085400000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
085500000828     C     WFDDAT        ANDLE     PARDA2
085600000825     C                   SETOFF                                           97
085700000825     C     WFDMES        LOOKUP    SMEI                                   97
085800000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
085900000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
086000070531     c                   if        PARWfdpv = 'S'
086100070531     C                   UPDATE    WFDfpv
086200070531     c                   else
086300000825     C                   UPDATE    WFDEL4
086400070531     c                   end
086500000828E   5C                   ENDIF
086600000828E   4C                   ENDIF
086700000825E   3C                   ENDIF
086800000825E   2C                   ENDIF
086900000825     C*
087000000825     C* ESTERO
087100000825IF  2C     WFDIOE        IFEQ      'E'
087200000825IF  3C     $OKDEE        IFEQ      'N'                                          *DELTA NON OK
087300070531     c                   if        PARWfdpv = 'S'
087400070531     C                   DELETE    WFDfpv
087500070531     c                   else
087600000825     C                   DELETE    WFDEL4
087700070531     c                   end
087800000825X   3C                   ELSE                                                   *DELTA OK
087900000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
088000000828     C     WFDDAT        ANDLE     PARDA2
088100000825     C                   SETOFF                                           97
088200000825     C     WFDMES        LOOKUP    SMEE                                   97
088300000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
088400000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
088500070531     c                   if        PARWfdpv = 'S'
088600070531     C                   UPDATE    WFDfpv
088700070531     c                   else
088800000825     C                   UPDATE    WFDEL4
088900070531     c                   end
089000000828E   5C                   ENDIF
089100000825E   4C                   ENDIF
089200000825E   3C                   ENDIF
089300000825E   2C                   ENDIF
089400000825     C*
089500000825     C* ITALIA + ESTERO
089600000825IF  2C     WFDIOE        IFEQ      *BLANKS
089700000825IF  3C     $OKDET        IFEQ      'N'                                          *DELTA NON OK
089800070531     c                   if        PARWfdpv = 'S'
089900070531     C                   DELETE    WFDfpv
090000070531     c                   else
090100000825     C                   DELETE    WFDEL4
090200070531     c                   end
090300000825X   3C                   ELSE                                                   *DELTA OK
090400000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
090500000828     C     WFDDAT        ANDLE     PARDA2
090600000825     C                   SETOFF                                           97
090700000825     C     WFDMES        LOOKUP    SMET                                   97
090800000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
090900000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
091000070531     c                   if        PARWfdpv = 'S'
091100070531     C                   UPDATE    WFDfpv
091200070531     c                   else
091300000825     C                   UPDATE    WFDEL4
091400070531     c                   end
091500000828E   5C                   ENDIF
091600000825E   4C                   ENDIF
091700000825E   3C                   ENDIF
091800000825E   2C                   ENDIF
091900000825     C*
092000070531     c                   if        PARWfdpv = 'S'
092100070531     C     DEPCLI        READE     WFdfpv4L                               98
092200070531     c                   else
092300000825     C     DEPCLI        READE     WFDEL04L                               98
092400070531     c                   end
092500000825E   1C                   ENDDO                                                  *DINE DATI CLIENTE
092600000825     C*
092700000825     C                   ENDSR
092800000825     C*------------------------------------------------------------------------*
092900000825     C* CALCOLA I DELTA SUI DATI MEMORIZZATI
093000000825     C*------------------------------------------------------------------------*
093100000825     C     CALCLI        BEGSR
093200000825     C*
093300000825     C* CALCOLA I DELTA E CONTROLLA SE RIENTRANO NEL RANGE RICHIESTO
093400000825IF  1C     PARPDD        IFNE      -999,9                                       *RICHIESTO
093500000825     C     PARPDA        ORNE      +999,9
093600000825     C*
093700000825     C* DELTA ITALIA
093800000825IF  2C     TOTIRD        IFGT      *ZEROS
093900000825     C     TOTIRP        ORGT      *ZEROS
094000020116     C                   Z-ADD     TOTIRD        WRRW                           *RICAVO REALE
094100020116     C                   Z-ADD     TOTIRP        WRPW                           *RICAVO PRESUNTO
094200000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
094300000825     C                   EXSR      CALDEL
094400000825     C                   Z-ADD     WDPW          PERDEI                         *% DELTA ITALIA
094500000825IF  3C     PERDEI        IFLT      PARPDD                                       *DELTA <> RANGE
094600000825     C     PERDEI        ORGT      PARPDA
094700000825     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
094800000825E   3C                   ENDIF
094900010313X   2C                   ELSE                                                   *NO RICAVI ITALIA
095000010313     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
095100000825E   2C                   ENDIF
095200000825     C* DELTA ESTERO
095300000825IF  2C     TOTERD        IFGT      *ZEROS
095400000825     C     TOTERP        ORGT      *ZEROS
095500020116     C                   Z-ADD     TOTERD        WRRW                           *RICAVO REALE
095600020116     C                   Z-ADD     TOTERP        WRPW                           *RICAVO PRESUNTO
095700000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
095800000825     C                   EXSR      CALDEL
095900000825     C                   Z-ADD     WDPW          PERDEE                         *% DELTA ESTERO
096000000825IF  3C     PERDEE        IFLT      PARPDD                                       *DELTA <> RANGE
096100000825     C     PERDEE        ORGT      PARPDA
096200000825     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
096300000825E   3C                   ENDIF
096400010313X   2C                   ELSE                                                   *NO RICAVI ESTERO
096500010313     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
096600000825E   2C                   ENDIF
096700000825     C* DELTA ITA + EST
096800000825IF  2C     TOTTRD        IFGT      *ZEROS
096900000825     C     TOTTRP        ORGT      *ZEROS
097000020116     C                   Z-ADD     TOTTRD        WRRW                           *RICAVO REALE
097100020116     C                   Z-ADD     TOTTRP        WRPW                           *RICAVO PRESUNTO
097200000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
097300000825     C                   EXSR      CALDEL
097400000825     C                   Z-ADD     WDPW          PERDET                         *% DELTA ESTERO
097500000825IF  3C     PERDET        IFLT      PARPDD                                       *DELTA <> RANGE
097600000825     C     PERDET        ORGT      PARPDA
097700000825     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
097800000825E   3C                   ENDIF
097900010313X   2C                   ELSE                                                   *NO RICAVI TOTALI
098000010313     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
098100000825E   2C                   ENDIF
098200000825     C*
098300000825E   1C                   ENDIF
098400000825     C*
098500000825     C                   ENDSR
098600000824     C*------------------------------------------------------------------------*
098700000824     C* CALDEL - CALCOLA IL DELTA
098800000824     C*          INPUT  --> WRRW: RICAVO REALE
098900970821     C*                     WRPW: RICAVO PRESUNTO
099000970821     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
099100970821     C*------------------------------------------------------------------------*
099200970821     C     CALDEL        BEGSR
099300970821     C*--------------------
099400970821     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
099500970821     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
099600970821     C*--------------------
099700970821     C*
099800970821     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
099900020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
100000970821     C     GAP           MULT      100           GAP                            (GAP x 100)
100100970821     C*
100200970821     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
100300970821IF  1C     WRRW          IFEQ      *ZEROS
100400000825     C                   Z-ADD     1             WRRW
100500970821E   1C                   ENDIF
100600970821     C*
100700970821     C     GAP           DIV       WRRW          GAP
100800000825     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
100900970821     C*
101000970821     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
101100970821     C     WDPW          COMP      0                                    28
101200970821     C* POSITIVO
101300970821IF  1C     *IN28         IFEQ      *OFF
101400970821IF  2C     GAP           IFGE      1000
101500970821     C                   Z-ADD     +999,9        WDPW
101600970821E   2C                   ENDIF
101700970821     C* NEGATIVO
101800970821X   1C                   ELSE
101900970821IF  2C     GAP           IFLE      -1000
102000970821     C                   Z-ADD     -999,9        WDPW
102100970821E   2C                   ENDIF
102200970821E   1C                   ENDIF
102300970821     C*
102400970821     C                   ENDSR
102500000000     C*------------------------------------------------------------------------*
102600000000     C* STAERR - STAMPA RIGA DI ERRORE PER AGENTE INESISTENTE O ANNULLATO
102700000000     C*------------------------------------------------------------------------*
102800000000     C     STAERR        BEGSR
102900000000     C*
103000000000IF  1C     *INOF         IFEQ      *ON
103100000000     C                   EXCEPT    TESTA
103200000000     C                   SETOFF                                       OF
103300000000E   1C                   ENDIF
103400000000     C*
103500000000     C* AGENTE NON ANCORA STAMPATO, SCRIVO ERRORE
103600000000     C                   SETOFF                                       97
103700000000     C     DEPAGE        LOOKUP    ERG                                    97
103800000000IF  1C     *IN97         IFEQ      *OFF
103900000000     C                   ADD       1             I                 3 0
104000000000     C                   Z-ADD     DEPAGE        ERG(I)                         *MEMORIZZA AGENTE
104100000000     C                   EXCEPT    ERROR                                        *STAMPA L'ERRORE
104200000000E   1C                   ENDIF
104300000000     C*
104400000000     C                   ENDSR
104500000000     C*------------------------------------------------------------------------*
104600000000     C* CARTAB - CARICA I DATI IN TABELLE
104700000000     C*------------------------------------------------------------------------*
104800000000     C     CARTAB        BEGSR
104900000000     C*--------------------
105000000000     C* ORGANIGRAMMA
105100000000     C*--------------------
105200000000     C                   Z-ADD     *ZEROS        F
105300000000     C                   Z-ADD     *ZEROS        KORFIL
105400000000     C     KEYORG        SETLL     AZORG01L
105500000000     C                   READ      AZORG01L                               99
105600000000DO  1C     *IN99         DOWEQ     '0'
105700980119IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
105800980119IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
105900000000     C                   ADD       1             F
106000000000     C                   Z-ADD     ORGFIL        FIL(F)
106100000000     C                   MOVEL     ORGDES        DFIL(F)
106200020906     C                   Z-ADD     1             KTBKUT
106300000000     C                   MOVEL     '05'          KTBCOD
106400000000     C                   MOVEL     *BLANKS       KTBKEY
106500000000     C                   MOVEL     ORGCAR        KTBKEY
106600000824     C     KEYTAB        CHAIN     TABEL00F                           99
106700980119IF  5C     *IN99         IFEQ      *OFF
106800000000     C                   Z-ADD     ORGCAR        ARE(F)
106900000000     C                   MOVEL     TBLUNI        DARE(F)
107000980119E   5C                   ENDIF
107100020906     C                   Z-ADD     1             KTBKUT
107200000000     C                   MOVEL     '17'          KTBCOD
107300000000     C                   MOVEL     *BLANKS       KTBKEY
107400000000     C                   MOVEL     ORGFL3        KTBKEY
107500000824     C     KEYTAB        CHAIN     TABEL00F                           99
107600980119IF  5C     *IN99         IFEQ      *OFF
107700000000     C                   MOVEL     ORGFL3        DIV(F)
107800000000     C                   MOVEL     TBLUNI        DDIV(F)
107900980119E   5C                   ENDIF
108000980119E   3C                   ENDIF
108100980119E   2C                   ENDIF
108200000000     C                   READ      AZORG01L                               99
108300000000E   1C                   ENDDO
108400000000     C*--------------------
108500000000     C* COMMERCIALI
108600000000     C*--------------------
108700000000     C                   Z-ADD     *ZEROS        C
108800020906     C                   Z-ADD     1             KTBKUT
108900000000     C                   MOVEL     '01'          KTBCOD
109000981103     C     KEYTA2        SETLL     TABEL00F
109100000824     C     KEYTA2        READE     TABEL00F                               99
109200000000DO  1C     *IN99         DOWEQ     *OFF
109300970825IF  2C     TBLFLG        IFEQ      *BLANKS
109400000000     C                   ADD       1             C
109500000000     C                   MOVEL     TBLUNI        DS01
109600000000     C                   MOVEL     TBLKEY        AGE(C)
109700000000     C                   MOVEL     §01RGF        RGF(C)
109800000000     C                   MOVEL     §01AGE        DAGE(C)
109900970825E   2C                   ENDIF
110000000824     C     KEYTA2        READE     TABEL00F                               99
110100000000E   1C                   ENDDO
110200000000     C*-------------------------
110300000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
110400000000     C*-------------------------
110500000000     C                   Z-ADD     *ZEROS        G
110600020906     C                   Z-ADD     1             KTBKUT
110700000000     C                   MOVEL     '2W'          KTBCOD
110800000000     C     KEYTA2        SETLL     TABEL00F
110900000824     C     KEYTA2        READE     TABEL00F                               99
111000000000DO  1C     *IN99         DOWEQ     *OFF
111100000000IF  2C     TBLFLG        IFEQ      *BLANKS
111200000000     C                   ADD       1             G
111300000000     C                   MOVEL     TBLKEY        FLAB(G)
111400000000     C                   MOVEL     TBLUNI        DS2W
111500000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
111600000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
111700000000E   2C                   ENDIF
111800000824     C     KEYTA2        READE     TABEL00F                               99
111900000000E   1C                   ENDDO
112000990217     C*--------------------
112100000000     C* FASCE DI PESO
112200990217     C*--------------------
112300990609IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
112400990217     C                   MOVEL     'SDC'         WSX               3
112500990217X   1C                   ELSE                                                   *FASCE DAL '99
112600020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO AL 2001
112700020325     C                   MOVEL     'S9C'         WSX
112800020325X   2C                   ELSE                                                   *FASCE DAL 2002
112900020325     C                   MOVEL     'S2C'         WSX
113000020325E   2C                   ENDIF
113100020325E   1C                   ENDIF
113200970821     C                   Z-ADD     *ZEROS        P
113300020906     C                   Z-ADD     1             KTBKUT
113400000000     C                   MOVEL     '2L'          KTBCOD
113500000000     C     KEYTA2        SETLL     TABEL00F
113600000824     C     KEYTA2        READE     TABEL00F                               99
113700000000DO  1C     *IN99         DOWEQ     *OFF
113800000000IF  2C     TBLFLG        IFEQ      *BLANKS
113900990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
114000000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
114100990217IF  3C     FILEST        IFEQ      WSX
114200000000     C                   MOVEL     TBLUNI        DS2L
114300000309IF  4C     §2LKGI        IFGE      5000
114400000309     C     PARDAT        ANDLT     200001
114500000309     C                   ELSE
114600970821     C                   ADD       1             P
114700970821     C                   MOVE      STACOD        FPE(P)                         *...+XX
114800000309E   4C                   ENDIF
114900000000E   3C                   ENDIF
115000000000E   2C                   ENDIF
115100000824     C     KEYTA2        READE     TABEL00F                               99
115200000000E   1C                   ENDDO
115300020314     C*-------------------------
115400020314     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
115500020314     C*-------------------------
115600020314     C                   Z-ADD     *ZEROS        R
115700020906     C                   Z-ADD     1             KTBKUT
115800020314     C                   MOVEL     '2M'          KTBCOD
115900020314     C     KEYTA2        SETLL     TABEL00F
116000020314     C     KEYTA2        READE     TABEL00F                               99
116100020314DO  1C     *IN99         DOWEQ     *OFF
116200020314IF  2C     TBLFLG        IFEQ      *BLANKS
116300020314     C                   ADD       1             R
116400020314     C                   MOVEL     TBLKEY        RGREG(R)
116500020314     C                   MOVEL     TBLUNI        DS2M
116600020314     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
116700020314E   2C                   ENDIF
116800020314     C     KEYTA2        READE     TABEL00F                               99
116900020314E   1C                   ENDDO
117000020404     C*----------
117100020404     C* NETWORK AZIENDALI
117200020404     C*----------
117300020404     C                   Z-ADD     *ZEROS        X
117400020404     C                   EVAL      TBECOD = 'NTW'
117500020404     C     KEYTBE        SETLL     TNTBE01L
117600020404     C     KEYTBE        READE     TNTBE01L
117700020404     C                   DOW       NOT %EOF(TNTBE01L)
117800020404     C                   ADD       1             X
117900020404     C                   MOVEL     TBEUNI        DNTW
118000020404     C                   EVAL      NTWCOD(X) = TBEKE1
118100020404     C                   EVAL      NTWORD(X) = §NTWORD
118200020404     C     KEYTBE        READE     TNTBE01L
118300020404     C                   ENDDO
118400000000     C*
118500000000     C                   ENDSR
118600970821     C*------------------------------------------------------------------------*
118700970821     C* AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
118800970821     C*------------------------------------------------------------------------*
118900970821     C     AMELAB        BEGSR
119000970821     C*---
119100970821     C* ANNO INIZIALE UGUALE AD ANNO FINALE
119200970821     C*---
119300970821IF  1C     WAA2          IFEQ      WAA
119400970821     C*
119500970821     C* CALCOLO I MESI DI DIFFERENZA
119600970821DO  2C     WMM           DOWLE     WMM2
119700970821     C                   ADD       1             JI
119800970821     C                   Z-ADD     WAA           ANN(JI)
119900970821     C                   Z-ADD     WMM           MES(JI)
120000970821     C                   ADD       1             WMM
120100970821E   2C                   ENDDO
120200970821     C*
120300970821     C* IL RESTO DELLA SCHIERA A ZERO
120400970821DO  2C     JI            DOWLT     12
120500970821     C                   ADD       1             JI
120600970821     C                   Z-ADD     *ZEROS        ANN(JI)
120700970821     C                   Z-ADD     *ZEROS        MES(JI)
120800970821E   2C                   ENDDO
120900970821     C*---
121000970821     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
121100970821     C*---
121200970821X   1C                   ELSE
121300970821     C*
121400970821     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
121500970821DO  2C     WMM           DOWLE     12
121600970821     C                   ADD       1             JI
121700970821     C                   Z-ADD     WAA           ANN(JI)
121800970821     C                   Z-ADD     WMM           MES(JI)
121900970821     C                   ADD       1             WMM
122000970821E   2C                   ENDDO
122100970821     C*
122200970821     C* MEMORIZZO I MESI DELL'ANNO FINALE
122300970821     C                   Z-ADD     01            WMM
122400970821DO  2C     WMM           DOWLE     WMM2
122500970821     C                   ADD       1             JI
122600970821     C                   Z-ADD     WAA2          ANN(JI)
122700970821     C                   Z-ADD     WMM           MES(JI)
122800970821     C                   ADD       1             WMM
122900970821E   2C                   ENDDO
123000970821     C*
123100970821     C* IL RESTO DELLA SCHIERA A ZERO
123200970821DO  2C     JI            DOWLT     12
123300970821     C                   ADD       1             JI
123400970821     C                   Z-ADD     *ZEROS        ANN(JI)
123500970821     C                   Z-ADD     *ZEROS        MES(JI)
123600970821E   2C                   ENDDO
123700970821E   1C                   ENDIF
123800970821     C*
123900970821     C                   ENDSR
124000980403     C*------------------------------------------------------------------------*
124100980403     C* FINSR - OPERAZIONI FINALI
124200980403     C*------------------------------------------------------------------------*
124300980403     C     FINSR         BEGSR
124400980403     C*
124500980403     C* CHIUDE I FILE DEI PGM CHIAMATI
124600980403     C                   CLEAR                   BS69DS
124700980403     C                   CLEAR                   ACODS
124800980403     C                   CLEAR                   INDDS
124900980403     C                   CLEAR                   CLPDS
125000980403     C                   CLEAR                   CLSDS
125100980403     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
125200980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
125300980403     C                   CALL      'TIBS69R'
125400980403     C                   PARM                    BS69DS
125500980403     C                   PARM                    ACODS
125600980403     C                   PARM                    INDDS
125700980403     C                   PARM                    CLPDS
125800980403     C                   PARM                    CLSDS
125900980403     C*
126000980403     C                   ENDSR
126100000000     C*------------------------------------------------------------------------*
126200000000     C* *INZSR - IMPOSTAZIONI INIZIALI
126300000000     C*------------------------------------------------------------------------*
126400000000     C     *INZSR        BEGSR
126500000000     C*
126600000000     C* PARAMETRI IN INPUT AL PGM
126700000000     C     *ENTRY        PLIST
126800000000     C                   PARM                    KPJBA
126900000000     C                   MOVEL     KPJBU         PARAM
127000070531     c*
127100070531     c* apertura condizionata dei 2 files da generare
127200070531     c                   if        PARWfdpv = 'S'
127300070531     c                   open      WFdfpv1l
127400070531     c                   open      WFdfpv4l
127500070723     c                   open      WFdfpv5l
127600070531     c                   else
127700070531     c                   open      WFdel01l
127800070531     c                   open      WFdel04l
127900070723     c                   open      WFdel05l
128000070531     c                   end
128100000000     C*
128200000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
128300000823     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
128400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
128500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
128600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
128700990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC01L
128800000000     C     *LIKE         DEFINE    SDCANN        KSCANN
128900000000     C     *LIKE         DEFINE    SDCMES        KSCMES
129000990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO01L
129100000000     C     *LIKE         DEFINE    SDOANN        KSOANN
129200000000     C     *LIKE         DEFINE    SDOMES        KSOMES
129300000000     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEL01L
129400000000     C     *LIKE         DEFINE    WFDARE        KWDARE
129500000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
129600000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
129700000000     C     *LIKE         DEFINE    WFDCLV        KWDCLV
129800000823     C     *LIKE         DEFINE    WFDCLI        KWDCLI
129900000000     C     *LIKE         DEFINE    WFDANN        KWDANN
130000000000     C     *LIKE         DEFINE    WFDMES        KWDMES
130100000824     C     *LIKE         DEFINE    WFDFPE        KWDFPE
130200000000     C     *LIKE         DEFINE    WFDIOE        KWDIOE
130300020315     C     *LIKE         DEFINE    WFDNTW        KWDNTW
130400000000     C*
130500000000     C* DEFINIZIONI DEPOSITI E VARIABILI
130600000000     C     *LIKE         DEFINE    CLPKSC        DEPKSC
130700000823     C     *LIKE         DEFINE    CLPKSC        DEPCLI
130800000000     C     *LIKE         DEFINE    CLPAGE        DEPAGE
130900000000     C     *LIKE         DEFINE    ORGFIL        DEPFIL
131000000000     C     *LIKE         DEFINE    ORGCAR        DEPARE
131100000000     C     *LIKE         DEFINE    ORGFL3        DEPDIV
131200000000     C     *LIKE         DEFINE    ORGFL3        DEPDIC
131300000000     C     *LIKE         DEFINE    WFDRRN        DEPRRN
131400000000     C     *LIKE         DEFINE    CLPAGE        DEPRGF
131500000000     C     *LIKE         DEFINE    CLPCLV        DEPCLV
131600000000     C     *LIKE         DEFINE    §01AGE        DEPRGD
131700000000     C     *LIKE         DEFINE    SDOCLN        DEPCLN
131800970821     C     *LIKE         DEFINE    WFDIOE        DEPIOE
131900020315     C     *LIKE         DEFINE    WFDNTW        DEPNTW
132000070531     C     *LIKE         DEFINE    WFDRRN        DEPRRN
132100070531     C* x il WFdfpv0F
132200070531     C     *LIKE         DEFINE    WFDpkg        DEPpkg
132300070531     C     *LIKE         DEFINE    WFDvol        DEPvol
132400070723     C     *LIKE         DEFINE    wfdANN        depANN
132500070723     C     *LIKE         DEFINE    wfdMES        depMES
132600000825     C*
132700000825     C* DEFINIZIONI VARIABILI DI LAVORO
132800000825     C     *LIKE         DEFINE    PARSPD        MENSPD
132900000000     C                   MOVEL     '0'           $FINE             1            *FINE PROGRAMMA
133000980601     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
133100980601     C                   MOVEL     '0'           PCLNEW            1            *CLIENTE NUOVO PRECE
133200000000     C                   MOVEL     '0'           CCLNEW            1            *CLIENTE NUOVO CORRE
133300000000     C                   Z-ADD     *ZEROS        C                 4 0          *PER COMMERCIALI
133400000000     C                   Z-ADD     *ZEROS        G                 4 0          *PER FLAG TIPI BOLLA
133500000000     C                   Z-ADD     *ZEROS        J                 4 0          *PER MESI ELABORABIL
133600000000     C                   Z-ADD     *ZEROS        F                 4 0          *PER ORGANIGRAMMA
133700970821     C                   Z-ADD     *ZEROS        P                 4 0          *PER FASCE DI PESO
133800970821     C                   Z-ADD     *ZEROS        A                 4 0          *PER ANNI
133900970821     C                   Z-ADD     *ZEROS        M                 4 0          *PER MESI
134000020404     C                   Z-ADD     *ZEROS        X                 3 0          *PER RAGGRUPPAMENTI REGIONI/NETWORK
134100020404     C                   Z-ADD     *ZEROS        R                 3 0          *PER NETWORK
134200000000     C                   SETON                                        OF        *OVERFLOW
134300000000     C*
134400000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
134500000000     C     KEYORG        KLIST
134600000000     C                   KFLD                    KORFIL                         *FILIALE
134700000000     C*
134800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
134900000000     C     KEYTAB        KLIST
135000000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
135100000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
135200000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
135300000000     C*
135400000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
135500000000     C     KEYTA2        KLIST
135600000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
135700000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
135800020404     C*
135900020404     C* CHIAVE LETTURA TNTBE00F - PARZIALE
136000020404     C     KEYTBE        KLIST
136100020404     C                   KFLD                    TBECOD
136200000000     C*
136300990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
136400000000     C     KEYSDC        KLIST
136500000000     C                   KFLD                    KSCKSC                         *CODICE CLIENTE
136600000000     C                   KFLD                    KSCANN                         *ANNO
136700000000     C                   KFLD                    KSCMES                         *MESE
136800000000     C*
136900990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
137000000000     C     KEYSDO        KLIST
137100000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
137200000000     C*
137300990609     C* CHIAVE LETTURA SISDO01L - COMPLETA
137400000000     C     KE2SDO        KLIST
137500000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
137600000000     C                   KFLD                    KSOANN                         *ANNO
137700000000     C                   KFLD                    KSOMES                         *MESE
137800000000     C*
137900000000     C* CHIAVE LETTURA WFDEL01L - COMPLETA
138000000000     C     KEYDEL        KLIST
138100000000     C                   KFLD                    KWDDIV                         *DIVISIONE
138200000000     C                   KFLD                    KWDARE                         *AREA
138300000000     C                   KFLD                    KWDFIL                         *FILIALE
138400000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
138500000000     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
138600000823     C                   KFLD                    KWDCLI                         *CLIENTE
138700000000     C                   KFLD                    KWDANN                         *ANNO
138800000000     C                   KFLD                    KWDMES                         *MESE
138900000824     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
139000000000     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
139100020614     C                   KFLD                    KWDNTW                         *NETWORK
139200000823     C*
139300000823     C* IMNPOSTA DATA / ORA CORRENTE
139400000823     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
139500000823     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
139600000823     C                   Z-ADD     *ZEROS        G08INV
139700000823     C                   MOVEL     '0'           G08ERR
139800000823     C                   CALL      'XSRDA8'
139900000823     C                   PARM                    WLBDA8
140000000823     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
140100000000     C*
140200000825     C* CARICA TABELLE OCCORRENTI
140300000000     C                   EXSR      CARTAB
140400970821     C*
140500970821     C* CALCOLA I MESI DA ELABORARE
140600990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
140700970821     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
140800990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
140900970821     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
141000970821     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORAR
141100970821     C                   EXSR      AMELAB
141200970822     C*
141300000825     C* PRENDE IL VALORE ASSOLUTO DELLA % ALLA QUALE SEGNALARE LO SCOSTAMENTO FASCE
141400970822     C                   MLLZO     1             PARSPD                         *VALORE ASSOLUTO
141500000825     C                   Z-ADD     *ZEROS        MENSPD
141600000825     C                   Z-SUB     PARSPD        MENSPD                         *% RICHIESTA MENO
141700001122     C*
141800001122     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
141900010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
142000010725     C                   MOVE      28            WDFIN
142100081114     C*
142200081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
142300081114     C*
142400081114     C*  Ultimo giorno del mese
142500081114     C                   MOVE      WDFIN         MMGG_4            4 0
142600081114     C                   MOVEL     MMGG_4        Mm_2              2 0
142700081114     C                   MOVEl     WDFIN         Anno_4            4 0
142800081114      * se è febbraio
142900081114     c                   If        Mm_2 = 2
143000081114      *
143100081114      *  potrebbe essere bisestile 29 gg.
143200081114     c     Anno_4        div       4             risulta           3 0
143300081114     c                   mvr                     resto             3 0
143400081114      *  Se bisestile
143500081114     c                   If        resto = 0
143600081114     C                   MOVE      '29'          WDFIN
143700081114     c                   end
143800081114      *
143900081114     c                   else
144000081114      *
144100081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
144200081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
144300081114     c                                Mm_2 = 9 or Mm_2 = 11
144400081114     C                   MOVE      '30'          WDFIN
144500081114     c                   else
144600081114     C                   MOVE      '31'          WDFIN
144700081114     c                   end
144800081114      *
144900081114     c                   endIf
145000081114     C*
145100081114     C*----------- F I N E    M E S E ---------------------------*
145200000000     C*
145300000000     C                   ENDSR
145400070723     C*------------------------------------------------------------------------*
145500070723     C* Aggiorna il flag Nuovo/Acquisito x UNificante
145600070723     C*------------------------------------------------------------------------*
145700070723     C     AGGWFD_uni    BEGSR
145800070727     C*
145900070727      *? Per prima cosa deve rendere omogenei i records nell'ambito dell'anno con "Y" e "S"...  ?
146000070727      *? significa che se nell'anno sono nati 2 codici clienti nuovi mi troverei "Y" su 2 mesi ?
146100070727      *? diversi nell'ambito dello stesso Anno.                             ?
146200070727      *? ..... ovviamente prevale la "Y" più vecchia a cui seguono delle "S" x indicare il Nuovo.
146300070727      *? -------------------------------------------------------------------------- ?
146400070727      *  (esempio esplicativo)
146500070727      *               Anno 200X? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
146600070727      *? Cliente (A)...  ?      |   | Y | S | S | S | S | S | S |   | S | S |   |
146700070727      *? Cliente (B)...  ?      |   |   |   | Y | S | S |   | S | S | S |   |   |
146800070727      *? -----------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
146900070727      *? Unificante (C)         |   | Y | S | Y | S | S | S | S | S | S | S |   |                         ?
147000070727      * -->
147100070727      *? Invece deve essere così|   | Y | S | S | S | S | S | S | S | S | S |   |   ?
147200070727      *? -------------------------------------------------------------------------- ?
147300070723     C*
147400070723     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
147500070723     c                   if        PARWfdpv = 'S'
147600070723     C     *LOVAL        SETLL     WFdfpv5L
147700070723     C                   READ      WFdfpv5L                               99
147800070723     c                   else
147900070723     C     *LOVAL        SETLL     WFDEL05L
148000070723     C                   READ      WFDEL05L                               99
148100070723     c                   end
148200070723     C*
148300070723     C* CICLO FINO A FINE FILE
148400070723DO  1C     *IN99         DOWEQ     *OFF
148500070723     C                   move      wfdIOE        depIOE
148600070723     C                   z-add     wfdCLI        depCLI
148700070723     C                   z-add     0             Cliente_Anno      1 0
148800070723      *
148900070723     C* a Rottura Cliente
149000070723DO  2C     *IN99         DOWEQ     *OFF
149100070723     C     depIOE        ANDEQ     wfdIOE
149200070723     C     depCLI        ANDEQ     wfdCLI
149300070723     C                   z-add     wfdANN        depANN
149400070723     C                   Clear                   Ypsilon_anno      1
149500070723     C                   add       1             Cliente_Anno
149600070723     C*
149700070723     C* a Rottura Cliente Anno
149800070723DO  3C     *IN99         DOWEQ     *OFF
149900070723     C     depIOE        ANDEQ     wfdIOE
150000070723     C     depCLI        ANDEQ     wfdCLI
150100070723     C     depANN        ANDEQ     wfdANN
150200070723     C                   z-add     wfdMES        depMES
150300070723     C*
150400070723     C* a Rottura Cliente Anno/Mese
150500070723DO  4C     *IN99         DOWEQ     *OFF
150600070723     C     depIOE        ANDEQ     wfdIOE
150700070723     C     depCLI        ANDEQ     wfdCLI
150800070723     C     depANN        ANDEQ     wfdANN
150900070723     C     depMES        ANDEQ     wfdMES
151000070723      *
151100070723      *  Memorizza il primo Ypsilon trovato x cliente Anno
151200070723     C                   if        wfdCLN <> ' ' and Ypsilon_anno = *blank
151300070723     C                   eval      Ypsilon_anno = wfdCLN
151400070723     c                   end
151500070723      *
151600070723      * Tutto il mese cliente deve essere a "Y"
151700070723     C                   if        Ypsilon_anno = 'Y'
151800070723     c                   eval      wfdCLN = 'Y'
151900070723     c                   end
152000070723      *
152100070723      * Dal mese seguente deve impostare sempre la "S"
152200070723      *   anche per l'anno seguente...
152300070723     C                   if        Ypsilon_anno = 'S'
152400070723     c                   eval      wfdCLN = 'S'
152500070723     c                   end
152600070723      *
152700070723     c                   if        PARWfdpv = 'S'
152800070723     C                   UPDATE    WFdfp5
152900070723     c                   else
153000070723     C                   UPDATE    WFDEL5
153100070723     c                   end
153200070723     C*
153300070723     C* LETTURA SUCCESSIVA DATI CLIENTE
153400070723     c                   if        PARWfdpv = 'S'
153500070723     C                   READ      WFdfpV5L                               99
153600070723     c                   else
153700070723     C                   READ      WFDEL05L                               99
153800070723     c                   end
153900070723E   4C                   ENDDO                                                  *ROTTURA Mese
154000070723     C*
154100070723     C*  Se il mese letto aveva la "Y" i mesi seguenti entro l'anno devono avere la "S"
154200070723     C                   if        Ypsilon_anno = 'Y'
154300070723     C                   eval      Ypsilon_anno = 'S'
154400070723     c                   end
154500070723      *
154600070723E   3C                   ENDDO                                                  *ROTTURA CLIENTE/Anno
154700070723     C*
154800070723E   2C                   ENDDO
154900070723     C*
155000070723E   1C                   ENDDO
155100070727     C*
155200070727      *? ------------------------------------------------------------------------------------------------- ?
155300070727      *? ......Dopo avere reso omogeneo nell'ambito dell'anno la sequenza di "Y" e "S", si deve considerare    ?
155400070727      *? sull'anno in CORSO che, se un cliente aveva già del fatturato nell'anno precedente,                           ?
155500070727      *? NON può considerarsi Nuovo.!!!!!!                         ?
155600070727      *? ..... quindi, sul Cliente, nell'Anno in CORSO, si devono eliminare le "Y" e "S" su tutti i records.
155700070727      *? ------------------------------------------------------------------------------------------------- ?
155800070727      *  (esempio esplicativo)
155900070727      *                              ? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
156000070727      *? Unific.(C) Anno Precedente ? |   |   |   | Y | S | S | S | S | S | S | S | S |
156100070727      *? Unific.(C) Anno  in  CORSO ? |   | Y | S | S | S | S | S | S |   |   |   |   |
156200070727      *? -----------------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
156300070727      *? ============================     ?
156400070727      * --> Invece deve essere così
156500070727      *? ============================     ?
156600070727      *? Unific.(C) Anno Precedente   |   |   |   | Y | S | S | S | S | S | S | S | S |   ?-- records anno precedente
156700070727      *? Unific.(C) Anno  in  CORSO   |   |   |   |   |   |   |   |   |   |   |   |   |   ?-- records anno in Corso
156800070727      *? -------------------------------------------------------------------------------- ?
156900070727     C*   Con un istruzione SQL possiamo quindi sistemare la situazione sull'anno in corso
157000070727     C*   semplicemente controllando se nell'anno precedente il Cliente aveva del Fatturato
157100070727     C*   quindi, aggiorniamo tutti i Suoi records dell'Anno in corso x dire che NON è NUOVO.
157200070727      *
157300070727     C/EXEC SQL
157400070727     C+ UPDATE wfdel00f SET wfdcln = ' '
157500070727     C+ WHERE wfdann = :ParAAI AND wfdcln <>' ' AND wfdcli
157600070727     C+ IN (SELECT wfdcli FROM wfdel00f WHERE wfdann = :ParAFP)
157700070727     C/END-EXEC
157800070723     C*
157900070723     C                   ENDSR
158000000000     O*------------------------------------------------------------------------*
158100000000     O* STAMPA DEI CODICI AGENTI INESISTENTI O ANNULLATI DA INSERIRE IN SEDE   *
158200000000     O*------------------------------------------------------------------------*
158300000000     OQSYSPRT   E            TESTA            02
158400000000     O                       STA(1)              66
158500000000     O                       STA(2)             132
158600980306     O                       UDATE              119 '  /  /  '
158700000000     O                       PAGE          Z    132
158800000000     O*------------------------
158900000000     O          E            TESTA            03
159000000000     O                       STA(3)              66
159100000000     O                       STA(3)             132
159200000000     O*------------------------
159300000000     O          E            TESTA            04
159400000000     O                       STA(4)              66
159500000000     O*------------------------
159600000000     O          E            TESTA            05
159700000000     O                       STA(3)              66
159800000000     O                       STA(3)             132
159900000000     O*------------------------
160000000000     O          E            ERROR       1
160100000000     O                       DEPAGE               9
160200000000     O                       DEPKSC              27
160300000824     O                       DEPCLI              47
160400000000     O*------------------------------------------------------------------------*
160500000000** STA
160600020116                           TISE92R      ***  AGENTI INESISTENTI DA1
160700980306 MODIFICARE IN ANAGRAFICA CLIENTE  ***       99/99/99     PAG.99992
160800000000------------------------------------------------------------------3
160900000824A G E N T E      C L I E N T E     U N I F I C A N T E            4
161000000824  9999999           9999999             9999999                   5
