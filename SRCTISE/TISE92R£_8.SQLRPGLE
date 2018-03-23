000100000000     H*------------------------------------------------------------------------*
000200100302     H* STATISTICA DELTA - CREAZIONE WFILE DI STAMPA                           *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800100202      *------
000900100202      *? INPUT
001000100202      *------
001100000000     FAZORG01L  IF   E           K DISK
001200000824     FTABEL00F  IF   E           K DISK
001300020404     FTNTBE01L  IF   E           K DISK
001400990609     FSISDC01L  IF   E           K DISK
001500990609     FSISDO01L  IF   E           K DISK
001600100127     FSISDP01L  IF   E           K DISK
001700100202      *------
001800100202      *? OUTPUT
001900100202      *------
002000100202      *?    St.DELTA
002100100127     FWFDEaV1L  UF A E           K DISK    usropn
002200100127     FWFDEa01L  UF A E           K DISK    usropn
002300100127     FWFDEa04L  UF   E           K DISK    usropn
002400100127     F                                     RENAME(WFDEA000:WFDEA4)
002500100127     FWFDEa05L  UF   E           K DISK    usropn
002600100127     F                                     RENAME(WFDEA000:WFDEA5)
002700100202      *---
002800100202      *?    St.Fascie PESI VOLUMI
002900100203     FWFdfpv1L  UF A E           K DISK    usropn
003000100203     FWFdfpv4L  UF   E           K DISK    usropn
003100100203     F                                     RENAME(WFDFPV00:WFDFPV)
003200100203     FWFdfpv5L  UF   E           K DISK    usropn
003300100203     F                                     RENAME(WFDFPV00:WFDFP5)
003400100202      *------
003500000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003600000000     D*------------------------------------------------------------------------*
003700000000     D* SCHIERE
003800000000     D*------------------------------------------------------------------------*
003900000000     D*------------------
004000000000     D* RIGHE DI STAMPA
004100000000     D*------------------
004200000000     D STA             S             66    DIM(5) CTDATA PERRCD(1)
004300000000     D*------------------
004400000000     D* COMMERCIALI IN ERRORE
004500000000     D*------------------
004600020314     D ERG             S              7  0 DIM(500)
004700000000     D*------------------
004800000000     D* ORGANIGRAMMA
004900000000     D*------------------
005000971124     D FIL             S              3  0 DIM(500)                             *FILIALI
005100971124     D DFIL            S             20    DIM(500)
005200971124     D ARE             S              3  0 DIM(500)                             *AREE
005300971124     D DARE            S             20    DIM(500)
005400971124     D DIV             S              1    DIM(500)                             *DIVISIONI
005500971124     D DDIV            S             20    DIM(500)
005600000000     D*------------------
005700000000     D* COMMERCIALI
005800000000     D*------------------
005900100202     D AGE             S              7  0 DIM(5000)                            *CODICI AGENTE
006000100202     D RGF             S              7  0 DIM(5000)                            *CODICI RAGGRUPPAMEN
006100100202     D DAGE            S             25    DIM(5000)                            *DECODIFICHE AGENTI
006200000000     D*------------------
006300000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
006400000000     D*------------------
006500000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
006600000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
006700000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
006800970821     D*------------------
006900970821     D* FASCE DI PESO
007000970821     D*------------------
007100020522     D FPE             S              2    DIM(11)
007200970821     D*------------------
007300970821     D* PERIODO DA ELABORARE
007400970821     D*------------------
007500990609     D ANN             S              4  0 DIM(12)                              *ANNO
007600000823     D MES             S              2  0 DIM(12)                              *MESE
007700000825     D*------------------
007800000825     D* MESI CON SCOSTAMENTO DELTA FASCE DA SEGNALARE
007900000825     D*------------------
008000000825     D SMEI            S              2  0 DIM(12)                              *ITALIA
008100000825     D SMEE            S              2  0 DIM(12)                              *ESTERO
008200000825     D SMET            S              2  0 DIM(12)                              *TOTALE
008300020314     D*------------------
008400020314     D* RAGGRUPPAMENTI REGIONI/NETWORK
008500020314     D*------------------
008600020315     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIONE
008700020315     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RAGGR. REGIONE
008800020404     D*------------------
008900020404     D* NETWORK AZIENDALI
009000020404     D*------------------
009100020404     D NTWCOD          S              3    DIM(100)                             *CODICI NETWORK
009200020404     D NTWORD          S              2  0 DIM(100)                             *PRIORITA' CONSIDERAZIONE NETWORK
009300000823     D*------------------
009400000823     D* DS CONTROLLO DATA (8)
009500000823     D*------------------
009600000823     D WLBDA8          DS                  INZ
009700000823     D  G08DAT                 1      8  0
009800000823     D  G08INV                 9     16  0
009900000823     D  G08ERR                17     17
010000000823     D  G08TGI                18     22  0
010100000823     D*------------------
010200000823     D* DS DI PROCEDURA
010300000823     D*------------------
010400000823     D BS10DS        E DS                  EXTNAME(TIBS10DS)
010500000000     D*------------------
010600000000     D* PARAMETRI DI LANCIO
010700000000     D*------------------
010800000000     D PARAM           DS
010900990609     D  PARDAT                 1      6  0
011000990609     D  PARAAI                 1      4  0
011100990609     D  PARMMI                 5      6  0
011200990609     D  PARDA2                 7     12  0
011300990609     D  PARAAF                 7     10  0
011400990609     D  PARMMF                11     12  0
011500990609     D  PARDCK                13     13
011600990609     D  PARDCC                14     14
011700990609     D  PARDCF                15     15
011800990609     D  PARFIC                16     18  0
011900990609     D  PARARC                19     21  0
012000990609     D  PARDIC                22     22
012100990609     D  PARDFF                23     23
012200990609     D  PARCOF                24     30  0
012300990609     D  PARDIF                31     31
012400990609     D  PARSTA                32     32
012500000824     D  PARSCE                33     33
012600990609     D  PARTSP                34     34
012700990609     D  PARIOE                35     35
012800990609     D  PARCLV                36     36
012900990609     D  PARDPT                37     42  0
013000990609     D  PARAPI                37     40  0
013100990609     D  PARMPI                41     42  0
013200990609     D  PARDP2                43     48  0
013300990609     D  PARAFP                43     46  0
013400990609     D  PARMFP                47     48  0
013500990609     D  PARSPD                49     52  1
013600990609     D  PARPDD                53     56  1
013700990609     D  PARPDA                57     60  1
013800000707     D  PARPOS                61     61
013900000825     D  PARCRE                62     62
014000010618     D  PARCL1                63     63
014100010618     D  PARCL2                64     64
014200010618     D  PARSIN                65     65
014300020404     D  PARCL3                66     66
014400020404     D  PARCL4                67     67
014500020404     D  PARNTW                68     68
014600030923     D  PARDFD                69     72  1
014700030923     D  PARDFA                73     76  1
014800030923     D  PARDFDP               77     80  1
014900030923     D  PARDFAP               81     84  1
015000030923     D  PARFPC                85     85
015100030923     D  PARWPC                86     86
015200030923     D  PARWCP                87     87
015300070531     D  PARWfdpv              88     88
015400000000     D*------------------
015500990609     D* DATA DI SISDC01L
015600000000     D*------------------
015700000000     D                 DS
015800990609     D  SDCANN                 1      4  0
015900990609     D  SDCMES                 5      6  0
016000990609     D  SDCDAT                 1      6  0
016100100215     D*------------------
016200100215     D* DATA DI SISDO00F
016300100215     D*------------------
016400100215     D                 DS
016500100215     D  SDOANN                 1      4  0
016600100215     D  SDOMES                 5      6  0
016700100215     D  SDODAT                 1      6  0
016800970821     D*------------------
016900100127     D* DATA DI WFDEA00F
017000970821     D*------------------
017100970821     D                 DS
017200970821     D  WFDANN                 1      4  0
017300970821     D  WFDMES                 5      6  0
017400990609     D  WFDDAT                 1      6  0
017500000000     D*------------------
017600000000     D* DS COMMERCIALI
017700000000     D*------------------
017800000000     D DS01          E DS
017900000000     D*------------------
018000000000     D* DS FASCE DI PESO
018100000000     D*------------------
018200000000     D DS2L          E DS
018300000000     D*------------------
018400000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
018500000000     D*------------------
018600000000     D DS2W          E DS
018700980403     D*------------------
018800980403     D* DS REPERIMENTO DATI UTENTE
018900980403     D*-------------------
019000980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
019100980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
019200980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
019300980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
019400980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
019500000000     D*------------------
019600000000     D* ARCHITETTURA
019700000000     D*------------------
019800000000     D KPJBA         E DS
019900020314     D*------------------
020000020314     D* DS DI LETTURA TABELLA "2M"
020100020314     D*------------------
020200020314     D DS2M          E DS
020300020404     D*-------------------
020400020404     D* DS DEFINIZIONE TABELLA "NETWORK"
020500020404     D*-------------------
020600020404     D DNTW          E DS
020700000000     C*------------------------------------------------------------------------*
020800000000     C* MAIN LINE
020900000000     C*------------------------------------------------------------------------*
021000000000     C*
021100000000     C* LEGGO IL PRIMO CLIENTE VALIDO
021200000000     C                   Z-ADD     *ZEROS        KSOKSC
021300990609     C     KEYSDO        SETGT     SISDO01L
021400990609     C                   READ      SISDO01L                               99
021500000000     C*
021600000000     C* PER TUTTI I CLIENTI PRESENTI NEI SALDI
021700000000DO  1C     *IN99         DOWEQ     *OFF
021800980119     C                   Z-ADD     SDOKSC        DEPKSC                         *DEPOSITO CLIENTE
021900980119     C*
022000980119     C* CONTROLLA VALIDITA' RECORD
022100980119     C                   EXSR      CHKSDO
022200000000     C*
022300000000     C* OPERAZIONI PER NUOVO CLIENTE
022400980119IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
022500000000     C                   EXSR      NEWKSC
022600100215     C*
022700000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
022800000000     C                   EXSR      SETFIL
022900000000     C*
023000000000     C* CICLO FINO A ROTTURA CODICE CLIENTE
023100980119DO  3C     $FINE         DOWEQ     '0'
023200000000     C*
023300000000     C* OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
023400000000     C                   EXSR      NEWDAT
023500100216     C*
023600100216     C* CREA I SALDI NEL WORK FILE x le VARIE
023700100216     c                   if        PARWfdpv <>'S'
023800100216     C                   EXSR      MEMSALVAR
023900100216     c                   end
024000000000     C*
024100000000     C* CICLO FINO A ROTTURA PERIODO SALDO CLIENTE
024200980119DO  4C     $FINE         DOWEQ     '0'
024300000000     C     SDCDAT        ANDEQ     DEPDAT
024400000000     C*
024500000000     C* CREA I SALDI NEL WORK FILE
024600000824     C                   EXSR      MEMSAL
024700000000     C*
024800000000     C* LEGGE IL RECORD SUCCESSIVO
024900000000     C                   EXSR      LETFIL
025000980119E   4C                   ENDDO                                                  *ROTTURA PERIODO
025100980119E   3C                   ENDDO                                                  *ROTTURA CLIENTE
025200000824E   2C                   ENDIF
025300000000     C*
025400000000     C* LEGGO IL SUCESSIVO CLIENTE VALIDO
025500000000     C                   Z-ADD     DEPKSC        KSOKSC
025600990609     C     KEYSDO        SETGT     SISDO01L
025700990609     C                   READ      SISDO01L                               99
025800000824E   1C                   ENDDO
025900000824     C*
026000000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
026100000825     C                   EXSR      AGGWFD
026200070723     C*
026300070723      *?reimposta correttamente il cliente Nuovo/Acquisito x Unificante...  ?
026400070723     c                   if        parSCE = 'U'
026500070723     C                   EXSR      AGGWFD_Uni
026600070723     c                   end
026700980403     C*
026800980403     C* OPERAZIONI FINALI
026900980403     C                   EXSR      FINSR
027000000000     C*
027100000000     C                   SETON                                        LR
027200000000     C*------------------------------------------------------------------------*
027300000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
027400000000     C*------------------------------------------------------------------------*
027500000000     C     NEWKSC        BEGSR
027600000824     C*
027700000824     C* IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-
027800000823IF  1C     PARSCE        IFEQ      'U'                                          *STAT PER UNIFICANTE
027900000823     C                   CLEAR                   BS10DS
028000001122     C*---               Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
028100010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
028200000823     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
028300000823     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
028400000823     C                   MOVE      DEPKSC        D10COD
028500000823     C                   CALL      'TIBS10R'
028600000823     C                   PARM                    BS10DS
028700100127     C                   Z-ADD     DEPKSC        UNIcli
028800000823IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
028900000823     C     D10COP        ANDNE     *ZEROS
029000100127     C                   Z-ADD     D10COP        UNIcli                         *UNIFICANTE
029100000823E   2C                   ENDIF
029200000823X   1C                   ELSE                                                   *STAT PER CLIENTE
029300100127     C                   Z-ADD     DEPKSC        UNIcli
029400000823E   1C                   ENDIF
029500000824     C*
029600000000     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE
029700100127     C                   MOVEL     UNIcli        DEPFIL                         *FIL+XXXX
029800000000     C                   Z-ADD     1             F
029900000000     C                   SETOFF                                           99
030000000000     C     DEPFIL        LOOKUP    FIL(F)                                 99
030100000000IF  1C     *IN99         IFEQ      *ON
030200000000     C                   Z-ADD     ARE(F)        DEPARE
030300000000     C                   MOVEL     DIV(F)        DEPDIV
030400000000X   1C                   ELSE
030500000000     C                   Z-ADD     *ZEROS        DEPARE
030600000000     C                   MOVEL     *BLANKS       DEPDIV
030700000000E   1C                   ENDIF
030800000824     C*
030900000824     C* REPERISCE IL COMMERCIALE, SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
031000980403     C                   CLEAR                   BS69DS
031100980403     C                   CLEAR                   CLPDS
031200980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
031300100127     C                   MOVEL     UNIcli        I69KCP                         *CLIENTE X CNCLP
031400980403     C                   CALL      'TIBS69R'
031500980403     C                   PARM                    BS69DS
031600980403     C                   PARM                    ACODS
031700980403     C                   PARM                    INDDS
031800980403     C                   PARM                    CLPDS
031900980403     C                   PARM                    CLSDS
032000980406IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
032100980403     C     CLPAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
032200100127     C                   MOVEL     UNIcli        NUM3              3 0          *CODICE FILIALE +
032300980403     C                   Z-ADD     *ZEROS        DEPAGE                         *AGENTE A ZERO  =
032400980403     C                   MOVEL     NUM3          DEPAGE                         *FIL+0000
032500980403X   1C                   ELSE                                                   *NO ERRORE
032600980403     C                   Z-ADD     CLPAGE        DEPAGE                         *CODICE AGENTE
032700980403E   1C                   ENDIF
032800000824     C*
032900000000     C* REPERISCE IL CODICE QUALITA'
033000000000     C                   MOVEL     'C'           DEPCLV
033100000000IF  1C     CLPCLV        IFNE      *BLANKS
033200000000     C                   MOVEL     CLPCLV        DEPCLV
033300000000E   1C                   ENDIF
033400000824     C*
033500000000     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
033600000000     C                   Z-ADD     1             C
033700000000     C                   SETOFF                                           99
033800000000     C     DEPAGE        LOOKUP    AGE(C)                                 99
033900000000IF  1C     *IN99         IFEQ      *OFF
034000000000     C                   EXSR      STAERR
034100000000     C                   Z-ADD     DEPAGE        DEPRGF                         RAGGRUPP = COD AGENT
034200000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
034300000000X   1C                   ELSE
034400000000IF  2C     RGF(C)        IFEQ      *ZEROS
034500000000     C                   Z-ADD     DEPAGE        DEPRGF
034600000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
034700000000X   2C                   ELSE
034800000000     C                   Z-ADD     RGF(C)        DEPRGF
034900000000     C*
035000000000     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
035100000000     C                   Z-ADD     1             C
035200000000     C                   SETOFF                                           99
035300000000     C     DEPRGF        LOOKUP    AGE(C)                                 99
035400000000IF  3C     *IN99         IFEQ      *ON
035500000000     C                   MOVEL     DAGE(C)       DEPRGD                         DESCRIZIONE RAGGR.TO
035600000000X   3C                   ELSE
035700000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGR.TO
035800000000E   3C                   ENDIF
035900000000E   2C                   ENDIF
036000000000E   1C                   ENDIF
036100000824     C*
036200000000     C* REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
036300000000     C                   Z-ADD     1             F
036400000000     C                   SETOFF                                           99
036500000000     C                   MOVEL     DEPRGF        NUM3                           *FIL+XXXX
036600000000     C     NUM3          LOOKUP    FIL(F)                                 99
036700000000IF  1C     *IN99         IFEQ      *ON
036800000000     C                   MOVEL     DIV(F)        DEPDIC
036900000000X   1C                   ELSE
037000000000     C                   MOVEL     *BLANKS       DEPDIC
037100000000E   1C                   ENDIF
037200000000     C*
037300000000     C                   ENDSR
037400000000     C*------------------------------------------------------------------------*
037500000000     C* NEWDAT - OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
037600000000     C*------------------------------------------------------------------------*
037700000000     C     NEWDAT        BEGSR
037800000000     C*
037900990609     C                   Z-ADD     SDCDAT        DEPDAT            6 0
038000000000     C*
038100000000     C* VEDO SE IL CLIENTE, NEL PERIODO, E' NUOVO O ACQUISITO
038200100127     C********           Z-ADD     UNIcli        KSOKSC
038300070720     C*
038400070720      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
038500070720     C                   Z-ADD     DEPksc        KSOKSC
038600070720     C*
038700000000     C                   Z-ADD     SDCANN        KSOANN
038800000000     C                   Z-ADD     SDCMES        KSOMES
038900990609     C     KE2SDO        CHAIN     SISDO01L                           99
039000000000IF  1C     *IN99         IFEQ      *OFF
039100070720     C                   MOVEL     SDOCLN        DEPCLN
039200000000X   1C                   ELSE
039300000000     C                   MOVEL     *BLANKS       DEPCLN
039400000000E   1C                   ENDIF
039500000000     C*
039600000000     C                   ENDSR
039700100127     C*------------------------------------------------------------------------*
039800100127     C* MEMSALVAR - MEMORIZZA I DATI
039900100127     C*------------------------------------------------------------------------*
040000100127     C     MEMSALvar     BEGSR
040100100127     C*
040200100128      *  ciclo sui saldi delle VARIE per riportarne i totali delle "e" e "h"
040300100127     C*
040400100127      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
040500100127     C                   Z-ADD     DEPksc        KSOKSC
040600100216     C                   Z-ADD     SDcDAT        DEPDAT            6 0
040700100216     C                   Z-ADD     SDcANN        KSOANN
040800100216     C                   Z-ADD     SDcMES        KSOMES
040900100127     C     KE2SDO        setll     SISDp01L
041000100127     C     KE2SDO        reade     SISDp01L
041100100127     C*
041200100127     c                   dow       not %EoF(SISDp01L)
041300100127     C*
041400100128     C*  Solo legate alla Priority o H10:30
041500100128     c                   if        SDpSVR = 'e' or SDpSVR = 'h'
041600100128     C*
041700100127     C* CONTROLLA ESISTENZA RECORD DI LAVORO
041800100127     C                   Z-ADD     UNIcli        wfvksc                         *CLIENTE
041900100127     C                   Z-ADD     SDpANN        wfvANN                         *ANNO
042000100127     C                   Z-ADD     SDpMES        wfvMES                         *MESE
042100100127     C                   moveL     SDpSVR        wfvSVR                         *VARIA
042200100127     C     KEYDEAV       CHAIN     WFDEAV1L
042300100127     C***
042400100127     C* I M M I S S I O N E
042500100127     C***
042600100127IF  1C                   IF        not %Found(WFDEAV1L)
042700100127      * --
042800100127     c                   CLEAR                   WFDEAV00
042900100127     C                   Z-ADD     UNIcli        WFvksc                         *CLIENTE
043000100127     C                   Z-ADD     SDpANN        WFvANN                         *ANNO
043100100127     C                   Z-ADD     SDpMES        WFvMES                         *MESE
043200100127     C                   MOVEL     SDpSVR        WFvSVR                         *VARIA
043300100127     C                   Z-ADD     SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
043400100127     C                   z-add     SDpRCP        WFvRCP                         *Ricavi Particolari
043500100127      * --
043600100127     C                   WRITE     WFDEAV00
043700100127      * --
043800100127X   1C                   ELSEif    %Found(WFDEAV1L)
043900100127     C***
044000100127     C* V A R I A Z I O N E
044100100127     C***
044200100127     C                   add       SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
044300100127     C                   add       SDpRCP        WFvRCP                         *Ricavi Particolari
044400100127      * ---
044500100127     C                   UPDATE    WFDEAV00
044600100127E   1C                   ENDIF
044700100127     C*
044800100128     c                   end
044900100128     C*
045000100127     C     KE2SDO        reade     SISDp01L
045100100127E   1C                   ENDdo
045200100127     C*
045300100127     C                   ENDSR
045400980119     C*------------------------------------------------------------------------*
045500980119     C* CHKSDO - CONTROLLA VALIDITA' RECORD
045600980119     C*------------------------------------------------------------------------*
045700980119     C     CHKSDO        BEGSR
045800980119     C*
045900980119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
046000980119     C*
046100980119     C                   ENDSR
046200000000     C*------------------------------------------------------------------------*
046300000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
046400000000     C*------------------------------------------------------------------------*
046500000000     C     SETFIL        BEGSR
046600000000     C*
046700000000     C* REIMPOSTO FLAG DI FINE LETTURA SALDI
046800000000     C                   MOVEL     '0'           $FINE
046900000000     C*
047000000000     C                   Z-ADD     DEPKSC        KSCKSC                         *CLIENTE
047100000000     C                   Z-ADD     PARAPI        KSCANN                         *ANNO PRECEDENTE INI
047200000000     C                   Z-ADD     PARMPI        KSCMES                         *MESE PRECEDENTE INI
047300000000     C*
047400000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO SALDO CLIENTE VALIDO
047500990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
047600000000     C*
047700000000IF  1C     *IN99         IFEQ      '1'
047800000000     C                   MOVEL     '1'           $FINE
047900000000X   1C                   ELSE
048000000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
048100000000E   1C                   ENDIF
048200000000     C*
048300000000     C                   ENDSR
048400000000     C*------------------------------------------------------------------------*
048500000000     C* LETFIL - LETTURA PROSSIMO RECORD SALDI CLIENTE
048600000000     C*------------------------------------------------------------------------*
048700000000     C     LETFIL        BEGSR
048800000000     C*
048900000000     C* LEGGO FINO A:
049000000000     C*     - FINE FILE
049100000000     C*     - TROVATO RECORD VALIDO
049200980601     C                   MOVEL     'N'           WRECOK
049300000000DO  1C     $FINE         DOWEQ     '0'
049400000000     C     WRECOK        ANDEQ     'N'
049500000000     C*
049600990609     C     KSCKSC        READE     SISDC01L                               99
049700000000     C*
049800000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
049900000000     C                   MOVEL     '1'           $FINE
050000000000X   2C                   ELSE
050100000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
050200000000E   2C                   ENDIF
050300000000     C*
050400000000E   1C                   ENDDO
050500000000     C*
050600000000     C                   ENDSR
050700000000     C*------------------------------------------------------------------------*
050800000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
050900000000     C*------------------------------------------------------------------------*
051000000000     C     CHKREC        BEGSR
051100000000     C*
051200980601     C                   MOVEL     'S'           WRECOK
051300020516     C*
051400020516     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
051500020516     C                   Z-ADD     1             G
051600020516     C                   SETOFF                                           28
051700020516     C     SDCRBL        LOOKUP    FLAB(G)                                28
051800020516IF  1C     *IN28         IFEQ      *OFF                                         *TIPO NON TROVATO --
051900020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
052000020516E   1C                   ENDIF
052100020516     C*
052200020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
052300020516IF  1C     WRECOK        IFEQ      'S'
052400020516IF  2C     SDCNSP        IFEQ      *ZEROS
052500020516     C     SDCPKG        ANDEQ     *ZEROS
052600020516     C     SDCPCC        ANDEQ     *ZEROS
052700020516     C     SDCPLS        ANDEQ     *ZEROS
052800020516     C     SDCNCO        ANDEQ     *ZEROS
052900020516     C     SDCVOL        ANDEQ     *ZEROS
053000020516     C     SDCVCC        ANDEQ     *ZEROS
053100020516     C     SDCVLS        ANDEQ     *ZEROS
053200020516     C     SDCIRB        ANDEQ     *ZEROS
053300020516     C     SDCIRR        ANDEQ     *ZEROS
053400020516     C     SDCINC        ANDEQ     *ZEROS
053500020516     C     SDCIRP        ANDEQ     *ZEROS
053600020516     C     SDCIRC        ANDEQ     *ZEROS
053700020516     C     SDCIRD        ANDEQ     *ZEROS
053800020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
053900020516E   2C                   ENDIF
054000020516E   1C                   ENDIF
054100000000     C*--------------------
054200000000     C* ESCLUSIONI
054300000000     C*--------------------
054400970821     C                   Z-ADD     1             P
054500970821     C                   SETOFF                                           28
054600970821     C     SDCFPE        LOOKUP    FPE(P)                                 28
054700970821IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
054800970821     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
054900970821E   1C                   ENDIF
055000000000     C*--------------------
055100000000     C* SELEZIONI
055200000000     C*--------------------
055300000000     C* SE I SALDI SONO DI DATA SUPERIORE A QUELLA RICHIESTA, ESCO DAL CICLO
055400000000IF  1C     SDCDAT        IFGT      PARDA2
055500000000     C                   MOVEL     'N'           WRECOK
055600000000     C                   MOVEL     '1'           $FINE
055700000000E   1C                   ENDIF
055800000000     C*
055900000000     C* SE TIPO SERVIZIO <> TIPO SERVIZIO RICHIESTO, ESCLUDO
056000100127IF  1C**** PARTSP        IFNE      *BLANKS                                      *NON TUTTI
056100100127     C**** SDCTSP        ANDNE     PARTSP
056200100127     C*********          MOVEL     'N'           WRECOK
056300100127E   1C*********          ENDIF
056400000000     C*
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700000824     C* MEMSAL - MEMORIZZA I DATI
056800000000     C*------------------------------------------------------------------------*
056900000824     C     MEMSAL        BEGSR
057000070531     C*
057100070531     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
057200070531     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
057300070531     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
057400070531     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
057500070531     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
057600000000     C*
057700000000     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
057800000000     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
057900000000IF  1C     FSP(G)        IFNE      'S'
058000000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
058100070531     C                   z-add     0             sdcPKG                         *PESO
058200070531     C                   z-add     0             sdcPCC                         *PESO CON CML
058300070531     C                   z-add     0             sdcPLS                         *PESO CML
058400070531     C                   z-add     0             sdcVOL                         *VOLUME TOTALE
058500070531     C                   z-add     0             sdcVCC                         *VOLUME CON CML
058600070531     C                   z-add     0             sdcVLS                         *VOLUME CML
058700000000E   1C                   ENDIF
058800000000     C*
058900070531     C* imposta il Peso ed il Volume
059000070531     c                   z-add     sdcPKG        depPKG
059100070531     c                   z-add     sdcVOL        depVOL
059200070531     C*
059300000000     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
059400000000     C                   Z-ADD     *ZEROS        DEPRRN
059500000000     C     SDCIRD        ADD       SDCIRR        DEPRRN
059600000000     C     SDCINC        ADD       DEPRRN        DEPRRN
059700020314     C*
059800020314     C* DETERMINA IL NETWORK
059900020404     C                   MOVEL     *BLANKS       DEPNTWP           3
060000020404     C                   Z-ADD     *ZEROS        DEPORDP           3 0
060100020314     C                   Z-ADD     1             R
060200020314     C     SDCREP        LOOKUP    RGREG(R)                               55
060300020314     C                   IF        *IN55 = *ON
060400020404     C                   EVAL      DEPNTWP = RGNTW(R)
060500020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
060600020404     C                   Z-ADD     1             X
060700020404     C     DEPNTWP       LOOKUP    NTWCOD(X)                              55
060800020404     C                   IF        *IN55 = *ON
060900020404     C                   EVAL      DEPORDP = NTWORD(X)
061000020404     C                   ENDIF
061100020314     C                   ENDIF
061200020314     C*
061300020404     C                   MOVEL     *BLANKS       DEPNTWA           3
061400020404     C                   Z-ADD     *ZEROS        DEPORDA           3 0
061500020314     C                   Z-ADD     1             R
061600020314     C     SDCREA        LOOKUP    RGREG(R)                               55
061700020314     C                   IF        *IN55 = *ON
061800020404     C                   EVAL      DEPNTWA = RGNTW(R)
061900020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
062000020404     C                   Z-ADD     1             X
062100020404     C     DEPNTWA       LOOKUP    NTWCOD(X)                              55
062200020404     C                   IF        *IN55 = *ON
062300020404     C                   EVAL      DEPORDA = NTWORD(X)
062400020404     C                   ENDIF
062500020314     C                   ENDIF
062600020404     C*
062700020404     C* IN BASE ALL'ORDINE DI CONSIDERAZIONE ATTRIBUISCO IL NETWORK DELLA SPEDIZIONE (ATTUALMENTE SOLO X ESTERO)
062800020404     C*
062900020404     C* 1) Italia su Italia
063000020404     C                   IF        DEPNTWP = *BLANKS AND
063100020404     C                             DEPNTWA = *BLANKS
063200020404     C                   EVAL      DEPNTW = *BLANKS
063300020404     C                   ELSE
063400020404     C* 2) Estero su Estero
063500020404     C                   IF        DEPNTWP <> *BLANKS AND
063600020404     C                             DEPNTWA <> *BLANKS
063700020404     C                   IF        DEPORDP < DEPORDA
063800020404     C                   EVAL      DEPNTW = DEPNTWP
063900020404     C                   ELSE
064000020404     C                   EVAL      DEPNTW = DEPNTWA
064100020404     C                   ENDIF
064200020404     C                   ELSE
064300020404     C* 3) Estero su Italia / Italia su Estero
064400020404     C                   IF        DEPNTWP <> *BLANKS
064500020404     C                   EVAL      DEPNTW = DEPNTWP
064600020404     C                   ELSE
064700020404     C                   EVAL      DEPNTW = DEPNTWA
064800020404     C                   ENDIF
064900020404     C                   ENDIF
065000020404     C                   ENDIF
065100020315     C*
065200020315     C* IMPOSTA SE FLAG ITALIA O ESTERO
065300020404     C     DEPNTWP       IFNE      *BLANKS
065400020404     C     DEPNTWA       ORNE      *BLANKS
065500020315     C                   MOVEL     'E'           DEPIOE                         *ESTERO
065600020315X   1C                   ELSE
065700020315     C                   MOVEL     'I'           DEPIOE                         *ITALIA
065800020315E   1C                   ENDIF
065900000000     C*
066000000000     C* CONTROLLA ESISTENZA RECORD DI LAVORO
066100000000     C                   MOVEL     DEPDIV        KWDDIV                         *DIVISIONE
066200000000     C                   Z-ADD     DEPARE        KWDARE                         *AREA
066300000000     C                   Z-ADD     DEPFIL        KWDFIL                         *FILIALE
066400000000     C                   Z-ADD     DEPAGE        KWDAGE                         *COMMERCIALE
066500000000     C                   MOVEL     DEPCLV        KWDCLV                         *CODICE QUALITA'
066600100127     C                   Z-ADD     UNIcli        KWDCLI                         *CLIENTE
066700000824     C                   Z-ADD     SDCANN        KWDANN                         *ANNO
066800000824     C                   Z-ADD     SDCMES        KWDMES                         *MESE
066900000824     C                   MOVEL     SDCFPE        KWDFPE                         *FASCIA DI PESO
067000970821     C                   MOVEL     DEPIOE        KWDIOE                         *ITALIA O ESTERO
067100020315     C                   MOVEL     DEPNTW        KWDNTW                         *NETWORK
067200100127     C                   MOVEL     SDCTSP        KWDTSP                         *Tipo Servizio
067300070531     c                   if        PARWfdpv = 'S'
067400100203     C     KEYDEL        CHAIN     WFdfpv1L                           99
067500070531     c                   else
067600100127     C     KEYDELtsp     CHAIN     WFDEA01L                           99
067700070531     c                   end
067800000000     C***
067900000000     C* I M M I S S I O N E
068000000000     C***
068100000000IF  1C     *IN99         IFEQ      '1'
068200100127     c                   CLEAR                   WFDEA000
068300000000     C                   MOVEL     DEPDIV        WFDDIV                         *DIVISIONE
068400000000     C                   Z-ADD     DEPARE        WFDARE                         *AREA
068500000000     C                   Z-ADD     DEPFIL        WFDFIL                         *FILIALE
068600000000     C                   Z-ADD     DEPAGE        WFDAGE                         *COMMERCIALE
068700000000     C                   MOVEL     DEPCLV        WFDCLV                         *CODICE QUALITA'
068800100127     C                   Z-ADD     UNIcli        WFDCLI                         *CLIENTE
068900000824     C                   Z-ADD     SDCANN        WFDANN                         *ANNO
069000000824     C                   Z-ADD     SDCMES        WFDMES                         *MESE
069100000824     C                   MOVEL     SDCFPE        WFDFPE                         *FASCIA DI PESO
069200000824     C                   MOVEL     DEPIOE        WFDIOE                         *ITALIA O ESTERO
069300000000     C                   Z-ADD     DEPRGF        WFDRGF                         *RAGGRUPP FIL AGENTE
069400000000     C                   MOVEL     DEPRGD        WFDRGD                         *DESCRIZ RAGGRUP.TO
069500000000     C                   MOVEL     DEPDIC        WFDDIC                         DIVISIONE COMMERCIAL
069600000000     C                   Z-ADD     SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
069700070531     C                   z-add     depPKG        WFDpkg                         *Peso
069800070531     C                   z-add     depVOL        WFDvol                         *Volume
069900000000     C                   Z-ADD     DEPRRN        WFDRRN                         RIC REALE NETTO DELT
070000000000     C                   Z-ADD     SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
070100070720      * --
070200000000     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
070300070720      * --
070400980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
070500020315     C                   MOVEL     DEPNTW        WFDNTW                         *NETWORK
070600100127      * --
070700100127     C                   move      SDCTSP        WFDTSP                         Tipo Servizio
070800100127      * --
070900070531     c                   if        PARWfdpv = 'S'
071000100203     C                   WRITE     WFdfpv00
071100070531     c                   else
071200100127     C                   WRITE     WFDEA000
071300070531     c                   end
071400000000X   1C                   ELSE
071500000000     C***
071600000000     C* V A R I A Z I O N E
071700000000     C***
071800000000     C     WFDNSP        ADD       SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
071900070531     C     WFDpkg        add       depPKG        WFDpkg                         *Peso
072000070531     C     WFDvol        add       depVOL        WFDvol                         *Volume
072100000000     C     WFDRRN        ADD       DEPRRN        WFDRRN                         RIC REALE NETTO DELT
072200000000     C     WFDRPN        ADD       SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
072300980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
072400070720      * ---
072500070720      * solo x unificante
072600070720      *?  Attenzione: se lanciata x Unificante, occorre determinare se il codice  ?
072700070720      *?    unificante è nuovo o vecchio considerando tutti i suoi figli e,  ?
072800070821      *?    qualora uno di essi sia considerato vecchio, sarà quest'ultimo
072900070821      *?     a dare la caratteristica di vecchio al codice Unificante Padre.
073000070720      *?  >> SU SISDO l'info di Nuovo/Vecchio è sempre per singolo Cliente. <<
073100070723     c                   if        parSCE = 'U'
073200070723     c                   if        wfdCLN = *blank or
073300070723     c                             wfdCLN = 'Y' and DEPCLN = 'S'
073400070720     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
073500070720     c                   end
073600070723     c                   end
073700070720      * ---
073800070531     c                   if        PARWfdpv = 'S'
073900100203     C                   UPDATE    WFdfpv00
074000070531     c                   else
074100100127     C                   UPDATE    WFDEA000
074200070531     c                   end
074300000000E   1C                   ENDIF
074400000000     C*
074500000000     C                   ENDSR
074600000824     C*------------------------------------------------------------------------*
074700000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
074800000824     C*------------------------------------------------------------------------*
074900000825     C     AGGWFD        BEGSR
075000000825     C*
075100000825     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
075200070531     c                   if        PARWfdpv = 'S'
075300100203     C     *LOVAL        SETLL     WFdfpv4L
075400100203     C                   READ(N)   WFdfpv4L                               99
075500070531     c                   else
075600100127     C     *LOVAL        SETLL     WFDEa04L
075700100127     C                   READ(N)   WFDEa04L                               99
075800070531     c                   end
075900000825     C*
076000000825     C* CICLO FINO A FINE FILE
076100000825DO  1C     *IN99         DOWEQ     *OFF
076200000825     C*
076300000825     C* OPERAZIONE PER NUOVO CLIENTE
076400000825     C                   EXSR      NEWCLI
076500000825     C*
076600000825     C* CICLO FINO A ROTTURA CLIENTE
076700000825DO  2C     *IN99         DOWEQ     *OFF
076800100127     C     UNIcli        ANDEQ     WFDCLI
076900070723      *
077000070723      *
077100000825IF  3C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
077200000825     C     WFDDAT        ANDLE     PARDA2
077300100127     C* MEMORIZZA I DATI -WFDEA-
077400000825     C                   EXSR      MEMWFD
077500000825E   3C                   ENDIF
077600000825     C*
077700000825     C* LETTURA SUCCESSIVA DATI CLIENTE
077800070531     c                   if        PARWfdpv = 'S'
077900100203     C                   READ(N)   WFdfpv4L                               99
078000070531     c                   else
078100100127     C                   READ(N)   WFDEa04L                               99
078200070531     c                   end
078300070723     C*
078400000825E   2C                   ENDDO                                                  *ROTTURA CLIENTE
078500000825     C*
078600000825     C* CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
078700000825     C                   EXSR      MODCLI
078800000825     C*
078900000825     C* SI RIPOSIZIONA E LEGGE I DATI DEL CLIENT SUCCESSIVO
079000070531     c                   if        PARWfdpv = 'S'
079100100203     C     UNIcli        SETGT     WFdfpv4L
079200100203     C                   READ(N)   WFdfpv4L                               99
079300070531     c                   else
079400100127     C     UNIcli        SETGT     WFDEa04L
079500100127     C                   READ(N)   WFDEa04L                               99
079600070531     c                   end
079700000825E   1C                   ENDDO
079800000824     C*
079900000824     C                   ENDSR
080000000825     C*------------------------------------------------------------------------*
080100100127     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE -WFDEA-
080200000825     C*------------------------------------------------------------------------*
080300000825     C     NEWCLI        BEGSR
080400000825     C*
080500000825     C* MEMORIZZA IL NUOVO CLIENTE IN UN DEPOSITO
080600100127     C                   Z-ADD     WFDCLI        UNIcli
080700000825     C*
080800000825     C* AZZERA LE VARIABILI DI LAVORO
080900020116     C                   Z-ADD     *ZEROS        TOTIRD           19 5          *RICAVI
081000020116     C                   Z-ADD     *ZEROS        TOTIRP           19 5
081100020116     C                   Z-ADD     *ZEROS        TOTERD           19 5
081200020116     C                   Z-ADD     *ZEROS        TOTERP           19 5
081300020116     C                   Z-ADD     *ZEROS        TOTTRD           19 5
081400020116     C                   Z-ADD     *ZEROS        TOTTRP           19 5
081500000825     C                   Z-ADD     *ZEROS        PERDEI            4 1          *% DELTA
081600000825     C                   Z-ADD     *ZEROS        PERDEE            4 1
081700000825     C                   Z-ADD     *ZEROS        PERDET            4 1
081800000825     C                   MOVEL     'S'           $OKDEI            1            *DELTA OK
081900000825     C                   MOVEL     'S'           $OKDEE            1
082000000825     C                   MOVEL     'S'           $OKDET            1
082100000825     C                   CLEAR                   SMEI                           *MM CON % SEGNALARE
082200000825     C                   CLEAR                   SMEE
082300000825     C                   CLEAR                   SMET
082400000825     C                   Z-ADD     *ZEROS        MI                2 0          *INDICE MESI
082500000825     C                   Z-ADD     *ZEROS        ME                2 0
082600000825     C                   Z-ADD     *ZEROS        MT                2 0
082700000825     C*
082800000825     C                   ENDSR
082900000825     C*------------------------------------------------------------------------*
083000000825     C* MEMWFD - MEMORIZZA I DATI DEL WFILE
083100000825     C*------------------------------------------------------------------------*
083200000825     C     MEMWFD        BEGSR
083300000825     C
083400000825     C* MEMORIZZA I RICAVI ITALIA E ESTERO DEL CLIENTE PER IL PERIODO RICHIESTO
083500000825IF  1C     WFDIOE        IFEQ      'I'                                          *ITALIA
083600000825     C                   ADD       WFDRRN        TOTIRD                         *TOT RIC REALE
083700000825     C                   ADD       WFDRPN        TOTIRP                         *TOT RIC PRESUNTO
083800000825E   1C                   ENDIF
083900000825IF  1C     WFDIOE        IFEQ      'E'                                          *ESTERO
084000000825     C                   ADD       WFDRRN        TOTERD                         *TOT RIC REALE
084100000825     C                   ADD       WFDRPN        TOTERP                         *TOT RIC PRESUNTO
084200000825E   1C                   ENDIF
084300000825IF  1C     WFDIOE        IFEQ      *BLANKS                                      *ITALIA + ESTERO
084400000825     C                   ADD       WFDRRN        TOTTRD                         *TOT RIC REALE
084500000825     C                   ADD       WFDRPN        TOTTRP                         *TOT RIC PRESUNTO
084600000825E   1C                   ENDIF
084700000825     C*
084800000825     C* CONTROLLA SE LA % DELTA DI CIASCUNA FASCIA DI PESO RIENTRA IN QUELLA RICHIESTA
084900000825IF  1C     PARSPD        IFNE      *ZEROS                                       *RICHIESTA
085000000825     C*
085100000825     C* ITALIA
085200000825IF  2C     WFDIOE        IFEQ      'I'
085300000825IF  3C     WFDRRN        IFGT      *ZEROS
085400000825     C     WFDRPN        ORGT      *ZEROS
085500020116     C                   Z-ADD     WFDRRN        WRRW             17 5          *RICAVO REALE
085600020116     C                   Z-ADD     WFDRPN        WRPW             17 5          *RICAVO PRESUNTO
085700000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
085800000825     C                   EXSR      CALDEL
085900000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
086000000825     C                   SETOFF                                           97
086100000825     C     WFDMES        LOOKUP    SMEI                                   97
086200000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
086300000825     C                   ADD       1             MI
086400000825     C                   Z-ADD     WFDMES        SMEI(MI)                       *MESE DA SEGNALARE
086500000825E   5C                   ENDIF
086600000825E   4C                   ENDIF
086700000825E   3C                   ENDIF
086800000825E   2C                   ENDIF
086900000825     C* ESTERO
087000000825IF  2C     WFDIOE        IFEQ      'E'
087100000825IF  3C     WFDRRN        IFGT      *ZEROS
087200000825     C     WFDRPN        ORGT      *ZEROS
087300020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
087400020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
087500000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
087600000825     C                   EXSR      CALDEL
087700000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
087800000825     C                   SETOFF                                           97
087900000825     C     WFDMES        LOOKUP    SMEE                                   97
088000000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
088100000825     C                   ADD       1             ME
088200000825     C                   Z-ADD     WFDMES        SMEE(ME)                       *MESE DA SEGNALARE
088300000825E   5C                   ENDIF
088400000825E   4C                   ENDIF
088500000825E   3C                   ENDIF
088600000825E   2C                   ENDIF
088700000825     C* ITALIA + ESTERO
088800000825IF  2C     WFDIOE        IFEQ      *BLANKS
088900000825IF  3C     WFDRRN        IFGT      *ZEROS
089000000825     C     WFDRPN        ORGT      *ZEROS
089100020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
089200020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
089300000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
089400000825     C                   EXSR      CALDEL
089500000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
089600000825     C                   SETOFF                                           97
089700000825     C     WFDMES        LOOKUP    SMET                                   97
089800000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
089900000825     C                   ADD       1             MT
090000000825     C                   Z-ADD     WFDMES        SMET(MT)                       *MESE DA SEGNALARE
090100000825E   5C                   ENDIF
090200000825E   4C                   ENDIF
090300000825E   3C                   ENDIF
090400000825E   2C                   ENDIF
090500000825     C*
090600000825E   1C                   ENDIF
090700000825     C*
090800000825     C                   ENDSR
090900000825     C*------------------------------------------------------------------------*
091000000825     C* MODCLI - CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
091100000825     C*------------------------------------------------------------------------*
091200000825     C     MODCLI        BEGSR
091300000825     C*
091400000825     C* CALCOLA IL DELTA DEL CLIENTE
091500000825     C                   EXSR      CALCLI
091600000825     C*
091700000825     C* RILEGGE I DATI DEL CLIENTE -->
091800000825     C* - CANCELLA IL CLIENTE SE NON RIENTRA NEL RANGE DELTA RICHIESTO
091900000825     C* - AGGIORNA IL CLIENTE SE HA UNA % DELTA FASCE DA SEGNALARE
092000070531     c                   if        PARWfdpv = 'S'
092100100203     C     UNIcli        SETLL     WFdfpv4L
092200100203     C     UNIcli        READE     WFdfpv4L                               98
092300070531     c                   else
092400100127     C     UNIcli        SETLL     WFDEa04L
092500100127     C     UNIcli        READE     WFDEa04L                               98
092600070531     c                   end
092700070531      *
092800000825DO  1C     *IN98         DOWEQ     *OFF
092900000825     C*
093000000825     C* ITALIA
093100000825IF  2C     WFDIOE        IFEQ      'I'
093200000825IF  3C     $OKDEI        IFEQ      'N'                                          *DELTA NON OK
093300070531     c                   if        PARWfdpv = 'S'
093400100203     C                   DELETE    WFDfpv
093500070531     c                   else
093600100127     C                   DELETE    WFDEa4
093700070531     c                   end
093800000825X   3C                   ELSE                                                   *DELTA OK
093900000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
094000000828     C     WFDDAT        ANDLE     PARDA2
094100000825     C                   SETOFF                                           97
094200000825     C     WFDMES        LOOKUP    SMEI                                   97
094300000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
094400000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
094500070531     c                   if        PARWfdpv = 'S'
094600100203     C                   UPDATE    WFDfpv
094700070531     c                   else
094800100127     C                   UPDATE    WFDEa4
094900070531     c                   end
095000000828E   5C                   ENDIF
095100000828E   4C                   ENDIF
095200000825E   3C                   ENDIF
095300000825E   2C                   ENDIF
095400000825     C*
095500000825     C* ESTERO
095600000825IF  2C     WFDIOE        IFEQ      'E'
095700000825IF  3C     $OKDEE        IFEQ      'N'                                          *DELTA NON OK
095800070531     c                   if        PARWfdpv = 'S'
095900100203     C                   DELETE    WFDfpv
096000070531     c                   else
096100100127     C                   DELETE    WFDEa4
096200070531     c                   end
096300000825X   3C                   ELSE                                                   *DELTA OK
096400000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
096500000828     C     WFDDAT        ANDLE     PARDA2
096600000825     C                   SETOFF                                           97
096700000825     C     WFDMES        LOOKUP    SMEE                                   97
096800000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
096900000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
097000070531     c                   if        PARWfdpv = 'S'
097100100203     C                   UPDATE    WFDfpv
097200070531     c                   else
097300100127     C                   UPDATE    WFDEa4
097400070531     c                   end
097500000828E   5C                   ENDIF
097600000825E   4C                   ENDIF
097700000825E   3C                   ENDIF
097800000825E   2C                   ENDIF
097900000825     C*
098000000825     C* ITALIA + ESTERO
098100000825IF  2C     WFDIOE        IFEQ      *BLANKS
098200000825IF  3C     $OKDET        IFEQ      'N'                                          *DELTA NON OK
098300070531     c                   if        PARWfdpv = 'S'
098400100203     C                   DELETE    WFDfpv
098500070531     c                   else
098600100127     C                   DELETE    WFDEa4
098700070531     c                   end
098800000825X   3C                   ELSE                                                   *DELTA OK
098900000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
099000000828     C     WFDDAT        ANDLE     PARDA2
099100000825     C                   SETOFF                                           97
099200000825     C     WFDMES        LOOKUP    SMET                                   97
099300000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
099400000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
099500070531     c                   if        PARWfdpv = 'S'
099600100203     C                   UPDATE    WFDfpv
099700070531     c                   else
099800100127     C                   UPDATE    WFDEa4
099900070531     c                   end
100000000828E   5C                   ENDIF
100100000825E   4C                   ENDIF
100200000825E   3C                   ENDIF
100300000825E   2C                   ENDIF
100400000825     C*
100500070531     c                   if        PARWfdpv = 'S'
100600100203     C     UNIcli        READE     WFdfpv4L                               98
100700070531     c                   else
100800100127     C     UNIcli        READE     WFDEa04L                               98
100900070531     c                   end
101000000825E   1C                   ENDDO                                                  *DINE DATI CLIENTE
101100000825     C*
101200000825     C                   ENDSR
101300000825     C*------------------------------------------------------------------------*
101400000825     C* CALCOLA I DELTA SUI DATI MEMORIZZATI
101500000825     C*------------------------------------------------------------------------*
101600000825     C     CALCLI        BEGSR
101700000825     C*
101800000825     C* CALCOLA I DELTA E CONTROLLA SE RIENTRANO NEL RANGE RICHIESTO
101900000825IF  1C     PARPDD        IFNE      -999,9                                       *RICHIESTO
102000000825     C     PARPDA        ORNE      +999,9
102100000825     C*
102200000825     C* DELTA ITALIA
102300000825IF  2C     TOTIRD        IFGT      *ZEROS
102400000825     C     TOTIRP        ORGT      *ZEROS
102500020116     C                   Z-ADD     TOTIRD        WRRW                           *RICAVO REALE
102600020116     C                   Z-ADD     TOTIRP        WRPW                           *RICAVO PRESUNTO
102700000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
102800000825     C                   EXSR      CALDEL
102900000825     C                   Z-ADD     WDPW          PERDEI                         *% DELTA ITALIA
103000000825IF  3C     PERDEI        IFLT      PARPDD                                       *DELTA <> RANGE
103100000825     C     PERDEI        ORGT      PARPDA
103200000825     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
103300000825E   3C                   ENDIF
103400010313X   2C                   ELSE                                                   *NO RICAVI ITALIA
103500010313     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
103600000825E   2C                   ENDIF
103700000825     C* DELTA ESTERO
103800000825IF  2C     TOTERD        IFGT      *ZEROS
103900000825     C     TOTERP        ORGT      *ZEROS
104000020116     C                   Z-ADD     TOTERD        WRRW                           *RICAVO REALE
104100020116     C                   Z-ADD     TOTERP        WRPW                           *RICAVO PRESUNTO
104200000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
104300000825     C                   EXSR      CALDEL
104400000825     C                   Z-ADD     WDPW          PERDEE                         *% DELTA ESTERO
104500000825IF  3C     PERDEE        IFLT      PARPDD                                       *DELTA <> RANGE
104600000825     C     PERDEE        ORGT      PARPDA
104700000825     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
104800000825E   3C                   ENDIF
104900010313X   2C                   ELSE                                                   *NO RICAVI ESTERO
105000010313     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
105100000825E   2C                   ENDIF
105200000825     C* DELTA ITA + EST
105300000825IF  2C     TOTTRD        IFGT      *ZEROS
105400000825     C     TOTTRP        ORGT      *ZEROS
105500020116     C                   Z-ADD     TOTTRD        WRRW                           *RICAVO REALE
105600020116     C                   Z-ADD     TOTTRP        WRPW                           *RICAVO PRESUNTO
105700000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
105800000825     C                   EXSR      CALDEL
105900000825     C                   Z-ADD     WDPW          PERDET                         *% DELTA ESTERO
106000000825IF  3C     PERDET        IFLT      PARPDD                                       *DELTA <> RANGE
106100000825     C     PERDET        ORGT      PARPDA
106200000825     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
106300000825E   3C                   ENDIF
106400010313X   2C                   ELSE                                                   *NO RICAVI TOTALI
106500010313     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
106600000825E   2C                   ENDIF
106700000825     C*
106800000825E   1C                   ENDIF
106900000825     C*
107000000825     C                   ENDSR
107100000824     C*------------------------------------------------------------------------*
107200000824     C* CALDEL - CALCOLA IL DELTA
107300000824     C*          INPUT  --> WRRW: RICAVO REALE
107400970821     C*                     WRPW: RICAVO PRESUNTO
107500970821     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
107600970821     C*------------------------------------------------------------------------*
107700970821     C     CALDEL        BEGSR
107800970821     C*--------------------
107900970821     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
108000970821     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
108100970821     C*--------------------
108200970821     C*
108300970821     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
108400020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
108500970821     C     GAP           MULT      100           GAP                            (GAP x 100)
108600970821     C*
108700970821     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
108800970821IF  1C     WRRW          IFEQ      *ZEROS
108900000825     C                   Z-ADD     1             WRRW
109000970821E   1C                   ENDIF
109100970821     C*
109200970821     C     GAP           DIV       WRRW          GAP
109300000825     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
109400970821     C*
109500970821     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
109600970821     C     WDPW          COMP      0                                    28
109700970821     C* POSITIVO
109800970821IF  1C     *IN28         IFEQ      *OFF
109900970821IF  2C     GAP           IFGE      1000
110000970821     C                   Z-ADD     +999,9        WDPW
110100970821E   2C                   ENDIF
110200970821     C* NEGATIVO
110300970821X   1C                   ELSE
110400970821IF  2C     GAP           IFLE      -1000
110500970821     C                   Z-ADD     -999,9        WDPW
110600970821E   2C                   ENDIF
110700970821E   1C                   ENDIF
110800970821     C*
110900970821     C                   ENDSR
111000000000     C*------------------------------------------------------------------------*
111100000000     C* STAERR - STAMPA RIGA DI ERRORE PER AGENTE INESISTENTE O ANNULLATO
111200000000     C*------------------------------------------------------------------------*
111300000000     C     STAERR        BEGSR
111400000000     C*
111500000000IF  1C     *INOF         IFEQ      *ON
111600000000     C                   EXCEPT    TESTA
111700000000     C                   SETOFF                                       OF
111800000000E   1C                   ENDIF
111900000000     C*
112000000000     C* AGENTE NON ANCORA STAMPATO, SCRIVO ERRORE
112100000000     C                   SETOFF                                       97
112200000000     C     DEPAGE        LOOKUP    ERG                                    97
112300000000IF  1C     *IN97         IFEQ      *OFF
112400000000     C                   ADD       1             I                 3 0
112500000000     C                   Z-ADD     DEPAGE        ERG(I)                         *MEMORIZZA AGENTE
112600000000     C                   EXCEPT    ERROR                                        *STAMPA L'ERRORE
112700000000E   1C                   ENDIF
112800000000     C*
112900000000     C                   ENDSR
113000000000     C*------------------------------------------------------------------------*
113100000000     C* CARTAB - CARICA I DATI IN TABELLE
113200000000     C*------------------------------------------------------------------------*
113300000000     C     CARTAB        BEGSR
113400000000     C*--------------------
113500000000     C* ORGANIGRAMMA
113600000000     C*--------------------
113700000000     C                   Z-ADD     *ZEROS        F
113800000000     C                   Z-ADD     *ZEROS        KORFIL
113900000000     C     KEYORG        SETLL     AZORG01L
114000000000     C                   READ      AZORG01L                               99
114100000000DO  1C     *IN99         DOWEQ     '0'
114200980119IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
114300980119IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
114400000000     C                   ADD       1             F
114500000000     C                   Z-ADD     ORGFIL        FIL(F)
114600000000     C                   MOVEL     ORGDES        DFIL(F)
114700020906     C                   Z-ADD     1             KTBKUT
114800000000     C                   MOVEL     '05'          KTBCOD
114900000000     C                   MOVEL     *BLANKS       KTBKEY
115000000000     C                   MOVEL     ORGCAR        KTBKEY
115100000824     C     KEYTAB        CHAIN     TABEL00F                           99
115200980119IF  5C     *IN99         IFEQ      *OFF
115300000000     C                   Z-ADD     ORGCAR        ARE(F)
115400000000     C                   MOVEL     TBLUNI        DARE(F)
115500980119E   5C                   ENDIF
115600020906     C                   Z-ADD     1             KTBKUT
115700000000     C                   MOVEL     '17'          KTBCOD
115800000000     C                   MOVEL     *BLANKS       KTBKEY
115900000000     C                   MOVEL     ORGFL3        KTBKEY
116000000824     C     KEYTAB        CHAIN     TABEL00F                           99
116100980119IF  5C     *IN99         IFEQ      *OFF
116200000000     C                   MOVEL     ORGFL3        DIV(F)
116300000000     C                   MOVEL     TBLUNI        DDIV(F)
116400980119E   5C                   ENDIF
116500980119E   3C                   ENDIF
116600980119E   2C                   ENDIF
116700000000     C                   READ      AZORG01L                               99
116800000000E   1C                   ENDDO
116900000000     C*--------------------
117000000000     C* COMMERCIALI
117100000000     C*--------------------
117200000000     C                   Z-ADD     *ZEROS        C
117300020906     C                   Z-ADD     1             KTBKUT
117400000000     C                   MOVEL     '01'          KTBCOD
117500981103     C     KEYTA2        SETLL     TABEL00F
117600000824     C     KEYTA2        READE     TABEL00F                               99
117700000000DO  1C     *IN99         DOWEQ     *OFF
117800970825IF  2C     TBLFLG        IFEQ      *BLANKS
117900000000     C                   ADD       1             C
118000000000     C                   MOVEL     TBLUNI        DS01
118100000000     C                   MOVEL     TBLKEY        AGE(C)
118200000000     C                   MOVEL     §01RGF        RGF(C)
118300000000     C                   MOVEL     §01AGE        DAGE(C)
118400970825E   2C                   ENDIF
118500000824     C     KEYTA2        READE     TABEL00F                               99
118600000000E   1C                   ENDDO
118700000000     C*-------------------------
118800000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
118900000000     C*-------------------------
119000000000     C                   Z-ADD     *ZEROS        G
119100020906     C                   Z-ADD     1             KTBKUT
119200000000     C                   MOVEL     '2W'          KTBCOD
119300000000     C     KEYTA2        SETLL     TABEL00F
119400000824     C     KEYTA2        READE     TABEL00F                               99
119500000000DO  1C     *IN99         DOWEQ     *OFF
119600000000IF  2C     TBLFLG        IFEQ      *BLANKS
119700000000     C                   ADD       1             G
119800000000     C                   MOVEL     TBLKEY        FLAB(G)
119900000000     C                   MOVEL     TBLUNI        DS2W
120000000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
120100000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
120200000000E   2C                   ENDIF
120300000824     C     KEYTA2        READE     TABEL00F                               99
120400000000E   1C                   ENDDO
120500990217     C*--------------------
120600000000     C* FASCE DI PESO
120700990217     C*--------------------
120800990609IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
120900990217     C                   MOVEL     'SDC'         WSX               3
121000990217X   1C                   ELSE                                                   *FASCE DAL '99
121100020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO AL 2001
121200020325     C                   MOVEL     'S9C'         WSX
121300020325X   2C                   ELSE                                                   *FASCE DAL 2002
121400020325     C                   MOVEL     'S2C'         WSX
121500020325E   2C                   ENDIF
121600020325E   1C                   ENDIF
121700970821     C                   Z-ADD     *ZEROS        P
121800020906     C                   Z-ADD     1             KTBKUT
121900000000     C                   MOVEL     '2L'          KTBCOD
122000000000     C     KEYTA2        SETLL     TABEL00F
122100000824     C     KEYTA2        READE     TABEL00F                               99
122200000000DO  1C     *IN99         DOWEQ     *OFF
122300000000IF  2C     TBLFLG        IFEQ      *BLANKS
122400990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
122500000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
122600990217IF  3C     FILEST        IFEQ      WSX
122700000000     C                   MOVEL     TBLUNI        DS2L
122800000309IF  4C     §2LKGI        IFGE      5000
122900000309     C     PARDAT        ANDLT     200001
123000000309     C                   ELSE
123100970821     C                   ADD       1             P
123200970821     C                   MOVE      STACOD        FPE(P)                         *...+XX
123300000309E   4C                   ENDIF
123400000000E   3C                   ENDIF
123500000000E   2C                   ENDIF
123600000824     C     KEYTA2        READE     TABEL00F                               99
123700000000E   1C                   ENDDO
123800020314     C*-------------------------
123900020314     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
124000020314     C*-------------------------
124100020314     C                   Z-ADD     *ZEROS        R
124200020906     C                   Z-ADD     1             KTBKUT
124300020314     C                   MOVEL     '2M'          KTBCOD
124400020314     C     KEYTA2        SETLL     TABEL00F
124500020314     C     KEYTA2        READE     TABEL00F                               99
124600020314DO  1C     *IN99         DOWEQ     *OFF
124700020314IF  2C     TBLFLG        IFEQ      *BLANKS
124800020314     C                   ADD       1             R
124900020314     C                   MOVEL     TBLKEY        RGREG(R)
125000020314     C                   MOVEL     TBLUNI        DS2M
125100020314     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
125200020314E   2C                   ENDIF
125300020314     C     KEYTA2        READE     TABEL00F                               99
125400020314E   1C                   ENDDO
125500020404     C*----------
125600020404     C* NETWORK AZIENDALI
125700020404     C*----------
125800020404     C                   Z-ADD     *ZEROS        X
125900020404     C                   EVAL      TBECOD = 'NTW'
126000020404     C     KEYTBE        SETLL     TNTBE01L
126100020404     C     KEYTBE        READE     TNTBE01L
126200020404     C                   DOW       NOT %EOF(TNTBE01L)
126300020404     C                   ADD       1             X
126400020404     C                   MOVEL     TBEUNI        DNTW
126500020404     C                   EVAL      NTWCOD(X) = TBEKE1
126600020404     C                   EVAL      NTWORD(X) = §NTWORD
126700020404     C     KEYTBE        READE     TNTBE01L
126800020404     C                   ENDDO
126900000000     C*
127000000000     C                   ENDSR
127100970821     C*------------------------------------------------------------------------*
127200970821     C* AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
127300970821     C*------------------------------------------------------------------------*
127400970821     C     AMELAB        BEGSR
127500970821     C*---
127600970821     C* ANNO INIZIALE UGUALE AD ANNO FINALE
127700970821     C*---
127800970821IF  1C     WAA2          IFEQ      WAA
127900970821     C*
128000970821     C* CALCOLO I MESI DI DIFFERENZA
128100970821DO  2C     WMM           DOWLE     WMM2
128200970821     C                   ADD       1             JI
128300970821     C                   Z-ADD     WAA           ANN(JI)
128400970821     C                   Z-ADD     WMM           MES(JI)
128500970821     C                   ADD       1             WMM
128600970821E   2C                   ENDDO
128700970821     C*
128800970821     C* IL RESTO DELLA SCHIERA A ZERO
128900970821DO  2C     JI            DOWLT     12
129000970821     C                   ADD       1             JI
129100970821     C                   Z-ADD     *ZEROS        ANN(JI)
129200970821     C                   Z-ADD     *ZEROS        MES(JI)
129300970821E   2C                   ENDDO
129400970821     C*---
129500970821     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
129600970821     C*---
129700970821X   1C                   ELSE
129800970821     C*
129900970821     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
130000970821DO  2C     WMM           DOWLE     12
130100970821     C                   ADD       1             JI
130200970821     C                   Z-ADD     WAA           ANN(JI)
130300970821     C                   Z-ADD     WMM           MES(JI)
130400970821     C                   ADD       1             WMM
130500970821E   2C                   ENDDO
130600970821     C*
130700970821     C* MEMORIZZO I MESI DELL'ANNO FINALE
130800970821     C                   Z-ADD     01            WMM
130900970821DO  2C     WMM           DOWLE     WMM2
131000970821     C                   ADD       1             JI
131100970821     C                   Z-ADD     WAA2          ANN(JI)
131200970821     C                   Z-ADD     WMM           MES(JI)
131300970821     C                   ADD       1             WMM
131400970821E   2C                   ENDDO
131500970821     C*
131600970821     C* IL RESTO DELLA SCHIERA A ZERO
131700970821DO  2C     JI            DOWLT     12
131800970821     C                   ADD       1             JI
131900970821     C                   Z-ADD     *ZEROS        ANN(JI)
132000970821     C                   Z-ADD     *ZEROS        MES(JI)
132100970821E   2C                   ENDDO
132200970821E   1C                   ENDIF
132300970821     C*
132400970821     C                   ENDSR
132500980403     C*------------------------------------------------------------------------*
132600980403     C* FINSR - OPERAZIONI FINALI
132700980403     C*------------------------------------------------------------------------*
132800980403     C     FINSR         BEGSR
132900980403     C*
133000980403     C* CHIUDE I FILE DEI PGM CHIAMATI
133100980403     C                   CLEAR                   BS69DS
133200980403     C                   CLEAR                   ACODS
133300980403     C                   CLEAR                   INDDS
133400980403     C                   CLEAR                   CLPDS
133500980403     C                   CLEAR                   CLSDS
133600980403     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
133700980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
133800980403     C                   CALL      'TIBS69R'
133900980403     C                   PARM                    BS69DS
134000980403     C                   PARM                    ACODS
134100980403     C                   PARM                    INDDS
134200980403     C                   PARM                    CLPDS
134300980403     C                   PARM                    CLSDS
134400980403     C*
134500980403     C                   ENDSR
134600000000     C*------------------------------------------------------------------------*
134700000000     C* *INZSR - IMPOSTAZIONI INIZIALI
134800000000     C*------------------------------------------------------------------------*
134900000000     C     *INZSR        BEGSR
135000000000     C*
135100000000     C* PARAMETRI IN INPUT AL PGM
135200000000     C     *ENTRY        PLIST
135300000000     C                   PARM                    KPJBA
135400000000     C                   MOVEL     KPJBU         PARAM
135500070531     c*
135600070531     c* apertura condizionata dei 2 files da generare
135700070531     c                   if        PARWfdpv = 'S'
135800100203     c                   open      WFdfpv1l
135900100203     c                   open      WFdfpv4l
136000100203     c                   open      WFdfpv5l
136100070531     c                   else
136200100127     c                   open      WFdea01l
136300100127     c                   open      WFdea04l
136400100127     c                   open      WFdea05l
136500100127     c                   open      WFdeav1l
136600070531     c                   end
136700000000     C*
136800000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
136900000823     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
137000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
137100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
137200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
137300990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC01L
137400000000     C     *LIKE         DEFINE    SDCANN        KSCANN
137500000000     C     *LIKE         DEFINE    SDCMES        KSCMES
137600990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO01L
137700000000     C     *LIKE         DEFINE    SDOANN        KSOANN
137800000000     C     *LIKE         DEFINE    SDOMES        KSOMES
137900100127     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEa01L
138000000000     C     *LIKE         DEFINE    WFDARE        KWDARE
138100000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
138200000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
138300000000     C     *LIKE         DEFINE    WFDCLV        KWDCLV
138400000823     C     *LIKE         DEFINE    WFDCLI        KWDCLI
138500000000     C     *LIKE         DEFINE    WFDANN        KWDANN
138600000000     C     *LIKE         DEFINE    WFDMES        KWDMES
138700000824     C     *LIKE         DEFINE    WFDFPE        KWDFPE
138800000000     C     *LIKE         DEFINE    WFDIOE        KWDIOE
138900020315     C     *LIKE         DEFINE    WFDNTW        KWDNTW
139000100127     C     *LIKE         DEFINE    WFDTSP        KWDTSP
139100000000     C*
139200000000     C* DEFINIZIONI DEPOSITI E VARIABILI
139300000000     C     *LIKE         DEFINE    CLPKSC        DEPKSC
139400100127     C     *LIKE         DEFINE    CLPKSC        UNIcli
139500000000     C     *LIKE         DEFINE    CLPAGE        DEPAGE
139600000000     C     *LIKE         DEFINE    ORGFIL        DEPFIL
139700000000     C     *LIKE         DEFINE    ORGCAR        DEPARE
139800000000     C     *LIKE         DEFINE    ORGFL3        DEPDIV
139900000000     C     *LIKE         DEFINE    ORGFL3        DEPDIC
140000000000     C     *LIKE         DEFINE    WFDRRN        DEPRRN
140100000000     C     *LIKE         DEFINE    CLPAGE        DEPRGF
140200000000     C     *LIKE         DEFINE    CLPCLV        DEPCLV
140300000000     C     *LIKE         DEFINE    §01AGE        DEPRGD
140400000000     C     *LIKE         DEFINE    SDOCLN        DEPCLN
140500970821     C     *LIKE         DEFINE    WFDIOE        DEPIOE
140600020315     C     *LIKE         DEFINE    WFDNTW        DEPNTW
140700070531     C     *LIKE         DEFINE    WFDRRN        DEPRRN
140800100203     C* x il WFdfpv0F
140900070531     C     *LIKE         DEFINE    WFDpkg        DEPpkg
141000070531     C     *LIKE         DEFINE    WFDvol        DEPvol
141100070723     C     *LIKE         DEFINE    wfdANN        depANN
141200070723     C     *LIKE         DEFINE    wfdMES        depMES
141300000825     C*
141400000825     C* DEFINIZIONI VARIABILI DI LAVORO
141500000825     C     *LIKE         DEFINE    PARSPD        MENSPD
141600000000     C                   MOVEL     '0'           $FINE             1            *FINE PROGRAMMA
141700980601     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
141800980601     C                   MOVEL     '0'           PCLNEW            1            *CLIENTE NUOVO PRECE
141900000000     C                   MOVEL     '0'           CCLNEW            1            *CLIENTE NUOVO CORRE
142000000000     C                   Z-ADD     *ZEROS        C                 4 0          *PER COMMERCIALI
142100000000     C                   Z-ADD     *ZEROS        G                 4 0          *PER FLAG TIPI BOLLA
142200000000     C                   Z-ADD     *ZEROS        J                 4 0          *PER MESI ELABORABIL
142300000000     C                   Z-ADD     *ZEROS        F                 4 0          *PER ORGANIGRAMMA
142400970821     C                   Z-ADD     *ZEROS        P                 4 0          *PER FASCE DI PESO
142500970821     C                   Z-ADD     *ZEROS        A                 4 0          *PER ANNI
142600970821     C                   Z-ADD     *ZEROS        M                 4 0          *PER MESI
142700020404     C                   Z-ADD     *ZEROS        X                 3 0          *PER RAGGRUPPAMENTI REGIONI/NETWORK
142800020404     C                   Z-ADD     *ZEROS        R                 3 0          *PER NETWORK
142900000000     C                   SETON                                        OF        *OVERFLOW
143000000000     C*
143100000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
143200000000     C     KEYORG        KLIST
143300000000     C                   KFLD                    KORFIL                         *FILIALE
143400000000     C*
143500000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
143600000000     C     KEYTAB        KLIST
143700000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
143800000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
143900000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
144000000000     C*
144100000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
144200000000     C     KEYTA2        KLIST
144300000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
144400000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
144500020404     C*
144600020404     C* CHIAVE LETTURA TNTBE00F - PARZIALE
144700020404     C     KEYTBE        KLIST
144800020404     C                   KFLD                    TBECOD
144900000000     C*
145000990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
145100000000     C     KEYSDC        KLIST
145200000000     C                   KFLD                    KSCKSC                         *CODICE CLIENTE
145300000000     C                   KFLD                    KSCANN                         *ANNO
145400000000     C                   KFLD                    KSCMES                         *MESE
145500000000     C*
145600990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
145700000000     C     KEYSDO        KLIST
145800000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
145900000000     C*
146000990609     C* CHIAVE LETTURA SISDO01L - COMPLETA
146100000000     C     KE2SDO        KLIST
146200000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
146300000000     C                   KFLD                    KSOANN                         *ANNO
146400000000     C                   KFLD                    KSOMES                         *MESE
146500100127     C*
146600100127     C* CHIAVE LETTURA WFdeaV1L - COMPLETA
146700100127     C     KEYdeaV       KLIST
146800100215     C                   KFLD                    WFVSVR                         *VARIA
146900100127     C                   KFLD                    WFVKSC                         *CODICE CLIENTE
147000100127     C                   KFLD                    WFVANN                         *ANNO
147100100127     C                   KFLD                    WFVMES                         *MESE
147200100127     C*
147300100127     C* CHIAVE LETTURA WFDEa01L - COMPLETA
147400000000     C     KEYDEL        KLIST
147500000000     C                   KFLD                    KWDDIV                         *DIVISIONE
147600000000     C                   KFLD                    KWDARE                         *AREA
147700000000     C                   KFLD                    KWDFIL                         *FILIALE
147800000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
147900000000     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
148000000823     C                   KFLD                    KWDCLI                         *CLIENTE
148100000000     C                   KFLD                    KWDANN                         *ANNO
148200000000     C                   KFLD                    KWDMES                         *MESE
148300000824     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
148400000000     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
148500020614     C                   KFLD                    KWDNTW                         *NETWORK
148600000823     C*
148700100127     C     KEYDELtsp     KLIST
148800100127     C                   KFLD                    KWDDIV                         *DIVISIONE
148900100127     C                   KFLD                    KWDARE                         *AREA
149000100127     C                   KFLD                    KWDFIL                         *FILIALE
149100100127     C                   KFLD                    KWDAGE                         *COMMERCIALE
149200100127     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
149300100127     C                   KFLD                    KWDCLI                         *CLIENTE
149400100127     C                   KFLD                    KWDANN                         *ANNO
149500100127     C                   KFLD                    KWDMES                         *MESE
149600100127     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
149700100127     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
149800100127     C                   KFLD                    KWDNTW                         *NETWORK
149900100127     C                   KFLD                    KWDTSP                         *TIPO SERVIZIO
150000100127     C*
150100000823     C* IMNPOSTA DATA / ORA CORRENTE
150200000823     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
150300000823     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
150400000823     C                   Z-ADD     *ZEROS        G08INV
150500000823     C                   MOVEL     '0'           G08ERR
150600000823     C                   CALL      'XSRDA8'
150700000823     C                   PARM                    WLBDA8
150800000823     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
150900000000     C*
151000000825     C* CARICA TABELLE OCCORRENTI
151100000000     C                   EXSR      CARTAB
151200970821     C*
151300970821     C* CALCOLA I MESI DA ELABORARE
151400990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
151500970821     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
151600990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
151700970821     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
151800970821     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORAR
151900970821     C                   EXSR      AMELAB
152000970822     C*
152100000825     C* PRENDE IL VALORE ASSOLUTO DELLA % ALLA QUALE SEGNALARE LO SCOSTAMENTO FASCE
152200970822     C                   MLLZO     1             PARSPD                         *VALORE ASSOLUTO
152300000825     C                   Z-ADD     *ZEROS        MENSPD
152400000825     C                   Z-SUB     PARSPD        MENSPD                         *% RICHIESTA MENO
152500001122     C*
152600001122     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
152700010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
152800010725     C                   MOVE      28            WDFIN
152900081114     C*
153000081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
153100081114     C*
153200081114     C*  Ultimo giorno del mese
153300081114     C                   MOVE      WDFIN         MMGG_4            4 0
153400081114     C                   MOVEL     MMGG_4        Mm_2              2 0
153500081114     C                   MOVEl     WDFIN         Anno_4            4 0
153600081114      * se è febbraio
153700081114     c                   If        Mm_2 = 2
153800081114      *
153900081114      *  potrebbe essere bisestile 29 gg.
154000081114     c     Anno_4        div       4             risulta           3 0
154100081114     c                   mvr                     resto             3 0
154200081114      *  Se bisestile
154300081114     c                   If        resto = 0
154400081114     C                   MOVE      '29'          WDFIN
154500081114     c                   end
154600081114      *
154700081114     c                   else
154800081114      *
154900081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
155000081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
155100081114     c                                Mm_2 = 9 or Mm_2 = 11
155200081114     C                   MOVE      '30'          WDFIN
155300081114     c                   else
155400081114     C                   MOVE      '31'          WDFIN
155500081114     c                   end
155600081114      *
155700081114     c                   endIf
155800081114     C*
155900081114     C*----------- F I N E    M E S E ---------------------------*
156000000000     C*
156100000000     C                   ENDSR
156200070723     C*------------------------------------------------------------------------*
156300070723     C* Aggiorna il flag Nuovo/Acquisito x UNificante
156400070723     C*------------------------------------------------------------------------*
156500070723     C     AGGWFD_uni    BEGSR
156600070727     C*
156700070727      *? Per prima cosa deve rendere omogenei i records nell'ambito dell'anno con "Y" e "S"...  ?
156800070727      *? significa che se nell'anno sono nati 2 codici clienti nuovi mi troverei "Y" su 2 mesi ?
156900070727      *? diversi nell'ambito dello stesso Anno.                             ?
157000070727      *? ..... ovviamente prevale la "Y" più vecchia a cui seguono delle "S" x indicare il Nuovo.
157100070727      *? -------------------------------------------------------------------------- ?
157200070727      *  (esempio esplicativo)
157300070727      *               Anno 200X? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
157400070727      *? Cliente (A)...  ?      |   | Y | S | S | S | S | S | S |   | S | S |   |
157500070727      *? Cliente (B)...  ?      |   |   |   | Y | S | S |   | S | S | S |   |   |
157600070727      *? -----------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
157700070727      *? Unificante (C)         |   | Y | S | Y | S | S | S | S | S | S | S |   |                         ?
157800070727      * -->
157900070727      *? Invece deve essere così|   | Y | S | S | S | S | S | S | S | S | S |   |   ?
158000070727      *? -------------------------------------------------------------------------- ?
158100070723     C*
158200070723     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
158300070723     c                   if        PARWfdpv = 'S'
158400100203     C     *LOVAL        SETLL     WFdfpv5L
158500100203     C                   READ      WFdfpv5L                               99
158600070723     c                   else
158700100127     C     *LOVAL        SETLL     WFDEa05L
158800100127     C                   READ      WFDEa05L                               99
158900070723     c                   end
159000070723     C*
159100070723     C* CICLO FINO A FINE FILE
159200070723DO  1C     *IN99         DOWEQ     *OFF
159300070723     C                   move      wfdIOE        depIOE
159400100127     C                   z-add     wfdCLI        UNIcli
159500070723     C                   z-add     0             Cliente_Anno      1 0
159600070723      *
159700070723     C* a Rottura Cliente
159800070723DO  2C     *IN99         DOWEQ     *OFF
159900070723     C     depIOE        ANDEQ     wfdIOE
160000100127     C     UNIcli        ANDEQ     wfdCLI
160100070723     C                   z-add     wfdANN        depANN
160200070723     C                   Clear                   Ypsilon_anno      1
160300070723     C                   add       1             Cliente_Anno
160400070723     C*
160500070723     C* a Rottura Cliente Anno
160600070723DO  3C     *IN99         DOWEQ     *OFF
160700070723     C     depIOE        ANDEQ     wfdIOE
160800100127     C     UNIcli        ANDEQ     wfdCLI
160900070723     C     depANN        ANDEQ     wfdANN
161000070723     C                   z-add     wfdMES        depMES
161100070723     C*
161200070723     C* a Rottura Cliente Anno/Mese
161300070723DO  4C     *IN99         DOWEQ     *OFF
161400070723     C     depIOE        ANDEQ     wfdIOE
161500100127     C     UNIcli        ANDEQ     wfdCLI
161600070723     C     depANN        ANDEQ     wfdANN
161700070723     C     depMES        ANDEQ     wfdMES
161800070723      *
161900070723      *  Memorizza il primo Ypsilon trovato x cliente Anno
162000070723     C                   if        wfdCLN <> ' ' and Ypsilon_anno = *blank
162100070723     C                   eval      Ypsilon_anno = wfdCLN
162200070723     c                   end
162300070723      *
162400070723      * Tutto il mese cliente deve essere a "Y"
162500070723     C                   if        Ypsilon_anno = 'Y'
162600070723     c                   eval      wfdCLN = 'Y'
162700070723     c                   end
162800070723      *
162900070723      * Dal mese seguente deve impostare sempre la "S"
163000070723      *   anche per l'anno seguente...
163100070723     C                   if        Ypsilon_anno = 'S'
163200070723     c                   eval      wfdCLN = 'S'
163300070723     c                   end
163400070723      *
163500070723     c                   if        PARWfdpv = 'S'
163600100203     C                   UPDATE    WFdfp5
163700070723     c                   else
163800100127     C                   UPDATE    WFDEa5
163900070723     c                   end
164000070723     C*
164100070723     C* LETTURA SUCCESSIVA DATI CLIENTE
164200070723     c                   if        PARWfdpv = 'S'
164300100203     C                   READ      WFdfpV5L                               99
164400070723     c                   else
164500100127     C                   READ      WFDEa05L                               99
164600070723     c                   end
164700070723E   4C                   ENDDO                                                  *ROTTURA Mese
164800070723     C*
164900070723     C*  Se il mese letto aveva la "Y" i mesi seguenti entro l'anno devono avere la "S"
165000070723     C                   if        Ypsilon_anno = 'Y'
165100070723     C                   eval      Ypsilon_anno = 'S'
165200070723     c                   end
165300070723      *
165400070723E   3C                   ENDDO                                                  *ROTTURA CLIENTE/Anno
165500070723     C*
165600070723E   2C                   ENDDO
165700070723     C*
165800070723E   1C                   ENDDO
165900070727     C*
166000070727      *? ------------------------------------------------------------------------------------------------- ?
166100070727      *? ......Dopo avere reso omogeneo nell'ambito dell'anno la sequenza di "Y" e "S", si deve considerare    ?
166200070727      *? sull'anno in CORSO che, se un cliente aveva già del fatturato nell'anno precedente,                           ?
166300070727      *? NON può considerarsi Nuovo.!!!!!!                         ?
166400070727      *? ..... quindi, sul Cliente, nell'Anno in CORSO, si devono eliminare le "Y" e "S" su tutti i records.
166500070727      *? ------------------------------------------------------------------------------------------------- ?
166600070727      *  (esempio esplicativo)
166700070727      *                              ? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
166800070727      *? Unific.(C) Anno Precedente ? |   |   |   | Y | S | S | S | S | S | S | S | S |
166900070727      *? Unific.(C) Anno  in  CORSO ? |   | Y | S | S | S | S | S | S |   |   |   |   |
167000070727      *? -----------------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
167100070727      *? ============================     ?
167200070727      * --> Invece deve essere così
167300070727      *? ============================     ?
167400070727      *? Unific.(C) Anno Precedente   |   |   |   | Y | S | S | S | S | S | S | S | S |   ?-- records anno precedente
167500070727      *? Unific.(C) Anno  in  CORSO   |   |   |   |   |   |   |   |   |   |   |   |   |   ?-- records anno in Corso
167600070727      *? -------------------------------------------------------------------------------- ?
167700070727     C*   Con un istruzione SQL possiamo quindi sistemare la situazione sull'anno in corso
167800070727     C*   semplicemente controllando se nell'anno precedente il Cliente aveva del Fatturato
167900070727     C*   quindi, aggiorniamo tutti i Suoi records dell'Anno in corso x dire che NON è NUOVO.
168000070727      *
168100070727     C/EXEC SQL
168200100127     C+ UPDATE wfdea00f SET wfdcln = ' '
168300070727     C+ WHERE wfdann = :ParAAI AND wfdcln <>' ' AND wfdcli
168400100127     C+ IN (SELECT wfdcli FROM wfdea00f WHERE wfdann = :ParAFP)
168500070727     C/END-EXEC
168600070723     C*
168700070723     C                   ENDSR
168800000000     O*------------------------------------------------------------------------*
168900000000     O* STAMPA DEI CODICI AGENTI INESISTENTI O ANNULLATI DA INSERIRE IN SEDE   *
169000000000     O*------------------------------------------------------------------------*
169100000000     OQSYSPRT   E            TESTA            02
169200000000     O                       STA(1)              66
169300000000     O                       STA(2)             132
169400980306     O                       UDATE              119 '  /  /  '
169500000000     O                       PAGE          Z    132
169600000000     O*------------------------
169700000000     O          E            TESTA            03
169800000000     O                       STA(3)              66
169900000000     O                       STA(3)             132
170000000000     O*------------------------
170100000000     O          E            TESTA            04
170200000000     O                       STA(4)              66
170300000000     O*------------------------
170400000000     O          E            TESTA            05
170500000000     O                       STA(3)              66
170600000000     O                       STA(3)             132
170700000000     O*------------------------
170800000000     O          E            ERROR       1
170900000000     O                       DEPAGE               9
171000000000     O                       DEPKSC              27
171100100127     O                       UNIcli              47
171200000000     O*------------------------------------------------------------------------*
171300000000** STA
171400100127                           TISE92AR     ***  AGENTI INESISTENTI DA1
171500980306 MODIFICARE IN ANAGRAFICA CLIENTE  ***       99/99/99     PAG.99992
171600000000------------------------------------------------------------------3
171700000824A G E N T E      C L I E N T E     U N I F I C A N T E            4
171800000824  9999999           9999999             9999999                   5
