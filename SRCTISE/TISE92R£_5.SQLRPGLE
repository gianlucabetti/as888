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
002900110401     FWAdfpv1L  UF A E           K DISK    usropn
003000110401     FWAdfpv4L  UF   E           K DISK    usropn
003100100203     F                                     RENAME(WFDFPV00:WFDFPV)
003200110401     FWAdfpv5L  UF   E           K DISK    usropn
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
041500130131     C*      al 31/1/2013 viene chiesto di calcolare il FUEL per scorporarlo dai ricavi
041600130131     C*
041700130131     C*   PRO-MEMORIA: le varie non hanno alcuna possibilità di essere distinte
041800130131     C*                x Italia/Estero o x Tipo servizio o x Network....Quindi possono
041900130131     C*                essere calcolate solo a livello di TOTALE ossia ITALIA+ESTERO ed
042000130131     C*                esclusivamente per CLIENTE o CLIENTE UNIFICANTE
042100130131     C*                infatti anche già adesso avviene così per l'espresso/H10:30.
042200100128     c                   if        SDpSVR = 'e' or SDpSVR = 'h'
042300130131     c                                or SDpSVR = 'f'                            <--- VARIE (f=FUEL)
042400100128     C*
042500100127     C* CONTROLLA ESISTENZA RECORD DI LAVORO
042600100127     C                   Z-ADD     UNIcli        wfvksc                         *CLIENTE
042700100127     C                   Z-ADD     SDpANN        wfvANN                         *ANNO
042800100127     C                   Z-ADD     SDpMES        wfvMES                         *MESE
042900100127     C                   moveL     SDpSVR        wfvSVR                         *VARIA
043000100127     C     KEYDEAV       CHAIN     WFDEAV1L
043100100127     C***
043200100127     C* I M M I S S I O N E
043300100127     C***
043400100127IF  1C                   IF        not %Found(WFDEAV1L)
043500100127      * --
043600100127     c                   CLEAR                   WFDEAV00
043700100127     C                   Z-ADD     UNIcli        WFvksc                         *CLIENTE
043800100127     C                   Z-ADD     SDpANN        WFvANN                         *ANNO
043900100127     C                   Z-ADD     SDpMES        WFvMES                         *MESE
044000100127     C                   MOVEL     SDpSVR        WFvSVR                         *VARIA
044100100127     C                   Z-ADD     SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
044200100127     C                   z-add     SDpRCP        WFvRCP                         *Ricavi Particolari
044300100127      * --
044400100127     C                   WRITE     WFDEAV00
044500100127      * --
044600100127X   1C                   ELSEif    %Found(WFDEAV1L)
044700100127     C***
044800100127     C* V A R I A Z I O N E
044900100127     C***
045000100127     C                   add       SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
045100100127     C                   add       SDpRCP        WFvRCP                         *Ricavi Particolari
045200100127      * ---
045300100127     C                   UPDATE    WFDEAV00
045400100127E   1C                   ENDIF
045500100127     C*
045600100128     c                   end
045700100128     C*
045800100127     C     KE2SDO        reade     SISDp01L
045900100127E   1C                   ENDdo
046000100127     C*
046100100127     C                   ENDSR
046200980119     C*------------------------------------------------------------------------*
046300980119     C* CHKSDO - CONTROLLA VALIDITA' RECORD
046400980119     C*------------------------------------------------------------------------*
046500980119     C     CHKSDO        BEGSR
046600980119     C*
046700980119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
046800980119     C*
046900980119     C                   ENDSR
047000000000     C*------------------------------------------------------------------------*
047100000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
047200000000     C*------------------------------------------------------------------------*
047300000000     C     SETFIL        BEGSR
047400000000     C*
047500000000     C* REIMPOSTO FLAG DI FINE LETTURA SALDI
047600000000     C                   MOVEL     '0'           $FINE
047700000000     C*
047800000000     C                   Z-ADD     DEPKSC        KSCKSC                         *CLIENTE
047900000000     C                   Z-ADD     PARAPI        KSCANN                         *ANNO PRECEDENTE INI
048000000000     C                   Z-ADD     PARMPI        KSCMES                         *MESE PRECEDENTE INI
048100000000     C*
048200000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO SALDO CLIENTE VALIDO
048300990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
048400000000     C*
048500000000IF  1C     *IN99         IFEQ      '1'
048600000000     C                   MOVEL     '1'           $FINE
048700000000X   1C                   ELSE
048800000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
048900000000E   1C                   ENDIF
049000000000     C*
049100000000     C                   ENDSR
049200000000     C*------------------------------------------------------------------------*
049300000000     C* LETFIL - LETTURA PROSSIMO RECORD SALDI CLIENTE
049400000000     C*------------------------------------------------------------------------*
049500000000     C     LETFIL        BEGSR
049600000000     C*
049700000000     C* LEGGO FINO A:
049800000000     C*     - FINE FILE
049900000000     C*     - TROVATO RECORD VALIDO
050000980601     C                   MOVEL     'N'           WRECOK
050100000000DO  1C     $FINE         DOWEQ     '0'
050200000000     C     WRECOK        ANDEQ     'N'
050300000000     C*
050400990609     C     KSCKSC        READE     SISDC01L                               99
050500000000     C*
050600000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
050700000000     C                   MOVEL     '1'           $FINE
050800000000X   2C                   ELSE
050900000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
051000000000E   2C                   ENDIF
051100000000     C*
051200000000E   1C                   ENDDO
051300000000     C*
051400000000     C                   ENDSR
051500000000     C*------------------------------------------------------------------------*
051600000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
051700000000     C*------------------------------------------------------------------------*
051800000000     C     CHKREC        BEGSR
051900000000     C*
052000980601     C                   MOVEL     'S'           WRECOK
052100020516     C*
052200020516     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
052300020516     C                   Z-ADD     1             G
052400020516     C                   SETOFF                                           28
052500020516     C     SDCRBL        LOOKUP    FLAB(G)                                28
052600020516IF  1C     *IN28         IFEQ      *OFF                                         *TIPO NON TROVATO --
052700020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
052800020516E   1C                   ENDIF
052900020516     C*
053000020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
053100020516IF  1C     WRECOK        IFEQ      'S'
053200020516IF  2C     SDCNSP        IFEQ      *ZEROS
053300020516     C     SDCPKG        ANDEQ     *ZEROS
053400020516     C     SDCPCC        ANDEQ     *ZEROS
053500020516     C     SDCPLS        ANDEQ     *ZEROS
053600020516     C     SDCNCO        ANDEQ     *ZEROS
053700020516     C     SDCVOL        ANDEQ     *ZEROS
053800020516     C     SDCVCC        ANDEQ     *ZEROS
053900020516     C     SDCVLS        ANDEQ     *ZEROS
054000020516     C     SDCIRB        ANDEQ     *ZEROS
054100020516     C     SDCIRR        ANDEQ     *ZEROS
054200020516     C     SDCINC        ANDEQ     *ZEROS
054300020516     C     SDCIRP        ANDEQ     *ZEROS
054400020516     C     SDCIRC        ANDEQ     *ZEROS
054500020516     C     SDCIRD        ANDEQ     *ZEROS
054600020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
054700020516E   2C                   ENDIF
054800020516E   1C                   ENDIF
054900000000     C*--------------------
055000000000     C* ESCLUSIONI
055100000000     C*--------------------
055200970821     C                   Z-ADD     1             P
055300970821     C                   SETOFF                                           28
055400970821     C     SDCFPE        LOOKUP    FPE(P)                                 28
055500970821IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
055600970821     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055700970821E   1C                   ENDIF
055800000000     C*--------------------
055900000000     C* SELEZIONI
056000000000     C*--------------------
056100000000     C* SE I SALDI SONO DI DATA SUPERIORE A QUELLA RICHIESTA, ESCO DAL CICLO
056200000000IF  1C     SDCDAT        IFGT      PARDA2
056300000000     C                   MOVEL     'N'           WRECOK
056400000000     C                   MOVEL     '1'           $FINE
056500000000E   1C                   ENDIF
056600000000     C*
056700000000     C* SE TIPO SERVIZIO <> TIPO SERVIZIO RICHIESTO, ESCLUDO
056800100127IF  1C**** PARTSP        IFNE      *BLANKS                                      *NON TUTTI
056900100127     C**** SDCTSP        ANDNE     PARTSP
057000100127     C*********          MOVEL     'N'           WRECOK
057100100127E   1C*********          ENDIF
057200000000     C*
057300000000     C                   ENDSR
057400000000     C*------------------------------------------------------------------------*
057500000824     C* MEMSAL - MEMORIZZA I DATI
057600000000     C*------------------------------------------------------------------------*
057700000824     C     MEMSAL        BEGSR
057800070531     C*
057900070531     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
058000070531     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
058100070531     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
058200070531     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
058300070531     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
058400000000     C*
058500000000     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
058600000000     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
058700000000IF  1C     FSP(G)        IFNE      'S'
058800000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
058900070531     C                   z-add     0             sdcPKG                         *PESO
059000070531     C                   z-add     0             sdcPCC                         *PESO CON CML
059100070531     C                   z-add     0             sdcPLS                         *PESO CML
059200070531     C                   z-add     0             sdcVOL                         *VOLUME TOTALE
059300070531     C                   z-add     0             sdcVCC                         *VOLUME CON CML
059400070531     C                   z-add     0             sdcVLS                         *VOLUME CML
059500000000E   1C                   ENDIF
059600000000     C*
059700070531     C* imposta il Peso ed il Volume
059800070531     c                   z-add     sdcPKG        depPKG
059900070531     c                   z-add     sdcVOL        depVOL
060000070531     C*
060100000000     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
060200000000     C                   Z-ADD     *ZEROS        DEPRRN
060300000000     C     SDCIRD        ADD       SDCIRR        DEPRRN
060400000000     C     SDCINC        ADD       DEPRRN        DEPRRN
060500020314     C*
060600020314     C* DETERMINA IL NETWORK
060700020404     C                   MOVEL     *BLANKS       DEPNTWP           3
060800020404     C                   Z-ADD     *ZEROS        DEPORDP           3 0
060900020314     C                   Z-ADD     1             R
061000020314     C     SDCREP        LOOKUP    RGREG(R)                               55
061100020314     C                   IF        *IN55 = *ON
061200020404     C                   EVAL      DEPNTWP = RGNTW(R)
061300020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
061400020404     C                   Z-ADD     1             X
061500020404     C     DEPNTWP       LOOKUP    NTWCOD(X)                              55
061600020404     C                   IF        *IN55 = *ON
061700020404     C                   EVAL      DEPORDP = NTWORD(X)
061800020404     C                   ENDIF
061900020314     C                   ENDIF
062000020314     C*
062100020404     C                   MOVEL     *BLANKS       DEPNTWA           3
062200020404     C                   Z-ADD     *ZEROS        DEPORDA           3 0
062300020314     C                   Z-ADD     1             R
062400020314     C     SDCREA        LOOKUP    RGREG(R)                               55
062500020314     C                   IF        *IN55 = *ON
062600020404     C                   EVAL      DEPNTWA = RGNTW(R)
062700020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
062800020404     C                   Z-ADD     1             X
062900020404     C     DEPNTWA       LOOKUP    NTWCOD(X)                              55
063000020404     C                   IF        *IN55 = *ON
063100020404     C                   EVAL      DEPORDA = NTWORD(X)
063200020404     C                   ENDIF
063300020314     C                   ENDIF
063400020404     C*
063500020404     C* IN BASE ALL'ORDINE DI CONSIDERAZIONE ATTRIBUISCO IL NETWORK DELLA SPEDIZIONE (ATTUALMENTE SOLO X ESTERO)
063600020404     C*
063700020404     C* 1) Italia su Italia
063800020404     C                   IF        DEPNTWP = *BLANKS AND
063900020404     C                             DEPNTWA = *BLANKS
064000020404     C                   EVAL      DEPNTW = *BLANKS
064100020404     C                   ELSE
064200020404     C* 2) Estero su Estero
064300020404     C                   IF        DEPNTWP <> *BLANKS AND
064400020404     C                             DEPNTWA <> *BLANKS
064500020404     C                   IF        DEPORDP < DEPORDA
064600020404     C                   EVAL      DEPNTW = DEPNTWP
064700020404     C                   ELSE
064800020404     C                   EVAL      DEPNTW = DEPNTWA
064900020404     C                   ENDIF
065000020404     C                   ELSE
065100020404     C* 3) Estero su Italia / Italia su Estero
065200020404     C                   IF        DEPNTWP <> *BLANKS
065300020404     C                   EVAL      DEPNTW = DEPNTWP
065400020404     C                   ELSE
065500020404     C                   EVAL      DEPNTW = DEPNTWA
065600020404     C                   ENDIF
065700020404     C                   ENDIF
065800020404     C                   ENDIF
065900020315     C*
066000020315     C* IMPOSTA SE FLAG ITALIA O ESTERO
066100020404     C     DEPNTWP       IFNE      *BLANKS
066200020404     C     DEPNTWA       ORNE      *BLANKS
066300020315     C                   MOVEL     'E'           DEPIOE                         *ESTERO
066400020315X   1C                   ELSE
066500020315     C                   MOVEL     'I'           DEPIOE                         *ITALIA
066600020315E   1C                   ENDIF
066700000000     C*
066800000000     C* CONTROLLA ESISTENZA RECORD DI LAVORO
066900000000     C                   MOVEL     DEPDIV        KWDDIV                         *DIVISIONE
067000000000     C                   Z-ADD     DEPARE        KWDARE                         *AREA
067100000000     C                   Z-ADD     DEPFIL        KWDFIL                         *FILIALE
067200000000     C                   Z-ADD     DEPAGE        KWDAGE                         *COMMERCIALE
067300000000     C                   MOVEL     DEPCLV        KWDCLV                         *CODICE QUALITA'
067400100127     C                   Z-ADD     UNIcli        KWDCLI                         *CLIENTE
067500000824     C                   Z-ADD     SDCANN        KWDANN                         *ANNO
067600000824     C                   Z-ADD     SDCMES        KWDMES                         *MESE
067700000824     C                   MOVEL     SDCFPE        KWDFPE                         *FASCIA DI PESO
067800970821     C                   MOVEL     DEPIOE        KWDIOE                         *ITALIA O ESTERO
067900020315     C                   MOVEL     DEPNTW        KWDNTW                         *NETWORK
068000100127     C                   MOVEL     SDCTSP        KWDTSP                         *Tipo Servizio
068100070531     c                   if        PARWfdpv = 'S'
068200110401     C     KEYDEL        CHAIN     WAdfpv1L                           99
068300070531     c                   else
068400100127     C     KEYDELtsp     CHAIN     WFDEA01L                           99
068500070531     c                   end
068600000000     C***
068700000000     C* I M M I S S I O N E
068800000000     C***
068900000000IF  1C     *IN99         IFEQ      '1'
069000100127     c                   CLEAR                   WFDEA000
069100000000     C                   MOVEL     DEPDIV        WFDDIV                         *DIVISIONE
069200000000     C                   Z-ADD     DEPARE        WFDARE                         *AREA
069300000000     C                   Z-ADD     DEPFIL        WFDFIL                         *FILIALE
069400000000     C                   Z-ADD     DEPAGE        WFDAGE                         *COMMERCIALE
069500000000     C                   MOVEL     DEPCLV        WFDCLV                         *CODICE QUALITA'
069600100127     C                   Z-ADD     UNIcli        WFDCLI                         *CLIENTE
069700000824     C                   Z-ADD     SDCANN        WFDANN                         *ANNO
069800000824     C                   Z-ADD     SDCMES        WFDMES                         *MESE
069900000824     C                   MOVEL     SDCFPE        WFDFPE                         *FASCIA DI PESO
070000000824     C                   MOVEL     DEPIOE        WFDIOE                         *ITALIA O ESTERO
070100000000     C                   Z-ADD     DEPRGF        WFDRGF                         *RAGGRUPP FIL AGENTE
070200000000     C                   MOVEL     DEPRGD        WFDRGD                         *DESCRIZ RAGGRUP.TO
070300000000     C                   MOVEL     DEPDIC        WFDDIC                         DIVISIONE COMMERCIAL
070400000000     C                   Z-ADD     SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
070500070531     C                   z-add     depPKG        WFDpkg                         *Peso
070600070531     C                   z-add     depVOL        WFDvol                         *Volume
070700000000     C                   Z-ADD     DEPRRN        WFDRRN                         RIC REALE NETTO DELT
070800000000     C                   Z-ADD     SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
070900070720      * --
071000000000     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
071100070720      * --
071200980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
071300020315     C                   MOVEL     DEPNTW        WFDNTW                         *NETWORK
071400100127      * --
071500100127     C                   move      SDCTSP        WFDTSP                         Tipo Servizio
071600100127      * --
071700070531     c                   if        PARWfdpv = 'S'
071800110401     C                   WRITE     Wfdfpv00
071900070531     c                   else
072000100127     C                   WRITE     WFDEA000
072100070531     c                   end
072200000000X   1C                   ELSE
072300000000     C***
072400000000     C* V A R I A Z I O N E
072500000000     C***
072600000000     C     WFDNSP        ADD       SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
072700070531     C     WFDpkg        add       depPKG        WFDpkg                         *Peso
072800070531     C     WFDvol        add       depVOL        WFDvol                         *Volume
072900000000     C     WFDRRN        ADD       DEPRRN        WFDRRN                         RIC REALE NETTO DELT
073000000000     C     WFDRPN        ADD       SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
073100980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
073200070720      * ---
073300070720      * solo x unificante
073400070720      *?  Attenzione: se lanciata x Unificante, occorre determinare se il codice  ?
073500070720      *?    unificante è nuovo o vecchio considerando tutti i suoi figli e,  ?
073600070821      *?    qualora uno di essi sia considerato vecchio, sarà quest'ultimo
073700070821      *?     a dare la caratteristica di vecchio al codice Unificante Padre.
073800070720      *?  >> SU SISDO l'info di Nuovo/Vecchio è sempre per singolo Cliente. <<
073900070723     c                   if        parSCE = 'U'
074000070723     c                   if        wfdCLN = *blank or
074100070723     c                             wfdCLN = 'Y' and DEPCLN = 'S'
074200070720     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
074300070720     c                   end
074400070723     c                   end
074500070720      * ---
074600070531     c                   if        PARWfdpv = 'S'
074700110401     C                   UPDATE    Wfdfpv00
074800070531     c                   else
074900100127     C                   UPDATE    WFDEA000
075000070531     c                   end
075100000000E   1C                   ENDIF
075200000000     C*
075300000000     C                   ENDSR
075400000824     C*------------------------------------------------------------------------*
075500000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
075600000824     C*------------------------------------------------------------------------*
075700000825     C     AGGWFD        BEGSR
075800000825     C*
075900000825     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
076000070531     c                   if        PARWfdpv = 'S'
076100110401     C     *LOVAL        SETLL     WAdfpv4L
076200110401     C                   READ(N)   WAdfpv4L                               99
076300070531     c                   else
076400100127     C     *LOVAL        SETLL     WFDEa04L
076500100127     C                   READ(N)   WFDEa04L                               99
076600070531     c                   end
076700000825     C*
076800000825     C* CICLO FINO A FINE FILE
076900000825DO  1C     *IN99         DOWEQ     *OFF
077000000825     C*
077100000825     C* OPERAZIONE PER NUOVO CLIENTE
077200000825     C                   EXSR      NEWCLI
077300000825     C*
077400000825     C* CICLO FINO A ROTTURA CLIENTE
077500000825DO  2C     *IN99         DOWEQ     *OFF
077600100127     C     UNIcli        ANDEQ     WFDCLI
077700070723      *
077800070723      *
077900000825IF  3C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
078000000825     C     WFDDAT        ANDLE     PARDA2
078100100127     C* MEMORIZZA I DATI -WFDEA-
078200000825     C                   EXSR      MEMWFD
078300000825E   3C                   ENDIF
078400000825     C*
078500000825     C* LETTURA SUCCESSIVA DATI CLIENTE
078600070531     c                   if        PARWfdpv = 'S'
078700110401     C                   READ(N)   WAdfpv4L                               99
078800070531     c                   else
078900100127     C                   READ(N)   WFDEa04L                               99
079000070531     c                   end
079100070723     C*
079200000825E   2C                   ENDDO                                                  *ROTTURA CLIENTE
079300000825     C*
079400000825     C* CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
079500000825     C                   EXSR      MODCLI
079600000825     C*
079700000825     C* SI RIPOSIZIONA E LEGGE I DATI DEL CLIENT SUCCESSIVO
079800070531     c                   if        PARWfdpv = 'S'
079900110401     C     UNIcli        SETGT     WAdfpv4L
080000110401     C                   READ(N)   WAdfpv4L                               99
080100070531     c                   else
080200100127     C     UNIcli        SETGT     WFDEa04L
080300100127     C                   READ(N)   WFDEa04L                               99
080400070531     c                   end
080500000825E   1C                   ENDDO
080600000824     C*
080700000824     C                   ENDSR
080800000825     C*------------------------------------------------------------------------*
080900100127     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE -WFDEA-
081000000825     C*------------------------------------------------------------------------*
081100000825     C     NEWCLI        BEGSR
081200000825     C*
081300000825     C* MEMORIZZA IL NUOVO CLIENTE IN UN DEPOSITO
081400100127     C                   Z-ADD     WFDCLI        UNIcli
081500000825     C*
081600000825     C* AZZERA LE VARIABILI DI LAVORO
081700020116     C                   Z-ADD     *ZEROS        TOTIRD           19 5          *RICAVI
081800020116     C                   Z-ADD     *ZEROS        TOTIRP           19 5
081900020116     C                   Z-ADD     *ZEROS        TOTERD           19 5
082000020116     C                   Z-ADD     *ZEROS        TOTERP           19 5
082100020116     C                   Z-ADD     *ZEROS        TOTTRD           19 5
082200020116     C                   Z-ADD     *ZEROS        TOTTRP           19 5
082300000825     C                   Z-ADD     *ZEROS        PERDEI            4 1          *% DELTA
082400000825     C                   Z-ADD     *ZEROS        PERDEE            4 1
082500000825     C                   Z-ADD     *ZEROS        PERDET            4 1
082600000825     C                   MOVEL     'S'           $OKDEI            1            *DELTA OK
082700000825     C                   MOVEL     'S'           $OKDEE            1
082800000825     C                   MOVEL     'S'           $OKDET            1
082900000825     C                   CLEAR                   SMEI                           *MM CON % SEGNALARE
083000000825     C                   CLEAR                   SMEE
083100000825     C                   CLEAR                   SMET
083200000825     C                   Z-ADD     *ZEROS        MI                2 0          *INDICE MESI
083300000825     C                   Z-ADD     *ZEROS        ME                2 0
083400000825     C                   Z-ADD     *ZEROS        MT                2 0
083500000825     C*
083600000825     C                   ENDSR
083700000825     C*------------------------------------------------------------------------*
083800000825     C* MEMWFD - MEMORIZZA I DATI DEL WFILE
083900000825     C*------------------------------------------------------------------------*
084000000825     C     MEMWFD        BEGSR
084100000825     C
084200000825     C* MEMORIZZA I RICAVI ITALIA E ESTERO DEL CLIENTE PER IL PERIODO RICHIESTO
084300000825IF  1C     WFDIOE        IFEQ      'I'                                          *ITALIA
084400000825     C                   ADD       WFDRRN        TOTIRD                         *TOT RIC REALE
084500000825     C                   ADD       WFDRPN        TOTIRP                         *TOT RIC PRESUNTO
084600000825E   1C                   ENDIF
084700000825IF  1C     WFDIOE        IFEQ      'E'                                          *ESTERO
084800000825     C                   ADD       WFDRRN        TOTERD                         *TOT RIC REALE
084900000825     C                   ADD       WFDRPN        TOTERP                         *TOT RIC PRESUNTO
085000000825E   1C                   ENDIF
085100000825IF  1C     WFDIOE        IFEQ      *BLANKS                                      *ITALIA + ESTERO
085200000825     C                   ADD       WFDRRN        TOTTRD                         *TOT RIC REALE
085300000825     C                   ADD       WFDRPN        TOTTRP                         *TOT RIC PRESUNTO
085400000825E   1C                   ENDIF
085500000825     C*
085600000825     C* CONTROLLA SE LA % DELTA DI CIASCUNA FASCIA DI PESO RIENTRA IN QUELLA RICHIESTA
085700000825IF  1C     PARSPD        IFNE      *ZEROS                                       *RICHIESTA
085800000825     C*
085900000825     C* ITALIA
086000000825IF  2C     WFDIOE        IFEQ      'I'
086100000825IF  3C     WFDRRN        IFGT      *ZEROS
086200000825     C     WFDRPN        ORGT      *ZEROS
086300020116     C                   Z-ADD     WFDRRN        WRRW             17 5          *RICAVO REALE
086400020116     C                   Z-ADD     WFDRPN        WRPW             17 5          *RICAVO PRESUNTO
086500000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
086600000825     C                   EXSR      CALDEL
086700000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
086800000825     C                   SETOFF                                           97
086900000825     C     WFDMES        LOOKUP    SMEI                                   97
087000000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
087100000825     C                   ADD       1             MI
087200000825     C                   Z-ADD     WFDMES        SMEI(MI)                       *MESE DA SEGNALARE
087300000825E   5C                   ENDIF
087400000825E   4C                   ENDIF
087500000825E   3C                   ENDIF
087600000825E   2C                   ENDIF
087700000825     C* ESTERO
087800000825IF  2C     WFDIOE        IFEQ      'E'
087900000825IF  3C     WFDRRN        IFGT      *ZEROS
088000000825     C     WFDRPN        ORGT      *ZEROS
088100020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
088200020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
088300000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
088400000825     C                   EXSR      CALDEL
088500000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
088600000825     C                   SETOFF                                           97
088700000825     C     WFDMES        LOOKUP    SMEE                                   97
088800000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
088900000825     C                   ADD       1             ME
089000000825     C                   Z-ADD     WFDMES        SMEE(ME)                       *MESE DA SEGNALARE
089100000825E   5C                   ENDIF
089200000825E   4C                   ENDIF
089300000825E   3C                   ENDIF
089400000825E   2C                   ENDIF
089500000825     C* ITALIA + ESTERO
089600000825IF  2C     WFDIOE        IFEQ      *BLANKS
089700000825IF  3C     WFDRRN        IFGT      *ZEROS
089800000825     C     WFDRPN        ORGT      *ZEROS
089900020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
090000020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
090100000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
090200000825     C                   EXSR      CALDEL
090300000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
090400000825     C                   SETOFF                                           97
090500000825     C     WFDMES        LOOKUP    SMET                                   97
090600000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
090700000825     C                   ADD       1             MT
090800000825     C                   Z-ADD     WFDMES        SMET(MT)                       *MESE DA SEGNALARE
090900000825E   5C                   ENDIF
091000000825E   4C                   ENDIF
091100000825E   3C                   ENDIF
091200000825E   2C                   ENDIF
091300000825     C*
091400000825E   1C                   ENDIF
091500000825     C*
091600000825     C                   ENDSR
091700000825     C*------------------------------------------------------------------------*
091800000825     C* MODCLI - CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
091900000825     C*------------------------------------------------------------------------*
092000000825     C     MODCLI        BEGSR
092100000825     C*
092200000825     C* CALCOLA IL DELTA DEL CLIENTE
092300000825     C                   EXSR      CALCLI
092400000825     C*
092500000825     C* RILEGGE I DATI DEL CLIENTE -->
092600000825     C* - CANCELLA IL CLIENTE SE NON RIENTRA NEL RANGE DELTA RICHIESTO
092700000825     C* - AGGIORNA IL CLIENTE SE HA UNA % DELTA FASCE DA SEGNALARE
092800070531     c                   if        PARWfdpv = 'S'
092900110401     C     UNIcli        SETLL     WAdfpv4L
093000110401     C     UNIcli        READE     WAdfpv4L                               98
093100070531     c                   else
093200100127     C     UNIcli        SETLL     WFDEa04L
093300100127     C     UNIcli        READE     WFDEa04L                               98
093400070531     c                   end
093500070531      *
093600000825DO  1C     *IN98         DOWEQ     *OFF
093700000825     C*
093800000825     C* ITALIA
093900000825IF  2C     WFDIOE        IFEQ      'I'
094000000825IF  3C     $OKDEI        IFEQ      'N'                                          *DELTA NON OK
094100070531     c                   if        PARWfdpv = 'S'
094200100203     C                   DELETE    WFDfpv
094300070531     c                   else
094400100127     C                   DELETE    WFDEa4
094500070531     c                   end
094600000825X   3C                   ELSE                                                   *DELTA OK
094700000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
094800000828     C     WFDDAT        ANDLE     PARDA2
094900000825     C                   SETOFF                                           97
095000000825     C     WFDMES        LOOKUP    SMEI                                   97
095100000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
095200000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
095300070531     c                   if        PARWfdpv = 'S'
095400100203     C                   UPDATE    WFDfpv
095500070531     c                   else
095600100127     C                   UPDATE    WFDEa4
095700070531     c                   end
095800000828E   5C                   ENDIF
095900000828E   4C                   ENDIF
096000000825E   3C                   ENDIF
096100000825E   2C                   ENDIF
096200000825     C*
096300000825     C* ESTERO
096400000825IF  2C     WFDIOE        IFEQ      'E'
096500000825IF  3C     $OKDEE        IFEQ      'N'                                          *DELTA NON OK
096600070531     c                   if        PARWfdpv = 'S'
096700100203     C                   DELETE    WFDfpv
096800070531     c                   else
096900100127     C                   DELETE    WFDEa4
097000070531     c                   end
097100000825X   3C                   ELSE                                                   *DELTA OK
097200000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
097300000828     C     WFDDAT        ANDLE     PARDA2
097400000825     C                   SETOFF                                           97
097500000825     C     WFDMES        LOOKUP    SMEE                                   97
097600000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
097700000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
097800070531     c                   if        PARWfdpv = 'S'
097900100203     C                   UPDATE    WFDfpv
098000070531     c                   else
098100100127     C                   UPDATE    WFDEa4
098200070531     c                   end
098300000828E   5C                   ENDIF
098400000825E   4C                   ENDIF
098500000825E   3C                   ENDIF
098600000825E   2C                   ENDIF
098700000825     C*
098800000825     C* ITALIA + ESTERO
098900000825IF  2C     WFDIOE        IFEQ      *BLANKS
099000000825IF  3C     $OKDET        IFEQ      'N'                                          *DELTA NON OK
099100070531     c                   if        PARWfdpv = 'S'
099200100203     C                   DELETE    WFDfpv
099300070531     c                   else
099400100127     C                   DELETE    WFDEa4
099500070531     c                   end
099600000825X   3C                   ELSE                                                   *DELTA OK
099700000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
099800000828     C     WFDDAT        ANDLE     PARDA2
099900000825     C                   SETOFF                                           97
100000000825     C     WFDMES        LOOKUP    SMET                                   97
100100000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
100200000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
100300070531     c                   if        PARWfdpv = 'S'
100400100203     C                   UPDATE    WFDfpv
100500070531     c                   else
100600100127     C                   UPDATE    WFDEa4
100700070531     c                   end
100800000828E   5C                   ENDIF
100900000825E   4C                   ENDIF
101000000825E   3C                   ENDIF
101100000825E   2C                   ENDIF
101200000825     C*
101300070531     c                   if        PARWfdpv = 'S'
101400110401     C     UNIcli        READE     WAdfpv4L                               98
101500070531     c                   else
101600100127     C     UNIcli        READE     WFDEa04L                               98
101700070531     c                   end
101800000825E   1C                   ENDDO                                                  *DINE DATI CLIENTE
101900000825     C*
102000000825     C                   ENDSR
102100000825     C*------------------------------------------------------------------------*
102200000825     C* CALCOLA I DELTA SUI DATI MEMORIZZATI
102300000825     C*------------------------------------------------------------------------*
102400000825     C     CALCLI        BEGSR
102500000825     C*
102600000825     C* CALCOLA I DELTA E CONTROLLA SE RIENTRANO NEL RANGE RICHIESTO
102700000825IF  1C     PARPDD        IFNE      -999,9                                       *RICHIESTO
102800000825     C     PARPDA        ORNE      +999,9
102900000825     C*
103000000825     C* DELTA ITALIA
103100000825IF  2C     TOTIRD        IFGT      *ZEROS
103200000825     C     TOTIRP        ORGT      *ZEROS
103300020116     C                   Z-ADD     TOTIRD        WRRW                           *RICAVO REALE
103400020116     C                   Z-ADD     TOTIRP        WRPW                           *RICAVO PRESUNTO
103500000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
103600000825     C                   EXSR      CALDEL
103700000825     C                   Z-ADD     WDPW          PERDEI                         *% DELTA ITALIA
103800000825IF  3C     PERDEI        IFLT      PARPDD                                       *DELTA <> RANGE
103900000825     C     PERDEI        ORGT      PARPDA
104000000825     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
104100000825E   3C                   ENDIF
104200010313X   2C                   ELSE                                                   *NO RICAVI ITALIA
104300010313     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
104400000825E   2C                   ENDIF
104500000825     C* DELTA ESTERO
104600000825IF  2C     TOTERD        IFGT      *ZEROS
104700000825     C     TOTERP        ORGT      *ZEROS
104800020116     C                   Z-ADD     TOTERD        WRRW                           *RICAVO REALE
104900020116     C                   Z-ADD     TOTERP        WRPW                           *RICAVO PRESUNTO
105000000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
105100000825     C                   EXSR      CALDEL
105200000825     C                   Z-ADD     WDPW          PERDEE                         *% DELTA ESTERO
105300000825IF  3C     PERDEE        IFLT      PARPDD                                       *DELTA <> RANGE
105400000825     C     PERDEE        ORGT      PARPDA
105500000825     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
105600000825E   3C                   ENDIF
105700010313X   2C                   ELSE                                                   *NO RICAVI ESTERO
105800010313     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
105900000825E   2C                   ENDIF
106000000825     C* DELTA ITA + EST
106100000825IF  2C     TOTTRD        IFGT      *ZEROS
106200000825     C     TOTTRP        ORGT      *ZEROS
106300020116     C                   Z-ADD     TOTTRD        WRRW                           *RICAVO REALE
106400020116     C                   Z-ADD     TOTTRP        WRPW                           *RICAVO PRESUNTO
106500000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
106600000825     C                   EXSR      CALDEL
106700000825     C                   Z-ADD     WDPW          PERDET                         *% DELTA ESTERO
106800000825IF  3C     PERDET        IFLT      PARPDD                                       *DELTA <> RANGE
106900000825     C     PERDET        ORGT      PARPDA
107000000825     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
107100000825E   3C                   ENDIF
107200010313X   2C                   ELSE                                                   *NO RICAVI TOTALI
107300010313     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
107400000825E   2C                   ENDIF
107500000825     C*
107600000825E   1C                   ENDIF
107700000825     C*
107800000825     C                   ENDSR
107900000824     C*------------------------------------------------------------------------*
108000000824     C* CALDEL - CALCOLA IL DELTA
108100000824     C*          INPUT  --> WRRW: RICAVO REALE
108200970821     C*                     WRPW: RICAVO PRESUNTO
108300970821     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
108400970821     C*------------------------------------------------------------------------*
108500970821     C     CALDEL        BEGSR
108600970821     C*--------------------
108700970821     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
108800970821     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
108900970821     C*--------------------
109000970821     C*
109100970821     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
109200020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
109300970821     C     GAP           MULT      100           GAP                            (GAP x 100)
109400970821     C*
109500970821     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
109600970821IF  1C     WRRW          IFEQ      *ZEROS
109700000825     C                   Z-ADD     1             WRRW
109800970821E   1C                   ENDIF
109900970821     C*
110000970821     C     GAP           DIV       WRRW          GAP
110100000825     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
110200970821     C*
110300970821     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
110400970821     C     WDPW          COMP      0                                    28
110500970821     C* POSITIVO
110600970821IF  1C     *IN28         IFEQ      *OFF
110700970821IF  2C     GAP           IFGE      1000
110800970821     C                   Z-ADD     +999,9        WDPW
110900970821E   2C                   ENDIF
111000970821     C* NEGATIVO
111100970821X   1C                   ELSE
111200970821IF  2C     GAP           IFLE      -1000
111300970821     C                   Z-ADD     -999,9        WDPW
111400970821E   2C                   ENDIF
111500970821E   1C                   ENDIF
111600970821     C*
111700970821     C                   ENDSR
111800000000     C*------------------------------------------------------------------------*
111900000000     C* STAERR - STAMPA RIGA DI ERRORE PER AGENTE INESISTENTE O ANNULLATO
112000000000     C*------------------------------------------------------------------------*
112100000000     C     STAERR        BEGSR
112200000000     C*
112300000000IF  1C     *INOF         IFEQ      *ON
112400000000     C                   EXCEPT    TESTA
112500000000     C                   SETOFF                                       OF
112600000000E   1C                   ENDIF
112700000000     C*
112800000000     C* AGENTE NON ANCORA STAMPATO, SCRIVO ERRORE
112900000000     C                   SETOFF                                       97
113000000000     C     DEPAGE        LOOKUP    ERG                                    97
113100000000IF  1C     *IN97         IFEQ      *OFF
113200000000     C                   ADD       1             I                 3 0
113300000000     C                   Z-ADD     DEPAGE        ERG(I)                         *MEMORIZZA AGENTE
113400000000     C                   EXCEPT    ERROR                                        *STAMPA L'ERRORE
113500000000E   1C                   ENDIF
113600000000     C*
113700000000     C                   ENDSR
113800000000     C*------------------------------------------------------------------------*
113900000000     C* CARTAB - CARICA I DATI IN TABELLE
114000000000     C*------------------------------------------------------------------------*
114100000000     C     CARTAB        BEGSR
114200000000     C*--------------------
114300000000     C* ORGANIGRAMMA
114400000000     C*--------------------
114500000000     C                   Z-ADD     *ZEROS        F
114600000000     C                   Z-ADD     *ZEROS        KORFIL
114700000000     C     KEYORG        SETLL     AZORG01L
114800000000     C                   READ      AZORG01L                               99
114900000000DO  1C     *IN99         DOWEQ     '0'
115000980119IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
115100980119IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
115200000000     C                   ADD       1             F
115300000000     C                   Z-ADD     ORGFIL        FIL(F)
115400000000     C                   MOVEL     ORGDES        DFIL(F)
115500020906     C                   Z-ADD     1             KTBKUT
115600000000     C                   MOVEL     '05'          KTBCOD
115700000000     C                   MOVEL     *BLANKS       KTBKEY
115800000000     C                   MOVEL     ORGCAR        KTBKEY
115900000824     C     KEYTAB        CHAIN     TABEL00F                           99
116000980119IF  5C     *IN99         IFEQ      *OFF
116100000000     C                   Z-ADD     ORGCAR        ARE(F)
116200000000     C                   MOVEL     TBLUNI        DARE(F)
116300980119E   5C                   ENDIF
116400020906     C                   Z-ADD     1             KTBKUT
116500000000     C                   MOVEL     '17'          KTBCOD
116600000000     C                   MOVEL     *BLANKS       KTBKEY
116700000000     C                   MOVEL     ORGFL3        KTBKEY
116800000824     C     KEYTAB        CHAIN     TABEL00F                           99
116900980119IF  5C     *IN99         IFEQ      *OFF
117000000000     C                   MOVEL     ORGFL3        DIV(F)
117100000000     C                   MOVEL     TBLUNI        DDIV(F)
117200980119E   5C                   ENDIF
117300980119E   3C                   ENDIF
117400980119E   2C                   ENDIF
117500000000     C                   READ      AZORG01L                               99
117600000000E   1C                   ENDDO
117700000000     C*--------------------
117800000000     C* COMMERCIALI
117900000000     C*--------------------
118000000000     C                   Z-ADD     *ZEROS        C
118100020906     C                   Z-ADD     1             KTBKUT
118200000000     C                   MOVEL     '01'          KTBCOD
118300981103     C     KEYTA2        SETLL     TABEL00F
118400000824     C     KEYTA2        READE     TABEL00F                               99
118500000000DO  1C     *IN99         DOWEQ     *OFF
118600970825IF  2C     TBLFLG        IFEQ      *BLANKS
118700000000     C                   ADD       1             C
118800000000     C                   MOVEL     TBLUNI        DS01
118900000000     C                   MOVEL     TBLKEY        AGE(C)
119000000000     C                   MOVEL     §01RGF        RGF(C)
119100000000     C                   MOVEL     §01AGE        DAGE(C)
119200970825E   2C                   ENDIF
119300000824     C     KEYTA2        READE     TABEL00F                               99
119400000000E   1C                   ENDDO
119500000000     C*-------------------------
119600000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
119700000000     C*-------------------------
119800000000     C                   Z-ADD     *ZEROS        G
119900020906     C                   Z-ADD     1             KTBKUT
120000000000     C                   MOVEL     '2W'          KTBCOD
120100000000     C     KEYTA2        SETLL     TABEL00F
120200000824     C     KEYTA2        READE     TABEL00F                               99
120300000000DO  1C     *IN99         DOWEQ     *OFF
120400000000IF  2C     TBLFLG        IFEQ      *BLANKS
120500000000     C                   ADD       1             G
120600000000     C                   MOVEL     TBLKEY        FLAB(G)
120700000000     C                   MOVEL     TBLUNI        DS2W
120800000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
120900000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
121000000000E   2C                   ENDIF
121100000824     C     KEYTA2        READE     TABEL00F                               99
121200000000E   1C                   ENDDO
121300990217     C*--------------------
121400000000     C* FASCE DI PESO
121500990217     C*--------------------
121600990609IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
121700990217     C                   MOVEL     'SDC'         WSX               3
121800990217X   1C                   ELSE                                                   *FASCE DAL '99
121900020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO AL 2001
122000020325     C                   MOVEL     'S9C'         WSX
122100020325X   2C                   ELSE                                                   *FASCE DAL 2002
122200020325     C                   MOVEL     'S2C'         WSX
122300020325E   2C                   ENDIF
122400020325E   1C                   ENDIF
122500970821     C                   Z-ADD     *ZEROS        P
122600020906     C                   Z-ADD     1             KTBKUT
122700000000     C                   MOVEL     '2L'          KTBCOD
122800000000     C     KEYTA2        SETLL     TABEL00F
122900000824     C     KEYTA2        READE     TABEL00F                               99
123000000000DO  1C     *IN99         DOWEQ     *OFF
123100000000IF  2C     TBLFLG        IFEQ      *BLANKS
123200990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
123300000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
123400990217IF  3C     FILEST        IFEQ      WSX
123500000000     C                   MOVEL     TBLUNI        DS2L
123600000309IF  4C     §2LKGI        IFGE      5000
123700000309     C     PARDAT        ANDLT     200001
123800000309     C                   ELSE
123900970821     C                   ADD       1             P
124000970821     C                   MOVE      STACOD        FPE(P)                         *...+XX
124100000309E   4C                   ENDIF
124200000000E   3C                   ENDIF
124300000000E   2C                   ENDIF
124400000824     C     KEYTA2        READE     TABEL00F                               99
124500000000E   1C                   ENDDO
124600020314     C*-------------------------
124700020314     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
124800020314     C*-------------------------
124900020314     C                   Z-ADD     *ZEROS        R
125000020906     C                   Z-ADD     1             KTBKUT
125100020314     C                   MOVEL     '2M'          KTBCOD
125200020314     C     KEYTA2        SETLL     TABEL00F
125300020314     C     KEYTA2        READE     TABEL00F                               99
125400020314DO  1C     *IN99         DOWEQ     *OFF
125500020314IF  2C     TBLFLG        IFEQ      *BLANKS
125600020314     C                   ADD       1             R
125700020314     C                   MOVEL     TBLKEY        RGREG(R)
125800020314     C                   MOVEL     TBLUNI        DS2M
125900020314     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
126000020314E   2C                   ENDIF
126100020314     C     KEYTA2        READE     TABEL00F                               99
126200020314E   1C                   ENDDO
126300020404     C*----------
126400020404     C* NETWORK AZIENDALI
126500020404     C*----------
126600020404     C                   Z-ADD     *ZEROS        X
126700020404     C                   EVAL      TBECOD = 'NTW'
126800020404     C     KEYTBE        SETLL     TNTBE01L
126900020404     C     KEYTBE        READE     TNTBE01L
127000020404     C                   DOW       NOT %EOF(TNTBE01L)
127100020404     C                   ADD       1             X
127200020404     C                   MOVEL     TBEUNI        DNTW
127300020404     C                   EVAL      NTWCOD(X) = TBEKE1
127400020404     C                   EVAL      NTWORD(X) = §NTWORD
127500020404     C     KEYTBE        READE     TNTBE01L
127600020404     C                   ENDDO
127700000000     C*
127800000000     C                   ENDSR
127900970821     C*------------------------------------------------------------------------*
128000970821     C* AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
128100970821     C*------------------------------------------------------------------------*
128200970821     C     AMELAB        BEGSR
128300970821     C*---
128400970821     C* ANNO INIZIALE UGUALE AD ANNO FINALE
128500970821     C*---
128600970821IF  1C     WAA2          IFEQ      WAA
128700970821     C*
128800970821     C* CALCOLO I MESI DI DIFFERENZA
128900970821DO  2C     WMM           DOWLE     WMM2
129000970821     C                   ADD       1             JI
129100970821     C                   Z-ADD     WAA           ANN(JI)
129200970821     C                   Z-ADD     WMM           MES(JI)
129300970821     C                   ADD       1             WMM
129400970821E   2C                   ENDDO
129500970821     C*
129600970821     C* IL RESTO DELLA SCHIERA A ZERO
129700970821DO  2C     JI            DOWLT     12
129800970821     C                   ADD       1             JI
129900970821     C                   Z-ADD     *ZEROS        ANN(JI)
130000970821     C                   Z-ADD     *ZEROS        MES(JI)
130100970821E   2C                   ENDDO
130200970821     C*---
130300970821     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
130400970821     C*---
130500970821X   1C                   ELSE
130600970821     C*
130700970821     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
130800970821DO  2C     WMM           DOWLE     12
130900970821     C                   ADD       1             JI
131000970821     C                   Z-ADD     WAA           ANN(JI)
131100970821     C                   Z-ADD     WMM           MES(JI)
131200970821     C                   ADD       1             WMM
131300970821E   2C                   ENDDO
131400970821     C*
131500970821     C* MEMORIZZO I MESI DELL'ANNO FINALE
131600970821     C                   Z-ADD     01            WMM
131700970821DO  2C     WMM           DOWLE     WMM2
131800970821     C                   ADD       1             JI
131900970821     C                   Z-ADD     WAA2          ANN(JI)
132000970821     C                   Z-ADD     WMM           MES(JI)
132100970821     C                   ADD       1             WMM
132200970821E   2C                   ENDDO
132300970821     C*
132400970821     C* IL RESTO DELLA SCHIERA A ZERO
132500970821DO  2C     JI            DOWLT     12
132600970821     C                   ADD       1             JI
132700970821     C                   Z-ADD     *ZEROS        ANN(JI)
132800970821     C                   Z-ADD     *ZEROS        MES(JI)
132900970821E   2C                   ENDDO
133000970821E   1C                   ENDIF
133100970821     C*
133200970821     C                   ENDSR
133300980403     C*------------------------------------------------------------------------*
133400980403     C* FINSR - OPERAZIONI FINALI
133500980403     C*------------------------------------------------------------------------*
133600980403     C     FINSR         BEGSR
133700980403     C*
133800980403     C* CHIUDE I FILE DEI PGM CHIAMATI
133900980403     C                   CLEAR                   BS69DS
134000980403     C                   CLEAR                   ACODS
134100980403     C                   CLEAR                   INDDS
134200980403     C                   CLEAR                   CLPDS
134300980403     C                   CLEAR                   CLSDS
134400980403     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
134500980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
134600980403     C                   CALL      'TIBS69R'
134700980403     C                   PARM                    BS69DS
134800980403     C                   PARM                    ACODS
134900980403     C                   PARM                    INDDS
135000980403     C                   PARM                    CLPDS
135100980403     C                   PARM                    CLSDS
135200980403     C*
135300980403     C                   ENDSR
135400000000     C*------------------------------------------------------------------------*
135500000000     C* *INZSR - IMPOSTAZIONI INIZIALI
135600000000     C*------------------------------------------------------------------------*
135700000000     C     *INZSR        BEGSR
135800000000     C*
135900000000     C* PARAMETRI IN INPUT AL PGM
136000000000     C     *ENTRY        PLIST
136100000000     C                   PARM                    KPJBA
136200000000     C                   MOVEL     KPJBU         PARAM
136300070531     c*
136400070531     c* apertura condizionata dei 2 files da generare
136500070531     c                   if        PARWfdpv = 'S'
136600110401     c                   open      WAdfpv1l
136700110401     c                   open      WAdfpv4l
136800110401     c                   open      WAdfpv5l
136900070531     c                   else
137000100127     c                   open      WFdea01l
137100100127     c                   open      WFdea04l
137200100127     c                   open      WFdea05l
137300100127     c                   open      WFdeav1l
137400070531     c                   end
137500000000     C*
137600000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
137700000823     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
137800000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
137900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
138000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
138100990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC01L
138200000000     C     *LIKE         DEFINE    SDCANN        KSCANN
138300000000     C     *LIKE         DEFINE    SDCMES        KSCMES
138400990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO01L
138500000000     C     *LIKE         DEFINE    SDOANN        KSOANN
138600000000     C     *LIKE         DEFINE    SDOMES        KSOMES
138700100127     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEa01L
138800000000     C     *LIKE         DEFINE    WFDARE        KWDARE
138900000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
139000000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
139100000000     C     *LIKE         DEFINE    WFDCLV        KWDCLV
139200000823     C     *LIKE         DEFINE    WFDCLI        KWDCLI
139300000000     C     *LIKE         DEFINE    WFDANN        KWDANN
139400000000     C     *LIKE         DEFINE    WFDMES        KWDMES
139500000824     C     *LIKE         DEFINE    WFDFPE        KWDFPE
139600000000     C     *LIKE         DEFINE    WFDIOE        KWDIOE
139700020315     C     *LIKE         DEFINE    WFDNTW        KWDNTW
139800100127     C     *LIKE         DEFINE    WFDTSP        KWDTSP
139900000000     C*
140000000000     C* DEFINIZIONI DEPOSITI E VARIABILI
140100000000     C     *LIKE         DEFINE    CLPKSC        DEPKSC
140200100127     C     *LIKE         DEFINE    CLPKSC        UNIcli
140300000000     C     *LIKE         DEFINE    CLPAGE        DEPAGE
140400000000     C     *LIKE         DEFINE    ORGFIL        DEPFIL
140500000000     C     *LIKE         DEFINE    ORGCAR        DEPARE
140600000000     C     *LIKE         DEFINE    ORGFL3        DEPDIV
140700000000     C     *LIKE         DEFINE    ORGFL3        DEPDIC
140800000000     C     *LIKE         DEFINE    WFDRRN        DEPRRN
140900000000     C     *LIKE         DEFINE    CLPAGE        DEPRGF
141000000000     C     *LIKE         DEFINE    CLPCLV        DEPCLV
141100000000     C     *LIKE         DEFINE    §01AGE        DEPRGD
141200000000     C     *LIKE         DEFINE    SDOCLN        DEPCLN
141300970821     C     *LIKE         DEFINE    WFDIOE        DEPIOE
141400020315     C     *LIKE         DEFINE    WFDNTW        DEPNTW
141500070531     C     *LIKE         DEFINE    WFDRRN        DEPRRN
141600100203     C* x il WFdfpv0F
141700070531     C     *LIKE         DEFINE    WFDpkg        DEPpkg
141800070531     C     *LIKE         DEFINE    WFDvol        DEPvol
141900070723     C     *LIKE         DEFINE    wfdANN        depANN
142000070723     C     *LIKE         DEFINE    wfdMES        depMES
142100000825     C*
142200000825     C* DEFINIZIONI VARIABILI DI LAVORO
142300000825     C     *LIKE         DEFINE    PARSPD        MENSPD
142400000000     C                   MOVEL     '0'           $FINE             1            *FINE PROGRAMMA
142500980601     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
142600980601     C                   MOVEL     '0'           PCLNEW            1            *CLIENTE NUOVO PRECE
142700000000     C                   MOVEL     '0'           CCLNEW            1            *CLIENTE NUOVO CORRE
142800000000     C                   Z-ADD     *ZEROS        C                 4 0          *PER COMMERCIALI
142900000000     C                   Z-ADD     *ZEROS        G                 4 0          *PER FLAG TIPI BOLLA
143000000000     C                   Z-ADD     *ZEROS        J                 4 0          *PER MESI ELABORABIL
143100000000     C                   Z-ADD     *ZEROS        F                 4 0          *PER ORGANIGRAMMA
143200970821     C                   Z-ADD     *ZEROS        P                 4 0          *PER FASCE DI PESO
143300970821     C                   Z-ADD     *ZEROS        A                 4 0          *PER ANNI
143400970821     C                   Z-ADD     *ZEROS        M                 4 0          *PER MESI
143500020404     C                   Z-ADD     *ZEROS        X                 3 0          *PER RAGGRUPPAMENTI REGIONI/NETWORK
143600020404     C                   Z-ADD     *ZEROS        R                 3 0          *PER NETWORK
143700000000     C                   SETON                                        OF        *OVERFLOW
143800000000     C*
143900000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
144000000000     C     KEYORG        KLIST
144100000000     C                   KFLD                    KORFIL                         *FILIALE
144200000000     C*
144300000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
144400000000     C     KEYTAB        KLIST
144500000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
144600000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
144700000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
144800000000     C*
144900000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
145000000000     C     KEYTA2        KLIST
145100000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
145200000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
145300020404     C*
145400020404     C* CHIAVE LETTURA TNTBE00F - PARZIALE
145500020404     C     KEYTBE        KLIST
145600020404     C                   KFLD                    TBECOD
145700000000     C*
145800990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
145900000000     C     KEYSDC        KLIST
146000000000     C                   KFLD                    KSCKSC                         *CODICE CLIENTE
146100000000     C                   KFLD                    KSCANN                         *ANNO
146200000000     C                   KFLD                    KSCMES                         *MESE
146300000000     C*
146400990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
146500000000     C     KEYSDO        KLIST
146600000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
146700000000     C*
146800990609     C* CHIAVE LETTURA SISDO01L - COMPLETA
146900000000     C     KE2SDO        KLIST
147000000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
147100000000     C                   KFLD                    KSOANN                         *ANNO
147200000000     C                   KFLD                    KSOMES                         *MESE
147300100127     C*
147400100127     C* CHIAVE LETTURA WFdeaV1L - COMPLETA
147500100127     C     KEYdeaV       KLIST
147600100215     C                   KFLD                    WFVSVR                         *VARIA
147700100127     C                   KFLD                    WFVKSC                         *CODICE CLIENTE
147800100127     C                   KFLD                    WFVANN                         *ANNO
147900100127     C                   KFLD                    WFVMES                         *MESE
148000100127     C*
148100100127     C* CHIAVE LETTURA WFDEa01L - COMPLETA
148200000000     C     KEYDEL        KLIST
148300000000     C                   KFLD                    KWDDIV                         *DIVISIONE
148400000000     C                   KFLD                    KWDARE                         *AREA
148500000000     C                   KFLD                    KWDFIL                         *FILIALE
148600000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
148700000000     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
148800000823     C                   KFLD                    KWDCLI                         *CLIENTE
148900000000     C                   KFLD                    KWDANN                         *ANNO
149000000000     C                   KFLD                    KWDMES                         *MESE
149100000824     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
149200000000     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
149300020614     C                   KFLD                    KWDNTW                         *NETWORK
149400000823     C*
149500100127     C     KEYDELtsp     KLIST
149600100127     C                   KFLD                    KWDDIV                         *DIVISIONE
149700100127     C                   KFLD                    KWDARE                         *AREA
149800100127     C                   KFLD                    KWDFIL                         *FILIALE
149900100127     C                   KFLD                    KWDAGE                         *COMMERCIALE
150000100127     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
150100100127     C                   KFLD                    KWDCLI                         *CLIENTE
150200100127     C                   KFLD                    KWDANN                         *ANNO
150300100127     C                   KFLD                    KWDMES                         *MESE
150400100127     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
150500100127     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
150600100127     C                   KFLD                    KWDNTW                         *NETWORK
150700100127     C                   KFLD                    KWDTSP                         *TIPO SERVIZIO
150800100127     C*
150900000823     C* IMNPOSTA DATA / ORA CORRENTE
151000000823     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
151100000823     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
151200000823     C                   Z-ADD     *ZEROS        G08INV
151300000823     C                   MOVEL     '0'           G08ERR
151400000823     C                   CALL      'XSRDA8'
151500000823     C                   PARM                    WLBDA8
151600000823     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
151700000000     C*
151800000825     C* CARICA TABELLE OCCORRENTI
151900000000     C                   EXSR      CARTAB
152000970821     C*
152100970821     C* CALCOLA I MESI DA ELABORARE
152200990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
152300970821     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
152400990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
152500970821     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
152600970821     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORAR
152700970821     C                   EXSR      AMELAB
152800970822     C*
152900000825     C* PRENDE IL VALORE ASSOLUTO DELLA % ALLA QUALE SEGNALARE LO SCOSTAMENTO FASCE
153000970822     C                   MLLZO     1             PARSPD                         *VALORE ASSOLUTO
153100000825     C                   Z-ADD     *ZEROS        MENSPD
153200000825     C                   Z-SUB     PARSPD        MENSPD                         *% RICHIESTA MENO
153300001122     C*
153400001122     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
153500010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
153600010725     C                   MOVE      28            WDFIN
153700081114     C*
153800081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
153900081114     C*
154000081114     C*  Ultimo giorno del mese
154100081114     C                   MOVE      WDFIN         MMGG_4            4 0
154200081114     C                   MOVEL     MMGG_4        Mm_2              2 0
154300081114     C                   MOVEl     WDFIN         Anno_4            4 0
154400081114      * se è febbraio
154500081114     c                   If        Mm_2 = 2
154600081114      *
154700081114      *  potrebbe essere bisestile 29 gg.
154800081114     c     Anno_4        div       4             risulta           3 0
154900081114     c                   mvr                     resto             3 0
155000081114      *  Se bisestile
155100081114     c                   If        resto = 0
155200081114     C                   MOVE      '29'          WDFIN
155300081114     c                   end
155400081114      *
155500081114     c                   else
155600081114      *
155700081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
155800081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
155900081114     c                                Mm_2 = 9 or Mm_2 = 11
156000081114     C                   MOVE      '30'          WDFIN
156100081114     c                   else
156200081114     C                   MOVE      '31'          WDFIN
156300081114     c                   end
156400081114      *
156500081114     c                   endIf
156600081114     C*
156700081114     C*----------- F I N E    M E S E ---------------------------*
156800000000     C*
156900000000     C                   ENDSR
157000070723     C*------------------------------------------------------------------------*
157100070723     C* Aggiorna il flag Nuovo/Acquisito x UNificante
157200070723     C*------------------------------------------------------------------------*
157300070723     C     AGGWFD_uni    BEGSR
157400070727     C*
157500070727      *? Per prima cosa deve rendere omogenei i records nell'ambito dell'anno con "Y" e "S"...  ?
157600070727      *? significa che se nell'anno sono nati 2 codici clienti nuovi mi troverei "Y" su 2 mesi ?
157700070727      *? diversi nell'ambito dello stesso Anno.                             ?
157800070727      *? ..... ovviamente prevale la "Y" più vecchia a cui seguono delle "S" x indicare il Nuovo.
157900070727      *? -------------------------------------------------------------------------- ?
158000070727      *  (esempio esplicativo)
158100070727      *               Anno 200X? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
158200070727      *? Cliente (A)...  ?      |   | Y | S | S | S | S | S | S |   | S | S |   |
158300070727      *? Cliente (B)...  ?      |   |   |   | Y | S | S |   | S | S | S |   |   |
158400070727      *? -----------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
158500070727      *? Unificante (C)         |   | Y | S | Y | S | S | S | S | S | S | S |   |                         ?
158600070727      * -->
158700070727      *? Invece deve essere così|   | Y | S | S | S | S | S | S | S | S | S |   |   ?
158800070727      *? -------------------------------------------------------------------------- ?
158900070723     C*
159000070723     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
159100070723     c                   if        PARWfdpv = 'S'
159200110401     C     *LOVAL        SETLL     WAdfpv5L
159300110401     C                   READ      WAdfpv5L                               99
159400070723     c                   else
159500100127     C     *LOVAL        SETLL     WFDEa05L
159600100127     C                   READ      WFDEa05L                               99
159700070723     c                   end
159800070723     C*
159900070723     C* CICLO FINO A FINE FILE
160000070723DO  1C     *IN99         DOWEQ     *OFF
160100070723     C                   move      wfdIOE        depIOE
160200100127     C                   z-add     wfdCLI        UNIcli
160300070723     C                   z-add     0             Cliente_Anno      1 0
160400070723      *
160500070723     C* a Rottura Cliente
160600070723DO  2C     *IN99         DOWEQ     *OFF
160700070723     C     depIOE        ANDEQ     wfdIOE
160800100127     C     UNIcli        ANDEQ     wfdCLI
160900070723     C                   z-add     wfdANN        depANN
161000070723     C                   Clear                   Ypsilon_anno      1
161100070723     C                   add       1             Cliente_Anno
161200070723     C*
161300070723     C* a Rottura Cliente Anno
161400070723DO  3C     *IN99         DOWEQ     *OFF
161500070723     C     depIOE        ANDEQ     wfdIOE
161600100127     C     UNIcli        ANDEQ     wfdCLI
161700070723     C     depANN        ANDEQ     wfdANN
161800070723     C                   z-add     wfdMES        depMES
161900070723     C*
162000070723     C* a Rottura Cliente Anno/Mese
162100070723DO  4C     *IN99         DOWEQ     *OFF
162200070723     C     depIOE        ANDEQ     wfdIOE
162300100127     C     UNIcli        ANDEQ     wfdCLI
162400070723     C     depANN        ANDEQ     wfdANN
162500070723     C     depMES        ANDEQ     wfdMES
162600070723      *
162700070723      *  Memorizza il primo Ypsilon trovato x cliente Anno
162800070723     C                   if        wfdCLN <> ' ' and Ypsilon_anno = *blank
162900070723     C                   eval      Ypsilon_anno = wfdCLN
163000070723     c                   end
163100070723      *
163200070723      * Tutto il mese cliente deve essere a "Y"
163300070723     C                   if        Ypsilon_anno = 'Y'
163400070723     c                   eval      wfdCLN = 'Y'
163500070723     c                   end
163600070723      *
163700070723      * Dal mese seguente deve impostare sempre la "S"
163800070723      *   anche per l'anno seguente...
163900070723     C                   if        Ypsilon_anno = 'S'
164000070723     c                   eval      wfdCLN = 'S'
164100070723     c                   end
164200070723      *
164300070723     c                   if        PARWfdpv = 'S'
164400100203     C                   UPDATE    WFdfp5
164500070723     c                   else
164600100127     C                   UPDATE    WFDEa5
164700070723     c                   end
164800070723     C*
164900070723     C* LETTURA SUCCESSIVA DATI CLIENTE
165000070723     c                   if        PARWfdpv = 'S'
165100110401     C                   READ      WAdfpV5L                               99
165200070723     c                   else
165300100127     C                   READ      WFDEa05L                               99
165400070723     c                   end
165500070723E   4C                   ENDDO                                                  *ROTTURA Mese
165600070723     C*
165700070723     C*  Se il mese letto aveva la "Y" i mesi seguenti entro l'anno devono avere la "S"
165800070723     C                   if        Ypsilon_anno = 'Y'
165900070723     C                   eval      Ypsilon_anno = 'S'
166000070723     c                   end
166100070723      *
166200070723E   3C                   ENDDO                                                  *ROTTURA CLIENTE/Anno
166300070723     C*
166400070723E   2C                   ENDDO
166500070723     C*
166600070723E   1C                   ENDDO
166700070727     C*
166800070727      *? ------------------------------------------------------------------------------------------------- ?
166900070727      *? ......Dopo avere reso omogeneo nell'ambito dell'anno la sequenza di "Y" e "S", si deve considerare    ?
167000070727      *? sull'anno in CORSO che, se un cliente aveva già del fatturato nell'anno precedente,                           ?
167100070727      *? NON può considerarsi Nuovo.!!!!!!                         ?
167200070727      *? ..... quindi, sul Cliente, nell'Anno in CORSO, si devono eliminare le "Y" e "S" su tutti i records.
167300070727      *? ------------------------------------------------------------------------------------------------- ?
167400070727      *  (esempio esplicativo)
167500070727      *                              ? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
167600070727      *? Unific.(C) Anno Precedente ? |   |   |   | Y | S | S | S | S | S | S | S | S |
167700070727      *? Unific.(C) Anno  in  CORSO ? |   | Y | S | S | S | S | S | S |   |   |   |   |
167800070727      *? -----------------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
167900070727      *? ============================     ?
168000070727      * --> Invece deve essere così
168100070727      *? ============================     ?
168200070727      *? Unific.(C) Anno Precedente   |   |   |   | Y | S | S | S | S | S | S | S | S |   ?-- records anno precedente
168300070727      *? Unific.(C) Anno  in  CORSO   |   |   |   |   |   |   |   |   |   |   |   |   |   ?-- records anno in Corso
168400070727      *? -------------------------------------------------------------------------------- ?
168500070727     C*   Con un istruzione SQL possiamo quindi sistemare la situazione sull'anno in corso
168600070727     C*   semplicemente controllando se nell'anno precedente il Cliente aveva del Fatturato
168700070727     C*   quindi, aggiorniamo tutti i Suoi records dell'Anno in corso x dire che NON è NUOVO.
168800070727      *
168900070727     C/EXEC SQL
169000100127     C+ UPDATE wfdea00f SET wfdcln = ' '
169100070727     C+ WHERE wfdann = :ParAAI AND wfdcln <>' ' AND wfdcli
169200100127     C+ IN (SELECT wfdcli FROM wfdea00f WHERE wfdann = :ParAFP)
169300070727     C/END-EXEC
169400070723     C*
169500121210     C*  Una volta sistemati tutti i records per l'anno in corso con solo la sequenza
169600121210     C*  giusta dal mese in cui nasce il cliente NUOVO.
169700121210     C*  Occorre sistemare eventuali saldi Antecedenti dovuti a storni o spalmature
169800121210     C*  di valori attribuiti al cliente UNIFICANTE....non potendo fare diversamente.
169900121207     C                   Exsr      NUOVO_CON_MESI
170000121207     C*
170100121207     C                   ENDSR
170200121207      *------------------------------------------------------------------------*
170300121207     C* Se L'AUT è considerato NUOVO nell'anno ed ha mesi precedenti con BLANK
170400121207      *------------------------------------------------------------------------*
170500121207     C     NUOVO_con_mesiBEGSR
170600121207     C*
170700121207     C*  se sono state fatte delle Note di Credito o altro tipo di Storni o Rettifiche
170800121210     C*  che vengono spalmate sui mesi precedenti a quello che ha la "Y" come mese iniziale.
170900121210     C*  Occorre ragionare impostando la "Y" anche su tali record ed in seguito fare
171000121210     C*  riferimento al mese più recente con la "Y" per conoscere qual'è il mese di VERO
171100121210     C*  inizio per il codice che si sta trattando.
171200121210     C*  Quindi sui mesi più vecchi ANTECEDENTI: verrà impostata la "Y" per poter
171300121210     C*  determinare nel programma TISE93R a rottura di cliente primo mese letto, se si
171400121210     C*  tratta di cliente NUOVO oppure acquisito.
171500121210     C*
171600121210     C/EXEC SQL
171700121210     C+ update  wfdea00f set  WFDCLN ='Y' WHERE wfdcln=' ' and
171800121210     C+ wfdcli in (select wfdcli from wfdea00f where wfdcln <> ' '
171900121210     C+ and wfdann = :PARAAI) and  wfdcli in  ( SELECT WFDCLI FROM
172000121210     C+ wfdea00f where wfdcli in (select wfdcli from wfdea00f
172100121210     C+ where wfdcln <> ' ' and wfdann = :PARAAI) and  wfdcln=' ')
172200121210     C/END-EXEC
172300121210     C*
172400121207     C                   ENDSR
172500121207     O*------------------------------------------------------------------------*
172600000000     O* STAMPA DEI CODICI AGENTI INESISTENTI O ANNULLATI DA INSERIRE IN SEDE   *
172700000000     O*------------------------------------------------------------------------*
172800000000     OQSYSPRT   E            TESTA            02
172900000000     O                       STA(1)              66
173000000000     O                       STA(2)             132
173100980306     O                       UDATE              119 '  /  /  '
173200000000     O                       PAGE          Z    132
173300000000     O*------------------------
173400000000     O          E            TESTA            03
173500000000     O                       STA(3)              66
173600000000     O                       STA(3)             132
173700000000     O*------------------------
173800000000     O          E            TESTA            04
173900000000     O                       STA(4)              66
174000000000     O*------------------------
174100000000     O          E            TESTA            05
174200000000     O                       STA(3)              66
174300000000     O                       STA(3)             132
174400000000     O*------------------------
174500000000     O          E            ERROR       1
174600000000     O                       DEPAGE               9
174700000000     O                       DEPKSC              27
174800100127     O                       UNIcli              47
174900000000     O*------------------------------------------------------------------------*
175000000000** STA
175100100127                           TISE92AR     ***  AGENTI INESISTENTI DA1
175200980306 MODIFICARE IN ANAGRAFICA CLIENTE  ***       99/99/99     PAG.99992
175300000000------------------------------------------------------------------3
175400000824A G E N T E      C L I E N T E     U N I F I C A N T E            4
175500000824  9999999           9999999             9999999                   5
