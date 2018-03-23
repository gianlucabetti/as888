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
001300140612     FAZCMM01L  IF   E           K DISK    usropn
001400020404     FTNTBE01L  IF   E           K DISK
001500990609     FSISDC01L  IF   E           K DISK
001600990609     FSISDO01L  IF   E           K DISK
001700100127     FSISDP01L  IF   E           K DISK
001800100202      *------
001900100202      *? OUTPUT
002000100202      *------
002100100202      *?    St.DELTA
002200100127     FWFDEaV1L  UF A E           K DISK    usropn
002300100127     FWFDEa01L  UF A E           K DISK    usropn
002400100127     FWFDEa04L  UF   E           K DISK    usropn
002500100127     F                                     RENAME(WFDEA000:WFDEA4)
002600100127     FWFDEa05L  UF   E           K DISK    usropn
002700100127     F                                     RENAME(WFDEA000:WFDEA5)
002800100202      *---
002900100202      *?    St.Fascie PESI VOLUMI
003000110401     FWAdfpv1L  UF A E           K DISK    usropn
003100110401     FWAdfpv4L  UF   E           K DISK    usropn
003200100203     F                                     RENAME(WFDFPV00:WFDFPV)
003300110401     FWAdfpv5L  UF   E           K DISK    usropn
003400100203     F                                     RENAME(WFDFPV00:WFDFP5)
003500100202      *------
003600000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003700000000     D*------------------------------------------------------------------------*
003800000000     D* SCHIERE
003900000000     D*------------------------------------------------------------------------*
004000000000     D*------------------
004100000000     D* RIGHE DI STAMPA
004200000000     D*------------------
004300000000     D STA             S             66    DIM(5) CTDATA PERRCD(1)
004400000000     D*------------------
004500000000     D* COMMERCIALI IN ERRORE
004600000000     D*------------------
004700020314     D ERG             S              7  0 DIM(500)
004800000000     D*------------------
004900000000     D* ORGANIGRAMMA
005000000000     D*------------------
005100971124     D FIL             S              3  0 DIM(500)                             *FILIALI
005200971124     D DFIL            S             20    DIM(500)
005300971124     D ARE             S              3  0 DIM(500)                             *AREE
005400971124     D DARE            S             20    DIM(500)
005500971124     D DIV             S              1    DIM(500)                             *DIVISIONI
005600971124     D DDIV            S             20    DIM(500)
005700000000     D*------------------
005800000000     D* COMMERCIALI
005900000000     D*------------------
006000100202     D AGE             S              7  0 DIM(5000)                            *CODICI AGENTE
006100100202     D RGF             S              7  0 DIM(5000)                            *CODICI RAGGRUPPAMEN
006200100202     D DAGE            S             25    DIM(5000)                            *DECODIFICHE AGENTI
006300000000     D*------------------
006400000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
006500000000     D*------------------
006600000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
006700000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
006800000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
006900970821     D*------------------
007000970821     D* FASCE DI PESO
007100970821     D*------------------
007200020522     D FPE             S              2    DIM(11)
007300970821     D*------------------
007400970821     D* PERIODO DA ELABORARE
007500970821     D*------------------
007600990609     D ANN             S              4  0 DIM(12)                              *ANNO
007700000823     D MES             S              2  0 DIM(12)                              *MESE
007800000825     D*------------------
007900000825     D* MESI CON SCOSTAMENTO DELTA FASCE DA SEGNALARE
008000000825     D*------------------
008100000825     D SMEI            S              2  0 DIM(12)                              *ITALIA
008200000825     D SMEE            S              2  0 DIM(12)                              *ESTERO
008300000825     D SMET            S              2  0 DIM(12)                              *TOTALE
008400020314     D*------------------
008500020314     D* RAGGRUPPAMENTI REGIONI/NETWORK
008600020314     D*------------------
008700020315     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIONE
008800020315     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RAGGR. REGIONE
008900020404     D*------------------
009000020404     D* NETWORK AZIENDALI
009100020404     D*------------------
009200020404     D NTWCOD          S              3    DIM(100)                             *CODICI NETWORK
009300020404     D NTWORD          S              2  0 DIM(100)                             *PRIORITA' CONSIDERAZIONE NETWORK
009400000823     D*------------------
009500000823     D* DS CONTROLLO DATA (8)
009600000823     D*------------------
009700000823     D WLBDA8          DS                  INZ
009800000823     D  G08DAT                 1      8  0
009900000823     D  G08INV                 9     16  0
010000000823     D  G08ERR                17     17
010100000823     D  G08TGI                18     22  0
010200000823     D*------------------
010300000823     D* DS DI PROCEDURA
010400000823     D*------------------
010500000823     D BS10DS        E DS                  EXTNAME(TIBS10DS)
010600000000     D*------------------
010700000000     D* PARAMETRI DI LANCIO
010800000000     D*------------------
010900000000     D PARAM           DS
011000990609     D  PARDAT                 1      6  0
011100990609     D  PARAAI                 1      4  0
011200990609     D  PARMMI                 5      6  0
011300990609     D  PARDA2                 7     12  0
011400990609     D  PARAAF                 7     10  0
011500990609     D  PARMMF                11     12  0
011600990609     D  PARDCK                13     13
011700990609     D  PARDCC                14     14
011800990609     D  PARDCF                15     15
011900990609     D  PARFIC                16     18  0
012000990609     D  PARARC                19     21  0
012100990609     D  PARDIC                22     22
012200990609     D  PARDFF                23     23
012300990609     D  PARCOF                24     30  0
012400990609     D  PARDIF                31     31
012500990609     D  PARSTA                32     32
012600000824     D  PARSCE                33     33
012700990609     D  PARTSP                34     34
012800990609     D  PARIOE                35     35
012900990609     D  PARCLV                36     36
013000990609     D  PARDPT                37     42  0
013100990609     D  PARAPI                37     40  0
013200990609     D  PARMPI                41     42  0
013300990609     D  PARDP2                43     48  0
013400990609     D  PARAFP                43     46  0
013500990609     D  PARMFP                47     48  0
013600990609     D  PARSPD                49     52  1
013700990609     D  PARPDD                53     56  1
013800990609     D  PARPDA                57     60  1
013900000707     D  PARPOS                61     61
014000000825     D  PARCRE                62     62
014100010618     D  PARCL1                63     63
014200010618     D  PARCL2                64     64
014300010618     D  PARSIN                65     65
014400020404     D  PARCL3                66     66
014500020404     D  PARCL4                67     67
014600020404     D  PARNTW                68     68
014700030923     D  PARDFD                69     72  1
014800030923     D  PARDFA                73     76  1
014900030923     D  PARDFDP               77     80  1
015000030923     D  PARDFAP               81     84  1
015100030923     D  PARFPC                85     85
015200030923     D  PARWPC                86     86
015300030923     D  PARWCP                87     87
015400070531     D  PARWfdpv              88     88
015500000000     D*------------------
015600990609     D* DATA DI SISDC01L
015700000000     D*------------------
015800000000     D                 DS
015900990609     D  SDCANN                 1      4  0
016000990609     D  SDCMES                 5      6  0
016100990609     D  SDCDAT                 1      6  0
016200100215     D*------------------
016300100215     D* DATA DI SISDO00F
016400100215     D*------------------
016500100215     D                 DS
016600100215     D  SDOANN                 1      4  0
016700100215     D  SDOMES                 5      6  0
016800100215     D  SDODAT                 1      6  0
016900970821     D*------------------
017000100127     D* DATA DI WFDEA00F
017100970821     D*------------------
017200970821     D                 DS
017300970821     D  WFDANN                 1      4  0
017400970821     D  WFDMES                 5      6  0
017500990609     D  WFDDAT                 1      6  0
017600000000     D*------------------
017700000000     D* DS COMMERCIALI
017800000000     D*------------------
017900000000     D DS01          E DS
018000000000     D*------------------
018100000000     D* DS FASCE DI PESO
018200000000     D*------------------
018300000000     D DS2L          E DS
018400000000     D*------------------
018500000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
018600000000     D*------------------
018700000000     D DS2W          E DS
018800980403     D*------------------
018900980403     D* DS REPERIMENTO DATI UTENTE
019000980403     D*-------------------
019100980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
019200980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
019300980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
019400980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
019500980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
019600000000     D*------------------
019700000000     D* ARCHITETTURA
019800000000     D*------------------
019900000000     D KPJBA         E DS
020000020314     D*------------------
020100020314     D* DS DI LETTURA TABELLA "2M"
020200020314     D*------------------
020300020314     D DS2M          E DS
020400020404     D*-------------------
020500020404     D* DS DEFINIZIONE TABELLA "NETWORK"
020600020404     D*-------------------
020700020404     D DNTW          E DS
020800000000     C*------------------------------------------------------------------------*
020900000000     C* MAIN LINE
021000000000     C*------------------------------------------------------------------------*
021100000000     C*
021200000000     C* LEGGO IL PRIMO CLIENTE VALIDO
021300000000     C                   Z-ADD     *ZEROS        KSOKSC
021400990609     C     KEYSDO        SETGT     SISDO01L
021500990609     C                   READ      SISDO01L                               99
021600000000     C*
021700000000     C* PER TUTTI I CLIENTI PRESENTI NEI SALDI
021800000000DO  1C     *IN99         DOWEQ     *OFF
021900980119     C                   Z-ADD     SDOKSC        DEPKSC                         *DEPOSITO CLIENTE
022000980119     C*
022100980119     C* CONTROLLA VALIDITA' RECORD
022200980119     C                   EXSR      CHKSDO
022300000000     C*
022400000000     C* OPERAZIONI PER NUOVO CLIENTE
022500980119IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
022600000000     C                   EXSR      NEWKSC
022700100215     C*
022800000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
022900000000     C                   EXSR      SETFIL
023000000000     C*
023100000000     C* CICLO FINO A ROTTURA CODICE CLIENTE
023200980119DO  3C     $FINE         DOWEQ     '0'
023300000000     C*
023400000000     C* OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
023500000000     C                   EXSR      NEWDAT
023600100216     C*
023700100216     C* CREA I SALDI NEL WORK FILE x le VARIE
023800100216     c                   if        PARWfdpv <>'S'
023900100216     C                   EXSR      MEMSALVAR
024000100216     c                   end
024100000000     C*
024200000000     C* CICLO FINO A ROTTURA PERIODO SALDO CLIENTE
024300980119DO  4C     $FINE         DOWEQ     '0'
024400000000     C     SDCDAT        ANDEQ     DEPDAT
024500000000     C*
024600000000     C* CREA I SALDI NEL WORK FILE
024700000824     C                   EXSR      MEMSAL
024800000000     C*
024900000000     C* LEGGE IL RECORD SUCCESSIVO
025000000000     C                   EXSR      LETFIL
025100980119E   4C                   ENDDO                                                  *ROTTURA PERIODO
025200980119E   3C                   ENDDO                                                  *ROTTURA CLIENTE
025300000824E   2C                   ENDIF
025400000000     C*
025500000000     C* LEGGO IL SUCESSIVO CLIENTE VALIDO
025600000000     C                   Z-ADD     DEPKSC        KSOKSC
025700990609     C     KEYSDO        SETGT     SISDO01L
025800990609     C                   READ      SISDO01L                               99
025900000824E   1C                   ENDDO
026000000824     C*
026100000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
026200000825     C                   EXSR      AGGWFD
026300070723     C*
026400070723      *?reimposta correttamente il cliente Nuovo/Acquisito x Unificante...  ?
026500070723     c                   if        parSCE = 'U'
026600070723     C                   EXSR      AGGWFD_Uni
026700070723     c                   end
026800980403     C*
026900980403     C* OPERAZIONI FINALI
027000980403     C                   EXSR      FINSR
027100000000     C*
027200000000     C                   SETON                                        LR
027300000000     C*------------------------------------------------------------------------*
027400000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
027500000000     C*------------------------------------------------------------------------*
027600000000     C     NEWKSC        BEGSR
027700000824     C*
027800000824     C* IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-
027900000823IF  1C     PARSCE        IFEQ      'U'                                          *STAT PER UNIFICANTE
028000000823     C                   CLEAR                   BS10DS
028100001122     C*---               Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
028200010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
028300000823     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
028400000823     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
028500000823     C                   MOVE      DEPKSC        D10COD
028600000823     C                   CALL      'TIBS10R'
028700000823     C                   PARM                    BS10DS
028800100127     C                   Z-ADD     DEPKSC        UNIcli
028900000823IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
029000000823     C     D10COP        ANDNE     *ZEROS
029100100127     C                   Z-ADD     D10COP        UNIcli                         *UNIFICANTE
029200000823E   2C                   ENDIF
029300000823X   1C                   ELSE                                                   *STAT PER CLIENTE
029400100127     C                   Z-ADD     DEPKSC        UNIcli
029500000823E   1C                   ENDIF
029600000824     C*
029700000000     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE
029800100127     C                   MOVEL     UNIcli        DEPFIL                         *FIL+XXXX
029900000000     C                   Z-ADD     1             F
030000000000     C                   SETOFF                                           99
030100000000     C     DEPFIL        LOOKUP    FIL(F)                                 99
030200000000IF  1C     *IN99         IFEQ      *ON
030300000000     C                   Z-ADD     ARE(F)        DEPARE
030400000000     C                   MOVEL     DIV(F)        DEPDIV
030500000000X   1C                   ELSE
030600000000     C                   Z-ADD     *ZEROS        DEPARE
030700000000     C                   MOVEL     *BLANKS       DEPDIV
030800000000E   1C                   ENDIF
030900000824     C*
031000000824     C* REPERISCE IL COMMERCIALE, SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
031100980403     C                   CLEAR                   BS69DS
031200980403     C                   CLEAR                   CLPDS
031300980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
031400100127     C                   MOVEL     UNIcli        I69KCP                         *CLIENTE X CNCLP
031500980403     C                   CALL      'TIBS69R'
031600980403     C                   PARM                    BS69DS
031700980403     C                   PARM                    ACODS
031800980403     C                   PARM                    INDDS
031900980403     C                   PARM                    CLPDS
032000980403     C                   PARM                    CLSDS
032100980406IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
032200980403     C     CLPAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
032300100127     C                   MOVEL     UNIcli        NUM3              3 0          *CODICE FILIALE +
032400980403     C                   Z-ADD     *ZEROS        DEPAGE                         *AGENTE A ZERO  =
032500980403     C                   MOVEL     NUM3          DEPAGE                         *FIL+0000
032600980403X   1C                   ELSE                                                   *NO ERRORE
032700980403     C                   Z-ADD     CLPAGE        DEPAGE                         *CODICE AGENTE
032800980403E   1C                   ENDIF
032900000824     C*
033000000000     C* REPERISCE IL CODICE QUALITA'
033100000000     C                   MOVEL     'C'           DEPCLV
033200000000IF  1C     CLPCLV        IFNE      *BLANKS
033300000000     C                   MOVEL     CLPCLV        DEPCLV
033400000000E   1C                   ENDIF
033500000824     C*
033600000000     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
033700000000     C                   Z-ADD     1             C
033800000000     C                   SETOFF                                           99
033900000000     C     DEPAGE        LOOKUP    AGE(C)                                 99
034000000000IF  1C     *IN99         IFEQ      *OFF
034100000000     C                   EXSR      STAERR
034200000000     C                   Z-ADD     DEPAGE        DEPRGF                         RAGGRUPP = COD AGENT
034300000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
034400000000X   1C                   ELSE
034500000000IF  2C     RGF(C)        IFEQ      *ZEROS
034600000000     C                   Z-ADD     DEPAGE        DEPRGF
034700000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
034800000000X   2C                   ELSE
034900000000     C                   Z-ADD     RGF(C)        DEPRGF
035000000000     C*
035100000000     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
035200000000     C                   Z-ADD     1             C
035300000000     C                   SETOFF                                           99
035400000000     C     DEPRGF        LOOKUP    AGE(C)                                 99
035500000000IF  3C     *IN99         IFEQ      *ON
035600000000     C                   MOVEL     DAGE(C)       DEPRGD                         DESCRIZIONE RAGGR.TO
035700000000X   3C                   ELSE
035800000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGR.TO
035900000000E   3C                   ENDIF
036000000000E   2C                   ENDIF
036100000000E   1C                   ENDIF
036200000824     C*
036300000000     C* REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
036400000000     C                   Z-ADD     1             F
036500000000     C                   SETOFF                                           99
036600000000     C                   MOVEL     DEPRGF        NUM3                           *FIL+XXXX
036700000000     C     NUM3          LOOKUP    FIL(F)                                 99
036800000000IF  1C     *IN99         IFEQ      *ON
036900000000     C                   MOVEL     DIV(F)        DEPDIC
037000000000X   1C                   ELSE
037100000000     C                   MOVEL     *BLANKS       DEPDIC
037200000000E   1C                   ENDIF
037300000000     C*
037400000000     C                   ENDSR
037500000000     C*------------------------------------------------------------------------*
037600000000     C* NEWDAT - OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
037700000000     C*------------------------------------------------------------------------*
037800000000     C     NEWDAT        BEGSR
037900000000     C*
038000990609     C                   Z-ADD     SDCDAT        DEPDAT            6 0
038100000000     C*
038200000000     C* VEDO SE IL CLIENTE, NEL PERIODO, E' NUOVO O ACQUISITO
038300100127     C********           Z-ADD     UNIcli        KSOKSC
038400070720     C*
038500070720      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
038600070720     C                   Z-ADD     DEPksc        KSOKSC
038700070720     C*
038800000000     C                   Z-ADD     SDCANN        KSOANN
038900000000     C                   Z-ADD     SDCMES        KSOMES
039000990609     C     KE2SDO        CHAIN     SISDO01L                           99
039100000000IF  1C     *IN99         IFEQ      *OFF
039200070720     C                   MOVEL     SDOCLN        DEPCLN
039300000000X   1C                   ELSE
039400000000     C                   MOVEL     *BLANKS       DEPCLN
039500000000E   1C                   ENDIF
039600000000     C*
039700000000     C                   ENDSR
039800100127     C*------------------------------------------------------------------------*
039900100127     C* MEMSALVAR - MEMORIZZA I DATI
040000100127     C*------------------------------------------------------------------------*
040100100127     C     MEMSALvar     BEGSR
040200100127     C*
040300100128      *  ciclo sui saldi delle VARIE per riportarne i totali delle "e" e "h"
040400100127     C*
040500100127      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
040600100127     C                   Z-ADD     DEPksc        KSOKSC
040700100216     C                   Z-ADD     SDcDAT        DEPDAT            6 0
040800100216     C                   Z-ADD     SDcANN        KSOANN
040900100216     C                   Z-ADD     SDcMES        KSOMES
041000100127     C     KE2SDO        setll     SISDp01L
041100100127     C     KE2SDO        reade     SISDp01L
041200100127     C*
041300100127     c                   dow       not %EoF(SISDp01L)
041400100127     C*
041500100128     C*  Solo legate alla Priority o H10:30
041600130131     C*      al 31/1/2013 viene chiesto di calcolare il FUEL per scorporarlo dai ricavi
041700130131     C*
041800130131     C*   PRO-MEMORIA: le varie non hanno alcuna possibilità di essere distinte
041900130131     C*                x Italia/Estero o x Tipo servizio o x Network....Quindi possono
042000130131     C*                essere calcolate solo a livello di TOTALE ossia ITALIA+ESTERO ed
042100130131     C*                esclusivamente per CLIENTE o CLIENTE UNIFICANTE
042200130131     C*                infatti anche già adesso avviene così per l'espresso/H10:30.
042300100128     c                   if        SDpSVR = 'e' or SDpSVR = 'h'
042400130131     c                                or SDpSVR = 'f'                            <--- VARIE (f=FUEL)
042500100128     C*
042600100127     C* CONTROLLA ESISTENZA RECORD DI LAVORO
042700100127     C                   Z-ADD     UNIcli        wfvksc                         *CLIENTE
042800100127     C                   Z-ADD     SDpANN        wfvANN                         *ANNO
042900100127     C                   Z-ADD     SDpMES        wfvMES                         *MESE
043000100127     C                   moveL     SDpSVR        wfvSVR                         *VARIA
043100100127     C     KEYDEAV       CHAIN     WFDEAV1L
043200100127     C***
043300100127     C* I M M I S S I O N E
043400100127     C***
043500100127IF  1C                   IF        not %Found(WFDEAV1L)
043600100127      * --
043700100127     c                   CLEAR                   WFDEAV00
043800100127     C                   Z-ADD     UNIcli        WFvksc                         *CLIENTE
043900100127     C                   Z-ADD     SDpANN        WFvANN                         *ANNO
044000100127     C                   Z-ADD     SDpMES        WFvMES                         *MESE
044100100127     C                   MOVEL     SDpSVR        WFvSVR                         *VARIA
044200100127     C                   Z-ADD     SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
044300100127     C                   z-add     SDpRCP        WFvRCP                         *Ricavi Particolari
044400100127      * --
044500100127     C                   WRITE     WFDEAV00
044600100127      * --
044700100127X   1C                   ELSEif    %Found(WFDEAV1L)
044800100127     C***
044900100127     C* V A R I A Z I O N E
045000100127     C***
045100100127     C                   add       SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
045200100127     C                   add       SDpRCP        WFvRCP                         *Ricavi Particolari
045300100127      * ---
045400100127     C                   UPDATE    WFDEAV00
045500100127E   1C                   ENDIF
045600100127     C*
045700100128     c                   end
045800100128     C*
045900100127     C     KE2SDO        reade     SISDp01L
046000100127E   1C                   ENDdo
046100100127     C*
046200100127     C                   ENDSR
046300980119     C*------------------------------------------------------------------------*
046400980119     C* CHKSDO - CONTROLLA VALIDITA' RECORD
046500980119     C*------------------------------------------------------------------------*
046600980119     C     CHKSDO        BEGSR
046700980119     C*
046800980119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
046900980119     C*
047000980119     C                   ENDSR
047100000000     C*------------------------------------------------------------------------*
047200000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
047300000000     C*------------------------------------------------------------------------*
047400000000     C     SETFIL        BEGSR
047500000000     C*
047600000000     C* REIMPOSTO FLAG DI FINE LETTURA SALDI
047700000000     C                   MOVEL     '0'           $FINE
047800000000     C*
047900000000     C                   Z-ADD     DEPKSC        KSCKSC                         *CLIENTE
048000000000     C                   Z-ADD     PARAPI        KSCANN                         *ANNO PRECEDENTE INI
048100000000     C                   Z-ADD     PARMPI        KSCMES                         *MESE PRECEDENTE INI
048200000000     C*
048300000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO SALDO CLIENTE VALIDO
048400990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
048500000000     C*
048600000000IF  1C     *IN99         IFEQ      '1'
048700000000     C                   MOVEL     '1'           $FINE
048800000000X   1C                   ELSE
048900000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
049000000000E   1C                   ENDIF
049100000000     C*
049200000000     C                   ENDSR
049300000000     C*------------------------------------------------------------------------*
049400000000     C* LETFIL - LETTURA PROSSIMO RECORD SALDI CLIENTE
049500000000     C*------------------------------------------------------------------------*
049600000000     C     LETFIL        BEGSR
049700000000     C*
049800000000     C* LEGGO FINO A:
049900000000     C*     - FINE FILE
050000000000     C*     - TROVATO RECORD VALIDO
050100980601     C                   MOVEL     'N'           WRECOK
050200000000DO  1C     $FINE         DOWEQ     '0'
050300000000     C     WRECOK        ANDEQ     'N'
050400000000     C*
050500990609     C     KSCKSC        READE     SISDC01L                               99
050600000000     C*
050700000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
050800000000     C                   MOVEL     '1'           $FINE
050900000000X   2C                   ELSE
051000000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
051100000000E   2C                   ENDIF
051200000000     C*
051300000000E   1C                   ENDDO
051400000000     C*
051500000000     C                   ENDSR
051600000000     C*------------------------------------------------------------------------*
051700000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
051800000000     C*------------------------------------------------------------------------*
051900000000     C     CHKREC        BEGSR
052000000000     C*
052100980601     C                   MOVEL     'S'           WRECOK
052200020516     C*
052300020516     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
052400020516     C                   Z-ADD     1             G
052500020516     C                   SETOFF                                           28
052600020516     C     SDCRBL        LOOKUP    FLAB(G)                                28
052700020516IF  1C     *IN28         IFEQ      *OFF                                         *TIPO NON TROVATO --
052800020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
052900020516E   1C                   ENDIF
053000020516     C*
053100020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
053200020516IF  1C     WRECOK        IFEQ      'S'
053300020516IF  2C     SDCNSP        IFEQ      *ZEROS
053400020516     C     SDCPKG        ANDEQ     *ZEROS
053500020516     C     SDCPCC        ANDEQ     *ZEROS
053600020516     C     SDCPLS        ANDEQ     *ZEROS
053700020516     C     SDCNCO        ANDEQ     *ZEROS
053800020516     C     SDCVOL        ANDEQ     *ZEROS
053900020516     C     SDCVCC        ANDEQ     *ZEROS
054000020516     C     SDCVLS        ANDEQ     *ZEROS
054100020516     C     SDCIRB        ANDEQ     *ZEROS
054200020516     C     SDCIRR        ANDEQ     *ZEROS
054300020516     C     SDCINC        ANDEQ     *ZEROS
054400020516     C     SDCIRP        ANDEQ     *ZEROS
054500020516     C     SDCIRC        ANDEQ     *ZEROS
054600020516     C     SDCIRD        ANDEQ     *ZEROS
054700020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
054800020516E   2C                   ENDIF
054900020516E   1C                   ENDIF
055000000000     C*--------------------
055100000000     C* ESCLUSIONI
055200000000     C*--------------------
055300970821     C                   Z-ADD     1             P
055400970821     C                   SETOFF                                           28
055500970821     C     SDCFPE        LOOKUP    FPE(P)                                 28
055600970821IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
055700970821     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055800970821E   1C                   ENDIF
055900000000     C*--------------------
056000000000     C* SELEZIONI
056100000000     C*--------------------
056200000000     C* SE I SALDI SONO DI DATA SUPERIORE A QUELLA RICHIESTA, ESCO DAL CICLO
056300000000IF  1C     SDCDAT        IFGT      PARDA2
056400000000     C                   MOVEL     'N'           WRECOK
056500000000     C                   MOVEL     '1'           $FINE
056600000000E   1C                   ENDIF
056700000000     C*
056800000000     C* SE TIPO SERVIZIO <> TIPO SERVIZIO RICHIESTO, ESCLUDO
056900100127IF  1C**** PARTSP        IFNE      *BLANKS                                      *NON TUTTI
057000100127     C**** SDCTSP        ANDNE     PARTSP
057100100127     C*********          MOVEL     'N'           WRECOK
057200100127E   1C*********          ENDIF
057300000000     C*
057400000000     C                   ENDSR
057500000000     C*------------------------------------------------------------------------*
057600000824     C* MEMSAL - MEMORIZZA I DATI
057700000000     C*------------------------------------------------------------------------*
057800000824     C     MEMSAL        BEGSR
057900070531     C*
058000070531     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
058100070531     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
058200070531     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
058300070531     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
058400070531     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
058500000000     C*
058600000000     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
058700000000     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
058800000000IF  1C     FSP(G)        IFNE      'S'
058900000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
059000070531     C                   z-add     0             sdcPKG                         *PESO
059100070531     C                   z-add     0             sdcPCC                         *PESO CON CML
059200070531     C                   z-add     0             sdcPLS                         *PESO CML
059300070531     C                   z-add     0             sdcVOL                         *VOLUME TOTALE
059400070531     C                   z-add     0             sdcVCC                         *VOLUME CON CML
059500070531     C                   z-add     0             sdcVLS                         *VOLUME CML
059600000000E   1C                   ENDIF
059700000000     C*
059800070531     C* imposta il Peso ed il Volume
059900070531     c                   z-add     sdcPKG        depPKG
060000070531     c                   z-add     sdcVOL        depVOL
060100070531     C*
060200000000     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
060300000000     C                   Z-ADD     *ZEROS        DEPRRN
060400000000     C     SDCIRD        ADD       SDCIRR        DEPRRN
060500000000     C     SDCINC        ADD       DEPRRN        DEPRRN
060600020314     C*
060700020314     C* DETERMINA IL NETWORK
060800020404     C                   MOVEL     *BLANKS       DEPNTWP           3
060900020404     C                   Z-ADD     *ZEROS        DEPORDP           3 0
061000020314     C                   Z-ADD     1             R
061100020314     C     SDCREP        LOOKUP    RGREG(R)                               55
061200020314     C                   IF        *IN55 = *ON
061300020404     C                   EVAL      DEPNTWP = RGNTW(R)
061400020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
061500020404     C                   Z-ADD     1             X
061600020404     C     DEPNTWP       LOOKUP    NTWCOD(X)                              55
061700020404     C                   IF        *IN55 = *ON
061800020404     C                   EVAL      DEPORDP = NTWORD(X)
061900020404     C                   ENDIF
062000020314     C                   ENDIF
062100020314     C*
062200020404     C                   MOVEL     *BLANKS       DEPNTWA           3
062300020404     C                   Z-ADD     *ZEROS        DEPORDA           3 0
062400020314     C                   Z-ADD     1             R
062500020314     C     SDCREA        LOOKUP    RGREG(R)                               55
062600020314     C                   IF        *IN55 = *ON
062700020404     C                   EVAL      DEPNTWA = RGNTW(R)
062800020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
062900020404     C                   Z-ADD     1             X
063000020404     C     DEPNTWA       LOOKUP    NTWCOD(X)                              55
063100020404     C                   IF        *IN55 = *ON
063200020404     C                   EVAL      DEPORDA = NTWORD(X)
063300020404     C                   ENDIF
063400020314     C                   ENDIF
063500020404     C*
063600020404     C* IN BASE ALL'ORDINE DI CONSIDERAZIONE ATTRIBUISCO IL NETWORK DELLA SPEDIZIONE (ATTUALMENTE SOLO X ESTERO)
063700020404     C*
063800020404     C* 1) Italia su Italia
063900020404     C                   IF        DEPNTWP = *BLANKS AND
064000020404     C                             DEPNTWA = *BLANKS
064100020404     C                   EVAL      DEPNTW = *BLANKS
064200020404     C                   ELSE
064300020404     C* 2) Estero su Estero
064400020404     C                   IF        DEPNTWP <> *BLANKS AND
064500020404     C                             DEPNTWA <> *BLANKS
064600020404     C                   IF        DEPORDP < DEPORDA
064700020404     C                   EVAL      DEPNTW = DEPNTWP
064800020404     C                   ELSE
064900020404     C                   EVAL      DEPNTW = DEPNTWA
065000020404     C                   ENDIF
065100020404     C                   ELSE
065200020404     C* 3) Estero su Italia / Italia su Estero
065300020404     C                   IF        DEPNTWP <> *BLANKS
065400020404     C                   EVAL      DEPNTW = DEPNTWP
065500020404     C                   ELSE
065600020404     C                   EVAL      DEPNTW = DEPNTWA
065700020404     C                   ENDIF
065800020404     C                   ENDIF
065900020404     C                   ENDIF
066000020315     C*
066100020315     C* IMPOSTA SE FLAG ITALIA O ESTERO
066200020404     C     DEPNTWP       IFNE      *BLANKS
066300020404     C     DEPNTWA       ORNE      *BLANKS
066400020315     C                   MOVEL     'E'           DEPIOE                         *ESTERO
066500020315X   1C                   ELSE
066600020315     C                   MOVEL     'I'           DEPIOE                         *ITALIA
066700020315E   1C                   ENDIF
066800000000     C*
066900000000     C* CONTROLLA ESISTENZA RECORD DI LAVORO
067000000000     C                   MOVEL     DEPDIV        KWDDIV                         *DIVISIONE
067100000000     C                   Z-ADD     DEPARE        KWDARE                         *AREA
067200000000     C                   Z-ADD     DEPFIL        KWDFIL                         *FILIALE
067300000000     C                   Z-ADD     DEPAGE        KWDAGE                         *COMMERCIALE
067400000000     C                   MOVEL     DEPCLV        KWDCLV                         *CODICE QUALITA'
067500100127     C                   Z-ADD     UNIcli        KWDCLI                         *CLIENTE
067600000824     C                   Z-ADD     SDCANN        KWDANN                         *ANNO
067700000824     C                   Z-ADD     SDCMES        KWDMES                         *MESE
067800000824     C                   MOVEL     SDCFPE        KWDFPE                         *FASCIA DI PESO
067900970821     C                   MOVEL     DEPIOE        KWDIOE                         *ITALIA O ESTERO
068000020315     C                   MOVEL     DEPNTW        KWDNTW                         *NETWORK
068100100127     C                   MOVEL     SDCTSP        KWDTSP                         *Tipo Servizio
068200070531     c                   if        PARWfdpv = 'S'
068300110401     C     KEYDEL        CHAIN     WAdfpv1L                           99
068400070531     c                   else
068500100127     C     KEYDELtsp     CHAIN     WFDEA01L                           99
068600070531     c                   end
068700000000     C***
068800000000     C* I M M I S S I O N E
068900000000     C***
069000000000IF  1C     *IN99         IFEQ      '1'
069100100127     c                   CLEAR                   WFDEA000
069200000000     C                   MOVEL     DEPDIV        WFDDIV                         *DIVISIONE
069300000000     C                   Z-ADD     DEPARE        WFDARE                         *AREA
069400000000     C                   Z-ADD     DEPFIL        WFDFIL                         *FILIALE
069500000000     C                   Z-ADD     DEPAGE        WFDAGE                         *COMMERCIALE
069600000000     C                   MOVEL     DEPCLV        WFDCLV                         *CODICE QUALITA'
069700100127     C                   Z-ADD     UNIcli        WFDCLI                         *CLIENTE
069800000824     C                   Z-ADD     SDCANN        WFDANN                         *ANNO
069900000824     C                   Z-ADD     SDCMES        WFDMES                         *MESE
070000000824     C                   MOVEL     SDCFPE        WFDFPE                         *FASCIA DI PESO
070100000824     C                   MOVEL     DEPIOE        WFDIOE                         *ITALIA O ESTERO
070200000000     C                   Z-ADD     DEPRGF        WFDRGF                         *RAGGRUPP FIL AGENTE
070300000000     C                   MOVEL     DEPRGD        WFDRGD                         *DESCRIZ RAGGRUP.TO
070400000000     C                   MOVEL     DEPDIC        WFDDIC                         DIVISIONE COMMERCIAL
070500000000     C                   Z-ADD     SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
070600070531     C                   z-add     depPKG        WFDpkg                         *Peso
070700070531     C                   z-add     depVOL        WFDvol                         *Volume
070800000000     C                   Z-ADD     DEPRRN        WFDRRN                         RIC REALE NETTO DELT
070900000000     C                   Z-ADD     SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
071000070720      * --
071100000000     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
071200070720      * --
071300980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
071400020315     C                   MOVEL     DEPNTW        WFDNTW                         *NETWORK
071500100127      * --
071600100127     C                   move      SDCTSP        WFDTSP                         Tipo Servizio
071700100127      * --
071800070531     c                   if        PARWfdpv = 'S'
071900110401     C                   WRITE     Wfdfpv00
072000070531     c                   else
072100100127     C                   WRITE     WFDEA000
072200070531     c                   end
072300000000X   1C                   ELSE
072400000000     C***
072500000000     C* V A R I A Z I O N E
072600000000     C***
072700000000     C     WFDNSP        ADD       SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
072800070531     C     WFDpkg        add       depPKG        WFDpkg                         *Peso
072900070531     C     WFDvol        add       depVOL        WFDvol                         *Volume
073000000000     C     WFDRRN        ADD       DEPRRN        WFDRRN                         RIC REALE NETTO DELT
073100000000     C     WFDRPN        ADD       SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
073200980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
073300070720      * ---
073400070720      * solo x unificante
073500070720      *?  Attenzione: se lanciata x Unificante, occorre determinare se il codice  ?
073600070720      *?    unificante è nuovo o vecchio considerando tutti i suoi figli e,  ?
073700070821      *?    qualora uno di essi sia considerato vecchio, sarà quest'ultimo
073800070821      *?     a dare la caratteristica di vecchio al codice Unificante Padre.
073900070720      *?  >> SU SISDO l'info di Nuovo/Vecchio è sempre per singolo Cliente. <<
074000070723     c                   if        parSCE = 'U'
074100070723     c                   if        wfdCLN = *blank or
074200070723     c                             wfdCLN = 'Y' and DEPCLN = 'S'
074300070720     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
074400070720     c                   end
074500070723     c                   end
074600070720      * ---
074700070531     c                   if        PARWfdpv = 'S'
074800110401     C                   UPDATE    Wfdfpv00
074900070531     c                   else
075000100127     C                   UPDATE    WFDEA000
075100070531     c                   end
075200000000E   1C                   ENDIF
075300000000     C*
075400000000     C                   ENDSR
075500000824     C*------------------------------------------------------------------------*
075600000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
075700000824     C*------------------------------------------------------------------------*
075800000825     C     AGGWFD        BEGSR
075900000825     C*
076000000825     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
076100070531     c                   if        PARWfdpv = 'S'
076200110401     C     *LOVAL        SETLL     WAdfpv4L
076300110401     C                   READ(N)   WAdfpv4L                               99
076400070531     c                   else
076500100127     C     *LOVAL        SETLL     WFDEa04L
076600100127     C                   READ(N)   WFDEa04L                               99
076700070531     c                   end
076800000825     C*
076900000825     C* CICLO FINO A FINE FILE
077000000825DO  1C     *IN99         DOWEQ     *OFF
077100000825     C*
077200000825     C* OPERAZIONE PER NUOVO CLIENTE
077300000825     C                   EXSR      NEWCLI
077400000825     C*
077500000825     C* CICLO FINO A ROTTURA CLIENTE
077600000825DO  2C     *IN99         DOWEQ     *OFF
077700100127     C     UNIcli        ANDEQ     WFDCLI
077800070723      *
077900070723      *
078000000825IF  3C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
078100000825     C     WFDDAT        ANDLE     PARDA2
078200100127     C* MEMORIZZA I DATI -WFDEA-
078300000825     C                   EXSR      MEMWFD
078400000825E   3C                   ENDIF
078500000825     C*
078600000825     C* LETTURA SUCCESSIVA DATI CLIENTE
078700070531     c                   if        PARWfdpv = 'S'
078800110401     C                   READ(N)   WAdfpv4L                               99
078900070531     c                   else
079000100127     C                   READ(N)   WFDEa04L                               99
079100070531     c                   end
079200070723     C*
079300000825E   2C                   ENDDO                                                  *ROTTURA CLIENTE
079400000825     C*
079500000825     C* CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
079600000825     C                   EXSR      MODCLI
079700000825     C*
079800000825     C* SI RIPOSIZIONA E LEGGE I DATI DEL CLIENT SUCCESSIVO
079900070531     c                   if        PARWfdpv = 'S'
080000110401     C     UNIcli        SETGT     WAdfpv4L
080100110401     C                   READ(N)   WAdfpv4L                               99
080200070531     c                   else
080300100127     C     UNIcli        SETGT     WFDEa04L
080400100127     C                   READ(N)   WFDEa04L                               99
080500070531     c                   end
080600000825E   1C                   ENDDO
080700000824     C*
080800000824     C                   ENDSR
080900000825     C*------------------------------------------------------------------------*
081000100127     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE -WFDEA-
081100000825     C*------------------------------------------------------------------------*
081200000825     C     NEWCLI        BEGSR
081300000825     C*
081400000825     C* MEMORIZZA IL NUOVO CLIENTE IN UN DEPOSITO
081500100127     C                   Z-ADD     WFDCLI        UNIcli
081600000825     C*
081700000825     C* AZZERA LE VARIABILI DI LAVORO
081800020116     C                   Z-ADD     *ZEROS        TOTIRD           19 5          *RICAVI
081900020116     C                   Z-ADD     *ZEROS        TOTIRP           19 5
082000020116     C                   Z-ADD     *ZEROS        TOTERD           19 5
082100020116     C                   Z-ADD     *ZEROS        TOTERP           19 5
082200020116     C                   Z-ADD     *ZEROS        TOTTRD           19 5
082300020116     C                   Z-ADD     *ZEROS        TOTTRP           19 5
082400000825     C                   Z-ADD     *ZEROS        PERDEI            4 1          *% DELTA
082500000825     C                   Z-ADD     *ZEROS        PERDEE            4 1
082600000825     C                   Z-ADD     *ZEROS        PERDET            4 1
082700000825     C                   MOVEL     'S'           $OKDEI            1            *DELTA OK
082800000825     C                   MOVEL     'S'           $OKDEE            1
082900000825     C                   MOVEL     'S'           $OKDET            1
083000000825     C                   CLEAR                   SMEI                           *MM CON % SEGNALARE
083100000825     C                   CLEAR                   SMEE
083200000825     C                   CLEAR                   SMET
083300000825     C                   Z-ADD     *ZEROS        MI                2 0          *INDICE MESI
083400000825     C                   Z-ADD     *ZEROS        ME                2 0
083500000825     C                   Z-ADD     *ZEROS        MT                2 0
083600000825     C*
083700000825     C                   ENDSR
083800000825     C*------------------------------------------------------------------------*
083900000825     C* MEMWFD - MEMORIZZA I DATI DEL WFILE
084000000825     C*------------------------------------------------------------------------*
084100000825     C     MEMWFD        BEGSR
084200000825     C
084300000825     C* MEMORIZZA I RICAVI ITALIA E ESTERO DEL CLIENTE PER IL PERIODO RICHIESTO
084400000825IF  1C     WFDIOE        IFEQ      'I'                                          *ITALIA
084500000825     C                   ADD       WFDRRN        TOTIRD                         *TOT RIC REALE
084600000825     C                   ADD       WFDRPN        TOTIRP                         *TOT RIC PRESUNTO
084700000825E   1C                   ENDIF
084800000825IF  1C     WFDIOE        IFEQ      'E'                                          *ESTERO
084900000825     C                   ADD       WFDRRN        TOTERD                         *TOT RIC REALE
085000000825     C                   ADD       WFDRPN        TOTERP                         *TOT RIC PRESUNTO
085100000825E   1C                   ENDIF
085200000825IF  1C     WFDIOE        IFEQ      *BLANKS                                      *ITALIA + ESTERO
085300000825     C                   ADD       WFDRRN        TOTTRD                         *TOT RIC REALE
085400000825     C                   ADD       WFDRPN        TOTTRP                         *TOT RIC PRESUNTO
085500000825E   1C                   ENDIF
085600000825     C*
085700000825     C* CONTROLLA SE LA % DELTA DI CIASCUNA FASCIA DI PESO RIENTRA IN QUELLA RICHIESTA
085800000825IF  1C     PARSPD        IFNE      *ZEROS                                       *RICHIESTA
085900000825     C*
086000000825     C* ITALIA
086100000825IF  2C     WFDIOE        IFEQ      'I'
086200000825IF  3C     WFDRRN        IFGT      *ZEROS
086300000825     C     WFDRPN        ORGT      *ZEROS
086400020116     C                   Z-ADD     WFDRRN        WRRW             17 5          *RICAVO REALE
086500020116     C                   Z-ADD     WFDRPN        WRPW             17 5          *RICAVO PRESUNTO
086600000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
086700000825     C                   EXSR      CALDEL
086800000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
086900000825     C                   SETOFF                                           97
087000000825     C     WFDMES        LOOKUP    SMEI                                   97
087100000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
087200000825     C                   ADD       1             MI
087300000825     C                   Z-ADD     WFDMES        SMEI(MI)                       *MESE DA SEGNALARE
087400000825E   5C                   ENDIF
087500000825E   4C                   ENDIF
087600000825E   3C                   ENDIF
087700000825E   2C                   ENDIF
087800000825     C* ESTERO
087900000825IF  2C     WFDIOE        IFEQ      'E'
088000000825IF  3C     WFDRRN        IFGT      *ZEROS
088100000825     C     WFDRPN        ORGT      *ZEROS
088200020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
088300020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
088400000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
088500000825     C                   EXSR      CALDEL
088600000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
088700000825     C                   SETOFF                                           97
088800000825     C     WFDMES        LOOKUP    SMEE                                   97
088900000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
089000000825     C                   ADD       1             ME
089100000825     C                   Z-ADD     WFDMES        SMEE(ME)                       *MESE DA SEGNALARE
089200000825E   5C                   ENDIF
089300000825E   4C                   ENDIF
089400000825E   3C                   ENDIF
089500000825E   2C                   ENDIF
089600000825     C* ITALIA + ESTERO
089700000825IF  2C     WFDIOE        IFEQ      *BLANKS
089800000825IF  3C     WFDRRN        IFGT      *ZEROS
089900000825     C     WFDRPN        ORGT      *ZEROS
090000020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
090100020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
090200000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
090300000825     C                   EXSR      CALDEL
090400000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
090500000825     C                   SETOFF                                           97
090600000825     C     WFDMES        LOOKUP    SMET                                   97
090700000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
090800000825     C                   ADD       1             MT
090900000825     C                   Z-ADD     WFDMES        SMET(MT)                       *MESE DA SEGNALARE
091000000825E   5C                   ENDIF
091100000825E   4C                   ENDIF
091200000825E   3C                   ENDIF
091300000825E   2C                   ENDIF
091400000825     C*
091500000825E   1C                   ENDIF
091600000825     C*
091700000825     C                   ENDSR
091800000825     C*------------------------------------------------------------------------*
091900000825     C* MODCLI - CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
092000000825     C*------------------------------------------------------------------------*
092100000825     C     MODCLI        BEGSR
092200000825     C*
092300000825     C* CALCOLA IL DELTA DEL CLIENTE
092400000825     C                   EXSR      CALCLI
092500000825     C*
092600000825     C* RILEGGE I DATI DEL CLIENTE -->
092700000825     C* - CANCELLA IL CLIENTE SE NON RIENTRA NEL RANGE DELTA RICHIESTO
092800000825     C* - AGGIORNA IL CLIENTE SE HA UNA % DELTA FASCE DA SEGNALARE
092900070531     c                   if        PARWfdpv = 'S'
093000110401     C     UNIcli        SETLL     WAdfpv4L
093100110401     C     UNIcli        READE     WAdfpv4L                               98
093200070531     c                   else
093300100127     C     UNIcli        SETLL     WFDEa04L
093400100127     C     UNIcli        READE     WFDEa04L                               98
093500070531     c                   end
093600070531      *
093700000825DO  1C     *IN98         DOWEQ     *OFF
093800000825     C*
093900000825     C* ITALIA
094000000825IF  2C     WFDIOE        IFEQ      'I'
094100000825IF  3C     $OKDEI        IFEQ      'N'                                          *DELTA NON OK
094200070531     c                   if        PARWfdpv = 'S'
094300100203     C                   DELETE    WFDfpv
094400070531     c                   else
094500100127     C                   DELETE    WFDEa4
094600070531     c                   end
094700000825X   3C                   ELSE                                                   *DELTA OK
094800000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
094900000828     C     WFDDAT        ANDLE     PARDA2
095000000825     C                   SETOFF                                           97
095100000825     C     WFDMES        LOOKUP    SMEI                                   97
095200000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
095300000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
095400070531     c                   if        PARWfdpv = 'S'
095500100203     C                   UPDATE    WFDfpv
095600070531     c                   else
095700100127     C                   UPDATE    WFDEa4
095800070531     c                   end
095900000828E   5C                   ENDIF
096000000828E   4C                   ENDIF
096100000825E   3C                   ENDIF
096200000825E   2C                   ENDIF
096300000825     C*
096400000825     C* ESTERO
096500000825IF  2C     WFDIOE        IFEQ      'E'
096600000825IF  3C     $OKDEE        IFEQ      'N'                                          *DELTA NON OK
096700070531     c                   if        PARWfdpv = 'S'
096800100203     C                   DELETE    WFDfpv
096900070531     c                   else
097000100127     C                   DELETE    WFDEa4
097100070531     c                   end
097200000825X   3C                   ELSE                                                   *DELTA OK
097300000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
097400000828     C     WFDDAT        ANDLE     PARDA2
097500000825     C                   SETOFF                                           97
097600000825     C     WFDMES        LOOKUP    SMEE                                   97
097700000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
097800000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
097900070531     c                   if        PARWfdpv = 'S'
098000100203     C                   UPDATE    WFDfpv
098100070531     c                   else
098200100127     C                   UPDATE    WFDEa4
098300070531     c                   end
098400000828E   5C                   ENDIF
098500000825E   4C                   ENDIF
098600000825E   3C                   ENDIF
098700000825E   2C                   ENDIF
098800000825     C*
098900000825     C* ITALIA + ESTERO
099000000825IF  2C     WFDIOE        IFEQ      *BLANKS
099100000825IF  3C     $OKDET        IFEQ      'N'                                          *DELTA NON OK
099200070531     c                   if        PARWfdpv = 'S'
099300100203     C                   DELETE    WFDfpv
099400070531     c                   else
099500100127     C                   DELETE    WFDEa4
099600070531     c                   end
099700000825X   3C                   ELSE                                                   *DELTA OK
099800000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
099900000828     C     WFDDAT        ANDLE     PARDA2
100000000825     C                   SETOFF                                           97
100100000825     C     WFDMES        LOOKUP    SMET                                   97
100200000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
100300000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
100400070531     c                   if        PARWfdpv = 'S'
100500100203     C                   UPDATE    WFDfpv
100600070531     c                   else
100700100127     C                   UPDATE    WFDEa4
100800070531     c                   end
100900000828E   5C                   ENDIF
101000000825E   4C                   ENDIF
101100000825E   3C                   ENDIF
101200000825E   2C                   ENDIF
101300000825     C*
101400070531     c                   if        PARWfdpv = 'S'
101500110401     C     UNIcli        READE     WAdfpv4L                               98
101600070531     c                   else
101700100127     C     UNIcli        READE     WFDEa04L                               98
101800070531     c                   end
101900000825E   1C                   ENDDO                                                  *DINE DATI CLIENTE
102000000825     C*
102100000825     C                   ENDSR
102200000825     C*------------------------------------------------------------------------*
102300000825     C* CALCOLA I DELTA SUI DATI MEMORIZZATI
102400000825     C*------------------------------------------------------------------------*
102500000825     C     CALCLI        BEGSR
102600000825     C*
102700000825     C* CALCOLA I DELTA E CONTROLLA SE RIENTRANO NEL RANGE RICHIESTO
102800000825IF  1C     PARPDD        IFNE      -999,9                                       *RICHIESTO
102900000825     C     PARPDA        ORNE      +999,9
103000000825     C*
103100000825     C* DELTA ITALIA
103200000825IF  2C     TOTIRD        IFGT      *ZEROS
103300000825     C     TOTIRP        ORGT      *ZEROS
103400020116     C                   Z-ADD     TOTIRD        WRRW                           *RICAVO REALE
103500020116     C                   Z-ADD     TOTIRP        WRPW                           *RICAVO PRESUNTO
103600000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
103700000825     C                   EXSR      CALDEL
103800000825     C                   Z-ADD     WDPW          PERDEI                         *% DELTA ITALIA
103900000825IF  3C     PERDEI        IFLT      PARPDD                                       *DELTA <> RANGE
104000000825     C     PERDEI        ORGT      PARPDA
104100000825     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
104200000825E   3C                   ENDIF
104300010313X   2C                   ELSE                                                   *NO RICAVI ITALIA
104400010313     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
104500000825E   2C                   ENDIF
104600000825     C* DELTA ESTERO
104700000825IF  2C     TOTERD        IFGT      *ZEROS
104800000825     C     TOTERP        ORGT      *ZEROS
104900020116     C                   Z-ADD     TOTERD        WRRW                           *RICAVO REALE
105000020116     C                   Z-ADD     TOTERP        WRPW                           *RICAVO PRESUNTO
105100000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
105200000825     C                   EXSR      CALDEL
105300000825     C                   Z-ADD     WDPW          PERDEE                         *% DELTA ESTERO
105400000825IF  3C     PERDEE        IFLT      PARPDD                                       *DELTA <> RANGE
105500000825     C     PERDEE        ORGT      PARPDA
105600000825     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
105700000825E   3C                   ENDIF
105800010313X   2C                   ELSE                                                   *NO RICAVI ESTERO
105900010313     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
106000000825E   2C                   ENDIF
106100000825     C* DELTA ITA + EST
106200000825IF  2C     TOTTRD        IFGT      *ZEROS
106300000825     C     TOTTRP        ORGT      *ZEROS
106400020116     C                   Z-ADD     TOTTRD        WRRW                           *RICAVO REALE
106500020116     C                   Z-ADD     TOTTRP        WRPW                           *RICAVO PRESUNTO
106600000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
106700000825     C                   EXSR      CALDEL
106800000825     C                   Z-ADD     WDPW          PERDET                         *% DELTA ESTERO
106900000825IF  3C     PERDET        IFLT      PARPDD                                       *DELTA <> RANGE
107000000825     C     PERDET        ORGT      PARPDA
107100000825     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
107200000825E   3C                   ENDIF
107300010313X   2C                   ELSE                                                   *NO RICAVI TOTALI
107400010313     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
107500000825E   2C                   ENDIF
107600000825     C*
107700000825E   1C                   ENDIF
107800000825     C*
107900000825     C                   ENDSR
108000000824     C*------------------------------------------------------------------------*
108100000824     C* CALDEL - CALCOLA IL DELTA
108200000824     C*          INPUT  --> WRRW: RICAVO REALE
108300970821     C*                     WRPW: RICAVO PRESUNTO
108400970821     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
108500970821     C*------------------------------------------------------------------------*
108600970821     C     CALDEL        BEGSR
108700970821     C*--------------------
108800970821     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
108900970821     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
109000970821     C*--------------------
109100970821     C*
109200970821     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
109300020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
109400970821     C     GAP           MULT      100           GAP                            (GAP x 100)
109500970821     C*
109600970821     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
109700970821IF  1C     WRRW          IFEQ      *ZEROS
109800000825     C                   Z-ADD     1             WRRW
109900970821E   1C                   ENDIF
110000970821     C*
110100970821     C     GAP           DIV       WRRW          GAP
110200000825     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
110300970821     C*
110400970821     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
110500970821     C     WDPW          COMP      0                                    28
110600970821     C* POSITIVO
110700970821IF  1C     *IN28         IFEQ      *OFF
110800970821IF  2C     GAP           IFGE      1000
110900970821     C                   Z-ADD     +999,9        WDPW
111000970821E   2C                   ENDIF
111100970821     C* NEGATIVO
111200970821X   1C                   ELSE
111300970821IF  2C     GAP           IFLE      -1000
111400970821     C                   Z-ADD     -999,9        WDPW
111500970821E   2C                   ENDIF
111600970821E   1C                   ENDIF
111700970821     C*
111800970821     C                   ENDSR
111900000000     C*------------------------------------------------------------------------*
112000000000     C* STAERR - STAMPA RIGA DI ERRORE PER AGENTE INESISTENTE O ANNULLATO
112100000000     C*------------------------------------------------------------------------*
112200000000     C     STAERR        BEGSR
112300000000     C*
112400000000IF  1C     *INOF         IFEQ      *ON
112500000000     C                   EXCEPT    TESTA
112600000000     C                   SETOFF                                       OF
112700000000E   1C                   ENDIF
112800000000     C*
112900000000     C* AGENTE NON ANCORA STAMPATO, SCRIVO ERRORE
113000000000     C                   SETOFF                                       97
113100000000     C     DEPAGE        LOOKUP    ERG                                    97
113200000000IF  1C     *IN97         IFEQ      *OFF
113300000000     C                   ADD       1             I                 3 0
113400000000     C                   Z-ADD     DEPAGE        ERG(I)                         *MEMORIZZA AGENTE
113500000000     C                   EXCEPT    ERROR                                        *STAMPA L'ERRORE
113600000000E   1C                   ENDIF
113700000000     C*
113800000000     C                   ENDSR
113900000000     C*------------------------------------------------------------------------*
114000000000     C* CARTAB - CARICA I DATI IN TABELLE
114100000000     C*------------------------------------------------------------------------*
114200000000     C     CARTAB        BEGSR
114300000000     C*--------------------
114400000000     C* ORGANIGRAMMA
114500000000     C*--------------------
114600000000     C                   Z-ADD     *ZEROS        F
114700000000     C                   Z-ADD     *ZEROS        KORFIL
114800000000     C     KEYORG        SETLL     AZORG01L
114900000000     C                   READ      AZORG01L                               99
115000000000DO  1C     *IN99         DOWEQ     '0'
115100980119IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
115200980119IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
115300000000     C                   ADD       1             F
115400000000     C                   Z-ADD     ORGFIL        FIL(F)
115500000000     C                   MOVEL     ORGDES        DFIL(F)
115600020906     C                   Z-ADD     1             KTBKUT
115700000000     C                   MOVEL     '05'          KTBCOD
115800000000     C                   MOVEL     *BLANKS       KTBKEY
115900000000     C                   MOVEL     ORGCAR        KTBKEY
116000000824     C     KEYTAB        CHAIN     TABEL00F                           99
116100980119IF  5C     *IN99         IFEQ      *OFF
116200000000     C                   Z-ADD     ORGCAR        ARE(F)
116300000000     C                   MOVEL     TBLUNI        DARE(F)
116400980119E   5C                   ENDIF
116500020906     C                   Z-ADD     1             KTBKUT
116600000000     C                   MOVEL     '17'          KTBCOD
116700000000     C                   MOVEL     *BLANKS       KTBKEY
116800000000     C                   MOVEL     ORGFL3        KTBKEY
116900000824     C     KEYTAB        CHAIN     TABEL00F                           99
117000980119IF  5C     *IN99         IFEQ      *OFF
117100000000     C                   MOVEL     ORGFL3        DIV(F)
117200000000     C                   MOVEL     TBLUNI        DDIV(F)
117300980119E   5C                   ENDIF
117400980119E   3C                   ENDIF
117500980119E   2C                   ENDIF
117600000000     C                   READ      AZORG01L                               99
117700000000E   1C                   ENDDO
117800000000     C*--------------------
117900000000     C* COMMERCIALI
118000000000     C*--------------------
118100140612     C********           Z-ADD     *ZEROS        C
118200140612     C********           Z-ADD     1             KTBKUT
118300140612     C********           MOVEL     '01'          KTBCOD
118400140612     C***  KEYTA2        SETLL     TABEL00F
118500140612     C***  KEYTA2        READE     TABEL00F                               99
118600140612DO  1C***  *IN99         DOWEQ     *OFF
118700140612IF  2C***  TBLFLG        IFEQ      *BLANKS
118800140612     C********           ADD       1             C
118900140612     C********           MOVEL     TBLUNI        DS01
119000140612     C********           MOVEL     TBLKEY        AGE(C)
119100140612     C********           MOVEL     §01RGF        RGF(C)
119200140612     C********           MOVEL     §01AGE        DAGE(C)
119300140612E   2C********           ENDIF
119400140612     C***  KEYTA2        READE     TABEL00F                               99
119500140612E   1C********           ENDDO
119600140612      *********
119700140612     C                   Z-ADD     *ZEROS        C
119800140612     c                   open      azCMM01l
119900140612     C     *loval        SETLL     azCMM01l
120000140612     C                   READ      azCMM01l
120100140612DO  1C                   DOW       not %Eof(azCMM01l)
120200140612IF  2C                   IF        cmmATB = *blank
120300140612     C                   ADD       1             C
120400140612     C                   z-add     cmmCOD        AGE(C)
120500140612     C                   z-add     cmmUNI        RGF(C)
120600140612     C                   MOVEL     cmmDES        DAGE(C)
120700140612E   2C                   ENDIF
120800140612     C                   READ      azCMM01l
120900140612E   1C                   ENDDO
121000140612     c                   close     azCMM01l
121100000000     C*-------------------------
121200000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
121300000000     C*-------------------------
121400000000     C                   Z-ADD     *ZEROS        G
121500020906     C                   Z-ADD     1             KTBKUT
121600000000     C                   MOVEL     '2W'          KTBCOD
121700000000     C     KEYTA2        SETLL     TABEL00F
121800000824     C     KEYTA2        READE     TABEL00F                               99
121900000000DO  1C     *IN99         DOWEQ     *OFF
122000000000IF  2C     TBLFLG        IFEQ      *BLANKS
122100000000     C                   ADD       1             G
122200000000     C                   MOVEL     TBLKEY        FLAB(G)
122300000000     C                   MOVEL     TBLUNI        DS2W
122400000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
122500000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
122600000000E   2C                   ENDIF
122700000824     C     KEYTA2        READE     TABEL00F                               99
122800000000E   1C                   ENDDO
122900990217     C*--------------------
123000000000     C* FASCE DI PESO
123100990217     C*--------------------
123200990609IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
123300990217     C                   MOVEL     'SDC'         WSX               3
123400990217X   1C                   ELSE                                                   *FASCE DAL '99
123500020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO AL 2001
123600020325     C                   MOVEL     'S9C'         WSX
123700020325X   2C                   ELSE                                                   *FASCE DAL 2002
123800020325     C                   MOVEL     'S2C'         WSX
123900020325E   2C                   ENDIF
124000020325E   1C                   ENDIF
124100970821     C                   Z-ADD     *ZEROS        P
124200020906     C                   Z-ADD     1             KTBKUT
124300000000     C                   MOVEL     '2L'          KTBCOD
124400000000     C     KEYTA2        SETLL     TABEL00F
124500000824     C     KEYTA2        READE     TABEL00F                               99
124600000000DO  1C     *IN99         DOWEQ     *OFF
124700000000IF  2C     TBLFLG        IFEQ      *BLANKS
124800990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
124900000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
125000990217IF  3C     FILEST        IFEQ      WSX
125100000000     C                   MOVEL     TBLUNI        DS2L
125200000309IF  4C     §2LKGI        IFGE      5000
125300000309     C     PARDAT        ANDLT     200001
125400000309     C                   ELSE
125500970821     C                   ADD       1             P
125600970821     C                   MOVE      STACOD        FPE(P)                         *...+XX
125700000309E   4C                   ENDIF
125800000000E   3C                   ENDIF
125900000000E   2C                   ENDIF
126000000824     C     KEYTA2        READE     TABEL00F                               99
126100000000E   1C                   ENDDO
126200020314     C*-------------------------
126300020314     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
126400020314     C*-------------------------
126500020314     C                   Z-ADD     *ZEROS        R
126600020906     C                   Z-ADD     1             KTBKUT
126700020314     C                   MOVEL     '2M'          KTBCOD
126800020314     C     KEYTA2        SETLL     TABEL00F
126900020314     C     KEYTA2        READE     TABEL00F                               99
127000020314DO  1C     *IN99         DOWEQ     *OFF
127100020314IF  2C     TBLFLG        IFEQ      *BLANKS
127200020314     C                   ADD       1             R
127300020314     C                   MOVEL     TBLKEY        RGREG(R)
127400020314     C                   MOVEL     TBLUNI        DS2M
127500020314     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
127600020314E   2C                   ENDIF
127700020314     C     KEYTA2        READE     TABEL00F                               99
127800020314E   1C                   ENDDO
127900020404     C*----------
128000020404     C* NETWORK AZIENDALI
128100020404     C*----------
128200020404     C                   Z-ADD     *ZEROS        X
128300020404     C                   EVAL      TBECOD = 'NTW'
128400020404     C     KEYTBE        SETLL     TNTBE01L
128500020404     C     KEYTBE        READE     TNTBE01L
128600020404     C                   DOW       NOT %EOF(TNTBE01L)
128700020404     C                   ADD       1             X
128800020404     C                   MOVEL     TBEUNI        DNTW
128900020404     C                   EVAL      NTWCOD(X) = TBEKE1
129000020404     C                   EVAL      NTWORD(X) = §NTWORD
129100020404     C     KEYTBE        READE     TNTBE01L
129200020404     C                   ENDDO
129300000000     C*
129400000000     C                   ENDSR
129500970821     C*------------------------------------------------------------------------*
129600970821     C* AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
129700970821     C*------------------------------------------------------------------------*
129800970821     C     AMELAB        BEGSR
129900970821     C*---
130000970821     C* ANNO INIZIALE UGUALE AD ANNO FINALE
130100970821     C*---
130200970821IF  1C     WAA2          IFEQ      WAA
130300970821     C*
130400970821     C* CALCOLO I MESI DI DIFFERENZA
130500970821DO  2C     WMM           DOWLE     WMM2
130600970821     C                   ADD       1             JI
130700970821     C                   Z-ADD     WAA           ANN(JI)
130800970821     C                   Z-ADD     WMM           MES(JI)
130900970821     C                   ADD       1             WMM
131000970821E   2C                   ENDDO
131100970821     C*
131200970821     C* IL RESTO DELLA SCHIERA A ZERO
131300970821DO  2C     JI            DOWLT     12
131400970821     C                   ADD       1             JI
131500970821     C                   Z-ADD     *ZEROS        ANN(JI)
131600970821     C                   Z-ADD     *ZEROS        MES(JI)
131700970821E   2C                   ENDDO
131800970821     C*---
131900970821     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
132000970821     C*---
132100970821X   1C                   ELSE
132200970821     C*
132300970821     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
132400970821DO  2C     WMM           DOWLE     12
132500970821     C                   ADD       1             JI
132600970821     C                   Z-ADD     WAA           ANN(JI)
132700970821     C                   Z-ADD     WMM           MES(JI)
132800970821     C                   ADD       1             WMM
132900970821E   2C                   ENDDO
133000970821     C*
133100970821     C* MEMORIZZO I MESI DELL'ANNO FINALE
133200970821     C                   Z-ADD     01            WMM
133300970821DO  2C     WMM           DOWLE     WMM2
133400970821     C                   ADD       1             JI
133500970821     C                   Z-ADD     WAA2          ANN(JI)
133600970821     C                   Z-ADD     WMM           MES(JI)
133700970821     C                   ADD       1             WMM
133800970821E   2C                   ENDDO
133900970821     C*
134000970821     C* IL RESTO DELLA SCHIERA A ZERO
134100970821DO  2C     JI            DOWLT     12
134200970821     C                   ADD       1             JI
134300970821     C                   Z-ADD     *ZEROS        ANN(JI)
134400970821     C                   Z-ADD     *ZEROS        MES(JI)
134500970821E   2C                   ENDDO
134600970821E   1C                   ENDIF
134700970821     C*
134800970821     C                   ENDSR
134900980403     C*------------------------------------------------------------------------*
135000980403     C* FINSR - OPERAZIONI FINALI
135100980403     C*------------------------------------------------------------------------*
135200980403     C     FINSR         BEGSR
135300980403     C*
135400980403     C* CHIUDE I FILE DEI PGM CHIAMATI
135500980403     C                   CLEAR                   BS69DS
135600980403     C                   CLEAR                   ACODS
135700980403     C                   CLEAR                   INDDS
135800980403     C                   CLEAR                   CLPDS
135900980403     C                   CLEAR                   CLSDS
136000980403     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
136100980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
136200980403     C                   CALL      'TIBS69R'
136300980403     C                   PARM                    BS69DS
136400980403     C                   PARM                    ACODS
136500980403     C                   PARM                    INDDS
136600980403     C                   PARM                    CLPDS
136700980403     C                   PARM                    CLSDS
136800980403     C*
136900980403     C                   ENDSR
137000000000     C*------------------------------------------------------------------------*
137100000000     C* *INZSR - IMPOSTAZIONI INIZIALI
137200000000     C*------------------------------------------------------------------------*
137300000000     C     *INZSR        BEGSR
137400000000     C*
137500000000     C* PARAMETRI IN INPUT AL PGM
137600000000     C     *ENTRY        PLIST
137700000000     C                   PARM                    KPJBA
137800000000     C                   MOVEL     KPJBU         PARAM
137900070531     c*
138000070531     c* apertura condizionata dei 2 files da generare
138100070531     c                   if        PARWfdpv = 'S'
138200110401     c                   open      WAdfpv1l
138300110401     c                   open      WAdfpv4l
138400110401     c                   open      WAdfpv5l
138500070531     c                   else
138600100127     c                   open      WFdea01l
138700100127     c                   open      WFdea04l
138800100127     c                   open      WFdea05l
138900100127     c                   open      WFdeav1l
139000070531     c                   end
139100000000     C*
139200000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
139300000823     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
139400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
139500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
139600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
139700990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC01L
139800000000     C     *LIKE         DEFINE    SDCANN        KSCANN
139900000000     C     *LIKE         DEFINE    SDCMES        KSCMES
140000990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO01L
140100000000     C     *LIKE         DEFINE    SDOANN        KSOANN
140200000000     C     *LIKE         DEFINE    SDOMES        KSOMES
140300100127     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEa01L
140400000000     C     *LIKE         DEFINE    WFDARE        KWDARE
140500000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
140600000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
140700000000     C     *LIKE         DEFINE    WFDCLV        KWDCLV
140800000823     C     *LIKE         DEFINE    WFDCLI        KWDCLI
140900000000     C     *LIKE         DEFINE    WFDANN        KWDANN
141000000000     C     *LIKE         DEFINE    WFDMES        KWDMES
141100000824     C     *LIKE         DEFINE    WFDFPE        KWDFPE
141200000000     C     *LIKE         DEFINE    WFDIOE        KWDIOE
141300020315     C     *LIKE         DEFINE    WFDNTW        KWDNTW
141400100127     C     *LIKE         DEFINE    WFDTSP        KWDTSP
141500000000     C*
141600000000     C* DEFINIZIONI DEPOSITI E VARIABILI
141700000000     C     *LIKE         DEFINE    CLPKSC        DEPKSC
141800100127     C     *LIKE         DEFINE    CLPKSC        UNIcli
141900000000     C     *LIKE         DEFINE    CLPAGE        DEPAGE
142000000000     C     *LIKE         DEFINE    ORGFIL        DEPFIL
142100000000     C     *LIKE         DEFINE    ORGCAR        DEPARE
142200000000     C     *LIKE         DEFINE    ORGFL3        DEPDIV
142300000000     C     *LIKE         DEFINE    ORGFL3        DEPDIC
142400000000     C     *LIKE         DEFINE    WFDRRN        DEPRRN
142500000000     C     *LIKE         DEFINE    CLPAGE        DEPRGF
142600000000     C     *LIKE         DEFINE    CLPCLV        DEPCLV
142700140612     C**** *LIKE         DEFINE    §01AGE        DEPRGD
142800140612     C     *LIKE         DEFINE    CMMDES        DEPRGD
142900000000     C     *LIKE         DEFINE    SDOCLN        DEPCLN
143000970821     C     *LIKE         DEFINE    WFDIOE        DEPIOE
143100020315     C     *LIKE         DEFINE    WFDNTW        DEPNTW
143200070531     C     *LIKE         DEFINE    WFDRRN        DEPRRN
143300100203     C* x il WFdfpv0F
143400070531     C     *LIKE         DEFINE    WFDpkg        DEPpkg
143500070531     C     *LIKE         DEFINE    WFDvol        DEPvol
143600070723     C     *LIKE         DEFINE    wfdANN        depANN
143700070723     C     *LIKE         DEFINE    wfdMES        depMES
143800000825     C*
143900000825     C* DEFINIZIONI VARIABILI DI LAVORO
144000000825     C     *LIKE         DEFINE    PARSPD        MENSPD
144100000000     C                   MOVEL     '0'           $FINE             1            *FINE PROGRAMMA
144200980601     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
144300980601     C                   MOVEL     '0'           PCLNEW            1            *CLIENTE NUOVO PRECE
144400000000     C                   MOVEL     '0'           CCLNEW            1            *CLIENTE NUOVO CORRE
144500000000     C                   Z-ADD     *ZEROS        C                 4 0          *PER COMMERCIALI
144600000000     C                   Z-ADD     *ZEROS        G                 4 0          *PER FLAG TIPI BOLLA
144700000000     C                   Z-ADD     *ZEROS        J                 4 0          *PER MESI ELABORABIL
144800000000     C                   Z-ADD     *ZEROS        F                 4 0          *PER ORGANIGRAMMA
144900970821     C                   Z-ADD     *ZEROS        P                 4 0          *PER FASCE DI PESO
145000970821     C                   Z-ADD     *ZEROS        A                 4 0          *PER ANNI
145100970821     C                   Z-ADD     *ZEROS        M                 4 0          *PER MESI
145200020404     C                   Z-ADD     *ZEROS        X                 3 0          *PER RAGGRUPPAMENTI REGIONI/NETWORK
145300020404     C                   Z-ADD     *ZEROS        R                 3 0          *PER NETWORK
145400000000     C                   SETON                                        OF        *OVERFLOW
145500000000     C*
145600000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
145700000000     C     KEYORG        KLIST
145800000000     C                   KFLD                    KORFIL                         *FILIALE
145900000000     C*
146000000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
146100000000     C     KEYTAB        KLIST
146200000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
146300000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
146400000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
146500000000     C*
146600000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
146700000000     C     KEYTA2        KLIST
146800000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
146900000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
147000020404     C*
147100020404     C* CHIAVE LETTURA TNTBE00F - PARZIALE
147200020404     C     KEYTBE        KLIST
147300020404     C                   KFLD                    TBECOD
147400000000     C*
147500990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
147600000000     C     KEYSDC        KLIST
147700000000     C                   KFLD                    KSCKSC                         *CODICE CLIENTE
147800000000     C                   KFLD                    KSCANN                         *ANNO
147900000000     C                   KFLD                    KSCMES                         *MESE
148000000000     C*
148100990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
148200000000     C     KEYSDO        KLIST
148300000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
148400000000     C*
148500990609     C* CHIAVE LETTURA SISDO01L - COMPLETA
148600000000     C     KE2SDO        KLIST
148700000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
148800000000     C                   KFLD                    KSOANN                         *ANNO
148900000000     C                   KFLD                    KSOMES                         *MESE
149000100127     C*
149100100127     C* CHIAVE LETTURA WFdeaV1L - COMPLETA
149200100127     C     KEYdeaV       KLIST
149300100215     C                   KFLD                    WFVSVR                         *VARIA
149400100127     C                   KFLD                    WFVKSC                         *CODICE CLIENTE
149500100127     C                   KFLD                    WFVANN                         *ANNO
149600100127     C                   KFLD                    WFVMES                         *MESE
149700100127     C*
149800100127     C* CHIAVE LETTURA WFDEa01L - COMPLETA
149900000000     C     KEYDEL        KLIST
150000000000     C                   KFLD                    KWDDIV                         *DIVISIONE
150100000000     C                   KFLD                    KWDARE                         *AREA
150200000000     C                   KFLD                    KWDFIL                         *FILIALE
150300000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
150400000000     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
150500000823     C                   KFLD                    KWDCLI                         *CLIENTE
150600000000     C                   KFLD                    KWDANN                         *ANNO
150700000000     C                   KFLD                    KWDMES                         *MESE
150800000824     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
150900000000     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
151000020614     C                   KFLD                    KWDNTW                         *NETWORK
151100000823     C*
151200100127     C     KEYDELtsp     KLIST
151300100127     C                   KFLD                    KWDDIV                         *DIVISIONE
151400100127     C                   KFLD                    KWDARE                         *AREA
151500100127     C                   KFLD                    KWDFIL                         *FILIALE
151600100127     C                   KFLD                    KWDAGE                         *COMMERCIALE
151700100127     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
151800100127     C                   KFLD                    KWDCLI                         *CLIENTE
151900100127     C                   KFLD                    KWDANN                         *ANNO
152000100127     C                   KFLD                    KWDMES                         *MESE
152100100127     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
152200100127     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
152300100127     C                   KFLD                    KWDNTW                         *NETWORK
152400100127     C                   KFLD                    KWDTSP                         *TIPO SERVIZIO
152500100127     C*
152600000823     C* IMNPOSTA DATA / ORA CORRENTE
152700000823     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
152800000823     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
152900000823     C                   Z-ADD     *ZEROS        G08INV
153000000823     C                   MOVEL     '0'           G08ERR
153100000823     C                   CALL      'XSRDA8'
153200000823     C                   PARM                    WLBDA8
153300000823     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
153400000000     C*
153500000825     C* CARICA TABELLE OCCORRENTI
153600000000     C                   EXSR      CARTAB
153700970821     C*
153800970821     C* CALCOLA I MESI DA ELABORARE
153900990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
154000970821     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
154100990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
154200970821     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
154300970821     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORAR
154400970821     C                   EXSR      AMELAB
154500970822     C*
154600000825     C* PRENDE IL VALORE ASSOLUTO DELLA % ALLA QUALE SEGNALARE LO SCOSTAMENTO FASCE
154700970822     C                   MLLZO     1             PARSPD                         *VALORE ASSOLUTO
154800000825     C                   Z-ADD     *ZEROS        MENSPD
154900000825     C                   Z-SUB     PARSPD        MENSPD                         *% RICHIESTA MENO
155000001122     C*
155100001122     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
155200010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
155300010725     C                   MOVE      28            WDFIN
155400081114     C*
155500081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
155600081114     C*
155700081114     C*  Ultimo giorno del mese
155800081114     C                   MOVE      WDFIN         MMGG_4            4 0
155900081114     C                   MOVEL     MMGG_4        Mm_2              2 0
156000081114     C                   MOVEl     WDFIN         Anno_4            4 0
156100081114      * se è febbraio
156200081114     c                   If        Mm_2 = 2
156300081114      *
156400081114      *  potrebbe essere bisestile 29 gg.
156500081114     c     Anno_4        div       4             risulta           3 0
156600081114     c                   mvr                     resto             3 0
156700081114      *  Se bisestile
156800081114     c                   If        resto = 0
156900081114     C                   MOVE      '29'          WDFIN
157000081114     c                   end
157100081114      *
157200081114     c                   else
157300081114      *
157400081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
157500081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
157600081114     c                                Mm_2 = 9 or Mm_2 = 11
157700081114     C                   MOVE      '30'          WDFIN
157800081114     c                   else
157900081114     C                   MOVE      '31'          WDFIN
158000081114     c                   end
158100081114      *
158200081114     c                   endIf
158300081114     C*
158400081114     C*----------- F I N E    M E S E ---------------------------*
158500000000     C*
158600000000     C                   ENDSR
158700070723     C*------------------------------------------------------------------------*
158800070723     C* Aggiorna il flag Nuovo/Acquisito x UNificante
158900070723     C*------------------------------------------------------------------------*
159000070723     C     AGGWFD_uni    BEGSR
159100070727     C*
159200070727      *? Per prima cosa deve rendere omogenei i records nell'ambito dell'anno con "Y" e "S"...  ?
159300070727      *? significa che se nell'anno sono nati 2 codici clienti nuovi mi troverei "Y" su 2 mesi ?
159400070727      *? diversi nell'ambito dello stesso Anno.                             ?
159500070727      *? ..... ovviamente prevale la "Y" più vecchia a cui seguono delle "S" x indicare il Nuovo.
159600070727      *? -------------------------------------------------------------------------- ?
159700070727      *  (esempio esplicativo)
159800070727      *               Anno 200X? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
159900070727      *? Cliente (A)...  ?      |   | Y | S | S | S | S | S | S |   | S | S |   |
160000070727      *? Cliente (B)...  ?      |   |   |   | Y | S | S |   | S | S | S |   |   |
160100070727      *? -----------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
160200070727      *? Unificante (C)         |   | Y | S | Y | S | S | S | S | S | S | S |   |                         ?
160300070727      * -->
160400070727      *? Invece deve essere così|   | Y | S | S | S | S | S | S | S | S | S |   |   ?
160500070727      *? -------------------------------------------------------------------------- ?
160600070723     C*
160700070723     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
160800070723     c                   if        PARWfdpv = 'S'
160900110401     C     *LOVAL        SETLL     WAdfpv5L
161000110401     C                   READ      WAdfpv5L                               99
161100070723     c                   else
161200100127     C     *LOVAL        SETLL     WFDEa05L
161300100127     C                   READ      WFDEa05L                               99
161400070723     c                   end
161500070723     C*
161600070723     C* CICLO FINO A FINE FILE
161700070723DO  1C     *IN99         DOWEQ     *OFF
161800070723     C                   move      wfdIOE        depIOE
161900100127     C                   z-add     wfdCLI        UNIcli
162000070723     C                   z-add     0             Cliente_Anno      1 0
162100070723      *
162200070723     C* a Rottura Cliente
162300070723DO  2C     *IN99         DOWEQ     *OFF
162400070723     C     depIOE        ANDEQ     wfdIOE
162500100127     C     UNIcli        ANDEQ     wfdCLI
162600070723     C                   z-add     wfdANN        depANN
162700070723     C                   Clear                   Ypsilon_anno      1
162800070723     C                   add       1             Cliente_Anno
162900070723     C*
163000070723     C* a Rottura Cliente Anno
163100070723DO  3C     *IN99         DOWEQ     *OFF
163200070723     C     depIOE        ANDEQ     wfdIOE
163300100127     C     UNIcli        ANDEQ     wfdCLI
163400070723     C     depANN        ANDEQ     wfdANN
163500070723     C                   z-add     wfdMES        depMES
163600070723     C*
163700070723     C* a Rottura Cliente Anno/Mese
163800070723DO  4C     *IN99         DOWEQ     *OFF
163900070723     C     depIOE        ANDEQ     wfdIOE
164000100127     C     UNIcli        ANDEQ     wfdCLI
164100070723     C     depANN        ANDEQ     wfdANN
164200070723     C     depMES        ANDEQ     wfdMES
164300070723      *
164400070723      *  Memorizza il primo Ypsilon trovato x cliente Anno
164500070723     C                   if        wfdCLN <> ' ' and Ypsilon_anno = *blank
164600070723     C                   eval      Ypsilon_anno = wfdCLN
164700070723     c                   end
164800070723      *
164900070723      * Tutto il mese cliente deve essere a "Y"
165000070723     C                   if        Ypsilon_anno = 'Y'
165100070723     c                   eval      wfdCLN = 'Y'
165200070723     c                   end
165300070723      *
165400070723      * Dal mese seguente deve impostare sempre la "S"
165500070723      *   anche per l'anno seguente...
165600070723     C                   if        Ypsilon_anno = 'S'
165700070723     c                   eval      wfdCLN = 'S'
165800070723     c                   end
165900070723      *
166000070723     c                   if        PARWfdpv = 'S'
166100100203     C                   UPDATE    WFdfp5
166200070723     c                   else
166300100127     C                   UPDATE    WFDEa5
166400070723     c                   end
166500070723     C*
166600070723     C* LETTURA SUCCESSIVA DATI CLIENTE
166700070723     c                   if        PARWfdpv = 'S'
166800110401     C                   READ      WAdfpV5L                               99
166900070723     c                   else
167000100127     C                   READ      WFDEa05L                               99
167100070723     c                   end
167200070723E   4C                   ENDDO                                                  *ROTTURA Mese
167300070723     C*
167400070723     C*  Se il mese letto aveva la "Y" i mesi seguenti entro l'anno devono avere la "S"
167500070723     C                   if        Ypsilon_anno = 'Y'
167600070723     C                   eval      Ypsilon_anno = 'S'
167700070723     c                   end
167800070723      *
167900070723E   3C                   ENDDO                                                  *ROTTURA CLIENTE/Anno
168000070723     C*
168100070723E   2C                   ENDDO
168200070723     C*
168300070723E   1C                   ENDDO
168400070727     C*
168500070727      *? ------------------------------------------------------------------------------------------------- ?
168600070727      *? ......Dopo avere reso omogeneo nell'ambito dell'anno la sequenza di "Y" e "S", si deve considerare    ?
168700070727      *? sull'anno in CORSO che, se un cliente aveva già del fatturato nell'anno precedente,                           ?
168800070727      *? NON può considerarsi Nuovo.!!!!!!                         ?
168900070727      *? ..... quindi, sul Cliente, nell'Anno in CORSO, si devono eliminare le "Y" e "S" su tutti i records.
169000070727      *? ------------------------------------------------------------------------------------------------- ?
169100070727      *  (esempio esplicativo)
169200070727      *                              ? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
169300070727      *? Unific.(C) Anno Precedente ? |   |   |   | Y | S | S | S | S | S | S | S | S |
169400070727      *? Unific.(C) Anno  in  CORSO ? |   | Y | S | S | S | S | S | S |   |   |   |   |
169500070727      *? -----------------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
169600070727      *? ============================     ?
169700070727      * --> Invece deve essere così
169800070727      *? ============================     ?
169900070727      *? Unific.(C) Anno Precedente   |   |   |   | Y | S | S | S | S | S | S | S | S |   ?-- records anno precedente
170000070727      *? Unific.(C) Anno  in  CORSO   |   |   |   |   |   |   |   |   |   |   |   |   |   ?-- records anno in Corso
170100070727      *? -------------------------------------------------------------------------------- ?
170200070727     C*   Con un istruzione SQL possiamo quindi sistemare la situazione sull'anno in corso
170300070727     C*   semplicemente controllando se nell'anno precedente il Cliente aveva del Fatturato
170400070727     C*   quindi, aggiorniamo tutti i Suoi records dell'Anno in corso x dire che NON è NUOVO.
170500070727      *
170600070727     C/EXEC SQL
170700100127     C+ UPDATE wfdea00f SET wfdcln = ' '
170800070727     C+ WHERE wfdann = :ParAAI AND wfdcln <>' ' AND wfdcli
170900100127     C+ IN (SELECT wfdcli FROM wfdea00f WHERE wfdann = :ParAFP)
171000070727     C/END-EXEC
171100070723     C*
171200121210     C*  Una volta sistemati tutti i records per l'anno in corso con solo la sequenza
171300121210     C*  giusta dal mese in cui nasce il cliente NUOVO.
171400121210     C*  Occorre sistemare eventuali saldi Antecedenti dovuti a storni o spalmature
171500121210     C*  di valori attribuiti al cliente UNIFICANTE....non potendo fare diversamente.
171600121207     C                   Exsr      NUOVO_CON_MESI
171700121207     C*
171800121207     C                   ENDSR
171900121207      *------------------------------------------------------------------------*
172000121207     C* Se L'AUT è considerato NUOVO nell'anno ed ha mesi precedenti con BLANK
172100121207      *------------------------------------------------------------------------*
172200121207     C     NUOVO_con_mesiBEGSR
172300121207     C*
172400121207     C*  se sono state fatte delle Note di Credito o altro tipo di Storni o Rettifiche
172500121210     C*  che vengono spalmate sui mesi precedenti a quello che ha la "Y" come mese iniziale.
172600121210     C*  Occorre ragionare impostando la "Y" anche su tali record ed in seguito fare
172700121210     C*  riferimento al mese più recente con la "Y" per conoscere qual'è il mese di VERO
172800121210     C*  inizio per il codice che si sta trattando.
172900121210     C*  Quindi sui mesi più vecchi ANTECEDENTI: verrà impostata la "Y" per poter
173000121210     C*  determinare nel programma TISE93R a rottura di cliente primo mese letto, se si
173100121210     C*  tratta di cliente NUOVO oppure acquisito.
173200121210     C*
173300121210     C/EXEC SQL
173400121210     C+ update  wfdea00f set  WFDCLN ='Y' WHERE wfdcln=' ' and
173500121210     C+ wfdcli in (select wfdcli from wfdea00f where wfdcln <> ' '
173600121210     C+ and wfdann = :PARAAI) and  wfdcli in  ( SELECT WFDCLI FROM
173700121210     C+ wfdea00f where wfdcli in (select wfdcli from wfdea00f
173800121210     C+ where wfdcln <> ' ' and wfdann = :PARAAI) and  wfdcln=' ')
173900121210     C/END-EXEC
174000121210     C*
174100121207     C                   ENDSR
174200121207     O*------------------------------------------------------------------------*
174300000000     O* STAMPA DEI CODICI AGENTI INESISTENTI O ANNULLATI DA INSERIRE IN SEDE   *
174400000000     O*------------------------------------------------------------------------*
174500000000     OQSYSPRT   E            TESTA            02
174600000000     O                       STA(1)              66
174700000000     O                       STA(2)             132
174800980306     O                       UDATE              119 '  /  /  '
174900000000     O                       PAGE          Z    132
175000000000     O*------------------------
175100000000     O          E            TESTA            03
175200000000     O                       STA(3)              66
175300000000     O                       STA(3)             132
175400000000     O*------------------------
175500000000     O          E            TESTA            04
175600000000     O                       STA(4)              66
175700000000     O*------------------------
175800000000     O          E            TESTA            05
175900000000     O                       STA(3)              66
176000000000     O                       STA(3)             132
176100000000     O*------------------------
176200000000     O          E            ERROR       1
176300000000     O                       DEPAGE               9
176400000000     O                       DEPKSC              27
176500100127     O                       UNIcli              47
176600000000     O*------------------------------------------------------------------------*
176700000000** STA
176800100127                           TISE92AR     ***  AGENTI INESISTENTI DA1
176900980306 MODIFICARE IN ANAGRAFICA CLIENTE  ***       99/99/99     PAG.99992
177000000000------------------------------------------------------------------3
177100000824A G E N T E      C L I E N T E     U N I F I C A N T E            4
177200000824  9999999           9999999             9999999                   5
