000100141219     /*END
000200000000     H*------------------------------------------------------------------------*
000300100302     H* STATISTICA DELTA - CREAZIONE WFILE DI STAMPA                           *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900100202      *------
001000100202      *? INPUT
001100100202      *------
001200000000     FAZORG01L  IF   E           K DISK
001300000824     FTABEL00F  IF   E           K DISK
001400140612     FAZCMM01L  IF   E           K DISK    usropn
001500020404     FTNTBE01L  IF   E           K DISK
001600990609     FSISDC01L  IF   E           K DISK
001700990609     FSISDO01L  IF   E           K DISK
001800100127     FSISDP01L  IF   E           K DISK
001900100202      *------
002000100202      *? OUTPUT
002100100202      *------
002200100202      *?    St.DELTA
002300100127     FWFDEaV1L  UF A E           K DISK    usropn
002400100127     FWFDEa01L  UF A E           K DISK    usropn
002500100127     FWFDEa04L  UF   E           K DISK    usropn
002600100127     F                                     RENAME(WFDEA000:WFDEA4)
002700100127     FWFDEa05L  UF   E           K DISK    usropn
002800100127     F                                     RENAME(WFDEA000:WFDEA5)
002900100202      *---
003000100202      *?    St.Fascie PESI VOLUMI
003100110401     FWAdfpv1L  UF A E           K DISK    usropn
003200110401     FWAdfpv4L  UF   E           K DISK    usropn
003300100203     F                                     RENAME(WFDFPV00:WFDFPV)
003400110401     FWAdfpv5L  UF   E           K DISK    usropn
003500100203     F                                     RENAME(WFDFPV00:WFDFP5)
003600100202      *------
003700000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003800000000     D*------------------------------------------------------------------------*
003900000000     D* SCHIERE
004000000000     D*------------------------------------------------------------------------*
004100000000     D*------------------
004200000000     D* RIGHE DI STAMPA
004300000000     D*------------------
004400000000     D STA             S             66    DIM(5) CTDATA PERRCD(1)
004500000000     D*------------------
004600000000     D* COMMERCIALI IN ERRORE
004700000000     D*------------------
004800020314     D ERG             S              7  0 DIM(500)
004900000000     D*------------------
005000000000     D* ORGANIGRAMMA
005100000000     D*------------------
005200971124     D FIL             S              3  0 DIM(500)                             *FILIALI
005300971124     D DFIL            S             20    DIM(500)
005400971124     D ARE             S              3  0 DIM(500)                             *AREE
005500971124     D DARE            S             20    DIM(500)
005600971124     D DIV             S              1    DIM(500)                             *DIVISIONI
005700971124     D DDIV            S             20    DIM(500)
005800000000     D*------------------
005900000000     D* COMMERCIALI
006000000000     D*------------------
006100141219     d c_Max_Cmm       c                   const(9000)
006200141219     D AGE             S              7  0 DIM(c_Max_Cmm)                       *CODICI AGENTE
006300141219     D RGF             S              7  0 DIM(c_Max_Cmm)                       *CODICI RAGGRUPPAMEN
006400141219     D DAGE            S             25    DIM(c_Max_Cmm)                       *DECODIFICHE AGENTI
006500000000     D*------------------
006600000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
006700000000     D*------------------
006800000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
006900000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
007000000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
007100970821     D*------------------
007200970821     D* FASCE DI PESO
007300970821     D*------------------
007400020522     D FPE             S              2    DIM(11)
007500970821     D*------------------
007600970821     D* PERIODO DA ELABORARE
007700970821     D*------------------
007800990609     D ANN             S              4  0 DIM(12)                              *ANNO
007900000823     D MES             S              2  0 DIM(12)                              *MESE
008000000825     D*------------------
008100000825     D* MESI CON SCOSTAMENTO DELTA FASCE DA SEGNALARE
008200000825     D*------------------
008300000825     D SMEI            S              2  0 DIM(12)                              *ITALIA
008400000825     D SMEE            S              2  0 DIM(12)                              *ESTERO
008500000825     D SMET            S              2  0 DIM(12)                              *TOTALE
008600020314     D*------------------
008700020314     D* RAGGRUPPAMENTI REGIONI/NETWORK
008800020314     D*------------------
008900020315     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIONE
009000020315     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RAGGR. REGIONE
009100020404     D*------------------
009200020404     D* NETWORK AZIENDALI
009300020404     D*------------------
009400020404     D NTWCOD          S              3    DIM(100)                             *CODICI NETWORK
009500020404     D NTWORD          S              2  0 DIM(100)                             *PRIORITA' CONSIDERAZIONE NETWORK
009600000823     D*------------------
009700000823     D* DS CONTROLLO DATA (8)
009800000823     D*------------------
009900000823     D WLBDA8          DS                  INZ
010000000823     D  G08DAT                 1      8  0
010100000823     D  G08INV                 9     16  0
010200000823     D  G08ERR                17     17
010300000823     D  G08TGI                18     22  0
010400000823     D*------------------
010500000823     D* DS DI PROCEDURA
010600000823     D*------------------
010700000823     D BS10DS        E DS                  EXTNAME(TIBS10DS)
010800000000     D*------------------
010900000000     D* PARAMETRI DI LANCIO
011000000000     D*------------------
011100000000     D PARAM           DS
011200990609     D  PARDAT                 1      6  0
011300990609     D  PARAAI                 1      4  0
011400990609     D  PARMMI                 5      6  0
011500990609     D  PARDA2                 7     12  0
011600990609     D  PARAAF                 7     10  0
011700990609     D  PARMMF                11     12  0
011800990609     D  PARDCK                13     13
011900990609     D  PARDCC                14     14
012000990609     D  PARDCF                15     15
012100990609     D  PARFIC                16     18  0
012200990609     D  PARARC                19     21  0
012300990609     D  PARDIC                22     22
012400990609     D  PARDFF                23     23
012500990609     D  PARCOF                24     30  0
012600990609     D  PARDIF                31     31
012700990609     D  PARSTA                32     32
012800000824     D  PARSCE                33     33
012900990609     D  PARTSP                34     34
013000990609     D  PARIOE                35     35
013100990609     D  PARCLV                36     36
013200990609     D  PARDPT                37     42  0
013300990609     D  PARAPI                37     40  0
013400990609     D  PARMPI                41     42  0
013500990609     D  PARDP2                43     48  0
013600990609     D  PARAFP                43     46  0
013700990609     D  PARMFP                47     48  0
013800990609     D  PARSPD                49     52  1
013900990609     D  PARPDD                53     56  1
014000990609     D  PARPDA                57     60  1
014100000707     D  PARPOS                61     61
014200000825     D  PARCRE                62     62
014300010618     D  PARCL1                63     63
014400010618     D  PARCL2                64     64
014500010618     D  PARSIN                65     65
014600020404     D  PARCL3                66     66
014700020404     D  PARCL4                67     67
014800020404     D  PARNTW                68     68
014900030923     D  PARDFD                69     72  1
015000030923     D  PARDFA                73     76  1
015100030923     D  PARDFDP               77     80  1
015200030923     D  PARDFAP               81     84  1
015300030923     D  PARFPC                85     85
015400030923     D  PARWPC                86     86
015500030923     D  PARWCP                87     87
015600070531     D  PARWfdpv              88     88
015700000000     D*------------------
015800990609     D* DATA DI SISDC01L
015900000000     D*------------------
016000000000     D                 DS
016100990609     D  SDCANN                 1      4  0
016200990609     D  SDCMES                 5      6  0
016300990609     D  SDCDAT                 1      6  0
016400100215     D*------------------
016500100215     D* DATA DI SISDO00F
016600100215     D*------------------
016700100215     D                 DS
016800100215     D  SDOANN                 1      4  0
016900100215     D  SDOMES                 5      6  0
017000100215     D  SDODAT                 1      6  0
017100970821     D*------------------
017200100127     D* DATA DI WFDEA00F
017300970821     D*------------------
017400970821     D                 DS
017500970821     D  WFDANN                 1      4  0
017600970821     D  WFDMES                 5      6  0
017700990609     D  WFDDAT                 1      6  0
017800000000     D*------------------
017900000000     D* DS COMMERCIALI
018000000000     D*------------------
018100150105     D*// DS01          E DS
018200000000     D*------------------
018300000000     D* DS FASCE DI PESO
018400000000     D*------------------
018500000000     D DS2L          E DS
018600000000     D*------------------
018700000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
018800000000     D*------------------
018900000000     D DS2W          E DS
019000980403     D*------------------
019100980403     D* DS REPERIMENTO DATI UTENTE
019200980403     D*-------------------
019300980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
019400980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
019500980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
019600980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
019700980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
019800000000     D*------------------
019900000000     D* ARCHITETTURA
020000000000     D*------------------
020100000000     D KPJBA         E DS
020200020314     D*------------------
020300020314     D* DS DI LETTURA TABELLA "2M"
020400020314     D*------------------
020500020314     D DS2M          E DS
020600020404     D*-------------------
020700020404     D* DS DEFINIZIONE TABELLA "NETWORK"
020800020404     D*-------------------
020900020404     D DNTW          E DS
021000000000     C*------------------------------------------------------------------------*
021100000000     C* MAIN LINE
021200000000     C*------------------------------------------------------------------------*
021300000000     C*
021400000000     C* LEGGO IL PRIMO CLIENTE VALIDO
021500000000     C                   Z-ADD     *ZEROS        KSOKSC
021600990609     C     KEYSDO        SETGT     SISDO01L
021700990609     C                   READ      SISDO01L                               99
021800000000     C*
021900000000     C* PER TUTTI I CLIENTI PRESENTI NEI SALDI
022000000000DO  1C     *IN99         DOWEQ     *OFF
022100980119     C                   Z-ADD     SDOKSC        DEPKSC                         *DEPOSITO CLIENTE
022200980119     C*
022300980119     C* CONTROLLA VALIDITA' RECORD
022400980119     C                   EXSR      CHKSDO
022500000000     C*
022600000000     C* OPERAZIONI PER NUOVO CLIENTE
022700980119IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
022800000000     C                   EXSR      NEWKSC
022900100215     C*
023000000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
023100000000     C                   EXSR      SETFIL
023200000000     C*
023300000000     C* CICLO FINO A ROTTURA CODICE CLIENTE
023400980119DO  3C     $FINE         DOWEQ     '0'
023500000000     C*
023600000000     C* OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
023700000000     C                   EXSR      NEWDAT
023800100216     C*
023900100216     C* CREA I SALDI NEL WORK FILE x le VARIE
024000100216     c                   if        PARWfdpv <>'S'
024100100216     C                   EXSR      MEMSALVAR
024200100216     c                   end
024300000000     C*
024400000000     C* CICLO FINO A ROTTURA PERIODO SALDO CLIENTE
024500980119DO  4C     $FINE         DOWEQ     '0'
024600000000     C     SDCDAT        ANDEQ     DEPDAT
024700000000     C*
024800000000     C* CREA I SALDI NEL WORK FILE
024900000824     C                   EXSR      MEMSAL
025000000000     C*
025100000000     C* LEGGE IL RECORD SUCCESSIVO
025200000000     C                   EXSR      LETFIL
025300980119E   4C                   ENDDO                                                  *ROTTURA PERIODO
025400980119E   3C                   ENDDO                                                  *ROTTURA CLIENTE
025500000824E   2C                   ENDIF
025600000000     C*
025700000000     C* LEGGO IL SUCESSIVO CLIENTE VALIDO
025800000000     C                   Z-ADD     DEPKSC        KSOKSC
025900990609     C     KEYSDO        SETGT     SISDO01L
026000990609     C                   READ      SISDO01L                               99
026100000824E   1C                   ENDDO
026200000824     C*
026300000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
026400000825     C                   EXSR      AGGWFD
026500070723     C*
026600070723      *?reimposta correttamente il cliente Nuovo/Acquisito x Unificante...  ?
026700070723     c                   if        parSCE = 'U'
026800070723     C                   EXSR      AGGWFD_Uni
026900070723     c                   end
027000980403     C*
027100980403     C* OPERAZIONI FINALI
027200980403     C                   EXSR      FINSR
027300000000     C*
027400000000     C                   SETON                                        LR
027500000000     C*------------------------------------------------------------------------*
027600000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
027700000000     C*------------------------------------------------------------------------*
027800000000     C     NEWKSC        BEGSR
027900000824     C*
028000000824     C* IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-
028100000823IF  1C     PARSCE        IFEQ      'U'                                          *STAT PER UNIFICANTE
028200000823     C                   CLEAR                   BS10DS
028300001122     C*---               Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
028400010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
028500000823     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
028600000823     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
028700000823     C                   MOVE      DEPKSC        D10COD
028800000823     C                   CALL      'TIBS10R'
028900000823     C                   PARM                    BS10DS
029000100127     C                   Z-ADD     DEPKSC        UNIcli
029100000823IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
029200000823     C     D10COP        ANDNE     *ZEROS
029300100127     C                   Z-ADD     D10COP        UNIcli                         *UNIFICANTE
029400000823E   2C                   ENDIF
029500000823X   1C                   ELSE                                                   *STAT PER CLIENTE
029600100127     C                   Z-ADD     DEPKSC        UNIcli
029700000823E   1C                   ENDIF
029800000824     C*
029900000000     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE
030000100127     C                   MOVEL     UNIcli        DEPFIL                         *FIL+XXXX
030100000000     C                   Z-ADD     1             F
030200000000     C                   SETOFF                                           99
030300000000     C     DEPFIL        LOOKUP    FIL(F)                                 99
030400000000IF  1C     *IN99         IFEQ      *ON
030500000000     C                   Z-ADD     ARE(F)        DEPARE
030600000000     C                   MOVEL     DIV(F)        DEPDIV
030700000000X   1C                   ELSE
030800000000     C                   Z-ADD     *ZEROS        DEPARE
030900000000     C                   MOVEL     *BLANKS       DEPDIV
031000000000E   1C                   ENDIF
031100000824     C*
031200000824     C* REPERISCE IL COMMERCIALE, SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
031300980403     C                   CLEAR                   BS69DS
031400980403     C                   CLEAR                   CLPDS
031500980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
031600100127     C                   MOVEL     UNIcli        I69KCP                         *CLIENTE X CNCLP
031700980403     C                   CALL      'TIBS69R'
031800980403     C                   PARM                    BS69DS
031900980403     C                   PARM                    ACODS
032000980403     C                   PARM                    INDDS
032100980403     C                   PARM                    CLPDS
032200980403     C                   PARM                    CLSDS
032300980406IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
032400980403     C     CLPAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
032500100127     C                   MOVEL     UNIcli        NUM3              3 0          *CODICE FILIALE +
032600980403     C                   Z-ADD     *ZEROS        DEPAGE                         *AGENTE A ZERO  =
032700980403     C                   MOVEL     NUM3          DEPAGE                         *FIL+0000
032800980403X   1C                   ELSE                                                   *NO ERRORE
032900980403     C                   Z-ADD     CLPAGE        DEPAGE                         *CODICE AGENTE
033000980403E   1C                   ENDIF
033100000824     C*
033200000000     C* REPERISCE IL CODICE QUALITA'
033300000000     C                   MOVEL     'C'           DEPCLV
033400000000IF  1C     CLPCLV        IFNE      *BLANKS
033500000000     C                   MOVEL     CLPCLV        DEPCLV
033600000000E   1C                   ENDIF
033700000824     C*
033800000000     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
033900000000     C                   Z-ADD     1             C
034000000000     C                   SETOFF                                           99
034100000000     C     DEPAGE        LOOKUP    AGE(C)                                 99
034200000000IF  1C     *IN99         IFEQ      *OFF
034300000000     C                   EXSR      STAERR
034400000000     C                   Z-ADD     DEPAGE        DEPRGF                         RAGGRUPP = COD AGENT
034500000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
034600000000X   1C                   ELSE
034700000000IF  2C     RGF(C)        IFEQ      *ZEROS
034800000000     C                   Z-ADD     DEPAGE        DEPRGF
034900000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
035000000000X   2C                   ELSE
035100000000     C                   Z-ADD     RGF(C)        DEPRGF
035200000000     C*
035300000000     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
035400000000     C                   Z-ADD     1             C
035500000000     C                   SETOFF                                           99
035600000000     C     DEPRGF        LOOKUP    AGE(C)                                 99
035700000000IF  3C     *IN99         IFEQ      *ON
035800000000     C                   MOVEL     DAGE(C)       DEPRGD                         DESCRIZIONE RAGGR.TO
035900000000X   3C                   ELSE
036000000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGR.TO
036100000000E   3C                   ENDIF
036200000000E   2C                   ENDIF
036300000000E   1C                   ENDIF
036400000824     C*
036500000000     C* REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
036600000000     C                   Z-ADD     1             F
036700000000     C                   SETOFF                                           99
036800000000     C                   MOVEL     DEPRGF        NUM3                           *FIL+XXXX
036900000000     C     NUM3          LOOKUP    FIL(F)                                 99
037000000000IF  1C     *IN99         IFEQ      *ON
037100000000     C                   MOVEL     DIV(F)        DEPDIC
037200000000X   1C                   ELSE
037300000000     C                   MOVEL     *BLANKS       DEPDIC
037400000000E   1C                   ENDIF
037500000000     C*
037600000000     C                   ENDSR
037700000000     C*------------------------------------------------------------------------*
037800000000     C* NEWDAT - OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
037900000000     C*------------------------------------------------------------------------*
038000000000     C     NEWDAT        BEGSR
038100000000     C*
038200990609     C                   Z-ADD     SDCDAT        DEPDAT            6 0
038300000000     C*
038400000000     C* VEDO SE IL CLIENTE, NEL PERIODO, E' NUOVO O ACQUISITO
038500100127     C********           Z-ADD     UNIcli        KSOKSC
038600070720     C*
038700070720      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
038800070720     C                   Z-ADD     DEPksc        KSOKSC
038900070720     C*
039000000000     C                   Z-ADD     SDCANN        KSOANN
039100000000     C                   Z-ADD     SDCMES        KSOMES
039200990609     C     KE2SDO        CHAIN     SISDO01L                           99
039300000000IF  1C     *IN99         IFEQ      *OFF
039400070720     C                   MOVEL     SDOCLN        DEPCLN
039500000000X   1C                   ELSE
039600000000     C                   MOVEL     *BLANKS       DEPCLN
039700000000E   1C                   ENDIF
039800000000     C*
039900000000     C                   ENDSR
040000100127     C*------------------------------------------------------------------------*
040100100127     C* MEMSALVAR - MEMORIZZA I DATI
040200100127     C*------------------------------------------------------------------------*
040300100127     C     MEMSALvar     BEGSR
040400100127     C*
040500100128      *  ciclo sui saldi delle VARIE per riportarne i totali delle "e" e "h"
040600100127     C*
040700100127      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
040800100127     C                   Z-ADD     DEPksc        KSOKSC
040900100216     C                   Z-ADD     SDcDAT        DEPDAT            6 0
041000100216     C                   Z-ADD     SDcANN        KSOANN
041100100216     C                   Z-ADD     SDcMES        KSOMES
041200100127     C     KE2SDO        setll     SISDp01L
041300100127     C     KE2SDO        reade     SISDp01L
041400100127     C*
041500100127     c                   dow       not %EoF(SISDp01L)
041600100127     C*
041700100128     C*  Solo legate alla Priority o H10:30
041800130131     C*      al 31/1/2013 viene chiesto di calcolare il FUEL per scorporarlo dai ricavi
041900130131     C*
042000130131     C*   PRO-MEMORIA: le varie non hanno alcuna possibilità di essere distinte
042100130131     C*                x Italia/Estero o x Tipo servizio o x Network....Quindi possono
042200130131     C*                essere calcolate solo a livello di TOTALE ossia ITALIA+ESTERO ed
042300130131     C*                esclusivamente per CLIENTE o CLIENTE UNIFICANTE
042400130131     C*                infatti anche già adesso avviene così per l'espresso/H10:30.
042500100128     c                   if        SDpSVR = 'e' or SDpSVR = 'h'
042600130131     c                                or SDpSVR = 'f'                            <--- VARIE (f=FUEL)
042700100128     C*
042800100127     C* CONTROLLA ESISTENZA RECORD DI LAVORO
042900100127     C                   Z-ADD     UNIcli        wfvksc                         *CLIENTE
043000100127     C                   Z-ADD     SDpANN        wfvANN                         *ANNO
043100100127     C                   Z-ADD     SDpMES        wfvMES                         *MESE
043200100127     C                   moveL     SDpSVR        wfvSVR                         *VARIA
043300100127     C     KEYDEAV       CHAIN     WFDEAV1L
043400100127     C***
043500100127     C* I M M I S S I O N E
043600100127     C***
043700100127IF  1C                   IF        not %Found(WFDEAV1L)
043800100127      * --
043900100127     c                   CLEAR                   WFDEAV00
044000100127     C                   Z-ADD     UNIcli        WFvksc                         *CLIENTE
044100100127     C                   Z-ADD     SDpANN        WFvANN                         *ANNO
044200100127     C                   Z-ADD     SDpMES        WFvMES                         *MESE
044300100127     C                   MOVEL     SDpSVR        WFvSVR                         *VARIA
044400100127     C                   Z-ADD     SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
044500100127     C                   z-add     SDpRCP        WFvRCP                         *Ricavi Particolari
044600100127      * --
044700100127     C                   WRITE     WFDEAV00
044800100127      * --
044900100127X   1C                   ELSEif    %Found(WFDEAV1L)
045000100127     C***
045100100127     C* V A R I A Z I O N E
045200100127     C***
045300100127     C                   add       SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
045400100127     C                   add       SDpRCP        WFvRCP                         *Ricavi Particolari
045500100127      * ---
045600100127     C                   UPDATE    WFDEAV00
045700100127E   1C                   ENDIF
045800100127     C*
045900100128     c                   end
046000100128     C*
046100100127     C     KE2SDO        reade     SISDp01L
046200100127E   1C                   ENDdo
046300100127     C*
046400100127     C                   ENDSR
046500980119     C*------------------------------------------------------------------------*
046600980119     C* CHKSDO - CONTROLLA VALIDITA' RECORD
046700980119     C*------------------------------------------------------------------------*
046800980119     C     CHKSDO        BEGSR
046900980119     C*
047000980119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
047100980119     C*
047200980119     C                   ENDSR
047300000000     C*------------------------------------------------------------------------*
047400000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
047500000000     C*------------------------------------------------------------------------*
047600000000     C     SETFIL        BEGSR
047700000000     C*
047800000000     C* REIMPOSTO FLAG DI FINE LETTURA SALDI
047900000000     C                   MOVEL     '0'           $FINE
048000000000     C*
048100000000     C                   Z-ADD     DEPKSC        KSCKSC                         *CLIENTE
048200000000     C                   Z-ADD     PARAPI        KSCANN                         *ANNO PRECEDENTE INI
048300000000     C                   Z-ADD     PARMPI        KSCMES                         *MESE PRECEDENTE INI
048400000000     C*
048500000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO SALDO CLIENTE VALIDO
048600990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
048700000000     C*
048800000000IF  1C     *IN99         IFEQ      '1'
048900000000     C                   MOVEL     '1'           $FINE
049000000000X   1C                   ELSE
049100000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
049200000000E   1C                   ENDIF
049300000000     C*
049400000000     C                   ENDSR
049500000000     C*------------------------------------------------------------------------*
049600000000     C* LETFIL - LETTURA PROSSIMO RECORD SALDI CLIENTE
049700000000     C*------------------------------------------------------------------------*
049800000000     C     LETFIL        BEGSR
049900000000     C*
050000000000     C* LEGGO FINO A:
050100000000     C*     - FINE FILE
050200000000     C*     - TROVATO RECORD VALIDO
050300980601     C                   MOVEL     'N'           WRECOK
050400000000DO  1C     $FINE         DOWEQ     '0'
050500000000     C     WRECOK        ANDEQ     'N'
050600000000     C*
050700990609     C     KSCKSC        READE     SISDC01L                               99
050800000000     C*
050900000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
051000000000     C                   MOVEL     '1'           $FINE
051100000000X   2C                   ELSE
051200000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
051300000000E   2C                   ENDIF
051400000000     C*
051500000000E   1C                   ENDDO
051600000000     C*
051700000000     C                   ENDSR
051800000000     C*------------------------------------------------------------------------*
051900000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
052000000000     C*------------------------------------------------------------------------*
052100000000     C     CHKREC        BEGSR
052200000000     C*
052300980601     C                   MOVEL     'S'           WRECOK
052400020516     C*
052500020516     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
052600020516     C                   Z-ADD     1             G
052700020516     C                   SETOFF                                           28
052800020516     C     SDCRBL        LOOKUP    FLAB(G)                                28
052900020516IF  1C     *IN28         IFEQ      *OFF                                         *TIPO NON TROVATO --
053000020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
053100020516E   1C                   ENDIF
053200020516     C*
053300020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
053400020516IF  1C     WRECOK        IFEQ      'S'
053500020516IF  2C     SDCNSP        IFEQ      *ZEROS
053600020516     C     SDCPKG        ANDEQ     *ZEROS
053700020516     C     SDCPCC        ANDEQ     *ZEROS
053800020516     C     SDCPLS        ANDEQ     *ZEROS
053900020516     C     SDCNCO        ANDEQ     *ZEROS
054000020516     C     SDCVOL        ANDEQ     *ZEROS
054100020516     C     SDCVCC        ANDEQ     *ZEROS
054200020516     C     SDCVLS        ANDEQ     *ZEROS
054300020516     C     SDCIRB        ANDEQ     *ZEROS
054400020516     C     SDCIRR        ANDEQ     *ZEROS
054500020516     C     SDCINC        ANDEQ     *ZEROS
054600020516     C     SDCIRP        ANDEQ     *ZEROS
054700020516     C     SDCIRC        ANDEQ     *ZEROS
054800020516     C     SDCIRD        ANDEQ     *ZEROS
054900020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055000020516E   2C                   ENDIF
055100020516E   1C                   ENDIF
055200000000     C*--------------------
055300000000     C* ESCLUSIONI
055400000000     C*--------------------
055500970821     C                   Z-ADD     1             P
055600970821     C                   SETOFF                                           28
055700970821     C     SDCFPE        LOOKUP    FPE(P)                                 28
055800970821IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
055900970821     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056000970821E   1C                   ENDIF
056100000000     C*--------------------
056200000000     C* SELEZIONI
056300000000     C*--------------------
056400000000     C* SE I SALDI SONO DI DATA SUPERIORE A QUELLA RICHIESTA, ESCO DAL CICLO
056500000000IF  1C     SDCDAT        IFGT      PARDA2
056600000000     C                   MOVEL     'N'           WRECOK
056700000000     C                   MOVEL     '1'           $FINE
056800000000E   1C                   ENDIF
056900000000     C*
057000000000     C* SE TIPO SERVIZIO <> TIPO SERVIZIO RICHIESTO, ESCLUDO
057100100127IF  1C**** PARTSP        IFNE      *BLANKS                                      *NON TUTTI
057200100127     C**** SDCTSP        ANDNE     PARTSP
057300100127     C*********          MOVEL     'N'           WRECOK
057400100127E   1C*********          ENDIF
057500000000     C*
057600000000     C                   ENDSR
057700000000     C*------------------------------------------------------------------------*
057800000824     C* MEMSAL - MEMORIZZA I DATI
057900000000     C*------------------------------------------------------------------------*
058000000824     C     MEMSAL        BEGSR
058100070531     C*
058200070531     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
058300070531     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
058400070531     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
058500070531     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
058600070531     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
058700000000     C*
058800000000     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
058900000000     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
059000000000IF  1C     FSP(G)        IFNE      'S'
059100000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
059200070531     C                   z-add     0             sdcPKG                         *PESO
059300070531     C                   z-add     0             sdcPCC                         *PESO CON CML
059400070531     C                   z-add     0             sdcPLS                         *PESO CML
059500070531     C                   z-add     0             sdcVOL                         *VOLUME TOTALE
059600070531     C                   z-add     0             sdcVCC                         *VOLUME CON CML
059700070531     C                   z-add     0             sdcVLS                         *VOLUME CML
059800000000E   1C                   ENDIF
059900000000     C*
060000070531     C* imposta il Peso ed il Volume
060100070531     c                   z-add     sdcPKG        depPKG
060200070531     c                   z-add     sdcVOL        depVOL
060300070531     C*
060400000000     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
060500000000     C                   Z-ADD     *ZEROS        DEPRRN
060600000000     C     SDCIRD        ADD       SDCIRR        DEPRRN
060700000000     C     SDCINC        ADD       DEPRRN        DEPRRN
060800020314     C*
060900020314     C* DETERMINA IL NETWORK
061000020404     C                   MOVEL     *BLANKS       DEPNTWP           3
061100020404     C                   Z-ADD     *ZEROS        DEPORDP           3 0
061200020314     C                   Z-ADD     1             R
061300020314     C     SDCREP        LOOKUP    RGREG(R)                               55
061400020314     C                   IF        *IN55 = *ON
061500020404     C                   EVAL      DEPNTWP = RGNTW(R)
061600020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
061700020404     C                   Z-ADD     1             X
061800020404     C     DEPNTWP       LOOKUP    NTWCOD(X)                              55
061900020404     C                   IF        *IN55 = *ON
062000020404     C                   EVAL      DEPORDP = NTWORD(X)
062100020404     C                   ENDIF
062200020314     C                   ENDIF
062300020314     C*
062400020404     C                   MOVEL     *BLANKS       DEPNTWA           3
062500020404     C                   Z-ADD     *ZEROS        DEPORDA           3 0
062600020314     C                   Z-ADD     1             R
062700020314     C     SDCREA        LOOKUP    RGREG(R)                               55
062800020314     C                   IF        *IN55 = *ON
062900020404     C                   EVAL      DEPNTWA = RGNTW(R)
063000020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
063100020404     C                   Z-ADD     1             X
063200020404     C     DEPNTWA       LOOKUP    NTWCOD(X)                              55
063300020404     C                   IF        *IN55 = *ON
063400020404     C                   EVAL      DEPORDA = NTWORD(X)
063500020404     C                   ENDIF
063600020314     C                   ENDIF
063700020404     C*
063800020404     C* IN BASE ALL'ORDINE DI CONSIDERAZIONE ATTRIBUISCO IL NETWORK DELLA SPEDIZIONE (ATTUALMENTE SOLO X ESTERO)
063900020404     C*
064000020404     C* 1) Italia su Italia
064100020404     C                   IF        DEPNTWP = *BLANKS AND
064200020404     C                             DEPNTWA = *BLANKS
064300020404     C                   EVAL      DEPNTW = *BLANKS
064400020404     C                   ELSE
064500020404     C* 2) Estero su Estero
064600020404     C                   IF        DEPNTWP <> *BLANKS AND
064700020404     C                             DEPNTWA <> *BLANKS
064800020404     C                   IF        DEPORDP < DEPORDA
064900020404     C                   EVAL      DEPNTW = DEPNTWP
065000020404     C                   ELSE
065100020404     C                   EVAL      DEPNTW = DEPNTWA
065200020404     C                   ENDIF
065300020404     C                   ELSE
065400020404     C* 3) Estero su Italia / Italia su Estero
065500020404     C                   IF        DEPNTWP <> *BLANKS
065600020404     C                   EVAL      DEPNTW = DEPNTWP
065700020404     C                   ELSE
065800020404     C                   EVAL      DEPNTW = DEPNTWA
065900020404     C                   ENDIF
066000020404     C                   ENDIF
066100020404     C                   ENDIF
066200020315     C*
066300020315     C* IMPOSTA SE FLAG ITALIA O ESTERO
066400020404     C     DEPNTWP       IFNE      *BLANKS
066500020404     C     DEPNTWA       ORNE      *BLANKS
066600020315     C                   MOVEL     'E'           DEPIOE                         *ESTERO
066700020315X   1C                   ELSE
066800020315     C                   MOVEL     'I'           DEPIOE                         *ITALIA
066900020315E   1C                   ENDIF
067000000000     C*
067100000000     C* CONTROLLA ESISTENZA RECORD DI LAVORO
067200000000     C                   MOVEL     DEPDIV        KWDDIV                         *DIVISIONE
067300000000     C                   Z-ADD     DEPARE        KWDARE                         *AREA
067400000000     C                   Z-ADD     DEPFIL        KWDFIL                         *FILIALE
067500000000     C                   Z-ADD     DEPAGE        KWDAGE                         *COMMERCIALE
067600000000     C                   MOVEL     DEPCLV        KWDCLV                         *CODICE QUALITA'
067700100127     C                   Z-ADD     UNIcli        KWDCLI                         *CLIENTE
067800000824     C                   Z-ADD     SDCANN        KWDANN                         *ANNO
067900000824     C                   Z-ADD     SDCMES        KWDMES                         *MESE
068000000824     C                   MOVEL     SDCFPE        KWDFPE                         *FASCIA DI PESO
068100970821     C                   MOVEL     DEPIOE        KWDIOE                         *ITALIA O ESTERO
068200020315     C                   MOVEL     DEPNTW        KWDNTW                         *NETWORK
068300100127     C                   MOVEL     SDCTSP        KWDTSP                         *Tipo Servizio
068400070531     c                   if        PARWfdpv = 'S'
068500110401     C     KEYDEL        CHAIN     WAdfpv1L                           99
068600070531     c                   else
068700100127     C     KEYDELtsp     CHAIN     WFDEA01L                           99
068800070531     c                   end
068900000000     C***
069000000000     C* I M M I S S I O N E
069100000000     C***
069200000000IF  1C     *IN99         IFEQ      '1'
069300100127     c                   CLEAR                   WFDEA000
069400000000     C                   MOVEL     DEPDIV        WFDDIV                         *DIVISIONE
069500000000     C                   Z-ADD     DEPARE        WFDARE                         *AREA
069600000000     C                   Z-ADD     DEPFIL        WFDFIL                         *FILIALE
069700000000     C                   Z-ADD     DEPAGE        WFDAGE                         *COMMERCIALE
069800000000     C                   MOVEL     DEPCLV        WFDCLV                         *CODICE QUALITA'
069900100127     C                   Z-ADD     UNIcli        WFDCLI                         *CLIENTE
070000000824     C                   Z-ADD     SDCANN        WFDANN                         *ANNO
070100000824     C                   Z-ADD     SDCMES        WFDMES                         *MESE
070200000824     C                   MOVEL     SDCFPE        WFDFPE                         *FASCIA DI PESO
070300000824     C                   MOVEL     DEPIOE        WFDIOE                         *ITALIA O ESTERO
070400000000     C                   Z-ADD     DEPRGF        WFDRGF                         *RAGGRUPP FIL AGENTE
070500000000     C                   MOVEL     DEPRGD        WFDRGD                         *DESCRIZ RAGGRUP.TO
070600000000     C                   MOVEL     DEPDIC        WFDDIC                         DIVISIONE COMMERCIAL
070700000000     C                   Z-ADD     SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
070800070531     C                   z-add     depPKG        WFDpkg                         *Peso
070900070531     C                   z-add     depVOL        WFDvol                         *Volume
071000000000     C                   Z-ADD     DEPRRN        WFDRRN                         RIC REALE NETTO DELT
071100000000     C                   Z-ADD     SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
071200070720      * --
071300000000     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
071400070720      * --
071500980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
071600020315     C                   MOVEL     DEPNTW        WFDNTW                         *NETWORK
071700100127      * --
071800100127     C                   move      SDCTSP        WFDTSP                         Tipo Servizio
071900100127      * --
072000070531     c                   if        PARWfdpv = 'S'
072100110401     C                   WRITE     Wfdfpv00
072200070531     c                   else
072300100127     C                   WRITE     WFDEA000
072400070531     c                   end
072500000000X   1C                   ELSE
072600000000     C***
072700000000     C* V A R I A Z I O N E
072800000000     C***
072900000000     C     WFDNSP        ADD       SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
073000070531     C     WFDpkg        add       depPKG        WFDpkg                         *Peso
073100070531     C     WFDvol        add       depVOL        WFDvol                         *Volume
073200000000     C     WFDRRN        ADD       DEPRRN        WFDRRN                         RIC REALE NETTO DELT
073300000000     C     WFDRPN        ADD       SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
073400980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
073500070720      * ---
073600070720      * solo x unificante
073700070720      *?  Attenzione: se lanciata x Unificante, occorre determinare se il codice  ?
073800070720      *?    unificante è nuovo o vecchio considerando tutti i suoi figli e,  ?
073900070821      *?    qualora uno di essi sia considerato vecchio, sarà quest'ultimo
074000070821      *?     a dare la caratteristica di vecchio al codice Unificante Padre.
074100070720      *?  >> SU SISDO l'info di Nuovo/Vecchio è sempre per singolo Cliente. <<
074200070723     c                   if        parSCE = 'U'
074300070723     c                   if        wfdCLN = *blank or
074400070723     c                             wfdCLN = 'Y' and DEPCLN = 'S'
074500070720     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
074600070720     c                   end
074700070723     c                   end
074800070720      * ---
074900070531     c                   if        PARWfdpv = 'S'
075000110401     C                   UPDATE    Wfdfpv00
075100070531     c                   else
075200100127     C                   UPDATE    WFDEA000
075300070531     c                   end
075400000000E   1C                   ENDIF
075500000000     C*
075600000000     C                   ENDSR
075700000824     C*------------------------------------------------------------------------*
075800000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
075900000824     C*------------------------------------------------------------------------*
076000000825     C     AGGWFD        BEGSR
076100000825     C*
076200000825     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
076300070531     c                   if        PARWfdpv = 'S'
076400110401     C     *LOVAL        SETLL     WAdfpv4L
076500110401     C                   READ(N)   WAdfpv4L                               99
076600070531     c                   else
076700100127     C     *LOVAL        SETLL     WFDEa04L
076800100127     C                   READ(N)   WFDEa04L                               99
076900070531     c                   end
077000000825     C*
077100000825     C* CICLO FINO A FINE FILE
077200000825DO  1C     *IN99         DOWEQ     *OFF
077300000825     C*
077400000825     C* OPERAZIONE PER NUOVO CLIENTE
077500000825     C                   EXSR      NEWCLI
077600000825     C*
077700000825     C* CICLO FINO A ROTTURA CLIENTE
077800000825DO  2C     *IN99         DOWEQ     *OFF
077900100127     C     UNIcli        ANDEQ     WFDCLI
078000070723      *
078100070723      *
078200000825IF  3C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
078300000825     C     WFDDAT        ANDLE     PARDA2
078400100127     C* MEMORIZZA I DATI -WFDEA-
078500000825     C                   EXSR      MEMWFD
078600000825E   3C                   ENDIF
078700000825     C*
078800000825     C* LETTURA SUCCESSIVA DATI CLIENTE
078900070531     c                   if        PARWfdpv = 'S'
079000110401     C                   READ(N)   WAdfpv4L                               99
079100070531     c                   else
079200100127     C                   READ(N)   WFDEa04L                               99
079300070531     c                   end
079400070723     C*
079500000825E   2C                   ENDDO                                                  *ROTTURA CLIENTE
079600000825     C*
079700000825     C* CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
079800000825     C                   EXSR      MODCLI
079900000825     C*
080000000825     C* SI RIPOSIZIONA E LEGGE I DATI DEL CLIENT SUCCESSIVO
080100070531     c                   if        PARWfdpv = 'S'
080200110401     C     UNIcli        SETGT     WAdfpv4L
080300110401     C                   READ(N)   WAdfpv4L                               99
080400070531     c                   else
080500100127     C     UNIcli        SETGT     WFDEa04L
080600100127     C                   READ(N)   WFDEa04L                               99
080700070531     c                   end
080800000825E   1C                   ENDDO
080900000824     C*
081000000824     C                   ENDSR
081100000825     C*------------------------------------------------------------------------*
081200100127     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE -WFDEA-
081300000825     C*------------------------------------------------------------------------*
081400000825     C     NEWCLI        BEGSR
081500000825     C*
081600000825     C* MEMORIZZA IL NUOVO CLIENTE IN UN DEPOSITO
081700100127     C                   Z-ADD     WFDCLI        UNIcli
081800000825     C*
081900000825     C* AZZERA LE VARIABILI DI LAVORO
082000020116     C                   Z-ADD     *ZEROS        TOTIRD           19 5          *RICAVI
082100020116     C                   Z-ADD     *ZEROS        TOTIRP           19 5
082200020116     C                   Z-ADD     *ZEROS        TOTERD           19 5
082300020116     C                   Z-ADD     *ZEROS        TOTERP           19 5
082400020116     C                   Z-ADD     *ZEROS        TOTTRD           19 5
082500020116     C                   Z-ADD     *ZEROS        TOTTRP           19 5
082600000825     C                   Z-ADD     *ZEROS        PERDEI            4 1          *% DELTA
082700000825     C                   Z-ADD     *ZEROS        PERDEE            4 1
082800000825     C                   Z-ADD     *ZEROS        PERDET            4 1
082900000825     C                   MOVEL     'S'           $OKDEI            1            *DELTA OK
083000000825     C                   MOVEL     'S'           $OKDEE            1
083100000825     C                   MOVEL     'S'           $OKDET            1
083200000825     C                   CLEAR                   SMEI                           *MM CON % SEGNALARE
083300000825     C                   CLEAR                   SMEE
083400000825     C                   CLEAR                   SMET
083500000825     C                   Z-ADD     *ZEROS        MI                2 0          *INDICE MESI
083600000825     C                   Z-ADD     *ZEROS        ME                2 0
083700000825     C                   Z-ADD     *ZEROS        MT                2 0
083800000825     C*
083900000825     C                   ENDSR
084000000825     C*------------------------------------------------------------------------*
084100000825     C* MEMWFD - MEMORIZZA I DATI DEL WFILE
084200000825     C*------------------------------------------------------------------------*
084300000825     C     MEMWFD        BEGSR
084400000825     C
084500000825     C* MEMORIZZA I RICAVI ITALIA E ESTERO DEL CLIENTE PER IL PERIODO RICHIESTO
084600000825IF  1C     WFDIOE        IFEQ      'I'                                          *ITALIA
084700000825     C                   ADD       WFDRRN        TOTIRD                         *TOT RIC REALE
084800000825     C                   ADD       WFDRPN        TOTIRP                         *TOT RIC PRESUNTO
084900000825E   1C                   ENDIF
085000000825IF  1C     WFDIOE        IFEQ      'E'                                          *ESTERO
085100000825     C                   ADD       WFDRRN        TOTERD                         *TOT RIC REALE
085200000825     C                   ADD       WFDRPN        TOTERP                         *TOT RIC PRESUNTO
085300000825E   1C                   ENDIF
085400000825IF  1C     WFDIOE        IFEQ      *BLANKS                                      *ITALIA + ESTERO
085500000825     C                   ADD       WFDRRN        TOTTRD                         *TOT RIC REALE
085600000825     C                   ADD       WFDRPN        TOTTRP                         *TOT RIC PRESUNTO
085700000825E   1C                   ENDIF
085800000825     C*
085900000825     C* CONTROLLA SE LA % DELTA DI CIASCUNA FASCIA DI PESO RIENTRA IN QUELLA RICHIESTA
086000000825IF  1C     PARSPD        IFNE      *ZEROS                                       *RICHIESTA
086100000825     C*
086200000825     C* ITALIA
086300000825IF  2C     WFDIOE        IFEQ      'I'
086400000825IF  3C     WFDRRN        IFGT      *ZEROS
086500000825     C     WFDRPN        ORGT      *ZEROS
086600020116     C                   Z-ADD     WFDRRN        WRRW             17 5          *RICAVO REALE
086700020116     C                   Z-ADD     WFDRPN        WRPW             17 5          *RICAVO PRESUNTO
086800000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
086900000825     C                   EXSR      CALDEL
087000000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
087100000825     C                   SETOFF                                           97
087200000825     C     WFDMES        LOOKUP    SMEI                                   97
087300000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
087400000825     C                   ADD       1             MI
087500000825     C                   Z-ADD     WFDMES        SMEI(MI)                       *MESE DA SEGNALARE
087600000825E   5C                   ENDIF
087700000825E   4C                   ENDIF
087800000825E   3C                   ENDIF
087900000825E   2C                   ENDIF
088000000825     C* ESTERO
088100000825IF  2C     WFDIOE        IFEQ      'E'
088200000825IF  3C     WFDRRN        IFGT      *ZEROS
088300000825     C     WFDRPN        ORGT      *ZEROS
088400020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
088500020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
088600000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
088700000825     C                   EXSR      CALDEL
088800000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
088900000825     C                   SETOFF                                           97
089000000825     C     WFDMES        LOOKUP    SMEE                                   97
089100000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
089200000825     C                   ADD       1             ME
089300000825     C                   Z-ADD     WFDMES        SMEE(ME)                       *MESE DA SEGNALARE
089400000825E   5C                   ENDIF
089500000825E   4C                   ENDIF
089600000825E   3C                   ENDIF
089700000825E   2C                   ENDIF
089800000825     C* ITALIA + ESTERO
089900000825IF  2C     WFDIOE        IFEQ      *BLANKS
090000000825IF  3C     WFDRRN        IFGT      *ZEROS
090100000825     C     WFDRPN        ORGT      *ZEROS
090200020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
090300020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
090400000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
090500000825     C                   EXSR      CALDEL
090600000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
090700000825     C                   SETOFF                                           97
090800000825     C     WFDMES        LOOKUP    SMET                                   97
090900000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
091000000825     C                   ADD       1             MT
091100000825     C                   Z-ADD     WFDMES        SMET(MT)                       *MESE DA SEGNALARE
091200000825E   5C                   ENDIF
091300000825E   4C                   ENDIF
091400000825E   3C                   ENDIF
091500000825E   2C                   ENDIF
091600000825     C*
091700000825E   1C                   ENDIF
091800000825     C*
091900000825     C                   ENDSR
092000000825     C*------------------------------------------------------------------------*
092100000825     C* MODCLI - CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
092200000825     C*------------------------------------------------------------------------*
092300000825     C     MODCLI        BEGSR
092400000825     C*
092500000825     C* CALCOLA IL DELTA DEL CLIENTE
092600000825     C                   EXSR      CALCLI
092700000825     C*
092800000825     C* RILEGGE I DATI DEL CLIENTE -->
092900000825     C* - CANCELLA IL CLIENTE SE NON RIENTRA NEL RANGE DELTA RICHIESTO
093000000825     C* - AGGIORNA IL CLIENTE SE HA UNA % DELTA FASCE DA SEGNALARE
093100070531     c                   if        PARWfdpv = 'S'
093200110401     C     UNIcli        SETLL     WAdfpv4L
093300110401     C     UNIcli        READE     WAdfpv4L                               98
093400070531     c                   else
093500100127     C     UNIcli        SETLL     WFDEa04L
093600100127     C     UNIcli        READE     WFDEa04L                               98
093700070531     c                   end
093800070531      *
093900000825DO  1C     *IN98         DOWEQ     *OFF
094000000825     C*
094100000825     C* ITALIA
094200000825IF  2C     WFDIOE        IFEQ      'I'
094300000825IF  3C     $OKDEI        IFEQ      'N'                                          *DELTA NON OK
094400070531     c                   if        PARWfdpv = 'S'
094500100203     C                   DELETE    WFDfpv
094600070531     c                   else
094700100127     C                   DELETE    WFDEa4
094800070531     c                   end
094900000825X   3C                   ELSE                                                   *DELTA OK
095000000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
095100000828     C     WFDDAT        ANDLE     PARDA2
095200000825     C                   SETOFF                                           97
095300000825     C     WFDMES        LOOKUP    SMEI                                   97
095400000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
095500000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
095600070531     c                   if        PARWfdpv = 'S'
095700100203     C                   UPDATE    WFDfpv
095800070531     c                   else
095900100127     C                   UPDATE    WFDEa4
096000070531     c                   end
096100000828E   5C                   ENDIF
096200000828E   4C                   ENDIF
096300000825E   3C                   ENDIF
096400000825E   2C                   ENDIF
096500000825     C*
096600000825     C* ESTERO
096700000825IF  2C     WFDIOE        IFEQ      'E'
096800000825IF  3C     $OKDEE        IFEQ      'N'                                          *DELTA NON OK
096900070531     c                   if        PARWfdpv = 'S'
097000100203     C                   DELETE    WFDfpv
097100070531     c                   else
097200100127     C                   DELETE    WFDEa4
097300070531     c                   end
097400000825X   3C                   ELSE                                                   *DELTA OK
097500000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
097600000828     C     WFDDAT        ANDLE     PARDA2
097700000825     C                   SETOFF                                           97
097800000825     C     WFDMES        LOOKUP    SMEE                                   97
097900000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
098000000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
098100070531     c                   if        PARWfdpv = 'S'
098200100203     C                   UPDATE    WFDfpv
098300070531     c                   else
098400100127     C                   UPDATE    WFDEa4
098500070531     c                   end
098600000828E   5C                   ENDIF
098700000825E   4C                   ENDIF
098800000825E   3C                   ENDIF
098900000825E   2C                   ENDIF
099000000825     C*
099100000825     C* ITALIA + ESTERO
099200000825IF  2C     WFDIOE        IFEQ      *BLANKS
099300000825IF  3C     $OKDET        IFEQ      'N'                                          *DELTA NON OK
099400070531     c                   if        PARWfdpv = 'S'
099500100203     C                   DELETE    WFDfpv
099600070531     c                   else
099700100127     C                   DELETE    WFDEa4
099800070531     c                   end
099900000825X   3C                   ELSE                                                   *DELTA OK
100000000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
100100000828     C     WFDDAT        ANDLE     PARDA2
100200000825     C                   SETOFF                                           97
100300000825     C     WFDMES        LOOKUP    SMET                                   97
100400000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
100500000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
100600070531     c                   if        PARWfdpv = 'S'
100700100203     C                   UPDATE    WFDfpv
100800070531     c                   else
100900100127     C                   UPDATE    WFDEa4
101000070531     c                   end
101100000828E   5C                   ENDIF
101200000825E   4C                   ENDIF
101300000825E   3C                   ENDIF
101400000825E   2C                   ENDIF
101500000825     C*
101600070531     c                   if        PARWfdpv = 'S'
101700110401     C     UNIcli        READE     WAdfpv4L                               98
101800070531     c                   else
101900100127     C     UNIcli        READE     WFDEa04L                               98
102000070531     c                   end
102100000825E   1C                   ENDDO                                                  *DINE DATI CLIENTE
102200000825     C*
102300000825     C                   ENDSR
102400000825     C*------------------------------------------------------------------------*
102500000825     C* CALCOLA I DELTA SUI DATI MEMORIZZATI
102600000825     C*------------------------------------------------------------------------*
102700000825     C     CALCLI        BEGSR
102800000825     C*
102900000825     C* CALCOLA I DELTA E CONTROLLA SE RIENTRANO NEL RANGE RICHIESTO
103000000825IF  1C     PARPDD        IFNE      -999,9                                       *RICHIESTO
103100000825     C     PARPDA        ORNE      +999,9
103200000825     C*
103300000825     C* DELTA ITALIA
103400000825IF  2C     TOTIRD        IFGT      *ZEROS
103500000825     C     TOTIRP        ORGT      *ZEROS
103600020116     C                   Z-ADD     TOTIRD        WRRW                           *RICAVO REALE
103700020116     C                   Z-ADD     TOTIRP        WRPW                           *RICAVO PRESUNTO
103800000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
103900000825     C                   EXSR      CALDEL
104000000825     C                   Z-ADD     WDPW          PERDEI                         *% DELTA ITALIA
104100000825IF  3C     PERDEI        IFLT      PARPDD                                       *DELTA <> RANGE
104200000825     C     PERDEI        ORGT      PARPDA
104300000825     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
104400000825E   3C                   ENDIF
104500010313X   2C                   ELSE                                                   *NO RICAVI ITALIA
104600010313     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
104700000825E   2C                   ENDIF
104800000825     C* DELTA ESTERO
104900000825IF  2C     TOTERD        IFGT      *ZEROS
105000000825     C     TOTERP        ORGT      *ZEROS
105100020116     C                   Z-ADD     TOTERD        WRRW                           *RICAVO REALE
105200020116     C                   Z-ADD     TOTERP        WRPW                           *RICAVO PRESUNTO
105300000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
105400000825     C                   EXSR      CALDEL
105500000825     C                   Z-ADD     WDPW          PERDEE                         *% DELTA ESTERO
105600000825IF  3C     PERDEE        IFLT      PARPDD                                       *DELTA <> RANGE
105700000825     C     PERDEE        ORGT      PARPDA
105800000825     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
105900000825E   3C                   ENDIF
106000010313X   2C                   ELSE                                                   *NO RICAVI ESTERO
106100010313     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
106200000825E   2C                   ENDIF
106300000825     C* DELTA ITA + EST
106400000825IF  2C     TOTTRD        IFGT      *ZEROS
106500000825     C     TOTTRP        ORGT      *ZEROS
106600020116     C                   Z-ADD     TOTTRD        WRRW                           *RICAVO REALE
106700020116     C                   Z-ADD     TOTTRP        WRPW                           *RICAVO PRESUNTO
106800000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
106900000825     C                   EXSR      CALDEL
107000000825     C                   Z-ADD     WDPW          PERDET                         *% DELTA ESTERO
107100000825IF  3C     PERDET        IFLT      PARPDD                                       *DELTA <> RANGE
107200000825     C     PERDET        ORGT      PARPDA
107300000825     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
107400000825E   3C                   ENDIF
107500010313X   2C                   ELSE                                                   *NO RICAVI TOTALI
107600010313     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
107700000825E   2C                   ENDIF
107800000825     C*
107900000825E   1C                   ENDIF
108000000825     C*
108100000825     C                   ENDSR
108200000824     C*------------------------------------------------------------------------*
108300000824     C* CALDEL - CALCOLA IL DELTA
108400000824     C*          INPUT  --> WRRW: RICAVO REALE
108500970821     C*                     WRPW: RICAVO PRESUNTO
108600970821     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
108700970821     C*------------------------------------------------------------------------*
108800970821     C     CALDEL        BEGSR
108900970821     C*--------------------
109000970821     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
109100970821     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
109200970821     C*--------------------
109300970821     C*
109400970821     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
109500020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
109600970821     C     GAP           MULT      100           GAP                            (GAP x 100)
109700970821     C*
109800970821     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
109900970821IF  1C     WRRW          IFEQ      *ZEROS
110000000825     C                   Z-ADD     1             WRRW
110100970821E   1C                   ENDIF
110200970821     C*
110300970821     C     GAP           DIV       WRRW          GAP
110400000825     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
110500970821     C*
110600970821     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
110700970821     C     WDPW          COMP      0                                    28
110800970821     C* POSITIVO
110900970821IF  1C     *IN28         IFEQ      *OFF
111000970821IF  2C     GAP           IFGE      1000
111100970821     C                   Z-ADD     +999,9        WDPW
111200970821E   2C                   ENDIF
111300970821     C* NEGATIVO
111400970821X   1C                   ELSE
111500970821IF  2C     GAP           IFLE      -1000
111600970821     C                   Z-ADD     -999,9        WDPW
111700970821E   2C                   ENDIF
111800970821E   1C                   ENDIF
111900970821     C*
112000970821     C                   ENDSR
112100000000     C*------------------------------------------------------------------------*
112200000000     C* STAERR - STAMPA RIGA DI ERRORE PER AGENTE INESISTENTE O ANNULLATO
112300000000     C*------------------------------------------------------------------------*
112400000000     C     STAERR        BEGSR
112500000000     C*
112600000000IF  1C     *INOF         IFEQ      *ON
112700000000     C                   EXCEPT    TESTA
112800000000     C                   SETOFF                                       OF
112900000000E   1C                   ENDIF
113000000000     C*
113100000000     C* AGENTE NON ANCORA STAMPATO, SCRIVO ERRORE
113200000000     C                   SETOFF                                       97
113300000000     C     DEPAGE        LOOKUP    ERG                                    97
113400000000IF  1C     *IN97         IFEQ      *OFF
113500000000     C                   ADD       1             I                 3 0
113600000000     C                   Z-ADD     DEPAGE        ERG(I)                         *MEMORIZZA AGENTE
113700000000     C                   EXCEPT    ERROR                                        *STAMPA L'ERRORE
113800000000E   1C                   ENDIF
113900000000     C*
114000000000     C                   ENDSR
114100000000     C*------------------------------------------------------------------------*
114200000000     C* CARTAB - CARICA I DATI IN TABELLE
114300000000     C*------------------------------------------------------------------------*
114400000000     C     CARTAB        BEGSR
114500000000     C*--------------------
114600000000     C* ORGANIGRAMMA
114700000000     C*--------------------
114800000000     C                   Z-ADD     *ZEROS        F
114900000000     C                   Z-ADD     *ZEROS        KORFIL
115000000000     C     KEYORG        SETLL     AZORG01L
115100000000     C                   READ      AZORG01L                               99
115200000000DO  1C     *IN99         DOWEQ     '0'
115300980119IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
115400980119IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
115500000000     C                   ADD       1             F
115600000000     C                   Z-ADD     ORGFIL        FIL(F)
115700000000     C                   MOVEL     ORGDES        DFIL(F)
115800020906     C                   Z-ADD     1             KTBKUT
115900000000     C                   MOVEL     '05'          KTBCOD
116000000000     C                   MOVEL     *BLANKS       KTBKEY
116100000000     C                   MOVEL     ORGCAR        KTBKEY
116200000824     C     KEYTAB        CHAIN     TABEL00F                           99
116300980119IF  5C     *IN99         IFEQ      *OFF
116400000000     C                   Z-ADD     ORGCAR        ARE(F)
116500000000     C                   MOVEL     TBLUNI        DARE(F)
116600980119E   5C                   ENDIF
116700020906     C                   Z-ADD     1             KTBKUT
116800000000     C                   MOVEL     '17'          KTBCOD
116900000000     C                   MOVEL     *BLANKS       KTBKEY
117000000000     C                   MOVEL     ORGFL3        KTBKEY
117100000824     C     KEYTAB        CHAIN     TABEL00F                           99
117200980119IF  5C     *IN99         IFEQ      *OFF
117300000000     C                   MOVEL     ORGFL3        DIV(F)
117400000000     C                   MOVEL     TBLUNI        DDIV(F)
117500980119E   5C                   ENDIF
117600980119E   3C                   ENDIF
117700980119E   2C                   ENDIF
117800000000     C                   READ      AZORG01L                               99
117900000000E   1C                   ENDDO
118000000000     C*--------------------
118100000000     C* COMMERCIALI
118200000000     C*--------------------
118300140612     C********           Z-ADD     *ZEROS        C
118400140612     C********           Z-ADD     1             KTBKUT
118500140612     C********           MOVEL     '01'          KTBCOD
118600140612     C***  KEYTA2        SETLL     TABEL00F
118700140612     C***  KEYTA2        READE     TABEL00F                               99
118800140612DO  1C***  *IN99         DOWEQ     *OFF
118900140612IF  2C***  TBLFLG        IFEQ      *BLANKS
119000140612     C********           ADD       1             C
119100140612     C********           MOVEL     TBLUNI        DS01
119200140612     C********           MOVEL     TBLKEY        AGE(C)
119300140612     C********           MOVEL     §01RGF        RGF(C)
119400140612     C********           MOVEL     §01AGE        DAGE(C)
119500140612E   2C********           ENDIF
119600140612     C***  KEYTA2        READE     TABEL00F                               99
119700140612E   1C********           ENDDO
119800140612      *********
119900140612     C                   Z-ADD     *ZEROS        C
120000140612     c                   open      azCMM01l
120100140612     C     *loval        SETLL     azCMM01l
120200140612     C                   READ      azCMM01l
120300140612DO  1C                   DOW       not %Eof(azCMM01l)
120400140612IF  2C                   IF        cmmATB = *blank
120500140612     C                   ADD       1             C
120600140612     C                   z-add     cmmCOD        AGE(C)
120700140612     C                   z-add     cmmUNI        RGF(C)
120800140612     C                   MOVEL     cmmDES        DAGE(C)
120900140612E   2C                   ENDIF
121000140612     C                   READ      azCMM01l
121100140612E   1C                   ENDDO
121200140612     c                   close     azCMM01l
121300000000     C*-------------------------
121400000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
121500000000     C*-------------------------
121600000000     C                   Z-ADD     *ZEROS        G
121700020906     C                   Z-ADD     1             KTBKUT
121800000000     C                   MOVEL     '2W'          KTBCOD
121900000000     C     KEYTA2        SETLL     TABEL00F
122000000824     C     KEYTA2        READE     TABEL00F                               99
122100000000DO  1C     *IN99         DOWEQ     *OFF
122200000000IF  2C     TBLFLG        IFEQ      *BLANKS
122300000000     C                   ADD       1             G
122400000000     C                   MOVEL     TBLKEY        FLAB(G)
122500000000     C                   MOVEL     TBLUNI        DS2W
122600000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
122700000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
122800000000E   2C                   ENDIF
122900000824     C     KEYTA2        READE     TABEL00F                               99
123000000000E   1C                   ENDDO
123100990217     C*--------------------
123200000000     C* FASCE DI PESO
123300990217     C*--------------------
123400990609IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
123500990217     C                   MOVEL     'SDC'         WSX               3
123600990217X   1C                   ELSE                                                   *FASCE DAL '99
123700020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO AL 2001
123800020325     C                   MOVEL     'S9C'         WSX
123900020325X   2C                   ELSE                                                   *FASCE DAL 2002
124000020325     C                   MOVEL     'S2C'         WSX
124100020325E   2C                   ENDIF
124200020325E   1C                   ENDIF
124300970821     C                   Z-ADD     *ZEROS        P
124400020906     C                   Z-ADD     1             KTBKUT
124500000000     C                   MOVEL     '2L'          KTBCOD
124600000000     C     KEYTA2        SETLL     TABEL00F
124700000824     C     KEYTA2        READE     TABEL00F                               99
124800000000DO  1C     *IN99         DOWEQ     *OFF
124900000000IF  2C     TBLFLG        IFEQ      *BLANKS
125000990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
125100000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
125200990217IF  3C     FILEST        IFEQ      WSX
125300000000     C                   MOVEL     TBLUNI        DS2L
125400000309IF  4C     §2LKGI        IFGE      5000
125500000309     C     PARDAT        ANDLT     200001
125600000309     C                   ELSE
125700970821     C                   ADD       1             P
125800970821     C                   MOVE      STACOD        FPE(P)                         *...+XX
125900000309E   4C                   ENDIF
126000000000E   3C                   ENDIF
126100000000E   2C                   ENDIF
126200000824     C     KEYTA2        READE     TABEL00F                               99
126300000000E   1C                   ENDDO
126400020314     C*-------------------------
126500020314     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
126600020314     C*-------------------------
126700020314     C                   Z-ADD     *ZEROS        R
126800020906     C                   Z-ADD     1             KTBKUT
126900020314     C                   MOVEL     '2M'          KTBCOD
127000020314     C     KEYTA2        SETLL     TABEL00F
127100020314     C     KEYTA2        READE     TABEL00F                               99
127200020314DO  1C     *IN99         DOWEQ     *OFF
127300020314IF  2C     TBLFLG        IFEQ      *BLANKS
127400020314     C                   ADD       1             R
127500020314     C                   MOVEL     TBLKEY        RGREG(R)
127600020314     C                   MOVEL     TBLUNI        DS2M
127700020314     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
127800020314E   2C                   ENDIF
127900020314     C     KEYTA2        READE     TABEL00F                               99
128000020314E   1C                   ENDDO
128100020404     C*----------
128200020404     C* NETWORK AZIENDALI
128300020404     C*----------
128400020404     C                   Z-ADD     *ZEROS        X
128500020404     C                   EVAL      TBECOD = 'NTW'
128600020404     C     KEYTBE        SETLL     TNTBE01L
128700020404     C     KEYTBE        READE     TNTBE01L
128800020404     C                   DOW       NOT %EOF(TNTBE01L)
128900020404     C                   ADD       1             X
129000020404     C                   MOVEL     TBEUNI        DNTW
129100020404     C                   EVAL      NTWCOD(X) = TBEKE1
129200020404     C                   EVAL      NTWORD(X) = §NTWORD
129300020404     C     KEYTBE        READE     TNTBE01L
129400020404     C                   ENDDO
129500000000     C*
129600000000     C                   ENDSR
129700970821     C*------------------------------------------------------------------------*
129800970821     C* AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
129900970821     C*------------------------------------------------------------------------*
130000970821     C     AMELAB        BEGSR
130100970821     C*---
130200970821     C* ANNO INIZIALE UGUALE AD ANNO FINALE
130300970821     C*---
130400970821IF  1C     WAA2          IFEQ      WAA
130500970821     C*
130600970821     C* CALCOLO I MESI DI DIFFERENZA
130700970821DO  2C     WMM           DOWLE     WMM2
130800970821     C                   ADD       1             JI
130900970821     C                   Z-ADD     WAA           ANN(JI)
131000970821     C                   Z-ADD     WMM           MES(JI)
131100970821     C                   ADD       1             WMM
131200970821E   2C                   ENDDO
131300970821     C*
131400970821     C* IL RESTO DELLA SCHIERA A ZERO
131500970821DO  2C     JI            DOWLT     12
131600970821     C                   ADD       1             JI
131700970821     C                   Z-ADD     *ZEROS        ANN(JI)
131800970821     C                   Z-ADD     *ZEROS        MES(JI)
131900970821E   2C                   ENDDO
132000970821     C*---
132100970821     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
132200970821     C*---
132300970821X   1C                   ELSE
132400970821     C*
132500970821     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
132600970821DO  2C     WMM           DOWLE     12
132700970821     C                   ADD       1             JI
132800970821     C                   Z-ADD     WAA           ANN(JI)
132900970821     C                   Z-ADD     WMM           MES(JI)
133000970821     C                   ADD       1             WMM
133100970821E   2C                   ENDDO
133200970821     C*
133300970821     C* MEMORIZZO I MESI DELL'ANNO FINALE
133400970821     C                   Z-ADD     01            WMM
133500970821DO  2C     WMM           DOWLE     WMM2
133600970821     C                   ADD       1             JI
133700970821     C                   Z-ADD     WAA2          ANN(JI)
133800970821     C                   Z-ADD     WMM           MES(JI)
133900970821     C                   ADD       1             WMM
134000970821E   2C                   ENDDO
134100970821     C*
134200970821     C* IL RESTO DELLA SCHIERA A ZERO
134300970821DO  2C     JI            DOWLT     12
134400970821     C                   ADD       1             JI
134500970821     C                   Z-ADD     *ZEROS        ANN(JI)
134600970821     C                   Z-ADD     *ZEROS        MES(JI)
134700970821E   2C                   ENDDO
134800970821E   1C                   ENDIF
134900970821     C*
135000970821     C                   ENDSR
135100980403     C*------------------------------------------------------------------------*
135200980403     C* FINSR - OPERAZIONI FINALI
135300980403     C*------------------------------------------------------------------------*
135400980403     C     FINSR         BEGSR
135500980403     C*
135600980403     C* CHIUDE I FILE DEI PGM CHIAMATI
135700980403     C                   CLEAR                   BS69DS
135800980403     C                   CLEAR                   ACODS
135900980403     C                   CLEAR                   INDDS
136000980403     C                   CLEAR                   CLPDS
136100980403     C                   CLEAR                   CLSDS
136200980403     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
136300980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
136400980403     C                   CALL      'TIBS69R'
136500980403     C                   PARM                    BS69DS
136600980403     C                   PARM                    ACODS
136700980403     C                   PARM                    INDDS
136800980403     C                   PARM                    CLPDS
136900980403     C                   PARM                    CLSDS
137000980403     C*
137100980403     C                   ENDSR
137200000000     C*------------------------------------------------------------------------*
137300000000     C* *INZSR - IMPOSTAZIONI INIZIALI
137400000000     C*------------------------------------------------------------------------*
137500000000     C     *INZSR        BEGSR
137600000000     C*
137700000000     C* PARAMETRI IN INPUT AL PGM
137800000000     C     *ENTRY        PLIST
137900000000     C                   PARM                    KPJBA
138000000000     C                   MOVEL     KPJBU         PARAM
138100070531     c*
138200070531     c* apertura condizionata dei 2 files da generare
138300070531     c                   if        PARWfdpv = 'S'
138400110401     c                   open      WAdfpv1l
138500110401     c                   open      WAdfpv4l
138600110401     c                   open      WAdfpv5l
138700070531     c                   else
138800100127     c                   open      WFdea01l
138900100127     c                   open      WFdea04l
139000100127     c                   open      WFdea05l
139100100127     c                   open      WFdeav1l
139200070531     c                   end
139300000000     C*
139400000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
139500000823     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
139600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
139700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
139800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
139900990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC01L
140000000000     C     *LIKE         DEFINE    SDCANN        KSCANN
140100000000     C     *LIKE         DEFINE    SDCMES        KSCMES
140200990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO01L
140300000000     C     *LIKE         DEFINE    SDOANN        KSOANN
140400000000     C     *LIKE         DEFINE    SDOMES        KSOMES
140500100127     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEa01L
140600000000     C     *LIKE         DEFINE    WFDARE        KWDARE
140700000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
140800000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
140900000000     C     *LIKE         DEFINE    WFDCLV        KWDCLV
141000000823     C     *LIKE         DEFINE    WFDCLI        KWDCLI
141100000000     C     *LIKE         DEFINE    WFDANN        KWDANN
141200000000     C     *LIKE         DEFINE    WFDMES        KWDMES
141300000824     C     *LIKE         DEFINE    WFDFPE        KWDFPE
141400000000     C     *LIKE         DEFINE    WFDIOE        KWDIOE
141500020315     C     *LIKE         DEFINE    WFDNTW        KWDNTW
141600100127     C     *LIKE         DEFINE    WFDTSP        KWDTSP
141700000000     C*
141800000000     C* DEFINIZIONI DEPOSITI E VARIABILI
141900000000     C     *LIKE         DEFINE    CLPKSC        DEPKSC
142000100127     C     *LIKE         DEFINE    CLPKSC        UNIcli
142100000000     C     *LIKE         DEFINE    CLPAGE        DEPAGE
142200000000     C     *LIKE         DEFINE    ORGFIL        DEPFIL
142300000000     C     *LIKE         DEFINE    ORGCAR        DEPARE
142400000000     C     *LIKE         DEFINE    ORGFL3        DEPDIV
142500000000     C     *LIKE         DEFINE    ORGFL3        DEPDIC
142600000000     C     *LIKE         DEFINE    WFDRRN        DEPRRN
142700000000     C     *LIKE         DEFINE    CLPAGE        DEPRGF
142800000000     C     *LIKE         DEFINE    CLPCLV        DEPCLV
142900140612     C**** *LIKE         DEFINE    §01AGE        DEPRGD
143000140612     C     *LIKE         DEFINE    CMMDES        DEPRGD
143100000000     C     *LIKE         DEFINE    SDOCLN        DEPCLN
143200970821     C     *LIKE         DEFINE    WFDIOE        DEPIOE
143300020315     C     *LIKE         DEFINE    WFDNTW        DEPNTW
143400070531     C     *LIKE         DEFINE    WFDRRN        DEPRRN
143500100203     C* x il WFdfpv0F
143600070531     C     *LIKE         DEFINE    WFDpkg        DEPpkg
143700070531     C     *LIKE         DEFINE    WFDvol        DEPvol
143800070723     C     *LIKE         DEFINE    wfdANN        depANN
143900070723     C     *LIKE         DEFINE    wfdMES        depMES
144000000825     C*
144100000825     C* DEFINIZIONI VARIABILI DI LAVORO
144200000825     C     *LIKE         DEFINE    PARSPD        MENSPD
144300000000     C                   MOVEL     '0'           $FINE             1            *FINE PROGRAMMA
144400980601     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
144500980601     C                   MOVEL     '0'           PCLNEW            1            *CLIENTE NUOVO PRECE
144600000000     C                   MOVEL     '0'           CCLNEW            1            *CLIENTE NUOVO CORRE
144700000000     C                   Z-ADD     *ZEROS        C                 4 0          *PER COMMERCIALI
144800000000     C                   Z-ADD     *ZEROS        G                 4 0          *PER FLAG TIPI BOLLA
144900000000     C                   Z-ADD     *ZEROS        J                 4 0          *PER MESI ELABORABIL
145000000000     C                   Z-ADD     *ZEROS        F                 4 0          *PER ORGANIGRAMMA
145100970821     C                   Z-ADD     *ZEROS        P                 4 0          *PER FASCE DI PESO
145200970821     C                   Z-ADD     *ZEROS        A                 4 0          *PER ANNI
145300970821     C                   Z-ADD     *ZEROS        M                 4 0          *PER MESI
145400020404     C                   Z-ADD     *ZEROS        X                 3 0          *PER RAGGRUPPAMENTI REGIONI/NETWORK
145500020404     C                   Z-ADD     *ZEROS        R                 3 0          *PER NETWORK
145600000000     C                   SETON                                        OF        *OVERFLOW
145700000000     C*
145800000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
145900000000     C     KEYORG        KLIST
146000000000     C                   KFLD                    KORFIL                         *FILIALE
146100000000     C*
146200000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
146300000000     C     KEYTAB        KLIST
146400000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
146500000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
146600000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
146700000000     C*
146800000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
146900000000     C     KEYTA2        KLIST
147000000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
147100000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
147200020404     C*
147300020404     C* CHIAVE LETTURA TNTBE00F - PARZIALE
147400020404     C     KEYTBE        KLIST
147500020404     C                   KFLD                    TBECOD
147600000000     C*
147700990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
147800000000     C     KEYSDC        KLIST
147900000000     C                   KFLD                    KSCKSC                         *CODICE CLIENTE
148000000000     C                   KFLD                    KSCANN                         *ANNO
148100000000     C                   KFLD                    KSCMES                         *MESE
148200000000     C*
148300990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
148400000000     C     KEYSDO        KLIST
148500000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
148600000000     C*
148700990609     C* CHIAVE LETTURA SISDO01L - COMPLETA
148800000000     C     KE2SDO        KLIST
148900000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
149000000000     C                   KFLD                    KSOANN                         *ANNO
149100000000     C                   KFLD                    KSOMES                         *MESE
149200100127     C*
149300100127     C* CHIAVE LETTURA WFdeaV1L - COMPLETA
149400100127     C     KEYdeaV       KLIST
149500100215     C                   KFLD                    WFVSVR                         *VARIA
149600100127     C                   KFLD                    WFVKSC                         *CODICE CLIENTE
149700100127     C                   KFLD                    WFVANN                         *ANNO
149800100127     C                   KFLD                    WFVMES                         *MESE
149900100127     C*
150000100127     C* CHIAVE LETTURA WFDEa01L - COMPLETA
150100000000     C     KEYDEL        KLIST
150200000000     C                   KFLD                    KWDDIV                         *DIVISIONE
150300000000     C                   KFLD                    KWDARE                         *AREA
150400000000     C                   KFLD                    KWDFIL                         *FILIALE
150500000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
150600000000     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
150700000823     C                   KFLD                    KWDCLI                         *CLIENTE
150800000000     C                   KFLD                    KWDANN                         *ANNO
150900000000     C                   KFLD                    KWDMES                         *MESE
151000000824     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
151100000000     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
151200020614     C                   KFLD                    KWDNTW                         *NETWORK
151300000823     C*
151400100127     C     KEYDELtsp     KLIST
151500100127     C                   KFLD                    KWDDIV                         *DIVISIONE
151600100127     C                   KFLD                    KWDARE                         *AREA
151700100127     C                   KFLD                    KWDFIL                         *FILIALE
151800100127     C                   KFLD                    KWDAGE                         *COMMERCIALE
151900100127     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
152000100127     C                   KFLD                    KWDCLI                         *CLIENTE
152100100127     C                   KFLD                    KWDANN                         *ANNO
152200100127     C                   KFLD                    KWDMES                         *MESE
152300100127     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
152400100127     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
152500100127     C                   KFLD                    KWDNTW                         *NETWORK
152600100127     C                   KFLD                    KWDTSP                         *TIPO SERVIZIO
152700100127     C*
152800000823     C* IMNPOSTA DATA / ORA CORRENTE
152900000823     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
153000000823     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
153100000823     C                   Z-ADD     *ZEROS        G08INV
153200000823     C                   MOVEL     '0'           G08ERR
153300000823     C                   CALL      'XSRDA8'
153400000823     C                   PARM                    WLBDA8
153500000823     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
153600000000     C*
153700000825     C* CARICA TABELLE OCCORRENTI
153800000000     C                   EXSR      CARTAB
153900970821     C*
154000970821     C* CALCOLA I MESI DA ELABORARE
154100990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
154200970821     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
154300990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
154400970821     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
154500970821     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORAR
154600970821     C                   EXSR      AMELAB
154700970822     C*
154800000825     C* PRENDE IL VALORE ASSOLUTO DELLA % ALLA QUALE SEGNALARE LO SCOSTAMENTO FASCE
154900970822     C                   MLLZO     1             PARSPD                         *VALORE ASSOLUTO
155000000825     C                   Z-ADD     *ZEROS        MENSPD
155100000825     C                   Z-SUB     PARSPD        MENSPD                         *% RICHIESTA MENO
155200001122     C*
155300001122     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
155400010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
155500010725     C                   MOVE      28            WDFIN
155600081114     C*
155700081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
155800081114     C*
155900081114     C*  Ultimo giorno del mese
156000081114     C                   MOVE      WDFIN         MMGG_4            4 0
156100081114     C                   MOVEL     MMGG_4        Mm_2              2 0
156200081114     C                   MOVEl     WDFIN         Anno_4            4 0
156300081114      * se è febbraio
156400081114     c                   If        Mm_2 = 2
156500081114      *
156600081114      *  potrebbe essere bisestile 29 gg.
156700081114     c     Anno_4        div       4             risulta           3 0
156800081114     c                   mvr                     resto             3 0
156900081114      *  Se bisestile
157000081114     c                   If        resto = 0
157100081114     C                   MOVE      '29'          WDFIN
157200081114     c                   end
157300081114      *
157400081114     c                   else
157500081114      *
157600081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
157700081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
157800081114     c                                Mm_2 = 9 or Mm_2 = 11
157900081114     C                   MOVE      '30'          WDFIN
158000081114     c                   else
158100081114     C                   MOVE      '31'          WDFIN
158200081114     c                   end
158300081114      *
158400081114     c                   endIf
158500081114     C*
158600081114     C*----------- F I N E    M E S E ---------------------------*
158700000000     C*
158800000000     C                   ENDSR
158900070723     C*------------------------------------------------------------------------*
159000070723     C* Aggiorna il flag Nuovo/Acquisito x UNificante
159100070723     C*------------------------------------------------------------------------*
159200070723     C     AGGWFD_uni    BEGSR
159300070727     C*
159400070727      *? Per prima cosa deve rendere omogenei i records nell'ambito dell'anno con "Y" e "S"...  ?
159500070727      *? significa che se nell'anno sono nati 2 codici clienti nuovi mi troverei "Y" su 2 mesi ?
159600070727      *? diversi nell'ambito dello stesso Anno.                             ?
159700070727      *? ..... ovviamente prevale la "Y" più vecchia a cui seguono delle "S" x indicare il Nuovo.
159800070727      *? -------------------------------------------------------------------------- ?
159900070727      *  (esempio esplicativo)
160000070727      *               Anno 200X? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
160100070727      *? Cliente (A)...  ?      |   | Y | S | S | S | S | S | S |   | S | S |   |
160200070727      *? Cliente (B)...  ?      |   |   |   | Y | S | S |   | S | S | S |   |   |
160300070727      *? -----------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
160400070727      *? Unificante (C)         |   | Y | S | Y | S | S | S | S | S | S | S |   |                         ?
160500070727      * -->
160600070727      *? Invece deve essere così|   | Y | S | S | S | S | S | S | S | S | S |   |   ?
160700070727      *? -------------------------------------------------------------------------- ?
160800070723     C*
160900070723     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
161000070723     c                   if        PARWfdpv = 'S'
161100110401     C     *LOVAL        SETLL     WAdfpv5L
161200110401     C                   READ      WAdfpv5L                               99
161300070723     c                   else
161400100127     C     *LOVAL        SETLL     WFDEa05L
161500100127     C                   READ      WFDEa05L                               99
161600070723     c                   end
161700070723     C*
161800070723     C* CICLO FINO A FINE FILE
161900070723DO  1C     *IN99         DOWEQ     *OFF
162000070723     C                   move      wfdIOE        depIOE
162100100127     C                   z-add     wfdCLI        UNIcli
162200070723     C                   z-add     0             Cliente_Anno      1 0
162300070723      *
162400070723     C* a Rottura Cliente
162500070723DO  2C     *IN99         DOWEQ     *OFF
162600070723     C     depIOE        ANDEQ     wfdIOE
162700100127     C     UNIcli        ANDEQ     wfdCLI
162800070723     C                   z-add     wfdANN        depANN
162900070723     C                   Clear                   Ypsilon_anno      1
163000070723     C                   add       1             Cliente_Anno
163100070723     C*
163200070723     C* a Rottura Cliente Anno
163300070723DO  3C     *IN99         DOWEQ     *OFF
163400070723     C     depIOE        ANDEQ     wfdIOE
163500100127     C     UNIcli        ANDEQ     wfdCLI
163600070723     C     depANN        ANDEQ     wfdANN
163700070723     C                   z-add     wfdMES        depMES
163800070723     C*
163900070723     C* a Rottura Cliente Anno/Mese
164000070723DO  4C     *IN99         DOWEQ     *OFF
164100070723     C     depIOE        ANDEQ     wfdIOE
164200100127     C     UNIcli        ANDEQ     wfdCLI
164300070723     C     depANN        ANDEQ     wfdANN
164400070723     C     depMES        ANDEQ     wfdMES
164500070723      *
164600070723      *  Memorizza il primo Ypsilon trovato x cliente Anno
164700070723     C                   if        wfdCLN <> ' ' and Ypsilon_anno = *blank
164800070723     C                   eval      Ypsilon_anno = wfdCLN
164900070723     c                   end
165000070723      *
165100070723      * Tutto il mese cliente deve essere a "Y"
165200070723     C                   if        Ypsilon_anno = 'Y'
165300070723     c                   eval      wfdCLN = 'Y'
165400070723     c                   end
165500070723      *
165600070723      * Dal mese seguente deve impostare sempre la "S"
165700070723      *   anche per l'anno seguente...
165800070723     C                   if        Ypsilon_anno = 'S'
165900070723     c                   eval      wfdCLN = 'S'
166000070723     c                   end
166100070723      *
166200070723     c                   if        PARWfdpv = 'S'
166300100203     C                   UPDATE    WFdfp5
166400070723     c                   else
166500100127     C                   UPDATE    WFDEa5
166600070723     c                   end
166700070723     C*
166800070723     C* LETTURA SUCCESSIVA DATI CLIENTE
166900070723     c                   if        PARWfdpv = 'S'
167000110401     C                   READ      WAdfpV5L                               99
167100070723     c                   else
167200100127     C                   READ      WFDEa05L                               99
167300070723     c                   end
167400070723E   4C                   ENDDO                                                  *ROTTURA Mese
167500070723     C*
167600070723     C*  Se il mese letto aveva la "Y" i mesi seguenti entro l'anno devono avere la "S"
167700070723     C                   if        Ypsilon_anno = 'Y'
167800070723     C                   eval      Ypsilon_anno = 'S'
167900070723     c                   end
168000070723      *
168100070723E   3C                   ENDDO                                                  *ROTTURA CLIENTE/Anno
168200070723     C*
168300070723E   2C                   ENDDO
168400070723     C*
168500070723E   1C                   ENDDO
168600070727     C*
168700070727      *? ------------------------------------------------------------------------------------------------- ?
168800070727      *? ......Dopo avere reso omogeneo nell'ambito dell'anno la sequenza di "Y" e "S", si deve considerare    ?
168900070727      *? sull'anno in CORSO che, se un cliente aveva già del fatturato nell'anno precedente,                           ?
169000070727      *? NON può considerarsi Nuovo.!!!!!!                         ?
169100070727      *? ..... quindi, sul Cliente, nell'Anno in CORSO, si devono eliminare le "Y" e "S" su tutti i records.
169200070727      *? ------------------------------------------------------------------------------------------------- ?
169300070727      *  (esempio esplicativo)
169400070727      *                              ? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
169500070727      *? Unific.(C) Anno Precedente ? |   |   |   | Y | S | S | S | S | S | S | S | S |
169600070727      *? Unific.(C) Anno  in  CORSO ? |   | Y | S | S | S | S | S | S |   |   |   |   |
169700070727      *? -----------------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
169800070727      *? ============================     ?
169900070727      * --> Invece deve essere così
170000070727      *? ============================     ?
170100070727      *? Unific.(C) Anno Precedente   |   |   |   | Y | S | S | S | S | S | S | S | S |   ?-- records anno precedente
170200070727      *? Unific.(C) Anno  in  CORSO   |   |   |   |   |   |   |   |   |   |   |   |   |   ?-- records anno in Corso
170300070727      *? -------------------------------------------------------------------------------- ?
170400070727     C*   Con un istruzione SQL possiamo quindi sistemare la situazione sull'anno in corso
170500070727     C*   semplicemente controllando se nell'anno precedente il Cliente aveva del Fatturato
170600070727     C*   quindi, aggiorniamo tutti i Suoi records dell'Anno in corso x dire che NON è NUOVO.
170700070727      *
170800070727     C/EXEC SQL
170900100127     C+ UPDATE wfdea00f SET wfdcln = ' '
171000070727     C+ WHERE wfdann = :ParAAI AND wfdcln <>' ' AND wfdcli
171100100127     C+ IN (SELECT wfdcli FROM wfdea00f WHERE wfdann = :ParAFP)
171200070727     C/END-EXEC
171300070723     C*
171400121210     C*  Una volta sistemati tutti i records per l'anno in corso con solo la sequenza
171500121210     C*  giusta dal mese in cui nasce il cliente NUOVO.
171600121210     C*  Occorre sistemare eventuali saldi Antecedenti dovuti a storni o spalmature
171700121210     C*  di valori attribuiti al cliente UNIFICANTE....non potendo fare diversamente.
171800121207     C                   Exsr      NUOVO_CON_MESI
171900121207     C*
172000121207     C                   ENDSR
172100121207      *------------------------------------------------------------------------*
172200121207     C* Se L'AUT è considerato NUOVO nell'anno ed ha mesi precedenti con BLANK
172300121207      *------------------------------------------------------------------------*
172400121207     C     NUOVO_con_mesiBEGSR
172500121207     C*
172600121207     C*  se sono state fatte delle Note di Credito o altro tipo di Storni o Rettifiche
172700121210     C*  che vengono spalmate sui mesi precedenti a quello che ha la "Y" come mese iniziale.
172800121210     C*  Occorre ragionare impostando la "Y" anche su tali record ed in seguito fare
172900121210     C*  riferimento al mese più recente con la "Y" per conoscere qual'è il mese di VERO
173000121210     C*  inizio per il codice che si sta trattando.
173100121210     C*  Quindi sui mesi più vecchi ANTECEDENTI: verrà impostata la "Y" per poter
173200121210     C*  determinare nel programma TISE93R a rottura di cliente primo mese letto, se si
173300121210     C*  tratta di cliente NUOVO oppure acquisito.
173400121210     C*
173500121210     C/EXEC SQL
173600121210     C+ update  wfdea00f set  WFDCLN ='Y' WHERE wfdcln=' ' and
173700121210     C+ wfdcli in (select wfdcli from wfdea00f where wfdcln <> ' '
173800121210     C+ and wfdann = :PARAAI) and  wfdcli in  ( SELECT WFDCLI FROM
173900121210     C+ wfdea00f where wfdcli in (select wfdcli from wfdea00f
174000121210     C+ where wfdcln <> ' ' and wfdann = :PARAAI) and  wfdcln=' ')
174100121210     C/END-EXEC
174200121210     C*
174300121207     C                   ENDSR
174400121207     O*------------------------------------------------------------------------*
174500000000     O* STAMPA DEI CODICI AGENTI INESISTENTI O ANNULLATI DA INSERIRE IN SEDE   *
174600000000     O*------------------------------------------------------------------------*
174700000000     OQSYSPRT   E            TESTA            02
174800000000     O                       STA(1)              66
174900000000     O                       STA(2)             132
175000980306     O                       UDATE              119 '  /  /  '
175100000000     O                       PAGE          Z    132
175200000000     O*------------------------
175300000000     O          E            TESTA            03
175400000000     O                       STA(3)              66
175500000000     O                       STA(3)             132
175600000000     O*------------------------
175700000000     O          E            TESTA            04
175800000000     O                       STA(4)              66
175900000000     O*------------------------
176000000000     O          E            TESTA            05
176100000000     O                       STA(3)              66
176200000000     O                       STA(3)             132
176300000000     O*------------------------
176400000000     O          E            ERROR       1
176500000000     O                       DEPAGE               9
176600000000     O                       DEPKSC              27
176700100127     O                       UNIcli              47
176800000000     O*------------------------------------------------------------------------*
176900000000** STA
177000100127                           TISE92AR     ***  AGENTI INESISTENTI DA1
177100980306 MODIFICARE IN ANAGRAFICA CLIENTE  ***       99/99/99     PAG.99992
177200000000------------------------------------------------------------------3
177300000824A G E N T E      C L I E N T E     U N I F I C A N T E            4
177400000824  9999999           9999999             9999999                   5
