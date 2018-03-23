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
021000160713      *--------------------------------------
021100160713     d digits          c                   '0123456789'
021200000000     C*------------------------------------------------------------------------*
021300000000     C* MAIN LINE
021400000000     C*------------------------------------------------------------------------*
021500000000     C*
021600000000     C* LEGGO IL PRIMO CLIENTE VALIDO
021700000000     C                   Z-ADD     *ZEROS        KSOKSC
021800990609     C     KEYSDO        SETGT     SISDO01L
021900990609     C                   READ      SISDO01L                               99
022000000000     C*
022100000000     C* PER TUTTI I CLIENTI PRESENTI NEI SALDI
022200000000DO  1C     *IN99         DOWEQ     *OFF
022300980119     C                   Z-ADD     SDOKSC        DEPKSC                         *DEPOSITO CLIENTE
022400980119     C*
022500980119     C* CONTROLLA VALIDITA' RECORD
022600980119     C                   EXSR      CHKSDO
022700000000     C*
022800000000     C* OPERAZIONI PER NUOVO CLIENTE
022900980119IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
023000000000     C                   EXSR      NEWKSC
023100100215     C*
023200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
023300000000     C                   EXSR      SETFIL
023400000000     C*
023500000000     C* CICLO FINO A ROTTURA CODICE CLIENTE
023600980119DO  3C     $FINE         DOWEQ     '0'
023700000000     C*
023800000000     C* OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
023900000000     C                   EXSR      NEWDAT
024000100216     C*
024100100216     C* CREA I SALDI NEL WORK FILE x le VARIE
024200100216     c                   if        PARWfdpv <>'S'
024300100216     C                   EXSR      MEMSALVAR
024400100216     c                   end
024500000000     C*
024600000000     C* CICLO FINO A ROTTURA PERIODO SALDO CLIENTE
024700980119DO  4C     $FINE         DOWEQ     '0'
024800000000     C     SDCDAT        ANDEQ     DEPDAT
024900000000     C*
025000000000     C* CREA I SALDI NEL WORK FILE
025100000824     C                   EXSR      MEMSAL
025200000000     C*
025300000000     C* LEGGE IL RECORD SUCCESSIVO
025400000000     C                   EXSR      LETFIL
025500980119E   4C                   ENDDO                                                  *ROTTURA PERIODO
025600980119E   3C                   ENDDO                                                  *ROTTURA CLIENTE
025700000824E   2C                   ENDIF
025800000000     C*
025900000000     C* LEGGO IL SUCESSIVO CLIENTE VALIDO
026000000000     C                   Z-ADD     DEPKSC        KSOKSC
026100990609     C     KEYSDO        SETGT     SISDO01L
026200990609     C                   READ      SISDO01L                               99
026300000824E   1C                   ENDDO
026400000824     C*
026500000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
026600000825     C                   EXSR      AGGWFD
026700070723     C*
026800070723      *?reimposta correttamente il cliente Nuovo/Acquisito x Unificante...  ?
026900070723     c                   if        parSCE = 'U'
027000070723     C                   EXSR      AGGWFD_Uni
027100070723     c                   end
027200980403     C*
027300980403     C* OPERAZIONI FINALI
027400980403     C                   EXSR      FINSR
027500000000     C*
027600000000     C                   SETON                                        LR
027700000000     C*------------------------------------------------------------------------*
027800000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
027900000000     C*------------------------------------------------------------------------*
028000000000     C     NEWKSC        BEGSR
028100000824     C*
028200000824     C* IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-
028300000823IF  1C     PARSCE        IFEQ      'U'                                          *STAT PER UNIFICANTE
028400000823     C                   CLEAR                   BS10DS
028500001122     C*---               Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
028600010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
028700000823     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
028800000823     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
028900000823     C                   MOVE      DEPKSC        D10COD
029000000823     C                   CALL      'TIBS10R'
029100000823     C                   PARM                    BS10DS
029200100127     C                   Z-ADD     DEPKSC        UNIcli
029300000823IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
029400000823     C     D10COP        ANDNE     *ZEROS
029500100127     C                   Z-ADD     D10COP        UNIcli                         *UNIFICANTE
029600000823E   2C                   ENDIF
029700000823X   1C                   ELSE                                                   *STAT PER CLIENTE
029800100127     C                   Z-ADD     DEPKSC        UNIcli
029900000823E   1C                   ENDIF
030000000824     C*
030100000000     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE
030200100127     C                   MOVEL     UNIcli        DEPFIL                         *FIL+XXXX
030300000000     C                   Z-ADD     1             F
030400000000     C                   SETOFF                                           99
030500000000     C     DEPFIL        LOOKUP    FIL(F)                                 99
030600000000IF  1C     *IN99         IFEQ      *ON
030700000000     C                   Z-ADD     ARE(F)        DEPARE
030800000000     C                   MOVEL     DIV(F)        DEPDIV
030900000000X   1C                   ELSE
031000000000     C                   Z-ADD     *ZEROS        DEPARE
031100000000     C                   MOVEL     *BLANKS       DEPDIV
031200000000E   1C                   ENDIF
031300000824     C*
031400000824     C* REPERISCE IL COMMERCIALE, SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
031500980403     C                   CLEAR                   BS69DS
031600980403     C                   CLEAR                   CLPDS
031700980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
031800100127     C                   MOVEL     UNIcli        I69KCP                         *CLIENTE X CNCLP
031900980403     C                   CALL      'TIBS69R'
032000980403     C                   PARM                    BS69DS
032100980403     C                   PARM                    ACODS
032200980403     C                   PARM                    INDDS
032300980403     C                   PARM                    CLPDS
032400980403     C                   PARM                    CLSDS
032500980406IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
032600980403     C     CLPAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
032700100127     C                   MOVEL     UNIcli        NUM3              3 0          *CODICE FILIALE +
032800980403     C                   Z-ADD     *ZEROS        DEPAGE                         *AGENTE A ZERO  =
032900980403     C                   MOVEL     NUM3          DEPAGE                         *FIL+0000
033000980403X   1C                   ELSE                                                   *NO ERRORE
033100980403     C                   Z-ADD     CLPAGE        DEPAGE                         *CODICE AGENTE
033200980403E   1C                   ENDIF
033300000824     C*
033400000000     C* REPERISCE IL CODICE QUALITA'
033500000000     C                   MOVEL     'C'           DEPCLV
033600000000IF  1C     CLPCLV        IFNE      *BLANKS
033700000000     C                   MOVEL     CLPCLV        DEPCLV
033800000000E   1C                   ENDIF
033900000824     C*
034000000000     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
034100000000     C                   Z-ADD     1             C
034200000000     C                   SETOFF                                           99
034300000000     C     DEPAGE        LOOKUP    AGE(C)                                 99
034400000000IF  1C     *IN99         IFEQ      *OFF
034500000000     C                   EXSR      STAERR
034600000000     C                   Z-ADD     DEPAGE        DEPRGF                         RAGGRUPP = COD AGENT
034700000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
034800000000X   1C                   ELSE
034900000000IF  2C     RGF(C)        IFEQ      *ZEROS
035000000000     C                   Z-ADD     DEPAGE        DEPRGF
035100000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGRUP
035200000000X   2C                   ELSE
035300000000     C                   Z-ADD     RGF(C)        DEPRGF
035400000000     C*
035500000000     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
035600000000     C                   Z-ADD     1             C
035700000000     C                   SETOFF                                           99
035800000000     C     DEPRGF        LOOKUP    AGE(C)                                 99
035900000000IF  3C     *IN99         IFEQ      *ON
036000000000     C                   MOVEL     DAGE(C)       DEPRGD                         DESCRIZIONE RAGGR.TO
036100000000X   3C                   ELSE
036200000000     C                   MOVEL     *ALL'*'       DEPRGD                         DESCRIZIONE RAGGR.TO
036300000000E   3C                   ENDIF
036400000000E   2C                   ENDIF
036500000000E   1C                   ENDIF
036600000824     C*
036700000000     C* REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
036800000000     C                   Z-ADD     1             F
036900000000     C                   SETOFF                                           99
037000000000     C                   MOVEL     DEPRGF        NUM3                           *FIL+XXXX
037100000000     C     NUM3          LOOKUP    FIL(F)                                 99
037200000000IF  1C     *IN99         IFEQ      *ON
037300000000     C                   MOVEL     DIV(F)        DEPDIC
037400000000X   1C                   ELSE
037500000000     C                   MOVEL     *BLANKS       DEPDIC
037600000000E   1C                   ENDIF
037700000000     C*
037800000000     C                   ENDSR
037900000000     C*------------------------------------------------------------------------*
038000000000     C* NEWDAT - OPERAZIONI PER NUOVO PERIODO SALDO CLIENTE
038100000000     C*------------------------------------------------------------------------*
038200000000     C     NEWDAT        BEGSR
038300000000     C*
038400990609     C                   Z-ADD     SDCDAT        DEPDAT            6 0
038500000000     C*
038600000000     C* VEDO SE IL CLIENTE, NEL PERIODO, E' NUOVO O ACQUISITO
038700100127     C********           Z-ADD     UNIcli        KSOKSC
038800070720     C*
038900070720      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
039000070720     C                   Z-ADD     DEPksc        KSOKSC
039100070720     C*
039200000000     C                   Z-ADD     SDCANN        KSOANN
039300000000     C                   Z-ADD     SDCMES        KSOMES
039400990609     C     KE2SDO        CHAIN     SISDO01L                           99
039500000000IF  1C     *IN99         IFEQ      *OFF
039600070720     C                   MOVEL     SDOCLN        DEPCLN
039700000000X   1C                   ELSE
039800000000     C                   MOVEL     *BLANKS       DEPCLN
039900000000E   1C                   ENDIF
040000000000     C*
040100000000     C                   ENDSR
040200100127     C*------------------------------------------------------------------------*
040300100127     C* MEMSALVAR - MEMORIZZA I DATI
040400100127     C*------------------------------------------------------------------------*
040500100127     C     MEMSALvar     BEGSR
040600100127     C*
040700100128      *  ciclo sui saldi delle VARIE per riportarne i totali delle "e" e "h"
040800100127     C*
040900100127      *?  Deve farlo sul codice cliente non sull'Unificante...come faceva prima !!... ?
041000100127     C                   Z-ADD     DEPksc        KSOKSC
041100100216     C                   Z-ADD     SDcDAT        DEPDAT            6 0
041200100216     C                   Z-ADD     SDcANN        KSOANN
041300100216     C                   Z-ADD     SDcMES        KSOMES
041400100127     C     KE2SDO        setll     SISDp01L
041500100127     C     KE2SDO        reade     SISDp01L
041600100127     C*
041700100127     c                   dow       not %EoF(SISDp01L)
041800100127     C*
041900100128     C*  Solo legate alla Priority o H10:30
042000130131     C*      al 31/1/2013 viene chiesto di calcolare il FUEL per scorporarlo dai ricavi
042100130131     C*
042200130131     C*   PRO-MEMORIA: le varie non hanno alcuna possibilità di essere distinte
042300130131     C*                x Italia/Estero o x Tipo servizio o x Network....Quindi possono
042400130131     C*                essere calcolate solo a livello di TOTALE ossia ITALIA+ESTERO ed
042500130131     C*                esclusivamente per CLIENTE o CLIENTE UNIFICANTE
042600130131     C*                infatti anche già adesso avviene così per l'espresso/H10:30.
042700100128     c                   if        SDpSVR = 'e' or SDpSVR = 'h'
042800130131     c                                or SDpSVR = 'f'                            <--- VARIE (f=FUEL)
042900100128     C*
043000100127     C* CONTROLLA ESISTENZA RECORD DI LAVORO
043100100127     C                   Z-ADD     UNIcli        wfvksc                         *CLIENTE
043200100127     C                   Z-ADD     SDpANN        wfvANN                         *ANNO
043300100127     C                   Z-ADD     SDpMES        wfvMES                         *MESE
043400100127     C                   moveL     SDpSVR        wfvSVR                         *VARIA
043500100127     C     KEYDEAV       CHAIN     WFDEAV1L
043600100127     C***
043700100127     C* I M M I S S I O N E
043800100127     C***
043900100127IF  1C                   IF        not %Found(WFDEAV1L)
044000100127      * --
044100100127     c                   CLEAR                   WFDEAV00
044200100127     C                   Z-ADD     UNIcli        WFvksc                         *CLIENTE
044300100127     C                   Z-ADD     SDpANN        WFvANN                         *ANNO
044400100127     C                   Z-ADD     SDpMES        WFvMES                         *MESE
044500100127     C                   MOVEL     SDpSVR        WFvSVR                         *VARIA
044600100127     C                   Z-ADD     SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
044700100127     C                   z-add     SDpRCP        WFvRCP                         *Ricavi Particolari
044800100127      * --
044900100127     C                   WRITE     WFDEAV00
045000100127      * --
045100100127X   1C                   ELSEif    %Found(WFDEAV1L)
045200100127     C***
045300100127     C* V A R I A Z I O N E
045400100127     C***
045500100127     C                   add       SDpNSP        WFvNSP                         *NUMERO SPEDIZIONI
045600100127     C                   add       SDpRCP        WFvRCP                         *Ricavi Particolari
045700100127      * ---
045800100127     C                   UPDATE    WFDEAV00
045900100127E   1C                   ENDIF
046000100127     C*
046100100128     c                   end
046200100128     C*
046300100127     C     KE2SDO        reade     SISDp01L
046400100127E   1C                   ENDdo
046500100127     C*
046600100127     C                   ENDSR
046700980119     C*------------------------------------------------------------------------*
046800980119     C* CHKSDO - CONTROLLA VALIDITA' RECORD
046900980119     C*------------------------------------------------------------------------*
047000980119     C     CHKSDO        BEGSR
047100980119     C*
047200980119     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
047300980119     C*
047400980119     C                   ENDSR
047500000000     C*------------------------------------------------------------------------*
047600000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
047700000000     C*------------------------------------------------------------------------*
047800000000     C     SETFIL        BEGSR
047900000000     C*
048000000000     C* REIMPOSTO FLAG DI FINE LETTURA SALDI
048100000000     C                   MOVEL     '0'           $FINE
048200000000     C*
048300000000     C                   Z-ADD     DEPKSC        KSCKSC                         *CLIENTE
048400000000     C                   Z-ADD     PARAPI        KSCANN                         *ANNO PRECEDENTE INI
048500000000     C                   Z-ADD     PARMPI        KSCMES                         *MESE PRECEDENTE INI
048600000000     C*
048700000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO SALDO CLIENTE VALIDO
048800990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
048900000000     C*
049000000000IF  1C     *IN99         IFEQ      '1'
049100000000     C                   MOVEL     '1'           $FINE
049200000000X   1C                   ELSE
049300000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
049400000000E   1C                   ENDIF
049500000000     C*
049600000000     C                   ENDSR
049700000000     C*------------------------------------------------------------------------*
049800000000     C* LETFIL - LETTURA PROSSIMO RECORD SALDI CLIENTE
049900000000     C*------------------------------------------------------------------------*
050000000000     C     LETFIL        BEGSR
050100000000     C*
050200000000     C* LEGGO FINO A:
050300000000     C*     - FINE FILE
050400000000     C*     - TROVATO RECORD VALIDO
050500980601     C                   MOVEL     'N'           WRECOK
050600000000DO  1C     $FINE         DOWEQ     '0'
050700000000     C     WRECOK        ANDEQ     'N'
050800000000     C*
050900990609     C     KSCKSC        READE     SISDC01L                               99
051000000000     C*
051100000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
051200000000     C                   MOVEL     '1'           $FINE
051300000000X   2C                   ELSE
051400000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
051500000000E   2C                   ENDIF
051600000000     C*
051700000000E   1C                   ENDDO
051800000000     C*
051900000000     C                   ENDSR
052000000000     C*------------------------------------------------------------------------*
052100000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
052200000000     C*------------------------------------------------------------------------*
052300000000     C     CHKREC        BEGSR
052400000000     C*
052500980601     C                   MOVEL     'S'           WRECOK
052600020516     C*
052700020516     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
052800020516     C                   Z-ADD     1             G
052900020516     C                   SETOFF                                           28
053000020516     C     SDCRBL        LOOKUP    FLAB(G)                                28
053100020516IF  1C     *IN28         IFEQ      *OFF                                         *TIPO NON TROVATO --
053200020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
053300020516E   1C                   ENDIF
053400020516     C*
053500020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
053600020516IF  1C     WRECOK        IFEQ      'S'
053700020516IF  2C     SDCNSP        IFEQ      *ZEROS
053800020516     C     SDCPKG        ANDEQ     *ZEROS
053900020516     C     SDCPCC        ANDEQ     *ZEROS
054000020516     C     SDCPLS        ANDEQ     *ZEROS
054100020516     C     SDCNCO        ANDEQ     *ZEROS
054200020516     C     SDCVOL        ANDEQ     *ZEROS
054300020516     C     SDCVCC        ANDEQ     *ZEROS
054400020516     C     SDCVLS        ANDEQ     *ZEROS
054500020516     C     SDCIRB        ANDEQ     *ZEROS
054600020516     C     SDCIRR        ANDEQ     *ZEROS
054700020516     C     SDCINC        ANDEQ     *ZEROS
054800020516     C     SDCIRP        ANDEQ     *ZEROS
054900020516     C     SDCIRC        ANDEQ     *ZEROS
055000020516     C     SDCIRD        ANDEQ     *ZEROS
055100020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055200020516E   2C                   ENDIF
055300020516E   1C                   ENDIF
055400000000     C*--------------------
055500000000     C* ESCLUSIONI
055600000000     C*--------------------
055700970821     C                   Z-ADD     1             P
055800970821     C                   SETOFF                                           28
055900970821     C     SDCFPE        LOOKUP    FPE(P)                                 28
056000970821IF  1C     *IN28         IFEQ      *OFF                                         *NON TROVATO
056100970821     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056200970821E   1C                   ENDIF
056300000000     C*--------------------
056400000000     C* SELEZIONI
056500000000     C*--------------------
056600000000     C* SE I SALDI SONO DI DATA SUPERIORE A QUELLA RICHIESTA, ESCO DAL CICLO
056700000000IF  1C     SDCDAT        IFGT      PARDA2
056800000000     C                   MOVEL     'N'           WRECOK
056900000000     C                   MOVEL     '1'           $FINE
057000000000E   1C                   ENDIF
057100000000     C*
057200000000     C* SE TIPO SERVIZIO <> TIPO SERVIZIO RICHIESTO, ESCLUDO
057300100127IF  1C**** PARTSP        IFNE      *BLANKS                                      *NON TUTTI
057400100127     C**** SDCTSP        ANDNE     PARTSP
057500100127     C*********          MOVEL     'N'           WRECOK
057600100127E   1C*********          ENDIF
057700000000     C*
057800000000     C                   ENDSR
057900000000     C*------------------------------------------------------------------------*
058000000824     C* MEMSAL - MEMORIZZA I DATI
058100000000     C*------------------------------------------------------------------------*
058200000824     C     MEMSAL        BEGSR
058300070531     C*
058400070531     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
058500070531     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
058600070531     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
058700070531     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
058800070531     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
058900000000     C*
059000000000     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
059100000000     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
059200000000IF  1C     FSP(G)        IFNE      'S'
059300000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
059400070531     C                   z-add     0             sdcPKG                         *PESO
059500070531     C                   z-add     0             sdcPCC                         *PESO CON CML
059600070531     C                   z-add     0             sdcPLS                         *PESO CML
059700070531     C                   z-add     0             sdcVOL                         *VOLUME TOTALE
059800070531     C                   z-add     0             sdcVCC                         *VOLUME CON CML
059900070531     C                   z-add     0             sdcVLS                         *VOLUME CML
060000000000E   1C                   ENDIF
060100000000     C*
060200070531     C* imposta il Peso ed il Volume
060300070531     c                   z-add     sdcPKG        depPKG
060400070531     c                   z-add     sdcVOL        depVOL
060500070531     C*
060600000000     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
060700000000     C                   Z-ADD     *ZEROS        DEPRRN
060800000000     C     SDCIRD        ADD       SDCIRR        DEPRRN
060900000000     C     SDCINC        ADD       DEPRRN        DEPRRN
061000020314     C*
061100020314     C* DETERMINA IL NETWORK
061200020404     C                   MOVEL     *BLANKS       DEPNTWP           3
061300020404     C                   Z-ADD     *ZEROS        DEPORDP           3 0
061400020314     C                   Z-ADD     1             R
061500020314     C     SDCREP        LOOKUP    RGREG(R)                               55
061600020314     C                   IF        *IN55 = *ON
061700020404     C                   EVAL      DEPNTWP = RGNTW(R)
061800020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
061900020404     C                   Z-ADD     1             X
062000020404     C     DEPNTWP       LOOKUP    NTWCOD(X)                              55
062100020404     C                   IF        *IN55 = *ON
062200020404     C                   EVAL      DEPORDP = NTWORD(X)
062300020404     C                   ENDIF
062400020314     C                   ENDIF
062500020314     C*
062600020404     C                   MOVEL     *BLANKS       DEPNTWA           3
062700020404     C                   Z-ADD     *ZEROS        DEPORDA           3 0
062800020314     C                   Z-ADD     1             R
062900020314     C     SDCREA        LOOKUP    RGREG(R)                               55
063000020314     C                   IF        *IN55 = *ON
063100020404     C                   EVAL      DEPNTWA = RGNTW(R)
063200020404     C* REPERISCO L'ORDINE DI CONSIDERAZOINE DEL NETWORK
063300020404     C                   Z-ADD     1             X
063400020404     C     DEPNTWA       LOOKUP    NTWCOD(X)                              55
063500020404     C                   IF        *IN55 = *ON
063600020404     C                   EVAL      DEPORDA = NTWORD(X)
063700020404     C                   ENDIF
063800020314     C                   ENDIF
063900020404     C*
064000020404     C* IN BASE ALL'ORDINE DI CONSIDERAZIONE ATTRIBUISCO IL NETWORK DELLA SPEDIZIONE (ATTUALMENTE SOLO X ESTERO)
064100020404     C*
064200020404     C* 1) Italia su Italia
064300020404     C                   IF        DEPNTWP = *BLANKS AND
064400020404     C                             DEPNTWA = *BLANKS
064500020404     C                   EVAL      DEPNTW = *BLANKS
064600020404     C                   ELSE
064700020404     C* 2) Estero su Estero
064800020404     C                   IF        DEPNTWP <> *BLANKS AND
064900020404     C                             DEPNTWA <> *BLANKS
065000020404     C                   IF        DEPORDP < DEPORDA
065100020404     C                   EVAL      DEPNTW = DEPNTWP
065200020404     C                   ELSE
065300020404     C                   EVAL      DEPNTW = DEPNTWA
065400020404     C                   ENDIF
065500020404     C                   ELSE
065600020404     C* 3) Estero su Italia / Italia su Estero
065700020404     C                   IF        DEPNTWP <> *BLANKS
065800020404     C                   EVAL      DEPNTW = DEPNTWP
065900020404     C                   ELSE
066000020404     C                   EVAL      DEPNTW = DEPNTWA
066100020404     C                   ENDIF
066200020404     C                   ENDIF
066300020404     C                   ENDIF
066400020315     C*
066500020315     C* IMPOSTA SE FLAG ITALIA O ESTERO
066600020404     C     DEPNTWP       IFNE      *BLANKS
066700020404     C     DEPNTWA       ORNE      *BLANKS
066800020315     C                   MOVEL     'E'           DEPIOE                         *ESTERO
066900020315X   1C                   ELSE
067000020315     C                   MOVEL     'I'           DEPIOE                         *ITALIA
067100020315E   1C                   ENDIF
067200000000     C*
067300000000     C* CONTROLLA ESISTENZA RECORD DI LAVORO
067400000000     C                   MOVEL     DEPDIV        KWDDIV                         *DIVISIONE
067500000000     C                   Z-ADD     DEPARE        KWDARE                         *AREA
067600000000     C                   Z-ADD     DEPFIL        KWDFIL                         *FILIALE
067700000000     C                   Z-ADD     DEPAGE        KWDAGE                         *COMMERCIALE
067800000000     C                   MOVEL     DEPCLV        KWDCLV                         *CODICE QUALITA'
067900100127     C                   Z-ADD     UNIcli        KWDCLI                         *CLIENTE
068000000824     C                   Z-ADD     SDCANN        KWDANN                         *ANNO
068100000824     C                   Z-ADD     SDCMES        KWDMES                         *MESE
068200000824     C                   MOVEL     SDCFPE        KWDFPE                         *FASCIA DI PESO
068300970821     C                   MOVEL     DEPIOE        KWDIOE                         *ITALIA O ESTERO
068400020315     C                   MOVEL     DEPNTW        KWDNTW                         *NETWORK
068500100127     C                   MOVEL     SDCTSP        KWDTSP                         *Tipo Servizio
068600070531     c                   if        PARWfdpv = 'S'
068700110401     C     KEYDEL        CHAIN     WAdfpv1L                           99
068800070531     c                   else
068900100127     C     KEYDELtsp     CHAIN     WFDEA01L                           99
069000070531     c                   end
069100000000     C***
069200000000     C* I M M I S S I O N E
069300000000     C***
069400000000IF  1C     *IN99         IFEQ      '1'
069500100127     c                   CLEAR                   WFDEA000
069600000000     C                   MOVEL     DEPDIV        WFDDIV                         *DIVISIONE
069700000000     C                   Z-ADD     DEPARE        WFDARE                         *AREA
069800000000     C                   Z-ADD     DEPFIL        WFDFIL                         *FILIALE
069900000000     C                   Z-ADD     DEPAGE        WFDAGE                         *COMMERCIALE
070000000000     C                   MOVEL     DEPCLV        WFDCLV                         *CODICE QUALITA'
070100100127     C                   Z-ADD     UNIcli        WFDCLI                         *CLIENTE
070200000824     C                   Z-ADD     SDCANN        WFDANN                         *ANNO
070300000824     C                   Z-ADD     SDCMES        WFDMES                         *MESE
070400000824     C                   MOVEL     SDCFPE        WFDFPE                         *FASCIA DI PESO
070500000824     C                   MOVEL     DEPIOE        WFDIOE                         *ITALIA O ESTERO
070600000000     C                   Z-ADD     DEPRGF        WFDRGF                         *RAGGRUPP FIL AGENTE
070700000000     C                   MOVEL     DEPRGD        WFDRGD                         *DESCRIZ RAGGRUP.TO
070800000000     C                   MOVEL     DEPDIC        WFDDIC                         DIVISIONE COMMERCIAL
070900000000     C                   Z-ADD     SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
071000070531     C                   z-add     depPKG        WFDpkg                         *Peso
071100070531     C                   z-add     depVOL        WFDvol                         *Volume
071200000000     C                   Z-ADD     DEPRRN        WFDRRN                         RIC REALE NETTO DELT
071300000000     C                   Z-ADD     SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
071400070720      * --
071500000000     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
071600070720      * --
071700980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
071800020315     C                   MOVEL     DEPNTW        WFDNTW                         *NETWORK
071900100127      * --
072000100127     C                   move      SDCTSP        WFDTSP                         Tipo Servizio
072100100127      * --
072200070531     c                   if        PARWfdpv = 'S'
072300110401     C                   WRITE     Wfdfpv00
072400070531     c                   else
072500100127     C                   WRITE     WFDEA000
072600070531     c                   end
072700000000X   1C                   ELSE
072800000000     C***
072900000000     C* V A R I A Z I O N E
073000000000     C***
073100000000     C     WFDNSP        ADD       SDCNSP        WFDNSP                         *NUMERO SPEDIZIONI
073200070531     C     WFDpkg        add       depPKG        WFDpkg                         *Peso
073300070531     C     WFDvol        add       depVOL        WFDvol                         *Volume
073400000000     C     WFDRRN        ADD       DEPRRN        WFDRRN                         RIC REALE NETTO DELT
073500000000     C     WFDRPN        ADD       SDCIRP        WFDRPN                         RICAVO PRESUNTO DELT
073600980528     C                   MOVEL     *BLANKS       WFDSPD                         *SCOST.% DELTA FASCE
073700070720      * ---
073800070720      * solo x unificante
073900070720      *?  Attenzione: se lanciata x Unificante, occorre determinare se il codice  ?
074000070720      *?    unificante è nuovo o vecchio considerando tutti i suoi figli e,  ?
074100070821      *?    qualora uno di essi sia considerato vecchio, sarà quest'ultimo
074200070821      *?     a dare la caratteristica di vecchio al codice Unificante Padre.
074300070720      *?  >> SU SISDO l'info di Nuovo/Vecchio è sempre per singolo Cliente. <<
074400070723     c                   if        parSCE = 'U'
074500070723     c                   if        wfdCLN = *blank or
074600070723     c                             wfdCLN = 'Y' and DEPCLN = 'S'
074700070720     C                   MOVEL     DEPCLN        WFDCLN                         *CLIENTE NUOVO O ACQ
074800070720     c                   end
074900070723     c                   end
075000070720      * ---
075100070531     c                   if        PARWfdpv = 'S'
075200110401     C                   UPDATE    Wfdfpv00
075300070531     c                   else
075400100127     C                   UPDATE    WFDEA000
075500070531     c                   end
075600000000E   1C                   ENDIF
075700000000     C*
075800000000     C                   ENDSR
075900000824     C*------------------------------------------------------------------------*
076000000825     C* LEGGE TUTTO IL WFILE E LO AGGIORNA IN CONSIDERAZIONE DELLE RICHIESTE FATTE
076100000824     C*------------------------------------------------------------------------*
076200000825     C     AGGWFD        BEGSR
076300000825     C*
076400000825     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
076500070531     c                   if        PARWfdpv = 'S'
076600110401     C     *LOVAL        SETLL     WAdfpv4L
076700110401     C                   READ(N)   WAdfpv4L                               99
076800070531     c                   else
076900100127     C     *LOVAL        SETLL     WFDEa04L
077000100127     C                   READ(N)   WFDEa04L                               99
077100070531     c                   end
077200000825     C*
077300000825     C* CICLO FINO A FINE FILE
077400000825DO  1C     *IN99         DOWEQ     *OFF
077500000825     C*
077600000825     C* OPERAZIONE PER NUOVO CLIENTE
077700000825     C                   EXSR      NEWCLI
077800000825     C*
077900000825     C* CICLO FINO A ROTTURA CLIENTE
078000000825DO  2C     *IN99         DOWEQ     *OFF
078100100127     C     UNIcli        ANDEQ     WFDCLI
078200070723      *
078300070723      *
078400000825IF  3C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
078500000825     C     WFDDAT        ANDLE     PARDA2
078600100127     C* MEMORIZZA I DATI -WFDEA-
078700000825     C                   EXSR      MEMWFD
078800000825E   3C                   ENDIF
078900000825     C*
079000000825     C* LETTURA SUCCESSIVA DATI CLIENTE
079100070531     c                   if        PARWfdpv = 'S'
079200110401     C                   READ(N)   WAdfpv4L                               99
079300070531     c                   else
079400100127     C                   READ(N)   WFDEa04L                               99
079500070531     c                   end
079600070723     C*
079700000825E   2C                   ENDDO                                                  *ROTTURA CLIENTE
079800000825     C*
079900000825     C* CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
080000000825     C                   EXSR      MODCLI
080100000825     C*
080200000825     C* SI RIPOSIZIONA E LEGGE I DATI DEL CLIENT SUCCESSIVO
080300070531     c                   if        PARWfdpv = 'S'
080400110401     C     UNIcli        SETGT     WAdfpv4L
080500110401     C                   READ(N)   WAdfpv4L                               99
080600070531     c                   else
080700100127     C     UNIcli        SETGT     WFDEa04L
080800100127     C                   READ(N)   WFDEa04L                               99
080900070531     c                   end
081000000825E   1C                   ENDDO
081100000824     C*
081200000824     C                   ENDSR
081300000825     C*------------------------------------------------------------------------*
081400100127     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE -WFDEA-
081500000825     C*------------------------------------------------------------------------*
081600000825     C     NEWCLI        BEGSR
081700000825     C*
081800000825     C* MEMORIZZA IL NUOVO CLIENTE IN UN DEPOSITO
081900100127     C                   Z-ADD     WFDCLI        UNIcli
082000000825     C*
082100000825     C* AZZERA LE VARIABILI DI LAVORO
082200020116     C                   Z-ADD     *ZEROS        TOTIRD           19 5          *RICAVI
082300020116     C                   Z-ADD     *ZEROS        TOTIRP           19 5
082400020116     C                   Z-ADD     *ZEROS        TOTERD           19 5
082500020116     C                   Z-ADD     *ZEROS        TOTERP           19 5
082600020116     C                   Z-ADD     *ZEROS        TOTTRD           19 5
082700020116     C                   Z-ADD     *ZEROS        TOTTRP           19 5
082800000825     C                   Z-ADD     *ZEROS        PERDEI            4 1          *% DELTA
082900000825     C                   Z-ADD     *ZEROS        PERDEE            4 1
083000000825     C                   Z-ADD     *ZEROS        PERDET            4 1
083100000825     C                   MOVEL     'S'           $OKDEI            1            *DELTA OK
083200000825     C                   MOVEL     'S'           $OKDEE            1
083300000825     C                   MOVEL     'S'           $OKDET            1
083400000825     C                   CLEAR                   SMEI                           *MM CON % SEGNALARE
083500000825     C                   CLEAR                   SMEE
083600000825     C                   CLEAR                   SMET
083700000825     C                   Z-ADD     *ZEROS        MI                2 0          *INDICE MESI
083800000825     C                   Z-ADD     *ZEROS        ME                2 0
083900000825     C                   Z-ADD     *ZEROS        MT                2 0
084000000825     C*
084100000825     C                   ENDSR
084200000825     C*------------------------------------------------------------------------*
084300000825     C* MEMWFD - MEMORIZZA I DATI DEL WFILE
084400000825     C*------------------------------------------------------------------------*
084500000825     C     MEMWFD        BEGSR
084600000825     C
084700000825     C* MEMORIZZA I RICAVI ITALIA E ESTERO DEL CLIENTE PER IL PERIODO RICHIESTO
084800000825IF  1C     WFDIOE        IFEQ      'I'                                          *ITALIA
084900000825     C                   ADD       WFDRRN        TOTIRD                         *TOT RIC REALE
085000000825     C                   ADD       WFDRPN        TOTIRP                         *TOT RIC PRESUNTO
085100000825E   1C                   ENDIF
085200000825IF  1C     WFDIOE        IFEQ      'E'                                          *ESTERO
085300000825     C                   ADD       WFDRRN        TOTERD                         *TOT RIC REALE
085400000825     C                   ADD       WFDRPN        TOTERP                         *TOT RIC PRESUNTO
085500000825E   1C                   ENDIF
085600000825IF  1C     WFDIOE        IFEQ      *BLANKS                                      *ITALIA + ESTERO
085700000825     C                   ADD       WFDRRN        TOTTRD                         *TOT RIC REALE
085800000825     C                   ADD       WFDRPN        TOTTRP                         *TOT RIC PRESUNTO
085900000825E   1C                   ENDIF
086000000825     C*
086100000825     C* CONTROLLA SE LA % DELTA DI CIASCUNA FASCIA DI PESO RIENTRA IN QUELLA RICHIESTA
086200000825IF  1C     PARSPD        IFNE      *ZEROS                                       *RICHIESTA
086300000825     C*
086400000825     C* ITALIA
086500000825IF  2C     WFDIOE        IFEQ      'I'
086600000825IF  3C     WFDRRN        IFGT      *ZEROS
086700000825     C     WFDRPN        ORGT      *ZEROS
086800020116     C                   Z-ADD     WFDRRN        WRRW             17 5          *RICAVO REALE
086900020116     C                   Z-ADD     WFDRPN        WRPW             17 5          *RICAVO PRESUNTO
087000000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
087100000825     C                   EXSR      CALDEL
087200000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
087300000825     C                   SETOFF                                           97
087400000825     C     WFDMES        LOOKUP    SMEI                                   97
087500000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
087600000825     C                   ADD       1             MI
087700000825     C                   Z-ADD     WFDMES        SMEI(MI)                       *MESE DA SEGNALARE
087800000825E   5C                   ENDIF
087900000825E   4C                   ENDIF
088000000825E   3C                   ENDIF
088100000825E   2C                   ENDIF
088200000825     C* ESTERO
088300000825IF  2C     WFDIOE        IFEQ      'E'
088400000825IF  3C     WFDRRN        IFGT      *ZEROS
088500000825     C     WFDRPN        ORGT      *ZEROS
088600020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
088700020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
088800000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
088900000825     C                   EXSR      CALDEL
089000000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
089100000825     C                   SETOFF                                           97
089200000825     C     WFDMES        LOOKUP    SMEE                                   97
089300000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
089400000825     C                   ADD       1             ME
089500000825     C                   Z-ADD     WFDMES        SMEE(ME)                       *MESE DA SEGNALARE
089600000825E   5C                   ENDIF
089700000825E   4C                   ENDIF
089800000825E   3C                   ENDIF
089900000825E   2C                   ENDIF
090000000825     C* ITALIA + ESTERO
090100000825IF  2C     WFDIOE        IFEQ      *BLANKS
090200000825IF  3C     WFDRRN        IFGT      *ZEROS
090300000825     C     WFDRPN        ORGT      *ZEROS
090400020116     C                   Z-ADD     WFDRRN        WRRW                           *RICAVO REALE
090500020116     C                   Z-ADD     WFDRPN        WRPW                           *RICAVO PRESUNTO
090600000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
090700000825     C                   EXSR      CALDEL
090800000825IF  4C     WDPW          IFLE      MENSPD                                       *MINORE VALORE MIN
090900000825     C                   SETOFF                                           97
091000000825     C     WFDMES        LOOKUP    SMET                                   97
091100000828IF  5C     *IN97         IFEQ      *OFF                                         *MAI INSERITO
091200000825     C                   ADD       1             MT
091300000825     C                   Z-ADD     WFDMES        SMET(MT)                       *MESE DA SEGNALARE
091400000825E   5C                   ENDIF
091500000825E   4C                   ENDIF
091600000825E   3C                   ENDIF
091700000825E   2C                   ENDIF
091800000825     C*
091900000825E   1C                   ENDIF
092000000825     C*
092100000825     C                   ENDSR
092200000825     C*------------------------------------------------------------------------*
092300000825     C* MODCLI - CANCELLA E/O AGGIORNA I DATI DEL CLIENTE PER % DELTA RICHIESTE
092400000825     C*------------------------------------------------------------------------*
092500000825     C     MODCLI        BEGSR
092600000825     C*
092700000825     C* CALCOLA IL DELTA DEL CLIENTE
092800000825     C                   EXSR      CALCLI
092900000825     C*
093000000825     C* RILEGGE I DATI DEL CLIENTE -->
093100000825     C* - CANCELLA IL CLIENTE SE NON RIENTRA NEL RANGE DELTA RICHIESTO
093200000825     C* - AGGIORNA IL CLIENTE SE HA UNA % DELTA FASCE DA SEGNALARE
093300070531     c                   if        PARWfdpv = 'S'
093400110401     C     UNIcli        SETLL     WAdfpv4L
093500110401     C     UNIcli        READE     WAdfpv4L                               98
093600070531     c                   else
093700100127     C     UNIcli        SETLL     WFDEa04L
093800100127     C     UNIcli        READE     WFDEa04L                               98
093900070531     c                   end
094000070531      *
094100000825DO  1C     *IN98         DOWEQ     *OFF
094200000825     C*
094300000825     C* ITALIA
094400000825IF  2C     WFDIOE        IFEQ      'I'
094500000825IF  3C     $OKDEI        IFEQ      'N'                                          *DELTA NON OK
094600070531     c                   if        PARWfdpv = 'S'
094700100203     C                   DELETE    WFDfpv
094800070531     c                   else
094900100127     C                   DELETE    WFDEa4
095000070531     c                   end
095100000825X   3C                   ELSE                                                   *DELTA OK
095200000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
095300000828     C     WFDDAT        ANDLE     PARDA2
095400000825     C                   SETOFF                                           97
095500000825     C     WFDMES        LOOKUP    SMEI                                   97
095600000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
095700000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
095800070531     c                   if        PARWfdpv = 'S'
095900100203     C                   UPDATE    WFDfpv
096000070531     c                   else
096100100127     C                   UPDATE    WFDEa4
096200070531     c                   end
096300000828E   5C                   ENDIF
096400000828E   4C                   ENDIF
096500000825E   3C                   ENDIF
096600000825E   2C                   ENDIF
096700000825     C*
096800000825     C* ESTERO
096900000825IF  2C     WFDIOE        IFEQ      'E'
097000000825IF  3C     $OKDEE        IFEQ      'N'                                          *DELTA NON OK
097100070531     c                   if        PARWfdpv = 'S'
097200100203     C                   DELETE    WFDfpv
097300070531     c                   else
097400100127     C                   DELETE    WFDEa4
097500070531     c                   end
097600000825X   3C                   ELSE                                                   *DELTA OK
097700000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
097800000828     C     WFDDAT        ANDLE     PARDA2
097900000825     C                   SETOFF                                           97
098000000825     C     WFDMES        LOOKUP    SMEE                                   97
098100000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
098200000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
098300070531     c                   if        PARWfdpv = 'S'
098400100203     C                   UPDATE    WFDfpv
098500070531     c                   else
098600100127     C                   UPDATE    WFDEa4
098700070531     c                   end
098800000828E   5C                   ENDIF
098900000825E   4C                   ENDIF
099000000825E   3C                   ENDIF
099100000825E   2C                   ENDIF
099200000825     C*
099300000825     C* ITALIA + ESTERO
099400000825IF  2C     WFDIOE        IFEQ      *BLANKS
099500000825IF  3C     $OKDET        IFEQ      'N'                                          *DELTA NON OK
099600070531     c                   if        PARWfdpv = 'S'
099700100203     C                   DELETE    WFDfpv
099800070531     c                   else
099900100127     C                   DELETE    WFDEa4
100000070531     c                   end
100100000825X   3C                   ELSE                                                   *DELTA OK
100200000828IF  4C     WFDDAT        IFGE      PARDAT                                       *DEL PERIODO CHIESTO
100300000828     C     WFDDAT        ANDLE     PARDA2
100400000825     C                   SETOFF                                           97
100500000825     C     WFDMES        LOOKUP    SMET                                   97
100600000828IF  5C     *IN97         IFEQ      *ON                                          *DA SEGNALARE
100700000825     C                   MOVEL     '*'           WFDSPD                         *SEGNALAZIONE
100800070531     c                   if        PARWfdpv = 'S'
100900100203     C                   UPDATE    WFDfpv
101000070531     c                   else
101100100127     C                   UPDATE    WFDEa4
101200070531     c                   end
101300000828E   5C                   ENDIF
101400000825E   4C                   ENDIF
101500000825E   3C                   ENDIF
101600000825E   2C                   ENDIF
101700000825     C*
101800070531     c                   if        PARWfdpv = 'S'
101900110401     C     UNIcli        READE     WAdfpv4L                               98
102000070531     c                   else
102100100127     C     UNIcli        READE     WFDEa04L                               98
102200070531     c                   end
102300000825E   1C                   ENDDO                                                  *DINE DATI CLIENTE
102400000825     C*
102500000825     C                   ENDSR
102600000825     C*------------------------------------------------------------------------*
102700000825     C* CALCOLA I DELTA SUI DATI MEMORIZZATI
102800000825     C*------------------------------------------------------------------------*
102900000825     C     CALCLI        BEGSR
103000000825     C*
103100000825     C* CALCOLA I DELTA E CONTROLLA SE RIENTRANO NEL RANGE RICHIESTO
103200000825IF  1C     PARPDD        IFNE      -999,9                                       *RICHIESTO
103300000825     C     PARPDA        ORNE      +999,9
103400000825     C*
103500000825     C* DELTA ITALIA
103600000825IF  2C     TOTIRD        IFGT      *ZEROS
103700000825     C     TOTIRP        ORGT      *ZEROS
103800020116     C                   Z-ADD     TOTIRD        WRRW                           *RICAVO REALE
103900020116     C                   Z-ADD     TOTIRP        WRPW                           *RICAVO PRESUNTO
104000000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
104100000825     C                   EXSR      CALDEL
104200000825     C                   Z-ADD     WDPW          PERDEI                         *% DELTA ITALIA
104300000825IF  3C     PERDEI        IFLT      PARPDD                                       *DELTA <> RANGE
104400000825     C     PERDEI        ORGT      PARPDA
104500000825     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
104600000825E   3C                   ENDIF
104700010313X   2C                   ELSE                                                   *NO RICAVI ITALIA
104800010313     C                   MOVEL     'N'           $OKDEI                         *DELTA NON OK
104900000825E   2C                   ENDIF
105000000825     C* DELTA ESTERO
105100000825IF  2C     TOTERD        IFGT      *ZEROS
105200000825     C     TOTERP        ORGT      *ZEROS
105300020116     C                   Z-ADD     TOTERD        WRRW                           *RICAVO REALE
105400020116     C                   Z-ADD     TOTERP        WRPW                           *RICAVO PRESUNTO
105500000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
105600000825     C                   EXSR      CALDEL
105700000825     C                   Z-ADD     WDPW          PERDEE                         *% DELTA ESTERO
105800000825IF  3C     PERDEE        IFLT      PARPDD                                       *DELTA <> RANGE
105900000825     C     PERDEE        ORGT      PARPDA
106000000825     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
106100000825E   3C                   ENDIF
106200010313X   2C                   ELSE                                                   *NO RICAVI ESTERO
106300010313     C                   MOVEL     'N'           $OKDEE                         *DELTA NON OK
106400000825E   2C                   ENDIF
106500000825     C* DELTA ITA + EST
106600000825IF  2C     TOTTRD        IFGT      *ZEROS
106700000825     C     TOTTRP        ORGT      *ZEROS
106800020116     C                   Z-ADD     TOTTRD        WRRW                           *RICAVO REALE
106900020116     C                   Z-ADD     TOTTRP        WRPW                           *RICAVO PRESUNTO
107000000825     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
107100000825     C                   EXSR      CALDEL
107200000825     C                   Z-ADD     WDPW          PERDET                         *% DELTA ESTERO
107300000825IF  3C     PERDET        IFLT      PARPDD                                       *DELTA <> RANGE
107400000825     C     PERDET        ORGT      PARPDA
107500000825     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
107600000825E   3C                   ENDIF
107700010313X   2C                   ELSE                                                   *NO RICAVI TOTALI
107800010313     C                   MOVEL     'N'           $OKDET                         *DELTA NON OK
107900000825E   2C                   ENDIF
108000000825     C*
108100000825E   1C                   ENDIF
108200000825     C*
108300000825     C                   ENDSR
108400000824     C*------------------------------------------------------------------------*
108500000824     C* CALDEL - CALCOLA IL DELTA
108600000824     C*          INPUT  --> WRRW: RICAVO REALE
108700970821     C*                     WRPW: RICAVO PRESUNTO
108800970821     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
108900970821     C*------------------------------------------------------------------------*
109000970821     C     CALDEL        BEGSR
109100970821     C*--------------------
109200970821     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
109300970821     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
109400970821     C*--------------------
109500970821     C*
109600970821     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
109700020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
109800970821     C     GAP           MULT      100           GAP                            (GAP x 100)
109900970821     C*
110000970821     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
110100970821IF  1C     WRRW          IFEQ      *ZEROS
110200000825     C                   Z-ADD     1             WRRW
110300970821E   1C                   ENDIF
110400970821     C*
110500970821     C     GAP           DIV       WRRW          GAP
110600000825     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
110700970821     C*
110800970821     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
110900970821     C     WDPW          COMP      0                                    28
111000970821     C* POSITIVO
111100970821IF  1C     *IN28         IFEQ      *OFF
111200970821IF  2C     GAP           IFGE      1000
111300970821     C                   Z-ADD     +999,9        WDPW
111400970821E   2C                   ENDIF
111500970821     C* NEGATIVO
111600970821X   1C                   ELSE
111700970821IF  2C     GAP           IFLE      -1000
111800970821     C                   Z-ADD     -999,9        WDPW
111900970821E   2C                   ENDIF
112000970821E   1C                   ENDIF
112100970821     C*
112200970821     C                   ENDSR
112300000000     C*------------------------------------------------------------------------*
112400000000     C* STAERR - STAMPA RIGA DI ERRORE PER AGENTE INESISTENTE O ANNULLATO
112500000000     C*------------------------------------------------------------------------*
112600000000     C     STAERR        BEGSR
112700000000     C*
112800000000IF  1C     *INOF         IFEQ      *ON
112900000000     C                   EXCEPT    TESTA
113000000000     C                   SETOFF                                       OF
113100000000E   1C                   ENDIF
113200000000     C*
113300000000     C* AGENTE NON ANCORA STAMPATO, SCRIVO ERRORE
113400000000     C                   SETOFF                                       97
113500000000     C     DEPAGE        LOOKUP    ERG                                    97
113600000000IF  1C     *IN97         IFEQ      *OFF
113700000000     C                   ADD       1             I                 3 0
113800000000     C                   Z-ADD     DEPAGE        ERG(I)                         *MEMORIZZA AGENTE
113900000000     C                   EXCEPT    ERROR                                        *STAMPA L'ERRORE
114000000000E   1C                   ENDIF
114100000000     C*
114200000000     C                   ENDSR
114300000000     C*------------------------------------------------------------------------*
114400000000     C* CARTAB - CARICA I DATI IN TABELLE
114500000000     C*------------------------------------------------------------------------*
114600000000     C     CARTAB        BEGSR
114700000000     C*--------------------
114800000000     C* ORGANIGRAMMA
114900000000     C*--------------------
115000000000     C                   Z-ADD     *ZEROS        F
115100000000     C                   Z-ADD     *ZEROS        KORFIL
115200000000     C     KEYORG        SETLL     AZORG01L
115300000000     C                   READ      AZORG01L                               99
115400000000DO  1C     *IN99         DOWEQ     '0'
115500980119IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
115600980119IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
115700000000     C                   ADD       1             F
115800000000     C                   Z-ADD     ORGFIL        FIL(F)
115900000000     C                   MOVEL     ORGDES        DFIL(F)
116000020906     C                   Z-ADD     1             KTBKUT
116100000000     C                   MOVEL     '05'          KTBCOD
116200000000     C                   MOVEL     *BLANKS       KTBKEY
116300000000     C                   MOVEL     ORGCAR        KTBKEY
116400000824     C     KEYTAB        CHAIN     TABEL00F                           99
116500980119IF  5C     *IN99         IFEQ      *OFF
116600000000     C                   Z-ADD     ORGCAR        ARE(F)
116700000000     C                   MOVEL     TBLUNI        DARE(F)
116800980119E   5C                   ENDIF
116900020906     C                   Z-ADD     1             KTBKUT
117000000000     C                   MOVEL     '17'          KTBCOD
117100000000     C                   MOVEL     *BLANKS       KTBKEY
117200000000     C                   MOVEL     ORGFL3        KTBKEY
117300000824     C     KEYTAB        CHAIN     TABEL00F                           99
117400980119IF  5C     *IN99         IFEQ      *OFF
117500000000     C                   MOVEL     ORGFL3        DIV(F)
117600000000     C                   MOVEL     TBLUNI        DDIV(F)
117700980119E   5C                   ENDIF
117800980119E   3C                   ENDIF
117900980119E   2C                   ENDIF
118000000000     C                   READ      AZORG01L                               99
118100000000E   1C                   ENDDO
118200000000     C*--------------------
118300000000     C* COMMERCIALI
118400000000     C*--------------------
118500140612     C********           Z-ADD     *ZEROS        C
118600140612     C********           Z-ADD     1             KTBKUT
118700140612     C********           MOVEL     '01'          KTBCOD
118800140612     C***  KEYTA2        SETLL     TABEL00F
118900140612     C***  KEYTA2        READE     TABEL00F                               99
119000140612DO  1C***  *IN99         DOWEQ     *OFF
119100140612IF  2C***  TBLFLG        IFEQ      *BLANKS
119200140612     C********           ADD       1             C
119300140612     C********           MOVEL     TBLUNI        DS01
119400140612     C********           MOVEL     TBLKEY        AGE(C)
119500140612     C********           MOVEL     §01RGF        RGF(C)
119600140612     C********           MOVEL     §01AGE        DAGE(C)
119700140612E   2C********           ENDIF
119800140612     C***  KEYTA2        READE     TABEL00F                               99
119900140612E   1C********           ENDDO
120000140612      *********
120100140612     C                   Z-ADD     *ZEROS        C
120200140612     c                   open      azCMM01l
120300140612     C     *loval        SETLL     azCMM01l
120400140612     C                   READ      azCMM01l
120500140612DO  1C                   DOW       not %Eof(azCMM01l)
120600140612IF  2C                   IF        cmmATB = *blank
120700140612     C                   ADD       1             C
120800140612     C                   z-add     cmmCOD        AGE(C)
120900140612     C                   z-add     cmmUNI        RGF(C)
121000140612     C                   MOVEL     cmmDES        DAGE(C)
121100140612E   2C                   ENDIF
121200140612     C                   READ      azCMM01l
121300140612E   1C                   ENDDO
121400140612     c                   close     azCMM01l
121500000000     C*-------------------------
121600000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
121700000000     C*-------------------------
121800000000     C                   Z-ADD     *ZEROS        G
121900020906     C                   Z-ADD     1             KTBKUT
122000000000     C                   MOVEL     '2W'          KTBCOD
122100000000     C     KEYTA2        SETLL     TABEL00F
122200000824     C     KEYTA2        READE     TABEL00F                               99
122300000000DO  1C     *IN99         DOWEQ     *OFF
122400000000IF  2C     TBLFLG        IFEQ      *BLANKS
122500000000     C                   ADD       1             G
122600000000     C                   MOVEL     TBLKEY        FLAB(G)
122700000000     C                   MOVEL     TBLUNI        DS2W
122800000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
122900000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
123000000000E   2C                   ENDIF
123100000824     C     KEYTA2        READE     TABEL00F                               99
123200000000E   1C                   ENDDO
123300990217     C*--------------------
123400000000     C* FASCE DI PESO
123500990217     C*--------------------
123600990609IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
123700990217     C                   MOVEL     'SDC'         WSX               3
123800990217X   1C                   ELSE                                                   *FASCE DAL '99
123900020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO AL 2001
124000020325     C                   MOVEL     'S9C'         WSX
124100020325X   2C                   ELSE                                                   *FASCE DAL 2002
124200020325     C                   MOVEL     'S2C'         WSX
124300020325E   2C                   ENDIF
124400020325E   1C                   ENDIF
124500970821     C                   Z-ADD     *ZEROS        P
124600020906     C                   Z-ADD     1             KTBKUT
124700000000     C                   MOVEL     '2L'          KTBCOD
124800000000     C     KEYTA2        SETLL     TABEL00F
124900000824     C     KEYTA2        READE     TABEL00F                               99
125000000000DO  1C     *IN99         DOWEQ     *OFF
125100000000IF  2C     TBLFLG        IFEQ      *BLANKS
125200990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
125300000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
125400990217IF  3C     FILEST        IFEQ      WSX
125500000000     C                   MOVEL     TBLUNI        DS2L
125600000309IF  4C     §2LKGI        IFGE      5000
125700000309     C     PARDAT        ANDLT     200001
125800000309     C                   ELSE
125900970821     C                   ADD       1             P
126000970821     C                   MOVE      STACOD        FPE(P)                         *...+XX
126100000309E   4C                   ENDIF
126200000000E   3C                   ENDIF
126300000000E   2C                   ENDIF
126400000824     C     KEYTA2        READE     TABEL00F                               99
126500000000E   1C                   ENDDO
126600020314     C*-------------------------
126700020314     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
126800020314     C*-------------------------
126900020314     C                   Z-ADD     *ZEROS        R
127000020906     C                   Z-ADD     1             KTBKUT
127100020314     C                   MOVEL     '2M'          KTBCOD
127200020314     C     KEYTA2        SETLL     TABEL00F
127300020314     C     KEYTA2        READE     TABEL00F                               99
127400020314DO  1C     *IN99         DOWEQ     *OFF
127500020314IF  2C     TBLFLG        IFEQ      *BLANKS
127600020314     C                   ADD       1             R
127700020314     C                   MOVEL     TBLKEY        RGREG(R)
127800020314     C                   MOVEL     TBLUNI        DS2M
127900020314     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
128000020314E   2C                   ENDIF
128100020314     C     KEYTA2        READE     TABEL00F                               99
128200020314E   1C                   ENDDO
128300020404     C*----------
128400020404     C* NETWORK AZIENDALI
128500020404     C*----------
128600020404     C                   Z-ADD     *ZEROS        X
128700020404     C                   EVAL      TBECOD = 'NTW'
128800020404     C     KEYTBE        SETLL     TNTBE01L
128900020404     C     KEYTBE        READE     TNTBE01L
129000020404     C                   DOW       NOT %EOF(TNTBE01L)
129100020404     C                   ADD       1             X
129200020404     C                   MOVEL     TBEUNI        DNTW
129300020404     C                   EVAL      NTWCOD(X) = TBEKE1
129400020404     C                   EVAL      NTWORD(X) = §NTWORD
129500020404     C     KEYTBE        READE     TNTBE01L
129600020404     C                   ENDDO
129700000000     C*
129800000000     C                   ENDSR
129900970821     C*------------------------------------------------------------------------*
130000970821     C* AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
130100970821     C*------------------------------------------------------------------------*
130200970821     C     AMELAB        BEGSR
130300970821     C*---
130400970821     C* ANNO INIZIALE UGUALE AD ANNO FINALE
130500970821     C*---
130600970821IF  1C     WAA2          IFEQ      WAA
130700970821     C*
130800970821     C* CALCOLO I MESI DI DIFFERENZA
130900970821DO  2C     WMM           DOWLE     WMM2
131000970821     C                   ADD       1             JI
131100970821     C                   Z-ADD     WAA           ANN(JI)
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
132200970821     C*---
132300970821     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
132400970821     C*---
132500970821X   1C                   ELSE
132600970821     C*
132700970821     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
132800970821DO  2C     WMM           DOWLE     12
132900970821     C                   ADD       1             JI
133000970821     C                   Z-ADD     WAA           ANN(JI)
133100970821     C                   Z-ADD     WMM           MES(JI)
133200970821     C                   ADD       1             WMM
133300970821E   2C                   ENDDO
133400970821     C*
133500970821     C* MEMORIZZO I MESI DELL'ANNO FINALE
133600970821     C                   Z-ADD     01            WMM
133700970821DO  2C     WMM           DOWLE     WMM2
133800970821     C                   ADD       1             JI
133900970821     C                   Z-ADD     WAA2          ANN(JI)
134000970821     C                   Z-ADD     WMM           MES(JI)
134100970821     C                   ADD       1             WMM
134200970821E   2C                   ENDDO
134300970821     C*
134400970821     C* IL RESTO DELLA SCHIERA A ZERO
134500970821DO  2C     JI            DOWLT     12
134600970821     C                   ADD       1             JI
134700970821     C                   Z-ADD     *ZEROS        ANN(JI)
134800970821     C                   Z-ADD     *ZEROS        MES(JI)
134900970821E   2C                   ENDDO
135000970821E   1C                   ENDIF
135100970821     C*
135200970821     C                   ENDSR
135300980403     C*------------------------------------------------------------------------*
135400980403     C* FINSR - OPERAZIONI FINALI
135500980403     C*------------------------------------------------------------------------*
135600980403     C     FINSR         BEGSR
135700980403     C*
135800980403     C* CHIUDE I FILE DEI PGM CHIAMATI
135900980403     C                   CLEAR                   BS69DS
136000980403     C                   CLEAR                   ACODS
136100980403     C                   CLEAR                   INDDS
136200980403     C                   CLEAR                   CLPDS
136300980403     C                   CLEAR                   CLSDS
136400980403     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
136500980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
136600980403     C                   CALL      'TIBS69R'
136700980403     C                   PARM                    BS69DS
136800980403     C                   PARM                    ACODS
136900980403     C                   PARM                    INDDS
137000980403     C                   PARM                    CLPDS
137100980403     C                   PARM                    CLSDS
137200980403     C*
137300980403     C                   ENDSR
137400000000     C*------------------------------------------------------------------------*
137500000000     C* *INZSR - IMPOSTAZIONI INIZIALI
137600000000     C*------------------------------------------------------------------------*
137700000000     C     *INZSR        BEGSR
137800000000     C*
137900000000     C* PARAMETRI IN INPUT AL PGM
138000000000     C     *ENTRY        PLIST
138100000000     C                   PARM                    KPJBA
138200000000     C                   MOVEL     KPJBU         PARAM
138300160713     c* Se richiamato x BUDGET questa DATA è Valorizzata
138400160713     C                   MOVE      KPJBU         Data_8alfa        8
138500070531     c*
138600160713      *  controllo validità campo Data ricevuto se in MODALITA' BUDGET
138700160713     c                   clear                   dta_BDG_unif      8 0
138800160713     c                   if        Data_8alfa <> *blank
138900160713     c     digits        check     Data_8alfa
139000160713     c                   if        not %Found
139100160713     c                   move      Data_8alfa    dta_BDG_unif
139200160713      *** se Budget PARIOE = *Blank per sommare ITALIA +ESTERO attenzione
139300160713     c                   end
139400160713     c                   endif
139500160713     c*
139600070531     c* apertura condizionata dei 2 files da generare
139700070531     c                   if        PARWfdpv = 'S'
139800110401     c                   open      WAdfpv1l
139900110401     c                   open      WAdfpv4l
140000110401     c                   open      WAdfpv5l
140100070531     c                   else
140200100127     c                   open      WFdea01l
140300100127     c                   open      WFdea04l
140400100127     c                   open      WFdea05l
140500100127     c                   open      WFdeav1l
140600070531     c                   end
140700000000     C*
140800000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
140900000823     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
141000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
141100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
141200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
141300990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC01L
141400000000     C     *LIKE         DEFINE    SDCANN        KSCANN
141500000000     C     *LIKE         DEFINE    SDCMES        KSCMES
141600990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO01L
141700000000     C     *LIKE         DEFINE    SDOANN        KSOANN
141800000000     C     *LIKE         DEFINE    SDOMES        KSOMES
141900100127     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEa01L
142000000000     C     *LIKE         DEFINE    WFDARE        KWDARE
142100000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
142200000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
142300000000     C     *LIKE         DEFINE    WFDCLV        KWDCLV
142400000823     C     *LIKE         DEFINE    WFDCLI        KWDCLI
142500000000     C     *LIKE         DEFINE    WFDANN        KWDANN
142600000000     C     *LIKE         DEFINE    WFDMES        KWDMES
142700000824     C     *LIKE         DEFINE    WFDFPE        KWDFPE
142800000000     C     *LIKE         DEFINE    WFDIOE        KWDIOE
142900020315     C     *LIKE         DEFINE    WFDNTW        KWDNTW
143000100127     C     *LIKE         DEFINE    WFDTSP        KWDTSP
143100000000     C*
143200000000     C* DEFINIZIONI DEPOSITI E VARIABILI
143300000000     C     *LIKE         DEFINE    CLPKSC        DEPKSC
143400100127     C     *LIKE         DEFINE    CLPKSC        UNIcli
143500000000     C     *LIKE         DEFINE    CLPAGE        DEPAGE
143600000000     C     *LIKE         DEFINE    ORGFIL        DEPFIL
143700000000     C     *LIKE         DEFINE    ORGCAR        DEPARE
143800000000     C     *LIKE         DEFINE    ORGFL3        DEPDIV
143900000000     C     *LIKE         DEFINE    ORGFL3        DEPDIC
144000000000     C     *LIKE         DEFINE    WFDRRN        DEPRRN
144100000000     C     *LIKE         DEFINE    CLPAGE        DEPRGF
144200000000     C     *LIKE         DEFINE    CLPCLV        DEPCLV
144300140612     C**** *LIKE         DEFINE    §01AGE        DEPRGD
144400140612     C     *LIKE         DEFINE    CMMDES        DEPRGD
144500000000     C     *LIKE         DEFINE    SDOCLN        DEPCLN
144600970821     C     *LIKE         DEFINE    WFDIOE        DEPIOE
144700020315     C     *LIKE         DEFINE    WFDNTW        DEPNTW
144800070531     C     *LIKE         DEFINE    WFDRRN        DEPRRN
144900100203     C* x il WFdfpv0F
145000070531     C     *LIKE         DEFINE    WFDpkg        DEPpkg
145100070531     C     *LIKE         DEFINE    WFDvol        DEPvol
145200070723     C     *LIKE         DEFINE    wfdANN        depANN
145300070723     C     *LIKE         DEFINE    wfdMES        depMES
145400000825     C*
145500000825     C* DEFINIZIONI VARIABILI DI LAVORO
145600000825     C     *LIKE         DEFINE    PARSPD        MENSPD
145700000000     C                   MOVEL     '0'           $FINE             1            *FINE PROGRAMMA
145800980601     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
145900980601     C                   MOVEL     '0'           PCLNEW            1            *CLIENTE NUOVO PRECE
146000000000     C                   MOVEL     '0'           CCLNEW            1            *CLIENTE NUOVO CORRE
146100000000     C                   Z-ADD     *ZEROS        C                 4 0          *PER COMMERCIALI
146200000000     C                   Z-ADD     *ZEROS        G                 4 0          *PER FLAG TIPI BOLLA
146300000000     C                   Z-ADD     *ZEROS        J                 4 0          *PER MESI ELABORABIL
146400000000     C                   Z-ADD     *ZEROS        F                 4 0          *PER ORGANIGRAMMA
146500970821     C                   Z-ADD     *ZEROS        P                 4 0          *PER FASCE DI PESO
146600970821     C                   Z-ADD     *ZEROS        A                 4 0          *PER ANNI
146700970821     C                   Z-ADD     *ZEROS        M                 4 0          *PER MESI
146800020404     C                   Z-ADD     *ZEROS        X                 3 0          *PER RAGGRUPPAMENTI REGIONI/NETWORK
146900020404     C                   Z-ADD     *ZEROS        R                 3 0          *PER NETWORK
147000000000     C                   SETON                                        OF        *OVERFLOW
147100000000     C*
147200000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
147300000000     C     KEYORG        KLIST
147400000000     C                   KFLD                    KORFIL                         *FILIALE
147500000000     C*
147600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
147700000000     C     KEYTAB        KLIST
147800000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
147900000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
148000000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
148100000000     C*
148200000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
148300000000     C     KEYTA2        KLIST
148400000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
148500000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
148600020404     C*
148700020404     C* CHIAVE LETTURA TNTBE00F - PARZIALE
148800020404     C     KEYTBE        KLIST
148900020404     C                   KFLD                    TBECOD
149000000000     C*
149100990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
149200000000     C     KEYSDC        KLIST
149300000000     C                   KFLD                    KSCKSC                         *CODICE CLIENTE
149400000000     C                   KFLD                    KSCANN                         *ANNO
149500000000     C                   KFLD                    KSCMES                         *MESE
149600000000     C*
149700990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
149800000000     C     KEYSDO        KLIST
149900000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
150000000000     C*
150100990609     C* CHIAVE LETTURA SISDO01L - COMPLETA
150200000000     C     KE2SDO        KLIST
150300000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
150400000000     C                   KFLD                    KSOANN                         *ANNO
150500000000     C                   KFLD                    KSOMES                         *MESE
150600100127     C*
150700100127     C* CHIAVE LETTURA WFdeaV1L - COMPLETA
150800100127     C     KEYdeaV       KLIST
150900100215     C                   KFLD                    WFVSVR                         *VARIA
151000100127     C                   KFLD                    WFVKSC                         *CODICE CLIENTE
151100100127     C                   KFLD                    WFVANN                         *ANNO
151200100127     C                   KFLD                    WFVMES                         *MESE
151300100127     C*
151400100127     C* CHIAVE LETTURA WFDEa01L - COMPLETA
151500000000     C     KEYDEL        KLIST
151600000000     C                   KFLD                    KWDDIV                         *DIVISIONE
151700000000     C                   KFLD                    KWDARE                         *AREA
151800000000     C                   KFLD                    KWDFIL                         *FILIALE
151900000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
152000000000     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
152100000823     C                   KFLD                    KWDCLI                         *CLIENTE
152200000000     C                   KFLD                    KWDANN                         *ANNO
152300000000     C                   KFLD                    KWDMES                         *MESE
152400000824     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
152500000000     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
152600020614     C                   KFLD                    KWDNTW                         *NETWORK
152700000823     C*
152800100127     C     KEYDELtsp     KLIST
152900100127     C                   KFLD                    KWDDIV                         *DIVISIONE
153000100127     C                   KFLD                    KWDARE                         *AREA
153100100127     C                   KFLD                    KWDFIL                         *FILIALE
153200100127     C                   KFLD                    KWDAGE                         *COMMERCIALE
153300100127     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
153400100127     C                   KFLD                    KWDCLI                         *CLIENTE
153500100127     C                   KFLD                    KWDANN                         *ANNO
153600100127     C                   KFLD                    KWDMES                         *MESE
153700100127     C                   KFLD                    KWDFPE                         *FASCIA DI PESO
153800100127     C                   KFLD                    KWDIOE                         *ITALIA O ESTERO
153900100127     C                   KFLD                    KWDNTW                         *NETWORK
154000100127     C                   KFLD                    KWDTSP                         *TIPO SERVIZIO
154100100127     C*
154200000823     C* IMNPOSTA DATA / ORA CORRENTE
154300000823     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
154400000823     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
154500000823     C                   Z-ADD     *ZEROS        G08INV
154600000823     C                   MOVEL     '0'           G08ERR
154700000823     C                   CALL      'XSRDA8'
154800000823     C                   PARM                    WLBDA8
154900000823     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
155000000000     C*
155100000825     C* CARICA TABELLE OCCORRENTI
155200000000     C                   EXSR      CARTAB
155300970821     C*
155400970821     C* CALCOLA I MESI DA ELABORARE
155500990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
155600970821     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
155700990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
155800970821     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
155900970821     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORAR
156000970821     C                   EXSR      AMELAB
156100970822     C*
156200000825     C* PRENDE IL VALORE ASSOLUTO DELLA % ALLA QUALE SEGNALARE LO SCOSTAMENTO FASCE
156300970822     C                   MLLZO     1             PARSPD                         *VALORE ASSOLUTO
156400000825     C                   Z-ADD     *ZEROS        MENSPD
156500000825     C                   Z-SUB     PARSPD        MENSPD                         *% RICHIESTA MENO
156600001122     C*
156700160713     C*    SE RICHIAMATO come BUDGET deve impostare la DATA di Riferimento
156800160713     C*       direttamente dal pgm chiamante
156900160713     c                   if        dta_BDG_unif   = 0
157000160713     C*
157100001122     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
157200010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
157300010725     C                   MOVE      28            WDFIN
157400081114     C*
157500081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
157600081114     C*
157700081114     C*  Ultimo giorno del mese
157800081114     C                   MOVE      WDFIN         MMGG_4            4 0
157900081114     C                   MOVEL     MMGG_4        Mm_2              2 0
158000081114     C                   MOVEl     WDFIN         Anno_4            4 0
158100081114      * se è febbraio
158200081114     c                   If        Mm_2 = 2
158300081114      *
158400081114      *  potrebbe essere bisestile 29 gg.
158500081114     c     Anno_4        div       4             risulta           3 0
158600081114     c                   mvr                     resto             3 0
158700081114      *  Se bisestile
158800081114     c                   If        resto = 0
158900081114     C                   MOVE      '29'          WDFIN
159000081114     c                   end
159100081114      *
159200081114     c                   else
159300081114      *
159400081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
159500081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
159600081114     c                                Mm_2 = 9 or Mm_2 = 11
159700081114     C                   MOVE      '30'          WDFIN
159800081114     c                   else
159900081114     C                   MOVE      '31'          WDFIN
160000081114     c                   end
160100081114      *
160200081114     c                   endIf
160300081114     C*
160400081114     C*----------- F I N E    M E S E ---------------------------*
160500000000     C*
160600160713     C*    altrimenti come BUDGET imposta la data di riferimento
160700160713     C*    dell'unificante
160800160713     c                   else
160900160713     C                   MOVE      dta_BDG_unif  WDFIN
161000160713     c                   end
161100160713     C*
161200000000     C                   ENDSR
161300070723     C*------------------------------------------------------------------------*
161400070723     C* Aggiorna il flag Nuovo/Acquisito x UNificante
161500070723     C*------------------------------------------------------------------------*
161600070723     C     AGGWFD_uni    BEGSR
161700070727     C*
161800070727      *? Per prima cosa deve rendere omogenei i records nell'ambito dell'anno con "Y" e "S"...  ?
161900070727      *? significa che se nell'anno sono nati 2 codici clienti nuovi mi troverei "Y" su 2 mesi ?
162000070727      *? diversi nell'ambito dello stesso Anno.                             ?
162100070727      *? ..... ovviamente prevale la "Y" più vecchia a cui seguono delle "S" x indicare il Nuovo.
162200070727      *? -------------------------------------------------------------------------- ?
162300070727      *  (esempio esplicativo)
162400070727      *               Anno 200X? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
162500070727      *? Cliente (A)...  ?      |   | Y | S | S | S | S | S | S |   | S | S |   |
162600070727      *? Cliente (B)...  ?      |   |   |   | Y | S | S |   | S | S | S |   |   |
162700070727      *? -----------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
162800070727      *? Unificante (C)         |   | Y | S | Y | S | S | S | S | S | S | S |   |                         ?
162900070727      * -->
163000070727      *? Invece deve essere così|   | Y | S | S | S | S | S | S | S | S | S |   |   ?
163100070727      *? -------------------------------------------------------------------------- ?
163200070723     C*
163300070723     C* LEGGE TUTTO IL WFILE APPENA SCRITTO
163400070723     c                   if        PARWfdpv = 'S'
163500110401     C     *LOVAL        SETLL     WAdfpv5L
163600110401     C                   READ      WAdfpv5L                               99
163700070723     c                   else
163800100127     C     *LOVAL        SETLL     WFDEa05L
163900100127     C                   READ      WFDEa05L                               99
164000070723     c                   end
164100070723     C*
164200070723     C* CICLO FINO A FINE FILE
164300070723DO  1C     *IN99         DOWEQ     *OFF
164400070723     C                   move      wfdIOE        depIOE
164500100127     C                   z-add     wfdCLI        UNIcli
164600070723     C                   z-add     0             Cliente_Anno      1 0
164700070723      *
164800070723     C* a Rottura Cliente
164900070723DO  2C     *IN99         DOWEQ     *OFF
165000070723     C     depIOE        ANDEQ     wfdIOE
165100100127     C     UNIcli        ANDEQ     wfdCLI
165200070723     C                   z-add     wfdANN        depANN
165300070723     C                   Clear                   Ypsilon_anno      1
165400070723     C                   add       1             Cliente_Anno
165500070723     C*
165600070723     C* a Rottura Cliente Anno
165700070723DO  3C     *IN99         DOWEQ     *OFF
165800070723     C     depIOE        ANDEQ     wfdIOE
165900100127     C     UNIcli        ANDEQ     wfdCLI
166000070723     C     depANN        ANDEQ     wfdANN
166100070723     C                   z-add     wfdMES        depMES
166200070723     C*
166300070723     C* a Rottura Cliente Anno/Mese
166400070723DO  4C     *IN99         DOWEQ     *OFF
166500070723     C     depIOE        ANDEQ     wfdIOE
166600100127     C     UNIcli        ANDEQ     wfdCLI
166700070723     C     depANN        ANDEQ     wfdANN
166800070723     C     depMES        ANDEQ     wfdMES
166900070723      *
167000070723      *  Memorizza il primo Ypsilon trovato x cliente Anno
167100070723     C                   if        wfdCLN <> ' ' and Ypsilon_anno = *blank
167200070723     C                   eval      Ypsilon_anno = wfdCLN
167300070723     c                   end
167400070723      *
167500070723      * Tutto il mese cliente deve essere a "Y"
167600070723     C                   if        Ypsilon_anno = 'Y'
167700070723     c                   eval      wfdCLN = 'Y'
167800070723     c                   end
167900070723      *
168000070723      * Dal mese seguente deve impostare sempre la "S"
168100070723      *   anche per l'anno seguente...
168200070723     C                   if        Ypsilon_anno = 'S'
168300070723     c                   eval      wfdCLN = 'S'
168400070723     c                   end
168500070723      *
168600070723     c                   if        PARWfdpv = 'S'
168700100203     C                   UPDATE    WFdfp5
168800070723     c                   else
168900100127     C                   UPDATE    WFDEa5
169000070723     c                   end
169100070723     C*
169200070723     C* LETTURA SUCCESSIVA DATI CLIENTE
169300070723     c                   if        PARWfdpv = 'S'
169400110401     C                   READ      WAdfpV5L                               99
169500070723     c                   else
169600100127     C                   READ      WFDEa05L                               99
169700070723     c                   end
169800070723E   4C                   ENDDO                                                  *ROTTURA Mese
169900070723     C*
170000070723     C*  Se il mese letto aveva la "Y" i mesi seguenti entro l'anno devono avere la "S"
170100070723     C                   if        Ypsilon_anno = 'Y'
170200070723     C                   eval      Ypsilon_anno = 'S'
170300070723     c                   end
170400070723      *
170500070723E   3C                   ENDDO                                                  *ROTTURA CLIENTE/Anno
170600070723     C*
170700070723E   2C                   ENDDO
170800070723     C*
170900070723E   1C                   ENDDO
171000070727     C*
171100070727      *? ------------------------------------------------------------------------------------------------- ?
171200070727      *? ......Dopo avere reso omogeneo nell'ambito dell'anno la sequenza di "Y" e "S", si deve considerare    ?
171300070727      *? sull'anno in CORSO che, se un cliente aveva già del fatturato nell'anno precedente,                           ?
171400070727      *? NON può considerarsi Nuovo.!!!!!!                         ?
171500070727      *? ..... quindi, sul Cliente, nell'Anno in CORSO, si devono eliminare le "Y" e "S" su tutti i records.
171600070727      *? ------------------------------------------------------------------------------------------------- ?
171700070727      *  (esempio esplicativo)
171800070727      *                              ? Gen Feb Mar Apr Mag Giu Lug Ago Set Ott Nov Dic
171900070727      *? Unific.(C) Anno Precedente ? |   |   |   | Y | S | S | S | S | S | S | S | S |
172000070727      *? Unific.(C) Anno  in  CORSO ? |   | Y | S | S | S | S | S | S |   |   |   |   |
172100070727      *? -----------------------------|---|---|---|---|---|---|---|---|---|---|---|---|-- ?
172200070727      *? ============================     ?
172300070727      * --> Invece deve essere così
172400070727      *? ============================     ?
172500070727      *? Unific.(C) Anno Precedente   |   |   |   | Y | S | S | S | S | S | S | S | S |   ?-- records anno precedente
172600070727      *? Unific.(C) Anno  in  CORSO   |   |   |   |   |   |   |   |   |   |   |   |   |   ?-- records anno in Corso
172700070727      *? -------------------------------------------------------------------------------- ?
172800070727     C*   Con un istruzione SQL possiamo quindi sistemare la situazione sull'anno in corso
172900070727     C*   semplicemente controllando se nell'anno precedente il Cliente aveva del Fatturato
173000070727     C*   quindi, aggiorniamo tutti i Suoi records dell'Anno in corso x dire che NON è NUOVO.
173100070727      *
173200070727     C/EXEC SQL
173300100127     C+ UPDATE wfdea00f SET wfdcln = ' '
173400070727     C+ WHERE wfdann = :ParAAI AND wfdcln <>' ' AND wfdcli
173500100127     C+ IN (SELECT wfdcli FROM wfdea00f WHERE wfdann = :ParAFP)
173600070727     C/END-EXEC
173700070723     C*
173800121210     C*  Una volta sistemati tutti i records per l'anno in corso con solo la sequenza
173900121210     C*  giusta dal mese in cui nasce il cliente NUOVO.
174000121210     C*  Occorre sistemare eventuali saldi Antecedenti dovuti a storni o spalmature
174100121210     C*  di valori attribuiti al cliente UNIFICANTE....non potendo fare diversamente.
174200121207     C                   Exsr      NUOVO_CON_MESI
174300121207     C*
174400121207     C                   ENDSR
174500121207      *------------------------------------------------------------------------*
174600121207     C* Se L'AUT è considerato NUOVO nell'anno ed ha mesi precedenti con BLANK
174700121207      *------------------------------------------------------------------------*
174800121207     C     NUOVO_con_mesiBEGSR
174900121207     C*
175000121207     C*  se sono state fatte delle Note di Credito o altro tipo di Storni o Rettifiche
175100121210     C*  che vengono spalmate sui mesi precedenti a quello che ha la "Y" come mese iniziale.
175200121210     C*  Occorre ragionare impostando la "Y" anche su tali record ed in seguito fare
175300121210     C*  riferimento al mese più recente con la "Y" per conoscere qual'è il mese di VERO
175400121210     C*  inizio per il codice che si sta trattando.
175500121210     C*  Quindi sui mesi più vecchi ANTECEDENTI: verrà impostata la "Y" per poter
175600121210     C*  determinare nel programma TISE93R a rottura di cliente primo mese letto, se si
175700121210     C*  tratta di cliente NUOVO oppure acquisito.
175800121210     C*
175900121210     C/EXEC SQL
176000121210     C+ update  wfdea00f set  WFDCLN ='Y' WHERE wfdcln=' ' and
176100121210     C+ wfdcli in (select wfdcli from wfdea00f where wfdcln <> ' '
176200121210     C+ and wfdann = :PARAAI) and  wfdcli in  ( SELECT WFDCLI FROM
176300121210     C+ wfdea00f where wfdcli in (select wfdcli from wfdea00f
176400121210     C+ where wfdcln <> ' ' and wfdann = :PARAAI) and  wfdcln=' ')
176500121210     C/END-EXEC
176600121210     C*
176700121207     C                   ENDSR
176800121207     O*------------------------------------------------------------------------*
176900000000     O* STAMPA DEI CODICI AGENTI INESISTENTI O ANNULLATI DA INSERIRE IN SEDE   *
177000000000     O*------------------------------------------------------------------------*
177100000000     OQSYSPRT   E            TESTA            02
177200000000     O                       STA(1)              66
177300000000     O                       STA(2)             132
177400980306     O                       UDATE              119 '  /  /  '
177500000000     O                       PAGE          Z    132
177600000000     O*------------------------
177700000000     O          E            TESTA            03
177800000000     O                       STA(3)              66
177900000000     O                       STA(3)             132
178000000000     O*------------------------
178100000000     O          E            TESTA            04
178200000000     O                       STA(4)              66
178300000000     O*------------------------
178400000000     O          E            TESTA            05
178500000000     O                       STA(3)              66
178600000000     O                       STA(3)             132
178700000000     O*------------------------
178800000000     O          E            ERROR       1
178900000000     O                       DEPAGE               9
179000000000     O                       DEPKSC              27
179100100127     O                       UNIcli              47
179200000000     O*------------------------------------------------------------------------*
179300000000** STA
179400100127                           TISE92AR     ***  AGENTI INESISTENTI DA1
179500980306 MODIFICARE IN ANAGRAFICA CLIENTE  ***       99/99/99     PAG.99992
179600000000------------------------------------------------------------------3
179700000824A G E N T E      C L I E N T E     U N I F I C A N T E            4
179800000824  9999999           9999999             9999999                   5
