000100000000     H*------------------------------------------------------------------------*
000200000000     H*                   - STATISTICA DELTA COMMERCIALE -                     *
000300000000     H*------------------------------------------------------------------------*
000400970820     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FWFDEL02L  IF   E           K DISK
000900000000     FAZORG01L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001100020116     FTISE94P   O    E             PRINTER OFLIND(*IN01)
001200030916     FWFTISE94L UF A E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*------------------
001700000000     D* DECODIFICA MESI DELL'ANNO
001800000000     D*------------------
001900000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
002000000000     D*------------------
002100000000     D* DECODIFICA STATISTICHE
002200000000     D*------------------
002300000000     D DST             S             25    DIM(5) CTDATA PERRCD(1)
002400000000     D*------------------
002500000000     D* DECODIFICA RIGHE DI STAMPA
002600000000     D*------------------
002700000825     D DRI             S             24    DIM(30) CTDATA PERRCD(1)
002800020319     D*------------------
002900020319     D* DECODIFICA NETWORK
003000020319     D*------------------
003100020319     D DNTW            S              3    DIM(5) CTDATA PERRCD(1)
003200000000     D*------------------
003300000000     D* ORGANIGRAMMA
003400000000     D*------------------
003500971124     D FIL             S              3  0 DIM(500)                             *FILIALI
003600971124     D DFIL            S             20    DIM(500)
003700971124     D ARE             S              3  0 DIM(500)                             *AREE
003800971124     D DARE            S             20    DIM(500)
003900971124     D DIV             S              1    DIM(500)                             *DIVISIONI
004000971124     D DDIV            S             20    DIM(500)
004100020627     D POS             S              3    DIM(500)                             *NETWORK POSTE
004200000000     D*------------------
004300000000     D* COMMERCIALI
004400000000     D*------------------
004500051027     D AGE             S              7  0 DIM(3000)                            *CODICI
004600051027     D DAGE            S             20    DIM(3000)                            *DECODIFICHE
004700051027     D CAGE            S              3  0 DIM(3000)                            *PERCENTUALI COMPETE
004800980521     D*------------------
004900980521     D* CATEGORIE CLIENTI
005000980521     D*------------------
005100020220     D ABC             S              1    DIM(5)                               QUALITA' CLIENTE
005200000000     D*------------------
005300000000     D* MESI DA ELABORARE
005400000000     D*------------------
005500000000     D MES             S              2  0 DIM(12)
005600000000     D*------------------
005700000000     D* ANNI DEI MESI DA ELABORARE
005800000000     D*------------------
005900990609     D ANN             S              4  0 DIM(12)
006000020319     D*------------------
006100020319     D* SCHIERA DI WRK X CONTEGGIO NETWORK SU ANNO "P" E ANNO "C" (LA DIMENSIONE=NUM. MESI x NUM. NETWORK => 13x10)
006200020319     D*------------------
006300020319     D WSK1            S             12    DIM(130)
006400000000    ?D*-------------------
006500000000    ?D* S C H I E R E   M E M O R I Z Z A Z I O N E  (12 MESI + ANNO PRECEDENTE)
006600000000    ?D*-------------------
006700000830     D*------------------
006800000830     D* FLAG CONTATORE CLIENTE PER MESE
006900000830     D*------------------
007000000830     D SFCCL           S              1    DIM(12)                              FLAG CONTEGGIO
007100000000     D*------------------
007200020220     D* SCHIERA PER TOTALI DIVISIONE (NO 'D')
007300000000     D*------------------
007400000000     D CLD             S             11  0 DIM(13)                              NUMERO CLIENTI
007500000000     D SPD             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
007600020116     D RRD             S             19  5 DIM(13)                              RICAVI REALI
007700020116     D RPD             S             19  5 DIM(13)                              RICAVI PRESUNTI
007800000000     D*------------------
007900020220     D* SCHIERA PER TOTALI RAGGRUPPAMENTO COMMERCIALE (NO 'D')
008000000000     D*------------------
008100000000     D CLR             S             11  0 DIM(13)                              NUMERO CLIENTI
008200000000     D SPR             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
008300020116     D RRR             S             19  5 DIM(13)                              RICAVI REALI
008400020116     D RPR             S             19  5 DIM(13)                              RICAVI PRESUNTI
008500000000     D*------------------
008600020220     D* SCHIERA PER TOTALI COMMERCIALE (NO 'D')
008700000000     D*------------------
008800000000     D CLC             S             11  0 DIM(13)                              NUMERO CLIENTI
008900000000     D SPC             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
009000020116     D RRC             S             19  5 DIM(13)                              RICAVI REALI
009100020116     D RPC             S             19  5 DIM(13)                              RICAVI PRESUNTI
009200020220     D*------------------
009300020220     D* SCHIERA PER TOTALI DIVISIONE
009400020220     D*------------------
009500020220     D CLDD            S             11  0 DIM(13)                              NUMERO CLIENTI
009600020220     D SPDD            S             13  0 DIM(13)                              NUMERO SPEDIZIONI
009700020220     D RRDD            S             19  5 DIM(13)                              RICAVI REALI
009800020220     D RPDD            S             19  5 DIM(13)                              RICAVI PRESUNTI
009900020220     D*------------------
010000020220     D* SCHIERA PER TOTALI RAGGRUPPAMENTO COMMERCIALE
010100020220     D*------------------
010200020220     D CLRD            S             11  0 DIM(13)                              NUMERO CLIENTI
010300020220     D SPRD            S             13  0 DIM(13)                              NUMERO SPEDIZIONI
010400020220     D RRRD            S             19  5 DIM(13)                              RICAVI REALI
010500020220     D RPRD            S             19  5 DIM(13)                              RICAVI PRESUNTI
010600020220     D*------------------
010700020220     D* SCHIERA PER TOTALI COMMERCIALE
010800020220     D*------------------
010900020220     D CLCD            S             11  0 DIM(13)                              NUMERO CLIENTI
011000020220     D SPCD            S             13  0 DIM(13)                              NUMERO SPEDIZIONI
011100020220     D RRCD            S             19  5 DIM(13)                              RICAVI REALI
011200020220     D RPCD            S             19  5 DIM(13)                              RICAVI PRESUNTI
011300020319     D*------------------
011400020319     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK - NN "D"
011500020319     D*------------------
011600020319     D NTWDIVN         DS                  OCCURS(5) INZ
011700020319     D  CLDN                   1     78P 0
011800020319     D                                     DIM(13)                              NUMERO CLIENTI
011900020319     D  SPDN                  79    169P 0
012000020319     D                                     DIM(13)                              SPEDIZIONI
012100020319     D  RRDN                 170    286P 5
012200020319     D                                     DIM(13)                              RICAVI REALI
012300020319     D  RPDN                 287    403P 5
012400020319     D                                     DIM(13)                              RICAVI PRESUNTI
012500020319     D*------------------
012600020319     D* DS MULTIPLA TOTALI RAGGRUPPAMENTO COMMERCIALE  - 5 NETWORK - NN "D"
012700020319     D*------------------
012800020319     D NTWRGFN         DS                  OCCURS(5) INZ
012900020319     D  CLRN                   1     78P 0
013000020319     D                                     DIM(13)                              NUMERO CLIENTI
013100020319     D  SPRN                  79    169P 0
013200020319     D                                     DIM(13)                              SPEDIZIONI
013300020319     D  RRRN                 170    286P 5
013400020319     D                                     DIM(13)                              RICAVI REALI
013500020319     D  RPRN                 287    403P 5
013600020319     D                                     DIM(13)                              RICAVI PRESUNTI
013700020319     D*------------------
013800020319     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK - NN "D"
013900020319     D*------------------
014000020319     D NTWAGEN         DS                  OCCURS(5) INZ
014100020319     D  CLCN                   1     78P 0
014200020319     D                                     DIM(13)                              NUMERO CLIENTI
014300020319     D  SPCN                  79    169P 0
014400020319     D                                     DIM(13)                              SPEDIZIONI
014500020319     D  RRCN                 170    286P 5
014600020319     D                                     DIM(13)                              RICAVI REALI
014700020319     D  RPCN                 287    403P 5
014800020319     D                                     DIM(13)                              RICAVI PRESUNTI
014900020319     D*------------------
015000020319     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK -
015100020319     D*------------------
015200020319     D NTWDIVND        DS                  OCCURS(5) INZ
015300020319     D  CLDND                  1     78P 0
015400020319     D                                     DIM(13)                              NUMERO CLIENTI
015500020319     D  SPDND                 79    169P 0
015600020319     D                                     DIM(13)                              SPEDIZIONI
015700020319     D  RRDND                170    286P 5
015800020319     D                                     DIM(13)                              RICAVI REALI
015900020319     D  RPDND                287    403P 5
016000020319     D                                     DIM(13)                              RICAVI PRESUNTI
016100020319     D*------------------
016200020319     D* DS MULTIPLA TOTALI RAGGRUPPAMENTO COMMERCIALE  - 5 NETWORK -
016300020319     D*------------------
016400020319     D NTWRGFND        DS                  OCCURS(5) INZ
016500020319     D  CLRND                  1     78P 0
016600020319     D                                     DIM(13)                              NUMERO CLIENTI
016700020319     D  SPRND                 79    169P 0
016800020319     D                                     DIM(13)                              SPEDIZIONI
016900020319     D  RRRND                170    286P 5
017000020319     D                                     DIM(13)                              RICAVI REALI
017100020319     D  RPRND                287    403P 5
017200020319     D                                     DIM(13)                              RICAVI PRESUNTI
017300020319     D*------------------
017400020319     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK -
017500020319     D*------------------
017600020319     D NTWAGEND        DS                  OCCURS(5) INZ
017700020319     D  CLCND                  1     78P 0
017800020319     D                                     DIM(13)                              NUMERO CLIENTI
017900020319     D  SPCND                 79    169P 0
018000020319     D                                     DIM(13)                              SPEDIZIONI
018100020319     D  RRCND                170    286P 5
018200020319     D                                     DIM(13)                              RICAVI REALI
018300020319     D  RPCND                287    403P 5
018400020319     D                                     DIM(13)                              RICAVI PRESUNTI
018500020319     D*------------------
018600020319     D* SCHIERE PER TOTALI 1° E 13° COLONNA X NETWORK ("D" E NN "D")
018700020319     D*------------------
018800020319     D CTDN            S             11  0 DIM(13)
018900020319     D CTDDN           S             11  0 DIM(13)
019000020319     D C2DN            S             11  0 DIM(13)
019100020319     D C2DDN           S             11  0 DIM(13)
019200020319     D CTRN            S             11  0 DIM(13)
019300020319     D CTRDN           S             11  0 DIM(13)
019400020319     D C2RN            S             11  0 DIM(13)
019500020319     D C2RDN           S             11  0 DIM(13)
019600020319     D CTCN            S             11  0 DIM(13)
019700020319     D CTCDN           S             11  0 DIM(13)
019800020319     D C2CN            S             11  0 DIM(13)
019900020319     D C2CDN           S             11  0 DIM(13)
020000020319     D*------------------
020100020319     D* SCHIERE DI STAMPA
020200000000     D*------------------
020300000000     D CLS             S             11  0 DIM(13)                              NUMERO CLIENTI
020400000000     D SPS             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
020500020116     D RRS             S             19  5 DIM(13)                              RICAVI REALI
020600020116     D RPS             S             19  5 DIM(13)                              RICAVI PRESUNTI
020700000000     D*------------------
020800020220     D* SCHIERE DI LAVORO PER TOTALI "A" + "B" + "C" + "T" + "D"
020900000000     D*------------------
021000000000     D CLW             S             11  0 DIM(13)                              NUMERO CLIENTI
021100000000     D SPW             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
021200020116     D RRW             S             19  5 DIM(13)                              RICAVI REALI
021300020116     D RPW             S             19  5 DIM(13)                              RICAVI PRESUNTI
021400000000     D*------------------
021500000000     D* SCHIERE DI LAVORO
021600000000     D*------------------
021700000000     D $CL             S             13  0 DIM(13)                              NUMERO CLIENTI
021800000000     D $SP             S             13  0 DIM(13)                              SPEDIZIONI
021900020116     D $RR             S             19  5 DIM(13)                              RICAVO REALE
022000020116     D $RP             S             19  5 DIM(13)                              RICAVO PRESUNTO
022100000000     D $PD             S              4  1 DIM(13)                              DELTA
022200020116     D $VA             S             19  5 DIM(13)                              VALORE GENERICO
022300000000     D*------------------------------------------------------------------------*
022400000000     D* INPUT
022500000000     D*------------------------------------------------------------------------*
022600000000     D KPJBA         E DS
022700000000     D*------------------
022800000000     D* PARAMETRI FILTRO
022900000000     D*------------------
023000000000     D PARAM           DS
023100990609     D  PARDAT                 1      6  0
023200990609     D  PARAAI                 1      4  0
023300990609     D  PARMMI                 5      6  0
023400990609     D  PARDA2                 7     12  0
023500990609     D  PARAAF                 7     10  0
023600990609     D  PARMMF                11     12  0
023700990609     D  PARDCK                13     13
023800990609     D  PARDCC                14     14
023900990609     D  PARDCF                15     15
024000990609     D  PARFIC                16     18  0
024100990609     D  PARARC                19     21  0
024200990609     D  PARDIC                22     22
024300990609     D  PARDFF                23     23
024400990609     D  PARCOF                24     30  0
024500990609     D  PARDIF                31     31
024600990609     D  PARSTA                32     32
024700000825     D  PARSCE                33     33
024800990609     D  PARTSP                34     34
024900990609     D  PARIOE                35     35
025000990609     D  PARCLV                36     36
025100990609     D  PARDPT                37     42  0
025200990609     D  PARAPI                37     40  0
025300990609     D  PARMPI                41     42  0
025400990609     D  PARDP2                43     48  0
025500990609     D  PARAFP                43     46  0
025600990609     D  PARMFP                47     48  0
025700990609     D  PARSPD                49     52  1
025800990609     D  PARPDD                53     56  1
025900990609     D  PARPDA                57     60  1
026000000707     D  PARPOS                61     61
026100000825     D  PARCRE                62     62
026200010618     D  PARCL1                63     63
026300010618     D  PARCL2                64     64
026400010618     D  PARSIN                65     65
026500020220     D  PARCL3                66     66
026600020220     D  PARCL4                67     67
026700020404     D  PARNTW                68     68
026800020702     D  PARDFD                69     72  1
026900020702     D  PARDFA                73     76  1
027000020702     D  PARDFDP               77     80  1
027100020702     D  PARDFAP               81     84  1
027200030916     D  PARFPC                85     85
027300030916     D  PARWPC                86     86
027400000707     D*------------------
027500000707     D* DS LETTURA CAMPO ORGDE3 DI AZORG00F
027600000707     D*------------------
027700000707     D OG143         E DS
027800090610     D* Descrizioni tipi servizio
027900090610     D*------------------
028000090610     D DS5e          E DS
028100090610     D*------------------
028200000000     D*------------------
028300000000     D* DS AGENTI COMMERCIALI
028400000000     D*------------------
028500000000     D DS01          E DS
028600951219     D*------------------
028700951219     D* COMPOSIZIONE DATA SALDI
028800951219     D*------------------
028900951219     D                 DS
029000951219     D  WFDANN                 1      4  0                                      *ANNO
029100951219     D  WFDMES                 5      6  0                                      *MESE
029200951219     D  WFDDAT                 1      6  0                                      *DATA
029300020319     D*------------------
029400020319     D* DS DI RIEMPIMENTO SCHIERA DI WRK WSK1
029500020319     D*------------------
029600020319     D DSWSK1          DS                  INZ
029700020319     D  WFDCLI                        7  0
029800020319     D  MM                            2  0
029900020319     D  WFDNTW                        3
030000030918     d*---
030100030918     d* Variabili
030200030918     d*---
030300030918     d a30             S             30                                         *alfa 30
030400030919     d WNrr            S              7  0 INZ                                  *contatore righe
030500000000     C*------------------------------------------------------------------------*
030600030917     C*?MAIN LINE
030700000000     C*------------------------------------------------------------------------*
030800000000     C*
030900000000     C* STAMPA PARAMETRI FILTRO
031000000000     C                   EXSR      STAPAR
031100000000     C*
031200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
031300000000     C                   EXSR      SETFIL
031400000000     C*
031500000000     C* CICLO FINO A FINE FILE DI LAVORO
031600000000DO  1C     $FINE         DOWEQ     '0'
031700000000     C*
031800000000     C* OPERAZIONI PER NUOVA DIVISIONE COMMERCIALE
031900000000     C                   EXSR      NEWDIV
032000000000     C*
032100000000     C* CICLO FINO A ROTTURA DI DIVISIONE COMMERCIALE
032200000000DO  2C     $FINE         DOWEQ     '0'
032300000000     C     WFDDIC        ANDEQ     DEPDIV
032400000000     C*
032500000000     C* OPERAZIONI PER NUOVO RAGGRUPPAMENTO FILIALE AGENTE
032600000000     C                   EXSR      NEWRGF
032700000000     C*
032800000000     C* CICLO FINO A ROTTURA DI RAGGRUPPAMENTO FILIALE AGENTE
032900000000DO  3C     $FINE         DOWEQ     '0'
033000000000     C     WFDDIC        ANDEQ     DEPDIV
033100000000     C     WFDRGF        ANDEQ     DEPRGF
033200000000     C*
033300000000     C* OPERAZIONI PER NUOVO AGENTE
033400000000     C                   EXSR      NEWAGE
033500000000     C*
033600000000     C* CICLO FINO A ROTTURA DI AGENTE
033700000000DO  4C     $FINE         DOWEQ     '0'
033800000000     C     WFDDIC        ANDEQ     DEPDIV
033900000000     C     WFDRGF        ANDEQ     DEPRGF
034000000000     C     WFDAGE        ANDEQ     DEPAGE
034100000000     C*
034200000000     C* OPERAZIONI PER NUOVO CLIENTE
034300000000     C                   EXSR      NEWKSC
034400000000     C*
034500000000     C* CICLO FINO A ROTTURA DI AGENTE
034600000000DO  5C     $FINE         DOWEQ     '0'
034700000000     C     WFDDIC        ANDEQ     DEPDIV
034800000000     C     WFDRGF        ANDEQ     DEPRGF
034900000000     C     WFDAGE        ANDEQ     DEPAGE
035000000825     C     WFDCLI        ANDEQ     DEPCLI
035100000000     C*
035200000000     C* MEMORIZZA I SALDI
035300000000     C                   EXSR      MEMSAL
035400000000     C*
035500000000     C* LEGGE IL RECORD SUCCESSIVO
035600000000     C                   EXSR      LETFIL
035700000000     C*
035800000000E   5C                   ENDDO
035900000000E   4C                   ENDDO
036000000000     C*
036100000000     C* STAMPA IL TOTALE COMMERCIALE
036200000000     C                   EXSR      STAAGE
036300000000E   3C                   ENDDO
036400000000     C*
036500000000     C* STAMPA IL TOTALE RAGGRUPPAMENTO FILIALE AGENTE
036600000000     C                   EXSR      STARGF
036700000000E   2C                   ENDDO
036800000000     C*
036900000000     C* STAMPA IL TOTALE DIVISIONE
037000000000     C                   EXSR      STADIV
037100000000E   1C                   ENDDO
037200000000     C*
037300000000     C* STAMPA RIGA FINE STAMPA
037400020116     C                   WRITE     SE94FI
037500000000     C*
037600000000     C                   SETON                                        LR
037700000000     C*------------------------------------------------------------------------*
037800000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
037900000000     C*------------------------------------------------------------------------*
038000000000     C     SETFIL        BEGSR
038100000000     C*
038200000000     C* POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
038300000000     C                   MOVEL     *BLANKS       KWDDIV                         *DIVISIONE
038400000000     C                   MOVEL     *BLANKS       KWDRGD                         *DESCRIZIONE RAGGR.T
038500000000     C                   Z-ADD     *ZEROS        KWDRGF                         *RAGGRUPPAMENTO
038600000000     C                   Z-ADD     *ZEROS        KWDAGE                         *COMMERCIALE
038700000825     C                   Z-ADD     *ZEROS        KWDCLI                         *CLIENTE
038800000000     C                   Z-ADD     *ZEROS        KWDANN                         *ANNO
038900000000     C                   Z-ADD     *ZEROS        KWDMES                         *MESE
039000000000     C     KEYDEL        SETLL     WFDEL02L                           99        *FINE ARCHIVIO
039100000000     C*
039200951219IF  1C     *IN99         IFEQ      *ON
039300000000     C                   MOVEL     '1'           $FINE
039400000000X   1C                   ELSE
039500000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
039600000000E   1C                   ENDIF
039700000000     C*
039800000000     C                   ENDSR
039900000000     C*------------------------------------------------------------------------*
040000000000     C*?LETFIL - LETTURA PROSSIMO RECORD DEL WORK FILE
040100000000     C*------------------------------------------------------------------------*
040200000000     C     LETFIL        BEGSR
040300000000     C*
040400000000     C* LEGGO FINO A:
040500000000     C*     - FINE FILE
040600000000     C*     - TROVATO RECORD VALIDO
040700000000     C                   MOVEL     'N'           WRECOK
040800000000DO  1C     $FINE         DOWEQ     '0'
040900000000     C     WRECOK        ANDEQ     'N'
041000000000     C*
041100000000     C* LETTURE SUCCESSIVE ALLA PRIMA
041200000000     C                   READ      WFDEL02L                               99
041300000000     C*
041400951219IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
041500000000     C                   MOVEL     '1'           $FINE
041600000000X   2C                   ELSE
041700000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
041800000000E   2C                   ENDIF
041900000000     C*
042000000000E   1C                   ENDDO
042100000000     C*
042200000000     C                   ENDSR
042300000000     C*------------------------------------------------------------------------*
042400000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD DEL WORK FILE
042500000000     C*------------------------------------------------------------------------*
042600000000     C     CHKREC        BEGSR
042700000000     C*
042800000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
042900951219     C*
043000990609IF  1C     WFDDAT        IFGE      PARDPT
043100990609     C     WFDDAT        ANDLE     PARDP2
043200951219     C                   MOVEL     'P'           TIPANN                         *PERIODO PRECEDENTE
043300951219E   1C                   ENDIF
043400990609IF  1C     WFDDAT        IFGE      PARDAT
043500990609     C     WFDDAT        ANDLE     PARDA2
043600951219     C                   MOVEL     'C'           TIPANN                         *PERIODO CORRENTE
043700951219E   1C                   ENDIF
043800951018     C*--------------------
043900951018     C* SELEZIONI
044000951018     C*--------------------
044100951018     C*
044200951018     C* SE RAGGRUPPAMENTO AGENTE <> RAGGRUPPAMENTO RICHIESTO, ESCLUDO
044300951018IF  1C     PARCOF        IFGT      *ZEROS                                       *DELTA COMMERCIALE
044400951018     C     WFDRGF        ANDNE     PARCOF
044500951018     C                   MOVEL     'N'           WRECOK
044600951018E   1C                   ENDIF
044700951018     C*
044800951018     C* SE DIVISIONE AGENTE <> DIVISIONE RICHIESTA, ESCLUDO
044900951018IF  1C     PARDIF        IFNE      *BLANKS                                      *DELTA COMMERCIALE
045000951018     C     WFDDIC        ANDNE     PARDIF
045100951018     C                   MOVEL     'N'           WRECOK
045200951018E   1C                   ENDIF
045300960710     C*
045400960710     C* SE CODICE QUALITA' CLIENTE <> CODICE QUALITA' RICHIESTO, ESCLUDO
045500010618IF  1C                   IF        (PARCLV <> *blanks and WFDCLV = PARCLV) or
045600010618     C                             (PARCL1 <> *blanks and WFDCLV = PARCL1) or
045700010619     C                             (PARCL2 <> *blanks and WFDCLV = PARCL2) or
045800020220     C                             (PARCL3 <> *blanks and WFDCLV = PARCL3) or
045900020220     C                             (PARCL4 <> *blanks and WFDCLV = PARCL4) or
046000010619     C                             (PARCLV = *blanks  and
046100010619     C                              PARCL1 = *blanks  and
046200020220     C                              PARCL2 = *blanks  and
046300020220     C                              PARCL3 = *blanks  and
046400020220     C                              PARCL4 = *blanks)
046500010618     C                   ELSE
046600960710     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046700960710E   1C                   ENDIF
046800000000     C*
046900950208     C* ESCLUSIONI PER DELTA ITALIA O ESTERO
047000950210IF  1C     PARIOE        IFNE      *BLANKS
047100950210     C     PARIOE        ANDNE     WFDIOE
047200950208     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047300950208E   1C                   ENDIF
047400000707     C*
047500000707     C* SE DATI NO POSTE ESCLUDE LE FILIALI POSTE, E VICEVERSA
047600000707IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
047700000707     C                   SETOFF                                           28
047800000707     C                   Z-ADD     1             I                 5 0
047900000707     C     WFDFIL        LOOKUP    FIL(I)                                 28
048000000711IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
048100020627IF  3C     POS(I)        IFEQ      'PPT'                                         FIL = POSTE
048200000711     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048300000711E   2C                   ENDIF
048400000711X   2C                   ELSE                                                   *SI DATI POSTE
048500020627IF  3C     POS(I)        IFNE      'PPT'                                         FIL <> POSTE
048600000711     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048700000711E   3C                   ENDIF
048800000711E   2C                   ENDIF
048900000707E   1C                   ENDIF
049000980528     C*
049100000000     C                   ENDSR
049200000000     C*------------------------------------------------------------------------*
049300000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE COMMERCIALE
049400000000     C*------------------------------------------------------------------------*
049500000000     C     NEWDIV        BEGSR
049600020220     C*
049700020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
049800020220     C                   MOVEL     '0'           FLGSIDD
049900000000     C*
050000000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
050100000000     C                   MOVEL     '0'           PASDIV
050200000000     C*
050300000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
050400000000     C                   MOVEL     WFDDIC        DEPDIV
050500000000     C*
050600000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
050700000000     C                   EXSR      DECDIV
050800000000     C*
050900000000     C* AZZERA I TOTALI DELLA DIVISIONE
051000000000     C                   CLEAR                   CLD                            *NUMERO CLIENTI
051100000000     C                   CLEAR                   SPD                            *NUMERO SPEDIZIONI
051200000000     C                   CLEAR                   RRD                            *RICAVI REALI
051300000000     C                   CLEAR                   RPD                            *RICAVI PRESUNTI
051400020220     C                   CLEAR                   CLDD                           *NUMERO CLIENTI
051500020220     C                   CLEAR                   SPDD                           *NUMERO SPEDIZIONI
051600020220     C                   CLEAR                   RRDD                           *RICAVI REALI
051700020220     C                   CLEAR                   RPDD                           *RICAVI PRESUNTI
051800030204DO  1C     1             DO        5             I
051900030204     C     I             OCCUR     NTWDIVN                                      *TOT X NETWORK
052000030204     C                   CLEAR                   CLDN                           *NUMERO CLIENTI
052100030204     C                   CLEAR                   SPDN                           *NUMERO SPEDIZIONI
052200030204     C                   CLEAR                   RRDN                           *RICAVI REALI
052300030204     C                   CLEAR                   RPDN                           *RICAVI PRESUNTI
052400030204     C     I             OCCUR     NTWDIVND                                     *TOT X NETWORK CLI "D"
052500030204     C                   CLEAR                   CLDND                          *NUMERO CLIENTI
052600030204     C                   CLEAR                   SPDND                          *NUMERO SPEDIZIONI
052700030204     C                   CLEAR                   RRDND                          *RICAVI REALI
052800030204     C                   CLEAR                   RPDND                          *RICAVI PRESUNTI
052900030204E   1C                   ENDDO
053000000000     C*
053100000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
053200020220     C                   Z-ADD     *ZEROS        CTD              11 0
053300020220     C                   Z-ADD     *ZEROS        CTDD             11 0
053400020319     C                   CLEAR                   CTDN
053500020319     C                   CLEAR                   CTDDN
053600951227     C*
053700951227     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (PRIMA COLONNA)
053800951227     C                   Z-ADD     *ZEROS        C2D              11 0
053900020220     C                   Z-ADD     *ZEROS        C2DD             11 0
054000020319     C                   CLEAR                   C2DN
054100020319     C                   CLEAR                   C2DDN
054200000000     C*
054300000000     C                   ENDSR
054400000000     C*------------------------------------------------------------------------*
054500000000     C*?NEWRGF - OPERAZIONI PER NUOVO RAGGRUPPAMENTO FILIALE AGENTE
054600000000     C*------------------------------------------------------------------------*
054700000000     C     NEWRGF        BEGSR
054800020220     C*
054900020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
055000020220     C                   MOVEL     '0'           FLGSIDR
055100000000     C*
055200000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
055300000000     C                   MOVEL     '0'           PASRGF
055400000000     C*
055500000000     C* MEMORIZZO IL NUOVO RAGGRUPPAMENTO IN UN DEPOSITO
055600000000     C                   Z-ADD     WFDRGF        DEPRGF
055700000000     C*
055800000000     C* DECODIFICO IL CODICE AGENTE RAGGRUPPAMENTO
055900000000     C                   EXSR      DECRGF
056000000000     C*
056100000000     C* AZZERA I TOTALI DEL RAGGRUPPAMNTO
056200000000     C                   CLEAR                   CLR                            *NUMERO CLIENTI
056300000000     C                   CLEAR                   SPR                            *NUMERO SPEDIZIONI
056400000000     C                   CLEAR                   RRR                            *RICAVI REALI
056500000000     C                   CLEAR                   RPR                            *RICAVI PRESUNTI
056600020220     C                   CLEAR                   CLRD                           *NUMERO CLIENTI
056700020220     C                   CLEAR                   SPRD                           *NUMERO SPEDIZIONI
056800020220     C                   CLEAR                   RRRD                           *RICAVI REALI
056900020220     C                   CLEAR                   RPRD                           *RICAVI PRESUNTI
057000030204DO  1C     1             DO        5             I
057100030204     C     I             OCCUR     NTWRGFN                                      *TOT X NETWORK
057200030204     C                   CLEAR                   CLRN                           *NUMERO CLIENTI
057300030204     C                   CLEAR                   SPRN                           *NUMERO SPEDIZIONI
057400030204     C                   CLEAR                   RRRN                           *RICAVI REALI
057500030204     C                   CLEAR                   RPRN                           *RICAVI PRESUNTI
057600030204     C     I             OCCUR     NTWRGFND                                     *TOT X NETWORK CLI "D"
057700030204     C                   CLEAR                   CLRND                          *NUMERO CLIENTI
057800030204     C                   CLEAR                   SPRND                          *NUMERO SPEDIZIONI
057900030204     C                   CLEAR                   RRRND                          *RICAVI REALI
058000030204     C                   CLEAR                   RPRND                          *RICAVI PRESUNTI
058100030204E   1C                   ENDDO
058200000000     C*
058300000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
058400000000     C                   Z-ADD     *ZEROS        CTR              11 0
058500020220     C                   Z-ADD     *ZEROS        CTRD             11 0
058600020319     C                   CLEAR                   CTRN
058700020319     C                   CLEAR                   CTRDN
058800951227     C*
058900951227     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (PRIMA COLONNA)
059000951227     C                   Z-ADD     *ZEROS        C2R              11 0
059100020220     C                   Z-ADD     *ZEROS        C2RD             11 0
059200020319     C                   CLEAR                   C2RN
059300020319     C                   CLEAR                   C2RDN
059400000000     C*
059500000000     C                   ENDSR
059600000000     C*------------------------------------------------------------------------*
059700000000     C*?NEWAGE - OPERAZIONI PER NUOVO AGENTE COMMERCIALE
059800000000     C*------------------------------------------------------------------------*
059900000000     C     NEWAGE        BEGSR
060000020220     C*
060100020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
060200020220     C                   MOVEL     '0'           FLGSIDA
060300000000     C*
060400000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
060500000000     C                   MOVEL     '0'           PASAGE
060600000000     C*
060700000000     C* MEMORIZZO IL NUOVO AGENTE IN UN DEPOSITO
060800000000     C                   Z-ADD     WFDAGE        DEPAGE
060900000000     C*
061000000000     C* DECODIFICO LA FILAILE DEL CODICE AGENTE
061100000000     C                   MOVEL     WFDAGE        DEPFIL                         *FIL+XXXX
061200000000     C                   EXSR      DECFIL
061300000000     C*
061400000000     C* AZZERA I TOTALI DELL' AGENTE
061500000000     C                   CLEAR                   CLC                            *NUMERO CLIENTI
061600000000     C                   CLEAR                   SPC                            *NUMERO SPEDIZIONI
061700000000     C                   CLEAR                   RRC                            *RICAVI REALI
061800000000     C                   CLEAR                   RPC                            *RICAVI PRESUNTI
061900020220     C                   CLEAR                   CLCD                           *NUMERO CLIENTI
062000020220     C                   CLEAR                   SPCD                           *NUMERO SPEDIZIONI
062100020220     C                   CLEAR                   RRCD                           *RICAVI REALI
062200020220     C                   CLEAR                   RPCD                           *RICAVI PRESUNTI
062300030204DO  1C     1             DO        5             I
062400030204     C     I             OCCUR     NTWAGEN                                      *TOT X NETWORK
062500030204     C                   CLEAR                   CLCN                           *NUMERO CLIENTI
062600030204     C                   CLEAR                   SPCN                           *NUMERO SPEDIZIONI
062700030204     C                   CLEAR                   RRCN                           *RICAVI REALI
062800030204     C                   CLEAR                   RPCN                           *RICAVI PRESUNTI
062900030204     C     I             OCCUR     NTWAGEND                                     *TOT X NETWORK CLI "D"
063000030204     C                   CLEAR                   CLCND                          *NUMERO CLIENTI
063100030204     C                   CLEAR                   SPCND                          *NUMERO SPEDIZIONI
063200030204     C                   CLEAR                   RRCND                          *RICAVI REALI
063300030204     C                   CLEAR                   RPCND                          *RICAVI PRESUNTI
063400030204E   1C                   ENDDO
063500000000     C*
063600000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
063700000000     C                   Z-ADD     *ZEROS        CTC              11 0
063800020220     C                   Z-ADD     *ZEROS        CTCD             11 0
063900020319     C                   CLEAR                   CTCN
064000020319     C                   CLEAR                   CTCDN
064100951227     C*
064200951227     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (PRIMA COLONNA)
064300951227     C                   Z-ADD     *ZEROS        C2C              11 0
064400020220     C                   Z-ADD     *ZEROS        C2CD             11 0
064500020319     C                   CLEAR                   C2CN
064600020319     C                   CLEAR                   C2CDN
064700000000     C*
064800000000     C                   ENDSR
064900000000     C*------------------------------------------------------------------------*
065000000000     C*?NEWKSC - OPERAZIONI PER NUOVO CLIENTE
065100000000     C*------------------------------------------------------------------------*
065200000000     C     NEWKSC        BEGSR
065300000000     C*
065400000000     C* NB: QUESTA ROTTURA SERVE SOLO PER CONTARE IL TOTALE DEI CLIENTI SERVITI
065500951227     C*     NEL PERIODO (PER REIMPOSTARE I CONTATORI "FLAKSC"E "FL2KSC")
065600000000     C*
065700000000     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (13^ COLONNA)
065800000000     C                   MOVEL     '0'           FLAKSC
065900951227     C*
066000951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (1^ COLONNA)
066100951227     C                   MOVEL     '0'           FL2KSC
066200000000     C*
066300000000     C* MEMORIZZO IL NUOVO CLIENTE IN UN DEPOSITO
066400000825     C                   Z-ADD     WFDCLI        DEPCLI
066500000830     C*
066600000830     C* REIMPOSTO FLAG CONTATORE CLIENTI PER MESE
066700000830     C                   MOVEL     '0'           SFCCL
066800020319     C*
066900020319     C* RE-INIZIALIZZO LA SCHIERA DI WRK X CLIENTE/MESE/NETWORK
067000020319     C                   Z-ADD     *ZEROS        Y
067100020319     C                   CLEAR                   WSK1
067200000000     C*
067300000000     C                   ENDSR
067400000000     C*------------------------------------------------------------------------*
067500000000     C*?MEMSAL - MEMORIZZA I SALDI DEL WORK FILE IN SCHIERE
067600000000     C*------------------------------------------------------------------------*
067700000000     C     MEMSAL        BEGSR
067800000000     C*
067900951219     C* I SALDI DEL PERIODO PRECEDENTE LI MEMORIZZA IN 13^ POSIZIONE DI SCHIERA
068000951219     C*
068100000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
068200000000     C                   MOVEL     '1'           PASAGE                         *AGENTE
068300000000     C                   MOVEL     '1'           PASRGF                         *RAGGRUPPAMENTO
068400000000     C                   MOVEL     '1'           PASDIV                         *DIVISIONE
068500951227     C*
068600951227     C* CONTA I CLIENTI DA STAMPARE ALLA 1^ O 13^ COLONNA SOLO A ROTTURA DI
068700951227     C* CLIENTE (SE STA LEGGENDO I SALDI DEL PERIODO PRECEDENTE O CORRENTE)
068800951227IF  1C     TIPANN        IFEQ      'C'                                          *CORRENTE
068900951227IF  2C     FLAKSC        IFEQ      '0'                                          *ROTTURA CLIENTE
069000951227     C                   MOVEL     '1'           FLAKSC
069100951227     C                   Z-ADD     1             CONKSC            1 0          *CLIENTE NON AGGIUNT
069200951227X   2C                   ELSE
069300951227     C                   Z-ADD     *ZEROS        CONKSC                         *CLIENTE AGGIUNTO
069400951227E   2C                   ENDIF
069500951227X   1C                   ELSE                                                   *PRECEDENTE
069600951227IF  2C     FL2KSC        IFEQ      '0'                                          *ROTTURA CLIENTE
069700951227     C                   MOVEL     '1'           FL2KSC
069800951227     C                   Z-ADD     1             CO2KSC            1 0          *CLIENTE AGGIUNTO
069900951227X   2C                   ELSE
070000951227     C                   Z-ADD     *ZEROS        CO2KSC                         *CLIENTE NON AGGIUNT
070100951227E   2C                   ENDIF
070200951227E   1C                   ENDIF
070300951219     C* M E S E
070400990609IF  2C     WFDDAT        IFGE      PARDPT                                       *PERIODO PRECEDENTE
070500990609     C     WFDDAT        ANDLE     PARDP2
070600951219     C                   Z-ADD     13            MM
070700951219E   2C                   ENDIF
070800990609IF  2C     WFDDAT        IFGE      PARDAT                                       *PERIODO CORRENTE
070900990609     C     WFDDAT        ANDLE     PARDA2
071000951219     C                   Z-ADD     1             MM
071100951219     C                   SETOFF                                           28
071200951219     C     WFDMES        LOOKUP    MES(MM)                                28
071300951219E   2C                   ENDIF
071400020220     C*
071500020222     C* VERIFICO SE ALMENO 1 CLIENTE "D"
071600020220     C                   IF        WFDCLV = 'D'
071700020220     C                   MOVEL     '1'           FLGSIDA           1
071800020220     C                   MOVEL     '1'           FLGSIDR           1
071900020220     C                   MOVEL     '1'           FLGSIDD           1
072000020220     C                   ENDIF
072100020319     C*
072200020319     C* VERIFICO SE X GIA' CONTEGGIATO IL CLIENTE/MESE/NETWORK
072300020319     C     DSWSK1        LOOKUP    WSK1                                   55
072400020319     C                   IF        *IN55 = *ON
072500020319     C                   Z-ADD     *ZEROS        CONNTW            1 0
072600020319     C                   ELSE
072700020319     C                   ADD       1             Y                 3 0
072800020319     C                   EVAL      WSK1(Y) = DSWSK1
072900020319     C                   Z-ADD     1             CONNTW
073000020319     C                   ENDIF
073100020319     C*
073200020319     C                   Z-ADD     1             K                 1 0
073300030312     C     WFDNTW        LOOKUP    DNTW(K)                                27
073400030312     C     *IN27         IFEQ      *OFF
073500020319     C                   EVAL      K = 1
073600020319     C                   ENDIF
073700000000     C*--------------------
073800020220     C* C O M M E R C I A L E
073900000000     C*--------------------
074000020319     C     K             OCCUR     NTWAGEN
074100020319     C     K             OCCUR     NTWAGEND
074200000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
074300000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
074400020220     C                   IF        WFDCLV = 'D'
074500020220     C                   ADD       1             CLCD(MM)                       NUMERO CLIENTI
074600020220     C                   ELSE
074700020220     C                   ADD       1             CLC(MM)                        NUMERO CLIENTI
074800060104     C********           ADD       1             CLCD(MM)                       NUMERO CLIENTI
074900020220     C                   ENDIF
075000000830E   3C                   ENDIF
075100020319     C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
075200020319     C                   IF        WFDCLV = 'D'
075300020319     C                   ADD       CONNTW        CLCND(MM)                      NUMERO CLIENTI
075400020319     C                   ELSE
075500020319     C                   ADD       CONNTW        CLCN(MM)                       NUMERO CLIENTI
075600060104     C************       ADD       CONNTW        CLCND(MM)                      NUMERO CLIENTI
075700020319     C                   ENDIF
075800020319     C                   ENDIF
075900020220     C                   IF        WFDCLV = 'D'
076000020220     C                   ADD       WFDNSP        SPCD(MM)                       NUMERO SPEDIZIONI
076100020220     C                   ADD       WFDRRN        RRCD(MM)                       RICAVI REALI DELTA
076200020220     C                   ADD       WFDRPN        RPCD(MM)                       RICAVI PRESUNTI DELT
076300020319     C                   ADD       WFDNSP        SPCND(MM)                      NUMERO SPEDIZIONI
076400020319     C                   ADD       WFDRRN        RRCND(MM)                      RICAVI REALI DELTA
076500020319     C                   ADD       WFDRPN        RPCND(MM)                      RICAVI PRESUNTI DELT
076600020220     C                   ELSE
076700000000     C                   ADD       WFDNSP        SPC(MM)                        NUMERO SPEDIZIONI
076800000000     C                   ADD       WFDRRN        RRC(MM)                        RICAVI REALI DELTA
076900000000     C                   ADD       WFDRPN        RPC(MM)                        RICAVI PRESUNTI DELT
077000060104     C*******            ADD       WFDNSP        SPCD(MM)                       NUMERO SPEDIZIONI
077100060104     C*******            ADD       WFDRRN        RRCD(MM)                       RICAVI REALI DELTA
077200060104     C*******            ADD       WFDRPN        RPCD(MM)                       RICAVI PRESUNTI DELT
077300020319     C                   ADD       WFDNSP        SPCN(MM)                       NUMERO SPEDIZIONI
077400020319     C                   ADD       WFDRRN        RRCN(MM)                       RICAVI REALI DELTA
077500020319     C                   ADD       WFDRPN        RPCN(MM)                       RICAVI PRESUNTI DELT
077600060104     C*******            ADD       WFDNSP        SPCND(MM)                      NUMERO SPEDIZIONI
077700060104     C*******            ADD       WFDRRN        RRCND(MM)                      RICAVI REALI DELTA
077800060104     C*******            ADD       WFDRPN        RPCND(MM)                      RICAVI PRESUNTI DELT
077900020220     C                   ENDIF
078000951227IF  2C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
078100020220     C                   IF        WFDCLV = 'D'
078200020319     C                   ADD       CO2KSC        C2CD
078300020319     C                   ADD       CONNTW        C2CDN(K)
078400020220     C                   ELSE
078500020220     C                   ADD       CO2KSC        C2C
078600060104     C*******            ADD       CO2KSC        C2CD
078700020319     C                   ADD       CONNTW        C2CN(K)
078800060104     C*******            ADD       CONNTW        C2CDN(K)
078900020220     C                   ENDIF
079000020220X   2C                   ELSE                                                   *PERIODO CORRENTE
079100020220     C                   IF        WFDCLV = 'D'
079200020319     C                   ADD       CONKSC        CTCD
079300020319     C                   ADD       CONNTW        CTCDN(K)
079400020220     C                   ELSE
079500020220     C                   ADD       CONKSC        CTC
079600060104     C********           ADD       CONKSC        CTCD
079700020319     C                   ADD       CONNTW        CTCN(K)
079800060104     C********           ADD       CONNTW        CTCDN(K)
079900020220     C                   ENDIF
080000951227E   2C                   ENDIF
080100000000     C*--------------------
080200000000     C* R A G G R U P P A M E N T O
080300000000     C*--------------------
080400020319     C     K             OCCUR     NTWRGFN
080500020319     C     K             OCCUR     NTWRGFND
080600000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
080700000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
080800020220     C                   IF        WFDCLV = 'D'
080900020220     C                   ADD       1             CLRD(MM)                       NUMERO CLIENTI
081000020220     C                   ELSE
081100020220     C                   ADD       1             CLR(MM)                        NUMERO CLIENTI
081200060104     C**********         ADD       1             CLRD(MM)                       NUMERO CLIENTI
081300020220     C                   ENDIF
081400000830E   3C                   ENDIF
081500020319     C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
081600020319     C                   IF        WFDCLV = 'D'
081700020319     C                   ADD       CONNTW        CLRND(MM)                      NUMERO CLIENTI
081800020319     C                   ELSE
081900020319     C                   ADD       CONNTW        CLRN(MM)                       NUMERO CLIENTI
082000060104     C***********        ADD       CONNTW        CLRND(MM)                      NUMERO CLIENTI
082100020319     C                   ENDIF
082200020319     C                   ENDIF
082300020220     C                   IF        WFDCLV = 'D'
082400020220     C                   ADD       WFDNSP        SPRD(MM)                       NUMERO SPEDIZIONI
082500020220     C                   ADD       WFDRRN        RRRD(MM)                       RICAVI REALI DELTA
082600020220     C                   ADD       WFDRPN        RPRD(MM)                       RICAVI PRESUNTI DELT
082700020319     C                   ADD       WFDNSP        SPRND(MM)                      NUMERO SPEDIZIONI
082800020319     C                   ADD       WFDRRN        RRRND(MM)                      RICAVI REALI DELTA
082900020319     C                   ADD       WFDRPN        RPRND(MM)                      RICAVI PRESUNTI DELT
083000020220     C                   ELSE
083100000000     C                   ADD       WFDNSP        SPR(MM)                        NUMERO SPEDIZIONI
083200000000     C                   ADD       WFDRRN        RRR(MM)                        RICAVI REALI DELTA
083300000000     C                   ADD       WFDRPN        RPR(MM)                        RICAVI PRESUNTI DELT
083400060104     C*******            ADD       WFDNSP        SPRD(MM)                       NUMERO SPEDIZIONI
083500060104     C*******            ADD       WFDRRN        RRRD(MM)                       RICAVI REALI DELTA
083600060104     C*******            ADD       WFDRPN        RPRD(MM)                       RICAVI PRESUNTI DELT
083700020319     C                   ADD       WFDNSP        SPRN(MM)                       NUMERO SPEDIZIONI
083800020319     C                   ADD       WFDRRN        RRRN(MM)                       RICAVI REALI DELTA
083900020319     C                   ADD       WFDRPN        RPRN(MM)                       RICAVI PRESUNTI DELT
084000060104     C*******            ADD       WFDNSP        SPRND(MM)                      NUMERO SPEDIZIONI
084100060104     C*******            ADD       WFDRRN        RRRND(MM)                      RICAVI REALI DELTA
084200060104     C*******            ADD       WFDRPN        RPRND(MM)                      RICAVI PRESUNTI DELT
084300020220     C                   ENDIF
084400951227IF  2C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
084500020220     C                   IF        WFDCLV = 'D'
084600020220     C                   ADD       CO2KSC        C2RD
084700020319     C                   ADD       CONNTW        C2RDN(K)
084800020220     C                   ELSE
084900951227     C                   ADD       CO2KSC        C2R
085000060104     C********           ADD       CO2KSC        C2RD
085100020319     C                   ADD       CONNTW        C2RN(K)
085200060104     C********           ADD       CONNTW        C2RDN(K)
085300020220     C                   ENDIF
085400951227X   2C                   ELSE                                                   *PERIODO CORRENTE
085500020220     C                   IF        WFDCLV = 'D'
085600020220     C                   ADD       CONKSC        CTRD
085700020319     C                   ADD       CONNTW        CTRDN(K)
085800020220     C                   ELSE
085900020220     C                   ADD       CONKSC        CTR
086000060104     C********           ADD       CONKSC        CTRD
086100020319     C                   ADD       CONNTW        CTRN(K)
086200060104     C********           ADD       CONNTW        CTRDN(K)
086300020220     C                   ENDIF
086400951227E   2C                   ENDIF
086500000000     C*--------------------
086600000000     C* D I V I S I O N E
086700000000     C*--------------------
086800020319     C     K             OCCUR     NTWDIVN
086900020319     C     K             OCCUR     NTWDIVND
087000000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
087100000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
087200020220     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
087300020220     C                   IF        WFDCLV = 'D'
087400020220     C                   ADD       1             CLDD(MM)                       NUMERO CLIENTI
087500020220     C                   ELSE
087600020220     C                   ADD       1             CLD(MM)                        NUMERO CLIENTI
087700060104     C********           ADD       1             CLDD(MM)                       NUMERO CLIENTI
087800020220     C                   ENDIF
087900000830E   3C                   ENDIF
088000020319     C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
088100020319     C                   IF        WFDCLV = 'D'
088200020319     C                   ADD       CONNTW        CLDND(MM)                      NUMERO CLIENTI
088300020319     C                   ELSE
088400020319     C                   ADD       CONNTW        CLDN(MM)                       NUMERO CLIENTI
088500060104     C*********          ADD       CONNTW        CLDND(MM)                      NUMERO CLIENTI
088600020319     C                   ENDIF
088700020319     C                   ENDIF
088800020220     C                   IF        WFDCLV = 'D'
088900020220     C                   ADD       WFDNSP        SPDD(MM)                       NUMERO SPEDIZIONI
089000020220     C                   ADD       WFDRRN        RRDD(MM)                       RICAVI REALI DELTA
089100020220     C                   ADD       WFDRPN        RPDD(MM)                       RICAVI PRESUNTI DELT
089200020319     C                   ADD       WFDNSP        SPDND(MM)                      NUMERO SPEDIZIONI
089300020319     C                   ADD       WFDRRN        RRDND(MM)                      RICAVI REALI DELTA
089400020319     C                   ADD       WFDRPN        RPDND(MM)                      RICAVI PRESUNTI DELT
089500020220     C                   ELSE
089600000000     C                   ADD       WFDNSP        SPD(MM)                        NUMERO SPEDIZIONI
089700000000     C                   ADD       WFDRRN        RRD(MM)                        RICAVI REALI DELTA
089800000000     C                   ADD       WFDRPN        RPD(MM)                        RICAVI PRESUNTI DELT
089900060104     C*******            ADD       WFDNSP        SPDD(MM)                       NUMERO SPEDIZIONI
090000060104     C*******            ADD       WFDRRN        RRDD(MM)                       RICAVI REALI DELTA
090100060104     C*******            ADD       WFDRPN        RPDD(MM)                       RICAVI PRESUNTI DELT
090200020319     C                   ADD       WFDNSP        SPDN(MM)                       NUMERO SPEDIZIONI
090300020319     C                   ADD       WFDRRN        RRDN(MM)                       RICAVI REALI DELTA
090400020319     C                   ADD       WFDRPN        RPDN(MM)                       RICAVI PRESUNTI DELT
090500060104     C*******            ADD       WFDNSP        SPDND(MM)                      NUMERO SPEDIZIONI
090600060104     C*******            ADD       WFDRRN        RRDND(MM)                      RICAVI REALI DELTA
090700060104     C*******            ADD       WFDRPN        RPDND(MM)                      RICAVI PRESUNTI DELT
090800020220     C                   ENDIF
090900951227IF  2C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
091000020220     C                   IF        WFDCLV = 'D'
091100020220     C                   ADD       CO2KSC        C2DD
091200020319     C                   ADD       CONNTW        C2DDN(K)
091300020220     C                   ELSE
091400020220     C                   ADD       CO2KSC        C2D
091500060104     C*********          ADD       CO2KSC        C2DD
091600020319     C                   ADD       CONNTW        C2DN(K)
091700060104     C*********          ADD       CONNTW        C2DDN(K)
091800020220     C                   ENDIF
091900951227X   2C                   ELSE                                                   *PERIODO CORRENTE
092000020220     C                   IF        WFDCLV = 'D'
092100020220     C                   ADD       CONKSC        CTDD
092200020319     C                   ADD       CONNTW        CTDDN(K)
092300020220     C                   ELSE
092400951227     C                   ADD       CONKSC        CTD
092500060104     C*******            ADD       CONKSC        CTDD
092600020319     C                   ADD       CONNTW        CTDN(K)
092700060104     C*******            ADD       CONNTW        CTDDN(K)
092800020220     C                   ENDIF
092900951227E   2C                   ENDIF
093000000000     C*
093100000000     C                   ENDSR
093200000000     C*------------------------------------------------------------------------*
093300000000     C*?STAAGE - IMPOSTA STAMPA VALORI AGENTE
093400000000     C*------------------------------------------------------------------------*
093500000000     C     STAAGE        BEGSR
093600020221     C*
093700020222     C                   MOVEL(P)  '*'           DESFLD            2
093800000000     C*
093900010618     C* STAMPA I VALORI DELL'AGENTE SOLO SE HO MEMORIZZATO QUALCOSA
094000000000IF  1C     PASAGE        IFEQ      '1'
094100000000     C***
094200000000     C* DESCRIZIONI INTESTAZIONE
094300000000     C***
094400020116     C                   CLEAR                   SE94I2
094500000000     C                   Z-ADD     DEPRGF        I2CRGF                         RAGGRUPPAMENTO AGENT
094600000000     C                   MOVEL     DESRGF        I2DRGF                         DESCRIZIONE RAGGR AG
094700000000     C                   MOVEL     DEPDIV        I2CDIV                         *DIVISIONE
094800000000     C                   MOVEL     DESDIV        I2DDIV                         DESCRIZIONE DIVISION
094900950301     C                   MOVEL     PADIOE        I2DIOE                         *DELTA ITALIA/ESTERO
095000000000     C*
095100020220     C                   MOVEL(p)  DRI(21)       PIDDES
095200000000     C*
095300000000     C                   MOVEL     DESFIL        DES15            15            *FILIALE DA IMPOSTAR
095400000000     C*
095500000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
095600000000     C                   Z-ADD     CLC           CLS                            NUMERO CLIENTI
095700000000     C                   Z-ADD     SPC           SPS                            NUMERO SPEDIZIONI
095800000000     C                   Z-ADD     RRC           RRS                            RICAVI REALI DELTA
095900000000     C                   Z-ADD     RPC           RPS                            RICAVI PRESUNTI DELT
096000951227     C                   Z-ADD     CTC           CTS              11 0          CLIENTI TOTALI 13^CO
096100951227     C                   Z-ADD     C2C           C2S              11 0          CLIENTI TOTALI 1^ CO
096200000000     C*
096300000000     C* ROUTINE DI STAMPA TOTALI
096400000000     C                   EXSR      STAMPA
096500020220     C*
096600020220     C* VERIFICO SE TOTALIZZATO ALMENO 1 CLIENTE 'D'
096700020220     C                   IF        FLGSIDA = '1'
096800020220     C*
096900020221     C                   EVAL      DES15 = DESFIL
097000020222     C                   MOVEL(P)  '**'          DESFLD
097100020220     C*
097200020220     C* SCHIERE MEMORIZZAZIONE CLIENTI 'D' --> SCHIERE STAMPA
097300020220     C                   Z-ADD     CLCD          CLS                            NUMERO CLIENTI
097400020220     C                   Z-ADD     SPCD          SPS                            NUMERO SPEDIZIONI
097500020220     C                   Z-ADD     RRCD          RRS                            RICAVI REALI DELTA
097600020220     C                   Z-ADD     RPCD          RPS                            RICAVI PRESUNTI DELT
097700020220     C                   Z-ADD     CTCD          CTS              11 0          CLIENTI TOTALI 13^CO
097800020220     C                   Z-ADD     C2CD          C2S              11 0          CLIENTI TOTALI 1^ CO
097900020220     C*
098000020220     C* ROUTINE DI STAMPA TOTALI
098100020220     C                   EXSR      STAMPA
098200020220     C*
098300020220     C                   ENDIF
098400000000     C*
098500000000E   1C                   ENDIF
098600000000     C*
098700000000     C                   ENDSR
098800000000     C*------------------------------------------------------------------------*
098900020220     C*?STARGF - IMPOSTA STAMPA VALORI RAGGRUPPAMENTO FILIALE AGENTE
099000000000     C*------------------------------------------------------------------------*
099100000000     C     STARGF        BEGSR
099200020222     C*
099300020222     C                   MOVEL(P)  '*'           DESFLD
099400000000     C*
099500000000     C* STAMPA I VALORI DEL RAGGRUPPAMENTO SOLO SE HO MEMORIZZATO QUALCOSA
099600000000IF  1C     PASRGF        IFEQ      '1'
099700000000     C***
099800000000     C* DESCRIZIONI
099900000000     C***
100000000000     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
100100000000     C*
100200000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
100300000000     C                   Z-ADD     CLR           CLS                            NUMERO CLIENTI
100400000000     C                   Z-ADD     SPR           SPS                            NUMERO SPEDIZIONI
100500000000     C                   Z-ADD     RRR           RRS                            RICAVI REALI DELTA
100600000000     C                   Z-ADD     RPR           RPS                            RICAVI PRESUNTI DELT
100700951227     C                   Z-ADD     CTR           CTS                            CLIENTI TOTALI 13^CO
100800951227     C                   Z-ADD     C2R           C2S                            CLIENTI TOTALI 1^ CO
100900000000     C*
101000000000     C* ROUTINE DI STAMPA TOTALI
101100000000     C                   EXSR      STAMPA
101200020319     C*
101300020319     C* STAMPO L'EVIDENZIAZIONE DEI NETWORK
101400020404     C*
101500020404     C* SOLO SE RICHIESTO NEL LANCIO
101600020404     C                   IF        PARNTW = 'S'
101700020404     C*
101800030312DO  1C     1             DO        5             K
101900020319     C     K             OCCUR     NTWRGFN
102000020319     C                   Z-ADD     CLRN          CLS                            NUMERO CLIENTI
102100020319     C                   Z-ADD     SPRN          SPS                            NUMERO SPEDIZIONI
102200020319     C                   Z-ADD     RRRN          RRS                            RICAVI REALI DELTA
102300020319     C                   Z-ADD     RPRN          RPS                            RICAVI PRESUNTI DELT
102400020319     C                   Z-ADD     CTRN(K)       CTS                            CLIENTI TOTALI 13^CO
102500020319     C                   Z-ADD     C2RN(K)       C2S                            CLIENTI TOTALI 1^ CO
102600020319     C*
102700020319     C* DESCRIZIONI IN STAMPA
102800020319     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
102900020319     C                   MOVE      DNTW(K)       DES15
103000020319     C*
103100020319     C* VERIFICO CHE VI SIA QUALCOSA DA MEMORIZZARE
103200020319     C                   XFOOT     CLS           WCLS             11 0
103300020319     C                   IF        WCLS > *ZEROS
103400020319     C*
103500020319     C* ROUTINE DI STAMPA TOTALI
103600020319     C                   EXSR      STAMPA
103700020319     C                   ENDIF
103800020319     C                   ENDDO
103900020404     C*
104000020404     C                   ENDIF
104100020220     C*
104200020220     C* VERIFICO SE TOTALIZZATO ALMENO 1 CLIENTE 'D'
104300020220     C                   IF        FLGSIDR = '1'
104400020220     C***
104500020220     C* DESCRIZIONI
104600020220     C***
104700020222     C                   MOVEL(P)  '**'          DESFLD
104800020220     C                   MOVEL     DRI(17)       DES15            15            "TOTALI" DA IMPOSTAR
104900000000     C*
105000020220     C* SCHIERE MEMORIZZAZIONE CLIENTI 'D' --> SCHIERE STAMPA
105100020220     C                   Z-ADD     CLRD          CLS                            NUMERO CLIENTI
105200020220     C                   Z-ADD     SPRD          SPS                            NUMERO SPEDIZIONI
105300020220     C                   Z-ADD     RRRD          RRS                            RICAVI REALI DELTA
105400020220     C                   Z-ADD     RPRD          RPS                            RICAVI PRESUNTI DELT
105500020220     C                   Z-ADD     CTRD          CTS                            CLIENTI TOTALI 13^CO
105600020220     C                   Z-ADD     C2RD          C2S                            CLIENTI TOTALI 1^ CO
105700020220     C*
105800020220     C* ROUTINE DI STAMPA TOTALI
105900020220     C                   EXSR      STAMPA
106000020319     C*
106100020319     C* STAMPO L'EVIDENZIAZIONE DEI NETWORK
106200020404     C*
106300020404     C* SOLO SE RICHIESTO NEL LANCIO
106400020404     C                   IF        PARNTW = 'S'
106500020404     C*
106600030312DO  1C     1             DO        5             K
106700020319     C     K             OCCUR     NTWRGFND
106800020319     C                   Z-ADD     CLRND         CLS                            NUMERO CLIENTI
106900020319     C                   Z-ADD     SPRND         SPS                            NUMERO SPEDIZIONI
107000020319     C                   Z-ADD     RRRND         RRS                            RICAVI REALI DELTA
107100020319     C                   Z-ADD     RPRND         RPS                            RICAVI PRESUNTI DELT
107200020319     C                   Z-ADD     CTRDN(K)      CTS                            CLIENTI TOTALI 13^CO
107300020319     C                   Z-ADD     C2RDN(K)      C2S                            CLIENTI TOTALI 1^ CO
107400020319     C*
107500020319     C* DESCRIZIONI IN STAMPA
107600020319     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
107700020319     C                   MOVE      DNTW(K)       DES15
107800020319     C*
107900020319     C* VERIFICO CHE VI SIA QUALCOSA DA MEMORIZZARE
108000020319     C                   XFOOT     CLS           WCLS             11 0
108100020319     C                   IF        WCLS > *ZEROS
108200020319     C*
108300020319     C* ROUTINE DI STAMPA TOTALI
108400020319     C                   EXSR      STAMPA
108500020319     C                   ENDIF
108600020319     C                   ENDDO
108700020220     C*
108800020220     C                   ENDIF
108900020404     C*
109000020404     C                   ENDIF
109100020220     C*
109200000000E   1C                   ENDIF
109300000000     C*
109400020116     C                   WRITE     SE94R2                                       *RIGA DI SEPARAZIONE
109500020220     C                   WRITE     SE94LE                                       *RIGA DI LEGENDA
109600000000     C*
109700000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
109800000000     C                   SETON                                        01
109900000000     C*
110000000000     C                   ENDSR
110100000000     C*------------------------------------------------------------------------*
110200020220     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
110300000000     C*------------------------------------------------------------------------*
110400000000     C     STADIV        BEGSR
110500020221     C*
110600020222     C                   MOVEL(P)  '*'           DESFLD
110700000000     C*
110800000000     C* STAMPA I VALORI DELLA DIVISIONE SOLO SE HO MEMORIZZATO QUALCOSA
110900000000IF  1C     PASDIV        IFEQ      '1'
111000000000     C***
111100000000     C* DESCRIZIONI
111200000000     C***
111300020116     C                   CLEAR                   SE94I2
111400000000     C                   Z-ADD     *ZEROS        I2CRGF                         RAGGRUPPAMENTO AGENT
111500000000     C                   MOVEL     DRI(1)        I2DRGF                         DESCRIZIONE RAGGR AG
111600000000     C                   MOVEL     DEPDIV        I2CDIV                         *DIVISIONE
111700000000     C                   MOVEL     DESDIV        I2DDIV                         DESCRIZIONE DIVISION
111800950313     C                   MOVEL     PADIOE        I2DIOE                         *DELTA ITALIA/ESTERO
111900000000     C*
112000020220     C                   MOVEL(p)  DRI(24)       PIDDES
112100000000     C*
112200000000     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
112300000000     C*
112400000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
112500000000     C                   Z-ADD     CLD           CLS                            NUMERO CLIENTI
112600000000     C                   Z-ADD     SPD           SPS                            NUMERO SPEDIZIONI
112700000000     C                   Z-ADD     RRD           RRS                            RICAVI REALI DELTA
112800000000     C                   Z-ADD     RPD           RPS                            RICAVI PRESUNTI DELT
112900951227     C                   Z-ADD     CTD           CTS                            CLIENTI TOTALI 13^CO
113000951227     C                   Z-ADD     C2D           C2S                            CLIENTI TOTALI 1^ CO
113100000000     C*
113200000000     C* ROUTINE DI STAMPA TOTALI
113300000000     C                   EXSR      STAMPA
113400020319     C*
113500020404     C* STAMPO L'EVIDENZIAZIONE DEI NETWORK
113600020404     C*
113700020404     C* SOLO SE RICHIESTO NEL LANCIO
113800020404     C                   IF        PARNTW = 'S'
113900020404     C*
114000030312DO  1C     1             DO        5             K
114100020319     C     K             OCCUR     NTWDIVN
114200020319     C                   Z-ADD     CLDN          CLS                            NUMERO CLIENTI
114300020319     C                   Z-ADD     SPDN          SPS                            NUMERO SPEDIZIONI
114400020319     C                   Z-ADD     RRDN          RRS                            RICAVI REALI DELTA
114500020319     C                   Z-ADD     RPDN          RPS                            RICAVI PRESUNTI DELT
114600020319     C                   Z-ADD     CTDN(K)       CTS                            CLIENTI TOTALI 13^CO
114700020319     C                   Z-ADD     C2DN(K)       C2S                            CLIENTI TOTALI 1^ CO
114800020319     C*
114900020319     C* DESCRIZIONI IN STAMPA
115000020319     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
115100020319     C                   MOVE      DNTW(K)       DES15
115200020319     C*
115300020319     C* VERIFICO CHE VI SIA QUALCOSA DA MEMORIZZARE
115400020319     C                   XFOOT     CLS           WCLS             11 0
115500020319     C                   IF        WCLS > *ZEROS
115600020319     C*
115700020319     C* ROUTINE DI STAMPA TOTALI
115800020319     C                   EXSR      STAMPA
115900020319     C                   ENDIF
116000020319     C                   ENDDO
116100020404     C*
116200020404     C                   ENDIF
116300020220     C*
116400020220     C* VERIFICO SE TOTALIZZATO ALMENO 1 CLIENTE 'D'
116500020220     C                   IF        FLGSIDD = '1'
116600020220     C*
116700020222     C                   MOVEL(P)  '**'          DESFLD
116800020220     C                   MOVEL     DRI(17)       DES15            15            "TOTALI" DA IMPOSTAR
116900020220     C*
117000020220     C* SCHIERE MEMORIZZAZIONE CLIENTI 'D' --> SCHIERE STAMPA
117100020220     C                   Z-ADD     CLDD          CLS                            NUMERO CLIENTI
117200020220     C                   Z-ADD     SPDD          SPS                            NUMERO SPEDIZIONI
117300020220     C                   Z-ADD     RRDD          RRS                            RICAVI REALI DELTA
117400020220     C                   Z-ADD     RPDD          RPS                            RICAVI PRESUNTI DELT
117500020220     C                   Z-ADD     CTDD          CTS                            CLIENTI TOTALI 13^CO
117600020220     C                   Z-ADD     C2DD          C2S                            CLIENTI TOTALI 1^ CO
117700020220     C*
117800020220     C* ROUTINE DI STAMPA TOTALI
117900020220     C                   EXSR      STAMPA
118000020319     C*
118100020319     C* STAMPO L'EVIDENZIAZIONE DEI NETWORK
118200020404     C*
118300020404     C* SOLO SE RICHIESTO NEL LANCIO
118400020404     C                   IF        PARNTW = 'S'
118500020404     C*
118600030312DO  1C     1             DO        5             K
118700020319     C     K             OCCUR     NTWDIVND
118800020319     C                   Z-ADD     CLDND         CLS                            NUMERO CLIENTI
118900020319     C                   Z-ADD     SPDND         SPS                            NUMERO SPEDIZIONI
119000020319     C                   Z-ADD     RRDND         RRS                            RICAVI REALI DELTA
119100020319     C                   Z-ADD     RPDND         RPS                            RICAVI PRESUNTI DELT
119200020319     C                   Z-ADD     CTDDN(K)      CTS                            CLIENTI TOTALI 13^CO
119300020319     C                   Z-ADD     C2DDN(K)      C2S                            CLIENTI TOTALI 1^ CO
119400020319     C*
119500020319     C* DESCRIZIONI IN STAMPA
119600020319     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
119700020319     C                   MOVE      DNTW(K)       DES15
119800020319     C*
119900020319     C* VERIFICO CHE VI SIA QUALCOSA DA MEMORIZZARE
120000020319     C                   XFOOT     CLS           WCLS             11 0
120100020319     C                   IF        WCLS > *ZEROS
120200020319     C*
120300020319     C* ROUTINE DI STAMPA TOTALI
120400020319     C                   EXSR      STAMPA
120500020319     C                   ENDIF
120600020319     C                   ENDDO
120700020220     C*
120800020220     C                   ENDIF
120900020404     C*
121000020404     C                   ENDIF
121100000000     C*
121200000000E   1C                   ENDIF
121300000000     C*
121400020116     C                   WRITE     SE94R2                                       *RIGA DI SEPARAZIONE
121500020220     C                   WRITE     SE94LE                                       *RIGA DI LEGENDA
121600000000     C*
121700000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
121800000000     C                   SETON                                        01
121900000000     C*
122000000000     C                   ENDSR
122100000000     C*------------------------------------------------------------------------*
122200020220     C*?STAMPA - STAMPA I DATI
122300000000     C*------------------------------------------------------------------------*
122400000000     C     STAMPA        BEGSR
122500000000     C*--------------------
122600000000     C* I N T E S T A Z I O N E
122700000000     C*--------------------
122800000000IF  1C     *IN01         IFEQ      *ON
122900000000     C                   EXSR      STAINT
123000000000E   1C                   ENDIF
123100000000     C*--------------------
123200000000     C* C L I E N T I
123300000000     C*--------------------
123400000000     C                   MOVEL     *BLANKS       P1DDES
123500020221     C                   MOVEL     DESFLD        P1DFLD
123600000000     C                   MOVEL     DRI(13)       DES07             7
123700000000     C                   MOVEL     DES15         P1DDES
123800000000     C                   MOVE      DES07         P1DDES
123900000000     C*
124000000000     C                   Z-ADD     CLS           $VA
124100020116     C                   Z-ADD     CTS           $VA13C           19 5
124200020116     C                   Z-ADD     C2S           $V213C           19 5
124300000000     C*
124400000000     C* SCRIVE I VALORI IN STAMPA
124500000000     C                   EXSR      SCRVAL
124600030917     C*
124700030917     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
124800030917     C                   MOVEL     PARIOE        SE94IOE                        *DELTA ITALIA/ESTERO
124900030917     C                   MOVEL     DEPDIV        SE94DIV                        *DIVISIONE
125000060104      * se totali Divisione deve impostare a 0 il campo sul file
125100060104     c                   if        i2crgf = 0
125200060104     C                   Z-ADD     0             SE94RGF                        *AGENTE UNIFICANTE
125300060104     c                   else
125400030917     C                   Z-ADD     DEPRGF        SE94RGF                        *AGENTE UNIFICANTE
125500060104     c                   end
125600030918     C                   EVAL      a30 = P1DFLD + P1DDES
125700030918     C                   MOVEL     a30           SE94DES                        *DESCRIZIONE
125800030917     C                   Z-ADD     $V213C        SE94CPR                        *VALORE PRECEDENTE
125900030917     C                   Z-ADD     $VA(1)        SE94C01                        *MESE UNO
126000030917     C                   Z-ADD     $VA(2)        SE94C02                        *MESE DUE
126100030917     C                   Z-ADD     $VA(3)        SE94C03                        *MESE TRE
126200030917     C                   Z-ADD     $VA(4)        SE94C04                        *MESE QUATTRO
126300030917     C                   Z-ADD     $VA(5)        SE94C05                        *MESE CINQUE
126400030917     C                   Z-ADD     $VA(6)        SE94C06                        *MESE SEI
126500030917     C                   Z-ADD     $VA(7)        SE94C07                        *MESE SETTE
126600030917     C                   Z-ADD     $VA(8)        SE94C08                        *MESE OTTO
126700030917     C                   Z-ADD     $VA(9)        SE94C09                        *MESE NOVE
126800030917     C                   Z-ADD     $VA(10)       SE94C10                        *MESE DIECI
126900030917     C                   Z-ADD     $VA(11)       SE94C11                        *MESE UNDICI
127000030917     C                   Z-ADD     $VA(12)       SE94C12                        *MESE DODICI
127100030917     C                   Z-ADD     $VA13C        SE94CTO                        *TOTALE MESI
127200000000     C*--------------------
127300000000     C* S P E D I Z I O N I
127400000000     C*--------------------
127500000000IF  1C     *IN01         IFEQ      *ON
127600000000     C                   EXSR      STAINT
127700000000E   1C                   ENDIF
127800000000     C*
127900000000     C                   MOVEL     *BLANKS       P1DDES
128000020221     C                   MOVEL     *BLANKS       P1DFLD
128100000000     C                   MOVEL     DRI(14)       DES07             7
128200000000     C                   MOVE      DES07         P1DDES
128300000000     C*
128400000000     C                   Z-ADD     SPS           $VA
128500020116     C                   Z-ADD     *ZEROS        $VA13C
128600000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
128700000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
128800000000E   1C                   ENDDO
128900020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
129000951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
129100020220     C*
129200020220     C* SCRIVE I VALORI IN STAMPA
129300000000     C                   EXSR      SCRVAL
129400030917     C*
129500030917     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
129600030917     C                   Z-ADD     $V213C        SE94NPR                        *VALORE PRECEDENTE
129700030917     C                   Z-ADD     $VA(1)        SE94N01                        *MESE UNO
129800030917     C                   Z-ADD     $VA(2)        SE94N02                        *MESE DUE
129900030917     C                   Z-ADD     $VA(3)        SE94N03                        *MESE TRE
130000030917     C                   Z-ADD     $VA(4)        SE94N04                        *MESE QUATTRO
130100030917     C                   Z-ADD     $VA(5)        SE94N05                        *MESE CINQUE
130200030917     C                   Z-ADD     $VA(6)        SE94N06                        *MESE SEI
130300030917     C                   Z-ADD     $VA(7)        SE94N07                        *MESE SETTE
130400030917     C                   Z-ADD     $VA(8)        SE94N08                        *MESE OTTO
130500030917     C                   Z-ADD     $VA(9)        SE94N09                        *MESE NOVE
130600030917     C                   Z-ADD     $VA(10)       SE94N10                        *MESE DIECI
130700030917     C                   Z-ADD     $VA(11)       SE94N11                        *MESE UNDICI
130800030917     C                   Z-ADD     $VA(12)       SE94N12                        *MESE DODICI
130900030917     C                   Z-ADD     $VA13C        SE94NTO                        *TOTALE MESI
131000000000     C*--------------------
131100000000     C* R I C A V I
131200000000     C*--------------------
131300000000IF  1C     *IN01         IFEQ      *ON
131400000000     C                   EXSR      STAINT
131500000000E   1C                   ENDIF
131600000000     C*
131700000000     C                   MOVEL     *BLANKS       P1DDES
131800020221     C                   MOVEL     *BLANKS       P1DFLD
131900000000     C                   MOVEL     DRI(15)       DES07             7
132000000000     C                   MOVE      DES07         P1DDES
132100000000     C*
132200020116     C* === CON L'EURO NN SI "MIGLIALIZZA" + ===
132300020116     C*    RRS           DIV       1000          $VA
132400020116     C                   Z-ADD     RRS           $VA
132500020116     C                   Z-ADD     *ZEROS        $VA13C
132600020116DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
132700020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
132800000000E   1C                   ENDDO
132900020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
133000951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA
133100000000     C*
133200000000     C* SCRIVE I VALORI IN STAMPA
133300000000     C                   EXSR      SCRVAL
133400030917     C*
133500030917     C* IMPOSTA I CAMPI "RICAVI REALI" DEL WFILE DI STAMPA
133600030917     C                   Z-ADD     $V213C        SE94RPR                        *VALORE PRECEDENTE
133700030917     C                   Z-ADD     $VA(1)        SE94R01                        *MESE UNO
133800030917     C                   Z-ADD     $VA(2)        SE94R02                        *MESE DUE
133900030917     C                   Z-ADD     $VA(3)        SE94R03                        *MESE TRE
134000030917     C                   Z-ADD     $VA(4)        SE94R04                        *MESE QUATTRO
134100030917     C                   Z-ADD     $VA(5)        SE94R05                        *MESE CINQUE
134200030917     C                   Z-ADD     $VA(6)        SE94R06                        *MESE SEI
134300030917     C                   Z-ADD     $VA(7)        SE94R07                        *MESE SETTE
134400030917     C                   Z-ADD     $VA(8)        SE94R08                        *MESE OTTO
134500030917     C                   Z-ADD     $VA(9)        SE94R09                        *MESE NOVE
134600030917     C                   Z-ADD     $VA(10)       SE94R10                        *MESE DIECI
134700030917     C                   Z-ADD     $VA(11)       SE94R11                        *MESE UNDICI
134800030917     C                   Z-ADD     $VA(12)       SE94R12                        *MESE DODICI
134900030917     C                   Z-ADD     $VA13C        SE94RTO                        *TOTALE MESI
135000000000     C*--------------------
135100000000     C* D E L T A
135200000000     C*--------------------
135300000000IF  1C     *IN01         IFEQ      *ON
135400000000     C                   EXSR      STAINT
135500000000E   1C                   ENDIF
135600000000     C*
135700000000     C                   MOVEL     *BLANKS       P2DDES
135800020221     C                   MOVEL     *BLANKS       P2DFLD
135900000000     C                   MOVEL     DRI(16)       DES07             7            *SECONDA DESCRIZIONE
136000000000     C                   MOVE      DES07         P2DDES
136100000000     C*
136200000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
136300000000     C                   Z-ADD     RRS           $RR                            *RICAVI REALI
136400000000     C                   Z-ADD     RPS           $RP                            *RICAVI PRESUNTI
136500000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
136600000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
136700000000     C                   EXSR      IMPCAL
136800000000     C*
136900000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
137000000000     C                   EXSR      SCRDEL
137100030917     C*
137200030917     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
137300030917     C                   Z-ADD     $PD(13)       SE94DPR                        *VALORE PRECEDENTE
137400030917     C                   Z-ADD     $PD(1)        SE94D01                        *MESE UNO
137500030917     C                   Z-ADD     $PD(2)        SE94D02                        *MESE DUE
137600030917     C                   Z-ADD     $PD(3)        SE94D03                        *MESE TRE
137700030917     C                   Z-ADD     $PD(4)        SE94D04                        *MESE QUATTRO
137800030917     C                   Z-ADD     $PD(5)        SE94D05                        *MESE CINQUE
137900030917     C                   Z-ADD     $PD(6)        SE94D06                        *MESE SEI
138000030917     C                   Z-ADD     $PD(7)        SE94D07                        *MESE SETTE
138100030917     C                   Z-ADD     $PD(8)        SE94D08                        *MESE OTTO
138200030917     C                   Z-ADD     $PD(9)        SE94D09                        *MESE NOVE
138300030917     C                   Z-ADD     $PD(10)       SE94D10                        *MESE DIECI
138400030917     C                   Z-ADD     $PD(11)       SE94D11                        *MESE UNDICI
138500030917     C                   Z-ADD     $PD(12)       SE94D12                        *MESE DODICI
138600030917     C                   Z-ADD     $PD13C        SE94DTO                        *TOTALE MESI
138700030917     C*
138800030917     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
138900030917IF  1C     PARWPC        IFEQ      'S'
139000030919     c                   ADD       1             WNrr
139100030919     c                   Z-ADD     WNrr          SE94NRR                        *righe in stampa
139200030917     C                   WRITE     WFTISE940
139300030917E   1C                   ENDIF
139400000000     C*
139500000000     C                   ENDSR
139600000000     C*------------------------------------------------------------------------*
139700000000     C*?STAINT - STAMPA INTESTAZIONE
139800000000     C*------------------------------------------------------------------------*
139900000000     C     STAINT        BEGSR
140000000000     C*
140100020116     C                   WRITE     SE94PT                                       *TESTATA
140200020116     C                   WRITE     SE94I2                                       *RIGA DESCRITTIVA
140300020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
140400020116     C                   WRITE     SE94PI                                       *RIGA MESI
140500020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
140600000000     C                   SETOFF                                       01
140700000000     C*
140800000000     C                   ENDSR
140900000000     C*------------------------------------------------------------------------*
141000000000     C*?IMPCAL - IMPOSTA IL CALCOLO DEL DESTA PER TUTTI I VALORI
141100000000     C*          INPUT  --> $RR    : SCHIERA RICAVO REALE
141200000000     C*                     $RP    : SCHIERA RICAVO PRESUNTO
141300000000     C*          OUTPUT --> $PD    : SCHIERA DELTA PERCENTUALE
141400000000     C*                     $PD13C : 13^ PERCENTUALE DELTA
141500000000     C*------------------------------------------------------------------------*
141600000000     C     IMPCAL        BEGSR
141700000000     C*--------------------
141800020220     C* DELTA PERIODO PRECEDENTE
141900000000     C*--------------------
142000020116     C                   Z-ADD     $RR(13)       WRRW             19 5          *RICAVO REALE
142100020116     C                   Z-ADD     $RP(13)       WRPW             19 5          *RICAVO PRESUNTO
142200000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
142300000000     C                   EXSR      CALDEL
142400000000     C                   Z-ADD     WDPW          $PD(13)                        *SCHIERA DELTA
142500000000     C*--------------------
142600000000     C* DELTA 12 MESI
142700000000     C*--------------------
142800020116DO  1C     1             DO        SPARM         J
142900020116     C                   Z-ADD     $RR(J)        WRRW                           *RICAVO REALE
143000020116     C                   Z-ADD     $RP(J)        WRPW                           *RICAVO PRESUNTO
143100020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
143200000000     C                   EXSR      CALDEL
143300000000     C                   Z-ADD     WDPW          $PD(J)                         *SCHIERA DELTA
143400000000E   1C                   ENDDO
143500000000     C*--------------------
143600000000     C* DELTA 13^ COLONNA
143700000000     C*--------------------
143800000000     C                   Z-ADD     *ZEROS        WRRW
143900000000     C                   Z-ADD     *ZEROS        WRPW
144000000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
144100020116     C                   ADD       $RR(J)        WRRW                           *RICAVO REALE
144200020116     C                   ADD       $RP(J)        WRPW                           *RICAVO PRESUNTO
144300000000E   1C                   ENDDO
144400020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
144500000000     C                   EXSR      CALDEL
144600000000     C                   Z-ADD     WDPW          $PD13C                         *13^ COLONNA
144700000000     C*
144800000000     C                   ENDSR
144900000000     C*------------------------------------------------------------------------*
145000000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
145100000000     C*          INPUT  --> WRRW: RICAVO REALE
145200000000     C*                     WRPW: RICAVO PRESUNTO
145300000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
145400000000     C*------------------------------------------------------------------------*
145500000000     C     CALDEL        BEGSR
145600000000     C*--------------------
145700000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
145800000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
145900000000     C*--------------------
146000000000     C*
146100000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
146200020116     C     WRRW          SUB       WRPW          GAP              19 5          (REALE - PRESUNTO)
146300000000     C     GAP           MULT      100           GAP                            (GAP x 100)
146400000000     C*
146500000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
146600000000IF  1C     WRRW          IFEQ      *ZEROS
146700000000     C                   Z-ADD     1             WRRW
146800000000E   1C                   ENDIF
146900000000     C*
147000000000     C     GAP           DIV       WRRW          GAP
147100000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
147200000000     C*
147300000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
147400000000     C     WDPW          COMP      0                                    28
147500000000     C* POSITIVO
147600000000IF  1C     *IN28         IFEQ      *OFF
147700000000IF  2C     GAP           IFGE      1000
147800000000     C                   Z-ADD     +999,9        WDPW
147900000000E   2C                   ENDIF
148000000000     C* NEGATIVO
148100000000X   1C                   ELSE
148200000000IF  2C     GAP           IFLE      -1000
148300000000     C                   Z-ADD     -999,9        WDPW
148400000000E   2C                   ENDIF
148500000000E   1C                   ENDIF
148600000000     C*
148700000000     C                   ENDSR
148800000000     C*------------------------------------------------------------------------*
148900000000     C*?SCRVAL - SCRIVE I VALORI IN STAMPA
149000000000     C*------------------------------------------------------------------------*
149100000000     C     SCRVAL        BEGSR
149200000000     C*
149300951227     C                   Z-ADD     $V213C        P1CVAP                         *VALORE PRECEDENTE
149400000000     C                   Z-ADD     $VA(1)        P1CVA1                         *MESE UNO
149500000000     C                   Z-ADD     $VA(2)        P1CVA2                         *MESE DUE
149600000000     C                   Z-ADD     $VA(3)        P1CVA3                         *MESE TRE
149700000000     C                   Z-ADD     $VA(4)        P1CVA4                         *MESE QUATTRO
149800000000     C                   Z-ADD     $VA(5)        P1CVA5                         *MESE CINQUE
149900000000     C                   Z-ADD     $VA(6)        P1CVA6                         *MESE SEI
150000000000     C                   Z-ADD     $VA(7)        P1CVA7                         *MESE SETTE
150100000000     C                   Z-ADD     $VA(8)        P1CVA8                         *MESE OTTO
150200000000     C                   Z-ADD     $VA(9)        P1CVA9                         *MESE NOVE
150300000000     C                   Z-ADD     $VA(10)       P1CVA0                         *MESE DIECI
150400000000     C                   Z-ADD     $VA(11)       P1CVAA                         *MESE UNDICI
150500000000     C                   Z-ADD     $VA(12)       P1CVAB                         *MESE DODICI
150600000000     C                   Z-ADD     $VA13C        P1CVAT                         *TOTALE MESI
150700000000     C*
150800020116     C                   WRITE     SE94P1                                       *STAMPA RIGA VALORE
150900000000     C*
151000000000     C                   ENDSR
151100000000     C*------------------------------------------------------------------------*
151200000000     C*?SCRDEL - SCRIVE I VALORI DELTA IN STAMPA
151300000000     C*------------------------------------------------------------------------*
151400000000     C     SCRDEL        BEGSR
151500000000     C*
151600000000     C                   Z-ADD     $PD(13)       P2CDEP                         *VALORE PRECEDENTE
151700000000     C                   Z-ADD     $PD(1)        P2CDE1                         *MESE UNO
151800000000     C                   Z-ADD     $PD(2)        P2CDE2                         *MESE DUE
151900000000     C                   Z-ADD     $PD(3)        P2CDE3                         *MESE TRE
152000000000     C                   Z-ADD     $PD(4)        P2CDE4                         *MESE QUATTRO
152100000000     C                   Z-ADD     $PD(5)        P2CDE5                         *MESE CINQUE
152200000000     C                   Z-ADD     $PD(6)        P2CDE6                         *MESE SEI
152300000000     C                   Z-ADD     $PD(7)        P2CDE7                         *MESE SETTE
152400000000     C                   Z-ADD     $PD(8)        P2CDE8                         *MESE OTTO
152500000000     C                   Z-ADD     $PD(9)        P2CDE9                         *MESE NOVE
152600000000     C                   Z-ADD     $PD(10)       P2CDE0                         *MESE DIECI
152700000000     C                   Z-ADD     $PD(11)       P2CDEA                         *MESE UNDICI
152800000000     C                   Z-ADD     $PD(12)       P2CDEB                         *MESE DODICI
152900000000     C                   Z-ADD     $PD13C        P2CDET                         *TOTALE MESI
153000000000     C*
153100020116     C                   WRITE     SE94P2                                       *STAMPA RIGA VALORE
153200000000     C*
153300000000     C                   ENDSR
153400000000     C*------------------------------------------------------------------------*
153500000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
153600000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
153700000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
153800000000     C*------------------------------------------------------------------------*
153900000000     C     DECDIV        BEGSR
154000000000     C*
154100000000     C                   Z-ADD     1             D
154200000000     C     DEPDIV        LOOKUP    DIV(D)                                 99
154300000000IF  1C     *IN99         IFEQ      *OFF
154400000000     C                   MOVEL     *ALL'*'       DESDIV           20
154500000000X   1C                   ELSE
154600000000     C                   MOVEL     DDIV(D)       DESDIV
154700000000E   1C                   ENDIF
154800000000     C*
154900000000     C                   ENDSR
155000000000     C*------------------------------------------------------------------------*
155100000000     C*?DECRGF - DECODIFICA RAGGRUPPAMENTO FILIALE AGENTE
155200000000     C*          INPUT  -- DEPRGF: CODICE RAGGRUPPAMENTO
155300000000     C*          OUTPUT -- DESRGF: DESCRIZIONE RAGGRUPPAMENTO FILIALE
155400000000     C*------------------------------------------------------------------------*
155500000000     C     DECRGF        BEGSR
155600000000     C*
155700000000     C                   MOVEL     WFDRGD        DESRGF
155800000000     C*
155900000000     C                   ENDSR
156000000000     C*------------------------------------------------------------------------*
156100000000     C*? DECFIL - DECODIFICA CODICE FILIALE
156200000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
156300000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
156400000000     C*------------------------------------------------------------------------*
156500000000     C     DECFIL        BEGSR
156600000000     C*
156700000000     C                   Z-ADD     1             D
156800000000     C     DEPFIL        LOOKUP    FIL(D)                                 99
156900000000IF  1C     *IN99         IFEQ      *OFF
157000000000     C                   MOVEL     *ALL'*'       DESFIL           20
157100000000X   1C                   ELSE
157200000000     C                   MOVEL     DFIL(D)       DESFIL
157300000000E   1C                   ENDIF
157400000000     C*
157500000000     C                   ENDSR
157600000000     C*------------------------------------------------------------------------*
157700000000     C*?DECAGE - DECODIFICA CODICE AGENTE
157800000000     C*          INPUT  -- DEPAGE: CODICE AGENTE COMMERCIALE
157900000000     C*          OUTPUT -- DESAGE: DESCRIZIONE AGENTE
158000000000     C*                    DESPCF: PERCENTUALE COMPETENZA FILIALE
158100000000     C*------------------------------------------------------------------------*
158200000000     C     DECAGE        BEGSR
158300000000     C*
158400980407     C                   MOVEL     *BLANKS       DESAGE           25
158500000000     C                   Z-ADD     1             D
158600000000     C     DEPAGE        LOOKUP    AGE(D)                                 99
158700000000IF  1C     *IN99         IFEQ      *OFF
158800980407     C                   MOVEL     *ALL'*'       DESAGE
158900000000     C                   Z-ADD     *ZEROS        DESPCF
159000000000X   1C                   ELSE
159100000000     C                   MOVEL     DAGE(D)       DESAGE
159200000000     C                   Z-ADD     CAGE(D)       DESPCF
159300000000E   1C                   ENDIF
159400000000     C*
159500000000     C                   ENDSR
159600000000     C*------------------------------------------------------------------------*
159700000000     C*? DECARE - DECODIFICA CODICE AREA
159800000000     C*          INPUT  -- DEPARE: CODICE AREA
159900000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
160000000000     C*------------------------------------------------------------------------*
160100000000     C     DECARE        BEGSR
160200000000     C*
160300000000     C                   Z-ADD     1             D
160400000000     C     DEPARE        LOOKUP    ARE(D)                                 99
160500000000IF  1C     *IN99         IFEQ      *OFF
160600000000     C                   MOVEL     *ALL'*'       DESARE           20
160700000000X   1C                   ELSE
160800000000     C                   MOVEL     DARE(D)       DESARE
160900000000E   1C                   ENDIF
161000000000     C*
161100000000     C                   ENDSR
161200000000     C*------------------------------------------------------------------------*
161300000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
161400000000     C*------------------------------------------------------------------------*
161500000000     C     CARMES        BEGSR
161600000000     C*
161700000000     C                   CLEAR                   ANN                            *ANNI
161800000000     C                   CLEAR                   MES                            *MESI
161900000000     C                   Z-ADD     *ZEROS        J                              *N° MESI DA ELABORAR
162000000000     C*
162100990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
162200000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
162300990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
162400000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
162500000000     C*
162600000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
162700000000IF  1C     WAA2          IFEQ      WAA
162800000000     C*
162900000000     C* CALCOLO I MESI DI DIFFERENZA
163000000000DO  2C     WMM           DOWLE     WMM2
163100000000     C                   ADD       1             J
163200000000     C                   Z-ADD     WAA           ANN(J)
163300000000     C                   Z-ADD     WMM           MES(J)
163400000000     C                   ADD       1             WMM
163500000000E   2C                   ENDDO
163600000000     C***
163700000000     C* SPARM = N° MESI SELEZIONATI
163800000000     C***
163900000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
164000000000     C*
164100000000     C* IL RESTO DELLA SCHIERA A ZERO
164200000000DO  2C     J             DOWLT     12
164300000000     C                   ADD       1             J
164400000000     C                   Z-ADD     *ZEROS        ANN(J)
164500000000     C                   Z-ADD     *ZEROS        MES(J)
164600000000E   2C                   ENDDO
164700000000     C*
164800000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
164900000000X   1C                   ELSE
165000000000     C*
165100000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
165200000000DO  2C     WMM           DOWLE     12
165300000000     C                   ADD       1             J
165400000000     C                   Z-ADD     WAA           ANN(J)
165500000000     C                   Z-ADD     WMM           MES(J)
165600000000     C                   ADD       1             WMM
165700000000E   2C                   ENDDO
165800000000     C*
165900000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
166000000000     C                   Z-ADD     01            WMM
166100000000DO  2C     WMM           DOWLE     WMM2
166200000000     C                   ADD       1             J
166300000000     C                   Z-ADD     WAA2          ANN(J)
166400000000     C                   Z-ADD     WMM           MES(J)
166500000000     C                   ADD       1             WMM
166600000000E   2C                   ENDDO
166700000000     C***
166800000000     C* SPARM = N° MESI SELEZIONATI
166900000000     C***
167000000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
167100000000     C*
167200000000     C* IL RESTO DELLA SCHIERA A ZERO
167300000000DO  2C     J             DOWLT     12
167400000000     C                   ADD       1             J
167500000000     C                   Z-ADD     *ZEROS        ANN(J)
167600000000     C                   Z-ADD     *ZEROS        MES(J)
167700000000E   2C                   ENDDO
167800000000E   1C                   ENDIF
167900000000     C*
168000000000     C                   ENDSR
168100000000     C*------------------------------------------------------------------------*
168200000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
168300000000     C*------------------------------------------------------------------------*
168400000000     C     DECTES        BEGSR
168500000000     C*--------------------
168600000000     C* DATA E ORA
168700000000     C*--------------------
168800000000     C                   Z-ADD     UDATE         DATA              6 0
168900000000     C                   TIME                    ORA               6 0
169000950602     C*--------------------
169100950602     C* DATI IN STATISTICA
169200950602     C*--------------------
169300950602IF  1C     PARIOE        IFEQ      'I'
169400950602     C                   MOVEL     DRI(5)        PTCIOE
169500950602E   1C                   ENDIF
169600950602IF  1C     PARIOE        IFEQ      'E'
169700950602     C                   MOVEL     DRI(6)        PTCIOE
169800950602E   1C                   ENDIF
169900950602IF  1C     PARIOE        IFEQ      *BLANKS
170000950602     C                   MOVEL     DRI(7)        PTCIOE
170100950602E   1C                   ENDIF
170200000829     C*--------------------
170300000829     C* DATI CLIENTE / UNIFICANTE
170400000829     C*--------------------
170500000829IF  1C     PARSCE        IFEQ      'C'
170600000829     C                   MOVEL     DRI(29)       PTDSCE
170700000829X   1C                   ELSE
170800000829     C                   MOVEL     DRI(30)       PTDSCE
170900000829E   1C                   ENDIF
171000000000     C*--------------------
171100000000     C* DECODIFICA MESI
171200000000     C*--------------------
171300000000IF  1C     MES(1)        IFGT      *ZEROS
171400000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ
171500000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
171600990609     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
171700000000E   1C                   ENDIF
171800000000     C*
171900000000IF  1C     MES(2)        IFGT      *ZEROS
172000000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ
172100000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
172200990609     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
172300000000E   1C                   ENDIF
172400000000     C*
172500000000IF  1C     MES(3)        IFGT      *ZEROS
172600000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ
172700000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
172800990609     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
172900000000E   1C                   ENDIF
173000000000     C*
173100000000IF  1C     MES(4)        IFGT      *ZEROS
173200000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ
173300000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
173400990609     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
173500000000E   1C                   ENDIF
173600000000     C*
173700000000IF  1C     MES(5)        IFGT      *ZEROS
173800000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ
173900000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
174000990609     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
174100000000E   1C                   ENDIF
174200000000     C*
174300000000IF  1C     MES(6)        IFGT      *ZEROS
174400000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ
174500000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
174600990609     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
174700000000E   1C                   ENDIF
174800000000     C*
174900000000IF  1C     MES(7)        IFGT      *ZEROS
175000000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ
175100000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
175200990609     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
175300000000E   1C                   ENDIF
175400000000     C*
175500000000IF  1C     MES(8)        IFGT      *ZEROS
175600000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ
175700000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
175800990609     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
175900000000E   1C                   ENDIF
176000000000     C*
176100000000IF  1C     MES(9)        IFGT      *ZEROS
176200000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ
176300000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
176400990609     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
176500000000E   1C                   ENDIF
176600000000     C*
176700000000IF  1C     MES(10)       IFGT      *ZEROS
176800000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ
176900000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
177000990609     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
177100000000E   1C                   ENDIF
177200000000     C*
177300000000IF  1C     MES(11)       IFGT      *ZEROS
177400000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ
177500000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
177600990609     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
177700000000E   1C                   ENDIF
177800000000     C*
177900000000IF  1C     MES(12)       IFGT      *ZEROS
178000000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ
178100000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
178200990609     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
178300000000E   1C                   ENDIF
178400000000     C*
178500000000     C                   ENDSR
178600000000     C*------------------------------------------------------------------------*
178700000000     C*?CARTAB - CARICA I DATI IN TABELLE
178800000000     C*------------------------------------------------------------------------*
178900000000     C     CARTAB        BEGSR
179000000000     C*--------------------
179100000000     C* FILIALI/AREE/DIVISIONI
179200000000     C*--------------------
179300000000     C                   Z-ADD     *ZEROS        KORFIL
179400000000     C     KEYORG        SETLL     AZORG01L
179500000000     C                   READ      AZORG01L                               99
179600000000     C                   Z-ADD     *ZEROS        F
179700951219DO  1C     *IN99         DOWEQ     *OFF
179800000000IF  2C     ORGFVA        IFEQ      *BLANKS
179900000000     C                   ADD       1             F
180000000000     C* F I L I A L I
180100000000     C                   Z-ADD     ORGFIL        FIL(F)
180200000000     C                   MOVEL     ORGDES        DFIL(F)
180300000707     C                   MOVEL     ORGDE3        OG143
180400020627     C                   MOVEL     §OGNTW        POS(F)
180500000000     C* A R E E
180600020906     C                   Z-ADD     1             KTBKUT
180700000000     C                   MOVEL     '05'          KTBCOD
180800000000     C                   MOVEL     *BLANKS       KTBKEY
180900000000     C                   MOVEL     ORGCAR        KTBKEY
181000000000     C     KEYTAB        CHAIN     TABEL00F                           99
181100000000IF  3C     *IN99         IFEQ      *OFF
181200000000     C                   Z-ADD     ORGCAR        ARE(F)
181300000000     C                   MOVEL     TBLUNI        DARE(F)
181400000000E   3C                   ENDIF
181500000000     C* D I V I S I O N I
181600020906     C                   Z-ADD     1             KTBKUT
181700000000     C                   MOVEL     '17'          KTBCOD
181800000000     C                   MOVEL     *BLANKS       KTBKEY
181900000000     C                   MOVEL     ORGFL3        KTBKEY
182000000000     C     KEYTAB        CHAIN     TABEL00F                           99
182100000000IF  3C     *IN99         IFEQ      *OFF
182200000000     C                   MOVEL     ORGFL3        DIV(F)
182300000000     C                   MOVEL     TBLUNI        DDIV(F)
182400000000E   3C                   ENDIF
182500000000E   2C                   ENDIF
182600000000     C                   READ      AZORG01L                               99
182700000000E   1C                   ENDDO
182800000000     C*--------------------
182900000000     C* COMMERCIALI
183000000000     C*--------------------
183100020906     C                   Z-ADD     1             KTBKUT
183200000000     C                   MOVEL     '01'          KTBCOD
183300981103     C     KEYTA2        SETLL     TABEL00F
183400981103     C     KEYTA2        READE     TABEL00F                               99
183500000000     C                   Z-ADD     *ZEROS        C
183600000000DO  1C     *IN99         DOWEQ     *OFF
183700000000     C                   ADD       1             C
183800000000     C                   MOVEL     TBLUNI        DS01
183900000000     C                   MOVEL     TBLKEY        AGE(C)
184000000000     C                   MOVEL     §01AGE        DAGE(C)
184100980528IF  3C     PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
184200980528     C                   Z-ADD     §01PCF        CAGE(C)
184300980528X   3C                   ELSE                                                   *% LAVORO ESTERO
184400980528     C                   Z-ADD     §01PCE        CAGE(C)
184500980528E   3C                   ENDIF
184600981103     C     KEYTA2        READE     TABEL00F                               99
184700000000E   1C                   ENDDO
184800980521     C*---
184900980521     C* QUALITA' CLIENTE
185000980521     C*---
185100980521     C                   Z-ADD     *ZEROS        C
185200020906     C                   Z-ADD     1             KTBKUT
185300980521     C                   MOVEL     'IC'          KTBCOD
185400980521     C     KEYTA2        SETLL     TABEL00F
185500980521     C     KEYTA2        READE     TABEL00F                               99
185600980521DO  1C     *IN99         DOWEQ     *OFF
185700980521IF  2C     TBLFLG        IFEQ      *BLANKS
185800980521     C                   ADD       1             C
185900980521     C                   MOVEL     TBLKEY        ABC(C)
186000980521E   2C                   ENDIF
186100980521     C     KEYTA2        READE     TABEL00F                               99
186200980521E   1C                   ENDDO
186300000000     C*
186400000000     C                   ENDSR
186500000000     C*------------------------------------------------------------------------*
186600000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
186700000000     C*------------------------------------------------------------------------*
186800000000     C     STAPAR        BEGSR
186900000000     C*--------------------
187000000000     C* NOME PROGRAMMA
187100000000     C*--------------------
187200020116     C                   MOVEL     'TISE94R'     PTCPGM
187300000000     C*--------------------
187400000000     C* DESCRIZIONE STATISTICA
187500000000     C*--------------------
187600000000     C                   MOVEL     DST(1)        PTCDST
187700000000     C*--------------------
187800000000     C* DATE LANCIO
187900000000     C*--------------------
188000000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
188100000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
188200000000     C*--------------------
188300000000     C* SELEZIONI PER TIPO SERVIZIO
188400000000     C*--------------------
188500000000IF  1C     PARTSP        IFEQ      *BLANKS
188600000000     C                   MOVEL     DRI(1)        PADTSP
188700000000X   1C                   ELSE
188800000000     C                   MOVEL     PARTSP        PACTSP
188900020906     C                   Z-ADD     1             KTBKUT
189000000000     C                   MOVEL     '5E'          KTBCOD
189100000000     C                   MOVEL     *BLANKS       KTBKEY
189200000000     C                   MOVEL     PARTSP        KTBKEY
189300000000     C     KEYTAB        CHAIN     TABEL00F                           99
189400000000IF  2C     *IN99         IFEQ      *OFF
189500090610     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
189600090610     C                   MOVEL     §5EDES        PADTSP                         *TIPO SERVIZIO
189700000000E   2C                   ENDIF
189800000000E   1C                   ENDIF
189900950208     C*--------------------
190000950208     C* SELEZIONI PER DELTA ITALIA/ESTERO
190100950208     C*--------------------
190200950208     C                   MOVEL     PARIOE        PACIOE
190300950208IF  1C     PARIOE        IFEQ      'I'
190400950208     C                   MOVEL     DRI(5)        PADIOE
190500950208E   1C                   ENDIF
190600950208IF  1C     PARIOE        IFEQ      'E'
190700950208     C                   MOVEL     DRI(6)        PADIOE
190800950208E   1C                   ENDIF
190900950208IF  1C     PARIOE        IFEQ      *BLANKS
191000950208     C                   MOVEL     DRI(7)        PADIOE
191100950208E   1C                   ENDIF
191200951018     C*--------------------
191300970822     C* SELEZIONI PER QUALITA' CLIENTE
191400951018     C*--------------------
191500951018     C                   MOVEL     PARCLV        PACCLV
191600010618     C                   MOVEL     PARCL1        PACCL1
191700010618     C                   MOVEL     PARCL2        PACCL2
191800020220     C                   MOVEL     PARCL3        PACCL3
191900020220     C                   MOVEL     PARCL4        PACCL4
192000951018     C                   MOVEL     *BLANKS       PADCLV
192100951018IF  1C     PARCLV        IFEQ      *BLANKS
192200010618     C     PARCL1        ANDEQ     *BLANKS
192300010618     C     PARCL2        ANDEQ     *BLANKS
192400020220     C     PARCL3        ANDEQ     *BLANKS
192500020220     C     PARCL4        ANDEQ     *BLANKS
192600951018     C                   MOVEL     DRI(1)        PADCLV
192700951018E   1C                   ENDIF
192800010618     C*--------------------
192900010618     C* TIPO STAMPA SINTETICA S/N
193000010618     C*--------------------
193100010618     C                   MOVEL     PARSIN        PACSIN
193200980528     C*--------------------
193300980528     C* % SCOSTAMENTO DELTA TOTALE
193400980528     C*--------------------
193500980528     C                   Z-ADD     PARPDD        PACPDD
193600980528     C                   Z-ADD     PARPDA        PACPDA
193700980528     C*--------------------
193800980528     C* % SCOSTAMENTO DELTA PER FASCE
193900980528     C*--------------------
194000980528     C                   Z-ADD     PARSPD        PACSPD
194100020702     C*--------------------
194200020702     C* % SCOSTAMENTO FATTURATO ANNO PRECEDENTE
194300020702     C*--------------------
194400020702     C                   Z-ADD     PARDFD        PACDFD
194500020702     C                   Z-ADD     PARDFA        PACDFA
194600020702     C                   Z-ADD     PARDFDP       PACDFDP
194700020702     C                   Z-ADD     PARDFAP       PACDFAP
194800951018     C*--------------------
194900000000     C* SELEZIONI DELTA FILIALE
195000000000     C*--------------------
195100000000IF  1C     PARFIC        IFGT      *ZEROS                                       *FILIALE
195200000000     C                   Z-ADD     PARFIC        DEPFIL
195300000000     C                   EXSR      DECFIL
195400000000     C                   Z-ADD     *ZEROS        DEPFIL
195500000000     C                   MOVEL     PARFIC        PACFIC
195600000000     C                   MOVEL     DESFIL        PADFIC
195700000000X   1C                   ELSE
195800000000     C                   MOVEL     *BLANKS       PACFIC
195900000000E   1C                   ENDIF
196000000000     C*
196100000000IF  1C     PARARC        IFGT      *ZEROS                                       *AREA
196200000000     C                   Z-ADD     PARARC        DEPARE
196300000000     C                   EXSR      DECARE
196400000000     C                   Z-ADD     *ZEROS        DEPARE
196500000000     C                   MOVEL     PARARC        PACARC
196600000000     C                   MOVEL     DESARE        PADARC
196700000000X   1C                   ELSE
196800000000     C                   MOVEL     *BLANKS       PACARC
196900000000E   1C                   ENDIF
197000000000     C*
197100000000IF  1C     PARDIC        IFNE      *BLANKS                                      *DIVISIONE
197200000000     C                   MOVEL     PARDIC        DEPDIV
197300000000     C                   EXSR      DECDIV
197400000000     C                   MOVEL     *BLANKS       DEPDIV
197500000000     C                   MOVEL     PARDIC        PACDIC
197600000000     C                   MOVEL     DESDIV        PADDIC
197700000000E   1C                   ENDIF
197800000000     C*--------------------
197900000000     C* SELEZIONI DELTA COMMERCIALE
198000000000     C*--------------------
198100000000IF  1C     PARCOF        IFGT      *ZEROS                                       *COMMERCIALE
198200000000     C                   Z-ADD     PARCOF        DEPAGE
198300000000     C                   EXSR      DECAGE
198400000000     C                   Z-ADD     *ZEROS        DEPAGE
198500000000     C                   MOVEL     PARCOF        PACCOF
198600000000     C                   MOVEL     DESAGE        PADCOF
198700000000X   1C                   ELSE
198800000000     C                   MOVEL     *BLANKS       PACCOF
198900000000E   1C                   ENDIF
199000000000     C*
199100000000IF  1C     PARDIF        IFNE      *BLANKS                                      *DIVISIONE
199200000000     C                   MOVEL     PARDIF        DEPDIV
199300000000     C                   EXSR      DECDIV
199400000000     C                   MOVEL     *BLANKS       DEPDIV
199500000000     C                   MOVEL     PARDIF        PACDIF
199600000000     C                   MOVEL     DESDIV        PADDIF
199700000000E   1C                   ENDIF
199800000707     C*--------------------
199900000707     C* DATI POSTE
200000000707     C*--------------------
200100000707IF  1C     PARPOS        IFEQ      'S'
200200000707     C                   MOVEL     'SI'          PADPOS
200300000707X   1C                   ELSE
200400000707     C                   MOVEL     'NO'          PADPOS
200500000707E   1C                   ENDIF
200600000825     C*--------------------
200700000825     C* DATI CLIENTE / UNIFICANTE
200800000825     C*--------------------
200900000825     C                   MOVEL     PARSCE        PACSCE
201000000825IF  1C     PARSCE        IFEQ      'C'
201100000825     C                   MOVEL     DRI(29)       PADSCE
201200000825X   1C                   ELSE
201300000825     C                   MOVEL     DRI(30)       PADSCE
201400000825E   1C                   ENDIF
201500000000     C*
201600000000     C* S T A M P A
201700000000     C*
201800020116     C                   WRITE     SE94PT                                       *INTESTAZIONE
201900020116     C                   WRITE     SE94PA                                       *PARAMETRI
202000000000     C*
202100000000     C* ACCENDO INDICATORE DI OF PER STAMPARE LA TESTATA ALLA 1^ ROTTURA CLIENTE
202200000000     C                   SETON                                        01
202300000000     C*
202400000000     C                   ENDSR
202500030916     C*------------------------------------------------------------------------*
202600030916     C*?CANWF - CANCELLA DATI WFILE, SE RICHIESTO
202700030916     C*------------------------------------------------------------------------*
202800030916     C     CANWF         BEGSR
202900030916     C*
203000030916     C     PARWPC        IFEQ      'S'                                          *SE RICHIESTO
203100030916     c                   MOVEL     PARIOE        KE94IOE                        *ITALIA/ESTERO
203200030916     C     KEYSE94       SETLL     WFTISE94L
203300030916     C     KEYSE94       READE     WFTISE94L                              99
203400030916     C                   DOW       NOT *IN99
203500030916     C                   DELETE    WFTISE940
203600030916     C     KEYSE94       READE     WFTISE94L                              99
203700030916     C                   ENDDO
203800030916     C                   ENDIF
203900030916     C*
204000030916     C                   ENDSR
204100000000     C*------------------------------------------------------------------------*
204200000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
204300000000     C*------------------------------------------------------------------------*
204400000000     C     *INZSR        BEGSR
204500000000     C*
204600000000     C* PARAMETRI IN INPUT AL PGM
204700000000     C     *ENTRY        PLIST
204800000000     C                   PARM                    KPJBA
204900000000     C                   MOVEL     KPJBU         PARAM
205000000000     C*
205100000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
205200000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
205300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
205400000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
205500000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
205600950213     C     *LIKE         DEFINE    WFDDIC        KWDDIV                         *WFDEL02L
205700000000     C     *LIKE         DEFINE    WFDRGD        KWDRGD
205800000000     C     *LIKE         DEFINE    WFDRGF        KWDRGF
205900000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
206000000825     C     *LIKE         DEFINE    WFDCLI        KWDCLI
206100000000     C     *LIKE         DEFINE    WFDANN        KWDANN
206200000000     C     *LIKE         DEFINE    WFDMES        KWDMES
206300030916     C     *LIKE         DEFINE    SE94IOE       KE94IOE
206400000000     C*
206500000000     C* DEFINISCE LE VARIABILI DI DEPOSITO
206600000000     C     *LIKE         DEFINE    WFDDIC        DEPDIV
206700000000     C     *LIKE         DEFINE    WFDARE        DEPARE
206800000000     C     *LIKE         DEFINE    WFDRGF        DEPRGF
206900000000     C     *LIKE         DEFINE    WFDAGE        DEPAGE
207000000000     C     *LIKE         DEFINE    WFDFIL        DEPFIL
207100000825     C     *LIKE         DEFINE    WFDCLI        DEPCLI
207200000000     C*
207300000000     C* DEFINISCE LE DESCRIZIONI DI DEPOSITO
207400000000     C     *LIKE         DEFINE    §01PCF        DESPCF
207500000000     C     *LIKE         DEFINE    WFDRGD        DESRGF
207600000000     C     *LIKE         DEFINE    ORGDES        DESSIL
207700000000     C*
207800000000     C* DEFINISCE INDICI DI SCHIERA
207900000000     C                   Z-ADD     *ZEROS        C                 4 0          *COMMERCIALI
208000000000     C                   Z-ADD     *ZEROS        D                 4 0          *PER DECODIFICHE
208100000000     C                   Z-ADD     *ZEROS        F                 4 0          *VOCI ORGANIGRAMMA
208200000000     C                   Z-ADD     *ZEROS        J                 4 0          *GENERICO
208300000000     C                   Z-ADD     *ZEROS        X                 2 0          *PER I MESI
208400000000     C                   Z-ADD     *ZEROS        MM                2 0          *PER I MESI
208500000000     C                   Z-ADD     *ZEROS        IN                2 0          *PER I MESI
208600000000     C                   Z-ADD     *ZEROS        SPARM             4 0          *N° MESI SELEZIONATI
208700000000     C*
208800000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
208900000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
209000000000     C                   MOVEL     '0'           PASDIV            1            MEMOR SALDI DIVISION
209100000000     C                   MOVEL     '0'           PASRGF            1            MEMOR SALDI FILIALE
209200000000     C                   MOVEL     '0'           PASAGE            1            MEMOR SALDI COMM.LE
209300000000     C                   MOVEL     ' '           TIPANN            1            *TIPO ANNO (P/C)
209400951227     C                   MOVEL     '0'           FLAKSC            1            FLAG CONT CLI CORR
209500951227     C                   MOVEL     '0'           FL2KSC            1            FLAG CONT CLI PREC
209600960710     C*--------------------
209700960710     C* CHIAVI DI LETTURA
209800960710     C*--------------------
209900960710     C* LETTURA TABEL00F - COMPLETA
210000000000     C     KEYTAB        KLIST
210100000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
210200000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
210300000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
210400960710     C* LETTURA TABEL00F - PARZIALE
210500000000     C     KEYTA2        KLIST
210600000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
210700000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
210800960710     C* LETTURA AZORG01L - COMPLETA
210900000000     C     KEYORG        KLIST
211000000000     C                   KFLD                    KORFIL                         *FILIALE
211100960710     C* LETTURA WFDEL02L - PARZIALE
211200000000     C     KEYDEL        KLIST
211300000000     C                   KFLD                    KWDDIV                         *DIVISIONE
211400000000     C                   KFLD                    KWDRGD                         *DESCRIZ RAGGRUP.TO
211500000000     C                   KFLD                    KWDRGF                         *RAGGRUP.TO FILIALE
211600000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
211700000825     C                   KFLD                    KWDCLI                         *CLIENTE
211800000000     C                   KFLD                    KWDANN                         *ANNO
211900000000     C                   KFLD                    KWDMES                         *MESE
212000030916     C* LETTURA WFTISE94L - PARZIALE
212100030916     C     KEYSE94       KLIST
212200030916     C                   KFLD                    KE94IOE                        *ITALIA/ESTERO
212300000000     C*
212400000000     C* MEMORIZZA I DATI IN TABELLE
212500000000     C                   EXSR      CARTAB
212600000000     C*
212700000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
212800000000     C                   EXSR      CARMES
212900000000     C*
213000000000     C* DECODIFICA INTESTAZIONE
213100000000     C                   EXSR      DECTES
213200030916     C*
213300030916     C* CANCELLA DATI WFILE, SE RICHIESTO
213400030916     C                   EXSR      CANWF
213500000000     C*
213600000000     C                   ENDSR
213700000000     O*------------------------------------------------------------------------*
213800000000**   DMM - DECODIFICHE MESI DELL'ANNO
213900000000GEN  1
214000000000FEB  2
214100000000MAR  3
214200000000APR  4
214300000000MAG  5
214400000000GIU  6
214500000000LUG  7
214600000000AGO  8
214700000000SET  9
214800000000OTT  10
214900000000NOV  11
215000000000DIC  12
215100000000**   DST - DECODIFICHE STATISTICHE
215200000000COMMERCIALE              1
215300000000                         2
215400000000                         3
215500000000                         4
215600000000                         5
215700000000**   DRI - DECODIFICHE RIGA DI STAMPA
215800000000TUTTI                    1
215900000000TUTTE                    2
2160000000003
2161009502084
216200950208ITALIA                   5
216300950208ESTERO                   6
216400950602ITALIA+ESTERO            7
2165000000008
2166000000009
21670000000010
21680000000011
216900020220TOTALE                  12
217000000000CLIENTI                 13
217100000000SPEDIZ                  14
217200000000RICAVO                  15
217300020220DELTA                   16
217400020221TOTALE                  17
21750000000018
21760000000019
21770000000020
217800000000 C O M M E R C I A L E  21
21790000000022
21800000000023
218100000000   D I V I S I O N E    24
21820000000025
21830000082526
21840000082527
21850000082528
218600000829DATI CLIENTE             29
218700000829DATI CLIENTE UNIFICANTE  30
218800020319**   DNTW - DECODIFICHE NETWORK
218900030312***                       1
219000020319EEX                       2
219100020319DPD                       3
219200020319FED                       4
219300020319EUP                       5
