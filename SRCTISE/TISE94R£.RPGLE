000100000000     H*------------------------------------------------------------------------*
000200000000     H*                   - STATISTICA DELTA COMMERCIALE -                     *
000300000000     H*------------------------------------------------------------------------*
000400970820     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800100202     FWFDEa02L  IF   E           K DISK
000900000000     FAZORG01L  IF   E           K DISK
001000140612     FAZCMM01L  IF   E           K DISK    usropn
001100000000     FTABEL00F  IF   E           K DISK
001200020116     FTISE94P   O    E             PRINTER OFLIND(*IN01)
001300030916     FWFTISE94L UF A E           K DISK
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*------------------
001800000000     D* DECODIFICA MESI DELL'ANNO
001900000000     D*------------------
002000000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
002100000000     D*------------------
002200000000     D* DECODIFICA STATISTICHE
002300000000     D*------------------
002400000000     D DST             S             25    DIM(5) CTDATA PERRCD(1)
002500000000     D*------------------
002600000000     D* DECODIFICA RIGHE DI STAMPA
002700000000     D*------------------
002800000825     D DRI             S             24    DIM(30) CTDATA PERRCD(1)
002900020319     D*------------------
003000020319     D* DECODIFICA NETWORK
003100020319     D*------------------
003200020319     D DNTW            S              3    DIM(5) CTDATA PERRCD(1)
003300000000     D*------------------
003400000000     D* ORGANIGRAMMA
003500000000     D*------------------
003600971124     D FIL             S              3  0 DIM(500)                             *FILIALI
003700971124     D DFIL            S             20    DIM(500)
003800971124     D ARE             S              3  0 DIM(500)                             *AREE
003900971124     D DARE            S             20    DIM(500)
004000971124     D DIV             S              1    DIM(500)                             *DIVISIONI
004100971124     D DDIV            S             20    DIM(500)
004200020627     D POS             S              3    DIM(500)                             *NETWORK POSTE
004300000000     D*------------------
004400000000     D* COMMERCIALI
004500000000     D*------------------
004600150102     D AGE             S              7  0 DIM(9000)                            *CODICI
004700150102     D DAGE            S             20    DIM(9000)                            *DECODIFICHE
004800150102     D CAGE            S              3  0 DIM(9000)                            *PERCENTUALI COMPETE
004900980521     D*------------------
005000980521     D* CATEGORIE CLIENTI
005100980521     D*------------------
005200020220     D ABC             S              1    DIM(5)                               QUALITA' CLIENTE
005300000000     D*------------------
005400000000     D* MESI DA ELABORARE
005500000000     D*------------------
005600000000     D MES             S              2  0 DIM(12)
005700000000     D*------------------
005800000000     D* ANNI DEI MESI DA ELABORARE
005900000000     D*------------------
006000990609     D ANN             S              4  0 DIM(12)
006100020319     D*------------------
006200020319     D* SCHIERA DI WRK X CONTEGGIO NETWORK SU ANNO "P" E ANNO "C" (LA DIMENSIONE=NUM. MESI x NUM. NETWORK => 13x10)
006300020319     D*------------------
006400020319     D WSK1            S             12    DIM(130)
006500000000    ?D*-------------------
006600000000    ?D* S C H I E R E   M E M O R I Z Z A Z I O N E  (12 MESI + ANNO PRECEDENTE)
006700000000    ?D*-------------------
006800000830     D*------------------
006900000830     D* FLAG CONTATORE CLIENTE PER MESE
007000000830     D*------------------
007100000830     D SFCCL           S              1    DIM(12)                              FLAG CONTEGGIO
007200000000     D*------------------
007300020220     D* SCHIERA PER TOTALI DIVISIONE (NO 'D')
007400000000     D*------------------
007500000000     D CLD             S             11  0 DIM(13)                              NUMERO CLIENTI
007600000000     D SPD             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
007700020116     D RRD             S             19  5 DIM(13)                              RICAVI REALI
007800020116     D RPD             S             19  5 DIM(13)                              RICAVI PRESUNTI
007900000000     D*------------------
008000020220     D* SCHIERA PER TOTALI RAGGRUPPAMENTO COMMERCIALE (NO 'D')
008100000000     D*------------------
008200000000     D CLR             S             11  0 DIM(13)                              NUMERO CLIENTI
008300000000     D SPR             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
008400020116     D RRR             S             19  5 DIM(13)                              RICAVI REALI
008500020116     D RPR             S             19  5 DIM(13)                              RICAVI PRESUNTI
008600000000     D*------------------
008700020220     D* SCHIERA PER TOTALI COMMERCIALE (NO 'D')
008800000000     D*------------------
008900000000     D CLC             S             11  0 DIM(13)                              NUMERO CLIENTI
009000000000     D SPC             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
009100020116     D RRC             S             19  5 DIM(13)                              RICAVI REALI
009200020116     D RPC             S             19  5 DIM(13)                              RICAVI PRESUNTI
009300020220     D*------------------
009400020220     D* SCHIERA PER TOTALI DIVISIONE
009500020220     D*------------------
009600020220     D CLDD            S             11  0 DIM(13)                              NUMERO CLIENTI
009700020220     D SPDD            S             13  0 DIM(13)                              NUMERO SPEDIZIONI
009800020220     D RRDD            S             19  5 DIM(13)                              RICAVI REALI
009900020220     D RPDD            S             19  5 DIM(13)                              RICAVI PRESUNTI
010000020220     D*------------------
010100020220     D* SCHIERA PER TOTALI RAGGRUPPAMENTO COMMERCIALE
010200020220     D*------------------
010300020220     D CLRD            S             11  0 DIM(13)                              NUMERO CLIENTI
010400020220     D SPRD            S             13  0 DIM(13)                              NUMERO SPEDIZIONI
010500020220     D RRRD            S             19  5 DIM(13)                              RICAVI REALI
010600020220     D RPRD            S             19  5 DIM(13)                              RICAVI PRESUNTI
010700020220     D*------------------
010800020220     D* SCHIERA PER TOTALI COMMERCIALE
010900020220     D*------------------
011000020220     D CLCD            S             11  0 DIM(13)                              NUMERO CLIENTI
011100020220     D SPCD            S             13  0 DIM(13)                              NUMERO SPEDIZIONI
011200020220     D RRCD            S             19  5 DIM(13)                              RICAVI REALI
011300020220     D RPCD            S             19  5 DIM(13)                              RICAVI PRESUNTI
011400020319     D*------------------
011500020319     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK - NN "D"
011600020319     D*------------------
011700020319     D NTWDIVN         DS                  OCCURS(5) INZ
011800020319     D  CLDN                   1     78P 0
011900020319     D                                     DIM(13)                              NUMERO CLIENTI
012000020319     D  SPDN                  79    169P 0
012100020319     D                                     DIM(13)                              SPEDIZIONI
012200020319     D  RRDN                 170    286P 5
012300020319     D                                     DIM(13)                              RICAVI REALI
012400020319     D  RPDN                 287    403P 5
012500020319     D                                     DIM(13)                              RICAVI PRESUNTI
012600020319     D*------------------
012700020319     D* DS MULTIPLA TOTALI RAGGRUPPAMENTO COMMERCIALE  - 5 NETWORK - NN "D"
012800020319     D*------------------
012900020319     D NTWRGFN         DS                  OCCURS(5) INZ
013000020319     D  CLRN                   1     78P 0
013100020319     D                                     DIM(13)                              NUMERO CLIENTI
013200020319     D  SPRN                  79    169P 0
013300020319     D                                     DIM(13)                              SPEDIZIONI
013400020319     D  RRRN                 170    286P 5
013500020319     D                                     DIM(13)                              RICAVI REALI
013600020319     D  RPRN                 287    403P 5
013700020319     D                                     DIM(13)                              RICAVI PRESUNTI
013800020319     D*------------------
013900020319     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK - NN "D"
014000020319     D*------------------
014100020319     D NTWAGEN         DS                  OCCURS(5) INZ
014200020319     D  CLCN                   1     78P 0
014300020319     D                                     DIM(13)                              NUMERO CLIENTI
014400020319     D  SPCN                  79    169P 0
014500020319     D                                     DIM(13)                              SPEDIZIONI
014600020319     D  RRCN                 170    286P 5
014700020319     D                                     DIM(13)                              RICAVI REALI
014800020319     D  RPCN                 287    403P 5
014900020319     D                                     DIM(13)                              RICAVI PRESUNTI
015000020319     D*------------------
015100020319     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK -
015200020319     D*------------------
015300020319     D NTWDIVND        DS                  OCCURS(5) INZ
015400020319     D  CLDND                  1     78P 0
015500020319     D                                     DIM(13)                              NUMERO CLIENTI
015600020319     D  SPDND                 79    169P 0
015700020319     D                                     DIM(13)                              SPEDIZIONI
015800020319     D  RRDND                170    286P 5
015900020319     D                                     DIM(13)                              RICAVI REALI
016000020319     D  RPDND                287    403P 5
016100020319     D                                     DIM(13)                              RICAVI PRESUNTI
016200020319     D*------------------
016300020319     D* DS MULTIPLA TOTALI RAGGRUPPAMENTO COMMERCIALE  - 5 NETWORK -
016400020319     D*------------------
016500020319     D NTWRGFND        DS                  OCCURS(5) INZ
016600020319     D  CLRND                  1     78P 0
016700020319     D                                     DIM(13)                              NUMERO CLIENTI
016800020319     D  SPRND                 79    169P 0
016900020319     D                                     DIM(13)                              SPEDIZIONI
017000020319     D  RRRND                170    286P 5
017100020319     D                                     DIM(13)                              RICAVI REALI
017200020319     D  RPRND                287    403P 5
017300020319     D                                     DIM(13)                              RICAVI PRESUNTI
017400020319     D*------------------
017500020319     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK -
017600020319     D*------------------
017700020319     D NTWAGEND        DS                  OCCURS(5) INZ
017800020319     D  CLCND                  1     78P 0
017900020319     D                                     DIM(13)                              NUMERO CLIENTI
018000020319     D  SPCND                 79    169P 0
018100020319     D                                     DIM(13)                              SPEDIZIONI
018200020319     D  RRCND                170    286P 5
018300020319     D                                     DIM(13)                              RICAVI REALI
018400020319     D  RPCND                287    403P 5
018500020319     D                                     DIM(13)                              RICAVI PRESUNTI
018600020319     D*------------------
018700020319     D* SCHIERE PER TOTALI 1° E 13° COLONNA X NETWORK ("D" E NN "D")
018800020319     D*------------------
018900020319     D CTDN            S             11  0 DIM(13)
019000020319     D CTDDN           S             11  0 DIM(13)
019100020319     D C2DN            S             11  0 DIM(13)
019200020319     D C2DDN           S             11  0 DIM(13)
019300020319     D CTRN            S             11  0 DIM(13)
019400020319     D CTRDN           S             11  0 DIM(13)
019500020319     D C2RN            S             11  0 DIM(13)
019600020319     D C2RDN           S             11  0 DIM(13)
019700020319     D CTCN            S             11  0 DIM(13)
019800020319     D CTCDN           S             11  0 DIM(13)
019900020319     D C2CN            S             11  0 DIM(13)
020000020319     D C2CDN           S             11  0 DIM(13)
020100020319     D*------------------
020200020319     D* SCHIERE DI STAMPA
020300000000     D*------------------
020400000000     D CLS             S             11  0 DIM(13)                              NUMERO CLIENTI
020500000000     D SPS             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
020600020116     D RRS             S             19  5 DIM(13)                              RICAVI REALI
020700020116     D RPS             S             19  5 DIM(13)                              RICAVI PRESUNTI
020800000000     D*------------------
020900020220     D* SCHIERE DI LAVORO PER TOTALI "A" + "B" + "C" + "T" + "D"
021000000000     D*------------------
021100000000     D CLW             S             11  0 DIM(13)                              NUMERO CLIENTI
021200000000     D SPW             S             13  0 DIM(13)                              NUMERO SPEDIZIONI
021300020116     D RRW             S             19  5 DIM(13)                              RICAVI REALI
021400020116     D RPW             S             19  5 DIM(13)                              RICAVI PRESUNTI
021500000000     D*------------------
021600000000     D* SCHIERE DI LAVORO
021700000000     D*------------------
021800000000     D $CL             S             13  0 DIM(13)                              NUMERO CLIENTI
021900000000     D $SP             S             13  0 DIM(13)                              SPEDIZIONI
022000020116     D $RR             S             19  5 DIM(13)                              RICAVO REALE
022100020116     D $RP             S             19  5 DIM(13)                              RICAVO PRESUNTO
022200000000     D $PD             S              4  1 DIM(13)                              DELTA
022300020116     D $VA             S             19  5 DIM(13)                              VALORE GENERICO
022400000000     D*------------------------------------------------------------------------*
022500000000     D* INPUT
022600000000     D*------------------------------------------------------------------------*
022700000000     D KPJBA         E DS
022800000000     D*------------------
022900000000     D* PARAMETRI FILTRO
023000000000     D*------------------
023100000000     D PARAM           DS
023200990609     D  PARDAT                 1      6  0
023300990609     D  PARAAI                 1      4  0
023400990609     D  PARMMI                 5      6  0
023500990609     D  PARDA2                 7     12  0
023600990609     D  PARAAF                 7     10  0
023700990609     D  PARMMF                11     12  0
023800990609     D  PARDCK                13     13
023900990609     D  PARDCC                14     14
024000990609     D  PARDCF                15     15
024100990609     D  PARFIC                16     18  0
024200990609     D  PARARC                19     21  0
024300990609     D  PARDIC                22     22
024400990609     D  PARDFF                23     23
024500990609     D  PARCOF                24     30  0
024600990609     D  PARDIF                31     31
024700990609     D  PARSTA                32     32
024800000825     D  PARSCE                33     33
024900990609     D  PARTSP                34     34
025000990609     D  PARIOE                35     35
025100990609     D  PARCLV                36     36
025200990609     D  PARDPT                37     42  0
025300990609     D  PARAPI                37     40  0
025400990609     D  PARMPI                41     42  0
025500990609     D  PARDP2                43     48  0
025600990609     D  PARAFP                43     46  0
025700990609     D  PARMFP                47     48  0
025800990609     D  PARSPD                49     52  1
025900990609     D  PARPDD                53     56  1
026000990609     D  PARPDA                57     60  1
026100000707     D  PARPOS                61     61
026200000825     D  PARCRE                62     62
026300010618     D  PARCL1                63     63
026400010618     D  PARCL2                64     64
026500010618     D  PARSIN                65     65
026600020220     D  PARCL3                66     66
026700020220     D  PARCL4                67     67
026800020404     D  PARNTW                68     68
026900020702     D  PARDFD                69     72  1
027000020702     D  PARDFA                73     76  1
027100020702     D  PARDFDP               77     80  1
027200020702     D  PARDFAP               81     84  1
027300030916     D  PARFPC                85     85
027400030916     D  PARWPC                86     86
027500000707     D*------------------
027600000707     D* DS LETTURA CAMPO ORGDE3 DI AZORG00F
027700000707     D*------------------
027800000707     D OG143         E DS
027900090610     D* Descrizioni tipi servizio
028000090610     D*------------------
028100090610     D DS5e          E DS
028200090610     D*------------------
028300000000     D*------------------
028400000000     D* DS AGENTI COMMERCIALI
028500000000     D*------------------
028600140612     D******* DS01          E DS
028700951219     D*------------------
028800951219     D* COMPOSIZIONE DATA SALDI
028900951219     D*------------------
029000951219     D                 DS
029100951219     D  WFDANN                 1      4  0                                      *ANNO
029200951219     D  WFDMES                 5      6  0                                      *MESE
029300951219     D  WFDDAT                 1      6  0                                      *DATA
029400020319     D*------------------
029500020319     D* DS DI RIEMPIMENTO SCHIERA DI WRK WSK1
029600020319     D*------------------
029700020319     D DSWSK1          DS                  INZ
029800020319     D  WFDCLI                        7  0
029900020319     D  MM                            2  0
030000020319     D  WFDNTW                        3
030100030918     d*---
030200030918     d* Variabili
030300030918     d*---
030400030918     d a30             S             30                                         *alfa 30
030500030919     d WNrr            S              7  0 INZ                                  *contatore righe
030600000000     C*------------------------------------------------------------------------*
030700030917     C*?MAIN LINE
030800000000     C*------------------------------------------------------------------------*
030900000000     C*
031000000000     C* STAMPA PARAMETRI FILTRO
031100000000     C                   EXSR      STAPAR
031200000000     C*
031300000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
031400000000     C                   EXSR      SETFIL
031500000000     C*
031600000000     C* CICLO FINO A FINE FILE DI LAVORO
031700000000DO  1C     $FINE         DOWEQ     '0'
031800000000     C*
031900000000     C* OPERAZIONI PER NUOVA DIVISIONE COMMERCIALE
032000000000     C                   EXSR      NEWDIV
032100000000     C*
032200000000     C* CICLO FINO A ROTTURA DI DIVISIONE COMMERCIALE
032300000000DO  2C     $FINE         DOWEQ     '0'
032400000000     C     WFDDIC        ANDEQ     DEPDIV
032500000000     C*
032600000000     C* OPERAZIONI PER NUOVO RAGGRUPPAMENTO FILIALE AGENTE
032700000000     C                   EXSR      NEWRGF
032800000000     C*
032900000000     C* CICLO FINO A ROTTURA DI RAGGRUPPAMENTO FILIALE AGENTE
033000000000DO  3C     $FINE         DOWEQ     '0'
033100000000     C     WFDDIC        ANDEQ     DEPDIV
033200000000     C     WFDRGF        ANDEQ     DEPRGF
033300000000     C*
033400000000     C* OPERAZIONI PER NUOVO AGENTE
033500000000     C                   EXSR      NEWAGE
033600000000     C*
033700000000     C* CICLO FINO A ROTTURA DI AGENTE
033800000000DO  4C     $FINE         DOWEQ     '0'
033900000000     C     WFDDIC        ANDEQ     DEPDIV
034000000000     C     WFDRGF        ANDEQ     DEPRGF
034100000000     C     WFDAGE        ANDEQ     DEPAGE
034200000000     C*
034300000000     C* OPERAZIONI PER NUOVO CLIENTE
034400000000     C                   EXSR      NEWKSC
034500000000     C*
034600000000     C* CICLO FINO A ROTTURA DI AGENTE
034700000000DO  5C     $FINE         DOWEQ     '0'
034800000000     C     WFDDIC        ANDEQ     DEPDIV
034900000000     C     WFDRGF        ANDEQ     DEPRGF
035000000000     C     WFDAGE        ANDEQ     DEPAGE
035100000825     C     WFDCLI        ANDEQ     DEPCLI
035200000000     C*
035300000000     C* MEMORIZZA I SALDI
035400000000     C                   EXSR      MEMSAL
035500000000     C*
035600000000     C* LEGGE IL RECORD SUCCESSIVO
035700000000     C                   EXSR      LETFIL
035800000000     C*
035900000000E   5C                   ENDDO
036000000000E   4C                   ENDDO
036100000000     C*
036200000000     C* STAMPA IL TOTALE COMMERCIALE
036300000000     C                   EXSR      STAAGE
036400000000E   3C                   ENDDO
036500000000     C*
036600000000     C* STAMPA IL TOTALE RAGGRUPPAMENTO FILIALE AGENTE
036700000000     C                   EXSR      STARGF
036800000000E   2C                   ENDDO
036900000000     C*
037000000000     C* STAMPA IL TOTALE DIVISIONE
037100000000     C                   EXSR      STADIV
037200000000E   1C                   ENDDO
037300000000     C*
037400000000     C* STAMPA RIGA FINE STAMPA
037500020116     C                   WRITE     SE94FI
037600000000     C*
037700000000     C                   SETON                                        LR
037800000000     C*------------------------------------------------------------------------*
037900000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
038000000000     C*------------------------------------------------------------------------*
038100000000     C     SETFIL        BEGSR
038200000000     C*
038300000000     C* POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
038400000000     C                   MOVEL     *BLANKS       KWDDIV                         *DIVISIONE
038500000000     C                   MOVEL     *BLANKS       KWDRGD                         *DESCRIZIONE RAGGR.T
038600000000     C                   Z-ADD     *ZEROS        KWDRGF                         *RAGGRUPPAMENTO
038700000000     C                   Z-ADD     *ZEROS        KWDAGE                         *COMMERCIALE
038800000825     C                   Z-ADD     *ZEROS        KWDCLI                         *CLIENTE
038900000000     C                   Z-ADD     *ZEROS        KWDANN                         *ANNO
039000000000     C                   Z-ADD     *ZEROS        KWDMES                         *MESE
039100100202     C     KEYDEL        SETLL     WFDEa02L                           99        *FINE ARCHIVIO
039200000000     C*
039300951219IF  1C     *IN99         IFEQ      *ON
039400000000     C                   MOVEL     '1'           $FINE
039500000000X   1C                   ELSE
039600000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
039700000000E   1C                   ENDIF
039800000000     C*
039900000000     C                   ENDSR
040000000000     C*------------------------------------------------------------------------*
040100000000     C*?LETFIL - LETTURA PROSSIMO RECORD DEL WORK FILE
040200000000     C*------------------------------------------------------------------------*
040300000000     C     LETFIL        BEGSR
040400000000     C*
040500000000     C* LEGGO FINO A:
040600000000     C*     - FINE FILE
040700000000     C*     - TROVATO RECORD VALIDO
040800000000     C                   MOVEL     'N'           WRECOK
040900000000DO  1C     $FINE         DOWEQ     '0'
041000000000     C     WRECOK        ANDEQ     'N'
041100000000     C*
041200000000     C* LETTURE SUCCESSIVE ALLA PRIMA
041300100202     C                   READ      WFDEa02L                               99
041400000000     C*
041500951219IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
041600000000     C                   MOVEL     '1'           $FINE
041700000000X   2C                   ELSE
041800000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
041900000000E   2C                   ENDIF
042000000000     C*
042100000000E   1C                   ENDDO
042200000000     C*
042300000000     C                   ENDSR
042400000000     C*------------------------------------------------------------------------*
042500000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD DEL WORK FILE
042600000000     C*------------------------------------------------------------------------*
042700000000     C     CHKREC        BEGSR
042800000000     C*
042900000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
043000951219     C*
043100990609IF  1C     WFDDAT        IFGE      PARDPT
043200990609     C     WFDDAT        ANDLE     PARDP2
043300951219     C                   MOVEL     'P'           TIPANN                         *PERIODO PRECEDENTE
043400951219E   1C                   ENDIF
043500990609IF  1C     WFDDAT        IFGE      PARDAT
043600990609     C     WFDDAT        ANDLE     PARDA2
043700951219     C                   MOVEL     'C'           TIPANN                         *PERIODO CORRENTE
043800951219E   1C                   ENDIF
043900951018     C*--------------------
044000951018     C* SELEZIONI
044100951018     C*--------------------
044200951018     C*
044300951018     C* SE RAGGRUPPAMENTO AGENTE <> RAGGRUPPAMENTO RICHIESTO, ESCLUDO
044400951018IF  1C     PARCOF        IFGT      *ZEROS                                       *DELTA COMMERCIALE
044500951018     C     WFDRGF        ANDNE     PARCOF
044600951018     C                   MOVEL     'N'           WRECOK
044700951018E   1C                   ENDIF
044800951018     C*
044900951018     C* SE DIVISIONE AGENTE <> DIVISIONE RICHIESTA, ESCLUDO
045000951018IF  1C     PARDIF        IFNE      *BLANKS                                      *DELTA COMMERCIALE
045100951018     C     WFDDIC        ANDNE     PARDIF
045200951018     C                   MOVEL     'N'           WRECOK
045300951018E   1C                   ENDIF
045400960710     C*
045500960710     C* SE CODICE QUALITA' CLIENTE <> CODICE QUALITA' RICHIESTO, ESCLUDO
045600010618IF  1C                   IF        (PARCLV <> *blanks and WFDCLV = PARCLV) or
045700010618     C                             (PARCL1 <> *blanks and WFDCLV = PARCL1) or
045800010619     C                             (PARCL2 <> *blanks and WFDCLV = PARCL2) or
045900020220     C                             (PARCL3 <> *blanks and WFDCLV = PARCL3) or
046000020220     C                             (PARCL4 <> *blanks and WFDCLV = PARCL4) or
046100010619     C                             (PARCLV = *blanks  and
046200010619     C                              PARCL1 = *blanks  and
046300020220     C                              PARCL2 = *blanks  and
046400020220     C                              PARCL3 = *blanks  and
046500020220     C                              PARCL4 = *blanks)
046600010618     C                   ELSE
046700960710     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046800960710E   1C                   ENDIF
046900000000     C*
047000950208     C* ESCLUSIONI PER DELTA ITALIA O ESTERO
047100950210IF  1C     PARIOE        IFNE      *BLANKS
047200950210     C     PARIOE        ANDNE     WFDIOE
047300950208     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047400950208E   1C                   ENDIF
047500000707     C*
047600000707     C* SE DATI NO POSTE ESCLUDE LE FILIALI POSTE, E VICEVERSA
047700000707IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
047800000707     C                   SETOFF                                           28
047900000707     C                   Z-ADD     1             I                 5 0
048000000707     C     WFDFIL        LOOKUP    FIL(I)                                 28
048100000711IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
048200020627IF  3C     POS(I)        IFEQ      'PPT'                                         FIL = POSTE
048300000711     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048400000711E   2C                   ENDIF
048500000711X   2C                   ELSE                                                   *SI DATI POSTE
048600020627IF  3C     POS(I)        IFNE      'PPT'                                         FIL <> POSTE
048700000711     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048800000711E   3C                   ENDIF
048900000711E   2C                   ENDIF
049000000707E   1C                   ENDIF
049100980528     C*
049200000000     C                   ENDSR
049300000000     C*------------------------------------------------------------------------*
049400000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE COMMERCIALE
049500000000     C*------------------------------------------------------------------------*
049600000000     C     NEWDIV        BEGSR
049700020220     C*
049800020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
049900020220     C                   MOVEL     '0'           FLGSIDD
050000000000     C*
050100000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
050200000000     C                   MOVEL     '0'           PASDIV
050300000000     C*
050400000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
050500000000     C                   MOVEL     WFDDIC        DEPDIV
050600000000     C*
050700000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
050800000000     C                   EXSR      DECDIV
050900000000     C*
051000000000     C* AZZERA I TOTALI DELLA DIVISIONE
051100000000     C                   CLEAR                   CLD                            *NUMERO CLIENTI
051200000000     C                   CLEAR                   SPD                            *NUMERO SPEDIZIONI
051300000000     C                   CLEAR                   RRD                            *RICAVI REALI
051400000000     C                   CLEAR                   RPD                            *RICAVI PRESUNTI
051500020220     C                   CLEAR                   CLDD                           *NUMERO CLIENTI
051600020220     C                   CLEAR                   SPDD                           *NUMERO SPEDIZIONI
051700020220     C                   CLEAR                   RRDD                           *RICAVI REALI
051800020220     C                   CLEAR                   RPDD                           *RICAVI PRESUNTI
051900030204DO  1C     1             DO        5             I
052000030204     C     I             OCCUR     NTWDIVN                                      *TOT X NETWORK
052100030204     C                   CLEAR                   CLDN                           *NUMERO CLIENTI
052200030204     C                   CLEAR                   SPDN                           *NUMERO SPEDIZIONI
052300030204     C                   CLEAR                   RRDN                           *RICAVI REALI
052400030204     C                   CLEAR                   RPDN                           *RICAVI PRESUNTI
052500030204     C     I             OCCUR     NTWDIVND                                     *TOT X NETWORK CLI "D"
052600030204     C                   CLEAR                   CLDND                          *NUMERO CLIENTI
052700030204     C                   CLEAR                   SPDND                          *NUMERO SPEDIZIONI
052800030204     C                   CLEAR                   RRDND                          *RICAVI REALI
052900030204     C                   CLEAR                   RPDND                          *RICAVI PRESUNTI
053000030204E   1C                   ENDDO
053100000000     C*
053200000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
053300020220     C                   Z-ADD     *ZEROS        CTD              11 0
053400020220     C                   Z-ADD     *ZEROS        CTDD             11 0
053500020319     C                   CLEAR                   CTDN
053600020319     C                   CLEAR                   CTDDN
053700951227     C*
053800951227     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (PRIMA COLONNA)
053900951227     C                   Z-ADD     *ZEROS        C2D              11 0
054000020220     C                   Z-ADD     *ZEROS        C2DD             11 0
054100020319     C                   CLEAR                   C2DN
054200020319     C                   CLEAR                   C2DDN
054300000000     C*
054400000000     C                   ENDSR
054500000000     C*------------------------------------------------------------------------*
054600000000     C*?NEWRGF - OPERAZIONI PER NUOVO RAGGRUPPAMENTO FILIALE AGENTE
054700000000     C*------------------------------------------------------------------------*
054800000000     C     NEWRGF        BEGSR
054900020220     C*
055000020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
055100020220     C                   MOVEL     '0'           FLGSIDR
055200000000     C*
055300000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
055400000000     C                   MOVEL     '0'           PASRGF
055500000000     C*
055600000000     C* MEMORIZZO IL NUOVO RAGGRUPPAMENTO IN UN DEPOSITO
055700000000     C                   Z-ADD     WFDRGF        DEPRGF
055800000000     C*
055900000000     C* DECODIFICO IL CODICE AGENTE RAGGRUPPAMENTO
056000000000     C                   EXSR      DECRGF
056100000000     C*
056200000000     C* AZZERA I TOTALI DEL RAGGRUPPAMNTO
056300000000     C                   CLEAR                   CLR                            *NUMERO CLIENTI
056400000000     C                   CLEAR                   SPR                            *NUMERO SPEDIZIONI
056500000000     C                   CLEAR                   RRR                            *RICAVI REALI
056600000000     C                   CLEAR                   RPR                            *RICAVI PRESUNTI
056700020220     C                   CLEAR                   CLRD                           *NUMERO CLIENTI
056800020220     C                   CLEAR                   SPRD                           *NUMERO SPEDIZIONI
056900020220     C                   CLEAR                   RRRD                           *RICAVI REALI
057000020220     C                   CLEAR                   RPRD                           *RICAVI PRESUNTI
057100030204DO  1C     1             DO        5             I
057200030204     C     I             OCCUR     NTWRGFN                                      *TOT X NETWORK
057300030204     C                   CLEAR                   CLRN                           *NUMERO CLIENTI
057400030204     C                   CLEAR                   SPRN                           *NUMERO SPEDIZIONI
057500030204     C                   CLEAR                   RRRN                           *RICAVI REALI
057600030204     C                   CLEAR                   RPRN                           *RICAVI PRESUNTI
057700030204     C     I             OCCUR     NTWRGFND                                     *TOT X NETWORK CLI "D"
057800030204     C                   CLEAR                   CLRND                          *NUMERO CLIENTI
057900030204     C                   CLEAR                   SPRND                          *NUMERO SPEDIZIONI
058000030204     C                   CLEAR                   RRRND                          *RICAVI REALI
058100030204     C                   CLEAR                   RPRND                          *RICAVI PRESUNTI
058200030204E   1C                   ENDDO
058300000000     C*
058400000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
058500000000     C                   Z-ADD     *ZEROS        CTR              11 0
058600020220     C                   Z-ADD     *ZEROS        CTRD             11 0
058700020319     C                   CLEAR                   CTRN
058800020319     C                   CLEAR                   CTRDN
058900951227     C*
059000951227     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (PRIMA COLONNA)
059100951227     C                   Z-ADD     *ZEROS        C2R              11 0
059200020220     C                   Z-ADD     *ZEROS        C2RD             11 0
059300020319     C                   CLEAR                   C2RN
059400020319     C                   CLEAR                   C2RDN
059500000000     C*
059600000000     C                   ENDSR
059700000000     C*------------------------------------------------------------------------*
059800000000     C*?NEWAGE - OPERAZIONI PER NUOVO AGENTE COMMERCIALE
059900000000     C*------------------------------------------------------------------------*
060000000000     C     NEWAGE        BEGSR
060100020220     C*
060200020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
060300020220     C                   MOVEL     '0'           FLGSIDA
060400000000     C*
060500000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
060600000000     C                   MOVEL     '0'           PASAGE
060700000000     C*
060800000000     C* MEMORIZZO IL NUOVO AGENTE IN UN DEPOSITO
060900000000     C                   Z-ADD     WFDAGE        DEPAGE
061000000000     C*
061100000000     C* DECODIFICO LA FILAILE DEL CODICE AGENTE
061200000000     C                   MOVEL     WFDAGE        DEPFIL                         *FIL+XXXX
061300000000     C                   EXSR      DECFIL
061400000000     C*
061500000000     C* AZZERA I TOTALI DELL' AGENTE
061600000000     C                   CLEAR                   CLC                            *NUMERO CLIENTI
061700000000     C                   CLEAR                   SPC                            *NUMERO SPEDIZIONI
061800000000     C                   CLEAR                   RRC                            *RICAVI REALI
061900000000     C                   CLEAR                   RPC                            *RICAVI PRESUNTI
062000020220     C                   CLEAR                   CLCD                           *NUMERO CLIENTI
062100020220     C                   CLEAR                   SPCD                           *NUMERO SPEDIZIONI
062200020220     C                   CLEAR                   RRCD                           *RICAVI REALI
062300020220     C                   CLEAR                   RPCD                           *RICAVI PRESUNTI
062400030204DO  1C     1             DO        5             I
062500030204     C     I             OCCUR     NTWAGEN                                      *TOT X NETWORK
062600030204     C                   CLEAR                   CLCN                           *NUMERO CLIENTI
062700030204     C                   CLEAR                   SPCN                           *NUMERO SPEDIZIONI
062800030204     C                   CLEAR                   RRCN                           *RICAVI REALI
062900030204     C                   CLEAR                   RPCN                           *RICAVI PRESUNTI
063000030204     C     I             OCCUR     NTWAGEND                                     *TOT X NETWORK CLI "D"
063100030204     C                   CLEAR                   CLCND                          *NUMERO CLIENTI
063200030204     C                   CLEAR                   SPCND                          *NUMERO SPEDIZIONI
063300030204     C                   CLEAR                   RRCND                          *RICAVI REALI
063400030204     C                   CLEAR                   RPCND                          *RICAVI PRESUNTI
063500030204E   1C                   ENDDO
063600000000     C*
063700000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
063800000000     C                   Z-ADD     *ZEROS        CTC              11 0
063900020220     C                   Z-ADD     *ZEROS        CTCD             11 0
064000020319     C                   CLEAR                   CTCN
064100020319     C                   CLEAR                   CTCDN
064200951227     C*
064300951227     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (PRIMA COLONNA)
064400951227     C                   Z-ADD     *ZEROS        C2C              11 0
064500020220     C                   Z-ADD     *ZEROS        C2CD             11 0
064600020319     C                   CLEAR                   C2CN
064700020319     C                   CLEAR                   C2CDN
064800000000     C*
064900000000     C                   ENDSR
065000000000     C*------------------------------------------------------------------------*
065100000000     C*?NEWKSC - OPERAZIONI PER NUOVO CLIENTE
065200000000     C*------------------------------------------------------------------------*
065300000000     C     NEWKSC        BEGSR
065400000000     C*
065500000000     C* NB: QUESTA ROTTURA SERVE SOLO PER CONTARE IL TOTALE DEI CLIENTI SERVITI
065600951227     C*     NEL PERIODO (PER REIMPOSTARE I CONTATORI "FLAKSC"E "FL2KSC")
065700000000     C*
065800000000     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (13^ COLONNA)
065900000000     C                   MOVEL     '0'           FLAKSC
066000951227     C*
066100951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (1^ COLONNA)
066200951227     C                   MOVEL     '0'           FL2KSC
066300000000     C*
066400000000     C* MEMORIZZO IL NUOVO CLIENTE IN UN DEPOSITO
066500000825     C                   Z-ADD     WFDCLI        DEPCLI
066600000830     C*
066700000830     C* REIMPOSTO FLAG CONTATORE CLIENTI PER MESE
066800000830     C                   MOVEL     '0'           SFCCL
066900020319     C*
067000020319     C* RE-INIZIALIZZO LA SCHIERA DI WRK X CLIENTE/MESE/NETWORK
067100020319     C                   Z-ADD     *ZEROS        Y
067200020319     C                   CLEAR                   WSK1
067300000000     C*
067400000000     C                   ENDSR
067500000000     C*------------------------------------------------------------------------*
067600000000     C*?MEMSAL - MEMORIZZA I SALDI DEL WORK FILE IN SCHIERE
067700000000     C*------------------------------------------------------------------------*
067800000000     C     MEMSAL        BEGSR
067900000000     C*
068000951219     C* I SALDI DEL PERIODO PRECEDENTE LI MEMORIZZA IN 13^ POSIZIONE DI SCHIERA
068100951219     C*
068200000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
068300000000     C                   MOVEL     '1'           PASAGE                         *AGENTE
068400000000     C                   MOVEL     '1'           PASRGF                         *RAGGRUPPAMENTO
068500000000     C                   MOVEL     '1'           PASDIV                         *DIVISIONE
068600951227     C*
068700951227     C* CONTA I CLIENTI DA STAMPARE ALLA 1^ O 13^ COLONNA SOLO A ROTTURA DI
068800951227     C* CLIENTE (SE STA LEGGENDO I SALDI DEL PERIODO PRECEDENTE O CORRENTE)
068900951227IF  1C     TIPANN        IFEQ      'C'                                          *CORRENTE
069000951227IF  2C     FLAKSC        IFEQ      '0'                                          *ROTTURA CLIENTE
069100951227     C                   MOVEL     '1'           FLAKSC
069200951227     C                   Z-ADD     1             CONKSC            1 0          *CLIENTE NON AGGIUNT
069300951227X   2C                   ELSE
069400951227     C                   Z-ADD     *ZEROS        CONKSC                         *CLIENTE AGGIUNTO
069500951227E   2C                   ENDIF
069600951227X   1C                   ELSE                                                   *PRECEDENTE
069700951227IF  2C     FL2KSC        IFEQ      '0'                                          *ROTTURA CLIENTE
069800951227     C                   MOVEL     '1'           FL2KSC
069900951227     C                   Z-ADD     1             CO2KSC            1 0          *CLIENTE AGGIUNTO
070000951227X   2C                   ELSE
070100951227     C                   Z-ADD     *ZEROS        CO2KSC                         *CLIENTE NON AGGIUNT
070200951227E   2C                   ENDIF
070300951227E   1C                   ENDIF
070400951219     C* M E S E
070500990609IF  2C     WFDDAT        IFGE      PARDPT                                       *PERIODO PRECEDENTE
070600990609     C     WFDDAT        ANDLE     PARDP2
070700951219     C                   Z-ADD     13            MM
070800951219E   2C                   ENDIF
070900990609IF  2C     WFDDAT        IFGE      PARDAT                                       *PERIODO CORRENTE
071000990609     C     WFDDAT        ANDLE     PARDA2
071100951219     C                   Z-ADD     1             MM
071200951219     C                   SETOFF                                           28
071300951219     C     WFDMES        LOOKUP    MES(MM)                                28
071400951219E   2C                   ENDIF
071500020220     C*
071600020222     C* VERIFICO SE ALMENO 1 CLIENTE "D"
071700020220     C                   IF        WFDCLV = 'D'
071800020220     C                   MOVEL     '1'           FLGSIDA           1
071900020220     C                   MOVEL     '1'           FLGSIDR           1
072000020220     C                   MOVEL     '1'           FLGSIDD           1
072100020220     C                   ENDIF
072200020319     C*
072300020319     C* VERIFICO SE X GIA' CONTEGGIATO IL CLIENTE/MESE/NETWORK
072400020319     C     DSWSK1        LOOKUP    WSK1                                   55
072500020319     C                   IF        *IN55 = *ON
072600020319     C                   Z-ADD     *ZEROS        CONNTW            1 0
072700020319     C                   ELSE
072800020319     C                   ADD       1             Y                 3 0
072900020319     C                   EVAL      WSK1(Y) = DSWSK1
073000020319     C                   Z-ADD     1             CONNTW
073100020319     C                   ENDIF
073200020319     C*
073300020319     C                   Z-ADD     1             K                 1 0
073400030312     C     WFDNTW        LOOKUP    DNTW(K)                                27
073500030312     C     *IN27         IFEQ      *OFF
073600020319     C                   EVAL      K = 1
073700020319     C                   ENDIF
073800000000     C*--------------------
073900020220     C* C O M M E R C I A L E
074000000000     C*--------------------
074100020319     C     K             OCCUR     NTWAGEN
074200020319     C     K             OCCUR     NTWAGEND
074300000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
074400000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
074500020220     C                   IF        WFDCLV = 'D'
074600020220     C                   ADD       1             CLCD(MM)                       NUMERO CLIENTI
074700020220     C                   ELSE
074800020220     C                   ADD       1             CLC(MM)                        NUMERO CLIENTI
074900020220     C                   ENDIF
075000000830E   3C                   ENDIF
075100020319     C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
075200020319     C                   IF        WFDCLV = 'D'
075300020319     C                   ADD       CONNTW        CLCND(MM)                      NUMERO CLIENTI
075400020319     C                   ELSE
075500020319     C                   ADD       CONNTW        CLCN(MM)                       NUMERO CLIENTI
075600020319     C                   ENDIF
075700020319     C                   ENDIF
075800020220     C                   IF        WFDCLV = 'D'
075900020220     C                   ADD       WFDNSP        SPCD(MM)                       NUMERO SPEDIZIONI
076000020220     C                   ADD       WFDRRN        RRCD(MM)                       RICAVI REALI DELTA
076100020220     C                   ADD       WFDRPN        RPCD(MM)                       RICAVI PRESUNTI DELT
076200020319     C                   ADD       WFDNSP        SPCND(MM)                      NUMERO SPEDIZIONI
076300020319     C                   ADD       WFDRRN        RRCND(MM)                      RICAVI REALI DELTA
076400020319     C                   ADD       WFDRPN        RPCND(MM)                      RICAVI PRESUNTI DELT
076500020220     C                   ELSE
076600000000     C                   ADD       WFDNSP        SPC(MM)                        NUMERO SPEDIZIONI
076700000000     C                   ADD       WFDRRN        RRC(MM)                        RICAVI REALI DELTA
076800000000     C                   ADD       WFDRPN        RPC(MM)                        RICAVI PRESUNTI DELT
076900020319     C                   ADD       WFDNSP        SPCN(MM)                       NUMERO SPEDIZIONI
077000020319     C                   ADD       WFDRRN        RRCN(MM)                       RICAVI REALI DELTA
077100020319     C                   ADD       WFDRPN        RPCN(MM)                       RICAVI PRESUNTI DELT
077200020220     C                   ENDIF
077300951227IF  2C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
077400020220     C                   IF        WFDCLV = 'D'
077500020319     C                   ADD       CO2KSC        C2CD
077600020319     C                   ADD       CONNTW        C2CDN(K)
077700020220     C                   ELSE
077800020220     C                   ADD       CO2KSC        C2C
077900020319     C                   ADD       CONNTW        C2CN(K)
078000020220     C                   ENDIF
078100020220X   2C                   ELSE                                                   *PERIODO CORRENTE
078200020220     C                   IF        WFDCLV = 'D'
078300020319     C                   ADD       CONKSC        CTCD
078400020319     C                   ADD       CONNTW        CTCDN(K)
078500020220     C                   ELSE
078600020220     C                   ADD       CONKSC        CTC
078700020319     C                   ADD       CONNTW        CTCN(K)
078800020220     C                   ENDIF
078900951227E   2C                   ENDIF
079000000000     C*--------------------
079100000000     C* R A G G R U P P A M E N T O
079200000000     C*--------------------
079300020319     C     K             OCCUR     NTWRGFN
079400020319     C     K             OCCUR     NTWRGFND
079500000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
079600000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
079700020220     C                   IF        WFDCLV = 'D'
079800020220     C                   ADD       1             CLRD(MM)                       NUMERO CLIENTI
079900020220     C                   ELSE
080000020220     C                   ADD       1             CLR(MM)                        NUMERO CLIENTI
080100020220     C                   ENDIF
080200000830E   3C                   ENDIF
080300020319     C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
080400020319     C                   IF        WFDCLV = 'D'
080500020319     C                   ADD       CONNTW        CLRND(MM)                      NUMERO CLIENTI
080600020319     C                   ELSE
080700020319     C                   ADD       CONNTW        CLRN(MM)                       NUMERO CLIENTI
080800020319     C                   ENDIF
080900020319     C                   ENDIF
081000020220     C                   IF        WFDCLV = 'D'
081100020220     C                   ADD       WFDNSP        SPRD(MM)                       NUMERO SPEDIZIONI
081200020220     C                   ADD       WFDRRN        RRRD(MM)                       RICAVI REALI DELTA
081300020220     C                   ADD       WFDRPN        RPRD(MM)                       RICAVI PRESUNTI DELT
081400020319     C                   ADD       WFDNSP        SPRND(MM)                      NUMERO SPEDIZIONI
081500020319     C                   ADD       WFDRRN        RRRND(MM)                      RICAVI REALI DELTA
081600020319     C                   ADD       WFDRPN        RPRND(MM)                      RICAVI PRESUNTI DELT
081700020220     C                   ELSE
081800000000     C                   ADD       WFDNSP        SPR(MM)                        NUMERO SPEDIZIONI
081900000000     C                   ADD       WFDRRN        RRR(MM)                        RICAVI REALI DELTA
082000000000     C                   ADD       WFDRPN        RPR(MM)                        RICAVI PRESUNTI DELT
082100020319     C                   ADD       WFDNSP        SPRN(MM)                       NUMERO SPEDIZIONI
082200020319     C                   ADD       WFDRRN        RRRN(MM)                       RICAVI REALI DELTA
082300020319     C                   ADD       WFDRPN        RPRN(MM)                       RICAVI PRESUNTI DELT
082400020220     C                   ENDIF
082500951227IF  2C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
082600020220     C                   IF        WFDCLV = 'D'
082700020220     C                   ADD       CO2KSC        C2RD
082800020319     C                   ADD       CONNTW        C2RDN(K)
082900020220     C                   ELSE
083000951227     C                   ADD       CO2KSC        C2R
083100020319     C                   ADD       CONNTW        C2RN(K)
083200020220     C                   ENDIF
083300951227X   2C                   ELSE                                                   *PERIODO CORRENTE
083400020220     C                   IF        WFDCLV = 'D'
083500020220     C                   ADD       CONKSC        CTRD
083600020319     C                   ADD       CONNTW        CTRDN(K)
083700020220     C                   ELSE
083800020220     C                   ADD       CONKSC        CTR
083900020319     C                   ADD       CONNTW        CTRN(K)
084000020220     C                   ENDIF
084100951227E   2C                   ENDIF
084200000000     C*--------------------
084300000000     C* D I V I S I O N E
084400000000     C*--------------------
084500020319     C     K             OCCUR     NTWDIVN
084600020319     C     K             OCCUR     NTWDIVND
084700000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
084800000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
084900020220     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
085000020220     C                   IF        WFDCLV = 'D'
085100020220     C                   ADD       1             CLDD(MM)                       NUMERO CLIENTI
085200020220     C                   ELSE
085300020220     C                   ADD       1             CLD(MM)                        NUMERO CLIENTI
085400020220     C                   ENDIF
085500000830E   3C                   ENDIF
085600020319     C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
085700020319     C                   IF        WFDCLV = 'D'
085800020319     C                   ADD       CONNTW        CLDND(MM)                      NUMERO CLIENTI
085900020319     C                   ELSE
086000020319     C                   ADD       CONNTW        CLDN(MM)                       NUMERO CLIENTI
086100020319     C                   ENDIF
086200020319     C                   ENDIF
086300020220     C                   IF        WFDCLV = 'D'
086400020220     C                   ADD       WFDNSP        SPDD(MM)                       NUMERO SPEDIZIONI
086500020220     C                   ADD       WFDRRN        RRDD(MM)                       RICAVI REALI DELTA
086600020220     C                   ADD       WFDRPN        RPDD(MM)                       RICAVI PRESUNTI DELT
086700020319     C                   ADD       WFDNSP        SPDND(MM)                      NUMERO SPEDIZIONI
086800020319     C                   ADD       WFDRRN        RRDND(MM)                      RICAVI REALI DELTA
086900020319     C                   ADD       WFDRPN        RPDND(MM)                      RICAVI PRESUNTI DELT
087000020220     C                   ELSE
087100000000     C                   ADD       WFDNSP        SPD(MM)                        NUMERO SPEDIZIONI
087200000000     C                   ADD       WFDRRN        RRD(MM)                        RICAVI REALI DELTA
087300000000     C                   ADD       WFDRPN        RPD(MM)                        RICAVI PRESUNTI DELT
087400020319     C                   ADD       WFDNSP        SPDN(MM)                       NUMERO SPEDIZIONI
087500020319     C                   ADD       WFDRRN        RRDN(MM)                       RICAVI REALI DELTA
087600020319     C                   ADD       WFDRPN        RPDN(MM)                       RICAVI PRESUNTI DELT
087700020220     C                   ENDIF
087800951227IF  2C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
087900020220     C                   IF        WFDCLV = 'D'
088000020220     C                   ADD       CO2KSC        C2DD
088100020319     C                   ADD       CONNTW        C2DDN(K)
088200020220     C                   ELSE
088300020220     C                   ADD       CO2KSC        C2D
088400020319     C                   ADD       CONNTW        C2DN(K)
088500020220     C                   ENDIF
088600951227X   2C                   ELSE                                                   *PERIODO CORRENTE
088700020220     C                   IF        WFDCLV = 'D'
088800020220     C                   ADD       CONKSC        CTDD
088900020319     C                   ADD       CONNTW        CTDDN(K)
089000020220     C                   ELSE
089100951227     C                   ADD       CONKSC        CTD
089200020319     C                   ADD       CONNTW        CTDN(K)
089300020220     C                   ENDIF
089400951227E   2C                   ENDIF
089500000000     C*
089600000000     C                   ENDSR
089700000000     C*------------------------------------------------------------------------*
089800000000     C*?STAAGE - IMPOSTA STAMPA VALORI AGENTE
089900000000     C*------------------------------------------------------------------------*
090000000000     C     STAAGE        BEGSR
090100020221     C*
090200020222     C                   MOVEL(P)  '*'           DESFLD            2
090300000000     C*
090400010618     C* STAMPA I VALORI DELL'AGENTE SOLO SE HO MEMORIZZATO QUALCOSA
090500000000IF  1C     PASAGE        IFEQ      '1'
090600000000     C***
090700000000     C* DESCRIZIONI INTESTAZIONE
090800000000     C***
090900020116     C                   CLEAR                   SE94I2
091000000000     C                   Z-ADD     DEPRGF        I2CRGF                         RAGGRUPPAMENTO AGENT
091100000000     C                   MOVEL     DESRGF        I2DRGF                         DESCRIZIONE RAGGR AG
091200000000     C                   MOVEL     DEPDIV        I2CDIV                         *DIVISIONE
091300000000     C                   MOVEL     DESDIV        I2DDIV                         DESCRIZIONE DIVISION
091400950301     C                   MOVEL     PADIOE        I2DIOE                         *DELTA ITALIA/ESTERO
091500000000     C*
091600020220     C                   MOVEL(p)  DRI(21)       PIDDES
091700000000     C*
091800000000     C                   MOVEL     DESFIL        DES15            15            *FILIALE DA IMPOSTAR
091900000000     C*
092000000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
092100000000     C                   Z-ADD     CLC           CLS                            NUMERO CLIENTI
092200000000     C                   Z-ADD     SPC           SPS                            NUMERO SPEDIZIONI
092300000000     C                   Z-ADD     RRC           RRS                            RICAVI REALI DELTA
092400000000     C                   Z-ADD     RPC           RPS                            RICAVI PRESUNTI DELT
092500951227     C                   Z-ADD     CTC           CTS              11 0          CLIENTI TOTALI 13^CO
092600951227     C                   Z-ADD     C2C           C2S              11 0          CLIENTI TOTALI 1^ CO
092700000000     C*
092800000000     C* ROUTINE DI STAMPA TOTALI
092900000000     C                   EXSR      STAMPA
093000020220     C*
093100020220     C* VERIFICO SE TOTALIZZATO ALMENO 1 CLIENTE 'D'
093200020220     C                   IF        FLGSIDA = '1'
093300020220     C*
093400020221     C                   EVAL      DES15 = DESFIL
093500020222     C                   MOVEL(P)  '**'          DESFLD
093600020220     C*
093700020220     C* SCHIERE MEMORIZZAZIONE CLIENTI 'D' --> SCHIERE STAMPA
093800020220     C                   Z-ADD     CLCD          CLS                            NUMERO CLIENTI
093900020220     C                   Z-ADD     SPCD          SPS                            NUMERO SPEDIZIONI
094000020220     C                   Z-ADD     RRCD          RRS                            RICAVI REALI DELTA
094100020220     C                   Z-ADD     RPCD          RPS                            RICAVI PRESUNTI DELT
094200020220     C                   Z-ADD     CTCD          CTS              11 0          CLIENTI TOTALI 13^CO
094300020220     C                   Z-ADD     C2CD          C2S              11 0          CLIENTI TOTALI 1^ CO
094400020220     C*
094500020220     C* ROUTINE DI STAMPA TOTALI
094600020220     C                   EXSR      STAMPA
094700020220     C*
094800020220     C                   ENDIF
094900000000     C*
095000000000E   1C                   ENDIF
095100000000     C*
095200000000     C                   ENDSR
095300000000     C*------------------------------------------------------------------------*
095400020220     C*?STARGF - IMPOSTA STAMPA VALORI RAGGRUPPAMENTO FILIALE AGENTE
095500000000     C*------------------------------------------------------------------------*
095600000000     C     STARGF        BEGSR
095700020222     C*
095800020222     C                   MOVEL(P)  '*'           DESFLD
095900000000     C*
096000000000     C* STAMPA I VALORI DEL RAGGRUPPAMENTO SOLO SE HO MEMORIZZATO QUALCOSA
096100000000IF  1C     PASRGF        IFEQ      '1'
096200000000     C***
096300000000     C* DESCRIZIONI
096400000000     C***
096500000000     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
096600000000     C*
096700000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
096800000000     C                   Z-ADD     CLR           CLS                            NUMERO CLIENTI
096900000000     C                   Z-ADD     SPR           SPS                            NUMERO SPEDIZIONI
097000000000     C                   Z-ADD     RRR           RRS                            RICAVI REALI DELTA
097100000000     C                   Z-ADD     RPR           RPS                            RICAVI PRESUNTI DELT
097200951227     C                   Z-ADD     CTR           CTS                            CLIENTI TOTALI 13^CO
097300951227     C                   Z-ADD     C2R           C2S                            CLIENTI TOTALI 1^ CO
097400000000     C*
097500000000     C* ROUTINE DI STAMPA TOTALI
097600000000     C                   EXSR      STAMPA
097700020319     C*
097800020319     C* STAMPO L'EVIDENZIAZIONE DEI NETWORK
097900020404     C*
098000020404     C* SOLO SE RICHIESTO NEL LANCIO
098100020404     C                   IF        PARNTW = 'S'
098200020404     C*
098300030312DO  1C     1             DO        5             K
098400020319     C     K             OCCUR     NTWRGFN
098500020319     C                   Z-ADD     CLRN          CLS                            NUMERO CLIENTI
098600020319     C                   Z-ADD     SPRN          SPS                            NUMERO SPEDIZIONI
098700020319     C                   Z-ADD     RRRN          RRS                            RICAVI REALI DELTA
098800020319     C                   Z-ADD     RPRN          RPS                            RICAVI PRESUNTI DELT
098900020319     C                   Z-ADD     CTRN(K)       CTS                            CLIENTI TOTALI 13^CO
099000020319     C                   Z-ADD     C2RN(K)       C2S                            CLIENTI TOTALI 1^ CO
099100020319     C*
099200020319     C* DESCRIZIONI IN STAMPA
099300020319     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
099400020319     C                   MOVE      DNTW(K)       DES15
099500020319     C*
099600020319     C* VERIFICO CHE VI SIA QUALCOSA DA MEMORIZZARE
099700020319     C                   XFOOT     CLS           WCLS             11 0
099800020319     C                   IF        WCLS > *ZEROS
099900020319     C*
100000020319     C* ROUTINE DI STAMPA TOTALI
100100020319     C                   EXSR      STAMPA
100200020319     C                   ENDIF
100300020319     C                   ENDDO
100400020404     C*
100500020404     C                   ENDIF
100600020220     C*
100700020220     C* VERIFICO SE TOTALIZZATO ALMENO 1 CLIENTE 'D'
100800020220     C                   IF        FLGSIDR = '1'
100900020220     C***
101000020220     C* DESCRIZIONI
101100020220     C***
101200020222     C                   MOVEL(P)  '**'          DESFLD
101300020220     C                   MOVEL     DRI(17)       DES15            15            "TOTALI" DA IMPOSTAR
101400000000     C*
101500020220     C* SCHIERE MEMORIZZAZIONE CLIENTI 'D' --> SCHIERE STAMPA
101600020220     C                   Z-ADD     CLRD          CLS                            NUMERO CLIENTI
101700020220     C                   Z-ADD     SPRD          SPS                            NUMERO SPEDIZIONI
101800020220     C                   Z-ADD     RRRD          RRS                            RICAVI REALI DELTA
101900020220     C                   Z-ADD     RPRD          RPS                            RICAVI PRESUNTI DELT
102000020220     C                   Z-ADD     CTRD          CTS                            CLIENTI TOTALI 13^CO
102100020220     C                   Z-ADD     C2RD          C2S                            CLIENTI TOTALI 1^ CO
102200020220     C*
102300020220     C* ROUTINE DI STAMPA TOTALI
102400020220     C                   EXSR      STAMPA
102500020319     C*
102600020319     C* STAMPO L'EVIDENZIAZIONE DEI NETWORK
102700020404     C*
102800020404     C* SOLO SE RICHIESTO NEL LANCIO
102900020404     C                   IF        PARNTW = 'S'
103000020404     C*
103100030312DO  1C     1             DO        5             K
103200020319     C     K             OCCUR     NTWRGFND
103300020319     C                   Z-ADD     CLRND         CLS                            NUMERO CLIENTI
103400020319     C                   Z-ADD     SPRND         SPS                            NUMERO SPEDIZIONI
103500020319     C                   Z-ADD     RRRND         RRS                            RICAVI REALI DELTA
103600020319     C                   Z-ADD     RPRND         RPS                            RICAVI PRESUNTI DELT
103700020319     C                   Z-ADD     CTRDN(K)      CTS                            CLIENTI TOTALI 13^CO
103800020319     C                   Z-ADD     C2RDN(K)      C2S                            CLIENTI TOTALI 1^ CO
103900020319     C*
104000020319     C* DESCRIZIONI IN STAMPA
104100020319     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
104200020319     C                   MOVE      DNTW(K)       DES15
104300020319     C*
104400020319     C* VERIFICO CHE VI SIA QUALCOSA DA MEMORIZZARE
104500020319     C                   XFOOT     CLS           WCLS             11 0
104600020319     C                   IF        WCLS > *ZEROS
104700020319     C*
104800020319     C* ROUTINE DI STAMPA TOTALI
104900020319     C                   EXSR      STAMPA
105000020319     C                   ENDIF
105100020319     C                   ENDDO
105200020220     C*
105300020220     C                   ENDIF
105400020404     C*
105500020404     C                   ENDIF
105600020220     C*
105700000000E   1C                   ENDIF
105800000000     C*
105900020116     C                   WRITE     SE94R2                                       *RIGA DI SEPARAZIONE
106000020220     C                   WRITE     SE94LE                                       *RIGA DI LEGENDA
106100000000     C*
106200000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
106300000000     C                   SETON                                        01
106400000000     C*
106500000000     C                   ENDSR
106600000000     C*------------------------------------------------------------------------*
106700020220     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
106800000000     C*------------------------------------------------------------------------*
106900000000     C     STADIV        BEGSR
107000020221     C*
107100020222     C                   MOVEL(P)  '*'           DESFLD
107200000000     C*
107300000000     C* STAMPA I VALORI DELLA DIVISIONE SOLO SE HO MEMORIZZATO QUALCOSA
107400000000IF  1C     PASDIV        IFEQ      '1'
107500000000     C***
107600000000     C* DESCRIZIONI
107700000000     C***
107800020116     C                   CLEAR                   SE94I2
107900000000     C                   Z-ADD     *ZEROS        I2CRGF                         RAGGRUPPAMENTO AGENT
108000000000     C                   MOVEL     DRI(1)        I2DRGF                         DESCRIZIONE RAGGR AG
108100000000     C                   MOVEL     DEPDIV        I2CDIV                         *DIVISIONE
108200000000     C                   MOVEL     DESDIV        I2DDIV                         DESCRIZIONE DIVISION
108300950313     C                   MOVEL     PADIOE        I2DIOE                         *DELTA ITALIA/ESTERO
108400000000     C*
108500020220     C                   MOVEL(p)  DRI(24)       PIDDES
108600000000     C*
108700000000     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
108800000000     C*
108900000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
109000000000     C                   Z-ADD     CLD           CLS                            NUMERO CLIENTI
109100000000     C                   Z-ADD     SPD           SPS                            NUMERO SPEDIZIONI
109200000000     C                   Z-ADD     RRD           RRS                            RICAVI REALI DELTA
109300000000     C                   Z-ADD     RPD           RPS                            RICAVI PRESUNTI DELT
109400951227     C                   Z-ADD     CTD           CTS                            CLIENTI TOTALI 13^CO
109500951227     C                   Z-ADD     C2D           C2S                            CLIENTI TOTALI 1^ CO
109600000000     C*
109700000000     C* ROUTINE DI STAMPA TOTALI
109800000000     C                   EXSR      STAMPA
109900020319     C*
110000020404     C* STAMPO L'EVIDENZIAZIONE DEI NETWORK
110100020404     C*
110200020404     C* SOLO SE RICHIESTO NEL LANCIO
110300020404     C                   IF        PARNTW = 'S'
110400020404     C*
110500030312DO  1C     1             DO        5             K
110600020319     C     K             OCCUR     NTWDIVN
110700020319     C                   Z-ADD     CLDN          CLS                            NUMERO CLIENTI
110800020319     C                   Z-ADD     SPDN          SPS                            NUMERO SPEDIZIONI
110900020319     C                   Z-ADD     RRDN          RRS                            RICAVI REALI DELTA
111000020319     C                   Z-ADD     RPDN          RPS                            RICAVI PRESUNTI DELT
111100020319     C                   Z-ADD     CTDN(K)       CTS                            CLIENTI TOTALI 13^CO
111200020319     C                   Z-ADD     C2DN(K)       C2S                            CLIENTI TOTALI 1^ CO
111300020319     C*
111400020319     C* DESCRIZIONI IN STAMPA
111500020319     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
111600020319     C                   MOVE      DNTW(K)       DES15
111700020319     C*
111800020319     C* VERIFICO CHE VI SIA QUALCOSA DA MEMORIZZARE
111900020319     C                   XFOOT     CLS           WCLS             11 0
112000020319     C                   IF        WCLS > *ZEROS
112100020319     C*
112200020319     C* ROUTINE DI STAMPA TOTALI
112300020319     C                   EXSR      STAMPA
112400020319     C                   ENDIF
112500020319     C                   ENDDO
112600020404     C*
112700020404     C                   ENDIF
112800020220     C*
112900020220     C* VERIFICO SE TOTALIZZATO ALMENO 1 CLIENTE 'D'
113000020220     C                   IF        FLGSIDD = '1'
113100020220     C*
113200020222     C                   MOVEL(P)  '**'          DESFLD
113300020220     C                   MOVEL     DRI(17)       DES15            15            "TOTALI" DA IMPOSTAR
113400020220     C*
113500020220     C* SCHIERE MEMORIZZAZIONE CLIENTI 'D' --> SCHIERE STAMPA
113600020220     C                   Z-ADD     CLDD          CLS                            NUMERO CLIENTI
113700020220     C                   Z-ADD     SPDD          SPS                            NUMERO SPEDIZIONI
113800020220     C                   Z-ADD     RRDD          RRS                            RICAVI REALI DELTA
113900020220     C                   Z-ADD     RPDD          RPS                            RICAVI PRESUNTI DELT
114000020220     C                   Z-ADD     CTDD          CTS                            CLIENTI TOTALI 13^CO
114100020220     C                   Z-ADD     C2DD          C2S                            CLIENTI TOTALI 1^ CO
114200020220     C*
114300020220     C* ROUTINE DI STAMPA TOTALI
114400020220     C                   EXSR      STAMPA
114500020319     C*
114600020319     C* STAMPO L'EVIDENZIAZIONE DEI NETWORK
114700020404     C*
114800020404     C* SOLO SE RICHIESTO NEL LANCIO
114900020404     C                   IF        PARNTW = 'S'
115000020404     C*
115100030312DO  1C     1             DO        5             K
115200020319     C     K             OCCUR     NTWDIVND
115300020319     C                   Z-ADD     CLDND         CLS                            NUMERO CLIENTI
115400020319     C                   Z-ADD     SPDND         SPS                            NUMERO SPEDIZIONI
115500020319     C                   Z-ADD     RRDND         RRS                            RICAVI REALI DELTA
115600020319     C                   Z-ADD     RPDND         RPS                            RICAVI PRESUNTI DELT
115700020319     C                   Z-ADD     CTDDN(K)      CTS                            CLIENTI TOTALI 13^CO
115800020319     C                   Z-ADD     C2DDN(K)      C2S                            CLIENTI TOTALI 1^ CO
115900020319     C*
116000020319     C* DESCRIZIONI IN STAMPA
116100020319     C                   MOVEL     DRI(12)       DES15            15            "TOTALI" DA IMPOSTAR
116200020319     C                   MOVE      DNTW(K)       DES15
116300020319     C*
116400020319     C* VERIFICO CHE VI SIA QUALCOSA DA MEMORIZZARE
116500020319     C                   XFOOT     CLS           WCLS             11 0
116600020319     C                   IF        WCLS > *ZEROS
116700020319     C*
116800020319     C* ROUTINE DI STAMPA TOTALI
116900020319     C                   EXSR      STAMPA
117000020319     C                   ENDIF
117100020319     C                   ENDDO
117200020220     C*
117300020220     C                   ENDIF
117400020404     C*
117500020404     C                   ENDIF
117600000000     C*
117700000000E   1C                   ENDIF
117800000000     C*
117900020116     C                   WRITE     SE94R2                                       *RIGA DI SEPARAZIONE
118000020220     C                   WRITE     SE94LE                                       *RIGA DI LEGENDA
118100000000     C*
118200000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
118300000000     C                   SETON                                        01
118400000000     C*
118500000000     C                   ENDSR
118600000000     C*------------------------------------------------------------------------*
118700020220     C*?STAMPA - STAMPA I DATI
118800000000     C*------------------------------------------------------------------------*
118900000000     C     STAMPA        BEGSR
119000000000     C*--------------------
119100000000     C* I N T E S T A Z I O N E
119200000000     C*--------------------
119300000000IF  1C     *IN01         IFEQ      *ON
119400000000     C                   EXSR      STAINT
119500000000E   1C                   ENDIF
119600000000     C*--------------------
119700000000     C* C L I E N T I
119800000000     C*--------------------
119900000000     C                   MOVEL     *BLANKS       P1DDES
120000020221     C                   MOVEL     DESFLD        P1DFLD
120100000000     C                   MOVEL     DRI(13)       DES07             7
120200000000     C                   MOVEL     DES15         P1DDES
120300000000     C                   MOVE      DES07         P1DDES
120400000000     C*
120500000000     C                   Z-ADD     CLS           $VA
120600020116     C                   Z-ADD     CTS           $VA13C           19 5
120700020116     C                   Z-ADD     C2S           $V213C           19 5
120800000000     C*
120900000000     C* SCRIVE I VALORI IN STAMPA
121000000000     C                   EXSR      SCRVAL
121100030917     C*
121200030917     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
121300030917     C                   MOVEL     PARIOE        SE94IOE                        *DELTA ITALIA/ESTERO
121400030917     C                   MOVEL     DEPDIV        SE94DIV                        *DIVISIONE
121500060104      * se totali Divisione deve impostare a 0 il campo sul file
121600060104     c                   if        i2crgf = 0
121700060104     C                   Z-ADD     0             SE94RGF                        *AGENTE UNIFICANTE
121800060104     c                   else
121900030917     C                   Z-ADD     DEPRGF        SE94RGF                        *AGENTE UNIFICANTE
122000060104     c                   end
122100030918     C                   EVAL      a30 = P1DFLD + P1DDES
122200030918     C                   MOVEL     a30           SE94DES                        *DESCRIZIONE
122300030917     C                   Z-ADD     $V213C        SE94CPR                        *VALORE PRECEDENTE
122400030917     C                   Z-ADD     $VA(1)        SE94C01                        *MESE UNO
122500030917     C                   Z-ADD     $VA(2)        SE94C02                        *MESE DUE
122600030917     C                   Z-ADD     $VA(3)        SE94C03                        *MESE TRE
122700030917     C                   Z-ADD     $VA(4)        SE94C04                        *MESE QUATTRO
122800030917     C                   Z-ADD     $VA(5)        SE94C05                        *MESE CINQUE
122900030917     C                   Z-ADD     $VA(6)        SE94C06                        *MESE SEI
123000030917     C                   Z-ADD     $VA(7)        SE94C07                        *MESE SETTE
123100030917     C                   Z-ADD     $VA(8)        SE94C08                        *MESE OTTO
123200030917     C                   Z-ADD     $VA(9)        SE94C09                        *MESE NOVE
123300030917     C                   Z-ADD     $VA(10)       SE94C10                        *MESE DIECI
123400030917     C                   Z-ADD     $VA(11)       SE94C11                        *MESE UNDICI
123500030917     C                   Z-ADD     $VA(12)       SE94C12                        *MESE DODICI
123600030917     C                   Z-ADD     $VA13C        SE94CTO                        *TOTALE MESI
123700000000     C*--------------------
123800000000     C* S P E D I Z I O N I
123900000000     C*--------------------
124000000000IF  1C     *IN01         IFEQ      *ON
124100000000     C                   EXSR      STAINT
124200000000E   1C                   ENDIF
124300000000     C*
124400000000     C                   MOVEL     *BLANKS       P1DDES
124500020221     C                   MOVEL     *BLANKS       P1DFLD
124600000000     C                   MOVEL     DRI(14)       DES07             7
124700000000     C                   MOVE      DES07         P1DDES
124800000000     C*
124900000000     C                   Z-ADD     SPS           $VA
125000020116     C                   Z-ADD     *ZEROS        $VA13C
125100000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
125200000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
125300000000E   1C                   ENDDO
125400020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
125500951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
125600020220     C*
125700020220     C* SCRIVE I VALORI IN STAMPA
125800000000     C                   EXSR      SCRVAL
125900030917     C*
126000030917     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
126100030917     C                   Z-ADD     $V213C        SE94NPR                        *VALORE PRECEDENTE
126200030917     C                   Z-ADD     $VA(1)        SE94N01                        *MESE UNO
126300030917     C                   Z-ADD     $VA(2)        SE94N02                        *MESE DUE
126400030917     C                   Z-ADD     $VA(3)        SE94N03                        *MESE TRE
126500030917     C                   Z-ADD     $VA(4)        SE94N04                        *MESE QUATTRO
126600030917     C                   Z-ADD     $VA(5)        SE94N05                        *MESE CINQUE
126700030917     C                   Z-ADD     $VA(6)        SE94N06                        *MESE SEI
126800030917     C                   Z-ADD     $VA(7)        SE94N07                        *MESE SETTE
126900030917     C                   Z-ADD     $VA(8)        SE94N08                        *MESE OTTO
127000030917     C                   Z-ADD     $VA(9)        SE94N09                        *MESE NOVE
127100030917     C                   Z-ADD     $VA(10)       SE94N10                        *MESE DIECI
127200030917     C                   Z-ADD     $VA(11)       SE94N11                        *MESE UNDICI
127300030917     C                   Z-ADD     $VA(12)       SE94N12                        *MESE DODICI
127400030917     C                   Z-ADD     $VA13C        SE94NTO                        *TOTALE MESI
127500000000     C*--------------------
127600000000     C* R I C A V I
127700000000     C*--------------------
127800000000IF  1C     *IN01         IFEQ      *ON
127900000000     C                   EXSR      STAINT
128000000000E   1C                   ENDIF
128100000000     C*
128200000000     C                   MOVEL     *BLANKS       P1DDES
128300020221     C                   MOVEL     *BLANKS       P1DFLD
128400000000     C                   MOVEL     DRI(15)       DES07             7
128500000000     C                   MOVE      DES07         P1DDES
128600000000     C*
128700020116     C* === CON L'EURO NN SI "MIGLIALIZZA" + ===
128800020116     C*    RRS           DIV       1000          $VA
128900020116     C                   Z-ADD     RRS           $VA
129000020116     C                   Z-ADD     *ZEROS        $VA13C
129100020116DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
129200020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
129300000000E   1C                   ENDDO
129400020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
129500951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA
129600000000     C*
129700000000     C* SCRIVE I VALORI IN STAMPA
129800000000     C                   EXSR      SCRVAL
129900030917     C*
130000030917     C* IMPOSTA I CAMPI "RICAVI REALI" DEL WFILE DI STAMPA
130100030917     C                   Z-ADD     $V213C        SE94RPR                        *VALORE PRECEDENTE
130200030917     C                   Z-ADD     $VA(1)        SE94R01                        *MESE UNO
130300030917     C                   Z-ADD     $VA(2)        SE94R02                        *MESE DUE
130400030917     C                   Z-ADD     $VA(3)        SE94R03                        *MESE TRE
130500030917     C                   Z-ADD     $VA(4)        SE94R04                        *MESE QUATTRO
130600030917     C                   Z-ADD     $VA(5)        SE94R05                        *MESE CINQUE
130700030917     C                   Z-ADD     $VA(6)        SE94R06                        *MESE SEI
130800030917     C                   Z-ADD     $VA(7)        SE94R07                        *MESE SETTE
130900030917     C                   Z-ADD     $VA(8)        SE94R08                        *MESE OTTO
131000030917     C                   Z-ADD     $VA(9)        SE94R09                        *MESE NOVE
131100030917     C                   Z-ADD     $VA(10)       SE94R10                        *MESE DIECI
131200030917     C                   Z-ADD     $VA(11)       SE94R11                        *MESE UNDICI
131300030917     C                   Z-ADD     $VA(12)       SE94R12                        *MESE DODICI
131400030917     C                   Z-ADD     $VA13C        SE94RTO                        *TOTALE MESI
131500000000     C*--------------------
131600000000     C* D E L T A
131700000000     C*--------------------
131800000000IF  1C     *IN01         IFEQ      *ON
131900000000     C                   EXSR      STAINT
132000000000E   1C                   ENDIF
132100000000     C*
132200000000     C                   MOVEL     *BLANKS       P2DDES
132300020221     C                   MOVEL     *BLANKS       P2DFLD
132400000000     C                   MOVEL     DRI(16)       DES07             7            *SECONDA DESCRIZIONE
132500000000     C                   MOVE      DES07         P2DDES
132600000000     C*
132700000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
132800000000     C                   Z-ADD     RRS           $RR                            *RICAVI REALI
132900000000     C                   Z-ADD     RPS           $RP                            *RICAVI PRESUNTI
133000000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
133100000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
133200000000     C                   EXSR      IMPCAL
133300000000     C*
133400000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
133500000000     C                   EXSR      SCRDEL
133600030917     C*
133700030917     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
133800030917     C                   Z-ADD     $PD(13)       SE94DPR                        *VALORE PRECEDENTE
133900030917     C                   Z-ADD     $PD(1)        SE94D01                        *MESE UNO
134000030917     C                   Z-ADD     $PD(2)        SE94D02                        *MESE DUE
134100030917     C                   Z-ADD     $PD(3)        SE94D03                        *MESE TRE
134200030917     C                   Z-ADD     $PD(4)        SE94D04                        *MESE QUATTRO
134300030917     C                   Z-ADD     $PD(5)        SE94D05                        *MESE CINQUE
134400030917     C                   Z-ADD     $PD(6)        SE94D06                        *MESE SEI
134500030917     C                   Z-ADD     $PD(7)        SE94D07                        *MESE SETTE
134600030917     C                   Z-ADD     $PD(8)        SE94D08                        *MESE OTTO
134700030917     C                   Z-ADD     $PD(9)        SE94D09                        *MESE NOVE
134800030917     C                   Z-ADD     $PD(10)       SE94D10                        *MESE DIECI
134900030917     C                   Z-ADD     $PD(11)       SE94D11                        *MESE UNDICI
135000030917     C                   Z-ADD     $PD(12)       SE94D12                        *MESE DODICI
135100030917     C                   Z-ADD     $PD13C        SE94DTO                        *TOTALE MESI
135200030917     C*
135300030917     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
135400030917IF  1C     PARWPC        IFEQ      'S'
135500030919     c                   ADD       1             WNrr
135600030919     c                   Z-ADD     WNrr          SE94NRR                        *righe in stampa
135700030917     C                   WRITE     WFTISE940
135800030917E   1C                   ENDIF
135900000000     C*
136000000000     C                   ENDSR
136100000000     C*------------------------------------------------------------------------*
136200000000     C*?STAINT - STAMPA INTESTAZIONE
136300000000     C*------------------------------------------------------------------------*
136400000000     C     STAINT        BEGSR
136500000000     C*
136600020116     C                   WRITE     SE94PT                                       *TESTATA
136700020116     C                   WRITE     SE94I2                                       *RIGA DESCRITTIVA
136800020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
136900020116     C                   WRITE     SE94PI                                       *RIGA MESI
137000020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
137100000000     C                   SETOFF                                       01
137200000000     C*
137300000000     C                   ENDSR
137400000000     C*------------------------------------------------------------------------*
137500000000     C*?IMPCAL - IMPOSTA IL CALCOLO DEL DESTA PER TUTTI I VALORI
137600000000     C*          INPUT  --> $RR    : SCHIERA RICAVO REALE
137700000000     C*                     $RP    : SCHIERA RICAVO PRESUNTO
137800000000     C*          OUTPUT --> $PD    : SCHIERA DELTA PERCENTUALE
137900000000     C*                     $PD13C : 13^ PERCENTUALE DELTA
138000000000     C*------------------------------------------------------------------------*
138100000000     C     IMPCAL        BEGSR
138200000000     C*--------------------
138300020220     C* DELTA PERIODO PRECEDENTE
138400000000     C*--------------------
138500020116     C                   Z-ADD     $RR(13)       WRRW             19 5          *RICAVO REALE
138600020116     C                   Z-ADD     $RP(13)       WRPW             19 5          *RICAVO PRESUNTO
138700000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
138800000000     C                   EXSR      CALDEL
138900000000     C                   Z-ADD     WDPW          $PD(13)                        *SCHIERA DELTA
139000000000     C*--------------------
139100000000     C* DELTA 12 MESI
139200000000     C*--------------------
139300020116DO  1C     1             DO        SPARM         J
139400020116     C                   Z-ADD     $RR(J)        WRRW                           *RICAVO REALE
139500020116     C                   Z-ADD     $RP(J)        WRPW                           *RICAVO PRESUNTO
139600020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
139700000000     C                   EXSR      CALDEL
139800000000     C                   Z-ADD     WDPW          $PD(J)                         *SCHIERA DELTA
139900000000E   1C                   ENDDO
140000000000     C*--------------------
140100000000     C* DELTA 13^ COLONNA
140200000000     C*--------------------
140300000000     C                   Z-ADD     *ZEROS        WRRW
140400000000     C                   Z-ADD     *ZEROS        WRPW
140500000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
140600020116     C                   ADD       $RR(J)        WRRW                           *RICAVO REALE
140700020116     C                   ADD       $RP(J)        WRPW                           *RICAVO PRESUNTO
140800000000E   1C                   ENDDO
140900020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
141000000000     C                   EXSR      CALDEL
141100000000     C                   Z-ADD     WDPW          $PD13C                         *13^ COLONNA
141200000000     C*
141300000000     C                   ENDSR
141400000000     C*------------------------------------------------------------------------*
141500000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
141600000000     C*          INPUT  --> WRRW: RICAVO REALE
141700000000     C*                     WRPW: RICAVO PRESUNTO
141800000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
141900000000     C*------------------------------------------------------------------------*
142000000000     C     CALDEL        BEGSR
142100000000     C*--------------------
142200000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
142300000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
142400000000     C*--------------------
142500000000     C*
142600000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
142700020116     C     WRRW          SUB       WRPW          GAP              19 5          (REALE - PRESUNTO)
142800000000     C     GAP           MULT      100           GAP                            (GAP x 100)
142900000000     C*
143000000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
143100000000IF  1C     WRRW          IFEQ      *ZEROS
143200000000     C                   Z-ADD     1             WRRW
143300000000E   1C                   ENDIF
143400000000     C*
143500000000     C     GAP           DIV       WRRW          GAP
143600000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
143700000000     C*
143800000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
143900000000     C     WDPW          COMP      0                                    28
144000000000     C* POSITIVO
144100000000IF  1C     *IN28         IFEQ      *OFF
144200000000IF  2C     GAP           IFGE      1000
144300000000     C                   Z-ADD     +999,9        WDPW
144400000000E   2C                   ENDIF
144500000000     C* NEGATIVO
144600000000X   1C                   ELSE
144700000000IF  2C     GAP           IFLE      -1000
144800000000     C                   Z-ADD     -999,9        WDPW
144900000000E   2C                   ENDIF
145000000000E   1C                   ENDIF
145100000000     C*
145200000000     C                   ENDSR
145300000000     C*------------------------------------------------------------------------*
145400000000     C*?SCRVAL - SCRIVE I VALORI IN STAMPA
145500000000     C*------------------------------------------------------------------------*
145600000000     C     SCRVAL        BEGSR
145700000000     C*
145800951227     C                   Z-ADD     $V213C        P1CVAP                         *VALORE PRECEDENTE
145900000000     C                   Z-ADD     $VA(1)        P1CVA1                         *MESE UNO
146000000000     C                   Z-ADD     $VA(2)        P1CVA2                         *MESE DUE
146100000000     C                   Z-ADD     $VA(3)        P1CVA3                         *MESE TRE
146200000000     C                   Z-ADD     $VA(4)        P1CVA4                         *MESE QUATTRO
146300000000     C                   Z-ADD     $VA(5)        P1CVA5                         *MESE CINQUE
146400000000     C                   Z-ADD     $VA(6)        P1CVA6                         *MESE SEI
146500000000     C                   Z-ADD     $VA(7)        P1CVA7                         *MESE SETTE
146600000000     C                   Z-ADD     $VA(8)        P1CVA8                         *MESE OTTO
146700000000     C                   Z-ADD     $VA(9)        P1CVA9                         *MESE NOVE
146800000000     C                   Z-ADD     $VA(10)       P1CVA0                         *MESE DIECI
146900000000     C                   Z-ADD     $VA(11)       P1CVAA                         *MESE UNDICI
147000000000     C                   Z-ADD     $VA(12)       P1CVAB                         *MESE DODICI
147100000000     C                   Z-ADD     $VA13C        P1CVAT                         *TOTALE MESI
147200000000     C*
147300020116     C                   WRITE     SE94P1                                       *STAMPA RIGA VALORE
147400000000     C*
147500000000     C                   ENDSR
147600000000     C*------------------------------------------------------------------------*
147700000000     C*?SCRDEL - SCRIVE I VALORI DELTA IN STAMPA
147800000000     C*------------------------------------------------------------------------*
147900000000     C     SCRDEL        BEGSR
148000000000     C*
148100000000     C                   Z-ADD     $PD(13)       P2CDEP                         *VALORE PRECEDENTE
148200000000     C                   Z-ADD     $PD(1)        P2CDE1                         *MESE UNO
148300000000     C                   Z-ADD     $PD(2)        P2CDE2                         *MESE DUE
148400000000     C                   Z-ADD     $PD(3)        P2CDE3                         *MESE TRE
148500000000     C                   Z-ADD     $PD(4)        P2CDE4                         *MESE QUATTRO
148600000000     C                   Z-ADD     $PD(5)        P2CDE5                         *MESE CINQUE
148700000000     C                   Z-ADD     $PD(6)        P2CDE6                         *MESE SEI
148800000000     C                   Z-ADD     $PD(7)        P2CDE7                         *MESE SETTE
148900000000     C                   Z-ADD     $PD(8)        P2CDE8                         *MESE OTTO
149000000000     C                   Z-ADD     $PD(9)        P2CDE9                         *MESE NOVE
149100000000     C                   Z-ADD     $PD(10)       P2CDE0                         *MESE DIECI
149200000000     C                   Z-ADD     $PD(11)       P2CDEA                         *MESE UNDICI
149300000000     C                   Z-ADD     $PD(12)       P2CDEB                         *MESE DODICI
149400000000     C                   Z-ADD     $PD13C        P2CDET                         *TOTALE MESI
149500000000     C*
149600020116     C                   WRITE     SE94P2                                       *STAMPA RIGA VALORE
149700000000     C*
149800000000     C                   ENDSR
149900000000     C*------------------------------------------------------------------------*
150000000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
150100000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
150200000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
150300000000     C*------------------------------------------------------------------------*
150400000000     C     DECDIV        BEGSR
150500000000     C*
150600000000     C                   Z-ADD     1             D
150700000000     C     DEPDIV        LOOKUP    DIV(D)                                 99
150800000000IF  1C     *IN99         IFEQ      *OFF
150900000000     C                   MOVEL     *ALL'*'       DESDIV           20
151000000000X   1C                   ELSE
151100000000     C                   MOVEL     DDIV(D)       DESDIV
151200000000E   1C                   ENDIF
151300000000     C*
151400000000     C                   ENDSR
151500000000     C*------------------------------------------------------------------------*
151600000000     C*?DECRGF - DECODIFICA RAGGRUPPAMENTO FILIALE AGENTE
151700000000     C*          INPUT  -- DEPRGF: CODICE RAGGRUPPAMENTO
151800000000     C*          OUTPUT -- DESRGF: DESCRIZIONE RAGGRUPPAMENTO FILIALE
151900000000     C*------------------------------------------------------------------------*
152000000000     C     DECRGF        BEGSR
152100000000     C*
152200000000     C                   MOVEL     WFDRGD        DESRGF
152300000000     C*
152400000000     C                   ENDSR
152500000000     C*------------------------------------------------------------------------*
152600000000     C*? DECFIL - DECODIFICA CODICE FILIALE
152700000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
152800000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
152900000000     C*------------------------------------------------------------------------*
153000000000     C     DECFIL        BEGSR
153100000000     C*
153200000000     C                   Z-ADD     1             D
153300000000     C     DEPFIL        LOOKUP    FIL(D)                                 99
153400000000IF  1C     *IN99         IFEQ      *OFF
153500000000     C                   MOVEL     *ALL'*'       DESFIL           20
153600000000X   1C                   ELSE
153700000000     C                   MOVEL     DFIL(D)       DESFIL
153800000000E   1C                   ENDIF
153900000000     C*
154000000000     C                   ENDSR
154100000000     C*------------------------------------------------------------------------*
154200000000     C*?DECAGE - DECODIFICA CODICE AGENTE
154300000000     C*          INPUT  -- DEPAGE: CODICE AGENTE COMMERCIALE
154400000000     C*          OUTPUT -- DESAGE: DESCRIZIONE AGENTE
154500000000     C*                    DESPCF: PERCENTUALE COMPETENZA FILIALE
154600000000     C*------------------------------------------------------------------------*
154700000000     C     DECAGE        BEGSR
154800000000     C*
154900980407     C                   MOVEL     *BLANKS       DESAGE           25
155000000000     C                   Z-ADD     1             D
155100000000     C     DEPAGE        LOOKUP    AGE(D)                                 99
155200000000IF  1C     *IN99         IFEQ      *OFF
155300980407     C                   MOVEL     *ALL'*'       DESAGE
155400000000     C                   Z-ADD     *ZEROS        DESPCF
155500000000X   1C                   ELSE
155600000000     C                   MOVEL     DAGE(D)       DESAGE
155700000000     C                   Z-ADD     CAGE(D)       DESPCF
155800000000E   1C                   ENDIF
155900000000     C*
156000000000     C                   ENDSR
156100000000     C*------------------------------------------------------------------------*
156200000000     C*? DECARE - DECODIFICA CODICE AREA
156300000000     C*          INPUT  -- DEPARE: CODICE AREA
156400000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
156500000000     C*------------------------------------------------------------------------*
156600000000     C     DECARE        BEGSR
156700000000     C*
156800000000     C                   Z-ADD     1             D
156900000000     C     DEPARE        LOOKUP    ARE(D)                                 99
157000000000IF  1C     *IN99         IFEQ      *OFF
157100000000     C                   MOVEL     *ALL'*'       DESARE           20
157200000000X   1C                   ELSE
157300000000     C                   MOVEL     DARE(D)       DESARE
157400000000E   1C                   ENDIF
157500000000     C*
157600000000     C                   ENDSR
157700000000     C*------------------------------------------------------------------------*
157800000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
157900000000     C*------------------------------------------------------------------------*
158000000000     C     CARMES        BEGSR
158100000000     C*
158200000000     C                   CLEAR                   ANN                            *ANNI
158300000000     C                   CLEAR                   MES                            *MESI
158400000000     C                   Z-ADD     *ZEROS        J                              *N° MESI DA ELABORAR
158500000000     C*
158600990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
158700000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
158800990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
158900000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
159000000000     C*
159100000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
159200000000IF  1C     WAA2          IFEQ      WAA
159300000000     C*
159400000000     C* CALCOLO I MESI DI DIFFERENZA
159500000000DO  2C     WMM           DOWLE     WMM2
159600000000     C                   ADD       1             J
159700000000     C                   Z-ADD     WAA           ANN(J)
159800000000     C                   Z-ADD     WMM           MES(J)
159900000000     C                   ADD       1             WMM
160000000000E   2C                   ENDDO
160100000000     C***
160200000000     C* SPARM = N° MESI SELEZIONATI
160300000000     C***
160400000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
160500000000     C*
160600000000     C* IL RESTO DELLA SCHIERA A ZERO
160700000000DO  2C     J             DOWLT     12
160800000000     C                   ADD       1             J
160900000000     C                   Z-ADD     *ZEROS        ANN(J)
161000000000     C                   Z-ADD     *ZEROS        MES(J)
161100000000E   2C                   ENDDO
161200000000     C*
161300000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
161400000000X   1C                   ELSE
161500000000     C*
161600000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
161700000000DO  2C     WMM           DOWLE     12
161800000000     C                   ADD       1             J
161900000000     C                   Z-ADD     WAA           ANN(J)
162000000000     C                   Z-ADD     WMM           MES(J)
162100000000     C                   ADD       1             WMM
162200000000E   2C                   ENDDO
162300000000     C*
162400000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
162500000000     C                   Z-ADD     01            WMM
162600000000DO  2C     WMM           DOWLE     WMM2
162700000000     C                   ADD       1             J
162800000000     C                   Z-ADD     WAA2          ANN(J)
162900000000     C                   Z-ADD     WMM           MES(J)
163000000000     C                   ADD       1             WMM
163100000000E   2C                   ENDDO
163200000000     C***
163300000000     C* SPARM = N° MESI SELEZIONATI
163400000000     C***
163500000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
163600000000     C*
163700000000     C* IL RESTO DELLA SCHIERA A ZERO
163800000000DO  2C     J             DOWLT     12
163900000000     C                   ADD       1             J
164000000000     C                   Z-ADD     *ZEROS        ANN(J)
164100000000     C                   Z-ADD     *ZEROS        MES(J)
164200000000E   2C                   ENDDO
164300000000E   1C                   ENDIF
164400000000     C*
164500000000     C                   ENDSR
164600000000     C*------------------------------------------------------------------------*
164700000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
164800000000     C*------------------------------------------------------------------------*
164900000000     C     DECTES        BEGSR
165000000000     C*--------------------
165100000000     C* DATA E ORA
165200000000     C*--------------------
165300000000     C                   Z-ADD     UDATE         DATA              6 0
165400000000     C                   TIME                    ORA               6 0
165500950602     C*--------------------
165600950602     C* DATI IN STATISTICA
165700950602     C*--------------------
165800950602IF  1C     PARIOE        IFEQ      'I'
165900950602     C                   MOVEL     DRI(5)        PTCIOE
166000950602E   1C                   ENDIF
166100950602IF  1C     PARIOE        IFEQ      'E'
166200950602     C                   MOVEL     DRI(6)        PTCIOE
166300950602E   1C                   ENDIF
166400950602IF  1C     PARIOE        IFEQ      *BLANKS
166500950602     C                   MOVEL     DRI(7)        PTCIOE
166600950602E   1C                   ENDIF
166700000829     C*--------------------
166800000829     C* DATI CLIENTE / UNIFICANTE
166900000829     C*--------------------
167000000829IF  1C     PARSCE        IFEQ      'C'
167100000829     C                   MOVEL     DRI(29)       PTDSCE
167200000829X   1C                   ELSE
167300000829     C                   MOVEL     DRI(30)       PTDSCE
167400000829E   1C                   ENDIF
167500000000     C*--------------------
167600000000     C* DECODIFICA MESI
167700000000     C*--------------------
167800000000IF  1C     MES(1)        IFGT      *ZEROS
167900000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ
168000000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
168100990609     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
168200000000E   1C                   ENDIF
168300000000     C*
168400000000IF  1C     MES(2)        IFGT      *ZEROS
168500000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ
168600000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
168700990609     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
168800000000E   1C                   ENDIF
168900000000     C*
169000000000IF  1C     MES(3)        IFGT      *ZEROS
169100000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ
169200000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
169300990609     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
169400000000E   1C                   ENDIF
169500000000     C*
169600000000IF  1C     MES(4)        IFGT      *ZEROS
169700000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ
169800000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
169900990609     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
170000000000E   1C                   ENDIF
170100000000     C*
170200000000IF  1C     MES(5)        IFGT      *ZEROS
170300000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ
170400000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
170500990609     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
170600000000E   1C                   ENDIF
170700000000     C*
170800000000IF  1C     MES(6)        IFGT      *ZEROS
170900000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ
171000000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
171100990609     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
171200000000E   1C                   ENDIF
171300000000     C*
171400000000IF  1C     MES(7)        IFGT      *ZEROS
171500000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ
171600000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
171700990609     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
171800000000E   1C                   ENDIF
171900000000     C*
172000000000IF  1C     MES(8)        IFGT      *ZEROS
172100000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ
172200000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
172300990609     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
172400000000E   1C                   ENDIF
172500000000     C*
172600000000IF  1C     MES(9)        IFGT      *ZEROS
172700000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ
172800000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
172900990609     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
173000000000E   1C                   ENDIF
173100000000     C*
173200000000IF  1C     MES(10)       IFGT      *ZEROS
173300000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ
173400000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
173500990609     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
173600000000E   1C                   ENDIF
173700000000     C*
173800000000IF  1C     MES(11)       IFGT      *ZEROS
173900000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ
174000000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
174100990609     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
174200000000E   1C                   ENDIF
174300000000     C*
174400000000IF  1C     MES(12)       IFGT      *ZEROS
174500000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ
174600000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
174700990609     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
174800000000E   1C                   ENDIF
174900000000     C*
175000000000     C                   ENDSR
175100000000     C*------------------------------------------------------------------------*
175200000000     C*?CARTAB - CARICA I DATI IN TABELLE
175300000000     C*------------------------------------------------------------------------*
175400000000     C     CARTAB        BEGSR
175500000000     C*--------------------
175600000000     C* FILIALI/AREE/DIVISIONI
175700000000     C*--------------------
175800000000     C                   Z-ADD     *ZEROS        KORFIL
175900000000     C     KEYORG        SETLL     AZORG01L
176000000000     C                   READ      AZORG01L                               99
176100000000     C                   Z-ADD     *ZEROS        F
176200951219DO  1C     *IN99         DOWEQ     *OFF
176300000000IF  2C     ORGFVA        IFEQ      *BLANKS
176400000000     C                   ADD       1             F
176500000000     C* F I L I A L I
176600000000     C                   Z-ADD     ORGFIL        FIL(F)
176700000000     C                   MOVEL     ORGDES        DFIL(F)
176800000707     C                   MOVEL     ORGDE3        OG143
176900020627     C                   MOVEL     §OGNTW        POS(F)
177000000000     C* A R E E
177100020906     C                   Z-ADD     1             KTBKUT
177200000000     C                   MOVEL     '05'          KTBCOD
177300000000     C                   MOVEL     *BLANKS       KTBKEY
177400000000     C                   MOVEL     ORGCAR        KTBKEY
177500000000     C     KEYTAB        CHAIN     TABEL00F                           99
177600000000IF  3C     *IN99         IFEQ      *OFF
177700000000     C                   Z-ADD     ORGCAR        ARE(F)
177800000000     C                   MOVEL     TBLUNI        DARE(F)
177900000000E   3C                   ENDIF
178000000000     C* D I V I S I O N I
178100020906     C                   Z-ADD     1             KTBKUT
178200000000     C                   MOVEL     '17'          KTBCOD
178300000000     C                   MOVEL     *BLANKS       KTBKEY
178400000000     C                   MOVEL     ORGFL3        KTBKEY
178500000000     C     KEYTAB        CHAIN     TABEL00F                           99
178600000000IF  3C     *IN99         IFEQ      *OFF
178700000000     C                   MOVEL     ORGFL3        DIV(F)
178800000000     C                   MOVEL     TBLUNI        DDIV(F)
178900000000E   3C                   ENDIF
179000000000E   2C                   ENDIF
179100000000     C                   READ      AZORG01L                               99
179200000000E   1C                   ENDDO
179300000000     C*--------------------
179400000000     C* COMMERCIALI
179500000000     C*--------------------
179600140612     C************       Z-ADD     1             KTBKUT
179700140612     C************       MOVEL     '01'          KTBCOD
179800140612     C***  KEYTA2        SETLL     TABEL00F
179900140612     C***  KEYTA2        READE     TABEL00F                               99
180000140612     C************       Z-ADD     *ZEROS        C
180100140612DO  1C***  *IN99         DOWEQ     *OFF
180200140612     C************       ADD       1             C
180300140612     C************       MOVEL     TBLUNI        DS01
180400140612     C************       MOVEL     TBLKEY        AGE(C)
180500140612     C************       MOVEL     §01AGE        DAGE(C)
180600140612IF  3C***  PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
180700140612     C************       Z-ADD     §01PCF        CAGE(C)
180800140612X   3C************       ELSE                                                   *% LAVORO ESTERO
180900140612     C************       Z-ADD     §01PCE        CAGE(C)
181000140612E   3C************       ENDIF
181100140612     C***  KEYTA2        READE     TABEL00F                               99
181200140612E   1C************       ENDDO
181300140612      *------
181400140612     c                   open      azCMM01l
181500140612     C                   Z-ADD     *ZEROS        C
181600140612     C     *loval        SETLL     azCMM01l
181700140612     C                   read      azCMM01l
181800140612DO  1C                   dow       not %Eof(azCMM01l)
181900140612     C                   ADD       1             C
182000140612     C                   z-add     cmmCOD        AGE(C)
182100140612     C                   MOVEL     cmmDES        DAGE(C)
182200140612IF  3C     PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
182300140612     C                   Z-ADD     cmmPLI        CAGE(C)
182400140612X   3C                   ELSE                                                   *% LAVORO ESTERO
182500140612     C                   Z-ADD     cmmPLE        CAGE(C)
182600140612E   3C                   ENDIF
182700140612     C                   read      azCMM01l
182800140612E   1C                   ENDDO
182900140612     c                   close     azCMM01l
183000980521     C*---
183100980521     C* QUALITA' CLIENTE
183200980521     C*---
183300980521     C                   Z-ADD     *ZEROS        C
183400020906     C                   Z-ADD     1             KTBKUT
183500980521     C                   MOVEL     'IC'          KTBCOD
183600980521     C     KEYTA2        SETLL     TABEL00F
183700980521     C     KEYTA2        READE     TABEL00F                               99
183800980521DO  1C     *IN99         DOWEQ     *OFF
183900980521IF  2C     TBLFLG        IFEQ      *BLANKS
184000980521     C                   ADD       1             C
184100980521     C                   MOVEL     TBLKEY        ABC(C)
184200980521E   2C                   ENDIF
184300980521     C     KEYTA2        READE     TABEL00F                               99
184400980521E   1C                   ENDDO
184500000000     C*
184600000000     C                   ENDSR
184700000000     C*------------------------------------------------------------------------*
184800000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
184900000000     C*------------------------------------------------------------------------*
185000000000     C     STAPAR        BEGSR
185100000000     C*--------------------
185200000000     C* NOME PROGRAMMA
185300000000     C*--------------------
185400020116     C                   MOVEL     'TISE94R'     PTCPGM
185500000000     C*--------------------
185600000000     C* DESCRIZIONE STATISTICA
185700000000     C*--------------------
185800000000     C                   MOVEL     DST(1)        PTCDST
185900000000     C*--------------------
186000000000     C* DATE LANCIO
186100000000     C*--------------------
186200000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
186300000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
186400000000     C*--------------------
186500000000     C* SELEZIONI PER TIPO SERVIZIO
186600000000     C*--------------------
186700000000IF  1C     PARTSP        IFEQ      *BLANKS
186800000000     C                   MOVEL     DRI(1)        PADTSP
186900000000X   1C                   ELSE
187000000000     C                   MOVEL     PARTSP        PACTSP
187100020906     C                   Z-ADD     1             KTBKUT
187200000000     C                   MOVEL     '5E'          KTBCOD
187300000000     C                   MOVEL     *BLANKS       KTBKEY
187400000000     C                   MOVEL     PARTSP        KTBKEY
187500000000     C     KEYTAB        CHAIN     TABEL00F                           99
187600000000IF  2C     *IN99         IFEQ      *OFF
187700090610     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
187800090610     C                   MOVEL     §5EDES        PADTSP                         *TIPO SERVIZIO
187900000000E   2C                   ENDIF
188000000000E   1C                   ENDIF
188100950208     C*--------------------
188200950208     C* SELEZIONI PER DELTA ITALIA/ESTERO
188300950208     C*--------------------
188400950208     C                   MOVEL     PARIOE        PACIOE
188500950208IF  1C     PARIOE        IFEQ      'I'
188600950208     C                   MOVEL     DRI(5)        PADIOE
188700950208E   1C                   ENDIF
188800950208IF  1C     PARIOE        IFEQ      'E'
188900950208     C                   MOVEL     DRI(6)        PADIOE
189000950208E   1C                   ENDIF
189100950208IF  1C     PARIOE        IFEQ      *BLANKS
189200950208     C                   MOVEL     DRI(7)        PADIOE
189300950208E   1C                   ENDIF
189400951018     C*--------------------
189500970822     C* SELEZIONI PER QUALITA' CLIENTE
189600951018     C*--------------------
189700951018     C                   MOVEL     PARCLV        PACCLV
189800010618     C                   MOVEL     PARCL1        PACCL1
189900010618     C                   MOVEL     PARCL2        PACCL2
190000020220     C                   MOVEL     PARCL3        PACCL3
190100020220     C                   MOVEL     PARCL4        PACCL4
190200951018     C                   MOVEL     *BLANKS       PADCLV
190300951018IF  1C     PARCLV        IFEQ      *BLANKS
190400010618     C     PARCL1        ANDEQ     *BLANKS
190500010618     C     PARCL2        ANDEQ     *BLANKS
190600020220     C     PARCL3        ANDEQ     *BLANKS
190700020220     C     PARCL4        ANDEQ     *BLANKS
190800951018     C                   MOVEL     DRI(1)        PADCLV
190900951018E   1C                   ENDIF
191000010618     C*--------------------
191100010618     C* TIPO STAMPA SINTETICA S/N
191200010618     C*--------------------
191300010618     C                   MOVEL     PARSIN        PACSIN
191400980528     C*--------------------
191500980528     C* % SCOSTAMENTO DELTA TOTALE
191600980528     C*--------------------
191700980528     C                   Z-ADD     PARPDD        PACPDD
191800980528     C                   Z-ADD     PARPDA        PACPDA
191900980528     C*--------------------
192000980528     C* % SCOSTAMENTO DELTA PER FASCE
192100980528     C*--------------------
192200980528     C                   Z-ADD     PARSPD        PACSPD
192300020702     C*--------------------
192400020702     C* % SCOSTAMENTO FATTURATO ANNO PRECEDENTE
192500020702     C*--------------------
192600020702     C                   Z-ADD     PARDFD        PACDFD
192700020702     C                   Z-ADD     PARDFA        PACDFA
192800020702     C                   Z-ADD     PARDFDP       PACDFDP
192900020702     C                   Z-ADD     PARDFAP       PACDFAP
193000951018     C*--------------------
193100000000     C* SELEZIONI DELTA FILIALE
193200000000     C*--------------------
193300000000IF  1C     PARFIC        IFGT      *ZEROS                                       *FILIALE
193400000000     C                   Z-ADD     PARFIC        DEPFIL
193500000000     C                   EXSR      DECFIL
193600000000     C                   Z-ADD     *ZEROS        DEPFIL
193700000000     C                   MOVEL     PARFIC        PACFIC
193800000000     C                   MOVEL     DESFIL        PADFIC
193900000000X   1C                   ELSE
194000000000     C                   MOVEL     *BLANKS       PACFIC
194100000000E   1C                   ENDIF
194200000000     C*
194300000000IF  1C     PARARC        IFGT      *ZEROS                                       *AREA
194400000000     C                   Z-ADD     PARARC        DEPARE
194500000000     C                   EXSR      DECARE
194600000000     C                   Z-ADD     *ZEROS        DEPARE
194700000000     C                   MOVEL     PARARC        PACARC
194800000000     C                   MOVEL     DESARE        PADARC
194900000000X   1C                   ELSE
195000000000     C                   MOVEL     *BLANKS       PACARC
195100000000E   1C                   ENDIF
195200000000     C*
195300000000IF  1C     PARDIC        IFNE      *BLANKS                                      *DIVISIONE
195400000000     C                   MOVEL     PARDIC        DEPDIV
195500000000     C                   EXSR      DECDIV
195600000000     C                   MOVEL     *BLANKS       DEPDIV
195700000000     C                   MOVEL     PARDIC        PACDIC
195800000000     C                   MOVEL     DESDIV        PADDIC
195900000000E   1C                   ENDIF
196000000000     C*--------------------
196100000000     C* SELEZIONI DELTA COMMERCIALE
196200000000     C*--------------------
196300000000IF  1C     PARCOF        IFGT      *ZEROS                                       *COMMERCIALE
196400000000     C                   Z-ADD     PARCOF        DEPAGE
196500000000     C                   EXSR      DECAGE
196600000000     C                   Z-ADD     *ZEROS        DEPAGE
196700000000     C                   MOVEL     PARCOF        PACCOF
196800000000     C                   MOVEL     DESAGE        PADCOF
196900000000X   1C                   ELSE
197000000000     C                   MOVEL     *BLANKS       PACCOF
197100000000E   1C                   ENDIF
197200000000     C*
197300000000IF  1C     PARDIF        IFNE      *BLANKS                                      *DIVISIONE
197400000000     C                   MOVEL     PARDIF        DEPDIV
197500000000     C                   EXSR      DECDIV
197600000000     C                   MOVEL     *BLANKS       DEPDIV
197700000000     C                   MOVEL     PARDIF        PACDIF
197800000000     C                   MOVEL     DESDIV        PADDIF
197900000000E   1C                   ENDIF
198000000707     C*--------------------
198100000707     C* DATI POSTE
198200000707     C*--------------------
198300000707IF  1C     PARPOS        IFEQ      'S'
198400000707     C                   MOVEL     'SI'          PADPOS
198500000707X   1C                   ELSE
198600000707     C                   MOVEL     'NO'          PADPOS
198700000707E   1C                   ENDIF
198800000825     C*--------------------
198900000825     C* DATI CLIENTE / UNIFICANTE
199000000825     C*--------------------
199100000825     C                   MOVEL     PARSCE        PACSCE
199200000825IF  1C     PARSCE        IFEQ      'C'
199300000825     C                   MOVEL     DRI(29)       PADSCE
199400000825X   1C                   ELSE
199500000825     C                   MOVEL     DRI(30)       PADSCE
199600000825E   1C                   ENDIF
199700000000     C*
199800000000     C* S T A M P A
199900000000     C*
200000020116     C                   WRITE     SE94PT                                       *INTESTAZIONE
200100020116     C                   WRITE     SE94PA                                       *PARAMETRI
200200000000     C*
200300000000     C* ACCENDO INDICATORE DI OF PER STAMPARE LA TESTATA ALLA 1^ ROTTURA CLIENTE
200400000000     C                   SETON                                        01
200500000000     C*
200600000000     C                   ENDSR
200700030916     C*------------------------------------------------------------------------*
200800030916     C*?CANWF - CANCELLA DATI WFILE, SE RICHIESTO
200900030916     C*------------------------------------------------------------------------*
201000030916     C     CANWF         BEGSR
201100030916     C*
201200030916     C     PARWPC        IFEQ      'S'                                          *SE RICHIESTO
201300030916     c                   MOVEL     PARIOE        KE94IOE                        *ITALIA/ESTERO
201400030916     C     KEYSE94       SETLL     WFTISE94L
201500030916     C     KEYSE94       READE     WFTISE94L                              99
201600030916     C                   DOW       NOT *IN99
201700030916     C                   DELETE    WFTISE940
201800030916     C     KEYSE94       READE     WFTISE94L                              99
201900030916     C                   ENDDO
202000030916     C                   ENDIF
202100030916     C*
202200030916     C                   ENDSR
202300000000     C*------------------------------------------------------------------------*
202400000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
202500000000     C*------------------------------------------------------------------------*
202600000000     C     *INZSR        BEGSR
202700000000     C*
202800000000     C* PARAMETRI IN INPUT AL PGM
202900000000     C     *ENTRY        PLIST
203000000000     C                   PARM                    KPJBA
203100000000     C                   MOVEL     KPJBU         PARAM
203200000000     C*
203300000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
203400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
203500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
203600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
203700000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
203800100202     C     *LIKE         DEFINE    WFDDIC        KWDDIV                         *WFDEa02L
203900000000     C     *LIKE         DEFINE    WFDRGD        KWDRGD
204000000000     C     *LIKE         DEFINE    WFDRGF        KWDRGF
204100000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
204200000825     C     *LIKE         DEFINE    WFDCLI        KWDCLI
204300000000     C     *LIKE         DEFINE    WFDANN        KWDANN
204400000000     C     *LIKE         DEFINE    WFDMES        KWDMES
204500030916     C     *LIKE         DEFINE    SE94IOE       KE94IOE
204600000000     C*
204700000000     C* DEFINISCE LE VARIABILI DI DEPOSITO
204800000000     C     *LIKE         DEFINE    WFDDIC        DEPDIV
204900000000     C     *LIKE         DEFINE    WFDARE        DEPARE
205000000000     C     *LIKE         DEFINE    WFDRGF        DEPRGF
205100000000     C     *LIKE         DEFINE    WFDAGE        DEPAGE
205200000000     C     *LIKE         DEFINE    WFDFIL        DEPFIL
205300000825     C     *LIKE         DEFINE    WFDCLI        DEPCLI
205400000000     C*
205500000000     C* DEFINISCE LE DESCRIZIONI DI DEPOSITO
205600140612     C**** *LIKE         DEFINE    §01PCF        DESPCF
205700140612     C     *LIKE         DEFINE    cmmPLI        DESPCF
205800000000     C     *LIKE         DEFINE    WFDRGD        DESRGF
205900000000     C     *LIKE         DEFINE    ORGDES        DESSIL
206000000000     C*
206100000000     C* DEFINISCE INDICI DI SCHIERA
206200000000     C                   Z-ADD     *ZEROS        C                 4 0          *COMMERCIALI
206300000000     C                   Z-ADD     *ZEROS        D                 4 0          *PER DECODIFICHE
206400000000     C                   Z-ADD     *ZEROS        F                 4 0          *VOCI ORGANIGRAMMA
206500000000     C                   Z-ADD     *ZEROS        J                 4 0          *GENERICO
206600000000     C                   Z-ADD     *ZEROS        X                 2 0          *PER I MESI
206700000000     C                   Z-ADD     *ZEROS        MM                2 0          *PER I MESI
206800000000     C                   Z-ADD     *ZEROS        IN                2 0          *PER I MESI
206900000000     C                   Z-ADD     *ZEROS        SPARM             4 0          *N° MESI SELEZIONATI
207000000000     C*
207100000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
207200000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
207300000000     C                   MOVEL     '0'           PASDIV            1            MEMOR SALDI DIVISION
207400000000     C                   MOVEL     '0'           PASRGF            1            MEMOR SALDI FILIALE
207500000000     C                   MOVEL     '0'           PASAGE            1            MEMOR SALDI COMM.LE
207600000000     C                   MOVEL     ' '           TIPANN            1            *TIPO ANNO (P/C)
207700951227     C                   MOVEL     '0'           FLAKSC            1            FLAG CONT CLI CORR
207800951227     C                   MOVEL     '0'           FL2KSC            1            FLAG CONT CLI PREC
207900960710     C*--------------------
208000960710     C* CHIAVI DI LETTURA
208100960710     C*--------------------
208200960710     C* LETTURA TABEL00F - COMPLETA
208300000000     C     KEYTAB        KLIST
208400000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
208500000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
208600000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
208700960710     C* LETTURA TABEL00F - PARZIALE
208800000000     C     KEYTA2        KLIST
208900000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
209000000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
209100960710     C* LETTURA AZORG01L - COMPLETA
209200000000     C     KEYORG        KLIST
209300000000     C                   KFLD                    KORFIL                         *FILIALE
209400100202     C* LETTURA WFDEa02L - PARZIALE
209500000000     C     KEYDEL        KLIST
209600000000     C                   KFLD                    KWDDIV                         *DIVISIONE
209700000000     C                   KFLD                    KWDRGD                         *DESCRIZ RAGGRUP.TO
209800000000     C                   KFLD                    KWDRGF                         *RAGGRUP.TO FILIALE
209900000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
210000000825     C                   KFLD                    KWDCLI                         *CLIENTE
210100000000     C                   KFLD                    KWDANN                         *ANNO
210200000000     C                   KFLD                    KWDMES                         *MESE
210300030916     C* LETTURA WFTISE94L - PARZIALE
210400030916     C     KEYSE94       KLIST
210500030916     C                   KFLD                    KE94IOE                        *ITALIA/ESTERO
210600000000     C*
210700000000     C* MEMORIZZA I DATI IN TABELLE
210800000000     C                   EXSR      CARTAB
210900000000     C*
211000000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
211100000000     C                   EXSR      CARMES
211200000000     C*
211300000000     C* DECODIFICA INTESTAZIONE
211400000000     C                   EXSR      DECTES
211500030916     C*
211600030916     C* CANCELLA DATI WFILE, SE RICHIESTO
211700030916     C                   EXSR      CANWF
211800000000     C*
211900000000     C                   ENDSR
212000000000     O*------------------------------------------------------------------------*
212100000000**   DMM - DECODIFICHE MESI DELL'ANNO
212200000000GEN  1
212300000000FEB  2
212400000000MAR  3
212500000000APR  4
212600000000MAG  5
212700000000GIU  6
212800000000LUG  7
212900000000AGO  8
213000000000SET  9
213100000000OTT  10
213200000000NOV  11
213300000000DIC  12
213400000000**   DST - DECODIFICHE STATISTICHE
213500000000COMMERCIALE              1
213600000000                         2
213700000000                         3
213800000000                         4
213900000000                         5
214000000000**   DRI - DECODIFICHE RIGA DI STAMPA
214100000000TUTTI                    1
214200000000TUTTE                    2
2143000000003
2144009502084
214500950208ITALIA                   5
214600950208ESTERO                   6
214700950602ITALIA+ESTERO            7
2148000000008
2149000000009
21500000000010
21510000000011
215200020220TOTALE                  12
215300000000CLIENTI                 13
215400000000SPEDIZ                  14
215500000000RICAVO                  15
215600020220DELTA                   16
215700020221TOTALE                  17
21580000000018
21590000000019
21600000000020
216100000000 C O M M E R C I A L E  21
21620000000022
21630000000023
216400000000   D I V I S I O N E    24
21650000000025
21660000082526
21670000082527
21680000082528
216900000829DATI CLIENTE             29
217000000829DATI CLIENTE UNIFICANTE  30
217100020319**   DNTW - DECODIFICHE NETWORK
217200030312***                       1
217300020319EEX                       2
217400020319DPD                       3
217500020319FED                       4
217600020319EUP                       5
