000100970612     H DECEDIT('0,') DATEDIT(*DMY.)
000200970612     H* FNLSI1R1 *---------------------------------------------------*
000300970612     H*                                                              *
000400970612     H*       - CHIUSURA BOLLA IN PARTENZA CON PRATICA IDD      -    *
000500970612     H*--------------------------------------------------------------*
000600060811     FAZORG01L  IF   e           k DISK
000700060811     F*--------
000800970616     FFNBLT01L  UF   E           K DISK
000900970613     F*--------
001000970612     FFNLSI1D   CF   E             WORKSTN
001100970612     D*--------------------------------------------------------------*
001200970612     D* DS
001300970612     D*--------------------------------------------------------------*
001400970612     D WLBDA8          DS
001500970612     D  G02DAT                 1      8  0
001600970612     D  G02INV                 9     16  0
001700970612     D  G02ERR                17     17
001800970612     D  G02TGI                18     22  0
001900970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002000060811     D OG143         E DS
002100970612     D CNCR80        E DS
002200970612     D KPJBA         E DS
002300970612     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
002400970613     D EDDS2A        E DS                  EXTNAME(EDIDS2A)
002500970612     D                SDS
002600970612     D  NOMPGM                 1     10
002700970612     C*--------------------------------------------------------------*
002800970612     C*  C I C L O     P R I N C I P A L E
002900970612     C*--------------------------------------------------------------*
003000970613     C     *ENTRY        PLIST
003100970613     C                   PARM                    KPJBA
003200970613     C                   MOVEL     KPJBU         FNLSI1
003300970612     C*  Inizializzo terza videata
003400970612     C                   EXSR      INZ03
003500970612     C*  Gestione della prima videata
003600970612     C     WFINE         DOWEQ     'N'
003700970612     C     WTPVID        CASEQ     '3'           GESD03
003800970612     C                   END
003900970612     C                   END
004000970612     C*  Fine pgm
004100970613     C                   MOVEL     FNLSI1        KPJBU
004200970613     C                   SETON                                        RT
004300970612     C*--------------------------------------------------------------*
004400970612     C* INZ03: Impostazione della TERZA videata
004500970612     C*--------------------------------------------------------------*
004600970612     C     INZ03         BEGSR
004700970612     C*
004800970613     C                   SETOFF                                       010203
004900970613     C                   SETOFF                                       040528
005000970613     C                   MOVEL     'N'           WFINE             1
005100970613     C                   MOVEL     '3'           WTPVID            1
005200970612     C                   MOVEL     NOMPGM        V3CPGM
005300970612     C                   MOVEL     DI1KSC        V3CKSC
005400970612     C                   MOVEL     DI1DEC        V3DKSC
005500060811      **? e se  si tratta di DPD (eccezione .......)
005600060811     c                   movel     v3dksc        trelnp            3 0
005700060811     C                   Z-ADD     treLNP        ORGFIL
005800060811     C     ORGFIL        CHAIN     AZORG01L
005900060811      **? e se  si tratta di DPD (eccezione .......)
006000060811     c                   clear                   og143
006100060811     c                   if        %Found(AZORG01L)
006200060811     c                   movel     orgde3        og143
006300060811     c                   end
006400060811
006500060811     c                   if        §OGNTW ='DPD'
006600060811     C                   MOVEL     'N'           V3CINV
006700060811     C                   ELSE
006800970613     C                   MOVEL     'S'           V3CINV
006900060811     C                   END
007000980203     C                   Z-ADD     DI1NCL        V3CNCL
007100980203     C                   Z-ADD     DI1AAS        V3CAAS
007200970612     C                   Z-ADD     DI1LNP        V3CLNP
007300970612     C                   Z-ADD     DI1NRS        V3CNRS
007400970612     C                   Z-ADD     DI1NSP        V3CNSP
007500970612     C                   Z-ADD     0             G02DAT
007600970612     C                   Z-ADD     DI1DSP        G02INV
007700970612     C                   MOVE      '3'           G02ERR
007800970612     C                   CALL      'XSRDA8'
007900970612     C                   PARM                    WLBDA8
008000970612     C                   Z-ADD     G02DAT        V3CDSP
008100980203     C                   MOVEL     DI1STS        V3CSTS
008200980203     C                   MOVEL     DI1CEV        V3CCEV
008300970612     C*
008400970612     C                   ENDSR
008500970613     C*--------------------------------------------------------------*
008600970613     C* GESD03: Gestione TERZA videata
008700970613     C*--------------------------------------------------------------*
008800970613     C     GESD03        BEGSR
008900970613     C*
009000970613     C* Emetto videata
009100970613     C                   EXFMT     LSI1D03
009200970613     C                   SETOFF                                       05
009300970613     C* Fine lavoro
009400970613     C     *INKC         IFEQ      '1'
009500970613     C                   MOVEL     'S'           WFINE             1
009600970613     C                   MOVEL     '03'          DI1CMD
009700970613     C                   GOTO      FINVD3
009800970613     C                   END
009900970613     C* Ritorno
010000970613     C     *INKL         IFEQ      '1'
010100970613     C                   MOVEL     'S'           WFINE
010200970613     C                   MOVEL     '12'          DI1CMD
010300970613     C                   GOTO      FINVD3
010400970613     C                   END
010500970613     C*
010600970613     C* Per errore vado a fine
010700970613     C   28              GOTO      FINVD3
010800970613     C* Per mancanza provvisoria parziale controllo se devo richied.
010900970613     C* ulteriore conferma
011000980203     C     DI1FRZ        IFEQ      'S'
011100970613     C     *INKA         IFEQ      '0'
011200970613     C* Se ho già inviato evento al partner lo segnalo
011300970613     C     *INKF         IFEQ      '1'
011400980204     C                   Z-ADD     DI1AAS        KAAS
011500970613     C* ..  e chiedo ulteriore conferma
011600980204     C     DI1FOR        IFEQ      'F'
011700970613     C                   SETON                                        05
011800970613     C                   ELSE
011900970613     C* ..  altrimenti effettuo aggiornamento
012000970613     C                   MOVEL     'S'           WFINE
012100970613     C                   MOVEL     '06'          DI1CMD
012200970613     C                   END
012300970613     C                   END
012400970613     C                   ELSE
012500970613     C* Se ho già dato ulteriore conferma esco x aggiornamento
012600970613     C                   MOVEL     'S'           WFINE
012700970617     C                   MOVEL     '06'          DI1CMD
012800970619     C                   MOVEL     'S'           DI1FOR
012900970617     C                   MOVEL     V3CSTS        DI1STS
013000970617     C                   MOVEL     V3CCEV        DI1CEV
013100970613     C                   END
013200970613     C* Per opzione 2-4  se conferma F06 esco x aggiornamento
013300970613     C                   ELSE
013400970613     C     *INKF         IFEQ      '1'
013500970613     C                   MOVEL     'S'           WFINE
013600970613     C                   MOVEL     '06'          DI1CMD
013700970613     C                   MOVEL     V3CSTS        DI1STS
013800970613     C                   MOVEL     V3CCEV        DI1CEV
013900970613     C                   END
014000980203     C                   END
014100970616     C*
014200970616     C* Altrimenti aggiorno tutti i segnacolli
014300980203     C     DI1BLT        IFEQ      'S'
014400980205     C     DI1TOT        ANDEQ     'S'
014500980203     C     DI1CCA        OREQ      'S'
014600980205     C     DI1TOT        ANDEQ     'S'
014700970616     C                   EXSR      AGGBLT
014800970616     C                   END
014900970613     C*
015000970613     C     FINVD3        ENDSR
015100970616     C*--------------------------------------------------------------*
015200970616     C* AGGBLT AGGIORNO segnacolli persi
015300970616     C*--------------------------------------------------------------*
015400970616     C     AGGBLT        BEGSR
015500970616     C*
015600970616     C* Aggiorno segnacolli fleggandoli come persi
015700970617     C                   Z-ADD     DI1AAS        KAAS
015800970617     C                   Z-ADD     DI1LNP        KLNP
015900970617     C                   Z-ADD     DI1NRS        KNRS
016000970617     C                   Z-ADD     DI1NSP        KNSP
016100970616     C     KBLT          CHAIN     FNBLT01L                           32
016200970616     C     *IN32         DOWEQ     '0'
016300980203     C     DI1BLT        IFEQ      'S'
016400970616     C                   Z-ADD     OGGI          BLTDFV
016500970616     C                   Z-ADD     99999         BLTNFV
016600970616     C                   Z-ADD     83            BLTNPP
016700970616     C                   Z-ADD     OGGI          BLTDCM
016800970616     C                   Z-ADD     OGGI          BLTDTR
016900970616     C                   MOVEL     'N'           BLTFTR
017000980203     C                   END
017100980203     C     DI1CCA        IFEQ      'S'
017200970616     C                   MOVEL     '7'           BLTFL1
017300970619     C                   END
017400970616     C                   UPDATE    FNBLT000
017500970616     C* Lettura successiva
017600970616     C     KBLT          READE     FNBLT01L                               32
017700970616     C                   END
017800970616     C*
017900970616     C                   ENDSR
018000970613     C*--------------------------------------------------------------*
018100970613     C* *INZSR: Operazioni iniziali
018200970613     C*--------------------------------------------------------------*
018300970613     C     *INZSR        BEGSR
018400970613     C*
018500970613     C* Richiamo xparut
018600970613     C                   Z-ADD     1             CODUT
018700970613     C                   CALL      'X§PARUT'
018800970613     C                   PARM                    UTEDSE
018900970613     C                   MOVEL     REC80         CNCR80
019000970613     C                   MOVEL     RAGUT         RSUT
019100970613      *  Definizione chiavi
019200970616     C     KBLT          KLIST
019300970616     C                   KFLD                    KAAS
019400970616     C                   KFLD                    KLNP
019500970616     C                   KFLD                    KNRS
019600970616     C                   KFLD                    KNSP
019700970613      *  Definizione variabili
019800970616     C     *LIKE         DEFINE    BLTAAS        KAAS
019900980204     C     *LIKE         DEFINE    BLTLNP        KLNP
020000980204     C     *LIKE         DEFINE    BLTNRS        KNRS
020100980204     C     *LIKE         DEFINE    BLTNSP        KNSP
020200970616      *
020300970616      *  Reperisco la data del giorno
020400970616     C                   TIME                    WHHDAT           14 0
020500970616     C                   MOVE      WHHDAT        G02DAT
020600970616     C                   MOVEL     *BLANK        G02ERR
020700970616     C                   CALL      'XSRDA8'
020800970616     C                   PARM                    WLBDA8
020900970616     C                   Z-ADD     G02INV        OGGI              8 0
021000970613     C*
021100970613     C                   ENDSR
