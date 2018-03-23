000100970612     H DECEDIT('0,') DATEDIT(*DMY.)
000200970612     H* FNLSI1R1 *---------------------------------------------------*
000300970612     H*                                                              *
000400970612     H*       - CHIUSURA BOLLA IN PARTENZA CON PRATICA IDD      -    *
000500970612     H*--------------------------------------------------------------*
000600060811     FAZORG01L  IF   e           k DISK
000700060811     F*--------
000800970616     FFNBLT01L  UF   E           K DISK
000900160408     F*--------
001000160408     FFNBLP01L  iF   E           K DISK
001100160408     Ftabel00f  iF   E           K DISK
001200970613     F*--------
001300970612     FFNLSI1D   CF   E             WORKSTN
001400970612     D*--------------------------------------------------------------*
001500160408     d Kcod            s                   like(tblcod) inz('3A')
001600160408     d Kkey            s                   like(tblkey)
001700970612     D* DS
001800970612     D*--------------------------------------------------------------*
001900970612     D WLBDA8          DS
002000970612     D  G02DAT                 1      8  0
002100970612     D  G02INV                 9     16  0
002200970612     D  G02ERR                17     17
002300970612     D  G02TGI                18     22  0
002400970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002500060811     D OG143         E DS
002600970612     D CNCR80        E DS
002700970612     D KPJBA         E DS
002800970612     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
002900970613     D EDDS2A        E DS                  EXTNAME(EDIDS2A)
003000160408
003100160408     D ds3a          E DS
003200970612     D                SDS
003300970612     D  NOMPGM                 1     10
003400970612     C*--------------------------------------------------------------*
003500970612     C*  C I C L O     P R I N C I P A L E
003600970612     C*--------------------------------------------------------------*
003700970613     C     *ENTRY        PLIST
003800970613     C                   PARM                    KPJBA
003900970613     C                   MOVEL     KPJBU         FNLSI1
004000970612     C*  Inizializzo terza videata
004100970612     C                   EXSR      INZ03
004200970612     C*  Gestione della prima videata
004300970612     C     WFINE         DOWEQ     'N'
004400970612     C     WTPVID        CASEQ     '3'           GESD03
004500970612     C                   END
004600970612     C                   END
004700970612     C*  Fine pgm
004800970613     C                   MOVEL     FNLSI1        KPJBU
004900970613     C                   SETON                                        RT
005000970612     C*--------------------------------------------------------------*
005100970612     C* INZ03: Impostazione della TERZA videata
005200970612     C*--------------------------------------------------------------*
005300970612     C     INZ03         BEGSR
005400970612     C*
005500970613     C                   SETOFF                                       010203
005600970613     C                   SETOFF                                       040528
005700970613     C                   MOVEL     'N'           WFINE             1
005800970613     C                   MOVEL     '3'           WTPVID            1
005900970612     C                   MOVEL     NOMPGM        V3CPGM
006000970612     C                   MOVEL     DI1KSC        V3CKSC
006100970612     C                   MOVEL     DI1DEC        V3DKSC
006200060811      **? e se  si tratta di DPD (eccezione .......)
006300060811     c                   movel     v3dksc        trelnp            3 0
006400060811     C                   Z-ADD     treLNP        ORGFIL
006500060811     C     ORGFIL        CHAIN     AZORG01L
006600060811      **? e se  si tratta di DPD (eccezione .......)
006700060811     c                   clear                   og143
006800060811     c                   if        %Found(AZORG01L)
006900060811     c                   movel     orgde3        og143
007000060811     c                   end
007100060811
007200060811     c                   if        §OGNTW ='DPD'
007300060811     C                   MOVEL     'N'           V3CINV
007400060811     C                   ELSE
007500970613     C                   MOVEL     'S'           V3CINV
007600060811     C                   END
007700980203     C                   Z-ADD     DI1NCL        V3CNCL
007800980203     C                   Z-ADD     DI1AAS        V3CAAS
007900970612     C                   Z-ADD     DI1LNP        V3CLNP
008000970612     C                   Z-ADD     DI1NRS        V3CNRS
008100970612     C                   Z-ADD     DI1NSP        V3CNSP
008200160408     c                   clear                   v3cass
008300160408     c     kblp          chain     fnblp01l
008400160408     c                   if        %found(fnblp01l)
008500160408     c                   eval      kkey=blpcbo
008600160408     c     ktab          chain     tabel00f
008700160408     c                   if        %found(tabel00f)
008800160408     c                   movel     tbluni        ds3a
008900160408     c                   if        %subst(§3atb1:1:1)='A'
009000160408     c                   movel     'ASSEGNATO'   v3cass
009100160408     c                   endif
009200160408     c                   endif
009300160408     c                   endif
009400970612     C                   Z-ADD     0             G02DAT
009500970612     C                   Z-ADD     DI1DSP        G02INV
009600970612     C                   MOVE      '3'           G02ERR
009700970612     C                   CALL      'XSRDA8'
009800970612     C                   PARM                    WLBDA8
009900970612     C                   Z-ADD     G02DAT        V3CDSP
010000980203     C                   MOVEL     DI1STS        V3CSTS
010100980203     C                   MOVEL     DI1CEV        V3CCEV
010200970612     C*
010300970612     C                   ENDSR
010400970613     C*--------------------------------------------------------------*
010500970613     C* GESD03: Gestione TERZA videata
010600970613     C*--------------------------------------------------------------*
010700970613     C     GESD03        BEGSR
010800970613     C*
010900970613     C* Emetto videata
011000970613     C                   EXFMT     LSI1D03
011100970613     C                   SETOFF                                       05
011200970613     C* Fine lavoro
011300970613     C     *INKC         IFEQ      '1'
011400970613     C                   MOVEL     'S'           WFINE             1
011500970613     C                   MOVEL     '03'          DI1CMD
011600970613     C                   GOTO      FINVD3
011700970613     C                   END
011800970613     C* Ritorno
011900970613     C     *INKL         IFEQ      '1'
012000970613     C                   MOVEL     'S'           WFINE
012100970613     C                   MOVEL     '12'          DI1CMD
012200970613     C                   GOTO      FINVD3
012300970613     C                   END
012400970613     C*
012500970613     C* Per errore vado a fine
012600970613     C   28              GOTO      FINVD3
012700970613     C* Per mancanza provvisoria parziale controllo se devo richied.
012800970613     C* ulteriore conferma
012900980203     C     DI1FRZ        IFEQ      'S'
013000970613     C     *INKA         IFEQ      '0'
013100970613     C* Se ho già inviato evento al partner lo segnalo
013200970613     C     *INKF         IFEQ      '1'
013300980204     C                   Z-ADD     DI1AAS        KAAS
013400970613     C* ..  e chiedo ulteriore conferma
013500980204     C     DI1FOR        IFEQ      'F'
013600970613     C                   SETON                                        05
013700970613     C                   ELSE
013800970613     C* ..  altrimenti effettuo aggiornamento
013900970613     C                   MOVEL     'S'           WFINE
014000970613     C                   MOVEL     '06'          DI1CMD
014100970613     C                   END
014200970613     C                   END
014300970613     C                   ELSE
014400970613     C* Se ho già dato ulteriore conferma esco x aggiornamento
014500970613     C                   MOVEL     'S'           WFINE
014600970617     C                   MOVEL     '06'          DI1CMD
014700970619     C                   MOVEL     'S'           DI1FOR
014800970617     C                   MOVEL     V3CSTS        DI1STS
014900970617     C                   MOVEL     V3CCEV        DI1CEV
015000970613     C                   END
015100970613     C* Per opzione 2-4  se conferma F06 esco x aggiornamento
015200970613     C                   ELSE
015300970613     C     *INKF         IFEQ      '1'
015400970613     C                   MOVEL     'S'           WFINE
015500970613     C                   MOVEL     '06'          DI1CMD
015600970613     C                   MOVEL     V3CSTS        DI1STS
015700970613     C                   MOVEL     V3CCEV        DI1CEV
015800970613     C                   END
015900980203     C                   END
016000970616     C*
016100970616     C* Altrimenti aggiorno tutti i segnacolli
016200160308     C     *INKF         IFEQ      '1'
016300980203     C     DI1BLT        IFEQ      'S'
016400980205     C     DI1TOT        ANDEQ     'S'
016500980203     C     DI1CCA        OREQ      'S'
016600980205     C     DI1TOT        ANDEQ     'S'
016700970616     C                   EXSR      AGGBLT
016800970616     C                   END
016900160308     C                   END
017000970613     C*
017100970613     C     FINVD3        ENDSR
017200970616     C*--------------------------------------------------------------*
017300970616     C* AGGBLT AGGIORNO segnacolli persi
017400970616     C*--------------------------------------------------------------*
017500970616     C     AGGBLT        BEGSR
017600970616     C*
017700970616     C* Aggiorno segnacolli fleggandoli come persi
017800970617     C                   Z-ADD     DI1AAS        KAAS
017900970617     C                   Z-ADD     DI1LNP        KLNP
018000970617     C                   Z-ADD     DI1NRS        KNRS
018100970617     C                   Z-ADD     DI1NSP        KNSP
018200970616     C     KBLT          CHAIN     FNBLT01L                           32
018300970616     C     *IN32         DOWEQ     '0'
018400980203     C     DI1BLT        IFEQ      'S'
018500970616     C                   Z-ADD     OGGI          BLTDFV
018600970616     C                   Z-ADD     99999         BLTNFV
018700970616     C                   Z-ADD     83            BLTNPP
018800970616     C                   Z-ADD     OGGI          BLTDCM
018900970616     C                   Z-ADD     OGGI          BLTDTR
019000970616     C                   MOVEL     'N'           BLTFTR
019100980203     C                   END
019200980203     C     DI1CCA        IFEQ      'S'
019300970616     C                   MOVEL     '7'           BLTFL1
019400970619     C                   END
019500970616     C                   UPDATE    FNBLT000
019600970616     C* Lettura successiva
019700970616     C     KBLT          READE     FNBLT01L                               32
019800970616     C                   END
019900970616     C*
020000970616     C                   ENDSR
020100970613     C*--------------------------------------------------------------*
020200970613     C* *INZSR: Operazioni iniziali
020300970613     C*--------------------------------------------------------------*
020400970613     C     *INZSR        BEGSR
020500970613     C*
020600970613     C* Richiamo xparut
020700970613     C                   Z-ADD     1             CODUT
020800970613     C                   CALL      'X§PARUT'
020900970613     C                   PARM                    UTEDSE
021000970613     C                   MOVEL     REC80         CNCR80
021100970613     C                   MOVEL     RAGUT         RSUT
021200970613      *  Definizione chiavi
021300970616     C     KBLT          KLIST
021400970616     C                   KFLD                    KAAS
021500970616     C                   KFLD                    KLNP
021600970616     C                   KFLD                    KNRS
021700970616     C                   KFLD                    KNSP
021800160408     C     KBLp          KLIST
021900160408     C                   KFLD                    v3cAAS
022000160408     C                   KFLD                    v3cLNP
022100160408     C                   KFLD                    v3cNRS
022200160408     C                   KFLD                    v3cNSP
022300160408     C     Ktab          KLIST
022400160408     C                   KFLD                    codut
022500160408     C                   KFLD                    kcod
022600160408     C                   KFLD                    kkey
022700970613      *  Definizione variabili
022800970616     C     *LIKE         DEFINE    BLTAAS        KAAS
022900980204     C     *LIKE         DEFINE    BLTLNP        KLNP
023000980204     C     *LIKE         DEFINE    BLTNRS        KNRS
023100980204     C     *LIKE         DEFINE    BLTNSP        KNSP
023200970616      *
023300970616      *  Reperisco la data del giorno
023400970616     C                   TIME                    WHHDAT           14 0
023500970616     C                   MOVE      WHHDAT        G02DAT
023600970616     C                   MOVEL     *BLANK        G02ERR
023700970616     C                   CALL      'XSRDA8'
023800970616     C                   PARM                    WLBDA8
023900970616     C                   Z-ADD     G02INV        OGGI              8 0
024000970613     C*
024100970613     C                   ENDSR
