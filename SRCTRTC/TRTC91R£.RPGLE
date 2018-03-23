000100960605      ***********************************************************************
000200960605      *   RICERCA TABELLE PER  E.D.I. ESTERO                                *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500960605     F*---------------------------------------------------------------------*
000600960605     F*  DATA BASE                                                          *
000700960605     F*---------------------------------------------------------------------*
000800960605     FEDTAB01L  IF   E           K DISK
000900960605     F*---------
001000960605     FTRTC91D   CF   E             WORKSTN
001100960605     F                                     SFILE(TC91S01:NRR1)
001200960612     D*---------------------------------------------------------------*
001300960612     D*  SCHIERE
001400960612     D*---------------------------------------------------------------*
001500960612     D CMD             S             40    DIM(1) CTDATA PERRCD(1)              Chkobj pgm
001600960605     D*---------------------------------------------------------------*
001700960605     D*  DS
001800960605     D*---------------------------------------------------------------*
001900960605     D KPJBA         E DS
002000960606     D TRTC91        E DS                  EXTNAME(TRTC91DS)
002100960612     D DS99          E DS                  EXTNAME(EDIDS99)
002200960605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002300960605     D CNCR80        E DS
002400960612     D*  Ds per controllo esistenza pgm
002500960612     D CMDAPP          DS            40
002600960612     D  WTBPGM                12     21
002700960605     D                SDS
002800960605     D  NOMPGM                 1     10
002900960605     C*---------------------------------------------------------------*
003000960605     C*  CICLO PRINCIPALE
003100960605     C*---------------------------------------------------------------*
003200960605      * Inizializzo videate
003300960605     C                   EXSR      CARSF1                                       i codici
003400960605      * Loop gestione videate
003500960605     C     WFINE         DOWEQ     'N'
003600960605     C     WTPVID        CASEQ     '1S'          GESS01                         Sbf.scelta cod.
003700960605     C     WTPVID        CASEQ     '1D'          GESD01                         no codici tab.
003800960605     C                   END
003900960605     C                   END
004000960605      * Fine Lavoro
004100960605     C     FINE          TAG
004200960605     C                   SETON                                        LR
004300960605     C*---------------------------------------------------------------*
004400960605     C*  CARSF1: Caricamento subfile dei codici
004500960605     C*---------------------------------------------------------------*
004600960605     C     CARSF1        BEGSR
004700960605      *
004800960605      *  Pulisco dati SFL
004900960605     C                   SETOFF                                       2021
005000960605     C                   WRITE     TC91C01
005100960605     C                   SETON                                        2021
005200960606     C                   MOVEL     '1S'          WTPVID            2
005300960605      *  Eseguo caricamento prima pagina
005400960612     C                   Z-ADD     1             SFNR1
005500960605     C                   MOVEL     '99'          KCOD
005600960606     C                   MOVEL     *LOVAL        WINKEY
005700960605     C                   EXSR      ROLL01
005800960605      *  Se non ho dati da caricare emetto videata vuota
005900960605     C     NRR1          IFEQ      0
006000960605      *  SFL vuoto
006100960605     C                   MOVEL     '1D'          WTPVID
006200960605     C                   END
006300960605     C*
006400960605     C                   ENDSR
006500960605     C*---------------------------------------------------------------*
006600960605     C*  GESS01: Gestione subfile codici tabelle
006700960605     C*---------------------------------------------------------------*
006800960605     C     GESS01        BEGSR
006900960605     C*
007000960605     C                   WRITE     TC91T01
007100960605     C                   EXFMT     TC91C01
007200960612     C                   Z-ADD     1             SFNR1
007300960605     C* Ritorno
007400960605     C     *INKL         IFEQ      '1'
007500960605     C                   MOVEL     '1'           WFINE
007600960606     C                   MOVEL     '1'           WFINE
007700960606     C                   MOVEL     '12'          D91CMD
007800960606     C                   MOVEL     TRTC91        KPJBU
007900960605     C                   GOTO      FINSF1
008000960605     C                   END
008100960605     C* ROLLUP
008200960605     C     *IN25         IFEQ      '1'
008300960605     C                   EXSR      ROLL01
008400960612     C                   Z-ADD     NRR1          SFNR1
008500960605     C                   GOTO      FINSF1
008600960605     C                   END
008700960605     C* Seleziono codice
008800960605     C                   READC     TC91S01                                30
008900960612     C     *IN30         IFEQ      '0'
009000960612     C     V1CSCE        IFEQ      'P'
009100960612     C                   MOVEL     CMD(1)        CMDAPP
009200960612     C                   MOVEL     V1CTPG        WTBPGM
009300960612     C                   Z-ADD     40            LUNG             15 5
009400960612     C                   MOVEL     *BLANKS       COMMAN
009500960612     C                   MOVEL     CMDAPP        COMMAN           80
009600960612     C                   CALL      'QCMDEXC'                            21
009700960612     C                   PARM                    COMMAN
009800960612     C                   PARM                    LUNG
009900960612     C     *IN21         IFEQ      '1'
010000960612     C                   MOVEL     '1'           V1CSCE
010100960612     C                   ELSE
010200960612     C                   CALL      V1CTPG
010300960612     C                   PARM                    KPJBA
010400960612     C                   MOVEL     TRTC91        KPJBU
010500960612     C                   MOVEL     'S'           WFINE
010600960612     C                   END
010700960612     C                   END
010800960612     C     V1CSCE        IFEQ      '1'
010900960605     C                   MOVEL     V1CCOD        D91COD
011000960605     C                   MOVEL     V1DCOD        D91UNI
011100960606     C                   MOVEL     TRTC91        KPJBU
011200960605     C                   MOVEL     'S'           WFINE
011300960605     C                   END
011400960612     C                   END
011500960605     C*
011600960605     C     FINSF1        ENDSR
011700960605     C*---------------------------------------------------------------*
011800960605     C*  GESD01: Gestione codici tabelle non presenti
011900960605     C*---------------------------------------------------------------*
012000960605     C     GESD01        BEGSR
012100960605     C*
012200960605     C                   EXFMT     TC91D01
012300960605     C* Ritorno
012400960605     C     *INKL         IFEQ      '1'
012500960605     C                   MOVEL     '1'           WFINE
012600960606     C                   MOVEL     '12'          D91CMD
012700960606     C                   MOVEL     TRTC91        KPJBU
012800960605     C                   END
012900960605     C*
013000960605     C                   ENDSR
013100960605     C*---------------------------------------------------------------*
013200960605     C*  ROLL01: Caricamento una pagina sufile codici
013300960605     C*---------------------------------------------------------------*
013400960605     C     ROLL01        BEGSR
013500960605     C*
013600960612     C                   Z-ADD     0             WNRIG1
013700960605     C                   MOVEL     WINKEY        KKEY
013800960605     C     KTAB          SETGT     EDTAB01L                           30
013900960605     C     KCOD          READE     EDTAB01L                               30
014000960605     C     *IN30         DOWEQ     '0'
014100960605     C     WNRIG1        ANDLT     17
014200040216     c*
014300040216     c*******            if        %subst(tabkey:1:2) <> 'PT' and
014400040216     c*******                      %subst(tabkey:1:2) <> 'PU' and
014500040216     c*******                      %subst(tabkey:1:2) <> 'CE'
014600960606     C                   ADD       1             NRR1              4 0
014700960606     C                   ADD       1             WNRIG1            2 0
014800960605     C                   MOVEL     *BLANKS       V1CSCE
014900960605     C                   MOVEL     TABKEY        V1CCOD
015000960612     C                   MOVEL     TABUNI        V1DCOD
015100960612     C                   MOVEL     TABUNI        DS99
015200960612     C                   MOVEL     §99PGM        V1CTPG
015300960605     C     TABFLG        IFEQ      ' '
015400960605     C                   MOVEL     *BLANKS       V1CANN
015500960605     C                   ELSE
015600960605     C                   MOVEL     'ANNULLAT'    V1CANN
015700960605     C                   MOVE      'O'           V1CANN
015800960605     C                   END
015900960605     C                   WRITE     TC91S01
016000110329     C                   MOVEL     TABKEY        WINKEY
016100040216     c*******            end
016200960606     C     KCOD          READE     EDTAB01L                               30
016300960605     C                   END
016400110329     C*******            MOVEL     TABKEY        WINKEY
016500960606     C*  Se sono a fine file accendo 23
016600960606     C     *IN30         COMP      '1'                                    23
016700960605     C*
016800960605     C                   ENDSR
016900960605     C*---------------------------------------------------------------*
017000960605     C*  *INZSR: Operazioni iniziali                                  *
017100960605     C*---------------------------------------------------------------*
017200960605     C     *INZSR        BEGSR
017300960605     C*
017400960605     C     *ENTRY        PLIST
017500960605     C                   PARM                    KPJBA
017600960606     C                   MOVEL     KPJBU         TRTC91
017700960605      *  Call a XPARUT
017800960605     C                   Z-ADD     1             CODUT
017900960605     C                   CALL      'X§PARUT'
018000960606     C                   PARM                    UT§DSE
018100960605     C                   MOVEL     REC80         CNCR80
018200960605     C                   MOVEL     RAGUT         RSUT
018300960605      * Definizione chiavi
018400960605     C     KTAB          KLIST
018500960605     C                   KFLD                    KCOD
018600960605     C                   KFLD                    KKEY
018700960605      * Definizione varibili
018800960605     C     *LIKE         DEFINE    TABCOD        KCOD
018900960605     C     *LIKE         DEFINE    TABKEY        KKEY
019000960606     C     *LIKE         DEFINE    TABKEY        WINKEY
019100960605      * Inizializzazione variabili
019200960605     C                   MOVEL     NOMPGM        V1CPGM
019300960605     C                   MOVEL     'N'           WFINE             1
019400960605     C*
019500960605     C                   ENDSR
019600960612**         CMD
019700960612CHKOBJ OBJ(          ) OBJTYPE(*PGM)
