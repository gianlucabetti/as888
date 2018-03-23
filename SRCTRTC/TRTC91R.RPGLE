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
005700130219     c     avanti        tag
005800960605     C                   EXSR      ROLL01
005900130219     c  n23              goto      avanti
006000960605      *  Se non ho dati da caricare emetto videata vuota
006100960605     C     NRR1          IFEQ      0
006200960605      *  SFL vuoto
006300960605     C                   MOVEL     '1D'          WTPVID
006400960605     C                   END
006500960605     C*
006600960605     C                   ENDSR
006700960605     C*---------------------------------------------------------------*
006800960605     C*  GESS01: Gestione subfile codici tabelle
006900960605     C*---------------------------------------------------------------*
007000960605     C     GESS01        BEGSR
007100960605     C*
007200960605     C                   WRITE     TC91T01
007300960605     C                   EXFMT     TC91C01
007400130219     c                   if        nrr1 = 0
007500130219     C                   Z-ADD     1             SFNR1
007600130219     c                   else
007700130219     C                   Z-ADD     sflrig1       SFNR1
007800130219     c                   end
007900960605     C* Ritorno
008000960605     C     *INKL         IFEQ      '1'
008100960605     C                   MOVEL     '1'           WFINE
008200960606     C                   MOVEL     '1'           WFINE
008300960606     C                   MOVEL     '12'          D91CMD
008400960606     C                   MOVEL     TRTC91        KPJBU
008500960605     C                   GOTO      FINSF1
008600960605     C                   END
008700960605     C* ROLLUP
008800130219     C**** *IN25         IFEQ      '1'
008900130219     C******             EXSR      ROLL01
009000130219     C******             Z-ADD     NRR1          SFNR1
009100130219     C*******            GOTO      FINSF1
009200130219     C*******            END
009300960605     C* Seleziono codice
009400960605     C                   READC     TC91S01                                30
009500960612     C     *IN30         IFEQ      '0'
009600960612     C     V1CSCE        IFEQ      'P'
009700960612     C                   MOVEL     CMD(1)        CMDAPP
009800960612     C                   MOVEL     V1CTPG        WTBPGM
009900960612     C                   Z-ADD     40            LUNG             15 5
010000960612     C                   MOVEL     *BLANKS       COMMAN
010100960612     C                   MOVEL     CMDAPP        COMMAN           80
010200960612     C                   CALL      'QCMDEXC'                            21
010300960612     C                   PARM                    COMMAN
010400960612     C                   PARM                    LUNG
010500960612     C     *IN21         IFEQ      '1'
010600960612     C                   MOVEL     '1'           V1CSCE
010700960612     C                   ELSE
010800960612     C                   CALL      V1CTPG
010900960612     C                   PARM                    KPJBA
011000960612     C                   MOVEL     TRTC91        KPJBU
011100960612     C                   MOVEL     'S'           WFINE
011200960612     C                   END
011300960612     C                   END
011400960612     C     V1CSCE        IFEQ      '1'
011500960605     C                   MOVEL     V1CCOD        D91COD
011600960605     C                   MOVEL     V1DCOD        D91UNI
011700960606     C                   MOVEL     TRTC91        KPJBU
011800960605     C                   MOVEL     'S'           WFINE
011900960605     C                   END
012000960612     C                   END
012100960605     C*
012200960605     C     FINSF1        ENDSR
012300960605     C*---------------------------------------------------------------*
012400960605     C*  GESD01: Gestione codici tabelle non presenti
012500960605     C*---------------------------------------------------------------*
012600960605     C     GESD01        BEGSR
012700960605     C*
012800960605     C                   EXFMT     TC91D01
012900960605     C* Ritorno
013000960605     C     *INKL         IFEQ      '1'
013100960605     C                   MOVEL     '1'           WFINE
013200960606     C                   MOVEL     '12'          D91CMD
013300960606     C                   MOVEL     TRTC91        KPJBU
013400960605     C                   END
013500960605     C*
013600960605     C                   ENDSR
013700960605     C*---------------------------------------------------------------*
013800960605     C*  ROLL01: Caricamento una pagina sufile codici
013900960605     C*---------------------------------------------------------------*
014000960605     C     ROLL01        BEGSR
014100960605     C*
014200960612     C                   Z-ADD     0             WNRIG1
014300960605     C                   MOVEL     WINKEY        KKEY
014400960605     C     KTAB          SETGT     EDTAB01L                           30
014500960605     C     KCOD          READE     EDTAB01L                               30
014600960605     C     *IN30         DOWEQ     '0'
014700960605     C     WNRIG1        ANDLT     17
014800040216     c*
014900040216     c*******            if        %subst(tabkey:1:2) <> 'PT' and
015000040216     c*******                      %subst(tabkey:1:2) <> 'PU' and
015100040216     c*******                      %subst(tabkey:1:2) <> 'CE'
015200960606     C                   ADD       1             NRR1              4 0
015300960606     C                   ADD       1             WNRIG1            2 0
015400960605     C                   MOVEL     *BLANKS       V1CSCE
015500960605     C                   MOVEL     TABKEY        V1CCOD
015600960612     C                   MOVEL     TABUNI        V1DCOD
015700960612     C                   MOVEL     TABUNI        DS99
015800960612     C                   MOVEL     §99PGM        V1CTPG
015900960605     C     TABFLG        IFEQ      ' '
016000960605     C                   MOVEL     *BLANKS       V1CANN
016100960605     C                   ELSE
016200960605     C                   MOVEL     'ANNULLAT'    V1CANN
016300960605     C                   MOVE      'O'           V1CANN
016400960605     C                   END
016500960605     C                   WRITE     TC91S01
016600110329     C                   MOVEL     TABKEY        WINKEY
016700040216     c*******            end
016800960606     C     KCOD          READE     EDTAB01L                               30
016900960605     C                   END
017000110329     C*******            MOVEL     TABKEY        WINKEY
017100960606     C*  Se sono a fine file accendo 23
017200960606     C     *IN30         COMP      '1'                                    23
017300960605     C*
017400960605     C                   ENDSR
017500960605     C*---------------------------------------------------------------*
017600960605     C*  *INZSR: Operazioni iniziali                                  *
017700960605     C*---------------------------------------------------------------*
017800960605     C     *INZSR        BEGSR
017900960605     C*
018000960605     C     *ENTRY        PLIST
018100960605     C                   PARM                    KPJBA
018200960606     C                   MOVEL     KPJBU         TRTC91
018300960605      *  Call a XPARUT
018400960605     C                   Z-ADD     1             CODUT
018500960605     C                   CALL      'X§PARUT'
018600960606     C                   PARM                    UT§DSE
018700960605     C                   MOVEL     REC80         CNCR80
018800960605     C                   MOVEL     RAGUT         RSUT
018900960605      * Definizione chiavi
019000960605     C     KTAB          KLIST
019100960605     C                   KFLD                    KCOD
019200960605     C                   KFLD                    KKEY
019300960605      * Definizione varibili
019400960605     C     *LIKE         DEFINE    TABCOD        KCOD
019500960605     C     *LIKE         DEFINE    TABKEY        KKEY
019600960606     C     *LIKE         DEFINE    TABKEY        WINKEY
019700960605      * Inizializzazione variabili
019800960605     C                   MOVEL     NOMPGM        V1CPGM
019900960605     C                   MOVEL     'N'           WFINE             1
020000960605     C*
020100960605     C                   ENDSR
020200960612**         CMD
020300960612CHKOBJ OBJ(          ) OBJTYPE(*PGM)
