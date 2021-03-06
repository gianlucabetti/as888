000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200960912     H* TNTL58R  *----------------------------------------------------*
000300951110     H*----------*                                                    *
000400960912     H*                 PULIZIA TRAINI     - BATCH -                  **
000500951110     H*---------------------------------------------------------------*
000600960913     FTNTST06L  UF   E           K DISK
000700960913     FTNTSM01L  UF   E           K DISK
000800960913     FTNTSR01L  UF   E           K DISK
000900960919     FTABEL00F  IF   E           K DISK
001000951110     D*
001100960912     D* DS PER TNTL57R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
001200960912     D DSTL57        E DS                  EXTNAME(TNTL57DS)
001300960229     D*
001400960919     D WLBDAT          DS
001500960919     D  G02DAT                 1      8  0
001600960919     D  G02INV                 9     16  0
001700960919     D  G02ERR                17     17
001800960919     D  G02TGI                18     22  0
001900960919     D WGIDAT          DS
002000960919     D  GIODAT                 1      8  0
002100960919     D  GIOINV                 9     16  0
002200960919     D  GIOTGI                17     21  0
002300960919     D*
002400970718     D DS5A2         E DS
002500951110     D KPJBA         E DS
002600951110     I/SPACE 3
002700951110     C*****************************************************************
002800951110     C* RIEPILOGO INDICATORI
002900951110     C*****************************************************************
003000960912     C* 30/31 - DI COMODO
003100960925     C* 99    - ALLOCAZIONE DTAARA
003200951110     C*****************************************************************
003300951110     C     *ENTRY        PLIST
003400951110     C                   PARM                    KPJBA
003500960912     C                   MOVEL     KPJBU         DSTL57
003600951113     C*
003700951110     C*---------------------------------------------------------------*
003800960925     C* ALLOCO LA DATA AREA
003900960925     C     *LOCK         IN        TNTL58                               99
004000960925     C                   Z-ADD     1             I                 3 0
004100960925     C*
004200960925     C* ALLOCAZIONE NON RIUSCITA: RITENTO 100 VOLTE AL MASSIMO
004300960925    1C     *IN99         DOWEQ     *ON
004400960925     C     I             ANDLT     100
004500960925     C     *LOCK         IN        TNTL58                               99
004600960925     C                   ADD       1             I
004700960925    1C                   ENDDO
004800960925     C*
004900960925     C* 99 ON  - VADO A FINE PGM
005000960925    0C     *IN99         IFEQ      *OFF
005100960925     C*
005200960925     C*
005300960919     C* SE RICEVO LA DATA ELABORAZIONE A 0 --> LA RICALCOLO
005400960919    1C     D57DEA        IFEQ      0
005500970718     C* DATA ELABORAZIONE AL: REPERISCO GIORNI DA DS5A
005600970718     C                   MOVEL     '5A'          COD
005700970718     C                   MOVEL(P)  '2'           KEY
005800960919     C     KTAB          CHAIN     TABEL                              30
005900960919     C     *IN30         IFEQ      *OFF
006000960919     C     TBLFLG        ANDEQ     ' '
006100970718     C                   MOVEL     TBLUNI        DS5A2
006200960919     C                   ELSE
006300970718     C                   CLEAR                   DS5A2
006400960919     C                   ENDIF
006500960919     C*
006600960919     C* PRENDO LA DATA DEL GIORNO DA TIME
006700960919     C                   TIME                    W0140            14 0
006800960919     C                   MOVE      W0140         UDATE8            8 0
006900960919     C*
007000960919     C                   MOVE      UDATE8        G02DAT
007100960919     C                   MOVEL     *BLANK        G02ERR
007200960919     C                   CALL      'XSRDA8'
007300960919     C                   PARM                    WLBDAT
007400960919     C*
007500970718     C* SOTTRAGGO �5AFVS AI GIORNI
007600970718     C     G02TGI        SUB       �5AFVS        GIOTGI
007700960919     C*
007800960919     C                   CALL      'XSRGI8'
007900960919     C                   PARM                    WGIDAT
008000960919     C                   Z-ADD     GIOINV        D57DEA
008100960919    1C                   ENDIF
008200960919     C*
008300960916     C*****
008400960916     C** C I C L O    P E R    I    T R A I N I    A N N U L L A T I
008500960916     C*****
008600960916     C* LEGGO SOLO I RECORD CON DFT=0 E ATB='A' IN MODO DA LEGGERE SOLO
008700960916     C*   I TRAINI ANNULLATI
008800960916     C                   CLEAR                   WDFT
008900960916     C                   MOVEL     'A'           WATB
009000960913     C     KTST          SETLL     TNTST06L
009100960913     C     KTST          READE     TNTST06L                               30
009200960912     C*
009300960912    1C     *IN30         DOWEQ     *OFF
009400960912     C*
009500960913     C* TRAINO ANNULLATO ---> CONFRONTO LA DATA ELABORAZIONE CON LA
009600960913     C*                       DATA TRAINO
009700960913    2C     TSTDTN        IFLE      D57DEA
009800960913     C                   EXSR      PULIZ
009900960913     C*
010000960913   X2C                   ELSE
010100960913     C* SE DATA TRAINO MAGGIORE DELLA DATA ELABORAZIONE: ESCO DAL CICLO
010200960913     C                   SETON                                            30
010300960913    2C                   ENDIF
010400960913     C*
010500960913     C  N30KTST          READE     TNTST06L                               30
010600960913    1C                   ENDDO
010700960913     C*
010800960916     C*****
010900960916     C** C I C L O    P E R    I    T R A I N I    V A L I D I
011000960916     C*****
011100960913     C* LEGGO SOLO I RECORD CON DFT > 0 IN MODO DA LEGGERE SOLO I
011200960913     C*   RECORD VALIDI
011300960916     C                   Z-ADD     1             WDFT
011400960913     C     WDFT          SETLL     TNTST06L
011500960913     C                   READ      TNTST06L                               30
011600960913     C*
011700960913    1C     *IN30         DOWEQ     *OFF
011800960913     C*
011900960912     C* TRAINO VALIDO    ---> CONFRONTO LA DATA ELABORAZIONE CON LA
012000960912     C*                       DATA FATTURA
012100960913    2C     TSTDFT        IFLE      D57DEA
012200960925     C*
012300960925     C* CONTROLLO CHE ANCHE LA DATA TRAINO SIA MINORE O UGUALE ALLA
012400960925     C*   DATA PULIZIA (QUESTO PERCHE' PUO' ACCADERE CHE LA DATA TRAINO
012500960925     C*   SIA MAGGIORE DELLA DATA FATTURA)
012600960925    3C     TSTDTN        IFLE      D57DEA
012700960925     C*
012800960925     C* MEMORIZZO LA DATA TRAINO PIU' RECENTE PULITA
012900960925     C     TSTDTN        IFGT      WDATA
013000960925     C                   Z-ADD     TSTDTN        WDATA
013100960925     C                   ENDIF
013200960925     C*
013300960912     C                   EXSR      PULIZ
013400960925    3C                   ENDIF
013500960925     C*
013600960913   X2C                   ELSE
013700960913     C* SE DATA FATTURA MAGGIORE DELLA DATA ELABORAZIONE:ESCO DAL CICLO
013800960913     C                   SETON                                            30
013900960913    2C                   ENDIF
014000960912     C*
014100960913     C  N30              READ      TNTST06L                               30
014200960912    1C                   ENDDO
014300960912     C*
014400960925     C*
014500960925     C* AGGIORNO DATA AREA CON DATA TRAINO PIU' RECENTE PULITA
014600960925    1C     WDATA         IFGT      TNTL58
014700960925     C                   Z-ADD     WDATA         TNTL58            8 0
014800960925    1C                   ENDIF
014900960925     C                   OUT       TNTL58
015000960925    0C                   ENDIF
015100960925     C*
015200960913     C                   SETON                                        LR
015300960913     C*
015400960912     C*--- ROUTINE DI PULIZIA ----------------------------------------*
015500960912     C     PULIZ         BEGSR
015600960912     C*
015700960912     C****  TNTSM00F ---> STORICO METRICUBI ASSEGNATI  ****
015800960912     C                   SETOFF                                       31
015900960913     C     KTST2         SETLL     TNTSM01L
016000960912    1C     *IN31         DOWEQ     *OFF
016100960913     C     KTST2         READE     TNTSM01L                               31
016200960912     C  N31              DELETE    TNTSM000
016300960912    1C                   ENDDO
016400960912     C*
016500960912     C****  TNTSR00F ---> STORICO TRATTE  ****
016600960912     C                   SETOFF                                       31
016700960913     C     KTST2         SETLL     TNTSR01L
016800960912    1C     *IN31         DOWEQ     *OFF
016900960913     C     KTST2         READE     TNTSR01L                               31
017000960912     C  N31              DELETE    TNTSR000
017100960912    1C                   ENDDO
017200960912     C*
017300960912     C****  TNTST00F ---> STORICO TRAINI  ****
017400960912     C                   DELETE    TNTST000
017500960912     C*
017600960912     C                   ENDSR
017700960912     C*
017800960226     C*--- SR INIZIALE -----------------------------------------------*
017900951110     C     *INZSR        BEGSR
018000960912     C****
018100960912     C** KLIST
018200960912     C****
018300960913     C* ACCESSO TNTST06L
018400960913     C     KTST          KLIST
018500960913     C                   KFLD                    WDFT
018600960913     C                   KFLD                    WATB
018700960912     C* ACCESSO TNTSR01L / TNTSM01L
018800960913     C     KTST2         KLIST
018900960912     C                   KFLD                    TSTTRN
019000960912     C                   KFLD                    TSTDTN
019100960919     C* ACCESSO TABEL00F
019200960919     C     KTAB          KLIST
019300960919     C                   KFLD                    CODUT
019400960919     C                   KFLD                    COD
019500960919     C                   KFLD                    KEY
019600960913     C****
019700960913     C** DEFINIZIONE CAMPI
019800960913     C****
019900960913     C     *LIKE         DEFINE    TSTDFT        WDFT
020000960913     C     *LIKE         DEFINE    TSTATB        WATB
020100960925     C     *LIKE         DEFINE    TSTDTN        WDATA
020200960919     C     *LIKE         DEFINE    TBLCOD        COD
020300960919     C     *LIKE         DEFINE    TBLKEY        KEY
020400960925     C*
020500960925     C* DATA AREA CONTENENTE LA DATA TRAINO PIU' RECENTE PULITA
020600960925     C     *DTAARA       DEFINE                  TNTL58
020700960228     C*
020800960919     C* IMPOSTAZIONE CAMPI FISSI
020900960919     C                   Z-ADD     1             CODUT             1 0
021000960229     C                   ENDSR
