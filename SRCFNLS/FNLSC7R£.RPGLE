000100000000     H DATEDIT(*YMD)
000200000000      **************************************************************************
000300950118      *  Nome programma:  FNLSC7R
000400000000      *  Descrizione   :  Pulizia archivi statistiche partenze (batch)
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  01 FEB 1994
000700000000      **************************************************************************
000800000000      *  Come parametro di ingresso vi e' solo la data limite per la pulizia
000900000000      *  dell'archivio; tale data viene impostata dal pgm di lancio
001000950118      *  FNLS60R
001100040924      *  24/09/04: Non si riceve più la data di pulizia ma viene calco-
001200040924      *  lata direttamenta da questo pgm
001300000000      **************************************************************************
001400990507     FFNSBO01L  UF   E           K DISK    USROPN
001500950118     FFNSCE01L  UF   E           K DISK    USROPN
001600970314     F*
001700970314     FFNSPA01L  UF   E           K DISK    USROPN
001800970314     FFNSAT04L  UF   E           K DISK    USROPN
001900970318     FFNANT01L  UF   E           K DISK    USROPN
002000040927     FTABEL00F  IF   E           K DISK
002100000000      *
002200000000     D KPJBA         E DS
002300040927     D DS5AS1        E DS
002400040927     D*PARAM           DS
002500040927     D**DATA PULIZIA AAAA/MM/GG (8 CARATTERI)
002600040927     D**CO1DPT                 9     16  0
002700040927     D**DATA PULIZIA AAAA/MM/GG (8 CARATTERI) PER FILE FNWRK00S
002800040927     D**CO1WRK                17     24  0
002900040927     D WLBDAT          DS
003000040927     D  G02DAT                 1      8  0
003100040927     D  G02INV                 9     16  0
003200040927     D  G02ERR                17     17
003300040927     D  G02TGI                18     22  0
003400040927     D WGIDAT          DS
003500040927     D  GIODAT                 1      8  0
003600040927     D  GIOINV                 9     16  0
003700040927     D  GIOTGI                17     21  0
003800000000      *
003900000000      **************************************************************************
004000000000      *             M A I N      L I N E
004100000000      **************************************************************************
004200000000      *
004300000000      *-------------------------------------------------------------------------
004400040927      * CALCOLO DATA PULIZIA STATISTICHE PARTENZA
004500040927     C* Ricavo il nr. giorni dalla data odierna
004600040927     C                   TIME                    WTIME            14 0
004700040927     C                   MOVE      WTIME         WDATE             8 0
004800040927     C                   MOVE      WDATE         G02DAT
004900040927     C                   MOVEL     *BLANK        G02ERR
005000040927     C                   CALL      'XSRDA8'
005100040927     C                   PARM                    WLBDAT
005200040927     C                   Z-ADD     0             CO1DPT            8 0
005300040927     C     G02TGI        SUB       §5ASTS        GIOTGI
005400040927      *
005500040927     C                   CALL      'XSRGI8'
005600040927     C                   PARM                    WGIDAT
005700040928     C                   Z-ADD     GIOinv        CO1DPT
005800040927     c*
005900000000      *
006000000000      * Leggo sequenzialmente il file statistiche sul bollettato
006100990507     C                   OPEN      FNSBO01L
006200990507     C     *LOVAL        SETLL     FNSBO01L
006300000000     C                   DO        *HIVAL
006400000000      *
006500990507     C                   READ      FNSBO01L                               31
006600990507     C  N31SBODRE        IFLE      CO1DPT
006700040927     C***  SBOFTR        ANDNE     ' '
006800000000      *
006900000000      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
007000990507     C                   DELETE    FNSBO000
007100000000      *
007200000000     C                   ENDIF
007300000000     C  N31              ENDDO
007400990507     C                   CLOSE     FNSBO01L
007500000000      *-------------------------------------------------------------------------
007600000000      *
007700000000      * Leggo sequenzialmente il file statistiche controllo entrate
007800950118     C                   OPEN      FNSCE01L
007900950118     C     *LOVAL        SETLL     FNSCE01L
008000000000     C                   DO        *HIVAL
008100000000      *
008200950118     C                   READ      FNSCE01L                               31
008300950118     C  N31SCEDRE        IFLE      CO1DPT
008400040927     C***  SCEFTR        ANDNE     ' '
008500000000      *
008600000000      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
008700950118     C                   DELETE    FNSCE000
008800000000      *
008900000000     C                   ENDIF
009000000000     C  N31              ENDDO
009100950118     C                   CLOSE     FNSCE01L
009200000000      *-------------------------------------------------------------------------
009300970317      * A TERRA
009400970314     C                   OPEN      FNSAT04L
009500970317     C     *LOVAL        SETLL     FNSAT04L
009600970317     C                   READ      FNSAT04L                               31
009700970317    1C     *IN31         DOWEQ     *OFF
009800970317      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
009900970317    2C     SATDRE        IFLE      CO1DPT
010000040927    3C***  SATFTR        IFNE      ' '
010100970317     C                   DELETE    FNSAT000
010200040927    3C***                END
010300970317   X2C                   ELSE
010400970317     C     KSAT          SETGT     FNSAT04L
010500970317    2C                   END
010600970317     C                   READ      FNSAT04L                               31
010700970317    1C                   ENDDO
010800970317     C                   CLOSE     FNSAT04L
010900970317     C*
011000970317     C* PARTITO
011100970314     C                   OPEN      FNSPA01L
011200970317     C     *LOVAL        SETLL     FNSPA01L
011300970317     C                   READ      FNSPA01L                               31
011400970317    1C     *IN31         DOWEQ     *OFF
011500970317      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
011600970317    2C     SPADRE        IFLE      CO1DPT
011700040927    3C***  SPAFTR        IFNE      ' '
011800970314     C                   DELETE    FNSPA000
011900040927    3C***                END
012000970317   X2C                   ELSE
012100970318     C     KSPA          SETGT     FNSPA01L
012200970317    2C                   END
012300970317     C                   READ      FNSPA01L                               31
012400970317    1C                   ENDDO
012500970314     C                   CLOSE     FNSPA01L
012600970318     C* TOTALI ANOMALIE
012700970318     C                   OPEN      FNANT01L
012800970318     C     *LOVAL        SETLL     FNANT01L
012900970318     C                   READ      FNANT01L                               31
013000970318    1C     *IN31         DOWEQ     *OFF
013100970318    2C     ANTDAO        IFLE      CO1DPT
013200040927     C***  ANTFT1        IFNE      *BLANKS
013300040927     C***  ANTFT2        ANDNE     *BLANKS
013400970318     C                   DELETE    FNANT000
013500040927     C***                END
013600970318   X2C                   ELSE
013700970318     C     ANTLNP        SETGT     FNANT01L
013800970318    2C                   END
013900970318     C                   READ      FNANT01L                               31
014000970318    1C                   ENDDO
014100970318     C                   CLOSE     FNANT01L
014200970314     C*
014300970314     C* RICHIAMO PGM PULIZIA MEMBRI FNWRK00S
014400970314     C*
014500040927     C****               MOVEL(P)  PARAM         KPJBU
014600040927     C****               CALL      'FNLSC7C'
014700040927     C****               PARM                    KPJBA
014800970314     C*
014900000000     C                   SETON                                        LR
015000970314     C****************  ROUTINE INIZIALE  *****************************
015100970314     C     *INZSR        BEGSR
015200970314     C*
015300970314     C     *ENTRY        PLIST
015400970314     C                   PARM                    KPJBA
015500040927     C****               MOVEL     KPJBU         PARAM
015600040927     C                   Z-ADD     1             CODUT
015700970314     C*
015800970314     C* IMPOSTO CAMPI CHIAVE
015900970317     C     KSAT          KLIST
016000970317     C                   KFLD                    SATFLE
016100970317     C                   KFLD                    SATLNP
016200970317     C*
016300970317     C     KSPA          KLIST
016400970317     C                   KFLD                    SPAFLE
016500970317     C                   KFLD                    SPALNP
016600040927     C* TABEL00F
016700040927     C     KTAB          KLIST
016800040927     C                   KFLD                    CODUT             1 0
016900040927     C                   KFLD                    COD
017000040927     C                   KFLD                    KEY
017100040927     c*
017200040927     C     *LIKE         DEFINE    TBLCOD        COD
017300040927     C     *LIKE         DEFINE    TBLKEY        KEY
017400040927     C* LETTURA GIORNI DI PULIZIA
017500040927     C                   MOVEL     '5A'          COD
017600040927     C                   MOVEL     'SEDE1   '    KEY
017700040927     C     KTAB          CHAIN     TABEL00F                           30
017800040927     C  N30              MOVEL     TBLUNI        DS5AS1
017900040927     C   30              CLEAR                   DS5AS1
018000970314     C*
018100970314     C                   ENDSR
