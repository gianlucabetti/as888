000100980122     H DECEDIT('0,') DATEDIT(*DMY.)
000200990826     FTITAS31C  IF   E           K DISK
000300990826     FTITA730C  IF   E           K DISK
000400980713     FAZORG01L  IF   E           K DISK
000500980713     FTABEL00F  IF   E           K DISK
000600980713     FWFTNASCF  O    E             DISK
000700980123     D                 DS
000800980123     D  AA                     1      4  0
000900980123     D  MM                     5      6  0
001000980123     D  GG                     7      8  0
001100980123     D  DATA                   1      8  0
001200980713     D TIBS69        E DS                  EXTNAME(TIBS69DS)
001300980713     D CNACO         E DS                  EXTNAME(CNACO00F)
001400980713     D CNIND         E DS                  EXTNAME(CNIND00F)
001500980713     D CNCLP         E DS                  EXTNAME(CNCLP00F)
001600980713     D FNCLS         E DS                  EXTNAME(FNCLS00F)
001700980122     D KPJBA         E DS
001800980122     C*****************************************************************
001900980122     C     *ENTRY        PLIST
002000980122     C                   PARM                    KPJBA
002100980122     C                   Z-ADD     1             CODUT             1 0
002200980713     C                   Z-ADD     0             DECO              1 0
002300980713     C                   Z-ADD     *ZEROS        KSC               7 0
002400980122     C                   MOVE      '7'           VARIA7
002500990520     C                   Z-ADD     19990101      DDFT              8 0
002600990826     C                   Z-ADD     19991231      ADFT              8 0
002700980122     C                   Z-ADD     99999999      MAXD              8 0
002800980122     C***
002900980122     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
003000980122     C***
003100980122     C                   TIME                    W0140            14 0
003200980122     C                   MOVE      W0140         UDATE8            8 0
003300980122     C                   TIME                    HHMMSS            6 0
003400980122     C*
003500980713     C     KTAB1         KLIST
003600980713     C                   KFLD                    CODUT
003700980713     C                   KFLD                    COD1              2
003800980713     C                   KFLD                    KEY               8
003900990826     C** ACCESSO TITAS31C PER CLIENTE / DATA FATTURA
004000980122     C     DSTAS         KLIST
004100980123     C                   KFLD                    PRIMO             7 0
004200980122     C                   KFLD                    DDFT
004300980122     C     DSTAS1        KLIST
004400980122     C                   KFLD                    KSC
004500980123     C                   KFLD                    DATFAT            8 0
004600980122     C                   KFLD                    NUMFAT            6 0
004700980122     C*
004800980122     C     DSTASM        KLIST
004900980122     C                   KFLD                    KSC
005000980122     C                   KFLD                    MAXD              8 0
005100990826     C** ACCESSO TITA730C PER ANNO SPED./ FIL. PAR/ NUM.SERIE/ NUM.SPED./ TIPBOL
005200980122     C** E 7 VARIA
005300980122     C     DSTA7         KLIST
005400980122     C                   KFLD                    TASAAS
005500980122     C                   KFLD                    TASLNP
005600980122     C                   KFLD                    TASNRS
005700980122     C                   KFLD                    TASNSP
005800980122     C                   KFLD                    TASTBL
005900980122     C                   KFLD                    VARIA7            1
006000980122     C*
006100990826     C     DSTAS         SETLL     TITAS31C
006200980122    1C                   DO        *HIVAL
006300990826     C                   READ      TITAS31C                               31
006400980122    2C     *IN31         IFEQ      '0'
006500980122     C* SE ROTTURA DI CODICI CLIENTI CONTROLLO SE E' IL PRIMO CLIENTE
006600980122     C* CONTROLLANDO KSC CON ZERO
006700980122    3C     TASKSC        IFEQ      *ZEROS
006800980122     C                   ITER
006900980122    3C                   END
007000980123     C* CLIENTE NON E' UGAULE AL PRECEDENTE
007100980122    3C     KSC           IFNE      TASKSC
007200980123     C* CONTROLLO SE E' IL PRIMO CLIENTE
007300980122    4C     KSC           IFEQ      *ZEROS
007400980123     C     PRIMO         IFEQ      *ZEROS
007500980123     C                   Z-ADD     TASKSC        PRIMO
007600990826     C     DSTAS         SETLL     TITAS31C
007700980123     C                   ITER
007800980123     C                   END
007900980122     C                   MOVE      TASKSC        COM4              4 0
008000980122     C* SE CLIENTE VALIDO LO PRENDO IN CONSIDERAZIONE
008100980122    5C     COM4          IFNE      8888
008200980122     C     COM4          ANDNE     9999
008300980122     C     TASDFT        ANDLE     ADFT
008400980123     C     TASDFT        ANDGE     DDFT
008500980122     C                   Z-ADD     TASKSC        KSC
008600980122     C                   Z-ADD     TASKSC        STKSC             7 0
008700980122     C                   Z-ADD     TASDFT        DATFAT            8 0
008800980122     C                   Z-ADD     TASNFT        NUMFAT
008900990826     C     DSTAS1        SETLL     TITAS31C
009000980123     C                   EXSR      DETT
009100980122    5C                   END
009200980122   X4C                   ELSE
009300980122     C* SE KSC NON E' UGUALE A ZERO STAMPO LA ROTTURA DEL CLIENTE
009400980122    5C     STTCP         IFNE      *ZEROS
009500980122     C                   EXSR      STAFAT
009600980122    5C                   END
009700980713     C                   Z-ADD     *ZEROS        DECO              1 0
009800980122     C                   MOVE      TASKSC        COM4
009900980122     C* SE CLIENTE VALIDO LO PRENDO IN CONSIDERAZIONE
010000980122    5C     COM4          IFNE      8888
010100980122     C     COM4          ANDNE     9999
010200980122     C                   Z-ADD     TASKSC        STKSC             7 0
010300980122     C                   Z-ADD     TASKSC        KSC
010400980123     C                   Z-ADD     TASKSC        PRIMO
010500980123     C                   Z-ADD     *ZEROS        DATFAT
010600980123     C                   Z-ADD     *ZEROS        NUMFAT
010700990826     C     DSTAS         SETLL     TITAS31C
010800980123     C                   ITER
010900980122   X5C                   ELSE
011000980122     C* SE NON E' VALIDO MI POSIZIONE SUL CLIENTE SUCCESSIVO
011100980123     C                   Z-ADD     TASKSC        KSC
011200990826     C     DSTASM        SETLL     TITAS31C
011300980122    5C                   END
011400980122    4C                   END
011500980123   X3C                   ELSE
011600980123     C* CLIENTE UGUALE MA FATTURA DIVERSA
011700980123     C     TASDFT        IFLE      ADFT
011800980123     C     TASDFT        ANDGE     DDFT
011900980123     C     DATFAT        IFNE      TASDFT
012000980123     C     NUMFAT        ORNE      TASNFT
012100980123     C                   Z-ADD     TASDFT        DATFAT
012200980123     C                   Z-ADD     TASNFT        NUMFAT
012300990826     C     DSTAS1        SETLL     TITAS31C
012400980123     C                   EXSR      DETT
012500980123     C                   END
012600980123     C                   END
012700980122    3C                   END
012800980122     C*
012900980123     C                   ITER
013000980123   X2C                   ELSE
013100980123     C                   LEAVE
013200980122    2C                   END
013300980122    1C                   ENDDO
013400980122     C*
013500980122     C     STTCP         IFNE      *ZEROS
013600980122     C                   EXSR      STAFAT
013700980122     C                   END
013800980122     C*
013900980713     C                   CLEAR                   TIBS69
014000980713     C                   MOVEL     'C'           I69TLA
014100980713     C                   CALL      'TIBS69R'
014200980713     C                   PARM                    TIBS69
014300980713     C                   PARM                    CNACO
014400980713     C                   PARM                    CNIND
014500980713     C                   PARM                    CNCLP
014600980713     C                   PARM                    FNCLS
014700980122     C                   SETON                                        LR
014800980122     C*------------------------------------------------------------------------
014900980122     C*  STAMPA A ROTTURA FATTURA
015000980122     C*------------------------------------------------------------------------
015100980122     C     STAFAT        BEGSR
015200980713     C*
015300980713     C     DECO          IFEQ      *ZEROS
015400980713     C                   MOVEL     *BLANKS       ASCRAG
015500980713      * CHIAMO TIBS69 PER RICERCA RAGIONE SOCIALE CLIENTE
015600980713     C                   CLEAR                   TIBS69
015700980713     C                   CLEAR                   CNACO
015800980713     C                   CLEAR                   CNIND
015900980713     C                   CLEAR                   CNCLP
016000980713     C                   CLEAR                   FNCLS
016100980713     C                   MOVEL     ' '           I69TLA
016200980713     C                   MOVEL     KNSIF         I69SIF
016300980713     C                   Z-ADD     KSC           I69KAC
016400980713     C                   CALL      'TIBS69R'
016500980713     C                   PARM                    TIBS69
016600980713     C                   PARM                    CNACO
016700980713     C                   PARM                    CNIND
016800980713     C                   PARM                    CNCLP
016900980713     C                   PARM                    FNCLS
017000980713     C     O69ERR        IFEQ      *BLANK
017100980713     C                   MOVEL     ACORAG        ASCRAG
017200980713     C                   ENDIF
017300980713     C*
017400980713     C* RICERCA DELL'AREA DI APPARTENENZA DEL CLIENTE
017500980713     C                   MOVEL     KSC           LINEA             3 0
017600980713     C     LINEA         CHAIN     AZORG01L                           32
017700980713     C   32              MOVEL     *BLANK        ORGCAR
017800980713     C** RICERCA DELL'AREA
017900980713     C                   MOVEL     '05'          COD1
018000980713     C                   MOVEL     *BLANKS       KEY
018100980713     C                   MOVEL     ORGCAR        KEY
018200980713     C     KTAB1         CHAIN     TABEL                              37
018300980713     C                   MOVEL     *BLANKS       ASCDAR
018400980713     C  N37              MOVEL     TBLUNI        ASCDAR
018500980713     C                   MOVEL     ORGCAR        ASCCAR
018600980713     C                   ADD       1             DECO
018700980713     C                   ENDIF
018800980713     C*
018900980713     C                   Z-ADD     DATFAT        ASCDFA
019000980713     C                   Z-ADD     NUMFAT        ASCNFA
019100980713     C                   Z-ADD     KSC           ASCKSC
019200980713     C                   Z-ADD     STTCP         ASCTCP
019300980122     C*
019400980713     C                   Z-ADD     *ZEROS        STTCP
019500980713     C* SCRIVO IL RECORD
019600980713     C                   WRITE     WFTNASC0
019700980122     C*
019800980122     C                   ENDSR
019900980123     C*------------------------------------------------------------------------
020000980123     C*  DETTAGLIO FATTURA
020100980123     C*------------------------------------------------------------------------
020200980123     C     DETT          BEGSR
020300980123     C*
020400980123    3C                   DO        *HIVAL
020500990826     C     DSTAS1        READE     TITAS31C                               33
020600980123    4C     *IN33         IFEQ      '0'
020700980123     C*
020800980123     C* DETTAGLIO CLIENTE
020900980123     C     TASSV1        IFEQ      '7'
021000990826     C                   ADD       TASVA1        STTCP            18 3
021100980123     C                   ITER
021200980123     C                   END
021300980123     C*
021400980123     C     TASSV2        IFEQ      '7'
021500980123     C                   ADD       TASVA2        STTCP
021600980123     C                   ITER
021700980123     C                   END
021800980123     C*
021900980123     C     TASSV3        IFEQ      '7'
022000980123     C                   ADD       TASVA3        STTCP
022100980123     C                   ITER
022200980123     C                   ELSE
022300980123     C*PROVO AD AGGANCIARE SE NON E' UGUALE A BLANKS
022400980123     C     TASSV3        IFNE      ' '
022500990826     C     DSTA7         CHAIN     TITA730C                           32
022600980123     C  N32              ADD       TA7VAN        STTCP
022700980123     C                   END
022800980123     C                   END
022900980123     C*
023000980123     C                   ITER
023100980123     C*
023200980123   X4C                   ELSE
023300980123     C*
023400980123     C     STTCP         IFNE      *ZEROS
023500980123     C                   EXSR      STAFAT
023600980123     C                   END
023700980123     C*
023800980123     C                   LEAVE
023900980123    4C                   END
024000980123    3C                   ENDDO
024100990826     C     DSTAS1        SETGT     TITAS31C
024200980123     C                   ENDSR
