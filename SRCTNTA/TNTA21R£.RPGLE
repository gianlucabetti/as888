000100011015     H DECEDIT('0,') DATEDIT(*DMY.)
000200011015      *****************************************************************
000300011015      *                                                               *
000400011015      *         STAMPA MANDATI VARIATI                                *
000500011015      *                                                               *
000600011015      *****************************************************************
000700011015
000800011015     FTNTA21D   CF   E             WORKSTN
000900011015
001000011015     D MSG             S             79    DIM(2) CTDATA PERRCD(1)
001100011015
001200011015     D* RICHIAMO PGM STAMPA MANDATI VARIATI -BATCH-
001300011015     D PARM21          DS
001400011015     D* Data Iniziale
001500011015     D  PARDAL                 1      8  0
001600011015     D* Data Finale
001700011015     D  PARAL                  9     16  0
001800011015     D*
001900011015     D KPJBA         E DS
002000011015     D*
002100011015     D G02DS           DS
002200011015     D  G02DAT                 1      8  0
002300011015     D  G02INV                 9     16  0
002400011015     D  G02ERR                17     17
002500011015     D  G02TGI                18     22  0
002600011015     D*
002700011015     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002800011015
002900011015     C*****************************************************************
003000011015     C* RIEPILOGO INDICATORI
003100011015     C*****************************************************************
003200011015     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
003300011015     C* 30    - DI COMODO
003400011015     C* 40/42 - ERRORI
003500011015     C*****************************************************************
003600011015     C     *ENTRY        PLIST
003700011015     C                   PARM                    KPJBA
003800011015     C*
003900011016     C                   CLEAR                   UT§DSE
004000011016     C                   Z-ADD     1             CODUT
004100011015     C                   CALL      'X§PARUT'
004200011015     C                   PARM                    UT§DSE
004300011015     C                   MOVEL     RAGUT         V1CRSU
004400011015     C*
004500011015     C                   Z-ADD     *DATE         V1CAL
004600011015     C*
004700011015     C     FOR01         TAG
004800011015     C                   EXFMT     TA21D01
004900011015     C*
005000011015     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
005100011015     C                   CLEAR                   V1CMSG
005200011015     C                   SETOFF                                       28
005300011015     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
005400011015     C                   SETOFF                                       404142
005500011015     C*
005600011015     C*> CMD3 - FINE LAVORO
005700011015     C   KC              GOTO      FINE
005800011015     C*
005900011015     C*> C O N T R O L L I
006000011015     C                   EXSR      CTRD01
006100011015     C*
006200011015     C  NKF
006300011015     COR 28              GOTO      FOR01
006400011015     C*
006500011015     C*
006600011015     C*> CMD6 - CONFERMA ELABORAZIONI
006700011015     C*
006800011015     C                   MOVEL     'TA22'        KCOAZ
006900011016     C                   CLEAR                   PARM21
007000011015     C                   Z-ADD     AMGDAL        PARDAL
007100011015     C                   Z-ADD     AMGAL         PARAL
007200011015     C                   MOVEL     PARM21        KPJBU
007300011016     C                   if        %subst(KNMUS:1:3) = 'EDP'
007400011016     C                   CALL      'TNTA22R'
007500011016     C                   PARM                    KPJBA
007600011016     C                   else
007700011016     C                   CALL      'BCH10'
007800011015     C                   PARM                    KPJBA
007900011016     C                   endif
008000011015     C*
008100011015     C     FINE          TAG
008200011015     C                   SETON                                        LR
008300011015     C*---------------------------------------------------------------*
008400011015     C*--- CONTROLLO CAMPI FORMATO -----------------------------------*
008500011015     C*---------------------------------------------------------------*
008600011015     C     CTRD01        BEGSR
008700011015     C*
008800011015     C                   Z-ADD     0             AMGDAL            8 0
008900011015     C                   Z-ADD     0             AMGAL             8 0
009000011015     C* DATA INIZIALE
009100011015   B1C     V1CDAL        IFNE      0
009200011015     C                   Z-ADD     V1CDAL        G02DAT
009300011015     C                   MOVEL     *BLANK        G02ERR
009400011015     C                   CALL      'XSRDA8'
009500011015     C                   PARM                    G02DS
009600011015   B2C     G02ERR        IFEQ      '1'
009700011015     C                   MOVEL     MSG(1)        V1CMSG
009800011015     C                   SETON                                        4128
009900011015     C                   GOTO      ENDCTR
010000011015   E2C                   ENDIF
010100011015     C                   Z-ADD     G02INV        AMGDAL
010200011015     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
010300011015     C                   Z-ADD     G02DAT        V1CDAL
010400011015   E1C                   ENDIF
010500011015     C*
010600011015     C* DATA FINALE
010700011015   B1C     V1CAL         IFNE      0
010800011015     C                   Z-ADD     V1CAL         G02DAT
010900011015     C                   MOVEL     *BLANK        G02ERR
011000011015     C                   CALL      'XSRDA8'
011100011015     C                   PARM                    G02DS
011200011015   B2C     G02ERR        IFEQ      '1'
011300011015     C                   MOVEL     MSG(1)        V1CMSG
011400011015     C                   SETON                                        4228
011500011015     C                   GOTO      ENDCTR
011600011015   E2C                   ENDIF
011700011015     C                   Z-ADD     G02INV        AMGAL
011800011015     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
011900011015     C                   Z-ADD     G02DAT        V1CAL
012000011015   E1C                   ENDIF
012100011015     C*
012200011015     C* RANGE DI DATE
012300011015   B1C     AMGDAL        IFGT      AMGAL
012400011015     C                   MOVEL     MSG(2)        V1CMSG
012500011015     C                   SETON                                        414228
012600011015     C                   GOTO      ENDCTR
012700011015   E1C                   ENDIF
012800011015     C*
012900011015     C     ENDCTR        ENDSR
013000011015     C*---------------------------------------------------------------*
013100011015**  MSG
013200011015Data Errata                                                                    1
013300011015Data Iniziale > Data Finale                                                    2
