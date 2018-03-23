000100011015     H DECEDIT('0,') DATEDIT(*DMY.)
000200011015      *****************************************************************
000300011015      *                                                               *
000400080407      *         ELENCO MANDATI VARIATI                                *
000500011015      *                                                               *
000600011015      *****************************************************************
000700011015
000800080407     FTNTA26D   CF   E             WORKSTN
000900011015
001000080409     D MSG             S             79    DIM(1) CTDATA PERRCD(1)
001100011015
001200011015     D* RICHIAMO PGM STAMPA MANDATI VARIATI -BATCH-
001300080407     D PARM26          DS
001400080407     D* Data
001500080407     D  PARDAY                 1      8  0
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
004600011015     C*
004700011015     C     FOR01         TAG
004800080409     C                   EXFMT     TA26D01
004900011015     C*
005000011015     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
005100011015     C                   CLEAR                   V1CMSG
005200011015     C                   SETOFF                                       28
005300011015     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
005400080409     C                   SETOFF                                       41
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
006800080409     C                   MOVEL     'TA27'        KCOAZ
006900080409     C                   CLEAR                   PARM26
007000080409     C                   Z-ADD     AMGDAY        PARDAY
007200080409     C                   MOVEL     PARM26        KPJBU
007300080409     C                   if        %subst(KNMUS:1:3) = 'EDP'
007400080409     C                   CALL      'TNTA27R'
007500080409     C                   PARM                    KPJBA
007600080409     C                   else
007700011016     C                   CALL      'BCH10'
007800011015     C                   PARM                    KPJBA
007900080409     C                   endif
008000011015     C*
008100011015     C     FINE          TAG
008200011015     C                   SETON                                        LR
008300011015     C*---------------------------------------------------------------*
008400011015     C*--- CONTROLLO CAMPI FORMATO -----------------------------------*
008500011015     C*---------------------------------------------------------------*
008600011015     C     CTRD01        BEGSR
008700011015     C*
008800080409     C                   Z-ADD     0             AMGDAY            8 0
009000080409     C* DATA
009100080409   B1C     V1CDAY        IFNE      0
009200080409     C                   Z-ADD     V1CDAY        G02DAT
009300011015     C                   MOVEL     *BLANK        G02ERR
009400011015     C                   CALL      'XSRDA8'
009500011015     C                   PARM                    G02DS
009600011015   B2C     G02ERR        IFEQ      '1'
009700011015     C                   MOVEL     MSG(1)        V1CMSG
009800011015     C                   SETON                                        4128
009900011015     C                   GOTO      ENDCTR
010000011015   E2C                   ENDIF
010100080409     C                   Z-ADD     G02INV        AMGDAY
010200011015     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
010300080409     C                   Z-ADD     G02DAT        V1CDAY
010400011015   E1C                   ENDIF
012800011015     C*
012900011015     C     ENDCTR        ENDSR
013000011015     C*---------------------------------------------------------------*
013100011015**  MSG
013200011015Data Errata                                                                    1
