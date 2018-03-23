000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200941202     H* FNLG87R *----------------------------------------------------*
000300900418     H*                                                              *
000400941202     H*   - STAMPA ELENCO MATERIALE X MAGAZZINO GIACENZE  -FILTRO-   *
000500900423     H*                                                              *
000600900418     H*--------------------------------------------------------------*
000700941202     FFNLG87D   CF   E             WORKSTN
000800941202      *
000900941202     D WLBDA8          DS
001000941202     D  G08DAT                 1      8  0
001100941202     D  G08INV                 9     16  0
001200941202     D  G08ERR                17     17
001300941202     D  G08TGI                18     22  0
001400941202      *
001500941202     D                SDS
001600941202     D  §PGM                   1     10
001700921012      *
001800921012     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001900941202      *
002000941202     D KPJBA         E DS
002100941202      *
002200941202     D FNLG87        E DS                  EXTNAME(FNLG87DS)
002300920522     C*
002400920522     C     *INKC         DOWEQ     '0'
002500921012     C*
002600941202     C                   EXFMT     LG8701D
002700920522     C  NKC              DO
002800920522     C                   SETOFF                                       99
002900920522     C* CONTROLLO DATA
003000941202     C     VDATA         IFNE      *ZEROS
003100941202     C     VRIST         IFEQ      'R'
003200941202     C                   Z-ADD     VDATA         G08DAT
003300941202     C                   MOVE      *ZEROS        G08INV
003400941202     C                   MOVEL     *BLANK        G08ERR
003500941202     C                   CALL      'XSRDA8'
003600941202     C                   PARM                    WLBDA8
003700941202     C     G08ERR        COMP      '1'                                    10
003800941202     C   10              SETON                                        1499
003900941202     C  N10              Z-ADD     G08DAT        VDATA
004000930519     C                   ELSE
004100930519     C* SE IMMESSA LA DATA CI VUOLE "R"
004200930519     C                   SETON                                        1699
004300941202     C                   END
004400941202     C                   ELSE
004500930519     C* SE IMMESSA "R" CI VUOLE LA DATA
004600941202     C     VRIST         IFEQ      'R'
004700930519     C                   SETON                                        1599
004800941202     C                   END
004900941202     C                   END
005000920522     C*  SE NON CI SONO ERRORI SOTTOMETTO LA STAMPA
005100920522     C  N99
005200920522     CAN KF              DO
005300941202     C                   MOVEL     VSEL          D87TST
005400941202     C                   MOVEL     VRIST         D87RIS
005500941202     C     VRIST         IFEQ      'R'
005600941202     C                   Z-ADD     G08INV        D87DAT
005700941202     C                   ELSE
005800941202     C                   MOVE      *ZEROS        D87DAT
005900941202     C                   ENDIF
006000050322     C                   MOVE      'S'           D87COMm
006100941202     C                   MOVEL     FNLG87        KPJBU
006200141216     C                   CALL      'FNLG86C'
006300920522     C                   PARM                    KPJBA
006400920522     C                   END
006500920522     C                   END
006600921012     C                   END
006700920522     C*
006800920522     C                   SETON                                        LR
006900941202     C*---------------------------------------------------------------*
007000941202     C     *INZSR        BEGSR
007100941202     C     *ENTRY        PLIST
007200941202     C                   PARM                    KPJBA
007300941202     C                   Z-ADD     1             CODUT
007400941202     C                   CALL      'X§PARUT'
007500941202     C                   PARM                    UTEDSE
007600941202     C*          CODUT     CABEQ-1        FINE
007700941202     C                   MOVEL     RAGUT         RSUT             20
007800941202     C*----------------------------------------------------*
007900941202     C                   Z-ADD     0             VDATA
008000941202     C                   MOVEL     '1'           VSEL
008100941202     C                   CLEAR                   FNLG87
008200941202     C                   ENDSR
