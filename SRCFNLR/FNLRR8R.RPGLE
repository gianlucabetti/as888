000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400041119      *  Descrizione   :  rielaborazione rientri
000500020624      *                   Programma di lancio
000600000000      *****************************************************************
000700041119     Ffnlrr8D   CF   E             WORKSTN
000800000000      *
000900940926     D*
001000000000     D KPJBA         E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300020307     D dsparam         DS
001400020624     D  datadal                       8s 0
001500020624     D  dataal                        8s 0
001501041119     D  filges                        3s 0
001600940926     D*
001700940926     D*
001800000000     D WLBDAT          DS
001900940926     D  G02DAT                 1      8  0
002000940926     D  G02INV                 9     16  0
002100940926     D  G02ERR                17     17
002200940926     D  G02TGI                18     22  0
002300000000      *
002400000000     C     *ENTRY        PLIST
002500000000     C                   PARM                    KPJBA
002600000000      *
002700000000      *--------------------------------------------------------------*
002800000000      *             M A I N      L I N E
002900000000      *--------------------------------------------------------------*
003000000000     C                   Z-ADD     1             CODUT
003100000000     C                   CALL      'X§PARUT'
003200000000     C                   PARM                    UT§DSE
003300000000     C                   MOVEL     RAGUT         RSUT             20
003400000000     C                   MOVEL     RAGUT         RSUT2            20
003500000000     C                   MOVEL     REC80         CNCR80
003600000000      *
003700000000      * Inizializzo i campi a video
003800020624     C                   Z-ADD     *ZERO         VIDal                          COD.FILIALE
003900000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
004000041119     c                   move      simpou        vfil
004001041119      * solo se profilo edp può variare il po altrimenti ognuno il suo
004002041119     c                   if        %subst(knmus:1:3) <> 'EDP'
004003041119     c                   seton                                        20
004004041119     c                   endif
004100000000      * Emetto il formato video
004200000000     C     EMETTO        TAG
004300020308     C                   SETOFF                                       90
004400041119     C                   EXFMT     lrr8D01
004500000000      *
004600000000     C   KC              GOTO      FINE                                         F3=USCITA
004700000000      *
004800000000      * Eseguo i  controlli sui campi immessi a video
004900000000     C                   EXSR      CONTR
005000000000      *
005100020308      * Se non vi sono errori ma non e' premuto F6 emetto video
005200020308     C  N90
005300020308     CanNkf              SETON                                          90      F6=CONFERMA
005400000000      *
005500000000      * Per errore generico riemetto il formato video
005600000000     C   90              GOTO      EMETTO
005700000000      *
005800020307      * Richiamo programma di stampa
005900020308     c                   if        *inkf
006000020624     c                   z-add     dalribal      dataal
006100020624     c                   z-add     dalrib        datadal
006101041119     c                   z-add     vfil          filges
006200020307     c                   movel     dsparam       kpjbu
006400041119      *richiamo sql pulizia rientri
006500041119     c                   call      'FNLRR8R1'
006600020624     c                   parm                    kpjba
007400020624     C                   GOTO      FINE                                         F3=USCITA
007500020308     c                   endif
007600000000      *
007700000000     C     FINE          TAG
007800000000     C                   SETON                                        LR
007900000000      *--------------------------------------------------------------*
008000000000      * Routine per i controlli sui campi del formato video          *
008100000000      *--------------------------------------------------------------*
008200000000     C     CONTR         BEGSR
008300000000      *
008400020308     c                   movea     '000000'      *in(41)
008500000000      *
008600020624      * controllo immissione delle date
008700020307     C     VIDDAL        ifeq      0
008800020308     C                   SETON                                        4390
008900000000     C                   GOTO      ENDCTL
008901041119     c                   else
008902041119     c     vidal         ifeq      0
008903041119     c                   move      viddal        vidal
008904041119     c                   endif
009000000000     C                   END
009001041119     c
009100020624     C     VIDal         ifeq      0
009200020624     C                   SETON                                        4190
009300020624     C                   GOTO      ENDCTL
009400020624     C                   END
009500000000      * Controllo data iniziale
009600000000     C     VIDDAL        IFGT      0
009700940926     C                   Z-ADD     VIDDAL        G02DAT
009800000000     C                   MOVEL     *BLANK        G02ERR
009900940926     C                   CALL      'XSRDA8'
010000000000     C                   PARM                    WLBDAT
010100000000     C     G02ERR        IFEQ      '1'
010200020308     C                   SETON                                        44  90
010300000000     C                   GOTO      ENDCTL
010400000000     C                   ENDIF
010500020307     C                   Z-ADD     G02INV        DALRIB            8 0
010600020308     C                   Z-ADD     G02dat        viddal
010700020307     c                   endif
010800020624      * Controllo data finale
010900020624     C     VIDal         IFGT      0
011000020624     C                   Z-ADD     VIDal         G02DAT
011100020624     C                   MOVEL     *BLANK        G02ERR
011200020624     C                   CALL      'XSRDA8'
011300020624     C                   PARM                    WLBDAT
011400020624     C     G02ERR        IFEQ      '1'
011500020624     C                   SETON                                        42  90
011600020624     C                   GOTO      ENDCTL
011700020624     C                   ENDIF
011800020624     C                   Z-ADD     G02INV        DALRIBal          8 0
011900020624     C                   Z-ADD     G02dat        vidal
012000020624     c                   endif
012100020624     C     dalrib        ifgt      dalribal
012200020624     C                   SETON                                        4590
012300020624     C                   GOTO      ENDCTL
012400020624     C                   END
012500000000      *
012600000000     C     ENDCTL        ENDSR
