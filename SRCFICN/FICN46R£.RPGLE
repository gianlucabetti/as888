000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400030113      *  Descrizione   :  elaborazione C.E. giornaliero COOP
000500020624      *                   Programma di lancio
000600000000      *****************************************************************
000700030113     Fficn38D   CF   E             WORKSTN
000800030205     ftntbe01l  if   e           k disk
000900000000      *
001000940926     D*
001100000000     D KPJBA         E DS
001200030205     d dccg          e ds
001300000000     D CNCR80        E DS
001400000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500020307     D dsparam         DS
001600020624     D  datadal                       8s 0
001700020624     D  dataal                        8s 0
001800940926     D*
001900940926     D*
002000000000     D WLBDAT          DS
002100940926     D  G02DAT                 1      8  0
002200940926     D  G02INV                 9     16  0
002300940926     D  G02ERR                17     17
002400940926     D  G02TGI                18     22  0
002500030205
002600030205     d dataiso         s               d   datfmt(*iso)
002700030205     d dataeur         s               d   datfmt(*eur)
002800000000      *
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100030205     c     ktbe          klist
003200030205     c                   kfld                    tbecod
003300030205     c                   kfld                    tbeke1
003400030205      *
003500030205     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
003600030205     C***
003700030205     C                   TIME                    W0120            14 0
003800030205     C                   MOVE      W0120         WDAT              8 0
003900030205     C*
004000030205     C                   Z-ADD     WDAT          G02DAT
004100030205     C                   MOVEL     *BLANK        G02ERR
004200030205     C                   CALL      'XSRDA8'
004300030205     C                   PARM                    WLBDAT
004400030205     C* UDATE A 8 IN AAAA/MM/GG
004500030205     C                   move      G02INV        dataiso
004600030205      *reperisco limiti da tabella
004700030205     c                   movel(p)  '1'           tbeke1
004800030205     c                   movel     'CCG'         tbecod
004900030205     c     ktbe          chain     tntbe01l                           90
005000030205     c                   if        not *in90
005100030205     c                   movel     tbeuni        dccg
005200030205      *tolgo giorni di tabella da udate
005300030205     c     dataiso       subdur    §cggus:*d     dataiso
005400030205     c                   move      dataiso       dataal
005500030205     c                   move      dataal        dataiso
005600030205     c                   move      dataiso       dataeur
005700030205     C                   move      dataeur       VIDAL                          DATA INIZIALE
005800030205      *tolgo giorni di tabella da data calcolata per primo limite
005900030205     c     dataiso       subdur    §cggls:*d     dataiso
006000030205     c                   move      dataiso       datadal
006100030205     c                   move      dataiso       dataeur
006200030205     C                   move      dataeur       VIDdAL                         DATA INIZIALE
006300030205     c                   endif
006400000000      *
006500000000      *--------------------------------------------------------------*
006600000000      *             M A I N      L I N E
006700000000      *--------------------------------------------------------------*
006800000000     C                   Z-ADD     1             CODUT
006900000000     C                   CALL      'X§PARUT'
007000000000     C                   PARM                    UT§DSE
007100000000     C                   MOVEL     RAGUT         RSUT             20
007200000000     C                   MOVEL     RAGUT         RSUT2            20
007300000000     C                   MOVEL     REC80         CNCR80
007400000000      *
007500000000      * Emetto il formato video
007600000000     C     EMETTO        TAG
007700020308     C                   SETOFF                                       90
007800030113     C                   EXFMT     cn38D01
007900000000      *
008000000000     C   KC              GOTO      FINE                                         F3=USCITA
008100000000      *
008200000000      * Eseguo i  controlli sui campi immessi a video
008300000000     C                   EXSR      CONTR
008400000000      *
008500020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008600020308     C  N90
008700020308     CanNkf              SETON                                          90      F6=CONFERMA
008800000000      *
008900000000      * Per errore generico riemetto il formato video
009000000000     C   90              GOTO      EMETTO
009100000000      *
009200030113      * Richiamo programma di elaborazione saldi
009300020308     c                   if        *inkf
009400020624     c                   z-add     dalribal      dataal
009500020624     c                   z-add     dalrib        datadal
009600020307     c                   movel     dsparam       kpjbu
009700030130     c                   move      'FN3R'        kcoaz
009800020626     c                   call      'BCH10'
009900020624     c                   parm                    kpjba
010000030113     c                   endif
010100000000      *
010200000000     C     FINE          TAG
010300000000     C                   SETON                                        LR
010400000000      *--------------------------------------------------------------*
010500000000      * Routine per i controlli sui campi del formato video          *
010600000000      *--------------------------------------------------------------*
010700000000     C     CONTR         BEGSR
010800000000      *
010900020308     c                   movea     '000000'      *in(41)
011000000000      *
011100020624      * controllo immissione delle date
011200020307     C     VIDDAL        ifeq      0
011300020308     C                   SETON                                        4390
011400000000     C                   GOTO      ENDCTL
011500000000     C                   END
011600020624     C     VIDal         ifeq      0
011700020624     C                   SETON                                        4190
011800020624     C                   GOTO      ENDCTL
011900020624     C                   END
012000000000      * Controllo data iniziale
012100000000     C     VIDDAL        IFGT      0
012200940926     C                   Z-ADD     VIDDAL        G02DAT
012300000000     C                   MOVEL     *BLANK        G02ERR
012400940926     C                   CALL      'XSRDA8'
012500000000     C                   PARM                    WLBDAT
012600000000     C     G02ERR        IFEQ      '1'
012700020308     C                   SETON                                        44  90
012800000000     C                   GOTO      ENDCTL
012900000000     C                   ENDIF
013000020307     C                   Z-ADD     G02INV        DALRIB            8 0
013100020308     C                   Z-ADD     G02dat        viddal
013200020307     c                   endif
013300020624      * Controllo data finale
013400020624     C     VIDal         IFGT      0
013500020624     C                   Z-ADD     VIDal         G02DAT
013600020624     C                   MOVEL     *BLANK        G02ERR
013700020624     C                   CALL      'XSRDA8'
013800020624     C                   PARM                    WLBDAT
013900020624     C     G02ERR        IFEQ      '1'
014000020624     C                   SETON                                        42  90
014100020624     C                   GOTO      ENDCTL
014200020624     C                   ENDIF
014300020624     C                   Z-ADD     G02INV        DALRIBal          8 0
014400020624     C                   Z-ADD     G02dat        vidal
014500020624     c                   endif
014600020624     C     dalrib        ifgt      dalribal
014700020624     C                   SETON                                        4590
014800020624     C                   GOTO      ENDCTL
014900020624     C                   END
015000000000      *
015100000000     C     ENDCTL        ENDSR
