000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400030108      *  Descrizione   :  Valorizzazione coop
000500020624      *                   Programma di lancio
000600000000      *****************************************************************
000700030108     Fficn35D   CF   E             WORKSTN
000800030204     ftntbe01l  if   e           k disk
000900000000      *
001000940926     D*
001100000000     D KPJBA         E DS
001200030204     d dccg          e ds
001300000000     D CNCR80        E DS
001400000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500020307     D dsparam         DS
001600020624     D  datadal                       8s 0
001700020624     D  dataal                        8s 0
001800030108     D  parpo                         3s 0
001900940926     D*
002000000000     D WLBDAT          DS
002100940926     D  G02DAT                 1      8  0
002200940926     D  G02INV                 9     16  0
002300940926     D  G02ERR                17     17
002400940926     D  G02TGI                18     22  0
002500030204
002600030204     d dataiso         s               d   datfmt(*iso)
002700030204     d dataeur         s               d   datfmt(*eur)
002800000000      *
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100030204     c     ktbe          klist
003200030204     c                   kfld                    tbecod
003300030204     c                   kfld                    tbeke1
003400000000      *
003500030204     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
003600030204     C***
003700030204     C                   TIME                    W0120            14 0
003800030204     C                   MOVE      W0120         WDAT              8 0
003900030204     C*
004000030204     C                   Z-ADD     WDAT          G02DAT
004100030204     C                   MOVEL     *BLANK        G02ERR
004200030204     C                   CALL      'XSRDA8'
004300030204     C                   PARM                    WLBDAT
004400030204     C* UDATE A 8 IN AAAA/MM/GG
004500030204     C                   move      G02INV        dataiso
004600030204      *reperisco limiti da tabella
004700030204     c                   movel(p)  '1'           tbeke1
004800030204     c                   movel     'CCG'         tbecod
004900030204     c     ktbe          chain     tntbe01l                           90
005000030204     c                   if        not *in90
005100030204     c                   movel     tbeuni        dccg
005200030204      *tolgo giorni di tabella da udate
005300030204     c     dataiso       subdur    §cgguv:*d     dataiso
005400030204     c                   move      dataiso       dataal
005500030204     c                   move      dataal        dataiso
005600030204     c                   move      dataiso       dataeur
005700030204     C                   move      dataeur       VIDAL                          DATA INIZIALE
005800030204      *tolgo giorni di tabella da data calcolata per primo limite
005900030204     c     dataiso       subdur    §cgglv:*d     dataiso
006000030204     c                   move      dataiso       datadal
006100030204     c                   move      dataiso       dataeur
006200030204     C                   move      dataeur       VIDdAL                         DATA INIZIALE
006300030204     c                   endif
006400000000      *--------------------------------------------------------------*
006500000000      *             M A I N      L I N E
006600000000      *--------------------------------------------------------------*
006700000000     C                   Z-ADD     1             CODUT
006800000000     C                   CALL      'X§PARUT'
006900000000     C                   PARM                    UT§DSE
007000000000     C                   MOVEL     RAGUT         RSUT             20
007100000000     C                   MOVEL     RAGUT         RSUT2            20
007200000000     C                   MOVEL     REC80         CNCR80
007300000000      *
007400000000      * Inizializzo i campi a video
007500030108     c                   z-add     *zero         vsinpo
007600000000      * Emetto il formato video
007700000000     C     EMETTO        TAG
007800020308     C                   SETOFF                                       90
007900030108     C                   EXFMT     cn35D01
008000000000      *
008100000000     C   KC              GOTO      FINE                                         F3=USCITA
008200000000      *
008300000000      * Eseguo i  controlli sui campi immessi a video
008400000000     C                   EXSR      CONTR
008500000000      *
008600020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008700020308     C  N90
008800020308     CanNkf              SETON                                          90      F6=CONFERMA
008900000000      *
009000000000      * Per errore generico riemetto il formato video
009100000000     C   90              GOTO      EMETTO
009200000000      *
009300020307      * Richiamo programma di stampa
009400020308     c                   if        *inkf
009500020624     c                   z-add     dalribal      dataal
009600020624     c                   z-add     dalrib        datadal
009700030108     c                   z-add     vsinpo        parpo
009800020307     c                   movel     dsparam       kpjbu
009900030108     c                   move      'FN36'        kcoaz
010000020624      *richiamo sql pulizia saldi e aggiornamento flag FCE di record saldato
010100020626     c                   call      'BCH10'
010200020624     c                   parm                    kpjba
010300020308     c                   endif
010400000000      *
010500000000     C     FINE          TAG
010600000000     C                   SETON                                        LR
010700000000      *--------------------------------------------------------------*
010800000000      * Routine per i controlli sui campi del formato video          *
010900000000      *--------------------------------------------------------------*
011000000000     C     CONTR         BEGSR
011100000000      *
011200020308     c                   movea     '000000'      *in(41)
011300000000      *
011400020624      * controllo immissione delle date
011500020307     C     VIDDAL        ifeq      0
011600020308     C                   SETON                                        4390
011700000000     C                   GOTO      ENDCTL
011800000000     C                   END
011900020624     C     VIDal         ifeq      0
012000020624     C                   SETON                                        4190
012100020624     C                   GOTO      ENDCTL
012200020624     C                   END
012300000000      * Controllo data iniziale
012400000000     C     VIDDAL        IFGT      0
012500940926     C                   Z-ADD     VIDDAL        G02DAT
012600000000     C                   MOVEL     *BLANK        G02ERR
012700940926     C                   CALL      'XSRDA8'
012800000000     C                   PARM                    WLBDAT
012900000000     C     G02ERR        IFEQ      '1'
013000020308     C                   SETON                                        44  90
013100000000     C                   GOTO      ENDCTL
013200000000     C                   ENDIF
013300020307     C                   Z-ADD     G02INV        DALRIB            8 0
013400020308     C                   Z-ADD     G02dat        viddal
013500020307     c                   endif
013600020624      * Controllo data finale
013700020624     C     VIDal         IFGT      0
013800020624     C                   Z-ADD     VIDal         G02DAT
013900020624     C                   MOVEL     *BLANK        G02ERR
014000020624     C                   CALL      'XSRDA8'
014100020624     C                   PARM                    WLBDAT
014200020624     C     G02ERR        IFEQ      '1'
014300020624     C                   SETON                                        42  90
014400020624     C                   GOTO      ENDCTL
014500020624     C                   ENDIF
014600020624     C                   Z-ADD     G02INV        DALRIBal          8 0
014700020624     C                   Z-ADD     G02dat        vidal
014800020624     c                   endif
014900020624     C     dalrib        ifgt      dalribal
015000020624     C                   SETON                                        4590
015100020624     C                   GOTO      ENDCTL
015200020624     C                   END
015300000000      *
015400000000     C     ENDCTL        ENDSR
