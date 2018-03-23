000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400081002      *  Descrizione   :  Stampa tariffe aff/def
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700090430     Ftrmzcntd  CF   E             WORKSTN
000800000000      *
000900940926     D*
001000000000     D KPJBA         E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300020307     D dsparam         DS
001400090430     D  parnrc                        7s 0
001500090423     D  pardat                        8s 0
001600090423     d dataiso         s               d   datfmt(*iso)
001700090423     d udateiso        s               d   datfmt(*iso)
001800090423     d dataeur         s               d   datfmt(*eur)
001900090423     D WLBDA8          DS
002000090423     D  G02DAT                 1      8  0
002100090423     D  G02INV                 9     16  0
002200090423     D  G02ERR                17     17
002300090423     D  G02TGI                18     22  0
002400060320      *__________________________________________________________________
002500000000     C     *ENTRY        PLIST
002600000000     C                   PARM                    KPJBA
002700090423     C                   time                    w0120            14 0
002800090423     C                   move      w0120         wdat              8 0
002900090423      * data da impostare su data stampa e convalida se lancio = 'C'
003000090423     c                   move      wdat          dataeur
003100090423     c                   move      dataeur       udateiso
003200090424     c                   move      dataeur       dataiso
003300090424     c                   move      wdat          viddat
003400090423      *
003500090408     C                   SETON                                        LR
003600000000      *
003700000000      *--------------------------------------------------------------*
003800000000      *             M A I N      L I N E
003900000000      *--------------------------------------------------------------*
004000000000     C                   Z-ADD     1             CODUT
004100000000     C                   CALL      'X§PARUT'
004200000000     C                   PARM                    UT§DSE
004300000000     C                   MOVEL     RAGUT         RSUT             20
004400000000     C                   MOVEL     RAGUT         RSUT2            20
004500000000     C                   MOVEL     REC80         CNCR80
004600000000      *
004700090430      * controllo edp
004800090430     c                   if        %subst(knmus:1:3) <>'EDP'
004900090408     c                   return
005000090408     c                   endif
005100000000      * Inizializzo i campi a video
005200090430     C                   Z-ADD     *ZERO         VIDnrc
005300000000      *
005400000000      * Emetto il formato video
005500060324     c                   do        *hival
005600000000     C     EMETTO        TAG
005700020308     C                   SETOFF                                       90
005800090430     C                   EXFMT     mzcntr1
005900000000      *
006000000000     C   KC              GOTO      FINE                                         F3=USCITA
006100000000      *
006200000000      * Eseguo i  controlli sui campi immessi a video
006300000000     C                   EXSR      CONTR
006400000000      *
006500020308      * Se non vi sono errori ma non e' premuto F6 emetto video
006600020308     C  N90
006700020308     CanNkf              SETON                                          90      F6=CONFERMA
006800000000      *
006900000000      * Per errore generico riemetto il formato video
007000000000     C   90              GOTO      EMETTO
007100000000      *
007200020307      * Richiamo programma di stampa
007300020308     c                   if        *inkf
007400090430     c                   move      vidnrc        parnrc
007500090428     c                   move      dataiso       pardat
007600020307     c                   movel     dsparam       kpjbu
007700090430     C                   CALL      'TRMZCNTR1'
007800000000     C                   PARM                    KPJBA
007900090114     c                   goto      fine
008000020308     c                   endif
008100060324     c                   enddo
008200000000      *
008300000000     C     FINE          TAG
008400090408     c                   return
008500000000      *--------------------------------------------------------------*
008600000000      * Routine per i controlli sui campi del formato video          *
008700000000      *--------------------------------------------------------------*
008800000000     C     CONTR         BEGSR
008900000000      *
009000090423     C                   SETOff                                       454647
009100090428     C                   SETOff                                       4841
009200090423      * controlli data certa
009300090424     c                   if        viddat <> wdat
009400090424     C                   MOVE      Viddat        G02DAT
009500090423     C                   MOVEL     *BLANK        G02ERR
009600090423     C                   CALL      'XSRDA8'
009700090423     C                   PARM                    WLBDA8
009800090423     C     G02ERR        IFEQ      '1'
009900090423     C                   SETON                                        4690
010000090423     c                   leavesr
010100090423     C                   END
010200090424     c                   move      g02dat        viddat
010300090423     c                   move      g02inv        dataiso
010400090423     c                   endif
010500090423      * data certa minore di udate non possibile
010600090423     c                   if        dataiso  < udateiso
010700090423     C                   SETON                                        4790
010800090423     c                   leavesr
010900090423     c                   endif
011000000000      *
011100090428      * data certa maggiore di udate di 7 giorni
011200090423     c     dataiso       subdur    udateiso      giorni:*d         5 0
011300090423     C                   if        giorni > 7
011400090423     C                   SETON                                        4890
011500090423     c                   leavesr
011600090423     c                   endif
011700090428     c   41              seton                                        90
011800090423      *
011900000000     C     ENDCTL        ENDSR
