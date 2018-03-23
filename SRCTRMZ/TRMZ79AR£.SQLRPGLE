000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400111025      *  Descrizione   :  Stampa contratti autisti per filiale
000500000000      *****************************************************************
000600111025     Ftrmz79ad  CF   E             WORKSTN
000700111025     Faitrs01l  if   e           k disk
000800111129     Faitrsa1l  if   e           k disk
000900000000      *
001000940926     D*
001100000000     D KPJBA         E DS
001200000000     D CNCR80        E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400111202     D aitrs         E DS                  EXTNAME(aitrs00F)
001500020307     D dsparam         DS
001600111025     D  parnrc                        7s 0 inz
001700111025     D  pardat                        8s 0 inz
001800090423     d dataiso         s               d   datfmt(*iso)
001900090423     d udateiso        s               d   datfmt(*iso)
002000090423     d dataeur         s               d   datfmt(*eur)
002100090423     D WLBDA8          DS
002200090423     D  G02DAT                 1      8  0
002300090423     D  G02INV                 9     16  0
002400090423     D  G02ERR                17     17
002500090423     D  G02TGI                18     22  0
002600060320      *__________________________________________________________________
002700111025      *             M A I N      L I N E
002800111025      *--------------------------------------------------------------*
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100090423     C                   time                    w0120            14 0
003200090423     C                   move      w0120         wdat              8 0
003300111129     c     ktrsa         klist
003400111129     c                   kfld                    trsnrc
003500111129     c                   kfld                    appen             1
003600111202     c                   move      '1'           appen
003700111025      * data da impostare su data stampa
003800090423     c                   move      wdat          dataeur
003900090423     c                   move      dataeur       udateiso
004000090424     c                   move      dataeur       dataiso
004100090424     c                   move      wdat          viddat
004200000000      *
004300000000     C                   Z-ADD     1             CODUT
004400000000     C                   CALL      'X§PARUT'
004500000000     C                   PARM                    UT§DSE
004600000000     C                   MOVEL     RAGUT         RSUT             20
004700000000     C                   MOVEL     RAGUT         RSUT2            20
004800000000     C                   MOVEL     REC80         CNCR80
004900000000      *
005000000000      * Inizializzo i campi a video
005100111025     C                   Z-ADD     *ZERO         VIDfil
005200000000      *
005300000000      * Emetto il formato video
005400060324     c                   do        *hival
005500000000     C     EMETTO        TAG
005600020308     C                   SETOFF                                       90
005700090430     C                   EXFMT     mzcntr1
005800000000      *
005900000000     C   KC              GOTO      FINE                                         F3=USCITA
006000000000      *
006100000000      * Eseguo i  controlli sui campi immessi a video
006200000000     C                   EXSR      CONTR
006300000000      *
006400020308      * Se non vi sono errori ma non e' premuto F6 emetto video
006500020308     C  N90
006600020308     CanNkf              SETON                                          90      F6=CONFERMA
006700000000      *
006800000000      * Per errore generico riemetto il formato video
006900000000     C   90              GOTO      EMETTO
007000000000      *
007100020307      * Richiamo programma di stampa
007200020308     c                   if        *inkf
007300111206     c*                  if        vidfil > 0
007400111202     c                   move      dataiso       pardat
007500111202     C/EXEC SQL
007600111206     C+ DECLARE A1 CURSOR FOR SELECT * FROM aitrs01l WHERE trsDsc > 0
007700111206     C+ AND trsdfc = 0 ORDER BY trsFil, trsSocG, trsIva
007800111202     C/END-EXEC
007900111202
008000111202     C/EXEC SQL
008100111202     C+ OPEN A1
008200111202     C/END-EXEC
008300111202
008400111206     C                   IF        sqlCode < 0
008500111206     C                   DUMP(A)
008600111206     C                   GOTO      fine
008700111206     C                   ENDIF
008800111206
008900111202     C                   dou       *inlr
009000111202
009100111202     C/EXEC SQL
009200111202     C+ FETCH NEXT FROM A1 INTO :aitrs
009300111202     C/END-EXEC
009400111202
009500111202     C                   select
009600111202     C                   when      sqlcod = 100
009700111202     C                   seton                                        lr
009800111206     C                   LEAVE
009900111206     C                   when      sqlcod < 0
010000111206     C                   DUMP(A)
010100111206     C                   LEAVE
010200111202     C                   other
010300111206     C                   exsr      verifica
010400111202     C                   endsl
010500111202     C*
010600111202     C                   enddo
010700111202
010800111202     C/EXEC SQL
010900111202     C+ CLOSE A1
011000111202     C/END-EXEC
011100111206     C                   GOTO      fine
011200111206     C
011300111124     c                   else
011400111124      *singolo contratto
011500111124     c     vidcon        chain     aitrs01l
011600111124     c                   if        %found(aitrs01l)
011700111124      *
011800111124     c                   if        trsdsc > 20111102 or trsdfc > 0
011900111124     c                   iter
012000111124     c                   endif
012100111124      *
012200111124     c                   move      trsnrc        parnrc
012300111124     c                   move      dataiso       pardat
012400111124     c                   movel     dsparam       kpjbu
012500111130     C                   CALL      'TRMZ79AR1'
012600111130     C                   PARM                    KPJBA
012700111124     c                   else
012800111124     c                   seton                                        4190
012900111124     c                   goto      emetto
013000111124     c                   endif
013100111206     c*                  endif
013200111025     c                   endif
013300111025     c                   enddo
013400000000      *
013500000000     C     FINE          TAG
013600111025     C                   SETON                                        LR
013700111202      *--------------------------------------------------------------*
013800111202     c     verifica      begsr
013900111202      *--------------------------------------------------------------*
014000111202      *già presente appendice "1" non ristampo
014100111202     c     ktrsa         chain     aitrsa1l
014200111202     c                   if        %found(aitrsa1l)
014300111206     c                   leavesr
014400111202     c                   endif
014500111202      *
014600111202     c                   if        trsdsc > 20111201 or trsdfc > 0
014700111206     c                   leavesr
014800111202     c                   endif
014900111202      *
015000111202     c                   move      trsnrc        parnrc
015100111202     c                   move      dataiso       pardat
015200111202     c                   movel     dsparam       kpjbu
015300111202     C                   CALL      'TRMZ79AR1'
015400111202     C                   PARM                    KPJBA
015500111202     c*
015600111202     c                   endsr
015700000000      *--------------------------------------------------------------*
015800000000      * Routine per i controlli sui campi del formato video          *
015900000000      *--------------------------------------------------------------*
016000000000     C     CONTR         BEGSR
016100000000      *
016200090423     C                   SETOff                                       454647
016300111124     C                   SETOff                                       484149
016400111206     c*                  if        vidfil = *zeros and vidcon = *zeros
016500111206     c*                  seton                                        4990
016600111206     c*                  leavesr
016700111206     c*                  endif
016800111206     c*                  if        vidfil > *zeros and vidcon > *zeros
016900111206     c*                  seton                                        4990
017000111206     c*                  leavesr
017100111206     c*                  endif
017200090423      * controlli data certa
017300090424     c                   if        viddat <> wdat
017400090424     C                   MOVE      Viddat        G02DAT
017500090423     C                   MOVEL     *BLANK        G02ERR
017600090423     C                   CALL      'XSRDA8'
017700090423     C                   PARM                    WLBDA8
017800090423     C     G02ERR        IFEQ      '1'
017900090423     C                   SETON                                        4690
018000090423     c                   leavesr
018100090423     C                   END
018200090424     c                   move      g02dat        viddat
018300090423     c                   move      g02inv        dataiso
018400090423     c                   endif
018500090423      * data certa minore di udate non possibile
018600090423     c                   if        dataiso  < udateiso
018700090423     C                   SETON                                        4790
018800090423     c                   leavesr
018900090423     c                   endif
019000000000      *
019100090428      * data certa maggiore di udate di 7 giorni
019200090423     c     dataiso       subdur    udateiso      giorni:*d         5 0
019300090423     C                   if        giorni > 7
019400090423     C                   SETON                                        4890
019500090423     c                   leavesr
019600090423     c                   endif
019700090423      *
019800000000     C     ENDCTL        ENDSR
