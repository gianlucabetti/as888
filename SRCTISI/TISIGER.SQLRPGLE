000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200150205      **********************************************************************
000300150205      * TISIGER invio e/c bancali al cliente
000400150205      **********************************************************************
000500040820     d KPJBA         E DS
000600150210     D*
000700150205     d azCGBds       E DS                  extname(AZCGB00F)
000800150205     d  cgb_RRN        s              4B 0
000900040820     D*
001000040820     D WLBDAT          DS
001100040820     D  G02DAT                 1      8  0
001200040820     D  G02INV                 9     16  0
001300040820     D  G02ERR                17     17
001400040820     D  G02TGI                18     22  0
001500040820      *
001600040820     D  data_Oggi      s                   like(G02INV) INZ(0)
001700040820      *
001800040826     D  almeno_uno     s              1
001900150211      * ?------------------------------------------------------------------ */
002000150211      * invio Mail al posto del FAX
002100150211     D trul44ds      E DS
002200150211     D trtcm1ds      E DS
002300150211     D*------------------
002400150211     D* DS REPERIMENTO DATI UTENTE
002500150211     D*-------------------
002600150211     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
002700150211     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
002800150211     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
002900150211     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
003000150211     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
003100150205      * ?------------------------------------------------------------------ */
003200150205     D Digits          C                   CONST('0123456789')
003300150205      * ?------------------------------------------------------------------ */
003400150206     C     *ENTRY        PLIST
003500150206     C                   PARM                    KPJBA
003600150206      *
003700150206     C                   TIME                    W0140            14 0
003800150206      *  imposta UDATE di elaborazione
003900150206     C                   move      *date         G02DAT
004000150206     C                   MOVE      *ZEROS        G02INV
004100150206     C                   MOVE      *BLANKS       G02ERR
004200150206     C                   CALL      'XSRDA8'
004300150206     C                   PARM                    WLBDAT
004400150206     C                   Z-ADD     G02INV        data_oggi
004500150206      **
004600150206     c                   clear                   almeno_uno
004700150210      **
004800150209      ** -----------  descrizione SQL:
004900150209      ** record da leggere dall'Anagrafica Clienti Bancali cod.UNIFICANTE
005000150209      ** sono quelli che:
005100150209      **   NON sono esclusi volutamente dall'invio della mail ->(CGBFRE < 999)
005200150209      **    (AND)
005300150209      **   SONO clienti attivi (nel range di Inizio Fine attività rispetto all'UDATE)
005400150209      **    (AND)
005500150209      ** ( Sono stati appena inseriti: la data invio ultima mail=(0) (CGBDTE=0)
005600150209      **    (OR)
005700150209      **   Se sono trascorsi i giorni (CGBFRE) necessari x l'invio di una nuova mail:
005800150209      **   confronta se la data risultante fra la differenza della UDATE  meno i GG. di
005900150209      **   "intervallo mail" è uguale o superiore alla data dell'ultima mail inviata
006000150209      ** )
006100150209      ** -----------
006200011210     C/EXEC SQL
006300150206     C+ Declare ANA_CGB Cursor for
006400150209     c+  select  azcgb00f.*,  RRN(azcgb00f)
006500150209     c+  from    azcgb00f
006600150209     c+  where   CGBFRE < 999
006700150209     c+  and
006800150209     c+   days(curdate()) between
006900150209     c+   days(DATE(TIMESTAMP_FORMAT(digits(CGBDTI), 'YYYYMMDD')))
007000150209     c+  and
007100150209     c+   days(DATE(TIMESTAMP_FORMAT(digits(CGBDTF), 'YYYYMMDD')))
007200150209     c+  and
007300150209     c+  (
007400150209     c+    CGBDTE=0
007500150209     c+  or
007600150209     c+   (days(curdate()) - CGBFRE) >=
007700150209     c+   days(DATE(TIMESTAMP_FORMAT(digits(CGBDTE), 'YYYYMMDD')))
007800150209     c+  )
007900150414     c+  order by cgbKSU
008000011210     C/END-EXEC
008100011210
008200011210     C/EXEC SQL
008300150205     C+ OPEN ANA_CGB
008400011210     C/END-EXEC
008500040820
008600040820     C                   DOU       SqlCod <> 0
008700011205     C/EXEC SQL
008800150211     C+ FETCH NEXT FROM ANA_CGB INTO :azCGBds, :cgb_RRN
008900011205     C/END-EXEC
009000011210     C                   SELECT
009100041007     c*
009200041007     c* a fine file Totali x rotture
009300041007     C                   WHEN      SqlCod = 100
009400041007     c                   if        almeno_uno ='S'
009500041007     c                   else
009600150205     c                   exsr      NO_dati
009700041007     c                   end
009800041007     c                   leave
009900041007     **
010000041007     C                   WHEN      SqlCod < 0
010100041007     C                   seton                                        H1
010200041007     c                   goto      fine
010300040820     c*
010400041007     C                   OTHER
010500040820     c* DETTAGLIO
010600150205     c                   move      'S'           almeno_uno
010700150205     c                   exsr      Dettaglio
010800040823      *
010900011210     C                   ENDSL
011000040820
011100011205     C                   ENDDO
011200011205     C/EXEC SQL
011300150205     C+ CLOSE ANA_CGB
011400011205     C/END-EXEC
011500011205     c*
011600150209      * totali generali
011700041007     c                   if        almeno_uno ='S'
011800041007     c                   end
011900040823      *
012000041007     c     fine          tag
012100020326     C                   SETON                                        LR
012200150206      * ?------------------------------------------------------------------ */
012300150210      *  per ogni anagrafica selezionata richiama il pgm di lista E/C
012400150206      * ?------------------------------------------------------------------ */
012500150205     C     Dettaglio     begSR
012600150211      **
012700150211      **  Esegue OVRPRTF x lancio E-mail
012800150211     c                   exsr      OVRPRTF_x_mail
012900150206      **
013000150210      **    Chiama il programma passando il RRN dell'anagrafica
013100150210      **  x eseguire E/C dei movimenti ed invio MAIL con saldo
013200150206     C                   z-add     CGB_rrn       parmCGBrrn        9 0
013300150210     C                   movel     parmCGBrrn    kpjbu
013400150216     c                   call      'TISIGER1'
013500150210     c                   parm                    kpjba
013600150209      *
013700040820     C                   ENDSR
013800150209      * ?------------------------------------------------------------------ */
013900150209      *
014000150209      * ?------------------------------------------------------------------ */
014100150209     C     NO_dati       begSR
014200150209      **
014300150209      **
014400150209     C                   ENDSR
014500150206      * ?------------------------------------------------------------------ */
014600150206     C* INIZIALIZZAZIONE
014700150206      * ?------------------------------------------------------------------ */
014800150206     C     *inzsr        BEGSR
014900150206     c*
015000150206     C                   ENDSR
015100150205      * ?------------------------------------------------------------------ */
015200150211     C*  Esegue OVRPRTF x lancio E-mail
015300150211      * ?------------------------------------------------------------------ */
015400150211     C     OVRPRTF_x_mailBEGSR
015500150211     C*
015600150211     C* REPERISCE la Rag.Sociale
015700150211     C                   clear                   desCLI           45
015800150211     C                   CLEAR                   BS69DS
015900150211     C                   CLEAR                   ACODS
016000150211     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
016100150211     C                   MOVE      cgbKSU        I69KAC                         *CLIENTE X CNCLP
016200150211     C                   CALL      'TIBS69R'
016300150211     C                   PARM                    BS69DS
016400150211     C                   PARM                    ACODS
016500150211     C                   PARM                    INDDS
016600150211     C                   PARM                    CLPDS
016700150211     C                   PARM                    CLSDS
016800150211IF  1C     O69ERR        IFNE      '1'                                          *ERRORE
016900150211     C                   movel     ACORAG        desCLI           45            *ragione sociale
017000150211E   1C                   ENDIF
017100150211     C*
017200150211     C*  Esegue  OVRPRTF x invio coda E-mail
017300150211     C*   con tutti i parametri tramite il TRUL44R
017400150211     C*
017500150211     c                   clear                   trul44ds
017600150211     c                   clear                   trtcm1ds
017700150211      *
017800150216     c                   eval      d44pgm = 'TISIGER1'
017900150211     c                   move      cgbKSU        d44ksc
018000150211     c                   eval      d44apl = 'C'
018100150211     c                   eval      d44nk1 = '0151' + %editc(d44ksc:'X')
018200150211     c                   eval      d44scope='*JOB'
018300150211     c                   eval      d44ovr = 'S'
018400150211     c                   eval      d44obj = '*OBJM*Estratto Conto Bancali C-
018500150211     c                             liente: ' + DESCLI
018600150216     c                   eval      d44prtf = 'TISIGEP'
018700150211     c                   eval      D44DES  = CGBMAIL
018800150211     c                   call      'TRUL44R'
018900150211     c                   parm                    trul44ds
019000150211     c                   parm                    trtcm1ds
019100150211      *
019200150211      **  Se c'è stato qualche problema
019300150211     c     d44err        ifne      *blank
019400150211     C**
019500150211     C**
019600150211     c                   endIF
019700150211     C*
019800150211     C                   ENDSR
019900150211      * ?------------------------------------------------------------------ */
