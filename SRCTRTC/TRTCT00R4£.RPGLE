000100110520      **?__________________________________________________________________ */
000200110520      *
000300110524      *  Scrive il file EDISTD80F che ha un unico campo a 80 colonne
000400110520      **?__________________________________________________________________ */
000500110520      * Riceve:
000600110524      *  il segmento da importare sulle righe da 80 caratteri
000700110520      **?__________________________________________________________________ */
000800110524      *
000900110525     fEDISTD80F uF a E           k disk    INFDS(StdRECDS)
001000110524      *
001100110525      **?************************************************************************
001200110621     d  esito          s              1
001300110525      * numero relativo di record
001400110525     D StdRECDS        DS
001500110525     D  NRrec                397    400B 0
001600110525      *----------------------------------------------------*
001700110524      **?__________________________________________________________________ */
001800110524     D lung_segmento   s              5u 0
001900110525     D lung_libero     s              5u 0
002000110520      **?__________________________________________________________________ */
002100110524      * ?     Input
002200110524      **?------------------------------------------------------------------ */
002300050628     c     *entry        plist
002400110524     c                   parm                    segmento       1950
002500110525      *
002600110525     c                   z-add     1             da                5 0
002700110525     c                   z-add     80            a                 5 0
002800110525     c                   z-add     1             daChr             5 0
002900110525     c                   z-add     80            aChr              5 0
003000110520      *
003100110524      *  controlla quanto è lunga la stringa di caratteri da riportare
003200110524      *  quindi esegue un controllo sull'ultimo record scritto e verifica
003300110524      *  la capienza ossia lo spazio disponibile per scrivere tot caratteri
003400110524      *  del segmento i rimanenti li riporterà sul segmento successivo....e così
003500110524      *  andando avanti ogni riga conterrà sempre 80 caratteri.
003600110524      *
003700110525     c                   eval      lung_segmento = %len(%trimR(segmento))
003800110524      *
003900110525     c     *hival        setll     EDIstd80F
004000110524     c                   readP     EDIstd80F
004100110524      *
004200110525      * SE E'VUOTO e SI DEVE INIZIARE dal 1°RECORD
004300110525      * --------------------------------------------
004400110525     c                   if        %Eof(EDIstd80F)     or
004500110525     c                             not %Eof(EDIstd80F) and
004600110525     c                                 %len(%trimR(RIGA80STD)) = 80
004700110525      * -----
004800110525     c                   z-add     1             numriga
004900110525      *
005000110525     c                   if        not %Eof(EDIstd80F)
005100110525     c                   add       NRrec         numriga
005200110525     c                   end
005300110525      *
005400110525     c                   exsr      Nuova_write
005500110524      * -----
005600110525     c                   elseIf    not %Eof(EDIstd80F) and
005700110525     c                                 %len(%trimR(RIGA80STD)) < 80
005800110525      * --------------------------------------------
005900110525      * SE SI DEVE CONTINUARE ad AGGIUNGERE records
006000110524      * -----
006100110525      *  lo spazio rimanente da riempire per arrivare a 80 nel campo
006200110525     c                   eval      lung_libero = (80 - %len(%trimR(RIGA80STD)))
006300110525      *
006400110525      * lo spazio x accodare il nuovo segmento
006500110525     c                   eval      DaChr = %len(%trimR(RIGA80STD)) + 1
006600110525      *
006700110525     c                   eval       aChr = lung_Libero
006800110525     c                   eval       a    = lung_Libero
006900110525      *
007000110525     c                   exsr      upd_record
007100110525      *
007200110525     c                   add       1             numriga
007300110525      *  quindi prosegue
007400110525     c                   eval       da = lung_Libero + 1
007500110525     c                   eval        a = 80
007600110525     c                   eval      DaChr =  1
007700110525     c                   eval       aChr =  80
007800110525      * -----
007900110525     c                   exsr      Nuova_write
008000110525      * -----
008100110524     c                   end
008200110520      **?__________________________________________________________________ */
008300110520     c                   seton                                        LR
008400110524      **?------------------------------------------------------------------ */
008500110525     c     Nuova_Write   BEGSR
008600110525      *
008700110525     c     NuovoRecord   tag
008800110525      * nuovi 80 caratteri
008900110525     c                   if        %subst(segmento:da:a) <> *blank
009000110525     c                   exsr      wrt_records
009100110525     c                   add       1             numriga
009200110525     c                   goto      NuovoRecord
009300110525     c                   end
009400110525      *
009500110525     c                   ENDSR
009600110525      **?------------------------------------------------------------------ */
009700110525     c     upd_record    BEGSR
009800110525      *
009900110525     c                   eval        %subst(riga80std:daChr:aChr)
010000110525     c                             = %subst(segmento:da:a)
010100110525      *
010200110525     c                   update    STANDARD80
010300110525      *
010400110525     c                   ENDSR
010500110525      **?------------------------------------------------------------------ */
010600110524     c     wrt_records   BEGSR
010700110524      *
010800110524     c                   eval        %subst(riga80std:daChr:aChr)
010900110524     c                             = %subst(segmento:da:a)
011000110524      *
011100110524     c                   write     STANDARD80
011200110525     c                   add       80            da
011300110524      *
011400110524     c                   ENDSR
011500110524      **?------------------------------------------------------------------ */
011600110621      * ?------------------------------------------------------------------ */
011700110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
011800110621      * ?------------------------------------------------------------------ */
011900110621     C     *pssr         BEGSR
012000110621     C
012100110621     C                   eval      esito = 'E'
012200110621     C                   ENDSR     '*CANCL'
012300110621     C
012400110621      *  ------------------------------------------------------------------ */
