000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200110520      **?__________________________________________________________________ */
000300110520      *
000400110524      *  Scrive il file EDISTD80F che ha un unico campo a 80 colonne
000500110520      **?__________________________________________________________________ */
000600110520      * Riceve:
000700110524      *  il segmento da importare sulle righe da 80 caratteri
000800110520      **?__________________________________________________________________ */
000900110524      *
001000110525     fEDISTD80F uF a E           k disk    INFDS(StdRECDS)
001100110524      *
001200110525      **?************************************************************************
001300110621     d  esito          s              1
001400110525      * numero relativo di record
001500110525     D StdRECDS        DS
001600110525     D  NRrec                397    400B 0
001700110525      *----------------------------------------------------*
001800110524      **?__________________________________________________________________ */
001900110524     D lung_segmento   s              5u 0
002000110525     D lung_libero     s              5u 0
002100110520      **?__________________________________________________________________ */
002200110524      * ?     Input
002300110524      **?------------------------------------------------------------------ */
002400050628     c     *entry        plist
002500110524     c                   parm                    segmento       1950
002600110525      *
002700110525     c                   z-add     1             da                5 0
002800110525     c                   z-add     80            a                 5 0
002900110525     c                   z-add     1             daChr             5 0
003000110525     c                   z-add     80            aChr              5 0
003100110520      *
003200110524      *  controlla quanto è lunga la stringa di caratteri da riportare
003300110524      *  quindi esegue un controllo sull'ultimo record scritto e verifica
003400110524      *  la capienza ossia lo spazio disponibile per scrivere tot caratteri
003500110524      *  del segmento i rimanenti li riporterà sul segmento successivo....e così
003600110524      *  andando avanti ogni riga conterrà sempre 80 caratteri.
003700110524      *
003800110525     c                   eval      lung_segmento = %len(%trimR(segmento))
003900110524      *
004000110525     c     *hival        setll     EDIstd80F
004100110524     c                   readP     EDIstd80F
004200110524      *
004300110525      * SE E'VUOTO e SI DEVE INIZIARE dal 1°RECORD
004400110525      * --------------------------------------------
004500110525     c                   if        %Eof(EDIstd80F)     or
004600110525     c                             not %Eof(EDIstd80F) and
004700110525     c                                 %len(%trimR(RIGA80STD)) = 80
004800110525      * -----
004900110525     c                   z-add     1             numriga
005000110525      *
005100110525     c                   if        not %Eof(EDIstd80F)
005200110525     c                   add       NRrec         numriga
005300110525     c                   end
005400110525      *
005500110525     c                   exsr      Nuova_write
005600110524      * -----
005700110525     c                   elseIf    not %Eof(EDIstd80F) and
005800110525     c                                 %len(%trimR(RIGA80STD)) < 80
005900110525      * --------------------------------------------
006000110525      * SE SI DEVE CONTINUARE ad AGGIUNGERE records
006100110524      * -----
006200110525      *  lo spazio rimanente da riempire per arrivare a 80 nel campo
006300110525     c                   eval      lung_libero = (80 - %len(%trimR(RIGA80STD)))
006400110525      *
006500110525      * lo spazio x accodare il nuovo segmento
006600110525     c                   eval      DaChr = %len(%trimR(RIGA80STD)) + 1
006700110525      *
006800110525     c                   eval       aChr = lung_Libero
006900110525     c                   eval       a    = lung_Libero
007000110525      *
007100110525     c                   exsr      upd_record
007200110525      *
007300110525     c                   add       1             numriga
007400110525      *  quindi prosegue
007500110525     c                   eval       da = lung_Libero + 1
007600110525     c                   eval        a = 80
007700110525     c                   eval      DaChr =  1
007800110525     c                   eval       aChr =  80
007900110525      * -----
008000110525     c                   exsr      Nuova_write
008100110525      * -----
008200110524     c                   end
008300110520      **?__________________________________________________________________ */
008400110520     c                   seton                                        LR
008500110524      **?------------------------------------------------------------------ */
008600110525     c     Nuova_Write   BEGSR
008700110525      *
008800110525     c     NuovoRecord   tag
008900110525      * nuovi 80 caratteri
009000110525     c                   if        %subst(segmento:da:a) <> *blank
009100110525     c                   exsr      wrt_records
009200110525     c                   add       1             numriga
009300110525     c                   goto      NuovoRecord
009400110525     c                   end
009500110525      *
009600110525     c                   ENDSR
009700110525      **?------------------------------------------------------------------ */
009800110525     c     upd_record    BEGSR
009900110525      *
010000110525     c                   eval        %subst(riga80std:daChr:aChr)
010100110525     c                             = %subst(segmento:da:a)
010200110525      *
010300110525     c                   update    STANDARD80
010400110525      *
010500110525     c                   ENDSR
010600110525      **?------------------------------------------------------------------ */
010700110524     c     wrt_records   BEGSR
010800110524      *
010900110524     c                   eval        %subst(riga80std:daChr:aChr)
011000110524     c                             = %subst(segmento:da:a)
011100110524      *
011200110524     c                   write     STANDARD80
011300110525     c                   add       80            da
011400110524      *
011500110524     c                   ENDSR
011600110524      **?------------------------------------------------------------------ */
011700110621      * ?------------------------------------------------------------------ */
011800110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
011900110621      * ?------------------------------------------------------------------ */
012000110621     C     *pssr         BEGSR
012100110621     C
012200110621     C                   eval      esito = 'E'
012300110621     C                   ENDSR     '*CANCL'
012400110621     C
012500110621      *  ------------------------------------------------------------------ */
