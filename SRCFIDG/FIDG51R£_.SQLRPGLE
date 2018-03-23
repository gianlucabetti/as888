000100110727     h dftactgrp(*no) actgrp(*new)
000200110728     H* FIDG16R1*-----------------------------------------------------*
000300110728     H* verifica se spedizioni consegnabili per un perfect order
000400930225     H*---------------------------------------------------------------*
000500110726     Ffnarb01l  IF   E           K DISK    prefix(£)
000600110726     Ffiarg01l  IF   E           K DISK    prefix(£)
000700900514     D KPJBA         E DS
000800061002     d* Parametri x Controllo profilo utenti
000900070302     d TIBS34ds      e ds
001000061002     d AZUTEds       e ds                  extname(AZUTE00F)
001100061002     d DDatiUte      e ds
001200070130     d fnarbds       e ds                  extname(fnarb00F)
001300110726     d fnarbds£      e ds                  extname(fnarb00F) prefix(£)
001400110726     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
001500110726     d fiargds       e ds                  extname(fiarg00F)
001600110726     d fiargds$      e ds                  extname(fiarg00F) prefix($)
001700110726     d fiargds£      e ds                  extname(fiarg00F) prefix(£)
001800070130     d fidg16ds      e ds
001900110726     D Fidg16ds1     E DS                  extname(fidg16ds) prefix(£)
002000110727     d conta           s              1  0
002100110727     d xco             s                   like(arbxco)
002200110727     d COMNSP          s                   like(arbNSP)
002300110727     d COMRMA          s                   like(arbRMA)
002400070130     C**********************************************************************
002500070131     c                   exsr      srinz
002600110726     c* 1 bolla con cui sto verificando le sorelle
002700110726     c                   eval      conta = 1
002800110727     c                   eval      COMRMA = ARBRMA
002900110727     c                   eval      COMNSP = ARBNSP
003000110726     c* leggo le sorelle
003100110726     C/EXEC SQL
003200110727     C+ DECLARE B1 CURSOR FOR SELECT fnarb01l.* FROM fnarb01l WHERE
003300110727     C+ arbrma = :COMrma and arbxco  <> ' ' and arbnsp <> :COMnsp
003400110726     C/END-EXEC
003500110726
003600110726     C/EXEC SQL
003700110726     C+ OPEN b1
003800110726     C/END-EXEC
003900110726
004000110726     C                   do        *hival
004100110726
004200110726     C/EXEC SQL
004300110727     C+ FETCH NEXT FROM b1 INTO :fnarbds£
004400110726     C/END-EXEC
004500110726
004600110726     C                   select
004700110726     C                   when      sqlcod = 100
004800110726     c                   leave
004900110726     C                   WHEN      SqlCod < 0
005000110726     c                   if        sqlcod <> -913
005100110726     C                   seton                                        H1
005200110726     c                   end
005300110726     c                   exsr      srfine
005400110726     C                   other
005500110726     c* conteggio sorelle per controllare XCO
005600110726     c                   add       1             conta
005700110727     c                   if        £arbndc = 0 and £arbntc = 0 and
005800110727     c                             arblna = £arblna and £arbfbc <> 'G'
005900110727     c                             and £arbfdn = ' '
006000110726     C                   exsr      srspedcon
006100110727     c                   if        £d16conso='N'
006200110727     c                   eval      d16msgo= 'Spedizioni x perfect order -
006300110727     c                             non assegnabili'
006400110727     c                   exsr      srfine
006500110727     c                   end
006600110727     c                   end
006700110726     C                   endsl
006800110726     C*
006900110726     C                   enddo
007000110726
007100110726     C/EXEC SQL
007200110726     C+ CLOSE b1
007300110726     C/END-EXEC
007400110727     c* verifico se ho già tutte le spedizioni
007500110727     c                   move      conta         xco
007600110727     c                   if        xco <> £arbxco
007700110727     c                   eval      d16msgo= 'Mancano spedizioni -
007800110727     c                             x perfect order'
007900110727     c                   exsr      srfine
008000110727     c                   end
008100110727     c*
008200110727     c                   if        d16conso = ' ' and d16consi= 'S'
008300110727     c                   eval      d16conso= 'S'
008400110727     c                   end
008500070130     c                   eval      kpjbu = fidg16ds
008600070131     c                   seton                                        lr
008700110726      *-----------------------------------------------
008800110726     C     srspedcon     begsr
008900110726      *-----------------------------------------------
009000110726     c                   clear                   fidg16ds1
009100110727     c                   clear                   FIARGDS£
009200110726     c                   eval      £D16ldcri= D16ldcri
009300110726     c                   eval      £D16dtaI = D16dtaI
009400110726     c                   eval      £D16oraI = D16oraI
009500110727     c                   eval      £d16aasi = £arbaas
009600110727     c                   eval      £d16lnpi = £arblnp
009700110727     c                   eval      £d16nrsi = £arbnrs
009800110727     c                   eval      £d16nspi = £arbnsp
009900110726     c                   eval      £d16consi= 'S'
010000110726     c                   eval      £d16dfvi = d16dfvi
010100110726     c                   eval      £d16ndci = 'E'
010200110727     c* controllo solo l'assegnabilità e non se ha sorelle altrimenti
010300110727     c* innesco un loop infinito
010400110727     c                   eval      £d16prfoi = ' '
010500110726     c                   eval      kpjbu = fidg16ds1
010600110727     c                   call      'FIDG16R'
010700110726     c                   parm                    kpjba
010800110727     c                   parm                    fnarbds£
010900110727     c                   parm                    fiargds£
011000110726     c                   eval      fidg16ds1 = kpjbu
011100110726     C                   ENDSR
011200070131     C**********************************************************************
011300070131     c     *inzsr        begsr
011400070131     C**********************************************************************
011500070131      * Reperisco dati job
011600070131     c                   exsr      DatiJob
011700070131     c*
011800070131     C     Karb          KLIST
011900070201     C                   KFLD                    d16aasi
012000070201     C                   KFLD                    d16lnpi
012100070201     C                   KFLD                    d16nrsi
012200070201     C                   KFLD                    d16nspi
012300070131     c*
012400070131     c                   endsr
012500070131     C**********************************************************************
012600070131     c     srinz         begsr
012700070131     C**********************************************************************
012800061002     C     *ENTRY        PLIST
012900070130     C                   PARM                    kpjba
013000070131     C                   PARM                    fnarbds$
013100070131     C                   PARM                    fiargds$
013200070130     c                   movel     kpjbu         fidg16ds
013300110727     c                   clear                   d16conso
013400110727     c                   clear                   d16msgo
013500070131     c*
013600070130     c                   if        %parms=1 or
013700070209     c                             $arbnsp = 0
013800070130     c     karb          chain     fnarb01l
013900070131     c                   if        not %found(fnarb01l)
014000070131     c                   clear                   fnarbds
014100070131     c                   else
014200070131     c                   eval      fnarbds = fnarbds£
014300070130     c                   end
014400070131     c                   end
014500070209     c                   if        (%parms > 1 and $arbnsp <>0)
014600070131     c                   eval      fnarbds = fnarbds$
014700070131     c                   end
014800070131     c*
014900070130     c                   if        %parms=1 or
015000070131     c                             (%parms = 3 and $argnsp = 0)
015100070131     c                   if        not %open(fiarg01l)
015200070130     c                   open      fiarg01l
015300070131     c                   end
015400070130     c     karb          chain     fiarg01l
015500070131     c                   if        not %found(fiarg01l)
015600070131     c                   clear                   fiargds
015700070131     c                   else
015800070131     c                   eval      fiargds = fiargds£
015900070131     c                   end
016000070130     c                   end
016100070131     c                   if        (%parms = 3 and $argnsp <>0)
016200070131     c                   eval      fiargds = fiargds$
016300070131     c                   end
016400070131     c*
016500070131     c                   if        arbnsp = 0
016600110727     c                   eval      d16msgo= 'Spedizione non trovata'
016700070131     c                   exsr      srfine
016800070131     c                   end
016900070130     c*
017000070130     c                   endsr
017100061002      *---------------------------------------------------------------*
017200061002      * Reperimento Dati del job (Utente/Operativi)                   *
017300061002      *---------------------------------------------------------------*
017400061002     c     DatiJob       BEGSR
017500061002      *
017600061002     c     *dtaara       define    §azute        azuteds
017700061002     c     *dtaara       define    §datiute      ddatiute
017800061002      *
017900061002     c                   in(E)     *dtaara
018000061002     c                   IF        %ERROR or RSUT = *blanks
018100061002     c                   clear                   Tibs34Ds
018200061002     c                   call      'TIBS34R'
018300061002     c                   parm                    Tibs34Ds
018400061002     c                   in        *dtaara
018500061002     c                   ENDIF
018600061002      *
018700061002     c                   ENDSR
018800110726     C**********************************************************************
018900110726     c     srfine        begsr
019000110726     C**********************************************************************
019100110727     c                   eval      d16conso = 'N'
019200110726     c* se richiesto il controllo x la messa in distinta vado a fine
019300110726     c* perchè sono errori bloccanti
019400110726     c                   eval      kpjbu = fidg16ds
019500110726     c                   seton                                        lr
019600110726     C                   ENDSR
