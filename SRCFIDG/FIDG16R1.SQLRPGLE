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
003000130221      *** Wurth adesso manda nel campo non solo numeri quindi per identificare
003100130221      ***  il Perfect Order si deve sostituire il test
003200110726     c* leggo le sorelle
003300130221     *******C/EXEC SQL
003400130221     *******C+ DECLARE B1 CURSOR FOR SELECT fnarb01l.* FROM fnarb01l WHERE
003500130221     *******C+ arbrma = :COMrma and arbxco  <> ' ' and arbnsp <> :COMnsp
003600130221     *******C/END-EXEC
003700110726
003800130221     C/EXEC SQL
003900130221     C+ DECLARE B1 CURSOR FOR SELECT fnarb01l.* FROM fnarb01l WHERE
004000130221     C+ arbrma = :COMrma and arbxco > '0' and arbnsp <> :COMnsp
004100130221     C/END-EXEC
004200130221
004300110726     C/EXEC SQL
004400110726     C+ OPEN b1
004500110726     C/END-EXEC
004600110726
004700110726     C                   do        *hival
004800110726
004900110726     C/EXEC SQL
005000110727     C+ FETCH NEXT FROM b1 INTO :fnarbds£
005100110726     C/END-EXEC
005200110726
005300110726     C                   select
005400110726     C                   when      sqlcod = 100
005500110726     c                   leave
005600110726     C                   WHEN      SqlCod < 0
005700110726     c                   if        sqlcod <> -913
005800110726     C                   seton                                        H1
005900110726     c                   end
006000110726     c                   exsr      srfine
006100110726     C                   other
006200110923     c* controllo xco delle sorelle che sia uguale altrimenti libere tutte
006300130221     c                   if        £arbxco <> arbxco and arbxco >'0'
006400110923     c                   eval      d16conso= 'S'
006500110923     c                   eval      kpjbu = fidg16ds
006600110923     c                   seton                                        lr
006700110923     c                   return
006800110912     c                   end
006900110912     c* conteggio sorelle per controllare XCO
007000110726     c                   add       1             conta
007100110905     c* controllo l'assegnabilità spediz. sorella solo
007200110905     c                   if        £arbndc = 0 and
007300110727     c                             arblna = £arblna and £arbfbc <> 'G'
007400110905     c*                            and £arbfdn = ' '
007500110726     C                   exsr      srspedcon
007600110727     c                   if        £d16conso='N'
007700110727     c                   eval      d16msgo= 'Spedizioni x perfect order -
007800110727     c                             non assegnabili'
007900110727     c                   exsr      srfine
008000110727     c                   end
008100110727     c                   end
008200110726     C                   endsl
008300110726     C*
008400110726     C                   enddo
008500110726
008600110726     C/EXEC SQL
008700110726     C+ CLOSE b1
008800110726     C/END-EXEC
008900110727     c* verifico se ho già tutte le spedizioni
009000110727     c                   move      conta         xco
009100130221     c                   if        xco < arbxco and arbxco >'0'
009200110727     c                   eval      d16msgo= 'Mancano spedizioni -
009300110727     c                             x perfect order'
009400110727     c                   exsr      srfine
009500110727     c                   end
009600110727     c*
009700110727     c                   if        d16conso = ' ' and d16consi= 'S'
009800110727     c                   eval      d16conso= 'S'
009900110727     c                   end
010000070130     c                   eval      kpjbu = fidg16ds
010100070131     c                   seton                                        lr
010200110726      *-----------------------------------------------
010300110726     C     srspedcon     begsr
010400110726      *-----------------------------------------------
010500110726     c                   clear                   fidg16ds1
010600110727     c                   clear                   FIARGDS£
010700110726     c                   eval      £D16ldcri= D16ldcri
010800110726     c                   eval      £D16dtaI = D16dtaI
010900110726     c                   eval      £D16oraI = D16oraI
010901150430     c                   eval      £D16p88I = D16p88I
011000110727     c                   eval      £d16aasi = £arbaas
011100110727     c                   eval      £d16lnpi = £arblnp
011200110727     c                   eval      £d16nrsi = £arbnrs
011300110727     c                   eval      £d16nspi = £arbnsp
011400110726     c                   eval      £d16consi= 'S'
011500110726     c                   eval      £d16dfvi = d16dfvi
011600110726     c                   eval      £d16ndci = 'E'
011700110727     c* controllo solo l'assegnabilità e non se ha sorelle altrimenti
011800110727     c* innesco un loop infinito
011900110727     c                   eval      £d16prfoi = ' '
012000110726     c                   eval      kpjbu = fidg16ds1
012100110727     c                   call      'FIDG16R'
012200110726     c                   parm                    kpjba
012300110727     c                   parm                    fnarbds£
012400110727     c                   parm                    fiargds£
012500110726     c                   eval      fidg16ds1 = kpjbu
012600110726     C                   ENDSR
012700070131     C**********************************************************************
012800070131     c     *inzsr        begsr
012900070131     C**********************************************************************
013000070131      * Reperisco dati job
013100070131     c                   exsr      DatiJob
013200070131     c*
013300070131     C     Karb          KLIST
013400070201     C                   KFLD                    d16aasi
013500070201     C                   KFLD                    d16lnpi
013600070201     C                   KFLD                    d16nrsi
013700070201     C                   KFLD                    d16nspi
013800070131     c*
013900070131     c                   endsr
014000070131     C**********************************************************************
014100070131     c     srinz         begsr
014200070131     C**********************************************************************
014300061002     C     *ENTRY        PLIST
014400070130     C                   PARM                    kpjba
014500070131     C                   PARM                    fnarbds$
014600070131     C                   PARM                    fiargds$
014700070130     c                   movel     kpjbu         fidg16ds
014800110727     c                   clear                   d16conso
014900110727     c                   clear                   d16msgo
015000070131     c*
015100070130     c                   if        %parms=1 or
015200070209     c                             $arbnsp = 0
015300070130     c     karb          chain     fnarb01l
015400070131     c                   if        not %found(fnarb01l)
015500070131     c                   clear                   fnarbds
015600070131     c                   else
015700070131     c                   eval      fnarbds = fnarbds£
015800070130     c                   end
015900070131     c                   end
016000070209     c                   if        (%parms > 1 and $arbnsp <>0)
016100070131     c                   eval      fnarbds = fnarbds$
016200070131     c                   end
016300070131     c*
016400070130     c                   if        %parms=1 or
016500070131     c                             (%parms = 3 and $argnsp = 0)
016600070131     c                   if        not %open(fiarg01l)
016700070130     c                   open      fiarg01l
016800070131     c                   end
016900070130     c     karb          chain     fiarg01l
017000070131     c                   if        not %found(fiarg01l)
017100070131     c                   clear                   fiargds
017200070131     c                   else
017300070131     c                   eval      fiargds = fiargds£
017400070131     c                   end
017500070130     c                   end
017600070131     c                   if        (%parms = 3 and $argnsp <>0)
017700070131     c                   eval      fiargds = fiargds$
017800070131     c                   end
017900070131     c*
018000070131     c                   if        arbnsp = 0
018100110727     c                   eval      d16msgo= 'Spedizione non trovata'
018200070131     c                   exsr      srfine
018300070131     c                   end
018400070130     c*
018500070130     c                   endsr
018600061002      *---------------------------------------------------------------*
018700061002      * Reperimento Dati del job (Utente/Operativi)                   *
018800061002      *---------------------------------------------------------------*
018900061002     c     DatiJob       BEGSR
019000061002      *
019100061002     c     *dtaara       define    §azute        azuteds
019200061002     c     *dtaara       define    §datiute      ddatiute
019300061002      *
019400061002     c                   in(E)     *dtaara
019500061002     c                   IF        %ERROR or RSUT = *blanks
019600061002     c                   clear                   Tibs34Ds
019700061002     c                   call      'TIBS34R'
019800061002     c                   parm                    Tibs34Ds
019900061002     c                   in        *dtaara
020000061002     c                   ENDIF
020100061002      *
020200061002     c                   ENDSR
020300110726     C**********************************************************************
020400110726     c     srfine        begsr
020500110726     C**********************************************************************
020600110727     c                   eval      d16conso = 'N'
020700110726     c* se richiesto il controllo x la messa in distinta vado a fine
020800110726     c* perchè sono errori bloccanti
020900110726     c                   eval      kpjbu = fidg16ds
021000110726     c                   seton                                        lr
021100110726     C                   ENDSR
