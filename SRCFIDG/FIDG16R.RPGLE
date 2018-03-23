000100110727     h dftactgrp(*no) actgrp(*caller)
000200070302     H* FIDG16R *-----------------------------------------------------*
000300070130     H* spedizione consegnabile S/N in base ai parametri ricevuti
000400930225     H*---------------------------------------------------------------*
000500070131     Ffnarb01l  IF   E           K DISK    usropn prefix(£)
000600070131     Ffiarg01l  IF   E           K DISK    usropn prefix(£)
000700070131     Fazcln01l  IF   E           K DISK    usropn
000800070130     Ftabel00f  IF   E           K DISK
000900110905     Ftntbe01l  IF   E           K DISK
001000070130     Ffnlbl02l  IF   E           K DISK
001100070514     Ffnlbl01l  IF   E           K DISK    rename(fnlbl000: fnlbl001)
001200070131     Ffiar401l  IF   E           K DISK
001300070322     Fazorg01l  IF   E           K DISK
001400070322     FFIAR601L  IF   E           K DISK
001500900514     D KPJBA         E DS
001600061002     d* Parametri x Controllo profilo utenti
001700070302     d dsbl4l        e ds
001800070302     d TIBS34ds      e ds
001900070314     d fnlr76ds      e ds
002000061002     d AZUTEds       e ds                  extname(AZUTE00F)
002100061002     d DDatiUte      e ds
002200150508     d fidstds       e ds                  extname(fidst00F)
002201150508     d fnarbds       e ds                  extname(fnarb00F)
002300070131     d fnarbds£      e ds                  extname(fnarb00F) prefix(£)
002400070131     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
002500070827     d fnarbdsc      e ds                  extname(fnarb00F) prefix(c)
002600070130     d fiargds       e ds                  extname(fiarg00F)
002700070131     d fiargds£      e ds                  extname(fiarg00F) prefix(£)
002800070131     d fiargds$      e ds                  extname(fiarg00F) prefix($)
002900070322     D og143         E DS
003000070323     D*fidn12ds      E DS
003100070130     d fidg16ds      e ds
003200110727     d fidg16ds1     e ds                  extname(fidg16ds) prefix(£)
003300110727     d trul52ds      e ds
003400110727     d dar5gen       e ds
003500070131     d dsbl4a        e ds
003600070130     d ds3a          e ds
003700110905     d dxcodta       e ds
003800070130     d ds7r          e ds
003900070131     D MES             S              1    DIM(31)
004000070131     D dtaiso          S               d
004100070131     D oggi            S              8  0
004200070130     C**********************************************************************
004300070131     c                   exsr      srinz
004400100202     c*
004500100202     c                   if        arbgma <> ' '
004600100202     C                   eval      tblcod = '7R'
004700100202     C                   eval      tblkey = arbgma
004800100202     C     ktab          chain     Tabel00f
004900100202     c                   if        %found(tabel00f)
005000100202     C                   MOVEL     TBLUNI        DS7R
005100100202     c                   else
005200100202     c                   clear                   ds7r
005300100202     c                   end
005400100203     c                   end
005500070227     c* flag generico di abilitazione spedizione
005600070329     c                   if        d16consi= 'S' or d16BGEi= 'S'
005700070329     c                   if        §B4LBGE <> ' '
005800070322     c                   eval      d16msgo= 'Spedizione con sospensione consegn-
005900070322     c                             a'
006000070329     c                   eval      d16bgeo = 'S'
006100070302     c                   exsr      srfine
006200070302     c                   end
006300070329     c                   end
006400070322     c* data consegna merce
006500070201     c                   if        d16consi= 'S' or d16dcmi= 'S'
006600070216     c                   if        arbdcm <> 0
006700070329     c*                  eval      d16msgo= 'Spedizione già consegnata'
006800070329     c                   eval      d16dcmo = 'S'
006900070131     c                   exsr      srfine
007000070131     c                   end
007100070131     c                   end
007200070130     c* numero distinta, controllo quella di ARG perchè anche quando saremo
007300070416     c* partiti continueremo ad aggiornarlo (solo per assegnaz. automatica)
007400070416     c*                  if        d16consi= 'S' or
007500070416     c                   if        d16ndci= 'G'
007600070201     c                             or d16ndci= 'E'
007700070130     c                   if        argndc <> 0
007800070329     c                   eval      d16ndco = 'S'
007900070329     c*                  eval      d16msgo= 'Spedizione già in distinta'
008000070131     c                   exsr      srfine
008100070130     c                   end
008200070131     c                   end
008300070209     c* su ARB x il momento lo controllo solo se richiesto
008400070416     c                   if        (d16consi= 'S' and d16ndci = ' ') or
008500070416     c                             d16ndci= 'B'
008600070201     c                             or d16ndci= 'E'
008700070131     c                   if        arbndc <> 0
008800070329     c                   eval      d16ndco = 'S'
008900070329     c*                  eval      d16msgo= 'Spedizione già in distinta'
009000070131     c                   exsr      srfine
009100070131     c                   end
009200070131     c                   end
009300070130     c* abilitazione contrassegno
009400070201     c                   if        d16consi= 'S' or d16acci= 'S'
009500070130     c                   if        arbacc <> 'S'
009600070329     c                   eval      d16acco = 'S'
009700070131     c                   eval      d16msgo= 'Incasso contrassegno non abilitato'
009800070131     c                   exsr      srfine
009900070130     c                   end
010000070131     c                   end
010100070130     c* abilitazione assegnato
010200070209     c                   if        d16consi= 'S' or d16acai= 'S'
010300070209     c                   if        arbaca <> 'S'
010400070329     c                   eval      d16acao = 'S'
010500070131     c                   eval      d16msgo= 'Incasso assegnato non abilitato'
010600070131     c                   exsr      srfine
010700070130     c                   end
010800070131     c                   end
010900070322     c* spedizione bloccata
011000070201     c                   if        d16consi= 'S' or d16fbci= 'S'
011100070216     c                   if        arbfbc <> ' '
011200070329     c                   eval      d16fbco = 'S'
011300070329     c                   if        arbfbc <> 'G' and arbfbc <> '*'
011400070305     c                   eval      d16msgo= 'Spedizione bloccata con ' +
011500070305     c                             arbfbc
011600070329     c                   end
011700070131     c                   exsr      srfine
011800070130     c                   end
011900070131     c                   end
012000070130     c* spedizione giacente
012100070201     c                   if        d16consi= 'S' or d16cmci= 'S'
012200070130     c                   if        arbcmc <> ' '
012300070329     c                   eval      d16cmco = 'S'
012400070329     c*                  eval      d16msgo= 'Spedizione giacenza da aprire'
012500070131     c                   exsr      srfine
012600070130     c                   end
012700070131     c                   end
012800070130     C* Controllo che la spedizione non sia una mamma
012900070201     c                   if        d16consi= 'S' or d16lblmi= 'S'
013000070130     C     KARB          CHAIN     FNLBL02L
013100070130     c                   if        %found(fnlbl02l)
013200070329     c                   eval      d16lblmo= 'S'
013300070329     c*                  eval      d16msgo= 'Spedizione mamma'
013400070131     c                   exsr      srfine
013500070130     c                   end
013600070131     c                   end
013700070130     C*  Controllo se spedizione consegnata in anticipo
013800070201     c                   if        d16consi= 'S' or d16dcri= 'S'
013900070131     C     ARBDCR        IFGE      oggi
014000070130     C     ARBTCR        ANDEQ     'D'
014100070131     C     ARBDCR        ORGT      oggi
014200070130     C     ARBTCR        ANDEQ     ' '
014300070329     c                   eval      d16dcro= 'S'
014400070201     c                   eval      d16msgo= 'Spedizione da consegnare IL o DOPO'
014500070131     c                   exsr      srfine
014600070130     c                   end
014700070131     c                   end
014800071210     C*  Controllo la data limite consegna richiesta prima del
014900071210     c                   if        d16consi= 'S' and d16ldcri <> 0
015000100202     c                             and §7ratt <> 'S'
015100071210     C     ARBDCR        IFGT      d16ldcri
015200071210     c                   eval      d16msgo= 'Data limite consegna richiesta PR-
015300071210     c                             IMA DEL > del ...'
015400071210     c                   exsr      srfine
015500071210     c                   end
015600071210     c                   end
015700070322     C* turno chiusura tutto il giorno. mezza giornata vado in consegna
015800070201     c                   if        d16consi= 'S' or d16gc12i= 'S'
015900070322     C     ARBGC1        IFNE      *BLANKS
016000070322     C                   IF        %subst(arbgc1: 1: 1) = WGG and
016100070322     C                             %subst(arbgc1: 2: 1) = ' '
016200070329     c                   eval      d16gc12o= 'S'
016300070131     c                   eval      d16msgo= 'Chiuso per turno'
016400070131     c                   exsr      srfine
016500070130     C                   END
016600070130     C                   END
016700070130     C*
016800070130     C     ARBGC2        IFNE      *BLANKS
016900070322     C                   IF        %subst(arbgc2: 1: 1) = WGG and
017000070322     C                             %subst(arbgc2: 2: 1) = ' '
017100070329     c                   eval      d16gc12o= 'S'
017200070131     c                   eval      d16msgo= 'Chiuso per turno'
017300070131     c                   exsr      srfine
017400070130     C                   END
017500070130     C                   END
017600070131     C                   END
017700070322     C* Controllo che ci sia la consegna richiesta x
017800070322     C* con appuntamento/ supermercato
017900070201     c                   if        d16consi= 'S' or d16tc12ai= 'S'
018000070302     c                   if        (arbtc1 = 'A' or arbtc2 = 'A' or
018100070302     c                             arbtc1 = 'S' or arbtc2 = 'S') and
018200070302     c                             §B4LBAS <> ' '
018300070329     c                   eval      d16tc12ao= 'S'
018400070131     c                   eval      d16msgo= 'Richiesto appuntamento'
018500070131     c                   exsr      srfine
018600070130     c                   endif
018700070130     C                   END
018800070214     c* fermo deposito
018900070216     c                   if        d16consi= 'S' or d16ffdi= 'S'
019000070507     c                   if        arbffd = 'S' and arblna <> 101 and
019100070507     c                             arblna <> 102
019200070329     c                   eval      d16ffdo = 'S'
019300070214     c                   eval      d16msgo= 'Spedizione in Fermo deposito'
019400070214     c                   exsr      srfine
019500070214     c                   end
019600070214     c                   end
019700070514     c* richiamo la funzione utilizzata anche dalla lista sped.da consegnare
019800070514     c* mi controlla la consegnabilità della merce
019900070514     c                   clear                   fnlr76ds
020000070514     c                   eval      ILR76DCO= oggi
020100070514     c                   eval      ILR76SPPAR='S'
020200070830     c                   movel     d16dtai       dssoglia
020300070830     c                   move      d16orai       dssoglia
020400071129     c* controllo: se ho un legame, se stessa linea di arr. e no segnacolli
020500071129     c* passo la chiave della mamma ma non la ds di arb. Sarà
020600070514     c* il pgm a recuperarsela
020700070514     C     Karb          CHAIN     FNLBL01L
020800070514     C                   IF        %found(fnlbl01l) and lbllap = lbllan
020900071129     c                             and arbfns = ' ' and arbncd = 0
021000071129     c                   clear                   fnarbdsc
021100070514     c                   eval      ILR76AAS= LBLAAP
021200070514     c                   eval      ILR76LNP= LBLLPP
021300070514     c                   eval      ILR76NRS= LBLNRP
021400070514     c                   eval      ILR76NSP= LBLNSP
021500070514     c                   call      'FNLR76R'
021600070514     c                   parm                    fnlr76ds
021700070827     c                   parm                    fnarbdsc
021800070830     C                   PARM                    dsSoglia         12 0
021900070514     c                   else
022000070314     c                   eval      ILR76AAS= arbaas
022100070314     c                   eval      ILR76LNP= arblnp
022200070314     c                   eval      ILR76NRS= arbnrs
022300070314     c                   eval      ILR76NSP= arbnsp
022400070330     c                   call      'FNLR76R'
022500070330     c                   parm                    fnlr76ds
022600070330     c                   parm                    fnarbds
022700070830     C                   PARM                    dsSoglia         12 0
022800070514     c                   end
022900070322     c* nessun collo consegnabile
023000070322     c                   if        OLR76CDACO = 0
023100070322     c                   if        d16consi= 'S' or d16colci= 'S'
023200070329     c                   eval      d16colco= 'S'
023300070322     c                   eval      d16msgo= 'Nessun collo consegnabile'
023400070314     c                   exsr      srfine
023500070322     c                   end
023600070322     c                   else
023700151015     c* se scelto di escludere pistola 88 (no H10.30 e no FW)
023701150513     c                   if        d16p88i= 'N' and arbtsp <> 'H' and
023702151015     c                             OLR76CDACO = OLR76CDC88 and arbcbo<>'FW'
023711150430     c                   eval      d16colco= 'S'
023712150430     c                   eval      d16msgo= 'Colli consegnabili ma pistola "88"'
023713150430     c                   exsr      srfine
023715150508     c                   end
023801150430     c* se la somma dei colli consegnati + i consegnabili non mi da i colli
023802150430     c* totali della spedizione sono nel caso di parziale  ...
023900070420     c                   if        (OLR76Cdaco + OLR76CDCM) <> arbncl
024000110728     C* se perfect order non abilito l'assegnabilità in caso di parziale
024100130221     c*********          if        d16consi= 'S' and arbxco <> ' '
024200130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
024300130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
024400130221     c                   if        d16consi= 'S' and arbxco > '0'
024500110728     c                   eval      d16msgo= 'Consegna parziale: perfect order'
024600110728     c                   exsr      srfine
024700110728     c                   end
024800070322     C* controllo particolarità consegne =  NO PARZ.
024900070322     c                   if        d16consi= 'S' or d16gmapi= 'S'
025000070322     c                   if        §7r8cp = 'N'
025100070329     c                   eval      d16gmapo= 'S'
025200070322     c                   eval      d16msgo= 'Consegna parziale: particolarità C-
025300070322     c                             P'
025400070322     c                   exsr      srfine
025500070322     c                   end
025600070322     c                   end
025700070322     c* controllo se c'è il contrassegno
025800070322     c                   if        d16consi= 'S' or d16parcai = 'S'
025900070322     c                   if        §3afca <> 0
026000070329     c                   eval      d16parcao = 'S'
026100070322     c                   eval      d16msgo= 'Consegna parziale: contrassegno'
026200070322     c                   exsr      srfine
026300070322     c                   end
026400070322     c                   end
026500070322     c* controllo se c'è fattura immediata
026600070322     c                   if        d16consi= 'S' or d16parfti = 'S'
026700070326     C                   MOVEL     §3ATB1        WTBOL1            1
026800070326     C                   MOVEL     §3ATB2        WTBOL2            1
026900070326     c                   clear                   AR6TRC
027000070326     c                   select
027100070326     C     WTBOL1        wheneq    'A'
027200070326     C                   MOVEL     '1'           AR6TRC
027300070326     C     WTBOL2        wheneq    'A'
027400070326     C                   MOVEL     '2'           AR6TRC
027500070326     c                   endsl
027600070326     c     kar6          chain     fiar601l
027700070322     c                   if        %found(fiar601l) and ar6nft <> 0
027800070329     c                   eval      d16parfto = 'S'
027900070322     c                   eval      d16msgo= 'Consegna parziale: fattura immedia-
028000070322     c                             ta'
028100070322     c                   exsr      srfine
028200070322     c                   end
028300070322     c                   end
028400070420     c* se parziali e colli consegnabili da almeno 1gg = 0 (appena arrivate)
028500091027     c* solo se ESPRESSI, H10.30, IMPORT o parzialmente consegnate
028600070322     c                   if        OLR76CDC1G = 0
028700070322     c                   if        d16consi= 'S' or d16parasgi = 'S'
028800070420     c* ???              if        arbtsp <> 'E'
028900070420     c                   if        arbtsp <> 'E' and OLR76CDCM = 0
029000091027     c                             and arbtsp <> 'H'
029100070322     c* verifico se spedizione import
029200070322     C                   MOVE      '   '         §OGntw
029300070322     c     arblnp        chain     azorg01l
029400070322     c                   if        %found(azorg01l)
029500070322     C                   MOVEL     ORGDE3        OG143
029600070322     c                   MOVEL     §ogntw        LNPntw            3
029700070322    1C     LNPntw        IFEQ      'EEX'
029800070322    1C     LNPntw        oreq      'EUP'
029900070322    1C     LNPntw        oreq      'DPD'
030000070322    1C     LNPntw        oreq      'FED'
030100070322     c                   else
030200070329     c                   eval      d16parasgo = 'S'
030300070322     c                   eval      d16msgo= 'Consegna parziale: arrivate oggi'
030400070322     c                   exsr      srfine
030500070322     c                   end
030600070322     c                   end
030700070322     c                   end
030800070322     c                   end
030900070322     c                   end
031000070322     c*
031100070322     c                   end
031200070322     c                   end
031300110727     c* verifico, se richiesto, il perfect order
031400110905     c                   movel     arbaas        dsp
031500110905     c                   move      arbmgs        dsp               8 0
031600130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
031700130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
031800130221     c*********          if        d16prfoi = 'S' and arbxco <>' ' and
031900130221     c                   if        d16prfoi = 'S' and arbxco >'0' and
032000110905     c                             (DAT1GG>oggi)
032100110727     c                   eval      fidg16ds1 = fidg16ds
032200110727     c                   eval      kpjbu = fidg16ds1
032300110727     c                   call      'FIDG16R1'
032400110727     c                   parm                    kpjba
032500110727     c                   parm                    fnarbds
032600110727     c                   parm                    fiargds
032700110727     c                   eval      fidg16ds1 = kpjbu
032800110902     c* escludo gli errori di non assegnabilità sorella se è già in
032900110902     c* distinta, se è già consegnata, se è in giacenza o se è fermo depos.
033000110902     c                   if        £d16conso = 'N'and
033100110902     c                             £d16ndco = *blanks and
033200110905     c                             £d16dcmo = *blanks and
033300110902     c                             £d16cmco = *blanks and
033400110902     c                             £d16ffdo = *blanks
033500110902     c                   eval      d16prfoo = 'S'
033600110902     c                   eval      d16msgo= 'Attesa per perfect order'
033700110727     c                   exsr      srfine
033800110727     c                   end
033900110902     c                   end
034000110727     c
034100070131     c**********************************************************************
034200070131     c* i controlli successivi non impediscono la messa in distinta
034300070131     c**********************************************************************
034400070131     c* DDT SI
034500070201     c                   if        d16ddtsii= 'S'
034600070131     C                   MOVEL     'A'           ar4trc
034700070131     C     Kfiar4        CHAIN     FiAR401L
034800070131if  1C                   if        %found(fiar401l)
034900070131     C                   movel     ar4not        dsbl4a
035000070131     c                   Move      §b4Abm        wbam              1
035100070131if  2C     WBAM          IFEQ      'S'
035200070131     C     WBAM          OREQ      'C'
035300070131     c     wbam          oreq      'P'
035400070131     c     wbam          oreq      'Q'
035500070131x   2C                   ELSE
035600070329     c                   eval      d16ddtsio = 'S'
035700070131     c                   eval      d16msgo= 'Spedizione senza DDT SI'
035800070131     c                   end
035900070131e   2C                   ENDIF
036000070131e   2C                   ENDIF
036100070131     c* resi
036200070201     c                   if        d16fbri= 'S'
036300070131     c                   if        arbfbr = 'S'
036400070329     c                   eval      d16fbro = 'S'
036500070201     c                   eval      d16msgo= 'Spedizione di reso'
036600070131     c                   end
036700070131     c                   end
036800070131     c* conti servizio
036900070201     c                   if        d16rblci= 'S'
037000070131     c                   if        §3ARBL = 'C'
037100070329     c                   eval      d16rblco = 'S'
037200070201     c                   eval      d16msgo= 'Spedizione in Conto servizio'
037300070131     c                   end
037400070131     c                   end
037500070216     c**********************************************************************
037600070131     c* si può mettere in distinta
037700070216     c**********************************************************************
037800150717     c*                  if        d16conso = ' ' and d16consi= 'S'
037801150717     c                   if        d16conso = ' '
037900070131     c                   eval      d16conso= 'S'
038000070131     c                   end
038100070130     c                   eval      kpjbu = fidg16ds
038200070201     c                   if        d16telai= 'C' or d16telai= ' '
038300070131     c                   seton                                        lr
038400070131     c                   else
038500070131     c                   seton                                        rt
038600070131     c                   end
038700070130     C**********************************************************************
038800070131     c     srfine        begsr
038900070130     C**********************************************************************
039000070131     c                   eval      d16conso = 'N'
039100070131     c* se richiesto il controllo x la messa in distinta vado a fine
039200070131     c* perchè sono errori bloccanti
039300070201     c                   if        d16consi= 'S' or arbnsp = 0
039400070131     c                   eval      kpjbu = fidg16ds
039500070201     c                   if        d16telai= 'C' or d16telai= ' '
039600070131     c                   seton                                        lr
039700070131     c                   else
039800070131     c                   seton                                        rt
039900070131     c                   end
040000070131     c                   return
040100070131     c                   end
040200070131     C                   ENDSR
040300070131     C**********************************************************************
040400070131     c     *inzsr        begsr
040500070131     C**********************************************************************
040600070131      * Reperisco dati job
040700070131     c                   exsr      DatiJob
040800070131     c*
040900070131     C     Karb          KLIST
041000070201     C                   KFLD                    d16aasi
041100070201     C                   KFLD                    d16lnpi
041200070201     C                   KFLD                    d16nrsi
041300070201     C                   KFLD                    d16nspi
041400070326     C     Kar6          KLIST
041500070326     C                   KFLD                    d16aasi
041600070326     C                   KFLD                    d16lnpi
041700070326     C                   KFLD                    d16nrsi
041800070326     C                   KFLD                    d16nspi
041900070326     C                   KFLD                    AR6TRC
042000070131     C     Ktab          KLIST
042100070131     C                   KFLD                    tblkut
042200070131     C                   KFLD                    tblcod
042300070131     C                   KFLD                    tblkey
042400070131     c                   z-add     1             tblkut
042500110905     C     Ktbe          KLIST
042600110905     C                   KFLD                    tbecod
042700110905     C                   KFLD                    tbeke1
042800070131     C     KCLN          KLIST
042900070131     C                   KFLD                    CLNTFP
043000070131     C                   KFLD                    CLNTFA
043100070131     C                   KFLD                    CLNANN
043200070131     C                   KFLD                    CLNMES
043300070131     C     Kfiar4        KLIST
043400070201     C                   KFLD                    d16aasi
043500070201     C                   KFLD                    d16lnpi
043600070201     C                   KFLD                    d16nrsi
043700070201     C                   KFLD                    d16nspi
043800070131     C                   KFLD                    ar4trc
043900070131     c*
044000070131     c                   endsr
044100070131     C**********************************************************************
044200070131     c     srinz         begsr
044300070131     C**********************************************************************
044400061002     C     *ENTRY        PLIST
044500070130     C                   PARM                    kpjba
044600070131     C                   PARM                    fnarbds$
044700070131     C                   PARM                    fiargds$
044800070130     c                   movel     kpjbu         fidg16ds
044900070131     c* pulisco i dati di output
045000070131     c                   clear                   d16conso
045100070131     c                   clear                   d16msgo
045200070131     c                   clear                   D16DcmO
045300070131     c                   clear                   D16NDCO
045400070131     c                   clear                   D16ACCO
045500070209     c                   clear                   D16aCAO
045600070131     c                   clear                   D16FBCO
045700070131     c                   clear                   D16CMCO
045800070131     c                   clear                   D16DDTSIO
045900070131     c                   clear                   D16FfDO
046000070131     c                   clear                   D16FBRO
046100070131     c                   clear                   D16RBLCO
046200070131     c                   clear                   D16LBLMO
046300070131     c                   clear                   D16GMAPO
046400070131     c                   clear                   D16DCRO
046500070131     c                   clear                   D16GC12O
046600070131     c                   clear                   D16TC12AO
046700070329     c                   clear                   D16bgeO
046800070322     c                   clear                   D16COLCo
046900070322     c                   clear                   D16PARCAo
047000070322     c                   clear                   D16PARFTo
047100070322     c                   clear                   D16PARASGo
047200110902     c                   clear                   d16prfoo
047300070131     c*
047400070130     c                   if        %parms=1 or
047500070209     c                             $arbnsp = 0
047600070131     c                   if        not %open(fnarb01l)
047700070130     c                   open      fnarb01l
047800070131     c                   end
047900070130     c     karb          chain     fnarb01l
048000070131     c                   if        not %found(fnarb01l)
048100070131     c                   clear                   fnarbds
048200070131     c                   else
048300070131     c                   eval      fnarbds = fnarbds£
048400070130     c                   end
048500070131     c                   end
048600070209     c                   if        (%parms > 1 and $arbnsp <>0)
048700070131     c                   eval      fnarbds = fnarbds$
048800070131     c                   end
048900070131     c*
049000070130     c                   if        %parms=1 or
049100070131     c                             (%parms = 3 and $argnsp = 0)
049200070131     c                   if        not %open(fiarg01l)
049300070130     c                   open      fiarg01l
049400070131     c                   end
049500070130     c     karb          chain     fiarg01l
049600070131     c                   if        not %found(fiarg01l)
049700070131     c                   clear                   fiargds
049800070131     c                   else
049900070131     c                   eval      fiargds = fiargds£
050000070131     c                   end
050100070130     c                   end
050200070131     c                   if        (%parms = 3 and $argnsp <>0)
050300070131     c                   eval      fiargds = fiargds$
050400070131     c                   end
050500070131     c*
050600070131     c                   if        arbnsp = 0
050700070131     c                   eval      d16msgo= 'Spedizione non trovata'
050800070131     c                   exsr      srfine
050900070131     c                   end
051000070130     c*
051100070130     C                   eval      tblcod = '3A'
051200070130     C                   eval      tblkey = arbcbo
051300070130     C     ktab          chain     Tabel00f
051400070130     c                   if        %found(tabel00f)
051500070130     C                   MOVEL     TBLUNI        DS3a
051600070130     c                   else
051700070130     c                   clear                   ds3a
051800070130     c                   end
051900070213     c* gg
052000070213     c                   if        d16dfvi = 0
052100070213     c                   move      udate         dtaiso
052200070213     c                   else
052300070213     c                   move      d16dfvi       dtaiso
052400070213     c                   end
052500070213     c                   move      dtaiso        oggi
052600070213     C                   Z-ADD     oggi          WMMGG             4 0
052700070213     C                   Z-ADD     oggi          WG                2 0
052800070213     C                   MOVEL     oggi          clnann
052900070213     C                   Z-ADD     0             CLNTFP
053000070213     C                   Z-ADD     0             CLNTFA
053100070213     C                   MOVEL     WMMGG         clnmes
053200070213     C                   OPEN      AZCLN01L
053300070213     C     KCLN          CHAIN     AZCLN01L                           31
053400070213     C                   MOVEA     CLNMAT        MES
053500070213     C                   MOVE      MES(WG)       WGG               1
053600070213     C                   close     AZCLN01L
053700110905     c* perferct order: xx gg di attesa per trazione O se non presente
053800110905     c* data trazione xx gg di attesa da data spedizione
053900130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
054000130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
054100130221     c********           if        arbxco <> ' '
054200130221     c                   if        arbxco > '0'
054300110905     C                   eval      tbecod = 'XCO'
054400110905     C                   eval      tbeke1 = 'DTA'
054500110905     C     ktbe          chain     Tntbe01l
054600110905     c                   if        %found(tntbe01l)
054700110905     C                   MOVEL     TBeUNI        Dxcodta
054800110905     c                   else
054900110905     c                   clear                   dxcodta
055000110905     c                   end
055100110905     c                   clear                   trul52ds
055200110905     c                   if        arbdti <> 0
055300110905     C                   Z-ADD     §XCODDTI      D52GIO                         *
055400110905     C                   MOVE      arbdti        D52DIN                         *
055500110905     c                   else
055600110905     C                   Z-ADD     §XCODdsp      D52GIO                         *
055700110905     C                   movel     arbAAS        d52din
055800110905     C                   move      arbmgs        d52din
055900110905     c                   end
056000110727     C                   CALL      'TRUL52R'
056100110727     C                   PARM                    TRUL52ds
056200110727     C                   MOVE      D52DFI        DAT1GG            8 0          *
056300110905     c                   end
056400070302     c* aggancio record L di fiar4 x reperire i blocchi di consegna
056500070302     C                   MOVEL     'L'           ar4trc
056600070302     C     Kfiar4        CHAIN     FiAR401L
056700070302if  1C                   if        %found(fiar401l)
056800070302     c                   eval      dsbl4l = ar4not
056900070302     c                   else
057000070302     c                   clear                   dsbl4l
057100070302     c                   end
057200070130     c*
057300070130     c                   endsr
057400061002      *---------------------------------------------------------------*
057500061002      * Reperimento Dati del job (Utente/Operativi)                   *
057600061002      *---------------------------------------------------------------*
057700061002     c     DatiJob       BEGSR
057800061002      *
057900061002     c     *dtaara       define    §azute        azuteds
058000061002     c     *dtaara       define    §datiute      ddatiute
058100061002      *
058200061002     c                   in(E)     *dtaara
058300061002     c                   IF        %ERROR or RSUT = *blanks
058400061002     c                   clear                   Tibs34Ds
058500061002     c                   call      'TIBS34R'
058600061002     c                   parm                    Tibs34Ds
058700061002     c                   in        *dtaara
058800061002     c                   ENDIF
058900061002      *
059000061002     c                   ENDSR
