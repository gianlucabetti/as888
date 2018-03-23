000100080812     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200980323     H*------------------------------------------------------------------------*
000300080805     H* PREP. FILE WORK WFAFU00F PER AUMENTO TARIFFE FUEL TARIFFE 2008         *
000400031103     H*                                                                        *
000500031103     H*                                                                        *
000600980323     H*------------------------------------------------------------------------*
000700980323     F*
000800080812     FWfafu00f  uf a e             DISK
000900990614      *
001000031103
001100031103      *------------------------------------------------------------------------*
001200031103
001300031103     D W0140           S             14  0
001400031103     D Wdtgio          S              8  0
001500031103     D datcor          S              8  0
001600031103     D x               S              3  0
001700080812     D §caso           S              1
001800080812     D new_padre       S              7  0
001900080813     D sv_cod          S              7  0
002000080813     D sv_rag          S             48
002100061124
002200061124      *  S C H I E R E
002300061124
002400031106     D SKC07           S              7  0 DIM(500)
002500031103
002600031103      *   D S   I N T E R N E / E S T E R N E
002700031103
002800031103     D WLBDAT          DS                  INZ
002900031103     D  G02DAT                 1      8  0
003000031103     D  G02INV                 9     16  0
003100031103     D  G02ERR                17     17
003200031103     D  G02TGI                18     22  0
003300071026
003400031103      * Parametri per pgm. TIBS10R (reperimento codice padre cliente)
003500031103     D TIBS10ds      e ds                  inz
003600031106     D   skc11                21   5520  0 DIM(500)
003700031103
003800031103      * Parametri per pgm. TIBS69R (decodifica cliente)
003900031103     D TIBS69ds      e ds                  inz
004000031103     D CNACOds       e ds                  extname(CNACO00F) inz
004100031103     D CNINDds       e ds                  extname(CNIND00F) inz
004200031103     D CNCLPds       e ds                  extname(CNCLP00F) inz
004300031103     D FNCLSds       e ds                  extname(FNCLS00F) inz
004400031103
004500080805      * file fuel
004600080805     d fuelds        e ds                  extname(fuel)
004700080812      * file fuel no
004800080812     d fuelnods      e ds                  extname(fuelno)
004900031103      *------------------------------------------------------------------------*
005000031103
005100080805     C*
005200080805     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni
005300080805     C
005400080805     C/EXEC SQL
005500080805     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005600080805     C/END-EXEC
005700080805     C*
005800080805
005900080805     c/exec SQL
006000080805     c+ declare C1 cursor for
006100080805     c+ select * from fuel
006200080805     c+ order by codcli
006300080805     c+ for read only
006400080805     c/end-exec
006500080805
006600080805     c/exec sql
006700080805     c+ open c1
006800080805     c/end-exec
006900080805
007000080805     c/exec SQL
007100080805     c+ Fetch next from C1 into :fuelds
007200080805     c/end-exec
007300080805
007400080805      * in fase di lettura
007500080812    1c                   dow       sqlcod = *zeros
007600080805
007700080812     c                   clear                   x
007800080812     c                   clear                   Skc07
007900080812     c                   clear                   new_padre
008000080812     c                   clear                   §caso
008100080805      *
008200080805      * cerco i clienti figli del cliente unificante attraverso il tibs10
008300080805      *
008400080805     c                   clear                   tibs10ds
008500080805     c                   eval      d10tle = 'ST'
008600080805     c                   eval      d10paf = 'F'
008700080805     c                   z-add     codcli        d10cod
008800080805     c                   CALL      'TIBS10R'
008900080805     c                   parm                    tibs10ds
009000080805     c                   parm      'A'           partpexe          1
009100080812      * se non ci sono errori elaboro i suoi figli
009200080812    2c                   if        d10err = ' '
009300080812     c                   eval      §caso = '1'
009400080812      * muovo in una schiera di 7
009500080812     c                   z-add     skc11         skc07
009600080812      *
009700080812      * elaboro tutti i clienti figli
009800080812      *
009900080812     c                   exsr      SR_elabora
010000080812    2c                   endif
010100080805      * se ci sono errori quindi non risulta padre  devo verificare se è figlio di altro
010200080805      * codice unificante
010300080812    2c                   if        d10err <> ' '
010400080805     c                   clear                   tibs10ds
010500080805     c                   eval      d10tle = 'ST'
010600080805     c                   eval      d10paf = 'P'
010700080805     c                   z-add     codcli        d10cod
010800080805     c                   CALL      'TIBS10R'
010900080805     c                   parm                    tibs10ds
011000080805     c                   parm      'A'           partpexe          1
011100080812      * se ci sono errori elabora se stesso non ha legami
011200080812    3c                   if        d10err <> ' '
011300080812     c                   eval      §caso = '1'
011400080812      * muovo in una schiera di 7
011500080812     c                   clear                   skc07
011600080812     c                   z-add     codcli        skc07(1)
011700080812      *
011800080812      * elaboro tutti i clienti figli
011900080812      *
012000080812     c                   exsr      SR_elabora
012100080812    3c                   endif
012200080805      * se non ci sono errori verifico se già presente il suo padre nell'archivio fuel
012300080805      * perchè se già presente il codice verrà poi elaborato con le stesse condizioni del
012400080805      * codice padre
012500080812    3c                   if        d10err = ' '
012600080813      * mi salvo il nuovo padre , codice originale e ragione sociale originale
012700080812     c                   z-add     d10cop        new_padre
012800080813     c                   eval      sv_cod =  codcli
012900080813     c                   eval      sv_rag =  ragsoc
013000080805     c/exec sql
013100080805     c+ select * into :fuelds  from fuel
013200080812     c+ where codcli = :new_padre
013300080805     c/end-exec
013400080812      * codice padre già trovato non faccio nulla
013500080812    4c                   if        sqlcod = 0
013600080812     c                   eval      §caso = '2'
013700080812     c                   exsr      Sr_elabora
013800080812
013900080812      * se nuovo codice padre non è presente verifico se è presente nel file fuelno (no aumento)
014000080812   x4c                   else
014100080812
014200080812     c/exec sql
014300080812     c+ select * into :fuelnods  from fuelno
014400080812     c+ where codclino = :new_padre
014500080812     c/end-exec
014600080812      * codice padre trovato nel file fuelno non faccio nulla
014700080812    5c                   if        sqlcod = 0
014800080812     c                   eval      §caso = '3'
014900080812     c                   exsr      Sr_elabora
015000080812
015100080812      * se non trovato neppure nel file fuelno elaboro se stesso e basta
015200080812     c                   else
015300080812
015400080812     c                   eval      §caso = '4'
015500080812     c                   exsr      Sr_elabora
015600080812
015700080812   e5c                   endif
015800080805
015900080812   e4c                   endif
016000080812
016100080812   e3c                   endif
016200080805
016300080812   e2c                   endif
016400080805
016500080805     c/exec sql
016600080805     c+ Fetch next from C1 into :fuelds
016700080805     c/end-exec
016800080812   e1c                   enddo
016900080805     c
017000080805     c/exec sql
017100080805     c+ close C1
017200080805     c/end-exec
017300031103      *
017400031103     c                   CALL      'TIBS10R'
017500031103     c                   parm                    tibs10ds
017600031103     c                   parm      'C'           partpexe
017700031103      *
017800031103     c                   seton                                        lr
017900031103      *---------------------------------------------------------------------------------------------
018000080812      * SR_ELABORA Elaborazione dei singoli codici
018100031103      *---------------------------------------------------------------------------------------------
018200080812     C     SR_ELABORA    BEGSR
018300080812
018400080812      * verifico i casi
018500080812      * CASO 1  Elaborazione normale sia del padre che dei suoi figli se ne ha
018600080812     c                   IF        §caso = '1'
018700080812
018800080812     c                   do        500           x
018900080812      * al primo cliente uguale a zero   esco dal ciclo
019000080812     c                   if        skc07(x) = *zeros
019100080812     c                   leave
019200080812     c                   endif
019300080812      * pulisco il record
019400080812     c                   clear                   WFAFU000
019500080812      * aggancio CNACO
019600080812      *
019700080812      * aggancio anagrafiche
019800080812     C                   clear                   TIBS69ds
019900080812     C                   clear                   CNACOds
020000080812     C                   clear                   CNINDds
020100080812     C                   clear                   CNCLPds
020200080812     C                   clear                   FNCLSds
020300080812     C                   z-add     skc07(x)      I69Kac
020400080812     C                   call      'TIBS69R'
020500080812     C                   parm                    TIBS69ds
020600080812     C                   parm                    CNACOds
020700080812     C                   parm                    CNINDds
020800080812     C                   parm                    CNCLPds
020900080812     C                   parm                    FNCLSds
021000080812      * valorizzo i campi del file
021100080812     c                   eval      Wfaclf = skc07(x)
021200080812     c                   eval      Wfaclu = Codcli
021300080812     c                   eval      Wfarag = Acorag
021400080812      * scrivo il record
021500080812     c                   write     wfafu000
021600080812      *
021700080812     c                   enddo
021800080812      *
021900080812     c                   endif
022000080812      * CASO 2  Il nuovo codice padre è presente nel file FUEL viene elaborato con il suo nuovo
022100080812      *         padre quindi non devo aumentare
022200080812     c                   IF        §caso = '2'
022300080812
022400080812      * pulisco il record
022500080812     c                   clear                   WFAFU000
022600080812      * valorizzo i campi del file
022700080813     c                   eval      Wfaclf = sv_cod
022800080813     c                   eval      Wfaclu = sv_cod
022900080813     c                   eval      Wfarag = sv_rag
023000080812     c                   eval      Wfadcl = 'Presente come figlio di ' +
023100080812     c                                      %trim(%editc(new_padre:'Z')) +
023200080812     c                                      ' (nuovo codice padre presente in +
023300080812     c                                      FUEL)'
023400080812     c                   eval      Wfaaum = 'N'
023500080812      * scrivo il record
023600080812     c                   write     wfafu000
023700080812      *
023800080812     c                   endif
023900080812      * CASO 3  Il nuovo codice padre è presente nel file FUELNO non viene elaborato nulla
024000080812
024100080812     c                   IF        §caso = '3'
024200080812
024300080812      * pulisco il record
024400080812     c                   clear                   WFAFU000
024500080812      * valorizzo i campi del file
024600080812     c                   eval      Wfaclf = Codcli
024700080812     c                   eval      Wfaclu = Codcli
024800080812     c                   eval      Wfarag = Ragsoc
024900080812     c                   eval      Wfadcl = 'Presente come figlio di ' +
025000080812     c                                      %trim(%editc(new_padre:'Z')) +
025100080812     c                                      ' (nuovo codice padre presente in +
025200080812     c                                      FUELNO)'
025300080812     c                   eval      Wfaaum = 'N'
025400080812      * scrivo il record
025500080812     c                   write     wfafu000
025600080812      *
025700080812     c                   endif
025800080812
025900080812      * CASO 3  Il nuovo codice padre non è presente nel file FUEL e nel file FUELNO  elaboro
026000080812      *         solo lui e non i suoi fratelli
026100080812     c                   IF        §caso = '4'
026200080812
026300080812      * pulisco il record
026400080812     c                   clear                   WFAFU000
026500080812      * valorizzo i campi del file
026600080812     c                   eval      Wfaclf = Codcli
026700080812     c                   eval      Wfaclu = Codcli
026800080812     c                   eval      Wfarag = Ragsoc
026900080812     c                   eval      Wfadcl = 'Presente come figlio di ' +
027000080812     c                                      %trim(%editc(new_padre:'Z')) +
027100080812     c                                      ' che non esiste nel file FUEL e +
027200080812     c                                      FUELNO elaboro solo lui e non i +
027300080812     c                                      suoi fratelli'
027400080812     c                   eval      Wfaaum = ' '
027500080812      * scrivo il record
027600080812     c                   write     wfafu000
027700080812      *
027800080812     c                   endif
027900030707      *
028000031103     c                   endsr
028100080812      *
028200031103      *****************************************************************
028300031103      * ROUTINE INIZIALE
028400031103      *****************************************************************
028500031103     C     *INZSR        BEGSR
028600031103
028700031103      * reperisce data del giorno
028800031103     C                   TIME                    W0140
028900031103      * UDATE IN GGMMAAAA
029000031103     C                   MOVE      W0140         WDTGIO
029100031103      * UDATE IN AAAAMMGG
029200031103     C                   Z-ADD     WDTGIO        G02DAT
029300031103     C                   MOVEL     *BLANK        G02ERR
029400031103     C                   CALL      'XSRDA8'
029500031103     C                   PARM                    WLBDAT
029600031103     C                   Z-ADD     G02INV        datcor
029700051108
029800031103     C                   endsr
