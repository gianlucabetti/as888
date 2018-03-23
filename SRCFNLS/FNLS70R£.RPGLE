000100050805     h decedit('0,') datedit(*dmy/)
000200050805
000300110111      * FNLS70R *-----------------------------------------------------*
000400110111      * CANCELLAZIONE DI EDIVAD/VAT/VAX SENZA IL RELATIVO EDIVAB      *
000500050805      *---------------------------------------------------------------*
000600050805
000700110111     fedivab4L  if   e           k disk
000800050805      *
000900110111     fedivad1L  Uf   e           k disk
001000110111     fedivat1L  Uf   e           k disk
001100110111     fedivax1L  Uf   e           k disk
001200050805
001300050808      *
001400050805      * STRUTTURE DATI -----------------------------------------------*
001500050808      *
001600050805     d KPJBA         e ds
002700050805      *
003600050805      * VARIABILI ----------------------------------------------------*
003700050808      *
004000050805     d Kfgs            s                   like(VABfgs) inz
004300050808      * - Flags booleani
004400050805     d $Found          s              1    inz(*off)
004800050805      *
004900050805      * KEY - LIST ---------------------------------------------------*
005000050808      *
005500110111     c     K04VAB04      klist
005600110111     c                   kfld                    VABcmr
005700110111     c                   kfld                    VABccm
005800110111     c                   kfld                    VABcnt
005801110111     c                   kfld                    VABaas
005802110111     c                   kfld                    VABlnp
005803110111     c                   kfld                    VABnrs
005900110111     c                   kfld                    VABnsp
006000050805
006100050805      *---------------------------------------------------------------*
006200050805
006300050805     c     *Entry        plist
006400050805     c                   parm                    KPJBA
006500050805      *
008000050808      * - Elaborazione FIVAD01L
008100110111     c                   exsr      ElabVAD
008200050808      * - Elaborazione FIVAT01L
008300110111     c                   exsr      ElabVAT
008400050808      * - Elaborazione FIVAX01L
008500110111     c                   exsr      ElabVAX
010600050805      *
010700050808      * Fine
010800050805     c                   eval      *inLR  = *on
013700050805      *
013800050805      *---------------------------------------------------------------*
013900110111      * Elaborazione file EDIVAD1L                                    *
014000050805      *---------------------------------------------------------------*
014100110111     c     ElabVAD       BEGSR
014200050808      *
014800110111     c     *loval        setll     edivad00
014900110111     c                   read      edivad00
015900050805      *
016000050805      * Elaborazione
016100110111do  1c                   DOW       NOT %eof(edivad1L)
016600050905      *
016700050905     c                   eval      VABaas   = VADaas
016800050905     c                   eval      VABlnp   = VADlnp
016900050905     c                   eval      VABnrs   = VADnrs
017000050905     c                   eval      VABnsp   = VADnsp
017100050905     c                   eval      Kfgs     = VADfgs
017200110111     c                   eval      VABccm   = VADccm
017201110111     c                   eval      VABcmr   = VADcmr
017202110111     c                   eval      VABcnt   = VADcnt
017300110111     c                   exsr      CercaVAB
017400050905if  3c                   if        $Found   = *off
017500110111     c                   DELETE    EDIVAD00
017600050905e   3c                   endif
017700050905      *
018500110113     c                   read      EDIVAD00
019300050805      *
019400050805e   1c                   ENDDO
019500050805      *
019600050805     c                   ENDSR
019700050805      *
019800050805      *---------------------------------------------------------------*
019900110111      * Elaborazione file EDIVAT1L                                    *
020000050805      *---------------------------------------------------------------*
020100110111     c     ElabVAT       BEGSR
020200050805      *
020800110111     c     *loval        setll     edivat00
020900110111     c                   read      edivat00
021900050805      *
022000050805      * Elaborazione
022100110111do  1c                   DOW       NOT %eof(EDIVAT1L)
022600050805      *
022700050905     c                   eval      VABaas   = VATaas
022800050905     c                   eval      VABlnp   = VATlnp
022900050905     c                   eval      VABnrs   = VATnrs
023000050905     c                   eval      VABnsp   = VATnsp
023100050905     c                   eval      Kfgs     = VATfgs
023200110111     c                   eval      VABccm   = VATccm
023201110111     c                   eval      VABcmr   = VATcmr
023202110111     c                   eval      VABcnt   = VATcnt
023300110111     c                   exsr      CercaVAB
023400050905if  3c                   if        $Found   = *off
023500110111     c                   DELETE    edivat00
023600050905e   3c                   endif
023700050905      *
024500110111     c                   read      EDIVAT00
025300050805      *
025400050805e   1c                   ENDDO
025500050805      *
025600050805     c                   ENDSR
025700050805      *
025800050805      *---------------------------------------------------------------*
025900110111      * Elaborazione file EDIVAX1L                                    *
026000050805      *---------------------------------------------------------------*
026100110111     c     ElabVAX       BEGSR
026200050805      *
026800110111     c     *loval        setll     EDIVAX00
026900110111     c                   read      EDIVAX00
027900050805      *
028000050805      * Elaborazione
028100110111do  1c                   DOW       NOT %eof(EDIVAX1L)
028600050805      *
028700050905     c                   eval      VABaas   = VAXaas
028800050905     c                   eval      VABlnp   = VAXlnp
028900050905     c                   eval      VABnrs   = VAXnrs
029000050905     c                   eval      VABnsp   = VAXnsp
029100050905     c                   eval      Kfgs     = VAXfgs
029200110111     c                   eval      VABccm   = VAXccm
029201110111     c                   eval      VABcmr   = VAXcmr
029202110111     c                   eval      VABcnt   = VAXcnt
029300110111     c                   exsr      CercaVAB
029400050905if  3c                   if        $Found   = *off
029500110111     c                   DELETE    EDIVAX00
029600050905e   3c                   endif
029700050905      *
030500110111     c                   read      EDIVAX00
031300050805      *
031400050805e   1c                   ENDDO
031500050805      *
031600050805     c                   ENDSR
031700050808      *
031800050808      *---------------------------------------------------------------*
031900110111      * Ricerca record corrispondenti in EDIVAB4L                     *
032000050808      *---------------------------------------------------------------*
032100110111     c     CercaVAB      BEGSR
032200050808      *
032300050808     c                   eval      $Found = *off
032400050808      *
032500110111     c     K04VAB04      setll     EDIVAB00
032600110111     c     K04VAB04      reade     EDIVAB00
032700050808      *
032800110111do  1c                   DOW       NOT %eof(EDIVAB4L)
032900050808      *
033100110111     c                   if        VABfgs = Kfgs
033200050808     c                   eval      $Found = *on
033300050808     c                   leave
033400050808e   2c                   endif
033500050808      *
033600110111     c     K04VAB04      reade     edivab00
033700050808      *
033800050808e   1c                   ENDDO
033900050808      *
034000050808     c                   ENDSR
