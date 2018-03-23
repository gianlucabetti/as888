000100060920      *---------------------------------------------------------------*
000200070209      * Da FIPSP00F accoda in FIBAR00F                                *
000300070209      *       e scrive file di log FIPSL00F                           *
000400060920      *---------------------------------------------------------------*
000500060920
000600061218     h decedit('0,')
000700040422
000800060920      *---------------------------------------------------------------*
000900060920
001000060920     fAZORG01L  if   e           k disk
001100071016     ftntbe01l  if   e           k disk
001200070209     fFIPSP01L  UF   e           k disk
001300070209     fFIBAR00F  O    e             disk
001400080116     fFIPSL01L  if   e           k disk
001500080116     F                                     RENAME(fipsl000:Fipsl001)
001600070209     fFIPSL00F  O    e             disk
001700040422
001800060920      *---------------------------------------------------------------*
001900060920
002000060920      *
002100060920      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
002200060920      *
002300060920      * - Calcolo Check-Digit
002400060920     d TRUL28ds      e ds                  inz
002500071016
002600071016     d og143         e ds
002700060920      *
002800061219      * - Campi Formato Record
002900070209     d FIpslds         ds                  inz
003000070209     d  pslnpg                 1      1
003100070209     d  pslnfv                 2     10
003200070209     d  pslbarco              11     28
003300070209     d  pslcan                29     29
003400070209     d  psldup                30     30
003500070209     d  pslman                31     31
003600070209     d  pslusr                32     41
003700070209     d  psldtorar             42     55
003800070209     d  pslmilles             56     58
003900071005     d  pslindip              59     70
004000071005     d  pslfiller             71    100
004100061219
004200061219      * - Segnacollo
004300061219     d wBarCode        ds                  inz
004400061219     d  wBar_LNP               1      3    inz
004500061219     d  wBar_LNA               4      6    inz
004600061219     d  wBar_NRS               7      8    inz
004700061219     d  wBar_NSC               9     15    inz
004800061219     d  wBar_ZNC              16     17    inz
004900061219     d  wBar_bar               1     17    inz
005000061219     d  wBar_CHK              18     18    inz
005100060920      * - Controllo data
005200060920     d WLBda8          ds                  inz
005300060920     d  G08dat                        8  0 inz
005400060920     d  G08inv                        8  0 inz
005500060928     d  G08err                        1    inz('3')
005600060920     d  G08tgi                        5  0 inz
005700060920      *
005800060920      *
005900060920      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
006000070209     d wnps            s              2
006100080116     d werrlin         s              1
006200080116     d werriip         s              1
006300070213     d comman          S            300                                         *comando CLP
006400070213     d lenght          S             15  5 INZ(%len(comman))                    *lunghezza coman CLP
006500071016     d parpo_n         s                   like(orgfil)
006600071016     d kcod            s                   like(tbecod)
006700071016     d kke1            s                   like(tbeke1)
006800071016     d kke2            s                   like(tbeke2)
006801110609
006802110609     d dataisocur      s               d   datfmt(*iso)
006803110609     d dataisospu      s               d   datfmt(*iso)
006804110609     d num_days        s             15p 0
006900060920      *
007000060920      *
007100060920      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
007200060920      *
007300060920
007400060920      *===============================================================*
007500060920      *?  I N D I C A T O R I                                        ?*
007600060920      *---------------------------------------------------------------*
007700060920      *===============================================================*
007800070209     c     *entry        plist
007900070209     c                   parm                    parpo             3
008000070209     c                   parm                    flag              1
008100070213     c                   parm                    parspu            1
008200071009     c                   parm                    sysnam            8
008300070209     c
008400070209     c                   clear                   flag
008500070213     c                   clear                   parspu
008600071016
008700071016     c     ktbe          klist
008800071016     C                   kfld                    kcod
008900071016     C                   kfld                    kke1
009000071016     C                   kfld                    kke2
009100080116
009200080116     c     kpsl01        klist
009300080116     C                   kfld                    pslfil
009400080116     C                   kfld                    psldtorar
009500080116     C                   kfld                    pslmilles
009600080116     C                   kfld                    psliddisp
009601110609     c
009602110609     c                   time                    dataisocur
009700060920      *
009800060920      * Ciclo di lettura spunte
009900070209     c     parpo         setll     Fipsp01l
010000061218     c                   do        *hival
010100070209     c     parpo         reade     Fipsp01l
010200070209     c                   if        %eof(fipsp01l)
010300061218     c                   leave
010400061218     c                   endif
010500060920      *
010600060920      *
010700070209      * - Valorizzazione campi del file di Log (FIpsl00F)
010800070209     c                   exsr      sr_Riefipsl
010900080116     c                   clear                   werrlin
011000080116     c                   clear                   werriip
011100080116     c*
011200080116     c     kpsl01        setll     fipsl01l
011300080116     c                   if        %equal(fipsl01l)
011400080116     c* memorizzo la presenza di altro record con stessa chiave nel
011500080116     c* file di log
011600080116     c                   eval      werrlin='1'
011700080116     c                   endif
011800060920      *
011900070209     c* - Controllo del contenuto del record e se ok scrivo fibarxxx
012000061218     c                   exsr      sr_chkspu
012100080116     c                   if        *in99=*off and werrlin=*blanks
012200061219     c                   write     barco
012300070213     c                   eval      parspu='S'
012400061219     c                   else
012500080116     c                   select
012600080116     c* errore indirizzo ip
012700080116     c                   when      werriip='1'
012800080117     c                   eval      pslerr='I'
012900080116     c* spunta non valida
013000080116     c                   when      *in99
013100070209     c                   eval      pslerr='E'
013200080116     c* errore linea
013300080116     c                   when      werrlin='1'
013400080117     c                   eval      pslerr='L'
013500080116     c                   other
013600080116     c                   eval      pslerr='E'
013700080116     c                   endsl
013800061218     c                   endif
013900060920      *
014000061219      * - Scrittura del Log
014100070209     c                   WRITE     FIpsl000
014200060920      * - Cancellazione record
014300070209     c                   delete    fipsp000
014400060920      *
014500060920e   1c                   ENDDO
014600070209     c
014700070209     c                   feod      fibar00f
014800070209     c                   shtdn                                        98
014900070209     c                   if        not *in98
015000070209     c                   seton                                        rt
015100070209     c                   else
015200070209     c                   eval      flag='1'
015300070209     c                   seton                                        lr
015400070209     c                   endif
015500060920      *
015600060921
015700061219      *---------------------------------------------------------------*
015800061219      *?Rimepimento campi del file di log                            ?*
015900061219      *---------------------------------------------------------------*
016000070209     c     SR_riefipsl   BEGSR
016100070209     c                   movel     pspprfc       pslprfc
016200070209     c                   movel     pspiddisp     psliddisp
016300070209     c                   movel     pspdati       fipslds
016400070209     c                   move      pspdatora     psldatora
016500070209     c                   clear                   pslerr
016600070306     c                   move      parpo         pslfil
016700061219     c                   endsr
016800061219      *---------------------------------------------------------------*
016900061219      *?Controllo validità della spunta                              ?*
017000061219      *---------------------------------------------------------------*
017100061219     c     SR_chkspu     BEGSR
017200061219     c                   eval      *in99   = *off
017300070209     c                   clear                   barco
017400071016      * Indirizzo IP
017500100616      * .... Prima terzina - Se diversa da '010' l'ip è generico quindi
017600100616      *                      continuo con gli altri controlli
017700071016     c                   if        %subst(pslindip:1:3)<>'010'
017800100616     c                   goto      noip
017900071016     c                   endif
018000071016      * .... Seconda terzina - Se non è il sistema di prova scarto sempre
018100071016      *                        se 46 o 68
018200071016    1c                   if        sysnam<>'AS888   '
018300071016     c                             and (%subst(pslindip:4:3)='046' or
018400071016     c                             %subst(pslindip:4:3)='068')
018500071016     c                   eval      *in99   = *on
018600080116     c                   eval      werriip='1'
018700071016     c                   goto      endchkspu
018800071016    1c                   endif
018900071016      *                      - Se diversa dal p.o. di spunta verifico
019000071016      *                        su azorg se specificato un cod. ind.IP
019100080116   1ac                   if        sysnam<>'AS888   ' or
019200080116     c                             (%subst(pslindip:4:3)<>'046' and
019300080116     c                              %subst(pslindip:4:3)<>'068')
019400071016    1c                   if        parpo <> %subst(pslindip:4:3)
019500071016     c                   move      parpo         parpo_n
019600071016     c     parpo_n       chain     azorg01l
019700071016     C                   movel     orgde3        og143
019800071016      *                      - Se non specificato un codice oppure è
019900071016      *                        ancora diverso controllo sulla tabella
020000071016      *                        delle abilitazione se p.o. di scarico
020100071016      *                        abilitato
020200071016    2c                   if        §ogiip = *blanks  or
020300071016     c                             §ogiip <> %subst(pslindip:4:3)
020400071016     c                   eval      kcod='APR'
020500071016     c                   clear                   kke1
020600071016     c                   clear                   kke2
020700071016    3c                   if        §ogiip<>*blanks
020800071016     c                   movel     §ogiip        kke1
020900071016   x3c                   else
021000071016     c                   movel     parpo         kke1
021100071016    3c                   endif
021200071016     c                   eval      kke2=%subst(pslindip:4:3)
021300071017     c     ktbe          chain     tntbe01l
021400071017    3c                   if        not %found(tntbe01l) or tbeatb<>*blanks
021500071016     c                   eval      *in99   = *on
021600080116     c                   eval      werriip='1'
021700071016     c                   goto      endchkspu
021800071016    3c                   endif
021900071016    2c                   endif
022000071016    1c                   endif
022100080116   1ac                   endif
022200100616
022300100616     c     noip          tag
022400071016
022500061219      * Controlla il numero foglio
022600061219     c                   eval      *in10   = *off
022700070209     c                   testn                   pslnfv               10
022800061219if  1c                   if             *in10  = *on
022900070209     c                             and  %subst(pslnfv: 9: 1) >= *zero
023000070209     c                             and  pslnfv > *zeros
023100061219     c                   clear                   BARnfv
023200070209     c                   move      pslnfv        BARnfv
023300061219x   1c                   else
023400061219     c                   eval      *in99   = *on
023500071016     c                   goto      endchkspu
023600061219e   1c                   endif
023700061219     c* Categoria foglio
023800070209     c                   if        pslnpg > *zeros
023900070209     c                   move      pslnpg        barnpg
024000061219     c                   else
024100061219     c                   clear                   barnpg
024200061219     c                   endif
024300061219     c* controlla il barcode
024400070209     c                   move      pslbarco      wbarcode
024500061219     c                   exsr      sr_ctrbarco
024600061219     c                   if        not *in99
024700061219     c                   movel     wBAR_lnp      BARlnp
024800061219     c                   movel     wBAR_lna      BARlna
024900061219     c                   movel     wBAR_nrs      BARnrs
025000061219     c                   movel     wBAR_nsc      BARnsc
025100061219     c                   movel     wBAR_znc      BARznc
025200061219     c                   movel     wBAR_chk      BARctr
025300071016     c                   else
025400071016     c                   goto      endchkspu
025500061219     c                   endif
025600061219     c* controlla data e ora/millesecondi spunta
025700061219     c                   exsr      Ctr_time
025800071016     c   99              goto      endchkspu
025900061219     c* duplicato S/N
026000070209     c                   if        psldup<>'S' and psldup<>'N'
026100061219     c                   eval      *in99=*on
026200071016     c                   goto      endchkspu
026300070209     c                   else
026400070209     c                   move      psldup        bardup
026500061219     c                   endif
026600061219     c* manuale S/N
026700070209     c                   if        pslman<>'S' and pslman<>'N'
026800061219     c                   eval      *in99=*on
026900071016     c                   goto      endchkspu
027000070209     c                   else
027100070209     c                   move      pslman        barman
027200061219     c                   endif
027300070209     c* pistola
027400070209     c                   clear                   wnps
027500070213     c* PROFILO "PR..."
027600070711    0c                   if        %subst(pslusr:1:2)='PR'
027700070711     c* il numero è nel profilo --> se non è numerico --> err
027800070711     c                   eval      wnps= %subst(pslusr:6:2)
027900070209     c                   eval      *in10   = *off
028000070209     c                   testn                   wnps                 10
028100070209if  1c                   if             *in10   = *on
028200070209     c                             and  %subst(wnps:2:1) >= *zero
028300070209     c                             and  wnps > *zeros
028400070209     c                   move      wnps          barnps
028500070213    2c                   if        barman='S'
028600070213     c                   add       50            barnps
028700070213     c* se ottengo un numero > 79 raggruppo su pistola manuale generica 79
028800070213    3c                   if        barnps>79
028900070213     c                   z-add     79            barnps
029000070213    3c                   endif
029100070213    2c                   endif
029200070213   x1c                   else
029300070209     c                   eval      *in99   = *on
029400071016     c                   goto      endchkspu
029500070213    1c                   endif
029600070213   x0c                   else
029700070213     c* PROFILO "AU..." --> pistola generica 44
029800070711     c                   if        %subst(pslusr:1:2)='AU'
029900070213     c                   z-add     44            barnps
030000070213     c                   endif
030100070213    0c                   endif
030200070213     c                   if        barnps = *zeros
030300070213     c                   eval      *in99   = *on
030400071016     c                   goto      endchkspu
030500070213     c                   endif
030600070213     c
030700070209     c* tipo anomalia
030800070209     c                   move      pslcan        barcan
030900070209     c* profilo utente
031000070216     c                   movel     pslusr        barpru
031100070209     c
031200070209     c* tipo apparato spunta: P=PDA
031300070209     c                   movel     'P'           bartap
031400071016     c     endchkspu     endsr
031500061219      *---------------------------------------------------------------*
031600061219      *?Controllo validità del barcode                               ?*
031700061219      *---------------------------------------------------------------*
031800061219     c     SR_ctrbarco   BEGSR
031900061219      * Controlla se dati numerici
032000061219     c                   eval      *in10   = *off
032100061219     c                   testn                   wbarcode             10
032200061219if  1c                   if             *in10   = *off
032300061219     c                             or   %subst(wbarcode:18:1) < *zero
032400061219     c                   eval      *in99   = *on
032500061219     c                   goto      endbarcod
032600061219e   1c                   endif
032700061219      *
032800061219      * Controlla se campi valorizzati
032900061219sel 1c                   select
033000061219w   1c                   when      wbar_lnp=*zeros
033100061219     c                   eval      *in99     = *on
033200061219w   1c                   when      wbar_lna=*zeros
033300061219     c                   eval      *in99     = *on
033400061219w   1c                   when      wbar_nsc=*zeros
033500061219     c                   eval      *in99     = *on
033600061219e   1c                   endsl
033700061219     c     *in99         cabeq     *on           endbarcod
033800061219      *
033900061219      * Controlla validità linea di partenza
034000061219     c                   clear                   ORGfag
034100061219     c                   movel     wBAR_lnp      ORGfil
034200061219     c     ORGfil        chain     AZORG
034300061219if  1c                   if        NOT  %found(AZORG01L)
034400061219     c                             or  (ORGfag <> 'A'
034500061219     c                              and ORGfag <> 'F')
034600061219     c                   eval      *in99   = *on
034700061219     c                   goto      endbarcod
034800061219e   1c                   endif
034900061219      *
035000061219      * Controlla validità linea di arrivo
035100061219     c                   clear                   ORGfag
035200061219     c                   movel     wBAR_lna      ORGfil
035300061219     c     ORGfil        chain     AZORG
035400061219if  1c                   if        NOT  %found(AZORG01L)
035500061219     c                             or  (ORGfag <> 'A'
035600061219     c                              and ORGfag <> 'F')
035700061219     c                   eval      *in99   = *on
035800061219     c                   goto      EndBarCod
035900061219e   1c                   endif
036000061219      *
036100061219      * Controlla validità check digit
036200061219     c                   clear                   TRUL28ds
036300061219     c                   eval      I28tla  = 'C'
036400061219     c                   eval      I28mod  = 'BAR'
036500061219     c                   eval      I28cod  = wBAR_bar
036600061219     c                   call      'TRUL28R1'
036700061219     c                   parm                    TRUL28ds
036800061219if  1c                   if             O28err <> *blanks
036900061219     c                             or   O28ckd <> wBAR_chk
037000061219     c                   eval      *in99   = *on
037100061219     c                   goto      EndBarCod
037200061219e   1c                   endif
037300061219      *
037400061219     c     EndBarCod     tag
037500061219     c                   endsr
037600061219      *---------------------------------------------------------------*
037700061219      *?Controllo data e ora spunta                                  ?*
037800061219      *---------------------------------------------------------------*
037900061219     c     Ctr_Time      BEGSR
038000061219      *
038100070209      * Controllo campi data e ora spunta da portare su FIBAR
038200061219     c                   eval      *in10   = *off
038300070209     c                   testn                   psldtorar            10
038400061219if  1c                   if             *in10  = *on
038500070209     c                             and  %subst(psldtorar:14:1) >= *zero
038600070209     c                             and  psldtorar > *zeros
038700061219     c                   reset                   WLBda8
038800070209     c                   movel     psldtorar     g08inv
038900061219     c                   call      'XSRDA8'
039000061219     c                   parm                    WLBda8
039100061219if  2c                   if        G08err  = *off
039101110608     c* Se la data è più vecchia di un mese significa che la pistola si è
039102110608     c* sprogrammata e in questo caso come data/ora assumo quella del lavoro
039103110609     c                   movel     psldtorar     w0080             8 0
039104110609     c                   eval      dataisospu=%date(w0080:*iso)
039105110609     c                   eval      num_days=%diff(dataisocur:dataisospu:*days)
039106110609     c                   if        num_days>30
039107110609     c                   eval      bardfs= %dec(dataisocur: *iso)
039108110609     c                   else
039200070209     c                   movel     psldtorar     bardfs
039201110609     c                   endif
039300070209     c                   move      psldtorar     w006a             6
039400061220     c                   movel     w006a         barhms
039500061219     c                   else
039600061219     c                   eval      *in99=*on
039700061219e   2c                   endif
039800061219     c                   else
039900061219     c                   eval      *in99=*on
040000061219e   1c                   endif
040100061219     c* millesecondi spunta
040200061219     c                   eval      *in10   = *off
040300070209     c                   testn                   pslmilles            10
040400061219if  1c                   if             *in10  = *on
040500070209     c                             and  %subst(pslmilles:3:1) >= *zero
040600070209     c                   move      pslmilles     barmis
040700061219     c                   else
040800061219     c                   eval      *in99 = *on
040900061219     c                   endif
041000061219      *
041100061219     c                   ENDSR
