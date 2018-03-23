000100971216     h*--------------------------------------------------------------------------------------------*
000200060530     h* Registra variazioni anagrafiche cliente
000300971216     h*--------------------------------------------------------------------------------------------*
000400020522     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000500971216     h DECEDIT('0,') DATEDIT(*DMY.)
000600971216     f*--------------------------------------------------------------------------------------------*
000700971216     f* Data base
000800060530     fcnacvd0f  o    e             disk
000900060901     fcnacvt1l  uF a e           k disk
001000971216     d*--------------------------------------------------------------------------------------------*
001100060531     d MSG             s             78    dim(2) ctdata perrcd(1)
001200060531     d datVar          s              1    dim(15)
001300060901     d k15             s              1    dim(15)
001400060530     d
001500060530     d dacv_A        e DS
001600060531     d                                     prefix(d)
001700060530     d wdacv_A       e DS                  extname(dacv_A)
001800060530     d                                     prefix(w)
001900060530     d dacv_A2       e DS
002000060531     d                                     prefix(d)
002100060530     d wdacv_A2      e DS                  extname(dacv_A2)
002200060530     d                                     prefix(w)
002300061027     d dacv_A3       e ds                  prefix(d)
002400061027     d wdacv_A3      e ds                  extname(dacv_A3)
002500061027     d                                     prefix(w)
002501171228     d dacv_A4       e ds                  prefix(d)
002502171228     d wdacv_A4      e ds                  extname(dacv_A4)
002503171228     d                                     prefix(w)
002600060530     d dacv_B        e DS
002700060531     d                                     prefix(d)
002800060530     d wdacv_B       e DS                  extname(dacv_B)
002900060530     d                                     prefix(w)
003000060530     d dacv_C        e DS
003100060531     d                                     prefix(d)
003200060530     d wdacv_C       e DS                  extname(dacv_C)
003300060530     d                                     prefix(w)
003400060530     d dacv_D        e DS
003500060531     d                                     prefix(d)
003600060530     d wdacv_D       e DS                  extname(dacv_D)
003700060530     d                                     prefix(w)
003800060530     d dacv_F        e DS
003900060531     d                                     prefix(d)
004000060530     d wdacv_F       e DS                  extname(dacv_F)
004100060530     d                                     prefix(w)
004200060530     d dacv_F2       e DS
004300060531     d                                     prefix(d)
004400060530     d wdacv_F2      e DS                  extname(dacv_F2)
004500060530     d                                     prefix(w)
004600060530     d dacv_G        e DS
004700060531     d                                     prefix(d)
004800060530     d wdacv_G       e DS                  extname(dacv_G)
004900060530     d                                     prefix(w)
005000060530     d dacv_O        e DS
005100060531     d                                     prefix(d)
005200060530     d wdacv_O       e DS                  extname(dacv_O)
005300060530     d                                     prefix(w)
005400060530     d dacv_P        e DS
005500060531     d                                     prefix(d)
005600060530     d wdacv_P       e DS                  extname(dacv_P)
005700060530     d                                     prefix(w)
005800060530     d dacv_S        e DS
005900060531     d                                     prefix(d)
006000060530     d wdacv_S       e DS                  extname(dacv_S)
006100060530     d                                     prefix(w)
006200060901     d
006300060901     d wcnacvt       e DS                  extname(cnacvt0f)
006400060901     d                                     prefix(w)
006500060901     d cnacvt        e DS                  extname(cnacvt0f)
006600060901     d
006700060531     d
006800060531     d tibs73ds      E DS                                                       *record cnaco
006900060530     d cnacods       E DS                  EXTNAME(cnaco00f)                    *record cnaco
007000971217     d cnindds       E DS                  EXTNAME(cnind00f)                    *record cnaco
007100971217     d cnclpds       E DS                  EXTNAME(cnclp00f)                    *record cnaco
007200971217     d fnclsds       E DS                  EXTNAME(fncls00f)                    *record cnaco
007300060530     d dataeur         s               d   datfmt(*eur)
007400060531     d conta           s              4  0 inz
007500060901     d yy              s              2  0
007600060901     d zz              s              2  0
007700060901     d newacvflo       s                   like(acvflo)                         *record cnaco
007800060901     d oldacvflo       s                   like(acvflo)                         *record cnaco
007900060901     d conto           s             12    inz
008000060901     d contoprima      s                   like(conto)                          *record cnaco
008100100806     d savclstic       s                   like(clstic)
008200971216     c*--------------------------------------------------------------------------------------------*
008300971216     c* Main lines
008400971216     c*--------------------------------------------------------------------------------------------*
008500060530     c     *ENTRY        PLIST
008600060530     c                   PARM                    tibs73ds                       *ds di procedura
008700060530     c                   PARM                    cnacods                        *record di cnaco
008800060530     c                   PARM                    cnindds                        *record di cnind
008900060530     c                   PARM                    cnclpds                        *record di cnclp
009000060530     c                   PARM                    fnclsds                        *record di fncls
009100060901     c*
009200060901     c     kacvt         klist
009300060901     c                   kfld                    acvkcc
009400060901     c                   kfld                    acvksc
009500060901     c                   kfld                    acvdav
009600060901     c                   kfld                    acvorv
009700060530     C*
009800060530     C* operazioni iniziali
009900060530if  1c                   IF        ibs73tla <> 'C'                              *per elaborare
010000060530     c*
010100060530     c* memorizzo le ds dell'immagien precedente per confronto
010200060530     c                   if        ibs73immag='P'
010300060531     c                   clear                   conta
010400060531     c                   EXSR      ImpoDS
010500060531     c                   EXSR      PRIMA
010600060530     c                   endif
010700060531     c
010800060530     c                   if        ibs73immag='D'
010900060901     c                   exsr      SALVACONTO
011000060901     c
011100060531     c                   select
011200060531     c* Inserimento : solo immagine DOPO
011300060531     c                   when      ibs73cta<>'M' and ibs73cta<>'A'
011400060531     c                   EXSR      ImpoDS
011500060531     c                   EXSR      DOPO
011600060804     c* Annullamento: solo immagine DOPO
011700060804     c                   when      ibs73cta='A'
011800060804     c                   EXSR      ImpoDS
011900060804     c                   EXSR      DOPO
012000060531     c* Errore se non ho resitrato l'immagine precedente
012100060531     c                   when      conta<>1
012200060531     c                   eval      obs73err='1'
012300060531     c                   eval      obs73msg=msg(1)
012400060531     c* Errore se il capoconto/conto sono diversi tra prima e dopo
012500060531     c                   when      contoprima<>conto
012600060531     c                   eval      obs73err='1'
012700060531     c                   eval      obs73msg=msg(2)
012800060531     c                   other
012900060531     c
013000060531     c* Manutenzione
013100060531     c                   EXSR      ImpoDS
013200060530     c                   EXSR      DOPO
013300060531     c                   endsl
013400060531     c                   endif
013500971216e   1c                   ENDIF
013600971216     c*
013700060530if  1C                   IF        ibs73tla = *blanks                           R lascia file aperti
013800971216     C                   SETON                                        rt
013900971218x   1C                   ELSE                                                   C/L chiude i file
014000971216     C                   SETON                                        lr
014100971217e   1C                   ENDIF
014200060531      *------------------------------------------------------------------------*
014300060531      * Imposto i campi delle DS
014400060531      *------------------------------------------------------------------------*
014500060531     c     ImpoDS        BegSr
014600060531     c                   if        ibs73yda='A' or ibs73yda=' '
014700060531     c                   eval      dindiva=indiva
014800060928     c                   eval      dacoflg=acoflg
014900060531     c                   eval      dacorag=acorag
015000060531     c                   eval      dindvia=indvia
015100060531     c                   eval      dindcit=indcit
015200060531     c                   eval      dindcae=indcae
015300060531     c                   eval      dindprv=indprv
015400060531     c                   eval      dindsta=indsta
015500060531     c                   eval      dindtlf=indtlf
015600060531     c                   eval      dindtel=indtel
015700060531     c                   eval      dindcdf=indcdf
015701171228     c                   eval      dacolib = acolib
015800060531     c                   endif
015900060531     c                   if        ibs73yda='B' or ibs73yda=' '
016000060531     c                   move      clpcon        d§clpcon
016100060531     c                   eval      dacoabl=acoabl
016200060531     c                   eval      dclpnar=clpnar
016300060531     c                   eval      datb02=atb02
016400060531     c                   endif
016500060531     c                   if        ibs73yda='C' or ibs73yda=' '
016600060531     c                   eval      dclpage=clpage
016700060531     c                   eval      dclpclv=clpclv
016800060531     c                   eval      dclpsol=clpsol
016900060531     c                   Move      acoitc        d§acoitc
017000060531     c                   endif
017100060531     c                   if        ibs73yda='F' or ibs73yda=' '
017200060531     c                   eval      dclpscf=clpscf
017300060531     c                   eval      dclptft=clptft
017400060531     c                   eval      dclpfft=clpfft
017500060531     c                   Eval      d§clsuni = %subst(clsflo:2:1)
017600060531     c                   Move      indsin        d§indsin
017700060531     c                   Eval      d§clpfun=%subst(clpfun:1:1)
017800060531     c                   eval      dindnpn=indnpn
017900060531     c                   eval      dindnpr=indnpr
018000060531     c                   eval      dinddpr=inddpr
018100060531     c                   endif
018200060531     c                   if        ibs73yda='D' or ibs73yda=' '
018300060531     c                   Eval      d§clsfpr = %subst(clsflo:4:1)
018400060531     c                   Eval      d§clspgd = %subst(clsflo:3:1)
018500060531     c                   Eval      d§clssdn = %subst(clsflo:5:1)
018600060531     c                   Eval      d§clssin = %subst(clsflo:7:1)
018700060531     c                   endif
018800060531     c                   if        ibs73yda='G' or ibs73yda=' '
018900060531     c                   Eval      d§clstcm = %subst(clsflo:10:1)
019000060531     c                   Eval      d§clstcf = %subst(clsflo:1:1)
019100060531     c                   Eval      d§clsapg = %subst(clsflo:9:1)
019200060531     c                   eval      dacorx1=acorx1
019300060606     c                   eval      dacory1=acory1
019400060531     c                   endif
019500060531     c                   if        ibs73yda='O' or ibs73yda=' '
019600060531     c                   Eval      d§clstpa = %subst(clsflo:8:1)
019700060531     c                   eval      dclsstp=clsstp
019800060531     c                   endif
019900060531     c                   if        ibs73yda='P' or ibs73yda=' '
020000060531     c                   Move      indcdp        d§indcdp
020200060531     c                   eval      dindabi=indabi
020300060531     c                   eval      dindcab=indcab
020400060531     c                   endif
020500060531     c                   if        ibs73yda='S' or ibs73yda=' '
020600060531     c                   Eval      d§indbca = %subst(indopz:1:1)
020900060531     c                   eval      dclpfpc=clpfpc
021200080116     c                   eval      dclpbab = clpbab
021300060531     c                   endif
021400100806
021500060531     c                   ENDSR
021600060530      *------------------------------------------------------------------------*
021700060530      * memorizzo immagine precedente la variazione
021800060530      *------------------------------------------------------------------------*
021900060530     c     PRIMA         BegSr
022000060531     c                   z-add     1             conta
022100060901     c                   exsr      SALVACONTO
022200060531     c                   movel     conto         contoprima
022300060530     c
022400060530     c* Imposto le DS per verificare se dati variati
022500060530     c* Dati anagrafici
022600060530     c                   if        ibs73yda='A' or ibs73yda=' '
022700060530     c                   movel     dacv_a        wdacv_a
022800060530     c                   movel     dacv_a2       wdacv_a2
022900061027     c                   movel     dacv_a3       wdacv_a3
022901171228     c                   movel     dacv_a4       wdacv_a4
023000060530     c                   endif
023100060530     c* blocco clienti
023200060530     c                   if        ibs73yda='B' or ibs73yda=' '
023300060530     c                   movel     dacv_b        wdacv_b
023400060530     c                   endif
023500060530     c* Dati Commerciali
023600060530     c                   if        ibs73yda='C' or ibs73yda=' '
023700060530     c                   movel     dacv_c        wdacv_c
023800060530     c                   endif
023900060530     c* Dati Fattuazione
024000060530     c                   if        ibs73yda='F' or ibs73yda=' '
024100060530     c                   movel     dacv_F        wdacv_F
024200060530     c                   movel     dacv_F2       wdacv_F2
024300060530     c                   endif
024400060530     c* Dati Pagamento Fattura
024500060530     c                   if        ibs73yda='P' or ibs73yda=' '
024600060530     c                   movel     dacv_P        wdacv_P
024700060530     c                   endif
024800060530     c* Dati Pagamento C/Assgni
024900060530     c                   if        ibs73yda='S' or ibs73yda=' '
025000060530     c                   movel     dacv_S        wdacv_S
025100060530     c                   endif
025200060530     c* Dati giacenze
025300060530     c                   if        ibs73yda='G' or ibs73yda=' '
025400060530     c                   movel     dacv_G        wdacv_G
025500060530     c                   endif
025600060530     c* Danni
025700060530     c                   if        ibs73yda='D' or ibs73yda=' '
025800060530     c                   movel     dacv_D        wdacv_D
025900060530     c                   endif
026000060530     c* Operativa
026100060530     c                   if        ibs73yda='O' or ibs73yda=' '
026200060530     c                   movel     dacv_O        wdacv_O
026300060530     c                   endif
026400100806
026500100806      * se richiamato da Proj mi salvo il campo CLSTIC --> tipi pagamento DN e NA
026600100806     c                   IF         IBS73pgm = 'PROJ'
026700100806     c                   eval       savclstic = clstic
026800100806     c                   ENDIF
026900060530
027000060530     c                   EndSr
027100060531      *------------------------------------------------------------------------*
027200060531      * salvo capoconto e sottoconto da memorizzare
027300060531      *------------------------------------------------------------------------*
027400060531     c     SALVACONTO    BEGSR
027500060531     c                   select
027600060531     c                   when      acoksc>0
027700060531     c                   eval      acvkcc=acokcc
027800060531     c                   eval      acvksc=acoksc
027900060531     c                   when      indksc>0
028000060531     c                   eval      acvkcc=indkcc
028100060531     c                   eval      acvksc=indksc
028200060531     c                   other
028300060531     c                   eval      acvkcc=clpkcc
028400060531     c                   eval      acvksc=clpksc
028500060531     c                   endsl
028600060901     c                   movel     acvkcc        conto
028700060901     c                   move      acvksc        conto
028800060531     c                   EndSr
028900060530      *------------------------------------------------------------------------*
029000060530      * Memorizzo immagine successiva la variazione con scrittura files
029100060530      *------------------------------------------------------------------------*
029200060530     c     DOPO          BegSr
029300060530     c
029400060530     c                   clear                   DatVar
029500060530     c                   clear                   acv               2 0
029600060530     c                   clear                   cnacvt00
029700060530     c                   clear                   cnacvd00
029800060531     c
029900060531     c                   exsr      SALVAconto
030000060531     c
030100060530     c                   time                    w0140            14 0
030200060530     c     *eur          move      w0140         dataeur
030300060530     c     *iso          movel     dataeur       acvdav
030400060530     c                   time                    acvorv
030500060530     c                   movel     ibs73pru      acvpru
030600060530     c                   movel     ibs73noj      acvnoj
030700060601     c                   movel     ibs73pgm      acvpgm
030800060804     c* Inserimento/Annullamento
030900060804     c                   if        ibs73cta<>'M'
031000060530     c                   eval      acvcta=ibs73cta
031100060530     c                   movel     'ABCDFGPOS'   ACVFLO
031200060530     c                   write     cnacvt00
031300060530     c                   endif
031400060530     c* Manutenzione
031500060530     c                   if        ibs73cta='M'
031600060530     c* Verifico se variati i dati
031700060530     c* 1) Anagrafici
031800060530     c                   if        ibs73yda='A' or ibs73yda=' '
031900060530     c                   if        dacv_A<>wdacv_a
032000060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
032100060530     c                   add       1             acv
032200060530     c                   eval      datvar(acv)='A'
032300060530     c                   eval      acvyda='A'
032400060530     c                   movel(P)  wdacv_a       acvprima
032500060530     c                   movel(P)  dacv_a        acvdopo
032600060530     c                   write     cnacvd00
032700060530     c                   else
032800060530     c* Se variati gli altri dati, memorizzo senza scrivere il
032900060530     c*   dettaglio
033000060530     c                   if        dacv_A2<>wdacv_a2
033100060530     c                   add       1             acv
033200060530     c                   eval      datvar(acv)='A'
033300060530     c                   endif
033400060530     c                   endif
033500061027      * scrivo dettaglio e memorizzo che dati indicare in testata (Cod.Fiscale)
033600061027     c                   if        dacv_A3<>wdacv_A3
033700061027     c                   add       1             acv
033800061027     c                   eval      datvar(acv)='A'
033900061027     c                   eval      acvyda='A'
034000061027     c                   eval      acvprg=3
034100061027     c                   movel(P)  wdacv_A3      acvprima
034200061027     c                   movel(P)  dacv_A3       acvdopo
034300061027     c                   write     cnacvd00
034400061027     c                   clear                   acvprg
034500061027     c                   endif
034501171228      * scrivo dettaglio e memorizzo che dati indicare in testata (Cod.Potenziale)
034502171228     c                   if        dacv_A4<>wdacv_A4
034503171228     c                   add       1             acv
034504171228     c                   eval      datvar(acv)='A'
034505171228     c                   eval      acvyda='A'
034506171228     c                   eval      acvprg=4
034507171228     c                   movel(P)  wdacv_A4      acvprima
034508171228     c                   movel(P)  dacv_A4       acvdopo
034509171228     c                   write     cnacvd00
034510171228     c                   clear                   acvprg
034511171228     c                   endif
034600060530     c                   endif
034700060530     c* 2) Blocco Clienti
034800060530     c                   if        ibs73yda='B' or ibs73yda=' '
034900060530     c                   if        dacv_B<>wdacv_B
035000060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
035100060530     c                   add       1             acv
035200060530     c                   eval      datvar(acv)='B'
035300060530     c                   eval      acvyda='B'
035400060530     c                   movel(P)  wdacv_b       acvprima
035500060530     c                   movel(P)  dacv_b        acvdopo
035600060530     c                   write     cnacvd00
035700060530     c                   endif
035800060530     c                   endif
035900060530     c* 3) commerciali
036000060530     c                   if        ibs73yda='C' or ibs73yda=' '
036100060530     c                   if        dacv_C<>wdacv_C
036200060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
036300060530     c                   add       1             acv
036400060530     c                   eval      datvar(acv)='C'
036500060530     c                   eval      acvyda='C'
036600060530     c                   movel(P)  wdacv_C       acvprima
036700060530     c                   movel(P)  dacv_C        acvdopo
036800060530     c                   write     cnacvd00
036900060530     c                   endif
037000060530     c                   endif
037100060530     c* 4) Fattuazione
037200060530     c                   if        ibs73yda='F' or ibs73yda=' '
037300060530     c                   if        dacv_F<>wdacv_F
037400060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
037500060530     c                   add       1             acv
037600060530     c                   eval      datvar(acv)='F'
037700060530     c                   eval      acvyda='F'
037800060530     c                   movel(P)  wdacv_F       acvprima
037900060530     c                   movel(P)  dacv_F        acvdopo
038000060530     c                   write     cnacvd00
038100060530     c                   endif
038200060530     c                   if        dacv_F2<>wdacv_F2
038300060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
038400060530     c                   add       1             acv
038500060530     c                   eval      datvar(acv)='F'
038600060530     c                   eval      acvyda='F'
038700060530     c                   eval      acvprg=2
038800060530     c                   movel(P)  wdacv_F2      acvprima
038900060530     c                   movel(P)  dacv_F2       acvdopo
039000060530     c                   write     cnacvd00
039100060530     c                   clear                   acvprg
039200060530     c                   endif
039300060530     c                   endif
039400060530     c* 5) Dati pagamento fattura
039500060530     c                   if        ibs73yda='P' or ibs73yda=' '
039600060530     c                   if        dacv_P<>wdacv_P
039700060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
039800060530     c                   add       1             acv
039900060530     c                   eval      datvar(acv)='P'
040000060530     c                   eval      acvyda='P'
040100060530     c                   movel(P)  wdacv_P       acvprima
040200060530     c                   movel(P)  dacv_P        acvdopo
040300060530     c                   write     cnacvd00
040400060530     c                   endif
040500060530     c                   endif
040600060530     c* 6) Dati pagamento C/Assegni
040700060530     c                   if        ibs73yda='S' or ibs73yda=' '
040800060530     c                   if        dacv_S<>wdacv_S
040900060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
041000060530     c                   add       1             acv
041100060530     c                   eval      datvar(acv)='S'
041200060530     c                   eval      acvyda='S'
041300060530     c                   movel(P)  wdacv_S       acvprima
041400060530     c                   movel(P)  dacv_S        acvdopo
041500060530     c                   write     cnacvd00
041600060530     c                   endif
041700060530     c                   endif
041800060530     c* 7) Dati Giacenze
041900060530     c                   if        ibs73yda='G' or ibs73yda=' '
042000060530     c                   if        dacv_G<>wdacv_G
042100060530     c                   add       1             acv
042200060530     c                   eval      datvar(acv)='G'
042300060530     c                   endif
042400060530     c                   endif
042500060530     c* 8) Dati Danni
042600060530     c                   if        ibs73yda='D' or ibs73yda=' '
042700060530     c                   if        dacv_D<>wdacv_D
042800060530     c                   add       1             acv
042900060530     c                   eval      datvar(acv)='D'
043000060530     c                   endif
043100060530     c                   endif
043200060530     c* 9) Dati operativi
043300060530     c                   if        ibs73yda='O' or ibs73yda=' '
043400060530     c                   if        dacv_O<>wdacv_O
043500060530     c                   add       1             acv
043600060530     c                   eval      datvar(acv)='O'
043700060530     c                   endif
043800060530     c                   endif
043900100806
044000100806      * se richiamato da Proj e CLSTIC variato scrivo variazioni e imposto flag x la testata
044100100806     c                   IF        IBS73pgm = 'PROJ' and
044200100806     c                             savclstic <> clstic
044300100806      * per il pagamento DN
044400100806     c                   IF        %subst(savclstic:1:1) <> %subst(clstic:1:1)
044500100806     c                   add       1             acv
044600100806     c                   eval      datvar(acv)='H'
044700100806     c                   eval      acvyda='H'
044800100806     c                   eval      acvprima = %subst(savclstic:1:1)
044900100806     c                   eval      acvdopo  = %subst(clstic:1:1)
045000100806     c                   write     cnacvd00
045100100806     c                   ENDIF
045200100806      * per il pagamento NA
045300100806     c                   IF        %subst(savclstic:2:1) <> %subst(clstic:2:1)
045400100806     c                   add       1             acv
045500100806     c                   eval      datvar(acv)='I'
045600160707     c                   eval      acvyda='I'
045700100806     c                   eval      acvprima = %subst(savclstic:2:1)
045800100806     c                   eval      acvdopo  = %subst(clstic:2:1)
045900100806     c                   write     cnacvd00
046000100806     c                   ENDIF
046100100806     c                   ENDIF
046200100806
046300060530     c* Write finale della testata
046400060901    1c                   if        datvar(1)<>*blanks
046500060530     c                   eval      acvcta='M'
046600060530     c                   movea     Datvar        acvflo
046700060901     c* Salvo il record e verifico se sono uguali, tranne il campo ACVFLO
046800060901     c                   movel     cnacvt        wcnacvt
046900060901     c                   movel     acvflo        newacvflo
047000060901     c                   clear                   wacvflo
047100060901     c
047200060901     c     kacvt         chain     cnacvt1l
047300060901    2c                   if        not %found(cnacvt1l)
047400060530     c                   write     cnacvt00
047500060901   x2c                   else
047600060901     c* Salvo acvflo e clearo  il campo
047700060901     c                   movel     acvflo        oldacvflo
047800060901     c                   clear                   acvflo
047900060901     c* Sono diversi write subito
048000060901    3c                   if        cnacvt<>wcnacvt
048100060901     c                   movel     wcnacvt       cnacvt
048200060901     c                   movel     newacvflo     acvflo
048300060901     c                   write     cnacvt00
048400060901   x3c                   else
048500060901     c* se sono uguali unisco acvflo e update
048600060901     c* DATVAR contiene acvflo nuovo
048700060901     c* k15    contiene acvflo old
048800060901     c                   movea     oldacvflo     k15
048900060901     c* aggiungo i mancanti
049000060901     c                   z-add     1             zz
049100060901     c     ' '           lookup    datvar(zz)                             30
049200060901    4c                   if        *in30
049300060901     c                   z-add     1             yy
049400060901    5c                   dow       yy<=15 and k15(YY)<>' '
049500060901     c     k15(yy)       lookup    datvar                                 31
049600060901    6c                   if        not *in31 and zz<=15
049700060901     c                   movel     k15(yy)       datvar(zz)
049800060901     c                   add       1             zz
049900060901    6c                   endif
050000060901     c                   add       1             yy
050100060901    5c                   enddo
050200060901    4c                   endif
050300060901     c
050400060901     c                   sorta     datvar
050500060901     c                   movea     datvar        acvflo
050600060901     c                   eval      acvflo=%triml(acvflo)
050700060901     c                   update    cnacvt00
050800060901    3c                   endif
050900060901    2c                   endif
051000060901    1c                   endif
051100060901    0c                   endif
051200060530     c                   EndSr
051300060531**
051400060531Errore: non passata l'immagine PRECEDENTE. Variazione non registrabile
051500060531Errore: Capoconto/conto PdC diverso tra immagine PRECEDENTE imm.SEGUENTE
