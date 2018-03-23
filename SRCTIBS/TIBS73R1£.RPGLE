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
008100971216     c*--------------------------------------------------------------------------------------------*
008200971216     c* Main lines
008300971216     c*--------------------------------------------------------------------------------------------*
008400060530     c     *ENTRY        PLIST
008500060530     c                   PARM                    tibs73ds                       *ds di procedura
008600060530     c                   PARM                    cnacods                        *record di cnaco
008700060530     c                   PARM                    cnindds                        *record di cnind
008800060530     c                   PARM                    cnclpds                        *record di cnclp
008900060530     c                   PARM                    fnclsds                        *record di fncls
009000060901     c*
009100060901     c     kacvt         klist
009200060901     c                   kfld                    acvkcc
009300060901     c                   kfld                    acvksc
009400060901     c                   kfld                    acvdav
009500060901     c                   kfld                    acvorv
009600060530     C*
009700060530     C* operazioni iniziali
009800060530if  1c                   IF        ibs73tla <> 'C'                              *per elaborare
009900060530     c*
010000060530     c* memorizzo le ds dell'immagien precedente per confronto
010100060530     c                   if        ibs73immag='P'
010200060531     c                   clear                   conta
010300060531     c                   EXSR      ImpoDS
010400060531     c                   EXSR      PRIMA
010500060530     c                   endif
010600060531     c
010700060530     c                   if        ibs73immag='D'
010800060901     c                   exsr      SALVACONTO
010900060901     c
011000060531     c                   select
011100060531     c* Inserimento : solo immagine DOPO
011200060531     c                   when      ibs73cta<>'M' and ibs73cta<>'A'
011300060531     c                   EXSR      ImpoDS
011400060531     c                   EXSR      DOPO
011500060804     c* Annullamento: solo immagine DOPO
011600060804     c                   when      ibs73cta='A'
011700060804     c                   EXSR      ImpoDS
011800060804     c                   EXSR      DOPO
011900060531     c* Errore se non ho resitrato l'immagine precedente
012000060531     c                   when      conta<>1
012100060531     c                   eval      obs73err='1'
012200060531     c                   eval      obs73msg=msg(1)
012300060531     c* Errore se il capoconto/conto sono diversi tra prima e dopo
012400060531     c                   when      contoprima<>conto
012500060531     c                   eval      obs73err='1'
012600060531     c                   eval      obs73msg=msg(2)
012700060531     c                   other
012800060531     c
012900060531     c* Manutenzione
013000060531     c                   EXSR      ImpoDS
013100060530     c                   EXSR      DOPO
013200060531     c                   endsl
013300060531     c                   endif
013400971216e   1c                   ENDIF
013500971216     c*
013600060530if  1C                   IF        ibs73tla = *blanks                           R lascia file aperti
013700971216     C                   SETON                                        rt
013800971218x   1C                   ELSE                                                   C/L chiude i file
013900971216     C                   SETON                                        lr
014000971217e   1C                   ENDIF
014100060531      *------------------------------------------------------------------------*
014200060531      * Imposto i campi delle DS
014300060531      *------------------------------------------------------------------------*
014400060531     c     ImpoDS        BegSr
014500060531     c                   if        ibs73yda='A' or ibs73yda=' '
014600060531     c                   eval      dindiva=indiva
014700060928     c                   eval      dacoflg=acoflg
014800060531     c                   eval      dacorag=acorag
014900060531     c                   eval      dindvia=indvia
015000060531     c                   eval      dindcit=indcit
015100060531     c                   eval      dindcae=indcae
015200060531     c                   eval      dindprv=indprv
015300060531     c                   eval      dindsta=indsta
015400060531     c                   eval      dindtlf=indtlf
015500060531     c                   eval      dindtel=indtel
015600060531     c                   eval      dindcdf=indcdf
015700060531     c                   endif
015800060531     c                   if        ibs73yda='B' or ibs73yda=' '
015900060531     c                   move      clpcon        d§clpcon
016000060531     c                   eval      dacoabl=acoabl
016100060531     c                   eval      dclpnar=clpnar
016200060531     c                   eval      datb02=atb02
016300060531     c                   endif
016400060531     c                   if        ibs73yda='C' or ibs73yda=' '
016500060531     c                   eval      dclpage=clpage
016600060531     c                   eval      dclpclv=clpclv
016700060531     c                   eval      dclpsol=clpsol
016800060531     c                   Move      acoitc        d§acoitc
016900060531     c                   endif
017000060531     c                   if        ibs73yda='F' or ibs73yda=' '
017100060531     c                   eval      dclpscf=clpscf
017200060531     c                   eval      dclptft=clptft
017300060531     c                   eval      dclpfft=clpfft
017400060531     c                   Eval      d§clsuni = %subst(clsflo:2:1)
017500060531     c                   Move      indsin        d§indsin
017600060531     c                   Eval      d§clpfun=%subst(clpfun:1:1)
017700060531     c                   eval      dindnpn=indnpn
017800060531     c                   eval      dindnpr=indnpr
017900060531     c                   eval      dinddpr=inddpr
018000060531     c                   endif
018100060531     c                   if        ibs73yda='D' or ibs73yda=' '
018200060531     c                   Eval      d§clsfpr = %subst(clsflo:4:1)
018300060531     c                   Eval      d§clspgd = %subst(clsflo:3:1)
018400060531     c                   Eval      d§clssdn = %subst(clsflo:5:1)
018500060531     c                   Eval      d§clssin = %subst(clsflo:7:1)
018600060531     c                   endif
018700060531     c                   if        ibs73yda='G' or ibs73yda=' '
018800060531     c                   Eval      d§clstcm = %subst(clsflo:10:1)
018900060531     c                   Eval      d§clstcf = %subst(clsflo:1:1)
019000060531     c                   Eval      d§clsapg = %subst(clsflo:9:1)
019100060531     c                   eval      dacorx1=acorx1
019200060606     c                   eval      dacory1=acory1
019300060531     c                   endif
019400060531     c                   if        ibs73yda='O' or ibs73yda=' '
019500060531     c                   Eval      d§clstpa = %subst(clsflo:8:1)
019600060531     c                   eval      dclsstp=clsstp
019700060531     c                   endif
019800060531     c                   if        ibs73yda='P' or ibs73yda=' '
019900060531     c                   Move      indcdp        d§indcdp
020000080211     c**!!!              eval      dindccb=indccb
020100060531     c                   eval      dindabi=indabi
020200060531     c                   eval      dindcab=indcab
020300060531     c                   endif
020400060531     c                   if        ibs73yda='S' or ibs73yda=' '
020500060531     c                   Eval      d§indbca = %subst(indopz:1:1)
020600080116     c**!!!              Eval      d§clpccba = %subst(clpccb:1:12)
020700080116     c**!!!              Eval      d§clpcina = %subst(clpccb:16:1)
020800060531     c                   eval      dclpfpc=clpfpc
020900080116     c**!!!              eval      dclpabi=clpabi
021000080116     c**!!!              eval      dclpcab=clpcab
021100080116     c                   eval      dclpbab = clpbab
021200060531     c                   endif
021300060531     c                   ENDSR
021400060530      *------------------------------------------------------------------------*
021500060530      * memorizzo immagine precedente la variazione
021600060530      *------------------------------------------------------------------------*
021700060530     c     PRIMA         BegSr
021800060531     c                   z-add     1             conta
021900060901     c                   exsr      SALVACONTO
022000060531     c                   movel     conto         contoprima
022100060530     c
022200060530     c* Imposto le DS per verificare se dati variati
022300060530     c* Dati anagrafici
022400060530     c                   if        ibs73yda='A' or ibs73yda=' '
022500060530     c                   movel     dacv_a        wdacv_a
022600060530     c                   movel     dacv_a2       wdacv_a2
022700061027     c                   movel     dacv_a3       wdacv_a3
022800060530     c                   endif
022900060530     c* blocco clienti
023000060530     c                   if        ibs73yda='B' or ibs73yda=' '
023100060530     c                   movel     dacv_b        wdacv_b
023200060530     c                   endif
023300060530     c* Dati Commerciali
023400060530     c                   if        ibs73yda='C' or ibs73yda=' '
023500060530     c                   movel     dacv_c        wdacv_c
023600060530     c                   endif
023700060530     c* Dati Fattuazione
023800060530     c                   if        ibs73yda='F' or ibs73yda=' '
023900060530     c                   movel     dacv_F        wdacv_F
024000060530     c                   movel     dacv_F2       wdacv_F2
024100060530     c                   endif
024200060530     c* Dati Pagamento Fattura
024300060530     c                   if        ibs73yda='P' or ibs73yda=' '
024400060530     c                   movel     dacv_P        wdacv_P
024500060530     c                   endif
024600060530     c* Dati Pagamento C/Assgni
024700060530     c                   if        ibs73yda='S' or ibs73yda=' '
024800060530     c                   movel     dacv_S        wdacv_S
024900060530     c                   endif
025000060530     c* Dati giacenze
025100060530     c                   if        ibs73yda='G' or ibs73yda=' '
025200060530     c                   movel     dacv_G        wdacv_G
025300060530     c                   endif
025400060530     c* Danni
025500060530     c                   if        ibs73yda='D' or ibs73yda=' '
025600060530     c                   movel     dacv_D        wdacv_D
025700060530     c                   endif
025800060530     c* Operativa
025900060530     c                   if        ibs73yda='O' or ibs73yda=' '
026000060530     c                   movel     dacv_O        wdacv_O
026100060530     c                   endif
026200060530
026300060530     c                   EndSr
026400060531      *------------------------------------------------------------------------*
026500060531      * salvo capoconto e sottoconto da memorizzare
026600060531      *------------------------------------------------------------------------*
026700060531     c     SALVACONTO    BEGSR
026800060531     c                   select
026900060531     c                   when      acoksc>0
027000060531     c                   eval      acvkcc=acokcc
027100060531     c                   eval      acvksc=acoksc
027200060531     c                   when      indksc>0
027300060531     c                   eval      acvkcc=indkcc
027400060531     c                   eval      acvksc=indksc
027500060531     c                   other
027600060531     c                   eval      acvkcc=clpkcc
027700060531     c                   eval      acvksc=clpksc
027800060531     c                   endsl
027900060901     c                   movel     acvkcc        conto
028000060901     c                   move      acvksc        conto
028100060531     c                   EndSr
028200060530      *------------------------------------------------------------------------*
028300060530      * Memorizzo immagine successiva la variazione con scrittura files
028400060530      *------------------------------------------------------------------------*
028500060530     c     DOPO          BegSr
028600060530     c
028700060530     c                   clear                   DatVar
028800060530     c                   clear                   acv               2 0
028900060530     c                   clear                   cnacvt00
029000060530     c                   clear                   cnacvd00
029100060531     c
029200060531     c                   exsr      SALVAconto
029300060531     c
029400060530     c                   time                    w0140            14 0
029500060530     c     *eur          move      w0140         dataeur
029600060530     c     *iso          movel     dataeur       acvdav
029700060530     c                   time                    acvorv
029800060530     c                   movel     ibs73pru      acvpru
029900060530     c                   movel     ibs73noj      acvnoj
030000060601     c                   movel     ibs73pgm      acvpgm
030100060804     c* Inserimento/Annullamento
030200060804     c                   if        ibs73cta<>'M'
030300060530     c                   eval      acvcta=ibs73cta
030400060530     c                   movel     'ABCDFGPOS'   ACVFLO
030500060530     c                   write     cnacvt00
030600060530     c                   endif
030700060530     c* Manutenzione
030800060530     c                   if        ibs73cta='M'
030900060530     c* Verifico se variati i dati
031000060530     c* 1) Anagrafici
031100060530     c                   if        ibs73yda='A' or ibs73yda=' '
031200060530     c                   if        dacv_A<>wdacv_a
031300060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
031400060530     c                   add       1             acv
031500060530     c                   eval      datvar(acv)='A'
031600060530     c                   eval      acvyda='A'
031700060530     c                   movel(P)  wdacv_a       acvprima
031800060530     c                   movel(P)  dacv_a        acvdopo
031900060530     c                   write     cnacvd00
032000060530     c                   else
032100060530     c* Se variati gli altri dati, memorizzo senza scrivere il
032200060530     c*   dettaglio
032300060530     c                   if        dacv_A2<>wdacv_a2
032400060530     c                   add       1             acv
032500060530     c                   eval      datvar(acv)='A'
032600060530     c                   endif
032700060530     c                   endif
032800061027      * scrivo dettaglio e memorizzo che dati indicare in testata (Cod.Fiscale)
032900061027     c                   if        dacv_A3<>wdacv_A3
033000061027     c                   add       1             acv
033100061027     c                   eval      datvar(acv)='A'
033200061027     c                   eval      acvyda='A'
033300061027     c                   eval      acvprg=3
033400061027     c                   movel(P)  wdacv_A3      acvprima
033500061027     c                   movel(P)  dacv_A3       acvdopo
033600061027     c                   write     cnacvd00
033700061027     c                   clear                   acvprg
033800061027     c                   endif
033900060530     c                   endif
034000060530     c* 2) Blocco Clienti
034100060530     c                   if        ibs73yda='B' or ibs73yda=' '
034200060530     c                   if        dacv_B<>wdacv_B
034300060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
034400060530     c                   add       1             acv
034500060530     c                   eval      datvar(acv)='B'
034600060530     c                   eval      acvyda='B'
034700060530     c                   movel(P)  wdacv_b       acvprima
034800060530     c                   movel(P)  dacv_b        acvdopo
034900060530     c                   write     cnacvd00
035000060530     c                   endif
035100060530     c                   endif
035200060530     c* 3) commerciali
035300060530     c                   if        ibs73yda='C' or ibs73yda=' '
035400060530     c                   if        dacv_C<>wdacv_C
035500060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
035600060530     c                   add       1             acv
035700060530     c                   eval      datvar(acv)='C'
035800060530     c                   eval      acvyda='C'
035900060530     c                   movel(P)  wdacv_C       acvprima
036000060530     c                   movel(P)  dacv_C        acvdopo
036100060530     c                   write     cnacvd00
036200060530     c                   endif
036300060530     c                   endif
036400060530     c* 4) Fattuazione
036500060530     c                   if        ibs73yda='F' or ibs73yda=' '
036600060530     c                   if        dacv_F<>wdacv_F
036700060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
036800060530     c                   add       1             acv
036900060530     c                   eval      datvar(acv)='F'
037000060530     c                   eval      acvyda='F'
037100060530     c                   movel(P)  wdacv_F       acvprima
037200060530     c                   movel(P)  dacv_F        acvdopo
037300060530     c                   write     cnacvd00
037400060530     c                   endif
037500060530     c                   if        dacv_F2<>wdacv_F2
037600060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
037700060530     c                   add       1             acv
037800060530     c                   eval      datvar(acv)='F'
037900060530     c                   eval      acvyda='F'
038000060530     c                   eval      acvprg=2
038100060530     c                   movel(P)  wdacv_F2      acvprima
038200060530     c                   movel(P)  dacv_F2       acvdopo
038300060530     c                   write     cnacvd00
038400060530     c                   clear                   acvprg
038500060530     c                   endif
038600060530     c                   endif
038700060530     c* 5) Dati pagamento fattura
038800060530     c                   if        ibs73yda='P' or ibs73yda=' '
038900060530     c                   if        dacv_P<>wdacv_P
039000060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
039100060530     c                   add       1             acv
039200060530     c                   eval      datvar(acv)='P'
039300060530     c                   eval      acvyda='P'
039400060530     c                   movel(P)  wdacv_P       acvprima
039500060530     c                   movel(P)  dacv_P        acvdopo
039600060530     c                   write     cnacvd00
039700060530     c                   endif
039800060530     c                   endif
039900060530     c* 6) Dati pagamento C/Assegni
040000060530     c                   if        ibs73yda='S' or ibs73yda=' '
040100060530     c                   if        dacv_S<>wdacv_S
040200060530     c* Scrivo dettaglio e memorizzo che dati indicare in testata
040300060530     c                   add       1             acv
040400060530     c                   eval      datvar(acv)='S'
040500060530     c                   eval      acvyda='S'
040600060530     c                   movel(P)  wdacv_S       acvprima
040700060530     c                   movel(P)  dacv_S        acvdopo
040800060530     c                   write     cnacvd00
040900060530     c                   endif
041000060530     c                   endif
041100060530     c* 7) Dati Giacenze
041200060530     c                   if        ibs73yda='G' or ibs73yda=' '
041300060530     c                   if        dacv_G<>wdacv_G
041400060530     c                   add       1             acv
041500060530     c                   eval      datvar(acv)='G'
041600060530     c                   endif
041700060530     c                   endif
041800060530     c* 8) Dati Danni
041900060530     c                   if        ibs73yda='D' or ibs73yda=' '
042000060530     c                   if        dacv_D<>wdacv_D
042100060530     c                   add       1             acv
042200060530     c                   eval      datvar(acv)='D'
042300060530     c                   endif
042400060530     c                   endif
042500060530     c* 9) Dati operativi
042600060530     c                   if        ibs73yda='O' or ibs73yda=' '
042700060530     c                   if        dacv_O<>wdacv_O
042800060530     c                   add       1             acv
042900060530     c                   eval      datvar(acv)='O'
043000060530     c                   endif
043100060530     c                   endif
043200060530     c* Write finale della testata
043300060901    1c                   if        datvar(1)<>*blanks
043400060530     c                   eval      acvcta='M'
043500060530     c                   movea     Datvar        acvflo
043600060901     c* Salvo il record e verifico se sono uguali, tranne il campo ACVFLO
043700060901     c                   movel     cnacvt        wcnacvt
043800060901     c                   movel     acvflo        newacvflo
043900060901     c                   clear                   wacvflo
044000060901     c
044100060901     c     kacvt         chain     cnacvt1l
044200060901    2c                   if        not %found(cnacvt1l)
044300060530     c                   write     cnacvt00
044400060901   x2c                   else
044500060901     c* Salvo acvflo e clearo  il campo
044600060901     c                   movel     acvflo        oldacvflo
044700060901     c                   clear                   acvflo
044800060901     c* Sono diversi write subito
044900060901    3c                   if        cnacvt<>wcnacvt
045000060901     c                   movel     wcnacvt       cnacvt
045100060901     c                   movel     newacvflo     acvflo
045200060901     c                   write     cnacvt00
045300060901   x3c                   else
045400060901     c* se sono uguali unisco acvflo e update
045500060901     c* DATVAR contiene acvflo nuovo
045600060901     c* k15    contiene acvflo old
045700060901     c                   movea     oldacvflo     k15
045800060901     c* aggiungo i mancanti
045900060901     c                   z-add     1             zz
046000060901     c     ' '           lookup    datvar(zz)                             30
046100060901    4c                   if        *in30
046200060901     c                   z-add     1             yy
046300060901    5c                   dow       yy<=15 and k15(YY)<>' '
046400060901     c     k15(yy)       lookup    datvar                                 31
046500060901    6c                   if        not *in31 and zz<=15
046600060901     c                   movel     k15(yy)       datvar(zz)
046700060901     c                   add       1             zz
046800060901    6c                   endif
046900060901     c                   add       1             yy
047000060901    5c                   enddo
047100060901    4c                   endif
047200060901     c
047300060901     c                   sorta     datvar
047400060901     c                   movea     datvar        acvflo
047500060901     c                   eval      acvflo=%triml(acvflo)
047600060901     c                   update    cnacvt00
047700060901    3c                   endif
047800060901    2c                   endif
047900060901    1c                   endif
048000060901    0c                   endif
048100060530     c                   EndSr
048200060531**
048300060531Errore: non passata l'immagine PRECEDENTE. Variazione non registrabile
048400060531Errore: Capoconto/conto PdC diverso tra immagine PRECEDENTE imm.SEGUENTE
