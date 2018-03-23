000100971216     h*-------------------------------------------------------------------------
000200170920     h* Registra variazioni avanzamento giacenze
000300971216     h*-------------------------------------------------------------------------
000500971216     h DECEDIT('0,') DATEDIT(*DMY.)
000600971216     f*-------------------------------------------------------------------------
000700971216     f* Data base
000800170508     Ftntbe01l  iF   E           K DISK
000900170920     ftigfp00f  o    e             disk
001000170920     ftigfd00f  o    e             disk
001100971216     d*-------------------------------------------------------------------------
001200060531     d datVar          s              1    dim(15)
001300060901     d k15             s              1    dim(15)
001400170920     d fnlg33ds      e DS
001500060530     d
001501170920      *vista before
001600170920     d tigcpB        e DS                  extname(tigcp00f) prefix(B)
001601170920      *vista after
001700170920     d tigcpA        e DS                  extname(tigcp00f) prefix(A)
002000170503      *
002001170920     d Dgcp          e DS
002200170920     d Dgcp_A        e DS
002300170920     d Dgcp_A1       e DS
002500170920     d Dgcp_B        e DS
002700170920     d Dgcp_C        e DS
002701170920     d Dgcp_D        e DS
002702171012     d Dgcp_E        e DS
003100170503      *
003300170920     d wDgcp_A       e DS                  extname(Dgcp_A)   prefix(w)
003400170920     d wDgcp_A1      e DS                  extname(Dgcp_A1)  prefix(w)
003600170920     d wDgcp_B       e DS                  extname(Dgcp_B)   prefix(w)
003800170920     d wDgcp_C       e DS                  extname(Dgcp_C)   prefix(w)
003801170920     d wDgcp_D       e DS                  extname(Dgcp_d)   prefix(w)
003802171012     d wDgcp_E       e DS                  extname(Dgcp_E)   prefix(w)
004200060530     d dataeur         s               d   datfmt(*eur)
004300060531     d conta           s              4  0 inz
004400060901     d yy              s              2  0
004500060901     d zz              s              2  0
004600971216     c*-------------------------------------------------------------------------
004700971216     c* Main lines
004800971216     c*-------------------------------------------------------------------------
004900060530     c     *ENTRY        PLIST
005000170920     c                   PARM                    fnlg33ds
005100170920     c                   PARM                    tigcpB
005200170920     c                   PARM                    tigcpA
005401170524      *
005600170920     c                   clear                   tigfp000
005700170920     c                   clear                   tigfd000
005800170508      *
005900170508     c                   time                    w0140            14 0
006000170508     c     *eur          move      w0140         dataeur
006100170504      *
006200170920     c     'GCP'         setll     tntbe01l
006300170508     c                   do        *hival
006400170920     c     'GCP'         reade     tntbe01l
006500170508     c                   if        %eof(tntbe01l)
006600170508     c                   leave
006700170508     c                   endif
006701170920     c                   movel     tbeuni        dgcp
007000170508     c                   enddo
007100170508      *
007200170508      * verifica modalità di ingresso
007300170508     c                   select
007301171002      * modifica
007400170920     c                   when      ilg33cta = 'M'
007500170508     c                   exsr      Modifica
007600171002      * riapertura
007700170920     c                   when      ilg33cta = 'R'
007800170508     c                   exsr      SoloTestata
008000171002      * ripristino
008001171002     c                   when      ilg33cta = 'I'
008002171002     c                   exsr      SoloTestata
008003171002     c                   eval      ilg33cta = 'M'
008004171002     c                   exsr      Modifica
008005171002      *
008100170508     c                   other
008200170508     c                   exsr      SoloTestata
008300170508      *
008400170508     c                   endsl
008500170503     c                   seton                                        lr
008600170508      *
008700170508      * ?___________________________________________________________________
008800170508      *   Scrive rekord di testata variazioni per inserimento
008900170508     c     SoloTestata   begsr
009000170508      * ?___________________________________________________________________
009100170508      *scrittura testata
009200170920     c                   movea     k15           gfpflo
009500170920     c                   movel     Ilg33noj      gfpnoj
009600170920     c                   movel     Ilg33PRU      gfppru
009700170920     c                   movel     Ilg33PGM      gfppgm
009800170920     c                   movel     Ilg33CTA      gfpcta
009900170920     c                   if        bgcpagc > 0
010000170920     c                   movel     bgcpagc       gfpagc
010001170920     c                   movel     bgcpfgc       gfpfgc
010002170920     c                   movel     bgcpngc       gfpngc
010003170920     c                   movel     bgcpfas       gfpfas
010200170508     c                   else
010201170920     c                   movel     agcpagc       gfpagc
010202170920     c                   movel     agcpfgc       gfpfgc
010203170920     c                   movel     agcpngc       gfpngc
010204170920     c                   movel     agcpfas       gfpfas
010500170508     c                   endif
010501170927     c                   eval      gfptime = %timestamp
010600170508
010700170920     c                   write     tigfp000
010800170504      *
010900170508     c                   endsr
011000170508      * ?___________________________________________________________________
011100170508      *   esegue routine riempimento campi e confronti per scritture
011200170508     c     modifica      begsr
011300170508      * ?___________________________________________________________________
011400170504     c                   exsr      RiempiBefore
011500170504     c                   exsr      RiempiAfter
011600170504     c                   exsr      confronta
011700170504      *
011800170504     c                   endsr
011900170504      *
012000170508      * ?___________________________________________________________________
012100170508      *   prepara ds con immagine Before per confronto
012200170504     c     RiempiBefore  begsr
012300170508      * ?___________________________________________________________________
012600170504      *valorizzo campi
012700170504      * A
012800170920     c                   eval      GCPASG  =  bGCPASG
012900170920     c                   eval      GCPSGS  =  bGCPSGS
013000170920     c                   eval      GCPSGR  =  bGCPSGR
013200170504      * A1
013300170920     c                   eval      GCPSGD  =  bGCPSGD
013301170920     c                   eval      GCPSGA  =  bGCPSGA
013600170504      * B
013700170920     c                   eval      GCPCAS  =  bGCPCAS
013800170920     c                   eval      GCPVCS  =  bGCPVCS
013900170920     c                   eval      GCPPPC  =  bGCPPPC
014000170920     c                   eval      GCPAPC  =  bGCPAPC
014800170504      * C
014900170920     c                   eval      GCPDIS  =  bGCPDIS
015000170920     c                   eval      GCPNPC  =  bGCPNPC
015300170920     c                   eval      GCPTFT  =  bGCPTFT
015301170920      * D
015302170920     c                   eval      GCPfas  =  bGCPfas
015303171012      * E
015306171012     c                   eval      GCPTCM  =  bGCPTCM
015307171012     c                   eval      GCPTFG  =  bGCPTFG
016600170504      *
016700170504     c                   endsr
016800170508      * ?___________________________________________________________________
016900170508      *   prepara ds con immagine After per confronto
017000170504     c     RiempiAfter   begsr
017100170508      * ?___________________________________________________________________
017201170920      *valorizzo campi
017202170920      * A
017203170922     c                   eval      wgCPASG  =  aGCPASG
017204170922     c                   eval      wGCPSGS  =  aGCPSGS
017205170922     c                   eval      wGCPSGR  =  aGCPSGR
017206170922      * A1
017207170922     c                   eval      wGCPSGD  =  aGCPSGD
017208170922     c                   eval      wGCPSGA  =  aGCPSGA
017209170920      * B
017210170922     c                   eval      wGCPCAS  =  aGCPCAS
017211170922     c                   eval      wGCPVCS  =  aGCPVCS
017212170922     c                   eval      wGCPPPC  =  aGCPPPC
017213170922     c                   eval      wGCPAPC  =  aGCPAPC
017214170920      * C
017215170922     c                   eval      wGCPDIS  =  aGCPDIS
017216170922     c                   eval      wGCPNPC  =  aGCPNPC
017219170922     c                   eval      wGCPTFT  =  aGCPTFT
017220170920      * D
017221170922     c                   eval      wGCPfas  =  aGCPfas
017222171012      * E
017225171012     c                   eval      wGCPTCM  =  aGCPTCM
017226171012     c                   eval      wGCPTFG  =  aGCPTFG
021400170504      *
021500170504     c                   endsr
021600170508      * ?___________________________________________________________________
021700170508      *   Confronta Ds con immagine prima e dopo se variate scrive dettaglio
021800170504     c     Confronta     begsr
021900170508      * ?___________________________________________________________________
022000170504      *valorizzo dati standard per tutti i rekord
022100170504     c                   clear                   DatVar
022200170504     c                   clear                   apv               2 0
022500170920     c                   movel     Ilg33PRU      gfppru
022600170920     c                   movel     Ilg33NOJ      gfpnoj
022700170920     c                   movel     Ilg33PGM      gfppgm
022800170920     c                   movel     Ilg33CTA      gfpcta
022900170921     c                   movel     agcpagc       gfpagc
023000170921     c                   movel     agcpfgc       gfpfgc
023001170921     c                   movel     agcpngc       gfpngc
023002170921     c                   movel     agcpfas       gfpfas
023003170926     c                   eval      gfptime = %timestamp
023400170920     c                   movel     gfpnoj        gfdnoj
023401170920      *
023500170920     c                   movel     gfpagc        gfdagc
023600170920     c                   movel     gfpfgc        gfdfgc
023601170920     c                   movel     gfpngc        gfdngc
023602170926     c                   movel     gfpfas        gfdfas
023603170926     c                   eval      gfdtime = gfptime
023700170504      *confronto ds una alla volta prima e dopo per verificare se dobbiamo
023800170504      * scrivere le variazioni GRUPPO A
023900170921     c                   if        dgcp_a <> wdgcp_a
024000170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
024100170504     c                   z-add     1             apv
024200170504     c                   eval      datvar(apv)='A'
024300170920     c                   eval      gfdgcp = 'A'
024400170920     c                   movel(P)  dgcp_a        gfdprima
024500170920     c                   movel(P)  wdgcp_a       gfddopo
024600170920     c                   write     tigfd000
024700170504     c                   endif
024800170504      *
024900170920     c                   if        dgcp_a1 <> wdgcp_a1
025000170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
025100170504     c                   z-add     1             apv
025200170504     c                   eval      datvar(apv)='A'
025300170920     c                   eval      gfdgcp = 'A'
025400170920     c                   eval      gfdprg = 1
025500170920     c                   movel(P)  dgcp_a1       gfdprima
025600170921     c                   movel(P)  wdgcp_a1      gfddopo
025700170920     c                   write     tigfd000
025800170504     c                   endif
025900170504      *
027000170504      * scrivere le variazioni GRUPPO B
027100170920     c                   if        dgcp_b <> wdgcp_b
027200170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
027300170505     c                   eval      apv = apv +1
027400170505     c                   eval      datvar(apv)='B'
027500170920     c                   eval      gfdgcp = 'B'
027600170920     c                   eval      gfdprg = 0
027700170921     c                   movel(P)  dgcp_b        gfdprima
027800170921     c                   movel(P)  wdgcp_b       gfddopo
027900170920     c                   write     tigfd000
028000170505     c                   endif
028100170504      *
029600170504      * scrivere le variazioni GRUPPO C
029700170920     c                   if        dgcp_C <> wdgcp_C
029800170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
029900170504     c                   eval      apv = apv +1
030000170504     c                   eval      datvar(apv)='C'
030100170920     c                   eval      gfdgcp = 'C'
030200170920     c                   eval      gfdprg = 0
030300170920     c                   movel(P)  dgcp_c        gfdprima
030400170920     c                   movel(P)  wdgcp_c       gfddopo
030500170920     c                   write     tigfd000
030600170504     c                   endif
032200170504      *
032300170504      * scrivere le variazioni GRUPPO D
032400170920     c                   if        dgcp_D <> wdgcp_D
032500170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
032600170504     c                   eval      apv = apv +1
032700170504     c                   eval      datvar(apv)='D'
032800170920     c                   eval      gfdgcp = 'D'
032900170920     c                   eval      gfdprg= 0
033000170920     c                   movel(P)  dgcp_d        gfdprima
033100170920     c                   movel(P)  wdgcp_d       gfddopo
033200170920     c                   write     tigfd000
033300170504     c                   endif
033301171012      * scrivere le variazioni GRUPPO E
033302171012     c                   if        dgcp_E <> wdgcp_E
033303171012     c* Scrivo dettaglio e memorizzo che dati indicare in testata
033304171012     c                   eval      apv = apv +1
033305171012     c                   eval      datvar(apv)='E'
033306171012     c                   eval      gfdgcp = 'E'
033307171012     c                   eval      gfdprg = 0
033308171012     c                   movel(P)  dgcp_e        gfdprima
033309171012     c                   movel(P)  wdgcp_e       gfddopo
033310171012     c                   write     tigfd000
033311171012     c                   endif
034500170504      *
034600170504      *scrittura testata
034700170920     c                   movea     Datvar        gfpflo
034800170920     c                   if        gfpflo <> *blank
034900170920     c                   write     tigfp000
035000170504     c                   endif
035100170504     c                   endsr
