000100971216     h*-------------------------------------------------------------------------
000200170608     h* Registra variazioni anagrafiche gestionali
000300971216     h*-------------------------------------------------------------------------
000400020522     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000500971216     h DECEDIT('0,') DATEDIT(*DMY.)
000600971216     f*-------------------------------------------------------------------------
000700971216     f* Data base
000800170508     Ftntbe01l  iF   E           K DISK
000900171205     ffiapvd0f  o    e             disk    commit
001000171205     ffiapvt0f  o    e             disk    commit
001100971216     d*-------------------------------------------------------------------------
001200060531     d datVar          s              1    dim(15)
001300060901     d k15             s              1    dim(15)
001400170504     d fnlv36ds      e DS
001500060530     d
001600170503     d fiapdB        e DS                  extname(fiapd00f) prefix(B)
001700170503     d fiapdA        e DS                  extname(fiapd00f) prefix(A)
001800170504     d fiap4B        e DS                  extname(fiapd40f) prefix(B)
001900170504     d fiap4A        e DS                  extname(fiapd40f) prefix(A)
002000170503      *
002001170524     d DAPD          e DS
002100170504     d dapdflr       e ds                  prefix(b)
002200170503     d DAPD_A        e DS
002300170504     d DAPD_A1       e DS
002400170503     d DAPD_A2       e DS
002500170503     d DAPD_B        e DS
002600170503     d DAPD_B1       e DS
002700170503     d DAPD_C        e DS
002800170503     d DAPD_C1       e DS
002900170503     d DAPD_D        e DS
002901170524     d DAPD_D1       e DS                  prefix(§)
003000171205     d*DAPD_T        e DS
003100170503      *
003200170504     d wdapdflr      e ds                  extname(dapdflr)  prefix(a)
003300170503     d wDAPD_A       e DS                  extname(DAPD_A)   prefix(w)
003400170504     d wDAPD_A1      e DS                  extname(DAPD_A1)  prefix(w)
003500170504     d wDAPD_A2      e DS                  extname(DAPD_A2)  prefix(w)
003600170503     d wDAPD_B       e DS                  extname(DAPD_B)   prefix(w)
003700170503     d wDAPD_B1      e DS                  extname(DAPD_B1)  prefix(w)
003800170503     d wDAPD_C       e DS                  extname(DAPD_C)   prefix(w)
003900170503     d wDAPD_C1      e DS                  extname(DAPD_C1)  prefix(w)
004000170503     d wDAPD_D       e DS                  extname(DAPD_D)   prefix(w)
004001170524     d wDAPD_D1      e DS                  extname(DAPD_D1)  prefix(w§)
004100171205     d*wDAPD_T       e DS                  extname(DAPD_T)   prefix(w)
004200060530     d dataeur         s               d   datfmt(*eur)
004300060531     d conta           s              4  0 inz
004400060901     d yy              s              2  0
004500060901     d zz              s              2  0
004600971216     c*-------------------------------------------------------------------------
004700971216     c* Main lines
004800971216     c*-------------------------------------------------------------------------
004900060530     c     *ENTRY        PLIST
005000170503     c                   PARM                    fnlv36ds
005100170503     c                   PARM                    fiapdB
005200170503     c                   PARM                    fiapdA
005300170504     c                   PARM                    fiap4B
005400170504     c                   PARM                    fiap4A
005401170524      *
005402170524     c                   if        ilv36tip = *blank
005403170524     c                   if        bapdtip <> *blank
005404170524     c                   movel     bapdtip       ilv36tip
005406170524     c                   else
005407170524     c                   movel     aapdtip       ilv36tip
005409170524     c                   endif
005410170524     c                   endif
005500170508      *
005600170508     c                   clear                   fiapvt00
005700170508     c                   clear                   fiapvd00
005800170508      *
005900170508     c                   time                    w0140            14 0
006000170508     c     *eur          move      w0140         dataeur
006100170504      *
006200170508     c     'APD'         setll     tntbe01l
006300170508     c                   do        *hival
006400170508     c     'APD'         reade     tntbe01l
006500170508     c                   if        %eof(tntbe01l)
006600170508     c                   leave
006700170508     c                   endif
006701170524     c                   movel     tbeuni        dapd
006702170524     c                   select
006703170524     c                   when      ilv36tip = 'A' and §apdaut = 'S'
006800170508     c                   add       1             yy
006900170508     c                   movel     tbeke1        k15(yy)
006901170524     c                   when      ilv36tip = 'C' and §apdcop = 'S'
006902170524     c                   add       1             yy
006903170524     c                   movel     tbeke1        k15(yy)
006904170524     c                   when      ilv36tip = 'D' and §apddef = 'S'
006905170524     c                   add       1             yy
006906170524     c                   movel     tbeke1        k15(yy)
006907170524     c                   endsl
007000170508     c                   enddo
007100170508      *
007200170508      * verifica modalità di ingresso
007300170508     c                   select
007400170508     c                   when      ilv36cta = 'M'
007500170508     c                   exsr      Modifica
007600170504      *
007700170508     c                   when      ilv36cta = 'R'
007800170508     c                   exsr      SoloTestata
007801170516     c                   eval      ilv36cta = 'M'
007900170508     c                   exsr      Modifica
008000170508      *
008100170508     c                   other
008200170508     c                   exsr      SoloTestata
008300170508      *
008400170508     c                   endsl
008401171205     c* se richiesto commit
008402171205     c                   if        ILV36com = 'S'
008403171205     c                   commit
008404171205     c                   end
008500170503     c                   seton                                        lr
008600170508      *
008700170508      * ?___________________________________________________________________
008800170508      *   Scrive rekord di testata variazioni per inserimento
008900170508     c     SoloTestata   begsr
009000170508      * ?___________________________________________________________________
009100170508      *scrittura testata
009200170508     c                   movea     k15           apvtflo
009300170508     c     *iso          movel     dataeur       apvtdav
009400170508     c                   time                    apvtorv
009500170508     c                   movel     ILV36noj      apvtnoj
009600170508     c                   movel     ILV36PRU      apvtpru
009700170508     c                   movel     ILV36PGM      apvtpgm
009800170508     c                   movel     ILV36CTA      apvtcta
009900170508     c                   if        bapdtip <> *blank
010000170508     c                   movel     bapdtip       apvttip
010100170508     c                   movel     bapdpdr       apvtpdr
010200170508     c                   else
010300170508     c                   movel     aapdtip       apvttip
010400170508     c                   movel     aapdpdr       apvtpdr
010500170508     c                   endif
010600170508
010700170508     c                   write     fiapvt00
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
012400170504      *valorizzo ds filler
012500170504     c                   eval      dapdflr = bapdflr
012600170504      *valorizzo campi
012700170504      * A
012800170504     c                   eval      APDKSC  =  bAPDKSC
012900170504     c                   eval      APDCSF  =  bAPDCSF
013000170504     c                   eval      APDPDD  =  bAPDPDD
013100170504     c                   eval      APDFCM  =  bAPDFCM
013200170504      * A1
013300170504     c                   eval      APDRSF  =  bAPDRSF
013400170504      * A2
013500170504     c                   eval      APDRSC  =  bAPDRSC
013600170504      * B
013700170504     c                   eval      APDSNU  =   bAPDsnu
013800170504     c                   eval      §APDTPM =   b§APDTPM
013900170504     c                   eval      §APDTSTPDA= b§APDTSTPDA
014000170504     c                   eval      §APDPDA =   b§APDPDA
014100170504     c                   eval      §APDTURNO = b§APDTURNO
014200170504      * B1
014300170504     c                   eval      APDPTP  =  bAPDPTP
014400170504     c                   eval      §APDGCD =  b§APDGCD
014500170504     c                   eval      §APDATS  = b§APDATS
014600170504     c                   eval      §APDAAT =  b§APDAAT
014700170504     c                   eval      §APDSPEC = b§APDSPEC
014800170504      * C
014900170504     c                   eval      APDFGR  =  bAPDFGR
015000170504     c                   eval      APDETM  =  bAPDETM
015100170504     c                   eval      APDETP  =  bAPDETP
015200170504     c                   eval      APDPKM  =  bAPDPKM
015300170504     c                   eval      APDPKP  =  bAPDPKP
015400170504      * C1
015500170504     c                   eval      APDCAM  =  bAPDCAM
015600170504     c                   eval      APDCAP  =  bAPDCAP
015700170504     c                   eval      APDSCM  =  bAPDSCM
015800170504     c                   eval      APDSCP  =  bAPDSCP
015900170504      * D
016000170504     c                   eval      APDPKL  =  bAPDPKL
016100170504     c                   eval      APDVLU  =  bAPDVLU
016200170504     c                   eval      §APDMCP =  b§APDMCP
016300170504     c                   eval      §APDKM  =  b§APDKM
016400171204      * T x tipo T non utilizza più ds ma confronta campo UNI di FIAPD4
016500171204     c*                  movel     bapd4uni      apd4uni
016600170504      *
016700170504     c                   endsr
016800170508      * ?___________________________________________________________________
016900170508      *   prepara ds con immagine After per confronto
017000170504     c     RiempiAfter   begsr
017100170508      * ?___________________________________________________________________
017200170504      *valorizzo ds filler
017300170504     c                   eval      wdapdflr = aapdflr
017400170504      *valorizzo campi
017500170504      * A
017600170504     c                   eval      wAPDKSC  =  aAPDKSC
017700170504     c                   eval      wAPDCSF  =  aAPDCSF
017800170504     c                   eval      wAPDPDD  =  aAPDPDD
017900170504     c                   eval      wAPDFCM  =  aAPDFCM
018000170504      * A1
018100170504     c                   eval      wAPDRSF  =  aAPDRSF
018200170504      * A2
018300170504     c                   eval      wAPDRSC  =  aAPDRSC
018400170504      * B
018500170504     c                   eval      wAPDSNU  =  aAPDSNU
018600170504     c                   eval      w§APDTPM =   a§APDTPM
018700170504     c                   eval      w§APDTSTPDA= a§APDTSTPDA
018800170504     c                   eval      w§APDPDA =   a§APDPDA
018900170504     c                   eval      w§APDTURNO = a§APDTURNO
019000170504      * B1
019100170504     c                   eval      wAPDPTP  =  aAPDPTP
019200170504     c                   eval      w§APDGCD =  a§APDGCD
019300170504     c                   eval      w§APDATS  = a§APDATS
019400170504     c                   eval      w§APDAAT =  a§APDAAT
019500170504     c                   eval      w§APDSPEC = a§APDSPEC
019600170504      * C
019700170504     c                   eval      wAPDFGR  =  aAPDFGR
019800170504     c                   eval      wAPDETM  =  aAPDETM
019900170504     c                   eval      wAPDETP  =  aAPDETP
020000170504     c                   eval      wAPDPKM  =  aAPDPKM
020100170504     c                   eval      wAPDPKP  =  aAPDPKP
020200170504      * C1
020300170504     c                   eval      wAPDCAM  =  aAPDCAM
020400170504     c                   eval      wAPDCAP  =  aAPDCAP
020500170504     c                   eval      wAPDSCM  =  aAPDSCM
020600170504     c                   eval      wAPDSCP  =  aAPDSCP
020700170504      * D
020800170504     c                   eval      wAPDPKL  =  aAPDPKL
020900170504     c                   eval      wAPDVLU  =  aAPDVLU
021000170504     c                   eval      w§APDMCP =  a§APDMCP
021100170504     c                   eval      w§APDKM  =  a§APDKM
021200170515      * T
021300171204     c*                  movel     aapd4uni      wapd4uni
021400170504      *
021500170504     c                   endsr
021600170508      * ?___________________________________________________________________
021700170508      *   Confronta Ds con immagine prima e dopo se variate scrive dettaglio
021800170504     c     Confronta     begsr
021900170508      * ?___________________________________________________________________
022000170504      *valorizzo dati standard per tutti i rekord
022100170504     c                   clear                   DatVar
022200170504     c                   clear                   apv               2 0
022300170504     c     *iso          movel     dataeur       apvtdav
022400170504     c                   time                    apvtorv
022500170504     c                   movel     ILV36PRU      apvtpru
022600170504     c                   movel     ILV36NOJ      apvtnoj
022700170504     c                   movel     ILV36PGM      apvtpgm
022800170504     c                   movel     ILV36CTA      apvtcta
022900170504     c                   movel     bapdtip       apvttip
023000170504     c                   movel     bapdpdr       apvtpdr
023100170504
023200170504     c                   movel     apvtdav       apvddav
023300170504     c                   movel     apvtorv       apvdorv
023400170504     c                   movel     apvtnoj       apvdnoj
023500170504     c                   movel     apvttip       apvdtip
023600170504     c                   movel     apvtpdr       apvdpdr
023700170504      *confronto ds una alla volta prima e dopo per verificare se dobbiamo
023800170504      * scrivere le variazioni GRUPPO A
023900170504     c                   if        dapd_a <> wdapd_a
024000170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
024100170504     c                   z-add     1             apv
024200170504     c                   eval      datvar(apv)='A'
024300170505     c                   eval      apvdapd='A'
024400170504     c                   movel(P)  dapd_a        apvdprima
024500170504     c                   movel(P)  wdapd_a       apvddopo
024600170504     c                   write     fiapvd00
024700170504     c                   endif
024800170504      *
024900170504     c                   if        dapd_a1 <> wdapd_a1
025000170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
025100170504     c                   z-add     1             apv
025200170504     c                   eval      datvar(apv)='A'
025300170505     c                   eval      apvdapd='A'
025400170504     c                   eval      apvdprg= 1
025500170504     c                   movel(P)  dapd_a1       apvdprima
025600170504     c                   movel(P)  wdapd_a1      apvddopo
025700170504     c                   write     fiapvd00
025800170504     c                   endif
025900170504      *
026000170504     c                   if        dapd_a2 <> wdapd_a2
026100170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
026200170504     c                   z-add     1             apv
026300170504     c                   eval      datvar(apv)='A'
026400170505     c                   eval      apvdapd='A'
026500170504     c                   eval      apvdprg= 2
026600170504     c                   movel(P)  dapd_a2       apvdprima
026700170504     c                   movel(P)  wdapd_a2      apvddopo
026800170504     c                   write     fiapvd00
026900170504     c                   endif
027000170504      * scrivere le variazioni GRUPPO B
027100170516     c                   if        dapd_b <> wdapd_b and
027101170516     c                             apvttip= 'A'
027200170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
027300170505     c                   eval      apv = apv +1
027400170505     c                   eval      datvar(apv)='B'
027500170505     c                   eval      apvdapd='B'
027600170505     c                   eval      apvdprg= 0
027700170505     c                   movel(P)  dapd_b        apvdprima
027800170505     c                   movel(P)  wdapd_b       apvddopo
027900170505     c                   write     fiapvd00
028000170505     c                   endif
028100170504      *
028200170516     c                   if        dapd_b1 <> wdapd_b1 and
028201170516     c                             apvttip= 'A'
028300170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
028400170505     c                   z-add     1             zz
028500170505     c     'B'           lookup    datvar(zz)                             30
028600170505     c                   if        not *in30
028700170505     c                   eval      apv = apv +1
028800170505     c                   eval      datvar(apv)='B'
028900170505     c                   endif
029000170505     c                   eval      apvdapd='B'
029100170505     c                   eval      apvdprg= 1
029200170508     c                   movel(P)  dapd_b1       apvdprima
029300170508     c                   movel(P)  wdapd_b1      apvddopo
029400170505     c                   write     fiapvd00
029500170505     c                   endif
029600170504      * scrivere le variazioni GRUPPO C
029700170516     c                   if        dapd_C <> wdapd_C  and
029701170516     c                             apvttip= 'A'
029800170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
029900170504     c                   eval      apv = apv +1
030000170504     c                   eval      datvar(apv)='C'
030100170505     c                   eval      apvdapd='C'
030200170504     c                   eval      apvdprg= 0
030300170504     c                   movel(P)  dapd_c        apvdprima
030400170504     c                   movel(P)  wdapd_c       apvddopo
030500170504     c                   write     fiapvd00
030600170504     c                   endif
030700170504      *
030800170516     c                   if        dapd_c1 <> wdapd_c1  and
030801170516     c                             apvttip= 'A'
030900170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
031000170504     c                   z-add     1             zz
031100170504     c     'C'           lookup    datvar(zz)                             30
031200170504     c                   if        not *in30
031300170504     c                   eval      apv = apv +1
031400170504     c                   eval      datvar(apv)='C'
031500170504     c                   endif
031600170505     c                   eval      apvdapd='C'
031700170504     c                   eval      apvdprg= 1
031800170508     c                   movel(P)  dapd_c1       apvdprima
031900170508     c                   movel(P)  wdapd_c1      apvddopo
032000170504     c                   write     fiapvd00
032100170504     c                   endif
032200170504      *
032300170504      * scrivere le variazioni GRUPPO D
032400170516     c                   if        dapd_D <> wdapd_D and
032401170524     c                             apvttip= 'A'
032500170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
032600170504     c                   eval      apv = apv +1
032700170504     c                   eval      datvar(apv)='D'
032800170505     c                   eval      apvdapd='D'
032900170504     c                   eval      apvdprg= 0
033000170504     c                   movel(P)  dapd_d        apvdprima
033100170504     c                   movel(P)  wdapd_d       apvddopo
033200170504     c                   write     fiapvd00
033300170504     c                   endif
033301170524      * scrivere le variazioni GRUPPO D1
033302170524     c                   if        dapd_D <> wdapd_D and
033303170524     c                             apvttip= 'D'
033304170524     c                   movel     dapd_d        dapd_d1
033305170524     c                   movel     wdapd_d       wdapd_d1
033306170524     c                   endif
033307170524     c                   if        dapd_D1 <> wdapd_D1 and
033308170524     c                             apvttip= 'D'
033309170524     c* Scrivo dettaglio e memorizzo che dati indicare in testata
033310170524     c                   eval      apv = apv +1
033311170524     c                   eval      datvar(apv)='D'
033312170524     c                   eval      apvdapd='D'
033313170524     c                   eval      apvdprg= 1
033314170524     c                   movel(P)  dapd_d1       apvdprima
033315170524     c                   movel(P)  wdapd_d1      apvddopo
033316170524     c                   write     fiapvd00
033317170524     c                   endif
033400170504      * scrivere le variazioni GRUPPO T
033500171204     c                   if        aapd4uni <> bapd4uni and
033501170516     c                             apvttip= 'A'
033600170504     c* Scrivo dettaglio e memorizzo che dati indicare in testata
033700170515     c                   eval      apv = apv +1
033800170515     c                   eval      datvar(apv)='T'
033900170515     c                   eval      apvdapd='T'
034000170515     c                   eval      apvdprg= 0
034100171213     c                   movel(P)  bapd4uni      apvdprima
034200171213     c                   movel(P)  aapd4uni      apvddopo
034201171213     c                   move      aapd4trd      apvd4trc
034300170515     c                   write     fiapvd00
034400170515     c                   endif
034500170504      *
034600170504      *scrittura testata
034700170504     c                   movea     Datvar        apvtflo
034800170504     c                   if        apvtflo <> *blank
034900170504     c                   write     fiapvt00
035000170504     c                   endif
035100170504     c                   endsr
