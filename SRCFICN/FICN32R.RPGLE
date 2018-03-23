000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200130122     h dftactgrp(*no) actgrp(*caller)
000300060220      **
000400080111     ffifce02l  if   e           k disk    RENAME(fifce000:fifce002)
000500060306     F                                     INFDS(FCEDS)
000600060306     ffifce00F  uf   e             disk    prefix(pf_)
000700060220      **
000800020610     feccet30c  if   e           k disk
000900020521     fecced30c  if   e           k disk
001000020517     ftitas30c  if   e           k disk
001100021203     ffiapd01l  if   e           k disk
001200030414     ffiftd05l  if   e           k disk
001300030417     ffiftt01l  if   e           k disk
001400020521     ffifss01l  uf a e           k disk
001500030321     ftabel00f  if   e           k disk
001600060220      **
001700060220     ftntbe01l  if   e           k disk
001800060220     fwfchk00f  o  a e           k disk
001900060220      **
002000020517     fqsysprt   o    f  132        printer infds(stamp)
002100030414      *--------------------------------------------------------------------
002200030522     D MSGT            S             70    DIM(1) CTDATA PERRCD(1)              MSG PER SEDE
002300030522     D MSG             S             66    DIM(3) CTDATA PERRCD(1)              MSG PER SEDE
002400040119     D MSGCT           S             70    DIM(1) CTDATA PERRCD(1)              MSG PER SEDE
002500040119     D MSGC            S             66    DIM(3) CTDATA PERRCD(1)              MSG PER SEDE
002600030522      *--------------------------------------------------------------------
002700060306      **  num.rel.record x accesso a file fisico
002800060306     D fceDS           DS
002900060306     D  fceNRR               397    400B 0
003000060306      *--------------------------------------------------------------------
003100060306      **
003200020626     d kpjba         e ds
003300030321     d ds3a          e ds
003400080924      *
003500130121     d Stampa_SI       s              1    inz('N')
003600130122     d Bolla_trs       s              1    inz('S')
003700030415     d Sum_record      s              1    inz('S')
003800030321     d vuoto           s              2    inz
003900030414     d Calc_Cmp        s              1
004000030321     d i               s              3  0 inz
004100030321     d sk3a            s              2    dim(50)
004200030414      *
004300060220     d dchk          e ds
004400060220     d Fila            s              3    dim(20)
004500060220     d dataoggi        s               d   datfmt(*iso)
004600060220     D WLBDAT          DS
004700060220     D  G02DAT                 1      8  0
004800060220     D  G02INV                 9     16  0
004900060220     D  G02ERR                17     17
005000060220     D  G02TGI                18     22  0
005100060220      *
005200030414     d NrFCE           s              9  0 inz
005300030414     d NoCnsNr         s              9  0 inz
005400030414     d NoCetNr         s              9  0 inz
005500030415     d NoFtdNr         s              9  0 inz
005600030417     d TotValC         s             12  3 inz
005700030417     d TotValR         s             12  3 inz
005800030414      *
005900030414     d  savaas         s                   like(fceaas)
006000030414     d  savlnp         s                   like(fcelnp)
006100030414     d  savnrs         s                   like(fcenrs)
006200030414     d  savnsp         s                   like(fcensp)
006300030414     d  savddc         s                   like(fceddc)
006400030417     d  savndc         s                   like(fcendc)
006500030414     d  savpdr         s                   like(fcepdr)
006600030414     d  savfgs         s                   like(fcefgs)
006700030414     d  savtsr         s                   like(fcetsr)
006800060125     d  savpdr_Fitt    s                   like(pdr_fitt)
006900090925     d  savfss001£     s                   like(fss001b)
007000090925     d  savfss005£     s                   like(fss005b)
007100090925     d  savfss016£     s                   like(fss016b)
007200090925     d  savfss019£     s                   like(fss019b)
007300090925     d  savfss034£     s                   like(fss034b)
007400090925     d  savfss035£     s                   like(fss035b)
007500090925     d  savfssXX1£     s                   like(fssXX1b)
007600090925     d  savfssXX2£     s                   like(fssXX2b)
007700090925     d  savfss997£     s                   like(fss997b)
007800090925     d  savfss998£     s                   like(fss998b)
007900030414      *
008000030414     d Costi_fss       ds
008100030414     d  savfss001                          like(fss001)
008200030414     d  savfss005                          like(fss005)
008300030414     d  savfss016                          like(fss016)
008400030414     d  savfss019                          like(fss019)
008500030414     d  savfss034                          like(fss034)
008600030414     d  savfss035                          like(fss035)
008700030414     d  savfssXX1                          like(fssXX1)
008800030414     d  savfssXX2                          like(fssXX2)
008900030414     d  savfss997                          like(fss997)
009000030414     d  savfss998                          like(fss998)
009100060220      *----------------------------------------------
009200030414      *
009300030414     d Compe_fss       ds
009400090925     d  savfss001b                         like(cetpar)
009500090925     d  savfss005b                         like(cetpar)
009600090925     d  savfss016b                         like(cetpar)
009700090925     d  savfss019b                         like(cetpar)
009800090925     d  savfss034b                         like(cetpar)
009900090925     d  savfss035b                         like(cetpar)
010000090925     d  savfssXX1b                         like(cetpar)
010100090925     d  savfssXX2b                         like(cetpar)
010200090925     d  savfss997b                         like(cetpar)
010300090925     d  savfss998b                         like(cetpar)
010400060220      *----------------------------------------------
010500030414      *
010600060124     d  fceFGS_sav     s                   like(fceFGS)
010700060124     d  fcePDR_sav     s                   like(fcePDR)
010800060124     d  fceDDC_sav     s                   like(fceDDC)
010900060123      *
011000060220      *  campi di totalizzazione x check x bolla
011100090925     d  tot_CHK001B    s                   like(cetpar)
011200090925     d  tot_CHK005B    s                   like(cetpar)
011300090925     d  tot_CHK016B    s                   like(cetpar)
011400090925     d  tot_CHK019B    s                   like(cetpar)
011500090925     d  tot_CHK034B    s                   like(cetpar)
011600090925     d  tot_CHK035B    s                   like(cetpar)
011700090925     d  tot_CHK997B    s                   like(cetpar)
011800090925     d  tot_CHK998B    s                   like(cetpar)
011900090925     d  tot_CHKXX1B    s                   like(cetpar)
012000090925     d  tot_CHKXX2B    s                   like(cetpar)
012100090925     d  tot_CHKTOTB    s                   like(cetpar)
012200060220      *
012300020517     d stamp           ds
012400020517     d  lin                  367    368b 0
012500031001      *--------------------------------------------------------------------
012600031001     D SndMg01         S             10                                         Message type
012700031001     D                                     INZ('*INFO')
012800031001     D SndMg02         S             10                                         Deluvery mode
012900031001     D                                     INZ('*BREAK')
013000031001     D SndMg03         S            256                                         Message text
013100031001     D SndMg04         S             10I 0                                      Length of text
013200031001     D                                     INZ(%SIZE(SndMg03))
013300031001     D SndMg05         S             10                                         User profile
013400031001     D                                     DIM(299)
013500031001     D SndMg06         S             10I 0                                      Number of user
013600031001     D SndMg07         S             10I 0                                      Message sent indic.
013700031001     D SndMg08         S             10I 0                                      Function requested
013800031001     D SndMg10         S              1                                         Show display
013900031001     D                                     INZ('N')
014000031001     D SndMg11         S             20                                         Qualified MSGQ name
014100031001     D SndMg12         S              4                                         Name type indicator
014200031001     D                                     INZ('*USR')
014300031001      * indice di scihera
014400031001     D X               S              5I 0
014500031001      *
014600031001     D/COPY QSYSINC/QRPGLESRC,QUSEC
014700000607      *--------------------------------------------------------------------
014800020521      *loop sui record di fce ancora da trattare
014900030414     c     *loval        setll     fifce02l
015000060123     c                   exsr      READ_FCE02
015100030414      *  x EoF
0152000304141    c                   dow       not %Eof(fifce02l)
015300030414      *
015400030414      *  salva chiave con cui fare i saldi
015500030414     c                   eval      savfgs = fcefgs
015600030414     c                   eval      savddc = fceddc
015700030414     c                   eval      savpdr = fcepdr
015800060125     c                   eval      savpdr_fitt = pdr_fitt
015900030414      *
016000030414      *  Pulizia campi da totalizzare x DDC/PDR
016100030414     c                   clear                   Costi_Fss
016200030414     c                   clear                   Compe_Fss
016300030414      *
016400030414      *  x FGS/DDC/PDR
0165000304142    c                   dow       not %Eof(fifce02l) and
016600030414     c                             savfgs  =  fcefgs  and
016700030414     c                             savddc  =  fceddc  and
016800030414     c                             savpdr  =  fcepdr
016900030414      *  salva chiave Bolla
017000030417     c                   eval      savndc = fcendc
017100030414     c                   eval      savaas = fceaas
017200030414     c                   eval      savlnp = fcelnp
017300030414     c                   eval      savnrs = fcenrs
017400030414     c                   eval      savnsp = fcensp
017500030414     c                   eval      savtsr = fcetsr
017600030414      *
017700030414      * A rottura di Bolla
017800030414     C                   exsr      Check_Bolla
017900030414      *
018000030414      *  x FGS/DDC/PDR/Bolla/TSR
0181000304143    c                   dow       not %Eof(fifce02l) and
018200030414     c                             savfgs  =  fcefgs  and
018300030414     c                             savddc  =  fceddc  and
018400030414     c                             savpdr  =  fcepdr  and
018500030417     c                             savndc  =  fcendc  and
018600030414     c                             savaas  =  fceaas  and
018700030414     c                             savlnp  =  fcelnp  and
018800030414     c                             savnrs  =  fcenrs  and
018900030414     c                             savnsp  =  fcensp  and
019000030414     c                             savtsr  =  fcetsr
019100030414      * Totalizza i Costi
019200030415     c                   if        sum_record ='S'
019300030414     c                   exsr      somma_Costi
019400030414      *
019500030414      *  Una volta letto e totalizzato:
019600030414      *   mette il flag di saldato sul record FCE
019700030414     c                   add       1             NrFCE
019800060306     c*******            movel     'S'           fcesal
019900060306      *
020000060306      * aggiorna  con il fisico
020100060306     c     fcenrr        chain     fifce00f
020200060306     c                   if        %Found(fifce00f)
020300060306     c                   eval      pf_fcesal ='S'
020400060306      * aggiorna
020500060306     c                   update    fifce000
020600060306     c                   end
020700060220     c                   endIf
020800060220      *
020900060123     c                   exsr      READ_FCE02
0210000304143    c                   enddo
021100030414      * a rottura di Bolla
0212000304142    c                   enddo
021300030414      *
021400030414      * a rottura di FGS/DDC/PDR
021500030414     C                   exsr      wri_Saldi
021600030414      *
0217000304141-   c                   enddo
021800030414      *
021900030522      * Invia msg a sede per FTD non ricevuti
022000030522     c                   if        NoFTDNr>0
022100030522     c                   exsr      SEND_MSG_EDP
022200030522     c                   end
022300030522      *
022400040119     c                   if        NoCETnr>500
022500040119     c                   exsr      SEND_MSG_CET
022600040119     c                   end
022700040119      *
022800130121     c                   if        Stampa_SI = 'S'
022900030414      *                 * ------------------ *
023000030414     c                   except    elaborati
023100030414      *                 * ------------------ *
023200130121     c                   end
023300130121      *
023400030414     c                   seton                                        lr
023500060125      * ?==================================================================*
023600060125      * ?Lettura del record                                                *
023700060125      * ?==================================================================*
023800060123     c     READ_FCE02    begsr
023900030414      *
024000060123      *  Lettura FILE
024100060123     c     ri_legge      tag
024200060123     c                   read      fifce02l
024300060123      *
0244000601231    c                   if        not %Eof(fifce02l)
024500060123      *
024600060123     c                   if        fceFGS <> fceFGS_sav or
024700060123     c                             fcePDR <> fcePDR_sav or
024800060123     c                             fceDDC <> fceDDC_sav
024900060125      *
025000060125      * ?Controlla se fittizio (Escluso da Fatturazione)
025100060125     c                   clear                   pdr_fitt          1
025200060125     c     kapdfit       chain     fiapd01l
025300060125     c                   if        %Found(Fiapd01L) and apdPDD ='S'
025400060125     c                   move      'S'           pdr_fitt
025500060125     c                   end
025600060123      *
025700060125      *  Controlla che il record letto appartenga ad una testata padroncini
025800060123      *  già consolidata in SEDE altrimenti deve saltare a leggerne un'altra.
025900060123      *  X essere consolidata in sede deve avere il flag FTR ='R' come ricevuto
026000060123      *   oltre che essere Confermata FVL='C' e ovviamente deve essrci la riga
026100060123      *   di totale ossia TSR = blank
026200060123     c                   move      'N'           rec_valido        1
026300060123      *
026400060123     c     kftt_Blk      chain     fiFTT01L
026500060123      *
026600060125      * ?Attenzione se il Padroncino è Fittizio non esiste il FIFTT Confermato
026700060208     c                   if        (%Found(fiFTT01L) and
026800060208     c                                 FTTFTR = 'R' and FTTFVL = 'C') or
026900060125      * ? ma proprio xchè fittizio deve essere considerato il record letto
027000060125     c                                pdr_fitt = 'S'
027100060123     c                   move      'S'           rec_valido
027200060123     c                   move      fceFGS        fceFGS_sav
027300060123     c                   move      fcePDR        fcePDR_sav
027400060123     c                   move      fceDDC        fceDDC_sav
027500060123     c                   end
027600060125      *
027700060123      * Se non dovesse essere presente il record in sede deve passare alla
027800060123      *  lettura x chiave superiore
027900060123     c                   if        rec_valido = 'N'
028000060123     c     kfce_sup      setgt     fifce02l
028100060123     c                   goto      ri_legge
028200060123     c                   end
028300060123      *
028400060123     c                   end
028500060123     c                   end
028600060123      *
028700060123     c                   endsr
028800060125      * ?==================================================================*
028900130122      *      Controlli sulla Bolla
029000060220      * ?==================================================================*
029100060220     c     Check_Bolla   begsr
029200060220      *
029300030415      * imposta il default per aggiornare il flag di saldato su FIFCE
029400030415      *  e sommare costi/competenze
029500030415     c                   eval      sum_record ='S'
029600030415      *
029700030415      * ------------------ *
029800030415      * Verifico se è una di quelle bolle da non trasmettere             .
029900030415      *  utilizzando la tab.: 3A
030000030415     c                   move      'S'           Bolla_trs
0301000304152    c                   if        fcecbo <> *blank
030200030415     c     fcecbo        lookup    sk3a                                   91
030300030415     c   91              move      'N'           Bolla_trs
0304000304152-   c                   endif
030500030415      *
030600030414      * ------------------ *
030700030414      *  Controlla data di consegna sul dettaglio Conteggi Padroncini
030800030414     c                   z-add     0             data_Cons         8 0
030900031107      *  e se è una bolla di solo incasso / oppure è parziale
031000031107     c                   clear                   Sinc_Parz         1
031100030414     c     kftd          chain     fiftd05l
031200030415      *
031300030414      * x prendere la data consegna
0314000602211    c                   if        %Found(fiftd05l) and pdr_fitt <> 'S'
031500060221      *
031600030414     c                   z-add     ftdDCM        data_Cons
031700031107      *
031800031107      * --------------------
031900031107      *  se è un solo incasso o una parziale
032000031107      *   non deve avere le competenze
032100031107      * --------------------
032200031107     c                   if        ftdSIC = 'S' or ftdCMC = 'P  '
032300031107     c                   move      'S'           Sinc_Parz
032400031107     c                   end
032500031107      *
0326000304141e   c                   else
032700030417      *
032800030415      *  Controlla se fittizio (Escluso da Fatturazione)
032900060125      * ?il controllo sul padroncino è stato fatto a monte a rottura di chiave
033000060125      * ? x sapere se era un Fittizio oppure NO.
033100060125     c                   if        pdr_fitt  <> 'S'
033200030415      *
033300030415      * se il padroncino non era fittizio poichè la bolla doveva essere
033400030415      *  stata trasmessa segnala la mancanza del FTD per possibili problemi
033500030415      *   sulle trasmissioni.
033600030415     c                   if        Bolla_trs = 'S'
033700030417      *
033800030417     c     kftt          chain     fiftt01l
033900030417      *
0340000304171    c                   if        %Found(fiftt01l)
034100030415      *  Manca il FIFTD
034200030415     c                   add       1             NoFtdNr
034300130121     c                   if        Stampa_SI = 'S'
034400030415      *                 * ------------------ *
034500030415     c                   except    noFtd
034600030415      *                 * ------------------ *
034700130121     c                   end
034800030417     c                   if        fcetsr = 'C'
034900030417     c                   add       fceice        totValC
035000030417     c                   end
035100030417     c                   if        fcetsr = 'R'
035200030417     c                   add       fceice        totValR
035300030417     c                   end
035400030415      *
035500030415      *  ......e non deve sommare  e va a fine routine
035600030415     c                   eval      sum_record ='N'
035700030415     c                   goto      end_chkbolla
035800030417      *
035900030417     c                   end
036000030417      *
036100030415     c                   end
036200030415      *
036300030415     c                   endIf
036400030415      *
036500030414      *   Se non c'è il dettaglio padroncini
036600030415      *   ...e quindi il padroncino non è stato valorizzato perchè fittizio
036700030414     c     ktas          Chain     titas30c
036800030415     c                   if        %Found(titas30c)
036900030414     c                   z-add     tasDCM        data_Cons
037000030414     c                   end
037100030414      *
0372000304141-   c                   endIf
037300030414      *
037400030414      * ------------------ *
037500030415      * solo per le prestazioni di consegna, se data consegna a zero,
037600030414      *  non sommo le competenze. Per i Ritiri invece sempre.
037700030414     c                   move      'S'           Calc_Cmp
037800030414      *
037900030414     c                   if        Data_Cons = 0 and fcetsr='C'
038000030414     c******             add       1             NoCnsNr
038100130121     c*******            if        Stampa_SI = 'S'
038200030414     c******             except    nocons
038300130121     c*******            end
038400030414     c                   move      'N'           Calc_Cmp
038500030414     c                   end
038600030414      *
038700031107      * Attenzione: se è una bolla di solo incasso o di consegna parziale
038800031107      *  non deve prendere le competenze:
038900031107     c                   if        Sinc_parz ='S'
039000031107     c                   move      'N'           Calc_Cmp
039100031107     c                   end
039200031107      *
039300030414      * ------------------ * * ------------------ * * ------------------ *
039400030415      *  Quindi solo se deve calcolare le competenze
039500030415     c                   if        Calc_Cmp = 'S'
039600030415      *
039700030414      *  Cerca record su CE
039800030414     c     kcet          chain     eccet30c
039900030414      *
040000030414      *  Non c'è il C/E
0401000304141    c                   if        not %Found(eccet30c)
040200030414      *  Se la bolla è stata trasmessa e non ho il C/E segnalo errore
0403000304142    C                   IF        Bolla_Trs ='S'
040400030417      *
040500030417      *  Controlla che ci sia la bolla in sede altrimenti è una "£" di cui deve prendere
040600030417      *   i costi e non le competenze
040700030417     c     ktas          Chain     titas30c
040800030417     c                   if        %Found(titas30c)
040900030417      *
041000130121     c                   if        Stampa_SI = 'S'
041100030414      *                 * ------------------ *
041200030414     c                   except    nocet
041300030414      *                 * ------------------ *
041400130121     c                   end
041500030414     c                   add       1             NoCetNr
041600030417     c                   if        fcetsr = 'C'
041700030417     c                   add       fceice        totValC
041800030417     c                   end
041900030417     c                   if        fcetsr = 'R'
042000030417     c                   add       fceice        totValR
042100030417     c                   end
042200030415      *
042300030415      *  ......e non deve sommare  e va a fine routine
042400030415     c                   eval      sum_record ='N'
042500030415     c                   goto      end_chkbolla
042600030417      *
0427000304172-   c                   end
042800030415      *
0429000304142-   c                   end
043000030414      *
0431000304141e   c                   else
043200030414      *  Ho le Competenze
043300030414      *   solo se devono essere calcolate le sommo
0434000304142    c                   if        Calc_Cmp = 'S'
043500030414      *
043600060220      *  Pulisce campi Spia x bolla totalizzanti le competenze
043700060220     c                   clear                   tot_CHK001B
043800060220     c                   clear                   tot_CHK005B
043900060220     c                   clear                   tot_CHK016B
044000060220     c                   clear                   tot_CHK019B
044100060220     c                   clear                   tot_CHK034B
044200060220     c                   clear                   tot_CHK035B
044300060220     c                   clear                   tot_CHK997B
044400060220     c                   clear                   tot_CHK998B
044500060220     c                   clear                   tot_CHKXX1B
044600060220     c                   clear                   tot_CHKXX2B
044700060220     c                   clear                   tot_CHKTOTB
044800060220      *
044900030414      *  e somma relativamente al Tipo Servizio le relative competenze
045000030415     c                   select
0451000304153    c                   when      fcetsr = 'R'
045200030414     c                   add       cetpar        savfss001b
045300060220     c                   add       cetpar        tot_CHK001B
045400060220     c                   add       cetpar        tot_CHKTOTB
0455000304153    c                   when      fcetsr = 'C'
045600030414     c                   add       cetard        savfss005b
045700060220     c                   add       cetard        tot_CHK005B
045800060220     c                   add       cetard        tot_CHKTOTB
0459000304153-   c                   endsl
046000030414      *
046100030414      *  quindi cerca le competenze di dettaglio se ci sono e le somma
0462000304143    c                   if        cetdet = 'S'
046300030414     c                   exsr      somma_Compe
0464000304143-   c                   endif
046500030414      *
046600060220      *  Controllo su filiali in esame: carica un work file x bolla/data
046700060220      *   x rilevare le competenze delle singole bolle .
046800060220     c                   move      fcefgs        fgsalfa           3
046900060220     c     fgsalfa       lookup    fila                                   33
047000060220     c                   if        *in33
047100060220     c                   exsr      wriSpia_Comp
047200060220     c                   end
047300060220      *
0474000304142-   c                   endif
047500030415      *
0476000304141-   c                   endif
047700030415      *
0478000304151-   c                   endif
047900030414      *
048000030415     c     end_chkbolla  endsr
048100030414     c*==================================================================*
048200030414     c     Somma_Compe   begsr
048300030414      *
048400020521     c     kced          setll     ecced30c
048500030414     c     kced          reade     ecced30c
048600030414      *
048700030414     c                   dow       not %Eof(ecced30c)
048800030414      *
048900020521     c                   select
049000030415     c                   when      cedcmp = 16 and fcetsr = 'C'
049100030414     c                   add       cedimp        savfss016b
049200060220     c                   add       cedimp        tot_CHK016B
049300060220     c                   add       cedimp        tot_CHKTOTB
049400030414      *
049500030415     c                   when      cedcmp = 19 and fcetsr = 'C'
049600030414     c                   add       cedimp        savfss019b
049700060220     c                   add       cedimp        tot_CHK019B
049800060220     c                   add       cedimp        tot_CHKTOTB
049900030414      *
050000030415     c                   when      cedcmp = 34 and fcetsr = 'C'
050100030414     c                   add       cedimp        savfss034b
050200060220     c                   add       cedimp        tot_CHK034B
050300060220     c                   add       cedimp        tot_CHKTOTB
050400030414      *
050500030415     c                   when      cedcmp = 35 and fcetsr = 'R'
050600030414     c                   add       cedimp        savfss035b
050700060220     c                   add       cedimp        tot_CHK035B
050800060220     c                   add       cedimp        tot_CHKTOTB
050900030430      *
051000030430     c                   when      cedcmp = 24 and fcetsr = 'C'
051100030430     c                   add       cedimp        savfssxx1b
051200060220     c                   add       cedimp        tot_CHKXX1B
051300060220     c                   add       cedimp        tot_CHKTOTB
051400030430      *
051500030430     c                   when      cedcmp = 43 and fcetsr = 'C'
051600030430     c                   add       cedimp        savfssxx2b
051700060220     c                   add       cedimp        tot_CHKXX2B
051800060220     c                   add       cedimp        tot_CHKTOTB
051900030430      *
052000030430     c                   when      cedcmp = 62 and fcetsr = 'C'
052100030430     c                   add       cedimp        savfssxx2b
052200060220     c                   add       cedimp        tot_CHKXX2B
052300060220     c                   add       cedimp        tot_CHKTOTB
052400030430      *
052500020521     c                   endsl
052600030414      *
052700030414     c     kced          reade     ecced30c
052800020521     c                   enddo
052900030414      *
053000020521     c                   endsr
053100030414     c*==================================================================*
053200060220      * scrive il record spia x le competenze da monitorare
053300060220     c*==================================================================*
053400060220     c     wriSpia_Comp  begsr
053500030414      *
053600060220     c                   movel     wdata8        CHKdat
053700060220     c                   movel     woramin       CHKtim
053800060220     c                   movel     fceFGS        CHKFGS
053900060220     c                   movel     fcePDR        CHKPDR
054000060220     c                   movel     fceTSR        CHKTSR
054100060220     c                   movel     fceNDC        CHKNDC
054200060220     c                   movel     fceDDC        CHKDDC
054300060220     c                   movel     fceAAS        CHKAAS
054400060220     c                   movel     fceLNP        CHKLNP
054500060220     c                   movel     fceNRS        CHKNRS
054600060220     c                   movel     fceNSP        CHKNSP
054700090925     c                   z-add(h)  tot_CHK001B   CHK001B
054800090925     c                   z-add(h)  tot_CHK005B   CHK005B
054900090925     c                   z-add(h)  tot_CHK016B   CHK016B
055000090925     c                   z-add(h)  tot_CHK019B   CHK019B
055100090925     c                   z-add(h)  tot_CHK034B   CHK034B
055200090925     c                   z-add(h)  tot_CHK035B   CHK035B
055300090925     c                   z-add(h)  tot_CHK997B   CHK997B
055400090925     c                   z-add(h)  tot_CHK998B   CHK998B
055500090925     c                   z-add(h)  tot_CHKXX1B   CHKXX1B
055600090925     c                   z-add(h)  tot_CHKXX2B   CHKXX2B
055700090925     c                   z-add(h)  tot_CHKTOTB   CHKTOTB
055800060220     c                   write     wfchk000
055900060220      *
056000060220     c                   endsr
056100060220     c*==================================================================*
056200060220     c     somma_costi   begsr
056300060220      *
056400020521     c                   select
056500020521     c                   when      fcecce = '001'
056600030414     c                   add       fceice        savfss001
056700030414      *
056800020521     c                   when      fcecce = '005'
056900030414     c                   add       fceice        savfss005
057000030414      *
057100020521     c                   when      fcecce = '016'
057200030414     c                   add       fceice        savfss016
057300030414      *
057400020521     c                   when      fcecce = '019'
057500030414     c                   add       fceice        savfss019
057600030414      *
057700020521     c                   when      fcecce = '034'
057800030414     c                   add       fceice        savfss034
057900030414      *
058000020521     c                   when      fcecce = '035'
058100030414     c                   add       fceice        savfss035
058200030414      *
058300020521     c                   when      fcecce = '997'
058400030414     c                   add       fceice        savfss997
058500030414      *
058600020521     c                   when      fcecce = '998'
058700030414     c                   add       fceice        savfss998
058800020521     c                   endsl
058900030414      *
059000020521     c                   endsr
059100030414     c*==================================================================*
059200030414      * Scrive i saldi
059300030414     c*==================================================================*
059400030414     c     Wri_saldi     begsr
059500030414      *
059600030414      * Controlla esistenza Saldi giornalieri
059700030414     c     kfss          chain     fifss01l
059800030414     c                   if        not %Found(fifss01l)
059900030414      *
060000030414     c                   clear                   fifss000
060100060125      *****
060200060125     c                   move      savpdr_fitt   fsspdd
060300030414      *
060400030414     c                   move      savpdr        fsspdr
060500030414     c                   move      savddc        fssddc
060600030414     c                   move      savfgs        fssfgs
060700030414      *
060800030414      *sommo comunque i costi da FCE e le Competenze
060900030414     c                   exsr      Costi_in_Fss
061000030414     c                   exsr      Compe_in_Fss
061100080924      *
061200080924      * ?su testata Conteggi x prendere eventuale sovrapprezzo carburante
061300080924     c     kfss_Blk      chain     fiFTT01L
061400080924     c                   if        %Found(fiFTT01L)
061500080924     c                   z-add     fttPCAR       fssXX1
061600080924     c                   endif
061700030414      *
061800030414     c                   write     fifss000
061900030414      *
062000030414     c                   else
062100030414      *
062200030414      *sommo comunque i costi da FCE e le Competenze
062300030414     c                   exsr      Costi_in_Fss
062400030414     c                   exsr      Compe_in_Fss
062500030414      *
062600030414     c                   update    fifss000
062700030414     c                   endif
062800030414      *
062900030414     c                   endsr
063000030414     c*==================================================================*
063100030414      *  Imposta i valori Costi
063200030414     c*==================================================================*
063300030414     c     Costi_in_Fss  begsr
063400030414     c                   add       savfss001     fss001
063500030414     c                   add       savfss005     fss005
063600030414     c                   add       savfss016     fss016
063700030414     c                   add       savfss019     fss019
063800030414     c                   add       savfss034     fss034
063900030414     c                   add       savfss035     fss035
064000080924      **
064100080924      **  Il campo FSSXX1, essendo al momento NON utilizzato, lo si è preso
064200080924      **  x memorizzare il costo del Supplemento carburante da sommare
064300080924      **  fra i Costi Stimati e quindi fuori dai costi reali dei Padroncini.
064400080924     c*****              add       savfssXX1     fssXX1
064500080924      *
064600030414     c                   add       savfssXX2     fssXX2
064700030414     c                   add       savfss997     fss997
064800030414     c                   add       savfss998     fss998
064900030414     c                   endsr
065000030414     c*==================================================================*
065100030414      *  Imposta i valori Competenze
065200030414     c*==================================================================*
065300030414     c     Compe_in_Fss  begsr
065400090925     c                   z-add(h)  savfss001b    savfss001£
065500090925     c                   add       savfss001£    fss001b
065600090925     c                   z-add(h)  savfss005b    savfss005£
065700090925     c                   add       savfss005£    fss005b
065800090925     c                   z-add(h)  savfss016b    savfss016£
065900090925     c                   add       savfss016£    fss016b
066000090925     c                   z-add(h)  savfss019b    savfss019£
066100090925     c                   add       savfss019£    fss019b
066200090925     c                   z-add(h)  savfss034b    savfss034£
066300090925     c                   add       savfss034£    fss034b
066400090925     c                   z-add(h)  savfss035b    savfss035£
066500090925     c                   add       savfss035£    fss035b
066600090925     c                   z-add(h)  savfssXX1b    savfssXX1£
066700090925     c                   add       savfssXX1£    fssXX1b
066800090925     c                   z-add(h)  savfssXX2b    savfssXX2£
066900090925     c                   add       savfssXX2£    fssXX2b
067000030414     c                   endsr
067100030414     c*==================================================================*
067200030414     c     *inzsr        begsr
067300030414     c*==================================================================*
067400020626     c     *entry        plist
067500020626     c                   parm                    kpjba
067600030414      *
067700020521     c                   z-add     66            lin
067800130121     c                   if        Stampa_SI = 'S'
067900030414      *                 * ------------------ *
068000020521     c                   except    testa
068100030414      *                 * ------------------ *
068200130121     c                   end
068300030414      *
068400020521     c     ktas          klist
068500020521     c                   kfld                    fceaas
068600020521     c                   kfld                    fcelnp
068700020521     c                   kfld                    fcenrs
068800020521     c                   kfld                    fcensp
068900030414      *
069000020521     c     kcet          klist
069100030414     c                   kfld                    fceaas
069200030414     c                   kfld                    fcelnp
069300030414     c                   kfld                    fcenrs
069400030414     c                   kfld                    fcensp
069500030414      *
069600020521     c     kced          klist
069700020521     c                   kfld                    cetaas
069800020521     c                   kfld                    cetlnp
069900020521     c                   kfld                    cetnrs
070000020521     c                   kfld                    cetnsp
070100020521
070200020521     c     kfss          klist
070300030414     c                   kfld                    savfgs
070400030414     c                   kfld                    savddc
070500030414     c                   kfld                    savpdr
070600030414      *
070700030414     c     kftd          klist
070800030414     c                   kfld                    fcepdr
070900030414     c                   kfld                    fcetsr
071000030414     c                   kfld                    fcendc
071100030414     c                   kfld                    fceddc
071200030414     c                   kfld                    fceaas
071300030414     c                   kfld                    fcelnp
071400030414     c                   kfld                    fcenrs
071500030414     c                   kfld                    fcensp
071600030417      *
071700030417     c     kftt          klist
071800030417     c                   kfld                    fcepdr
071900030417     c                   kfld                    fcetsr
072000030417     c                   kfld                    fcendc
072100030417     c                   kfld                    fceddc
072200060123      *
072300060123     c     kftt_Blk      klist
072400060123     c                   kfld                    fcepdr
072500060123     c                   kfld                    TSR_blk           1
072600060123     c                   kfld                    NDC_0             7 0
072700060123     c                   kfld                    fceddc
072800060123      *
072900060123     c                   move      *blank        TSR_blk           1
073000060123     c                   move      *zeros        NDC_0             7 0
073100080924      *
073200080924     c     kfss_Blk      klist
073300080924     c                   kfld                    fsspdr
073400080924     c                   kfld                    TSR_blk           1
073500080924     c                   kfld                    NDC_0             7 0
073600080924     c                   kfld                    fssddc
073700060123
073800060123     c     kfce_sup      klist
073900060123     c                   kfld                    fcefgs
074000060123     c                   kfld                    fceddc
074100060123     c                   kfld                    fcepdr
074200030414      *
074300030415     c     kapdfit       klist
074400021203     c                   kfld                    apdtip
074500030415     c                   kfld                    fcepdr
074600030415      *
074700030415     c     kapdf         klist
074800030415     c                   kfld                    apdtip
074900030415     c                   kfld                    savpdr
075000021203     c                   movel     'A'           apdtip
075100030321      *
075200030321     c     ktab          klist
075300030321     c                   kfld                    tblkut
075400030321     c                   kfld                    tblcod
075500030321     c                   z-add     1             tblkut
075600030321     c                   move      '3A'          tblcod
075700060220      *
075800060220     c     ktbe          klist
075900060220     c                   kfld                    TBECOD
076000060220     c                   kfld                    TBEKE1
076100060220     c                   movel     'CHK'         TBECOD
076200060220     c                   movel(p)  'FIL'         TBEKE1
076300060220      *
076400060220      * x monitorare qualche filiale sui saldi
076500060220     c     ktbe          chain     tntbe01l
076600060220     c                   if        %found(tntbe01l)
076700060220     c                   movel     tbeuni        dchk
076800060220     c     ' ':'0'       xlate     dchk          dchk
076900060220     c                   movea     dchk          fila
077000060220     c                   endIF
077100030321      *
077200030321      *carico in skiera i codici bolla che non vengono trasmessi in sede x
077300030321      *forzare la somma dei rekord senza corrispondenza
077400030321     c     ktab          setll     tabel00f
077500030321     c                   do        *hival
077600030321     c     ktab          reade     tabel00f                               93
077700030321     c   93              leave
077800030321     c                   if        tblflg <> *blank
077900030321     c                   iter
078000030321     c                   end
078100030321     c                   movel     tbluni        ds3a
078200030321     c                   if        §3absd = *blank
078300030321     c                   iter
078400030321     c                   end
078500030321     c                   z-add     1             i
078600030321     c     vuoto         lookup    sk3a(i)                                92
078700030321     c   92              movel     tblkey        sk3a(i)
078800030321     c                   enddo
078900060123      **
079000060123     c                   clear                   fceFGS_sav
079100060123     c                   clear                   fcePDR_sav
079200060123     c                   clear                   fceDDC_sav
079300060220     C***
079400060220     C                   TIME                    W0140            14 0
079500060220     C                   MOVEl     W0140         Wora              6 0
079600060220     C                   MOVEl     W0140         Woramin           4 0
079700060220     C                   MOVE      W0140         WDAT              8 0
079800060220     C*
079900060220     C                   Z-ADD     Wdat          G02DAT
080000060220     C                   MOVEL     *BLANK        G02ERR
080100060220     C                   CALL      'XSRDA8'
080200060220     C                   PARM                    WLBDAT
080300060220     C* UDATE A 8 IN AAAA/MM/GG
080400060220     C                   move      G02INV        dataoggi
080500060220     C                   z-add     G02INV        Wdata8            8 0
080600060123      **
080700020521     c                   endsr
080800030522     c*==================================================================*
080900030522      * Manda un Msg in Posta AS Sede a EDP*
081000030522     c*==================================================================*
081100030522     c     SEND_MSG_EDP  begsr
081200030522      *
081300100817      * invia segnalazione via mail
081400130121     c                   eval      emlto = 'CedAlert@brt.it'
081500100817     c                   eval      emlcc = *blank
081600100817     c                   eval      emogg = 'ATTENZIONE C/E GIORNALIER-
081700100817     c                             O MANCANO TRASMISSIONI'
081800130122     c                   if        Stampa_SI = 'S'
081900100817     C                   eval      emmsg = msgt(1) + msg(1) + ' ' +
082000100817     c                             msg(2) + ' ' + msg(3)
082100130122     c                   else
082200130122     C                   eval      emmsg = msgt(1) + ' ' +
082300130122     c                             msg(2)
082400130122     c                   end
082500130122     c
082600100817     C                   CALL      'TIS701C1'
082700100817     C                   PARM                    emlto           253
082800100817     C                   PARM                    emlcc           253
082900100817     C                   PARM                    emogg            44
083000100817     C                   PARM                    emmsg          5000
083100100817     C                   PARM                    emesito           1
083200030522      *
083300031001      * INVIA MESSAGGIO ad un Utente --> EDPAB
083400031001     c                   z-add     1             x
083500031001     C                   exsr      CalQEZSNDMG
083600031001      *
083700030522     c                   endsr
083800031001     ***********************************************************************
083900031001     **
084000031001     ** Send Message (QEZSNDMG) API
084100031001     **
084200031001     ***********************************************************************
084300031001     C     CalQEZSNDMG   BEGSR
084400031001      *
084500031001     ** Invio messaggio all'utente.
084600031001     C                   EVAL      SndMg03 = 'ATTENZIONE:  Procedura saldi FICN-
084700031001     C                             32R ha riscontrato un disallineamento fra FI-
084800031001     C                             FTD00F e FIFCE00F ricevuti. Non sono arrivat-
084900130122     C                             i FIFTD da filiale.'
085000130122     c                   if        Stampa_SI = 'S'
085100130122     C                   EVAL      SndMg03 = %trim(SndMg03) +
085200130122     C                             ' Vedere stampa di controllo in Sede: -
085300130122     C                             << WRKSPLF SELECT(*ALL *ALL *ALL FICN32R) >>'
085400130122     c                   end
085500031001     C                   EVAL      SndMg05(x) = 'EDPAB     '
085600031001     C                   EVAL      SndMg06 = x
085700031001     C                   CLEAR                   QUSEC
085800031001     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
085900031001
086000031001     C                   CALL      'QEZSNDMG'
086100031001     C                   PARM                    SndMg01
086200031001     C                   PARM                    SndMg02
086300031001     C                   PARM                    SndMg03
086400031001     C                   PARM                    SndMg04
086500031001     C                   PARM                    SndMg05
086600031001     C                   PARM                    SndMg06
086700031001     C                   PARM                    SndMg07
086800031001     C                   PARM                    SndMg08
086900031001     C                   PARM                    Qusec
087000031001     C                   PARM                    SndMg10
087100031001     C                   PARM                    SndMg11
087200031001     C                   PARM                    SndMg12
087300031001      *
087400031001     C                   ENDSR
087500040119     c*==================================================================*
087600040119      * Manda un Msg in Posta AS Sede a EDP*
087700040119     c*==================================================================*
087800040119     c     SEND_MSG_CET  begsr
087900040119      *
088000100817      * invia segnalazione via mail
088100130121     c                   eval      emlto = 'CedAlert@brt.it'
088200100817     c                   eval      emlcc = *blank
088300100817     c                   eval      emogg = 'ATTENZIONE C/E GIORNALIER-
088400100817     c                             O Controllare ECCET'
088500130122     c                   if        Stampa_SI = 'S'
088600100817     C                   eval      emmsg = msgct(1) + msgc(1) + ' ' +
088700100817     c                             msgc(2) + ' ' + msgc(3)
088800130122     c                   else
088900130122     C                   eval      emmsg = msgct(1) + ' ' +
089000130122     c                             msgc(2)
089100130122     c                   end
089200100817     c
089300100817     C                   CALL      'TIS701C1'
089400100817     C                   PARM                    emlto           253
089500100817     C                   PARM                    emlcc           253
089600100817     C                   PARM                    emogg            44
089700100817     C                   PARM                    emmsg          5000
089800100817     C                   PARM                    emesito           1
089900040119      *
090000040119      * INVIA MESSAGGIO ad un Utente --> EDPAB
090100040119     c                   z-add     1             x
090200040119     C                   exsr      cetQEZSNDMG
090300040119      *
090400040119     c                   endsr
090500040119     ***********************************************************************
090600040119     **
090700040119     ** Send Message (QEZSNDMG) API
090800040119     **
090900040119     ***********************************************************************
091000040119     C     cetQEZSNDMG   BEGSR
091100040119      *
091200040119     ** Invio messaggio all'utente.
091300040119     C                   EVAL      SndMg03 = 'ATTENZIONE:  Procedura saldi FICN-
091400130122     C                             32R ha trovato oltre 500 records ECCET00F no-
091500130122     C                             n trovati x il relativo calcolo dei saldi.'
091600130122     c                   if        Stampa_SI ='S'
091700130122     C                   EVAL      SndMg03 = %trim(SndMg03) +
091800130122     C                              ' -> Vedere stampa di controllo in Sede: -
091900130122     C                             << WRKSPLF SELECT(*ALL *ALL *ALL FICN32R) >>'
092000130122     c                   end
092100040119     C                   EVAL      SndMg05(x) = 'EDPAB     '
092200040119     C                   EVAL      SndMg06 = x
092300040119     C                   CLEAR                   QUSEC
092400040119     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
092500040119
092600040119     C                   CALL      'QEZSNDMG'
092700040119     C                   PARM                    SndMg01
092800040119     C                   PARM                    SndMg02
092900040119     C                   PARM                    SndMg03
093000040119     C                   PARM                    SndMg04
093100040119     C                   PARM                    SndMg05
093200040119     C                   PARM                    SndMg06
093300040119     C                   PARM                    SndMg07
093400040119     C                   PARM                    SndMg08
093500040119     C                   PARM                    Qusec
093600040119     C                   PARM                    SndMg10
093700040119     C                   PARM                    SndMg11
093800040119     C                   PARM                    SndMg12
093900040119      *
094000040119     C                   ENDSR
094100030414     c*==================================================================*
094200020517     OQSYSPRT   e            TESTA             2
094300020517     O                                           70 'Anomalie riscontrate fluss-
094400020517     O                                              o C/E di filiale'
094500030414     O*****     e            nocons      1
094600030414     o*****                  fceaas
094700030414     o*****                  fcelnp             + 1
094800030414     o*****                  fcenrs             + 1
094900030414     o*****                  fcensp             + 1
095000030414     o*****                  fcecce             + 1
095100030414     o*****                  fceice        2    + 1
095200030414     o*****                                     + 2 'Data CONSEGNA  = 0  '
095300030415     O          e            noFtd       1
095400030415     o                       fceaas
095500030415     o                       fcelnp             + 1
095600030415     o                       fcenrs             + 1
095700030415     o                       fcensp             + 1
095800030415     o                       fcecce             + 1
095900030415     o                       fceice        2    + 1
096000030417     o                                          + 2 ' non c''è  FIFTD --->   '
096100030416     o                       fcetsr             + 1
096200030416     o                       fcepdr        Z    + 2
096300030416     o                       fcendc        Z    + 2
096400030416     o                       fceddc             + 2
096500030417     o                       fcesal             + 2
096600030415     o
096700020517     O          e            nocet       1
096800020517     o                       fceaas
096900020517     o                       fcelnp             + 1
097000020517     o                       fcenrs             + 1
097100020517     o                       fcensp             + 1
097200020517     o                       fcecce             + 1
097300020610     o                       fceice        2    + 1
097400020517     o                                          + 2 ' non trovato Rec. ECCET'
097500030416     o                       fcetsr             + 1
097600030416     o                       fcepdr        Z    + 2
097700030416     o                       fcendc        Z    + 2
097800030416     o                       fceddc             + 2
097900030417     o                       fcesal             + 2
098000080111     o                       tasLNA             + 2
098100020517     o
098200020517     O          e            elaborati   1
098300030414     o                       NrFCE         z    + 2
098400030415     o                                          + 2 'Rec.FIFCE elaborati'
098500030414     o*****                  NoCnsNr       z    + 2
098600030415     o*****                                     + 2 'Rec.TITAS no cons.'
098700030415     o                       NoFtdNr       z    + 2
098800030415     o                                          + 2 'Rec.FIFTD non trasm.'
098900030415     o                       NoCetNr       z    + 2
099000030415     o                                          + 2 'Rec.ECCET non trov.'
099100030417     O          e            elaborati   1
099200030417     o                                          + 2 'Totale Valori Costi:'
099300030417     o                       TotValC       K    + 2
099400030417     o                                          + 2 'Consegne'
099500030417     o                       TotValR       K    + 2
099600030417     o                                          + 2 'Ritiri'
099700030522** MSGT
099800030522SALDI x C/E GIORNALIERO: Mancano delle Trasmissioni
099900030522** MSG
100000030522Controllare la stampa del saldificato.
100100030522     Non sono arrivate delle trasmissioni sull'FIFTD00R
100200030522         < Controllare la stampa generata dai saldi >
100300040119** MSGCT
100400040119SALDI x C/E GIORNALIERO:(FICN32R) Controllare ECCET non trovati
100500040119** MSGC
100600040119Controllare la stampa del saldificato.
100700040119    Oltre 500 records di ECCET non trovati per calcolare i saldi
100800040119        < Controllare la stampa generata dai saldi FICN32R >
