000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
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
003500030414     d Bolla_trs       s              1    inz('S')
003600030415     d Sum_record      s              1    inz('S')
003700030321     d vuoto           s              2    inz
003800030414     d Calc_Cmp        s              1
003900030321     d i               s              3  0 inz
004000030321     d sk3a            s              2    dim(50)
004100030414      *
004200060220     d dchk          e ds
004300060220     d Fila            s              3    dim(20)
004400060220     d dataoggi        s               d   datfmt(*iso)
004500060220     D WLBDAT          DS
004600060220     D  G02DAT                 1      8  0
004700060220     D  G02INV                 9     16  0
004800060220     D  G02ERR                17     17
004900060220     D  G02TGI                18     22  0
005000060220      *
005100030414     d NrFCE           s              9  0 inz
005200030414     d NoCnsNr         s              9  0 inz
005300030414     d NoCetNr         s              9  0 inz
005400030415     d NoFtdNr         s              9  0 inz
005500030417     d TotValC         s             12  3 inz
005600030417     d TotValR         s             12  3 inz
005700030414      *
005800030414     d  savaas         s                   like(fceaas)
005900030414     d  savlnp         s                   like(fcelnp)
006000030414     d  savnrs         s                   like(fcenrs)
006100030414     d  savnsp         s                   like(fcensp)
006200030414     d  savddc         s                   like(fceddc)
006300030417     d  savndc         s                   like(fcendc)
006400030414     d  savpdr         s                   like(fcepdr)
006500030414     d  savfgs         s                   like(fcefgs)
006600030414     d  savtsr         s                   like(fcetsr)
006700060125     d  savpdr_Fitt    s                   like(pdr_fitt)
006800030414      *
006900030414     d Costi_fss       ds
007000030414     d  savfss001                          like(fss001)
007100030414     d  savfss005                          like(fss005)
007200030414     d  savfss016                          like(fss016)
007300030414     d  savfss019                          like(fss019)
007400030414     d  savfss034                          like(fss034)
007500030414     d  savfss035                          like(fss035)
007600030414     d  savfssXX1                          like(fssXX1)
007700030414     d  savfssXX2                          like(fssXX2)
007800030414     d  savfss997                          like(fss997)
007900030414     d  savfss998                          like(fss998)
008000060220      *----------------------------------------------
008100030414      *
008200030414     d Compe_fss       ds
008300030414     d  savfss001b                         like(fss001b)
008400030414     d  savfss005b                         like(fss005b)
008500030414     d  savfss016b                         like(fss016b)
008600030414     d  savfss019b                         like(fss019b)
008700030414     d  savfss034b                         like(fss034b)
008800030414     d  savfss035b                         like(fss035b)
008900030414     d  savfssXX1b                         like(fssXX1b)
009000030414     d  savfssXX2b                         like(fssXX2b)
009100030414     d  savfss997b                         like(fss997b)
009200030414     d  savfss998b                         like(fss998b)
009300060220      *----------------------------------------------
009400030414      *
009500060124     d  fceFGS_sav     s                   like(fceFGS)
009600060124     d  fcePDR_sav     s                   like(fcePDR)
009700060124     d  fceDDC_sav     s                   like(fceDDC)
009800060123      *
009900060220      *  campi di totalizzazione x check x bolla
010000060220     d  tot_CHK001B    s                   like(CHK001B)
010100060220     d  tot_CHK005B    s                   like(CHK005B)
010200060220     d  tot_CHK016B    s                   like(CHK016B)
010300060220     d  tot_CHK019B    s                   like(CHK019B)
010400060220     d  tot_CHK034B    s                   like(CHK034B)
010500060220     d  tot_CHK035B    s                   like(CHK035B)
010600060220     d  tot_CHK997B    s                   like(CHK997B)
010700060220     d  tot_CHK998B    s                   like(CHK998B)
010800060220     d  tot_CHKXX1B    s                   like(CHKXX1B)
010900060220     d  tot_CHKXX2B    s                   like(CHKXX2B)
011000060220     d  tot_CHKTOTB    s                   like(CHKTOTB)
011100060220      *
011200020517     d stamp           ds
011300020517     d  lin                  367    368b 0
011400031001      *--------------------------------------------------------------------
011500031001     D SndMg01         S             10                                         Message type
011600031001     D                                     INZ('*INFO')
011700031001     D SndMg02         S             10                                         Deluvery mode
011800031001     D                                     INZ('*BREAK')
011900031001     D SndMg03         S            256                                         Message text
012000031001     D SndMg04         S             10I 0                                      Length of text
012100031001     D                                     INZ(%SIZE(SndMg03))
012200031001     D SndMg05         S             10                                         User profile
012300031001     D                                     DIM(299)
012400031001     D SndMg06         S             10I 0                                      Number of user
012500031001     D SndMg07         S             10I 0                                      Message sent indic.
012600031001     D SndMg08         S             10I 0                                      Function requested
012700031001     D SndMg10         S              1                                         Show display
012800031001     D                                     INZ('N')
012900031001     D SndMg11         S             20                                         Qualified MSGQ name
013000031001     D SndMg12         S              4                                         Name type indicator
013100031001     D                                     INZ('*USR')
013200031001      * indice di scihera
013300031001     D X               S              5I 0
013400031001      *
013500031001     D/COPY QSYSINC/QRPGLESRC,QUSEC
013600000607      *--------------------------------------------------------------------
013700020521      *loop sui record di fce ancora da trattare
013800030414     c     *loval        setll     fifce02l
013900060123     c                   exsr      READ_FCE02
014000060123     c*******            read      fifce02l
014100030414      *  x EoF
0142000304141    c                   dow       not %Eof(fifce02l)
014300030414      *
014400030414      *  salva chiave con cui fare i saldi
014500030414     c                   eval      savfgs = fcefgs
014600030414     c                   eval      savddc = fceddc
014700030414     c                   eval      savpdr = fcepdr
014800060125     c                   eval      savpdr_fitt = pdr_fitt
014900030414      *
015000030414      *  Pulizia campi da totalizzare x DDC/PDR
015100030414     c                   clear                   Costi_Fss
015200030414     c                   clear                   Compe_Fss
015300030414      *
015400030414      *  x FGS/DDC/PDR
0155000304142    c                   dow       not %Eof(fifce02l) and
015600030414     c                             savfgs  =  fcefgs  and
015700030414     c                             savddc  =  fceddc  and
015800030414     c                             savpdr  =  fcepdr
015900030414      *  salva chiave Bolla
016000030417     c                   eval      savndc = fcendc
016100030414     c                   eval      savaas = fceaas
016200030414     c                   eval      savlnp = fcelnp
016300030414     c                   eval      savnrs = fcenrs
016400030414     c                   eval      savnsp = fcensp
016500030414     c                   eval      savtsr = fcetsr
016600030414      *
016700030414      * A rottura di Bolla
016800030414     C                   exsr      Check_Bolla
016900030414      *
017000030414      *  x FGS/DDC/PDR/Bolla/TSR
0171000304143    c                   dow       not %Eof(fifce02l) and
017200030414     c                             savfgs  =  fcefgs  and
017300030414     c                             savddc  =  fceddc  and
017400030414     c                             savpdr  =  fcepdr  and
017500030417     c                             savndc  =  fcendc  and
017600030414     c                             savaas  =  fceaas  and
017700030414     c                             savlnp  =  fcelnp  and
017800030414     c                             savnrs  =  fcenrs  and
017900030414     c                             savnsp  =  fcensp  and
018000030414     c                             savtsr  =  fcetsr
018100030414      * Totalizza i Costi
018200030415     c                   if        sum_record ='S'
018300030414     c                   exsr      somma_Costi
018400030414      *
018500030414      *  Una volta letto e totalizzato:
018600030414      *   mette il flag di saldato sul record FCE
018700030414     c                   add       1             NrFCE
018800060306     c*******            movel     'S'           fcesal
018900060306      *
019000060306      * aggiorna  con il fisico
019100060306     c     fcenrr        chain     fifce00f
019200060306     c                   if        %Found(fifce00f)
019300060306     c                   eval      pf_fcesal ='S'
019400060306      * aggiorna
019500060306     c                   update    fifce000
019600060306     c                   end
019700060220     c                   endIf
019800060220      *
019900060123     c                   exsr      READ_FCE02
020000060123     c*********          read      fifce02l
0201000304143    c                   enddo
020200030414      * a rottura di Bolla
0203000304142    c                   enddo
020400030414      *
020500030414      * a rottura di FGS/DDC/PDR
020600030414     C                   exsr      wri_Saldi
020700030414      *
0208000304141-   c                   enddo
020900030414      *
021000030522      * Invia msg a sede per FTD non ricevuti
021100030522     c                   if        NoFTDNr>0
021200030522     c                   exsr      SEND_MSG_EDP
021300030522     c                   end
021400030522      *
021500040119     c                   if        NoCETnr>500
021600040119     c                   exsr      SEND_MSG_CET
021700040119     c                   end
021800040119      *
021900030414      *                 * ------------------ *
022000030414     c                   except    elaborati
022100030414      *                 * ------------------ *
022200030414     c                   seton                                        lr
022300060125      * ?==================================================================*
022400060125      * ?Lettura del record                                                *
022500060125      * ?==================================================================*
022600060123     c     READ_FCE02    begsr
022700030414      *
022800060123      *  Lettura FILE
022900060123     c     ri_legge      tag
023000060123     c                   read      fifce02l
023100060123      *
0232000601231    c                   if        not %Eof(fifce02l)
023300060123      *
023400060123     c                   if        fceFGS <> fceFGS_sav or
023500060123     c                             fcePDR <> fcePDR_sav or
023600060123     c                             fceDDC <> fceDDC_sav
023700060125      *
023800060125      * ?Controlla se fittizio (Escluso da Fatturazione)
023900060125     c                   clear                   pdr_fitt          1
024000060125     c     kapdfit       chain     fiapd01l
024100060125     c                   if        %Found(Fiapd01L) and apdPDD ='S'
024200060125     c                   move      'S'           pdr_fitt
024300060125     c                   end
024400060123      *
024500060125      *  Controlla che il record letto appartenga ad una testata padroncini
024600060123      *  già consolidata in SEDE altrimenti deve saltare a leggerne un'altra.
024700060123      *  X essere consolidata in sede deve avere il flag FTR ='R' come ricevuto
024800060123      *   oltre che essere Confermata FVL='C' e ovviamente deve essrci la riga
024900060123      *   di totale ossia TSR = blank
025000060123     c                   move      'N'           rec_valido        1
025100060123      *
025200060123     c     kftt_Blk      chain     fiFTT01L
025300060123      *
025400060125      * ?Attenzione se il Padroncino è Fittizio non esiste il FIFTT Confermato
025500060208     c                   if        (%Found(fiFTT01L) and
025600060208     c                                 FTTFTR = 'R' and FTTFVL = 'C') or
025700060125      * ? ma proprio xchè fittizio deve essere considerato il record letto
025800060125     c                                pdr_fitt = 'S'
025900060123     c                   move      'S'           rec_valido
026000060123     c                   move      fceFGS        fceFGS_sav
026100060123     c                   move      fcePDR        fcePDR_sav
026200060123     c                   move      fceDDC        fceDDC_sav
026300060123     c                   end
026400060125      *
026500060123      * Se non dovesse essere presente il record in sede deve passare alla
026600060123      *  lettura x chiave superiore
026700060123     c                   if        rec_valido = 'N'
026800060123     c     kfce_sup      setgt     fifce02l
026900060123     c                   goto      ri_legge
027000060123     c                   end
027100060123      *
027200060123     c                   end
027300060123     c                   end
027400060123      *
027500060123     c                   endsr
027600060125      * ?==================================================================*
027700060220      * ?==================================================================*
027800060220     c     Check_Bolla   begsr
027900060220      *
028000030415      * imposta il default per aggiornare il flag di saldato su FIFCE
028100030415      *  e sommare costi/competenze
028200030415     c                   eval      sum_record ='S'
028300030415      *
028400030415      * ------------------ *
028500030415      * Verifico se è una di quelle bolle da non trasmettere             .
028600030415      *  utilizzando la tab.: 3A
028700030415     c                   move      'S'           Bolla_trs
0288000304152    c                   if        fcecbo <> *blank
028900030415     c     fcecbo        lookup    sk3a                                   91
029000030415     c   91              move      'N'           Bolla_trs
0291000304152-   c                   endif
029200030415      *
029300030414      * ------------------ *
029400030414      *  Controlla data di consegna sul dettaglio Conteggi Padroncini
029500030414     c                   z-add     0             data_Cons         8 0
029600031107      *  e se è una bolla di solo incasso / oppure è parziale
029700031107     c                   clear                   Sinc_Parz         1
029800030414     c     kftd          chain     fiftd05l
029900030415      *
030000030414      * x prendere la data consegna
0301000602211    c                   if        %Found(fiftd05l) and pdr_fitt <> 'S'
030200060221      *
030300030414     c                   z-add     ftdDCM        data_Cons
030400031107      *
030500031107      * --------------------
030600031107      *  se è un solo incasso o una parziale
030700031107      *   non deve avere le competenze
030800031107      * --------------------
030900031107     c                   if        ftdSIC = 'S' or ftdCMC = 'P  '
031000031107     c                   move      'S'           Sinc_Parz
031100031107     c                   end
031200031107      *
0313000304141e   c                   else
031400030417      *
031500030415      *  Controlla se fittizio (Escluso da Fatturazione)
031600060125     c*****kapdfit       chain     fiapd01l                           95
031700060125     c**n95              if        apdPDD <> 'S'
031800060125      * ?il controllo sul padroncino è stato fatto a monte a rottura di chiave
031900060125      * ? x sapere se era un Fittizio oppure NO.
032000060125     c                   if        pdr_fitt  <> 'S'
032100030415      *
032200030415      * se il padroncino non era fittizio poichè la bolla doveva essere
032300030415      *  stata trasmessa segnala la mancanza del FTD per possibili problemi
032400030415      *   sulle trasmissioni.
032500030415     c                   if        Bolla_trs = 'S'
032600030417      *
032700030417     c     kftt          chain     fiftt01l
032800030417      *
0329000304171    c                   if        %Found(fiftt01l)
033000030415      *  Manca il FIFTD
033100030415     c                   add       1             NoFtdNr
033200030415      *                 * ------------------ *
033300030415     c                   except    noFtd
033400030415      *                 * ------------------ *
033500030417     c                   if        fcetsr = 'C'
033600030417     c                   add       fceice        totValC
033700030417     c                   end
033800030417     c                   if        fcetsr = 'R'
033900030417     c                   add       fceice        totValR
034000030417     c                   end
034100030415      *
034200030415      *  ......e non deve sommare  e va a fine routine
034300030415     c                   eval      sum_record ='N'
034400030415     c                   goto      end_chkbolla
034500030417      *
034600030417     c                   end
034700030417      *
034800030415     c                   end
034900030415      *
035000030415     c                   endIf
035100030415      *
035200030414      *   Se non c'è il dettaglio padroncini
035300030415      *   ...e quindi il padroncino non è stato valorizzato perchè fittizio
035400030414     c     ktas          Chain     titas30c
035500030415     c                   if        %Found(titas30c)
035600030414     c                   z-add     tasDCM        data_Cons
035700030414     c                   end
035800030414      *
0359000304141-   c                   endIf
036000030414      *
036100030414      * ------------------ *
036200030415      * solo per le prestazioni di consegna, se data consegna a zero,
036300030414      *  non sommo le competenze. Per i Ritiri invece sempre.
036400030414     c                   move      'S'           Calc_Cmp
036500030414      *
036600030414     c                   if        Data_Cons = 0 and fcetsr='C'
036700030414     c******             add       1             NoCnsNr
036800030414     c******             except    nocons
036900030414     c                   move      'N'           Calc_Cmp
037000030414     c                   end
037100030414      *
037200031107      * Attenzione: se è una bolla di solo incasso o di consegna parziale
037300031107      *  non deve prendere le competenze:
037400031107     c                   if        Sinc_parz ='S'
037500031107     c                   move      'N'           Calc_Cmp
037600031107     c                   end
037700031107      *
037800030414      * ------------------ * * ------------------ * * ------------------ *
037900030415      *  Quindi solo se deve calcolare le competenze
038000030415     c                   if        Calc_Cmp = 'S'
038100030415      *
038200030414      *  Cerca record su CE
038300030414     c     kcet          chain     eccet30c
038400030414      *
038500030414      *  Non c'è il C/E
0386000304141    c                   if        not %Found(eccet30c)
038700030414      *  Se la bolla è stata trasmessa e non ho il C/E segnalo errore
0388000304142    C                   IF        Bolla_Trs ='S'
038900030417      *
039000030417      *  Controlla che ci sia la bolla in sede altrimenti è una "£" di cui deve prendere
039100030417      *   i costi e non le competenze
039200030417     c     ktas          Chain     titas30c
039300030417     c                   if        %Found(titas30c)
039400030417      *
039500030414      *                 * ------------------ *
039600030414     c                   except    nocet
039700030414      *                 * ------------------ *
039800030414     c                   add       1             NoCetNr
039900030417     c                   if        fcetsr = 'C'
040000030417     c                   add       fceice        totValC
040100030417     c                   end
040200030417     c                   if        fcetsr = 'R'
040300030417     c                   add       fceice        totValR
040400030417     c                   end
040500030415      *
040600030415      *  ......e non deve sommare  e va a fine routine
040700030415     c                   eval      sum_record ='N'
040800030415     c                   goto      end_chkbolla
040900030417      *
0410000304172-   c                   end
041100030415      *
0412000304142-   c                   end
041300030414      *
0414000304141e   c                   else
041500030414      *  Ho le Competenze
041600030414      *   solo se devono essere calcolate le sommo
0417000304142    c                   if        Calc_Cmp = 'S'
041800030414      *
041900060220      *  Pulisce campi Spia x bolla totalizzanti le competenze
042000060220     c                   clear                   tot_CHK001B
042100060220     c                   clear                   tot_CHK005B
042200060220     c                   clear                   tot_CHK016B
042300060220     c                   clear                   tot_CHK019B
042400060220     c                   clear                   tot_CHK034B
042500060220     c                   clear                   tot_CHK035B
042600060220     c                   clear                   tot_CHK997B
042700060220     c                   clear                   tot_CHK998B
042800060220     c                   clear                   tot_CHKXX1B
042900060220     c                   clear                   tot_CHKXX2B
043000060220     c                   clear                   tot_CHKTOTB
043100060220      *
043200030414      *  e somma relativamente al Tipo Servizio le relative competenze
043300030415     c                   select
0434000304153    c                   when      fcetsr = 'R'
043500030414     c                   add       cetpar        savfss001b
043600060220     c                   add       cetpar        tot_CHK001B
043700060220     c                   add       cetpar        tot_CHKTOTB
0438000304153    c                   when      fcetsr = 'C'
043900030414     c                   add       cetard        savfss005b
044000060220     c                   add       cetard        tot_CHK005B
044100060220     c                   add       cetard        tot_CHKTOTB
0442000304153-   c                   endsl
044300030414      *
044400030414      *  quindi cerca le competenze di dettaglio se ci sono e le somma
0445000304143    c                   if        cetdet = 'S'
044600030414     c                   exsr      somma_Compe
0447000304143-   c                   endif
044800030414      *
044900060220      *  Controllo su filiali in esame: carica un work file x bolla/data
045000060220      *   x rilevare le competenze delle singole bolle .
045100060220     c                   move      fcefgs        fgsalfa           3
045200060220     c     fgsalfa       lookup    fila                                   33
045300060220     c                   if        *in33
045400060220     c                   exsr      wriSpia_Comp
045500060220     c                   end
045600060220      *
0457000304142-   c                   endif
045800030415      *
0459000304141-   c                   endif
046000030415      *
0461000304151-   c                   endif
046200030414      *
046300030415     c     end_chkbolla  endsr
046400030414     c*==================================================================*
046500030414     c     Somma_Compe   begsr
046600030414      *
046700020521     c     kced          setll     ecced30c
046800030414     c     kced          reade     ecced30c
046900030414      *
047000030414     c                   dow       not %Eof(ecced30c)
047100030414      *
047200020521     c                   select
047300030415     c                   when      cedcmp = 16 and fcetsr = 'C'
047400030414     c                   add       cedimp        savfss016b
047500060220     c                   add       cedimp        tot_CHK016B
047600060220     c                   add       cedimp        tot_CHKTOTB
047700030414      *
047800030415     c                   when      cedcmp = 19 and fcetsr = 'C'
047900030414     c                   add       cedimp        savfss019b
048000060220     c                   add       cedimp        tot_CHK019B
048100060220     c                   add       cedimp        tot_CHKTOTB
048200030414      *
048300030415     c                   when      cedcmp = 34 and fcetsr = 'C'
048400030414     c                   add       cedimp        savfss034b
048500060220     c                   add       cedimp        tot_CHK034B
048600060220     c                   add       cedimp        tot_CHKTOTB
048700030414      *
048800030415     c                   when      cedcmp = 35 and fcetsr = 'R'
048900030414     c                   add       cedimp        savfss035b
049000060220     c                   add       cedimp        tot_CHK035B
049100060220     c                   add       cedimp        tot_CHKTOTB
049200030430      *
049300030430     c                   when      cedcmp = 24 and fcetsr = 'C'
049400030430     c                   add       cedimp        savfssxx1b
049500060220     c                   add       cedimp        tot_CHKXX1B
049600060220     c                   add       cedimp        tot_CHKTOTB
049700030430      *
049800030430     c                   when      cedcmp = 43 and fcetsr = 'C'
049900030430     c                   add       cedimp        savfssxx2b
050000060220     c                   add       cedimp        tot_CHKXX2B
050100060220     c                   add       cedimp        tot_CHKTOTB
050200030430      *
050300030430     c                   when      cedcmp = 62 and fcetsr = 'C'
050400030430     c                   add       cedimp        savfssxx2b
050500060220     c                   add       cedimp        tot_CHKXX2B
050600060220     c                   add       cedimp        tot_CHKTOTB
050700030430      *
050800020521     c                   endsl
050900030414      *
051000030414     c     kced          reade     ecced30c
051100020521     c                   enddo
051200030414      *
051300020521     c                   endsr
051400030414     c*==================================================================*
051500060220      * scrive il record spia x le competenze da monitorare
051600060220     c*==================================================================*
051700060220     c     wriSpia_Comp  begsr
051800030414      *
051900060220     c                   movel     wdata8        CHKdat
052000060220     c                   movel     woramin       CHKtim
052100060220     c                   movel     fceFGS        CHKFGS
052200060220     c                   movel     fcePDR        CHKPDR
052300060220     c                   movel     fceTSR        CHKTSR
052400060220     c                   movel     fceNDC        CHKNDC
052500060220     c                   movel     fceDDC        CHKDDC
052600060220     c                   movel     fceAAS        CHKAAS
052700060220     c                   movel     fceLNP        CHKLNP
052800060220     c                   movel     fceNRS        CHKNRS
052900060220     c                   movel     fceNSP        CHKNSP
053000060220     c                   movel     tot_CHK001B   CHK001B
053100060220     c                   movel     tot_CHK005B   CHK005B
053200060220     c                   movel     tot_CHK016B   CHK016B
053300060220     c                   movel     tot_CHK019B   CHK019B
053400060220     c                   movel     tot_CHK034B   CHK034B
053500060220     c                   movel     tot_CHK035B   CHK035B
053600060220     c                   movel     tot_CHK997B   CHK997B
053700060220     c                   movel     tot_CHK998B   CHK998B
053800060220     c                   movel     tot_CHKXX1B   CHKXX1B
053900060220     c                   movel     tot_CHKXX2B   CHKXX2B
054000060220     c                   movel     tot_CHKTOTB   CHKTOTB
054100060220     c                   write     wfchk000
054200060220      *
054300060220     c                   endsr
054400060220     c*==================================================================*
054500060220     c     somma_costi   begsr
054600060220      *
054700020521     c                   select
054800020521     c                   when      fcecce = '001'
054900030414     c                   add       fceice        savfss001
055000030414      *
055100020521     c                   when      fcecce = '005'
055200030414     c                   add       fceice        savfss005
055300030414      *
055400020521     c                   when      fcecce = '016'
055500030414     c                   add       fceice        savfss016
055600030414      *
055700020521     c                   when      fcecce = '019'
055800030414     c                   add       fceice        savfss019
055900030414      *
056000020521     c                   when      fcecce = '034'
056100030414     c                   add       fceice        savfss034
056200030414      *
056300020521     c                   when      fcecce = '035'
056400030414     c                   add       fceice        savfss035
056500030414      *
056600020521     c                   when      fcecce = '997'
056700030414     c                   add       fceice        savfss997
056800030414      *
056900020521     c                   when      fcecce = '998'
057000030414     c                   add       fceice        savfss998
057100020521     c                   endsl
057200030414      *
057300020521     c                   endsr
057400030414     c*==================================================================*
057500030414      * Scrive i saldi
057600030414     c*==================================================================*
057700030414     c     Wri_saldi     begsr
057800030414      *
057900030414      * Controlla esistenza Saldi giornalieri
058000030414     c     kfss          chain     fifss01l
058100030414     c                   if        not %Found(fifss01l)
058200030414      *
058300030414     c                   clear                   fifss000
058400060125      *****
058500060125      ***** Eliminata lettura su Fiapd poichè è fatta a monte
058600060125     c*****kapdf         chain     fiapd01l                           95
058700060125     c**n95              move      apdpdd        fsspdd
058800060125     c                   move      savpdr_fitt   fsspdd
058900030414      *
059000030414     c                   move      savpdr        fsspdr
059100030414     c                   move      savddc        fssddc
059200030414     c                   move      savfgs        fssfgs
059300030414      *
059400030414      *sommo comunque i costi da FCE e le Competenze
059500030414     c                   exsr      Costi_in_Fss
059600030414     c                   exsr      Compe_in_Fss
059700080924      *
059800080924      * ?su testata Conteggi x prendere eventuale sovrapprezzo carburante
059900080924     c     kfss_Blk      chain     fiFTT01L
060000080924     c                   if        %Found(fiFTT01L)
060100080924     c                   z-add     fttPCAR       fssXX1
060200080924     c                   endif
060300030414      *
060400030414     c                   write     fifss000
060500030414      *
060600030414     c                   else
060700030414      *
060800030414      *sommo comunque i costi da FCE e le Competenze
060900030414     c                   exsr      Costi_in_Fss
061000030414     c                   exsr      Compe_in_Fss
061100030414      *
061200030414     c                   update    fifss000
061300030414     c                   endif
061400030414      *
061500030414     c                   endsr
061600030414     c*==================================================================*
061700030414      *  Imposta i valori Costi
061800030414     c*==================================================================*
061900030414     c     Costi_in_Fss  begsr
062000030414     c                   add       savfss001     fss001
062100030414     c                   add       savfss005     fss005
062200030414     c                   add       savfss016     fss016
062300030414     c                   add       savfss019     fss019
062400030414     c                   add       savfss034     fss034
062500030414     c                   add       savfss035     fss035
062600080924      **
062700080924      **  Il campo FSSXX1, essendo al momento NON utilizzato, lo si è preso
062800080924      **  x memorizzare il costo del Supplemento carburante da sommare
062900080924      **  fra i Costi Stimati e quindi fuori dai costi reali dei Padroncini.
063000080924     c*****              add       savfssXX1     fssXX1
063100080924      *
063200030414     c                   add       savfssXX2     fssXX2
063300030414     c                   add       savfss997     fss997
063400030414     c                   add       savfss998     fss998
063500030414     c                   endsr
063600030414     c*==================================================================*
063700030414      *  Imposta i valori Competenze
063800030414     c*==================================================================*
063900030414     c     Compe_in_Fss  begsr
064000030414     c                   add       savfss001b    fss001b
064100030414     c                   add       savfss005b    fss005b
064200030414     c                   add       savfss016b    fss016b
064300030414     c                   add       savfss019b    fss019b
064400030414     c                   add       savfss034b    fss034b
064500030414     c                   add       savfss035b    fss035b
064600030414     c                   add       savfssXX1b    fssXX1b
064700030414     c                   add       savfssXX2b    fssXX2b
064800030414     c                   endsr
064900030414     c*==================================================================*
065000030414     c     *inzsr        begsr
065100030414     c*==================================================================*
065200020626     c     *entry        plist
065300020626     c                   parm                    kpjba
065400030414      *
065500020521     c                   z-add     66            lin
065600030414      *                 * ------------------ *
065700020521     c                   except    testa
065800030414      *                 * ------------------ *
065900030414      *
066000020521     c     ktas          klist
066100020521     c                   kfld                    fceaas
066200020521     c                   kfld                    fcelnp
066300020521     c                   kfld                    fcenrs
066400020521     c                   kfld                    fcensp
066500030414      *
066600020521     c     kcet          klist
066700030414     c                   kfld                    fceaas
066800030414     c                   kfld                    fcelnp
066900030414     c                   kfld                    fcenrs
067000030414     c                   kfld                    fcensp
067100030414      *
067200020521     c     kced          klist
067300020521     c                   kfld                    cetaas
067400020521     c                   kfld                    cetlnp
067500020521     c                   kfld                    cetnrs
067600020521     c                   kfld                    cetnsp
067700020521
067800020521     c     kfss          klist
067900030414     c                   kfld                    savfgs
068000030414     c                   kfld                    savddc
068100030414     c                   kfld                    savpdr
068200030414      *
068300030414     c     kftd          klist
068400030414     c                   kfld                    fcepdr
068500030414     c                   kfld                    fcetsr
068600030414     c                   kfld                    fcendc
068700030414     c                   kfld                    fceddc
068800030414     c                   kfld                    fceaas
068900030414     c                   kfld                    fcelnp
069000030414     c                   kfld                    fcenrs
069100030414     c                   kfld                    fcensp
069200030417      *
069300030417     c     kftt          klist
069400030417     c                   kfld                    fcepdr
069500030417     c                   kfld                    fcetsr
069600030417     c                   kfld                    fcendc
069700030417     c                   kfld                    fceddc
069800060123      *
069900060123     c     kftt_Blk      klist
070000060123     c                   kfld                    fcepdr
070100060123     c                   kfld                    TSR_blk           1
070200060123     c                   kfld                    NDC_0             7 0
070300060123     c                   kfld                    fceddc
070400060123      *
070500060123     c                   move      *blank        TSR_blk           1
070600060123     c                   move      *zeros        NDC_0             7 0
070700080924      *
070800080924     c     kfss_Blk      klist
070900080924     c                   kfld                    fsspdr
071000080924     c                   kfld                    TSR_blk           1
071100080924     c                   kfld                    NDC_0             7 0
071200080924     c                   kfld                    fssddc
071300060123
071400060123     c     kfce_sup      klist
071500060123     c                   kfld                    fcefgs
071600060123     c                   kfld                    fceddc
071700060123     c                   kfld                    fcepdr
071800030414      *
071900030415     c     kapdfit       klist
072000021203     c                   kfld                    apdtip
072100030415     c                   kfld                    fcepdr
072200030415      *
072300030415     c     kapdf         klist
072400030415     c                   kfld                    apdtip
072500030415     c                   kfld                    savpdr
072600021203     c                   movel     'A'           apdtip
072700030321      *
072800030321     c     ktab          klist
072900030321     c                   kfld                    tblkut
073000030321     c                   kfld                    tblcod
073100030321     c                   z-add     1             tblkut
073200030321     c                   move      '3A'          tblcod
073300060220      *
073400060220     c     ktbe          klist
073500060220     c                   kfld                    TBECOD
073600060220     c                   kfld                    TBEKE1
073700060220     c                   movel     'CHK'         TBECOD
073800060220     c                   movel(p)  'FIL'         TBEKE1
073900060220      *
074000060220      * x monitorare qualche filiale sui saldi
074100060220     c     ktbe          chain     tntbe01l
074200060220     c                   if        %found(tntbe01l)
074300060220     c                   movel     tbeuni        dchk
074400060220     c     ' ':'0'       xlate     dchk          dchk
074500060220     c                   movea     dchk          fila
074600060220     c                   endIF
074700030321      *
074800030321      *carico in skiera i codici bolla che non vengono trasmessi in sede x
074900030321      *forzare la somma dei rekord senza corrispondenza
075000030321     c     ktab          setll     tabel00f
075100030321     c                   do        *hival
075200030321     c     ktab          reade     tabel00f                               93
075300030321     c   93              leave
075400030321     c                   if        tblflg <> *blank
075500030321     c                   iter
075600030321     c                   end
075700030321     c                   movel     tbluni        ds3a
075800030321     c                   if        §3absd = *blank
075900030321     c                   iter
076000030321     c                   end
076100030321     c                   z-add     1             i
076200030321     c     vuoto         lookup    sk3a(i)                                92
076300030321     c   92              movel     tblkey        sk3a(i)
076400030321     c                   enddo
076500060123      **
076600060123     c                   clear                   fceFGS_sav
076700060123     c                   clear                   fcePDR_sav
076800060123     c                   clear                   fceDDC_sav
076900060220     C***
077000060220     C                   TIME                    W0140            14 0
077100060220     C                   MOVEl     W0140         Wora              6 0
077200060220     C                   MOVEl     W0140         Woramin           4 0
077300060220     C                   MOVE      W0140         WDAT              8 0
077400060220     C*
077500060220     C                   Z-ADD     Wdat          G02DAT
077600060220     C                   MOVEL     *BLANK        G02ERR
077700060220     C                   CALL      'XSRDA8'
077800060220     C                   PARM                    WLBDAT
077900060220     C* UDATE A 8 IN AAAA/MM/GG
078000060220     C                   move      G02INV        dataoggi
078100060220     C                   z-add     G02INV        Wdata8            8 0
078200060123      **
078300020521     c                   endsr
078400030522     c*==================================================================*
078500030522      * Manda un Msg in Posta AS Sede a EDP*
078600030522     c*==================================================================*
078700030522     c     SEND_MSG_EDP  begsr
078800030522      *
078900030522      * Invio Messaggio
079000030522     C                   CALL      'TNSYM4C'
079100030522     C                   PARM                    MSGT
079200030522     C                   PARM                    MSG
079300030522     C                   PARM      046           FIL               3 0
079400030522     C                   PARM      'EDP*'        UTEN             10
079500030522      *
079600031001      * INVIA MESSAGGIO ad un Utente --> EDPAB
079700031001     c                   z-add     1             x
079800031001     C                   exsr      CalQEZSNDMG
079900031001      *
080000030522     c                   endsr
080100031001     ***********************************************************************
080200031001     **
080300031001     ** Send Message (QEZSNDMG) API
080400031001     **
080500031001     ***********************************************************************
080600031001     C     CalQEZSNDMG   BEGSR
080700031001      *
080800031001     ** Invio messaggio all'utente.
080900031001     C                   EVAL      SndMg03 = 'ATTENZIONE:  Procedura saldi FICN-
081000031001     C                             32R ha riscontrato un disallineamento fra FI-
081100031001     C                             FTD00F e FIFCE00F ricevuti. Non sono arrivat-
081200031001     C                             i FIFTD da filiale vedere stampa di controll-
081300031001     C                             o: << WRKSPLF SELECT(*ALL *ALL *ALL FICN32R)-
081400031001     C                              >> in Sede.'
081500031001     C                   EVAL      SndMg05(x) = 'EDPAB     '
081600031001     C                   EVAL      SndMg06 = x
081700031001     C                   CLEAR                   QUSEC
081800031001     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
081900031001
082000031001     C                   CALL      'QEZSNDMG'
082100031001     C                   PARM                    SndMg01
082200031001     C                   PARM                    SndMg02
082300031001     C                   PARM                    SndMg03
082400031001     C                   PARM                    SndMg04
082500031001     C                   PARM                    SndMg05
082600031001     C                   PARM                    SndMg06
082700031001     C                   PARM                    SndMg07
082800031001     C                   PARM                    SndMg08
082900031001     C                   PARM                    Qusec
083000031001     C                   PARM                    SndMg10
083100031001     C                   PARM                    SndMg11
083200031001     C                   PARM                    SndMg12
083300031001      *
083400031001     C                   ENDSR
083500040119     c*==================================================================*
083600040119      * Manda un Msg in Posta AS Sede a EDP*
083700040119     c*==================================================================*
083800040119     c     SEND_MSG_CET  begsr
083900040119      *
084000040119      * Invio Messaggio
084100040119     C                   CALL      'TNSYM4C'
084200040119     C                   PARM                    MSGCT
084300040119     C                   PARM                    MSGC
084400040119     C                   PARM      046           FIL               3 0
084500040119     C                   PARM      'EDP*'        UTEN             10
084600040119      *
084700040119      * INVIA MESSAGGIO ad un Utente --> EDPAB
084800040119     c                   z-add     1             x
084900040119     C                   exsr      cetQEZSNDMG
085000040119      *
085100040119     c                   endsr
085200040119     ***********************************************************************
085300040119     **
085400040119     ** Send Message (QEZSNDMG) API
085500040119     **
085600040119     ***********************************************************************
085700040119     C     cetQEZSNDMG   BEGSR
085800040119      *
085900040119     ** Invio messaggio all'utente.
086000040119     C                   EVAL      SndMg03 = 'ATTENZIONE:  Procedura saldi FICN-
086100040119     C                             32R ha riscontrato oltre 500 records ECCET00-
086200040119     C                             F non trovati x il relativo calcolo dei sald-
086300040119     C                             i. -> Vedere stampa di controllo: -
086400040119     C                             << WRKSPLF SELECT(*ALL *ALL *ALL FICN32R)-
086500040119     C                              >> in Sede.'
086600040119     C                   EVAL      SndMg05(x) = 'EDPAB     '
086700040119     C                   EVAL      SndMg06 = x
086800040119     C                   CLEAR                   QUSEC
086900040119     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
087000040119
087100040119     C                   CALL      'QEZSNDMG'
087200040119     C                   PARM                    SndMg01
087300040119     C                   PARM                    SndMg02
087400040119     C                   PARM                    SndMg03
087500040119     C                   PARM                    SndMg04
087600040119     C                   PARM                    SndMg05
087700040119     C                   PARM                    SndMg06
087800040119     C                   PARM                    SndMg07
087900040119     C                   PARM                    SndMg08
088000040119     C                   PARM                    Qusec
088100040119     C                   PARM                    SndMg10
088200040119     C                   PARM                    SndMg11
088300040119     C                   PARM                    SndMg12
088400040119      *
088500040119     C                   ENDSR
088600030414     c*==================================================================*
088700020517     OQSYSPRT   e            TESTA             2
088800020517     O                                           70 'Anomalie riscontrate fluss-
088900020517     O                                              o C/E di filiale'
089000030414     O*****     e            nocons      1
089100030414     o*****                  fceaas
089200030414     o*****                  fcelnp             + 1
089300030414     o*****                  fcenrs             + 1
089400030414     o*****                  fcensp             + 1
089500030414     o*****                  fcecce             + 1
089600030414     o*****                  fceice        2    + 1
089700030414     o*****                                     + 2 'Data CONSEGNA  = 0  '
089800030415     O          e            noFtd       1
089900030415     o                       fceaas
090000030415     o                       fcelnp             + 1
090100030415     o                       fcenrs             + 1
090200030415     o                       fcensp             + 1
090300030415     o                       fcecce             + 1
090400030415     o                       fceice        2    + 1
090500030417     o                                          + 2 ' non c''è  FIFTD --->   '
090600030416     o                       fcetsr             + 1
090700030416     o                       fcepdr        Z    + 2
090800030416     o                       fcendc        Z    + 2
090900030416     o                       fceddc             + 2
091000030417     o                       fcesal             + 2
091100030415     o
091200020517     O          e            nocet       1
091300020517     o                       fceaas
091400020517     o                       fcelnp             + 1
091500020517     o                       fcenrs             + 1
091600020517     o                       fcensp             + 1
091700020517     o                       fcecce             + 1
091800020610     o                       fceice        2    + 1
091900020517     o                                          + 2 ' non trovato Rec. ECCET'
092000030416     o                       fcetsr             + 1
092100030416     o                       fcepdr        Z    + 2
092200030416     o                       fcendc        Z    + 2
092300030416     o                       fceddc             + 2
092400030417     o                       fcesal             + 2
092500080111     o                       tasLNA             + 2
092600020517     o
092700020517     O          e            elaborati   1
092800030414     o                       NrFCE         z    + 2
092900030415     o                                          + 2 'Rec.FIFCE elaborati'
093000030414     o*****                  NoCnsNr       z    + 2
093100030415     o*****                                     + 2 'Rec.TITAS no cons.'
093200030415     o                       NoFtdNr       z    + 2
093300030415     o                                          + 2 'Rec.FIFTD non trasm.'
093400030415     o                       NoCetNr       z    + 2
093500030415     o                                          + 2 'Rec.ECCET non trov.'
093600030417     O          e            elaborati   1
093700030417     o                                          + 2 'Totale Valori Costi:'
093800030417     o                       TotValC       K    + 2
093900030417     o                                          + 2 'Consegne'
094000030417     o                       TotValR       K    + 2
094100030417     o                                          + 2 'Ritiri'
094200030522** MSGT
094300030522SALDI x C/E GIORNALIERO: Mancano delle Trasmissioni
094400030522** MSG
094500030522Controllare la stampa del saldificato.
094600030522     Non sono arrivate delle trasmissioni sull'FIFTD00R
094700030522         < Controllare la stampa generata dai saldi >
094800040119** MSGCT
094900040119SALDI x C/E GIORNALIERO:(FICN32R) Controllare ECCET non trovati
095000040119** MSGC
095100040119Controllare la stampa del saldificato.
095200040119    Oltre 500 records di ECCET non trovati per calcolare i saldi
095300040119        < Controllare la stampa generata dai saldi FICN32R >
