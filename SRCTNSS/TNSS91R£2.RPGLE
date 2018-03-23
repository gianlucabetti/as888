000100000000     H*------------------------------------------------------------------------*
000200141114     H* STATISTICA ORM esteri commissionati
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000801141114     ftabel00f  if   e           k disk
000802141119     fazorg01l  if   e           k disk
000807141114     ftitas38c  if   e           k Disk    extfile(wlibsede)usropn
000808141114     ftita430c  if   e           k Disk    extfile(wlibsede)usropn
000809141119     ftnss91p   o    e             printer usropn
000810141119     f                                     oflind(*in30)
000811141117      *
000812141117      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
000813141117      *
000814141118     d sk_ass          s              2    dim(30)
000815150212     d sk_rec          s              2    dim(30)
000816141114      *
000817141114      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
000818141114      *
000819141118     d kpjba         e ds
000820141118     d Tibs34ds      e ds                  inz
000821141118     d §azute        e ds                  Extname(Azute00f)
000822141118     d                                     dtaara
000823141118     d §Datiute      e ds                  EXTname(ddatiute)
000824141118     d                                     dtaara
000825141118
000826141118     d ds3a          e ds
000827150212     d dsbl4i        e ds
000828141118
000829141117     D DStot           ds                  occurs(5)
000830141117      * dati import
000831141118     d  icl                           5  0
000832141118     d  ips                           9  1
000833141117     d  isp                           5  0
000834141117      * dati export
000835141117     d  ecl                           5  0
000836141118     d  eps                           9  1
000837141117     d  esp                           5  0
000838141114      * - Parametri
000839141114     d tnss90ds      e ds
000840141117     d  sk_fil                17     31  0 dim(5)
000841141114
000842141114      *
000843141114      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
000844141114      *
000845141114      * - Campi di comodo
000846141118     d dataiso_cor     s               d   datfmt(*iso)
000847141118     d dataeur_cor     s               d   datfmt(*eur)
000848141118     d dataeur         s               d   datfmt(*eur)
000849141118     d waas            s                   like(tasaas)
000850141118     d waasMax         s                   like(tasaas)
000851141118     d wmgs            s                   like(tasmgs)
000852141118     d wmgsMax         s                   like(tasmgs)
000853150212     d wtrc            s                   like(ta4trc) inz('I')
000854141118     d wimp            s              1  0
000855141118     d wexp            s              1  0
000856141118     d i               s              3  0
000857150212     d ir              s              3  0
000858141119     d n               s              3  0
000859141119     d PosIn           s              3  0
000860141119     d wtitolo2        s                   like(ptitolo2)
000861141114     D Wlibsede        S             21
000862141114     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS38C')
000863141114     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS38C')
000864141114     D WlibsedeP4      S             21    inz('GAITRAGRPS/TITA430C')
000865141114     D WlibsedeB4      S             21    INZ('GAITRAGRU /TITA430C')
000866141118      // ? PROTOTIPI ?
000867141118      /copy gaitrasrc/srcprotopr,tibs34r
000868141114
000869141114     c     *Entry        plist
000870141114     c                   parm                    KPJBA
000871141114     c
000872141114     c                   movel     kpjbu         tnss90ds
000873141114      *
000874141114      /free
000875141117
000876141114          exsr sr_elabora;
000877141118          exsr sr_stampa ;
000878141114
000879141114       eval *inlr=*on;
000880141114       //-------------------------------------------------------------*
000881141114       //Routine conteggio spedizioni/peso/colli                      *
000882141114       //-------------------------------------------------------------*
000883141114       begsr sr_elabora;
000884141117
000885141117          waas=%dec(%subst(%editc(SS90DSD:'X'):1:4):4:0);
000886141117          wmgs=%dec(%subst(%editc(SS90DSD:'X'):5:4):4:0);
000887141117          waasMax=%dec(%subst(%editc(SS90DSa:'X'):1:4):4:0);
000888141117          wmgsMax=%dec(%subst(%editc(SS90DSa:'X'):5:4):4:0);
000889141117
000890141114          setll (waas: wmgs) titas38c;
000891141114          read  titas38c;
000892141114          dow not %eof(titas38c);
000893141118             clear wimp;
000894141118             clear wexp;
000895141114           // Uscita dal ciclo se superata data al
000896141117             if tasaas>Waasmax or (tasaas=Waasmax and tasmgs>WmgsMax);
000897141114                leave;
000898141114             endif;
000899150212           // Proseguo solo se non è bolla di recupero
000900150212             if %lookup(tascbo:sk_rec)=0;
000901141118           //Verifico se lnp o lna della bolla presente
000902141118             i=%lookup(taslnp:sk_fil);
000903141118             if i>0;
000904141118                eval wimp=i;
000905141118             else;
000906141118                i=%lookup(taslna:sk_fil);
000907141118                if i>0;
000908141118                   eval wexp=i;
000909141118                endif;
000910141118             endif;
000911150212                if wimp>*zeros and %lookup(tascbo:sk_ass)=0;
000912150212                   clear dsbl4i;
000913150212                   Chain (tasaas: taslnp: tasnrs: tasnsp: wtrc) tita430c;
000914150212                   if  %found(tita430c);
000915150212                      eval dsbl4i=ta4not;
000916150212                   endif;
000917150212                endif;
000918141118           // Scarto bolla se lnp o lna non rientra nelle filiali richieste
000919141118           // Scarto bolla se non in porto assegnato
000920150212             if ((wimp>*zeros or wexp>*zeros ) and %lookup(tascbo:sk_ass)>0) or
000921150212                ( wimp>*zeros and %subst(§b4ipn:1:4)='0844');
000922141114           // Scarto bolla se senza ORM
000923150212           //   chain (tasaas: taslnp: tasnrs: tasnsp: wtrc) tita430c;
000924150212           //   if  %found(tita430c);
000925141118
000926141118           // memorizzo peso colli e contatore spedizioni
000927141118                   exsr sr_memdati;
000928141114
000929150212           //   endif;
000930141118             endif;
000931150212             endif;
000932141118
000933141114             read titas38c  ;
000934141114          enddo;
000935141114       endsr;
000936141117       //-------------------------------------------------------------*
000937141117       //Routine memorizzazione dati in schiera                       *
000938141117       //-------------------------------------------------------------*
000939141117       begsr sr_memdati;
000940141118       //
000941141118       if wimp>0   ;
000942141118          %occur(dstot)=wimp;
000943141118          icl+=tasncl;
000944141118          if tasncp=tasncl or taspkc>taspkf;
000945141118             ips+=taspkc;
000946141118          else;
000947141118             ips+=taspkf;
000948141118          endif;
000949141118          isp+=1     ;
000950141118       else;
000951141118          %occur(dstot)=wexp;
000952141118          ecl+=tasncl;
000953141119          if tasncp=tasncl or taspkc>taspkf;
000954141119             eps+=taspkc;
000955141119          else;
000956141119             eps+=taspkf;
000957141119          endif;
000958141118          esp+=1     ;
000959141118       endif ;
000960141117       endsr;
000961141118       //-------------------------------------------------------------*
000962141118       //Routine di stampa                                            *
000963141118       //-------------------------------------------------------------*
000964141118       begsr sr_stampa ;
000965141118          open tnss91p;
000966141118       // inversione data dal/al
000967141118          dataeur=%date(SS90DSD:*iso);
000968141118          p1dal =%dec(dataeur);
000969141118          dataeur=%date(SS90DSA:*iso);
000970141118          p1al  =%dec(dataeur);
000971141118       // elenco delle filiali di lancio  con relativa decodifica
000972141118          for i=1 to %elem(sk_fil) ;
000973141119             if sk_fil(i)>0;
000974141119                clear orgdes;
000975141119                chain sk_fil(i) azorg01l;
000976141119                eval wtitolo2=%trim(wtitolo2) + '  ' +%editc(sk_fil(i):'X') +
000977141119                              '-' + %subst(orgdes:1:10);
000978141119             endif;
000979141118          endfor;
000980141118          eval wtitolo2=%trim(wtitolo2) ;
000981141118          n=%checkr(' ':wtitolo2);
000982141119          PosIn=(%len(ptitolo2)-n)/2 ;
000983141119          eval %subst(ptitolo2:posin)=%trim(wtitolo2);
000984141119       // Stampo testata
000985141119          write ss91t00;
000986141119          *in30=*off;
000987141119
000988141119          for i=1 to %elem(sk_fil) ;
000989141119           if sk_fil(i)>0;
000990141119             clear orgdes;
000991141119             chain sk_fil(i) azorg01l;
000992141119             pfil=sk_fil(i);
000993141119             pfild=orgdes;
000994141119             %occur(dstot)=i;
000995141119             p1speI=isp;
000996141119             p1nclI=icl;
000997141119             p1pesI=ips;
000998141119             p1speE=esp;
000999141119             p1nclE=ecl;
001000141119             p1pesE=eps;
001001141119       // Stampa riga
001002141119             if *in30;
001003141119                 write ss91t00;
001004141119                 *in30=*off;
001005141119             endif;
001006141119             write ss91r00;
001007141119           endif;
001008141119          endfor;
001009141119
001010141119       // Stampa "Fine Stampa"
001011141119          write fines;
001012141118       endsr ;
001013141114       //-------------------------------------------------------------*
001014141114       //Routine Iniziale                                             *
001100141114       //-------------------------------------------------------------*
001200141114       begsr *inzsr;
001300141114
001301141118       //Reperisco dati job
001302141118         exsr datijob;
001303141118
001600141114       if %subst(knsif:7:1) = 'P';
001700141114         Wlibsede = Wlibsedep;
001800141114         Else ;
001900141114         wlibsede = Wlibsedeb;
002000141114       endif;
002100141114       Open   Titas38c;
002200141114       if %subst(knsif:7:1) = 'P';
002300141114         Wlibsede = Wlibsedep4;
002400141114         Else ;
002500141114         wlibsede = Wlibsedeb4;
002600141114       endif;
002700141114       Open   tita430c;
002800141114       //vtcpgm=SDSpgm;
002900141114       // Leggo tabella "3A" per caricare schiera dei codici bolla in assegnato
002901150212       // + schiera codici di recupero
003100141114       i=0;
003101150212       ir=0;
003200141114       setll (1:'3A') tabel00f  ;
003300141114       reade (1:'3A') tabel00f  ;
003400141114
003500141114  1    dow    not %eof(tabel00f)  ;
003600141114          ds3a=tbluni           ;
003700141114          if %subst(§3atb1:1:1)='A';
003800141114             i=i+1;
003900141114             sk_ass(i)=%subst(tblkey:1:%len(tascbo));
004000141114          endif;
004001150212          if §3arbl='R';
004002150212             ir=ir+1;
004003150212             sk_rec(ir)=%subst(tblkey:1:%len(tascbo));
004004150212          endif;
004100141114          reade (1:'3A') tabel00f  ;
004200141114  1    enddo     ;
004300141114
004400141114       // Reperisco la data corrente
004500141114       dataiso_cor=(%date());
004600141114       dataeur_cor=dataiso_cor;
004700141118       wdate=%dec(dataeur_cor);
004800141118       utime= %dec(%time());
004900141114
004901141117       // pulizia ds multipla
004902141117       for i=1 to 5;
004903141117          %occur(dstot)=i ;
004904141118          clear dstot     ;
004914141117       endfor;
005000141114       EndSr;
005100141118       //-------------------------------------------------------------*
005200141118       //Reperimento Dati del job (Utente/Operativi)                  *
005300141118       //-------------------------------------------------------------*
005400141118       Begsr DatiJob;
005500141118
005600141118          in(e) §azute;
005700141118          if not %error;
005800141118           in(e) §datiute;
005900141118          endif;
006000141118          if %error or rsut = *blanks;
006100141118           tibs34r(tibs34ds);
006200141118           in §azute;
006300141118           in §datiute;
006400141118          endif;
006700141118         endsr;
006800141118
