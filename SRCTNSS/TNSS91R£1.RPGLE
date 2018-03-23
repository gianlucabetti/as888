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
000815141114      *
000816141114      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
000817141114      *
000818141118     d kpjba         e ds
000819141118     d Tibs34ds      e ds                  inz
000820141118     d §azute        e ds                  Extname(Azute00f)
000821141118     d                                     dtaara
000822141118     d §Datiute      e ds                  EXTname(ddatiute)
000823141118     d                                     dtaara
000824141118
000825141118     d ds3a          e ds
000826141118
000827141117     D DStot           ds                  occurs(5)
000828141117      * dati import
000829141118     d  icl                           5  0
000830141118     d  ips                           9  1
000831141117     d  isp                           5  0
000832141117      * dati export
000833141117     d  ecl                           5  0
000834141118     d  eps                           9  1
000835141117     d  esp                           5  0
000836141114      * - Parametri
000837141114     d tnss90ds      e ds
000838141117     d  sk_fil                17     31  0 dim(5)
000839141114
000840141114      *
000841141114      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
000842141114      *
000843141114      * - Campi di comodo
000844141118     d dataiso_cor     s               d   datfmt(*iso)
000845141118     d dataeur_cor     s               d   datfmt(*eur)
000846141118     d dataeur         s               d   datfmt(*eur)
000847141118     d waas            s                   like(tasaas)
000848141118     d waasMax         s                   like(tasaas)
000849141118     d wmgs            s                   like(tasmgs)
000850141118     d wmgsMax         s                   like(tasmgs)
000851141114     d wtrc            s                   like(ta4trc) inz('M')
000852141118     d wimp            s              1  0
000853141118     d wexp            s              1  0
000854141118     d i               s              3  0
000855141119     d n               s              3  0
000856141119     d PosIn           s              3  0
000857141119     d wtitolo2        s                   like(ptitolo2)
000858141114     D Wlibsede        S             21
000859141114     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS38C')
000860141114     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS38C')
000861141114     D WlibsedeP4      S             21    inz('GAITRAGRPS/TITA430C')
000862141114     D WlibsedeB4      S             21    INZ('GAITRAGRU /TITA430C')
000863141118      // ? PROTOTIPI ?
000864141118      /copy gaitrasrc/srcprotopr,tibs34r
000865141114
000866141114     c     *Entry        plist
000867141114     c                   parm                    KPJBA
000868141114     c
000869141114     c                   movel     kpjbu         tnss90ds
000870141114      *
000871141114      /free
000872141117
000873141114          exsr sr_elabora;
000874141118          exsr sr_stampa ;
000875141114
000876141114       eval *inlr=*on;
000877141114       //-------------------------------------------------------------*
000878141114       //Routine conteggio spedizioni/peso/colli                      *
000879141114       //-------------------------------------------------------------*
000880141114       begsr sr_elabora;
000881141117
000882141117          waas=%dec(%subst(%editc(SS90DSD:'X'):1:4):4:0);
000883141117          wmgs=%dec(%subst(%editc(SS90DSD:'X'):5:4):4:0);
000884141117          waasMax=%dec(%subst(%editc(SS90DSa:'X'):1:4):4:0);
000885141117          wmgsMax=%dec(%subst(%editc(SS90DSa:'X'):5:4):4:0);
000886141117
000887141114          setll (waas: wmgs) titas38c;
000888141114          read  titas38c;
000889141114          dow not %eof(titas38c);
000890141118             clear wimp;
000891141118             clear wexp;
000892141114           // Uscita dal ciclo se superata data al
000893141117             if tasaas>Waasmax or (tasaas=Waasmax and tasmgs>WmgsMax);
000894141114                leave;
000895141114             endif;
000896141118           //Verifico se lnp o lna della bolla presente
000897141118             i=%lookup(taslnp:sk_fil);
000898141118             if i>0;
000899141118                eval wimp=i;
000900141118             else;
000901141118                i=%lookup(taslna:sk_fil);
000902141118                if i>0;
000903141118                   eval wexp=i;
000904141118                endif;
000905141118             endif;
000906141118           // Scarto bolla se lnp o lna non rientra nelle filiali richieste
000907141118           // Scarto bolla se non in porto assegnato
000908141118             if (wimp>*zeros  or  wexp>*zeros ) and %lookup(tascbo:sk_ass)>0;
000909141114           // Scarto bolla se senza ORM
000910141118                chain (tasaas: taslnp: tasnrs: tasnsp: wtrc) tita430c;
000911141118                if  %found(tita430c);
000912141118
000913141118           // memorizzo peso colli e contatore spedizioni
000914141118                   exsr sr_memdati;
000915141114
000916141118                endif;
000917141118             endif;
000918141118
000919141114             read titas38c  ;
000920141114          enddo;
000921141114       endsr;
000922141117       //-------------------------------------------------------------*
000923141117       //Routine memorizzazione dati in schiera                       *
000924141117       //-------------------------------------------------------------*
000925141117       begsr sr_memdati;
000926141118       //
000927141118       if wimp>0   ;
000928141118          %occur(dstot)=wimp;
000929141118          icl+=tasncl;
000930141118          if tasncp=tasncl or taspkc>taspkf;
000931141118             ips+=taspkc;
000932141118          else;
000933141118             ips+=taspkf;
000934141118          endif;
000935141118          isp+=1     ;
000936141118       else;
000937141118          %occur(dstot)=wexp;
000938141118          ecl+=tasncl;
000939141119          if tasncp=tasncl or taspkc>taspkf;
000940141119             eps+=taspkc;
000941141119          else;
000942141119             eps+=taspkf;
000943141119          endif;
000944141118          esp+=1     ;
000945141118       endif ;
000946141117       endsr;
000947141118       //-------------------------------------------------------------*
000948141118       //Routine di stampa                                            *
000949141118       //-------------------------------------------------------------*
000950141118       begsr sr_stampa ;
000951141118          open tnss91p;
000952141118       // inversione data dal/al
000953141118          dataeur=%date(SS90DSD:*iso);
000954141118          p1dal =%dec(dataeur);
000955141118          dataeur=%date(SS90DSA:*iso);
000956141118          p1al  =%dec(dataeur);
000957141118       // elenco delle filiali di lancio  con relativa decodifica
000958141118          for i=1 to %elem(sk_fil) ;
000959141119             if sk_fil(i)>0;
000962141119                clear orgdes;
000963141119                chain sk_fil(i) azorg01l;
000964141119                eval wtitolo2=%trim(wtitolo2) + '  ' +%editc(sk_fil(i):'X') +
000965141119                              '-' + %subst(orgdes:1:10);
000966141119             endif;
000967141118          endfor;
000968141118          eval wtitolo2=%trim(wtitolo2) ;
000969141118          n=%checkr(' ':wtitolo2);
000970141119          PosIn=(%len(ptitolo2)-n)/2 ;
000971141119          eval %subst(ptitolo2:posin)=%trim(wtitolo2);
000972141119       // Stampo testata
000973141119          write ss91t00;
000974141119          *in30=*off;
000975141119
000976141119          for i=1 to %elem(sk_fil) ;
000977141119           if sk_fil(i)>0;
000980141119             clear orgdes;
000981141119             chain sk_fil(i) azorg01l;
000982141119             pfil=sk_fil(i);
000983141119             pfild=orgdes;
000984141119             %occur(dstot)=i;
000985141119             p1speI=isp;
000986141119             p1nclI=icl;
000987141119             p1pesI=ips;
000988141119             p1speE=esp;
000989141119             p1nclE=ecl;
000990141119             p1pesE=eps;
000991141119       // Stampa riga
000992141119             if *in30;
000993141119                 write ss91t00;
000994141119                 *in30=*off;
000995141119             endif;
000996141119             write ss91r00;
000997141119           endif;
000998141119          endfor;
000999141119
001000141119       // Stampa "Fine Stampa"
001001141119          write fines;
001002141118       endsr ;
001003141114       //-------------------------------------------------------------*
001004141114       //Routine Iniziale                                             *
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
003100141114       i=0;
003200141114       setll (1:'3A') tabel00f  ;
003300141114       reade (1:'3A') tabel00f  ;
003400141114
003500141114  1    dow    not %eof(tabel00f)  ;
003600141114          ds3a=tbluni           ;
003700141114          if %subst(§3atb1:1:1)='A';
003800141114             i=i+1;
003900141114             sk_ass(i)=%subst(tblkey:1:%len(tascbo));
004000141114          endif;
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
