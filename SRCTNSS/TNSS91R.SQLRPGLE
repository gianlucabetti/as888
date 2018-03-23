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
000809150224     ftitaa30c  if   e           k Disk    extfile(wlibsede)usropn
000810171031     ffipnd01l  if   e           k disk
000811150224     fwfSDP00f  o    e             disk
000812141119     ftnss91p   o    e             printer usropn
000813141119     f                                     oflind(*in30)
000814141117      *
000815141117      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
000816141117      *
000817141118     d sk_ass          s              2    dim(30)
000818150212     d sk_rec          s              2    dim(30)
000819141114      *
000820141114      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
000821141114      *
000822141118     d kpjba         e ds
000823141118     d Tibs34ds      e ds                  inz
000824141118     d §azute        e ds                  Extname(Azute00f)
000825141118     d                                     dtaara
000826141118     d §Datiute      e ds                  EXTname(ddatiute)
000827141118     d                                     dtaara
000828150224     d TIBS69DS      E DS                  INZ
000829150224     d DS_cnaco      E DS                  extname(CNACO00F)
000830150224     d DS_cnind      E DS                  extname(CNIND00F)
000831150224     d DS_cnclp      E DS                  extname(CNCLP00F)
000832150224     d DS_fncls      E DS                  extname(FNCLS00F)
000833141118
000834141118     d ds3a          e ds
000835171031     d*dsbl4i        e ds
000836150225     d dta4a         e ds
000837141118
000838141117     D DStot           ds                  occurs(5)
000839141117      * dati import
000840141118     d  icl                           5  0
000841141118     d  ips                           9  1
000842141117     d  isp                           5  0
000843141117      * dati export
000844141117     d  ecl                           5  0
000845141118     d  eps                           9  1
000846141117     d  esp                           5  0
000847141114      * - Parametri
000848141114     d tnss90ds      e ds
000849141117     d  sk_fil                17     31  0 dim(5)
000850141114
000851141114      *
000852141114      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
000853141114      *
000854141114      * - Campi di comodo
000855141118     d dataiso_cor     s               d   datfmt(*iso)
000856141118     d dataeur_cor     s               d   datfmt(*eur)
000857141118     d dataeur         s               d   datfmt(*eur)
000858141118     d waas            s                   like(tasaas)
000859141118     d waasMax         s                   like(tasaas)
000860141118     d wmgs            s                   like(tasmgs)
000861141118     d wmgsMax         s                   like(tasmgs)
000862171031     d*wtrc            s                   like(ta4trc) inz('I')
000863141118     d wimp            s              1  0
000864141118     d wexp            s              1  0
000865150224     d wpeso           s                   like(taspkf)
000866150224     d wfil            s                   like(sdpfil)
000867150224     d rsd2            s             35
000868141118     d i               s              3  0
000869150212     d ir              s              3  0
000870141119     d n               s              3  0
000871141119     d PosIn           s              3  0
000872141119     d wtitolo2        s                   like(ptitolo2)
000873141114     D Wlibsede        S             21
000874141114     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS38C')
000875141114     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS38C')
000876141114     D WlibsedeP4      S             21    inz('GAITRAGRPS/TITA430C')
000877141114     D WlibsedeB4      S             21    INZ('GAITRAGRU /TITA430C')
000878150224     D WlibsedePa      S             21    inz('GAITRAGRPS/TITAA30C')
000879150224     D WlibsedeBa      S             21    INZ('GAITRAGRU /TITAA30C')
000880141118      // ? PROTOTIPI ?
000881141118      /copy gaitrasrc/srcprotopr,tibs34r
000882150224      /copy gaitrasrc/srcprotopr,tibs69r
000883141114
000884141114     c     *Entry        plist
000885141114     c                   parm                    KPJBA
000886141114     c
000887141114     c                   movel     kpjbu         tnss90ds
000888141114      *
000889141114      /free
000890150224         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
000891150224
000892150224          for i=1 to %elem(sk_fil) ;
000893150224             if sk_fil(i)>0;
000894150224                 wfil=sk_fil(i);
000895150224                 exec sql
000896150224                 DELETE from wfsdp00F
000897150224                 WHERE sdpfil = :wfil ;
000898150224             endif;
000899150224          endfor;
000900141117
000901141114          exsr sr_elabora;
000902141118          exsr sr_stampa ;
000903141114
000904141114       eval *inlr=*on;
000905141114       //-------------------------------------------------------------*
000906141114       //Routine conteggio spedizioni/peso/colli                      *
000907141114       //-------------------------------------------------------------*
000908141114       begsr sr_elabora;
000909141117
000910141117          waas=%dec(%subst(%editc(SS90DSD:'X'):1:4):4:0);
000911141117          wmgs=%dec(%subst(%editc(SS90DSD:'X'):5:4):4:0);
000912141117          waasMax=%dec(%subst(%editc(SS90DSa:'X'):1:4):4:0);
000913141117          wmgsMax=%dec(%subst(%editc(SS90DSa:'X'):5:4):4:0);
000914141117
000915141114          setll (waas: wmgs) titas38c;
000916141114          read  titas38c;
000917141114          dow not %eof(titas38c);
000918141118             clear wimp;
000919141118             clear wexp;
000920141114           // Uscita dal ciclo se superata data al
000921141117             if tasaas>Waasmax or (tasaas=Waasmax and tasmgs>WmgsMax);
000922141114                leave;
000923141114             endif;
000924150212           // Proseguo solo se non è bolla di recupero
000925150212             if %lookup(tascbo:sk_rec)=0;
000926141118           //Verifico se lnp o lna della bolla presente
000927141118             i=%lookup(taslnp:sk_fil);
000928141118             if i>0;
000929141118                eval wimp=i;
000930141118             else;
000931141118                i=%lookup(taslna:sk_fil);
000932141118                if i>0;
000933141118                   eval wexp=i;
000934141118                endif;
000935141118             endif;
000936150224                if wimp>*zeros and %lookup(tascbo:sk_ass)=0;
000937171031           //      clear dsbl4i;
000938171031           //      Chain (tasaas: taslnp: tasnrs: tasnsp: wtrc) tita430c;
000939171031           //      if  %found(tita430c);
000940171031           //         eval dsbl4i=ta4not;
000941171031           //      endif;
000942171031                   clear pndipn;
000943171031                   Chain (tasaas: taslnp: tasnrs: tasnsp) fipnd01l;
000947150224                endif;
000948141118           // Scarto bolla se lnp o lna non rientra nelle filiali richieste
000949141118           // Scarto bolla se non in porto assegnato
000950150212             if ((wimp>*zeros or wexp>*zeros ) and %lookup(tascbo:sk_ass)>0) or
000951171031           //   ( wimp>*zeros and %subst(§b4ipn:1:4)='0844');
000952171031                ( wimp>*zeros and %subst(pndipn:1:4)='0844');
000953141114           // Scarto bolla se senza ORM
000954150212           //   chain (tasaas: taslnp: tasnrs: tasnsp: wtrc) tita430c;
000955150212           //   if  %found(tita430c);
000956141118
000957141118           // memorizzo peso colli e contatore spedizioni
000958141118                   exsr sr_memdati;
000959150224           // memorizzo spedizione in WF
000960150224                 if wexp>*zeros;
000961150223                   exsr sr_wfile;
000962150224                 endif;
000963141114
000964150212           //   endif;
000965141118             endif;
000966150212             endif;
000967141118
000968141114             read titas38c  ;
000969141114          enddo;
000970141114       endsr;
000971141117       //-------------------------------------------------------------*
000972141117       //Routine memorizzazione dati in schiera                       *
000973141117       //-------------------------------------------------------------*
000974141117       begsr sr_memdati;
000975141118       //
000976150224       clear wpeso;
000977150224
000978141118       if wimp>0   ;
000979141118          %occur(dstot)=wimp;
000980141118          icl+=tasncl;
000981141118          if tasncp=tasncl or taspkc>taspkf;
000982141118             ips+=taspkc;
000983141118          else;
000984141118             ips+=taspkf;
000985141118          endif;
000986141118          isp+=1     ;
000987141118       else;
000988141118          %occur(dstot)=wexp;
000989141118          ecl+=tasncl;
000990141119          if tasncp=tasncl or taspkc>taspkf;
000991141119             eps+=taspkc;
000992150224             wpeso=taspkc;
000993141119          else;
000994141119             eps+=taspkf;
000995150224             wpeso=taspkf;
000996141119          endif;
000997141118          esp+=1     ;
000998141118       endif ;
000999141117       endsr;
001000150224       //-------------------------------------------------------------*
001001150224       //Routine memorizzazione record sped. in wfile                 *
001002150224       //-------------------------------------------------------------*
001003150224       begsr sr_wfile  ;
001004150224
001005150224       clear wfsdp000;
001006150224       //
001007150225       SDPDEL=%dec(dataiso_cor);
001008150225       SDPUSR=knmus;
001009150225       SDPDAL=ss90dsd;
001010150225       SDPAL =ss90dsa;
001011150224       sdpfil=taslna;
001012150224       // dati mittente
001013150224       if %subst(%editc(tasccm:'X'):4:4)='0000' or
001014150224          %subst(%editc(tasccm:'X'):4:4)='8888'         ;
001015150224          chain (tasaas: taslnp: tasnrs: tasnsp: 'M') titaa30c;
001016150224          if %found(titaa30c);
001017150224             SDPRSM=taarsc;
001018150224             SDPINM=taaind;
001019150224             SDPCAM=taacap;
001020150224             SDPLOM=taaloc;
001021150224             SDPPRM=taaprv;
001022150224             SDPNZM=taanaz;
001023150224          endif;
001024150224       else;
001025150224          clear tibs69ds;
001026150224          i69kac=tasccm;
001027150224          i69kin=tasccm;
001028150224          clear ds_cnaco;
001029150224          clear ds_cnind;
001030150224          clear ds_cnclp;
001031150224          clear ds_fncls;
001032150224          tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
001033150224          SDPRSM=acorag;
001034150224          SDPINM=indvia;
001035150224          SDPCAM=indcae;
001036150224          SDPLOM=indcit;
001037150224          SDPPRM=indprv;
001038150224          SDPNZM=indsta;
001039150224       endif;
001040150224       // Dati destinatario
001041150224       // cerco 2 parte della ragione sociale
001042150224       clear rsd2;
001043150224       chain (tasaas: taslnp: tasnrs: tasnsp: 'D') tita430c;
001044150224 b1    if %found(tita430C);
001045150224          rsd2=ta4not ;
001046150224       endif;
001047150224       SDPRSD=tasrsd+rsd2;
001048150224       SDPIND=tasind;
001049150224       SDPCAD=tascad;
001050150224       SDPLOD=taslod;
001051150224       SDPPRD=tasprd;
001052150224       SDPNZD=tasnzd;
001053150224       // Colli e peso
001054150224       SDPNCL=tasncl;
001055150224       SDPPKG=wpeso;
001058150224       // Rif. spedizione
001059150224       sdprif=%subst(%editc(tasaas:'X'):3:2)+%editc(taslnp:'X')+
001060150224               %editc(tasnrs:'X')+%editc(tasnsp:'X')   ;
001061150224       // Numero Parcel dpd
001064171031       //clear dsbl4i;
001065171031       //Chain (tasaas: taslnp: tasnrs: tasnsp: wtrc) tita430c;
001066171031       //if  %found(tita430c);
001067171031       //    eval dsbl4i=ta4not;
001068171031       //endif;
001069171031       //SDPIPN=§b4ipn;
001070171031       clear pndipn;
001072171031       chain (tasaas: taslnp: tasnrs: tasnsp) fipnd01l;
001073171031       SDPIPN=pndipn;
001074150225       // RMN e RMA
001075150225       sdprmn=tasrmn;
001076150225
001077150225       clear dta4a ;
001078150225       Chain (tasaas: taslnp: tasnrs: tasnsp: 'A') tita430c;
001079150225       if  %found(tita430c);
001080150225           eval dta4a=ta4not;
001081150225       endif;
001082150225       sdprma=§TA4ARMA;
001083150225
001084150224       write wfsdp000;
001085150224       endsr;
001086141118       //-------------------------------------------------------------*
001087141118       //Routine di stampa                                            *
001088141118       //-------------------------------------------------------------*
001089141118       begsr sr_stampa ;
001090141118          open tnss91p;
001091141118       // inversione data dal/al
001092141118          dataeur=%date(SS90DSD:*iso);
001093141118          p1dal =%dec(dataeur);
001094141118          dataeur=%date(SS90DSA:*iso);
001095141118          p1al  =%dec(dataeur);
001096141118       // elenco delle filiali di lancio  con relativa decodifica
001097141118          for i=1 to %elem(sk_fil) ;
001098141119             if sk_fil(i)>0;
001099141119                clear orgdes;
001100141119                chain sk_fil(i) azorg01l;
001101141119                eval wtitolo2=%trim(wtitolo2) + '  ' +%editc(sk_fil(i):'X') +
001102141119                              '-' + %subst(orgdes:1:10);
001103141119             endif;
001104141118          endfor;
001105141118          eval wtitolo2=%trim(wtitolo2) ;
001106141118          n=%checkr(' ':wtitolo2);
001107141119          PosIn=(%len(ptitolo2)-n)/2 ;
001108141119          eval %subst(ptitolo2:posin)=%trim(wtitolo2);
001109141119       // Stampo testata
001110141119          write ss91t00;
001111141119          *in30=*off;
001112141119
001113141119          for i=1 to %elem(sk_fil) ;
001114141119           if sk_fil(i)>0;
001115141119             clear orgdes;
001116141119             chain sk_fil(i) azorg01l;
001117141119             pfil=sk_fil(i);
001118141119             pfild=orgdes;
001119141119             %occur(dstot)=i;
001120141119             p1speI=isp;
001121141119             p1nclI=icl;
001122141119             p1pesI=ips;
001123141119             p1speE=esp;
001124141119             p1nclE=ecl;
001125141119             p1pesE=eps;
001126141119       // Stampa riga
001127141119             if *in30;
001128141119                 write ss91t00;
001129141119                 *in30=*off;
001130141119             endif;
001131141119             write ss91r00;
001132141119           endif;
001133141119          endfor;
001134141119
001135141119       // Stampa "Fine Stampa"
001136141119          write fines;
001137141118       endsr ;
001138141114       //-------------------------------------------------------------*
001139141114       //Routine Iniziale                                             *
001140141114       //-------------------------------------------------------------*
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
002701150224       if %subst(knsif:7:1) = 'P';
002702150224         Wlibsede = Wlibsedepa;
002703150224         Else ;
002704150224         wlibsede = Wlibsedeba;
002705150224       endif;
002706150224       Open   titaa30c;
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
