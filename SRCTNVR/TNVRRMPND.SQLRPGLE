000100940224     H*--------------------------------------------------------------*
000101170918     H DECEDIT('0,') DATEDIT(*YMD.)
000200170919     Ffiar405l  iF   E           k DISK
000201170919     Ftita437c  iF   E           k DISK    extfile(wtita4) usropn
000202170919     Ftitas30c  iF   E           k DISK    extfile(wtitas) usropn
000300170925     Ffipnd01l  uf a E           k DISK    usropn
000301170919     Ffnblp01l  if   E           k DISK
000302170925     Fazorg01l  if   E           k DISK
000800060928
000801170918     d Wtita4          s             21    inz('GAITRAGRU /TITA437C')
000802170919     d Wtitas          s             21    inz('GAITRAGRPS/TITAS30C')
000803170918
000804170925     d og143         e ds
000805170918     d dsbl4i        e ds
000806170920     d dpnddp4       e ds                  prefix(DP4_)
000807170920     d dpnddi4       e ds                  prefix(DI4_)
000808170918     d dpndflo       e ds
000900900131     C*---------------------------------------------------------------*
000901170925         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
000902170925           exec sql
000903170925           DELETE
000904170925           FROM  fipnd00f;
000905170925     c                   open      FIPND01L
000916170925
000919170918     c* primo giro  : fiar4 --> fipnd
000920170918       setll ('I') fiar405l ;
000921170918       reade ('I') fiar405l ;
000922170918  1    dow    not %eof(fiar405l)  ;
000923170919          chain (ar4aas:ar4lnp:ar4nrs:ar4nsp) fipnd01l;
000924170918          if not %found(fipnd01l);
000925170918             clear fipnd000;
000926170919             dsbl4i=ar4not;
000927170920          // Sia import che export
000928170918             clear dpndflo;
000929170918             §PNDTRD     = §b4trd ;
000930170918             §PNDRECDIR  = §b4recdir;
000931170919             pndflo  = dpndflo;
000932170920
000933170919             PNDAAS  = ar4aas;
000934170919             PNDLNP  = ar4lnp;
000935170919             PNDNRS  = ar4nrs;
000936170919             PNDNSP  = ar4nsp;
000937170919             PNDIPN  = §b4ipn;
000938170920             PNDISC  = §b4isc;
000939170920             chain (ar4aas:ar4lnp:ar4nrs:ar4nsp) fnblp01l ;
000940170920             if %found(fnblp01l);
000941170920                pnddim  =  blpdim  ;
000943170920             // Solo per Import
000944170920            if (ar4lnp >=190 and ar4lnp <=197)  or
000945170920               (ar4lnp >=291 and ar4lnp <=294)  ;
000946170922             PNDBUCNP = 0  ;
000947170925             PNDIDP7  = *blanks    ;
000948170920             PNDBUCNA = 023;
000949170925             %subst(PNDIDA7:1:3) = '023'  ;
000953170925             %subst(PNDIDA7:4:4) = §b4ida;
000955170920             PNDDPE  = 'DI4';
000956170920             clear dpnddi4;
000957170920             DI4_§B4ICD  =  §B4ICD ;
000958170920             DI4_§B4IDS  =  §B4IDS ;
000959170920             DI4_§B4IBI  =  §B4IBI ;
000960170920             pndetiu     =  dpnddi4 ;
000961170920            else  ;
000962170920             // Solo per Export
000963170925             PNDDPE  = 'DP4';
000964170925             PNDBUCNP = 023    ;
000965170925             %subst(PNDIDP7:1:3) = '023'  ;
000966170925             chain ar4lnp azorg01l;
000967170925             if %found(azorg01l);
000968170925                og143 = orgde3;
000969170925                %subst(PNDIDP7:4:4) = §ogdp1;
000970170925             endif;
000978170920             DP4_§B4ICD  =  §B4ICD ;
000979170920             DP4_§B4IDA  =  §B4IDA ;
000980170920             DP4_§B4IDS  =  §B4IDS ;
000981170920             DP4_§B4IZA  =  §B4IZA ;
000982170920             DP4_§B4IBI  =  §B4IBI ;
000983170920             DP4_§B4IAT  =  §B4IAT ;
000984170920             DP4_§B4IPY  =  §B4IPY ;
000985170920             pndetiu     =  dpnddp4 ;
000986170920            endif;
000987170918             write fipnd000;
000988170925             endif;
000989170918          endif;
000990170918          reade ('I') fiar405l ;
000991170918       enddo;
000992170918     c* secondo giro: tita4 --> fipnd
003301170918     c                   Open(e)   tita437c
003302170918     c                   if        not %open(tita437c)
003303170918     c                   eval      %subst(Wtita4:7:4)='GRPS'
003304170918     c                   Open      tita437c
003305170918     c                   endif
003306170919     c                   Open(e)   titas30c
003307170919     c                   if        not %open(titas30c)
003308170919     c                   eval      %subst(Wtitas:7:4)='GRU '
003309170919     c                   Open      titas30c
003310170919     c                   endif
003311170919       setll *loval tita437c ;
003312170919       read tita437c ;
003313170918  1    dow    not %eof(tita437c)  ;
003314170919          chain (ta4aas:ta4lnp:ta4nrs:ta4nsp) fipnd01l;
003315170918          if not %found(fipnd01l);
003316170918             clear fipnd000;
003317170919             dsbl4i=ta4not;
003318170920          // Sia import che export
003319170918             clear dpndflo;
003320170918             §PNDTRD     = §b4trd ;
003321170918             §PNDRECDIR  = §b4recdir;
003322170919             pndflo  = dpndflo;
003323170920
003325170919             PNDAAS  = ta4aas;
003326170919             PNDLNP  = ta4lnp;
003327170919             PNDNRS  = ta4nrs;
003328170919             PNDNSP  = ta4nsp;
003329170919             PNDIPN  = §b4ipn;
003331170919             PNDISC  = §b4isc;
003332170920             pnddgs  = *date       ;
003333170920             chain (ta4aas:ta4lnp:ta4nrs:ta4nsp) titas30c ;
003334170920             if %found(titas30c);
003335170920                pnddim  =  tasdim  ;
003337170920             // Solo per Import
003338170920            if (ta4lnp >=190 and ta4lnp <=197)  or
003339170920               (ta4lnp >=291 and ta4lnp <=294)  ;
003340170922             PNDBUCNP = 0      ;
003341170922             PNDIDP7  = '       '  ;
003342170920             PNDBUCNA = 023;
003343170925             %subst(PNDIDA7:1:3) = '023'  ;
003347170925             %subst(PNDIDA7:4:4) = §b4ida;
003349170920             PNDDPE  = 'DI4';
003350170920             clear dpnddi4;
003351170920             DI4_§B4ICD  =  §B4ICD ;
003352170920             DI4_§B4IDS  =  §B4IDS ;
003353170920             DI4_§B4IBI  =  §B4IBI ;
003354170920             pndetiu     =  dpnddi4 ;
003355170920            else  ;
003356170920             // Solo per Export
003357170925             PNDDPE  = 'DP4';
003358170925             PNDBUCNP = 023    ;
003359170925             %subst(PNDIDP7:1:3) = '023'  ;
003360170925             chain ta4lnp azorg01l ;
003361170925             if %found(azorg01l);
003362170925                og143 = orgde3;
003363170925                %subst(PNDIDP7:4:4) = §ogdp1;
003364170925             endif;
003365170920             DP4_§B4ICD  =  §B4ICD ;
003366170920             DP4_§B4IDA  =  §B4IDA ;
003367170920             DP4_§B4IDS  =  §B4IDS ;
003368170920             DP4_§B4IZA  =  §B4IZA ;
003369170920             DP4_§B4IBI  =  §B4IBI ;
003370170920             DP4_§B4IAT  =  §B4IAT ;
003371170920             DP4_§B4IPY  =  §B4IPY ;
003372170920             pndetiu     =  dpnddp4 ;
003373170920            endif;
003374170918             write fipnd000;
003375170925             endif;
003376170918          endif;
003377170919          read tita437c ;
003378170918       enddo;
003400030205      *
003500000000     C                   SETON                                        LR
