000100940224     H*--------------------------------------------------------------*
000101170918     H DECEDIT('0,') DATEDIT(*YMD.)
000102171018     Ffipnd01l  if   E           k DISK    infds(infpnd)
000103171018     Ffipnd00f  uf   E             DISK    rename(fipnd000:fipndf)
000104171018     f                                     prefix(f_)
000105171017     Ftitas30c  iF   E           k DISK    extfile(wtitas) usropn
000106171017     Ffnblp01l  if   E           k DISK
000107171018     Ffnblt01l  if   E           k DISK
000200170919     Ffiar405l  iF   E           k DISK
000201171017     Fazorg01l  iF   E           k DISK
000202171017     FQsysprt   O    F  132        printer oflind(*inof)
000800060928
000801171018     D infpnd          DS
000802171018     D   pnd_rrn             397    400I 0
000803171017     D kpnd            DS
000804171018     D  pndAAS                14     17  0
000805171018     D  pndLNP                18     20  0
000806171018     D  pndNRS                21     22  0
000807171018     D  pndnSP                23     29  0
000808171017     D kar4            DS
000809171018     D  ar4AAS                14     17  0
000810171018     D  ar4LNP                18     20  0
000811171018     D  ar4NRS                21     22  0
000812171018     D  ar4nSP                23     29  0
000813171017     D og143         e DS
000814171019     d contdel         s              9  0 inz
000817171018     d wpnd            s              1    inz
000818171018     d sav_nsp         s                   like(kar4)
000819170919     d Wtitas          s             21    inz('GAITRAGRPS/TITAS30C')
000820170918
000900900131     C*---------------------------------------------------------------*
000901171017     c                   Open(e)   titas30c
000902171017     c                   if        not %open(titas30c)
000903171017     c                   eval      %subst(Wtitas:7:4)='GRU '
000904171017     c                   Open      titas30c
000905171017     c                   endif
000906171017       setll *loval fipnd01l ;
000907171017       read fipnd01l;
000908171017       dow not %eof(fipnd01l);
000909171018        if pnddim< 20171017;
000910171017          // verifico se spedizione esistente su TITAS
000911171017          setll (pndaas:pndlnp:pndnrs:pndnsp) titas30c;
000912171017             // TROVATO TITAS
000913171017          if %equal(titas30c);
000925171017          else;
000926171017             // NON TROVATO TITAS
000927171018             setll (pndaas:pndlnp:pndnrs:pndnsp) fnblp01l;
000928171018             if not %equal(fnblp01l);
000929171017                chain ('I':pndipn) fiar405l ;
000930171017                if not %found(fiar405l);
000938171017                else;
000939171019             // record da deletare
000940171019                  if pnddgs>0                 ;
000941171019                     except delpnd;
000942171019                     contdel+=1;
000943171019                     delete pnd_rrn fipnd00f ;
001022171018                  endif;
001023171018                endif;
001024171018             endif;
001025171018          endif;
001026171018         endif;
001028171018         read fipnd01l;
001029171017       enddo;
001030171018       except  cont;
001031170925
003400030205      *
003500000000     C                   SETON                                        LR
003600171017     o*--------------------------------------------------------------*
003700171017     o* Stampa
003800171017     o*-------------------------------------------------------------*
004500171019     Oqsysprt   E            delpnd      1
004600171017     o                       pndaas           +   2
004700171017     o                       pndlnp           +   2
004800171017     o                       pndnrs           +   2
004900171017     o                       pndnsp           +   2
004901171017     o                       pndipn           +   2
004902171019     o                       pnddgs           +   2
004903171019     o                       pnddim           +   2
005000171017     o                                        +   2 'Da deletare'
005800171018     O          E            cont        1
005801171019     o                                        +   1 'Tot sped. da cancellare'
005900171019     o                       contdel       z  +   1
