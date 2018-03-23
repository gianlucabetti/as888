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
000814171018     d contdg          s              9  0 inz
000815171018     d contnum         s              9  0 inz
000816171018     d contimp         s              9  0 inz
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
000914171017          // Verifico se devo aggiornare la data dgs
000915171017             chain (pndaas:pndlnp:pndnrs:pndnsp) fnblp01l;
000916171017             if not %found(fnblp01l) and pnddgs=0  ;
000917171018                contdg+=1;
000918171017                except aggdgs;
000919171018          //    chain pnd_rrn fipnd00f ;
000920171018          //    if %found(fipnd00f)   ;
000921171018          //    pnddgs=*date ;
000922171018          //       update fipndf  ;
000923171018          //    endif ;
000924171017             endif;
000925171017          else;
000926171017             // NON TROVATO TITAS
000927171018             setll (pndaas:pndlnp:pndnrs:pndnsp) fnblp01l;
000928171018             if not %equal(fnblp01l);
000929171017                chain ('I':pndipn) fiar405l ;
000930171017                if not %found(fiar405l);
000931171017             //     record da deletare per ora lo stampo
000932171017                // stampo solo dal 2017
000933171017                 if pndaas>= 2017;
000934171017                    except delpnd  ;
000935171017                 endif ;
000936171018                 delete pnd_rrn fipnd00f ;
000938171017                else;
000939171017             // record da aggiornare se nsp<>
000940171018                  if pnddgs=0 and pnddpe='DI4';
000941171018
000942171018             // conto quante spedizioni import dpd ci sono su ar4 per il nr.parcel
000943171018             // memorizzando la key spedizione della prima import che trovo se
000944171018             // se dversa dalla key di pnd e la cui fls <>ar4lnp
000945171018
000946171018                       clear contimp;
000947171018                       clear sav_nsp;
000948171018                       setll ('I':pndipn) fiar405l ;
000949171018                       reade ('I':pndipn) fiar405l ;
000950171018                       dow not %eof(fiar405l);
000951171018                          chain ar4lnp azorg01l ;
000952171018                          if %found(azorg01l);
000953171018                             og143=orgde3;
000954171018                             if §ogntw='DPD';
000955171018                                 if kar4<>kpnd ;
000956171018                                   chain (ar4aas:ar4lnp:ar4nrs:ar4nsp) fnblt01l;
000957171018                                   if %found(fnblt01l);
000958171018                                      if bltfls<>ar4lnp and bltfls=pndlnp;
000959171018                                         eval sav_nsp=kar4 ;
000960171018                                      endif;
000961171018                                   endif;
000962171018                                 endif;
000963171018                                contimp+=1;
000964171018                             endif;
000965171018                          endif;
000966171018                          reade ('I':pndipn) fiar405l ;
000967171018                       enddo;
000968171018
000969171018
000970171018           // se trovato + di un fiar4 import o non trovato nessuno -->  stampo
000971171018           // stampo anche se non trovato fiar4 valido
000972171018                       if contimp<>1 or  sav_nsp=*blanks;
000973171018                           except errore;
000974171018                       else;
000975171018           // se trovato
000977171018                           kar4=sav_nsp;
000983171018     c                   eval       SQLCOD = 0
000984171018     c                   clear                   wpnd
000985171018     C/EXEC SQL
000986171018     C+ SELECT  'S' into :wpnd
000987171018     C+   FROM  fipnd00F   WHERE  pndaas = :ar4aas and pndlnp= :ar4lnp
000988171018     C+   and pndnrs= :ar4nrs and pndnsp= :ar4nsp
000989171018     C+    fetch first row only
000990171018     C/END-EXEC
000991171018                         if  wpnd  = ' ' ;
000992171018                            contnum+=1   ;
000993171018                            except aggnsp  ;
000994171018                            chain pnd_rrn fipnd00f ;
000995171018                            if %found(fipnd00f)   ;
000996171018                              f_pndaas=ar4aas;
000997171018                              f_pndlnp=ar4lnp;
000998171018                              f_pndnrs=ar4nrs;
000999171018                              f_pndnsp=ar4nsp;
001000171018                              update fipndf  ;
001001171018                            endif ;
001002171018                         endif;
001021171018                       endif;
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
003900171017     OQsysPrt   E            aggdgs      1
004000171017     o                       pndaas           +   2
004100171017     o                       pndlnp           +   2
004200171017     o                       pndnrs           +   2
004300171017     o                       pndnsp           +   2
004400171017     o                                        +   2 'Agg.Data'
004500171017     O          E            delpnd      1
004600171017     o                       pndaas           +   2
004700171017     o                       pndlnp           +   2
004800171017     o                       pndnrs           +   2
004900171017     o                       pndnsp           +   2
004901171017     o                       pndipn           +   2
005000171017     o                                        +   2 'Da deletare'
005100171017     O          E            aggnsp      1
005200171017     o                       pndaas           +   2
005300171017     o                       pndlnp           +   2
005400171017     o                       pndnrs           +   2
005500171017     o                       pndnsp           +   2
005600171017     o                       pndipn           +   2
005700171017     o                                        +   2 'Da aggiornare con:'
005701171017     o                       ar4aas           +   2
005702171017     o                       ar4lnp           +   2
005703171017     o                       ar4nrs           +   2
005704171017     o                       ar4nsp           +   2
005705171018     O          E            errore      1
005706171018     o                       pndaas           +   2
005707171018     o                       pndlnp           +   2
005708171018     o                       pndnrs           +   2
005709171018     o                       pndnsp           +   2
005710171018     o                       pndipn           +   2
005711171018     o                                        +   2 'Verificare'
005800171018     O          E            cont        1
005801171018     o                                        +   1 'Tot sped. con n.ro da agg.'
005900171018     o                       contnum       z  +   1
005901171018     o                                        +   2 'Tot sped. con data da agg.'
006000171018     o                       contdg        z  +   1
