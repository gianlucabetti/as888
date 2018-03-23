000100940224     H*--------------------------------------------------------------*
000101170918     H DECEDIT('0,') DATEDIT(*YMD.)
000102171018     Ffipnd01l  if   E           k DISK    infds(infpnd)
000103171018     Ffipnd00f  uf   E             DISK    rename(fipnd000:fipndf)
000104171017     Ftitas30c  iF   E           k DISK    extfile(wtitas) usropn
000105171017     Ffnblp01l  if   E           k DISK
000200170919     Ffiar405l  iF   E           k DISK
000201171017     Fazorg01l  iF   E           k DISK
000202171017     FQsysprt   O    F  132        printer oflind(*inof)
000800060928
000801171018     D infpnd          DS
000802171018     D   pnd_rrn             397    400I 0
000803171017     D kpnd            DS
000804171017     D  pndAAS                14     17  0
000805171017     D  pndLNP                18     20  0
000806171017     D  pndNRS                21     22  0
000807171017     D  pndnSP                23     29  0
000808171017     D kar4            DS
000809171017     D  ar4AAS                14     17  0
000810171017     D  ar4LNP                18     20  0
000811171017     D  ar4NRS                21     22  0
000812171017     D  ar4nSP                23     29  0
000813171017     D og143         e DS
000814171018     d contdg          s              9  0 inz
000815171018     d contnum         s              9  0 inz
000816171018     d wpnd            s              1    inz
000817170919     d Wtitas          s             21    inz('GAITRAGRPS/TITAS30C')
000818170918
000900900131     C*---------------------------------------------------------------*
000901171017     c                   Open(e)   titas30c
000902171017     c                   if        not %open(titas30c)
000903171017     c                   eval      %subst(Wtitas:7:4)='GRU '
000904171017     c                   Open      titas30c
000905171017     c                   endif
000906171017       setll *loval fipnd01l ;
000907171017       read fipnd01l;
000908171017       dow not %eof(fipnd01l);
000909171017          // verifico se spedizione esistente su TITAS
000910171017          setll (pndaas:pndlnp:pndnrs:pndnsp) titas30c;
000911171017             // TROVATO TITAS
000912171017          if %equal(titas30c);
000913171017          // Verifico se devo aggiornare la data dgs
000914171017             chain (pndaas:pndlnp:pndnrs:pndnsp) fnblp01l;
000915171017             if not %found(fnblp01l) and pnddgs=0  ;
000916171018                contdg+=1;
000917171017                except aggdgs;
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
000936171017                else;
000937171017             // record da aggiornare se nsp<>
000938171017             if pnddgs=0 and pnddpe='DI4';
000939171017                 chain ar4lnp azorg01l ;
000940171017                 if %found(azorg01l);
000941171017                    og143=orgde3;
000942171017                   if §ogntw='DPD';
000943171017                      if              kpnd<>kar4                 ;
000944171018     c                   eval       SQLCOD = 0
000945171018     c                   clear                   wpnd
000946171018     C/EXEC SQL
000947171018     C+ SELECT  'S' into :wpnd
000948171018     C+   FROM  fipnd00F   WHERE  pndaas = :ar4aas and pndlnp= :ar4lnp
000949171018     C+   and pndnrs= :ar4nrs and pndnsp= :ar4nsp
000950171018     C+    fetch first row only
000951171018     C/END-EXEC
000952171018                      if  wpnd  = ' ' ;
000953171018                          contnum+=1   ;
000954171017                          except aggnsp  ;
000955171018                      endif;
000956171017                      endif;
000957171017                   else;
000958171017                      reade ('I':pndipn) fiar405l;
000959171017                      dow not %eof(fiar405l);
000960171017                         chain ar4lnp azorg01l ;
000961171017                         if %found(azorg01l);
000962171017                             og143=orgde3;
000963171017                             if §ogntw='DPD';
000964171017                                if              kpnd<>kar4                 ;
000965171018     c                   eval       SQLCOD = 0
000966171018     c                   clear                   wpnd
000967171018     C/EXEC SQL
000968171018     C+ SELECT  'S' into :wpnd
000969171018     C+   FROM  fipnd00F   WHERE  pndaas = :ar4aas and pndlnp= :ar4lnp
000970171018     C+   and pndnrs= :ar4nrs and pndnsp= :ar4nsp
000971171018     C+    fetch first row only
000972171018     C/END-EXEC
000973171018                      if  wpnd  = ' ' ;
000974171018                                    contnum+=1   ;
000975171017                                    except aggnsp  ;
000976171018                      endif;
000977171017                                endif;
000978171017                             endif;
000979171017                         endif;
000980171017                         reade ('I':pndipn) fiar405l;
000981171017                      enddo;
000982171017                   endif;
000983171017                 endif;
000984171017                 endif;
000985171017                endif;
000986171018                endif;
000987171017          endif;
000988171017          read fipnd01l;
000989171017       enddo;
000990171018       except  cont;
000991170925
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
005800171018     O          E            cont        1
005801171018     o                                        +   1 'Tot sped. con n.ro da agg.'
005900171018     o                       contnum       z  +   1
005901171018     o                                        +   2 'Tot sped. con data da agg.'
006000171018     o                       contdg        z  +   1
