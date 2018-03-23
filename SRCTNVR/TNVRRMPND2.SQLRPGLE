000100940224     H*--------------------------------------------------------------*
000101170918     H DECEDIT('0,') DATEDIT(*YMD.)
000102171017     Ffipnd01l  if   E           k DISK
000103171017     Ftitas30c  iF   E           k DISK    extfile(wtitas) usropn
000104171017     Ffnblp01l  if   E           k DISK
000200170919     Ffiar405l  iF   E           k DISK
000201171017     Fazorg01l  iF   E           k DISK
000202171017     FQsysprt   O    F  132        printer oflind(*inof)
000800060928
000801171017     D kpnd            DS
000802171017     D  pndAAS                14     17  0
000803171017     D  pndLNP                18     20  0
000804171017     D  pndNRS                21     22  0
000805171017     D  pndnSP                23     29  0
000806171017     D kar4            DS
000807171017     D  ar4AAS                14     17  0
000808171017     D  ar4LNP                18     20  0
000809171017     D  ar4NRS                21     22  0
000810171017     D  ar4nSP                23     29  0
000811171017     D og143         e DS
000812170919     d Wtitas          s             21    inz('GAITRAGRPS/TITAS30C')
000813170918
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
000916171017                except aggdgs;
000917171017             endif;
000918171017          else;
000919171017             // NON TROVATO TITAS
000920171017                chain ('I':pndipn) fiar405l ;
000921171017                if not %found(fiar405l);
000922171017             //     record da deletare per ora lo stampo
000923171017                // stampo solo dal 2017
000924171017                 if pndaas>= 2017;
000925171017                    except delpnd  ;
000926171017                 endif ;
000927171017                else;
000928171017             // record da aggiornare se nsp<>
000929171017                 chain ar4lnp azorg01l ;
000930171017                 if %found(azorg01l);
000931171017                    og143=orgde3;
000932171017                   if §ogntw='DPD';
000933171017                      if pnddgs=0 and kpnd<>kar4 and pnddpe='DI4';
000934171017                          except aggnsp  ;
000935171017                      endif;
000937171017                   else;
000938171017                      reade ('I':pndipn) fiar405l;
000939171017                      dow not %eof(fiar405l);
000940171017                         chain ar4lnp azorg01l ;
000941171017                         if %found(azorg01l);
000942171017                             og143=orgde3;
000943171017                             if §ogntw='DPD';
000944171017                                if pnddgs=0 and kpnd<>kar4 and pnddpe='DI4';
000945171017                                    except aggnsp  ;
000946171017                                endif;
000947171017                             endif;
000948171017                         endif;
000949171017                         reade ('I':pndipn) fiar405l;
000950171017                      enddo;
000951171017                   endif;
000952171017                 endif;
000953171017                endif;
000954171017          endif;
000955171017          read fipnd01l;
000956171017       enddo;
000957170925
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
