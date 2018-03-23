000001180220     FAzUte01L  IF   E           K DISK    usropn
000002180220      * Ds di riferimento al file esterno AzUte00F
000003180301     D AzuteDs       E DS                  ExtName(AzUte00F)
000005180228     D tibs34ds1     E ds
000006180220     d
000007180301     d azuteds_p       s                   like(AzuteDs)
000008180220     d dataiso_cor     s               d   datfmt(*iso)
000009180219     d dataiso_ini     s               d   datfmt(*iso)
000010180222     d w_fil           s                   like(utefil)
000011180220      * ricavo profilo del JOB
000012180220     DINFDS           SDS
000013180220     D USER                  254    263
000014180220
000015180226      *************************************************************************************
000016180307      *  INPUT: p.o. utente, facoltativo
000017180307      *
000018180307      *  OUTPUT:
000020180226      *  - Capoconto clienti
000021180226      *  - Ragione sociale generica "Brt S.p.A"
000022180226      *  - Ragione Sociale generica + codice filiale: quello ricevuto in input o in mancanza
000023180226      *                      la filiale utente del job
000024180226      *  - Data blocco fatturazione
000025180226      *
000026180227      *  A richiesta restituisce anche il record di AZUTE dell'utente del JOB
000027180307      *
000028180227      *  Se eventuale POU ricevuto in input non coincide con l'utente del job azuteds
000029180227      *  viene restituito vuoto
000030180226      **********************************************************************************************
000031180226     C* Record di AZUTE in OUTPUT a richiesta: restituisce i dati dell'utente del JOB
000100180219     c     *entry        plist
000200180228     C                   parm                    tibs34ds1
000202180301     C                   parm                    AzuteDs_p
000203180220         clear Oragut;
000204180220         clear Oraguts;
000205180220         clear Okci;
000206180228         clear O341blkf;
000207180301         if %parms>1;
000208180301            clear AzuteDs_p ;
000209180301         endif;
000210180301         clear Azuteds  ;
000211180220
000212180220         Okci = 0151 ;
000213180223
000214180220         Oraguts = 'BRT S.p.A.' ;
000215180223         Oragut  = 'BRT S.p.A.' ;
000216180223      // Aggancio azute solo se non ricevuto il POU oppure se richiesta la restituzione
000217180223      // del record di AZUTE
000218180228         if i341pou = 0 or %parms>1;
000219180223           open azute01l;
000220180220           chain USER azute01l;
000221180220         ENDIF;
000222180223
000223180228         if i341pou>0;
000224180228           w_fil = i341pou;
000225180223         else;
000226180223           clear W_fil;
000227180223          if %found(azute01l);
000228180223           w_fil = utefil;
000229180223          endif;
000230180220         ENDIF;
000231180227         // restituisco vuota ds di azute se ricevuto un p.o utente diverso dall'utente del
000232180227         // job altrimenti non ci sarebbe coerenza fra il pou della ragione sociale e
000233180227         // e il record di azute reperito.
000234180227         if w_fil <> utefil;
000235180227           clear AzuteDs;
000236180227         ENDIF;
000237180223
000238180223         if w_fil > 0;
000239180223            Oragut = %trim(Oraguts) + ' (' +
000240180223                    %editc(w_fil:'X') + ')';
000241180223         endif;
000242180220
000243180223         // Calcolo la data blocco fatturazione
000244180223
000245180219         // Reperisco la data corrente
000246180219            dataiso_cor=(%date());
000247180220            dataiso_ini=dataiso_cor;
000248180223
000249180219         if  %subdt(dataiso_cor:*days) >=2  ;
000250180220         // vado alla fine del mese precedente
000251180220           dataiso_ini = dataiso_ini-%days(%subdt(dataiso_cor:*days));
000252180219         else;
000253180220         // tolgo un mese
000254180223           dataiso_cor = dataiso_cor-%months(1);
000255180220         // vado alla fine del mese precedente
000256180220           dataiso_cor = dataiso_cor-%days(%subdt(dataiso_cor:*days));
000257180219         endif;
000258180228         O341BLKF = %dec(dataiso_ini);
000259180301         if %parms>1;
000260180301            azuteds_p=azuteds;
000261180301         endif;
000262180222
000263180222         if %open(azute01l);
000264180222            close azute01l ;
000265180222         endif;
000300180219         *inlr = *on;
