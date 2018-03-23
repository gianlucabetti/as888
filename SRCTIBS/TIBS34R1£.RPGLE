000001180220     FAzUte01L  IF   E           K DISK    usropn
000002180220      * Ds di riferimento al file esterno AzUte00F
000003180220     D AzuteDs       E DS                  ExtName(AzUte00F)
000004180223     D x§bs34ds      e ds
000005180220     d
000006180220     d dataiso_cor     s               d   datfmt(*iso)
000007180219     d dataiso_ini     s               d   datfmt(*iso)
000008180222     d w_fil           s                   like(utefil)
000009180220      * ricavo profilo del JOB
000010180220     DINFDS           SDS
000011180220     D USER                  254    263
000012180220
000013180226      *************************************************************************************
000014180226      *  Il pgm restituisce:
000015180226      *
000016180226      *  - Capoconto clienti
000017180226      *  - Ragione sociale generica "Brt S.p.A"
000018180226      *  - Ragione Sociale generica + codice filiale: quello ricevuto in input o in mancanza
000019180226      *                      la filiale utente del job
000020180226      *  - Data blocco fatturazione
000021180226      *
000022180227      *  A richiesta restituisce anche il record di AZUTE dell'utente del JOB
000023180227      *  Se eventuale POU ricevuto in input non coincide con l'utente del job azuteds
000024180227      *  viene restituito vuoto
000025180226      **********************************************************************************************
000026180226     C* Record di AZUTE in OUTPUT a richiesta: restituisce i dati dell'utente del JOB
000100180219     c     *entry        plist
000200180220     C                   parm                    x§bs34ds
000202180220     C                   parm                    AzuteDs
000203180220         clear Oragut;
000204180220         clear Oraguts;
000205180220         clear Okci;
000206180220         clear O§34blkf;
000207180222         clear AzuteDs ;
000208180220
000209180220         Okci = 0151 ;
000210180223
000211180220         Oraguts = 'BRT S.p.A.' ;
000212180223         Oragut  = 'BRT S.p.A.' ;
000213180223      // Aggancio azute solo se non ricevuto il POU oppure se richiesta la restituzione
000214180223      // del record di AZUTE
000215180223         if i§34pou = 0 or %parms>1;
000216180223           open azute01l;
000217180220           chain USER azute01l;
000218180220         ENDIF;
000219180223
000220180220         if i§34pou>0;
000221180220           w_fil = i§34pou;
000222180223         else;
000223180223           clear W_fil;
000224180223          if %found(azute01l);
000225180223           w_fil = utefil;
000226180223          endif;
000227180220         ENDIF;
000228180227         // restituisco vuota ds di azute se ricevuto un p.o utente diverso dall'utente del
000229180227         // job altrimenti non ci sarebbe coerenza fra il pou della ragione sociale e
000230180227         // e il record di azute reperito.
000232180227         if w_fil <> utefil;
000233180227           clear AzuteDs;
000234180227         ENDIF;
000235180223
000236180223         if w_fil > 0;
000237180223            Oragut = %trim(Oraguts) + ' (' +
000238180223                    %editc(w_fil:'X') + ')';
000239180223         endif;
000240180220
000241180223         // Calcolo la data blocco fatturazione
000242180223
000243180219         // Reperisco la data corrente
000244180219            dataiso_cor=(%date());
000245180220            dataiso_ini=dataiso_cor;
000246180223
000247180219         if  %subdt(dataiso_cor:*days) >=2  ;
000248180220         // vado alla fine del mese precedente
000249180220           dataiso_ini = dataiso_ini-%days(%subdt(dataiso_cor:*days));
000250180219         else;
000251180220         // tolgo un mese
000252180223           dataiso_cor = dataiso_cor-%months(1);
000253180220         // vado alla fine del mese precedente
000254180220           dataiso_cor = dataiso_cor-%days(%subdt(dataiso_cor:*days));
000255180219         endif;
000256180223         O§34BLKF = %dec(dataiso_ini);
000257180222
000258180222         if %open(azute01l);
000259180222            close azute01l ;
000260180222         endif;
000300180219         *inlr = *on;
