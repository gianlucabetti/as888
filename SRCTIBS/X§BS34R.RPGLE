000002180220      * Ds di riferimento al file esterno AzUte00F
000003180220     D AzuteDs       E DS                  ExtName(AzUte00F)
000005180301     d dDatiute      e ds
000006180301     d ddutxxx       e ds
000007180301     D
000008180301     D tibs34ds      E ds
000009180301     D tibs34ds1     E ds
000010180301     D x§bs34ds1     e ds
000011180301     D x§bs34ds2     e ds                  prefix(ds2_)
000012180301     d dataiso_cor     s               d   datfmt(*iso)
000013180220     d
000014180220      * ricavo profilo del JOB
000015180220     DINFDS           SDS
000016180220     D USER                  254    263
000017180220
000018180226      *************************************************************************************
000019180306      *  Il pgm restituisce 2 parametri, IL SECONDO FACOLTATIVO:
000020180301      *
000021180306      *  A)Nel primo parametro (x§bs34ds1):
000022180226      *  - Capoconto clienti
000023180226      *  - Ragione sociale generica "Brt S.p.A"
000024180301      *  - Ragione Sociale generica + codice filiale
000025180226      *  - Data blocco fatturazione
000026180301      *
000027180306      *  B)Nel secondo parametro (x§bs34ds2):
000028180301      *  - SIMFEL
000029180301      *  - SIMPOU
000030180301      *  - SIMTPP
000031180301      *
000032180306      * ATTENZIONE:
000033180301      * Il reperimento di tali dati viene effettuato in base all'utente del job. Per necessità
000034180301      * diverse bisogna richiamare il TIBS34R
000035180226      *
000036180226      **********************************************************************************************
000037180226     C* Record di AZUTE in OUTPUT a richiesta: restituisce i dati dell'utente del JOB
000100180219     c     *entry        plist
000200180301     C                   parm                    x§bs34ds1
000202180301     C                   parm                    x§bs34ds2
000203180301         clear x§bs34ds1;
000204180301         if %parms>1;
000205180301            clear x§bs34ds2;
000206180301         endif;
000207180301      * Se già caricata l'area dati utente la utilizzo
000208180301     c     *dtaara       define    §azute        azuteds
000209180301     c     *dtaara       define    §datiute      ddatiute
000210180301     c                   in(E)     *dtaara
000211180301    1c                   If        not %error  and RSUT<> *blanks and
000212180301     c                             dutute=user
000213180301     c                   exsr      sr_impo
000234180301     c* data blocco fatturazione da ricalcolare se data dutdte <> data sistema
000235180301     c                   if        dutdte <>  %dec(dataiso_cor)
000236180301     c                   clear                   tibs34ds1
000237180301     c                   call      'TIBS34R1'
000238180301     c                   parm                    TIBS34DS1
000239180301     c                   eval      O§34BLKF = O341BLKF
000240180301     c                   ENDIF
000241180301     c                   else
000242180306     c* in mancanza dell'area dati utente o se presente relativa ad utente <> dall'utente
000243180306     c* del job ??????????????  non è un problema se richiama tibs34r se area dati presente????
000244180301     c                   if        %parms<2
000245180301     c                   clear                   tibs34ds1
000246180301     c                   call      'TIBS34R1'
000247180301     c                   parm                    tibs34ds1
000248180301     C                   eval      O§34BLKF=O341BLKF
000249180301     c                   eval      O§raguts=ORAGUTS
000250180301     c                   eval      O§RAGUT =Oragut
000251180301     c                   eval      o§kci   =Okci
000252180301     c                   else
000253180301     c                   clear                   tibs34ds
000254180301     c                   call      'TIBS34R'
000255180301     c                   parm                    tibs34ds
000256180301     c                   in        *dtaara
000257180301     c                   exsr      sr_impo
000264180301     c                   ENDIF
000265180301    1c                   EndIf
000266180301
000300180219         *inlr = *on;
000301180301     c***********************************************************************************
000302180301     c     sr_impo       begsr
000303180301     c                   eval      ddutxxx=dutxxx
000304180301     c                   eval      O§34BLKF=§DUTBLKF
000305180301     c                   eval      O§RAGUTS=rsutold
000306180301     c                   eval      O§RAGUT =rsut
000307180301     c                   eval      o§kci   =dutkci
000308180301     c                   if        %parms>1
000309180301     c                   eval      ds2_simfel=simfel
000310180301     c                   eval      ds2_simpou=dutpou
000311180301     c                   eval      ds2_simtpp=dutlpo
000312180301     c                   ENDIF
000313180301     c                   endsr
