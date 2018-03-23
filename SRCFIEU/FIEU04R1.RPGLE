000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060615      **?-------------------------------------------------------------------*****
000500060613      *  Da UPLOAD                                                              *
000600161130      *  TRASCODIFICA : GEODATA  TTEVENTS  in FIEU18DS RIGA x RIGA              *
000700161018      *               STATI EXPORT BARTOLINI a DPD                              *
000800060615      **?-------------------------------------------------------------------*****
000900161020      *
001000161020     FEDivab2L  Uf   E           K DISK
001100161020     FEDiVAT3L  Uf   E           K DISK
001200161020     FEDivat4L  iF   E           K DISK    rename(EDIVAT00:EDIVAT4)
001300161020      *
001400161020     FFIVAT00F  uF   E             DISK    rename(FIVAT000:FIVATFIS)
001500161020     FFivat02L  if   E           K DISK    infds(vatds)
001600161020     FFivab01L  uf   E           K DISK
001700170907      *
001800161020     FWFIEU00F  uf a E           k DISK
001900160121      *
002000160121     FAZorg01L  if   E           K DISK
002100171003     Ffipnd03l  IF   E           K DISK
002200180209     Ftidp401l  IF   E           K DISK
002300171212     FTABEL00F  IF   E           K DISK
002400171003      ** ------------------------------------------------------------------ */
002500161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
002600161124     D FIEU04DS40    E DS                  prefix(TTE_)
002700171003      ** ------------------------------------------------------------------ */
002800171003      *
002900171212      *-------------------
003000171212     D DS3IDP        E DS
003100171212      *-------------------
003200161020     d vatds           ds
003300161020     d  vat_nrr              397    400b 0
003400171003      ** -------------------------------------------------------------------*****
003500000223     D W0140           S             14  0
003600991129     D WORA            S              6  0
003700991129     D WDTGIO          S              8  0
003800991129     D DATEU           S              8  0
003900180212     D SDATE           S              8  0
004000180212     D SORA            S              6  0
004100991129     D DATA_eur        S               D   DATFMT(*eur)
004200160120     D DATA_iso        S               D   DATFMT(*iso)
004300161020     d dVATe         e ds
004400160120      *-------------------
004500160121     D OG143         E DS
004600160121      *-------------------
004700171003     D prmCPD10F     e DS                  extname(DPCDP10F)
004800171003     D prmLEG10F     e DS                  extname(DPLEG10F)
004900171003      *----------
005000171003     D DlegFLO       E DS
005100171003     D TISIEDEPDS    E DS
005200171003      *-------------------
005300160120     d Wdata8          DS
005400160120     d  dadata                 1      8  0
005500160120     d  adata                  9     16  0
005600160120     d  GioLav                17     21  0
005700160120      *
005800160120     D DepotParcel     S              4
005900171003      *  ______________________________________________________________     */
006000161020     D  se_errore      s              1
006100161020     D  Msg_Vin_80     s             80
006200060619     D Testa_Sk        s             10    DIM(50)
006300171003      *  ______________________________________________________________     */
006400161020     D   Digits        C                   '0123456789'
006500161020     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
006600161020     d Gomme_365       c                   365
006700161020     d Gomme_366       c                   366
006800161020     d Gomme_367       c                   367
006900171003      *  ______________________________________________________________     */
007000060612     c                   SETON                                        LR
007100171212      *-----
007200171212      **  in funzione degli SCAN 5 portati sul TTEVENTS vengono rimandati a noi
007300171212      **  tutti gli scan che NOI abbiamo inviato a DPD. Per questo occorre filtrare
007400171212      **  tutti gli scan generati da NOI, quindi con B.UNIT = "023" e devono essere
007500171212      **  scartati e non Elaborati.
007600171212     c                   if        %len(%trim(TTE_DEPOT))= 7  and
007700171212     c                             %Subst(TTE_DEPOT:1:3) = BU_brt
007800180116      * Errore su VINMSG   impostando "I" il FIEU04R ignora la scrittura sul TIDP400F
007900180116     C                   eval      se_errore   = 'I'
008000171212     C                   eval      Msg_Vin_80 = 'SCAN da ignorare perché -
008100180212     C                             generato da BRT;DEPOT:' + %trim(TTE_DEPOT) +
008200180212     C                             ' SCAN:' + %trim(TTE_SCANCODE)
008300171212     c                   RETURN
008400171212     c                   endif
008500180112      ** -----------
008600180112      *  Controlla che non sia un parcel di IMPORT di cui deve prendere solo
008700180112      *   lo SCAN 05:
008800180112      *  se lo trova e non è un reso e la B.U. di arrivo siamo noi è un IMPORT
008900180112      *   e lo SCAN non è 05, deve ignorarlo.
009000180112      *  oppure x NON trovato.
009100180116     c                   movel     TTE_SCANCODE  SCAN_2            2
009200180112     c     tte_parcelNR  chain     fipnd03l
009300180116     c                   if        ( %Found(fipnd03l) and
009400180112     c                              %subst(PNDDPE:1:2)= 'DI' and
009500180112     c                              PNDISC <>'300' and
009600180116     c                              SCAN_2 <>'05' )
009700180112     c                             or
009800180116     c                             ( not %Found(fipnd03l) and
009900180116     c                              SCAN_2 <>'05' )
010000180116      * Errore su VINMSG   impostando "I" il FIEU04R ignora la scrittura sul TIDP400F
010100180116     C                   eval      se_errore   = 'I'
010200180115     C                   eval      Msg_Vin_80 = 'NON Trovato FIPND x SCAN-
010300180212     C                             :' + %trim(TTE_SCANCODE) +
010400180212     C                             ' di PARCEL:' + %trim(TTE_PARCELNR)
010500180112     c                   RETURN
010600180112     c                   end
010700180209      **
010800180209     c     tte_parcelNR  setll     tidp401l
010900180209     c     tte_parcelNR  reade     tidp401l
011000180209     c                   dow       not %Eof(tidp401l)
011100180212      *
011200180212     c                   clear                   dp4_weight        8 2
011300180212     c                   eval      dp4_weight = tte_weight/100
011400180212      *
011500180209      * se il record è già arrivato e DPD lo sta rimandando UGUALE deve SALTARLO
011600180209     c                   IF        TTE_SERVICECOD  = DP4SERVICE  and
011700180209     c                             TTE_ASCODE      = DP4ASCODE   and
011800180209     c                             TTE_DZIPCODE    = DP4DZIPCOD  and
011900180209     c                             TTE_DCOUNTRYCD  = DP4DCOUNTR  and
012000180209     c                             TTE_SCANCODE    = DP4SCAN     and
012100180209     c                             TTE_REASONCOD1  = DP4ADDCOD1  and
012200180209     c                             TTE_REASONCOD2  = DP4ADDCOD2  and
012300180209     c                             TTE_REASONCOD3  = DP4ADDCOD3  and
012400180209     c                             TTE_DEPOT       = DP4DEPCOD   and
012500180209     c                             TTE_STDATETIME  = DP4DATTIM   and
012600180209     c                             TTE_DDEPOT      = DP4DELDEP   and
012700180209     c                             TTE_DELIVTOUR   = DP4TOUR     and
012800180212     c                             DP4_WEIGHT      = DP4WEIGHT   and
012900180209     c                             TTE_PARCELREF   = DP4PARREF   and
013000180209     c                             TTE_PARCELREF2  = DP4PARREF2  and
013100180209     c                             TTE_PODIMAGREF  = DP4PODIMAG  and
013200180209     c                             TTE_RNAME       = DP4RECNAME  and
013300180209     c                             TTE_INFOCONT1   = DP4INFO01   and
013400180209     c                             TTE_INFOCONT2   = DP4INFO02
013500180212     C                   movel     TTE_STDATETIMESDATE
013600180212     C                   move      TTE_STDATETIMESORA
013700180209      **
013800180209      * Errore su VINMSG   impostando "I" il FIEU04R ignora la scrittura sul TIDP400F
013900180209     C                   eval      se_errore   = 'I'
014000180212     C                   eval      Msg_Vin_80 = 'SCAN ' +  %trim(TTE_SCANCODE) +
014100180212     C                             ' arrivato precedentemente: '+
014200180212     C                             %trim(TTE_PARCELNR) +' '+
014300180212     c                             %editw(SDATE:'    /  /  ') + ' ' +
014400180212     c                             %editw(SORA:'  :  :  ')
014500180209     c                   RETURN
014600180209     c                   end
014700180209      *
014800180209     c     tte_parcelNR  reade     tidp401l
014900180209     c                   enddo
015000180209      *
015100161020      ** ===========
015200060612      ** Decodifica record a campi variabili
015300060612     c                   exsr      Decod_Record
015400160412      ** ===========
015500171212      **   x SCAN 18 PUDO  solo su nostro EXPORT  sotto tipologia 128/129 PUDO
015600171212      *   trasforma lo SCAN RICEVUTO in un ALTRO SCAN da passare poi al FIEU18R
015700171212      *   per essere elaborato (utilizza la tabella "SCS" per sostituire lo SCAN
015800171212      *   con un altro codificato sulla "DIR")
015900161020     c                   if           i5SCANT  = 18
016000160412     c                   IF        (%subst(i5PARCEL:1:4) = '0844'  or
016100160412     c                              %subst(i5PARCEL:1:4) = '0845')     and
016200160412     c                             (%subst(i5INFOTXT:1:4) = '128:' or
016300170908     c                              %subst(i5INFOTXT:1:4) = '129:' or
016400170908     c                              %subst(i5INFOTXT:2:4) = '128:' or
016500170908     c                              %subst(i5INFOTXT:2:4) = '129:')
016600160412      *   trasforma lo SCAN RICEVUTO in un ALTRO SCAN per descrivere il
016700160412     c                   exsr      SCAN_18_PUDO
016800161020      **
016900161020     c                   if        se_errore ='S'
017000161020     C                   eval       Msg_Vin_80 = 'Errori conv.SCAN 18 PUDO'
017100161020     c                   RETURN
017200161020     c                   endif
017300160412      **
017400160412     c                   END
017500161020     c                   endif
017600160120      ** ===========
017700160120      *** se arrivato uno  SCAN 05 si attiva la spedizione sul GEODATA
017800160120      **   in sospensione sul EDIVAB e se non lo trova lo scrive o aggiorna
017900160120      ** e NON è la ripetizione del nostro scan x quando facciamo l'export.
018000160120      **   ossia non è un 0844/0845 Italia
018100161020     c                   IF        i5SCANT  = 05
018200160120     c                               and
018300160120     c                              %subst(i5PARCEL:1:4) <> '0844'
018400160120     c                               and
018500160120     c                              %subst(i5PARCEL:1:4) <> '0845'
018600160120      ** ===========
018700160120     c                   exsr      SCAN_05
018800161021      **
018900161021     c                   if        se_errore ='S'
019000161021     C                   eval       Msg_Vin_80 = 'Errori conv.SCAN 05'
019100161021     c                   RETURN
019200161021     c                   endif
019300160120      ** ===========
019400060612     c                   else
019500060612
019600060612      *  con il record in canna scrive direttamente il VAB ed il VAT
019700161021     c                   exsr      callFIEU18
019800160121     c                   end
019900160121
020000161021      *  Problemi nella decodifica dei campi
020100161021     c                   if        se_errore<>'S'
020200060614      *  Record OK
020300160126      *  x gli SCAN 05 deve riportare quanto segnalato dalla routine SCAN_05
020400160126     c                   if        i5SCANT <> 05
020500160126     c                                or
020600160126     c                             (i5SCANT  = 05 and
020700160126     c                             (%subst(i5PARCEL:1:4) = '0844'
020800160126     c                               or
020900160126     c                              %subst(i5PARCEL:1:4) = '0845') )
021000161020     C                   eval       Msg_Vin_80 = *Blank
021100160126     c                   end
021200060614     c                   end
021300060612      **
021400161020     c                   RETURN
021500161020      * ?______________________________________________________________     */
021600161020      *?    Decodifica record a campi variabili
021700161020      * ?______________________________________________________________     */
021800161020     c     Decod_Record  Begsr
021900161020
022000161130      * ? Sposta dal tracciato TTE al vecchio FIEU18DS poichè al momento
022100161020      * ?  non vi è nulla di nuovo da aggiungere
022200161130     c                   clear                   FIEU18
022300170907     c                   clear                   i5DEPCODE7        7
022400180117     c                   clear                   i5DDEPOT7         7
022500161020     c                   eval      I5PARCEL    = TTE_PARCELNR
022600170317      **
022700170317      ** nuovo depot può essere  lungo 7 anzichè 4 ei vecchio Depot di 4 sta a dx.
022800170317     c                   if        %len(TTE_DEPOT) = 7
022900171129     c                   movel     TTE_DEPOT     depot7            7
023000171129     c                   move      depot7        depot4            4
023100170317     c                   movel     depot4        i5DEPCODE
023200171129     c                   movel     depot7        i5DEPNAME
023300180117     c                   move      depot7        i5DEPCODE7
023400170317     c                   elseif    %len(TTE_DEPOT) = 4
023500161020     c                   eval      I5DEPCODE   = TTE_DEPOT
023600180117     c                   move      *all'0'       i5DEPCODE7
023700180117     c                   move      I5DEPCODE     i5DEPCODE7
023800170317     c                   end
023900170907      *
024000180117      *   Depot di SCAN e Depot di CONSEGNA
024100180117     c                   if        %len(TTE_DDEPOT)= 7
024200180117     c                   move      TTE_DDEPOT    i5DDEPOT7
024300180117     c                   elseif    %len(TTE_DDEPOT)= 4
024400180117     c                   movel     TTE_DDEPOT    depot4
024500180117     c                   move      *all'0'       i5DDEPOT7
024600180117     c                   move      depot4        i5DDEPOT7
024700180117     c                   end
024800180117      **
024900180117      * mette in fila i 2 DEPOT
025000180117     c                   movel     i5DEPCODE7    DEPOT2_14        14
025100180117     c                   move      i5DDEPOT7     DEPOT2_14
025200180117     c                   movel     DEPOT2_14     i5DEPNAME
025300180117      **  i 2 DEPOT li imposta nema i5DEPNAME  depot di scansione e depot di consegna
025400180117      **    7 + 7 = 14 bytes
025500180117      **
025600161130     c                   movel     *zeros        I5DATTIM
025700161130     c                   movel     TTE_STDATETIMEI5DATTIM
025800161020     c                   eval      I5SERVICE   = TTE_SERVICECOD
025900161020     c                   eval      I5RECNAME   = TTE_RNAME
026000161020     c                   eval      I5INFOTXT   =
026100161020     c                             %editc(TTE_INFOCONT1:'X') + TTE_INFOCONT2
026200161020      **
026300161020      **  essendo in decagrammi ha quindi 2 decimali quindi basta allineare a destra
026400161020     c                   move      TTE_WEIGHT    I5WEIGHT
026500161020      **
026600161020     c                   movel     TTE_SCANCODE  I5SCANT
026700161020     c                   movel     TTE_DZIPCODE  I5CONSZIP
026800161020     c                   movel     TTE_REASONCOD1I5ADDSER1
026900161020     c                   movel     TTE_REASONCOD2I5ADDSER2
027000161020     c                   movel     TTE_REASONCOD3I5ADDSER3
027100171129      **  solo se pieno
027200171129     c                   if        TTE_PODIMAGREF <> *blank
027300171129     c                   movel     TTE_PODIMAGREFcampo7a           7
027400171129     c                   clear                   len7a             3 0
027500171129     c                   clear                   da                3 0
027600171129     c                   eval      len7a = %Len(%Trim(campo7a))
027700171129     c                   eval      da = 8 - len7a
027800171129     c                   move      *all'0'       podimag7          7
027900171129     c                   eval      %subst(podimag7:da:len7a)=
028000171129     c                             %subst(campo7a:1:len7a)
028100171129     c                   move      podimag7      I5PODIMAG
028200171129     c                   end
028300161020      **
028400161020     c                   Endsr
028500161020      * ?_______________________________________________________________    */
028600161020     C*      SCAN 18 X PARCEL SHOP
028700161020      * ?_______________________________________________________________    */
028800161020     C     SCAN_18_PUDO  BEGSR
028900161020      *
029000161020      * ------ passa la DS completa e riceverà lo SCAN sostitutivo
029100171212      *         dalla tabella "SCS" relativo all'evento del PUDO.
029200171212      *          Controlla e cancella eventuale SCAN 13 in attesa
029300171212      *           che avrebbe dato automaticamente x consegnata la spedizione.
029400180313     c                   call      'FIEU04R18'
029500161130     c                   parm                    FIEU18
029600161020      *
029700161020      *  se torna con lo stesso SCAN 18 senza averlo sostituito
029800161020     c                   if        i5SCANT =  18
029900161020     c                   Eval      se_errore = 'S'
030000161020     c                   End
030100161020      *
030200161020     c                   Endsr
030300161020      * ?_______________________________________________________________    */
030400161020      *   ARRIVATO uno SCAN 05 controlla bolla in sospensione
030500161020      * ?_______________________________________________________________    */
030600161020     C     SCAN_05       BEGSR
030700161020      *
030800161020      * registra il 1° arrivo dello SCAN 05
030900161020     c                   exsr      check_WFIEU00
031000161020      *
031100161020      **  se non è ancora stato confermato il CMR
031200161020     C                   eval        vatTRC = 'E'
031300161020     C                   eval        vatNOT = i5PARCEL
031400161020     C     Kvat4E        setll     edivat4l
031500161020     C     vatTRC        reade     edivat4l
031600161020      *--
031700161020     c                   IF        not %EoF(edivat4l)
031800161020     c                                 and
031900161020     C                             I5PARCEL = %SUBST(vatNOT:1:14)
032000161020      *--
032100161020      *-   Se era in attesa dello SCAN05 di collo PARTITO
032200161020     c                   if           vatCMR = GEO_in_Sospeso
032300161020     C                   eval       Msg_Vin_80 = 'Già su EDIVAB in Sospeso'
032400161020      * ==========
032500161020      *  imposta il CMR x la conferma
032600161020     c                   exsr      CAMBIA_CMR
032700161020     c                   exsr      UPD_EDIVA
032800161020      *
032900161020     c                   else
033000161020      *-  Se c'è su EDIVAB
033100161020      *--  ed era già stato ATTIVATO (quindi non più in sospensione)
033200161020     C                   eval      Msg_Vin_80 = 'SCAN 05 aggiorna solo -
033300161020     c                             serv.code e peso'
033400161020     c                   clear                   new_CMR
033500161020     C                   Exsr      UPD_EDIVA
033600161020     c                   end
033700161020      *--
033800161020     c                   else
033900161020      *--
034000161020      ** non presente su EDIVAB/T   (su FIVAB ? o FNBLP ? oppure non c'era ?)
034100161020     c                   exsr      NON_SU_EDIVA
034200161020      *--
034300161020     c                   end
034400170907      * -----------------                  -------------------
034500161020     C                   ENDSR
034600161020      * ?_______________________________________________________________    */
034700161020      *    Rileva la prima volta che arriva lo SCAN 05
034800161020      * ?_______________________________________________________________    */
034900161020     C     check_WFIEU00 BEGSR
035000161020     C*
035100161020     c     i5PARCEL      chain     wfieu00f
035200161020     c                   if        %Found(wfieu00f)
035300161020     c                   if        F00DATSC5 = 0
035400161020     c                   eval      F00DATSC5 = dateu
035500161020     c                   eval      F00ORASC5 = wora
035600161020     c                   update    fieu000
035700161020     c                   end
035800161020     c                   else
035900161020     c                   clear                   fieu000
036000161020     c                   eval      F00PARCEL = i5PARCEL
036100161020     c                   eval      F00DATSC5 = dateu
036200161020     c                   eval      F00ORASC5 = wora
036300161020     c                   write     fieu000
036400161020     c                   end
036500161020     C*
036600161020     C                   ENDSR
036700161020      * ?_______________________________________________________________    */
036800161020      *  si compone il CMR correttamente eliminando "IN SOSPESO"
036900161020      * ?_______________________________________________________________    */
037000161020     C     CAMBIA_CMR    BEGSR
037100161020      *
037200161020     c                   clear                   new_CMR          35
037300161020     c                   movel     I5DATTIM      I5DATAw           8 0
037400161020      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
037500161020     c     *iso          movel     DATEU         DATA_iso
037600161020    2c                   if        I5DATAw >0
037700161020     c                   TEST(D)                 I5DATAw                99
037800161020     c  n99*iso          movel     I5DATAw       DATA_iso
037900161020     c                   end
038000161020      *
038100161020      *  Da quale Depot è partito il pacco (aggiunge i giorni di viaggio)
038200161020     c                   exsr      gg_di_VIAGGIO
038300161020      *
038400161020      *   Se la data risultante è inferiore alla UDATE forza UDATE
038500161020     c                   move      data_iso      data_iso8         8 0
038600161020     c                   if        data_iso8 < dateu
038700161020     c                   move      dateu         data_iso8
038800161020     c                   end
038900161020      **
039000161020      * IMPOSTA IL CMR x LA SPEDIZIONE
039100161020     c                   eval      new_CMR = 'GEO_' + %editc(data_iso8:'Y')
039200161020      *
039300161020      **  Controlla se si tratta di gomme per poterle evidenziare nel CMR
039400161020     c                   if        i5SERVICE = Gomme_365 or
039500161020     c                             i5SERVICE = Gomme_366 or
039600161020     c                             i5SERVICE = Gomme_367
039700161020     c                   eval        new_CMR = %trim(new_CMR) + '_GOMME'
039800161020     c                   end
039900161020      *
040000161020     c                   Endsr
040100161020      * ?_______________________________________________________________    */
040200161020     C*  in base al Depot di provenienza determina i giorni di viaggio
040300161020      * ?_______________________________________________________________    */
040400161020     C     gg_di_VIAGGIO BEGSR
040500161020      *
040600161020      ** imposta il default
040700161020     c                   z-add     1             gg_incr           3 0
040800161020     c                   movel     I5DEPCODE     Start_depot       4
040900161020     c     ritenta       tag
041000161020      *
041100161020      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
041200161020      *   e se non fosse presente con i primi 4 caratteri del Parcel
041300161020     c                   if        Start_depot <> *blank
041400161020     c                   movel     Start_depot   DepotParcel
041500161020     c                   z-add     1             giro_nr           3 0
041600161020     c                   Else
041700161020     c                   z-add     2             giro_nr
041800161020     c                   movel     i5PARCEL      DepotParcel
041900161020     c                   End
042000161020      *
042100161020      *
042200171003      *  aggancia Versione/Anagrafica e Legame
042300171003      *        TISIEDEP
042400171003      *        I_DATAVER      1     8 S  0 DATA VERSIONE/0=UDATE
042500171003      *        I_DEPOT4       9     4 S  0 =0 SE PASSSATO IL DEPOT A 7
042600171003      *        I_DEPOT7      13     7 S  0 =0 SE PASSSATO IL DEPOT A 4
042700171003      *        O_FLGVERS     20     1 A    TEST VERSIONE 0=OK/1=ERR/BLK=NON TESTATO
042800171003      *        O_FLGCAPP     21     1 A    TEST CAPPARIO 0=OK/1=ERR/BLK=NON TESTATO
042900171003      *        O_FLGLEGA     22     1 A    TEST LEGAMI   0=OK/1=ERR/BLK=NON TESTATO
043000171003      *
043100171003     c                   clear                   tisieDEPds
043200171003     c                   movel     DepotParcel   I_DEPOT4
043300171003     c                   call      'TISIEDEPR'
043400171003     c                   parm                    tisieDEPds
043500171003     c                   parm                    prmCPD10F
043600171003     c                   parm                    prmLEG10F
043700171003      *  se trovato TUTTO OK:
043800171003     c                   if        O_FLGVERS = '0' and
043900171003     c                             O_FLGCAPP = '0' and
044000171003     c                             O_FLGLEGA = '0'
044100171003      *
044200171003     c                   eval      DlegFLO = legFLO
044300171003     c                   if          §legFLOGGI <> *blank and
044400171003     c                               §legFLOGGI <> '000'  and
044500171003     c                               %check(digits:§legFLOGGI) = 0
044600171003     c                   move      §legFLOGGI    gg_incr           3 0
044700171003     c                   end
044800171003      *
044900171003     c                   endif
045000171003      *
045100171003     c                   if        §legFLOLNP = '000' or
045200171003     c                             §legFLOLNP = *blank
045300171003     c                   if          giro_nr = 1
045400171003     c                   clear                   Start_depot
045500171003     c                   goto      ritenta
045600171003     c                   End
045700171003     c                   End
045800171003      *
045900171003      *=============
046000161020      *  se Non c'è incremento
046100161020     c                   if        gg_incr = 0
046200161020     c                   exsr      test_gg_lav
046300161020     c                   else
046400161020      *  aggiunge x giorni (ma solo lavorativi)
046500161020     c                   do        gg_incr       giorni            3 0
046600161020     c                   adddur    1:*D          data_iso
046700161020     c                   exsr      test_gg_lav
046800161020     c                   enddo
046900161020     c                   endif
047000161020      *
047100161020     c                   Endsr
047200161020      * ?_______________________________________________________________    */
047300161020     C*  test sui GG lavorativi altrimenti CERCA fino al primo lavorativo
047400161020      * ?_______________________________________________________________    */
047500161020     C     test_gg_lav   BEGSR
047600161020      *
047700161020      * -----------
047800161020      * Cerca definitivamente il D-day lavorativo
047900161020      *  esclude sia il sabato che la domenica
048000161020      * -----------
048100161020     c                   do        *hival
048200161020     c                   move      data_iso      dadata
048300161020     c                   move      data_iso      adata
048400161020     c                   CALL      'XSRLAV8'
048500161020     c                   PARM                    Wdata8
048600161020      * se corrisponde ho trovato il D-Day
048700161020     c                   if        giolav <> 0
048800161020     c                   leave
048900161020     c                   else
049000161020      * altrimenti continuo a cercare il primo giorno lavorativo valido.
049100161020     c     data_iso      adddur    1:*d          data_iso
049200161020     c                   iter
049300161020     c                   endif
049400161020     c                   enddo
049500161020      *
049600161020     c                   Endsr
049700161020      * ?================================================================== */
049800161020     C*?       ERA su EDIVAB in SOSPENSIONE in attesa dello SCAN 05
049900161020      * ?================================================================== */
050000161020     C     UPD_EDIVA     BEGSR
050100161020      *
050200161020      *  per chiave bolla aggiorna la bolla in SOSPENZIONE che adesso
050300161020      *  risulta partita con l'IGATEAVIS ricevuto (Manifest)
050400161020     C     KSosp_EDIVA   setll     EDIVAT3L
050500161020     C     KSosp_EDIVA   reade     EDIVAT3L
050600161020      *
050700161020     c                   DOw       not %EoF(EDIVAT3L)
050800161020      *
050900161020      *  sul record del Parcel/Service Code sostituisce il SERVICE CODE con quello
051000161020      *  passato nello SCAN 05
051100161020     C                   if        I5PARCEL = %SUBST(vatNOT:1:14)
051200161020     c                                  and
051300161020     C                               vatTRC = 'E'
051400161020     c                                  and
051500161020     C                             I5SERVICE <> *zeros
051600161020     c                   eval      dVATe     = vatNOT
051700161020     c                   move      i5SERVICE     §VATESCD
051800161020     c                   eval      vatNOT    = dVATe
051900161020     c                   End
052000161020      *
052100161020      *  sempre il nuovo CMR GEO_Data
052200161020     C                   if        new_CMR  <> *blank
052300161020     c                   eval      vatCMR  =  new_CMR
052400161020     c                   End
052500161020     c                   Update    EDIVAT00
052600161020      *
052700161020     C     KSosp_EDIVA   reade     EDIVAT3L
052800161020     c                   EndDO
052900161020      * ==========
053000161020      *  poi allinea anche la testata bolla del VAB
053100161020      * ==========
053200161020     C     KSosp_EDIVA   chain     EDIVAB2L
053300161020     c                   if        %Found(EDIVAB2L)
053400161020      *
053500161020      * Se passato il PESO lo sostituisce
053600161020     c                   if        I5WEIGHT > 0
053700161020     c                   Z-ADD     I5WEIGHT      VABPKB
053800161020      * CORREZIONI:
053900161020      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
054000161020     c                   IF        vabPKB > 31,5
054100161020     c                   eval           vabPKB = 30
054200161020     c                   elseIF    vabpkb =0
054300161020     c                   eval           vabpkb = 0,1
054400161020     c                   END
054500161020     c                   End
054600161020      *
054700161020     C                   if        new_CMR  <> *blank
054800161020     c                   eval           vabCMR = new_CMR
054900161020     c                   End
055000161020     c                   eval      vabSCL = 'G'
055100161020     c                   Update    EDIVAB00
055200161020     c                   End
055300161020      *
055400161020     c                   Endsr
055500161020      * ?_______________________________________________________________    */
055600161020     C*?   NON ERA su EDIVAB   cerca su FNBLP e su FIVAB
055700161020      * ?_______________________________________________________________    */
055800161020     C     NON_SU_EDIVA  BEGSR
055900161020      *
056000161020     c                   clear                   in_FNBLP          1
056100161020     c                   clear                   in_FIVAB          1
056200161020      **
056300171003      **  se già confermato in Partenza
056400171003     C     i5PARCEL      chain     fipnd03l
056500171003     c                   IF        %Found(fipnd03l)
056600171003     c                   movel     i5PARCEL      DepotParcel
056700171003      **
056800161020      *   controlla che sia DPD con il Network e non con la linea
056900171003     c     pndLNP        chain     azorg01l
057000161020     c                   if        %Found(azorg01l)
057100161020     c                   eval          OG143  =  orgDE3
057200161020     c                   if        §OGNTW = 'DPD'
057300161020     c                   eval          in_FNBLP = 'S'
057400161020     C                   eval      Msg_Vin_80 = 'Già su BLP Confermato'
057500161020     c                   LEAVESR
057600161020     c                   end
057700161020     c                   end
057800161020      **
057900161020     c                   endIF
058000161020      ** ---------
058100161020      **  se non ancora confermato da Cliente
058200161020     C                   eval        vatTRC = 'E'
058300161020     C                   eval        vatNOT = i5PARCEL
058400161020     C     Kvat4E        setll     fivat02l
058500161020     C     vatTRC        reade     fivat02l
058600161020      *-
058700161020     c                   dow         not %EoF(fivat02l)
058800161020      *-
058900161020     c* imposta DS x reperire il Parcel
059000161020     c                   movel     vatNOT        dVATe
059100161020      *-
059200161020      *- se superata la chiave parcel esce dal ciclo x evitare letture inutili.
059300161020     C                   if           §vatEPRN > i5PARCEL
059400161020     c                   leave
059500161020     c                   end
059600161020      *-
059700161020     c* se la chiave è superiore esce dal ciclo
059800161020     C                   if          i5PARCEL = §vatEPRN
059900161020      *-
060000161020     c     vatLNP        chain     azorg01l
060100161020     c                   if        %Found(azorg01l)
060200161020     c                   eval      OG143 = orgDE3
060300161020      *-
060400161020     c                   if        §OGNTW = 'DPD'
060500161020      * Deve aggiornare il SERVICE CODE con l'ultimo arrivato
060600161020     c     vat_nrr       chain     fivat00f
060700161020     c                   if        %found(fivat00f)
060800161020     c                   eval      dVATe     = vatNOT
060900161020     c                   move      i5SERVICE     §VATESCD
061000161020     c                   eval      vatNOT    = dVATe
061100161020     c                   update    fivatFIS
061200161020     c                   end
061300161020      *
061400161020     c                   eval        vabCCM = vatCCM
061500161020     c                   move      i5Parcel      vabRMN
061600161020     c     kvab01        chain     fivab01l
061700161020     c                   if        %Found(fivab01l)
061800161020     C                   eval      Msg_Vin_80 =
061900161020     C                             'Aggiorna SOLO Peso/Serv.Code su FIVAB'
062000161020     c                   eval          in_FIVAB = 'S'
062100161020      *
062200161020      * Se passato il PESO lo sostituisce
062300161020     c                   if        I5WEIGHT > 0
062400161020     c                   Z-ADD     I5WEIGHT      VABPKB
062500161020      * CORREZIONI:
062600161020      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
062700161020     c                   IF        vabPKB > 31,5
062800161020     c                   eval           vabPKB = 30
062900161020     c                   elseIF    vabpkb =0
063000161020     c                   eval           vabpkb = 0,1
063100161020     c                   END
063200161020     c                   End
063300161020      *
063400161020     c                   update    fivab000
063500161020     c                   LEAVESR
063600161020     c                   end
063700161020     c                   end
063800161020      *
063900161020     c                   end
064000161020      *-
064100161020     c                   end
064200161020     C     vatTRC        reade     fivat02l
064300161020     c                   end
064400161020      **
064500161020     C                   ENDSR
064600161020      * ?================================================================== */
064700060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
064800161020      * ?================================================================== */
064900161021     C     callFIEU18    BEGSR
065000060614      *
065100060615     c                   clear                   ok_18             1
065200060630     c                   clear                   msgerr           80
065300060620     c                   clear                   msg_vin_80
065400060615     c                   call      'FIEU18R'
065500161130     c                   parm                    FIEU18
065600060615     c                   parm                    ok_18
065700060620     c                   parm                    msgerr
065800060615
065900161020     c                   eval       se_errore = ok_18
066000060620     c                   eval      msg_vin_80 = msgerr
066100060612      *
066200060615     c                   Endsr
066300161020      * ?_______________________________________________________________    */
066400161020      *    ROUT iniziale
066500060615      * ?_______________________________________________________________    */
066600060615     C     *INZSR        BEGSR
066700160120      *
066800160120     C     Kvat4E        KLIST
066900160120     C                   KFLD                    vatTRC
067000160120     C                   KFLD                    vatNOT
067100160121      *
067200160121     C     kVAT01        KLIST
067300160121     C                   KFLD                    VABFGS
067400160121     C                   KFLD                    VABCCM
067500160121     C                   KFLD                    VABAAS
067600160121     C                   KFLD                    VABLNP
067700160121     C                   KFLD                    VABNRS
067800160121     C                   KFLD                    VABNSP
067900160121      *
068000160121     C     kVAB01        KLIST
068100160121     C                   KFLD                    vabCCM
068200160121     C                   KFLD                    vabRMN
068300160120      *
068400160120     C     KSosp_EDIVA   KLIST
068500160120     C                   KFLD                    vatAAS
068600160120     C                   KFLD                    vatLNP
068700160120     C                   KFLD                    vatNRS
068800160120     C                   KFLD                    vatNSP
068900160120      *
069000171212     C     KTABel        KLIST
069100171212     C                   KFLD                    TBLKUT
069200171212     C                   KFLD                    TBLCOD
069300171212     C                   KFLD                    TBLKEY
069400160120      *
069500160120      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
069600991129     c     *ENTRY        PLIST
069700161124     C                   parm                    FIEU04DS40
069800161020     C                   parm                    se_errore
069900161020     C                   parm                    Msg_Vin_80
070000161020      **
070100971216      * Recupero data e ora
070200971216     C                   TIME                    WORA
070300991124     C                   TIME                    W0140
070400991124      * UDATE IN GGMMAAAA
070500991124     C                   MOVE      W0140         WDTGIO
070600991124      * UDATE IN AAAAMMGG
070700991124     C     *eur          MOVEL     WDTGIO        DATA_eur
070800991124     C     *iso          MOVEL     DATA_eur      dateu
070900171212      *
071000171212      *  Cod.BRT -- B.U.
071100171212     c                   movel     '023'         BU_brt            3
071200171212     C                   z-add     1             TBLKUT
071300171212     C                   movel     '3I'          TBLCOD
071400171212     C                   movel(p)  'DPD'         TBLKEY
071500171212     c     KTABel        chain     tabel00f
071600171212     c                   if        %Found(tabel00f)
071700171212     c                   movel     tbluni        ds3Idp
071800171212     c                   move      §3IBUCN       BU_brt
071900171212     c                   end
072000171212     C*------------------
072100160120      *
072200991124     C                   ENDSR
072300161020      * ?_______________________________________________________________    */
072400161020      *    Se errori NON blocca il programma
072500161020      * ?_______________________________________________________________    */
072600161020     C     *pssr         BEGSR
072700161020     C                   ENDSR     '*CANCL'
072800160120      *  ------------------------------------------------------------------ */
