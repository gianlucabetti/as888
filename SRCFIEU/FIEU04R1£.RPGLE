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
001800170907     FTNCLD00F  IF   E           K DISK
001900161020      *
002000160125     FTIDP200F  uf a E           k DISK
002100161020      *
002200161020     FWFIEU00F  uf a E           k DISK
002300160121      *
002400160121     FAZorg01L  if   E           K DISK
002500171003     Ffipnd03l  IF   E           K DISK
002600171212     FTABEL00F  IF   E           K DISK
002700171003      ** ------------------------------------------------------------------ */
002800161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
002900161124     D FIEU04DS40    E DS                  prefix(TTE_)
003000171003      ** ------------------------------------------------------------------ */
003100171003      *
003200171212      *-------------------
003300171212     D DS3IDP        E DS
003400171212      *-------------------
003500161020     d vatds           ds
003600161020     d  vat_nrr              397    400b 0
003700171003      ** -------------------------------------------------------------------*****
003800000223     D W0140           S             14  0
003900991129     D WORA            S              6  0
004000991129     D WDTGIO          S              8  0
004100991129     D DATEU           S              8  0
004200991129     D DATA_eur        S               D   DATFMT(*eur)
004300160120     D DATA_iso        S               D   DATFMT(*iso)
004400161020     d dVATe         e ds
004500160120      *-------------------
004600160121     D OG143         E DS
004700160121      *-------------------
004800171003     D prmCPD10F     e DS                  extname(DPCDP10F)
004900171003     D prmLEG10F     e DS                  extname(DPLEG10F)
005000171003      *----------
005100171003     D DlegFLO       E DS
005200171003     D TISIEDEPDS    E DS
005300171003      *-------------------
005400160120     d Wdata8          DS
005500160120     d  dadata                 1      8  0
005600160120     d  adata                  9     16  0
005700160120     d  GioLav                17     21  0
005800160120      *
005900160120     D DepotParcel     S              4
006000171003      *  ______________________________________________________________     */
006100161020     D  se_errore      s              1
006200161020     D  Msg_Vin_80     s             80
006300060619     D Testa_Sk        s             10    DIM(50)
006400171003      *  ______________________________________________________________     */
006500161020     D   Digits        C                   '0123456789'
006600161020     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
006700161020     d Gomme_365       c                   365
006800161020     d Gomme_366       c                   366
006900161020     d Gomme_367       c                   367
007000171003      *  ______________________________________________________________     */
007100060612     c                   SETON                                        LR
007200171212      *-----
007300171212      **  in funzione degli SCAN 5 portati sul TTEVENTS vengono rimandati a noi
007400171212      **  tutti gli scan che NOI abbiamo inviato a DPD. Per questo occorre filtrare
007500171212      **  tutti gli scan generati da NOI, quindi con B.UNIT = "023" e devono essere
007600171212      **  scartati e non Elaborati.
007700171212     c                   if        %len(%trim(TTE_DEPOT))= 7  and
007800171212     c                             %Subst(TTE_DEPOT:1:3) = BU_brt
007900171212      * Errore su VINMSG
008000171212     C                   eval      se_errore   = 'S'
008100171212     C                   eval      Msg_Vin_80 = 'SCAN da ignorare perché -
008200171212     C                             generato da BRT;DEPOT:' + %trim(TTE_DEPOT)
008300171212     c                   RETURN
008400171212     c                   endif
008500161020      ** ===========
008600060612      ** Decodifica record a campi variabili
008700060612     c                   exsr      Decod_Record
008800160412      ** ===========
008900171212      **   x SCAN 18 PUDO  solo su nostro EXPORT  sotto tipologia 128/129 PUDO
009000171212      *   trasforma lo SCAN RICEVUTO in un ALTRO SCAN da passare poi al FIEU18R
009100171212      *   per essere elaborato (utilizza la tabella "SCS" per sostituire lo SCAN
009200171212      *   con un altro codificato sulla "DIR")
009300161020     c                   if           i5SCANT  = 18
009400160412     c                   IF        (%subst(i5PARCEL:1:4) = '0844'  or
009500160412     c                              %subst(i5PARCEL:1:4) = '0845')     and
009600160412     c                             (%subst(i5INFOTXT:1:4) = '128:' or
009700170908     c                              %subst(i5INFOTXT:1:4) = '129:' or
009800170908     c                              %subst(i5INFOTXT:2:4) = '128:' or
009900170908     c                              %subst(i5INFOTXT:2:4) = '129:')
010000160412      *   trasforma lo SCAN RICEVUTO in un ALTRO SCAN per descrivere il
010100160412     c                   exsr      SCAN_18_PUDO
010200161020      **
010300161020     c                   if        se_errore ='S'
010400161020     C                   eval       Msg_Vin_80 = 'Errori conv.SCAN 18 PUDO'
010500161020     c                   RETURN
010600161020     c                   endif
010700160412      **
010800160412     c                   END
010900161020     c                   endif
011000160120      ** ===========
011100160120      *** se arrivato uno  SCAN 05 si attiva la spedizione sul GEODATA
011200160120      **   in sospensione sul EDIVAB e se non lo trova lo scrive o aggiorna
011300160120      ** e NON è la ripetizione del nostro scan x quando facciamo l'export.
011400160120      **   ossia non è un 0844/0845 Italia
011500161020     c                   IF        i5SCANT  = 05
011600160120     c                               and
011700160120     c                              %subst(i5PARCEL:1:4) <> '0844'
011800160120     c                               and
011900160120     c                              %subst(i5PARCEL:1:4) <> '0845'
012000160120      ** ===========
012100160120     c                   exsr      SCAN_05
012200161021      **
012300161021     c                   if        se_errore ='S'
012400161021     C                   eval       Msg_Vin_80 = 'Errori conv.SCAN 05'
012500161021     c                   RETURN
012600161021     c                   endif
012700160120      ** ===========
012800060612     c                   else
012900060612
013000060612      *  con il record in canna scrive direttamente il VAB ed il VAT
013100161021     c                   exsr      callFIEU18
013200160121     c                   end
013300160121
013400161021      *  Problemi nella decodifica dei campi
013500161021     c                   if        se_errore<>'S'
013600060614      *  Record OK
013700160126      *  x gli SCAN 05 deve riportare quanto segnalato dalla routine SCAN_05
013800160126     c                   if        i5SCANT <> 05
013900160126     c                                or
014000160126     c                             (i5SCANT  = 05 and
014100160126     c                             (%subst(i5PARCEL:1:4) = '0844'
014200160126     c                               or
014300160126     c                              %subst(i5PARCEL:1:4) = '0845') )
014400161020     C                   eval       Msg_Vin_80 = *Blank
014500160126     c                   end
014600060614     c                   end
014700060612      **
014800161020     c                   RETURN
014900161020      * ?______________________________________________________________     */
015000161020      *?    Decodifica record a campi variabili
015100161020      * ?______________________________________________________________     */
015200161020     c     Decod_Record  Begsr
015300161020
015400161130      * ? Sposta dal tracciato TTE al vecchio FIEU18DS poichè al momento
015500161020      * ?  non vi è nulla di nuovo da aggiungere
015600161130     c                   clear                   FIEU18
015700170907     c                   clear                   i5DEPCODE7        7
015800161020     c                   eval      I5PARCEL    = TTE_PARCELNR
015900170317      **
016000170317      ** nuovo depot può essere  lungo 7 anzichè 4 ei vecchio Depot di 4 sta a dx.
016100170317     c                   if        %len(TTE_DEPOT) = 7
016200171129     c                   movel     TTE_DEPOT     depot7            7
016300171129     c                   move      depot7        depot4            4
016400170317     c                   movel     depot4        i5DEPCODE
016500171129     c                   movel     depot7        i5DEPNAME
016600170317     c                   elseif    %len(TTE_DEPOT) = 4
016700161020     c                   eval      I5DEPCODE   = TTE_DEPOT
016800170317     c                   end
016900170907      *
017000170907     c                   movel     TTE_DEPOT     i5DEPCODE7        7
017100170317      **
017200161130     c                   movel     *zeros        I5DATTIM
017300161130     c                   movel     TTE_STDATETIMEI5DATTIM
017400161020     c                   eval      I5SERVICE   = TTE_SERVICECOD
017500161020     c                   eval      I5RECNAME   = TTE_RNAME
017600161020     c                   eval      I5INFOTXT   =
017700161020     c                             %editc(TTE_INFOCONT1:'X') + TTE_INFOCONT2
017800161020      **
017900161020      **  essendo in decagrammi ha quindi 2 decimali quindi basta allineare a destra
018000161020     c                   move      TTE_WEIGHT    I5WEIGHT
018100161020      **
018200161020     c                   movel     TTE_SCANCODE  I5SCANT
018300161020     c                   movel     TTE_DZIPCODE  I5CONSZIP
018400161020     c                   movel     TTE_REASONCOD1I5ADDSER1
018500161020     c                   movel     TTE_REASONCOD2I5ADDSER2
018600161020     c                   movel     TTE_REASONCOD3I5ADDSER3
018700171129      **  solo se pieno
018800171129     c                   if        TTE_PODIMAGREF <> *blank
018900171129     c                   movel     TTE_PODIMAGREFcampo7a           7
019000171129     c                   clear                   len7a             3 0
019100171129     c                   clear                   da                3 0
019200171129     c                   eval      len7a = %Len(%Trim(campo7a))
019300171129     c                   eval      da = 8 - len7a
019400171129     c                   move      *all'0'       podimag7          7
019500171129     c                   eval      %subst(podimag7:da:len7a)=
019600171129     c                             %subst(campo7a:1:len7a)
019700171129     c                   move      podimag7      I5PODIMAG
019800171129     c                   end
019900161020      **
020000161020     c                   Endsr
020100161020      * ?_______________________________________________________________    */
020200161020     C*      SCAN 18 X PARCEL SHOP
020300161020      * ?_______________________________________________________________    */
020400161020     C     SCAN_18_PUDO  BEGSR
020500161020      *
020600161020      * ------ passa la DS completa e riceverà lo SCAN sostitutivo
020700171212      *         dalla tabella "SCS" relativo all'evento del PUDO.
020800171212      *          Controlla e cancella eventuale SCAN 13 in attesa
020900171212      *           che avrebbe dato automaticamente x consegnata la spedizione.
021000161020     c                   call      'FIEU02R18'
021100161130     c                   parm                    FIEU18
021200161020      *
021300161020      *  se torna con lo stesso SCAN 18 senza averlo sostituito
021400161020     c                   if        i5SCANT =  18
021500161020     c                   Eval      se_errore = 'S'
021600161020     c                   End
021700161020      *
021800161020     c                   Endsr
021900161020      * ?_______________________________________________________________    */
022000161020      *   ARRIVATO uno SCAN 05 controlla bolla in sospensione
022100161020      * ?_______________________________________________________________    */
022200161020     C     SCAN_05       BEGSR
022300161020      *
022400170907     c                   clear                   wri_SCAN5         1
022500170907      *
022600161020      * registra il 1° arrivo dello SCAN 05
022700161020     c                   exsr      check_WFIEU00
022800161020      *
022900161020      **  se non è ancora stato confermato il CMR
023000161020     C                   eval        vatTRC = 'E'
023100161020     C                   eval        vatNOT = i5PARCEL
023200161020     C     Kvat4E        setll     edivat4l
023300161020     C     vatTRC        reade     edivat4l
023400161020      *--
023500161020     c                   IF        not %EoF(edivat4l)
023600161020     c                                 and
023700161020     C                             I5PARCEL = %SUBST(vatNOT:1:14)
023800161020      *--
023900161020      *-   Se era in attesa dello SCAN05 di collo PARTITO
024000161020     c                   if           vatCMR = GEO_in_Sospeso
024100161020     C                   eval       Msg_Vin_80 = 'Già su EDIVAB in Sospeso'
024200161020      * ==========
024300161020      *  imposta il CMR x la conferma
024400161020     c                   exsr      CAMBIA_CMR
024500161020     c                   exsr      UPD_EDIVA
024600161020      *
024700161020     c                   else
024800161020      *-  Se c'è su EDIVAB
024900161020      *--  ed era già stato ATTIVATO (quindi non più in sospensione)
025000161020     C                   eval      Msg_Vin_80 = 'SCAN 05 aggiorna solo -
025100161020     c                             serv.code e peso'
025200161020     c                   clear                   new_CMR
025300161020     C                   Exsr      UPD_EDIVA
025400161020     c                   end
025500161020      *--
025600161020     c                   else
025700161020      *--
025800161020      ** non presente su EDIVAB/T   (su FIVAB ? o FNBLP ? oppure non c'era ?)
025900161020     c                   exsr      NON_SU_EDIVA
026000161020      *--
026100161020      *-  Se non c'è da NESSUNA parte, lo scrivo Parcheggiandolo
026200161020      *--   sul TIDP200F in attesa che arrivi poi la BOLLA sul GEODATA:
026300161020     c                   if         in_FNBLP <> 'S'
026400161020     c                                 and
026500161020     c                              in_FIVAB <> 'S'
026600170907     c                   move      'S'           wri_SCAN5
026700170907     c********           exsr      WRT_TIDP2
026800161020     c                   end
026900161020      *--
027000161020     c                   end
027100170907      **
027200170907      * -----------------                  -------------------
027300170907      **  testa se VTG
027400170907     c                   clear                   parcel_VTG        1
027500170907     c                   movel     I5PARCEL      Parcel14         14
027600170907     C                   movel(p)  'VTG'         cldCOD
027700170907     C                   movel     Parcel14      cldDEP
027800170907     c     key_VTG       setll     tncld00F
027900170907     c     key_VTG       reade     tncld00F
028000170907     c                   dow       not %EoF(tncld00F)
028100170907     c                   if        dateu >= cldDDA and
028200170907     c                             dateu <= cldADA
028300170907      **  Se è un collo VTG
028400170907     c                   movel     'S'           parcel_VTG
028500170907     c                   leave
028600170907     c                   end
028700170907     c     key_VTG       reade     tncld00F
028800170907     c                   endDO
028900170907      **
029000170907      **  Deve scrivere lo SCAN"05" in attesa oppure sempre per i VTG
029100170907     c                   if        wri_SCAN5  = 'S' or
029200170907     c                             parcel_VTG = 'S'
029300170907     c                   exsr      WRT_TIDP2
029400170907     c                   end
029500161020      **
029600161020     C                   ENDSR
029700161020      * ?_______________________________________________________________    */
029800161020      *    Rileva la prima volta che arriva lo SCAN 05
029900161020      * ?_______________________________________________________________    */
030000161020     C     check_WFIEU00 BEGSR
030100161020     C*
030200161020     c     i5PARCEL      chain     wfieu00f
030300161020     c                   if        %Found(wfieu00f)
030400161020     c                   if        F00DATSC5 = 0
030500161020     c                   eval      F00DATSC5 = dateu
030600161020     c                   eval      F00ORASC5 = wora
030700161020     c                   update    fieu000
030800161020     c                   end
030900161020     c                   else
031000161020     c                   clear                   fieu000
031100161020     c                   eval      F00PARCEL = i5PARCEL
031200161020     c                   eval      F00DATSC5 = dateu
031300161020     c                   eval      F00ORASC5 = wora
031400161020     c                   write     fieu000
031500161020     c                   end
031600161020     C*
031700161020     C                   ENDSR
031800161020      * ?_______________________________________________________________    */
031900161020      *  si compone il CMR correttamente eliminando "IN SOSPESO"
032000161020      * ?_______________________________________________________________    */
032100161020     C     CAMBIA_CMR    BEGSR
032200161020      *
032300161020     c                   clear                   new_CMR          35
032400161020     c                   movel     I5DATTIM      I5DATAw           8 0
032500161020      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
032600161020     c     *iso          movel     DATEU         DATA_iso
032700161020    2c                   if        I5DATAw >0
032800161020     c                   TEST(D)                 I5DATAw                99
032900161020     c  n99*iso          movel     I5DATAw       DATA_iso
033000161020     c                   end
033100161020      *
033200161020      *  Da quale Depot è partito il pacco (aggiunge i giorni di viaggio)
033300161020     c                   exsr      gg_di_VIAGGIO
033400161020      *
033500161020      *   Se la data risultante è inferiore alla UDATE forza UDATE
033600161020     c                   move      data_iso      data_iso8         8 0
033700161020     c                   if        data_iso8 < dateu
033800161020     c                   move      dateu         data_iso8
033900161020     c                   end
034000161020      **
034100161020      * IMPOSTA IL CMR x LA SPEDIZIONE
034200161020     c                   eval      new_CMR = 'GEO_' + %editc(data_iso8:'Y')
034300161020      *
034400161020      **  Controlla se si tratta di gomme per poterle evidenziare nel CMR
034500161020     c                   if        i5SERVICE = Gomme_365 or
034600161020     c                             i5SERVICE = Gomme_366 or
034700161020     c                             i5SERVICE = Gomme_367
034800161020     c                   eval        new_CMR = %trim(new_CMR) + '_GOMME'
034900161020     c                   end
035000161020      *
035100161020     c                   Endsr
035200161020      * ?_______________________________________________________________    */
035300161020     C*  in base al Depot di provenienza determina i giorni di viaggio
035400161020      * ?_______________________________________________________________    */
035500161020     C     gg_di_VIAGGIO BEGSR
035600161020      *
035700161020      ** imposta il default
035800161020     c                   z-add     1             gg_incr           3 0
035900161020     c                   movel     I5DEPCODE     Start_depot       4
036000161020     c     ritenta       tag
036100161020      *
036200161020      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
036300161020      *   e se non fosse presente con i primi 4 caratteri del Parcel
036400161020     c                   if        Start_depot <> *blank
036500161020     c                   movel     Start_depot   DepotParcel
036600161020     c                   z-add     1             giro_nr           3 0
036700161020     c                   Else
036800161020     c                   z-add     2             giro_nr
036900161020     c                   movel     i5PARCEL      DepotParcel
037000161020     c                   End
037100161020      *
037200161020      *
037300171003      *  aggancia Versione/Anagrafica e Legame
037400171003      *        TISIEDEP
037500171003      *        I_DATAVER      1     8 S  0 DATA VERSIONE/0=UDATE
037600171003      *        I_DEPOT4       9     4 S  0 =0 SE PASSSATO IL DEPOT A 7
037700171003      *        I_DEPOT7      13     7 S  0 =0 SE PASSSATO IL DEPOT A 4
037800171003      *        O_FLGVERS     20     1 A    TEST VERSIONE 0=OK/1=ERR/BLK=NON TESTATO
037900171003      *        O_FLGCAPP     21     1 A    TEST CAPPARIO 0=OK/1=ERR/BLK=NON TESTATO
038000171003      *        O_FLGLEGA     22     1 A    TEST LEGAMI   0=OK/1=ERR/BLK=NON TESTATO
038100171003      *
038200171003     c                   clear                   tisieDEPds
038300171003     c                   movel     DepotParcel   I_DEPOT4
038400171003     c                   call      'TISIEDEPR'
038500171003     c                   parm                    tisieDEPds
038600171003     c                   parm                    prmCPD10F
038700171003     c                   parm                    prmLEG10F
038800171003      *  se trovato TUTTO OK:
038900171003     c                   if        O_FLGVERS = '0' and
039000171003     c                             O_FLGCAPP = '0' and
039100171003     c                             O_FLGLEGA = '0'
039200171003      *
039300171003     c                   eval      DlegFLO = legFLO
039400171003     c                   if          §legFLOGGI <> *blank and
039500171003     c                               §legFLOGGI <> '000'  and
039600171003     c                               %check(digits:§legFLOGGI) = 0
039700171003     c                   move      §legFLOGGI    gg_incr           3 0
039800171003     c                   end
039900171003      *
040000171003     c                   endif
040100171003      *
040200171003     c                   if        §legFLOLNP = '000' or
040300171003     c                             §legFLOLNP = *blank
040400171003     c                   if          giro_nr = 1
040500171003     c                   clear                   Start_depot
040600171003     c                   goto      ritenta
040700171003     c                   End
040800171003     c                   End
040900171003      *
041000171003      *=============
041100161020      *  se Non c'è incremento
041200161020     c                   if        gg_incr = 0
041300161020     c                   exsr      test_gg_lav
041400161020     c                   else
041500161020      *  aggiunge x giorni (ma solo lavorativi)
041600161020     c                   do        gg_incr       giorni            3 0
041700161020     c                   adddur    1:*D          data_iso
041800161020     c                   exsr      test_gg_lav
041900161020     c                   enddo
042000161020     c                   endif
042100161020      *
042200161020     c                   Endsr
042300161020      * ?_______________________________________________________________    */
042400161020     C*  test sui GG lavorativi altrimenti CERCA fino al primo lavorativo
042500161020      * ?_______________________________________________________________    */
042600161020     C     test_gg_lav   BEGSR
042700161020      *
042800161020      * -----------
042900161020      * Cerca definitivamente il D-day lavorativo
043000161020      *  esclude sia il sabato che la domenica
043100161020      * -----------
043200161020     c                   do        *hival
043300161020     c                   move      data_iso      dadata
043400161020     c                   move      data_iso      adata
043500161020     c                   CALL      'XSRLAV8'
043600161020     c                   PARM                    Wdata8
043700161020      * se corrisponde ho trovato il D-Day
043800161020     c                   if        giolav <> 0
043900161020     c                   leave
044000161020     c                   else
044100161020      * altrimenti continuo a cercare il primo giorno lavorativo valido.
044200161020     c     data_iso      adddur    1:*d          data_iso
044300161020     c                   iter
044400161020     c                   endif
044500161020     c                   enddo
044600161020      *
044700161020     c                   Endsr
044800161020      * ?================================================================== */
044900161020     C*?       ERA su EDIVAB in SOSPENSIONE in attesa dello SCAN 05
045000161020      * ?================================================================== */
045100161020     C     UPD_EDIVA     BEGSR
045200161020      *
045300161020      *  per chiave bolla aggiorna la bolla in SOSPENZIONE che adesso
045400161020      *  risulta partita con l'IGATEAVIS ricevuto (Manifest)
045500161020     C     KSosp_EDIVA   setll     EDIVAT3L
045600161020     C     KSosp_EDIVA   reade     EDIVAT3L
045700161020      *
045800161020     c                   DOw       not %EoF(EDIVAT3L)
045900161020      *
046000161020      *  sul record del Parcel/Service Code sostituisce il SERVICE CODE con quello
046100161020      *  passato nello SCAN 05
046200161020     C                   if        I5PARCEL = %SUBST(vatNOT:1:14)
046300161020     c                                  and
046400161020     C                               vatTRC = 'E'
046500161020     c                                  and
046600161020     C                             I5SERVICE <> *zeros
046700161020     c                   eval      dVATe     = vatNOT
046800161020     c                   move      i5SERVICE     §VATESCD
046900161020     c                   eval      vatNOT    = dVATe
047000161020     c                   End
047100161020      *
047200161020      *  sempre il nuovo CMR GEO_Data
047300161020     C                   if        new_CMR  <> *blank
047400161020     c                   eval      vatCMR  =  new_CMR
047500161020     c                   End
047600161020     c                   Update    EDIVAT00
047700161020      *
047800161020     C     KSosp_EDIVA   reade     EDIVAT3L
047900161020     c                   EndDO
048000161020      * ==========
048100161020      *  poi allinea anche la testata bolla del VAB
048200161020      * ==========
048300161020     C     KSosp_EDIVA   chain     EDIVAB2L
048400161020     c                   if        %Found(EDIVAB2L)
048500161020      *
048600161020      * Se passato il PESO lo sostituisce
048700161020     c                   if        I5WEIGHT > 0
048800161020     c                   Z-ADD     I5WEIGHT      VABPKB
048900161020      * CORREZIONI:
049000161020      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
049100161020     c                   IF        vabPKB > 31,5
049200161020     c                   eval           vabPKB = 30
049300161020     c                   elseIF    vabpkb =0
049400161020     c                   eval           vabpkb = 0,1
049500161020     c                   END
049600161020     c                   End
049700161020      *
049800161020     C                   if        new_CMR  <> *blank
049900161020     c                   eval           vabCMR = new_CMR
050000161020     c                   End
050100161020     c                   eval      vabSCL = 'G'
050200161020     c                   Update    EDIVAB00
050300161020     c                   End
050400161020      *
050500161020     c                   Endsr
050600161020      * ?_______________________________________________________________    */
050700161020     C*?   NON ERA su EDIVAB   cerca su FNBLP e su FIVAB
050800161020      * ?_______________________________________________________________    */
050900161020     C     NON_SU_EDIVA  BEGSR
051000161020      *
051100161020     c                   clear                   in_FNBLP          1
051200161020     c                   clear                   in_FIVAB          1
051300161020      **
051400171003      **  se già confermato in Partenza
051500171003     C     i5PARCEL      chain     fipnd03l
051600171003     c                   IF        %Found(fipnd03l)
051700171003     c                   movel     i5PARCEL      DepotParcel
051800171003      **
051900161020      *   controlla che sia DPD con il Network e non con la linea
052000171003     c     pndLNP        chain     azorg01l
052100161020     c                   if        %Found(azorg01l)
052200161020     c                   eval          OG143  =  orgDE3
052300161020     c                   if        §OGNTW = 'DPD'
052400161020     c                   eval          in_FNBLP = 'S'
052500161020     C                   eval      Msg_Vin_80 = 'Già su BLP Confermato'
052600161020     c                   LEAVESR
052700161020     c                   end
052800161020     c                   end
052900161020      **
053000161020     c                   endIF
053100161020      ** ---------
053200161020      **  se non ancora confermato da Cliente
053300161020     C                   eval        vatTRC = 'E'
053400161020     C                   eval        vatNOT = i5PARCEL
053500161020     C     Kvat4E        setll     fivat02l
053600161020     C     vatTRC        reade     fivat02l
053700161020      *-
053800161020     c                   dow         not %EoF(fivat02l)
053900161020      *-
054000161020     c* imposta DS x reperire il Parcel
054100161020     c                   movel     vatNOT        dVATe
054200161020      *-
054300161020      *- se superata la chiave parcel esce dal ciclo x evitare letture inutili.
054400161020     C                   if           §vatEPRN > i5PARCEL
054500161020     c                   leave
054600161020     c                   end
054700161020      *-
054800161020     c* se la chiave è superiore esce dal ciclo
054900161020     C                   if          i5PARCEL = §vatEPRN
055000161020      *-
055100161020     c     vatLNP        chain     azorg01l
055200161020     c                   if        %Found(azorg01l)
055300161020     c                   eval      OG143 = orgDE3
055400161020      *-
055500161020     c                   if        §OGNTW = 'DPD'
055600161020      * Deve aggiornare il SERVICE CODE con l'ultimo arrivato
055700161020     c     vat_nrr       chain     fivat00f
055800161020     c                   if        %found(fivat00f)
055900161020     c                   eval      dVATe     = vatNOT
056000161020     c                   move      i5SERVICE     §VATESCD
056100161020     c                   eval      vatNOT    = dVATe
056200161020     c                   update    fivatFIS
056300161020     c                   end
056400161020      *
056500161020     c                   eval        vabCCM = vatCCM
056600161020     c                   move      i5Parcel      vabRMN
056700161020     c     kvab01        chain     fivab01l
056800161020     c                   if        %Found(fivab01l)
056900161020     C                   eval      Msg_Vin_80 =
057000161020     C                             'Aggiorna SOLO Peso/Serv.Code su FIVAB'
057100161020     c                   eval          in_FIVAB = 'S'
057200161020      *
057300161020      * Se passato il PESO lo sostituisce
057400161020     c                   if        I5WEIGHT > 0
057500161020     c                   Z-ADD     I5WEIGHT      VABPKB
057600161020      * CORREZIONI:
057700161020      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
057800161020     c                   IF        vabPKB > 31,5
057900161020     c                   eval           vabPKB = 30
058000161020     c                   elseIF    vabpkb =0
058100161020     c                   eval           vabpkb = 0,1
058200161020     c                   END
058300161020     c                   End
058400161020      *
058500161020     c                   update    fivab000
058600161020     c                   LEAVESR
058700161020     c                   end
058800161020     c                   end
058900161020      *
059000161020     c                   end
059100161020      *-
059200161020     c                   end
059300161020     C     vatTRC        reade     fivat02l
059400161020     c                   end
059500161020      **
059600161020     C                   ENDSR
059700161020      * ?================================================================== */
059800161020     C*?   Lo scrive sul TIDP200F in parcheggio
059900161020      * ?================================================================== */
060000161020     C     WRT_TIDP2     BEGSR
060100161020      *
060200161020     c     i5parcel      chain     tidp200f
060300161020      *
060400161020     c                   eval      I2PARCEL    =    I5PARCEL
060500161020     c                   eval      I2SCANT     =    I5SCANT
060600161020     c                   eval      I2DEPCODE   =    I5DEPCODE
060700170907     c                   eval      I2DEPNAME   =    i5DEPCODE7
060800161020     c                   eval      I2DATTIM    =    I5DATTIM                    <<----
060900161020     c                   eval      I2ROUTE     =    I5ROUTE
061000161020     c                   eval      I2TOUR      =    I5TOUR
061100161020     c                   eval      I2PCODE     =    I5PCODE
061200161020     c                   eval      I2SERVICE   =    I5SERVICE
061300161020     c                   eval      I2CCOUNTRY  =    I5CCOUNTRY
061400161020     c                   eval      I2CONSZIP   =    I5CONSZIP
061500161020     c                   eval      I2ADDSER1   =    I5ADDSER1
061600161020     c                   eval      I2ADDSER2   =    I5ADDSER2
061700161020     c                   eval      I2ADDSER3   =    I5ADDSER3
061800161020     c                   eval      I2WEIGHT    =    I5WEIGHT
061900161020     c                   eval      I2CUSTREF   =    I5CUSTREF
062000161020     c                   eval      I2PODIMAG   =    I5PODIMAG
062100161020     c                   eval      I2RECNAME   =    I5RECNAME
062200161020     c                   eval      I2INFOTXT   =    I5INFOTXT
062300161020      *
062400161020     c                   if        %Found(TIDP200F)
062500161020     c                   update    TIDP2000
062600161020     c                   else
062700161020     c                   write     TIDP2000
062800161020     c                   end
062900161020      **
063000161020     C                   ENDSR
063100161020      * ?================================================================== */
063200060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
063300161020      * ?================================================================== */
063400161021     C     callFIEU18    BEGSR
063500060614      *
063600060615     c                   clear                   ok_18             1
063700060630     c                   clear                   msgerr           80
063800060620     c                   clear                   msg_vin_80
063900060615     c                   call      'FIEU18R'
064000161130     c                   parm                    FIEU18
064100060615     c                   parm                    ok_18
064200060620     c                   parm                    msgerr
064300060615
064400161020     c                   eval       se_errore = ok_18
064500060620     c                   eval      msg_vin_80 = msgerr
064600060612      *
064700060615     c                   Endsr
064800161020      * ?_______________________________________________________________    */
064900161020      *    ROUT iniziale
065000060615      * ?_______________________________________________________________    */
065100060615     C     *INZSR        BEGSR
065200160120      *
065300160120     C     Kvat4E        KLIST
065400160120     C                   KFLD                    vatTRC
065500160120     C                   KFLD                    vatNOT
065600160121      *
065700160121     C     kVAT01        KLIST
065800160121     C                   KFLD                    VABFGS
065900160121     C                   KFLD                    VABCCM
066000160121     C                   KFLD                    VABAAS
066100160121     C                   KFLD                    VABLNP
066200160121     C                   KFLD                    VABNRS
066300160121     C                   KFLD                    VABNSP
066400160121      *
066500160121     C     kVAB01        KLIST
066600160121     C                   KFLD                    vabCCM
066700160121     C                   KFLD                    vabRMN
066800160120      *
066900160120     C     KSosp_EDIVA   KLIST
067000160120     C                   KFLD                    vatAAS
067100160120     C                   KFLD                    vatLNP
067200160120     C                   KFLD                    vatNRS
067300160120     C                   KFLD                    vatNSP
067400160120      *
067500170907     C     key_VTG       KLIST
067600170907     C                   KFLD                    cldCOD
067700170907     C                   KFLD                    cldDEP
067800171212      *
067900171212     C     KTABel        KLIST
068000171212     C                   KFLD                    TBLKUT
068100171212     C                   KFLD                    TBLCOD
068200171212     C                   KFLD                    TBLKEY
068300160120      *
068400160120      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
068500991129     c     *ENTRY        PLIST
068600161124     C                   parm                    FIEU04DS40
068700161020     C                   parm                    se_errore
068800161020     C                   parm                    Msg_Vin_80
068900161020      **
069000971216      * Recupero data e ora
069100971216     C                   TIME                    WORA
069200991124     C                   TIME                    W0140
069300991124      * UDATE IN GGMMAAAA
069400991124     C                   MOVE      W0140         WDTGIO
069500991124      * UDATE IN AAAAMMGG
069600991124     C     *eur          MOVEL     WDTGIO        DATA_eur
069700991124     C     *iso          MOVEL     DATA_eur      dateu
069800171212      *
069900171212      *  Cod.BRT -- B.U.
070000171212     c                   movel     '023'         BU_brt            3
070100171212     C                   z-add     1             TBLKUT
070200171212     C                   movel     '3I'          TBLCOD
070300171212     C                   movel(p)  'DPD'         TBLKEY
070400171212     c     KTABel        chain     tabel00f
070500171212     c                   if        %Found(tabel00f)
070600171212     c                   movel     tbluni        ds3Idp
070700171212     c                   move      §3IBUCN       BU_brt
070800171212     c                   end
070900171212     C*------------------
071000160120      *
071100991124     C                   ENDSR
071200161020      * ?_______________________________________________________________    */
071300161020      *    Se errori NON blocca il programma
071400161020      * ?_______________________________________________________________    */
071500161020     C     *pssr         BEGSR
071600161020     C                   ENDSR     '*CANCL'
071700160120      *  ------------------------------------------------------------------ */
