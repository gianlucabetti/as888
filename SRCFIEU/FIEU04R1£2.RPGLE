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
002600171003      ** ------------------------------------------------------------------ */
002700161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
002800161124     D FIEU04DS40    E DS                  prefix(TTE_)
002900171003      ** ------------------------------------------------------------------ */
003000171003      *
003100161020     d vatds           ds
003200161020     d  vat_nrr              397    400b 0
003300171003      ** -------------------------------------------------------------------*****
003400000223     D W0140           S             14  0
003500991129     D WORA            S              6  0
003600991129     D WDTGIO          S              8  0
003700991129     D DATEU           S              8  0
003800991129     D DATA_eur        S               D   DATFMT(*eur)
003900160120     D DATA_iso        S               D   DATFMT(*iso)
004000161020     d dVATe         e ds
004100160120      *-------------------
004200160121     D OG143         E DS
004300160121      *-------------------
004400171003     D prmCPD10F     e DS                  extname(DPCDP10F)
004500171003     D prmLEG10F     e DS                  extname(DPLEG10F)
004600171003      *----------
004700171003     D DlegFLO       E DS
004800171003     D TISIEDEPDS    E DS
004900171003      *-------------------
005000160120     d Wdata8          DS
005100160120     d  dadata                 1      8  0
005200160120     d  adata                  9     16  0
005300160120     d  GioLav                17     21  0
005400160120      *
005500160120     D DepotParcel     S              4
005600171003      *  ______________________________________________________________     */
005700161020     D  se_errore      s              1
005800161020     D  Msg_Vin_80     s             80
005900060619     D Testa_Sk        s             10    DIM(50)
006000171003      *  ______________________________________________________________     */
006100161020     D   Digits        C                   '0123456789'
006200161020     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
006300161020     d Gomme_365       c                   365
006400161020     d Gomme_366       c                   366
006500161020     d Gomme_367       c                   367
006600171003      *  ______________________________________________________________     */
006700060612     c                   SETON                                        LR
006800161020      ** ===========
006900060612      ** Decodifica record a campi variabili
007000060612     c                   exsr      Decod_Record
007100160412      ** ===========
007200160412      **   x SCAN 18 PUDO  solo su nostro EXPORT  sotto tipologia 28/29 PUDO
007300161020     c                   if           i5SCANT  = 18
007400160412     c                   IF        (%subst(i5PARCEL:1:4) = '0844'  or
007500160412     c                              %subst(i5PARCEL:1:4) = '0845')     and
007600160412     c                             (%subst(i5INFOTXT:1:4) = '128:' or
007700170908     c                              %subst(i5INFOTXT:1:4) = '129:' or
007800170908     c                              %subst(i5INFOTXT:2:4) = '128:' or
007900170908     c                              %subst(i5INFOTXT:2:4) = '129:')
008000160412      **
008100160412      *   trasforma lo SCAN RICEVUTO in un ALTRO SCAN per descrivere il
008200160412     c                   exsr      SCAN_18_PUDO
008300161020      **
008400161020     c                   if        se_errore ='S'
008500161020     C                   eval       Msg_Vin_80 = 'Errori conv.SCAN 18 PUDO'
008600161020     c                   RETURN
008700161020     c                   endif
008800160412      **
008900160412     c                   END
009000161020     c                   endif
009100160120      ** ===========
009200160120      *** se arrivato uno  SCAN 05 si attiva la spedizione sul GEODATA
009300160120      **   in sospensione sul EDIVAB e se non lo trova lo scrive o aggiorna
009400160120      ** e NON è la ripetizione del nostro scan x quando facciamo l'export.
009500160120      **   ossia non è un 0844/0845 Italia
009600161020     c                   IF        i5SCANT  = 05
009700160120     c                               and
009800160120     c                              %subst(i5PARCEL:1:4) <> '0844'
009900160120     c                               and
010000160120     c                              %subst(i5PARCEL:1:4) <> '0845'
010100160120      ** ===========
010200160120     c                   exsr      SCAN_05
010300161021      **
010400161021     c                   if        se_errore ='S'
010500161021     C                   eval       Msg_Vin_80 = 'Errori conv.SCAN 05'
010600161021     c                   RETURN
010700161021     c                   endif
010800160120      ** ===========
010900060612     c                   else
011000060612
011100060612      *  con il record in canna scrive direttamente il VAB ed il VAT
011200161021     c                   exsr      callFIEU18
011300160121     c                   end
011400160121
011500161021      *  Problemi nella decodifica dei campi
011600161021     c                   if        se_errore<>'S'
011700060614      *  Record OK
011800160126      *  x gli SCAN 05 deve riportare quanto segnalato dalla routine SCAN_05
011900160126     c                   if        i5SCANT <> 05
012000160126     c                                or
012100160126     c                             (i5SCANT  = 05 and
012200160126     c                             (%subst(i5PARCEL:1:4) = '0844'
012300160126     c                               or
012400160126     c                              %subst(i5PARCEL:1:4) = '0845') )
012500161020     C                   eval       Msg_Vin_80 = *Blank
012600160126     c                   end
012700060614     c                   end
012800060612      **
012900161020     c                   RETURN
013000161020      * ?______________________________________________________________     */
013100161020      *?    Decodifica record a campi variabili
013200161020      * ?______________________________________________________________     */
013300161020     c     Decod_Record  Begsr
013400161020
013500161130      * ? Sposta dal tracciato TTE al vecchio FIEU18DS poichè al momento
013600161020      * ?  non vi è nulla di nuovo da aggiungere
013700161020      **
013800161130     c                   clear                   FIEU18
013900170907     c                   clear                   i5DEPCODE7        7
014000161020     c                   eval      I5PARCEL    = TTE_PARCELNR
014100170317      **
014200170317      ** nuovo depot può essere  lungo 7 anzichè 4 ei vecchio Depot di 4 sta a dx.
014300170317     c                   if        %len(TTE_DEPOT) = 7
014400171129     c                   movel     TTE_DEPOT     depot7            7
014500171129     c                   move      depot7        depot4            4
014600170317     c                   movel     depot4        i5DEPCODE
014700171129     c                   movel     depot7        i5DEPNAME
014800170317     c                   elseif    %len(TTE_DEPOT) = 4
014900161020     c                   eval      I5DEPCODE   = TTE_DEPOT
015000170317     c                   end
015100170907      *
015200170907     c                   movel     TTE_DEPOT     i5DEPCODE7        7
015300170317      **
015400161130     c                   movel     *zeros        I5DATTIM
015500161130     c                   movel     TTE_STDATETIMEI5DATTIM
015600161020     c                   eval      I5SERVICE   = TTE_SERVICECOD
015700161020     c                   eval      I5RECNAME   = TTE_RNAME
015800161020     c                   eval      I5INFOTXT   =
015900161020     c                             %editc(TTE_INFOCONT1:'X') + TTE_INFOCONT2
016000161020      **
016100161020      **  essendo in decagrammi ha quindi 2 decimali quindi basta allineare a destra
016200161020     c                   move      TTE_WEIGHT    I5WEIGHT
016300161020      **
016400161020     c                   movel     TTE_SCANCODE  I5SCANT
016500161020     c                   movel     TTE_DZIPCODE  I5CONSZIP
016600161020     c                   movel     TTE_REASONCOD1I5ADDSER1
016700161020     c                   movel     TTE_REASONCOD2I5ADDSER2
016800161020     c                   movel     TTE_REASONCOD3I5ADDSER3
016900171129      **  solo se pieno
017000171129     c                   if        TTE_PODIMAGREF <> *blank
017100171129     c                   movel     TTE_PODIMAGREFcampo7a           7
017200171129     c                   clear                   len7a             3 0
017300171129     c                   clear                   da                3 0
017400171129     c                   eval      len7a = %Len(%Trim(campo7a))
017500171129     c                   eval      da = 8 - len7a
017600171129     c                   move      *all'0'       podimag7          7
017700171129     c                   eval      %subst(podimag7:da:len7a)=
017800171129     c                             %subst(campo7a:1:len7a)
017900171129     c                   move      podimag7      I5PODIMAG
018000171129     c                   end
018100161020      **
018200161020     c                   Endsr
018300161020      * ?_______________________________________________________________    */
018400161020     C*      SCAN 18 X PARCEL SHOP
018500161020      * ?_______________________________________________________________    */
018600161020     C     SCAN_18_PUDO  BEGSR
018700161020      *
018800161020      * ------ passa la DS completa e riceverà lo SCAN sostitutivo
018900161020      *        relativo all'evento del PUDO. Controlla e cancella
019000161020      *        eventuale SCAN 13 in attesa che avrebbe dato automaticamente
019100161020      *        x consegnata la spedizione.
019200161020     c                   call      'FIEU02R18'
019300161130     c                   parm                    FIEU18
019400161020      *
019500161020      *  se torna con lo stesso SCAN 18 senza averlo sostituito
019600161020     c                   if        i5SCANT =  18
019700161020     c                   Eval      se_errore = 'S'
019800161020     c                   End
019900161020      *
020000161020     c                   Endsr
020100161020      * ?_______________________________________________________________    */
020200161020      *   ARRIVATO uno SCAN 05 controlla bolla in sospensione
020300161020      * ?_______________________________________________________________    */
020400161020     C     SCAN_05       BEGSR
020500161020      *
020600170907     c                   clear                   wri_SCAN5         1
020700170907      *
020800161020      * registra il 1° arrivo dello SCAN 05
020900161020     c                   exsr      check_WFIEU00
021000161020      *
021100161020      **  se non è ancora stato confermato il CMR
021200161020     C                   eval        vatTRC = 'E'
021300161020     C                   eval        vatNOT = i5PARCEL
021400161020     C     Kvat4E        setll     edivat4l
021500161020     C     vatTRC        reade     edivat4l
021600161020      *--
021700161020     c                   IF        not %EoF(edivat4l)
021800161020     c                                 and
021900161020     C                             I5PARCEL = %SUBST(vatNOT:1:14)
022000161020      *--
022100161020      *-   Se era in attesa dello SCAN05 di collo PARTITO
022200161020     c                   if           vatCMR = GEO_in_Sospeso
022300161020     C                   eval       Msg_Vin_80 = 'Già su EDIVAB in Sospeso'
022400161020      * ==========
022500161020      *  imposta il CMR x la conferma
022600161020     c                   exsr      CAMBIA_CMR
022700161020     c                   exsr      UPD_EDIVA
022800161020      *
022900161020     c                   else
023000161020      *-  Se c'è su EDIVAB
023100161020      *--  ed era già stato ATTIVATO (quindi non più in sospensione)
023200161020     C                   eval      Msg_Vin_80 = 'SCAN 05 aggiorna solo -
023300161020     c                             serv.code e peso'
023400161020     c                   clear                   new_CMR
023500161020     C                   Exsr      UPD_EDIVA
023600161020     c                   end
023700161020      *--
023800161020     c                   else
023900161020      *--
024000161020      ** non presente su EDIVAB/T   (su FIVAB ? o FNBLP ? oppure non c'era ?)
024100161020     c                   exsr      NON_SU_EDIVA
024200161020      *--
024300161020      *-  Se non c'è da NESSUNA parte, lo scrivo Parcheggiandolo
024400161020      *--   sul TIDP200F in attesa che arrivi poi la BOLLA sul GEODATA:
024500161020     c                   if         in_FNBLP <> 'S'
024600161020     c                                 and
024700161020     c                              in_FIVAB <> 'S'
024800170907     c                   move      'S'           wri_SCAN5
024900170907     c********           exsr      WRT_TIDP2
025000161020     c                   end
025100161020      *--
025200161020     c                   end
025300170907      **
025400170907      * -----------------                  -------------------
025500170907      **  testa se VTG
025600170907     c                   clear                   parcel_VTG        1
025700170907     c                   movel     I5PARCEL      Parcel14         14
025800170907     C                   movel(p)  'VTG'         cldCOD
025900170907     C                   movel     Parcel14      cldDEP
026000170907     c     key_VTG       setll     tncld00F
026100170907     c     key_VTG       reade     tncld00F
026200170907     c                   dow       not %EoF(tncld00F)
026300170907     c                   if        dateu >= cldDDA and
026400170907     c                             dateu <= cldADA
026500170907      **  Se è un collo VTG
026600170907     c                   movel     'S'           parcel_VTG
026700170907     c                   leave
026800170907     c                   end
026900170907     c     key_VTG       reade     tncld00F
027000170907     c                   endDO
027100170907      **
027200170907      **  Deve scrivere lo SCAN"05" in attesa oppure sempre per i VTG
027300170907     c                   if        wri_SCAN5  = 'S' or
027400170907     c                             parcel_VTG = 'S'
027500170907     c                   exsr      WRT_TIDP2
027600170907     c                   end
027700161020      **
027800161020     C                   ENDSR
027900161020      * ?_______________________________________________________________    */
028000161020      *    Rileva la prima volta che arriva lo SCAN 05
028100161020      * ?_______________________________________________________________    */
028200161020     C     check_WFIEU00 BEGSR
028300161020     C*
028400161020     c     i5PARCEL      chain     wfieu00f
028500161020     c                   if        %Found(wfieu00f)
028600161020     c                   if        F00DATSC5 = 0
028700161020     c                   eval      F00DATSC5 = dateu
028800161020     c                   eval      F00ORASC5 = wora
028900161020     c                   update    fieu000
029000161020     c                   end
029100161020     c                   else
029200161020     c                   clear                   fieu000
029300161020     c                   eval      F00PARCEL = i5PARCEL
029400161020     c                   eval      F00DATSC5 = dateu
029500161020     c                   eval      F00ORASC5 = wora
029600161020     c                   write     fieu000
029700161020     c                   end
029800161020     C*
029900161020     C                   ENDSR
030000161020      * ?_______________________________________________________________    */
030100161020      *  si compone il CMR correttamente eliminando "IN SOSPESO"
030200161020      * ?_______________________________________________________________    */
030300161020     C     CAMBIA_CMR    BEGSR
030400161020      *
030500161020     c                   clear                   new_CMR          35
030600161020     c                   movel     I5DATTIM      I5DATAw           8 0
030700161020      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
030800161020     c     *iso          movel     DATEU         DATA_iso
030900161020    2c                   if        I5DATAw >0
031000161020     c                   TEST(D)                 I5DATAw                99
031100161020     c  n99*iso          movel     I5DATAw       DATA_iso
031200161020     c                   end
031300161020      *
031400161020      *  Da quale Depot è partito il pacco (aggiunge i giorni di viaggio)
031500161020     c                   exsr      gg_di_VIAGGIO
031600161020      *
031700161020      *   Se la data risultante è inferiore alla UDATE forza UDATE
031800161020     c                   move      data_iso      data_iso8         8 0
031900161020     c                   if        data_iso8 < dateu
032000161020     c                   move      dateu         data_iso8
032100161020     c                   end
032200161020      **
032300161020      * IMPOSTA IL CMR x LA SPEDIZIONE
032400161020     c                   eval      new_CMR = 'GEO_' + %editc(data_iso8:'Y')
032500161020      *
032600161020      **  Controlla se si tratta di gomme per poterle evidenziare nel CMR
032700161020     c                   if        i5SERVICE = Gomme_365 or
032800161020     c                             i5SERVICE = Gomme_366 or
032900161020     c                             i5SERVICE = Gomme_367
033000161020     c                   eval        new_CMR = %trim(new_CMR) + '_GOMME'
033100161020     c                   end
033200161020      *
033300161020     c                   Endsr
033400161020      * ?_______________________________________________________________    */
033500161020     C*  in base al Depot di provenienza determina i giorni di viaggio
033600161020      * ?_______________________________________________________________    */
033700161020     C     gg_di_VIAGGIO BEGSR
033800161020      *
033900161020      ** imposta il default
034000161020     c                   z-add     1             gg_incr           3 0
034100161020     c                   movel     I5DEPCODE     Start_depot       4
034200161020     c     ritenta       tag
034300161020      *
034400161020      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
034500161020      *   e se non fosse presente con i primi 4 caratteri del Parcel
034600161020     c                   if        Start_depot <> *blank
034700161020     c                   movel     Start_depot   DepotParcel
034800161020     c                   z-add     1             giro_nr           3 0
034900161020     c                   Else
035000161020     c                   z-add     2             giro_nr
035100161020     c                   movel     i5PARCEL      DepotParcel
035200161020     c                   End
035300161020      *
035400161020      *
035500171003      *  aggancia Versione/Anagrafica e Legame
035600171003      *        TISIEDEP
035700171003      *        I_DATAVER      1     8 S  0 DATA VERSIONE/0=UDATE
035800171003      *        I_DEPOT4       9     4 S  0 =0 SE PASSSATO IL DEPOT A 7
035900171003      *        I_DEPOT7      13     7 S  0 =0 SE PASSSATO IL DEPOT A 4
036000171003      *        O_FLGVERS     20     1 A    TEST VERSIONE 0=OK/1=ERR/BLK=NON TESTATO
036100171003      *        O_FLGCAPP     21     1 A    TEST CAPPARIO 0=OK/1=ERR/BLK=NON TESTATO
036200171003      *        O_FLGLEGA     22     1 A    TEST LEGAMI   0=OK/1=ERR/BLK=NON TESTATO
036300171003      *
036400171003     c                   clear                   tisieDEPds
036500171003     c                   movel     DepotParcel   I_DEPOT4
036600171003     c                   call      'TISIEDEPR'
036700171003     c                   parm                    tisieDEPds
036800171003     c                   parm                    prmCPD10F
036900171003     c                   parm                    prmLEG10F
037000171003      *  se trovato TUTTO OK:
037100171003     c                   if        O_FLGVERS = '0' and
037200171003     c                             O_FLGCAPP = '0' and
037300171003     c                             O_FLGLEGA = '0'
037400171003      *
037500171003     c                   eval      DlegFLO = legFLO
037600171003     c                   if          §legFLOGGI <> *blank and
037700171003     c                               §legFLOGGI <> '000'  and
037800171003     c                               %check(digits:§legFLOGGI) = 0
037900171003     c                   move      §legFLOGGI    gg_incr           3 0
038000171003     c                   end
038100171003      *
038200171003     c                   endif
038300171003      *
038400171003     c                   if        §legFLOLNP = '000' or
038500171003     c                             §legFLOLNP = *blank
038600171003     c                   if          giro_nr = 1
038700171003     c                   clear                   Start_depot
038800171003     c                   goto      ritenta
038900171003     c                   End
039000171003     c                   End
039100171003      *
039200171003      *=============
039300161020      *  se Non c'è incremento
039400161020     c                   if        gg_incr = 0
039500161020     c                   exsr      test_gg_lav
039600161020     c                   else
039700161020      *  aggiunge x giorni (ma solo lavorativi)
039800161020     c                   do        gg_incr       giorni            3 0
039900161020     c                   adddur    1:*D          data_iso
040000161020     c                   exsr      test_gg_lav
040100161020     c                   enddo
040200161020     c                   endif
040300161020      *
040400161020     c                   Endsr
040500161020      * ?_______________________________________________________________    */
040600161020     C*  test sui GG lavorativi altrimenti CERCA fino al primo lavorativo
040700161020      * ?_______________________________________________________________    */
040800161020     C     test_gg_lav   BEGSR
040900161020      *
041000161020      * -----------
041100161020      * Cerca definitivamente il D-day lavorativo
041200161020      *  esclude sia il sabato che la domenica
041300161020      * -----------
041400161020     c                   do        *hival
041500161020     c                   move      data_iso      dadata
041600161020     c                   move      data_iso      adata
041700161020     c                   CALL      'XSRLAV8'
041800161020     c                   PARM                    Wdata8
041900161020      * se corrisponde ho trovato il D-Day
042000161020     c                   if        giolav <> 0
042100161020     c                   leave
042200161020     c                   else
042300161020      * altrimenti continuo a cercare il primo giorno lavorativo valido.
042400161020     c     data_iso      adddur    1:*d          data_iso
042500161020     c                   iter
042600161020     c                   endif
042700161020     c                   enddo
042800161020      *
042900161020     c                   Endsr
043000161020      * ?================================================================== */
043100161020     C*?       ERA su EDIVAB in SOSPENSIONE in attesa dello SCAN 05
043200161020      * ?================================================================== */
043300161020     C     UPD_EDIVA     BEGSR
043400161020      *
043500161020      *  per chiave bolla aggiorna la bolla in SOSPENZIONE che adesso
043600161020      *  risulta partita con l'IGATEAVIS ricevuto (Manifest)
043700161020     C     KSosp_EDIVA   setll     EDIVAT3L
043800161020     C     KSosp_EDIVA   reade     EDIVAT3L
043900161020      *
044000161020     c                   DOw       not %EoF(EDIVAT3L)
044100161020      *
044200161020      *  sul record del Parcel/Service Code sostituisce il SERVICE CODE con quello
044300161020      *  passato nello SCAN 05
044400161020     C                   if        I5PARCEL = %SUBST(vatNOT:1:14)
044500161020     c                                  and
044600161020     C                               vatTRC = 'E'
044700161020     c                                  and
044800161020     C                             I5SERVICE <> *zeros
044900161020     c                   eval      dVATe     = vatNOT
045000161020     c                   move      i5SERVICE     §VATESCD
045100161020     c                   eval      vatNOT    = dVATe
045200161020     c                   End
045300161020      *
045400161020      *  sempre il nuovo CMR GEO_Data
045500161020     C                   if        new_CMR  <> *blank
045600161020     c                   eval      vatCMR  =  new_CMR
045700161020     c                   End
045800161020     c                   Update    EDIVAT00
045900161020      *
046000161020     C     KSosp_EDIVA   reade     EDIVAT3L
046100161020     c                   EndDO
046200161020      * ==========
046300161020      *  poi allinea anche la testata bolla del VAB
046400161020      * ==========
046500161020     C     KSosp_EDIVA   chain     EDIVAB2L
046600161020     c                   if        %Found(EDIVAB2L)
046700161020      *
046800161020      * Se passato il PESO lo sostituisce
046900161020     c                   if        I5WEIGHT > 0
047000161020     c                   Z-ADD     I5WEIGHT      VABPKB
047100161020      * CORREZIONI:
047200161020      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
047300161020     c                   IF        vabPKB > 31,5
047400161020     c                   eval           vabPKB = 30
047500161020     c                   elseIF    vabpkb =0
047600161020     c                   eval           vabpkb = 0,1
047700161020     c                   END
047800161020     c                   End
047900161020      *
048000161020     C                   if        new_CMR  <> *blank
048100161020     c                   eval           vabCMR = new_CMR
048200161020     c                   End
048300161020     c                   eval      vabSCL = 'G'
048400161020     c                   Update    EDIVAB00
048500161020     c                   End
048600161020      *
048700161020     c                   Endsr
048800161020      * ?_______________________________________________________________    */
048900161020     C*?   NON ERA su EDIVAB   cerca su FNBLP e su FIVAB
049000161020      * ?_______________________________________________________________    */
049100161020     C     NON_SU_EDIVA  BEGSR
049200161020      *
049300161020     c                   clear                   in_FNBLP          1
049400161020     c                   clear                   in_FIVAB          1
049500161020      **
049600171003      **  se già confermato in Partenza
049700171003     C     i5PARCEL      chain     fipnd03l
049800171003     c                   IF        %Found(fipnd03l)
049900171003     c                   movel     i5PARCEL      DepotParcel
050000171003      **
050100161020      *   controlla che sia DPD con il Network e non con la linea
050200171003     c     pndLNP        chain     azorg01l
050300161020     c                   if        %Found(azorg01l)
050400161020     c                   eval          OG143  =  orgDE3
050500161020     c                   if        §OGNTW = 'DPD'
050600161020     c                   eval          in_FNBLP = 'S'
050700161020     C                   eval      Msg_Vin_80 = 'Già su BLP Confermato'
050800161020     c                   LEAVESR
050900161020     c                   end
051000161020     c                   end
051100161020      **
051200161020     c                   endIF
051300161020      ** ---------
051400161020      **  se non ancora confermato da Cliente
051500161020     C                   eval        vatTRC = 'E'
051600161020     C                   eval        vatNOT = i5PARCEL
051700161020     C     Kvat4E        setll     fivat02l
051800161020     C     vatTRC        reade     fivat02l
051900161020      *-
052000161020     c                   dow         not %EoF(fivat02l)
052100161020      *-
052200161020     c* imposta DS x reperire il Parcel
052300161020     c                   movel     vatNOT        dVATe
052400161020      *-
052500161020      *- se superata la chiave parcel esce dal ciclo x evitare letture inutili.
052600161020     C                   if           §vatEPRN > i5PARCEL
052700161020     c                   leave
052800161020     c                   end
052900161020      *-
053000161020     c* se la chiave è superiore esce dal ciclo
053100161020     C                   if          i5PARCEL = §vatEPRN
053200161020      *-
053300161020     c     vatLNP        chain     azorg01l
053400161020     c                   if        %Found(azorg01l)
053500161020     c                   eval      OG143 = orgDE3
053600161020      *-
053700161020     c                   if        §OGNTW = 'DPD'
053800161020      * Deve aggiornare il SERVICE CODE con l'ultimo arrivato
053900161020     c     vat_nrr       chain     fivat00f
054000161020     c                   if        %found(fivat00f)
054100161020     c                   eval      dVATe     = vatNOT
054200161020     c                   move      i5SERVICE     §VATESCD
054300161020     c                   eval      vatNOT    = dVATe
054400161020     c                   update    fivatFIS
054500161020     c                   end
054600161020      *
054700161020     c                   eval        vabCCM = vatCCM
054800161020     c                   move      i5Parcel      vabRMN
054900161020     c     kvab01        chain     fivab01l
055000161020     c                   if        %Found(fivab01l)
055100161020     C                   eval      Msg_Vin_80 =
055200161020     C                             'Aggiorna SOLO Peso/Serv.Code su FIVAB'
055300161020     c                   eval          in_FIVAB = 'S'
055400161020      *
055500161020      * Se passato il PESO lo sostituisce
055600161020     c                   if        I5WEIGHT > 0
055700161020     c                   Z-ADD     I5WEIGHT      VABPKB
055800161020      * CORREZIONI:
055900161020      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
056000161020     c                   IF        vabPKB > 31,5
056100161020     c                   eval           vabPKB = 30
056200161020     c                   elseIF    vabpkb =0
056300161020     c                   eval           vabpkb = 0,1
056400161020     c                   END
056500161020     c                   End
056600161020      *
056700161020     c                   update    fivab000
056800161020     c                   LEAVESR
056900161020     c                   end
057000161020     c                   end
057100161020      *
057200161020     c                   end
057300161020      *-
057400161020     c                   end
057500161020     C     vatTRC        reade     fivat02l
057600161020     c                   end
057700161020      **
057800161020     C                   ENDSR
057900161020      * ?================================================================== */
058000161020     C*?   Lo scrive sul TIDP200F in parcheggio
058100161020      * ?================================================================== */
058200161020     C     WRT_TIDP2     BEGSR
058300161020      *
058400161020     c     i5parcel      chain     tidp200f
058500161020      *
058600161020     c                   eval      I2PARCEL    =    I5PARCEL
058700161020     c                   eval      I2SCANT     =    I5SCANT
058800161020     c                   eval      I2DEPCODE   =    I5DEPCODE
058900170907     c                   eval      I2DEPNAME   =    i5DEPCODE7
059000161020     c                   eval      I2DATTIM    =    I5DATTIM                    <<----
059100161020     c                   eval      I2ROUTE     =    I5ROUTE
059200161020     c                   eval      I2TOUR      =    I5TOUR
059300161020     c                   eval      I2PCODE     =    I5PCODE
059400161020     c                   eval      I2SERVICE   =    I5SERVICE
059500161020     c                   eval      I2CCOUNTRY  =    I5CCOUNTRY
059600161020     c                   eval      I2CONSZIP   =    I5CONSZIP
059700161020     c                   eval      I2ADDSER1   =    I5ADDSER1
059800161020     c                   eval      I2ADDSER2   =    I5ADDSER2
059900161020     c                   eval      I2ADDSER3   =    I5ADDSER3
060000161020     c                   eval      I2WEIGHT    =    I5WEIGHT
060100161020     c                   eval      I2CUSTREF   =    I5CUSTREF
060200161020     c                   eval      I2PODIMAG   =    I5PODIMAG
060300161020     c                   eval      I2RECNAME   =    I5RECNAME
060400161020     c                   eval      I2INFOTXT   =    I5INFOTXT
060500161020      *
060600161020     c                   if        %Found(TIDP200F)
060700161020     c                   update    TIDP2000
060800161020     c                   else
060900161020     c                   write     TIDP2000
061000161020     c                   end
061100161020      **
061200161020     C                   ENDSR
061300161020      * ?================================================================== */
061400060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
061500161020      * ?================================================================== */
061600161021     C     callFIEU18    BEGSR
061700060614      *
061800060615     c                   clear                   ok_18             1
061900060630     c                   clear                   msgerr           80
062000060620     c                   clear                   msg_vin_80
062100060615     c                   call      'FIEU18R'
062200161130     c                   parm                    FIEU18
062300060615     c                   parm                    ok_18
062400060620     c                   parm                    msgerr
062500060615
062600161020     c                   eval       se_errore = ok_18
062700060620     c                   eval      msg_vin_80 = msgerr
062800060612      *
062900060615     c                   Endsr
063000161020      * ?_______________________________________________________________    */
063100161020      *    ROUT iniziale
063200060615      * ?_______________________________________________________________    */
063300060615     C     *INZSR        BEGSR
063400160120      *
063500160120     C     Kvat4E        KLIST
063600160120     C                   KFLD                    vatTRC
063700160120     C                   KFLD                    vatNOT
063800160121      *
063900160121     C     kVAT01        KLIST
064000160121     C                   KFLD                    VABFGS
064100160121     C                   KFLD                    VABCCM
064200160121     C                   KFLD                    VABAAS
064300160121     C                   KFLD                    VABLNP
064400160121     C                   KFLD                    VABNRS
064500160121     C                   KFLD                    VABNSP
064600160121      *
064700160121     C     kVAB01        KLIST
064800160121     C                   KFLD                    vabCCM
064900160121     C                   KFLD                    vabRMN
065000160120      *
065100160120     C     KSosp_EDIVA   KLIST
065200160120     C                   KFLD                    vatAAS
065300160120     C                   KFLD                    vatLNP
065400160120     C                   KFLD                    vatNRS
065500160120     C                   KFLD                    vatNSP
065600160120      *
065700170907     C     key_VTG       KLIST
065800170907     C                   KFLD                    cldCOD
065900170907     C                   KFLD                    cldDEP
066000160120      *
066100160120      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
066200991129     c     *ENTRY        PLIST
066300161124     C                   parm                    FIEU04DS40
066400161020     C                   parm                    se_errore
066500161020     C                   parm                    Msg_Vin_80
066600161020      **
066700971216      * Recupero data e ora
066800971216     C                   TIME                    WORA
066900991124     C                   TIME                    W0140
067000991124      * UDATE IN GGMMAAAA
067100991124     C                   MOVE      W0140         WDTGIO
067200991124      * UDATE IN AAAAMMGG
067300991124     C     *eur          MOVEL     WDTGIO        DATA_eur
067400991124     C     *iso          MOVEL     DATA_eur      dateu
067500160120      *
067600991124     C                   ENDSR
067700161020      * ?_______________________________________________________________    */
067800161020      *    Se errori NON blocca il programma
067900161020      * ?_______________________________________________________________    */
068000161020     C     *pssr         BEGSR
068100161020     C                   ENDSR     '*CANCL'
068200160120      *  ------------------------------------------------------------------ */
