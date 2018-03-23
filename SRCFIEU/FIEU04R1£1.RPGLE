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
014400170317     c                   movel     TTE_DEPOT     campo7            7
014500170317     c                   move      campo7        depot4            4
014600170317     c                   movel     depot4        i5DEPCODE
014700170317     c                   elseif    %len(TTE_DEPOT) = 4
014800161020     c                   eval      I5DEPCODE   = TTE_DEPOT
014900170317     c                   end
015000170907      *
015100170907     c                   movel     TTE_DEPOT     i5DEPCODE7        7
015200170317      **
015300161130     c                   movel     *zeros        I5DATTIM
015400161130     c                   movel     TTE_STDATETIMEI5DATTIM
015500161020     c                   eval      I5SERVICE   = TTE_SERVICECOD
015600161020     c                   eval      I5RECNAME   = TTE_RNAME
015700161020     c                   eval      I5INFOTXT   =
015800161020     c                             %editc(TTE_INFOCONT1:'X') + TTE_INFOCONT2
015900161020      **
016000161020      **  essendo in decagrammi ha quindi 2 decimali quindi basta allineare a destra
016100161020     c                   move      TTE_WEIGHT    I5WEIGHT
016200161020      **
016300161020     c                   movel     TTE_SCANCODE  I5SCANT
016400161020     c                   movel     TTE_DZIPCODE  I5CONSZIP
016500161020     c                   movel     TTE_REASONCOD1I5ADDSER1
016600161020     c                   movel     TTE_REASONCOD2I5ADDSER2
016700161020     c                   movel     TTE_REASONCOD3I5ADDSER3
016800161020     c                   movel     TTE_PODIMAGREFI5PODIMAG
016900161020      **
017000161020     c                   Endsr
017100161020      * ?_______________________________________________________________    */
017200161020     C*      SCAN 18 X PARCEL SHOP
017300161020      * ?_______________________________________________________________    */
017400161020     C     SCAN_18_PUDO  BEGSR
017500161020      *
017600161020      * ------ passa la DS completa e riceverà lo SCAN sostitutivo
017700161020      *        relativo all'evento del PUDO. Controlla e cancella
017800161020      *        eventuale SCAN 13 in attesa che avrebbe dato automaticamente
017900161020      *        x consegnata la spedizione.
018000161020     c                   call      'FIEU02R18'
018100161130     c                   parm                    FIEU18
018200161020      *
018300161020      *  se torna con lo stesso SCAN 18 senza averlo sostituito
018400161020     c                   if        i5SCANT =  18
018500161020     c                   Eval      se_errore = 'S'
018600161020     c                   End
018700161020      *
018800161020     c                   Endsr
018900161020      * ?_______________________________________________________________    */
019000161020      *   ARRIVATO uno SCAN 05 controlla bolla in sospensione
019100161020      * ?_______________________________________________________________    */
019200161020     C     SCAN_05       BEGSR
019300161020      *
019400170907     c                   clear                   wri_SCAN5         1
019500170907      *
019600161020      * registra il 1° arrivo dello SCAN 05
019700161020     c                   exsr      check_WFIEU00
019800161020      *
019900161020      **  se non è ancora stato confermato il CMR
020000161020     C                   eval        vatTRC = 'E'
020100161020     C                   eval        vatNOT = i5PARCEL
020200161020     C     Kvat4E        setll     edivat4l
020300161020     C     vatTRC        reade     edivat4l
020400161020      *--
020500161020     c                   IF        not %EoF(edivat4l)
020600161020     c                                 and
020700161020     C                             I5PARCEL = %SUBST(vatNOT:1:14)
020800161020      *--
020900161020      *-   Se era in attesa dello SCAN05 di collo PARTITO
021000161020     c                   if           vatCMR = GEO_in_Sospeso
021100161020     C                   eval       Msg_Vin_80 = 'Già su EDIVAB in Sospeso'
021200161020      * ==========
021300161020      *  imposta il CMR x la conferma
021400161020     c                   exsr      CAMBIA_CMR
021500161020     c                   exsr      UPD_EDIVA
021600161020      *
021700161020     c                   else
021800161020      *-  Se c'è su EDIVAB
021900161020      *--  ed era già stato ATTIVATO (quindi non più in sospensione)
022000161020     C                   eval      Msg_Vin_80 = 'SCAN 05 aggiorna solo -
022100161020     c                             serv.code e peso'
022200161020     c                   clear                   new_CMR
022300161020     C                   Exsr      UPD_EDIVA
022400161020     c                   end
022500161020      *--
022600161020     c                   else
022700161020      *--
022800161020      ** non presente su EDIVAB/T   (su FIVAB ? o FNBLP ? oppure non c'era ?)
022900161020     c                   exsr      NON_SU_EDIVA
023000161020      *--
023100161020      *-  Se non c'è da NESSUNA parte, lo scrivo Parcheggiandolo
023200161020      *--   sul TIDP200F in attesa che arrivi poi la BOLLA sul GEODATA:
023300161020     c                   if         in_FNBLP <> 'S'
023400161020     c                                 and
023500161020     c                              in_FIVAB <> 'S'
023600170907     c                   move      'S'           wri_SCAN5
023700170907     c********           exsr      WRT_TIDP2
023800161020     c                   end
023900161020      *--
024000161020     c                   end
024100170907      **
024200170907      * -----------------                  -------------------
024300170907      **  testa se VTG
024400170907     c                   clear                   parcel_VTG        1
024500170907     c                   movel     I5PARCEL      Parcel14         14
024600170907     C                   movel(p)  'VTG'         cldCOD
024700170907     C                   movel     Parcel14      cldDEP
024800170907     c     key_VTG       setll     tncld00F
024900170907     c     key_VTG       reade     tncld00F
025000170907     c                   dow       not %EoF(tncld00F)
025100170907     c                   if        dateu >= cldDDA and
025200170907     c                             dateu <= cldADA
025300170907      **  Se è un collo VTG
025400170907     c                   movel     'S'           parcel_VTG
025500170907     c                   leave
025600170907     c                   end
025700170907     c     key_VTG       reade     tncld00F
025800170907     c                   endDO
025900170907      **
026000170907      **  Deve scrivere lo SCAN"05" in attesa oppure sempre per i VTG
026100170907     c                   if        wri_SCAN5  = 'S' or
026200170907     c                             parcel_VTG = 'S'
026300170907     c                   exsr      WRT_TIDP2
026400170907     c                   end
026500161020      **
026600161020     C                   ENDSR
026700161020      * ?_______________________________________________________________    */
026800161020      *    Rileva la prima volta che arriva lo SCAN 05
026900161020      * ?_______________________________________________________________    */
027000161020     C     check_WFIEU00 BEGSR
027100161020     C*
027200161020     c     i5PARCEL      chain     wfieu00f
027300161020     c                   if        %Found(wfieu00f)
027400161020     c                   if        F00DATSC5 = 0
027500161020     c                   eval      F00DATSC5 = dateu
027600161020     c                   eval      F00ORASC5 = wora
027700161020     c                   update    fieu000
027800161020     c                   end
027900161020     c                   else
028000161020     c                   clear                   fieu000
028100161020     c                   eval      F00PARCEL = i5PARCEL
028200161020     c                   eval      F00DATSC5 = dateu
028300161020     c                   eval      F00ORASC5 = wora
028400161020     c                   write     fieu000
028500161020     c                   end
028600161020     C*
028700161020     C                   ENDSR
028800161020      * ?_______________________________________________________________    */
028900161020      *  si compone il CMR correttamente eliminando "IN SOSPESO"
029000161020      * ?_______________________________________________________________    */
029100161020     C     CAMBIA_CMR    BEGSR
029200161020      *
029300161020     c                   clear                   new_CMR          35
029400161020     c                   movel     I5DATTIM      I5DATAw           8 0
029500161020      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
029600161020     c     *iso          movel     DATEU         DATA_iso
029700161020    2c                   if        I5DATAw >0
029800161020     c                   TEST(D)                 I5DATAw                99
029900161020     c  n99*iso          movel     I5DATAw       DATA_iso
030000161020     c                   end
030100161020      *
030200161020      *  Da quale Depot è partito il pacco (aggiunge i giorni di viaggio)
030300161020     c                   exsr      gg_di_VIAGGIO
030400161020      *
030500161020      *   Se la data risultante è inferiore alla UDATE forza UDATE
030600161020     c                   move      data_iso      data_iso8         8 0
030700161020     c                   if        data_iso8 < dateu
030800161020     c                   move      dateu         data_iso8
030900161020     c                   end
031000161020      **
031100161020      * IMPOSTA IL CMR x LA SPEDIZIONE
031200161020     c                   eval      new_CMR = 'GEO_' + %editc(data_iso8:'Y')
031300161020      *
031400161020      **  Controlla se si tratta di gomme per poterle evidenziare nel CMR
031500161020     c                   if        i5SERVICE = Gomme_365 or
031600161020     c                             i5SERVICE = Gomme_366 or
031700161020     c                             i5SERVICE = Gomme_367
031800161020     c                   eval        new_CMR = %trim(new_CMR) + '_GOMME'
031900161020     c                   end
032000161020      *
032100161020     c                   Endsr
032200161020      * ?_______________________________________________________________    */
032300161020     C*  in base al Depot di provenienza determina i giorni di viaggio
032400161020      * ?_______________________________________________________________    */
032500161020     C     gg_di_VIAGGIO BEGSR
032600161020      *
032700161020      ** imposta il default
032800161020     c                   z-add     1             gg_incr           3 0
032900161020     c                   movel     I5DEPCODE     Start_depot       4
033000161020     c     ritenta       tag
033100161020      *
033200161020      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
033300161020      *   e se non fosse presente con i primi 4 caratteri del Parcel
033400161020     c                   if        Start_depot <> *blank
033500161020     c                   movel     Start_depot   DepotParcel
033600161020     c                   z-add     1             giro_nr           3 0
033700161020     c                   Else
033800161020     c                   z-add     2             giro_nr
033900161020     c                   movel     i5PARCEL      DepotParcel
034000161020     c                   End
034100161020      *
034200161020      *
034300171003      *  aggancia Versione/Anagrafica e Legame
034400171003      *        TISIEDEP
034500171003      *        I_DATAVER      1     8 S  0 DATA VERSIONE/0=UDATE
034600171003      *        I_DEPOT4       9     4 S  0 =0 SE PASSSATO IL DEPOT A 7
034700171003      *        I_DEPOT7      13     7 S  0 =0 SE PASSSATO IL DEPOT A 4
034800171003      *        O_FLGVERS     20     1 A    TEST VERSIONE 0=OK/1=ERR/BLK=NON TESTATO
034900171003      *        O_FLGCAPP     21     1 A    TEST CAPPARIO 0=OK/1=ERR/BLK=NON TESTATO
035000171003      *        O_FLGLEGA     22     1 A    TEST LEGAMI   0=OK/1=ERR/BLK=NON TESTATO
035100171003      *
035200171003     c                   clear                   tisieDEPds
035300171003     c                   movel     DepotParcel   I_DEPOT4
035400171003     c                   call      'TISIEDEPR'
035500171003     c                   parm                    tisieDEPds
035600171003     c                   parm                    prmCPD10F
035700171003     c                   parm                    prmLEG10F
035800171003      *  se trovato TUTTO OK:
035900171003     c                   if        O_FLGVERS = '0' and
036000171003     c                             O_FLGCAPP = '0' and
036100171003     c                             O_FLGLEGA = '0'
036200171003      *
036300171003     c                   eval      DlegFLO = legFLO
036400171003     c                   if          §legFLOGGI <> *blank and
036500171003     c                               §legFLOGGI <> '000'  and
036600171003     c                               %check(digits:§legFLOGGI) = 0
036700171003     c                   move      §legFLOGGI    gg_incr           3 0
036800171003     c                   end
036900171003      *
037000171003     c                   endif
037100171003      *
037200171003     c                   if        §legFLOLNP = '000' or
037300171003     c                             §legFLOLNP = *blank
037400171003     c                   if          giro_nr = 1
037500171003     c                   clear                   Start_depot
037600171003     c                   goto      ritenta
037700171003     c                   End
037800171003     c                   End
037900171003      *
038000171003      *=============
038100161020      *  se Non c'è incremento
038200161020     c                   if        gg_incr = 0
038300161020     c                   exsr      test_gg_lav
038400161020     c                   else
038500161020      *  aggiunge x giorni (ma solo lavorativi)
038600161020     c                   do        gg_incr       giorni            3 0
038700161020     c                   adddur    1:*D          data_iso
038800161020     c                   exsr      test_gg_lav
038900161020     c                   enddo
039000161020     c                   endif
039100161020      *
039200161020     c                   Endsr
039300161020      * ?_______________________________________________________________    */
039400161020     C*  test sui GG lavorativi altrimenti CERCA fino al primo lavorativo
039500161020      * ?_______________________________________________________________    */
039600161020     C     test_gg_lav   BEGSR
039700161020      *
039800161020      * -----------
039900161020      * Cerca definitivamente il D-day lavorativo
040000161020      *  esclude sia il sabato che la domenica
040100161020      * -----------
040200161020     c                   do        *hival
040300161020     c                   move      data_iso      dadata
040400161020     c                   move      data_iso      adata
040500161020     c                   CALL      'XSRLAV8'
040600161020     c                   PARM                    Wdata8
040700161020      * se corrisponde ho trovato il D-Day
040800161020     c                   if        giolav <> 0
040900161020     c                   leave
041000161020     c                   else
041100161020      * altrimenti continuo a cercare il primo giorno lavorativo valido.
041200161020     c     data_iso      adddur    1:*d          data_iso
041300161020     c                   iter
041400161020     c                   endif
041500161020     c                   enddo
041600161020      *
041700161020     c                   Endsr
041800161020      * ?================================================================== */
041900161020     C*?       ERA su EDIVAB in SOSPENSIONE in attesa dello SCAN 05
042000161020      * ?================================================================== */
042100161020     C     UPD_EDIVA     BEGSR
042200161020      *
042300161020      *  per chiave bolla aggiorna la bolla in SOSPENZIONE che adesso
042400161020      *  risulta partita con l'IGATEAVIS ricevuto (Manifest)
042500161020     C     KSosp_EDIVA   setll     EDIVAT3L
042600161020     C     KSosp_EDIVA   reade     EDIVAT3L
042700161020      *
042800161020     c                   DOw       not %EoF(EDIVAT3L)
042900161020      *
043000161020      *  sul record del Parcel/Service Code sostituisce il SERVICE CODE con quello
043100161020      *  passato nello SCAN 05
043200161020     C                   if        I5PARCEL = %SUBST(vatNOT:1:14)
043300161020     c                                  and
043400161020     C                               vatTRC = 'E'
043500161020     c                                  and
043600161020     C                             I5SERVICE <> *zeros
043700161020     c                   eval      dVATe     = vatNOT
043800161020     c                   move      i5SERVICE     §VATESCD
043900161020     c                   eval      vatNOT    = dVATe
044000161020     c                   End
044100161020      *
044200161020      *  sempre il nuovo CMR GEO_Data
044300161020     C                   if        new_CMR  <> *blank
044400161020     c                   eval      vatCMR  =  new_CMR
044500161020     c                   End
044600161020     c                   Update    EDIVAT00
044700161020      *
044800161020     C     KSosp_EDIVA   reade     EDIVAT3L
044900161020     c                   EndDO
045000161020      * ==========
045100161020      *  poi allinea anche la testata bolla del VAB
045200161020      * ==========
045300161020     C     KSosp_EDIVA   chain     EDIVAB2L
045400161020     c                   if        %Found(EDIVAB2L)
045500161020      *
045600161020      * Se passato il PESO lo sostituisce
045700161020     c                   if        I5WEIGHT > 0
045800161020     c                   Z-ADD     I5WEIGHT      VABPKB
045900161020      * CORREZIONI:
046000161020      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
046100161020     c                   IF        vabPKB > 31,5
046200161020     c                   eval           vabPKB = 30
046300161020     c                   elseIF    vabpkb =0
046400161020     c                   eval           vabpkb = 0,1
046500161020     c                   END
046600161020     c                   End
046700161020      *
046800161020     C                   if        new_CMR  <> *blank
046900161020     c                   eval           vabCMR = new_CMR
047000161020     c                   End
047100161020     c                   eval      vabSCL = 'G'
047200161020     c                   Update    EDIVAB00
047300161020     c                   End
047400161020      *
047500161020     c                   Endsr
047600161020      * ?_______________________________________________________________    */
047700161020     C*?   NON ERA su EDIVAB   cerca su FNBLP e su FIVAB
047800161020      * ?_______________________________________________________________    */
047900161020     C     NON_SU_EDIVA  BEGSR
048000161020      *
048100161020     c                   clear                   in_FNBLP          1
048200161020     c                   clear                   in_FIVAB          1
048300161020      **
048400171003      **  se già confermato in Partenza
048500171003     C     i5PARCEL      chain     fipnd03l
048600171003     c                   IF        %Found(fipnd03l)
048700171003     c                   movel     i5PARCEL      DepotParcel
048800171003      **
048900161020      *   controlla che sia DPD con il Network e non con la linea
049000171003     c     pndLNP        chain     azorg01l
049100161020     c                   if        %Found(azorg01l)
049200161020     c                   eval          OG143  =  orgDE3
049300161020     c                   if        §OGNTW = 'DPD'
049400161020     c                   eval          in_FNBLP = 'S'
049500161020     C                   eval      Msg_Vin_80 = 'Già su BLP Confermato'
049600161020     c                   LEAVESR
049700161020     c                   end
049800161020     c                   end
049900161020      **
050000161020     c                   endIF
050100161020      ** ---------
050200161020      **  se non ancora confermato da Cliente
050300161020     C                   eval        vatTRC = 'E'
050400161020     C                   eval        vatNOT = i5PARCEL
050500161020     C     Kvat4E        setll     fivat02l
050600161020     C     vatTRC        reade     fivat02l
050700161020      *-
050800161020     c                   dow         not %EoF(fivat02l)
050900161020      *-
051000161020     c* imposta DS x reperire il Parcel
051100161020     c                   movel     vatNOT        dVATe
051200161020      *-
051300161020      *- se superata la chiave parcel esce dal ciclo x evitare letture inutili.
051400161020     C                   if           §vatEPRN > i5PARCEL
051500161020     c                   leave
051600161020     c                   end
051700161020      *-
051800161020     c* se la chiave è superiore esce dal ciclo
051900161020     C                   if          i5PARCEL = §vatEPRN
052000161020      *-
052100161020     c     vatLNP        chain     azorg01l
052200161020     c                   if        %Found(azorg01l)
052300161020     c                   eval      OG143 = orgDE3
052400161020      *-
052500161020     c                   if        §OGNTW = 'DPD'
052600161020      * Deve aggiornare il SERVICE CODE con l'ultimo arrivato
052700161020     c     vat_nrr       chain     fivat00f
052800161020     c                   if        %found(fivat00f)
052900161020     c                   eval      dVATe     = vatNOT
053000161020     c                   move      i5SERVICE     §VATESCD
053100161020     c                   eval      vatNOT    = dVATe
053200161020     c                   update    fivatFIS
053300161020     c                   end
053400161020      *
053500161020     c                   eval        vabCCM = vatCCM
053600161020     c                   move      i5Parcel      vabRMN
053700161020     c     kvab01        chain     fivab01l
053800161020     c                   if        %Found(fivab01l)
053900161020     C                   eval      Msg_Vin_80 =
054000161020     C                             'Aggiorna SOLO Peso/Serv.Code su FIVAB'
054100161020     c                   eval          in_FIVAB = 'S'
054200161020      *
054300161020      * Se passato il PESO lo sostituisce
054400161020     c                   if        I5WEIGHT > 0
054500161020     c                   Z-ADD     I5WEIGHT      VABPKB
054600161020      * CORREZIONI:
054700161020      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
054800161020     c                   IF        vabPKB > 31,5
054900161020     c                   eval           vabPKB = 30
055000161020     c                   elseIF    vabpkb =0
055100161020     c                   eval           vabpkb = 0,1
055200161020     c                   END
055300161020     c                   End
055400161020      *
055500161020     c                   update    fivab000
055600161020     c                   LEAVESR
055700161020     c                   end
055800161020     c                   end
055900161020      *
056000161020     c                   end
056100161020      *-
056200161020     c                   end
056300161020     C     vatTRC        reade     fivat02l
056400161020     c                   end
056500161020      **
056600161020     C                   ENDSR
056700161020      * ?================================================================== */
056800161020     C*?   Lo scrive sul TIDP200F in parcheggio
056900161020      * ?================================================================== */
057000161020     C     WRT_TIDP2     BEGSR
057100161020      *
057200161020     c     i5parcel      chain     tidp200f
057300161020      *
057400161020     c                   eval      I2PARCEL    =    I5PARCEL
057500161020     c                   eval      I2SCANT     =    I5SCANT
057600161020     c                   eval      I2DEPCODE   =    I5DEPCODE
057700170907     c                   eval      I2DEPNAME   =    i5DEPCODE7
057800161020     c                   eval      I2DATTIM    =    I5DATTIM                    <<----
057900161020     c                   eval      I2ROUTE     =    I5ROUTE
058000161020     c                   eval      I2TOUR      =    I5TOUR
058100161020     c                   eval      I2PCODE     =    I5PCODE
058200161020     c                   eval      I2SERVICE   =    I5SERVICE
058300161020     c                   eval      I2CCOUNTRY  =    I5CCOUNTRY
058400161020     c                   eval      I2CONSZIP   =    I5CONSZIP
058500161020     c                   eval      I2ADDSER1   =    I5ADDSER1
058600161020     c                   eval      I2ADDSER2   =    I5ADDSER2
058700161020     c                   eval      I2ADDSER3   =    I5ADDSER3
058800161020     c                   eval      I2WEIGHT    =    I5WEIGHT
058900161020     c                   eval      I2CUSTREF   =    I5CUSTREF
059000161020     c                   eval      I2PODIMAG   =    I5PODIMAG
059100161020     c                   eval      I2RECNAME   =    I5RECNAME
059200161020     c                   eval      I2INFOTXT   =    I5INFOTXT
059300161020      *
059400161020     c                   if        %Found(TIDP200F)
059500161020     c                   update    TIDP2000
059600161020     c                   else
059700161020     c                   write     TIDP2000
059800161020     c                   end
059900161020      **
060000161020     C                   ENDSR
060100161020      * ?================================================================== */
060200060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
060300161020      * ?================================================================== */
060400161021     C     callFIEU18    BEGSR
060500060614      *
060600060615     c                   clear                   ok_18             1
060700060630     c                   clear                   msgerr           80
060800060620     c                   clear                   msg_vin_80
060900060615     c                   call      'FIEU18R'
061000161130     c                   parm                    FIEU18
061100060615     c                   parm                    ok_18
061200060620     c                   parm                    msgerr
061300060615
061400161020     c                   eval       se_errore = ok_18
061500060620     c                   eval      msg_vin_80 = msgerr
061600060612      *
061700060615     c                   Endsr
061800161020      * ?_______________________________________________________________    */
061900161020      *    ROUT iniziale
062000060615      * ?_______________________________________________________________    */
062100060615     C     *INZSR        BEGSR
062200160120      *
062300160120     C     Kvat4E        KLIST
062400160120     C                   KFLD                    vatTRC
062500160120     C                   KFLD                    vatNOT
062600160121      *
062700160121     C     kVAT01        KLIST
062800160121     C                   KFLD                    VABFGS
062900160121     C                   KFLD                    VABCCM
063000160121     C                   KFLD                    VABAAS
063100160121     C                   KFLD                    VABLNP
063200160121     C                   KFLD                    VABNRS
063300160121     C                   KFLD                    VABNSP
063400160121      *
063500160121     C     kVAB01        KLIST
063600160121     C                   KFLD                    vabCCM
063700160121     C                   KFLD                    vabRMN
063800160120      *
063900160120     C     KSosp_EDIVA   KLIST
064000160120     C                   KFLD                    vatAAS
064100160120     C                   KFLD                    vatLNP
064200160120     C                   KFLD                    vatNRS
064300160120     C                   KFLD                    vatNSP
064400160120      *
064500170907     C     key_VTG       KLIST
064600170907     C                   KFLD                    cldCOD
064700170907     C                   KFLD                    cldDEP
064800160120      *
064900160120      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
065000991129     c     *ENTRY        PLIST
065100161124     C                   parm                    FIEU04DS40
065200161020     C                   parm                    se_errore
065300161020     C                   parm                    Msg_Vin_80
065400161020      **
065500971216      * Recupero data e ora
065600971216     C                   TIME                    WORA
065700991124     C                   TIME                    W0140
065800991124      * UDATE IN GGMMAAAA
065900991124     C                   MOVE      W0140         WDTGIO
066000991124      * UDATE IN AAAAMMGG
066100991124     C     *eur          MOVEL     WDTGIO        DATA_eur
066200991124     C     *iso          MOVEL     DATA_eur      dateu
066300160120      *
066400991124     C                   ENDSR
066500161020      * ?_______________________________________________________________    */
066600161020      *    Se errori NON blocca il programma
066700161020      * ?_______________________________________________________________    */
066800161020     C     *pssr         BEGSR
066900161020     C                   ENDSR     '*CANCL'
067000160120      *  ------------------------------------------------------------------ */
