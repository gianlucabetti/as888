000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060615      **?-------------------------------------------------------------------*****
000500060613      *  Da UPLOAD                                                              *
000600161018      *  TRASCODIFICA : GEODATA  TTEVENTS                                       *
000700161018      *               STATI EXPORT BARTOLINI a DPD                              *
000800060615      **?-------------------------------------------------------------------*****
000900060612     Ftivin00r  uF   E             DISK    usropn
001000060719     FTidp502L  uF   E           K DISK    usropn
001100991124      *
001200160125     FTIDP200F  uf a E           k DISK
001300160121      *
001400160121     FAZorg01L  if   E           K DISK
001500160120     FDPcdp01l  IF   E           K DISK
001600160120     FDPpdc01l  IF   E           K DISK
001700161018      **?-------------------------------------------------------------------*****
001800160205      *
001900000907      *-------------------
002000060612     D Lung_Parcel     S              3  0
002100060614     D Tipo_error      S              1  0
002200060612      *-------------------
002300991129     D W015A           S             15
002400000223     D W0140           S             14  0
002500991129     D WORA            S              6  0
002600991129     D XX              S              3  0 INZ
002700991129     D WDTGIO          S              8  0
002800991129     D DATEU           S              8  0
002900060720     D DATamg          S              8  0
003000991129     D DATA_eur        S               D   DATFMT(*eur)
003100060720     D DATA_amg        S               D   DATFMT(*iso)
003200160120     D DATA_iso        S               D   DATFMT(*iso)
003300050112     D svkpjbu         s                   like(kpjbu)
003400060413      *-------------------
003500991129     D KPJBA         E DS
003600000406     D DDPP          E DS
003700160120      *-------------------
003800160121     D OG143         E DS
003900160121      *-------------------
004000160120     D DpdcFLO       E DS
004100160120     D tisie2ds      E DS
004200160120      *-------------------
004300160120     d Wdata8          DS
004400160120     d  dadata                 1      8  0
004500160120     d  adata                  9     16  0
004600160120     d  GioLav                17     21  0
004700160120      *
004800160120     D DepotParcel     S              4
004900160120      * ?------------------------------------------------------------------ */
005000160120     D   Digits        C                   '0123456789'
005100160120     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
005200160120     d Gomme_365       c                   365
005300160120     d Gomme_366       c                   366
005400160120     d Gomme_367       c                   367
005500060614      **?------------------------------------------------------------------ */
005600060614     C*? Ds Decodifica dei campi
005700060614     D TIDP500       E DS                  EXTNAME(TIDP500W)
005800060612      * ?   * Campi da tradurre con interi
005900060623     D atoll           PR            20I 0
006000060623     D                                     EXTPROC('atoll')
006100060612     D  Char                           *
006200060612     D                                     VALUE
006300060612     D                                     OPTIONS(*STRING)
006400060623      * ?   * Campi da tradurre con interi
006500060623     D atoi            PR            10I 0
006600060623     D                                     EXTPROC('atoi')
006700060623     D  Char                           *
006800060623     D                                     VALUE
006900060623     D                                     OPTIONS(*STRING)
007000060612      * ?   * Campi da tradurre con decimali
007100060612     D atof            PR             8F
007200060612     D                                     EXTPROC('atof')
007300060612     D  Char                           *
007400060612     D                                     VALUE
007500060612     D                                     OPTIONS(*STRING)
007600060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
007700060612      * ?   * Campi x decodifica * (INPUT  del Record)
007800060612     D  Dati           s           1500
007900060612     D Separa_campi    s              1
008000060612     D Decimal_Separ   s              1
008100060612      * ?   * Campi decodificati * (OUTPUT del Record)
008200060612      *       Schiere di Output / Campi di Output
008300060614     D  Alfa40         s             40
008400060612     d  Sk             s              3u 0
008500060612     D    NR_campi     s              3u 0
008600060612     D  Campi_Record   ds
008700060612     D    Dato_sk                    40    DIM(100)
008800060612     D  Campi_Lunghi   ds
008900060612     D    LunDato_sk                  3u 0 DIM(100)
009000060612     D Campi_Numerici  ds
009100060612     D    DatoNum_sk                  1    DIM(100)
009200060612     D Campi_Decimali  ds
009300060612     D    Decimal_sk                  3u 0 DIM(100)
009400060615      * ? ------------------------------------------------------------------
009500060612     D  position       s              3  0 INZ(0)
009600060612
009700060612     D  se_errore      s              1    inz(' ')
009800060612     D  Msg_Err        s            132    inz(' ')
009900060614     D  Msg_Vin_80     s             80    inz(' ')
010000060619     D Testa_Sk        s             10    DIM(50)
010100060615      * ?______________________________________________________________     */
010200060612      *   Ciclo principale
010300060615      * ?______________________________________________________________     */
010400060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
010500060612     C*
010600060612     C                   if        not %open(tivin00r)
010700060612     C                   open      tivin00r
010800060612     C                   endif
010900060612     C*? Controllo dati arrivati da DPD
011000060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
011100060612      * ?- controllando che la trasmissione sia completa.
011200060612
011300060612     c                   exsr      check_Trasm
011400060612
011500060613      **  Errore di trasmissione x tutti i records
011600060613      **   --> file in errore
011700060613     c                   if        se_errore = 'S'
011800060612
011900060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
012000060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
012100060614     C                               >> Manca Testata Messaggio !! +
012200060614     C                               Farsi rimandare i DATI !!'
012300060612
012400060612      * ?  Scrive su tutti i records il tipo di errore
012500060612     c     *start        setll     tivin00r
012600060612     c                   read      tivin00r
012700060612     c                   dow       not %eof(tivin00r)
012800060612     C                   eval      vinMSG = Msg_Err
012900060612     C                   eval      vinFLG = '2'
013000060613     c                   eval      esito  = '2'
013100060612     c                   update    tivin000
013200060612     c                   read      tivin00r
013300060612     c                   endDO
013400060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
013500060615     c                   else
013600060623
013700060612     c                   exsr      Importa_Msg
013800060612
013900060614     c                   end
014000060614
014100060612     C                   if        %open(tivin00r)
014200060612     C                   close     tivin00r
014300060612     C                   endif
014400060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
014500060614      *
014600060710     c                   if        almeno_uno ='S' and esito ='1'
014700060710     C                   eval      esito ='0'
014800060614     C                   endif
014900060719      *
015000060719      * ?Prima di uscire dal programma esegue un ciclo sugli SCAN in attesa
015100060719      * ? x dare come consegnate le spedizioni di cui si è ricevuto il MIC
015200060719      * ? e non lo SCAN di consegna.
015300060719
015400060719     c                   exsr      SCAN_ATTESA
015500060719
015600060614      *
015700060612     c                   SETON                                        LR
015800060615      * ?________________________________________________________________   */
015900060615      *? Controlla la trasmissione  primo record descrittivo tracciato
016000060615      * ?________________________________________________________________   */
016100060612     c     Check_Trasm   Begsr
016200060612
016300060612     C                   clear                   se_errore
016400060614      ** primo  record con intestazione dei campi
016500060612     c     *start        setll     tivin00r
016600060612     c                   read      tivin00r
016700060612     c                   if        not %eof(tivin00r)
016800060615      *
016900060612     c                   exsr      Decod_Record
017000060630      *
017100060630      **  Se presente un errore nel record emette una segnalazione msg
017200060630     c                   if        se_errore <> 'S'
017300060630     C                   eval      vinFLG = '1'
017400060710     c                   eval      esito  = '0'
017500060630     c                   update    tivin000
017600060630     c                   endif
017700060615      *
017800060612     c                   endif
017900060612
018000060612     c                   endSR
018100060615      * ?________________________________________________________________   */
018200060612      *? Importa i records della tramsissione
018300060615      * ?________________________________________________________________   */
018400060612     c     Importa_Msg   Begsr
018500060612
018600060614      * per controllare se almeno un record è stato importato sul VAB
018700060614     c                   clear                   Almeno_Uno        1
018800060614
018900060623      * Continua a leggere dal secondo record.
019000060623      *   Il primo è la testata già controllata dalla Check_trasm.
019100060612     c                   read      Tivin00r
019200060612
019300060612     c                   dow       not %eof(Tivin00r)
019400060614
019500060614      * solo i record sflaggati da rielaborare
019600060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
019700060612
019800060612      ** Controlli formali sui campi
019900060612     c                   clear                   se_errore
020000060620     c                   clear                   Msg_Vin_80
020100060612
020200060612      ** Decodifica record a campi variabili
020300060612     c                   exsr      Decod_Record
020400160412      ** ===========
020500160412      **   x SCAN 18 PUDO  solo su nostro EXPORT  sotto tipologia 28/29 PUDO
020600160412     c                   if        se_errore <>'S' and i5SCANT  = 18
020700160412      ***
020800160412     c                   IF        (%subst(i5PARCEL:1:4) = '0844'  or
020900160412     c                              %subst(i5PARCEL:1:4) = '0845')     and
021000160412     c                             (%subst(i5INFOTXT:1:4) = '128:' or
021100160412     c                              %subst(i5INFOTXT:1:4) = '129:')
021200160412      **
021300160412      *   trasforma lo SCAN RICEVUTO in un ALTRO SCAN per descrivere il
021400160412     c                   exsr      SCAN_18_PUDO
021500160412      **
021600160412     c                   END
021700160413     c                   endIF
021800160412      ** ===========
021900060615      *
022000060612      **  Se presente un errore nel record emette una segnalazione msg
022100060612     c                   if        se_errore ='S'
022200060614     C                   eval      vinMSG = Msg_Vin_80
022300060623     C                   eval      vinFLG = '2'
022400060623     c                   eval      esito  = '1'
022500160120      *
022600160120      ** ===========
022700160120      *** se arrivato uno  SCAN 05 si attiva la spedizione sul GEODATA
022800160120      **   in sospensione sul EDIVAB e se non lo trova lo scrive o aggiorna
022900160120      ** e NON è la ripetizione del nostro scan x quando facciamo l'export.
023000160120      **   ossia non è un 0844/0845 Italia
023100160120     c                   elseIF    i5SCANT  = 05
023200160120     c                               and
023300160120     c                              %subst(i5PARCEL:1:4) <> '0844'
023400160120     c                               and
023500160120     c                              %subst(i5PARCEL:1:4) <> '0845'
023600160120      ** ===========
023700160120     c                   exsr      SCAN_05
023800160120      ** ===========
023900060612     c                   else
024000060612
024100060612      *  con il record in canna scrive direttamente il VAB ed il VAT
024200060614     c                   exsr      WRT_EVB_CDE
024300160121     c                   end
024400160121
024500060614      *  Problemi nella decodifica dei campi VAB/VAT
024600060614     c                   if        se_errore ='S'
024700060614     C                   eval      vinMSG = Msg_Vin_80
024800060614     C                   eval      vinFLG = '2'
024900060623     c                   eval      esito  = '1'
025000060614     c                   else
025100060614      *  Record OK
025200060615     c                   move      'S'           Almeno_Uno
025300060612     C                   eval      vinFLG = '1'
025400160126      *  x gli SCAN 05 deve riportare quanto segnalato dalla routine SCAN_05
025500160126     c                   if        i5SCANT <> 05
025600160126     c                                or
025700160126     c                             (i5SCANT  = 05 and
025800160126     c                             (%subst(i5PARCEL:1:4) = '0844'
025900160126     c                               or
026000160126     c                              %subst(i5PARCEL:1:4) = '0845') )
026100060614     C                   eval      vinMSG = *Blank
026200160126     c                   end
026300060614     c                   end
026400060612
026500060612     c                   update    Tivin000
026600060614     c                   endIF
026700060612
026800060612     c                   read      Tivin00r
026900060612     C                   ENDdo
027000060612      **
027100060612     c                   endSR
027200060615      * ?______________________________________________________________     */
027300060612      *?    Decodifica record a campi variabili
027400060615      * ?______________________________________________________________     */
027500060612     c     Decod_Record  Begsr
027600060612
027700060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
027800060612     c                   movel(p)  VINDTA        dati
027900060612      **
028000060612     c                   exsr      Split_Rec
028100060612      **
028200060612      **  Records di Intestazione e di fine Msg
028300060614     c                   if        %subst(dati:1:8) ='PARCELNO'
028400060614     c                   exsr      Rec_Testata
028500060615      *
028600060612     c                   Else
028700060612      ** conta le righe
028800060612     c                   exsr      Rec_Dettaglio
028900060615      * ?
029000060612     c                   End
029100060612      **
029200060612     c                   Endsr
029300060615      * ?________________________________________________________________   */
029400060614      *?      Record di Testata
029500060615      * ?________________________________________________________________   */
029600060614     c     Rec_Testata   Begsr
029700060612      * ?  Prima riga
029800060620     c     'PARCELNO'    SCAN      dati:1        position
029900060614     c                   if        %Found and position < 10
030000060614      ** esempio di record :
030100060614      * ?   PARCELNO;SCAN_CODE;DEPOT_CODE;DEPOTNAME;EVENT_DATE_TIME.......
030200060614      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
030300060614     C                   clear                   Testa_Sk
030400060614      * ? Dal 1° campo troviamo i nomi delle intestazioni dei campi C.Request
030500060614     C     1             do        50            sk
030600060614     c                   eval      Testa_Sk(sk) = %subst(Dato_sk(sk):1:+
030700060614     c                             LunDato_sk(sk))
030800060614     c                   enddo
030900060614      **
031000060612     c                   ElSe
031100060614      * ?    Se l'inizio del file non ha la testata errore di trasmissione
031200060614      * ?-----> Errore
031300060614     C                   eval      se_errore = 'S'
031400060614
031500060612     c                   End
031600060612      **
031700060612     c                   Endsr
031800060615      * ?_______________________________________________________________    */
031900060612      *?      Riga Bolla Import da controllare e tradurre
032000060615      * ?_______________________________________________________________    */
032100060612     c     Rec_Dettaglio Begsr
032200060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
032300060612      **
032400060615     c                   clear                   TIdp500
032500060614     c                   clear                   Msg_Err
032600060614      * ? Nel 1° campo : Parcel Number               (Oblligatorio)  PARCELNO
032700060614      **                                                             Numero Parcel
032800060614     C                   z-add     1             sk
032900060614     c                   If        LunDato_sk(sk) =  0
033000060614      * ?-----> Errore
033100060614     c                   eval      tipo_error = 1
033200060614     c                   exsr      Field_Error
033300060614     c                   Else
033400060614     c                   eval      i5PARCEL   = %subst(Dato_sk(sk):1:+
033500060614     c                                              LunDato_sk(sk))
033600060614     c                   End
033700060614      **
033800060614      * ? Nel 2° campo : Codice evento SCAN  DPD     (Oblligatorio)  SCAN_CODE
033900060614      **                                                             Codice SCAN
034000060614     C                   add       1             sk
034100060614     c                   If        DatoNum_sk(sk) = 'N' or
034200060614     c                             LunDato_sk(sk) =  0
034300060614      * ?-----> Errore
034400060614     c                   eval      tipo_error = 2
034500060614     c                   exsr      Field_Error
034600060614     c                   Else
034700060614     c                   eval      i5SCANT  = atoi(%TRIM(Dato_sk(sk)))
034800060614     c                   End
034900060614      * ? Nel 3° campo : DEPOT che ha fatto lo SCAN  (Oblligatorio)  DEPOT_CODE
035000060614      **                                                             Codice Depot
035100060614     C                   add       1             sk
035200060614     c                   If        LunDato_sk(sk) =  0
035300060614      * ?-----> Errore
035400060614     c                   eval      tipo_error = 1
035500060614     c                   exsr      Field_Error
035600060614     c                   Else
035700060614     c                   eval      i5DEPCODE  = %subst(Dato_sk(sk):1:+
035800060614     c                                              LunDato_sk(sk))
035900060614     c                   End
036000060630      * ? Nel 4° campo : NOME DEPOT                                  DEPOTNAME
036100060614      **                                                             Nome Depot
036200060614     C                   add       1             sk
036300060630     c                   If        LunDato_sk(sk) >  0
036400060614     c                   eval      i5DEPNAME  = %subst(Dato_sk(sk):1:+
036500060614     c                                              LunDato_sk(sk))
036600060614     c                   End
036700060630      * ? Nel 5° campo : Data Ora SCAN             Obbligatorio      DATE/TIME
036800060614      **                                                             DATA/ORA Evento
036900060614     C                   add       1             sk
037000060614     c                   If        DatoNum_sk(sk) = 'N' or
037100060614     c                             LunDato_sk(sk) =  0
037200060614      * ?-----> Errore
037300060614     c                   eval      tipo_error = 2
037400060614     c                   exsr      Field_Error
037500060614     c                   Else
037600060623     c                   eval      i5DATTIM = atoll(%TRIM(Dato_sk(sk)))
037700060614     c                   End
037800060614      * ? Nel 6° campo : ROUTE non sempre presente                   ROUTE
037900060614      **                                                             ROUTE
038000060614     C                   add       1             sk
038100060614     c                   If        LunDato_sk(sk) >  0
038200060614     c                   eval      i5ROUTE    = %subst(Dato_sk(sk):1:+
038300060614     c                                              LunDato_sk(sk))
038400060614     c                   End
038500060614      * ? Nel 7° campo : TOUR  non sempre presente                   TOUR
038600060614      **                                                             TOUR
038700060614     C                   add       1             sk
038800060630     c                   If        DatoNum_sk(sk) = 'N' and
038900060614     c                             LunDato_sk(sk) >  0
039000060614      * ?-----> Errore
039100060614     c                   eval      tipo_error = 3
039200060614     c                   exsr      Field_Error
039300060614     c                   Else
039400060614     c                   eval      i5TOUR     =  atoi(%TRIM(Dato_sk(sk)))
039500060614     c                   End
039600060630      * ? Nel 8° campo : TIPO PARCEL 01/02                           PCODE
039700060630      **                                                             Parcel Code
039800060614     C                   add       1             sk
039900060630     c                   If        DatoNum_sk(sk) = 'N' and
040000060630     c                             LunDato_sk(sk) >  0
040100060614      * ?-----> Errore
040200060630     c                   eval      tipo_error = 3
040300060614     c                   exsr      Field_Error
040400060614     c                   Else
040500060614     c                   eval      i5PCODE  = atoi(%TRIM(Dato_sk(sk)))
040600060614     c                   End
040700060630      * ? Nel 9° campo : Codice di Servizio                          SERVICE
040800060614      **                                                             SERVICE CODE
040900060614     C                   add       1             sk
041000060630     c                   If        DatoNum_sk(sk) = 'N' and
041100060630     c                             LunDato_sk(sk) >  0
041200060614      * ?-----> Errore
041300060630     c                   eval      tipo_error = 3
041400060614     c                   exsr      Field_Error
041500060614     c                   Else
041600060614     c                   eval      i5SERVICE  = atoi(%TRIM(Dato_sk(sk)))
041700060614     c                   End
041800060630      * ? Nel 10° campo : Nazione Destinatario                       CONSEGNEE_COUNTRY_C
041900060614      **                                                             NAZIONE DESTINATARIO
042000060614     C                   add       1             sk
042100060630     c                   If        DatoNum_sk(sk) = 'N' and
042200060630     c                             LunDato_sk(sk) >  0
042300060614      * ?-----> Errore
042400060630     c                   eval      tipo_error = 3
042500060614     c                   exsr      Field_Error
042600060614     c                   Else
042700060614     c                   eval      i5CCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
042800060614     c                   End
042900060630      * ? Nel 11° campo : Cod.Postale Destinatario                   CONSEGNEE_ZIP
043000060614      **                                                             CAP DESTINATARIO
043100060614     C                   add       1             sk
043200060614     c                   If        LunDato_sk(sk) >  0
043300060614     c                   eval      i5CONSZIP  = %subst(Dato_sk(sk):1:+
043400060614     c                                              LunDato_sk(sk))
043500060614     c                   End
043600060614      * ? Nel 12° campo : CODICI AGGIUNTIVI                          ADD_SERVICE1
043700060614      **                                                             cod.AGG. 1
043800060614     C                   add       1             sk
043900060614     c                   If        DatoNum_sk(sk) = 'N' and
044000060614     c                             LunDato_sk(sk) >  0
044100060614      * ?-----> Errore
044200060614     c                   eval      tipo_error = 3
044300060614     c                   exsr      Field_Error
044400060614     c                   Else
044500060614     c                   eval      i5ADDSER1  =  atoi(%TRIM(Dato_sk(sk)))
044600060614     c                   End
044700060614      * ? Nel 13° campo : CODICI AGGIUNTIVI                          ADD_SERVICE2
044800060614      **                                                             cod.AGG. 2
044900060614     C                   add       1             sk
045000060614     c                   If        DatoNum_sk(sk) = 'N' and
045100060614     c                             LunDato_sk(sk) >  0
045200060614      * ?-----> Errore
045300060614     c                   eval      tipo_error = 3
045400060614     c                   exsr      Field_Error
045500060614     c                   Else
045600060614     c                   eval      i5ADDSER2  =  atoi(%TRIM(Dato_sk(sk)))
045700060614     c                   End
045800060614      * ? Nel 14° campo : CODICI AGGIUNTIVI                          ADD_SERVICE3
045900060614      **                                                             cod.AGG. 3
046000060614     C                   add       1             sk
046100060614     c                   If        DatoNum_sk(sk) = 'N' and
046200060614     c                             LunDato_sk(sk) >  0
046300060614      * ?-----> Errore
046400060614     c                   eval      tipo_error = 3
046500060614     c                   exsr      Field_Error
046600060614     c                   Else
046700060614     c                   eval      i5ADDSER3  =  atoi(%TRIM(Dato_sk(sk)))
046800060614     c                   End
046900060614      * ? Nel 15° campo : PESO                                       WEIGHT
047000060614      **                                                             Peso in KG
047100060614     C                   add       1             sk
047200060614     c                   If        DatoNum_sk(sk) = 'N' and
047300060614     c                             LunDato_sk(sk) >  0
047400060614      * ?-----> Errore
047500060614     c                   eval      tipo_error = 3
047600060614     c                   exsr      Field_Error
047700060614     c                   Else
047800060623     c                   eval      i5WEIGHT   =  atof(%TRIM(Dato_sk(sk)))
047900160704     c                   div       100           i5weight
048000060614     c                   End
048100060614      * ? Nel 16° campo : Riferimento Cliente                        CUSTOMER_REFERENCE
048200060614      **                                                             Riferimento Cliente
048300060614     C                   add       1             sk
048400060614     c                   If        LunDato_sk(sk) >  0
048500060614     c                   eval      i5CUSTREF  = %subst(Dato_sk(sk):1:+
048600060614     c                                              LunDato_sk(sk))
048700060614     c                   End
048800060614      * ? Nel 17° campo : POD IMAGE di riferimento                   POD_IMAGE_REF
048900060614      **                                                             Riferimento POD IMAGE
049000060614     C                   add       1             sk
049100060614     c                   If        DatoNum_sk(sk) = 'N' and
049200060614     c                             LunDato_sk(sk) >  0
049300060614      * ?-----> Errore
049400060614     c                   eval      tipo_error = 3
049500060614     c                   exsr      Field_Error
049600060614     c                   Else
049700060614     c                   eval      i5PODIMAG  =  atoi(%TRIM(Dato_sk(sk)))
049800060614     c                   End
049900060614      * ? Nel 18° campo : FIRMA di chi ha ricevuto la merce          RECEIVER_NAME
050000060614      **                                                             FIRMA
050100060614     C                   add       1             sk
050200060614     c                   If        LunDato_sk(sk) >  0
050300060614     c                   eval      i5RECNAME  = %subst(Dato_sk(sk):1:+
050400060614     c                                              LunDato_sk(sk))
050500060614     c                   End
050600060614      * ? Nel 18° campo : INFO TESTO                                 INFO_TEXT
050700060614      **                                                             TESTO AGGIUNTIVO
050800060614     C                   add       1             sk
050900060614     c                   If        LunDato_sk(sk) >  0
051000060614     c                   eval      i5INFOTXT  = %subst(Dato_sk(sk):1:+
051100060614     c                                              LunDato_sk(sk))
051200060614     c                   End
051300060614      **
051400060614     c     End_Check     tag
051500060615      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDP500W
051600060612      **
051700060612     c                   Endsr
051800060615      * ?____________________________________________________________       */
051900060614      *?      Campi in errore
052000060615      * ?____________________________________________________________       */
052100060614     c     Field_Error   Begsr
052200060612      **
052300060614     C                   eval      se_errore   = 'S'
052400060614      * msg video o in posta
052500060614     c                   If        Msg_Err = *blank
052600060614     c
052700060614     c                   Select
052800060614     c
052900060614     c                   When      Tipo_Error = 1
053000060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
053100060614     C                             ' >> Obblig.- Manca Dato !!'
053200060614     c
053300060614     c                   When      Tipo_Error = 2
053400060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
053500060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
053600060614     c
053700060614     c                   When      Tipo_Error = 3
053800060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
053900060614     C                             ' >> NON Numerico  !!'
054000060614     c                   EndSL
054100060614     c
054200060614     c                   End
054300060614      * Errore su VINMSG
054400060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
054500060614     c                                          + %trim(Testa_sk(sk))
054600060614      **
054700060614     c                   Endsr
054800060615      * ?____________________________________________________________       */
054900060612      *?      Suddivide i campi della riga con carattere divisorio (;)
055000060615      * ?____________________________________________________________       */
055100060612     c     Split_Rec     Begsr
055200060612      **
055300060612     c                   clear                   Nr_campi
055400060612     c                   clear                   Campi_Record
055500060612     c                   clear                   Campi_Lunghi
055600060612     c                   clear                   Campi_Numerici
055700060612     c                   clear                   Campi_Decimali
055800060612     c                   movel     ';'           Separa_campi
055900060612     c                   movel     '.'           Decimal_Separ
056000060612      *
056100060612     c                   call      'FIEU00R'
056200060612      * ?     Input
056300060612     c                   parm                    Separa_campi
056400060612     c                   parm                    Decimal_Separ
056500060612     c                   parm                    dati
056600060612      * ?     Output  su schiere di 100 elementi
056700060612     c                   parm                    Nr_campi
056800060612     c                   parm                    Campi_Record                   * i dati
056900060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
057000060612     c                   parm                    Campi_Numerici                 * se numerici
057100060612     c                   parm                    Campi_Decimali                 * quanti decimali
057200060612      **
057300060612     c                   Endsr
057400060615      * ?_______________________________________________________________    */
057500060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
057600060615      * ?_______________________________________________________________    */
057700060615     C     WRT_EVB_CDE   BEGSR
057800060614      *
057900060615     c                   clear                   ok_18             1
058000060630     c                   clear                   msgerr           80
058100060620     c                   clear                   msg_vin_80
058200060615     c                   call      'FIEU18R'
058300060615     c                   parm                    tidp500
058400060615     c                   parm                    ok_18
058500060620     c                   parm                    msgerr
058600060615
058700060615     c                   move      ok_18         se_errore
058800060620     c                   eval      msg_vin_80 = msgerr
058900060612      *
059000060615     c                   Endsr
059100060719      * ?_______________________________________________________________    */
059200060719     C*? ESEGUO ciclo sugli SCAN in ATTESA x chiudere spedizioni come CONSEGNATE
059300060719      * ?_______________________________________________________________    */
059400060719     C     SCAN_ATTESA   BEGSR
059500060719     c                   open      tidp502L
059600060719      *
059700060719      * Dalla data scadenza più bassa
059800060719     c     *START        setll     tidp502L
059900060719     c                   read      tidp502L
060000060719      *
060100060719     c                   dow       not %EoF(tidp502L)
060200060719      *
060300060719      *  se la data scadenza (GG di attesa) è superiore all'UDATE esce dal ciclo
060400060719     c                   movel     DP5DATAGG     Data08            8 0
060500060719     c                   if        Data08 > DATEU
060600060719     c                   leave
060700060719     c                   end
060800060719      *
060900060719      * Imposta il record da passare al pgm di elaborazione i modalità (A)
061000060719      *   elaborazione SCAN di ATTESA.
061100060719     c                   clear                   tidp500
061200060719      *
061300060719     c                   eval      I5PARCEL    =    DP5PARCEL
061400060719     c                   eval      I5SCANT     =    DP5SCANT
061500060719     c                   eval      I5DEPCODE   =    DP5DEPCODE
061600060719     c                   eval      I5DEPNAME   =    DP5DEPNAME
061700060808     c******             eval      I5DATTIM    =    DP5DATAGG                   <<----
061800060808     c                   eval      I5DATTIM    =    DP5DATTIM                   <<----
061900060719     c                   eval      I5ROUTE     =    DP5ROUTE
062000060719     c                   eval      I5TOUR      =    DP5TOUR
062100060719     c                   eval      I5PCODE     =    DP5PCODE
062200060719     c                   eval      I5SERVICE   =    DP5SERVICE
062300060719     c                   eval      I5CCOUNTRY  =    DP5COUNTRY
062400060719     c                   eval      I5CONSZIP   =    DP5CONSZIP
062500060719     c                   eval      I5ADDSER1   =    DP5ADDSER1
062600060719     c                   eval      I5ADDSER2   =    DP5ADDSER2
062700060719     c                   eval      I5ADDSER3   =    DP5ADDSER3
062800060719     c                   eval      I5WEIGHT    =    DP5WEIGHT
062900060719     c                   eval      I5CUSTREF   =    DP5CUSTREF
063000060719     c                   eval      I5PODIMAG   =    DP5PODIMAG
063100060719     c                   eval      I5RECNAME   =    DP5RECNAME
063200060719     c                   eval      I5INFOTXT   =    DP5INFOTXT
063300060719      *
063400060719     c                   move      'A'           ok_18             1
063500060719     c                   clear                   msgerr           80
063600060719     c                   clear                   msg_vin_80
063700060719     c                   call      'FIEU18R'
063800060719     c                   parm                    tidp500
063900060719     c                   parm                    ok_18
064000060719     c                   parm                    msgerr
064100060720      *
064200060720      * calcolo un mese dalla data dell'evento: (serve per eliminare il record
064300060720      *  se ritornato con errori dal FIEU18R)
064400060720     c                   clear                   piu_di_30gg       1
064500060720     c                   movel     dp5DATAGG     datamg            8 0
064600060720     C     *iso          MOVEL     datamg        data_amg
064700060720     c                   adddur    30:*d         data_amg
064800060720     C     *iso          MOVEL     data_amg      datamg
064900060720      *
065000060720      *  Se sono passati + di 30 giorni
065100060720     c                   if        datamg < WDTGIO
065200060720     c                   eval      piu_di_30gg ='S'
065300060720     c                   end
065400060720      *
065500060719      * se non presenti errori cancella lo SCAN di attesa
065600060720      *  oppure dopo un mese di errori lo cancello comunque se persevera la segnalazione
065700060720     c                   if        msgerr = *blank or
065800060720     c                             msgerr <> *blank and piu_di_30gg ='S'
065900060719     c                   delete    tidp500r
066000060719     c                   End
066100060719      *
066200060719     c                   read      tidp502L
066300060719     c                   EndDO
066400060719      *
066500060719     c                   close     tidp502L
066600060719     c                   Endsr
066700060615      * ?_______________________________________________________________    */
066800060615     C     *INZSR        BEGSR
066900060615      * ?_______________________________________________________________    */
067000160120      *
067100160120     C     Kvat4E        KLIST
067200160120     C                   KFLD                    vatTRC
067300160120     C                   KFLD                    vatNOT
067400160121      *
067500160121     C     kVAT01        KLIST
067600160121     C                   KFLD                    VABFGS
067700160121     C                   KFLD                    VABCCM
067800160121     C                   KFLD                    VABAAS
067900160121     C                   KFLD                    VABLNP
068000160121     C                   KFLD                    VABNRS
068100160121     C                   KFLD                    VABNSP
068200160121      *
068300160121      *
068400160121     C     kVAB01        KLIST
068500160121     C                   KFLD                    vabCCM
068600160121     C                   KFLD                    vabRMN
068700160120      *
068800160120     C     KSosp_EDIVA   KLIST
068900160120     C                   KFLD                    vatAAS
069000160120     C                   KFLD                    vatLNP
069100160120     C                   KFLD                    vatNRS
069200160120     C                   KFLD                    vatNSP
069300160120      *
069400160120     C     key_Depot     KLIST
069500160120     C                   KFLD                    vers_CAP_DPD
069600160120     C                   KFLD                    DepotParcel
069700160120      *
069800160120     C     key_Dep       KLIST
069900160120     C                   KFLD                    DepotParcel
070000160120     C                   KFLD                    DATEU
070100160121     C*
070200160121     C     Kar4I         KLIST
070300160121     C                   KFLD                    AR4TRC
070400160121     C                   KFLD                    AR4N14
070500160120      *
070600160120      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
070700051205      *
070800991129     c     *ENTRY        PLIST
070900060613     C                   parm                    esito             1
071000971215      *
071100971216      * Recupero data e ora
071200971216     C                   TIME                    WORA
071300991124     C                   TIME                    W0140
071400991124      * UDATE IN GGMMAAAA
071500991124     C                   MOVE      W0140         WDTGIO
071600991124      * UDATE IN AAAAMMGG
071700991124     C     *eur          MOVEL     WDTGIO        DATA_eur
071800991124     C     *iso          MOVEL     DATA_eur      dateu
071900160120      *
072000160120     c                   z-add     0             vers_CAP_DPD      5 0
072100160120     c* recupero l'ultima versione di cappario DPD
072200160120     c                   clear                   tisie2ds
072300160120     c                   eval      sie2dri=*all'9'
072400160120     c                   call      'TISIE2R'
072500160120     c                   parm                    tisie2ds
072600160120     c* se trovata versione reperisco  descrizioni deposito
072700160120    2c                   if        sie2ver > 0
072800160120     c                   z-add     sie2ver       vers_CAP_DPD
072900160120     c                   end
073000160120      *
073100050414      *
073200991124     C                   ENDSR
073300060615      * ?_______________________________________________________________    */
073400060615     C     *pssr         BEGSR
073500160121      *
073600060614     C                   eval      esito = '2'
073700060614     C*
073800060614     C                   ENDSR     '*CANCL'
073900160229      * ?-----------------------------------------------------------------  */
074000160229      *    Rileva la prima volta che arriva lo SCAN 05
074100160229      * ?-----------------------------------------------------------------  */
074200160229     C     check_WFIEU00 BEGSR
074300160229     C*
074400160229     c     i5PARCEL      chain     wfieu00f
074500160229     c                   if        %Found(wfieu00f)
074600160229     c                   if        F00DATSC5 = 0
074700160229     c                   eval      F00DATSC5 = dateu
074800160229     c                   eval      F00ORASC5 = wora
074900160229     c                   update    fieu000
075000160229     c                   end
075100160229     c                   else
075200160229     c                   clear                   fieu000
075300160229     c                   eval      F00PARCEL = i5PARCEL
075400160229     c                   eval      F00DATSC5 = dateu
075500160229     c                   eval      F00ORASC5 = wora
075600160229     c                   write     fieu000
075700160229     c                   end
075800160229     C*
075900160229     C                   ENDSR
076000160120      * ?-----------------------------------------------------------------  */
076100160120      *   ARRIVATO uno SCAN 05 controlla bolla in sospensione
076200160120      * ?-----------------------------------------------------------------  */
076300160120     C     SCAN_05       BEGSR
076400160120      *
076500160229      * registra il 1° arrivo dello SCAN 05
076600160229     c                   exsr      check_WFIEU00
076700160229      *
076800160229      *
076900160120      **  se non è ancora stato confermato il CMR
077000160120     C                   eval        vatTRC = 'E'
077100160120     C                   eval        vatNOT = i5PARCEL
077200160120     C     Kvat4E        setll     edivat4l
077300160120     C     vatTRC        reade     edivat4l
077400160120      *--
077500160120     c                   IF        not %EoF(edivat4l)
077600160120     c                                 and
077700160120     C                             I5PARCEL = %SUBST(vatNOT:1:14)
077800160120      *--
077900160120      *-   Se era in attesa dello SCAN05 di collo PARTITO
078000160120     c                   if           vatCMR = GEO_in_Sospeso
078100160126     C                   eval      vinMSG = 'Già su EDIVAB in Sospeso'
078200160205      * ==========
078300160205      *  imposta il CMR x la conferma
078400160205     c                   exsr      CAMBIA_CMR
078500160120     c                   exsr      UPD_EDIVA
078600160120     c                   else
078700160120      *-  Se c'è su EDIVAB
078800160120      *--  ed era già stato ATTIVATO (quindi non più in sospensione)
078900160229     C                   eval      vinMSG = 'Attivato da IGATE/solo UPD. serv-
079000160205     c                             .code e peso'
079100160205     c                   clear                   new_CMR
079200160205     C                   Exsr      UPD_EDIVA
079300160121     c                   end
079400160121      *--
079500160121     c                   else
079600160121      *--
079700160121      ** non presente su EDIVAB/T   (su FIVAB ? o FNBLP ? oppure non c'era ?)
079800160121     c                   exsr      NON_SU_EDIVA
079900160121      *--
080000160121      *-  Se non c'è da NESSUNA parte, lo scrivo Parcheggiandolo
080100160121      *--   sul TIDP200F in attesa che arrivi poi la BOLLA sul GEODATA:
080200160121     c                   if         in_FNBLP <> 'S'
080300160121     c                                 and
080400160121     c                              in_FIVAB <> 'S'
080500160121     c                   exsr      WRT_TIDP2
080600160121     c                   end
080700160121      *--
080800160121     c                   end
080900160121      **
081000160121     C                   ENDSR
081100160121      * ?================================================================== */
081200160121     C*?   Lo scrive sul TIDP200F in parcheggio
081300160121      * ?================================================================== */
081400160121     C     WRT_TIDP2     BEGSR
081500160125      *
081600160125     c     i5parcel      chain     tidp200f
081700160120      *
081800160121     c                   eval      I2PARCEL    =    I5PARCEL
081900160121     c                   eval      I2SCANT     =    I5SCANT
082000160121     c                   eval      I2DEPCODE   =    I5DEPCODE
082100160121     c                   eval      I2DEPNAME   =    I5DEPNAME
082200160121     c                   eval      I2DATTIM    =    I5DATTIM                    <<----
082300160121     c                   eval      I2ROUTE     =    I5ROUTE
082400160121     c                   eval      I2TOUR      =    I5TOUR
082500160121     c                   eval      I2PCODE     =    I5PCODE
082600160121     c                   eval      I2SERVICE   =    I5SERVICE
082700160121     c                   eval      I2CCOUNTRY  =    I5CCOUNTRY
082800160121     c                   eval      I2CONSZIP   =    I5CONSZIP
082900160121     c                   eval      I2ADDSER1   =    I5ADDSER1
083000160121     c                   eval      I2ADDSER2   =    I5ADDSER2
083100160121     c                   eval      I2ADDSER3   =    I5ADDSER3
083200160121     c                   eval      I2WEIGHT    =    I5WEIGHT
083300160121     c                   eval      I2CUSTREF   =    I5CUSTREF
083400160121     c                   eval      I2PODIMAG   =    I5PODIMAG
083500160121     c                   eval      I2RECNAME   =    I5RECNAME
083600160121     c                   eval      I2INFOTXT   =    I5INFOTXT
083700160121      *
083800160125     c                   if        %Found(TIDP200F)
083900160125     c                   update    TIDP2000
084000160125     c                   else
084100160125     c                   write     TIDP2000
084200160125     c                   end
084300160121      **
084400160121     C                   ENDSR
084500160121      * ?================================================================== */
084600160121     C*?   NON ERA su EDIVAB   cerca su FNBLP e su FIVAB
084700160121      * ?================================================================== */
084800160121     C     NON_SU_EDIVA  BEGSR
084900160121      *
085000160121     c                   clear                   in_FNBLP          1
085100160121     c                   clear                   in_FIVAB          1
085200160121      *
085300160121      **  se già confermato in Partenza
085400160121     C                   eval        AR4TRC = 'I'
085500160121     C                   eval        AR4N14 = i5PARCEL
085600160121     C     Kar4I         chain     fiar404l
085700160121     c                   IF        %Found(fiar404l)
085800160121     c                   movel     i5PARCEL      DepotParcel
085900160121      **
086000160121      *   controlla che sia DPD con il Network e non con la linea
086100160121     c     ar4LNP        chain     azorg01l
086200160121     c                   if        %Found(azorg01l)
086300160121     c                   eval          OG143  =  orgDE3
086400160121     c                   if        §OGNTW = 'DPD'
086500160121     c                   eval          in_FNBLP = 'S'
086600160126     C                   eval      vinMSG = 'Già su BLP Confermato'
086700160121     c                   LEAVESR
086800160121     c                   end
086900160121     c                   end
087000160121      **
087100160121     c                   endIF
087200160121      ** ---------
087300160121      **  se non ancora confermato da Cliente
087400160121     C                   eval        vatTRC = 'E'
087500160121     C                   eval        vatNOT = i5PARCEL
087600160121     C     Kvat4E        setll     fivat02l
087700160121     C     vatTRC        reade     fivat02l
087800160205      *-
087900160205     c                   dow         not %EoF(fivat02l)
088000160222      *-
088100160222     c* imposta DS x reperire il Parcel
088200160205     c                   movel     vatNOT        dVATe
088300160222      *-
088400160222      *- se superata la chiave parcel esce dal ciclo x evitare letture inutili.
088500160222     C                   if           §vatEPRN > i5PARCEL
088600160222     c                   leave
088700160222     c                   end
088800160222      *-
088900160222     c* se la chiave è superiore esce dal ciclo
089000160222      *-
089100160121     C                   if          i5PARCEL = §vatEPRN
089200160205      *-
089300160121     c     vatLNP        chain     azorg01l
089400160121     c                   if        %Found(azorg01l)
089500160121     c                   eval      OG143 = orgDE3
089600160205      *-
089700160121     c                   if        §OGNTW = 'DPD'
089800160205      * Deve aggiornare il SERVICE CODE con l'ultimo arrivato
089900160205     c     vat_nrr       chain     fivat00f
090000160205     c                   if        %found(fivat00f)
090100160205     c                   eval      dVATe     = vatNOT
090200160205     c                   move      i5SERVICE     §VATESCD
090300160205     c                   eval      vatNOT    = dVATe
090400160205     c                   update    fivatFIS
090500160205     c                   end
090600160205      *
090700160121     c                   eval        vabCCM = vatCCM
090800160121     c                   move      i5Parcel      vabRMN
090900160121     c     kvab01        chain     fivab01l
091000160121     c                   if        %Found(fivab01l)
091100160205     C                   eval      vinMSG =
091200160205     C                             'Già su FIVAB aggiorna Peso/Serv.Code'
091300160121     c                   eval          in_FIVAB = 'S'
091400160205      *
091500160205      * Se passato il PESO lo sostituisce
091600160205     c                   if        I5WEIGHT > 0
091700160205     c                   Z-ADD     I5WEIGHT      VABPKB
091800160205      * CORREZIONI:
091900160205      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
092000160205     c                   IF        vabPKB > 31,5
092100160205     c                   eval           vabPKB = 30
092200160205     c                   elseIF    vabpkb =0
092300160205     c                   eval           vabpkb = 0,1
092400160205     c                   END
092500160205     c                   End
092600160205      *
092700160205     c                   update    fivab000
092800160205     c                   LEAVESR
092900160121     c                   end
093000160121     c                   end
093100160121      *
093200160121     c                   end
093300160205      *-
093400160205     c                   end
093500160205     C     vatTRC        reade     fivat02l
093600160121     c                   end
093700160121      **
093800160120     C                   ENDSR
093900160120      * ?================================================================== */
094000160120     C*?       ERA su EDIVAB in SOSPENSIONE in attesa dello SCAN 05
094100160120      * ?================================================================== */
094200160205     C     UPD_EDIVA     BEGSR
094300160120      *
094400160120      *  per chiave bolla aggiorna la bolla in SOSPENZIONE che adesso
094500160120      *  risulta partita con l'IGATEAVIS ricevuto (Manifest)
094600160120     C     KSosp_EDIVA   setll     EDIVAT3L
094700160120     C     KSosp_EDIVA   reade     EDIVAT3L
094800160120      *
094900160120     c                   DOw       not %EoF(EDIVAT3L)
095000160120      *
095100160120      *  sul record del Parcel/Service Code sostituisce il SERVICE CODE con quello
095200160120      *  passato nello SCAN 05
095300160120     C                   if        I5PARCEL = %SUBST(vatNOT:1:14)
095400160120     c                                  and
095500160120     C                               vatTRC = 'E'
095600160205     c                                  and
095700160205     C                             I5SERVICE <> *zeros
095800160205     c                   eval      dVATe     = vatNOT
095900160205     c                   move      i5SERVICE     §VATESCD
096000160205     c                   eval      vatNOT    = dVATe
096100160120     c                   End
096200160120      *
096300160120      *  sempre il nuovo CMR GEO_Data
096400160205     C                   if        new_CMR  <> *blank
096500160120     c                   eval      vatCMR  =  new_CMR
096600160205     c                   End
096700160120     c                   Update    EDIVAT00
096800160120      *
096900160120     C     KSosp_EDIVA   reade     EDIVAT3L
097000160120     c                   EndDO
097100160120      * ==========
097200160120      *  poi allinea anche la testata bolla del VAB
097300160120      * ==========
097400160120     C     KSosp_EDIVA   chain     EDIVAB2L
097500160120     c                   if        %Found(EDIVAB2L)
097600160120      *
097700160120      * Se passato il PESO lo sostituisce
097800160120     c                   if        I5WEIGHT > 0
097900160120     c                   Z-ADD     I5WEIGHT      VABPKB
098000160120      * CORREZIONI:
098100160120      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
098200160120     c                   IF        vabPKB > 31,5
098300160120     c                   eval           vabPKB = 30
098400160120     c                   elseIF    vabpkb =0
098500160120     c                   eval           vabpkb = 0,1
098600160120     c                   END
098700160205     c                   End
098800160120      *
098900160205     C                   if        new_CMR  <> *blank
099000160120     c                   eval           vabCMR = new_CMR
099100160205     c                   End
099200160229     c                   eval      vabSCL = 'G'
099300160120     c                   Update    EDIVAB00
099400160120     c                   End
099500160120      * ==========
099600160120     c                   Endsr
099700160120     C*----------------------------------------------------------------
099800160120      *  si compone il CMR correttamente eliminando "IN SOSPESO"
099900160120     C*----------------------------------------------------------------
100000160120     C     CAMBIA_CMR    BEGSR
100100160120      *
100200160120     c                   clear                   new_CMR          35
100300160120     c                   movel     I5DATTIM      I5DATAw           8 0
100400160120      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
100500160120     c     *iso          movel     DATEU         DATA_iso
100600160120    2c                   if        I5DATAw >0
100700160120     c                   TEST(D)                 I5DATAw                99
100800160120     c  n99*iso          movel     I5DATAw       DATA_iso
100900160120     c                   end
101000160120      *
101100160120      *  Da quale Depot è partito il pacco (aggiunge i giorni di viaggio)
101200160120     c                   exsr      gg_di_VIAGGIO
101300160120      *
101400160120      *   Se la data risultante è inferiore alla UDATE forza UDATE
101500160120     c                   move      data_iso      data_iso8         8 0
101600160120     c                   if        data_iso8 < dateu
101700160120     c                   move      dateu         data_iso8
101800160120     c                   end
101900160120      **
102000160120      * IMPOSTA IL CMR x LA SPEDIZIONE
102100160120     c                   eval      new_CMR = 'GEO_' + %editc(data_iso8:'Y')
102200160120      *
102300160120      **  Controlla se si tratta di gomme per poterle evidenziare nel CMR
102400160120     c                   if        i5SERVICE = Gomme_365 or
102500160120     c                             i5SERVICE = Gomme_366 or
102600160120     c                             i5SERVICE = Gomme_367
102700160120     c                   eval        new_CMR = %trim(new_CMR) + '_GOMME'
102800160120     c                   end
102900160120      *
103000160120     c                   Endsr
103100160120     C*----------------------------------------------------------------
103200160120     C*  in base al Depot di provenienza determina i giorni di viaggio
103300160120     C*----------------------------------------------------------------
103400160120     C     gg_di_VIAGGIO BEGSR
103500160120      *
103600160120      ** imposta il default
103700160120     c                   z-add     1             gg_incr           3 0
103800160120     c                   movel     I5DEPCODE     Start_depot       4
103900160120     c     ritenta       tag
104000160120      *
104100160120      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
104200160120      *   e se non fosse presente con i primi 4 caratteri del Parcel
104300160120     c                   if        Start_depot <> *blank
104400160120     c                   movel     Start_depot   DepotParcel
104500160120     c                   z-add     1             giro_nr           3 0
104600160120     c                   Else
104700160120     c                   z-add     2             giro_nr
104800160120     c                   movel     i5PARCEL      DepotParcel
104900160120     c                   End
105000160120      *
105100160120     C     key_Depot     chain     DPcdp01l
105200160120     c                   if        %Found(DPcdp01l)
105300160120      *
105400160120     C     key_Dep       setgt     DPpdc01l
105500160120     C     DepotParcel   readpe    DPpdc01l
105600160120     c                   dow       not %Eof(DPpdc01l)
105700160120      *   se trovato il Depot VALIDO
105800160120     c                   if        pdcDDE <= dateU and dateU <= pdcDSC
105900160120     c                   eval      DpdcFLO = pdcFLO
106000160120      *
106100160120     c                   if        §PDCFLOGGI <> *blank and
106200160120     c                             §PDCFLOGGI <> '000'  and
106300160120     c                             %check(digits:§PDCFLOGGI)=0
106400160120     c                   move      §PDCFLOGGI    gg_incr           3 0
106500160120     c                   leave
106600160120     c                   end
106700160120      *
106800160120     c                   end
106900160120      *
107000160120     C     DepotParcel   readpe    DPpdc01l
107100160120     c                   Enddo
107200160120     c                   End
107300160120      *
107400160120     c                   if        §PDCFLOLNP = '000' or
107500160120     c                             §PDCFLOLNP = *blank
107600160120     c                   if          giro_nr = 1
107700160120     c                   clear                   Start_depot
107800160120     c                   goto      ritenta
107900160120     c                   End
108000160120     c                   End
108100160120      *
108200160120      *=============
108300160120      *  se Non c'è incremento
108400160120     c                   if        gg_incr = 0
108500160120     c                   exsr      test_gg_lav
108600160120     c                   else
108700160120      *  aggiunge x giorni (ma solo lavorativi)
108800160120     c                   do        gg_incr       giorni            3 0
108900160120     c                   adddur    1:*D          data_iso
109000160120     c                   exsr      test_gg_lav
109100160120     c                   enddo
109200160120     c                   endif
109300160120      *=============
109400160120      *
109500160120     c                   Endsr
109600160120     C*----------------------------------------------------------------
109700160120     C*  test sui GG lavorativi altrimenti CERCA fino al primo lavorativo
109800160120     C*----------------------------------------------------------------
109900160120     C     test_gg_lav   BEGSR
110000160120      *
110100160120      * -----------
110200160120      * Cerca definitivamente il D-day lavorativo
110300160120      *  esclude sia il sabato che la domenica
110400160120      * -----------
110500160120     c                   do        *hival
110600160120     c                   move      data_iso      dadata
110700160120     c                   move      data_iso      adata
110800160120     c                   CALL      'XSRLAV8'
110900160120     c                   PARM                    Wdata8
111000160120      * se corrisponde ho trovato il D-Day
111100160120     c                   if        giolav <> 0
111200160120     c                   leave
111300160120     c                   else
111400160120      * altrimenti continuo a cercare il primo giorno lavorativo valido.
111500160120     c     data_iso      adddur    1:*d          data_iso
111600160120     c                   iter
111700160120     c                   endif
111800160120     c                   enddo
111900160413      *
112000160413     c                   Endsr
112100160413     C*----------------------------------------------------------------
112200160413     C*  SCAN 18 X PARCEL SHOP
112300160413     C*----------------------------------------------------------------
112400160413     C     SCAN_18_PUDO  BEGSR
112500160413      *
112600160413      * ------ passa la DS completa e riceverà lo SCAN sostitutivo
112700160413      *        relativo all'evento del PUDO. Controlla e cancella
112800160413      *        eventuale SCAN 13 in attesa che avrebbe dato automaticamente
112900160413      *        x consegnata la spedizione.
113000160413     c                   call      'FIEU02R18'
113100160413     c                   parm                    TIDP500
113200160120      *
113300160120     c                   Endsr
113400160120      *  ------------------------------------------------------------------ */
