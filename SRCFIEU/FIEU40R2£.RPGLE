000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300060915     H DECEDIT('0,') DATEDIT(*YMD/)
000400060615      **?-------------------------------------------------------------------*****
000500060613      *  Da UPLOAD                                                              *
000600060809      *  TRASCODIFICA : STATUSDATACR                                            *
000700060809      *               ORM da DPD a BARTOLINI                                    *
000800060614      *                                nuovo parcel e depot                     *
000900060615      **?-------------------------------------------------------------------*****
001000060612     Ftivin00r  uF   E             DISK    usropn
001100060809     FFIDPO06L  uF   E           k DISK
001200060809     FFnORM01L  uF   E           k DISK
001300060809     FFnORF01L  iF a E           k DISK
001400060809     FtnTBE01L  iF   E           k DISK
001500060809      *
001600000907      *-------------------
001700060612     D Lung_Parcel     S              3  0
001800060614     D Tipo_error      S              1  0
001900060413      *-------------------
002000060809     D dMRD          e DS
002100060809      *
002200060809     D  data_Oggi      s              8  0 INZ(0)
002300060809     D  day            s              8  0 INZ(0)
002400060809     D  ore            s              6  0 INZ(0)
002500060809     D  w0140          s             14  0 INZ(0)
002600060809     D*
002700060809     D WLBDAT          DS
002800060809     D  G02DAT                 1      8  0
002900060809     D  G02INV                 9     16  0
003000060809     D  G02ERR                17     17
003100060809     D  G02TGI                18     22  0
003200060809
003300060809     D  cBAR           s              2    DIM(99)
003400060809     D  cDPD           s              3  0 DIM(99)
003500060809     D  cFAS           s              3  0 DIM(99)
003600080403      *
003700080403      * motivo generico di Mancato ritiro
003800080403     d staddserX       s              3  0
003900060809
004000060809      * ?   *--------------------------------------------------------------*
004100060809      * ?   ( Descrizione Campi x scomposizione FLAT record )
004200060809      * ?   *--------------------------------------------------------------*
004300060809     D FIDPOSTS      E DS
004400060612      * ?   * Campi da tradurre con interi
004500060623     D atoll           PR            20I 0
004600060623     D                                     EXTPROC('atoll')
004700060612     D  Char                           *
004800060612     D                                     VALUE
004900060612     D                                     OPTIONS(*STRING)
005000060623      * ?   * Campi da tradurre con interi
005100060623     D atoi            PR            10I 0
005200060623     D                                     EXTPROC('atoi')
005300060623     D  Char                           *
005400060623     D                                     VALUE
005500060623     D                                     OPTIONS(*STRING)
005600060612      * ?   * Campi da tradurre con decimali
005700060612     D atof            PR             8F
005800060612     D                                     EXTPROC('atof')
005900060612     D  Char                           *
006000060612     D                                     VALUE
006100060612     D                                     OPTIONS(*STRING)
006200060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
006300060612      * ?   * Campi x decodifica * (INPUT  del Record)
006400060612     D  Dati           s           1500
006500060612     D Separa_campi    s              1
006600060612     D Decimal_Separ   s              1
006700060612      * ?   * Campi decodificati * (OUTPUT del Record)
006800060612      *       Schiere di Output / Campi di Output
006900060614     D  Alfa40         s             40
007000060612     d  Sk             s              3u 0
007100060612     D    NR_campi     s              3u 0
007200060612     D  Campi_Record   ds
007300060612     D    Dato_sk                    40    DIM(100)
007400060612     D  Campi_Lunghi   ds
007500060612     D    LunDato_sk                  3u 0 DIM(100)
007600060612     D Campi_Numerici  ds
007700060612     D    DatoNum_sk                  1    DIM(100)
007800060612     D Campi_Decimali  ds
007900060612     D    Decimal_sk                  3u 0 DIM(100)
008000060615      * ? ------------------------------------------------------------------
008100060612     D  position       s              3  0 INZ(0)
008200060612
008300060612     D  se_errore      s              1    inz(' ')
008400060612     D  Msg_Err        s            132    inz(' ')
008500060614     D  Msg_Vin_80     s             80    inz(' ')
008600060619     D Testa_Sk        s             10    DIM(50)
008700060809
008800060809     D Digits          C                   CONST('0123456789')
008900060615      * ?______________________________________________________________     */
009000060612      *   Ciclo principale
009100060615      * ?______________________________________________________________     */
009200060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
009300060612     C*
009400060612     C                   if        not %open(tivin00r)
009500060612     C                   open      tivin00r
009600060612     C                   endif
009700060612     C*? Controllo dati arrivati da DPD
009800060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
009900060612      * ?- controllando che la trasmissione sia completa.
010000060612
010100060612     c                   exsr      check_Trasm
010200060612
010300060613      **  Errore di trasmissione x tutti i records
010400060613      **   --> file in errore
010500060613     c                   if        se_errore = 'S'
010600060612
010700060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
010800060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
010900060614     C                               >> Manca Testata Messaggio !! +
011000060614     C                               Farsi rimandare i DATI !!'
011100060612
011200060612      * ?  Scrive su tutti i records il tipo di errore
011300060612     c     *start        setll     tivin00r
011400060612     c                   read      tivin00r
011500060612     c                   dow       not %eof(tivin00r)
011600060612     C                   eval      vinMSG = Msg_Err
011700060612     C                   eval      vinFLG = '2'
011800060613     c                   eval      esito  = '2'
011900060612     c                   update    tivin000
012000060612     c                   read      tivin00r
012100060612     c                   endDO
012200060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
012300060615     c                   else
012400060623
012500060612     c                   exsr      Importa_Msg
012600060612
012700060614     c                   end
012800060614
012900060612     C                   if        %open(tivin00r)
013000060612     C                   close     tivin00r
013100060612     C                   endif
013200060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
013300060614      *
013400060710     c                   if        almeno_uno ='S' and esito ='1'
013500060710     C                   eval      esito ='0'
013600060614     C                   endif
013700060614      *
013800060612     c                   SETON                                        LR
013900060615      * ?________________________________________________________________   */
014000060615      *? Controlla la trasmissione  primo record descrittivo tracciato
014100060615      * ?________________________________________________________________   */
014200060612     c     Check_Trasm   Begsr
014300060612
014400060612     C                   clear                   se_errore
014500060614      ** primo  record con intestazione dei campi
014600060612     c     *start        setll     tivin00r
014700060612     c                   read      tivin00r
014800060612     c                   if        not %eof(tivin00r)
014900060615      *
015000060612     c                   exsr      Decod_Record
015100060630      *
015200060630      **  Se presente un errore nel record emette una segnalazione msg
015300060630     c                   if        se_errore <> 'S'
015400060630     C                   eval      vinFLG = '1'
015500060710     c                   eval      esito  = '0'
015600060630     c                   update    tivin000
015700060630     c                   endif
015800060615      *
015900060612     c                   endif
016000060612
016100060612     c                   endSR
016200060615      * ?________________________________________________________________   */
016300060612      *? Importa i records della tramsissione
016400060615      * ?________________________________________________________________   */
016500060612     c     Importa_Msg   Begsr
016600060612
016700060614      * per controllare se almeno un record è stato importato sul VAB
016800060614     c                   clear                   Almeno_Uno        1
016900060614
017000060623      * Continua a leggere dal secondo record.
017100060623      *   Il primo è la testata già controllata dalla Check_trasm.
017200060612     c                   read      Tivin00r
017300060612
017400060612     c                   dow       not %eof(Tivin00r)
017500060614
017600060614      * solo i record sflaggati da rielaborare
017700060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
017800060612
017900060612      ** Controlli formali sui campi
018000060612     c                   clear                   se_errore
018100060620     c                   clear                   Msg_Vin_80
018200060612
018300060612      ** Decodifica record a campi variabili
018400060612     c                   exsr      Decod_Record
018500060615      *
018600060612      **  Se presente un errore nel record emette una segnalazione msg
018700060612     c                   if        se_errore ='S'
018800060614     C                   eval      vinMSG = Msg_Vin_80
018900060623     C                   eval      vinFLG = '2'
019000060623     c                   eval      esito  = '1'
019100060612     c                   else
019200060612
019300060612      *  con il record in canna scrive direttamente il VAB ed il VAT
019400060809     c                   exsr      WRT_FILE
019500060614
019600060614      *  Problemi nella decodifica dei campi VAB/VAT
019700060614     c                   if        se_errore ='S'
019800060614     C                   eval      vinMSG = Msg_Vin_80
019900060614     C                   eval      vinFLG = '2'
020000060623     c                   eval      esito  = '1'
020100060614     c                   else
020200060614      *  Record OK
020300060615     c                   move      'S'           Almeno_Uno
020400060612     C                   eval      vinFLG = '1'
020500060614     C                   eval      vinMSG = *Blank
020600060614     c                   end
020700060612
020800060612     c                   end
020900060612
021000060612     c                   update    Tivin000
021100060614     c                   endIF
021200060612
021300060612     c                   read      Tivin00r
021400060612     C                   ENDdo
021500060612      **
021600060612     c                   endSR
021700060615      * ?______________________________________________________________     */
021800060612      *?    Decodifica record a campi variabili
021900060615      * ?______________________________________________________________     */
022000060612     c     Decod_Record  Begsr
022100060612
022200060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
022300060612     c                   movel(p)  VINDTA        dati
022400060612      **
022500060612     c                   exsr      Split_Rec
022600060612      **
022700060612      **  Records di Intestazione e di fine Msg
022800060614     c                   if        %subst(dati:1:8) ='PARCELNO'
022900060614     c                   exsr      Rec_Testata
023000060615      *
023100060612     c                   Else
023200060612      ** conta le righe
023300060612     c                   exsr      Rec_Dettaglio
023400060615      * ?
023500060612     c                   End
023600060612      **
023700060612     c                   Endsr
023800060615      * ?________________________________________________________________   */
023900060614      *?      Record di Testata
024000060615      * ?________________________________________________________________   */
024100060614     c     Rec_Testata   Begsr
024200060612      * ?  Prima riga
024300060620     c     'PARCELNO'    SCAN      dati:1        position
024400060614     c                   if        %Found and position < 10
024500060614      ** esempio di record :
024600060614      * ?   PARCELNO;SCAN_CODE;DEPOT_CODE;DEPOTNAME;EVENT_DATE_TIME.......
024700060614      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
024800060614     C                   clear                   Testa_Sk
024900060614      * ? Dal 1° campo troviamo i nomi delle intestazioni dei campi C.Request
025000060614     C     1             do        50            sk
025100060614     c                   eval      Testa_Sk(sk) = %subst(Dato_sk(sk):1:+
025200060614     c                             LunDato_sk(sk))
025300060614     c                   enddo
025400060614      **
025500060612     c                   ElSe
025600060614      * ?    Se l'inizio del file non ha la testata errore di trasmissione
025700060614      * ?-----> Errore
025800060614     C                   eval      se_errore = 'S'
025900060614
026000060612     c                   End
026100060612      **
026200060612     c                   Endsr
026300060615      * ?_______________________________________________________________    */
026400060612      *?      Riga Bolla Import da controllare e tradurre
026500060615      * ?_______________________________________________________________    */
026600060612     c     Rec_Dettaglio Begsr
026700060809      **
026800060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
026900060612      **
027000060809     c                   clear                   FIDPOSTS
027100060614     c                   clear                   Msg_Err
027200060809      **
027300060614      * ? Nel 1° campo : Parcel Number               (Oblligatorio)  PARCELNO
027400060614      **                                                             Numero Parcel
027500060614     C                   z-add     1             sk
027600060614     c                   If        LunDato_sk(sk) =  0
027700060614      * ?-----> Errore
027800060614     c                   eval      tipo_error = 1
027900060614     c                   exsr      Field_Error
028000060614     c                   Else
028100060809     c                   eval      STPARCEL   = %subst(Dato_sk(sk):1:+
028200060614     c                                              LunDato_sk(sk))
028300060614     c                   End
028400060614      **
028500060614      * ? Nel 2° campo : Codice evento SCAN  DPD     (Oblligatorio)  SCAN_CODE
028600060614      **                                                             Codice SCAN
028700060614     C                   add       1             sk
028800060614     c                   If        DatoNum_sk(sk) = 'N' or
028900060614     c                             LunDato_sk(sk) =  0
029000060614      * ?-----> Errore
029100060614     c                   eval      tipo_error = 2
029200060614     c                   exsr      Field_Error
029300060614     c                   Else
029400060809     c                   eval      STSCANT  = atoi(%TRIM(Dato_sk(sk)))
029500060614     c                   End
029600060809      **
029700060614      * ? Nel 3° campo : DEPOT che ha fatto lo SCAN  (Oblligatorio)  DEPOT_CODE
029800060614      **                                                             Codice Depot
029900060614     C                   add       1             sk
030000060614     c                   If        LunDato_sk(sk) =  0
030100060614      * ?-----> Errore
030200060614     c                   eval      tipo_error = 1
030300060614     c                   exsr      Field_Error
030400060614     c                   Else
030500060809     c                   eval      STDEPCODE  = %subst(Dato_sk(sk):1:+
030600060614     c                                              LunDato_sk(sk))
030700060614     c                   End
030800060809      **
030900060630      * ? Nel 4° campo : NOME DEPOT                                  DEPOTNAME
031000060614      **                                                             Nome Depot
031100060614     C                   add       1             sk
031200060630     c                   If        LunDato_sk(sk) >  0
031300060809     c                   eval      STDEPNAME  = %subst(Dato_sk(sk):1:+
031400060614     c                                              LunDato_sk(sk))
031500060614     c                   End
031600060809      **
031700060630      * ? Nel 5° campo : Data Ora SCAN             Obbligatorio      DATE/TIME
031800060614      **                                                             DATA/ORA Evento
031900060614     C                   add       1             sk
032000060614     c                   If        DatoNum_sk(sk) = 'N' or
032100060614     c                             LunDato_sk(sk) =  0
032200060614      * ?-----> Errore
032300060614     c                   eval      tipo_error = 2
032400060614     c                   exsr      Field_Error
032500060614     c                   Else
032600060809     c                   eval      STDATTIM = atoll(%TRIM(Dato_sk(sk)))
032700060614     c                   End
032800060809      **
032900060614      * ? Nel 6° campo : ROUTE non sempre presente                   ROUTE
033000060614      **                                                             ROUTE
033100060614     C                   add       1             sk
033200060614     c                   If        LunDato_sk(sk) >  0
033300060809     c                   eval      STROUTE    = %subst(Dato_sk(sk):1:+
033400060614     c                                              LunDato_sk(sk))
033500060614     c                   End
033600060809      **
033700060614      * ? Nel 7° campo : TOUR  non sempre presente                   TOUR
033800060614      **                                                             TOUR
033900060614     C                   add       1             sk
034000060630     c                   If        DatoNum_sk(sk) = 'N' and
034100060614     c                             LunDato_sk(sk) >  0
034200060614      * ?-----> Errore
034300060614     c                   eval      tipo_error = 3
034400060614     c                   exsr      Field_Error
034500060614     c                   Else
034600060809     c                   eval      STTOUR     =  atoi(%TRIM(Dato_sk(sk)))
034700060614     c                   End
034800060809      **
034900060630      * ? Nel 8° campo : TIPO PARCEL 01/02                           PCODE
035000060630      **                                                             Parcel Code
035100060614     C                   add       1             sk
035200060630     c                   If        DatoNum_sk(sk) = 'N' and
035300060630     c                             LunDato_sk(sk) >  0
035400060614      * ?-----> Errore
035500060630     c                   eval      tipo_error = 3
035600060614     c                   exsr      Field_Error
035700060614     c                   Else
035800060809     c                   eval      STPCODE  = atoi(%TRIM(Dato_sk(sk)))
035900060614     c                   End
036000060809      **
036100060630      * ? Nel 9° campo : Codice di Servizio                          SERVICE
036200060614      **                                                             SERVICE CODE
036300060614     C                   add       1             sk
036400060630     c                   If        DatoNum_sk(sk) = 'N' and
036500060630     c                             LunDato_sk(sk) >  0
036600060614      * ?-----> Errore
036700060630     c                   eval      tipo_error = 3
036800060614     c                   exsr      Field_Error
036900060614     c                   Else
037000060809     c                   eval      STSERVICE  = atoi(%TRIM(Dato_sk(sk)))
037100060614     c                   End
037200060809      **
037300060630      * ? Nel 10° campo : Nazione Destinatario                       CONSEGNEE_COUNTRY_C
037400060614      **                                                             NAZIONE DESTINATARIO
037500060614     C                   add       1             sk
037600060630     c                   If        DatoNum_sk(sk) = 'N' and
037700060630     c                             LunDato_sk(sk) >  0
037800060614      * ?-----> Errore
037900060630     c                   eval      tipo_error = 3
038000060614     c                   exsr      Field_Error
038100060614     c                   Else
038200060809     c                   eval      STCCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
038300060614     c                   End
038400060809      **
038500060630      * ? Nel 11° campo : Cod.Postale Destinatario                   CONSEGNEE_ZIP
038600060614      **                                                             CAP DESTINATARIO
038700060614     C                   add       1             sk
038800060614     c                   If        LunDato_sk(sk) >  0
038900060809     c                   eval      STCONSZIP  = %subst(Dato_sk(sk):1:+
039000060614     c                                              LunDato_sk(sk))
039100060614     c                   End
039200060809      **
039300060614      * ? Nel 12° campo : CODICI AGGIUNTIVI                          ADD_SERVICE1
039400060614      **                                                             cod.AGG. 1
039500060614     C                   add       1             sk
039600060614     c                   If        DatoNum_sk(sk) = 'N' and
039700060614     c                             LunDato_sk(sk) >  0
039800060614      * ?-----> Errore
039900060614     c                   eval      tipo_error = 3
040000060614     c                   exsr      Field_Error
040100060614     c                   Else
040200060809     c                   eval      STADDSER1  =  atoi(%TRIM(Dato_sk(sk)))
040300060614     c                   End
040400060809      **
040500060614      * ? Nel 13° campo : CODICI AGGIUNTIVI                          ADD_SERVICE2
040600060614      **                                                             cod.AGG. 2
040700060614     C                   add       1             sk
040800060614     c                   If        DatoNum_sk(sk) = 'N' and
040900060614     c                             LunDato_sk(sk) >  0
041000060614      * ?-----> Errore
041100060614     c                   eval      tipo_error = 3
041200060614     c                   exsr      Field_Error
041300060614     c                   Else
041400060809     c                   eval      STADDSER2  =  atoi(%TRIM(Dato_sk(sk)))
041500060614     c                   End
041600060809      **
041700060614      * ? Nel 14° campo : CODICI AGGIUNTIVI                          ADD_SERVICE3
041800060614      **                                                             cod.AGG. 3
041900060614     C                   add       1             sk
042000060614     c                   If        DatoNum_sk(sk) = 'N' and
042100060614     c                             LunDato_sk(sk) >  0
042200060614      * ?-----> Errore
042300060614     c                   eval      tipo_error = 3
042400060614     c                   exsr      Field_Error
042500060614     c                   Else
042600060809     c                   eval      STADDSER3  =  atoi(%TRIM(Dato_sk(sk)))
042700060614     c                   End
042800060809      **
042900060614      * ? Nel 15° campo : PESO                                       WEIGHT
043000060614      **                                                             Peso in KG
043100060614     C                   add       1             sk
043200060614     c                   If        DatoNum_sk(sk) = 'N' and
043300060614     c                             LunDato_sk(sk) >  0
043400060614      * ?-----> Errore
043500060614     c                   eval      tipo_error = 3
043600060614     c                   exsr      Field_Error
043700060614     c                   Else
043800060809     c                   eval      STWEIGHT   =  atof(%TRIM(Dato_sk(sk)))
043900060614     c                   End
044000060809      **
044100060614      * ? Nel 16° campo : Riferimento Cliente                        CUSTOMER_REFERENCE
044200060614      **                                                             Riferimento Cliente
044300060614     C                   add       1             sk
044400060614     c                   If        LunDato_sk(sk) >  0
044500060809     c                   eval      STCUSTREF  = %subst(Dato_sk(sk):1:+
044600060614     c                                              LunDato_sk(sk))
044700060614     c                   End
044800060809      **
044900060614      * ? Nel 17° campo : POD IMAGE di riferimento                   POD_IMAGE_REF
045000060614      **                                                             Riferimento POD IMAGE
045100060614     C                   add       1             sk
045200060614     c                   If        DatoNum_sk(sk) = 'N' and
045300060614     c                             LunDato_sk(sk) >  0
045400060614      * ?-----> Errore
045500060614     c                   eval      tipo_error = 3
045600060614     c                   exsr      Field_Error
045700060614     c                   Else
045800060809     c                   eval      STPODIMAG  =  atoi(%TRIM(Dato_sk(sk)))
045900060614     c                   End
046000060809      **
046100060614      * ? Nel 18° campo : FIRMA di chi ha ricevuto la merce          RECEIVER_NAME
046200060614      **                                                             FIRMA
046300060614     C                   add       1             sk
046400060614     c                   If        LunDato_sk(sk) >  0
046500060809     c                   eval      STRECNAME  = %subst(Dato_sk(sk):1:+
046600060614     c                                              LunDato_sk(sk))
046700060614     c                   End
046800060809      **
046900060614      * ? Nel 18° campo : INFO TESTO                                 INFO_TEXT
047000060614      **                                                             TESTO AGGIUNTIVO
047100060614     C                   add       1             sk
047200060614     c                   If        LunDato_sk(sk) >  0
047300060809     c                   eval      STINFOTXT  = %subst(Dato_sk(sk):1:+
047400060614     c                                              LunDato_sk(sk))
047500060614     c                   End
047600060809      **
047700060809      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL FIDPOSTS
047800060612      **
047900060612     c                   Endsr
048000060615      * ?____________________________________________________________       */
048100060614      *?      Campi in errore
048200060615      * ?____________________________________________________________       */
048300060614     c     Field_Error   Begsr
048400060612      **
048500060614     C                   eval      se_errore   = 'S'
048600060614      * msg video o in posta
048700060614     c                   If        Msg_Err = *blank
048800060614     c
048900060614     c                   Select
049000060614     c
049100060614     c                   When      Tipo_Error = 1
049200060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
049300060614     C                             ' >> Obblig.- Manca Dato !!'
049400060614     c
049500060614     c                   When      Tipo_Error = 2
049600060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
049700060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
049800060614     c
049900060614     c                   When      Tipo_Error = 3
050000060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
050100060614     C                             ' >> NON Numerico  !!'
050200060614     c                   EndSL
050300060614     c
050400060614     c                   End
050500060614      * Errore su VINMSG
050600060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
050700060614     c                                          + %trim(Testa_sk(sk))
050800060614      **
050900060614     c                   Endsr
051000060615      * ?____________________________________________________________       */
051100060612      *?      Suddivide i campi della riga con carattere divisorio (;)
051200060615      * ?____________________________________________________________       */
051300060612     c     Split_Rec     Begsr
051400060612      **
051500060612     c                   clear                   Nr_campi
051600060612     c                   clear                   Campi_Record
051700060612     c                   clear                   Campi_Lunghi
051800060612     c                   clear                   Campi_Numerici
051900060612     c                   clear                   Campi_Decimali
052000060612     c                   movel     ';'           Separa_campi
052100060612     c                   movel     '.'           Decimal_Separ
052200060612      *
052300060612     c                   call      'FIEU00R'
052400060612      * ?     Input
052500060612     c                   parm                    Separa_campi
052600060612     c                   parm                    Decimal_Separ
052700060612     c                   parm                    dati
052800060612      * ?     Output  su schiere di 100 elementi
052900060612     c                   parm                    Nr_campi
053000060612     c                   parm                    Campi_Record                   * i dati
053100060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
053200060612     c                   parm                    Campi_Numerici                 * se numerici
053300060612     c                   parm                    Campi_Decimali                 * quanti decimali
053400060612      **
053500060612     c                   Endsr
053600060809      * ?_______________________________________________________________    */
053700060809     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
053800060809      * ?_______________________________________________________________    */
053900060809     C     WRT_FILE      BEGSR
054000060809      **
054100060809      **  Se è in questo punto significa che non sono stati trovati errori
054200060809      **    formali quindi si passa gli status per avanzamento ORM
054300080407      *
054400080527      *
054500080527      *   27/05/2008:
054600080527      ** ?  AL MOMENTO E' STATO DISATTIVATO
054700080527      ** ?  se per errore girasse l'UPLOAD :
054800080527      ** ?  NON DEVE FARE NULLA
054900080527      **?              /*********************/
055000080527     c                   goto      ENDSR_WRT
055100080527      **?              /*********************/
055200080527      *
055300080527      *
055400080407      *
055500080407      ** ?  Ma ignora tutti gli altri SCAN eccetto lo SCAN 18 con il motivo
055600080407      ** ?  di Mancato RITIRO.
055700080407     c                   if        STSCANT = 18
055800060809      **?              /*---------------------- */
055900060809     c                   ExSr      Avanza_ORM
056000060809      * ?              /*---------------------- */
056100080407     c                   End
056200080527      *
056300080527      *
056400080527      *
056500060809      *
056600080527     c     ENDSR_WRT     Endsr
056700060809      * ?------------------------------------------------------------------ */
056800060809      *?    Imposta i campi per il DPO e ORM/ORF
056900060809      * ?------------------------------------------------------------------ */
057000060809     c     Avanza_ORM    Begsr
057100060809      **
057200060809     c     STPARCEL      chain     fidpo06l
057300060809     c                   if        %Found(fidpo06l)
057400080516      **
057500080516      ** se non ha già ricevuto nel CRDATA02: (status 4) l'exception code
057600080528     c**************     If        dpoSTS <> 4  or
057700080528     c                   If        dpoSTS  = 4  and DPOCDMR = *blank
057800060809      **
057900060809      **  Cosa ha causato il Mancato Ritiro ????
058000060809     c                   if        STSCANT = 18
058100080516      **
058200060809     c                   clear                   cod_mancRit       2
058300060809     c                   z-add     1             cd
058400060809     c     STADDSER1     lookup    cDPD(cd)                               33
058500080403     c                   z-add     STADDSER1     STADDSERx
058600080403      *
058700080403     c                   if        *in33=*off and STADDSER2 > 0
058800080403     c                   z-add     1             cd
058900080403     c     STADDSER2     lookup    cDPD(cd)                               33
059000080403     c                   z-add     STADDSER2     STADDSERx
059100080403     c                   endIf
059200080403      *
059300080403     c                   if        *in33=*off and STADDSER3 > 0
059400080403     c                   z-add     1             cd
059500080403     c     STADDSER3     lookup    cDPD(cd)                               33
059600080403     c                   z-add     STADDSER3     STADDSERx
059700080403     c                   endIf
059800080403      *
059900080528      *  Se non è codificato nella tabella MRD non deve fare NULLA
060000080528      *   rilascia solo il record aggiornandolo.
060100080528     c  n33              goto      END_AVANZA_ORM
060200080528      *
060300080528      *
060400061109     c   33              move      cBAR(cd)      cod_mancRit
060500061109      *   ANNULLAMENTO PRIMA DELL'AFFIDAMENTO --> non sapendo quale mettere .......
060600061109     c  n33              move      '80'          cod_mancRit
060700060809      **
060800060809      ** Controlla sull'ORM la Fase di avanzamento
060900060809     c     KOrM1         chain     fnOrm01l
061000060809
061100060809      **  Controllo Fase ORM se tutto ok dovrebbe essere ancora non chiusa
061200060809      **   se invece è oltre --> errore
061300060921     c                   if        not %Found(fnorm01l)
061400060921      ** se addirittura non esiste il file  (Veramente Strano)
061500060921      **   imposta il codice di errore --> '10' x ricordare di non aver trovato il
061600060921      **    nostro ORM
061700060921     c                   movel     '10'          DPOCODER
061800060921     c                   else
061900060809      **
062000060809     c                   if        ORmFAO < 900
062100060809      ** aggiorna
062200060809      ** avanzamento fase su testata ORM (senza distinta/padr.)
062300060809     c                   z-add     999           ormFAO
062400080516     c                   z-add     cFAS(cd)      ormFAO
062500080516      ****
062600060809     c                   z-add     data_oggi     ormDFO
062700060809     c                   z-add     ore           ormOFO
062800060809     c                   update    fnORM000
062900060809
063000060809      **  Allinea le Fasi in dettaglio
063100060809     c     KOrM1         setgt     fnOrf01l
063200060809     c     KOrM1         readpe    fnOrf01l
063300060809     c                   if        not %Eof(fnorf01l)
063400060809      **  aggiunge la nuova fase --> 999
063500060921     c                   eval      ORFCAR = cod_mancRit
063600060921     c                   z-add     ormFAO        ORFFAR
063700060809     c                   z-add     data_oggi     ORFDAE
063800060809     c                   z-add     data_oggi     ORFDFC
063900060809     c                   z-add     ore           ORFORE
064000060809     c                   eval      ORFPUE = 'ORMDPD_AUT'
064100060809     c                   write     fnorf000
064200060809     c                   end
064300060809      **
064400060809     c                   else
064500060809      **  Se non era nella fase che si aspettava
064600060921      **   imposta il codice di errore --> '01' ORM per fase non congrua.
064700061002     c                   if        ORmFAO <> 999
064800060921     c                   movel     '01'          DPOCODER
064900060809     c                   end
065000061002     c                   end
065100060809      **
065200060809     c                   end
065300060809      **
065400060809     c                   else
065500060809      ** Se non riceve uno scan 18 segnala il problema sul FIDPO00F
065600080407      **
065700080407      ** ?  Questo non è + vero. DPD invia altri SCAN oltre il 18.
065800080407     c*******            movel     '09'          DPOCODER
065900060809     c                   end
066000060809      **
066100060809      **  Quindi aggiorna FIDPO con o senza l'errore con il Nr.Parcel
066200060809     c                   z-add     8             DPOSTS
066300080403     c******             move      STADDSER1     DPOCDMR
066400080403     c                   move      STADDSERx     DPOCDMR
066500060809     c                   z-add     data_oggi     DPODTRIC
066600080516     c*
066700080516     c                   end
066800080516      **
066900080528     c     END_AVANZA_ORMtag
067000060809     c                   update    fidpo000
067100060809     c                   end
067200060809      **
067300080528     c                   Endsr
067400060615      * ?_______________________________________________________________    */
067500060615     C     *INZSR        BEGSR
067600060615      * ?_______________________________________________________________    */
067700991129     c     *ENTRY        PLIST
067800060613     C                   parm                    esito             1
067900060809      **
068000060809     c     Korm1         klist
068100060809     c                   kfld                    DPOPOE
068200060809     c                   kfld                    DPONRS
068300060809     c                   kfld                    DPONOR
068400060809     c                   kfld                    DPONRV
068500060809      *
068600060809      *  imposta UDATE di elaborazione
068700060915     C                   move      *date         data_oggi
068800060915     C                   move      *date         G02INV
068900060915     C                   MOVE      *ZEROS        G02DAT
069000060809     C                   MOVE      *BLANKS       G02ERR
069100060809     C                   CALL      'XSRDA8'
069200060809     C                   PARM                    WLBDAT
069300060809     C                   move      data_oggi     day
069400060809      * Recupero data e ora
069500060809     C                   TIME                    W0140
069600060809     C                   MOVEl     W0140         ore
069700060809      **
069800060809     c                   move      'MRD'         tbeCOD
069900060809     c                   clear                   cBAR
070000060809     c                   clear                   cDPD
070100060809     c                   z-add     0             cd                3 0
070200060809      **
070300060809     c     tbeCOD        setll     tnTBE01l
070400060809     c     tbeCOD        reade     tnTBE01l
070500060809      **
070600060809     c                   dow       not %Eof(tnTBE01l)
070700060809     c                   add       1             cd
070800060809     c                   movel     tbeke1        cBAR(cd)
070900060809     c                   movel     tbeuni        dMRD
071000060809     c                   movel     §MRDCAR       cDPD(cd)
071100060809     c                   movel     §MRDFAR       cFAS(cd)
071200060809     c     tbeCOD        reade     tnTBE01l
071300060809     c                   Enddo
071400050414      *
071500991124     C                   ENDSR
071600060615      * ?_______________________________________________________________    */
071700060615     C     *pssr         BEGSR
071800060615      * ?_______________________________________________________________    */
071900060614     C                   eval      esito = '2'
072000060614     C*
072100060614     C                   ENDSR     '*CANCL'
072200060614      *-------------------------------------------------------------------------
