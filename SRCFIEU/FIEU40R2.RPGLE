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
002000170228     d dfar          e ds
002100170228      *-------------------
002200060809     D dMRD          e DS
002300060809      *
002400060809     D  data_Oggi      s              8  0 INZ(0)
002500060809     D  day            s              8  0 INZ(0)
002600060809     D  ore            s              6  0 INZ(0)
002700060809     D  w0140          s             14  0 INZ(0)
002800060809     D*
002900060809     D WLBDAT          DS
003000060809     D  G02DAT                 1      8  0
003100060809     D  G02INV                 9     16  0
003200060809     D  G02ERR                17     17
003300060809     D  G02TGI                18     22  0
003400060809
003500060809     D  cBAR           s              2    DIM(99)
003600060809     D  cDPD           s              3  0 DIM(99)
003700060809     D  cFAS           s              3  0 DIM(99)
003800080403      *
003900080403      * motivo generico di Mancato ritiro
004000080403     d staddserX       s              3  0
004100060809
004200060809      * ?   *--------------------------------------------------------------*
004300060809      * ?   ( Descrizione Campi x scomposizione FLAT record )
004400060809      * ?   *--------------------------------------------------------------*
004500060809     D FIDPOSTS      E DS
004600060612      * ?   * Campi da tradurre con interi
004700060623     D atoll           PR            20I 0
004800060623     D                                     EXTPROC('atoll')
004900060612     D  Char                           *
005000060612     D                                     VALUE
005100060612     D                                     OPTIONS(*STRING)
005200060623      * ?   * Campi da tradurre con interi
005300060623     D atoi            PR            10I 0
005400060623     D                                     EXTPROC('atoi')
005500060623     D  Char                           *
005600060623     D                                     VALUE
005700060623     D                                     OPTIONS(*STRING)
005800060612      * ?   * Campi da tradurre con decimali
005900060612     D atof            PR             8F
006000060612     D                                     EXTPROC('atof')
006100060612     D  Char                           *
006200060612     D                                     VALUE
006300060612     D                                     OPTIONS(*STRING)
006400060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
006500060612      * ?   * Campi x decodifica * (INPUT  del Record)
006600060612     D  Dati           s           1500
006700060612     D Separa_campi    s              1
006800060612     D Decimal_Separ   s              1
006900060612      * ?   * Campi decodificati * (OUTPUT del Record)
007000060612      *       Schiere di Output / Campi di Output
007100060614     D  Alfa40         s             40
007200060612     d  Sk             s              3u 0
007300060612     D    NR_campi     s              3u 0
007400060612     D  Campi_Record   ds
007500060612     D    Dato_sk                    40    DIM(100)
007600060612     D  Campi_Lunghi   ds
007700060612     D    LunDato_sk                  3u 0 DIM(100)
007800060612     D Campi_Numerici  ds
007900060612     D    DatoNum_sk                  1    DIM(100)
008000060612     D Campi_Decimali  ds
008100060612     D    Decimal_sk                  3u 0 DIM(100)
008200060615      * ? ------------------------------------------------------------------
008300060612     D  position       s              3  0 INZ(0)
008400060612
008500060612     D  se_errore      s              1    inz(' ')
008600060612     D  Msg_Err        s            132    inz(' ')
008700060614     D  Msg_Vin_80     s             80    inz(' ')
008800060619     D Testa_Sk        s             10    DIM(50)
008900060809
009000060809     D Digits          C                   CONST('0123456789')
009100060615      * ?______________________________________________________________     */
009200060612      *   Ciclo principale
009300060615      * ?______________________________________________________________     */
009400060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
009500060612     C*
009600060612     C                   if        not %open(tivin00r)
009700060612     C                   open      tivin00r
009800060612     C                   endif
009900060612     C*? Controllo dati arrivati da DPD
010000060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
010100060612      * ?- controllando che la trasmissione sia completa.
010200060612
010300060612     c                   exsr      check_Trasm
010400060612
010500060613      **  Errore di trasmissione x tutti i records
010600060613      **   --> file in errore
010700060613     c                   if        se_errore = 'S'
010800060612
010900060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
011000060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
011100060614     C                               >> Manca Testata Messaggio !! +
011200060614     C                               Farsi rimandare i DATI !!'
011300060612
011400060612      * ?  Scrive su tutti i records il tipo di errore
011500060612     c     *start        setll     tivin00r
011600060612     c                   read      tivin00r
011700060612     c                   dow       not %eof(tivin00r)
011800060612     C                   eval      vinMSG = Msg_Err
011900060612     C                   eval      vinFLG = '2'
012000060613     c                   eval      esito  = '2'
012100060612     c                   update    tivin000
012200060612     c                   read      tivin00r
012300060612     c                   endDO
012400060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
012500060615     c                   else
012600060623
012700060612     c                   exsr      Importa_Msg
012800060612
012900060614     c                   end
013000060614
013100060612     C                   if        %open(tivin00r)
013200060612     C                   close     tivin00r
013300060612     C                   endif
013400060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
013500060614      *
013600060710     c                   if        almeno_uno ='S' and esito ='1'
013700060710     C                   eval      esito ='0'
013800060614     C                   endif
013900060614      *
014000060612     c                   SETON                                        LR
014100060615      * ?________________________________________________________________   */
014200060615      *? Controlla la trasmissione  primo record descrittivo tracciato
014300060615      * ?________________________________________________________________   */
014400060612     c     Check_Trasm   Begsr
014500060612
014600060612     C                   clear                   se_errore
014700060614      ** primo  record con intestazione dei campi
014800060612     c     *start        setll     tivin00r
014900060612     c                   read      tivin00r
015000060612     c                   if        not %eof(tivin00r)
015100060615      *
015200060612     c                   exsr      Decod_Record
015300060630      *
015400060630      **  Se presente un errore nel record emette una segnalazione msg
015500060630     c                   if        se_errore <> 'S'
015600060630     C                   eval      vinFLG = '1'
015700060710     c                   eval      esito  = '0'
015800060630     c                   update    tivin000
015900060630     c                   endif
016000060615      *
016100060612     c                   endif
016200060612
016300060612     c                   endSR
016400060615      * ?________________________________________________________________   */
016500060612      *? Importa i records della tramsissione
016600060615      * ?________________________________________________________________   */
016700060612     c     Importa_Msg   Begsr
016800060612
016900060614      * per controllare se almeno un record è stato importato sul VAB
017000060614     c                   clear                   Almeno_Uno        1
017100060614
017200060623      * Continua a leggere dal secondo record.
017300060623      *   Il primo è la testata già controllata dalla Check_trasm.
017400060612     c                   read      Tivin00r
017500060612
017600060612     c                   dow       not %eof(Tivin00r)
017700060614
017800060614      * solo i record sflaggati da rielaborare
017900060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
018000060612
018100060612      ** Controlli formali sui campi
018200060612     c                   clear                   se_errore
018300060620     c                   clear                   Msg_Vin_80
018400060612
018500060612      ** Decodifica record a campi variabili
018600060612     c                   exsr      Decod_Record
018700060615      *
018800060612      **  Se presente un errore nel record emette una segnalazione msg
018900060612     c                   if        se_errore ='S'
019000060614     C                   eval      vinMSG = Msg_Vin_80
019100060623     C                   eval      vinFLG = '2'
019200060623     c                   eval      esito  = '1'
019300060612     c                   else
019400060612
019500060612      *  con il record in canna scrive direttamente il VAB ed il VAT
019600060809     c                   exsr      WRT_FILE
019700060614
019800060614      *  Problemi nella decodifica dei campi VAB/VAT
019900060614     c                   if        se_errore ='S'
020000060614     C                   eval      vinMSG = Msg_Vin_80
020100060614     C                   eval      vinFLG = '2'
020200060623     c                   eval      esito  = '1'
020300060614     c                   else
020400060614      *  Record OK
020500060615     c                   move      'S'           Almeno_Uno
020600060612     C                   eval      vinFLG = '1'
020700060614     C                   eval      vinMSG = *Blank
020800060614     c                   end
020900060612
021000060612     c                   end
021100060612
021200060612     c                   update    Tivin000
021300060614     c                   endIF
021400060612
021500060612     c                   read      Tivin00r
021600060612     C                   ENDdo
021700060612      **
021800060612     c                   endSR
021900060615      * ?______________________________________________________________     */
022000060612      *?    Decodifica record a campi variabili
022100060615      * ?______________________________________________________________     */
022200060612     c     Decod_Record  Begsr
022300060612
022400060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
022500060612     c                   movel(p)  VINDTA        dati
022600060612      **
022700060612     c                   exsr      Split_Rec
022800060612      **
022900060612      **  Records di Intestazione e di fine Msg
023000060614     c                   if        %subst(dati:1:8) ='PARCELNO'
023100060614     c                   exsr      Rec_Testata
023200060615      *
023300060612     c                   Else
023400060612      ** conta le righe
023500060612     c                   exsr      Rec_Dettaglio
023600060615      * ?
023700060612     c                   End
023800060612      **
023900060612     c                   Endsr
024000060615      * ?________________________________________________________________   */
024100060614      *?      Record di Testata
024200060615      * ?________________________________________________________________   */
024300060614     c     Rec_Testata   Begsr
024400060612      * ?  Prima riga
024500060620     c     'PARCELNO'    SCAN      dati:1        position
024600060614     c                   if        %Found and position < 10
024700060614      ** esempio di record :
024800060614      * ?   PARCELNO;SCAN_CODE;DEPOT_CODE;DEPOTNAME;EVENT_DATE_TIME.......
024900060614      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
025000060614     C                   clear                   Testa_Sk
025100060614      * ? Dal 1° campo troviamo i nomi delle intestazioni dei campi C.Request
025200060614     C     1             do        50            sk
025300060614     c                   eval      Testa_Sk(sk) = %subst(Dato_sk(sk):1:+
025400060614     c                             LunDato_sk(sk))
025500060614     c                   enddo
025600060614      **
025700060612     c                   ElSe
025800060614      * ?    Se l'inizio del file non ha la testata errore di trasmissione
025900060614      * ?-----> Errore
026000060614     C                   eval      se_errore = 'S'
026100060614
026200060612     c                   End
026300060612      **
026400060612     c                   Endsr
026500060615      * ?_______________________________________________________________    */
026600060612      *?      Riga Bolla Import da controllare e tradurre
026700060615      * ?_______________________________________________________________    */
026800060612     c     Rec_Dettaglio Begsr
026900060809      **
027000060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
027100060612      **
027200060809     c                   clear                   FIDPOSTS
027300060614     c                   clear                   Msg_Err
027400060809      **
027500060614      * ? Nel 1° campo : Parcel Number               (Oblligatorio)  PARCELNO
027600060614      **                                                             Numero Parcel
027700060614     C                   z-add     1             sk
027800060614     c                   If        LunDato_sk(sk) =  0
027900060614      * ?-----> Errore
028000060614     c                   eval      tipo_error = 1
028100060614     c                   exsr      Field_Error
028200060614     c                   Else
028300060809     c                   eval      STPARCEL   = %subst(Dato_sk(sk):1:+
028400060614     c                                              LunDato_sk(sk))
028500060614     c                   End
028600060614      **
028700060614      * ? Nel 2° campo : Codice evento SCAN  DPD     (Oblligatorio)  SCAN_CODE
028800060614      **                                                             Codice SCAN
028900060614     C                   add       1             sk
029000060614     c                   If        DatoNum_sk(sk) = 'N' or
029100060614     c                             LunDato_sk(sk) =  0
029200060614      * ?-----> Errore
029300060614     c                   eval      tipo_error = 2
029400060614     c                   exsr      Field_Error
029500060614     c                   Else
029600060809     c                   eval      STSCANT  = atoi(%TRIM(Dato_sk(sk)))
029700060614     c                   End
029800060809      **
029900060614      * ? Nel 3° campo : DEPOT che ha fatto lo SCAN  (Oblligatorio)  DEPOT_CODE
030000060614      **                                                             Codice Depot
030100060614     C                   add       1             sk
030200060614     c                   If        LunDato_sk(sk) =  0
030300060614      * ?-----> Errore
030400060614     c                   eval      tipo_error = 1
030500060614     c                   exsr      Field_Error
030600060614     c                   Else
030700060809     c                   eval      STDEPCODE  = %subst(Dato_sk(sk):1:+
030800060614     c                                              LunDato_sk(sk))
030900060614     c                   End
031000060809      **
031100060630      * ? Nel 4° campo : NOME DEPOT                                  DEPOTNAME
031200060614      **                                                             Nome Depot
031300060614     C                   add       1             sk
031400060630     c                   If        LunDato_sk(sk) >  0
031500060809     c                   eval      STDEPNAME  = %subst(Dato_sk(sk):1:+
031600060614     c                                              LunDato_sk(sk))
031700060614     c                   End
031800060809      **
031900060630      * ? Nel 5° campo : Data Ora SCAN             Obbligatorio      DATE/TIME
032000060614      **                                                             DATA/ORA Evento
032100060614     C                   add       1             sk
032200060614     c                   If        DatoNum_sk(sk) = 'N' or
032300060614     c                             LunDato_sk(sk) =  0
032400060614      * ?-----> Errore
032500060614     c                   eval      tipo_error = 2
032600060614     c                   exsr      Field_Error
032700060614     c                   Else
032800060809     c                   eval      STDATTIM = atoll(%TRIM(Dato_sk(sk)))
032900060614     c                   End
033000060809      **
033100060614      * ? Nel 6° campo : ROUTE non sempre presente                   ROUTE
033200060614      **                                                             ROUTE
033300060614     C                   add       1             sk
033400060614     c                   If        LunDato_sk(sk) >  0
033500060809     c                   eval      STROUTE    = %subst(Dato_sk(sk):1:+
033600060614     c                                              LunDato_sk(sk))
033700060614     c                   End
033800060809      **
033900060614      * ? Nel 7° campo : TOUR  non sempre presente                   TOUR
034000060614      **                                                             TOUR
034100060614     C                   add       1             sk
034200060630     c                   If        DatoNum_sk(sk) = 'N' and
034300060614     c                             LunDato_sk(sk) >  0
034400060614      * ?-----> Errore
034500060614     c                   eval      tipo_error = 3
034600060614     c                   exsr      Field_Error
034700060614     c                   Else
034800060809     c                   eval      STTOUR     =  atoi(%TRIM(Dato_sk(sk)))
034900060614     c                   End
035000060809      **
035100060630      * ? Nel 8° campo : TIPO PARCEL 01/02                           PCODE
035200060630      **                                                             Parcel Code
035300060614     C                   add       1             sk
035400060630     c                   If        DatoNum_sk(sk) = 'N' and
035500060630     c                             LunDato_sk(sk) >  0
035600060614      * ?-----> Errore
035700060630     c                   eval      tipo_error = 3
035800060614     c                   exsr      Field_Error
035900060614     c                   Else
036000060809     c                   eval      STPCODE  = atoi(%TRIM(Dato_sk(sk)))
036100060614     c                   End
036200060809      **
036300060630      * ? Nel 9° campo : Codice di Servizio                          SERVICE
036400060614      **                                                             SERVICE CODE
036500060614     C                   add       1             sk
036600060630     c                   If        DatoNum_sk(sk) = 'N' and
036700060630     c                             LunDato_sk(sk) >  0
036800060614      * ?-----> Errore
036900060630     c                   eval      tipo_error = 3
037000060614     c                   exsr      Field_Error
037100060614     c                   Else
037200060809     c                   eval      STSERVICE  = atoi(%TRIM(Dato_sk(sk)))
037300060614     c                   End
037400060809      **
037500060630      * ? Nel 10° campo : Nazione Destinatario                       CONSEGNEE_COUNTRY_C
037600060614      **                                                             NAZIONE DESTINATARIO
037700060614     C                   add       1             sk
037800060630     c                   If        DatoNum_sk(sk) = 'N' and
037900060630     c                             LunDato_sk(sk) >  0
038000060614      * ?-----> Errore
038100060630     c                   eval      tipo_error = 3
038200060614     c                   exsr      Field_Error
038300060614     c                   Else
038400060809     c                   eval      STCCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
038500060614     c                   End
038600060809      **
038700060630      * ? Nel 11° campo : Cod.Postale Destinatario                   CONSEGNEE_ZIP
038800060614      **                                                             CAP DESTINATARIO
038900060614     C                   add       1             sk
039000060614     c                   If        LunDato_sk(sk) >  0
039100060809     c                   eval      STCONSZIP  = %subst(Dato_sk(sk):1:+
039200060614     c                                              LunDato_sk(sk))
039300060614     c                   End
039400060809      **
039500060614      * ? Nel 12° campo : CODICI AGGIUNTIVI                          ADD_SERVICE1
039600060614      **                                                             cod.AGG. 1
039700060614     C                   add       1             sk
039800060614     c                   If        DatoNum_sk(sk) = 'N' and
039900060614     c                             LunDato_sk(sk) >  0
040000060614      * ?-----> Errore
040100060614     c                   eval      tipo_error = 3
040200060614     c                   exsr      Field_Error
040300060614     c                   Else
040400060809     c                   eval      STADDSER1  =  atoi(%TRIM(Dato_sk(sk)))
040500060614     c                   End
040600060809      **
040700060614      * ? Nel 13° campo : CODICI AGGIUNTIVI                          ADD_SERVICE2
040800060614      **                                                             cod.AGG. 2
040900060614     C                   add       1             sk
041000060614     c                   If        DatoNum_sk(sk) = 'N' and
041100060614     c                             LunDato_sk(sk) >  0
041200060614      * ?-----> Errore
041300060614     c                   eval      tipo_error = 3
041400060614     c                   exsr      Field_Error
041500060614     c                   Else
041600060809     c                   eval      STADDSER2  =  atoi(%TRIM(Dato_sk(sk)))
041700060614     c                   End
041800060809      **
041900060614      * ? Nel 14° campo : CODICI AGGIUNTIVI                          ADD_SERVICE3
042000060614      **                                                             cod.AGG. 3
042100060614     C                   add       1             sk
042200060614     c                   If        DatoNum_sk(sk) = 'N' and
042300060614     c                             LunDato_sk(sk) >  0
042400060614      * ?-----> Errore
042500060614     c                   eval      tipo_error = 3
042600060614     c                   exsr      Field_Error
042700060614     c                   Else
042800060809     c                   eval      STADDSER3  =  atoi(%TRIM(Dato_sk(sk)))
042900060614     c                   End
043000060809      **
043100060614      * ? Nel 15° campo : PESO                                       WEIGHT
043200060614      **                                                             Peso in KG
043300060614     C                   add       1             sk
043400060614     c                   If        DatoNum_sk(sk) = 'N' and
043500060614     c                             LunDato_sk(sk) >  0
043600060614      * ?-----> Errore
043700060614     c                   eval      tipo_error = 3
043800060614     c                   exsr      Field_Error
043900060614     c                   Else
044000060809     c                   eval      STWEIGHT   =  atof(%TRIM(Dato_sk(sk)))
044100060614     c                   End
044200060809      **
044300060614      * ? Nel 16° campo : Riferimento Cliente                        CUSTOMER_REFERENCE
044400060614      **                                                             Riferimento Cliente
044500060614     C                   add       1             sk
044600060614     c                   If        LunDato_sk(sk) >  0
044700060809     c                   eval      STCUSTREF  = %subst(Dato_sk(sk):1:+
044800060614     c                                              LunDato_sk(sk))
044900060614     c                   End
045000060809      **
045100060614      * ? Nel 17° campo : POD IMAGE di riferimento                   POD_IMAGE_REF
045200060614      **                                                             Riferimento POD IMAGE
045300060614     C                   add       1             sk
045400060614     c                   If        DatoNum_sk(sk) = 'N' and
045500060614     c                             LunDato_sk(sk) >  0
045600060614      * ?-----> Errore
045700060614     c                   eval      tipo_error = 3
045800060614     c                   exsr      Field_Error
045900060614     c                   Else
046000060809     c                   eval      STPODIMAG  =  atoi(%TRIM(Dato_sk(sk)))
046100060614     c                   End
046200060809      **
046300060614      * ? Nel 18° campo : FIRMA di chi ha ricevuto la merce          RECEIVER_NAME
046400060614      **                                                             FIRMA
046500060614     C                   add       1             sk
046600060614     c                   If        LunDato_sk(sk) >  0
046700060809     c                   eval      STRECNAME  = %subst(Dato_sk(sk):1:+
046800060614     c                                              LunDato_sk(sk))
046900060614     c                   End
047000060809      **
047100060614      * ? Nel 18° campo : INFO TESTO                                 INFO_TEXT
047200060614      **                                                             TESTO AGGIUNTIVO
047300060614     C                   add       1             sk
047400060614     c                   If        LunDato_sk(sk) >  0
047500060809     c                   eval      STINFOTXT  = %subst(Dato_sk(sk):1:+
047600060614     c                                              LunDato_sk(sk))
047700060614     c                   End
047800060809      **
047900060809      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL FIDPOSTS
048000060612      **
048100060612     c                   Endsr
048200060615      * ?____________________________________________________________       */
048300060614      *?      Campi in errore
048400060615      * ?____________________________________________________________       */
048500060614     c     Field_Error   Begsr
048600060612      **
048700060614     C                   eval      se_errore   = 'S'
048800060614      * msg video o in posta
048900060614     c                   If        Msg_Err = *blank
049000060614     c
049100060614     c                   Select
049200060614     c
049300060614     c                   When      Tipo_Error = 1
049400060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
049500060614     C                             ' >> Obblig.- Manca Dato !!'
049600060614     c
049700060614     c                   When      Tipo_Error = 2
049800060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
049900060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
050000060614     c
050100060614     c                   When      Tipo_Error = 3
050200060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
050300060614     C                             ' >> NON Numerico  !!'
050400060614     c                   EndSL
050500060614     c
050600060614     c                   End
050700060614      * Errore su VINMSG
050800060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
050900060614     c                                          + %trim(Testa_sk(sk))
051000060614      **
051100060614     c                   Endsr
051200060615      * ?____________________________________________________________       */
051300060612      *?      Suddivide i campi della riga con carattere divisorio (;)
051400060615      * ?____________________________________________________________       */
051500060612     c     Split_Rec     Begsr
051600060612      **
051700060612     c                   clear                   Nr_campi
051800060612     c                   clear                   Campi_Record
051900060612     c                   clear                   Campi_Lunghi
052000060612     c                   clear                   Campi_Numerici
052100060612     c                   clear                   Campi_Decimali
052200060612     c                   movel     ';'           Separa_campi
052300060612     c                   movel     '.'           Decimal_Separ
052400060612      *
052500060612     c                   call      'FIEU00R'
052600060612      * ?     Input
052700060612     c                   parm                    Separa_campi
052800060612     c                   parm                    Decimal_Separ
052900060612     c                   parm                    dati
053000060612      * ?     Output  su schiere di 100 elementi
053100060612     c                   parm                    Nr_campi
053200060612     c                   parm                    Campi_Record                   * i dati
053300060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
053400060612     c                   parm                    Campi_Numerici                 * se numerici
053500060612     c                   parm                    Campi_Decimali                 * quanti decimali
053600060612      **
053700060612     c                   Endsr
053800060809      * ?_______________________________________________________________    */
053900060809     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
054000060809      * ?_______________________________________________________________    */
054100060809     C     WRT_FILE      BEGSR
054200060809      **
054300060809      **  Se è in questo punto significa che non sono stati trovati errori
054400060809      **    formali quindi si passa gli status per avanzamento ORM
054500080407      *
054600080527      *
054700080527      *   27/05/2008:
054800080527      ** ?  AL MOMENTO E' STATO DISATTIVATO
054900080527      ** ?  se per errore girasse l'UPLOAD :
055000080527      ** ?  NON DEVE FARE NULLA
055100080527      **?              /*********************/
055200080527     c                   goto      ENDSR_WRT
055300080527      **?              /*********************/
055400080527      *
055500080527      *
055600080407      *
055700080407      ** ?  Ma ignora tutti gli altri SCAN eccetto lo SCAN 18 con il motivo
055800080407      ** ?  di Mancato RITIRO.
055900080407     c                   if        STSCANT = 18
056000060809      **?              /*---------------------- */
056100060809     c                   ExSr      Avanza_ORM
056200060809      * ?              /*---------------------- */
056300080407     c                   End
056400080527      *
056500080527      *
056600080527      *
056700060809      *
056800080527     c     ENDSR_WRT     Endsr
056900060809      * ?------------------------------------------------------------------ */
057000060809      *?    Imposta i campi per il DPO e ORM/ORF
057100060809      * ?------------------------------------------------------------------ */
057200060809     c     Avanza_ORM    Begsr
057300060809      **
057400060809     c     STPARCEL      chain     fidpo06l
057500060809     c                   if        %Found(fidpo06l)
057600080516      **
057700080516      ** se non ha già ricevuto nel CRDATA02: (status 4) l'exception code
057800080528     c**************     If        dpoSTS <> 4  or
057900080528     c                   If        dpoSTS  = 4  and DPOCDMR = *blank
058000060809      **
058100060809      **  Cosa ha causato il Mancato Ritiro ????
058200060809     c                   if        STSCANT = 18
058300080516      **
058400060809     c                   clear                   cod_mancRit       2
058500060809     c                   z-add     1             cd
058600060809     c     STADDSER1     lookup    cDPD(cd)                               33
058700080403     c                   z-add     STADDSER1     STADDSERx
058800080403      *
058900080403     c                   if        *in33=*off and STADDSER2 > 0
059000080403     c                   z-add     1             cd
059100080403     c     STADDSER2     lookup    cDPD(cd)                               33
059200080403     c                   z-add     STADDSER2     STADDSERx
059300080403     c                   endIf
059400080403      *
059500080403     c                   if        *in33=*off and STADDSER3 > 0
059600080403     c                   z-add     1             cd
059700080403     c     STADDSER3     lookup    cDPD(cd)                               33
059800080403     c                   z-add     STADDSER3     STADDSERx
059900080403     c                   endIf
060000080403      *
060100080528      *  Se non è codificato nella tabella MRD non deve fare NULLA
060200080528      *   rilascia solo il record aggiornandolo.
060300080528     c  n33              goto      END_AVANZA_ORM
060400080528      *
060500080528      *
060600061109     c   33              move      cBAR(cd)      cod_mancRit
060700061109      *   ANNULLAMENTO PRIMA DELL'AFFIDAMENTO --> non sapendo quale mettere .......
060800061109     c  n33              move      '80'          cod_mancRit
060900060809      **
061000060809      ** Controlla sull'ORM la Fase di avanzamento
061100060809     c     KOrM1         chain     fnOrm01l
061200060809
061300060809      **  Controllo Fase ORM se tutto ok dovrebbe essere ancora non chiusa
061400060809      **   se invece è oltre --> errore
061500060921     c                   if        not %Found(fnorm01l)
061600060921      ** se addirittura non esiste il file  (Veramente Strano)
061700060921      **   imposta il codice di errore --> '10' x ricordare di non aver trovato il
061800060921      **    nostro ORM
061900060921     c                   movel     '10'          DPOCODER
062000060921     c                   else
062100060809      **
062200060809     c                   if        ORmFAO < 900
062300060809      ** aggiorna
062400060809      ** avanzamento fase su testata ORM (senza distinta/padr.)
062500060809     c                   z-add     999           ormFAO
062600080516     c                   z-add     cFAS(cd)      ormFAO
062700170228      ****
062800170228      * imposto il campo ORMETI
062900170228     c                   eval      tbeCOD = 'FAR'
063000170228     c                   movel(p)  ormfao        tbeke1
063100170228     c     ktbe1         chain     tntbe01l
063200170228     c                   if        %Found(tntbe01l)
063300170228     c                   eval      dfar = tbeuni
063400170228     c                   eval      ormeti = d§farass
063500170228     c                   end
063600080516      ****
063700060809     c                   z-add     data_oggi     ormDFO
063800060809     c                   z-add     ore           ormOFO
063900060809     c                   update    fnORM000
064000060809
064100060809      **  Allinea le Fasi in dettaglio
064200060809     c     KOrM1         setgt     fnOrf01l
064300060809     c     KOrM1         readpe    fnOrf01l
064400060809     c                   if        not %Eof(fnorf01l)
064500060809      **  aggiunge la nuova fase --> 999
064600060921     c                   eval      ORFCAR = cod_mancRit
064700060921     c                   z-add     ormFAO        ORFFAR
064800060809     c                   z-add     data_oggi     ORFDAE
064900060809     c                   z-add     data_oggi     ORFDFC
065000060809     c                   z-add     ore           ORFORE
065100060809     c                   eval      ORFPUE = 'ORMDPD_AUT'
065200060809     c                   write     fnorf000
065300060809     c                   end
065400060809      **
065500060809     c                   else
065600060809      **  Se non era nella fase che si aspettava
065700060921      **   imposta il codice di errore --> '01' ORM per fase non congrua.
065800061002     c                   if        ORmFAO <> 999
065900060921     c                   movel     '01'          DPOCODER
066000060809     c                   end
066100061002     c                   end
066200060809      **
066300060809     c                   end
066400060809      **
066500060809     c                   else
066600060809      ** Se non riceve uno scan 18 segnala il problema sul FIDPO00F
066700080407      **
066800080407      ** ?  Questo non è + vero. DPD invia altri SCAN oltre il 18.
066900080407     c*******            movel     '09'          DPOCODER
067000060809     c                   end
067100060809      **
067200060809      **  Quindi aggiorna FIDPO con o senza l'errore con il Nr.Parcel
067300060809     c                   z-add     8             DPOSTS
067400080403     c******             move      STADDSER1     DPOCDMR
067500080403     c                   move      STADDSERx     DPOCDMR
067600060809     c                   z-add     data_oggi     DPODTRIC
067700080516     c*
067800080516     c                   end
067900080516      **
068000080528     c     END_AVANZA_ORMtag
068100060809     c                   update    fidpo000
068200060809     c                   end
068300060809      **
068400080528     c                   Endsr
068500060615      * ?_______________________________________________________________    */
068600060615     C     *INZSR        BEGSR
068700060615      * ?_______________________________________________________________    */
068800991129     c     *ENTRY        PLIST
068900060613     C                   parm                    esito             1
069000060809      **
069100060809     c     Korm1         klist
069200060809     c                   kfld                    DPOPOE
069300060809     c                   kfld                    DPONRS
069400060809     c                   kfld                    DPONOR
069500060809     c                   kfld                    DPONRV
069600170228      **
069700170228     c     Ktbe1         klist
069800170228     c                   kfld                    tbeCOD
069900170228     c                   kfld                    tbeKE1
070000060809      *
070100060809      *  imposta UDATE di elaborazione
070200060915     C                   move      *date         data_oggi
070300060915     C                   move      *date         G02INV
070400060915     C                   MOVE      *ZEROS        G02DAT
070500060809     C                   MOVE      *BLANKS       G02ERR
070600060809     C                   CALL      'XSRDA8'
070700060809     C                   PARM                    WLBDAT
070800060809     C                   move      data_oggi     day
070900060809      * Recupero data e ora
071000060809     C                   TIME                    W0140
071100060809     C                   MOVEl     W0140         ore
071200060809      **
071300060809     c                   move      'MRD'         tbeCOD
071400060809     c                   clear                   cBAR
071500060809     c                   clear                   cDPD
071600060809     c                   z-add     0             cd                3 0
071700060809      **
071800060809     c     tbeCOD        setll     tnTBE01l
071900060809     c     tbeCOD        reade     tnTBE01l
072000060809      **
072100060809     c                   dow       not %Eof(tnTBE01l)
072200060809     c                   add       1             cd
072300060809     c                   movel     tbeke1        cBAR(cd)
072400060809     c                   movel     tbeuni        dMRD
072500060809     c                   movel     §MRDCAR       cDPD(cd)
072600060809     c                   movel     §MRDFAR       cFAS(cd)
072700060809     c     tbeCOD        reade     tnTBE01l
072800060809     c                   Enddo
072900050414      *
073000991124     C                   ENDSR
073100060615      * ?_______________________________________________________________    */
073200060615     C     *pssr         BEGSR
073300060615      * ?_______________________________________________________________    */
073400060614     C                   eval      esito = '2'
073500060614     C*
073600060614     C                   ENDSR     '*CANCL'
073700060614      *-------------------------------------------------------------------------
