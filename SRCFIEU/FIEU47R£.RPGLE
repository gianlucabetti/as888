000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300060925     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600060809      *  Importa da FTP server la tabella dei Depots DPD                        *
000700060809      *  TRASCODIFICA : DEPOT.TAB                                               *
000800060612      **?************************************************************************
000900060612     Ftivin00r  uF   E             DISK    usropn
001000060925     fdppdc01l  uf   e           k disk
001100060808      * ?================================================================== */
001200060809     D Tipo_error      S              1  0
001300080513     D Msg_inviato     S              1    inz(' ')
001400060808      * ?================================================================== */
001500061003     D  data_Vers      s              8  0 INZ(0)
001600061003     D  data8alfa      s              8    INZ('00000000')
001700060925
001800060808     D  data_Oggi      s              8  0 INZ(0)
001900060808     D  day            s              8  0 INZ(0)
002000060808     D  ore            s              6  0 INZ(0)
002100060808     D  w0140          s             14  0 INZ(0)
002200060809      *
002300060808     D WLBDAT          DS
002400060808     D  G02DAT                 1      8  0
002500060808     D  G02INV                 9     16  0
002600060808     D  G02ERR                17     17
002700060808     D  G02TGI                18     22  0
002800060809      * ?   *--------------------------------------------------------------*
002900060809      * ?   ( Descrizione Campi x scomposizione FLAT record )
003000060809      * ?   *--------------------------------------------------------------*
003100060612      * ?================================================================== */
003200060809      * ?   * Campi da tradurre con interi
003300060809     D atoll           PR            20I 0
003400060809     D                                     EXTPROC('atoll')
003500060809     D  Char                           *
003600060809     D                                     VALUE
003700060809     D                                     OPTIONS(*STRING)
003800060612      * ?   * Campi da tradurre con interi
003900060612     D atoi            PR            10I 0
004000060612     D                                     EXTPROC('atoi')
004100060612     D  Char                           *
004200060612     D                                     VALUE
004300060612     D                                     OPTIONS(*STRING)
004400060612      * ?   * Campi da tradurre con decimali
004500060612     D atof            PR             8F
004600060612     D                                     EXTPROC('atof')
004700060612     D  Char                           *
004800060612     D                                     VALUE
004900060612     D                                     OPTIONS(*STRING)
005000060612      * ?   *--------------------------------------------------------------*
005100060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
005200060612      * ?   *--------------------------------------------------------------*
005300060612      *
005400060612      * ?   * Campi x decodifica * (INPUT  del Record)
005500060612     D  Dati           s           1500
005600060612     D Separa_campi    s              1
005700060612     D Decimal_Separ   s              1
005800060809      *
005900060612      * ?   * Campi decodificati * (OUTPUT del Record)
006000060612      *       Schiere di Output / Campi di Output
006100060614     D  Alfa40         s             40
006200060612     d  Sk             s              3u 0
006300060612     D    NR_campi     s              3u 0
006400060612     D  Campi_Record   ds
006500060612     D    Dato_sk                    40    DIM(100)
006600060612     D  Campi_Lunghi   ds
006700060612     D    LunDato_sk                  3u 0 DIM(100)
006800060612     D Campi_Numerici  ds
006900060612     D    DatoNum_sk                  1    DIM(100)
007000060612     D Campi_Decimali  ds
007100060612     D    Decimal_sk                  3u 0 DIM(100)
007200060612      * ?   *--------------------------------------------------------------*
007300060612     D  position       s              3  0 INZ(0)
007400060612
007500060612     D  se_errore      s              1    inz(' ')
007600060612     D  Msg_Err        s            132    inz(' ')
007700060614     D  Msg_Vin_80     s             80    inz(' ')
007800060710      **
007900060710     D Digits          C                   '0123456789'
008000061103     D MancaDEP        C                   'Manca Depot su tabella DPPDC00F'
008100060612      * ?================================================================== */
008200060612      *   Ciclo principale
008300060413      * ?********************************************************************/
008400060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
008500060612     C*
008600060612     C                   if        not %open(tivin00r)
008700060612     C                   open      tivin00r
008800060612     C                   endif
008900061003      * ?------------------------------------------------------------------ */
009000061003     C*? Controllo dati arrivati da DPD
009100061003      * ?------------------------------------------------------------------ */
009200061003      * ?- Occorre fare un primo test sull'integrità della trasmissione
009300061003      * ?- controllando che la trasmissione sia completa.
009400061003
009500061003      * ?              /*---------------------- */
009600061003     c                   exsr      check_Trasm
009700061003      * ?              /*---------------------- */
009800061003      **
009900061003      **  Errore di trasmissione x tutti i records
010000061003      **   --> file in errore
010100061003     c                   if        se_errore = 'S'
010200061003
010300061003      ** Messaggio da riportare su ogni record x tutta la trasmissione
010400061003     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
010500061003     C                               >> Farsi rimandare i DATI !!'
010600061003
010700061003      * ?  Scrive su tutti i records il tipo di errore
010800061003     c     *start        setll     tivin00r
010900061003     c                   read      tivin00r
011000061003     c                   dow       not %eof(tivin00r)
011100061003     C                   eval      vinMSG = Msg_Err
011200061003     C                   eval      vinFLG = '2'
011300061003     c                   eval      esito  = '2'
011400061003     c                   update    tivin000
011500061003     c                   read      tivin00r
011600061003     c                   endDO
011700061003      **
011800061003     c                   else
011900061003      **
012000060612      **
012100060612      * ?              /*---------------------- */
012200060612     c                   exsr      Importa_Msg
012300060612      * ?              /*---------------------- */
012400060614
012500061003     c                   end
012600061003
012700060612     C                   if        %open(tivin00r)
012800060612     C                   close     tivin00r
012900060612     C                   endif
013000060612      *
013100060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
013200060614      *
013300060710     c                   if        almeno_uno ='S' and esito ='1'
013400060710     C                   eval      esito ='0'
013500060614     C                   endif
013600060614      *
013700060612     c                   SETON                                        LR
013800061003      * ?================================================================== */
013900061003      *? Controlla la trasmissione   se completa
014000061003      * ?------------------------------------------------------------------ */
014100061003     c     Check_Trasm   Begsr
014200061003
014300061003     C                   clear                   se_errore
014400061003      ** primo  record
014500061003     c     *start        setll     tivin00r
014600061003     c                   read      tivin00r
014700061003     c                   if        not %eof(tivin00r)
014800061003      * ?              /*---------------------- */
014900061003     c                   movel(p)  VINDTA        inizio_file       5
015000061003      * ?              /*---------------------- */
015100061003     c                   endif
015200061003      **
015300061003      ** ultimo record
015400061003     c     *hival        setll     tivin00r
015500061003     c                   readp     tivin00r
015600061003     c                   if        not %eof(tivin00r)
015700061003      * ?              /*---------------------- */
015800061003     c                   movel(p)  VINDTA        fine_file         4
015900061003      * ?              /*---------------------- */
016000061003     c                   endif
016100061003
016200061003      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
016300061003      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
016400061003      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
016500061003     C                   if        inizio_file <> '*HEAD' or
016600061003     C                             fine_file   <> '*END'
016700061003      * ?-----> Errore
016800061003     C                   eval      se_errore = 'S'
016900061003
017000061003     c                   end
017100061003
017200061003     c                   endSR
017300060612      * ?------------------------------------------------------------------ */
017400060612      *? Importa i records della tramsissione
017500060612      * ?------------------------------------------------------------------ */
017600060612     c     Importa_Msg   Begsr
017700060612
017800060614      * per controllare se almeno un record è stato importato sul VAB
017900060614     c                   clear                   Almeno_Uno        1
018000060614
018100060612     c     *start        setll     Tivin00r
018200060612     c                   read      Tivin00r
018300060612
018400060612     c                   dow       not %eof(Tivin00r)
018500060614
018600060614      * solo i record sflaggati da rielaborare
018700060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
018800060612
018900060612      ** Controlli formali sui campi
019000060612     c                   clear                   se_errore
019100060620     C                   clear                   Msg_Vin_80
019200060612
019300060612      ** Decodifica record a campi variabili
019400060612      * ?              /*---------------------- */
019500060612     c                   exsr      Decod_Record
019600060612      * ?              /*---------------------- */
019700060612
019800060612      **  Se presente un errore nel record emette una segnalazione msg
019900060612     c                   if        se_errore ='S'
020000060614     C                   eval      vinMSG = Msg_Vin_80
020100060621     c                   end
020200060612
020300061003      *  con il record in canna aggiorna la tabella dei DEPOT
020400061003     c                   if        %subst(dati:5:1) = ';'  and
020500061003     c                             %subst(dati:1:1) <>'*'
020600060612      * ?              /*---------------------- */
020700060809     c                   exsr      WRT_FILE
020800060612      * ?              /*---------------------- */
020900060620     c                   end
021000060614
021100060809      *  Problemi nella decodifica dei campi VAO
021200060614     c                   if        se_errore ='S'
021300060614     C                   eval      vinMSG = Msg_Vin_80
021400060614     c                   end
021500060612
021600060621      *  Sempre Record OK
021700060621     C                   eval      vinFLG = '1'
021800060710     c                   eval      esito  = '0'
021900060621
022000061103      *  x errore bloccante nell'aggiornamento del file
022100060621     c                   if        err_bloccante ='S'
022200060621     C                   eval      vinFLG = '2'
022300060710     c                   eval      esito  = '1'
022400060621     c                   end
022500060612
022600060612     c                   update    Tivin000
022700060614     c                   endIF
022800060612
022900060612     c                   read      Tivin00r
023000060612     C                   ENDdo
023100060612      **
023200060612     c                   endSR
023300060612      * ?------------------------------------------------------------------ */
023400060612      *?    Decodifica record a campi variabili
023500060612      * ?------------------------------------------------------------------ */
023600060612     c     Decod_Record  Begsr
023700060612
023800060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
023900060612     c                   movel(p)  VINDTA        dati
024000061003
024100061003      ** non sono righe dati
024200061003     c                   if        %subst(dati:1:1) ='*'
024300061003      **  Data Versione:
024400061003     c                   if        %subst(dati:1:5) ='*DATE'
024500061003      **
024600061003      * ?              /*---------------------- */
024700061003     c                   exsr      Check_Vers
024800061003      * ?              /*---------------------- */
024900061003     c                   End
025000061003      **
025100061003     c                   else
025200060612      **
025300060612      * ?              /*---------------------- */
025400060612     c                   exsr      Split_Rec
025500060612      * ?              /*---------------------- */
025600060612      **
025700060809      **  Record di dettaglio
025800060809     c                   if        %subst(dati:5:1) =';'
025900060612      * ?              /*---------------------- */
026000060612     c                   exsr      Rec_Dettaglio
026100060612      * ?              /*---------------------- */
026200060612     c                   End
026300061003     c                   End
026400060612      **
026500061003     c                   Endsr
026600061003      * ?------------------------------------------------------------------ */
026700061003      *?      Controlla la data Versione Ricevuta
026800061003      * ?------------------------------------------------------------------ */
026900061003     c     Check_Vers    Begsr
027000061003      *
027100061003     c                   eval      data8alfa= %subst(dati:7:8)
027200061003     c     digits        check     data8alfa     position
027300061003      **
027400061003      **  presenti campi non numerici: (Ossia trovati)
027500061003     c                   iF        %Found
027600061003     c                   eval      Data_Vers = *date
027700061003     c                   else
027800061003      *  se la data non è corretta comunque imposta UDATE
027900061003     C                   move      data8alfa     G02INV
028000061003     C                   MOVE      *ZEROS        G02dat
028100061003     C                   MOVE      '3'           G02ERR
028200061003     C                   CALL      'XSRDA8'
028300061003     C                   PARM                    WLBDAT
028400061003     c                   if        g02err <> '0'
028500061003     c                   eval      Data_Vers = *date
028600061003     c                   Else
028700061003     c                   move      data8alfa     Data_Vers
028800061003     c                   End
028900061003     c                   End
029000061003      *
029100060612     c                   Endsr
029200060612      * ?------------------------------------------------------------------ */
029300060612      *?      Riga Bolla Import da controllare e tradurre
029400060612      * ?------------------------------------------------------------------ */
029500060612     c     Rec_Dettaglio Begsr
029600060612      **
029700060809      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
029800060809     c                   clear                   Testa_Field      10
029900060809     c                   clear                   COD_DEPOT         4
030000060809     c                   clear                   DATA_ATT          8 0
030100060809     c                   clear                   CR_AUTOM          1
030200060809     c                   clear                   COUNTRY           3
030300060809     c                   clear                   DESCRIZ          30
030400060614     c                   clear                   Msg_Err
030500060808      **
030600060809      * ? Nel 1° campo : Numero Depot Ordinante DPD  (Oblligatorio)  DEPOT4
030700060809      **                                                             DEPOT4
030800060808     C                   z-add     1             sk
030900060809     c                   If        LunDato_sk(sk) =  0
031000060808      * ?-----> Errore
031100060809     C                   eval      tipo_error = 1
031200060809     c                   eval      Testa_Field = 'DEPOT'
031300060809     C                   exsr      Field_Error
031400060808     c                   Else
031500060809     c                   eval      COD_DEPOT = %subst(Dato_sk(sk):1:+
031600060809     c                                              LunDato_sk(sk))
031700060808     c                   End
031800060809      **
031900060809      * ? Nel 2° campo : Data da quando è attivo il DEPOT            DATADEP
032000060809      **
032100060809     C                   z-add     2             sk
032200060809     c                   If        DatoNum_sk(sk) = 'N' and
032300060809     c                             LunDato_sk(sk) >  0
032400060809      * ?-----> Errore
032500060809     C                   eval      tipo_error = 3
032600060809     c                   eval      Testa_Field = 'DATA_ATT'
032700060809     C                   exsr      Field_Error
032800060809     c                   Else
032900060809     c                   eval      DATA_ATT = atoi(%TRIM(Dato_sk(sk)))
033000060809     c                   End
033100060809      **
033200060809      * ? Nel  4° campo : CAMPO NAZIONE                              COUNTRY
033300060809      **                                                             COUNTRY
033400060809     C                   z-add     4             sk
033500060809     c                   If        LunDato_sk(sk) >  0
033600060809     c                   eval      COUNTRY  = %subst(Dato_sk(sk):1:+
033700060809     c                                              LunDato_sk(sk))
033800060809     c                   End
033900060809      **
034000060809      **
034100060809      * ? Nel 31° campo : Esegue i Collection Request                CRAUTOM
034200060809      **                                                             CRAUTOM
034300060809     C                   z-add     31            sk
034400061003     c                   If        LunDato_sk(sk) =  0
034500060809      * ?-----> Errore
034600060809     C                   eval      tipo_error = 1
034700060809     c                   eval      Testa_Field = 'CR_AUTOM'
034800060809     C                   exsr      Field_Error
034900060809     c                   Else
035000060809     c                   eval      CR_AUTOM = %subst(Dato_sk(sk):1:+
035100060809     c                                              LunDato_sk(sk))
035200060925      * ?-
035300060925     c                   if        CR_AUTOM = 'Y'
035400060925     c                   eval      CR_AUTOM = 'S'
035500060925     c                   End
035600060925     c                   if        CR_AUTOM = 'N'
035700060925     c                   eval      CR_AUTOM = *blank
035800060925     c                   End
035900060809     c                   End
036000060809      **
036100060809      * ? Nel 57° campo : Descrizione del DEPOT                      DESCRIZ
036200060809      **                                                             DESCRIZ
036300060809     C                   z-add     57            sk
036400060809     c                   If        LunDato_sk(sk) >  0
036500060809     c                   eval      DESCRIZ  = %subst(Dato_sk(sk):1:+
036600060809     c                                              LunDato_sk(sk))
036700060809     c                   end
036800060808      **
036900060809      * ?----->  HA I CAMPI IMPOSTATI
037000060612      **
037100060612     c                   Endsr
037200060614      * ?------------------------------------------------------------------ */
037300060614      *?      Campi in errore
037400060614      * ?------------------------------------------------------------------ */
037500060614     c     Field_Error   Begsr
037600060614      **
037700060614     C                   eval      se_errore   = 'S'
037800060614      * msg video o in posta
037900060614     c                   If        Msg_Err = *blank
038000060614     c
038100060614     c                   Select
038200060614     c
038300060614     c                   When      Tipo_Error = 1
038400060809     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_field) +
038500060614     C                             ' >> Obblig.- Manca Dato !!'
038600060614     c
038700060614     c                   When      Tipo_Error = 2
038800060809     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_field) +
038900060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
039000060614     c
039100060614     c                   When      Tipo_Error = 3
039200060809     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_field) +
039300060614     C                             ' >> NON Numerico  !!'
039400060614     c
039500060614     c                   When      Tipo_Error = 4
039600060809     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_field) +
039700060614     C                             ' >> Conversione non riuscita !!'
039800060614     c                   EndSL
039900060614     c
040000060614     c                   End
040100060614      * Errore su VINMSG
040200060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
040300060809     c                                          + %trim(Testa_field)
040400060614      **
040500060614     c                   Endsr
040600060808      * ?------------------------------------------------------------------ */
040700060808      *?    Scrive i Files degli ORM
040800060808      * ?------------------------------------------------------------------ */
040900060809     c     WRT_FILE      Begsr
041000060808      **
041100060809      **  Tabella da AGGIORNARE
041200060808      **?              /*---------------------- */
041300060925     c                   ExSr      Aggiorna_PDC
041400060808      * ?              /*---------------------- */
041500060809
041600060809     c                   endSR
041700060808      * ?------------------------------------------------------------------ */
041800060809      *?    Imposta i dati
041900060808      * ?------------------------------------------------------------------ */
042000060925     c     Aggiorna_PDC  Begsr
042100060808      **
042200061103     c                   move      'S'           err_bloccante     1
042300061103     c                   movel     MancaDEP      Msg_Vin_80
042400060925
042500061003     c                   eval      wdata = data_vers
042600060925     c                   eval      depot = Cod_Depot
042700060925     c     kpdc          setll     dppdc01l
042800060925     c                   do        *hival
042900060925     c     depot         readpe    dppdc01l
043000060925     c                   if        %eof(dppdc01l)
043100060925     c                   leave
043200060925     c                   endif
043300060925     c                   if        wdata >= pdcdde and wdata <= pdcdsc
043400060925     c                   movel     CR_AUTOM      pdcorma
043500061103     c                   clear                   err_bloccante
043600061103     C                   clear                   Msg_Vin_80
043700060925     c                   update    dppdc000
043800060925     c                   leave
043900060925     c                   endif
044000060925     c                   enddo
044100061106      **
044200080707      * ?-----> Errore
044300080707     c                   if        err_bloccante = 'S'
044400080707     C                   eval      se_errore = 'S'
044500080707     c                   End
044600061106      ** invio messaggio per avvisare disallinea
044700080513     c                   if        err_bloccante = 'S' and Msg_inviato = ' '
044800080513     c                   eval      Msg_inviato ='S'
044900080818      *
045000080818      * L'informazione di "non tutti i Depots sono stati ...." non è al momento
045100080818      *  importante e necessaria, si salta l'invio del messaggio
045200080818     c                   goto      no_mail
045300080818      *
045400061106     C                   exsr      Alert_email
045500080818      *
045600080818     c     no_mail       tag
045700080818      *
045800061106     c                   End
045900060808      **
046000060808     c                   Endsr
046100060612      * ?------------------------------------------------------------------ */
046200060612      *?      Suddivide i campi della riga con carattere divisorio (;)
046300060612      * ?------------------------------------------------------------------ */
046400061106     c     ALERT_EMAIL   Begsr
046500060612      **
046600061106     C* Inizializzo variabili
046700061106     C                   movel     *blanks       wrkEml          253
046800061106     C                   movel     *blanks       wrkMsg         5000
046900061106     C                   movel     *blanks       wrkOgg           44
047000061106      *
047100061106     C* Valorizzo i campi della e-m@ail - indirizzo
047200061106     C                   eval      wrkEml= 'CED@bartolini.it'
047300061106
047400080513      * Oggetto della Mail da inviare
047500080513     C                   eval      wrkOgg='Ricez.DEPOTS.TAB x gest. ORM autom.D+
047600080513     C                             epot DPD'
047700061106
047800080513     C                   eval      wrkMsg = 'Avviso: Nel file DPPDC00F,'       +
047900061106     C                             ' (File dei Depot DPD da cappario DPD),   ' +
048000080513     C                             ' NON tutti i Depots DPD sono stati '       +
048100080513     C                             ' codificati. Questo non è un messaggio '   +
048200080513     C                             ' Bloccante ma solo di avviso. '
048300061106     C*
048400061106     C                   call(e)   'TIS701C'
048500061106     C                   parm                    wrkEml
048600061106     C                   parm                    wrkOgg
048700061106     C                   parm                    wrkMsg
048800061106      **
048900061106     c                   Endsr
049000061106      * ?------------------------------------------------------------------ */
049100061106      *?      Suddivide i campi della riga con carattere divisorio (;)
049200061106      * ?------------------------------------------------------------------ */
049300061106     c     Split_Rec     Begsr
049400061106      **
049500060612     c                   clear                   Nr_campi
049600060612     c                   clear                   Campi_Record
049700060612     c                   clear                   Campi_Lunghi
049800060612     c                   clear                   Campi_Numerici
049900060612     c                   clear                   Campi_Decimali
050000060612     c                   movel     ';'           Separa_campi
050100060612     c                   movel     '.'           Decimal_Separ
050200060612      *
050300060612     c                   call      'FIEU00R'
050400060612      * ?     Input
050500060612     c                   parm                    Separa_campi
050600060612     c                   parm                    Decimal_Separ
050700060612     c                   parm                    dati
050800060612      * ?     Output  su schiere di 100 elementi
050900060612     c                   parm                    Nr_campi
051000060612     c                   parm                    Campi_Record                   * i dati
051100060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
051200060612     c                   parm                    Campi_Numerici                 * se numerici
051300060612     c                   parm                    Campi_Decimali                 * quanti decimali
051400060612      **
051500060612     c                   Endsr
051600971215      *----------------------------------------------------------------
051700051205      *   DEFINIZIONE CHIAVI                               *
051800060808      *----------------------------------------------------------------
051900051205     C     *INZSR        BEGSR
052000051205      *
052100991129     c     *ENTRY        PLIST
052200060613     C                   parm                    esito             1
052300060925     c*
052400060925     c     kpdc          klist
052500060925     c                   kfld                    depot             4
052600060925     c                   kfld                    wdata             8 0
052700060809      *
052800060809      *  imposta UDATE di elaborazione
052900060925     C                   move      *date         data_oggi
053000060925     C                   move      *date         G02INV
053100060925     C                   MOVE      *ZEROS        G02dat
053200060809     C                   MOVE      *BLANKS       G02ERR
053300060809     C                   CALL      'XSRDA8'
053400060809     C                   PARM                    WLBDAT
053500060809     C                   move      data_oggi     day
053600060809     C                   TIME                    W0140
053700060809     C                   MOVEl     W0140         ore
053800050414      *
053900991124     C                   ENDSR
054000060621      * ?------------------------------------------------------------------ */
054100060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
054200060621      * ?------------------------------------------------------------------ */
054300060621     C     *pssr         BEGSR
054400060621     C
054500060710     C                   eval      esito = '2'
054600060621     C
054700060621     C                   ENDSR     '*CANCL'
054800060614      *-------------------------------------------------------------------------
