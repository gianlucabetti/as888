000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300060925     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600060809      *  Importa da FTP server la tabella dei Depots DPD                        *
000700060809      *  TRASCODIFICA : DEPOT.TAB                                               *
000800060612      **?************************************************************************
000900060612     Ftivin00r  uF   E             DISK    usropn
001000171011     fdpLEG14l  uf   e           k disk
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
008000171011     D MancaDEP        C                   'Manca Depot su tabella DPLEG00F'
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
024500061003      * ?              /*---------------------- */
024600061003     c                   exsr      Check_Vers
024700061003      * ?              /*---------------------- */
024800061003     c                   End
024900061003      **
025000061003     c                   else
025100060612      **
025200060612      * ?              /*---------------------- */
025300060612     c                   exsr      Split_Rec
025400060612      * ?              /*---------------------- */
025500060612      **
025600060809      **  Record di dettaglio
025700060809     c                   if        %subst(dati:5:1) =';'
025800060612      * ?              /*---------------------- */
025900060612     c                   exsr      Rec_Dettaglio
026000060612      * ?              /*---------------------- */
026100060612     c                   End
026200061003     c                   End
026300060612      **
026400061003     c                   Endsr
026500061003      * ?------------------------------------------------------------------ */
026600061003      *?      Controlla la data Versione Ricevuta
026700061003      * ?------------------------------------------------------------------ */
026800061003     c     Check_Vers    Begsr
026900061003      *
027000061003     c                   eval      data8alfa= %subst(dati:7:8)
027100061003     c     digits        check     data8alfa     position
027200061003      **
027300061003      **  presenti campi non numerici: (Ossia trovati)
027400061003     c                   iF        %Found
027500061003     c                   eval      Data_Vers = *date
027600061003     c                   else
027700061003      *  se la data non è corretta comunque imposta UDATE
027800061003     C                   move      data8alfa     G02INV
027900061003     C                   MOVE      *ZEROS        G02dat
028000061003     C                   MOVE      '3'           G02ERR
028100061003     C                   CALL      'XSRDA8'
028200061003     C                   PARM                    WLBDAT
028300061003     c                   if        g02err <> '0'
028400061003     c                   eval      Data_Vers = *date
028500061003     c                   Else
028600061003     c                   move      data8alfa     Data_Vers
028700061003     c                   End
028800061003     c                   End
028900061003      *
029000060612     c                   Endsr
029100060612      * ?------------------------------------------------------------------ */
029200060612      *?      Riga Bolla Import da controllare e tradurre
029300060612      * ?------------------------------------------------------------------ */
029400060612     c     Rec_Dettaglio Begsr
029500060612      **
029600060809      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
029700060809     c                   clear                   Testa_Field      10
029800060809     c                   clear                   COD_DEPOT         4
029900060809     c                   clear                   DATA_ATT          8 0
030000060809     c                   clear                   CR_AUTOM          1
030100060809     c                   clear                   COUNTRY           3
030200060809     c                   clear                   DESCRIZ          30
030300060614     c                   clear                   Msg_Err
030400060808      **
030500060809      * ? Nel 1° campo : Numero Depot Ordinante DPD  (Oblligatorio)  DEPOT4
030600060809      **                                                             DEPOT4
030700060808     C                   z-add     1             sk
030800060809     c                   If        LunDato_sk(sk) =  0
030900060808      * ?-----> Errore
031000060809     C                   eval      tipo_error = 1
031100060809     c                   eval      Testa_Field = 'DEPOT'
031200060809     C                   exsr      Field_Error
031300060808     c                   Else
031400060809     c                   eval      COD_DEPOT = %subst(Dato_sk(sk):1:+
031500060809     c                                              LunDato_sk(sk))
031600060808     c                   End
031700060809      **
031800060809      * ? Nel 2° campo : Data da quando è attivo il DEPOT            DATADEP
031900060809      **
032000060809     C                   z-add     2             sk
032100060809     c                   If        DatoNum_sk(sk) = 'N' and
032200060809     c                             LunDato_sk(sk) >  0
032300060809      * ?-----> Errore
032400060809     C                   eval      tipo_error = 3
032500060809     c                   eval      Testa_Field = 'DATA_ATT'
032600060809     C                   exsr      Field_Error
032700060809     c                   Else
032800060809     c                   eval      DATA_ATT = atoi(%TRIM(Dato_sk(sk)))
032900060809     c                   End
033000060809      **
033100060809      * ? Nel  4° campo : CAMPO NAZIONE                              COUNTRY
033200060809      **                                                             COUNTRY
033300060809     C                   z-add     4             sk
033400060809     c                   If        LunDato_sk(sk) >  0
033500060809     c                   eval      COUNTRY  = %subst(Dato_sk(sk):1:+
033600060809     c                                              LunDato_sk(sk))
033700060809     c                   End
033800060809      **
033900060809      **
034000060809      * ? Nel 31° campo : Esegue i Collection Request                CRAUTOM
034100060809      **                                                             CRAUTOM
034200060809     C                   z-add     31            sk
034300061003     c                   If        LunDato_sk(sk) =  0
034400060809      * ?-----> Errore
034500060809     C                   eval      tipo_error = 1
034600060809     c                   eval      Testa_Field = 'CR_AUTOM'
034700060809     C                   exsr      Field_Error
034800060809     c                   Else
034900060809     c                   eval      CR_AUTOM = %subst(Dato_sk(sk):1:+
035000060809     c                                              LunDato_sk(sk))
035100060925      * ?-
035200060925     c                   if        CR_AUTOM = 'Y'
035300060925     c                   eval      CR_AUTOM = 'S'
035400060925     c                   End
035500060925     c                   if        CR_AUTOM = 'N'
035600060925     c                   eval      CR_AUTOM = *blank
035700060925     c                   End
035800060809     c                   End
035900060809      **
036000060809      * ? Nel 57° campo : Descrizione del DEPOT                      DESCRIZ
036100060809      **                                                             DESCRIZ
036200060809     C                   z-add     57            sk
036300060809     c                   If        LunDato_sk(sk) >  0
036400060809     c                   eval      DESCRIZ  = %subst(Dato_sk(sk):1:+
036500060809     c                                              LunDato_sk(sk))
036600060809     c                   end
036700060808      **
036800060809      * ?----->  HA I CAMPI IMPOSTATI
036900060612      **
037000060612     c                   Endsr
037100060614      * ?------------------------------------------------------------------ */
037200060614      *?      Campi in errore
037300060614      * ?------------------------------------------------------------------ */
037400060614     c     Field_Error   Begsr
037500060614      **
037600060614     C                   eval      se_errore   = 'S'
037700060614      * msg video o in posta
037800060614     c                   If        Msg_Err = *blank
037900060614     c
038000060614     c                   Select
038100060614     c
038200060614     c                   When      Tipo_Error = 1
038300060809     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_field) +
038400060614     C                             ' >> Obblig.- Manca Dato !!'
038500060614     c
038600060614     c                   When      Tipo_Error = 2
038700060809     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_field) +
038800060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
038900060614     c
039000060614     c                   When      Tipo_Error = 3
039100060809     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_field) +
039200060614     C                             ' >> NON Numerico  !!'
039300060614     c
039400060614     c                   When      Tipo_Error = 4
039500060809     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_field) +
039600060614     C                             ' >> Conversione non riuscita !!'
039700060614     c                   EndSL
039800060614     c
039900060614     c                   End
040000060614      * Errore su VINMSG
040100060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
040200060809     c                                          + %trim(Testa_field)
040300060614      **
040400060614     c                   Endsr
040500060808      * ?------------------------------------------------------------------ */
040600060808      *?    Scrive i Files degli ORM
040700060808      * ?------------------------------------------------------------------ */
040800060809     c     WRT_FILE      Begsr
040900060808      **
041000060809      **  Tabella da AGGIORNARE
041100060808      **?              /*---------------------- */
041200171011     c                   ExSr      Aggiorna_LEG
041300060808      * ?              /*---------------------- */
041400060809
041500060809     c                   endSR
041600060808      * ?------------------------------------------------------------------ */
041700060809      *?    Imposta i dati
041800060808      * ?------------------------------------------------------------------ */
041900171011     c     Aggiorna_LEG  Begsr
042000060808      **
042100061103     c                   move      'S'           err_bloccante     1
042200061103     c                   movel     MancaDEP      Msg_Vin_80
042300060925
042400061003     c                   eval      wdata = data_vers
042500060925     c                   eval      depot = Cod_Depot
042600171011     c     kleg          setll     dpleg14l
042700060925     c                   do        *hival
042800171011     c     depot         readpe    dpleg14l
042900171011     c                   if        %eof(dpleg14l)
043000060925     c                   leave
043100060925     c                   endif
043200171011     c                   if        wdata >= legdde and wdata <= legdsc
043300171011     c                   movel     CR_AUTOM      legorma
043400061103     c                   clear                   err_bloccante
043500061103     C                   clear                   Msg_Vin_80
043600171011     c                   update    dpleg000
043700060925     c                   leave
043800060925     c                   endif
043900060925     c                   enddo
044000061106      **
044100080707      * ?-----> Errore
044200080707     c                   if        err_bloccante = 'S'
044300080707     C                   eval      se_errore = 'S'
044400080707     c                   End
044500061106      ** invio messaggio per avvisare disallinea
044600080513     c                   if        err_bloccante = 'S' and Msg_inviato = ' '
044700080513     c                   eval      Msg_inviato ='S'
044800080818      *
044900080818      * L'informazione di "non tutti i Depots sono stati ...." non è al momento
045000080818      *  importante e necessaria, si salta l'invio del messaggio
045100080818     c                   goto      no_mail
045200080818      *
045300061106     C                   exsr      Alert_email
045400080818      *
045500080818     c     no_mail       tag
045600080818      *
045700061106     c                   End
045800060808      **
045900060808     c                   Endsr
046000060612      * ?------------------------------------------------------------------ */
046100060612      *?      Suddivide i campi della riga con carattere divisorio (;)
046200060612      * ?------------------------------------------------------------------ */
046300061106     c     ALERT_EMAIL   Begsr
046400060612      **
046500061106     C* Inizializzo variabili
046600061106     C                   movel     *blanks       wrkEml          253
046700061106     C                   movel     *blanks       wrkMsg         5000
046800061106     C                   movel     *blanks       wrkOgg           44
046900061106      *
047000061106     C* Valorizzo i campi della e-m@ail - indirizzo
047100061106     C                   eval      wrkEml= 'CED@bartolini.it'
047200061106
047300080513      * Oggetto della Mail da inviare
047400080513     C                   eval      wrkOgg='Ricez.DEPOTS.TAB x gest. ORM autom.D+
047500080513     C                             epot DPD'
047600061106
047700171011     C                   eval      wrkMsg = 'Avviso: Nel file DPLEG00F,'       +
047800061106     C                             ' (File dei Depot DPD da cappario DPD),   ' +
047900080513     C                             ' NON tutti i Depots DPD sono stati '       +
048000080513     C                             ' codificati. Questo non è un messaggio '   +
048100080513     C                             ' Bloccante ma solo di avviso. '
048200061106     C*
048300061106     C                   call(e)   'TIS701C'
048400061106     C                   parm                    wrkEml
048500061106     C                   parm                    wrkOgg
048600061106     C                   parm                    wrkMsg
048700061106      **
048800061106     c                   Endsr
048900061106      * ?------------------------------------------------------------------ */
049000061106      *?      Suddivide i campi della riga con carattere divisorio (;)
049100061106      * ?------------------------------------------------------------------ */
049200061106     c     Split_Rec     Begsr
049300061106      **
049400060612     c                   clear                   Nr_campi
049500060612     c                   clear                   Campi_Record
049600060612     c                   clear                   Campi_Lunghi
049700060612     c                   clear                   Campi_Numerici
049800060612     c                   clear                   Campi_Decimali
049900060612     c                   movel     ';'           Separa_campi
050000060612     c                   movel     '.'           Decimal_Separ
050100060612      *
050200060612     c                   call      'FIEU00R'
050300060612      * ?     Input
050400060612     c                   parm                    Separa_campi
050500060612     c                   parm                    Decimal_Separ
050600060612     c                   parm                    dati
050700060612      * ?     Output  su schiere di 100 elementi
050800060612     c                   parm                    Nr_campi
050900060612     c                   parm                    Campi_Record                   * i dati
051000060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
051100060612     c                   parm                    Campi_Numerici                 * se numerici
051200060612     c                   parm                    Campi_Decimali                 * quanti decimali
051300060612      **
051400060612     c                   Endsr
051500971215      *----------------------------------------------------------------
051600051205      *   DEFINIZIONE CHIAVI                               *
051700060808      *----------------------------------------------------------------
051800051205     C     *INZSR        BEGSR
051900051205      *
052000991129     c     *ENTRY        PLIST
052100060613     C                   parm                    esito             1
052200060925     c*
052300171011     c     kleg          klist
052400060925     c                   kfld                    depot             4
052500060925     c                   kfld                    wdata             8 0
052600060809      *
052700060809      *  imposta UDATE di elaborazione
052800060925     C                   move      *date         data_oggi
052900060925     C                   move      *date         G02INV
053000060925     C                   MOVE      *ZEROS        G02dat
053100060809     C                   MOVE      *BLANKS       G02ERR
053200060809     C                   CALL      'XSRDA8'
053300060809     C                   PARM                    WLBDAT
053400060809     C                   move      data_oggi     day
053500060809     C                   TIME                    W0140
053600060809     C                   MOVEl     W0140         ore
053700050414      *
053800991124     C                   ENDSR
053900060621      * ?------------------------------------------------------------------ */
054000060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
054100060621      * ?------------------------------------------------------------------ */
054200060621     C     *pssr         BEGSR
054300060621     C
054400060710     C                   eval      esito = '2'
054500060621     C
054600060621     C                   ENDSR     '*CANCL'
054700060614      *-------------------------------------------------------------------------
