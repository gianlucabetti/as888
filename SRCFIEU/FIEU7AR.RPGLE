000100061222 * ?  ***************************************************************************
000200061222 * ?  *   Nuovo traduttore dati Sernam da file CSV
000300061222 * ?  ***************************************************************************
000400130718      * ?Il programma x funzionare ha bisogno di un file FLAT sul TIVIN00R
000500061222 * ?  *   che deve contenere i dati di un file (.csv) che utilizzi separatore
000600061222 * ?  *   fra i campi (;)
000700061222      * ?Basandosi sul file excel ricevuto da Sernam nelle colonne:
000800130718      *    AN =>(46) = Importo
000900130718      *     F =>(06) = Num.Sped.Bartolini
001000130718      *     G =>(07) = Riferimento Spedizione alternativo
001100130718      *     D =>(04) = Data Documento
001200100318      *     C =>(03) = Num. Documento Non è + gestito sostituito dal codice cliente
001300130718      *     Y =>(25) = Peso tassato
001400130718      *    AA =>(27) = Volume
001500061222      * ?Occorre fare attenzione se dovessero cambiare le posizioni alle colonne
001600061222      * ? dell'Excel.
001700061222 * ?  ***************************************************************************
001800060726     H DFTACTGRP(*NO) ACTGRP(*CALLER)
001900060726     H BNDDIR('QC2LE')
002000060727     H DECEDIT('0,') DATEDIT(*DMY.)
002100060726 * ?  ***************************************************************************
002200061221      *  Da UPLOAD                                                              *
002300061221      *                  --> CSV   Scarico Dati                                 *
002400060726      * Il pgm crea:                                                            *
002500130717      *             TNEFR00F x SERNAM                                           *
002600060727      * ?================================================================== */
002700130718     Ftivin00R  UF   E             DISK    rename(TIVIN000:INPUT)
002800130724      *
002900130724      *  per poter agganciare gli ORM ed arrivare alla spedizione
003000130724     Ftita432c  iF   E           k DISK
003100060727      *
003200130717     FTNEFR00F  O  a E             DISK
003300060727      * ?================================================================== */
003400040901     d stamp           ds
003500040901     d  lin                  367    368b 0
003600061221      *
003700061221     D                 DS
003800061221     D  Anno                   1      2  0
003900061221     D  Linea                  3      5  0
004000061221     D  Serie                  6      7  0
004100061221     D  Spediz                 8     14  0
004200061221     D Sped_DS                 1     14  0
004300040901      *
004400030624     D WLBDAT          DS
004500030624     D  G02DAT                 1      8  0
004600030624     D  G02INV                 9     16  0
004700030624     D  G02ERR                17     17
004800030624     D  G02TGI                18     22  0
004900060726      *
005000060726     D WDTGIO          S              8  0
005100060726     D DATEU           S              8  0
005200060726     D DATamg          S              8  0
005300060726     D DATA_eur        S               D   DATFMT(*eur)
005400060726     D DATA_amg        S               D   DATFMT(*iso)
005500060726      *-------------------
005600060726     D Tipo_error      S              1  0
005700060726     D Campo_errato    S             10
005800060726      * ?================================================================== */
005900060726     C*? Ds Decodifica dei campi
006000060726      * ?================================================================== */
006100061221      * ?   * Campi da tradurre con interi
006200061221     D atoll           PR            20I 0
006300061221     D                                     EXTPROC('atoll')
006400061221     D  Char                           *
006500061221     D                                     VALUE
006600061221     D                                     OPTIONS(*STRING)
006700060726      * ?   * Campi da tradurre con interi
006800060726     D atoi            PR            10I 0
006900060726     D                                     EXTPROC('atoi')
007000060726     D  Char                           *
007100060726     D                                     VALUE
007200060726     D                                     OPTIONS(*STRING)
007300060726      * ?   * Campi da tradurre con decimali
007400060726     D atof            PR             8F
007500060726     D                                     EXTPROC('atof')
007600060726     D  Char                           *
007700060726     D                                     VALUE
007800060726     D                                     OPTIONS(*STRING)
007900060726      * ?   *--------------------------------------------------------------*
008000060726      * ?   ( Descrizione Campi x scomposizione FLAT record )
008100060726      * ?   *--------------------------------------------------------------*
008200060726      *
008300060726      * ?   * Campi x decodifica * (INPUT  del Record)
008400060726     D  Dati           s           1500
008500060726     D Separa_campi    s              1
008600060726     D Decimal_Separ   s              1
008700060726      * ?   * Campi decodificati * (OUTPUT del Record)
008800060726      *       Schiere di Output / Campi di Output
008900060726     D  Alfa40         s             40
009000060726     d  Sk             s              3u 0
009100060726     D    NR_campi     s              3u 0
009200060726     D  Campi_Record   ds
009300060726     D    Dato_sk                    40    DIM(100)
009400060726     D  Campi_Lunghi   ds
009500060726     D    LunDato_sk                  3u 0 DIM(100)
009600060726     D Campi_Numerici  ds
009700060726     D    DatoNum_sk                  1    DIM(100)
009800060726     D Campi_Decimali  ds
009900060726     D    Decimal_sk                  3u 0 DIM(100)
010000060726      * ?   *--------------------------------------------------------------*
010100060726     D  position       s              3  0 INZ(0)
010200060726
010300060726     D  se_errore      s              1    inz(' ')
010400060726     D  Msg_Err        s            132    inz(' ')
010500060726      * ?* ------------------------------------------------------ *
010600060726     D Digits          C                   '0123456789'
010700060726      * ?================================================================== */
010800060726      *   Ciclo principale
010900060726      * ?================================================================== */
011000060726      * per controllare se almeno un record è stato importato sul VAB
011100060726     c                   clear                   Almeno_Uno        1
011200060726     c                   clear                   Esito             1
011300060726
011400060726      * Continua a leggere dal secondo record.
011500060726      *   Il primo è la testata già controllata dalla Check_trasm.
011600130718     c                   read      TIVIN00R
011700130718     c                   dow       not %eof(TIVIN00R)
011800060726
011900130719     c                   if        vinflg =' '
012000130719
012100060726      ** Controlli formali sui campi
012200060726     c                   clear                   se_errore
012300060726
012400060726      ** Decodifica record a campi variabili
012500060726     c                   exsr      Decod_Record
012600060726      *
012700060726      **  Se presente un errore nel record emette una segnalazione msg
012800060726     c                   if        se_errore ='S'
012900060726     c                   eval      esito  = '1'
013000060726     c                   else
013100130715      **
013200060726     c                   exsr      WRT_TNEFR
013300061221
013400061221     c                   endIF
013500060726
013600060726      *  Problemi nella decodifica dei campi VAB/VAT
013700060726     c                   if        se_errore ='S'
013800060726     c                   eval      esito  = '1'
013900130719     c                   movel     Msg_Err       vinMSG
014000130718     c                   move      '2'           vinFLG
014100060726     c                   else
014200060726      *  Record OK
014300130718     c                   move      '1'           vinFLG
014400060726     c                   move      'S'           Almeno_Uno
014500130718     c                   end
014600130718
014700130718     c                   update    INPUT
014800130719     c                   end
014900130719
015000130718     c                   read      TIVIN00R
015100130719
015200060726     C                   ENDdo
015300060726      **
015400060726     c                   SETON                                        LR
015500060726      * ?______________________________________________________________     */
015600060726      *?    Decodifica record a campi variabili
015700060726      * ?______________________________________________________________     */
015800060726     c     Decod_Record  Begsr
015900060915
016000060726      * ? Sposta il record a tracciato libero su un campo di lavoro       =
016100130718     c                   eval      dati = %trimr(vindta) + ';'
016200100920     c********           clear                   lunga             5 0
016300130718     c********           eval      lunga = %len(%trim(vindta))
016400100920     c********           eval      %subst(dati:lunga+1:1) = ';'
016500061221      **
016600060726     c                   exsr      Split_Rec
016700060726     c                   exsr      Rec_Dettaglio
016800060915      **
016900060726     c                   Endsr
017000060726      * ?_______________________________________________________________    */
017100060726      *?      Riga Bolla Import da controllare e tradurre
017200060726      * ?_______________________________________________________________    */
017300060726     c     Rec_Dettaglio Begsr
017400061221      **
017500061221     c                   clear                   Msg_Err
017600060726      **
017700060726      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
017800061221     c                   clear                   Tipo_DOC          3
017900061221     c                   clear                   Num_FATT         11 0
018000061221     c                   clear                   Dat_FATT          8 0
018100061221     c                   clear                   Num_Sped         14
018200130719     c                   clear                   Riferimento      20
018300061221     c                   clear                   Importo           7 2
018400130718     c                   clear                   PesoTass          9 3
018500130718     c                   clear                   Volume            9 6
018600060726      **
018700130718      * ? Nel 1° campo : Importo                     (Obbligatorio)  Importo
018800061221      **                                                             Importo
018900130719     C                   if        colonna1 >0
019000061222     C                   z-add     colonna1      sk
019100061221     c                   If        DatoNum_sk(sk) = 'N' or
019200061221     c                             LunDato_sk(sk) =  0
019300061221      * ?-----> Errore
019400061221     c                   eval      tipo_error = 2
019500061221     c                   eval      Campo_errato = 'Importo'
019600061221     c                   exsr      Field_Error
019700130719      * causa testata del file scritta in un certo modo
019800130719      * sono costretto a uscire x non far spaccare il programma
019900130719     c                   goto      End_Check
020000061221     c                   Else
020100061221     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
020200061221     c                   eval      Importo  = atof(%TRIM(Dato_sk(sk)))
020300061221     c                   End
020400130719     c                   End
020500061222      **
020600130723      * ? Nel  2° campo : Numero Riferimento         (Obbligatorio)  Num Spediz.Bart
020700061222      **                                                             Num Spediz.Bart
020800130719     C                   if        colonna2 >0
020900061222     C                   z-add     colonna2      sk
021000061222     c                   If        LunDato_sk(sk) =  0
021100061222      * ?-----> Errore
021200061222     c                   eval      tipo_error = 1
021300061222     c                   eval      Campo_errato = 'N.Spediz.'
021400061222     c                   exsr      Field_Error
021500061222     c                   Else
021600130723      ***
021700130723      ** attenzione se mandano come riferimento un ORM inseriscono "-" come separatore e
021800130723      **    comunque, sempre,
021900130723      **  nel primo carattere c'è sempre un apice per rendere alfa il dato su excel.
022000130723      ***
022100130724     c     digits        check     Dato_sk(sk):2 nr                3 0
022200130724     C                   IF        %FOUND
022300130724     C                   EXSR      NonNumeric
022400130724     c                   IF         NOSTRA_BOLLA <> *blank and
022500130724     c                              NOSTRO_ORM   <> *blank
022600130724     c                   eval      Num_Sped   = NOSTRA_BOLLA
022700130724     c                   end
022800130724     C                   ENDIF
022900130724      *
023000130724      *** se non era un ORM
023100130724     c                   if        Num_Sped   = *Blank
023200130723     c                   if        LunDato_sk(sk)>14
023300130723     c                              and %subst(Dato_sk(sk):1:1)=''''
023400130718     c                   eval      Num_Sped   =  %subst(Dato_sk(sk):
023500130723     c                               2:LunDato_sk(sk))
023600130718     c                   else
023700130718     c                   eval      Num_Sped   = %subst(Dato_sk(sk):1:+
023800130718     c                                              LunDato_sk(sk))
023900130718     c                   End
024000130724     c                   end
024100130724      **
024200130719     c                   eval      Riferimento= %subst(Dato_sk(sk):1:+
024300130719     c                                              LunDato_sk(sk))
024400061222     c                   End
024500130719     c                   End
024600060726      **
024700130723      * ? Nel  3° campo : Numero Riferimento      Alternativo        Num Alternativo
024800130723      **                                                             Num Alternativo
024900130724     C                   if        colonna3 >0 and Num_Sped = *Blank and
025000130724     c                             %Len(%Trim(Num_Sped)) < 14
025100130718     C                   z-add     colonna3      sk
025200130718     c                   If        LunDato_sk(sk) =  0
025300130718      * ?-----> Errore
025400130718     c                   eval      tipo_error = 1
025500130718     c                   eval      Campo_errato = 'Rif-Alter'
025600130718     c                   exsr      Field_Error
025700130718     c                   Else
025800130718     c                   if        LunDato_sk(sk)>14
025900130723     c                              and %subst(Dato_sk(sk):1:1)=''''
026000130718     c                   eval      Num_Sped   =  %subst(Dato_sk(sk):
026100130723     c                               2:LunDato_sk(sk))
026200130718     c                   else
026300130718     c                   eval      Num_Sped   = %subst(Dato_sk(sk):1:+
026400130718     c                                              LunDato_sk(sk))
026500130718     c                   End
026600130718     c                   End
026700130719     c                   End
026800130718      *
026900130723      *
027000100318      **  Non essendo + gestito lo imposto fisso
027100130719     C                   if        colonna4 >0
027200130718     c                   eval      Tipo_Doc   = 'FAT'
027300061222      **
027400130718      * ? Nel  4° campo : Data Documento             (Obbligatorio)  Data   Documento
027500061222      **                                                             Data   Documento
027600061222     C                   z-add     colonna4      sk
027700061222     c                   If        DatoNum_sk(sk) = 'N' or
027800061222     c                             LunDato_sk(sk) =  0
027900061222      * ?-----> Errore
028000061222     C                   eval      tipo_error = 2
028100061222     c                   eval      Campo_errato = 'Data Fatt'
028200061222     C                   exsr      Field_Error
028300061222     c                   Else
028400061222     c                   eval      Dat_FATT = atoi(%TRIM(Dato_sk(sk)))
028500130718     C                   z-add     Dat_FATT      campo4_0          4 0
028600061222     c                   End
028700130718      **-
028800130718      **- Se la data è in GG/MM/AAAA  la devo girare
028900130718     c                   if        campo4_0 > 2000
029000130718     C                   move      Dat_FATT      G02DAT
029100130718     C                   MOVE      *ZEROS        G02INV
029200130718     C                   MOVE      ' '           G02ERR
029300130718     C                   CALL      'XSRDA8'
029400130718     C                   PARM                    WLBDAT
029500130718     C                   Z-ADD     G02INV        Dat_FATT
029600130718     c                   end
029700130719     c                   end
029800061221      **
029900130718      * ? Nel  5° campo : Num Documento              (Obbligatorio)  Numero Documento
030000061221      **                                                             Numero Documento
030100130719     C                   if        colonna5 >0
030200061222     C                   z-add     colonna5      sk
030300061221     c                   If        DatoNum_sk(sk) = 'N' or
030400061221     c                             LunDato_sk(sk) =  0
030500061221      * ?-----> Errore
030600061221     C                   eval      tipo_error = 2
030700061221     c                   eval      Campo_errato = 'N.Fattura'
030800061221     C                   exsr      Field_Error
030900061221     c                   Else
031000061221     c                   eval      Num_FATT = atoll(%TRIM(Dato_sk(sk)))
031100061221     c                   End
031200130719     c                   end
031300130718      **
031400130718      * ? Nel 6° campo : Peso Tassato                (Obbligatorio)  Valore
031500130718      **                                                             valore
031600130719     C                   if        colonna6 >0
031700130718     C                   z-add     colonna6      sk
031800130718     c                   If        DatoNum_sk(sk) = 'N' or
031900130718     c                             LunDato_sk(sk) =  0
032000130718      * ?-----> Errore
032100130718     c                   eval      tipo_error = 2
032200130718     c                   eval      Campo_errato = 'Peso_T.'
032300130718     c                   exsr      Field_Error
032400130718     c                   Else
032500130718     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
032600130718     c                   eval      PesoTass = atof(%TRIM(Dato_sk(sk)))
032700130718     c                   End
032800130719     c                   end
032900130718      **
033000130718      * ? Nel 7° campo : Volume                      (Obbligatorio)  Valore
033100130718      **                                                             valore
033200130719     C                   if        colonna7 >0
033300130718     C                   z-add     colonna7      sk
033400130718     c                   If        DatoNum_sk(sk) = 'N' or
033500130718     c                             LunDato_sk(sk) =  0
033600130718      * ?-----> Errore
033700130718     c                   eval      tipo_error = 2
033800130718     c                   eval      Campo_errato = 'Volume '
033900130718     c                   exsr      Field_Error
034000130718     c                   Else
034100130718     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
034200130718     c                   eval      Volume   = atof(%TRIM(Dato_sk(sk)))
034300130718     c                   End
034400130719     c                   end
034500060726      **
034600060726     c     End_Check     tag
034700060726      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
034800060726      **
034900060726     c                   Endsr
035000060726      * ?____________________________________________________________       */
035100130724      *?   se il campo contiene il riferimento non come SPEDIZIONE BRT di 14 digits
035200060726      * ?____________________________________________________________       */
035300130724     c     NonNumeric    Begsr
035400130724      **
035500130724     c                   clear                   SALVA_RIFER      35
035600130724     c                   clear                   NOSTRA_BOLLA     14
035700130724     c                   clear                   NOSTRO_ORM       14
035800130724     c                   clear                   VALUE            40
035900130724     c                   eval      Value = Dato_sk(sk)
036000130724      **
036100130724     c                   clear                   Nbytes            3 0
036200130724     c                   CLEAR                   FLDstd            7
036300130724     c                   move      *zeros        fieldPOE          3
036400130724     c                   move      *zeros        fieldNSR          2
036500130724     c                   move      *zeros        fieldNOR          7
036600130724     c                   move      *zeros        fieldNRV          2
036700130724      **
036800130724      **  il nr posizionale è salvato ma sempre incrementato di (1)
036900130724      **    per eseguire correttamente le substring seguenti
037000130724     c                   z-add     Nr            SavNR1            3 0
037100130724     c                   add       1             SavNR1            3 0
037200130724      **
037300130724      **   potrebbe essere un ORM se ha i campi separati da (-)
037400130724     c                   if        %subst(value:Nr:1)='-'
037500130724      *****
037600130724     c                   eval      fldSTD= %subst(value:2:Nr-2)
037700130724     c                   eval      Nbytes = %len(%trim(fldstd))
037800130724      *carica il POE
037900130724     c                   if        Nbytes =1
038000130724     c                   EVAL      %subst(fieldPOE:3:1)=%subst(fldSTD:1:1)
038100130724     c                   elseif    Nbytes =2
038200130724     c                   EVAL      %subst(fieldPOE:2:2)=%subst(fldSTD:1:2)
038300130724     c                   elseif    Nbytes =3
038400130724     c                   EVAL      fieldPOE =%subst(fldSTD:1:3)
038500130724     c                   end
038600130724      **
038700130724      **  DOPO AVER CARICATO il POE deve vedere se c'è SERIE o NUMERO o PROGRESSIVO
038800130724      **    QUINDI CERCA un (-) SEGUENTE
038900130724      **
039000130724      ***      controlla la presenza di un altro (-) separatore
039100130724     c                   z-add     0             w                 3 0
039200130724     c                   eval      nr = nr +1
039300130724     c     '-'           scan      value:nr      w
039400130724     C                   IF        not %FOUND
039500130724      **   SE NON C'E'
039600130724      **     imposta il NOR direttamente e basta
039700130724      **-----
039800130724     c                   z-add     Nr            SavNR1            3 0
039900130724     c     ' '           scan      value:nr      w
040000130724     C                   IF        %FOUND
040100130724     c                   eval      nr = w
040200130724     c                   eval      fldSTD=
040300130724     c                             %subst(value:savNR1:(nr-savNR1))
040400130724     c                   eval      Nbytes = %len(%trim(fldstd))
040500130724      *carica il NUMERO
040600130724     c                   if        Nbytes =1
040700130724     c                   EVAL      %subst(fieldNOR:7:1)=%subst(fldSTD:1:1)
040800130724     c                   elseif    Nbytes =2
040900130724     c                   EVAL      %subst(fieldNOR:6:2)=%subst(fldSTD:1:2)
041000130724     c                   elseif    Nbytes =3
041100130724     c                   EVAL      %subst(fieldNOR:5:3)=%subst(fldSTD:1:3)
041200130724     c                   elseif    Nbytes =4
041300130724     c                   EVAL      %subst(fieldNOR:4:4)=%subst(fldSTD:1:4)
041400130724     c                   elseif    Nbytes =5
041500130724     c                   EVAL      %subst(fieldNOR:3:5)=%subst(fldSTD:1:5)
041600130724     c                   elseif    Nbytes =6
041700130724     c                   EVAL      %subst(fieldNOR:2:6)=%subst(fldSTD:1:6)
041800130724     c                   elseif    Nbytes =7
041900130724     c                   EVAL      %subst(fieldNOR:1:7)=%subst(fldSTD:1:7)
042000130724     c                   end
042100130724      *
042200130724     c                   END
042300130724      **-----
042400130724     C                   ELSE
042500130724      **-----
042600130724      **   SE C'E'
042700130724     c                   eval      nr = w
042800130724     c                   eval      fldSTD=
042900130724     c                             %subst(value:savNR1:(nr-savNR1))
043000130724     c                   eval      Nbytes = %len(%trim(fldstd))
043100130724     c                   clear                   conNRserie        1
043200130724      *
043300130724      * dopo averlo utilizzato:
043400130724      **  il nr posizionale è salvato ma sempre incrementato di (1)
043500130724      **    per eseguire correttamente le substring seguenti
043600130724     c                   z-add     Nr            SavNR1            3 0
043700130724      *carica la SERIE
043800130724     c                   if        Nbytes =1
043900130724     c                   EVAL      %subst(fieldNSR:2:1)=%subst(fldSTD:1:1)
044000130724     c                   move      'S'           conNRserie
044100130724     c                   elseif    Nbytes =2
044200130724     c                   EVAL      fieldNSR=%subst(fldSTD:1:2)
044300130724     c                   move      'S'           conNRserie
044400130724      *caricata  SERIE                         ===
044500130724     ***
044600130724      *carica la NUMERO
044700130724     c                   elseif    Nbytes =3
044800130724     c                   EVAL      %subst(fieldNOR:5:3)=%subst(fldSTD:1:3)
044900130724     c                   elseif    Nbytes =4
045000130724     c                   EVAL      %subst(fieldNOR:4:4)=%subst(fldSTD:1:4)
045100130724     c                   elseif    Nbytes =5
045200130724     c                   EVAL      %subst(fieldNOR:3:5)=%subst(fldSTD:1:5)
045300130724     c                   elseif    Nbytes =6
045400130724     c                   EVAL      %subst(fieldNOR:2:6)=%subst(fldSTD:1:6)
045500130724     c                   elseif    Nbytes =7
045600130724     c                   EVAL      %subst(fieldNOR:1:7)=%subst(fldSTD:1:7)
045700130724      *caricato  NUMERO                        ===
045800130724     c                   end
045900130724      **--------
046000130724      *** CONTROLLA SE C'E'UN ALTRO (-)  oppure
046100130724     c                   eval      nr = nr +1
046200130724     c                   z-add     Nr            SavNR1            3 0
046300130724     c     '-'           scan      value:nr      w
046400130724     c                   if        not %FOUND
046500130724     c     ' '           scan      value:nr      w
046600130724     c                   end
046700130724      * TROVATO
046800130724     C                   IF        %FOUND
046900130724      * dopo averlo utilizzato:
047000130724      **  il nr posizionale è salvato ma sempre incrementato di (1)
047100130724      **    per eseguire correttamente le substring seguenti
047200130724     c                   eval      nr = w
047300130724      **
047400130724     c                   eval      fldSTD=
047500130724     c                             %subst(value:savNR1:(nr-savNR1))
047600130724     c                   eval      Nbytes = %len(%trim(fldstd))
047700130724      **
047800130724      **   se NON aveva caricato la SERIE allora manca il solo PROGRESSIVO
047900130724     c                   IF        conNRserie = *blank
048000130724      *
048100130724      *carica il PROGRESSIVO
048200130724     c                   if        Nbytes =1
048300130724     c                   EVAL      %subst(fieldNRV:2:1)=%subst(fldSTD:1:1)
048400130724     c                   elseif    Nbytes =2
048500130724     c                   EVAL      fieldNRV=%subst(fldSTD:1:2)
048600130724      *caricato  PROGRESSIVO                   ===
048700130724     c                   end
048800130724      **
048900130724      **  se invece aveva caricato la SERIE deve caricare il
049000130724     c                   elseIF    conNRserie <>*blank
049100130724      *carica il NUMERO
049200130724     c                   if        Nbytes =1
049300130724     c                   EVAL      %subst(fieldNOR:7:1)=%subst(fldSTD:1:1)
049400130724     c                   elseif    Nbytes =2
049500130724     c                   EVAL      %subst(fieldNOR:6:2)=%subst(fldSTD:1:2)
049600130724     c                   elseif    Nbytes =3
049700130724     c                   EVAL      %subst(fieldNOR:5:3)=%subst(fldSTD:1:3)
049800130724     c                   elseif    Nbytes =4
049900130724     c                   EVAL      %subst(fieldNOR:4:4)=%subst(fldSTD:1:4)
050000130724     c                   elseif    Nbytes =5
050100130724     c                   EVAL      %subst(fieldNOR:3:5)=%subst(fldSTD:1:5)
050200130724     c                   elseif    Nbytes =6
050300130724     c                   EVAL      %subst(fieldNOR:2:6)=%subst(fldSTD:1:6)
050400130724     c                   elseif    Nbytes =7
050500130724     c                   EVAL      %subst(fieldNOR:1:7)=%subst(fldSTD:1:7)
050600130724     c                   end
050700130724      *
050800130724      *caricato  NUMERO                        ===
050900130724      *** CONTROLLA SE C'E'UN ALTRO (-)   x il PROGRESSIVO
051000130724     c                   eval      nr = nr +1
051100130724     c                   z-add     Nr            SavNR1
051200130724     c     '-'           scan      value:nr      w
051300130724     c                   if        not %FOUND
051400130724     c     ' '           scan      value:nr      w
051500130724     c                   end
051600130724      * TROVATO
051700130724     C                   IF        %FOUND
051800130724     c                   eval      nr = w
051900130724     c                   eval      fldSTD=
052000130724     c                             %subst(value:savNR1:(nr-savNR1))
052100130724     c                   eval      Nbytes = %len(%trim(fldstd))
052200130724      *
052300130724      *carica il PROGRESSIVO
052400130724     c                   if        Nbytes =1
052500130724     c                   EVAL      %subst(fieldNRV:2:1)=%subst(fldSTD:1:1)
052600130724     c                   elseif    Nbytes =2
052700130724     c                   EVAL      fieldNRV=%subst(fldSTD:1:2)
052800130724      *caricato  PROGRESSIVO                   ===
052900130724     c                   end
053000130724      **
053100130724     C                   END
053200130724      **
053300130724     c                   end
053400130724      *
053500130724     C                   END
053600130724      ***
053700130724     C                   ENDIF
053800130724      **
053900130724      **  Aggancia l'ORM e poi la SPEDIZIONE
054000130724     C                   eval      NOSTRO_ORM = fieldPOE +
054100130724     C                                          fieldNSR +
054200130724     C                                          fieldNOR +
054300130724     C                                          fieldNRV
054400130724     C     NOSTRO_ORM    CHAIN     TITA432C
054500130724     C                   if        %Found(TITA432C)
054600130724      **
054700130724     c                   eval      NOSTRA_BOLLA =
054800130724     c                             %subst(%editc(ta4AAS:'X'):3:2) +
054900130724     c                                    %editc(ta4LNP:'X')      +
055000130724     c                                    %editc(ta4NRS:'X')      +
055100130724     c                                    %editc(ta4NSP:'X')
055200130724     c                   eval      SALVA_RIFER = value
055300130724     c                   end
055400130724      ***
055500130724     c                   endIF
055600130724      **
055700130724     c                   Endsr
055800130724      * ?____________________________________________________________       */
055900130724      *?      Campi in errore
056000130724      * ?____________________________________________________________       */
056100130724     c     Field_Error   Begsr
056200130724      **
056300060726     C                   eval      se_errore   = 'S'
056400060726      * msg errore
056500060726     c                   If        Msg_Err = *blank
056600060726     c
056700060726     c                   Select
056800060726     c
056900060726      * msg errore : Obbligatorio
057000060726     c                   When      Tipo_Error = 1
057100060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
057200060726     C                             ' >> Obblig.- Manca Dato !!'
057300060726     c
057400060726      * msg errore : Obbligatorio / Numerico
057500060726     c                   When      Tipo_Error = 2
057600060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
057700060726     C                             ' >> Obblig.Mancante o NON Numerico !!'
057800060726     c
057900060726      * msg errore : Numerico
058000060726     c                   When      Tipo_Error = 3
058100060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
058200060726     C                             ' >> NON Numerico  !!'
058300060726     c                   EndSL
058400060726     c
058500060726     c                   End
058600060726      **
058700060726     c                   Endsr
058800060726      * ?____________________________________________________________       */
058900060726      *?      Suddivide i campi della riga con carattere divisorio (;)
059000060726      * ?____________________________________________________________       */
059100060726     c     Split_Rec     Begsr
059200060726      **
059300060726     c                   clear                   Nr_campi
059400060726     c                   clear                   Campi_Record
059500060726     c                   clear                   Campi_Lunghi
059600060726     c                   clear                   Campi_Numerici
059700060726     c                   clear                   Campi_Decimali
059800060726     c                   movel     ';'           Separa_campi
059900060727     c                   movel     ','           Decimal_Separ
060000060726      *
060100060726     c                   call      'FIEU00R'
060200060726      * ?     Input
060300060726     c                   parm                    Separa_campi
060400060726     c                   parm                    Decimal_Separ
060500060726     c                   parm                    dati
060600060726      * ?     Output  su schiere di 100 elementi
060700060726     c                   parm                    Nr_campi
060800060726     c                   parm                    Campi_Record                   * i dati
060900060726     c                   parm                    Campi_Lunghi                   * lunghezza dati
061000060726     c                   parm                    Campi_Numerici                 * se numerici
061100060726     c                   parm                    Campi_Decimali                 * quanti decimali
061200060726      **
061300060726     c                   Endsr
061400060726      * ?_______________________________________________________________    */
061500130717     C*? ESEGUO SCRITTURA del File TNEFR00F
061600060726      * ?_______________________________________________________________    */
061700060726     C     WRT_TNEFR     BEGSR
061800060726      *
061900060726     c                   clear                   tnEFR000
062000060726      *
062100061221     c                   eval      EFRNTW = Partner
062200061221     c                   eval      EFRGRP = '001'
062300061221     c                   eval      EFRCAU = Tipo_Doc
062400130719     c                   eval      EFRRIF = Num_Sped
062500061221      *
062600060726     c                   move      *all'0'       EFRNFT
062700061221      *
062800061221     c                   if        Num_Fatt > 0
062900061221     c                   move      Num_Fatt      EFRNFT
063000060726     c                   end
063100060726      *
063200060726      * cambia in tutti 9 il nr.fattura se non era presente
063300060726     c                   if        EFRNFT = *all'0'
063400060726     c                   move      *all'9'       EFRNFT
063500060726     c                   end
063600060726      *
063700061221     c                   if        Dat_Fatt > 0
063800061221     c                   z-add     Dat_Fatt      EFRDFT
063900061221     c                   end
064000060726      *
064100060726      * se è un credito va inserito in negativo
064200061221     c*****              move      'C'           EFRTPD
064300130718     c*****              move      'D'           EFRTPD
064400130718      *
064500061221     c                   z-add     Importo       EFRIMP
064600060726     c                   move      'F'           EFRTPD
064700130718      *
064800060726      *  imposta UDATE
064900060726     C                   move      *date         G02DAT
065000060726     C                   MOVE      *ZEROS        G02INV
065100060726     C                   MOVE      *BLANKS       G02ERR
065200060726     C                   CALL      'XSRDA8'
065300060726     C                   PARM                    WLBDAT
065400060726     C                   Z-ADD     G02INV        efrDTR
065500130718     c                   z-add     PesoTass      EFRPES
065600130718     c                   z-add     Volume        EFRVOL
065700130719     c                   movel     Riferimento   EFRRFP
065800060726      *
065900060726     c                   write     tnEFR000
066000060726      *
066100060726     c                   Endsr
066200060726      * ?_______________________________________________________________    */
066300060726     C     *INZSR        BEGSR
066400060726      * ?_______________________________________________________________    */
066500060726      *
066600060726     c     *ENTRY        PLIST
066700061222     c                   parm                    partner           3
066800061222     C                   parm                    colonna1          3 0
066900061222     C                   parm                    colonna2          3 0
067000061222     C                   parm                    colonna3          3 0
067100061222     C                   parm                    colonna4          3 0
067200061222     C                   parm                    colonna5          3 0
067300130718     C                   parm                    colonna6          3 0
067400130718     C                   parm                    colonna7          3 0
067500061222     C                   parm                    esito             1
067600060726      *
067700060726      *  imposta UDATE
067800060726     C                   move      *date         G02DAT
067900060726     C                   MOVE      *ZEROS        G02INV
068000060726     C                   MOVE      *BLANKS       G02ERR
068100060726     C                   CALL      'XSRDA8'
068200060726     C                   PARM                    WLBDAT
068300060726     C                   Z-ADD     G02INV        data_oggi         8 0
068400060726      * Recupero data e ora
068500060726     C                   TIME                    W0140            14 0
068600061221     C                   MOVEl     W0140         ore               6 0
068700060726      * UDATE IN GGMMAAAA
068800060726     C                   MOVE      W0140         WDTGIO
068900060726      * UDATE IN AAAAMMGG
069000060726     C     *eur          MOVEL     WDTGIO        DATA_eur
069100060726     C     *iso          MOVEL     DATA_eur      dateu
069200060726     c                   seton                                        66
069300060726      *
069400060726     c                   Endsr
069500060911      * ?_______________________________________________________________    */
