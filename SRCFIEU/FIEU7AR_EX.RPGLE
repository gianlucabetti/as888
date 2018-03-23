000100061222 * ?  ***************************************************************************
000200061222 * ?  *   Nuovo traduttore dati Sernam da file CSV
000300061222 * ?  ***************************************************************************
000400070228      * ?Il programma x funzionare ha bisogno di un file FLAT : "fieu7aF"
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
002700060727     FTNcfp01L  iF   E           k DISK
002800070228     Ffieu7aF   UF   E             DISK    rename(FIEU7AF:INPUT)
002900061221     F                                     PREFIX(x)
003000060727      *
003100130717     FTNEFR00F  O  a E             DISK
003200060727      * ?================================================================== */
003300040901     d stamp           ds
003400040901     d  lin                  367    368b 0
003500061221      *
003600061221     D                 DS
003700061221     D  Anno                   1      2  0
003800061221     D  Linea                  3      5  0
003900061221     D  Serie                  6      7  0
004000061221     D  Spediz                 8     14  0
004100061221     D Sped_DS                 1     14  0
004200040901      *
004300030624     D WLBDAT          DS
004400030624     D  G02DAT                 1      8  0
004500030624     D  G02INV                 9     16  0
004600030624     D  G02ERR                17     17
004700030624     D  G02TGI                18     22  0
004800060726      *
004900060726     D WDTGIO          S              8  0
005000060726     D DATEU           S              8  0
005100060726     D DATamg          S              8  0
005200060726     D DATA_eur        S               D   DATFMT(*eur)
005300060726     D DATA_amg        S               D   DATFMT(*iso)
005400060726      *-------------------
005500060726     D Tipo_error      S              1  0
005600060726     D Campo_errato    S             10
005700060726      * ?================================================================== */
005800060726     C*? Ds Decodifica dei campi
005900060726      * ?================================================================== */
006000061221      * ?   * Campi da tradurre con interi
006100061221     D atoll           PR            20I 0
006200061221     D                                     EXTPROC('atoll')
006300061221     D  Char                           *
006400061221     D                                     VALUE
006500061221     D                                     OPTIONS(*STRING)
006600060726      * ?   * Campi da tradurre con interi
006700060726     D atoi            PR            10I 0
006800060726     D                                     EXTPROC('atoi')
006900060726     D  Char                           *
007000060726     D                                     VALUE
007100060726     D                                     OPTIONS(*STRING)
007200060726      * ?   * Campi da tradurre con decimali
007300060726     D atof            PR             8F
007400060726     D                                     EXTPROC('atof')
007500060726     D  Char                           *
007600060726     D                                     VALUE
007700060726     D                                     OPTIONS(*STRING)
007800060726      * ?   *--------------------------------------------------------------*
007900060726      * ?   ( Descrizione Campi x scomposizione FLAT record )
008000060726      * ?   *--------------------------------------------------------------*
008100060726      *
008200060726      * ?   * Campi x decodifica * (INPUT  del Record)
008300060726     D  Dati           s           1500
008400060726     D Separa_campi    s              1
008500060726     D Decimal_Separ   s              1
008600060726      * ?   * Campi decodificati * (OUTPUT del Record)
008700060726      *       Schiere di Output / Campi di Output
008800060726     D  Alfa40         s             40
008900060726     d  Sk             s              3u 0
009000060726     D    NR_campi     s              3u 0
009100060726     D  Campi_Record   ds
009200060726     D    Dato_sk                    40    DIM(100)
009300060726     D  Campi_Lunghi   ds
009400060726     D    LunDato_sk                  3u 0 DIM(100)
009500060726     D Campi_Numerici  ds
009600060726     D    DatoNum_sk                  1    DIM(100)
009700060726     D Campi_Decimali  ds
009800060726     D    Decimal_sk                  3u 0 DIM(100)
009900060726      * ?   *--------------------------------------------------------------*
010000060726     D  position       s              3  0 INZ(0)
010100060726
010200060726     D  se_errore      s              1    inz(' ')
010300060726     D  Msg_Err        s            132    inz(' ')
010400060726      * ?* ------------------------------------------------------ *
010500060726     D Digits          C                   '0123456789'
010600060726      * ?================================================================== */
010700060726      *   Ciclo principale
010800060726      * ?================================================================== */
010900060726      * per controllare se almeno un record è stato importato sul VAB
011000060726     c                   clear                   Almeno_Uno        1
011100060726     c                   clear                   Esito             1
011200060726
011300060726      * Continua a leggere dal secondo record.
011400060726      *   Il primo è la testata già controllata dalla Check_trasm.
011500070228     c                   read      fieu7aF
011600070228     c                   dow       not %eof(fieu7aF)
011700060726
011800060726      ** Controlli formali sui campi
011900060726     c                   clear                   se_errore
012000060726
012100060726      ** Decodifica record a campi variabili
012200060726     c                   exsr      Decod_Record
012300060726      *
012400060726      **  Se presente un errore nel record emette una segnalazione msg
012500060726     c                   if        se_errore ='S'
012600060726     c                   eval      esito  = '1'
012700060726     c                   else
012800130715      **
012900060726     c                   exsr      WRT_TNEFR
013000061221
013100061221     c                   endIF
013200060726
013300060726      *  Problemi nella decodifica dei campi VAB/VAT
013400060726     c                   if        se_errore ='S'
013500060726     c                   eval      esito  = '1'
013600070228     c                   move      Msg_Err       xfieu7aF
013700061221     c                   update    INPUT
013800060726     c                   else
013900060726      *  Record OK
014000060726     c                   move      'S'           Almeno_Uno
014100061221     c                   delete    INPUT
014200060726     c                   end
014300060726
014400070228     c                   read      fieu7aF
014500060726     C                   ENDdo
014600060726      **
014700060726     c                   SETON                                        LR
014800060726      * ?______________________________________________________________     */
014900060726      *?    Decodifica record a campi variabili
015000060726      * ?______________________________________________________________     */
015100060726     c     Decod_Record  Begsr
015200060915
015300060726      * ? Sposta il record a tracciato libero su un campo di lavoro       =
015400100920     c                   eval      dati = %trimr(xfieu7aF) + ';'
015500100920     c********           clear                   lunga             5 0
015600100920     c********           eval      lunga = %len(%trim(xfieu7aF))
015700100920     c********           eval      %subst(dati:lunga+1:1) = ';'
015800061221      **
015900060726     c                   exsr      Split_Rec
016000060726     c                   exsr      Rec_Dettaglio
016100060915      **
016200060726     c                   Endsr
016300060726      * ?_______________________________________________________________    */
016400060726      *?      Riga Bolla Import da controllare e tradurre
016500060726      * ?_______________________________________________________________    */
016600060726     c     Rec_Dettaglio Begsr
016700061221      **
016800061221     c                   clear                   Msg_Err
016900060726      **
017000060726      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
017100061221     c                   clear                   Tipo_DOC          3
017200061221     c                   clear                   Num_FATT         11 0
017300061221     c                   clear                   Dat_FATT          8 0
017400061221     c                   clear                   Num_Sped         14
017500130718     c                   clear                   Riferimento      14
017600061221     c                   clear                   Importo           7 2
017700130718     c                   clear                   PesoTass          9 3
017800130718     c                   clear                   Volume            9 6
017900060726      **
018000130718      * ? Nel 1° campo : Importo                     (Obbligatorio)  Importo
018100061221      **                                                             Importo
018200061222     C                   z-add     colonna1      sk
018300061221     c                   If        DatoNum_sk(sk) = 'N' or
018400061221     c                             LunDato_sk(sk) =  0
018500061221      * ?-----> Errore
018600061221     c                   eval      tipo_error = 2
018700061221     c                   eval      Campo_errato = 'Importo'
018800061221     c                   exsr      Field_Error
018900061221     c                   Else
019000061221     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
019100061221     c                   eval      Importo  = atof(%TRIM(Dato_sk(sk)))
019200061221     c                   End
019300061222      **
019400130718      * ? Nel  2° campo : Numero Spedizione Bart     (Obbligatorio)  Num Spediz.Bart
019500061222      **                                                             Num Spediz.Bart
019600061222     C                   z-add     colonna2      sk
019700061222     c                   If        LunDato_sk(sk) =  0
019800061222      * ?-----> Errore
019900061222     c                   eval      tipo_error = 1
020000061222     c                   eval      Campo_errato = 'N.Spediz.'
020100061222     c                   exsr      Field_Error
020200061222     c                   Else
020300130718     c                   if        LunDato_sk(sk)>14
020400130718     c                   eval      Num_Sped   =  %subst(Dato_sk(sk):
020500130718     c                               LunDato_sk(sk)-13:LunDato_sk(sk))
020600130718     c                   else
020700130718     c                   eval      Num_Sped   = %subst(Dato_sk(sk):1:+
020800130718     c                                              LunDato_sk(sk))
020900130718     c                   End
021000061222     c                   End
021100060726      **
021200130718      * ? Nel  3° campo : Numero Spedizione Bart  Alternativo        Num Spediz.Bart
021300130718      **                                                             Num Spediz.Bart
021400130718     c                   if        %Len(%Trim(Num_Sped)) < 14
021500130718     C                   z-add     colonna3      sk
021600130718     c                   If        LunDato_sk(sk) =  0
021700130718      * ?-----> Errore
021800130718     c                   eval      tipo_error = 1
021900130718     c                   eval      Campo_errato = 'Rif-Alter'
022000130718     c                   exsr      Field_Error
022100130718     c                   Else
022200130718     c                   if        LunDato_sk(sk)>14
022300130718     c                   eval      Num_Sped   =  %subst(Dato_sk(sk):
022400130718     c                               LunDato_sk(sk)-13:LunDato_sk(sk))
022500130718     c                   else
022600130718     c                   eval      Num_Sped   = %subst(Dato_sk(sk):1:+
022700130718     c                                              LunDato_sk(sk))
022800130718     c                   End
022900130718     c                   End
023000130718     c                   End
023100130718      *
023200100318      **  Non essendo + gestito lo imposto fisso
023300130718     c                   eval      Tipo_Doc   = 'FAT'
023400061222      **
023500130718      * ? Nel  4° campo : Data Documento             (Obbligatorio)  Data   Documento
023600061222      **                                                             Data   Documento
023700061222     C                   z-add     colonna4      sk
023800061222     c                   If        DatoNum_sk(sk) = 'N' or
023900061222     c                             LunDato_sk(sk) =  0
024000061222      * ?-----> Errore
024100061222     C                   eval      tipo_error = 2
024200061222     c                   eval      Campo_errato = 'Data Fatt'
024300061222     C                   exsr      Field_Error
024400061222     c                   Else
024500061222     c                   eval      Dat_FATT = atoi(%TRIM(Dato_sk(sk)))
024600130718     C                   z-add     Dat_FATT      campo4_0          4 0
024700061222     c                   End
024800130718      **-
024900130718      **- Se la data è in GG/MM/AAAA  la devo girare
025000130718     c                   if        campo4_0 > 2000
025100130718     C                   move      Dat_FATT      G02DAT
025200130718     C                   MOVE      *ZEROS        G02INV
025300130718     C                   MOVE      ' '           G02ERR
025400130718     C                   CALL      'XSRDA8'
025500130718     C                   PARM                    WLBDAT
025600130718     C                   Z-ADD     G02INV        Dat_FATT
025700130718     c                   end
025800061221      **
025900130718      * ? Nel  5° campo : Num Documento              (Obbligatorio)  Numero Documento
026000061221      **                                                             Numero Documento
026100061222     C                   z-add     colonna5      sk
026200061221     c                   If        DatoNum_sk(sk) = 'N' or
026300061221     c                             LunDato_sk(sk) =  0
026400061221      * ?-----> Errore
026500061221     C                   eval      tipo_error = 2
026600061221     c                   eval      Campo_errato = 'N.Fattura'
026700061221     C                   exsr      Field_Error
026800061221     c                   Else
026900061221     c                   eval      Num_FATT = atoll(%TRIM(Dato_sk(sk)))
027000061221     c                   End
027100130718      **
027200130718      * ? Nel 6° campo : Peso Tassato                (Obbligatorio)  Valore
027300130718      **                                                             valore
027400130718     C                   z-add     colonna6      sk
027500130718     c                   If        DatoNum_sk(sk) = 'N' or
027600130718     c                             LunDato_sk(sk) =  0
027700130718      * ?-----> Errore
027800130718     c                   eval      tipo_error = 2
027900130718     c                   eval      Campo_errato = 'Peso_T.'
028000130718     c                   exsr      Field_Error
028100130718     c                   Else
028200130718     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
028300130718     c                   eval      PesoTass = atof(%TRIM(Dato_sk(sk)))
028400130718     c                   End
028500130718      **
028600130718      * ? Nel 7° campo : Volume                      (Obbligatorio)  Valore
028700130718      **                                                             valore
028800130718     C                   z-add     colonna7      sk
028900130718     c                   If        DatoNum_sk(sk) = 'N' or
029000130718     c                             LunDato_sk(sk) =  0
029100130718      * ?-----> Errore
029200130718     c                   eval      tipo_error = 2
029300130718     c                   eval      Campo_errato = 'Volume '
029400130718     c                   exsr      Field_Error
029500130718     c                   Else
029600130718     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
029700130718     c                   eval      Volume   = atof(%TRIM(Dato_sk(sk)))
029800130718     c                   End
029900060726      **
030000060726     c     End_Check     tag
030100060726      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
030200060726      **
030300060726     c                   Endsr
030400060726      * ?____________________________________________________________       */
030500060726      *?      Campi in errore
030600060726      * ?____________________________________________________________       */
030700060726     c     Field_Error   Begsr
030800060726      **
030900060726     C                   eval      se_errore   = 'S'
031000060726      * msg errore
031100060726     c                   If        Msg_Err = *blank
031200060726     c
031300060726     c                   Select
031400060726     c
031500060726      * msg errore : Obbligatorio
031600060726     c                   When      Tipo_Error = 1
031700060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
031800060726     C                             ' >> Obblig.- Manca Dato !!'
031900060726     c
032000060726      * msg errore : Obbligatorio / Numerico
032100060726     c                   When      Tipo_Error = 2
032200060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
032300060726     C                             ' >> Obblig.Mancante o NON Numerico !!'
032400060726     c
032500060726      * msg errore : Numerico
032600060726     c                   When      Tipo_Error = 3
032700060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
032800060726     C                             ' >> NON Numerico  !!'
032900060726     c                   EndSL
033000060726     c
033100060726     c                   End
033200060726      **
033300060726     c                   Endsr
033400060726      * ?____________________________________________________________       */
033500060726      *?      Suddivide i campi della riga con carattere divisorio (;)
033600060726      * ?____________________________________________________________       */
033700060726     c     Split_Rec     Begsr
033800060726      **
033900060726     c                   clear                   Nr_campi
034000060726     c                   clear                   Campi_Record
034100060726     c                   clear                   Campi_Lunghi
034200060726     c                   clear                   Campi_Numerici
034300060726     c                   clear                   Campi_Decimali
034400060726     c                   movel     ';'           Separa_campi
034500060727     c                   movel     ','           Decimal_Separ
034600060726      *
034700060726     c                   call      'FIEU00R'
034800060726      * ?     Input
034900060726     c                   parm                    Separa_campi
035000060726     c                   parm                    Decimal_Separ
035100060726     c                   parm                    dati
035200060726      * ?     Output  su schiere di 100 elementi
035300060726     c                   parm                    Nr_campi
035400060726     c                   parm                    Campi_Record                   * i dati
035500060726     c                   parm                    Campi_Lunghi                   * lunghezza dati
035600060726     c                   parm                    Campi_Numerici                 * se numerici
035700060726     c                   parm                    Campi_Decimali                 * quanti decimali
035800060726      **
035900060726     c                   Endsr
036000060726      * ?_______________________________________________________________    */
036100130717     C*? ESEGUO SCRITTURA del File TNEFR00F
036200060726      * ?_______________________________________________________________    */
036300060726     C     WRT_TNEFR     BEGSR
036400060726      *
036500060726     c                   clear                   tnEFR000
036600060726      *
036700061221     c                   eval      EFRNTW = Partner
036800061221     c                   eval      EFRGRP = '001'
036900061221     c                   eval      EFRCAU = Tipo_Doc
037000130718     c                   eval      EFRRFP = Num_Sped
037100061221      *
037200060726     c                   move      *all'0'       EFRNFT
037300061221      *
037400061221     c                   if        Num_Fatt > 0
037500061221     c                   move      Num_Fatt      EFRNFT
037600060726     c                   end
037700060726      *
037800060726      * cambia in tutti 9 il nr.fattura se non era presente
037900060726     c                   if        EFRNFT = *all'0'
038000060726     c                   move      *all'9'       EFRNFT
038100060726     c                   end
038200060726      *
038300061221     c                   if        Dat_Fatt > 0
038400061221     c                   z-add     Dat_Fatt      EFRDFT
038500061221     c                   end
038600060726      *
038700060726      * se è un credito va inserito in negativo
038800061221     c*****              move      'C'           EFRTPD
038900130718     c*****              move      'D'           EFRTPD
039000130718      *
039100061221     c                   z-add     Importo       EFRIMP
039200060726     c                   move      'F'           EFRTPD
039300130718      *
039400060726      *  imposta UDATE
039500060726     C                   move      *date         G02DAT
039600060726     C                   MOVE      *ZEROS        G02INV
039700060726     C                   MOVE      *BLANKS       G02ERR
039800060726     C                   CALL      'XSRDA8'
039900060726     C                   PARM                    WLBDAT
040000060726     C                   Z-ADD     G02INV        efrDTR
040100130718     c                   z-add     PesoTass      EFRPES
040200130718     c                   z-add     Volume        EFRVOL
040300060726      *
040400060726     c                   write     tnEFR000
040500060726      *
040600060726     c                   Endsr
040700060726      * ?_______________________________________________________________    */
040800060726     C     *INZSR        BEGSR
040900060726      * ?_______________________________________________________________    */
041000060726      *
041100060726     c     *ENTRY        PLIST
041200061222     c                   parm                    partner           3
041300061222     C                   parm                    colonna1          3 0
041400061222     C                   parm                    colonna2          3 0
041500061222     C                   parm                    colonna3          3 0
041600061222     C                   parm                    colonna4          3 0
041700061222     C                   parm                    colonna5          3 0
041800130718     C                   parm                    colonna6          3 0
041900130718     C                   parm                    colonna7          3 0
042000061222     C                   parm                    esito             1
042100060726      *
042200060726      *  imposta UDATE
042300060726     C                   move      *date         G02DAT
042400060726     C                   MOVE      *ZEROS        G02INV
042500060726     C                   MOVE      *BLANKS       G02ERR
042600060726     C                   CALL      'XSRDA8'
042700060726     C                   PARM                    WLBDAT
042800060726     C                   Z-ADD     G02INV        data_oggi         8 0
042900060726      * Recupero data e ora
043000060726     C                   TIME                    W0140            14 0
043100061221     C                   MOVEl     W0140         ore               6 0
043200060726      * UDATE IN GGMMAAAA
043300060726     C                   MOVE      W0140         WDTGIO
043400060726      * UDATE IN AAAAMMGG
043500060726     C     *eur          MOVEL     WDTGIO        DATA_eur
043600060726     C     *iso          MOVEL     DATA_eur      dateu
043700060726     c                   seton                                        66
043800060726      *
043900060726     c                   Endsr
044000060911      * ?_______________________________________________________________    */
