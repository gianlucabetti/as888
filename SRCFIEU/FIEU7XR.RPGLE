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
002700060727     FTNcfp01L  iF   E           k DISK
002800130718     Ftivin00R  UF   E             DISK    rename(TIVIN000:INPUT)
002900060727      *
003000130717     FTNEFR00F  O  a E             DISK
003100060727      * ?================================================================== */
003200040901     d stamp           ds
003300040901     d  lin                  367    368b 0
003400061221      *
003500061221     D                 DS
003600061221     D  Anno                   1      2  0
003700061221     D  Linea                  3      5  0
003800061221     D  Serie                  6      7  0
003900061221     D  Spediz                 8     14  0
004000061221     D Sped_DS                 1     14  0
004100040901      *
004200030624     D WLBDAT          DS
004300030624     D  G02DAT                 1      8  0
004400030624     D  G02INV                 9     16  0
004500030624     D  G02ERR                17     17
004600030624     D  G02TGI                18     22  0
004700060726      *
004800060726     D WDTGIO          S              8  0
004900060726     D DATEU           S              8  0
005000060726     D DATamg          S              8  0
005100060726     D DATA_eur        S               D   DATFMT(*eur)
005200060726     D DATA_amg        S               D   DATFMT(*iso)
005300060726      *-------------------
005400060726     D Tipo_error      S              1  0
005500060726     D Campo_errato    S             10
005600060726      * ?================================================================== */
005700060726     C*? Ds Decodifica dei campi
005800060726      * ?================================================================== */
005900061221      * ?   * Campi da tradurre con interi
006000061221     D atoll           PR            20I 0
006100061221     D                                     EXTPROC('atoll')
006200061221     D  Char                           *
006300061221     D                                     VALUE
006400061221     D                                     OPTIONS(*STRING)
006500060726      * ?   * Campi da tradurre con interi
006600060726     D atoi            PR            10I 0
006700060726     D                                     EXTPROC('atoi')
006800060726     D  Char                           *
006900060726     D                                     VALUE
007000060726     D                                     OPTIONS(*STRING)
007100060726      * ?   * Campi da tradurre con decimali
007200060726     D atof            PR             8F
007300060726     D                                     EXTPROC('atof')
007400060726     D  Char                           *
007500060726     D                                     VALUE
007600060726     D                                     OPTIONS(*STRING)
007700060726      * ?   *--------------------------------------------------------------*
007800060726      * ?   ( Descrizione Campi x scomposizione FLAT record )
007900060726      * ?   *--------------------------------------------------------------*
008000060726      *
008100060726      * ?   * Campi x decodifica * (INPUT  del Record)
008200060726     D  Dati           s           1500
008300060726     D Separa_campi    s              1
008400060726     D Decimal_Separ   s              1
008500060726      * ?   * Campi decodificati * (OUTPUT del Record)
008600060726      *       Schiere di Output / Campi di Output
008700060726     D  Alfa40         s             40
008800060726     d  Sk             s              3u 0
008900060726     D    NR_campi     s              3u 0
009000060726     D  Campi_Record   ds
009100060726     D    Dato_sk                    40    DIM(100)
009200060726     D  Campi_Lunghi   ds
009300060726     D    LunDato_sk                  3u 0 DIM(100)
009400060726     D Campi_Numerici  ds
009500060726     D    DatoNum_sk                  1    DIM(100)
009600060726     D Campi_Decimali  ds
009700060726     D    Decimal_sk                  3u 0 DIM(100)
009800060726      * ?   *--------------------------------------------------------------*
009900060726     D  position       s              3  0 INZ(0)
010000060726
010100060726     D  se_errore      s              1    inz(' ')
010200060726     D  Msg_Err        s            132    inz(' ')
010300060726      * ?* ------------------------------------------------------ *
010400060726     D Digits          C                   '0123456789'
010500060726      * ?================================================================== */
010600060726      *   Ciclo principale
010700060726      * ?================================================================== */
010800060726      * per controllare se almeno un record è stato importato sul VAB
010900060726     c                   clear                   Almeno_Uno        1
011000060726     c                   clear                   Esito             1
011100060726
011200060726      * Continua a leggere dal secondo record.
011300060726      *   Il primo è la testata già controllata dalla Check_trasm.
011400130718     c                   read      TIVIN00R
011500130718     c                   dow       not %eof(TIVIN00R)
011600060726
011700130719     c                   if        vinflg =' '
011800130719
011900060726      ** Controlli formali sui campi
012000060726     c                   clear                   se_errore
012100060726
012200060726      ** Decodifica record a campi variabili
012300060726     c                   exsr      Decod_Record
012400060726      *
012500060726      **  Se presente un errore nel record emette una segnalazione msg
012600060726     c                   if        se_errore ='S'
012700060726     c                   eval      esito  = '1'
012800060726     c                   else
012900130715      **
013000060726     c                   exsr      WRT_TNEFR
013100061221
013200061221     c                   endIF
013300060726
013400060726      *  Problemi nella decodifica dei campi VAB/VAT
013500060726     c                   if        se_errore ='S'
013600060726     c                   eval      esito  = '1'
013700130719     c                   movel     Msg_Err       vinMSG
013800130718     c                   move      '2'           vinFLG
013900060726     c                   else
014000060726      *  Record OK
014100130718     c                   move      '1'           vinFLG
014200060726     c                   move      'S'           Almeno_Uno
014300130718     c                   end
014400130718
014500130718     c                   update    INPUT
014600130719     c                   end
014700130719
014800130718     c                   read      TIVIN00R
014900130719
015000060726     C                   ENDdo
015100060726      **
015200060726     c                   SETON                                        LR
015300060726      * ?______________________________________________________________     */
015400060726      *?    Decodifica record a campi variabili
015500060726      * ?______________________________________________________________     */
015600060726     c     Decod_Record  Begsr
015700060915
015800060726      * ? Sposta il record a tracciato libero su un campo di lavoro       =
015900130718     c                   eval      dati = %trimr(vindta) + ';'
016000100920     c********           clear                   lunga             5 0
016100130718     c********           eval      lunga = %len(%trim(vindta))
016200100920     c********           eval      %subst(dati:lunga+1:1) = ';'
016300061221      **
016400060726     c                   exsr      Split_Rec
016500060726     c                   exsr      Rec_Dettaglio
016600060915      **
016700060726     c                   Endsr
016800060726      * ?_______________________________________________________________    */
016900060726      *?      Riga Bolla Import da controllare e tradurre
017000060726      * ?_______________________________________________________________    */
017100060726     c     Rec_Dettaglio Begsr
017200061221      **
017300061221     c                   clear                   Msg_Err
017400060726      **
017500060726      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
017600061221     c                   clear                   Tipo_DOC          3
017700061221     c                   clear                   Num_FATT         11 0
017800061221     c                   clear                   Dat_FATT          8 0
017900061221     c                   clear                   Num_Sped         14
018000130718     c                   clear                   Riferimento      14
018100061221     c                   clear                   Importo           7 2
018200130718     c                   clear                   PesoTass          9 3
018300130718     c                   clear                   Volume            9 6
018400060726      **
018500130718      * ? Nel 1° campo : Importo                     (Obbligatorio)  Importo
018600061221      **                                                             Importo
018700130719     C                   if        colonna1 >0
018800061222     C                   z-add     colonna1      sk
018900061221     c                   If        DatoNum_sk(sk) = 'N' or
019000061221     c                             LunDato_sk(sk) =  0
019100061221      * ?-----> Errore
019200061221     c                   eval      tipo_error = 2
019300061221     c                   eval      Campo_errato = 'Importo'
019400061221     c                   exsr      Field_Error
019500130719      * causa testata del file scritta in un certo modo
019600130719      * sono costretto a uscire x non far spaccare il programma
019700130719     c                   goto      End_Check
019800061221     c                   Else
019900061221     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
020000061221     c                   eval      Importo  = atof(%TRIM(Dato_sk(sk)))
020100061221     c                   End
020200130719     c                   End
020300061222      **
020400130718      * ? Nel  2° campo : Numero Spedizione Bart     (Obbligatorio)  Num Spediz.Bart
020500061222      **                                                             Num Spediz.Bart
020600130719     C                   if        colonna2 >0
020700061222     C                   z-add     colonna2      sk
020800061222     c                   If        LunDato_sk(sk) =  0
020900061222      * ?-----> Errore
021000061222     c                   eval      tipo_error = 1
021100061222     c                   eval      Campo_errato = 'N.Spediz.'
021200061222     c                   exsr      Field_Error
021300061222     c                   Else
021400130718     c                   if        LunDato_sk(sk)>14
021500130718     c                   eval      Num_Sped   =  %subst(Dato_sk(sk):
021600130718     c                               LunDato_sk(sk)-13:LunDato_sk(sk))
021700130718     c                   else
021800130718     c                   eval      Num_Sped   = %subst(Dato_sk(sk):1:+
021900130718     c                                              LunDato_sk(sk))
022000130718     c                   End
022100061222     c                   End
022200130719     c                   End
022300060726      **
022400130718      * ? Nel  3° campo : Numero Spedizione Bart  Alternativo        Num Spediz.Bart
022500130718      **                                                             Num Spediz.Bart
022600130719     C                   if        colonna3 >0
022700130718     c                   if        %Len(%Trim(Num_Sped)) < 14
022800130718     C                   z-add     colonna3      sk
022900130718     c                   If        LunDato_sk(sk) =  0
023000130718      * ?-----> Errore
023100130718     c                   eval      tipo_error = 1
023200130718     c                   eval      Campo_errato = 'Rif-Alter'
023300130718     c                   exsr      Field_Error
023400130718     c                   Else
023500130718     c                   if        LunDato_sk(sk)>14
023600130718     c                   eval      Num_Sped   =  %subst(Dato_sk(sk):
023700130718     c                               LunDato_sk(sk)-13:LunDato_sk(sk))
023800130718     c                   else
023900130718     c                   eval      Num_Sped   = %subst(Dato_sk(sk):1:+
024000130718     c                                              LunDato_sk(sk))
024100130718     c                   End
024200130718     c                   End
024300130718     c                   End
024400130719     c                   End
024500130718      *
024600100318      **  Non essendo + gestito lo imposto fisso
024700130719     C                   if        colonna4 >0
024800130718     c                   eval      Tipo_Doc   = 'FAT'
024900061222      **
025000130718      * ? Nel  4° campo : Data Documento             (Obbligatorio)  Data   Documento
025100061222      **                                                             Data   Documento
025200061222     C                   z-add     colonna4      sk
025300061222     c                   If        DatoNum_sk(sk) = 'N' or
025400061222     c                             LunDato_sk(sk) =  0
025500061222      * ?-----> Errore
025600061222     C                   eval      tipo_error = 2
025700061222     c                   eval      Campo_errato = 'Data Fatt'
025800061222     C                   exsr      Field_Error
025900061222     c                   Else
026000061222     c                   eval      Dat_FATT = atoi(%TRIM(Dato_sk(sk)))
026100130718     C                   z-add     Dat_FATT      campo4_0          4 0
026200061222     c                   End
026300130718      **-
026400130718      **- Se la data è in GG/MM/AAAA  la devo girare
026500130718     c                   if        campo4_0 > 2000
026600130718     C                   move      Dat_FATT      G02DAT
026700130718     C                   MOVE      *ZEROS        G02INV
026800130718     C                   MOVE      ' '           G02ERR
026900130718     C                   CALL      'XSRDA8'
027000130718     C                   PARM                    WLBDAT
027100130718     C                   Z-ADD     G02INV        Dat_FATT
027200130718     c                   end
027300130719     c                   end
027400061221      **
027500130718      * ? Nel  5° campo : Num Documento              (Obbligatorio)  Numero Documento
027600061221      **                                                             Numero Documento
027700130719     C                   if        colonna5 >0
027800061222     C                   z-add     colonna5      sk
027900061221     c                   If        DatoNum_sk(sk) = 'N' or
028000061221     c                             LunDato_sk(sk) =  0
028100061221      * ?-----> Errore
028200061221     C                   eval      tipo_error = 2
028300061221     c                   eval      Campo_errato = 'N.Fattura'
028400061221     C                   exsr      Field_Error
028500061221     c                   Else
028600061221     c                   eval      Num_FATT = atoll(%TRIM(Dato_sk(sk)))
028700061221     c                   End
028800130719     c                   end
028900130718      **
029000130718      * ? Nel 6° campo : Peso Tassato                (Obbligatorio)  Valore
029100130718      **                                                             valore
029200130719     C                   if        colonna6 >0
029300130718     C                   z-add     colonna6      sk
029400130718     c                   If        DatoNum_sk(sk) = 'N' or
029500130718     c                             LunDato_sk(sk) =  0
029600130718      * ?-----> Errore
029700130718     c                   eval      tipo_error = 2
029800130718     c                   eval      Campo_errato = 'Peso_T.'
029900130718     c                   exsr      Field_Error
030000130718     c                   Else
030100130718     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
030200130718     c                   eval      PesoTass = atof(%TRIM(Dato_sk(sk)))
030300130718     c                   End
030400130719     c                   end
030500130718      **
030600130718      * ? Nel 7° campo : Volume                      (Obbligatorio)  Valore
030700130718      **                                                             valore
030800130719     C                   if        colonna7 >0
030900130718     C                   z-add     colonna7      sk
031000130718     c                   If        DatoNum_sk(sk) = 'N' or
031100130718     c                             LunDato_sk(sk) =  0
031200130718      * ?-----> Errore
031300130718     c                   eval      tipo_error = 2
031400130718     c                   eval      Campo_errato = 'Volume '
031500130718     c                   exsr      Field_Error
031600130718     c                   Else
031700130718     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
031800130718     c                   eval      Volume   = atof(%TRIM(Dato_sk(sk)))
031900130718     c                   End
032000130719     c                   end
032100060726      **
032200060726     c     End_Check     tag
032300060726      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
032400060726      **
032500060726     c                   Endsr
032600060726      * ?____________________________________________________________       */
032700060726      *?      Campi in errore
032800060726      * ?____________________________________________________________       */
032900060726     c     Field_Error   Begsr
033000060726      **
033100060726     C                   eval      se_errore   = 'S'
033200060726      * msg errore
033300060726     c                   If        Msg_Err = *blank
033400060726     c
033500060726     c                   Select
033600060726     c
033700060726      * msg errore : Obbligatorio
033800060726     c                   When      Tipo_Error = 1
033900060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
034000060726     C                             ' >> Obblig.- Manca Dato !!'
034100060726     c
034200060726      * msg errore : Obbligatorio / Numerico
034300060726     c                   When      Tipo_Error = 2
034400060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
034500060726     C                             ' >> Obblig.Mancante o NON Numerico !!'
034600060726     c
034700060726      * msg errore : Numerico
034800060726     c                   When      Tipo_Error = 3
034900060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
035000060726     C                             ' >> NON Numerico  !!'
035100060726     c                   EndSL
035200060726     c
035300060726     c                   End
035400060726      **
035500060726     c                   Endsr
035600060726      * ?____________________________________________________________       */
035700060726      *?      Suddivide i campi della riga con carattere divisorio (;)
035800060726      * ?____________________________________________________________       */
035900060726     c     Split_Rec     Begsr
036000060726      **
036100060726     c                   clear                   Nr_campi
036200060726     c                   clear                   Campi_Record
036300060726     c                   clear                   Campi_Lunghi
036400060726     c                   clear                   Campi_Numerici
036500060726     c                   clear                   Campi_Decimali
036600060726     c                   movel     ';'           Separa_campi
036700060727     c                   movel     ','           Decimal_Separ
036800060726      *
036900060726     c                   call      'FIEU00R'
037000060726      * ?     Input
037100060726     c                   parm                    Separa_campi
037200060726     c                   parm                    Decimal_Separ
037300060726     c                   parm                    dati
037400060726      * ?     Output  su schiere di 100 elementi
037500060726     c                   parm                    Nr_campi
037600060726     c                   parm                    Campi_Record                   * i dati
037700060726     c                   parm                    Campi_Lunghi                   * lunghezza dati
037800060726     c                   parm                    Campi_Numerici                 * se numerici
037900060726     c                   parm                    Campi_Decimali                 * quanti decimali
038000060726      **
038100060726     c                   Endsr
038200060726      * ?_______________________________________________________________    */
038300130717     C*? ESEGUO SCRITTURA del File TNEFR00F
038400060726      * ?_______________________________________________________________    */
038500060726     C     WRT_TNEFR     BEGSR
038600060726      *
038700060726     c                   clear                   tnEFR000
038800060726      *
038900061221     c                   eval      EFRNTW = Partner
039000061221     c                   eval      EFRGRP = '001'
039100061221     c                   eval      EFRCAU = Tipo_Doc
039200130718     c                   eval      EFRRFP = Num_Sped
039300061221      *
039400060726     c                   move      *all'0'       EFRNFT
039500061221      *
039600061221     c                   if        Num_Fatt > 0
039700061221     c                   move      Num_Fatt      EFRNFT
039800060726     c                   end
039900060726      *
040000060726      * cambia in tutti 9 il nr.fattura se non era presente
040100060726     c                   if        EFRNFT = *all'0'
040200060726     c                   move      *all'9'       EFRNFT
040300060726     c                   end
040400060726      *
040500061221     c                   if        Dat_Fatt > 0
040600061221     c                   z-add     Dat_Fatt      EFRDFT
040700061221     c                   end
040800060726      *
040900060726      * se è un credito va inserito in negativo
041000061221     c*****              move      'C'           EFRTPD
041100130718     c*****              move      'D'           EFRTPD
041200130718      *
041300061221     c                   z-add     Importo       EFRIMP
041400060726     c                   move      'F'           EFRTPD
041500130718      *
041600060726      *  imposta UDATE
041700060726     C                   move      *date         G02DAT
041800060726     C                   MOVE      *ZEROS        G02INV
041900060726     C                   MOVE      *BLANKS       G02ERR
042000060726     C                   CALL      'XSRDA8'
042100060726     C                   PARM                    WLBDAT
042200060726     C                   Z-ADD     G02INV        efrDTR
042300130718     c                   z-add     PesoTass      EFRPES
042400130718     c                   z-add     Volume        EFRVOL
042500060726      *
042600060726     c                   write     tnEFR000
042700060726      *
042800060726     c                   Endsr
042900060726      * ?_______________________________________________________________    */
043000060726     C     *INZSR        BEGSR
043100060726      * ?_______________________________________________________________    */
043200060726      *
043300060726     c     *ENTRY        PLIST
043400061222     c                   parm                    partner           3
043500061222     C                   parm                    colonna1          3 0
043600061222     C                   parm                    colonna2          3 0
043700061222     C                   parm                    colonna3          3 0
043800061222     C                   parm                    colonna4          3 0
043900061222     C                   parm                    colonna5          3 0
044000130718     C                   parm                    colonna6          3 0
044100130718     C                   parm                    colonna7          3 0
044200061222     C                   parm                    esito             1
044300060726      *
044400060726      *  imposta UDATE
044500060726     C                   move      *date         G02DAT
044600060726     C                   MOVE      *ZEROS        G02INV
044700060726     C                   MOVE      *BLANKS       G02ERR
044800060726     C                   CALL      'XSRDA8'
044900060726     C                   PARM                    WLBDAT
045000060726     C                   Z-ADD     G02INV        data_oggi         8 0
045100060726      * Recupero data e ora
045200060726     C                   TIME                    W0140            14 0
045300061221     C                   MOVEl     W0140         ore               6 0
045400060726      * UDATE IN GGMMAAAA
045500060726     C                   MOVE      W0140         WDTGIO
045600060726      * UDATE IN AAAAMMGG
045700060726     C     *eur          MOVEL     WDTGIO        DATA_eur
045800060726     C     *iso          MOVEL     DATA_eur      dateu
045900060726     c                   seton                                        66
046000060726      *
046100060726     c                   Endsr
046200060911      * ?_______________________________________________________________    */
