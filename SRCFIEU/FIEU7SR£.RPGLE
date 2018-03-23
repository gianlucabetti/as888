000100061222 * ?  ***************************************************************************
000200081023      *  Da UPLOAD                                                              *
000300081024      *  Importa da E-mail i dati di fatturazione di Sernam x ctl.fatt.ptn.     *
000400061222 * ?  *   Nuovo traduttore dati Sernam da file CSV
000500061222 * ?  ***************************************************************************
000600081023      * ?Il programma x funzionare ha bisogno di un file FLAT : TIVIN000
000700061222 * ?  *   che deve contenere i dati di un file (.csv) che utilizzi separatore
000800061222 * ?  *   fra i campi (;)
000900061222      * ?Basandosi sul file excel ricevuto da Sernam nelle colonne:
001000061222      *     G =>(7)  = Importo
001100061222      *     N =>(14) = Num.Sped.Bartolini
001200061222      *     O =>(15) = Tipo Documento FAC(Fattura Cliente)
001300061222      *     P =>(16) = Data Documento
001400061222      *     Q =>(17) = Num. Documento
001500061222      * ?Occorre fare attenzione se dovessero cambiare le posizioni alle colonne
001600061222      * ? dell'Excel.
001700061222 * ?  ***************************************************************************
001800060726     H DFTACTGRP(*NO) ACTGRP(*CALLER)
001900060726     H BNDDIR('QC2LE')
002000060727     H DECEDIT('0,') DATEDIT(*DMY.)
002100060726 * ?  ***************************************************************************
002200061221      *  Da UPLOAD                                                              *
002300081024      *                  --> CSV   Scarico Dati  come TXT                       *
002400060726      * Il pgm crea:                                                            *
002500061221      *             TNEFR00F x SERNAM                                           *
002600060727      * ?================================================================== */
002700081023     Ftivin00r  uF   E             DISK    usropn
002800081024      **?**************
002900030623     FTNEFR00F  O  a E             DISK
003000060727      * ?================================================================== */
003100040901     d stamp           ds
003200040901     d  lin                  367    368b 0
003300061221      *
003400061221     D                 DS
003500061221     D  Anno                   1      2  0
003600061221     D  Linea                  3      5  0
003700061221     D  Serie                  6      7  0
003800061221     D  Spediz                 8     14  0
003900061221     D Sped_DS                 1     14  0
004000040901      *
004100081023     D  data_Oggi      s              8  0 INZ(0)
004200081023     D  day            s              8  0 INZ(0)
004300081023     D  ore            s              6  0 INZ(0)
004400081023     D  w0140          s             14  0 INZ(0)
004500081023      *
004600030624     D WLBDAT          DS
004700030624     D  G02DAT                 1      8  0
004800030624     D  G02INV                 9     16  0
004900030624     D  G02ERR                17     17
005000030624     D  G02TGI                18     22  0
005100060726      *
005200060726     D WDTGIO          S              8  0
005300060726     D DATEU           S              8  0
005400060726     D DATamg          S              8  0
005500060726     D DATA_eur        S               D   DATFMT(*eur)
005600060726     D DATA_amg        S               D   DATFMT(*iso)
005700060726      *-------------------
005800060726     D Tipo_error      S              1  0
005900060726     D Campo_errato    S             10
006000060726      * ?================================================================== */
006100060726     C*? Ds Decodifica dei campi
006200060726      * ?================================================================== */
006300061221      * ?   * Campi da tradurre con interi
006400061221     D atoll           PR            20I 0
006500061221     D                                     EXTPROC('atoll')
006600061221     D  Char                           *
006700061221     D                                     VALUE
006800061221     D                                     OPTIONS(*STRING)
006900060726      * ?   * Campi da tradurre con interi
007000060726     D atoi            PR            10I 0
007100060726     D                                     EXTPROC('atoi')
007200060726     D  Char                           *
007300060726     D                                     VALUE
007400060726     D                                     OPTIONS(*STRING)
007500060726      * ?   * Campi da tradurre con decimali
007600060726     D atof            PR             8F
007700060726     D                                     EXTPROC('atof')
007800060726     D  Char                           *
007900060726     D                                     VALUE
008000060726     D                                     OPTIONS(*STRING)
008100060726      * ?   *--------------------------------------------------------------*
008200060726      * ?   ( Descrizione Campi x scomposizione FLAT record )
008300060726      * ?   *--------------------------------------------------------------*
008400060726      *
008500060726      * ?   * Campi x decodifica * (INPUT  del Record)
008600060726     D  Dati           s           1500
008700060726     D Separa_campi    s              1
008800060726     D Decimal_Separ   s              1
008900060726      * ?   * Campi decodificati * (OUTPUT del Record)
009000060726      *       Schiere di Output / Campi di Output
009100060726     D  Alfa40         s             40
009200060726     d  Sk             s              3u 0
009300060726     D    NR_campi     s              3u 0
009400060726     D  Campi_Record   ds
009500060726     D    Dato_sk                    40    DIM(100)
009600060726     D  Campi_Lunghi   ds
009700060726     D    LunDato_sk                  3u 0 DIM(100)
009800060726     D Campi_Numerici  ds
009900060726     D    DatoNum_sk                  1    DIM(100)
010000060726     D Campi_Decimali  ds
010100060726     D    Decimal_sk                  3u 0 DIM(100)
010200060726      * ?   *--------------------------------------------------------------*
010300060726     D  position       s              3  0 INZ(0)
010400060726
010500060726     D  se_errore      s              1    inz(' ')
010600060726     D  Msg_Err        s            132    inz(' ')
010700081023     D  Msg_Vin_80     s             80    inz(' ')
010800060726      * ?* ------------------------------------------------------ *
010900060726     D Digits          C                   '0123456789'
011000060726      * ?================================================================== */
011100060726      *   Ciclo principale
011200060726      * ?================================================================== */
011300081024      * per controllare se almeno un record è stato importato
011400060726     c                   clear                   Almeno_Uno        1
011500060726     c                   clear                   Esito             1
011600081024     c                   clear                   err_bloccante     1
011700081023      **
011800081023      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
011900081023     C                   if        not %open(tivin00r)
012000081023     C                   open      tivin00r
012100081023     C                   endif
012200081023      * ?              /*---------------------- */
012300081024
012400081024     c     *start        setll     Tivin00r
012500081024     c                   read      Tivin00r
012600081024
012700081024     c                   dow       not %eof(Tivin00r)
012800081024
012900081024      * solo i record sflaggati da rielaborare
013000081024     c                   IF        vinFLG = *blank and vinDTA <> *blank
013100081024      *  Sempre Record OK
013200081024     C                   eval      vinFLG = '1'
013300081024     c                   eval      esito  = '0'
013400081024
013500081024      ** Controlli formali sui campi
013600081024     c                   clear                   se_errore
013700081024     C                   clear                   Msg_Vin_80
013800081024
013900081024      ** Decodifica record a campi variabili
014000081024      * ?              /*---------------------- */
014100081024     c                   exsr      Decod_Record
014200081024      * ?              /*---------------------- */
014300081024
014400081024      **  Se presente un errore nel record emette una segnalazione msg
014500081024     c                   if        se_errore ='S'
014600081024     C                   eval      vinMSG = Msg_Vin_80
014700081024     c                   else
014800081024      **
014900081024      *  con il record in canna scrive direttamente il TNEFR
015000081024     c                   if        Tipo_DOC = 'FAC'
015100081024      * ?              /*---------------------- */
015200081024     c                   exsr      WRT_TNEFR
015300081024      * ?              /*---------------------- */
015400081024     c                   else
015500081024      **
015600081024     c                   eval      se_errore = 'S'
015700081024     c                   eval      esito  = '1'
015800081024     C                   eval      Msg_Err = 'Tipo Documento non  -> FAC <- '
015900081024     c                   end
016000081024
016100081024     c                   endIF
016200081024
016300081024      *  x errore bloccante nella composizione del VAO
016400081024     c                   if        err_bloccante ='S'
016500081024     C                   eval      vinFLG = '2'
016600081024     c                   eval      esito  = '1'
016700081024     c                   end
016800081024
016900081024     c                   update    Tivin000
017000081024     c                   endIF
017100081024
017200081024     c                   read      Tivin00r
017300081024     C                   ENDdo
017400081023      * ?              /*---------------------- */
017500081023
017600081023     C                   if        %open(tivin00r)
017700081023     C                   close     tivin00r
017800081023     C                   endif
017900081023      *
018000081023      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
018100081023     c                   if        almeno_uno ='S' and esito ='1'
018200081023     C                   eval      esito ='0'
018300081023     C                   endif
018400081023      *
018500081023     c                   SETON                                        LR
018600060726      * ?______________________________________________________________     */
018700060726      *?    Decodifica record a campi variabili
018800060726      * ?______________________________________________________________     */
018900060726     c     Decod_Record  Begsr
019000060915
019100060726      * ? Sposta il record a tracciato libero su un campo di lavoro       =
019200081024     c                   movel(p)  VINDTA        dati
019300081024      * ?              /*---------------------- */
019400060726     c                   exsr      Split_Rec
019500081024      * ?              /*---------------------- */
019600060726     c                   exsr      Rec_Dettaglio
019700081024      * ?              /*---------------------- */
019800060915      **
019900060726     c                   Endsr
020000060726      * ?_______________________________________________________________    */
020100060726      *?      Riga Bolla Import da controllare e tradurre
020200060726      * ?_______________________________________________________________    */
020300060726     c     Rec_Dettaglio Begsr
020400061221      **
020500061221     c                   clear                   Msg_Err
020600060726      **
020700060726      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
020800061221     c                   clear                   Tipo_DOC          3
020900061221     c                   clear                   Num_FATT         11 0
021000061221     c                   clear                   Dat_FATT          8 0
021100061221     c                   clear                   Num_Sped         14
021200061221     c                   clear                   Importo           7 2
021300060726      **
021400081024      * ? Nel 7° campo : Importo                     (Obbligatorio)  Importo
021500061221      **                                                             Importo
021600061222     C                   z-add     colonna1      sk
021700061221     c                   If        DatoNum_sk(sk) = 'N' or
021800061221     c                             LunDato_sk(sk) =  0
021900061221      * ?-----> Errore
022000061221     c                   eval      tipo_error = 2
022100061221     c                   eval      Campo_errato = 'Importo'
022200061221     c                   exsr      Field_Error
022300061221     c                   Else
022400061221     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
022500061221     c                   eval      Importo  = atof(%TRIM(Dato_sk(sk)))
022600061221     c                   End
022700061222      **
022800061222      * ? Nel 14° campo : Numero Spedizione Bart     (Oblligatorio)  Num Spediz.Bart
022900061222      **                                                             Num Spediz.Bart
023000061222     C                   z-add     colonna2      sk
023100061222     c                   If        LunDato_sk(sk) =  0
023200061222      * ?-----> Errore
023300061222     c                   eval      tipo_error = 1
023400061222     c                   eval      Campo_errato = 'N.Spediz.'
023500061222     c                   exsr      Field_Error
023600061222     c                   Else
023700061222     c                   eval      Num_Sped   = %subst(Dato_sk(sk):1:+
023800061222     c                                              LunDato_sk(sk))
023900061222     c                   End
024000060726      **
024100061221      * ? Nel 15°campo : Tipo Documento                   (Obbl.)    Tipo Importo
024200060726      **                                                             Tipo Importo
024300061222     C                   z-add     colonna3      sk
024400060726     c                   If        LunDato_sk(sk) =  0
024500060726      * ?-----> Errore
024600060726     c                   eval      tipo_error = 1
024700061221     c                   eval      Campo_errato = 'Tipo_Doc'
024800060726     c                   exsr      Field_Error
024900060726     c                   Else
025000061221     c                   eval      Tipo_Doc   = %subst(Dato_sk(sk):1:+
025100060726     c                                              LunDato_sk(sk))
025200060726     c                   End
025300061222      **
025400061222      * ? Nel 16° campo : Data Documento             (Oblligatorio)  Data   Documento
025500061222      **                                                             Data   Documento
025600061222     C                   z-add     colonna4      sk
025700061222     c                   If        DatoNum_sk(sk) = 'N' or
025800061222     c                             LunDato_sk(sk) =  0
025900061222      * ?-----> Errore
026000061222     C                   eval      tipo_error = 2
026100061222     c                   eval      Campo_errato = 'Data Fatt'
026200061222     C                   exsr      Field_Error
026300061222     c                   Else
026400061222     c                   eval      Dat_FATT = atoi(%TRIM(Dato_sk(sk)))
026500061222     c                   End
026600061221      **
026700061221      * ? Nel 17° campo : Num Documento              (Oblligatorio)  Numero Documento
026800061221      **                                                             Numero Documento
026900061222     C                   z-add     colonna5      sk
027000061221     c                   If        DatoNum_sk(sk) = 'N' or
027100061221     c                             LunDato_sk(sk) =  0
027200061221      * ?-----> Errore
027300061221     C                   eval      tipo_error = 2
027400061221     c                   eval      Campo_errato = 'N.Fattura'
027500061221     C                   exsr      Field_Error
027600061221     c                   Else
027700061221     c                   eval      Num_FATT = atoll(%TRIM(Dato_sk(sk)))
027800061221     c                   End
027900060726      **
028000060726     c     End_Check     tag
028100060726      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
028200060726      **
028300060726     c                   Endsr
028400060726      * ?____________________________________________________________       */
028500060726      *?      Campi in errore
028600060726      * ?____________________________________________________________       */
028700060726     c     Field_Error   Begsr
028800060726      **
028900060726     C                   eval      se_errore   = 'S'
029000060726      * msg errore
029100060726     c                   If        Msg_Err = *blank
029200060726     c
029300060726     c                   Select
029400060726     c
029500060726      * msg errore : Obbligatorio
029600060726     c                   When      Tipo_Error = 1
029700060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
029800060726     C                             ' >> Obblig.- Manca Dato !!'
029900060726     c
030000060726      * msg errore : Obbligatorio / Numerico
030100060726     c                   When      Tipo_Error = 2
030200060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
030300060726     C                             ' >> Obblig.Mancante o NON Numerico !!'
030400060726     c
030500060726      * msg errore : Numerico
030600060726     c                   When      Tipo_Error = 3
030700060726     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
030800060726     C                             ' >> NON Numerico  !!'
030900060726     c                   EndSL
031000060726     c
031100060726     c                   End
031200060726      **
031300060726     c                   Endsr
031400060726      * ?____________________________________________________________       */
031500060726      *?      Suddivide i campi della riga con carattere divisorio (;)
031600060726      * ?____________________________________________________________       */
031700060726     c     Split_Rec     Begsr
031800060726      **
031900060726     c                   clear                   Nr_campi
032000060726     c                   clear                   Campi_Record
032100060726     c                   clear                   Campi_Lunghi
032200060726     c                   clear                   Campi_Numerici
032300060726     c                   clear                   Campi_Decimali
032400060726     c                   movel     ';'           Separa_campi
032500060727     c                   movel     ','           Decimal_Separ
032600060726      *
032700060726     c                   call      'FIEU00R'
032800060726      * ?     Input
032900060726     c                   parm                    Separa_campi
033000060726     c                   parm                    Decimal_Separ
033100060726     c                   parm                    dati
033200060726      * ?     Output  su schiere di 100 elementi
033300060726     c                   parm                    Nr_campi
033400060726     c                   parm                    Campi_Record                   * i dati
033500060726     c                   parm                    Campi_Lunghi                   * lunghezza dati
033600060726     c                   parm                    Campi_Numerici                 * se numerici
033700060726     c                   parm                    Campi_Decimali                 * quanti decimali
033800060726      **
033900060726     c                   Endsr
034000060726      * ?_______________________________________________________________    */
034100060726     C*? ESEGUO SCRITTURA del File TNEFR00F
034200060726      * ?_______________________________________________________________    */
034300060726     C     WRT_TNEFR     BEGSR
034400060726      *
034500060726     c                   clear                   tnEFR000
034600060726      *
034700061221     c                   eval      EFRNTW = Partner
034800061221     c                   eval      EFRGRP = '001'
034900061221     c                   eval      EFRCAU = Tipo_Doc
035000061221     c                   eval      EFRRIF = Num_Sped
035100061221      *
035200060726     c                   move      *all'0'       EFRNFT
035300061221      *
035400061221     c                   if        Num_Fatt > 0
035500061221     c                   move      Num_Fatt      EFRNFT
035600060726     c                   end
035700060726      *
035800060726      * cambia in tutti 9 il nr.fattura se non era presente
035900060726     c                   if        EFRNFT = *all'0'
036000060726     c                   move      *all'9'       EFRNFT
036100060726     c                   end
036200060726      *
036300061221     c                   if        Dat_Fatt > 0
036400061221     c                   z-add     Dat_Fatt      EFRDFT
036500061221     c                   end
036600060726      *
036700060726      * se è un credito va inserito in negativo
036800061221     c                   z-add     Importo       EFRIMP
036900060726     c                   move      'F'           EFRTPD
037000060726      *  imposta UDATE
037100060726     C                   move      *date         G02DAT
037200060726     C                   MOVE      *ZEROS        G02INV
037300060726     C                   MOVE      *BLANKS       G02ERR
037400060726     C                   CALL      'XSRDA8'
037500060726     C                   PARM                    WLBDAT
037600060726     C                   Z-ADD     G02INV        efrDTR
037700060726      *
037800060726     c                   write     tnEFR000
037900060726      *
038000060726     c                   Endsr
038100060726      * ?_______________________________________________________________    */
038200060726     C     *INZSR        BEGSR
038300060726      * ?_______________________________________________________________    */
038400060726      *
038500060726     c     *ENTRY        PLIST
038600061222     c                   parm                    partner           3
038700061222     C                   parm                    colonna1          3 0
038800061222     C                   parm                    colonna2          3 0
038900061222     C                   parm                    colonna3          3 0
039000061222     C                   parm                    colonna4          3 0
039100061222     C                   parm                    colonna5          3 0
039200061222     C                   parm                    esito             1
039300060726      *
039400060726      *  imposta UDATE
039500060726     C                   move      *date         G02DAT
039600060726     C                   MOVE      *ZEROS        G02INV
039700060726     C                   MOVE      *BLANKS       G02ERR
039800060726     C                   CALL      'XSRDA8'
039900060726     C                   PARM                    WLBDAT
040000060726     C                   Z-ADD     G02INV        data_oggi         8 0
040100060726      * Recupero data e ora
040200060726     C                   TIME                    W0140            14 0
040300061221     C                   MOVEl     W0140         ore               6 0
040400060726      * UDATE IN GGMMAAAA
040500060726     C                   MOVE      W0140         WDTGIO
040600060726      * UDATE IN AAAAMMGG
040700060726     C     *eur          MOVEL     WDTGIO        DATA_eur
040800060726     C     *iso          MOVEL     DATA_eur      dateu
040900060726     c                   seton                                        66
041000060726      *
041100060726     c                   Endsr
041200060911      * ?_______________________________________________________________    */
