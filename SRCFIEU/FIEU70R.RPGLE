000100060726     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060726     H BNDDIR('QC2LE')
000300060727     H DECEDIT('0,') DATEDIT(*DMY.)
000400081202      * ?====================================================================== */
000500060726 * ?  *   Nuovo traduttore Files Clearing BIC3 Parcel a 14
000600081202      * ?====================================================================== */
000700081202      *  Da Procedura CD --> Scarico Dati  Da un UNICO FILE                     *
000800081202      *                                                                         *
000900081202      *     contenente i 3 formati di traduzione:                               *
001000081202      *     1)  FILE_GENERALE                                                   *
001100081202      *     2)  FILE_VTG                                                        *
001200081202      *     3)  FILE_ISOLE                                                      *
001300081202      *                                                                         *
001400081202      * Il pgm crea:                                                            *
001500130717      *             TNEFR00F x DPD                                              *
001600081202      * ?====================================================================== */
001700081202     F****Tita434C  iF   E           k DISK
001800081202     F****Titas32C  iF   E           k DISK
001900060727     FTNcfp01L  iF   E           k DISK
002000081202     FWfDPD1PC  UF   E             DISK    rename(WFDPD1PC:WFDPDW)
002100060727     F                                     PREFIX(x)
002200060727      *
002300130717     FTNEFR00F  O  a E             DISK
002400040901     fFIEU70P   o    E             PRINTER OFLIND(*in66)
002500060727      * ?================================================================== */
002600040901     d stamp           ds
002700040901     d  lin                  367    368b 0
002800040901      *
002900030624     D WLBDAT          DS
003000030624     D  G02DAT                 1      8  0
003100030624     D  G02INV                 9     16  0
003200030624     D  G02ERR                17     17
003300030624     D  G02TGI                18     22  0
003400060726      *
003500060726     D WDTGIO          S              8  0
003600060726     D DATEU           S              8  0
003700060726     D DATamg          S              8  0
003800060726     D DATA_eur        S               D   DATFMT(*eur)
003900060726     D DATA_amg        S               D   DATFMT(*iso)
004000060726      *-------------------
004100081202     D Data_8          S              8  0
004200060726     D Lung_Parcel     S              3  0
004300060726     D Tipo_error      S              1  0
004400060726     D Campo_errato    S             10
004500060911      *
004600060911     D DsParcel        DS
004700060911     d  Parcel0                       1
004800060911     d  Depot3                        3
004900060911     d  NoveNove                      2
005000060911     d  NumParcel                     8
005100060911      *
005200060911     D Parcel11        S             11
005300060911     D Parcel14        S             14
005400060911      *
005500060911      * CALCOLO DEL CHCK DIGIT PER BARCODE DPD
005600060911     D                 DS
005700060911     D  §11                    1     11  0
005800060911     D                                     DIM(11)
005900060911     D  DPDCHD                12     12  0
006000060911     D  DPDBRC                 1     12  0
006100081202      *
006200081202     D Valore_x_Isola...
006300081202     D                 S              7s 4 inz(13,1613)
006400060726      * ?================================================================== */
006500060726     C*? Ds Decodifica dei campi
006600060726      * ?================================================================== */
006700060726     D TIDPC0DS      E DS                  EXTNAME(TIDPC00W)
006800060726      **
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
010700060726      * ?* ------------------------------------------------------ *
010800081202      *
010900060726     D Digits          C                   '0123456789'
011000060726      * ?================================================================== */
011100060726      *   Ciclo principale
011200060726      * ?================================================================== */
011300060726      * per controllare se almeno un record è stato importato sul VAB
011400081202     c                   clear                   traduttore        1 0
011500081202     c                   clear                   riga_generica     1
011600060726     c                   clear                   Almeno_Uno        1
011700060726     c                   clear                   Esito             1
011800060726
011900060726      * Continua a leggere dal secondo record.
012000060726      *   Il primo è la testata già controllata dalla Check_trasm.
012100081202     c                   read      WFDPD1PC
012200081202     c                   dow       not %eof(WFDPD1PC)
012300091005      * se record vuoto
012400091005     c                   If        xWFDPD1PC  = *blank
012500091005     c                   eval      riga_generica = 'S'
012600091005     c                   else
012700060726
012800060726      ** Controlli formali sui campi
012900060726     c                   clear                   se_errore
013000081202     c                   eval       riga_generica = 'S'
013100081202      * >>>>>>>>>>>>>
013200081202      * Tipi di traduzione
013300081202     c                   If        %subst(xWFDPD1PC:1:13) = 'FILE_GENERALE'
013400081202     c                   z-add     1             traduttore
013500081202      *
013600081202     c                   elseIf    %subst(xWFDPD1PC:1:8)  = 'FILE_VTG'
013700081202     c                   z-add     2             traduttore
013800081202      *
013900081202     c                   elseIf    %subst(xWFDPD1PC:1:10) = 'FILE_ISOLE'
014000081202     c                   z-add     3             traduttore
014100081202      *
014200081202      * Fine tipo di traduzione
014300081202     c                   elseIf    %subst(xWFDPD1PC:1:8)  = 'END_FILE'
014400081202     c                   clear                   traduttore        1 0
014500081202      *  Riga da importare
014600081202     c                   else
014700081202     c                   eval       riga_generica = 'N'
014800081202     c                   end
014900091005     c
015000091005     c                   end
015100170105     *** FISSO VTG
015200170105     c                   z-add     2             traduttore
015300081202      * >>>>>>>>>>>>>
015400081202      *  Per record generico  deve essere non tradotto ma solo cancellato
015500081202     c                   If        riga_generica = 'S'
015600081202     c                   delete    WFDPDW
015700081202     c                   else
015800081202      *      >>>>>>>>>>>>>
015900081202      ** Decodifica record a campi variabili
016000081202      *      >>>>>>>>>>>>>
016100081202     c                   If        traduttore = 1
016200081202     c                   exsr      Decod_Record1
016300081202     c
016400081202     c                   elseIf    traduttore = 2
016500081202     c                   exsr      Decod_Record2
016600081202     c
016700081202     c                   elseIf    traduttore = 3
016800081202     c                   exsr      Decod_Record3
016900081202     c                   endIf
017000081202      *  Record OK
017100060726      **  Se presente un errore nel record emette una segnalazione msg
017200060726     c                   if        se_errore ='S'
017300060726     c                   eval      esito  = '1'
017400060726     c                   else
017500060726      *  con il record in canna scrive direttamente il TNEFR
017600060726     c                   exsr      WRT_TNEFR
017700060726
017800060726      *  Problemi nella decodifica dei campi VAB/VAT
017900060726     c                   if        se_errore ='S'
018000060726     c                   eval      esito  = '1'
018100060726     c                   else
018200060726      *  Record OK
018300060726     c                   move      'S'           Almeno_Uno
018400060801     c                   delete    WFDPDW
018500060726     c                   end
018600081202     c                   endIf
018700060726
018800081202     c                   end
018900081202      * >>>>>>>>>>>>>
019000081202     c                   read      WFDPD1PC
019100060726     C                   ENDdo
019200060726      **
019300060726     c                   SETON                                        LR
019400060726      * ?______________________________________________________________     */
019500060726      *?    Decodifica record a campi variabili
019600060726      * ?______________________________________________________________     */
019700081202     c     Decod_Record1 Begsr
019800060726
019900060915      **   deve eliminare le virgole nei valori con decimali
020000081202     c                   eval      dati= %subst(xWFDPD1PC:1:90) +
020100081202     c                               %subst(xWFDPD1PC:92:12) +
020200081202     c                               %subst(xWFDPD1PC:105:20)
020300060915     c                   movel(p)  dati          TIdpc0DS
020400060915      **
020500060726     c                   Endsr
020600081202      * ?______________________________________________________________     */
020700081202      *?    Decodifica record con campi separati da (;)
020800081202      * ?______________________________________________________________     */
020900081202     c     Decod_Record2 Begsr
021000081202
021100081202      * ? Sposta il record a tracciato libero su un campo di lavoro       =
021200081202     c                   movel(p)  xWFDPD1PC     dati
021300081202     c                   eval      dati = %Trim(Dati) + ';'
021400081202     c                   exsr      Split_Rec
021500081202     c                   exsr      Rec_Dettaglio2
021600081202      **
021700081202     c                   Endsr
021800081202      * ?______________________________________________________________     */
021900081202      *?    Decodifica record con campi separati da (;)
022000081202      * ?______________________________________________________________     */
022100081202     c     Decod_Record3 Begsr
022200081202
022300081202      * ? Sposta il record a tracciato libero su un campo di lavoro       =
022400081202     c                   movel(p)  xWFDPD1PC     dati
022500081202     c                   eval      dati = %Trim(Dati) + ';'
022600081202     c                   exsr      Split_Rec
022700081202     c                   exsr      Rec_Dettaglio3
022800081202      **
022900081202     c                   Endsr
023000060726      * ?_______________________________________________________________    */
023100130717     C*? ESEGUO SCRITTURA del File TNEFR00F
023200060726      * ?_______________________________________________________________    */
023300060726     C     WRT_TNEFR     BEGSR
023400060726      *
023500060726     c                   clear                   tnEFR000
023600060726      *
023700060726     c                   eval      EFRNTW = 'DPD'
023800081202      *
023900081202      *  Imposta i campi con i vari tipi di tracciato
024000081202     c                   movel     DPCPRN        EFRRIF
024100081202      *
024200081202      * NON VA + BENE si salta la compattazione:
024300081202     c                   goto      NOcompatta
024400081202      **
024500081202      **  se il parcel ha '0' e '99' compatta il riferimento a '11' caratteri
024600081202     c                   if        %subst(efrRIF:1:1) = '0' and
024700081202     c                             %subst(efrRIF:5:2) = '99'
024800081202     c                   clear                   fld11            11
024900081202     c                   eval      fld11 = %subst(efrRIF:2:3) +
025000081202     c                                     %subst(efrRIF:7:8)
025100081202     c                   movel(p)  fld11         EFRRIF
025200081202     c                   end
025300081202      *
025400081202     c     NOcompatta    tag
025500081202      *
025600081202     c                   move      *all'0'       EFRNFT
025700081202     c                   if        DPCdoc <> *blank
025800081202     c     digits        check     DPCdoc:1
025900081202     c                   if        Not %Found
026000081202     c                   move      DPCdoc        EFRNFT
026100081202     c                   end
026200081202     c                   end
026300081202      *
026400081202      * cambia in tutti 9 il nr.fattura se non era presente
026500081202     c                   if        EFRNFT = *all'0'
026600081202     c                   move      *all'9'       EFRNFT
026700081202     c                   end
026800081202      *
026900081202     c                   movel     DPCymo        EFRDFT
027000081202      *
027100081202      *  La Data di fattura del clearing è una data generica di fine mese
027200081202     c                   move      DPCymo        mese              2 0
027300081202     c                   movel     DPCymo        anno              4 0
027400081202     c                   select
027500081202     c                   when      mese = 2
027600081202     c     anno          div       4             bise              2 0
027700081202     c                   mvr                     resto             3 0
027800081202     c                   if        resto > 0
027900081202     c                   move      28            EFRDFT
028000081202     c                   else
028100081202     c                   move      29            EFRDFT
028200081202     c                   end
028300081202     c                   when      mese = 11 or
028400081202     c                             mese = 04 or
028500081202     c                             mese = 06 or
028600081202     c                             mese = 09
028700081202     c                   move      30            EFRDFT
028800081202     c                   other
028900081202     c                   move      31            EFRDFT
029000081202     c                   endsl
029100081202      *
029200081202      *  Prende il gruppo di apprtenenza x causale originale
029300081202     c                   exsr      TBE_CFP
029400081202     c                   movel     DPCPCD        efrCAU
029500081202      *
029600081202      * se è un credito va inserito in negativo
029700081202     c                   if        DPCECD = 'G'
029800081202     c                   z-sub     DPCAMO        EFRIMP
029900081202     c                   move      'C'           EFRTPD
030000081202     c                   else
030100081202     c                   z-add     DPCAMO        EFRIMP
030200081202     c                   if        DPCECD = 'R'
030300081202     c                   move      'F'           EFRTPD
030400081202     c                   else
030500081202     c                   move      'D'           EFRTPD
030600081202     c                   end
030700081202     c                   end
030800081202      *  imposta UDATE
030900081202     C                   move      *date         G02DAT
031000081202     C                   MOVE      *ZEROS        G02INV
031100081202     C                   MOVE      *BLANKS       G02ERR
031200081202     C                   CALL      'XSRDA8'
031300081202     C                   PARM                    WLBDAT
031400081202     C                   Z-ADD     G02INV        efrDTR
031500081202      *
031600081202      *
031700081202     c*******            exsr      sped_Bartol
031800060726      *
031900060726      *   Se ci sono delle voci non classificate in tabella segnala in stampa
032000060726     c                   if        efrgrp = '999'
032100060726     c                   if        *in66
032200060726      *                 * ------------------ *
032300060726     c                   write     testa
032400060726      *                 * ------------------ *
032500060726     c                   setoff                                       66
032600060726     c                   end
032700060726     c                   write     dettagli
032800060726     c                   End
032900060726      *
033000060726     c                   write     tnEFR000
033100060726      *
033200060726     c                   Endsr
033300060726      * ?_______________________________________________________________    */
033400040115      *  Decodifica il raggruppamento per la Causale Originale
033500060726      * ?_______________________________________________________________    */
033600040115     c     TBE_CFP       Begsr
033700040115      *
033800040115     c                   move      '999'         Gruppo            3
033900040115      *
034000040115     c                   movel     'DPD'         cfpNTW
034100060726     c                   movel(P)  DPCPCD        cfpCAU
034200040115     c     Kcfp          setll     tnCFP01l
034300040115      *
034400040115     C                   IF        %EQUAL
034500040115      *
034600040115     C                   DOU       %EOF
034700040115     C     Kcfp          Reade     tnCFP01l
034800040115     C                   IF        NOT %EOF and
034900040115     C                             cfpDDA <= Data_oggi and
035000040115     C                             cfpADA >= Data_oggi
035100040115     c                   move      cfpGRP        Gruppo
035200040115     c                   leave
035300040115     C                   ENDIF
035400040115     C                   ENDDO
035500040115      *
035600040115     c                   end
035700040115      *
035800040115     c                   movel     Gruppo        efrGRP
035900040115      *
036000040115     c                   Endsr
036100081202      * ?_______________________________________________________________    */
036200081202      *?      Riga da controllare e tradurre
036300081202      * ?_______________________________________________________________    */
036400081202     c     Rec_Dettaglio2Begsr
036500081202      **
036600081202      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
036700081202     c                   clear                   TIdpc0DS
036800081202     c                   clear                   Msg_Err
036900081202      **
037000081202      * ? Nel 1° campo : Data YYYYMM                 (Oblligatorio)  Data
037100081202      **                                                             Data
037200081202     C                   z-add     1             sk
037300081202     c                   If        DatoNum_sk(sk) = 'N' or
037400081202     c                             LunDato_sk(sk) =  0
037500081202      * ?-----> Errore
037600081202     c                   eval      tipo_error = 2
037700081202     c                   eval      Campo_errato = 'DPCYMO'
037800081202     c                   exsr      Field_Error
037900081202     c                   Else
038000081202     c                   eval      DPCYMO   = atoi(%TRIM(Dato_sk(sk)))
038100081202     c                   End
038200081202      **
038300081202      * ? Nel 2° campo : Tipo Nota Credito                (Obbl.)    Tipo Importo
038400081202      **                                                             Tipo Importo
038500081202     c                   eval      DPCECD     = 'G'
038600081202      **
038700081202      * ? Nel  2° campo : Documento                  (Oblligatorio)  Numero Documento
038800081202      **                                                             Numero Documento
038900081202     C                   z-add     2             sk
039000081202     c                   If        LunDato_sk(sk) =  0
039100081202      * ?-----> Errore
039200081202     c                   eval      tipo_error = 1
039300081202     c                   eval      Campo_errato = 'DPCDOC'
039400081202     c                   exsr      Field_Error
039500081202     c                   Else
039600081202     c                   eval      DPCDOC     = %subst(Dato_sk(sk):1:+
039700081202     c                                              LunDato_sk(sk))
039800081202     c                   End
039900081202      **
040000081202      * ? Nel 3° campo : il Parcel Number                 (Obbl.)    Parcel Nr.
040100081202      **                                                             Parcel Nr.
040200081202     C                   z-add     3             sk
040300081202     c                   If        LunDato_sk(sk) =  0
040400081202      * ?-----> Errore
040500081202     c                   eval      tipo_error = 1
040600081202     c                   eval      Campo_errato = 'DPCPRN'
040700081202     c                   exsr      Field_Error
040800081202     c                   Else
040900081202     c                   eval      DPCPRN     = %subst(Dato_sk(sk):1:+
041000081202     c                                              LunDato_sk(sk))
041100081202      ** controlla lunghezza del Parcel
041200081202     c                   clear                   Lung_Parcel
041300081202     c                   eval      Lung_Parcel = %Len(%trim(DPCPRN))
041400081202      **
041500081202      * attenzione se si riceve un Parcel "Taroccato" ossia è di 11 ma con 0...99......
041600081202      * si deve riportarlo a 11
041700081202     C                   MOVEL     DPCPRN        dsparcel
041800081202     c                   clear                   parcel11
041900081202      *  Deve reimpostare a 11 il Parcel
042000081202     c                   if        Parcel0 = '0' and NoveNove ='99'
042100081202     c                   movel     Depot3        parcel11
042200081202     c                   move      NumParcel     parcel11
042300081202     C                   MOVEL(p)  parcel11      DPCPRN
042400081202     c                   end
042500081202      *
042600081202     c                   End
042700081202      **
042800081202      **
042900081202      * ? Nel 13° campo : Tipo Causale               (Oblligatorio)  Position Code
043000081202      **                                                             Tipo causale
043100081202     C                   z-add     13            sk
043200081202     c                   If        DatoNum_sk(sk) = 'N' or
043300081202     c                             LunDato_sk(sk) =  0
043400081202      * ?-----> Errore
043500081202     c                   eval      tipo_error = 2
043600081202     c                   eval      Campo_errato = 'DPCPCD'
043700081202     c                   exsr      Field_Error
043800081202     c                   Else
043900081202     c                   eval      DPCPCD   = atof(%TRIM(Dato_sk(sk)))
044000081202     c                   End
044100081202      **
044200081202      **
044300081202      * ? Nel 27° campo : Importo                    (Oblligatorio)  Importo
044400081202      **                                                             Importo
044500081202     C                   z-add     27            sk
044600081202     c                   If        DatoNum_sk(sk) = 'N' or
044700081202     c                             LunDato_sk(sk) =  0
044800081202      * ?-----> Errore
044900081202     c                   eval      tipo_error = 2
045000081202     c                   eval      Campo_errato = 'DPCAMO'
045100081202     c                   exsr      Field_Error
045200081202     c                   Else
045300081202     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
045400081202     c                   eval(H)   DPCAMO   = atof(%TRIM(Dato_sk(sk)))
045500081202     c                   End
045600081202      **
045700081202      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
045800081202      **
045900081202     c                   Endsr
046000081202      * ?_______________________________________________________________    */
046100081202      *?      Riga da controllare e tradurre
046200081202      * ?_______________________________________________________________    */
046300081202     c     Rec_Dettaglio3Begsr
046400081202      **
046500081202      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
046600081202     c                   clear                   TIdpc0DS
046700081202     c                   clear                   Msg_Err
046800081202      **
046900081202      * ? Nel 6° campo : Data YYYYMM                 (Oblligatorio)  Data
047000081202      **                                                             Data
047100081202     C                   z-add     6             sk
047200081202     c                   If        DatoNum_sk(sk) = 'N' or
047300081202     c                             LunDato_sk(sk) =  0
047400081202      * ?-----> Errore
047500081202     c                   eval      tipo_error = 2
047600081202     c                   eval      Campo_errato = 'DPCYMO'
047700081202     c                   exsr      Field_Error
047800081202     c                   Else
047900081202     c                   eval      DATA_8   = atoi(%TRIM(Dato_sk(sk)))
048000081202     c                   movel     data_8        DPCYMO
048100081202     c                   End
048200081202      **
048300081202      * ? Nel 2° campo : il Parcel Number                 (Obbl.)    Parcel Nr.
048400081202      **                                                             Parcel Nr.
048500081202     C                   z-add     2             sk
048600081202     c                   If        LunDato_sk(sk) =  0
048700081202      * ?-----> Errore
048800081202     c                   eval      tipo_error = 1
048900081202     c                   eval      Campo_errato = 'DPCPRN'
049000081202     c                   exsr      Field_Error
049100081202     c                   Else
049200081202     c                   eval      DPCPRN     = %subst(Dato_sk(sk):1:+
049300081202     c                                              LunDato_sk(sk))
049400081202      ** controlla lunghezza del Parcel
049500081202     c                   clear                   Lung_Parcel
049600081202     c                   eval      Lung_Parcel = %Len(%trim(DPCPRN))
049700081202      **
049800081202      * attenzione se si riceve un Parcel "Taroccato" ossia è di 11 ma con 0...99......
049900081202      * si deve riportarlo a 11
050000081202     C                   MOVEL     DPCPRN        dsparcel
050100081202     c                   clear                   parcel11
050200081202      *  Deve reimpostare a 11 il Parcel
050300081202     c                   if        Parcel0 = '0' and NoveNove ='99'
050400081202     c                   movel     Depot3        parcel11
050500081202     c                   move      NumParcel     parcel11
050600081202     C                   MOVEL(p)  parcel11      DPCPRN
050700081202     c                   else
050800081202      * Attenzione:
050900081202      *    se mandano il parcel a 12 o 13 caratteri
051000081202     c                   if        Lung_Parcel = 12
051100081202     C                   MOVEL(p)  DPCPRN        parcel11
051200081202     C                   MOVEL(p)  parcel11      DPCPRN
051300081202      *
051400081202     c                   elseIf    Lung_Parcel = 13
051500081202     C                   MOVE      *all'0'       parcel14
051600081202     C                   MOVE      DPCPRN        parcel14
051700081202     C                   MOVEL(p)  parcel14      DPCPRN
051800081202     c                   end
051900081202      *
052000081202     c                   end
052100081202      *
052200081202     c                   End
052300081202      **
052400081202      * ? Nel    campo : Tipo Nota Credito                (Obbl.)    Tipo Importo
052500081202      **                                                             Tipo Importo
052600081202     c                   eval      DPCECD     = 'G'
052700081202      **
052800081202      * ? Nel     campo : Importo                    (Oblligatorio)  Importo
052900081202      **                                                             Importo fisso
053000081202     c                   eval      DPCAMO   = Valore_x_Isola
053100081202      **
053200081202      * ? Nel     campo : Tipo Causale               (Obbligatorio)  Position Code
053300081202      **                                                             Tipo causale
053400081202     c                   eval      DPCPCD   = 950
053500081202      **
053600081202      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
053700081202      **
053800081202     c                   Endsr
053900081202      * ?____________________________________________________________       */
054000081202      *?      Campi in errore
054100081202      * ?____________________________________________________________       */
054200081202     c     Field_Error   Begsr
054300081202      **
054400081202     C                   eval      se_errore   = 'S'
054500081202      * msg errore
054600081202     c                   If        Msg_Err = *blank
054700081202     c
054800081202     c                   Select
054900081202     c
055000081202      * msg errore : Obbligatorio
055100081202     c                   When      Tipo_Error = 1
055200081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
055300081202     C                             ' >> Obblig.- Manca Dato !!'
055400081202     c
055500081202      * msg errore : Obbligatorio / Numerico
055600081202     c                   When      Tipo_Error = 2
055700081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
055800081202     C                             ' >> Obblig.Mancante o NON Numerico !!'
055900081202     c
056000081202      * msg errore : Numerico
056100081202     c                   When      Tipo_Error = 3
056200081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
056300081202     C                             ' >> NON Numerico  !!'
056400081202     c                   EndSL
056500081202     c
056600081202     c                   End
056700081202      **
056800081202     c                   Endsr
056900081202      * ?____________________________________________________________       */
057000081202      *?      Suddivide i campi della riga con carattere divisorio (;)
057100081202      * ?____________________________________________________________       */
057200081202     c     Split_Rec     Begsr
057300081202      **
057400081202     c                   clear                   Nr_campi
057500081202     c                   clear                   Campi_Record
057600081202     c                   clear                   Campi_Lunghi
057700081202     c                   clear                   Campi_Numerici
057800081202     c                   clear                   Campi_Decimali
057900081202     c                   movel     ';'           Separa_campi
058000081202     c                   movel     ','           Decimal_Separ
058100081202      *
058200081202     c                   call      'FIEU00R'
058300081202      * ?     Input
058400081202     c                   parm                    Separa_campi
058500081202     c                   parm                    Decimal_Separ
058600081202     c                   parm                    dati
058700081202      * ?     Output  su schiere di 100 elementi
058800081202     c                   parm                    Nr_campi
058900081202     c                   parm                    Campi_Record                   * i dati
059000081202     c                   parm                    Campi_Lunghi                   * lunghezza dati
059100081202     c                   parm                    Campi_Numerici                 * se numerici
059200081202     c                   parm                    Campi_Decimali                 * quanti decimali
059300081202      **
059400081202     c                   Endsr
059500060726      * ?_______________________________________________________________    */
059600060726     C     *INZSR        BEGSR
059700060726      * ?_______________________________________________________________    */
059800060726      *
059900060726     c     *ENTRY        PLIST
060000060726     C                   parm                    esito             1
060100060726      *
060200060726     c     kcfp          klist
060300060726     c                   kfld                    cfpNTW
060400060726     c                   kfld                    cfpCAU
060500060726      *
060600060726     c                   eval      partner = 'DPD'
060700060726      *
060800060726      *  imposta UDATE
060900060726     C                   move      *date         G02DAT
061000060726     C                   MOVE      *ZEROS        G02INV
061100060726     C                   MOVE      *BLANKS       G02ERR
061200060726     C                   CALL      'XSRDA8'
061300060726     C                   PARM                    WLBDAT
061400060726     C                   Z-ADD     G02INV        data_oggi         8 0
061500060726     C                   MOVE      data_oggi     day
061600060726      * Recupero data e ora
061700060726     C                   TIME                    W0140            14 0
061800060726     C                   MOVEl     W0140         ore
061900060726      * UDATE IN GGMMAAAA
062000060726     C                   MOVE      W0140         WDTGIO
062100060726      * UDATE IN AAAAMMGG
062200060726     C     *eur          MOVEL     WDTGIO        DATA_eur
062300060726     C     *iso          MOVEL     DATA_eur      dateu
062400060726     c                   seton                                        66
062500060726      *
062600060726     c                   Endsr
062700081202      * ?_______________________________________________________________    */
062800081202      *   Decodifica dai files di sede la spedizione (non + utilizzato)
062900081202      * ?_______________________________________________________________    */
063000081202     C*****Sped_Bartol   Begsr
063100081202      *********
063200081202      ***reperisce la spedizione bartolini da TITA433C
063300081202     c*********          movel     efrRIF        ta4DPD
063400081202     c*****ta4DPD        chain     tita434C
063500081202     c*********          if        %Found(tita434C)
063600081202     c*********          eval      EFRAAS = TA4AAS
063700081202     c*********          eval      EFRLNP = TA4LNP
063800081202     c*********          eval      EFRNRS = TA4NRS
063900081202     c*********          eval      EFRNSP = TA4NSP
064000081202     c*********          else
064100081202      **prova*con il RMN direttamente su TITAS
064200081202     c*****digits        check     ta4DPD:1
064300081202     c*********          if        Not %Found
064400081202     c*********          move      ta4DPD        fld11n           11 0
064500081202     c*****fld11n        mult      10            fld12n0          12 0
064600081202     c*********          z-add     fld12n0       fld12n9          12 0
064700081202     c*********          add       9             fld12n9
064800081202     c*********          z-add     fld12n0       fld15n0          15 0
064900081202     c*****fld15n0       setll     titas32C
065000081202     c*********          read      titas32C
065100081202     c*********          if        not %Eof(titas32C) and
065200081202     c*********                    tasrmn <= fld12n9
065300081202      **se*l'ha trovato imposta i campi della spedizione
065400081202     c*********          eval      EFRAAS = TAsAAS
065500081202     c*********          eval      EFRLNP = TAsLNP
065600081202     c*********          eval      EFRNRS = TAsNRS
065700081202     c*********          eval      EFRNSP = TAsNSP
065800081202     c*********          eval      EFRLNA = TAsLNA
065900081202     c*********          eval      EFRMGS = TAsMGS
066000081202     c*********          end
066100081202      *********
066200081202     c*********          end
066300081202     c*********          end
066400081202      *********
066500081202     C*********          Endsr
066600060911      * ?_______________________________________________________________    */
