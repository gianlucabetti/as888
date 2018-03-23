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
015100081202     c
015200081202      * >>>>>>>>>>>>>
015300081202      *  Per record generico  deve essere non tradotto ma solo cancellato
015400081202     c                   If        riga_generica = 'S'
015500081202     c                   delete    WFDPDW
015600081202     c                   else
015700081202      *      >>>>>>>>>>>>>
015800081202      ** Decodifica record a campi variabili
015900081202      *      >>>>>>>>>>>>>
016000081202     c                   If        traduttore = 1
016100081202     c                   exsr      Decod_Record1
016200081202     c
016300081202     c                   elseIf    traduttore = 2
016400081202     c                   exsr      Decod_Record2
016500081202     c
016600081202     c                   elseIf    traduttore = 3
016700081202     c                   exsr      Decod_Record3
016800081202     c                   endIf
016900081202      *  Record OK
017000060726      **  Se presente un errore nel record emette una segnalazione msg
017100060726     c                   if        se_errore ='S'
017200060726     c                   eval      esito  = '1'
017300060726     c                   else
017400060726      *  con il record in canna scrive direttamente il TNEFR
017500060726     c                   exsr      WRT_TNEFR
017600060726
017700060726      *  Problemi nella decodifica dei campi VAB/VAT
017800060726     c                   if        se_errore ='S'
017900060726     c                   eval      esito  = '1'
018000060726     c                   else
018100060726      *  Record OK
018200060726     c                   move      'S'           Almeno_Uno
018300060801     c                   delete    WFDPDW
018400060726     c                   end
018500081202     c                   endIf
018600060726
018700081202     c                   end
018800081202      * >>>>>>>>>>>>>
018900081202     c                   read      WFDPD1PC
019000060726     C                   ENDdo
019100060726      **
019200060726     c                   SETON                                        LR
019300060726      * ?______________________________________________________________     */
019400060726      *?    Decodifica record a campi variabili
019500060726      * ?______________________________________________________________     */
019600081202     c     Decod_Record1 Begsr
019700060726
019800060915      **   deve eliminare le virgole nei valori con decimali
019900081202     c                   eval      dati= %subst(xWFDPD1PC:1:90) +
020000081202     c                               %subst(xWFDPD1PC:92:12) +
020100081202     c                               %subst(xWFDPD1PC:105:20)
020200060915     c                   movel(p)  dati          TIdpc0DS
020300060915      **
020400060726     c                   Endsr
020500081202      * ?______________________________________________________________     */
020600081202      *?    Decodifica record con campi separati da (;)
020700081202      * ?______________________________________________________________     */
020800081202     c     Decod_Record2 Begsr
020900081202
021000081202      * ? Sposta il record a tracciato libero su un campo di lavoro       =
021100081202     c                   movel(p)  xWFDPD1PC     dati
021200081202     c                   eval      dati = %Trim(Dati) + ';'
021300081202     c                   exsr      Split_Rec
021400081202     c                   exsr      Rec_Dettaglio2
021500081202      **
021600081202     c                   Endsr
021700081202      * ?______________________________________________________________     */
021800081202      *?    Decodifica record con campi separati da (;)
021900081202      * ?______________________________________________________________     */
022000081202     c     Decod_Record3 Begsr
022100081202
022200081202      * ? Sposta il record a tracciato libero su un campo di lavoro       =
022300081202     c                   movel(p)  xWFDPD1PC     dati
022400081202     c                   eval      dati = %Trim(Dati) + ';'
022500081202     c                   exsr      Split_Rec
022600081202     c                   exsr      Rec_Dettaglio3
022700081202      **
022800081202     c                   Endsr
022900060726      * ?_______________________________________________________________    */
023000130717     C*? ESEGUO SCRITTURA del File TNEFR00F
023100060726      * ?_______________________________________________________________    */
023200060726     C     WRT_TNEFR     BEGSR
023300060726      *
023400060726     c                   clear                   tnEFR000
023500060726      *
023600060726     c                   eval      EFRNTW = 'DPD'
023700081202      *
023800081202      *  Imposta i campi con i vari tipi di tracciato
023900081202     c                   movel     DPCPRN        EFRRIF
024000081202      *
024100081202      * NON VA + BENE si salta la compattazione:
024200081202     c                   goto      NOcompatta
024300081202      **
024400081202      **  se il parcel ha '0' e '99' compatta il riferimento a '11' caratteri
024500081202     c                   if        %subst(efrRIF:1:1) = '0' and
024600081202     c                             %subst(efrRIF:5:2) = '99'
024700081202     c                   clear                   fld11            11
024800081202     c                   eval      fld11 = %subst(efrRIF:2:3) +
024900081202     c                                     %subst(efrRIF:7:8)
025000081202     c                   movel(p)  fld11         EFRRIF
025100081202     c                   end
025200081202      *
025300081202     c     NOcompatta    tag
025400081202      *
025500081202     c                   move      *all'0'       EFRNFT
025600081202     c                   if        DPCdoc <> *blank
025700081202     c     digits        check     DPCdoc:1
025800081202     c                   if        Not %Found
025900081202     c                   move      DPCdoc        EFRNFT
026000081202     c                   end
026100081202     c                   end
026200081202      *
026300081202      * cambia in tutti 9 il nr.fattura se non era presente
026400081202     c                   if        EFRNFT = *all'0'
026500081202     c                   move      *all'9'       EFRNFT
026600081202     c                   end
026700081202      *
026800081202     c                   movel     DPCymo        EFRDFT
026900081202      *
027000081202      *  La Data di fattura del clearing è una data generica di fine mese
027100081202     c                   move      DPCymo        mese              2 0
027200081202     c                   movel     DPCymo        anno              4 0
027300081202     c                   select
027400081202     c                   when      mese = 2
027500081202     c     anno          div       4             bise              2 0
027600081202     c                   mvr                     resto             3 0
027700081202     c                   if        resto > 0
027800081202     c                   move      28            EFRDFT
027900081202     c                   else
028000081202     c                   move      29            EFRDFT
028100081202     c                   end
028200081202     c                   when      mese = 11 or
028300081202     c                             mese = 04 or
028400081202     c                             mese = 06 or
028500081202     c                             mese = 09
028600081202     c                   move      30            EFRDFT
028700081202     c                   other
028800081202     c                   move      31            EFRDFT
028900081202     c                   endsl
029000081202      *
029100081202      *  Prende il gruppo di apprtenenza x causale originale
029200081202     c                   exsr      TBE_CFP
029300081202     c                   movel     DPCPCD        efrCAU
029400081202      *
029500081202      * se è un credito va inserito in negativo
029600081202     c                   if        DPCECD = 'G'
029700081202     c                   z-sub     DPCAMO        EFRIMP
029800081202     c                   move      'C'           EFRTPD
029900081202     c                   else
030000081202     c                   z-add     DPCAMO        EFRIMP
030100081202     c                   if        DPCECD = 'R'
030200081202     c                   move      'F'           EFRTPD
030300081202     c                   else
030400081202     c                   move      'D'           EFRTPD
030500081202     c                   end
030600081202     c                   end
030700081202      *  imposta UDATE
030800081202     C                   move      *date         G02DAT
030900081202     C                   MOVE      *ZEROS        G02INV
031000081202     C                   MOVE      *BLANKS       G02ERR
031100081202     C                   CALL      'XSRDA8'
031200081202     C                   PARM                    WLBDAT
031300081202     C                   Z-ADD     G02INV        efrDTR
031400081202      *
031500081202      *
031600081202     c*******            exsr      sped_Bartol
031700060726      *
031800060726      *   Se ci sono delle voci non classificate in tabella segnala in stampa
031900060726     c                   if        efrgrp = '999'
032000060726     c                   if        *in66
032100060726      *                 * ------------------ *
032200060726     c                   write     testa
032300060726      *                 * ------------------ *
032400060726     c                   setoff                                       66
032500060726     c                   end
032600060726     c                   write     dettagli
032700060726     c                   End
032800060726      *
032900060726     c                   write     tnEFR000
033000060726      *
033100060726     c                   Endsr
033200060726      * ?_______________________________________________________________    */
033300040115      *  Decodifica il raggruppamento per la Causale Originale
033400060726      * ?_______________________________________________________________    */
033500040115     c     TBE_CFP       Begsr
033600040115      *
033700040115     c                   move      '999'         Gruppo            3
033800040115      *
033900040115     c                   movel     'DPD'         cfpNTW
034000060726     c                   movel(P)  DPCPCD        cfpCAU
034100040115     c     Kcfp          setll     tnCFP01l
034200040115      *
034300040115     C                   IF        %EQUAL
034400040115      *
034500040115     C                   DOU       %EOF
034600040115     C     Kcfp          Reade     tnCFP01l
034700040115     C                   IF        NOT %EOF and
034800040115     C                             cfpDDA <= Data_oggi and
034900040115     C                             cfpADA >= Data_oggi
035000040115     c                   move      cfpGRP        Gruppo
035100040115     c                   leave
035200040115     C                   ENDIF
035300040115     C                   ENDDO
035400040115      *
035500040115     c                   end
035600040115      *
035700040115     c                   movel     Gruppo        efrGRP
035800040115      *
035900040115     c                   Endsr
036000081202      * ?_______________________________________________________________    */
036100081202      *?      Riga da controllare e tradurre
036200081202      * ?_______________________________________________________________    */
036300081202     c     Rec_Dettaglio2Begsr
036400081202      **
036500081202      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
036600081202     c                   clear                   TIdpc0DS
036700081202     c                   clear                   Msg_Err
036800081202      **
036900081202      * ? Nel 1° campo : Data YYYYMM                 (Oblligatorio)  Data
037000081202      **                                                             Data
037100081202     C                   z-add     1             sk
037200081202     c                   If        DatoNum_sk(sk) = 'N' or
037300081202     c                             LunDato_sk(sk) =  0
037400081202      * ?-----> Errore
037500081202     c                   eval      tipo_error = 2
037600081202     c                   eval      Campo_errato = 'DPCYMO'
037700081202     c                   exsr      Field_Error
037800081202     c                   Else
037900081202     c                   eval      DPCYMO   = atoi(%TRIM(Dato_sk(sk)))
038000081202     c                   End
038100081202      **
038200081202      * ? Nel 2° campo : Tipo Nota Credito                (Obbl.)    Tipo Importo
038300081202      **                                                             Tipo Importo
038400081202     c                   eval      DPCECD     = 'G'
038500081202      **
038600081202      * ? Nel  2° campo : Documento                  (Oblligatorio)  Numero Documento
038700081202      **                                                             Numero Documento
038800081202     C                   z-add     2             sk
038900081202     c                   If        LunDato_sk(sk) =  0
039000081202      * ?-----> Errore
039100081202     c                   eval      tipo_error = 1
039200081202     c                   eval      Campo_errato = 'DPCDOC'
039300081202     c                   exsr      Field_Error
039400081202     c                   Else
039500081202     c                   eval      DPCDOC     = %subst(Dato_sk(sk):1:+
039600081202     c                                              LunDato_sk(sk))
039700081202     c                   End
039800081202      **
039900081202      * ? Nel 3° campo : il Parcel Number                 (Obbl.)    Parcel Nr.
040000081202      **                                                             Parcel Nr.
040100081202     C                   z-add     3             sk
040200081202     c                   If        LunDato_sk(sk) =  0
040300081202      * ?-----> Errore
040400081202     c                   eval      tipo_error = 1
040500081202     c                   eval      Campo_errato = 'DPCPRN'
040600081202     c                   exsr      Field_Error
040700081202     c                   Else
040800081202     c                   eval      DPCPRN     = %subst(Dato_sk(sk):1:+
040900081202     c                                              LunDato_sk(sk))
041000081202      ** controlla lunghezza del Parcel
041100081202     c                   clear                   Lung_Parcel
041200081202     c                   eval      Lung_Parcel = %Len(%trim(DPCPRN))
041300081202      **
041400081202      * attenzione se si riceve un Parcel "Taroccato" ossia è di 11 ma con 0...99......
041500081202      * si deve riportarlo a 11
041600081202     C                   MOVEL     DPCPRN        dsparcel
041700081202     c                   clear                   parcel11
041800081202      *  Deve reimpostare a 11 il Parcel
041900081202     c                   if        Parcel0 = '0' and NoveNove ='99'
042000081202     c                   movel     Depot3        parcel11
042100081202     c                   move      NumParcel     parcel11
042200081202     C                   MOVEL(p)  parcel11      DPCPRN
042300081202     c                   end
042400081202      *
042500081202     c                   End
042600081202      **
042700081202      **
042800081202      * ? Nel 13° campo : Tipo Causale               (Oblligatorio)  Position Code
042900081202      **                                                             Tipo causale
043000081202     C                   z-add     13            sk
043100081202     c                   If        DatoNum_sk(sk) = 'N' or
043200081202     c                             LunDato_sk(sk) =  0
043300081202      * ?-----> Errore
043400081202     c                   eval      tipo_error = 2
043500081202     c                   eval      Campo_errato = 'DPCPCD'
043600081202     c                   exsr      Field_Error
043700081202     c                   Else
043800081202     c                   eval      DPCPCD   = atof(%TRIM(Dato_sk(sk)))
043900081202     c                   End
044000081202      **
044100081202      **
044200081202      * ? Nel 27° campo : Importo                    (Oblligatorio)  Importo
044300081202      **                                                             Importo
044400081202     C                   z-add     27            sk
044500081202     c                   If        DatoNum_sk(sk) = 'N' or
044600081202     c                             LunDato_sk(sk) =  0
044700081202      * ?-----> Errore
044800081202     c                   eval      tipo_error = 2
044900081202     c                   eval      Campo_errato = 'DPCAMO'
045000081202     c                   exsr      Field_Error
045100081202     c                   Else
045200081202     c     ',':'.'       xlate     Dato_sk(sk)   Dato_sk(sk)
045300081202     c                   eval(H)   DPCAMO   = atof(%TRIM(Dato_sk(sk)))
045400081202     c                   End
045500081202      **
045600081202      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
045700081202      **
045800081202     c                   Endsr
045900081202      * ?_______________________________________________________________    */
046000081202      *?      Riga da controllare e tradurre
046100081202      * ?_______________________________________________________________    */
046200081202     c     Rec_Dettaglio3Begsr
046300081202      **
046400081202      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
046500081202     c                   clear                   TIdpc0DS
046600081202     c                   clear                   Msg_Err
046700081202      **
046800081202      * ? Nel 6° campo : Data YYYYMM                 (Oblligatorio)  Data
046900081202      **                                                             Data
047000081202     C                   z-add     6             sk
047100081202     c                   If        DatoNum_sk(sk) = 'N' or
047200081202     c                             LunDato_sk(sk) =  0
047300081202      * ?-----> Errore
047400081202     c                   eval      tipo_error = 2
047500081202     c                   eval      Campo_errato = 'DPCYMO'
047600081202     c                   exsr      Field_Error
047700081202     c                   Else
047800081202     c                   eval      DATA_8   = atoi(%TRIM(Dato_sk(sk)))
047900081202     c                   movel     data_8        DPCYMO
048000081202     c                   End
048100081202      **
048200081202      * ? Nel 2° campo : il Parcel Number                 (Obbl.)    Parcel Nr.
048300081202      **                                                             Parcel Nr.
048400081202     C                   z-add     2             sk
048500081202     c                   If        LunDato_sk(sk) =  0
048600081202      * ?-----> Errore
048700081202     c                   eval      tipo_error = 1
048800081202     c                   eval      Campo_errato = 'DPCPRN'
048900081202     c                   exsr      Field_Error
049000081202     c                   Else
049100081202     c                   eval      DPCPRN     = %subst(Dato_sk(sk):1:+
049200081202     c                                              LunDato_sk(sk))
049300081202      ** controlla lunghezza del Parcel
049400081202     c                   clear                   Lung_Parcel
049500081202     c                   eval      Lung_Parcel = %Len(%trim(DPCPRN))
049600081202      **
049700081202      * attenzione se si riceve un Parcel "Taroccato" ossia è di 11 ma con 0...99......
049800081202      * si deve riportarlo a 11
049900081202     C                   MOVEL     DPCPRN        dsparcel
050000081202     c                   clear                   parcel11
050100081202      *  Deve reimpostare a 11 il Parcel
050200081202     c                   if        Parcel0 = '0' and NoveNove ='99'
050300081202     c                   movel     Depot3        parcel11
050400081202     c                   move      NumParcel     parcel11
050500081202     C                   MOVEL(p)  parcel11      DPCPRN
050600081202     c                   else
050700081202      * Attenzione:
050800081202      *    se mandano il parcel a 12 o 13 caratteri
050900081202     c                   if        Lung_Parcel = 12
051000081202     C                   MOVEL(p)  DPCPRN        parcel11
051100081202     C                   MOVEL(p)  parcel11      DPCPRN
051200081202      *
051300081202     c                   elseIf    Lung_Parcel = 13
051400081202     C                   MOVE      *all'0'       parcel14
051500081202     C                   MOVE      DPCPRN        parcel14
051600081202     C                   MOVEL(p)  parcel14      DPCPRN
051700081202     c                   end
051800081202      *
051900081202     c                   end
052000081202      *
052100081202     c                   End
052200081202      **
052300081202      * ? Nel    campo : Tipo Nota Credito                (Obbl.)    Tipo Importo
052400081202      **                                                             Tipo Importo
052500081202     c                   eval      DPCECD     = 'G'
052600081202      **
052700081202      * ? Nel     campo : Importo                    (Oblligatorio)  Importo
052800081202      **                                                             Importo fisso
052900081202     c                   eval      DPCAMO   = Valore_x_Isola
053000081202      **
053100081202      * ? Nel     campo : Tipo Causale               (Obbligatorio)  Position Code
053200081202      **                                                             Tipo causale
053300081202     c                   eval      DPCPCD   = 950
053400081202      **
053500081202      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
053600081202      **
053700081202     c                   Endsr
053800081202      * ?____________________________________________________________       */
053900081202      *?      Campi in errore
054000081202      * ?____________________________________________________________       */
054100081202     c     Field_Error   Begsr
054200081202      **
054300081202     C                   eval      se_errore   = 'S'
054400081202      * msg errore
054500081202     c                   If        Msg_Err = *blank
054600081202     c
054700081202     c                   Select
054800081202     c
054900081202      * msg errore : Obbligatorio
055000081202     c                   When      Tipo_Error = 1
055100081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
055200081202     C                             ' >> Obblig.- Manca Dato !!'
055300081202     c
055400081202      * msg errore : Obbligatorio / Numerico
055500081202     c                   When      Tipo_Error = 2
055600081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
055700081202     C                             ' >> Obblig.Mancante o NON Numerico !!'
055800081202     c
055900081202      * msg errore : Numerico
056000081202     c                   When      Tipo_Error = 3
056100081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
056200081202     C                             ' >> NON Numerico  !!'
056300081202     c                   EndSL
056400081202     c
056500081202     c                   End
056600081202      **
056700081202     c                   Endsr
056800081202      * ?____________________________________________________________       */
056900081202      *?      Suddivide i campi della riga con carattere divisorio (;)
057000081202      * ?____________________________________________________________       */
057100081202     c     Split_Rec     Begsr
057200081202      **
057300081202     c                   clear                   Nr_campi
057400081202     c                   clear                   Campi_Record
057500081202     c                   clear                   Campi_Lunghi
057600081202     c                   clear                   Campi_Numerici
057700081202     c                   clear                   Campi_Decimali
057800081202     c                   movel     ';'           Separa_campi
057900081202     c                   movel     ','           Decimal_Separ
058000081202      *
058100081202     c                   call      'FIEU00R'
058200081202      * ?     Input
058300081202     c                   parm                    Separa_campi
058400081202     c                   parm                    Decimal_Separ
058500081202     c                   parm                    dati
058600081202      * ?     Output  su schiere di 100 elementi
058700081202     c                   parm                    Nr_campi
058800081202     c                   parm                    Campi_Record                   * i dati
058900081202     c                   parm                    Campi_Lunghi                   * lunghezza dati
059000081202     c                   parm                    Campi_Numerici                 * se numerici
059100081202     c                   parm                    Campi_Decimali                 * quanti decimali
059200081202      **
059300081202     c                   Endsr
059400060726      * ?_______________________________________________________________    */
059500060726     C     *INZSR        BEGSR
059600060726      * ?_______________________________________________________________    */
059700060726      *
059800060726     c     *ENTRY        PLIST
059900060726     C                   parm                    esito             1
060000060726      *
060100060726     c     kcfp          klist
060200060726     c                   kfld                    cfpNTW
060300060726     c                   kfld                    cfpCAU
060400060726      *
060500060726     c                   eval      partner = 'DPD'
060600060726      *
060700060726      *  imposta UDATE
060800060726     C                   move      *date         G02DAT
060900060726     C                   MOVE      *ZEROS        G02INV
061000060726     C                   MOVE      *BLANKS       G02ERR
061100060726     C                   CALL      'XSRDA8'
061200060726     C                   PARM                    WLBDAT
061300060726     C                   Z-ADD     G02INV        data_oggi         8 0
061400060726     C                   MOVE      data_oggi     day
061500060726      * Recupero data e ora
061600060726     C                   TIME                    W0140            14 0
061700060726     C                   MOVEl     W0140         ore
061800060726      * UDATE IN GGMMAAAA
061900060726     C                   MOVE      W0140         WDTGIO
062000060726      * UDATE IN AAAAMMGG
062100060726     C     *eur          MOVEL     WDTGIO        DATA_eur
062200060726     C     *iso          MOVEL     DATA_eur      dateu
062300060726     c                   seton                                        66
062400060726      *
062500060726     c                   Endsr
062600081202      * ?_______________________________________________________________    */
062700081202      *   Decodifica dai files di sede la spedizione (non + utilizzato)
062800081202      * ?_______________________________________________________________    */
062900081202     C*****Sped_Bartol   Begsr
063000081202      *********
063100081202      ***reperisce la spedizione bartolini da TITA433C
063200081202     c*********          movel     efrRIF        ta4DPD
063300081202     c*****ta4DPD        chain     tita434C
063400081202     c*********          if        %Found(tita434C)
063500081202     c*********          eval      EFRAAS = TA4AAS
063600081202     c*********          eval      EFRLNP = TA4LNP
063700081202     c*********          eval      EFRNRS = TA4NRS
063800081202     c*********          eval      EFRNSP = TA4NSP
063900081202     c*********          else
064000081202      **prova*con il RMN direttamente su TITAS
064100081202     c*****digits        check     ta4DPD:1
064200081202     c*********          if        Not %Found
064300081202     c*********          move      ta4DPD        fld11n           11 0
064400081202     c*****fld11n        mult      10            fld12n0          12 0
064500081202     c*********          z-add     fld12n0       fld12n9          12 0
064600081202     c*********          add       9             fld12n9
064700081202     c*********          z-add     fld12n0       fld15n0          15 0
064800081202     c*****fld15n0       setll     titas32C
064900081202     c*********          read      titas32C
065000081202     c*********          if        not %Eof(titas32C) and
065100081202     c*********                    tasrmn <= fld12n9
065200081202      **se*l'ha trovato imposta i campi della spedizione
065300081202     c*********          eval      EFRAAS = TAsAAS
065400081202     c*********          eval      EFRLNP = TAsLNP
065500081202     c*********          eval      EFRNRS = TAsNRS
065600081202     c*********          eval      EFRNSP = TAsNSP
065700081202     c*********          eval      EFRLNA = TAsLNA
065800081202     c*********          eval      EFRMGS = TAsMGS
065900081202     c*********          end
066000081202      *********
066100081202     c*********          end
066200081202     c*********          end
066300081202      *********
066400081202     C*********          Endsr
066500060911      * ?_______________________________________________________________    */
