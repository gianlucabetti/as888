000100060726     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060726     H BNDDIR('QC2LE')
000300060727     H DECEDIT('0,') DATEDIT(*DMY.)
000400081202      * ?====================================================================== */
000500140227 * ?  *   Nuovo traduttore Files Clearing ICS
000600081202      * ?====================================================================== */
000700140227      *  via FTP - Da un UNICO FILE                                             *
000800140307      *             non ci sono più le causali e i raggruppamenti x il TNEFR    *
000900140307      *             ma una somma verticale di tutto.                            *
001000081202      * Il pgm crea:                                                            *
001100130717      *             TNEFR00F x DPD                                              *
001200081202      * ?====================================================================== */
001300081202     FWfDPD1PC  UF   E             DISK    rename(WFDPD1PC:WFDPDW)
001400060727     F                                     PREFIX(x)
001500060727      *
001600160929     FTNEFR00F  O  a E             DISK    usropn
001700140227     fFIEU60P   o    E             PRINTER OFLIND(*in66)
001800060727      * ?================================================================== */
001900140312      *
002000140312      *  Invio E-mail
002100140312     D Indirizzi       s            253
002200140312     D Oggetto         s             44
002300140312     D Messaggio       s           5000
002400140312      *
002500040901     d stamp           ds
002600040901     d  lin                  367    368b 0
002700040901      *
002800030624     D WLBDAT          DS
002900030624     D  G02DAT                 1      8  0
003000030624     D  G02INV                 9     16  0
003100030624     D  G02ERR                17     17
003200030624     D  G02TGI                18     22  0
003300060726      *
003400060726     D WDTGIO          S              8  0
003500060726     D DATEU           S              8  0
003600060726     D DATamg          S              8  0
003700060726     D DATA_eur        S               D   DATFMT(*eur)
003800060726     D DATA_amg        S               D   DATFMT(*iso)
003900060726      *-------------------
004000081202     D Data_8          S              8  0
004100060726     D Tipo_error      S              1  0
004200060726     D Campo_errato    S             10
004300060911      *
004400060911      * CALCOLO DEL CHCK DIGIT PER BARCODE DPD
004500060911     D                 DS
004600060911     D  §11                    1     11  0
004700060911     D                                     DIM(11)
004800060911     D  DPDCHD                12     12  0
004900060911     D  DPDBRC                 1     12  0
005000081202      *
005100081202     D Valore_x_Isola...
005200081202     D                 S              7s 4 inz(13,1613)
005300060726      * ?================================================================== */
005400060726     C*? Ds Decodifica dei campi
005500060726      * ?================================================================== */
005600140227     D TIDPC0DS      E DS                  EXTNAME(TIDPCICS)
005700150423     D kpjba         E DS
005800060726      **
005900060726      * ?   * Campi da tradurre con interi
006000060726     D atoi            PR            10I 0
006100060726     D                                     EXTPROC('atoi')
006200060726     D  Char                           *
006300060726     D                                     VALUE
006400060726     D                                     OPTIONS(*STRING)
006500060726      * ?   * Campi da tradurre con decimali
006600060726     D atof            PR             8F
006700060726     D                                     EXTPROC('atof')
006800060726     D  Char                           *
006900060726     D                                     VALUE
007000060726     D                                     OPTIONS(*STRING)
007100060726      * ?   *--------------------------------------------------------------*
007200060726      * ?   ( Descrizione Campi x scomposizione FLAT record )
007300060726      * ?   *--------------------------------------------------------------*
007400060726      *
007500060726      * ?   * Campi x decodifica * (INPUT  del Record)
007600060726     D  Dati           s           1500
007700060726     D Separa_campi    s              1
007800060726     D Decimal_Separ   s              1
007900060726      * ?   * Campi decodificati * (OUTPUT del Record)
008000060726      *       Schiere di Output / Campi di Output
008100060726     D  Alfa40         s             40
008200060726     d  Sk             s              3u 0
008300060726     D    NR_campi     s              3u 0
008400060726     D  Campi_Record   ds
008500060726     D    Dato_sk                    40    DIM(100)
008600060726     D  Campi_Lunghi   ds
008700060726     D    LunDato_sk                  3u 0 DIM(100)
008800060726     D Campi_Numerici  ds
008900060726     D    DatoNum_sk                  1    DIM(100)
009000060726     D Campi_Decimali  ds
009100060726     D    Decimal_sk                  3u 0 DIM(100)
009200060726      * ?   *--------------------------------------------------------------*
009300060726     D  position       s              3  0 INZ(0)
009400060726
009500060726     D  se_errore      s              1    inz(' ')
009600060726     D  Msg_Err        s            132    inz(' ')
009700060726      * ?* ------------------------------------------------------ *
009800081202      *
009900150515     D numeri_ammessi  C                   '0123456789.'
010000140312     d CEDalert        C                   CONST('CEDalert@Brt.it;')
010100060726      * ?================================================================== */
010200060726      *   Ciclo principale
010300060726      * ?================================================================== */
010400060726      * per controllare se almeno un record è stato importato sul VAB
010500081202     c                   clear                   traduttore        1 0
010600081202     c                   clear                   riga_generica     1
010700060726     c                   clear                   Almeno_Uno        1
010800060726
010900140312     c                   clear                   righe_rimaste     5 0
011000140312
011100060726      * Continua a leggere dal secondo record.
011200060726      *   Il primo è la testata già controllata dalla Check_trasm.
011300081202     c                   read      WFDPD1PC
011400081202     c                   dow       not %eof(WFDPD1PC)
011500140227      * >>>>>>>>>>>>>
011600140227     c                   eval      riga_generica = 'N'
011700140227     c                   clear                   se_errore
011800091005      * se record vuoto
011900091005     c                   If        xWFDPD1PC  = *blank
012000081202     c                   eval       riga_generica = 'S'
012100140227     c                   else
012200140227     c                   movel(p)  xWFDPD1PC     dati
012300140227     c                   eval      position = %scan('ParcelNo':dati)
012400140227     c                   if        position >0
012500140227     c                   eval       riga_generica = 'S'
012600140227     c                   end
012700140227     c                   end
012800081202      * >>>>>>>>>>>>>
012900081202      *  Per record generico  deve essere non tradotto ma solo cancellato
013000140312     c                   If        riga_generica = 'S'
013100140312     c                   delete    WFDPDW
013200140312      * >>>>>>>>>>>>>
013300140312     c                   elseIF    riga_generica = 'N'
013400140303      *
013500081202      *      >>>>>>>>>>>>>
013600081202      ** Decodifica record a campi variabili
013700081202      *      >>>>>>>>>>>>>
013800140227     c                   exsr      Decod_Record
013900081202      *  Record OK
014000060726      **  Se presente un errore nel record emette una segnalazione msg
014100060726     c                   if        se_errore ='S'
014200140317      **
014300140317      **   stampa ECCEZIONE - riga NON IMPORTATA
014400140317     c                   write     dettagli
014500140317      **
014600140312     c                   eval      righe_rimaste = righe_rimaste +1
014700140317      **
014800060726     c                   else
014900140303
015000060726      *  con il record in canna scrive direttamente il TNEFR
015100060726     c                   exsr      WRT_TNEFR
015200060726
015300060726      *  Problemi nella decodifica dei campi VAB/VAT
015400060726     c                   if        se_errore ='S'
015500140317      **
015600140317      **   stampa ECCEZIONE - riga NON IMPORTATA
015700140317     c                   moveL     Msg_Err       ERRORE
015800140317     c                   write     dettagli
015900140317      **
016000140312     c                   eval      righe_rimaste = righe_rimaste +1
016100140317
016200140317     c                   move      Msg_Err       xWFDPD1PC
016300140317     c                   update    WFDPDW
016400140317      **
016500140303     c                   else
016600140317      **
016700060726     c                   move      'S'           Almeno_Uno
016800140311     c                   delete    WFDPDW
016900140317      **
017000060726     c                   end
017100140303
017200081202     c                   endIf
017300060726
017400140303     c                   endIf
017500081202      * >>>>>>>>>>>>>
017600081202     c                   read      WFDPD1PC
017700060726     C                   ENDdo
017800140312      **
017900140312     c                   if        righe_rimaste >0
018000140312     c                   exsr      invio_mail
018100140312     c                   end
018200160929      **
018300160929     c                   close     TNEFR00F
018400060726      **
018500160929      **  trasforma i RESI in DIROTTAMENTI (solo Export) dove il reso è
018600160929      **   dato per consegnato.
018700160929     c                   exsr      DIROTTA
018800160929      **
018900060726     c                   SETON                                        LR
019000060726      * ?______________________________________________________________     */
019100060726      *?    Decodifica record a campi variabili
019200060726      * ?______________________________________________________________     */
019300140227     c     Decod_Record  Begsr
019400140227
019500140227      * ? Sposta il record a tracciato libero su un campo di lavoro       =
019600140303     c                   clear                   deb_cred          1
019700140227     c                   movel(p)  xWFDPD1PC     dati
019800140227     c                   eval      dati = %Trim(Dati) + ';'
019900140227     c                   exsr      Split_Rec
020000140227     c                   exsr      Rec_Dettaglio
020100140227      **
020200140227     c                   Endsr
020300081202      * ?_______________________________________________________________    */
020400081202      *?      Riga da controllare e tradurre
020500081202      * ?_______________________________________________________________    */
020600140227     c     Rec_Dettaglio Begsr
020700081202      **
020800081202      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
020900081202     c                   clear                   TIdpc0DS
021000081202     c                   clear                   Msg_Err
021100140227      **
021200140227      * ? Nel 1° campo : il Parcel Number                 (Obbl.)    Parcel Nr.
021300140227      **                                                             Parcel Nr.
021400140227     C                   z-add     1             sk
021500140227     c                   If        LunDato_sk(sk) =  0
021600140317     c                              or
021700140317     c                             LunDato_sk(sk) > 1 and
021800140317     c                             LunDato_sk(sk) < 14
021900140227      * ?-----> Errore
022000140227     c                   eval      tipo_error = 1
022100140227     c                   eval      Campo_errato = 'DPCPRN'
022200140227     c                   exsr      Field_Error
022300140227     c                   Else
022400140227     c                   eval      DPCPRN     = %subst(Dato_sk(sk):1:+
022500140227     c                                              LunDato_sk(sk))
022600140227     c                   End
022700140227      **
022800081202      **
022900140303      * ? Nel 2° campo : Data YYYY-MM                (Oblligatorio)  Data
023000081202      **                                                             Data
023100140227     C                   z-add     2             sk
023200140303     c                   If        LunDato_sk(sk) =  0
023300140317     c                              or
023400140317     c                             LunDato_sk(sk) > 1 and
023500140317     c                             LunDato_sk(sk) < 7
023600140317     c                              or
023700140317     c                             %subst(Dato_sk(sk):5:1) <>'-'
023800140303      * ?-----> Errore
023900140303     c                   eval      tipo_error = 1
024000140303     c                   eval      Campo_errato = 'DPCYMO'
024100140303     c                   exsr      Field_Error
024200140303     c                   Else
024300140303     c                   eval      DPCYMO     = %subst(Dato_sk(sk):1:+
024400140303     c                                              LunDato_sk(sk))
024500081202     c                   End
024600081202      **
024700140305      **
024800140305      * ? Nel 12° campo :                  PROCESS/CAUSALE
024900140305      **
025000140305     C                   z-add     12            sk
025100140311     c                   If        LunDato_sk(sk) >  0
025200140305     c                   eval      DPCPROC    = %subst(Dato_sk(sk):1:+
025300140305     c                                              LunDato_sk(sk))
025400140305     c                   End
025500140305      **
025600140305      **
025700140305      * ? Nel 13° campo :                  ELEMENT
025800140305      **
025900140305     C                   z-add     13            sk
026000140311     c                   If        LunDato_sk(sk) >  0
026100140305     c                   eval      DPCELEM    = %subst(Dato_sk(sk):1:+
026200140305     c                                              LunDato_sk(sk))
026300140305     c                   End
026400140305      **
026500140307      **
026600140307      * ? Nel 14° campo :                  RATE-KEY
026700140307      **
026800140307     C                   z-add     14            sk
026900140311     c                   If        LunDato_sk(sk) >  0
027000140307     c                   eval      DPCRAKE    = %subst(Dato_sk(sk):1:+
027100140307     c                                              LunDato_sk(sk))
027200140307     c                   End
027300140307      **
027400140305      **
027500140305      * ? Nel 15° campo :                  COD WEIGHT
027600140305      **
027700140305     C                   z-add     15            sk
027800140311     c                   If        LunDato_sk(sk) >  0
027900140305     c                   eval      DPCWGHT    = %subst(Dato_sk(sk):1:+
028000140305     c                                              LunDato_sk(sk))
028100140305     c                   End
028200140305      **
028300081202      **
028400140227      * ? Nel 21° campo : Importo                    (Oblligatorio)  Importo
028500140317      **
028600140303      **    OCCORRE CAPIRE SE UN CREDITO O DEBITO
028700140303      **     IL DEBITO È PRECEDUTO DAL (-)
028800140303     C                   z-add     21            sk
028900140317      **
029000140317     c                   If        LunDato_sk(sk) >  0
029100140303     c                   movel     'C'           deb_cred
029200140303      *
029300140303     c                   if        %subst(Dato_sk(sk):1:1) = '-'
029400140303     c                   movel     'D'           deb_cred
029500140303     c                   eval      Dato_sk(sk) = %subst(Dato_sk(sk):2:39)
029600140317     c                   eval      LunDato_sk(sk) = LunDato_sk(sk) - 1
029700140303     c                   end
029800140317      **
029900140317      **  TOGLIE il PUNTO e lo sostituisce
030000150515      ******* ATTENZIONE ATOF vuole il punto
030100150515     c***************     '.':','       xlate     Dato_sk(sk)   Dato_sk(sk)
030200140317      **
030300140317      **  se ci sono altri caratteri al posto dei numeri e della virgola
030400140317     c     numeri_ammessiCHECK     Dato_sk(sk)   N                 3 0
030500140317     c                   if        %Found
030600140317     c                              and N <= LunDato_sk(sk)
030700140317      * ?-----> Errore
030800140317     c                   eval      tipo_error = 2
030900140317     c                   eval      Campo_errato = 'DPCAMO'
031000140317     c                   exsr      Field_Error
031100140317     c                   else
031200140317      **
031300081202     c                   eval(H)   DPCAMO   = atof(%TRIM(Dato_sk(sk)))
031400140317     c                   end
031500140317     c                   end
031600081202      **
031700081202      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
031800081202      **
031900081202     c                   Endsr
032000081202      * ?____________________________________________________________       */
032100081202      *?      Campi in errore
032200081202      * ?____________________________________________________________       */
032300081202     c     Field_Error   Begsr
032400081202      **
032500081202     C                   eval      se_errore   = 'S'
032600081202      * msg errore
032700081202     c                   If        Msg_Err = *blank
032800081202     c
032900081202     c                   Select
033000081202     c
033100081202      * msg errore : Obbligatorio
033200081202     c                   When      Tipo_Error = 1
033300081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
033400081202     C                             ' >> Obblig.- Manca Dato !!'
033500081202     c
033600081202      * msg errore : Obbligatorio / Numerico
033700081202     c                   When      Tipo_Error = 2
033800081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
033900081202     C                             ' >> Obblig.Mancante o NON Numerico !!'
034000081202     c
034100081202      * msg errore : Numerico
034200081202     c                   When      Tipo_Error = 3
034300081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
034400081202     C                             ' >> NON Numerico  !!'
034500081202     c                   EndSL
034600081202     c
034700081202     c                   End
034800081202      **
034900081202     c                   Endsr
035000081202      * ?____________________________________________________________       */
035100081202      *?      Suddivide i campi della riga con carattere divisorio (;)
035200081202      * ?____________________________________________________________       */
035300081202     c     Split_Rec     Begsr
035400081202      **
035500081202     c                   clear                   Nr_campi
035600081202     c                   clear                   Campi_Record
035700081202     c                   clear                   Campi_Lunghi
035800081202     c                   clear                   Campi_Numerici
035900081202     c                   clear                   Campi_Decimali
036000081202     c                   movel     ';'           Separa_campi
036100081202     c                   movel     ','           Decimal_Separ
036200081202      *
036300081202     c                   call      'FIEU00R'
036400081202      * ?     Input
036500081202     c                   parm                    Separa_campi
036600081202     c                   parm                    Decimal_Separ
036700081202     c                   parm                    dati
036800081202      * ?     Output  su schiere di 100 elementi
036900081202     c                   parm                    Nr_campi
037000081202     c                   parm                    Campi_Record                   * i dati
037100081202     c                   parm                    Campi_Lunghi                   * lunghezza dati
037200081202     c                   parm                    Campi_Numerici                 * se numerici
037300081202     c                   parm                    Campi_Decimali                 * quanti decimali
037400081202      **
037500081202     c                   Endsr
037600140227      * ?_______________________________________________________________    */
037700140227     C*? ESEGUO SCRITTURA del File TNEFR00F
037800140227      * ?_______________________________________________________________    */
037900140227     C     WRT_TNEFR     BEGSR
038000140227      *
038100160628      * CAUSALI :   1°Byte / 2°Byte / 3°Byte
038200160628      *             N=Normale
038300160628      *             R=Reso
038400160628      *             O=ORM
038500160628      *                    / P=PICCOLO
038600160628      *                    / G=GRANDE
038700160628      *                    / F=FUORI MISURA
038800160628      *                             /  0=NoISOLA
038900160628      *                             /  I=ISOLA
039000160628      * quindi i codici possono essere la combinazione dei 3 bytes
039100160628      *
039200160628      *
039300140227     c                   clear                   tnEFR000
039400140227      *
039500140227     c                   eval      EFRNTW = 'DPD'
039600140227      *
039700140227      *  Imposta i campi con i vari tipi di tracciato
039800140227     c                   movel     DPCPRN        EFRRIF
039900140305     c                   eval      EFRRFP = %trim(DPCproc) + ';' +
040000140305     c                                      %trim(DPCelem) + ';' +
040100140305     c                                      %trim(DPCwght)
040200140227      *
040300140227     c                   movel     DPCymo        EFRNFT
040400140227      *
040500160628      * Causale  inizializzata
040600160628     c                   movel     'NG0'         EFRCAU
040700140314      *
040800160628      *  W31.5+ causale = 'xFx'         FUORI MISURA
040900140314     c     'W31.5+'      SCAN      DPCwght
041000140314     c                   if        %FOUND
041100160628     c                   eval      %subst(EFRCAU:2:1)='F'
041200160628     c                   else
041300160628      *  W03    causale = 'xPx'         PICCOLO
041400160628     c     'W03 '        SCAN      DPCwght
041500160628     c                   if        %FOUND
041600160628     c                   eval      %subst(EFRCAU:2:1)='P'
041700140314     c                   end
041800160628     c                   end
041900160628      *
042000160628      *  ISLAND causale = 'xxI'         ISOLA
042100160628     c     'ISLAND'      SCAN      DPCelem
042200160628     c                   if        %FOUND
042300160628     c                   eval      %subst(EFRCAU:3:1)='I'
042400160628     c                   end
042500140314      *
042600160628      *  Pick   causale = 'Oxx'         O.R.M. Pick-up
042700140314     c     'Pick'        SCAN      DPCproc
042800140314     c                   if        %FOUND
042900160628     c                   eval      %subst(EFRCAU:1:1)='O'
043000140314     c                   end
043100160628      *
043200160628      *  RESO   causale = 'Rxx'         RESO
043300160628     c     'RETOURE'     SCAN      DPCproc
043400160628     c                   if        %FOUND
043500160628     c                   eval      %subst(EFRCAU:1:1)='R'
043600160628     c                   end
043700140303      *
043800140227      *  La Data di fattura del clearing è una data generica di fine mese
043900140227     c                   move      DPCymo        mese              2 0
044000140227     c                   movel     DPCymo        anno              4 0
044100140303      *
044200140303     C                   EVAL      EFRDFT = (ANNO*10000)+(MESE*100)
044300140303      *
044400140227     c                   select
044500140227     c                   when      mese = 2
044600140227     c     anno          div       4             bise              2 0
044700140227     c                   mvr                     resto             3 0
044800140227     c                   if        resto > 0
044900140227     c                   move      28            EFRDFT
045000140227     c                   else
045100140227     c                   move      29            EFRDFT
045200140227     c                   end
045300140227     c                   when      mese = 11 or
045400140227     c                             mese = 04 or
045500140227     c                             mese = 06 or
045600140227     c                             mese = 09
045700140227     c                   move      30            EFRDFT
045800140227     c                   other
045900140227     c                   move      31            EFRDFT
046000140227     c                   endsl
046100140227      *
046200140303      * IMPORTO TOTALE DELLA RIGA A CREDITO O DEBITO
046300140305     c                   eval      EFRTPD = deb_cred
046400140305      *
046500140305     c                   if        EFRTPD = 'C'
046600140305     c                   z-sub     DPCAMO        EFRIMP
046700140305     c                   else
046800140227     c                   z-add     DPCAMO        EFRIMP
046900140305     c                   end
047000140305     c                   movel     '001'         efrGRP
047100140227      **
047200140227      *  imposta UDATE
047300140227     C                   move      *date         G02DAT
047400140227     C                   MOVE      *ZEROS        G02INV
047500140227     C                   MOVE      *BLANKS       G02ERR
047600140227     C                   CALL      'XSRDA8'
047700140227     C                   PARM                    WLBDAT
047800140227     C                   Z-ADD     G02INV        efrDTR
047900140227      *
048000140227      *
048100140227     c                   write     tnEFR000
048200140227      *
048300140227     c                   Endsr
048400060726      * ?_______________________________________________________________    */
048500060726     C     *INZSR        BEGSR
048600060726      * ?_______________________________________________________________    */
048700060726      *
048800060726     c     *ENTRY        PLIST
048900150423     C                   parm                    kpjba
049000060726      *
049100060726     c                   eval      partner = 'DPD'
049200160929     c                   open      TNEFR00F
049300060726      *
049400060726      *  imposta UDATE
049500060726     C                   move      *date         G02DAT
049600060726     C                   MOVE      *ZEROS        G02INV
049700060726     C                   MOVE      *BLANKS       G02ERR
049800060726     C                   CALL      'XSRDA8'
049900060726     C                   PARM                    WLBDAT
050000060726     C                   Z-ADD     G02INV        data_oggi         8 0
050100060726     C                   MOVE      data_oggi     day
050200060726      * Recupero data e ora
050300060726     C                   TIME                    W0140            14 0
050400060726     C                   MOVEl     W0140         ore
050500060726      * UDATE IN GGMMAAAA
050600060726     C                   MOVE      W0140         WDTGIO
050700060726      * UDATE IN AAAAMMGG
050800060726     C     *eur          MOVEL     WDTGIO        DATA_eur
050900060726     C     *iso          MOVEL     DATA_eur      dateu
051000060726     c                   seton                                        66
051100060726      *
051200140307     c                   write     testa
051300140307      *
051400060726     c                   Endsr
051500060911      * ?_______________________________________________________________    */
051600140312     c*==================================================================*
051700140312      * Manda un Msg x E-mail
051800140312     c*==================================================================*
051900140312     c     Invio_Mail    begsr
052000140312      *
052100140312     C*   Alert_mail : invio Mail a CEDalert@Brt.it                  *
052200140312     C* Inizializzo variabili
052300140312     C                   movel     *blanks       wrkEml          253
052400140312     C                   movel     *blanks       wrkMsg         5000
052500140312     C                   movel     *blanks       wrkOgg           44
052600140312      *
052700140312     C* Valorizzo i campi della e-m@ail - indirizzo
052800140312     C                   eval      wrkEml = CEDalert
052900140312     C                   eval      wrkOgg = 'IMPORTA dati CLEARING DPD'
053000140312     C                   eval      wrkMsg = 'Problemi durante l'' importazione-
053100140312     C                              dati Clearing DPD, controllare il file -
053200140312     C                             WFDPD1PC che ha dei records NON trasferiti -
053300140312     C                             sul sistema di Controllo fatture'
053400140312     C                   call(e)   'TRTCT00R2'
053500140312     C                   parm                    wrkEml
053600140312     C                   parm                    wrkOgg
053700140312     C                   parm                    wrkMsg
053800140312     C*
053900140312     C                   ENDSR
054000140312     ***********************************************************************
054100160929     C*
054200160929     ***********************************************************************
054300160929     C     DIROTTA       BEGSR
054400160929     C*
054500160929     C*  Trasforma le causali di RESO in DIROTTAMENTO se la spedizione
054600160929     C*  è stata anche CONSEGNATA con DELIVERY.
054700160929     C*
054800160929     C/EXEC SQL
054900160929     c+  update tnefr00f set
055000160929     c+  efrcau='D' concat substr(efrcau, 2, 2)
055100160929     c+  WHERE  efrntw ='DPD' and efrrif like '084%' and
055200160929     c+         substr(efrcau, 1, 1) ='R'            and
055300160929     c+         efrrif concat digits(efrdft)        in (
055400160929     c+  SELECT efrrif concat digits(efrdft)
055500160929     c+    FROM tnefr00f
055600160929     c+   WHERE efrntw='DPD' and EFRRFP like '%DELIVERY%'
055700160929     c+     and efrRFP not like '%ISLAND%' and efrrif in
055800160929     c+  ( select efrrif FROM tnefr00f WHERE efrntw='DPD'
055900160929     c+       and EFRRFP like '%SYSTEM-RETOURE%'))
056000160929     C/END-EXEC
056100160929     C*
056200160929     C                   ENDSR
056300160929     ***********************************************************************
