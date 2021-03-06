000100060726     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060726     H BNDDIR('QC2LE')
000300060727     H DECEDIT('0,') DATEDIT(*DMY.)
000400081202      * ?====================================================================== */
000500140227 * ?  *   Nuovo traduttore Files Clearing ICS
000600081202      * ?====================================================================== */
000700140227      *  via FTP - Da un UNICO FILE                                             *
000800140307      *             non ci sono pi� le causali e i raggruppamenti x il TNEFR    *
000900140307      *             ma una somma verticale di tutto.                            *
001000081202      * Il pgm crea:                                                            *
001100130717      *             TNEFR00F x DPD                                              *
001200081202      * ?====================================================================== */
001300081202     FWfDPD1PC  UF   E             DISK    rename(WFDPD1PC:WFDPDW)
001400060727     F                                     PREFIX(x)
001500060727      *
001600130717     FTNEFR00F  O  a E             DISK
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
004600060911     D  �11                    1     11  0
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
010400060726      * per controllare se almeno un record � stato importato sul VAB
010500081202     c                   clear                   traduttore        1 0
010600081202     c                   clear                   riga_generica     1
010700060726     c                   clear                   Almeno_Uno        1
010800060726
010900140312     c                   clear                   righe_rimaste     5 0
011000140312
011100060726      * Continua a leggere dal secondo record.
011200060726      *   Il primo � la testata gi� controllata dalla Check_trasm.
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
018200060726      **
018300060726     c                   SETON                                        LR
018400060726      * ?______________________________________________________________     */
018500060726      *?    Decodifica record a campi variabili
018600060726      * ?______________________________________________________________     */
018700140227     c     Decod_Record  Begsr
018800140227
018900140227      * ? Sposta il record a tracciato libero su un campo di lavoro       =
019000140303     c                   clear                   deb_cred          1
019100140227     c                   movel(p)  xWFDPD1PC     dati
019200140227     c                   eval      dati = %Trim(Dati) + ';'
019300140227     c                   exsr      Split_Rec
019400140227     c                   exsr      Rec_Dettaglio
019500140227      **
019600140227     c                   Endsr
019700081202      * ?_______________________________________________________________    */
019800081202      *?      Riga da controllare e tradurre
019900081202      * ?_______________________________________________________________    */
020000140227     c     Rec_Dettaglio Begsr
020100081202      **
020200081202      **   Controlli Formali sui campi: Obbligatoriet� e controllo Numerico
020300081202     c                   clear                   TIdpc0DS
020400081202     c                   clear                   Msg_Err
020500140227      **
020600140227      * ? Nel 1� campo : il Parcel Number                 (Obbl.)    Parcel Nr.
020700140227      **                                                             Parcel Nr.
020800140227     C                   z-add     1             sk
020900140227     c                   If        LunDato_sk(sk) =  0
021000140317     c                              or
021100140317     c                             LunDato_sk(sk) > 1 and
021200140317     c                             LunDato_sk(sk) < 14
021300140227      * ?-----> Errore
021400140227     c                   eval      tipo_error = 1
021500140227     c                   eval      Campo_errato = 'DPCPRN'
021600140227     c                   exsr      Field_Error
021700140227     c                   Else
021800140227     c                   eval      DPCPRN     = %subst(Dato_sk(sk):1:+
021900140227     c                                              LunDato_sk(sk))
022000140227     c                   End
022100140227      **
022200081202      **
022300140303      * ? Nel 2� campo : Data YYYY-MM                (Oblligatorio)  Data
022400081202      **                                                             Data
022500140227     C                   z-add     2             sk
022600140303     c                   If        LunDato_sk(sk) =  0
022700140317     c                              or
022800140317     c                             LunDato_sk(sk) > 1 and
022900140317     c                             LunDato_sk(sk) < 7
023000140317     c                              or
023100140317     c                             %subst(Dato_sk(sk):5:1) <>'-'
023200140303      * ?-----> Errore
023300140303     c                   eval      tipo_error = 1
023400140303     c                   eval      Campo_errato = 'DPCYMO'
023500140303     c                   exsr      Field_Error
023600140303     c                   Else
023700140303     c                   eval      DPCYMO     = %subst(Dato_sk(sk):1:+
023800140303     c                                              LunDato_sk(sk))
023900081202     c                   End
024000081202      **
024100140305      **
024200140305      * ? Nel 12� campo :                  PROCESS/CAUSALE
024300140305      **
024400140305     C                   z-add     12            sk
024500140311     c                   If        LunDato_sk(sk) >  0
024600140305     c                   eval      DPCPROC    = %subst(Dato_sk(sk):1:+
024700140305     c                                              LunDato_sk(sk))
024800140305     c                   End
024900140305      **
025000140305      **
025100140305      * ? Nel 13� campo :                  ELEMENT
025200140305      **
025300140305     C                   z-add     13            sk
025400140311     c                   If        LunDato_sk(sk) >  0
025500140305     c                   eval      DPCELEM    = %subst(Dato_sk(sk):1:+
025600140305     c                                              LunDato_sk(sk))
025700140305     c                   End
025800140305      **
025900140307      **
026000140307      * ? Nel 14� campo :                  RATE-KEY
026100140307      **
026200140307     C                   z-add     14            sk
026300140311     c                   If        LunDato_sk(sk) >  0
026400140307     c                   eval      DPCRAKE    = %subst(Dato_sk(sk):1:+
026500140307     c                                              LunDato_sk(sk))
026600140307     c                   End
026700140307      **
026800140305      **
026900140305      * ? Nel 15� campo :                  COD WEIGHT
027000140305      **
027100140305     C                   z-add     15            sk
027200140311     c                   If        LunDato_sk(sk) >  0
027300140305     c                   eval      DPCWGHT    = %subst(Dato_sk(sk):1:+
027400140305     c                                              LunDato_sk(sk))
027500140305     c                   End
027600140305      **
027700081202      **
027800140227      * ? Nel 21� campo : Importo                    (Oblligatorio)  Importo
027900140317      **
028000140303      **    OCCORRE CAPIRE SE UN CREDITO O DEBITO
028100140303      **     IL DEBITO � PRECEDUTO DAL (-)
028200140303     C                   z-add     21            sk
028300140317      **
028400140317     c                   If        LunDato_sk(sk) >  0
028500140303     c                   movel     'C'           deb_cred
028600140303      *
028700140303     c                   if        %subst(Dato_sk(sk):1:1) = '-'
028800140303     c                   movel     'D'           deb_cred
028900140303     c                   eval      Dato_sk(sk) = %subst(Dato_sk(sk):2:39)
029000140317     c                   eval      LunDato_sk(sk) = LunDato_sk(sk) - 1
029100140303     c                   end
029200140317      **
029300140317      **  TOGLIE il PUNTO e lo sostituisce
029400150515      ******* ATTENZIONE ATOF vuole il punto
029500150515     c***************     '.':','       xlate     Dato_sk(sk)   Dato_sk(sk)
029600140317      **
029700140317      **  se ci sono altri caratteri al posto dei numeri e della virgola
029800140317     c     numeri_ammessiCHECK     Dato_sk(sk)   N                 3 0
029900140317     c                   if        %Found
030000140317     c                              and N <= LunDato_sk(sk)
030100140317      * ?-----> Errore
030200140317     c                   eval      tipo_error = 2
030300140317     c                   eval      Campo_errato = 'DPCAMO'
030400140317     c                   exsr      Field_Error
030500140317     c                   else
030600140317      **
030700081202     c                   eval(H)   DPCAMO   = atof(%TRIM(Dato_sk(sk)))
030800140317     c                   end
030900140317     c                   end
031000081202      **
031100081202      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDPC00W
031200081202      **
031300081202     c                   Endsr
031400081202      * ?____________________________________________________________       */
031500081202      *?      Campi in errore
031600081202      * ?____________________________________________________________       */
031700081202     c     Field_Error   Begsr
031800081202      **
031900081202     C                   eval      se_errore   = 'S'
032000081202      * msg errore
032100081202     c                   If        Msg_Err = *blank
032200081202     c
032300081202     c                   Select
032400081202     c
032500081202      * msg errore : Obbligatorio
032600081202     c                   When      Tipo_Error = 1
032700081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
032800081202     C                             ' >> Obblig.- Manca Dato !!'
032900081202     c
033000081202      * msg errore : Obbligatorio / Numerico
033100081202     c                   When      Tipo_Error = 2
033200081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
033300081202     C                             ' >> Obblig.Mancante o NON Numerico !!'
033400081202     c
033500081202      * msg errore : Numerico
033600081202     c                   When      Tipo_Error = 3
033700081202     C                   eval      Msg_Err = 'Campo : ' + %trim(Campo_errato) +
033800081202     C                             ' >> NON Numerico  !!'
033900081202     c                   EndSL
034000081202     c
034100081202     c                   End
034200081202      **
034300081202     c                   Endsr
034400081202      * ?____________________________________________________________       */
034500081202      *?      Suddivide i campi della riga con carattere divisorio (;)
034600081202      * ?____________________________________________________________       */
034700081202     c     Split_Rec     Begsr
034800081202      **
034900081202     c                   clear                   Nr_campi
035000081202     c                   clear                   Campi_Record
035100081202     c                   clear                   Campi_Lunghi
035200081202     c                   clear                   Campi_Numerici
035300081202     c                   clear                   Campi_Decimali
035400081202     c                   movel     ';'           Separa_campi
035500081202     c                   movel     ','           Decimal_Separ
035600081202      *
035700081202     c                   call      'FIEU00R'
035800081202      * ?     Input
035900081202     c                   parm                    Separa_campi
036000081202     c                   parm                    Decimal_Separ
036100081202     c                   parm                    dati
036200081202      * ?     Output  su schiere di 100 elementi
036300081202     c                   parm                    Nr_campi
036400081202     c                   parm                    Campi_Record                   * i dati
036500081202     c                   parm                    Campi_Lunghi                   * lunghezza dati
036600081202     c                   parm                    Campi_Numerici                 * se numerici
036700081202     c                   parm                    Campi_Decimali                 * quanti decimali
036800081202      **
036900081202     c                   Endsr
037000140227      * ?_______________________________________________________________    */
037100140227     C*? ESEGUO SCRITTURA del File TNEFR00F
037200140227      * ?_______________________________________________________________    */
037300140227     C     WRT_TNEFR     BEGSR
037400140227      *
037500140227     c                   clear                   tnEFR000
037600140227      *
037700140227     c                   eval      EFRNTW = 'DPD'
037800140227      *
037900140227      *  Imposta i campi con i vari tipi di tracciato
038000140227     c                   movel     DPCPRN        EFRRIF
038100140305     c                   eval      EFRRFP = %trim(DPCproc) + ';' +
038200140305     c                                      %trim(DPCelem) + ';' +
038300140305     c                                      %trim(DPCwght)
038400140227      *
038500140227     c                   movel     DPCymo        EFRNFT
038600140227      *
038700150115      * Causale  "N01" chiodata una per tutte
038800150115     c                   movel     'N01'         EFRCAU
038900140314      *
039000150115      *  W31.5+ causale = 'N02'         FUORI MISURA
039100140314     c     'W31.5+'      SCAN      DPCwght
039200140314     c                   if        %FOUND
039300150115     c                   movel     'N02'         EFRCAU
039400140314     c                   end
039500140314     c
039600140314      *
039700150115      *  Pick   causale = 'N03'         RITIRO
039800140314     c     'Pick'        SCAN      DPCproc
039900140314     c                   if        %FOUND
040000150115     c                   if        efrCAU ='N01'
040100150115     c                   movel     'N03'         EFRCAU
040200140314     c                   else
040300150115     c                   movel     'N04'         EFRCAU
040400140314     c                   end
040500140314     c                   end
040600140314     c
040700140303      *
040800140227      *  La Data di fattura del clearing � una data generica di fine mese
040900140227     c                   move      DPCymo        mese              2 0
041000140227     c                   movel     DPCymo        anno              4 0
041100140303      *
041200140303     C                   EVAL      EFRDFT = (ANNO*10000)+(MESE*100)
041300140303      *
041400140227     c                   select
041500140227     c                   when      mese = 2
041600140227     c     anno          div       4             bise              2 0
041700140227     c                   mvr                     resto             3 0
041800140227     c                   if        resto > 0
041900140227     c                   move      28            EFRDFT
042000140227     c                   else
042100140227     c                   move      29            EFRDFT
042200140227     c                   end
042300140227     c                   when      mese = 11 or
042400140227     c                             mese = 04 or
042500140227     c                             mese = 06 or
042600140227     c                             mese = 09
042700140227     c                   move      30            EFRDFT
042800140227     c                   other
042900140227     c                   move      31            EFRDFT
043000140227     c                   endsl
043100140227      *
043200140303      * IMPORTO TOTALE DELLA RIGA A CREDITO O DEBITO
043300140305     c                   eval      EFRTPD = deb_cred
043400140305      *
043500140305     c                   if        EFRTPD = 'C'
043600140305     c                   z-sub     DPCAMO        EFRIMP
043700140305     c                   else
043800140227     c                   z-add     DPCAMO        EFRIMP
043900140305     c                   end
044000140305     c                   movel     '001'         efrGRP
044100140227      **
044200140227      *  imposta UDATE
044300140227     C                   move      *date         G02DAT
044400140227     C                   MOVE      *ZEROS        G02INV
044500140227     C                   MOVE      *BLANKS       G02ERR
044600140227     C                   CALL      'XSRDA8'
044700140227     C                   PARM                    WLBDAT
044800140227     C                   Z-ADD     G02INV        efrDTR
044900140227      *
045000140227      *
045100140227     c                   write     tnEFR000
045200140227      *
045300140227     c                   Endsr
045400060726      * ?_______________________________________________________________    */
045500060726     C     *INZSR        BEGSR
045600060726      * ?_______________________________________________________________    */
045700060726      *
045800060726     c     *ENTRY        PLIST
045900150423     C                   parm                    kpjba
046000060726      *
046100060726     c                   eval      partner = 'DPD'
046200060726      *
046300060726      *  imposta UDATE
046400060726     C                   move      *date         G02DAT
046500060726     C                   MOVE      *ZEROS        G02INV
046600060726     C                   MOVE      *BLANKS       G02ERR
046700060726     C                   CALL      'XSRDA8'
046800060726     C                   PARM                    WLBDAT
046900060726     C                   Z-ADD     G02INV        data_oggi         8 0
047000060726     C                   MOVE      data_oggi     day
047100060726      * Recupero data e ora
047200060726     C                   TIME                    W0140            14 0
047300060726     C                   MOVEl     W0140         ore
047400060726      * UDATE IN GGMMAAAA
047500060726     C                   MOVE      W0140         WDTGIO
047600060726      * UDATE IN AAAAMMGG
047700060726     C     *eur          MOVEL     WDTGIO        DATA_eur
047800060726     C     *iso          MOVEL     DATA_eur      dateu
047900060726     c                   seton                                        66
048000060726      *
048100140307     c                   write     testa
048200140307      *
048300060726     c                   Endsr
048400060911      * ?_______________________________________________________________    */
048500140312     c*==================================================================*
048600140312      * Manda un Msg x E-mail
048700140312     c*==================================================================*
048800140312     c     Invio_Mail    begsr
048900140312      *
049000140312     C*   Alert_mail : invio Mail a CEDalert@Brt.it                  *
049100140312     C* Inizializzo variabili
049200140312     C                   movel     *blanks       wrkEml          253
049300140312     C                   movel     *blanks       wrkMsg         5000
049400140312     C                   movel     *blanks       wrkOgg           44
049500140312      *
049600140312     C* Valorizzo i campi della e-m@ail - indirizzo
049700140312     C                   eval      wrkEml = CEDalert
049800140312     C                   eval      wrkOgg = 'IMPORTA dati CLEARING DPD'
049900140312     C                   eval      wrkMsg = 'Problemi durante l'' importazione-
050000140312     C                              dati Clearing DPD, controllare il file -
050100140312     C                             WFDPD1PC che ha dei records NON trasferiti -
050200140312     C                             sul sistema di Controllo fatture'
050300140312     C                   call(e)   'TRTCT00R2'
050400140312     C                   parm                    wrkEml
050500140312     C                   parm                    wrkOgg
050600140312     C                   parm                    wrkMsg
050700140312     C*
050800140312     C                   ENDSR
050900140312     ***********************************************************************
