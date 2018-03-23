000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600060808      *  Importa da FTP server i Collection request DPD                         *
000700060809      *  TRASCODIFICA : CRDATA02      IMPORT DPD nuovo parcel e depot           *
000800060612      **?************************************************************************
000900060612     Ftivin00r  uF   E             DISK    usropn
001000060809      *
001100060809     FFIDPO03L  uF   E           k DISK
001200060809     FFnORM01L  uF   E           k DISK
001300060809     FFnORF01L  iF a E           k DISK
001400061017     Ffiar401l  if   E           k disk    rename(FIAR4000:FIAR4001)
001500061017     Ffiar403l  if   E           k disk
001600060906     FTntbe01L  iF   E           k DISK
001700060808      * ?================================================================== */
001800060612     D Lung_Parcel     S              3  0
001900060620     D Parcel11        S             11
002000060710     D Parcel14        S             14
002100060809     D Tipo_error      S              1  0
002200080516     D Consegna_estero...
002300080516     D                 S              2    INZ('82')
002400060808      * ?================================================================== */
002500060808     D  data_Oggi      s              8  0 INZ(0)
002600060808     D  day            s              8  0 INZ(0)
002700060808     D  ore            s              6  0 INZ(0)
002800060808     D  w0140          s             14  0 INZ(0)
002900060809      *
003000060808     D WLBDAT          DS
003100060808     D  G02DAT                 1      8  0
003200060808     D  G02INV                 9     16  0
003300060808     D  G02ERR                17     17
003400060808     D  G02TGI                18     22  0
003500060906     D x               s              3s 0
003600060906     D mancod          s              2
003700060906     D manfase         s              3s 0
003800060906     D manb            s              2    dim(50)
003900060906     D mand            s              3s 0 dim(50)
004000060906     D manf            s              3s 0 dim(50)
004100170228      *-------------------
004200170228     d dfar          e ds
004300170228      *-------------------
004400060906     D dmrd          E DS
004500061017     D dsbl4I        E DS
004600070612     D dsbl4M        E DS
004700060809      * ?   *--------------------------------------------------------------*
004800060809      * ?   ( Descrizione Campi x scomposizione FLAT record )
004900060809      * ?   *--------------------------------------------------------------*
005000060809     D FIDPODTA2     e DS
005100061002     D     Zcode5             61     63  0
005200061002     D     Zcode4             64     66  0
005300060809     D     Zcode3             67     69  0
005400061002     D     Zcode2             70     72  0
005500061002     D     Zcode1             73     75  0
005600080404     D     Zcod               61     75  0 DIM(5)
005700080404      *
005800080404      * ?( add.code ==> [82] Parcel in più )
005900080404     D Parcel_inPIU    s              3s 0 inz(82)
006000080404      *
006100080423      * ?   *--------------------------------------------------------------*
006200080423      *         per riapertura automatica dell'ORM  --> FIOR51R
006300080423      * ?   *--------------------------------------------------------------*
006400080423     D FIOR51DS      e DS
006500060612      * ?================================================================== */
006600170102     d FIEU39DS      e DS
006700170102
006800060809      * ?   * Campi da tradurre con interi
006900060809     D atoll           PR            20I 0
007000060809     D                                     EXTPROC('atoll')
007100060809     D  Char                           *
007200060809     D                                     VALUE
007300060809     D                                     OPTIONS(*STRING)
007400060612      * ?   * Campi da tradurre con interi
007500060612     D atoi            PR            10I 0
007600060612     D                                     EXTPROC('atoi')
007700060612     D  Char                           *
007800060612     D                                     VALUE
007900060612     D                                     OPTIONS(*STRING)
008000060612      * ?   * Campi da tradurre con decimali
008100060612     D atof            PR             8F
008200060612     D                                     EXTPROC('atof')
008300060612     D  Char                           *
008400060612     D                                     VALUE
008500060612     D                                     OPTIONS(*STRING)
008600060612      * ?   *--------------------------------------------------------------*
008700060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
008800060612      * ?   *--------------------------------------------------------------*
008900060612      *
009000060612      * ?   * Campi x decodifica * (INPUT  del Record)
009100060612     D  Dati           s           1500
009200060612     D Separa_campi    s              1
009300060612     D Decimal_Separ   s              1
009400060809      *
009500060612      * ?   * Campi decodificati * (OUTPUT del Record)
009600060612      *       Schiere di Output / Campi di Output
009700060614     D  Alfa40         s             40
009800060612     d  Sk             s              3u 0
009900060612     D    NR_campi     s              3u 0
010000060612     D  Campi_Record   ds
010100060612     D    Dato_sk                    40    DIM(100)
010200060612     D  Campi_Lunghi   ds
010300060612     D    LunDato_sk                  3u 0 DIM(100)
010400060612     D Campi_Numerici  ds
010500060612     D    DatoNum_sk                  1    DIM(100)
010600060612     D Campi_Decimali  ds
010700060612     D    Decimal_sk                  3u 0 DIM(100)
010800060612      * ?   *--------------------------------------------------------------*
010900060612     D  position       s              3  0 INZ(0)
011000060612
011100060612     D  se_errore      s              1    inz(' ')
011200060612     D  Msg_Err        s            132    inz(' ')
011300060614     D  Msg_Vin_80     s             80    inz(' ')
011400060612      * ?* ------------------------------------------------------ *
011500060612      * Records di controllo mandati nell'IGATEAVIS
011600060612      * ?   * Dati Record UNO *
011700060612     DDepot_Trs_UNO    s             10
011800060612     D User_Trs_UNO    s             10
011900060612     D Data_Trs_UNO    s              8  0
012000060612     D Time_Trs_UNO    s              6  0
012100060612     D  Num_Trs_UNO    s              9  0
012200060809      * ?   * Dati Record DUE *
012300060809     D Vers_Trs_DUE    s             15
012400060809      * ?   * Dati Record TRE *
012500060612     D Testa_Sk        s             10    DIM(50)
012600060809      * ?   * Dati Record QUATTRO *
012700060612     D  Num_Trs_FIN    s              9  0
012800060710      **
012900060710     D Digits          C                   '0123456789'
013000060612      * ?================================================================== */
013100060612      *   Ciclo principale
013200060413      * ?********************************************************************/
013300060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
013400060612     C*
013500060612     C                   if        not %open(tivin00r)
013600060612     C                   open      tivin00r
013700060612     C                   endif
013800060612      **
013900060612      * ?------------------------------------------------------------------ */
014000060612     C*? Controllo dati arrivati da DPD
014100060612      * ?------------------------------------------------------------------ */
014200060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
014300060612      * ?- controllando che la trasmissione sia completa.
014400060612
014500060612      * ?              /*---------------------- */
014600060612     c                   exsr      check_Trasm
014700060612      * ?              /*---------------------- */
014800060612      **
014900060613      **  Errore di trasmissione x tutti i records
015000060613      **   --> file in errore
015100060613     c                   if        se_errore = 'S'
015200060612
015300060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
015400060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
015500060613     C                               >> Farsi rimandare i DATI !!'
015600060612
015700060612      * ?  Scrive su tutti i records il tipo di errore
015800060612     c     *start        setll     tivin00r
015900060612     c                   read      tivin00r
016000060612     c                   dow       not %eof(tivin00r)
016100060612     C                   eval      vinMSG = Msg_Err
016200060612     C                   eval      vinFLG = '2'
016300060613     c                   eval      esito  = '2'
016400060612     c                   update    tivin000
016500060612     c                   read      tivin00r
016600060612     c                   endDO
016700060612      **
016800060612     c                   else
016900060614      **
017000060612      * ?------------------------------------------------------------------ */
017100060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
017200060612      * ?------------------------------------------------------------------ */
017300060612      * ?              /*---------------------- */
017400060612     c                   exsr      Importa_Msg
017500060612      * ?              /*---------------------- */
017600060612
017700060614     c                   end
017800060614
017900060612     C                   if        %open(tivin00r)
018000060612     C                   close     tivin00r
018100060612     C                   endif
018200060612      *
018300060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
018400060614      *
018500060710     c                   if        almeno_uno ='S' and esito ='1'
018600060710     C                   eval      esito ='0'
018700060614     C                   endif
018800170102
018900170102     c                   eval      i39tla='C'
019000170102     c                   call      'FIEU39R'
019100170102     c                   parm                    fieu39ds
019200060614      *
019300060612     c                   SETON                                        LR
019400060612      * ?================================================================== */
019500060612      *? Controlla la trasmissione   se completa
019600060612      * ?------------------------------------------------------------------ */
019700060612     c     Check_Trasm   Begsr
019800060612
019900060612     C                   clear                   se_errore
020000060612      ** primo  record
020100060612     c     *start        setll     tivin00r
020200060612     c                   read      tivin00r
020300060612     c                   if        not %eof(tivin00r)
020400060612      * ?              /*---------------------- */
020500060612     c                   exsr      Decod_Record
020600060612      * ?              /*---------------------- */
020700060612     c                   endif
020800060612      **
020900060612      ** ultimo record
021000060612     c     *hival        setll     tivin00r
021100060612     c                   readp     tivin00r
021200060612     c                   if        not %eof(tivin00r)
021300060612      * ?              /*---------------------- */
021400060612     c                   exsr      Decod_Record
021500060612      * ?              /*---------------------- */
021600060612     c                   endif
021700060612
021800060612      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
021900060612      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
022000060612      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
022100060612     C                   if        num_trs_UNO <> num_trs_FIN
022200060612      * ?-----> Errore
022300060612     C                   eval      se_errore = 'S'
022400060612
022500060612     c                   end
022600060612
022700060612     c                   endSR
022800060612      * ?------------------------------------------------------------------ */
022900060612      *? Importa i records della tramsissione
023000060612      * ?------------------------------------------------------------------ */
023100060612     c     Importa_Msg   Begsr
023200060612
023300060614      * per controllare se almeno un record è stato importato sul VAB
023400060614     c                   clear                   Almeno_Uno        1
023500060614
023600060612     c     *start        setll     Tivin00r
023700060612     c                   read      Tivin00r
023800060612
023900060612     c                   dow       not %eof(Tivin00r)
024000060614
024100060614      * solo i record sflaggati da rielaborare
024200060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
024300060612
024400060612      ** Controlli formali sui campi
024500060612     c                   clear                   se_errore
024600060620     C                   clear                   Msg_Vin_80
024700060612
024800060612      ** Decodifica record a campi variabili
024900060612      * ?              /*---------------------- */
025000060612     c                   exsr      Decod_Record
025100060612      * ?              /*---------------------- */
025200060612
025300060612      **  Se presente un errore nel record emette una segnalazione msg
025400060612     c                   if        se_errore ='S'
025500060614     C                   eval      vinMSG = Msg_Vin_80
025600060621     c                   end
025700060612
025800060809      *  con il record in canna scrive direttamente il VAO
025900060620     c                   if        %subst(dati:1:1) <> '#'
026000060612      * ?              /*---------------------- */
026100060809     c                   exsr      WRT_FILE
026200060612      * ?              /*---------------------- */
026300060620     c                   end
026400060614
026500060809      *  Problemi nella decodifica dei campi VAO
026600060614     c                   if        se_errore ='S'
026700060614     C                   eval      vinMSG = Msg_Vin_80
026800060614     c                   end
026900060612
027000060621      *  Sempre Record OK
027100060621     C                   eval      vinFLG = '1'
027200060710     c                   eval      esito  = '0'
027300060621
027400060809      *  x errore bloccante nella composizione del VAO
027500060621     c                   if        err_bloccante ='S'
027600060621     C                   eval      vinFLG = '2'
027700060710     c                   eval      esito  = '1'
027800060621     c                   end
027900060612
028000060612     c                   update    Tivin000
028100060614     c                   endIF
028200060612
028300060612     c                   read      Tivin00r
028400060612     C                   ENDdo
028500060612      **
028600060612     c                   endSR
028700060612      * ?------------------------------------------------------------------ */
028800060612      *?    Decodifica record a campi variabili
028900060612      * ?------------------------------------------------------------------ */
029000060612     c     Decod_Record  Begsr
029100060612
029200060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
029300060612     c                   movel(p)  VINDTA        dati
029400060612      **
029500060612      * ?              /*---------------------- */
029600060612     c                   exsr      Split_Rec
029700060612      * ?              /*---------------------- */
029800060612      **
029900060612      **  Records di Intestazione e di fine Msg
030000060612     c                   if        %subst(dati:1:1) ='#'
030100060612      * ?              /*---------------------- */
030200060612     c                   exsr      Rec_Speciali
030300060612      * ?              /*---------------------- */
030400060612     c                   Else
030500060612      ** conta le righe
030600060612      * ?              /*---------------------- */
030700060612     c                   exsr      Rec_Dettaglio
030800060612      * ?              /*---------------------- */
030900060612     c                   End
031000060612      **
031100060612     c                   Endsr
031200060612      * ?------------------------------------------------------------------ */
031300060612      *?      Record di Testata e di Fine Msg
031400060612      * ?------------------------------------------------------------------ */
031500060612     c     Rec_Speciali  Begsr
031600060612      **
031700060612      * ?  Prima riga
031800060612     c     'FILE'        SCAN      dati:2        position
031900060612     c                   if        %Found and position < 5
032000060612     c                   exsr      Rec_UNO
032100060612     c                   ElSe
032200060612     c                   End
032300060612      **
032400060612      * ?  Seconda riga (testate dei campi)
032500060809     c     'Version'     SCAN      dati:2        position
032600060809     c                   if        %Found and position < 5
032700060809     c                   exsr      Rec_DUE
032800060809     c                   End
032900060809      **
033000060809      * ?  Terza riga (testate dei campi)
033100060612     c     'DEF'         SCAN      dati:2        position
033200060612     c                   if        %Found and position < 5
033300060809     c                   exsr      Rec_TRE
033400060612     c                   End
033500060612      **
033600060612      * ?  Ultima riga
033700060612     c     'END'         SCAN      dati:2        position
033800060612     c                   if        %Found and position < 5
033900060612     c                   exsr      Rec_LAST
034000060612     c                   End
034100060612      **
034200060612     c                   Endsr
034300060612      * ?------------------------------------------------------------------ */
034400060612      *?      Primo Record
034500060612      * ?------------------------------------------------------------------ */
034600060612     c     Rec_UNO       Begsr
034700060612      **
034800060612      ** esempio di record :
034900060808      * ?   # FILE;N00;depot945;20050603;143526;6;
035000060612      **
035100060612      **  Inizializza i campi della trasmissione
035200060612     C                   eval      Depot_Trs_UNO = *blank
035300060612     C                   eval      User_Trs_UNO  = *blank
035400060612     C                   eval      Data_Trs_UNO  = 0
035500060612     C                   eval      Time_Trs_UNO  = 0
035600060612     C                   eval      Num_Trs_UNO   = 0
035700060612
035800060612      **   Memorizza il numero della trasm. da confrontare con l'ultimo record.
035900060612      **     Tutto il dettaglio deve essere compreso entro questi 2 records
036000060612      **    altrimenti il msg.non è completo e bisogna farsi rimandare il file.
036100060612
036200060808      * ? Nel 2° campo di testata c'è lo USER in Delis
036300060808     C                   z-add     2             sk
036400060808     c                   eval      User_Trs_UNO = %subst(Dato_sk(sk):1:+
036500060808     c                             LunDato_sk(sk))
036600060808
036700060808      * ? Nel 3° campo di testata c'è il Dispatch Depot
036800060808     C                   z-add     3             sk
036900060808     c                   eval      Depot_Trs_UNO = %subst(Dato_sk(sk):1:+
037000060808     c                             LunDato_sk(sk))
037100060808
037200060808      * ? Nel 4° campo di testata c'è la data trasmissione
037300060808     C                   z-add     4             sk
037400060808     c                   if        DatoNum_sk(sk) = 'S'
037500060808     c                   eval      Data_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
037600060808     c                   End
037700060808
037800060808      * ? Nel 5° campo di testata c'è il nr.seriale di trasmissione
037900060808     C                   z-add     5             sk
038000060808     c                   if        DatoNum_sk(sk) = 'S'
038100060808     c                   eval      Time_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
038200060808     c                   End
038300060808
038400060808      * ? Nel 6° campo di testata c'è il nr.seriale di trasmissione
038500060808     C                   z-add     6             sk
038600060808     c                   if        DatoNum_sk(sk) = 'S'
038700060808     c                   eval      num_trs_UNO = atoi(%TRIM(Dato_sk(sk)))
038800060808     c                   End
038900060612      **
039000060612     c                   Endsr
039100060809      * ?------------------------------------------------------------------ */
039200060809      *?      Secondo Record
039300060809      * ?------------------------------------------------------------------ */
039400060809     c     Rec_DUE       Begsr
039500060809      **
039600060809      ** esempio di record :
039700060809      * ?   # Version;JR20040831;
039800060809      **
039900060809      **  Inizializza i campi della versione trasmissione
040000060809     C                   eval      Vers_Trs_DUE = *blank
040100060809      **
040200060809      **   Memorizza la versione i trasm.
040300060809
040400060809      * ? Nel 2° campo di testata c'è lo USER in Delis
040500060809     C                   z-add     2             sk
040600060809     c                   eval      Vers_Trs_DUE = %subst(Dato_sk(sk):1:+
040700060809     c                             LunDato_sk(sk))
040800060809      **
040900060809     c                   Endsr
041000060612      * ?------------------------------------------------------------------ */
041100060809      *?      Terzo   Record
041200060612      * ?------------------------------------------------------------------ */
041300060809     c     Rec_TRE       Begsr
041400060612      **
041500060612      ** esempio di record :
041600060808      * ?   # DEF;CR:CRDATA01;ODEPOT;ORDERNR;CDEPOT;OCUSTNO;ONAME;ONAME2;OS...
041700060612      **
041800060612      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
041900060612     C                   clear                   Testa_Sk
042000060612
042100060612      * ? Dal 3° campo troviamo i nomi delle intestazioni dei campi C.Request
042200060612     C     3             do        50            sk
042300060612     c                   eval      Testa_Sk(sk-2) = %subst(Dato_sk(sk):1:+
042400060612     c                             LunDato_sk(sk))
042500060612     c                   enddo
042600060612      **
042700060612     c                   Endsr
042800060612      * ?------------------------------------------------------------------ */
042900060612      *?      Ultimo  Record
043000060612      * ?------------------------------------------------------------------ */
043100060612     c     Rec_LAST      Begsr
043200060612      **
043300060612      ** esempio di record :
043400060808      * ?   # END;6;
043500060612      **
043600060612      **  Inizializza i campi della trasmissione
043700060612     C                   eval      Num_Trs_fin = 0
043800060612      **
043900060612      **  Nell'ultimo record di chiusura deve esserci lo stesso numero di trasmissione
044000060612      **  impostato nel primo record.
044100060612      **
044200060612      * ? Nel 2° campo di fine trsm. c'è il nr.seriale di trasmissione
044300060612     C                   z-add     2             sk
044400060612     c                   if        DatoNum_sk(sk) = 'S'
044500060612     c                   eval      num_trs_fin = atoi(%TRIM(Dato_sk(sk)))
044600060612     c                   End
044700060612      **
044800060612     c                   Endsr
044900060612      * ?------------------------------------------------------------------ */
045000060612      *?      Riga Bolla Import da controllare e tradurre
045100060612      * ?------------------------------------------------------------------ */
045200060612     c     Rec_Dettaglio Begsr
045300060612      **
045400060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
045500060612      **
045600060809     c                   clear                   fidpoDTA2
045700060614     c                   clear                   Msg_Err
045800060809      **
045900060809      * ? Nel 1° campo : Numero Requesting Depot     (Oblligatorio)  ODEPOT
046000060809      **                                                             DEPOT - ORDINANTE
046100060809     C                   z-add     1             sk
046200060809     c                   If        LunDato_sk(sk) =  0
046300060809      * ?-----> Errore
046400060809     C                   eval      tipo_error = 1
046500060809     C                   exsr      Field_Error
046600060809     c                   Else
046700060809     c                   eval      ODEPOT = %subst(Dato_sk(sk):1:+
046800060809     c                                              LunDato_sk(sk))
046900060809     c                   End
047000060809      **
047100060809      * ? Nel 2° campo : Numero Ordine DPD           (Oblligatorio)  ORDERNR
047200060809      **   parte 2 della chiave ORM DPD                              NUM.ORDINE
047300060809     C                   add       1             sk
047400060809     c                   If        DatoNum_sk(sk) = 'N' or
047500060809     c                             LunDato_sk(sk) =  0
047600060809      * ?-----> Errore
047700060809     C                   eval      tipo_error = 2
047800060809     C                   exsr      Field_Error
047900060809     c                   Else
048000060809     c                   eval      ORDERNR= atoi(%TRIM(Dato_sk(sk)))
048100060809     c                   End
048200060809      **
048300060809      * ? Nel 3° campo : Depot Che deve eseguire ORM                 CDEPOT
048400060809      **                                                             DEPOT DI RITIRO
048500060809     C                   add       1             sk
048600060809     c                   If        LunDato_sk(sk) =  0
048700060809      * ?-----> Errore
048800060809     C                   eval      tipo_error = 1
048900060809     C                   exsr      Field_Error
049000060809     c                   Else
049100060809     c                   eval      RDEPOT = %subst(Dato_sk(sk):1:+
049200060809     c                                              LunDato_sk(sk))
049300060809     c                   End
049400060809      **
049500060809      * ? Nel 4° campo : Parcel Number                               PKNR
049600060809      **                                                             PARCEL NR
049700060809     C                   add       1             sk
049800060809     c                   If        LunDato_sk(sk) =  0
049900060809      * ?-----> Errore
050000060809     C                   eval      tipo_error = 1
050100060809     C                   exsr      Field_Error
050200060809     c                   Else
050300060809     c                   eval      PKNR   = %subst(Dato_sk(sk):1:+
050400060809     c                                              LunDato_sk(sk))
050500060809      * ?   ( Il parcel non è + numerico )   controlla lunghezza del Parcel
050600060809     c                   clear                   Lung_Parcel
050700060809     c                   eval      Lung_Parcel = %Len(%trim(PKNR))
050800060809     c                   if        Lung_Parcel = 11
050900060809     C                   MOVEL     PKNR          parcel11
051000060809     c                   end
051100060809     c                   if        Lung_Parcel = 14
051200060809     C                   MOVEL     PKNR          parcel14
051300060809     c                   end
051400060809      **
051500060809     c                   End
051600090227      **
051700090227      * ? Nel 5° campo : Data Creazione Ordine                       NEUDAT
051800090227      **                                                             DATA CREAZIONE ORDINE
051900090227     C                   add       1             sk
052000090227     c                   If        DatoNum_sk(sk) = 'N' and
052100090227     c                             LunDato_sk(sk) >  0
052200090227      * ?-----> Errore
052300090227     C                   eval      tipo_error = 3
052400090227     C                   exsr      Field_Error
052500090227     c                   Else
052600090227     c                   eval      NEUDAT  =atoi(%TRIM(Dato_sk(sk)))
052700090227     c                   End
052800060809      **
052900090227      * ? Nel 6° campo : Data Ritiro                                 DATPCK
053000060809      **                                                             DATA RITIRO
053100060809     C                   add       1             sk
053200060809     c                   If        DatoNum_sk(sk) = 'N' and
053300060809     c                             LunDato_sk(sk) >  0
053400060809      * ?-----> Errore
053500060809     C                   eval      tipo_error = 3
053600060809     C                   exsr      Field_Error
053700060809     c                   Else
053800060809     c                   eval      DATPCK  =atoi(%TRIM(Dato_sk(sk)))
053900060809     c                   End
054000060809      **
054100060809      * ? Nel 7° campo : STATUS                      (Oblligatorio)  STATUS
054200060809      **                                                             status 1-2-3-4-5
054300060809     C                   add       1             sk
054400060809     c                   If        LunDato_sk(sk) =  0
054500060809      * ?-----> Errore
054600060809     C                   eval      tipo_error = 1
054700060809     C                   exsr      Field_Error
054800060809     c                   Else
054900060809     c                   eval      STATUS = %subst(Dato_sk(sk):1:+
055000060809     c                                              LunDato_sk(sk))
055100060809     c                   End
055200060809      **
055300060809      * ? Nel 8° campo : Peso (Con decimali)                         WEIGHT
055400060809      **                                                             PESO IN KG
055500060809     C                   add       1             sk
055600060809     c                   If        DatoNum_sk(sk) = 'N' and
055700060809     c                             LunDato_sk(sk) >  0
055800060809      * ?-----> Errore
055900060809     C                   eval      tipo_error = 3
056000060809     C                   exsr      Field_Error
056100060809     c                   Else
056200060809     c                   eval      WEIGHT = atof(%TRIM(Dato_sk(sk)))
056300060809     c                   End
056400060809      **
056500060809      * ? Nel 9° campo : SERVICE CODE                                SERVICE
056600060809      **                                                             SERVICE CODE
056700060809     C                   add       1             sk
056800060809     c                   If        DatoNum_sk(sk) = 'N' and
056900060809     c                             LunDato_sk(sk) >  0
057000060809      * ?-----> Errore
057100060809     C                   eval      tipo_error = 3
057200060809     C                   exsr      Field_Error
057300060809     c                   Else
057400060809     c                   eval      SERVICE= atoi(%TRIM(Dato_sk(sk)))
057500060809     c                   End
057600060809      **
057700060809      * ? Nel 10° campo : CUSTOM  status code                        CUSTOM
057800060809      **                                                             CUSTOM S.CODE
057900060809     C                   add       1             sk
058000060809     c                   If        LunDato_sk(sk) >  0
058100060809      * ?-----> Errore
058200060809     c                   eval      CUSTOM = %subst(Dato_sk(sk):1:+
058300060809     c                                              LunDato_sk(sk))
058400060809     c                   End
058500060809      **
058600060809      * ? Nel 11° campo : Codice Scan                                SCANT
058700060809      **                                                             SCAN
058800060809     C                   add       1             sk
058900060809     c                   If        DatoNum_sk(sk) = 'N' and
059000060809     c                             LunDato_sk(sk) >  0
059100060809      * ?-----> Errore
059200060809     C                   eval      tipo_error = 3
059300060809     C                   exsr      Field_Error
059400060809     c                   Else
059500060809     c                   eval      SCANT  = atoi(%TRIM(Dato_sk(sk)))
059600060809     c                   End
059700060809      **
059800060809      * ? Nel 12° campo : 5 Codici Addizionali                       ZCODE
059900060809      **                                                             COD.AGGIUNTIVI
060000060809     C                   add       1             sk
060100060809     c                   If        DatoNum_sk(sk) = 'N' and
060200060809     c                             LunDato_sk(sk) >  0
060300060809      * ?-----> Errore
060400060809     C                   eval      tipo_error = 3
060500060809     C                   exsr      Field_Error
060600060809     c                   Else
060700060809     c                   eval      ZCODE  = atoi(%TRIM(Dato_sk(sk)))
060800060809     c                   End
060900060808      **
061000060809      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL FIDPODTA2
061100060612      **
061200060612     c                   Endsr
061300060614      * ?------------------------------------------------------------------ */
061400060614      *?      Campi in errore
061500060614      * ?------------------------------------------------------------------ */
061600060614     c     Field_Error   Begsr
061700060614      **
061800060614     C                   eval      se_errore   = 'S'
061900060614      * msg video o in posta
062000060614     c                   If        Msg_Err = *blank
062100060614     c
062200060614     c                   Select
062300060614     c
062400060614     c                   When      Tipo_Error = 1
062500060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
062600060614     C                             ' >> Obblig.- Manca Dato !!'
062700060614     c
062800060614     c                   When      Tipo_Error = 2
062900060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
063000060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
063100060614     c
063200060614     c                   When      Tipo_Error = 3
063300060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
063400060614     C                             ' >> NON Numerico  !!'
063500060614     c
063600060614     c                   When      Tipo_Error = 4
063700060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
063800060614     C                             ' >> Conversione non riuscita !!'
063900060614     c                   EndSL
064000060614     c
064100060614     c                   End
064200060614      * Errore su VINMSG
064300060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
064400060614     c                                          + %trim(Testa_sk(sk))
064500060614      **
064600060614     c                   Endsr
064700060808      * ?------------------------------------------------------------------ */
064800060808      *?    Scrive i Files degli ORM
064900060808      * ?------------------------------------------------------------------ */
065000060809     c     WRT_FILE      Begsr
065100060809      **
065200060809      **  Se è in questo punto significa che non sono stati trovati errori
065300060809      **    formali quindi si passa gli status per avanzamento ORM
065400060809      **?              /*---------------------- */
065500060809     c                   ExSr      Avanza_ORM
065600060809      * ?              /*---------------------- */
065700060808      **
065800060809     c                   endSR
065900060809      * ?------------------------------------------------------------------ */
066000060809      *?    Imposta i campi per il VAO
066100060809      * ?------------------------------------------------------------------ */
066200060809     c     Avanza_ORM    Begsr
066300060809      **
066400060809     c                   clear                   err_bloccante     1
066500080605     c                   clear                   new_ormFAO        3 0
066600060906      * ?---------------------------------  **
066700060906      *?      ATTENZIONE: DA COMPLETARE.
066800060906      *?      COME GESTIRE I NUOVI STATUS
066900060906      *?       e GLI SCAN + COD.ADD.
067000060906      * ?---------------------------------  **
067100060906      **  SCAN
067200060906      **   SCAN + cod.addizionale
067300060906     c                   if        SCANT > 0
067400080404      **  ZCODE
067500061109     c                   clear                   mancod
067600061109     c                   clear                   manfase
067700080404     c                   clear                   ZcodeX            3 0
067800080404      *
067900080404     c                   if        ZCODE > 0
068000080404      *
068100080404      * ?/* subito controlla la presenza di un Parcel eccedente
068200080404      * ?   indicato con -->(82) */
068300080404     c                   z-add     1             x
068400080404     c     Parcel_inPIU  lookup    zcod(x)                                66
068500080404     c                   if        %Equal
068600080404     c                   z-add     Parcel_inPIU  zcode1
068700080404     c                   end
068800080404      *
068900080404      ** prendo in considerazione solo il primo valido
069000060906     c                   z-add     1             x
069100080404     c                   if        Zcode1 > 0
069200060906     c     zcode1        lookup    mand(x)                                66
069300080404     c   66              z-add     zcode1        ZcodeX
069400080404     c                   elseIf    Zcode2 > 0
069500080404     c     zcode2        lookup    mand(x)                                66
069600080404     c   66              z-add     zcode2        ZcodeX
069700080404     c                   elseIf    Zcode3 > 0
069800080404     c     zcode3        lookup    mand(x)                                66
069900080404     c   66              z-add     zcode3        ZcodeX
070000080404     c                   elseIf    Zcode4 > 0
070100080404     c     zcode4        lookup    mand(x)                                66
070200080404     c   66              z-add     zcode4        ZcodeX
070300080404     c                   elseIf    Zcode5 > 0
070400080404     c     zcode5        lookup    mand(x)                                66
070500080404     c   66              z-add     zcode5        ZcodeX
070600080404     c                   Endif
070700080404      *
070800080404     c                   if        %Equal
070900060906     c                   movel     manb(x)       mancod
071000060906     c                   movel     manf(x)       manfase
071100061109     c                   End
071200060906      **
071300060906     c                   End
071400060906      **
071500060906     c                   End
071600060920      * ?---------------------------------  **
071700060920      **  x DPD blank sullo status significa (1) (detto da DPD)
071800060920     c                   if        STATUS = *blank
071900060920     c                   move      '1'           STATUS
072000060920     c                   End
072100060920      **
072200060921      * ?- Aggancia il record del legame purchè non sia stato aggiornato
072300060921      * ?-  dall'altro programma con un Mancato Ritiro - SCAN 18.
072400060920     c     KDPO3         chain     fidpo03l
072500060920     c                   if        %Found(fidpo03l) and dpoSTS <> 8
072600080530     c                   clear                   DPOCODER
072700060920      **
072800060921      * ?- Controlla sull'ORM la Fase di avanzamento
072900060920     c     KOrM1         chain     fnOrm01l
073000060920      ** se addirittura non esiste il file  (Veramente Strano)
073100060920      **   imposta l'errore --> '10' x ricordare di non aver trovato l'ORM Bartolini
073200060920     c                   if        not %Found(fnorm01l)
073300060920     c                   movel     '10'          DPOCODER
073400060920     c                   else
073500060920      **
073600060921      * ?Controllo Fase ORM e/o aggiorna ORM Bartolini a seconda dello stato ricevuto
073700060920     c                   select
073800060920      **
073900060920      *?      Abbinato Parcel a Ordine DPD
074000060920     c                   when      STATUS = '1'
074100060920
074200060920     c                   if        ORmFAO <= 300
074300060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
074400060920     c                   z-add     400           ormFAO
074500060921     c                   exsr      aggior_fase
074600060921      *
074700060920     c                   else
074800061017      *
074900061017      * ?- Attenzione: occorre controllare se c'è stato un problema di ritardo dell'informazione
075000061017      * ?  di stato ricevuta in ritardo rispetto al pacco che può essere già arrivato come bolla
075100061017      * ?  import e quindi l'ORM può trovarsi già chiuso.
075200061017      * ? MA TUTTO è già stato trattato operativamente in modo corretto (NON DEVE SEGNALARE NULLA)
075300061017      * ?altrimenti
075400061017      * ? SE COSI'non fosse, occorre segnalare l'errore x essere preso in considerazione.
075500061017     c                   if        ORmFAO >=900
075600080529      *
075700080529      * comunque deve far vedere che è arrivato un legame x un Annullato
075800080529     c                   If        dpoANN <> *blank
075900080529     c                   clear                   dpoANN
076000080529     c                   end
076100061017      *
076200061017     C* aggancia l'ORM legato alla Bolla
076300061017     c                   eval      ar4trc ='M'
076400061017     c                   move      dpoPOE        dpoPOEa           3
076500061017     c                   move      dpoNRS        dpoNRSa           2
076600061017     c                   move      dpoNOR        dpoNORa           7
076700061017     c                   move      dpoNRV        dpoNRVa           2
076800070612     c****               eval      ar4n30 = %trim(dpopoeA) + %trim(dponrsA) +
076900070612     c****                                  %trim(dponorA) + %trim(dponrvA)
077000070612     c                   move      dpopoeA       §B4POE
077100070612     c                   move      dponrsA       §B4NSR
077200070612     c                   move      dponorA       §B4NOR
077300070612     c                   move      dponrvA       §B4NRV
077400070612     c                   eval      ar4n30 = dsbl4m
077500070613
077600070613     c                   move      'N'           Sped_xOrm         1
077700061017     C     Kar43         chain     fiar403l
077800070613      *? x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
077900070613     c                   dow       not %Eof(fiar403l)
078000070613     c                   movel     AR4NOT        dsbl4M
078100070613     c                   if        §B4CLG = 'S'
078200070613     c                   move      'S'           Sped_xOrm         1
078300070613     c                   leave
078400070613     c                   end
078500070613     C     Kar43         reade     fiar403l
078600070613     c                   enddo
078700070613      ***
078800061017      * ?Se non trovato l'ORM legato ad una bolla invio l'errore
078900070613     c*********          if        not %Found(fiar403l)
079000070613     c                   if        Sped_xOrm = 'N'
079100061017     c                   movel     '01'          DPOCODER
079200061017     c                   else
079300061017      * ?se trovato, altro tipo di controllo sul Parcel:
079400061017      * ? La bolla generata potrebbe essere stata associata all'ORM manualmente
079500061017      * ? attribuendo UN ALTRO N.PARCEL  -->  SI DEVE SEGNALARE L'ANOMALIA.
079600061017     c                   eval      ar4trc ='I'
079700061017     C     Kar41         chain     fiar401l
079800061017     c                   if         %Found(fiar401l)
079900061017     c                   movel     ar4not        dsbl4i
080000061017
080100061017     c                   if        Lung_Parcel = 11
080200061017     C                   movel(p)  parcel11      alfa14           14
080300061017     c                   end
080400061017     c                   if        Lung_Parcel = 14
080500061017     C                   movel(p)  parcel14      alfa14
080600061017     c                   end
080700061017      * ?se Parcel diverso --> segnala l'errore
080800061017     C                   if        §B4IPN  <>  alfa14
080900061017     c                   movel     '02'          DPOCODER
081000061017     c                   end
081100061017     c                   end
081200061017     c                   end
081300061017      *
081400061017     c                   else
081500061017      * ?Se non era nella fase che si aspettava imposta il codice di errore --> '01'
081600061017      * ?   sul nostro avanzamento ORM per fase non congrua.
081700080523     c***   Se sono qui l'ho già ricevuto precedentemente e non devo + registrarlo
081800080523     c*********          movel     '01'          DPOCODER
081900060920     c                   end
082000061017     c                   endIF
082100060920      **
082200060920      *?      Ritirato Parcel in DPD
082300060920     c                   when      STATUS = '2'
082400060920
082500060920     c                   if        ORmFAO = 300 or ORmFAO = 400
082600080516      **
082700080516      ** Attenzione se si tratta di un ORM di un Parcel che deve essere consegnato
082800080516      **   all'Estero il nostro ORM deve essere chiuso con fase 910 (+82 ritirato da altra filiale)
082900080516     c                   if        ormNAC <> *blank                             All'Estero
083000080516     c                   z-add     910           ormFAO
083100080516     c                   else
083200080516
083300080516      ** Se consegna in Italia:
083400060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
083500060920     c                   z-add     600           ormFAO
083600080516      **
083700080516     c                   end
083800080516      **
083900060921     c                   exsr      aggior_fase
084000060921      *
084100060920     c                   else
084200080527      *
084300080527      * ? Se NON era stato ritirato (Fase 500) ma successivamente arriva
084400080527      * ? che è stato ritirato, si devono scrivere 2 fasi:
084500080527      * ? 400 -> Assegnato (nuovamente)
084600080527      * ? 600 -> Ritirato
084700080527     c                   if        ORmFAO =500
084800080527      *   Assegna
084900080605     c                   z-add     400           new_ormFAO
085000080527     c                   exsr      aggior_fase
085100080527      *   Ritirato
085200080605     c                   clear                   new_ormFAO
085300080527     c                   z-add     600           ormFAO
085400080527     c                   exsr      aggior_fase
085500080527      *
085600080527     c                   else
085700061017      *
085800061017      * ?- Attenzione: occorre controllare se c'è stato un problema di ritardo dell'informazione
085900061017      * ?  di stato ricevuta in ritardo rispetto al pacco che può essere già arrivato come bolla
086000061017      * ?  import e quindi l'ORM può trovarsi già chiuso.
086100061017      * ? MA TUTTO è già stato trattato operativamente in modo corretto (NON DEVE SEGNALARE NULLA)
086200061017      * ?altrimenti
086300061017      * ? SE COSI'non fosse, occorre segnalare l'errore x essere preso in considerazione.
086400061017     c                   if        ORmFAO >=900
086500061017      *
086600061017     C* aggancia l'ORM legato alla Bolla
086700061017     c                   eval      ar4trc ='M'
086800061017     c                   move      dpoPOE        dpoPOEa           3
086900061017     c                   move      dpoNRS        dpoNRSa           2
087000061017     c                   move      dpoNOR        dpoNORa           7
087100061017     c                   move      dpoNRV        dpoNRVa           2
087200070613     c*****              eval      ar4n30 = %trim(dpopoeA) + %trim(dponrsA) +
087300070613     c*****                                 %trim(dponorA) + %trim(dponrvA)
087400070613     c                   move      dpopoeA       §B4POE
087500070613     c                   move      dponrsA       §B4NSR
087600070613     c                   move      dponorA       §B4NOR
087700070613     c                   move      dponrvA       §B4NRV
087800070613     c                   eval      ar4n30 = dsbl4m
087900070613     c                   move      'N'           Sped_xOrm         1
088000070613     C     Kar43         chain     fiar403l
088100070613      *? x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
088200070613     c                   dow       not %Eof(fiar403l)
088300070613     c                   movel     AR4NOT        dsbl4M
088400070613     c                   if        §B4CLG = 'S'
088500070613     c                   move      'S'           Sped_xOrm         1
088600070613     c                   leave
088700070613     c                   end
088800070613     C     Kar43         reade     fiar403l
088900070613     c                   enddo
089000070613      ***
089100061017      * ?Se non trovato l'ORM legato ad una bolla invio l'errore
089200070613     c********           if        not %Found(fiar403l)
089300070613     c                   if        Sped_xOrm = 'N'
089400061017     c                   movel     '01'          DPOCODER
089500061017     c                   else
089600061017      * ?se trovato, altro tipo di controllo sul Parcel:
089700061017      * ? La bolla generata potrebbe essere stata associata all'ORM manualmente
089800061017      * ? attribuendo UN ALTRO N.PARCEL  -->  SI DEVE SEGNALARE L'ANOMALIA.
089900061017     c                   eval      ar4trc ='I'
090000061017     C     Kar41         chain     fiar401l
090100061017     c                   if         %Found(fiar401l)
090200061017     c                   movel     ar4not        dsbl4i
090300061017
090400061017     c                   if        Lung_Parcel = 11
090500061017     C                   movel(p)  parcel11      alfa14           14
090600061017     c                   end
090700061017     c                   if        Lung_Parcel = 14
090800061017     C                   movel(p)  parcel14      alfa14
090900061017     c                   end
091000061017      * ?se Parcel diverso --> segnala l'errore
091100061017     C                   if        §B4IPN  <>  alfa14
091200061017     c                   movel     '02'          DPOCODER
091300061017     c                   end
091400061017     c                   end
091500061017     c                   end
091600061017      *
091700061017     c                   else
091800061017      * ?Se non era nella fase che si aspettava imposta il codice di errore --> '01'
091900061017      * ?   sul nostro avanzamento ORM per fase non congrua.
092000061017     c                   movel     '01'          DPOCODER
092100061017     c                   end
092200080527      *
092300080527     c                   end
092400080527      *
092500061017     c                   endIF
092600060920      **
092700060920      *?     Stornato l'Ordine in DPD
092800060920     c                   when      STATUS = '3'
092900060920
093000060920     c                   if        ORmFAO <= 300
093100060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
093200060920     c                   z-add     999           ormFAO
093300060921     c                   exsr      aggior_fase
093400060921      *
093500060920     c                   else
093600060920      **  Se non era nella fase che si aspettava
093700060920      **   imposta il codice di errore --> '03' a ricordare lo STATUS 3 come errore
093800061214     c********           movel     '03'          DPOCODER
093900060920     c                   end
094000060920      **
094100060920      *?      Mancato Ritiro + GLI SCAN + COD.ADD.
094200060920     c                   when      STATUS = '4'
094300061120
094400080407      *   Su Chiarimento con DPD: ?
094500080516      * ?  Si è deciso di rilevare il motivo dallo Status 4 qui oppure dal STATUSDATACR
094600080516      * ?   con lo SCAN 18 e l'exception code.
094700080516      * ?  Comunque il primo che riceviamo con il motivo lo prendiamo.    ?
094800080407      *
094900061120      ** attenzione : se si tratta di un Mancato ritiro xchè presenti + parcel rispetto
095000061120      **   a quelli previsti (quindi SCAN 18 con cod.add=82) significa che occorre far fare
095100061120      **   al committente altri ORM per poter ritirare i parcel in più.
095200061120      **
095300061120      ** i dati arrivati in questo modo significa che è stato abbinato un parcel all'ORM
095400061120      **  ma ce ne sono 2 che non hanno l'ORM e che non possono essere ritirati.
095500061120      **   0163;409990;0148;01489000002045;20061109;;1;;;;;;
095600061120      **   0163;409990;0148;01489000002045;20061109;;4;;;;18;082;
095700061120      **   0163;409990;0148;01489000002045;20061109;;4;;;;18;082;
095800061120      **  NOTARE lo stesso riferimento ordine e lo stesso nr.Parcel nei records status (4)
095900080407     c*
096000080404     c                   if        zcodeX =  Parcel_inPIU
096100061120     c
096200080404     c                   movel     Parcel_inPIU  DPOCODER
096300061120     c                   add       1             DPOMANC
096400061120     c                   move      DPOSTS        STATUS
096500061120
096600061120     c                   else
096700060920
096800080527     c******             if        ORmFAO <= 400
096900080527     c                   if        ORmFAO <= 500
097000080527
097100060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
097200080516     c******             z-add     999           ormFAO
097300080516     c***  da tabella MRD:
097400080829     c                   if        manFase <> 0
097500080829
097600080516     c                   z-add     manFase       ormFAO
097700060921     c                   exsr      aggior_fase
097800080829      **
097900080829     c                   else
098000080829      **
098100080829      **  Se segnalano un Mancato ritiro NON codificato lo segnalo e non
098200080829      **   aggiorno l'ORM.
098300080829     c                   movel     '03'          DPOCODER
098400080829      **
098500080829     c                   end
098600060920      **
098700060920     c                   else
098800060920      **  Se non era nella fase che si aspettava imposta il codice di errore --> '01'
098900060920      **    sul nostro avanzamento ORM per fase non congrua.
099000060920     c                   movel     '01'          DPOCODER
099100060920     c                   end
099200060920      **
099300061120     c                   end
099400061120      **
099500060920      *?      Cancellazione non possibile in DPD
099600060920     c                   when      STATUS = '5'
099700060920
099800100723     c                   if        ORmFAO >  900
099900080513      *     rilascia record per poterlo poi riaprire
100000080513     c                   update    fnORM000
100100080513      *
100200060920      ** deve riaprire l'ORM che era stato precedentemente chiuso
100300080423      ** con Fase 100.
100400080423     c                   clear                   fior51ds
100500080423     c                   eval      I51TIP = 'A'
100600080423     c                   eval      I51POE = dpoPOE
100700080423     c                   eval      I51NSR = dpoNRS
100800080423     c                   eval      I51NOR = dpoNOR
100900080423     c                   eval      I51NRV = dpoNRV
101000080423     c                   eval      I51FAR = 100
101100080423     c                   call      'FIOR51R'
101200080423     c                   parm                    fior51ds
101300080513      **
101400080513      **  Deve riattivare il record se annullato sul FIDPO
101500080513     c                   if        O51ERR = *blank and
101600080513     c                             dpoANN <> *blank
101700080513     c                   clear                   dpoANN
101800080513     c                   end
101900060920      **
102000060921     c                   else
102100060920      **  Se non era nella fase che si aspettava imposta il codice di errore --> '01'
102200060920      **    sul nostro avanzamento ORM per fase non congrua.
102300060920     c                   movel     '01'          DPOCODER
102400060920     c                   end
102500060920      **
102600060920     c                   endsl
102700080516      *?     Finiti gli STATUS:
102800060920      **
102900060920     c                   end
103000060920      **
103100060920      **  Quindi aggiorna FIDPO con o senza l'errore con il Nr.Parcel
103200060920      **   solo se non è stato precedentemente ricevuto e aggiornato dal file
103300060920      **    degli STATI con SCAN 18
103400080516     c                   if        dpoSTS <> 8  or
103500080516     c                             dpoSTS =  8 and DPOcdmr = *blank
103600080516      **
103700060920     c                   move      *zeros        DPOSTS
103800060920     c                   move      STATUS        DPOSTS
103900080516      **
104000061002      * se mancato ritiro
104100080404     c                   if        dpoSTS =  4 and zcodeX > 0
104200080404     c                   move      zcodeX        DPOcdmr
104300061002     c                   end
104400080516      **
104500060920     c                   z-add     data_oggi     DPODTRIC
104600060920     c                   end
104700080516      **
104800080530      **  Se deve segnalare un errore riattiva il
104900080530     c                   if        DPOCODER <> *blank
105000080530     c                   MOVEL     *blank        dpoANN
105100080530     c                   end
105200080530      **
105300060920     c                   MOVEL     PKNR          DPONRP
105400060920     c                   update    fidpo000
105500170102     c
105600170102     c* Richiamo pgm per aggiornare codice bolla da franco a ass su blp se ancora "Da partire"
105700170102     c                   clear                   fieu39ds
105800170102     c                   eval      I39POE=dpopoe
105900170102     c                   eval      I39NSR=dponrs
106000170102     c                   eval      I39NOR=dponor
106100170102     c                   eval      I39NRV=dponrv
106200170102     c                   eval      I39NPR=dponrp
106300170102     c                   call      'FIEU39R'
106400170102     c                   parm                    fieu39ds
106500060920      **
106600060920     c                   end
106700060809      * ?---------------------------------  **
106800060809      **
106900060809     c                   Endsr
107000060612      * ?------------------------------------------------------------------ */
107100060921      *?      Aggiorna Fase sugli ORM
107200060612      * ?------------------------------------------------------------------ */
107300060921     c     Aggior_Fase   Begsr
107400060612      **
107500080605      * per poter gestire la scrittura delle 2 fasi consecutive
107600080605      *  e aggiornare una volta sola l'FNORM000
107700080605      **
107800080605     c                   if        new_ormFAO = 0
107900080605      **
108000170228      * imposto il campo ORMETI
108100170228     c                   eval      tbeCOD = 'FAR'
108200170228     c                   movel(p)  ormfao        tbeke1
108300170228     c     ktbe1         chain     tntbe01l
108400170228     c                   if        %Found(tntbe01l)
108500170228     c                   eval      dfar = tbeuni
108600170228     c                   eval      ormeti = d§farass
108700170228     c                   end
108800170228      ****
108900060921     c                   z-add     data_oggi     ormDFO
109000060921     c                   z-add     ore           ormOFO
109100060921     c                   update    fnORM000
109200080605      **
109300080605     c                   else
109400080605     c                   eval      ormFAO = new_ormFAO
109500080605     c                   end
109600060921
109700060921      **  Allinea le Fasi in dettaglio
109800060921     c     KOrM1         setgt     fnOrf01l
109900060921     c     KOrM1         readpe    fnOrf01l
110000060921     c                   if        not %Eof(fnorf01l)
110100080516      *
110200080516      ** Chiuso prima dell'affidamento
110300061017     c                   if           STATUS = '3'
110400061017     c                   eval         ORFCAR = '80'
110500060921     c                   end
110600080516      *
110700080516      ** Mancato Ritiro con motivo: (tabella "MRD")
110800061017     c                   if           STATUS = '4'
110900061017     c                   eval         ORFCAR = mancod
111000060921     c                   end
111100080516      **
111200080605      **  se prima era come mancato ritiro adesso riassegna e non deve
111300080605      **   impostare il codice di mancato ritiro.
111400080605     c                   if           STATUS = '2'
111500080605     c                   if        new_ormFAO = 400 and ORFFAR = 500
111600080605     c                   clear                   ORFCAR
111700080605     c                   end
111800080605     c                   end
111900080605      **
112000080516      **   Imposta Stessa Fase dell'ORM
112100060921     c                   z-add     ormFAO        ORFFAR
112200080516      **
112300080516      *  Quando Fase 910:
112400080516      *  è il caso di un ORM che deve essere ritirato e consegnato NON in ITALIA
112500080516      **  ma solo all'estero.
112600080516     c                   if        ormFAO = 910
112700080516     c                   eval      ORFCAR = Consegna_estero
112800080516     c                   end
112900080605      **
113000060921     c                   z-add     data_oggi     ORFDAE
113100060921     c                   z-add     data_oggi     ORFDFC
113200060921     c                   z-add     ore           ORFORE
113300060921     c                   eval      ORFPUE = 'ORMDPD_AUT'
113400060921     c                   write     fnorf000
113500060921     c                   end
113600060921      **
113700060921     c                   Endsr
113800060921      * ?------------------------------------------------------------------ */
113900060921      *?      Suddivide i campi della riga con carattere divisorio (;)
114000060921      * ?------------------------------------------------------------------ */
114100060921     c     Split_Rec     Begsr
114200060921      **
114300060612     c                   clear                   Nr_campi
114400060612     c                   clear                   Campi_Record
114500060612     c                   clear                   Campi_Lunghi
114600060612     c                   clear                   Campi_Numerici
114700060612     c                   clear                   Campi_Decimali
114800060612     c                   movel     ';'           Separa_campi
114900090302     c                   movel     '.'           Decimal_Separ
115000060612      *
115100060612     c                   call      'FIEU00R'
115200060612      * ?     Input
115300060612     c                   parm                    Separa_campi
115400060612     c                   parm                    Decimal_Separ
115500060612     c                   parm                    dati
115600060612      * ?     Output  su schiere di 100 elementi
115700060612     c                   parm                    Nr_campi
115800060612     c                   parm                    Campi_Record                   * i dati
115900060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
116000060612     c                   parm                    Campi_Numerici                 * se numerici
116100060612     c                   parm                    Campi_Decimali                 * quanti decimali
116200060612      **
116300060612     c                   Endsr
116400971215      *----------------------------------------------------------------
116500051205      *   DEFINIZIONE CHIAVI                               *
116600060808      *----------------------------------------------------------------
116700051205     C     *INZSR        BEGSR
116800051205      *
116900991129     c     *ENTRY        PLIST
117000060613     C                   parm                    esito             1
117100060809      **
117200060809     c     Korm1         klist
117300060809     c                   kfld                    DPOPOE
117400060809     c                   kfld                    DPONRS
117500060809     c                   kfld                    DPONOR
117600060809     c                   kfld                    DPONRV
117700061017      *
117800061017     C     Kar43         KLIST
117900061017     C                   KFLD                    ar4trc
118000061017     C                   KFLD                    ar4n30
118100061017      *
118200061017     c     kar41         klist
118300061017     C                   kfld                    AR4AAS
118400061017     C                   kfld                    AR4LNP
118500061017     C                   kfld                    AR4NRS
118600061017     C                   kfld                    AR4NSP
118700061017     C                   kfld                    ar4trc
118800060809      **
118900061017     c     KDPO3         klist
119000060809     c                   kfld                    ODEPOT
119100060809     c                   kfld                    ORDERNR
119200170228      **
119300170228     c     Ktbe1         klist
119400170228     c                   kfld                    tbeCOD
119500170228     c                   kfld                    tbeKE1
119600060808      *
119700060808      *  imposta UDATE di elaborazione
119800060915     C                   move      *date         data_oggi
119900060915     C                   move      *date         G02inv
120000060915     C                   MOVE      *ZEROS        G02dat
120100060808     C                   MOVE      *BLANKS       G02ERR
120200060808     C                   CALL      'XSRDA8'
120300060808     C                   PARM                    WLBDAT
120400060808     C                   move      data_oggi     day
120500060808     C                   TIME                    W0140
120600060808     C                   MOVEl     W0140         ore
120700060906
120800060906      **?carica motivi mancata consegna                                     */
120900060906     c     'MRD'         setll     tntbe01l
121000060906     c                   do        *hival
121100060906     c     'MRD'         reade     tntbe01l
121200060906     c                   if        %eof(tntbe01l)
121300060906     c                   leave
121400060906     c                   endif
121500060906     c                   movel     tbeuni        dmrd
121600060906     c                   add       1             x
121700060906     c                   movel     tbeke1        manb(x)
121800060906     c                   movel     §mrdcar       mand(x)
121900060906     c                   movel     §mrdfar       manf(x)
122000060906     c                   enddo
122100060906
122200050414      *
122300991124     C                   ENDSR
122400060621      * ?------------------------------------------------------------------ */
122500060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
122600060621      * ?------------------------------------------------------------------ */
122700060621     C     *pssr         BEGSR
122800060621     C
122900060710     C                   eval      esito = '2'
123000060621     C
123100060621     C                   ENDSR     '*CANCL'
123200060614      *-------------------------------------------------------------------------
