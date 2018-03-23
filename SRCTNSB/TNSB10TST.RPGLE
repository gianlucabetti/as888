000100111025       //==============================================================
000200111025       //?TNSB10TST * Test nuovo LAGUNA                                ?
000300111025       //==============================================================
000400111025
000500111025       //--------------------------------------------------------------
000600111025       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700111025       //--------------------------------------------------------------
000800111025
000900111025     /*PRM dbgview(*source)
001000111025     /*END
001100111025
001200111025       //--------------------------------------------------------------
001300111025       //?Specifiche di controllo.                                     ?
001400111025       //--------------------------------------------------------------
001500111025
001600111025     h decedit('0,') datedit(*dmy/)
001700111025     h option(*nodebugio)
001800040119
001900111025       //--------------------------------------------------------------
002000111025       //?Dichiarazione file.                                          ?
002100111025       //--------------------------------------------------------------
002200040119
002300111025       // -?Tabelle "LAC"?
002400111025     f*//TNTBE01L  if   e           k disk
002500111025       // -?Tabella "TB"?
002600111025     f*//TABEL00F  if   e           k disk
002700111025
002800111025       // -?Immagini scanner da Laguna ad AS?
002900040119     fTILAG00R  Uf   e           k disk
003000111025
003100111025       // -?LOG per colloquio operazioni con Laguna DPD?
003200041021     fTILAO01L  Uf a e           k disk
003300111025       // -?LOG per colloquio operazioni con Laguna CLIENTI?
003400050314     fTILAC01L  Uf a e           k disk
003500111025
003600111025       // -?Storico immagine Laguna?
003700061227     fTILAD00F  O    e             disk
003800040119
003900111025       //--------------------------------------------------------------
004000111025       //?Definizione costanti.                                        ?
004100111025       //--------------------------------------------------------------
004200111025
004300111026       // -?Num. spedizioni da registrare in TILAO ed in TILAC?
004400111028     d c_Rec_TILAx     c                   const(1000)
004500061227
004600111025
004700111025       //--------------------------------------------------------------
004800111025       //?Definizione schiere.                                         ?
004900111025       //--------------------------------------------------------------
005000111025
005100111025
005200111025       //--------------------------------------------------------------
005300111025       //?Definizione strutture dati.                                  ?
005400111025       //--------------------------------------------------------------
005500111025
005600111025       // -?Ora/Data e Data/Ora?
005700040331     d DS_Time1        ds            14    inz
005800040331     d   dsT1_hms                     6  0 inz
005900040331     d   dsT1_dmy                     8  0 inz
006000111025     d DS_Time2        ds            14    inz
006100111025     d   dsT2_ymd                     8  0 inz
006200111025     d   dsT2_hms                     6  0 inz
006300111025
006400111025       // -?Status ds?
006500040331     d Status         sds           333
006600040331     d  SDSjob               244    253                                         Job name
006700111025
006800111025       //--------------------------------------------------------------
006900111025       //?Definizione variabili globali.                               ?
007000111025       //--------------------------------------------------------------
007100111025
007200111026       // -?Contatori?
007300111026     d wCountLAC       s              7  0 inz
007400111026     d wCountLAO       s              7  0 inz
007500111026     d wRem            s              3  0 inz
007600111025
007700111025       // -?Campi chiave?
007800111026     d kLAOcop         s                   like(LAOcop)  inz('DPD')
007900111026     d kLACcop         s                   like(LACcop)  inz('CLI')
008000111025
008100111025       // -?Campi di comodo?
008200111026     d wNIM            s                   like(LACnim)  inz
008300111026     d wDIR            s                   like(LACdir)  inz
008400111026     d wTPI            s                   like(LACtpi)  inz
008500040331     d WtasDCM         s              8    inz(*zeros)
008600050330     d WtasRMN         s             15    inz(*zeros)
008700111025     d XtasRMN         s             15  0 inz
008800111026     d XtasRMA         s             15    inz
008900040331     d Wtime           s             14  0 inz
009000111026     d*// Wdata_00        s              8  0 inz
009100040331     d WdateISO        s               d   datfmt(*ISO) inz
009200111025     d tilao_u_w       s              1    inz
009300111025     d tilac_u_w       s              1    inz
009400090212
009500090212       //--------------------------------------------------------------
009600090212       //?Definizione procedure usate.                                 ?
009700090212       //--------------------------------------------------------------
009800090212
009900090218
010000090218       //--------------------------------------------------------------
010100090218       //?Definizione key-list.                                        ?
010200090218       //--------------------------------------------------------------
010300090218
010400111025       // -?File TILAO01L?
010500041021     c     K05LAO        klist
010600041021     c                   kfld                    LAGaas
010700041021     c                   kfld                    LAGlnp
010800041021     c                   kfld                    LAGnrs
010900041021     c                   kfld                    LAGnsp
011000111025     c                   kfld                    kLAOcop
011100111025       // -?File TILAC01L?
011200050314     c     K05LAC        klist
011300050314     c                   kfld                    LAGaas
011400050314     c                   kfld                    LAGlnp
011500050314     c                   kfld                    LAGnrs
011600050314     c                   kfld                    LAGnsp
011700111025     c                   kfld                    kLACcop
011800111024
011900111025       //--------------------------------------------------------------
012000111025       //?I N D I C A T O R I   U T I L I Z Z A T I                    ?
012100111025       //--------------------------------------------------------------
012200111026       // 99    - Errore in scrittura record nel file TILAD00F per
012300111026       //         chiave duplicata
012400111025       //--------------------------------------------------------------
012500040119
012600111026
012700111025       //--------------------------------------------------------------
012800111025       //?M A I N - L I N E                                            ?
012900111025       //--------------------------------------------------------------
013000090807
013100040331      * Operazioni Iniziali
013200040331     c                   exsr      OperazIniz
013300040331      *
013400111025      *
013500040119      * Ciclo di lettura del file TILAG00R
013600040119     c                   read      TILAG000
013700040119      *
013800111025      *
013900111025do  1c                   DOW       NOT %eof(TILAG00R)
014000040119      *
014100111025      *
014200040119      * - Elaborazione dei soli rec. "IM"
014300111025if  2c                   If        LAGapp = 'IM'
014400040119      *
014500111025      *
014600111025      /free
014700111025
014800111025           XtasRMN += 1;
014900111025
015000111025           select;
015100111026             when  wCountLAO < c_Rec_TILAx;
015200111026               wCountLAO += 1;
015300111025               exsr  Wrt_TILAO;
015400111026             when  wCountLAC < c_Rec_TILAx;
015500111026               wCountLAC += 1;
015600111025               exsr  Wrt_TILAC;
015700111025           endsl;
015800111025
015900090218      /end-free
016000061227      *
016100111024      * Sposto da TILAG00R in TILAD00F   SEMPRE
016200061227     c                   exsr      Wrt_TILAD
016300040119      *
016400111025      *
016500111025e   2c                   EndIf
016600040119      *
016700111025      *
016800040119      * - Lettura del record successivo nel file TILAG00R
016900040119     c                   read      TILAG000
017000040119      *
017100111025      *
017200111025e   1c                   ENDDO
017300111026      *
017400111026      *
017500111026      * Fine
017600111026     c                   return
017700040331      *
017800040331      **-------------------------------------------------------------**
017900040331      ** OperazIniz ** Operazioni Iniziali                           **
018000040331      **-------------------------------------------------------------**
018100040331     c     OperazIniz    BEGSR
018200111025      *
018300111025     c                   eval      *inLR = *on
018400040331      *
018500040331      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
018600111025     c                   time                    Wtime
018700111025     c                   movel     Wtime         DS_Time1
018800111025     c                   eval      dsT2_hms   = dsT1_hms
018900111025     c     *eur          movel     dsT1_dmy      WdateISO
019000111025     c                   move      WdateISO      dsT2_ymd
019100111025      *
019200111025     c                   eval      WtasDCM = %editc( dsT2_ymd : 'X' )
019300040331      *
019400040331     c                   ENDSR
019500040331      *
019600040331      **-------------------------------------------------------------**
019700040331      ** Wrt_TILAO  ** Registrazione dati nel file TILAO00F x DPD    **
019800040331      **-------------------------------------------------------------**
019900040331     c     Wrt_TILAO     BEGSR
020000040331      *
020100040331      * Impostazione dei campi numerici in campi alfanumerici di comodo
020200111025      /free
020300111025         WtasRMN =  %editc( XtasRMN : 'X' );
020400111025      /end-free
020500040331      *
020600041021      * Se su TILAO esiste già un record non elaborato aggiorno else scrivo
020700041021     C                   eval      Tilao_U_W = 'W'
020800041021      *
020900111025     C     k05lao        setll     Tilao01L
021000111025     C     k05lao        reade     Tilao01L
021100111025     C                   DOW       NOT %EOF(TILAO01L)
021200111025     C                   IF        LAGela = '00'
021300111025     C                   eval      Tilao_U_W = 'U'
021400111025     c                   leave
021500111025     C                   ELSE
021600111025     C     k05lao        reade     Tilao01L
021700111025     c                   ENDIF
021800111025     c                   ENDDO
021900041021      *
022000040331      * Preparazione del record
022100040331     c                   clear                   TILAO000
022200040331     c                   move      DS_Time2      LAOtim
022300111024     c                   move      LAGaas        LAOaas
022400111024     c                   move      LAGlnp        LAOlnp
022500111024     c                   move      LAGnrs        LAOnrs
022600111024     c                   move      LAGnsp        LAOnsp
022700040331     c                   movel     'DPD'         LAOcop
022800111025      *
022900111026      * Impostazione nome immagine
023000111026     c                   exsr      sr_NomeImmag
023100111026     c                   eval      LAOope =  wNIM
023200111025      *
023300040331     c                   movel     SDSjob        LAGidl
023400040331     c                   move      *zeros        LAGela
023500040331      *
023600041021      * Scrittura /aggiornamento
023700041021     C                   IF        Tilao_U_W = 'U'
023800041021     c                   update    TILAO000
023900041021     c                   ELSE
024000041021     c                   write     TILAO000
024100041021     c                   ENDIF
024200040331      *
024300111025     c                   ENDSR
024400040119      *
024500050314      **-------------------------------------------------------------**
024600050314      ** Wrt_TILAC  ** Registrazione dati nel file TILAC00F x cliente**
024700050314      **-------------------------------------------------------------**
024800050314     c     Wrt_TILAC     BEGSR
024900050314      *
025000050314      * Se su TILAC esiste già un record non elaborato aggiorno else scrivo
025100050314     C                   eval      Tilac_U_W = 'W'
025200050314      *
025300050314     C     k05lac        setll     Tilac01L
025400050314     C     k05lac        reade     Tilac01L
025500050314     C                   DOW       NOT %EOF(TILAC01L)
025600050314     C                   IF        LACela = '00'
025700050314     C                   eval      Tilac_U_W = 'U'
025800050314     c                   leave
025900050314     C                   ELSE
026000050314     C     k05lac        reade     Tilac01L
026100050314     c                   ENDIF
026200050314     c                   ENDDO
026300050314      *
026400050314      * Preparazione del record
026500050314     c                   clear                   TILAC000
026600050314     c                   move      DS_Time2      LACtim
026700111026     c                   move      LAGaas        LACaas
026800111026     c                   move      LAGlnp        LAClnp
026900111024     c                   move      LAGnrs        LACnrs
027000111024     c                   move      LAGnsp        LACnsp
027100050314     c                   movel     'CLI'         LACcop
027200111026      *
027300111026     c                   exsr      sr_Dir_TipoImg
027400111026     c                   eval      LACdir = wDIR
027500111026     c                   eval      LACtpi = wTPI
027600111026      *
027700111026     c                   exsr      sr_NomeImmag
027800111026     c                   eval      LACnim = wNIM
027900111026      *
028000111026     c                   move      *zeros        LACela
028100111026     c                   eval      lactela = 'A'
028200111026      *
028300111026      * flag nome immagine:
028400090310      * calcolo ID job in base alla frequenza addebito
028500090310      * - se frequenza addebito per Immagine
028600111025     c*//                when      lactadu = 'I'
028700090310      *   id = n.spedizione
028800111026     c*//                eval      lacidjob = lacaas + laclnp +
028900111026     c*//                                     lacnrs + lacnsp
029000050314      *
029100050314      * Scrittura /aggiornamento
029200050314     C                   IF        Tilac_U_W = 'U'
029300050314     c                   update    TILAC000
029400050314     c                   ELSE
029500050314     c                   write     TILAC000
029600050314     c                   ENDIF
029700050314      *
029800111025     c                   ENDSR
029900061227      *
030000061227      **-------------------------------------------------------------**
030100061227      ** Wrt_TILAD  ** Registrazione dati nel file TILAD00F          **
030200061227      **-------------------------------------------------------------**
030300061227     c     Wrt_TILAD     BEGSR
030400061227      *
030500061227     c                   clear                   TILAD000
030600111028     c*//                eval      LADtim = LAGtim
030700111028     c                   move      DS_Time2      LADtim
030800061227     c                   eval      LADaas = LAGaas
030900061227     c                   eval      LADlnp = LAGlnp
031000061227     c                   eval      LADnrs = LAGnrs
031100061227     c                   eval      LADnsp = LAGnsp
031200061227     c                   eval      LADxx1 = LAGxx1
031300061227     c                   eval      LADcop = 'MOV'
031400111026     c*//                eval      LADdir = 'ERRATE'
031500061227     c                   eval      LADela = *zeros
031600111026      *
031700111026     c                   exsr      sr_Dir_TipoImg
031800111026     c                   eval      LADdir = wDIR
031900111026     c*//                eval      LADtpi = wTPI
032000111026      *
032100111026     c*//                exsr      sr_NomeImmag
032200111026     c*//                eval      LADnim = wNIM
032300111026      *
032400111026     c*//                eval      LADidl = SDSjob
032500061227      *
032600061227     c                   WRITE     TILAD000                             99
032700061227      *
032800061227      * Cancellazione record dal file TILAG00R
032900111024      * => NO !!!
033000111025     c*//*//*            DELETE    TILAG000
033100061227      *
033200061227     c                   ENDSR
033300111026
033400111026      /Free
033500111026
033600111026       //--------------------------------------------------------------
033700111026       //?Impostazione nome immagine.                                  ?
033800111026       //--------------------------------------------------------------
033900111026       BEGSR  sr_NomeImmag;
034000111026
034100111026         wRem = %rem( XtasRMN : 3 );
034200111026
034300111026         Select;
034400111026           When  wRem = 1;
034500111026             wNIM = LAGaas + LAGlnp + LAGnrs + LAGnsp;
034600111026           When  wRem = 2;
034700111026             wNIM = 'TEST JDOC';
034800111026           Other;
034900111026             wNIM = 'TEST JDOC_' + LAGaas + LAGlnp + LAGnrs + LAGnsp;
035000111026         EndSl;
035100111026
035200111026       ENDSR;
035300111026
035400111026       //--------------------------------------------------------------
035500111026       //?Impostazione directory & tipo immagine.                      ?
035600111026       //--------------------------------------------------------------
035700111026       BEGSR  sr_Dir_TipoImg;
035800111026
035900111026         wRem = %rem( XtasRMN : 2 );
036000111026
036100111026         Select;
036200111026           When  wRem = 1;
036300111026             wDIR = 'TEST/2011';
036400111026             wTPI = 'JPG';
036500111026           Other;
036600111026             wDIR = '2011 10-26';
036700111026             wTPI = 'TIF';
036800111026         EndSl;
036900111026
037000111026       ENDSR;
037100111026
037200111026      /end-free
