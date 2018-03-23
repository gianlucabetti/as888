000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600120823      *  TRASCODIFICA : tracciato NET EXPRESS EUROPE  da GEL                    *
000700120823      *               BOLLE IMPORT                                              *
000800060612      **?************************************************************************
000900991124      * Il pgm crea:                                                            *
001000020919      *             EDivab3L file spedizioni da confermare per camion           *
001100020919      *             FiVAB01L file spedizioni da confermare                      *
001200060609      **?************************************************************************
001300120824     FEDTAB01L  IF   E           K DISK
001400120824      *
001500120824     Ftivin00r  uF   E             DISK    commit usropn
001600120823     FEDGEL00F  UF A E           K DISK
001700120824     FEDRDE01L  UF A E           K DISK    commit
001800991124      *
001900991206     FFLNUF01L  UF   E           K DISK
002000991129     FTNTBE01L  IF   E           K DISK
002100050112     FTABEL00F  IF   E           K DISK
002200120824     FCNACO00F  IF   E           K DISK
002300120824     FCNIND00F  IF   E           K DISK
002400971216      *
002500110930     FEDivat4L  iF   E           K DISK    rename(EDIVAT00:EDIVAT4)
002600061016      *
002700120824     FFiVAB01L  UF A E           K DISK    commit
002800120824     FEDivab3L  UF A E           K DISK    commit
002900041215      *
003000120824     FEDiVAT3L  Uf A E           K DISK    commit
003100120824     FFiVAT01L  Uf A E           K DISK    commit
003200940321      *----------------------------------------------------*
003300120824      * Tabella partner
003400120824     D EDIDSPT       E DS
003500120824     D EDIDSPU       E DS
003600120824     D EDIDSPV       E DS
003700120824     D EDIDSTC       E DS
003800120824     D DS1B          E DS
003900120824     D DS15          E DS
004000120824     D DSCV          E DS
004100120824     D kKUT            s                   LIKE(TBLKUT)
004200120824     D kCOD            s                   LIKE(TBLCOD)
004300120824     D kKEY            s                   LIKE(TBLKEY)
004400120824     D kKCC            s                   LIKE(ACOKCC)
004500120824     D kKSC            s                   LIKE(ACOKSC)
004600120824     D kAAS            s                   LIKE(VABAAS)
004700120824     D kLNP            s                   LIKE(VABLNP)
004800120824     D kNRS            s                   LIKE(VABNRS)
004900120824     D kNSP            s                   LIKE(VABNSP)
005000120824     D kCMR            s                   LIKE(VABCMR)
005100120824     D kCCM            s                   LIKE(VABCCM)
005200120824     D kNMC            s                   LIKE(RDENMC)
005300120824     D kNRP            s                   LIKE(RDENRP)
005400120824     D kLNP1           s                   LIKE(RDELNP)
005500120924     D flgCBO          s                   LIKE(VABcbo)
005600120924     D savRSD          s                   LIKE(vabrsd)
005700120924     D savRD2          s                   LIKE(vabrd2)
005800120824      *
005900120824      *  Invio E-mail
006000120824     D Indirizzi       s            253
006100120824     D Oggetto         s             44
006200120824     D Messaggio       s           5000
006300120824      *
006400120824     d   DSmail        ds
006500120824     D Mail_Ind                      44
006600120824     d   IND_char                     1    dim(44)
006700120824      *
006800120824     D Lun_Ind         s              5u 0
006900120824      *
007000120824     D Dac             s              5u 0
007100120824     D Luc             s              5u 0
007200120824      *
007300120824     C*---------------------------------------------------------------*
007400120824     D MsgDSP          S            256                                         Testo generico
007500120824     C*---------------------------------------------------------------*
007600120824     D SndMg01         S             10                                         Message type
007700120824     D                                     INZ('*INFO')
007800120824     D SndMg02         S             10                                         Deluvery mode
007900120824     D                                     INZ('*BREAK')
008000120824     D SndMg03         S            256                                         Message text
008100120824     D SndMg04         S             10I 0                                      Length of text
008200120824     D                                     INZ(%SIZE(SndMg03))
008300120824     D SndMg05         S             10                                         User profile
008400120824     D                                     DIM(299)
008500120824     D SndMg06         S             10I 0                                      Number of user
008600120824     D SndMg07         S             10I 0                                      Message sent indic.
008700120824     D SndMg08         S             10I 0                                      Function requested
008800120824     D SndMg10         S              1                                         Show display
008900120824     D                                     INZ('N')
009000120824     D SndMg11         S             20                                         Qualified MSGQ name
009100120824     D SndMg12         S              4                                         Name type indicator
009200120824     D                                     INZ('*USR')
009300120824      * indice di scihera
009400120824     D Jx              S              5I 0
009500120824      *
009600120824     D/COPY QSYSINC/QRPGLESRC,QUSEC
009700120824      *
009800120824      *
009900121107     D CPT             S             35    DIM(200)
010000121105     D CPTparz         S             35    DIM(%elem(CPT))
010100121105     D KSC_UNB         S              7    DIM(%elem(CPT))
010200121105     D DPT             S             90    DIM(%elem(CPT))
010300121105     D DPU             S             90    DIM(%elem(CPT))
010400121105     D DPV             S             90    DIM(%elem(CPT))
010500121105     D TRUL0SDS      E DS
010600121105      *
010700121105     d Ptn_parziale    s              1    inz('N')
010800121105     d sav_CPTx        s              3  0 inz(0)
010900120824      * Tabella codici trattamento merce (1B)
011000120824     D CTM             S              2    DIM(200)
011100120824     D DTM             S             67    DIM(200)
011200120824      * Tabella codici nazioni
011300120824     D C15             S              3    DIM(500)
011400120824     D ISO             S              3    DIM(500)
011500120824     D CPF             S              2  0 DIM(500)
011600120824      *
011700120824      *  DS x scomposizione numero spedizione
011800120824     D DSSPE           DS
011900120824     D  SPEAAS                 1      4  0
012000120824     D  SPELNP                 5      7  0
012100120824     D  SPENRS                 8      9  0
012200120824     D  SPENSP                10     16  0
012300120824      *
012400120824      *  DS x scomposizione segnacollo
012500120824     D DSSGN           DS
012600120824     D  SGNLNP                 1      3  0
012700120824     D  SGNLNA                 4      6  0
012800120824     D  SGNNRS                 7      8  0
012900120824     D  SGNNSC                 9     15  0
013000120824     D  SGNZNC                16     17  0
013100120824      *
013200000907      *-------------------
013300060614     D Tipo_error      S              1  0
013400060612      *-------------------
013500991129     D W015A           S             15
013600091109     D W030A           S             30
013700000223     D W0140           S             14  0
013800991129     D WORA            S              6  0
013900991129     D XX              S              3  0 INZ
014000991129     D WDTGIO          S              8  0
014100991129     D DATEU           S              8  0
014200991129     D DATA_eur        S               D   DATFMT(*eur)
014300120824      **
014400120824     D WLBDA8          DS
014500120824     D  G02DAT                 1      8  0
014600120824     D  G02INV                 9     16  0
014700120824     D  G02ERR                17     17
014800120824     D  G02TGI                18     22  0
014900120824      **
015000991206     D KAAA            S                   LIKE(NUFAAA)
015100991206     D KCNU            S                   LIKE(NUFCNU)
015200991206     D KFIL            S                   LIKE(NUFFIL)
015300050112     D NUM_Sped        s                   LIKE(vabnsp)
015400050112     D svkpjbu         s                   like(kpjbu)
015500120827     D salva_shpNbr    s             15
015600120827     D allinea_shpNbr  s             15
015700120827     D shp15           S              1    DIM(15)
015800120824      *
015900120824      * Tabella CL --> codici clienti - tariffa
016000120824     D CCL             S             35    DIM(300)
016100120824     D DCL             S             90    DIM(300)
016200120824      * Schiere x caricamento cod.cliente  <>
016300120824     D KCL             S              7  0 DIM(300)
016400120824      * Termini di consegna
016500120824     D K35_TpBolla     S             35    DIM(100)
016600120824     D Cod_TpBolla     S              3    DIM(100)
016700120824     D Decod_Porto     S              1    DIM(100)
016800120824     d Trovata_Tab_TB  S              1
016900120824      *
017000120824      * Tipo pagamento C/Assegno
017100120824     D CTC35           S             35    DIM(100)
017200120824     D TPI             S              2    DIM(100)
017300120824      * Decodifiche servizi speciali
017400120824     D SS35            S             35    DIM(100)
017500120824     D SS              S              3    DIM(100)
017600120824     D DSS             S             90    DIM(100)
017700120824      * Schiere x routine di conversione CAP
017800120824     D CAP7            S              1    DIM(7)
017900120824     D CAPM            S              1    DIM(7)
018000120824      * Stringhe per conversione alfanumerico/numerico N.Documento
018100120824     D CA              S             78    DIM(1) CTDATA PERRCD(1)
018200120824     D CN              S             78    DIM(1) CTDATA PERRCD(1)
018300120824     D recS            S             42    DIM(37) CTDATA PERRCD(1)
018400120824     D recT            S             35    DIM(16) CTDATA PERRCD(1)
018500060413      *-------------------
018600991129     D KPJBA         E DS
018700060608      **?------------------------------------------------------------------ */
018800060608     C*? Ds Scrittura del VAT "E"  passaggio nuovi campi BIC3
018900060608      **?------------------------------------------------------------------ */
019000120823     D edGEL00s      E DS
019100120823     D edGEL00t      E DS
019200060608      **?------------------------------------------------------------------ */
019300050112      * Numeratore Bolle (302)
019400050112     D trul33ds      E DS
019500050112     D Ds3C          E DS
019600070202     D TISI95DS      E DS
019700120824     D TIBS55DS      E DS
019800120824     D UTEDSE0F      E DS
019900120824     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
020000120824     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
020100120824      * Ds scomposzione tipo capoconti
020200120824     D TCUDS           DS
020300120824     D  F34                    3      4
020400120824     D  F56                    5      6
020500060612      * ?================================================================== */
020600060612      * ?   *--------------------------------------------------------------*
020700060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
020800060612      * ?   *--------------------------------------------------------------*
020900060612      *
021000060612      * ?   * Campi x decodifica * (INPUT  del Record)
021100060612     D  Dati           s           1500
021200060612     D Separa_campi    s              1
021300060612     D Decimal_Separ   s              1
021400060612      * ?   * Campi decodificati * (OUTPUT del Record)
021500060612      *       Schiere di Output / Campi di Output
021600060614     D  Alfa40         s             40
021700060612     d  Sk             s              3u 0
021800060612     D    NR_campi     s              3u 0
021900060612     D  Campi_Record   ds
022000060612     D    Dato_sk                    40    DIM(100)
022100060612     D  Campi_Lunghi   ds
022200060612     D    LunDato_sk                  3u 0 DIM(100)
022300060612     D Campi_Numerici  ds
022400060612     D    DatoNum_sk                  1    DIM(100)
022500060612     D Campi_Decimali  ds
022600060612     D    Decimal_sk                  3u 0 DIM(100)
022700060612      * ?   *--------------------------------------------------------------*
022800060612     D  position       s              3  0 INZ(0)
022900060612
023000060612     D  se_errore      s              1    inz(' ')
023100060612     D  Msg_Err        s            132    inz(' ')
023200060614     D  Msg_Vin_80     s             80    inz(' ')
023300060612      * ?* ------------------------------------------------------ *
023400060612      * Records di controllo mandati nell'IGATEAVIS
023500120827     D Testa_Sk        s             10    DIM(50)
023600120504     d non_inviato_prima...
023700120504     d                 s              1
023800060710      **
023900060710     D Digits          C                   '0123456789'
024000120824     D Spedizione      C                   'S'
024100120824     D Segnacollo      C                   'T'
024200120824     D UNB_Chiodato    C                   'GEL_DE'
024300120824     d bart_it         C                   CONST('@Brt.it;')
024400121219     d CEDALERT_Brt    C                   CONST('CedAlert@brt.it')
024500120827     D*------------------
024600120827     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
024700120827     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
024800111006      *
024900060612      * ?================================================================== */
025000060612      *   Ciclo principale
025100060413      * ?********************************************************************/
025200060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
025300060612     C*
025400060612     C                   if        not %open(tivin00r)
025500060612     C                   open      tivin00r
025600060612     C                   endif
025700060614      **
025800060612      * ?              /*---------------------- */
025900060612     c                   exsr      Importa_Msg
026000060612      * ?              /*---------------------- */
026100060614
026200060612     C                   if        %open(tivin00r)
026300060612     C                   close     tivin00r
026400060612     C                   endif
026500120824      **
026600120824      ** Dopo aver scaricato dal TIVIN, si riorganizza i dati, li rilegge
026700120824      **  e li imposta sul VAB e sul VAT.
026800120824      * ?              /*---------------------- */
026900120824     c                   exsr      Scrive_IMPORT
027000120824      * ?              /*---------------------- */
027100060612      *
027200060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
027300121210     c                   if        almeno_uno ='S' and esito ='1'
027400121210     C                   eval      esito ='0'
027500121210     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import GEL_DE'
027600121210     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione BOLLE E-
027700130204     C                             DI GEL 3110012 Germania con errori. Avvisare-
027800121210     c                             AB.'
027900121210     c                   if            §PVinv <> 'N'
028000130315     c                              and manda_mail='S'
028100121210     c                   exsr      invio_mail
028200121210     c                   end
028300121210     C                   endif
028400120824      *
028500060612     c                   SETON                                        LR
028600060612      * ?------------------------------------------------------------------ */
028700060612      *? Importa i records della tramsissione
028800060612      * ?------------------------------------------------------------------ */
028900060612     c     Importa_Msg   Begsr
029000060612
029100060614      * per controllare se almeno un record è stato importato sul VAB
029200060614     c                   clear                   Almeno_Uno        1
029300130315     c                   clear                   manda_mail        1
029400060614
029500060612     c     *start        setll     Tivin00r
029600060612     c                   read      Tivin00r
029700060612
029800060612     c                   dow       not %eof(Tivin00r)
029900060614
030000060614      * solo i record sflaggati da rielaborare
030100060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
030200120827     C     minu:maiu     XLATE     VINDTA        vinDTA                         *Minus -> Maiuscolo
030300060612      ** Controlli formali sui campi
030400060612     c                   clear                   se_errore
030500060620     C                   clear                   Msg_Vin_80
030600120824     c                   clear                   err_bloccante     1
030700060612
030800060612      ** Decodifica record a campi variabili
030900060612      * ?              /*---------------------- */
031000060612     c                   exsr      Decod_Record
031100060612      * ?              /*---------------------- */
031200060612
031300060614      *  Problemi nella decodifica dei campi VAB/VAT
031400060614     c                   if        se_errore ='S'
031500060614     C                   eval      vinMSG = Msg_Vin_80
031600060614     c                   end
031700060621      *  Sempre Record OK
031800060621     C                   eval      vinFLG = '1'
031900120824
032000120824      *  x errore bloccante nella composizione del VAB/VAT
032100120824     c                   if        err_bloccante ='S'
032200120824     C                   eval      vinFLG = '2'
032300120824     c                   eval      esito  = '1'
032400120824     c                   end
032500060612
032600060612     c                   update    Tivin000
032700060614     c                   endIF
032800060612
032900060612     c                   read      Tivin00r
033000060612     C                   ENDdo
033100060612      **
033200060612     c                   endSR
033300060612      * ?------------------------------------------------------------------ */
033400060612      *?    Decodifica record a campi variabili
033500060612      * ?------------------------------------------------------------------ */
033600060612     c     Decod_Record  Begsr
033700060612
033800060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
033900060612     c                   movel(p)  VINDTA        dati
034000120305      **
034100060612      * ?              /*---------------------- */
034200060612     c                   exsr      Split_Rec
034300060612      * ?              /*---------------------- */
034400060612      ** conta le righe
034500060612      * ?              /*---------------------- */
034600060612     c                   exsr      Rec_Dettaglio
034700060612      * ?              /*---------------------- */
034800060612      **
034900060612     c                   Endsr
035000120305      * ?------------------------------------------------------------------ */
035100120305      *?      Riga Bolla Import da controllare e tradurre
035200120305      * ?------------------------------------------------------------------ */
035300120305     c     Rec_Dettaglio Begsr
035400120305      **
035500120823      **   CONTROLLA quale dei tipi ricord sta leggendo (S) o (T)
035600120823      **
035700060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
035800060612      **
035900120824     c                   clear                   edGEL00T
036000120824     c                   clear                   edGEL00S
036100120823     c                   clear                   edGEL000
036200060614     c                   clear                   Msg_Err
036300120824     c                   clear                   Testa_Sk
036400120823      **
036500120823      **  in Base a quanto ricevuto nel primo campo deve decodificare
036600120823      **     con la relativa DS (S) o (T)  alla fine scrive il record sul file
036700120823      **  EDGEL00F.
036800120823      **
036900120823      * ? Nel 1° campo : c'è sempre il TIPO RECORD   (Obbligatorio)  S o T
037000060612     C                   z-add     1             sk
037100120823     c                   If        LunDato_sk(sk) =  0
037200060612      * ?-----> Errore
037300130315     c                   eval       manda_mail='S'
037400060614     c                   eval      tipo_error = 1
037500060614     c                   exsr      Field_Error
037600060612     c                   Else
037700120823     c                   eval      STYPEREC   = %subst(Dato_sk(sk):1:+
037800060612     c                                              LunDato_sk(sk))
037900060612     c                   End
038000060612      **
038100120823      * ? Nel 2° campo : c'è sempre il DEPOT Mittente(Obbligatorio)  CODICE DEPOT
038200060612     C                   add       1             sk
038300120823     c                   If        LunDato_sk(sk) =  0
038400060612      * ?-----> Errore
038500120823     c                   eval      tipo_error = 1
038600060614     c                   exsr      Field_Error
038700060612     c                   Else
038800120823     c                   eval      SSNDDEPOT  = %subst(Dato_sk(sk):1:+
038900120823     c                                              LunDato_sk(sk))
039000060612     c                   End
039100060612      **
039200120823      **   qui si dividono le decodifiche dal 3° campo in poi.
039300120823      **
039400120824     c                   IF        STYPEREC   = Spedizione
039500120824     c                   clear                   Testa_Sk
039600120827     c                   z-add     0             num               3 0
039700120827     c                   z-add     0             x
039800120827     c                   eval      num = %elem(recS)
039900120827     c                   do        num           x
040000120827     c                   movel     recS(x)       Testa_Sk(x)
040100120827     c                   enddo
040200120823     c                   Exsr      Record_S
040300120823      **
040400120824     c                   elseIF    STYPEREC   = Segnacollo
040500120824     c                   clear                   Testa_Sk
040600120827     c                   z-add     0             num               3 0
040700120827     c                   z-add     0             x
040800120827     c                   eval      num = %elem(recT)
040900120827     c                   do        num           x
041000120827     c                   movel     recT(x)       Testa_Sk(x)
041100120827     c                   enddo
041200120823     c                   Exsr      Record_T
041300120823      **
041400120823     c                   End
041500120823      **
041600120823     c                   Endsr
041700120823      * ?------------------------------------------------------------------ */
041800120823      *?      Riga Bolla (S)
041900120823      * ?------------------------------------------------------------------ */
042000120823     c     Record_S      Begsr
042100120823      **
042200120823      * ? Nel 3° campo : DATA ARRIVO AL DEPOT     (Obbligatorio)
042300120823      **
042400060612     C                   add       1             sk
042500120824     c                   If        LunDato_sk(sk) > 0
042600120823     c                   eval      STIMEARR = %subst(Dato_sk(sk):1:+
042700120823     c                                              LunDato_sk(sk))
042800060612     c                   End
042900060612      **
043000120824      * ? Nel 4° campo : CODICE CLIENTE MITTENTE
043100120823      **
043200120823     C                   add       1             sk
043300120824     c                   If        DatoNum_sk(sk)<> 'N' and
043400120824     c                             LunDato_sk(sk) >  0
043500120827     c                   move      *all'0'       fld30            30
043600120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
043700120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
043800120827     c                   move      fld30         SSNDCODE
043900120823     c                   End
044000060612      **
044100120823      * ? Nel 5° campo : RAGSOC MITTENTE      (Obbligatorio)
044200120823      **
044300120823     C                   add       1             sk
044400120823     c                   If        LunDato_sk(sk) =  0
044500120823      * ?-----> Errore
044600120823     c                   eval      tipo_error = 1
044700120823     c                   exsr      Field_Error
044800120823     c                   Else
044900120823     c                   eval      SSNDNAME   = %subst(Dato_sk(sk):1:+
045000120823     c                                              LunDato_sk(sk))
045100120823     c                   End
045200120823      **
045300120823      * ? Nel 6° campo : RAGSOC MITTENTE ADD
045400120823      **
045500120823     C                   add       1             sk
045600120823     c                   If        LunDato_sk(sk) >  0
045700120823     c                   eval      SSNDNAMEAD = %subst(Dato_sk(sk):1:+
045800120823     c                                              LunDato_sk(sk))
045900120823     c                   End
046000060612      **
046100120824      * ? Nel 7° campo : INDIRIZZO MITTENTE
046200120823      **
046300060612     C                   add       1             sk
046400120824     c                   If        LunDato_sk(sk) >  0
046500120823     c                   eval      SSNDADDR   = %subst(Dato_sk(sk):1:+
046600060612     c                                              LunDato_sk(sk))
046700060612     c                   End
046800060612      **
046900120823      * ? Nel 8° campo :  NAZIONE MITTENTE         (Obbligatorio)
047000120823      **
047100120823     C                   add       1             sk
047200120823     c                   If        LunDato_sk(sk) =  0
047300120823      * ?-----> Errore
047400120823     c                   eval      tipo_error = 1
047500120823     c                   exsr      Field_Error
047600120823     c                   Else
047700120823     c                   eval      SSNDCOUNTR = %subst(Dato_sk(sk):1:+
047800120823     c                                              LunDato_sk(sk))
047900120823     c                   End
048000060612      **
048100120823      * ? Nel 9° campo : CAP MITTENTE              (Obbligatorio)
048200120823      **
048300120823     C                   add       1             sk
048400120823     c                   If        LunDato_sk(sk) =  0
048500120823      * ?-----> Errore
048600120823     c                   eval      tipo_error = 1
048700120823     c                   exsr      Field_Error
048800120823     c                   Else
048900120823     c                   eval      SSNDZIPCOD = %subst(Dato_sk(sk):1:+
049000120823     c                                              LunDato_sk(sk))
049100120823     c                   End
049200120823      **
049300120823      * ? Nel 10° campo : LOCALITÀ MITTENTE        (Obbligatorio)
049400120823      **
049500060612     C                   add       1             sk
049600120824     c                   If        LunDato_sk(sk) >  0
049700120823     c                   eval      SSNDCITY   = %subst(Dato_sk(sk):1:+
049800060612     c                                              LunDato_sk(sk))
049900060612     c                   End
050000060612      **
050100120823      * ? Nel 11° campo :  RIFERIMENTO CLIENTE    (Obbligatorio)
050200120823      **
050300120823     C                   add       1             sk
050400120823     c                   If        LunDato_sk(sk) =  0
050500120823      * ?-----> Errore
050600130315     c                   eval       manda_mail='S'
050700120823     c                   eval      tipo_error = 1
050800120823     c                   exsr      Field_Error
050900120823     c                   Else
051000120823     c                   eval      SSNDREFER  = %subst(Dato_sk(sk):1:+
051100120823     c                                              LunDato_sk(sk))
051200120823     c                   End
051300120823      **
051400120823      * ? Nel 12° campo :  DATA MANIFEST
051500120823      **
051600060612     C                   add       1             sk
051700120824     c                   If        LunDato_sk(sk) =  0
051800120824      * ?-----> Errore
051900120824     c                   eval      tipo_error = 1
052000120824     c                   exsr      Field_Error
052100120824     c                   Else
052200120824     c                   eval      SSNDDATE   = %subst(Dato_sk(sk):1:+
052300060612     c                                              LunDato_sk(sk))
052400060612     c                   End
052500060612      **
052600120823      * ? Nel 13° campo : RAGSOC DESTINATARIO     (Obbligatorio)
052700120823      **
052800060612     C                   add       1             sk
052900060612     c                   If        LunDato_sk(sk) =  0
053000060612      * ?-----> Errore
053100060614     c                   eval      tipo_error = 1
053200060614     c                   exsr      Field_Error
053300060612     c                   Else
053400120823     c                   eval      SRCPNAME   = %subst(Dato_sk(sk):1:+
053500060612     c                                              LunDato_sk(sk))
053600060612     c                   End
053700060612      **
053800120823      * ? Nel 14° campo : RAGSOC DESTINATARIO ADD
053900120823      **
054000060612     C                   add       1             sk
054100060612     c                   If        LunDato_sk(sk) >  0
054200120823     c                   eval      SRCPNAMEAD = %subst(Dato_sk(sk):1:+
054300060612     c                                              LunDato_sk(sk))
054400060612     c                   End
054500060612      **
054600120823      * ? Nel 15° campo :  INDIRIZZO DESTINATARIO  (Obbligatorio)
054700120823      **
054800120823     C                   add       1             sk
054900120823     c                   If        LunDato_sk(sk) =  0
055000120823      * ?-----> Errore
055100120823     c                   eval      tipo_error = 1
055200120823     c                   exsr      Field_Error
055300120823     c                   Else
055400120823     c                   eval      SRCPADDR   = %subst(Dato_sk(sk):1:+
055500120823     c                                              LunDato_sk(sk))
055600120823     c                   End
055700060612      **
055800120823      * ? Nel 16° campo :  NAZIONE DESTINATARIO  (Obbligatorio)
055900120823      **
056000120823     C                   add       1             sk
056100120823     c                   If        LunDato_sk(sk) =  0
056200120823      * ?-----> Errore
056300120823     c                   eval      tipo_error = 1
056400120823     c                   exsr      Field_Error
056500120823     c                   Else
056600120823     c                   eval      SRCPCOUNTR = %subst(Dato_sk(sk):1:+
056700120823     c                                              LunDato_sk(sk))
056800120823     c                   End
056900060612      **
057000120823      * ? Nel 17° campo : CAP DESTINATARIO
057100120823      **
057200060612     C                   add       1             sk
057300060612     c                   If        LunDato_sk(sk) >  0
057400120823     c                   eval      SRCPZIPCOD  = %subst(Dato_sk(sk):1:+
057500060612     c                                              LunDato_sk(sk))
057600060612     c                   End
057700060612      **
057800120823      * ? Nel 18° campo :  LOCALITÀ DESTINATARIO  (Obbligatorio)
057900120823      **
058000120823     C                   add       1             sk
058100120823     c                   If        LunDato_sk(sk) =  0
058200120823      * ?-----> Errore
058300120823     c                   eval      tipo_error = 1
058400120823     c                   exsr      Field_Error
058500120823     c                   Else
058600120823     c                   eval      SRCPCITY  = %subst(Dato_sk(sk):1:+
058700120823     c                                              LunDato_sk(sk))
058800120823     c                   End
058900120823      **
059000120823      * ? Nel 19° campo :   NR.COLLI SPEDIZIONE  (Obbligatorio)
059100120823      **
059200120823     C                   add       1             sk
059300120823     c                   If        DatoNum_sk(sk) = 'N' or
059400120823     c                             LunDato_sk(sk) =  0
059500120823      * ?-----> Errore
059600120823     c                   eval      tipo_error = 2
059700120823     c                   exsr      Field_Error
059800120823     c                   Else
059900120827     c                   move      *all'0'       fld30            30
060000120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
060100120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
060200120827     c                   move      fld30         SNRPARCELS
060300120823     c                   End
060400120823      **
060500120823      * ? Nel 20° campo :   SEMPRE (N)
060600120823      **
060700060612     C                   add       1             sk
060800120823     c                   If        LunDato_sk(sk) >  0
060900120823     c                   eval      SXXLDIMENS = %subst(Dato_sk(sk):1:+
061000060612     c                                              LunDato_sk(sk))
061100060612     c                   End
061200060612      **
061300120823      * ? Nel 21° campo :  VUOTO
061400120823      **
061500120823     C                   add       1             sk
061600120823     c                   If        LunDato_sk(sk) >  0
061700120827     c                   move      *all'0'       fld30            30
061800120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
061900120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
062000120827     c                   move      fld30         SXXLPARCEL
062100120823     c                   End
062200060612      **
062300120823      * ? Nel 22° campo : PESO SENZA VIRG.CON 1 DEC
062400120823      **
062500120823     C                   add       1             sk
062600120823     c                   If        DatoNum_sk(sk) = 'N' or
062700120823     c                             LunDato_sk(sk) =  0
062800120823      * ?-----> Errore
062900120823     c                   eval      tipo_error = 2
063000120823     c                   exsr      Field_Error
063100120823     c                   Else
063200120827     c                   move      *all'0'       fld30            30
063300120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
063400120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
063500120827     c                   move      fld30         SREALWEIGH
063600120823     c                   End
063700120823      **
063800121016      * ? Nel 23° campo : PESO TASSABILE VOLUME*170 SENZA VIRG.CON 1 DEC
063900120823      **
064000120823     C                   add       1             sk
064100120823     c                   If        DatoNum_sk(sk) = 'N' or
064200120823     c                             LunDato_sk(sk) =  0
064300120823      * ?-----> Errore
064400120823     c                   eval      tipo_error = 2
064500120823     c                   exsr      Field_Error
064600120823     c                   Else
064700120827     c                   move      *all'0'       fld30            30
064800120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
064900120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
065000120827     c                   move      fld30         SVOLFROMWG
065100120823     c                   End
065200120823      **
065300120823      * ? Nel 24° campo :  SEMPRE -> O OVERNIGHT
065400120823      **
065500120823     C                   add       1             sk
065600120823     c                   If        LunDato_sk(sk) >  0
065700120823     c                   eval      SSERVICE   = %subst(Dato_sk(sk):1:+
065800120823     c                                              LunDato_sk(sk))
065900120823     c                   End
066000120823      **
066100120823      * ? Nel 25° campo :  SEMPRE -> N ?????
066200120823      **
066300120823     C                   add       1             sk
066400120823     c                   If        LunDato_sk(sk) >  0
066500120823     c                   eval      SAUTOMPOD  = %subst(Dato_sk(sk):1:+
066600120823     c                                              LunDato_sk(sk))
066700120823     c                   End
066800120823      **
066900120823      * ? Nel 26° campo : VUOTO
067000120823      **
067100120823     C                   add       1             sk
067200120823     c                   If        LunDato_sk(sk) >  0
067300120823     c                   eval      SSPECIALAD = %subst(Dato_sk(sk):1:+
067400120823     c                                              LunDato_sk(sk))
067500120823     c                   End
067600120823      **
067700120823      * ? Nel 27° campo : VUOTO
067800120823      **
067900120823     C                   add       1             sk
068000120823     c                   If        LunDato_sk(sk) >  0
068100120823     c                   eval      STIMELATER = %subst(Dato_sk(sk):1:+
068200120823     c                                              LunDato_sk(sk))
068300120823     c                   End
068400120823      **
068500120823      * ? Nel 28° campo : SEMPRE -> Y PORTO FRANCO
068600120823      **
068700120823     C                   add       1             sk
068800120823     c                   If        LunDato_sk(sk) >  0
068900120823     c                   eval      SPORTOF    = %subst(Dato_sk(sk):1:+
069000120823     c                                              LunDato_sk(sk))
069100120823     c                   End
069200120823      **
069300120823      * ? Nel 29° campo : VALORE CONTRASSEGNO
069400120823      **
069500120823     C                   add       1             sk
069600120823     c                   If        LunDato_sk(sk) >  0
069700120827     c                   move      *all'0'       fld30            30
069800120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
069900120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
070000120827     c                   move      fld30         SCODAMOUNT
070100120823     c                   End
070200120823      **
070300120823      * ? Nel 30° campo : VUOTO
070400120823      **
070500120823     C                   add       1             sk
070600120823     c                   If        LunDato_sk(sk) >  0
070700120823     c                   eval      SCHEQUE    = %subst(Dato_sk(sk):1:+
070800120823     c                                              LunDato_sk(sk))
070900120823     c                   End
071000120823      **
071100120823      * ? Nel 31° campo : VALORE ASSICURAZIONE
071200120823      **
071300120823     C                   add       1             sk
071400120823     c                   If        LunDato_sk(sk) >  0
071500120827     c                   move      *all'0'       fld30            30
071600120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
071700120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
071800120827     c                   move      fld30         SINSURANCE
071900120823     c                   End
072000120823      **
072100121207      * ? Nel 32° campo : NOTE 1    se Contiene "RETURN" è una bolla di reso
072200121207      * ?    seguito dalla nostro numero BOLLA di export restituita.
072300120823      **
072400120823     C                   add       1             sk
072500120823     c                   If        LunDato_sk(sk) >  0
072600120823     c                   eval      SNOTE1     = %subst(Dato_sk(sk):1:+
072700120823     c                                              LunDato_sk(sk))
072800120823     c                   End
072900120823      **
073000120823      * ? Nel 33° campo : NOTE 2
073100120823      **
073200120823     C                   add       1             sk
073300120823     c                   If        LunDato_sk(sk) >  0
073400120823     c                   eval      SNOTE2     = %subst(Dato_sk(sk):1:+
073500120823     c                                              LunDato_sk(sk))
073600121030      ** nel campo lungo 50
073700121030      **   il 30/10/2012 si è definito :
073800121030      **    1  1   flag blk/"A" per Appuntamento
073900121030      **    2 19   il numero del contatto telefonico
074000121030      **   20 50   il nome della persona da contattare x la consegna.
074100121030     ***
074200120823     c                   End
074300120823      **
074400120823      * ? Nel 34° campo :  RIFERIMENTO SPEDIZIONE   (Obbligatorio)
074500120823      **
074600120823     C                   add       1             sk
074700120823     c                   If        DatoNum_sk(sk) = 'N' or
074800120823     c                             LunDato_sk(sk) =  0
074900120823      * ?-----> Errore
075000130315     c                   eval       manda_mail='S'
075100120823     c                   eval      tipo_error = 2
075200120823     c                   exsr      Field_Error
075300120823     c                   Else
075400120827     c                   move      *all'0'       fld30            30
075500120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
075600120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
075700120827     c                   move      fld30         SSHPNUMBER
075800120823     c                   End
075900120823      **
076000120823      * ? Nel 35° campo :  DEPOT DESTINATARIO
076100120823      **
076200120823     C                   add       1             sk
076300120823     c                   If        LunDato_sk(sk) >  0
076400120823     c                   eval      SRCPDEPOT  = %subst(Dato_sk(sk):1:+
076500120823     c                                              LunDato_sk(sk))
076600120823     c                   End
076700120823      **
076800121207      * ? Nel 36° campo :  per GESTIONE RESI / BOLLE DA ORM se ="160"
076900121207      * ?      se nel 32° campo c'è "RETURN" si tratta di un RESO altrimenti è una BOLLA da ORM
077000121207      * ?       e nel 11° campo c'è il riferimento del nostro ORM.
077100120823      **
077200121008     C                   add       1             sk
077300121008     c                   If        LunDato_sk(sk) >  0
077400121008     c                   eval      SORDEPOT  = %subst(Dato_sk(sk):1:+
077500121008     c                                              LunDato_sk(sk))
077600120823     c                   End
077700121207      **
077800121207      * ? Nel 37° campo :
077900120823      **
078000120823     C                   add       1             sk
078100120823     c                   If        LunDato_sk(sk) >  0
078200120827     c                   move      *all'0'       fld30            30
078300120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
078400120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
078500120827     c                   move      fld30         SPICKUPREF
078600121008      **
078700120823     c                   End
078800121207      **
078900121207      * ?----->  INCONGRUENZE principali fra campi (COME REGOLE)
079000121207      **            x determinare BOLLE DI RESO o BOLLE DA ORM
079100121207     c                   if        SORDEPOT  = '160'
079200121207      **
079300121207     c                   if        %Subst(SNOTE1:1:6) <> 'RETURN'
079400140303      ************
079500140303      **   NON MANDANO più i trattini nel riferimento ORM quindi NON si deve emettere l'errore.
079600140303      ************
079700121207      ***  es.:159-70696-00   --> in tal caso imposto nelle note del VAB "DA ORM:159-70696-00"
079800140303     c**** '-'           scan      SSNDREFER                              11
079900121207      * se ha trovato almeno un trattino è probabile sia il nostro ORM
080000140303     c**********         if        not *in11
080100121207      * ?-----> Errore
080200140303     c**********         eval      tipo_error = 6
080300140303     c**********         exsr      Field_Error
080400140303     c**********         end
080500140303      **
080600121207     c                   end
080700121207      **
080800121207     c                   else
080900121207     c                   if        %Subst(SNOTE1:1:6) = 'RETURN'
081000121207      * ?-----> Errore
081100121207     c                   eval      tipo_error = 5
081200121207     c                   exsr      Field_Error
081300121207      *
081400121207     c                   else
081500121207      ***  es.:159-70696-00   --> in tal caso imposto nelle note del VAB "DA ORM:159-70696-00"
081600121207     c     '-'           scan      SSNDREFER                              11
081700121207      * se ha trovato almeno un trattino è probabile sia il nostro ORM
081800121207     c                   if        *in11
081900121207      * ?-----> Errore
082000121207     c                   eval      tipo_error = 7
082100121207     c                   exsr      Field_Error
082200121207     c                   end
082300121207     c                   endif
082400121207      ***
082500121207     c                   end
082600120823      **
082700121207      **  Attenzione se si tratta di un ORM non  eseguito lo deve SEGNALARE
082800121207     c     'LEERFAHRT'   scan      SNOTE1                                 11
082900121207     c                   if        *in11
083000121207      * ?-----> Errore
083100121207     c                   eval      tipo_error = 8
083200121207     c                   exsr      Field_Error
083300121207     c                   end
083400121207      **
083500060612      **
083600120823      * ?----->  HA I CAMPI IMPOSTATI SULLA DS
083700060612      **
083800120823     c                   write     edGEL000
083900120823      **
084000060612     c                   Endsr
084100060614      * ?------------------------------------------------------------------ */
084200120824      *?      Riga Collo (T)
084300120824      * ?------------------------------------------------------------------ */
084400120824     c     Record_T      Begsr
084500120824      **
084600120824      * ? Nel 3° campo : BARCODE                  (Obbligatorio)
084700120824      **
084800120824     C                   add       1             sk
084900120824     c                   If        LunDato_sk(sk) =  0
085000120824      * ?-----> Errore
085100120824     c                   eval      tipo_error = 1
085200120824     c                   exsr      Field_Error
085300120824     c                   Else
085400120824     c                   eval      TBARCODE = %subst(Dato_sk(sk):1:+
085500120824     c                                              LunDato_sk(sk))
085600120824     c                   End
085700120824      **
085800120824      * ? Nel 4° campo :  RIFERIMENTO SPEDIZIONE   (Obbligatorio)
085900120824      **
086000120824     C                   add       1             sk
086100120824     c                   If        DatoNum_sk(sk) = 'N' or
086200120824     c                             LunDato_sk(sk) =  0
086300120824      * ?-----> Errore
086400130315     c                   eval       manda_mail='S'
086500120824     c                   eval      tipo_error = 2
086600120824     c                   exsr      Field_Error
086700120824     c                   Else
086800120827     c                   move      *all'0'       fld30            30
086900120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
087000120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
087100120827     c                   move      fld30         SSHPNUMBER
087200120824     c                   End
087300120824      **
087400120824      * ? Nel 5° campo :  VUOTO
087500120824      **
087600120824     C                   add       1             sk
087700120824     c                   If        LunDato_sk(sk) >  0
087800120824     c                   eval      TEMPTY1    = %subst(Dato_sk(sk):1:+
087900120824     c                                              LunDato_sk(sk))
088000120824     c                   End
088100120824      **
088200120824      * ? Nel 6° campo :  VUOTO
088300120824      **
088400120824     C                   add       1             sk
088500120824     c                   If        LunDato_sk(sk) >  0
088600120824     c                   eval      TEMPTY2    = %subst(Dato_sk(sk):1:+
088700120824     c                                              LunDato_sk(sk))
088800120824     c                   End
088900120824      **
089000120824      * ? Nel 7° campo : VUOTO
089100120824      **
089200120824     C                   add       1             sk
089300120824     c                   If        DatoNum_sk(sk)<> 'N' and
089400120824     c                             LunDato_sk(sk) >  0
089500120827     c                   move      *all'0'       fld30            30
089600120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
089700120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
089800120827     c                   move      fld30         TEMPTY3
089900120824     c                   End
090000120824      **
090100120824      * ? Nel 8° campo : VUOTO
090200120824      **
090300120824     C                   add       1             sk
090400120824     c                   If        DatoNum_sk(sk)<> 'N' and
090500120824     c                             LunDato_sk(sk) >  0
090600120827     c                   move      *all'0'       fld30            30
090700120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
090800120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
090900120827     c                   move      fld30         TEMPTY4
091000120824     c                   End
091100120824      **
091200120824      * ? Nel 9° campo :  VUOTO
091300120824      **
091400120824     C                   add       1             sk
091500120824     c                   If        LunDato_sk(sk) >  0
091600120824     c                   eval      TEMPTY5    = %subst(Dato_sk(sk):1:+
091700120824     c                                              LunDato_sk(sk))
091800120824     c                   End
091900120824      **
092000120824      * ? Nel 10° campo :   NR.PROGRESSIVO COLLO    (Obbligatorio)
092100120824      **
092200120824     C                   add       1             sk
092300120824     c                   If        DatoNum_sk(sk) = 'N' or
092400120824     c                             LunDato_sk(sk) =  0
092500120824      * ?-----> Errore
092600120824     c                   eval      tipo_error = 2
092700120824     c                   exsr      Field_Error
092800120824     c                   Else
092900120827     c                   move      *all'0'       fld30            30
093000120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
093100120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
093200120827     c                   move      fld30         TPACKAGENR
093300120824     c                   End
093400120824      **
093500120824      * ? Nel 11° campo :  DEPOT DESTINATARIO
093600120824      **
093700120824     C                   add       1             sk
093800120824     c                   If        LunDato_sk(sk) >  0
093900120824     c                   eval      SRCPDEPOT  = %subst(Dato_sk(sk):1:+
094000120824     c                                              LunDato_sk(sk))
094100120824     c                   End
094200120824      **
094300120824      * ? Nel 12° campo : LENGTH IN CM DECIMAL O
094400120824      **
094500120824     C                   add       1             sk
094600120824     c                   If        DatoNum_sk(sk)<> 'N' and
094700120824     c                             LunDato_sk(sk) >  0
094800120827     c                   move      *all'0'       fld30            30
094900120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
095000120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
095100120827     c                   move      fld30         TLENGHTCM
095200120824     c                   End
095300120824      **
095400120824      * ? Nel 13° campo : WIDTH IN CM DECIMAL 0
095500120824      **
095600120824     C                   add       1             sk
095700120824     c                   If        DatoNum_sk(sk)<> 'N' and
095800120827     c                             LunDato_sk(sk) >  0
095900120827     c                   move      *all'0'       fld30            30
096000120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
096100120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
096200120827     c                   move      fld30         TWIDTHCM
096300120824     c                   End
096400120824      **
096500120824      * ? Nel 14° campo :  HEIGHT IN CM DECIMAL 0
096600120824      **
096700120824     C                   add       1             sk
096800120824     c                   If        DatoNum_sk(sk)<> 'N' and
096900120824     c                             LunDato_sk(sk) >  0
097000120827     c                   move      *all'0'       fld30            30
097100120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
097200120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
097300120827     c                   move      fld30         THEIGHTCM
097400120824     c                   End
097500120824      **
097600120824      * ? Nel 15° campo :   PESO REALE DEL COLLO
097700120824      **
097800120824     C                   add       1             sk
097900120824     c                   If        DatoNum_sk(sk)<> 'N' and
098000120824     c                             LunDato_sk(sk) >  0
098100120827     c                   move      *all'0'       fld30            30
098200120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
098300120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
098400120827     c                   move      fld30         TREALWEIGH
098500120824     c                   End
098600120824      **
098700120824      * ? Nel 16° campo :   VOLUME DEL COLLO
098800120824      **
098900120824     C                   add       1             sk
099000120824     c                   If        DatoNum_sk(sk)<> 'N' and
099100120824     c                             LunDato_sk(sk) >  0
099200120827     c                   move      *all'0'       fld30            30
099300120827     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
099400120827     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
099500120827     c                   move      fld30         TVOLFROMWG
099600120824     c                   End
099700120824      **
099800120824      **
099900120824      * ?----->  HA I CAMPI IMPOSTATI SULLA DS
100000120824      **
100100120824     c                   write     edGEL000
100200120824      **
100300120824     c                   Endsr
100400120824      * ?------------------------------------------------------------------ */
100500060614      *?      Campi in errore
100600060614      * ?------------------------------------------------------------------ */
100700060614     c     Field_Error   Begsr
100800060614      **
100900060614     C                   eval      se_errore   = 'S'
101000121207     c                   eval      err_bloccante ='S'
101100060614      * msg video o in posta
101200060614     c                   If        Msg_Err = *blank
101300060614     c
101400060614     c                   Select
101500060614     c
101600060614     c                   When      Tipo_Error = 1
101700060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
101800060614     C                             ' >> Obblig.- Manca Dato !!'
101900060614     c
102000060614     c                   When      Tipo_Error = 2
102100060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
102200060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
102300060614     c
102400060614     c                   When      Tipo_Error = 3
102500060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
102600060614     C                             ' >> NON Numerico  !!'
102700060614     c
102800060614     c                   When      Tipo_Error = 4
102900060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
103000060614     C                             ' >> Conversione non riuscita !!'
103100121207     c
103200121207     c                   When      Tipo_Error = 5
103300121207     C                   eval      Msg_Err = 'RETURN :RESO non configurato corr-
103400121207     C                             ettamente!/ERRORE CONTROLLARE il record.'
103500121207     c
103600121207     c                   When      Tipo_Error = 6
103700121207     C                   eval      Msg_Err = 'DA ORM :Inviato senza nostro Rife-
103800121207     C                             rimento ORM!-manca il campo di RIFERIMENTO.'
103900121207     c
104000121207     c                   When      Tipo_Error = 7
104100121207     C                   eval      Msg_Err = 'DEPOT <> 160. NON PREVISTO x RESO-
104200121207     C                              o Bolla da ORM.'
104300121207     c
104400121207     c                   When      Tipo_Error = 8
104500121207     C                   eval      Msg_Err = 'NON è una BOLLA. Ma una segnalazi-
104600121207     C                             one di un ORM non eseguito da GEL.'
104700121207     c
104800060614     c                   EndSL
104900060614     c
105000060614     c                   End
105100060614      * Errore su VINMSG
105200121207     c                   if        Msg_Err <> ' '
105300121207     C                   eval      Msg_Vin_80 = %trim(Msg_Err)   + ';'
105400060614     c                                          + %trim(Testa_sk(sk))
105500120824     c                   else
105600120824     C                   eval      Msg_Vin_80 = %trim(Testa_sk(sk))
105700120824     c                   End
105800060614      **
105900060614     c                   Endsr
106000060612      * ?------------------------------------------------------------------ */
106100060612      *?      Suddivide i campi della riga con carattere divisorio (;)
106200060612      * ?------------------------------------------------------------------ */
106300060612     c     Split_Rec     Begsr
106400060612      **
106500060612     c                   clear                   Nr_campi
106600060612     c                   clear                   Campi_Record
106700060612     c                   clear                   Campi_Lunghi
106800060612     c                   clear                   Campi_Numerici
106900060612     c                   clear                   Campi_Decimali
107000060612     c                   movel     ';'           Separa_campi
107100120823     c                   movel     '.'           Decimal_Separ
107200120824      **
107300120824      *  Essendo un file CSV come DPD richiama il FIEU anzichè il TRTCT
107400120824      *
107500060612     c                   call      'FIEU00R'
107600060612      * ?     Input
107700060612     c                   parm                    Separa_campi
107800060612     c                   parm                    Decimal_Separ
107900060612     c                   parm                    dati
108000060612      * ?     Output  su schiere di 100 elementi
108100060612     c                   parm                    Nr_campi
108200060612     c                   parm                    Campi_Record                   * i dati
108300060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
108400060612     c                   parm                    Campi_Numerici                 * se numerici
108500060612     c                   parm                    Campi_Decimali                 * quanti decimali
108600060612      **
108700060612     c                   Endsr
108800120824      * ?------------------------------------------------------------------ */
108900120824      *?  Rilegge il file che si è creato e  scrive il VAB
109000120824      * ?------------------------------------------------------------------ */
109100120824     c     Scrive_IMPORT Begsr
109200120824
109300120824      * per controllare se almeno un record è stato importato sul VAB
109400120824     c                   clear                   Almeno_Uno        1
109500120824
109600120824     c     *start        setll     EdGEL00f
109700120824     c                   read      EdGEL00f
109800120824
109900120824     c                   dow       not %eof(EdGEL00f)
110000120827      *
110100120824      * ?              /*---------------------- */
110200120824     c                   exsr      WRT_VAB_VAT
110300120824      * ?              /*---------------------- */
110400121107     c*
110500121107     c                   COMMIT
110600121107     c*
110700120824     c                   read      EdGEL00f
110800120824     C                   ENDdo
110900120824      **
111000120824     c                   endSR
111100060612      * ?================================================================== */
111200060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
111300060612      * ?================================================================== */
111400060612     C     WRT_VAB_VAT   BEGSR
111500120824      **
111600120824      **----------
111700120824      **  Se si tratta del record BOLLA (S)
111800120824     c                   IF        STYPEREC   = Spedizione
111900120824      **----------
112000060612      * Clearizzo EDiVAB in quanto ha gli stessi campi di FiVAB più
112100060612      *  alcuni specifici
112200060612     c                   clear                   edivab00
112300060612     c                   clear                   fivab000
112400120827
112500120827      **  x eliminare gli zeri inutili davanti al
112600120827     C*    numero di riferimento della Spedizione
112700120827     C                   clear                   allinea_shpNbr
112800120827     C                   clear                   T
112900120827     C                   MOVE      SSHPNUMBER    allinea_shpNbr
113000120827     C                   MOVEA     allinea_shpNbrshp15
113100120827     C                   DO        15            T
113200120827     C     shp15(T)      IFEQ      '0'
113300120827     c                   eval      shp15(T) = ' '
113400120827     c                   ELSE
113500120827     c                   leave
113600120827     C                   ENDIF
113700120827     C                   ENDDO
113800120827     C                   movea(p)  shp15(T)      allinea_shpNbr
113900120824      **
114000120824      **  Salva il riferimento Spedizione x confrontare le righe successive
114100120824      **      di dettaglio colli
114200120827     c                   eval         Salva_shpNbr = allinea_shpNbr
114300120824      **
114400120824     c                   exsr      Scrive_VAB
114500120824      **----------
114600120824      **  Se si tratta del record COLLO (T)
114700120824     c                   elseIF    STYPEREC   = Segnacollo
114800120824      **----------
114900120824      *
115000120824      * procede solo il dettaglio colli appartiene alla testata di spedizione
115100120827     c                   if           Salva_shpNbr = allinea_shpNbr
115200120824      * Clearizzo EDiVAT in quanto ha gli stessi campi di FiVAT più
115300120824      *  alcuni specifici
115400120824     c                   clear                   edivat00
115500120824     c                   clear                   fivat000
115600120824      **
115700120824     c                   exsr      Scrive_VAT
115800120824      **
115900120824     c                   End
116000120824      **----------
116100120824     c                   Endif
116200120824      **----------
116300120824      **
116400120824     c                   EndSR
116500060612      **?------------------------------------------------------------------ */
116600120824      **  utilizza parte dei metodi di traduttori EDIFACT - EDI DA IFCSUM
116700060612      **?------------------------------------------------------------------ */
116800120824     c     Scrive_VAB    BEGSR
116900091106      *
117000120824      * ?Data  spedizione
117100120824     C                   MOVEL     UMONTH        VABMGS                         = oggi
117200120824     C                   MOVE      Uday          VABMGS                         = oggi
117300120824      *
117400120824     C                   MOVEL     §PTKSC        VABccm
117500120824     C                   MOVEL     §PTCTR        VABctr
117600120824     C                   MOVEL     §PTlnp        VABfgs
117700120824     C                   MOVEL     §PTlnp        VABlnp
117800120824     C                   MOVEL     §PTnrs        VABnrs
117900120824     C                   MOVEL     §PTctm        VABctm
118000120824      * Dati mittente
118100120824     C                   eval      VABRMO = SSNDNAME + SSNDNAMEad
118200120824      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
118300120824     C                   movel     §PTNAZ        VABNMO
118400120824      *
118500120824     C                   IF          SSNDCOUNTR <> *blanks
118600120824     C                   z-add     1             yy                3 0
118700120824     C     SSNDCOUNTR    LOOKUP    ISO(YY)                                32
118800120824     C                   if         *in32 = *on
118900120824     C                   MOVEL     C15(YY)       VABNMO
119000120824     C                   endif
119100120824     C                   ENDIF
119200120824      *
119300120824      * Normalizzo CAP mittente originale
119400120824     C                   clear                   T
119500120824     C                   clear                   S                 3 0
119600120824     C                   MOVEA     *BLANKS       CAPM
119700120824     C                   MOVEA     SSNDZIPCOD    CAP7
119800120824     C                   DO        7             T                 3 0
119900120824     C     CAP7(T)       IFNE      *BLANKS
120000120824     C                   ADD       1             S
120100120824     C                   MOVE      CAP7(T)       CAPM(S)
120200120824     C                   ENDIF
120300120824     C                   ENDDO
120400120824     C*
120500120824      *     Controllo validità CAP
120600120824     C                   MOVEA     CAPM          VABCMO
120700120824      *
120800120824     C                   CLEAR                   TISI95DS
120900120824     C                   MOVEL     VABCMO        I95CAP
121000120824     C                   MOVEL     VABNMO        I95NAR
121100120824     C                   MOVEL     WOGGI         I95DAT
121200120824     C                   MOVEL     '3'           I95TCN
121300120824     C                   CALL      'TISI95R'
121400120824     C                   PARM                    TISI95DS
121500120824      *     Errore
121600120824     C     O95ERR        IFNE      *BLANKS
121700120824     C                   MOVEL     INDCAE        VABCMO
121800120824     C                   MOVEL     §PTNAZ        VABNMO
121900120824     C                   END
122000120824      *
122100120824      *  Riferimento Originale della Spedizione
122200120824     C                   z-add     SSHPNUMBER    vabrmn
122300120827     C                   movel     allinea_shpNbrvabrma
122400120824     C******* ????       movel     SSNDREFER     vabrma
122500120824      *
122600060612      * ?Dati  destinatario
122700120824     c                   movel     SRCPNAME      vabrsd                         Ragione Sociale
122800120824     C                   movel     SRCPNAMEad    vabrd2                         rag.soc.2
122900120824     c                   movel     SRCPADDR      vabind                         Indirizzo
123000120824     c                   movel     SRCPCITY      vablod                         Località
123100120824     c                   movel     SRCPZIPCOD    vabcad                         C.A.P.
123200120827     c                   movel     SRCPCOUNTR    vabnzd                         NAZIONE
123300151029     c                   if        vabnzd = 'I' or
123400151029     c                             vabnzd = 'IT'
123500121017     c                   clear                   vabNZD
123600121017     c                   end
123700120824      * ?la provincia la calcolo
123800070202     C                   CLEAR                   TISI95DS
123900070202     C                   MOVEL     vabCAD        I95CAP
124000070202     C                   MOVEL     DATEU         I95DAT
124100120827     C                   MOVEL     vabNZD        I95NAR
124200120827     C                   MOVEL     vabLOD        I95LOC
124300070202     C                   MOVEL     '3'           I95TCN
124400070202     C                   CALL      'TISI95R'
124500070202     C                   PARM                    TISI95DS
124600070202      *     Errore
124700070202     C     O95ERR        IFeq      *BLANKS
124800070202     C                   MOVEL     O95PRV        vabPRD
124900070202     C                   END
125000070202      *
125100060612      * ?Tipo servizio
125200060612     c                   movel     'C'           vabtsp
125300060612      * ?Colli
125400120824     c                   z-add     SNRPARCELS    vabncl
125500060612      *
125600120824      * ?Peso     senza virgola con 1 decimale
125700120824     c     SREALWEIGH    div       10            vabPKB
125800120824      *
125900120824      * ?Volume   senza virgola con 1 decimale
126000121005      *** non è il volume bensì il peso da volume........
126100121016      ***   ha un moltiplicatore (170) rapporto peso/volume ed inoltre
126200121016      ***   è moltiplicato 10 per una virgola decimale
126300121016      ***   quindi per ricavare il volume devo dividere x 170 e x 10
126400121016      ***   ossia x 1700.
126500121016     c     SVOLFROMWG    div       1700          vabVLB
126600121016     c*
126700120824      *
126800120824      * ?Controlla il Contrassegno
126900120824     C     §PUCAS        IFEQ      'S'                                          C/Assegno
127000120905     C     SCODAMOUNT    div       100           vabCAS                          div 100
127100121105      ** non viene passata la divisa del contrassegno
127200121105     C     vabcas        ifGT      *ZEROS
127300121105     c                   eval      vabvca = 'EUR'
127400121105     c                   end
127500120905      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO                                hanno 2 dec.
127600120824      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
127700120824     C     §PUTPC        IFEQ      'P'
127800120824     C     vabcas        ANDGT     *ZEROS
127900120824     C                   MOVEL     'OM'          VABTIC
128000120824     C                   END
128100120824     C                   END
128200060612      *
128300120824      * ?Controlla importo assicurato
128400121105     C*******            if        SINSURANCE > 0
128500121105     C*******            z-add     SINSURANCE    VABIAS
128600121105      ** non viene passata la divisa del importo assicurato.
128700121105     c*******            eval      vabvas = 'EUR'
128800121105     C*******            END
128900120824      *
129000060612      * ?Codice bolla
129100120924     C     §puPFA        ifNE      *blank
129200120924     c                   eval      flgCBO = §puPFA
129300120924     c                   end
129400120924      *
129500120924      *    Porto Franco = 'Y'
129600120924     c                   if        SPORTOF = 'Y'
129700120924     C                   MOVEL     '1'           vabCBO                         NO C/assegno
129800120924     c                   eval      flgCBO = 'F'
129900120924     c                   end
130000120924      *
130100120924      *    Porto Assegnato  se ha un ORM collegato   se c'è il n° Ordine allora è un ASSEGNATO
130200120924      *  GEL DISTINGUE in questo modo mentre nel flag del PORTO FRANCO c'è sempre PORTO FRANCO.
130300121207      *
130400121207      *    a questo punto occorre testare se vi era un ORM tramite il SORDEPOT
130500121207      **    ci deve essere come DEPOT --> 160
130600121207     c                   if        SORDEPOT <> *blank and SORDEPOT = '160'
130700121207      *
130800121207     c                   If            %Subst(SNOTE1:1:6) <> 'RETURN'
130900121127      *
131000120924     C                   MOVEL     '2'           vabCBO                         NO C/assegno
131100120924     c                   eval      flgCBO = 'A'
131200121207      **
131300121127      ***
131400121127      *** Inoltre imposto il riferimento nostro ORM se trovo nel SSNDREFER un codice separato
131500121127      *** da dei trattini come usa fare GEL altrimenti non faccio nulla.
131600121127      ***  es.:159-70696-00   --> in tal caso imposto nelle note del VAB "DA ORM:159-70696-00"
131700140303     c**** '-'           scan      SSNDREFER                              11
131800121127      * se ha trovato almeno un trattino allora imposta la NOTA dell'ORM
131900140303     c************       if        *in11
132000140303      *
132100140303      *   imposta il riferimento così com'è
132200121127     C                   eval        VABNOT = 'DA ORM:' + %Trim(SSNDREFER)
132300140303      *
132400140303     c************       end
132500140303     c************
132600140303     c************   Non mandano più i trattini per distingure il codice ORM nei sottocomponenti
132700140303     c************    quindi mettiamo direttamente il codice nelle note così come viene passato.
132800140303     c************
132900121207      **
133000121207      **  Attenzione se si tratta di un ORM non  eseguito lo deve SEGNALARE
133100121207     c     'LEERFAHRT'   scan      SNOTE1                                 11
133200121207     c                   if        *in11
133300121207      * ?-----> si TRATTA di un NOSTRO ORM che GEL NON HA FATTO
133400121207     C                   eval        VABNOT = 'ORM non FATTO:'+ %Trim(SSNDREFER)
133500121207     c                   end
133600120824      *
133700120924      *  Se si tratta di un RESO: nel campo NOTE Special Information viene impostato
133800120924      *   RETURN: ed il codice del nostro riferimento.
133900120924      *    Quindi si tratta di un assegnato.
134000120924      *   NELLA RAG.SOCIALE Destinatario aggiungo per facilitare l'interpretazione
134100120924      *    di una bolla da trattare come reso --> "RESO A:" spostando a destra la rag.soc.dest.
134200121207      **    ci deve essere come DEPOT --> 160
134300121207     c                   elseIF    %Subst(SNOTE1:1:6) = 'RETURN'
134400120924      **
134500120924     C                   MOVEL     '2'           vabCBO                         NO C/assegno
134600120924     c                   eval      flgCBO = 'A'
134700120924      **
134800120924      **   Sposta il nome del destinatario
134900120924     c                   eval      savRSD = vabrsd
135000120924     c                   eval      savRD2 = vabrd2
135100120924     c                   eval      vabrsd = 'RESO A: ' + savrsd
135200120924     c                   eval      vabrd2 = %subst(savrsd:28:8) + savrd2
135300120924      **
135400120924     c                   end
135500121207      **
135600121207     c                   endIF
135700120924      *
135800120824      *  imposta il default dalla tabella PT se c'è
135900120924     C     flgCBO        ifNE      *blank
136000120824     c                   SELECT
136100120824      *  Porto Franco
136200120924     c                   WHEN      flgCBO = 'F'  and  VABcas > *zeros
136300120824     C                   movel     '4'           VABCBO                         + C/Ass
136400120924     c                   WHEN      flgCBO = 'F'
136500120824     C                   movel     '1'           VABCBO                         Senza C/Ass.
136600120824      *  Porto Assegnato
136700120924     c                   WHEN      flgCBO = 'A'  and  VABCAS > *zeros
136800120824     C                   movel     '6'           VABCBO                         + C/Ass
136900120924     C                   WHEN      flgCBO = 'A'
137000120824     C                   movel     '2'           VABCBO                         Senza C/Ass
137100120824     C                   ENDSL
137200120824     c                   end
137300120824      *
137400120824     C*  Se non è stato impostato il codice bolla lo imposto io
137500120824     C     VABCBO        IFEQ      *BLANKS
137600120824     C     VABCAS        IFGT      0
137700120824     C                   MOVEL     '4'           VABCBO                         + C/Ass
137800120824     C                   ELSE
137900120824     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
138000120824     C                   END
138100120824     C                   END
138200120824      *
138300120824      * Imposta le NOte
138400120824     C     VABNOT        IFEQ      *BLANKS
138500120824     C                   MOVEL     SNOTE1        VABNOT
138600120824     C                   ELSE
138700121207      **
138800121207      **  Attenzione se si tratta di un ORM non  eseguito lo deve SEGNALARE
138900121207     c     'LEERFAHRT'   scan      SNOTE1                                 11
139000121207     c                   if        *in11
139100121207      * ?-----> si TRATTA di un NOSTRO ORM che GEL NON HA FATTO
139200121207     C                   eval        VABNT2 = 'MANCATO RITIRO:'+ %Trim(SNOTE1)
139300121207     c                   else
139400121207      *
139500120824     C                   MOVEL     SNOTE1        VABNT2
139600121207     c                   end
139700120824     C                   END
139800121030      *
139900121030      * x Appuntamento : Pre-Advise
140000121030     c                   IF        %subst(SNOTE2:1:1) ='A' and VABTC1 = *blank
140100121030     C                   MOVEL     'A'           VABTC1
140200121030     c                   elseIF    %subst(SNOTE2:1:1) ='A' and VABTC2 = *blank
140300121030     C                   MOVEL     'A'           VABTC2
140400121030     C                   END
140500060612      *
140600120824      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
140700120824      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
140800120824      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
140900120824      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
141000120824      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
141100120824      *
141200120824     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
141300120824     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
141400120824     c                   else
141500120824      *
141600120824     C*  Se non è previsto che il cliente attribuisca nr.sped.
141700120824     C*  lo attribuisco io
141800120824      **              **------------------**
141900120824      * ?Numero spedizione
142000120824      *  Preleva dal numeratore il numero Bolla
142100120824     c                   exsr      repnum
142200120824      **              **------------------**
142300120824     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
142400120824      **              **------------------**
142500120824     c                   end
142600120824     C*
142700120824     C*  Se cap mittente originale a blank abblenco nazione
142800120824     C     VABCMO        IFEQ      *BLANKS
142900120824     C                   MOVEL     *BLANKS       VABNMO
143000120824     C                   END
143100120824     C*
143200100920      *
143300060612      * ?Scrivo EDiVAB o FiVAB
143400060612     C                   If        su_EDIVAB = 'S'
143500060612      ***
143600090629      *  Solo se ci sta aggiungo la data
143700120824     C                   eval       VABcmr = 'GEL:'+ SSNDDATE
143800090629      *
143900090629     c                   clear                   lungo_size        3 0
144000090629     c                   eval      lungo_size = %len(%Trim(VABCMR))
144100090629      *
144200090629     c                   if        lungo_size < 30
144300090629     c                   z-add     DATEU         DATA4             4 0
144400090629     c                   EVAL      VABCMR = %trim(VABCMR) + '_' +
144500090629     c                             %trim(%editw(DATA4: '0 /  '))
144600090629     c                   end
144700100315      *
144800060612     c                   z-add     DATEU         VABdcm
144900060612     c                   z-add     DATEU         VABdts
145000060612     c                   movel     WORA          VABhms
145100060612      ***
145200060612     c                   write     EDivab00
145300120824     c                   exsr      wrtvat_RifSPE
145400060612      *
145500060612     c                   ELSE
145600060612      ***
145700060612     c                   write     FiVAB000
145800120824     c                   exsr      wrtvat_RifSPE
145900060612      ***
146000060612     c                   ENDIF
146100060614      ***
146200060614     c                   move      'S'           Almeno_Uno
146300120824     C**
146400120824     C* Do errore se previsto CMR ma non c'è
146500120824     C     §PTCMR        IFNE      ' '
146600120824     C                   EXSR      MEMCMR
146700120824     C                   END
146800120824     C* Do errore se previsto AFB ma non c'è
146900120824     C     §PTNFV        IFNE      ' '
147000120824     C                   EXSR      MEMAFB
147100120824     C                   END
147200060612      *
147300060614     c     Error_VAB     Endsr
147400060612     C*----------------------------------------------------------------
147500060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
147600060612     C*----------------------------------------------------------------
147700120824     C     WRTVAT_rifspe BEGSR
147800120824      *
147900120824     c                   clear                   ediVAT00
148000120824     c                   clear                   fiVAT000
148100120824      * riferimento del Partner che una volta veniva riportato sul BL4
148200120824     C                   Z-ADD     VABCNT        VATCNT
148300120824     C                   MOVEL     VABCMR        VATCMR
148400120824     C                   Z-ADD     VABCCM        VATCCM
148500120824     C                   movel     VABfgs        VATfgs
148600120824     C                   Z-ADD     VABLNP        VATLNP
148700120824     C                   Z-ADD     VABAAS        VATAAS
148800120824     C                   Z-ADD     VABNRS        VATNRS
148900120824     C                   Z-ADD     VABNSP        VATNSP
149000120824     C                   MOVEL     'C'           VATTRC
149100120827     C                   MOVEL     allinea_shpNbrVATNOT
149200120824      *
149300120824      * ?Scrivo EDiVAB o FiVAB
149400120824     C                   If        su_EDIVAB = 'S'
149500120824     C                   WRITE     EDiVAT00
149600120824     c                   else
149700120824     C                   WRITE     fiVAT000
149800120824     c                   end
149900121031      *
150000121031      *  riferimento di consegna destinatario
150100121031     c                   if        %subst(SNOTE2:20:31) <> *blank
150200121031     C                   Z-ADD     VABCNT        VATCNT
150300121031     C                   MOVEL     VABCMR        VATCMR
150400121031     C                   Z-ADD     VABCCM        VATCCM
150500121031     C                   movel     VABfgs        VATfgs
150600121031     C                   Z-ADD     VABLNP        VATLNP
150700121031     C                   Z-ADD     VABAAS        VATAAS
150800121031     C                   Z-ADD     VABNRS        VATNRS
150900121031     C                   Z-ADD     VABNSP        VATNSP
151000121031     C                   MOVEL     'A'           VATTRC
151100121031     C                   eval       VATNOT = %subst(SNOTE2:20:31)
151200121031      *
151300121031     C                   If        su_EDIVAB = 'S'
151400121031     C                   MOVEL     VABCMR        VATCMR
151500121031     C                   Z-ADD     VABCNT        VATCNT
151600121031     C                   WRITE     EDiVAT00
151700121031     c                   else
151800121031     C                   WRITE     fiVAT000
151900121031     c                   end
152000121031     c                   endIF
152100121031      *
152200121031      *  riferimento di consegna telefonico
152300121031     c                   if        %subst(SNOTE2:2:18) <> *blank
152400121031     C                   Z-ADD     VABCNT        VATCNT
152500121031     C                   MOVEL     VABCMR        VATCMR
152600121031     C                   Z-ADD     VABCCM        VATCCM
152700121031     C                   movel     VABfgs        VATfgs
152800121031     C                   Z-ADD     VABLNP        VATLNP
152900121031     C                   Z-ADD     VABAAS        VATAAS
153000121031     C                   Z-ADD     VABNRS        VATNRS
153100121031     C                   Z-ADD     VABNSP        VATNSP
153200121031     C                   MOVEL     'B'           VATTRC
153300121031     c                   eval      VATNOT = %subst(SNOTE2:2:18)
153400121031      *
153500121031     C                   If        su_EDIVAB = 'S'
153600121031     C                   MOVEL     VABCMR        VATCMR
153700121031     C                   Z-ADD     VABCNT        VATCNT
153800121031     C                   WRITE     EDiVAT00
153900121031     c                   else
154000121031     C                   WRITE     fiVAT000
154100121031     c                   end
154200121031     c                   endIF
154300120824      *
154400120824     c     Error_RIF     Endsr
154500120824     C*----------------------------------------------------------------
154600120824     C*? ESEGUO SCRITTURA dei singoli COLLI  della Spedizione
154700120824     C*----------------------------------------------------------------
154800120824     C     Scrive_VAT    BEGSR
154900120824      *
155000120824      * ? Segnacollo -->
155100120824     C                   movel     VABfgs        VATFGS
155200041215     C                   Z-ADD     VABCCM        VATCCM
155300041215     C                   Z-ADD     VABAAS        VATAAS
155400041215     C                   Z-ADD     VABLNP        VATLNP
155500041215     C                   Z-ADD     VABNRS        VATNRS
155600041215     C                   Z-ADD     VABNSP        VATNSP
155700060413      **
155800060608     C                   MOVEL     'E'           VATTRC
155900120824     C                   MOVEL     TBARCODE      VATNOT
156000041215      **
156100051219     C                   If        su_EDIVAB = 'S'
156200041215     C                   MOVEL     VABCMR        VATCMR
156300041215     C                   Z-ADD     VABCNT        VATCNT
156400041215     C                   WRITE     EDiVAT00
156500041215     c                   else
156600041215     C                   WRITE     fiVAT000
156700041215     c                   end
156800060413      *
156900041215     C                   ENDSR
157000971215      *----------------------------------------------------------------
157100971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
157200971215      *----------------------------------------------------------------
157300120824     C     RepNum        BEGSR
157400050112      *
157500050112      *    deve controllare sulla tabella "3C" se il numero spedizione
157600050112      *     deve essere mantenuto oppure no
157700050112     C                   clear                   Ds3C
157800060620     C                   Z-ADD     1             TBLKUT
157900050112     C                   MOVEL     '3C'          TBLCOD
158000050112     C                   movel(p)  VABccm        TBLKEY
158100050112     C     KTABel        CHAIN     TABEL00F                           30
158200050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
158300050112     C                   MOVEL     TBLUNI        Ds3C
158400050112     C                   END
158500050112      *
158600050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
158700050112     c                   if        §3CFsp <> 'S'
158800050112      *
158900050112     C* NSP => Stacco un numeratore da AZNUM
159000050112     c                   movel     kpjbu         svkpjbu
159100050112     c                   clear                   kpjbu
159200050112     C                   clear                   TRUL33DS
159300050112     C                   eval      I33OPE = *zeros
159400050112     C                   eval      I33CNU = 302
159500050112     C                   eval      I33NUM = 1
159600050112     C                   movel     TRUL33DS      KPJBU
159700050112     C                   call      'TRUL33R'
159800050112     C                   parm                    KPJBA
159900050112     C                   movel     KPJBU         TRUL33DS
160000050112     C                   if        O33ERR = *zeros
160100050112     c                   z-add     o33nrf        NUM_SPED
160200050112     C                   endif
160300050112     c                   movel     svkpjbu       kpjbu
160400050112      ****
160500050112     c                   ELSE
160600050112      *   se si vuole mantenere il numero spedizione
160700050112      ****
160800050112     C                   MOVE      UYEAR         KAAA
160900050112     C                   Z-ADD     3             KCNU
161000050112     C                   Z-ADD     VABLNP        KFIL
161100050112     C     KNUF          CHAIN     FLNUF                              9999
161200050112     C                   IF        *IN99 = *OFF
161300050112     C                   ADD       1             NUFNUM
161400050112     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE
161500050112     C                   UPDATE    FLNUF
161600060612     c                   END
161700060411      *
161800050112     c                   EndIF
161900050112      **
162000050112      **              **------------------**
162100050112     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
162200050112      **              **------------------**
162300971215      *
162400050112     C                   MOVE      UYEAR         KAAA
162500991206     c                   IF        KAAA > 90
162600991206     C     1900          add       kaaa          VABAAS
162700991206     C                   ELSE                                                   data sped.
162800991206     C     2000          add       kaaa          VABAAS
162900991206     c                   ENDIF                                                  data sped.
163000971216      *
163100991206     C                   ENDSR
163200051205      *----------------------------------------------------*
163300051205      *   DEFINIZIONE CHIAVI                               *
163400051205      *----------------------------------------------------*
163500051205     C     *INZSR        BEGSR
163600051205      *
163700991129     c     *ENTRY        PLIST
163800060613     C                   parm                    esito             1
163900110930     C                   eval      esito ='0'
164000120824      *
164100120824     C     KRDE          KLIST
164200120824     C                   KFLD                    KAAS
164300120824     C                   KFLD                    KLNP1
164400120824     C                   KFLD                    KNRS
164500120824     C                   KFLD                    KNSP
164600120824     C                   KFLD                    KNMC
164700120824     C                   KFLD                    KNRP
164800120824      *
164900120824     C     KACO          KLIST
165000120824     C                   KFLD                    KKUT
165100120824     C                   KFLD                    KKCC
165200120824     C                   KFLD                    KKSC
165300120824      *
165400120824     C     KIND          KLIST
165500120824     C                   KFLD                    KKUT
165600120824     C                   KFLD                    KKCC
165700120824     C                   KFLD                    KKSC
165800120824     C*
165900120824     C     KTAB1         KLIST
166000120824     C                   KFLD                    KKUT
166100120824     C                   KFLD                    KCOD
166200120824     C*
166300120824      *
166400991129      *
166500991206     C     KNUF          KLIST
166600991206     C                   KFLD                    KAAA
166700991206     C                   KFLD                    KCNU
166800991206     C                   KFLD                    KFIL
166900971215      *
167000050112     C     KTABel        KLIST
167100050112     C                   KFLD                    TBLKUT
167200050112     C                   KFLD                    TBLCOD
167300050112     C                   KFLD                    TBLKEY
167400100920      *
167500110930     C     Kvat4E        KLIST
167600110930     C                   KFLD                    vatTRC
167700110930     C                   KFLD                    vatNOT
167800000908      *
167900971216      * Recupero data e ora
168000971216     C                   TIME                    WORA
168100991124     C                   TIME                    W0140
168200120824      * UDATE IN
168300120824     C                   MOVE      W0140         WDTGIO                                 GGMMAAAA
168400120824     C     *eur          MOVEL     WDTGIO        DATA_eur                               AAAAMMGG
168500991124     C     *iso          MOVEL     DATA_eur      dateu
168600120824     C* RECUPERO DATA E ORA
168700120824     C                   TIME                    WHHDAT           14 0
168800120824     C                   MOVE      WHHDAT        G02DAT
168900120824     C                   MOVE      WHHDAT        WDATA8            8 0
169000120824     C                   MOVE      WDATA8        WAA2              2 0
169100120824     C                   MOVEL     WDATA8        DATA              6 0
169200120824     C                   MOVE      WAA2          DATA
169300120824     C                   MOVEL     *BLANK        G02ERR
169400120824     C                   CALL      'XSRDA8'
169500120824     C                   PARM                    WLBDA8
169600120824     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
169700120824     C                   TIME                    WORA              6 0
169800120824     C                   TIME                    W0140                            data e ora
169900120824      *
170000120824     C                   CLEAR                   TIBS55DS
170100120824     C                   MOVEL     'L'           I50TLA
170200120824     C                   CALL      'TIBS55R'
170300120824     C                   PARM                    TIBS55DS
170400120824      *
170500120824     C* RECUPERO DATI DELL'UTENTE
170600120824     C                   Z-ADD     1             CODUT             1 0
170700120824     C                   CALL      'XPARUT'
170800120824     C                   PARM                    UTEDSE0F
170900120824     C                   MOVEL     RAGUT         RSUT             20
171000120824     C*
171100120824     C* RICERCA CAPOCONTI
171200120824     C                   DO        50            X
171300120824     C                   MOVE      TCU(X)        TCUDS
171400120824     C     F56           IFEQ      'CG'
171500120824     C     F34           ANDEQ     '01'
171600120824     C                   Z-ADD     KCU(X)        KCI               4 0
171700120824     C                   END
171800120824     C                   END
171900120824     C*
172000120824     c                   movel(p)  'EDPAB'       User_Msg         10
172100120824     C*
172200120824     C* IMPOSTO A ZERO VARIABILI DI PGM
172300120824     C                   MOVEL     CA(1)         WCA              78
172400120824     C                   MOVEL     CN(1)         WCN              78
172500120824      *
172600120824      * ?              /*--------------------------- */
172700120824     c                   exsr      CARICA_TABELLE
172800120824      * ?              /*--------------------------- */
172900120824      *
173000120824      *------------------
173100120824      *  INIZIALIZZA  dati fondamentali messaggio
173200120824     C                   clear                   EDIDSPT
173300120824     C                   clear                   EDIDSPU
173400120824     C                   clear                   EDIDSPV
173500120824     C                   move      'N'           PT_ok             1
173600120824      *
173700120824      ** ---------------------------------------------------------------
173800120824      * quindi 1° cosa:
173900120824      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
174000120824      *     senza questo il messaggio NON è trascodificabile
174100120824     C                   MOVEL     UNB_Chiodato  WCOD             35
174200120824     C                   MOVEl(p)  UNB_Chiodato  UNB_Mittente     35
174300120824     C                   Z-ADD     1             XX                3 0
174400120824     C     WCOD          LOOKUP    CPT(XX)                                32
174500120824      *
174600120824      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
174700120824     C                   If        *IN32 = *off
174800120824      * ******  Errore generale sul Messaggio
174900120824      * Msg di allerta Traduzione
175000121219     C                   EVAL      Indirizzi = CEDALERT_Brt
175100120824     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import GEL_DE'
175200120824     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
175300120824     C                             Bolle import in filiale - messaggio -
175400120824     C                             con UNB GEL_DE non presente in tab.PT contro-
175500120824     C                             llare EDI ricevuto su UPLOAD.'
175600120824     c                   exsr      invio_mail
175700120824      *
175800120824     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
175900120824     C                   eval      vinFLG = '2'
176000120824     C                   eval      se_errore   = 'S'
176100120824     c                   goto      end_inzsr
176200120824      *
176300120824     C                   move      'N'           PT_ok             1
176400120824      *             *------------------------------*
176500120824     C                   eLSe
176600120824      *             *------------------------------*
176700120824      * Se tutto OK su tab.PT:
176800120824     C                   move      'S'           PT_ok             1
176900120824      *
177000120824     C                   MOVEL     DPT(XX)       EDIDSPT
177100120824     C                   MOVEL     DPU(XX)       EDIDSPU
177200120824     C                   MOVEL     DPV(XX)       EDIDSPV
177300120824      *
177400120824      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
177500120824      *  ricevuti:
177600120824     c                   movel     §PVema        mail_ind         44
177700120824      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
177800120824     c                   if        mail_ind <> *blank
177900120824     c                   exsr      imp_ADDR_mail
178000120824     c                   end
178100120824      *
178200120824      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
178300120824      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
178400120824      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
178500120824     C                   if        §PULUN = *blank
178600120824     C                   eval      §PULUN = '00'
178700120824     c                   end
178800120824      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
178900120824     C                   move      §puLUN        WVATlun           2 0          *0/>0
179000120824      *
179100120824      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
179200120824      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
179300120824      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
179400120824      *  i records trasmessi).
179500120824      * Controllo se deve mantenere il numero bolla passato dal messaggio
179600120824      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
179700120824     c                   clear                   mantiene_nsp      1            *blk/'S'
179800120824     c                   eval      mantiene_nsp = §puNSP                        *blk/'S'
179900120824      **
180000120824     C*   scrive EDIVAB o FIVAB
180100120824     c                   clear                   su_EDIVAB         1
180200120824     C     §PUWRK        IFne      *blank
180300120824     c                   movel     'S'           su_EDIVAB
180400120824     c                   end
180500120824      *
180600120824      *  In base al cod.trattamento merce reperisco tipo tratt.
180700120824     C                   CLEAR                   DS1B
180800120824     C                   Z-ADD     1             Y                 3 0
180900120824     C     §PTctm        LOOKUP    CTM(Y)                                 32
181000120824     C     *IN32         IFEQ      '1'
181100120824     C                   MOVEL     DTM(Y)        DS1B
181200120824     C                   MOVEL     DS1B          WSVTRM           38
181300120824     C                   END
181400120824      *
181500120824      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
181600120824     C                   clear                   ACORAG
181700120824     C                   clear                   INDCAE
181800120824     C                   clear                   INDCAP
181900120824     C                   clear                   INDSTA
182000120824      * Decodifico partner
182100120824     C                   Z-ADD     1             KKUT
182200120824     C                   Z-ADD     KCI           KKCC
182300120824     C                   MOVE      §PTKSC        KKSC
182400120824     C     KACO          CHAIN     CNACO00F                           31
182500120824     C     KIND          CHAIN     CNIND00F                           31
182600120824      *
182700120824      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
182800120824      *   questa schiera è utilizzata anche per i clienti.
182900120824     C     §ptKSC        LOOKUP    KCL                                    39
183000120824     c                   If        *in39 = *off
183100120824     C                   add       1             WW                3 0
183200120824     C                   z-add     §ptKSC        KCL(WW)
183300120824     C                   Endif
183400120824      *
183500120824     c                   Endif
183600120824      **
183700120824     C     end_inzsr     ENDSR
183800120824      * ?------------------------------------------------------------------ */
183900120824      *?    CARICA TABELLE SU SCHIERE
184000120824      * ?------------------------------------------------------------------ */
184100120824     C     CARICA_TABELLEBEGSR
184200120824      *
184300120824     C* Caricamento Tabella 1B
184400120824     C                   Z-ADD     0             X                 4 0
184500120824     C                   Z-ADD     CODUT         KKUT
184600120824     C                   MOVEL     '1B'          KCOD
184700120824     C     KTAB1         CHAIN     TABEL00F                           30
184800120824     C     *IN30         DOWEQ     '0'
184900120824     C     TBLFLG        IFEQ      *BLANKS
185000120824     C                   ADD       1             X
185100120824     C                   MOVEL     TBLKEY        CTM(X)
185200120824     C                   MOVEL     TBLUNI        DTM(X)
185300120824     C                   END
185400120824     C     KTAB1         READE     TABEL00F                               30
185500120824     C                   END
185600120824      *
185700120824     C* Caricamento Tabella 15
185800120824     C                   Z-ADD     0             X                 4 0
185900120824     C                   Z-ADD     CODUT         KKUT
186000120824     C                   MOVEL     '15'          KCOD
186100120824     C     KTAB1         CHAIN     TABEL00F                           30
186200120824     C     *IN30         DOWEQ     '0'
186300120824     C     TBLFLG        IFEQ      *BLANKS
186400120824     C                   ADD       1             X
186500120824     C                   MOVEL     TBLKEY        C15(X)
186600120824     C                   MOVEL     TBLUNI        DS15
186700120824     C                   MOVEL     §15COD        ISO(X)
186800120824     C                   END
186900120824     C     KTAB1         READE     TABEL00F                               30
187000120824     C                   END
187100120824      *
187200120824      * Caricamento Tabella termini consegna
187300120824     C                   Z-ADD     0             X
187400120824     C                   MOVEL     'TB'          TABCOD
187500120824     C     TABCOD        CHAIN     EDTAB01L                           30
187600120824     C     *IN30         DOWEQ     '0'
187700120824     C     TABFLG        IFEQ      *BLANKS
187800120824     C                   ADD       1             X
187900120824     C                   eval      K35_TpBolla(X) = TABKEY
188000120824     C                   eval      Cod_TpBolla(X) = TABKEY
188100120824     C                   eval      Decod_Porto(X) = TABUNI
188200120824     C                   END
188300120824     C     TABCOD        READE     EDTAB01L                               30
188400120824     C                   END
188500120824      *
188600120824     C* Caricamento Tabella Tipo Incasso C/Assegno
188700120824     C                   Z-ADD     0             X
188800120824     C                   MOVEL     'TC'          TABCOD
188900120824     C     TABCOD        CHAIN     EDTAB01L                           30
189000120824     C     *IN30         DOWEQ     '0'
189100120824     C     TABFLG        IFEQ      *BLANKS
189200120824     C                   ADD       1             X
189300120824     C                   MOVEL     TABKEY        CTC35(X)
189400120824     C                   MOVEL     TABUNI        TPI(X)
189500120824     C                   END
189600120824     C     TABCOD        READE     EDTAB01L                               30
189700120824     C                   END
189800120824      *
189900120824      * Caricamento Tabella Partner esteri
190000120824     C                   Z-ADD     0             X
190100120824     C                   MOVEL     'PT'          TABCOD
190200120824     C     TABCOD        CHAIN     EDTAB01L                           30
190300120824     C     *IN30         DOWEQ     '0'
190400120824      *
190500120824     C                   SELECT
190600120824     C     TABFLG        WHENNE    *BLANKS
190700120824      *
190800120824     C                   OTHER
190900120824     C                   ADD       1             X
191000120824     C                   MOVEL     TABKEY        CPT(X)
191100120824     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
191200120824     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
191300120824     C                   MOVEL     TABUNI        DPT(X)
191400120824     C                   ENDSL
191500120824      *
191600120824     C     TABCOD        READE     EDTAB01L                               30
191700120824     C                   END
191800121105      *
191900121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
192000121105     c                   exsr      ChecK_PT
192100121105      *
192200120824      * salva quanti Codici UNB ha caricato
192300120824     c                   z-add     x             sav_CPTx
192400120824      *
192500120824     C                   MOVEL     'PU'          TABCOD
192600120824     C     TABCOD        CHAIN     EDTAB01L                           30
192700120824     C     *IN30         DOWEQ     '0'
192800120824      *
192900120824     C                   SELECT
193000120824     C     TABFLG        WHENNE    *BLANKS
193100120824      *
193200120824     C                   OTHER
193300120824     C                   MOVEL     TABKEY        CODPU            35
193400120824     C                   Z-ADD     1             X
193500120824     C     CODPU         LOOKUP    CPT(X)                                 32
193600120824     C   32              MOVEL     TABUNI        DPU(X)
193700120824     C                   ENDSL
193800120824      *
193900120824     C     TABCOD        READE     EDTAB01L                               30
194000120824     C                   END
194100120824      *
194200120824      * Prosegue tab.PT e PU
194300120824     C                   MOVEL     'PV'          TABCOD
194400120824     C     TABCOD        CHAIN     EDTAB01L                           30
194500120824     C     *IN30         DOWEQ     '0'
194600120824      *
194700120824     C                   SELECT
194800120824     C     TABFLG        WHENNE    *BLANKS
194900120824      *
195000120824     C                   OTHER
195100120824     C                   MOVEL     TABKEY        CODPV            35
195200120824     C                   Z-ADD     1             X
195300120824     C     CODPV         LOOKUP    CPT(X)                                 32
195400120824     C   32              MOVEL     TABUNI        DPV(X)
195500120824     C                   ENDSL
195600120824      *
195700120824     C     TABCOD        READE     EDTAB01L                               30
195800120824     C                   END
195900120824      *
196000120824     C* Caricamento Tabella codici clienti - tariffe
196100120824     C                   Z-ADD     0             X
196200120824     C                   MOVEL     'CL'          TABCOD
196300120824     C     TABCOD        CHAIN     EDTAB01L                           30
196400120824     C     *IN30         DOWEQ     '0'
196500120824     C     TABFLG        IFEQ      *BLANKS
196600120824     C                   ADD       1             X
196700120824     C                   MOVEL     TABKEY        CCL(X)
196800120824     C                   MOVEL     TABUNI        DCL(X)
196900120824     C                   END
197000120824     C     TABCOD        READE     EDTAB01L                               30
197100120824     C                   END
197200120824      *
197300120824     C* Caricamento Serivizi speciali
197400120824     C                   Z-ADD     0             X
197500120824     C                   MOVEL     'SS'          TABCOD
197600120824     C     TABCOD        CHAIN     EDTAB01L                           30
197700120824     C     *IN30         DOWEQ     '0'
197800120824     C     TABFLG        IFEQ      *BLANKS
197900120824     C                   ADD       1             X
198000120824     C                   MOVEL     TABKEY        SS(X)
198100120824     C                   MOVEL     TABKEY        SS35(X)
198200120824     C                   MOVEL     TABUNI        DSS(X)
198300120824     C                   END
198400120824     C     TABCOD        READE     EDTAB01L                               30
198500120824     C                   END
198600120824      *
198700120824     C                   ENDSR
198800060621      * ?------------------------------------------------------------------ */
198900060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
199000060621      * ?------------------------------------------------------------------ */
199100060621     C     *pssr         BEGSR
199200060621     C
199300060710     C                   eval      esito = '2'
199400120504     C*
199500121219     C                   eval      Messaggio='Il File ricevuto da GEL '+
199600121219     C                             'ha avuto problemi durante la traduzione. '+
199700121220     C                             ' Controllare UPLOAD. Potrebbero esserci' +
199800121220     C                             ' dei records con i campi suddivisi male.'+
199900121220     C                             ' Potrebbero mancare dei (;) separatori ' +
200000121220     C                             'sfasando i dati di una spedizione x man' +
200100121220     C                             'dare in crisi il traduttore!!!'
200200090605      **    avviso tramite MAIL
200300121210     c                   if        §PVinv <> 'N'
200400121219     C                   exSR      email_alert
200500121210     c                   end
200600121107      **
200700121107      **  ripristina la riga in errore
200800121107     c                   rolbk
200900090605      **
201000060621     C                   ENDSR     '*CANCL'
201100120824     C*----------------------------------------------------------------
201200120824     C*? MEMCMR - MEMORIZZO NUMERO CMR
201300120824     C*----------------------------------------------------------------
201400120824     C     MEMCMR        BEGSR
201500120824     C*
201600120824     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
201700120824     C     §PTCM1        IFEQ      'S'
201800120824     C                   CLEAR                   EDRDE000
201900120824     C                   MOVEL     'TD1154'      KNMC
202000120824     C                   Z-ADD     1             KNRP
202100120824     C                   MOVEL     §PTksc        KLNP1
202200120824     C     KRDE          CHAIN     EDRDE01L                           31
202300120824     C     *IN31         IFEQ      '1'
202400120824     C                   Z-ADD     VABAAS        RDEAAS
202500120824     C                   Z-ADD     VABLNP        RDELNP
202600120824     C                   Z-ADD     VABNRS        RDENRS
202700120824     C                   Z-ADD     VABNSP        RDENSP
202800120824     C                   MOVEL     'TD1154'      RDENMC
202900120824     C                   Z-ADD     1             RDENRP
203000120824     C                   MOVEL     vabcmr        RDEVAL
203100120824     C                   WRITE     EDRDE000                             99
203200120824     C                   END
203300120824     C*  Memorizzo qualificatore CMR
203400120824     C                   CLEAR                   EDRDE000
203500120824     C                   MOVEL     'TD1153'      KNMC
203600120824     C                   Z-ADD     1             KNRP
203700120824     C                   MOVEL     §PTksc        KLNP1
203800120824     C     KRDE          CHAIN     EDRDE01L                           31
203900120824     C     *IN31         IFEQ      '1'
204000120824     C                   Z-ADD     VABAAS        RDEAAS
204100120824     C                   Z-ADD     VABLNP        RDELNP
204200120824     C                   Z-ADD     VABNRS        RDENRS
204300120824     C                   Z-ADD     VABNSP        RDENSP
204400120824     C                   Z-ADD     1             RDENRP
204500120824     C                   MOVEL     'TD1153'      RDENMC
204600120824     C                   MOVEL     'CMR'         RDEVAL
204700120824     C                   WRITE     EDRDE000                             99
204800120824     C                   END
204900120824     C*  Memorizzo la data
205000120824     C                   CLEAR                   EDRDE000
205100120824     C                   MOVEL     'TD2380'      KNMC
205200120824     C                   Z-ADD     1             KNRP
205300120824     C                   MOVEL     §PTksc        KLNP1
205400120824     C     KRDE          CHAIN     EDRDE01L                           31
205500120824     C     *IN31         IFEQ      '1'
205600120824     C                   Z-ADD     VABAAS        RDEAAS
205700120824     C                   Z-ADD     VABLNP        RDELNP
205800120824     C                   Z-ADD     VABNRS        RDENRS
205900120824     C                   Z-ADD     VABNSP        RDENSP
206000120824     C                   Z-ADD     1             RDENRP
206100120824     C                   MOVEL     'TD2380'      RDENMC
206200120824     C                   MOVEL     VABDCM        RDEVAL
206300120824     C                   WRITE     EDRDE000                             99
206400120824     C                   END
206500120824     C*
206600120824     C                   END                                                    §PTCM1 = 'S'
206700120824     C*
206800120824     C                   ENDSR
206900120824     C*----------------------------------------------------------------
207000120824     C*? MEMAFB - MEMORIZZO NUMERO AFB
207100120824     C*----------------------------------------------------------------
207200120824     C     MEMAFB        BEGSR
207300120824     C*
207400120824     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
207500120824     C     §PTNF1        IFEQ      'S'
207600120824     C                   CLEAR                   EDRDE000
207700120824     C                   MOVEL     §PTksc        KLNP1
207800120824     C                   MOVEL     'TD1154'      KNMC
207900120824     C                   Z-ADD     1             KNRP
208000120824     C     KRDE          CHAIN     EDRDE01L                           31
208100120824     C     *IN31         IFEQ      '1'
208200120824     C                   Z-ADD     VABAAS        RDEAAS
208300120824     C                   Z-ADD     VABLNP        RDELNP
208400120824     C                   Z-ADD     VABNRS        RDENRS
208500120824     C                   Z-ADD     VABNSP        RDENSP
208600120824     C                   MOVEL     'TD1154'      RDENMC
208700120824     C     §PTCM1        IFNE      'S'
208800120824     C     vabcmr        OREQ      *BLANKS
208900120824     C                   Z-ADD     1             RDENRP
209000120824     C                   ELSE
209100120824     C                   Z-ADD     2             RDENRP
209200120824     C                   END
209300120824     C                   MOVEL     VABCMR        RDEVAL
209400120824     C                   WRITE     EDRDE000                             99
209500120824     C                   END
209600120824     C*  Memorizzo qualificatore CMR
209700120824     C                   CLEAR                   EDRDE000
209800120824     C                   MOVEL     §PTksc        KLNP1
209900120824     C                   MOVEL     'TD1153'      KNMC
210000120824     C                   Z-ADD     1             KNRP
210100120824     C     KRDE          CHAIN     EDRDE01L                           31
210200120824     C     *IN31         IFEQ      '1'
210300120824     C                   Z-ADD     VABAAS        RDEAAS
210400120824     C                   Z-ADD     VABLNP        RDELNP
210500120824     C                   Z-ADD     VABNRS        RDENRS
210600120824     C                   Z-ADD     VABNSP        RDENSP
210700120824     C     §PTCM1        IFNE      'S'
210800120824     C     vabcmr        OREQ      *BLANKS
210900120824     C                   Z-ADD     1             RDENRP
211000120824     C                   ELSE
211100120824     C                   Z-ADD     2             RDENRP
211200120824     C                   END
211300120824     C                   MOVEL     'TD1153'      RDENMC
211400120824     C                   MOVEL     'AFB'         RDEVAL
211500120824     C                   WRITE     EDRDE000                             99
211600120824     C                   END
211700120824     C*  Memorizzo la data
211800120824     C                   CLEAR                   EDRDE000
211900120824     C                   MOVEL     §PTksc        KLNP1
212000120824     C                   MOVEL     'TD2380'      KNMC
212100120824     C                   Z-ADD     1             KNRP
212200120824     C     KRDE          CHAIN     EDRDE01L                           31
212300120824     C     *IN31         IFEQ      '1'
212400120824     C                   Z-ADD     VABAAS        RDEAAS
212500120824     C                   Z-ADD     VABLNP        RDELNP
212600120824     C                   Z-ADD     VABNRS        RDENRS
212700120824     C                   Z-ADD     VABNSP        RDENSP
212800120824     C     §PTCM1        IFNE      'S'
212900120824     C     vabcmr        OREQ      *BLANKS
213000120824     C                   Z-ADD     1             RDENRP
213100120824     C                   ELSE
213200120824     C                   Z-ADD     2             RDENRP
213300120824     C                   END
213400120824     C                   MOVEL     'TD2380'      RDENMC
213500120824     C                   MOVEL     VABDCM        RDEVAL
213600120824     C                   WRITE     EDRDE000                             99
213700120824     C                   END
213800120824     C*
213900120824     C                   END                                                    §PTCM1 = 'S'
214000120824     C*
214100120824     C                   ENDSR
214200090605      * ?------------------------------------------------------------------ */
214300090605      *?  Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
214400090605      * ?------------------------------------------------------------------ */
214500090605     C     eMail_alert   BEGSR
214600090605     C*
214700121219     C                   eval      Oggetto ='UPLOAD file da GEL con ERRORI -
214800090605     C                             in Stato 2.'
214900121219     C*
215000121219     C                   eval      Indirizzi = 'Andrea.Bertocchi@brt.it'
215100121219     c                   exsr      invio_MAIL
215200090605      *
215300121219     C                   eval      Indirizzi = CEDALERT_Brt
215400121219     c                   exsr      invio_MAIL
215500090605      *
215600090605     C                   ENDSR
215700120824     c*==================================================================*
215800120824      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
215900120824     c*==================================================================*
216000120824     c     Imp_ADDR_mail begsr
216100120824      *
216200120824     c                   clear                   Indirizzi
216300120824      *  Lunghezza indirizzi presenti
216400120824     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
216500120824     c                   z-add     1             al_char           3 0
216600120824     c                   z-add     1             dal_char          3 0
216700120824     c                   z-add     1             tot_char          3 0
216800120824      *
216900120824     c     successivo    tag
217000120824      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
217100120824      *   e aggiunge il dominio Bartolini + il (;) separatore
217200120824     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
217300120824     c                   eval      tot_char = al_char - dal_char
217400120824     c                   z-add     dal_char      Dac
217500120824     c                   z-add     tot_char      Luc
217600120824      *
217700120824     c                   clear                   Indir_Bartol     40
217800120824     c                   clear                   Indir_Parz       20
217900120824     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
218000120824     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
218100120824     c                             %Trim(bart_it)
218200120824     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
218300120824      *
218400120824     c                   if        al_char = Lun_Ind
218500120824     c                   goto      fine_indmail
218600120824     c                   else
218700120824     c                   eval      dal_char = ( al_char + 1 )
218800120824     c                   goto      successivo
218900120824     c                   end
219000120824      *
219100120824     C     fine_indmail  TAG
219200120824      *
219300120824     C                   ENDSR
219400120824     c*==================================================================*
219500120824      * Manda un Msg x E-mail
219600120824     c*==================================================================*
219700120824     c     Invio_Mail    begsr
219800150408      *
219900150408      *** Basta messaggi inutili
220000150408      ***  Tanto GEL NON manda mai i dati correttamente... quindi quello che
220100150408      ***  si può tradurre lo traduciamo!!!!!
220200150408     c                   goto      NO_MAIL_PIU
220300120824      *
220400121219     C*   Alert_mail : invio Mail a CedAlert@brt.it             *
220500120824     C* Inizializzo variabili
220600120824     C                   movel     *blanks       wrkEml          253
220700120824     C                   movel     *blanks       wrkMsg         5000
220800120824     C                   movel     *blanks       wrkOgg           44
220900120824      *
221000120824     C* Valorizzo i campi della e-m@ail - indirizzo
221100120824     C                   eval      wrkEml = Indirizzi
221200120824     C                   eval      wrkOgg = Oggetto
221300120824     C                   eval      wrkMsg = Messaggio
221400120824     C*
221500130204     C*   se *PSSR deve informare anche CED
221600130204     c                   if        esito = '2'
221700120824     C                   call(e)   'TRTCT00R2'
221800120824     C                   parm                    wrkEml
221900120824     C                   parm                    wrkOgg
222000120824     C                   parm                    wrkMsg
222100130204     c                   else
222200130204     C*  Altrimenti NON mandare a CED
222300130204     C                   movel     *blank        wrkesito
222400130204     C                   movel     *blank        wrkEmlcc
222500130204     C                   call(e)   'TRTCT00R5'
222600130204     C                   parm                    wrkEml
222700130204     C                   parm                    wrkEmlcc        253
222800130204     C                   parm                    wrkOgg
222900130204     C                   parm                    wrkMsg
223000130204     C                   parm                    wrkesito          1
223100130204     c                   end
223200120824     C*
223300150408     c     NO_MAIL_PIU   tag
223400120824     C                   ENDSR
223500121105      *---------------------------------------------------------------*
223600121105      * CTRL caricamento schiere    PT                               -*
223700121105      *---------------------------------------------------------------*
223800121105     C     Check_PT      begsr
223900121105     C*
224000121105     c* Controllo riempimento schiera
224100121105     c                   clear                   trul0sds
224200121105     c                   eval      i0sski='CPT'
224300121105     c                   eval      i0sele=%elem(CPT)
224400121105     c                   eval      i0spie=x
224500121105     c                   eval      i0sfile='EDTAB00F'
224600121105     c                   eval      i0ssif=knsif
224700121105     c                   eval      i0spgm='TRTCT15R'
224800121113     c                   move      kpjbu         SvKpjbu
224900121113     c                   clear                   kpjbu
225000121105     c                   movel     trul0sds      kpjbu
225100121105     c                   call      'TRUL0SR'
225200121105     c                   parm                    kpjba
225300121113     c                   eval      kpjbu  =  SvKpjbu
225400121105     C*
225500121105     C                   ENDSR
225600120824     O*****************************************************************
225700120824** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
225800120824ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
225900120824** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
226000120824987654321098765432109876540123456789987654321098765432109876540999999999999999 1
226100120824** Nomi campi record S
226200120824STYPEREC    (TIPO REC -->(S))
226300120824SSNDDEPOT   (DEPOT MITTENTE)
226400120824STIMEARR    (DATA ARRIVO AL DEPOT)
226500120824SSNDCODE    (CODICE CLIENTE MITTENTE)
226600120824SSNDNAME    (RAGSOC MITTENTE)
226700120824SSNDNAMEAD  (RAGSOC MITTENTE ADD)
226800120824SSNDADDR    (INDIRIZZO MITTENTE)
226900120824SSNDCOUNTR  (NAZIONE MITTENTE)
227000120824SSNDZIPCOD  (CAP MITTENTE)
227100120824SSNDCITY    (LOCALITÀ MITTENTE)
227200120824SSNDREFER   (RIFERIMENTO CLIENTE)
227300120824SSNDDATE    (DATA MANIFEST)
227400120824SRCPNAME    (RAGSOC DESTINATARIO)
227500120824SRCPNAMEAD  (RAGSOC DESTINATARIO ADD)
227600120824SRCPADDR    (INDIRIZZO DESTINATARIO)
227700120824SRCPCOUNTR  (NAZIONE DESTINATARIO)
227800120824SRCPZIPCOD  (CAP DESTINATARIO)
227900120824SRCPCITY    (LOCALITÀ DESTINATARIO)
228000120824SNRPARCELS  (NR.COLLI SPEDIZIONE)
228100120824SXXLDIMENS  (SEMPRE (N))
228200120824SXXLPARCEL  (VUOTO)
228300120824SREALWEIGH  (PESO SENZA VIRG.CON 1 DEC)
228400120824SVOLFROMWG  (VOLUME SENZA VIRG.CON 1 DEC)
228500120824SSERVICE    (SEMPRE -> O OVERNIGHT)
228600120824SAUTOMPOD   (SEMPRE -> N ?????)
228700120824SSPECIALAD  (VUOTO)
228800120824STIMELATER  (VUOTO)
228900120824SPORTOF     (SEMPRE -> Y PORTO FRANCO)
229000120824SCODAMOUNT  (VALORE CONTRASSEGNO)
229100120824SCHEQUE     (VUOTO)
229200120824SINSURANCE  (VALORE ASSICURAZIONE)
229300120824SNOTE1      (NOTE 1)
229400120824SNOTE2      (NOTE 2)
229500120824SSHPNUMBER  (RIFERIMENTO SPEDIZIONE)
229600120824SRCPDEPOT   (DEPOT DESTINATARIO)
229700120924SORDEPOT    (DEPOT ORDINE DI RITIRO - commissionante)
229800120924SPICKUPREF  (ORDINE DI RITIRO)
229900120824** Nomi campi record T
230000120824TTYPEREC    (TIPO REC -->(T))
230100120824TSNDDEPOT   (DEPOT MITTENTE)
230200120824TBARCODE    (BARCODE)
230300120824TSHPNUMBER  (RIFERIMENTO SPEDIZIONE)
230400120824TEMPTY1     (VUOTO)
230500120824TEMPTY2     (VUOTO)
230600120824TEMPTY3     (DATA VUOTO)
230700120824TEMPTY4     (TIME VUOTO)
230800120824TEMPTY5     (VUOTO)
230900120824TPACKAGENR  (NR.PROGRESSIVO COLLO)
231000120824TRECDEPOT   (DEPOT DESTINATARIO)
231100120824TLENGHTCM   (LENGTH IN CM DECIMAL 0)
231200120824TWIDTHCM    (WIDTH IN CM DECIMAL 0)
231300120824THEIGHTCM   (HEIGHT IN CM DECIMAL 0)
231400120824TREALWEIGH  (PESO REALE DEL COLLO)
231500120824TVOLFROMWG  (VOLUME DEL COLLO)
